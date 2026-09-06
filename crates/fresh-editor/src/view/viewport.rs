use crate::model::buffer::Buffer;
use crate::model::cursor::Cursor;
use crate::primitives::line_iterator::MAX_LINE_BYTES;
use crate::primitives::line_wrapping::WrapConfig;
use crate::view::ui::view_pipeline::{LineStart, ViewLine};
/// The first visible row, as a buffer byte.
///
/// `row_offset` is a signed displacement from the row `byte` addresses. Zero for
/// ordinary rows. It goes negative when the viewport starts on an injected row —
/// a plugin virtual line drawn above its anchor — because such a row owns no
/// byte of its own and can only be described relative to one.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct ViewAnchor {
    pub byte: usize,
    pub row_offset: isize,
}

/// The viewport - what portion of the buffer is visible
#[derive(Debug, Clone)]
pub struct Viewport {
    /// Where the viewport starts, as one coordinate.
    ///
    /// The pair it replaces — a logical-line byte plus a row offset inside that
    /// line — is the root of this module's remaining complexity. `top_byte` had
    /// to be a *line start*, so on a file that is one enormous line it was
    /// pinned at 0 and the whole scroll position lived in the offset, which the
    /// renderer could only satisfy by building every row from byte 0 and
    /// discarding the ones above the viewport. Two coordinates also need
    /// reconciling with each other, which is what `snap_to_logical_line_start`,
    /// `scrolled_up_in_wrap` and the fresh#1574 patch pair all are.
    ///
    /// One coordinate has nothing to reconcile. See `wrap_model/viewport.py`.
    pub anchor: ViewAnchor,

    /// Left column offset (horizontal scroll position)
    pub left_column: usize,

    /// Terminal dimensions
    pub width: u16,
    pub height: u16,

    /// Scroll offset (lines to keep visible above/below cursor)
    pub scroll_offset: usize,

    /// True while the row-space pass places this viewport vertically — set
    /// each frame by the pre-frame reconcile (`orchestration::reconcile`)
    /// when a wrap index exists for the split's geometry. While set, the byte-oriented `ensure_visible` yields its
    /// vertical half entirely: two passes deciding vertical placement is
    /// fresh#1574, and the byte pass's wrap counting is the one that
    /// disagrees with what is drawn. It stays for two jobs only — horizontal
    /// column scroll with wrap off, and vertical placement on files beyond
    /// the index's size ceilings, where no row space exists to decide in.
    pub(crate) row_pass_owns_placement: bool,

    /// Horizontal scroll offset (columns to keep visible left/right of cursor)
    pub horizontal_scroll_offset: usize,

    /// Whether line wrapping is enabled
    /// When true, horizontal scrolling is disabled
    pub line_wrap_enabled: bool,

    /// Whether the view may scroll sideways to follow the cursor.
    ///
    /// False for a buffer rendered by a mounted widget panel: the widget
    /// runtime already lays its rows out to the panel's exact width and
    /// clips them, and the "cursor" there is a focus marker rather than a
    /// text caret. Letting it drag the view sideways shifts the whole
    /// panel — header included — by however far past the edge the focused
    /// row happens to end.
    pub horizontal_scroll_enabled: bool,

    /// Terminal-grid wrap mode (fresh#2649): with `line_wrap_enabled`,
    /// rows break at exact column boundaries every `wrap_column` columns
    /// (no word-boundary preference, no gutter, no hanging indent),
    /// matching the live PTY grid so entering terminal scroll-back never
    /// reflows. Only terminal buffers set this.
    pub grid_wrap: bool,

    /// Whether wrapped continuation lines should be indented to match leading whitespace
    pub wrap_indent: bool,

    /// Column at which to wrap lines (None = viewport width)
    pub wrap_column: Option<usize>,

    /// Compose-mode page width override.  When `Some(cw)` and the
    /// viewport is wider than `cw`, the renderer wraps content at
    /// `cw` columns and centers it inside the split.  Mirrors
    /// `SplitViewState::compose_width`.
    ///
    /// Scroll math (`Viewport::scroll_*`,
    /// `scrollbar_math::ensure_index`) reads this so per-line visual
    /// row counts are computed at the renderer's effective wrap
    /// width, not the raw split width.  Without that, on a wide
    /// terminal with `compose_width` set, mouse wheel and scrollbar
    /// drag stop short of the buffer's tail because each long
    /// paragraph is counted as 1–2 rows by scroll math but drawn as
    /// 3–4 rows by the renderer.
    pub compose_width: Option<u16>,

    /// Whether line numbers are visible in this viewport.  When
    /// hidden (typical in compose mode), `gutter_width` returns 0
    /// instead of `digits + 4` — keeping scroll math's wrap budget
    /// in sync with the renderer's, which uses
    /// `state.margins.left_total_width()` and gives 0 when the line
    /// number column is suppressed.  Mirrors
    /// `SplitViewState::show_line_numbers`.
    pub show_line_numbers: bool,

    /// Whether viewport needs synchronization with cursor positions
    /// When true, ensure_visible needs to be called before rendering
    /// This allows batching multiple cursor movements into a single viewport update
    needs_sync: bool,

    /// Whether to skip viewport sync on next resize
    /// This is set when restoring a session to prevent the restored scroll position
    /// from being overwritten by ensure_visible during the first render
    skip_resize_sync: bool,

    /// Whether to skip ensure_visible on next render
    /// This is set after scroll actions (Ctrl+Up/Down) to prevent the scroll
    /// from being immediately undone by ensure_visible
    skip_ensure_visible: bool,

    /// Maximum line length encountered so far (in display columns).
    /// Updated incrementally as visible lines are rendered, avoiding full-file scans.
    pub max_line_length_seen: usize,

    /// When true, the next frame should scroll to show the last line at the
    /// bottom of the viewport.  Set by same-buffer scroll sync when the
    /// active split is at the end of the document.  Consumed (cleared) by the
    /// pre-frame reconcile, which applies it in row space
    /// ([`Self::scroll_to_end_in_rows`]).
    pub sync_scroll_to_end: bool,

    /// Small per-viewport row-count cache used by the scroll hot paths
    /// (`scroll_down_visual`, `apply_visual_scroll_limit`, etc.) to
    /// avoid re-running `apply_wrapping_transform` on the same logical
    /// line for every mouse-wheel tick. Particularly important for
    /// buffers with a single very long wrapped line — without this,
    /// each tick pays the O(n²) word-boundary wrap cost over the whole
    /// line.
    ///
    /// Distinct from the cross-consumer cache on `EditorState`: that
    /// one is shared between the renderer and scroll math, while this
    /// one is purely a local memoization for the viewport's own
    /// per-tick counting. The keys use `buffer.version()` as the
    /// version; plugin-driven (soft-break / conceal) changes aren't
    /// detected here, but in the absence of plugins the per-line row
    /// count depends only on buffer content + geometry, which is what
    /// this cache covers.
    pub(crate) wrap_row_cache: crate::view::line_wrap_cache::RowCountCache,
}

impl Viewport {
    /// Byte the viewport starts at.
    ///
    /// Still a logical-line start everywhere except an anchored render, which is
    /// the migration this pair is being collapsed into: once every consumer
    /// reads `anchor` directly these two accessors go away.
    #[inline]
    pub fn top_byte(&self) -> usize {
        self.anchor.byte
    }

    #[inline]
    pub fn set_top_byte(&mut self, byte: usize) {
        self.anchor.byte = byte;
    }

    /// Rows into the logical line at [`Self::top_byte`] that the viewport starts.
    #[inline]
    pub fn top_view_line_offset(&self) -> usize {
        self.anchor.row_offset.max(0) as usize
    }

    #[inline]
    pub fn set_top_view_line_offset(&mut self, rows: usize) {
        self.anchor.row_offset = rows as isize;
    }

    /// Create a new viewport
    pub fn new(width: u16, height: u16) -> Self {
        Self {
            anchor: ViewAnchor::default(),
            left_column: 0,
            width,
            height,
            scroll_offset: 3,
            row_pass_owns_placement: false,
            horizontal_scroll_offset: 5,
            line_wrap_enabled: false,
            horizontal_scroll_enabled: true,
            grid_wrap: false,
            wrap_indent: true,
            wrap_column: None,
            compose_width: None,
            show_line_numbers: true,
            needs_sync: false,
            skip_resize_sync: false,
            skip_ensure_visible: false,
            max_line_length_seen: 0,
            sync_scroll_to_end: false,
            // The scroll hot paths only ever touch a handful of nearby
            // lines per event, so this memo doesn't need to remember
            // every line of every buffer.
            wrap_row_cache: crate::view::line_wrap_cache::RowCountCache::with_capacity(2048),
        }
    }

    /// If `pos` falls inside a hidden fold range, return that range.
    fn containing_hidden_range(
        hidden_ranges: &[(usize, usize)],
        pos: usize,
    ) -> Option<(usize, usize)> {
        hidden_ranges
            .iter()
            .find(|&&(start, end)| pos >= start && pos < end)
            .copied()
    }

    /// First byte at or after `pos` that the renderer actually draws.
    /// A collapsed body occupies no rows, so a scroll walk must step over
    /// it wholesale instead of paying a row per hidden line. Loops because
    /// folds can be adjacent or nested; each jump strictly advances.
    fn skip_hidden_forward(hidden_ranges: &[(usize, usize)], pos: usize) -> usize {
        let mut p = pos;
        while let Some((_, end)) = Self::containing_hidden_range(hidden_ranges, p) {
            p = end;
        }
        p
    }

    /// Line start of the row the reader sees for `pos`: a position inside a
    /// collapsed body maps back to that fold's header line, the one row the
    /// whole region draws. Each jump strictly retreats, so this terminates.
    fn visible_row_start(buffer: &Buffer, hidden_ranges: &[(usize, usize)], pos: usize) -> usize {
        let mut p = pos;
        while let Some((start, _)) = Self::containing_hidden_range(hidden_ranges, p) {
            if start == 0 {
                return 0;
            }
            p = crate::view::folding::indent_folding::find_line_start_byte(buffer, start - 1);
        }
        p
    }

    /// Walk `lines` rendered rows down from `start`, skipping collapsed bodies.
    fn walk_down_visible_lines(
        buffer: &mut Buffer,
        hidden_ranges: &[(usize, usize)],
        start: usize,
        lines: usize,
    ) -> usize {
        let mut position = Self::skip_hidden_forward(hidden_ranges, start);
        for _ in 0..lines {
            let mut iter = buffer.line_iterator(position, 80);
            if iter.next_line().is_none() {
                break;
            }
            position = Self::skip_hidden_forward(hidden_ranges, iter.current_position());
        }
        position
    }

    /// Walk `lines` rendered rows up from `start`, collapsing each hidden
    /// body to the single header row it draws.
    fn walk_up_visible_lines(
        buffer: &mut Buffer,
        hidden_ranges: &[(usize, usize)],
        start: usize,
        lines: usize,
    ) -> usize {
        let mut position = Self::visible_row_start(buffer, hidden_ranges, start);
        for _ in 0..lines {
            let mut iter = buffer.line_iterator(position, 80);
            if iter.prev().is_none() {
                return 0;
            }
            let pos = iter.current_position();
            position = Self::visible_row_start(buffer, hidden_ranges, pos);
        }
        position
    }

    /// Mark viewport to skip sync on next resize (used after session restore)
    pub fn set_skip_resize_sync(&mut self) {
        self.skip_resize_sync = true;
    }

    /// Check and clear the skip_resize_sync flag
    /// Returns true if sync should be skipped
    pub fn should_skip_resize_sync(&mut self) -> bool {
        let skip = self.skip_resize_sync;
        self.skip_resize_sync = false;
        skip
    }

    /// Mark viewport to skip ensure_visible on next render
    /// This is used after scroll actions to prevent the scroll from being undone
    pub fn set_skip_ensure_visible(&mut self) {
        tracing::trace!("set_skip_ensure_visible: setting flag to true");
        self.skip_ensure_visible = true;
    }

    /// Check if ensure_visible should be skipped (does NOT consume the flag)
    /// Returns true if ensure_visible should be skipped
    pub fn should_skip_ensure_visible(&self) -> bool {
        self.skip_ensure_visible
    }

    /// Clear the skip_ensure_visible flag
    /// This should be called after all ensure_visible calls in a render pass
    pub fn clear_skip_ensure_visible(&mut self) {
        self.skip_ensure_visible = false;
    }

    /// Set the scroll offset
    pub fn set_scroll_offset(&mut self, offset: usize) {
        self.scroll_offset = offset;
    }

    /// Update terminal dimensions
    pub fn resize(&mut self, width: u16, height: u16) {
        self.width = width;
        self.height = height;
    }

    /// Effective wrap width for compose-aware scroll math.  Returns
    /// the viewport width clamped to `compose_width` when set.  The
    /// renderer wraps at this width; scroll math must match or
    /// `max_scroll_row` ends up wrong on wide viewports with a narrow
    /// page width.
    #[inline]
    pub fn effective_width(&self) -> u16 {
        match self.compose_width {
            Some(cw) => cw.min(self.width).max(1),
            None => self.width,
        }
    }

    /// Get the number of visible lines
    pub fn visible_line_count(&self) -> usize {
        self.height as usize
    }

    /// Calculate the gutter width based on buffer length
    /// Format: "[indicator]{:>N} │ " where N is the number of digits for line numbers
    /// - Indicator column: 1 char (space, or symbols like ●/✗/⚠)
    /// - Line numbers: N digits (min 2), right-aligned
    /// - Separator: " │ " = 3 chars (space, box char, space)
    ///
    /// Total width = 1 + N + 3 = N + 4 (where N >= 2 minimum, so min 6 total).
    /// The width adapts to the buffer's line count — small files don't waste
    /// space on a 4-digit-wide column. `MIN_LINE_NUMBER_DIGITS` keeps it from
    /// shrinking so much that a 1-line buffer feels cramped.
    pub fn gutter_width(&self, buffer: &Buffer) -> usize {
        let byte_offset_mode = buffer.line_count().is_none();
        let gutter_estimate = if byte_offset_mode {
            // In byte offset mode, gutter shows byte offsets up to file size
            buffer.len().max(1)
        } else {
            buffer.line_count().unwrap_or(1)
        };
        let digits = if gutter_estimate == 0 {
            1
        } else {
            ((gutter_estimate as f64).log10().floor() as usize) + 1
        };
        1 + digits.max(crate::view::margin::MIN_LINE_NUMBER_DIGITS) + 3
    }

    /// Smallest read budget [`row_budget_bytes`](Self::row_budget_bytes) will
    /// hand out. Below a few KB the bounded read stops paying for itself.
    const MIN_ROW_BUDGET_BYTES: usize = 4096;

    /// Bytes-per-column headroom in [`row_budget_bytes`](Self::row_budget_bytes).
    ///
    /// Four bytes is the densest UTF-8 spends on a one-column character;
    /// doubling that lands the prefix past `cap` rows rather than exactly on
    /// the boundary, where it would fall through to the whole-line path.
    const ROW_BUDGET_BYTES_PER_COLUMN: usize = 8;

    /// How many bytes of a logical line are worth reading to decide whether it
    /// wraps past `cap` visual rows.
    ///
    /// Hard-capped at [`MAX_LINE_BYTES`], for two independent reasons. `cap` is
    /// not always screen-bounded — two callers add `top_view_line_offset`, which
    /// grows without limit as you page into one enormous line — so an uncapped
    /// budget would ask for tens of megabytes a keystroke, worse than the
    /// whole-line read it replaces since a saturating count is not cached. And
    /// `LineIterator` decodes each piece separately, so a budget spanning
    /// several would take a lossy cut at every boundary instead of the single
    /// one [`next_line_visual_rows_capped`](Self::next_line_visual_rows_capped)
    /// allows a row of slack for.
    ///
    /// A budget, not a proof: a `cap` too large for 100 KB to settle, or
    /// pathologically narrow content, falls through to the whole-line read —
    /// which is what this did before the cap existed, and is still cached.
    fn row_budget_bytes(cap: usize, wrap_config: &WrapConfig) -> usize {
        let width = wrap_config
            .grid_cols
            .unwrap_or_else(|| {
                wrap_config
                    .first_line_width
                    .saturating_add(wrap_config.gutter_width)
            })
            .max(1);
        cap.saturating_mul(width)
            .saturating_mul(Self::ROW_BUDGET_BYTES_PER_COLUMN)
            .clamp(Self::MIN_ROW_BUDGET_BYTES, MAX_LINE_BYTES)
    }

    /// Visual rows of the next logical line, saturating at `cap`.
    ///
    /// Returns `(line_start, rows, line_end)`. When `line_end` is `Some`,
    /// `rows` is the line's exact row count and `iter` sits on the next line,
    /// exactly as if the caller had used `next_logical_line` itself. When it
    /// is `None` the line was shown to reach `cap` rows from a bounded prefix:
    /// `rows` is reported as `cap`, and `iter` is parked mid-line and must not
    /// be advanced again — every caller has its answer at that point and
    /// returns.
    ///
    /// Scroll math never needs more than a viewport's worth of rows out of one
    /// line; counting them unconditionally wrapped all 53 MB of a single-line
    /// file, several times, before the first frame (issue #1806).
    fn next_line_visual_rows_capped(
        iter: &mut crate::primitives::line_iterator::LineIterator<'_>,
        cap: usize,
        wrap_config: &WrapConfig,
        soft_breaks: &[(usize, u16)],
        virtual_lines: &[usize],
        cache: Option<(&mut crate::view::line_wrap_cache::RowCountCache, u64)>,
    ) -> Option<(usize, usize, Option<usize>)> {
        let cap = cap.max(1);
        let budget = Self::row_budget_bytes(cap, wrap_config);
        let (line_start, mut content, complete) = iter.next_logical_line_budgeted(budget)?;

        if !complete {
            // Sound because word wrap is greedy left to right: appending text
            // never removes a wrap, so a prefix filling `cap` rows means the
            // whole line does. Not cached — the count is a floor, not the
            // line's height.
            //
            // A row of slack for the byte cut: a character split across it
            // decodes to replacement characters a column or two wider, which
            // can add a row the real line lacks. Exactly one such cut, since
            // `row_budget_bytes` never exceeds one `LineIterator` piece.
            //
            // The prefix ends where the *reader* stopped, not where the decoded
            // string does — U+FFFD is wider than what it replaces, so
            // `content.len()` would pull in soft breaks and virtual lines from
            // beyond the prefix.
            let prefix_end = iter.current_position();
            let rows = Self::count_visual_rows_for_line(
                line_start,
                prefix_end,
                content.trim_end_matches(['\n', '\r']),
                wrap_config,
                soft_breaks,
                virtual_lines,
                None,
            );
            if rows > cap {
                return Some((line_start, cap, None));
            }
            // The budget didn't settle it: pay for the whole line, as before.
            iter.finish_logical_line(&mut content);
        }

        let line_end = iter.current_position();
        let rows = Self::count_visual_rows_for_line(
            line_start,
            line_end,
            content.trim_end_matches(['\n', '\r']),
            wrap_config,
            soft_breaks,
            virtual_lines,
            cache,
        );
        Some((line_start, rows, Some(line_end)))
    }

    /// Count visual rows for a single logical line, accounting for plugin soft
    /// breaks (e.g. markdown_compose's hanging-indent wrapping).
    ///
    /// `soft_breaks` is a sorted slice of `(byte_position, indent)` pairs
    /// describing plugin-injected line breaks.  When any fall in
    /// `[line_start, line_end)` we run the renderer's full wrap pipeline
    /// per soft-break-bounded segment (`apply_soft_breaks` →
    /// `apply_wrapping_transform`) so the scroll math agrees row-for-row
    /// with the rendered output even when individual segments still
    /// need word-wrap (markdown_compose's wide tables, very long
    /// paragraphs).  Without breaks we run word-wrap on the whole line.
    ///
    /// Lock-step with the renderer (see `apply_soft_breaks` /
    /// `apply_wrapping_transform` in `split_rendering::transforms`).
    fn count_visual_rows_for_line(
        line_start: usize,
        line_end: usize,
        line_text: &str,
        wrap_config: &WrapConfig,
        soft_breaks: &[(usize, u16)],
        virtual_lines: &[usize],
        cache: Option<(&mut crate::view::line_wrap_cache::RowCountCache, u64)>,
    ) -> usize {
        // Plugin virtual lines (e.g. markdown_compose's `┌─┬─┐` table
        // borders) draw real rows the renderer paints; without them
        // here, mouse wheel / PageDown clamp short of the buffer's
        // tail.
        let v_lo = virtual_lines.partition_point(|p| *p < line_start);
        let v_hi = virtual_lines.partition_point(|p| *p < line_end);
        let extra_virtual_rows = v_hi - v_lo;
        // Terminal-grid wrap: exact-column count, allocation-free. Soft
        // breaks / the shared cache don't apply to terminal buffers, and
        // the count is cheap enough to recompute per call (fresh#2649).
        if let Some(cols) = wrap_config.grid_cols {
            return crate::view::line_wrap_cache::count_visual_rows_for_text_grid(line_text, cols)
                as usize
                + extra_virtual_rows;
        }
        let lo = soft_breaks.partition_point(|p| p.0 < line_start);
        let hi = soft_breaks.partition_point(|p| p.0 < line_end);
        let line_breaks = &soft_breaks[lo..hi];
        if !line_breaks.is_empty() {
            // Run the renderer's full pipeline per segment so segments
            // that still need word-wrap (long paragraph text after a
            // narrow soft-break wrap) are counted at their true row
            // count, not assumed to be one row each.  Skips the cache
            // (the count is already cheap and the soft-break-aware
            // helper isn't keyed for the per-line cache).
            let effective_width = wrap_config
                .first_line_width
                .saturating_add(wrap_config.gutter_width)
                .max(2);
            return crate::view::line_wrap_cache::count_visual_rows_for_text_with_soft_breaks(
                line_text,
                line_start,
                line_breaks,
                effective_width,
                wrap_config.gutter_width,
                wrap_config.hanging_indent,
            ) as usize
                + extra_virtual_rows;
        }
        {
            // Run the renderer's wrap function on a single-Text-token view of
            // the line.  This matches `apply_wrapping_transform`'s word-
            // boundary semantics and uses the same effective width the
            // renderer uses — no more char-wrap-vs-word-wrap drift.
            // See docs/internal/line-wrap-cache-plan.md.
            // `apply_wrapping_transform`'s available text width is
            //   effective_width - gutter_width
            // where `effective_width` is the value passed as its
            // `content_width` parameter.
            //
            // We want the inner `available_width` to equal
            // `wrap_config.first_line_width` — that IS the text column
            // budget the renderer uses (its content_width is
            // `viewport.width - 1` for EOL-cursor reservation, and
            // viewport.width already excludes the scrollbar; WrapConfig
            // happens to encode the same end result because its caller
            // doubles the scrollbar subtraction).
            //
            // So: effective_width = first_line_width + gutter_width.
            let effective_width = wrap_config
                .first_line_width
                .saturating_add(wrap_config.gutter_width)
                .max(2);

            // The viewport-local cache is a count-only memoization for
            // the scroll hot paths — it holds row counts, not layout.
            // The cross-consumer cache on `EditorState` is the one that
            // holds real `ViewLine`s from the full pipeline.
            let compute = || {
                crate::view::line_wrap_cache::count_visual_rows_for_text(
                    line_text,
                    effective_width,
                    wrap_config.gutter_width,
                    wrap_config.hanging_indent,
                )
            };
            // The cache is keyed by `line_start`, so an entry is only
            // reusable if `line_text` really is that whole line. Callers
            // that pass a prefix (cursor-row math) or a `LineIterator`
            // piece cut at the read budget would otherwise store a short
            // count under the line's key and every later lookup — from
            // the wheel, the scrollbar clamp, ensure-visible — would read
            // it back (issue #2843: a 100 KB piece of a 441 KB line
            // cached 916 rows for a 4130-row line). Trailing `\r\n` is
            // trimmed off `line_text`, hence the 2-byte slack.
            let covers_whole_line = line_text.len() + 2 >= line_end.saturating_sub(line_start);
            if let Some((cache, pipeline_inputs_ver)) = cache.filter(|_| covers_whole_line) {
                use crate::view::line_wrap_cache::{CacheViewMode, LineWrapKey};
                let key = LineWrapKey {
                    pipeline_inputs_version: pipeline_inputs_ver,
                    view_mode: CacheViewMode::Source,
                    line_start,
                    effective_width: effective_width as u32,
                    gutter_width: wrap_config.gutter_width as u16,
                    wrap_column: None,
                    hanging_indent: wrap_config.hanging_indent,
                    line_wrap_enabled: true,
                    // Grid mode returns before this cache path.
                    grid_wrap: false,
                    // Scroll math is cursor-blind by convention (matches
                    // `WrapIndex` and its own cursor-free inputs).
                    cursor_sig: 0,
                };
                return cache.get_or_insert_with(key, compute) as usize + extra_virtual_rows;
            }
            compute() as usize + extra_virtual_rows
        }
    }
}

/// Source byte at the start of each visual (word-wrap) row of `line_text`,
/// where `line_start` is the absolute byte offset of the line.  The Nth
/// entry is the byte position that the renderer draws at the start of the
/// Nth visual row — the byte counterpart of
/// [`count_visual_rows_for_text`](crate::view::line_wrap_cache::count_visual_rows_for_text)
/// (which returns only the row *count*).  Used to translate the viewport's
/// `top_view_line_offset` (a visual-row index inside the logical line at
/// `top_byte`) back into a buffer byte so PageUp/PageDown can land the
/// cursor on the row actually shown at the top of the viewport — without
/// this, a single hugely-wrapped line maps every visual row back to the
/// line's start byte.
///
/// Drives the same machine as the renderer, so the byte mapping and the
/// drawn rows cannot disagree.
fn wrap_segment_source_bytes(
    line_text: &str,
    line_start: usize,
    effective_width: usize,
    gutter_width: usize,
    hanging_indent: bool,
) -> Vec<usize> {
    use crate::view::wrap_machine::{WrapMachine, WrapRule};
    use fresh_core::api::{ViewTokenWire, ViewTokenWireKind};

    let tokens = vec![ViewTokenWire {
        source_offset: Some(line_start),
        kind: ViewTokenWireKind::Text(line_text.to_string()),
        style: None,
    }];
    let out = WrapMachine::run(
        tokens,
        WrapRule::Word {
            content_width: effective_width,
            gutter_width,
            hanging_indent,
        },
    );
    let mut rows: Vec<usize> = out
        .rows
        .iter()
        .map(|r| r.source_byte.unwrap_or(line_start))
        .collect();
    if rows.is_empty() {
        rows.push(line_start);
    }
    rows
}

impl Viewport {
    /// Source byte of the visual row currently shown at the top of the
    /// viewport, accounting for `top_view_line_offset` rows into the
    /// soft-wrapped logical line at `top_byte`.
    ///
    /// Mirrors the scroll primitives' notion of position: `scroll_*_visual`
    /// keep `top_byte` at a logical-line start and stash the wrap-segment
    /// index in `top_view_line_offset`.  PageUp/PageDown need the *byte* of
    /// that row to land the cursor where the user is looking; using
    /// `top_byte` alone teleports the cursor to the logical-line start,
    /// which on a single hugely-wrapped file is the very top of the buffer.
    ///
    /// Returns `top_byte` when the offset is 0, when wrapping is off, or
    /// when plugin soft-breaks / virtual lines intersect the line (those
    /// rows aren't plain word-wrap segments — their byte mapping is owned
    /// by the render pipeline, so we conservatively defer to `top_byte`).
    pub fn top_visual_row_source_byte(
        &mut self,
        buffer: &mut Buffer,
        soft_breaks: &[(usize, u16)],
        virtual_lines: &[usize],
        hidden_ranges: &[(usize, usize)],
    ) -> usize {
        if !self.line_wrap_enabled || self.top_view_line_offset() == 0 {
            return self.top_byte();
        }

        let line_start = self.top_byte();
        let offset = self.top_view_line_offset();

        // Walk the rows as one continuous run. Wrapping each `MAX_LINE_BYTES`
        // read piece on its own restarts the machine at column 0 and re-measures
        // a hanging indent from mid-line content, so it gained a row per piece
        // against `top_view_line_offset`, which is counted over the joined line.
        let rule = self.wrap_rule(buffer);
        let folds = Self::fold_skip(hidden_ranges);
        let starts = crate::view::row_walk::row_starts_from(
            buffer,
            line_start,
            rule,
            offset.saturating_add(1),
            &folds,
        );
        let walk_end = starts.last().copied().unwrap_or(line_start);

        // Plugin soft-breaks and virtual rows make `top_view_line_offset` count
        // rows that are not plain word-wrap segments; their byte mapping lives
        // in the render pipeline, so defer to the old behaviour for such lines.
        // Tested over the span actually walked — asking where the logical line
        // *ends* would read all of it, which is the cost this branch removes.
        let touches_soft_break = soft_breaks
            .iter()
            .any(|(p, _)| *p >= line_start && *p <= walk_end);
        let touches_virtual = virtual_lines
            .iter()
            .any(|p| *p >= line_start && *p <= walk_end);
        if touches_soft_break || touches_virtual {
            return self.top_byte();
        }

        starts.get(offset).copied().unwrap_or(walk_end)
    }

    /// Source byte of each row `text` is drawn as, given it starts at
    /// `text_start`.
    ///
    /// Wrapped at the same width as [`Self::compute_line_layout`], which counts
    /// the rows this enumerates.
    fn drawn_row_source_bytes(
        text: &str,
        text_start: usize,
        wrap_config: &WrapConfig,
    ) -> Vec<usize> {
        if let Some(cols) = wrap_config.grid_cols {
            return crate::view::line_wrap_cache::grid_segment_source_bytes(text, text_start, cols);
        }
        let effective_width = wrap_config
            .first_line_width
            .saturating_add(wrap_config.gutter_width)
            .max(2);
        wrap_segment_source_bytes(
            text,
            text_start,
            effective_width,
            wrap_config.gutter_width,
            wrap_config.hanging_indent,
        )
    }

    /// Scroll by `delta` visual rows using the wrap index — the whole of wheel
    /// scrolling, as arithmetic.
    ///
    /// Replaces what `scroll_up_visual` / `scroll_down_visual` /
    /// `apply_visual_scroll_limit` do by reading text: each of those walks the
    /// logical line counting wrap segments, so a wheel event on a file that is
    /// one enormous line reads and re-decodes that line twice. Here the index
    /// already knows which row is where, so nothing is read at all.
    ///
    /// The viewport keeps its `(top_byte, top_view_line_offset)` pair; the new
    /// absolute row is converted back through `byte_of_row`.
    pub fn scroll_visual_rows(
        &mut self,
        index: &crate::view::wrap_index::WrapIndex,
        buffer: &Buffer,
        delta: isize,
    ) {
        let top_line = buffer.get_line_number(self.top_byte());
        let top_row =
            index.line_first_row(top_line) as isize + self.top_view_line_offset() as isize;
        let total = index.total_rows() as isize;
        let max_top = (total - self.visible_line_count() as isize).max(0);
        let new_top = (top_row + delta).clamp(0, max_top);
        if new_top == top_row {
            return;
        }
        let addr = index.byte_of_row(buffer, new_top as u32);
        self.set_top_byte(buffer.line_start_offset(addr.line).unwrap_or(0));
        self.set_top_view_line_offset(addr.row_in_line);
    }

    /// Scroll up by N lines (byte-based)
    /// When line_wrap_enabled is true, scrolls by visual rows instead of logical lines
    ///
    /// `soft_breaks` is a sorted slice of plugin-injected break byte positions.
    /// Pass an empty slice when there are no plugin breaks (raw mode, etc.).
    ///
    /// `hidden_ranges` holds `(start_byte, end_byte)` for collapsed folds so the
    /// walk counts rendered rows, not logical lines. Without it a page of scroll
    /// budget is spent on lines nobody can see and the viewport barely moves.
    pub fn scroll_up(
        &mut self,
        buffer: &mut Buffer,
        soft_breaks: &[(usize, u16)],
        virtual_lines: &[usize],
        hidden_ranges: &[(usize, usize)],
        lines: usize,
    ) {
        if self.line_wrap_enabled {
            self.scroll_up_visual(buffer, soft_breaks, virtual_lines, hidden_ranges, lines);
        } else {
            let new_position = if hidden_ranges.is_empty() {
                let mut iter = buffer.line_iterator(self.top_byte(), 80);
                for _ in 0..lines {
                    if iter.prev().is_none() {
                        break;
                    }
                }
                iter.current_position()
            } else {
                Self::walk_up_visible_lines(buffer, hidden_ranges, self.top_byte(), lines)
            };
            self.set_top_byte_with_limit(buffer, soft_breaks, virtual_lines, new_position);
        }
    }

    /// Vertically center the viewport on `position`.
    ///
    /// In wrap mode this centers the *visual row* containing `position`,
    /// not its logical line: wrapped lines above the target (and a target
    /// buried deep inside one long wrapped line) are counted in real
    /// visual rows so the match lands mid-pane. Centering by logical line
    /// (the naive `line - height/2`) drifts badly in heavily-wrapped files
    /// because each logical line above can occupy many rows.
    ///
    /// Soft breaks / virtual lines are assumed absent (the Live Grep
    /// preview's only caller loads plain file buffers), so an empty slice
    /// is passed to the visual-row scroll.
    pub fn center_on_position(&mut self, buffer: &mut Buffer, position: usize) {
        let half = self.visible_line_count() / 2;

        if !self.line_wrap_enabled {
            // Unwrapped: one visual row per logical line, so walk back
            // `half` logical lines from the target.
            let mut iter = buffer.line_iterator(position, 80);
            for _ in 0..half {
                if iter.prev().is_none() {
                    break;
                }
            }
            self.set_top_byte(iter.current_position());
            self.set_top_view_line_offset(0);
            return;
        }

        // Wrapped: find which visual row inside its logical line the
        // target sits on, anchor the viewport top to that row, then
        // scroll up `half` real visual rows (which walks back through any
        // wrapped lines above).
        let line = buffer.get_line_number(position);
        let line_start = buffer.line_start_offset(line).unwrap_or(position);
        let wrap_config = self.make_wrap_config(buffer);
        let match_row_in_line = if position > line_start {
            let prefix = buffer
                .get_text_range_mut(line_start, position - line_start)
                .ok()
                .and_then(|b| String::from_utf8(b).ok())
                .unwrap_or_default();
            // Rows the pre-match text occupies; the target is on the last
            // of them (`saturating_sub(1)` maps a 1-row prefix to row 0).
            Self::count_visual_rows_for_line(
                line_start,
                position,
                &prefix,
                &wrap_config,
                &[],
                &[],
                None,
            )
            .saturating_sub(1)
        } else {
            0
        };

        self.set_top_byte(line_start);
        self.set_top_view_line_offset(match_row_in_line);
        self.scroll_up(buffer, &[], &[], &[], half);
    }

    /// Scroll down by N lines (byte-based)
    /// When line_wrap_enabled is true, scrolls by visual rows instead of logical lines
    ///
    /// `soft_breaks` is a sorted slice of plugin-injected break byte positions.
    /// Pass an empty slice when there are no plugin breaks (raw mode, etc.).
    ///
    /// `hidden_ranges` holds `(start_byte, end_byte)` for collapsed folds so the
    /// walk counts rendered rows, not logical lines. Without it a page of scroll
    /// budget is spent on lines nobody can see and the viewport barely moves.
    pub fn scroll_down(
        &mut self,
        buffer: &mut Buffer,
        soft_breaks: &[(usize, u16)],
        virtual_lines: &[usize],
        hidden_ranges: &[(usize, usize)],
        lines: usize,
    ) {
        if self.line_wrap_enabled {
            self.scroll_down_visual(buffer, soft_breaks, virtual_lines, hidden_ranges, lines);
        } else {
            let new_position = if hidden_ranges.is_empty() {
                let mut iter = buffer.line_iterator(self.top_byte(), 80);
                for _ in 0..lines {
                    if iter.next_line().is_none() {
                        break;
                    }
                }
                iter.current_position()
            } else {
                Self::walk_down_visible_lines(buffer, hidden_ranges, self.top_byte(), lines)
            };
            self.set_top_byte_with_limit(buffer, soft_breaks, virtual_lines, new_position);
        }
    }

    /// Scroll up by N visual rows (for line-wrapped content)
    /// This counts wrapped segments, not logical lines
    fn scroll_up_visual(
        &mut self,
        buffer: &mut Buffer,
        soft_breaks: &[(usize, u16)],
        virtual_lines: &[usize],
        hidden_ranges: &[(usize, usize)],
        visual_rows: usize,
    ) {
        if visual_rows == 0 {
            return;
        }

        // Addressed by byte: scrolling reads the rows it moves over and nothing
        // else. The walk below counts from the logical line's start, which on a
        // one-line file is every row above the viewport.
        if crate::view::row_walk::addresses_rows_by_byte(buffer, self.line_wrap_enabled) {
            let rule = self.wrap_rule(buffer);
            let folds = Self::fold_skip(hidden_ranges);
            let top = self.top_byte();
            let new_top =
                crate::view::row_walk::row_start_before(buffer, top, visual_rows, rule, &folds);
            self.set_anchored_top(buffer, rule, &folds, new_top);
            return;
        }

        let buffer_version = buffer.version();
        let wrap_config = self.make_wrap_config(buffer);

        // We need to move backwards through visual rows
        // Start from current top_byte and count backwards
        let mut rows_remaining = visual_rows;
        let mut current_byte = self.top_byte();

        // First, check if we have a top_view_line_offset (mid-line position)
        // If so, we can scroll up within the current line first
        if self.top_view_line_offset() > 0 {
            let rows_in_offset = self.top_view_line_offset().min(rows_remaining);
            self.set_top_view_line_offset(self.top_view_line_offset() - rows_in_offset);
            rows_remaining -= rows_in_offset;
            if rows_remaining == 0 {
                return;
            }
        }

        // Now scroll backwards through logical lines, counting visual rows
        let mut iter = buffer.line_iterator(current_byte, 80);

        while rows_remaining > 0 {
            // Move to previous line
            if iter.prev().is_none() {
                // Hit beginning of buffer
                self.set_top_byte(0);
                self.set_top_view_line_offset(0);
                return;
            }

            // A collapsed body draws as its header row, so land on the header
            // and charge the whole region one row rather than one per line.
            let raw_start = iter.current_position();
            let line_start = Self::visible_row_start(buffer, hidden_ranges, raw_start);
            iter = buffer.line_iterator(line_start, 80);

            // Get the line content to calculate how many visual rows it has
            let (line_end, line_content) = if let Some((_, content)) = iter.next_logical_line() {
                let end = iter.current_position();
                (end, content.trim_end_matches(['\n', '\r']).to_string())
            } else {
                (line_start, String::new())
            };
            // Move back to the line start position
            iter = buffer.line_iterator(line_start, 80);

            let visual_rows_in_line = Self::count_visual_rows_for_line(
                line_start,
                line_end,
                &line_content,
                &wrap_config,
                soft_breaks,
                virtual_lines,
                Some((&mut self.wrap_row_cache, buffer_version)),
            );

            if visual_rows_in_line >= rows_remaining {
                // This line has enough visual rows to satisfy the remaining scroll
                // Position at the appropriate segment within this line
                self.set_top_byte(line_start);
                self.set_top_view_line_offset(visual_rows_in_line - rows_remaining);
                return;
            }

            // This line doesn't have enough rows, continue to previous line
            rows_remaining -= visual_rows_in_line;
            current_byte = line_start;
        }

        self.set_top_byte(current_byte);
        self.set_top_view_line_offset(0);
    }

    /// Scroll down by N visual rows (for line-wrapped content)
    /// This counts wrapped segments, not logical lines
    fn scroll_down_visual(
        &mut self,
        buffer: &mut Buffer,
        soft_breaks: &[(usize, u16)],
        virtual_lines: &[usize],
        hidden_ranges: &[(usize, usize)],
        visual_rows: usize,
    ) {
        if visual_rows == 0 {
            return;
        }

        // Mirror of `scroll_up_visual`'s anchored branch.
        if crate::view::row_walk::addresses_rows_by_byte(buffer, self.line_wrap_enabled) {
            let rule = self.wrap_rule(buffer);
            let folds = Self::fold_skip(hidden_ranges);
            let top = self.top_byte();
            let new_top =
                crate::view::row_walk::row_start_after(buffer, top, rule, visual_rows, &folds);
            self.set_anchored_top(buffer, rule, &folds, new_top);
            return;
        }

        let buffer_version = buffer.version();
        let wrap_config = self.make_wrap_config(buffer);
        let buffer_len = buffer.len();

        let mut rows_remaining = visual_rows;
        let current_top = self.top_byte();
        let mut iter = buffer.line_iterator(current_top, 80);

        // First, handle any existing top_view_line_offset
        // Get current line's visual row count to see how many rows are left in
        // it. Only `top_view_line_offset + rows_remaining + 1` rows can change
        // the outcome below, so the count stops there — a line taller than that
        // absorbs the whole scroll wherever its real bottom is.
        let current_visual_rows = match Self::next_line_visual_rows_capped(
            &mut iter,
            self.top_view_line_offset() + rows_remaining + 1,
            &wrap_config,
            soft_breaks,
            virtual_lines,
            Some((&mut self.wrap_row_cache, buffer_version)),
        ) {
            Some((_, rows, _)) => rows,
            // Past the end of the buffer: no line to count, which the row
            // counter has always reported as the one row an empty line draws.
            None => 1,
        };
        // Reset iterator to start of this line for later use
        iter = buffer.line_iterator(current_top, 80);
        let rows_left_in_current = current_visual_rows.saturating_sub(self.top_view_line_offset());

        if rows_remaining < rows_left_in_current {
            // Can satisfy scroll within current line, but we still
            // need to reclamp: if the current line is the last line
            // of the buffer, advancing `top_view_line_offset` can push
            // the viewport past the point where it can be filled with
            // real content, leaving past-EOF `~` rows below.
            self.set_top_view_line_offset(self.top_view_line_offset() + rows_remaining);
            self.apply_visual_scroll_limit(buffer, soft_breaks, virtual_lines, &wrap_config);
            return;
        }

        // Move past the current line
        rows_remaining -= rows_left_in_current;
        self.set_top_view_line_offset(0);

        // Move to next line
        if iter.next_logical_line().is_none() {
            // Already at end of buffer
            return;
        }

        // Continue scrolling through subsequent lines
        loop {
            let raw_start = iter.current_position();
            let line_start = Self::skip_hidden_forward(hidden_ranges, raw_start);
            if line_start != raw_start {
                // Collapsed body: no rows drawn, so step past it for free.
                iter = buffer.line_iterator(line_start, 80);
            }

            // Check for end of buffer
            if line_start >= buffer_len {
                self.set_top_byte_with_limit(buffer, soft_breaks, virtual_lines, line_start);
                return;
            }

            let Some((_, visual_rows_in_line, _)) = Self::next_line_visual_rows_capped(
                &mut iter,
                rows_remaining + 1,
                &wrap_config,
                soft_breaks,
                virtual_lines,
                Some((&mut self.wrap_row_cache, buffer_version)),
            ) else {
                // End of buffer
                self.set_top_byte_with_limit(buffer, soft_breaks, virtual_lines, line_start);
                return;
            };

            if rows_remaining < visual_rows_in_line {
                // This line has enough visual rows to satisfy the scroll
                self.set_top_byte(line_start);
                self.set_top_view_line_offset(rows_remaining);
                // Apply visual-row-aware scroll limit
                self.apply_visual_scroll_limit(buffer, soft_breaks, virtual_lines, &wrap_config);
                return;
            }

            // Not enough rows in this line, continue to next
            rows_remaining -= visual_rows_in_line;

            if rows_remaining == 0 {
                // Exactly consumed this line, position at start of next
                let next_pos = iter.current_position();
                self.set_top_byte(next_pos);
                self.set_top_view_line_offset(0);
                // Apply visual-row-aware scroll limit
                self.apply_visual_scroll_limit(buffer, soft_breaks, virtual_lines, &wrap_config);
                return;
            }
        }
    }

    /// Apply visual-row-aware scroll limit to prevent over-scrolling.
    /// This ensures the viewport is always filled with content when possible.
    /// Returns true if position was adjusted, false if no adjustment needed.
    fn apply_visual_scroll_limit(
        &mut self,
        buffer: &mut Buffer,
        soft_breaks: &[(usize, u16)],
        virtual_lines: &[usize],
        wrap_config: &WrapConfig,
    ) {
        let viewport_height = self.visible_line_count();
        if viewport_height == 0 {
            return;
        }

        let buffer_version = buffer.version();
        // Count visual rows from current position to end of buffer
        let mut visual_rows_remaining = 0;
        let mut iter = buffer.line_iterator(self.top_byte(), 80);
        let top_offset = self.top_view_line_offset();

        // First, count rows in current line (from top_view_line_offset to end).
        // The rows above the offset are scrolled off, so this line has to
        // reach `viewport_height + top_offset` before it can fill the viewport
        // on its own.
        if let Some((_, rows, line_end)) = Self::next_line_visual_rows_capped(
            &mut iter,
            viewport_height + top_offset,
            wrap_config,
            soft_breaks,
            virtual_lines,
            Some((&mut self.wrap_row_cache, buffer_version)),
        ) {
            visual_rows_remaining += rows.saturating_sub(top_offset);
            if line_end.is_none() {
                // Saturated: this line alone fills the viewport.
                return;
            }
        }

        // Count rows in subsequent lines
        while visual_rows_remaining < viewport_height {
            let Some((_, rows, line_end)) = Self::next_line_visual_rows_capped(
                &mut iter,
                viewport_height - visual_rows_remaining,
                wrap_config,
                soft_breaks,
                virtual_lines,
                Some((&mut self.wrap_row_cache, buffer_version)),
            ) else {
                break;
            };
            visual_rows_remaining += rows;

            // Early exit if we have enough rows
            if visual_rows_remaining >= viewport_height {
                return; // No need to adjust
            }
            if line_end.is_none() {
                break;
            }
        }

        // If we don't have enough rows to fill viewport, find the max scroll position
        // and set it directly (instead of calling scroll_up_visual which can be jumpy)
        if visual_rows_remaining < viewport_height {
            // Find the max scroll position by scanning from the beginning
            let (max_byte, max_offset) = self.find_max_visual_scroll_position(
                buffer,
                soft_breaks,
                virtual_lines,
                wrap_config,
                viewport_height,
            );
            self.set_top_byte(max_byte);
            self.set_top_view_line_offset(max_offset);
        }
    }

    /// Find the maximum scroll position that still shows viewport_height visual rows.
    /// Returns (top_byte, top_view_line_offset) for the max scroll position.
    fn find_max_visual_scroll_position(
        &mut self,
        buffer: &mut Buffer,
        soft_breaks: &[(usize, u16)],
        virtual_lines: &[usize],
        wrap_config: &WrapConfig,
        viewport_height: usize,
    ) -> (usize, usize) {
        let buffer_version = buffer.version();
        let buffer_len = buffer.len();
        if buffer_len == 0 {
            return (0, 0);
        }

        // Scan backward from the end to find a starting point, then scan forward.
        // This is O(viewport_height) instead of O(total_lines), avoiding a full-file
        // scan that hangs on large files.
        let scan_start = {
            let mut iter = buffer.line_iterator(buffer_len, 80);
            // Go back 2x viewport_height logical lines — each line produces at least
            // 1 visual row, so this guarantees enough visual rows.
            for _ in 0..(viewport_height * 2) {
                if iter.prev().is_none() {
                    break;
                }
            }
            iter.current_position()
        };

        // Build visual row positions from scan_start to end of file
        let mut positions: Vec<(usize, usize)> = Vec::new();
        let mut iter = buffer.line_iterator(scan_start, 80);
        while let Some((line_start, content)) = iter.next_logical_line() {
            let line_end = iter.current_position();
            let line_content = content.trim_end_matches(['\n', '\r']).to_string();
            let visual_rows_in_line = Self::count_visual_rows_for_line(
                line_start,
                line_end,
                &line_content,
                wrap_config,
                soft_breaks,
                virtual_lines,
                Some((&mut self.wrap_row_cache, buffer_version)),
            );

            for offset in 0..visual_rows_in_line {
                positions.push((line_start, offset));
            }
        }

        let total_rows = positions.len();
        if total_rows <= viewport_height {
            // Everything from scan_start fits — the whole file fits in the viewport
            return (0, 0);
        }

        let max_scroll_row = total_rows - viewport_height;
        positions[max_scroll_row]
    }

    /// Scroll through ViewLines (view-transform aware)
    ///
    /// This method scrolls through display lines rather than source lines,
    /// correctly handling view transforms that inject headers or other content.
    ///
    /// # Arguments
    /// * `view_lines` - The current display lines (from ViewLineIterator)
    /// * `line_offset` - Positive to scroll down, negative to scroll up
    ///
    /// # Returns
    /// The new top_byte position after scrolling
    pub fn scroll_view_lines(&mut self, view_lines: &[ViewLine], line_offset: isize) {
        let viewport_height = self.visible_line_count();
        if view_lines.is_empty() || viewport_height == 0 {
            return;
        }

        // Find the current view line index that corresponds to top_byte
        let current_idx = self.find_view_line_for_byte(view_lines, self.top_byte());

        // Calculate target index
        let target_idx = if line_offset >= 0 {
            current_idx.saturating_add(line_offset as usize)
        } else {
            current_idx.saturating_sub(line_offset.unsigned_abs())
        };

        // Apply scroll limit: don't scroll past the point where viewport can't be filled
        let max_top_idx = view_lines.len().saturating_sub(viewport_height);
        let clamped_idx = target_idx.min(max_top_idx);

        // Get the source byte for the target view line
        if let Some(new_top_byte) = self.get_source_byte_for_view_line(view_lines, clamped_idx) {
            tracing::trace!(
                "scroll_view_lines: offset={}, current_idx={}, target_idx={}, clamped_idx={}, new_top_byte={}",
                line_offset, current_idx, target_idx, clamped_idx, new_top_byte
            );
            self.set_top_byte(new_top_byte);
        }
    }

    /// Find the view line index that contains a source byte position
    /// Returns the line where the byte falls within its range, not just the first line
    /// starting at or after the byte.
    fn find_view_line_for_byte(&self, view_lines: &[ViewLine], target_byte: usize) -> usize {
        // Find the line that contains the target byte by checking if target is
        // between this line's start and the next line's start
        let mut best_match = 0;

        for (idx, line) in view_lines.iter().enumerate() {
            if let Some(first_source) = line.char_source_bytes.iter().find_map(|m| *m) {
                if first_source <= target_byte {
                    // This line starts at or before target, so it might contain it
                    best_match = idx;
                } else {
                    // This line starts after target, so previous line contains it
                    break;
                }
            }
        }

        // If the cursor is past the last source-mapped byte, check whether there
        // is a trailing empty view line (after a source newline) that the cursor
        // belongs on — e.g. the empty line after a file's trailing '\n'.
        if let Some(last) = view_lines.last() {
            let last_idx = view_lines.len() - 1;
            if last_idx > best_match
                && last.char_source_bytes.is_empty()
                && matches!(last.line_start, LineStart::AfterSourceNewline)
            {
                let best_max = view_lines[best_match]
                    .char_source_bytes
                    .iter()
                    .filter_map(|b| *b)
                    .max()
                    .unwrap_or(0);
                if target_byte > best_max {
                    return last_idx;
                }
            }
        }

        best_match
    }

    /// Get the source byte position for a view line index
    /// For injected lines (headers), walks forward to find the next source line
    fn get_source_byte_for_view_line(&self, view_lines: &[ViewLine], idx: usize) -> Option<usize> {
        // Start from the requested index and walk forward to find a line with source mapping
        for line in view_lines.iter().skip(idx) {
            if let Some(source_byte) = line.char_source_bytes.iter().find_map(|m| *m) {
                return Some(source_byte);
            }
        }
        // If all remaining lines are injected, try to get the last known source position
        // by walking backwards
        for line in view_lines.iter().take(idx).rev() {
            if let Some(source_byte) = line.char_source_bytes.iter().find_map(|m| *m) {
                // This is the last source position before our target
                // We want to stay at that position
                return Some(source_byte);
            }
        }
        // No source bytes found at all - keep current position
        Some(self.top_byte())
    }

    /// Ensure cursor is visible using view lines (Layout-aware)
    ///
    /// This method uses view lines to check visibility, correctly handling
    /// view transforms that inject headers or other virtual content.
    ///
    /// # Arguments
    /// * `view_lines` - The current display lines (from ViewLineIterator)
    /// * `cursor` - The cursor to ensure is visible
    /// * `gutter_width` - Width of the gutter (for cursor positioning)
    ///
    /// Returns true if scrolling occurred.
    /// Scroll the cursor into view using **absolute visual rows**, before any
    /// rows are built.
    ///
    /// The layout-based [`Self::ensure_visible_in_layout`] can only run *after*
    /// `build_view_data`, so a frame that placed there had to build rows to
    /// discover it needed to scroll and then rebuild because it did — the
    /// up-to-three builds per frame `compute_buffer_layout` used to carry.
    /// Deciding in row space needs no rows at all: the wrap index answers
    /// "which row is the cursor on" with a binary search. This runs from the
    /// pre-frame reconcile (`orchestration::reconcile::place_pane`), before
    /// anything is built.
    ///
    /// Returns whether this pass **owns vertical placement** for the frame —
    /// not whether it moved anything. The caller forwards it to
    /// [`Self::ensure_visible_in_layout`] as `rows_settled`, and the two must
    /// not both apply the margin. "Decided not to scroll" is still owning the
    /// decision, so a `true` return with no movement is normal.
    ///
    /// The viewport keeps its `(top_byte, top_view_line_offset)` pair — a new
    /// absolute top row is converted back through `byte_of_row`, so this changes
    /// *how* the decision is made, not what the viewport is.
    ///
    /// Only the margin phase lives here. Revealing virtual lines above the
    /// cursor and horizontal column scrolling still need materialised rows and
    /// stay in the layout pass, which finds the cursor already in range and does
    /// nothing further.
    pub fn ensure_visible_in_rows(
        &mut self,
        index: &crate::view::wrap_index::WrapIndex,
        buffer: &crate::model::buffer::Buffer,
        cursor_byte: usize,
        expansion: Option<&CursorLineExpansion>,
    ) -> bool {
        if self.should_skip_resize_sync() || self.should_skip_ensure_visible() {
            return false;
        }
        let viewport_height = self.visible_line_count();
        if viewport_height == 0 {
            return false;
        }

        // Effective rows: the canonical index with the cursor's one divergent
        // line expanded to its drawn row count. Placement must target the row
        // the cursor is *drawn* on, and clamp against the rows that will
        // actually exist — a document that fits the window canonically can
        // exceed it revealed, and the canonical clamp would forbid the scroll
        // that shows the cursor at all. See the model's
        // `EditorModel.ensure_cursor_visible`, which this mirrors exactly.
        let (exp_line_start, exp_first, exp_canonical, exp_drawn) = match expansion {
            Some(e) => (
                e.line_start,
                e.first_row as usize,
                e.canonical_rows,
                e.drawn_rows,
            ),
            None => (usize::MAX, 0, 0, 0),
        };
        let delta = exp_drawn.saturating_sub(exp_canonical);

        let total_rows = index.total_rows() as usize + delta;
        let top_line = buffer.get_line_number(self.top_byte());
        let canonical_top = index.line_first_row(top_line) as usize + self.top_view_line_offset();
        let top_row = if delta > 0 && self.top_byte() == exp_line_start {
            // An anchor inside the divergent line counts drawn rows — that is
            // how the anchored build slices the cursor-aware stream.
            exp_first + self.top_view_line_offset()
        } else if delta > 0 && canonical_top >= exp_first + exp_canonical {
            canonical_top + delta
        } else {
            canonical_top
        };
        let cursor_row = match expansion {
            Some(e) => e.cursor_row_drawn as usize,
            None => index.row_of_byte(buffer, cursor_byte) as usize,
        };

        // Same margin rule as the layout pass, in absolute rows — including its
        // guard, which is `top_view_line_offset > 0`, i.e. "the viewport is
        // parked inside a wrapped line". Not `top_row > 0`, which is true after
        // any scroll at all: with wrap off that turns on a margin the layout
        // pass deliberately leaves off, because there the byte-oriented
        // pre-render `ensure_visible` has already placed the cursor and a second
        // margin just pushes it a few rows further from the edge.
        let apply_margin = self.line_wrap_enabled || self.top_view_line_offset() > 0;
        let margin = if apply_margin {
            self.scroll_offset.min(viewport_height / 2)
        } else {
            0
        };
        let max_top = total_rows.saturating_sub(viewport_height);

        let in_top_margin = cursor_row < top_row + margin;
        let in_bottom_margin = cursor_row + margin + 1 > top_row + viewport_height;
        if !in_top_margin && !in_bottom_margin {
            return apply_margin;
        }

        let target_top = if in_top_margin {
            cursor_row.saturating_sub(margin)
        } else {
            (cursor_row + margin + 1).saturating_sub(viewport_height)
        };

        let new_top = target_top.min(max_top);
        if new_top == top_row {
            return apply_margin;
        }

        if delta > 0 && new_top >= exp_first && new_top < exp_first + exp_drawn {
            // Target lands inside the expanded line: anchor at its start with a
            // drawn-row offset, which the anchored build interprets directly.
            self.set_top_byte(exp_line_start);
            self.set_top_view_line_offset(new_top - exp_first);
        } else {
            let canonical_target = if delta > 0 && new_top >= exp_first + exp_drawn {
                new_top - delta
            } else {
                new_top
            };
            let addr = index.byte_of_row(buffer, canonical_target as u32);
            self.set_top_byte(buffer.line_start_offset(addr.line).unwrap_or(0));
            self.set_top_view_line_offset(addr.row_in_line);
        }
        apply_margin
    }

    /// Horizontal placement only.
    ///
    /// Vertical placement lives in [`Self::ensure_visible_in_rows`], which runs
    /// before any row is built and decides in absolute rows. This pass used to
    /// do it too, from the window it was handed — and the two disagreed
    /// structurally, because a window that starts at a logical line cannot
    /// express a viewport parked inside one. That disagreement is fresh#1574.
    ///
    pub fn ensure_visible_in_layout(
        &mut self,
        view_lines: &[ViewLine],
        cursor: &Cursor,
        gutter_width: usize,
    ) -> bool {
        let render_width = self.width as usize;
        self.ensure_visible_in_layout_with_render_width(
            view_lines,
            cursor,
            render_width,
            gutter_width,
        )
    }

    pub(crate) fn ensure_visible_in_layout_with_render_width(
        &mut self,
        view_lines: &[ViewLine],
        cursor: &Cursor,
        render_width: usize,
        gutter_width: usize,
    ) -> bool {
        self.left_column =
            self.layout_column_scroll(view_lines, cursor, render_width, gutter_width);
        // The pure form above reads the resize-sync flag; this, the writing
        // form, consumes it as the layout pass always has.
        let _ = self.should_skip_resize_sync();
        false
    }

    /// The horizontal scroll this frame's rows call for — the pure form of
    /// [`Self::ensure_visible_in_layout`].
    ///
    /// Returns the `left_column` the frame should draw with, from the rows
    /// that were built: the cursor's visual column needs the row's own
    /// char→column map (tabs, wide characters, spliced inlay hints), so this
    /// is the one placement decision that cannot move ahead of the build.
    /// Nothing is written; the pane's paint stores the value afterwards, with
    /// [`Self::should_skip_resize_sync`] consumed then, so the frame paints
    /// what the writing form used to paint.
    pub(crate) fn layout_column_scroll(
        &self,
        view_lines: &[ViewLine],
        cursor: &Cursor,
        render_width: usize,
        gutter_width: usize,
    ) -> usize {
        // A restored session keeps its scroll position for one frame, and a
        // scroll action keeps its own (Ctrl+Up/Down); neither is undone here.
        if self.skip_resize_sync || self.skip_ensure_visible {
            tracing::trace!("layout_column_scroll: SKIPPING (skip flag set)");
            return self.left_column;
        }

        let viewport_height = self.visible_line_count();
        if view_lines.is_empty() || viewport_height == 0 {
            tracing::trace!(
                "layout_column_scroll: early-out, view_lines.len={} viewport_height={} cursor_pos={} top_byte={}",
                view_lines.len(),
                viewport_height,
                cursor.position,
                self.top_byte(),
            );
            return self.left_column;
        }

        // Find the cursor's absolute view line position (in the full view_lines array)
        let cursor_view_line = self.find_view_line_for_byte(view_lines, cursor.position);

        tracing::trace!(
            "layout_column_scroll: cursor_pos={} cursor_view_line={} top_view_line_offset={} top_byte={} viewport_height={} view_lines.len={} line_wrap_enabled={}",
            cursor.position,
            cursor_view_line,
            self.top_view_line_offset(),
            self.top_byte(),
            viewport_height,
            view_lines.len(),
            self.line_wrap_enabled,
        );

        // Horizontal only. Vertical placement belongs to
        // `ensure_visible_in_rows`, which runs before anything is built and
        // decides in absolute rows; this pass sees only the window it was
        // handed and cannot reach past it, which is what made the two disagree.
        self.column_scroll_for_cursor_row(
            view_lines,
            cursor,
            cursor_view_line,
            render_width,
            gutter_width,
        )
    }

    /// The horizontal scroll that keeps the cursor's column on screen.
    ///
    /// Returns the current `left_column` unchanged when the cursor is on a
    /// line not present in `view_lines` (e.g. a newly inserted line) —
    /// `ensure_visible` already handled that.
    fn column_scroll_for_cursor_row(
        &self,
        view_lines: &[ViewLine],
        cursor: &Cursor,
        cursor_view_line: usize,
        render_width: usize,
        gutter_width: usize,
    ) -> usize {
        if cursor_view_line >= view_lines.len() {
            return self.left_column;
        }

        let line = &view_lines[cursor_view_line];
        // Byte position of the first character in this line; cursor column is
        // then the visual width from the line start.
        let line_start = line.char_source_bytes.iter().find_map(|m| *m).unwrap_or(0);

        // Byte position where this line ends (start of next line or end of view).
        let line_end_byte = if cursor_view_line + 1 < view_lines.len() {
            // Next line exists, use its start as this line's end.
            view_lines[cursor_view_line + 1]
                .char_source_bytes
                .iter()
                .find_map(|m| *m)
                .unwrap_or(usize::MAX)
        } else {
            // Last view line — the content length (including newline) is the end.
            let content_bytes = line.text.len();
            line_start.saturating_add(content_bytes)
        };

        // Only handle horizontal scroll if the cursor is actually within this line.
        if cursor.position >= line_end_byte {
            return self.left_column;
        }

        // Visual column of the cursor, taken from the canonical
        // char→column map on the ViewLine. This accounts for tab
        // expansion, wide/CJK characters, AND inline inlay-hint
        // cells spliced before wrapping — so horizontal scroll
        // follows the cursor's true on-screen column instead of a
        // hint-blind byte walk. When the cursor sits one past the
        // last source char (end of line), fall back to the line's
        // full visual width.
        let cursor_visual_col = line
            .char_source_bytes
            .iter()
            .position(|b| *b == Some(cursor.position))
            .map(|ci| line.visual_col_at_char(ci))
            .unwrap_or_else(|| line.visual_width());

        // Line width for scroll clamping, excluding the trailing
        // newline cell (width 1) the ViewLine carries.
        let line_visual_width = line
            .visual_width()
            .saturating_sub(usize::from(line.ends_with_newline));
        self.column_visible_simple(
            cursor_visual_col,
            line_visual_width,
            render_width,
            gutter_width,
        )
    }

    /// Renderer/layout-only column visibility check using laid-out line
    /// geometry. Returns the `left_column` that shows `column`.
    fn column_visible_simple(
        &self,
        column: usize,
        line_length: usize,
        render_width: usize,
        gutter_width: usize,
    ) -> usize {
        // Skip if line wrapping is enabled (all columns visible via wrapping)
        // or if this view never scrolls sideways (a widget panel).
        if self.line_wrap_enabled || !self.horizontal_scroll_enabled {
            return 0;
        }

        // `render_width` is the geometry used to build and draw these lines.
        // It may be narrower than this viewport in compose mode, and already
        // accounts for a shown vertical scrollbar column.
        let visible_width = render_width.saturating_sub(gutter_width);

        if visible_width == 0 {
            return self.left_column;
        }

        let effective_offset = self.horizontal_scroll_offset.min(visible_width / 2);
        let ideal_left = self.left_column + effective_offset;
        let ideal_right = self.left_column + visible_width.saturating_sub(effective_offset);

        let mut left_column = self.left_column;
        if column < ideal_left {
            left_column = column.saturating_sub(effective_offset);
        } else if column >= ideal_right {
            let target_position = visible_width
                .saturating_sub(effective_offset)
                .saturating_sub(1);
            left_column = column.saturating_sub(target_position);
        }

        // Limit scroll to line length
        if line_length > 0 {
            let max_left_column = line_length.saturating_sub(visible_width.saturating_sub(1));
            if left_column > max_left_column {
                left_column = max_left_column;
            }
        }
        left_column
    }

    /// Set top_byte with automatic scroll limit enforcement
    /// This prevents scrolling past the end of the buffer by ensuring
    /// the viewport can be filled from the proposed position
    fn set_top_byte_with_limit(
        &mut self,
        buffer: &mut Buffer,
        soft_breaks: &[(usize, u16)],
        virtual_lines: &[usize],
        proposed_top_byte: usize,
    ) {
        tracing::trace!(
            "DEBUG set_top_byte_with_limit: proposed_top_byte={}",
            proposed_top_byte
        );

        let viewport_height = self.visible_line_count();
        if viewport_height == 0 {
            self.set_top_byte(proposed_top_byte);
            return;
        }

        if buffer.is_empty() {
            self.set_top_byte(0);
            return;
        }

        if self.line_wrap_enabled {
            self.clamp_top_byte_wrapped(
                buffer,
                soft_breaks,
                virtual_lines,
                proposed_top_byte,
                viewport_height,
            );
        } else {
            self.clamp_top_byte_unwrapped(buffer, proposed_top_byte, viewport_height);
        }
    }

    /// Wrapped-mode body of [`set_top_byte_with_limit`].
    ///
    /// Counts visual rows (wrapped segments) rather than logical lines, since
    /// each logical line may wrap into several visual rows, then backtracks to
    /// the maximum valid scroll position if `proposed_top_byte` can't fill the
    /// viewport.
    fn clamp_top_byte_wrapped(
        &mut self,
        buffer: &mut Buffer,
        soft_breaks: &[(usize, u16)],
        virtual_lines: &[usize],
        proposed_top_byte: usize,
        viewport_height: usize,
    ) {
        let buffer_version = buffer.version();
        let wrap_config = self.make_wrap_config(buffer);

        let mut iter = buffer.line_iterator(proposed_top_byte, 80);
        let mut visual_rows = 0;

        while let Some((_, rows, line_end)) = Self::next_line_visual_rows_capped(
            &mut iter,
            viewport_height - visual_rows,
            &wrap_config,
            soft_breaks,
            virtual_lines,
            Some((&mut self.wrap_row_cache, buffer_version)),
        ) {
            visual_rows += rows;
            if visual_rows >= viewport_height {
                self.set_top_byte(proposed_top_byte);
                return;
            }
            // Only a saturating count parks the iterator mid-line, and that
            // count would have satisfied the test above.
            debug_assert!(line_end.is_some());
            if line_end.is_none() {
                break;
            }
        }

        if visual_rows >= viewport_height {
            self.set_top_byte(proposed_top_byte);
            return;
        }

        // Not enough visual rows to fill viewport from proposed position.
        // Use find_max_visual_scroll_position which correctly counts wrapped rows.
        let (max_byte, max_offset) = self.find_max_visual_scroll_position(
            buffer,
            soft_breaks,
            virtual_lines,
            &wrap_config,
            viewport_height,
        );
        // Only backtrack if the proposed position is past the maximum
        if proposed_top_byte > max_byte
            || (proposed_top_byte == max_byte && self.top_view_line_offset() > max_offset)
        {
            self.set_top_byte(max_byte);
            self.set_top_view_line_offset(max_offset);
        } else {
            self.set_top_byte(proposed_top_byte);
        }
    }

    /// Non-wrapped body of [`set_top_byte_with_limit`].
    ///
    /// Counts logical lines from `proposed_top_byte`; if there aren't enough to
    /// fill the viewport, backtracks line-by-line so the last line still rests
    /// at the bottom.
    fn clamp_top_byte_unwrapped(
        &mut self,
        buffer: &mut Buffer,
        proposed_top_byte: usize,
        viewport_height: usize,
    ) {
        let mut iter = buffer.line_iterator(proposed_top_byte, 80);
        let mut lines_visible = 0;

        while iter.next_line().is_some() {
            lines_visible += 1;
            if lines_visible >= viewport_height {
                // We have a full viewport of content, use proposed position
                tracing::trace!(
                    "DEBUG: Full viewport available, setting top_byte={}",
                    proposed_top_byte
                );
                self.set_top_byte(proposed_top_byte);
                return;
            }
        }

        tracing::trace!(
            "DEBUG: After iteration, lines_visible={}, viewport_height={}",
            lines_visible,
            viewport_height
        );

        // If we have enough lines to fill the viewport, we're good
        if lines_visible >= viewport_height {
            tracing::trace!(
                "DEBUG: Enough lines to fill viewport, setting top_byte={}",
                proposed_top_byte
            );
            self.set_top_byte(proposed_top_byte);
            return;
        }

        // We don't have enough lines to fill the viewport from proposed_top_byte
        // Calculate how many lines we're short and scroll back
        let lines_short = viewport_height - lines_visible;
        tracing::trace!("DEBUG: lines_short={}, scrolling back", lines_short);

        let mut backtrack_iter = buffer.line_iterator(proposed_top_byte, 80);
        tracing::trace!(
            "DEBUG: Backtracking from byte {}",
            backtrack_iter.current_position()
        );
        for i in 0..lines_short {
            let pos_before = backtrack_iter.current_position();
            if backtrack_iter.prev().is_none() {
                tracing::trace!(
                    "DEBUG: Hit beginning of buffer at backtrack iteration {}",
                    i
                );
                break; // Hit the beginning of the buffer
            }
            let pos_after = backtrack_iter.current_position();
            tracing::trace!(
                "DEBUG: Backtrack iteration {}: {} -> {}",
                i,
                pos_before,
                pos_after
            );
        }

        let final_top_byte = backtrack_iter.current_position();
        tracing::trace!(
            "DEBUG: After backtracking, setting top_byte={}",
            final_top_byte
        );
        self.set_top_byte(final_top_byte);
    }

    /// Scroll to a specific line (byte-based)
    /// This seeks from the beginning to find the byte position of the line
    pub fn scroll_to(&mut self, buffer: &mut Buffer, line: usize) {
        // Seek from the beginning to find the byte position for this line
        let mut iter = buffer.line_iterator(0, 80);
        let mut current_line = 0;

        while current_line < line {
            if let Some((line_start, _)) = iter.next_line() {
                if current_line + 1 == line {
                    // Soft breaks unknown here (called from cursor flows that
                    // don't have ready access to the buffer's marker state).
                    // Pass empty: limit calc will use width-only wrap_line.
                    self.set_top_byte_with_limit(buffer, &[], &[], line_start);
                    return;
                }
                current_line += 1;
            } else {
                // Reached end of buffer before target line
                break;
            }
        }

        // If we didn't find the line, stay at the last valid position
        let target_position = iter.current_position();
        self.set_top_byte_with_limit(buffer, &[], &[], target_position);
    }

    /// Scroll so the document's last row sits at the bottom of the viewport,
    /// decided in absolute rows from the wrap index — the row-space form of
    /// [`Self::scroll_to_end_of_view`], which needed the rows built to count
    /// them. Same clamp as [`Self::scroll_visual_rows`]: the last full page.
    /// Returns `true` if the viewport moved.
    pub fn scroll_to_end_in_rows(
        &mut self,
        index: &crate::view::wrap_index::WrapIndex,
        buffer: &Buffer,
    ) -> bool {
        let viewport_height = self.visible_line_count();
        if viewport_height == 0 {
            return false;
        }
        let top_line = buffer.get_line_number(self.top_byte());
        let top_row = index.line_first_row(top_line) as usize + self.top_view_line_offset();
        let max_top = (index.total_rows() as usize).saturating_sub(viewport_height);
        if top_row == max_top {
            return false;
        }
        let addr = index.byte_of_row(buffer, max_top as u32);
        self.set_top_byte(buffer.line_start_offset(addr.line).unwrap_or(0));
        self.set_top_view_line_offset(addr.row_in_line);
        true
    }

    /// [`Self::scroll_to_end_in_rows`] for a buffer with no wrap index (one
    /// beyond the index's size ceilings): propose the end of the buffer as
    /// the top and let the scroll limit back it up to the last full page,
    /// the way a wheel scroll to the bottom lands. Byte-based, so the walk
    /// is bounded by one screen of lines.
    pub fn scroll_to_end_unindexed(&mut self, buffer: &mut Buffer) {
        self.set_top_view_line_offset(0);
        let end = buffer.len();
        self.set_top_byte_with_limit(buffer, &[], &[], end);
    }

    /// Mark viewport as needing synchronization with cursor positions
    /// This defers the actual viewport update until sync_with_cursor is called
    pub fn mark_needs_sync(&mut self) {
        self.needs_sync = true;
    }

    /// Check if viewport needs synchronization
    pub fn needs_sync(&self) -> bool {
        self.needs_sync
    }

    /// Synchronize viewport with cursor position (deferred ensure_visible)
    /// This should be called before rendering to batch multiple cursor movements
    pub fn sync_with_cursor(&mut self, buffer: &mut Buffer, cursor: &Cursor) {
        if self.needs_sync {
            self.ensure_visible(buffer, cursor, &[]);
            self.needs_sync = false;
        }
    }

    /// Low-level: ensure cursor is visible, scrolling if necessary.
    ///
    /// Callers should prefer [`BufferViewState::ensure_cursor_visible`] which
    /// automatically resolves fold ranges from the marker list. Use this
    /// directly only from the rendering pipeline where fold ranges are already
    /// resolved, or from unit tests (pass `&[]` for `hidden_ranges`).
    ///
    /// `hidden_ranges` contains `(start_byte, end_byte)` pairs for collapsed
    /// fold regions so that line counting skips hidden lines.
    pub(crate) fn ensure_visible(
        &mut self,
        buffer: &mut Buffer,
        cursor: &Cursor,
        hidden_ranges: &[(usize, usize)],
    ) {
        self.ensure_visible_with_virtual(buffer, cursor, hidden_ranges, 0);
    }

    /// [`ensure_visible`](Self::ensure_visible) for a cursor sitting
    /// `virtual_columns` past its line's content end (virtual space): the
    /// horizontal scroll targets the cursor's on-screen column, not its
    /// clipped byte column. The render path passes the real value; other
    /// callers pass 0 and are corrected by the per-frame render sync.
    pub(crate) fn ensure_visible_with_virtual(
        &mut self,
        buffer: &mut Buffer,
        cursor: &Cursor,
        hidden_ranges: &[(usize, usize)],
        virtual_columns: usize,
    ) {
        let _span = tracing::trace_span!(
            "ensure_visible",
            cursor_pos = cursor.position,
            top_byte = self.top_byte(),
        )
        .entered();

        // When `top_view_line_offset > 0` the byte-oriented visibility math
        // undercounts because it measures from `top_byte` rather than the
        // actual visible top. Defer to `ensure_visible_in_layout` unless the
        // cursor is so far below that the layout-aware path can't reach it
        // either (issue #1574 / #1689 follow-up).
        //
        // Only when there is a row pass to defer to. With no wrap index this
        // pass is the only vertical authority, so deferring means nobody
        // scrolls at all once a top is parked inside a line (issue #1806).
        // `check_wrapped_visibility` measures from the visible top, so the
        // undercount this guards against is not there to guard.
        if self.row_pass_owns_placement
            && self.top_view_line_offset() > 0
            && cursor.position >= self.top_byte()
        {
            let top_line = buffer.get_line_number(self.top_byte());
            let cursor_line = buffer.get_line_number(cursor.position);
            let viewport_height = self.visible_line_count().max(1);
            if cursor_line < top_line.saturating_add(viewport_height.saturating_mul(2)) {
                return;
            }
        }

        if self.should_skip_resize_sync() {
            tracing::trace!("ensure_visible: SKIPPING due to skip_resize_sync");
            return;
        }
        if self.should_skip_ensure_visible() {
            tracing::trace!("ensure_visible: SKIPPING due to skip_ensure_visible flag");
            return;
        }
        tracing::trace!(
            "ensure_visible: NOT skipping, skip_ensure_visible={}",
            self.skip_ensure_visible
        );

        let viewport_lines = self.visible_line_count().max(1);
        tracing::trace!(
            "ensure_visible: cursor={}, top_byte={}, viewport_lines={}, line_wrap={}",
            cursor.position,
            self.top_byte(),
            viewport_lines,
            self.line_wrap_enabled
        );

        self.load_data_around_cursor(buffer, cursor.position, viewport_lines);

        // No index covers this buffer, so placement is a bounded walk from the
        // anchor, and this branch owns the whole of it. The gate implies wrap is
        // on, so the horizontal half of the tail below is its `left_column = 0`;
        // do that here rather than falling through, since the vertical half
        // would then re-place the viewport by row.
        if !self.row_pass_owns_placement
            && crate::view::row_walk::addresses_rows_by_byte(buffer, self.line_wrap_enabled)
        {
            self.ensure_visible_anchored(buffer, cursor, hidden_ranges);
            self.left_column = 0;
            return;
        }

        let cursor_line_start = buffer.line_iterator(cursor.position, 80).current_position();
        let effective_offset = self.scroll_offset.min(viewport_lines / 2);

        let (cursor_is_visible, cursor_near_top) = if self.row_pass_owns_placement {
            // Vertical placement belongs to the row pass; claiming the cursor
            // is visible short-circuits every scroll below while the
            // horizontal handling further down still runs.
            (true, false)
        } else if cursor_line_start < self.top_byte() {
            (false, true)
        } else if self.line_wrap_enabled {
            self.check_wrapped_visibility(
                buffer,
                cursor,
                cursor_line_start,
                viewport_lines,
                effective_offset,
                hidden_ranges,
            )
        } else {
            self.check_nowrap_visibility(
                buffer,
                cursor_line_start,
                viewport_lines,
                effective_offset,
                hidden_ranges,
            )
        };

        tracing::trace!(
            "ensure_visible: cursor_line_start={}, cursor_is_visible={}",
            cursor_line_start,
            cursor_is_visible
        );

        if !cursor_is_visible {
            let _span =
                tracing::trace_span!("ensure_visible_scroll", cursor_near_top, cursor_line_start,)
                    .entered();
            if self.line_wrap_enabled {
                // The wrapped backward scan starts visual_rows_counted at 1+
                // (cursor's own row), so the target is 1 more than the no-wrap case.
                //
                // TODO: this backward walk calls `layout_for_plain_text` directly,
                // bypassing both `LineWrapCache` and the `WrapIndex`.
                // Migrating requires threading `&mut EditorState` through
                // `ensure_visible` and its 6 call sites; left as a follow-up since
                // folds force a fallback path here anyway.
                self.scroll_to_cursor_wrapped(
                    buffer,
                    cursor,
                    cursor_line_start,
                    effective_offset,
                    cursor_near_top,
                    hidden_ranges,
                );
            } else {
                let target_rows_from_top = if cursor_near_top {
                    effective_offset
                } else {
                    viewport_lines.saturating_sub(effective_offset + 1)
                };
                self.scroll_to_cursor_nowrap(
                    buffer,
                    cursor_line_start,
                    target_rows_from_top,
                    hidden_ranges,
                );
            }
        }

        // Horizontal scrolling (disabled when wrapping — all columns visible via wrap).
        if !self.line_wrap_enabled {
            let cursor_column = cursor.position.saturating_sub(cursor_line_start) + virtual_columns;
            let mut line_iter = buffer.line_iterator(cursor_line_start, 80);
            let line_length = if let Some((_, content)) = line_iter.next_line() {
                content.trim_end_matches('\n').len()
            } else {
                0
            };
            // In virtual space the cursor column exceeds the line length;
            // widen the scroll limit so the viewport can follow it.
            let line_length = line_length.max(cursor_column);
            self.ensure_column_visible(cursor_column, line_length, buffer);
        } else {
            self.left_column = 0;
        }
    }

    /// Force-load bytes around `cursor_pos` so that line iterators won't hit
    /// unloaded segments in large lazy-loaded files.
    fn load_data_around_cursor(
        &mut self,
        buffer: &mut Buffer,
        cursor_pos: usize,
        viewport_lines: usize,
    ) {
        let estimated_viewport_bytes = viewport_lines * 200;
        let load_start = cursor_pos.saturating_sub(estimated_viewport_bytes * 2);
        let remaining_bytes = buffer.len().saturating_sub(load_start);
        let load_length = (estimated_viewport_bytes * 3).min(remaining_bytes);
        let _span = tracing::trace_span!("ensure_visible_load", load_start, load_length).entered();
        if let Err(e) = buffer.get_text_range_mut(load_start, load_length) {
            tracing::warn!("Failed to load data around cursor at {}: {}", cursor_pos, e);
        }
    }

    /// Build the `WrapConfig` used by visibility and scroll helpers.
    /// In terminal-grid mode (fresh#2649) this is the exact-column grid
    /// config at `wrap_column` columns; every scroll/visibility helper
    /// funnels through here so they all share one row model.
    fn make_wrap_config(&self, buffer: &mut Buffer) -> WrapConfig {
        if self.grid_wrap {
            return WrapConfig::grid(self.grid_cols());
        }
        let gutter_width = self.gutter_width(buffer);
        WrapConfig::new(
            self.effective_width() as usize,
            gutter_width,
            true,
            self.wrap_indent,
        )
    }

    /// Grid-wrap column count: the capture-time terminal width stored in
    /// `wrap_column`, falling back to the viewport width.
    pub(crate) fn grid_cols(&self) -> usize {
        self.wrap_column.unwrap_or(self.width as usize).max(1)
    }

    /// Compute the line-wrap layout for `line_text` using `wrap_config`.
    /// How `line_text` wraps, as the rows the renderer would draw for it.
    ///
    /// One run: row boundaries inside a line come from the wrap alone.
    ///
    /// The token build used to inject a `Break` every `MAX_SAFE_LINE_WIDTH`
    /// characters counted from wherever its read began, so the rows a line fell
    /// into depended on where it had been read from and no two readers agreed.
    /// Nothing splits a line into pieces here now; the width bound is the wrap
    /// width itself, which with wrap off is `MAX_SAFE_LINE_WIDTH` — see
    /// `view_data::effective_wrap_width`.
    fn compute_line_layout(
        line_text: &str,
        wrap_config: &WrapConfig,
    ) -> Vec<crate::view::ui::view_pipeline::ViewLine> {
        if let Some(cols) = wrap_config.grid_cols {
            return crate::view::line_wrap_cache::layout_for_plain_text_grid(line_text, cols, 4);
        }
        let effective_width = wrap_config
            .first_line_width
            .saturating_add(wrap_config.gutter_width)
            .max(2);
        crate::view::line_wrap_cache::layout_for_plain_text(
            line_text,
            effective_width,
            wrap_config.gutter_width,
            wrap_config.hanging_indent,
            4,
        )
    }

    /// The wrap rule this viewport's rows are decided by — the same rule the
    /// renderer hands the wrap machine, so a walk over rows here and the rows
    /// drawn there are the same rows.
    pub(crate) fn wrap_rule(&self, buffer: &mut Buffer) -> crate::view::wrap_machine::WrapRule {
        use crate::view::wrap_machine::WrapRule;
        if self.grid_wrap {
            return WrapRule::Grid {
                cols: self.grid_cols().max(1),
            };
        }
        let config = self.make_wrap_config(buffer);
        WrapRule::Word {
            content_width: config
                .first_line_width
                .saturating_add(config.gutter_width)
                .max(2),
            gutter_width: config.gutter_width,
            hanging_indent: config.hanging_indent,
        }
    }

    /// Place the viewport for a buffer whose rows are addressed by byte.
    ///
    /// The anchor is the first visible row, so "is the cursor on screen" is a
    /// walk of at most a screenful and moving the view is choosing a different
    /// row start. Nothing counts rows from the logical line's start — a count
    /// that could not be taken past the first `MAX_LINE_BYTES` the reader
    /// returns, so it saturated and the view stopped following (issue #1806).
    /// The viewport's `hidden_ranges` in the shape the token build wants.
    ///
    /// `reconcile` derives both from the same `folds.resolved_ranges()`; this is
    /// the one place the two shapes meet, so a walk and the frame it is placing
    /// skip the same bytes.
    fn fold_skip(hidden_ranges: &[(usize, usize)]) -> Vec<std::ops::Range<usize>> {
        let mut ranges: Vec<std::ops::Range<usize>> =
            hidden_ranges.iter().map(|(s, e)| *s..*e).collect();
        ranges.sort_by_key(|r| r.start);
        ranges
    }

    /// Largest top an anchored viewport may take: the row start that still
    /// leaves a screenful below it.
    ///
    /// The row-numbered path gets this from `set_top_byte_with_limit` /
    /// `apply_visual_scroll_limit`, which an anchored top cannot use — they
    /// clamp a *line* start plus a row offset. Without it a wheel roll at EOF
    /// walks the top on until the buffer's last row is the first drawn row and
    /// the rest of the screen is empty.
    fn max_anchored_top(
        &self,
        buffer: &mut Buffer,
        rule: crate::view::wrap_machine::WrapRule,
        folds: &[std::ops::Range<usize>],
    ) -> usize {
        let height = self.visible_line_count();
        if height <= 1 {
            return buffer.len();
        }
        crate::view::row_walk::row_start_before(
            buffer,
            buffer.len(),
            height.saturating_sub(1),
            rule,
            folds,
        )
    }

    /// [`Self::set_top_byte`] for an anchored viewport, held to
    /// [`Self::max_anchored_top`].
    fn set_anchored_top(
        &mut self,
        buffer: &mut Buffer,
        rule: crate::view::wrap_machine::WrapRule,
        folds: &[std::ops::Range<usize>],
        proposed: usize,
    ) {
        let capped = proposed.min(self.max_anchored_top(buffer, rule, folds));
        self.set_top_byte(capped);
        // An anchored viewport has no second coordinate to reconcile.
        self.set_top_view_line_offset(0);
    }

    fn ensure_visible_anchored(
        &mut self,
        buffer: &mut Buffer,
        cursor: &Cursor,
        hidden_ranges: &[(usize, usize)],
    ) {
        use crate::view::row_walk;

        let height = self.visible_line_count().max(1);
        let margin = self.scroll_offset.min((height.saturating_sub(1)) / 2);
        let rule = self.wrap_rule(buffer);
        let folds = Self::fold_skip(hidden_ranges);
        let top = self.top_byte();

        // An anchored viewport has no second coordinate to reconcile. Reset
        // before deciding: every branch below writes the top through
        // `set_anchored_top`, which sets it again, and a viewport arriving here
        // with a stale offset would otherwise read as that many rows lower.
        self.set_top_view_line_offset(0);

        if cursor.position < top {
            // Above the window: the cursor's row becomes the margin row.
            let cursor_row_start =
                row_walk::row_start_before(buffer, cursor.position, 0, rule, &folds);
            let new_top =
                row_walk::row_start_before(buffer, cursor_row_start, margin, rule, &folds);
            self.set_anchored_top(buffer, rule, &folds, new_top);
            return;
        }

        // Rows from the top down to the cursor, looking one screen ahead.
        let last_row = height.saturating_sub(1);
        if let Some(row) =
            row_walk::rows_between(buffer, top, cursor.position, rule, last_row, &folds)
        {
            if row >= margin && row + margin <= last_row {
                return; // inside the margin band: nothing to do
            }
            if row < margin {
                let new_top = row_walk::row_start_before(buffer, top, margin - row, rule, &folds);
                self.set_anchored_top(buffer, rule, &folds, new_top);
                return;
            }
            // Below the bottom margin but on screen: move down by the shortfall.
            let shortfall = row + margin - last_row;
            let new_top = row_walk::row_start_after(buffer, top, rule, shortfall, &folds);
            self.set_anchored_top(buffer, rule, &folds, new_top);
            return;
        }

        // Further below than a screen: put the cursor on the bottom margin row.
        let cursor_row_start = row_walk::row_start_before(buffer, cursor.position, 0, rule, &folds);
        let rows_above = last_row.saturating_sub(margin);
        let new_top =
            row_walk::row_start_before(buffer, cursor_row_start, rows_above, rule, &folds);
        self.set_anchored_top(buffer, rule, &folds, new_top);
    }

    /// Return `(is_visible, cursor_near_top)` for wrap mode.
    ///
    /// Counts visual rows from `top_byte` toward the cursor; a cursor at the
    /// edge of the scroll margin is considered not visible so that the margin
    /// invariant is maintained.
    fn check_wrapped_visibility(
        &self,
        buffer: &mut Buffer,
        cursor: &Cursor,
        cursor_line_start: usize,
        viewport_lines: usize,
        effective_offset: usize,
        hidden_ranges: &[(usize, usize)],
    ) -> (bool, bool) {
        let wrap_config = self.make_wrap_config(buffer);
        let mut iter = buffer.line_iterator(self.top_byte(), 80);
        let mut visual_rows: usize = 0;
        let mut cursor_near_top = false;
        // Rows of the top line scrolled off above the screen. The walk below
        // counts from `top_byte` — the line's *start* — so without this the
        // cursor reads as that many rows higher than it is drawn.
        let hidden_above = self.top_view_line_offset();

        loop {
            let current_pos = iter.current_position();

            if current_pos >= cursor_line_start {
                if current_pos != cursor_line_start {
                    // Overshot — shouldn't happen in practice.
                    return (false, false);
                }
                let line_content = iter
                    .next_line()
                    .map(|(_, c)| c.trim_end_matches(['\n', '\r']).to_string())
                    .unwrap_or_default();
                let layout = Self::compute_line_layout(&line_content, &wrap_config);
                let segments_count = layout.len().max(1);
                let cursor_column = cursor.position.saturating_sub(cursor_line_start);
                let (cursor_segment_idx, _) =
                    crate::view::line_wrap_cache::byte_position_in_layout(&layout, cursor_column);
                visual_rows += cursor_segment_idx.min(segments_count - 1) + 1;
                let rows_from_top = visual_rows.saturating_sub(hidden_above);

                // rows_from_top is 1-based here; > effective_offset gives the same
                // margin as lines_from_top >= effective_offset in no-wrap mode.
                let vis = rows_from_top > effective_offset
                    && rows_from_top <= viewport_lines.saturating_sub(effective_offset);
                if !vis && rows_from_top <= effective_offset {
                    cursor_near_top = true;
                }
                return (vis, cursor_near_top);
            }

            // Skip a complete hidden fold region at once.
            if let Some((_, end)) = Self::containing_hidden_range(hidden_ranges, current_pos) {
                while iter.current_position() < end && iter.current_position() < cursor_line_start {
                    if iter.next_line().is_none() {
                        break;
                    }
                }
                continue;
            }

            if let Some((_, line_content)) = iter.next_line() {
                let layout =
                    Self::compute_line_layout(line_content.trim_end_matches('\n'), &wrap_config);
                visual_rows += layout.len();
                if visual_rows.saturating_sub(hidden_above) >= viewport_lines {
                    return (false, false);
                }
            } else {
                return (false, false);
            }
        }
    }

    /// Return `(is_visible, cursor_near_top)` for no-wrap mode.
    fn check_nowrap_visibility(
        &self,
        buffer: &mut Buffer,
        cursor_line_start: usize,
        viewport_lines: usize,
        effective_offset: usize,
        hidden_ranges: &[(usize, usize)],
    ) -> (bool, bool) {
        let mut iter = buffer.line_iterator(self.top_byte(), 80);
        let mut lines_from_top: usize = 0;

        while iter.current_position() < cursor_line_start && lines_from_top < viewport_lines {
            let pos = iter.current_position();
            if let Some((_, end)) = Self::containing_hidden_range(hidden_ranges, pos) {
                while iter.current_position() < end && iter.current_position() < cursor_line_start {
                    if iter.next_line().is_none() {
                        break;
                    }
                }
                continue;
            }
            if iter.next_line().is_none() {
                break;
            }
            lines_from_top += 1;
        }

        let cursor_near_top = lines_from_top < effective_offset;
        let visible = lines_from_top >= effective_offset
            && lines_from_top < viewport_lines.saturating_sub(effective_offset);
        tracing::trace!(
            "ensure_visible (no wrap): lines_from_top={}, effective_offset={}, visible={}",
            lines_from_top,
            effective_offset,
            visible
        );
        (visible, cursor_near_top)
    }

    /// Scroll `top_byte` / `top_view_line_offset` so the cursor lands inside
    /// the scroll margin (wrap mode). `effective_offset` is the margin depth.
    fn scroll_to_cursor_wrapped(
        &mut self,
        buffer: &mut Buffer,
        cursor: &Cursor,
        cursor_line_start: usize,
        effective_offset: usize,
        cursor_near_top: bool,
        hidden_ranges: &[(usize, usize)],
    ) {
        let viewport_lines = self.visible_line_count().max(1);
        let target_visual_rows = if cursor_near_top {
            effective_offset + 1
        } else {
            viewport_lines.saturating_sub(effective_offset)
        };
        let wrap_config = self.make_wrap_config(buffer);
        let mut iter = buffer.line_iterator(cursor_line_start, 80);
        let mut visual_rows_counted: usize = 0;
        let mut cursor_segment_idx_in_line: usize = 0;

        // Count rows from the cursor's own line up to the cursor position.
        if let Some((_, line_content)) = iter.next_line() {
            let line_text = line_content.trim_end_matches('\n');
            let layout = Self::compute_line_layout(line_text, &wrap_config);
            let cursor_column = cursor.position.saturating_sub(cursor_line_start);
            let (cursor_segment_idx, _) =
                crate::view::line_wrap_cache::byte_position_in_layout(&layout, cursor_column);
            cursor_segment_idx_in_line = cursor_segment_idx;
            visual_rows_counted += cursor_segment_idx + 1;
        } else {
            // EOF after trailing newline — empty logical line needs 1 row.
            visual_rows_counted += 1;
        }

        // Fast path: the cursor's own line has enough wrap segments above the
        // cursor to satisfy the scroll margin. Stay on this line and adjust
        // `top_view_line_offset` instead of walking further back. Without
        // this, Up-arrow onto the last row of a long wrapped paragraph would
        // teleport the cursor many rows down (issue #1574, step 16).
        if cursor_near_top && visual_rows_counted >= target_visual_rows {
            self.set_top_byte_with_limit(buffer, &[], &[], cursor_line_start);
            self.set_top_view_line_offset(
                cursor_segment_idx_in_line.saturating_sub(effective_offset),
            );
            return;
        }

        // The new top is a row *inside* the cursor's line. The backward walk
        // below can only land on a line start, which is enough while an earlier
        // line supplies the rows above the cursor; when the cursor's own line
        // supplies them all it has nowhere to go and leaves the top put — every
        // scroll, on a one-line file (issue #1806). `scroll_down_visual`, which
        // `PageDown` runs, has always parked the top inside a line this way.
        if !cursor_near_top && visual_rows_counted >= target_visual_rows {
            self.set_top_byte_with_limit(buffer, &[], &[], cursor_line_start);
            self.set_top_view_line_offset(visual_rows_counted - target_visual_rows);
            return;
        }

        // Walk backward counting visual rows until we accumulate target_visual_rows.
        // When scrolling UP and the walk overshoots, set `top_view_line_offset`
        // within the landing line so the cursor ends up at exactly
        // `effective_offset` rows from the new top (issue #1574, step 16).
        // This is intentionally not done for scroll-DOWN — that path relies on
        // landing at line start (`top_view_line_offset = 0`).
        iter = buffer.line_iterator(cursor_line_start, 80);
        let mut top_offset_in_landing_line: usize = 0;

        while visual_rows_counted < target_visual_rows {
            if iter.prev().is_none() {
                break;
            }
            // Skip hidden fold regions backward.
            while let Some((start, _)) =
                Self::containing_hidden_range(hidden_ranges, iter.current_position())
            {
                while iter.current_position() >= start {
                    if iter.prev().is_none() {
                        break;
                    }
                }
            }
            if let Some((_, line_content)) = iter.next_line() {
                let line_text = line_content.trim_end_matches('\n');
                let layout = Self::compute_line_layout(line_text, &wrap_config);
                let added = layout.len().max(1);
                let new_total = visual_rows_counted + added;
                if cursor_near_top && new_total >= target_visual_rows {
                    let rows_from_this_line =
                        target_visual_rows.saturating_sub(visual_rows_counted);
                    top_offset_in_landing_line = added.saturating_sub(rows_from_this_line);
                    iter.prev();
                    break;
                }
                visual_rows_counted = new_total;
                iter.prev();
            }
        }

        let new_top_byte = iter.current_position();
        self.set_top_byte_with_limit(buffer, &[], &[], new_top_byte);
        self.set_top_view_line_offset(top_offset_in_landing_line);
        if cursor_near_top {}
    }

    /// Scroll `top_byte` so the cursor lands at `target_rows_from_top` logical
    /// lines from the new viewport top (no-wrap mode).
    fn scroll_to_cursor_nowrap(
        &mut self,
        buffer: &mut Buffer,
        cursor_line_start: usize,
        target_rows_from_top: usize,
        hidden_ranges: &[(usize, usize)],
    ) {
        let mut iter = buffer.line_iterator(cursor_line_start, 80);
        let mut visible_counted: usize = 0;

        while visible_counted < target_rows_from_top {
            if iter.prev().is_none() {
                break;
            }
            // Skip hidden fold regions backward.
            while let Some((start, _)) =
                Self::containing_hidden_range(hidden_ranges, iter.current_position())
            {
                while iter.current_position() >= start {
                    if iter.prev().is_none() {
                        break;
                    }
                }
            }
            visible_counted += 1;
        }

        let new_top_byte = iter.current_position();
        self.set_top_byte_with_limit(buffer, &[], &[], new_top_byte);
        self.set_top_view_line_offset(0);
    }

    /// Ensure a line is visible with scroll offset applied
    /// This is a legacy method kept for backward compatibility with tests
    /// In practice, use ensure_visible() which works directly with cursors and bytes
    pub fn ensure_line_visible(&mut self, buffer: &mut Buffer, line: usize) {
        // Seek to the target line to get its byte position
        let mut seek_iter = buffer.line_iterator(0, 80);
        let mut current_line = 0;
        let mut target_line_byte = 0;

        while current_line < line {
            if let Some((line_start, _)) = seek_iter.next_line() {
                if current_line + 1 == line {
                    target_line_byte = line_start;
                    break;
                }
                current_line += 1;
            } else {
                // Reached end of buffer before target line
                return;
            }
        }

        // Check if the line is already visible by iterating from top_byte
        let visible_count = self.visible_line_count();
        let mut iter = buffer.line_iterator(self.top_byte(), 80);
        let mut lines_from_top = 0;
        let mut target_is_visible = false;

        while let Some((line_byte, _)) = iter.next_line() {
            if line_byte == target_line_byte {
                target_is_visible = lines_from_top < visible_count;
                break;
            }
            lines_from_top += 1;
            if lines_from_top >= visible_count {
                break;
            }
        }

        // If not visible, scroll to show it with scroll offset
        if !target_is_visible {
            let effective_offset = self.scroll_offset.min(visible_count / 2);
            let target_line_from_top = effective_offset;

            // Move backwards from target to find new top_byte
            let mut iter = buffer.line_iterator(target_line_byte, 80);
            for _ in 0..target_line_from_top {
                if iter.prev().is_none() {
                    break;
                }
            }
            let position = iter.current_position();
            // Cursor-positioning flow: no soft-break info available here.
            self.set_top_byte_with_limit(buffer, &[], &[], position);
        }
    }

    /// Ensure a column is visible with horizontal scroll offset applied
    ///
    /// # Arguments
    /// * `column` - The column position within the line (0-indexed)
    /// * `line_length` - The length of the line content (without newline)
    /// * `buffer` - The buffer (for calculating gutter width)
    pub fn ensure_column_visible(
        &mut self,
        column: usize,
        line_length: usize,
        buffer: &mut Buffer,
    ) {
        // A view that never scrolls sideways (a widget panel) stays pinned
        // at column 0 whatever the cursor does.
        if !self.horizontal_scroll_enabled {
            self.left_column = 0;
            return;
        }
        // `self.width` is the content width; split layout has already removed
        // any vertical scrollbar column.
        let gutter_width = self.gutter_width(buffer);
        let visible_width = (self.width as usize).saturating_sub(gutter_width);

        if visible_width == 0 {
            return; // Terminal too narrow
        }

        // If viewport is too small for scroll offset, use what we can
        let effective_offset = self.horizontal_scroll_offset.min(visible_width / 2);

        // Calculate the ideal left and right boundaries with scroll offset
        let ideal_left = self.left_column + effective_offset;
        let ideal_right = self.left_column + visible_width.saturating_sub(effective_offset);

        if column < ideal_left {
            // Cursor is to the left of the ideal zone - scroll left
            self.left_column = column.saturating_sub(effective_offset);
        } else if column >= ideal_right {
            // Cursor is to the right of the ideal zone - scroll right
            // Place cursor at (visible_width - effective_offset - 1) to keep it in valid range [0, visible_width-1]
            let target_position = visible_width
                .saturating_sub(effective_offset)
                .saturating_sub(1);
            self.left_column = column.saturating_sub(target_position);
        }

        // BUGFIX: Limit left_column to ensure content is always visible
        // Don't scroll past the point where the end of the line would be off-screen to the left
        // This prevents the viewport from scrolling into "empty space" past the line content
        if line_length > 0 {
            // Calculate the maximum left_column that still shows some content
            // Account for cursor potentially being one position past the line content (at position line_length)
            // If the line is shorter than visible width, left_column should be 0
            // Otherwise, allow scrolling enough to show position line_length at the last visible column
            let max_left_column = line_length.saturating_sub(visible_width.saturating_sub(1));

            // Limit left_column to max_left_column
            if self.left_column > max_left_column {
                self.left_column = max_left_column;
            }
        }
    }

    /// Ensure multiple cursors are visible (smart scroll for multi-cursor)
    /// Prioritizes keeping the primary cursor visible
    pub fn ensure_cursors_visible(
        &mut self,
        buffer: &mut Buffer,
        cursors: &[(usize, &Cursor)], // (priority, cursor) - lower priority number = higher priority
    ) {
        if cursors.is_empty() {
            return;
        }

        // Sort cursors by priority (primary cursor first)
        let mut sorted_cursors: Vec<_> = cursors.to_vec();
        sorted_cursors.sort_by_key(|(priority, _)| *priority);

        // Get byte positions for all cursors (at line starts)
        let cursor_line_bytes: Vec<usize> = sorted_cursors
            .iter()
            .map(|(_, cursor)| {
                let iter = buffer.line_iterator(cursor.position, 80);
                iter.current_position()
            })
            .collect();

        // Count how many lines span between min and max cursors
        let min_byte = *cursor_line_bytes.iter().min().unwrap();
        let max_byte = *cursor_line_bytes.iter().max().unwrap();

        // Count lines between min and max using iterator
        let mut iter = buffer.line_iterator(min_byte, 80);
        let mut line_span = 0;
        while let Some((line_byte, _)) = iter.next_line() {
            if line_byte >= max_byte {
                break;
            }
            line_span += 1;
        }

        let visible_count = self.visible_line_count();

        // If all cursors fit in the viewport, center them
        if line_span < visible_count {
            let lines_to_go_back = visible_count / 2;
            let mut iter = buffer.line_iterator(min_byte, 80);
            for _ in 0..lines_to_go_back {
                if iter.prev().is_none() {
                    break;
                }
            }
            let position = iter.current_position();
            // Cursor-positioning flow: no soft-break info available here.
            self.set_top_byte_with_limit(buffer, &[], &[], position);
        } else {
            // Can't fit all cursors, ensure primary is visible
            let primary_cursor = sorted_cursors[0].1;
            self.ensure_visible(buffer, primary_cursor, &[]);
        }
    }

    /// Get the cursor screen position (x, y) which is (col, row) for rendering
    /// This returns the position relative to the viewport, accounting for horizontal scrolling
    ///
    /// NOTE: This function is kept for popup positioning and multi-cursor display,
    /// but is NO LONGER used for primary cursor rendering, which now happens during
    /// the line rendering loop in split_rendering.rs to eliminate duplicate line iteration.
    pub fn cursor_screen_position(&self, buffer: &mut Buffer, cursor: &Cursor) -> (u16, u16) {
        // Find line start using iterator
        let cursor_iter = buffer.line_iterator(cursor.position, 80);
        let line_start = cursor_iter.current_position();
        let column = cursor.position.saturating_sub(line_start);

        // Wrap config used for both visual-row counting (lines above the
        // cursor) and the cursor's own intra-line position. Built once.
        let wrap_config = if self.line_wrap_enabled {
            Some(self.make_wrap_config(buffer))
        } else {
            None
        };

        // Count visual rows from top_byte up to (but not including) the
        // cursor's line. With wrap enabled, lines above the cursor may
        // occupy multiple visual rows; counting logical lines anchors
        // popups (e.g. completion) to the wrong screen row in heavily
        // wrapped buffers — see issue #1794.
        let mut iter = buffer.line_iterator(self.top_byte(), 80);
        let mut screen_row: usize = 0;

        while let Some((line_byte, content)) = iter.next_logical_line() {
            if line_byte >= line_start {
                break;
            }
            if let Some(ref config) = wrap_config {
                let line_end = iter.current_position();
                let line_text = content.trim_end_matches(['\n', '\r']);
                screen_row += Self::count_visual_rows_for_line(
                    line_byte,
                    line_end,
                    line_text,
                    config,
                    &[],
                    &[],
                    None,
                );
            } else {
                screen_row += 1;
            }
        }

        // Calculate screen column and additional wrapped rows if line wrapping is enabled
        let (screen_col, additional_rows) = if let Some(ref config) = wrap_config {
            // Get the line text for wrapping
            let mut line_iter = buffer.line_iterator(line_start, 80);
            let line_text = if let Some((_start, content)) = line_iter.next_logical_line() {
                // Remove trailing newline if present
                content.trim_end_matches(['\n', '\r']).to_string()
            } else {
                String::new()
            };

            // Wrap the line via the renderer's word-boundary wrap so the
            // returned screen coordinates match where the renderer draws
            // the cursor.
            let layout = Self::compute_line_layout(&line_text, config);

            // Find which ViewLine the cursor is in and its visual column.
            let (segment_idx, col_in_segment) =
                crate::view::line_wrap_cache::byte_position_in_layout(&layout, column);

            (col_in_segment as u16, segment_idx)
        } else {
            // No wrapping - account for horizontal scrolling
            let screen_col = column.saturating_sub(self.left_column) as u16;
            (screen_col, 0)
        };

        // If `top_byte` sits mid-line (visual offset into the first
        // visible logical line), the on-screen origin is shifted up by
        // that offset.
        let total_row = (screen_row + additional_rows).saturating_sub(self.top_view_line_offset());

        // Return (x, y) which is (col, row)
        (screen_col, total_row as u16)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::buffer::Buffer;
    use crate::model::cursor::Cursor;

    /// The capped count agrees with the plain one whenever it does not
    /// saturate — the guarantee that lets scroll math use it everywhere.
    #[test]
    fn capped_row_count_matches_the_plain_count() {
        let content = format!(
            "short\n{}\nanother short line\n",
            "word ".repeat(400) // ~2 KB: wraps to many rows, still under budget
        );
        let mut buffer = Buffer::from_str_test(&content);
        let wrap_config = WrapConfig::new(80, 5, true, false);

        let mut plain = Vec::new();
        {
            let mut iter = buffer.line_iterator(0, 80);
            while let Some((line_start, text)) = iter.next_logical_line() {
                let line_end = iter.current_position();
                plain.push(Viewport::count_visual_rows_for_line(
                    line_start,
                    line_end,
                    text.trim_end_matches(['\n', '\r']),
                    &wrap_config,
                    &[],
                    &[],
                    None,
                ));
            }
        }

        let mut capped = Vec::new();
        {
            let mut iter = buffer.line_iterator(0, 80);
            // A cap far above any of these lines' row counts: nothing saturates.
            while let Some((_, rows, line_end)) = Viewport::next_line_visual_rows_capped(
                &mut iter,
                10_000,
                &wrap_config,
                &[],
                &[],
                None,
            ) {
                assert!(line_end.is_some(), "no line here should saturate");
                capped.push(rows);
            }
        }

        assert_eq!(plain, capped);
        assert!(plain.iter().any(|&r| r > 1), "the middle line must wrap");
    }

    /// The budget never exceeds one `LineIterator` piece, however large a `cap`
    /// it is asked for — two callers inflate `cap` by `top_view_line_offset`,
    /// which grows without bound as you page into one enormous line.
    #[test]
    fn row_budget_never_exceeds_one_read_piece() {
        let wrap_config = WrapConfig::new(200, 5, true, false);

        // A viewport-sized cap stays well inside the ceiling...
        assert!(Viewport::row_budget_bytes(40, &wrap_config) < MAX_LINE_BYTES);
        // ...and an offset-inflated one is clamped rather than believed.
        for cap in [1_000, 30_000, usize::MAX] {
            assert_eq!(
                Viewport::row_budget_bytes(cap, &wrap_config),
                MAX_LINE_BYTES,
                "cap {cap} escaped the budget ceiling"
            );
        }
    }

    /// Issue #1806: a line far taller than the viewport is answered from a
    /// bounded prefix. The count saturates at the cap, the line end is
    /// withheld (the read stopped mid-line), and — the whole point — the
    /// iterator never walked the megabytes behind it.
    #[test]
    fn capped_row_count_saturates_on_a_huge_line() {
        let content = format!("{}\n", "word ".repeat(400_000)); // ~2 MB, one line
        let mut buffer = Buffer::from_str_test(&content);
        let wrap_config = WrapConfig::new(80, 5, true, false);

        let mut iter = buffer.line_iterator(0, 80);
        let (line_start, rows, line_end) =
            Viewport::next_line_visual_rows_capped(&mut iter, 30, &wrap_config, &[], &[], None)
                .expect("a line");

        assert_eq!(line_start, 0);
        assert_eq!(rows, 30, "reported as exactly the cap");
        assert!(line_end.is_none(), "a saturating count parks mid-line");
        assert!(
            iter.current_position() < 100_000,
            "read {} bytes of a 2 MB line to lay out 30 rows",
            iter.current_position()
        );
    }

    /// A short line is still returned whole and exactly, even under a cap it
    /// could never reach.
    #[test]
    fn capped_row_count_is_exact_below_the_cap() {
        let mut buffer = Buffer::from_str_test("alpha\nbeta\n");
        let wrap_config = WrapConfig::new(80, 5, true, false);

        let mut iter = buffer.line_iterator(0, 80);
        let (line_start, rows, line_end) =
            Viewport::next_line_visual_rows_capped(&mut iter, 30, &wrap_config, &[], &[], None)
                .expect("a line");
        assert_eq!((line_start, rows), (0, 1));
        assert_eq!(line_end, Some(6), "iterator sits on the next line");
    }

    /// The path that made opening a 53 MB single-line file take ~19 s: it asked
    /// for the line's full row count when it only needed to know the line fills
    /// the screen. Same result, without that count.
    ///
    /// Asserted on work, not the clock: a full row count only ever goes through
    /// the row-count cache, so an empty cache after the clamp *is* the statement
    /// that the line was never wrapped — and it means that on a loaded CI box
    /// too.
    #[test]
    fn clamping_a_huge_line_does_not_count_the_whole_line() {
        let content = format!("{}\n", "word ".repeat(200_000)); // ~1 MB, one line
        let mut buffer = Buffer::from_str_test(&content);
        let mut vp = Viewport::new(80, 24);
        vp.line_wrap_enabled = true;

        vp.set_top_byte_with_limit(&mut buffer, &[], &[], 0);

        assert_eq!(vp.top_byte(), 0, "the proposed top still stands");
        assert!(
            vp.wrap_row_cache.is_empty(),
            "the clamp computed and cached a full row count for the huge line"
        );
    }

    /// Control for the test above: without it, an empty cache could equally
    /// mean the clamp never ran.
    #[test]
    fn clamping_ordinary_lines_does_count_them() {
        let content = "a short line\n".repeat(200);
        let mut buffer = Buffer::from_str_test(&content);
        let mut vp = Viewport::new(80, 24);
        vp.line_wrap_enabled = true;

        vp.set_top_byte_with_limit(&mut buffer, &[], &[], 0);

        assert!(
            !vp.wrap_row_cache.is_empty(),
            "the clamp should count ordinary lines exactly, and cache them"
        );
    }

    #[test]
    fn test_viewport_new() {
        let vp = Viewport::new(80, 24);
        assert_eq!(vp.width, 80);
        assert_eq!(vp.height, 24);
        assert_eq!(vp.top_byte(), 0);
    }

    #[test]
    fn test_scroll_up_down() {
        // Create a buffer with more lines than the viewport to make scrolling possible
        let mut content = String::new();
        for i in 1..=50 {
            if i > 1 {
                content.push('\n');
            }
            content.push_str(&format!("line{}", i));
        }
        let mut buffer = Buffer::from_str_test(&content);
        let mut vp = Viewport::new(80, 24);

        vp.scroll_down(&mut buffer, &[], &[], &[], 10);
        // Check that we scrolled down (top_byte should be > 0)
        assert!(vp.top_byte() > 0);

        let prev_top = vp.top_byte();
        vp.scroll_up(&mut buffer, &[], &[], &[], 5);
        // Check that we scrolled up (top_byte should be less than before)
        assert!(vp.top_byte() < prev_top);

        vp.scroll_up(&mut buffer, &[], &[], &[], 100);
        assert_eq!(vp.top_byte(), 0); // Can't scroll past 0
    }

    /// A collapsed fold draws one header row, so a scroll of N rows must
    /// cross it for free. Counting its hidden lines instead spends the whole
    /// page budget on rows nobody sees and the viewport barely moves.
    #[test]
    fn scroll_skips_collapsed_fold_bodies() {
        // 200 lines; lines 10..=99 (0-based) are hidden behind a header at 9.
        let mut content = String::new();
        for i in 0..200 {
            content.push_str(&format!("line{i}\n"));
        }
        let mut buffer = Buffer::from_str_test(&content);
        let line_start = |n: usize| -> usize {
            content
                .split_inclusive('\n')
                .take(n)
                .map(|l| l.len())
                .sum::<usize>()
        };
        let hidden = [(line_start(10), line_start(100))];

        // 24 rendered rows down from line 0: lines 0..=9 are ten rows, the
        // fold body is none, so the remaining 14 land on line 114.
        let mut vp = Viewport::new(80, 30);
        vp.scroll_down(&mut buffer, &[], &[], &hidden, 24);
        assert_eq!(vp.top_byte(), line_start(114));

        // Scrolling back the same distance returns to the start.
        vp.scroll_up(&mut buffer, &[], &[], &hidden, 24);
        assert_eq!(vp.top_byte(), 0);

        // A step that ends inside the fold lands past it, never on a hidden
        // line — the cursor follows the viewport top, so it must be visible.
        let mut vp = Viewport::new(80, 30);
        vp.scroll_down(&mut buffer, &[], &[], &hidden, 10);
        assert_eq!(vp.top_byte(), line_start(100));

        // Without the fold the same walk is pure logical lines.
        let mut vp = Viewport::new(80, 30);
        vp.scroll_down(&mut buffer, &[], &[], &[], 24);
        assert_eq!(vp.top_byte(), line_start(24));
    }

    #[test]
    fn center_on_position_unwrapped_centers_logical_line() {
        // 50 single-row lines, height 24 → half = 12. Centering on line
        // index 29 should put the viewport top 12 logical lines above it.
        let mut content = String::new();
        for i in 0..50 {
            content.push_str(&format!("line{i}\n"));
        }
        let mut buffer = Buffer::from_str_test(&content);
        let mut vp = Viewport::new(80, 24); // wrap off by default

        let pos = buffer.line_start_offset(29).unwrap();
        vp.center_on_position(&mut buffer, pos);

        assert_eq!(buffer.get_line_number(vp.top_byte()), 29 - 12);
        assert_eq!(vp.top_view_line_offset(), 0);
    }

    #[test]
    fn center_on_position_wrapped_counts_visual_rows() {
        // A long line that wraps into many visual rows sits directly above
        // the match. Naive logical-line centering (match_line - height/2)
        // would scroll the top back past the long line and push the match
        // off the bottom of the pane; visual-row centering must instead
        // stop *inside* the long line so the match stays centered.
        let mut content = String::new();
        for i in 0..18 {
            content.push_str(&format!("short{i}\n"));
        }
        content.push_str(&"x".repeat(400)); // line 18: wraps into >5 rows
        content.push('\n');
        content.push_str("THE_MATCH\n"); // line 19
        for i in 0..10 {
            content.push_str(&format!("tail{i}\n"));
        }
        let mut buffer = Buffer::from_str_test(&content);

        let mut vp = Viewport::new(40, 10); // half = 5
        vp.line_wrap_enabled = true;

        let pos = buffer.line_start_offset(19).unwrap();
        vp.center_on_position(&mut buffer, pos);

        // Visual-row centering lands the top inside the wrapped line just
        // above the match (line 18), not back at logical line 14.
        assert_eq!(
            buffer.get_line_number(vp.top_byte()),
            18,
            "top should sit within the wrapped line above the match"
        );
        assert!(
            vp.top_view_line_offset() > 0,
            "top should be partway down the wrapped line's visual rows"
        );
    }

    #[test]
    fn test_ensure_line_visible() {
        let mut buffer = Buffer::from_str_test("line1\nline2\nline3\nline4\nline5\nline6\nline7\nline8\nline9\nline10\nline11\nline12\nline13\nline14\nline15\nline16\nline17\nline18\nline19\nline20\nline21\nline22\nline23\nline24\nline25\nline26\nline27\nline28\nline29\nline30\nline31\nline32\nline33\nline34\nline35\nline36\nline37\nline38\nline39\nline40\nline41\nline42\nline43\nline44\nline45\nline46\nline47\nline48\nline49\nline50\nline51");
        let mut vp = Viewport::new(80, 24);
        vp.scroll_offset = 3;

        // Line within scroll offset should adjust viewport
        vp.ensure_line_visible(&mut buffer, 2);
        // top_byte should be close to the beginning since line 2 is near the top
        assert!(vp.top_byte() < 100);

        // Line far below should scroll down
        vp.ensure_line_visible(&mut buffer, 50);
        assert!(vp.top_byte() > 0);
        // Verify the line is now visible by checking we can iterate to it
        let mut iter = buffer.line_iterator(vp.top_byte(), 80);
        let mut found = false;
        for _ in 0..vp.visible_line_count() {
            if iter.next_line().is_none() {
                break;
            }
            found = true;
        }
        assert!(found);
    }

    #[test]
    fn test_ensure_visible_with_cursor() {
        let mut buffer = Buffer::from_str_test("line1\nline2\nline3\nline4\nline5\nline6\nline7\nline8\nline9\nline10\nline11\nline12\nline13\nline14\nline15\nline16\nline17\nline18\nline19\nline20");
        let mut vp = Viewport::new(80, 10);

        // Find byte position of line 15 using iterator
        let mut iter = buffer.line_iterator(0, 80);
        let mut cursor_pos = 0;
        for i in 0..15 {
            if let Some((line_start, _)) = iter.next_line() {
                if i == 14 {
                    cursor_pos = line_start;
                    break;
                }
            }
        }

        let cursor = Cursor::new(cursor_pos);
        vp.ensure_visible(&mut buffer, &cursor, &[]);

        // Verify cursor is now visible by checking we scrolled appropriately
        assert!(vp.top_byte() > 0);
    }

    #[test]
    fn test_cursor_screen_position() {
        let mut buffer = Buffer::from_str_test("line1\nline2\nline3");
        let vp = Viewport::new(80, 24);

        let cursor = Cursor::new(6); // Start of line 1 ("line2")
        let (x, y) = vp.cursor_screen_position(&mut buffer, &cursor);
        // x is column (horizontal), y is row (vertical)
        assert_eq!(x, 0); // Column 0 (start of line)
        assert_eq!(y, 1); // Row 1 (second line, since top_line is 0)
    }

    /// Issue #1794: completion popup is anchored to the wrong screen row in
    /// heavily-wrapped buffers because the row count from `top_byte` to the
    /// cursor's line was being computed in *logical lines* rather than
    /// *visual rows*. With wrap enabled, lines above the cursor that occupy
    /// multiple visual rows must each contribute their full visual-row count.
    #[test]
    fn test_cursor_screen_position_with_wrapped_lines_above() {
        // Build 4 lines where each line wraps to ~3 visual rows in a 30-col
        // viewport. Identical wrap behaviour to the issue's repro: long
        // sentences that will be word-wrapped onto multiple rows.
        let long = "the quick brown fox jumps over the lazy dog and runs away";
        let content = format!("{long}\n{long}\n{long}\n{long}");
        let mut buffer = Buffer::from_str_test(&content);

        let mut vp = Viewport::new(30, 24);
        vp.line_wrap_enabled = true;
        vp.show_line_numbers = false; // simpler width math

        // Place the cursor at the END of line 4 (last logical line). With
        // ~30-col wrap, each of the 3 prior lines wraps to 3 visual rows
        // (= 9 rows total above), and the cursor's own line lands on its
        // last sub-row. The popup expects the cursor's true visual row.
        let cursor_pos = content.len();
        let cursor = Cursor::new(cursor_pos);
        let (_x, y) = vp.cursor_screen_position(&mut buffer, &cursor);

        // With 3 wrapped lines above (>=2 visual rows each) the cursor's
        // visual row must be at least 6. Pre-fix, this returns 3 + segment
        // (i.e. ~5) because the prior 3 lines were counted as 1 row each.
        assert!(
            y >= 6,
            "expected cursor visual row >= 6 (3 wrapped lines above × >=2 rows), got {y}"
        );
    }

    #[test]
    fn test_ensure_visible_cursor_above_viewport() {
        // Create buffer with many lines
        let mut buffer = Buffer::from_str_test("line1\nline2\nline3\nline4\nline5\nline6\nline7\nline8\nline9\nline10\nline11\nline12\nline13\nline14\nline15\nline16\nline17\nline18\nline19\nline20");
        let mut vp = Viewport::new(80, 10); // 10 lines visible

        // Scroll down to show lines 10-19 (top_byte at line 10)
        // scroll_to uses 1-based line numbers, so line 10 = argument 10
        vp.scroll_to(&mut buffer, 10);
        let _old_top_byte = vp.top_byte();

        // Verify we scrolled to around line 10
        let top_line = buffer.get_line_number(vp.top_byte());
        assert!(
            top_line >= 9,
            "Should have scrolled down to at least line 10"
        );

        // Now move cursor to line 5 (above the viewport)
        let mut iter = buffer.line_iterator(0, 80);
        let mut line_5_byte = 0;
        for i in 0..5 {
            if let Some((line_start, _)) = iter.next_line() {
                if i == 4 {
                    line_5_byte = line_start;
                    break;
                }
            }
        }
        let cursor = Cursor::new(line_5_byte);

        // Before fix, this should fail because ensure_visible doesn't detect cursor is above viewport
        vp.ensure_visible(&mut buffer, &cursor, &[]);

        // Verify that viewport scrolled up to make cursor visible
        // The viewport should now be positioned so cursor (line 5) is visible
        let new_top_line = buffer.get_line_number(vp.top_byte());
        let cursor_line = buffer.get_line_number(line_5_byte);
        assert!(
            cursor_line >= new_top_line,
            "Cursor line should be at or below top of viewport"
        );
        assert!(
            new_top_line < top_line,
            "Viewport should have scrolled up from line {}",
            top_line
        );

        // Verify cursor is within visible area
        let lines_from_top = cursor_line.saturating_sub(new_top_line);
        assert!(
            lines_from_top < vp.visible_line_count(),
            "Cursor should be within visible area"
        );

        // Verify cursor is placed near the scroll margin (not centered)
        // With minimal scroll, cursor above viewport is placed at scroll_offset from top
        let expected_offset = vp.scroll_offset.min(vp.visible_line_count() / 2);
        assert!(
            lines_from_top <= expected_offset + 1,
            "Cursor should be near scroll margin, expected around {}, got {}",
            expected_offset,
            lines_from_top
        );
    }

    #[test]
    fn test_ensure_visible_cursor_below_viewport_centers() {
        // Create buffer with many lines
        let mut buffer = Buffer::from_str_test("line1\nline2\nline3\nline4\nline5\nline6\nline7\nline8\nline9\nline10\nline11\nline12\nline13\nline14\nline15\nline16\nline17\nline18\nline19\nline20");
        let mut vp = Viewport::new(80, 10); // 10 lines visible

        // Start at top (line 1 visible)
        assert_eq!(vp.top_byte(), 0);

        // Move cursor to line 15 (below viewport)
        let mut iter = buffer.line_iterator(0, 80);
        let mut line_15_byte = 0;
        for i in 0..15 {
            if let Some((line_start, _)) = iter.next_line() {
                if i == 14 {
                    line_15_byte = line_start;
                    break;
                }
            }
        }
        let cursor = Cursor::new(line_15_byte);

        vp.ensure_visible(&mut buffer, &cursor, &[]);

        // Verify cursor is placed near the bottom scroll margin (not centered)
        // With minimal scroll, cursor below viewport is placed at (viewport - scroll_offset) from top
        let new_top_line = buffer.get_line_number(vp.top_byte());
        let cursor_line = buffer.get_line_number(line_15_byte);
        let lines_from_top = cursor_line.saturating_sub(new_top_line);

        let viewport_lines = vp.visible_line_count();
        let expected_offset = vp.scroll_offset.min(viewport_lines / 2);
        let expected_bottom = viewport_lines.saturating_sub(expected_offset + 1);
        assert!(
            lines_from_top >= expected_bottom.saturating_sub(1),
            "Cursor should be near bottom margin when jumping down, expected around {}, got {}",
            expected_bottom,
            lines_from_top
        );
    }

    #[test]
    fn column_visible_simple_uses_explicit_render_width() {
        // Compose mode can render a narrow centered page inside a wide viewport.
        let mut vp = Viewport::new(80, 24);
        vp.line_wrap_enabled = false;
        vp.horizontal_scroll_offset = 0;

        let left_column = vp.column_visible_simple(12, 13, 6, 0);

        assert_eq!(
            left_column, 7,
            "the layout path must use its passed render width, not viewport width"
        );
    }

    #[test]
    fn ensure_column_visible_does_not_reserve_scrollbar_inside_content_width() {
        let mut buffer = Buffer::from_str_test("x".repeat(20).as_str());
        let mut vp = Viewport::new(10, 24);
        vp.line_wrap_enabled = false;
        vp.horizontal_scroll_offset = 0;

        // The one-line buffer has a six-column gutter, leaving columns 0..=3
        // visible within the viewport's content width.
        vp.ensure_column_visible(3, 20, &mut buffer);

        assert_eq!(vp.left_column, 0);
    }

    #[test]
    fn test_ensure_column_visible_resets_to_zero() {
        // Test that horizontal scroll is reset when cursor moves to column 0
        // This simulates what happens after pressing Enter on a long line
        let mut buffer = Buffer::from_str_test("a".repeat(100).as_str());
        let mut vp = Viewport::new(80, 24);
        vp.line_wrap_enabled = false;

        // First, scroll right by moving cursor to end of line
        let cursor_at_end = Cursor::new(100);
        vp.ensure_visible(&mut buffer, &cursor_at_end, &[]);

        println!("After moving to position 100:");
        println!("  left_column = {}", vp.left_column);

        // Verify we've scrolled right
        assert!(
            vp.left_column > 0,
            "Should have scrolled right, but left_column = {}",
            vp.left_column
        );

        // Now simulate pressing Enter: newline is added, cursor moves to start of new line
        // Add the newline to the buffer
        // Note: In real usage the buffer would be modified, but for this test we just
        // need to test ensure_column_visible with cursor at column 0

        // Test ensure_column_visible directly with column=0 and the current left_column
        // This simulates what should happen when cursor is at column 0 on a new line
        vp.ensure_column_visible(0, 0, &mut buffer); // column=0, line_length=0 (empty new line)

        println!("After ensure_column_visible(0, 0):");
        println!("  left_column = {}", vp.left_column);

        assert_eq!(
            vp.left_column, 0,
            "left_column should be reset to 0 when cursor is at column 0, but got {}",
            vp.left_column
        );
    }

    /// Regression for #1689 follow-up: in wrap mode with
    /// `top_view_line_offset > 0`, the early-return at the top of
    /// `ensure_visible` used to fire for *any* cursor below `top_byte`,
    /// stranding cursors that were many lines below the viewport. Verify
    /// the early-return now defers to a real scroll when the cursor is
    /// far below (more than 2x viewport height in source lines).
    #[test]
    fn test_ensure_visible_far_below_top_with_wrap_offset_does_scroll() {
        // 200 lines so we have plenty of room for "far below".
        let mut content = String::new();
        for i in 0..200 {
            content.push_str(&format!("line_{i}\n"));
        }
        let mut buffer = Buffer::from_str_test(&content);

        let mut vp = Viewport::new(80, 10); // 10 visible lines
        vp.line_wrap_enabled = true;

        // Park top_byte at line 5 and inject a non-zero wrap offset to
        // trigger the wrap-mode early-return. (Real users hit this state
        // via the wrap-aware scroll-up path, but we set it manually here.)
        let mut iter = buffer.line_iterator(0, 80);
        let mut line_5_byte = 0;
        for i in 0..5 {
            if let Some((line_start, _)) = iter.next_line() {
                if i == 4 {
                    line_5_byte = line_start;
                    break;
                }
            }
        }
        vp.set_top_byte(line_5_byte);
        vp.set_top_view_line_offset(2); // > 0 → triggers the early-return path

        let top_before = vp.top_byte();

        // Move cursor to line 100 — way below `top_byte` (10*2=20 viewport
        // heights of 1 source line each, so cursor at line 100 is well
        // beyond `top_line + 2*viewport_height`).
        let mut iter = buffer.line_iterator(0, 80);
        let mut line_100_byte = 0;
        for i in 0..100 {
            if let Some((line_start, _)) = iter.next_line() {
                if i == 99 {
                    line_100_byte = line_start;
                    break;
                }
            }
        }
        let cursor = Cursor::new(line_100_byte);

        vp.ensure_visible(&mut buffer, &cursor, &[]);

        assert_ne!(
            vp.top_byte(),
            top_before,
            "ensure_visible must scroll when the cursor is far below the viewport \
             top, even in wrap mode with `top_view_line_offset > 0`. Pre-fix this \
             early-returned at the top of `ensure_visible` and the viewport stalled."
        );

        // Cursor should now be inside the viewport's source-line range.
        let new_top_line = buffer.get_line_number(vp.top_byte());
        let cursor_line = buffer.get_line_number(line_100_byte);
        let viewport_height = vp.visible_line_count();
        assert!(
            cursor_line >= new_top_line && cursor_line < new_top_line + viewport_height,
            "After scrolling, cursor line {cursor_line} should be inside viewport \
             line range [{new_top_line}, {})",
            new_top_line + viewport_height
        );
    }

    /// Companion case: in the SAME wrap-mode + `top_view_line_offset > 0`
    /// state, a cursor that's only slightly below the viewport top must
    /// still trigger the early-return so we don't undo the wrap-aware
    /// scroll machinery that was added for #1574. The fix's heuristic is
    /// "skip when cursor is within 2x viewport-height of top".
    #[test]
    fn test_ensure_visible_close_below_top_with_wrap_offset_still_skips() {
        let mut content = String::new();
        for i in 0..50 {
            content.push_str(&format!("line_{i}\n"));
        }
        let mut buffer = Buffer::from_str_test(&content);

        let mut vp = Viewport::new(80, 10);
        vp.line_wrap_enabled = true;
        // A 50-line file is well inside the index's ceilings, so this frame has
        // a row pass and that pass owns vertical placement — the state the
        // deferral is *for*. See `..._with_no_row_pass_scrolls` below for the
        // other side.
        vp.row_pass_owns_placement = true;

        let mut iter = buffer.line_iterator(0, 80);
        let mut line_5_byte = 0;
        for i in 0..5 {
            if let Some((line_start, _)) = iter.next_line() {
                if i == 4 {
                    line_5_byte = line_start;
                    break;
                }
            }
        }
        vp.set_top_byte(line_5_byte);
        vp.set_top_view_line_offset(2);

        let top_before = vp.top_byte();

        // Cursor at line 8 — only 3 lines below top, well within 2x
        // viewport height (=20). Should hit the early-return: viewport
        // unchanged, deferred to render-time `ensure_visible_in_layout`.
        let mut iter = buffer.line_iterator(0, 80);
        let mut line_8_byte = 0;
        for i in 0..8 {
            if let Some((line_start, _)) = iter.next_line() {
                if i == 7 {
                    line_8_byte = line_start;
                    break;
                }
            }
        }
        let cursor = Cursor::new(line_8_byte);

        vp.ensure_visible(&mut buffer, &cursor, &[]);

        assert_eq!(
            vp.top_byte(),
            top_before,
            "Cursor close below top in wrap mode must still defer to \
             ensure_visible_in_layout (the #1574 invariant). Got top_byte={}, expected {}",
            vp.top_byte(),
            top_before
        );
    }

    /// The other side: with no wrap index there is no row pass to defer to, so
    /// a cursor below the screen must move the viewport — and can only do so by
    /// parking the top *inside* the line, the file being one line.
    ///
    /// Both halves failed before: cursor-follow only ever landed on a line
    /// start, and once an offset was set `ensure_visible` returned early for
    /// every later scroll, so `Down` stopped at the first screenful (#1806).
    #[test]
    fn test_ensure_visible_inside_one_long_line_with_no_row_pass_scrolls() {
        // One line, no newline: the file is a single wrapped paragraph, so
        // there is no earlier line for the backward walk to use.
        let content = "x".repeat(4000);
        let mut buffer = Buffer::from_str_test(&content);

        let mut vp = Viewport::new(80, 10);
        vp.line_wrap_enabled = true;
        // No row pass for this frame, so the byte-oriented pass places the
        // viewport. This buffer is small, so that is the row-counted path, not
        // the anchored one — `addresses_rows_by_byte` covers large files only.
        assert!(!vp.row_pass_owns_placement);

        // Top of the file, and a cursor far enough into the line to be well
        // below a 10-row viewport at any plausible row width.
        vp.set_top_byte(0);
        vp.set_top_view_line_offset(0);
        let cursor = Cursor::new(2000);

        vp.ensure_visible(&mut buffer, &cursor, &[]);

        assert!(
            vp.top_view_line_offset() > 0,
            "a cursor {} bytes into a single wrapped line is below a {}-row \
             viewport; ensure_visible must scroll to it, which inside one line \
             means a non-zero top_view_line_offset (got {}, top_byte {})",
            2000,
            vp.visible_line_count(),
            vp.top_view_line_offset(),
            vp.top_byte(),
        );
    }

    #[test]
    fn test_ensure_visible_non_default_scroll_offset() {
        // 100 lines to guarantee the viewport can fill from any scroll target
        let mut content = String::new();
        for i in 1..=100 {
            content.push_str(&format!("line{i}\n"));
        }
        let mut buffer = Buffer::from_str_test(&content);
        let mut vp = Viewport::new(80, 24);
        vp.scroll_offset = 10;

        // Position cursor at line 35, well below the initial viewport
        let mut iter = buffer.line_iterator(0, 80);
        let mut target_byte = 0;
        for i in 0..35 {
            if let Some((line_start, _)) = iter.next_line() {
                if i == 34 {
                    target_byte = line_start;
                    break;
                }
            }
        }
        let cursor = Cursor::new(target_byte);

        vp.ensure_visible(&mut buffer, &cursor, &[]);

        let new_top_line = buffer.get_line_number(vp.top_byte());
        let cursor_line = buffer.get_line_number(target_byte);
        let lines_from_top = cursor_line.saturating_sub(new_top_line);

        let viewport_lines = vp.visible_line_count();
        // With scroll_offset=10, viewport=24 → effective = min(10, 12) = 10
        // Cursor below viewport → target = viewport - effective_offset - 1 = 13
        let expected_rows_from_top =
            viewport_lines.saturating_sub(vp.scroll_offset.min(viewport_lines / 2) + 1);
        assert!(
            lines_from_top >= expected_rows_from_top.saturating_sub(1),
            "With scroll_offset=10, cursor should be near bottom margin (~row {}), got {}",
            expected_rows_from_top,
            lines_from_top
        );
        // Default scroll_offset=3 would place cursor at row ~20, so
        // row 13 proves a non-default scroll_offset changes behavior.
        assert!(
            lines_from_top < viewport_lines.saturating_sub(3),
            "With scroll_offset=10, cursor at row {} should be earlier than the default-offset position (~row 21)",
            lines_from_top
        );
    }
}

/// The cursor's line, expanded to the rows the frame will actually draw.
///
/// The index is canonical (cursor-blind); the frame renders the cursor's line
/// cursor-aware. Activation scopes are line-local, so this one line is the
/// only place the two can disagree — and placement has to work in the drawn
/// rows or it parks the cursor outside the margin band and the next press
/// finds nothing to do (fresh#1574's stall). Resolved by the render path,
/// which has the managers and the cursors; `None` means no divergence.
#[derive(Debug, Clone, Copy)]
pub struct CursorLineExpansion {
    /// Byte where the divergent line starts.
    pub line_start: usize,
    /// The line's first row, canonical == drawn (rows above never diverge).
    pub first_row: u32,
    /// Rows the canonical index gives the line.
    pub canonical_rows: usize,
    /// Rows the cursor-aware wrap gives it.
    pub drawn_rows: usize,
    /// The row the cursor is drawn on, in effective rows.
    pub cursor_row_drawn: u32,
}
