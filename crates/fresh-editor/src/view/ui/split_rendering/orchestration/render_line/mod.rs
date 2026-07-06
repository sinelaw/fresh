//! Per-line render loop.
//!
//! This is the hot path for drawing the editor viewport. It consumes the
//! shared `SelectionContext` / `DecorationContext` carriers along with a
//! concern-scoped `LineRenderInput`, and produces a vector of styled
//! `Line<'static>` plus cursor and per-cell metadata.
//!
//! `render_view_lines` is the orchestrator: per view line it runs the
//! gutter pass, the per-cell pass (`cells`), the cursor-placement passes,
//! inline diagnostics, the tail fills, and the mouse-mapping bookkeeping.
//! The post-loop work (implicit trailing line, EOF tildes) lives in
//! `trailing`. Everything here is quarantined to `orchestration/`.

use super::super::gutter::{render_left_margin, LeftMarginContext};
use super::super::layout::ViewAnchor;
use super::super::spans::push_span_with_map;
use super::contexts::{DecorationContext, SelectionContext};
use super::overlay_sweep::OverlayActiveSet;
use super::selection_sweep::SelectionActiveSet;
use super::tail_fill::{resolve_tail_fill, TailFillInput};
use cells::{render_line_cells, CellPassInput};
use trailing::{fill_eof_rows, render_implicit_trailing_line, PostRowAccumulator, PostRowContext};

mod cells;
mod trailing;

use crate::app::types::ViewLineMapping;
use crate::config::IndentationGuideMode;
use crate::state::EditorState;
use crate::view::theme::Theme;
use crate::view::ui::view_pipeline::{should_show_line_number, LineStart, ViewLine};
use ratatui::layout::Rect;
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span};
use std::collections::HashSet;

pub(crate) struct LineRenderOutput {
    pub lines: Vec<Line<'static>>,
    pub cursor: Option<(u16, u16)>,
    pub last_line_end: Option<LastLineEnd>,
    pub content_lines_rendered: usize,
    pub view_line_mappings: Vec<ViewLineMapping>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct LastLineEnd {
    pub pos: (u16, u16),
    pub terminated_with_newline: bool,
}

pub(crate) struct LineRenderInput<'a> {
    pub state: &'a EditorState,
    pub theme: &'a Theme,
    /// Display lines from the view pipeline (each line has its own mappings, styles, etc.)
    pub view_lines: &'a [ViewLine],
    pub view_anchor: ViewAnchor,
    pub render_area: Rect,
    pub gutter_width: usize,
    pub selection: &'a SelectionContext,
    pub decorations: &'a DecorationContext,
    pub visible_line_count: usize,
    pub lsp_waiting: bool,
    pub is_active: bool,
    pub line_wrap: bool,
    pub estimated_lines: usize,
    /// Left column offset for horizontal scrolling
    pub left_column: usize,
    /// Whether to show relative line numbers (distance from cursor)
    pub relative_line_numbers: bool,
    /// Skip REVERSED style on the primary cursor (session mode or non-block cursor style)
    pub session_mode: bool,
    /// No hardware cursor: always render software cursor indicators
    pub software_cursor_only: bool,
    /// Whether to show line numbers in the gutter
    pub show_line_numbers: bool,
    /// Whether the gutter shows byte offsets instead of line numbers
    pub byte_offset_mode: bool,
    /// Whether to show tilde (~) markers on lines past end-of-file
    pub show_tilde: bool,
    /// Whether to highlight the line containing the cursor
    pub highlight_current_line: bool,
    /// Indentation guide rendering mode.
    pub indentation_guide: IndentationGuideMode,
    /// Glyph used when an indentation guide is drawn.
    pub indentation_guide_glyph: &'a str,
    /// Per-cell theme key map for the theme inspector (screen_width used for indexing)
    pub cell_theme_map: &'a mut Vec<crate::app::types::CellThemeInfo>,
    /// Screen width for cell_theme_map indexing
    pub screen_width: u16,
}

/// Software-cursor screen position, tracked across all per-line passes.
#[derive(Default)]
struct CursorTracker {
    x: u16,
    y: u16,
    found: bool,
}

impl CursorTracker {
    /// Record the cursor position unless one was already found.
    fn place(&mut self, x: u16, y: u16) {
        if !self.found {
            self.force(x, y);
        }
    }

    /// Record the cursor position, overriding any earlier hit. Used by
    /// the line-end passes that re-derive a more accurate position.
    fn force(&mut self, x: u16, y: u16) {
        self.x = x;
        self.y = y;
        self.found = true;
    }
}

/// Monotonic cursors for the O(1)-amortised highlight/semantic span
/// lookups (spans are sorted by byte range; the per-cell pass only ever
/// advances these).
#[derive(Default)]
struct SpanCursors {
    highlight: usize,
    semantic: usize,
}

#[derive(Clone, Copy, Debug)]
struct ActiveIndentationGuide {
    column: usize,
    first_line_idx: usize,
    last_line_idx: usize,
}

impl ActiveIndentationGuide {
    /// Resolve the single guide drawn in `active` mode: the indentation guide
    /// of the block enclosing the primary cursor. Block detection reuses the
    /// editor's indent-based folding (`find_fold_range_at_byte`) rather than any
    /// bespoke delimiter heuristics, so a guide spans exactly the rows the
    /// matching fold would hide and the column sits at the block header's
    /// indentation.
    fn for_view_lines(
        mode: IndentationGuideMode,
        is_active_split: bool,
        state: &EditorState,
        view_lines: &[ViewLine],
        primary_cursor_position: usize,
    ) -> Option<Self> {
        use crate::view::folding::indent_folding;

        if mode != IndentationGuideMode::Active || !is_active_split {
            return None;
        }

        let tab_size = normalized_tab_size(state.buffer_settings.tab_size);
        let est_ll = state.buffer.estimated_line_length();
        let max_scan_bytes = crate::config::INDENT_FOLD_MAX_SCAN_LINES * est_ll;
        let max_upward = crate::config::INDENT_FOLD_MAX_UPWARD_SCAN;

        // Target the cursor's line *start*: `find_fold_range_at_byte` matches a
        // fold whose last hidden line starts at or after the target, so probing
        // with the raw cursor byte would miss the enclosing block whenever the
        // cursor sits past the indentation of that block's last line.
        let target_byte =
            indent_folding::find_line_start_byte(&state.buffer, primary_cursor_position);
        let (header_byte, body_start_byte, body_end_byte) =
            indent_folding::find_fold_range_at_byte(
                &state.buffer,
                target_byte,
                tab_size,
                max_scan_bytes,
                max_upward,
            )?;

        // The guide sits at the header line's indentation column; every body
        // line is more indented than the header, so each has a cell there.
        let header_line = state
            .buffer
            .get_line(state.buffer.get_line_number(header_byte))?;
        let column = indent_folding::slice_indent(&header_line, tab_size).0;

        // Map the fold's hidden byte range onto the visible view-line indices.
        let first_line_idx = view_lines
            .iter()
            .position(|line| line.source_start_byte.is_some_and(|b| b >= body_start_byte))?;
        let last_line_idx = view_lines
            .iter()
            .enumerate()
            .rev()
            .find(|(_, line)| line.source_start_byte.is_some_and(|b| b < body_end_byte))
            .map(|(idx, _)| idx)?;
        if first_line_idx > last_line_idx {
            return None;
        }

        Some(Self {
            column,
            first_line_idx,
            last_line_idx,
        })
    }

    fn column_for_line(self, line_idx: usize) -> Option<usize> {
        (line_idx >= self.first_line_idx && line_idx <= self.last_line_idx).then_some(self.column)
    }
}

fn normalized_tab_size(tab_size: usize) -> usize {
    match tab_size {
        0 => 4,
        n => n,
    }
}

/// Leading-indent classification of a single view line for guide drawing.
/// Returns `(indent_width, has_content)`. `None` means the row contributes no
/// guide and does not affect the indentation staircase — soft-wrap
/// continuations and plugin-injected rows.
fn view_line_indent(line: &ViewLine, tab_size: usize) -> Option<(usize, bool)> {
    if line.source_start_byte.is_none()
        || matches!(
            line.line_start,
            LineStart::AfterBreak | LineStart::AfterInjectedNewline
        )
    {
        return None;
    }

    // `line.text` is display text with tabs already expanded to spaces, so the
    // measured width matches the rendered column of each leading cell.
    let (width, all_blank) =
        crate::view::folding::indent_folding::slice_indent(line.text.as_bytes(), tab_size);
    Some((width, !all_blank))
}

/// Computes `all`-mode indentation-guide columns one visible row at a time
/// while walking the viewport top-to-bottom. It keeps an indentation staircase
/// (`stack`) of the open ancestor indent widths; per-row columns are written
/// into a caller-owned buffer, so a full render adds no per-frame heap
/// allocation beyond these reused buffers.
#[derive(Default)]
struct GuideColumnScanner {
    stack: Vec<usize>,
}

impl GuideColumnScanner {
    /// Write the guide columns for `line` into `out` and advance the staircase.
    ///
    /// `next_content_indent` is the indentation of the next non-blank source row
    /// below `line` (looked up by the caller), used only when `line` is blank.
    fn columns_for(
        &mut self,
        line: &ViewLine,
        tab_size: usize,
        next_content_indent: Option<usize>,
        out: &mut Vec<usize>,
    ) {
        match view_line_indent(line, tab_size) {
            // Content row: its guides are the open ancestors strictly to its
            // left; it then becomes an ancestor for the rows below.
            Some((width, true)) => {
                while self.stack.last().is_some_and(|&w| w >= width) {
                    self.stack.pop();
                }
                fill_guide_columns(&self.stack, width, tab_size, out);
                self.stack.push(width);
            }
            // Blank (empty or whitespace-only) row: draw the enclosing block's
            // guides straight through it so they stay continuous, without
            // disturbing the staircase. The guides shown are those of the
            // *deeper* of the row above (the open block) and the row below (the
            // upcoming content) — so guides flow into a nested body even when the
            // blank line sits directly under the block opener, before any body
            // row has been seen (e.g. the empty line right after `fn f() {`).
            Some((_, false)) => {
                let above = self.stack.last().copied().unwrap_or(0);
                let level = above.max(next_content_indent.unwrap_or(0));
                let cut = self.stack.partition_point(|&w| w < level);
                fill_guide_columns(&self.stack[..cut], level, tab_size, out);
            }
            // Soft-wrap continuation row: it carries no indent of its own, but
            // its wrap-indent padding aligns under the parent line's leading
            // whitespace. Replay the parent content row's guides through that
            // padding so the staircase runs unbroken across the wrap. The padding
            // width is the row's leading run of byte-less cells (empty when
            // `wrap_indent` is off, which correctly yields no guides). Computed
            // against the open ancestors without mutating the staircase — a
            // continuation opens no block.
            None if matches!(line.line_start, LineStart::AfterBreak) => {
                let pad_width = line
                    .char_source_bytes
                    .iter()
                    .take_while(|b| b.is_none())
                    .count();
                let cut = self.stack.partition_point(|&w| w < pad_width);
                fill_guide_columns(&self.stack[..cut], pad_width, tab_size, out);
            }
            // Injected / source-less row: no guides, staircase untouched.
            None => out.clear(),
        }
    }
}

/// Indentation of the next non-blank source row at or after `start_idx`, used to
/// flow blank-line guides into an upcoming nested body. Blank and
/// continuation/injected rows are skipped; `None` if none remain.
fn next_content_indent(
    view_lines: &[ViewLine],
    start_idx: usize,
    tab_size: usize,
) -> Option<usize> {
    view_lines
        .get(start_idx..)
        .into_iter()
        .flatten()
        .find_map(|line| match view_line_indent(line, tab_size) {
            Some((width, true)) => Some(width),
            _ => None,
        })
}

/// Reconstruct the open-ancestor indentation stack as of the buffer line
/// starting at `first_visible_line_start`, by walking the buffer *upward*.
///
/// This is exactly the staircase a [`GuideColumnScanner`] would hold had it
/// scanned every line from the buffer's start: the strictly-increasing indent
/// widths of the enclosing blocks. Deriving the seed from the buffer's absolute
/// indentation — rather than from the handful of `view_lines` that precede the
/// viewport (there are none under vertical scroll, since `view_lines` begins at
/// the viewport top) — makes a row's guide columns depend only on absolute
/// indentation. A block whose opening line has scrolled above the viewport then
/// still contributes its guide to the interior rows below it, instead of the
/// guide vanishing until the opener scrolls back into view.
fn prime_guide_stack_from_buffer(
    buffer: &crate::model::buffer::Buffer,
    first_visible_line_start: usize,
    tab_size: usize,
    max_upward_lines: usize,
) -> Vec<usize> {
    use crate::view::folding::indent_folding::{find_line_start_byte, slice_indent};

    // Collected in decreasing order while walking up, then reversed.
    let mut ancestors_desc: Vec<usize> = Vec::new();
    let mut min_indent_seen = usize::MAX;
    let mut line_start = first_visible_line_start;

    // Bound the upward scan to a fixed window (mirroring the active-mode fold
    // search) so this never degrades into a full-buffer scan for a viewport
    // sitting deep inside one enormous top-level block.
    for _ in 0..max_upward_lines {
        if line_start == 0 {
            break;
        }
        let prev_line_start = find_line_start_byte(buffer, line_start - 1);
        // Slice the previous line's content *excluding* its trailing `\n` (at
        // `line_start - 1`) so a whitespace-only line is correctly classified
        // as blank rather than as content ending in a newline.
        let content = buffer.slice_bytes(prev_line_start..line_start - 1);
        let (width, all_blank) = slice_indent(&content, tab_size);
        // Each strictly-shallower content line opens an enclosing block; once we
        // reach column 0 the staircase is complete.
        if !all_blank && width < min_indent_seen {
            ancestors_desc.push(width);
            min_indent_seen = width;
            if width == 0 {
                break;
            }
        }
        line_start = prev_line_start;
    }

    ancestors_desc.reverse();
    ancestors_desc
}

/// Fill `out` with the guide columns for a line of indent `width` whose open
/// ancestors (strictly increasing, all `< width`) are `ancestors`. With no
/// visible ancestor the guides fall back to tab stops so a scrolled view still
/// shows guides for its topmost rows.
fn fill_guide_columns(ancestors: &[usize], width: usize, tab_size: usize, out: &mut Vec<usize>) {
    out.clear();
    if ancestors.is_empty() {
        let tab_size = normalized_tab_size(tab_size);
        let mut col = 0;
        while col < width {
            out.push(col);
            col += tab_size;
        }
    } else {
        out.extend_from_slice(ancestors);
        // A guide at column 0 is implied whenever the shallowest visible
        // ancestor is itself indented (its own parent scrolled off-screen).
        if ancestors[0] != 0 {
            out.push(0);
        }
    }
}

/// Append guide glyphs for `guide_columns` lying *beyond* the row's own rendered
/// content (`content_visual_end` is the first visual column past it). The
/// per-cell pass already draws guides on existing leading-whitespace cells; this
/// fills in guide columns that have no cell to restyle — the case for empty rows
/// (no cells at all) and whitespace-only rows narrower than their guides — so the
/// vertical guides run continuously through blank lines. Synthesised cells map to
/// no source byte, exactly like the tail fill. Code rows pass through untouched:
/// their guide columns all sit inside their indent, so none exceed
/// `content_visual_end`.
#[allow(clippy::too_many_arguments)]
fn append_blank_line_guides(
    guide_columns: &[usize],
    content_visual_end: usize,
    left_col: usize,
    content_width: usize,
    glyph: char,
    style: Style,
    rendered_cols: &mut usize,
    line_spans: &mut Vec<Span<'static>>,
    line_view_map: &mut Vec<Option<usize>>,
) {
    // Deepest guide column needing synthesis: past the rendered content and not
    // scrolled off the left edge.
    let Some(last_col) = guide_columns
        .iter()
        .copied()
        .filter(|&c| c >= content_visual_end && c >= left_col)
        .max()
    else {
        return;
    };
    let start = content_visual_end.max(left_col);
    // Clamp to the visible content area so guides never spill past the viewport.
    let end_col = last_col.min(left_col + content_width.saturating_sub(1));
    if end_col < start {
        return;
    }

    let mut text = String::with_capacity(end_col + 1 - start);
    for col in start..=end_col {
        text.push(if guide_columns.contains(&col) {
            glyph
        } else {
            ' '
        });
    }
    push_span_with_map(line_spans, line_view_map, text, style, None);
    *rendered_cols += end_col + 1 - start;
}

pub(crate) fn render_view_lines(input: LineRenderInput<'_>) -> LineRenderOutput {
    use crate::view::folding::indent_folding;

    let LineRenderInput {
        state,
        theme,
        view_lines,
        view_anchor,
        render_area,
        gutter_width,
        selection,
        decorations,
        visible_line_count,
        lsp_waiting,
        is_active,
        line_wrap,
        estimated_lines,
        left_column,
        relative_line_numbers,
        session_mode,
        software_cursor_only,
        show_line_numbers,
        byte_offset_mode,
        show_tilde,
        highlight_current_line,
        indentation_guide,
        indentation_guide_glyph,
        cell_theme_map,
        screen_width,
    } = input;

    prefill_cell_theme_map(cell_theme_map, screen_width, render_area, gutter_width);

    let primary_cursor_position = selection.primary_cursor_position;

    // Compute cursor line start byte — universal key for cursor line highlight
    let cursor_line_start_byte =
        indent_folding::find_line_start_byte(&state.buffer, primary_cursor_position);

    // Exclusive end of the cursor's logical line. A view sub-row whose first
    // source byte falls in `[cursor_line_start_byte, cursor_line_end_byte)`
    // belongs to the same logical line as the cursor — even if a plugin
    // soft-break (compose-mode wrapping) put the sub-row's start mid-line.
    // Without this, the highlight only landed on the *first* visual sub-row
    // of a soft-wrapped paragraph (issue #1790). Computed by direct byte scan
    // so it doesn't depend on the cached `primary_cursor_line_number` being
    // in sync with the cursor position.
    let cursor_line_end_byte =
        indent_folding::find_line_end_byte(&state.buffer, primary_cursor_position);

    let active_indentation_guide = ActiveIndentationGuide::for_view_lines(
        indentation_guide,
        is_active,
        state,
        view_lines,
        primary_cursor_position,
    );
    // Resolve the guide glyph once (single display cell; falls back to `▏`),
    // matching the per-cell pass — used for blank-line guide synthesis.
    let guide_glyph_char = {
        let c = indentation_guide_glyph.trim().chars().next().unwrap_or('▏');
        if crate::primitives::display_width::char_width(c) != 1 {
            '▏'
        } else {
            c
        }
    };
    // `all`-mode guide columns are scanned lazily, one row at a time, as the
    // render loop walks the viewport. The scanner is only built for `all` mode
    // (the default `none` and `active` modes skip it entirely). Its staircase is
    // seeded from the buffer's *absolute* indentation above the first visible
    // row (not from the preceding `view_lines`, which under vertical scroll
    // start at the viewport top and so carry no ancestor context). This keeps a
    // row's guides identical regardless of scroll offset — a block whose opener
    // has scrolled off the top still draws its guide on the interior rows.
    let mut guide_column_scanner = (indentation_guide == IndentationGuideMode::All).then(|| {
        let first_visible_source = view_lines
            .get(view_anchor.start_line_idx..)
            .into_iter()
            .flatten()
            .find_map(|line| line.source_start_byte);
        let stack = match first_visible_source {
            Some(src) => prime_guide_stack_from_buffer(
                &state.buffer,
                indent_folding::find_line_start_byte(&state.buffer, src),
                state.buffer_settings.tab_size,
                crate::config::INDENT_FOLD_MAX_UPWARD_SCAN,
            ),
            None => Vec::new(),
        };
        GuideColumnScanner { stack }
    });

    // Cursors for O(1) amortized span lookups (spans are sorted by byte range)
    let mut span_cursors = SpanCursors::default();
    // Linear-range + block-rect selection sweep. The cell loop just
    // asks `contains(byte_pos, byte_index)` — see SelectionActiveSet.
    let mut selection_sweep = SelectionActiveSet::new(&selection.ranges, &selection.block_rects);

    // Overlay sweep: O(1) amortised per cell, zero allocation per cell.
    // Line-sweep over the viewport overlays. See `OverlayActiveSet`
    // for the contract — this is the per-render-call state machine
    // that knows which overlays cover the byte the cell loop is
    // currently on, and which ones touched the current visual row
    // (fuel for the `extend_to_line_end` tail-fill).
    let mut overlay_sweep = OverlayActiveSet::new(
        &decorations.viewport_overlays,
        &decorations.overlay_position_index,
    );

    let mut lines: Vec<Line<'static>> = Vec::new();
    let mut view_line_mappings: Vec<ViewLineMapping> = Vec::new();
    // Reused across rows so `all`-mode guide-column computation allocates at
    // most once per render (when its capacity first grows).
    let mut guide_columns_buf: Vec<usize> = Vec::new();
    let mut lines_rendered = 0usize;
    let mut view_iter_idx = view_anchor.start_line_idx;
    let mut cursor = CursorTracker::default();
    // ANSI parser state threaded across the soft-wrapped continuation rows
    // of a single logical line. Terminal scrollback stores each terminal
    // line as one *unwrapped* logical line, so a long colored line wraps
    // into several view rows here; without carrying the parser, every row
    // after the first reset to the default style (colors vanished, and an
    // SGR sequence split across the wrap showed up as literal text). Reset
    // at each new logical line below.
    let mut ansi_parser: Option<crate::primitives::ansi::AnsiParser> = None;
    let mut last_line_end: Option<LastLineEnd> = None;
    let mut last_gutter_num: Option<usize> = None;
    let mut trailing_empty_line_rendered = false;
    let mut is_on_cursor_line = false;

    let is_empty_buffer = state.buffer.is_empty();

    // x of the last visible cell on the most recent non-empty row
    // (used for cursor-on-newline placement and `last_line_end`)
    let mut last_visible_x: u16 = 0;

    loop {
        // Get the current ViewLine from the pipeline
        let current_view_line_idx = view_iter_idx;
        let current_view_line = if let Some(vl) = view_lines.get(current_view_line_idx) {
            vl
        } else if is_empty_buffer && lines_rendered == 0 {
            // Handle empty buffer case - create a minimal line
            static EMPTY_LINE: std::sync::OnceLock<ViewLine> = std::sync::OnceLock::new();
            EMPTY_LINE.get_or_init(|| ViewLine {
                text: String::new(),
                source_start_byte: None,
                char_source_bytes: Vec::new(),
                char_styles: Vec::new(),
                char_visual_cols: Vec::new(),
                visual_to_char: Vec::new(),
                tab_starts: HashSet::new(),
                line_start: LineStart::Beginning,
                ends_with_newline: false,
                virtual_gutter_glyph: None,
                virtual_line_style: None,
            })
        } else {
            break;
        };

        // `line_content` borrows the ViewLine's text directly — no per-line
        // `String::clone`; the borrow is valid for the whole per-line body
        // since `current_view_line` is a shared reference into `view_lines`.
        let line_content: &str = &current_view_line.text;
        let line_has_newline = current_view_line.ends_with_newline;
        let line_char_source_bytes = &current_view_line.char_source_bytes;
        let line_start_type = current_view_line.line_start;

        view_iter_idx += 1;

        if lines_rendered >= visible_line_count {
            break;
        }

        // Use the elegant pipeline's should_show_line_number function
        // This correctly handles: injected content, wrapped continuations, and source lines
        let show_line_number = should_show_line_number(current_view_line);

        // is_continuation means "don't show line number" for rendering purposes
        let is_continuation = !show_line_number;

        // Per-line byte offset — universal key for all fold/diagnostic/indicator lookups
        let line_start_byte: Option<usize> = if !is_continuation {
            line_char_source_bytes
                .iter()
                .find_map(|opt| *opt)
                .or_else(|| {
                    // Trailing empty line (after final newline) has no source bytes,
                    // but its logical position is buffer.len() — needed for diagnostic
                    // gutter markers placed at the end of the file.
                    if line_content.is_empty() && line_start_type == LineStart::AfterSourceNewline {
                        Some(state.buffer.len())
                    } else {
                        None
                    }
                })
        } else {
            None
        };

        // Track whether this line is the cursor line (for current line highlighting).
        // Non-continuation lines check their start byte; continuation lines inherit.
        // We use a range check (rather than equality with the logical-line start)
        // so plugin-injected soft-break sub-rows — whose first source byte lands
        // mid-line — are still recognised as belonging to the cursor's logical
        // line (issue #1790).
        if !is_continuation {
            is_on_cursor_line = line_start_byte
                .is_some_and(|b| b >= cursor_line_start_byte && b < cursor_line_end_byte);
        }

        // Gutter display number — line number for small files, byte offset for large files
        let gutter_num = if let Some(byte) = line_start_byte {
            let n = if byte_offset_mode {
                byte
            } else {
                state.buffer.get_line_number(byte)
            };
            last_gutter_num = Some(n);
            n
        } else if !is_continuation {
            // Non-continuation line with no source bytes (trailing empty line
            // produced by ViewLineIterator after final newline).
            // For empty buffers (last_gutter_num is None), show line 0 (displays as "1").
            last_gutter_num.map_or(0, |n| n + 1)
        } else {
            0
        };

        lines_rendered += 1;

        // Screen row this line will occupy (rows already pushed)
        let current_row = lines.len() as u16;

        // Apply horizontal scrolling - skip characters before left_column
        let left_col = left_column;

        let mut line_spans = Vec::new();
        let mut line_view_map: Vec<Option<usize>> = Vec::new();

        // Render left margin (indicators + line numbers + separator)
        render_left_margin(
            &LeftMarginContext {
                state,
                theme,
                is_continuation,
                line_start_byte,
                gutter_num,
                estimated_lines,
                diagnostic_lines: &decorations.diagnostic_lines,
                line_indicators: &decorations.line_indicators,
                fold_indicators: &decorations.fold_indicators,
                cursor_line_start_byte,
                cursor_line_number: state.primary_cursor_line_number.value(),
                relative_line_numbers,
                show_line_numbers,
                byte_offset_mode,
                highlight_current_line,
                is_active,
                virtual_gutter_glyph: current_view_line.virtual_gutter_glyph.as_ref(),
            },
            &mut line_spans,
            &mut line_view_map,
        );

        // Performance optimization: For very long lines, only process visible characters.
        // Calculate the maximum characters we might need to render based on screen width.
        // For wrapped lines, we need enough characters to fill the visible viewport;
        // for non-wrapped lines, we only need one screen width worth.
        let visible_lines_remaining = visible_line_count.saturating_sub(lines_rendered);
        let max_visible_chars = if line_wrap {
            // With wrapping: might need chars for multiple wrapped lines.
            // Be generous to avoid cutting off wrapped content.
            (render_area.width as usize)
                .saturating_mul(visible_lines_remaining.max(1))
                .saturating_add(200)
        } else {
            // Without wrapping: only need one line worth of characters
            (render_area.width as usize).saturating_add(100)
        };
        let max_chars_to_process = left_col.saturating_add(max_visible_chars);

        // A soft-wrap continuation (`AfterBreak`) keeps the running ANSI
        // state so colors persist across the wrap; any other line start
        // begins a fresh logical line, so reset to the default style.
        if line_start_type != LineStart::AfterBreak {
            ansi_parser = None;
        }

        // Advance the `all`-mode guide scanner for this row and capture its
        // guide columns into the reused buffer (empty in `none`/`active` mode).
        if let Some(scanner) = guide_column_scanner.as_mut() {
            let tab_size = state.buffer_settings.tab_size;
            // Blank rows look ahead to the next content row so their guides flow
            // into an upcoming nested body; content rows don't need it.
            let next_indent = match view_line_indent(current_view_line, tab_size) {
                Some((_, false)) => {
                    next_content_indent(view_lines, current_view_line_idx + 1, tab_size)
                }
                _ => None,
            };
            scanner.columns_for(
                current_view_line,
                tab_size,
                next_indent,
                &mut guide_columns_buf,
            );
        } else {
            guide_columns_buf.clear();
        }

        // The single active-mode guide column for this row (None in other
        // modes); reused by the cell pass and the blank-line guide synthesis.
        let active_guide_col =
            active_indentation_guide.and_then(|guide| guide.column_for_line(current_view_line_idx));

        // Per-cell pass: walk the line's characters and emit styled spans
        let cells = render_line_cells(
            CellPassInput {
                state,
                theme,
                view_line: current_view_line,
                selection,
                decorations,
                gutter_num,
                current_row,
                render_area,
                gutter_width,
                screen_width,
                left_col,
                max_chars_to_process,
                lsp_waiting,
                is_active,
                session_mode,
                is_on_cursor_line,
                highlight_current_line,
                indentation_guide,
                indentation_guide_glyph,
                indentation_guide_columns: &guide_columns_buf,
                active_indentation_guide_col: active_guide_col,
            },
            &mut selection_sweep,
            &mut overlay_sweep,
            &mut span_cursors,
            &mut cursor,
            &mut ansi_parser,
            cell_theme_map.as_mut_slice(),
            &mut line_spans,
            &mut line_view_map,
        );
        let mut rendered_cols = cells.rendered_cols;

        if !line_has_newline {
            // The end-of-line cursor can only be placed on rows whose final
            // screen y is already known: empty rows, unwrapped rows, or rows
            // with empty content (wrapped non-empty rows may still grow).
            let seg_y = (line_spans.is_empty() || !line_wrap || line_content.is_empty())
                .then_some(current_row);
            place_line_end_cursor(
                &LineEndCursorInput {
                    view_line: current_view_line,
                    selection,
                    buffer_len: state.buffer.len(),
                    theme,
                    is_active,
                    software_cursor_only,
                    gutter_width,
                    left_col,
                    col_offset: cells.col_offset,
                    seg_y,
                },
                &mut cursor,
                &mut line_spans,
                &mut line_view_map,
            );
        }

        if !line_spans.is_empty() {
            if let Some(x) = locate_cursor_in_view_map(
                &line_view_map,
                primary_cursor_position,
                is_on_cursor_line,
                current_row,
                &mut cursor,
            ) {
                last_visible_x = x;
            }
        }

        let content_width = render_area.width.saturating_sub(gutter_width as u16) as usize;
        let cursor_line_active = is_on_cursor_line && highlight_current_line && is_active;

        // Draw indentation guides *through* this row at guide columns the
        // per-cell pass could not reach — i.e. columns past the row's own
        // leading whitespace. Blank and empty rows have few or no leading-space
        // cells to restyle, so without this their guides would gap out; here the
        // missing guide cells (and the plain spaces between them) are synthesised
        // so guides stay vertically continuous across blank lines. Code rows
        // never trigger this (their guide columns all fall inside their indent).
        let row_can_have_guides = current_view_line.source_start_byte.is_some()
            && !matches!(
                line_start_type,
                LineStart::AfterBreak | LineStart::AfterInjectedNewline
            );
        if row_can_have_guides {
            let synth_columns: &[usize] = match indentation_guide {
                IndentationGuideMode::All => &guide_columns_buf,
                IndentationGuideMode::Active => active_guide_col.as_slice(),
                IndentationGuideMode::None => &[],
            };
            let mut guide_style = Style::default().fg(theme.indentation_guide_fg);
            if cursor_line_active {
                guide_style = guide_style.bg(theme.current_line_bg);
            }
            append_blank_line_guides(
                synth_columns,
                left_col + rendered_cols,
                left_col,
                content_width,
                guide_glyph_char,
                guide_style,
                &mut rendered_cols,
                &mut line_spans,
                &mut line_view_map,
            );
        }

        // Inline diagnostic text: render after line content (before extend_to_line_end fill).
        // Only for non-continuation lines that have a diagnostic overlay.
        if let Some(lsb) = line_start_byte {
            if let Some((message, diag_style)) = decorations.diagnostic_inline_texts.get(&lsb) {
                append_inline_diagnostic(
                    message,
                    diag_style,
                    content_width,
                    cursor_line_active,
                    theme.current_line_bg,
                    &mut rendered_cols,
                    &mut line_spans,
                    &mut line_view_map,
                );
            }
        }

        // Paint trailing columns with the overlay-extend bg, or fall
        // back to the virtual-line bg. See `tail_fill` for the policy.
        let remaining_cols = content_width.saturating_sub(rendered_cols);
        if remaining_cols > 0 {
            if let Some(fill) = resolve_tail_fill(TailFillInput {
                current_view_line,
                theme,
                overlay_fill: overlay_sweep.fill_overlay(),
                syntax_extend_bg: cells.syntax_extend_bg,
                first_line_byte_pos: cells.first_line_byte_pos,
                last_line_byte_pos: cells.last_line_byte_pos,
            }) {
                push_span_with_map(
                    &mut line_spans,
                    &mut line_view_map,
                    " ".repeat(remaining_cols),
                    fill.style,
                    fill.source_byte,
                );
            }
        }

        // Fill remaining width with current_line_bg for cursor line highlighting.
        // Add the span directly (not via push_span_with_map) to avoid extending
        // line_view_map, which would break mouse click byte mapping.
        if cursor_line_active && remaining_cols > 0 {
            line_spans.push(Span::styled(
                " ".repeat(remaining_cols),
                Style::default().bg(theme.current_line_bg),
            ));
        }

        // For virtual rows (no source bytes), inherit from previous row
        let prev_line_end_byte = view_line_mappings
            .last()
            .map(|prev: &ViewLineMapping| prev.line_end_byte)
            .unwrap_or(0);
        view_line_mappings.push(build_view_line_mapping(
            current_view_line,
            &line_view_map,
            gutter_width,
            prev_line_end_byte,
            state.buffer.len(),
        ));

        // Track if line was empty before moving line_spans
        let line_was_empty = line_spans.is_empty();
        lines.push(Line::from(line_spans));

        // Detect the trailing empty ViewLine produced by ViewLineIterator
        // when at_buffer_end is true: empty content, no newline,
        // line_start == AfterSourceNewline.  This is a visual display aid,
        // not an actual content line — don't update last_line_end for it
        // (same policy as the implicit empty line rendered below).
        let is_iterator_trailing_empty = line_content.is_empty()
            && !line_has_newline
            && line_start_type == LineStart::AfterSourceNewline;
        if is_iterator_trailing_empty {
            trailing_empty_line_rendered = true;
        }

        // Update last_line_end and check for cursor on newline BEFORE the break check.
        // This ensures the last visible line's metadata is captured.
        //
        // end_x is the cursor position after the last visible character.
        // For empty lines, last_visible_x stays at 0, so we need to ensure end_x is
        // at least gutter_width to place the cursor after the gutter, not in it.
        let end_x = if line_was_empty {
            gutter_width as u16
        } else {
            last_visible_x.saturating_add(1)
        };
        let line_len_chars = line_content.chars().count();

        // Don't update last_line_end for the iterator's trailing empty
        // line — it's a display aid, not actual content.
        if !is_iterator_trailing_empty {
            last_line_end = Some(LastLineEnd {
                pos: (end_x, current_row),
                terminated_with_newline: line_has_newline,
            });
        }

        if line_has_newline && line_len_chars > 0 {
            let newline_idx = line_len_chars.saturating_sub(1);
            if let Some(Some(src_newline)) = line_char_source_bytes.get(newline_idx) {
                if *src_newline == primary_cursor_position {
                    // Cursor position now includes gutter width (consistent with main cursor tracking).
                    // For empty lines (just newline), cursor should be at gutter width (after gutter);
                    // for lines with content, cursor on newline should be after the content
                    // (end_x already includes the gutter, via last_visible_x).
                    // A virtual-space cursor sits `primary_virtual_cols`
                    // further right of the content end.
                    let vcols = selection.primary_virtual_cols as u16;
                    if line_len_chars == 1 {
                        cursor.force(gutter_width as u16 + vcols, current_row);
                    } else {
                        cursor.force(end_x + vcols, current_row);
                    }
                }
            }
        }

        if lines_rendered >= visible_line_count {
            break;
        }
    }

    // Implicit trailing empty line (when the last content line ended
    // with a newline) and its `ViewLineMapping` fallback — see
    // `trailing::render_implicit_trailing_line` for the contract.
    render_implicit_trailing_line(
        last_line_end.as_ref(),
        &PostRowContext {
            state,
            theme,
            render_area,
            gutter_width,
            decorations,
            cursor_line_start_byte,
            primary_cursor_position,
            primary_virtual_cols: selection.primary_virtual_cols,
            byte_offset_mode,
            show_line_numbers,
            highlight_current_line,
            is_active,
            last_gutter_num,
            visible_line_count,
            trailing_empty_line_rendered,
        },
        &mut PostRowAccumulator {
            lines: &mut lines,
            view_line_mappings: &mut view_line_mappings,
            lines_rendered: &mut lines_rendered,
            cursor_screen_x: &mut cursor.x,
            cursor_screen_y: &mut cursor.y,
            have_cursor: &mut cursor.found,
        },
    );

    // Pad the bottom of the viewport with `~` / after_eof_bg shading.
    fill_eof_rows(&mut lines, theme, render_area, show_tilde);

    LineRenderOutput {
        lines,
        // Clamp to the render area so a far-right virtual-space cursor can't
        // place the hardware cursor into a neighboring split.
        cursor: cursor
            .found
            .then_some((cursor.x.min(render_area.width.saturating_sub(1)), cursor.y)),
        last_line_end,
        content_lines_rendered: lines_rendered,
        view_line_mappings,
    }
}

/// Fill the content area with default gutter/editor theme info so the
/// theme inspector has an answer for cells the per-cell pass never touches.
fn prefill_cell_theme_map(
    cell_theme_map: &mut [crate::app::types::CellThemeInfo],
    screen_width: u16,
    render_area: Rect,
    gutter_width: usize,
) {
    if screen_width == 0 {
        return;
    }
    let gutter_info = crate::app::types::CellThemeInfo {
        fg_key: Some("editor.line_number_fg".into()),
        bg_key: Some("editor.line_number_bg".into()),
        region: "Line Numbers".into(),
        syntax_category: None,
    };
    let content_info = crate::app::types::CellThemeInfo {
        fg_key: Some("editor.fg".into()),
        bg_key: Some("editor.bg".into()),
        region: "Editor Content".into(),
        syntax_category: None,
    };
    let sw = screen_width as usize;
    for row in render_area.y..render_area.y + render_area.height {
        for col in render_area.x..render_area.x + render_area.width {
            let idx = row as usize * sw + col as usize;
            if let Some(cell) = cell_theme_map.get_mut(idx) {
                *cell = if col < render_area.x + gutter_width as u16 {
                    gutter_info.clone()
                } else {
                    content_info.clone()
                };
            }
        }
    }
}

/// Style for the software cursor indicator cell.
fn cursor_indicator_style(theme: &Theme, is_active: bool) -> Style {
    if is_active {
        Style::default()
            .fg(theme.editor_fg)
            .bg(theme.editor_bg)
            .add_modifier(Modifier::REVERSED)
    } else {
        Style::default()
            .fg(theme.editor_fg)
            .bg(theme.inactive_cursor)
    }
}

/// Inputs for the cursor/indicator pass on a line without a trailing newline.
struct LineEndCursorInput<'a> {
    view_line: &'a ViewLine,
    selection: &'a SelectionContext,
    buffer_len: usize,
    theme: &'a Theme,
    is_active: bool,
    software_cursor_only: bool,
    gutter_width: usize,
    left_col: usize,
    /// Visual column after the last processed character.
    col_offset: usize,
    /// Row to place the cursor on, when already known (see call site).
    seg_y: Option<u16>,
}

/// On a line that doesn't end with `\n`, place the cursor when it sits
/// *after* the last character, and append the software cursor indicator
/// when the hardware cursor won't be drawn there.
fn place_line_end_cursor(
    input: &LineEndCursorInput<'_>,
    cursor: &mut CursorTracker,
    line_spans: &mut Vec<Span<'static>>,
    line_view_map: &mut Vec<Option<usize>>,
) {
    let line_content: &str = &input.view_line.text;
    let line_char_source_bytes = &input.view_line.char_source_bytes;
    let cursor_positions = &input.selection.cursor_positions;
    let primary_cursor_position = input.selection.primary_cursor_position;

    let line_len_chars = line_content.chars().count();

    // Map view positions to buffer positions using per-line char_source_bytes
    let last_char_idx = line_len_chars.saturating_sub(1);
    let after_last_char_idx = line_len_chars;

    let last_char_buf_pos = line_char_source_bytes.get(last_char_idx).copied().flatten();
    let after_last_char_buf_pos = line_char_source_bytes
        .get(after_last_char_idx)
        .copied()
        .flatten();

    let expected_after_pos = last_char_buf_pos.map(|p| p + 1).unwrap_or(input.buffer_len);
    let cursor_at_end = cursor_positions.iter().any(|&pos| {
        // Cursor is "at end" only if it's AFTER the last character, not ON it.
        // A cursor ON the last character should render on that character (handled in cell pass).
        let matches_after = after_last_char_buf_pos.is_some_and(|bp| pos == bp);
        // Fallback: when there's no mapping after last char (EOF), check if cursor is after last char.
        // The fallback should match the position that would be "after" if there was a mapping.
        // For empty lines with no source mappings (e.g. trailing empty line after final '\n'),
        // the expected position is buffer.len() (EOF), not 0.
        let matches_fallback = after_last_char_buf_pos.is_none() && pos == expected_after_pos;

        matches_after || matches_fallback
    });
    if !cursor_at_end {
        return;
    }

    // Primary cursor is at end only if AFTER the last char, not ON it
    let is_primary_at_end = after_last_char_buf_pos.is_some_and(|bp| bp == primary_cursor_position)
        || (after_last_char_buf_pos.is_none() && primary_cursor_position >= input.buffer_len);

    // Virtual space: columns past the content end that the end-of-line
    // cursor(s) sit at. Shifts the hardware cursor and pads the software
    // indicator out to the cursor's real on-screen column.
    let end_byte = after_last_char_buf_pos.unwrap_or(expected_after_pos);
    let virtual_pad = input
        .selection
        .virtual_cols_at
        .get(&end_byte)
        .copied()
        .unwrap_or(0);

    // Track cursor position for primary cursor
    if let Some(seg_y) = input.seg_y {
        if is_primary_at_end {
            // Cursor position includes gutter width (consistent with main cursor tracking).
            // For empty lines, cursor is at gutter width (right after gutter);
            // for non-empty lines without newline, cursor is after the last visible character.
            let x = if line_len_chars == 0 {
                input.gutter_width as u16
            } else {
                // col_offset is the visual column after the last character.
                // Subtract left_col to get the screen position after horizontal scroll.
                input.gutter_width as u16 + input.col_offset.saturating_sub(input.left_col) as u16
            };
            cursor.force(x + input.selection.primary_virtual_cols as u16, seg_y);
        }
    }

    // When software_cursor_only, always add the indicator space because
    // the backend does not render a hardware cursor.  In terminal mode,
    // the primary cursor at end-of-line relies on the hardware cursor.
    let should_add_indicator = if input.is_active {
        input.software_cursor_only || !is_primary_at_end
    } else {
        true
    };
    if should_add_indicator {
        if virtual_pad > 0 {
            push_span_with_map(
                line_spans,
                line_view_map,
                " ".repeat(virtual_pad),
                Style::default(),
                None,
            );
        }
        push_span_with_map(
            line_spans,
            line_view_map,
            " ".to_string(),
            cursor_indicator_style(input.theme, input.is_active),
            None,
        );
    }
}

/// Scan a rendered line's view map for the primary cursor and the line's
/// last visible cell.
///
/// When the cursor byte falls inside a concealed range (e.g. syntax markers
/// hidden by compose-mode plugins), no view-map entry will exactly match
/// `primary_cursor_position`.  In that case we fall back to the nearest
/// visible byte that is >= the cursor byte on the same line — this keeps
/// the cursor visible for the one frame between cursor movement and the
/// plugin's conceal-refresh response.
///
/// The fallback is gated by `is_on_cursor_line` so that lines below the
/// cursor don't snap a phantom cursor onto themselves when the cursor's
/// own line is offscreen (issue #1965: mouse-wheel scroll past the
/// cursor drew a phantom cursor at the top of the new viewport).
///
/// Returns the x of the last visible cell, if any.
fn locate_cursor_in_view_map(
    line_view_map: &[Option<usize>],
    primary_cursor_position: usize,
    is_on_cursor_line: bool,
    current_row: u16,
    cursor: &mut CursorTracker,
) -> Option<u16> {
    let mut nearest_fallback: Option<(u16, usize)> = None; // (screen_x, byte_distance)
    let mut last_visible_x: Option<u16> = None;
    for (screen_x, source_offset) in line_view_map.iter().enumerate() {
        if let Some(src) = source_offset {
            // Exact match: cursor byte is visible
            if *src == primary_cursor_position {
                cursor.place(screen_x as u16, current_row);
            }
            // Track nearest visible byte >= cursor position for fallback
            if !cursor.found && is_on_cursor_line && *src >= primary_cursor_position {
                let dist = *src - primary_cursor_position;
                if nearest_fallback.is_none_or(|(_, best)| dist < best) {
                    nearest_fallback = Some((screen_x as u16, dist));
                }
            }
            last_visible_x = Some(screen_x as u16);
        }
    }
    // Fallback: cursor byte was concealed — snap to nearest visible byte
    if let Some((fallback_x, _)) = nearest_fallback {
        cursor.place(fallback_x, current_row);
    }
    last_visible_x
}

/// Right-align an inline diagnostic message after the line's content.
/// No-op when there isn't room for a meaningful amount of text.
#[allow(clippy::too_many_arguments)]
fn append_inline_diagnostic(
    message: &str,
    diag_style: &Style,
    content_width: usize,
    cursor_line_active: bool,
    current_line_bg: Color,
    rendered_cols: &mut usize,
    line_spans: &mut Vec<Span<'static>>,
    line_view_map: &mut Vec<Option<usize>>,
) {
    let available = content_width.saturating_sub(*rendered_cols);
    let gap = 2usize;
    let min_text = 10usize;
    if available <= gap + min_text {
        return;
    }

    // Truncate message to fit
    let max_chars = available - gap;
    let display: String = if message.chars().count() > max_chars {
        let truncated: String = message.chars().take(max_chars.saturating_sub(1)).collect();
        format!("{}…", truncated)
    } else {
        message.to_string()
    };
    let display_width = display.chars().count();

    // Right-align: fill gap between code and diagnostic text
    let padding = available.saturating_sub(display_width);
    if padding > 0 {
        let pad_style = if cursor_line_active {
            Style::default().bg(current_line_bg)
        } else {
            Style::default()
        };
        push_span_with_map(
            line_spans,
            line_view_map,
            " ".repeat(padding),
            pad_style,
            None,
        );
        *rendered_cols += padding;
    }

    // Apply current line background to diagnostic text when on cursor line
    let effective_diag_style = if cursor_line_active && diag_style.bg.is_none() {
        diag_style.bg(current_line_bg)
    } else {
        *diag_style
    };
    push_span_with_map(
        line_spans,
        line_view_map,
        display,
        effective_diag_style,
        None,
    );
    *rendered_cols += display_width;
}

/// Build the mouse-click/cursor-movement mapping for a rendered row.
fn build_view_line_mapping(
    view_line: &ViewLine,
    line_view_map: &[Option<usize>],
    gutter_width: usize,
    prev_line_end_byte: usize,
    buffer_len: usize,
) -> ViewLineMapping {
    let line_end_byte = if view_line.ends_with_newline {
        // Position ON the newline - find the last source byte (the newline's position)
        view_line
            .char_source_bytes
            .iter()
            .rev()
            .find_map(|m| *m)
            .unwrap_or(prev_line_end_byte)
    } else if let Some((char_idx, &Some(last_byte_start))) = view_line
        .char_source_bytes
        .iter()
        .enumerate()
        .rev()
        .find(|(_, m)| m.is_some())
    {
        // Position AFTER the last character - find last source byte and add char length
        if let Some(last_char) = view_line.text.chars().nth(char_idx) {
            last_byte_start + last_char.len_utf8()
        } else {
            last_byte_start
        }
    } else if matches!(view_line.line_start, LineStart::AfterSourceNewline)
        && prev_line_end_byte + 2 >= buffer_len
    {
        // Trailing empty line after the final source newline.
        // The cursor on this line lives at buffer_len.
        buffer_len
    } else {
        // Virtual row with no source bytes (e.g. table border from conceals).
        // Inherit line_end_byte from the previous row so cursor movement
        // through virtual rows lands at a valid source position.
        prev_line_end_byte
    };

    // Content mapping starts after the gutter
    let content_map = if line_view_map.len() >= gutter_width {
        line_view_map[gutter_width..].to_vec()
    } else {
        Vec::new()
    };

    // Mark plugin-injected virtual rows so `move_visual_line` can
    // skip them.  Both the first row (AfterInjectedNewline) and any
    // wrap continuations (AfterBreak whose content has no source
    // bytes) belong to the virtual line.
    let is_plugin_virtual = matches!(view_line.line_start, LineStart::AfterInjectedNewline)
        || (matches!(view_line.line_start, LineStart::AfterBreak)
            && !view_line.char_source_bytes.iter().any(|b| b.is_some()));

    ViewLineMapping {
        visual_to_char: (0..content_map.len()).collect(),
        char_source_bytes: content_map,
        line_end_byte,
        is_plugin_virtual,
    }
}
