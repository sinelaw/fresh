//! Per-cell rendering pass: the inner character loop of `render_view_lines`.
//!
//! Walks one `ViewLine`'s characters and emits styled spans plus the
//! per-cell bookkeeping later passes need (cursor hits, rendered width,
//! extend-to-line-end fill inputs). Pulled out of `render_view_lines` so
//! the orchestrator reads as a sequence of per-line passes instead of one
//! interleaved loop.
//!
//! `CellPass` is the per-line state machine: `render_line_cells` builds
//! one, feeds it every character, and collects the `CellPassOutput`.
//! Each per-cell concern (cursor detection, style resolution, display
//! character, span emission, position bookkeeping) is its own method.

use super::super::super::char_style::{compute_char_style, CharStyleContext, CharStyleOutput};
use super::super::super::spans::{
    push_debug_tag, push_span_with_map, span_bg_info_at, span_color_at, span_info_at,
    DebugSpanTracker, SpanAccumulator,
};
use super::super::contexts::{DecorationContext, SelectionContext};
use super::super::overlay_sweep::OverlayActiveSet;
use super::super::selection_sweep::SelectionActiveSet;
use super::{cursor_indicator_style, CursorTracker, SpanCursors};
use crate::app::types::CellThemeInfo;
use crate::config::IndentationGuideMode;
use crate::primitives::ansi::AnsiParser;
use crate::primitives::display_width::char_width;
use crate::state::EditorState;
use crate::view::overlay::Overlay;
use crate::view::theme::Theme;
use crate::view::ui::view_pipeline::{LineStart, ViewLine};
use ratatui::layout::Rect;
use ratatui::style::{Color, Style};
use ratatui::text::Span;
use std::ops::ControlFlow;

/// Read-only inputs for one line's cell pass.
pub(super) struct CellPassInput<'a, 'c> {
    pub state: &'a EditorState,
    pub theme: &'a Theme,
    pub view_line: &'a ViewLine,
    pub selection: &'a SelectionContext,
    pub decorations: &'a DecorationContext,
    /// Gutter display number for this line (for the block-selection sweep).
    pub gutter_num: usize,
    /// Screen row this line will occupy (rows already pushed).
    pub current_row: u16,
    pub render_area: Rect,
    pub gutter_width: usize,
    /// Screen width for `cell_theme_map` indexing (0 disables recording).
    pub screen_width: u16,
    /// Left column offset for horizontal scrolling.
    pub left_col: usize,
    /// Cap on processed visual columns — keeps very long lines cheap.
    pub max_chars_to_process: usize,
    pub lsp_waiting: bool,
    pub is_active: bool,
    /// Skip REVERSED style on the primary cursor (session mode or
    /// non-block cursor style).
    pub session_mode: bool,
    pub is_on_cursor_line: bool,
    pub highlight_current_line: bool,
    pub indentation_guide: IndentationGuideMode,
    pub indentation_guide_glyph: &'a str,
    pub indentation_guide_columns: &'c [usize],
    /// In active mode, the one guide column to draw for this line when it is
    /// inside the active cursor's indentation block.
    pub active_indentation_guide_col: Option<usize>,
}

/// Per-line results the later passes consume.
pub(super) struct CellPassOutput {
    /// Cells that landed on screen — fills extend from here to the
    /// viewport's right edge, so this stays correct under horizontal
    /// scroll (unlike a raw character count).
    pub rendered_cols: usize,
    /// Visual column after the last processed character.
    pub col_offset: usize,
    /// First/last source bytes seen on this row (tail-fill inputs).
    pub first_line_byte_pos: Option<usize>,
    pub last_line_byte_pos: Option<usize>,
    /// Row-wide bg carried by a syntax category whose
    /// `bg_extends_to_line_end()` is true (diff Inserted / Deleted /
    /// Changed). Picked up by the tail-fill pass so the bg wash
    /// continues past the scoped text to the viewport's right edge.
    pub syntax_extend_bg: Option<Color>,
}

/// Render one line's characters into `line_spans` / `line_view_map`.
///
/// Owns everything that happens per cell: ANSI parsing, selection and
/// overlay sweeps, syntax/semantic span lookups, whitespace indicators,
/// debug "reveal codes", software-cursor hits, and the theme-inspector
/// cell map.
#[allow(clippy::too_many_arguments)]
pub(super) fn render_line_cells<'a, 'c>(
    input: CellPassInput<'a, 'c>,
    selection_sweep: &mut SelectionActiveSet<'a>,
    overlay_sweep: &mut OverlayActiveSet<'a>,
    span_cursors: &mut SpanCursors,
    cursor: &mut CursorTracker,
    ansi_parser: &mut Option<AnsiParser>,
    cell_theme_map: &mut [CellThemeInfo],
    line_spans: &mut Vec<Span<'static>>,
    line_view_map: &mut Vec<Option<usize>>,
) -> CellPassOutput {
    let line_content: &'a str = &input.view_line.text;

    // The ANSI parser is threaded across the soft-wrapped rows of one
    // logical line (the caller resets it to `None` at each new logical
    // line). Create it lazily the first time a row carries ESC so that
    // colors — and a multi-byte SGR sequence split across a wrap
    // boundary — continue onto the wrapped continuation rows instead of
    // resetting to the default style at every row. Rows whose logical
    // line never contains ESC keep the `None` fast path.
    if ansi_parser.is_none() && line_content.contains('\x1b') {
        *ansi_parser = Some(AnsiParser::new());
    }

    // Reset the per-row touched set. Wrap continuations inherit overlays
    // still active from the previous row of the same source line; new
    // source lines do not (see OverlayActiveSet).
    overlay_sweep.enter_row(matches!(input.view_line.line_start, LineStart::AfterBreak));

    let mut pass = CellPass {
        // ANSI parser threaded from the caller across wrapped rows.
        ansi_parser,
        // Debug mode: track active highlight/overlay spans for
        // WordPerfect-style reveal codes.
        debug_tracker: input
            .state
            .debug_highlight_mode
            .then(DebugSpanTracker::default),
        non_ws: non_ws_bounds(line_content),
        line_total_visual_width: input.view_line.visual_width(),
        input,
        selection_sweep,
        overlay_sweep,
        span_cursors,
        cursor,
        cell_theme_map,
        line_spans,
        line_view_map,
        span_acc: SpanAccumulator::new(),
        byte_index: 0,
        display_char_idx: 0,
        col_offset: 0,
        visible_char_count: 0,
        rendered_cols: 0,
        first_line_byte_pos: None,
        last_line_byte_pos: None,
        syntax_extend_bg: None,
    };

    for ch in line_content.chars() {
        if pass.process_char(ch).is_break() {
            break;
        }
    }
    pass.finish()
}

/// Per-line state for the cell pass. One instance per view line; the
/// `_sweep` / `span_cursors` / `cursor` borrows carry state *across*
/// lines, everything else is reset per line.
struct CellPass<'a, 'b, 'c> {
    input: CellPassInput<'a, 'c>,
    selection_sweep: &'b mut SelectionActiveSet<'a>,
    overlay_sweep: &'b mut OverlayActiveSet<'a>,
    span_cursors: &'b mut SpanCursors,
    cursor: &'b mut CursorTracker,
    cell_theme_map: &'b mut [CellThemeInfo],
    line_spans: &'b mut Vec<Span<'static>>,
    line_view_map: &'b mut Vec<Option<usize>>,

    /// Merges consecutive characters with the same style — critical for
    /// proper rendering of combining characters (Thai, etc.)
    span_acc: SpanAccumulator,
    ansi_parser: &'b mut Option<AnsiParser>,
    debug_tracker: Option<DebugSpanTracker>,
    /// First/last non-whitespace char indices (whitespace indicators).
    non_ws: (Option<usize>, Option<usize>),
    line_total_visual_width: usize,

    /// Byte offset in the line's text.
    byte_index: usize,
    /// Character index in the line's text (indexes char_source_bytes).
    display_char_idx: usize,
    /// Visual column position.
    col_offset: usize,
    /// All visual columns stepped over (for the long-line break check).
    visible_char_count: usize,
    /// Visual columns that landed on screen.
    rendered_cols: usize,
    first_line_byte_pos: Option<usize>,
    last_line_byte_pos: Option<usize>,
    syntax_extend_bg: Option<Color>,
}

/// Resolved style and theme-inspector metadata for one cell.
struct ResolvedCellStyle {
    style: Style,
    is_secondary_cursor: bool,
    fg_theme_key: Option<&'static str>,
    bg_theme_key: Option<&'static str>,
    region: &'static str,
    /// Syntax category display name (theme inspector).
    syntax_category: Option<&'static str>,
}

impl CellPass<'_, '_, '_> {
    /// Process one character; `Break` when the long-line cap is reached.
    fn process_char(&mut self, ch: char) -> ControlFlow<()> {
        // Source byte for this character, via character index
        // (char_source_bytes is indexed by character position, not visual column)
        let byte_pos = self
            .input
            .view_line
            .char_source_bytes
            .get(self.display_char_idx)
            .copied()
            .flatten();

        if let Some(bp) = byte_pos {
            // Track byte positions for extend_to_line_end
            if self.first_line_byte_pos.is_none() {
                self.first_line_byte_pos = Some(bp);
            }
            self.last_line_byte_pos = Some(bp);

            // Advance overlay active-set sweep for this cell. Monotonic
            // in `bp` across all view lines in this render call.
            self.overlay_sweep.advance_to(bp);
        }

        // Process character through ANSI parser first (if line has ANSI).
        // `None` means the character is part of an escape sequence: skip it.
        let Some(ansi_style) = self.parse_ansi(ch) else {
            // ANSI escape chars have zero visual width, so don't advance
            // col_offset. IMPORTANT: if the cursor is on this ANSI byte,
            // track it.
            if byte_pos == Some(self.input.selection.primary_cursor_position) {
                self.cursor
                    .place(self.cell_screen_x(), self.input.current_row);
            }
            self.byte_index += ch.len_utf8();
            self.display_char_idx += 1;
            return ControlFlow::Continue(());
        };

        // Performance: skip expensive style calculations for characters beyond
        // the visible range. Use visible_char_count (not byte_index) since ANSI
        // codes don't take up visible space. This is critical for performance
        // with very long lines (e.g., 100KB single line).
        if self.visible_char_count > self.input.max_chars_to_process {
            return ControlFlow::Break(());
        }

        // Skip characters horizontally scrolled out on the left
        if self.col_offset >= self.input.left_col {
            self.render_visible_cell(ch, byte_pos, ansi_style);
        }

        self.advance(ch);
        ControlFlow::Continue(())
    }

    /// Style and emit one on-screen cell.
    fn render_visible_cell(&mut self, ch: char, byte_pos: Option<usize>, ansi_style: Style) {
        // Is this view position the START of a tab expansion?
        let is_tab_start = self.input.view_line.tab_starts.contains(&self.col_offset);
        let is_cursor = self.cursor_hits_cell(byte_pos);

        // Refresh the block-rect active set for this row.
        // Idempotent on the same gutter line (no inner gate).
        self.selection_sweep.enter_line(self.input.gutter_num);

        // For primary cursor in active split, terminal hardware cursor provides
        // visual indication, so we can still show selection background.
        // Only exclude secondary cursors from selection (they use REVERSED styling).
        // Bug #614: Previously excluded all cursor positions, causing first char
        // of selection to display with wrong background for bar/underline cursors.
        let is_primary_cursor =
            is_cursor && byte_pos == Some(self.input.selection.primary_cursor_position);
        let exclude_from_selection = is_cursor && !(self.input.is_active && is_primary_cursor);
        let is_selected =
            !exclude_from_selection && self.selection_sweep.contains(byte_pos, self.byte_index);

        let resolved = self.resolve_cell_style(byte_pos, ansi_style, is_cursor, is_selected);
        self.record_cell_theme(&resolved);

        // `indicator_buf` holds the UTF-8 bytes of a single fallback indicator
        // char on the stack — no heap allocation per cell.
        let mut indicator_buf = [0u8; 4];
        let is_lsp_cursor = is_cursor && self.input.lsp_waiting && self.input.is_active;
        let is_indentation_guide = !is_lsp_cursor && self.is_indentation_guide_cell(ch, byte_pos);
        let (display_char, is_whitespace_indicator) = if is_indentation_guide {
            let guide_char = self
                .input
                .indentation_guide_glyph
                .trim()
                .chars()
                .next()
                .unwrap_or('▏');
            let guide_glyph: &str = if char_width(guide_char) != 1 {
                '▏'.encode_utf8(&mut indicator_buf)
            } else {
                guide_char.encode_utf8(&mut indicator_buf)
            };
            (guide_glyph, false)
        } else {
            self.display_cell_text(ch, is_cursor, is_tab_start, &mut indicator_buf)
        };

        // Apply subdued indicator colors from theme. Cursor styling keeps
        // precedence (so guides do not obscure the caret), but selection does
        // not: a guide keeps its subdued foreground even inside a selection,
        // layered over the selection background carried by `resolved.style`.
        // This stops the guide glyph from lighting up to full-contrast text
        // (which read as a literal glyph) when the leading whitespace is
        // selected. Whitespace indicators still defer to selection styling.
        let mut style = resolved.style;
        if is_indentation_guide && !is_cursor {
            style = style.fg(self.input.theme.indentation_guide_fg);
        } else if is_whitespace_indicator && !is_cursor && !is_selected {
            style = style.fg(self.input.theme.whitespace_indicator_fg);
        }

        if !display_char.is_empty() {
            self.emit_cell(display_char, style, byte_pos, ch);
        }

        self.place_cell_cursor(ch, byte_pos, is_cursor, resolved.is_secondary_cursor);
    }

    /// Whether the current leading-whitespace cell should render as an
    /// indentation guide. Guides are visual-only replacements for leading
    /// whitespace cells, so they preserve byte mappings and do not draw through
    /// code or injected content. On a normal row that whitespace is real source
    /// text; on a soft-wrap continuation row it is the synthetic wrap-indent
    /// padding (which maps to no source byte), so guides keep running through the
    /// padding and the staircase stays unbroken across the wrap.
    fn is_indentation_guide_cell(&self, ch: char, byte_pos: Option<usize>) -> bool {
        if matches!(self.input.indentation_guide, IndentationGuideMode::None) || ch != ' ' {
            return false;
        }

        // Plugin-injected continuation rows never carry guides.
        if matches!(
            self.input.view_line.line_start,
            LineStart::AfterInjectedNewline
        ) {
            return false;
        }

        // A soft-wrap continuation's leading cells are byte-less wrap-indent
        // padding; every other row's are real source whitespace. Requiring the
        // matching byte-mapping for each case keeps guides off injected/virtual
        // content on normal rows and off the real wrapped text on continuations.
        let is_wrap_continuation = matches!(self.input.view_line.line_start, LineStart::AfterBreak);
        if byte_pos.is_none() != is_wrap_continuation {
            return false;
        }

        if self.input.view_line.source_start_byte.is_none() {
            return false;
        }

        if !self.is_leading_indent_cell() {
            return false;
        }

        match self.input.indentation_guide {
            IndentationGuideMode::None => false,
            IndentationGuideMode::All => self
                .input
                .indentation_guide_columns
                .contains(&self.col_offset),
            IndentationGuideMode::Active => {
                self.input.active_indentation_guide_col == Some(self.col_offset)
            }
        }
    }

    fn is_leading_indent_cell(&self) -> bool {
        match self.non_ws {
            (Some(first), _) => self.display_char_idx < first,
            // All-whitespace lines: every rendered space is leading
            // indentation. Newline characters are excluded by the caller's
            // `ch == ' '` check.
            _ => true,
        }
    }

    /// Whether a cursor should render on this cell.
    ///
    /// For tab expansions, only the FIRST space (the tab_start position)
    /// shows the cursor — this prevents it from appearing on all 8
    /// expanded spaces.
    fn cursor_hits_cell(&self, byte_pos: Option<usize>) -> bool {
        let Some(bp) = byte_pos else {
            return false;
        };
        if !self.input.selection.cursor_positions.contains(&bp)
            || bp >= self.input.state.buffer.len()
        {
            return false;
        }
        // Detect tab expansion by checking whether the previous char maps
        // to the same source byte. Show cursor if this is the start of the
        // line, OR the previous char had a different byte pos.
        let prev_char_idx = self.display_char_idx.saturating_sub(1);
        let prev_byte_pos = self
            .input
            .view_line
            .char_source_bytes
            .get(prev_char_idx)
            .copied()
            .flatten();
        self.display_char_idx == 0 || prev_byte_pos != Some(bp)
    }

    /// Layer token / ANSI / syntax / semantic / overlay / selection /
    /// cursor styling for one cell, and remember any row-wide diff bg.
    fn resolve_cell_style(
        &mut self,
        byte_pos: Option<usize>,
        ansi_style: Style,
        is_cursor: bool,
        is_selected: bool,
    ) -> ResolvedCellStyle {
        let input = &self.input;
        let highlight_spans = &input.decorations.highlight_spans;

        // char_styles is indexed by character position, not visual column
        let token_style = input
            .view_line
            .char_styles
            .get(self.display_char_idx)
            .and_then(|s| s.as_ref());

        // Resolve highlight/semantic colors via cursor-based O(1) lookup
        let (highlight_color, highlight_theme_key, highlight_display_name) = match byte_pos {
            Some(bp) => span_info_at(highlight_spans, &mut self.span_cursors.highlight, bp),
            None => (None, None, None),
        };
        // Diff categories carry a bg the renderer paints as a row wash.
        // `span_bg_info_at` is an O(1) peek using the cursor
        // `span_info_at` just advanced; no second walk.
        let (highlight_bg, highlight_bg_extends) = match byte_pos {
            Some(bp) => span_bg_info_at(highlight_spans, self.span_cursors.highlight, bp),
            None => (None, false),
        };
        let highlight_bg_theme_key = highlight_bg
            .and(highlight_theme_key)
            .or(highlight_theme_key);
        let semantic_token_color = match byte_pos {
            Some(bp) => span_color_at(
                &input.decorations.semantic_token_spans,
                &mut self.span_cursors.semantic,
                bp,
            ),
            None => None,
        };

        // Pre-resolved active overlays for this cell. Empty slice when
        // byte_pos is None (ANSI continuation / virtual cells) — matches
        // pre-sweep behaviour where `bp = None` short-circuited overlay
        // filtering.
        let cell_overlays: &[&Overlay] = if byte_pos.is_some() {
            self.overlay_sweep.at_cursor()
        } else {
            &[]
        };

        let CharStyleOutput {
            style,
            is_secondary_cursor,
            fg_theme_key,
            bg_theme_key,
            region,
        } = compute_char_style(&CharStyleContext {
            byte_pos,
            token_style,
            ansi_style,
            is_cursor,
            is_selected,
            theme: input.theme,
            highlight_color,
            highlight_theme_key,
            highlight_bg,
            highlight_bg_theme_key,
            semantic_token_color,
            active_overlays: cell_overlays,
            primary_cursor_position: input.selection.primary_cursor_position,
            is_active: input.is_active,
            skip_primary_cursor_reverse: input.session_mode,
            is_cursor_line_highlighted: input.is_on_cursor_line
                && input.highlight_current_line
                && input.is_active,
            current_line_bg: input.theme.current_line_bg,
        });

        // Remember this row's diff bg so the tail-fill pass can continue
        // the wash past the scoped text. Only set when the category
        // actually wants extension — keeps per-token bg scopes (none
        // today, but possible) from unintentionally bleeding to the
        // row's right edge.
        if let (Some(bg), true) = (highlight_bg, highlight_bg_extends) {
            self.syntax_extend_bg = Some(bg);
        }

        ResolvedCellStyle {
            style,
            is_secondary_cursor,
            fg_theme_key,
            bg_theme_key,
            region,
            syntax_category: highlight_display_name,
        }
    }

    /// Record cell theme info for the theme inspector popup.
    fn record_cell_theme(&mut self, resolved: &ResolvedCellStyle) {
        if self.input.screen_width == 0 {
            return;
        }
        let screen_col = self.input.render_area.x + self.cell_screen_x();
        let screen_row = self.input.render_area.y + self.input.current_row;
        let idx = screen_row as usize * self.input.screen_width as usize + screen_col as usize;
        if let Some(cell) = self.cell_theme_map.get_mut(idx) {
            *cell = CellThemeInfo {
                fg_key: resolved.fg_theme_key.map(std::borrow::Cow::Borrowed),
                bg_key: resolved.bg_theme_key.map(std::borrow::Cow::Borrowed),
                region: std::borrow::Cow::Borrowed(resolved.region),
                syntax_category: resolved.syntax_category.map(std::borrow::Cow::Borrowed),
            };
        }
    }

    /// What to draw for this character: the char itself, a whitespace
    /// indicator (→ / ·), an LSP-waiting marker, a debug escape, or
    /// nothing (newline). Tabs are already expanded by ViewLineIterator.
    fn display_cell_text<'buf>(
        &self,
        ch: char,
        is_cursor: bool,
        is_tab_start: bool,
        indicator_buf: &'buf mut [u8; 4],
    ) -> (&'buf str, bool) {
        let ws = &self.input.state.buffer_settings.whitespace;
        let ws_show_tab = is_tab_start
            && ws_indicator_visible(
                self.display_char_idx,
                self.non_ws,
                ws.tabs_leading,
                ws.tabs_inner,
                ws.tabs_trailing,
            );
        let ws_show_space = ch == ' '
            && !is_tab_start
            && ws_indicator_visible(
                self.display_char_idx,
                self.non_ws,
                ws.spaces_leading,
                ws.spaces_inner,
                ws.spaces_trailing,
            );

        if is_cursor && self.input.lsp_waiting && self.input.is_active {
            ("⋯", false)
        } else if self.debug_tracker.is_some() && ch == '\r' {
            // Debug mode: show CR explicitly
            ("\\r", false)
        } else if self.debug_tracker.is_some() && ch == '\n' {
            // Debug mode: show LF explicitly
            ("\\n", false)
        } else if ch == '\n' {
            ("", false)
        } else if ws_show_tab {
            // Visual indicator for tab: show → at the first position
            ('→'.encode_utf8(indicator_buf), true)
        } else if ws_show_space {
            // Visual indicator for space: show · when enabled
            ('·'.encode_utf8(indicator_buf), true)
        } else {
            (ch.encode_utf8(indicator_buf), false)
        }
    }

    /// Push the cell's text through the span accumulator, wrapped in
    /// debug reveal-code tags when debug mode is on.
    fn emit_cell(&mut self, display_char: &str, style: Style, byte_pos: Option<usize>, ch: char) {
        // Debug mode: insert opening tags for spans starting at this
        // position, then the byte position before the character.
        if let Some(ref mut tracker) = self.debug_tracker {
            // Flush before debug tags
            self.span_acc.flush(self.line_spans, self.line_view_map);
            let opening_tags = tracker.get_opening_tags(
                byte_pos,
                &self.input.decorations.highlight_spans,
                &self.input.decorations.viewport_overlays,
            );
            for tag in opening_tags {
                push_debug_tag(self.line_spans, self.line_view_map, tag);
            }
            if let Some(bp) = byte_pos {
                push_debug_tag(self.line_spans, self.line_view_map, format!("[{}]", bp));
            }
        }

        for c in display_char.chars() {
            self.span_acc
                .push(c, style, byte_pos, self.line_spans, self.line_view_map);
        }

        // Debug mode: insert closing tags for spans ending at this position.
        // Check using the NEXT byte position to see if we're leaving a span.
        if let Some(ref mut tracker) = self.debug_tracker {
            // Flush before debug tags
            self.span_acc.flush(self.line_spans, self.line_view_map);
            let next_byte_pos = byte_pos.map(|bp| bp + ch.len_utf8());
            for tag in tracker.get_closing_tags(next_byte_pos) {
                push_debug_tag(self.line_spans, self.line_view_map, tag);
            }
        }
    }

    /// Cursor handling that happens after the cell is emitted: zero-width
    /// characters (which get no view-map entry) and the indicator drawn
    /// when a cursor sits on a newline.
    fn place_cell_cursor(
        &mut self,
        ch: char,
        byte_pos: Option<usize>,
        is_cursor: bool,
        is_secondary_cursor: bool,
    ) {
        // Zero-width chars don't get map entries, so explicitly record
        // the cursor position here.
        if byte_pos == Some(self.input.selection.primary_cursor_position) && char_width(ch) == 0 {
            self.cursor
                .place(self.cell_screen_x(), self.input.current_row);
        }

        if is_cursor && ch == '\n' {
            let should_add_indicator = if self.input.is_active {
                is_secondary_cursor
            } else {
                true
            };
            if should_add_indicator {
                // Flush accumulated text before adding the cursor indicator
                // so the indicator appears after the line content, not before
                self.span_acc.flush(self.line_spans, self.line_view_map);
                // A virtual-space cursor sits past the content end: pad the
                // indicator out to its on-screen column.
                let virtual_pad = byte_pos
                    .and_then(|bp| self.input.selection.virtual_cols_at.get(&bp))
                    .copied()
                    .unwrap_or(0);
                if virtual_pad > 0 {
                    push_span_with_map(
                        self.line_spans,
                        self.line_view_map,
                        " ".repeat(virtual_pad),
                        Style::default(),
                        None,
                    );
                }
                push_span_with_map(
                    self.line_spans,
                    self.line_view_map,
                    " ".to_string(),
                    cursor_indicator_style(self.input.theme, self.input.is_active),
                    byte_pos,
                );
            }
        }
    }

    /// Step position bookkeeping past `ch`.
    fn advance(&mut self, ch: char) {
        self.byte_index += ch.len_utf8();
        self.display_char_idx += 1;

        // col_offset tracks visual column position (for indexing into visual_to_char).
        // We read the per-char visual column that view_pipeline assigned so that
        // grapheme clusters (ZWJ emoji, base+combining, etc.) advance by
        // `UnicodeWidthStr::width(cluster)` — the same width ratatui uses when
        // re-segmenting spans — instead of summing per-codepoint `char_width`.
        // Without this, the renderer's col_offset diverges from the view
        // pipeline's for any cluster whose str_width ≠ Σ char_width, producing
        // variable-width rendering corruption (issue #1577).
        let next_col_for_char = self
            .input
            .view_line
            .char_visual_cols
            .get(self.display_char_idx)
            .copied()
            .unwrap_or(self.line_total_visual_width);
        let ch_width = next_col_for_char.saturating_sub(self.col_offset);
        // `\n` gets visual width 1 from the view pipeline but renders as
        // empty — don't count it as an on-screen cell.
        let was_rendered = self.col_offset >= self.input.left_col && ch != '\n';
        self.col_offset = next_col_for_char;
        self.visible_char_count += ch_width;
        if was_rendered {
            self.rendered_cols += ch_width;
        }
    }

    /// Run `ch` through the line's ANSI parser; `None` means the char is
    /// part of an escape sequence. Lines without ESC use the fast path.
    fn parse_ansi(&mut self, ch: char) -> Option<Style> {
        match self.ansi_parser.as_mut() {
            Some(parser) => parser.parse_char(ch),
            None => Some(Style::default()),
        }
    }

    /// Screen x of the current cell (gutter + column, after horizontal scroll).
    fn cell_screen_x(&self) -> u16 {
        self.input.gutter_width as u16 + self.col_offset.saturating_sub(self.input.left_col) as u16
    }

    /// Flush the accumulator and hand back the per-line results.
    fn finish(mut self) -> CellPassOutput {
        self.span_acc.flush(self.line_spans, self.line_view_map);
        CellPassOutput {
            rendered_cols: self.rendered_cols,
            col_offset: self.col_offset,
            first_line_byte_pos: self.first_line_byte_pos,
            last_line_byte_pos: self.last_line_byte_pos,
            syntax_extend_bg: self.syntax_extend_bg,
        }
    }
}

/// Indices of the first and last non-whitespace characters of a line
/// (`None`s when the line is all whitespace).
fn non_ws_bounds(line_content: &str) -> (Option<usize>, Option<usize>) {
    let mut first: Option<usize> = None;
    let mut last: Option<usize> = None;
    for (i, c) in line_content.chars().enumerate() {
        if c != ' ' && c != '\n' && c != '\r' {
            if first.is_none() {
                first = Some(i);
            }
            last = Some(i);
        }
    }
    (first, last)
}

/// Whether a whitespace indicator at char index `idx` should be shown,
/// given the line's non-whitespace bounds and the leading/inner/trailing
/// visibility toggles.
fn ws_indicator_visible(
    idx: usize,
    non_ws_bounds: (Option<usize>, Option<usize>),
    leading: bool,
    inner: bool,
    trailing: bool,
) -> bool {
    match non_ws_bounds {
        (Some(first), Some(last)) => {
            if idx < first {
                leading
            } else if idx > last {
                trailing
            } else {
                inner
            }
        }
        // All-whitespace line: every position is both leading and trailing.
        _ => leading || trailing,
    }
}
