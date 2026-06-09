//! Per-cell rendering pass: the inner character loop of `render_view_lines`.
//!
//! Walks one `ViewLine`'s characters and emits styled spans plus the
//! per-cell bookkeeping later passes need (cursor hits, rendered width,
//! extend-to-line-end fill inputs). Pulled out of `render_view_lines` so
//! the orchestrator reads as a sequence of per-line passes instead of one
//! interleaved loop.

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
use crate::primitives::ansi::AnsiParser;
use crate::primitives::display_width::char_width;
use crate::state::EditorState;
use crate::view::overlay::Overlay;
use crate::view::theme::Theme;
use crate::view::ui::view_pipeline::{LineStart, ViewLine};
use ratatui::layout::Rect;
use ratatui::style::Style;
use ratatui::text::Span;

/// Read-only inputs for one line's cell pass.
pub(super) struct CellPassInput<'a> {
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
    pub syntax_extend_bg: Option<ratatui::style::Color>,
}

/// Render one line's characters into `line_spans` / `line_view_map`.
///
/// Owns everything that happens per cell: ANSI parsing, selection and
/// overlay sweeps, syntax/semantic span lookups, whitespace indicators,
/// debug "reveal codes", software-cursor hits, and the theme-inspector
/// cell map.
#[allow(clippy::too_many_arguments)]
pub(super) fn render_line_cells(
    ctx: CellPassInput<'_>,
    selection_sweep: &mut SelectionActiveSet<'_>,
    overlay_sweep: &mut OverlayActiveSet<'_>,
    span_cursors: &mut SpanCursors,
    cursor: &mut CursorTracker,
    cell_theme_map: &mut [CellThemeInfo],
    line_spans: &mut Vec<Span<'static>>,
    line_view_map: &mut Vec<Option<usize>>,
) -> CellPassOutput {
    let CellPassInput {
        state,
        theme,
        view_line,
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
    } = ctx;

    let line_content: &str = &view_line.text;
    let line_char_source_bytes = &view_line.char_source_bytes;
    let line_char_styles = &view_line.char_styles;
    let line_char_visual_cols = &view_line.char_visual_cols;
    let line_total_visual_width = view_line.visual_width();
    let line_tab_starts = &view_line.tab_starts;

    let cursor_positions = &selection.cursor_positions;
    let primary_cursor_position = selection.primary_cursor_position;
    let highlight_spans = &decorations.highlight_spans;
    let semantic_token_spans = &decorations.semantic_token_spans;
    let viewport_overlays = &decorations.viewport_overlays;
    let ws = &state.buffer_settings.whitespace;

    // Pre-compute whitespace position boundaries for this view line in a
    // single pass — no intermediate `Vec<char>` per line.
    let non_ws = non_ws_bounds(line_content);

    let mut byte_index = 0; // Byte offset in line_content string
    let mut display_char_idx = 0usize; // Character index in text (for char_source_bytes)
    let mut col_offset = 0usize; // Visual column position

    // Accumulator for merging consecutive characters with the same style.
    // This is critical for proper rendering of combining characters (Thai, etc.)
    let mut span_acc = SpanAccumulator::new();

    // ANSI parser for this line to handle escape sequences.
    // Optimization: only create parser if line contains ESC byte.
    let line_has_ansi = line_content.contains('\x1b');
    let mut ansi_parser = if line_has_ansi {
        Some(AnsiParser::new())
    } else {
        None
    };
    // visible_char_count: all chars stepped over (for long-line break check).
    // rendered_cols: chars that landed on screen (for fill width — so full-line
    // bg fills reach the viewport edge under horizontal scroll).
    let mut visible_char_count = 0usize;
    let mut rendered_cols = 0usize;

    // Debug mode: track active highlight/overlay spans for WordPerfect-style reveal codes
    let mut debug_tracker = if state.debug_highlight_mode {
        Some(DebugSpanTracker::default())
    } else {
        None
    };

    // Track byte positions for extend_to_line_end feature
    let mut first_line_byte_pos: Option<usize> = None;
    let mut last_line_byte_pos: Option<usize> = None;
    let mut syntax_extend_bg: Option<ratatui::style::Color> = None;

    // Reset the per-row touched set. Wrap continuations inherit overlays
    // still active from the previous row of the same source line; new
    // source lines do not (see OverlayActiveSet).
    overlay_sweep.enter_row(matches!(view_line.line_start, LineStart::AfterBreak));

    for ch in line_content.chars() {
        // Get source byte for this character using character index
        // (char_source_bytes is indexed by character position, not visual column)
        let byte_pos = line_char_source_bytes
            .get(display_char_idx)
            .copied()
            .flatten();

        // Track byte positions for extend_to_line_end
        if let Some(bp) = byte_pos {
            if first_line_byte_pos.is_none() {
                first_line_byte_pos = Some(bp);
            }
            last_line_byte_pos = Some(bp);

            // Advance overlay active-set sweep for this cell. Monotonic
            // in `bp` across all view lines in this render call.
            overlay_sweep.advance_to(bp);
        }

        // Process character through ANSI parser first (if line has ANSI).
        // If parser returns None, the character is part of an escape sequence and should be skipped.
        let ansi_style = if let Some(ref mut parser) = ansi_parser {
            match parser.parse_char(ch) {
                Some(style) => style,
                None => {
                    // This character is part of an ANSI escape sequence, skip it.
                    // ANSI escape chars have zero visual width, so don't increment col_offset.
                    // IMPORTANT: If the cursor is on this ANSI byte, track it.
                    if byte_pos == Some(primary_cursor_position) {
                        // Account for horizontal scrolling by using col_offset - left_col
                        cursor.place(
                            gutter_width as u16 + col_offset.saturating_sub(left_col) as u16,
                            current_row,
                        );
                    }
                    byte_index += ch.len_utf8();
                    display_char_idx += 1;
                    // Note: col_offset not incremented - ANSI chars have 0 visual width
                    continue;
                }
            }
        } else {
            // No ANSI in this line - use default style (fast path)
            Style::default()
        };

        // Performance: skip expensive style calculations for characters beyond visible range.
        // Use visible_char_count (not byte_index) since ANSI codes don't take up visible space.
        if visible_char_count > max_chars_to_process {
            // Fast path: skip remaining characters without processing.
            // This is critical for performance with very long lines (e.g., 100KB single line)
            break;
        }

        // Skip characters before left_column
        if col_offset >= left_col {
            // Check if this view position is the START of a tab expansion
            let is_tab_start = line_tab_starts.contains(&col_offset);

            // Check if this character is at a cursor position.
            // For tab expansions: only show cursor on the FIRST space (the tab_start position).
            // This prevents cursor from appearing on all 8 expanded spaces.
            let is_cursor = byte_pos
                .map(|bp| {
                    if !cursor_positions.contains(&bp) || bp >= state.buffer.len() {
                        return false;
                    }
                    // If this byte maps to a tab character, only show cursor at tab_start.
                    // Check if this is part of a tab expansion by looking at previous char.
                    let prev_char_idx = display_char_idx.saturating_sub(1);
                    let prev_byte_pos =
                        line_char_source_bytes.get(prev_char_idx).copied().flatten();
                    // Show cursor if: this is start of line, OR previous char had different byte pos
                    display_char_idx == 0 || prev_byte_pos != Some(bp)
                })
                .unwrap_or(false);

            // Refresh the block-rect active set for this row.
            // Idempotent on the same gutter line (no inner gate).
            selection_sweep.enter_line(gutter_num);

            // For primary cursor in active split, terminal hardware cursor provides
            // visual indication, so we can still show selection background.
            // Only exclude secondary cursors from selection (they use REVERSED styling).
            // Bug #614: Previously excluded all cursor positions, causing first char
            // of selection to display with wrong background for bar/underline cursors.
            let is_primary_cursor = is_cursor && byte_pos == Some(primary_cursor_position);
            let exclude_from_selection = is_cursor && !(is_active && is_primary_cursor);

            let is_selected =
                !exclude_from_selection && selection_sweep.contains(byte_pos, byte_index);

            // char_styles is indexed by character position, not visual column
            let token_style = line_char_styles
                .get(display_char_idx)
                .and_then(|s| s.as_ref());

            // Resolve highlight/semantic colors via cursor-based O(1) lookup
            let (highlight_color, highlight_theme_key, highlight_display_name) = match byte_pos {
                Some(bp) => span_info_at(highlight_spans, &mut span_cursors.highlight, bp),
                None => (None, None, None),
            };
            // Diff categories carry a bg the renderer paints as a row wash.
            // `span_bg_info_at` is an O(1) peek using the cursor
            // `span_info_at` just advanced; no second walk.
            let (highlight_bg, highlight_bg_extends) = match byte_pos {
                Some(bp) => span_bg_info_at(highlight_spans, span_cursors.highlight, bp),
                None => (None, false),
            };
            let highlight_bg_theme_key = highlight_bg
                .and(highlight_theme_key)
                .or(highlight_theme_key);
            let semantic_token_color = match byte_pos {
                Some(bp) => span_color_at(semantic_token_spans, &mut span_cursors.semantic, bp),
                None => None,
            };

            // Pre-resolved active overlays for this cell. Empty slice
            // when byte_pos is None (ANSI continuation / virtual cells)
            // — matches pre-sweep behaviour where `bp = None`
            // short-circuited overlay filtering.
            let cell_overlays: &[&Overlay] = if byte_pos.is_some() {
                overlay_sweep.at_cursor()
            } else {
                &[]
            };

            let CharStyleOutput {
                mut style,
                is_secondary_cursor,
                fg_theme_key,
                bg_theme_key,
                region: cell_region,
            } = compute_char_style(&CharStyleContext {
                byte_pos,
                token_style,
                ansi_style,
                is_cursor,
                is_selected,
                theme,
                highlight_color,
                highlight_theme_key,
                highlight_bg,
                highlight_bg_theme_key,
                semantic_token_color,
                active_overlays: cell_overlays,
                primary_cursor_position,
                is_active,
                skip_primary_cursor_reverse: session_mode,
                is_cursor_line_highlighted: is_on_cursor_line
                    && highlight_current_line
                    && is_active,
                current_line_bg: theme.current_line_bg,
            });

            // Remember this row's diff bg so the tail-fill pass can
            // continue the wash past the scoped text. Only set when the
            // category actually wants extension — keeps per-token bg
            // scopes (none today, but possible) from unintentionally
            // bleeding to the row's right edge.
            if let (Some(bg), true) = (highlight_bg, highlight_bg_extends) {
                syntax_extend_bg = Some(bg);
            }

            // Record cell theme info for the theme inspector popup
            if screen_width > 0 {
                let screen_col = render_area.x
                    + gutter_width as u16
                    + col_offset.saturating_sub(left_col) as u16;
                let screen_row = render_area.y + current_row;
                let idx = screen_row as usize * screen_width as usize + screen_col as usize;
                if let Some(cell) = cell_theme_map.get_mut(idx) {
                    *cell = CellThemeInfo {
                        fg_key: fg_theme_key,
                        bg_key: bg_theme_key,
                        region: cell_region,
                        syntax_category: highlight_display_name,
                    };
                }
            }

            // Determine display character (tabs already expanded in ViewLineIterator).
            // Show tab indicator (→) or space indicator (·) based on granular
            // whitespace visibility settings (leading/inner/trailing positions).
            // `indicator_buf` holds the UTF-8 bytes of a single char on the
            // stack — no heap allocation per cell.
            let mut indicator_buf = [0u8; 4];
            let mut is_whitespace_indicator = false;

            let ws_show_tab = is_tab_start
                && ws_indicator_visible(
                    display_char_idx,
                    non_ws,
                    ws.tabs_leading,
                    ws.tabs_inner,
                    ws.tabs_trailing,
                );
            let ws_show_space = ch == ' '
                && !is_tab_start
                && ws_indicator_visible(
                    display_char_idx,
                    non_ws,
                    ws.spaces_leading,
                    ws.spaces_inner,
                    ws.spaces_trailing,
                );

            let display_char: &str = if is_cursor && lsp_waiting && is_active {
                "⋯"
            } else if debug_tracker.is_some() && ch == '\r' {
                // Debug mode: show CR explicitly
                "\\r"
            } else if debug_tracker.is_some() && ch == '\n' {
                // Debug mode: show LF explicitly
                "\\n"
            } else if ch == '\n' {
                ""
            } else if ws_show_tab {
                // Visual indicator for tab: show → at the first position
                is_whitespace_indicator = true;
                '→'.encode_utf8(&mut indicator_buf)
            } else if ws_show_space {
                // Visual indicator for space: show · when enabled
                is_whitespace_indicator = true;
                '·'.encode_utf8(&mut indicator_buf)
            } else {
                ch.encode_utf8(&mut indicator_buf)
            };

            // Apply subdued whitespace indicator color from theme
            if is_whitespace_indicator && !is_cursor && !is_selected {
                style = style.fg(theme.whitespace_indicator_fg);
            }

            if !display_char.is_empty() {
                // Debug mode: insert opening tags for spans starting at this position
                if let Some(ref mut tracker) = debug_tracker {
                    // Flush before debug tags
                    span_acc.flush(line_spans, line_view_map);
                    let opening_tags =
                        tracker.get_opening_tags(byte_pos, highlight_spans, viewport_overlays);
                    for tag in opening_tags {
                        push_debug_tag(line_spans, line_view_map, tag);
                    }
                    // Debug mode: show byte position before each character
                    if let Some(bp) = byte_pos {
                        push_debug_tag(line_spans, line_view_map, format!("[{}]", bp));
                    }
                }

                // Use accumulator to merge consecutive chars with same style.
                // This is critical for combining characters (Thai diacritics, etc.)
                for c in display_char.chars() {
                    span_acc.push(c, style, byte_pos, line_spans, line_view_map);
                }

                // Debug mode: insert closing tags for spans ending at this position.
                // Check using the NEXT byte position to see if we're leaving a span.
                if let Some(ref mut tracker) = debug_tracker {
                    // Flush before debug tags
                    span_acc.flush(line_spans, line_view_map);
                    // Look ahead to next byte position to determine closing tags
                    let next_byte_pos = byte_pos.map(|bp| bp + ch.len_utf8());
                    let closing_tags = tracker.get_closing_tags(next_byte_pos);
                    for tag in closing_tags {
                        push_debug_tag(line_spans, line_view_map, tag);
                    }
                }
            }

            // Track cursor position for zero-width characters.
            // Zero-width chars don't get map entries, so we need to explicitly record cursor pos.
            if byte_pos == Some(primary_cursor_position) && char_width(ch) == 0 {
                // Account for horizontal scrolling by subtracting left_col
                cursor.place(
                    gutter_width as u16 + col_offset.saturating_sub(left_col) as u16,
                    current_row,
                );
            }

            if is_cursor && ch == '\n' {
                let should_add_indicator = if is_active { is_secondary_cursor } else { true };
                if should_add_indicator {
                    // Flush accumulated text before adding cursor indicator
                    // so the indicator appears after the line content, not before
                    span_acc.flush(line_spans, line_view_map);
                    push_span_with_map(
                        line_spans,
                        line_view_map,
                        " ".to_string(),
                        cursor_indicator_style(theme, is_active),
                        byte_pos,
                    );
                }
            }
        }

        byte_index += ch.len_utf8();
        display_char_idx += 1; // Increment character index for next lookup

        // col_offset tracks visual column position (for indexing into visual_to_char).
        // We read the per-char visual column that view_pipeline assigned so that
        // grapheme clusters (ZWJ emoji, base+combining, etc.) advance by
        // `UnicodeWidthStr::width(cluster)` — the same width ratatui uses when
        // re-segmenting spans — instead of summing per-codepoint `char_width`.
        // Without this, the renderer's col_offset diverges from the view
        // pipeline's for any cluster whose str_width ≠ Σ char_width, producing
        // variable-width rendering corruption (issue #1577).
        let next_col_for_char = line_char_visual_cols
            .get(display_char_idx)
            .copied()
            .unwrap_or(line_total_visual_width);
        let ch_width = next_col_for_char.saturating_sub(col_offset);
        // `\n` gets visual width 1 from the view pipeline but renders as
        // empty — don't count it as an on-screen cell.
        let was_rendered = col_offset >= left_col && ch != '\n';
        col_offset = next_col_for_char;
        visible_char_count += ch_width;
        if was_rendered {
            rendered_cols += ch_width;
        }
    }

    // Flush any remaining accumulated text at end of line
    span_acc.flush(line_spans, line_view_map);

    CellPassOutput {
        rendered_cols,
        col_offset,
        first_line_byte_pos,
        last_line_byte_pos,
        syntax_extend_bg,
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
