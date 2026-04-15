//! Per-line render loop.
//!
//! This is the hot path for drawing the editor viewport. It consumes the
//! shared `SelectionContext` / `DecorationContext` carriers along with a
//! concern-scoped `LineRenderInput`, and produces a vector of styled
//! `Line<'static>` plus cursor and per-cell metadata.
//!
//! Everything here is quarantined to `orchestration/`.

use super::super::char_style::{compute_char_style, CharStyleContext, CharStyleOutput};
use super::super::gutter::{render_left_margin, LeftMarginContext};
use super::super::layout::ViewAnchor;
use super::super::spans::{
    push_debug_tag, push_span_with_map, span_color_at, span_info_at, DebugSpanTracker,
    SpanAccumulator,
};
use super::super::style::dim_color_for_tilde;
use super::contexts::{DecorationContext, SelectionContext};
use crate::app::types::ViewLineMapping;
use crate::primitives::ansi::AnsiParser;
use crate::primitives::display_width::char_width;
use crate::state::EditorState;
use crate::view::theme::Theme;
use crate::view::ui::view_pipeline::{should_show_line_number, LineStart, ViewLine};
use crate::view::virtual_text::VirtualTextPosition;
use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
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
    /// Per-cell theme key map for the theme inspector (screen_width used for indexing)
    pub cell_theme_map: &'a mut Vec<crate::app::types::CellThemeInfo>,
    /// Screen width for cell_theme_map indexing
    pub screen_width: u16,
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
        cell_theme_map,
        screen_width,
    } = input;

    // Fill the entire content area with default editor bg/gutter theme info
    if screen_width > 0 {
        let gutter_info = crate::app::types::CellThemeInfo {
            fg_key: Some("editor.line_number_fg"),
            bg_key: Some("editor.line_number_bg"),
            region: "Line Numbers",
            syntax_category: None,
        };
        let content_info = crate::app::types::CellThemeInfo {
            fg_key: Some("editor.fg"),
            bg_key: Some("editor.bg"),
            region: "Editor Content",
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

    let selection_ranges = &selection.ranges;
    let block_selections = &selection.block_rects;
    let cursor_positions = &selection.cursor_positions;
    let primary_cursor_position = selection.primary_cursor_position;

    // Compute cursor line start byte — universal key for cursor line highlight
    let cursor_line_start_byte =
        indent_folding::find_line_start_byte(&state.buffer, primary_cursor_position);

    let highlight_spans = &decorations.highlight_spans;
    let semantic_token_spans = &decorations.semantic_token_spans;
    let viewport_overlays = &decorations.viewport_overlays;
    let virtual_text_lookup = &decorations.virtual_text_lookup;
    let diagnostic_lines = &decorations.diagnostic_lines;
    let line_indicators = &decorations.line_indicators;

    // Cursors for O(1) amortized span lookups (spans are sorted by byte range)
    let mut hl_cursor = 0usize;
    let mut sem_cursor = 0usize;

    let mut lines = Vec::new();
    let mut view_line_mappings = Vec::new();
    let mut lines_rendered = 0usize;
    let mut view_iter_idx = view_anchor.start_line_idx;
    let mut cursor_screen_x = 0u16;
    let mut cursor_screen_y = 0u16;
    let mut have_cursor = false;
    let mut last_line_end: Option<LastLineEnd> = None;
    let mut last_gutter_num: Option<usize> = None;
    let mut trailing_empty_line_rendered = false;
    let mut is_on_cursor_line = false;

    let is_empty_buffer = state.buffer.is_empty();

    // Track cursor position during rendering (eliminates duplicate line iteration)
    let mut last_visible_x: u16 = 0;
    let _view_start_line_skip = view_anchor.start_line_skip; // Currently unused

    loop {
        // Get the current ViewLine from the pipeline
        let current_view_line = if let Some(vl) = view_lines.get(view_iter_idx) {
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
            })
        } else {
            break;
        };

        // Extract line data
        let line_content = current_view_line.text.clone();
        let line_has_newline = current_view_line.ends_with_newline;
        let line_char_source_bytes = &current_view_line.char_source_bytes;
        let line_char_styles = &current_view_line.char_styles;
        let line_visual_to_char = &current_view_line.visual_to_char;
        let line_tab_starts = &current_view_line.tab_starts;
        let _line_start_type = current_view_line.line_start;

        // Pre-compute whitespace position boundaries for this view line.
        // first_non_ws: index of first non-whitespace char (None if all whitespace)
        // last_non_ws: index of last non-whitespace char (None if all whitespace)
        let line_chars_for_ws: Vec<char> = line_content.chars().collect();
        let first_non_ws_idx = line_chars_for_ws
            .iter()
            .position(|&c| c != ' ' && c != '\n' && c != '\r');
        let last_non_ws_idx = line_chars_for_ws
            .iter()
            .rposition(|&c| c != ' ' && c != '\n' && c != '\r');

        // Helper to get source byte at a visual column using the new O(1) lookup
        let _source_byte_at_col = |vis_col: usize| -> Option<usize> {
            let char_idx = line_visual_to_char.get(vis_col).copied()?;
            line_char_source_bytes.get(char_idx).copied().flatten()
        };

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
                    if line_content.is_empty() && _line_start_type == LineStart::AfterSourceNewline
                    {
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
        if !is_continuation {
            is_on_cursor_line = line_start_byte.is_some_and(|b| b == cursor_line_start_byte);
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

        // Apply horizontal scrolling - skip characters before left_column
        let left_col = left_column;

        // Build line with selection highlighting
        let mut line_spans = Vec::new();
        let mut line_view_map: Vec<Option<usize>> = Vec::new();
        let mut last_seg_y: Option<u16> = None;
        let mut _last_seg_width: usize = 0;

        // Accumulator for merging consecutive characters with the same style
        // This is critical for proper rendering of combining characters (Thai, etc.)
        let mut span_acc = SpanAccumulator::new();

        // Render left margin (indicators + line numbers + separator)
        render_left_margin(
            &LeftMarginContext {
                state,
                theme,
                is_continuation,
                line_start_byte,
                gutter_num,
                estimated_lines,
                diagnostic_lines,
                line_indicators,
                fold_indicators: &decorations.fold_indicators,
                cursor_line_start_byte,
                cursor_line_number: state.primary_cursor_line_number.value(),
                relative_line_numbers,
                show_line_numbers,
                byte_offset_mode,
                highlight_current_line,
                is_active,
            },
            &mut line_spans,
            &mut line_view_map,
        );

        // Check if this line has any selected text
        let mut byte_index = 0; // Byte offset in line_content string
        let mut display_char_idx = 0usize; // Character index in text (for char_source_bytes)
        let mut col_offset = 0usize; // Visual column position

        // Performance optimization: For very long lines, only process visible characters
        // Calculate the maximum characters we might need to render based on screen width
        // For wrapped lines, we need enough characters to fill the visible viewport
        // For non-wrapped lines, we only need one screen width worth
        let visible_lines_remaining = visible_line_count.saturating_sub(lines_rendered);
        let max_visible_chars = if line_wrap {
            // With wrapping: might need chars for multiple wrapped lines
            // Be generous to avoid cutting off wrapped content
            (render_area.width as usize)
                .saturating_mul(visible_lines_remaining.max(1))
                .saturating_add(200)
        } else {
            // Without wrapping: only need one line worth of characters
            (render_area.width as usize).saturating_add(100)
        };
        let max_chars_to_process = left_col.saturating_add(max_visible_chars);

        // ANSI parser for this line to handle escape sequences
        // Optimization: only create parser if line contains ESC byte
        let line_has_ansi = line_content.contains('\x1b');
        let mut ansi_parser = if line_has_ansi {
            Some(AnsiParser::new())
        } else {
            None
        };
        // Track visible characters separately from byte position for ANSI handling
        let mut visible_char_count = 0usize;

        // Debug mode: track active highlight/overlay spans for WordPerfect-style reveal codes
        let mut debug_tracker = if state.debug_highlight_mode {
            Some(DebugSpanTracker::default())
        } else {
            None
        };

        // Track byte positions for extend_to_line_end feature
        let mut first_line_byte_pos: Option<usize> = None;
        let mut last_line_byte_pos: Option<usize> = None;

        let chars_iterator = line_content.chars().peekable();
        for ch in chars_iterator {
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
            }

            // Process character through ANSI parser first (if line has ANSI)
            // If parser returns None, the character is part of an escape sequence and should be skipped
            let ansi_style = if let Some(ref mut parser) = ansi_parser {
                match parser.parse_char(ch) {
                    Some(style) => style,
                    None => {
                        // This character is part of an ANSI escape sequence, skip it
                        // ANSI escape chars have zero visual width, so don't increment col_offset
                        // IMPORTANT: If the cursor is on this ANSI byte, track it
                        if let Some(bp) = byte_pos {
                            if bp == primary_cursor_position && !have_cursor {
                                // Account for horizontal scrolling by using col_offset - left_col
                                cursor_screen_x = gutter_width as u16
                                    + col_offset.saturating_sub(left_col) as u16;
                                cursor_screen_y = lines_rendered.saturating_sub(1) as u16;
                                have_cursor = true;
                            }
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

            // Performance: skip expensive style calculations for characters beyond visible range
            // Use visible_char_count (not byte_index) since ANSI codes don't take up visible space
            if visible_char_count > max_chars_to_process {
                // Fast path: skip remaining characters without processing
                // This is critical for performance with very long lines (e.g., 100KB single line)
                break;
            }

            // Skip characters before left_column
            if col_offset >= left_col {
                // Check if this view position is the START of a tab expansion
                let is_tab_start = line_tab_starts.contains(&col_offset);

                // Check if this character is at a cursor position
                // For tab expansions: only show cursor on the FIRST space (the tab_start position)
                // This prevents cursor from appearing on all 8 expanded spaces
                let is_cursor = byte_pos
                    .map(|bp| {
                        if !cursor_positions.contains(&bp) || bp >= state.buffer.len() {
                            return false;
                        }
                        // If this byte maps to a tab character, only show cursor at tab_start
                        // Check if this is part of a tab expansion by looking at previous char
                        let prev_char_idx = display_char_idx.saturating_sub(1);
                        let prev_byte_pos =
                            line_char_source_bytes.get(prev_char_idx).copied().flatten();
                        // Show cursor if: this is start of line, OR previous char had different byte pos
                        display_char_idx == 0 || prev_byte_pos != Some(bp)
                    })
                    .unwrap_or(false);

                // Check if this character is in any selection range (but not at cursor position)
                // Also check for block/rectangular selections (uses gutter_num which is
                // the line number for small files — block_rects stores line numbers)
                let is_in_block_selection =
                    block_selections
                        .iter()
                        .any(|(start_line, start_col, end_line, end_col)| {
                            gutter_num >= *start_line
                                && gutter_num <= *end_line
                                && byte_index >= *start_col
                                && byte_index <= *end_col
                        });

                // For primary cursor in active split, terminal hardware cursor provides
                // visual indication, so we can still show selection background.
                // Only exclude secondary cursors from selection (they use REVERSED styling).
                // Bug #614: Previously excluded all cursor positions, causing first char
                // of selection to display with wrong background for bar/underline cursors.
                let is_primary_cursor = is_cursor && byte_pos == Some(primary_cursor_position);
                let exclude_from_selection = is_cursor && !(is_active && is_primary_cursor);

                let is_selected = !exclude_from_selection
                    && (byte_pos.is_some_and(|bp| {
                        selection_ranges.iter().any(|range| range.contains(&bp))
                    }) || is_in_block_selection);

                // Compute character style using helper function
                // char_styles is indexed by character position, not visual column
                let token_style = line_char_styles
                    .get(display_char_idx)
                    .and_then(|s| s.as_ref());

                // Resolve highlight/semantic colors via cursor-based O(1) lookup
                let (highlight_color, highlight_theme_key, highlight_display_name) = match byte_pos
                {
                    Some(bp) => span_info_at(highlight_spans, &mut hl_cursor, bp),
                    None => (None, None, None),
                };
                let semantic_token_color = match byte_pos {
                    Some(bp) => span_color_at(semantic_token_spans, &mut sem_cursor, bp),
                    None => None,
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
                    semantic_token_color,
                    viewport_overlays,
                    primary_cursor_position,
                    is_active,
                    skip_primary_cursor_reverse: session_mode,
                    is_cursor_line_highlighted: is_on_cursor_line
                        && highlight_current_line
                        && is_active,
                    current_line_bg: theme.current_line_bg,
                });

                // Record cell theme info for the theme inspector popup
                if screen_width > 0 {
                    let screen_col = render_area.x
                        + gutter_width as u16
                        + col_offset.saturating_sub(left_col) as u16;
                    let screen_row = render_area.y + lines.len() as u16;
                    let idx = screen_row as usize * screen_width as usize + screen_col as usize;
                    if let Some(cell) = cell_theme_map.get_mut(idx) {
                        *cell = crate::app::types::CellThemeInfo {
                            fg_key: fg_theme_key,
                            bg_key: bg_theme_key,
                            region: cell_region,
                            syntax_category: highlight_display_name,
                        };
                    }
                }

                // Determine display character (tabs already expanded in ViewLineIterator)
                // Show tab indicator (→) or space indicator (·) based on granular
                // whitespace visibility settings (leading/inner/trailing positions)
                let indicator_buf: String;
                let mut is_whitespace_indicator = false;

                // Classify whitespace position: leading, inner, or trailing
                // Leading = before first non-ws char, Trailing = after last non-ws char
                // All-whitespace lines match both leading and trailing
                let ws_show_tab = is_tab_start && {
                    let ws = &state.buffer_settings.whitespace;
                    match (first_non_ws_idx, last_non_ws_idx) {
                        (None, _) | (_, None) => ws.tabs_leading || ws.tabs_trailing,
                        (Some(first), Some(last)) => {
                            if display_char_idx < first {
                                ws.tabs_leading
                            } else if display_char_idx > last {
                                ws.tabs_trailing
                            } else {
                                ws.tabs_inner
                            }
                        }
                    }
                };
                let ws_show_space = ch == ' ' && !is_tab_start && {
                    let ws = &state.buffer_settings.whitespace;
                    match (first_non_ws_idx, last_non_ws_idx) {
                        (None, _) | (_, None) => ws.spaces_leading || ws.spaces_trailing,
                        (Some(first), Some(last)) => {
                            if display_char_idx < first {
                                ws.spaces_leading
                            } else if display_char_idx > last {
                                ws.spaces_trailing
                            } else {
                                ws.spaces_inner
                            }
                        }
                    }
                };

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
                    indicator_buf = "→".to_string();
                    &indicator_buf
                } else if ws_show_space {
                    // Visual indicator for space: show · when enabled
                    is_whitespace_indicator = true;
                    indicator_buf = "·".to_string();
                    &indicator_buf
                } else {
                    indicator_buf = ch.to_string();
                    &indicator_buf
                };

                // Apply subdued whitespace indicator color from theme
                if is_whitespace_indicator && !is_cursor && !is_selected {
                    style = style.fg(theme.whitespace_indicator_fg);
                }

                if let Some(bp) = byte_pos {
                    if let Some(vtexts) = virtual_text_lookup.get(&bp) {
                        for vtext in vtexts
                            .iter()
                            .filter(|v| v.position == VirtualTextPosition::BeforeChar)
                        {
                            // Flush accumulated text before inserting virtual text
                            span_acc.flush(&mut line_spans, &mut line_view_map);
                            // Add extra space if at end of line (before newline)
                            let extra_space = if ch == '\n' { " " } else { "" };
                            let text_with_space = format!("{}{} ", extra_space, vtext.text);
                            push_span_with_map(
                                &mut line_spans,
                                &mut line_view_map,
                                text_with_space,
                                vtext.resolved_style(theme),
                                None,
                            );
                        }
                    }
                }

                if !display_char.is_empty() {
                    // Debug mode: insert opening tags for spans starting at this position
                    if let Some(ref mut tracker) = debug_tracker {
                        // Flush before debug tags
                        span_acc.flush(&mut line_spans, &mut line_view_map);
                        let opening_tags =
                            tracker.get_opening_tags(byte_pos, highlight_spans, viewport_overlays);
                        for tag in opening_tags {
                            push_debug_tag(&mut line_spans, &mut line_view_map, tag);
                        }
                    }

                    // Debug mode: show byte position before each character
                    if debug_tracker.is_some() {
                        if let Some(bp) = byte_pos {
                            push_debug_tag(
                                &mut line_spans,
                                &mut line_view_map,
                                format!("[{}]", bp),
                            );
                        }
                    }

                    // Use accumulator to merge consecutive chars with same style
                    // This is critical for combining characters (Thai diacritics, etc.)
                    for c in display_char.chars() {
                        span_acc.push(c, style, byte_pos, &mut line_spans, &mut line_view_map);
                    }

                    // Debug mode: insert closing tags for spans ending at this position
                    // Check using the NEXT byte position to see if we're leaving a span
                    if let Some(ref mut tracker) = debug_tracker {
                        // Flush before debug tags
                        span_acc.flush(&mut line_spans, &mut line_view_map);
                        // Look ahead to next byte position to determine closing tags
                        let next_byte_pos = byte_pos.map(|bp| bp + ch.len_utf8());
                        let closing_tags = tracker.get_closing_tags(next_byte_pos);
                        for tag in closing_tags {
                            push_debug_tag(&mut line_spans, &mut line_view_map, tag);
                        }
                    }
                }

                // Track cursor position for zero-width characters
                // Zero-width chars don't get map entries, so we need to explicitly record cursor pos
                if !have_cursor {
                    if let Some(bp) = byte_pos {
                        if bp == primary_cursor_position && char_width(ch) == 0 {
                            // Account for horizontal scrolling by subtracting left_col
                            cursor_screen_x =
                                gutter_width as u16 + col_offset.saturating_sub(left_col) as u16;
                            cursor_screen_y = lines.len() as u16;
                            have_cursor = true;
                        }
                    }
                }

                if let Some(bp) = byte_pos {
                    if let Some(vtexts) = virtual_text_lookup.get(&bp) {
                        for vtext in vtexts
                            .iter()
                            .filter(|v| v.position == VirtualTextPosition::AfterChar)
                        {
                            // Flush the accumulated text so the virtual
                            // text is placed *after* the current char
                            // rather than sneaking in front of the next
                            // flush of `span_acc`. Without this flush,
                            // AfterChar hints render at the start of the
                            // buffered run (issue #1572).
                            span_acc.flush(&mut line_spans, &mut line_view_map);
                            let text_with_space = format!(" {}", vtext.text);
                            push_span_with_map(
                                &mut line_spans,
                                &mut line_view_map,
                                text_with_space,
                                vtext.resolved_style(theme),
                                None,
                            );
                        }
                    }
                }

                if is_cursor && ch == '\n' {
                    let should_add_indicator = if is_active { is_secondary_cursor } else { true };
                    if should_add_indicator {
                        // Flush accumulated text before adding cursor indicator
                        // so the indicator appears after the line content, not before
                        span_acc.flush(&mut line_spans, &mut line_view_map);
                        let cursor_style = if is_active {
                            Style::default()
                                .fg(theme.editor_fg)
                                .bg(theme.editor_bg)
                                .add_modifier(Modifier::REVERSED)
                        } else {
                            Style::default()
                                .fg(theme.editor_fg)
                                .bg(theme.inactive_cursor)
                        };
                        push_span_with_map(
                            &mut line_spans,
                            &mut line_view_map,
                            " ".to_string(),
                            cursor_style,
                            byte_pos,
                        );
                    }
                }
            }

            byte_index += ch.len_utf8();
            display_char_idx += 1; // Increment character index for next lookup
                                   // col_offset tracks visual column position (for indexing into visual_to_char)
                                   // visual_to_char has one entry per visual column, not per character
            let ch_width = char_width(ch);
            col_offset += ch_width;
            visible_char_count += ch_width;
        }

        // Flush any remaining accumulated text at end of line
        span_acc.flush(&mut line_spans, &mut line_view_map);

        // Set last_seg_y early so cursor detection works for both empty and non-empty lines
        // For lines without wrapping, this will be the final y position
        // Also set for empty content lines (regardless of line_wrap) so cursor at EOF can be positioned
        let content_is_empty = line_content.is_empty();
        if line_spans.is_empty() || !line_wrap || content_is_empty {
            last_seg_y = Some(lines.len() as u16);
        }

        if !line_has_newline {
            let line_len_chars = line_content.chars().count();

            // Map view positions to buffer positions using per-line char_source_bytes
            let last_char_idx = line_len_chars.saturating_sub(1);
            let after_last_char_idx = line_len_chars;

            let last_char_buf_pos = line_char_source_bytes.get(last_char_idx).copied().flatten();
            let after_last_char_buf_pos = line_char_source_bytes
                .get(after_last_char_idx)
                .copied()
                .flatten();

            let cursor_at_end = cursor_positions.iter().any(|&pos| {
                // Cursor is "at end" only if it's AFTER the last character, not ON it.
                // A cursor ON the last character should render on that character (handled in main loop).
                let matches_after = after_last_char_buf_pos.is_some_and(|bp| pos == bp);
                // Fallback: when there's no mapping after last char (EOF), check if cursor is after last char
                // The fallback should match the position that would be "after" if there was a mapping.
                // For empty lines with no source mappings (e.g. trailing empty line after final '\n'),
                // the expected position is buffer.len() (EOF), not 0.
                let expected_after_pos = last_char_buf_pos
                    .map(|p| p + 1)
                    .unwrap_or(state.buffer.len());
                let matches_fallback =
                    after_last_char_buf_pos.is_none() && pos == expected_after_pos;

                matches_after || matches_fallback
            });

            if cursor_at_end {
                // Primary cursor is at end only if AFTER the last char, not ON it
                let is_primary_at_end = after_last_char_buf_pos
                    .is_some_and(|bp| bp == primary_cursor_position)
                    || (after_last_char_buf_pos.is_none()
                        && primary_cursor_position >= state.buffer.len());

                // Track cursor position for primary cursor
                if let Some(seg_y) = last_seg_y {
                    if is_primary_at_end {
                        // Cursor position now includes gutter width (consistent with main cursor tracking)
                        // For empty lines, cursor is at gutter width (right after gutter)
                        // For non-empty lines without newline, cursor is after the last visible character
                        // Account for horizontal scrolling by using col_offset - left_col
                        cursor_screen_x = if line_len_chars == 0 {
                            gutter_width as u16
                        } else {
                            // col_offset is the visual column after the last character
                            // Subtract left_col to get the screen position after horizontal scroll
                            gutter_width as u16 + col_offset.saturating_sub(left_col) as u16
                        };
                        cursor_screen_y = seg_y;
                        have_cursor = true;
                    }
                }

                // When software_cursor_only, always add the indicator space because
                // the backend does not render a hardware cursor.  In terminal mode,
                // the primary cursor at end-of-line relies on the hardware cursor.
                let should_add_indicator = if is_active {
                    software_cursor_only || !is_primary_at_end
                } else {
                    true
                };
                if should_add_indicator {
                    let cursor_style = if is_active {
                        Style::default()
                            .fg(theme.editor_fg)
                            .bg(theme.editor_bg)
                            .add_modifier(Modifier::REVERSED)
                    } else {
                        Style::default()
                            .fg(theme.editor_fg)
                            .bg(theme.inactive_cursor)
                    };
                    push_span_with_map(
                        &mut line_spans,
                        &mut line_view_map,
                        " ".to_string(),
                        cursor_style,
                        None,
                    );
                }
            }
        }

        // ViewLines are already wrapped (Break tokens became newlines in ViewLineIterator)
        // so each line is one visual line - no need to wrap again
        let current_y = lines.len() as u16;
        last_seg_y = Some(current_y);

        if !line_spans.is_empty() {
            // Find cursor position and track last visible x by iterating through line_view_map
            // Note: line_view_map includes both gutter and content character mappings
            //
            // When the cursor byte falls inside a concealed range (e.g. syntax markers
            // hidden by compose-mode plugins), no view_map entry will exactly match
            // primary_cursor_position.  In that case we fall back to the nearest
            // visible byte that is >= the cursor byte on the same line — this keeps
            // the cursor visible for the one frame between cursor movement and the
            // plugin's conceal-refresh response.
            let mut nearest_fallback: Option<(u16, usize)> = None; // (screen_x, byte_distance)
            for (screen_x, source_offset) in line_view_map.iter().enumerate() {
                if let Some(src) = source_offset {
                    // Exact match: cursor byte is visible
                    if *src == primary_cursor_position && !have_cursor {
                        cursor_screen_x = screen_x as u16;
                        cursor_screen_y = current_y;
                        have_cursor = true;
                    }
                    // Track nearest visible byte >= cursor position for fallback
                    if !have_cursor && *src >= primary_cursor_position {
                        let dist = *src - primary_cursor_position;
                        if nearest_fallback.is_none() || dist < nearest_fallback.unwrap().1 {
                            nearest_fallback = Some((screen_x as u16, dist));
                        }
                    }
                    last_visible_x = screen_x as u16;
                }
            }
            // Fallback: cursor byte was concealed — snap to nearest visible byte
            if !have_cursor {
                if let Some((fallback_x, _)) = nearest_fallback {
                    cursor_screen_x = fallback_x;
                    cursor_screen_y = current_y;
                    have_cursor = true;
                }
            }
        }

        // Inline diagnostic text: render after line content (before extend_to_line_end fill).
        // Only for non-continuation lines that have a diagnostic overlay.
        if let Some(lsb) = line_start_byte {
            if let Some((message, diag_style)) = decorations.diagnostic_inline_texts.get(&lsb) {
                let content_width = render_area.width.saturating_sub(gutter_width as u16) as usize;
                let used = visible_char_count;
                let available = content_width.saturating_sub(used);
                let gap = 2usize;
                let min_text = 10usize;

                if available > gap + min_text {
                    // Truncate message to fit
                    let max_chars = available - gap;
                    let display: String = if message.chars().count() > max_chars {
                        let truncated: String =
                            message.chars().take(max_chars.saturating_sub(1)).collect();
                        format!("{}…", truncated)
                    } else {
                        message.clone()
                    };
                    let display_width = display.chars().count();

                    // Right-align: fill gap between code and diagnostic text
                    let padding = available.saturating_sub(display_width);
                    let cursor_line_active =
                        is_on_cursor_line && highlight_current_line && is_active;
                    if padding > 0 {
                        let pad_style = if cursor_line_active {
                            Style::default().bg(theme.current_line_bg)
                        } else {
                            Style::default()
                        };
                        push_span_with_map(
                            &mut line_spans,
                            &mut line_view_map,
                            " ".repeat(padding),
                            pad_style,
                            None,
                        );
                        visible_char_count += padding;
                    }

                    // Apply current line background to diagnostic text when on cursor line
                    let effective_diag_style = if cursor_line_active && diag_style.bg.is_none() {
                        diag_style.bg(theme.current_line_bg)
                    } else {
                        *diag_style
                    };
                    push_span_with_map(
                        &mut line_spans,
                        &mut line_view_map,
                        display,
                        effective_diag_style,
                        None,
                    );
                    visible_char_count += display_width;
                }
            }
        }

        // Fill remaining width for overlays with extend_to_line_end
        // Only when line wrapping is disabled (side-by-side diff typically disables wrapping)
        if !line_wrap {
            // Calculate the content area width (total width minus gutter)
            let content_width = render_area.width.saturating_sub(gutter_width as u16) as usize;
            let remaining_cols = content_width.saturating_sub(visible_char_count);

            if remaining_cols > 0 {
                // Find the highest priority background color from overlays with extend_to_line_end
                // that overlap with this line's byte range. Overlay ranges
                // are half-open `[start, end)`, so an overlay whose end
                // equals this line's first byte ends *before* the line
                // begins and must NOT match — `range.end > start` (strict),
                // not `>=`. With `>=`, an overlay covering the previous
                // line's content + trailing newline would bleed its bg
                // onto this line's trailing fill.
                let fill_style: Option<Style> = if let (Some(start), Some(end)) =
                    (first_line_byte_pos, last_line_byte_pos)
                {
                    viewport_overlays
                        .iter()
                        .filter(|(overlay, range)| {
                            overlay.extend_to_line_end && range.start <= end && range.end > start
                        })
                        .max_by_key(|(o, _)| o.priority)
                        .and_then(|(overlay, _)| {
                            match &overlay.face {
                                crate::view::overlay::OverlayFace::Background { color } => {
                                    // Set both fg and bg to ensure ANSI codes are output
                                    Some(Style::default().fg(*color).bg(*color))
                                }
                                crate::view::overlay::OverlayFace::Style { style } => {
                                    // Extract background from style if present
                                    // Set fg to same as bg for invisible text
                                    style.bg.map(|bg| Style::default().fg(bg).bg(bg))
                                }
                                crate::view::overlay::OverlayFace::ThemedStyle {
                                    fallback_style,
                                    bg_theme,
                                    ..
                                } => {
                                    // Try theme key first, fall back to style's bg
                                    let bg = bg_theme
                                        .as_ref()
                                        .and_then(|key| theme.resolve_theme_key(key))
                                        .or(fallback_style.bg);
                                    bg.map(|bg| Style::default().fg(bg).bg(bg))
                                }
                                _ => None,
                            }
                        })
                } else {
                    None
                };

                if let Some(fill_bg) = fill_style {
                    let fill_text = " ".repeat(remaining_cols);
                    push_span_with_map(
                        &mut line_spans,
                        &mut line_view_map,
                        fill_text,
                        fill_bg,
                        None,
                    );
                }
            }
        }

        // Fill remaining width with current_line_bg for cursor line highlighting.
        // Add the span directly (not via push_span_with_map) to avoid extending
        // line_view_map, which would break mouse click byte mapping.
        if is_on_cursor_line && highlight_current_line && is_active {
            let content_width = render_area.width.saturating_sub(gutter_width as u16) as usize;
            let remaining_cols = content_width.saturating_sub(visible_char_count);
            if remaining_cols > 0 {
                span_acc.flush(&mut line_spans, &mut line_view_map);
                line_spans.push(Span::styled(
                    " ".repeat(remaining_cols),
                    Style::default().bg(theme.current_line_bg),
                ));
            }
        }

        // For virtual rows (no source bytes), inherit from previous row
        let prev_line_end_byte = view_line_mappings
            .last()
            .map(|prev: &ViewLineMapping| prev.line_end_byte)
            .unwrap_or(0);

        // Calculate line_end_byte for this line
        let line_end_byte = if current_view_line.ends_with_newline {
            // Position ON the newline - find the last source byte (the newline's position)
            current_view_line
                .char_source_bytes
                .iter()
                .rev()
                .find_map(|m| *m)
                .unwrap_or(prev_line_end_byte)
        } else {
            // Position AFTER the last character - find last source byte and add char length
            if let Some((char_idx, &Some(last_byte_start))) = current_view_line
                .char_source_bytes
                .iter()
                .enumerate()
                .rev()
                .find(|(_, m)| m.is_some())
            {
                // Get the character at this index to find its UTF-8 byte length
                if let Some(last_char) = current_view_line.text.chars().nth(char_idx) {
                    last_byte_start + last_char.len_utf8()
                } else {
                    last_byte_start
                }
            } else if matches!(current_view_line.line_start, LineStart::AfterSourceNewline)
                && prev_line_end_byte + 2 >= state.buffer.len()
            {
                // Trailing empty line after the final source newline.
                // The cursor on this line lives at buffer_len.
                state.buffer.len()
            } else {
                // Virtual row with no source bytes (e.g. table border from conceals).
                // Inherit line_end_byte from the previous row so cursor movement
                // through virtual rows lands at a valid source position.
                prev_line_end_byte
            }
        };

        // Capture accurate view line mapping for mouse clicks
        // Content mapping starts after the gutter
        let content_map = if line_view_map.len() >= gutter_width {
            line_view_map[gutter_width..].to_vec()
        } else {
            Vec::new()
        };
        view_line_mappings.push(ViewLineMapping {
            char_source_bytes: content_map.clone(),
            visual_to_char: (0..content_map.len()).collect(),
            line_end_byte,
        });

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
            && _line_start_type == LineStart::AfterSourceNewline;
        if is_iterator_trailing_empty {
            trailing_empty_line_rendered = true;
        }

        // Update last_line_end and check for cursor on newline BEFORE the break check
        // This ensures the last visible line's metadata is captured
        if let Some(y) = last_seg_y {
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
                    pos: (end_x, y),
                    terminated_with_newline: line_has_newline,
                });
            }

            if line_has_newline && line_len_chars > 0 {
                let newline_idx = line_len_chars.saturating_sub(1);
                if let Some(Some(src_newline)) = line_char_source_bytes.get(newline_idx) {
                    if *src_newline == primary_cursor_position {
                        // Cursor position now includes gutter width (consistent with main cursor tracking)
                        // For empty lines (just newline), cursor should be at gutter width (after gutter)
                        // For lines with content, cursor on newline should be after the content
                        if line_len_chars == 1 {
                            // Empty line - just the newline character
                            cursor_screen_x = gutter_width as u16;
                            cursor_screen_y = y;
                        } else {
                            // Line has content before the newline - cursor after last char
                            // end_x already includes gutter (from last_visible_x)
                            cursor_screen_x = end_x;
                            cursor_screen_y = y;
                        }
                        have_cursor = true;
                    }
                }
            }
        }

        if lines_rendered >= visible_line_count {
            break;
        }
    }

    // If the last line ended with a newline, render an implicit empty line after it.
    // This shows the line number for the cursor position after the final newline.
    // Skip this if the ViewLineIterator already produced the trailing empty line.
    if let Some(ref end) = last_line_end {
        if end.terminated_with_newline
            && lines_rendered < visible_line_count
            && !trailing_empty_line_rendered
        {
            // Render the implicit line after the newline
            let mut implicit_line_spans = Vec::new();
            // The implicit trailing line is at buffer.len()
            let implicit_line_byte = state.buffer.len();
            let implicit_gutter_num = if byte_offset_mode {
                implicit_line_byte
            } else {
                last_gutter_num.map_or(0, |n| n + 1)
            };

            let implicit_is_cursor_line = implicit_line_byte == cursor_line_start_byte;
            let implicit_cursor_bg =
                if implicit_is_cursor_line && highlight_current_line && is_active {
                    Some(theme.current_line_bg)
                } else {
                    None
                };

            if state.margins.left_config.enabled {
                // Indicator column: check for diagnostic markers on this implicit line
                if decorations.diagnostic_lines.contains(&implicit_line_byte) {
                    let mut style = Style::default().fg(ratatui::style::Color::Red);
                    if let Some(bg) = implicit_cursor_bg {
                        style = style.bg(bg);
                    }
                    implicit_line_spans.push(Span::styled("●", style));
                } else {
                    let mut style = Style::default();
                    if let Some(bg) = implicit_cursor_bg {
                        style = style.bg(bg);
                    }
                    implicit_line_spans.push(Span::styled(" ", style));
                }

                // Line number (or byte offset in byte_offset_mode)
                let rendered_text = if byte_offset_mode && show_line_numbers {
                    format!(
                        "{:>width$}",
                        implicit_gutter_num,
                        width = state.margins.left_config.width
                    )
                } else {
                    let estimated_lines = state.buffer.line_count().unwrap_or(
                        (state.buffer.len() / state.buffer.estimated_line_length()).max(1),
                    );
                    let margin_content = state.margins.render_line(
                        implicit_gutter_num,
                        crate::view::margin::MarginPosition::Left,
                        estimated_lines,
                        show_line_numbers,
                    );
                    margin_content.render(state.margins.left_config.width).0
                };
                let mut margin_style = Style::default().fg(theme.line_number_fg);
                if let Some(bg) = implicit_cursor_bg {
                    margin_style = margin_style.bg(bg);
                }
                implicit_line_spans.push(Span::styled(rendered_text, margin_style));

                // Separator
                if state.margins.left_config.show_separator {
                    let mut sep_style = Style::default().fg(theme.line_number_fg);
                    if let Some(bg) = implicit_cursor_bg {
                        sep_style = sep_style.bg(bg);
                    }
                    implicit_line_spans.push(Span::styled(
                        state.margins.left_config.separator.to_string(),
                        sep_style,
                    ));
                }
            }

            // Fill remaining width with current_line_bg for cursor line
            if let Some(bg) = implicit_cursor_bg {
                let gutter_w = if state.margins.left_config.enabled {
                    state.margins.left_total_width()
                } else {
                    0
                };
                let content_width = render_area.width.saturating_sub(gutter_w as u16) as usize;
                if content_width > 0 {
                    implicit_line_spans.push(Span::styled(
                        " ".repeat(content_width),
                        Style::default().bg(bg),
                    ));
                }
            }

            let implicit_y = lines.len() as u16;
            lines.push(Line::from(implicit_line_spans));
            lines_rendered += 1;

            // Add mapping for implicit line
            // It has no content, so map is empty (gutter is handled by offset in screen_to_buffer_position)
            let buffer_len = state.buffer.len();

            view_line_mappings.push(ViewLineMapping {
                char_source_bytes: Vec::new(),
                visual_to_char: Vec::new(),
                line_end_byte: buffer_len,
            });

            // NOTE: We intentionally do NOT update last_line_end here.
            // The implicit empty line is a visual display aid, not an actual content line.
            // last_line_end should track the last actual content line for cursor placement logic.

            // If primary cursor is at EOF (after the newline), set cursor on this line
            if primary_cursor_position == state.buffer.len() && !have_cursor {
                cursor_screen_x = gutter_width as u16;
                cursor_screen_y = implicit_y;
                have_cursor = true;
            }
        }
    }

    // Even when there was no screen room to render the implicit trailing
    // empty line, we must still add a ViewLineMapping for it.  Without
    // the mapping, move_visual_line (Down key) thinks the last rendered
    // row is the boundary and returns None — preventing the cursor from
    // reaching the trailing empty line (which would trigger a viewport
    // scroll on the next render).
    if let Some(ref end) = last_line_end {
        if end.terminated_with_newline {
            let last_mapped_byte = view_line_mappings
                .last()
                .map(|m| m.line_end_byte)
                .unwrap_or(0);
            let near_buffer_end = last_mapped_byte + 2 >= state.buffer.len();
            let already_mapped = view_line_mappings.last().is_some_and(|m| {
                m.char_source_bytes.is_empty() && m.line_end_byte == state.buffer.len()
            });
            if near_buffer_end && !already_mapped {
                view_line_mappings.push(ViewLineMapping {
                    char_source_bytes: Vec::new(),
                    visual_to_char: Vec::new(),
                    line_end_byte: state.buffer.len(),
                });
            }
        }
    }

    // Fill remaining rows with tilde characters to indicate EOF (like vim/neovim).
    // This also ensures proper clearing in differential rendering because tildes
    // are guaranteed to differ from previous content, forcing ratatui to update.
    // See: https://github.com/ratatui/ratatui/issues/1606
    //
    // NOTE: We use a computed darker color instead of Modifier::DIM because the DIM
    // modifier can bleed through to overlays (like menus) rendered on top of these
    // lines due to how terminal escape sequences are output.
    // See: https://github.com/sinelaw/fresh/issues/458
    if show_tilde {
        let eof_fg = dim_color_for_tilde(theme.line_number_fg);
        let eof_style = Style::default().fg(eof_fg);
        while lines.len() < render_area.height as usize {
            // Show tilde with dim styling, padded with spaces to fill the line
            let tilde_line = format!(
                "~{}",
                " ".repeat(render_area.width.saturating_sub(1) as usize)
            );
            lines.push(Line::styled(tilde_line, eof_style));
        }
    }

    LineRenderOutput {
        lines,
        cursor: have_cursor.then_some((cursor_screen_x, cursor_screen_y)),
        last_line_end,
        content_lines_rendered: lines_rendered,
        view_line_mappings,
    }
}
