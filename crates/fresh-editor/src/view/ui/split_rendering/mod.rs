//! Split pane layout and buffer rendering.
//!
//! This module is organized into two tiers:
//!
//! - **Self-contained leaves** (`spans`, `style`, `char_style`, `base_tokens`,
//!   `transforms`, `view_data`, `folding`, `scrollbar`, `layout`, `gutter`,
//!   `post_pass`) — none of these depend on any shared render-time carrier.
//! - **Orchestration** (`orchestration::*`) — the only files that share
//!   `SelectionContext` / `DecorationContext`. Quarantined in a subdirectory
//!   so the coupling is visible from `ls` alone.
//!
//! The public API is re-exposed via the [`SplitRenderer`] façade at the
//! bottom of this file; it forwards to `orchestration::*`.

pub(crate) mod base_tokens;
mod char_style;
mod folding;
mod gutter;
mod layout;
mod orchestration;
mod post_pass;
mod scrollbar;
mod spans;
mod style;
pub(crate) mod transforms;
mod view_data;

use crate::app::types::ViewLineMapping;
use crate::app::BufferMetadata;
use crate::model::buffer::Buffer;
use crate::model::event::{BufferId, EventLog, LeafId, SplitDirection};
use crate::primitives::ansi_background::AnsiBackground;
use crate::state::EditorState;
use crate::view::split::SplitManager;
use ratatui::layout::Rect;
use ratatui::Frame;
use std::collections::HashMap;

/// Maximum line width before forced wrapping is applied, even when line wrapping is disabled.
/// This prevents memory exhaustion when opening files with extremely long lines (e.g., 10MB
/// single-line JSON files). Lines exceeding this width are wrapped into multiple visual lines,
/// each bounded to this width. 10,000 columns is far wider than any monitor while keeping
/// memory usage reasonable (~80KB per ViewLine instead of hundreds of MB).
const MAX_SAFE_LINE_WIDTH: usize = 10_000;

/// Public façade for split-pane rendering.
///
/// All logic lives in `orchestration::*`. This struct exists only to
/// preserve the `SplitRenderer::…` call sites in the rest of the crate;
/// nothing inside the `split_rendering` module references it.
pub struct SplitRenderer;

impl SplitRenderer {
    #[allow(clippy::too_many_arguments)]
    #[allow(clippy::type_complexity)]
    pub fn render_content(
        frame: &mut Frame,
        area: Rect,
        split_manager: &SplitManager,
        buffers: &mut HashMap<BufferId, EditorState>,
        buffer_metadata: &HashMap<BufferId, BufferMetadata>,
        event_logs: &mut HashMap<BufferId, EventLog>,
        composite_buffers: &mut HashMap<BufferId, crate::model::composite_buffer::CompositeBuffer>,
        composite_view_states: &mut HashMap<
            (LeafId, BufferId),
            crate::view::composite_view::CompositeViewState,
        >,
        theme: &crate::view::theme::Theme,
        ansi_background: Option<&AnsiBackground>,
        background_fade: f32,
        lsp_waiting: bool,
        large_file_threshold_bytes: u64,
        line_wrap: bool,
        estimated_line_length: usize,
        highlight_context_bytes: usize,
        split_view_states: Option<&mut HashMap<LeafId, crate::view::split::SplitViewState>>,
        grouped_subtrees: &HashMap<LeafId, crate::view::split::SplitNode>,
        hide_cursor: bool,
        hovered_tab: Option<(crate::view::split::TabTarget, LeafId, bool)>,
        hovered_close_split: Option<LeafId>,
        hovered_maximize_split: Option<LeafId>,
        is_maximized: bool,
        relative_line_numbers: bool,
        tab_bar_visible: bool,
        use_terminal_bg: bool,
        session_mode: bool,
        software_cursor_only: bool,
        show_vertical_scrollbar: bool,
        show_horizontal_scrollbar: bool,
        diagnostics_inline_text: bool,
        show_tilde: bool,
        highlight_current_column: bool,
        cell_theme_map: &mut Vec<crate::app::types::CellThemeInfo>,
        screen_width: u16,
        pending_hardware_cursor: &mut Option<(u16, u16)>,
    ) -> (
        Vec<(LeafId, BufferId, Rect, Rect, usize, usize)>,
        HashMap<LeafId, crate::view::ui::tabs::TabLayout>,
        Vec<(LeafId, u16, u16, u16)>,
        Vec<(LeafId, u16, u16, u16)>,
        HashMap<LeafId, Vec<ViewLineMapping>>,
        Vec<(LeafId, BufferId, Rect, usize, usize, usize)>,
        Vec<(
            crate::model::event::ContainerId,
            SplitDirection,
            u16,
            u16,
            u16,
        )>,
    ) {
        orchestration::render_content(
            frame,
            area,
            split_manager,
            buffers,
            buffer_metadata,
            event_logs,
            composite_buffers,
            composite_view_states,
            theme,
            ansi_background,
            background_fade,
            lsp_waiting,
            large_file_threshold_bytes,
            line_wrap,
            estimated_line_length,
            highlight_context_bytes,
            split_view_states,
            grouped_subtrees,
            hide_cursor,
            hovered_tab,
            hovered_close_split,
            hovered_maximize_split,
            is_maximized,
            relative_line_numbers,
            tab_bar_visible,
            use_terminal_bg,
            session_mode,
            software_cursor_only,
            show_vertical_scrollbar,
            show_horizontal_scrollbar,
            diagnostics_inline_text,
            show_tilde,
            highlight_current_column,
            cell_theme_map,
            screen_width,
            pending_hardware_cursor,
        )
    }

    #[allow(clippy::too_many_arguments)]
    pub fn compute_content_layout(
        area: Rect,
        split_manager: &SplitManager,
        buffers: &mut HashMap<BufferId, EditorState>,
        split_view_states: &mut HashMap<LeafId, crate::view::split::SplitViewState>,
        theme: &crate::view::theme::Theme,
        lsp_waiting: bool,
        estimated_line_length: usize,
        highlight_context_bytes: usize,
        relative_line_numbers: bool,
        use_terminal_bg: bool,
        session_mode: bool,
        software_cursor_only: bool,
        tab_bar_visible: bool,
        show_vertical_scrollbar: bool,
        show_horizontal_scrollbar: bool,
        diagnostics_inline_text: bool,
        show_tilde: bool,
    ) -> HashMap<LeafId, Vec<ViewLineMapping>> {
        orchestration::compute_content_layout(
            area,
            split_manager,
            buffers,
            split_view_states,
            theme,
            lsp_waiting,
            estimated_line_length,
            highlight_context_bytes,
            relative_line_numbers,
            use_terminal_bg,
            session_mode,
            software_cursor_only,
            tab_bar_visible,
            show_vertical_scrollbar,
            show_horizontal_scrollbar,
            diagnostics_inline_text,
            show_tilde,
        )
    }

    /// Render a single buffer into an arbitrary screen rect.
    ///
    /// Public façade over the per-leaf renderer for callers that
    /// drive layout outside of the split tree (e.g. the Live Grep
    /// floating overlay's preview pane — see render.rs). The leaf is
    /// not registered in `SplitManager`; the caller owns the
    /// `SplitViewState` and is responsible for cursor, viewport, and
    /// fold state. Returns the per-line mappings used for hit
    /// testing — overlay callers may discard them.
    #[allow(clippy::too_many_arguments)]
    pub fn render_phantom_leaf(
        frame: &mut Frame,
        state: &mut EditorState,
        cursors: &crate::model::cursor::Cursors,
        viewport: &mut crate::view::viewport::Viewport,
        folds: &mut crate::view::folding::FoldManager,
        event_log: Option<&mut EventLog>,
        area: Rect,
        theme: &crate::view::theme::Theme,
        ansi_background: Option<&AnsiBackground>,
        background_fade: f32,
        view_mode: crate::state::ViewMode,
        compose_width: Option<u16>,
        compose_column_guides: Option<Vec<u16>>,
        view_transform: Option<
            crate::services::plugins::api::ViewTransformPayload,
        >,
        estimated_line_length: usize,
        highlight_context_bytes: usize,
        buffer_id: BufferId,
        relative_line_numbers: bool,
        use_terminal_bg: bool,
        session_mode: bool,
        software_cursor_only: bool,
        rulers: &[usize],
        show_line_numbers: bool,
        highlight_current_line: bool,
        diagnostics_inline_text: bool,
        show_tilde: bool,
        highlight_current_column: bool,
        cell_theme_map: &mut Vec<crate::app::types::CellThemeInfo>,
        screen_width: u16,
    ) -> Vec<crate::app::types::ViewLineMapping> {
        // Phantom leaves are never the focused split, so:
        // - is_active = false (no current-line emphasis chrome owned
        //   by the focus split)
        // - hide_cursor = true (the user's cursor lives in the
        //   overlay's prompt input, not the preview)
        // - lsp_waiting = false (preview never owns LSP requests)
        // - pending_hardware_cursor: the preview must not move the
        //   terminal's hardware cursor away from the prompt input.
        let mut sink: Option<(u16, u16)> = None;
        orchestration::render_buffer_in_split(
            frame,
            state,
            cursors,
            viewport,
            folds,
            event_log,
            area,
            /* is_active */ false,
            theme,
            ansi_background,
            background_fade,
            /* lsp_waiting */ false,
            view_mode,
            compose_width,
            compose_column_guides,
            view_transform,
            estimated_line_length,
            highlight_context_bytes,
            buffer_id,
            /* hide_cursor */ true,
            relative_line_numbers,
            use_terminal_bg,
            session_mode,
            software_cursor_only,
            rulers,
            show_line_numbers,
            highlight_current_line,
            diagnostics_inline_text,
            show_tilde,
            highlight_current_column,
            cell_theme_map,
            screen_width,
            &mut sink,
        )
    }

    /// Public wrapper for building base tokens - used by render.rs for the
    /// view_transform_request hook.
    pub fn build_base_tokens_for_hook(
        buffer: &mut Buffer,
        top_byte: usize,
        estimated_line_length: usize,
        visible_count: usize,
        is_binary: bool,
        line_ending: crate::model::buffer::LineEnding,
    ) -> Vec<fresh_core::api::ViewTokenWire> {
        orchestration::build_base_tokens_for_hook(
            buffer,
            top_byte,
            estimated_line_length,
            visible_count,
            is_binary,
            line_ending,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::folding::fold_indicators_for_viewport;
    use super::layout::{calculate_view_anchor, calculate_viewport_end};
    use super::orchestration::overlays::{decoration_context, selection_context};
    use super::orchestration::render_buffer::resolve_cursor_fallback;
    use super::orchestration::render_line::{
        render_view_lines, LastLineEnd, LineRenderInput, LineRenderOutput,
    };
    use super::post_pass::apply_osc8_to_cells;
    use super::transforms::apply_wrapping_transform;
    use super::view_data::build_view_data;
    use super::*;

    use crate::model::buffer::{Buffer, LineEnding};
    use crate::model::filesystem::StdFileSystem;
    use crate::primitives::display_width::str_width;
    use crate::state::{EditorState, ViewMode};
    use crate::view::folding::FoldManager;
    use crate::view::theme;
    use crate::view::theme::Theme;
    use crate::view::ui::view_pipeline::{LineStart, ViewLine};
    use crate::view::viewport::Viewport;
    use fresh_core::api::ViewTokenWire;
    use lsp_types::FoldingRange;
    use std::collections::HashSet;
    use std::sync::Arc;

    fn test_fs() -> Arc<dyn crate::model::filesystem::FileSystem + Send + Sync> {
        Arc::new(StdFileSystem)
    }

    fn render_output_for(
        content: &str,
        cursor_pos: usize,
    ) -> (LineRenderOutput, usize, bool, usize) {
        render_output_for_with_gutters(content, cursor_pos, false)
    }

    fn render_output_for_with_gutters(
        content: &str,
        cursor_pos: usize,
        gutters_enabled: bool,
    ) -> (LineRenderOutput, usize, bool, usize) {
        let mut state = EditorState::new(20, 6, 1024, test_fs());
        state.buffer = Buffer::from_str(content, 1024, test_fs());
        let mut cursors = crate::model::cursor::Cursors::new();
        cursors.primary_mut().position = cursor_pos.min(state.buffer.len());
        // Create a standalone viewport (no longer part of EditorState)
        let viewport = Viewport::new(20, 4);
        // Enable/disable line numbers/gutters based on parameter
        state.margins.left_config.enabled = gutters_enabled;

        let render_area = Rect::new(0, 0, 20, 4);
        let visible_count = viewport.visible_line_count();
        let gutter_width = state.margins.left_total_width();
        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();
        let empty_folds = FoldManager::new();

        let view_data = build_view_data(
            &mut state,
            &viewport,
            None,
            content.len().max(1),
            visible_count,
            false, // line wrap disabled for tests
            render_area.width as usize,
            gutter_width,
            &ViewMode::Source, // Tests use source mode
            &empty_folds,
            &theme,
        );
        let view_anchor = calculate_view_anchor(&view_data.lines, 0);

        let estimated_lines = (state.buffer.len() / state.buffer.estimated_line_length()).max(1);
        state.margins.update_width_for_buffer(estimated_lines, true);
        let gutter_width = state.margins.left_total_width();

        let selection = selection_context(&state, &cursors);
        let _ = state
            .buffer
            .populate_line_cache(viewport.top_byte, visible_count);
        let viewport_start = viewport.top_byte;
        let viewport_end = calculate_viewport_end(
            &mut state,
            viewport_start,
            content.len().max(1),
            visible_count,
        );
        let decorations = decoration_context(
            &mut state,
            viewport_start,
            viewport_end,
            selection.primary_cursor_position,
            &empty_folds,
            &theme,
            100_000,           // default highlight context bytes
            &ViewMode::Source, // Tests use source mode
            false,             // inline diagnostics off for test
            &[],
        );

        let mut dummy_theme_map = Vec::new();
        let output = render_view_lines(LineRenderInput {
            state: &state,
            theme: &theme,
            view_lines: &view_data.lines,
            view_anchor,
            render_area,
            gutter_width,
            selection: &selection,
            decorations: &decorations,
            visible_line_count: visible_count,
            lsp_waiting: false,
            is_active: true,
            line_wrap: viewport.line_wrap_enabled,
            estimated_lines,
            left_column: viewport.left_column,
            relative_line_numbers: false,
            session_mode: false,
            software_cursor_only: false,
            show_line_numbers: true, // Tests show line numbers
            byte_offset_mode: false, // Tests use exact line numbers
            show_tilde: true,
            highlight_current_line: true,
            cell_theme_map: &mut dummy_theme_map,
            screen_width: 0,
        });

        (
            output,
            state.buffer.len(),
            content.ends_with('\n'),
            selection.primary_cursor_position,
        )
    }

    #[test]
    fn test_folding_hides_lines_and_adds_placeholder() {
        let content = "header\nline1\nline2\ntail\n";
        let mut state = EditorState::new(40, 6, 1024, test_fs());
        state.buffer = Buffer::from_str(content, 1024, test_fs());

        let start = state.buffer.line_start_offset(1).unwrap();
        let end = state.buffer.line_start_offset(3).unwrap();
        let mut folds = FoldManager::new();
        folds.add(&mut state.marker_list, start, end, Some("...".to_string()));

        let viewport = Viewport::new(40, 6);
        let gutter_width = state.margins.left_total_width();
        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();
        let view_data = build_view_data(
            &mut state,
            &viewport,
            None,
            content.len().max(1),
            viewport.visible_line_count(),
            false,
            40,
            gutter_width,
            &ViewMode::Source,
            &folds,
            &theme,
        );

        let lines: Vec<String> = view_data.lines.iter().map(|l| l.text.clone()).collect();
        assert!(lines.iter().any(|l| l.contains("header")));
        assert!(lines.iter().any(|l| l.contains("tail")));
        assert!(!lines.iter().any(|l| l.contains("line1")));
        assert!(!lines.iter().any(|l| l.contains("line2")));
        assert!(lines
            .iter()
            .any(|l| l.contains("header") && l.contains("...")));
    }

    #[test]
    fn test_fold_indicators_collapsed_and_expanded() {
        let content = "a\nb\nc\nd\n";
        let mut state = EditorState::new(40, 6, 1024, test_fs());
        state.buffer = Buffer::from_str(content, 1024, test_fs());

        let lsp_ranges = vec![
            FoldingRange {
                start_line: 0,
                end_line: 1,
                start_character: None,
                end_character: None,
                kind: None,
                collapsed_text: None,
            },
            FoldingRange {
                start_line: 1,
                end_line: 2,
                start_character: None,
                end_character: None,
                kind: None,
                collapsed_text: None,
            },
        ];
        state
            .folding_ranges
            .set_from_lsp(&state.buffer, &mut state.marker_list, lsp_ranges);

        let start = state.buffer.line_start_offset(1).unwrap();
        let end = state.buffer.line_start_offset(2).unwrap();
        let mut folds = FoldManager::new();
        folds.add(&mut state.marker_list, start, end, None);

        let line1_byte = state.buffer.line_start_offset(1).unwrap();
        let view_lines = vec![ViewLine {
            text: "b\n".to_string(),
            source_start_byte: Some(line1_byte),
            char_source_bytes: vec![Some(line1_byte), Some(line1_byte + 1)],
            char_styles: vec![None, None],
            char_visual_cols: vec![0, 1],
            visual_to_char: vec![0, 1],
            tab_starts: HashSet::new(),
            line_start: LineStart::AfterSourceNewline,
            ends_with_newline: true,
        }];

        let indicators = fold_indicators_for_viewport(&state, &folds, &view_lines);

        // Collapsed fold: header is line 0 (byte 0)
        assert_eq!(indicators.get(&0).map(|i| i.collapsed), Some(true));
        // LSP range starting at line 1 (byte 2, since "a\n" is 2 bytes)
        assert_eq!(
            indicators.get(&line1_byte).map(|i| i.collapsed),
            Some(false)
        );
    }

    #[test]
    fn last_line_end_tracks_trailing_newline() {
        let output = render_output_for("abc\n", 4);
        assert_eq!(
            output.0.last_line_end,
            Some(LastLineEnd {
                pos: (3, 0),
                terminated_with_newline: true
            })
        );
    }

    #[test]
    fn last_line_end_tracks_no_trailing_newline() {
        let output = render_output_for("abc", 3);
        assert_eq!(
            output.0.last_line_end,
            Some(LastLineEnd {
                pos: (3, 0),
                terminated_with_newline: false
            })
        );
    }

    #[test]
    fn cursor_after_newline_places_on_next_line() {
        let (output, buffer_len, buffer_newline, cursor_pos) = render_output_for("abc\n", 4);
        let cursor = resolve_cursor_fallback(
            output.cursor,
            cursor_pos,
            buffer_len,
            buffer_newline,
            output.last_line_end,
            output.content_lines_rendered,
            0, // gutter_width (gutters disabled in tests)
        );
        assert_eq!(cursor, Some((0, 1)));
    }

    #[test]
    fn cursor_at_end_without_newline_stays_on_line() {
        let (output, buffer_len, buffer_newline, cursor_pos) = render_output_for("abc", 3);
        let cursor = resolve_cursor_fallback(
            output.cursor,
            cursor_pos,
            buffer_len,
            buffer_newline,
            output.last_line_end,
            output.content_lines_rendered,
            0, // gutter_width (gutters disabled in tests)
        );
        assert_eq!(cursor, Some((3, 0)));
    }

    // Helper to count all cursor positions in rendered output
    // Cursors can appear as:
    // 1. Primary cursor in output.cursor (hardware cursor position)
    // 2. Visual spans with REVERSED modifier (secondary cursors, or primary cursor with contrast fix)
    // 3. Visual spans with special background color (inactive cursors)
    fn count_all_cursors(output: &LineRenderOutput) -> Vec<(u16, u16)> {
        let mut cursor_positions = Vec::new();

        // Check for primary cursor in output.cursor field
        let primary_cursor = output.cursor;
        if let Some(cursor_pos) = primary_cursor {
            cursor_positions.push(cursor_pos);
        }

        // Check for visual cursor indicators in rendered spans (secondary/inactive cursors)
        for (line_idx, line) in output.lines.iter().enumerate() {
            let mut col = 0u16;
            for span in line.spans.iter() {
                // Check if this span has the REVERSED modifier (secondary cursor)
                if span
                    .style
                    .add_modifier
                    .contains(ratatui::style::Modifier::REVERSED)
                {
                    let pos = (col, line_idx as u16);
                    // Only add if this is not the primary cursor position
                    // (primary cursor may also have REVERSED for contrast)
                    if primary_cursor != Some(pos) {
                        cursor_positions.push(pos);
                    }
                }
                // Count the visual width of this span's content
                col += str_width(&span.content) as u16;
            }
        }

        cursor_positions
    }

    // Helper to dump rendered output for debugging
    fn dump_render_output(content: &str, cursor_pos: usize, output: &LineRenderOutput) {
        eprintln!("\n=== RENDER DEBUG ===");
        eprintln!("Content: {:?}", content);
        eprintln!("Cursor position: {}", cursor_pos);
        eprintln!("Hardware cursor (output.cursor): {:?}", output.cursor);
        eprintln!("Last line end: {:?}", output.last_line_end);
        eprintln!("Content lines rendered: {}", output.content_lines_rendered);
        eprintln!("\nRendered lines:");
        for (line_idx, line) in output.lines.iter().enumerate() {
            eprintln!("  Line {}: {} spans", line_idx, line.spans.len());
            for (span_idx, span) in line.spans.iter().enumerate() {
                let has_reversed = span
                    .style
                    .add_modifier
                    .contains(ratatui::style::Modifier::REVERSED);
                let bg_color = format!("{:?}", span.style.bg);
                eprintln!(
                    "    Span {}: {:?} (REVERSED: {}, BG: {})",
                    span_idx, span.content, has_reversed, bg_color
                );
            }
        }
        eprintln!("===================\n");
    }

    // Helper to get final cursor position after fallback resolution
    // Also validates that exactly one cursor is present
    fn get_final_cursor(content: &str, cursor_pos: usize) -> Option<(u16, u16)> {
        let (output, buffer_len, buffer_newline, cursor_pos) =
            render_output_for(content, cursor_pos);

        // Count all cursors (hardware + visual) in the rendered output
        let all_cursors = count_all_cursors(&output);

        // Validate that at most one cursor is present in rendered output
        // (Some cursors are added by fallback logic, not during rendering)
        assert!(
            all_cursors.len() <= 1,
            "Expected at most 1 cursor in rendered output, found {} at positions: {:?}",
            all_cursors.len(),
            all_cursors
        );

        let final_cursor = resolve_cursor_fallback(
            output.cursor,
            cursor_pos,
            buffer_len,
            buffer_newline,
            output.last_line_end,
            output.content_lines_rendered,
            0, // gutter_width (gutters disabled in tests)
        );

        // Debug dump if we find unexpected results
        if all_cursors.len() > 1 || (all_cursors.len() == 1 && Some(all_cursors[0]) != final_cursor)
        {
            dump_render_output(content, cursor_pos, &output);
        }

        // If a cursor was rendered, it should match the final cursor position
        if let Some(rendered_cursor) = all_cursors.first() {
            assert_eq!(
                Some(*rendered_cursor),
                final_cursor,
                "Rendered cursor at {:?} doesn't match final cursor {:?}",
                rendered_cursor,
                final_cursor
            );
        }

        // Validate that we have a final cursor position (either rendered or from fallback)
        assert!(
            final_cursor.is_some(),
            "Expected a final cursor position, but got None. Rendered cursors: {:?}",
            all_cursors
        );

        final_cursor
    }

    // Helper to simulate typing a character and check if it appears at cursor position
    fn check_typing_at_cursor(
        content: &str,
        cursor_pos: usize,
        char_to_type: char,
    ) -> (Option<(u16, u16)>, String) {
        // Get cursor position before typing
        let cursor_before = get_final_cursor(content, cursor_pos);

        // Simulate inserting the character at cursor position
        let mut new_content = content.to_string();
        if cursor_pos <= content.len() {
            new_content.insert(cursor_pos, char_to_type);
        }

        (cursor_before, new_content)
    }

    #[test]
    fn e2e_cursor_at_start_of_nonempty_line() {
        // "abc" with cursor at position 0 (before 'a')
        let cursor = get_final_cursor("abc", 0);
        assert_eq!(cursor, Some((0, 0)), "Cursor should be at column 0, line 0");

        let (cursor_pos, new_content) = check_typing_at_cursor("abc", 0, 'X');
        assert_eq!(
            new_content, "Xabc",
            "Typing should insert at cursor position"
        );
        assert_eq!(cursor_pos, Some((0, 0)));
    }

    #[test]
    fn e2e_cursor_in_middle_of_line() {
        // "abc" with cursor at position 1 (on 'b')
        let cursor = get_final_cursor("abc", 1);
        assert_eq!(cursor, Some((1, 0)), "Cursor should be at column 1, line 0");

        let (cursor_pos, new_content) = check_typing_at_cursor("abc", 1, 'X');
        assert_eq!(
            new_content, "aXbc",
            "Typing should insert at cursor position"
        );
        assert_eq!(cursor_pos, Some((1, 0)));
    }

    #[test]
    fn e2e_cursor_at_end_of_line_no_newline() {
        // "abc" with cursor at position 3 (after 'c', at EOF)
        let cursor = get_final_cursor("abc", 3);
        assert_eq!(
            cursor,
            Some((3, 0)),
            "Cursor should be at column 3, line 0 (after last char)"
        );

        let (cursor_pos, new_content) = check_typing_at_cursor("abc", 3, 'X');
        assert_eq!(new_content, "abcX", "Typing should append at end");
        assert_eq!(cursor_pos, Some((3, 0)));
    }

    #[test]
    fn e2e_cursor_at_empty_line() {
        // "\n" with cursor at position 0 (on the newline itself)
        let cursor = get_final_cursor("\n", 0);
        assert_eq!(
            cursor,
            Some((0, 0)),
            "Cursor on empty line should be at column 0"
        );

        let (cursor_pos, new_content) = check_typing_at_cursor("\n", 0, 'X');
        assert_eq!(new_content, "X\n", "Typing should insert before newline");
        assert_eq!(cursor_pos, Some((0, 0)));
    }

    #[test]
    fn e2e_cursor_after_newline_at_eof() {
        // "abc\n" with cursor at position 4 (after newline, at EOF)
        let cursor = get_final_cursor("abc\n", 4);
        assert_eq!(
            cursor,
            Some((0, 1)),
            "Cursor after newline at EOF should be on next line"
        );

        let (cursor_pos, new_content) = check_typing_at_cursor("abc\n", 4, 'X');
        assert_eq!(new_content, "abc\nX", "Typing should insert on new line");
        assert_eq!(cursor_pos, Some((0, 1)));
    }

    #[test]
    fn e2e_cursor_on_newline_with_content() {
        // "abc\n" with cursor at position 3 (on the newline character)
        let cursor = get_final_cursor("abc\n", 3);
        assert_eq!(
            cursor,
            Some((3, 0)),
            "Cursor on newline after content should be after last char"
        );

        let (cursor_pos, new_content) = check_typing_at_cursor("abc\n", 3, 'X');
        assert_eq!(new_content, "abcX\n", "Typing should insert before newline");
        assert_eq!(cursor_pos, Some((3, 0)));
    }

    #[test]
    fn e2e_cursor_multiline_start_of_second_line() {
        // "abc\ndef" with cursor at position 4 (start of second line, on 'd')
        let cursor = get_final_cursor("abc\ndef", 4);
        assert_eq!(
            cursor,
            Some((0, 1)),
            "Cursor at start of second line should be at column 0, line 1"
        );

        let (cursor_pos, new_content) = check_typing_at_cursor("abc\ndef", 4, 'X');
        assert_eq!(
            new_content, "abc\nXdef",
            "Typing should insert at start of second line"
        );
        assert_eq!(cursor_pos, Some((0, 1)));
    }

    #[test]
    fn e2e_cursor_multiline_end_of_first_line() {
        // "abc\ndef" with cursor at position 3 (on newline of first line)
        let cursor = get_final_cursor("abc\ndef", 3);
        assert_eq!(
            cursor,
            Some((3, 0)),
            "Cursor on newline of first line should be after content"
        );

        let (cursor_pos, new_content) = check_typing_at_cursor("abc\ndef", 3, 'X');
        assert_eq!(
            new_content, "abcX\ndef",
            "Typing should insert before newline"
        );
        assert_eq!(cursor_pos, Some((3, 0)));
    }

    #[test]
    fn e2e_cursor_empty_buffer() {
        // Empty buffer with cursor at position 0
        let cursor = get_final_cursor("", 0);
        assert_eq!(
            cursor,
            Some((0, 0)),
            "Cursor in empty buffer should be at origin"
        );

        let (cursor_pos, new_content) = check_typing_at_cursor("", 0, 'X');
        assert_eq!(
            new_content, "X",
            "Typing in empty buffer should insert character"
        );
        assert_eq!(cursor_pos, Some((0, 0)));
    }

    #[test]
    fn e2e_cursor_empty_buffer_with_gutters() {
        // Empty buffer with cursor at position 0, with gutters enabled
        // The cursor should be positioned at the gutter width (right after the gutter),
        // NOT at column 0 (which would be in the gutter area)
        let (output, buffer_len, buffer_newline, cursor_pos) =
            render_output_for_with_gutters("", 0, true);

        // With gutters enabled, the gutter width should be > 0
        // Default gutter includes: 1 char indicator + line number width + separator
        // For a 1-line buffer, line number width is typically 1 digit + padding
        let gutter_width = {
            let mut state = EditorState::new(20, 6, 1024, test_fs());
            state.margins.left_config.enabled = true;
            state.margins.update_width_for_buffer(1, true);
            state.margins.left_total_width()
        };
        assert!(gutter_width > 0, "Gutter width should be > 0 when enabled");

        // CRITICAL: Check the RENDERED cursor position directly from output.cursor
        // This is what the terminal will actually use for cursor positioning
        // The cursor should be rendered at gutter_width, not at 0
        assert_eq!(
            output.cursor,
            Some((gutter_width as u16, 0)),
            "RENDERED cursor in empty buffer should be at gutter_width ({}), got {:?}",
            gutter_width,
            output.cursor
        );

        let final_cursor = resolve_cursor_fallback(
            output.cursor,
            cursor_pos,
            buffer_len,
            buffer_newline,
            output.last_line_end,
            output.content_lines_rendered,
            gutter_width,
        );

        // Cursor should be at (gutter_width, 0) - right after the gutter on line 0
        assert_eq!(
            final_cursor,
            Some((gutter_width as u16, 0)),
            "Cursor in empty buffer with gutters should be at gutter_width, not column 0"
        );
    }

    #[test]
    fn e2e_cursor_between_empty_lines() {
        // "\n\n" with cursor at position 1 (on second newline)
        let cursor = get_final_cursor("\n\n", 1);
        assert_eq!(cursor, Some((0, 1)), "Cursor on second empty line");

        let (cursor_pos, new_content) = check_typing_at_cursor("\n\n", 1, 'X');
        assert_eq!(new_content, "\nX\n", "Typing should insert on second line");
        assert_eq!(cursor_pos, Some((0, 1)));
    }

    #[test]
    fn e2e_cursor_at_eof_after_multiple_lines() {
        // "abc\ndef\nghi" with cursor at position 11 (at EOF, no trailing newline)
        let cursor = get_final_cursor("abc\ndef\nghi", 11);
        assert_eq!(
            cursor,
            Some((3, 2)),
            "Cursor at EOF after 'i' should be at column 3, line 2"
        );

        let (cursor_pos, new_content) = check_typing_at_cursor("abc\ndef\nghi", 11, 'X');
        assert_eq!(new_content, "abc\ndef\nghiX", "Typing should append at end");
        assert_eq!(cursor_pos, Some((3, 2)));
    }

    #[test]
    fn e2e_cursor_at_eof_with_trailing_newline() {
        // "abc\ndef\nghi\n" with cursor at position 12 (after trailing newline)
        let cursor = get_final_cursor("abc\ndef\nghi\n", 12);
        assert_eq!(
            cursor,
            Some((0, 3)),
            "Cursor after trailing newline should be on line 3"
        );

        let (cursor_pos, new_content) = check_typing_at_cursor("abc\ndef\nghi\n", 12, 'X');
        assert_eq!(
            new_content, "abc\ndef\nghi\nX",
            "Typing should insert on new line"
        );
        assert_eq!(cursor_pos, Some((0, 3)));
    }

    #[test]
    fn e2e_jump_to_end_of_buffer_no_trailing_newline() {
        // Simulate Ctrl+End: jump from start to end of buffer without trailing newline
        let content = "abc\ndef\nghi";

        // Start at position 0
        let cursor_at_start = get_final_cursor(content, 0);
        assert_eq!(cursor_at_start, Some((0, 0)), "Cursor starts at beginning");

        // Jump to EOF (position 11, after 'i')
        let cursor_at_eof = get_final_cursor(content, 11);
        assert_eq!(
            cursor_at_eof,
            Some((3, 2)),
            "After Ctrl+End, cursor at column 3, line 2"
        );

        // Type a character at EOF
        let (cursor_before_typing, new_content) = check_typing_at_cursor(content, 11, 'X');
        assert_eq!(cursor_before_typing, Some((3, 2)));
        assert_eq!(new_content, "abc\ndef\nghiX", "Character appended at end");

        // Verify cursor position in the new content
        let cursor_after_typing = get_final_cursor(&new_content, 12);
        assert_eq!(
            cursor_after_typing,
            Some((4, 2)),
            "After typing, cursor moved to column 4"
        );

        // Move cursor to start of buffer - verify cursor is no longer at end
        let cursor_moved_away = get_final_cursor(&new_content, 0);
        assert_eq!(cursor_moved_away, Some((0, 0)), "Cursor moved to start");
        // The cursor should NOT be at the end anymore - verify by rendering without cursor at end
        // This implicitly tests that only one cursor is rendered
    }

    #[test]
    fn e2e_jump_to_end_of_buffer_with_trailing_newline() {
        // Simulate Ctrl+End: jump from start to end of buffer WITH trailing newline
        let content = "abc\ndef\nghi\n";

        // Start at position 0
        let cursor_at_start = get_final_cursor(content, 0);
        assert_eq!(cursor_at_start, Some((0, 0)), "Cursor starts at beginning");

        // Jump to EOF (position 12, after trailing newline)
        let cursor_at_eof = get_final_cursor(content, 12);
        assert_eq!(
            cursor_at_eof,
            Some((0, 3)),
            "After Ctrl+End, cursor at column 0, line 3 (new line)"
        );

        // Type a character at EOF
        let (cursor_before_typing, new_content) = check_typing_at_cursor(content, 12, 'X');
        assert_eq!(cursor_before_typing, Some((0, 3)));
        assert_eq!(
            new_content, "abc\ndef\nghi\nX",
            "Character inserted on new line"
        );

        // After typing, the cursor should move forward
        let cursor_after_typing = get_final_cursor(&new_content, 13);
        assert_eq!(
            cursor_after_typing,
            Some((1, 3)),
            "After typing, cursor should be at column 1, line 3"
        );

        // Move cursor to middle of buffer - verify cursor is no longer at end
        let cursor_moved_away = get_final_cursor(&new_content, 4);
        assert_eq!(
            cursor_moved_away,
            Some((0, 1)),
            "Cursor moved to start of line 1 (position 4 = start of 'def')"
        );
    }

    #[test]
    fn e2e_jump_to_end_of_empty_buffer() {
        // Edge case: Ctrl+End in empty buffer should stay at (0,0)
        let content = "";

        let cursor_at_eof = get_final_cursor(content, 0);
        assert_eq!(
            cursor_at_eof,
            Some((0, 0)),
            "Empty buffer: cursor at origin"
        );

        // Type a character
        let (cursor_before_typing, new_content) = check_typing_at_cursor(content, 0, 'X');
        assert_eq!(cursor_before_typing, Some((0, 0)));
        assert_eq!(new_content, "X", "Character inserted");

        // Verify cursor after typing
        let cursor_after_typing = get_final_cursor(&new_content, 1);
        assert_eq!(
            cursor_after_typing,
            Some((1, 0)),
            "After typing, cursor at column 1"
        );

        // Move cursor back to start - verify cursor is no longer at end
        let cursor_moved_away = get_final_cursor(&new_content, 0);
        assert_eq!(
            cursor_moved_away,
            Some((0, 0)),
            "Cursor moved back to start"
        );
    }

    #[test]
    fn e2e_jump_to_end_of_single_empty_line() {
        // Edge case: buffer with just a newline
        let content = "\n";

        // Position 0 is ON the newline
        let cursor_on_newline = get_final_cursor(content, 0);
        assert_eq!(
            cursor_on_newline,
            Some((0, 0)),
            "Cursor on the newline character"
        );

        // Position 1 is AFTER the newline (EOF)
        let cursor_at_eof = get_final_cursor(content, 1);
        assert_eq!(
            cursor_at_eof,
            Some((0, 1)),
            "After Ctrl+End, cursor on line 1"
        );

        // Type at EOF
        let (cursor_before_typing, new_content) = check_typing_at_cursor(content, 1, 'X');
        assert_eq!(cursor_before_typing, Some((0, 1)));
        assert_eq!(new_content, "\nX", "Character on second line");

        let cursor_after_typing = get_final_cursor(&new_content, 2);
        assert_eq!(
            cursor_after_typing,
            Some((1, 1)),
            "After typing, cursor at column 1, line 1"
        );

        // Move cursor to the newline - verify cursor is no longer at end
        let cursor_moved_away = get_final_cursor(&new_content, 0);
        assert_eq!(
            cursor_moved_away,
            Some((0, 0)),
            "Cursor moved to the newline on line 0"
        );
    }
    // NOTE: Tests for view transform header handling have been moved to src/ui/view_pipeline.rs
    // where the elegant token-based pipeline properly handles these cases.
    // The view_pipeline tests cover:
    // - test_simple_source_lines
    // - test_wrapped_continuation
    // - test_injected_header_then_source
    // - test_mixed_scenario

    // ==================== CRLF Tokenization Tests ====================

    use fresh_core::api::ViewTokenWireKind;

    /// Helper to extract source_offset from tokens for easier assertion
    fn extract_token_offsets(tokens: &[ViewTokenWire]) -> Vec<(String, Option<usize>)> {
        tokens
            .iter()
            .map(|t| {
                let kind_str = match &t.kind {
                    ViewTokenWireKind::Text(s) => format!("Text({})", s),
                    ViewTokenWireKind::Newline => "Newline".to_string(),
                    ViewTokenWireKind::Space => "Space".to_string(),
                    ViewTokenWireKind::Break => "Break".to_string(),
                    ViewTokenWireKind::BinaryByte(b) => format!("Byte(0x{:02x})", b),
                };
                (kind_str, t.source_offset)
            })
            .collect()
    }

    /// Test tokenization of CRLF content with a single line.
    /// Verifies that Newline token is at \r position and \n is skipped.
    #[test]
    fn test_build_base_tokens_crlf_single_line() {
        // Content: "abc\r\n" (5 bytes: a=0, b=1, c=2, \r=3, \n=4)
        let content = b"abc\r\n";
        let mut buffer = Buffer::from_bytes(content.to_vec(), test_fs());
        buffer.set_line_ending(LineEnding::CRLF);

        let tokens = SplitRenderer::build_base_tokens_for_hook(
            &mut buffer,
            0,     // top_byte
            80,    // estimated_line_length
            10,    // visible_count
            false, // is_binary
            LineEnding::CRLF,
        );

        let offsets = extract_token_offsets(&tokens);

        // Should have: Text("abc") at 0, Newline at 3
        // The \n at byte 4 should be skipped
        assert!(
            offsets
                .iter()
                .any(|(kind, off)| kind == "Text(abc)" && *off == Some(0)),
            "Expected Text(abc) at offset 0, got: {:?}",
            offsets
        );
        assert!(
            offsets
                .iter()
                .any(|(kind, off)| kind == "Newline" && *off == Some(3)),
            "Expected Newline at offset 3 (\\r position), got: {:?}",
            offsets
        );

        // Verify there's only one Newline token
        let newline_count = offsets.iter().filter(|(k, _)| k == "Newline").count();
        assert_eq!(
            newline_count, 1,
            "Should have exactly 1 Newline token for CRLF, got {}: {:?}",
            newline_count, offsets
        );
    }

    /// Test tokenization of CRLF content with multiple lines.
    /// This verifies that source_offset correctly accumulates across lines.
    #[test]
    fn test_build_base_tokens_crlf_multiple_lines() {
        // Content: "abc\r\ndef\r\nghi\r\n" (15 bytes)
        // Line 1: a=0, b=1, c=2, \r=3, \n=4
        // Line 2: d=5, e=6, f=7, \r=8, \n=9
        // Line 3: g=10, h=11, i=12, \r=13, \n=14
        let content = b"abc\r\ndef\r\nghi\r\n";
        let mut buffer = Buffer::from_bytes(content.to_vec(), test_fs());
        buffer.set_line_ending(LineEnding::CRLF);

        let tokens = SplitRenderer::build_base_tokens_for_hook(
            &mut buffer,
            0,
            80,
            10,
            false,
            LineEnding::CRLF,
        );

        let offsets = extract_token_offsets(&tokens);

        // Expected tokens:
        // Text("abc") at 0, Newline at 3
        // Text("def") at 5, Newline at 8
        // Text("ghi") at 10, Newline at 13

        // Verify line 1 tokens
        assert!(
            offsets
                .iter()
                .any(|(kind, off)| kind == "Text(abc)" && *off == Some(0)),
            "Line 1: Expected Text(abc) at 0, got: {:?}",
            offsets
        );
        assert!(
            offsets
                .iter()
                .any(|(kind, off)| kind == "Newline" && *off == Some(3)),
            "Line 1: Expected Newline at 3, got: {:?}",
            offsets
        );

        // Verify line 2 tokens - THIS IS WHERE OFFSET DRIFT WOULD APPEAR
        assert!(
            offsets
                .iter()
                .any(|(kind, off)| kind == "Text(def)" && *off == Some(5)),
            "Line 2: Expected Text(def) at 5, got: {:?}",
            offsets
        );
        assert!(
            offsets
                .iter()
                .any(|(kind, off)| kind == "Newline" && *off == Some(8)),
            "Line 2: Expected Newline at 8, got: {:?}",
            offsets
        );

        // Verify line 3 tokens - DRIFT ACCUMULATES HERE
        assert!(
            offsets
                .iter()
                .any(|(kind, off)| kind == "Text(ghi)" && *off == Some(10)),
            "Line 3: Expected Text(ghi) at 10, got: {:?}",
            offsets
        );
        assert!(
            offsets
                .iter()
                .any(|(kind, off)| kind == "Newline" && *off == Some(13)),
            "Line 3: Expected Newline at 13, got: {:?}",
            offsets
        );

        // Verify exactly 3 Newline tokens
        let newline_count = offsets.iter().filter(|(k, _)| k == "Newline").count();
        assert_eq!(newline_count, 3, "Should have 3 Newline tokens");
    }

    /// Test tokenization of LF content to compare with CRLF.
    /// LF mode should NOT skip anything - each character gets its own offset.
    #[test]
    fn test_build_base_tokens_lf_mode_for_comparison() {
        // Content: "abc\ndef\n" (8 bytes)
        // Line 1: a=0, b=1, c=2, \n=3
        // Line 2: d=4, e=5, f=6, \n=7
        let content = b"abc\ndef\n";
        let mut buffer = Buffer::from_bytes(content.to_vec(), test_fs());
        buffer.set_line_ending(LineEnding::LF);

        let tokens = SplitRenderer::build_base_tokens_for_hook(
            &mut buffer,
            0,
            80,
            10,
            false,
            LineEnding::LF,
        );

        let offsets = extract_token_offsets(&tokens);

        // Verify LF offsets
        assert!(
            offsets
                .iter()
                .any(|(kind, off)| kind == "Text(abc)" && *off == Some(0)),
            "LF Line 1: Expected Text(abc) at 0"
        );
        assert!(
            offsets
                .iter()
                .any(|(kind, off)| kind == "Newline" && *off == Some(3)),
            "LF Line 1: Expected Newline at 3"
        );
        assert!(
            offsets
                .iter()
                .any(|(kind, off)| kind == "Text(def)" && *off == Some(4)),
            "LF Line 2: Expected Text(def) at 4"
        );
        assert!(
            offsets
                .iter()
                .any(|(kind, off)| kind == "Newline" && *off == Some(7)),
            "LF Line 2: Expected Newline at 7"
        );
    }

    /// Test that CRLF in LF-mode file shows \r as control character.
    /// This verifies that \r is rendered as <0D> in LF files.
    #[test]
    fn test_build_base_tokens_crlf_in_lf_mode_shows_control_char() {
        // Content: "abc\r\n" but buffer is in LF mode
        let content = b"abc\r\n";
        let mut buffer = Buffer::from_bytes(content.to_vec(), test_fs());
        buffer.set_line_ending(LineEnding::LF); // Force LF mode

        let tokens = SplitRenderer::build_base_tokens_for_hook(
            &mut buffer,
            0,
            80,
            10,
            false,
            LineEnding::LF,
        );

        let offsets = extract_token_offsets(&tokens);

        // In LF mode, \r should be rendered as BinaryByte(0x0d)
        assert!(
            offsets.iter().any(|(kind, _)| kind == "Byte(0x0d)"),
            "LF mode should render \\r as control char <0D>, got: {:?}",
            offsets
        );
    }

    /// Test tokenization starting from middle of file (top_byte != 0).
    /// Verifies that source_offset is correct even when not starting from byte 0.
    #[test]
    fn test_build_base_tokens_crlf_from_middle() {
        // Content: "abc\r\ndef\r\nghi\r\n" (15 bytes)
        // Start from byte 5 (beginning of "def")
        let content = b"abc\r\ndef\r\nghi\r\n";
        let mut buffer = Buffer::from_bytes(content.to_vec(), test_fs());
        buffer.set_line_ending(LineEnding::CRLF);

        let tokens = SplitRenderer::build_base_tokens_for_hook(
            &mut buffer,
            5, // Start from line 2
            80,
            10,
            false,
            LineEnding::CRLF,
        );

        let offsets = extract_token_offsets(&tokens);

        // Should have:
        // Text("def") at 5, Newline at 8
        // Text("ghi") at 10, Newline at 13
        assert!(
            offsets
                .iter()
                .any(|(kind, off)| kind == "Text(def)" && *off == Some(5)),
            "Starting from byte 5: Expected Text(def) at 5, got: {:?}",
            offsets
        );
        assert!(
            offsets
                .iter()
                .any(|(kind, off)| kind == "Text(ghi)" && *off == Some(10)),
            "Starting from byte 5: Expected Text(ghi) at 10, got: {:?}",
            offsets
        );
    }

    /// End-to-end test: verify full pipeline from CRLF buffer to ViewLine to highlighting lookup
    /// This test simulates the complete flow that would trigger the offset drift bug.
    #[test]
    fn test_crlf_highlight_span_lookup() {
        use crate::view::ui::view_pipeline::ViewLineIterator;

        // Simulate Java-like CRLF content:
        // "int x;\r\nint y;\r\n"
        // Bytes: i=0, n=1, t=2, ' '=3, x=4, ;=5, \r=6, \n=7,
        //        i=8, n=9, t=10, ' '=11, y=12, ;=13, \r=14, \n=15
        let content = b"int x;\r\nint y;\r\n";
        let mut buffer = Buffer::from_bytes(content.to_vec(), test_fs());
        buffer.set_line_ending(LineEnding::CRLF);

        // Step 1: Generate tokens
        let tokens = SplitRenderer::build_base_tokens_for_hook(
            &mut buffer,
            0,
            80,
            10,
            false,
            LineEnding::CRLF,
        );

        // Verify tokens have correct offsets
        let offsets = extract_token_offsets(&tokens);
        eprintln!("Tokens: {:?}", offsets);

        // Step 2: Convert tokens to ViewLines
        let view_lines: Vec<_> = ViewLineIterator::new(&tokens, false, false, 4, false).collect();
        assert_eq!(view_lines.len(), 2, "Should have 2 view lines");

        // Step 3: Verify char_source_bytes mapping for each line
        // Line 1: "int x;\n" displayed, maps to bytes 0-6
        eprintln!(
            "Line 1 char_source_bytes: {:?}",
            view_lines[0].char_source_bytes
        );
        assert_eq!(
            view_lines[0].char_source_bytes.len(),
            7,
            "Line 1 should have 7 chars: 'i','n','t',' ','x',';','\\n'"
        );
        // Check specific mappings
        assert_eq!(
            view_lines[0].char_source_bytes[0],
            Some(0),
            "Line 1 'i' -> byte 0"
        );
        assert_eq!(
            view_lines[0].char_source_bytes[4],
            Some(4),
            "Line 1 'x' -> byte 4"
        );
        assert_eq!(
            view_lines[0].char_source_bytes[5],
            Some(5),
            "Line 1 ';' -> byte 5"
        );
        assert_eq!(
            view_lines[0].char_source_bytes[6],
            Some(6),
            "Line 1 newline -> byte 6 (\\r pos)"
        );

        // Line 2: "int y;\n" displayed, maps to bytes 8-14
        eprintln!(
            "Line 2 char_source_bytes: {:?}",
            view_lines[1].char_source_bytes
        );
        assert_eq!(
            view_lines[1].char_source_bytes.len(),
            7,
            "Line 2 should have 7 chars: 'i','n','t',' ','y',';','\\n'"
        );
        // Check specific mappings - THIS IS WHERE DRIFT WOULD SHOW
        assert_eq!(
            view_lines[1].char_source_bytes[0],
            Some(8),
            "Line 2 'i' -> byte 8"
        );
        assert_eq!(
            view_lines[1].char_source_bytes[4],
            Some(12),
            "Line 2 'y' -> byte 12"
        );
        assert_eq!(
            view_lines[1].char_source_bytes[5],
            Some(13),
            "Line 2 ';' -> byte 13"
        );
        assert_eq!(
            view_lines[1].char_source_bytes[6],
            Some(14),
            "Line 2 newline -> byte 14 (\\r pos)"
        );

        // Step 4: Simulate highlight span lookup
        // If TreeSitter highlights "int" as keyword (bytes 0-3 for line 1, bytes 8-11 for line 2),
        // the lookup should find these correctly.
        let simulated_highlight_spans = [
            // "int" on line 1: bytes 0-3
            (0usize..3usize, "keyword"),
            // "int" on line 2: bytes 8-11
            (8usize..11usize, "keyword"),
        ];

        // Verify that looking up byte positions from char_source_bytes finds the right spans
        for (line_idx, view_line) in view_lines.iter().enumerate() {
            for (char_idx, byte_pos) in view_line.char_source_bytes.iter().enumerate() {
                if let Some(bp) = byte_pos {
                    let in_span = simulated_highlight_spans
                        .iter()
                        .find(|(range, _)| range.contains(bp))
                        .map(|(_, name)| *name);

                    // First 3 chars of each line should be in keyword span
                    let expected_in_keyword = char_idx < 3;
                    let actually_in_keyword = in_span == Some("keyword");

                    if expected_in_keyword != actually_in_keyword {
                        panic!(
                            "CRLF offset drift detected! Line {} char {} (byte {}): expected keyword={}, got keyword={}",
                            line_idx + 1, char_idx, bp, expected_in_keyword, actually_in_keyword
                        );
                    }
                }
            }
        }
    }

    /// Test that apply_wrapping_transform correctly breaks long lines.
    /// This prevents memory exhaustion from extremely long single-line files (issue #481).
    #[test]
    fn test_apply_wrapping_transform_breaks_long_lines() {
        use fresh_core::api::{ViewTokenWire, ViewTokenWireKind};

        // Create a token with 25,000 characters (longer than MAX_SAFE_LINE_WIDTH of 10,000)
        let long_text = "x".repeat(25_000);
        let tokens = vec![
            ViewTokenWire {
                kind: ViewTokenWireKind::Text(long_text),
                source_offset: Some(0),
                style: None,
            },
            ViewTokenWire {
                kind: ViewTokenWireKind::Newline,
                source_offset: Some(25_000),
                style: None,
            },
        ];

        // Apply wrapping with MAX_SAFE_LINE_WIDTH (simulating line_wrap disabled)
        let wrapped = apply_wrapping_transform(tokens, MAX_SAFE_LINE_WIDTH, 0, false);

        // Count Break tokens - should have at least 2 breaks for 25K chars at 10K width
        let break_count = wrapped
            .iter()
            .filter(|t| matches!(t.kind, ViewTokenWireKind::Break))
            .count();

        assert!(
            break_count >= 2,
            "25K char line should have at least 2 breaks at 10K width, got {}",
            break_count
        );

        // Verify total content is preserved (excluding Break tokens)
        let total_chars: usize = wrapped
            .iter()
            .filter_map(|t| match &t.kind {
                ViewTokenWireKind::Text(s) => Some(s.len()),
                _ => None,
            })
            .sum();

        assert_eq!(
            total_chars, 25_000,
            "Total character count should be preserved after wrapping"
        );
    }

    /// Property test encoding the wrap-boundary invariant that the
    /// char-split path of [`apply_wrapping_transform`] must satisfy.
    ///
    /// The invariant is scoped to **char-split** row endings — rows
    /// whose last emitted grapheme falls strictly INSIDE a source Text
    /// token.  Word-wrap breaks (where the row ends at whitespace
    /// between tokens) are outside the scope of the char-split
    /// improvement and pass through unchecked; they land at a token
    /// boundary by construction.
    ///
    /// For every non-final visual row whose end is mid-Text-token:
    ///
    /// 1. **No overflow.** The row's visual width is at most
    ///    `content_width`.
    /// 2. **No loss.** Concatenating every emitted row in order yields
    ///    exactly the original input.
    /// 3. **Prefer UAX #29 word boundaries.** Let `hard_cap` be the
    ///    largest char position where the row could still fit, and
    ///    `floor = max(hard_cap - MAX_LOOKBACK, hard_cap / 2)`, both
    ///    measured in characters from the start of this row inside the
    ///    input.  If any `split_word_bound_indices()` boundary lies in
    ///    `[floor, hard_cap]`, the split must land at the LARGEST such
    ///    boundary.
    /// 4. **Fall back to hard cap.** If no word boundary lies in that
    ///    window, the split lands at `hard_cap` exactly (char split).
    #[cfg(test)]
    mod wrap_boundary_property {
        use super::apply_wrapping_transform;
        use fresh_core::api::{ViewTokenWire, ViewTokenWireKind};
        use proptest::prelude::*;
        use unicode_segmentation::UnicodeSegmentation;

        /// Matches the constant used by the implementation.  Defined
        /// here as well so the property test can compute the same
        /// window without reaching into the module internals.
        const MAX_LOOKBACK: usize = 16;

        fn tokens_from_input(input: &str) -> Vec<ViewTokenWire> {
            let mut tokens: Vec<ViewTokenWire> = Vec::new();
            let mut buf = String::new();
            let mut buf_start = 0usize;
            for (i, c) in input.char_indices() {
                if c == ' ' {
                    if !buf.is_empty() {
                        tokens.push(ViewTokenWire {
                            kind: ViewTokenWireKind::Text(std::mem::take(&mut buf)),
                            source_offset: Some(buf_start),
                            style: None,
                        });
                    }
                    tokens.push(ViewTokenWire {
                        kind: ViewTokenWireKind::Space,
                        source_offset: Some(i),
                        style: None,
                    });
                    buf_start = i + 1;
                } else {
                    if buf.is_empty() {
                        buf_start = i;
                    }
                    buf.push(c);
                }
            }
            if !buf.is_empty() {
                tokens.push(ViewTokenWire {
                    kind: ViewTokenWireKind::Text(buf.clone()),
                    source_offset: Some(buf_start),
                    style: None,
                });
            }
            tokens.push(ViewTokenWire {
                kind: ViewTokenWireKind::Newline,
                source_offset: Some(input.len()),
                style: None,
            });
            tokens
        }

        /// Reconstruct the sequence of visual rows from the wrapped
        /// token stream.  Each entry is the row's rendered content
        /// (Text + Space, with Break separating rows; Newline ends the
        /// last row).
        fn visual_rows(wrapped: &[ViewTokenWire]) -> Vec<String> {
            let mut rows: Vec<String> = vec![String::new()];
            for t in wrapped {
                match &t.kind {
                    ViewTokenWireKind::Text(s) => {
                        rows.last_mut().unwrap().push_str(s);
                    }
                    ViewTokenWireKind::Space => {
                        rows.last_mut().unwrap().push(' ');
                    }
                    ViewTokenWireKind::Break => {
                        rows.push(String::new());
                    }
                    ViewTokenWireKind::Newline => {
                        // End of logical line — ignore for wrap row
                        // purposes; we don't wrap across Newline here.
                    }
                    _ => {}
                }
            }
            rows
        }

        proptest! {
            // A handful of cases per run is plenty — wrapping is
            // deterministic, but the input space is large and we want
            // shrinking to work.
            #![proptest_config(ProptestConfig {
                cases: 256,
                .. ProptestConfig::default()
            })]

            /// Core property: the four invariants stated on the module
            /// docstring above.
            #[test]
            fn prop_wrap_respects_boundaries(
                input in "[a-zA-Z0-9().,:;/_=+ \\-]{1,120}",
                content_width in 5usize..40,
            ) {
                // Hanging indent off and gutter 0 — we want to isolate
                // the Text char-split logic from the indent path.
                let tokens = tokens_from_input(&input);
                let wrapped = apply_wrapping_transform(tokens, content_width, 0, false);
                let rows = visual_rows(&wrapped);

                // Invariant 1: no row exceeds content_width.
                for (i, row) in rows.iter().enumerate() {
                    prop_assert!(
                        row.chars().count() <= content_width,
                        "row {i} {:?} has width {} > content_width {content_width}",
                        row,
                        row.chars().count(),
                    );
                }

                // Invariant 2: lossless reconstruction.
                let reconstructed: String = rows.concat();
                prop_assert_eq!(
                    &reconstructed,
                    &input,
                    "reconstruction differs from input"
                );

                // Invariants 3 + 4: every non-final split lands at
                // either the largest word boundary in the lookback
                // window or the hard cap.
                let boundaries: std::collections::BTreeSet<usize> = input
                    .split_word_bound_indices()
                    .map(|(i, _)| i)
                    .chain(std::iter::once(input.len()))
                    .collect();

                let mut cursor_bytes = 0usize;
                let mut cursor_chars = 0usize;
                for (i, row) in rows.iter().enumerate() {
                    let row_bytes = row.len();
                    let row_chars = row.chars().count();
                    let row_end_bytes = cursor_bytes + row_bytes;
                    let row_end_chars = cursor_chars + row_chars;
                    let is_last = i + 1 == rows.len();

                    if !is_last {
                        // Only apply the boundary invariant to char-
                        // splits — row endings that fall strictly
                        // inside a Text token.  When the row ends at
                        // or adjacent to a space, it's a word-wrap
                        // break, which is outside this invariant.
                        let input_bytes = input.as_bytes();
                        let prev_is_space =
                            row_end_bytes > 0 && input_bytes[row_end_bytes - 1] == b' ';
                        let next_is_space = row_end_bytes < input_bytes.len()
                            && input_bytes[row_end_bytes] == b' ';
                        let is_mid_text = !prev_is_space && !next_is_space;
                        if !is_mid_text {
                            cursor_bytes = row_end_bytes;
                            cursor_chars = row_end_chars;
                            continue;
                        }

                        // The hard cap is the last char position this row
                        // could have reached: current cursor + content_width.
                        let hard_cap_chars = cursor_chars + content_width;
                        let hard_cap_bytes = char_index_to_byte(&input, hard_cap_chars);
                        let floor_chars = cursor_chars
                            + content_width.saturating_sub(MAX_LOOKBACK).max(content_width / 2);
                        let floor_bytes = char_index_to_byte(&input, floor_chars);

                        // Invariant 3 + 4: either the chosen split is
                        // the largest word boundary in [floor,
                        // hard_cap] (when any such boundary exists) or
                        // it's the hard cap itself (char-split
                        // fallback).  Do not exempt "row is exactly
                        // content_width" from the check — that's the
                        // case the improvement is supposed to change.
                        let max_in_window = boundaries
                            .range(floor_bytes..=hard_cap_bytes)
                            .next_back()
                            .copied();
                        match max_in_window {
                            Some(max_b) => {
                                prop_assert_eq!(
                                    row_end_bytes,
                                    max_b,
                                    "split at byte {} but largest word boundary in \
                                     [floor={}, hard_cap={}] is {}; row={:?}, input={:?}",
                                    row_end_bytes,
                                    floor_bytes,
                                    hard_cap_bytes,
                                    max_b,
                                    row,
                                    input,
                                );
                            }
                            None => {
                                prop_assert_eq!(
                                    row_end_bytes,
                                    hard_cap_bytes,
                                    "no word boundary in [floor={}, hard_cap={}], so \
                                     char-split must land at hard_cap, but split is at \
                                     byte {}; row={:?}, input={:?}",
                                    floor_bytes,
                                    hard_cap_bytes,
                                    row_end_bytes,
                                    row,
                                    input,
                                );
                            }
                        }
                    }

                    cursor_bytes = row_end_bytes;
                    cursor_chars = row_end_chars;
                }
            }
        }

        /// Translate a char index into a byte index for ASCII-ish
        /// inputs; clamps to input length.
        fn char_index_to_byte(s: &str, char_idx: usize) -> usize {
            s.char_indices()
                .nth(char_idx)
                .map(|(b, _)| b)
                .unwrap_or(s.len())
        }
    }

    /// Test that normal-length lines are not affected by safety wrapping.
    #[test]
    fn test_apply_wrapping_transform_preserves_short_lines() {
        use fresh_core::api::{ViewTokenWire, ViewTokenWireKind};

        // Create a token with 100 characters (much shorter than MAX_SAFE_LINE_WIDTH)
        let short_text = "x".repeat(100);
        let tokens = vec![
            ViewTokenWire {
                kind: ViewTokenWireKind::Text(short_text.clone()),
                source_offset: Some(0),
                style: None,
            },
            ViewTokenWire {
                kind: ViewTokenWireKind::Newline,
                source_offset: Some(100),
                style: None,
            },
        ];

        // Apply wrapping with MAX_SAFE_LINE_WIDTH (simulating line_wrap disabled)
        let wrapped = apply_wrapping_transform(tokens, MAX_SAFE_LINE_WIDTH, 0, false);

        // Should have no Break tokens for short lines
        let break_count = wrapped
            .iter()
            .filter(|t| matches!(t.kind, ViewTokenWireKind::Break))
            .count();

        assert_eq!(
            break_count, 0,
            "Short lines should not have any breaks, got {}",
            break_count
        );

        // Original text should be preserved exactly
        let text_tokens: Vec<_> = wrapped
            .iter()
            .filter_map(|t| match &t.kind {
                ViewTokenWireKind::Text(s) => Some(s.clone()),
                _ => None,
            })
            .collect();

        assert_eq!(text_tokens.len(), 1, "Should have exactly one Text token");
        assert_eq!(
            text_tokens[0], short_text,
            "Text content should be unchanged"
        );
    }

    /// End-to-end test: verify large single-line content with sequential markers
    /// is correctly chunked, wrapped, and all data is preserved through the pipeline.
    #[test]
    fn test_large_single_line_sequential_data_preserved() {
        use crate::view::ui::view_pipeline::ViewLineIterator;
        use fresh_core::api::{ViewTokenWire, ViewTokenWireKind};

        // Create content with sequential markers that span multiple chunks
        // Format: "[00001][00002]..." - each marker is 7 chars
        let num_markers = 5_000; // ~35KB, enough to test chunking at 10K char intervals
        let content: String = (1..=num_markers).map(|i| format!("[{:05}]", i)).collect();

        // Create tokens simulating what build_base_tokens would produce
        let tokens = vec![
            ViewTokenWire {
                kind: ViewTokenWireKind::Text(content.clone()),
                source_offset: Some(0),
                style: None,
            },
            ViewTokenWire {
                kind: ViewTokenWireKind::Newline,
                source_offset: Some(content.len()),
                style: None,
            },
        ];

        // Apply safety wrapping (simulating line_wrap=false with MAX_SAFE_LINE_WIDTH)
        let wrapped = apply_wrapping_transform(tokens, MAX_SAFE_LINE_WIDTH, 0, false);

        // Convert to ViewLines
        let view_lines: Vec<_> = ViewLineIterator::new(&wrapped, false, false, 4, false).collect();

        // Reconstruct content from ViewLines
        let mut reconstructed = String::new();
        for line in &view_lines {
            // Skip the trailing newline character in each line's text
            let text = line.text.trim_end_matches('\n');
            reconstructed.push_str(text);
        }

        // Verify all content is preserved
        assert_eq!(
            reconstructed.len(),
            content.len(),
            "Reconstructed content length should match original"
        );

        // Verify sequential markers are all present
        for i in 1..=num_markers {
            let marker = format!("[{:05}]", i);
            assert!(
                reconstructed.contains(&marker),
                "Missing marker {} after pipeline",
                marker
            );
        }

        // Verify order is preserved by checking sample positions
        let pos_100 = reconstructed.find("[00100]").expect("Should find [00100]");
        let pos_1000 = reconstructed.find("[01000]").expect("Should find [01000]");
        let pos_3000 = reconstructed.find("[03000]").expect("Should find [03000]");
        assert!(
            pos_100 < pos_1000 && pos_1000 < pos_3000,
            "Markers should be in sequential order: {} < {} < {}",
            pos_100,
            pos_1000,
            pos_3000
        );

        // Verify we got multiple visual lines (content was wrapped)
        assert!(
            view_lines.len() >= 3,
            "35KB content should produce multiple visual lines at 10K width, got {}",
            view_lines.len()
        );

        // Verify each ViewLine is bounded in size (memory safety check)
        for (i, line) in view_lines.iter().enumerate() {
            assert!(
                line.text.len() <= MAX_SAFE_LINE_WIDTH + 10, // +10 for newline and rounding
                "ViewLine {} exceeds safe width: {} chars",
                i,
                line.text.len()
            );
        }
    }

    /// Helper: strip OSC 8 escape sequences from a string, returning plain text.
    fn strip_osc8(s: &str) -> String {
        let mut result = String::with_capacity(s.len());
        let bytes = s.as_bytes();
        let mut i = 0;
        while i < bytes.len() {
            if i + 3 < bytes.len()
                && bytes[i] == 0x1b
                && bytes[i + 1] == b']'
                && bytes[i + 2] == b'8'
                && bytes[i + 3] == b';'
            {
                i += 4;
                while i < bytes.len() && bytes[i] != 0x07 {
                    i += 1;
                }
                if i < bytes.len() {
                    i += 1;
                }
            } else {
                result.push(bytes[i] as char);
                i += 1;
            }
        }
        result
    }

    /// Read a row from a ratatui buffer, skipping the second cell of 2-char
    /// OSC 8 chunks so we get clean text.
    fn read_row(buf: &ratatui::buffer::Buffer, y: u16) -> String {
        let width = buf.area().width;
        let mut s = String::new();
        let mut col = 0u16;
        while col < width {
            let cell = &buf[(col, y)];
            let stripped = strip_osc8(cell.symbol());
            let chars = stripped.chars().count();
            if chars > 1 {
                s.push_str(&stripped);
                col += chars as u16;
            } else {
                s.push_str(&stripped);
                col += 1;
            }
        }
        s.trim_end().to_string()
    }

    #[test]
    fn test_apply_osc8_to_cells_preserves_adjacent_cells() {
        use ratatui::buffer::Buffer;
        use ratatui::layout::Rect;

        // Simulate: "[Quick Install](#installation)" in a 40-wide buffer row 0
        let text = "[Quick Install](#installation)";
        let area = Rect::new(0, 0, 40, 1);
        let mut buf = Buffer::empty(area);
        for (i, ch) in text.chars().enumerate() {
            if (i as u16) < 40 {
                buf[(i as u16, 0)].set_symbol(&ch.to_string());
            }
        }

        // Overlay covers "Quick Install" = cols 1..14 (bytes 9..22 mapped to screen)
        let url = "https://example.com";

        // Apply with cursor at col 0 (not inside the overlay range)
        apply_osc8_to_cells(&mut buf, 1, 14, 0, url, Some((0, 0)));

        let row = read_row(&buf, 0);
        assert_eq!(
            row, text,
            "After OSC 8 application, reading the row should reproduce the original text"
        );

        // Cell 14 = ']' must not be touched
        let cell14 = strip_osc8(buf[(14, 0)].symbol());
        assert_eq!(cell14, "]", "Cell 14 (']') must not be modified by OSC 8");

        // Cell 0 = '[' must not be touched
        let cell0 = strip_osc8(buf[(0, 0)].symbol());
        assert_eq!(cell0, "[", "Cell 0 ('[') must not be modified by OSC 8");
    }

    #[test]
    fn test_apply_osc8_stable_across_reapply() {
        use ratatui::buffer::Buffer;
        use ratatui::layout::Rect;

        let text = "[Quick Install](#installation)";
        let area = Rect::new(0, 0, 40, 1);

        // First render: apply OSC 8 with cursor at col 0
        let mut buf1 = Buffer::empty(area);
        for (i, ch) in text.chars().enumerate() {
            if (i as u16) < 40 {
                buf1[(i as u16, 0)].set_symbol(&ch.to_string());
            }
        }
        apply_osc8_to_cells(&mut buf1, 1, 14, 0, "https://example.com", Some((0, 0)));
        let row1 = read_row(&buf1, 0);

        // Second render: fresh buffer, same text, apply OSC 8 with cursor at col 5
        let mut buf2 = Buffer::empty(area);
        for (i, ch) in text.chars().enumerate() {
            if (i as u16) < 40 {
                buf2[(i as u16, 0)].set_symbol(&ch.to_string());
            }
        }
        apply_osc8_to_cells(&mut buf2, 1, 14, 0, "https://example.com", Some((5, 0)));
        let row2 = read_row(&buf2, 0);

        assert_eq!(row1, text);
        assert_eq!(row2, text);
    }

    #[test]
    #[ignore = "OSC 8 hyperlinks disabled pending ratatui diff fix"]
    fn test_apply_osc8_diff_between_renders() {
        use ratatui::buffer::Buffer;
        use ratatui::layout::Rect;

        // Simulate ratatui's diff-based update: a "concealed" render followed
        // by an "unconcealed" render. The backend buffer accumulates diffs.
        let area = Rect::new(0, 0, 40, 1);

        // --- Render 1: concealed text "Quick Install" at cols 0..12, rest is space ---
        let concealed = "Quick Install";
        let mut frame1 = Buffer::empty(area);
        for (i, ch) in concealed.chars().enumerate() {
            frame1[(i as u16, 0)].set_symbol(&ch.to_string());
        }
        // OSC 8 covers cols 0..13 (concealed mapping)
        apply_osc8_to_cells(&mut frame1, 0, 13, 0, "https://example.com", Some((0, 5)));

        // Simulate backend: starts empty, apply diff from frame1
        let prev = Buffer::empty(area);
        let mut backend = Buffer::empty(area);
        let diff1 = prev.diff(&frame1);
        for (x, y, cell) in &diff1 {
            backend[(*x, *y)] = (*cell).clone();
        }

        // --- Render 2: unconcealed "[Quick Install](#installation)" ---
        let full = "[Quick Install](#installation)";
        let mut frame2 = Buffer::empty(area);
        for (i, ch) in full.chars().enumerate() {
            if (i as u16) < 40 {
                frame2[(i as u16, 0)].set_symbol(&ch.to_string());
            }
        }
        // OSC 8 covers cols 1..14 (unconcealed mapping)
        apply_osc8_to_cells(&mut frame2, 1, 14, 0, "https://example.com", Some((0, 0)));

        // Apply diff from frame1→frame2 to backend
        let diff2 = frame1.diff(&frame2);
        for (x, y, cell) in &diff2 {
            backend[(*x, *y)] = (*cell).clone();
        }

        // Backend should now show the full text when read
        let row = read_row(&backend, 0);
        assert_eq!(
            row, full,
            "After diff-based update from concealed to unconcealed, \
             backend should show full text"
        );

        // Specifically, cell 14 must be ']'
        let cell14 = strip_osc8(backend[(14, 0)].symbol());
        assert_eq!(cell14, "]", "Cell 14 must be ']' after unconcealed render");
    }

    // --- Current line highlight tests ---

    fn render_with_highlight_option(
        content: &str,
        cursor_pos: usize,
        highlight_current_line: bool,
    ) -> LineRenderOutput {
        let mut state = EditorState::new(20, 6, 1024, test_fs());
        state.buffer = Buffer::from_str(content, 1024, test_fs());
        let mut cursors = crate::model::cursor::Cursors::new();
        cursors.primary_mut().position = cursor_pos.min(state.buffer.len());
        let viewport = Viewport::new(20, 4);
        state.margins.left_config.enabled = false;

        let render_area = Rect::new(0, 0, 20, 4);
        let visible_count = viewport.visible_line_count();
        let gutter_width = state.margins.left_total_width();
        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();
        let empty_folds = FoldManager::new();

        let view_data = build_view_data(
            &mut state,
            &viewport,
            None,
            content.len().max(1),
            visible_count,
            false,
            render_area.width as usize,
            gutter_width,
            &ViewMode::Source,
            &empty_folds,
            &theme,
        );
        let view_anchor = calculate_view_anchor(&view_data.lines, 0);

        let estimated_lines = (state.buffer.len() / state.buffer.estimated_line_length()).max(1);
        state.margins.update_width_for_buffer(estimated_lines, true);
        let gutter_width = state.margins.left_total_width();

        let selection = selection_context(&state, &cursors);
        let _ = state
            .buffer
            .populate_line_cache(viewport.top_byte, visible_count);
        let viewport_start = viewport.top_byte;
        let viewport_end = calculate_viewport_end(
            &mut state,
            viewport_start,
            content.len().max(1),
            visible_count,
        );
        let decorations = decoration_context(
            &mut state,
            viewport_start,
            viewport_end,
            selection.primary_cursor_position,
            &empty_folds,
            &theme,
            100_000,
            &ViewMode::Source,
            false,
            &[],
        );

        render_view_lines(LineRenderInput {
            state: &state,
            theme: &theme,
            view_lines: &view_data.lines,
            view_anchor,
            render_area,
            gutter_width,
            selection: &selection,
            decorations: &decorations,
            visible_line_count: visible_count,
            lsp_waiting: false,
            is_active: true,
            line_wrap: viewport.line_wrap_enabled,
            estimated_lines,
            left_column: viewport.left_column,
            relative_line_numbers: false,
            session_mode: false,
            software_cursor_only: false,
            show_line_numbers: false,
            byte_offset_mode: false,
            show_tilde: true,
            highlight_current_line,
            cell_theme_map: &mut Vec::new(),
            screen_width: 0,
        })
    }

    /// Check whether any span on a given line has `current_line_bg` as its background.
    fn line_has_current_line_bg(output: &LineRenderOutput, line_idx: usize) -> bool {
        let current_line_bg = ratatui::style::Color::Rgb(40, 40, 40);
        if let Some(line) = output.lines.get(line_idx) {
            line.spans
                .iter()
                .any(|span| span.style.bg == Some(current_line_bg))
        } else {
            false
        }
    }

    #[test]
    fn current_line_highlight_enabled_highlights_cursor_line() {
        let output = render_with_highlight_option("abc\ndef\nghi\n", 0, true);
        // Cursor is on line 0 — it should have current_line_bg
        assert!(
            line_has_current_line_bg(&output, 0),
            "Cursor line (line 0) should have current_line_bg when highlighting is enabled"
        );
        // Line 1 should NOT have current_line_bg
        assert!(
            !line_has_current_line_bg(&output, 1),
            "Non-cursor line (line 1) should NOT have current_line_bg"
        );
    }

    #[test]
    fn current_line_highlight_disabled_no_highlight() {
        let output = render_with_highlight_option("abc\ndef\nghi\n", 0, false);
        // No line should have current_line_bg when disabled
        assert!(
            !line_has_current_line_bg(&output, 0),
            "Cursor line should NOT have current_line_bg when highlighting is disabled"
        );
        assert!(
            !line_has_current_line_bg(&output, 1),
            "Non-cursor line should NOT have current_line_bg when highlighting is disabled"
        );
    }

    #[test]
    fn current_line_highlight_follows_cursor_position() {
        // Cursor on line 1 (byte 4 = start of "def")
        let output = render_with_highlight_option("abc\ndef\nghi\n", 4, true);
        assert!(
            !line_has_current_line_bg(&output, 0),
            "Line 0 should NOT have current_line_bg when cursor is on line 1"
        );
        assert!(
            line_has_current_line_bg(&output, 1),
            "Line 1 should have current_line_bg when cursor is there"
        );
        assert!(
            !line_has_current_line_bg(&output, 2),
            "Line 2 should NOT have current_line_bg when cursor is on line 1"
        );
    }
}
