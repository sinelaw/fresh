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
use crate::config::IndentationGuideMode;
use crate::model::buffer::Buffer;
use crate::model::event::{BufferId, EventLog, LeafId, SplitDirection};
use crate::primitives::ansi_background::AnsiBackground;
use crate::state::EditorState;
use crate::view::split::SplitManager;
use ratatui::layout::Rect;
use std::collections::HashMap;

/// Maximum line width before forced wrapping is applied, even when line wrapping is disabled.
/// This prevents memory exhaustion when opening files with extremely long lines (e.g., 10MB
/// single-line JSON files). Lines exceeding this width are wrapped into multiple visual lines,
/// each bounded to this width. 10,000 columns is far wider than any monitor while keeping
/// memory usage reasonable (~80KB per ViewLine instead of hundreds of MB).
const MAX_SAFE_LINE_WIDTH: usize = 10_000;

/// Immutable editor render settings for one frame.
///
/// Bundles the static `config.editor.*` flags (plus a couple of stable
/// `Editor` fields) that `render_content` and its callees only ever read.
/// Deliberately holds *only* settings — no buffers, view-states, theme,
/// geometry, output sinks, or per-frame computed flags (hover, cursor,
/// mode) — so it can be built once and shared without entangling borrows.
#[derive(Debug, Clone, Copy)]
pub struct EditorRenderConfig<'a> {
    pub large_file_threshold_bytes: u64,
    pub line_wrap: bool,
    pub estimated_line_length: usize,
    pub highlight_context_bytes: usize,
    pub relative_line_numbers: bool,
    pub use_terminal_bg: bool,
    pub show_vertical_scrollbar: bool,
    pub show_horizontal_scrollbar: bool,
    pub diagnostics_inline_text: bool,
    pub show_tilde: bool,
    pub highlight_current_column: bool,
    pub indentation_guide: IndentationGuideMode,
    pub indentation_guide_glyph: &'a str,
    pub rainbow_indentation: bool,
    pub hide_current_line_on_selection: bool,
    pub background_fade: f32,
    pub software_cursor_only: bool,
}

impl<'a> EditorRenderConfig<'a> {
    /// Build from the static editor config plus the two stable `Editor`
    /// flags that don't live under `config.editor`. Borrows only
    /// `config.editor`, so it composes with a disjoint `&mut windows`
    /// borrow at the call site.
    pub fn new(
        editor: &'a crate::config::EditorConfig,
        background_fade: f32,
        software_cursor_only: bool,
    ) -> Self {
        Self {
            large_file_threshold_bytes: editor.large_file_threshold_bytes,
            line_wrap: editor.line_wrap,
            estimated_line_length: editor.estimated_line_length,
            highlight_context_bytes: editor.highlight_context_bytes,
            relative_line_numbers: editor.relative_line_numbers,
            use_terminal_bg: editor.use_terminal_bg,
            show_vertical_scrollbar: editor.show_vertical_scrollbar,
            show_horizontal_scrollbar: editor.show_horizontal_scrollbar,
            diagnostics_inline_text: editor.diagnostics_inline_text,
            show_tilde: editor.show_tilde,
            highlight_current_column: editor.highlight_current_column,
            indentation_guide: editor.indentation_guide,
            indentation_guide_glyph: &editor.indentation_guide_glyph,
            rainbow_indentation: editor.rainbow_indentation,
            hide_current_line_on_selection: editor.hide_current_line_on_selection,
            background_fade,
            software_cursor_only,
        }
    }
}

/// "How to render" — the appearance/policy inputs that are identical for
/// every split in a frame: the theme, the ANSI backdrop, and the editor
/// render config. Built once at the top of the render pass and threaded by
/// reference through the whole painter chain (`render_content` →
/// `render_buffer_in_split` → …), so each layer forwards one `RenderStyle`
/// instead of re-listing ~16 style parameters. Distinct from per-split state
/// and the draw target, which vary or are mutated.
//
// No `Debug` derive — `AnsiBackground` isn't `Debug`.
#[derive(Clone, Copy)]
pub struct RenderStyle<'a> {
    pub theme: &'a crate::view::theme::Theme,
    pub ansi_background: Option<&'a AnsiBackground>,
    pub cfg: EditorRenderConfig<'a>,
}

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
        buf: &mut ratatui::buffer::Buffer,
        area: Rect,
        split_manager: &SplitManager,
        buffers: &mut HashMap<BufferId, EditorState>,
        buffer_metadata: &HashMap<BufferId, BufferMetadata>,
        preview_buffer: Option<BufferId>,
        event_logs: &mut HashMap<BufferId, EventLog>,
        composite_buffers: &mut HashMap<BufferId, crate::model::composite_buffer::CompositeBuffer>,
        composite_view_states: &mut HashMap<
            (LeafId, BufferId),
            crate::view::composite_view::CompositeViewState,
        >,
        style: RenderStyle<'_>,
        lsp_waiting: bool,
        split_view_states: Option<&mut HashMap<LeafId, crate::view::split::SplitViewState>>,
        grouped_subtrees: &HashMap<LeafId, crate::view::split::SplitNode>,
        hide_cursor: bool,
        hovered_tab: Option<(crate::view::split::TabTarget, LeafId, bool)>,
        hovered_close_split: Option<LeafId>,
        hovered_maximize_split: Option<LeafId>,
        is_maximized: bool,
        tab_bar_visible: bool,
        session_mode: bool,
        scrollback_view_splits: &std::collections::HashSet<LeafId>,
        cell_theme_map: &mut Vec<crate::app::types::CellThemeInfo>,
        screen_width: u16,
        pending_hardware_cursor: &mut Option<(u16, u16)>,
        // Forwarded to the tab-bar renderer: when false the tab bar lays out but
        // paints no cells (web renders tabs natively); panes always draw.
        draw_tab_bar: bool,
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
            buf,
            area,
            split_manager,
            buffers,
            buffer_metadata,
            preview_buffer,
            event_logs,
            composite_buffers,
            composite_view_states,
            style,
            lsp_waiting,
            split_view_states,
            grouped_subtrees,
            hide_cursor,
            hovered_tab,
            hovered_close_split,
            hovered_maximize_split,
            is_maximized,
            tab_bar_visible,
            session_mode,
            scrollback_view_splits,
            cell_theme_map,
            screen_width,
            pending_hardware_cursor,
            draw_tab_bar,
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
        buf: &mut ratatui::buffer::Buffer,
        state: &mut EditorState,
        cursors: &crate::model::cursor::Cursors,
        viewport: &mut crate::view::viewport::Viewport,
        folds: &mut crate::view::folding::FoldManager,
        event_log: Option<&mut EventLog>,
        area: Rect,
        style: RenderStyle<'_>,
        view_mode: crate::state::ViewMode,
        compose_width: Option<u16>,
        compose_column_guides: Option<Vec<u16>>,
        view_transform: Option<crate::services::plugins::api::ViewTransformPayload>,
        buffer_id: BufferId,
        session_mode: bool,
        rulers: &[usize],
        show_line_numbers: bool,
        highlight_current_line: bool,
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
            buf,
            state,
            cursors,
            viewport,
            folds,
            event_log,
            area,
            /* is_active */ false,
            style,
            /* lsp_waiting */ false,
            view_mode,
            compose_width,
            compose_column_guides,
            view_transform,
            buffer_id,
            /* hide_cursor */ true,
            session_mode,
            rulers,
            show_line_numbers,
            highlight_current_line,
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
        render_output_for_with_options(
            content,
            cursor_pos,
            gutters_enabled,
            IndentationGuideMode::None,
            crate::config::default_indentation_guide_glyph(),
            0,
            None,
        )
    }

    fn render_output_for_with_indentation_guide(
        content: &str,
        cursor_pos: usize,
        left_column: usize,
    ) -> (LineRenderOutput, usize, bool, usize) {
        render_output_for_with_indentation_guide_mode(
            content,
            cursor_pos,
            left_column,
            IndentationGuideMode::All,
        )
    }

    fn render_output_for_with_indentation_guide_mode(
        content: &str,
        cursor_pos: usize,
        left_column: usize,
        indentation_guide: IndentationGuideMode,
    ) -> (LineRenderOutput, usize, bool, usize) {
        render_output_for_with_options(
            content,
            cursor_pos,
            false,
            indentation_guide,
            crate::config::default_indentation_guide_glyph(),
            left_column,
            None,
        )
    }

    fn render_output_for_with_indentation_guide_mode_and_tab_size(
        content: &str,
        cursor_pos: usize,
        left_column: usize,
        indentation_guide: IndentationGuideMode,
        tab_size: usize,
    ) -> (LineRenderOutput, usize, bool, usize) {
        render_output_for_with_options(
            content,
            cursor_pos,
            false,
            indentation_guide,
            crate::config::default_indentation_guide_glyph(),
            left_column,
            Some(tab_size),
        )
    }

    fn render_output_for_with_options(
        content: &str,
        cursor_pos: usize,
        gutters_enabled: bool,
        indentation_guide: IndentationGuideMode,
        indentation_guide_glyph: String,
        left_column: usize,
        tab_size: Option<usize>,
    ) -> (LineRenderOutput, usize, bool, usize) {
        let mut state = EditorState::new(20, 6, 1024, test_fs());
        state.buffer = Buffer::from_str(content, 1024, test_fs());
        if let Some(tab_size) = tab_size {
            state.buffer_settings.tab_size = tab_size;
        }
        let mut cursors = crate::model::cursor::Cursors::new();
        cursors.primary_mut().position = cursor_pos.min(state.buffer.len());
        // Create a standalone viewport (no longer part of EditorState)
        let mut viewport = Viewport::new(20, 4);
        viewport.left_column = left_column;
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
            &[],
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
            viewport.left_column,
            render_area.width as usize,
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
            indentation_guide,
            indentation_guide_glyph: &indentation_guide_glyph,
            rainbow_indentation: false,
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

    /// Render `content` with the viewport scrolled so the buffer byte
    /// `top_byte` is the first visible row. Used to exercise indentation-guide
    /// rendering when a block-opening line has scrolled above the viewport.
    fn render_output_scrolled_with_indentation_guide(
        content: &str,
        top_byte: usize,
    ) -> LineRenderOutput {
        let mut state = EditorState::new(20, 6, 1024, test_fs());
        state.buffer = Buffer::from_str(content, 1024, test_fs());
        let mut cursors = crate::model::cursor::Cursors::new();
        cursors.primary_mut().position = 0;
        let mut viewport = Viewport::new(20, 10);
        viewport.top_byte = top_byte;
        state.margins.left_config.enabled = false;

        let render_area = Rect::new(0, 0, 20, 10);
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
            &[],
        );
        let view_anchor = calculate_view_anchor(&view_data.lines, viewport.top_byte);

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
            viewport.left_column,
            render_area.width as usize,
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

        let glyph = crate::config::default_indentation_guide_glyph();
        let mut dummy_theme_map = Vec::new();
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
            show_line_numbers: true,
            byte_offset_mode: false,
            show_tilde: true,
            highlight_current_line: true,
            indentation_guide: IndentationGuideMode::All,
            indentation_guide_glyph: &glyph,
            rainbow_indentation: false,
            cell_theme_map: &mut dummy_theme_map,
            screen_width: 0,
        })
    }

    fn rendered_line_text(output: &LineRenderOutput, line_idx: usize) -> String {
        output.lines[line_idx]
            .spans
            .iter()
            .map(|span| span.content.as_ref())
            .collect::<String>()
            .trim_end()
            .to_string()
    }

    #[test]
    fn viewport_end_is_bounded_for_overlong_single_line() {
        // The decoration pass (syntax highlight, reference highlight, bracket /
        // rainbow overlay) scans `viewport_start..viewport_end` on every frame.
        // A minified asset (SVG/JSON/etc.) can be a single line of hundreds of
        // KB while only ~`width` columns are on screen, so the scanned span
        // must stay proportional to the screen, not the line length — otherwise
        // every cursor move re-scans the whole line (issue #2529, per-keystroke
        // lag).
        let huge = "a".repeat(200_000);
        let mut state = EditorState::new(80, 40, 1024, test_fs());
        state.buffer = Buffer::from_str(&huge, 1024, test_fs());

        let viewport_start = 0;
        let visible_count = 40;
        let left_column = 0;
        let viewport_width = 80;
        let _ = state
            .buffer
            .populate_line_cache(viewport_start, visible_count);
        let est_line_len = state.buffer.estimated_line_length().max(1);

        let viewport_end = calculate_viewport_end(
            &mut state,
            viewport_start,
            est_line_len,
            visible_count,
            left_column,
            viewport_width,
        );

        let span = viewport_end - viewport_start;
        assert!(
            span < 10_000,
            "decoration viewport should be bounded to the visible window, \
             got {span} bytes for an {viewport_width}-column screen"
        );
    }

    #[test]
    fn viewport_end_covers_horizontal_scroll_window() {
        // When a long line is scrolled right, the clamp must still reach far
        // enough to cover the visible window (`left_column + width` columns) so
        // on-screen content keeps its highlighting; it just must not overshoot
        // to the line's true end.
        let huge = "a".repeat(200_000);
        let mut state = EditorState::new(80, 40, 1024, test_fs());
        state.buffer = Buffer::from_str(&huge, 1024, test_fs());

        let viewport_start = 0;
        let visible_count = 40;
        let left_column = 5_000;
        let viewport_width = 80;
        let _ = state
            .buffer
            .populate_line_cache(viewport_start, visible_count);
        let est_line_len = state.buffer.estimated_line_length().max(1);

        let viewport_end = calculate_viewport_end(
            &mut state,
            viewport_start,
            est_line_len,
            visible_count,
            left_column,
            viewport_width,
        );

        // Must cover the visible window (these are 1-byte ASCII columns)...
        assert!(
            viewport_end >= left_column + viewport_width,
            "viewport_end {viewport_end} must cover the scrolled-in window \
             ending at column {}",
            left_column + viewport_width
        );
        // ...but must not span the whole 200_000-byte line.
        assert!(
            viewport_end < 100_000,
            "viewport_end {viewport_end} should stay bounded, not reach line end"
        );
    }

    #[test]
    fn indentation_guide_disabled_preserves_leading_spaces() {
        let (output, _, _, _) = render_output_for("    let x = 1;\n", 0);

        assert_eq!(rendered_line_text(&output, 0), "    let x = 1;");
        assert!(!rendered_line_text(&output, 0).contains('▏'));
    }

    #[test]
    fn indentation_guide_render_for_space_indents_but_not_root_lines() {
        let (output, _, _, _) = render_output_for_with_indentation_guide(
            "fn main()\n    let child = 1;\n        let grandchild = 2;\nroot\n",
            0,
            0,
        );

        assert_eq!(rendered_line_text(&output, 0), "fn main()");
        assert_eq!(rendered_line_text(&output, 1), "▏   let child = 1;");
        assert_eq!(
            rendered_line_text(&output, 2),
            "▏   ▏   let grandchild = 2;"
        );
        assert_eq!(rendered_line_text(&output, 3), "root");
    }

    #[test]
    fn indentation_guide_follow_four_space_file_indents_when_tab_size_is_two() {
        let (output, _, _, _) = render_output_for_with_indentation_guide_mode_and_tab_size(
            "fn main() {\n    child();\n        grandchild();\n}\n",
            0,
            0,
            IndentationGuideMode::All,
            2,
        );

        assert_eq!(rendered_line_text(&output, 0), "fn main() {");
        assert_eq!(rendered_line_text(&output, 1), "▏   child();");
        assert_eq!(rendered_line_text(&output, 2), "▏   ▏   grandchild();");
        assert_eq!(rendered_line_text(&output, 3), "}");
    }

    #[test]
    fn indentation_guide_active_mode_uses_file_indent_width_when_tab_size_is_two() {
        let content = "function test() {\n    if (1) {\n        // test\n    }\n}\n";
        let cursor_pos = content.find("// test").unwrap();
        let (output, _, _, _) = render_output_for_with_indentation_guide_mode_and_tab_size(
            content,
            cursor_pos,
            0,
            IndentationGuideMode::Active,
            2,
        );

        assert_eq!(rendered_line_text(&output, 0), "function test() {");
        assert_eq!(rendered_line_text(&output, 1), "    if (1) {");
        assert_eq!(rendered_line_text(&output, 2), "    ▏   // test");
        assert_eq!(rendered_line_text(&output, 3), "    }");
    }

    #[test]
    fn indentation_guide_follow_nearest_earlier_parent_indent_when_widths_vary() {
        let (output, _, _, _) = render_output_for_with_indentation_guide_mode_and_tab_size(
            "root\n  child\n      grand\n  sibling\n",
            0,
            0,
            IndentationGuideMode::All,
            4,
        );

        assert_eq!(rendered_line_text(&output, 0), "root");
        assert_eq!(rendered_line_text(&output, 1), "▏ child");
        assert_eq!(rendered_line_text(&output, 2), "▏ ▏   grand");
        assert_eq!(rendered_line_text(&output, 3), "▏ sibling");
    }

    #[test]
    fn indentation_guide_active_mode_uses_nearest_earlier_parent_indent() {
        let content = "root\n  child\n      grand\n  sibling\n";
        let cursor_pos = content.find("grand").unwrap();
        let (output, _, _, _) = render_output_for_with_indentation_guide_mode_and_tab_size(
            content,
            cursor_pos,
            0,
            IndentationGuideMode::Active,
            4,
        );

        assert_eq!(rendered_line_text(&output, 0), "root");
        assert_eq!(rendered_line_text(&output, 1), "  child");
        assert_eq!(rendered_line_text(&output, 2), "  ▏   grand");
        assert_eq!(rendered_line_text(&output, 3), "  sibling");
    }

    #[test]
    fn indentation_guide_resume_after_lower_indent_line() {
        let (output, _, _, _) = render_output_for_with_indentation_guide(
            "        before\n    lower\n        after\n",
            0,
            0,
        );

        assert_eq!(rendered_line_text(&output, 0), "▏   ▏   before");
        assert_eq!(rendered_line_text(&output, 1), "▏   lower");
        assert_eq!(rendered_line_text(&output, 2), "▏   ▏   after");
    }

    #[test]
    fn indentation_guide_all_mode_draws_through_blank_lines() {
        // A whitespace-only line inside a nested block continues *every*
        // enclosing block's guides straight through it, rather than leaving a
        // one-row gap. With the block openers (`fn f() {` at col 0, `if x {` at
        // col 4) on screen, the blank row must still draw both the col-0 and
        // col-4 guides (it has eight trailing spaces, so those guide cells
        // exist), and the staircase resumes unchanged on the row below.
        let blank = " ".repeat(8);
        let content =
            format!("fn f() {{\n    if x {{\n        a;\n{blank}\n        b;\n    }}\n}}\n");
        let output = render_output_scrolled_with_indentation_guide(&content, 0);

        assert_eq!(rendered_line_text(&output, 0), "fn f() {");
        assert_eq!(rendered_line_text(&output, 1), "▏   if x {");
        assert_eq!(rendered_line_text(&output, 2), "▏   ▏   a;");
        assert_eq!(rendered_line_text(&output, 3), "▏   ▏");
        assert_eq!(rendered_line_text(&output, 4), "▏   ▏   b;");
        assert_eq!(rendered_line_text(&output, 5), "▏   }");
        assert_eq!(rendered_line_text(&output, 6), "}");
    }

    #[test]
    fn indentation_guide_empty_line_does_not_collapse_staircase() {
        // A *completely empty* line (a bare `\n`) inside a nested block must not
        // reset the indent staircase: the code row below it keeps its full set of
        // guides. (Before `slice_indent` treated `\n` as a terminator, a bare
        // "\n" read as indent-0 content, popping the whole stack — so `b;` lost
        // its col-4 guide and rendered "▏   b;".) The empty row's own rendering is
        // covered by the draw-through test below.
        let content = "fn f() {\n    if x {\n        a;\n\n        b;\n    }\n}\n";
        let output = render_output_scrolled_with_indentation_guide(content, 0);

        assert_eq!(rendered_line_text(&output, 0), "fn f() {");
        assert_eq!(rendered_line_text(&output, 1), "▏   if x {");
        assert_eq!(rendered_line_text(&output, 2), "▏   ▏   a;");
        // row 3 is the empty line — owned by the draw-through test.
        assert_eq!(rendered_line_text(&output, 4), "▏   ▏   b;");
        assert_eq!(rendered_line_text(&output, 5), "▏   }");
    }

    #[test]
    fn indentation_guide_survives_scroll_past_block_opener() {
        // A block whose opening lines (`mod m {` at col 0, `fn f() {` at col 4)
        // have scrolled above the viewport must still draw their guides on the
        // interior rows below. Previously the all-mode scanner derived its
        // staircase only from the visible rows, so the off-screen openers'
        // levels were missing — the col-4 guide vanished on the deeper interior
        // rows and reappeared only when scrolling the opener back into view.
        let content = concat!(
            "mod m {\n",             // col 0  (scrolled off-screen)
            "    fn f() {\n",        // col 4  (scrolled off-screen)
            "        let arr = [\n", // col 8  <- first visible row
            "            a,\n",      // col 12
            "            b,\n",      // col 12
            "        ];\n",          // col 8
            "        let c = 1;\n",  // col 8
            "    }\n",
            "}\n",
        );
        let top_byte = content.find("        let arr = [").unwrap();
        let output = render_output_scrolled_with_indentation_guide(content, top_byte);

        // First visible row keeps both ancestor guides (col 0 and col 4) even
        // though both owning lines are off-screen.
        assert_eq!(rendered_line_text(&output, 0), "▏   ▏   let arr = [");
        // Interior rows show the full staircase, including the col-4 guide that
        // the scroll regression used to drop.
        assert_eq!(rendered_line_text(&output, 1), "▏   ▏   ▏   a,");
        assert_eq!(rendered_line_text(&output, 2), "▏   ▏   ▏   b,");
        assert_eq!(rendered_line_text(&output, 3), "▏   ▏   ];");
    }

    #[test]
    fn indentation_guide_draws_through_blank_line_when_opener_scrolled_off() {
        // Combines the two cases that interact here: a whitespace-only line in
        // the middle of a block (guides must be drawn straight through it) while
        // the block's openers (`mod m {` at col 0, `fn f() {` at col 4) have
        // scrolled above the viewport. The primer must skip the blank line as it
        // walks up to reconstruct the staircase, and the drawn-through guides
        // must use that primed staircase — otherwise the col-4 guide drops on the
        // blank row exactly as it did on the textual interior rows.
        // 12 spaces: a whitespace-only line wide enough to carry guide cells at
        // columns 0/4/8 (the renderer only replaces existing leading-space cells).
        let blank = " ".repeat(12);
        let content = format!(
            "mod m {{\n    fn f() {{\n        let arr = [\n            alpha_value,\n{blank}\n            beta_value,\n        ];\n        let after = compute();\n    }}\n}}\n"
        );
        let top_byte = content.find("        let arr = [").unwrap();
        let output = render_output_scrolled_with_indentation_guide(&content, top_byte);

        assert_eq!(rendered_line_text(&output, 0), "▏   ▏   let arr = [");
        assert_eq!(rendered_line_text(&output, 1), "▏   ▏   ▏   alpha_value,");
        // The whitespace-only row continues the enclosing block's guides — the
        // col-4 guide (owned by the off-screen `fn f() {`) must be drawn through.
        assert_eq!(rendered_line_text(&output, 2), "▏   ▏   ▏");
        assert_eq!(rendered_line_text(&output, 3), "▏   ▏   ▏   beta_value,");
    }

    #[test]
    fn indentation_guide_draws_through_completely_empty_lines() {
        // A bare `\n` line has no cells for the per-cell pass to restyle, so its
        // guides are synthesised. At root depth the empty line carries the single
        // col-0 guide; nested, it carries every ancestor guide of the surrounding
        // body — keeping the vertical guides continuous through the gap.
        let root = "int main() {\n\n    greet();\n    \n    return 0;\n}\n";
        let out = render_output_scrolled_with_indentation_guide(root, 0);
        assert_eq!(rendered_line_text(&out, 0), "int main() {");
        assert_eq!(rendered_line_text(&out, 1), "▏"); // empty line, drawn through
        assert_eq!(rendered_line_text(&out, 2), "▏   greet();");
        assert_eq!(rendered_line_text(&out, 3), "▏"); // whitespace-only line
        assert_eq!(rendered_line_text(&out, 4), "▏   return 0;");
        assert_eq!(rendered_line_text(&out, 5), "}");

        let nested = "fn f() {\n    if x {\n        a;\n\n        b;\n    }\n}\n";
        let out = render_output_scrolled_with_indentation_guide(nested, 0);
        assert_eq!(rendered_line_text(&out, 2), "▏   ▏   a;");
        // The empty interior line carries both the col-0 and col-4 guides.
        assert_eq!(rendered_line_text(&out, 3), "▏   ▏");
        assert_eq!(rendered_line_text(&out, 4), "▏   ▏   b;");
    }

    #[test]
    fn indentation_guide_empty_line_after_opener_flows_into_body() {
        // The empty line sits directly under the opener, before any body row —
        // so the staircase alone (just the opener's level) would under-draw. The
        // look-ahead to the next content row pulls the body's guide onto the
        // empty line, so the guide is continuous from the opener down.
        let content = "if outer {\n\n    inner();\n}\n";
        let out = render_output_scrolled_with_indentation_guide(content, 0);
        assert_eq!(rendered_line_text(&out, 0), "if outer {");
        assert_eq!(rendered_line_text(&out, 1), "▏"); // flows into the body below
        assert_eq!(rendered_line_text(&out, 2), "▏   inner();");
    }

    #[test]
    fn indentation_guide_render_for_tabs() {
        let (output, _, _, _) =
            render_output_for_with_indentation_guide("\tchild\n\t\tgrand\n", 0, 0);

        assert_eq!(rendered_line_text(&output, 0), "▏   child");
        assert_eq!(rendered_line_text(&output, 1), "▏   ▏   grand");
    }

    #[test]
    fn indentation_guide_respect_horizontal_scroll() {
        let (output, _, _, _) = render_output_for_with_indentation_guide("        grand\n", 0, 4);

        assert_eq!(rendered_line_text(&output, 0), "▏   grand");
    }

    #[test]
    fn indentation_guide_use_configured_glyph() {
        let (output, _, _, _) = render_output_for_with_options(
            "        grand\n",
            0,
            false,
            IndentationGuideMode::All,
            "┊".to_string(),
            0,
            None,
        );

        assert_eq!(rendered_line_text(&output, 0), "┊   ┊   grand");
    }

    #[test]
    fn indentation_guide_renderer_normalizes_blank_and_padded_glyphs() {
        let (output, _, _, _) = render_output_for_with_options(
            "    child\n",
            0,
            false,
            IndentationGuideMode::All,
            "  ┊  ".to_string(),
            0,
            None,
        );
        assert_eq!(rendered_line_text(&output, 0), "┊   child");

        let (output, _, _, _) = render_output_for_with_options(
            "    child\n",
            0,
            false,
            IndentationGuideMode::All,
            "   ".to_string(),
            0,
            None,
        );
        assert_eq!(rendered_line_text(&output, 0), "▏   child");
    }

    #[test]
    fn indentation_guide_renderer_uses_one_character_from_glyph_setting() {
        let (output, _, _, _) = render_output_for_with_options(
            "    child\n",
            0,
            false,
            IndentationGuideMode::All,
            "  ABC  ".to_string(),
            0,
            None,
        );

        assert_eq!(rendered_line_text(&output, 0), "A   child");
    }

    #[test]
    fn indentation_guide_renderer_rejects_double_width_glyphs() {
        let (output, _, _, _) = render_output_for_with_options(
            "    child\n",
            0,
            false,
            IndentationGuideMode::All,
            "😀".to_string(),
            0,
            None,
        );

        assert_eq!(rendered_line_text(&output, 0), "▏   child");
    }

    #[test]
    fn indentation_guide_active_mode_renders_only_innermost_active_block() {
        let content = "    if ready {\n        inner\n        sibling\n    }\n";
        let cursor_pos = content.find("inner").unwrap();
        let (output, _, _, _) = render_output_for_with_indentation_guide_mode(
            content,
            cursor_pos,
            0,
            IndentationGuideMode::Active,
        );

        assert_eq!(rendered_line_text(&output, 0), "    if ready {");
        assert_eq!(rendered_line_text(&output, 1), "    ▏   inner");
        assert_eq!(rendered_line_text(&output, 2), "    ▏   sibling");
        assert_eq!(rendered_line_text(&output, 3), "    }");
    }

    #[test]
    fn indentation_guide_active_mode_updates_when_cursor_changes_blocks() {
        // Two sibling blocks: the active guide follows whichever block encloses
        // the cursor.
        let content = "    a\n        x\n    b\n        y\n";

        let first_cursor = content.find('x').unwrap();
        let (first_output, _, _, _) = render_output_for_with_indentation_guide_mode(
            content,
            first_cursor,
            0,
            IndentationGuideMode::Active,
        );
        assert_eq!(rendered_line_text(&first_output, 1), "    ▏   x");
        assert_eq!(rendered_line_text(&first_output, 3), "        y");

        let second_cursor = content.find('y').unwrap();
        let (second_output, _, _, _) = render_output_for_with_indentation_guide_mode(
            content,
            second_cursor,
            0,
            IndentationGuideMode::Active,
        );
        assert_eq!(rendered_line_text(&second_output, 1), "        x");
        assert_eq!(rendered_line_text(&second_output, 3), "    ▏   y");
    }

    #[test]
    fn indentation_guide_active_mode_supports_tabs() {
        let content = "\tchild\n\t\tgrand\n";
        let cursor_pos = content.find("grand").unwrap();
        let (output, _, _, _) = render_output_for_with_indentation_guide_mode(
            content,
            cursor_pos,
            0,
            IndentationGuideMode::Active,
        );

        // Tab cells that are not replaced by the active guide retain the
        // existing leading-tab whitespace indicator.
        assert_eq!(rendered_line_text(&output, 0), "→   child");
        assert_eq!(rendered_line_text(&output, 1), "→   ▏   grand");
    }

    #[test]
    fn indentation_guide_active_mode_cursor_on_block_header_uses_child_block() {
        // The cursor's line heads a more-indented block, so the active guide is
        // the child block's guide (one level in), not the header's own level.
        // Block detection is purely indentation-based, so this holds regardless
        // of the trailing delimiter (`{`, `(`, `[`, `:`, …).
        let content = "    if (1) {\n        // test\n    }\n";
        let cursor_pos = content.find('{').unwrap();
        let (output, _, _, _) = render_output_for_with_indentation_guide_mode(
            content,
            cursor_pos,
            0,
            IndentationGuideMode::Active,
        );

        assert_eq!(rendered_line_text(&output, 0), "    if (1) {");
        assert_eq!(rendered_line_text(&output, 1), "    ▏   // test");
        assert_eq!(rendered_line_text(&output, 2), "    }");
    }

    #[test]
    fn indentation_guide_active_mode_cursor_in_body_uses_enclosing_block() {
        let content = "    if (1) {\n        // test\n    }\n";
        let cursor_pos = content.find("// test").unwrap();
        let (output, _, _, _) = render_output_for_with_indentation_guide_mode(
            content,
            cursor_pos,
            0,
            IndentationGuideMode::Active,
        );

        assert_eq!(rendered_line_text(&output, 0), "    if (1) {");
        assert_eq!(rendered_line_text(&output, 1), "    ▏   // test");
        assert_eq!(rendered_line_text(&output, 2), "    }");
    }

    #[test]
    fn indentation_guide_active_mode_dedent_line_with_no_parent_has_no_guide() {
        // The cursor sits on a dedent line (`}`) at the outermost indent here,
        // so it belongs to no enclosing block and draws no guide.
        let content = "    if (1) {\n        // test\n    }\n";
        let cursor_pos = content.find("    }").unwrap() + 4;
        let (output, _, _, _) = render_output_for_with_indentation_guide_mode(
            content,
            cursor_pos,
            0,
            IndentationGuideMode::Active,
        );

        assert_eq!(rendered_line_text(&output, 0), "    if (1) {");
        assert_eq!(rendered_line_text(&output, 1), "        // test");
        assert_eq!(rendered_line_text(&output, 2), "    }");
    }

    #[test]
    fn indentation_guide_active_mode_dedent_line_uses_enclosing_block_when_nested() {
        // A dedent line that still sits inside an outer block draws that outer
        // block's guide.
        let content = "fn () {\n    if (1) {\n        // test\n    }\n}\n";
        let cursor_pos = content.find("    }").unwrap() + 4;
        let (output, _, _, _) = render_output_for_with_indentation_guide_mode(
            content,
            cursor_pos,
            0,
            IndentationGuideMode::Active,
        );

        // Viewport is 4 rows tall in this harness, so the trailing `}` (line 4)
        // is off-screen; the dedent line `}` (line 3) carries the outer guide.
        assert_eq!(rendered_line_text(&output, 0), "fn () {");
        assert_eq!(rendered_line_text(&output, 1), "▏   if (1) {");
        assert_eq!(rendered_line_text(&output, 2), "▏       // test");
        assert_eq!(rendered_line_text(&output, 3), "▏   }");
    }

    #[test]
    fn indentation_guide_all_mode_is_cursor_independent() {
        // `all` mode draws every level regardless of cursor position.
        let content = "    if (1) {\n        // test\n    }\n";
        let cursor_pos = content.find('{').unwrap() + 1;
        let (output, _, _, _) = render_output_for_with_indentation_guide(content, cursor_pos, 0);

        assert_eq!(rendered_line_text(&output, 0), "▏   if (1) {");
        assert_eq!(rendered_line_text(&output, 1), "▏   ▏   // test");
        assert_eq!(rendered_line_text(&output, 2), "▏   }");
    }

    #[test]
    fn indentation_guide_active_mode_root_header_draws_column_zero_guide() {
        // A cursor on a root-level header still gets the child block's guide,
        // which lands at column 0 — consistent with `all` mode.
        let (output, _, _, _) = render_output_for_with_indentation_guide_mode(
            "root\n    child\n",
            0,
            0,
            IndentationGuideMode::Active,
        );

        assert_eq!(rendered_line_text(&output, 0), "root");
        assert_eq!(rendered_line_text(&output, 1), "▏   child");
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
            &[],
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
    fn fold_indicator_lands_on_header_not_blank_line_after_it() {
        // A blank line immediately after a foldable header must not steal the
        // fold marker. The indent-based detector consumes `ViewLine::text`,
        // which keeps the trailing `\n`; before `slice_indent` treated `\n` as a
        // terminator, a bare "\n" read as indent-0 *content*, so `int main() {`
        // looked unfoldable (its next "non-blank" line was the blank one) and the
        // blank line itself looked like the indent-0 header of the body below.
        let content = "int main() {\n\n    body();\n    \n    more();\n}\n";
        let mut state = EditorState::new(40, 10, 1024, test_fs());
        state.buffer = Buffer::from_str(content, 1024, test_fs());
        let viewport = Viewport::new(40, 10);
        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();
        let folds = FoldManager::new();
        let view_data = build_view_data(
            &mut state,
            &viewport,
            None,
            content.len().max(1),
            viewport.visible_line_count(),
            false,
            40,
            0,
            &ViewMode::Source,
            &folds,
            &theme,
            &[],
        );

        let indicators = fold_indicators_for_viewport(&state, &folds, &view_data.lines);

        let header_byte = 0; // `int main() {`
        let blank_byte = content.find("\n\n").unwrap() + 1; // the empty line
        assert!(
            indicators.contains_key(&header_byte),
            "fold marker should be on the function header"
        );
        assert!(
            !indicators.contains_key(&blank_byte),
            "fold marker must not be on the blank line"
        );
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
            virtual_gutter_glyph: None,
            virtual_line_style: None,
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

    /// Helper for issue-1363 tests: tokenize a plain ASCII string into
    /// `Text` / `Space` tokens the same way `build_base_tokens` would
    /// (one `Space` per literal ' '; runs of non-space chars coalesce
    /// into a single `Text`).
    fn tokenize_for_wrap(text: &str) -> Vec<fresh_core::api::ViewTokenWire> {
        use fresh_core::api::{ViewTokenWire, ViewTokenWireKind};
        let mut tokens = Vec::new();
        let mut buf = String::new();
        let mut buf_start: Option<usize> = None;
        for (i, ch) in text.char_indices() {
            if ch == ' ' {
                if !buf.is_empty() {
                    tokens.push(ViewTokenWire {
                        source_offset: buf_start,
                        kind: ViewTokenWireKind::Text(std::mem::take(&mut buf)),
                        style: None,
                    });
                    buf_start = None;
                }
                tokens.push(ViewTokenWire {
                    source_offset: Some(i),
                    kind: ViewTokenWireKind::Space,
                    style: None,
                });
            } else {
                if buf.is_empty() {
                    buf_start = Some(i);
                }
                buf.push(ch);
            }
        }
        if !buf.is_empty() {
            tokens.push(ViewTokenWire {
                source_offset: buf_start,
                kind: ViewTokenWireKind::Text(buf),
                style: None,
            });
        }
        tokens
    }

    /// Materialise the row strings emitted by `apply_wrapping_transform`
    /// by walking its token output and splitting on `Break`.
    fn rows_from_wrapped(wrapped: &[fresh_core::api::ViewTokenWire]) -> Vec<String> {
        use fresh_core::api::ViewTokenWireKind;
        let mut rows: Vec<String> = vec![String::new()];
        for tok in wrapped {
            match &tok.kind {
                ViewTokenWireKind::Text(s) => rows.last_mut().unwrap().push_str(s),
                ViewTokenWireKind::Space => rows.last_mut().unwrap().push(' '),
                ViewTokenWireKind::Newline => {}
                ViewTokenWireKind::Break => rows.push(String::new()),
                ViewTokenWireKind::BinaryByte(_) => {}
            }
        }
        if rows.last().map(|r| r.is_empty()).unwrap_or(false) {
            rows.pop();
        }
        rows
    }

    /// Issue #1363: wrap should not leak a leading space onto the
    /// continuation row.  The fix moves the last word on the prior
    /// row to the continuation row when a trailing Space would
    /// otherwise overflow — `["AAAAA BBBBBB", " CCCC"]` becomes
    /// `["AAAAA ", "BBBBBB CCCC"]`.
    #[test]
    fn issue_1363_no_leading_space_on_continuation_row() {
        let tokens = tokenize_for_wrap("AAAAA BBBBBB CCCC");
        let wrapped = apply_wrapping_transform(tokens, 12, 0, false);
        let rows = rows_from_wrapped(&wrapped);
        assert_eq!(rows.len(), 2, "expected 2 rows, got {:?}", rows);
        for (i, row) in rows.iter().enumerate() {
            assert!(
                !row.starts_with(' '),
                "row {i} {:?} starts with whitespace (issue #1363): rows = {:?}",
                row,
                rows,
            );
            assert!(
                row.chars().count() <= 12,
                "row {i} {:?} width {} exceeds eff_width 12 (issue #1363): no char may overflow",
                row,
                row.chars().count(),
            );
        }
    }

    /// Issue #1363: source content is preserved across the wrap —
    /// concatenating the rows recovers the original text.  Guards
    /// against the back-up logic dropping or duplicating tokens.
    #[test]
    fn issue_1363_back_up_preserves_content() {
        let input = "AAAAA BBBBBB CCCC";
        let tokens = tokenize_for_wrap(input);
        let wrapped = apply_wrapping_transform(tokens, 12, 0, false);
        let rows = rows_from_wrapped(&wrapped);
        let reconstructed: String = rows.concat();
        assert_eq!(reconstructed, input, "rows = {:?}", rows);
    }

    /// Issue #1363: when the row contains only one word (e.g. a
    /// char-split chunk), there's no prior Space to back up to.  The
    /// fix degrades gracefully to the old behaviour — the residual
    /// leading-space case is accepted as out of scope.  The invariant
    /// we DO maintain is that no row's visible content exceeds the
    /// effective width.
    #[test]
    fn issue_1363_single_word_row_falls_back() {
        use fresh_core::api::{ViewTokenWire, ViewTokenWireKind};
        let tokens = vec![
            ViewTokenWire {
                source_offset: Some(0),
                kind: ViewTokenWireKind::Text("XXXXXXXX".to_string()),
                style: None,
            },
            ViewTokenWire {
                source_offset: Some(8),
                kind: ViewTokenWireKind::Space,
                style: None,
            },
            ViewTokenWire {
                source_offset: Some(9),
                kind: ViewTokenWireKind::Text("YYYY".to_string()),
                style: None,
            },
        ];
        let wrapped = apply_wrapping_transform(tokens, 8, 0, false);
        let rows = rows_from_wrapped(&wrapped);
        for row in &rows {
            assert!(
                row.chars().count() <= 8,
                "row {:?} exceeds eff_width 8 in fallback case",
                row,
            );
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
            &[],
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
            viewport.left_column,
            render_area.width as usize,
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
            indentation_guide: IndentationGuideMode::None,
            indentation_guide_glyph: "▏",
            rainbow_indentation: false,
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

    /// Agreement test: the standalone `wrap_str_to_width` helper used by
    /// the virtual-line path must produce the same chunk boundaries as
    /// `apply_wrapping_transform` does for a single Text token starting
    /// on a fresh row (no tabs, no ANSI, no hanging indent).  This
    /// pins the two implementations together so the doc-comment claim
    /// "virtual lines wrap like source lines" stays honest.
    #[test]
    fn wrap_str_to_width_matches_apply_wrapping_transform() {
        use crate::primitives::visual_layout::wrap_str_to_width;
        use fresh_core::api::{ViewTokenWire, ViewTokenWireKind};

        // A range of inputs that exercise both the word-boundary and
        // hard-cap fallback paths.  Each (text, wrap_width) pair must
        // produce identical chunk byte boundaries on both code paths.
        let cases: &[(&str, usize)] = &[
            ("hello world how are you today friend", 12),
            ("the quick brown fox jumps over the lazy dog", 18),
            ("https://example.com/very-long-path/file", 24),
            (&"x".repeat(120), 32),
            (&"abc ".repeat(40), 25),
            ("dialog.getButton(...).setOnClickListener", 24),
        ];

        for &(text, wrap_width) in cases {
            // Direct helper output.
            let helper_chunks = wrap_str_to_width(text, wrap_width);
            let helper_strings: Vec<&str> =
                helper_chunks.iter().map(|r| &text[r.clone()]).collect();

            // Run the full transform on a single Text token.  Use
            // `gutter_width = 0` so `available_width == content_width`
            // and the transform's effective wrap width matches what
            // we pass to `wrap_str_to_width`.
            let tokens = vec![ViewTokenWire {
                kind: ViewTokenWireKind::Text(text.to_string()),
                source_offset: Some(0),
                style: None,
            }];
            let wrapped = apply_wrapping_transform(tokens, wrap_width, 0, false);

            // Reconstruct the chunks the transform emitted by walking
            // its output: each Text token is one chunk; Break tokens
            // delimit chunks.  Skip standalone Spaces/etc. — they
            // don't appear in our pure-text inputs.
            let mut transform_strings: Vec<String> = Vec::new();
            for tok in &wrapped {
                match &tok.kind {
                    ViewTokenWireKind::Text(t) => transform_strings.push(t.clone()),
                    ViewTokenWireKind::Break => {}
                    other => panic!("unexpected token kind in agreement test: {:?}", other),
                }
            }

            assert_eq!(
                transform_strings
                    .iter()
                    .map(String::as_str)
                    .collect::<Vec<_>>(),
                helper_strings,
                "wrap mismatch for text={text:?} wrap_width={wrap_width}",
            );
        }
    }
}
