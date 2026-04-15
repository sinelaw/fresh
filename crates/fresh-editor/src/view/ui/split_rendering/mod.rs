//! Split pane layout and buffer rendering

mod base_tokens;
mod char_style;
mod folding;
mod gutter;
mod layout;
mod post_pass;
mod scrollbar;
mod spans;
mod style;
mod transforms;
mod view_data;

use base_tokens::build_base_tokens;
use folding::{
    apply_folding, diff_indicators_for_viewport, fold_adjusted_visible_count,
    fold_indicators_for_viewport, FoldIndicator,
};
use gutter::{render_compose_margins, render_left_margin, LeftMarginContext};
use layout::{
    calculate_compose_layout, calculate_view_anchor, calculate_viewport_end, render_separator,
    resolve_view_preferences, split_buffers_for_tabs, split_layout, sync_viewport_to_content,
    ComposeLayout, SplitLayout, ViewAnchor, ViewPreferences,
};
use scrollbar::{
    compute_max_line_length, render_composite_scrollbar, render_horizontal_scrollbar,
    render_scrollbar, scrollbar_line_counts,
};
use view_data::{build_view_data, ViewData};
use transforms::{
    apply_conceal_ranges, apply_soft_breaks, apply_wrapping_transform, inject_virtual_lines,
};
use char_style::{compute_char_style, CharStyleContext, CharStyleOutput};
use post_pass::{
    apply_background_to_lines, apply_hyperlink_overlays, apply_osc8_to_cells, render_column_guides,
    render_ruler_bg,
};
use spans::{
    compress_chars, compute_inline_diff, push_debug_tag, push_span_with_map, span_color_at,
    span_info_at, DebugSpanTracker, SpanAccumulator,
};
use style::{
    append_fold_placeholder, create_virtual_line, dim_color_for_tilde, fold_placeholder_style,
    inline_diagnostic_style,
};

use std::collections::BTreeMap;

use crate::app::types::ViewLineMapping;
use crate::app::BufferMetadata;
use crate::model::buffer::Buffer;
use crate::model::cursor::SelectionMode;
use crate::model::event::{BufferId, EventLog, LeafId, SplitDirection};
use crate::primitives::ansi::AnsiParser;
use crate::primitives::ansi_background::AnsiBackground;
use crate::primitives::display_width::char_width;
use crate::state::{EditorState, ViewMode};
use crate::view::folding::FoldManager;
use crate::view::split::SplitManager;
use crate::view::ui::tabs::TabsRenderer;
use crate::view::ui::view_pipeline::{
    should_show_line_number, LineStart, ViewLine, ViewLineIterator,
};
use crate::view::virtual_text::VirtualTextPosition;
use fresh_core::api::{ViewTokenStyle, ViewTransformPayload};
use ratatui::layout::Rect;
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Clear, Paragraph};
use ratatui::Frame;
use std::collections::{HashMap, HashSet};
use std::ops::Range;

/// Maximum line width before forced wrapping is applied, even when line wrapping is disabled.
/// This prevents memory exhaustion when opening files with extremely long lines (e.g., 10MB
/// single-line JSON files). Lines exceeding this width are wrapped into multiple visual lines,
/// each bounded to this width. 10,000 columns is far wider than any monitor while keeping
/// memory usage reasonable (~80KB per ViewLine instead of hundreds of MB).
const MAX_SAFE_LINE_WIDTH: usize = 10_000;



struct SelectionContext {
    ranges: Vec<Range<usize>>,
    block_rects: Vec<(usize, usize, usize, usize)>,
    cursor_positions: Vec<usize>,
    primary_cursor_position: usize,
}

struct DecorationContext {
    highlight_spans: Vec<crate::primitives::highlighter::HighlightSpan>,
    semantic_token_spans: Vec<crate::primitives::highlighter::HighlightSpan>,
    viewport_overlays: Vec<(crate::view::overlay::Overlay, Range<usize>)>,
    virtual_text_lookup: HashMap<usize, Vec<crate::view::virtual_text::VirtualText>>,
    /// Diagnostic lines indexed by line-start byte offset
    diagnostic_lines: HashSet<usize>,
    /// Inline diagnostic text per line (line_start_byte -> (message, style))
    /// Derived from viewport overlays; highest severity wins per line.
    diagnostic_inline_texts: HashMap<usize, (String, Style)>,
    /// Line indicators indexed by line-start byte offset
    line_indicators: BTreeMap<usize, crate::view::margin::LineIndicator>,
    /// Fold indicators indexed by line-start byte offset
    fold_indicators: BTreeMap<usize, FoldIndicator>,
}

struct LineRenderOutput {
    lines: Vec<Line<'static>>,
    cursor: Option<(u16, u16)>,
    last_line_end: Option<LastLineEnd>,
    content_lines_rendered: usize,
    view_line_mappings: Vec<ViewLineMapping>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct LastLineEnd {
    pos: (u16, u16),
    terminated_with_newline: bool,
}

/// Output of the pure layout computation phase of buffer rendering.
/// Contains everything the drawing phase needs to produce the final frame.
struct BufferLayoutOutput {
    view_line_mappings: Vec<ViewLineMapping>,
    render_output: LineRenderOutput,
    render_area: Rect,
    compose_layout: ComposeLayout,
    effective_editor_bg: Color,
    view_mode: ViewMode,
    left_column: usize,
    gutter_width: usize,
    buffer_ends_with_newline: bool,
    selection: SelectionContext,
}

struct LineRenderInput<'a> {
    state: &'a EditorState,
    theme: &'a crate::view::theme::Theme,
    /// Display lines from the view pipeline (each line has its own mappings, styles, etc.)
    view_lines: &'a [ViewLine],
    view_anchor: ViewAnchor,
    render_area: Rect,
    gutter_width: usize,
    selection: &'a SelectionContext,
    decorations: &'a DecorationContext,
    visible_line_count: usize,
    lsp_waiting: bool,
    is_active: bool,
    line_wrap: bool,
    estimated_lines: usize,
    /// Left column offset for horizontal scrolling
    left_column: usize,
    /// Whether to show relative line numbers (distance from cursor)
    relative_line_numbers: bool,
    /// Skip REVERSED style on the primary cursor (session mode or non-block cursor style)
    session_mode: bool,
    /// No hardware cursor: always render software cursor indicators
    software_cursor_only: bool,
    /// Whether to show line numbers in the gutter
    show_line_numbers: bool,
    /// Whether the gutter shows byte offsets instead of line numbers
    /// (large file without line index scan)
    byte_offset_mode: bool,
    /// Whether to show tilde (~) markers on lines past end-of-file
    show_tilde: bool,
    /// Whether to highlight the line containing the cursor
    highlight_current_line: bool,
    /// Per-cell theme key map for the theme inspector (screen_width used for indexing)
    cell_theme_map: &'a mut Vec<crate::app::types::CellThemeInfo>,
    /// Screen width for cell_theme_map indexing
    screen_width: u16,
}




/// Renders split panes and their content
pub struct SplitRenderer;

impl SplitRenderer {
    /// Render the main content area with all splits
    ///
    /// # Arguments
    /// * `frame` - The ratatui frame to render to
    /// * `area` - The rectangular area to render in
    /// * `split_manager` - The split manager
    /// * `buffers` - All open buffers
    /// * `buffer_metadata` - Metadata for buffers (contains display names)
    /// * `event_logs` - Event logs for each buffer
    /// * `theme` - The active theme for colors
    /// * `lsp_waiting` - Whether LSP is waiting
    /// * `large_file_threshold_bytes` - Threshold for using constant scrollbar thumb size
    /// * `line_wrap` - Whether line wrapping is enabled
    /// * `estimated_line_length` - Estimated average line length for large file line estimation
    /// * `hide_cursor` - Whether to hide the hardware cursor (e.g., when menu is open)
    ///
    /// # Returns
    /// * Vec of (split_id, buffer_id, content_rect, scrollbar_rect, thumb_start, thumb_end) for mouse handling
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
        _line_wrap: bool,
        estimated_line_length: usize,
        highlight_context_bytes: usize,
        mut split_view_states: Option<&mut HashMap<LeafId, crate::view::split::SplitViewState>>,
        grouped_subtrees: &HashMap<LeafId, crate::view::split::SplitNode>,
        hide_cursor: bool,
        hovered_tab: Option<(crate::view::split::TabTarget, LeafId, bool)>, // (target, split_id, is_close_button)
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
        cell_theme_map: &mut Vec<crate::app::types::CellThemeInfo>,
        screen_width: u16,
    ) -> (
        Vec<(LeafId, BufferId, Rect, Rect, usize, usize)>,
        HashMap<LeafId, crate::view::ui::tabs::TabLayout>, // tab layouts per split
        Vec<(LeafId, u16, u16, u16)>,                      // close split button areas
        Vec<(LeafId, u16, u16, u16)>,                      // maximize split button areas
        HashMap<LeafId, Vec<ViewLineMapping>>,             // view line mappings for mouse clicks
        Vec<(LeafId, BufferId, Rect, usize, usize, usize)>, // horizontal scrollbar areas (rect + max_content_width + thumb_start + thumb_end)
        Vec<(
            crate::model::event::ContainerId,
            SplitDirection,
            u16,
            u16,
            u16,
        )>, // hit areas for separators inside active Grouped subtrees
    ) {
        let _span = tracing::trace_span!("render_content").entered();

        // Get all visible splits with their areas.
        //
        // Each entry in `visible_buffers` is
        //   (tab_bar_owner_split, effective_leaf_id, buffer_id, split_area, kind)
        //
        // where `kind` is:
        //   - `Normal`: regular split. Render tab bar + buffer content.
        //   - `GroupTabBarOnly`: main split where a group is active. Render
        //     the tab bar (to show the group tab) but skip buffer content
        //     (the group's inner leaves will fill it).
        //   - `InnerLeaf`: a leaf inside a Grouped subtree. `split_area` is
        //     the already-computed content rect for this inner leaf; no tab
        //     bar is rendered.
        #[derive(Copy, Clone, PartialEq, Eq)]
        enum RenderKind {
            Normal,
            GroupTabBarOnly,
            InnerLeaf,
        }

        let base_visible = split_manager.get_visible_buffers(area);
        let active_split_id = split_manager.active_split();
        let has_multiple_splits = base_visible.len() > 1;

        // Expand groups: for each main leaf, if its SplitViewState has an
        // active group tab, emit a tab-bar-only entry for the main split
        // followed by one InnerLeaf entry per panel.
        let mut visible_buffers: Vec<(LeafId, LeafId, BufferId, Rect, RenderKind)> = Vec::new();
        for (main_split_id, main_buffer_id, split_area) in &base_visible {
            let active_group = split_view_states
                .as_deref()
                .and_then(|svs| svs.get(main_split_id))
                .and_then(|vs| vs.active_group_tab);

            if let Some(group_leaf) = active_group {
                if let Some(grouped) = grouped_subtrees.get(&group_leaf) {
                    // Compute the content rect for this main split (after tab bar).
                    let split_tab_bar_visible = tab_bar_visible
                        && !split_view_states
                            .as_deref()
                            .and_then(|svs| svs.get(main_split_id))
                            .is_some_and(|vs| vs.suppress_chrome);
                    let main_layout = split_layout(
                        *split_area,
                        split_tab_bar_visible,
                        show_vertical_scrollbar,
                        show_horizontal_scrollbar,
                    );
                    let inner_leaves = grouped.get_leaves_with_rects(main_layout.content_rect);
                    visible_buffers.push((
                        *main_split_id,
                        *main_split_id,
                        *main_buffer_id,
                        *split_area,
                        RenderKind::GroupTabBarOnly,
                    ));
                    for (inner_leaf, inner_buffer, inner_rect) in &inner_leaves {
                        // Keep inner panel viewports in sync with their actual
                        // rendered dimensions. This ensures editor.getViewport()
                        // returns the correct panel size (not the terminal size)
                        // and fixes resize-timing issues since the viewport is
                        // updated synchronously during rendering.
                        if let Some(svs) = split_view_states.as_deref_mut() {
                            if let Some(vs) = svs.get_mut(inner_leaf) {
                                vs.viewport.resize(inner_rect.width, inner_rect.height);
                            }
                        }
                        visible_buffers.push((
                            *main_split_id,
                            *inner_leaf,
                            *inner_buffer,
                            *inner_rect,
                            RenderKind::InnerLeaf,
                        ));
                    }
                    continue;
                }
            }

            visible_buffers.push((
                *main_split_id,
                *main_split_id,
                *main_buffer_id,
                *split_area,
                RenderKind::Normal,
            ));
        }

        // Collect areas for mouse handling
        let mut split_areas = Vec::new();
        let mut horizontal_scrollbar_areas: Vec<(LeafId, BufferId, Rect, usize, usize, usize)> =
            Vec::new();
        let mut tab_layouts: HashMap<LeafId, crate::view::ui::tabs::TabLayout> = HashMap::new();
        let mut close_split_areas = Vec::new();
        let mut maximize_split_areas = Vec::new();
        let mut view_line_mappings: HashMap<LeafId, Vec<ViewLineMapping>> = HashMap::new();

        // Render each split.
        for (main_split_id, split_id, buffer_id, split_area, kind) in visible_buffers {
            let is_active = split_id == active_split_id;
            let is_inner_group_leaf = kind == RenderKind::InnerLeaf;
            let skip_content = kind == RenderKind::GroupTabBarOnly;
            let _ = main_split_id; // no longer needed below, kept for clarity

            // Suppress chrome (tab bar) for splits in buffer groups
            let split_tab_bar_visible = !is_inner_group_leaf
                && tab_bar_visible
                && !split_view_states
                    .as_deref()
                    .and_then(|svs| svs.get(&split_id))
                    .is_some_and(|vs| vs.suppress_chrome);
            // Hide tildes per-split (e.g., for buffer group panels)
            let split_show_tilde = show_tilde
                && !split_view_states
                    .as_deref()
                    .and_then(|svs| svs.get(&split_id))
                    .is_some_and(|vs| vs.hide_tilde);

            // Non-scrollable panels (Fixed toolbars/headers/footers by default,
            // or any panel created with `scrollable: false`) don't get a
            // scrollbar — their content is pinned to the panel size.
            let is_non_scrollable = buffers.get(&buffer_id).is_some_and(|s| !s.scrollable);
            let panel_show_vscroll = show_vertical_scrollbar && !is_non_scrollable;

            let layout = if is_inner_group_leaf {
                // Inner leaf: split_area IS the content rect already.
                SplitLayout {
                    tabs_rect: Rect::new(split_area.x, split_area.y, 0, 0),
                    content_rect: Rect::new(
                        split_area.x,
                        split_area.y,
                        split_area
                            .width
                            .saturating_sub(if panel_show_vscroll { 1 } else { 0 }),
                        split_area.height,
                    ),
                    scrollbar_rect: Rect::new(
                        split_area.x + split_area.width.saturating_sub(1),
                        split_area.y,
                        if panel_show_vscroll { 1 } else { 0 },
                        split_area.height,
                    ),
                    horizontal_scrollbar_rect: Rect::new(0, 0, 0, 0),
                }
            } else {
                split_layout(
                    split_area,
                    split_tab_bar_visible,
                    show_vertical_scrollbar && !is_non_scrollable,
                    show_horizontal_scrollbar && !is_non_scrollable,
                )
            };
            let (split_buffers, tab_scroll_offset) = if is_inner_group_leaf {
                (Vec::new(), 0)
            } else {
                split_buffers_for_tabs(split_view_states.as_deref(), split_id, buffer_id)
            };

            // Determine hover state for this split's tabs
            let tab_hover_for_split = hovered_tab.and_then(|(hover_buf, hover_split, is_close)| {
                if hover_split == split_id {
                    Some((hover_buf, is_close))
                } else {
                    None
                }
            });

            // Only render tabs and split control buttons when tab bar is visible
            if split_tab_bar_visible {
                // Determine the active target for this split's tab bar.
                // If the split's SplitViewState marks a group tab as active,
                // that's the active target; otherwise the currently displayed
                // buffer.
                let active_target = split_view_states
                    .as_deref()
                    .and_then(|svs| svs.get(&split_id))
                    .map(|vs| vs.active_target())
                    .unwrap_or(crate::view::split::TabTarget::Buffer(buffer_id));
                // Collect group names from the stashed Grouped subtrees.
                let group_names: HashMap<LeafId, String> = grouped_subtrees
                    .iter()
                    .filter_map(|(leaf_id, node)| {
                        if let crate::view::split::SplitNode::Grouped { name, .. } = node {
                            Some((*leaf_id, name.clone()))
                        } else {
                            None
                        }
                    })
                    .collect();
                // Render tabs for this split and collect hit areas
                let tab_layout = TabsRenderer::render_for_split(
                    frame,
                    layout.tabs_rect,
                    &split_buffers,
                    buffers,
                    buffer_metadata,
                    composite_buffers,
                    active_target,
                    theme,
                    is_active,
                    tab_scroll_offset,
                    tab_hover_for_split,
                    &group_names,
                );

                // Store the tab layout for this split
                tab_layouts.insert(split_id, tab_layout);
                let tab_row = layout.tabs_rect.y;

                // Render split control buttons at the right side of tabs row
                // Show maximize/unmaximize button when: multiple splits exist OR we're currently maximized
                // Show close button when: multiple splits exist AND we're not maximized
                let show_maximize_btn = has_multiple_splits || is_maximized;
                let show_close_btn = has_multiple_splits && !is_maximized;

                if show_maximize_btn || show_close_btn {
                    // Calculate button positions from right edge
                    // Layout: [maximize] [space] [close] |
                    let mut btn_x = layout.tabs_rect.x + layout.tabs_rect.width.saturating_sub(2);

                    // Render close button first (rightmost) if visible
                    if show_close_btn {
                        let is_hovered = hovered_close_split == Some(split_id);
                        let close_fg = if is_hovered {
                            theme.tab_close_hover_fg
                        } else {
                            theme.line_number_fg
                        };
                        let close_button = Paragraph::new("×")
                            .style(Style::default().fg(close_fg).bg(theme.tab_separator_bg));
                        let close_area = Rect::new(btn_x, tab_row, 1, 1);
                        frame.render_widget(close_button, close_area);
                        close_split_areas.push((split_id, tab_row, btn_x, btn_x + 1));
                        btn_x = btn_x.saturating_sub(2); // Move left with 1 space for next button
                    }

                    // Render maximize/unmaximize button
                    if show_maximize_btn {
                        let is_hovered = hovered_maximize_split == Some(split_id);
                        let max_fg = if is_hovered {
                            theme.tab_close_hover_fg
                        } else {
                            theme.line_number_fg
                        };
                        // Use □ for maximize, ⧉ for unmaximize (restore)
                        let icon = if is_maximized { "⧉" } else { "□" };
                        let max_button = Paragraph::new(icon)
                            .style(Style::default().fg(max_fg).bg(theme.tab_separator_bg));
                        let max_area = Rect::new(btn_x, tab_row, 1, 1);
                        frame.render_widget(max_button, max_area);
                        maximize_split_areas.push((split_id, tab_row, btn_x, btn_x + 1));
                    }
                }
            }

            // For GroupTabBarOnly entries we've already rendered the tab bar;
            // skip buffer content rendering so the group's inner leaves can
            // draw into the content rect without being overwritten.
            if skip_content {
                view_line_mappings.insert(split_id, Vec::new());
                continue;
            }

            // Get references separately to avoid double borrow
            let state_opt = buffers.get_mut(&buffer_id);
            let event_log_opt = event_logs.get_mut(&buffer_id);

            if let Some(state) = state_opt {
                // Check if this is a composite buffer - render differently
                if state.is_composite_buffer {
                    // Take initial_focus_hunk before borrowing composite immutably
                    let initial_focus_hunk = composite_buffers
                        .get_mut(&buffer_id)
                        .and_then(|c| c.initial_focus_hunk.take());
                    if let Some(composite) = composite_buffers.get(&buffer_id) {
                        // Update SplitViewState viewport to match actual rendered area
                        // This ensures cursor movement uses correct viewport height after resize
                        if let Some(ref mut svs) = split_view_states {
                            if let Some(split_vs) = svs.get_mut(&split_id) {
                                if split_vs.viewport.width != layout.content_rect.width
                                    || split_vs.viewport.height != layout.content_rect.height
                                {
                                    split_vs.viewport.resize(
                                        layout.content_rect.width,
                                        layout.content_rect.height,
                                    );
                                }
                            }
                        }

                        // Get or create composite view state
                        let pane_count = composite.pane_count();
                        let view_state = composite_view_states
                            .entry((split_id, buffer_id))
                            .or_insert_with(|| {
                                crate::view::composite_view::CompositeViewState::new(
                                    buffer_id, pane_count,
                                )
                            });

                        // Apply deferred initial focus hunk (first render only).
                        // This runs here because it's the only place where both the
                        // CompositeViewState and the correct viewport height exist.
                        if let Some(hunk_index) = initial_focus_hunk {
                            let mut target_row = None;
                            // Walk hunk headers to find the Nth one
                            let mut hunk_count = 0usize;
                            for (row_idx, row) in composite.alignment.rows.iter().enumerate() {
                                if row.row_type
                                    == crate::model::composite_buffer::RowType::HunkHeader
                                {
                                    if hunk_count == hunk_index {
                                        target_row = Some(row_idx);
                                        break;
                                    }
                                    hunk_count += 1;
                                }
                            }
                            if let Some(row) = target_row {
                                let viewport_height =
                                    layout.content_rect.height.saturating_sub(1) as usize;
                                let context_above = viewport_height / 3;
                                view_state.cursor_row = row;
                                view_state.scroll_row = row.saturating_sub(context_above);
                            }
                        }

                        // Render composite buffer with side-by-side panes
                        Self::render_composite_buffer(
                            frame,
                            layout.content_rect,
                            composite,
                            buffers,
                            theme,
                            is_active,
                            view_state,
                            use_terminal_bg,
                            split_show_tilde,
                        );

                        // Render scrollbar for composite buffer
                        let total_rows = composite.row_count();
                        let content_height = layout.content_rect.height.saturating_sub(1) as usize; // -1 for header
                        let (thumb_start, thumb_end) =
                            if show_vertical_scrollbar && !is_non_scrollable {
                                render_composite_scrollbar(
                                    frame,
                                    layout.scrollbar_rect,
                                    total_rows,
                                    view_state.scroll_row,
                                    content_height,
                                    is_active,
                                )
                            } else {
                                (0, 0)
                            };

                        // Store the areas for mouse handling
                        split_areas.push((
                            split_id,
                            buffer_id,
                            layout.content_rect,
                            layout.scrollbar_rect,
                            thumb_start,
                            thumb_end,
                        ));
                        if show_horizontal_scrollbar {
                            horizontal_scrollbar_areas.push((
                                split_id,
                                buffer_id,
                                layout.horizontal_scrollbar_rect,
                                0, // composite buffers don't horizontal-scroll
                                0,
                                0,
                            ));
                        }
                    }
                    view_line_mappings.insert(split_id, Vec::new());
                    continue;
                }

                // Get viewport from SplitViewState (authoritative source)
                // We need to get it mutably for sync operations
                // Use as_deref() to get Option<&HashMap> for read-only operations
                let view_state_opt = split_view_states
                    .as_deref()
                    .and_then(|vs| vs.get(&split_id));
                let viewport_clone =
                    view_state_opt
                        .map(|vs| vs.viewport.clone())
                        .unwrap_or_else(|| {
                            crate::view::viewport::Viewport::new(
                                layout.content_rect.width,
                                layout.content_rect.height,
                            )
                        });
                let mut viewport = viewport_clone;

                // Get cursors from the split's view state
                let split_cursors = split_view_states
                    .as_deref()
                    .and_then(|vs| vs.get(&split_id))
                    .map(|vs| vs.cursors.clone())
                    .unwrap_or_default();
                // Resolve hidden fold byte ranges so ensure_visible can skip
                // folded lines when counting distance to the cursor.
                let hidden_ranges: Vec<(usize, usize)> = split_view_states
                    .as_deref()
                    .and_then(|vs| vs.get(&split_id))
                    .map(|vs| {
                        vs.folds
                            .resolved_ranges(&state.buffer, &state.marker_list)
                            .into_iter()
                            .map(|r| (r.start_byte, r.end_byte))
                            .collect()
                    })
                    .unwrap_or_default();

                {
                    let _span = tracing::trace_span!("sync_viewport_to_content").entered();
                    sync_viewport_to_content(
                        &mut viewport,
                        &mut state.buffer,
                        &split_cursors,
                        layout.content_rect,
                        &hidden_ranges,
                    );
                }
                let view_prefs =
                    resolve_view_preferences(state, split_view_states.as_deref(), split_id);

                // When cursors are hidden, also suppress current-line highlighting
                // and selection rendering so the buffer appears fully non-interactive.
                let effective_highlight_current_line =
                    view_prefs.highlight_current_line && state.show_cursors;

                let mut empty_folds = FoldManager::new();
                let folds = split_view_states
                    .as_deref_mut()
                    .and_then(|vs| vs.get_mut(&split_id))
                    .map(|vs| &mut vs.folds)
                    .unwrap_or(&mut empty_folds);

                let _render_buf_span = tracing::trace_span!("render_buffer_in_split").entered();
                let split_view_mappings = Self::render_buffer_in_split(
                    frame,
                    state,
                    &split_cursors,
                    &mut viewport,
                    folds,
                    event_log_opt,
                    layout.content_rect,
                    is_active,
                    theme,
                    ansi_background,
                    background_fade,
                    lsp_waiting,
                    view_prefs.view_mode,
                    view_prefs.compose_width,
                    view_prefs.compose_column_guides,
                    view_prefs.view_transform,
                    estimated_line_length,
                    highlight_context_bytes,
                    buffer_id,
                    hide_cursor,
                    relative_line_numbers,
                    use_terminal_bg,
                    session_mode,
                    software_cursor_only,
                    &view_prefs.rulers,
                    view_prefs.show_line_numbers,
                    effective_highlight_current_line,
                    diagnostics_inline_text,
                    split_show_tilde,
                    cell_theme_map,
                    screen_width,
                );

                drop(_render_buf_span);

                // Store view line mappings for mouse click handling
                view_line_mappings.insert(split_id, split_view_mappings);

                // For small files, count actual lines for accurate scrollbar
                // For large files, we'll use a constant thumb size
                let buffer_len = state.buffer.len();
                let (total_lines, top_line) = {
                    let _span = tracing::trace_span!("scrollbar_line_counts").entered();
                    scrollbar_line_counts(
                        state,
                        &viewport,
                        large_file_threshold_bytes,
                        buffer_len,
                    )
                };

                // Render vertical scrollbar for this split and get thumb position
                let (thumb_start, thumb_end) = if show_vertical_scrollbar && !is_non_scrollable {
                    render_scrollbar(
                        frame,
                        state,
                        &viewport,
                        layout.scrollbar_rect,
                        is_active,
                        theme,
                        large_file_threshold_bytes,
                        total_lines,
                        top_line,
                    )
                } else {
                    (0, 0)
                };

                // Compute the actual max line length for horizontal scrollbar
                let max_content_width = if show_horizontal_scrollbar && !viewport.line_wrap_enabled
                {
                    let mcw = compute_max_line_length(state, &mut viewport);
                    // Clamp left_column so content can't scroll past the end of the longest line
                    let visible_width = viewport.width as usize;
                    let max_scroll = mcw.saturating_sub(visible_width);
                    if viewport.left_column > max_scroll {
                        viewport.left_column = max_scroll;
                    }
                    mcw
                } else {
                    0
                };

                // Render horizontal scrollbar for this split
                let (hthumb_start, hthumb_end) = if show_horizontal_scrollbar {
                    render_horizontal_scrollbar(
                        frame,
                        &viewport,
                        layout.horizontal_scrollbar_rect,
                        is_active,
                        max_content_width,
                    )
                } else {
                    (0, 0)
                };

                // Write back updated viewport to SplitViewState
                // This is crucial for cursor visibility tracking (ensure_visible_in_layout updates)
                // NOTE: We do NOT clear skip_ensure_visible here - it should persist across
                // renders until something actually needs cursor visibility check
                if let Some(view_states) = split_view_states.as_deref_mut() {
                    if let Some(view_state) = view_states.get_mut(&split_id) {
                        tracing::trace!(
                            "Writing back viewport: top_byte={}, skip_ensure_visible={}",
                            viewport.top_byte,
                            viewport.should_skip_ensure_visible()
                        );
                        view_state.viewport = viewport.clone();
                    }
                }

                // Store the areas for mouse handling
                split_areas.push((
                    split_id,
                    buffer_id,
                    layout.content_rect,
                    layout.scrollbar_rect,
                    thumb_start,
                    thumb_end,
                ));
                if show_horizontal_scrollbar {
                    horizontal_scrollbar_areas.push((
                        split_id,
                        buffer_id,
                        layout.horizontal_scrollbar_rect,
                        max_content_width,
                        hthumb_start,
                        hthumb_end,
                    ));
                }
            }
        }

        // Render split separators — for both the main tree and any
        // active Grouped subtrees dispatched at render time.
        let separators = split_manager.get_separators(area);
        for (direction, x, y, length) in separators {
            render_separator(frame, direction, x, y, length, theme);
        }
        // Walk base_visible again to render internal separators of active
        // groups (the group's Split nodes live in the side-map, not in the
        // main split tree, so split_manager doesn't know about them).
        // Collect these separators with their container IDs so the hit-test
        // path in `app::render` can wire up dragging.
        let mut grouped_separator_areas: Vec<(
            crate::model::event::ContainerId,
            SplitDirection,
            u16,
            u16,
            u16,
        )> = Vec::new();
        for (main_split_id, _main_buffer_id, split_area) in &base_visible {
            let active_group = split_view_states
                .as_deref()
                .and_then(|svs| svs.get(main_split_id))
                .and_then(|vs| vs.active_group_tab);
            if let Some(group_leaf) = active_group {
                if let Some(grouped) = grouped_subtrees.get(&group_leaf) {
                    let split_tab_bar_visible = tab_bar_visible
                        && !split_view_states
                            .as_deref()
                            .and_then(|svs| svs.get(main_split_id))
                            .is_some_and(|vs| vs.suppress_chrome);
                    let main_layout = split_layout(
                        *split_area,
                        split_tab_bar_visible,
                        show_vertical_scrollbar,
                        show_horizontal_scrollbar,
                    );
                    if let crate::view::split::SplitNode::Grouped { layout, .. } = grouped {
                        for (id, direction, x, y, length) in
                            layout.get_separators_with_ids(main_layout.content_rect)
                        {
                            render_separator(frame, direction, x, y, length, theme);
                            grouped_separator_areas.push((id, direction, x, y, length));
                        }
                    }
                }
            }
        }

        (
            split_areas,
            tab_layouts,
            close_split_areas,
            maximize_split_areas,
            view_line_mappings,
            horizontal_scrollbar_areas,
            grouped_separator_areas,
        )
    }

    /// Layout-only path: computes view_line_mappings for all visible splits
    /// without drawing anything. Used by macro replay to keep the cached layout
    /// fresh between actions without paying the cost of full rendering.
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
        let visible_buffers = split_manager.get_visible_buffers(area);
        let active_split_id = split_manager.active_split();
        let mut view_line_mappings: HashMap<LeafId, Vec<ViewLineMapping>> = HashMap::new();

        for (split_id, buffer_id, split_area) in visible_buffers {
            let is_active = split_id == active_split_id;

            // Suppress chrome (tab bar) for splits in buffer groups
            let split_tab_bar_visible = tab_bar_visible
                && !split_view_states
                    .get(&split_id)
                    .map_or(false, |vs| vs.suppress_chrome);

            let layout = split_layout(
                split_area,
                split_tab_bar_visible,
                show_vertical_scrollbar,
                show_horizontal_scrollbar,
            );

            let state = match buffers.get_mut(&buffer_id) {
                Some(s) => s,
                None => continue,
            };

            // Skip composite buffers — they don't produce view_line_mappings
            if state.is_composite_buffer {
                view_line_mappings.insert(split_id, Vec::new());
                continue;
            }

            // Get viewport from SplitViewState (authoritative source)
            let viewport_clone = split_view_states
                .get(&split_id)
                .map(|vs| vs.viewport.clone())
                .unwrap_or_else(|| {
                    crate::view::viewport::Viewport::new(
                        layout.content_rect.width,
                        layout.content_rect.height,
                    )
                });
            let mut viewport = viewport_clone;

            // Get cursors from the split's view state
            let split_cursors = split_view_states
                .get(&split_id)
                .map(|vs| vs.cursors.clone())
                .unwrap_or_default();
            // Resolve hidden fold byte ranges so ensure_visible can skip
            // folded lines when counting distance to the cursor.
            let hidden_ranges: Vec<(usize, usize)> = split_view_states
                .get(&split_id)
                .map(|vs| {
                    vs.folds
                        .resolved_ranges(&state.buffer, &state.marker_list)
                        .into_iter()
                        .map(|r| (r.start_byte, r.end_byte))
                        .collect()
                })
                .unwrap_or_default();

            sync_viewport_to_content(
                &mut viewport,
                &mut state.buffer,
                &split_cursors,
                layout.content_rect,
                &hidden_ranges,
            );
            let view_prefs =
                resolve_view_preferences(state, Some(&*split_view_states), split_id);

            let effective_highlight_current_line =
                view_prefs.highlight_current_line && state.show_cursors;

            let mut empty_folds = FoldManager::new();
            let folds = split_view_states
                .get_mut(&split_id)
                .map(|vs| &mut vs.folds)
                .unwrap_or(&mut empty_folds);

            let layout_output = Self::compute_buffer_layout(
                state,
                &split_cursors,
                &mut viewport,
                folds,
                layout.content_rect,
                is_active,
                theme,
                lsp_waiting,
                view_prefs.view_mode,
                view_prefs.compose_width,
                view_prefs.view_transform,
                estimated_line_length,
                highlight_context_bytes,
                relative_line_numbers,
                use_terminal_bg,
                session_mode,
                software_cursor_only,
                view_prefs.show_line_numbers,
                effective_highlight_current_line,
                diagnostics_inline_text,
                show_tilde,
                None, // No cell theme map for layout-only computation
            );

            view_line_mappings.insert(split_id, layout_output.view_line_mappings);

            // Write back updated viewport to SplitViewState
            if let Some(view_state) = split_view_states.get_mut(&split_id) {
                view_state.viewport = viewport;
            }
        }

        view_line_mappings
    }


    /// Render a composite buffer (side-by-side view of multiple source buffers)
    /// Uses ViewLines for proper syntax highlighting, ANSI handling, etc.
    #[allow(clippy::too_many_arguments)]
    fn render_composite_buffer(
        frame: &mut Frame,
        area: Rect,
        composite: &crate::model::composite_buffer::CompositeBuffer,
        buffers: &mut HashMap<BufferId, EditorState>,
        theme: &crate::view::theme::Theme,
        _is_active: bool,
        view_state: &mut crate::view::composite_view::CompositeViewState,
        use_terminal_bg: bool,
        show_tilde: bool,
    ) {
        use crate::model::composite_buffer::{CompositeLayout, RowType};

        // Compute effective editor background: terminal default or theme-defined
        let effective_editor_bg = if use_terminal_bg {
            ratatui::style::Color::Reset
        } else {
            theme.editor_bg
        };

        let scroll_row = view_state.scroll_row;
        let cursor_row = view_state.cursor_row;

        // Clear the area first
        frame.render_widget(Clear, area);

        // Calculate pane widths based on layout
        let pane_count = composite.sources.len();
        if pane_count == 0 {
            return;
        }

        // Extract show_separator from layout
        let show_separator = match &composite.layout {
            CompositeLayout::SideBySide { show_separator, .. } => *show_separator,
            _ => false,
        };

        // Calculate pane areas
        let separator_width = if show_separator { 1 } else { 0 };
        let total_separators = (pane_count.saturating_sub(1)) as u16 * separator_width;
        let available_width = area.width.saturating_sub(total_separators);

        let pane_widths: Vec<u16> = match &composite.layout {
            CompositeLayout::SideBySide { ratios, .. } => {
                let default_ratio = 1.0 / pane_count as f32;
                ratios
                    .iter()
                    .chain(std::iter::repeat(&default_ratio))
                    .take(pane_count)
                    .map(|r| (available_width as f32 * r).round() as u16)
                    .collect()
            }
            _ => {
                // Equal widths for stacked/unified layouts
                let pane_width = available_width / pane_count as u16;
                vec![pane_width; pane_count]
            }
        };

        // Store computed pane widths in view state for cursor movement calculations
        view_state.pane_widths = pane_widths.clone();

        // Render headers first
        let header_height = 1u16;
        let mut x_offset = area.x;
        for (idx, (source, &width)) in composite.sources.iter().zip(&pane_widths).enumerate() {
            let header_area = Rect::new(x_offset, area.y, width, header_height);
            let is_focused = idx == view_state.focused_pane;

            let header_style = if is_focused {
                Style::default()
                    .fg(theme.tab_active_fg)
                    .bg(theme.tab_active_bg)
            } else {
                Style::default()
                    .fg(theme.tab_inactive_fg)
                    .bg(theme.tab_inactive_bg)
            };

            let header_text = format!(" {} ", source.label);
            let header = Paragraph::new(header_text).style(header_style);
            frame.render_widget(header, header_area);

            x_offset += width + separator_width;
        }

        // Content area (below headers)
        let content_y = area.y + header_height;
        let content_height = area.height.saturating_sub(header_height);
        let visible_rows = content_height as usize;

        // Render aligned rows
        let alignment = &composite.alignment;
        let total_rows = alignment.rows.len();

        // Build ViewData and get syntax highlighting for each pane
        // Store: (ViewLines, line->ViewLine mapping, highlight spans)
        struct PaneRenderData {
            lines: Vec<ViewLine>,
            line_to_view_line: HashMap<usize, usize>,
            highlight_spans: Vec<crate::primitives::highlighter::HighlightSpan>,
        }

        let mut pane_render_data: Vec<Option<PaneRenderData>> = Vec::new();

        for (pane_idx, source) in composite.sources.iter().enumerate() {
            if let Some(source_state) = buffers.get_mut(&source.buffer_id) {
                // Find the first and last source lines we need for this pane
                let visible_lines: Vec<usize> = alignment
                    .rows
                    .iter()
                    .skip(scroll_row)
                    .take(visible_rows)
                    .filter_map(|row| row.get_pane_line(pane_idx))
                    .map(|r| r.line)
                    .collect();

                let first_line = visible_lines.iter().copied().min();
                let last_line = visible_lines.iter().copied().max();

                if let (Some(first_line), Some(last_line)) = (first_line, last_line) {
                    // Get byte range for highlighting
                    let top_byte = source_state
                        .buffer
                        .line_start_offset(first_line)
                        .unwrap_or(0);
                    let end_byte = source_state
                        .buffer
                        .line_start_offset(last_line + 1)
                        .unwrap_or(source_state.buffer.len());

                    // Get syntax highlighting spans from the highlighter
                    let highlight_spans = source_state.highlighter.highlight_viewport(
                        &source_state.buffer,
                        top_byte,
                        end_byte,
                        theme,
                        1024, // highlight_context_bytes
                    );

                    // Create a temporary viewport for building view data
                    let pane_width = pane_widths.get(pane_idx).copied().unwrap_or(80);
                    let mut viewport =
                        crate::view::viewport::Viewport::new(pane_width, content_height);
                    viewport.top_byte = top_byte;
                    viewport.line_wrap_enabled = false;

                    let pane_width = pane_widths.get(pane_idx).copied().unwrap_or(80) as usize;
                    let gutter_width = 4; // Line number width
                    let content_width = pane_width.saturating_sub(gutter_width);

                    // Build ViewData for this pane
                    // Need enough lines to cover from first_line to last_line
                    let lines_needed = last_line - first_line + 10;
                    let empty_folds = FoldManager::new();
                    let view_data = build_view_data(
                        source_state,
                        &viewport,
                        None,         // No view transform
                        80,           // estimated_line_length
                        lines_needed, // visible_count - enough to cover the range
                        false,        // line_wrap_enabled
                        content_width,
                        gutter_width,
                        &ViewMode::Source, // Composite view uses source mode
                        &empty_folds,
                        theme,
                    );

                    // Build source_line -> ViewLine index mapping
                    let mut line_to_view_line: HashMap<usize, usize> = HashMap::new();
                    let mut current_line = first_line;
                    for (idx, view_line) in view_data.lines.iter().enumerate() {
                        if should_show_line_number(view_line) {
                            line_to_view_line.insert(current_line, idx);
                            current_line += 1;
                        }
                    }

                    pane_render_data.push(Some(PaneRenderData {
                        lines: view_data.lines,
                        line_to_view_line,
                        highlight_spans,
                    }));
                } else {
                    pane_render_data.push(None);
                }
            } else {
                pane_render_data.push(None);
            }
        }

        // Now render aligned rows using ViewLines
        for view_row in 0..visible_rows {
            let display_row = scroll_row + view_row;
            if display_row >= total_rows {
                if show_tilde {
                    // Fill with tildes for empty rows
                    let mut x = area.x;
                    for &width in &pane_widths {
                        let tilde_area = Rect::new(x, content_y + view_row as u16, width, 1);
                        let tilde =
                            Paragraph::new("~").style(Style::default().fg(theme.line_number_fg));
                        frame.render_widget(tilde, tilde_area);
                        x += width + separator_width;
                    }
                }
                continue;
            }

            let aligned_row = &alignment.rows[display_row];
            let is_cursor_row = display_row == cursor_row;
            // Get selection column range for this row (if any)
            let selection_cols = view_state.selection_column_range(display_row);

            // Determine row background based on type (selection is now character-level)
            let row_bg = match aligned_row.row_type {
                RowType::Addition => Some(theme.diff_add_bg),
                RowType::Deletion => Some(theme.diff_remove_bg),
                RowType::Modification => Some(theme.diff_modify_bg),
                RowType::HunkHeader => Some(theme.current_line_bg),
                RowType::Context => None,
            };

            // Compute inline diff for modified rows (to highlight changed words/characters)
            let inline_diffs: Vec<Vec<Range<usize>>> = if aligned_row.row_type
                == RowType::Modification
            {
                // Get line content from both panes
                let mut line_contents: Vec<Option<String>> = Vec::new();
                for (pane_idx, source) in composite.sources.iter().enumerate() {
                    if let Some(line_ref) = aligned_row.get_pane_line(pane_idx) {
                        if let Some(source_state) = buffers.get(&source.buffer_id) {
                            line_contents.push(
                                source_state
                                    .buffer
                                    .get_line(line_ref.line)
                                    .map(|line| String::from_utf8_lossy(&line).to_string()),
                            );
                        } else {
                            line_contents.push(None);
                        }
                    } else {
                        line_contents.push(None);
                    }
                }

                // Compute inline diff between panes (typically old vs new)
                if line_contents.len() >= 2 {
                    if let (Some(old_text), Some(new_text)) = (&line_contents[0], &line_contents[1])
                    {
                        let (old_ranges, new_ranges) = compute_inline_diff(old_text, new_text);
                        vec![old_ranges, new_ranges]
                    } else {
                        vec![Vec::new(); composite.sources.len()]
                    }
                } else {
                    vec![Vec::new(); composite.sources.len()]
                }
            } else {
                // For non-modification rows, no inline highlighting
                vec![Vec::new(); composite.sources.len()]
            };

            // Render each pane for this row
            let mut x_offset = area.x;
            for (pane_idx, (_source, &width)) in
                composite.sources.iter().zip(&pane_widths).enumerate()
            {
                let pane_area = Rect::new(x_offset, content_y + view_row as u16, width, 1);

                // Get horizontal scroll offset for this pane
                let left_column = view_state
                    .get_pane_viewport(pane_idx)
                    .map(|v| v.left_column)
                    .unwrap_or(0);

                // Get source line for this pane
                let source_line_opt = aligned_row.get_pane_line(pane_idx);

                if let Some(source_line_ref) = source_line_opt {
                    // Try to get ViewLine and highlight spans from pre-built data
                    let pane_data = pane_render_data.get(pane_idx).and_then(|opt| opt.as_ref());
                    let view_line_opt = pane_data.and_then(|data| {
                        data.line_to_view_line
                            .get(&source_line_ref.line)
                            .and_then(|&idx| data.lines.get(idx))
                    });
                    let highlight_spans = pane_data
                        .map(|data| data.highlight_spans.as_slice())
                        .unwrap_or(&[]);

                    let gutter_width = 4usize;
                    let max_content_width = width.saturating_sub(gutter_width as u16) as usize;

                    let is_focused_pane = pane_idx == view_state.focused_pane;

                    // Determine background - cursor row highlight only on focused pane
                    // Selection is now character-level, handled in render_view_line_content
                    let bg = if is_cursor_row && is_focused_pane {
                        theme.current_line_bg
                    } else {
                        row_bg.unwrap_or(effective_editor_bg)
                    };

                    // Selection range for this row (only for focused pane)
                    let pane_selection_cols = if is_focused_pane {
                        selection_cols
                    } else {
                        None
                    };

                    // Line number
                    let line_num = format!("{:>3} ", source_line_ref.line + 1);
                    let line_num_style = Style::default().fg(theme.line_number_fg).bg(bg);

                    let is_cursor_pane = is_focused_pane;
                    let cursor_column = view_state.cursor_column;

                    // Get inline diff ranges for this pane
                    let inline_ranges = inline_diffs.get(pane_idx).cloned().unwrap_or_default();

                    // Determine highlight color for changed portions (brighter than line bg)
                    let highlight_bg = match aligned_row.row_type {
                        RowType::Deletion => Some(theme.diff_remove_highlight_bg),
                        RowType::Addition => Some(theme.diff_add_highlight_bg),
                        RowType::Modification => {
                            if pane_idx == 0 {
                                Some(theme.diff_remove_highlight_bg)
                            } else {
                                Some(theme.diff_add_highlight_bg)
                            }
                        }
                        _ => None,
                    };

                    // Build spans using ViewLine if available (for syntax highlighting)
                    let mut spans = vec![Span::styled(line_num, line_num_style)];

                    if let Some(view_line) = view_line_opt {
                        // Use ViewLine for syntax-highlighted content
                        Self::render_view_line_content(
                            &mut spans,
                            view_line,
                            highlight_spans,
                            left_column,
                            max_content_width,
                            bg,
                            theme,
                            is_cursor_row && is_cursor_pane,
                            cursor_column,
                            &inline_ranges,
                            highlight_bg,
                            pane_selection_cols,
                        );
                    } else {
                        // This branch should be unreachable:
                        // - visible_lines is collected from the same range we iterate over
                        // - If source_line_ref exists, that line was in visible_lines
                        // - So pane_render_data exists and the line should be in the mapping
                        // - With line_wrap disabled, each source line = one ViewLine
                        tracing::warn!(
                            "ViewLine missing for composite buffer: pane={}, line={}, pane_data={}",
                            pane_idx,
                            source_line_ref.line,
                            pane_data.is_some()
                        );
                        // Graceful degradation: render empty content with background
                        let base_style = Style::default().fg(theme.editor_fg).bg(bg);
                        let padding = " ".repeat(max_content_width);
                        spans.push(Span::styled(padding, base_style));
                    }

                    let line = Line::from(spans);
                    let para = Paragraph::new(line);
                    frame.render_widget(para, pane_area);
                } else {
                    // No content for this pane (padding/gap line)
                    let is_focused_pane = pane_idx == view_state.focused_pane;
                    // For empty lines in focused pane, show selection if entire line is selected
                    let pane_has_selection = is_focused_pane
                        && selection_cols
                            .map(|(start, end)| start == 0 && end == usize::MAX)
                            .unwrap_or(false);

                    let bg = if pane_has_selection {
                        theme.selection_bg
                    } else if is_cursor_row && is_focused_pane {
                        theme.current_line_bg
                    } else {
                        row_bg.unwrap_or(effective_editor_bg)
                    };
                    let style = Style::default().fg(theme.line_number_fg).bg(bg);

                    // Check if cursor should be shown on this empty line
                    let is_cursor_pane = pane_idx == view_state.focused_pane;
                    if is_cursor_row && is_cursor_pane && view_state.cursor_column == 0 {
                        // Show cursor on empty line
                        let cursor_style = Style::default().fg(theme.editor_bg).bg(theme.editor_fg);
                        let gutter_width = 4usize;
                        let max_content_width = width.saturating_sub(gutter_width as u16) as usize;
                        let padding = " ".repeat(max_content_width.saturating_sub(1));
                        let line = Line::from(vec![
                            Span::styled("    ", style),
                            Span::styled(" ", cursor_style),
                            Span::styled(padding, Style::default().bg(bg)),
                        ]);
                        let para = Paragraph::new(line);
                        frame.render_widget(para, pane_area);
                    } else {
                        // Empty gap line with diff background
                        let gap_style = Style::default().bg(bg);
                        let empty_content = " ".repeat(width as usize);
                        let para = Paragraph::new(empty_content).style(gap_style);
                        frame.render_widget(para, pane_area);
                    }
                }

                x_offset += width;

                // Render separator
                if show_separator && pane_idx < pane_count - 1 {
                    let sep_area =
                        Rect::new(x_offset, content_y + view_row as u16, separator_width, 1);
                    let sep =
                        Paragraph::new("│").style(Style::default().fg(theme.split_separator_fg));
                    frame.render_widget(sep, sep_area);
                    x_offset += separator_width;
                }
            }
        }
    }

    /// Render ViewLine content with syntax highlighting to spans
    #[allow(clippy::too_many_arguments)]
    fn render_view_line_content(
        spans: &mut Vec<Span<'static>>,
        view_line: &ViewLine,
        highlight_spans: &[crate::primitives::highlighter::HighlightSpan],
        left_column: usize,
        max_width: usize,
        bg: Color,
        theme: &crate::view::theme::Theme,
        show_cursor: bool,
        cursor_column: usize,
        inline_ranges: &[Range<usize>],
        highlight_bg: Option<Color>,
        selection_cols: Option<(usize, usize)>, // (start_col, end_col) for selection
    ) {
        let text = &view_line.text;
        let char_source_bytes = &view_line.char_source_bytes;

        // Apply horizontal scroll and collect visible characters with styles
        let chars: Vec<char> = text.chars().collect();
        let mut col = 0usize;
        let mut rendered = 0usize;
        let mut current_span_text = String::new();
        let mut current_style: Option<Style> = None;
        let mut hl_cursor = 0usize;

        for (char_idx, ch) in chars.iter().enumerate() {
            let char_width = char_width(*ch);

            // Skip characters before left_column
            if col < left_column {
                col += char_width;
                continue;
            }

            // Stop if we've rendered enough
            if rendered >= max_width {
                break;
            }

            // Get source byte position for this character
            let byte_pos = char_source_bytes.get(char_idx).and_then(|b| *b);

            // Get syntax highlight color via cursor-based O(1) lookup
            let highlight_color =
                byte_pos.and_then(|bp| span_color_at(highlight_spans, &mut hl_cursor, bp));

            // Check if this character is in an inline diff range
            let in_inline_range = inline_ranges.iter().any(|r| r.contains(&char_idx));

            // Check if this character is in selection range
            let in_selection = selection_cols
                .map(|(start, end)| col >= start && col < end)
                .unwrap_or(false);

            // Determine background: selection > inline diff > normal
            let char_bg = if in_selection {
                theme.selection_bg
            } else if in_inline_range {
                highlight_bg.unwrap_or(bg)
            } else {
                bg
            };

            // Build character style
            let char_style = if let Some(color) = highlight_color {
                Style::default().fg(color).bg(char_bg)
            } else {
                Style::default().fg(theme.editor_fg).bg(char_bg)
            };

            // Handle cursor - cursor_column is absolute position, compare directly with col
            let final_style = if show_cursor && col == cursor_column {
                // Invert colors for cursor
                Style::default().fg(theme.editor_bg).bg(theme.editor_fg)
            } else {
                char_style
            };

            // Accumulate or flush spans based on style changes
            if let Some(style) = current_style {
                if style != final_style && !current_span_text.is_empty() {
                    spans.push(Span::styled(std::mem::take(&mut current_span_text), style));
                }
            }

            current_style = Some(final_style);
            current_span_text.push(*ch);
            col += char_width;
            rendered += char_width;
        }

        // Flush remaining span
        if !current_span_text.is_empty() {
            if let Some(style) = current_style {
                spans.push(Span::styled(current_span_text, style));
            }
        }

        // Pad to fill width
        if rendered < max_width {
            let padding_len = max_width - rendered;
            // cursor_column is absolute, convert to visual position for padding check
            let cursor_visual = cursor_column.saturating_sub(left_column);

            // Check if cursor is in the padding area (past end of line content)
            if show_cursor && cursor_visual >= rendered && cursor_visual < max_width {
                // Cursor is in padding area - render cursor as single char
                let cursor_offset = cursor_visual - rendered;
                let cursor_style = Style::default().fg(theme.editor_bg).bg(theme.editor_fg);
                let normal_style = Style::default().bg(bg);

                // Pre-cursor padding (if cursor is not at start of padding)
                if cursor_offset > 0 {
                    spans.push(Span::styled(" ".repeat(cursor_offset), normal_style));
                }
                // Single-char cursor
                spans.push(Span::styled(" ", cursor_style));
                // Post-cursor padding
                let remaining = padding_len.saturating_sub(cursor_offset + 1);
                if remaining > 0 {
                    spans.push(Span::styled(" ".repeat(remaining), normal_style));
                }
            } else {
                // No cursor in padding - just fill with background
                spans.push(Span::styled(
                    " ".repeat(padding_len),
                    Style::default().bg(bg),
                ));
            }
        }
    }







    /// Public wrapper for building base tokens - used by render.rs for the view_transform_request hook
    pub fn build_base_tokens_for_hook(
        buffer: &mut Buffer,
        top_byte: usize,
        estimated_line_length: usize,
        visible_count: usize,
        is_binary: bool,
        line_ending: crate::model::buffer::LineEnding,
    ) -> Vec<fresh_core::api::ViewTokenWire> {
        build_base_tokens(
            buffer,
            top_byte,
            estimated_line_length,
            visible_count,
            is_binary,
            line_ending,
        )
    }




    fn selection_context(
        state: &EditorState,
        cursors: &crate::model::cursor::Cursors,
    ) -> SelectionContext {
        // When cursors are hidden, suppress all visual selection feedback
        // (no selection highlight, no block rects, no cursor positions)
        if !state.show_cursors {
            return SelectionContext {
                ranges: Vec::new(),
                block_rects: Vec::new(),
                cursor_positions: Vec::new(),
                primary_cursor_position: cursors.primary().position,
            };
        }

        let ranges: Vec<Range<usize>> = cursors
            .iter()
            .filter_map(|(_, cursor)| {
                // Don't include normal selection for cursors in block selection mode
                // Block selections are rendered separately via block_rects
                if cursor.selection_mode == SelectionMode::Block {
                    None
                } else {
                    cursor.selection_range()
                }
            })
            .collect();

        let block_rects: Vec<(usize, usize, usize, usize)> = cursors
            .iter()
            .filter_map(|(_, cursor)| {
                if cursor.selection_mode == SelectionMode::Block {
                    if let Some(anchor) = cursor.block_anchor {
                        // Convert cursor position to 2D coords
                        let cur_line = state.buffer.get_line_number(cursor.position);
                        let cur_line_start = state.buffer.line_start_offset(cur_line).unwrap_or(0);
                        let cur_col = cursor.position.saturating_sub(cur_line_start);

                        // Return normalized rectangle (min values first)
                        Some((
                            anchor.line.min(cur_line),
                            anchor.column.min(cur_col),
                            anchor.line.max(cur_line),
                            anchor.column.max(cur_col),
                        ))
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect();

        let cursor_positions: Vec<usize> =
            cursors.iter().map(|(_, cursor)| cursor.position).collect();

        SelectionContext {
            ranges,
            block_rects,
            cursor_positions,
            primary_cursor_position: cursors.primary().position,
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn decoration_context(
        state: &mut EditorState,
        viewport_start: usize,
        viewport_end: usize,
        primary_cursor_position: usize,
        folds: &FoldManager,
        theme: &crate::view::theme::Theme,
        highlight_context_bytes: usize,
        view_mode: &ViewMode,
        diagnostics_inline_text: bool,
        view_lines: &[ViewLine],
    ) -> DecorationContext {
        use crate::view::folding::indent_folding;

        // Extend highlighting range by ~1 viewport size before/after for better context.
        // This helps tree-sitter parse multi-line constructs that span viewport boundaries.
        let viewport_size = viewport_end.saturating_sub(viewport_start);
        let highlight_start = viewport_start.saturating_sub(viewport_size);
        let highlight_end = viewport_end
            .saturating_add(viewport_size)
            .min(state.buffer.len());

        let highlight_spans = state.highlighter.highlight_viewport(
            &state.buffer,
            highlight_start,
            highlight_end,
            theme,
            highlight_context_bytes,
        );

        // Update reference highlight overlays (debounced, creates overlays that auto-adjust)
        state.reference_highlight_overlay.update(
            &state.buffer,
            &mut state.overlays,
            &mut state.marker_list,
            &mut state.reference_highlighter,
            primary_cursor_position,
            viewport_start,
            viewport_end,
            highlight_context_bytes,
            theme.semantic_highlight_bg,
        );

        // Update bracket highlight overlays
        state.bracket_highlight_overlay.update(
            &state.buffer,
            &mut state.overlays,
            &mut state.marker_list,
            primary_cursor_position,
        );

        // Semantic tokens are stored as overlays so their ranges track edits.
        // Convert them into highlight spans for the render pipeline.
        let is_compose = matches!(view_mode, ViewMode::PageView);
        let md_emphasis_ns =
            fresh_core::overlay::OverlayNamespace::from_string("md-emphasis".to_string());
        let mut semantic_token_spans = Vec::new();
        let mut viewport_overlays = Vec::new();
        for (overlay, range) in
            state
                .overlays
                .query_viewport(viewport_start, viewport_end, &state.marker_list)
        {
            if crate::services::lsp::semantic_tokens::is_semantic_token_overlay(overlay) {
                if let crate::view::overlay::OverlayFace::Foreground { color } = &overlay.face {
                    semantic_token_spans.push(crate::primitives::highlighter::HighlightSpan {
                        range,
                        color: *color,
                        category: None,
                    });
                }
                continue;
            }

            // Skip markdown compose overlays in Source mode — they should only
            // render in the Compose-mode split.
            if !is_compose && overlay.namespace.as_ref() == Some(&md_emphasis_ns) {
                continue;
            }

            viewport_overlays.push((overlay.clone(), range));
        }

        // Sort overlays by priority (ascending) so higher priority overlays
        // are applied last in the rendering loop and their styles take effect.
        // This ensures e.g. an error overlay (priority 100) renders its background
        // on top of a hint overlay (priority 10) at the same range.
        viewport_overlays.sort_by_key(|(overlay, _)| overlay.priority);

        // Use the lsp-diagnostic namespace to identify diagnostic overlays
        // Key by line-start byte so lookups match line_start_byte in render loop
        let diagnostic_ns = crate::services::lsp::diagnostics::lsp_diagnostic_namespace();
        let diagnostic_lines: HashSet<usize> = viewport_overlays
            .iter()
            .filter_map(|(overlay, range)| {
                if overlay.namespace.as_ref() == Some(&diagnostic_ns) {
                    return Some(indent_folding::find_line_start_byte(
                        &state.buffer,
                        range.start,
                    ));
                }
                None
            })
            .collect();

        // Build inline diagnostic text map from the same viewport overlays.
        // For each line with diagnostics, keep only the highest-priority (severity) message.
        let diagnostic_inline_texts: HashMap<usize, (String, Style)> = if diagnostics_inline_text {
            let mut by_line: HashMap<usize, (String, Style, i32)> = HashMap::new();
            for (overlay, range) in &viewport_overlays {
                if overlay.namespace.as_ref() != Some(&diagnostic_ns) {
                    continue;
                }
                if let Some(ref message) = overlay.message {
                    let line_start =
                        indent_folding::find_line_start_byte(&state.buffer, range.start);
                    let priority = overlay.priority;
                    let dominated = by_line
                        .get(&line_start)
                        .is_some_and(|(_, _, existing_pri)| *existing_pri >= priority);
                    if !dominated {
                        let style = inline_diagnostic_style(priority, theme);
                        // Take first line of multi-line messages
                        let first_line = message.lines().next().unwrap_or(message);
                        by_line.insert(line_start, (first_line.to_string(), style, priority));
                    }
                }
            }
            by_line
                .into_iter()
                .map(|(k, (msg, style, _))| (k, (msg, style)))
                .collect()
        } else {
            HashMap::new()
        };

        let virtual_text_lookup: HashMap<usize, Vec<crate::view::virtual_text::VirtualText>> =
            state
                .virtual_texts
                .build_lookup(&state.marker_list, viewport_start, viewport_end)
                .into_iter()
                .map(|(position, texts)| (position, texts.into_iter().cloned().collect()))
                .collect();

        // Pre-compute line indicators for the viewport (only query markers in visible range)
        // Key by line-start byte so lookups match line_start_byte in render loop
        let mut line_indicators = state.margins.get_indicators_for_viewport(
            viewport_start,
            viewport_end,
            |byte_offset| indent_folding::find_line_start_byte(&state.buffer, byte_offset),
        );

        // Merge native diff-since-saved indicators (cornflower blue │ for unsaved edits).
        // These have priority 5, lower than git gutter (10), so existing indicators win.
        let diff_indicators =
            diff_indicators_for_viewport(state, viewport_start, viewport_end);
        for (key, diff_ind) in diff_indicators {
            line_indicators.entry(key).or_insert(diff_ind);
        }

        let fold_indicators = fold_indicators_for_viewport(state, folds, view_lines);

        DecorationContext {
            highlight_spans,
            semantic_token_spans,
            viewport_overlays,
            virtual_text_lookup,
            diagnostic_lines,
            diagnostic_inline_texts,
            line_indicators,
            fold_indicators,
        }
    }



    // semantic token colors are mapped when overlays are created


    fn render_view_lines(input: LineRenderInput<'_>) -> LineRenderOutput {
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
                        if line_content.is_empty()
                            && _line_start_type == LineStart::AfterSourceNewline
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
                    let is_in_block_selection = block_selections.iter().any(
                        |(start_line, start_col, end_line, end_col)| {
                            gutter_num >= *start_line
                                && gutter_num <= *end_line
                                && byte_index >= *start_col
                                && byte_index <= *end_col
                        },
                    );

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
                    let (highlight_color, highlight_theme_key, highlight_display_name) =
                        match byte_pos {
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
                            let opening_tags = tracker.get_opening_tags(
                                byte_pos,
                                highlight_spans,
                                viewport_overlays,
                            );
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
                                cursor_screen_x = gutter_width as u16
                                    + col_offset.saturating_sub(left_col) as u16;
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
                        let should_add_indicator =
                            if is_active { is_secondary_cursor } else { true };
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

                let last_char_buf_pos =
                    line_char_source_bytes.get(last_char_idx).copied().flatten();
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
                    let content_width =
                        render_area.width.saturating_sub(gutter_width as u16) as usize;
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
                        let effective_diag_style = if cursor_line_active && diag_style.bg.is_none()
                        {
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
                                overlay.extend_to_line_end
                                    && range.start <= end
                                    && range.end > start
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

    fn resolve_cursor_fallback(
        current_cursor: Option<(u16, u16)>,
        primary_cursor_position: usize,
        buffer_len: usize,
        buffer_ends_with_newline: bool,
        last_line_end: Option<LastLineEnd>,
        lines_rendered: usize,
        gutter_width: usize,
    ) -> Option<(u16, u16)> {
        if current_cursor.is_some() || primary_cursor_position != buffer_len {
            return current_cursor;
        }

        if buffer_ends_with_newline {
            if let Some(end) = last_line_end {
                // When the last rendered line was the newline-terminated content
                // line, the cursor belongs on the implicit empty line one row
                // below.  But when the trailing empty line was already emitted
                // by the ViewLineIterator (terminated_with_newline == false),
                // the cursor belongs on that rendered row itself.
                let y = if end.terminated_with_newline {
                    end.pos.1.saturating_add(1)
                } else {
                    end.pos.1
                };
                return Some((gutter_width as u16, y));
            }
            return Some((gutter_width as u16, lines_rendered as u16));
        }

        last_line_end.map(|end| end.pos)
    }

    /// Pure layout computation for a buffer in a split pane.
    /// No frame/drawing involved — produces a BufferLayoutOutput that the
    /// drawing phase can consume.
    #[allow(clippy::too_many_arguments)]
    fn compute_buffer_layout(
        state: &mut EditorState,
        cursors: &crate::model::cursor::Cursors,
        viewport: &mut crate::view::viewport::Viewport,
        folds: &mut FoldManager,
        area: Rect,
        is_active: bool,
        theme: &crate::view::theme::Theme,
        lsp_waiting: bool,
        view_mode: ViewMode,
        compose_width: Option<u16>,
        view_transform: Option<ViewTransformPayload>,
        estimated_line_length: usize,
        highlight_context_bytes: usize,
        relative_line_numbers: bool,
        use_terminal_bg: bool,
        session_mode: bool,
        software_cursor_only: bool,
        show_line_numbers: bool,
        highlight_current_line: bool,
        diagnostics_inline_text: bool,
        show_tilde: bool,
        cell_theme_map: Option<(&mut Vec<crate::app::types::CellThemeInfo>, u16)>,
    ) -> BufferLayoutOutput {
        let _span = tracing::trace_span!("compute_buffer_layout").entered();

        // Configure shared margin layout for this split's line number setting.
        state.margins.configure_for_line_numbers(show_line_numbers);

        // Compute effective editor background: terminal default or theme-defined
        let effective_editor_bg = if use_terminal_bg {
            ratatui::style::Color::Reset
        } else {
            theme.editor_bg
        };

        let line_wrap = viewport.line_wrap_enabled;

        let overlay_count = state.overlays.all().len();
        if overlay_count > 0 {
            tracing::trace!("render_content: {} overlays present", overlay_count);
        }

        let visible_count = viewport.visible_line_count();

        let buffer_len = state.buffer.len();
        let byte_offset_mode = state.buffer.line_count().is_none();
        let estimated_lines = if byte_offset_mode {
            // In byte offset mode, gutter shows byte offsets, so size the gutter
            // for the largest byte offset (file size)
            buffer_len.max(1)
        } else {
            state.buffer.line_count().unwrap_or(1)
        };
        state
            .margins
            .update_width_for_buffer(estimated_lines, show_line_numbers);
        let gutter_width = state.margins.left_total_width();

        let compose_layout = calculate_compose_layout(area, &view_mode, compose_width);
        let render_area = compose_layout.render_area;

        // Clone view_transform so we can reuse it if scrolling triggers a rebuild
        let view_transform_for_rebuild = view_transform.clone();

        let view_data = {
            let _span = tracing::trace_span!("build_view_data").entered();
            build_view_data(
                state,
                viewport,
                view_transform,
                estimated_line_length,
                visible_count,
                line_wrap,
                render_area.width as usize,
                gutter_width,
                &view_mode,
                folds,
                theme,
            )
        };

        // Same-buffer scroll sync: if the sync code flagged this viewport to
        // scroll to the end, apply it now using the view lines we just built.
        let sync_scrolled = if viewport.sync_scroll_to_end {
            viewport.sync_scroll_to_end = false;
            viewport.scroll_to_end_of_view(&view_data.lines)
        } else {
            false
        };

        // If the sync adjustment changed top_byte, rebuild view_data before
        // ensure_visible_in_layout runs (so it sees the correct view lines).
        let (view_data, view_transform_for_rebuild) = if sync_scrolled {
            viewport.top_view_line_offset = 0;
            let rebuilt = build_view_data(
                state,
                viewport,
                view_transform_for_rebuild,
                estimated_line_length,
                visible_count,
                line_wrap,
                render_area.width as usize,
                gutter_width,
                &view_mode,
                folds,
                theme,
            );
            viewport.scroll_to_end_of_view(&rebuilt.lines);
            (rebuilt, None)
        } else {
            (view_data, Some(view_transform_for_rebuild))
        };

        // Ensure cursor is visible using Layout-aware check (handles virtual lines)
        let primary = *cursors.primary();
        let scrolled = viewport.ensure_visible_in_layout(&view_data.lines, &primary, gutter_width);

        // If we scrolled, rebuild view_data from the new top_byte and then re-run the
        // layout-aware check so that top_view_line_offset is correct for the rebuilt data.
        let view_data = if scrolled {
            if let Some(vt) = view_transform_for_rebuild {
                viewport.top_view_line_offset = 0;
                let rebuilt = build_view_data(
                    state,
                    viewport,
                    vt,
                    estimated_line_length,
                    visible_count,
                    line_wrap,
                    render_area.width as usize,
                    gutter_width,
                    &view_mode,
                    folds,
                    theme,
                );
                let _ = viewport.ensure_visible_in_layout(&rebuilt.lines, &primary, gutter_width);
                rebuilt
            } else {
                view_data
            }
        } else {
            view_data
        };

        let view_anchor = calculate_view_anchor(&view_data.lines, viewport.top_byte);

        let selection = Self::selection_context(state, cursors);

        tracing::trace!(
            "Rendering buffer with {} cursors at positions: {:?}, primary at {}, is_active: {}, buffer_len: {}",
            selection.cursor_positions.len(),
            selection.cursor_positions,
            selection.primary_cursor_position,
            is_active,
            state.buffer.len()
        );

        if !selection.cursor_positions.is_empty()
            && !selection
                .cursor_positions
                .contains(&selection.primary_cursor_position)
        {
            tracing::warn!(
                "Primary cursor position {} not found in cursor_positions list: {:?}",
                selection.primary_cursor_position,
                selection.cursor_positions
            );
        }

        let adjusted_visible_count = fold_adjusted_visible_count(
            &state.buffer,
            &state.marker_list,
            folds,
            viewport.top_byte,
            visible_count,
        );

        // Populate line cache to ensure chunks are loaded for rendering.
        // For small files this also builds the line index; for large files
        // it just loads the needed chunks from disk.
        let _ = state
            .buffer
            .populate_line_cache(viewport.top_byte, adjusted_visible_count);

        let viewport_start = viewport.top_byte;
        let viewport_end = calculate_viewport_end(
            state,
            viewport_start,
            estimated_line_length,
            adjusted_visible_count,
        );

        let decorations = Self::decoration_context(
            state,
            viewport_start,
            viewport_end,
            selection.primary_cursor_position,
            folds,
            theme,
            highlight_context_bytes,
            &view_mode,
            diagnostics_inline_text,
            &view_data.lines,
        );

        let calculated_offset = viewport.top_view_line_offset;

        tracing::trace!(
            top_byte = viewport.top_byte,
            top_view_line_offset = viewport.top_view_line_offset,
            calculated_offset,
            view_data_lines = view_data.lines.len(),
            "view line offset calculation"
        );
        let (view_lines_to_render, adjusted_view_anchor) =
            if calculated_offset > 0 && calculated_offset < view_data.lines.len() {
                let sliced = &view_data.lines[calculated_offset..];
                let adjusted_anchor = calculate_view_anchor(sliced, viewport.top_byte);
                (sliced, adjusted_anchor)
            } else {
                (&view_data.lines[..], view_anchor)
            };

        // Use provided cell theme map or a temporary dummy
        let mut dummy_map = Vec::new();
        let (map_ref, sw) = match cell_theme_map {
            Some((map, w)) => (map, w),
            None => (&mut dummy_map, 0u16),
        };

        let render_output = Self::render_view_lines(LineRenderInput {
            state,
            theme,
            view_lines: view_lines_to_render,
            view_anchor: adjusted_view_anchor,
            render_area,
            gutter_width,
            selection: &selection,
            decorations: &decorations,
            visible_line_count: visible_count,
            lsp_waiting,
            is_active,
            line_wrap,
            estimated_lines,
            left_column: viewport.left_column,
            relative_line_numbers,
            session_mode,
            software_cursor_only,
            show_line_numbers,
            byte_offset_mode,
            show_tilde,
            highlight_current_line,
            cell_theme_map: map_ref,
            screen_width: sw,
        });

        let view_line_mappings = render_output.view_line_mappings.clone();

        let buffer_ends_with_newline = if !state.buffer.is_empty() {
            let last_char = state.get_text_range(state.buffer.len() - 1, state.buffer.len());
            last_char == "\n"
        } else {
            false
        };

        BufferLayoutOutput {
            view_line_mappings,
            render_output,
            render_area,
            compose_layout,
            effective_editor_bg,
            view_mode,
            left_column: viewport.left_column,
            gutter_width,
            buffer_ends_with_newline,
            selection,
        }
    }

    /// Draw a buffer into a frame using pre-computed layout output.
    #[allow(clippy::too_many_arguments)]
    fn draw_buffer_in_split(
        frame: &mut Frame,
        state: &EditorState,
        cursors: &crate::model::cursor::Cursors,
        layout_output: BufferLayoutOutput,
        event_log: Option<&mut EventLog>,
        area: Rect,
        is_active: bool,
        theme: &crate::view::theme::Theme,
        ansi_background: Option<&AnsiBackground>,
        background_fade: f32,
        hide_cursor: bool,
        software_cursor_only: bool,
        rulers: &[usize],
        compose_column_guides: Option<Vec<u16>>,
    ) {
        let render_area = layout_output.render_area;
        let effective_editor_bg = layout_output.effective_editor_bg;
        let gutter_width = layout_output.gutter_width;
        let starting_line_num = 0; // used only for background offset

        render_compose_margins(
            frame,
            area,
            &layout_output.compose_layout,
            &layout_output.view_mode,
            theme,
            effective_editor_bg,
        );

        let mut lines = layout_output.render_output.lines;
        let background_x_offset = layout_output.left_column;

        if let Some(bg) = ansi_background {
            apply_background_to_lines(
                &mut lines,
                render_area.width,
                bg,
                effective_editor_bg,
                theme.editor_fg,
                background_fade,
                background_x_offset,
                starting_line_num,
            );
        }

        frame.render_widget(Clear, render_area);
        let editor_block = Block::default()
            .borders(Borders::NONE)
            .style(Style::default().bg(effective_editor_bg));
        frame.render_widget(Paragraph::new(lines).block(editor_block), render_area);

        let cursor = Self::resolve_cursor_fallback(
            layout_output.render_output.cursor,
            layout_output.selection.primary_cursor_position,
            state.buffer.len(),
            layout_output.buffer_ends_with_newline,
            layout_output.render_output.last_line_end,
            layout_output.render_output.content_lines_rendered,
            gutter_width,
        );

        let cursor_screen_pos = if is_active && state.show_cursors && !hide_cursor {
            cursor.map(|(cx, cy)| {
                let screen_x = render_area.x.saturating_add(cx);
                let max_y = render_area.height.saturating_sub(1);
                let screen_y = render_area.y.saturating_add(cy.min(max_y));
                (screen_x, screen_y)
            })
        } else {
            None
        };

        // Render config-based vertical rulers
        if !rulers.is_empty() {
            let ruler_cols: Vec<u16> = rulers.iter().map(|&r| r as u16).collect();
            render_ruler_bg(
                frame,
                &ruler_cols,
                theme.ruler_bg,
                render_area,
                gutter_width,
                layout_output.render_output.content_lines_rendered,
                layout_output.left_column,
            );
        }

        // Render compose column guides
        if let Some(guides) = compose_column_guides {
            let guide_style = Style::default()
                .fg(theme.line_number_fg)
                .add_modifier(Modifier::DIM);
            render_column_guides(
                frame,
                &guides,
                guide_style,
                render_area,
                gutter_width,
                layout_output.render_output.content_lines_rendered,
                0,
            );
        }

        if let Some((screen_x, screen_y)) = cursor_screen_pos {
            frame.set_cursor_position((screen_x, screen_y));

            // When software_cursor_only the backend has no hardware cursor, so
            // ensure the cell at the cursor position always has REVERSED style.
            // This covers all edge cases (end-of-line, empty buffer, newline
            // positions) where the per-character REVERSED styling from
            // compute_char_style may not have been applied.
            if software_cursor_only {
                let buf = frame.buffer_mut();
                let area = buf.area;
                if screen_x < area.x + area.width && screen_y < area.y + area.height {
                    let cell = &mut buf[(screen_x, screen_y)];
                    // Only override empty / default-background cells to avoid
                    // double-reversing cells that already got software cursor
                    // styling in render_view_lines.
                    if !cell.modifier.contains(Modifier::REVERSED) {
                        cell.set_char(' ');
                        cell.fg = theme.editor_fg;
                        cell.bg = theme.editor_bg;
                        cell.modifier.insert(Modifier::REVERSED);
                    }
                }
            }

            if let Some(event_log) = event_log {
                let cursor_pos = cursors.primary().position;
                let buffer_len = state.buffer.len();
                event_log.log_render_state(cursor_pos, screen_x, screen_y, buffer_len);
            }
        }
    }

    /// Render a single buffer in a split pane (convenience wrapper).
    /// Calls compute_buffer_layout then draw_buffer_in_split.
    /// Returns the view line mappings for mouse click handling.
    #[allow(clippy::too_many_arguments)]
    fn render_buffer_in_split(
        frame: &mut Frame,
        state: &mut EditorState,
        cursors: &crate::model::cursor::Cursors,
        viewport: &mut crate::view::viewport::Viewport,
        folds: &mut FoldManager,
        event_log: Option<&mut EventLog>,
        area: Rect,
        is_active: bool,
        theme: &crate::view::theme::Theme,
        ansi_background: Option<&AnsiBackground>,
        background_fade: f32,
        lsp_waiting: bool,
        view_mode: ViewMode,
        compose_width: Option<u16>,
        compose_column_guides: Option<Vec<u16>>,
        view_transform: Option<ViewTransformPayload>,
        estimated_line_length: usize,
        highlight_context_bytes: usize,
        _buffer_id: BufferId,
        hide_cursor: bool,
        relative_line_numbers: bool,
        use_terminal_bg: bool,
        session_mode: bool,
        software_cursor_only: bool,
        rulers: &[usize],
        show_line_numbers: bool,
        highlight_current_line: bool,
        diagnostics_inline_text: bool,
        show_tilde: bool,
        cell_theme_map: &mut Vec<crate::app::types::CellThemeInfo>,
        screen_width: u16,
    ) -> Vec<ViewLineMapping> {
        let layout_output = Self::compute_buffer_layout(
            state,
            cursors,
            viewport,
            folds,
            area,
            is_active,
            theme,
            lsp_waiting,
            view_mode.clone(),
            compose_width,
            view_transform,
            estimated_line_length,
            highlight_context_bytes,
            relative_line_numbers,
            use_terminal_bg,
            session_mode,
            software_cursor_only,
            show_line_numbers,
            highlight_current_line,
            diagnostics_inline_text,
            show_tilde,
            Some((cell_theme_map, screen_width)),
        );

        let view_line_mappings = layout_output.view_line_mappings.clone();

        Self::draw_buffer_in_split(
            frame,
            state,
            cursors,
            layout_output,
            event_log,
            area,
            is_active,
            theme,
            ansi_background,
            background_fade,
            hide_cursor,
            software_cursor_only,
            rulers,
            compose_column_guides,
        );

        view_line_mappings
    }


}

#[cfg(test)]
mod tests {
    use crate::model::filesystem::StdFileSystem;
    use std::sync::Arc;

    fn test_fs() -> Arc<dyn crate::model::filesystem::FileSystem + Send + Sync> {
        Arc::new(StdFileSystem)
    }
    use super::*;
    use crate::model::buffer::Buffer;
    use crate::primitives::display_width::str_width;
    use crate::view::theme;
    use crate::view::theme::Theme;
    use crate::view::viewport::Viewport;
    use lsp_types::FoldingRange;

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

        let selection = SplitRenderer::selection_context(&state, &cursors);
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
        let decorations = SplitRenderer::decoration_context(
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
        let output = SplitRenderer::render_view_lines(LineRenderInput {
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
        let cursor = SplitRenderer::resolve_cursor_fallback(
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
        let cursor = SplitRenderer::resolve_cursor_fallback(
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

        let final_cursor = SplitRenderer::resolve_cursor_fallback(
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

        let final_cursor = SplitRenderer::resolve_cursor_fallback(
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

    use crate::model::buffer::LineEnding;
    use fresh_core::api::{ViewTokenWire, ViewTokenWireKind};

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
        let wrapped =
            apply_wrapping_transform(tokens, MAX_SAFE_LINE_WIDTH, 0, false);

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
        let wrapped =
            apply_wrapping_transform(tokens, MAX_SAFE_LINE_WIDTH, 0, false);

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
        let wrapped =
            apply_wrapping_transform(tokens, MAX_SAFE_LINE_WIDTH, 0, false);

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
        apply_osc8_to_cells(
            &mut buf1,
            1,
            14,
            0,
            "https://example.com",
            Some((0, 0)),
        );
        let row1 = read_row(&buf1, 0);

        // Second render: fresh buffer, same text, apply OSC 8 with cursor at col 5
        let mut buf2 = Buffer::empty(area);
        for (i, ch) in text.chars().enumerate() {
            if (i as u16) < 40 {
                buf2[(i as u16, 0)].set_symbol(&ch.to_string());
            }
        }
        apply_osc8_to_cells(
            &mut buf2,
            1,
            14,
            0,
            "https://example.com",
            Some((5, 0)),
        );
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
        apply_osc8_to_cells(
            &mut frame1,
            0,
            13,
            0,
            "https://example.com",
            Some((0, 5)),
        );

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
        apply_osc8_to_cells(
            &mut frame2,
            1,
            14,
            0,
            "https://example.com",
            Some((0, 0)),
        );

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

        let selection = SplitRenderer::selection_context(&state, &cursors);
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
        let decorations = SplitRenderer::decoration_context(
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

        SplitRenderer::render_view_lines(LineRenderInput {
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
