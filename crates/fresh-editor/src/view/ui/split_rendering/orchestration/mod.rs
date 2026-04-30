//! Orchestration layer.
//!
//! This subdirectory holds the code that depends on the shared
//! [`SelectionContext`](contexts::SelectionContext) and
//! [`DecorationContext`](contexts::DecorationContext) carriers. Everything
//! *outside* this directory is self-contained and has no such dependency —
//! the quarantine is intentional.
//!
//! Public entry points ([`render_content`], [`compute_content_layout`],
//! [`build_base_tokens_for_hook`]) live here too; the top-level `mod.rs`
//! is a thin façade that re-exports them via the `SplitRenderer` struct.

pub(super) mod contexts;
pub(super) mod overlays;
pub(super) mod render_buffer;
pub(super) mod render_composite;
pub(super) mod render_line;

use super::base_tokens::build_base_tokens;
use super::layout::{
    render_separator, resolve_view_preferences, split_buffers_for_tabs, split_layout,
    sync_viewport_to_content, SplitLayout,
};
use super::scrollbar::{
    compute_max_line_length, render_composite_scrollbar, render_horizontal_scrollbar,
    render_scrollbar, scrollbar_line_counts,
};
use crate::app::types::ViewLineMapping;
use crate::app::BufferMetadata;
use crate::model::buffer::Buffer;
use crate::model::event::{BufferId, EventLog, LeafId, SplitDirection};
use crate::primitives::ansi_background::AnsiBackground;
use crate::state::EditorState;
use crate::view::folding::FoldManager;
use crate::view::split::SplitManager;
use crate::view::ui::tabs::TabsRenderer;
use ratatui::layout::Rect;
use ratatui::style::Style;
use ratatui::widgets::Paragraph;
use ratatui::Frame;
use render_buffer::compute_buffer_layout;
// Re-exported one level up (split_rendering::SplitRenderer) so the
// `render_phantom_leaf` façade can forward into the per-leaf
// pipeline. Stays crate-private; callers use the façade.
pub(super) use render_buffer::render_buffer_in_split;
use render_composite::render_composite_buffer;
use std::collections::HashMap;

/// # Returns
/// * Vec of (split_id, buffer_id, content_rect, scrollbar_rect, thumb_start, thumb_end) for mouse handling
#[allow(clippy::too_many_arguments)]
#[allow(clippy::type_complexity)]
pub(crate) fn render_content(
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
    highlight_current_column: bool,
    cell_theme_map: &mut Vec<crate::app::types::CellThemeInfo>,
    screen_width: u16,
    pending_hardware_cursor: &mut Option<(u16, u16)>,
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

        // Synthesized placeholder buffer (kept alive when
        // `auto_create_empty_buffer_on_last_buffer_close` is disabled): paint
        // the pane blank with a subdued, centered hint so the user sees how
        // to leave the empty workspace state.
        let is_synthetic_placeholder = buffer_metadata
            .get(&buffer_id)
            .is_some_and(|m| m.synthetic_placeholder);
        if is_synthetic_placeholder {
            render_placeholder_hint(frame, layout.content_rect, theme);
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
                                split_vs
                                    .viewport
                                    .resize(layout.content_rect.width, layout.content_rect.height);
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
                            if row.row_type == crate::model::composite_buffer::RowType::HunkHeader {
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
                    render_composite_buffer(
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
                    let (thumb_start, thumb_end) = if show_vertical_scrollbar && !is_non_scrollable
                    {
                        render_composite_scrollbar(
                            frame,
                            layout.scrollbar_rect,
                            total_rows,
                            view_state.scroll_row,
                            content_height,
                            is_active,
                            theme,
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
                let (split_compose_width, split_show_line_numbers) = split_view_states
                    .as_deref()
                    .and_then(|vs| vs.get(&split_id))
                    .map(|vs| (vs.compose_width, vs.show_line_numbers))
                    .unwrap_or((None, true));
                sync_viewport_to_content(
                    &mut viewport,
                    &mut state.buffer,
                    &split_cursors,
                    layout.content_rect,
                    &hidden_ranges,
                    split_compose_width,
                    split_show_line_numbers,
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
            let split_view_mappings = render_buffer_in_split(
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
                highlight_current_column && state.show_cursors,
                cell_theme_map,
                screen_width,
                pending_hardware_cursor,
            );

            drop(_render_buf_span);

            // Store view line mappings for mouse click handling
            view_line_mappings.insert(split_id, split_view_mappings);

            // For small files, count actual lines for accurate scrollbar
            // For large files, we'll use a constant thumb size
            let buffer_len = state.buffer.len();
            let (total_lines, top_line) = {
                let _span = tracing::trace_span!("scrollbar_line_counts").entered();
                scrollbar_line_counts(state, &viewport, large_file_threshold_bytes, buffer_len)
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
            let max_content_width = if show_horizontal_scrollbar && !viewport.line_wrap_enabled {
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
                    theme,
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
                        "Writing back viewport: top_byte={}, top_view_line_offset={}, skip_ensure_visible={}",
                        viewport.top_byte,
                        viewport.top_view_line_offset,
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
pub(crate) fn compute_content_layout(
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
                .is_some_and(|vs| vs.suppress_chrome);

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

        let (split_compose_width, split_show_line_numbers) = split_view_states
            .get(&split_id)
            .map(|vs| (vs.compose_width, vs.show_line_numbers))
            .unwrap_or((None, true));
        sync_viewport_to_content(
            &mut viewport,
            &mut state.buffer,
            &split_cursors,
            layout.content_rect,
            &hidden_ranges,
            split_compose_width,
            split_show_line_numbers,
        );
        let view_prefs = resolve_view_preferences(state, Some(&*split_view_states), split_id);

        let effective_highlight_current_line =
            view_prefs.highlight_current_line && state.show_cursors;

        let mut empty_folds = FoldManager::new();
        let folds = split_view_states
            .get_mut(&split_id)
            .map(|vs| &mut vs.folds)
            .unwrap_or(&mut empty_folds);

        let layout_output = compute_buffer_layout(
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

/// Public wrapper for building base tokens - used by render.rs for the view_transform_request hook
pub(crate) fn build_base_tokens_for_hook(
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
        &[],
    )
}

/// Render a centered, subdued hint in the empty pane left behind when the
/// user closes the last buffer with both `file_explorer.auto_open_on_last_buffer_close`
/// and `editor.auto_create_empty_buffer_on_last_buffer_close` set to false.
/// Tells the user how to escape the blank-workspace state.
fn render_placeholder_hint(frame: &mut Frame, area: Rect, theme: &crate::view::theme::Theme) {
    const HINT: &str =
        "Ctrl+P  command palette   ·   Ctrl+O  open file   ·   Ctrl+E  file explorer";
    let needed_width = HINT.chars().count() as u16;
    if area.width < needed_width || area.height == 0 {
        return;
    }
    let x = area.x + area.width.saturating_sub(needed_width) / 2;
    let y = area.y + area.height / 2;
    let hint_area = Rect::new(x, y, needed_width, 1);
    let style = Style::default().fg(theme.syntax_comment);
    frame.render_widget(Paragraph::new(HINT).style(style), hint_area);
}
