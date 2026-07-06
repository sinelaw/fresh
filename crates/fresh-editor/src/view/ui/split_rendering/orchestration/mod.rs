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
pub(super) mod overlay_sweep;
pub(super) mod overlays;
pub(super) mod render_buffer;
pub(super) mod render_composite;
pub(super) mod render_line;
pub(super) mod selection_sweep;
pub(super) mod tail_fill;

use super::base_tokens::build_base_tokens;
use super::layout::{
    render_separator, resolve_view_preferences, split_buffers_for_tabs, split_layout,
    sync_viewport_to_content, SplitLayout,
};
use super::scrollbar::{
    compute_max_line_length, render_composite_scrollbar, render_horizontal_scrollbar,
    render_scrollbar, scrollbar_line_counts,
};
use super::EditorRenderConfig;
use crate::app::types::ViewLineMapping;
use crate::app::BufferMetadata;
use crate::config::IndentationGuideMode;
use crate::model::buffer::Buffer;
use crate::model::event::{BufferId, EventLog, LeafId, SplitDirection};
use crate::state::EditorState;
use crate::view::folding::FoldManager;
use crate::view::split::SplitManager;
use crate::view::ui::tabs::TabsRenderer;
use ratatui::layout::Rect;
use ratatui::style::Style;
use ratatui::widgets::Paragraph;
use ratatui::widgets::Widget;
use render_buffer::compute_buffer_layout;
// Re-exported one level up (split_rendering::SplitRenderer) so the
// `render_phantom_leaf` façade can forward into the per-leaf
// pipeline. Stays crate-private; callers use the façade.
pub(super) use render_buffer::render_buffer_in_split;
use render_composite::render_composite_buffer;
use std::collections::HashMap;

/// How a single visible split should be rendered. Computed up-front by
/// [`expand_visible_buffers`], which expands any active buffer-group tab into
/// its inner panels.
#[derive(Copy, Clone, PartialEq, Eq)]
enum RenderKind {
    /// Regular split: render the tab bar and the buffer content.
    Normal,
    /// Main split whose buffer group is active: render the tab bar (to show
    /// the group tab) but skip buffer content — the group's inner leaves fill
    /// it instead.
    GroupTabBarOnly,
    /// A leaf inside a Grouped subtree. `split_area` is already the content
    /// rect for this inner leaf; no tab bar is rendered.
    InnerLeaf,
}

/// One visible split to render: `(tab_bar_owner_split, effective_leaf_id,
/// buffer_id, split_area, kind)`.
type VisibleBuffer = (LeafId, LeafId, BufferId, Rect, RenderKind);

/// # Returns
/// * Vec of (split_id, buffer_id, content_rect, scrollbar_rect, thumb_start, thumb_end) for mouse handling
#[allow(clippy::too_many_arguments)]
#[allow(clippy::type_complexity)]
pub(crate) fn render_content(
    buf: &mut ratatui::buffer::Buffer,
    area: Rect,
    split_manager: &SplitManager,
    buffers: &mut HashMap<BufferId, EditorState>,
    buffer_metadata: &HashMap<BufferId, BufferMetadata>,
    // Buffer id of the window's single preview tab (`window.preview`), or
    // `None`. Drives the italic "(preview)" tab styling.
    preview_buffer: Option<BufferId>,
    event_logs: &mut HashMap<BufferId, EventLog>,
    composite_buffers: &mut HashMap<BufferId, crate::model::composite_buffer::CompositeBuffer>,
    composite_view_states: &mut HashMap<
        (LeafId, BufferId),
        crate::view::composite_view::CompositeViewState,
    >,
    // Appearance/policy group (theme + ANSI backdrop + editor render config),
    // identical for every split. Unpacked into locals at the top of the body
    // and threaded as-is into render_buffer_in_split.
    style: crate::view::ui::RenderStyle<'_>,
    lsp_waiting: bool,
    mut split_view_states: Option<&mut HashMap<LeafId, crate::view::split::SplitViewState>>,
    grouped_subtrees: &HashMap<LeafId, crate::view::split::SplitNode>,
    hide_cursor: bool,
    hovered_tab: Option<(crate::view::split::TabTarget, LeafId, bool)>, // (target, split_id, is_close_button)
    hovered_close_split: Option<LeafId>,
    hovered_maximize_split: Option<LeafId>,
    is_maximized: bool,
    tab_bar_visible: bool,
    session_mode: bool,
    // Whether the window is in terminal mode. When set, the active split's
    // terminal buffer is showing its live PTY grid (not the read-only
    // scrollback view), so its vertical scrollbar is suppressed and the grid
    // reclaims that column. Exiting terminal mode brings the scrollbar back.
    terminal_mode: bool,
    cell_theme_map: &mut Vec<crate::app::types::CellThemeInfo>,
    screen_width: u16,
    pending_hardware_cursor: &mut Option<(u16, u16)>,
    // When false, the tab bar computes its layout but paints no cells (web
    // renders tabs natively). Panes always draw. TUI passes `true`.
    draw_tab_bar: bool,
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

    // Unpack the style group. `theme` is forwarded to the sub-painters; the
    // whole `style` (incl. `ansi_background` + full `cfg`) is threaded as-is
    // into render_buffer_in_split. Only the `cfg` flags this layer reads itself
    // (per-split flag math, scrollbars, composite) are bound below; the rest
    // ride along inside `style.cfg`.
    let crate::view::ui::RenderStyle { theme, cfg, .. } = style;
    let EditorRenderConfig {
        large_file_threshold_bytes,
        use_terminal_bg,
        show_vertical_scrollbar,
        show_horizontal_scrollbar,
        show_tilde,
        highlight_current_column,
        hide_current_line_on_selection,
        ..
    } = cfg;

    let base_visible = split_manager.get_visible_buffers(area);
    let active_split_id = split_manager.active_split();
    let has_multiple_splits = base_visible.len() > 1;

    // Expand any active buffer-group tabs into their inner panels.
    let visible_buffers = expand_visible_buffers(
        &base_visible,
        split_view_states.as_deref_mut(),
        grouped_subtrees,
        tab_bar_visible,
        show_vertical_scrollbar,
        show_horizontal_scrollbar,
    );

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
        // For a buffer-group panel (inner leaf), `active_split()` returns the
        // group's *outer* leaf, so `is_active` is never true for the panel
        // itself. The panel is focused when the active split's
        // `focused_group_leaf` points at this inner leaf. Used to gate the
        // composite cursor so it doesn't linger after Tab moves focus away.
        let panel_focused = if is_inner_group_leaf {
            split_view_states
                .as_deref()
                .and_then(|svs| svs.get(&active_split_id))
                .and_then(|vs| vs.focused_group_leaf)
                .is_some_and(|fl| fl == split_id)
        } else {
            is_active
        };
        let _ = main_split_id; // no longer needed below, kept for clarity

        // Suppress chrome (tab bar) for splits in buffer groups
        let split_tab_bar_visible = !is_inner_group_leaf
            && tab_bar_visible
            && !split_view_states
                .as_deref()
                .and_then(|svs| svs.get(&split_id))
                .is_some_and(|vs| vs.suppress_chrome);
        // Hide tildes per-split (e.g., for buffer group panels). Also
        // hide them when the split's active buffer is a terminal in
        // scrollback view — the PTY drew blank rows, so empty rows
        // past end-of-buffer should look blank rather than tilde-padded.
        // Without this, viewing a small-PTY terminal in a larger split
        // (e.g. workspace-restored dock terminal switched via Alt+]
        // into the main pane) shows tildes where the live PTY drew
        // blank.
        let active_buf_is_terminal = buffer_metadata
            .get(&buffer_id)
            .and_then(|m| m.virtual_mode())
            .is_some_and(|m| m == "terminal");
        let split_show_tilde = show_tilde
            && !split_view_states
                .as_deref()
                .and_then(|svs| svs.get(&split_id))
                .is_some_and(|vs| vs.hide_tilde)
            && !active_buf_is_terminal;

        // Non-scrollable panels (Fixed toolbars/headers/footers by default,
        // or any panel created with `scrollable: false`) don't get a
        // scrollbar — their content is pinned to the panel size.
        let is_non_scrollable = buffers.get(&buffer_id).is_some_and(|s| !s.scrollable);

        // A terminal showing its live PTY grid suppresses the scrollbar so the
        // grid uses the full split width. The live grid is shown for the active
        // split's terminal while in terminal mode (the read-only scrollback view
        // shown after exiting terminal mode keeps its scrollbar). See
        // `render_terminal_splits`, which overlays the grid into `content_rect`.
        let terminal_showing_live_grid = active_buf_is_terminal && is_active && terminal_mode;
        let panel_show_vscroll =
            show_vertical_scrollbar && !is_non_scrollable && !terminal_showing_live_grid;

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
                panel_show_vscroll,
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
            render_split_tab_bar(
                buf,
                &layout,
                split_id,
                buffer_id,
                buffers,
                buffer_metadata,
                composite_buffers,
                split_view_states.as_deref(),
                grouped_subtrees,
                &split_buffers,
                theme,
                is_active,
                tab_scroll_offset,
                tab_hover_for_split,
                preview_buffer,
                draw_tab_bar,
                has_multiple_splits,
                is_maximized,
                hovered_close_split,
                hovered_maximize_split,
                cell_theme_map,
                screen_width,
                &mut tab_layouts,
                &mut close_split_areas,
                &mut maximize_split_areas,
            );
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
            render_placeholder_hint(buf, layout.content_rect, theme);
            view_line_mappings.insert(split_id, Vec::new());
            continue;
        }

        // Composite buffers (side-by-side diff/compare panes) render through a
        // separate pipeline; dispatch them to their own helper.
        if buffers
            .get(&buffer_id)
            .is_some_and(|s| s.is_composite_buffer)
        {
            render_composite_split(
                buf,
                &layout,
                split_id,
                buffer_id,
                buffers,
                composite_buffers,
                composite_view_states,
                split_view_states.as_deref_mut(),
                theme,
                panel_focused,
                use_terminal_bg,
                split_show_tilde,
                show_vertical_scrollbar,
                is_non_scrollable,
                is_active,
                show_horizontal_scrollbar,
                &mut split_areas,
                &mut horizontal_scrollbar_areas,
            );
            view_line_mappings.insert(split_id, Vec::new());
            continue;
        }

        // Get references separately to avoid double borrow
        let state_opt = buffers.get_mut(&buffer_id);
        let event_log_opt = event_logs.get_mut(&buffer_id);

        if let Some(state) = state_opt {
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
                    is_non_scrollable,
                    state.buffer_settings.virtual_space,
                );
            }
            let view_prefs =
                resolve_view_preferences(state, split_view_states.as_deref(), split_id);

            // When cursors are hidden, also suppress current-line highlighting
            // and selection rendering so the buffer appears fully non-interactive.
            let has_selection = hide_current_line_on_selection
                && split_cursors
                    .iter()
                    .any(|(_, c)| c.selection_range().is_some());
            let effective_highlight_current_line =
                view_prefs.highlight_current_line && state.show_cursors && !has_selection;

            // Column rulers are a source-code editing aid; virtual buffers
            // (dashboard, *Diagnostics*, grep results, ...) aren't code, so
            // the config-driven rulers would just paint stripes over plugin
            // chrome. Suppress them for any virtual buffer.
            let is_virtual_buffer = buffer_metadata
                .get(&buffer_id)
                .is_some_and(|m| m.is_virtual());
            let effective_rulers: &[usize] = if is_virtual_buffer {
                &[]
            } else {
                &view_prefs.rulers
            };

            // Indentation guides are a source-code editing aid, like the column
            // rulers above. Resolve the effective mode per buffer: an explicit
            // plugin override (`setIndentationGuide`) wins; otherwise virtual
            // buffers (grep results, *Diagnostics*, the Git Log commit list, …)
            // default off because they aren't code, while ordinary file buffers
            // follow the global setting. A file-backed tool view the virtual
            // default can't catch — the Git Log commit-detail diff — opts out via
            // the override. This is independent of the line-number gutter, so an
            // ordinary buffer keeps its guides with line numbers turned off.
            let mut style = style;
            style.cfg.indentation_guide = match state.indentation_guide_override {
                Some(true) => style.cfg.indentation_guide,
                Some(false) => IndentationGuideMode::None,
                None if is_virtual_buffer => IndentationGuideMode::None,
                None => style.cfg.indentation_guide,
            };

            let mut empty_folds = FoldManager::new();
            let folds = split_view_states
                .as_deref_mut()
                .and_then(|vs| vs.get_mut(&split_id))
                .map(|vs| &mut vs.folds)
                .unwrap_or(&mut empty_folds);

            let _render_buf_span = tracing::trace_span!("render_buffer_in_split").entered();
            let split_view_mappings = render_buffer_in_split(
                buf,
                state,
                &split_cursors,
                &mut viewport,
                folds,
                event_log_opt,
                layout.content_rect,
                is_active,
                style,
                lsp_waiting,
                view_prefs.view_mode,
                view_prefs.compose_width,
                view_prefs.compose_column_guides,
                view_prefs.view_transform,
                buffer_id,
                hide_cursor,
                session_mode,
                effective_rulers,
                view_prefs.show_line_numbers,
                effective_highlight_current_line,
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
            let (thumb_start, thumb_end) = if panel_show_vscroll {
                render_scrollbar(
                    buf,
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
                    buf,
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
        render_separator(buf, direction, x, y, length, theme);
    }
    // Walk the visible splits again to render internal separators of any
    // active buffer groups (their Split nodes live in the side-map, not the
    // main split tree, so `split_manager` doesn't know about them).
    let grouped_separator_areas = render_grouped_separators(
        buf,
        &base_visible,
        split_view_states.as_deref(),
        grouped_subtrees,
        theme,
        tab_bar_visible,
        show_vertical_scrollbar,
        show_horizontal_scrollbar,
    );

    // Record vertical-scrollbar theme keys for the inspector, from the
    // thumb/track geometry just computed for each split.
    record_scrollbar_theme_runs(&split_areas, cell_theme_map, screen_width);

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

/// Build the list of splits to render, expanding any active buffer-group tab
/// into a [`RenderKind::GroupTabBarOnly`] entry for the main split followed by
/// one [`RenderKind::InnerLeaf`] entry per panel. Inner-panel viewports are
/// resized to their rendered rects so `editor.getViewport()` reports the panel
/// size (not the terminal size) and resize timing stays correct.
fn expand_visible_buffers(
    base_visible: &[(LeafId, BufferId, Rect)],
    mut split_view_states: Option<&mut HashMap<LeafId, crate::view::split::SplitViewState>>,
    grouped_subtrees: &HashMap<LeafId, crate::view::split::SplitNode>,
    tab_bar_visible: bool,
    show_vertical_scrollbar: bool,
    show_horizontal_scrollbar: bool,
) -> Vec<VisibleBuffer> {
    let mut visible_buffers: Vec<VisibleBuffer> = Vec::new();
    for (main_split_id, main_buffer_id, split_area) in base_visible {
        let active_group = split_view_states
            .as_deref()
            .and_then(|svs| svs.get(main_split_id))
            .and_then(|vs| vs.active_group_tab);

        let grouped = active_group.and_then(|leaf| grouped_subtrees.get(&leaf));
        let Some(grouped) = grouped else {
            visible_buffers.push((
                *main_split_id,
                *main_split_id,
                *main_buffer_id,
                *split_area,
                RenderKind::Normal,
            ));
            continue;
        };

        // Compute the content rect for this main split (after its tab bar),
        // then lay the group's leaves out within it.
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
            // Keep inner panel viewports in sync with their actual rendered
            // dimensions (updated synchronously during rendering).
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
    }
    visible_buffers
}

/// Render one split's tab bar and its split-control (close / maximize) buttons,
/// recording the resulting tab layout and button hit areas for mouse handling.
#[allow(clippy::too_many_arguments)]
fn render_split_tab_bar(
    buf: &mut ratatui::buffer::Buffer,
    layout: &SplitLayout,
    split_id: LeafId,
    buffer_id: BufferId,
    buffers: &HashMap<BufferId, EditorState>,
    buffer_metadata: &HashMap<BufferId, BufferMetadata>,
    composite_buffers: &HashMap<BufferId, crate::model::composite_buffer::CompositeBuffer>,
    split_view_states: Option<&HashMap<LeafId, crate::view::split::SplitViewState>>,
    grouped_subtrees: &HashMap<LeafId, crate::view::split::SplitNode>,
    split_buffers: &[crate::view::split::TabTarget],
    theme: &crate::view::theme::Theme,
    is_active: bool,
    tab_scroll_offset: usize,
    tab_hover_for_split: Option<(crate::view::split::TabTarget, bool)>,
    preview_buffer: Option<BufferId>,
    draw_tab_bar: bool,
    has_multiple_splits: bool,
    is_maximized: bool,
    hovered_close_split: Option<LeafId>,
    hovered_maximize_split: Option<LeafId>,
    cell_theme_map: &mut [crate::app::types::CellThemeInfo],
    screen_width: u16,
    tab_layouts: &mut HashMap<LeafId, crate::view::ui::tabs::TabLayout>,
    close_split_areas: &mut Vec<(LeafId, u16, u16, u16)>,
    maximize_split_areas: &mut Vec<(LeafId, u16, u16, u16)>,
) {
    // Determine the active target for this split's tab bar: the marked group
    // tab if any, otherwise the currently displayed buffer.
    let active_target = split_view_states
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
    // Render tabs for this split and collect hit areas. The tab bar records its
    // theme-key runs into a local vec as it paints; apply them to the per-cell
    // map afterward (the map isn't borrowed here).
    let mut tab_runs: Vec<crate::app::types::ThemeRun> = Vec::new();
    let tab_layout = {
        let mut rec = crate::app::types::CellThemeRecorder::new(&mut tab_runs);
        TabsRenderer::render_for_split(
            buf,
            layout.tabs_rect,
            split_buffers,
            buffers,
            buffer_metadata,
            composite_buffers,
            active_target,
            theme,
            is_active,
            tab_scroll_offset,
            tab_hover_for_split,
            &group_names,
            preview_buffer,
            Some(&mut rec),
            draw_tab_bar,
        )
    };
    crate::app::types::apply_theme_runs(cell_theme_map, screen_width, &tab_runs);

    tab_layouts.insert(split_id, tab_layout);
    let tab_row = layout.tabs_rect.y;

    // Split control buttons at the right side of the tabs row.
    //   Maximize/unmaximize: shown when multiple splits exist OR maximized.
    //   Close: shown when multiple splits exist AND not maximized.
    let show_maximize_btn = has_multiple_splits || is_maximized;
    let show_close_btn = has_multiple_splits && !is_maximized;
    if !show_maximize_btn && !show_close_btn {
        return;
    }

    // Layout from the right edge: [maximize] [space] [close] |
    let mut btn_x = layout.tabs_rect.x + layout.tabs_rect.width.saturating_sub(2);
    if show_close_btn {
        let is_hovered = hovered_close_split == Some(split_id);
        let close_fg = if is_hovered {
            theme.tab_close_hover_fg
        } else {
            theme.line_number_fg
        };
        let close_button =
            Paragraph::new("×").style(Style::default().fg(close_fg).bg(theme.tab_separator_bg));
        close_button.render(Rect::new(btn_x, tab_row, 1, 1), buf);
        close_split_areas.push((split_id, tab_row, btn_x, btn_x + 1));
        btn_x = btn_x.saturating_sub(2); // 1 space before the next button
    }
    if show_maximize_btn {
        let is_hovered = hovered_maximize_split == Some(split_id);
        let max_fg = if is_hovered {
            theme.tab_close_hover_fg
        } else {
            theme.line_number_fg
        };
        // □ = maximize, ⧉ = unmaximize (restore).
        let icon = if is_maximized { "⧉" } else { "□" };
        let max_button =
            Paragraph::new(icon).style(Style::default().fg(max_fg).bg(theme.tab_separator_bg));
        max_button.render(Rect::new(btn_x, tab_row, 1, 1), buf);
        maximize_split_areas.push((split_id, tab_row, btn_x, btn_x + 1));
    }
}

/// Render a composite (side-by-side panes) buffer for one split, plus its
/// scrollbar, and record the content/scrollbar areas for mouse handling.
#[allow(clippy::too_many_arguments)]
fn render_composite_split(
    buf: &mut ratatui::buffer::Buffer,
    layout: &SplitLayout,
    split_id: LeafId,
    buffer_id: BufferId,
    buffers: &mut HashMap<BufferId, EditorState>,
    composite_buffers: &mut HashMap<BufferId, crate::model::composite_buffer::CompositeBuffer>,
    composite_view_states: &mut HashMap<
        (LeafId, BufferId),
        crate::view::composite_view::CompositeViewState,
    >,
    split_view_states: Option<&mut HashMap<LeafId, crate::view::split::SplitViewState>>,
    theme: &crate::view::theme::Theme,
    panel_focused: bool,
    use_terminal_bg: bool,
    split_show_tilde: bool,
    show_vertical_scrollbar: bool,
    is_non_scrollable: bool,
    is_active: bool,
    show_horizontal_scrollbar: bool,
    split_areas: &mut Vec<(LeafId, BufferId, Rect, Rect, usize, usize)>,
    horizontal_scrollbar_areas: &mut Vec<(LeafId, BufferId, Rect, usize, usize, usize)>,
) {
    // Take initial_focus_hunk before borrowing composite immutably.
    let initial_focus_hunk = composite_buffers
        .get_mut(&buffer_id)
        .and_then(|c| c.initial_focus_hunk.take());
    let Some(composite) = composite_buffers.get(&buffer_id) else {
        return;
    };

    // Update SplitViewState viewport to match the actual rendered area so
    // cursor movement uses the correct viewport height after a resize.
    if let Some(svs) = split_view_states {
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

    // Get or create the composite view state.
    let pane_count = composite.pane_count();
    let view_state = composite_view_states
        .entry((split_id, buffer_id))
        .or_insert_with(|| {
            crate::view::composite_view::CompositeViewState::new(buffer_id, pane_count)
        });

    // Apply the deferred initial focus hunk (first render only). This runs here
    // because it's the only place where both the CompositeViewState and the
    // correct viewport height exist.
    if let Some(hunk_index) = initial_focus_hunk {
        let mut hunk_count = 0usize;
        let target_row = composite.alignment.rows.iter().position(|row| {
            if row.row_type == crate::model::composite_buffer::RowType::HunkHeader {
                let hit = hunk_count == hunk_index;
                hunk_count += 1;
                hit
            } else {
                false
            }
        });
        if let Some(row) = target_row {
            let viewport_height = layout.content_rect.height.saturating_sub(1) as usize;
            let context_above = viewport_height / 3;
            view_state.cursor_row = row;
            view_state.scroll_row = row.saturating_sub(context_above);
        }
    }

    render_composite_buffer(
        buf,
        layout.content_rect,
        composite,
        buffers,
        theme,
        panel_focused,
        view_state,
        use_terminal_bg,
        split_show_tilde,
    );

    let total_rows = composite.row_count();
    let content_height = layout.content_rect.height.saturating_sub(1) as usize; // -1 for header
    let (thumb_start, thumb_end) = if show_vertical_scrollbar && !is_non_scrollable {
        render_composite_scrollbar(
            buf,
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

/// Render the internal separators of any active buffer groups and return their
/// hit areas (with container IDs) so the hit-test path can wire up dragging.
/// A group's Split nodes live in the side-map, not the main split tree, so
/// `split_manager` doesn't know about them.
#[allow(clippy::too_many_arguments)]
#[allow(clippy::type_complexity)]
fn render_grouped_separators(
    buf: &mut ratatui::buffer::Buffer,
    base_visible: &[(LeafId, BufferId, Rect)],
    split_view_states: Option<&HashMap<LeafId, crate::view::split::SplitViewState>>,
    grouped_subtrees: &HashMap<LeafId, crate::view::split::SplitNode>,
    theme: &crate::view::theme::Theme,
    tab_bar_visible: bool,
    show_vertical_scrollbar: bool,
    show_horizontal_scrollbar: bool,
) -> Vec<(
    crate::model::event::ContainerId,
    SplitDirection,
    u16,
    u16,
    u16,
)> {
    let mut grouped_separator_areas = Vec::new();
    for (main_split_id, _main_buffer_id, split_area) in base_visible {
        let active_group = split_view_states
            .and_then(|svs| svs.get(main_split_id))
            .and_then(|vs| vs.active_group_tab);
        let Some(grouped) = active_group.and_then(|leaf| grouped_subtrees.get(&leaf)) else {
            continue;
        };
        let split_tab_bar_visible = tab_bar_visible
            && !split_view_states
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
                render_separator(buf, direction, x, y, length, theme);
                grouped_separator_areas.push((id, direction, x, y, length));
            }
        }
    }
    grouped_separator_areas
}

/// Record vertical-scrollbar theme keys (thumb vs. track) for the theme
/// inspector, from the geometry computed for each split during rendering.
fn record_scrollbar_theme_runs(
    split_areas: &[(LeafId, BufferId, Rect, Rect, usize, usize)],
    cell_theme_map: &mut [crate::app::types::CellThemeInfo],
    screen_width: u16,
) {
    let mut sb_runs: Vec<crate::app::types::ThemeRun> = Vec::new();
    for (_, _, _, scrollbar_rect, thumb_start, thumb_end) in split_areas {
        for row in 0..scrollbar_rect.height {
            let is_thumb = (row as usize) >= *thumb_start && (row as usize) < *thumb_end;
            sb_runs.push(crate::app::types::ThemeRun {
                x: scrollbar_rect.x,
                y: scrollbar_rect.y + row,
                w: scrollbar_rect.width,
                fg_key: Some(if is_thumb {
                    "ui.scrollbar_thumb_fg"
                } else {
                    "ui.scrollbar_track_fg"
                }),
                bg_key: Some("editor.bg"),
                region: if is_thumb {
                    "Scrollbar Thumb"
                } else {
                    "Scrollbar Track"
                },
            });
        }
    }
    crate::app::types::apply_theme_runs(cell_theme_map, screen_width, &sb_runs);
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
        let pin_to_top = !state.scrollable;
        sync_viewport_to_content(
            &mut viewport,
            &mut state.buffer,
            &split_cursors,
            layout.content_rect,
            &hidden_ranges,
            split_compose_width,
            split_show_line_numbers,
            pin_to_top,
            state.buffer_settings.virtual_space,
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
            IndentationGuideMode::None,
            "▏",
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
fn render_placeholder_hint(
    buf: &mut ratatui::buffer::Buffer,
    area: Rect,
    theme: &crate::view::theme::Theme,
) {
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
    Paragraph::new(HINT).style(style).render(hint_area, buf);
}
