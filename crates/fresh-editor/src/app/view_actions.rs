//! View mode action handlers.
//!
//! This module contains handlers for view-related actions like compose mode
//! toggling. All bodies live on `impl Window` — none of the helpers reach
//! editor-global state (plugin manager, mode registry, etc.); they manipulate
//! per-window split-view-state and animations.

use crate::app::window::Window;
use crate::model::event::LeafId;
use crate::state::ViewMode;
use fresh_i18n::t;

impl Window {
    /// Toggle between Compose and Source view modes for the active split.
    pub fn handle_toggle_page_view(&mut self) {
        let (mgr, _) = self
            .buffers
            .splits()
            .expect("active window must have a populated split layout");
        let active_split = mgr.active_split();
        let active_buffer = mgr
            .get_buffer_id(active_split.into())
            .unwrap_or(crate::model::event::BufferId(0));
        let default_wrap = self.resolve_line_wrap_for_buffer(active_buffer);
        let default_line_numbers = self.config().editor.line_numbers;
        let page_width = self
            .buffers
            .get(&active_buffer)
            .and_then(|s| self.config().languages.get(&s.language))
            .and_then(|lc| lc.page_width)
            .or(self.config().editor.page_width);

        let view_mode = {
            let (_, vs_map) = self
                .buffers
                .splits()
                .expect("active window must have a populated split layout");
            let current = vs_map
                .get(&active_split)
                .map(|vs| vs.view_mode.clone())
                .unwrap_or(ViewMode::Source);
            match current {
                ViewMode::PageView => ViewMode::Source,
                _ => ViewMode::PageView,
            }
        };

        // Update split view state (source of truth for view mode and line numbers)
        if let Some(vs) = self
            .split_view_states_mut()
            .expect("active window must have a populated split layout")
            .get_mut(&active_split)
        {
            vs.view_mode = view_mode.clone();
            // In Compose mode, disable builtin line wrap - the plugin handles
            // wrapping by inserting Break tokens in the view transform pipeline.
            // In Source mode, respect the user's default_wrap preference.
            vs.viewport.line_wrap_enabled = match view_mode {
                ViewMode::PageView => false,
                // A per-buffer override wins over the global/language default
                // when returning to Source mode.
                ViewMode::Source => vs.line_wrap_override.unwrap_or(default_wrap),
            };
            match view_mode {
                ViewMode::PageView => {
                    vs.show_line_numbers = false;
                    // Apply page_width from language config if available
                    if let Some(width) = page_width {
                        vs.compose_width = Some(width as u16);
                    }
                }
                ViewMode::Source => {
                    // Clear compose width to remove margins
                    vs.compose_width = None;
                    // A per-buffer override wins over the global default.
                    vs.show_line_numbers = vs.line_numbers_override.unwrap_or(default_line_numbers);
                }
            }
        }

        let mode_label = match view_mode {
            ViewMode::PageView => t!("view.page_view").to_string(),
            ViewMode::Source => "Source".to_string(),
        };
        self.set_status_message(t!("view.mode", mode = mode_label).to_string());
    }

    /// Start a horizontal slide over the given split's content area to
    /// visualize a tab switch. `direction`: +1 = the new tab is to
    /// the right of the previous one in tab order, so the new view
    /// pushes in from the right; -1 = the new tab is to the left,
    /// view pushes in from the left; 0 = no animation.
    ///
    /// `area` is where the pane — or, for a buffer group, the union of its
    /// panels — sits, which the caller asks the shell tree for: a `Window`
    /// cannot see the tree, and this used to resolve it from the painter's
    /// record of the last render pass. A pane not on screen yet (freshly
    /// created) has no rectangle and the call is a no-op, as before:
    /// animation is a decorative layer and missing it costs nothing.
    pub(crate) fn animate_tab_switch(&mut self, area: ratatui::layout::Rect, direction: i32) {
        if direction == 0 {
            return;
        }
        if !self.config().editor.animations {
            return;
        }
        if area.width == 0 || area.height == 0 {
            return;
        }
        let from = if direction > 0 {
            crate::view::animation::Edge::Right
        } else {
            crate::view::animation::Edge::Left
        };
        self.animations.start(
            area,
            crate::view::animation::AnimationKind::SlideIn {
                from,
                duration: std::time::Duration::from_millis(260),
                delay: std::time::Duration::ZERO,
            },
        );
    }
}

impl super::Editor {
    /// Where a pane's content is — or, when the pane is showing a buffer
    /// group, the box its panels cover between them.
    ///
    /// A grouped pane renders its *inner* subtree, so the tree has a
    /// rectangle per panel (log / detail / toolbar) and none under the outer
    /// leaf's key. The union of the inner ones is the area the group
    /// occupies, which is what a tab-switch animation slides.
    pub(crate) fn pane_or_group_content_rect(
        &self,
        split_id: LeafId,
    ) -> Option<ratatui::layout::Rect> {
        if let Some(rect) = self.pane_content_rect(split_id) {
            return Some(rect);
        }
        let win = self.windows.get(&self.active_window)?;
        let (_, vs_map) = win.buffers.splits()?;
        let group_leaf = vs_map.get(&split_id).and_then(|vs| vs.active_group_tab)?;
        let mut inner: Vec<LeafId> = Vec::new();
        collect_leaf_ids(win.grouped_subtrees.get(&group_leaf)?, &mut inner);
        inner
            .into_iter()
            .filter_map(|leaf| self.pane_content_rect(leaf))
            .reduce(rect_union)
    }
}

/// Walk a SplitNode collecting every Leaf's `split_id`.
fn collect_leaf_ids(node: &crate::view::split::SplitNode, out: &mut Vec<LeafId>) {
    use crate::view::split::SplitNode;
    match node {
        SplitNode::Leaf { split_id, .. } => out.push(*split_id),
        SplitNode::Split { first, second, .. } => {
            collect_leaf_ids(first, out);
            collect_leaf_ids(second, out);
        }
        SplitNode::Grouped { layout, .. } => collect_leaf_ids(layout, out),
    }
}

fn rect_union(a: ratatui::layout::Rect, b: ratatui::layout::Rect) -> ratatui::layout::Rect {
    let x = a.x.min(b.x);
    let y = a.y.min(b.y);
    let right = a.x.saturating_add(a.width).max(b.x.saturating_add(b.width));
    let bottom =
        a.y.saturating_add(a.height)
            .max(b.y.saturating_add(b.height));
    ratatui::layout::Rect::new(x, y, right.saturating_sub(x), bottom.saturating_sub(y))
}
