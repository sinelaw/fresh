//! View mode action handlers.
//!
//! This module contains handlers for view-related actions like compose mode toggling.

use super::Editor;
use crate::model::event::LeafId;
use crate::state::ViewMode;
use rust_i18n::t;

impl Editor {
    /// Toggle between Compose and Source view modes.
    pub fn handle_toggle_page_view(&mut self) {
        let active_split = self.split_manager.active_split();
        let active_buffer = self
            .split_manager
            .get_buffer_id(active_split.into())
            .unwrap_or(crate::model::event::BufferId(0));
        let default_wrap = self.resolve_line_wrap_for_buffer(active_buffer);
        let default_line_numbers = self.config.editor.line_numbers;
        let page_width = self
            .buffers
            .get(&active_buffer)
            .and_then(|s| self.config.languages.get(&s.language))
            .and_then(|lc| lc.page_width)
            .or(self.config.editor.page_width);

        let view_mode = {
            let current = self
                .split_view_states
                .get(&active_split)
                .map(|vs| vs.view_mode.clone())
                .unwrap_or(ViewMode::Source);
            match current {
                ViewMode::PageView => ViewMode::Source,
                _ => ViewMode::PageView,
            }
        };

        // Update split view state (source of truth for view mode and line numbers)
        if let Some(vs) = self.split_view_states.get_mut(&active_split) {
            vs.view_mode = view_mode.clone();
            // In Compose mode, disable builtin line wrap - the plugin handles
            // wrapping by inserting Break tokens in the view transform pipeline.
            // In Source mode, respect the user's default_wrap preference.
            vs.viewport.line_wrap_enabled = match view_mode {
                ViewMode::PageView => false,
                ViewMode::Source => default_wrap,
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
                    vs.view_transform = None;
                    vs.show_line_numbers = default_line_numbers;
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
    /// The split's Rect is resolved from the cached layout captured
    /// in the last render pass. If the split isn't on screen yet
    /// (freshly created) the call is a no-op — animation is a purely
    /// decorative layer and missing it does not affect correctness.
    pub(crate) fn animate_tab_switch(&mut self, split_id: LeafId, direction: i32) {
        if direction == 0 {
            return;
        }
        if !self.config.editor.animations {
            return;
        }
        let Some(area) = self.split_or_group_content_rect(split_id) else {
            return;
        };
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

    /// Resolve the on-screen Rect that covers the split `split_id` from
    /// the cached layout.
    ///
    /// Normally a split_id maps 1:1 to a single entry in
    /// `cached_layout.split_areas` (the split's content rect). When a
    /// buffer-group tab is active, however, the split renders the
    /// group's inner subtree — split_areas then has one entry per
    /// inner panel (log / detail / toolbar etc.) and NO entry for the
    /// outer split id. In that case we walk the stashed group subtree
    /// to collect every inner LeafId, look each one up in split_areas,
    /// and return the bounding box. That gives us the overall area the
    /// group occupies on screen.
    fn split_or_group_content_rect(&self, split_id: LeafId) -> Option<ratatui::layout::Rect> {
        if let Some(rect) = self
            .cached_layout
            .split_areas
            .iter()
            .find(|(sid, _, _, _, _, _)| *sid == split_id)
            .map(|(_, _, content_rect, _, _, _)| *content_rect)
        {
            return Some(rect);
        }

        // Fallback: is this split hosting a buffer-group tab? If so,
        // walk the group's inner subtree to collect its leaf ids and
        // union their cached content rects.
        let group_leaf = self
            .split_view_states
            .get(&split_id)
            .and_then(|vs| vs.active_group_tab)?;
        let subtree = self.grouped_subtrees.get(&group_leaf)?;

        let mut inner_leaves: Vec<LeafId> = Vec::new();
        collect_leaf_ids(subtree, &mut inner_leaves);

        let mut union: Option<ratatui::layout::Rect> = None;
        for (sid, _, content, _, _, _) in &self.cached_layout.split_areas {
            if !inner_leaves.contains(sid) {
                continue;
            }
            union = Some(match union {
                None => *content,
                Some(prev) => rect_union(prev, *content),
            });
        }
        union
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
