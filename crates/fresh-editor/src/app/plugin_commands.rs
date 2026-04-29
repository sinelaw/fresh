//! Plugin command handlers - extracted from the monolithic handle_plugin_command
//!
//! This module groups plugin commands by domain for better maintainability.

use crate::model::cursor::Cursors;
use crate::model::event::{BufferId, ContainerId, CursorId, Event, LeafId, OverlayFace, SplitId};
use crate::view::overlay::{OverlayHandle, OverlayNamespace};
use crate::view::split::SplitViewState;
use anyhow::Result as AnyhowResult;
use fresh_core::api::{
    GrepMatch, JsCallbackId, LayoutHints, MenuPosition, OverlayOptions, PluginResponse,
    ReplaceResult, ViewTransformPayload,
};

use super::Editor;

/// Directory names to always skip during project file walking.
const IGNORED_DIRS: &[&str] = &[
    ".git",
    "node_modules",
    "target",
    "__pycache__",
    ".hg",
    ".svn",
    ".DS_Store",
];

/// Build `FileSearchOptions` from the common grep parameters.
fn make_search_opts(
    fixed_string: bool,
    case_sensitive: bool,
    whole_words: bool,
    max_matches: usize,
) -> crate::model::filesystem::FileSearchOptions {
    crate::model::filesystem::FileSearchOptions {
        fixed_string,
        case_sensitive,
        whole_word: whole_words,
        max_matches,
    }
}

impl Editor {
    // ==================== Menu Helpers ====================

    // ==================== Overlay Commands ====================

    /// Handle AddOverlay command
    ///
    /// Colors can be RGB arrays or theme key strings. Theme keys are resolved
    /// at render time, so overlays update with theme changes.
    pub(super) fn handle_add_overlay(
        &mut self,
        buffer_id: BufferId,
        namespace: Option<OverlayNamespace>,
        range: std::ops::Range<usize>,
        options: OverlayOptions,
    ) {
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            let face = OverlayFace::from_options(options.clone());
            let event = Event::AddOverlay {
                namespace,
                range,
                face,
                priority: 10,
                message: None,
                extend_to_line_end: options.extend_to_line_end,
                url: options.url.clone(),
            };
            state.apply(&mut Cursors::default(), &event);
            // Note: Overlays are ephemeral, not added to event log for undo/redo

            // Request a re-render so overlays added asynchronously (e.g. after
            // an external process returns) become visible without requiring
            // the user to type or scroll.
            #[cfg(feature = "plugins")]
            {
                self.plugin_render_requested = true;
            }
        }
    }

    /// Handle RemoveOverlay command
    pub(super) fn handle_remove_overlay(&mut self, buffer_id: BufferId, handle: OverlayHandle) {
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            let event = Event::RemoveOverlay { handle };
            state.apply(&mut Cursors::default(), &event);
            // Note: Overlays are ephemeral, not added to event log for undo/redo
        }
    }

    /// Handle ClearAllOverlays command
    pub(super) fn handle_clear_all_overlays(&mut self, buffer_id: BufferId) {
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            // Use the OverlayManager's clear method
            state.overlays.clear(&mut state.marker_list);

            // Note: We don't add this to the event log because:
            // 1. Clearing overlays doesn't affect undo/redo (overlays are ephemeral)
            // 2. This is a plugin-initiated action, not a user edit
        }
    }

    /// Handle ClearNamespace command
    pub(super) fn handle_clear_namespace(
        &mut self,
        buffer_id: BufferId,
        namespace: OverlayNamespace,
    ) {
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state
                .overlays
                .clear_namespace(&namespace, &mut state.marker_list);
            // Note: Overlays are ephemeral, not added to event log for undo/redo
        }
    }

    /// Handle ClearOverlaysInRange command
    pub(super) fn handle_clear_overlays_in_range(
        &mut self,
        buffer_id: BufferId,
        start: usize,
        end: usize,
    ) {
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state
                .overlays
                .remove_in_range(&(start..end), &mut state.marker_list);
            // Note: Overlays are ephemeral, not added to event log for undo/redo
        }
    }

    // ==================== Virtual Text Commands ====================

    /// Handle AddVirtualText command
    #[allow(clippy::too_many_arguments)]
    pub(super) fn handle_add_virtual_text(
        &mut self,
        buffer_id: BufferId,
        virtual_text_id: String,
        position: usize,
        text: String,
        color: (u8, u8, u8),
        use_bg: bool,
        before: bool,
    ) {
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            use crate::view::virtual_text::VirtualTextPosition;
            use ratatui::style::{Color, Style};

            let vtext_position = if before {
                VirtualTextPosition::BeforeChar
            } else {
                VirtualTextPosition::AfterChar
            };

            let style = if use_bg {
                // For background colors, use the color as background with a space character
                Style::default().bg(Color::Rgb(color.0, color.1, color.2))
            } else {
                // For foreground colors, use the color as foreground
                Style::default().fg(Color::Rgb(color.0, color.1, color.2))
            };

            // Remove any existing virtual text with this ID first
            state
                .virtual_texts
                .remove_by_id(&mut state.marker_list, &virtual_text_id);

            // Add the new virtual text
            state.virtual_texts.add_with_id(
                &mut state.marker_list,
                position,
                text,
                style,
                vtext_position,
                0, // priority
                virtual_text_id,
            );
        }
    }

    /// Handle AddVirtualTextStyled — richer form that accepts theme
    /// keys for fg/bg and individual style modifiers.  Theme keys are
    /// resolved at render time so labels follow theme changes live.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn handle_add_virtual_text_styled(
        &mut self,
        buffer_id: BufferId,
        virtual_text_id: String,
        position: usize,
        text: String,
        fg: Option<fresh_core::api::OverlayColorSpec>,
        bg: Option<fresh_core::api::OverlayColorSpec>,
        bold: bool,
        italic: bool,
        before: bool,
    ) {
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            use crate::view::virtual_text::VirtualTextPosition;
            use fresh_core::api::OverlayColorSpec;
            use ratatui::style::{Color, Modifier, Style};

            let vtext_position = if before {
                VirtualTextPosition::BeforeChar
            } else {
                VirtualTextPosition::AfterChar
            };

            // Build a fallback style from any concrete RGB values; theme
            // keys are passed through separately so the renderer can
            // resolve them on each frame.
            let mut fallback = Style::default();
            let mut fg_theme_key: Option<String> = None;
            let mut bg_theme_key: Option<String> = None;
            match &fg {
                Some(OverlayColorSpec::Rgb(r, g, b)) => {
                    fallback = fallback.fg(Color::Rgb(*r, *g, *b));
                }
                Some(OverlayColorSpec::ThemeKey(k)) => {
                    fg_theme_key = Some(k.clone());
                }
                None => {}
            }
            match &bg {
                Some(OverlayColorSpec::Rgb(r, g, b)) => {
                    fallback = fallback.bg(Color::Rgb(*r, *g, *b));
                }
                Some(OverlayColorSpec::ThemeKey(k)) => {
                    bg_theme_key = Some(k.clone());
                }
                None => {}
            }
            if bold {
                fallback = fallback.add_modifier(Modifier::BOLD);
            }
            if italic {
                fallback = fallback.add_modifier(Modifier::ITALIC);
            }

            // Replace any existing virtual text with this ID.
            state
                .virtual_texts
                .remove_by_id(&mut state.marker_list, &virtual_text_id);

            state.virtual_texts.add_with_id_and_theme_keys(
                &mut state.marker_list,
                position,
                text,
                fallback,
                fg_theme_key,
                bg_theme_key,
                vtext_position,
                0, // priority
                virtual_text_id,
            );
        }
    }

    /// Handle RemoveVirtualText command
    pub(super) fn handle_remove_virtual_text(
        &mut self,
        buffer_id: BufferId,
        virtual_text_id: String,
    ) {
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state
                .virtual_texts
                .remove_by_id(&mut state.marker_list, &virtual_text_id);
        }
    }

    /// Handle RemoveVirtualTextsByPrefix command
    pub(super) fn handle_remove_virtual_texts_by_prefix(
        &mut self,
        buffer_id: BufferId,
        prefix: String,
    ) {
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state
                .virtual_texts
                .remove_by_prefix(&mut state.marker_list, &prefix);
        }
    }

    /// Handle ClearVirtualTexts command
    pub(super) fn handle_clear_virtual_texts(&mut self, buffer_id: BufferId) {
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state.virtual_texts.clear(&mut state.marker_list);
        }
    }

    /// Handle AddVirtualLine command
    ///
    /// Theme keys carried by the colour specs are NOT resolved here — they
    /// are stashed verbatim on the VirtualText so the renderer can resolve
    /// them against the live theme on every frame and the line follows
    /// theme changes without the plugin re-emitting it.  Only RGB colour
    /// specs and short colour names (`"Gray"`, `"DarkGray"`, …) are
    /// pre-baked into the fallback `Style`.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn handle_add_virtual_line(
        &mut self,
        buffer_id: BufferId,
        position: usize,
        text: String,
        fg_color: Option<fresh_core::api::OverlayColorSpec>,
        bg_color: Option<fresh_core::api::OverlayColorSpec>,
        above: bool,
        namespace: String,
        priority: i32,
    ) {
        use crate::view::theme::named_color_from_str;
        use crate::view::virtual_text::{VirtualTextNamespace, VirtualTextPosition};
        use fresh_core::api::OverlayColorSpec;
        use ratatui::style::{Color, Style};

        // Split a colour spec into "fallback colour baked into Style" and
        // "theme key resolved at render time".  Named colour strings are
        // recognised eagerly here so they end up in the fallback Style
        // (mirrors OverlayFace::from_options' policy).
        fn split(spec: Option<OverlayColorSpec>) -> (Option<Color>, Option<String>) {
            match spec {
                Some(OverlayColorSpec::Rgb(r, g, b)) => (Some(Color::Rgb(r, g, b)), None),
                Some(OverlayColorSpec::ThemeKey(key)) => {
                    if let Some(color) = named_color_from_str(&key) {
                        (Some(color), None)
                    } else {
                        (None, Some(key))
                    }
                }
                None => (None, None),
            }
        }

        let (fg_fallback, fg_theme_key) = split(fg_color);
        let (bg_fallback, bg_theme_key) = split(bg_color);

        let mut style = Style::default();
        if let Some(c) = fg_fallback {
            style = style.fg(c);
        }
        if let Some(c) = bg_fallback {
            style = style.bg(c);
        }

        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            let placement = if above {
                VirtualTextPosition::LineAbove
            } else {
                VirtualTextPosition::LineBelow
            };
            let ns = VirtualTextNamespace::from_string(namespace);

            state.virtual_texts.add_line_with_theme_keys(
                &mut state.marker_list,
                position,
                text,
                style,
                fg_theme_key,
                bg_theme_key,
                placement,
                ns,
                priority,
            );
        }
    }

    /// Handle ClearVirtualTextNamespace command
    pub(super) fn handle_clear_virtual_text_namespace(
        &mut self,
        buffer_id: BufferId,
        namespace: String,
    ) {
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            use crate::view::virtual_text::VirtualTextNamespace;
            let ns = VirtualTextNamespace::from_string(namespace);
            state
                .virtual_texts
                .clear_namespace(&mut state.marker_list, &ns);
        }
    }

    // ==================== Conceal Commands ====================

    /// Handle AddConceal command - add a conceal range that hides or replaces bytes
    pub(super) fn handle_add_conceal(
        &mut self,
        buffer_id: BufferId,
        namespace: OverlayNamespace,
        start: usize,
        end: usize,
        replacement: Option<String>,
    ) {
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state
                .conceals
                .add(&mut state.marker_list, namespace, start..end, replacement);
            #[cfg(feature = "plugins")]
            {
                self.plugin_render_requested = true;
            }
        }
    }

    /// Handle ClearConcealNamespace command
    pub(super) fn handle_clear_conceal_namespace(
        &mut self,
        buffer_id: BufferId,
        namespace: OverlayNamespace,
    ) {
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state
                .conceals
                .clear_namespace(&namespace, &mut state.marker_list);
        }
    }

    /// Handle ClearConcealsInRange command
    pub(super) fn handle_clear_conceals_in_range(
        &mut self,
        buffer_id: BufferId,
        start: usize,
        end: usize,
    ) {
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state
                .conceals
                .remove_in_range(&(start..end), &mut state.marker_list);
        }
    }

    // ==================== Fold Commands ====================

    /// Handle AddFold command — register a collapsed fold range for the
    /// given buffer. The fold lives on the buffer-view-state's
    /// FoldManager, which is keyed per-(split, buffer); we add it to
    /// every view state that currently shows the buffer (typically
    /// exactly one).
    pub(super) fn handle_add_fold(
        &mut self,
        buffer_id: BufferId,
        start: usize,
        end: usize,
        placeholder: Option<String>,
    ) {
        let Some(state) = self.buffers.get_mut(&buffer_id) else {
            return;
        };
        for vs in self.split_view_states.values_mut() {
            if vs.keyed_states.contains_key(&buffer_id) {
                let buf_state = vs.ensure_buffer_state(buffer_id);
                buf_state
                    .folds
                    .add(&mut state.marker_list, start, end, placeholder.clone());
            }
        }
        #[cfg(feature = "plugins")]
        {
            self.plugin_render_requested = true;
        }
    }

    /// Handle ClearFolds command — drop every collapsed fold range on
    /// the buffer (across all view states that host it).
    pub(super) fn handle_clear_folds(&mut self, buffer_id: BufferId) {
        let Some(state) = self.buffers.get_mut(&buffer_id) else {
            return;
        };
        for vs in self.split_view_states.values_mut() {
            if vs.keyed_states.contains_key(&buffer_id) {
                let buf_state = vs.ensure_buffer_state(buffer_id);
                buf_state.folds.clear(&mut state.marker_list);
            }
        }
        #[cfg(feature = "plugins")]
        {
            self.plugin_render_requested = true;
        }
    }

    // ==================== Soft Break Commands ====================

    /// Handle AddSoftBreak command
    pub(super) fn handle_add_soft_break(
        &mut self,
        buffer_id: BufferId,
        namespace: OverlayNamespace,
        position: usize,
        indent: u16,
    ) {
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state
                .soft_breaks
                .add(&mut state.marker_list, namespace, position, indent);
            #[cfg(feature = "plugins")]
            {
                self.plugin_render_requested = true;
            }
        }
    }

    /// Handle ClearSoftBreakNamespace command
    pub(super) fn handle_clear_soft_break_namespace(
        &mut self,
        buffer_id: BufferId,
        namespace: OverlayNamespace,
    ) {
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state
                .soft_breaks
                .clear_namespace(&namespace, &mut state.marker_list);
        }
    }

    /// Handle ClearSoftBreaksInRange command
    pub(super) fn handle_clear_soft_breaks_in_range(
        &mut self,
        buffer_id: BufferId,
        start: usize,
        end: usize,
    ) {
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state
                .soft_breaks
                .remove_in_range(start, end, &mut state.marker_list);
        }
    }

    // ==================== Menu Commands ====================

    /// Handle AddMenuItem command
    pub(super) fn handle_add_menu_item(
        &mut self,
        menu_label: String,
        item: crate::config::MenuItem,
        position: MenuPosition,
    ) {
        let inserted = self.with_menu_by_label(&menu_label, |menu| {
            let insert_idx = match position {
                MenuPosition::Top => 0,
                MenuPosition::Bottom => menu.items.len(),
                MenuPosition::Before(label) => menu
                    .items
                    .iter()
                    .position(|i| match i {
                        crate::config::MenuItem::Action { label: l, .. }
                        | crate::config::MenuItem::Submenu { label: l, .. } => l == &label,
                        _ => false,
                    })
                    .unwrap_or(menu.items.len()),
                MenuPosition::After(label) => menu
                    .items
                    .iter()
                    .position(|i| match i {
                        crate::config::MenuItem::Action { label: l, .. }
                        | crate::config::MenuItem::Submenu { label: l, .. } => l == &label,
                        _ => false,
                    })
                    .map(|i| i + 1)
                    .unwrap_or(menu.items.len()),
            };
            menu.items.insert(insert_idx, item);
            insert_idx
        });

        match inserted {
            Some(idx) => tracing::info!(
                "Added menu item to '{}' at position {}",
                menu_label,
                idx
            ),
            None => tracing::warn!("Menu '{}' not found for adding item", menu_label),
        }
    }

    /// Handle AddMenu command
    pub(super) fn handle_add_menu(&mut self, menu: crate::config::Menu, position: MenuPosition) {
        // Calculate insert index based on position
        let total_menus = self.menus.menus.len() + self.menu_state.plugin_menus.len();

        let insert_idx = match position {
            MenuPosition::Top => 0,
            MenuPosition::Bottom => total_menus,
            MenuPosition::Before(label) => {
                // Find in built-in menus first
                self.menus
                    .menus
                    .iter()
                    .position(|m| m.label == label)
                    .or_else(|| {
                        // Then in plugin menus (offset by built-in menus count)
                        self.menu_state
                            .plugin_menus
                            .iter()
                            .position(|m| m.label == label)
                            .map(|i| self.menus.menus.len() + i)
                    })
                    .unwrap_or(total_menus)
            }
            MenuPosition::After(label) => {
                // Find in built-in menus first
                self.menus
                    .menus
                    .iter()
                    .position(|m| m.label == label)
                    .map(|i| i + 1)
                    .or_else(|| {
                        // Then in plugin menus (offset by built-in menus count)
                        self.menu_state
                            .plugin_menus
                            .iter()
                            .position(|m| m.label == label)
                            .map(|i| self.menus.menus.len() + i + 1)
                    })
                    .unwrap_or(total_menus)
            }
        };

        // If inserting before built-in menus end, we can't actually insert into built-in menus
        // So we always add to plugin_menus, but position it logically
        // For now, just append to plugin_menus (they appear after built-in menus)
        let plugin_idx = if insert_idx >= self.menus.menus.len() {
            insert_idx - self.menus.menus.len()
        } else {
            // Can't insert before built-in menus, so put at start of plugin menus
            0
        };

        self.menu_state
            .plugin_menus
            .insert(plugin_idx.min(self.menu_state.plugin_menus.len()), menu);
        tracing::info!(
            "Added plugin menu at index {} (total menus: {})",
            plugin_idx,
            self.menus.menus.len() + self.menu_state.plugin_menus.len()
        );
    }

    /// Handle RemoveMenuItem command
    pub(super) fn handle_remove_menu_item(&mut self, menu_label: String, item_label: String) {
        let removed = self.with_menu_by_label(&menu_label, |menu| {
            let original_len = menu.items.len();
            menu.items.retain(|item| match item {
                crate::config::MenuItem::Action { label, .. }
                | crate::config::MenuItem::Submenu { label, .. } => label != &item_label,
                _ => true,
            });
            menu.items.len() < original_len
        });

        match removed {
            Some(true) => tracing::info!("Removed menu item '{}' from '{}'", item_label, menu_label),
            Some(false) => tracing::warn!("Menu item '{}' not found in '{}'", item_label, menu_label),
            None => tracing::warn!("Menu '{}' not found for removing item", menu_label),
        }
    }

    /// Handle RemoveMenu command
    pub(super) fn handle_remove_menu(&mut self, menu_label: String) {
        // Can only remove plugin menus, not config menus
        let original_len = self.menu_state.plugin_menus.len();
        self.menu_state
            .plugin_menus
            .retain(|m| m.label != menu_label);

        if self.menu_state.plugin_menus.len() < original_len {
            tracing::info!("Removed plugin menu '{}'", menu_label);
        } else {
            tracing::warn!(
                "Plugin menu '{}' not found (note: cannot remove config menus)",
                menu_label
            );
        }
    }

    // ==================== Split Commands ====================

    /// Handle FocusSplit command
    pub(super) fn handle_focus_split(&mut self, split_id: SplitId) {
        // Plugin sends arbitrary SplitId — convert to LeafId at the boundary
        let leaf_id = LeafId(split_id);
        // Get the buffer for this split
        if let Some(buffer_id) = self.split_manager.buffer_for_split(leaf_id) {
            self.focus_split(leaf_id, buffer_id);
            tracing::info!("Focused split {:?}", split_id);
        } else {
            tracing::warn!("Split {:?} not found", split_id);
        }
    }

    /// Handle SetSplitBuffer command
    pub(super) fn handle_set_split_buffer(&mut self, split_id: SplitId, buffer_id: BufferId) {
        // Verify the buffer exists
        if !self.buffers.contains_key(&buffer_id) {
            tracing::error!("Buffer {:?} not found for SetSplitBuffer", buffer_id);
            return;
        }

        // Plugin sends arbitrary SplitId — convert to LeafId at the boundary.
        // Go through set_pane_buffer so tree + SVS stay consistent (the
        // downstream view_state block tweaks open_buffers/view_transform
        // further, but the primitive is what keeps the invariant).
        let leaf_id = LeafId(split_id);
        self.set_pane_buffer(leaf_id, buffer_id);
        tracing::info!("Set split {:?} to buffer {:?}", split_id, buffer_id);

        // Switch per-buffer view state — the new buffer's own view_transform
        // and compose_width will be restored (or defaults if first time)
        if let Some(view_state) = self.split_view_states.get_mut(&leaf_id) {
            view_state.switch_buffer(buffer_id);
        }

        // If this is the active split, update active buffer with all side effects
        if self.split_manager.active_split() == leaf_id {
            self.set_active_buffer(buffer_id);
        }
    }

    /// Handle CloseSplit command
    pub(super) fn handle_close_split(&mut self, split_id: SplitId) {
        // Plugin sends arbitrary SplitId — convert to LeafId at the boundary
        let leaf_id = LeafId(split_id);
        match self.split_manager.close_split(leaf_id) {
            Ok(()) => {
                // Clean up the view state for the closed split
                self.split_view_states.remove(&leaf_id);
                tracing::info!("Closed split {:?}", split_id);
            }
            Err(e) => {
                tracing::warn!("Failed to close split {:?}: {}", split_id, e);
            }
        }
    }

    /// Handle SetSplitRatio command
    pub(super) fn handle_set_split_ratio(&mut self, split_id: SplitId, ratio: f32) {
        // Plugin sends arbitrary SplitId — convert to ContainerId at the boundary
        let container_id = ContainerId(split_id);
        self.split_manager.set_ratio(container_id, ratio);
        tracing::debug!("Set split {:?} ratio to {}", split_id, ratio);
    }

    /// Handle DistributeSplitsEvenly command
    pub(super) fn handle_distribute_splits_evenly(&mut self) {
        // The split_ids parameter is currently ignored - we distribute ALL splits evenly
        // A future enhancement could distribute only the specified splits
        self.split_manager.distribute_splits_evenly();
        tracing::debug!("Distributed splits evenly");
    }

    /// Handle SetBufferCursor command
    ///
    /// Walks both the main split tree (`split_manager.splits_for_buffer`) AND
    /// the inner leaves of all grouped subtrees stored in `grouped_subtrees`,
    /// mirroring `handle_scroll_buffer_to_line` — buffer-group panel buffers
    /// are not represented in `split_manager`'s tree, so the basic lookup
    /// returns nothing for them.
    pub(super) fn handle_set_buffer_cursor(&mut self, buffer_id: BufferId, position: usize) {
        // Find all splits that display this buffer (main tree + grouped subtrees).
        let mut splits: Vec<crate::app::LeafId> = self.split_manager.splits_for_buffer(buffer_id);
        for node in self.grouped_subtrees.values() {
            if let crate::view::split::SplitNode::Grouped { layout, .. } = node {
                for inner_leaf in layout.leaf_split_ids() {
                    if let Some(vs) = self.split_view_states.get(&inner_leaf) {
                        if vs.active_buffer == buffer_id && !splits.contains(&inner_leaf) {
                            splits.push(inner_leaf);
                        }
                    }
                }
            }
        }
        let active_split = self.split_manager.active_split();

        tracing::debug!(
            "SetBufferCursor: buffer_id={:?}, position={}, found {} splits: {:?}, active={:?}",
            buffer_id,
            position,
            splits.len(),
            splits,
            active_split
        );

        if splits.is_empty() {
            tracing::warn!("No splits found for buffer {:?}", buffer_id);
        }

        // Get the buffer for ensure_visible
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            for leaf_id in &splits {
                let is_active = *leaf_id == active_split;

                if let Some(view_state) = self.split_view_states.get_mut(leaf_id) {
                    // Set cursor position in the split's view state
                    view_state.cursors.primary_mut().move_to(position, false);
                    // Ensure the cursor is visible by scrolling the split's viewport
                    view_state.ensure_cursor_visible(&mut state.buffer, &state.marker_list);
                    tracing::debug!(
                        "SetBufferCursor: updated split {:?} (active={}) viewport top_byte={}",
                        leaf_id,
                        is_active,
                        view_state.viewport.top_byte
                    );

                    // Note: cursors and viewport are now owned by SplitViewState, no sync needed
                } else {
                    tracing::warn!(
                        "SetBufferCursor: split {:?} not found in split_view_states",
                        leaf_id
                    );
                }
            }
        } else {
            tracing::warn!("Buffer {:?} not found for SetBufferCursor", buffer_id);
        }
    }

    /// Handle SetSplitScroll command
    pub(super) fn handle_set_split_scroll(&mut self, split_id: SplitId, top_byte: usize) {
        // Plugin sends arbitrary SplitId — convert to LeafId at the boundary
        let leaf_id = LeafId(split_id);
        if let Some(view_state) = self.split_view_states.get_mut(&leaf_id) {
            // Get the buffer associated with this split to check bounds
            let buffer_id = if let Some(id) = self.split_manager.buffer_for_split(leaf_id) {
                id
            } else {
                tracing::warn!("SetSplitScroll: buffer for split {:?} not found", split_id);
                return;
            };

            if let Some(state) = self.buffers.get_mut(&buffer_id) {
                // Manually set top_byte, then perform validity check with scroll_to logic if needed,
                // or just clamp it. viewport.scroll_to takes a line number, not byte.
                // But viewport.top_byte is public.

                // Let's use set_top_byte_with_limit internal logic via a public helper or direct assignment
                // if we trust the plugin. But safer to ensure valid range.
                let max_byte = state.buffer.len();
                let clamped_byte = top_byte.min(max_byte);

                // We don't have direct access to set_top_byte_with_limit here easily without exposing it.
                // However, Viewport struct is in another crate (view::viewport).
                // Let's trust the Viewport's internal state management or just set it.
                // Viewport.top_byte is pub.

                view_state.viewport.top_byte = clamped_byte;
                // Also reset view line offset to 0 as we are setting absolute byte position
                view_state.viewport.top_view_line_offset = 0;
                // Skip ensure_visible so the scroll position isn't undone during render
                view_state.viewport.set_skip_ensure_visible();

                tracing::debug!(
                    "SetSplitScroll: split {:?} scrolled to byte {}",
                    split_id,
                    clamped_byte
                );
            }
        } else {
            tracing::warn!("SetSplitScroll: split {:?} not found", split_id);
        }
    }

    /// Handle RequestHighlights command
    pub(super) fn handle_request_highlights(
        &mut self,
        buffer_id: BufferId,
        range: std::ops::Range<usize>,
        request_id: u64,
    ) {
        let spans = if let Some(state) = self.buffers.get_mut(&buffer_id) {
            let spans = state.highlighter.highlight_viewport(
                &state.buffer,
                range.start,
                range.end,
                &self.theme,
                self.config.editor.highlight_context_bytes,
            );

            spans
                .into_iter()
                .map(|s| {
                    let color = match s.color {
                        ratatui::style::Color::Rgb(r, g, b) => (r, g, b),
                        _ => (128, 128, 128), // fallback for indexed colors
                    };
                    fresh_core::api::TsHighlightSpan {
                        start: s.range.start as u32,
                        end: s.range.end as u32,
                        color,
                        bold: false,
                        italic: false,
                    }
                })
                .collect()
        } else {
            vec![]
        };

        self.send_plugin_response(PluginResponse::HighlightsComputed { request_id, spans });
    }

    // ==================== Text Editing Commands ====================

    /// Handle InsertText command
    pub(super) fn handle_insert_text(
        &mut self,
        buffer_id: BufferId,
        position: usize,
        text: String,
    ) {
        let text_len = text.len();
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            let event = Event::Insert {
                position,
                text,
                cursor_id: CursorId(0),
            };
            // Apply to buffer with dummy cursors (real cursors adjusted below)
            state.apply(&mut Cursors::default(), &event);
            if let Some(log) = self.event_logs.get_mut(&buffer_id) {
                log.append(event);
            }
        }
        // Adjust cursors in all splits that display this buffer
        for leaf_id in self.split_manager.splits_for_buffer(buffer_id) {
            if let Some(view_state) = self.split_view_states.get_mut(&leaf_id) {
                view_state.cursors.adjust_for_edit(position, 0, text_len);
            }
        }
    }

    /// Handle DeleteRange command
    pub(super) fn handle_delete_range(
        &mut self,
        buffer_id: BufferId,
        range: std::ops::Range<usize>,
    ) {
        let delete_start = range.start;
        let delete_len = range.end.saturating_sub(range.start);
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            let deleted_text = state.get_text_range(range.start, range.end);
            let event = Event::Delete {
                range,
                deleted_text,
                cursor_id: CursorId(0),
            };
            // Apply to buffer with dummy cursors (real cursors adjusted below)
            state.apply(&mut Cursors::default(), &event);
            if let Some(log) = self.event_logs.get_mut(&buffer_id) {
                log.append(event);
            }
        }
        // Adjust cursors in all splits that display this buffer
        for leaf_id in self.split_manager.splits_for_buffer(buffer_id) {
            if let Some(view_state) = self.split_view_states.get_mut(&leaf_id) {
                view_state
                    .cursors
                    .adjust_for_edit(delete_start, delete_len, 0);
            }
        }
    }

    /// Handle InsertAtCursor command
    pub(super) fn handle_insert_at_cursor(&mut self, text: String) {
        // Read cursor position first to avoid borrow conflicts
        let cursor_pos = self.active_cursors().primary().position;
        let event = Event::Insert {
            position: cursor_pos,
            text,
            cursor_id: CursorId(0),
        };
        // Borrow cursors and state simultaneously from different parts of self
        {
            let split_id = self.split_manager.active_split();
            let active_buf = self.active_buffer();
            let cursors = &mut self.split_view_states.get_mut(&split_id).unwrap().cursors;
            let state = self.buffers.get_mut(&active_buf).unwrap();
            state.apply(cursors, &event);
        }
        self.active_event_log_mut().append(event);
    }

    /// Handle DeleteSelection command
    pub(super) fn handle_delete_selection(&mut self) {
        // Get deletions from cursors (now in SplitViewState)
        let deletions: Vec<_> = {
            self.active_cursors()
                .iter()
                .filter_map(|(_, c)| c.selection_range())
                .collect()
        };

        if !deletions.is_empty() {
            // Get deleted text and cursor id
            let primary_id = self.active_cursors().primary_id();
            let state = self.active_state_mut();
            let events: Vec<_> = deletions
                .iter()
                .rev()
                .map(|range| {
                    let deleted_text = state.get_text_range(range.start, range.end);
                    Event::Delete {
                        range: range.clone(),
                        deleted_text,
                        cursor_id: primary_id,
                    }
                })
                .collect();

            // Apply events
            for event in events {
                self.log_and_apply_event(&event);
            }
        }
    }

    // ==================== File/Navigation Commands ====================

    /// Helper to jump to a line/column position in the active buffer
    pub(super) fn jump_to_line_column(&mut self, line: Option<usize>, column: Option<usize>) {
        // Convert 1-indexed line/column to byte position
        let target_line = line.unwrap_or(1).saturating_sub(1); // Convert to 0-indexed
        let column_offset = column.unwrap_or(1).saturating_sub(1); // Convert to 0-indexed

        let state = self.active_state_mut();
        let mut iter = state.buffer.line_iterator(0, 80);
        let mut target_byte = 0;

        // Iterate through lines until we reach the target
        for current_line in 0..=target_line {
            if let Some((line_start, _)) = iter.next_line() {
                if current_line == target_line {
                    target_byte = line_start;
                    break;
                }
            } else {
                // Reached end of buffer before target line
                break;
            }
        }

        // Add the column offset to position within the line
        // Column offset is byte offset from line start (matching git grep --column behavior)
        let final_position = target_byte + column_offset;

        // Ensure we don't go past the buffer end
        let buffer_len = state.buffer.len();
        let clamped_position = final_position.min(buffer_len);

        // Update the cached line number so the status bar shows the correct
        // position. Without this, the status bar reads a stale value from
        // state.primary_cursor_line_number which was set before the jump.
        state.primary_cursor_line_number = crate::model::buffer::LineNumber::Absolute(target_line);

        // Funnel through the navigation primitive so the cursor is guaranteed
        // visible in the viewport (#1689 — without this, jump_to_line_column
        // could land off-screen if a prior scroll set skip_ensure_visible).
        self.jump_active_cursor_to(
            clamped_position,
            super::navigation::JumpOptions::navigation(),
        );
    }

    /// Handle OpenFileAtLocation command
    pub(super) fn handle_open_file_at_location(
        &mut self,
        path: std::path::PathBuf,
        line: Option<usize>,
        column: Option<usize>,
    ) -> AnyhowResult<()> {
        // Open the file (may switch to an already-open buffer)
        if let Err(e) = self.open_file(&path) {
            tracing::error!("Failed to open file from plugin: {}", e);
            return Ok(());
        }

        // If line/column specified, jump to that location
        if line.is_some() || column.is_some() {
            self.jump_to_line_column(line, column);
        }
        Ok(())
    }

    /// Handle OpenFileInSplit command
    pub(super) fn handle_open_file_in_split(
        &mut self,
        split_id: usize,
        path: std::path::PathBuf,
        line: Option<usize>,
        column: Option<usize>,
    ) -> AnyhowResult<()> {
        // Switch to the target split
        let target_split_id = LeafId(SplitId(split_id));
        if !self.split_manager.set_active_split(target_split_id) {
            tracing::error!("Failed to switch to split {}", split_id);
            return Ok(());
        }

        // Open the file in the now-active split
        if let Err(e) = self.open_file(&path) {
            tracing::error!("Failed to open file from plugin: {}", e);
            return Ok(());
        }

        // Jump to the specified location (or default to start)
        self.jump_to_line_column(line, column);
        Ok(())
    }

    /// Handle OpenFileInBackground command
    pub(super) fn handle_open_file_in_background(&mut self, path: std::path::PathBuf) {
        // Open file in a new tab without switching to it
        if let Err(e) = self.open_file_no_focus(&path) {
            tracing::error!("Failed to open file in background: {}", e);
        } else {
            tracing::info!("Opened file in background: {:?}", path);
        }
    }

    /// Handle ShowBuffer command.
    ///
    /// If `buffer_id` belongs to a buffer group (i.e., it's one of the group's
    /// panel buffers), this activates the group's tab and focuses that panel
    /// instead of clobbering the current split's leaf with the panel buffer —
    /// which would bypass the group-tab dispatch path and break rendering.
    pub(super) fn handle_show_buffer(&mut self, buffer_id: BufferId) {
        if !self.buffers.contains_key(&buffer_id) {
            tracing::warn!("Buffer {:?} not found", buffer_id);
            return;
        }

        // If this buffer belongs to a group, route through the group's tab.
        if let Some(&group_id) = self.buffer_to_group.get(&buffer_id) {
            // Find the panel name for this buffer in the group, then focus it.
            let panel_name = self.buffer_groups.get(&group_id).and_then(|g| {
                g.panel_buffers
                    .iter()
                    .find_map(|(name, &bid)| (bid == buffer_id).then(|| name.clone()))
            });
            if let Some(panel_name) = panel_name {
                self.focus_panel(group_id.0, panel_name);
                tracing::info!(
                    "Switched to group panel buffer {:?} via group {:?}",
                    buffer_id,
                    group_id
                );
                return;
            }
        }

        self.set_active_buffer(buffer_id);
        tracing::info!("Switched to buffer {:?}", buffer_id);
    }

    /// Handle CloseBuffer command
    pub(super) fn handle_close_buffer(&mut self, buffer_id: BufferId) {
        match self.close_buffer(buffer_id) {
            Ok(()) => {
                tracing::info!("Closed buffer {:?}", buffer_id);
            }
            Err(e) => {
                tracing::error!("Failed to close buffer {:?}: {}", buffer_id, e);
            }
        }
    }

    // ==================== View/Layout Commands ====================

    /// Handle SetLayoutHints command
    ///
    /// Targets `buffer_id`'s state in the resolved split, not the split's
    /// active buffer. Plugins call this asynchronously, so by the time the
    /// command is drained the focused buffer may have changed; binding to
    /// `buffer_id` keeps the hint with the buffer the plugin chose.
    pub(super) fn handle_set_layout_hints(
        &mut self,
        buffer_id: BufferId,
        split_id: Option<SplitId>,
        hints: LayoutHints,
    ) {
        let target_split = split_id
            .map(LeafId)
            .unwrap_or(self.split_manager.active_split());
        let view_state = self
            .split_view_states
            .entry(target_split)
            .or_insert_with(|| {
                SplitViewState::with_buffer(self.terminal_width, self.terminal_height, buffer_id)
            });
        let buf_state = view_state.ensure_buffer_state(buffer_id);
        buf_state.compose_width = hints.compose_width;
        buf_state.compose_column_guides = hints.column_guides;
    }

    /// Handle SetViewMode command
    pub(super) fn handle_set_view_mode(&mut self, buffer_id: BufferId, mode: &str) {
        use crate::state::ViewMode;
        let view_mode = match mode {
            "page_view" | "compose" => ViewMode::PageView,
            _ => ViewMode::Source,
        };
        // Set on the specified buffer's per-split view state.
        // Use buffer_id to target the correct buffer (not just the active one)
        // so that "toggle compose all" can affect non-active buffers.
        let active_split = self.split_manager.active_split();
        if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
            if let Some(buf_state) = view_state.buffer_state_mut(buffer_id) {
                buf_state.view_mode = view_mode;
            } else {
                // Buffer not yet in this split — fall back to setting on active
                view_state.view_mode = view_mode;
            }
        }
    }

    /// Handle SetViewState command — persist plugin state in BufferViewState
    pub(super) fn handle_set_view_state(
        &mut self,
        buffer_id: BufferId,
        key: String,
        value: Option<serde_json::Value>,
    ) {
        let active_split = self.split_manager.active_split();
        if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
            let buf_state = view_state.ensure_buffer_state(buffer_id);
            match value {
                Some(v) => {
                    buf_state.plugin_state.insert(key, v);
                }
                None => {
                    buf_state.plugin_state.remove(&key);
                }
            }
        }
    }

    /// Handle SetGlobalState command — persist plugin-level global state
    pub(super) fn handle_set_global_state(
        &mut self,
        plugin_name: String,
        key: String,
        value: Option<serde_json::Value>,
    ) {
        match value {
            Some(v) => {
                self.plugin_global_state
                    .entry(plugin_name)
                    .or_default()
                    .insert(key, v);
            }
            None => {
                if let Some(map) = self.plugin_global_state.get_mut(&plugin_name) {
                    map.remove(&key);
                    if map.is_empty() {
                        self.plugin_global_state.remove(&plugin_name);
                    }
                }
            }
        }
    }

    /// Handle SetLineNumbers command
    ///
    /// Sets line number visibility on the specified buffer's per-split view state,
    /// so that different splits showing the same buffer can have independent
    /// line number settings (e.g., source mode shows line numbers, compose hides them).
    pub(super) fn handle_set_line_numbers(&mut self, buffer_id: BufferId, enabled: bool) {
        let active_split = self.split_manager.active_split();
        if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
            if let Some(buf_state) = view_state.buffer_state_mut(buffer_id) {
                buf_state.show_line_numbers = enabled;
            } else {
                // Buffer not yet in this split — fall back to setting on active
                view_state.show_line_numbers = enabled;
            }
        }
    }

    /// Handle SetLineWrap command
    pub(super) fn handle_set_line_wrap(
        &mut self,
        _buffer_id: BufferId,
        split_id: Option<SplitId>,
        enabled: bool,
    ) {
        let target_split = split_id
            .map(LeafId)
            .unwrap_or(self.split_manager.active_split());
        if let Some(view_state) = self.split_view_states.get_mut(&target_split) {
            view_state.viewport.line_wrap_enabled = enabled;
        }
    }

    /// Handle SubmitViewTransform command
    pub(super) fn handle_submit_view_transform(
        &mut self,
        buffer_id: BufferId,
        split_id: Option<SplitId>,
        payload: ViewTransformPayload,
    ) {
        let target_split = split_id
            .map(LeafId)
            .unwrap_or(self.split_manager.active_split());
        let view_state = self
            .split_view_states
            .entry(target_split)
            .or_insert_with(|| {
                SplitViewState::with_buffer(self.terminal_width, self.terminal_height, buffer_id)
            });
        // Reject stale view transforms — the buffer was edited since the
        // view_transform_request that produced this response, so the token
        // source_offsets are from before the edit. Applying them would cause
        // conceals to appear at wrong positions for one frame (flicker).
        if view_state.view_transform_stale {
            tracing::trace!(
                "Rejecting stale SubmitViewTransform for split {:?}",
                target_split
            );
            return;
        }
        view_state.view_transform = Some(payload);
    }

    /// Handle ClearViewTransform command
    pub(super) fn handle_clear_view_transform(&mut self, split_id: Option<SplitId>) {
        let target_split = split_id
            .map(LeafId)
            .unwrap_or(self.split_manager.active_split());
        if let Some(view_state) = self.split_view_states.get_mut(&target_split) {
            view_state.view_transform = None;
            view_state.compose_width = None;
        }
    }

    /// Handle RefreshAllLines command — clear seen_byte_ranges for every buffer
    /// so the lines_changed hook re-fires for all visible content.
    /// Called when a plugin registers for the lines_changed hook to handle the
    /// race where render marks lines as "seen" before the plugin has initialized.
    pub(super) fn handle_refresh_all_lines(&mut self) {
        self.seen_byte_ranges.clear();
        #[cfg(feature = "plugins")]
        {
            self.plugin_render_requested = true;
        }
    }

    /// Handle RefreshLines command
    pub(super) fn handle_refresh_lines(&mut self, buffer_id: BufferId) {
        // Clear seen_byte_ranges for this buffer so all visible lines will be re-processed
        // on the next render. This is useful when a plugin is enabled and needs to
        // process lines that were already marked as seen.
        self.seen_byte_ranges.remove(&buffer_id);
        // Request a render so the lines_changed hook fires
        #[cfg(feature = "plugins")]
        {
            self.plugin_render_requested = true;
        }
    }

    /// Handle SetLineIndicator command
    pub(super) fn handle_set_line_indicator(
        &mut self,
        buffer_id: BufferId,
        line: usize,
        namespace: String,
        symbol: String,
        color: (u8, u8, u8),
        priority: i32,
    ) {
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            // Convert line number to byte offset for marker-based tracking
            let byte_offset = state.buffer.line_start_offset(line).unwrap_or(0);
            let indicator = crate::view::margin::LineIndicator::new(
                symbol,
                ratatui::style::Color::Rgb(color.0, color.1, color.2),
                priority,
            );
            state
                .margins
                .set_line_indicator(byte_offset, namespace, indicator);
        }
    }

    /// Handle SetLineIndicators batch command
    pub(super) fn handle_set_line_indicators(
        &mut self,
        buffer_id: BufferId,
        lines: Vec<usize>,
        namespace: String,
        symbol: String,
        color: (u8, u8, u8),
        priority: i32,
    ) {
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            let indicator = crate::view::margin::LineIndicator::new(
                symbol,
                ratatui::style::Color::Rgb(color.0, color.1, color.2),
                priority,
            );
            for line in lines {
                let byte_offset = state.buffer.line_start_offset(line).unwrap_or(0);
                state
                    .margins
                    .set_line_indicator(byte_offset, namespace.clone(), indicator.clone());
            }
        }
    }

    /// Handle ClearLineIndicators command
    pub(super) fn handle_clear_line_indicators(&mut self, buffer_id: BufferId, namespace: String) {
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state
                .margins
                .clear_line_indicators_for_namespace(&namespace);
        }
    }

    // ==================== Status/Prompt Commands ====================

    /// Handle SetStatus command
    pub(super) fn handle_set_status(&mut self, message: String) {
        if message.trim().is_empty() {
            self.plugin_status_message = None;
        } else {
            // Log status message for history
            tracing::info!(target: "status", "{}", message);
            // Detect plugin errors and collect them for test assertions
            // Error patterns: "Plugin error", "JS error", "handler error"
            let lower = message.to_lowercase();
            if lower.contains("plugin error")
                || lower.contains("js error")
                || lower.contains("handler error")
                || lower.contains("error in")
            {
                self.plugin_errors.push(message.clone());
            }
            // Clear core status message so only plugin message shows
            self.status_message = None;
            self.plugin_status_message = Some(message.clone());
        }
    }

    /// Handle StartPrompt command
    pub(super) fn handle_start_prompt(&mut self, label: String, prompt_type: String) {
        // Create a plugin-controlled prompt
        use crate::view::prompt::{Prompt, PromptType};
        self.prompt = Some(Prompt::new(
            label,
            PromptType::Plugin {
                custom_type: prompt_type.clone(),
            },
        ));

        // Fire the prompt_changed hook immediately with empty input
        // This allows plugins to initialize the prompt state
        use crate::services::plugins::hooks::HookArgs;
        self.plugin_manager.run_hook(
            "prompt_changed",
            HookArgs::PromptChanged {
                prompt_type: prompt_type.clone(),
                input: String::new(),
            },
        );
    }

    /// Handle StartPromptWithInitial command
    pub(super) fn handle_start_prompt_with_initial(
        &mut self,
        label: String,
        prompt_type: String,
        initial_value: String,
    ) {
        // Create a plugin-controlled prompt with initial text
        use crate::view::prompt::{Prompt, PromptType};
        self.prompt = Some(Prompt::with_initial_text(
            label,
            PromptType::Plugin {
                custom_type: prompt_type.clone(),
            },
            initial_value.clone(),
        ));

        // Fire the prompt_changed hook immediately with the initial value
        use crate::services::plugins::hooks::HookArgs;
        self.plugin_manager.run_hook(
            "prompt_changed",
            HookArgs::PromptChanged {
                prompt_type: prompt_type.clone(),
                input: initial_value,
            },
        );
    }

    /// Handle StartPromptAsync command (for editor.prompt() API)
    pub(super) fn handle_start_prompt_async(
        &mut self,
        label: String,
        initial_value: String,
        callback_id: fresh_core::api::JsCallbackId,
    ) {
        // Store the callback for resolution when prompt completes
        self.pending_async_prompt_callback = Some(callback_id);

        // Create an async prompt (uses special prompt type)
        use crate::view::prompt::{Prompt, PromptType};
        self.prompt = Some(Prompt::with_initial_text(
            label,
            PromptType::AsyncPrompt,
            initial_value.clone(),
        ));

        // Fire the prompt_changed hook
        use crate::services::plugins::hooks::HookArgs;
        self.plugin_manager.run_hook(
            "prompt_changed",
            HookArgs::PromptChanged {
                prompt_type: "async_prompt".to_string(),
                input: initial_value,
            },
        );
    }

    /// Handle SetPromptSuggestions command
    pub(super) fn handle_set_prompt_suggestions(
        &mut self,
        suggestions: Vec<fresh_core::command::Suggestion>,
    ) {
        use crate::input::commands::{CommandSource, Suggestion as EditorSuggestion};

        // Convert from plugin API suggestions to internal suggestions
        let internal_suggestions: Vec<EditorSuggestion> = suggestions
            .into_iter()
            .map(|s| {
                let source = s.source.map(|src| match src {
                    fresh_core::command::CommandSource::Builtin => CommandSource::Builtin,
                    fresh_core::command::CommandSource::Plugin(name) => CommandSource::Plugin(name),
                });
                EditorSuggestion {
                    text: s.text,
                    description: s.description,
                    value: s.value,
                    disabled: s.disabled.unwrap_or(false),
                    keybinding: s.keybinding,
                    source,
                }
            })
            .collect();

        if let Some(prompt) = &mut self.prompt {
            // Set original_suggestions for Rust-side filtering (used by prompts that
            // don't handle their own filtering like theme editor dropdowns)
            prompt.original_suggestions = Some(internal_suggestions.clone());
            prompt.suggestions = internal_suggestions;
            // Select first suggestion by default
            prompt.selected_suggestion = if prompt.suggestions.is_empty() {
                None
            } else {
                Some(0)
            };
            // Track that suggestions were set for this input value.
            // If filter_suggestions is called with the same input, we skip filtering
            // because the plugin has already provided filtered results.
            prompt.suggestions_set_for_input = Some(prompt.input.clone());
        }
    }

    // ==================== Command/Mode Registration ====================

    /// Handle RegisterCommand command
    pub(super) fn handle_register_command(&self, command: fresh_core::command::Command) {
        use crate::input::commands::{Command as EditorCommand, CommandSource};
        use crate::input::keybindings::Action;

        // Convert from plugin API command to internal command
        let internal_command = EditorCommand {
            name: command.name.clone(),
            description: command.description,
            action: Action::PluginAction(command.action_name),
            contexts: vec![], // Plugin commands available in all contexts by default
            custom_contexts: command.custom_contexts,
            source: CommandSource::Plugin(command.plugin_name),
        };

        tracing::debug!(
            "handle_register_command: name='{}', action={:?}",
            internal_command.name,
            internal_command.action
        );
        self.command_registry
            .read()
            .unwrap()
            .register(internal_command);
    }

    /// Handle UnregisterCommand command
    pub(super) fn handle_unregister_command(&self, name: String) {
        self.command_registry.read().unwrap().unregister(&name);
    }

    /// Handle DefineMode command
    pub(super) fn handle_define_mode(
        &mut self,
        name: String,
        bindings: Vec<(String, String)>,
        read_only: bool,
        allow_text_input: bool,
        inherit_normal_bindings: bool,
        plugin_name: Option<String>,
    ) {
        use super::parse_key_string;
        use crate::input::buffer_mode::BufferMode;
        use crate::input::keybindings::{Action, KeyContext};

        let mode = BufferMode::new(name.clone())
            .with_read_only(read_only)
            .with_allow_text_input(allow_text_input)
            .with_inherit_normal_bindings(inherit_normal_bindings)
            .with_plugin_name(plugin_name);

        // Clear any existing plugin defaults for this mode before re-registering
        {
            let mut kb = self.keybindings.write().unwrap();
            kb.clear_plugin_defaults_for_mode(&name);
            kb.set_mode_inherits_normal_bindings(&name, inherit_normal_bindings);
        }

        let mode_context = KeyContext::Mode(name.clone());

        // Parse key bindings from strings
        // Key strings can be single keys ("g", "C-f") or chord sequences ("g g", "z z")
        for (key_str, command) in &bindings {
            let parts: Vec<&str> = key_str.split_whitespace().collect();

            if parts.len() == 1 {
                // Single key binding
                if let Some((code, modifiers)) = parse_key_string(key_str) {
                    let action = Action::from_str(command, &std::collections::HashMap::new())
                        .unwrap_or_else(|| Action::PluginAction(command.clone()));
                    self.keybindings.write().unwrap().load_plugin_default(
                        mode_context.clone(),
                        code,
                        modifiers,
                        action,
                    );
                } else {
                    tracing::warn!("Failed to parse key binding: {}", key_str);
                }
            } else {
                // Chord sequence (multiple keys separated by space)
                let mut sequence = Vec::new();
                let mut parse_failed = false;

                for part in &parts {
                    if let Some((code, modifiers)) = parse_key_string(part) {
                        sequence.push((code, modifiers));
                    } else {
                        tracing::warn!("Failed to parse key in chord: {} (in {})", part, key_str);
                        parse_failed = true;
                        break;
                    }
                }

                if !parse_failed && !sequence.is_empty() {
                    tracing::debug!("Adding chord binding: {:?} -> {}", sequence, command);
                    let action = Action::from_str(command, &std::collections::HashMap::new())
                        .unwrap_or_else(|| Action::PluginAction(command.clone()));
                    self.keybindings.write().unwrap().load_plugin_chord_default(
                        mode_context.clone(),
                        sequence,
                        action,
                    );
                }
            }
        }

        self.mode_registry.register(mode);

        // Update keybinding labels in plugin state snapshot for getKeybindingLabel API
        #[cfg(feature = "plugins")]
        {
            if let Some(snapshot_handle) = self.plugin_manager.state_snapshot_handle() {
                if let Ok(mut snapshot) = snapshot_handle.write() {
                    // Remove old labels for this mode
                    snapshot
                        .keybinding_labels
                        .retain(|k, _| !k.ends_with(&format!("\0{}", name)));
                    // Add current labels from plugin defaults in KeybindingResolver
                    let keybindings_read = self.keybindings.read().unwrap();
                    if let Some(mode_bindings) =
                        keybindings_read.get_plugin_defaults().get(&mode_context)
                    {
                        for (key_code, modifiers) in mode_bindings.keys() {
                            let label =
                                crate::input::keybindings::format_keybinding(key_code, modifiers);
                            if let Some((_key_str, cmd)) = bindings
                                .iter()
                                .find(|(k, _)| parse_key_string(k) == Some((*key_code, *modifiers)))
                            {
                                let key = format!("{}\0{}", cmd, name);
                                snapshot.keybinding_labels.insert(key, label);
                            }
                        }
                    }
                }
            }
        }

        tracing::info!("Registered buffer mode '{}'", name);
    }

    // ==================== LSP Commands ====================

    /// Handle SendLspRequest command
    pub(super) fn handle_send_lsp_request(
        &mut self,
        language: String,
        method: String,
        params: Option<serde_json::Value>,
        request_id: u64,
    ) {
        tracing::debug!(
            "Plugin LSP request {} for language '{}': method={}",
            request_id,
            language,
            method
        );
        let error = if let Some(lsp) = self.lsp.as_mut() {
            // Respect auto_start setting for plugin requests
            use crate::services::lsp::manager::LspSpawnResult;
            if lsp.try_spawn(&language, None) != LspSpawnResult::Spawned {
                Some(format!(
                    "LSP server for '{}' is not running (auto_start disabled)",
                    language
                ))
            } else if let Some(handle) = lsp.get_handle_mut(&language) {
                handle.send_plugin_request(request_id, method, params).err()
            } else {
                Some(format!("LSP server for '{}' is unavailable", language))
            }
        } else {
            Some("LSP manager not initialized".to_string())
        };
        if let Some(err_msg) = error {
            self.plugin_manager
                .reject_callback(fresh_core::api::JsCallbackId::from(request_id), err_msg);
        }
    }

    // ==================== Clipboard Commands ====================

    /// Handle SetClipboard command
    pub(super) fn handle_set_clipboard(&mut self, text: String) {
        self.clipboard.copy(text);
    }

    // ==================== Language Pack Commands ====================

    /// Handle RegisterGrammar command
    /// Adds a grammar to the pending list until reload_grammars() is called
    pub(super) fn handle_register_grammar(
        &mut self,
        language: String,
        grammar_path: String,
        extensions: Vec<String>,
    ) {
        use super::PendingGrammar;
        self.pending_grammars.push(PendingGrammar {
            language: language.clone(),
            grammar_path,
            extensions,
        });
        tracing::info!(
            "Grammar registered for '{}' (call reload_grammars to apply)",
            language
        );
    }

    /// Handle RegisterLanguageConfig command
    /// Applies language configuration immediately to runtime config
    pub(super) fn handle_register_language_config(
        &mut self,
        language: String,
        config: fresh_core::api::LanguagePackConfig,
    ) {
        // Convert LanguagePackConfig to the internal LanguageConfig format
        let lang_config = crate::config::LanguageConfig {
            comment_prefix: config.comment_prefix,
            auto_indent: config.auto_indent.unwrap_or(true),
            use_tabs: config.use_tabs,
            tab_size: config.tab_size,
            show_whitespace_tabs: config.show_whitespace_tabs.unwrap_or(true),
            formatter: config.formatter.map(|f| crate::config::FormatterConfig {
                command: f.command,
                args: f.args,
                stdin: true,       // Default: read from stdin
                timeout_ms: 10000, // Default: 10 second timeout
            }),
            ..Default::default()
        };
        self.config_mut()
            .languages
            .insert(language.clone(), lang_config);
        tracing::info!("Language config registered for '{}'", language);
    }

    /// Handle RegisterLspServer command
    /// Applies LSP server configuration immediately
    pub(super) fn handle_register_lsp_server(
        &mut self,
        language: String,
        config: fresh_core::api::LspServerPackConfig,
    ) {
        // Convert LspServerPackConfig to the internal LspServerConfig format
        let process_limits = match config.process_limits {
            Some(pl) => crate::types::ProcessLimits {
                max_memory_percent: pl.max_memory_percent,
                max_cpu_percent: pl.max_cpu_percent,
                enabled: pl
                    .enabled
                    .unwrap_or(pl.max_memory_percent.is_some() || pl.max_cpu_percent.is_some()),
            },
            None => Default::default(),
        };
        let lsp_config = crate::types::LspServerConfig {
            command: config.command,
            args: config.args,
            enabled: true, // Explicitly enable - Default::default() gives false
            auto_start: config.auto_start.unwrap_or(true),
            initialization_options: config.initialization_options,
            process_limits,
            ..Default::default()
        };
        // Update LSP manager if available
        if let Some(ref mut lsp) = self.lsp {
            lsp.set_language_config(language.clone(), lsp_config.clone());
        }
        // Also update runtime config
        self.config_mut().lsp.insert(
            language.clone(),
            crate::types::LspLanguageConfig::Multi(vec![lsp_config]),
        );
        tracing::info!("LSP server registered for '{}'", language);
    }

    /// Handle ReloadGrammars command
    /// Defers the actual rebuild — sets a flag so all pending grammars from the
    /// current command batch are collected before a single rebuild.
    /// The callback_id will be resolved when the background build completes.
    pub(super) fn handle_reload_grammars(&mut self, callback_id: fresh_core::api::JsCallbackId) {
        tracing::debug!(
            "ReloadGrammars requested, pending_grammars count: {}",
            self.pending_grammars.len()
        );
        self.grammar_reload_pending = true;
        self.pending_grammar_callbacks.push(callback_id);
    }

    /// Flush pending grammars: spawn a background rebuild if any ReloadGrammars
    /// commands were received during this command batch.
    ///
    /// Called after processing all plugin commands in a batch, so that multiple
    /// RegisterGrammar+ReloadGrammars pairs result in only one rebuild.
    /// The rebuild happens on a background thread; when complete, a
    /// `GrammarRegistryBuilt` message swaps in the new registry.
    ///
    /// On the first call, this triggers the deferred full grammar build
    /// (user grammars + language packs + any plugin grammars accumulated so far).
    pub(super) fn flush_pending_grammars(&mut self) {
        // On the first call, start the deferred full grammar build.
        // This includes any plugin grammars that were registered during init,
        // so we get everything in a single builder.build() pass.
        if self.needs_full_grammar_build {
            self.needs_full_grammar_build = false;
            self.grammar_reload_pending = false;

            // Drain all pending grammars to include in the initial build
            let additional: Vec<_> = self
                .pending_grammars
                .drain(..)
                .map(|g| crate::primitives::grammar::GrammarSpec {
                    language: g.language.clone(),
                    path: std::path::PathBuf::from(g.grammar_path),
                    extensions: g.extensions.clone(),
                })
                .collect();

            // Update config.languages with the extensions so detect_language() works
            for crate::primitives::grammar::GrammarSpec {
                language,
                extensions,
                ..
            } in &additional
            {
                let lang_config = self
                    .config_mut()
                    .languages
                    .entry(language.clone())
                    .or_default();
                for ext in extensions {
                    if !lang_config.extensions.contains(ext) {
                        lang_config.extensions.push(ext.clone());
                    }
                }
            }

            let callback_ids: Vec<_> = self.pending_grammar_callbacks.drain(..).collect();
            self.start_background_grammar_build(additional, callback_ids);
            return;
        }

        if !self.grammar_reload_pending {
            return;
        }
        self.grammar_reload_pending = false;

        // If a background build is already in progress, it will call
        // flush_pending_grammars() again when it completes — so just
        // re-arm the flag and return.
        if self.grammar_build_in_progress {
            self.grammar_reload_pending = true;
            tracing::debug!("Grammar build in progress, deferring flush");
            return;
        }

        use std::path::PathBuf;

        if self.pending_grammars.is_empty() {
            tracing::debug!("Grammar reload requested but no pending grammars");
            return;
        }

        // Deduplicate: skip grammars whose extensions are all already mapped
        // in the current registry (meaning the grammar was already loaded by
        // for_editor or a previous build).
        let pending_before = self.pending_grammars.len();
        self.pending_grammars.retain(|g| {
            // Check if ALL extensions for this grammar are already mapped
            let all_mapped = !g.extensions.is_empty()
                && g.extensions
                    .iter()
                    .all(|ext| self.grammar_registry.find_by_extension(ext).is_some());
            if all_mapped {
                tracing::debug!(
                    "Skipping already-loaded grammar '{}' (extensions {:?} already mapped)",
                    g.language,
                    g.extensions
                );
                false
            } else {
                true
            }
        });
        if pending_before != self.pending_grammars.len() {
            tracing::info!(
                "Deduplicated pending grammars: {} -> {}",
                pending_before,
                self.pending_grammars.len()
            );
        }

        if self.pending_grammars.is_empty() {
            tracing::info!(
                "All pending grammars already loaded, resolving callbacks without rebuild"
            );
            // Resolve callbacks immediately — no rebuild needed
            #[cfg(feature = "plugins")]
            for cb_id in self.pending_grammar_callbacks.drain(..) {
                self.plugin_manager
                    .resolve_callback(cb_id, "null".to_string());
            }
            #[cfg(not(feature = "plugins"))]
            self.pending_grammar_callbacks.clear();
            return;
        }

        tracing::info!(
            "Flushing {} pending grammars via background rebuild",
            self.pending_grammars.len()
        );

        // Collect pending grammars
        let additional: Vec<crate::primitives::grammar::GrammarSpec> = self
            .pending_grammars
            .drain(..)
            .map(|g| crate::primitives::grammar::GrammarSpec {
                language: g.language.clone(),
                path: PathBuf::from(g.grammar_path),
                extensions: g.extensions.clone(),
            })
            .collect();

        // Update config.languages with the extensions so detect_language() works
        for crate::primitives::grammar::GrammarSpec {
            language,
            extensions,
            ..
        } in &additional
        {
            let lang_config = self
                .config_mut()
                .languages
                .entry(language.clone())
                .or_default();
            for ext in extensions {
                if !lang_config.extensions.contains(ext) {
                    lang_config.extensions.push(ext.clone());
                }
            }
        }

        // Collect pending callback IDs to resolve when build completes
        let callback_ids: Vec<_> = self.pending_grammar_callbacks.drain(..).collect();

        // Spawn background rebuild
        let base_registry = std::sync::Arc::clone(&self.grammar_registry);
        if let Some(bridge) = &self.async_bridge {
            let sender = bridge.sender();
            self.grammar_build_in_progress = true;
            std::thread::Builder::new()
                .name("grammar-rebuild".to_string())
                .spawn(move || {
                    use crate::primitives::grammar::GrammarRegistry;
                    match GrammarRegistry::with_additional_grammars(&base_registry, &additional) {
                        Some(new_registry) => {
                            // Ok to ignore: receiver may be gone if app is shutting down.
                            drop(sender.send(
                                crate::services::async_bridge::AsyncMessage::GrammarRegistryBuilt {
                                    registry: std::sync::Arc::new(new_registry),
                                    callback_ids,
                                },
                            ));
                        }
                        None => {
                            tracing::error!("Failed to rebuild grammar registry in background");
                            // Still send the message so callbacks get resolved (even on failure)
                            drop(sender.send(
                                crate::services::async_bridge::AsyncMessage::GrammarRegistryBuilt {
                                    registry: base_registry,
                                    callback_ids,
                                },
                            ));
                        }
                    }
                })
                .ok();
        }
    }

    // ==================== Project Grep ====================

    /// Handle GrepProject command: walk files, search buffers/disk, collect matches
    pub(super) fn handle_grep_project(
        &mut self,
        pattern: String,
        fixed_string: bool,
        case_sensitive: bool,
        max_results: usize,
        whole_words: bool,
        callback_id: JsCallbackId,
    ) {
        if pattern.is_empty() {
            let json = serde_json::to_string(&Vec::<GrepMatch>::new())
                .unwrap_or_else(|_| "[]".to_string());
            self.plugin_manager.resolve_callback(callback_id, json);
            return;
        }

        // Build search options for FileSystem::search_file
        let fs_opts = make_search_opts(fixed_string, case_sensitive, whole_words, max_results);

        // Build regex for open buffer searches (piece tree path still needs it)
        let regex = match crate::model::filesystem::build_search_regex(&pattern, &fs_opts) {
            Ok(re) => re,
            Err(e) => {
                self.plugin_manager
                    .reject_callback(callback_id, format!("Invalid regex: {}", e));
                return;
            }
        };

        let query_len = pattern.len();
        let mut results: Vec<GrepMatch> = Vec::new();

        // Build a map of open buffer paths -> BufferId
        let mut open_buffer_paths: std::collections::HashMap<std::path::PathBuf, BufferId> =
            std::collections::HashMap::new();
        for (bid, state) in &self.buffers {
            if let Some(path) = state.buffer.file_path() {
                open_buffer_paths.insert(path.to_path_buf(), *bid);
            }
        }

        // Collect all project files via FileSystem trait (works for both local and remote)
        let cwd = self.working_dir.clone();
        let cancel = std::sync::atomic::AtomicBool::new(false);
        let mut file_paths: Vec<std::path::PathBuf> = Vec::new();
        if let Err(e) =
            self.authority
                .filesystem
                .walk_files(&cwd, IGNORED_DIRS, &cancel, &mut |path, _rel| {
                    file_paths.push(path.to_path_buf());
                    true
                })
        {
            tracing::warn!("walk_files failed: {}", e);
        }

        // Search each file: open buffers via piece tree, others via fs.search_file
        for file_path in &file_paths {
            if results.len() >= max_results {
                break;
            }
            let remaining = max_results - results.len();

            if let Some(&bid) = open_buffer_paths.get(file_path) {
                // Search the open buffer — hybrid search uses fs.search_file
                // for unloaded regions (avoids transferring large files)
                if let Some(state) = self.buffers.get_mut(&bid) {
                    let matches = match state.buffer.search_hybrid(
                        &pattern,
                        &fs_opts,
                        regex.clone(),
                        remaining,
                        query_len,
                    ) {
                        Ok(m) => m,
                        Err(_) => continue,
                    };
                    let file_str = file_path.to_string_lossy().to_string();
                    for m in &matches {
                        results.push(GrepMatch {
                            file: file_str.clone(),
                            buffer_id: bid.0,
                            byte_offset: m.byte_offset,
                            length: m.length,
                            line: m.line,
                            column: m.column,
                            context: m.context.clone(),
                        });
                    }
                }
            } else {
                // Not open — search via FileSystem trait
                let fs_opts_file =
                    make_search_opts(fixed_string, case_sensitive, whole_words, remaining);
                let mut cursor = crate::model::filesystem::FileSearchCursor::new();
                let mut file_matches = Vec::new();
                while !cursor.done && file_matches.len() < remaining {
                    match self.authority.filesystem.search_file(
                        file_path,
                        &pattern,
                        &fs_opts_file,
                        &mut cursor,
                    ) {
                        Ok(batch) => file_matches.extend(batch),
                        Err(_) => break,
                    }
                }
                if file_matches.is_empty() {
                    continue;
                }
                let file_str = file_path.to_string_lossy().to_string();
                for m in file_matches {
                    results.push(GrepMatch {
                        file: file_str.clone(),
                        buffer_id: 0,
                        byte_offset: m.byte_offset,
                        length: m.length,
                        line: m.line,
                        column: m.column,
                        context: m.context,
                    });
                }
            }
        }

        let json = serde_json::to_string(&results).unwrap_or_else(|_| "[]".to_string());
        self.plugin_manager.resolve_callback(callback_id, json);
    }

    // ==================== Streaming Grep ====================

    /// Handle GrepProjectStreaming: parallel, non-blocking search with incremental results.
    ///
    /// - Snapshots dirty buffers on the main thread (piece tree isn't Send)
    /// - Spawns a tokio task that walks the directory tree and fans out file searches
    /// - Each file's matches are sent back immediately via AsyncBridge
    /// - Supports cancellation via AtomicBool when a new search starts
    #[allow(clippy::too_many_arguments)]
    pub(super) fn handle_grep_project_streaming(
        &mut self,
        pattern: String,
        fixed_string: bool,
        case_sensitive: bool,
        max_results: usize,
        whole_words: bool,
        search_id: u64,
        callback_id: JsCallbackId,
    ) {
        // Cancel any previous streaming search
        if let Some(prev_cancel) = self.streaming_grep_cancellation.take() {
            prev_cancel.store(true, std::sync::atomic::Ordering::Relaxed);
        }

        tracing::info!(
            "handle_grep_project_streaming: pattern={:?} search_id={} has_runtime={}",
            pattern,
            search_id,
            self.tokio_runtime.is_some()
        );

        // Handle empty pattern
        if pattern.is_empty() {
            self.plugin_manager.resolve_callback(
                callback_id,
                format!(r#"{{"searchId":{},"totalMatches":0}}"#, search_id),
            );
            return;
        }

        // Build search options and validate regex on main thread (catches errors early)
        let fs_opts = make_search_opts(fixed_string, case_sensitive, whole_words, max_results);
        // Build regex for dirty buffer snapshots (piece tree path still needs it)
        let regex = match crate::model::filesystem::build_search_regex(&pattern, &fs_opts) {
            Ok(re) => re,
            Err(e) => {
                self.plugin_manager
                    .reject_callback(callback_id, format!("Invalid regex: {}", e));
                return;
            }
        };

        // Extract hybrid search plans for dirty buffers on the main thread.
        // This only copies the small loaded/edited regions — unloaded regions
        // are represented as file range coordinates, avoiding full-file transfer.
        let mut dirty_plans: std::collections::HashMap<
            std::path::PathBuf,
            (BufferId, crate::model::buffer::HybridSearchPlan),
        > = std::collections::HashMap::new();
        for (bid, state) in &mut self.buffers {
            if let Some(path) = state.buffer.file_path().map(|p| p.to_path_buf()) {
                if state.buffer.is_modified() {
                    if let Some(plan) = state.buffer.search_hybrid_plan() {
                        dirty_plans.insert(path, (*bid, plan));
                    }
                }
            }
        }

        // Set up cancellation
        let cancel = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false));
        self.streaming_grep_cancellation = Some(cancel.clone());

        let filesystem = self.authority.filesystem.clone();
        let filesystem_walker = self.authority.filesystem.clone();
        let cwd = self.working_dir.clone();
        let query_len = pattern.len();

        let Some(bridge) = &self.async_bridge else {
            self.plugin_manager
                .reject_callback(callback_id, "No async bridge available".to_string());
            return;
        };
        let sender = bridge.sender();

        let Some(runtime) = &self.tokio_runtime else {
            self.plugin_manager
                .reject_callback(callback_id, "No tokio runtime available".to_string());
            return;
        };

        runtime.spawn(async move {
            // Channel from walker to searchers
            let (path_tx, mut path_rx) = tokio::sync::mpsc::channel::<std::path::PathBuf>(256);

            let cancel_walker = cancel.clone();

            // Walker task: recursively walks via FileSystem trait (works local and remote)
            tokio::task::spawn_blocking(move || {
                tracing::info!(
                    "GrepStreaming walker: starting from {:?} search_id={}",
                    cwd,
                    search_id
                );
                let mut file_count = 0usize;

                if let Err(e) = filesystem_walker.walk_files(
                    &cwd,
                    IGNORED_DIRS,
                    &cancel_walker,
                    &mut |path, _rel| {
                        file_count += 1;
                        path_tx.blocking_send(path.to_path_buf()).is_ok()
                    },
                ) {
                    tracing::warn!("GrepStreaming walk_files failed: {}", e);
                }

                tracing::info!(
                    "GrepStreaming walker: done, sent {} files (search_id={})",
                    file_count,
                    search_id
                );
                // path_tx dropped here, signalling completion to consumers
            });

            // Searcher coordinator: reads from channel, spawns parallel searchers
            let semaphore = std::sync::Arc::new(tokio::sync::Semaphore::new(8));
            let match_count = std::sync::Arc::new(std::sync::atomic::AtomicUsize::new(0));

            // Collect join handles so we can wait for all searchers to finish
            let mut handles: Vec<tokio::task::JoinHandle<()>> = Vec::new();

            while let Some(file_path) = path_rx.recv().await {
                if cancel.load(std::sync::atomic::Ordering::Relaxed) {
                    break;
                }
                if match_count.load(std::sync::atomic::Ordering::Relaxed) >= max_results {
                    break;
                }

                let permit = match semaphore.clone().acquire_owned().await {
                    Ok(p) => p,
                    Err(_) => break,
                };

                let fs = filesystem.clone();
                let sender = sender.clone();
                let cancel = cancel.clone();
                let match_count = match_count.clone();
                let regex = regex.clone();
                let pattern = pattern.clone();
                let fs_opts = fs_opts.clone();
                let dirty_plan = dirty_plans.remove(&file_path);

                let handle = tokio::task::spawn_blocking(move || {
                    let _permit = permit;

                    if cancel.load(std::sync::atomic::Ordering::Relaxed) {
                        return;
                    }

                    let current_count = match_count.load(std::sync::atomic::Ordering::Relaxed);
                    if current_count >= max_results {
                        return;
                    }
                    let remaining = max_results - current_count;

                    if let Some((bid, plan)) = dirty_plan {
                        // Dirty buffer — execute hybrid search plan (searches
                        // unloaded regions via fs.search_file, loaded in memory)
                        let matches = match plan
                            .execute(&*fs, &pattern, &fs_opts, &regex, remaining, query_len)
                        {
                            Ok(m) => m,
                            Err(e) => {
                                tracing::debug!(
                                    "GrepProjectStreaming: hybrid search failed {:?}: {}",
                                    file_path,
                                    e
                                );
                                return;
                            }
                        };

                        if !matches.is_empty() {
                            let file_str = file_path.to_string_lossy().to_string();
                            let file_matches: Vec<GrepMatch> = matches
                                .iter()
                                .map(|m| GrepMatch {
                                    file: file_str.clone(),
                                    buffer_id: bid.0,
                                    byte_offset: m.byte_offset,
                                    length: m.length,
                                    line: m.line,
                                    column: m.column,
                                    context: m.context.clone(),
                                })
                                .collect();
                            match_count.fetch_add(
                                file_matches.len(),
                                std::sync::atomic::Ordering::Relaxed,
                            );
                            let json = serde_json::to_string(&file_matches)
                                .unwrap_or_else(|_| "[]".to_string());
                            drop(
                                sender.send(crate::services::async_bridge::AsyncMessage::Plugin(
                                    fresh_core::api::PluginAsyncMessage::GrepStreamingProgress {
                                        search_id,
                                        matches_json: json,
                                    },
                                )),
                            );
                        }
                    } else {
                        // Search via FileSystem trait
                        let fs_opts = crate::model::filesystem::FileSearchOptions {
                            fixed_string,
                            case_sensitive,
                            whole_word: whole_words,
                            max_matches: remaining,
                        };
                        let mut cursor = crate::model::filesystem::FileSearchCursor::new();
                        while !cursor.done {
                            if cancel.load(std::sync::atomic::Ordering::Relaxed) {
                                break;
                            }
                            let current = match_count.load(std::sync::atomic::Ordering::Relaxed);
                            if current >= max_results {
                                break;
                            }

                            let batch =
                                match fs.search_file(&file_path, &pattern, &fs_opts, &mut cursor) {
                                    Ok(b) => b,
                                    Err(e) => {
                                        tracing::debug!(
                                            "search_file failed {:?}: {}",
                                            file_path,
                                            e
                                        );
                                        break;
                                    }
                                };
                            if batch.is_empty() {
                                continue;
                            }

                            match_count
                                .fetch_add(batch.len(), std::sync::atomic::Ordering::Relaxed);
                            let file_str = file_path.to_string_lossy().to_string();
                            let file_matches: Vec<GrepMatch> = batch
                                .into_iter()
                                .map(|m| GrepMatch {
                                    file: file_str.clone(),
                                    buffer_id: 0,
                                    byte_offset: m.byte_offset,
                                    length: m.length,
                                    line: m.line,
                                    column: m.column,
                                    context: m.context,
                                })
                                .collect();
                            let json = serde_json::to_string(&file_matches)
                                .unwrap_or_else(|_| "[]".to_string());
                            drop(
                                sender.send(crate::services::async_bridge::AsyncMessage::Plugin(
                                    fresh_core::api::PluginAsyncMessage::GrepStreamingProgress {
                                        search_id,
                                        matches_json: json,
                                    },
                                )),
                            );
                        }
                    }
                });

                handles.push(handle);
            }

            // Wait for all searchers to complete
            tracing::info!(
                "GrepStreaming coordinator: waiting for {} searchers",
                handles.len()
            );
            for handle in handles {
                drop(handle.await);
            }

            let total = match_count.load(std::sync::atomic::Ordering::Relaxed);
            let truncated = total >= max_results;
            tracing::info!(
                "GrepStreaming coordinator: complete, total_matches={}, truncated={}",
                total,
                truncated
            );
            drop(
                sender.send(crate::services::async_bridge::AsyncMessage::Plugin(
                    fresh_core::api::PluginAsyncMessage::GrepStreamingComplete {
                        search_id,
                        callback_id: callback_id.as_u64(),
                        total_matches: total,
                        truncated,
                    },
                )),
            );
        });
    }

    // ==================== Replace In Buffer ====================

    /// Handle ReplaceInBuffer: open file if needed, apply edits, save
    pub(super) fn handle_replace_in_buffer(
        &mut self,
        file_path: std::path::PathBuf,
        matches: Vec<(usize, usize)>,
        replacement: String,
        callback_id: JsCallbackId,
    ) {
        if matches.is_empty() {
            let result = ReplaceResult {
                replacements: 0,
                buffer_id: 0,
            };
            let json = serde_json::to_string(&result).unwrap_or_else(|_| "null".to_string());
            self.plugin_manager.resolve_callback(callback_id, json);
            return;
        }

        // Find or open the buffer for this file
        let buffer_id = if let Some((&bid, _)) = self
            .buffers
            .iter()
            .find(|(_, state)| state.buffer.file_path() == Some(&file_path))
        {
            bid
        } else {
            // Open the file — creates a buffer via FileSystem trait
            match self.open_file_no_focus(&file_path) {
                Ok(bid) => {
                    // Mark as hidden from tabs so it doesn't clutter the UI
                    if let Some(meta) = self.buffer_metadata.get_mut(&bid) {
                        meta.hidden_from_tabs = true;
                    }
                    // `open_file_no_focus` unconditionally attaches the new
                    // buffer as a tab to the preferred split.  When we're
                    // running as a side effect of the Search/Replace panel,
                    // the preferred split may be the panel's split (or any
                    // normal split), which then carries a phantom tab for
                    // this "hidden" buffer.  Close-Buffer on the panel would
                    // then fall through to that tab instead of closing the
                    // whole split.  Strip the buffer from every split's tab
                    // list so only the panel split holds the panel buffer.
                    for view_state in self.split_view_states.values_mut() {
                        view_state.remove_buffer(bid);
                    }
                    bid
                }
                Err(e) => {
                    self.plugin_manager.reject_callback(
                        callback_id,
                        format!("Failed to open file {:?}: {}", file_path, e),
                    );
                    return;
                }
            }
        };

        // Sort matches by byte offset descending — editing from end backwards
        // prevents earlier edits from shifting later offsets
        let mut sorted_matches = matches;
        sorted_matches.sort_by(|a, b| b.0.cmp(&a.0));

        // Build bulk edits: (start, del_len, replacement)
        let edits: Vec<(usize, usize, &str)> = sorted_matches
            .iter()
            .map(|&(offset, len)| (offset, len, replacement.as_str()))
            .collect();

        let replacements = edits.len();

        // Owned tuples for helpers that don't take references.
        let edits_owned: Vec<(usize, usize, String)> = sorted_matches
            .iter()
            .map(|&(offset, len)| (offset, len, replacement.clone()))
            .collect();
        // Merged edit-lengths list for marker/margin replay on undo/redo.
        // Mirrors the merging logic in `apply_events_as_bulk_edit`.
        let edit_lengths: Vec<(usize, usize, usize)> = {
            let mut lengths: Vec<(usize, usize, usize)> = Vec::new();
            for (pos, del_len, text) in &edits_owned {
                if let Some(last) = lengths.last_mut() {
                    if last.0 == *pos {
                        last.1 += del_len;
                        last.2 += text.len();
                        continue;
                    }
                }
                lengths.push((*pos, *del_len, text.len()));
            }
            lengths
        };

        // Apply edits and capture pre/post snapshots so the replace is undoable
        // via the standard event log machinery.  Project replace has no
        // meaningful cursor positions to restore on undo, so we pass empty
        // cursor lists.
        let mut saved_path: Option<std::path::PathBuf> = None;
        let bulk_edit_event = if let Some(state) = self.buffers.get_mut(&buffer_id) {
            let old_snapshot = state.buffer.snapshot_buffer_state();
            let displaced_markers = state.capture_displaced_markers_bulk(&edits_owned);

            // Apply all edits as a single bulk operation
            state.buffer.apply_bulk_edits(&edits);

            // Adjust markers and margins to track the edits, matching the
            // logic used by interactive multi-cursor edits.
            for &(pos, del_len, ins_len) in &edit_lengths {
                if del_len > 0 && ins_len > 0 {
                    if ins_len > del_len {
                        state.marker_list.adjust_for_insert(pos, ins_len - del_len);
                        state.margins.adjust_for_insert(pos, ins_len - del_len);
                    } else if del_len > ins_len {
                        state.marker_list.adjust_for_delete(pos, del_len - ins_len);
                        state.margins.adjust_for_delete(pos, del_len - ins_len);
                    }
                } else if del_len > 0 {
                    state.marker_list.adjust_for_delete(pos, del_len);
                    state.margins.adjust_for_delete(pos, del_len);
                } else if ins_len > 0 {
                    state.marker_list.adjust_for_insert(pos, ins_len);
                    state.margins.adjust_for_insert(pos, ins_len);
                }
            }

            state.highlighter.invalidate_all();

            let new_snapshot = state.buffer.snapshot_buffer_state();

            // Save the buffer via the FileSystem trait.  Capture the path
            // into the outer `saved_path` so we can refresh the watched
            // mtime after dropping the &mut self borrow on `state` —
            // otherwise the auto-revert poller sees the new mtime, treats
            // it as an external change, and reverts the buffer from disk,
            // wiping the event log we're about to append (see bug #1).
            if let Some(path) = state.buffer.file_path().map(|p| p.to_path_buf()) {
                if let Err(e) = state.buffer.save_to_file(&path) {
                    self.plugin_manager.reject_callback(
                        callback_id,
                        format!("Failed to save file {:?}: {}", path, e),
                    );
                    return;
                }
                saved_path = Some(path);
            }

            Some(Event::BulkEdit {
                old_snapshot: Some(old_snapshot),
                new_snapshot: Some(new_snapshot),
                old_cursors: Vec::new(),
                new_cursors: Vec::new(),
                description: format!(
                    "Project replace ({} replacement{})",
                    replacements,
                    if replacements == 1 { "" } else { "s" }
                ),
                edits: edit_lengths,
                displaced_markers,
            })
        } else {
            None
        };

        // Refresh the watched mtime for the just-saved file so the
        // auto-revert poller does NOT treat our own save as an external
        // change.  Without this, `handle_file_changed` → `revert_buffer_by_id`
        // would run and reset the event log we're about to append to,
        // making the replace silently un-undoable (bug #1).
        if let Some(ref path) = saved_path {
            self.watch_file(path);
        }

        // Record the BulkEdit on the buffer's event log so Undo can revert it.
        if let Some(event) = bulk_edit_event {
            if let Some(event_log) = self.event_logs.get_mut(&buffer_id) {
                event_log.append(event);
                // The file on disk is now in the post-replace state, so mark
                // this position as "saved".  Otherwise `saved_at_index` would
                // stay at its pre-replace value and undo (which moves
                // `current_index` backwards) would land on the old saved
                // position and `update_modified_from_event_log` would clear
                // the modified flag — leaving the user with a reverted buffer
                // that looks clean even though disk still has the XYZ
                // content.  We want the tab to show `a.txt*` after undo.
                event_log.mark_saved();
            }
            self.invalidate_layouts_for_buffer(buffer_id);

            // Notify LSP with full document content (bulk edits collapse
            // incremental ranges).
            let full_content_change = self
                .buffers
                .get(&buffer_id)
                .and_then(|s| s.buffer.to_string())
                .map(|text| {
                    vec![lsp_types::TextDocumentContentChangeEvent {
                        range: None,
                        range_length: None,
                        text,
                    }]
                })
                .unwrap_or_default();
            if !full_content_change.is_empty() {
                self.send_lsp_changes_for_buffer(buffer_id, full_content_change);
            }
        }

        let result = ReplaceResult {
            replacements,
            buffer_id: buffer_id.0,
        };
        let json = serde_json::to_string(&result).unwrap_or_else(|_| "null".to_string());
        self.plugin_manager.resolve_callback(callback_id, json);
    }

    /// Handle StartAnimationArea: translate the plugin description into an
    /// AnimationKind and start it at the given Rect with the plugin's ID.
    pub(super) fn handle_start_animation_area(
        &mut self,
        id: u64,
        rect: fresh_core::api::AnimationRect,
        kind: fresh_core::api::PluginAnimationKind,
    ) {
        if !self.config.editor.animations {
            return;
        }
        let area = ratatui::layout::Rect::new(rect.x, rect.y, rect.width, rect.height);
        if area.width == 0 || area.height == 0 {
            return;
        }
        let animation_kind = translate_plugin_animation_kind(kind);
        self.animations.start_with_id(
            crate::view::animation::AnimationId::from_raw(id),
            area,
            animation_kind,
        );
    }

    /// Handle StartAnimationVirtualBuffer: resolve the virtual buffer's
    /// current on-screen Rect, then delegate to `handle_start_animation_area`.
    /// If the rect isn't in the cached split layout yet (common when the
    /// buffer was just created and no render pass has placed it), the
    /// request is queued and drained at the top of the next render pass
    /// once `split_areas` has been recomputed.
    pub(super) fn handle_start_animation_virtual_buffer(
        &mut self,
        id: u64,
        buffer_id: BufferId,
        kind: fresh_core::api::PluginAnimationKind,
    ) {
        if !self.config.editor.animations {
            return;
        }
        match self.virtual_buffer_screen_rect(buffer_id) {
            Some(area) => {
                let animation_kind = translate_plugin_animation_kind(kind);
                self.animations.start_with_id(
                    crate::view::animation::AnimationId::from_raw(id),
                    area,
                    animation_kind,
                );
            }
            None => {
                tracing::debug!(
                    "animate_virtual_buffer: buffer {:?} not yet on screen, deferring",
                    buffer_id
                );
                self.pending_vb_animations.push((id, buffer_id, kind));
            }
        }
    }

    /// Retry deferred virtual-buffer animations now that split_areas has
    /// been recomputed. Called from render() after layout but before
    /// animations.apply_all so the first frame of the effect lands in
    /// the same render pass.
    pub(crate) fn drain_pending_vb_animations(&mut self) {
        if self.pending_vb_animations.is_empty() {
            return;
        }
        let pending = std::mem::take(&mut self.pending_vb_animations);
        for (id, buffer_id, kind) in pending {
            match self.virtual_buffer_screen_rect(buffer_id) {
                Some(area) => {
                    let animation_kind = translate_plugin_animation_kind(kind);
                    self.animations.start_with_id(
                        crate::view::animation::AnimationId::from_raw(id),
                        area,
                        animation_kind,
                    );
                }
                None => {
                    // Still not visible; keep pending for next frame.
                    self.pending_vb_animations.push((id, buffer_id, kind));
                }
            }
        }
    }

    /// Look up the on-screen Rect currently occupied by `buffer_id`, if any.
    /// Reads from the cached split layout captured in the last render pass.
    pub(crate) fn virtual_buffer_screen_rect(
        &self,
        buffer_id: BufferId,
    ) -> Option<ratatui::layout::Rect> {
        self.cached_layout
            .split_areas
            .iter()
            .find(|(_, bid, _, _, _, _)| *bid == buffer_id)
            .map(|(_, _, content_rect, _, _, _)| *content_rect)
    }
}

/// Translate the plugin-facing animation description to the internal
/// `AnimationKind` the runner consumes.
fn translate_plugin_animation_kind(
    kind: fresh_core::api::PluginAnimationKind,
) -> crate::view::animation::AnimationKind {
    use crate::view::animation::{AnimationKind, Edge};
    use fresh_core::api::{PluginAnimationEdge, PluginAnimationKind};
    use std::time::Duration;
    match kind {
        PluginAnimationKind::SlideIn {
            from,
            duration_ms,
            delay_ms,
        } => AnimationKind::SlideIn {
            from: match from {
                PluginAnimationEdge::Top => Edge::Top,
                PluginAnimationEdge::Bottom => Edge::Bottom,
                PluginAnimationEdge::Left => Edge::Left,
                PluginAnimationEdge::Right => Edge::Right,
            },
            duration: Duration::from_millis(duration_ms as u64),
            delay: Duration::from_millis(delay_ms as u64),
        },
    }
}

#[cfg(test)]
mod tests {
    use crate::app::Editor;
    use crate::config::Config;
    use crate::config_io::DirectoryContext;
    use fresh_core::api::LayoutHints;
    use std::sync::Arc;
    use tempfile::TempDir;

    fn make_editor() -> (Editor, TempDir) {
        let config = Config::default();
        let temp_dir = TempDir::new().unwrap();
        let dir_context = DirectoryContext::for_testing(temp_dir.path());
        let fs: Arc<dyn crate::model::filesystem::FileSystem + Send + Sync> =
            Arc::new(crate::model::filesystem::StdFileSystem);
        let editor = Editor::new(
            config,
            80,
            24,
            dir_context,
            crate::view::color_support::ColorCapability::TrueColor,
            fs,
        )
        .unwrap();
        (editor, temp_dir)
    }

    /// Plugin sends `setLayoutHints(targetBufferId, …)` for buffer X while
    /// buffer Y is active in the split. The compose_width must land on X
    /// (the targeted buffer), not on Y. Without the fix, `view_state` derefs
    /// to the active buffer's `BufferViewState`, so Y receives the width and
    /// renders centered without anything ever asking for it on Y.
    #[test]
    fn handle_set_layout_hints_targets_specified_buffer_not_active() {
        let (mut editor, _temp) = make_editor();

        let initial_buf = editor.active_buffer();
        let other_buf = editor.new_buffer();
        // new_buffer makes `other_buf` active; switch back so initial_buf is active
        // and `other_buf` is the non-active target the plugin wants to reach.
        editor.switch_buffer(initial_buf);
        assert_eq!(editor.active_buffer(), initial_buf);

        // Plugin call: target the non-active buffer.
        editor.handle_set_layout_hints(
            other_buf,
            None,
            LayoutHints {
                compose_width: Some(80),
                column_guides: None,
            },
        );

        let active_split = editor.split_manager.active_split();
        let view_state = editor
            .split_view_states
            .get(&active_split)
            .expect("split view state");

        let other_state = view_state
            .buffer_state(other_buf)
            .expect("other buffer keyed in split");
        assert_eq!(
            other_state.compose_width,
            Some(80),
            "compose_width must land on the targeted buffer",
        );

        let active_state = view_state
            .buffer_state(initial_buf)
            .expect("active buffer keyed in split");
        assert_eq!(
            active_state.compose_width, None,
            "compose_width must NOT land on the (different) active buffer",
        );
    }
}
