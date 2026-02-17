//! Plugin command handlers - extracted from the monolithic handle_plugin_command
//!
//! This module groups plugin commands by domain for better maintainability.

use crate::model::cursor::Cursors;
use crate::model::event::{BufferId, CursorId, Event, OverlayFace, SplitId};
use crate::view::overlay::{OverlayHandle, OverlayNamespace};
use crate::view::split::SplitViewState;
use anyhow::Result as AnyhowResult;
use fresh_core::api::{
    LayoutHints, MenuPosition, OverlayOptions, PluginResponse, ViewTransformPayload,
};

use super::Editor;

impl Editor {
    // ==================== Menu Helpers ====================

    /// Find a menu by label, searching built-in menus first then plugin menus.
    fn find_menu_by_label_mut(&mut self, label: &str) -> Option<&mut crate::config::Menu> {
        // Check built-in menus first
        if let Some(menu) = self.menus.menus.iter_mut().find(|m| m.label == label) {
            return Some(menu);
        }
        // Then check plugin menus
        self.menu_state
            .plugin_menus
            .iter_mut()
            .find(|m| m.label == label)
    }

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
    #[allow(clippy::too_many_arguments)]
    pub(super) fn handle_add_virtual_line(
        &mut self,
        buffer_id: BufferId,
        position: usize,
        text: String,
        fg_color: (u8, u8, u8),
        bg_color: Option<(u8, u8, u8)>,
        above: bool,
        namespace: String,
        priority: i32,
    ) {
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            use crate::view::virtual_text::{VirtualTextNamespace, VirtualTextPosition};
            use ratatui::style::{Color, Style};

            let placement = if above {
                VirtualTextPosition::LineAbove
            } else {
                VirtualTextPosition::LineBelow
            };

            let mut style = Style::default().fg(Color::Rgb(fg_color.0, fg_color.1, fg_color.2));
            if let Some(bg) = bg_color {
                style = style.bg(Color::Rgb(bg.0, bg.1, bg.2));
            }
            let ns = VirtualTextNamespace::from_string(namespace);

            state.virtual_texts.add_line(
                &mut state.marker_list,
                position,
                text,
                style,
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
        if let Some(menu) = self.find_menu_by_label_mut(&menu_label) {
            // Insert at the specified position
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
            tracing::info!(
                "Added menu item to '{}' at position {}",
                menu_label,
                insert_idx
            );
        } else {
            tracing::warn!("Menu '{}' not found for adding item", menu_label);
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
        if let Some(menu) = self.find_menu_by_label_mut(&menu_label) {
            // Remove item with matching label
            let original_len = menu.items.len();
            menu.items.retain(|item| match item {
                crate::config::MenuItem::Action { label, .. }
                | crate::config::MenuItem::Submenu { label, .. } => label != &item_label,
                _ => true, // Keep separators
            });

            if menu.items.len() < original_len {
                tracing::info!("Removed menu item '{}' from '{}'", item_label, menu_label);
            } else {
                tracing::warn!("Menu item '{}' not found in '{}'", item_label, menu_label);
            }
        } else {
            tracing::warn!("Menu '{}' not found for removing item", menu_label);
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
        // Get the buffer for this split
        if let Some(buffer_id) = self.split_manager.buffer_for_split(split_id) {
            self.focus_split(split_id, buffer_id);
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

        match self.split_manager.set_split_buffer(split_id, buffer_id) {
            Ok(()) => {
                tracing::info!("Set split {:?} to buffer {:?}", split_id, buffer_id);

                // Switch per-buffer view state — the new buffer's own view_transform
                // and compose_width will be restored (or defaults if first time)
                if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
                    view_state.switch_buffer(buffer_id);
                }

                // If this is the active split, update active buffer with all side effects
                if self.split_manager.active_split() == split_id {
                    self.set_active_buffer(buffer_id);
                }
            }
            Err(e) => {
                tracing::error!("Failed to set split buffer: {}", e);
            }
        }
    }

    /// Handle CloseSplit command
    pub(super) fn handle_close_split(&mut self, split_id: SplitId) {
        match self.split_manager.close_split(split_id) {
            Ok(()) => {
                // Clean up the view state for the closed split
                self.split_view_states.remove(&split_id);
                tracing::info!("Closed split {:?}", split_id);
            }
            Err(e) => {
                tracing::warn!("Failed to close split {:?}: {}", split_id, e);
            }
        }
    }

    /// Handle SetSplitRatio command
    pub(super) fn handle_set_split_ratio(&mut self, split_id: SplitId, ratio: f32) {
        match self.split_manager.set_ratio(split_id, ratio) {
            Ok(()) => {
                tracing::debug!("Set split {:?} ratio to {}", split_id, ratio);
            }
            Err(e) => {
                tracing::warn!("Failed to set split ratio {:?}: {}", split_id, e);
            }
        }
    }

    /// Handle DistributeSplitsEvenly command
    pub(super) fn handle_distribute_splits_evenly(&mut self) {
        // The split_ids parameter is currently ignored - we distribute ALL splits evenly
        // A future enhancement could distribute only the specified splits
        self.split_manager.distribute_splits_evenly();
        tracing::debug!("Distributed splits evenly");
    }

    /// Handle SetBufferCursor command
    pub(super) fn handle_set_buffer_cursor(&mut self, buffer_id: BufferId, position: usize) {
        // Find all splits that display this buffer and update their view states
        let splits = self.split_manager.splits_for_buffer(buffer_id);
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
            for split_id in &splits {
                let is_active = *split_id == active_split;

                if let Some(view_state) = self.split_view_states.get_mut(split_id) {
                    // Set cursor position in the split's view state
                    view_state.cursors.primary_mut().move_to(position, false);
                    // Ensure the cursor is visible by scrolling the split's viewport
                    let cursor = *view_state.cursors.primary();
                    view_state
                        .viewport
                        .ensure_visible(&mut state.buffer, &cursor);
                    tracing::debug!(
                        "SetBufferCursor: updated split {:?} (active={}) viewport top_byte={}",
                        split_id,
                        is_active,
                        view_state.viewport.top_byte
                    );

                    // Note: cursors and viewport are now owned by SplitViewState, no sync needed
                } else {
                    tracing::warn!(
                        "SetBufferCursor: split {:?} not found in split_view_states",
                        split_id
                    );
                }
            }
        } else {
            tracing::warn!("Buffer {:?} not found for SetBufferCursor", buffer_id);
        }
    }

    /// Handle SetSplitScroll command
    pub(super) fn handle_set_split_scroll(&mut self, split_id: SplitId, top_byte: usize) {
        if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
            // Get the buffer associated with this split to check bounds
            let buffer_id = if let Some(id) = self.split_manager.buffer_for_split(split_id) {
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
        for split_id in self.split_manager.splits_for_buffer(buffer_id) {
            if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
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
        for split_id in self.split_manager.splits_for_buffer(buffer_id) {
            if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
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
                self.active_event_log_mut().append(event.clone());
                self.apply_event_to_active_buffer(&event);
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

        // Update cursors (now in SplitViewState)
        let cursors = self.active_cursors_mut();
        cursors.primary_mut().position = clamped_position;
        cursors.primary_mut().anchor = None;

        // Ensure the position is visible in the active split's viewport
        let active_split = self.split_manager.active_split();
        let active_buffer = self.active_buffer();
        if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
            let cursor = *view_state.cursors.primary();
            let state = self.buffers.get_mut(&active_buffer).unwrap();
            view_state
                .viewport
                .ensure_visible(&mut state.buffer, &cursor);
        }
    }

    /// Handle OpenFileAtLocation command
    pub(super) fn handle_open_file_at_location(
        &mut self,
        path: std::path::PathBuf,
        line: Option<usize>,
        column: Option<usize>,
    ) -> AnyhowResult<()> {
        // Open the file
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
        let target_split_id = SplitId(split_id);
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

    /// Handle ShowBuffer command
    pub(super) fn handle_show_buffer(&mut self, buffer_id: BufferId) {
        if self.buffers.contains_key(&buffer_id) {
            self.set_active_buffer(buffer_id);
            tracing::info!("Switched to buffer {:?}", buffer_id);
        } else {
            tracing::warn!("Buffer {:?} not found", buffer_id);
        }
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
    pub(super) fn handle_set_layout_hints(
        &mut self,
        buffer_id: BufferId,
        split_id: Option<SplitId>,
        hints: LayoutHints,
    ) {
        let target_split = split_id.unwrap_or(self.split_manager.active_split());
        let view_state = self
            .split_view_states
            .entry(target_split)
            .or_insert_with(|| {
                SplitViewState::with_buffer(self.terminal_width, self.terminal_height, buffer_id)
            });
        view_state.compose_width = hints.compose_width;
        view_state.compose_column_guides = hints.column_guides;
    }

    /// Handle SetViewMode command
    pub(super) fn handle_set_view_mode(&mut self, _buffer_id: BufferId, mode: &str) {
        use crate::state::ViewMode;
        let view_mode = match mode {
            "compose" => ViewMode::Compose,
            _ => ViewMode::Source,
        };
        // Set on the active split's per-buffer view state (source of truth)
        let active_split = self.split_manager.active_split();
        if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
            view_state.view_mode = view_mode;
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

    /// Handle SetLineNumbers command
    ///
    /// Sets line number visibility on the active split's per-buffer view state,
    /// so that different splits showing the same buffer can have independent
    /// line number settings (e.g., source mode shows line numbers, compose hides them).
    pub(super) fn handle_set_line_numbers(&mut self, _buffer_id: BufferId, enabled: bool) {
        let active_split = self.split_manager.active_split();
        if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
            view_state.show_line_numbers = enabled;
        }
    }

    /// Handle SetLineWrap command
    pub(super) fn handle_set_line_wrap(
        &mut self,
        _buffer_id: BufferId,
        split_id: Option<SplitId>,
        enabled: bool,
    ) {
        let target_split = split_id.unwrap_or(self.split_manager.active_split());
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
        let target_split = split_id.unwrap_or(self.split_manager.active_split());
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
        let target_split = split_id.unwrap_or(self.split_manager.active_split());
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
        parent: Option<String>,
        bindings: Vec<(String, String)>,
        read_only: bool,
    ) {
        use super::parse_key_string;
        use crate::input::buffer_mode::BufferMode;

        let mut mode = BufferMode::new(name.clone()).with_read_only(read_only);

        if let Some(parent_name) = parent {
            mode = mode.with_parent(parent_name);
        }

        // Parse key bindings from strings
        // Key strings can be single keys ("g", "C-f") or chord sequences ("g g", "z z")
        for (key_str, command) in bindings {
            let parts: Vec<&str> = key_str.split_whitespace().collect();

            if parts.len() == 1 {
                // Single key binding
                if let Some((code, modifiers)) = parse_key_string(&key_str) {
                    mode = mode.with_binding(code, modifiers, command);
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
                    mode = mode.with_chord_binding(sequence, command);
                }
            }
        }

        self.mode_registry.register(mode);
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
            if lsp.try_spawn(&language) != LspSpawnResult::Spawned {
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
            use_tabs: config.use_tabs.unwrap_or(false),
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
        self.config.languages.insert(language.clone(), lang_config);
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
        let lsp_config = crate::types::LspServerConfig {
            command: config.command,
            args: config.args,
            enabled: true, // Explicitly enable - Default::default() gives false
            auto_start: config.auto_start.unwrap_or(true),
            initialization_options: config.initialization_options,
            ..Default::default()
        };
        // Update LSP manager if available
        if let Some(ref mut lsp) = self.lsp {
            lsp.set_language_config(language.clone(), lsp_config.clone());
        }
        // Also update runtime config
        self.config.lsp.insert(language.clone(), lsp_config);
        tracing::info!("LSP server registered for '{}'", language);
    }

    /// Handle ReloadGrammars command
    /// Rebuilds the grammar registry with pending grammars and invalidates highlight caches
    pub(super) fn handle_reload_grammars(&mut self) {
        use crate::primitives::grammar::GrammarRegistry;
        use std::path::PathBuf;

        tracing::info!(
            "[SYNTAX DEBUG] handle_reload_grammars called, pending_grammars count: {}",
            self.pending_grammars.len()
        );

        if self.pending_grammars.is_empty() {
            tracing::debug!("ReloadGrammars called but no pending grammars");
            return;
        }

        // Collect pending grammars
        let additional: Vec<_> = self
            .pending_grammars
            .drain(..)
            .map(|g| {
                tracing::info!(
                    "[SYNTAX DEBUG] pending grammar: lang='{}', path='{}', extensions={:?}",
                    g.language,
                    g.grammar_path,
                    g.extensions
                );
                (
                    g.language.clone(),
                    PathBuf::from(g.grammar_path),
                    g.extensions.clone(),
                )
            })
            .collect();

        let grammar_count = additional.len();

        // Update config.languages with the extensions so detect_language() works
        for (language, _path, extensions) in &additional {
            let lang_config = self
                .config
                .languages
                .entry(language.clone())
                .or_insert_with(Default::default);
            // Add extensions that aren't already present
            for ext in extensions {
                if !lang_config.extensions.contains(ext) {
                    lang_config.extensions.push(ext.clone());
                }
            }
            tracing::info!(
                "[SYNTAX DEBUG] updated config.languages['{}']: extensions={:?}, grammar='{}'",
                language,
                lang_config.extensions,
                lang_config.grammar
            );
        }

        tracing::info!(
            "[SYNTAX DEBUG] before rebuild: registry has {} syntaxes, user_extensions: {}",
            self.grammar_registry.available_syntaxes().len(),
            self.grammar_registry.user_extensions_debug()
        );

        // Rebuild registry with pending grammars
        match GrammarRegistry::with_additional_grammars(&self.grammar_registry, &additional) {
            Some(new_registry) => {
                tracing::info!(
                    "[SYNTAX DEBUG] after rebuild: new registry has {} syntaxes, user_extensions: {}",
                    new_registry.available_syntaxes().len(),
                    new_registry.user_extensions_debug()
                );
                self.grammar_registry = std::sync::Arc::new(new_registry);

                // Re-detect syntax for all buffers that might now have highlighting
                // Collect buffer IDs and paths first to avoid borrow issues
                let buffers_to_update: Vec<_> = self
                    .buffer_metadata
                    .iter()
                    .filter_map(|(id, meta)| meta.file_path().map(|p| (*id, p.to_path_buf())))
                    .collect();

                for (buf_id, path) in buffers_to_update {
                    if let Some(state) = self.buffers.get_mut(&buf_id) {
                        // Re-create the highlight engine with the new grammar registry
                        let new_engine =
                            crate::primitives::highlight_engine::HighlightEngine::for_file_with_languages(
                                &path,
                                &self.grammar_registry,
                                &self.config.languages,
                            );

                        // Only update if the new engine has highlighting capability
                        // or if the current one doesn't (don't downgrade)
                        if new_engine.has_highlighting() || !state.highlighter.has_highlighting() {
                            state.highlighter = new_engine;
                            tracing::debug!(
                                "Updated syntax highlighting for {:?}",
                                path.file_name()
                            );
                        }
                    }
                }

                // Emit event for plugins that might want to react
                self.emit_event(
                    "grammars_changed",
                    serde_json::json!({ "count": grammar_count }),
                );

                tracing::info!("Grammars reloaded ({} new grammars)", grammar_count);
            }
            None => {
                tracing::error!("Failed to rebuild grammar registry");
            }
        }
    }
}
