//! Mouse input handling.
//!
//! This module contains all mouse event handling logic including:
//! - Click, double-click, and drag handling
//! - Scrollbar interaction
//! - Hover target computation
//! - Split separator dragging
//! - Text selection via mouse

use super::*;
use crate::input::keybindings::Action;
use crate::model::event::{SplitDirection, SplitId};
use crate::services::plugins::hooks::HookArgs;
use crate::view::prompt::PromptType;
use anyhow::Result as AnyhowResult;
use rust_i18n::t;

impl Editor {
    /// Handle a mouse event.
    /// Returns true if a re-render is needed.
    pub fn handle_mouse(
        &mut self,
        mouse_event: crossterm::event::MouseEvent,
    ) -> AnyhowResult<bool> {
        use crossterm::event::{MouseButton, MouseEventKind};

        let col = mouse_event.column;
        let row = mouse_event.row;

        // Detect double-click for left button down events (used by all handlers)
        let is_double_click = if matches!(mouse_event.kind, MouseEventKind::Down(MouseButton::Left))
        {
            let now = self.time_source.now();
            let is_double = if let (Some(previous_time), Some(previous_pos)) =
                (self.previous_click_time, self.previous_click_position)
            {
                let double_click_threshold =
                    std::time::Duration::from_millis(self.config.editor.double_click_time_ms);
                let within_time = now.duration_since(previous_time) < double_click_threshold;
                let same_position = previous_pos == (col, row);
                within_time && same_position
            } else {
                false
            };

            // Update click tracking
            if is_double {
                self.previous_click_time = None;
                self.previous_click_position = None;
            } else {
                self.previous_click_time = Some(now);
                self.previous_click_position = Some((col, row));
            }
            is_double
        } else {
            false
        };

        // When settings modal is open, capture all mouse events
        if self.settings_state.as_ref().map_or(false, |s| s.visible) {
            return self.handle_settings_mouse(mouse_event, is_double_click);
        }

        // When calibration wizard is active, ignore all mouse events
        if self.calibration_wizard.is_some() {
            return Ok(false);
        }

        // Cancel LSP rename prompt on any mouse interaction
        let mut needs_render = false;
        if let Some(ref prompt) = self.prompt {
            if matches!(prompt.prompt_type, PromptType::LspRename { .. }) {
                self.cancel_prompt();
                needs_render = true;
            }
        }

        // Update mouse cursor position for software cursor rendering (used by GPM)
        // When GPM is active, we always need to re-render to update the cursor position
        let cursor_moved = self.mouse_cursor_position != Some((col, row));
        self.mouse_cursor_position = Some((col, row));
        if self.gpm_active && cursor_moved {
            needs_render = true;
        }

        tracing::trace!(
            "handle_mouse: kind={:?}, col={}, row={}",
            mouse_event.kind,
            col,
            row
        );

        match mouse_event.kind {
            MouseEventKind::Down(MouseButton::Left) => {
                if is_double_click {
                    // Double click detected - both clicks within time threshold AND at same position
                    self.handle_mouse_double_click(col, row)?;
                    needs_render = true;
                    return Ok(needs_render);
                }
                self.handle_mouse_click(col, row)?;
                needs_render = true;
            }
            MouseEventKind::Drag(MouseButton::Left) => {
                self.handle_mouse_drag(col, row)?;
                needs_render = true;
            }
            MouseEventKind::Up(MouseButton::Left) => {
                // Check if we were dragging a separator to trigger terminal resize
                let was_dragging_separator = self.mouse_state.dragging_separator.is_some();

                // Check if we were dragging a tab and complete the drop
                if let Some(drag_state) = self.mouse_state.dragging_tab.take() {
                    if drag_state.is_dragging() {
                        if let Some(drop_zone) = drag_state.drop_zone {
                            self.execute_tab_drop(
                                drag_state.buffer_id,
                                drag_state.source_split_id,
                                drop_zone,
                            );
                        }
                    }
                }

                // Stop dragging and clear drag state
                self.mouse_state.dragging_scrollbar = None;
                self.mouse_state.drag_start_row = None;
                self.mouse_state.drag_start_top_byte = None;
                self.mouse_state.dragging_separator = None;
                self.mouse_state.drag_start_position = None;
                self.mouse_state.drag_start_ratio = None;
                self.mouse_state.dragging_file_explorer = false;
                self.mouse_state.drag_start_explorer_width = None;
                // Clear text selection drag state (selection remains in cursor)
                self.mouse_state.dragging_text_selection = false;
                self.mouse_state.drag_selection_split = None;
                self.mouse_state.drag_selection_anchor = None;
                // Clear popup scrollbar drag state
                self.mouse_state.dragging_popup_scrollbar = None;
                self.mouse_state.drag_start_popup_scroll = None;

                // If we finished dragging a separator, resize visible terminals
                if was_dragging_separator {
                    self.resize_visible_terminals();
                }

                needs_render = true;
            }
            MouseEventKind::Moved => {
                // Dispatch MouseMove hook to plugins (fire-and-forget, no blocking check)
                {
                    // Find content rect for the split under the mouse
                    let content_rect = self
                        .cached_layout
                        .split_areas
                        .iter()
                        .find(|(_, _, content_rect, _, _, _)| {
                            col >= content_rect.x
                                && col < content_rect.x + content_rect.width
                                && row >= content_rect.y
                                && row < content_rect.y + content_rect.height
                        })
                        .map(|(_, _, rect, _, _, _)| *rect);

                    let (content_x, content_y) = content_rect.map(|r| (r.x, r.y)).unwrap_or((0, 0));

                    self.plugin_manager.run_hook(
                        "mouse_move",
                        HookArgs::MouseMove {
                            column: col,
                            row,
                            content_x,
                            content_y,
                        },
                    );
                }

                // Only re-render if hover target actually changed
                // (preserve needs_render if already set, e.g., for GPM cursor updates)
                let hover_changed = self.update_hover_target(col, row);
                needs_render = needs_render || hover_changed;

                // Track LSP hover state for mouse-triggered hover popups
                self.update_lsp_hover_state(col, row);
            }
            MouseEventKind::ScrollUp => {
                // Check if file browser is active and should handle scroll
                if self.is_file_open_active() && self.handle_file_open_scroll(-3) {
                    needs_render = true;
                } else if self.is_mouse_over_any_popup(col, row) {
                    // Scroll the popup content (works for all popups including completion)
                    self.scroll_popup(-3);
                    needs_render = true;
                } else {
                    // If in terminal mode, exit to scrollback mode first so scrolling works
                    if self.terminal_mode && self.is_terminal_buffer(self.active_buffer()) {
                        self.sync_terminal_to_buffer(self.active_buffer());
                        self.terminal_mode = false;
                        self.key_context = crate::input::keybindings::KeyContext::Normal;
                    }
                    // Dismiss hover/signature help popups on scroll
                    self.dismiss_transient_popups();
                    self.handle_mouse_scroll(col, row, -3)?;
                    // Sync viewport from SplitViewState to EditorState so rendering sees the scroll
                    self.sync_split_view_state_to_editor_state();
                    needs_render = true;
                }
            }
            MouseEventKind::ScrollDown => {
                // Check if file browser is active and should handle scroll
                if self.is_file_open_active() && self.handle_file_open_scroll(3) {
                    needs_render = true;
                } else if self.is_mouse_over_any_popup(col, row) {
                    // Scroll the popup content (works for all popups including completion)
                    self.scroll_popup(3);
                    needs_render = true;
                } else {
                    // If in terminal mode, exit to scrollback mode first so scrolling works
                    if self.terminal_mode && self.is_terminal_buffer(self.active_buffer()) {
                        self.sync_terminal_to_buffer(self.active_buffer());
                        self.terminal_mode = false;
                        self.key_context = crate::input::keybindings::KeyContext::Normal;
                    }
                    // Dismiss hover/signature help popups on scroll
                    self.dismiss_transient_popups();
                    self.handle_mouse_scroll(col, row, 3)?;
                    // Sync viewport from SplitViewState to EditorState so rendering sees the scroll
                    self.sync_split_view_state_to_editor_state();
                    needs_render = true;
                }
            }
            MouseEventKind::Down(MouseButton::Right) => {
                // Handle right-click for context menus
                self.handle_right_click(col, row)?;
                needs_render = true;
            }
            _ => {
                // Ignore other mouse events for now
            }
        }

        self.mouse_state.last_position = Some((col, row));
        Ok(needs_render)
    }

    /// Update the current hover target based on mouse position
    /// Returns true if the hover target changed (requiring a re-render)
    pub(super) fn update_hover_target(&mut self, col: u16, row: u16) -> bool {
        let old_target = self.mouse_state.hover_target.clone();
        let new_target = self.compute_hover_target(col, row);
        let changed = old_target != new_target;
        self.mouse_state.hover_target = new_target.clone();

        // If a menu is currently open and we're hovering over a different menu bar item,
        // switch to that menu automatically
        if let Some(active_menu_idx) = self.menu_state.active_menu {
            if let Some(HoverTarget::MenuBarItem(hovered_menu_idx)) = new_target.clone() {
                if hovered_menu_idx != active_menu_idx {
                    self.menu_state.open_menu(hovered_menu_idx);
                    return true; // Force re-render since menu changed
                }
            }

            // If hovering over a menu dropdown item, check if it's a submenu and open it
            if let Some(HoverTarget::MenuDropdownItem(_, item_idx)) = new_target.clone() {
                let all_menus: Vec<crate::config::Menu> = self
                    .menus
                    .menus
                    .iter()
                    .chain(self.menu_state.plugin_menus.iter())
                    .cloned()
                    .collect();

                // If this item is the parent of the currently open submenu, keep it open.
                // This prevents blinking when hovering over the parent item of an open submenu.
                if self.menu_state.submenu_path.first() == Some(&item_idx) {
                    tracing::trace!(
                        "menu hover: staying on submenu parent item_idx={}, submenu_path={:?}",
                        item_idx,
                        self.menu_state.submenu_path
                    );
                    return changed;
                }

                // Clear any open submenus since we're at a different item in the main dropdown
                if !self.menu_state.submenu_path.is_empty() {
                    tracing::trace!(
                        "menu hover: clearing submenu_path={:?} for different item_idx={}",
                        self.menu_state.submenu_path,
                        item_idx
                    );
                    self.menu_state.submenu_path.clear();
                    self.menu_state.highlighted_item = Some(item_idx);
                    return true;
                }

                // Check if the hovered item is a submenu
                if let Some(menu) = all_menus.get(active_menu_idx) {
                    if let Some(crate::config::MenuItem::Submenu { items, .. }) =
                        menu.items.get(item_idx)
                    {
                        if !items.is_empty() {
                            tracing::trace!("menu hover: opening submenu at item_idx={}", item_idx);
                            self.menu_state.submenu_path.push(item_idx);
                            self.menu_state.highlighted_item = Some(0);
                            return true;
                        }
                    }
                }
                // Update highlighted item for non-submenu items too
                if self.menu_state.highlighted_item != Some(item_idx) {
                    self.menu_state.highlighted_item = Some(item_idx);
                    return true;
                }
            }

            // If hovering over a submenu item, handle submenu navigation
            if let Some(HoverTarget::SubmenuItem(depth, item_idx)) = new_target {
                // If this item is the parent of a currently open nested submenu, keep it open.
                // This prevents blinking when hovering over the parent item of an open nested submenu.
                // submenu_path[depth] stores the index of the nested submenu opened from this level.
                if self.menu_state.submenu_path.len() > depth
                    && self.menu_state.submenu_path.get(depth) == Some(&item_idx)
                {
                    tracing::trace!(
                        "menu hover: staying on nested submenu parent depth={}, item_idx={}, submenu_path={:?}",
                        depth,
                        item_idx,
                        self.menu_state.submenu_path
                    );
                    return changed;
                }

                // Truncate submenu path to this depth (close any deeper submenus)
                if self.menu_state.submenu_path.len() > depth {
                    tracing::trace!(
                        "menu hover: truncating submenu_path={:?} to depth={} for item_idx={}",
                        self.menu_state.submenu_path,
                        depth,
                        item_idx
                    );
                    self.menu_state.submenu_path.truncate(depth);
                }

                let all_menus: Vec<crate::config::Menu> = self
                    .menus
                    .menus
                    .iter()
                    .chain(self.menu_state.plugin_menus.iter())
                    .cloned()
                    .collect();

                // Get the items at this depth
                if let Some(items) = self
                    .menu_state
                    .get_current_items(&all_menus, active_menu_idx)
                {
                    // Check if hovered item is a submenu - if so, open it
                    if let Some(crate::config::MenuItem::Submenu {
                        items: sub_items, ..
                    }) = items.get(item_idx)
                    {
                        if !sub_items.is_empty()
                            && !self.menu_state.submenu_path.contains(&item_idx)
                        {
                            tracing::trace!(
                                "menu hover: opening nested submenu at depth={}, item_idx={}",
                                depth,
                                item_idx
                            );
                            self.menu_state.submenu_path.push(item_idx);
                            self.menu_state.highlighted_item = Some(0);
                            return true;
                        }
                    }
                    // Update highlighted item
                    if self.menu_state.highlighted_item != Some(item_idx) {
                        self.menu_state.highlighted_item = Some(item_idx);
                        return true;
                    }
                }
            }
        }

        // Handle tab context menu hover - update highlighted item
        if let Some(HoverTarget::TabContextMenuItem(item_idx)) = new_target {
            if let Some(ref mut menu) = self.tab_context_menu {
                if menu.highlighted != item_idx {
                    menu.highlighted = item_idx;
                    return true;
                }
            }
        }

        changed
    }

    /// Update LSP hover state based on mouse position
    /// Tracks position for debounced hover requests
    ///
    /// Hover popup stays visible when:
    /// - Mouse is over the hover popup itself
    /// - Mouse is within the hovered symbol range
    /// Hover is dismissed when mouse leaves the editor area entirely.
    fn update_lsp_hover_state(&mut self, col: u16, row: u16) {
        tracing::trace!(col, row, "update_lsp_hover_state: raw mouse position");

        // Check if mouse is over a transient popup - if so, keep hover active
        if self.is_mouse_over_transient_popup(col, row) {
            return;
        }

        // Find which split the mouse is over
        let split_info = self
            .cached_layout
            .split_areas
            .iter()
            .find(|(_, _, content_rect, _, _, _)| {
                col >= content_rect.x
                    && col < content_rect.x + content_rect.width
                    && row >= content_rect.y
                    && row < content_rect.y + content_rect.height
            })
            .map(|(split_id, buffer_id, content_rect, _, _, _)| {
                (*split_id, *buffer_id, *content_rect)
            });

        let Some((split_id, buffer_id, content_rect)) = split_info else {
            // Mouse is not over editor content - clear hover state and dismiss popup
            if self.mouse_state.lsp_hover_state.is_some() {
                self.mouse_state.lsp_hover_state = None;
                self.mouse_state.lsp_hover_request_sent = false;
                self.dismiss_transient_popups();
            }
            return;
        };

        // Get cached mappings and gutter width for this split
        let cached_mappings = self
            .cached_layout
            .view_line_mappings
            .get(&split_id)
            .cloned();
        let gutter_width = self
            .buffers
            .get(&buffer_id)
            .map(|s| s.margins.left_total_width() as u16)
            .unwrap_or(0);
        let fallback = self
            .buffers
            .get(&buffer_id)
            .map(|s| s.buffer.len())
            .unwrap_or(0);

        // Convert screen position to buffer byte position
        let Some(byte_pos) = Self::screen_to_buffer_position(
            col,
            row,
            content_rect,
            gutter_width,
            &cached_mappings,
            fallback,
            false, // Don't include gutter
        ) else {
            // Mouse is in gutter - clear hover state
            if self.mouse_state.lsp_hover_state.is_some() {
                self.mouse_state.lsp_hover_state = None;
                self.mouse_state.lsp_hover_request_sent = false;
                self.dismiss_transient_popups();
            }
            return;
        };

        // Check if mouse is past the end of line content - don't trigger hover for empty space
        let content_col = col.saturating_sub(content_rect.x);
        let text_col = content_col.saturating_sub(gutter_width) as usize;
        let visual_row = row.saturating_sub(content_rect.y) as usize;

        let line_info = cached_mappings
            .as_ref()
            .and_then(|mappings| mappings.get(visual_row))
            .map(|line_mapping| {
                (
                    line_mapping.visual_to_char.len(),
                    line_mapping.line_end_byte,
                )
            });

        let is_past_line_end_or_empty = line_info
            .map(|(line_len, _)| {
                // Empty lines (just newline) should not trigger hover
                if line_len <= 1 {
                    return true;
                }
                text_col >= line_len
            })
            // If mouse is below all mapped lines (no mapping), don't trigger hover
            .unwrap_or(true);

        tracing::trace!(
            col,
            row,
            content_col,
            text_col,
            visual_row,
            gutter_width,
            byte_pos,
            ?line_info,
            is_past_line_end_or_empty,
            "update_lsp_hover_state: position check"
        );

        if is_past_line_end_or_empty {
            tracing::trace!(
                "update_lsp_hover_state: mouse past line end or empty line, clearing hover"
            );
            // Mouse is past end of line content - clear hover state and don't trigger new hover
            if self.mouse_state.lsp_hover_state.is_some() {
                self.mouse_state.lsp_hover_state = None;
                self.mouse_state.lsp_hover_request_sent = false;
                self.dismiss_transient_popups();
            }
            return;
        }

        // Check if mouse is within the hovered symbol range - if so, keep hover active
        if let Some((start, end)) = self.hover_symbol_range {
            if byte_pos >= start && byte_pos < end {
                // Mouse is still over the hovered symbol - keep hover state
                return;
            }
        }

        // Check if we're still hovering the same position
        if let Some((old_pos, _, _, _)) = self.mouse_state.lsp_hover_state {
            if old_pos == byte_pos {
                // Same position - keep existing state
                return;
            }
            // Position changed outside symbol range - reset state and dismiss popup
            self.dismiss_transient_popups();
        }

        // Start tracking new hover position
        self.mouse_state.lsp_hover_state = Some((byte_pos, std::time::Instant::now(), col, row));
        self.mouse_state.lsp_hover_request_sent = false;
    }

    /// Check if mouse position is over a transient popup (hover, signature help)
    fn is_mouse_over_transient_popup(&self, col: u16, row: u16) -> bool {
        // Check if there's a transient popup showing
        let has_transient_popup = self
            .active_state()
            .popups
            .top()
            .is_some_and(|p| p.transient);

        if !has_transient_popup {
            return false;
        }

        self.is_mouse_over_any_popup(col, row)
    }

    /// Check if mouse position is over any popup (including non-transient ones like completion)
    fn is_mouse_over_any_popup(&self, col: u16, row: u16) -> bool {
        // Check if there's any popup showing
        if !self.active_state().popups.is_visible() {
            return false;
        }

        // Check if mouse is over any popup area
        for (_popup_idx, popup_rect, _inner_rect, _scroll_offset, _num_items, _, _) in
            self.cached_layout.popup_areas.iter()
        {
            if col >= popup_rect.x
                && col < popup_rect.x + popup_rect.width
                && row >= popup_rect.y
                && row < popup_rect.y + popup_rect.height
            {
                return true;
            }
        }

        false
    }

    /// Compute what hover target is at the given position
    fn compute_hover_target(&self, col: u16, row: u16) -> Option<HoverTarget> {
        // Check tab context menu first (it's rendered on top)
        if let Some(ref menu) = self.tab_context_menu {
            let menu_x = menu.position.0;
            let menu_y = menu.position.1;
            let menu_width = 22u16;
            let items = super::types::TabContextMenuItem::all();
            let menu_height = items.len() as u16 + 2;

            if col >= menu_x
                && col < menu_x + menu_width
                && row > menu_y
                && row < menu_y + menu_height - 1
            {
                let item_idx = (row - menu_y - 1) as usize;
                if item_idx < items.len() {
                    return Some(HoverTarget::TabContextMenuItem(item_idx));
                }
            }
        }

        // Check suggestions area first (command palette, autocomplete)
        if let Some((inner_rect, start_idx, _visible_count, total_count)) =
            &self.cached_layout.suggestions_area
        {
            if col >= inner_rect.x
                && col < inner_rect.x + inner_rect.width
                && row >= inner_rect.y
                && row < inner_rect.y + inner_rect.height
            {
                let relative_row = (row - inner_rect.y) as usize;
                let item_idx = start_idx + relative_row;

                if item_idx < *total_count {
                    return Some(HoverTarget::SuggestionItem(item_idx));
                }
            }
        }

        // Check popups (they're rendered on top)
        // Check from top to bottom (reverse order since last popup is on top)
        for (popup_idx, _popup_rect, inner_rect, scroll_offset, num_items, _, _) in
            self.cached_layout.popup_areas.iter().rev()
        {
            if col >= inner_rect.x
                && col < inner_rect.x + inner_rect.width
                && row >= inner_rect.y
                && row < inner_rect.y + inner_rect.height
                && *num_items > 0
            {
                // Calculate which item is being hovered
                let relative_row = (row - inner_rect.y) as usize;
                let item_idx = scroll_offset + relative_row;

                if item_idx < *num_items {
                    return Some(HoverTarget::PopupListItem(*popup_idx, item_idx));
                }
            }
        }

        // Check file browser popup
        if self.is_file_open_active() {
            if let Some(hover) = self.compute_file_browser_hover(col, row) {
                return Some(hover);
            }
        }

        // Check menu bar (row 0)
        if row == 0 {
            let all_menus: Vec<crate::config::Menu> = self
                .menus
                .menus
                .iter()
                .chain(self.menu_state.plugin_menus.iter())
                .cloned()
                .collect();

            if let Some(menu_idx) = self.menu_state.get_menu_at_position(&all_menus, col) {
                return Some(HoverTarget::MenuBarItem(menu_idx));
            }
        }

        // Check menu dropdown items if a menu is open (including submenus)
        if let Some(active_idx) = self.menu_state.active_menu {
            let all_menus: Vec<crate::config::Menu> = self
                .menus
                .menus
                .iter()
                .chain(self.menu_state.plugin_menus.iter())
                .cloned()
                .collect();

            if let Some(menu) = all_menus.get(active_idx) {
                if let Some(hover) =
                    self.compute_menu_dropdown_hover(col, row, menu, active_idx, &all_menus)
                {
                    return Some(hover);
                }
            }
        }

        // Check file explorer close button and border (for resize)
        if let Some(explorer_area) = self.cached_layout.file_explorer_area {
            // Close button is at position: explorer_area.x + explorer_area.width - 3 to -1
            let close_button_x = explorer_area.x + explorer_area.width.saturating_sub(3);
            if row == explorer_area.y
                && col >= close_button_x
                && col < explorer_area.x + explorer_area.width
            {
                return Some(HoverTarget::FileExplorerCloseButton);
            }

            // The border is at the right edge of the file explorer area
            let border_x = explorer_area.x + explorer_area.width;
            if col == border_x
                && row >= explorer_area.y
                && row < explorer_area.y + explorer_area.height
            {
                return Some(HoverTarget::FileExplorerBorder);
            }
        }

        // Check split separators
        for (split_id, direction, sep_x, sep_y, sep_length) in &self.cached_layout.separator_areas {
            let is_on_separator = match direction {
                SplitDirection::Horizontal => {
                    row == *sep_y && col >= *sep_x && col < sep_x + sep_length
                }
                SplitDirection::Vertical => {
                    col == *sep_x && row >= *sep_y && row < sep_y + sep_length
                }
            };

            if is_on_separator {
                return Some(HoverTarget::SplitSeparator(*split_id, *direction));
            }
        }

        // Check tab areas using cached hit regions (computed during rendering)
        // Check split control buttons first (they're on top of the tab row)
        for (split_id, btn_row, start_col, end_col) in &self.cached_layout.close_split_areas {
            if row == *btn_row && col >= *start_col && col < *end_col {
                return Some(HoverTarget::CloseSplitButton(*split_id));
            }
        }

        for (split_id, btn_row, start_col, end_col) in &self.cached_layout.maximize_split_areas {
            if row == *btn_row && col >= *start_col && col < *end_col {
                return Some(HoverTarget::MaximizeSplitButton(*split_id));
            }
        }

        for (split_id, buffer_id, tab_row, start_col, end_col, close_start) in
            &self.cached_layout.tab_areas
        {
            if row == *tab_row && col >= *start_col && col < *end_col {
                // Check if hovering over the close button
                if col >= *close_start {
                    return Some(HoverTarget::TabCloseButton(*buffer_id, *split_id));
                }
                // Otherwise, return TabName for hover effect on tab name
                return Some(HoverTarget::TabName(*buffer_id, *split_id));
            }
        }

        // Check scrollbars
        for (split_id, _buffer_id, _content_rect, scrollbar_rect, thumb_start, thumb_end) in
            &self.cached_layout.split_areas
        {
            if col >= scrollbar_rect.x
                && col < scrollbar_rect.x + scrollbar_rect.width
                && row >= scrollbar_rect.y
                && row < scrollbar_rect.y + scrollbar_rect.height
            {
                let relative_row = row.saturating_sub(scrollbar_rect.y) as usize;
                let is_on_thumb = relative_row >= *thumb_start && relative_row < *thumb_end;

                if is_on_thumb {
                    return Some(HoverTarget::ScrollbarThumb(*split_id));
                } else {
                    return Some(HoverTarget::ScrollbarTrack(*split_id));
                }
            }
        }

        // Check status bar indicators
        if let Some((status_row, _status_x, _status_width)) = self.cached_layout.status_bar_area {
            if row == status_row {
                // Check line ending indicator area
                if let Some((le_row, le_start, le_end)) =
                    self.cached_layout.status_bar_line_ending_area
                {
                    if row == le_row && col >= le_start && col < le_end {
                        return Some(HoverTarget::StatusBarLineEndingIndicator);
                    }
                }

                // Check LSP indicator area
                if let Some((lsp_row, lsp_start, lsp_end)) = self.cached_layout.status_bar_lsp_area
                {
                    if row == lsp_row && col >= lsp_start && col < lsp_end {
                        return Some(HoverTarget::StatusBarLspIndicator);
                    }
                }

                // Check warning badge area
                if let Some((warn_row, warn_start, warn_end)) =
                    self.cached_layout.status_bar_warning_area
                {
                    if row == warn_row && col >= warn_start && col < warn_end {
                        return Some(HoverTarget::StatusBarWarningBadge);
                    }
                }
            }
        }

        // Check search options bar checkboxes
        if let Some(ref layout) = self.cached_layout.search_options_layout {
            use crate::view::ui::status_bar::SearchOptionsHover;
            if let Some(hover) = layout.checkbox_at(col, row) {
                return Some(match hover {
                    SearchOptionsHover::CaseSensitive => HoverTarget::SearchOptionCaseSensitive,
                    SearchOptionsHover::WholeWord => HoverTarget::SearchOptionWholeWord,
                    SearchOptionsHover::Regex => HoverTarget::SearchOptionRegex,
                    SearchOptionsHover::ConfirmEach => HoverTarget::SearchOptionConfirmEach,
                    SearchOptionsHover::None => return None,
                });
            }
        }

        // No hover target
        None
    }

    /// Handle mouse double click (down event)
    /// Double-click in editor area selects the word under the cursor.
    pub(super) fn handle_mouse_double_click(&mut self, col: u16, row: u16) -> AnyhowResult<()> {
        tracing::debug!("handle_mouse_double_click at col={}, row={}", col, row);

        // Is it in the file open dialog?
        if self.handle_file_open_double_click(col, row) {
            return Ok(());
        }

        // Find which split/buffer was clicked and handle double-click
        let split_areas = self.cached_layout.split_areas.clone();
        for (split_id, buffer_id, content_rect, _scrollbar_rect, _thumb_start, _thumb_end) in
            &split_areas
        {
            if col >= content_rect.x
                && col < content_rect.x + content_rect.width
                && row >= content_rect.y
                && row < content_rect.y + content_rect.height
            {
                // Double-clicked on an editor split
                if self.is_terminal_buffer(*buffer_id) {
                    self.key_context = crate::input::keybindings::KeyContext::Terminal;
                    // Don't select word in terminal buffers
                    return Ok(());
                }

                self.key_context = crate::input::keybindings::KeyContext::Normal;

                // Position cursor at click location and select word
                self.handle_editor_double_click(col, row, *split_id, *buffer_id, *content_rect)?;
                return Ok(());
            }
        }

        Ok(())
    }

    /// Handle double-click in editor content area - selects the word under cursor
    fn handle_editor_double_click(
        &mut self,
        col: u16,
        row: u16,
        split_id: crate::model::event::SplitId,
        buffer_id: BufferId,
        content_rect: ratatui::layout::Rect,
    ) -> AnyhowResult<()> {
        use crate::model::event::Event;

        // Focus this split
        self.focus_split(split_id, buffer_id);

        // Get cached view line mappings for this split
        let cached_mappings = self
            .cached_layout
            .view_line_mappings
            .get(&split_id)
            .cloned();

        // Get fallback from SplitViewState viewport
        let fallback = self
            .split_view_states
            .get(&split_id)
            .map(|vs| vs.viewport.top_byte)
            .unwrap_or(0);

        // Calculate clicked position in buffer
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            let gutter_width = state.margins.left_total_width() as u16;

            let Some(target_position) = Self::screen_to_buffer_position(
                col,
                row,
                content_rect,
                gutter_width,
                &cached_mappings,
                fallback,
                true, // Allow gutter clicks
            ) else {
                return Ok(());
            };

            // Move cursor to clicked position first
            let primary_cursor_id = state.cursors.primary_id();
            let event = Event::MoveCursor {
                cursor_id: primary_cursor_id,
                old_position: 0,
                new_position: target_position,
                old_anchor: None,
                new_anchor: None,
                old_sticky_column: 0,
                new_sticky_column: 0,
            };

            if let Some(event_log) = self.event_logs.get_mut(&buffer_id) {
                event_log.append(event.clone());
            }
            state.apply(&event);
        }

        // Now select the word under cursor
        self.handle_action(Action::SelectWord)?;

        Ok(())
    }
    /// Handle mouse click (down event)
    pub(super) fn handle_mouse_click(&mut self, col: u16, row: u16) -> AnyhowResult<()> {
        // Check if click is on tab context menu first
        if self.tab_context_menu.is_some() {
            if let Some(result) = self.handle_tab_context_menu_click(col, row) {
                return result;
            }
        }

        // Check if click is on suggestions (command palette, autocomplete)
        if let Some((inner_rect, start_idx, _visible_count, total_count)) =
            &self.cached_layout.suggestions_area.clone()
        {
            if col >= inner_rect.x
                && col < inner_rect.x + inner_rect.width
                && row >= inner_rect.y
                && row < inner_rect.y + inner_rect.height
            {
                let relative_row = (row - inner_rect.y) as usize;
                let item_idx = start_idx + relative_row;

                if item_idx < *total_count {
                    // Select and execute the clicked suggestion
                    if let Some(prompt) = &mut self.prompt {
                        prompt.selected_suggestion = Some(item_idx);
                    }
                    // Execute the suggestion (same as pressing Enter)
                    return self.handle_action(Action::PromptConfirm);
                }
            }
        }

        // Check if click is on a popup scrollbar first (they're rendered on top)
        // Collect scroll info first to avoid borrow conflicts
        let scrollbar_scroll_info: Option<(usize, i32)> =
            self.cached_layout.popup_areas.iter().rev().find_map(
                |(
                    popup_idx,
                    _popup_rect,
                    inner_rect,
                    _scroll_offset,
                    _num_items,
                    scrollbar_rect,
                    total_lines,
                )| {
                    let sb_rect = scrollbar_rect.as_ref()?;
                    if col >= sb_rect.x
                        && col < sb_rect.x + sb_rect.width
                        && row >= sb_rect.y
                        && row < sb_rect.y + sb_rect.height
                    {
                        let relative_row = (row - sb_rect.y) as usize;
                        let track_height = sb_rect.height as usize;
                        let visible_lines = inner_rect.height as usize;

                        if track_height > 0 && *total_lines > visible_lines {
                            let max_scroll = total_lines.saturating_sub(visible_lines);
                            let target_scroll = if track_height > 1 {
                                (relative_row * max_scroll) / (track_height.saturating_sub(1))
                            } else {
                                0
                            };
                            Some((*popup_idx, target_scroll as i32))
                        } else {
                            Some((*popup_idx, 0))
                        }
                    } else {
                        None
                    }
                },
            );

        if let Some((popup_idx, target_scroll)) = scrollbar_scroll_info {
            // Set up drag state for popup scrollbar (reuse drag_start_row like editor scrollbar)
            self.mouse_state.dragging_popup_scrollbar = Some(popup_idx);
            self.mouse_state.drag_start_row = Some(row);
            // Get current scroll offset before mutable borrow
            let current_scroll = self
                .active_state()
                .popups
                .get(popup_idx)
                .map(|p| p.scroll_offset)
                .unwrap_or(0);
            self.mouse_state.drag_start_popup_scroll = Some(current_scroll);
            // Now do the scroll
            let state = self.active_state_mut();
            if let Some(popup) = state.popups.get_mut(popup_idx) {
                let delta = target_scroll - current_scroll as i32;
                popup.scroll_by(delta);
            }
            return Ok(());
        }

        // Check if click is on a popup content area (they're rendered on top)
        for (_popup_idx, _popup_rect, inner_rect, scroll_offset, num_items, _, _) in
            self.cached_layout.popup_areas.iter().rev()
        {
            if col >= inner_rect.x
                && col < inner_rect.x + inner_rect.width
                && row >= inner_rect.y
                && row < inner_rect.y + inner_rect.height
            {
                // Calculate relative position within the popup content area
                let relative_col = (col - inner_rect.x) as usize;
                let relative_row = (row - inner_rect.y) as usize;

                // First, check if this is a markdown popup with a link
                let link_url = {
                    let state = self.active_state();
                    state
                        .popups
                        .top()
                        .and_then(|popup| popup.link_at_position(relative_col, relative_row))
                };

                if let Some(url) = link_url {
                    // Open the URL in the default browser
                    #[cfg(feature = "runtime")]
                    if let Err(e) = open::that(&url) {
                        self.set_status_message(format!("Failed to open URL: {}", e));
                    } else {
                        self.set_status_message(format!("Opening: {}", url));
                    }
                    return Ok(());
                }

                // For list popups, handle item selection
                if *num_items > 0 {
                    let item_idx = scroll_offset + relative_row;

                    if item_idx < *num_items {
                        // Select and execute the clicked item
                        let state = self.active_state_mut();
                        if let Some(popup) = state.popups.top_mut() {
                            if let crate::view::popup::PopupContent::List { items: _, selected } =
                                &mut popup.content
                            {
                                *selected = item_idx;
                            }
                        }
                        // Execute the popup selection (same as pressing Enter)
                        return self.handle_action(Action::PopupConfirm);
                    }
                }
            }
        }

        // Check if click is on the file browser popup
        if self.is_file_open_active() {
            if self.handle_file_open_click(col, row) {
                return Ok(());
            }
        }

        // Check if click is on menu bar (row 0)
        if row == 0 {
            let all_menus: Vec<crate::config::Menu> = self
                .menus
                .menus
                .iter()
                .chain(self.menu_state.plugin_menus.iter())
                .cloned()
                .collect();

            if let Some(menu_idx) = self.menu_state.get_menu_at_position(&all_menus, col) {
                // Toggle menu: if same menu is open, close it; otherwise open clicked menu
                if self.menu_state.active_menu == Some(menu_idx) {
                    self.close_menu_with_auto_hide();
                } else {
                    // Dismiss transient popups and clear hover state when opening menu
                    self.on_editor_focus_lost();
                    self.menu_state.open_menu(menu_idx);
                }
            } else {
                // Clicked on menu bar but not on a menu label - close any open menu
                self.close_menu_with_auto_hide();
            }
            return Ok(());
        }

        // Check if click is on an open menu dropdown
        if let Some(active_idx) = self.menu_state.active_menu {
            let all_menus: Vec<crate::config::Menu> = self
                .menus
                .menus
                .iter()
                .chain(self.menu_state.plugin_menus.iter())
                .cloned()
                .collect();

            if let Some(menu) = all_menus.get(active_idx) {
                // Handle click on menu dropdown chain (including submenus)
                if let Some(click_result) =
                    self.handle_menu_dropdown_click(col, row, menu, active_idx, &all_menus)?
                {
                    return click_result;
                }
            }

            // Click outside the dropdown - close the menu
            self.close_menu_with_auto_hide();
            return Ok(());
        }

        // Check if click is on file explorer
        if let Some(explorer_area) = self.cached_layout.file_explorer_area {
            if col >= explorer_area.x
                && col < explorer_area.x + explorer_area.width
                && row >= explorer_area.y
                && row < explorer_area.y + explorer_area.height
            {
                self.handle_file_explorer_click(col, row, explorer_area)?;
                return Ok(());
            }
        }

        // Check if click is on a scrollbar
        let scrollbar_hit = self.cached_layout.split_areas.iter().find_map(
            |(split_id, buffer_id, _content_rect, scrollbar_rect, thumb_start, thumb_end)| {
                if col >= scrollbar_rect.x
                    && col < scrollbar_rect.x + scrollbar_rect.width
                    && row >= scrollbar_rect.y
                    && row < scrollbar_rect.y + scrollbar_rect.height
                {
                    let relative_row = row.saturating_sub(scrollbar_rect.y) as usize;
                    let is_on_thumb = relative_row >= *thumb_start && relative_row < *thumb_end;
                    Some((*split_id, *buffer_id, *scrollbar_rect, is_on_thumb))
                } else {
                    None
                }
            },
        );

        if let Some((split_id, buffer_id, scrollbar_rect, is_on_thumb)) = scrollbar_hit {
            self.focus_split(split_id, buffer_id);

            if is_on_thumb {
                // Click on thumb - start drag from current position (don't jump)
                self.mouse_state.dragging_scrollbar = Some(split_id);
                self.mouse_state.drag_start_row = Some(row);
                // Record the current viewport position from SplitViewState
                if let Some(view_state) = self.split_view_states.get(&split_id) {
                    self.mouse_state.drag_start_top_byte = Some(view_state.viewport.top_byte);
                }
            } else {
                // Click on track - jump to position
                self.mouse_state.dragging_scrollbar = Some(split_id);
                self.handle_scrollbar_jump(col, row, split_id, buffer_id, scrollbar_rect)?;
            }
            return Ok(());
        }

        // Check if click is on status bar indicators
        if let Some((status_row, _status_x, _status_width)) = self.cached_layout.status_bar_area {
            if row == status_row {
                // Check line ending indicator - click opens line ending selector
                if let Some((le_row, le_start, le_end)) =
                    self.cached_layout.status_bar_line_ending_area
                {
                    if row == le_row && col >= le_start && col < le_end {
                        return self.handle_action(Action::SetLineEnding);
                    }
                }

                // Check LSP indicator - click opens LSP status popup
                if let Some((lsp_row, lsp_start, lsp_end)) = self.cached_layout.status_bar_lsp_area
                {
                    if row == lsp_row && col >= lsp_start && col < lsp_end {
                        return self.handle_action(Action::ShowLspStatus);
                    }
                }

                // Check warning badge - click opens warning log
                if let Some((warn_row, warn_start, warn_end)) =
                    self.cached_layout.status_bar_warning_area
                {
                    if row == warn_row && col >= warn_start && col < warn_end {
                        return self.handle_action(Action::ShowWarnings);
                    }
                }
            }
        }

        // Check if click is on search options checkboxes
        if let Some(ref layout) = self.cached_layout.search_options_layout.clone() {
            use crate::view::ui::status_bar::SearchOptionsHover;
            if let Some(hover) = layout.checkbox_at(col, row) {
                match hover {
                    SearchOptionsHover::CaseSensitive => {
                        return self.handle_action(Action::ToggleSearchCaseSensitive);
                    }
                    SearchOptionsHover::WholeWord => {
                        return self.handle_action(Action::ToggleSearchWholeWord);
                    }
                    SearchOptionsHover::Regex => {
                        return self.handle_action(Action::ToggleSearchRegex);
                    }
                    SearchOptionsHover::ConfirmEach => {
                        return self.handle_action(Action::ToggleSearchConfirmEach);
                    }
                    SearchOptionsHover::None => {}
                }
            }
        }

        // Check if click is on file explorer border (for drag resizing)
        if let Some(explorer_area) = self.cached_layout.file_explorer_area {
            let border_x = explorer_area.x + explorer_area.width;
            if col == border_x
                && row >= explorer_area.y
                && row < explorer_area.y + explorer_area.height
            {
                // Start file explorer border drag
                self.mouse_state.dragging_file_explorer = true;
                self.mouse_state.drag_start_position = Some((col, row));
                self.mouse_state.drag_start_explorer_width = Some(self.file_explorer_width_percent);
                return Ok(());
            }
        }

        // Check if click is on a split separator (for drag resizing)
        for (split_id, direction, sep_x, sep_y, sep_length) in &self.cached_layout.separator_areas {
            let is_on_separator = match direction {
                SplitDirection::Horizontal => {
                    // Horizontal separator: spans full width at a specific y
                    row == *sep_y && col >= *sep_x && col < sep_x + sep_length
                }
                SplitDirection::Vertical => {
                    // Vertical separator: spans full height at a specific x
                    col == *sep_x && row >= *sep_y && row < sep_y + sep_length
                }
            };

            if is_on_separator {
                // Start separator drag
                self.mouse_state.dragging_separator = Some((*split_id, *direction));
                self.mouse_state.drag_start_position = Some((col, row));
                // Store the initial ratio
                if let Some(ratio) = self.split_manager.get_ratio(*split_id) {
                    self.mouse_state.drag_start_ratio = Some(ratio);
                }
                return Ok(());
            }
        }

        // Check if click is on a close split button
        let close_split_click = self
            .cached_layout
            .close_split_areas
            .iter()
            .find(|(_, btn_row, start_col, end_col)| {
                row == *btn_row && col >= *start_col && col < *end_col
            })
            .map(|(split_id, _, _, _)| *split_id);

        if let Some(split_id) = close_split_click {
            if let Err(e) = self.split_manager.close_split(split_id) {
                self.set_status_message(
                    t!("error.cannot_close_split", error = e.to_string()).to_string(),
                );
            } else {
                // Update active buffer to match the new active split
                let new_active_split = self.split_manager.active_split();
                if let Some(buffer_id) = self.split_manager.buffer_for_split(new_active_split) {
                    self.set_active_buffer(buffer_id);
                }
                self.set_status_message(t!("split.closed").to_string());
            }
            return Ok(());
        }

        // Check if click is on a maximize split button
        let maximize_split_click = self
            .cached_layout
            .maximize_split_areas
            .iter()
            .find(|(_, btn_row, start_col, end_col)| {
                row == *btn_row && col >= *start_col && col < *end_col
            })
            .map(|(split_id, _, _, _)| *split_id);

        if let Some(_split_id) = maximize_split_click {
            // Toggle maximize state
            match self.split_manager.toggle_maximize() {
                Ok(maximized) => {
                    if maximized {
                        self.set_status_message(t!("split.maximized").to_string());
                    } else {
                        self.set_status_message(t!("split.restored").to_string());
                    }
                }
                Err(e) => self.set_status_message(e),
            }
            return Ok(());
        }

        // Check if click is on a tab using cached hit areas (computed during rendering)
        let tab_click = self.cached_layout.tab_areas.iter().find_map(
            |(split_id, buffer_id, tab_row, start_col, end_col, close_start)| {
                if row == *tab_row && col >= *start_col && col < *end_col {
                    let is_close_button = col >= *close_start;
                    Some((*split_id, *buffer_id, is_close_button))
                } else {
                    None
                }
            },
        );

        if let Some((split_id, clicked_buffer, clicked_close)) = tab_click {
            self.focus_split(split_id, clicked_buffer);

            // Handle close button click - use close_tab logic
            if clicked_close {
                self.close_tab_in_split(clicked_buffer, split_id);
                return Ok(());
            }

            // Start potential tab drag (will only become active after moving threshold)
            self.mouse_state.dragging_tab = Some(super::types::TabDragState::new(
                clicked_buffer,
                split_id,
                (col, row),
            ));
            return Ok(());
        }

        // Check if click is in editor content area
        tracing::debug!(
            "handle_mouse_click: checking {} split_areas for click at ({}, {})",
            self.cached_layout.split_areas.len(),
            col,
            row
        );
        for (split_id, buffer_id, content_rect, _scrollbar_rect, _thumb_start, _thumb_end) in
            &self.cached_layout.split_areas
        {
            tracing::debug!(
                "  split_id={:?}, content_rect=({}, {}, {}x{})",
                split_id,
                content_rect.x,
                content_rect.y,
                content_rect.width,
                content_rect.height
            );
            if col >= content_rect.x
                && col < content_rect.x + content_rect.width
                && row >= content_rect.y
                && row < content_rect.y + content_rect.height
            {
                // Click in editor - focus split and position cursor
                tracing::debug!("  -> HIT! calling handle_editor_click");
                self.handle_editor_click(col, row, *split_id, *buffer_id, *content_rect)?;
                return Ok(());
            }
        }
        tracing::debug!("  -> No split area hit");

        Ok(())
    }

    /// Handle mouse drag event
    pub(super) fn handle_mouse_drag(&mut self, col: u16, row: u16) -> AnyhowResult<()> {
        // If dragging scrollbar, update scroll position
        if let Some(dragging_split_id) = self.mouse_state.dragging_scrollbar {
            // Find the buffer and scrollbar rect for this split
            for (split_id, buffer_id, _content_rect, scrollbar_rect, _thumb_start, _thumb_end) in
                &self.cached_layout.split_areas
            {
                if *split_id == dragging_split_id {
                    // Check if we started dragging from the thumb (have drag_start_row)
                    if self.mouse_state.drag_start_row.is_some() {
                        // Relative drag from thumb
                        self.handle_scrollbar_drag_relative(
                            row,
                            *split_id,
                            *buffer_id,
                            *scrollbar_rect,
                        )?;
                    } else {
                        // Jump drag (started from track)
                        self.handle_scrollbar_jump(
                            col,
                            row,
                            *split_id,
                            *buffer_id,
                            *scrollbar_rect,
                        )?;
                    }
                    return Ok(());
                }
            }
        }

        // If dragging popup scrollbar, update popup scroll position
        if let Some(popup_idx) = self.mouse_state.dragging_popup_scrollbar {
            // Find the popup's scrollbar rect from cached layout
            if let Some((_, _, inner_rect, _, _, scrollbar_rect, total_lines)) = self
                .cached_layout
                .popup_areas
                .iter()
                .find(|(idx, _, _, _, _, _, _)| *idx == popup_idx)
            {
                if let Some(sb_rect) = scrollbar_rect {
                    let track_height = sb_rect.height as usize;
                    let visible_lines = inner_rect.height as usize;

                    if track_height > 0 && *total_lines > visible_lines {
                        let relative_row = row.saturating_sub(sb_rect.y) as usize;
                        let max_scroll = total_lines.saturating_sub(visible_lines);
                        let target_scroll = if track_height > 1 {
                            (relative_row * max_scroll) / (track_height.saturating_sub(1))
                        } else {
                            0
                        };

                        let state = self.active_state_mut();
                        if let Some(popup) = state.popups.get_mut(popup_idx) {
                            let current_scroll = popup.scroll_offset as i32;
                            let delta = target_scroll as i32 - current_scroll;
                            popup.scroll_by(delta);
                        }
                    }
                }
            }
            return Ok(());
        }

        // If dragging separator, update split ratio
        if let Some((split_id, direction)) = self.mouse_state.dragging_separator {
            self.handle_separator_drag(col, row, split_id, direction)?;
            return Ok(());
        }

        // If dragging file explorer border, update width
        if self.mouse_state.dragging_file_explorer {
            self.handle_file_explorer_border_drag(col)?;
            return Ok(());
        }

        // If dragging to select text
        if self.mouse_state.dragging_text_selection {
            self.handle_text_selection_drag(col, row)?;
            return Ok(());
        }

        // If dragging a tab, update position and compute drop zone
        if self.mouse_state.dragging_tab.is_some() {
            self.handle_tab_drag(col, row)?;
            return Ok(());
        }

        Ok(())
    }

    /// Handle text selection drag - extends selection from anchor to current position
    fn handle_text_selection_drag(&mut self, col: u16, row: u16) -> AnyhowResult<()> {
        use crate::model::event::Event;

        let Some(split_id) = self.mouse_state.drag_selection_split else {
            return Ok(());
        };
        let Some(anchor_position) = self.mouse_state.drag_selection_anchor else {
            return Ok(());
        };

        // Find the buffer for this split
        let buffer_id = self
            .cached_layout
            .split_areas
            .iter()
            .find(|(sid, _, _, _, _, _)| *sid == split_id)
            .map(|(_, bid, _, _, _, _)| *bid);

        let Some(buffer_id) = buffer_id else {
            return Ok(());
        };

        // Find the content rect for this split
        let content_rect = self
            .cached_layout
            .split_areas
            .iter()
            .find(|(sid, _, _, _, _, _)| *sid == split_id)
            .map(|(_, _, rect, _, _, _)| *rect);

        let Some(content_rect) = content_rect else {
            return Ok(());
        };

        // Get cached view line mappings for this split
        let cached_mappings = self
            .cached_layout
            .view_line_mappings
            .get(&split_id)
            .cloned();

        // Get fallback from SplitViewState viewport
        let fallback = self
            .split_view_states
            .get(&split_id)
            .map(|vs| vs.viewport.top_byte)
            .unwrap_or(0);

        // Calculate the target position from screen coordinates
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            let gutter_width = state.margins.left_total_width() as u16;

            let Some(target_position) = Self::screen_to_buffer_position(
                col,
                row,
                content_rect,
                gutter_width,
                &cached_mappings,
                fallback,
                true, // Allow gutter clicks for drag selection
            ) else {
                return Ok(());
            };

            // Move cursor to target position while keeping anchor to create selection
            let primary_cursor_id = state.cursors.primary_id();
            let event = Event::MoveCursor {
                cursor_id: primary_cursor_id,
                old_position: 0,
                new_position: target_position,
                old_anchor: None,
                new_anchor: Some(anchor_position), // Keep anchor to maintain selection
                old_sticky_column: 0,
                new_sticky_column: 0,
            };

            if let Some(event_log) = self.event_logs.get_mut(&buffer_id) {
                event_log.append(event.clone());
            }
            state.apply(&event);
        }

        Ok(())
    }

    /// Handle file explorer border drag for resizing
    pub(super) fn handle_file_explorer_border_drag(&mut self, col: u16) -> AnyhowResult<()> {
        let Some((start_col, _start_row)) = self.mouse_state.drag_start_position else {
            return Ok(());
        };
        let Some(start_width) = self.mouse_state.drag_start_explorer_width else {
            return Ok(());
        };

        // Calculate the delta in screen space
        let delta = col as i32 - start_col as i32;
        let total_width = self.terminal_width as i32;

        if total_width > 0 {
            // Convert screen delta to percentage delta
            let percent_delta = delta as f32 / total_width as f32;
            // Clamp the new width between 10% and 50%
            let new_width = (start_width + percent_delta).clamp(0.1, 0.5);
            self.file_explorer_width_percent = new_width;
        }

        Ok(())
    }

    /// Handle separator drag for split resizing
    pub(super) fn handle_separator_drag(
        &mut self,
        col: u16,
        row: u16,
        split_id: SplitId,
        direction: SplitDirection,
    ) -> AnyhowResult<()> {
        let Some((start_col, start_row)) = self.mouse_state.drag_start_position else {
            return Ok(());
        };
        let Some(start_ratio) = self.mouse_state.drag_start_ratio else {
            return Ok(());
        };
        let Some(editor_area) = self.cached_layout.editor_content_area else {
            return Ok(());
        };

        // Calculate the delta in screen space
        let (delta, total_size) = match direction {
            SplitDirection::Horizontal => {
                // For horizontal splits, we move the separator up/down (row changes)
                let delta = row as i32 - start_row as i32;
                let total = editor_area.height as i32;
                (delta, total)
            }
            SplitDirection::Vertical => {
                // For vertical splits, we move the separator left/right (col changes)
                let delta = col as i32 - start_col as i32;
                let total = editor_area.width as i32;
                (delta, total)
            }
        };

        // Convert screen delta to ratio delta
        // The ratio represents the fraction of space the first split gets
        if total_size > 0 {
            let ratio_delta = delta as f32 / total_size as f32;
            let new_ratio = (start_ratio + ratio_delta).clamp(0.1, 0.9);

            // Update the split ratio
            let _ = self.split_manager.set_ratio(split_id, new_ratio);
        }

        Ok(())
    }

    /// Handle right-click event
    pub(super) fn handle_right_click(&mut self, col: u16, row: u16) -> AnyhowResult<()> {
        // First check if a tab context menu is open and the click is on a menu item
        if let Some(ref menu) = self.tab_context_menu {
            let menu_x = menu.position.0;
            let menu_y = menu.position.1;
            let menu_width = 22u16; // "Close to the Right" + padding
            let menu_height = super::types::TabContextMenuItem::all().len() as u16 + 2; // items + borders

            // Check if click is inside the menu
            if col >= menu_x
                && col < menu_x + menu_width
                && row >= menu_y
                && row < menu_y + menu_height
            {
                // Click inside menu - let left-click handler deal with it
                return Ok(());
            }
        }

        // Check if right-click is on a tab
        let tab_click = self.cached_layout.tab_areas.iter().find_map(
            |(split_id, buffer_id, tab_row, start_col, end_col, _close_start)| {
                if row == *tab_row && col >= *start_col && col < *end_col {
                    Some((*split_id, *buffer_id))
                } else {
                    None
                }
            },
        );

        if let Some((split_id, buffer_id)) = tab_click {
            // Open tab context menu
            self.tab_context_menu = Some(TabContextMenu::new(buffer_id, split_id, col, row + 1));
        } else {
            // Click outside tab - close context menu if open
            self.tab_context_menu = None;
        }

        Ok(())
    }

    /// Handle left-click on tab context menu
    pub(super) fn handle_tab_context_menu_click(
        &mut self,
        col: u16,
        row: u16,
    ) -> Option<AnyhowResult<()>> {
        let menu = self.tab_context_menu.as_ref()?;
        let menu_x = menu.position.0;
        let menu_y = menu.position.1;
        let menu_width = 22u16;
        let items = super::types::TabContextMenuItem::all();
        let menu_height = items.len() as u16 + 2; // items + borders

        // Check if click is inside the menu area
        if col < menu_x || col >= menu_x + menu_width || row < menu_y || row >= menu_y + menu_height
        {
            // Click outside menu - close it
            self.tab_context_menu = None;
            return Some(Ok(()));
        }

        // Check if click is on the border (first or last row)
        if row == menu_y || row == menu_y + menu_height - 1 {
            return Some(Ok(()));
        }

        // Calculate which item was clicked (accounting for border)
        let item_idx = (row - menu_y - 1) as usize;
        if item_idx >= items.len() {
            return Some(Ok(()));
        }

        // Get the menu state before closing it
        let buffer_id = menu.buffer_id;
        let split_id = menu.split_id;
        let item = items[item_idx];

        // Close the menu
        self.tab_context_menu = None;

        // Execute the action
        Some(self.execute_tab_context_menu_action(item, buffer_id, split_id))
    }

    /// Execute a tab context menu action
    fn execute_tab_context_menu_action(
        &mut self,
        item: super::types::TabContextMenuItem,
        buffer_id: BufferId,
        split_id: SplitId,
    ) -> AnyhowResult<()> {
        use super::types::TabContextMenuItem;

        match item {
            TabContextMenuItem::Close => {
                self.close_tab_in_split(buffer_id, split_id);
            }
            TabContextMenuItem::CloseOthers => {
                self.close_other_tabs_in_split(buffer_id, split_id);
            }
            TabContextMenuItem::CloseToRight => {
                self.close_tabs_to_right_in_split(buffer_id, split_id);
            }
            TabContextMenuItem::CloseToLeft => {
                self.close_tabs_to_left_in_split(buffer_id, split_id);
            }
            TabContextMenuItem::CloseAll => {
                self.close_all_tabs_in_split(split_id);
            }
        }

        Ok(())
    }
}
