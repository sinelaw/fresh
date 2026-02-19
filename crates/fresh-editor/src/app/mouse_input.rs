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
use crate::model::event::{ContainerId, CursorId, LeafId, SplitDirection};
use crate::services::plugins::hooks::HookArgs;
use crate::view::popup_mouse::{popup_areas_to_layout_info, PopupHitTester};
use crate::view::prompt::PromptType;
use crate::view::ui::tabs::TabHit;
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

        // Detect multi-click (double/triple) for left button down events
        let (is_double_click, is_triple_click) =
            if matches!(mouse_event.kind, MouseEventKind::Down(MouseButton::Left)) {
                let now = self.time_source.now();
                let is_consecutive = if let (Some(previous_time), Some(previous_pos)) =
                    (self.previous_click_time, self.previous_click_position)
                {
                    let threshold =
                        std::time::Duration::from_millis(self.config.editor.double_click_time_ms);
                    let within_time = now.duration_since(previous_time) < threshold;
                    let same_position = previous_pos == (col, row);
                    within_time && same_position
                } else {
                    false
                };

                // Update click tracking
                if is_consecutive {
                    self.click_count += 1;
                } else {
                    self.click_count = 1;
                }
                self.previous_click_time = Some(now);
                self.previous_click_position = Some((col, row));

                let is_triple = self.click_count >= 3;
                let is_double = self.click_count == 2;

                if is_triple {
                    // Reset after triple-click so the next click starts fresh
                    self.click_count = 0;
                    self.previous_click_time = None;
                    self.previous_click_position = None;
                }

                (is_double, is_triple)
            } else {
                (false, false)
            };

        // When keybinding editor is open, capture all mouse events
        if self.keybinding_editor.is_some() {
            return self.handle_keybinding_editor_mouse(mouse_event);
        }

        // When settings modal is open, capture all mouse events
        if self.settings_state.as_ref().is_some_and(|s| s.visible) {
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

        // Check if we should forward mouse events to the terminal
        // Forward if: in terminal mode, mouse is over terminal buffer, and terminal is in alternate screen mode
        if let Some(result) = self.try_forward_mouse_to_terminal(col, row, mouse_event) {
            return result;
        }

        match mouse_event.kind {
            MouseEventKind::Down(MouseButton::Left) => {
                if is_triple_click {
                    // Triple click detected - select entire line
                    self.handle_mouse_triple_click(col, row)?;
                    needs_render = true;
                    return Ok(needs_render);
                }
                if is_double_click {
                    // Double click detected - both clicks within time threshold AND at same position
                    self.handle_mouse_double_click(col, row)?;
                    needs_render = true;
                    return Ok(needs_render);
                }
                self.handle_mouse_click(col, row, mouse_event.modifiers)?;
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
                self.mouse_state.dragging_horizontal_scrollbar = None;
                self.mouse_state.drag_start_hcol = None;
                self.mouse_state.drag_start_left_column = None;
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
                // Clear popup text selection drag state (selection remains in popup)
                self.mouse_state.selecting_in_popup = None;

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
                // Shift+ScrollUp => horizontal scroll left
                if mouse_event
                    .modifiers
                    .contains(crossterm::event::KeyModifiers::SHIFT)
                {
                    self.handle_horizontal_scroll(col, row, -3)?;
                    needs_render = true;
                } else if self.handle_prompt_scroll(-3) {
                    // Check if prompt with suggestions is active and should handle scroll
                    needs_render = true;
                } else if self.is_file_open_active() && self.handle_file_open_scroll(-3) {
                    // Check if file browser is active and should handle scroll
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
                    needs_render = true;
                }
            }
            MouseEventKind::ScrollDown => {
                // Shift+ScrollDown => horizontal scroll right
                if mouse_event
                    .modifiers
                    .contains(crossterm::event::KeyModifiers::SHIFT)
                {
                    self.handle_horizontal_scroll(col, row, 3)?;
                    needs_render = true;
                } else if self.handle_prompt_scroll(3) {
                    // Check if prompt with suggestions is active and should handle scroll
                    needs_render = true;
                } else if self.is_file_open_active() && self.handle_file_open_scroll(3) {
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
                    needs_render = true;
                }
            }
            MouseEventKind::ScrollLeft => {
                // Native horizontal scroll left
                self.handle_horizontal_scroll(col, row, -3)?;
                needs_render = true;
            }
            MouseEventKind::ScrollRight => {
                // Native horizontal scroll right
                self.handle_horizontal_scroll(col, row, 3)?;
                needs_render = true;
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
        if let Some(HoverTarget::TabContextMenuItem(item_idx)) = new_target.clone() {
            if let Some(ref mut menu) = self.tab_context_menu {
                if menu.highlighted != item_idx {
                    menu.highlighted = item_idx;
                    return true;
                }
            }
        }

        // Handle file explorer status indicator hover - show tooltip
        // Always dismiss existing tooltip first when target changes
        if old_target != new_target
            && matches!(
                old_target,
                Some(HoverTarget::FileExplorerStatusIndicator(_))
            )
        {
            self.dismiss_file_explorer_status_tooltip();
        }

        if let Some(HoverTarget::FileExplorerStatusIndicator(ref path)) = new_target {
            // Only show tooltip if this is a new hover (not already showing for this path)
            if old_target != new_target {
                self.show_file_explorer_status_tooltip(path.clone(), col, row);
                return true;
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
    ///
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

        // Get compose width for this split
        let compose_width = self
            .split_view_states
            .get(&split_id)
            .and_then(|vs| vs.compose_width);

        // Convert screen position to buffer byte position
        let Some(byte_pos) = Self::screen_to_buffer_position(
            col,
            row,
            content_rect,
            gutter_width,
            &cached_mappings,
            fallback,
            false, // Don't include gutter
            compose_width,
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
        let layouts = popup_areas_to_layout_info(&self.cached_layout.popup_areas);
        let hit_tester = PopupHitTester::new(&layouts, &self.active_state().popups);
        hit_tester.is_over_transient_popup(col, row)
    }

    /// Check if mouse position is over any popup (including non-transient ones like completion)
    fn is_mouse_over_any_popup(&self, col: u16, row: u16) -> bool {
        let layouts = popup_areas_to_layout_info(&self.cached_layout.popup_areas);
        let hit_tester = PopupHitTester::new(&layouts, &self.active_state().popups);
        hit_tester.is_over_popup(col, row)
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

        // Check menu bar (row 0, only when visible)
        // Check menu bar using cached layout from previous render
        if self.menu_bar_visible {
            if let Some(ref menu_layout) = self.cached_layout.menu_layout {
                if let Some(menu_idx) = menu_layout.menu_at(col, row) {
                    return Some(HoverTarget::MenuBarItem(menu_idx));
                }
            }
        }

        // Check menu dropdown items if a menu is open (including submenus)
        if let Some(active_idx) = self.menu_state.active_menu {
            if let Some(hover) = self.compute_menu_dropdown_hover(col, row, active_idx) {
                return Some(hover);
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

            // Check if hovering over a status indicator in the file explorer content area
            // Status indicators are in the rightmost 2 characters of each row (before border)
            let content_start_y = explorer_area.y + 1; // +1 for title bar
            let content_end_y = explorer_area.y + explorer_area.height.saturating_sub(1); // -1 for bottom border
            let status_indicator_x = explorer_area.x + explorer_area.width.saturating_sub(3); // 2 chars + 1 border

            if row >= content_start_y
                && row < content_end_y
                && col >= status_indicator_x
                && col < explorer_area.x + explorer_area.width.saturating_sub(1)
            {
                // Determine which item is at this row
                if let Some(ref explorer) = self.file_explorer {
                    let relative_row = row.saturating_sub(content_start_y) as usize;
                    let scroll_offset = explorer.get_scroll_offset();
                    let item_index = relative_row + scroll_offset;
                    let display_nodes = explorer.get_display_nodes();

                    if item_index < display_nodes.len() {
                        let (node_id, _indent) = display_nodes[item_index];
                        if let Some(node) = explorer.tree().get_node(node_id) {
                            return Some(HoverTarget::FileExplorerStatusIndicator(
                                node.entry.path.clone(),
                            ));
                        }
                    }
                }
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

        for (split_id, tab_layout) in &self.cached_layout.tab_layouts {
            match tab_layout.hit_test(col, row) {
                Some(TabHit::CloseButton(buffer_id)) => {
                    return Some(HoverTarget::TabCloseButton(buffer_id, *split_id));
                }
                Some(TabHit::TabName(buffer_id)) => {
                    return Some(HoverTarget::TabName(buffer_id, *split_id));
                }
                Some(TabHit::ScrollLeft)
                | Some(TabHit::ScrollRight)
                | Some(TabHit::BarBackground)
                | None => {}
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

                // Check encoding indicator area
                if let Some((enc_row, enc_start, enc_end)) =
                    self.cached_layout.status_bar_encoding_area
                {
                    if row == enc_row && col >= enc_start && col < enc_end {
                        return Some(HoverTarget::StatusBarEncodingIndicator);
                    }
                }

                // Check language indicator area
                if let Some((lang_row, lang_start, lang_end)) =
                    self.cached_layout.status_bar_language_area
                {
                    if row == lang_row && col >= lang_start && col < lang_end {
                        return Some(HoverTarget::StatusBarLanguageIndicator);
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

        // Handle popups: dismiss if clicking outside, block if clicking inside
        if self.is_mouse_over_any_popup(col, row) {
            // Double-click inside popup - block from reaching editor
            return Ok(());
        } else {
            // Double-click outside popup - dismiss transient popups
            self.dismiss_transient_popups();
        }

        // Is it in the file open dialog?
        if self.handle_file_open_double_click(col, row) {
            return Ok(());
        }

        // Is it in the file explorer? Double-click opens file AND focuses editor
        if let Some(explorer_area) = self.cached_layout.file_explorer_area {
            if col >= explorer_area.x
                && col < explorer_area.x + explorer_area.width
                && row > explorer_area.y // Skip title bar
                && row < explorer_area.y + explorer_area.height
            {
                // Open file and focus editor (via file_explorer_open_file which calls focus_editor)
                self.file_explorer_open_file()?;
                return Ok(());
            }
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
        split_id: LeafId,
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
        let leaf_id = split_id;
        let fallback = self
            .split_view_states
            .get(&leaf_id)
            .map(|vs| vs.viewport.top_byte)
            .unwrap_or(0);

        // Get compose width for this split
        let compose_width = self
            .split_view_states
            .get(&leaf_id)
            .and_then(|vs| vs.compose_width);

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
                compose_width,
            ) else {
                return Ok(());
            };

            // Move cursor to clicked position first
            let primary_cursor_id = self
                .split_view_states
                .get(&leaf_id)
                .map(|vs| vs.cursors.primary_id())
                .unwrap_or(CursorId(0));
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
            if let Some(cursors) = self
                .split_view_states
                .get_mut(&leaf_id)
                .map(|vs| &mut vs.cursors)
            {
                state.apply(cursors, &event);
            }
        }

        // Now select the word under cursor
        self.handle_action(Action::SelectWord)?;

        Ok(())
    }
    /// Handle mouse triple click (down event)
    /// Triple-click in editor area selects the entire line under the cursor.
    pub(super) fn handle_mouse_triple_click(&mut self, col: u16, row: u16) -> AnyhowResult<()> {
        tracing::debug!("handle_mouse_triple_click at col={}, row={}", col, row);

        // Handle popups: dismiss if clicking outside, block if clicking inside
        if self.is_mouse_over_any_popup(col, row) {
            return Ok(());
        } else {
            self.dismiss_transient_popups();
        }

        // Find which split/buffer was clicked
        let split_areas = self.cached_layout.split_areas.clone();
        for (split_id, buffer_id, content_rect, _scrollbar_rect, _thumb_start, _thumb_end) in
            &split_areas
        {
            if col >= content_rect.x
                && col < content_rect.x + content_rect.width
                && row >= content_rect.y
                && row < content_rect.y + content_rect.height
            {
                if self.is_terminal_buffer(*buffer_id) {
                    return Ok(());
                }

                self.key_context = crate::input::keybindings::KeyContext::Normal;

                // Use the same pattern as handle_editor_double_click:
                // first focus and position cursor, then select line
                self.handle_editor_triple_click(col, row, *split_id, *buffer_id, *content_rect)?;
                return Ok(());
            }
        }

        Ok(())
    }

    /// Handle triple-click in editor content area - selects the entire line under cursor
    fn handle_editor_triple_click(
        &mut self,
        col: u16,
        row: u16,
        split_id: LeafId,
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

        let leaf_id = split_id;
        let fallback = self
            .split_view_states
            .get(&leaf_id)
            .map(|vs| vs.viewport.top_byte)
            .unwrap_or(0);

        // Get compose width for this split
        let compose_width = self
            .split_view_states
            .get(&leaf_id)
            .and_then(|vs| vs.compose_width);

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
                true,
                compose_width,
            ) else {
                return Ok(());
            };

            // Move cursor to clicked position first
            let primary_cursor_id = self
                .split_view_states
                .get(&leaf_id)
                .map(|vs| vs.cursors.primary_id())
                .unwrap_or(CursorId(0));
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
            if let Some(cursors) = self
                .split_view_states
                .get_mut(&leaf_id)
                .map(|vs| &mut vs.cursors)
            {
                state.apply(cursors, &event);
            }
        }

        // Now select the entire line
        self.handle_action(Action::SelectLine)?;

        Ok(())
    }

    /// Handle mouse click (down event)
    pub(super) fn handle_mouse_click(
        &mut self,
        col: u16,
        row: u16,
        modifiers: crossterm::event::KeyModifiers,
    ) -> AnyhowResult<()> {
        // Check if click is on tab context menu first
        if self.tab_context_menu.is_some() {
            if let Some(result) = self.handle_tab_context_menu_click(col, row) {
                return result;
            }
        }

        // Dismiss transient popups (like hover) when clicking outside them
        // This check must happen before we process the click elsewhere
        if !self.is_mouse_over_any_popup(col, row) {
            self.dismiss_transient_popups();
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
        for (popup_idx, _popup_rect, inner_rect, scroll_offset, num_items, _, _) in
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

                // For text/markdown popups, start text selection
                let is_text_popup = {
                    let state = self.active_state();
                    state.popups.top().is_some_and(|p| {
                        matches!(
                            p.content,
                            crate::view::popup::PopupContent::Text(_)
                                | crate::view::popup::PopupContent::Markdown(_)
                        )
                    })
                };

                if is_text_popup {
                    let line = scroll_offset + relative_row;
                    let popup_idx_copy = *popup_idx; // Copy before mutable borrow
                    let state = self.active_state_mut();
                    if let Some(popup) = state.popups.top_mut() {
                        popup.start_selection(line, relative_col);
                    }
                    // Track that we're selecting in a popup
                    self.mouse_state.selecting_in_popup = Some(popup_idx_copy);
                    return Ok(());
                }
            }
        }

        // If click is inside a popup's outer bounds but wasn't handled above,
        // block it from reaching the editor (e.g., clicking on popup border)
        if self.is_mouse_over_any_popup(col, row) {
            return Ok(());
        }

        // Check if click is on the file browser popup
        if self.is_file_open_active() && self.handle_file_open_click(col, row) {
            return Ok(());
        }

        // Check if click is on menu bar using cached layout
        if self.menu_bar_visible {
            if let Some(ref menu_layout) = self.cached_layout.menu_layout {
                if let Some(menu_idx) = menu_layout.menu_at(col, row) {
                    // Toggle menu: if same menu is open, close it; otherwise open clicked menu
                    if self.menu_state.active_menu == Some(menu_idx) {
                        self.close_menu_with_auto_hide();
                    } else {
                        // Dismiss transient popups and clear hover state when opening menu
                        self.on_editor_focus_lost();
                        self.menu_state.open_menu(menu_idx);
                    }
                    return Ok(());
                } else if row == 0 {
                    // Clicked on menu bar background but not on a menu label - close any open menu
                    self.close_menu_with_auto_hide();
                    return Ok(());
                }
            }
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
                if let Some(click_result) = self.handle_menu_dropdown_click(col, row, menu)? {
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
                    self.mouse_state.drag_start_view_line_offset =
                        Some(view_state.viewport.top_view_line_offset);
                }
            } else {
                // Click on track - jump to position
                self.mouse_state.dragging_scrollbar = Some(split_id);
                self.handle_scrollbar_jump(col, row, split_id, buffer_id, scrollbar_rect)?;
            }
            return Ok(());
        }

        // Check if click is on horizontal scrollbar
        let hscrollbar_hit = self
            .cached_layout
            .horizontal_scrollbar_areas
            .iter()
            .find_map(
                |(
                    split_id,
                    buffer_id,
                    hscrollbar_rect,
                    max_content_width,
                    thumb_start,
                    thumb_end,
                )| {
                    if col >= hscrollbar_rect.x
                        && col < hscrollbar_rect.x + hscrollbar_rect.width
                        && row >= hscrollbar_rect.y
                        && row < hscrollbar_rect.y + hscrollbar_rect.height
                    {
                        let relative_col = col.saturating_sub(hscrollbar_rect.x) as usize;
                        let is_on_thumb = relative_col >= *thumb_start && relative_col < *thumb_end;
                        Some((
                            *split_id,
                            *buffer_id,
                            *hscrollbar_rect,
                            *max_content_width,
                            is_on_thumb,
                        ))
                    } else {
                        None
                    }
                },
            );

        if let Some((split_id, buffer_id, hscrollbar_rect, max_content_width, is_on_thumb)) =
            hscrollbar_hit
        {
            self.focus_split(split_id, buffer_id);
            self.mouse_state.dragging_horizontal_scrollbar = Some(split_id);

            if is_on_thumb {
                // Click on thumb - start drag from current position (don't jump)
                self.mouse_state.drag_start_hcol = Some(col);
                if let Some(view_state) = self.split_view_states.get(&split_id) {
                    self.mouse_state.drag_start_left_column = Some(view_state.viewport.left_column);
                }
            } else {
                // Click on track - jump to position
                self.mouse_state.drag_start_hcol = None;
                self.mouse_state.drag_start_left_column = None;

                let relative_col = col.saturating_sub(hscrollbar_rect.x) as f64;
                let track_width = hscrollbar_rect.width as f64;
                let ratio = if track_width > 1.0 {
                    (relative_col / (track_width - 1.0)).clamp(0.0, 1.0)
                } else {
                    0.0
                };

                if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
                    let visible_width = view_state.viewport.width as usize;
                    let max_scroll = max_content_width.saturating_sub(visible_width);
                    let target_col = (ratio * max_scroll as f64).round() as usize;
                    view_state.viewport.left_column = target_col.min(max_scroll);
                    view_state.viewport.set_skip_ensure_visible();
                }
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

                // Check encoding indicator - click opens encoding selector
                if let Some((enc_row, enc_start, enc_end)) =
                    self.cached_layout.status_bar_encoding_area
                {
                    if row == enc_row && col >= enc_start && col < enc_end {
                        return self.handle_action(Action::SetEncoding);
                    }
                }

                // Check language indicator - click opens language selector
                if let Some((lang_row, lang_start, lang_end)) =
                    self.cached_layout.status_bar_language_area
                {
                    if row == lang_row && col >= lang_start && col < lang_end {
                        return self.handle_action(Action::SetLanguage);
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

                // Check message area - click opens status log
                if let Some((msg_row, msg_start, msg_end)) =
                    self.cached_layout.status_bar_message_area
                {
                    if row == msg_row && col >= msg_start && col < msg_end {
                        return self.handle_action(Action::ShowStatusLog);
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
                if let Some(ratio) = self.split_manager.get_ratio((*split_id).into()) {
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

        // Check if click is on a tab using cached tab layouts (computed during rendering)
        // Debug: show tab layout info
        for (split_id, tab_layout) in &self.cached_layout.tab_layouts {
            tracing::debug!(
                "Tab layout for split {:?}: bar_area={:?}, left_scroll={:?}, right_scroll={:?}",
                split_id,
                tab_layout.bar_area,
                tab_layout.left_scroll_area,
                tab_layout.right_scroll_area
            );
        }

        let tab_hit = self
            .cached_layout
            .tab_layouts
            .iter()
            .find_map(|(split_id, tab_layout)| {
                let hit = tab_layout.hit_test(col, row);
                tracing::debug!(
                    "Tab hit_test at ({}, {}) for split {:?} returned {:?}",
                    col,
                    row,
                    split_id,
                    hit
                );
                hit.map(|h| (*split_id, h))
            });

        if let Some((split_id, hit)) = tab_hit {
            match hit {
                TabHit::CloseButton(buffer_id) => {
                    self.focus_split(split_id, buffer_id);
                    self.close_tab_in_split(buffer_id, split_id);
                    return Ok(());
                }
                TabHit::TabName(buffer_id) => {
                    self.focus_split(split_id, buffer_id);
                    // Start potential tab drag (will only become active after moving threshold)
                    self.mouse_state.dragging_tab = Some(super::types::TabDragState::new(
                        buffer_id,
                        split_id,
                        (col, row),
                    ));
                    return Ok(());
                }
                TabHit::ScrollLeft => {
                    // Scroll tabs left by one tab width (use 5 chars as estimate)
                    self.set_status_message("ScrollLeft clicked!".to_string());
                    if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
                        view_state.tab_scroll_offset =
                            view_state.tab_scroll_offset.saturating_sub(10);
                    }
                    return Ok(());
                }
                TabHit::ScrollRight => {
                    // Scroll tabs right by one tab width (use 5 chars as estimate)
                    self.set_status_message("ScrollRight clicked!".to_string());
                    if let Some(view_state) = self.split_view_states.get_mut(&split_id) {
                        view_state.tab_scroll_offset =
                            view_state.tab_scroll_offset.saturating_add(10);
                    }
                    return Ok(());
                }
                TabHit::BarBackground => {}
            }
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
                self.handle_editor_click(
                    col,
                    row,
                    *split_id,
                    *buffer_id,
                    *content_rect,
                    modifiers,
                )?;
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

        // If dragging horizontal scrollbar, update horizontal scroll position
        if let Some(dragging_split_id) = self.mouse_state.dragging_horizontal_scrollbar {
            for (
                split_id,
                _buffer_id,
                hscrollbar_rect,
                max_content_width,
                thumb_start,
                thumb_end,
            ) in &self.cached_layout.horizontal_scrollbar_areas
            {
                if *split_id == dragging_split_id {
                    let track_width = hscrollbar_rect.width as f64;
                    if track_width <= 1.0 {
                        break;
                    }

                    if let (Some(drag_start_hcol), Some(drag_start_left_column)) = (
                        self.mouse_state.drag_start_hcol,
                        self.mouse_state.drag_start_left_column,
                    ) {
                        // Relative drag from thumb - move proportionally to mouse offset
                        // Use thumb size to compute the correct ratio so thumb tracks with mouse
                        let col_offset = (col as i32) - (drag_start_hcol as i32);
                        if let Some(view_state) = self.split_view_states.get_mut(&dragging_split_id)
                        {
                            let visible_width = view_state.viewport.width as usize;
                            let max_scroll = max_content_width.saturating_sub(visible_width);
                            if max_scroll > 0 {
                                let thumb_size = thumb_end.saturating_sub(*thumb_start).max(1);
                                let track_travel = (track_width - thumb_size as f64).max(1.0);
                                let scroll_per_pixel = max_scroll as f64 / track_travel;
                                let scroll_offset =
                                    (col_offset as f64 * scroll_per_pixel).round() as i64;
                                let new_left =
                                    (drag_start_left_column as i64 + scroll_offset).max(0) as usize;
                                view_state.viewport.left_column = new_left.min(max_scroll);
                                view_state.viewport.set_skip_ensure_visible();
                            }
                        }
                    } else {
                        // Jump drag (started from track) - jump to absolute position
                        let relative_col = col.saturating_sub(hscrollbar_rect.x) as f64;
                        let ratio = (relative_col / (track_width - 1.0)).clamp(0.0, 1.0);

                        if let Some(view_state) = self.split_view_states.get_mut(&dragging_split_id)
                        {
                            let visible_width = view_state.viewport.width as usize;
                            let max_scroll = max_content_width.saturating_sub(visible_width);
                            let target_col = (ratio * max_scroll as f64).round() as usize;
                            view_state.viewport.left_column = target_col.min(max_scroll);
                            view_state.viewport.set_skip_ensure_visible();
                        }
                    }

                    return Ok(());
                }
            }
        }

        // If selecting text in popup, extend selection
        if let Some(popup_idx) = self.mouse_state.selecting_in_popup {
            // Find the popup area from cached layout
            if let Some((_, _, inner_rect, scroll_offset, _, _, _)) = self
                .cached_layout
                .popup_areas
                .iter()
                .find(|(idx, _, _, _, _, _, _)| *idx == popup_idx)
            {
                // Check if mouse is within the popup inner area
                if col >= inner_rect.x
                    && col < inner_rect.x + inner_rect.width
                    && row >= inner_rect.y
                    && row < inner_rect.y + inner_rect.height
                {
                    let relative_col = (col - inner_rect.x) as usize;
                    let relative_row = (row - inner_rect.y) as usize;
                    let line = scroll_offset + relative_row;

                    let state = self.active_state_mut();
                    if let Some(popup) = state.popups.get_mut(popup_idx) {
                        popup.extend_selection(line, relative_col);
                    }
                }
            }
            return Ok(());
        }

        // If dragging popup scrollbar, update popup scroll position
        if let Some(popup_idx) = self.mouse_state.dragging_popup_scrollbar {
            // Find the popup's scrollbar rect from cached layout
            if let Some((_, _, inner_rect, _, _, Some(sb_rect), total_lines)) = self
                .cached_layout
                .popup_areas
                .iter()
                .find(|(idx, _, _, _, _, _, _)| *idx == popup_idx)
            {
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

        let leaf_id = split_id;

        // Get fallback from SplitViewState viewport
        let fallback = self
            .split_view_states
            .get(&leaf_id)
            .map(|vs| vs.viewport.top_byte)
            .unwrap_or(0);

        // Get compose width for this split
        let compose_width = self
            .split_view_states
            .get(&leaf_id)
            .and_then(|vs| vs.compose_width);

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
                compose_width,
            ) else {
                return Ok(());
            };

            // Move cursor to target position while keeping anchor to create selection
            let primary_cursor_id = self
                .split_view_states
                .get(&leaf_id)
                .map(|vs| vs.cursors.primary_id())
                .unwrap_or(CursorId(0));
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
            if let Some(cursors) = self
                .split_view_states
                .get_mut(&leaf_id)
                .map(|vs| &mut vs.cursors)
            {
                state.apply(cursors, &event);
            }
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
        split_id: ContainerId,
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
            self.split_manager.set_ratio(split_id, new_ratio);
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
        let tab_hit =
            self.cached_layout.tab_layouts.iter().find_map(
                |(split_id, tab_layout)| match tab_layout.hit_test(col, row) {
                    Some(TabHit::TabName(buffer_id) | TabHit::CloseButton(buffer_id)) => {
                        Some((*split_id, buffer_id))
                    }
                    _ => None,
                },
            );

        if let Some((split_id, buffer_id)) = tab_hit {
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
        leaf_id: LeafId,
    ) -> AnyhowResult<()> {
        use super::types::TabContextMenuItem;
        match item {
            TabContextMenuItem::Close => {
                self.close_tab_in_split(buffer_id, leaf_id);
            }
            TabContextMenuItem::CloseOthers => {
                self.close_other_tabs_in_split(buffer_id, leaf_id);
            }
            TabContextMenuItem::CloseToRight => {
                self.close_tabs_to_right_in_split(buffer_id, leaf_id);
            }
            TabContextMenuItem::CloseToLeft => {
                self.close_tabs_to_left_in_split(buffer_id, leaf_id);
            }
            TabContextMenuItem::CloseAll => {
                self.close_all_tabs_in_split(leaf_id);
            }
        }

        Ok(())
    }

    /// Show a tooltip for a file explorer status indicator
    fn show_file_explorer_status_tooltip(&mut self, path: std::path::PathBuf, col: u16, row: u16) {
        use crate::view::popup::{Popup, PopupPosition};
        use ratatui::style::Style;

        let is_directory = path.is_dir();

        // Get the decoration for this file to determine the status
        let decoration = self
            .file_explorer_decoration_cache
            .direct_for_path(&path)
            .cloned();

        // For directories, also check bubbled decoration
        let bubbled_decoration = if is_directory && decoration.is_none() {
            self.file_explorer_decoration_cache
                .bubbled_for_path(&path)
                .cloned()
        } else {
            None
        };

        // Check if file/folder has unsaved changes in editor
        let has_unsaved_changes = if is_directory {
            // Check if any buffer under this directory has unsaved changes
            self.buffers.iter().any(|(buffer_id, state)| {
                if state.buffer.is_modified() {
                    if let Some(metadata) = self.buffer_metadata.get(buffer_id) {
                        if let Some(file_path) = metadata.file_path() {
                            return file_path.starts_with(&path);
                        }
                    }
                }
                false
            })
        } else {
            self.buffers.iter().any(|(buffer_id, state)| {
                if state.buffer.is_modified() {
                    if let Some(metadata) = self.buffer_metadata.get(buffer_id) {
                        return metadata.file_path() == Some(&path);
                    }
                }
                false
            })
        };

        // Build tooltip content
        let mut lines: Vec<String> = Vec::new();

        if let Some(decoration) = &decoration {
            let symbol = &decoration.symbol;
            let explanation = match symbol.as_str() {
                "U" => "Untracked - File is not tracked by git",
                "M" => "Modified - File has unstaged changes",
                "A" => "Added - File is staged for commit",
                "D" => "Deleted - File is staged for deletion",
                "R" => "Renamed - File has been renamed",
                "C" => "Copied - File has been copied",
                "!" => "Conflicted - File has merge conflicts",
                "●" => "Has changes - Contains modified files",
                _ => "Unknown status",
            };
            lines.push(format!("{} - {}", symbol, explanation));
        } else if bubbled_decoration.is_some() {
            lines.push("● - Contains modified files".to_string());
        } else if has_unsaved_changes {
            if is_directory {
                lines.push("● - Contains unsaved changes".to_string());
            } else {
                lines.push("● - Unsaved changes in editor".to_string());
            }
        } else {
            return; // No status to show
        }

        // For directories, show list of modified files
        if is_directory {
            // get_modified_files_in_directory returns None if no files, so no need to check is_empty()
            if let Some(modified_files) = self.get_modified_files_in_directory(&path) {
                lines.push(String::new()); // Empty line separator
                lines.push("Modified files:".to_string());
                // Resolve symlinks for proper prefix stripping
                let resolved_path = path.canonicalize().unwrap_or_else(|_| path.clone());
                const MAX_FILES: usize = 8;
                for (i, file) in modified_files.iter().take(MAX_FILES).enumerate() {
                    // Show relative path from the directory
                    let display_name = file
                        .strip_prefix(&resolved_path)
                        .unwrap_or(file)
                        .to_string_lossy()
                        .to_string();
                    lines.push(format!("  {}", display_name));
                    if i == MAX_FILES - 1 && modified_files.len() > MAX_FILES {
                        lines.push(format!(
                            "  ... and {} more",
                            modified_files.len() - MAX_FILES
                        ));
                        break;
                    }
                }
            }
        } else {
            // For files, try to get git diff stats
            if let Some(stats) = self.get_git_diff_stats(&path) {
                lines.push(String::new()); // Empty line separator
                lines.push(stats);
            }
        }

        if lines.is_empty() {
            return;
        }

        // Create popup
        let mut popup = Popup::text(lines, &self.theme);
        popup.title = Some("Git Status".to_string());
        popup.transient = true;
        popup.position = PopupPosition::Fixed { x: col, y: row + 1 };
        popup.width = 50;
        popup.max_height = 15;
        popup.border_style = Style::default().fg(self.theme.popup_border_fg);
        popup.background_style = Style::default().bg(self.theme.popup_bg);

        // Show the popup
        if let Some(state) = self.buffers.get_mut(&self.active_buffer()) {
            state.popups.show(popup);
        }
    }

    /// Dismiss the file explorer status tooltip
    fn dismiss_file_explorer_status_tooltip(&mut self) {
        // Dismiss any transient popups
        if let Some(state) = self.buffers.get_mut(&self.active_buffer()) {
            state.popups.dismiss_transient();
        }
    }

    /// Get git diff stats for a file (insertions/deletions)
    fn get_git_diff_stats(&self, path: &std::path::Path) -> Option<String> {
        use std::process::Command;

        // Run git diff --numstat for the file
        let output = Command::new("git")
            .args(["diff", "--numstat", "--"])
            .arg(path)
            .current_dir(&self.working_dir)
            .output()
            .ok()?;

        if !output.status.success() {
            return None;
        }

        let stdout = String::from_utf8_lossy(&output.stdout);
        let line = stdout.lines().next()?;
        let parts: Vec<&str> = line.split('\t').collect();

        if parts.len() >= 2 {
            let insertions = parts[0];
            let deletions = parts[1];

            // Handle binary files (shows as -)
            if insertions == "-" && deletions == "-" {
                return Some("Binary file changed".to_string());
            }

            let ins: i32 = insertions.parse().unwrap_or(0);
            let del: i32 = deletions.parse().unwrap_or(0);

            if ins > 0 || del > 0 {
                return Some(format!("+{} -{} lines", ins, del));
            }
        }

        // Also check staged changes
        let staged_output = Command::new("git")
            .args(["diff", "--numstat", "--cached", "--"])
            .arg(path)
            .current_dir(&self.working_dir)
            .output()
            .ok()?;

        if staged_output.status.success() {
            let staged_stdout = String::from_utf8_lossy(&staged_output.stdout);
            if let Some(line) = staged_stdout.lines().next() {
                let parts: Vec<&str> = line.split('\t').collect();
                if parts.len() >= 2 {
                    let insertions = parts[0];
                    let deletions = parts[1];

                    if insertions == "-" && deletions == "-" {
                        return Some("Binary file staged".to_string());
                    }

                    let ins: i32 = insertions.parse().unwrap_or(0);
                    let del: i32 = deletions.parse().unwrap_or(0);

                    if ins > 0 || del > 0 {
                        return Some(format!("+{} -{} lines (staged)", ins, del));
                    }
                }
            }
        }

        None
    }

    /// Get list of modified files in a directory
    fn get_modified_files_in_directory(
        &self,
        dir_path: &std::path::Path,
    ) -> Option<Vec<std::path::PathBuf>> {
        use std::process::Command;

        // Resolve symlinks to get the actual directory path
        let resolved_path = dir_path
            .canonicalize()
            .unwrap_or_else(|_| dir_path.to_path_buf());

        // Run git status --porcelain to get list of modified files
        let output = Command::new("git")
            .args(["status", "--porcelain", "--"])
            .arg(&resolved_path)
            .current_dir(&self.working_dir)
            .output()
            .ok()?;

        if !output.status.success() {
            return None;
        }

        let stdout = String::from_utf8_lossy(&output.stdout);
        let modified_files: Vec<std::path::PathBuf> = stdout
            .lines()
            .filter_map(|line| {
                // Git porcelain format: XY filename
                // where XY is the status (M, A, D, ??, etc.)
                if line.len() > 3 {
                    let file_part = &line[3..];
                    // Handle renamed files (old -> new format)
                    let file_name = if file_part.contains(" -> ") {
                        file_part.split(" -> ").last().unwrap_or(file_part)
                    } else {
                        file_part
                    };
                    Some(self.working_dir.join(file_name))
                } else {
                    None
                }
            })
            .collect();

        if modified_files.is_empty() {
            None
        } else {
            Some(modified_files)
        }
    }
}
