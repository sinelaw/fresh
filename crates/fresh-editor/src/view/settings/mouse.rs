//! Mouse input handling for the Settings dialog.
//!
//! This module contains all mouse event handling for the settings modal,
//! including clicks, scrolling, and drag operations.

use crate::app::Editor;
use anyhow::Result as AnyhowResult;
use rust_i18n::t;

use super::items::SettingControl;
use super::{FocusPanel, SettingsHit, SettingsLayout};

/// Computed layout for entry dialog hit testing
struct EntryDialogLayout {
    dialog_x: u16,
    dialog_y: u16,
    dialog_width: u16,
    dialog_height: u16,
    inner_x: u16,
    inner_y: u16,
    inner_width: u16,
    inner_height: u16,
    button_y: u16,
    scrollbar_x: u16,
}

impl EntryDialogLayout {
    /// Compute entry dialog layout from modal area
    fn from_modal(modal: ratatui::layout::Rect) -> Option<Self> {
        if modal.width == 0 || modal.height == 0 {
            return None;
        }

        let dialog_width = (modal.width * 85 / 100).clamp(50, 90);
        let dialog_height = (modal.height * 90 / 100).max(15);
        let dialog_x = modal.x + (modal.width.saturating_sub(dialog_width)) / 2;
        let dialog_y = modal.y + (modal.height.saturating_sub(dialog_height)) / 2;

        Some(Self {
            dialog_x,
            dialog_y,
            dialog_width,
            dialog_height,
            inner_x: dialog_x + 2,
            inner_y: dialog_y + 1,
            inner_width: dialog_width.saturating_sub(4),
            inner_height: dialog_height.saturating_sub(5),
            button_y: dialog_y + dialog_height - 2,
            scrollbar_x: dialog_x + dialog_width - 3,
        })
    }

    fn contains(&self, col: u16, row: u16) -> bool {
        col >= self.dialog_x
            && col < self.dialog_x + self.dialog_width
            && row >= self.dialog_y
            && row < self.dialog_y + self.dialog_height
    }

    fn in_content_area(&self, col: u16, row: u16) -> bool {
        col >= self.inner_x
            && col < self.inner_x + self.inner_width
            && row >= self.inner_y
            && row < self.inner_y + self.inner_height
    }

    fn near_scrollbar(&self, col: u16) -> bool {
        col >= self.scrollbar_x.saturating_sub(2) && col <= self.dialog_x + self.dialog_width
    }
}

impl Editor {
    /// Handle mouse events when settings modal is open.
    pub(crate) fn handle_settings_mouse(
        &mut self,
        mouse_event: crossterm::event::MouseEvent,
        is_double_click: bool,
    ) -> AnyhowResult<bool> {
        use crossterm::event::{MouseButton, MouseEventKind};

        let col = mouse_event.column;
        let row = mouse_event.row;

        // When help overlay is open, consume all mouse events
        if let Some(ref state) = self.settings_state {
            if state.showing_help {
                return Ok(false);
            }
        }

        // Handle confirm dialog mouse events
        let showing_confirm = self
            .settings_state
            .as_ref()
            .map(|s| s.showing_confirm_dialog)
            .unwrap_or(false);
        if showing_confirm {
            match mouse_event.kind {
                MouseEventKind::Moved => {
                    let hover = self.get_confirm_dialog_button_at(col, row);
                    if let Some(ref mut state) = self.settings_state {
                        state.confirm_dialog_hover = hover;
                    }
                    return Ok(hover.is_some());
                }
                MouseEventKind::Down(MouseButton::Left) => {
                    return self.handle_confirm_dialog_click(col, row);
                }
                _ => {}
            }
            return Ok(false);
        }

        // Handle mouse events for entry dialog
        if let Some(ref mut state) = self.settings_state {
            if state.showing_entry_dialog() {
                match mouse_event.kind {
                    MouseEventKind::Moved => {
                        return Ok(self.entry_dialog_update_hover(col, row));
                    }
                    MouseEventKind::ScrollUp => {
                        if let Some(dialog) = state.entry_dialog_mut() {
                            dialog.scroll_up();
                            return Ok(true);
                        }
                    }
                    MouseEventKind::ScrollDown => {
                        if let Some(dialog) = state.entry_dialog_mut() {
                            dialog.scroll_down(20);
                            return Ok(true);
                        }
                    }
                    MouseEventKind::Drag(MouseButton::Left) => {
                        return Ok(self.entry_dialog_scrollbar_drag(col, row));
                    }
                    MouseEventKind::Down(MouseButton::Left) => {
                        return self.handle_entry_dialog_click(col, row, is_double_click);
                    }
                    _ => {}
                }
                return Ok(false);
            }
        }

        // Track hover position and compute hover hit for visual feedback
        match mouse_event.kind {
            MouseEventKind::Moved => {
                let hover_hit = self
                    .cached_layout
                    .settings_layout
                    .as_ref()
                    .and_then(|layout: &SettingsLayout| layout.hit_test(col, row));

                if let Some(ref mut state) = self.settings_state {
                    let old_hit = state.hover_hit;
                    state.hover_position = Some((col, row));
                    state.hover_hit = hover_hit;

                    // Update dropdown hover index when hovering over options
                    let new_hover_idx = match hover_hit {
                        Some(SettingsHit::ControlDropdownOption(_, opt_idx)) => Some(opt_idx),
                        _ => None,
                    };
                    let hover_changed = state.set_dropdown_hover(new_hover_idx);

                    return Ok(old_hit != hover_hit || hover_changed);
                }
                return Ok(false);
            }
            MouseEventKind::ScrollUp => {
                // If a dropdown is open, forward scroll to it
                if let Some(ref mut state) = self.settings_state {
                    if state.is_dropdown_open() {
                        state.dropdown_scroll(-3);
                        return Ok(true);
                    }
                    // If search is active and we have results, scroll search results
                    if state.search_active && !state.search_results.is_empty() {
                        return Ok(state.search_scroll_up(3));
                    }
                }
                return Ok(self.settings_scroll_up(3));
            }
            MouseEventKind::ScrollDown => {
                // If a dropdown is open, forward scroll to it
                if let Some(ref mut state) = self.settings_state {
                    if state.is_dropdown_open() {
                        state.dropdown_scroll(3);
                        return Ok(true);
                    }
                    // If search is active and we have results, scroll search results
                    if state.search_active && !state.search_results.is_empty() {
                        return Ok(state.search_scroll_down(3));
                    }
                }
                return Ok(self.settings_scroll_down(3));
            }
            MouseEventKind::Drag(MouseButton::Left) => {
                // Check if dragging on search scrollbar
                if let Some(ref mut state) = self.settings_state {
                    if state.search_active && !state.search_results.is_empty() {
                        if let Some(scrolled) = self.search_scrollbar_drag(col, row) {
                            return Ok(scrolled);
                        }
                    }
                }
                return Ok(self.settings_scrollbar_drag(col, row));
            }
            MouseEventKind::Down(MouseButton::Left) => {}
            _ => return Ok(false),
        }

        // Use cached settings layout for hit testing
        let Some(hit) = self
            .cached_layout
            .settings_layout
            .as_ref()
            .and_then(|layout: &SettingsLayout| layout.hit_test(col, row))
        else {
            return Ok(false);
        };

        // Check if a dropdown is open and click is outside of it
        if let Some(ref mut state) = self.settings_state {
            if state.is_dropdown_open() {
                let is_click_on_open_dropdown = matches!(
                    hit,
                    SettingsHit::ControlDropdown(idx) | SettingsHit::ControlDropdownOption(idx, _)
                        if idx == state.selected_item
                );
                if !is_click_on_open_dropdown {
                    state.dropdown_cancel();
                    return Ok(true);
                }
            }
        }

        match hit {
            SettingsHit::Outside | SettingsHit::Background | SettingsHit::SettingsPanel => {}
            SettingsHit::Category(idx) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus.set(FocusPanel::Categories);
                    state.selected_category = idx;
                    state.selected_item = 0;
                    state.scroll_panel = crate::view::ui::ScrollablePanel::new();
                    state.sub_focus = None;
                }
            }
            SettingsHit::SearchResult(idx) => {
                // Click on search result - select it and jump to it (same as Enter)
                if let Some(ref mut state) = self.settings_state {
                    state.selected_search_result = idx;
                    state.jump_to_search_result();
                }
            }
            SettingsHit::Item(idx) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus.set(FocusPanel::Settings);
                    state.selected_item = idx;
                }
            }
            SettingsHit::ControlToggle(idx) | SettingsHit::ControlDropdown(idx) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus.set(FocusPanel::Settings);
                    state.selected_item = idx;
                }
                self.settings_activate_current();
            }
            SettingsHit::ControlDropdownOption(idx, option_idx) => {
                // Click on a dropdown option - select it and close dropdown
                if let Some(ref mut state) = self.settings_state {
                    state.focus.set(FocusPanel::Settings);
                    state.selected_item = idx;
                    state.dropdown_select(option_idx);
                }
            }
            SettingsHit::ControlDecrement(idx) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus.set(FocusPanel::Settings);
                    state.selected_item = idx;
                }
                self.settings_decrement_current();
            }
            SettingsHit::ControlIncrement(idx) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus.set(FocusPanel::Settings);
                    state.selected_item = idx;
                }
                self.settings_increment_current();
            }
            SettingsHit::ControlText(idx) | SettingsHit::ControlTextListRow(idx, _) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus.set(FocusPanel::Settings);
                    state.selected_item = idx;
                    state.start_editing();
                }
            }
            SettingsHit::ControlMapRow(idx, row_idx) => {
                let is_add_new_row = if let Some(ref mut state) = self.settings_state {
                    state.focus.set(FocusPanel::Settings);
                    state.selected_item = idx;

                    let mut is_add_new = false;
                    if let Some(page) = state.pages.get_mut(state.selected_category) {
                        if let Some(item) = page.items.get_mut(idx) {
                            if let SettingControl::Map(map_state) = &mut item.control {
                                is_add_new = row_idx >= map_state.entries.len();
                                map_state.focused_entry = if row_idx < map_state.entries.len() {
                                    Some(row_idx)
                                } else {
                                    None
                                };
                            }
                        }
                    }
                    is_add_new
                } else {
                    false
                };
                // "Add new" row activates on single click (#604), entries require double-click
                if is_add_new_row || is_double_click {
                    self.settings_activate_current();
                }
            }
            SettingsHit::ControlMapAddNew(idx) => {
                // Click on map add-new row - focus it and activate immediately
                if let Some(ref mut state) = self.settings_state {
                    state.focus.set(FocusPanel::Settings);
                    state.selected_item = idx;

                    if let Some(page) = state.pages.get_mut(state.selected_category) {
                        if let Some(item) = page.items.get_mut(idx) {
                            if let SettingControl::Map(map_state) = &mut item.control {
                                map_state.focused_entry = None; // Focus add-new row
                            }
                        }
                    }
                }
                // Single click on add-new activates immediately
                self.settings_activate_current();
            }
            SettingsHit::LayerButton => {
                if let Some(ref mut state) = self.settings_state {
                    state.cycle_target_layer();
                }
            }
            SettingsHit::SaveButton => self.close_settings(true),
            SettingsHit::CancelButton => {
                if let Some(ref mut state) = self.settings_state {
                    if state.has_changes() {
                        state.showing_confirm_dialog = true;
                        state.confirm_dialog_selection = 0;
                    } else {
                        state.visible = false;
                    }
                }
            }
            SettingsHit::ResetButton => {
                if let Some(ref mut state) = self.settings_state {
                    state.reset_current_to_default();
                }
            }
            SettingsHit::EditButton => {
                // Open config file for the selected layer
                if let Some(ref state) = self.settings_state {
                    let layer = state.target_layer;
                    // Best-effort: open may fail if file doesn't exist yet
                    #[allow(clippy::let_underscore_must_use)]
                    let _ = self.open_config_file(layer);
                }
            }
            SettingsHit::Scrollbar => self.settings_scrollbar_click(row),
            SettingsHit::SearchScrollbar => self.search_scrollbar_click(row),
            SettingsHit::SearchResultsPanel => {
                // Clicking on search results panel background - no action needed
            }
        }

        Ok(true)
    }

    fn settings_scroll_up(&mut self, delta: usize) -> bool {
        self.settings_state
            .as_mut()
            .map(|state| state.scroll_up(delta))
            .unwrap_or(false)
    }

    fn settings_scroll_down(&mut self, delta: usize) -> bool {
        self.settings_state
            .as_mut()
            .map(|state| state.scroll_down(delta))
            .unwrap_or(false)
    }

    fn settings_scrollbar_click(&mut self, row: u16) {
        if let Some(ref scrollbar_area) = self
            .cached_layout
            .settings_layout
            .as_ref()
            .and_then(|l| l.scrollbar_area)
        {
            if scrollbar_area.height > 0 {
                let relative_y = row.saturating_sub(scrollbar_area.y);
                let ratio = relative_y as f32 / scrollbar_area.height as f32;
                if let Some(ref mut state) = self.settings_state {
                    state.scroll_to_ratio(ratio);
                }
            }
        }
    }

    fn settings_scrollbar_drag(&mut self, col: u16, row: u16) -> bool {
        if let Some(ref scrollbar_area) = self
            .cached_layout
            .settings_layout
            .as_ref()
            .and_then(|l| l.scrollbar_area)
        {
            let in_scrollbar_x = col >= scrollbar_area.x.saturating_sub(1)
                && col <= scrollbar_area.x + scrollbar_area.width;
            if in_scrollbar_x && scrollbar_area.height > 0 {
                let relative_y = row.saturating_sub(scrollbar_area.y);
                let ratio = relative_y as f32 / scrollbar_area.height as f32;
                if let Some(ref mut state) = self.settings_state {
                    return state.scroll_to_ratio(ratio);
                }
            }
        }
        false
    }

    fn search_scrollbar_click(&mut self, row: u16) {
        if let Some(ref scrollbar_area) = self
            .cached_layout
            .settings_layout
            .as_ref()
            .and_then(|l| l.search_scrollbar_area)
        {
            if scrollbar_area.height > 0 {
                let relative_y = row.saturating_sub(scrollbar_area.y);
                let ratio = relative_y as f32 / scrollbar_area.height as f32;
                if let Some(ref mut state) = self.settings_state {
                    state.search_scroll_to_ratio(ratio);
                }
            }
        }
    }

    fn search_scrollbar_drag(&mut self, col: u16, row: u16) -> Option<bool> {
        if let Some(ref scrollbar_area) = self
            .cached_layout
            .settings_layout
            .as_ref()
            .and_then(|l| l.search_scrollbar_area)
        {
            let in_scrollbar_x = col >= scrollbar_area.x.saturating_sub(1)
                && col <= scrollbar_area.x + scrollbar_area.width;
            if in_scrollbar_x && scrollbar_area.height > 0 {
                let relative_y = row.saturating_sub(scrollbar_area.y);
                let ratio = relative_y as f32 / scrollbar_area.height as f32;
                if let Some(ref mut state) = self.settings_state {
                    return Some(state.search_scroll_to_ratio(ratio));
                }
            }
        }
        None // Not on search scrollbar
    }

    fn entry_dialog_layout(&self) -> Option<EntryDialogLayout> {
        self.cached_layout
            .settings_layout
            .as_ref()
            .and_then(|l| EntryDialogLayout::from_modal(l.modal_area))
    }

    fn entry_dialog_scrollbar_drag(&mut self, col: u16, row: u16) -> bool {
        let Some(layout) = self.entry_dialog_layout() else {
            return false;
        };

        if layout.near_scrollbar(col) && layout.inner_height > 0 {
            let relative_y = row.saturating_sub(layout.inner_y);
            let ratio = (relative_y as f32 / layout.inner_height as f32).clamp(0.0, 1.0);

            if let Some(ref mut state) = self.settings_state {
                if let Some(dialog) = state.entry_dialog_mut() {
                    dialog.scroll_to_ratio(ratio);
                    return true;
                }
            }
        }
        false
    }

    fn entry_dialog_update_hover(&mut self, col: u16, row: u16) -> bool {
        let Some(layout) = self.entry_dialog_layout() else {
            return false;
        };

        let Some(ref mut state) = self.settings_state else {
            return false;
        };
        let Some(dialog) = state.entry_dialog_mut() else {
            return false;
        };

        let old_item = dialog.hover_item;
        let old_button = dialog.hover_button;

        // Reset hover state
        dialog.hover_item = None;
        dialog.hover_button = None;

        if !layout.contains(col, row) {
            return old_item.is_some() || old_button.is_some();
        }

        // Check button hover
        if row == layout.button_y {
            let buttons: &[&str] = if dialog.is_new || dialog.no_delete {
                &["[ Save ]", "[ Cancel ]"]
            } else {
                &["[ Save ]", "[ Delete ]", "[ Cancel ]"]
            };
            let total_width: u16 = buttons.iter().map(|b| b.len() as u16 + 2).sum();
            let mut x = layout.dialog_x + (layout.dialog_width.saturating_sub(total_width)) / 2;

            for (idx, label) in buttons.iter().enumerate() {
                let width = label.len() as u16;
                if col >= x && col < x + width {
                    dialog.hover_button = Some(idx);
                    break;
                }
                x += width + 2;
            }
        }

        // Check item hover (only for editable items)
        if layout.in_content_area(col, row) {
            let click_y = (row - layout.inner_y) as usize + dialog.scroll_offset;
            let mut content_y: usize = 0;

            // Check if we have a separator between read-only and editable items
            let first_editable = dialog.first_editable_index;
            let has_separator = first_editable > 0 && first_editable < dialog.items.len();

            for (idx, item) in dialog.items.iter().enumerate() {
                // Account for separator before first editable item
                if has_separator && idx == first_editable {
                    content_y += 1; // separator height
                }

                let item_end = content_y + item.control.control_height() as usize;
                if click_y >= content_y && click_y < item_end {
                    // Only hover on editable items
                    if !item.read_only {
                        dialog.hover_item = Some(idx);
                    }
                    break;
                }
                content_y = item_end;
            }
        }

        old_item != dialog.hover_item || old_button != dialog.hover_button
    }

    fn handle_entry_dialog_click(
        &mut self,
        col: u16,
        row: u16,
        _is_double_click: bool,
    ) -> AnyhowResult<bool> {
        let Some(layout) = self.entry_dialog_layout() else {
            return Ok(false);
        };

        if !layout.contains(col, row) {
            return Ok(false);
        }

        // Button click
        if row == layout.button_y {
            return self.handle_entry_dialog_button_click(col, &layout);
        }

        // Item click
        if layout.in_content_area(col, row) {
            return self.handle_entry_dialog_item_click(row, &layout);
        }

        Ok(false)
    }

    fn handle_entry_dialog_button_click(
        &mut self,
        col: u16,
        layout: &EntryDialogLayout,
    ) -> AnyhowResult<bool> {
        let Some(ref mut state) = self.settings_state else {
            return Ok(false);
        };
        let Some(dialog) = state.entry_dialog_mut() else {
            return Ok(false);
        };

        let buttons: &[&str] = if dialog.is_new || dialog.no_delete {
            &["[ Save ]", "[ Cancel ]"]
        } else {
            &["[ Save ]", "[ Delete ]", "[ Cancel ]"]
        };
        let total_width: u16 = buttons.iter().map(|b| b.len() as u16 + 2).sum();
        let mut x = layout.dialog_x + (layout.dialog_width.saturating_sub(total_width)) / 2;

        for (idx, label) in buttons.iter().enumerate() {
            let width = label.len() as u16;
            if col >= x && col < x + width {
                dialog.focus_on_buttons = true;
                dialog.focused_button = idx;
                return self.settings_entry_dialog_activate_button();
            }
            x += width + 2;
        }
        Ok(false)
    }

    fn handle_entry_dialog_item_click(
        &mut self,
        row: u16,
        layout: &EntryDialogLayout,
    ) -> AnyhowResult<bool> {
        let Some(ref mut state) = self.settings_state else {
            return Ok(false);
        };
        let Some(dialog) = state.entry_dialog_mut() else {
            return Ok(false);
        };

        let click_y = (row - layout.inner_y) as usize + dialog.scroll_offset;
        let mut content_y: usize = 0;

        // Check if we have a separator between read-only and editable items
        let first_editable = dialog.first_editable_index;
        let has_separator = first_editable > 0 && first_editable < dialog.items.len();

        for (idx, item) in dialog.items.iter().enumerate() {
            // Account for separator before first editable item
            if has_separator && idx == first_editable {
                content_y += 1; // separator height
            }

            let item_end = content_y + item.control.control_height() as usize;
            if click_y >= content_y && click_y < item_end {
                // Skip clicks on read-only items
                if item.read_only {
                    return Ok(false);
                }
                dialog.focus_on_buttons = false;
                dialog.selected_item = idx;
                dialog.update_focus_states();
                if !dialog.editing_text {
                    dialog.start_editing();
                }
                return Ok(true);
            }
            content_y = item_end;
        }
        Ok(false)
    }

    fn settings_entry_dialog_activate_button(&mut self) -> AnyhowResult<bool> {
        let Some(ref mut state) = self.settings_state else {
            return Ok(false);
        };

        let (btn, is_new) = {
            let Some(dialog) = state.entry_dialog() else {
                return Ok(false);
            };
            (dialog.focused_button, dialog.is_new)
        };

        match (btn, is_new) {
            (0, _) => state.save_entry_dialog(),
            (1, false) => state.delete_entry_dialog(),
            _ => state.close_entry_dialog(),
        }
        Ok(true)
    }

    fn handle_confirm_dialog_click(&mut self, col: u16, row: u16) -> AnyhowResult<bool> {
        if let Some(idx) = self.get_confirm_dialog_button_at(col, row) {
            match idx {
                0 => self.save_settings_and_close(),
                1 => self.discard_settings_and_close(),
                2 => {
                    if let Some(ref mut state) = self.settings_state {
                        state.showing_confirm_dialog = false;
                    }
                }
                _ => {}
            }
            return Ok(true);
        }
        Ok(false)
    }

    /// Returns which confirm dialog button (0-2) is at the given position, if any
    fn get_confirm_dialog_button_at(&self, col: u16, row: u16) -> Option<usize> {
        let modal_area = self
            .cached_layout
            .settings_layout
            .as_ref()
            .map(|l| l.modal_area)?;

        let changes_count = self
            .settings_state
            .as_ref()
            .map(|s| s.get_change_descriptions().len())
            .unwrap_or(0);

        // Calculate dialog dimensions (same as in render_confirm_dialog)
        let dialog_width = 50u16.min(modal_area.width.saturating_sub(4));
        let dialog_height = (7 + changes_count as u16)
            .min(20)
            .min(modal_area.height.saturating_sub(4));
        let dialog_x = modal_area.x + (modal_area.width.saturating_sub(dialog_width)) / 2;
        let dialog_y = modal_area.y + (modal_area.height.saturating_sub(dialog_height)) / 2;

        let inner_x = dialog_x + 2;
        let inner_width = dialog_width.saturating_sub(4);
        let button_y = dialog_y + dialog_height - 3;

        // Check if on button row
        if row != button_y {
            return None;
        }

        // Button labels (must match render_confirm_dialog)
        let options = [
            t!("confirm.save_and_exit").to_string(),
            t!("confirm.discard").to_string(),
            t!("confirm.cancel").to_string(),
        ];
        let total_width: u16 = options.iter().map(|o| o.len() as u16 + 4).sum::<u16>() + 4;
        let mut x = inner_x + (inner_width.saturating_sub(total_width)) / 2;

        for (idx, label) in options.iter().enumerate() {
            let button_width = label.len() as u16 + 4;
            if col >= x && col < x + button_width + 1 {
                return Some(idx);
            }
            x += button_width + 3;
        }

        None
    }

    fn save_settings_and_close(&mut self) {
        self.save_settings();
        if let Some(ref mut state) = self.settings_state {
            state.visible = false;
            state.showing_confirm_dialog = false;
        }
    }

    fn discard_settings_and_close(&mut self) {
        if let Some(ref mut state) = self.settings_state {
            state.visible = false;
            state.showing_confirm_dialog = false;
        }
    }
}
