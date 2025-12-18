//! Mouse input handling for the Settings dialog.
//!
//! This module contains all mouse event handling for the settings modal,
//! including clicks, scrolling, and drag operations.

use crate::app::Editor;

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

        let dialog_width = (modal.width * 85 / 100).min(90).max(50);
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
    ) -> std::io::Result<bool> {
        use crossterm::event::{MouseButton, MouseEventKind};

        let col = mouse_event.column;
        let row = mouse_event.row;

        // When confirm dialog or help overlay is open, consume all mouse events
        if let Some(ref state) = self.settings_state {
            if state.showing_confirm_dialog || state.showing_help {
                return Ok(false);
            }
        }

        // Handle mouse events for entry dialog
        if let Some(ref mut state) = self.settings_state {
            if state.showing_entry_dialog() {
                match mouse_event.kind {
                    MouseEventKind::ScrollUp => {
                        if let Some(ref mut dialog) = state.entry_dialog {
                            dialog.scroll_up();
                            return Ok(true);
                        }
                    }
                    MouseEventKind::ScrollDown => {
                        if let Some(ref mut dialog) = state.entry_dialog {
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
                    return Ok(old_hit != hover_hit);
                }
                return Ok(false);
            }
            MouseEventKind::ScrollUp => return Ok(self.settings_scroll_up(3)),
            MouseEventKind::ScrollDown => return Ok(self.settings_scroll_down(3)),
            MouseEventKind::Drag(MouseButton::Left) => {
                return Ok(self.settings_scrollbar_drag(col, row))
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
                    SettingsHit::ControlDropdown(idx) if idx == state.selected_item
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
                    state.focus_panel = FocusPanel::Categories;
                    state.selected_category = idx;
                    state.selected_item = 0;
                    state.scroll_panel = crate::view::ui::ScrollablePanel::new();
                    state.sub_focus = None;
                }
            }
            SettingsHit::Item(idx) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus_panel = FocusPanel::Settings;
                    state.selected_item = idx;
                }
            }
            SettingsHit::ControlToggle(idx) | SettingsHit::ControlDropdown(idx) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus_panel = FocusPanel::Settings;
                    state.selected_item = idx;
                }
                self.settings_activate_current();
            }
            SettingsHit::ControlDecrement(idx) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus_panel = FocusPanel::Settings;
                    state.selected_item = idx;
                }
                self.settings_decrement_current();
            }
            SettingsHit::ControlIncrement(idx) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus_panel = FocusPanel::Settings;
                    state.selected_item = idx;
                }
                self.settings_increment_current();
            }
            SettingsHit::ControlText(idx) | SettingsHit::ControlTextListRow(idx, _) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus_panel = FocusPanel::Settings;
                    state.selected_item = idx;
                    state.start_editing();
                }
            }
            SettingsHit::ControlMapRow(idx, row_idx) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus_panel = FocusPanel::Settings;
                    state.selected_item = idx;

                    if let Some(page) = state.pages.get_mut(state.selected_category) {
                        if let Some(item) = page.items.get_mut(idx) {
                            if let SettingControl::Map(map_state) = &mut item.control {
                                map_state.focused_entry = if row_idx < map_state.entries.len() {
                                    Some(row_idx)
                                } else {
                                    None
                                };
                            }
                        }
                    }
                }
                if is_double_click {
                    self.settings_activate_current();
                }
            }
            SettingsHit::SaveButton => self.save_settings(),
            SettingsHit::CancelButton => {
                if let Some(ref mut state) = self.settings_state {
                    state.visible = false;
                }
            }
            SettingsHit::ResetButton => {
                if let Some(ref mut state) = self.settings_state {
                    state.reset_current_to_default();
                }
            }
            SettingsHit::Scrollbar => self.settings_scrollbar_click(row),
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
                if let Some(ref mut dialog) = state.entry_dialog {
                    dialog.scroll_to_ratio(ratio);
                    return true;
                }
            }
        }
        false
    }

    fn handle_entry_dialog_click(
        &mut self,
        col: u16,
        row: u16,
        _is_double_click: bool,
    ) -> std::io::Result<bool> {
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
    ) -> std::io::Result<bool> {
        let Some(ref mut state) = self.settings_state else {
            return Ok(false);
        };
        let Some(ref mut dialog) = state.entry_dialog else {
            return Ok(false);
        };

        let buttons: &[&str] = if dialog.is_new {
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
    ) -> std::io::Result<bool> {
        let Some(ref mut state) = self.settings_state else {
            return Ok(false);
        };
        let Some(ref mut dialog) = state.entry_dialog else {
            return Ok(false);
        };

        let click_y = (row - layout.inner_y) as usize + dialog.scroll_offset;
        let mut content_y: usize = 0;

        for (idx, item) in dialog.items.iter().enumerate() {
            let item_end = content_y + item.control.control_height() as usize;
            if click_y >= content_y && click_y < item_end {
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

    fn settings_entry_dialog_activate_button(&mut self) -> std::io::Result<bool> {
        let Some(ref mut state) = self.settings_state else {
            return Ok(false);
        };

        let (btn, is_new) = {
            let Some(ref dialog) = state.entry_dialog else {
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
}
