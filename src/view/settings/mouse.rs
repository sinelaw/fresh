//! Mouse input handling for the Settings dialog.
//!
//! This module contains all mouse event handling for the settings modal,
//! including clicks, scrolling, and drag operations.

use crate::app::Editor;

use super::{FocusPanel, SettingsHit, SettingsLayout};

impl Editor {
    /// Handle mouse events when settings modal is open.
    pub(crate) fn handle_settings_mouse(
        &mut self,
        mouse_event: crossterm::event::MouseEvent,
    ) -> std::io::Result<bool> {
        use crossterm::event::{MouseButton, MouseEventKind};

        // When confirm dialog or help overlay is open, consume all mouse events
        if let Some(ref state) = self.settings_state {
            if state.showing_confirm_dialog || state.showing_help {
                return Ok(false);
            }
        }

        // Handle mouse events for entry dialog (scroll support)
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
                            // Use a reasonable viewport estimate (will be corrected on render)
                            dialog.scroll_down(20);
                            return Ok(true);
                        }
                    }
                    _ => {}
                }
                // Consume other events without action
                return Ok(false);
            }
        }

        let col = mouse_event.column;
        let row = mouse_event.row;

        // Track hover position and compute hover hit for visual feedback
        match mouse_event.kind {
            MouseEventKind::Moved => {
                // Compute hover hit from cached layout
                let hover_hit = self
                    .cached_layout
                    .settings_layout
                    .as_ref()
                    .and_then(|layout: &SettingsLayout| layout.hit_test(col, row));

                if let Some(ref mut state) = self.settings_state {
                    let old_hit = state.hover_hit;
                    state.hover_position = Some((col, row));
                    state.hover_hit = hover_hit;
                    // Re-render if hover target changed
                    return Ok(old_hit != hover_hit);
                }
                return Ok(false);
            }
            MouseEventKind::ScrollUp => {
                return Ok(self.settings_scroll_up(3));
            }
            MouseEventKind::ScrollDown => {
                return Ok(self.settings_scroll_down(3));
            }
            MouseEventKind::Drag(MouseButton::Left) => {
                return Ok(self.settings_scrollbar_drag(col, row));
            }
            MouseEventKind::Down(MouseButton::Left) => {
                // Handle click below
            }
            _ => return Ok(false),
        }

        // Use cached settings layout for hit testing
        let hit = self
            .cached_layout
            .settings_layout
            .as_ref()
            .and_then(|layout: &SettingsLayout| layout.hit_test(col, row));

        let Some(hit) = hit else {
            return Ok(false);
        };

        // Check if a dropdown is open and click is outside of it
        // If so, cancel the dropdown and consume the click
        if let Some(ref mut state) = self.settings_state {
            if state.is_dropdown_open() {
                let is_click_on_open_dropdown = matches!(
                    hit,
                    SettingsHit::ControlDropdown(idx) if idx == state.selected_item
                );
                if !is_click_on_open_dropdown {
                    // Click outside dropdown - cancel and restore original value
                    state.dropdown_cancel();
                    return Ok(true);
                }
            }
        }

        match hit {
            SettingsHit::Outside => {
                // Click outside modal - do nothing (only Cancel button closes)
            }
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
            SettingsHit::ControlToggle(idx) => {
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
            SettingsHit::ControlDropdown(idx) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus_panel = FocusPanel::Settings;
                    state.selected_item = idx;
                }
                self.settings_activate_current();
            }
            SettingsHit::ControlText(idx) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus_panel = FocusPanel::Settings;
                    state.selected_item = idx;
                    state.start_editing();
                }
            }
            SettingsHit::ControlTextListRow(idx, _row_idx) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus_panel = FocusPanel::Settings;
                    state.selected_item = idx;
                    state.start_editing();
                }
            }
            SettingsHit::ControlMapRow(idx, _row_idx) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus_panel = FocusPanel::Settings;
                    state.selected_item = idx;
                }
            }
            SettingsHit::SaveButton => {
                self.save_settings();
            }
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
            SettingsHit::Background => {
                // Click on background inside modal - do nothing
            }
            SettingsHit::Scrollbar => {
                self.settings_scrollbar_click(row);
            }
            SettingsHit::SettingsPanel => {
                // Click on settings panel area - do nothing (scroll handled above)
            }
        }

        Ok(true)
    }

    /// Scroll settings panel up by delta items.
    fn settings_scroll_up(&mut self, delta: usize) -> bool {
        self.settings_state
            .as_mut()
            .map(|state| state.scroll_up(delta))
            .unwrap_or(false)
    }

    /// Scroll settings panel down by delta items.
    fn settings_scroll_down(&mut self, delta: usize) -> bool {
        self.settings_state
            .as_mut()
            .map(|state| state.scroll_down(delta))
            .unwrap_or(false)
    }

    /// Handle scrollbar click at the given row position.
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

    /// Handle scrollbar drag at the given position.
    fn settings_scrollbar_drag(&mut self, col: u16, row: u16) -> bool {
        if let Some(ref scrollbar_area) = self
            .cached_layout
            .settings_layout
            .as_ref()
            .and_then(|l| l.scrollbar_area)
        {
            // Check if we're in or near the scrollbar area (allow some horizontal tolerance)
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
}
