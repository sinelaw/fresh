//! Input handling for the Settings dialog.
//!
//! Implements the InputHandler trait for SettingsState, routing input
//! through the focus hierarchy: Dialog -> Panel -> Control.

use super::items::SettingControl;
use super::state::{FocusPanel, SettingsState};
use crate::input::handler::{DeferredAction, InputContext, InputHandler, InputResult};
use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

impl InputHandler for SettingsState {
    fn handle_key_event(&mut self, event: &KeyEvent, ctx: &mut InputContext) -> InputResult {
        // Entry dialog takes priority when open
        if self.entry_dialog.is_some() {
            return self.handle_entry_dialog_input(event, ctx);
        }

        // Confirmation dialog takes priority
        if self.showing_confirm_dialog {
            return self.handle_confirm_dialog_input(event, ctx);
        }

        // Help overlay takes priority
        if self.showing_help {
            return self.handle_help_input(event, ctx);
        }

        // Search mode takes priority
        if self.search_active {
            return self.handle_search_input(event, ctx);
        }

        // Route to focused panel
        match self.focus_panel {
            FocusPanel::Categories => self.handle_categories_input(event, ctx),
            FocusPanel::Settings => self.handle_settings_input(event, ctx),
            FocusPanel::Footer => self.handle_footer_input(event, ctx),
        }
    }

    fn is_modal(&self) -> bool {
        true // Settings dialog consumes all unhandled input
    }
}

impl SettingsState {
    /// Handle input when entry dialog is open
    fn handle_entry_dialog_input(
        &mut self,
        event: &KeyEvent,
        _ctx: &mut InputContext,
    ) -> InputResult {
        match event.code {
            KeyCode::Esc => {
                self.close_entry_dialog();
                InputResult::Consumed
            }
            KeyCode::Enter if event.modifiers.contains(KeyModifiers::CONTROL) => {
                self.save_entry_dialog();
                InputResult::Consumed
            }
            KeyCode::Tab => {
                if let Some(ref mut dialog) = self.entry_dialog {
                    dialog.focus_next();
                }
                InputResult::Consumed
            }
            KeyCode::BackTab => {
                if let Some(ref mut dialog) = self.entry_dialog {
                    dialog.focus_prev();
                }
                InputResult::Consumed
            }
            KeyCode::Char(c) => {
                if let Some(ref mut dialog) = self.entry_dialog {
                    dialog.insert_char(c);
                }
                InputResult::Consumed
            }
            KeyCode::Backspace => {
                if let Some(ref mut dialog) = self.entry_dialog {
                    dialog.backspace();
                }
                InputResult::Consumed
            }
            KeyCode::Delete => {
                if let Some(ref mut dialog) = self.entry_dialog {
                    dialog.delete_list_item();
                }
                InputResult::Consumed
            }
            KeyCode::Left => {
                if let Some(ref mut dialog) = self.entry_dialog {
                    dialog.cursor_left();
                }
                InputResult::Consumed
            }
            KeyCode::Right => {
                if let Some(ref mut dialog) = self.entry_dialog {
                    dialog.cursor_right();
                }
                InputResult::Consumed
            }
            KeyCode::Up => {
                if let Some(ref mut dialog) = self.entry_dialog {
                    dialog.dropdown_prev();
                }
                InputResult::Consumed
            }
            KeyCode::Down => {
                if let Some(ref mut dialog) = self.entry_dialog {
                    dialog.dropdown_next();
                }
                InputResult::Consumed
            }
            _ => InputResult::Consumed, // Modal: consume all
        }
    }

    /// Handle input when confirmation dialog is showing
    fn handle_confirm_dialog_input(
        &mut self,
        event: &KeyEvent,
        ctx: &mut InputContext,
    ) -> InputResult {
        match event.code {
            KeyCode::Left => {
                if self.confirm_dialog_selection > 0 {
                    self.confirm_dialog_selection -= 1;
                }
                InputResult::Consumed
            }
            KeyCode::Right => {
                if self.confirm_dialog_selection < 2 {
                    self.confirm_dialog_selection += 1;
                }
                InputResult::Consumed
            }
            KeyCode::Enter => {
                match self.confirm_dialog_selection {
                    0 => ctx.defer(DeferredAction::CloseSettings { save: true }), // Save
                    1 => ctx.defer(DeferredAction::CloseSettings { save: false }), // Discard
                    2 => self.showing_confirm_dialog = false, // Cancel - back to settings
                    _ => {}
                }
                InputResult::Consumed
            }
            KeyCode::Esc => {
                self.showing_confirm_dialog = false;
                InputResult::Consumed
            }
            KeyCode::Char('s') | KeyCode::Char('S') => {
                ctx.defer(DeferredAction::CloseSettings { save: true });
                InputResult::Consumed
            }
            KeyCode::Char('d') | KeyCode::Char('D') => {
                ctx.defer(DeferredAction::CloseSettings { save: false });
                InputResult::Consumed
            }
            _ => InputResult::Consumed, // Modal: consume all
        }
    }

    /// Handle input when help overlay is showing
    fn handle_help_input(&mut self, _event: &KeyEvent, _ctx: &mut InputContext) -> InputResult {
        // Any key dismisses help
        self.showing_help = false;
        InputResult::Consumed
    }

    /// Handle input when search is active
    fn handle_search_input(&mut self, event: &KeyEvent, _ctx: &mut InputContext) -> InputResult {
        match event.code {
            KeyCode::Esc => {
                self.cancel_search();
                InputResult::Consumed
            }
            KeyCode::Enter => {
                self.jump_to_search_result();
                InputResult::Consumed
            }
            KeyCode::Up => {
                self.search_prev();
                InputResult::Consumed
            }
            KeyCode::Down => {
                self.search_next();
                InputResult::Consumed
            }
            KeyCode::Char(c) => {
                self.search_push_char(c);
                InputResult::Consumed
            }
            KeyCode::Backspace => {
                self.search_pop_char();
                InputResult::Consumed
            }
            _ => InputResult::Consumed, // Modal: consume all
        }
    }

    /// Handle input when Categories panel is focused
    fn handle_categories_input(&mut self, event: &KeyEvent, ctx: &mut InputContext) -> InputResult {
        match event.code {
            KeyCode::Up => {
                self.select_prev();
                InputResult::Consumed
            }
            KeyCode::Down => {
                self.select_next();
                InputResult::Consumed
            }
            KeyCode::Tab => {
                self.toggle_focus();
                InputResult::Consumed
            }
            KeyCode::Char('/') => {
                self.start_search();
                InputResult::Consumed
            }
            KeyCode::Char('?') => {
                self.toggle_help();
                InputResult::Consumed
            }
            KeyCode::Esc => {
                self.request_close(ctx);
                InputResult::Consumed
            }
            KeyCode::Enter | KeyCode::Right => {
                // Enter/Right on categories: move focus to settings panel
                self.focus_panel = FocusPanel::Settings;
                InputResult::Consumed
            }
            _ => InputResult::Ignored, // Let modal catch it
        }
    }

    /// Handle input when Settings panel is focused
    fn handle_settings_input(&mut self, event: &KeyEvent, ctx: &mut InputContext) -> InputResult {
        // If editing text, handle text input
        if self.editing_text {
            return self.handle_text_editing_input(event, ctx);
        }

        // If dropdown is open, handle dropdown navigation
        if self.is_dropdown_open() {
            return self.handle_dropdown_input(event, ctx);
        }

        match event.code {
            KeyCode::Up => {
                self.select_prev();
                InputResult::Consumed
            }
            KeyCode::Down => {
                self.select_next();
                InputResult::Consumed
            }
            KeyCode::Tab => {
                self.toggle_focus();
                InputResult::Consumed
            }
            KeyCode::Left => {
                self.handle_control_decrement();
                InputResult::Consumed
            }
            KeyCode::Right => {
                self.handle_control_increment();
                InputResult::Consumed
            }
            KeyCode::Enter | KeyCode::Char(' ') => {
                self.handle_control_activate(ctx);
                InputResult::Consumed
            }
            KeyCode::Char('/') => {
                self.start_search();
                InputResult::Consumed
            }
            KeyCode::Char('?') => {
                self.toggle_help();
                InputResult::Consumed
            }
            KeyCode::Esc => {
                self.request_close(ctx);
                InputResult::Consumed
            }
            _ => InputResult::Ignored, // Let modal catch it
        }
    }

    /// Handle input when Footer is focused
    fn handle_footer_input(&mut self, event: &KeyEvent, ctx: &mut InputContext) -> InputResult {
        match event.code {
            KeyCode::Left => {
                if self.footer_button_index > 0 {
                    self.footer_button_index -= 1;
                }
                InputResult::Consumed
            }
            KeyCode::Right => {
                if self.footer_button_index < 2 {
                    self.footer_button_index += 1;
                }
                InputResult::Consumed
            }
            KeyCode::Tab => {
                self.toggle_focus();
                InputResult::Consumed
            }
            KeyCode::Enter => {
                match self.footer_button_index {
                    0 => self.reset_current_to_default(),
                    1 => ctx.defer(DeferredAction::CloseSettings { save: true }),
                    2 => self.request_close(ctx),
                    _ => {}
                }
                InputResult::Consumed
            }
            KeyCode::Esc => {
                self.request_close(ctx);
                InputResult::Consumed
            }
            KeyCode::Char('/') => {
                self.start_search();
                InputResult::Consumed
            }
            KeyCode::Char('?') => {
                self.toggle_help();
                InputResult::Consumed
            }
            _ => InputResult::Ignored, // Let modal catch it
        }
    }

    /// Handle input when editing text in a control
    fn handle_text_editing_input(
        &mut self,
        event: &KeyEvent,
        _ctx: &mut InputContext,
    ) -> InputResult {
        match event.code {
            KeyCode::Esc => {
                self.stop_editing();
                InputResult::Consumed
            }
            KeyCode::Enter => {
                self.text_add_item();
                InputResult::Consumed
            }
            KeyCode::Char(c) => {
                self.text_insert(c);
                InputResult::Consumed
            }
            KeyCode::Backspace => {
                self.text_backspace();
                InputResult::Consumed
            }
            KeyCode::Delete => {
                self.text_remove_focused();
                InputResult::Consumed
            }
            KeyCode::Left => {
                self.text_move_left();
                InputResult::Consumed
            }
            KeyCode::Right => {
                self.text_move_right();
                InputResult::Consumed
            }
            KeyCode::Up => {
                self.text_focus_prev();
                InputResult::Consumed
            }
            KeyCode::Down => {
                self.text_focus_next();
                InputResult::Consumed
            }
            _ => InputResult::Consumed, // Consume all during text edit
        }
    }

    /// Handle input when dropdown is open
    fn handle_dropdown_input(&mut self, event: &KeyEvent, _ctx: &mut InputContext) -> InputResult {
        match event.code {
            KeyCode::Up => {
                self.dropdown_prev();
                InputResult::Consumed
            }
            KeyCode::Down => {
                self.dropdown_next();
                InputResult::Consumed
            }
            KeyCode::Enter => {
                self.dropdown_confirm();
                InputResult::Consumed
            }
            KeyCode::Esc => {
                self.dropdown_cancel();
                InputResult::Consumed
            }
            _ => InputResult::Consumed, // Consume all while dropdown is open
        }
    }

    /// Request to close settings (shows confirm dialog if there are changes)
    fn request_close(&mut self, ctx: &mut InputContext) {
        if self.has_changes() {
            self.showing_confirm_dialog = true;
            self.confirm_dialog_selection = 0;
        } else {
            ctx.defer(DeferredAction::CloseSettings { save: false });
        }
    }

    /// Handle control activation (Enter/Space on a setting)
    fn handle_control_activate(&mut self, _ctx: &mut InputContext) {
        if let Some(item) = self.current_item_mut() {
            match &mut item.control {
                SettingControl::Toggle(ref mut state) => {
                    state.checked = !state.checked;
                    self.on_value_changed();
                }
                SettingControl::Dropdown(ref mut state) => {
                    state.open = !state.open;
                }
                SettingControl::Number(_) => {
                    self.start_number_editing();
                }
                SettingControl::Text(_) => {
                    self.start_editing();
                }
                SettingControl::TextList(_) => {
                    self.start_editing();
                }
                SettingControl::Map(ref mut state) => {
                    if state.focused_entry.is_none() {
                        // On add-new row: start editing
                        self.start_editing();
                    } else if state.value_schema.is_some() {
                        // Has schema: open entry dialog
                        self.open_entry_dialog();
                    } else {
                        // Toggle expanded
                        if let Some(idx) = state.focused_entry {
                            if state.expanded.contains(&idx) {
                                state.expanded.retain(|&i| i != idx);
                            } else {
                                state.expanded.push(idx);
                            }
                        }
                    }
                    self.on_value_changed();
                }
                SettingControl::KeybindingList(_) | SettingControl::Complex { .. } => {
                    // Not editable via simple controls
                }
            }
        }
    }

    /// Handle control increment (Right arrow on numbers/dropdowns)
    fn handle_control_increment(&mut self) {
        if let Some(item) = self.current_item_mut() {
            match &mut item.control {
                SettingControl::Number(ref mut state) => {
                    state.value += 1;
                    if let Some(max) = state.max {
                        state.value = state.value.min(max);
                    }
                    self.on_value_changed();
                }
                SettingControl::Dropdown(ref mut state) => {
                    if state.selected + 1 < state.options.len() {
                        state.selected += 1;
                        self.on_value_changed();
                    }
                }
                SettingControl::Map(ref mut state) => {
                    // Navigate within map entries
                    let entry_count = state.entries.len();
                    if let Some(idx) = state.focused_entry {
                        if idx + 1 < entry_count {
                            state.focused_entry = Some(idx + 1);
                        }
                    }
                }
                _ => {}
            }
        }
    }

    /// Handle control decrement (Left arrow on numbers/dropdowns)
    fn handle_control_decrement(&mut self) {
        if let Some(item) = self.current_item_mut() {
            match &mut item.control {
                SettingControl::Number(ref mut state) => {
                    if state.value > 0 {
                        state.value -= 1;
                    }
                    if let Some(min) = state.min {
                        state.value = state.value.max(min);
                    }
                    self.on_value_changed();
                }
                SettingControl::Dropdown(ref mut state) => {
                    if state.selected > 0 {
                        state.selected -= 1;
                        self.on_value_changed();
                    }
                }
                SettingControl::Map(ref mut state) => {
                    if let Some(idx) = state.focused_entry {
                        if idx > 0 {
                            state.focused_entry = Some(idx - 1);
                        }
                    }
                }
                _ => {}
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

    fn key(code: KeyCode) -> KeyEvent {
        KeyEvent::new(code, KeyModifiers::NONE)
    }

    #[test]
    fn test_settings_is_modal() {
        // SettingsState should be modal - consume all unhandled input
        let schema = include_str!("../../../plugins/config-schema.json");
        let config = crate::config::Config::default();
        let state = SettingsState::new(schema, &config).unwrap();
        assert!(state.is_modal());
    }

    #[test]
    fn test_categories_panel_does_not_leak_to_settings() {
        let schema = include_str!("../../../plugins/config-schema.json");
        let config = crate::config::Config::default();
        let mut state = SettingsState::new(schema, &config).unwrap();
        state.visible = true;
        state.focus_panel = FocusPanel::Categories;

        let mut ctx = InputContext::new();

        // Enter on categories should NOT affect settings items
        // It should just move focus to settings panel
        let result = state.handle_key_event(&key(KeyCode::Enter), &mut ctx);
        assert_eq!(result, InputResult::Consumed);
        assert_eq!(state.focus_panel, FocusPanel::Settings);

        // Go back to categories
        state.focus_panel = FocusPanel::Categories;

        // Left/Right on categories should be consumed but not affect settings
        let result = state.handle_key_event(&key(KeyCode::Right), &mut ctx);
        assert_eq!(result, InputResult::Consumed);
        // Should have moved to settings panel
        assert_eq!(state.focus_panel, FocusPanel::Settings);
    }

    #[test]
    fn test_tab_cycles_focus_panels() {
        let schema = include_str!("../../../plugins/config-schema.json");
        let config = crate::config::Config::default();
        let mut state = SettingsState::new(schema, &config).unwrap();
        state.visible = true;

        let mut ctx = InputContext::new();

        // Start at Categories
        assert_eq!(state.focus_panel, FocusPanel::Categories);

        // Tab -> Settings
        state.handle_key_event(&key(KeyCode::Tab), &mut ctx);
        assert_eq!(state.focus_panel, FocusPanel::Settings);

        // Tab -> Footer
        state.handle_key_event(&key(KeyCode::Tab), &mut ctx);
        assert_eq!(state.focus_panel, FocusPanel::Footer);

        // Tab -> Categories (wrap around)
        state.handle_key_event(&key(KeyCode::Tab), &mut ctx);
        assert_eq!(state.focus_panel, FocusPanel::Categories);
    }

    #[test]
    fn test_escape_shows_confirm_dialog_with_changes() {
        let schema = include_str!("../../../plugins/config-schema.json");
        let config = crate::config::Config::default();
        let mut state = SettingsState::new(schema, &config).unwrap();
        state.visible = true;

        // Simulate a change
        state
            .pending_changes
            .insert("/test".to_string(), serde_json::json!(true));

        let mut ctx = InputContext::new();

        // Escape should show confirm dialog, not close directly
        state.handle_key_event(&key(KeyCode::Esc), &mut ctx);
        assert!(state.showing_confirm_dialog);
        assert!(ctx.deferred_actions.is_empty()); // No close action yet
    }

    #[test]
    fn test_escape_closes_directly_without_changes() {
        let schema = include_str!("../../../plugins/config-schema.json");
        let config = crate::config::Config::default();
        let mut state = SettingsState::new(schema, &config).unwrap();
        state.visible = true;

        let mut ctx = InputContext::new();

        // Escape without changes should defer close action
        state.handle_key_event(&key(KeyCode::Esc), &mut ctx);
        assert!(!state.showing_confirm_dialog);
        assert_eq!(ctx.deferred_actions.len(), 1);
        assert!(matches!(
            ctx.deferred_actions[0],
            DeferredAction::CloseSettings { save: false }
        ));
    }

    #[test]
    fn test_confirm_dialog_navigation() {
        let schema = include_str!("../../../plugins/config-schema.json");
        let config = crate::config::Config::default();
        let mut state = SettingsState::new(schema, &config).unwrap();
        state.visible = true;
        state.showing_confirm_dialog = true;
        state.confirm_dialog_selection = 0; // Save

        let mut ctx = InputContext::new();

        // Right -> Discard
        state.handle_key_event(&key(KeyCode::Right), &mut ctx);
        assert_eq!(state.confirm_dialog_selection, 1);

        // Right -> Cancel
        state.handle_key_event(&key(KeyCode::Right), &mut ctx);
        assert_eq!(state.confirm_dialog_selection, 2);

        // Right again -> stays at Cancel (no wrap)
        state.handle_key_event(&key(KeyCode::Right), &mut ctx);
        assert_eq!(state.confirm_dialog_selection, 2);

        // Left -> Discard
        state.handle_key_event(&key(KeyCode::Left), &mut ctx);
        assert_eq!(state.confirm_dialog_selection, 1);
    }

    #[test]
    fn test_search_mode_captures_typing() {
        let schema = include_str!("../../../plugins/config-schema.json");
        let config = crate::config::Config::default();
        let mut state = SettingsState::new(schema, &config).unwrap();
        state.visible = true;

        let mut ctx = InputContext::new();

        // Start search
        state.handle_key_event(&key(KeyCode::Char('/')), &mut ctx);
        assert!(state.search_active);

        // Type search query
        state.handle_key_event(&key(KeyCode::Char('t')), &mut ctx);
        state.handle_key_event(&key(KeyCode::Char('a')), &mut ctx);
        state.handle_key_event(&key(KeyCode::Char('b')), &mut ctx);
        assert_eq!(state.search_query, "tab");

        // Escape cancels search
        state.handle_key_event(&key(KeyCode::Esc), &mut ctx);
        assert!(!state.search_active);
        assert!(state.search_query.is_empty());
    }

    #[test]
    fn test_footer_button_activation() {
        let schema = include_str!("../../../plugins/config-schema.json");
        let config = crate::config::Config::default();
        let mut state = SettingsState::new(schema, &config).unwrap();
        state.visible = true;
        state.focus_panel = FocusPanel::Footer;
        state.footer_button_index = 1; // Save button

        let mut ctx = InputContext::new();

        // Enter on Save button should defer save action
        state.handle_key_event(&key(KeyCode::Enter), &mut ctx);
        assert_eq!(ctx.deferred_actions.len(), 1);
        assert!(matches!(
            ctx.deferred_actions[0],
            DeferredAction::CloseSettings { save: true }
        ));
    }
}
