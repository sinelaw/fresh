//! Settings modal UI operations for the Editor.
//!
//! This module contains all methods related to the settings modal:
//! - Opening/closing the settings modal
//! - Saving settings to config
//! - Navigation (up/down)
//! - Activating/toggling settings
//! - Incrementing/decrementing numeric values

use crate::config_io::{ConfigLayer, ConfigResolver};
use crate::input::keybindings::KeybindingResolver;
use anyhow::Result as AnyhowResult;
use rust_i18n::t;

use super::Editor;

impl Editor {
    /// Open the settings modal
    pub fn open_settings(&mut self) {
        // Include schema at compile time
        const SCHEMA_JSON: &str = include_str!("../../plugins/config-schema.json");

        // Create settings state if not exists, or show existing
        if self.settings_state.is_none() {
            match crate::view::settings::SettingsState::new(SCHEMA_JSON, &self.config) {
                Ok(mut state) => {
                    // Load layer sources to show where each setting value comes from
                    let resolver =
                        ConfigResolver::new(self.dir_context.clone(), self.working_dir.clone());
                    if let Ok(sources) = resolver.get_layer_sources() {
                        state.set_layer_sources(sources);
                    }
                    state.show();
                    self.settings_state = Some(state);
                }
                Err(e) => {
                    self.set_status_message(
                        t!("settings.failed_to_open", error = e.to_string()).to_string(),
                    );
                }
            }
        } else if let Some(ref mut state) = self.settings_state {
            state.show();
        }
    }

    /// Close the settings modal
    ///
    /// If `save` is true and there are changes, they will be applied first.
    pub fn close_settings(&mut self, save: bool) {
        if save {
            self.save_settings();
        }
        if let Some(ref mut state) = self.settings_state {
            if !save && state.has_changes() {
                // Discard changes
                state.discard_changes();
            }
            state.hide();
        }
    }

    /// Save the settings from the modal to config
    pub fn save_settings(&mut self) {
        let old_theme = self.config.theme.clone();
        let old_locale = self.config.locale.clone();
        let old_plugins = self.config.plugins.clone();

        // Get target layer and new config
        let (target_layer, new_config) = {
            if let Some(ref state) = self.settings_state {
                if !state.has_changes() {
                    return;
                }
                match state.apply_changes(&self.config) {
                    Ok(config) => (state.target_layer, config),
                    Err(e) => {
                        self.set_status_message(
                            t!("settings.failed_to_apply", error = e.to_string()).to_string(),
                        );
                        return;
                    }
                }
            } else {
                return;
            }
        };

        // Apply the new config
        self.config = new_config.clone();

        // Apply runtime changes
        if old_theme != self.config.theme {
            if let Some(theme) = crate::view::theme::Theme::from_name(&self.config.theme) {
                self.theme = theme;
                tracing::info!("Theme changed to '{}'", self.config.theme.0);
            } else {
                tracing::error!("Theme '{}' not found", self.config.theme.0);
                self.set_status_message(format!("Theme '{}' not found", self.config.theme.0));
            }
        }

        // Apply locale change at runtime
        if old_locale != self.config.locale {
            if let Some(locale) = self.config.locale.as_option() {
                crate::i18n::set_locale(locale);
                // Regenerate menus with the new locale
                self.menus = crate::config::MenuConfig::translated();
                tracing::info!("Locale changed to '{}'", locale);
            } else {
                // Auto-detect from environment
                crate::i18n::init();
                self.menus = crate::config::MenuConfig::translated();
                tracing::info!("Locale reset to auto-detect");
            }
            // Refresh command palette commands with new locale
            if let Ok(mut registry) = self.command_registry.write() {
                registry.refresh_builtin_commands();
            }
        }

        // Handle plugin enable/disable changes
        self.apply_plugin_config_changes(&old_plugins);

        // Update keybindings
        self.keybindings = KeybindingResolver::new(&self.config);

        // Save to disk using the appropriate layer
        let resolver = ConfigResolver::new(self.dir_context.clone(), self.working_dir.clone());

        let layer_name = match target_layer {
            ConfigLayer::User => "User",
            ConfigLayer::Project => "Project",
            ConfigLayer::Session => "Session",
            ConfigLayer::System => "System", // Should never happen
        };

        match resolver.save_to_layer(&new_config, target_layer) {
            Ok(()) => {
                self.set_status_message(
                    t!("settings.saved_to_layer", layer = layer_name).to_string(),
                );
                // Clear settings state entirely so next open creates fresh state
                // from the updated config. This fixes issue #474 where reopening
                // settings after save would show stale values.
                self.settings_state = None;
            }
            Err(e) => {
                self.set_status_message(
                    t!("settings.failed_to_save", error = e.to_string()).to_string(),
                );
            }
        }
    }

    /// Open the config file for the specified layer in the editor.
    /// Creates the file with default template if it doesn't exist.
    /// If there are pending changes in the Settings UI, warns the user and doesn't proceed.
    pub fn open_config_file(&mut self, layer: ConfigLayer) -> AnyhowResult<()> {
        // Check for pending changes before opening config file
        if let Some(ref state) = self.settings_state {
            if state.has_changes() {
                self.set_status_message(t!("settings.pending_changes").to_string());
                return Ok(());
            }
        }

        let resolver = ConfigResolver::new(self.dir_context.clone(), self.working_dir.clone());

        let path = match layer {
            ConfigLayer::User => resolver.user_config_path(),
            ConfigLayer::Project => resolver.project_config_write_path(),
            ConfigLayer::Session => resolver.session_config_path(),
            ConfigLayer::System => {
                self.set_status_message(t!("settings.cannot_edit_system").to_string());
                return Ok(());
            }
        };

        // Create parent directory if needed
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }

        // Create file with template if it doesn't exist
        if !path.exists() {
            let template = match layer {
                ConfigLayer::User => {
                    r#"{
  "version": 1,
  "theme": "default",
  "editor": {
    "tab_size": 4,
    "line_numbers": true
  }
}
"#
                }
                ConfigLayer::Project => {
                    r#"{
  "version": 1,
  "editor": {
    "tab_size": 4
  },
  "languages": {}
}
"#
                }
                ConfigLayer::Session => {
                    r#"{
  "version": 1
}
"#
                }
                ConfigLayer::System => unreachable!(),
            };
            std::fs::write(&path, template)?;
        }

        // Close settings and open the config file
        self.settings_state = None;
        self.open_file(&path)?;

        let layer_name = match layer {
            ConfigLayer::User => "User",
            ConfigLayer::Project => "Project",
            ConfigLayer::Session => "Session",
            ConfigLayer::System => "System",
        };
        self.set_status_message(
            t!(
                "settings.editing_config",
                layer = layer_name,
                path = path.display().to_string()
            )
            .to_string(),
        );

        Ok(())
    }

    /// Navigate settings up
    pub fn settings_navigate_up(&mut self) {
        if let Some(ref mut state) = self.settings_state {
            state.select_prev();
        }
    }

    /// Navigate settings down
    pub fn settings_navigate_down(&mut self) {
        if let Some(ref mut state) = self.settings_state {
            state.select_next();
        }
    }

    /// Activate/toggle the currently selected setting
    pub fn settings_activate_current(&mut self) {
        use crate::view::settings::items::SettingControl;
        use crate::view::settings::FocusPanel;

        // Check if we're in the Footer panel - handle button activation
        let focus_panel = self
            .settings_state
            .as_ref()
            .map(|s| s.focus_panel)
            .unwrap_or(FocusPanel::Categories);

        if focus_panel == FocusPanel::Footer {
            let button_index = self
                .settings_state
                .as_ref()
                .map(|s| s.footer_button_index)
                .unwrap_or(2);
            match button_index {
                0 => {
                    // Layer button - cycle target layer
                    if let Some(ref mut state) = self.settings_state {
                        state.cycle_target_layer();
                    }
                }
                1 => {
                    // Reset button
                    if let Some(ref mut state) = self.settings_state {
                        state.reset_current_to_default();
                    }
                }
                2 => {
                    // Save button - save and close
                    self.close_settings(true);
                }
                3 => {
                    // Cancel button
                    self.close_settings(false);
                }
                _ => {}
            }
            return;
        }

        // When Categories panel is focused, Enter does nothing to settings controls
        // (keys should not leak to the right panel)
        if focus_panel == FocusPanel::Categories {
            return;
        }

        // Get the current item's control type to determine action
        let control_type = {
            if let Some(ref state) = self.settings_state {
                state.current_item().map(|item| match &item.control {
                    SettingControl::Toggle(_) => "toggle",
                    SettingControl::Number(_) => "number",
                    SettingControl::Dropdown(_) => "dropdown",
                    SettingControl::Text(_) => "text",
                    SettingControl::TextList(_) => "textlist",
                    SettingControl::Map(_) => "map",
                    SettingControl::ObjectArray(_) => "objectarray",
                    SettingControl::Json(_) => "json",
                    SettingControl::Complex { .. } => "complex",
                })
            } else {
                None
            }
        };

        // Perform the action based on control type
        match control_type {
            Some("toggle") => {
                if let Some(ref mut state) = self.settings_state {
                    if let Some(item) = state.current_item_mut() {
                        if let SettingControl::Toggle(ref mut toggle_state) = item.control {
                            toggle_state.checked = !toggle_state.checked;
                        }
                    }
                    state.on_value_changed();
                }
            }
            Some("dropdown") => {
                // Toggle dropdown open/closed, or confirm selection if open
                if let Some(ref mut state) = self.settings_state {
                    if state.is_dropdown_open() {
                        state.dropdown_confirm();
                    } else {
                        state.dropdown_toggle();
                    }
                }
            }
            Some("textlist") => {
                // Enter text editing mode for TextList controls
                if let Some(ref mut state) = self.settings_state {
                    state.start_editing();
                }
            }
            Some("map") => {
                // For Map controls: check if map has a value schema (supports entry dialogs)
                if let Some(ref mut state) = self.settings_state {
                    if let Some(item) = state.current_item_mut() {
                        if let SettingControl::Map(ref mut map_state) = item.control {
                            if map_state.focused_entry.is_none() {
                                // On add-new row: start editing to add new entry
                                state.start_editing();
                            } else if map_state.value_schema.is_some() {
                                // Map has schema: open entry dialog
                                state.open_entry_dialog();
                            } else {
                                // For other maps: toggle expanded
                                if let Some(idx) = map_state.focused_entry {
                                    if map_state.expanded.contains(&idx) {
                                        map_state.expanded.retain(|&i| i != idx);
                                    } else {
                                        map_state.expanded.push(idx);
                                    }
                                }
                            }
                        }
                    }
                    state.on_value_changed();
                }
            }
            Some("text") => {
                // For Text controls: enter text editing mode
                if let Some(ref mut state) = self.settings_state {
                    state.start_editing();
                }
            }
            Some("number") => {
                // For Number controls: enter number editing mode
                if let Some(ref mut state) = self.settings_state {
                    state.start_number_editing();
                }
            }
            _ => {}
        }
    }

    /// Increment the current setting value (for Number and Dropdown controls)
    pub fn settings_increment_current(&mut self) {
        use crate::view::settings::items::SettingControl;
        use crate::view::settings::FocusPanel;

        // Check if we're in the Footer panel - navigate buttons instead
        let focus_panel = self
            .settings_state
            .as_ref()
            .map(|s| s.focus_panel)
            .unwrap_or(FocusPanel::Categories);

        if focus_panel == FocusPanel::Footer {
            if let Some(ref mut state) = self.settings_state {
                // Navigate to next footer button (wrapping around)
                state.footer_button_index = (state.footer_button_index + 1) % 4;
            }
            return;
        }

        // When Categories panel is focused, Left/Right don't affect settings controls
        if focus_panel == FocusPanel::Categories {
            return;
        }

        let control_type = {
            if let Some(ref state) = self.settings_state {
                state.current_item().map(|item| match &item.control {
                    SettingControl::Number(_) => "number",
                    SettingControl::Dropdown(_) => "dropdown",
                    _ => "other",
                })
            } else {
                None
            }
        };

        match control_type {
            Some("number") => {
                if let Some(ref mut state) = self.settings_state {
                    if let Some(item) = state.current_item_mut() {
                        if let SettingControl::Number(ref mut num_state) = item.control {
                            num_state.increment();
                        }
                    }
                    state.on_value_changed();
                }
            }
            Some("dropdown") => {
                if let Some(ref mut state) = self.settings_state {
                    if let Some(item) = state.current_item_mut() {
                        if let SettingControl::Dropdown(ref mut dropdown_state) = item.control {
                            dropdown_state.select_next();
                        }
                    }
                    state.on_value_changed();
                }
            }
            _ => {}
        }
    }

    /// Decrement the current setting value (for Number and Dropdown controls)
    pub fn settings_decrement_current(&mut self) {
        use crate::view::settings::items::SettingControl;
        use crate::view::settings::FocusPanel;

        // Check if we're in the Footer panel - navigate buttons instead
        let focus_panel = self
            .settings_state
            .as_ref()
            .map(|s| s.focus_panel)
            .unwrap_or(FocusPanel::Categories);

        if focus_panel == FocusPanel::Footer {
            if let Some(ref mut state) = self.settings_state {
                // Navigate to previous footer button (wrapping around)
                state.footer_button_index = if state.footer_button_index == 0 {
                    3
                } else {
                    state.footer_button_index - 1
                };
            }
            return;
        }

        // When Categories panel is focused, Left/Right don't affect settings controls
        if focus_panel == FocusPanel::Categories {
            return;
        }

        let control_type = {
            if let Some(ref state) = self.settings_state {
                state.current_item().map(|item| match &item.control {
                    SettingControl::Number(_) => "number",
                    SettingControl::Dropdown(_) => "dropdown",
                    _ => "other",
                })
            } else {
                None
            }
        };

        match control_type {
            Some("number") => {
                if let Some(ref mut state) = self.settings_state {
                    if let Some(item) = state.current_item_mut() {
                        if let SettingControl::Number(ref mut num_state) = item.control {
                            num_state.decrement();
                        }
                    }
                    state.on_value_changed();
                }
            }
            Some("dropdown") => {
                if let Some(ref mut state) = self.settings_state {
                    if let Some(item) = state.current_item_mut() {
                        if let SettingControl::Dropdown(ref mut dropdown_state) = item.control {
                            dropdown_state.select_prev();
                        }
                    }
                    state.on_value_changed();
                }
            }
            _ => {}
        }
    }

    /// Apply plugin configuration changes by loading/unloading plugins as needed
    fn apply_plugin_config_changes(
        &mut self,
        old_plugins: &std::collections::HashMap<String, crate::config::PluginConfig>,
    ) {
        // Collect changes first to avoid borrow issues
        let changes: Vec<_> = self
            .config
            .plugins
            .iter()
            .filter_map(|(name, new_config)| {
                let was_enabled = old_plugins.get(name).map(|c| c.enabled).unwrap_or(true);
                if new_config.enabled != was_enabled {
                    Some((name.clone(), new_config.enabled, new_config.path.clone()))
                } else {
                    None
                }
            })
            .collect();

        // Apply changes
        for (name, now_enabled, path) in changes {
            if now_enabled {
                // Plugin was disabled, now enabled - load it
                if let Some(ref path) = path {
                    tracing::info!("Loading newly enabled plugin: {}", name);
                    if let Err(e) = self.plugin_manager.load_plugin(path) {
                        tracing::error!("Failed to load plugin '{}': {}", name, e);
                        self.set_status_message(format!("Failed to load plugin '{}': {}", name, e));
                    }
                }
            } else {
                // Plugin was enabled, now disabled - unload it
                tracing::info!("Unloading disabled plugin: {}", name);
                if let Err(e) = self.plugin_manager.unload_plugin(&name) {
                    tracing::error!("Failed to unload plugin '{}': {}", name, e);
                    self.set_status_message(format!("Failed to unload plugin '{}': {}", name, e));
                }
            }
        }
    }
}
