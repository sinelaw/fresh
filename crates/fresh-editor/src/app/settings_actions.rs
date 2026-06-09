//! Settings modal UI operations for the Editor.
//!
//! This module contains all methods related to the settings modal:
//! - Opening/closing the settings modal
//! - Saving settings to config
//! - Navigation (up/down)
//! - Activating/toggling settings
//! - Incrementing/decrementing numeric values

use crate::config::Config;
use crate::config_io::{ConfigLayer, ConfigResolver};
use crate::input::keybindings::KeybindingResolver;
use crate::types::LspServerConfig;
use anyhow::Result as AnyhowResult;
use rust_i18n::t;

use super::Editor;

impl Editor {
    /// Open the settings modal
    pub fn open_settings(&mut self) {
        // Include schema at compile time
        const SCHEMA_JSON: &str = include_str!("../../plugins/config-schema.json");

        // Snapshot of plugin-provided schemas to inject as a
        // "Plugin Settings" category — clone the map so we can drop
        // the read lock before constructing SettingsState. Rebuilt on
        // every open so plugins that lazily register their schemas
        // after startup (or via Reload Plugin) show up without
        // requiring a full editor restart.
        let plugin_schemas = self.plugin_schemas.read().unwrap().clone();

        match crate::view::settings::SettingsState::new_with_plugin_schemas(
            SCHEMA_JSON,
            &self.config,
            &plugin_schemas,
        ) {
            Ok(mut state) => {
                // Load layer sources to show where each setting value comes from
                let resolver =
                    ConfigResolver::new(self.dir_context.clone(), self.working_dir().to_path_buf());
                if let Ok(sources) = resolver.get_layer_sources() {
                    state.set_layer_sources(sources);
                }
                // Snapshot plugin-registered status-bar tokens for the dual-list picker.
                let tokens = self.status_bar_token_registry.lock().unwrap().clone();
                state.set_status_bar_tokens(tokens);
                state.show();
                self.settings_state = Some(state);
            }
            Err(e) => {
                self.set_status_message(
                    t!("settings.failed_to_open", error = e.to_string()).to_string(),
                );
            }
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
        #[cfg(windows)]
        let old_mouse_hover = self.config.editor.mouse_hover_enabled;

        // Get target layer, new config, and the actual changes made
        let (target_layer, new_config, pending_changes, pending_deletions) = {
            if let Some(ref state) = self.settings_state {
                if !state.has_changes() {
                    return;
                }
                match state.apply_changes(&self.config) {
                    Ok(config) => (
                        state.target_layer,
                        config,
                        state.pending_changes.clone(),
                        state.pending_deletions.clone(),
                    ),
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
        self.set_config(new_config.clone());

        // Refresh cached raw user config for plugins
        self.set_user_config_raw(Config::read_user_config_raw(self.working_dir()));

        // Apply runtime changes
        if old_theme != self.config.theme {
            if let Some(theme) = self.theme_registry.get_cloned(&self.config.theme) {
                *self.theme.write().unwrap() = theme;
                self.start_theme_transition_animation();
                tracing::info!("Theme changed to '{}'", self.config.theme.0);
            } else {
                tracing::error!("Theme '{}' not found", self.config.theme.0);
                self.set_status_message(format!("Theme '{}' not found", self.config.theme.0));
            }
        }

        // Apply locale change at runtime
        if old_locale != self.config.locale {
            let locale_owned = self.config.locale.as_option().map(|s| s.to_string());
            if let Some(locale) = locale_owned {
                crate::i18n::set_locale(&locale);
                // Regenerate menus with the new locale
                self.set_menus(crate::config::MenuConfig::translated());
                tracing::info!("Locale changed to '{}'", locale);
            } else {
                // Auto-detect from environment
                crate::i18n::init();
                self.set_menus(crate::config::MenuConfig::translated());
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
        *self.keybindings.write().unwrap() = KeybindingResolver::new(&self.config);

        // Update LSP configs
        let __active_id = self.active_window;
        if let Some(lsp) = self.windows.get_mut(&__active_id).map(|w| &mut w.lsp) {
            for (language, lsp_configs) in &self.config.lsp {
                lsp.set_language_configs(language.clone(), lsp_configs.as_slice().to_vec());
            }
            // Configure universal (global) LSP servers
            let universal_servers: Vec<LspServerConfig> = self
                .config
                .universal_lsp
                .values()
                .flat_map(|lc| lc.as_slice().to_vec())
                .filter(|c| c.enabled)
                .collect();
            lsp.set_universal_configs(universal_servers);
        }

        // Propagate editor config to all split and buffer view states
        for view_state in self
            .windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_view_states_mut())
            .expect("active window must have a populated split layout")
            .values_mut()
        {
            view_state.show_line_numbers = self.config.editor.line_numbers;
            for buf_state in view_state.keyed_states.values_mut() {
                buf_state.rulers = self.config.editor.rulers.clone();
            }
        }

        // Apply bar visibility changes immediately
        self.active_window_mut().menu_bar_visible = self.config.editor.show_menu_bar;
        self.active_window_mut().tab_bar_visible = self.config.editor.show_tab_bar;
        self.active_window_mut().status_bar_visible = self.config.editor.show_status_bar;
        self.active_window_mut().prompt_line_visible = self.config.editor.show_prompt_line;

        // Propagate file-explorer settings to live runtime state (IgnorePatterns
        // and width are shadows of config, not read live on each render).
        self.active_window_mut().file_explorer_width = self.config.file_explorer.width;
        self.active_window_mut().file_explorer_side = self.config.file_explorer.side;
        let active_id = self.active_window;
        if let Some(explorer) = self
            .windows
            .get_mut(&active_id)
            .and_then(|w| w.file_explorer.as_mut())
        {
            let patterns = explorer.ignore_patterns_mut();
            patterns.set_show_hidden(self.config.file_explorer.show_hidden);
            patterns.set_show_gitignored(self.config.file_explorer.show_gitignored);
            explorer.set_compact_directories(self.config.file_explorer.compact_directories);
        }

        // On Windows, switch mouse tracking mode when mouse_hover_enabled changes.
        // Mode 1003 (all motion) is used for hover; mode 1002 (cell motion) otherwise.
        #[cfg(windows)]
        if old_mouse_hover != self.config.editor.mouse_hover_enabled {
            let mode = if self.config.editor.mouse_hover_enabled {
                fresh_winterm::MouseMode::AllMotion
            } else {
                // Clear any pending hover state when disabling
                self.active_window_mut().mouse_state.lsp_hover_state = None;
                self.active_window_mut().mouse_state.lsp_hover_request_sent = false;
                fresh_winterm::MouseMode::CellMotion
            };
            if let Err(e) = fresh_winterm::set_mouse_mode(mode) {
                tracing::error!("Failed to switch mouse mode: {}", e);
            }
        }

        // Propagate tab_size/use_tabs/auto_close/whitespace visibility to all open buffers
        // Each buffer resolves its settings from its language + the new global config
        for (_, state) in self
            .windows
            .get_mut(&self.active_window)
            .map(|w| &mut w.buffers)
            .expect("active window present")
        {
            let mut whitespace =
                crate::config::WhitespaceVisibility::from_editor_config(&self.config.editor);
            state.buffer_settings.auto_close = self.config.editor.auto_close;
            if let Some(lang_config) = self.config.languages.get(&state.language) {
                state.buffer_settings.tab_size =
                    lang_config.tab_size.unwrap_or(self.config.editor.tab_size);
                state.buffer_settings.use_tabs =
                    lang_config.use_tabs.unwrap_or(self.config.editor.use_tabs);
                whitespace =
                    whitespace.with_language_tab_override(lang_config.show_whitespace_tabs);
                // Auto close: language override (only if globally enabled)
                if state.buffer_settings.auto_close {
                    if let Some(lang_auto_close) = lang_config.auto_close {
                        state.buffer_settings.auto_close = lang_auto_close;
                    }
                }
                // Word characters: from language config
                if let Some(ref wc) = lang_config.word_characters {
                    state.buffer_settings.word_characters = wc.clone();
                } else {
                    state.buffer_settings.word_characters.clear();
                }
            } else {
                state.buffer_settings.tab_size = self.config.editor.tab_size;
                state.buffer_settings.use_tabs = self.config.editor.use_tabs;
            }
            state.buffer_settings.whitespace = whitespace;
        }

        // Save ONLY the changes to disk (preserves external edits to the config file)
        let resolver =
            ConfigResolver::new(self.dir_context.clone(), self.working_dir().to_path_buf());

        let layer_name = match target_layer {
            ConfigLayer::User => "User",
            ConfigLayer::Project => "Project",
            ConfigLayer::Session => "Session",
            ConfigLayer::System => "System", // Should never happen
        };

        match resolver.save_changes_to_layer(&pending_changes, &pending_deletions, target_layer) {
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

        let resolver =
            ConfigResolver::new(self.dir_context.clone(), self.working_dir().to_path_buf());

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
            self.authority.filesystem.create_dir_all(parent)?;
        }

        // Create file with template if it doesn't exist
        if !self.authority.filesystem.exists(&path) {
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
            self.authority
                .filesystem
                .write_file(&path, template.as_bytes())?;
        }

        // Close settings and open the config file
        self.settings_state = None;
        match self.open_file(&path) {
            Ok(_) => {
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
            }
            Err(e) => {
                // Check if this is a large file encoding confirmation error
                if let Some(confirmation) =
                    e.downcast_ref::<crate::model::buffer::LargeFileEncodingConfirmation>()
                {
                    self.start_large_file_encoding_confirmation(confirmation);
                } else {
                    self.set_status_message(
                        t!("file.error_opening", error = e.to_string()).to_string(),
                    );
                }
            }
        }

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
            .map(|s| s.focus_panel())
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
                    // Reset/Inherit button — for nullable items, set to null (inherit);
                    // for non-nullable items, reset to default
                    if let Some(ref mut state) = self.settings_state {
                        let is_nullable_set = state
                            .current_item()
                            .map(|item| item.nullable && !item.is_null)
                            .unwrap_or(false);
                        if is_nullable_set {
                            state.set_current_to_null();
                        } else {
                            state.reset_current_to_default();
                        }
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
                    SettingControl::DualList(_) => "duallist",
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
                                // On add-new row: open dialog with empty key
                                if map_state.value_schema.is_some() {
                                    state.open_add_entry_dialog();
                                }
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
            .map(|s| s.focus_panel())
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
            // Number inc/dec removed — direct typing only. Action still
            // exists for Dropdown cycling.
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
            .map(|s| s.focus_panel())
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
            // Number inc/dec removed — direct typing only. Action still
            // exists for Dropdown cycling.
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
                    let load_result = self.plugin_manager.read().unwrap().load_plugin(path);
                    if let Err(e) = load_result {
                        tracing::error!("Failed to load plugin '{}': {}", name, e);
                        self.set_status_message(format!("Failed to load plugin '{}': {}", name, e));
                    }
                }
            } else {
                // Plugin was enabled, now disabled - unload it
                tracing::info!("Unloading disabled plugin: {}", name);
                let unload_result = self.plugin_manager.write().unwrap().unload_plugin(&name);
                if let Err(e) = unload_result {
                    tracing::error!("Failed to unload plugin '{}': {}", name, e);
                    self.set_status_message(format!("Failed to unload plugin '{}': {}", name, e));
                } else {
                    // Clean up status bar tokens for this plugin
                    self.remove_plugin_status_bar_elements(&name);
                }
            }
        }
    }
}
