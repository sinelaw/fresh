//! Toggle actions and configuration operations for the Editor.
//!
//! This module contains toggle methods and configuration operations:
//! - Toggle line numbers, debug highlights, menu bar
//! - Toggle mouse capture, mouse hover, inlay hints
//! - Reset buffer settings
//! - Config dump, save, and reload

use rust_i18n::t;

use crate::config::Config;
use crate::config_io::{ConfigLayer, ConfigResolver};
use crate::input::keybindings::KeybindingResolver;

use super::Editor;

impl Editor {
    /// Toggle line numbers in the gutter for the active split.
    ///
    /// Line number visibility is stored per-split in `BufferViewState` so that
    /// different splits of the same buffer can independently show/hide line numbers
    /// (e.g., source mode shows them, compose mode hides them).
    pub fn toggle_line_numbers(&mut self) {
        let active_split = self.split_manager.active_split();
        if let Some(vs) = self.split_view_states.get_mut(&active_split) {
            let currently_shown = vs.show_line_numbers;
            vs.show_line_numbers = !currently_shown;
            if currently_shown {
                self.set_status_message(t!("toggle.line_numbers_hidden").to_string());
            } else {
                self.set_status_message(t!("toggle.line_numbers_shown").to_string());
            }
        }
    }

    /// Toggle debug highlight mode for the active buffer
    /// When enabled, shows byte positions and highlight span info for debugging
    pub fn toggle_debug_highlights(&mut self) {
        if let Some(state) = self.buffers.get_mut(&self.active_buffer()) {
            state.debug_highlight_mode = !state.debug_highlight_mode;
            if state.debug_highlight_mode {
                self.set_status_message(t!("toggle.debug_mode_on").to_string());
            } else {
                self.set_status_message(t!("toggle.debug_mode_off").to_string());
            }
        }
    }

    /// Toggle menu bar visibility
    pub fn toggle_menu_bar(&mut self) {
        self.menu_bar_visible = !self.menu_bar_visible;
        // When explicitly toggling, clear auto-show state
        self.menu_bar_auto_shown = false;
        // Close any open menu when hiding the menu bar
        if !self.menu_bar_visible {
            self.menu_state.close_menu();
        }
        let status = if self.menu_bar_visible {
            t!("toggle.menu_bar_shown")
        } else {
            t!("toggle.menu_bar_hidden")
        };
        self.set_status_message(status.to_string());
    }

    /// Toggle tab bar visibility
    pub fn toggle_tab_bar(&mut self) {
        self.tab_bar_visible = !self.tab_bar_visible;
        let status = if self.tab_bar_visible {
            t!("toggle.tab_bar_shown")
        } else {
            t!("toggle.tab_bar_hidden")
        };
        self.set_status_message(status.to_string());
    }

    /// Get tab bar visibility
    pub fn tab_bar_visible(&self) -> bool {
        self.tab_bar_visible
    }

    /// Toggle vertical scrollbar visibility
    pub fn toggle_vertical_scrollbar(&mut self) {
        self.config.editor.show_vertical_scrollbar = !self.config.editor.show_vertical_scrollbar;
        let status = if self.config.editor.show_vertical_scrollbar {
            t!("toggle.vertical_scrollbar_shown")
        } else {
            t!("toggle.vertical_scrollbar_hidden")
        };
        self.set_status_message(status.to_string());
    }

    /// Toggle horizontal scrollbar visibility
    pub fn toggle_horizontal_scrollbar(&mut self) {
        self.config.editor.show_horizontal_scrollbar =
            !self.config.editor.show_horizontal_scrollbar;
        let status = if self.config.editor.show_horizontal_scrollbar {
            t!("toggle.horizontal_scrollbar_shown")
        } else {
            t!("toggle.horizontal_scrollbar_hidden")
        };
        self.set_status_message(status.to_string());
    }

    /// Reset buffer settings (tab_size, use_tabs, show_whitespace_tabs) to config defaults
    pub fn reset_buffer_settings(&mut self) {
        let buffer_id = self.active_buffer();

        // Determine settings from config using buffer's stored language
        let (tab_size, use_tabs, show_whitespace_tabs) =
            if let Some(state) = self.buffers.get(&buffer_id) {
                let language = &state.language;
                if let Some(lang_config) = self.config.languages.get(language) {
                    (
                        lang_config.tab_size.unwrap_or(self.config.editor.tab_size),
                        lang_config.use_tabs,
                        lang_config.show_whitespace_tabs,
                    )
                } else {
                    (self.config.editor.tab_size, false, true)
                }
            } else {
                (self.config.editor.tab_size, false, true)
            };

        // Apply settings to buffer
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state.buffer_settings.tab_size = tab_size;
            state.buffer_settings.use_tabs = use_tabs;
            state.buffer_settings.show_whitespace_tabs = show_whitespace_tabs;
        }

        self.set_status_message(t!("toggle.buffer_settings_reset").to_string());
    }

    /// Toggle mouse capture on/off
    pub fn toggle_mouse_capture(&mut self) {
        use std::io::stdout;

        self.mouse_enabled = !self.mouse_enabled;

        if self.mouse_enabled {
            let _ = crossterm::execute!(stdout(), crossterm::event::EnableMouseCapture);
            self.set_status_message(t!("toggle.mouse_capture_enabled").to_string());
        } else {
            let _ = crossterm::execute!(stdout(), crossterm::event::DisableMouseCapture);
            self.set_status_message(t!("toggle.mouse_capture_disabled").to_string());
        }
    }

    /// Check if mouse capture is enabled
    pub fn is_mouse_enabled(&self) -> bool {
        self.mouse_enabled
    }

    /// Toggle mouse hover for LSP on/off
    pub fn toggle_mouse_hover(&mut self) {
        self.config.editor.mouse_hover_enabled = !self.config.editor.mouse_hover_enabled;

        if self.config.editor.mouse_hover_enabled {
            self.set_status_message(t!("toggle.mouse_hover_enabled").to_string());
        } else {
            // Clear any pending hover state
            self.mouse_state.lsp_hover_state = None;
            self.mouse_state.lsp_hover_request_sent = false;
            self.set_status_message(t!("toggle.mouse_hover_disabled").to_string());
        }
    }

    /// Check if mouse hover is enabled
    pub fn is_mouse_hover_enabled(&self) -> bool {
        self.config.editor.mouse_hover_enabled
    }

    /// Set GPM active flag (enables software mouse cursor rendering)
    ///
    /// When GPM is used for mouse input on Linux consoles, we need to draw
    /// our own mouse cursor because GPM can't draw on the alternate screen
    /// buffer used by TUI applications.
    pub fn set_gpm_active(&mut self, active: bool) {
        self.gpm_active = active;
    }

    /// Toggle inlay hints visibility
    pub fn toggle_inlay_hints(&mut self) {
        self.config.editor.enable_inlay_hints = !self.config.editor.enable_inlay_hints;

        if self.config.editor.enable_inlay_hints {
            // Re-request inlay hints for the active buffer
            self.request_inlay_hints_for_active_buffer();
            self.set_status_message(t!("toggle.inlay_hints_enabled").to_string());
        } else {
            // Clear inlay hints from all buffers
            for state in self.buffers.values_mut() {
                state.virtual_texts.clear(&mut state.marker_list);
            }
            self.set_status_message(t!("toggle.inlay_hints_disabled").to_string());
        }
    }

    /// Dump the current configuration to the user's config file
    pub fn dump_config(&mut self) {
        // Create the config directory if it doesn't exist
        if let Err(e) = self.filesystem.create_dir_all(&self.dir_context.config_dir) {
            self.set_status_message(
                t!("error.config_dir_failed", error = e.to_string()).to_string(),
            );
            return;
        }

        let config_path = self.dir_context.config_path();
        let resolver = ConfigResolver::new(self.dir_context.clone(), self.working_dir.clone());

        // Save the config to user layer
        match resolver.save_to_layer(&self.config, ConfigLayer::User) {
            Ok(()) => {
                // Open the saved config file in a new buffer
                match self.open_file(&config_path) {
                    Ok(_buffer_id) => {
                        self.set_status_message(
                            t!("config.saved", path = config_path.display().to_string())
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
                                t!("config.saved_failed_open", error = e.to_string()).to_string(),
                            );
                        }
                    }
                }
            }
            Err(e) => {
                self.set_status_message(
                    t!("error.config_save_failed", error = e.to_string()).to_string(),
                );
            }
        }
    }

    /// Save the current configuration to file (without opening it)
    ///
    /// Returns Ok(()) on success, or an error message on failure
    pub fn save_config(&self) -> Result<(), String> {
        // Create the config directory if it doesn't exist
        self.filesystem
            .create_dir_all(&self.dir_context.config_dir)
            .map_err(|e| format!("Failed to create config directory: {}", e))?;

        let resolver = ConfigResolver::new(self.dir_context.clone(), self.working_dir.clone());
        resolver
            .save_to_layer(&self.config, ConfigLayer::User)
            .map_err(|e| format!("Failed to save config: {}", e))
    }

    /// Reload configuration from the config file
    ///
    /// This reloads the config from disk, applies runtime changes (theme, keybindings),
    /// and emits a config_changed event so plugins can update their state accordingly.
    /// Uses the layered config system to properly merge with defaults.
    pub fn reload_config(&mut self) {
        let old_theme = self.config.theme.clone();
        self.config = Config::load_with_layers(&self.dir_context, &self.working_dir);

        // Refresh cached raw user config for plugins
        self.user_config_raw = Config::read_user_config_raw(&self.working_dir);

        // Apply theme change if needed
        if old_theme != self.config.theme {
            if let Some(theme) = self.theme_registry.get_cloned(&self.config.theme) {
                self.theme = theme;
                tracing::info!("Theme changed to '{}'", self.config.theme.0);
            } else {
                tracing::error!("Theme '{}' not found", self.config.theme.0);
            }
        }

        // Always reload keybindings (complex types don't implement PartialEq)
        self.keybindings = KeybindingResolver::new(&self.config);

        // Update clipboard configuration
        self.clipboard.apply_config(&self.config.clipboard);

        // Update LSP configs
        if let Some(ref mut lsp) = self.lsp {
            for (language, lsp_config) in &self.config.lsp {
                lsp.set_language_config(language.clone(), lsp_config.clone());
            }
        }

        // Emit event so plugins know config changed
        let config_path = Config::find_config_path(&self.working_dir);
        self.emit_event(
            "config_changed",
            serde_json::json!({
                "path": config_path.map(|p| p.to_string_lossy().into_owned()),
            }),
        );
    }

    /// Reload the theme registry from disk.
    ///
    /// Call this after installing new theme packages or saving new themes.
    /// This rescans all theme directories and updates the available themes list.
    pub fn reload_themes(&mut self) {
        use crate::view::theme::ThemeLoader;

        let theme_loader = ThemeLoader::new(self.dir_context.themes_dir());
        self.theme_registry = theme_loader.load_all();

        // Re-apply current theme if it still exists, otherwise it might have been updated
        if let Some(theme) = self.theme_registry.get_cloned(&self.config.theme) {
            self.theme = theme;
        }

        tracing::info!(
            "Theme registry reloaded ({} themes)",
            self.theme_registry.len()
        );

        // Emit event so plugins know themes changed
        self.emit_event("themes_changed", serde_json::json!({}));
    }

    /// Persist a single config change to the user config file.
    ///
    /// Used when toggling settings via menu/command palette so that
    /// the change is saved immediately (matching the settings UI behavior).
    pub(super) fn persist_config_change(&self, json_pointer: &str, value: serde_json::Value) {
        let resolver = ConfigResolver::new(self.dir_context.clone(), self.working_dir.clone());
        let changes = std::collections::HashMap::from([(json_pointer.to_string(), value)]);
        let deletions = std::collections::HashSet::new();
        if let Err(e) = resolver.save_changes_to_layer(&changes, &deletions, ConfigLayer::User) {
            tracing::error!("Failed to persist config change {}: {}", json_pointer, e);
        }
    }
}
