//! Per-setting prompt orchestrators on `Editor`.
//!
//! Each setting (theme, encoding, language, keybinding map, cursor style,
//! locale, ruler, line ending) has a triple of methods on Editor:
//!   start_*_prompt      seed a prompt with suggestions for the setting
//!   apply_*             apply the chosen value to live state
//!   save_*_to_config    persist to config when the user asks to save
//!
//! 14 such triples (~850 lines) lived in input.rs by historical accident;
//! they're prompt orchestration, not key dispatch. Move them here as a
//! sibling impl Editor block. A future phase will collapse the
//! boilerplate into a single SettingsPromptBuilder<T>.

use rust_i18n::t;

use crate::config_io::{ConfigLayer, ConfigResolver};
use crate::view::prompt::PromptType;

use super::Editor;

impl Editor {
    /// Start the line ending selection prompt
    pub(super) fn start_set_line_ending_prompt(&mut self) {
        use crate::model::buffer::LineEnding;

        let current_line_ending = self.active_state().buffer.line_ending();

        let options = [
            (LineEnding::LF, "LF", "Unix/Linux/Mac"),
            (LineEnding::CRLF, "CRLF", "Windows"),
            (LineEnding::CR, "CR", "Classic Mac"),
        ];

        let current_index = options
            .iter()
            .position(|(le, _, _)| *le == current_line_ending)
            .unwrap_or(0);

        let suggestions: Vec<crate::input::commands::Suggestion> = options
            .iter()
            .map(|(le, name, desc)| {
                let is_current = *le == current_line_ending;
                crate::input::commands::Suggestion {
                    text: format!("{} ({})", name, desc),
                    description: if is_current {
                        Some("current".to_string())
                    } else {
                        None
                    },
                    value: Some(name.to_string()),
                    disabled: false,
                    keybinding: None,
                    source: None,
                }
            })
            .collect();

        self.prompt = Some(crate::view::prompt::Prompt::with_suggestions(
            "Line ending: ".to_string(),
            PromptType::SetLineEnding,
            suggestions,
        ));

        if let Some(prompt) = self.prompt.as_mut() {
            if !prompt.suggestions.is_empty() {
                prompt.selected_suggestion = Some(current_index);
                let (_, name, desc) = options[current_index];
                prompt.input = format!("{} ({})", name, desc);
                prompt.cursor_pos = prompt.input.len();
                prompt.selection_anchor = Some(0);
            }
        }
    }

    /// Start the encoding selection prompt
    pub(super) fn start_set_encoding_prompt(&mut self) {
        use crate::model::buffer::Encoding;

        let current_encoding = self.active_state().buffer.encoding();

        let suggestions: Vec<crate::input::commands::Suggestion> = Encoding::all()
            .iter()
            .map(|enc| {
                let is_current = *enc == current_encoding;
                crate::input::commands::Suggestion {
                    text: format!("{} ({})", enc.display_name(), enc.description()),
                    description: if is_current {
                        Some("current".to_string())
                    } else {
                        None
                    },
                    value: Some(enc.display_name().to_string()),
                    disabled: false,
                    keybinding: None,
                    source: None,
                }
            })
            .collect();

        let current_index = Encoding::all()
            .iter()
            .position(|enc| *enc == current_encoding)
            .unwrap_or(0);

        self.prompt = Some(crate::view::prompt::Prompt::with_suggestions(
            "Encoding: ".to_string(),
            PromptType::SetEncoding,
            suggestions,
        ));

        if let Some(prompt) = self.prompt.as_mut() {
            if !prompt.suggestions.is_empty() {
                prompt.selected_suggestion = Some(current_index);
                let enc = Encoding::all()[current_index];
                prompt.input = format!("{} ({})", enc.display_name(), enc.description());
                prompt.cursor_pos = prompt.input.len();
                // Select all text so typing immediately replaces it
                prompt.selection_anchor = Some(0);
            }
        }
    }

    /// Start the reload with encoding prompt
    ///
    /// Prompts user to select an encoding, then reloads the current file with that encoding.
    /// Requires the buffer to have no unsaved modifications.
    pub(super) fn start_reload_with_encoding_prompt(&mut self) {
        use crate::model::buffer::Encoding;

        // Check if buffer has a file path
        let has_file = self
            .buffers
            .get(&self.active_buffer())
            .and_then(|s| s.buffer.file_path())
            .is_some();

        if !has_file {
            self.set_status_message("Cannot reload: buffer has no file".to_string());
            return;
        }

        // Check for unsaved modifications
        let is_modified = self
            .buffers
            .get(&self.active_buffer())
            .map(|s| s.buffer.is_modified())
            .unwrap_or(false);

        if is_modified {
            self.set_status_message(
                "Cannot reload: buffer has unsaved modifications (save first)".to_string(),
            );
            return;
        }

        let current_encoding = self.active_state().buffer.encoding();

        let suggestions: Vec<crate::input::commands::Suggestion> = Encoding::all()
            .iter()
            .map(|enc| {
                let is_current = *enc == current_encoding;
                crate::input::commands::Suggestion {
                    text: format!("{} ({})", enc.display_name(), enc.description()),
                    description: if is_current {
                        Some("current".to_string())
                    } else {
                        None
                    },
                    value: Some(enc.display_name().to_string()),
                    disabled: false,
                    keybinding: None,
                    source: None,
                }
            })
            .collect();

        let current_index = Encoding::all()
            .iter()
            .position(|enc| *enc == current_encoding)
            .unwrap_or(0);

        self.prompt = Some(crate::view::prompt::Prompt::with_suggestions(
            "Reload with encoding: ".to_string(),
            PromptType::ReloadWithEncoding,
            suggestions,
        ));

        if let Some(prompt) = self.prompt.as_mut() {
            if !prompt.suggestions.is_empty() {
                prompt.selected_suggestion = Some(current_index);
                let enc = Encoding::all()[current_index];
                prompt.input = format!("{} ({})", enc.display_name(), enc.description());
                prompt.cursor_pos = prompt.input.len();
                prompt.selection_anchor = Some(0);
            }
        }
    }

    /// Start the language selection prompt
    pub(super) fn start_set_language_prompt(&mut self) {
        use crate::input::commands::CommandSource;

        let current_language = self.active_state().language.clone();

        // Map each catalog entry's display name to a config key (when the user
        // declared a custom key for it) so we can show the extra column.
        let mut config_key_by_display: std::collections::HashMap<String, String> =
            std::collections::HashMap::new();
        for (lang_id, lang_config) in &self.config.languages {
            if let Some(entry) = self.grammar_registry.find_by_name(&lang_config.grammar) {
                config_key_by_display
                    .entry(entry.display_name.clone())
                    .or_insert_with(|| lang_id.clone());
            }
        }

        // Build suggestions from all available syntect syntaxes + Plain Text option
        let mut suggestions: Vec<crate::input::commands::Suggestion> = vec![
            // Plain Text option (no syntax highlighting)
            crate::input::commands::Suggestion {
                text: "Plain Text".to_string(),
                description: if current_language == "text" || current_language == "Plain Text" {
                    Some("current".to_string())
                } else {
                    None
                },
                value: Some("Plain Text".to_string()),
                disabled: false,
                keybinding: Some("text".to_string()),
                source: Some(CommandSource::Builtin),
            },
        ];

        struct LangEntry {
            display_name: String,
            config_key: String,
            source: &'static str,
        }

        // The catalog is the single source of truth: every syntect grammar,
        // every tree-sitter-only language, and every user-config-declared
        // entry lives here after `apply_language_config`.
        let mut entries: Vec<LangEntry> = self
            .grammar_registry
            .catalog()
            .iter()
            .map(|entry| {
                let (config_key, source) = match config_key_by_display.get(&entry.display_name) {
                    Some(key) => (key.clone(), "config"),
                    None => (entry.language_id.clone(), "builtin"),
                };
                LangEntry {
                    display_name: entry.display_name.clone(),
                    config_key,
                    source,
                }
            })
            .collect();

        // Sort alphabetically for easier navigation
        entries.sort_unstable_by(|a, b| {
            a.display_name
                .to_lowercase()
                .cmp(&b.display_name.to_lowercase())
        });

        let mut current_index_found = None;
        for entry in &entries {
            let is_current =
                entry.config_key == current_language || entry.display_name == current_language;
            if is_current {
                current_index_found = Some(suggestions.len());
            }

            let description = if is_current {
                format!("{} (current)", entry.config_key)
            } else {
                entry.config_key.clone()
            };

            let source = if entry.source == "config" {
                Some(CommandSource::Plugin("config".to_string()))
            } else {
                Some(CommandSource::Builtin)
            };

            suggestions.push(crate::input::commands::Suggestion {
                text: entry.display_name.clone(),
                description: Some(description),
                value: Some(entry.display_name.clone()),
                disabled: false,
                keybinding: None,
                source,
            });
        }

        // Find current language index
        let current_index = current_index_found.unwrap_or(0);

        self.prompt = Some(crate::view::prompt::Prompt::with_suggestions(
            "Language: ".to_string(),
            PromptType::SetLanguage,
            suggestions,
        ));

        if let Some(prompt) = self.prompt.as_mut() {
            if !prompt.suggestions.is_empty() {
                prompt.selected_suggestion = Some(current_index);
                // Don't set input - keep it empty so typing filters the list
                // The selected suggestion shows the current language
            }
        }
    }

    /// Start the theme selection prompt with available themes
    pub(super) fn start_select_theme_prompt(&mut self) {
        let available_themes = self.theme_registry.list();
        // The config may hold a portable form (`s-dark.json`, `builtin://dark`,
        // `file://${HOME}/…`) rather than a canonical registry key. Resolve
        // it so the picker can pre-highlight the current theme.
        let resolved_current = self
            .theme_registry
            .resolve_key(&self.config.theme.0)
            .unwrap_or_else(|| self.config.theme.0.clone());
        let current_theme_key = resolved_current.as_str();

        // Find the index of the current theme (match by key first, then name)
        let current_index = available_themes
            .iter()
            .position(|info| info.key == *current_theme_key)
            .or_else(|| {
                let normalized = crate::view::theme::normalize_theme_name(current_theme_key);
                available_themes.iter().position(|info| {
                    crate::view::theme::normalize_theme_name(&info.name) == normalized
                })
            })
            .unwrap_or(0);

        let suggestions: Vec<crate::input::commands::Suggestion> = available_themes
            .iter()
            .map(|info| {
                let is_current = Some(info) == available_themes.get(current_index);
                // Build a short display key for the description column.
                // - file:// URLs: strip prefix to show path relative to user themes dir
                // - https:// URLs: strip scheme
                let display_key: std::borrow::Cow<'_, str> =
                    if let Some(path_str) = info.key.strip_prefix("file://") {
                        let path = std::path::Path::new(path_str);
                        let themes_dir = self.dir_context.themes_dir();
                        path.strip_prefix(&themes_dir)
                            .map(|rel| rel.to_string_lossy())
                            .unwrap_or_else(|_| path.to_string_lossy())
                    } else if let Some(rest) = info.key.strip_prefix("https://") {
                        std::borrow::Cow::Borrowed(rest)
                    } else if let Some(rest) = info.key.strip_prefix("http://") {
                        std::borrow::Cow::Borrowed(rest)
                    } else {
                        std::borrow::Cow::Borrowed(info.key.as_str())
                    };
                let description = if is_current {
                    Some(format!("{} (current)", display_key))
                } else {
                    Some(display_key.to_string())
                };
                crate::input::commands::Suggestion {
                    text: info.name.clone(),
                    description,
                    value: Some(info.key.clone()),
                    disabled: false,
                    keybinding: None,
                    source: None,
                }
            })
            .collect();

        self.prompt = Some(crate::view::prompt::Prompt::with_suggestions(
            "Select theme: ".to_string(),
            PromptType::SelectTheme {
                original_theme: current_theme_key.to_string(),
            },
            suggestions,
        ));

        if let Some(prompt) = self.prompt.as_mut() {
            if !prompt.suggestions.is_empty() {
                prompt.selected_suggestion = Some(current_index);
                // Set input to match selected theme key
                if let Some(suggestion) = prompt.suggestions.get(current_index) {
                    prompt.input = suggestion.get_value().to_string();
                } else {
                    prompt.input = current_theme_key.to_string();
                }
                prompt.cursor_pos = prompt.input.len();
                // Select all so typing replaces the pre-filled value
                prompt.selection_anchor = Some(0);
            }
        }
    }

    /// Apply a theme by key (or name for backward compat) and persist to config
    pub(super) fn apply_theme(&mut self, key_or_name: &str) {
        if !key_or_name.is_empty() {
            if let Some(theme) = self.theme_registry.get_cloned(key_or_name) {
                self.theme = theme;

                // Set terminal cursor color to match theme
                self.theme.set_terminal_cursor_color();

                // Re-apply all overlays so colors match the new theme
                // (diagnostic and semantic token overlays bake RGB at creation time).
                self.reapply_all_overlays();

                // Persist the portable form of the theme (issue #1621) so a
                // shared dotfiles `config.json` resolves the same theme on
                // every machine:
                //   - built-ins    → `builtin://NAME`
                //   - user theme   → relative path from themes dir (e.g.
                //                    `s-dark.json`, `nord/dark.json`)
                //   - outside dir  → absolute `file://…` (user may hand-edit
                //                    to use `${HOME}` / `${XDG_CONFIG_HOME}`)
                //   - URL package  → repo URL key kept as-is
                let resolved = self
                    .theme_registry
                    .resolve_key(key_or_name)
                    .unwrap_or_else(|| key_or_name.to_string());
                let to_persist = self
                    .theme_registry
                    .portable_form(&resolved)
                    .unwrap_or(resolved);
                self.config_mut().theme = to_persist.into();

                // Persist to config file
                self.save_theme_to_config();

                self.set_status_message(
                    t!("view.theme_changed", theme = self.theme.name.clone()).to_string(),
                );
            } else {
                self.set_status_message(format!("Theme '{}' not found", key_or_name));
            }
        }
    }

    /// Re-apply all stored diagnostics and semantic tokens with the current
    /// theme colors. Both overlay types bake RGB values at creation time, so
    /// they must be rebuilt when the theme changes.
    pub(super) fn reapply_all_overlays(&mut self) {
        // --- Diagnostics ---
        crate::services::lsp::diagnostics::invalidate_cache_all();
        let entries: Vec<(String, Vec<lsp_types::Diagnostic>)> = self
            .stored_diagnostics
            .iter()
            .map(|(uri, diags)| (uri.clone(), diags.clone()))
            .collect();
        for (uri, diagnostics) in entries {
            if let Some(buffer_id) = self.find_buffer_by_uri(&uri) {
                if let Some(state) = self.buffers.get_mut(&buffer_id) {
                    crate::services::lsp::diagnostics::apply_diagnostics_to_state_cached(
                        state,
                        &diagnostics,
                        &self.theme,
                    );
                }
            }
        }

        // --- Semantic tokens ---
        let buffer_ids: Vec<_> = self.buffers.keys().cloned().collect();
        for buffer_id in buffer_ids {
            let tokens = self
                .buffers
                .get(&buffer_id)
                .and_then(|s| s.semantic_tokens.as_ref())
                .map(|store| store.tokens.clone());
            if let Some(tokens) = tokens {
                if let Some(state) = self.buffers.get_mut(&buffer_id) {
                    crate::services::lsp::semantic_tokens::apply_semantic_tokens_to_state(
                        state,
                        &tokens,
                        &self.theme,
                    );
                }
            }
        }
    }

    /// Preview a theme by key or name (without persisting to config)
    /// Used for live preview when navigating theme selection
    pub(super) fn preview_theme(&mut self, key_or_name: &str) {
        if !key_or_name.is_empty() {
            if let Some(theme) = self.theme_registry.get_cloned(key_or_name) {
                if theme.name != self.theme.name {
                    self.theme = theme;
                    self.theme.set_terminal_cursor_color();
                    self.reapply_all_overlays();
                }
            }
        }
    }

    /// Save the current theme setting to the user's config file
    pub(super) fn save_theme_to_config(&mut self) {
        // Create the directory if it doesn't exist
        if let Err(e) = self
            .authority
            .filesystem
            .create_dir_all(&self.dir_context.config_dir)
        {
            tracing::warn!("Failed to create config directory: {}", e);
            return;
        }

        // Save the theme using explicit changes to avoid the issue where
        // changing to the default theme doesn't persist (because save_to_layer
        // computes delta vs defaults and sees no difference).
        let resolver = ConfigResolver::new(self.dir_context.clone(), self.working_dir.clone());
        let config_path = resolver.user_config_path();
        tracing::info!(
            "Saving theme '{}' to user config at {}",
            self.config.theme.0,
            config_path.display()
        );

        let mut changes = std::collections::HashMap::new();
        changes.insert(
            "/theme".to_string(),
            serde_json::Value::String(self.config.theme.0.clone()),
        );

        match resolver.save_changes_to_layer(
            &changes,
            &std::collections::HashSet::new(),
            ConfigLayer::User,
        ) {
            Ok(()) => {
                tracing::info!("Theme saved successfully to {}", config_path.display());
            }
            Err(e) => {
                tracing::warn!("Failed to save theme to config: {}", e);
            }
        }
    }

    /// Start the keybinding map selection prompt with available maps
    pub(super) fn start_select_keybinding_map_prompt(&mut self) {
        // Built-in keybinding maps
        let builtin_maps = vec!["default", "emacs", "vscode", "macos"];

        // Collect user-defined keybinding maps from config
        let user_maps: Vec<&str> = self
            .config
            .keybinding_maps
            .keys()
            .map(|s| s.as_str())
            .collect();

        // Combine built-in and user maps
        let mut all_maps: Vec<&str> = builtin_maps;
        for map in &user_maps {
            if !all_maps.contains(map) {
                all_maps.push(map);
            }
        }

        let current_map = &self.config.active_keybinding_map;

        // Find the index of the current keybinding map
        let current_index = all_maps
            .iter()
            .position(|name| *name == current_map)
            .unwrap_or(0);

        let suggestions: Vec<crate::input::commands::Suggestion> = all_maps
            .iter()
            .map(|map_name| {
                let is_current = *map_name == current_map;
                crate::input::commands::Suggestion {
                    text: map_name.to_string(),
                    description: if is_current {
                        Some("(current)".to_string())
                    } else {
                        None
                    },
                    value: Some(map_name.to_string()),
                    disabled: false,
                    keybinding: None,
                    source: None,
                }
            })
            .collect();

        self.prompt = Some(crate::view::prompt::Prompt::with_suggestions(
            "Select keybinding map: ".to_string(),
            PromptType::SelectKeybindingMap,
            suggestions,
        ));

        if let Some(prompt) = self.prompt.as_mut() {
            if !prompt.suggestions.is_empty() {
                prompt.selected_suggestion = Some(current_index);
                prompt.input = current_map.to_string();
                prompt.cursor_pos = prompt.input.len();
                prompt.selection_anchor = Some(0);
            }
        }
    }

    /// Apply a keybinding map by name and persist it to config
    pub(super) fn apply_keybinding_map(&mut self, map_name: &str) {
        if map_name.is_empty() {
            return;
        }

        // Check if the map exists (either built-in or user-defined)
        let is_builtin = matches!(map_name, "default" | "emacs" | "vscode" | "macos");
        let is_user_defined = self.config.keybinding_maps.contains_key(map_name);

        if is_builtin || is_user_defined {
            // Update the active keybinding map in config
            self.config_mut().active_keybinding_map = map_name.to_string().into();

            // Reload the keybinding resolver with the new map
            *self.keybindings.write().unwrap() =
                crate::input::keybindings::KeybindingResolver::new(&self.config);

            // Persist to config file
            self.save_keybinding_map_to_config();

            self.set_status_message(t!("view.keybindings_switched", map = map_name).to_string());
        } else {
            self.set_status_message(t!("view.keybindings_unknown", map = map_name).to_string());
        }
    }

    /// Save the current keybinding map setting to the user's config file
    pub(super) fn save_keybinding_map_to_config(&mut self) {
        // Create the directory if it doesn't exist
        if let Err(e) = self
            .authority
            .filesystem
            .create_dir_all(&self.dir_context.config_dir)
        {
            tracing::warn!("Failed to create config directory: {}", e);
            return;
        }

        // Save the config using the resolver
        let resolver = ConfigResolver::new(self.dir_context.clone(), self.working_dir.clone());
        if let Err(e) = resolver.save_to_layer(&self.config, ConfigLayer::User) {
            tracing::warn!("Failed to save keybinding map to config: {}", e);
        }
    }

    /// Start the cursor style selection prompt
    pub(super) fn start_select_cursor_style_prompt(&mut self) {
        use crate::config::CursorStyle;

        let current_style = self.config.editor.cursor_style;

        // Build suggestions from available cursor styles
        let suggestions: Vec<crate::input::commands::Suggestion> = CursorStyle::OPTIONS
            .iter()
            .zip(CursorStyle::DESCRIPTIONS.iter())
            .map(|(style_name, description)| {
                let is_current = *style_name == current_style.as_str();
                crate::input::commands::Suggestion {
                    text: description.to_string(),
                    description: if is_current {
                        Some("(current)".to_string())
                    } else {
                        None
                    },
                    value: Some(style_name.to_string()),
                    disabled: false,
                    keybinding: None,
                    source: None,
                }
            })
            .collect();

        // Find the index of the current cursor style
        let current_index = CursorStyle::OPTIONS
            .iter()
            .position(|s| *s == current_style.as_str())
            .unwrap_or(0);

        self.prompt = Some(crate::view::prompt::Prompt::with_suggestions(
            "Select cursor style: ".to_string(),
            PromptType::SelectCursorStyle,
            suggestions,
        ));

        if let Some(prompt) = self.prompt.as_mut() {
            if !prompt.suggestions.is_empty() {
                prompt.selected_suggestion = Some(current_index);
                prompt.input = CursorStyle::DESCRIPTIONS[current_index].to_string();
                prompt.cursor_pos = prompt.input.len();
                prompt.selection_anchor = Some(0);
            }
        }
    }

    /// Apply a cursor style and persist it to config
    pub(super) fn apply_cursor_style(&mut self, style_name: &str) {
        use crate::config::CursorStyle;

        if let Some(style) = CursorStyle::parse(style_name) {
            // Update the config in memory
            self.config_mut().editor.cursor_style = style;

            // Apply the cursor style to the terminal
            if self.session_mode {
                // In session mode, queue the escape sequence to be sent to the client
                self.queue_escape_sequences(style.to_escape_sequence());
            } else {
                // In normal mode, write directly to stdout
                use std::io::stdout;
                // Best-effort cursor style change to stdout.
                #[allow(clippy::let_underscore_must_use)]
                let _ = crossterm::execute!(stdout(), style.to_crossterm_style());
            }

            // Persist to config file
            self.save_cursor_style_to_config();

            // Find the description for the status message
            let description = CursorStyle::OPTIONS
                .iter()
                .zip(CursorStyle::DESCRIPTIONS.iter())
                .find(|(name, _)| **name == style_name)
                .map(|(_, desc)| *desc)
                .unwrap_or(style_name);

            self.set_status_message(
                t!("view.cursor_style_changed", style = description).to_string(),
            );
        }
    }

    /// Start the remove ruler prompt with current rulers as suggestions
    pub(super) fn start_remove_ruler_prompt(&mut self) {
        let active_split = self.split_manager.active_split();
        let rulers = self
            .split_view_states
            .get(&active_split)
            .map(|vs| vs.rulers.clone())
            .unwrap_or_default();

        if rulers.is_empty() {
            self.set_status_message(t!("rulers.none_configured").to_string());
            return;
        }

        let suggestions: Vec<crate::input::commands::Suggestion> = rulers
            .iter()
            .map(|&col| crate::input::commands::Suggestion {
                text: format!("Column {}", col),
                description: None,
                value: Some(col.to_string()),
                disabled: false,
                keybinding: None,
                source: None,
            })
            .collect();

        self.prompt = Some(crate::view::prompt::Prompt::with_suggestions(
            t!("rulers.remove_prompt").to_string(),
            PromptType::RemoveRuler,
            suggestions,
        ));
    }

    /// Save the current cursor style setting to the user's config file
    pub(super) fn save_cursor_style_to_config(&mut self) {
        // Create the directory if it doesn't exist
        if let Err(e) = self
            .authority
            .filesystem
            .create_dir_all(&self.dir_context.config_dir)
        {
            tracing::warn!("Failed to create config directory: {}", e);
            return;
        }

        // Save the config using the resolver
        let resolver = ConfigResolver::new(self.dir_context.clone(), self.working_dir.clone());
        if let Err(e) = resolver.save_to_layer(&self.config, ConfigLayer::User) {
            tracing::warn!("Failed to save cursor style to config: {}", e);
        }
    }

    /// Start the locale selection prompt with available locales
    pub(super) fn start_select_locale_prompt(&mut self) {
        let available_locales = crate::i18n::available_locales();
        let current_locale = crate::i18n::current_locale();

        // Find the index of the current locale
        let current_index = available_locales
            .iter()
            .position(|name| *name == current_locale)
            .unwrap_or(0);

        let suggestions: Vec<crate::input::commands::Suggestion> = available_locales
            .iter()
            .map(|locale_name| {
                let is_current = *locale_name == current_locale;
                let description = if let Some((english_name, native_name)) =
                    crate::i18n::locale_display_name(locale_name)
                {
                    if english_name == native_name {
                        // Same name (e.g., English/English)
                        if is_current {
                            format!("{} (current)", english_name)
                        } else {
                            english_name.to_string()
                        }
                    } else {
                        // Different names (e.g., German/Deutsch)
                        if is_current {
                            format!("{} / {} (current)", english_name, native_name)
                        } else {
                            format!("{} / {}", english_name, native_name)
                        }
                    }
                } else {
                    // Unknown locale
                    if is_current {
                        "(current)".to_string()
                    } else {
                        String::new()
                    }
                };
                crate::input::commands::Suggestion {
                    text: locale_name.to_string(),
                    description: if description.is_empty() {
                        None
                    } else {
                        Some(description)
                    },
                    value: Some(locale_name.to_string()),
                    disabled: false,
                    keybinding: None,
                    source: None,
                }
            })
            .collect();

        self.prompt = Some(crate::view::prompt::Prompt::with_suggestions(
            t!("locale.select_prompt").to_string(),
            PromptType::SelectLocale,
            suggestions,
        ));

        if let Some(prompt) = self.prompt.as_mut() {
            if !prompt.suggestions.is_empty() {
                prompt.selected_suggestion = Some(current_index);
                // Start with empty input to show all options initially
                prompt.input = String::new();
                prompt.cursor_pos = 0;
            }
        }
    }

    /// Apply a locale and persist it to config
    pub(super) fn apply_locale(&mut self, locale_name: &str) {
        if !locale_name.is_empty() {
            // Update the locale at runtime
            crate::i18n::set_locale(locale_name);

            // Update the config in memory
            self.config_mut().locale = crate::config::LocaleName(Some(locale_name.to_string()));

            // Regenerate menus with the new locale
            self.set_menus(crate::config::MenuConfig::translated());

            // Refresh command palette commands with new locale
            if let Ok(mut registry) = self.command_registry.write() {
                registry.refresh_builtin_commands();
            }

            // Persist to config file
            self.save_locale_to_config();

            self.set_status_message(t!("locale.changed", locale_name = locale_name).to_string());
        }
    }

    /// Save the current locale setting to the user's config file
    pub(super) fn save_locale_to_config(&mut self) {
        // Create the directory if it doesn't exist
        if let Err(e) = self
            .authority
            .filesystem
            .create_dir_all(&self.dir_context.config_dir)
        {
            tracing::warn!("Failed to create config directory: {}", e);
            return;
        }

        // Save the config using the resolver
        let resolver = ConfigResolver::new(self.dir_context.clone(), self.working_dir.clone());
        if let Err(e) = resolver.save_to_layer(&self.config, ConfigLayer::User) {
            tracing::warn!("Failed to save locale to config: {}", e);
        }
    }
}
