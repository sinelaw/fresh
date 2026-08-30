//! Keybinding editor action handling
//!
//! This module provides the action handlers for the keybinding editor modal.

use super::keybinding_editor::KeybindingEditor;
use super::Editor;
use crate::input::handler::InputResult;
use crate::view::keybinding_editor::{handle_keybinding_editor_input, KeybindingEditorAction};
use crossterm::event::KeyEvent;

impl Editor {
    /// Open the keybinding editor modal
    pub fn open_keybinding_editor(&mut self) {
        use crate::config::MenuExt;
        let config_path = self.dir_context.config_path().display().to_string();
        let cmd_registry = self.command_registry.read().unwrap();
        let keybindings = self.keybindings.read().unwrap();
        // Enumerate top-level menu ids (File, Edit, …, plus plugin menus) so
        // the action dropdown can offer `menu_open:<name>` variants instead of
        // one un-parseable bare `menu_open` row.
        let menu_names: Vec<String> = self
            .menus
            .menus
            .iter()
            .chain(self.menu_state.plugin_menus.iter())
            .map(|m| m.match_id().to_string())
            .collect();
        self.keybinding_editor = Some(KeybindingEditor::new(
            &self.config,
            &keybindings,
            &self.mode_registry,
            &cmd_registry,
            config_path,
            &menu_names,
        ));
    }

    /// Handle input when keybinding editor is active
    pub fn handle_keybinding_editor_input(&mut self, event: &KeyEvent) -> InputResult {
        let mut editor = match self.keybinding_editor.take() {
            Some(e) => e,
            None => return InputResult::Ignored,
        };

        let action = handle_keybinding_editor_input(&mut editor, event);

        match action {
            KeybindingEditorAction::Consumed => {
                self.keybinding_editor = Some(editor);
                InputResult::Consumed
            }
            KeybindingEditorAction::Close => {
                // Close without saving
                self.set_status_message("Keybinding editor closed".to_string());
                InputResult::Consumed
            }
            KeybindingEditorAction::SaveAndClose => {
                // Save custom bindings to config
                self.save_keybinding_editor_changes(&editor);
                InputResult::Consumed
            }
            KeybindingEditorAction::StatusMessage(msg) => {
                self.set_status_message(msg);
                self.keybinding_editor = Some(editor);
                InputResult::Consumed
            }
        }
    }

    /// Save keybinding editor changes to config
    pub(crate) fn save_keybinding_editor_changes(&mut self, editor: &KeybindingEditor) {
        if !editor.has_changes {
            return;
        }

        // Remove deleted custom bindings from config
        for remove in editor.get_pending_removes() {
            self.config_mut().keybindings.retain(|kb| {
                !(kb.action == remove.action
                    && kb.key == remove.key
                    && kb.modifiers == remove.modifiers
                    // Chords carry an empty key/modifiers pair, so without
                    // this every custom chord for the same action+context
                    // would match and be dropped together.
                    && kb.keys == remove.keys
                    && kb.when == remove.when)
            });
        }

        // Add new custom bindings
        let new_bindings = editor.get_custom_bindings();
        for binding in new_bindings {
            self.config_mut().keybindings.push(binding);
        }

        // Rebuild the keybinding resolver, keeping plugin-contributed
        // bindings alive across the rebuild (#2307).
        self.keybindings
            .write()
            .unwrap()
            .reload_from_config(&self.config);

        // Save to config file via the pending changes mechanism
        let config_value = match serde_json::to_value(&self.config.keybindings) {
            Ok(v) => v,
            Err(e) => {
                self.set_status_message(format!("Failed to serialize keybindings: {}", e));
                return;
            }
        };

        let mut changes = std::collections::HashMap::new();
        changes.insert("/keybindings".to_string(), config_value);

        let resolver = crate::config_io::ConfigResolver::new(
            self.dir_context.clone(),
            self.working_dir().to_path_buf(),
        );

        match resolver.save_changes_to_layer(
            &changes,
            &std::collections::HashSet::new(),
            crate::config_io::ConfigLayer::User,
        ) {
            Ok(()) => {
                self.set_status_message("Keybinding changes saved".to_string());
            }
            Err(e) => {
                self.set_status_message(format!("Failed to save keybindings: {}", e));
            }
        }
    }

    /// Check if keybinding editor is active
    pub fn is_keybinding_editor_active(&self) -> bool {
        self.keybinding_editor.is_some()
    }

    /// Select a display row by index (and toggle it if it's a section header) —
    /// the same effect as a TUI click on that table row. Used by the web
    /// `/kbedit` route so a native row click selects through the real editor.
    #[cfg(feature = "web")]
    pub(crate) fn kbedit_select_display_row(&mut self, idx: usize) {
        if let Some(ed) = self.keybinding_editor.as_mut() {
            if idx < ed.display_rows.len() {
                ed.selected = idx;
                if ed.selected_is_section_header() {
                    ed.toggle_section_at_selected();
                }
            }
        }
    }
}
