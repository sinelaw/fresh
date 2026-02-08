//! Keybinding editor action handling
//!
//! This module provides the action handlers for the keybinding editor modal.

use super::keybinding_editor::KeybindingEditor;
use super::Editor;
use crate::input::handler::InputResult;
use crate::view::keybinding_editor::{handle_keybinding_editor_input, KeybindingEditorAction};
use crate::view::ui::point_in_rect;
use crossterm::event::{KeyEvent, MouseButton, MouseEvent, MouseEventKind};

impl Editor {
    /// Open the keybinding editor modal
    pub fn open_keybinding_editor(&mut self) {
        let config_path = self.dir_context.config_path().display().to_string();
        self.keybinding_editor = Some(KeybindingEditor::new(
            &self.config,
            &self.keybindings,
            config_path,
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
    fn save_keybinding_editor_changes(&mut self, editor: &KeybindingEditor) {
        if !editor.has_changes {
            return;
        }

        // Collect all custom bindings from the editor
        let new_bindings = editor.get_custom_bindings();

        // Add new bindings to existing custom keybindings
        for binding in new_bindings {
            self.config.keybindings.push(binding);
        }

        // Rebuild the keybinding resolver
        self.keybindings = crate::input::keybindings::KeybindingResolver::new(&self.config);

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
            self.working_dir.clone(),
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

    /// Handle mouse events when keybinding editor is active
    /// Returns Ok(true) if a re-render is needed
    pub fn handle_keybinding_editor_mouse(
        &mut self,
        mouse_event: MouseEvent,
    ) -> anyhow::Result<bool> {
        let mut editor = match self.keybinding_editor.take() {
            Some(e) => e,
            None => return Ok(false),
        };

        let col = mouse_event.column;
        let row = mouse_event.row;
        let layout = &editor.layout;

        // All mouse events inside modal are consumed (masked from reaching underlying editor)
        // Events outside the modal are ignored (but still consumed to prevent leaking)
        if !point_in_rect(layout.modal_area, col, row) {
            self.keybinding_editor = Some(editor);
            return Ok(false);
        }

        match mouse_event.kind {
            MouseEventKind::ScrollUp => {
                // Scroll the table
                if editor.edit_dialog.is_none() && !editor.showing_confirm_dialog {
                    editor.select_prev();
                }
            }
            MouseEventKind::ScrollDown => {
                if editor.edit_dialog.is_none() && !editor.showing_confirm_dialog {
                    editor.select_next();
                }
            }
            MouseEventKind::Down(MouseButton::Left) => {
                // Handle confirm dialog clicks first
                if editor.showing_confirm_dialog {
                    if let Some((save_r, discard_r, cancel_r)) = layout.confirm_buttons {
                        if point_in_rect(save_r, col, row) {
                            self.save_keybinding_editor_changes(&editor);
                            return Ok(true);
                        } else if point_in_rect(discard_r, col, row) {
                            self.set_status_message("Keybinding editor closed".to_string());
                            return Ok(true);
                        } else if point_in_rect(cancel_r, col, row) {
                            editor.showing_confirm_dialog = false;
                        }
                    }
                    self.keybinding_editor = Some(editor);
                    return Ok(true);
                }

                // Handle edit dialog clicks
                if editor.edit_dialog.is_some() {
                    // Button clicks
                    if let Some((save_r, cancel_r)) = layout.dialog_buttons {
                        if point_in_rect(save_r, col, row) {
                            // Save button
                            if let Some(err) = editor.apply_edit_dialog() {
                                self.set_status_message(err);
                            }
                            self.keybinding_editor = Some(editor);
                            return Ok(true);
                        } else if point_in_rect(cancel_r, col, row) {
                            // Cancel button - close dialog
                            editor.edit_dialog = None;
                            self.keybinding_editor = Some(editor);
                            return Ok(true);
                        }
                    }
                    // Field clicks
                    if let Some(r) = layout.dialog_key_field {
                        if point_in_rect(r, col, row) {
                            if let Some(ref mut dialog) = editor.edit_dialog {
                                dialog.focus_area = 0;
                                dialog.mode = crate::app::keybinding_editor::EditMode::RecordingKey;
                            }
                        }
                    }
                    if let Some(r) = layout.dialog_action_field {
                        if point_in_rect(r, col, row) {
                            if let Some(ref mut dialog) = editor.edit_dialog {
                                dialog.focus_area = 1;
                                dialog.mode =
                                    crate::app::keybinding_editor::EditMode::EditingAction;
                            }
                        }
                    }
                    if let Some(r) = layout.dialog_context_field {
                        if point_in_rect(r, col, row) {
                            if let Some(ref mut dialog) = editor.edit_dialog {
                                dialog.focus_area = 2;
                                dialog.mode =
                                    crate::app::keybinding_editor::EditMode::EditingContext;
                            }
                        }
                    }
                    self.keybinding_editor = Some(editor);
                    return Ok(true);
                }

                // Click on search bar to focus it
                if let Some(search_r) = layout.search_bar {
                    if point_in_rect(search_r, col, row) {
                        editor.start_search();
                        self.keybinding_editor = Some(editor);
                        return Ok(true);
                    }
                }

                // Click on table row to select
                let table_area = layout.table_area;
                let first_row_y = layout.table_first_row_y;
                if point_in_rect(table_area, col, row) && row >= first_row_y {
                    let clicked_row = (row - first_row_y) as usize;
                    let new_selected = editor.scroll.offset as usize + clicked_row;
                    if new_selected < editor.filtered_indices.len() {
                        editor.selected = new_selected;
                    }
                }
            }
            _ => {}
        }

        self.keybinding_editor = Some(editor);
        Ok(true)
    }
}
