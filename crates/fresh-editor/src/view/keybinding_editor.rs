//! The keybinding editor's input handling.
//!
//! **Its rendering is gone**: box, chrome, table and dialogs are all
//! `view::shell::keybinding` now, and the ten rectangles this filed for a
//! mouse arm went with them. What is left is the keyboard, which was always
//! the editor's own business.

use crate::app::keybinding_editor::{DeleteResult, EditMode, KeybindingEditor, SearchMode};
use crate::input::keybindings::{format_keybinding, normalize_key};
use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use fresh_i18n::t;

// ==================== INPUT HANDLING ====================

/// Handle input for the keybinding editor. Returns true if the editor should close.
pub fn handle_keybinding_editor_input(
    editor: &mut KeybindingEditor,
    event: &KeyEvent,
) -> KeybindingEditorAction {
    // Help overlay
    if editor.showing_help {
        match event.code {
            KeyCode::Esc | KeyCode::Char('?') | KeyCode::Enter => {
                editor.showing_help = false;
            }
            _ => {}
        }
        return KeybindingEditorAction::Consumed;
    }

    // Confirm dialog
    if editor.showing_confirm_dialog {
        return handle_confirm_input(editor, event);
    }

    // Edit dialog
    if editor.edit_dialog.is_some() {
        return handle_edit_dialog_input(editor, event);
    }

    // Search mode (only when focused/accepting input)
    if editor.search_active && editor.search_focused {
        return handle_search_input(editor, event);
    }

    // Main table navigation
    handle_main_input(editor, event)
}

/// Actions that the keybinding editor can return to the parent
pub enum KeybindingEditorAction {
    /// Input was consumed, no further action needed
    Consumed,
    /// Close the editor (no save)
    Close,
    /// Save and close
    SaveAndClose,
    /// Status message to display
    StatusMessage(String),
}

fn handle_main_input(editor: &mut KeybindingEditor, event: &KeyEvent) -> KeybindingEditorAction {
    match (event.code, event.modifiers) {
        // Close / clear search
        (KeyCode::Esc, KeyModifiers::NONE) => {
            if editor.search_active {
                // Search is visible but unfocused — clear it
                editor.cancel_search();
                KeybindingEditorAction::Consumed
            } else if editor.has_changes {
                editor.showing_confirm_dialog = true;
                editor.confirm_selection = 0;
                KeybindingEditorAction::Consumed
            } else {
                KeybindingEditorAction::Close
            }
        }

        // Save
        (KeyCode::Char('s'), m) if m.contains(KeyModifiers::CONTROL) => {
            KeybindingEditorAction::SaveAndClose
        }

        // Navigation
        (KeyCode::Up, KeyModifiers::NONE) | (KeyCode::Char('k'), KeyModifiers::NONE) => {
            editor.select_prev();
            KeybindingEditorAction::Consumed
        }
        (KeyCode::Down, KeyModifiers::NONE) | (KeyCode::Char('j'), KeyModifiers::NONE) => {
            editor.select_next();
            KeybindingEditorAction::Consumed
        }
        (KeyCode::PageUp, _) => {
            editor.page_up();
            KeybindingEditorAction::Consumed
        }
        (KeyCode::PageDown, _) => {
            editor.page_down();
            KeybindingEditorAction::Consumed
        }
        (KeyCode::Home, _) => {
            editor.selected = 0;
            editor.scroll.offset = 0;
            KeybindingEditorAction::Consumed
        }
        (KeyCode::End, _) => {
            editor.selected = editor.display_rows.len().saturating_sub(1);
            editor.ensure_visible_public();
            KeybindingEditorAction::Consumed
        }

        // Search (re-focuses existing search if visible)
        (KeyCode::Char('/'), KeyModifiers::NONE) => {
            editor.start_search();
            KeybindingEditorAction::Consumed
        }

        // Record key search
        (KeyCode::Char('r'), KeyModifiers::NONE) => {
            editor.start_record_key_search();
            KeybindingEditorAction::Consumed
        }

        // Help
        (KeyCode::Char('?'), _) => {
            editor.showing_help = true;
            KeybindingEditorAction::Consumed
        }

        // Add binding
        (KeyCode::Char('a'), KeyModifiers::NONE) => {
            editor.open_add_dialog();
            KeybindingEditorAction::Consumed
        }

        // Enter: toggle section header or edit binding
        (KeyCode::Enter, KeyModifiers::NONE) => {
            if editor.selected_is_section_header() {
                editor.toggle_section_at_selected();
            } else {
                editor.open_edit_dialog();
            }
            KeybindingEditorAction::Consumed
        }

        // Delete binding
        (KeyCode::Char('d'), KeyModifiers::NONE) | (KeyCode::Delete, _) => {
            match editor.delete_selected() {
                DeleteResult::CustomRemoved => KeybindingEditorAction::StatusMessage(
                    t!("keybinding_editor.status_binding_removed").to_string(),
                ),
                DeleteResult::KeymapOverridden => KeybindingEditorAction::StatusMessage(
                    t!("keybinding_editor.status_keymap_overridden").to_string(),
                ),
                DeleteResult::CannotDelete | DeleteResult::NothingSelected => {
                    KeybindingEditorAction::StatusMessage(
                        t!("keybinding_editor.status_cannot_delete").to_string(),
                    )
                }
            }
        }

        // Context filter
        (KeyCode::Char('c'), KeyModifiers::NONE) => {
            editor.cycle_context_filter();
            KeybindingEditorAction::Consumed
        }

        // Source filter
        (KeyCode::Char('s'), KeyModifiers::NONE) => {
            editor.cycle_source_filter();
            KeybindingEditorAction::Consumed
        }

        _ => KeybindingEditorAction::Consumed,
    }
}

fn handle_search_input(editor: &mut KeybindingEditor, event: &KeyEvent) -> KeybindingEditorAction {
    match editor.search_mode {
        SearchMode::Text => match (event.code, event.modifiers) {
            (KeyCode::Esc, _) => {
                editor.cancel_search();
                KeybindingEditorAction::Consumed
            }
            (KeyCode::Enter, _) | (KeyCode::Down, _) => {
                // Unfocus search, keep results visible, move to list
                editor.search_focused = false;
                KeybindingEditorAction::Consumed
            }
            (KeyCode::Up, _) => {
                // Unfocus search, move to list, select last item
                editor.search_focused = false;
                editor.selected = editor.filtered_indices.len().saturating_sub(1);
                editor.ensure_visible_public();
                KeybindingEditorAction::Consumed
            }
            (KeyCode::Tab, _) => {
                // Switch to record key mode
                editor.search_mode = SearchMode::RecordKey;
                editor.search_key_display.clear();
                editor.search_key_code = None;
                KeybindingEditorAction::Consumed
            }
            (KeyCode::Backspace, _) => {
                editor.search_query.pop();
                editor.apply_filters();
                KeybindingEditorAction::Consumed
            }
            (KeyCode::Char(c), m) if !m.contains(KeyModifiers::CONTROL) => {
                editor.search_query.push(c);
                editor.apply_filters();
                KeybindingEditorAction::Consumed
            }
            _ => KeybindingEditorAction::Consumed,
        },
        SearchMode::RecordKey => match (event.code, event.modifiers) {
            (KeyCode::Esc, KeyModifiers::NONE) => {
                editor.cancel_search();
                KeybindingEditorAction::Consumed
            }
            (KeyCode::Tab, KeyModifiers::NONE) => {
                // Switch to text mode, preserve query
                editor.search_mode = SearchMode::Text;
                editor.apply_filters();
                KeybindingEditorAction::Consumed
            }
            (KeyCode::Enter, KeyModifiers::NONE) => {
                // Unfocus search, keep results visible
                editor.search_focused = false;
                KeybindingEditorAction::Consumed
            }
            _ => {
                // Record the key
                editor.record_search_key(event);
                KeybindingEditorAction::Consumed
            }
        },
    }
}

fn handle_edit_dialog_input(
    editor: &mut KeybindingEditor,
    event: &KeyEvent,
) -> KeybindingEditorAction {
    // Take the dialog out to avoid borrow conflicts
    let mut dialog = match editor.edit_dialog.take() {
        Some(d) => d,
        None => return KeybindingEditorAction::Consumed,
    };

    // In special-capture mode on the key field, record the very next key
    // (including Esc, Tab, Enter) and exit capture mode.
    if dialog.capturing_special && dialog.focus_area == 0 {
        match event.code {
            KeyCode::Modifier(_) => {} // ignore bare modifier presses
            _ => {
                // Normalize the event so terminals that don't report SHIFT for
                // uppercase letters still produce a "Shift+letter" binding (e.g.
                // Shift+P stored as `key=p, modifiers=[shift]` rather than just
                // `key=p`). This mirrors the lookup-time normalization so the
                // recorded binding will match at runtime.
                let (norm_code, norm_mods) = normalize_key(event.code, event.modifiers);
                dialog.key_code = Some(norm_code);
                dialog.modifiers = norm_mods;
                // A recorded key replaces whatever the row held — including a
                // chord sequence, which must not linger and win at save time.
                dialog.chord_keys.clear();
                dialog.key_display = format_keybinding(&norm_code, &norm_mods);
                dialog.conflicts = editor.find_conflicts(norm_code, norm_mods, &dialog.context);
                dialog.capturing_special = false;
            }
        }
        editor.edit_dialog = Some(dialog);
        return KeybindingEditorAction::Consumed;
    }

    // Close dialog on Esc
    if event.code == KeyCode::Esc && event.modifiers == KeyModifiers::NONE {
        // Don't put it back - it's closed
        return KeybindingEditorAction::Consumed;
    }

    match dialog.focus_area {
        0 => {
            // Key recording area
            match (event.code, event.modifiers) {
                // Enter enters special-capture mode for the next keypress
                (KeyCode::Enter, KeyModifiers::NONE) => {
                    dialog.capturing_special = true;
                }
                (KeyCode::Tab | KeyCode::Down, KeyModifiers::NONE) => {
                    dialog.focus_area = 1;
                    dialog.mode = EditMode::EditingAction;
                }
                _ => {
                    // Keys are only recorded via capture mode (Enter then key).
                    // Ignore everything else in the key field.
                }
            }
        }
        1 => {
            // Action editing area with autocomplete
            match (event.code, event.modifiers) {
                (KeyCode::Tab, KeyModifiers::NONE) => {
                    // Accept selected autocomplete suggestion, or move to next field
                    if dialog.autocomplete_visible {
                        if let Some(sel) = dialog.autocomplete_selected {
                            if sel < dialog.autocomplete_suggestions.len() {
                                let suggestion = dialog.autocomplete_suggestions[sel].clone();
                                dialog.action_text = suggestion;
                                dialog.action_cursor = dialog.action_text.len();
                                dialog.autocomplete_visible = false;
                                dialog.autocomplete_selected = None;
                                dialog.action_error = None;
                            }
                        }
                    } else {
                        dialog.focus_area = 2;
                        dialog.mode = EditMode::EditingContext;
                    }
                }
                (KeyCode::BackTab, _) => {
                    dialog.autocomplete_visible = false;
                    dialog.focus_area = 0;
                    dialog.mode = EditMode::RecordingKey;
                }
                (KeyCode::Enter, KeyModifiers::NONE) => {
                    // Accept selected autocomplete suggestion, or move to buttons
                    if dialog.autocomplete_visible {
                        if let Some(sel) = dialog.autocomplete_selected {
                            if sel < dialog.autocomplete_suggestions.len() {
                                let suggestion = dialog.autocomplete_suggestions[sel].clone();
                                dialog.action_text = suggestion;
                                dialog.action_cursor = dialog.action_text.len();
                                dialog.autocomplete_visible = false;
                                dialog.autocomplete_selected = None;
                                dialog.action_error = None;
                            }
                        }
                    } else {
                        dialog.focus_area = 3;
                        dialog.selected_button = 0;
                        dialog.mode = EditMode::EditingContext;
                    }
                }
                (KeyCode::Up, _) if dialog.autocomplete_visible => {
                    // Navigate autocomplete up
                    if let Some(sel) = dialog.autocomplete_selected {
                        if sel > 0 {
                            dialog.autocomplete_selected = Some(sel - 1);
                        }
                    }
                }
                (KeyCode::Down, _) if dialog.autocomplete_visible => {
                    // Navigate autocomplete down
                    if let Some(sel) = dialog.autocomplete_selected {
                        let max = dialog.autocomplete_suggestions.len().saturating_sub(1);
                        if sel < max {
                            dialog.autocomplete_selected = Some(sel + 1);
                        }
                    }
                }
                (KeyCode::Up, KeyModifiers::NONE) => {
                    // Move to previous field (key)
                    dialog.autocomplete_visible = false;
                    dialog.focus_area = 0;
                    dialog.mode = EditMode::RecordingKey;
                }
                (KeyCode::Down, KeyModifiers::NONE) => {
                    // Move to next field (context)
                    dialog.focus_area = 2;
                    dialog.mode = EditMode::EditingContext;
                }
                (KeyCode::Esc, _) if dialog.autocomplete_visible => {
                    // Close autocomplete without closing dialog
                    dialog.autocomplete_visible = false;
                    dialog.autocomplete_selected = None;
                    // Put dialog back and return early (don't let outer Esc handler close dialog)
                    editor.edit_dialog = Some(dialog);
                    return KeybindingEditorAction::Consumed;
                }
                (KeyCode::Backspace, _) => {
                    if dialog.action_cursor > 0 {
                        dialog.action_cursor -= 1;
                        dialog.action_text.remove(dialog.action_cursor);
                        dialog.action_error = None;
                    }
                    // Put dialog back and update autocomplete
                    editor.edit_dialog = Some(dialog);
                    editor.update_autocomplete();
                    return KeybindingEditorAction::Consumed;
                }
                (KeyCode::Char(c), m) if !m.contains(KeyModifiers::CONTROL) => {
                    dialog.action_text.insert(dialog.action_cursor, c);
                    dialog.action_cursor += 1;
                    dialog.action_error = None;
                    // Put dialog back and update autocomplete
                    editor.edit_dialog = Some(dialog);
                    editor.update_autocomplete();
                    return KeybindingEditorAction::Consumed;
                }
                _ => {}
            }
        }
        2 => {
            // Context selection area
            match (event.code, event.modifiers) {
                (KeyCode::Tab | KeyCode::Down, KeyModifiers::NONE) => {
                    dialog.focus_area = 3;
                    dialog.selected_button = 0;
                }
                (KeyCode::BackTab, _) | (KeyCode::Up, KeyModifiers::NONE) => {
                    dialog.focus_area = 1;
                    dialog.mode = EditMode::EditingAction;
                }
                (KeyCode::Left, _) if dialog.context_option_index > 0 => {
                    dialog.context_option_index -= 1;
                    dialog.context = dialog.context_options[dialog.context_option_index].clone();
                    // Update conflicts
                    if let Some(key_code) = dialog.key_code {
                        dialog.conflicts =
                            editor.find_conflicts(key_code, dialog.modifiers, &dialog.context);
                    }
                }
                (KeyCode::Right, _)
                    if dialog.context_option_index + 1 < dialog.context_options.len() =>
                {
                    dialog.context_option_index += 1;
                    dialog.context = dialog.context_options[dialog.context_option_index].clone();
                    if let Some(key_code) = dialog.key_code {
                        dialog.conflicts =
                            editor.find_conflicts(key_code, dialog.modifiers, &dialog.context);
                    }
                }
                (KeyCode::Enter, _) => {
                    dialog.focus_area = 3;
                    dialog.selected_button = 0;
                }
                _ => {}
            }
        }
        3 => {
            // Buttons area
            match (event.code, event.modifiers) {
                (KeyCode::Tab, KeyModifiers::NONE) => {
                    if dialog.selected_button < 1 {
                        // Move from Save to Cancel
                        dialog.selected_button = 1;
                    } else {
                        // Wrap from Cancel to Key field
                        dialog.focus_area = 0;
                        dialog.mode = EditMode::RecordingKey;
                    }
                }
                (KeyCode::BackTab, _) => {
                    if dialog.selected_button > 0 {
                        // Move from Cancel to Save
                        dialog.selected_button = 0;
                    } else {
                        // Wrap from Save to Context field
                        dialog.focus_area = 2;
                        dialog.mode = EditMode::EditingContext;
                    }
                }
                (KeyCode::Up, KeyModifiers::NONE) => {
                    dialog.focus_area = 2;
                    dialog.mode = EditMode::EditingContext;
                }
                (KeyCode::Left, _) if dialog.selected_button > 0 => {
                    dialog.selected_button -= 1;
                }
                (KeyCode::Right, _) if dialog.selected_button < 1 => {
                    dialog.selected_button += 1;
                }
                (KeyCode::Enter, _) => {
                    if dialog.selected_button == 0 {
                        // Save - put the dialog back first so apply_edit_dialog can take it
                        editor.edit_dialog = Some(dialog);
                        if let Some(err) = editor.apply_edit_dialog() {
                            // Validation failed - dialog is still open with error
                            return KeybindingEditorAction::StatusMessage(err);
                        }
                        return KeybindingEditorAction::Consumed;
                    } else {
                        // Cancel - don't put dialog back
                        return KeybindingEditorAction::Consumed;
                    }
                }
                _ => {}
            }
        }
        _ => {}
    }

    // Put the dialog back
    editor.edit_dialog = Some(dialog);
    KeybindingEditorAction::Consumed
}

fn handle_confirm_input(editor: &mut KeybindingEditor, event: &KeyEvent) -> KeybindingEditorAction {
    match (event.code, event.modifiers) {
        (KeyCode::Left, _) => {
            if editor.confirm_selection > 0 {
                editor.confirm_selection -= 1;
            }
            KeybindingEditorAction::Consumed
        }
        (KeyCode::Right, _) => {
            if editor.confirm_selection < 2 {
                editor.confirm_selection += 1;
            }
            KeybindingEditorAction::Consumed
        }
        (KeyCode::Enter, _) => match editor.confirm_selection {
            0 => KeybindingEditorAction::SaveAndClose,
            1 => KeybindingEditorAction::Close, // Discard
            _ => {
                editor.showing_confirm_dialog = false;
                KeybindingEditorAction::Consumed
            }
        },
        (KeyCode::Esc, _) => {
            editor.showing_confirm_dialog = false;
            KeybindingEditorAction::Consumed
        }
        _ => KeybindingEditorAction::Consumed,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::app::keybinding_editor::EditBindingState;
    use crate::config::Config;
    use crate::input::buffer_mode::ModeRegistry;
    use crate::input::command_registry::CommandRegistry;
    use crate::input::keybindings::KeybindingResolver;

    // **The two placement tests moved with the placement.** They pinned that
    // the modal stays inside the area it is handed and centres in it — the
    // orchestrator-dock regression, where it was placed relative to column 0
    // and bled left under the dock. That is `view::shell::keybinding`'s now
    // (`the_box_centres_beside_the_dock`), stated against the region the layer
    // names rather than against a rectangle a caller passed in.

    fn make_editor() -> KeybindingEditor {
        let config = Config::default();
        let resolver = KeybindingResolver::new(&config);
        let mode_registry = ModeRegistry::new();
        let cmd_registry = CommandRegistry::new();
        let menu_names: Vec<String> = ["File", "Edit", "View"]
            .iter()
            .map(|s| s.to_string())
            .collect();
        KeybindingEditor::new(
            &config,
            &resolver,
            &mode_registry,
            &cmd_registry,
            String::from("/tmp/fresh-config.toml"),
            &menu_names,
        )
    }

    /// Drive the add-binding dialog through one "capture key" flow and
    /// return the resulting (key_code, modifiers).
    fn capture_in_add_dialog(event: KeyEvent) -> (Option<KeyCode>, KeyModifiers) {
        let mut editor = make_editor();
        editor.edit_dialog = Some(EditBindingState::new_add());
        // Enter the "press a key" capture mode by sending Enter on the key
        // field, then send the simulated event.
        let enter = KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE);
        handle_keybinding_editor_input(&mut editor, &enter);
        handle_keybinding_editor_input(&mut editor, &event);
        let dialog = editor.edit_dialog.as_ref().expect("dialog still open");
        (dialog.key_code, dialog.modifiers)
    }

    #[test]
    fn add_dialog_records_shift_when_terminal_omits_shift_modifier() {
        // Regression for https://github.com/sinelaw/fresh/issues/1899
        // When a non-kitty terminal sends Char('P') with no modifier (the
        // typical case for Shift+P), the add-binding dialog must still
        // capture this as a "Shift+P" binding rather than just "p".
        let plain_upper = KeyEvent::new(KeyCode::Char('P'), KeyModifiers::empty());
        let (code, mods) = capture_in_add_dialog(plain_upper);
        assert_eq!(code, Some(KeyCode::Char('p')));
        assert!(
            mods.contains(KeyModifiers::SHIFT),
            "Shift+P (sent as plain 'P') must capture SHIFT (got modifiers={:?})",
            mods
        );
    }

    #[test]
    fn add_dialog_records_shift_when_terminal_includes_shift_modifier() {
        // The fix must not regress the kitty-protocol path either.
        let kitty_shift = KeyEvent::new(KeyCode::Char('P'), KeyModifiers::SHIFT);
        let (code, mods) = capture_in_add_dialog(kitty_shift);
        assert_eq!(code, Some(KeyCode::Char('p')));
        assert!(mods.contains(KeyModifiers::SHIFT));
    }

    #[test]
    fn add_dialog_preserves_ctrl_when_capturing_upper_letter() {
        // CapsLock+Ctrl+A — uppercase letter with CONTROL modifier — should
        // record as plain Ctrl+A (no inferred SHIFT) so the long-standing
        // caps-lock-tolerant lookup keeps working.
        let caps_ctrl_a = KeyEvent::new(KeyCode::Char('A'), KeyModifiers::CONTROL);
        let (code, mods) = capture_in_add_dialog(caps_ctrl_a);
        assert_eq!(code, Some(KeyCode::Char('a')));
        assert_eq!(mods, KeyModifiers::CONTROL);
    }
}
