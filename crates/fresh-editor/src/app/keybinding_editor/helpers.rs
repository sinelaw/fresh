//! Helper/utility functions for the keybinding editor.

use crate::config::KeyPress;
use crate::input::keybindings::{format_keybinding, KeyContext, KeybindingResolver};
use crossterm::event::{KeyCode, KeyModifiers};

/// The canonical spelling of a `when` clause.
///
/// `KeyContext::from_when_clause` accepts aliases — `file_explorer` and
/// `fileExplorer` are the same context, and the built-in keymaps use both — so
/// anything that groups or matches rows by their context string has to fold
/// them together first. An unrecognised clause (a plugin `mode:` context, or a
/// typo) is passed through unchanged so it still round-trips.
pub fn canonical_context(when: &str) -> String {
    KeyContext::from_when_clause(when)
        .map(|ctx| ctx.to_when_clause())
        .unwrap_or_else(|| when.to_string())
}

/// Format chord keys for display
pub fn format_chord_keys(keys: &[KeyPress]) -> String {
    keys.iter()
        .filter_map(|kp| {
            let key_code = KeybindingResolver::parse_key_public(&kp.key)?;
            let modifiers = KeybindingResolver::parse_modifiers_public(&kp.modifiers);
            Some(format_keybinding(&key_code, &modifiers))
        })
        .collect::<Vec<_>>()
        .join(" ")
}

/// Convert a KeyCode back to a config-friendly name
pub fn key_code_to_config_name(key_code: KeyCode) -> String {
    match key_code {
        KeyCode::Char(c) => c.to_lowercase().to_string(),
        KeyCode::Enter => "Enter".to_string(),
        KeyCode::Tab => "Tab".to_string(),
        KeyCode::Backspace => "Backspace".to_string(),
        KeyCode::Delete => "Delete".to_string(),
        KeyCode::Esc => "Escape".to_string(),
        KeyCode::Up => "Up".to_string(),
        KeyCode::Down => "Down".to_string(),
        KeyCode::Left => "Left".to_string(),
        KeyCode::Right => "Right".to_string(),
        KeyCode::Home => "Home".to_string(),
        KeyCode::End => "End".to_string(),
        KeyCode::PageUp => "PageUp".to_string(),
        KeyCode::PageDown => "PageDown".to_string(),
        KeyCode::Insert => "Insert".to_string(),
        // The one keypad key with no main-keyboard equivalent, so the one the
        // Debug spelling (`KeypadBegin`) would strand: `parse_key` knows it by
        // its keysym name, from the same table the input parser decodes it
        // with. Every other name above is in `keybindings::NAMED_KEYS`, and
        // `config_names_round_trip` is what keeps that true.
        KeyCode::KeypadBegin => "kp_begin".to_string(),
        KeyCode::F(n) => format!("F{}", n),
        _ => format!("{:?}", key_code),
    }
}

/// Convert KeyModifiers back to config-friendly modifier names
pub fn modifiers_to_config_names(modifiers: KeyModifiers) -> Vec<String> {
    let mut names = Vec::new();
    if modifiers.contains(KeyModifiers::CONTROL) {
        names.push("ctrl".to_string());
    }
    if modifiers.contains(KeyModifiers::ALT) {
        names.push("alt".to_string());
    }
    if modifiers.contains(KeyModifiers::SHIFT) {
        names.push("shift".to_string());
    }
    if modifiers.contains(KeyModifiers::SUPER) {
        names.push("super".to_string());
    }
    names
}
