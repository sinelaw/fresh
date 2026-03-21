//! Helper/utility functions for the keybinding editor.

use crate::config::KeyPress;
use crate::input::keybindings::{format_keybinding, KeybindingResolver};
use crossterm::event::{KeyCode, KeyModifiers};

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
