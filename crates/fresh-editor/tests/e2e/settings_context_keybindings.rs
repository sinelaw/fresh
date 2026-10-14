//! Custom keybindings in the `"settings"` context must fire while the
//! Settings dialog has the keyboard.
//!
//! The keymap accepts `"when": "settings"` and the keybinding editor offers
//! "settings" as a context, but the dialog's keys went straight to
//! `SettingsState::dispatch_input` (`app/chrome/modals.rs`
//! `dispatch_settings_key`), which hard-codes its keys; a key it ignored was
//! dropped without consulting the `KeybindingResolver`, so a user binding
//! saved in that context was silently dead.
//!
//! Ctrl+J is used because nothing in the dialog's hard-coded handlers
//! answers it (every panel returns `InputResult::Ignored` for it) and no
//! built-in keymap binds Ctrl+J.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::{Config, Keybinding};
use std::collections::HashMap;

fn harness_with_settings_binding(action: &str) -> EditorTestHarness {
    let mut config = Config::default();
    config.keybindings.push(Keybinding {
        key: "j".to_string(),
        modifiers: vec!["ctrl".to_string()],
        keys: vec![],
        chord: String::new(),
        action: action.to_string(),
        args: HashMap::new(),
        when: Some("settings".to_string()),
    });
    EditorTestHarness::create(100, 40, HarnessOptions::new().with_config(config)).unwrap()
}

/// A user binding `Ctrl+J → settings_help` in the settings context opens the
/// help overlay, as the built-in `?` does (`settings::test_settings_help_overlay`).
#[test]
fn custom_settings_context_binding_opens_help() {
    let mut harness = harness_with_settings_binding("settings_help");

    harness.open_settings().unwrap();
    harness.assert_screen_not_contains("Keyboard Shortcuts");

    harness
        .send_key(KeyCode::Char('j'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("Keyboard Shortcuts");
}

/// A user binding `Ctrl+J → close_settings` in the settings context closes
/// the (unmodified) dialog.
#[test]
fn custom_settings_context_binding_closes_settings() {
    let mut harness = harness_with_settings_binding("close_settings");

    harness.open_settings().unwrap();
    assert!(harness.editor().is_settings_open());

    harness
        .send_key(KeyCode::Char('j'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    assert!(
        !harness.editor().is_settings_open(),
        "Ctrl+J is bound to close_settings with when=settings; the Settings \
         dialog must close when it is pressed"
    );
}
