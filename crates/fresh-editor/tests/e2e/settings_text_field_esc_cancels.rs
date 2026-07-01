//! E2E: in the main Settings view (not the entry dialog), editing a plain
//! `Text` field follows the platform-standard edit convention —
//! **Enter commits** the typed value and leaves edit mode, and **Esc reverts**
//! it to what it was before the edit began. This matches Windows, macOS, and
//! the web, where Esc cancels an in-progress edit rather than accepting it.
//!
//! The reproducer field is Terminal → Command (`/terminal/shell/command`), the
//! same one reported as broken. Pre-change, Enter was a no-op on a Text field
//! (trapping the user in edit mode) and Esc *accepted* the edit, so both
//! assertions below fail without the fix. Tests drive only keyboard events and
//! assert on rendered output, per CONTRIBUTING.md ("E2E Tests Observe, Not
//! Inspect").

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::{Config, TerminalShellConfig};

/// Return the full text of the first rendered row that contains `needle`.
fn row_with(harness: &EditorTestHarness, needle: &str) -> String {
    harness
        .screen_to_string()
        .lines()
        .find(|line| line.contains(needle))
        .unwrap_or("")
        .to_string()
}

/// Open Settings and focus the Terminal → Command text field via search.
/// Leaves the field focused in the Settings panel (not yet in edit mode).
fn focus_terminal_command(harness: &mut EditorTestHarness) {
    harness.open_settings().unwrap();
    // Search by the field's unique description phrase and jump to it.
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    harness.type_text("Executable to launch").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    assert!(
        row_with(harness, "Command").contains('['),
        "precondition: the Command value cell should be visible; screen was:\n{}",
        harness.screen_to_string()
    );
}

/// Enter commits the typed value AND exits edit mode. Proven on screen: after
/// Enter, a plain (non-navigation) keystroke no longer lands in the field, so
/// the value is unchanged — which only holds if Enter left edit mode.
#[test]
fn enter_commits_and_exits_text_field() {
    let mut harness = EditorTestHarness::with_config(140, 40, Config::default()).unwrap();
    harness.render().unwrap();

    focus_terminal_command(&mut harness);

    // Enter edit mode and type a value.
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("bashrc").unwrap();
    harness.render().unwrap();
    assert!(
        row_with(&harness, "Command").contains("bashrc"),
        "the edit buffer should show the typed value; row: {:?}",
        row_with(&harness, "Command")
    );

    // Enter must commit and leave edit mode.
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // A plain letter now that we're out of edit mode is not text input — if
    // Enter had trapped us in edit mode (the bug), this 'X' would append.
    harness
        .send_key(KeyCode::Char('X'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    let row = row_with(&harness, "Command");
    assert!(
        row.contains("bashrc") && !row.contains("bashrcX") && !row.contains("bashrcx"),
        "Enter must commit the value and exit edit mode (no stray 'X'); row: {:?}",
        row
    );
}

/// Esc discards an in-progress edit and restores the field's previous value —
/// here the empty default. Without the fix Esc accepts the edit, so the typed
/// text survives on screen.
#[test]
fn esc_reverts_text_field_to_empty() {
    let mut harness = EditorTestHarness::with_config(140, 40, Config::default()).unwrap();
    harness.render().unwrap();

    focus_terminal_command(&mut harness);

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("hello").unwrap();
    harness.render().unwrap();
    assert!(
        row_with(&harness, "Command").contains("hello"),
        "the edit buffer should show the typed text; row: {:?}",
        row_with(&harness, "Command")
    );

    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    assert!(
        !row_with(&harness, "Command").contains("hello"),
        "Esc must revert the field to its empty pre-edit value; row: {:?}",
        row_with(&harness, "Command")
    );
}

/// Esc reverts to a *non-empty* prior value too, and clears the "modified"
/// bookkeeping the edit triggered: a field whose committed value came from
/// config returns to that value with no modified marker after Esc.
#[test]
fn esc_reverts_text_field_to_prior_value() {
    let mut config = Config::default();
    config.terminal.shell = Some(TerminalShellConfig {
        command: "bash".to_string(),
        args: Vec::new(),
    });
    let mut harness = EditorTestHarness::with_config(140, 40, config).unwrap();
    harness.render().unwrap();

    focus_terminal_command(&mut harness);
    assert!(
        row_with(&harness, "Command").contains("bash"),
        "precondition: Command starts as the configured 'bash'; row: {:?}",
        row_with(&harness, "Command")
    );

    // Edit: the first keystroke replaces the value (select-all-on-type).
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("zsh").unwrap();
    harness.render().unwrap();
    assert!(
        row_with(&harness, "Command").contains("zsh"),
        "the edit buffer should show the replacement 'zsh'; row: {:?}",
        row_with(&harness, "Command")
    );

    // Esc reverts to 'bash' and drops the modified state.
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let row = row_with(&harness, "Command");
    assert!(
        row.contains("bash") && !row.contains("zsh"),
        "Esc must restore the prior 'bash' value; row: {:?}",
        row
    );
    assert!(
        !row.contains('●') && !row.to_lowercase().contains("modified"),
        "Esc must clear the modified marker when restoring the original; row: {:?}",
        row
    );
}
