//! E2E tests for user-configured languages in the language selector popup.
//!
//! When a user configures a custom language (e.g. "fish") in config.json with
//! a grammar that doesn't exist in syntect, the language should still appear
//! in the Set Language popup, be marked as current when active, and be
//! selectable to switch to.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::{Config, LanguageConfig};
use std::collections::HashMap;

/// Helper: open command palette, type a command name, and press Enter.
fn run_command(harness: &mut EditorTestHarness, command_name: &str) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    harness.type_text(command_name).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

fn make_fish_config() -> Config {
    let mut languages = HashMap::new();
    languages.insert(
        "fish".to_string(),
        LanguageConfig {
            extensions: vec!["fish".to_string()],
            grammar: "fish".to_string(),
            comment_prefix: Some("#".to_string()),
            auto_indent: true,
            ..Default::default()
        },
    );
    let mut config = Config::default();
    config.languages = languages;
    config
}

/// Opening a .fish file with a "fish" language config should show "fish" in
/// the status bar, and the Set Language popup should list "fish" as an option
/// marked as current.
#[test]
fn test_config_language_visible_in_set_language_popup() {
    let config = make_fish_config();

    let mut harness = EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new()
            .with_project_root()
            .with_full_grammar_registry()
            .with_config(config),
    )
    .unwrap();
    let project_dir = harness.project_dir().unwrap();

    // Create and open a .fish file
    let fish_file = project_dir.join("test.fish");
    std::fs::write(&fish_file, "#!/usr/bin/fish\necho hello\n").unwrap();
    harness.open_file(&fish_file).unwrap();
    harness.render().unwrap();

    // The status bar should show "fish"
    let status_bar = harness.get_status_bar();
    assert!(
        status_bar.contains("fish"),
        "Status bar should show 'fish' for a .fish file. Got: {}",
        status_bar
    );

    // Open the Set Language popup
    run_command(&mut harness, "Set Language");
    harness.wait_for_prompt().unwrap();
    harness.render().unwrap();

    // Type "fish" to filter the language list
    harness.type_text("fish").unwrap();
    harness.render().unwrap();

    // The popup should show "fish" as an option
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("fish"),
        "Set Language popup should show 'fish' as a language option.\nScreen:\n{}",
        screen
    );

    // "fish" should be marked as current
    assert!(
        screen.contains("current"),
        "The 'fish' option should be marked as 'current' since the active file is .fish.\nScreen:\n{}",
        screen
    );

    // The popup should show "config" as the source for fish
    assert!(
        screen.contains("config"),
        "The 'fish' entry should show 'config' as its source.\nScreen:\n{}",
        screen
    );

    // Dismiss prompt
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
}

/// The Set Language popup should NOT show "bash" as current when the active
/// file is a .fish file with a custom fish language config.
/// Instead, "fish" should be marked as current.
#[test]
fn test_config_language_selector_does_not_select_bash_for_fish() {
    let config = make_fish_config();

    let mut harness = EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new()
            .with_project_root()
            .with_full_grammar_registry()
            .with_config(config),
    )
    .unwrap();
    let project_dir = harness.project_dir().unwrap();

    let fish_file = project_dir.join("config.fish");
    std::fs::write(&fish_file, "set -x PATH $HOME/bin $PATH\n").unwrap();
    harness.open_file(&fish_file).unwrap();
    harness.render().unwrap();

    // The status bar should show "fish", not "Bourne Again Shell (bash)"
    let status_bar = harness.get_status_bar();
    assert!(
        status_bar.contains("fish"),
        "Status bar should show 'fish', not bash. Got: {}",
        status_bar
    );
    assert!(
        !status_bar.contains("bash") && !status_bar.contains("Bourne"),
        "Status bar should NOT show bash/Bourne for a .fish file. Got: {}",
        status_bar
    );

    // Open the Set Language popup (unfiltered) and check the screen
    run_command(&mut harness, "Set Language");
    harness.wait_for_prompt().unwrap();
    harness.render().unwrap();

    // The popup should contain "fish" marked as current somewhere on screen.
    // Scroll to where fish would be by typing "fish" to filter.
    harness.type_text("fish").unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();

    // "fish" should appear with "(current)" annotation in the description
    assert!(
        screen.contains("fish") && screen.contains("current"),
        "fish should be listed and marked as current in the Set Language popup.\nScreen:\n{}",
        screen
    );

    // Check that the row with "Bourne" (if visible) does NOT have "current"
    // by checking individual screen rows
    let height = 30u16;
    let mut bash_is_current = false;
    for row in 0..height {
        let line = harness.screen_row_text(row);
        if line.contains("Bourne") && line.contains("current") {
            bash_is_current = true;
            break;
        }
    }
    assert!(
        !bash_is_current,
        "Bourne Again Shell (bash) should NOT be marked as current for a .fish file.\nScreen:\n{}",
        screen
    );

    // Dismiss prompt
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
}

/// Selecting "fish" from the Set Language popup should set the language to
/// "fish" and the status bar should update accordingly.
#[test]
fn test_select_config_language_from_popup_updates_status_bar() {
    let config = make_fish_config();

    let mut harness = EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new()
            .with_project_root()
            .with_full_grammar_registry()
            .with_config(config),
    )
    .unwrap();
    let project_dir = harness.project_dir().unwrap();

    // Start with a plain text file
    let txt_file = project_dir.join("script.txt");
    std::fs::write(&txt_file, "echo hello\n").unwrap();
    harness.open_file(&txt_file).unwrap();
    harness.render().unwrap();

    // Status bar should show "Text" initially
    let status_bar = harness.get_status_bar();
    assert!(
        status_bar.contains("Text"),
        "Initial status bar should show 'Text'. Got: {}",
        status_bar
    );

    // Use Set Language to switch to fish
    run_command(&mut harness, "Set Language");
    harness.wait_for_prompt().unwrap();
    harness.render().unwrap();

    harness.type_text("fish").unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Status bar should now show "fish"
    let status_bar = harness.get_status_bar();
    assert!(
        status_bar.contains("fish"),
        "After selecting fish, status bar should show 'fish'. Got: {}",
        status_bar
    );

    // Re-open Set Language popup and verify fish is now marked as current
    run_command(&mut harness, "Set Language");
    harness.wait_for_prompt().unwrap();
    harness.render().unwrap();

    harness.type_text("fish").unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("fish") && screen.contains("current"),
        "After switching to fish, the Set Language popup should show fish as current.\nScreen:\n{}",
        screen
    );

    // Dismiss prompt
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
}
