//! Regression test for <https://github.com/sinelaw/fresh/issues/3575>:
//!
//! Replace and Query Replace in plain-text mode ignored the Case Sensitive and
//! Whole Word toggles. The prompt highlighted matches according to the
//! toggles, but the replacement always matched case-sensitively and by
//! substring, so with the shipped default (case folded) `todo` highlighted
//! `TODO`, `Todo` and `todo` and then replaced only the last.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use tempfile::TempDir;

/// Open `content`, run Replace (Ctrl+R) for `search` -> `replacement`, applying
/// `toggles` (Alt+<key>) while the search prompt is open, and return the screen
/// once the status line reports the replace.
fn replace_all(content: &str, toggles: &[char], search: &str, replacement: &str) -> String {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("notes.txt");
    std::fs::write(&file_path, content).unwrap();

    let mut harness =
        EditorTestHarness::create(100, 24, HarnessOptions::new().without_empty_plugins_dir())
            .unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('r'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_screen_contains("Replace: ").unwrap();
    for toggle in toggles {
        harness
            .send_key(KeyCode::Char(*toggle), KeyModifiers::ALT)
            .unwrap();
    }
    harness.type_text(search).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_for_screen_contains(&format!("Replace '{search}' with: "))
        .unwrap();
    harness.type_text(replacement).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_screen_contains("Replaced ").unwrap();
    harness.screen_to_string()
}

#[test]
fn replace_all_folds_case_when_case_sensitive_is_off() {
    let screen = replace_all("TODO Todo todo\n", &[], "todo", "done");
    assert!(
        screen.contains("done done done"),
        "all three spellings should be replaced, got:\n{screen}"
    );
}

#[test]
fn replace_all_stays_case_exact_when_case_sensitive_is_on() {
    let screen = replace_all("TODO Todo todo\n", &['c'], "todo", "done");
    assert!(
        screen.contains("TODO Todo done"),
        "only the exact spelling should be replaced, got:\n{screen}"
    );
}

#[test]
fn replace_all_honours_whole_word() {
    let screen = replace_all("cat concat cat\n", &['w'], "cat", "dog");
    assert!(
        screen.contains("dog concat dog"),
        "`concat` is not a whole-word match, got:\n{screen}"
    );
}

#[test]
fn case_folded_replace_keeps_the_replacement_literal() {
    // Outside regex mode `$1` and `\n` are ordinary text.
    let screen = replace_all("TODO\n", &[], "todo", "$1\\n");
    assert!(
        screen.contains("$1\\n"),
        "the replacement should be inserted verbatim, got:\n{screen}"
    );
}

#[test]
fn query_replace_folds_case_and_keeps_the_replacement_literal() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("notes.txt");
    std::fs::write(&file_path, "TODO Todo todo\n").unwrap();

    let mut harness =
        EditorTestHarness::create(100, 24, HarnessOptions::new().without_empty_plugins_dir())
            .unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    harness
        .send_key(
            KeyCode::Char('r'),
            KeyModifiers::CONTROL | KeyModifiers::ALT,
        )
        .unwrap();
    harness.wait_for_screen_contains("Query replace: ").unwrap();
    harness.type_text("todo").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_for_screen_contains("Query replace 'todo' with: ")
        .unwrap();
    harness.type_text("$0").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_screen_contains("Replace? (y)es").unwrap();

    // Answer yes for each of the three spellings.
    for _ in 0..3 {
        harness.type_text("y").unwrap();
    }
    harness.wait_for_screen_contains("Replaced 3 occ").unwrap();
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("$0 $0 $0"),
        "all three spellings should be replaced with the literal text, got:\n{screen}"
    );
}
