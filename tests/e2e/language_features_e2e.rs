use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use tempfile::TempDir;

/// Test auto-close quotes in Rust (should happen)
#[test]
fn test_auto_close_quotes_rust() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "").unwrap();

    let mut config = Config::default();
    config.editor.auto_indent = true;

    let mut harness = EditorTestHarness::create(
        80,
        24,
        HarnessOptions::new()
            .without_empty_plugins_dir()
            .with_config(config),
    )
    .unwrap();
    harness.open_file(&file_path).unwrap();

    // Verify language detection
    let language = &harness.editor().active_state().language;
    assert_eq!(language, "rust", "Language should be rust");

    // Type a quote
    harness.type_text("\"").unwrap();
    harness.render().unwrap();

    // Should have two quotes: ""
    harness.assert_buffer_content("\"\"");

    // Cursor should be at 1 (between the quotes)
    let cursor = harness.editor().active_state().cursors.primary();
    assert_eq!(cursor.position, 1);
}

/// Test auto-close quotes in Text (should NOT happen)
#[test]
fn test_no_auto_close_quotes_text() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "").unwrap();

    let mut config = Config::default();
    config.editor.auto_indent = true;

    let mut harness = EditorTestHarness::create(
        80,
        24,
        HarnessOptions::new()
            .without_empty_plugins_dir()
            .with_config(config),
    )
    .unwrap();
    harness.open_file(&file_path).unwrap();

    // Type a quote
    harness.type_text("\"").unwrap();
    harness.render().unwrap();

    // Should have only one quote: "
    harness.assert_buffer_content("\"");

    // Cursor should be at 1 (after the quote)
    let cursor = harness.editor().active_state().cursors.primary();
    assert_eq!(cursor.position, 1);
}

/// Test word movement with punctuation
#[test]
fn test_word_movement_e2e() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "foo.bar").unwrap();

    let mut harness =
        EditorTestHarness::create(80, 24, HarnessOptions::new().without_empty_plugins_dir())
            .unwrap();
    harness.open_file(&file_path).unwrap();

    // Cursor at 0. Ctrl+Right.
    harness
        .send_key(KeyCode::Right, KeyModifiers::CONTROL)
        .unwrap();

    // Should stop at '.' (pos 3)
    let cursor = harness.editor().active_state().cursors.primary();
    assert_eq!(cursor.position, 3);

    // Ctrl+Right again.
    harness
        .send_key(KeyCode::Right, KeyModifiers::CONTROL)
        .unwrap();

    // Should stop at 'bar' (pos 4)
    let cursor = harness.editor().active_state().cursors.primary();
    assert_eq!(cursor.position, 4);
}

/// Test Ctrl+D multicursor
#[test]
fn test_ctrl_d_multicursor_e2e() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "foo foo foo").unwrap();

    let mut harness =
        EditorTestHarness::create(80, 24, HarnessOptions::new().without_empty_plugins_dir())
            .unwrap();
    harness.open_file(&file_path).unwrap();

    // Select first "foo" (0..3)
    harness
        .send_key(KeyCode::Right, KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::SHIFT)
        .unwrap();

    let cursor = harness.editor().active_state().cursors.primary();
    assert_eq!(cursor.position, 3);
    assert_eq!(cursor.anchor, Some(0));

    // Press Ctrl+D (mapped to 'd' with control usually, relying on keybindings)
    // Assuming default keybindings map Ctrl+D to AddCursorNextMatch
    harness
        .send_key(KeyCode::Char('d'), KeyModifiers::CONTROL)
        .unwrap();

    // Verify 2 cursors
    let count = harness.editor().active_state().cursors.iter().count();
    assert_eq!(count, 2);

    // Check positions: second cursor should be at 7 (4..7)
    let cursors: Vec<_> = harness
        .editor()
        .active_state()
        .cursors
        .iter()
        .map(|(_, c)| c.position)
        .collect();
    assert!(cursors.contains(&7));
}
