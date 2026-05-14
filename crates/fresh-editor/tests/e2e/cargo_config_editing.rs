// Regression test for issue #1970: All files under `.cargo` are treated as library files.
//
// Previously, `is_library_path` matched anything containing `/.cargo/`, so user-editable
// files like `.cargo/config.toml` were opened read-only even though they aren't
// downloaded crate sources. Only files under `.cargo/registry/` and `.cargo/git/`
// should be considered library files.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use tempfile::TempDir;

/// `.cargo/config.toml` (project-level or global) should be editable.
#[test]
fn test_cargo_config_toml_is_editable() {
    let temp_dir = TempDir::new().unwrap();
    let cargo_dir = temp_dir.path().join(".cargo");
    std::fs::create_dir_all(&cargo_dir).unwrap();
    let config_path = cargo_dir.join("config.toml");
    std::fs::write(&config_path, "[build]\njobs = 4\n").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&config_path).unwrap();
    harness.render().unwrap();

    // Type a unique two-character sequence; it should appear in the buffer.
    harness
        .send_key(KeyCode::Char('Q'), KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(KeyCode::Char('Z'), KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("QZ");
    harness.assert_screen_not_contains("Editing disabled");
}

/// Files under `.cargo/registry/` are downloaded crate sources and must stay read-only.
#[test]
fn test_cargo_registry_file_stays_readonly() {
    let temp_dir = TempDir::new().unwrap();
    let registry_dir = temp_dir
        .path()
        .join(".cargo/registry/src/index.crates.io-abc/serde-1.0.0/src");
    std::fs::create_dir_all(&registry_dir).unwrap();
    let lib_file = registry_dir.join("lib.rs");
    std::fs::write(&lib_file, "pub fn placeholder() {}\n").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&lib_file).unwrap();
    harness.render().unwrap();

    let screen_before = harness.screen_to_string();

    // Typing should be a no-op; the buffer content must not change.
    harness
        .send_key(KeyCode::Char('x'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("pub fn placeholder() {}");

    let screen_after = harness.screen_to_string();
    let content_before: Vec<&str> = screen_before.lines().take(5).collect();
    let content_after: Vec<&str> = screen_after.lines().take(5).collect();
    assert_eq!(
        content_before, content_after,
        "Buffer content should not change when typing in a read-only registry source file"
    );
}
