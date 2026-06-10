// End-to-end tests for `.editorconfig` support (issue #959).
//
// These drive the real open-file + keyboard pipeline and assert on the
// resulting buffer content, which is what the user sees and saves. Each test
// fails before the feature is wired up (Tab would insert the editor's default
// indentation) and passes once `.editorconfig` indentation is honored.

mod common;

use common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

/// `indent_style = tab` makes Tab insert a literal tab character, overriding
/// the editor default (spaces).
#[test]
fn editorconfig_tab_style_inserts_tab() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let dir = harness.temp_dir_path().unwrap().to_path_buf();

    fs::write(
        dir.join(".editorconfig"),
        "root = true\n[*]\nindent_style = tab\n",
    )
    .unwrap();
    let file = dir.join("notes.txt");
    fs::write(&file, "").unwrap();

    harness.open_file(&file).unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();

    assert_eq!(
        harness.get_buffer_content().unwrap(),
        "\t",
        "indent_style=tab should make Tab insert a tab character"
    );
}

/// `indent_style = space` with `indent_size = 2` makes Tab insert two spaces,
/// overriding the editor default width (4).
#[test]
fn editorconfig_space_style_inserts_sized_spaces() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let dir = harness.temp_dir_path().unwrap().to_path_buf();

    fs::write(
        dir.join(".editorconfig"),
        "root = true\n[*]\nindent_style = space\nindent_size = 2\n",
    )
    .unwrap();
    let file = dir.join("notes.txt");
    fs::write(&file, "").unwrap();

    harness.open_file(&file).unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();

    assert_eq!(
        harness.get_buffer_content().unwrap(),
        "  ",
        "indent_style=space, indent_size=2 should make Tab insert two spaces"
    );
}

/// A section glob scopes settings to matching files only.
///
/// We deliberately target `*.txt` (whose default indentation is spaces) rather
/// than a language with tab defaults, so the assertion isolates `.editorconfig`
/// behavior from any language config.
#[test]
fn editorconfig_section_glob_scopes_settings() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let dir = harness.temp_dir_path().unwrap().to_path_buf();

    // Tabs only for *.txt; other files keep the editor default (spaces).
    fs::write(
        dir.join(".editorconfig"),
        "root = true\n[*.txt]\nindent_style = tab\n",
    )
    .unwrap();

    let txt = dir.join("notes.txt");
    fs::write(&txt, "").unwrap();
    harness.open_file(&txt).unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    assert_eq!(
        harness.get_buffer_content().unwrap(),
        "\t",
        "*.txt matches the section and should use tabs"
    );

    let md = dir.join("readme.md");
    fs::write(&md, "").unwrap();
    harness.open_file(&md).unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    assert!(
        harness.get_buffer_content().unwrap().starts_with(' '),
        "non-matching files keep the default space indentation"
    );
}
