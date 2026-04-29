//! E2E coverage for the "Copy File Path" / "Copy Relative File Path" commands
//! (issue #1752): user-visible status message + clipboard contents.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

/// Helper: open the command palette, type the given query, accept the first
/// suggestion via Tab, and execute it with Enter.
fn run_command_palette(harness: &mut EditorTestHarness, query: &str) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text(query).unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

#[test]
fn copy_relative_file_path_via_command_palette() {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(120, 24, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    let file_path = project_root.join("hello.txt");
    fs::write(&file_path, "hi\n").unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    run_command_palette(&mut harness, "Copy Relative File Path");

    // Status bar surfaces the copied path so the user gets confirmation.
    harness.assert_screen_contains("Copied path:");
    harness.assert_screen_contains("hello.txt");

    let clipboard = harness.editor_mut().clipboard_content_for_test();
    assert_eq!(
        clipboard, "hello.txt",
        "relative copy should drop the workspace root prefix"
    );
}

#[test]
fn copy_file_path_via_command_palette_uses_absolute_path() {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(120, 24, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    let file_path = project_root.join("absolute.txt");
    fs::write(&file_path, "x\n").unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    run_command_palette(&mut harness, "Copy File Path");

    let clipboard = harness.editor_mut().clipboard_content_for_test();
    let expected = file_path.to_string_lossy().into_owned();
    assert_eq!(
        clipboard, expected,
        "absolute copy should equal the on-disk path of the buffer"
    );
}

#[test]
fn copy_relative_file_path_falls_back_to_absolute_outside_workspace() {
    // The buffer's file lives outside the workspace, so the relative form has
    // no shorter representation — we verify the absolute path is used as a
    // safe fallback rather than failing or leaving the clipboard untouched.
    let outside = tempfile::tempdir().unwrap();
    let outside_file = outside.path().join("outside.txt");
    fs::write(&outside_file, "out\n").unwrap();

    let mut harness =
        EditorTestHarness::with_temp_project_and_config(120, 24, Default::default()).unwrap();
    harness.open_file(&outside_file).unwrap();
    harness.render().unwrap();

    run_command_palette(&mut harness, "Copy Relative File Path");

    let clipboard = harness.editor_mut().clipboard_content_for_test();
    let expected = outside_file.to_string_lossy().into_owned();
    assert_eq!(
        clipboard, expected,
        "relative copy of an out-of-workspace file should fall back to the absolute path"
    );
}

#[test]
fn copy_file_path_on_unsaved_buffer_reports_no_path() {
    let mut harness = EditorTestHarness::new(120, 24).unwrap();
    harness.render().unwrap();

    // Capture the unsaved-buffer clipboard before invoking the command so we
    // can prove the command did not overwrite it.
    let baseline = harness.editor_mut().clipboard_content_for_test();

    run_command_palette(&mut harness, "Copy File Path");

    harness.assert_screen_contains("Buffer has no file path");

    let clipboard = harness.editor_mut().clipboard_content_for_test();
    assert_eq!(
        clipboard, baseline,
        "no-path case must leave the clipboard untouched"
    );
}
