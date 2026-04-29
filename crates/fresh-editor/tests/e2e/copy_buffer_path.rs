//! E2E coverage for the "Copy File Path" / "Copy Relative File Path" commands
//! (issue #1752): user-visible status message + clipboard contents, and the
//! matching items on the tab right-click context menu.

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

// ── Tab context menu coverage ────────────────────────────────────────────────

/// Locate the active tab on screen so right-click events land on a real tab
/// rather than empty header space.
fn active_tab_position(harness: &EditorTestHarness) -> (u16, u16) {
    let active = harness.editor().active_buffer();
    for (_split_id, tab_layout) in harness.editor().get_tab_layouts() {
        for tab in &tab_layout.tabs {
            if tab.buffer_id() == Some(active) {
                let center_col = tab.tab_area.x + tab.tab_area.width / 2;
                return (center_col, tab.tab_area.y);
            }
        }
    }
    panic!("active tab not found in tab layouts");
}

#[test]
fn tab_right_click_menu_lists_copy_path_entries() {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(120, 30, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    let file_path = project_root.join("ctx.txt");
    fs::write(&file_path, "x\n").unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    let (col, row) = active_tab_position(&harness);
    harness.mouse_right_click(col, row).unwrap();
    harness.render().unwrap();

    // Both new entries are visible in the popup, alongside the existing
    // close-* entries. Asserting on rendered text matches CONTRIBUTING.md's
    // "observe, not inspect" rule.
    harness.assert_screen_contains("Copy Relative Path");
    harness.assert_screen_contains("Copy Full Path");
    harness.assert_screen_contains("Close");
}

#[test]
fn tab_right_click_copy_relative_path_copies_to_clipboard() {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(120, 30, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    let file_path = project_root.join("rel.txt");
    fs::write(&file_path, "x\n").unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    let (col, row) = active_tab_position(&harness);
    harness.mouse_right_click(col, row).unwrap();
    harness.render().unwrap();

    let (item_col, item_row) = harness
        .find_text_on_screen("Copy Relative Path")
        .expect("'Copy Relative Path' should be visible after tab right-click");
    harness.mouse_click(item_col, item_row).unwrap();
    harness.render().unwrap();

    let clipboard = harness.editor_mut().clipboard_content_for_test();
    assert_eq!(clipboard, "rel.txt");
    harness.assert_screen_contains("Copied path:");
}

#[test]
fn tab_right_click_copy_full_path_copies_absolute() {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(120, 30, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    let file_path = project_root.join("full.txt");
    fs::write(&file_path, "x\n").unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    let (col, row) = active_tab_position(&harness);
    harness.mouse_right_click(col, row).unwrap();
    harness.render().unwrap();

    let (item_col, item_row) = harness
        .find_text_on_screen("Copy Full Path")
        .expect("'Copy Full Path' should be visible after tab right-click");
    harness.mouse_click(item_col, item_row).unwrap();
    harness.render().unwrap();

    let clipboard = harness.editor_mut().clipboard_content_for_test();
    let expected = file_path.to_string_lossy().into_owned();
    assert_eq!(clipboard, expected);
}
