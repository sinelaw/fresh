//! E2E coverage for the "Copy File Path" / "Copy Relative File Path" commands
//! (issue #1752): the user-visible status message confirms what was copied,
//! plus the matching items on the tab right-click context menu.
//!
//! These tests follow CONTRIBUTING.md's "observe, not inspect" rule: they
//! assert only on rendered output. The status bar surfaces the copied path
//! via "Copied path: <path>", which is the same text that lands in the
//! clipboard — so observing the status line is equivalent to inspecting
//! the clipboard, but free from system-clipboard / tempdir-canonicalization
//! flakiness in parallel CI runs.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;
use std::path::MAIN_SEPARATOR;

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

/// Wide harness so the status bar isn't clipped at the right edge — paths
/// rendered alongside `Copied path:` can run long, and a clipped line would
/// hide the very evidence the test needs to observe.
fn wide_temp_project_harness() -> EditorTestHarness {
    EditorTestHarness::with_temp_project_and_config(220, 30, Default::default()).unwrap()
}

#[test]
fn copy_relative_file_path_via_command_palette() {
    let mut harness = wide_temp_project_harness();
    let project_root = harness.project_dir().unwrap();

    let file_path = project_root.join("hello.txt");
    fs::write(&file_path, "hi\n").unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    run_command_palette(&mut harness, "Copy Relative File Path");

    // Relative form drops the workspace prefix entirely — only the basename
    // remains, so the rendered status line is exactly "Copied path: hello.txt".
    harness.assert_screen_contains("Copied path: hello.txt");
}

#[test]
fn copy_file_path_via_command_palette_uses_absolute_path() {
    let mut harness = wide_temp_project_harness();
    let project_root = harness.project_dir().unwrap();

    let file_path = project_root.join("absolute.txt");
    fs::write(&file_path, "x\n").unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    run_command_palette(&mut harness, "Copy File Path");

    // The absolute form must include the workspace-root parent segment
    // ("project_root" — created by the harness as a fixed-name subdir of the
    // temp dir). The relative form would render as just "absolute.txt", so
    // observing the workspace-root segment proves the absolute branch ran.
    let screen = harness.screen_to_string();
    let expected_segment = format!("project_root{}absolute.txt", MAIN_SEPARATOR);
    assert!(
        screen.contains("Copied path:") && screen.contains(&expected_segment),
        "expected status bar to render the absolute path containing {expected_segment:?}, \
         got screen:\n{screen}"
    );
}

#[test]
fn copy_relative_file_path_falls_back_to_absolute_outside_workspace() {
    // When the buffer's file lives outside the workspace, strip_prefix fails
    // and the implementation falls back to the absolute path. We use a
    // fixed-name marker subdirectory under the OS temp root so we can match
    // it in the rendered status bar without depending on the platform-
    // specific tempdir prefix (e.g. "/tmp" vs "/private/var/folders/...").
    let outside = tempfile::tempdir().unwrap();
    let outside_dir = outside.path().join("outside_marker");
    fs::create_dir(&outside_dir).unwrap();
    let outside_file = outside_dir.join("outside.txt");
    fs::write(&outside_file, "out\n").unwrap();

    let mut harness = wide_temp_project_harness();
    harness.open_file(&outside_file).unwrap();
    harness.render().unwrap();

    run_command_palette(&mut harness, "Copy Relative File Path");

    let screen = harness.screen_to_string();
    let expected_segment = format!("outside_marker{}outside.txt", MAIN_SEPARATOR);
    assert!(
        screen.contains("Copied path:") && screen.contains(&expected_segment),
        "expected status bar to render the absolute fallback path containing \
         {expected_segment:?}, got screen:\n{screen}"
    );
}

#[test]
fn copy_file_path_on_unsaved_buffer_reports_no_path() {
    let mut harness = EditorTestHarness::new(120, 24).unwrap();
    harness.render().unwrap();

    run_command_palette(&mut harness, "Copy File Path");

    // The user-visible signal is the status bar — that's all we assert on.
    // Whether the clipboard was touched is an implementation detail covered
    // by the unit tests on Clipboard / Editor::copy_buffer_path.
    harness.assert_screen_contains("Buffer has no file path");
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
    let mut harness = wide_temp_project_harness();
    let project_root = harness.project_dir().unwrap();

    let file_path = project_root.join("ctx.txt");
    fs::write(&file_path, "x\n").unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    let (col, row) = active_tab_position(&harness);
    harness.mouse_right_click(col, row).unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("Copy Relative Path");
    harness.assert_screen_contains("Copy Full Path");
    harness.assert_screen_contains("Close");
}

#[test]
fn tab_right_click_copy_relative_path_copies_to_clipboard() {
    let mut harness = wide_temp_project_harness();
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

    harness.assert_screen_contains("Copied path: rel.txt");
}

#[test]
fn tab_right_click_copy_full_path_copies_absolute() {
    let mut harness = wide_temp_project_harness();
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

    let screen = harness.screen_to_string();
    let expected_segment = format!("project_root{}full.txt", MAIN_SEPARATOR);
    assert!(
        screen.contains("Copied path:") && screen.contains(&expected_segment),
        "expected status bar to render the absolute path containing {expected_segment:?}, \
         got screen:\n{screen}"
    );
}
