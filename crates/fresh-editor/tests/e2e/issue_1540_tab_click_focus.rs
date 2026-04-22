//! Regression test for issue #1540:
//!
//! After clicking inside the file explorer, clicking a file tab in the
//! tab bar should move keyboard focus to that tab's buffer. Previously
//! the click switched the visible buffer but left the key context in
//! `FileExplorer`, so subsequent keystrokes (typing, Ctrl+F, …) were
//! still routed to the file explorer instead of the editor.
//!
//! Reproduction (from the issue):
//!   1. open fresh on a folder
//!   2. click one or more files in the file navigator
//!   3. click on one of the file tabs
//!   4. try to type / press Ctrl+F
//! Without the fix, typing is absorbed by the file-explorer search
//! buffer and Ctrl+F does nothing useful.
//!
//! Observability: key context is checked via the public accessor, and
//! the effect on typing is observed through the rendered screen, per
//! CONTRIBUTING §Testing.
use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;
use std::time::Duration;

const TAB_BAR_ROW: u16 = 1;
const EXPLORER_CLICK_COL: u16 = 10;

fn explorer_row_for(harness: &EditorTestHarness, name: &str) -> u16 {
    let screen = harness.screen_to_string();
    const FIRST_EXPLORER_ROW: usize = 2;
    for (row, line) in screen.lines().enumerate().skip(FIRST_EXPLORER_ROW) {
        let prefix: String = line.chars().take(40).collect();
        if prefix.contains(name) {
            return row as u16;
        }
    }
    panic!("file {name} not found in file explorer;\nscreen:\n{screen}");
}

fn advance_past_double_click(harness: &mut EditorTestHarness) {
    let dc_window = Duration::from_millis(
        harness
            .config()
            .editor
            .double_click_time_ms
            .saturating_mul(2),
    );
    harness.advance_time(dc_window);
}

/// Single-click a file in the explorer. Opens it as a preview and keeps
/// key focus in the file explorer — that is the precondition for the
/// bug under test.
fn single_click_file(harness: &mut EditorTestHarness, name: &str) {
    advance_past_double_click(harness);
    let row = explorer_row_for(harness, name);
    harness.mouse_click(EXPLORER_CLICK_COL, row).unwrap();
}

#[test]
fn clicking_tab_after_explorer_click_moves_focus_to_editor() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let project = harness.project_dir().unwrap();
    fs::write(project.join("alpha.txt"), "alpha\n").unwrap();

    // Open explorer and wait for the file to appear.
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("alpha.txt").unwrap();

    // Single-click opens the file as a preview tab and leaves focus in
    // the file explorer.
    single_click_file(&mut harness, "alpha.txt");
    assert_eq!(
        harness.editor().get_key_context(),
        fresh::input::keybindings::KeyContext::FileExplorer,
        "precondition: single-click in explorer should leave focus in FileExplorer"
    );

    // Find alpha.txt's tab column and click it. The tab bar row contains
    // box-drawing glyphs for the explorer border, so byte offsets in the
    // row string don't equal visual columns — count chars instead.
    let tab_row_text = harness.screen_row_text(TAB_BAR_ROW);
    let needle: Vec<char> = "alpha.txt".chars().collect();
    let tab_chars: Vec<char> = tab_row_text.chars().collect();
    let tab_col = tab_chars
        .windows(needle.len())
        .position(|w| w == needle.as_slice())
        .unwrap_or_else(|| panic!("alpha.txt missing from tab row: {tab_row_text:?}"))
        as u16;

    advance_past_double_click(&mut harness);
    harness.mouse_click(tab_col, TAB_BAR_ROW).unwrap();

    // Focus must move to the editor — this is the core fix for #1540.
    assert_eq!(
        harness.editor().get_key_context(),
        fresh::input::keybindings::KeyContext::Normal,
        "clicking a tab must move focus from the explorer to the editor"
    );

    // End-to-end confirmation: typing after the tab click reaches the
    // editor buffer, not the explorer.
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.type_text("INSERTED").unwrap();
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("alphaINSERTED"),
        "typing after a tab click must reach the alpha.txt buffer; screen:\n{screen}"
    );
}
