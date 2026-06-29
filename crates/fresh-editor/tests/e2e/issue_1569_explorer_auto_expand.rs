//! E2E tests for issue #1569: When opening file explorer (Ctrl+B) it should
//! automatically expand to show the current file.
//!
//! Repro (manual, reproduced in tmux):
//!   1. Make sure file explorer is closed.
//!   2. Open a file in a nested directory, e.g. `src/nested/deep.rs`.
//!   3. Press Ctrl+B to open the file explorer.
//!   4. Expected: the explorer auto-expands `src/` and `src/nested/`,
//!      revealing `deep.rs` (and ideally selecting it).
//!   5. Actual: the explorer only shows the root's immediate children
//!      (`src/` collapsed, `README.md`). The current file isn't revealed
//!      until the user toggles the explorer off-and-on again (at which
//!      point it *does* auto-expand correctly), confirming the state is
//!      simply missing on first-open.
//!
//! <https://github.com/sinelaw/fresh/issues/1569>

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

/// On first open, the file explorer must auto-expand directories to reveal
/// the currently open file.
#[test]
fn test_file_explorer_auto_expands_to_current_file_on_first_open() {
    // Build a nested project layout:
    //   project_root/
    //     src/
    //       nested/
    //         deep.rs      <-- current file
    //       shallow.rs
    //     README.md
    let mut harness = EditorTestHarness::with_temp_project(120, 30).unwrap();
    let project_root = harness.project_dir().unwrap();

    fs::create_dir_all(project_root.join("src/nested")).unwrap();
    let deep_path = project_root.join("src/nested/deep.rs");
    fs::write(&deep_path, "fn main() {}\n").unwrap();
    fs::write(project_root.join("src/shallow.rs"), "fn hello() {}\n").unwrap();
    fs::write(project_root.join("README.md"), "# Project\n").unwrap();

    // Open the nested file.
    harness.open_file(&deep_path).unwrap();
    harness.render().unwrap();

    // Sanity: the file explorer is not yet open.
    harness.assert_screen_not_contains("File Explorer");

    // Open file explorer (Ctrl+B toggles it visible in default keymap).
    harness
        .send_key(KeyCode::Char('b'), KeyModifiers::CONTROL)
        .unwrap();

    // Wait for the file explorer panel itself to render.
    harness
        .wait_until(|h| h.screen_to_string().contains("File Explorer"))
        .unwrap();

    // Grab only the explorer pane's text by reading the rows inside its
    // panel (the buffer/tabs area is off to the right). Each panel line
    // starts with `┌`/`└`/`│` and ends at the right border (`┐`, `┘`, or
    // a closing `│` followed by whitespace/tab content).
    let screen = harness.screen_to_string();
    let explorer_text: String = screen
        .lines()
        .filter(|line| line.starts_with('┌') || line.starts_with('└') || line.starts_with('│'))
        .map(|line| {
            // Find the first "right border" character after the initial
            // border char — this is the end of the explorer panel.
            let mut it = line.char_indices();
            let _left = it.next(); // skip leading border
            let end = it
                .find(|(_, c)| matches!(c, '┐' | '┘' | '│'))
                .map(|(i, c)| i + c.len_utf8())
                .unwrap_or(line.len());
            line[..end].to_string()
        })
        .collect::<Vec<_>>()
        .join("\n");

    // Sanity: the explorer panel actually rendered.
    assert!(
        explorer_text.contains("File Explorer"),
        "Expected the explorer panel to render. Full screen:\n{}",
        screen
    );

    // Expected: `deep.rs` appears inside the explorer panel because the
    // directories leading to it were auto-expanded.
    //
    // Actual (bug): `src` is collapsed, so `deep.rs` and `nested` are
    // nowhere in the panel's text.
    assert!(
        explorer_text.contains("deep.rs"),
        "Issue #1569: On first open, the file explorer should auto-expand \
         directories to reveal the currently open file (deep.rs). \
         Explorer panel text:\n{}\n\nFull screen:\n{}",
        explorer_text,
        screen
    );
    assert!(
        explorer_text.contains("nested"),
        "Intermediate directory `nested` should be visible in the \
         explorer tree (meaning `src` was auto-expanded). \
         Explorer panel text:\n{}\n\nFull screen:\n{}",
        explorer_text,
        screen
    );
}
