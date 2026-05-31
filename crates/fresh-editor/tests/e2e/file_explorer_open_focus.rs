//! Tests for opening files while the file explorer is focused.
//!
//! Two related behaviours, both exercised from the FileExplorer key context:
//!  1. Ctrl+O must open the "Open file:" prompt, even though the binding's
//!     default is registered only for the Normal context.
//!  2. Opening a file via Quick Open (Ctrl+P, Backspace, type filename, Enter)
//!     must transfer focus to the new buffer — typing afterwards goes into the
//!     editor, not into the file explorer's search filter.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

/// Ctrl+O should open the "Open file:" prompt when the file explorer is focused.
#[test]
fn test_ctrl_o_opens_prompt_when_file_explorer_focused() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let project_root = harness.project_dir().unwrap();
    fs::write(project_root.join("a.txt"), "hello").unwrap();

    // Open and focus the file explorer.
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("a.txt").unwrap();

    // Confirm the explorer is focused by checking the rendered status line.
    harness
        .wait_until(|h| h.screen_to_string().contains("File explorer"))
        .unwrap();

    // Send Ctrl+O. With the explorer focused, this should still trigger the
    // Open File prompt — the binding currently exists only for the Normal
    // context, which is the bug under test.
    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();

    // The "Open file:" prompt text should appear on screen.
    harness
        .wait_until(|h| h.screen_to_string().contains("Open file:"))
        .expect("Ctrl+O should open the file-open prompt when explorer is focused");
}

/// After opening a file via Quick Open (file mode) while the explorer is
/// focused, typing should go into the opened buffer — not into the file
/// explorer's search filter.
#[test]
fn test_quick_open_focuses_editor_when_file_explorer_focused() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let project_root = harness.project_dir().unwrap();
    fs::write(project_root.join("target.txt"), "world").unwrap();

    // Focus the file explorer.
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("target.txt").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("File explorer"))
        .unwrap();

    // Open Quick Open (command mode by default), then backspace to switch to
    // file mode — this matches the exact user flow from the bug report.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("target.txt").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for the buffer to open (its content appears on screen).
    harness
        .wait_until(|h| h.screen_to_string().contains("world"))
        .unwrap();

    // Type into what should now be the focused editor. If focus is still on
    // the file explorer, the keys would feed its search filter instead.
    harness.type_text("X").unwrap();

    // The opened buffer should now contain the inserted character. Asserting
    // on the rendered screen (per CONTRIBUTING.md) — we expect "Xworld" to
    // appear in the editor pane, not the original "world".
    harness
        .wait_until(|h| h.screen_to_string().contains("Xworld"))
        .expect(
            "Typing after Quick Open should land in the opened buffer, not the explorer filter",
        );
}
