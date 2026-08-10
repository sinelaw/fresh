//! Issue #2878: two splits on the same file must keep independent edit points.
//!
//! Editing in one split used to teleport every other split showing the same
//! buffer to the edit position. Plain typing was fine — it emits a single
//! `Insert` — but any action that emits more than one event (transpose,
//! move-line, toggle-comment, …) takes the bulk-edit path, which *assigned*
//! the editing split's cursor position to all the others. The reporter hit it
//! with Ctrl+T and had to reposition the second pane's edit point after every
//! such edit.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// 200 numbered lines, wide enough to tell any two apart on screen.
fn numbered_lines() -> String {
    (1..=200)
        .map(|i| format!("line {:03}: the quick brown fox\n", i))
        .collect()
}

/// Run a command palette entry by name.
fn run_command(harness: &mut EditorTestHarness, query: &str) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text(query).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// Ctrl+T in the first split must leave the second split parked where the
/// user put it, rather than dragging it to the edited line.
#[test]
fn test_transpose_in_one_split_leaves_other_split_in_place() {
    let mut harness = EditorTestHarness::new(160, 40).unwrap();
    harness.load_buffer_from_text(&numbered_lines()).unwrap();

    // Split; focus lands in the new (second) split, cursor at the top.
    run_command(&mut harness, "split vert");

    // Park the second split's edit point far down the file.
    harness
        .send_key_repeat(KeyCode::Down, KeyModifiers::NONE, 150)
        .unwrap();
    harness.assert_screen_contains("line 151");

    // Back to the first split, and put its cursor between two characters
    // so transpose has something to swap.
    run_command(&mut harness, "prev split");
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('t'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // The edit landed in the first split: "line 001" -> "lnie 001".
    harness.assert_screen_contains("lnie 001");

    // Return to the second split and nudge its cursor — that is when a
    // cursor moved out from under the user reveals itself, by scrolling
    // the pane to wherever it was moved to.
    run_command(&mut harness, "next split");
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // The second split is still showing line 151. Before the fix its cursor
    // had been reset to the transpose position, so this pane jumped to the
    // top of the file and line 151 was nowhere on screen.
    harness.assert_screen_contains("line 151");
}

/// The same guarantee for Alt+Down (move line down) — transpose is only one
/// of the multi-event editing actions that took the bulk path.
#[test]
fn test_move_line_in_one_split_leaves_other_split_in_place() {
    let mut harness = EditorTestHarness::new(160, 40).unwrap();
    harness.load_buffer_from_text(&numbered_lines()).unwrap();

    run_command(&mut harness, "split vert");
    harness
        .send_key_repeat(KeyCode::Down, KeyModifiers::NONE, 150)
        .unwrap();
    harness.assert_screen_contains("line 151");

    run_command(&mut harness, "prev split");
    harness.send_key(KeyCode::Down, KeyModifiers::ALT).unwrap();
    harness.render().unwrap();

    run_command(&mut harness, "next split");
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("line 151");
}
