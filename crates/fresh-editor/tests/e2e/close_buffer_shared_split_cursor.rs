//! Regression test: closing a buffer that is *also* open in another split must
//! leave the surviving tab's cursor usable.
//!
//! When the same buffer is shown in two splits and you close it from a split
//! that has another tab to fall back to, the close took the "remove this tab,
//! there are other viewports" path. That path activated the replacement tab by
//! moving only the split *tree* (`set_split_buffer`) and never the matching
//! `SplitViewState.active_buffer`, so the view-state stayed stranded on the
//! just-closed buffer. The cursor and render then read that stale view-state
//! (anchored at offset 0) while typed text went into the tree's actual buffer:
//! every keystroke inserted at the start of the buffer, reversed (`1234` →
//! `4321`), with the cursor frozen.
//!
//! The test drives the flow through the command palette and asserts only on
//! rendered output: typed characters must append at the cursor in the
//! surviving buffer, not pile up reversed at its top.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

fn run_command(harness: &mut EditorTestHarness, command: &str) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text(command).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

#[test]
fn typing_after_closing_a_split_shared_buffer_targets_the_surviving_buffer() {
    let mut harness = EditorTestHarness::new(120, 30).unwrap();
    harness.load_buffer_from_text("AAAA\n").unwrap();
    harness.render().unwrap();

    // Duplicate the buffer across two vertical splits (both show it).
    run_command(&mut harness, "Split Vertical");

    // Open a new buffer in the active split and type three lines into it,
    // leaving the cursor at the end of "line three".
    run_command(&mut harness, "New File");
    harness.type_text("line one").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("line two").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("line three").unwrap();
    harness.render().unwrap();

    // Switch back to the duplicated buffer's tab in this split, then close it.
    // The split still has the new-file tab to fall back to, and the buffer is
    // still open in the other split — this is the multi-viewport close path.
    run_command(&mut harness, "Previous Buffer");
    run_command(&mut harness, "Close Buffer");

    // Focus is on the surviving new-file buffer with its cursor preserved.
    // Typing must append to "line three", not insert reversed at offset 0.
    harness.type_text("1234").unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("line three1234");
    harness.assert_screen_not_contains("4321");
}
