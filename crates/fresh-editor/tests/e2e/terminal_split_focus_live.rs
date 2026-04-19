//! Regression test: moving focus away from a focused terminal split —
//! whether via the `next_split` keyboard command or by mouse-clicking
//! another split — should always do two things:
//!
//! 1. Stop routing keyboard input to the terminal (terminal mode off).
//! 2. Keep rendering the terminal's live PTY output in its visible pane.
//!
//! Before the fix the mouse-click path did both, but the keyboard path
//! asymmetrically left `terminal_mode` on, so the cursor in the newly
//! focused buffer stayed hidden until the first keystroke self-corrected
//! the state.

use crate::common::harness::EditorTestHarness;
use portable_pty::{native_pty_system, PtySize};

fn harness_or_skip(width: u16, height: u16) -> Option<EditorTestHarness> {
    if native_pty_system()
        .openpty(PtySize {
            rows: 1,
            cols: 1,
            pixel_width: 0,
            pixel_height: 0,
        })
        .is_err()
    {
        eprintln!("Skipping terminal test: PTY not available in this environment");
        return None;
    }
    EditorTestHarness::new(width, height).ok()
}

/// Write directly to a terminal by id (bypasses `active_buffer()` routing,
/// so it works even after we've moved focus to a non-terminal split).
fn write_to_terminal(
    harness: &EditorTestHarness,
    terminal_buffer: fresh::model::event::BufferId,
    bytes: &[u8],
) {
    let terminal_id = harness
        .editor()
        .get_terminal_id(terminal_buffer)
        .expect("terminal id");
    let handle = harness
        .editor()
        .terminal_manager()
        .get(terminal_id)
        .expect("terminal handle");
    handle.write(bytes);
}

/// Set up a vertical split with a non-terminal buffer in one pane and a
/// live terminal (in terminal mode) in the other. Returns the terminal's
/// buffer id. The concrete layout (which pane is left/right) is an
/// implementation detail we don't rely on; callers that need to click the
/// non-terminal pane do so by finding coordinates outside the terminal.
fn setup_split_with_terminal(harness: &mut EditorTestHarness) -> fresh::model::event::BufferId {
    // Split vertically while on the default [No Name] buffer so both
    // splits show the same non-terminal buffer.
    harness.editor_mut().split_pane_vertical();
    harness.render().unwrap();

    // Open a terminal — it becomes the active buffer in whichever split is
    // active. The other split continues to show [No Name].
    harness.editor_mut().next_split();
    harness.editor_mut().open_terminal();
    harness.render().unwrap();

    let terminal_buffer = harness.editor().active_buffer_id();
    assert!(harness.editor().is_terminal_buffer(terminal_buffer));
    assert!(harness.editor().is_terminal_mode());

    // Wait for the shell prompt to settle so later writes go to a live shell.
    write_to_terminal(harness, terminal_buffer, b"echo FRESH_READY\n");
    harness
        .wait_until(|h| h.screen_to_string().contains("FRESH_READY"))
        .unwrap();

    terminal_buffer
}

/// Keyboard "next split" away from the terminal: after moving focus off
/// the terminal, keyboard input should no longer be captured by it
/// (terminal mode off), and the visible terminal pane must keep
/// rendering new PTY output.
#[test]
fn terminal_keeps_updating_after_next_split() {
    let mut harness = match harness_or_skip(120, 30) {
        Some(h) => h,
        None => return,
    };
    let terminal_buffer = setup_split_with_terminal(&mut harness);

    // Move focus away from the terminal using the keyboard command.
    harness.editor_mut().next_split();
    harness.render().unwrap();

    // The active buffer should now be the non-terminal buffer on the other
    // split; confirm that as a sanity check.
    assert!(
        !harness
            .editor()
            .is_terminal_buffer(harness.editor().active_buffer_id()),
        "next_split should have moved focus to the non-terminal split"
    );

    // Terminal mode should be off — keyboard goes to the focused text
    // buffer, not the terminal.
    assert!(
        !harness.editor().is_terminal_mode(),
        "switching to a non-terminal split via next_split should stop \
         capturing keyboard input for the terminal"
    );

    // And the terminal pane must keep reflecting new PTY output.
    write_to_terminal(&harness, terminal_buffer, b"echo MARKER_AFTER_NEXT_SPLIT\n");
    harness
        .wait_until(|h| h.screen_to_string().contains("MARKER_AFTER_NEXT_SPLIT"))
        .unwrap();
}

/// Mouse click away from the terminal onto the other split: same
/// expectation — the terminal must keep rendering new output, and we
/// also want terminal mode to stop capturing keyboard input.
#[test]
fn terminal_keeps_updating_after_mouse_click_away() {
    let mut harness = match harness_or_skip(120, 30) {
        Some(h) => h,
        None => return,
    };
    let terminal_buffer = setup_split_with_terminal(&mut harness);

    // Click inside the non-terminal split's content area. Row 5 is safely
    // below the menu and tab rows; column 90 lands in the right half of
    // the 120-column screen, which is the non-terminal pane for the
    // layout produced by `setup_split_with_terminal`.
    harness.mouse_click(90, 5).unwrap();
    harness.render().unwrap();

    // Sanity check: the click actually moved focus off the terminal.
    assert!(
        !harness
            .editor()
            .is_terminal_buffer(harness.editor().active_buffer_id()),
        "click should have moved focus to the non-terminal split"
    );

    // After the click, keyboard should no longer be captured by the terminal.
    assert!(
        !harness.editor().is_terminal_mode(),
        "mouse-clicking another split should exit terminal mode (stop capturing keys)"
    );

    // Critical: the visible terminal pane must keep reflecting new PTY output
    // even though the terminal is no longer focused.
    write_to_terminal(
        &harness,
        terminal_buffer,
        b"echo MARKER_AFTER_MOUSE_CLICK\n",
    );
    harness
        .wait_until(|h| h.screen_to_string().contains("MARKER_AFTER_MOUSE_CLICK"))
        .unwrap();
}
