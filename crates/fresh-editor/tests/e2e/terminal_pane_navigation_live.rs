//! Regression test: `NextPane`/`PrevPane` must treat a target terminal
//! buffer exactly like `next_split`/`prev_split` and `next_buffer`/
//! `prev_buffer` do — re-deriving terminal mode for the newly focused
//! pane.
//!
//! Before the fix `cycle_pane` navigated to the target (split, tab) pair
//! but never re-synced terminal mode. The non-explicit `ExitTerminalMode`
//! deferred on the way out of a terminal is a no-op that assumes "the
//! upcoming focus change re-derives the key context" — true for the split
//! commands, but `cycle_pane` never did it. As a result:
//!
//! 1. Jumping ONTO a terminal via NextPane left the key context in Normal,
//!    so keystrokes never reached the PTY (no terminal mode).
//! 2. Jumping AWAY from a terminal could leave a stale Terminal context.
//!
//! In both cases the visible terminal pane must also keep rendering live
//! PTY output.

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
/// so it works even after we've moved focus to a non-terminal pane).
fn write_to_terminal(
    harness: &EditorTestHarness,
    terminal_buffer: fresh::model::event::BufferId,
    bytes: &[u8],
) {
    let terminal_id = harness
        .editor()
        .active_window()
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
/// buffer id.
fn setup_split_with_terminal(harness: &mut EditorTestHarness) -> fresh::model::event::BufferId {
    harness.editor_mut().split_pane_vertical();
    harness.render().unwrap();

    harness.editor_mut().next_split();
    harness.editor_mut().open_terminal();
    harness.render().unwrap();

    let terminal_buffer = harness.editor().active_buffer_id();
    assert!(harness
        .editor()
        .active_window()
        .is_terminal_buffer(terminal_buffer));
    assert!(harness.editor().is_terminal_mode());

    // Wait for the shell prompt to settle so later writes go to a live shell.
    write_to_terminal(harness, terminal_buffer, b"echo FRESH_READY\n");
    harness
        .wait_until(|h| h.screen_to_string().contains("FRESH_READY"))
        .unwrap();

    terminal_buffer
}

/// Across a full `next_pane` cycle the terminal-mode flag must track the
/// focused buffer: ON exactly when the active buffer is the terminal, OFF
/// otherwise — the same invariant `next_split`/`prev_split` and
/// `next_buffer`/`prev_buffer` maintain. This exercises both directions of
/// the bug at once: landing on the terminal must restore terminal mode, and
/// landing off it must clear it. The exact number of panes is an
/// implementation detail, so we walk enough steps to wrap around fully.
#[test]
fn next_pane_syncs_terminal_mode_to_focused_buffer() {
    let mut harness = match harness_or_skip(120, 30) {
        Some(h) => h,
        None => return,
    };
    let _terminal_buffer = setup_split_with_terminal(&mut harness);

    // We start on the terminal (terminal mode on). Walk a generous number
    // of panes so we wrap past every pane at least once, checking the
    // invariant at each stop.
    let mut saw_terminal_focus = false;
    let mut saw_non_terminal_focus = false;
    for step in 0..8 {
        harness.editor_mut().next_pane();
        harness.render().unwrap();

        let active = harness.editor().active_buffer_id();
        let is_terminal = harness.editor().active_window().is_terminal_buffer(active);
        if is_terminal {
            saw_terminal_focus = true;
            assert!(
                harness.editor().is_terminal_mode(),
                "step {step}: focused the terminal via next_pane but terminal \
                 mode was off — keystrokes would not reach the PTY"
            );
        } else {
            saw_non_terminal_focus = true;
            assert!(
                !harness.editor().is_terminal_mode(),
                "step {step}: focused a non-terminal buffer via next_pane but \
                 terminal mode was still on"
            );
        }
    }

    assert!(
        saw_terminal_focus && saw_non_terminal_focus,
        "the cycle should have visited both the terminal and a non-terminal pane"
    );
}

/// The visible terminal pane must keep rendering live PTY output after
/// `next_pane` moves focus away from it.
#[test]
fn terminal_keeps_updating_after_next_pane_away() {
    let mut harness = match harness_or_skip(120, 30) {
        Some(h) => h,
        None => return,
    };
    let terminal_buffer = setup_split_with_terminal(&mut harness);

    // Cycle until focus leaves the terminal.
    for _ in 0..8 {
        harness.editor_mut().next_pane();
        harness.render().unwrap();
        let active = harness.editor().active_buffer_id();
        if !harness.editor().active_window().is_terminal_buffer(active) {
            break;
        }
    }
    assert!(
        !harness
            .editor()
            .active_window()
            .is_terminal_buffer(harness.editor().active_buffer_id()),
        "next_pane should have reached a non-terminal pane"
    );

    write_to_terminal(&harness, terminal_buffer, b"echo MARKER_AFTER_NEXT_PANE\n");
    harness
        .wait_until(|h| h.screen_to_string().contains("MARKER_AFTER_NEXT_PANE"))
        .unwrap();
}
