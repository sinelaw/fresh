//! How many wheel reports a child program in the integrated terminal gets
//! per physical wheel notch.
//!
//! Smooth scrolling (on by default) splits a notch into the line that lands
//! with the event and lines owed to later frames, which it replays through
//! the same dispatch. For a child that takes the wheel itself, those replays
//! used to be forwarded again: every notch reached the child more than once.
//!
//! Uses the byte dumper from `terminal_child_keys`; the harness is wide
//! enough that all reports stay on one screen row, so they can be counted.

use super::terminal_child_keys::{dumper_config_with_setup, pty_available, send_sentinel};
use crate::common::harness::EditorTestHarness;

/// SGR wheel-down report prefix, `ESC [ < 65 ;`.
const SGR_WHEEL_DOWN: &str = " 1b 5b 3c 36 35 3b";
/// Cursor Down, `ESC [ B` — what alternate-scroll mode turns a notch into.
const CURSOR_DOWN: &str = " 1b 5b 42";

/// Open a dumper whose child first writes `setup`, scroll one notch down
/// over it, let the smooth-scroll walk finish, and return the screen after
/// the sentinel has arrived.
fn one_notch_over_dumper(setup: &str) -> Option<String> {
    if !pty_available() {
        eprintln!("Skipping: PTY not available in this environment");
        return None;
    }
    let config = dumper_config_with_setup(setup);
    assert!(
        config.editor.smooth_scroll && config.editor.animations,
        "the bug needs the default smooth-scroll walk"
    );
    let mut harness = EditorTestHarness::with_temp_project_and_config(240, 30, config).unwrap();
    harness.editor_mut().open_terminal();
    harness.wait_for_screen_contains("READY").unwrap();

    harness.mouse_scroll_down(20, 10).unwrap();
    // The walk replays its owed lines on later frames; wait until it has
    // nothing left to replay, so a duplicate report would already be out.
    harness
        .wait_until(|h| !h.editor().has_pending_wheel_scroll())
        .unwrap();
    send_sentinel(&mut harness);
    Some(harness.screen_to_string())
}

/// A child with SGR mouse tracking gets exactly one wheel report per notch.
#[test]
#[cfg(unix)]
fn one_wheel_notch_reaches_a_mouse_tracking_child_once() {
    let Some(screen) = one_notch_over_dumper("\\033[?1000h\\033[?1006h") else {
        return;
    };
    assert_eq!(
        screen.matches(SGR_WHEEL_DOWN).count(),
        1,
        "one notch must be one wheel report.\nScreen:\n{screen}"
    );
}

/// An alternate-screen child without mouse tracking gets one notch's worth
/// of arrow keys (three Cursor Downs), not one set per replayed line.
#[test]
#[cfg(unix)]
fn one_wheel_notch_reaches_an_alt_screen_child_as_one_set_of_arrows() {
    let Some(screen) = one_notch_over_dumper("\\033[?1049h") else {
        return;
    };
    assert_eq!(
        screen.matches(CURSOR_DOWN).count(),
        3,
        "one notch must be three Cursor Down keys.\nScreen:\n{screen}"
    );
}
