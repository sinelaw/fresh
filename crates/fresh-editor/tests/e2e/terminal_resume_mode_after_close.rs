//! Regression test for issue #2485: a terminal that becomes the active
//! split after a *second* terminal split is closed must keep the mode it
//! had when it was defocused.
//!
//! Before the fix, closing the second terminal snapped focus back to the
//! first terminal through the low-level split-collapse path, which never
//! re-synced terminal mode — so a terminal that was actively capturing
//! keyboard was left in read-only scrollback, and the user had to press
//! `Ctrl+Space` to resume.
//!
//! The bug is masked for a terminal that keeps producing output, because
//! `jump_to_end_on_output` re-enters terminal mode on the next byte. These
//! tests disable that setting so they assert the focus-restore logic itself.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use fresh::model::event::SplitDirection;
use portable_pty::{native_pty_system, PtySize};

fn config_no_jump_to_end() -> Config {
    let mut config = Config::default();
    // Isolate the focus-restore behaviour from the output-driven auto-resume
    // that would otherwise paper over the bug.
    config.terminal.jump_to_end_on_output = false;
    config
}

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
    EditorTestHarness::with_config(width, height, config_no_jump_to_end()).ok()
}

/// Run a command through the command palette (mirrors the user driving the
/// "Close Buffer" / "Exit Terminal Mode" commands).
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

/// First terminal is active (capturing keyboard). Opening a second terminal
/// split and then closing it must return focus to the first terminal *in
/// terminal mode* — it was active when it lost focus.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses a PTY shell
fn first_terminal_resumes_terminal_mode_after_closing_second_split() {
    let mut harness = match harness_or_skip(120, 30) {
        Some(h) => h,
        None => return,
    };

    // First terminal — becomes the active buffer in terminal mode.
    harness.editor_mut().open_terminal();
    harness.render().unwrap();
    let first = harness.editor().active_buffer_id();
    assert!(harness.editor().active_window().is_terminal_buffer(first));
    assert!(
        harness.editor().is_terminal_mode(),
        "first terminal should start in terminal mode"
    );

    // Second terminal in a new split — becomes active.
    harness
        .editor_mut()
        .open_terminal_split(SplitDirection::Vertical);
    harness.render().unwrap();
    let second = harness.editor().active_buffer_id();
    assert_ne!(first, second, "second terminal should be a distinct buffer");
    assert!(harness.editor().is_terminal_mode());

    // Close the second terminal's buffer; focus snaps back to the first.
    run_command(&mut harness, "Close Buffer");

    assert_eq!(
        harness.editor().active_buffer_id(),
        first,
        "closing the second terminal should focus the first terminal"
    );
    assert!(
        harness.editor().is_terminal_mode(),
        "the first terminal was active when defocused, so it must resume \
         terminal mode when it becomes the last remaining split"
    );
}

/// If the first terminal was left in read-only scrollback when it lost
/// focus, closing the second terminal must *not* silently re-activate it —
/// it should stay in scrollback (the mode it had when defocused).
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses a PTY shell
fn first_terminal_stays_in_scrollback_after_closing_second_split() {
    let mut harness = match harness_or_skip(120, 30) {
        Some(h) => h,
        None => return,
    };

    harness.editor_mut().open_terminal();
    harness.render().unwrap();
    let first = harness.editor().active_buffer_id();
    assert!(harness.editor().is_terminal_mode());

    // User explicitly leaves terminal mode → read-only scrollback.
    run_command(&mut harness, "Exit Terminal Mode");
    assert!(
        !harness.editor().is_terminal_mode(),
        "explicit exit should drop the first terminal to read-only"
    );

    // Open and then close a second terminal split.
    harness
        .editor_mut()
        .open_terminal_split(SplitDirection::Vertical);
    harness.render().unwrap();
    let second = harness.editor().active_buffer_id();
    assert_ne!(first, second);

    run_command(&mut harness, "Close Buffer");

    assert_eq!(
        harness.editor().active_buffer_id(),
        first,
        "closing the second terminal should focus the first terminal"
    );
    assert!(
        !harness.editor().is_terminal_mode(),
        "the first terminal was in scrollback when defocused, so it must \
         stay read-only rather than being re-activated"
    );
}
