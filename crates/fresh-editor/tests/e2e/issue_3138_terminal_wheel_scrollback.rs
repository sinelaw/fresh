//! The mouse wheel in a terminal whose program asked for neither the mouse
//! nor the alternate screen scrolls Fresh's own scrollback — and that view
//! has to behave like a terminal emulator's scrolled-up view (#3138).
//!
//! That is what an agent CLI streaming at the normal screen looks like (devin;
//! codex before 0.157.1), and it was unusable both ways:
//!
//! * `terminal.jump_to_end_on_output: true` (default) — the next byte of
//!   output yanked a wheel-scrolled view back to the live grid, so history
//!   could not be read while the program kept printing.
//! * `false` — the view held, but it was a frozen snapshot with no way back:
//!   scrolling down stopped at the snapshot's end while the program's output
//!   went on underneath, until Ctrl+Space.
//!
//! Now a view the user scrolled up holds its place against output, and
//! scrolling back down to its end resumes following the live grid. A
//! wheel-down on a live terminal leaves it live.
//!
//! Output lines are produced by shell arithmetic (`echo NEW_$((1+1))` prints
//! `NEW_2`) so the echoed command line never matches what the test looks
//! for. Waits are semantic; the terminal grid is read only to know the
//! child's output has arrived while the view (deliberately) doesn't show it.
#![cfg(not(windows))]

use crate::common::global_state::pin_posix_shell;
use crate::common::harness::EditorTestHarness;
use portable_pty::{native_pty_system, PtySize};

const W: u16 = 80;
const H: u16 = 24;
/// A point inside the terminal's content area.
const WHEEL_AT: (u16, u16) = (40, 12);
/// The first content row (below the menu bar and the tab bar).
const TOP_ROW: usize = 2;

fn harness_or_skip() -> Option<EditorTestHarness> {
    let pty = native_pty_system().openpty(PtySize {
        rows: 1,
        cols: 1,
        pixel_width: 0,
        pixel_height: 0,
    });
    if pty.is_err() {
        eprintln!("Skipping terminal test: PTY not available in this environment");
        return None;
    }
    EditorTestHarness::new(W, H).ok()
}

fn type_into_shell(h: &mut EditorTestHarness, line: &str) {
    h.editor_mut()
        .active_window_mut()
        .send_terminal_input(line.as_bytes());
}

/// Whether the child's output has reached the terminal grid — for waiting
/// only, while the pane is (on purpose) showing something else.
fn grid_contains(h: &EditorTestHarness, needle: &str) -> bool {
    h.editor()
        .active_window()
        .get_active_terminal_state()
        .is_some_and(|s| s.screen_lines().iter().any(|l| l.contains(needle)))
}

/// The `HIST_nnn` number on screen row `row`, if that row shows one.
fn hist_at_row(h: &EditorTestHarness, row: usize) -> Option<u32> {
    let screen = h.screen_to_string();
    let line = screen.lines().nth(row)?.trim_start();
    line.strip_prefix("HIST_")?
        .split(|c: char| !c.is_ascii_digit())
        .next()?
        .parse()
        .ok()
}

/// Every `HIST_nnn` line on screen, top to bottom.
fn visible_history(h: &EditorTestHarness) -> Vec<String> {
    h.screen_to_string()
        .lines()
        .map(str::trim)
        .filter(|l| l.starts_with("HIST_"))
        .map(|l| l.split_whitespace().next().unwrap_or_default().to_string())
        .collect()
}

/// A live terminal with 60 lines of `HIST_101`..`HIST_160` history and a
/// `TAIL_42` line at the bottom, with the shell's prompt settled.
fn terminal_with_history(jump_to_end_on_output: bool) -> Option<EditorTestHarness> {
    let mut h = harness_or_skip()?;
    h.editor_mut().open_terminal();
    h.editor_mut()
        .set_terminal_jump_to_end_on_output(jump_to_end_on_output);
    h.render().unwrap();
    type_into_shell(
        &mut h,
        "for i in $(seq 101 160); do echo HIST_$i; done; echo TAIL_$((7*6))\n",
    );
    h.wait_until(|h| h.screen_to_string().contains("TAIL_42"))
        .unwrap();
    h.wait_for_async_quiescence(3).unwrap();
    Some(h)
}

/// Scroll up into history, let the program print, then scroll back down.
fn scrolled_view_holds_then_resumes(jump_to_end_on_output: bool) {
    let _sh = pin_posix_shell();
    let Some(mut h) = terminal_with_history(jump_to_end_on_output) else {
        return;
    };
    let (x, y) = WHEEL_AT;

    // The first notch moves the view by one notch's worth (3 lines), from
    // where the live grid was — not to the top of the scrollback.
    let top_before = hist_at_row(&h, TOP_ROW).expect("history fills the top row");
    h.mouse_scroll_up(x, y).unwrap();
    assert_eq!(
        hist_at_row(&h, TOP_ROW),
        Some(top_before - 3),
        "one wheel notch should scroll three lines up.\n{}",
        h.screen_to_string()
    );
    for _ in 0..4 {
        h.mouse_scroll_up(x, y).unwrap();
    }
    h.assert_screen_not_contains("TAIL_42");
    let held = visible_history(&h);
    assert!(!held.is_empty(), "{}", h.screen_to_string());

    // The program prints while the user reads.
    type_into_shell(&mut h, "echo NEW_$((1+1))\n");
    h.wait_until(|h| grid_contains(h, "NEW_2")).unwrap();
    h.wait_for_async_quiescence(3).unwrap();
    assert_eq!(
        visible_history(&h),
        held,
        "new output must not move a view the user scrolled up.\n{}",
        h.screen_to_string()
    );
    h.assert_screen_not_contains("NEW_2");

    // Scrolling back down past the end resumes the live grid, which has the
    // output printed meanwhile and goes on following new output.
    for _ in 0..10 {
        h.mouse_scroll_down(x, y).unwrap();
    }
    h.assert_screen_contains("NEW_2");
    type_into_shell(&mut h, "echo LATER_$((2+3))\n");
    h.wait_until(|h| h.screen_to_string().contains("LATER_5"))
        .unwrap();
}

#[test]
fn wheel_scrolled_view_holds_against_output_and_resumes_at_end() {
    scrolled_view_holds_then_resumes(true);
}

#[test]
fn wheel_scrolled_view_resumes_at_end_without_jump_to_end_on_output() {
    scrolled_view_holds_then_resumes(false);
}

/// A wheel-down on a live terminal has nowhere to go: the terminal must stay
/// live and keep showing new output, not park in a frozen snapshot.
#[test]
fn wheel_down_on_live_terminal_keeps_following_output() {
    let _sh = pin_posix_shell();
    let Some(mut h) = terminal_with_history(false) else {
        return;
    };
    let (x, y) = WHEEL_AT;
    h.mouse_scroll_down(x, y).unwrap();
    type_into_shell(&mut h, "echo AFTER_$((3+4))\n");
    h.wait_until(|h| h.screen_to_string().contains("AFTER_7"))
        .unwrap();
}
