//! End-to-end tests for Ctrl+Click / Ctrl+hover "open path in Fresh" from the
//! embedded terminal.
//!
//! These drive real mouse events against a live terminal grid and assert on
//! rendered output (the opened file's contents + status bar), per the project's
//! "observe, not inspect" testing rule.
//!
//! Determinism: instead of spawning a shell (whose prompt output races with the
//! test), we spawn a no-output command (`sleep`) and feed the program output
//! directly into the terminal grid via `process_output`. That mirrors exactly
//! what the PTY read loop does, minus the timing nondeterminism.
//!
//! NOTE: requires a working PTY (/dev/ptmx); skipped where unavailable.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyModifiers, MouseButton, MouseEvent, MouseEventKind};
use fresh::model::event::BufferId;
use fresh::services::terminal::TerminalId;
use portable_pty::{native_pty_system, PtySize};

fn harness_or_skip(
    width: u16,
    height: u16,
    working_dir: std::path::PathBuf,
) -> Option<EditorTestHarness> {
    if native_pty_system()
        .openpty(PtySize {
            rows: 1,
            cols: 1,
            pixel_width: 0,
            pixel_height: 0,
        })
        .is_err()
    {
        eprintln!("Skipping terminal-link test: PTY not available in this environment");
        return None;
    }
    EditorTestHarness::with_working_dir(width, height, working_dir).ok()
}

/// Spawn a no-output terminal (`sleep`) in the active split, enter live
/// terminal mode, and feed `output` into its grid as if a program had printed
/// it. Returns the terminal buffer id.
fn open_terminal_with_output(
    harness: &mut EditorTestHarness,
    output: &[u8],
) -> (TerminalId, BufferId) {
    let (terminal_id, buffer_id, _leaf) = harness
        .editor_mut()
        .active_window_mut()
        .create_plugin_terminal(fresh::app::PluginTerminalSpec {
            cwd: None,       // default to window root (working_dir)
            direction: None, // attach as a tab in the active split
            ratio: None,
            focus: true,
            persistent: false,
            command: Some(vec!["sleep".into(), "30".into()]),
            title: None,
        })
        .expect("spawn sleep terminal");
    harness.editor_mut().enter_terminal_mode();
    // Feed program output directly into the grid (no PTY/prompt race).
    harness
        .editor()
        .terminal_manager()
        .get(terminal_id)
        .expect("terminal handle")
        .state
        .lock()
        .expect("lock terminal state")
        .process_output(output);
    harness.render().unwrap();
    (terminal_id, buffer_id)
}

/// Locate `needle` on the rendered screen, returning the (col, row) of its
/// first character so a test can click into it.
fn find_on_screen(harness: &EditorTestHarness, needle: &str) -> Option<(u16, u16)> {
    let height = harness.buffer().area.height;
    for y in 0..height {
        let text = harness.get_row_text(y);
        if let Some(byte) = text.find(needle) {
            // ASCII content: byte offset == column.
            return Some((byte as u16, y));
        }
    }
    None
}

/// Send a Ctrl+Left click (down + up) at the given cell.
fn ctrl_left_click(harness: &mut EditorTestHarness, col: u16, row: u16) {
    for kind in [
        MouseEventKind::Down(MouseButton::Left),
        MouseEventKind::Up(MouseButton::Left),
    ] {
        harness
            .send_mouse(MouseEvent {
                kind,
                column: col,
                row,
                modifiers: KeyModifiers::CONTROL,
            })
            .unwrap();
    }
}

/// Ctrl+Click on a `path:line:col` printed in the terminal opens the file
/// (resolved against Fresh's working directory) and jumps to that line.
#[test]
fn ctrl_click_opens_workdir_relative_path_at_line() {
    let tmp = tempfile::tempdir().unwrap();
    std::fs::create_dir_all(tmp.path().join("src")).unwrap();
    std::fs::write(
        tmp.path().join("src/main.rs"),
        "line one\nline two TARGET\nline three\n",
    )
    .unwrap();

    let mut harness = match harness_or_skip(100, 24, tmp.path().to_path_buf()) {
        Some(h) => h,
        None => return,
    };

    open_terminal_with_output(&mut harness, b"build error at src/main.rs:2:6 here\n");

    // Sanity: the path is visible in the live terminal grid.
    harness.assert_screen_contains("src/main.rs:2:6");

    let (col, row) = find_on_screen(&harness, "src/main.rs:2:6").expect("path on screen");
    // Click a few cells into the path token.
    ctrl_left_click(&mut harness, col + 4, row);
    harness.render().unwrap();

    // Observe rendered output: the file is now open (its line-2 content shows)
    // and the status bar reports the line-2 jump.
    harness.assert_screen_contains("line two TARGET");
    harness.assert_screen_contains("Ln 2");
}

/// Ctrl+Click resolves a relative path against the terminal's OSC 7 working
/// directory when the file doesn't exist relative to Fresh's workdir.
#[test]
fn ctrl_click_resolves_path_via_osc7_cwd() {
    // Fresh's workdir (no `notes.txt` here).
    let workdir = tempfile::tempdir().unwrap();
    // A separate dir the shell `cd`'d into, reported via OSC 7.
    let osc7_dir = tempfile::tempdir().unwrap();
    std::fs::write(
        osc7_dir.path().join("notes.txt"),
        "OSC7 RESOLVED CONTENT\nsecond line\n",
    )
    .unwrap();

    let mut harness = match harness_or_skip(100, 24, workdir.path().to_path_buf()) {
        Some(h) => h,
        None => return,
    };

    // Emit OSC 7 for `osc7_dir`, then print a path that only resolves there.
    // Build a proper `file://host/<path>` URI with forward slashes so it's
    // well-formed on Windows too (where the dir is `C:\...`): the parser strips
    // the leading `/` before the drive.
    let uri_path = osc7_dir
        .path()
        .to_string_lossy()
        .replace('\\', "/")
        .trim_start_matches('/')
        .to_string();
    let osc7 = format!("\x1b]7;file://host/{uri_path}\x1b\\edit notes.txt now\n");
    open_terminal_with_output(&mut harness, osc7.as_bytes());

    harness.assert_screen_contains("edit notes.txt now");

    let (col, row) = find_on_screen(&harness, "notes.txt").expect("path on screen");
    ctrl_left_click(&mut harness, col + 1, row);
    harness.render().unwrap();

    // The file opened from the OSC 7 directory, not the workdir.
    harness.assert_screen_contains("OSC7 RESOLVED CONTENT");
}

/// Ctrl+Click also works in the terminal *scrollback* view (the synced
/// read-only buffer shown when not in live terminal mode).
#[test]
fn ctrl_click_opens_path_in_scrollback_view() {
    let tmp = tempfile::tempdir().unwrap();
    std::fs::create_dir_all(tmp.path().join("src")).unwrap();
    std::fs::write(
        tmp.path().join("src/main.rs"),
        "line one\nline two TARGET\nline three\n",
    )
    .unwrap();

    let mut harness = match harness_or_skip(100, 24, tmp.path().to_path_buf()) {
        Some(h) => h,
        None => return,
    };

    open_terminal_with_output(&mut harness, b"build error at src/main.rs:2:6 here\n");

    // Leave live terminal mode: sync the grid to the scrollback buffer and
    // drop terminal_mode, exactly as a scroll/click-away does.
    let buffer_id = harness.editor().active_buffer_id();
    harness
        .editor_mut()
        .active_window_mut()
        .sync_terminal_to_buffer(buffer_id);
    harness.editor_mut().active_window_mut().terminal_mode = false;
    harness.render().unwrap();

    // The path is shown by the normal buffer renderer now.
    harness.assert_screen_contains("src/main.rs:2:6");

    let (col, row) = find_on_screen(&harness, "src/main.rs:2:6").expect("path on screen");
    ctrl_left_click(&mut harness, col + 4, row);
    harness.render().unwrap();

    harness.assert_screen_contains("line two TARGET");
    harness.assert_screen_contains("Ln 2");
}

/// A Ctrl+Click on text that does *not* resolve to a real file is inert: no
/// file opens, the terminal stays focused.
#[test]
fn ctrl_click_on_nonexistent_path_is_inert() {
    let tmp = tempfile::tempdir().unwrap();
    let mut harness = match harness_or_skip(100, 24, tmp.path().to_path_buf()) {
        Some(h) => h,
        None => return,
    };

    open_terminal_with_output(&mut harness, b"see does/not/exist.rs:1:1 here\n");
    harness.assert_screen_contains("does/not/exist.rs");

    let (col, row) = find_on_screen(&harness, "does/not/exist.rs").expect("path on screen");
    ctrl_left_click(&mut harness, col + 2, row);
    harness.render().unwrap();

    // Nothing opened: the terminal text is still the active view.
    harness.assert_screen_contains("does/not/exist.rs");
}
