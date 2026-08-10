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
//! what the PTY read loop does, minus the timing nondeterminism. One wrinkle
//! remains: on Windows, ConPTY itself paints the (empty) screen when the child
//! spawns, racing with — and possibly wiping — the injected bytes, so the
//! helpers re-feed the output until it is stably visible on screen.
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

/// Feed program output directly into the terminal grid, exactly as the PTY
/// read loop would (minus the timing nondeterminism).
fn feed_output(harness: &EditorTestHarness, terminal_id: TerminalId, output: &[u8]) {
    harness
        .editor()
        .terminal_manager()
        .get(terminal_id)
        .expect("terminal handle")
        .state
        .lock()
        .expect("lock terminal state")
        .process_output(output);
}

/// Feed `output` into the terminal grid and wait until `marker` (a substring
/// of that output) is visible on the rendered screen, re-feeding it if it
/// disappears.
///
/// The re-feed matters on Windows: even a no-output child makes ConPTY paint
/// the screen *it* knows about when the process spawns — and that screen does
/// not include bytes injected behind its back — so the PTY reader thread can
/// race with the injection and wipe it from the grid. That burst is finite
/// (`sleep` itself prints nothing), so re-feeding until the text survives a
/// render converges.
fn feed_output_until_visible(
    harness: &mut EditorTestHarness,
    terminal_id: TerminalId,
    output: &[u8],
    marker: &str,
) {
    feed_output(harness, terminal_id, output);
    harness
        .wait_until(|h| {
            if h.screen_to_string().contains(marker) {
                return true;
            }
            // A racing repaint consumed it — feed it again and re-check after
            // the next tick's render.
            feed_output(h, terminal_id, output);
            false
        })
        .unwrap();
}

/// Spawn a no-output terminal (`sleep`) in the active split, enter live
/// terminal mode, and feed `output` into its grid as if a program had printed
/// it, waiting until `visible_marker` shows on screen. Returns the terminal
/// and its buffer id.
fn open_terminal_with_output(
    harness: &mut EditorTestHarness,
    output: &[u8],
    visible_marker: &str,
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
            env: std::collections::HashMap::new(),
        })
        .expect("spawn sleep terminal");
    harness.editor_mut().enter_terminal_mode();
    feed_output_until_visible(harness, terminal_id, output, visible_marker);
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

/// Send a Ctrl+move (buttonless motion) at the given cell — the hover gesture.
fn ctrl_move(harness: &mut EditorTestHarness, col: u16, row: u16) {
    harness
        .send_mouse(MouseEvent {
            kind: MouseEventKind::Moved,
            column: col,
            row,
            modifiers: KeyModifiers::CONTROL,
        })
        .unwrap();
}

/// Whether the rendered cell at `(x, y)` is underlined — how the Ctrl+hover
/// link highlight paints a clickable path (observe rendered output, not state).
fn cell_underlined(harness: &EditorTestHarness, x: u16, y: u16) -> bool {
    harness
        .get_cell_style(x, y)
        .map(|s| {
            s.add_modifier
                .contains(ratatui::style::Modifier::UNDERLINED)
        })
        .unwrap_or(false)
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

    open_terminal_with_output(
        &mut harness,
        b"build error at src/main.rs:2:6 here\n",
        "src/main.rs:2:6",
    );

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
    open_terminal_with_output(&mut harness, osc7.as_bytes(), "edit notes.txt now");

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

    open_terminal_with_output(
        &mut harness,
        b"build error at src/main.rs:2:6 here\n",
        "src/main.rs:2:6",
    );

    // Leave live terminal mode: drop the focused split into read-only
    // scrollback (syncs the grid into the buffer), exactly as a scroll/
    // click-away does.
    harness.editor_mut().enter_terminal_scrollback();
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

    let output: &[u8] = b"see does/not/exist.rs:1:1 here\n";
    let (terminal_id, _buffer_id) =
        open_terminal_with_output(&mut harness, output, "does/not/exist.rs");

    let (col, row) = find_on_screen(&harness, "does/not/exist.rs").expect("path on screen");
    ctrl_left_click(&mut harness, col + 2, row);
    harness.render().unwrap();

    // Nothing opened. The link open path is synchronous, so a wrongly-opened
    // file would already show as a tab — assert on the tab bar (which a
    // ConPTY repaint can't wipe, unlike the grid text itself).
    let tab_bar = harness.get_row_text(1);
    assert!(
        !tab_bar.contains("exist.rs"),
        "Ctrl+Click on a nonexistent path must not open a buffer; tab bar: {tab_bar}"
    );
    // And the live terminal is still the active view: its grid still renders
    // to the screen (re-feed in case a racing repaint wiped it).
    feed_output_until_visible(&mut harness, terminal_id, output, "does/not/exist.rs");
}

/// The real Claude-Code scenario: a full-screen mouse-driven TUI (alt screen +
/// all-motion tracking + SGR) rendering a file reference in its `Tool(path:line)`
/// form. Ctrl+Click on it opens the file — the parenthesised path is detected
/// despite the `Update(` prefix and trailing `)`, and the click isn't forwarded
/// to the program.
#[test]
fn ctrl_click_opens_parenthesized_tool_call_path_in_tui() {
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

    // Alt screen (1049) + all-motion tracking (1003) + SGR (1006), then a
    // tool-call line as Claude Code renders it.
    open_terminal_with_output(
        &mut harness,
        b"\x1b[?1049h\x1b[?1003h\x1b[?1006h\x1b[H  Update(src/main.rs:2:6)\n",
        "src/main.rs:2:6",
    );

    // Click inside the parenthesised path.
    let (col, row) = find_on_screen(&harness, "src/main.rs:2:6").expect("path on screen");
    ctrl_left_click(&mut harness, col + 2, row);
    harness.render().unwrap();

    harness.assert_screen_contains("line two TARGET");
    harness.assert_screen_contains("Ln 2");
}

/// A program that enabled mouse reporting (DECSET 1000/1006 — a "raw mode" TUI
/// that grabbed the mouse) must not steal Ctrl+Click: the click opens the path
/// link instead of being forwarded into the program's stdin. Before the fix,
/// `try_forward_mouse_to_terminal` ran ahead of the link opener and swallowed
/// the Ctrl+press (`wants_mouse && !shift`), so links only worked at a bare
/// shell prompt.
#[test]
fn ctrl_click_opens_path_under_mouse_reporting_program() {
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

    // Enable click + SGR mouse reporting (1000 + 1006), then print the path —
    // exactly what a mouse-driven program on the main screen does.
    open_terminal_with_output(
        &mut harness,
        b"\x1b[?1000h\x1b[?1006hbuild error at src/main.rs:2:6 here\n",
        "src/main.rs:2:6",
    );

    let (col, row) = find_on_screen(&harness, "src/main.rs:2:6").expect("path on screen");
    ctrl_left_click(&mut harness, col + 4, row);
    harness.render().unwrap();

    harness.assert_screen_contains("line two TARGET");
    harness.assert_screen_contains("Ln 2");
}

/// Ctrl+Click opens a path even while an alternate-screen program (vim/less/
/// htop) is running. Before the fix, `detect_terminal_link_at` bailed on
/// alternate-screen, so both the click and the hover highlight were dead there.
#[test]
fn ctrl_click_opens_path_in_alternate_screen() {
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

    // Enter the alternate screen (1049), home the cursor, then print the path
    // onto it — like a full-screen program showing a build error.
    open_terminal_with_output(
        &mut harness,
        b"\x1b[?1049h\x1b[Hbuild error at src/main.rs:2:6 here\n",
        "src/main.rs:2:6",
    );

    let (col, row) = find_on_screen(&harness, "src/main.rs:2:6").expect("path on screen");
    ctrl_left_click(&mut harness, col + 4, row);
    harness.render().unwrap();

    harness.assert_screen_contains("line two TARGET");
    harness.assert_screen_contains("Ln 2");
}

/// Ctrl+hover underlines a path shown by an alternate-screen program. Before the
/// fix, `detect_terminal_link_at` bailed on alternate-screen, so hover never
/// highlighted there even though the click's Moved event reached it.
#[test]
fn ctrl_hover_highlights_path_in_alternate_screen() {
    let tmp = tempfile::tempdir().unwrap();
    std::fs::create_dir_all(tmp.path().join("src")).unwrap();
    std::fs::write(tmp.path().join("src/main.rs"), "x\n").unwrap();

    let mut harness = match harness_or_skip(100, 24, tmp.path().to_path_buf()) {
        Some(h) => h,
        None => return,
    };

    open_terminal_with_output(
        &mut harness,
        b"\x1b[?1049h\x1b[Hopen src/main.rs:2:6 to jump\n",
        "src/main.rs:2:6",
    );

    let (col, row) = find_on_screen(&harness, "src/main.rs:2:6").expect("path on screen");
    // Not underlined until we Ctrl+hover it.
    assert!(
        !cell_underlined(&harness, col + 2, row),
        "path should not be underlined before hovering"
    );

    ctrl_move(&mut harness, col + 2, row);
    harness.render().unwrap();

    assert!(
        cell_underlined(&harness, col, row) && cell_underlined(&harness, col + 4, row),
        "Ctrl+hover should underline the path span in an alternate-screen program"
    );
}

/// Ctrl+hover underlines a path even when the program subscribed to all-motion
/// mouse tracking (DECSET 1003). Before the fix, the buttonless Moved event was
/// forwarded to the PTY (`terminal_wants_mouse_motion`) before the hover code
/// ran, so the highlight never appeared.
#[test]
fn ctrl_hover_highlights_path_under_all_motion_tracking() {
    let tmp = tempfile::tempdir().unwrap();
    std::fs::create_dir_all(tmp.path().join("src")).unwrap();
    std::fs::write(tmp.path().join("src/main.rs"), "x\n").unwrap();

    let mut harness = match harness_or_skip(100, 24, tmp.path().to_path_buf()) {
        Some(h) => h,
        None => return,
    };

    // All-motion tracking (1003) + SGR (1006): the mode that legitimately
    // receives buttonless motion — yet Ctrl+move must still drive link hover.
    open_terminal_with_output(
        &mut harness,
        b"\x1b[?1003h\x1b[?1006hopen src/main.rs:2:6 to jump\n",
        "src/main.rs:2:6",
    );

    let (col, row) = find_on_screen(&harness, "src/main.rs:2:6").expect("path on screen");
    ctrl_move(&mut harness, col + 2, row);
    harness.render().unwrap();

    assert!(
        cell_underlined(&harness, col, row) && cell_underlined(&harness, col + 4, row),
        "Ctrl+hover should underline the path span under all-motion tracking"
    );
}
