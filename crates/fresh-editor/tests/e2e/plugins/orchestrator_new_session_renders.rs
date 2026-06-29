//! Regression coverage for the "Orchestrator new session shows
//! empty buffer until a key is pressed" bug.
//!
//! User-facing flow (`Orchestrator: Open` → `Alt+N` → fill `Agent
//! Command` with `python3` → submit) opens the new session's tab
//! on a terminal buffer, but the content area paints blank. Any
//! printable keystroke (e.g. `a`) makes the live grid appear.
//!
//! Under the hood, the JS plugin issues `editor.createWindow(...)`
//! followed by `editor.createTerminal({ focus: false, command:
//! argv, ... })`. The host's `create_plugin_terminal` builds the
//! terminal buffer and switches the window's active buffer to it,
//! but leaves `terminal_mode` off — so the rendering path skips
//! the live grid and shows the (still-empty) file-backed scrollback
//! view instead.
//!
//! The bug is unmasked when `jump_to_end_on_output` is `false`.
//! With the default-on setting, the `TerminalOutput` async handler
//! auto-enters terminal mode on the first PTY chunk and hides the
//! issue.
//!
//! The test reproduces the symptom by issuing the same plugin
//! commands the orchestrator plugin issues from its
//! `window_created` hook, then asserting that the printed marker
//! text is visible on screen without any further keyboard input.

#![cfg(feature = "plugins")]

use crate::common::harness::EditorTestHarness;
use fresh_core::api::PluginCommand;
use portable_pty::{native_pty_system, PtySize};

const MARKER: &str = "STARTUP_DONE_MARKER";

fn pty_available() -> bool {
    native_pty_system()
        .openpty(PtySize {
            rows: 1,
            cols: 1,
            pixel_width: 0,
            pixel_height: 0,
        })
        .is_ok()
}

/// After creating a new orchestrator session whose agent command
/// produces immediate output, that output must be visible without
/// requiring the user to press any key.
///
/// `create_plugin_terminal` (the host code path that the
/// orchestrator plugin reaches via `editor.createTerminal`) leaves
/// `terminal_mode = false`, so the renderer falls back to the
/// file-backed scrollback view of the terminal buffer — which is
/// empty until the live grid is synced. The fix is to enable
/// terminal mode in `create_plugin_terminal` (mirroring
/// `open_terminal_in_window`) so the live grid renders immediately.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses Unix shell (sh -c) as the agent command.
fn new_session_renders_terminal_output_without_keypress() {
    if !pty_available() {
        eprintln!("Skipping orchestrator new-session render test: PTY not available");
        return;
    }

    fresh::i18n::set_locale("en");
    let mut harness = EditorTestHarness::with_temp_project(160, 50).unwrap();
    harness.tick_and_render().unwrap();

    // Unmask the bug: with `jump_to_end_on_output = true`, the
    // first chunk of PTY output triggers `enter_terminal_mode()`
    // from the async dispatch path and the live grid renders even
    // though `create_plugin_terminal` left `terminal_mode` off.
    // With it disabled, the grid stays hidden until something else
    // flips `terminal_mode`.
    harness
        .editor_mut()
        .set_terminal_jump_to_end_on_output(false);

    let project_root = harness.project_dir().unwrap().canonicalize().unwrap();

    // Mirror the orchestrator plugin's `window_created` handoff:
    // create a new window, dive into it, then ask the host to
    // spawn the agent command in a terminal in that window with
    // `focus: false` (the orchestrator plugin passes false because
    // it dove into the window first; `create_plugin_terminal`
    // still attaches the terminal buffer to the active split).
    let new_window = harness
        .editor_mut()
        .create_window_at(project_root.clone(), "agent-session".into());
    harness.editor_mut().set_active_window(new_window);
    harness.tick_and_render().unwrap();

    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::CreateTerminal {
            cwd: Some(project_root.to_string_lossy().into_owned()),
            direction: None,
            ratio: None,
            focus: Some(false),
            persistent: false,
            window_id: Some(new_window),
            command: Some(vec![
                "sh".into(),
                "-c".into(),
                format!("printf {}; sleep 60", MARKER),
            ]),
            title: Some("agent".into()),
            request_id: 0,
        })
        .unwrap();

    // Drive the event loop until either the marker shows up on
    // screen (fix in place) or we've given the PTY ample time
    // to produce output without it becoming visible (bug in
    // place). We deliberately do NOT use `wait_until` here:
    // its indefinite poll wouldn't catch the "still empty
    // after the output arrived" state — only the timeout
    // would, and that's an unreliable failure signature in CI.
    // Instead, drive ticks until the *editor* has observed the
    // PTY output (the terminal manager's state grid has
    // visible content), then assert on the rendered screen.
    let terminal_buffer = harness.editor().active_buffer_id();
    harness
        .wait_until(|h| {
            let win = h.editor().active_window();
            let Some(terminal_id) = win.get_terminal_id(terminal_buffer) else {
                return false;
            };
            let Some(handle) = win.terminal_manager.get(terminal_id) else {
                return false;
            };
            let Ok(state) = handle.state.lock() else {
                return false;
            };
            // Walk every row in the grid for the marker —
            // testing whether the host has actually parsed the
            // PTY chunk into the terminal state, independent of
            // whether the renderer chose to draw it.
            let (_, rows) = state.size();
            (0..rows).any(|r| {
                let line: String = state.get_line(r).iter().map(|cell| cell.c).collect();
                line.contains(MARKER)
            })
        })
        .unwrap();

    // The output is in the terminal state. The only remaining
    // question is whether the renderer painted it. Tick once
    // more so the renderer observes the current state.
    harness.tick_and_render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains(MARKER),
        "The new session's terminal output (`{}`) reached the host's terminal \
         state but did not render. The bug: `create_plugin_terminal` leaves \
         `terminal_mode = false`, so the renderer falls back to the empty \
         scrollback view. Screen:\n{}",
        MARKER,
        screen,
    );
}

/// The atomic `createWindowWithTerminal` entry point — the path
/// Orchestrator's new-session flow takes — must seed the new
/// window with the agent terminal as its *only* buffer, never
/// alongside a placeholder `[No Name]` tab. The legacy
/// `createWindow + createTerminal` sequence the orchestrator used
/// to do left `[No Name]` as a leftover first tab because
/// `create_window_at`'s eager seed populates the layout before the
/// terminal arrives.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses Unix shell (sh -c) as the agent command.
fn new_session_atomic_api_seeds_terminal_as_only_tab() {
    if !pty_available() {
        eprintln!("Skipping orchestrator atomic-API test: PTY not available");
        return;
    }

    fresh::i18n::set_locale("en");
    let mut harness = EditorTestHarness::with_temp_project(160, 50).unwrap();
    harness.tick_and_render().unwrap();

    let project_root = harness.project_dir().unwrap().canonicalize().unwrap();

    let born_authority = harness.editor().local_session_authority(&project_root);
    let (new_window, _terminal_id, terminal_buffer) = harness
        .editor_mut()
        .create_window_with_terminal(
            project_root.clone(),
            "agent-session".into(),
            Some(project_root.clone()),
            Some(vec![
                "sh".into(),
                "-c".into(),
                format!("printf {}; sleep 60", MARKER),
            ]),
            Some("agent".into()),
            born_authority,
            None,
        )
        .expect("create_window_with_terminal should succeed");

    harness.tick_and_render().unwrap();

    // The new window's buffer set must contain exactly one
    // buffer — the terminal. No `[No Name]` placeholder.
    let win = harness
        .editor()
        .session(new_window)
        .expect("new window present");
    let buffer_count = win.buffers.len();
    assert_eq!(
        buffer_count, 1,
        "new session window should be born with the terminal as its only buffer; \
         found {} buffers (a `[No Name]` placeholder likely got seeded alongside)",
        buffer_count,
    );

    // And that one buffer should be the terminal buffer that
    // `create_window_with_terminal` returned.
    assert!(
        win.terminal_buffers.contains_key(&terminal_buffer),
        "the seed buffer must be the agent terminal — `create_window_with_terminal` \
         returned its buffer id but the window's terminal-buffer map doesn't \
         know about it",
    );

    // Rendered screen sanity: only one tab visible (the terminal's),
    // no `[No Name]` chrome.
    harness
        .wait_until(|h| {
            let win = h.editor().active_window();
            let Some(tid) = win.get_terminal_id(terminal_buffer) else {
                return false;
            };
            let Some(handle) = win.terminal_manager.get(tid) else {
                return false;
            };
            let Ok(state) = handle.state.lock() else {
                return false;
            };
            let (_, rows) = state.size();
            (0..rows).any(|r| {
                let line: String = state.get_line(r).iter().map(|cell| cell.c).collect();
                line.contains(MARKER)
            })
        })
        .unwrap();
    harness.tick_and_render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("[No Name]"),
        "the new session window must not surface a `[No Name]` placeholder tab. \
         Screen:\n{}",
        screen,
    );
    assert!(
        screen.contains(MARKER),
        "the agent terminal's output must render on screen without any further \
         input. Screen:\n{}",
        screen,
    );
}
