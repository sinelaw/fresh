//! Regression test (#2237 / #2234 item 4): a born-attached (SSH / Kubernetes)
//! new session must not strand the New-Session form's editor mode on the
//! window the form was opened over.
//!
//! ## The wedge the user reported
//! With a session open, keystrokes stop reaching the buffer: clicking the
//! buffer moves the cursor but typing does nothing, the file explorer still
//! navigates and Enter-opens a file (its own `KeyContext` bypasses the mode
//! check), Esc does NOT help, and ONLY switching to another session restores
//! typing. The trigger is "right after adding an SSH session via the
//! Orchestrator: New dialog".
//!
//! ## Root cause
//! `setEditorMode` writes to whichever window is active when the plugin calls
//! it (`Editor::handle_set_editor_mode` → `active_window_mut().editor_mode`).
//! `orchestrator-open` / `orchestrator-new-form` are defined
//! `readOnly + allowTextInput`, so a window stuck in one swallows every
//! printable key (routed to `mode_text_input:*`) and blocks Esc/arrows.
//!
//! `set_active_window` already clears the *outgoing* window's panel-scoped
//! mode whenever a floating widget panel is mounted, which makes the *local*
//! picker "dive" safe. But the born-attached remote path never goes through
//! `set_active_window`: `create_remote_session_window` →
//! `create_window_with_terminal` writes `self.active_window = id` directly. So
//! when the orchestrator's `runRemoteAttach` does `await attachRemoteAgent(...)`
//! (which builds AND activates the born-attached window) and only THEN runs
//! `closeForm()` → `setEditorMode(null)`, the clear lands on the new window and
//! the source window keeps `orchestrator-new-form`. The local New-Session path
//! is safe only because it calls `closeForm()` *before*
//! `createWindowWithTerminal`.
//!
//! ## What this test does (and asserts on rendered output)
//! It reproduces the exact host-level ordering the SSH path produces, using a
//! local `create_window_with_terminal` (the same host call the born-attached
//! path makes — no SSH server needed), then drives the keyboard and asserts on
//! the *rendered buffer*:
//!   1. open a file in window A and type a baseline marker — it renders,
//!   2. mount the form panel + set `orchestrator-new-form` on A,
//!   3. `create_window_with_terminal` → window B becomes active (direct pointer
//!      write, bypassing the guard),
//!   4. unmount the panel + `setEditorMode(null)` (what `closeForm` does),
//!   5. switch back to A and type a second marker — it must render.
//!
//! Without the fix, step 5's marker is swallowed and never appears on screen.

use crate::common::harness::EditorTestHarness;
use fresh_core::api::{PluginCommand, WidgetSpec};
use portable_pty::{native_pty_system, PtySize};

const WIDTH: u16 = 120;
const HEIGHT: u16 = 40;

/// The orchestrator new-session form's editor mode (`NEW_SESSION_MODE` in
/// `orchestrator.ts`) — defined `readOnly + allowTextInput`.
const NEW_SESSION_MODE: &str = "orchestrator-new-form";

const BASELINE_MARKER: &str = "QQBASELINEQQ";
const POST_SWITCH_MARKER: &str = "ZZWEDGEZZ";

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

/// Minimal valid panel spec — only its presence matters
/// (`floating_widget_panel.is_some()`), not its contents.
fn minimal_panel_spec() -> WidgetSpec {
    WidgetSpec::Spacer {
        cols: 1,
        flex: false,
        key: None,
    }
}

#[test]
fn born_attached_session_does_not_wedge_source_window_typing() {
    if !pty_available() {
        eprintln!("Skipping born-attached typing-wedge test: PTY not available");
        return;
    }

    fresh::i18n::set_locale("en");
    let mut harness = EditorTestHarness::with_temp_project(WIDTH, HEIGHT).unwrap();
    harness.tick_and_render().unwrap();

    // Window A is the base window, active at boot — the session the user opens
    // "Orchestrator: New" from. Open a real file so there's a focused, editable
    // buffer whose contents we can observe on screen.
    let window_a = harness.editor().active_window_id();
    let project_root = harness.project_dir().unwrap().canonicalize().unwrap();
    let file_path = project_root.join("wedge.txt");
    std::fs::write(&file_path, "first line\n").unwrap();
    harness.open_file(&file_path).unwrap();

    // Baseline: typing into A's buffer reaches the buffer and renders.
    harness.type_text(BASELINE_MARKER).unwrap();
    harness.assert_screen_contains(BASELINE_MARKER);

    // 1. `openForm`: mount the New-Session form panel and set its per-window
    //    editor mode on the active window (A).
    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::MountFloatingWidget {
            panel_id: 1,
            spec: minimal_panel_spec(),
            width_pct: 60,
            height_pct: 90,
            as_dock: false,
        })
        .unwrap();
    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::SetEditorMode {
            mode: Some(NEW_SESSION_MODE.into()),
        })
        .unwrap();

    // 2. `await attachRemoteAgent(...)` success → `create_remote_session_window`
    //    → `create_window_with_terminal`. The born-attached window becomes
    //    active via a DIRECT active-pointer write, bypassing `set_active_window`
    //    (and its panel-mode-clear guard). A local terminal exercises the same
    //    host call the SSH path makes.
    let (window_b, _terminal_id, _terminal_buffer) = harness
        .editor_mut()
        .create_window_with_terminal(
            project_root.clone(),
            "agent-session".into(),
            Some(project_root.clone()),
            Some(vec!["sh".into(), "-c".into(), "sleep 60".into()]),
            Some("agent".into()),
            None,
        )
        .expect("create_window_with_terminal should succeed");
    harness.tick_and_render().unwrap();
    assert_eq!(
        harness.editor().active_window_id(),
        window_b,
        "born-attached window must be active after creation",
    );

    // 3. `closeForm()`: unmount the form panel, then clear the editor mode. The
    //    clear lands on the now-active born-attached window (B), not on A.
    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::UnmountFloatingWidget { panel_id: 1 })
        .unwrap();
    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::SetEditorMode { mode: None })
        .unwrap();

    // 4. The user switches back to session A.
    harness.editor_mut().set_active_window(window_a);
    harness.tick_and_render().unwrap();

    // The baseline marker is still on screen — we're back on A's buffer.
    harness.assert_screen_contains(BASELINE_MARKER);

    // 5. Typing into A's buffer must reach the buffer and render. Without the
    //    fix, A is stranded in `orchestrator-new-form` (readOnly +
    //    allowTextInput), which swallows every printable key, so this marker
    //    never appears — the buffer is wedged exactly as reported.
    harness.type_text(POST_SWITCH_MARKER).unwrap();
    harness.assert_screen_contains(POST_SWITCH_MARKER);
}
