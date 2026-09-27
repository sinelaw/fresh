//! Regression test (#2237 / #2234 item 4): a born-attached (SSH / Kubernetes)
//! new session must not strand the New-Session form's mode on the window the
//! form was opened over, nor take that window's own mode away.
//!
//! ## The wedge the user reported
//! With a session open, keystrokes stop reaching the buffer: clicking the
//! buffer moves the cursor but typing does nothing, the file explorer still
//! navigates and Enter-opens a file (its own `KeyContext` bypasses the mode
//! check), Esc does NOT help, and ONLY switching to another session restores
//! typing. The trigger is "right after adding an SSH session via the
//! Orchestrator: New dialog".
//!
//! ## Original root cause
//! The form used to key itself through the window's editor mode
//! (`setEditorMode("orchestrator-new-form")`, a `readOnly + allowTextInput`
//! mode that swallows every printable key). `setEditorMode` writes to
//! whichever window is active, and the born-attached path —
//! `create_remote_session_window` → `create_window_with_terminal`, which
//! writes `self.active_window = id` directly — activates the new window
//! before `closeForm()` cleared the mode, so the clear landed on the new
//! window and the source window stayed wedged. The host then cleared the
//! outgoing window's editor mode on every switch made with a floating panel
//! up.
//!
//! ## Now
//! The form declares its keymap at mount (`mount({ mode })`): the mode lives
//! on the editor-wide floating panel and is never written to a window, so
//! there is nothing to strand. The blanket clear went with it (follow-up to
//! #3386): it could only wipe a mode the window really owns — vi's — and
//! leave `j` typing a `j`.
//!
//! ## What this test does (and asserts on rendered output)
//! It reproduces the host-level ordering the SSH path produces, using a local
//! `create_window_with_terminal` (the same host call the born-attached path
//! makes — no SSH server needed):
//!   1. open a file in window A, give A a mode of its own that lets text
//!      through, and type a baseline marker — it renders,
//!   2. mount the form panel with `orchestrator-new-form` as its mode,
//!   3. `create_window_with_terminal` → window B becomes active (direct
//!      pointer write),
//!   4. unmount the panel (what `closeForm` does),
//!   5. switch back to A: A still holds its own mode, B never held the
//!      form's, and a second marker typed into A renders.

use crate::common::harness::EditorTestHarness;
use fresh_core::api::{PluginCommand, WidgetSpec};
use portable_pty::{native_pty_system, PtySize};

const WIDTH: u16 = 120;
const HEIGHT: u16 = 40;

/// The orchestrator new-session form's keymap (`NEW_SESSION_MODE` in
/// `orchestrator.ts`) — defined `readOnly + allowTextInput`, so a window
/// stranded in it swallows every printable key.
const NEW_SESSION_MODE: &str = "orchestrator-new-form";

/// Window A's own mode: a stand-in for a persistent per-window mode (vi's,
/// markdown-source's). Not read-only, so text still reaches the buffer.
const WINDOW_MODE: &str = "test-window-mode";

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

    // Both modes exist, as the plugins that own them define them.
    for (name, read_only, allow_text_input) in
        [(NEW_SESSION_MODE, true, true), (WINDOW_MODE, false, false)]
    {
        harness
            .editor_mut()
            .handle_plugin_command(PluginCommand::DefineMode {
                name: name.into(),
                bindings: Vec::new(),
                read_only,
                allow_text_input,
                inherit_normal_bindings: false,
                plugin_name: None,
                shortcuts: Vec::new(),
                scoped: Vec::new(),
            })
            .unwrap();
    }
    // Window A holds a mode of its own before the dialog opens.
    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::SetEditorMode {
            mode: Some(WINDOW_MODE.into()),
        })
        .unwrap();

    // Baseline: typing into A's buffer reaches the buffer and renders.
    harness.type_text(BASELINE_MARKER).unwrap();
    harness.assert_screen_contains(BASELINE_MARKER);

    // 1. `openForm`: mount the New-Session form panel with its own keymap.
    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::MountFloatingWidget {
            plugin: "test-plugin".to_string(),
            panel_id: 1,
            spec: minimal_panel_spec(),
            width_pct: 60,
            height_pct: 90,
            as_dock: false,
            focus_marker: false,
            label_align: Default::default(),
            title: None,
            closable: false,
            start_blurred: false,
            mode: Some(NEW_SESSION_MODE.into()),
        })
        .unwrap();

    // 2. `await attachRemoteAgent(...)` success → `create_remote_session_window`
    //    → `create_window_with_terminal`. The born-attached window becomes
    //    active via a DIRECT active-pointer write, bypassing
    //    `set_active_window`. A local terminal exercises the same host call
    //    the SSH path makes.
    let born_authority = harness.editor().local_session_authority(&project_root);
    let (window_b, _terminal_id, _terminal_buffer) = harness
        .editor_mut()
        .create_window_with_terminal(
            project_root.clone(),
            "agent-session".into(),
            Some(project_root.clone()),
            Some(vec!["sh".into(), "-c".into(), "sleep 60".into()]),
            Some("agent".into()),
            std::sync::Arc::new(fresh::services::authority::Connection::plain(
                born_authority,
            )),
            None,
            None,
            false,
            None,
        )
        .expect("create_window_with_terminal should succeed");
    harness.tick_and_render().unwrap();
    assert_eq!(
        harness.editor().active_window_id(),
        window_b,
        "born-attached window must be active after creation",
    );

    // 3. `closeForm()`: unmount the form panel. Its mode goes with it.
    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::UnmountFloatingWidget {
            plugin: "test-plugin".to_string(),
            panel_id: 1,
        })
        .unwrap();
    assert_eq!(
        harness.editor().editor_mode(),
        None,
        "the form's mode leaked onto the born-attached window",
    );

    // 4. The user switches back to session A.
    harness.editor_mut().set_active_window(window_a);
    harness.tick_and_render().unwrap();

    // The baseline marker is still on screen — we're back on A's buffer —
    // and A still holds its own mode: switching away with the form up must
    // neither strand the form's mode there nor wipe A's.
    harness.assert_screen_contains(BASELINE_MARKER);
    assert_eq!(
        harness.editor().editor_mode(),
        Some(WINDOW_MODE.to_string()),
        "the born-attached switch changed window A's own mode",
    );

    // 5. Typing into A's buffer must reach the buffer and render. A window
    //    stranded in `orchestrator-new-form` (readOnly + allowTextInput)
    //    swallows every printable key, so this marker would never appear.
    harness.type_text(POST_SWITCH_MARKER).unwrap();
    harness.assert_screen_contains(POST_SWITCH_MARKER);
}
