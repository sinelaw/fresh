//! Regression test: a floating-panel-scoped editor mode must not
//! leak onto a window the user switches *away* from.
//!
//! Bug: after using the Orchestrator (which mounts a floating widget
//! panel — the session picker / new-session form — and sets a
//! per-window `editor_mode` via `setEditorMode`), then interacting
//! with a session's terminal and opening a file via quick-open, the
//! newly opened buffer ignored *all* keyboard input — no cursor
//! movement, no edits, no status-bar feedback — until the user
//! "switched orchestrator sessions back and forth".
//!
//! Root cause: `setEditorMode` writes to whichever window is active
//! when the plugin calls it. The orchestrator "dive" switches the
//! active window (`setActiveWindow(target)`) *before* it clears the
//! mode (`closeOpenDialog()` → `setEditorMode(null)`), so the clear
//! lands on the destination window and the source window is left
//! stuck in the panel's mode. That mode is masked while the window
//! sits in terminal mode and then silently swallows every printable
//! key once the user leaves terminal mode (opens a file).
//!
//! Fix: `set_active_window` clears the outgoing window's `editor_mode`
//! whenever a floating widget panel is mounted — a panel-scoped mode
//! belongs to the (global) panel, not the window it was opened over.
//!
//! This test reproduces the leak with a minimal plugin-command
//! sequence: mount a panel + set a mode on window A, switch to B,
//! switch back to A, and assert A's mode was cleared (it would still
//! be `Some(...)` — and thus eat input — without the fix).

use crate::common::harness::EditorTestHarness;
use fresh_core::api::{PluginCommand, WidgetSpec};

const WIDTH: u16 = 120;
const HEIGHT: u16 = 40;

/// Minimal valid panel spec — its contents don't matter, only that a
/// floating widget panel is mounted (`floating_widget_panel.is_some()`).
fn minimal_panel_spec() -> WidgetSpec {
    WidgetSpec::Spacer {
        cols: 1,
        flex: false,
        key: None,
    }
}

#[test]
fn panel_mode_does_not_leak_onto_window_switched_away_from() {
    let mut harness = EditorTestHarness::with_temp_project(WIDTH, HEIGHT).unwrap();

    // Window A is the base window (id 1), active at boot. Window B is
    // a second project window we create but do NOT activate yet.
    // Keep the tempdir alive for the test's duration (dropping it
    // would delete B's root out from under the harness).
    let win_b_dir = tempfile::tempdir().unwrap();
    let win_b = harness
        .editor_mut()
        .create_window_at(win_b_dir.path().to_path_buf(), "session-b".into());

    // Simulate the Orchestrator picker on window A: mount a floating
    // widget panel and set a panel-scoped editor mode. This is exactly
    // what `openControlRoom` does (`setEditorMode("orchestrator-open")`
    // + a mounted picker panel).
    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::MountFloatingWidget {
            panel_id: 1,
            spec: minimal_panel_spec(),
            width_pct: 50,
            height_pct: 50,
            as_dock: false,
        })
        .unwrap();
    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::SetEditorMode {
            mode: Some("orchestrator-open".into()),
        })
        .unwrap();

    assert_eq!(
        harness.editor().editor_mode(),
        Some("orchestrator-open".to_string()),
        "precondition: window A should hold the panel-scoped mode"
    );

    // The "dive": switch the active window while the panel is still
    // mounted (mirrors the orchestrator calling `setActiveWindow`
    // before `closeOpenDialog`'s `setEditorMode(null)`).
    harness.editor_mut().set_active_window(win_b);

    // Return to window A by any non-picker route (the host
    // Next/Prev Window cycle, a tab click, etc. — none of which clear
    // a plugin mode). Without the fix, A is still stuck in
    // "orchestrator-open" here.
    harness
        .editor_mut()
        .set_active_window(fresh_core::WindowId(1));

    assert_eq!(
        harness.editor().editor_mode(),
        None,
        "panel-scoped mode leaked onto window A after switching away \
         and back — it would silently swallow all buffer input"
    );
}
