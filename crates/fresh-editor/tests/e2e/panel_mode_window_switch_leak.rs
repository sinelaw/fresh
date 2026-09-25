//! Regression test: switching windows while a floating panel is mounted
//! neither leaks the panel's mode onto a window nor takes a window's own
//! editor mode away.
//!
//! History (#2237 / #2234 item 4): the Orchestrator used to key its picker
//! and new-session form through the window's editor mode
//! (`setEditorMode("orchestrator-open")`). `setEditorMode` writes to
//! whichever window is active, and the "dive" switched windows before the
//! dialog cleared the mode, so the source window was left in the dialog's
//! mode and swallowed every printable key. The host papered over it by
//! clearing the outgoing window's editor mode on every switch made with a
//! floating panel up.
//!
//! A panel now names its keymap at mount (`mount({ mode })`,
//! `FloatingWidgetState::mode`): editor-wide, travelling with the panel,
//! never written to a window. There is nothing left to leak, and clearing
//! the outgoing window's mode only wiped whatever really lived there — vi's
//! "vi-normal", so afterwards `j` typed a `j` (follow-up to #3386).
//!
//! The test mounts a panel with its own mode over window A, which holds a
//! mode of its own, switches to B and back, and asserts B never picks up
//! either mode and A keeps its own throughout.

use crate::common::harness::EditorTestHarness;
use fresh_core::api::{PluginCommand, WidgetSpec};

const WIDTH: u16 = 120;
const HEIGHT: u16 = 40;

/// The window's own mode — what vi_mode keeps in the slot.
const WINDOW_MODE: &str = "vi-normal";
/// The panel's keymap, declared at mount.
const PANEL_MODE: &str = "orchestrator-open";

/// Minimal valid panel spec — its contents don't matter, only that a
/// floating widget panel is mounted.
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

    // Window A is the base window, active at boot. Window B is a second
    // project window we create but do NOT activate yet. Keep the tempdir
    // alive for the test's duration (dropping it would delete B's root out
    // from under the harness).
    let window_a = harness.editor().active_window_id();
    let win_b_dir = tempfile::tempdir().unwrap();
    let win_b = harness
        .editor_mut()
        .create_window_at(win_b_dir.path().to_path_buf(), "session-b".into());

    // Window A holds a mode of its own (vi's), set before any dialog opens.
    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::SetEditorMode {
            mode: Some(WINDOW_MODE.into()),
        })
        .unwrap();

    // The Orchestrator picker over window A: a floating panel that declares
    // its keymap at mount, as `openControlRoom` does.
    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::MountFloatingWidget {
            plugin: "test-plugin".to_string(),
            panel_id: 1,
            spec: minimal_panel_spec(),
            width_pct: 50,
            height_pct: 50,
            as_dock: false,
            focus_marker: false,
            label_align: Default::default(),
            title: None,
            closable: false,
            start_blurred: false,
            mode: Some(PANEL_MODE.into()),
        })
        .unwrap();
    assert_eq!(
        harness.editor().editor_mode(),
        Some(WINDOW_MODE.to_string()),
        "mounting a panel with its own mode leaves the window's mode alone"
    );

    // The "dive": switch the active window while the panel is still up.
    harness.editor_mut().set_active_window(win_b);
    assert_eq!(
        harness.editor().editor_mode(),
        None,
        "window B picked up a mode it never set"
    );

    // Return to window A by any non-picker route (the host Next/Prev Window
    // cycle, a tab click, ...).
    harness.editor_mut().set_active_window(window_a);
    assert_eq!(
        harness.editor().editor_mode(),
        Some(WINDOW_MODE.to_string()),
        "switching away with a floating panel up wiped window A's own mode"
    );

    // Closing the dialog hands nothing back and takes nothing away: A keeps
    // its mode, B still has none.
    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::UnmountFloatingWidget {
            plugin: "test-plugin".to_string(),
            panel_id: 1,
        })
        .unwrap();
    assert_eq!(
        harness.editor().editor_mode(),
        Some(WINDOW_MODE.to_string()),
        "closing the panel changed window A's mode"
    );
    harness.editor_mut().set_active_window(win_b);
    assert_eq!(
        harness.editor().editor_mode(),
        None,
        "the panel's mode leaked onto window B"
    );
}
