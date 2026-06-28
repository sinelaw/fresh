//! Regression test for issue #2504: the **View → Mouse Support** menu item
//! showed unticked even though the mouse actually worked, and the per-window
//! flag could desync when switching windows.
//!
//! Mouse capture is a single global terminal property (toggling it issues
//! `Enable`/`DisableMouseCapture` on the process stdout), so it is now backed
//! by one shared `Arc<AtomicBool>` rather than a per-window `bool`. These
//! tests pin two properties:
//!
//! 1. A freshly built editor reports mouse capture as enabled (the default,
//!    matching the always-on terminal capture; the real TUI seeds it from the
//!    live `TerminalModes` state at startup).
//! 2. The state is shared across windows — toggling it in one window is
//!    immediately reflected in another, and a newly created workspace window
//!    sees the live value instead of a stale default.

use fresh::config::Config;
use fresh::config_io::DirectoryContext;
use fresh::model::filesystem::{FileSystem, StdFileSystem};
use std::path::Path;
use std::sync::Arc;

fn editor_in(project: &Path, dir_context: &DirectoryContext) -> fresh::app::Editor {
    let filesystem: Arc<dyn FileSystem + Send + Sync> = Arc::new(StdFileSystem);
    let config = Config {
        check_for_updates: false,
        ..Config::default()
    };
    fresh::app::Editor::for_test(
        config,
        80,
        24,
        Some(project.to_path_buf()),
        dir_context.clone(),
        fresh::view::color_support::ColorCapability::TrueColor,
        filesystem,
        None,
        None,
        false,
        false,
    )
    .unwrap()
}

fn test_editor() -> (tempfile::TempDir, fresh::app::Editor) {
    let sandbox = tempfile::tempdir().unwrap();
    let project = sandbox.path().join("project");
    let data_home = sandbox.path().join("data-home");
    std::fs::create_dir_all(&project).unwrap();
    std::fs::create_dir_all(&data_home).unwrap();
    let dir_context = DirectoryContext::for_testing(&data_home);
    let editor = editor_in(&project, &dir_context);
    (sandbox, editor)
}

#[test]
fn initial_window_reports_mouse_enabled() {
    let (_sandbox, editor) = test_editor();

    assert!(
        editor.is_mouse_enabled(),
        "the initial window should report mouse capture as enabled, matching the \
         terminal mouse capture that is turned on at startup",
    );
}

#[test]
fn mouse_capture_state_is_shared_across_windows() {
    let (sandbox, mut editor) = test_editor();

    // Mirror the "Orchestrator: New Workspace" path: a brand new window via
    // `Window::new`, then make it active.
    let new_root = sandbox.path().join("workspace");
    std::fs::create_dir_all(&new_root).unwrap();
    let workspace_id = editor.create_window_at(new_root, "workspace".to_string());
    let initial_id = editor.active_window_id();

    // The freshly created workspace window observes the live state, not a
    // stale per-window default.
    editor.set_active_window(workspace_id);
    assert!(
        editor.is_mouse_enabled(),
        "a freshly created workspace window should report mouse capture as enabled",
    );

    // Disabling capture from one window is visible from every other window —
    // there is a single source of truth, so the menu checkbox can never
    // desync when switching windows.
    editor.set_mouse_capture(false);
    assert!(!editor.is_mouse_enabled(), "disable should take effect");

    editor.set_active_window(initial_id);
    assert!(
        !editor.is_mouse_enabled(),
        "the other window must observe the same (disabled) state after switching",
    );

    editor.set_mouse_capture(true);
    editor.set_active_window(workspace_id);
    assert!(
        editor.is_mouse_enabled(),
        "re-enabling from one window is observed from the other window too",
    );
}
