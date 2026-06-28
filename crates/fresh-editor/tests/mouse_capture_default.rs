//! Regression test for issue #2504: the **View → Mouse Support** menu item
//! showed unticked even though the mouse actually worked.
//!
//! The terminal-level mouse capture is enabled unconditionally at startup
//! (see `services::terminal_modes`), but the per-window `mouse_enabled`
//! flag — which drives the menu checkbox via the `mouse_capture` context
//! key — was constructed as `false`. So every freshly built window,
//! including ones created by **Orchestrator: New Workspace**, reported the
//! mouse as disabled while clicks still moved the cursor.
//!
//! The flag must default to `true` so the checkbox matches reality.

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

#[test]
fn initial_window_reports_mouse_enabled() {
    let sandbox = tempfile::tempdir().unwrap();
    let project = sandbox.path().join("project");
    let data_home = sandbox.path().join("data-home");
    std::fs::create_dir_all(&project).unwrap();
    std::fs::create_dir_all(&data_home).unwrap();
    let dir_context = DirectoryContext::for_testing(&data_home);
    let editor = editor_in(&project, &dir_context);

    assert!(
        editor.is_mouse_enabled(),
        "the initial window should report mouse capture as enabled, matching the \
         terminal mouse capture that is turned on at startup",
    );
}

#[test]
fn new_workspace_window_reports_mouse_enabled() {
    let sandbox = tempfile::tempdir().unwrap();
    let project = sandbox.path().join("project");
    let data_home = sandbox.path().join("data-home");
    std::fs::create_dir_all(&project).unwrap();
    std::fs::create_dir_all(&data_home).unwrap();
    let dir_context = DirectoryContext::for_testing(&data_home);
    let mut editor = editor_in(&project, &dir_context);

    // Mirror the "Orchestrator: New Workspace" path, which spins up a brand
    // new window via `Window::new` and makes it active.
    let new_root = project.join("workspace");
    std::fs::create_dir_all(&new_root).unwrap();
    let id = editor.create_window_at(new_root, "workspace".to_string());
    editor.set_active_window(id);

    assert!(
        editor.is_mouse_enabled(),
        "a freshly created workspace window should report mouse capture as \
         enabled so the View -> Mouse Support menu item is ticked",
    );
}
