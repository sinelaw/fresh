//! Regression tests for eager Orchestrator-session persistence.
//!
//! Sessions used to be written to the directory-keyed workspace registry
//! (`workspaces/*.json`) only when the editor exited *cleanly* (the quit-time
//! `save_all_windows_workspaces`). A killed or crashed editor therefore forgot
//! every session opened since the last clean quit — the dock came back missing
//! the workspaces the user actually had open.
//!
//! The fix checkpoints a window's workspace at natural points that don't depend
//! on a clean shutdown: switching away from a window, and finalizing a new
//! session's identity (`setWindowState`, which the Orchestrator calls right
//! after creating a window). These tests pin that behavior by asserting the
//! on-disk workspace exists *without any quit having happened*.

use fresh::config::Config;
use fresh::config_io::DirectoryContext;
use fresh::model::filesystem::StdFileSystem;
use fresh::workspace::Workspace;
use std::path::Path;
use std::sync::Arc;

fn editor_in(project: &Path, dir_context: &DirectoryContext) -> fresh::app::Editor {
    let filesystem: Arc<dyn fresh::model::filesystem::FileSystem + Send + Sync> =
        Arc::new(StdFileSystem);
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

/// Switching away from a window writes its workspace immediately — a later
/// hard kill (no clean quit) still finds it in the registry.
#[test]
fn switching_away_persists_the_outgoing_window_without_a_quit() {
    let sandbox = tempfile::tempdir().unwrap();
    let proj_a = sandbox.path().join("a");
    let proj_b = sandbox.path().join("b");
    let data_home = sandbox.path().join("data-home");
    std::fs::create_dir_all(&proj_a).unwrap();
    std::fs::create_dir_all(&proj_b).unwrap();
    std::fs::create_dir_all(&data_home).unwrap();
    // Unique tmp roots, so the global workspace registry has no stale entry for
    // either — the precondition assertion below is meaningful.
    let proj_a = proj_a.canonicalize().unwrap();
    let proj_b = proj_b.canonicalize().unwrap();
    let file_a = proj_a.join("hello.txt");
    std::fs::write(&file_a, "hi").unwrap();

    let dir_context = DirectoryContext::for_testing(&data_home);
    let mut e = editor_in(&proj_a, &dir_context);
    e.open_file(&file_a).unwrap();

    // No clean quit has happened and we never switched away, so A's session is
    // not yet in the on-disk registry.
    assert!(
        Workspace::load(&proj_a).unwrap().is_none(),
        "precondition: window A's workspace must not be on disk before any checkpoint"
    );

    // Open a second window and switch to it. The switch is the checkpoint: it
    // must persist the *outgoing* window (A) before leaving it.
    let win_b = e.create_window_at(proj_b.clone(), "b".into());
    e.set_active_window(win_b);

    let saved = Workspace::load(&proj_a)
        .unwrap()
        .expect("switching away must persist the outgoing window without a quit");
    assert_eq!(
        saved.working_dir, proj_a,
        "the persisted workspace is window A's, keyed on its own root"
    );
    assert!(
        saved_contains_file(&saved, &file_a),
        "the checkpoint captured A's open file (hello.txt)"
    );
}

/// The file A had open must be recorded somewhere in the saved workspace. It
/// lives under the project root, so it is captured in the split layout rather
/// than `external_files`; a JSON scan for its name is stable regardless of the
/// exact serialized split-node shape.
fn saved_contains_file(ws: &Workspace, file: &Path) -> bool {
    let json = serde_json::to_string(ws).unwrap_or_default();
    let name = file.file_name().and_then(|n| n.to_str()).unwrap_or("");
    !name.is_empty() && json.contains(name)
}

/// Setting per-session plugin state (what the Orchestrator does right after
/// creating a session, to tag its `project_path`) checkpoints the window, so a
/// freshly created session is in the registry the moment it is tagged — before
/// any switch or quit.
#[test]
fn tagging_a_new_session_persists_it_without_a_quit() {
    use fresh_core::api::PluginCommand;

    let sandbox = tempfile::tempdir().unwrap();
    let proj_a = sandbox.path().join("a");
    let proj_b = sandbox.path().join("b");
    let data_home = sandbox.path().join("data-home");
    std::fs::create_dir_all(&proj_a).unwrap();
    std::fs::create_dir_all(&proj_b).unwrap();
    std::fs::create_dir_all(&data_home).unwrap();
    let proj_a = proj_a.canonicalize().unwrap();
    let proj_b = proj_b.canonicalize().unwrap();
    let file_a = proj_a.join("seed.txt");
    std::fs::write(&file_a, "x").unwrap();

    let dir_context = DirectoryContext::for_testing(&data_home);
    let mut e = editor_in(&proj_a, &dir_context);

    // Create a second session for project B and make it active — this mirrors
    // what `createWindowWithTerminal` does (it dives into the new window). Give
    // it real content so it is savable.
    let win_b = e.create_window_at(proj_b.clone(), "b".into());
    e.set_active_window(win_b);
    let file_b = proj_b.join("seed.txt");
    std::fs::write(&file_b, "y").unwrap();
    e.open_file(&file_b).unwrap();

    // Not yet tagged, and if the harness didn't checkpoint on switch we can't
    // rely on B being on disk — so drive the exact tagging call the plugin
    // makes and require *that* to persist B.
    let before = Workspace::load(&proj_b).unwrap();

    e.handle_plugin_command(PluginCommand::SetWindowState {
        plugin_name: "orchestrator".into(),
        key: "project_path".into(),
        value: Some(serde_json::Value::String(proj_b.to_string_lossy().into_owned())),
    })
    .unwrap();

    let after = Workspace::load(&proj_b)
        .unwrap()
        .expect("tagging a session's identity must persist it without a quit");
    assert_eq!(after.working_dir, proj_b);
    assert_eq!(
        after.session_plugin_state["orchestrator"]["project_path"],
        serde_json::Value::String(proj_b.to_string_lossy().into_owned()),
        "the persisted session carries the project_path the plugin just set"
    );
    // Whether or not `before` existed (a switch-away checkpoint may have
    // written it already), the tagging call must leave a complete, identity-
    // carrying record behind.
    let _ = before;
}
