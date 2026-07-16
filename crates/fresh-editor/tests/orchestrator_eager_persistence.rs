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

/// Setting editor-global plugin state (what the Orchestrator does when the
/// user organises dock sessions into folders) must be flushed to
/// `<data>/orchestrator/state/<plugin>.json` immediately — before any quit.
/// It used to be written only by the clean-quit `save_orchestrator_state`
/// call, so a killed or crashed editor forgot every folder and
/// session→folder assignment made since the last clean exit (issue #2703).
#[cfg(feature = "plugins")]
#[test]
fn setting_global_state_persists_it_without_a_quit() {
    use fresh_core::api::PluginCommand;

    let sandbox = tempfile::tempdir().unwrap();
    let proj = sandbox.path().join("proj");
    let data_home = sandbox.path().join("data-home");
    std::fs::create_dir_all(&proj).unwrap();
    std::fs::create_dir_all(&data_home).unwrap();
    let proj = proj.canonicalize().unwrap();

    let dir_context = DirectoryContext::for_testing(&data_home);
    let mut e = editor_in(&proj, &dir_context);

    let folders = serde_json::json!([{ "id": "df1", "name": "myfolder", "parent": null }]);
    e.handle_plugin_command(PluginCommand::SetGlobalState {
        plugin_name: "orchestrator".into(),
        key: "orchestrator.dock.folders".into(),
        value: Some(folders.clone()),
    })
    .unwrap();

    // No quit has happened — the state file must already be on disk.
    let state_path = dir_context
        .data_dir
        .join("orchestrator")
        .join("state")
        .join("orchestrator.json");
    let bytes = std::fs::read(&state_path).unwrap_or_else(|e| {
        panic!(
            "setting global state must persist {} without a quit: {e}",
            state_path.display()
        )
    });
    let map: serde_json::Value = serde_json::from_slice(&bytes).unwrap();
    assert_eq!(
        map["orchestrator.dock.folders"], folders,
        "the persisted state carries the folder list the plugin just set"
    );

    // Deleting the key persists too (an empty map on disk, not the stale
    // folder list) — clearing your last folder must also survive a crash.
    e.handle_plugin_command(PluginCommand::SetGlobalState {
        plugin_name: "orchestrator".into(),
        key: "orchestrator.dock.folders".into(),
        value: None,
    })
    .unwrap();
    let map: serde_json::Value =
        serde_json::from_slice(&std::fs::read(&state_path).unwrap()).unwrap();
    assert!(
        map.get("orchestrator.dock.folders").is_none(),
        "deleting the key must be flushed as well, got: {map}"
    );
}

/// Two editor processes sharing the data dir must not clobber each other's
/// global plugin state. Each instance loads `orchestrator/state/*.json` once
/// at boot, and the per-change flush used to rewrite the whole file from that
/// instance's in-memory map — so an instance that booted *before* another
/// instance organised the dock would, on its next unrelated write (a fold
/// toggle, a history push) or on quit, silently revert every folder and
/// session→folder assignment the other instance had saved. Reproduced
/// interactively: organise folders in instance B, collapse a folder in
/// instance A → B's folders vanish from disk.
///
/// The fix merges on write: an instance only rewrites the keys *it* changed,
/// on top of whatever is on disk, instead of snapshotting its whole map.
#[cfg(feature = "plugins")]
#[test]
fn concurrent_instances_do_not_clobber_each_others_global_state() {
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

    let dir_context = DirectoryContext::for_testing(&data_home);
    // Instance A boots first — its in-memory copy of the global state is
    // whatever was on disk now (nothing).
    let mut a = editor_in(&proj_a, &dir_context);
    // Instance B boots and the user organises the dock there: a folder and a
    // session filed under it.
    let mut b = editor_in(&proj_b, &dir_context);
    let folders = serde_json::json!([{ "id": "df1", "name": "Keep", "parent": null }]);
    let assignments = serde_json::json!({ proj_b.to_string_lossy(): "df1" });
    b.handle_plugin_command(PluginCommand::SetGlobalState {
        plugin_name: "orchestrator".into(),
        key: "orchestrator.dock.folders".into(),
        value: Some(folders.clone()),
    })
    .unwrap();
    b.handle_plugin_command(PluginCommand::SetGlobalState {
        plugin_name: "orchestrator".into(),
        key: "orchestrator.dock.assignments".into(),
        value: Some(assignments.clone()),
    })
    .unwrap();

    // Instance A — which knows nothing of B's folders — makes an unrelated
    // change (what a fold toggle in A's dock does). This must not wipe B's
    // folders from disk.
    let expanded = serde_json::json!(["folder:df1"]);
    a.handle_plugin_command(PluginCommand::SetGlobalState {
        plugin_name: "orchestrator".into(),
        key: "orchestrator.dock.expanded".into(),
        value: Some(expanded.clone()),
    })
    .unwrap();

    let state_path = dir_context
        .data_dir
        .join("orchestrator")
        .join("state")
        .join("orchestrator.json");
    let read_state = || -> serde_json::Value {
        serde_json::from_slice(&std::fs::read(&state_path).unwrap()).unwrap()
    };
    let map = read_state();
    assert_eq!(
        map["orchestrator.dock.folders"], folders,
        "A's unrelated write must not clobber the folders B saved; got: {map}"
    );
    assert_eq!(
        map["orchestrator.dock.assignments"], assignments,
        "A's unrelated write must not clobber the assignments B saved; got: {map}"
    );
    assert_eq!(
        map["orchestrator.dock.expanded"], expanded,
        "A's own change must land alongside B's keys; got: {map}"
    );

    // A's clean quit must not resurrect its stale snapshot either — the
    // quit-time save flushes only what A actually changed.
    a.save_orchestrator_state();
    let map = read_state();
    assert_eq!(
        map["orchestrator.dock.folders"], folders,
        "A's quit-time save must not clobber the folders B saved; got: {map}"
    );

    // And a deletion in A merges too: it removes exactly that key from disk,
    // not everything A never knew about.
    a.handle_plugin_command(PluginCommand::SetGlobalState {
        plugin_name: "orchestrator".into(),
        key: "orchestrator.dock.expanded".into(),
        value: None,
    })
    .unwrap();
    let map = read_state();
    assert!(
        map.get("orchestrator.dock.expanded").is_none(),
        "A's deletion must be flushed; got: {map}"
    );
    assert_eq!(
        map["orchestrator.dock.folders"], folders,
        "A's deletion must leave B's keys intact; got: {map}"
    );
}

/// Setting per-session plugin state (what the Orchestrator does right after
/// creating a session, to tag its `project_path`) checkpoints the window, so a
/// freshly created session is in the registry the moment it is tagged — before
/// any switch or quit.
///
/// `SetWindowState` is a plugin command (`handle_plugin_command` only exists
/// with the `plugins` feature), so this test is gated to that build — the
/// min-size / no-plugins configuration has no session-tagging path to exercise.
#[cfg(feature = "plugins")]
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
        value: Some(serde_json::Value::String(
            proj_b.to_string_lossy().into_owned(),
        )),
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
