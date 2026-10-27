//! Variants of #3633's A3 — one file edited in two places. A file has one
//! buffer across an editor's workspaces, so there is one version of it to
//! keep: a second workspace opening it goes to the first's, and one restored
//! after another took the file comes back without its own copy. A second
//! editor gets the file read-only while the first has it open. And unsaved
//! work a saved workspace's layout places is left to that workspace.

use crate::common::global_state::isolated_dir_context;
use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use fresh::config_io::DirectoryContext;
use fresh::services::recovery::{entries_for_root, RecoveryEntry};
use std::path::Path;
use tempfile::TempDir;

fn hot_exit_config() -> Config {
    let mut config = Config::default();
    config.editor.hot_exit = true;
    config.editor.auto_recovery_save_interval_secs = 0;
    config
}

fn launch(dir: &Path, dir_context: &DirectoryContext) -> EditorTestHarness {
    launch_with(dir, dir_context, hot_exit_config())
}

fn launch_with(dir: &Path, dir_context: &DirectoryContext, config: Config) -> EditorTestHarness {
    EditorTestHarness::create(
        120,
        24,
        HarnessOptions::new()
            .with_config(config)
            .with_working_dir(dir.to_path_buf())
            .with_shared_dir_context(dir_context.clone())
            .with_empty_plugins_dir(),
    )
    .unwrap()
}

/// An Orchestrator daemon whose first workspace is on `dir`.
fn daemon_in(dir: &Path, dir_context: &DirectoryContext) -> EditorTestHarness {
    let mut harness = launch(dir, dir_context);
    harness.editor_mut().set_session_mode(true);
    harness
        .editor_mut()
        .set_session_name(Some(fresh::server::ORCHESTRATOR_DAEMON.into()));
    harness.startup(true, &[]).unwrap();
    harness
}

/// The text an entry holds (a small file's, whole).
fn text_of(entry: &RecoveryEntry) -> String {
    let chunk = entry
        .metadata_path
        .parent()
        .unwrap()
        .join(format!("{}.chunk.0", entry.id));
    String::from_utf8_lossy(&std::fs::read(chunk).unwrap()).into_owned()
}

/// The entries the store holds for `root` whose content holds `needle`.
fn entries_holding(
    dir_context: &DirectoryContext,
    root: &Path,
    needle: &str,
) -> Vec<RecoveryEntry> {
    entries_for_root(&dir_context.recovery_dir(), root)
        .into_iter()
        .filter(|e| text_of(e).contains(needle))
        .collect()
}

fn tick(harness: &mut EditorTestHarness) {
    harness
        .editor_mut()
        .auto_recovery_save_dirty_buffers()
        .unwrap();
    // Deletes run off the main loop
    harness.render().unwrap();
}

fn save(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
}

/// The windows of `h` with a buffer for `file`, materializing every one.
fn windows_with(h: &mut EditorTestHarness, file: &Path) -> Vec<fresh_core::WindowId> {
    h.editor_mut().materialize_all_windows();
    (1..=64)
        .map(fresh_core::WindowId)
        .filter(|id| {
            h.editor().session(*id).is_some_and(|w| {
                w.buffers
                    .iter()
                    .any(|(_, s)| s.buffer.file_path() == Some(file))
            })
        })
        .collect()
}

/// (a) Two workspaces on the *same* root (co-tenants, as Extract Tab to New
/// Workspace makes), both opening F: the second is taken to the first's
/// buffer, so their edits are one version with one entry. After a crash
/// and a reboot of every workspace on the root, F is in one of them, once,
/// with both edits.
#[test]
#[cfg(target_os = "linux")] // the pinned data dir; see `isolated_dir_context`
fn co_tenant_workspaces_on_one_root_share_one_buffer_for_a_file() {
    let sandbox = TempDir::new().unwrap();
    let (dir_context, _pin) = isolated_dir_context(sandbox.path());
    let root = sandbox.path().join("project");
    std::fs::create_dir(&root).unwrap();
    let root = root.canonicalize().unwrap();
    let file = root.join("F.txt");
    std::fs::write(&file, "base\n").unwrap();
    std::fs::write(root.join("d.txt"), "d\n").unwrap();

    {
        let mut h = launch(&root, &dir_context);
        h.startup(true, &[]).unwrap();
        let a = h.editor().active_window_id();
        // A co-tenant: Extract Tab to New Workspace on a throwaway tab
        h.open_file(&root.join("d.txt")).unwrap();
        let d = h.editor().active_buffer();
        h.editor_mut().extract_tab_to_new_workspace(d);
        let b = (1..=64)
            .map(fresh_core::WindowId)
            .find(|id| *id != a && h.editor().session(*id).is_some_and(|w| w.root == root))
            .expect("the co-tenant");

        h.editor_mut().set_active_window(a);
        h.open_file(&file).unwrap();
        h.type_text("A-EDIT ").unwrap();
        tick(&mut h);
        h.editor_mut().set_active_window(b);
        h.render().unwrap();
        h.open_file(&file).unwrap();
        assert_eq!(h.editor().active_window_id(), a, "F is open in A");
        h.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
        h.type_text(" B-EDIT").unwrap();
        tick(&mut h);

        let entries = entries_holding(&dir_context, &root, "A-EDIT");
        assert_eq!(entries.len(), 1, "one entry for F");
        assert!(text_of(&entries[0]).contains("B-EDIT"));
        assert_eq!(windows_with(&mut h, &file), vec![a]);

        // A checkpoint, then a crash
        h.editor_mut().save_all_windows_workspaces().unwrap();
    }

    // Reboot: every workspace on the root comes back, F in one of them
    let mut h = launch(&root, &dir_context);
    h.startup(true, &[]).unwrap();
    let holding = windows_with(&mut h, &file);
    assert_eq!(holding.len(), 1, "F has one buffer: {holding:?}");
    let window = h.editor().session(holding[0]).unwrap();
    let text = window
        .buffers
        .iter()
        .find(|(_, s)| s.buffer.file_path() == Some(file.as_path()))
        .and_then(|(_, s)| s.buffer.to_string())
        .unwrap();
    assert!(text.contains("A-EDIT") && text.contains("B-EDIT"), "{text}");
}

/// (b) A workspace whose saved layout has F, restored after another
/// workspace opened F, comes back without a tab of its own for it: F stays
/// where it is open.
#[test]
fn a_workspace_restored_after_another_took_a_file_leaves_it_there() {
    let temp = TempDir::new().unwrap();
    let ws_a = temp.path().join("wsA");
    let ws_b = temp.path().join("wsB");
    let shared = temp.path().join("shared");
    for dir in [&ws_a, &ws_b, &shared] {
        std::fs::create_dir(dir).unwrap();
    }
    let file = shared
        .join("F.txt")
        .canonicalize()
        .unwrap_or(shared.join("F.txt"));
    std::fs::write(&file, "base\n").unwrap();
    let file = file.canonicalize().unwrap();
    let dir_context = DirectoryContext::for_testing(temp.path());

    // B has F in its layout
    {
        let mut daemon = daemon_in(&ws_a, &dir_context);
        let b = daemon
            .editor_mut()
            .create_window_at(ws_b.clone(), "wsB".to_string());
        daemon.editor_mut().set_active_window(b);
        daemon.render().unwrap();
        daemon.open_file(&file).unwrap();
        daemon.render().unwrap();
        daemon.shutdown(true).unwrap();
    }

    // A opens F before B is shown; then B is
    let mut daemon = daemon_in(&ws_a, &dir_context);
    daemon.render().unwrap();
    daemon.open_file(&file).unwrap();
    daemon.type_text("A-EDIT ").unwrap();
    daemon.render().unwrap();
    let holding = windows_with(&mut daemon, &file);
    assert_eq!(holding.len(), 1, "F has one buffer: {holding:?}");
    let text = daemon
        .editor()
        .session(holding[0])
        .unwrap()
        .buffers
        .iter()
        .find(|(_, s)| s.buffer.file_path() == Some(file.as_path()))
        .and_then(|(_, s)| s.buffer.to_string())
        .unwrap();
    assert!(
        text.contains("A-EDIT"),
        "the edit is in F's one buffer: {text}"
    );
}

/// (c) Two editors at once on one project — a direct launch and the
/// Orchestrator. The second to open F gets it read-only while the first
/// has it open, so there is never a second version of it to keep.
#[test]
fn a_second_editor_gets_a_file_the_first_has_open_read_only() {
    let temp = TempDir::new().unwrap();
    let root = temp.path().join("project");
    std::fs::create_dir(&root).unwrap();
    let file = root.join("F.txt");
    std::fs::write(&file, "base\n").unwrap();
    let dir_context = DirectoryContext::for_testing(temp.path());

    let mut direct = launch(&root, &dir_context);
    direct.startup(true, &[]).unwrap();
    direct.open_file(&file).unwrap();
    direct.type_text("D-EDIT ").unwrap();
    tick(&mut direct);

    {
        let mut daemon = daemon_in(&root, &dir_context);
        daemon.open_file(&file).unwrap();
        daemon.render().unwrap();
        daemon.assert_screen_contains("read-only");
        daemon.type_text("O-EDIT ").unwrap();
        tick(&mut daemon);
        assert!(
            entries_holding(&dir_context, &root, "O-EDIT").is_empty(),
            "the second editor changed a file the first has open"
        );
        daemon.shutdown(true).unwrap();
    }

    // Once the first lets go, a later editor gets it writable — and its work
    save(&mut direct);
    direct.shutdown(true).unwrap();
    drop(direct);
    let mut daemon = daemon_in(&root, &dir_context);
    daemon.open_file(&file).unwrap();
    daemon.type_text("LATER ").unwrap();
    daemon.render().unwrap();
    daemon.assert_screen_contains("LATER");
}

/// Workspace B on a root has a saved workspace file whose layout places an
/// unnamed buffer, and that buffer's unsaved work left in the store when its
/// window closed. A new window on the same root — a co-tenant from Extract
/// Tab to New Workspace — must leave that work for B. A `--no-restore`
/// launch on the root, which stands in for every workspace there, does bring
/// it back, and its exit keeps it.
#[test]
#[cfg(target_os = "linux")] // the pinned data dir; see `isolated_dir_context`
fn a_new_window_leaves_a_saved_co_tenants_work_to_it() {
    let sandbox = TempDir::new().unwrap();
    let (dir_context, _pin) = isolated_dir_context(sandbox.path());
    let root = sandbox.path().join("project");
    std::fs::create_dir(&root).unwrap();
    let root = root.canonicalize().unwrap();
    for name in ["d.txt", "e.txt"] {
        std::fs::write(root.join(name), "x\n").unwrap();
    }
    let free_entry_of_b = |_b: &str| {
        let entries = entries_holding(&dir_context, &root, "B-UNSAVED");
        assert_eq!(entries.len(), 1, "B's work is in the store, once");
        assert!(
            !entries[0]
                .metadata_path
                .to_string_lossy()
                .contains("/held/"),
            "B's work is held by an editor: {:?}",
            entries[0].metadata_path
        );
    };

    let b_id = {
        let mut h = launch(&root, &dir_context);
        h.startup(true, &[]).unwrap();
        let a = h.editor().active_window_id();
        let co_tenant = |h: &EditorTestHarness, but: &[fresh_core::WindowId]| {
            (1..=64)
                .map(fresh_core::WindowId)
                .find(|id| {
                    !but.contains(id) && h.editor().session(*id).is_some_and(|w| w.root == root)
                })
                .expect("the co-tenant")
        };

        // Workspace B, with unsaved work, saved and then closed
        h.open_file(&root.join("d.txt")).unwrap();
        let d = h.editor().active_buffer();
        h.editor_mut().extract_tab_to_new_workspace(d);
        let b = co_tenant(&h, &[a]);
        h.editor_mut().set_active_window(b);
        h.render().unwrap();
        h.new_buffer().unwrap();
        h.type_text("B-UNSAVED").unwrap();
        tick(&mut h);
        h.editor_mut().save_all_windows_workspaces().unwrap();
        let b_id = h.editor().session(b).unwrap().stable_id.clone();
        h.editor_mut().set_active_window(a);
        h.render().unwrap();
        assert!(h.editor_mut().close_window(b));
        free_entry_of_b(&b_id);

        // A new co-tenant D, switched to, and A switched back to
        h.open_file(&root.join("e.txt")).unwrap();
        let e = h.editor().active_buffer();
        h.editor_mut().extract_tab_to_new_workspace(e);
        let new = co_tenant(&h, &[a, b]);
        h.editor_mut().set_active_window(new);
        h.render().unwrap();
        h.assert_screen_not_contains("B-UNSAVED");
        h.editor_mut().set_active_window(a);
        h.render().unwrap();
        h.assert_screen_not_contains("B-UNSAVED");
        free_entry_of_b(&b_id);
        h.shutdown(true).unwrap();
        b_id
    };
    free_entry_of_b(&b_id);

    // `--no-restore`: brings it back, still B's. Where B's window is open
    // again in this launch, the work is left to it and shows there.
    let mut config = hot_exit_config();
    config.editor.restore_previous_session = false;
    let mut h = launch_with(&root, &dir_context, config);
    h.startup(true, &[]).unwrap();
    h.render().unwrap();
    if !h.screen_to_string().contains("B-UNSAVED") {
        let b = (1..=64)
            .map(fresh_core::WindowId)
            .find(|id| h.editor().session(*id).is_some_and(|w| w.stable_id == b_id))
            .expect("B's work is on screen, or B's window is open to take it");
        h.editor_mut().set_active_window(b);
        h.render().unwrap();
    }
    h.assert_screen_contains("B-UNSAVED");
    h.shutdown(true).unwrap();
    drop(h);
    free_entry_of_b(&b_id);
}
