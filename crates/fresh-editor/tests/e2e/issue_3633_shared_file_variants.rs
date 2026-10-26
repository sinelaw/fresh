//! Variants of #3633's A3 — one file edited in two places — that a per-path
//! entry id got wrong whatever the store's layout: two workspaces on the
//! same root, two editors at once, and a plain save instead of Save As.
//! Each place keeps its own entry, and one's save or exit never touches the
//! other's.

use crate::common::global_state::isolated_dir_context;
use crate::common::harness::{layout, EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use fresh::config_io::DirectoryContext;
use fresh::services::recovery::{entries_under, partition_dir, RecoveryEntry};
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
    entries_under(&partition_dir(&dir_context.recovery_dir(), root))
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

fn save_as(harness: &mut EditorTestHarness, path: &Path) {
    harness
        .editor_mut()
        .dispatch_action_for_tests(fresh::input::keybindings::Action::SaveAs);
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text(path.to_str().unwrap()).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// (a) Two workspaces on the *same* root (co-tenants, as Extract Tab to New
/// Workspace makes), each editing F. They share a store directory, so only
/// their entry ids keep them apart. A's Save As leaves B's entry alone, and
/// after a crash and a reboot each workspace gets its own work back — B's
/// edit in B, nothing of it in A.
#[test]
#[cfg(target_os = "linux")] // the pinned data dir; see `isolated_dir_context`
fn co_tenant_workspaces_on_one_root_keep_their_own_work() {
    let sandbox = TempDir::new().unwrap();
    let (dir_context, _pin) = isolated_dir_context(sandbox.path());
    let root = sandbox.path().join("project");
    std::fs::create_dir(&root).unwrap();
    let root = root.canonicalize().unwrap();
    let file = root.join("F.txt");
    std::fs::write(&file, "base\n").unwrap();
    std::fs::write(root.join("d.txt"), "d\n").unwrap();

    let (a_id, b_id) = {
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
        h.type_text("B-EDIT ").unwrap();
        tick(&mut h);

        let a_entries = entries_holding(&dir_context, &root, "A-EDIT");
        let b_entries = entries_holding(&dir_context, &root, "B-EDIT");
        assert_eq!(a_entries.len(), 1);
        assert_eq!(b_entries.len(), 1);
        assert_ne!(a_entries[0].id, b_entries[0].id);

        h.editor_mut().set_active_window(a);
        h.render().unwrap();
        save_as(&mut h, &root.join("G.txt"));
        tick(&mut h);
        assert!(entries_holding(&dir_context, &root, "A-EDIT").is_empty());
        let b_entries = entries_holding(&dir_context, &root, "B-EDIT");
        assert_eq!(b_entries.len(), 1, "A's Save As took B's entry");
        assert!(b_entries[0].metadata.saving.is_none());

        // A checkpoint, then a crash
        h.editor_mut().save_all_windows_workspaces().unwrap();
        (
            h.editor().session(a).unwrap().stable_id.clone(),
            h.editor().session(b).unwrap().stable_id.clone(),
        )
    };

    // Reboot: every workspace on the root comes back
    let mut h = launch(&root, &dir_context);
    h.startup(true, &[]).unwrap();
    h.editor_mut().materialize_all_windows();
    let mut holding_b_edit = Vec::new();
    for id in (1..=64).map(fresh_core::WindowId) {
        let Some(window) = h.editor().session(id) else {
            continue;
        };
        let has = window
            .buffers
            .iter()
            .any(|(_, s)| s.buffer.to_string().is_some_and(|t| t.contains("B-EDIT")));
        if has {
            holding_b_edit.push(window.stable_id.clone());
        }
        assert!(
            !window.buffers.iter().any(|(_, s)| s
                .buffer
                .to_string()
                .is_some_and(|t| t.contains("A-EDIT ") && s.buffer.is_modified())),
            "A's saved work came back as unsaved"
        );
    }
    assert_eq!(
        holding_b_edit,
        vec![b_id],
        "B's edit must come back in B, once, and not in A ({a_id})"
    );
}

/// (b) Two editors at once on one project — a direct launch and the
/// Orchestrator — both editing F. Each has its own entry; the direct
/// launch's save and clean exit leave the daemon's alone, and after the
/// daemon crashes its edit comes back.
#[test]
fn two_editors_on_one_file_keep_their_own_work() {
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
        daemon.type_text("O-EDIT ").unwrap();
        tick(&mut daemon);

        let d_entries = entries_holding(&dir_context, &root, "D-EDIT");
        let o_entries = entries_holding(&dir_context, &root, "O-EDIT");
        assert_eq!(d_entries.len(), 1);
        assert_eq!(o_entries.len(), 1);
        assert_ne!(d_entries[0].id, o_entries[0].id);

        save(&mut direct);
        assert!(std::fs::read_to_string(&file).unwrap().contains("D-EDIT"));
        tick(&mut direct);
        let o_entries = entries_holding(&dir_context, &root, "O-EDIT");
        assert_eq!(
            o_entries.len(),
            1,
            "a save in one editor took the other's entry"
        );
        assert!(o_entries[0].metadata.saving.is_none());

        direct.shutdown(true).unwrap();
        drop(direct);
        assert_eq!(
            entries_holding(&dir_context, &root, "O-EDIT").len(),
            1,
            "one editor's exit deleted the other's entry"
        );
        // The daemon is killed
    }

    let mut daemon = daemon_in(&root, &dir_context);
    daemon.render().unwrap();
    daemon.assert_screen_contains("O-EDIT");
}

/// (c) The report's two workspaces with a plain save in A instead of Save
/// As: F itself is written with A's work. B's entry is untouched by it —
/// not deleted, not marked saved — and B gets its edit back after a crash,
/// while A never shows it.
#[test]
fn a_plain_save_in_one_workspace_leaves_the_others_work() {
    let temp = TempDir::new().unwrap();
    let ws_a = temp.path().join("wsA");
    let ws_b = temp.path().join("wsB");
    let shared = temp.path().join("shared");
    for dir in [&ws_a, &ws_b, &shared] {
        std::fs::create_dir(dir).unwrap();
    }
    let file = shared.join("F.txt");
    std::fs::write(&file, "base\n").unwrap();
    let dir_context = DirectoryContext::for_testing(temp.path());

    {
        let mut daemon = daemon_in(&ws_a, &dir_context);
        let a = daemon.editor().active_window_id();
        daemon.open_file(&file).unwrap();
        daemon.type_text("A-EDIT ").unwrap();
        tick(&mut daemon);
        let b = daemon
            .editor_mut()
            .create_window_at(ws_b.clone(), "wsB".to_string());
        daemon.editor_mut().set_active_window(b);
        daemon.render().unwrap();
        daemon.open_file(&file).unwrap();
        daemon.type_text("B-EDIT ").unwrap();
        tick(&mut daemon);

        daemon.editor_mut().set_active_window(a);
        daemon.render().unwrap();
        save(&mut daemon);
        assert!(std::fs::read_to_string(&file).unwrap().contains("A-EDIT"));
        tick(&mut daemon);
        assert!(entries_holding(&dir_context, &ws_a, "A-EDIT").is_empty());
        let b_entries = entries_holding(&dir_context, &ws_b, "B-EDIT");
        assert_eq!(b_entries.len(), 1, "A's save took B's entry");
        assert!(
            b_entries[0].metadata.saving.is_none(),
            "A's save was recorded in B's entry"
        );
        // Killed
    }

    {
        let mut a = daemon_in(&ws_a, &dir_context);
        a.render().unwrap();
        a.assert_screen_not_contains("B-EDIT");
        a.shutdown(true).unwrap();
    }
    let mut b = daemon_in(&ws_b, &dir_context);
    b.render().unwrap();
    b.assert_screen_contains("B-EDIT");
    // F changed on disk since B's backup, so B's version may come back as a
    // copy beside it; either way, once
    let tabs = b.screen_row_text(layout::TAB_BAR_ROW as u16);
    assert_eq!(tabs.matches('*').count(), 1, "{tabs}");
}

/// Workspace B on a root has a saved workspace file and unsaved work it left
/// in the store when its window closed. A new window on the same root — a
/// co-tenant from Extract Tab to New Workspace — must leave that work for B,
/// not take it and stamp it as its own. A `--no-restore` launch on the root,
/// which stands in for every workspace there, does bring it back, but leaves
/// it B's.
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
    let free_entry_of_b = |stamp: &str| {
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
        assert_eq!(entries[0].metadata.workspace_id.as_deref(), Some(stamp));
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

    // `--no-restore`: brings it back, still B's
    let mut config = hot_exit_config();
    config.editor.restore_previous_session = false;
    let mut h = launch_with(&root, &dir_context, config);
    h.startup(true, &[]).unwrap();
    h.render().unwrap();
    h.assert_screen_contains("B-UNSAVED");
    h.shutdown(true).unwrap();
    drop(h);
    free_entry_of_b(&b_id);
}
