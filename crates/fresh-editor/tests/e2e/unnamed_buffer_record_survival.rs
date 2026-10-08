//! A session that cannot account for a workspace's unnamed buffers must not
//! destroy their records.
//!
//! The record (the `unnamed_buffers` list in the workspace file) is the only
//! pointer to content the recovery store holds. A save rebuilds that list from
//! the buffers that are *live*, so any session that did not restore them
//! writes the pointer away while the content stays on disk, unreachable.
//!
//! #3475 is one way into that state — the entry lives in a store this editor
//! cannot see. These are the others, each measured against a live editor
//! before being written down:
//!
//! 1. `hot_exit` was off for one session.
//! 2. The recovery store could not be listed (a transient I/O failure).
//! 3. The workspace file could not be parsed, so a *new* workspace identity
//!    was minted beside it and the old record abandoned.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use fresh::config::Config;
use fresh::config_io::DirectoryContext;
use fresh::workspace::Workspace;
use std::path::Path;
use tempfile::TempDir;

const MARK: &str = "UNSAVED_KEEP_ME";

/// A harness over `dir`, sharing `dir_context` so every launch is the same
/// user and installation in a different (or the same) working directory.
fn harness_in(
    dir: &Path,
    dir_context: &DirectoryContext,
    hot_exit: bool,
) -> anyhow::Result<EditorTestHarness> {
    let mut config = Config::default();
    config.editor.hot_exit = hot_exit;
    EditorTestHarness::create(
        100,
        24,
        HarnessOptions::new()
            .with_config(config)
            .with_working_dir(dir.to_path_buf())
            .with_shared_dir_context(dir_context.clone())
            .without_empty_plugins_dir(),
    )
}

/// The recovery ids `root`'s workspace file currently records.
fn recorded_ids(root: &Path) -> Vec<String> {
    Workspace::load(root)
        .ok()
        .flatten()
        .map(|w| {
            w.unnamed_buffers
                .iter()
                .map(|r| r.recovery_id.clone())
                .collect()
        })
        .unwrap_or_default()
}

/// Leave `project` holding one unsaved, never-named buffer, then exit cleanly.
/// Returns the recovery id the workspace file now points at.
fn seed_unsaved_buffer(project: &Path, dir_context: &DirectoryContext) -> String {
    let mut harness = harness_in(project, dir_context, true).unwrap();
    harness.startup(true, &[]).unwrap();
    harness.new_buffer().unwrap();
    harness.type_text(MARK).unwrap();
    harness.render().unwrap();
    harness.shutdown(true).unwrap();

    let ids = recorded_ids(project);
    assert_eq!(
        ids.len(),
        1,
        "the seed must leave exactly one unnamed-buffer record to protect"
    );
    ids.into_iter().next().unwrap()
}

/// The one directory the standalone recovery store keeps `root`'s entries in.
fn store_dir(dir_context: &DirectoryContext) -> std::path::PathBuf {
    let default_dir = dir_context.recovery_dir().join("default");
    let mut subdirs: Vec<_> = std::fs::read_dir(&default_dir)
        .unwrap_or_else(|e| panic!("no recovery store under {}: {e}", default_dir.display()))
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .filter(|p| p.is_dir())
        .collect();
    assert_eq!(
        subdirs.len(),
        1,
        "expected exactly one standalone store, found {subdirs:?}"
    );
    subdirs.pop().unwrap()
}

/// Turning `hot_exit` off for one session must not cost the record.
///
/// Restore returns early when the setting is off, so nothing reads the list —
/// and the save writes it back from the live buffers, which is empty. One
/// visit with the setting off and the pointer is gone for good, while the
/// content sits in the store with nothing naming it.
#[test]
fn a_session_with_hot_exit_off_keeps_the_record() {
    let temp = TempDir::new().unwrap();
    let project = temp.path().join("project");
    std::fs::create_dir(&project).unwrap();
    let dir_context = DirectoryContext::for_testing(temp.path());
    std::fs::create_dir_all(dir_context.workspaces_dir()).unwrap();

    let id = seed_unsaved_buffer(&project, &dir_context);

    // One session with the setting off: open the project and leave.
    {
        let mut harness = harness_in(&project, &dir_context, false).unwrap();
        harness.startup(true, &[]).unwrap();
        harness.render().unwrap();
        harness.shutdown(true).unwrap();
    }

    assert_eq!(
        recorded_ids(&project),
        vec![id],
        "a session with hot_exit off did not read the unnamed-buffer record, \
         so it must not rewrite it"
    );

    // And with the setting back on, the buffer must still come back.
    let mut harness = harness_in(&project, &dir_context, true).unwrap();
    harness.startup(true, &[]).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains(MARK);
}

/// A store this session could not list must not be read as an empty store.
///
/// `list_recoverable` failing is indistinguishable, today, from the entry not
/// being there: both drop the reference. But a directory that cannot be read
/// is a transient fault — the content is still in it — so the record has to
/// survive until a session can actually look.
#[test]
fn a_session_that_cannot_list_the_store_keeps_the_record() {
    let temp = TempDir::new().unwrap();
    let project = temp.path().join("project");
    std::fs::create_dir(&project).unwrap();
    let dir_context = DirectoryContext::for_testing(temp.path());
    std::fs::create_dir_all(dir_context.workspaces_dir()).unwrap();

    let id = seed_unsaved_buffer(&project, &dir_context);

    // Make the store unlistable without destroying it: move it aside and put
    // a regular file in its place, so `read_dir` fails with ENOTDIR.
    let store = store_dir(&dir_context);
    let aside = store.with_extension("aside");
    std::fs::rename(&store, &aside).unwrap();
    std::fs::write(&store, b"not a directory").unwrap();

    {
        let mut harness = harness_in(&project, &dir_context, true).unwrap();
        // The session lock cannot be written either, and `startup` reports
        // that. A real editor logs it and carries on (measured against
        // `fresh --web`: the run came up and warned `Failed to list recovery
        // entries: Not a directory`), so this must not stop the test.
        let _ = harness.startup(true, &[]);
        harness.render().unwrap();
        harness.shutdown(true).unwrap();
    }

    // Put the store back: the content was never gone.
    std::fs::remove_file(&store).unwrap();
    std::fs::rename(&aside, &store).unwrap();

    assert_eq!(
        recorded_ids(&project),
        vec![id],
        "a session that could not list the recovery store has no grounds to \
         decide the content is gone"
    );

    let mut harness = harness_in(&project, &dir_context, true).unwrap();
    harness.startup(true, &[]).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains(MARK);
}

/// A workspace file that cannot be parsed must not be abandoned.
///
/// Today the restore error is logged and a fresh `stable_id` is minted, so the
/// window writes a *second* workspace file for the same root and the old
/// record is orphaned. The recovery entry is orphaned with it: its
/// `workspace_id` names an identity no window holds any more, so adoption
/// cannot reclaim it either, and the content is unreachable for good.
#[test]
fn a_workspace_file_that_cannot_be_parsed_is_not_abandoned() {
    let temp = TempDir::new().unwrap();
    let project = temp.path().join("project");
    std::fs::create_dir(&project).unwrap();
    let dir_context = DirectoryContext::for_testing(temp.path());
    std::fs::create_dir_all(dir_context.workspaces_dir()).unwrap();

    seed_unsaved_buffer(&project, &dir_context);

    // `Workspace::save` names its file from the process-wide data dir
    // (`get_workspaces_dir`), which every test in this binary shares, so
    // count only the files whose encoded-root prefix is this project's.
    let canonical = project.canonicalize().unwrap_or_else(|_| project.clone());
    let prefix = format!(
        "{}.",
        fresh::workspace::encode_path_for_filename(&canonical)
    );
    let workspace_files = move || -> Vec<std::path::PathBuf> {
        let dir = fresh::workspace::get_workspaces_dir().unwrap();
        let mut v: Vec<_> = std::fs::read_dir(dir)
            .unwrap()
            .filter_map(|e| e.ok())
            .map(|e| e.path())
            .filter(|p| p.extension().is_some_and(|e| e == "json"))
            .filter(|p| {
                p.file_name()
                    .and_then(|n| n.to_str())
                    .is_some_and(|n| n.starts_with(&prefix))
            })
            .collect();
        v.sort();
        v
    };
    let before = workspace_files();
    assert_eq!(before.len(), 1, "one workspace file after the seed");

    // Corrupt it, keeping the name — the name is where the identity lives.
    std::fs::write(&before[0], b"{ not valid json").unwrap();

    {
        let mut harness = harness_in(&project, &dir_context, true).unwrap();
        harness.startup(true, &[]).unwrap();
        harness.render().unwrap();
        harness.shutdown(true).unwrap();
    }

    assert_eq!(
        workspace_files().len(),
        1,
        "a workspace whose file could not be parsed must keep its identity \
         rather than minting a second file for the same root: {:?}",
        workspace_files()
    );

    // The unsaved content must still be reachable.
    let mut harness = harness_in(&project, &dir_context, true).unwrap();
    harness.startup(true, &[]).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains(MARK);
}
