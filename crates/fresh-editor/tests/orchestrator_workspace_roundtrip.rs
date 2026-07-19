//! Property tests for workspace capture → restore round-tripping
//! (issue #2056, Stage 1 safety net).
//!
//! The invariant: saving a window's workspace and restoring it into a
//! fresh editor reproduces the same set of open files. This pins the
//! "faithful per-window restore" half of the spec and de-risks the
//! Stage 1 refactor (moving capture/restore onto `Window`): the
//! observable round-trip behavior must not change.
//!
//! Also hosts the [`stable_id`] suite for durable workspace identity.

use fresh::config::Config;
use fresh::config_io::DirectoryContext;
use fresh::model::filesystem::StdFileSystem;
use proptest::prelude::*;
use std::collections::BTreeSet;
use std::path::{Path, PathBuf};
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

/// Open file paths in the active window, as a sorted set.
fn open_paths(editor: &fresh::app::Editor) -> BTreeSet<PathBuf> {
    editor.active_window().buffers.paths().into_iter().collect()
}

proptest! {
    #![proptest_config(ProptestConfig { cases: 40, ..ProptestConfig::default() })]

    /// Any set of opened files round-trips through save → restore.
    #[test]
    fn open_files_roundtrip_through_workspace(
        names in prop::collection::btree_set("[a-z][a-z0-9]{2,7}", 1..=5),
        // a few external (out-of-project) files exercise the
        // `external_files` path specifically.
        ext_names in prop::collection::btree_set("[a-z][a-z0-9]{2,7}", 0..=3),
    ) {
        let sandbox = tempfile::tempdir().unwrap();
        let project = sandbox.path().join("project");
        let external = sandbox.path().join("external");
        let data_home = sandbox.path().join("data-home");
        std::fs::create_dir_all(&project).unwrap();
        std::fs::create_dir_all(&external).unwrap();
        std::fs::create_dir_all(&data_home).unwrap();
        let project = project.canonicalize().unwrap();
        let external = external.canonicalize().unwrap();

        // Materialize the files and collect their absolute paths.
        let mut expected: BTreeSet<PathBuf> = BTreeSet::new();
        for n in &names {
            let p = project.join(format!("{n}.txt"));
            std::fs::write(&p, "x").unwrap();
            expected.insert(p);
        }
        for n in &ext_names {
            let p = external.join(format!("{n}.txt"));
            std::fs::write(&p, "y").unwrap();
            expected.insert(p);
        }

        let dir_context = DirectoryContext::for_testing(&data_home);

        // editor 1: open every file, then save the workspace.
        {
            let mut e1 = editor_in(&project, &dir_context);
            for p in &expected {
                e1.open_file(p).unwrap();
            }
            // The opened set should be exactly `expected` (plus possibly
            // the initial [No Name] scratch, which has no path so isn't
            // in paths()).
            prop_assert_eq!(open_paths(&e1), expected.clone());
            e1.save_workspace().unwrap();
        }

        // editor 2: fresh editor in the same project + data dir; restore.
        let mut e2 = editor_in(&project, &dir_context);
        let restored = e2.try_restore_workspace().unwrap();
        prop_assert!(restored, "a workspace was saved, so restore must report success");

        prop_assert_eq!(
            open_paths(&e2),
            expected,
            "restored window must reopen exactly the files that were saved"
        );
    }
}

/// Durable workspace identity (`stable_id`).
///
/// A workspace's on-disk snapshot is keyed by an identity minted once at
/// window creation — `workspaces/<encoded-root>.<stable_id>.json` — with
/// the encoded root serving only as a filename-level locator. These tests
/// pin the identity lifecycle: it survives save → restore cycles without
/// spawning sibling files, legacy root-keyed files (no id) are adopted and
/// re-keyed on the next save, duplicate files claiming one directory
/// resolve to the freshest snapshot, and delete removes every variant.
///
/// (Lives in this binary rather than its own `tests/*.rs` target: each
/// integration-test binary links a full editor executable, which is real
/// CI disk and link time.)
mod stable_id {
    use fresh::config::Config;
    use fresh::config_io::DirectoryContext;
    use fresh::model::filesystem::StdFileSystem;
    use fresh::workspace::{
        encode_path_for_filename, find_workspace_file_by_root, get_workspaces_dir, Workspace,
    };
    use std::path::{Path, PathBuf};
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

    /// All workspace files in the (process-global) workspaces dir whose name
    /// starts with `project`'s encoded root. Unique temp roots per test keep
    /// parallel tests from seeing each other's files.
    fn files_for_root(project: &Path) -> Vec<PathBuf> {
        let canonical = project.canonicalize().unwrap();
        let prefix = encode_path_for_filename(&canonical);
        let dir = get_workspaces_dir().unwrap();
        match std::fs::read_dir(dir) {
            Ok(entries) => entries
                .flatten()
                .map(|e| e.path())
                .filter(|p| {
                    p.file_name()
                        .and_then(|n| n.to_str())
                        .is_some_and(|n| n.starts_with(&*prefix) && n.ends_with(".json"))
                })
                .collect(),
            Err(_) => Vec::new(),
        }
    }

    fn read_stable_id(path: &Path) -> Option<String> {
        let val: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(path).ok()?).ok()?;
        val.get("stable_id")
            .and_then(|v| v.as_str())
            .map(str::to_owned)
    }

    #[test]
    fn stable_id_survives_restart_without_file_proliferation() {
        let sandbox = tempfile::tempdir().unwrap();
        let project = sandbox.path().join("project");
        std::fs::create_dir_all(&project).unwrap();
        let project = project.canonicalize().unwrap();
        std::fs::write(project.join("a.txt"), "x").unwrap();
        let dir_context = DirectoryContext::for_testing(&sandbox.path().join("data-home"));

        // Editor 1: open a file, save twice (a checkpoint plus a quit save).
        let first_id;
        {
            let mut e1 = editor_in(&project, &dir_context);
            e1.open_file(&project.join("a.txt")).unwrap();
            e1.save_workspace().unwrap();
            e1.save_workspace().unwrap();
            let files = files_for_root(&project);
            assert_eq!(files.len(), 1, "repeated saves reuse one file: {files:?}");
            first_id = read_stable_id(&files[0]).expect("saved workspace carries a stable_id");
        }

        // Editor 2 ("restart"): restore, then save again — the identity must
        // continue, not fork a sibling file with a freshly minted id.
        let mut e2 = editor_in(&project, &dir_context);
        assert!(e2.try_restore_workspace().unwrap());
        assert_eq!(
            e2.active_window().stable_id,
            first_id,
            "the restored window adopts the persisted identity"
        );
        e2.save_workspace().unwrap();

        let files = files_for_root(&project);
        assert_eq!(
            files.len(),
            1,
            "a restart must not mint a sibling workspace file: {files:?}"
        );
        assert_eq!(
            read_stable_id(&files[0]).as_deref(),
            Some(first_id.as_str())
        );
    }

    #[test]
    fn legacy_root_keyed_file_is_adopted_and_rekeyed_on_save() {
        let sandbox = tempfile::tempdir().unwrap();
        let project = sandbox.path().join("project");
        std::fs::create_dir_all(&project).unwrap();
        let project = project.canonicalize().unwrap();
        std::fs::write(project.join("legacy.txt"), "x").unwrap();
        let dir_context = DirectoryContext::for_testing(&sandbox.path().join("data-home"));

        // A pre-stable-id workspace file at the legacy root-keyed name,
        // produced by capturing a real layout and stripping the id.
        let legacy_path = {
            let mut e = editor_in(&project, &dir_context);
            e.open_file(&project.join("legacy.txt")).unwrap();
            let mut ws = e.capture_workspace();
            ws.stable_id = None;
            ws.label = Some("legacy-label".to_string());
            ws.save().unwrap();
            let files = files_for_root(&project);
            assert_eq!(files.len(), 1);
            assert_eq!(
                files[0],
                fresh::workspace::get_workspace_path(&project).unwrap(),
                "an id-less snapshot lands at the legacy root-keyed name"
            );
            files[0].clone()
        };

        // A fresh editor finds the legacy file by root...
        let mut e = editor_in(&project, &dir_context);
        assert!(e.try_restore_workspace().unwrap());
        let adopted = e.active_window().stable_id.clone();
        assert!(!adopted.is_empty());

        // ...and the next save re-keys it: the id-keyed file appears, the
        // legacy file is retired, and lookup keeps resolving.
        e.save_workspace().unwrap();
        assert!(
            !legacy_path.exists(),
            "the superseded legacy file is retired on save"
        );
        let files = files_for_root(&project);
        assert_eq!(
            files.len(),
            1,
            "exactly the re-keyed file remains: {files:?}"
        );
        assert_eq!(read_stable_id(&files[0]).as_deref(), Some(adopted.as_str()));

        let loaded = Workspace::load(&project)
            .unwrap()
            .expect("loadable by root");
        assert_eq!(
            loaded.stable_id.as_deref(),
            Some(adopted.as_str()),
            "the re-keyed snapshot carries the adopted identity"
        );
    }

    #[test]
    fn duplicate_files_for_one_root_resolve_to_freshest_snapshot() {
        let sandbox = tempfile::tempdir().unwrap();
        let project = sandbox.path().join("project");
        std::fs::create_dir_all(&project).unwrap();
        let project = project.canonicalize().unwrap();

        let dir = get_workspaces_dir().unwrap();
        std::fs::create_dir_all(&dir).unwrap();
        let encoded = encode_path_for_filename(&project);

        let mut old = Workspace::new(project.clone());
        old.label = Some("old".to_string());
        old.saved_at = 100;
        std::fs::write(
            dir.join(format!("{encoded}.json")),
            serde_json::to_vec(&old).unwrap(),
        )
        .unwrap();

        let mut new = Workspace::new(project.clone());
        new.label = Some("new".to_string());
        new.saved_at = 200;
        new.stable_id = Some("ws-test-1".to_string());
        std::fs::write(
            dir.join(format!("{encoded}.ws-test-1.json")),
            serde_json::to_vec(&new).unwrap(),
        )
        .unwrap();

        let loaded = Workspace::load(&project).unwrap().expect("resolvable");
        assert_eq!(
            loaded.label.as_deref(),
            Some("new"),
            "arbitration picks the freshest snapshot, not the stale legacy file"
        );

        // Deleting the workspace removes every variant, so a killed workspace
        // can't resurrect from the stale duplicate.
        Workspace::delete(&project).unwrap();
        assert!(files_for_root(&project).is_empty());
        assert!(find_workspace_file_by_root(&project).unwrap().is_none());
    }

    /// An encoded root that is a *prefix* of another's (`/a` vs `/a.b`) must
    /// not cross-match: lookup verifies the `working_dir` recorded inside each
    /// candidate, so `a.b`'s files never count for `a`.
    #[test]
    fn prefix_colliding_roots_do_not_cross_match() {
        let sandbox = tempfile::tempdir().unwrap();
        let short = sandbox.path().join("proj");
        let long = sandbox.path().join("proj.aux");
        std::fs::create_dir_all(&short).unwrap();
        std::fs::create_dir_all(&long).unwrap();
        let short = short.canonicalize().unwrap();
        let long = long.canonicalize().unwrap();

        let mut ws_long = Workspace::new(long.clone());
        ws_long.label = Some("long-root".to_string());
        ws_long.stable_id = Some("ws-test-long".to_string());
        ws_long.save().unwrap();

        assert!(
            Workspace::load(&short).unwrap().is_none(),
            "the short root must not resolve to the longer root's file"
        );
        let loaded = Workspace::load(&long).unwrap().expect("long root loads");
        assert_eq!(loaded.label.as_deref(), Some("long-root"));
    }

    /// `save` garbage-collects every other file claiming the same root — a
    /// stale id-keyed sibling left by a prior window, plus the legacy
    /// root-keyed file — so shadowed snapshots don't accumulate on disk. Reads
    /// only arbitrate the winner; without this GC the losers would linger.
    #[test]
    fn save_retires_stale_duplicate_siblings() {
        let sandbox = tempfile::tempdir().unwrap();
        let project = sandbox.path().join("project");
        std::fs::create_dir_all(&project).unwrap();
        let project = project.canonicalize().unwrap();

        let dir = get_workspaces_dir().unwrap();
        std::fs::create_dir_all(&dir).unwrap();
        let encoded = encode_path_for_filename(&project);

        // A stale id-keyed file from a prior window at this root...
        let mut stale = Workspace::new(project.clone());
        stale.label = Some("stale".to_string());
        stale.saved_at = 100;
        stale.stable_id = Some("ws-stale".to_string());
        std::fs::write(
            dir.join(format!("{encoded}.ws-stale.json")),
            serde_json::to_vec(&stale).unwrap(),
        )
        .unwrap();
        // ...and a legacy root-keyed file for the same root.
        let mut legacy = Workspace::new(project.clone());
        legacy.label = Some("legacy".to_string());
        legacy.saved_at = 50;
        std::fs::write(
            dir.join(format!("{encoded}.json")),
            serde_json::to_vec(&legacy).unwrap(),
        )
        .unwrap();
        assert_eq!(files_for_root(&project).len(), 2);

        // A newer window saves under its own id.
        let mut current = Workspace::new(project.clone());
        current.label = Some("current".to_string());
        current.saved_at = 200;
        current.stable_id = Some("ws-current".to_string());
        current.save().unwrap();

        // Only the just-saved id-keyed file survives; the duplicates are gone.
        let files = files_for_root(&project);
        assert_eq!(
            files.len(),
            1,
            "stale duplicates must be retired on save: {files:?}"
        );
        assert_eq!(read_stable_id(&files[0]).as_deref(), Some("ws-current"));

        Workspace::delete(&project).unwrap();
    }

    /// `delete` removes *every* file claiming the root — id-keyed and legacy —
    /// so a killed workspace can't resurrect from a surviving duplicate.
    #[test]
    fn delete_removes_every_variant() {
        let sandbox = tempfile::tempdir().unwrap();
        let project = sandbox.path().join("project");
        std::fs::create_dir_all(&project).unwrap();
        let project = project.canonicalize().unwrap();

        let dir = get_workspaces_dir().unwrap();
        std::fs::create_dir_all(&dir).unwrap();
        let encoded = encode_path_for_filename(&project);

        let variants: [(String, Option<&str>, u64); 3] = [
            (format!("{encoded}.json"), None, 50),
            (format!("{encoded}.ws-a.json"), Some("ws-a"), 100),
            (format!("{encoded}.ws-b.json"), Some("ws-b"), 200),
        ];
        for (name, id, at) in &variants {
            let mut ws = Workspace::new(project.clone());
            ws.saved_at = *at;
            ws.stable_id = id.map(str::to_string);
            std::fs::write(dir.join(name), serde_json::to_vec(&ws).unwrap()).unwrap();
        }
        assert_eq!(files_for_root(&project).len(), 3);

        Workspace::delete(&project).unwrap();

        assert!(
            files_for_root(&project).is_empty(),
            "delete must remove every variant claiming the root"
        );
        assert!(find_workspace_file_by_root(&project).unwrap().is_none());
    }
}
