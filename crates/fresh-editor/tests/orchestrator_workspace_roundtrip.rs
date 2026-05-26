//! Property tests for workspace capture → restore round-tripping
//! (issue #2056, Stage 1 safety net).
//!
//! The invariant: saving a window's workspace and restoring it into a
//! fresh editor reproduces the same set of open files. This pins the
//! "faithful per-window restore" half of the spec and de-risks the
//! Stage 1 refactor (moving capture/restore onto `Window`): the
//! observable round-trip behavior must not change.

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
