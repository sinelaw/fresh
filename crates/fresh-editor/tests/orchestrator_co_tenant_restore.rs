//! Cold-reboot round-trip for co-tenant workspaces (multiple windows over one
//! project root).
//!
//! This is the invariant the interactive Orchestrator dock could not be
//! scripted to confirm: extract a tab into a second window over the *same*
//! root, quit, relaunch — and each of the two co-tenant windows must restore
//! its OWN file. The source keeps `alpha`, the extracted window keeps `beta`.
//! Without a durable per-window identity the two on-disk files would collapse
//! (both loading the freshest snapshot, so both showing `beta`) — exactly the
//! "same buffer opened twice" hazard multiple-workspaces-per-root has to avoid.
//!
//! Persistence is isolated by `common::global_state::isolated_dir_context`:
//! workspace save/load resolve through the pinned data dir, and the editor's
//! boot discovery reads the same `DirectoryContext::data_dir`, so both point
//! at one isolated tree. The pin is thread-local — the `$XDG_DATA_HOME`
//! `set_var` it replaced was process-global, and once every root was folded
//! into one `all_tests` binary a sibling's `set_var` moved this test's
//! workspaces dir out from under it between the save and the read below.
//! Linux-gated: `dirs::data_dir()` ignores `XDG_DATA_HOME` off Linux, so the
//! fallback these paths model is not the platform one there.
#![cfg(target_os = "linux")]

use fresh::config::Config;
use fresh::config_io::DirectoryContext;
use fresh::model::filesystem::StdFileSystem;
use std::collections::BTreeSet;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use crate::common::global_state::isolated_dir_context;

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

#[test]
fn co_tenants_persist_and_restore_each_own_file() {
    let sandbox = tempfile::tempdir().unwrap();
    let (dir_context, _data_dir_pin) = isolated_dir_context(sandbox.path());
    let project = sandbox.path().join("project");
    std::fs::create_dir(&project).unwrap();
    let project = project.canonicalize().unwrap();
    let alpha = project.join("alpha.txt");
    let beta = project.join("beta.txt");
    std::fs::write(&alpha, "alpha\n").unwrap();
    std::fs::write(&beta, "beta\n").unwrap();

    // Session 1: open both files (beta focused last), extract beta into a
    // co-tenant over the same root, then persist every window.
    {
        let mut e1 = editor_in(&project, &dir_context);
        e1.open_file(&alpha).unwrap();
        e1.open_file(&beta).unwrap();
        let beta_buffer = e1.active_buffer();
        e1.extract_tab_to_new_workspace(beta_buffer);
        // Source window keeps alpha; the new co-tenant took beta.
        e1.save_all_windows_workspaces().unwrap();
    }

    // Two distinct on-disk workspace files now describe this one root.
    let workspaces_dir = dir_context.data_dir.join("workspaces");
    let files: Vec<_> = std::fs::read_dir(&workspaces_dir)
        .unwrap()
        .filter_map(|e| e.ok())
        .filter(|e| e.path().extension().is_some_and(|x| x == "json"))
        .collect();
    assert_eq!(
        files.len(),
        2,
        "two co-tenant windows must persist as two distinct workspace files, got: {:?}",
        files.iter().map(|e| e.file_name()).collect::<Vec<_>>()
    );

    // Session 2: cold reboot at the same root. Boot discovery rebuilds both
    // co-tenant windows; restore the foreground (as a real launch does) and
    // lazily materialize the background co-tenant.
    let mut e2 = editor_in(&project, &dir_context);
    e2.restore_active_window_on_launch(false).unwrap();
    e2.materialize_all_windows();

    let mut file_sets: Vec<BTreeSet<PathBuf>> = Vec::new();
    for id in 1..=64u64 {
        if let Some(w) = e2.session(fresh_core::WindowId(id)) {
            if w.root != project {
                continue;
            }
            let paths: BTreeSet<PathBuf> = w.buffers.paths().into_iter().collect();
            if !paths.is_empty() {
                file_sets.push(paths);
            }
        }
    }
    file_sets.sort();

    // Each co-tenant restored its OWN file: one window holds alpha, the other
    // beta — not two copies of the freshest snapshot, and not a single
    // survivor with the other silently dropped.
    assert_eq!(
        file_sets,
        vec![
            BTreeSet::from([alpha.clone()]),
            BTreeSet::from([beta.clone()]),
        ],
        "each restored co-tenant must reopen exactly its own file"
    );
}
