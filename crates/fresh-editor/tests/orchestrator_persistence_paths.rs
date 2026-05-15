//! Regression test for issue #1991.
//!
//! Pre-fix, `Editor::save_orchestrator_state` wrote `windows.json`
//! into `<working_dir>/.fresh/` on every quit — leaving a stray
//! directory in the user's working tree even for sessions that never
//! touched any orchestrator feature.
//!
//! Post-fix, orchestrator state lives under
//! `<data_dir>/orchestrator/<encoded_working_dir>/`, mirroring how
//! other per-project editor state is stored.

mod common;

use fresh::config::Config;
use fresh::config_io::DirectoryContext;
use fresh::model::filesystem::StdFileSystem;
use std::sync::Arc;
use tempfile::TempDir;

#[test]
fn save_orchestrator_state_does_not_create_dotfresh_in_working_dir() {
    // Use two separate temp dirs: one for the editor's working dir
    // (the user's project), one for the platform data dir. This
    // mirrors the production layout where `dir_context.data_dir`
    // is `$XDG_DATA_HOME/fresh/`, completely separate from the
    // CWD the user invokes `fresh` from.
    let project_dir = TempDir::new().unwrap();
    let data_root = TempDir::new().unwrap();

    let dir_context = DirectoryContext::for_testing(data_root.path());
    let filesystem: Arc<dyn fresh::model::filesystem::FileSystem + Send + Sync> =
        Arc::new(StdFileSystem);

    let config = Config {
        check_for_updates: false,
        ..Config::default()
    };

    let editor = fresh::app::Editor::for_test(
        config,
        80,
        24,
        Some(project_dir.path().to_path_buf()),
        dir_context.clone(),
        fresh::view::color_support::ColorCapability::TrueColor,
        filesystem,
        None,  // time source
        None,  // grammar registry
        false, // enable_plugins
        false, // enable_embedded_plugins
    )
    .unwrap();

    // Trigger the quit-time persistence write.
    editor.save_orchestrator_state();

    // The bug: a `.fresh/` directory used to appear right here,
    // inside the user's project tree. After the fix it must not.
    let stray = project_dir.path().join(".fresh");
    assert!(
        !stray.exists(),
        "save_orchestrator_state must not create {stray:?} in the working tree (issue #1991)"
    );

    // And the corresponding orchestrator state must have landed
    // under the platform data dir instead.
    let canonical_project = project_dir
        .path()
        .canonicalize()
        .unwrap_or_else(|_| project_dir.path().to_path_buf());
    let encoded = fresh::workspace::encode_path_for_filename(&canonical_project);
    let expected_windows_file = dir_context
        .data_dir
        .join("orchestrator")
        .join(&encoded)
        .join("windows.json");
    assert!(
        expected_windows_file.exists(),
        "expected orchestrator state at {expected_windows_file:?}"
    );
}
