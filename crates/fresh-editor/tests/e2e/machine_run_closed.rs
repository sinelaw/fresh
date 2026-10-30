//! A machine's `run` resolves, never rejects — also once the machine is
//! closed: it ends with code -1 and says why in `stderr`, as a command that
//! cannot start does. Callers written for `spawnProcess` expect a result,
//! and an unhandled rejection stops their plugin.
//!
//! The `test_machine_run_closed.ts` plugin runs `echo` on the active
//! window's machine and on one it has closed, and puts both outcomes in the
//! status bar.

use crate::common::harness::{copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

#[test]
#[cfg(unix)]
fn run_on_a_closed_machine_resolves_with_minus_one() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    let plugins_dir = project_root.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    fs::write(
        plugins_dir.join("test_machine_run_closed.ts"),
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/plugins/test_machine_run_closed.ts"
        )),
    )
    .unwrap();

    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 24, Default::default(), project_root)
            .unwrap();
    h.render().unwrap();
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.type_text("TestRun: Closed machine").unwrap();
    h.render().unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();

    h.wait_until(|h| h.screen_to_string().contains("RUN open="))
        .unwrap();
    let screen = h.screen_to_string();
    assert!(
        screen.contains("RUN open=0:hi closed=-1:true"),
        "expected the open run to succeed and the closed one to resolve -1:\n{screen}"
    );
}
