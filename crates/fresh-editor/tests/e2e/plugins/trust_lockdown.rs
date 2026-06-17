//! Security regression: a plugin must not be able to *set* the workspace
//! trust level. Plugins may request the trust prompt (the user decides), but
//! the level-setting actions (`workspace_trust_trust` / `_restrict` / `_block`)
//! are denied when dispatched through the generic `executeActions` channel —
//! matching the VS Code / JetBrains / Zed model where extensions can ask but
//! never grant. See `handle_execute_actions` in `plugin_dispatch.rs`.

use crate::common::harness::{copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use fresh::services::workspace_trust::TrustLevel;
use std::fs;
use tempfile::TempDir;

/// Install the tiny `test_trust_lockdown` plugin into the project.
fn setup_plugin(project_root: &std::path::Path) {
    let plugins_dir = project_root.join("plugins");
    fs::create_dir_all(&plugins_dir).expect("create plugins dir");
    copy_plugin_lib(&plugins_dir);
    const SRC: &str = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/tests/plugins/test_trust_lockdown.ts"
    ));
    fs::write(plugins_dir.join("test_trust_lockdown.ts"), SRC).expect("write test plugin");
}

#[test]
fn plugin_cannot_set_workspace_trust_level() {
    let tmp = TempDir::new().unwrap();
    let project = tmp.path().to_path_buf();
    setup_plugin(&project);

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(140, 40, Config::default(), project)
            .unwrap();

    // Pin the workspace to Restricted, then publish it to the plugin snapshot
    // so `editor.workspaceTrustLevel()` reads "restricted".
    harness
        .editor()
        .authority()
        .workspace_trust
        .set_level(TrustLevel::Restricted);
    harness.editor_mut().update_plugin_state_snapshot();
    harness.editor_mut().fire_plugins_loaded_hook();
    harness.render().unwrap();

    // Run the plugin command that tries to elevate trust from the plugin and
    // then reports the resulting level.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("TestTrust: Try Elevate").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // The plugin reported the level *after* its (denied) elevation attempt.
    // It must still be Restricted — a plugin cannot grant trust.
    harness
        .wait_until(|h| h.screen_to_string().contains("TRUST-AFTER:restricted"))
        .unwrap();
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("TRUST-AFTER:trusted"),
        "plugin must not be able to elevate workspace trust; screen:\n{screen}"
    );

    // And the authoritative core level is unchanged too.
    assert_eq!(
        harness.editor().authority().workspace_trust.level(),
        TrustLevel::Restricted,
        "core trust level must remain Restricted after a plugin elevation attempt"
    );
}
