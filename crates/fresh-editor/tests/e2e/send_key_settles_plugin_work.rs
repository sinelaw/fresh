//! `send_key` must not return mid-handler.
//!
//! It settles a keypress's plugin work before returning, so a test can
//! press two keys in a row and reason about them as if the editor had
//! applied the first one completely. Watching the plugin command channel
//! go quiet cannot deliver that: a handler awaiting a host round-trip is
//! quiet, and so is one whose continuation is running on the plugin
//! thread.
//!
//! The probe plugin (`tests/plugins/test_drain_probe.ts`) makes that
//! window wide enough to test instead of rare enough to flake.

use crate::common::harness::{copy_plugin_lib, EditorTestHarness, HarnessOptions};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use fresh::input::keybindings::Action::PluginAction;
use std::fs;

const PLUGIN: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/tests/plugins/test_drain_probe.ts"
));

/// What the probe writes into the buffer once its handler has finished.
const MARK: &str = "PROBE-SETTLED";

/// F9 runs the probe's handler. An action name the editor does not know
/// resolves to the plugin action of that name.
fn probe_config() -> Config {
    let mut config = Config::default();
    config.keybindings.push(fresh::config::Keybinding {
        key: "f9".to_string(),
        modifiers: vec![],
        keys: vec![],
        chord: String::new(),
        action: "probe_key".to_string(),
        args: std::collections::HashMap::new(),
        when: None,
    });
    config
}

#[test]
fn send_key_returns_only_once_the_handler_has_finished() {
    init_tracing_from_env();
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project = temp_dir.path().join("project");
    fs::create_dir(&project).unwrap();
    let plugins_dir = project.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    fs::write(plugins_dir.join("test_drain_probe.ts"), PLUGIN).unwrap();

    let mut h = EditorTestHarness::create(
        80,
        24,
        HarnessOptions::new()
            .with_working_dir(project.clone())
            .with_config(probe_config())
            .without_empty_plugins_dir(),
    )
    .unwrap();

    // The plugin's command reaching the registry means it is loaded.
    h.wait_until(|h| {
        h.editor()
            .command_registry()
            .read()
            .unwrap()
            .get_all()
            .iter()
            .any(|c| c.action == PluginAction("probe_key".to_string()))
    })
    .unwrap();

    h.send_key(KeyCode::F(9), KeyModifiers::NONE).unwrap();

    // Deliberately no wait: `send_key` already claims this.
    let screen = h.screen_to_string();
    assert!(
        screen.contains(MARK),
        "send_key returned before the handler it dispatched had finished. \
         Screen:\n{screen}"
    );
}
