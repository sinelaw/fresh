//! E2E test: the `config_changed` hook reaches plugins.
//!
//! Plugins declare settings with `editor.defineConfigX(...)`, and the
//! Settings UI renders and persists them — but before this hook nothing
//! told a plugin that a save had happened. A plugin that cached a
//! setting (rather than re-reading it at every point of use) kept
//! serving the old value until the editor restarted, so its setting
//! looked broken. `save_settings` re-applied config to host subsystems
//! only, and the `config_changed` that `reload_config` emitted went to
//! the in-process control-event bus, which plugins never see.
//!
//! The test plugin caches its setting *once* at load — deliberately the
//! shape that used to go stale — and refreshes the cache only from its
//! `config_changed` handler. It exposes the cached value as buffer text
//! (per CONTRIBUTING.md §2: observe rendered output, don't inspect
//! internals), and mirrors the handler-invocation count into the status
//! bar so tests have a deterministic barrier to wait on.

use crate::common::harness::{copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::fs;

const PLUGIN_NAME: &str = "cfg_hook";
const PLUGIN_SOURCE: &str = r#"
/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

editor.defineConfigBoolean("loud", {
    default: false,
    description: "Cached at load time; only refreshed via config_changed",
});

// Read ONCE, at load. This is the shape the hook exists for: a plugin
// that keeps a setting in a variable instead of re-reading it on every
// use has no other way to notice a Settings-UI save.
let loud = ((editor.getPluginConfig() ?? {}) as { loud?: boolean }).loud === true;
let reloads = 0;

registerHandler("cfg_hook_on_config_changed", () => {
    const cfg = (editor.getPluginConfig() ?? {}) as { loud?: boolean };
    loud = cfg.loud === true;
    reloads++;
    // Barrier for the tests: the handler has run AND the re-read is done.
    editor.setStatus(`cfghook-reloads=${reloads}`);
});
editor.on("config_changed", "cfg_hook_on_config_changed");

registerHandler("cfg_hook_show", () => {
    editor.insertAtCursor(`loud=${loud} reloads=${reloads}`);
});
editor.registerCommand(
    "cfg_hook: Show Cached",
    "Insert the plugin's cached config value into the buffer",
    "cfg_hook_show",
    null,
);

registerHandler("cfg_hook_reload", () => {
    editor.reloadConfig();
});
editor.registerCommand(
    "cfg_hook: Reload Config",
    "Ask the host to reload config from disk",
    "cfg_hook_reload",
    null,
);
"#;

fn harness_with_test_plugin() -> (EditorTestHarness, tempfile::TempDir) {
    let temp = tempfile::TempDir::new().expect("tempdir");
    let working_dir = temp.path().join("work");
    fs::create_dir_all(&working_dir).unwrap();
    let plugins_dir = working_dir.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();

    fs::write(
        plugins_dir.join(format!("{}.ts", PLUGIN_NAME)),
        PLUGIN_SOURCE,
    )
    .unwrap();
    copy_plugin_lib(&plugins_dir);

    let harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Config::default(), working_dir)
            .expect("harness");
    (harness, temp)
}

/// Run a command from the plugin through the command palette.
fn run_command(h: &mut EditorTestHarness, name: &str) {
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text(name).unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_for_prompt_closed().unwrap();
    h.render().unwrap();
}

/// Navigate the Settings UI category list until `name` is highlighted.
/// Mirrors `plugin_config_registration.rs`: the `>` selection marker and
/// the category name land on the same rendered line whether or not the
/// category draws an expand chevron.
fn focus_category(h: &mut EditorTestHarness, name: &str) {
    for _ in 0..40 {
        if h.screen_to_string()
            .lines()
            .any(|line| line.contains('>') && line.contains(name))
        {
            return;
        }
        h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        h.render().unwrap();
    }
    panic!(
        "category {:?} never became selected. Screen:\n{}",
        name,
        h.screen_to_string()
    );
}

/// Saving a Settings-UI change fires `config_changed`, and the handler
/// sees the *new* value — the snapshot is refreshed before the hook
/// runs, so a plugin re-reading `getPluginConfig()` inside its handler
/// can't read back the value it just replaced.
#[test]
fn settings_save_fires_config_changed_with_the_new_value() {
    let (mut harness, _tmp) = harness_with_test_plugin();
    harness.render().unwrap();

    // Baseline: the cached value is the declared default, and no
    // config_changed has fired yet.
    run_command(&mut harness, "cfg_hook: Show Cached");
    let before = harness.screen_to_string();
    assert!(
        before.contains("loud=false reloads=0"),
        "plugin should start from its declared default with no hook \
         invocations. Screen:\n{before}"
    );

    // Flip `loud` false → true in the Settings UI and save.
    harness.open_settings().unwrap();
    focus_category(&mut harness, &format!("Plugin: {}", PLUGIN_NAME));
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains(": [v]"))
        .unwrap();
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();

    // The plugin's own status line is the barrier: it is written from
    // inside the handler, after the re-read, so seeing it means the hook
    // ran to completion. Waiting on "Settings saved" instead would race
    // the plugin-thread round trip.
    harness
        .wait_until(|h| h.screen_to_string().contains("cfghook-reloads=1"))
        .unwrap();

    // Esc dismisses the (already saved) settings modal.
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness
        .wait_until(|h| !h.screen_to_string().contains("Settings ["))
        .unwrap();

    run_command(&mut harness, "cfg_hook: Show Cached");
    let after = harness.screen_to_string();
    assert!(
        after.contains("loud=true reloads=1"),
        "the config_changed handler must have refreshed the cached value \
         to the saved one. Screen:\n{after}"
    );
}

/// The reload-from-disk path fires the hook too. `reload_config` used to
/// only `emit_event` onto the control-event bus, which no plugin
/// subscribes to (and nothing else reads), so this fired into the void.
#[test]
fn config_reload_fires_config_changed() {
    let (mut harness, _tmp) = harness_with_test_plugin();
    harness.render().unwrap();

    run_command(&mut harness, "cfg_hook: Show Cached");
    assert!(
        harness.screen_to_string().contains("reloads=0"),
        "no hook invocation expected before the reload. Screen:\n{}",
        harness.screen_to_string()
    );

    run_command(&mut harness, "cfg_hook: Reload Config");
    harness
        .wait_until(|h| h.screen_to_string().contains("cfghook-reloads=1"))
        .unwrap();

    run_command(&mut harness, "cfg_hook: Show Cached");
    let after = harness.screen_to_string();
    assert!(
        after.contains("reloads=1"),
        "reloading config must notify plugins. Screen:\n{after}"
    );
}
