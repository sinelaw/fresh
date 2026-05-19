//! E2E test: a plugin registers config fields via
//! `editor.defineConfigBoolean/.../defineConfigString`. The Settings UI
//! must surface a "Plugin: <name>" category populated with those
//! fields, the user must be able to toggle/edit values, and a re-open
//! must show the persisted state. The test plugin exposes its current
//! config value as visible buffer text (via a command) so we can assert
//! on the rendered output rather than internal state — matching the
//! "E2E Tests Observe, Not Inspect" rule in CONTRIBUTING.md.
//!
//! Without the new `defineConfigX` API + Settings-UI integration this
//! test panics: the plugin's category never appears, so the navigation
//! loop times out before finding it.

use crate::common::harness::{copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::fs;

const PLUGIN_NAME: &str = "cfg_test";
const PLUGIN_SOURCE: &str = r#"
/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

// Strongly-typed config registration. The TS types are inferred from
// each call, and the host throws synchronously if anything's wrong.
editor.defineConfigString("prefix", {
    default: "DEFAULT",
    description: "Prefix prepended to the inserted greeting",
});
editor.defineConfigBoolean("uppercase", {
    default: false,
    description: "Whether to uppercase the greeting suffix",
});

function insertGreeting(): void {
    // Re-read on each invocation so a Settings UI save is picked up live.
    const cfg = (editor.getPluginConfig() ?? {}) as { prefix?: string; uppercase?: boolean };
    const prefix = cfg.prefix ?? "DEFAULT";
    const suffix = cfg.uppercase ? "HELLO" : "hello";
    editor.insertAtCursor(`${prefix}:${suffix}`);
}
registerHandler("cfg_test_insert", insertGreeting);

editor.registerCommand(
    "cfg_test: Insert Greeting",
    "Insert greeting using the plugin config values",
    "cfg_test_insert",
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

/// Run the plugin's "Insert Greeting" command via the command palette.
/// The plugin handler synchronously appends to the active buffer; the
/// inserted text shows up on the next render.
fn run_insert_greeting(h: &mut EditorTestHarness) {
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text("cfg_test: Insert Greeting").unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_for_prompt_closed().unwrap();
    h.render().unwrap();
}

/// Navigate the Settings UI category list until the named entry is
/// highlighted. The selection marker `>` lives in column 0 of the
/// category cell, but expandable categories (those with sub-sections)
/// render a `▶` chevron in front of the name, which shifts the layout.
/// Just look for any line that contains both the `>` selector glyph
/// and the category name — robust against either layout.
fn focus_category(h: &mut EditorTestHarness, name: &str) {
    for _ in 0..40 {
        if category_is_selected(h, name) {
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

/// True iff some line in the rendered screen contains both the `>`
/// selection marker AND `name`. The marker is column-aligned to the
/// category cell's first glyph, so co-occurrence on the same line is
/// a reliable indicator that *that* row is the selected one.
fn category_is_selected(h: &EditorTestHarness, name: &str) -> bool {
    h.screen_to_string()
        .lines()
        .any(|line| line.contains('>') && line.contains(name))
}

/// 1. The plugin's category shows up under "Plugin: <name>".
/// 2. Both registered fields render with their default values.
/// 3. After toggling a boolean and saving, the plugin's visible
///    behavior reflects the new value on the next invocation.
#[test]
fn plugin_config_round_trip_toggles_visible_behavior() {
    crate::common::tracing::init_tracing_from_env();
    let (mut harness, _tmp) = harness_with_test_plugin();
    harness.render().unwrap();

    // Sanity: default behavior — `uppercase: false` → lower-case suffix.
    run_insert_greeting(&mut harness);
    let after_first = harness.screen_to_string();
    assert!(
        after_first.contains("DEFAULT:hello"),
        "Plugin should have inserted the default greeting. Screen:\n{after_first}"
    );

    // Open settings and verify the plugin category is present. The
    // schema-registration commands arrive on the editor's command
    // channel asynchronously, so the very first `open_settings` may
    // race them — wait_until-reopen-loop until the category shows up.
    // The Settings panel rebuilds its category list every time it
    // opens, so re-opening is enough to pick up newly-registered
    // schemas without an editor restart.
    let plugin_marker = format!("Plugin: {}", PLUGIN_NAME);
    harness.open_settings().unwrap();
    // The Settings panel rebuilds its category list every time it
    // opens, so close + reopen drives a fresh read of plugin_schemas.
    // Loop on the screen contents until the registration commands
    // have drained from the plugin → editor channel and the category
    // surfaces.
    for _attempt in 0..200 {
        if harness.screen_to_string().contains(&plugin_marker) {
            break;
        }
        harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
        harness.open_settings().unwrap();
    }
    let after_open = harness.screen_to_string();
    assert!(
        after_open.contains(&plugin_marker),
        "Settings UI should show the plugin's category. Screen:\n{after_open}"
    );

    // Plugin categories belong at the bottom of the category list so
    // plugin configuration doesn't interleave with built-in editor
    // settings. Verify by asserting that the plugin marker appears
    // AFTER every built-in category name in the left-panel pane.
    let plugin_marker_pos = after_open
        .find(&plugin_marker)
        .expect("plugin marker present");
    for builtin in &[
        "General",
        "Clipboard",
        "Editor",
        "File Browser",
        "File Explorer",
        "Packages",
        "Plugins",
        "Terminal",
        "Warnings",
    ] {
        let bi_pos = after_open
            .find(builtin)
            .unwrap_or_else(|| panic!("built-in category {:?} missing from screen", builtin));
        assert!(
            bi_pos < plugin_marker_pos,
            "Built-in category {:?} (offset {bi_pos}) must render before the plugin \
             marker {:?} (offset {plugin_marker_pos}). Screen:\n{after_open}",
            builtin,
            plugin_marker,
        );
    }

    focus_category(&mut harness, &format!("Plugin: {}", PLUGIN_NAME));
    let after_focus = harness.screen_to_string();
    assert!(
        after_focus.contains("Prefix"),
        "Plugin category should render the `prefix` field. Screen:\n{after_focus}"
    );
    assert!(
        after_focus.contains("Uppercase"),
        "Plugin category should render the `uppercase` field. Screen:\n{after_focus}"
    );
    assert!(
        after_focus.contains("DEFAULT"),
        "The text input should show the declared default `DEFAULT`. Screen:\n{after_focus}"
    );

    // Move focus into the settings panel and toggle `uppercase` (the
    // second item) from false → true.
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let after_toggle = harness.screen_to_string();
    assert!(
        after_toggle.contains(": [v]"),
        "Toggling `uppercase` should leave the toggle checked ([v]). Screen:\n{after_toggle}"
    );

    // Save via Ctrl+S, then close the settings dialog.
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Settings saved"))
        .unwrap();

    // Re-running the plugin command must now insert the upper-case
    // suffix — proving the new value reached the plugin's
    // `getPluginConfig()` read on the snapshot.
    run_insert_greeting(&mut harness);
    let after_second = harness.screen_to_string();
    assert!(
        after_second.contains("DEFAULT:HELLO"),
        "After toggling `uppercase` and saving, the plugin must observe \
         the new value on its next invocation. Screen:\n{after_second}"
    );

    // Re-opening Settings rebuilds the category tree from the live
    // schema map, so the persisted value should still be reflected
    // (toggle still renders checked as [v]).
    harness.open_settings().unwrap();
    focus_category(&mut harness, &format!("Plugin: {}", PLUGIN_NAME));
    let after_reopen = harness.screen_to_string();
    assert!(
        after_reopen.contains("Uppercase")
            && after_reopen.contains(": [v]")
            && after_reopen.contains("(user)"),
        "Re-opened settings must show the persisted Uppercase=[v] \
         from the User layer. Screen:\n{after_reopen}"
    );
}

/// The plugin's category must NOT appear when the plugin is disabled.
/// Mirrors the explicit design decision: disabled plugins are hidden
/// from the Settings UI, period.
#[test]
fn plugin_config_category_hidden_when_plugin_disabled() {
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

    let mut config = Config::default();
    config.plugins.insert(
        PLUGIN_NAME.to_string(),
        fresh::config::PluginConfig {
            enabled: false,
            path: None,
            settings: serde_json::Value::Null,
        },
    );

    let mut harness = EditorTestHarness::with_config_and_working_dir(120, 40, config, working_dir)
        .expect("harness");
    harness.render().unwrap();

    harness.open_settings().unwrap();
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains(&format!("Plugin: {}", PLUGIN_NAME)),
        "A disabled plugin must not show a 'Plugin: <name>' category. Screen:\n{screen}"
    );
}
