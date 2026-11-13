//! E2E test: a plugin declares a map setting with `editor.defineConfigMap`.
//!
//! The Settings UI must show it as a map — a row per entry, its key and the
//! `displayField` value — under the plugin's category; the plugin must read
//! back the declared default, or the user's value when the config file sets
//! one; and a malformed declaration must throw with a message saying what is
//! wrong. The test plugin writes what it reads into the buffer, so the checks
//! look at the screen (CONTRIBUTING.md, "E2E Tests Observe, Not Inspect").

use crate::common::harness::{copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::{Config, PluginConfig};
use std::fs;

const PLUGIN_NAME: &str = "cfg_map_test";
const PLUGIN_SOURCE: &str = r#"
/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

editor.defineConfigMap("tools", {
    default: {
        alpha: { command: "alpha-cmd", args: ["--one", "{file}"] },
        beta: { command: "beta-cmd", args: [] },
    },
    value: {
        command: { type: "string", description: "The program to run" },
        args: "string[]",
    },
    displayField: "command",
    description: "Programs by name",
});

// Declarations that must be refused, each with its message kept to show.
const refusals: string[] = [];
for (const [label, options] of [
    ["badtype", { default: {}, value: { command: "float" } }],
    ["baddefault", { default: { x: { command: 3 } }, value: { command: "string" } }],
    ["badfield", { default: { x: { other: "y" } }, value: { command: "string" } }],
    ["baddisplay", { default: {}, value: { command: "string" }, displayField: "nope" }],
] as const) {
    try {
        editor.defineConfigMap(label, options as never);
        refusals.push(`${label}: accepted`);
    } catch (e) {
        refusals.push(`${label}: ${String((e as Error).message ?? e)}`);
    }
}

function show(): void {
    const cfg = (editor.getPluginConfig() ?? {}) as {
        tools?: Record<string, { command?: string; args?: string[] }>;
    };
    const tools = Object.entries(cfg.tools ?? {})
        .map(([k, v]) => `${k}=${v.command}(${(v.args ?? []).join(" ")})`)
        .join(" ");
    editor.insertAtCursor(`TOOLS ${tools}\n${refusals.join("\n")}\n`);
}
registerHandler("cfg_map_test_show", show);
editor.registerCommand("cfg_map_test: Show", "Show the tools map", "cfg_map_test_show", null);
"#;

fn harness_with(config: Config) -> (EditorTestHarness, tempfile::TempDir) {
    let temp = tempfile::TempDir::new().expect("tempdir");
    let working_dir = temp.path().join("work");
    fs::create_dir_all(&working_dir).unwrap();
    let plugins_dir = working_dir.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    fs::write(plugins_dir.join(format!("{PLUGIN_NAME}.ts")), PLUGIN_SOURCE).unwrap();
    copy_plugin_lib(&plugins_dir);
    let harness = EditorTestHarness::with_config_and_working_dir(140, 40, config, working_dir)
        .expect("harness");
    (harness, temp)
}

fn run_command(h: &mut EditorTestHarness, command: &str) {
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text(command).unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_for_prompt_closed().unwrap();
}

/// The plugin reads the declared default, and malformed declarations are
/// refused with a message naming the problem.
#[test]
fn a_map_setting_reads_its_default_and_refuses_bad_declarations() {
    crate::common::tracing::init_tracing_from_env();
    let (mut h, _tmp) = harness_with(Config::default());
    run_command(&mut h, "cfg_map_test: Show");
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("TOOLS alpha=alpha-cmd(--one {file}) beta=beta-cmd()")
            && s.contains("badtype: defineConfigMap(\"badtype\"): field `command` has type \"float\"")
            && s.contains("baddefault: defineConfigMap(\"baddefault\"): default entry `x`: field `command` must be string")
            && s.contains("badfield: defineConfigMap(\"badfield\"): default entry `x` has field `other`")
            && s.contains("baddisplay: defineConfigMap(\"baddisplay\"): `displayField` \"nope\"")
            && !s.contains("accepted")
    })
    .unwrap();
}

/// A map set in the config file replaces the default.
#[test]
fn a_map_setting_reads_the_users_value() {
    crate::common::tracing::init_tracing_from_env();
    let mut config = Config::default();
    config.plugins.insert(
        PLUGIN_NAME.to_string(),
        PluginConfig {
            enabled: true,
            path: None,
            settings: serde_json::json!({
                "tools": { "gamma": { "command": "gamma-cmd", "args": ["-v"] } }
            }),
        },
    );
    let (mut h, _tmp) = harness_with(config);
    run_command(&mut h, "cfg_map_test: Show");
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("TOOLS gamma=gamma-cmd(-v)") && !s.contains("alpha-cmd")
    })
    .unwrap();
}

/// The Settings UI shows the map under the plugin's category: a row per
/// entry with its key and its `displayField` value.
#[test]
fn a_map_setting_shows_as_a_map_in_the_settings_ui() {
    crate::common::tracing::init_tracing_from_env();
    let (mut h, _tmp) = harness_with(Config::default());
    h.render().unwrap();
    run_command(&mut h, "Open Settings");
    h.send_key(KeyCode::Char('/'), KeyModifiers::NONE).unwrap();
    h.type_text("tools").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("cfg_map_test"))
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("Tools")
            && s.lines()
                .any(|l| l.contains("alpha") && l.contains("alpha-cmd"))
            && s.lines()
                .any(|l| l.contains("beta") && l.contains("beta-cmd"))
    })
    .unwrap();
}
