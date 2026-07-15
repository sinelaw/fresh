//! End-to-end coverage for the bundled DAP plugin.
//!
//! The test drives every `Debug:` command through the command palette and
//! talks to an isolated fake adapter over the same stdin/stdout framing used
//! in production. Assertions observe only rendered status text and gutter
//! markers, as required by CONTRIBUTING.md §2.

#![cfg(feature = "plugins")]

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::{Config, PluginConfig};
use std::fs;
use std::path::Path;

const FAKE_ADAPTER: &str = r###"#!/usr/bin/env bash
set -eu

next_seq=1
program=""
current_line=2

send_frame() {
    local json="$1"
    printf 'Content-Length: %s\r\n\r\n%s' "${#json}" "$json"
}

send_response() {
    local request_seq="$1"
    local command="$2"
    local body="$3"
    local json
    printf -v json \
        '{"seq":%s,"type":"response","request_seq":%s,"success":true,"command":"%s","body":%s}' \
        "$next_seq" "$request_seq" "$command" "$body"
    next_seq=$((next_seq + 1))
    send_frame "$json"
}

send_event() {
    local event="$1"
    local body="$2"
    local json
    printf -v json '{"seq":%s,"type":"event","event":"%s","body":%s}' \
        "$next_seq" "$event" "$body"
    next_seq=$((next_seq + 1))
    send_frame "$json"
}

while true; do
    length=""
    while IFS= read -r header; do
        header="${header%$'\r'}"
        if [[ -z "$header" ]]; then
            break
        fi
        if [[ "$header" =~ ^Content-Length:[[:space:]]*([0-9]+)$ ]]; then
            length="${BASH_REMATCH[1]}"
        fi
    done
    [[ -n "$length" ]] || exit 0

    payload=""
    IFS= read -r -N "$length" payload || true
    [[ "$payload" =~ \"seq\":([0-9]+) ]] || exit 2
    request_seq="${BASH_REMATCH[1]}"
    [[ "$payload" =~ \"command\":\"([^\"]+)\" ]] || exit 3
    command="${BASH_REMATCH[1]}"

    case "$command" in
        initialize)
            send_response "$request_seq" "$command" \
                '{"supportsConfigurationDoneRequest":true}'
            ;;
        launch)
            if [[ "$payload" =~ \"program\":\"([^\"]+)\" ]]; then
                program="${BASH_REMATCH[1]}"
            fi
            send_response "$request_seq" "$command" '{}'
            send_event initialized '{}'
            ;;
        setBreakpoints)
            send_response "$request_seq" "$command" \
                '{"breakpoints":[{"verified":true,"line":1}]}'
            ;;
        configurationDone)
            send_response "$request_seq" "$command" '{}'
            send_event stopped '{"reason":"breakpoint","threadId":1}'
            ;;
        stackTrace)
            body=""
            printf -v body \
                '{"stackFrames":[{"id":1,"name":"fake-frame","source":{"name":"debug_target.txt","path":"%s"},"line":%s,"column":1}],"totalFrames":1}' \
                "$program" "$current_line"
            send_response "$request_seq" "$command" "$body"
            ;;
        threads)
            send_response "$request_seq" "$command" \
                '{"threads":[{"id":1,"name":"main"}]}'
            ;;
        continue)
            send_response "$request_seq" "$command" '{}'
            send_event continued '{"threadId":1,"allThreadsContinued":true}'
            ;;
        pause)
            current_line=2
            send_response "$request_seq" "$command" '{}'
            send_event stopped '{"reason":"pause","threadId":1}'
            ;;
        next)
            current_line=3
            send_response "$request_seq" "$command" '{}'
            send_event stopped '{"reason":"step","threadId":1}'
            ;;
        stepIn)
            current_line=2
            send_response "$request_seq" "$command" '{}'
            send_event stopped '{"reason":"step","threadId":1}'
            ;;
        stepOut)
            current_line=3
            send_response "$request_seq" "$command" '{}'
            send_event stopped '{"reason":"step","threadId":1}'
            ;;
        disconnect)
            send_response "$request_seq" "$command" '{}'
            exit 0
            ;;
        *)
            send_response "$request_seq" "$command" '{}'
            ;;
    esac
done
"###;

fn run_palette_command(harness: &mut EditorTestHarness, command: &str) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains(">command"))
        .unwrap();
    harness.type_text(command).unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains(command))
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| !h.screen_to_string().contains(">command"))
        .unwrap();
}

fn wait_for_indicator(harness: &mut EditorTestHarness, marker: char, source_line: &str) {
    harness
        .wait_until(|h| {
            h.screen_to_string()
                .lines()
                .any(|line| line.contains(marker) && line.contains(source_line))
        })
        .unwrap();
}

fn wait_for_no_indicator(harness: &mut EditorTestHarness, marker: char) {
    harness
        .wait_until(|h| !h.screen_to_string().contains(marker))
        .unwrap();
}

fn dap_harness() -> (EditorTestHarness, tempfile::TempDir) {
    fresh::i18n::set_locale("en");

    let temp = tempfile::tempdir().unwrap();
    let workspace = temp.path().join("work");
    fs::create_dir_all(&workspace).unwrap();

    let target = workspace.join("debug_target.txt");
    fs::write(
        &target,
        "breakpoint line\npaused on second line\npaused on third line\n",
    )
    .unwrap();

    let adapter = workspace.join("fake-dap-adapter.sh");
    fs::write(&adapter, FAKE_ADAPTER).unwrap();

    let vscode = workspace.join(".vscode");
    fs::create_dir_all(&vscode).unwrap();
    fs::write(
        vscode.join("launch.json"),
        serde_json::to_vec_pretty(&serde_json::json!({
            "version": "0.2.0",
            "configurations": [{
                "name": "Fake DAP",
                "type": "fake",
                "request": "launch",
                "program": target,
            }],
        }))
        .unwrap(),
    )
    .unwrap();

    let plugins_dir = workspace.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "dap");
    copy_plugin_lib(&plugins_dir);

    let mut config = Config::default();
    config.plugins.insert(
        "dap".to_string(),
        PluginConfig {
            enabled: true,
            path: Some(plugins_dir.join("dap.ts")),
            settings: serde_json::json!({
                "adapters": [{
                    "type": "fake",
                    "command": "bash",
                    "args": [adapter],
                }],
            }),
        },
    );

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(140, 36, config, workspace).unwrap();
    harness.open_file(Path::new(&target)).unwrap();
    harness.render().unwrap();
    (harness, temp)
}

#[test]
fn debug_palette_flow_renders_breakpoints_steps_and_teardown() {
    let (mut harness, _temp) = dap_harness();

    run_palette_command(&mut harness, "Debug: Toggle Breakpoint");
    wait_for_indicator(&mut harness, '●', "breakpoint line");

    run_palette_command(&mut harness, "Debug: Start");
    harness
        .wait_until(|h| h.screen_to_string().contains("Paused at fake-frame"))
        .unwrap();
    wait_for_indicator(&mut harness, '▶', "paused on second line");

    run_palette_command(&mut harness, "Debug: Continue");
    harness
        .wait_until(|h| h.screen_to_string().contains("Debug running"))
        .unwrap();
    wait_for_no_indicator(&mut harness, '▶');

    run_palette_command(&mut harness, "Debug: Pause");
    wait_for_indicator(&mut harness, '▶', "paused on second line");

    run_palette_command(&mut harness, "Debug: Step Over");
    wait_for_indicator(&mut harness, '▶', "paused on third line");

    run_palette_command(&mut harness, "Debug: Step Into");
    wait_for_indicator(&mut harness, '▶', "paused on second line");

    run_palette_command(&mut harness, "Debug: Step Out");
    wait_for_indicator(&mut harness, '▶', "paused on third line");

    run_palette_command(&mut harness, "Debug: Stop");
    harness
        .wait_until(|h| h.screen_to_string().contains("Debug session stopped"))
        .unwrap();
    wait_for_no_indicator(&mut harness, '▶');
    wait_for_indicator(&mut harness, '●', "breakpoint line");
}
