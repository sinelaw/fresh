//! `buffer_modified` reaches plugins for every edit, not only the
//! single-event ones that also fire `after_insert` / `after_delete`: a bulk
//! edit (typing over a selection, multi-cursor typing), an LSP code action,
//! and a plugin's own edit through the plugin API.

use crate::common::harness::{copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::fs;

const PROBE_PLUGIN: &str = r#"
/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

let count = 0;
editor.on("buffer_modified", (args) => {
  count += 1;
  editor.setStatus(`modified ${count} len ${editor.getBufferLength(args.buffer_id)}`);
});

// Deliberately an edit made by a plugin, through the plugin API, which is a
// path that announced nothing at all while the hook was fired per edit site.
registerHandler("probe_insert", () => {
  editor.insertText(editor.getActiveBufferId(), 0, "ZZ");
});
editor.registerCommand("Probe Insert", "Insert ZZ through the plugin API", "probe_insert", null);

editor.setStatus("probe loaded");
"#;

fn harness_with_probe(text: &str) -> (EditorTestHarness, tempfile::TempDir) {
    let dir = tempfile::TempDir::new().unwrap();
    let root = dir.path().canonicalize().unwrap();
    harness_with_probe_in(root, "notes.txt", text, Config::default(), dir)
}

/// Opens `name` holding `text` under `root`, with the probe plugin added to
/// `config`. `dir` is handed back so the tree outlives the test.
fn harness_with_probe_in(
    root: std::path::PathBuf,
    name: &str,
    text: &str,
    mut config: Config,
    dir: tempfile::TempDir,
) -> (EditorTestHarness, tempfile::TempDir) {
    fs::write(root.join(name), text).unwrap();
    let plugins_dir = root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    fs::write(plugins_dir.join("modified_probe.ts"), PROBE_PLUGIN).unwrap();

    config.plugins.insert(
        "modified_probe".to_string(),
        fresh_core::config::PluginConfig {
            enabled: true,
            path: Some(plugins_dir.join("modified_probe.ts")),
            ..Default::default()
        },
    );
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(100, 30, config, root.clone()).unwrap();
    harness.wait_for_screen_contains("probe loaded").unwrap();
    harness.open_file(&root.join(name)).unwrap();
    harness.render().unwrap();
    (harness, dir)
}

/// The hook fires once for the edit, not again for anything after it.
fn assert_fired_once(harness: &mut EditorTestHarness, len: usize) {
    harness
        .wait_for_screen_contains(&format!("modified 1 len {len}"))
        .unwrap();
    harness.wait_for_async_quiescence(5).unwrap();
    harness.assert_screen_contains(&format!("modified 1 len {len}"));
}

#[test]
fn typing_over_a_selection_fires_buffer_modified() {
    let (mut harness, _dir) = harness_with_probe("alpha\nbeta\n");

    harness.send_key(KeyCode::End, KeyModifiers::SHIFT).unwrap();
    harness.type_text("X").unwrap();
    harness.assert_buffer_content("X\nbeta\n");

    assert_fired_once(&mut harness, 7);
}

#[test]
fn multi_cursor_typing_fires_buffer_modified() {
    let (mut harness, _dir) = harness_with_probe("alpha\nbeta\n");

    harness.editor_mut().add_cursor_below();
    harness.type_text("X").unwrap();
    harness.assert_buffer_content("Xalpha\nXbeta\n");

    assert_fired_once(&mut harness, 13);
}

/// A fake language server that offers one code action, replacing
/// `let x = 5;` on line 1 with `let x = 42;` in the document it was opened
/// with. Applying it goes through `apply_events_to_buffer_as_bulk_edit`.
/// Each method it receives is appended to the file named by its argument.
#[cfg(unix)]
fn code_action_lsp_script(dir: &std::path::Path) -> std::path::PathBuf {
    use std::os::unix::fs::PermissionsExt;

    let script = r##"#!/bin/bash
LOG_FILE="$1"
DOC_URI=""

read_message() {
    local content_length=0
    while IFS=: read -r key value; do
        key=$(echo "$key" | tr -d '\r\n')
        value=$(echo "$value" | tr -d '\r\n ')
        if [ "$key" = "Content-Length" ]; then
            content_length=$value
        fi
        if [ -z "$key" ]; then
            break
        fi
    done
    if [ $content_length -gt 0 ]; then
        dd bs=1 count=$content_length 2>/dev/null
    fi
}

send_message() {
    local message="$1"
    printf "Content-Length: ${#message}\r\n\r\n%s" "$message"
}

while true; do
    msg=$(read_message)
    if [ -z "$msg" ]; then break; fi
    method=$(echo "$msg" | grep -o '"method":"[^"]*"' | cut -d'"' -f4)
    echo "$method" >> "$LOG_FILE"
    msg_id=$(echo "$msg" | grep -o '"id":[0-9]*' | cut -d':' -f2)
    case "$method" in
        "initialize")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":{"capabilities":{"textDocumentSync":2,"codeActionProvider":true}}}'
            ;;
        "textDocument/didOpen")
            DOC_URI=$(echo "$msg" | grep -o '"uri":"[^"]*"' | head -1 | cut -d'"' -f4)
            ;;
        "textDocument/codeAction")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":[{"title":"Change value","kind":"refactor","edit":{"documentChanges":[{"textDocument":{"uri":"'"$DOC_URI"'","version":null},"edits":[{"range":{"start":{"line":1,"character":4},"end":{"line":1,"character":14}},"newText":"let x = 42;"}]}]}}]}'
            ;;
        "shutdown")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":null}'
            break
            ;;
        *)
            if [ -n "$msg_id" ]; then
                send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":null}'
            fi
            ;;
    esac
done
"##;
    let path = dir.join("fake_code_action_lsp.sh");
    fs::write(&path, script).unwrap();
    let mut perms = fs::metadata(&path).unwrap().permissions();
    perms.set_mode(0o755);
    fs::set_permissions(&path, perms).unwrap();
    path
}

#[test]
#[cfg(unix)]
fn lsp_code_action_fires_buffer_modified() {
    let dir = tempfile::TempDir::new().unwrap();
    let root = dir.path().canonicalize().unwrap();
    let script = code_action_lsp_script(&root);
    let log = root.join("lsp_methods.log");

    let mut config = Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: script.to_string_lossy().to_string(),
            args: Some(vec![log.to_string_lossy().to_string()]),
            enabled: true,
            auto_start: true,
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
            env: Default::default(),
            language_id_overrides: Default::default(),
            root_markers: Default::default(),
            name: None,
            only_features: None,
            except_features: None,
        }]),
    );
    let text = "fn main() {\n    let x = 5;\n}\n";
    let (mut harness, _dir) = harness_with_probe_in(root, "main.rs", text, config, dir);
    harness.wait_for_screen_contains("LSP (on)").unwrap();
    // The code action edits the document the server was opened with.
    harness
        .wait_until(|_| {
            fs::read_to_string(&log)
                .unwrap_or_default()
                .contains("textDocument/didOpen")
        })
        .unwrap();

    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Char('.'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();
    harness.wait_for_screen_contains("Change value").unwrap();
    harness
        .send_key(KeyCode::Char('1'), KeyModifiers::NONE)
        .unwrap();

    let edited = "fn main() {\n    let x = 42;\n}\n";
    harness
        .wait_until(|h| h.get_buffer_content().as_deref() == Some(edited))
        .unwrap();
    assert_fired_once(&mut harness, edited.len());
}

/// A plugin's own edit is announced to plugins, including back to the plugin
/// that made it.
///
/// `editor.insertText()` and its four siblings (`deleteRange`,
/// `insertAtCursor`, `replaceRange`, `replaceInBuffer` — the last is how the
/// project search panel writes its replacements) fired nothing while
/// `buffer_modified` was fired from the edit sites, so a plugin could not see
/// another's edits, nor the panel its own. Firing once per pass for every
/// buffer whose content moved covers them without any of the five saying so.
///
/// Without that, the wait below never resolves: the text is inserted and the
/// hook never fires.
#[test]
fn a_plugin_api_edit_fires_buffer_modified() {
    let (mut harness, _dir) = harness_with_probe("alpha\nbeta\n");

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_palette_command("Probe Insert").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("ZZalpha"))
        .unwrap();
    assert_fired_once(&mut harness, 13);
}
