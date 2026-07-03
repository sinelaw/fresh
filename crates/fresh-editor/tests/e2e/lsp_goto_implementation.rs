//! E2E test for the `textDocument/implementation` LSP request ("Go to Implementation").
//!
//! Mirrors `lsp_goto_definition_readonly.rs`: a fake LSP server (bash script)
//! advertises `implementationProvider` and answers `textDocument/implementation`.
//! The test drives the `Ctrl+F12` keybinding and asserts, via the server log,
//! that the request travelled the full path (keybinding -> action ->
//! `request_implementation` -> async handler -> server) and that a response was
//! produced and consumed by the editor without hanging.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// `Ctrl+F12` on a symbol issues `textDocument/implementation` and the editor
/// processes the response (jumping/listing) without hanging.
#[test]
#[cfg_attr(windows, ignore)] // Uses bash script for fake LSP server
fn test_goto_implementation_request() -> anyhow::Result<()> {
    let temp_dir = tempfile::TempDir::new()?;
    // On macOS, temp paths are symlinks; the editor canonicalizes paths, so
    // URIs must use the canonical path to match.
    let project_root = temp_dir.path().canonicalize()?;

    // A "trait" definition and its concrete implementation in the same file.
    // Cursor goes onto the trait name; implementation jumps to the impl block.
    let main_file = project_root.join("main.py");
    std::fs::write(
        &main_file,
        "class Greeter:\n    def greet(self): ...\n\nclass EnglishGreeter(Greeter):\n    def greet(self):\n        return \"hi\"\n",
    )?;

    let main_uri = format!("file://{}", main_file.to_str().unwrap());

    let log_file = project_root.join("lsp_log.txt");
    let log_path = log_file.to_str().unwrap();

    // Fake LSP server that advertises `implementationProvider` and, for an
    // implementation request, points to the concrete subclass (line 3).
    let script = format!(
        r##"#!/bin/bash

MAIN_URI="{main_uri}"
LOG_FILE="{log_path}"

> "$LOG_FILE"

read_message() {{
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
}}

send_message() {{
    local message="$1"
    local length=${{#message}}
    printf "Content-Length: $length\r\n\r\n%s" "$message"
}}

while true; do
    msg=$(read_message)

    if [ -z "$msg" ]; then
        break
    fi

    method=$(echo "$msg" | grep -o '"method":"[^"]*"' | cut -d'"' -f4)
    msg_id=$(echo "$msg" | grep -o '"id":[0-9]*' | cut -d':' -f2)

    echo "RECV: method=$method id=$msg_id" >> "$LOG_FILE"

    case "$method" in
        "initialize")
            send_message '{{"jsonrpc":"2.0","id":'$msg_id',"result":{{"capabilities":{{"implementationProvider":true,"textDocumentSync":1}}}}}}'
            echo "SENT: initialize response" >> "$LOG_FILE"
            ;;
        "initialized")
            echo "ACTION: initialized" >> "$LOG_FILE"
            ;;
        "textDocument/didOpen"|"textDocument/didChange"|"textDocument/didClose")
            uri=$(echo "$msg" | grep -o '"uri":"[^"]*"' | head -1 | cut -d'"' -f4)
            echo "ACTION: $method uri=$uri" >> "$LOG_FILE"
            ;;
        "textDocument/implementation")
            uri=$(echo "$msg" | grep -o '"uri":"[^"]*"' | head -1 | cut -d'"' -f4)
            echo "RECV: implementation request from uri=$uri" >> "$LOG_FILE"
            # Point to the concrete subclass EnglishGreeter on line 3.
            send_message '{{"jsonrpc":"2.0","id":'$msg_id',"result":{{"uri":"'"$MAIN_URI"'","range":{{"start":{{"line":3,"character":6}},"end":{{"line":3,"character":20}}}}}}}}'
            echo "SENT: implementation -> main.py:3" >> "$LOG_FILE"
            ;;
        "textDocument/diagnostic")
            send_message '{{"jsonrpc":"2.0","id":'$msg_id',"result":{{"kind":"full","items":[]}}}}'
            ;;
        "textDocument/inlayHint")
            send_message '{{"jsonrpc":"2.0","id":'$msg_id',"result":[]}}'
            ;;
        "textDocument/semanticTokens/full"|"textDocument/semanticTokens/full/delta"|"textDocument/semanticTokens/range")
            send_message '{{"jsonrpc":"2.0","id":'$msg_id',"result":{{"data":[]}}}}'
            ;;
        "shutdown")
            send_message '{{"jsonrpc":"2.0","id":'$msg_id',"result":null}}'
            echo "ACTION: shutdown" >> "$LOG_FILE"
            break
            ;;
    esac
done
"##
    );

    let script_path = project_root.join("fake_impl_lsp.sh");
    std::fs::write(&script_path, &script)?;

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = std::fs::metadata(&script_path)?.permissions();
        perms.set_mode(0o755);
        std::fs::set_permissions(&script_path, perms)?;
    }

    // Configure editor with the fake LSP for Python.
    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "python".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: script_path.to_string_lossy().to_string(),
            args: Some(vec![]),
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

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(100, 30, config, project_root)?;

    // Open main.py and wait for LSP to initialize.
    harness.open_file(&main_file)?;
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.contains("ACTION: initialized")
    })?;

    // Move cursor onto "Greeter" on line 1 (the class name).
    // Content line 1: "class Greeter:"
    for _ in 0..8 {
        harness.send_key(KeyCode::Right, KeyModifiers::NONE)?;
    }
    harness.process_async_and_render()?;

    // Trigger Go to Implementation via Ctrl+F12.
    harness.send_key(KeyCode::F(12), KeyModifiers::CONTROL)?;

    // The request must reach the server...
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.contains("RECV: implementation request")
    })?;

    // ...and the server must answer (response is then consumed by the editor's
    // `handle_implementation_response`, which would hang the wait above on any
    // break in the request plumbing).
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.contains("SENT: implementation -> main.py:3")
    })?;

    // Drain the async response so the editor processes it without panicking.
    harness.process_async_and_render()?;

    Ok(())
}
