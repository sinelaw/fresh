//! E2E tests for the `universal_lsp` config feature.
//!
//! Universal LSP servers apply to all languages, running alongside
//! language-specific LSP servers. These tests verify that:
//! - A server configured in `universal_lsp` is spawned for a language
//! - Go-to-definition works through a universal LSP server
//! - Find-references works through a universal LSP server

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Verify that a universal LSP server is spawned when opening a file,
/// and that go-to-definition and find-references work through it.
///
/// Steps:
/// 1. Configure a fake LSP server under `universal_lsp` (not `lsp`)
/// 2. Open a Rust file
/// 3. Wait for LSP to initialize
/// 4. Trigger go-to-definition → verify cursor jumps to the definition
/// 5. Trigger find-references → verify the references are found
#[test]
#[cfg_attr(windows, ignore)] // Uses bash script for fake LSP server
fn test_universal_lsp_go_to_definition_and_references() -> anyhow::Result<()> {
    let temp_dir = tempfile::TempDir::new()?;
    let project_root = temp_dir.path().canonicalize()?;

    // Create a .git dir so root_markers detect this as a project root
    std::fs::create_dir_all(project_root.join(".git"))?;

    // Create a Rust file
    let test_file = project_root.join("test.rs");
    std::fs::write(
        &test_file,
        "struct Point {\n    x: f64,\n}\n\nfn main() {\n    let p = Point { x: 1.0 };\n}\n",
    )?;

    let test_uri = format!("file://{}", test_file.to_str().unwrap());

    let log_file = project_root.join("lsp_log.txt");
    let log_path = log_file.to_str().unwrap();

    // Create a fake LSP server script that supports definition + references + completion
    let script = format!(
        r##"#!/bin/bash

LOG_FILE="{log_path}"
TEST_URI="{test_uri}"

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
            send_message '{{"jsonrpc":"2.0","id":'$msg_id',"result":{{"capabilities":{{"textDocumentSync":1,"definitionProvider":true,"referencesProvider":true,"completionProvider":{{"triggerCharacters":["."]}}}}}}}}'
            echo "SENT: initialize response" >> "$LOG_FILE"
            ;;
        "initialized")
            echo "ACTION: initialized" >> "$LOG_FILE"
            ;;
        "textDocument/didOpen"|"textDocument/didChange"|"textDocument/didClose")
            echo "ACTION: $method" >> "$LOG_FILE"
            ;;
        "textDocument/definition")
            # Always jump to line 0 (struct Point definition)
            send_message '{{"jsonrpc":"2.0","id":'$msg_id',"result":{{"uri":"'"$TEST_URI"'","range":{{"start":{{"line":0,"character":7}},"end":{{"line":0,"character":12}}}}}}}}'
            echo "SENT: definition -> line 0" >> "$LOG_FILE"
            ;;
        "textDocument/references")
            # Return two reference locations
            send_message '{{"jsonrpc":"2.0","id":'$msg_id',"result":[{{"uri":"'"$TEST_URI"'","range":{{"start":{{"line":0,"character":7}},"end":{{"line":0,"character":12}}}}}},{{"uri":"'"$TEST_URI"'","range":{{"start":{{"line":5,"character":12}},"end":{{"line":5,"character":17}}}}}}]}}'
            echo "SENT: references (2 locations)" >> "$LOG_FILE"
            ;;
        "textDocument/completion")
            send_message '{{"jsonrpc":"2.0","id":'$msg_id',"result":{{"isIncomplete":false,"items":[{{"label":"Point","kind":22}}]}}}}'
            echo "SENT: completion" >> "$LOG_FILE"
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

    let script_path = project_root.join("fake_universal_lsp.sh");
    std::fs::write(&script_path, &script)?;

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = std::fs::metadata(&script_path)?.permissions();
        perms.set_mode(0o755);
        std::fs::set_permissions(&script_path, perms)?;
    }

    // Configure universal_lsp with our fake server (no per-language lsp for rust)
    let mut config = fresh::config::Config::default();

    // Disable the default rust-analyzer so only the universal server is active
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: "rust-analyzer".to_string(),
            enabled: false,
            ..Default::default()
        }]),
    );

    // Add the fake server as a universal LSP
    config.universal_lsp.insert(
        "test-universal".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: script_path.to_string_lossy().to_string(),
            args: vec![],
            enabled: true,
            auto_start: true,
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
            env: Default::default(),
            language_id_overrides: Default::default(),
            root_markers: Default::default(),
            name: Some("TestUniversalLSP".to_string()),
            only_features: None,
            except_features: None,
        }]),
    );

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 30, config, project_root)?;

    // Open the Rust file
    harness.open_file(&test_file)?;

    // Wait for the universal LSP server to initialize
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.contains("ACTION: initialized")
    })?;

    // Verify LSP is running for rust (via the universal server)
    let running = harness.editor().running_lsp_servers();
    assert!(
        running.contains(&"rust".to_string()),
        "Universal LSP should be running for 'rust' language, but running servers are: {:?}",
        running
    );

    // --- Test Go-to-Definition ---
    // Move cursor to line 6 (0-indexed: 5), on "Point" in "let p = Point { x: 1.0 };"
    // Line 6 content: "    let p = Point { x: 1.0 };"
    for _ in 0..5 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    }
    // Move right to "Point" (past "    let p = ")
    for _ in 0..12 {
        harness.send_key(KeyCode::Right, KeyModifiers::NONE)?;
    }
    harness.process_async_and_render()?;

    // Trigger go-to-definition (F12)
    harness.send_key(KeyCode::F(12), KeyModifiers::NONE)?;

    // Wait for the definition response
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.contains("SENT: definition -> line 0")
    })?;

    // Wait for the status message confirming the jump
    harness.wait_until(|h| h.screen_to_string().contains("Jumped to definition"))?;

    // --- Test Find References ---
    // Trigger find references (Shift+F12) from the current position (line 1 after jump)
    harness.send_key(KeyCode::F(12), KeyModifiers::SHIFT)?;

    // Wait for the references response
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.contains("SENT: references (2 locations)")
    })?;

    // Wait for the references result to appear in the UI
    harness.wait_until(|h| {
        let screen = h.screen_to_string();
        // The status message or references panel should show "2" references
        screen.contains("2") && (screen.contains("reference") || screen.contains("Reference"))
    })?;

    Ok(())
}
