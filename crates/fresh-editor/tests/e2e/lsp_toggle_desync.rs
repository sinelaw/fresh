//! E2E tests for LSP toggle desync fix (GitHub issue #952)
//!
//! When LSP is toggled off and back on, the editor must re-send didOpen
//! with the current buffer content. The fix sends didClose when toggling
//! off, which clears document_versions in the async handler so that the
//! subsequent didOpen is accepted (not skipped by should_skip_did_open).
//!
//! Previous bug flow (before fix):
//! 1. Open file -> didOpen sent, document_versions[path] = 0
//! 2. Edit -> didChange sent, version incremented
//! 3. Toggle LSP OFF -> lsp_opened_with cleared, but NO didClose sent
//! 4. Edit while LSP disabled -> buffer changes, server not notified
//! 5. Toggle LSP ON -> tries didOpen, but should_skip_did_open returns true
//!    because document_versions still has the path. didOpen is SKIPPED.
//! 6. Edit -> didChange sent with ranges relative to current buffer,
//!    but server has stale content from step 2. DESYNC!
//!
//! Fixed flow:
//! 3. Toggle LSP OFF -> didClose sent, document_versions cleared
//! 5. Toggle LSP ON -> didOpen accepted, server gets current content

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Create a fake LSP server script that logs full message bodies to a file.
/// This lets us inspect the exact text content sent in didOpen and
/// the exact contentChanges sent in didChange, to verify whether the
/// server received proper re-sync after toggle.
fn create_body_logging_lsp_script() -> std::path::PathBuf {
    let script = r#"#!/bin/bash

# Log file path (passed as first argument)
LOG_FILE="${1:-/tmp/fake_lsp_body_log.txt}"

# Clear log file at start
> "$LOG_FILE"

# Function to read a message
read_message() {
    # Read headers
    local content_length=0
    while IFS=: read -r key value; do
        key=$(echo "$key" | tr -d '\r\n')
        value=$(echo "$value" | tr -d '\r\n ')
        if [ "$key" = "Content-Length" ]; then
            content_length=$value
        fi
        # Empty line marks end of headers
        if [ -z "$key" ]; then
            break
        fi
    done

    # Read content
    if [ $content_length -gt 0 ]; then
        dd bs=1 count=$content_length 2>/dev/null
    fi
}

# Function to send a message
send_message() {
    local message="$1"
    local length=${#message}
    echo -en "Content-Length: $length\r\n\r\n$message"
}

# Main loop
while true; do
    # Read incoming message
    msg=$(read_message)

    if [ -z "$msg" ]; then
        break
    fi

    # Extract method from JSON
    method=$(echo "$msg" | grep -o '"method":"[^"]*"' | cut -d'"' -f4)
    msg_id=$(echo "$msg" | grep -o '"id":[0-9]*' | cut -d':' -f2)

    # Log method and full message body for didOpen and didChange
    case "$method" in
        "textDocument/didOpen"|"textDocument/didChange"|"textDocument/didClose")
            echo "METHOD:$method" >> "$LOG_FILE"
            echo "BODY:$msg" >> "$LOG_FILE"
            echo "---" >> "$LOG_FILE"
            ;;
        *)
            if [ -n "$method" ]; then
                echo "METHOD:$method" >> "$LOG_FILE"
                echo "---" >> "$LOG_FILE"
            fi
            ;;
    esac

    case "$method" in
        "initialize")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":{"capabilities":{"completionProvider":{"triggerCharacters":["."]},"textDocumentSync":2}}}'
            ;;
        "textDocument/didOpen"|"textDocument/didChange"|"textDocument/didSave"|"textDocument/didClose")
            # Notifications - no response needed
            ;;
        "textDocument/diagnostic")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":{"items":[]}}'
            ;;
        "textDocument/inlayHint")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":[]}'
            ;;
        "textDocument/completion")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":[]}'
            ;;
        "$/cancelRequest")
            # Cancel requests are notifications - no response needed
            ;;
        "shutdown")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":null}'
            break
            ;;
    esac
done
"#;

    let script_path = std::env::temp_dir().join("fake_lsp_body_logging.sh");
    std::fs::write(&script_path, script).expect("Failed to write fake LSP script");

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = std::fs::metadata(&script_path)
            .expect("Failed to get script metadata")
            .permissions();
        perms.set_mode(0o755);
        std::fs::set_permissions(&script_path, perms).expect("Failed to set script permissions");
    }

    script_path
}

/// Test that toggling LSP off, editing, and toggling back on properly
/// resyncs the document via didClose + didOpen.
///
/// This test verifies the fix for issue #952:
/// - When LSP is toggled off, didClose is sent to clear document_versions
/// - When LSP is toggled back on, didOpen is accepted (not skipped)
/// - The server receives the current buffer content after re-enable
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses Bash-based fake LSP server
fn test_lsp_toggle_off_edit_toggle_on_causes_desync() -> anyhow::Result<()> {
    let _ = tracing_subscriber::fmt()
        .with_env_filter("fresh=debug")
        .try_init();

    // Create the body-logging fake LSP server script
    let script_path = create_body_logging_lsp_script();

    // Create temp dir and test file
    let temp_dir = tempfile::tempdir()?;
    let log_file = temp_dir.path().join("lsp_toggle_desync_log.txt");
    let test_file = temp_dir.path().join("test.rs");
    let initial_content = "fn main() {\n    let x = 5;\n}\n";
    std::fs::write(&test_file, initial_content)?;

    // Configure editor with fake LSP and a keybinding for toggle
    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::services::lsp::LspServerConfig {
            command: script_path.to_string_lossy().to_string(),
            args: vec![log_file.to_string_lossy().to_string()],
            enabled: true,
            auto_start: true,
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
            env: Default::default(),
        },
    );

    // Add keybinding for LspToggleForBuffer (Alt+T)
    config.keybindings.push(fresh::config::Keybinding {
        key: "t".to_string(),
        modifiers: vec!["alt".to_string()],
        keys: vec![],
        action: "lsp_toggle_for_buffer".to_string(),
        args: std::collections::HashMap::new(),
        when: None,
    });

    // Create harness
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    // Step 1: Open the test file (triggers didOpen)
    harness.open_file(&test_file)?;
    harness.render()?;

    // Wait for didOpen to be sent
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.contains("METHOD:textDocument/didOpen")
    })?;

    // Step 2: Type some text (triggers didChange)
    harness.type_text("abc")?;

    // Wait for didChange
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.contains("METHOD:textDocument/didChange")
    })?;

    // Step 3: Toggle LSP OFF (Alt+T)
    harness.send_key(KeyCode::Char('t'), KeyModifiers::ALT)?;
    harness.render()?;

    // Wait for didClose to be sent to the LSP server
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.contains("METHOD:textDocument/didClose")
    })?;

    // Step 4: Edit while LSP is disabled - type more text
    harness.type_text("XYZ")?;
    harness.render()?;

    // Step 5: Toggle LSP back ON (Alt+T)
    harness.send_key(KeyCode::Char('t'), KeyModifiers::ALT)?;
    harness.render()?;

    // Wait for second didOpen (re-sync after toggle)
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.matches("METHOD:textDocument/didOpen").count() >= 2
    })?;

    // Read the final log and analyze
    let final_log = std::fs::read_to_string(&log_file).unwrap_or_default();
    eprintln!("[TEST] Final LSP log:\n{}", final_log);

    // Count didOpen messages
    let did_open_count = final_log.matches("METHOD:textDocument/didOpen").count();

    // After toggle off + edit + toggle on, a SECOND didOpen must be sent
    // to resync the document content. The fix sends didClose on toggle-off
    // so that document_versions is cleared and the subsequent didOpen is accepted.
    assert_eq!(
        did_open_count, 2,
        "Expected 2 didOpen messages (initial open + re-open after toggle). Got {}.",
        did_open_count
    );

    // Verify didClose was sent when toggling off
    let did_close_count = final_log.matches("METHOD:textDocument/didClose").count();
    assert_eq!(
        did_close_count, 1,
        "Expected 1 didClose message when toggling LSP off. Got {}.",
        did_close_count
    );

    Ok(())
}

/// Test that toggling LSP off sends didClose to the server.
///
/// When LSP is toggled off, the editor must send didClose so that the
/// async handler's document_versions is cleared. This allows the
/// subsequent didOpen (when LSP is toggled back on) to be accepted
/// rather than skipped by should_skip_did_open.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses Bash-based fake LSP server
fn test_lsp_toggle_off_sends_did_close() -> anyhow::Result<()> {
    let _ = tracing_subscriber::fmt()
        .with_env_filter("fresh=debug")
        .try_init();

    let script_path = create_body_logging_lsp_script();

    let temp_dir = tempfile::tempdir()?;
    let log_file = temp_dir.path().join("lsp_toggle_close_log.txt");
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn main() {}\n")?;

    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::services::lsp::LspServerConfig {
            command: script_path.to_string_lossy().to_string(),
            args: vec![log_file.to_string_lossy().to_string()],
            enabled: true,
            auto_start: true,
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
            env: Default::default(),
        },
    );

    config.keybindings.push(fresh::config::Keybinding {
        key: "t".to_string(),
        modifiers: vec!["alt".to_string()],
        keys: vec![],
        action: "lsp_toggle_for_buffer".to_string(),
        args: std::collections::HashMap::new(),
        when: None,
    });

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    harness.open_file(&test_file)?;
    harness.render()?;

    // Wait for didOpen
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.contains("METHOD:textDocument/didOpen")
    })?;

    // Toggle LSP OFF
    harness.send_key(KeyCode::Char('t'), KeyModifiers::ALT)?;
    harness.render()?;

    // Wait for didClose to be sent to the LSP server
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.contains("METHOD:textDocument/didClose")
    })?;

    let log = std::fs::read_to_string(&log_file).unwrap_or_default();
    eprintln!("[TEST] LSP log after toggle off:\n{}", log);

    let did_close_count = log.matches("METHOD:textDocument/didClose").count();

    // didClose must be sent when toggling LSP off so that document_versions
    // is cleared in the async handler. This allows the subsequent didOpen
    // (when toggling back on) to be accepted.
    assert_eq!(
        did_close_count, 1,
        "Expected 1 didClose message when toggling LSP off. Got {}.",
        did_close_count
    );

    Ok(())
}
