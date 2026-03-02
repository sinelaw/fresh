//! E2E tests for publishDiagnostics client capability (issue #1006)
//!
//! The editor must advertise `textDocument.publishDiagnostics` in the client
//! capabilities sent during the LSP `initialize` request. Without this,
//! strict servers (like pyright, rust-analyzer) may withhold diagnostics,
//! while permissive servers (like clangd) send them regardless.
//!
//! These tests verify the fix using two fake LSP servers:
//! 1. A "strict" server that only sends publishDiagnostics if the client
//!    advertises the capability (mimics pyright behavior)
//! 2. A "permissive" server that always sends publishDiagnostics (mimics clangd)
//!
//! Both should now receive diagnostics since the editor advertises the capability.

use crate::common::harness::EditorTestHarness;

/// Create a fake LSP server that checks client capabilities and only sends
/// publishDiagnostics if the client advertises `textDocument.publishDiagnostics`.
///
/// This mimics how strict LSP servers behave. The script:
/// 1. Parses the `initialize` request params
/// 2. Checks for `publishDiagnostics` in the JSON
/// 3. Logs whether the capability was found
/// 4. Only sends diagnostics on didOpen/didChange if the capability was present
fn create_strict_server_script() -> std::path::PathBuf {
    let script = r#"#!/bin/bash

# Log file path (passed as first argument)
LOG_FILE="${1:-/tmp/fake_lsp_strict_log.txt}"

# Clear log file at start
> "$LOG_FILE"

# Whether client advertised publishDiagnostics capability
HAS_PUBLISH_DIAGNOSTICS=0

# Function to read a message
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

# Function to send a message
send_message() {
    local message="$1"
    local length=${#message}
    echo -en "Content-Length: $length\r\n\r\n$message"
}

# Main loop
while true; do
    msg=$(read_message)

    if [ -z "$msg" ]; then
        break
    fi

    method=$(echo "$msg" | grep -o '"method":"[^"]*"' | cut -d'"' -f4)
    msg_id=$(echo "$msg" | grep -o '"id":[0-9]*' | cut -d':' -f2)

    case "$method" in
        "initialize")
            # Check if client advertises publishDiagnostics capability
            if echo "$msg" | grep -q '"publishDiagnostics"'; then
                HAS_PUBLISH_DIAGNOSTICS=1
                echo "CAPABILITY:publishDiagnostics=YES" >> "$LOG_FILE"
            else
                HAS_PUBLISH_DIAGNOSTICS=0
                echo "CAPABILITY:publishDiagnostics=NO" >> "$LOG_FILE"
            fi

            # Log full initialize params for debugging
            echo "INIT_PARAMS:$msg" >> "$LOG_FILE"
            echo "---" >> "$LOG_FILE"

            # Respond with server capabilities
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":{"capabilities":{"textDocumentSync":2,"completionProvider":{"triggerCharacters":["."]}}}}'
            ;;
        "textDocument/didOpen")
            uri=$(echo "$msg" | grep -o '"uri":"[^"]*"' | head -1 | cut -d'"' -f4)
            echo "METHOD:textDocument/didOpen" >> "$LOG_FILE"

            # Only send diagnostics if client advertised the capability
            # This is how a strict server behaves
            if [ $HAS_PUBLISH_DIAGNOSTICS -eq 1 ]; then
                echo "ACTION:sending_diagnostics (capability present)" >> "$LOG_FILE"
                send_message '{"jsonrpc":"2.0","method":"textDocument/publishDiagnostics","params":{"uri":"'$uri'","diagnostics":[{"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":5}},"severity":1,"message":"Strict server: error found"}]}}'
            else
                echo "ACTION:withholding_diagnostics (capability missing)" >> "$LOG_FILE"
            fi
            echo "---" >> "$LOG_FILE"
            ;;
        "textDocument/didChange")
            echo "METHOD:textDocument/didChange" >> "$LOG_FILE"
            echo "---" >> "$LOG_FILE"
            ;;
        "textDocument/diagnostic")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":{"items":[]}}'
            ;;
        "textDocument/inlayHint")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":[]}'
            ;;
        "$/cancelRequest")
            ;;
        "shutdown")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":null}'
            break
            ;;
    esac
done
"#;

    let script_path = std::env::temp_dir().join("fake_lsp_strict_server.sh");
    std::fs::write(&script_path, script).expect("Failed to write strict server script");

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

/// Create a fake LSP server that always sends publishDiagnostics regardless
/// of client capabilities (mimics clangd behavior).
fn create_permissive_server_script() -> std::path::PathBuf {
    let script = r#"#!/bin/bash

# Log file path (passed as first argument)
LOG_FILE="${1:-/tmp/fake_lsp_permissive_log.txt}"

# Clear log file at start
> "$LOG_FILE"

# Function to read a message
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

# Function to send a message
send_message() {
    local message="$1"
    local length=${#message}
    echo -en "Content-Length: $length\r\n\r\n$message"
}

# Main loop
while true; do
    msg=$(read_message)

    if [ -z "$msg" ]; then
        break
    fi

    method=$(echo "$msg" | grep -o '"method":"[^"]*"' | cut -d'"' -f4)
    msg_id=$(echo "$msg" | grep -o '"id":[0-9]*' | cut -d':' -f2)

    case "$method" in
        "initialize")
            # Log whether client advertised publishDiagnostics
            if echo "$msg" | grep -q '"publishDiagnostics"'; then
                echo "CAPABILITY:publishDiagnostics=YES" >> "$LOG_FILE"
            else
                echo "CAPABILITY:publishDiagnostics=NO" >> "$LOG_FILE"
            fi
            echo "---" >> "$LOG_FILE"

            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":{"capabilities":{"textDocumentSync":2,"completionProvider":{"triggerCharacters":["."]}}}}'
            ;;
        "textDocument/didOpen")
            uri=$(echo "$msg" | grep -o '"uri":"[^"]*"' | head -1 | cut -d'"' -f4)
            echo "METHOD:textDocument/didOpen" >> "$LOG_FILE"
            echo "ACTION:sending_diagnostics (always, like clangd)" >> "$LOG_FILE"
            echo "---" >> "$LOG_FILE"

            # Always send diagnostics regardless of client capability
            send_message '{"jsonrpc":"2.0","method":"textDocument/publishDiagnostics","params":{"uri":"'$uri'","diagnostics":[{"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":5}},"severity":1,"message":"Permissive server: error found"}]}}'
            ;;
        "textDocument/didChange")
            echo "METHOD:textDocument/didChange" >> "$LOG_FILE"
            echo "---" >> "$LOG_FILE"
            ;;
        "textDocument/diagnostic")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":{"items":[]}}'
            ;;
        "textDocument/inlayHint")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":[]}'
            ;;
        "$/cancelRequest")
            ;;
        "shutdown")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":null}'
            break
            ;;
    esac
done
"#;

    let script_path = std::env::temp_dir().join("fake_lsp_permissive_server.sh");
    std::fs::write(&script_path, script).expect("Failed to write permissive server script");

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

/// Verify that a strict LSP server sends diagnostics when the editor
/// advertises `textDocument.publishDiagnostics` capability (issue #1006).
///
/// This test creates a fake LSP server that checks client capabilities
/// (like pyright/rust-analyzer do) and only sends publishDiagnostics if
/// the client advertises the capability. The editor now advertises it,
/// so the server sends diagnostics.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses Bash-based fake LSP server
fn test_strict_server_sends_diagnostics_with_capability() -> anyhow::Result<()> {
    let _ = tracing_subscriber::fmt()
        .with_env_filter("fresh=debug")
        .try_init();

    let script_path = create_strict_server_script();

    let temp_dir = tempfile::tempdir()?;
    let log_file = temp_dir.path().join("strict_server_log.txt");
    let test_file = temp_dir.path().join("test.py");
    std::fs::write(&test_file, "def main():\n    x = 1\n")?;

    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "python".to_string(),
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

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    // Open the Python test file (triggers initialize + didOpen)
    harness.open_file(&test_file)?;
    harness.render()?;

    // Wait for the server to send diagnostics and the editor to display them
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.contains("ACTION:sending_diagnostics (capability present)")
    })?;

    // Read the server log
    let log = std::fs::read_to_string(&log_file)?;
    eprintln!("[TEST] Strict server log:\n{}", log);

    // Verify the server saw that publishDiagnostics capability IS advertised
    assert!(
        log.contains("CAPABILITY:publishDiagnostics=YES"),
        "Expected server to detect publishDiagnostics capability.\nLog:\n{}",
        log
    );

    Ok(())
}

/// Verify that a permissive server (like clangd) sends diagnostics regardless
/// of whether the client advertises publishDiagnostics capability.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses Bash-based fake LSP server
fn test_permissive_server_sends_diagnostics_without_capability() -> anyhow::Result<()> {
    let _ = tracing_subscriber::fmt()
        .with_env_filter("fresh=debug")
        .try_init();

    let script_path = create_permissive_server_script();

    let temp_dir = tempfile::tempdir()?;
    let log_file = temp_dir.path().join("permissive_server_log.txt");
    let test_file = temp_dir.path().join("test.c");
    std::fs::write(&test_file, "int main() { return 0; }\n")?;

    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "c".to_string(),
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

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    // Open the C test file
    harness.open_file(&test_file)?;
    harness.render()?;

    // Wait for didOpen to be logged
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.contains("METHOD:textDocument/didOpen")
    })?;

    // Wait for diagnostics to be received and rendered by the editor
    // The permissive server sends them immediately on didOpen
    harness.wait_until(|h| {
        let screen = h.screen_to_string();
        screen.contains("E:1")
    })?;

    let log = std::fs::read_to_string(&log_file)?;
    eprintln!("[TEST] Permissive server log:\n{}", log);

    // This server always sends diagnostics regardless of capability
    assert!(
        log.contains("ACTION:sending_diagnostics"),
        "Expected permissive server to send diagnostics.\nLog:\n{}",
        log
    );

    Ok(())
}
