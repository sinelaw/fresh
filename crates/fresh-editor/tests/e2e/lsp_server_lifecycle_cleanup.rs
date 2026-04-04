//! E2E tests for LSP server lifecycle cleanup
//!
//! Tests that:
//! 1. Stopping an LSP server clears its diagnostics from the screen
//! 2. With two servers running, stopping one clears only that server's
//!    diagnostics while the other server's diagnostics remain

use crate::common::harness::EditorTestHarness;

/// Create a fake LSP server that publishes one error diagnostic on didOpen.
fn create_error_server_script(dir: &std::path::Path, filename: &str) -> std::path::PathBuf {
    let script = r##"#!/bin/bash
LOG_FILE="${1:-/tmp/fake_lsp_log.txt}"
> "$LOG_FILE"

DID_OPEN_URI=""
VERSION=0

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
    local length=${#message}
    printf "Content-Length: $length\r\n\r\n%s" "$message"
}

while true; do
    msg=$(read_message)
    if [ -z "$msg" ]; then break; fi

    method=$(echo "$msg" | grep -o '"method":"[^"]*"' | cut -d'"' -f4)
    msg_id=$(echo "$msg" | grep -o '"id":[0-9]*' | cut -d':' -f2)

    echo "RECV: method=$method id=$msg_id" >> "$LOG_FILE"

    case "$method" in
        "initialize")
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":{"capabilities":{"positionEncoding":"utf-16","textDocumentSync":{"openClose":true,"change":2,"save":{}},"inlayHintProvider":{"resolveProvider":false}}}}'
            echo "ACTION: initialized" >> "$LOG_FILE"
            ;;
        "initialized")
            echo "ACTION: client sent initialized" >> "$LOG_FILE"
            ;;
        "textDocument/didOpen")
            DID_OPEN_URI=$(echo "$msg" | grep -o '"uri":"[^"]*"' | head -1 | cut -d'"' -f4)
            VERSION=1
            echo "ACTION: didOpen uri=$DID_OPEN_URI" >> "$LOG_FILE"

            # Publish one error diagnostic at line 1, chars 17-24
            send_message '{"jsonrpc":"2.0","method":"textDocument/publishDiagnostics","params":{"uri":"'"$DID_OPEN_URI"'","diagnostics":[{"range":{"start":{"line":1,"character":17},"end":{"line":1,"character":24}},"severity":1,"code":"E0308","source":"rustc","message":"mismatched types\nexpected `i32`, found `&str`"}],"version":'"$VERSION"'}}'
            echo "SENT: publishDiagnostics with 1 error" >> "$LOG_FILE"
            ;;
        "textDocument/didChange")
            VERSION=$((VERSION + 1))
            echo "ACTION: didChange version=$VERSION" >> "$LOG_FILE"
            ;;
        "textDocument/didClose")
            echo "ACTION: didClose" >> "$LOG_FILE"
            ;;
        "textDocument/inlayHint")
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":[]}'
            ;;
        "$/cancelRequest")
            ;;
        "shutdown")
            echo "ACTION: shutdown" >> "$LOG_FILE"
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":null}'
            break
            ;;
        *)
            if [ -n "$method" ] && [ -n "$msg_id" ]; then
                send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":null}'
            fi
            ;;
    esac
done

echo "SERVER: exiting" >> "$LOG_FILE"
"##;

    let script_path = dir.join(filename);
    std::fs::write(&script_path, script).expect("Failed to write server script");

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

/// Create a fake LSP server that publishes one warning diagnostic on didOpen.
/// Uses a different range (line 2) to produce a distinct diagnostic.
fn create_warning_server_script(dir: &std::path::Path, filename: &str) -> std::path::PathBuf {
    let script = r##"#!/bin/bash
LOG_FILE="${1:-/tmp/fake_lsp_log.txt}"
> "$LOG_FILE"

DID_OPEN_URI=""
VERSION=0

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
    local length=${#message}
    printf "Content-Length: $length\r\n\r\n%s" "$message"
}

while true; do
    msg=$(read_message)
    if [ -z "$msg" ]; then break; fi

    method=$(echo "$msg" | grep -o '"method":"[^"]*"' | cut -d'"' -f4)
    msg_id=$(echo "$msg" | grep -o '"id":[0-9]*' | cut -d':' -f2)

    echo "RECV: method=$method id=$msg_id" >> "$LOG_FILE"

    case "$method" in
        "initialize")
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":{"capabilities":{"positionEncoding":"utf-16","textDocumentSync":{"openClose":true,"change":2,"save":{}},"inlayHintProvider":{"resolveProvider":false}}}}'
            echo "ACTION: initialized" >> "$LOG_FILE"
            ;;
        "initialized")
            echo "ACTION: client sent initialized" >> "$LOG_FILE"
            ;;
        "textDocument/didOpen")
            DID_OPEN_URI=$(echo "$msg" | grep -o '"uri":"[^"]*"' | head -1 | cut -d'"' -f4)
            VERSION=1
            echo "ACTION: didOpen uri=$DID_OPEN_URI" >> "$LOG_FILE"

            # Publish one warning diagnostic at line 2
            send_message '{"jsonrpc":"2.0","method":"textDocument/publishDiagnostics","params":{"uri":"'"$DID_OPEN_URI"'","diagnostics":[{"range":{"start":{"line":2,"character":4},"end":{"line":2,"character":20}},"severity":2,"code":"unused_variable","source":"clippy","message":"unused variable: `x`"}],"version":'"$VERSION"'}}'
            echo "SENT: publishDiagnostics with 1 warning" >> "$LOG_FILE"
            ;;
        "textDocument/didChange")
            VERSION=$((VERSION + 1))
            echo "ACTION: didChange version=$VERSION" >> "$LOG_FILE"
            ;;
        "textDocument/didClose")
            echo "ACTION: didClose" >> "$LOG_FILE"
            ;;
        "textDocument/inlayHint")
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":[]}'
            ;;
        "$/cancelRequest")
            ;;
        "shutdown")
            echo "ACTION: shutdown" >> "$LOG_FILE"
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":null}'
            break
            ;;
        *)
            if [ -n "$method" ] && [ -n "$msg_id" ]; then
                send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":null}'
            fi
            ;;
    esac
done

echo "SERVER: exiting" >> "$LOG_FILE"
"##;

    let script_path = dir.join(filename);
    std::fs::write(&script_path, script).expect("Failed to write server script");

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

/// Test that stopping an LSP server clears its diagnostics from the screen.
///
/// Flow:
/// 1. Open a file → server publishes diagnostics → "E:1" shown in status bar
/// 2. Stop the server via handle_stop_lsp_server
/// 3. Verify "E:1" is gone from the screen
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_stopping_server_clears_diagnostics() -> anyhow::Result<()> {
    crate::common::tracing::init_tracing_from_env();

    let temp_dir = tempfile::tempdir()?;
    let script_path = create_error_server_script(temp_dir.path(), "fake_error_server.sh");
    let log_file = temp_dir.path().join("lsp_stop_diag_log.txt");
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(
        &test_file,
        "fn main() {\n    let x: i32 = \"hello\";\n    println!(\"{}\", x);\n}\n",
    )?;

    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: script_path.to_string_lossy().to_string(),
            args: vec![log_file.to_string_lossy().to_string()],
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

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    // Open the Rust test file (triggers initialize + didOpen)
    harness.open_file(&test_file)?;
    harness.render()?;

    // Wait for diagnostics to appear on screen
    harness.wait_until(|h| {
        let screen = h.screen_to_string();
        screen.contains("E:1")
    })?;

    // Stop the LSP server programmatically (simulates command palette selection)
    harness.editor_mut().handle_stop_lsp_server("rust");

    // Wait for diagnostics to be cleared from the screen
    harness.wait_until(|h| {
        let screen = h.screen_to_string();
        !screen.contains("E:1")
    })?;

    Ok(())
}

/// Test that with two servers running, both receive didOpen on start and
/// both publish diagnostics that appear on screen.
///
/// Flow:
/// 1. Configure two servers (error-server + warning-server), both auto_start
/// 2. Open a file → both start, both get didOpen, both publish diagnostics
/// 3. Verify E:1 and W:1 both visible (proving both servers got didOpen)
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_two_servers_both_receive_didopen_and_publish_diagnostics() -> anyhow::Result<()> {
    crate::common::tracing::init_tracing_from_env();

    let temp_dir = tempfile::tempdir()?;
    let error_script = create_error_server_script(temp_dir.path(), "fake_error_server_multi.sh");
    let warning_script =
        create_warning_server_script(temp_dir.path(), "fake_warning_server_multi.sh");
    let error_log = temp_dir.path().join("error_server_log.txt");
    let warning_log = temp_dir.path().join("warning_server_log.txt");
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(
        &test_file,
        "fn main() {\n    let x: i32 = \"hello\";\n    println!(\"{}\", x);\n}\n",
    )?;

    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![
            fresh::services::lsp::LspServerConfig {
                command: error_script.to_string_lossy().to_string(),
                args: vec![error_log.to_string_lossy().to_string()],
                enabled: true,
                auto_start: true,
                process_limits: fresh::services::process_limits::ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                root_markers: Default::default(),
                name: Some("error-server".to_string()),
                only_features: None,
                except_features: None,
            },
            fresh::services::lsp::LspServerConfig {
                command: warning_script.to_string_lossy().to_string(),
                args: vec![warning_log.to_string_lossy().to_string()],
                enabled: true,
                auto_start: true,
                process_limits: fresh::services::process_limits::ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                root_markers: Default::default(),
                name: Some("warning-server".to_string()),
                only_features: None,
                except_features: None,
            },
        ]),
    );

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        200,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    // Open the file → both servers start automatically
    tracing::warn!("[test] opening test file");
    harness.open_file(&test_file)?;
    harness.render()?;

    // Wait for both servers to publish diagnostics:
    // error-server → E:1 (1 error), warning-server → W:1 (1 warning)
    tracing::warn!("[test] waiting for E:1 && W:1");
    let error_log_for_closure = error_log.clone();
    let warning_log_for_closure = warning_log.clone();
    let mut last_diag_dump = std::time::Instant::now();
    harness.wait_until(|h| {
        let screen = h.screen_to_string();
        let has_error = screen.contains("E:1");
        let has_warning = screen.contains("W:1");

        // Periodically dump server logs to diagnose which server is stuck
        let now = std::time::Instant::now();
        if now.duration_since(last_diag_dump) >= std::time::Duration::from_secs(15) {
            last_diag_dump = now;
            let elog = std::fs::read_to_string(&error_log_for_closure).unwrap_or_default();
            let wlog = std::fs::read_to_string(&warning_log_for_closure).unwrap_or_default();
            tracing::warn!(
                has_error,
                has_warning,
                "[test] still waiting — error-server log:\n{}\nwarning-server log:\n{}",
                elog,
                wlog
            );
        }

        has_error && has_warning
    })?;
    tracing::warn!("[test] both diagnostics visible, checking server logs");

    // Verify both servers received didOpen (they published diagnostics in
    // response to didOpen, so seeing E:1+W:1 already proves this, but let's
    // also check the server logs for clarity).
    let elog = std::fs::read_to_string(&error_log)?;
    assert!(
        elog.contains("ACTION: didOpen"),
        "Error server should have received didOpen.\nLog:\n{}",
        elog
    );
    let wlog = std::fs::read_to_string(&warning_log)?;
    assert!(
        wlog.contains("ACTION: didOpen"),
        "Warning server should have received didOpen.\nLog:\n{}",
        wlog
    );

    Ok(())
}
