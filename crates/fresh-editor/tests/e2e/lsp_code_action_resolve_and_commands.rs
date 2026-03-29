//! E2E tests for LSP code action resolve, executeCommand, and workspace/applyEdit.
//!
//! These tests verify the complete code action lifecycle:
//! 1. Code actions with `command` → workspace/executeCommand → workspace/applyEdit
//! 2. Code actions needing resolve → codeAction/resolve → apply edit
//! 3. Code actions with both `edit` and `command` → apply edit then execute command

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Create a fake LSP server that supports:
/// - Code actions with commands (triggers workspace/applyEdit during executeCommand)
/// - Code actions needing resolve (resolveProvider: true)
/// - Code actions with both edit and command
/// - Logs all received methods to a file for verification
fn create_full_code_action_lsp_script(dir: &std::path::Path) -> std::path::PathBuf {
    let script = r##"#!/bin/bash

LOG_FILE="${1:-/tmp/fake_lsp_log.txt}"
> "$LOG_FILE"

DOC_URI=""
NEXT_SERVER_REQ_ID=2000

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

    # Log all methods
    echo "METHOD:$method" >> "$LOG_FILE"
    if [ "$method" = "textDocument/didOpen" ] || [ "$method" = "textDocument/didChange" ] || [ "$method" = "workspace/executeCommand" ] || [ "$method" = "codeAction/resolve" ]; then
        echo "BODY:$msg" >> "$LOG_FILE"
    fi
    echo "---" >> "$LOG_FILE"

    case "$method" in
        "initialize")
            # Advertise codeActionProvider with resolveProvider: true
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":{"capabilities":{"textDocumentSync":2,"codeActionProvider":{"resolveProvider":true},"diagnosticProvider":{"interFileDependencies":false,"workspaceDiagnostics":false},"inlayHintProvider":{"resolveProvider":false}}}}'
            ;;
        "textDocument/didOpen")
            DOC_URI=$(echo "$msg" | grep -o '"uri":"[^"]*"' | head -1 | cut -d'"' -f4)
            ;;
        "textDocument/codeAction")
            # Return 3 code actions:
            # 1. "Command action" - has command only (no edit), will trigger applyEdit during executeCommand
            # 2. "Resolve action" - has data only (needs resolve to get edit)
            # 3. "Edit+Command action" - has both edit and command
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":[{"title":"Command action","kind":"refactor","command":{"title":"Run command","command":"test.applyChange","arguments":["arg1"]}},{"title":"Resolve action","kind":"refactor","data":{"needsResolve":true}},{"title":"Edit+Command action","kind":"refactor","edit":{"documentChanges":[{"textDocument":{"uri":"'"$DOC_URI"'","version":null},"edits":[{"range":{"start":{"line":1,"character":4},"end":{"line":1,"character":14}},"newText":"let x = 99;"}]}]},"command":{"title":"Post-edit command","command":"test.postEdit","arguments":[]}}]}'
            ;;
        "workspace/executeCommand")
            # Extract command name
            cmd_name=$(echo "$msg" | grep -o '"command":"[^"]*"' | cut -d'"' -f4)
            if [ "$cmd_name" = "test.applyChange" ]; then
                # Send workspace/applyEdit back to the client with a real edit
                NEXT_SERVER_REQ_ID=$((NEXT_SERVER_REQ_ID + 1))
                send_message '{"jsonrpc":"2.0","id":'$NEXT_SERVER_REQ_ID',"method":"workspace/applyEdit","params":{"label":"Command edit","edit":{"documentChanges":[{"textDocument":{"uri":"'"$DOC_URI"'","version":null},"edits":[{"range":{"start":{"line":1,"character":4},"end":{"line":1,"character":14}},"newText":"let x = 77;"}]}]}}}'
                # Read the applyEdit response (applied: true/false)
                apply_response=$(read_message)
                echo "APPLY_RESPONSE:$apply_response" >> "$LOG_FILE"
                echo "---" >> "$LOG_FILE"
            fi
            # Respond to executeCommand with null
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":null}'
            ;;
        "codeAction/resolve")
            # Fill in the edit for the resolve action
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":{"title":"Resolve action","kind":"refactor","data":{"needsResolve":true},"edit":{"documentChanges":[{"textDocument":{"uri":"'"$DOC_URI"'","version":null},"edits":[{"range":{"start":{"line":1,"character":4},"end":{"line":1,"character":14}},"newText":"let x = 88;"}]}]}}}'
            ;;
        "textDocument/diagnostic")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":{"items":[]}}'
            ;;
        "textDocument/inlayHint")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":[]}'
            ;;
        "shutdown")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":null}'
            break
            ;;
    esac
done
"##;

    let script_path = dir.join("fake_lsp_full_code_action.sh");
    std::fs::write(&script_path, script).expect("Failed to write fake LSP script");

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = std::fs::metadata(&script_path).unwrap().permissions();
        perms.set_mode(0o755);
        std::fs::set_permissions(&script_path, perms).unwrap();
    }

    script_path
}

fn setup_editor(
    temp_dir: &tempfile::TempDir,
    log_file: &std::path::Path,
) -> anyhow::Result<(EditorTestHarness, std::path::PathBuf)> {
    let script_path = create_full_code_action_lsp_script(temp_dir.path());
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn main() {\n    let x = 5;\n}\n")?;

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

    let mut harness = EditorTestHarness::create(
        80,
        24,
        crate::common::harness::HarnessOptions::new()
            .with_config(config)
            .with_working_dir(temp_dir.path().to_path_buf()),
    )?;

    harness.open_file(&test_file)?;
    harness.render()?;

    // Wait for LSP to initialize and process didOpen (verified via server log)
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(log_file).unwrap_or_default();
        log.contains("METHOD:textDocument/didOpen")
    })?;

    Ok((harness, test_file))
}

/// Trigger code actions and wait for the popup to appear.
fn trigger_code_actions(harness: &mut EditorTestHarness) -> anyhow::Result<()> {
    // Move to line 2
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.render()?;

    // Request code actions (Ctrl+.)
    harness.send_key(KeyCode::Char('.'), KeyModifiers::ALT)?;
    harness.render()?;

    // Wait for popup
    harness.wait_for_screen_contains("Command action")?;
    Ok(())
}

/// Test: code action with command only → executeCommand → workspace/applyEdit
///
/// The fake LSP returns a code action with only a `command` field. When the editor
/// sends workspace/executeCommand, the server responds with workspace/applyEdit
/// containing a real edit. The edit should be applied to the buffer.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_code_action_with_command_sends_execute_and_applies_edit() -> anyhow::Result<()> {
    let _ = tracing_subscriber::fmt()
        .with_env_filter("fresh=debug")
        .try_init();

    let temp_dir = tempfile::tempdir()?;
    let log_file = temp_dir.path().join("lsp_cmd_log.txt");
    let (mut harness, _test_file) = setup_editor(&temp_dir, &log_file)?;

    trigger_code_actions(&mut harness)?;

    // Select "Command action" (item 1)
    harness.send_key(KeyCode::Char('1'), KeyModifiers::NONE)?;
    harness.render()?;

    // Wait for executeCommand to be sent to the server
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.contains("METHOD:workspace/executeCommand")
    })?;

    // The server sends workspace/applyEdit during executeCommand, which replaces
    // "let x = 5;" with "let x = 77;". Wait for the edit to appear.
    harness.wait_for_screen_contains("77")?;

    // Verify the buffer was actually modified
    let content = harness.get_buffer_content().unwrap_or_default();
    assert!(
        content.contains("let x = 77;"),
        "Buffer should contain 'let x = 77;' after command execution.\nActual: {}",
        content
    );

    Ok(())
}

/// Test: code action needing resolve → codeAction/resolve → apply edit
///
/// The fake LSP returns a code action with only `data` (no edit, no command).
/// The editor must send codeAction/resolve to get the full action with edit,
/// then apply the edit.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_code_action_resolve_then_apply() -> anyhow::Result<()> {
    let _ = tracing_subscriber::fmt()
        .with_env_filter("fresh=debug")
        .try_init();

    let temp_dir = tempfile::tempdir()?;
    let log_file = temp_dir.path().join("lsp_resolve_log.txt");
    let (mut harness, _test_file) = setup_editor(&temp_dir, &log_file)?;

    trigger_code_actions(&mut harness)?;

    // Select "Resolve action" (item 2)
    harness.send_key(KeyCode::Char('2'), KeyModifiers::NONE)?;
    harness.render()?;

    // Wait for codeAction/resolve to be sent
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.contains("METHOD:codeAction/resolve")
    })?;

    // The resolved action has an edit that replaces "let x = 5;" with "let x = 88;"
    harness.wait_for_screen_contains("88")?;

    let content = harness.get_buffer_content().unwrap_or_default();
    assert!(
        content.contains("let x = 88;"),
        "Buffer should contain 'let x = 88;' after resolve+apply.\nActual: {}",
        content
    );

    Ok(())
}

/// Test: code action with both edit and command → apply edit then execute command
///
/// The fake LSP returns a code action with both `edit` (replaces text) and
/// `command` (sent to server). The editor must apply the edit first, then
/// send workspace/executeCommand.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_code_action_with_edit_and_command() -> anyhow::Result<()> {
    let _ = tracing_subscriber::fmt()
        .with_env_filter("fresh=debug")
        .try_init();

    let temp_dir = tempfile::tempdir()?;
    let log_file = temp_dir.path().join("lsp_edit_cmd_log.txt");
    let (mut harness, _test_file) = setup_editor(&temp_dir, &log_file)?;

    trigger_code_actions(&mut harness)?;

    // Select "Edit+Command action" (item 3)
    harness.send_key(KeyCode::Char('3'), KeyModifiers::NONE)?;
    harness.render()?;

    // The edit replaces "let x = 5;" with "let x = 99;"
    harness.wait_for_screen_contains("99")?;

    // The command should also be sent to the server
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.contains("METHOD:workspace/executeCommand")
    })?;

    // Verify the edit was applied
    let content = harness.get_buffer_content().unwrap_or_default();
    assert!(
        content.contains("let x = 99;"),
        "Buffer should contain 'let x = 99;' from the edit.\nActual: {}",
        content
    );

    // Verify the command was sent (check log for executeCommand with test.postEdit)
    let log = std::fs::read_to_string(&log_file)?;
    assert!(
        log.contains("test.postEdit"),
        "Server should have received executeCommand with test.postEdit.\nLog: {}",
        log
    );

    Ok(())
}
