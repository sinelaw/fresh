//! E2E test for LSP desync after BulkEdit undo
//!
//! When a BulkEdit (from code actions, rename, etc.) is undone, the editor must
//! send a didChange to keep the LSP server in sync. BulkEdit restores a tree
//! snapshot so there are no incremental byte positions to convert — the editor
//! must send a full-document replacement.
//!
//! Previous bug flow (before fix):
//! 1. Open file → didOpen sent
//! 2. Apply code action → BulkEdit applied, didChange sent correctly
//!    (via apply_events_to_buffer_as_bulk_edit)
//! 3. Undo → BulkEdit inverse applied via apply_event_to_active_buffer,
//!    but collect_lsp_changes(BulkEdit) returns empty → NO didChange sent
//! 4. Server now has stale content → diagnostics at wrong positions,
//!    code actions crash with ArgumentOutOfRangeException
//!
//! Fixed flow:
//! 3. Undo → BulkEdit detected as buffer-modifying with no incremental changes →
//!    full-document didChange sent → server stays in sync

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Create a fake LSP server that:
/// 1. Returns a code action with a real workspace edit (replaces text)
/// 2. Logs all didOpen/didChange messages to a file for verification
fn create_code_action_logging_lsp_script(dir: &std::path::Path) -> std::path::PathBuf {
    // The script captures the URI from didOpen and returns a code action that
    // replaces "let x = 5;" with "let x = 42;" using that URI.
    let script = r##"#!/bin/bash

LOG_FILE="${1:-/tmp/fake_lsp_log.txt}"
> "$LOG_FILE"

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
    local length=${#message}
    printf "Content-Length: $length\r\n\r\n%s" "$message"
}

while true; do
    msg=$(read_message)
    if [ -z "$msg" ]; then break; fi

    method=$(echo "$msg" | grep -o '"method":"[^"]*"' | cut -d'"' -f4)
    msg_id=$(echo "$msg" | grep -o '"id":[0-9]*' | cut -d':' -f2)

    case "$method" in
        "textDocument/didOpen")
            DOC_URI=$(echo "$msg" | grep -o '"uri":"[^"]*"' | head -1 | cut -d'"' -f4)
            echo "METHOD:textDocument/didOpen" >> "$LOG_FILE"
            echo "BODY:$msg" >> "$LOG_FILE"
            echo "---" >> "$LOG_FILE"
            ;;
        "textDocument/didChange")
            echo "METHOD:textDocument/didChange" >> "$LOG_FILE"
            echo "BODY:$msg" >> "$LOG_FILE"
            echo "---" >> "$LOG_FILE"
            ;;
    esac

    case "$method" in
        "initialize")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":{"capabilities":{"textDocumentSync":2,"codeActionProvider":true,"diagnosticProvider":{"interFileDependencies":false,"workspaceDiagnostics":false},"inlayHintProvider":{"resolveProvider":false}}}}'
            ;;
        "textDocument/codeAction")
            # Return a code action that replaces "let x = 5;" (line 1, chars 4-14)
            # with "let x = 42;" — a real workspace edit
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":[{"title":"Change value","kind":"refactor","edit":{"documentChanges":[{"textDocument":{"uri":"'"$DOC_URI"'","version":null},"edits":[{"range":{"start":{"line":1,"character":4},"end":{"line":1,"character":14}},"newText":"let x = 42;"}]}]}}]}'
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
        "shutdown")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":null}'
            break
            ;;
    esac
done
"##;

    let script_path = dir.join("fake_lsp_code_action_undo.sh");
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

/// Test that undoing a code action (BulkEdit) sends didChange to the LSP server.
///
/// Reproduces the bug where applying a code action and undoing it desynchronizes
/// the LSP server's document state, causing wrong diagnostic positions and
/// ArgumentOutOfRangeException on subsequent requests.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_code_action_undo_sends_did_change() -> anyhow::Result<()> {
    let _ = tracing_subscriber::fmt()
        .with_env_filter("fresh=debug")
        .try_init();

    let temp_dir = tempfile::tempdir()?;
    let script_path = create_code_action_logging_lsp_script(temp_dir.path());
    let log_file = temp_dir.path().join("lsp_code_action_undo_log.txt");
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

    // Open file, wait for LSP ready
    harness.open_file(&test_file)?;
    harness.render()?;
    harness.wait_for_screen_contains("ready")?;

    // Wait for didOpen
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.contains("METHOD:textDocument/didOpen")
    })?;

    // Move cursor to line 2 (the "let x = 5;" line)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.render()?;

    // Request code actions (Alt+.)
    harness.send_key(KeyCode::Char('.'), KeyModifiers::ALT)?;
    harness.render()?;

    // Wait for the code action popup
    harness.wait_for_screen_contains("Change value")?;

    // Count didChange before applying the code action
    let log_before_action = std::fs::read_to_string(&log_file)?;
    let changes_before_action = log_before_action
        .matches("METHOD:textDocument/didChange")
        .count();

    // Select the code action (press Enter or 1)
    harness.send_key(KeyCode::Char('1'), KeyModifiers::NONE)?;
    harness.render()?;

    // Wait for didChange from the code action application
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.matches("METHOD:textDocument/didChange").count() > changes_before_action
    })?;

    // Verify the code action was applied — "42" should be on screen
    harness.wait_for_screen_contains("42")?;

    // Count didChange before undo
    let log_before_undo = std::fs::read_to_string(&log_file)?;
    let changes_before_undo = log_before_undo
        .matches("METHOD:textDocument/didChange")
        .count();

    // Undo the code action (Ctrl+Z)
    harness.send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)?;
    harness.render()?;

    // Verify undo was applied — original "5" should be back
    harness.wait_for_screen_contains("let x = 5")?;

    // THIS IS THE KEY ASSERTION: undo must send didChange to keep server in sync.
    // Before the fix, no didChange was sent for BulkEdit undo, causing the LSP
    // server to retain the post-code-action content while the editor reverted.
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.matches("METHOD:textDocument/didChange").count() > changes_before_undo
    })?;

    // Verify the undo didChange contains the original content (not "42")
    let final_log = std::fs::read_to_string(&log_file)?;
    let last_change_idx = final_log.rfind("METHOD:textDocument/didChange").unwrap();
    let last_change_body = &final_log[last_change_idx..];

    // The undo sends a full-document replacement with the reverted content
    assert!(
        !last_change_body.contains("42"),
        "After undo, the last didChange should NOT contain '42' (the code action value).\n\
         Last didChange: {}",
        last_change_body
    );

    Ok(())
}
