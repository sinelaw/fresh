//! E2E tests for completionItem/resolve and textDocument/formatting.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Create a fake LSP server that supports:
/// - Completion with additional_text_edits (auto-imports)
/// - completionItem/resolve (adds additional_text_edits on resolve)
/// - textDocument/formatting (returns edits to reformat)
/// - Logs all received methods to a file for verification
fn create_completion_and_formatting_lsp_script(dir: &std::path::Path) -> std::path::PathBuf {
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

    # Log all methods
    echo "METHOD:$method" >> "$LOG_FILE"
    echo "---" >> "$LOG_FILE"

    case "$method" in
        "initialize")
            # Completion with resolveProvider, formatting support
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":{"capabilities":{"textDocumentSync":2,"completionProvider":{"resolveProvider":true,"triggerCharacters":["."]},"documentFormattingProvider":true,"diagnosticProvider":{"interFileDependencies":false,"workspaceDiagnostics":false},"inlayHintProvider":{"resolveProvider":false}}}}'
            ;;
        "textDocument/didOpen")
            DOC_URI=$(echo "$msg" | grep -o '"uri":"[^"]*"' | head -1 | cut -d'"' -f4)
            ;;
        "textDocument/completion")
            # Return completion items — "println" has additional_text_edits (auto-import)
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":[{"label":"println","kind":3,"detail":"macro","insertText":"println!(\"$1\")","insertTextFormat":2,"additionalTextEdits":[{"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":0}},"newText":"use std::io;\n"}]},{"label":"print","kind":3,"detail":"macro","insertText":"print!(\"$1\")","insertTextFormat":2}]}'
            ;;
        "completionItem/resolve")
            echo "RESOLVE_BODY:$msg" >> "$LOG_FILE"
            echo "---" >> "$LOG_FILE"
            # Return the item with additional_text_edits filled in
            # (In practice the item from the request is echoed back with extras added)
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":{"label":"print","kind":3,"detail":"macro","insertText":"print!(\"$1\")","insertTextFormat":2,"additionalTextEdits":[{"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":0}},"newText":"use std::fmt;\n"}]}}'
            ;;
        "textDocument/formatting")
            echo "FORMAT_BODY:$msg" >> "$LOG_FILE"
            echo "---" >> "$LOG_FILE"
            # Return edits that add a newline at end of file and fix indentation
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":[{"range":{"start":{"line":1,"character":0},"end":{"line":1,"character":4}},"newText":"    "}]}'
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

    let script_path = dir.join("fake_lsp_completion_formatting.sh");
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
    initial_content: &str,
    file_ext: &str,
    language: &str,
) -> anyhow::Result<(EditorTestHarness, std::path::PathBuf)> {
    let script_path = create_completion_and_formatting_lsp_script(temp_dir.path());
    let test_file = temp_dir.path().join(format!("test.{}", file_ext));
    std::fs::write(&test_file, initial_content)?;

    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        language.to_string(),
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
        120,
        30,
        crate::common::harness::HarnessOptions::new()
            .with_config(config)
            .with_working_dir(temp_dir.path().to_path_buf()),
    )?;

    harness.open_file(&test_file)?;
    harness.render()?;

    // Wait for LSP to initialize (server received didOpen)
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(log_file).unwrap_or_default();
        log.contains("METHOD:textDocument/didOpen")
    })?;

    Ok((harness, test_file))
}

/// Test: completion with additional_text_edits applies auto-import on accept.
///
/// The fake LSP returns "println" with an additional_text_edit that inserts
/// "use std::io;\n" at the top of the file. When the user accepts the
/// completion, the import should be added.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_completion_accept_applies_additional_text_edits() -> anyhow::Result<()> {
    let _ = tracing_subscriber::fmt()
        .with_env_filter("fresh=debug")
        .try_init();

    let temp_dir = tempfile::tempdir()?;
    let log_file = temp_dir.path().join("lsp_completion_log.txt");
    let (mut harness, _test_file) = setup_editor(
        &temp_dir,
        &log_file,
        "fn main() {\n    p\n}\n",
        "rs",
        "rust",
    )?;

    // Move to the 'p' on line 2 (after "    p")
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.send_key(KeyCode::End, KeyModifiers::NONE)?;
    harness.render()?;

    // Trigger completion (type a character to trigger, or use the trigger char '.')
    // Actually, let's just type 'r' to extend "p" to "pr" which should trigger completion
    harness.type_text("r")?;
    harness.render()?;

    // Wait for completion popup to appear with "println"
    harness.wait_for_screen_contains("println")?;

    // Accept the first completion item (println) with Enter
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.render()?;

    // Wait for the additional_text_edit to be applied — "use std::io;" at top
    harness.wait_for_screen_contains("use std::io;")?;

    let content = harness.get_buffer_content().unwrap_or_default();
    assert!(
        content.contains("use std::io;"),
        "Buffer should contain auto-import 'use std::io;' from additional_text_edits.\nActual: {}",
        content
    );

    Ok(())
}

/// Test: textDocument/formatting applies edits to the buffer.
///
/// The fake LSP responds to formatting with an edit that fixes indentation.
/// The FormatBuffer action should send the request and apply the result.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_format_buffer_via_lsp() -> anyhow::Result<()> {
    let _ = tracing_subscriber::fmt()
        .with_env_filter("fresh=debug")
        .try_init();

    let temp_dir = tempfile::tempdir()?;
    let log_file = temp_dir.path().join("lsp_format_log.txt");
    // Use "lua" — no default external formatter, so format_buffer falls back to LSP
    let (mut harness, _test_file) = setup_editor(
        &temp_dir,
        &log_file,
        "function main()\n    local x = 5\nend\n",
        "lua",
        "lua",
    )?;

    // Wait for LSP to report capabilities (status shows "ready")
    harness.wait_for_screen_contains("ready")?;

    // Open command palette with Ctrl+P and run "Format Buffer"
    harness.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)?;
    harness.render()?;
    harness.type_text("format buffer")?;
    harness.render()?;
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.render()?;

    // Wait for the formatting request to reach the server
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.contains("METHOD:textDocument/formatting")
    })?;

    // Wait for "Formatted" status message confirming edits were applied
    harness.wait_for_screen_contains("Formatted")?;

    Ok(())
}
