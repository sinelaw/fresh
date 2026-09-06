//! End-to-end coverage for rendering and executing LSP code lenses.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};

fn create_code_lens_lsp_script(dir: &std::path::Path) -> std::path::PathBuf {
    let script = r##"#!/bin/bash

LOG_FILE="$1"
> "$LOG_FILE"

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
    if [ "$content_length" -gt 0 ]; then
        dd bs=1 count="$content_length" 2>/dev/null
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

    echo "METHOD:$method" >> "$LOG_FILE"
    echo "BODY:$msg" >> "$LOG_FILE"

    case "$method" in
        "initialize")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":{"capabilities":{"textDocumentSync":2,"codeLensProvider":{"resolveProvider":false}}}}'
            ;;
        "textDocument/codeLens")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":[{"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":0}},"command":{"title":"Run Test","command":"test.run","arguments":["unit"]}}]}'
            ;;
        "workspace/executeCommand")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":null}'
            ;;
        "shutdown")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":null}'
            break
            ;;
    esac
done
"##;

    let script_path = dir.join("fake_code_lens_lsp.sh");
    std::fs::write(&script_path, script).expect("failed to write fake LSP script");

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut permissions = std::fs::metadata(&script_path).unwrap().permissions();
        permissions.set_mode(0o755);
        std::fs::set_permissions(&script_path, permissions).unwrap();
    }

    script_path
}

#[test]
#[cfg_attr(windows, ignore = "uses a Bash fake LSP server")]
fn test_code_lens_renders_and_executes_command() -> anyhow::Result<()> {
    let temp_dir = tempfile::tempdir()?;
    let log_file = temp_dir.path().join("code_lens.log");
    let script_path = create_code_lens_lsp_script(temp_dir.path());
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn main() {}\n")?;

    let mut config = fresh::config::Config::default();
    config.editor.enable_code_lens = true;
    config.editor.enable_inlay_hints = false;
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: script_path.to_string_lossy().to_string(),
            args: Some(vec![log_file.to_string_lossy().to_string()]),
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
        HarnessOptions::new()
            .with_config(config)
            .with_working_dir(temp_dir.path().to_path_buf()),
    )?;
    harness.open_file(&test_file)?;
    harness.render()?;

    harness.wait_until(|h| h.editor().active_window().is_lsp_server_ready("rust"))?;
    harness.wait_for_screen_contains("Run Test")?;
    assert!(
        harness.screen_to_string().contains("Run Test"),
        "code lens should render as a virtual line"
    );

    harness.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)?;
    harness.type_text("Execute Code Lens")?;
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.render()?;
    harness.wait_for_screen_contains("Code Lenses")?;
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.render()?;

    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.contains("METHOD:workspace/executeCommand")
            && log.contains("\"command\":\"test.run\"")
            && log.contains("\"arguments\":[\"unit\"]")
    })?;

    Ok(())
}
