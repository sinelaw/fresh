//! E2E tests for the global LSP master switch (GitHub issue #1770).
//!
//! Users asked for "a general configuration to disable the LSP feature for
//! all languages" — previously LSP could only be disabled per language
//! (per-server `enabled` / `auto_start` flags). The top-level
//! `lsp_enabled: false` config field must:
//!
//! - block auto-start of every configured server on buffer load —
//!   per-language and universal servers alike,
//! - keep the status-bar pill visible ("LSP (off)", dimmed) so the user
//!   still has a discoverable surface for LSP,
//! - NOT block manual starts: "Start/Restart LSP Server" from the command
//!   palette is an explicit user action and overrides the global opt-out,
//!   exactly like it already overrides per-server `enabled=false`.
//!
//! Verification strategy (mirrors `lsp_autostart_selective`): each fake
//! server's first action is creating its log file, so file non-existence
//! proves the process was never spawned. Spawned servers publish one
//! diagnostic on didOpen, which renders as a count ("E:1") on the status
//! bar, so positive assertions observe rendered output only.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Create a fake LSP server script whose first action is creating its log
/// file, and which publishes one error diagnostic on `didOpen`.
fn create_diagnostic_server_script(dir: &std::path::Path, filename: &str) -> std::path::PathBuf {
    let script = r##"#!/bin/bash
LOG_FILE="$1"
SEVERITY="${2:-1}"
> "$LOG_FILE"
echo "ACTION: spawned severity=$SEVERITY" >> "$LOG_FILE"

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
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":{"capabilities":{"positionEncoding":"utf-16","textDocumentSync":{"openClose":true,"change":2,"save":{}}}}}'
            ;;
        "initialized")
            ;;
        "textDocument/didOpen")
            uri=$(echo "$msg" | grep -o '"uri":"[^"]*"' | head -1 | cut -d'"' -f4)
            send_message '{"jsonrpc":"2.0","method":"textDocument/publishDiagnostics","params":{"uri":"'"$uri"'","diagnostics":[{"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":1}},"severity":'"$SEVERITY"',"source":"fake","message":"sev'"$SEVERITY"'"}],"version":1}}'
            echo "ACTION: didOpen published severity=$SEVERITY" >> "$LOG_FILE"
            ;;
        "shutdown")
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

fn build_server_config(
    name: &str,
    script: &std::path::Path,
    log_path: &std::path::Path,
    severity: u8,
) -> fresh::services::lsp::LspServerConfig {
    fresh::services::lsp::LspServerConfig {
        command: script.to_string_lossy().to_string(),
        args: Some(vec![log_path.to_string_lossy().to_string(), severity.to_string()]),
        enabled: true,
        auto_start: true,
        process_limits: fresh::services::process_limits::ProcessLimits::default(),
        initialization_options: None,
        env: Default::default(),
        language_id_overrides: Default::default(),
        root_markers: Default::default(),
        name: Some(name.to_string()),
        only_features: None,
        except_features: None,
    }
}

/// `lsp_enabled: false` must block auto-start of every configured server —
/// the per-language server AND the universal server, both individually
/// enabled+auto_start. The status bar still shows the "LSP (off)" pill
/// (the language has configured servers), and no diagnostic count appears
/// because no server ever spawned.
///
/// Without the fix, both servers spawn on buffer load: their log files
/// appear and the published diagnostics render as "E:1"/"W:1", so the
/// wait below never sees "LSP (off)" without "E:" alongside it.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses Bash-based fake LSP server
fn test_lsp_enabled_false_blocks_all_autostart() -> anyhow::Result<()> {
    crate::common::tracing::init_tracing_from_env();

    let temp_dir = tempfile::tempdir()?;
    let script = create_diagnostic_server_script(temp_dir.path(), "fake_lsp_global_disable.sh");

    let log_rust = temp_dir.path().join("rust_auto.log");
    let log_universal = temp_dir.path().join("universal_auto.log");

    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn main() {}\n")?;

    let mut config = fresh::config::Config::default();
    config.lsp_enabled = false;

    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![build_server_config(
            "rust-auto",
            &script,
            &log_rust,
            1, // error
        )]),
    );
    config.universal_lsp.insert(
        "universal-auto".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![build_server_config(
            "universal-auto",
            &script,
            &log_universal,
            2, // warning
        )]),
    );

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        200,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    harness.open_file(&test_file)?;
    harness.render()?;

    // The pill must surface the dormant state: servers are configured for
    // rust, but globally disabled. If the master switch were ignored, the
    // server would be Starting/Running by now (try_spawn registers the
    // handle synchronously on buffer open) and the pill would render
    // "LSP (on)" or a spinner instead — this wait would never satisfy.
    harness.wait_until(|h| h.get_status_bar().contains("LSP (off)"))?;

    // No diagnostics can have been published by unspawned servers.
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("E:1") && !screen.contains("W:1"),
        "No diagnostics should render when LSP is globally disabled — no \
         server should have spawned. Screen:\n{}",
        screen
    );

    // Belt-and-braces: each script creates its log file as its very first
    // action, so file non-existence proves the process was never spawned.
    assert!(
        !log_rust.exists(),
        "Per-language server must NOT spawn when lsp_enabled=false. \
         Log file unexpectedly exists: {:?}",
        log_rust
    );
    assert!(
        !log_universal.exists(),
        "Universal server must NOT spawn when lsp_enabled=false. \
         Log file unexpectedly exists: {:?}",
        log_universal
    );

    Ok(())
}

/// Manual start must still work while LSP is globally disabled: an
/// explicit "Start/Restart LSP Server" action overrides the global
/// opt-out, exactly like it already overrides per-server `enabled=false`.
/// The spawned server publishes an error diagnostic, which must reach the
/// status bar — proving the full spawn + didOpen flow works under the
/// global switch.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses Bash-based fake LSP server
fn test_lsp_enabled_false_still_allows_manual_start() -> anyhow::Result<()> {
    crate::common::tracing::init_tracing_from_env();

    let temp_dir = tempfile::tempdir()?;
    let script = create_diagnostic_server_script(temp_dir.path(), "fake_lsp_manual_start.sh");
    let log_rust = temp_dir.path().join("rust_manual.log");

    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn main() {}\n")?;

    let mut config = fresh::config::Config::default();
    config.lsp_enabled = false;

    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![build_server_config(
            "rust-manual",
            &script,
            &log_rust,
            1, // error
        )]),
    );

    // Bind Alt+R to the manual start/restart action (single-server config
    // restarts immediately, no prompt).
    config.keybindings.push(fresh::config::Keybinding {
        key: "r".to_string(),
        modifiers: vec!["alt".to_string()],
        keys: vec![],
        action: "lsp_restart".to_string(),
        args: std::collections::HashMap::new(),
        when: None,
    });

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        200,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    harness.open_file(&test_file)?;
    harness.render()?;

    // Auto-start is suppressed by the global switch.
    harness.wait_until(|h| h.get_status_bar().contains("LSP (off)"))?;
    assert!(
        !log_rust.exists(),
        "Server must not auto-start while lsp_enabled=false"
    );

    // Manual start: explicit user action overrides the global opt-out.
    harness.send_key(KeyCode::Char('r'), KeyModifiers::ALT)?;
    harness.render()?;

    // The manually started server receives didOpen and publishes an error
    // diagnostic that must render on the status bar.
    harness.wait_until(|h| h.screen_to_string().contains("E:1"))?;

    let log = std::fs::read_to_string(&log_rust)?;
    assert!(
        log.contains("ACTION: didOpen"),
        "Manually started server should have received didOpen. Log:\n{}",
        log
    );

    Ok(())
}
