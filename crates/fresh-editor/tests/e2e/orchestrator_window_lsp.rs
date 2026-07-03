//! Regression (e2e): a window opened through the orchestrator drives
//! its *own* LSP. Opening a code buffer in a dived-into orchestrator
//! window must bring the language server up exactly as it does in the
//! base window — observed purely through the status-bar indicator
//! flipping to `"LSP (on)"`.
//!
//! Before the fix only the base window (id 1) owned an `LspManager`;
//! windows the orchestrator created started with none, so a code buffer
//! opened in one left LSP dead and the status popup's "Start <server>"
//! reported *"No LSP manager available"*. Now every window builds its
//! own manager in `Window::new`, so the server comes up in any window.
//!
//! Without the fix the orchestrator window never spawns the server, the
//! indicator stays `"LSP (off)"`, and `wait_until` blocks until the
//! external test runner times out — i.e. the test fails. With the fix
//! the indicator reaches `"LSP (on)"` and the wait returns.
//!
//! Per CONTRIBUTING (e2e observes, not inspects) the assertion is on
//! rendered output only; the structural invariant ("every window owns a
//! manager") is pinned separately by the unit test in `e2e::sessions`.

use crate::common::harness::EditorTestHarness;

/// Minimal fake LSP server: answers `initialize` (advertising basic
/// sync capabilities) and stays running. Enough for the editor to mark
/// the server `Running` and paint `"LSP (on)"` — no real-language
/// server, so the test is hermetic and deterministic.
fn create_fake_lsp_server(dir: &std::path::Path) -> std::path::PathBuf {
    let script = r##"#!/bin/bash
LOG_FILE="${1:-/tmp/orch_lsp_log.txt}"
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
    echo "RECV: $method id=$msg_id" >> "$LOG_FILE"
    case "$method" in
        "initialize")
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":{"capabilities":{"positionEncoding":"utf-16","textDocumentSync":{"openClose":true,"change":2,"save":{}}}}}'
            ;;
        "initialized") ;;
        "shutdown")
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":null}'
            break
            ;;
        "exit") break ;;
        *)
            if [ -n "$method" ] && [ -n "$msg_id" ]; then
                send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":null}'
            fi
            ;;
    esac
done
echo "SERVER: exiting" >> "$LOG_FILE"
"##;

    let script_path = dir.join("fake_orch_lsp.sh");
    std::fs::write(&script_path, script).expect("failed to write fake server");

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = std::fs::metadata(&script_path).unwrap().permissions();
        perms.set_mode(0o755);
        std::fs::set_permissions(&script_path, perms).unwrap();
    }

    script_path
}

/// Build a Config whose `rust` language auto-starts the fake server.
fn config_with_fake_rust_lsp(
    script: &std::path::Path,
    log: &std::path::Path,
) -> fresh::config::Config {
    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: script.to_string_lossy().to_string(),
            args: Some(vec![log.to_string_lossy().to_string()]),
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
    config
}

/// Opening a code buffer in a dived-into orchestrator window brings the
/// server up — the status bar reads `"LSP (on)"`, proving that window
/// owns and drives its own manager (not just the base window).
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn orchestrator_window_drives_its_own_lsp_indicator() -> anyhow::Result<()> {
    let base_dir = tempfile::tempdir()?;
    let script = create_fake_lsp_server(base_dir.path());
    let log = base_dir.path().join("orch_lsp_log.txt");
    let config = config_with_fake_rust_lsp(&script, &log);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        config,
        base_dir.path().to_path_buf(),
    )?;

    // Spawn a second window the way the orchestrator does (one window
    // per project root) and dive into it — there's no keyboard path to
    // create an orchestrator session without the full plugin form, so
    // the multi-window setup uses the same helpers as the existing
    // next/prev-window e2e tests. The *assertion* below is on rendered
    // output only.
    let agent_dir = tempfile::tempdir()?;
    let agent_id = harness
        .editor_mut()
        .create_window_at(agent_dir.path().to_path_buf(), "agent".to_string());
    harness.editor_mut().set_active_window(agent_id);

    // Open a rust buffer in the orchestrator window.
    let file = agent_dir.path().join("main.rs");
    std::fs::write(&file, "fn main() {}\n")?;
    harness.open_file(&file)?;
    harness.render()?;

    // The orchestrator window's own LSP manager must auto-start the
    // server. Observed only through the status-bar pill. Without the fix
    // this window has no manager, the server never starts, the pill
    // stays "LSP (off)", and this wait blocks until the runner times out.
    harness.wait_until(|h| h.screen_to_string().contains("LSP (on)"))?;
    assert!(
        harness.screen_to_string().contains("LSP (on)"),
        "orchestrator window should drive its own LSP — status bar must \
         read 'LSP (on)' for a rust buffer opened in it.\nScreen:\n{}",
        harness.screen_to_string()
    );

    // Keep the temp roots alive until the assertions are done.
    drop(agent_dir);
    drop(base_dir);
    Ok(())
}
