//! E2E tests: LSP lifecycle visibility for configured-but-dormant servers.
//!
//! Context: `docs/internal/LSP_HEURISTIC_EVAL_CLANGD.md` raises as its
//! top concern (H-1) that a user opening a buffer whose language has an
//! LSP *configured* but with `auto_start = false` has no way to tell the
//! LSP exists — the status bar is indistinguishable from "no LSP at all."
//!
//! The fix widens the LSP status bar segment to describe the full LSP
//! situation for the active buffer, not just the servers that have
//! already spawned:
//!
//! | Running | Configured | status-bar text | indicator state |
//! | ------- | ---------- | --------------- | --------------- |
//! | 0       | 0          | `""`            | None            |
//! | 0       | N > 0      | `"LSP (off)"`   | Off             |
//! | M > 0   | any        | `"LSP (on)"`    | On              |
//! | any err | any        | `"LSP (error)"` | Error           |
//!
//! These tests cover the dormant (row 2) and empty (row 1) cases; the
//! running/error rows are covered by the broader LSP e2e suite.

use crate::common::harness::{EditorTestHarness, HarnessOptions};

/// Fake LSP server that writes a marker line to a log file on startup.
/// The marker's presence proves the server process was actually spawned;
/// its absence proves the server stayed dormant.
fn create_spawn_marker_script(dir: &std::path::Path, filename: &str) -> std::path::PathBuf {
    let script = r##"#!/bin/bash
LOG_FILE="$1"
echo "SPAWNED pid=$$" > "$LOG_FILE"

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
        "initialize")
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":{"capabilities":{"positionEncoding":"utf-16","textDocumentSync":{"openClose":true,"change":2,"save":{}}}}}'
            ;;
        "textDocument/didOpen")
            # If the editor ever did open the document against a dormant
            # server, publish a red error diagnostic so a stray "E:1" would
            # appear on the status bar — making any leak loud.
            uri=$(echo "$msg" | grep -o '"uri":"[^"]*"' | head -1 | cut -d'"' -f4)
            send_message '{"jsonrpc":"2.0","method":"textDocument/publishDiagnostics","params":{"uri":"'"$uri"'","diagnostics":[{"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":1}},"severity":1,"source":"dormant-should-not-publish","message":"leak"}],"version":1}}'
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

/// Opening a buffer whose language has a single configured-but-dormant
/// server must surface the dormant state on the status bar ("LSP (off)").
/// The server must not actually spawn, and no diagnostic may leak through.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses Bash-based fake LSP server
fn test_dormant_lsp_renders_off_indicator_on_status_bar() -> anyhow::Result<()> {
    let _ = tracing_subscriber::fmt()
        .with_env_filter("fresh=warn")
        .try_init();

    let temp = tempfile::tempdir()?;
    let script = create_spawn_marker_script(temp.path(), "fake_lsp_dormant.sh");
    let marker = temp.path().join("spawn_marker.log");
    let file = temp.path().join("hello.rs");
    std::fs::write(&file, "fn main() {}\n")?;

    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: script.to_string_lossy().to_string(),
            args: vec![marker.to_string_lossy().to_string()],
            enabled: true,
            auto_start: false, // configured but dormant
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
            env: Default::default(),
            language_id_overrides: Default::default(),
            root_markers: Default::default(),
            name: Some("rust-dormant".to_string()),
            only_features: None,
            except_features: None,
        }]),
    );

    let mut harness = EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new()
            .with_config(config)
            .with_working_dir(temp.path().to_path_buf()),
    )?;

    harness.open_file(&file)?;

    // Wait for the dormant indicator to show up. Semantic wait — keeps
    // the test stable even when buffer-activation + status-bar plumbing
    // needs a tick or two to settle.
    harness.wait_until(|h| h.get_status_bar().contains("LSP (off)"))?;

    // The spawn marker would have been written as the script's very
    // first action on startup. Its absence proves the server was never
    // spawned — the indicator is not a side-effect of starting the LSP.
    assert!(
        !marker.exists(),
        "auto_start=false server must remain dormant, but a spawn marker \
         was written at {:?}",
        marker
    );

    // The fake script publishes an error diagnostic on didOpen. If the
    // editor wrongly routed the buffer through the dormant server, "E:1"
    // would appear. Its absence corroborates the marker check.
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("E:1"),
        "Dormant LSP must not publish diagnostics. Screen:\n{}",
        screen
    );

    Ok(())
}

/// Opening a buffer whose language has two configured-but-dormant servers
/// must still surface the dormant state on the status bar ("LSP (off)").
/// The indicator collapses to a single "off" label regardless of the
/// server count — the popup (opened by clicking) lists per-server detail.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses Bash-based fake LSP server
fn test_dormant_lsp_count_reflects_configured_servers() -> anyhow::Result<()> {
    let _ = tracing_subscriber::fmt()
        .with_env_filter("fresh=warn")
        .try_init();

    let temp = tempfile::tempdir()?;
    let script = create_spawn_marker_script(temp.path(), "fake_lsp_dormant_two.sh");
    let marker_a = temp.path().join("spawn_marker_a.log");
    let marker_b = temp.path().join("spawn_marker_b.log");
    let file = temp.path().join("hello.rs");
    std::fs::write(&file, "fn main() {}\n")?;

    let build = |name: &str, marker: &std::path::Path| fresh::services::lsp::LspServerConfig {
        command: script.to_string_lossy().to_string(),
        args: vec![marker.to_string_lossy().to_string()],
        enabled: true,
        auto_start: false,
        process_limits: fresh::services::process_limits::ProcessLimits::default(),
        initialization_options: None,
        env: Default::default(),
        language_id_overrides: Default::default(),
        root_markers: Default::default(),
        name: Some(name.to_string()),
        only_features: None,
        except_features: None,
    };

    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![
            build("rust-dormant-a", &marker_a),
            build("rust-dormant-b", &marker_b),
        ]),
    );

    let mut harness = EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new()
            .with_config(config)
            .with_working_dir(temp.path().to_path_buf()),
    )?;

    harness.open_file(&file)?;
    harness.wait_until(|h| h.get_status_bar().contains("LSP (off)"))?;

    assert!(
        !marker_a.exists() && !marker_b.exists(),
        "Neither dormant server should spawn. markers: a={:?} exists={}, b={:?} exists={}",
        marker_a,
        marker_a.exists(),
        marker_b,
        marker_b.exists()
    );

    Ok(())
}

/// A buffer whose language has no LSP configured must not render the
/// dormant indicator. Uses `.txt` (plain text) because the default
/// config ships a pre-configured `rust-analyzer` entry for `rust` with
/// `enabled=true, auto_start=false` — which, with this change, *is*
/// treated as dormant. "Plain text" has no default LSP entry.
///
/// Control case — proves the indicator is driven by configuration, not
/// by something global that would follow the user across every buffer.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_buffer_with_no_lsp_configured_renders_no_indicator() -> anyhow::Result<()> {
    let _ = tracing_subscriber::fmt()
        .with_env_filter("fresh=warn")
        .try_init();

    let temp = tempfile::tempdir()?;
    let file = temp.path().join("notes.txt");
    std::fs::write(&file, "hello\n")?;

    let config = fresh::config::Config::default();

    let mut harness = EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new()
            .with_config(config)
            .with_working_dir(temp.path().to_path_buf()),
    )?;

    harness.open_file(&file)?;

    // Wait for the "opened" status to settle, then assert the LSP
    // segment is absent.
    harness.wait_until(|h| h.get_status_bar().contains("notes.txt"))?;

    let status = harness.get_status_bar();
    assert!(
        !status.contains("LSP"),
        "No LSP is configured for this buffer's language — status bar \
         must not mention LSP. Status: {:?}",
        status
    );

    Ok(())
}

/// Switching from a buffer whose language has a dormant LSP to a buffer
/// whose language has none must clear the dormant indicator. Without this
/// refresh, the stale "LSP (off)" would follow the user across buffers.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_dormant_indicator_refreshes_on_buffer_switch() -> anyhow::Result<()> {
    let _ = tracing_subscriber::fmt()
        .with_env_filter("fresh=warn")
        .try_init();

    let temp = tempfile::tempdir()?;
    let script = create_spawn_marker_script(temp.path(), "fake_lsp_switch.sh");
    let marker = temp.path().join("spawn_marker_switch.log");

    let rust_file = temp.path().join("hello.rs");
    std::fs::write(&rust_file, "fn main() {}\n")?;
    // The `text` language has no default LSP config, so switching to a
    // plain-text buffer should clear the dormant indicator. Use an
    // extension the built-in detector maps to "text".
    let txt_file = temp.path().join("notes.txt");
    std::fs::write(&txt_file, "hello\n")?;

    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: script.to_string_lossy().to_string(),
            args: vec![marker.to_string_lossy().to_string()],
            enabled: true,
            auto_start: false,
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
            env: Default::default(),
            language_id_overrides: Default::default(),
            root_markers: Default::default(),
            name: Some("rust-dormant".to_string()),
            only_features: None,
            except_features: None,
        }]),
    );

    let mut harness = EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new()
            .with_config(config)
            .with_working_dir(temp.path().to_path_buf()),
    )?;

    // 1. Open the Rust buffer — dormant indicator must appear.
    harness.open_file(&rust_file)?;
    harness.wait_until(|h| h.get_status_bar().contains("LSP (off)"))?;

    // 2. Open the plain-text buffer — indicator must clear, because
    //    "text" has no LSP configured.
    harness.open_file(&txt_file)?;
    harness.wait_until(|h| !h.get_status_bar().contains("LSP (off)"))?;

    // 3. Switch back to the Rust buffer — indicator must reappear.
    harness.open_file(&rust_file)?;
    harness.wait_until(|h| h.get_status_bar().contains("LSP (off)"))?;

    // Sanity: still no spawn throughout.
    assert!(
        !marker.exists(),
        "Dormant server must not spawn merely from buffer switching. \
         Marker: {:?}",
        marker
    );

    Ok(())
}
