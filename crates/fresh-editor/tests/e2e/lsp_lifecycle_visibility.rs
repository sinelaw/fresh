//! E2E test: LSP lifecycle visibility when `auto_start = false`.
//!
//! Context: the heuristic evaluation in
//! `docs/internal/LSP_HEURISTIC_EVAL_CLANGD.md` raises as its top concern
//! that a user opening a buffer whose language has an LSP *configured* but
//! with `auto_start = false` has no way to tell the LSP exists. The status
//! bar shows the language but no "LSP: off" / "LSP: dormant" badge, so the
//! configured-but-dormant state is indistinguishable from "no LSP at all".
//!
//! This test pins down the *current* behavior of the editor via a fake LSP
//! server so future work can decide whether it is acceptable:
//!
//! 1. The LSP script creates a marker file as its very first action, so we
//!    can prove the process was *not* spawned (marker file never appears).
//! 2. With `auto_start = false`, no marker file appears after many async
//!    ticks, confirming the LSP stays dormant (as intended).
//! 3. The status bar row on screen contains zero visible cue that an LSP is
//!    configured — no "LSP", no "off", no server name. The status bar is
//!    byte-identical to the control case where no LSP is configured at all.
//!
//! If assertion (3) ever starts failing, it will be because someone added
//! the dormant-LSP indicator the heuristic evaluation recommends (P0 item 1
//! in the remediation plan). That's a good failure — update the assertion.

use crate::common::harness::{EditorTestHarness, HarnessOptions};

/// Fake LSP server that writes a marker line to its log file on startup.
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
            # If the editor ever did open the document against this server,
            # publish a bright red error diagnostic so an "E:1" would appear
            # on the status bar — making any leak loud.
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

/// Open a `.rs` file with `rust-dormant` LSP configured `auto_start=false`
/// and capture the status bar + screen. Then open the same file with NO
/// LSP configured. The two captures must be identical — proving the UI
/// offers no affordance for the dormant-LSP state.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses Bash-based fake LSP server
fn test_dormant_lsp_has_no_visible_indicator() -> anyhow::Result<()> {
    let _ = tracing_subscriber::fmt()
        .with_env_filter("fresh=warn")
        .try_init();

    // ----- Scenario A: LSP configured, auto_start = false -----
    let temp_a = tempfile::tempdir()?;
    let script_a = create_spawn_marker_script(temp_a.path(), "fake_lsp_dormant.sh");
    let marker_a = temp_a.path().join("spawn_marker.log");
    let file_a = temp_a.path().join("hello.rs");
    std::fs::write(&file_a, "fn main() {}\n")?;

    let mut config_a = fresh::config::Config::default();
    config_a.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: script_a.to_string_lossy().to_string(),
            args: vec![marker_a.to_string_lossy().to_string()],
            enabled: true,
            auto_start: false, // <-- the key: configured but dormant
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

    let mut harness_a = EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new()
            .with_config(config_a)
            .with_working_dir(temp_a.path().to_path_buf()),
    )?;

    harness_a.open_file(&file_a)?;
    harness_a.render()?;

    // Give any would-be auto-start a generous window to fire. The spawn
    // marker is written on script startup, so its presence would be near-
    // instantaneous if the editor were (wrongly) spawning the server.
    for _ in 0..40 {
        harness_a.process_async_and_render()?;
    }

    assert!(
        !marker_a.exists(),
        "auto_start=false server must not be spawned on buffer open, but \
         its spawn marker appeared at {:?}",
        marker_a
    );

    let status_a = harness_a.get_status_bar();
    let screen_a = harness_a.screen_to_string();

    // ----- Scenario B: no LSP configured at all (control) -----
    let temp_b = tempfile::tempdir()?;
    let file_b = temp_b.path().join("hello.rs");
    std::fs::write(&file_b, "fn main() {}\n")?;

    let config_b = fresh::config::Config::default(); // no LSP entries

    let mut harness_b = EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new()
            .with_config(config_b)
            .with_working_dir(temp_b.path().to_path_buf()),
    )?;

    harness_b.open_file(&file_b)?;
    harness_b.render()?;
    for _ in 0..40 {
        harness_b.process_async_and_render()?;
    }

    let status_b = harness_b.get_status_bar();

    eprintln!("[TEST] Status bar WITH dormant LSP configured:\n  {}", status_a.trim_end());
    eprintln!("[TEST] Status bar WITHOUT any LSP configured:\n  {}", status_b.trim_end());

    // Core assertion 1: the status bar is identical between the two
    // scenarios — the UI draws no distinction between "LSP configured but
    // dormant" and "no LSP at all".
    assert_eq!(
        status_a.trim_end(),
        status_b.trim_end(),
        "Status bar differs between 'dormant LSP configured' and 'no LSP \
         configured' — a visible cue has apparently been added. Update this \
         test if that is intentional.\n\
         WITH dormant LSP: {:?}\n\
         WITHOUT any LSP:  {:?}",
        status_a.trim_end(),
        status_b.trim_end(),
    );

    // Core assertion 2: no textual cue anywhere on screen refers to the
    // configured server, its state, or a way to start it. If any of these
    // tokens appear, concern #1 has been (at least partially) addressed.
    let lower = screen_a.to_lowercase();
    for cue in [
        "lsp:",
        "lsp off",
        "lsp dormant",
        "lsp (off)",
        "rust-dormant",
        "start lsp",
        "not auto-starting",
    ] {
        assert!(
            !lower.contains(cue),
            "Screen mentions {:?}, which would be a dormant-LSP affordance. \
             If this is intentional, update this test.\nFull screen:\n{}",
            cue,
            screen_a
        );
    }

    // Core assertion 3: no diagnostic leaked through. Proves the server
    // really did stay dormant (the script would publish an error on any
    // didOpen it received).
    assert!(
        !screen_a.contains("E:1"),
        "Dormant LSP must not publish diagnostics. Screen:\n{}",
        screen_a
    );

    Ok(())
}
