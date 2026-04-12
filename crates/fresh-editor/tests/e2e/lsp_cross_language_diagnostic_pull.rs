//! Reproduction for the cross-language diagnostic-pull bug.
//!
//! User-visible symptom (reported 2026-04-13):
//! ```text
//! WARN fresh::services::lsp::async_handler:
//!   LSP response error: file not found: /home/noam/repos/fresh/default.nix (code -32603)
//! WARN fresh::services::lsp::async_handler:
//!   LSP response error: file not found: /home/noam/repos/fresh/package.json (code -32603)
//! ```
//!
//! Scenario: user has a Rust file focused and rust-analyzer running; two
//! unrelated buffers (`package.json`, `default.nix`) are also open in the
//! background. rust-analyzer sends `workspace/diagnostic/refresh`, we
//! react by pulling diagnostics for every open buffer, and — the bug —
//! we send the pull request to the rust-analyzer handle even for the
//! JSON and Nix URIs. rust-analyzer replies with `file not found`
//! because it doesn't track those documents.
//!
//! Root cause: `pull_diagnostics_for_language` in `app/async_messages.rs`
//! collects URIs from every `buffer_metadata` entry without filtering by
//! the buffer's language, then sends all of them to a handle resolved
//! for a single language.
//!
//! This test reproduces the flow by spawning a fake "rust-analyzer"
//! that (a) advertises `diagnosticProvider` on initialize, (b) sends
//! `workspace/diagnostic/refresh` after `didOpen` for the Rust file,
//! and (c) logs every `textDocument/diagnostic` request it receives so
//! the test can assert which URIs reached the server.

use crate::common::harness::EditorTestHarness;

/// Fake rust-analyzer-ish server: advertises diagnostic refresh, emits one
/// after the Rust file is opened, and logs every pull request URI.
fn create_refresh_and_log_server_script(dir: &std::path::Path) -> std::path::PathBuf {
    let script = r##"#!/bin/bash
LOG_FILE="${1:-/tmp/fake_refresh_log.txt}"
> "$LOG_FILE"
DID_OPEN_URI=""

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
    if [ -z "$msg" ]; then
        break
    fi
    method=$(echo "$msg" | grep -o '"method":"[^"]*"' | cut -d'"' -f4)
    msg_id=$(echo "$msg" | grep -o '"id":[0-9]*' | cut -d':' -f2)
    echo "RECV: method=$method id=$msg_id" >> "$LOG_FILE"

    case "$method" in
        "initialize")
            # Advertise diagnosticProvider so the editor treats us as a
            # pull-model diagnostic source.
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":{"capabilities":{"positionEncoding":"utf-16","textDocumentSync":{"openClose":true,"change":2,"save":{}},"diagnosticProvider":{"identifier":"fake-ra","interFileDependencies":true,"workspaceDiagnostics":false}}}}'
            ;;
        "initialized")
            ;;
        "textDocument/didOpen")
            URI=$(echo "$msg" | grep -o '"uri":"[^"]*"' | head -1 | cut -d'"' -f4)
            echo "DIDOPEN: $URI" >> "$LOG_FILE"
            # Only fire the refresh after the FIRST didOpen so background
            # buffer opens don't trigger a cascade — mirrors rust-analyzer
            # which only refreshes after the workspace has loaded.
            if [ -z "$DID_OPEN_URI" ]; then
                DID_OPEN_URI="$URI"
                send_message '{"jsonrpc":"2.0","id":9001,"method":"workspace/diagnostic/refresh","params":{}}'
                echo "SENT: workspace/diagnostic/refresh" >> "$LOG_FILE"
            fi
            ;;
        "textDocument/diagnostic")
            # The whole point of this test: log which URI we were asked
            # about. A correctly-scoped client never pulls non-rust URIs
            # from a rust-only server.
            URI=$(echo "$msg" | grep -o '"uri":"[^"]*"' | head -1 | cut -d'"' -f4)
            echo "PULL: $URI" >> "$LOG_FILE"
            # Respond with empty diagnostics so the client doesn't stall
            # waiting for us.
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":{"kind":"full","resultId":"fake","items":[]}}'
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
echo "SERVER: exiting" >> "$LOG_FILE"
"##;

    let script_path = dir.join("fake_refresh_server.sh");
    std::fs::write(&script_path, script).expect("Failed to write fake refresh server");

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = std::fs::metadata(&script_path).unwrap().permissions();
        perms.set_mode(0o755);
        std::fs::set_permissions(&script_path, perms).unwrap();
    }

    script_path
}

/// When the Rust server sends `workspace/diagnostic/refresh`, the editor
/// must re-pull diagnostics **only for buffers whose language is rust** —
/// not every open buffer regardless of language. Otherwise the Rust
/// server receives `textDocument/diagnostic` for `.json` / `.nix` URIs
/// it has never tracked and responds `file not found (code -32603)`,
/// polluting the log with spurious warnings and wasting a round-trip.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_workspace_diagnostic_refresh_pulls_only_same_language_buffers() -> anyhow::Result<()> {
    let _ = tracing_subscriber::fmt()
        .with_env_filter("fresh=debug")
        .try_init();

    let temp_dir = tempfile::tempdir()?;
    let script_path = create_refresh_and_log_server_script(temp_dir.path());
    let log_file = temp_dir.path().join("refresh_server_log.txt");

    // Three files: one Rust (the focused one), two unrelated languages
    // whose URIs rust-analyzer knows nothing about.
    let rust_file = temp_dir.path().join("api.rs");
    std::fs::write(&rust_file, "fn main() {}\n")?;
    let json_file = temp_dir.path().join("package.json");
    std::fs::write(&json_file, "{}\n")?;
    let nix_file = temp_dir.path().join("default.nix");
    std::fs::write(&nix_file, "{}\n")?;

    // Only Rust has an LSP server configured. json / nix are open buffers
    // without an LSP of their own — exactly the user's reported layout.
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

    // Open the non-Rust buffers first, as background tabs. They must not
    // spawn rust-analyzer — the fake LSP only auto-starts on a rust buffer.
    harness.open_file(&json_file)?;
    harness.open_file(&nix_file)?;
    // Now open the Rust file — this spawns the fake server and triggers
    // the refresh cascade.
    harness.open_file(&rust_file)?;
    harness.render()?;

    // Wait for the server to send the workspace refresh.
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.contains("SENT: workspace/diagnostic/refresh")
    })?;

    // Wait for at least one post-refresh pull so we know the editor's
    // refresh handler has run and queued its commands.
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.lines().any(|l| l.starts_with("PULL:"))
    })?;

    // Synchronization barrier: send a `textDocument/didChange` on the
    // Rust buffer *after* the refresh. `didChange` causes the editor to
    // schedule another pull; because LSP commands travel in order on a
    // single channel to the LSP task, observing the follow-up pull on
    // the server means every pull queued by the preceding refresh has
    // already been processed. Without this barrier the test races the
    // LSP shutdown and can miss the buggy cross-language pulls.
    use crossterm::event::{KeyCode, KeyModifiers};
    harness.send_key(KeyCode::Char('x'), KeyModifiers::NONE)?;
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        // We initially see PULL for the rust URI on open. We want a
        // *second* PULL for the rust URI (from the post-didChange pull)
        // to have landed.
        log.lines()
            .filter(|l| l.starts_with("PULL:") && l.contains("api.rs"))
            .count()
            >= 2
    })?;

    let log = std::fs::read_to_string(&log_file)?;
    eprintln!("[TEST] fake-ra log:\n{}", log);

    // The assertions that matter: the rust-only server must NEVER have
    // been asked about the JSON or Nix URIs.
    let pulled_uris: Vec<String> = log
        .lines()
        .filter_map(|l| l.strip_prefix("PULL: ").map(|s| s.to_string()))
        .collect();

    let json_uri_fragment = "package.json";
    let nix_uri_fragment = "default.nix";
    let rust_uri_fragment = "api.rs";

    assert!(
        pulled_uris.iter().any(|u| u.contains(rust_uri_fragment)),
        "Sanity: the rust-analyzer handle should have been pulled for the Rust URI.\nPulled: {:#?}",
        pulled_uris
    );
    assert!(
        !pulled_uris.iter().any(|u| u.contains(json_uri_fragment)),
        "BUG: rust-analyzer handle was asked to pull diagnostics for a JSON URI \
         (`package.json`). `workspace/diagnostic/refresh` must only re-pull \
         buffers whose language matches the server's scope.\nPulled: {:#?}",
        pulled_uris
    );
    assert!(
        !pulled_uris.iter().any(|u| u.contains(nix_uri_fragment)),
        "BUG: rust-analyzer handle was asked to pull diagnostics for a Nix URI \
         (`default.nix`). `workspace/diagnostic/refresh` must only re-pull \
         buffers whose language matches the server's scope.\nPulled: {:#?}",
        pulled_uris
    );

    Ok(())
}

/// Fake rust-analyzer-ish server that emits the `$/ra_projectLoadingStatus`
/// notification with `quiescent: true` — the signal rust-analyzer sends
/// when its workspace finishes indexing. Logs every inlay-hint URI it
/// receives so the test can assert only rust URIs are sent.
fn create_quiescent_server_script(dir: &std::path::Path) -> std::path::PathBuf {
    let script = r##"#!/bin/bash
LOG_FILE="${1:-/tmp/fake_quiescent_log.txt}"
> "$LOG_FILE"
DID_OPEN_URI=""

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
    if [ -z "$msg" ]; then
        break
    fi
    method=$(echo "$msg" | grep -o '"method":"[^"]*"' | cut -d'"' -f4)
    msg_id=$(echo "$msg" | grep -o '"id":[0-9]*' | cut -d':' -f2)
    echo "RECV: method=$method id=$msg_id" >> "$LOG_FILE"

    case "$method" in
        "initialize")
            # Advertise inlayHintProvider so the editor sends inlay-hint
            # requests after quiescent.
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":{"capabilities":{"positionEncoding":"utf-16","textDocumentSync":{"openClose":true,"change":2,"save":{}},"inlayHintProvider":{"resolveProvider":false}}}}'
            ;;
        "initialized") ;;
        "textDocument/didOpen")
            URI=$(echo "$msg" | grep -o '"uri":"[^"]*"' | head -1 | cut -d'"' -f4)
            echo "DIDOPEN: $URI" >> "$LOG_FILE"
            if [ -z "$DID_OPEN_URI" ]; then
                DID_OPEN_URI="$URI"
                # Quiescent notification: rust-analyzer emits
                # experimental/serverStatus with quiescent=true when the
                # project finishes loading.
                send_message '{"jsonrpc":"2.0","method":"experimental/serverStatus","params":{"health":"ok","quiescent":true,"message":null}}'
                echo "SENT: experimental/serverStatus quiescent=true" >> "$LOG_FILE"
            fi
            ;;
        "textDocument/inlayHint")
            URI=$(echo "$msg" | grep -o '"uri":"[^"]*"' | head -1 | cut -d'"' -f4)
            echo "INLAY: $URI" >> "$LOG_FILE"
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":[]}'
            ;;
        "textDocument/diagnostic")
            URI=$(echo "$msg" | grep -o '"uri":"[^"]*"' | head -1 | cut -d'"' -f4)
            echo "PULL: $URI" >> "$LOG_FILE"
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":{"kind":"full","resultId":"fake","items":[]}}'
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
echo "SERVER: exiting" >> "$LOG_FILE"
"##;

    let script_path = dir.join("fake_quiescent_server.sh");
    std::fs::write(&script_path, script).expect("Failed to write fake quiescent server");

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = std::fs::metadata(&script_path).unwrap().permissions();
        perms.set_mode(0o755);
        std::fs::set_permissions(&script_path, perms).unwrap();
    }

    script_path
}

/// When rust-analyzer signals project-load is complete (quiescent=true),
/// the editor re-requests inlay hints for every open buffer. That loop
/// must filter by language; otherwise the rust handle receives
/// `textDocument/inlayHint` for `.json` / `.nix` URIs it has never
/// tracked and replies `file not found (code -32603)` — the second
/// source of the user-reported warnings.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_quiescent_inlay_hint_burst_only_for_same_language_buffers() -> anyhow::Result<()> {
    let _ = tracing_subscriber::fmt()
        .with_env_filter("fresh=debug")
        .try_init();

    let temp_dir = tempfile::tempdir()?;
    let script_path = create_quiescent_server_script(temp_dir.path());
    let log_file = temp_dir.path().join("quiescent_log.txt");

    let rust_file = temp_dir.path().join("api.rs");
    std::fs::write(&rust_file, "fn main() {}\n")?;
    let json_file = temp_dir.path().join("package.json");
    std::fs::write(&json_file, "{}\n")?;
    let nix_file = temp_dir.path().join("default.nix");
    std::fs::write(&nix_file, "{}\n")?;

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

    harness.open_file(&json_file)?;
    harness.open_file(&nix_file)?;
    harness.open_file(&rust_file)?;
    harness.render()?;

    // Wait for the server to emit its quiescent notification.
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.contains("SENT: experimental/serverStatus quiescent=true")
    })?;

    // Wait for at least one inlay-hint request so we know the editor's
    // quiescent handler has run.
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.lines().any(|l| l.starts_with("INLAY:"))
    })?;

    // Synchronization barrier: didChange on the rust buffer forces a
    // follow-up inlay-hint pull. Observing it means any quiescent-burst
    // inlay-hints have all landed on the server side.
    use crossterm::event::{KeyCode, KeyModifiers};
    harness.send_key(KeyCode::Char('x'), KeyModifiers::NONE)?;
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.lines()
            .filter(|l| l.starts_with("INLAY:") && l.contains("api.rs"))
            .count()
            >= 2
    })?;

    let log = std::fs::read_to_string(&log_file)?;
    eprintln!("[TEST] fake-ra quiescent log:\n{}", log);

    let inlay_uris: Vec<String> = log
        .lines()
        .filter_map(|l| l.strip_prefix("INLAY: ").map(|s| s.to_string()))
        .collect();

    assert!(
        inlay_uris.iter().any(|u| u.contains("api.rs")),
        "Sanity: the rust-analyzer handle should have been asked for inlay hints on the Rust URI.\nSent: {:#?}",
        inlay_uris
    );
    assert!(
        !inlay_uris.iter().any(|u| u.contains("package.json")),
        "BUG: rust-analyzer handle was asked for inlay hints on a JSON URI \
         (`package.json`) during the post-quiescent burst. The quiescent \
         handler must only re-request for buffers whose language matches \
         the server's scope.\nSent: {:#?}",
        inlay_uris
    );
    assert!(
        !inlay_uris.iter().any(|u| u.contains("default.nix")),
        "BUG: rust-analyzer handle was asked for inlay hints on a Nix URI \
         (`default.nix`) during the post-quiescent burst.\nSent: {:#?}",
        inlay_uris
    );

    Ok(())
}
