//! Regression: a server-initiated `workspace/semanticTokens/refresh` must
//! cause the client to re-pull semantic tokens even for a buffer the user
//! has not edited -- that is the whole point of the refresh request (the
//! server learned something, e.g. from another file, that changes this
//! file's tokens).
//!
//! The refresh handler (`handle_lsp_semantic_tokens_refresh` ->
//! `request_semantic_tokens_for_language` -> debounce ->
//! `maybe_request_semantic_tokens`) used to bail out with "Already up to
//! date" because the stored tokens' version equals the unchanged buffer
//! version, so the server never saw a second `textDocument/semanticTokens/full`.
//!
//! The same staleness bit server restarts (the new process was never asked
//! for tokens, and when it eventually was, it got a `full/delta` against the
//! old process's `resultId`) and a failed delta (its `resultId` was kept, so
//! every later edit repeated the failing delta).

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Fake server that:
/// - advertises full (non-delta, no range) semantic tokens,
/// - logs every `textDocument/semanticTokens/full` it receives with a counter,
/// - sends one `workspace/semanticTokens/refresh` request to the client once
///   the test creates `$TRIGGER_FILE` (so the refresh provably arrives after
///   the editor has stored the first response -- ordering is driven by the
///   test's semantic wait, not by a timer).
fn create_refresh_server_script(dir: &std::path::Path) -> std::path::PathBuf {
    let script = r##"#!/bin/bash

LOG_FILE="$1"
TRIGGER_FILE="$2"
> "$LOG_FILE"
FULL_COUNT=0
WATCHER_STARTED=0

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
    msg_id=$(echo "$msg" | grep -o '"id":[0-9]*' | head -1 | cut -d':' -f2)

    echo "RECV: method=$method id=$msg_id" >> "$LOG_FILE"

    case "$method" in
        "initialize")
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":{"capabilities":{"textDocumentSync":1,"semanticTokensProvider":{"legend":{"tokenTypes":["keyword","function","variable"],"tokenModifiers":["declaration","deprecated"]},"full":true}}}}'
            ;;
        "initialized")
            ;;
        "textDocument/didOpen")
            echo "ACTION: didOpen" >> "$LOG_FILE"
            ;;
        "textDocument/didChange")
            # The test never edits; record it so the test can prove that.
            echo "ACTION: didChange" >> "$LOG_FILE"
            ;;
        "textDocument/semanticTokens/full")
            FULL_COUNT=$((FULL_COUNT + 1))
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":{"resultId":"'"$FULL_COUNT"'","data":[0,0,2,0,0,0,3,4,1,0]}}'
            echo "SEMTOK_FULL #$FULL_COUNT" >> "$LOG_FILE"
            if [ "$WATCHER_STARTED" = "0" ]; then
                WATCHER_STARTED=1
                # Server->client request, sent once the test says the first
                # response has been applied. The main loop is idle (blocked
                # on read) at that point, so stdout is not contended.
                (
                    while [ ! -f "$TRIGGER_FILE" ]; do sleep 0.05; done
                    send_message '{"jsonrpc":"2.0","id":9000,"method":"workspace/semanticTokens/refresh","params":null}'
                    echo "SENT: workspace/semanticTokens/refresh" >> "$LOG_FILE"
                ) &
            fi
            ;;
        "$/cancelRequest")
            ;;
        "shutdown")
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":null}'
            break
            ;;
        *)
            # Answer unknown requests; ignore responses (no method), e.g. the
            # client's null reply to our refresh request.
            if [ -n "$method" ] && [ -n "$msg_id" ]; then
                send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":null}'
            fi
            ;;
    esac
done
"##;

    let script_path = dir.join("fake_semtok_refresh_server.sh");
    std::fs::write(&script_path, script).expect("Failed to write fake server script");

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

fn full_request_count(log_file: &std::path::Path) -> usize {
    std::fs::read_to_string(log_file)
        .unwrap_or_default()
        .lines()
        .filter(|l| l.starts_with("SEMTOK_FULL #"))
        .count()
}

#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses Bash-based fake LSP server
fn test_semantic_tokens_refresh_repulls_unedited_buffer() -> anyhow::Result<()> {
    let temp_dir = tempfile::tempdir()?;
    let script_path = create_refresh_server_script(temp_dir.path());
    let log_file = temp_dir.path().join("semtok_refresh_log.txt");
    let trigger_file = temp_dir.path().join("send_refresh");
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn main() { let value = 1; }\n")?;

    let mut config = fresh::config::Config::default();
    config.editor.enable_semantic_tokens_full = true;
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: script_path.to_string_lossy().to_string(),
            args: Some(vec![
                log_file.to_string_lossy().to_string(),
                trigger_file.to_string_lossy().to_string(),
            ]),
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
        100,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    harness.open_file(&test_file)?;
    harness.render()?;

    // 1. First full pull is answered and stored for the current buffer version.
    harness.wait_until(|h| {
        let state = h.editor().active_state();
        state
            .semantic_tokens
            .as_ref()
            .map(|store| store.version == state.buffer.version())
            .unwrap_or(false)
    })?;
    assert_eq!(full_request_count(&log_file), 1);
    let version_before_refresh = harness.editor().active_state().buffer.version();

    // 2. Server asks the client to re-pull (buffer untouched).
    std::fs::write(&trigger_file, "")?;
    harness.wait_until(|_| {
        std::fs::read_to_string(&log_file)
            .unwrap_or_default()
            .contains("SENT: workspace/semanticTokens/refresh")
    })?;

    // 3. The client must issue a second semanticTokens/full request.
    //    With the bug `maybe_request_semantic_tokens` returned early
    //    ("Already up to date") and this wait never completed; nextest's
    //    external timeout failed the test.
    harness.wait_until(|_| full_request_count(&log_file) >= 2)?;

    // The re-pull must not have been caused by an edit.
    assert_eq!(
        harness.editor().active_state().buffer.version(),
        version_before_refresh,
        "buffer must be unedited: the refresh alone should trigger the re-pull"
    );
    let log = std::fs::read_to_string(&log_file)?;
    assert!(
        !log.contains("ACTION: didChange"),
        "no didChange expected.\nLog:\n{}",
        log
    );

    Ok(())
}


/// Fake server advertising `full: { delta: true }` whose `resultId`s are
/// scoped to the server process (`i<instance>-<n>`), like a real server's:
/// a `full/delta` naming another process's (or a forgotten) `resultId` is
/// answered with an error. Each spawn bumps the instance number kept in
/// `$COUNTER_FILE`, so a restart is observable in the log.
///
/// With `$FORGET_FIRST` = 1 the server has dropped its first result (`i1-1`)
/// from its cache, so deltas against it fail although the id is its own:
/// that exercises the client's delta-error handling without a restart.
fn create_delta_server_script(dir: &std::path::Path) -> std::path::PathBuf {
    let script = r##"#!/bin/bash

LOG_FILE="$1"
COUNTER_FILE="$2"
FORGET_FIRST="$3"
INSTANCE=$(( $(cat "$COUNTER_FILE" 2>/dev/null || echo 0) + 1 ))
echo "$INSTANCE" > "$COUNTER_FILE"
N=0
echo "START instance=$INSTANCE" >> "$LOG_FILE"

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

send_tokens() {
    N=$((N + 1))
    send_message '{"jsonrpc":"2.0","id":'"$1"',"result":{"resultId":"i'"$INSTANCE"'-'"$N"'","data":[0,0,2,0,0,0,3,4,1,0]}}'
}

while true; do
    msg=$(read_message)
    if [ -z "$msg" ]; then
        break
    fi

    method=$(echo "$msg" | grep -o '"method":"[^"]*"' | cut -d'"' -f4)
    msg_id=$(echo "$msg" | grep -o '"id":[0-9]*' | head -1 | cut -d':' -f2)

    case "$method" in
        "initialize")
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":{"capabilities":{"textDocumentSync":1,"semanticTokensProvider":{"legend":{"tokenTypes":["keyword","function","variable"],"tokenModifiers":["declaration","deprecated"]},"full":{"delta":true}}}}}'
            ;;
        "textDocument/semanticTokens/full")
            echo "FULL instance=$INSTANCE" >> "$LOG_FILE"
            send_tokens "$msg_id"
            ;;
        "textDocument/semanticTokens/full/delta")
            prev=$(echo "$msg" | grep -o '"previousResultId":"[^"]*"' | cut -d'"' -f4)
            if [ "$FORGET_FIRST" = "1" ] && [ "$prev" = "i1-1" ]; then
                echo "DELTA_FAILED instance=$INSTANCE prev=$prev" >> "$LOG_FILE"
                send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"error":{"code":-32602,"message":"unknown previousResultId"}}'
            elif [ "${prev#i$INSTANCE-}" = "$prev" ]; then
                echo "DELTA_STALE instance=$INSTANCE prev=$prev" >> "$LOG_FILE"
                send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"error":{"code":-32602,"message":"unknown previousResultId"}}'
            else
                echo "DELTA_OK instance=$INSTANCE prev=$prev" >> "$LOG_FILE"
                send_tokens "$msg_id"
            fi
            ;;
        "shutdown")
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":null}'
            ;;
        "exit")
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

    let script_path = dir.join("fake_semtok_delta_server.sh");
    std::fs::write(&script_path, script).expect("Failed to write fake server script");

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

struct DeltaServerSetup {
    _temp_dir: tempfile::TempDir,
    log_file: std::path::PathBuf,
    test_file: std::path::PathBuf,
    harness: EditorTestHarness,
}

fn start_delta_server(forget_first_result: bool) -> anyhow::Result<DeltaServerSetup> {
    let temp_dir = tempfile::tempdir()?;
    let script_path = create_delta_server_script(temp_dir.path());
    let log_file = temp_dir.path().join("semtok_delta_log.txt");
    let counter_file = temp_dir.path().join("instance_counter");
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn main() { let value = 1; }\n")?;

    let mut config = fresh::config::Config::default();
    config.editor.enable_semantic_tokens_full = true;
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: script_path.to_string_lossy().to_string(),
            args: Some(vec![
                log_file.to_string_lossy().to_string(),
                counter_file.to_string_lossy().to_string(),
                if forget_first_result { "1" } else { "0" }.to_string(),
            ]),
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

    let harness = EditorTestHarness::with_config_and_working_dir(
        100,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;
    Ok(DeltaServerSetup {
        _temp_dir: temp_dir,
        log_file,
        test_file,
        harness,
    })
}

fn log_count(log_file: &std::path::Path, prefix: &str) -> usize {
    std::fs::read_to_string(log_file)
        .unwrap_or_default()
        .lines()
        .filter(|l| l.starts_with(prefix))
        .count()
}

/// Wait until the active buffer holds fresh tokens for its current version
/// carrying the given `resultId`.
fn wait_for_result_id(harness: &mut EditorTestHarness, expected: &str) -> anyhow::Result<()> {
    harness.wait_until(|h| {
        let state = h.editor().active_state();
        state.semantic_tokens.as_ref().is_some_and(|store| {
            store.version == state.buffer.version() && store.result_id.as_deref() == Some(expected)
        })
    })
}

/// After a server restart the new process must be asked for tokens (the
/// buffer is unedited, so nothing else would ask), and with a plain `full`:
/// the old process's `resultId` means nothing to it.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses Bash-based fake LSP server
fn test_semantic_tokens_repulled_with_full_after_server_restart() -> anyhow::Result<()> {
    let DeltaServerSetup {
        _temp_dir,
        log_file,
        test_file,
        mut harness,
    } = start_delta_server(false)?;

    harness.open_file(&test_file)?;
    harness.render()?;
    wait_for_result_id(&mut harness, "i1-1")?;

    harness.editor_mut().handle_restart_lsp_server("rust");

    // The restarted process answers a plain `full` without any edit.
    wait_for_result_id(&mut harness, "i2-1")?;
    assert_eq!(log_count(&log_file, "FULL instance=2"), 1);

    // Later edits use deltas against the new process's baseline.
    harness.send_key(KeyCode::End, KeyModifiers::NONE)?;
    harness.type_text("x")?;
    wait_for_result_id(&mut harness, "i2-2")?;

    let log = std::fs::read_to_string(&log_file)?;
    assert!(
        log.contains("DELTA_OK instance=2 prev=i2-1"),
        "expected a delta against the new baseline.\nLog:\n{}",
        log
    );
    assert!(
        !log.contains("DELTA_STALE"),
        "the old process's resultId must not reach the new one.\nLog:\n{}",
        log
    );
    Ok(())
}

/// When a `full/delta` request fails, the stored `resultId` must be dropped
/// so the next request is a plain `full`, instead of every later request
/// repeating the same failing delta.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses Bash-based fake LSP server
fn test_semantic_tokens_delta_error_falls_back_to_full() -> anyhow::Result<()> {
    let DeltaServerSetup {
        _temp_dir,
        log_file,
        test_file,
        mut harness,
    } = start_delta_server(true)?;

    harness.open_file(&test_file)?;
    harness.render()?;
    wait_for_result_id(&mut harness, "i1-1")?;

    // Edit -> delta against the forgotten `i1-1` -> error -> plain full.
    // With the bug the client kept re-sending the failing delta and fresh
    // tokens never arrived.
    harness.send_key(KeyCode::End, KeyModifiers::NONE)?;
    harness.type_text("x")?;
    wait_for_result_id(&mut harness, "i1-2")?;
    let log = std::fs::read_to_string(&log_file)?;
    assert_eq!(log_count(&log_file, "DELTA_FAILED"), 1, "Log:\n{}", log);
    assert_eq!(log_count(&log_file, "FULL instance=1"), 2, "Log:\n{}", log);

    // Deltas resume against the new baseline.
    harness.type_text("y")?;
    wait_for_result_id(&mut harness, "i1-3")?;
    let log = std::fs::read_to_string(&log_file)?;
    assert!(
        log.contains("DELTA_OK instance=1 prev=i1-2"),
        "expected a delta against the new baseline.\nLog:\n{}",
        log
    );
    assert_eq!(log_count(&log_file, "DELTA_FAILED"), 1, "Log:\n{}", log);
    Ok(())
}
