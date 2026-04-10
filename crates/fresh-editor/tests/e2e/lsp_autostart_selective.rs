//! E2E tests: only enabled + auto_start servers matching the opened buffer's
//! language should spawn automatically on buffer load.
//!
//! Users configure multiple LSP servers in many combinations:
//! - universal (global) servers in `universal_lsp`
//! - a single server per language in `lsp[language]`
//! - multiple servers for a single language
//!
//! For each configured server the user can set `enabled` and `auto_start`.
//! The contract tested here: on buffer load, ONLY servers that are both
//! `enabled=true` and `auto_start=true`, AND belong to the opened buffer's
//! language, should spawn automatically. Every other configured server must
//! stay dormant until the user explicitly starts it via the command palette.
//!
//! Verification strategy: each fake server publishes one diagnostic of a
//! configurable severity on `didOpen`. The rust status bar displays a count
//! of diagnostics ("E:<n>" errors, "W:<n>" warnings). Spawning a server
//! yields a visible diagnostic on screen, so we assert on rendered output.

use crate::common::harness::EditorTestHarness;

/// Create a parameterized fake LSP server script.
///
/// The script takes two arguments:
///   $1 — log file path (first action truncates/creates the file, so its
///        existence proves the process was spawned)
///   $2 — diagnostic severity (1=error, 2=warning, 3=info, 4=hint)
///
/// On `didOpen` the script publishes exactly one diagnostic of the given
/// severity, so the editor status bar renders a count (E:/W:/I:/H:)
/// reflecting which servers actually spawned.
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
            echo "ACTION: initialize-reply" >> "$LOG_FILE"
            ;;
        "initialized")
            echo "ACTION: initialized" >> "$LOG_FILE"
            ;;
        "textDocument/didOpen")
            uri=$(echo "$msg" | grep -o '"uri":"[^"]*"' | head -1 | cut -d'"' -f4)
            send_message '{"jsonrpc":"2.0","method":"textDocument/publishDiagnostics","params":{"uri":"'"$uri"'","diagnostics":[{"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":1}},"severity":'"$SEVERITY"',"source":"fake","message":"sev'"$SEVERITY"'"}],"version":1}}'
            echo "ACTION: didOpen published severity=$SEVERITY" >> "$LOG_FILE"
            ;;
        "textDocument/didChange")
            echo "ACTION: didChange" >> "$LOG_FILE"
            ;;
        "textDocument/didClose")
            echo "ACTION: didClose" >> "$LOG_FILE"
            ;;
        "shutdown")
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":null}'
            echo "ACTION: shutdown" >> "$LOG_FILE"
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

/// Severity codes matching LSP spec.
const SEV_ERROR: u8 = 1;
const SEV_WARNING: u8 = 2;

fn build_server_config(
    name: &str,
    script: &std::path::Path,
    log_path: &std::path::Path,
    severity: u8,
    enabled: bool,
    auto_start: bool,
) -> fresh::services::lsp::LspServerConfig {
    fresh::services::lsp::LspServerConfig {
        command: script.to_string_lossy().to_string(),
        args: vec![log_path.to_string_lossy().to_string(), severity.to_string()],
        enabled,
        auto_start,
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

/// Four LSP servers configured in different constellations; only one is
/// eligible to auto-start. The status bar must render exactly "E:1" (from
/// the single eligible server) and no warning count.
///
/// Config:
/// - rust language:
///     * `rust-auto`    — enabled=true,  auto_start=true,  severity=error
///       → MUST spawn → contributes E:1
///     * `rust-manual`  — enabled=true,  auto_start=false, severity=warning
///       → must NOT spawn → would contribute W:1 (bug signature)
///     * `rust-disabled`— enabled=false, auto_start=true,  severity=warning
///       → must NOT spawn → would contribute W:1 (bug signature)
/// - python language:
///     * `python-auto`  — enabled=true,  auto_start=true,  severity=warning
///       → must NOT spawn (wrong language for .rs) → would contribute W:1
///
/// Regression for the bug where `try_spawn()` pre-checks that *any* config
/// has `auto_start+enabled` and then delegates to `force_spawn()`, which
/// indiscriminately spawns *every* enabled config — ignoring each config's
/// own `auto_start` flag.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_only_autostart_enabled_servers_spawn_for_language() -> anyhow::Result<()> {
    crate::common::tracing::init_tracing_from_env();

    let temp_dir = tempfile::tempdir()?;
    let script = create_diagnostic_server_script(temp_dir.path(), "fake_lsp_selective.sh");

    let log_rust_auto = temp_dir.path().join("rust_auto.log");
    let log_rust_manual = temp_dir.path().join("rust_manual.log");
    let log_rust_disabled = temp_dir.path().join("rust_disabled.log");
    let log_python_auto = temp_dir.path().join("python_auto.log");

    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn main() {\n    println!(\"hi\");\n}\n")?;

    let mut config = fresh::config::Config::default();

    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![
            build_server_config("rust-auto", &script, &log_rust_auto, SEV_ERROR, true, true),
            build_server_config(
                "rust-manual",
                &script,
                &log_rust_manual,
                SEV_WARNING,
                true,
                false,
            ),
            build_server_config(
                "rust-disabled",
                &script,
                &log_rust_disabled,
                SEV_WARNING,
                false,
                true,
            ),
        ]),
    );

    config.lsp.insert(
        "python".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![build_server_config(
            "python-auto",
            &script,
            &log_python_auto,
            SEV_WARNING,
            true,
            true,
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

    // Rendered output assertion: wait for the single eligible server's
    // error diagnostic to appear on the status bar.
    harness.wait_until(|h| h.screen_to_string().contains("E:1"))?;

    // Sanity check: the error server's log exists and reached didOpen
    // (proving the wait above was satisfied by the correct server).
    let elog = std::fs::read_to_string(&log_rust_auto)?;
    assert!(
        elog.contains("ACTION: didOpen"),
        "rust-auto (the one eligible server) should have received didOpen.\nLog:\n{}",
        elog
    );

    // The buggy path would have spawned rust-manual (warning) at the same
    // time as rust-auto (both sit inside the same `force_spawn` loop), so
    // by the time E:1 is visible rust-manual's warning would also have
    // been published and rendered.
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("W:1") && !screen.contains("W:2"),
        "No warnings should be rendered — only the rust-auto server (which \
         publishes an error) should have spawned. Screen:\n{}",
        screen
    );

    // Belt-and-braces: the script creates its log file as its very first
    // action, so file non-existence proves the script process was never
    // spawned. This catches cases where a server spawns but fails to
    // publish its diagnostic for any reason.
    assert!(
        !log_rust_manual.exists(),
        "Server 'rust-manual' (enabled=true, auto_start=false) must NOT be \
         spawned automatically on buffer load. Log file unexpectedly exists: {:?}",
        log_rust_manual
    );
    assert!(
        !log_rust_disabled.exists(),
        "Server 'rust-disabled' (enabled=false, auto_start=true) must NOT be \
         spawned on buffer load. Log file unexpectedly exists: {:?}",
        log_rust_disabled
    );
    assert!(
        !log_python_auto.exists(),
        "Server 'python-auto' (wrong language for .rs) must NOT be spawned \
         when a Rust buffer is opened. Log file unexpectedly exists: {:?}",
        log_python_auto
    );

    Ok(())
}

/// Verify that a universal (global) LSP server with `enabled=true` but
/// `auto_start=false` does not spawn automatically, even when a
/// per-language server for the opened buffer has `auto_start=true`.
///
/// Universal servers are appended to each per-language config list at
/// startup. The bug: `try_spawn()` sees that *some* config for the language
/// has `auto_start`, then delegates to `force_spawn()` which spawns *every*
/// enabled config — including the universal server that the user
/// explicitly marked as not-auto-start.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_universal_server_respects_auto_start_flag() -> anyhow::Result<()> {
    crate::common::tracing::init_tracing_from_env();

    let temp_dir = tempfile::tempdir()?;
    let script =
        create_diagnostic_server_script(temp_dir.path(), "fake_lsp_universal_selective.sh");

    let log_rust_auto = temp_dir.path().join("rust_auto_u.log");
    let log_universal_manual = temp_dir.path().join("universal_manual.log");

    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn main() {}\n")?;

    let mut config = fresh::config::Config::default();

    // Per-language rust server: enabled + auto_start → MUST spawn, publishes error
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![build_server_config(
            "rust-auto",
            &script,
            &log_rust_auto,
            SEV_ERROR,
            true,
            true,
        )]),
    );

    // Universal server: enabled=true but auto_start=false → must NOT spawn,
    // would publish a warning if spawned
    config.universal_lsp.insert(
        "universal-manual".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![build_server_config(
            "universal-manual",
            &script,
            &log_universal_manual,
            SEV_WARNING,
            true,
            false,
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

    harness.wait_until(|h| h.screen_to_string().contains("E:1"))?;

    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("W:1") && !screen.contains("W:2"),
        "No warnings should be rendered — the universal server (auto_start=false) \
         must not have spawned. Screen:\n{}",
        screen
    );

    assert!(
        !log_universal_manual.exists(),
        "Universal LSP server with auto_start=false must NOT spawn \
         automatically on buffer load. Log file unexpectedly exists: {:?}",
        log_universal_manual
    );

    Ok(())
}

/// Create a fake LSP script that appends its PID to a shared spawn log on
/// startup, so callers can count how many processes were launched.
fn create_counting_server_script(dir: &std::path::Path, filename: &str) -> std::path::PathBuf {
    let script = r##"#!/bin/bash
SPAWN_LOG="$1"
echo "$$" >> "$SPAWN_LOG"

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
        "initialized")
            ;;
        "textDocument/didOpen")
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

/// A universal LSP server (enabled=true, auto_start=true) must be spawned
/// only once per project, even when files of multiple languages are opened.
///
/// The current architecture appends the universal config to every configured
/// language's server list. When files of different languages are opened,
/// each language spawns its own instance of the universal server — resulting
/// in multiple processes for what should be a single server.
///
/// This test opens a Rust file and a TOML file, with a universal LSP
/// configured. It asserts only one universal LSP process is spawned.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_universal_lsp_spawned_once_across_languages() -> anyhow::Result<()> {
    crate::common::tracing::init_tracing_from_env();

    let temp_dir = tempfile::tempdir()?;
    let script = create_counting_server_script(temp_dir.path(), "fake_universal_counter.sh");
    let spawn_log = temp_dir.path().join("spawn_count.log");

    let rust_file = temp_dir.path().join("test.rs");
    std::fs::write(&rust_file, "fn main() {}\n")?;

    let toml_file = temp_dir.path().join("test.toml");
    std::fs::write(&toml_file, "[package]\nname = \"test\"\n")?;

    let mut config = fresh::config::Config::default();

    // Disable per-language servers so only the universal server is relevant
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: "rust-analyzer".to_string(),
            enabled: false,
            ..Default::default()
        }]),
    );
    config.lsp.insert(
        "toml".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: "taplo".to_string(),
            enabled: false,
            ..Default::default()
        }]),
    );

    // Universal server: enabled + auto_start → should spawn exactly once
    config.universal_lsp.insert(
        "test-universal".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: script.to_string_lossy().to_string(),
            args: vec![spawn_log.to_string_lossy().to_string()],
            enabled: true,
            auto_start: true,
            name: Some("TestUniversal".to_string()),
            ..Default::default()
        }]),
    );

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        200,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    // Open both files
    harness.open_file(&rust_file)?;
    harness.render()?;
    harness.open_file(&toml_file)?;
    harness.render()?;

    // Let LSP processes start
    for _ in 0..20 {
        harness.process_async_and_render()?;
    }

    // Wait for at least one spawn
    harness.wait_until(|_| spawn_log.exists())?;

    // Give a moment for any additional (buggy) spawns to register
    for _ in 0..10 {
        harness.process_async_and_render()?;
    }

    let log_content = std::fs::read_to_string(&spawn_log)?;
    let spawn_count = log_content.lines().filter(|l| !l.is_empty()).count();

    assert_eq!(
        spawn_count,
        1,
        "Universal LSP should be spawned exactly once across all languages, \
         but was spawned {} times. PIDs: {}",
        spawn_count,
        log_content.trim()
    );

    Ok(())
}

/// A universal LSP server must receive didOpen notifications for files
/// of any language, not just the language it was spawned under.
///
/// Regression: universal handles are created with language "__universal__"
/// but the did_open method validates that the document language matches
/// the handle language. This causes all didOpen notifications to be
/// rejected with "Language mismatch".
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_universal_lsp_receives_did_open_for_all_languages() -> anyhow::Result<()> {
    crate::common::tracing::init_tracing_from_env();

    let temp_dir = tempfile::tempdir()?;
    let script = create_diagnostic_server_script(temp_dir.path(), "fake_universal_didopen.sh");
    let log_universal = temp_dir.path().join("universal_didopen.log");

    let rust_file = temp_dir.path().join("test.rs");
    std::fs::write(&rust_file, "fn main() {}\n")?;

    let mut config = fresh::config::Config::default();

    // Disable per-language rust server
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: "rust-analyzer".to_string(),
            enabled: false,
            ..Default::default()
        }]),
    );

    // Universal server: publishes error diagnostic on didOpen
    config.universal_lsp.insert(
        "test-universal".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![build_server_config(
            "test-universal",
            &script,
            &log_universal,
            SEV_ERROR,
            true,
            true,
        )]),
    );

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        200,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    harness.open_file(&rust_file)?;
    harness.render()?;

    // The universal server publishes an error diagnostic on didOpen.
    // If didOpen was rejected due to language mismatch, E:1 will never appear.
    harness.wait_until(|h| h.screen_to_string().contains("E:1"))?;

    let log = std::fs::read_to_string(&log_universal)?;
    assert!(
        log.contains("ACTION: didOpen"),
        "Universal LSP must receive didOpen for rust files. Log:\n{}",
        log
    );

    Ok(())
}
