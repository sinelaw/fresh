//! Reproducer: after `Stop LSP Server` the status-bar indicator still
//! reads `"LSP (on)"` and the status popup still lists the server as
//! running, even though the handle has been shut down and removed.
//!
//! User-reported on 2026-04-13: invoking the `Stop LSP Server` palette
//! entry tears down the rust-analyzer process but the UI never catches
//! up — the pill keeps the "running" color and clicking it opens a
//! popup whose Stop/Restart actions pretend the server is still alive.
//!
//! Root cause (from `app/prompt_actions.rs:handle_stop_lsp_server`):
//! the handler calls `lsp.shutdown_server(language)` (which removes
//! handles from the manager) but never touches `lsp_server_statuses`
//! on the `Editor`. `compose_lsp_status` in `app/render.rs` reads
//! `lsp_server_statuses` to drive the indicator — it looks for any
//! entry that isn't `Shutdown`, so a stale `Running`/`Initializing`
//! entry keeps the pill stuck at "on" indefinitely. Compare with
//! `handle_lsp_status_action`'s `stop:` branch, which does clear the
//! status entry after shutting the server down — that's the pattern
//! the palette handler is missing.

use crate::common::harness::EditorTestHarness;

/// Fake LSP server that stays running after initialize and shuts down
/// cleanly on request — enough to exercise the stop path without any
/// real-language server complications.
fn create_long_running_server_script(dir: &std::path::Path) -> std::path::PathBuf {
    let script = r##"#!/bin/bash
LOG_FILE="${1:-/tmp/fake_stop_log.txt}"
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

    let script_path = dir.join("fake_stop_server.sh");
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

/// After invoking the `Stop LSP Server` command-palette action, the
/// status-bar indicator must no longer read `"LSP (on)"`. The user-
/// visible contract is: stopping the server immediately switches the
/// pill back to `"LSP (off)"` (since the server is still configured
/// via the editor's default config, just not currently running) or
/// at worst to an empty indicator. Any flavor of `"(on)"` is wrong —
/// the server is dead.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_stop_lsp_server_clears_stale_on_indicator() -> anyhow::Result<()> {
    let _ = tracing_subscriber::fmt()
        .with_env_filter("fresh=debug")
        .try_init();

    let temp_dir = tempfile::tempdir()?;
    let script_path = create_long_running_server_script(temp_dir.path());
    let log_file = temp_dir.path().join("stop_log.txt");
    let test_file = temp_dir.path().join("hello.rs");
    std::fs::write(&test_file, "fn main() {}\n")?;

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

    harness.open_file(&test_file)?;
    harness.render()?;

    // Wait for the server to be up and the indicator to reflect it.
    harness.wait_until(|h| h.screen_to_string().contains("LSP (on)"))?;

    // Sanity: the pre-stop state is what the user sees before triggering
    // the palette entry.
    assert!(
        harness.screen_to_string().contains("LSP (on)"),
        "Pre-condition: indicator should read 'LSP (on)' before stop."
    );

    // Invoke the exact prompt-confirm path that `Ctrl+P -> Stop LSP
    // Server -> <language>` hits. The prompt handler accepts either
    // `"<language>"` or `"<language>/<server_name>"`.
    harness.editor_mut().handle_stop_lsp_server("rust");

    // Pump one render so the status bar has a chance to recompose.
    harness.render()?;

    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("LSP (on)"),
        "BUG: after `Stop LSP Server`, the status bar still reads \
         'LSP (on)'. `handle_stop_lsp_server` shut the handle down but \
         didn't clear `lsp_server_statuses`, so the render-time \
         composition still sees a non-Shutdown entry and keeps the pill \
         stuck in the running state.\nScreen:\n{}",
        screen
    );
    assert!(
        screen.contains("LSP (off)"),
        "After stopping, the indicator should read 'LSP (off)' (the \
         server is still configured, just not running) — it shouldn't \
         vanish or show any other state.\nScreen:\n{}",
        screen
    );

    Ok(())
}

/// Fake LSP that, on `initialize`, fires a `$/progress begin`
/// notification that never gets a corresponding `end`. This puts the
/// editor's `lsp_progress` map into the "active work for this
/// language" state, which drives the status-bar spinner. Used by the
/// test below to reproduce the user-reported "spinner stuck after
/// stop" bug.
fn create_progress_stuck_server_script(dir: &std::path::Path) -> std::path::PathBuf {
    let script = r##"#!/bin/bash
LOG_FILE="${1:-/tmp/progress_stuck_log.txt}"
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
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":{"capabilities":{"positionEncoding":"utf-16","textDocumentSync":{"openClose":true,"change":2,"save":{}},"workDoneProgress":true}}}'
            ;;
        "initialized")
            # Fire a progress-begin that we intentionally never end —
            # the server is about to be stopped and we want to prove
            # the editor clears progress state on its side rather
            # than waiting for an `end` that'll never arrive.
            send_message '{"jsonrpc":"2.0","method":"$/progress","params":{"token":"stuck-1","value":{"kind":"begin","title":"indexing"}}}'
            echo "SENT: progress begin" >> "$LOG_FILE"
            ;;
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
"##;

    let script_path = dir.join("fake_progress_stuck_server.sh");
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

/// After `Stop LSP Server` the status-bar indicator must stop
/// showing the progress spinner, even when the server had open
/// `$/progress` work in flight — the server is dead and will never
/// send an `end` notification, so the editor has to clear its own
/// progress bookkeeping when it stops the server.
///
/// Bug (user-reported): stopping an LSP that had emitted a
/// `$/progress begin` leaves `lsp_progress` populated; the spinner
/// branch in `compose_lsp_status` wins over the "(off)" branch and
/// the pill stays stuck on the rotating braille char. Worse, the
/// spinner stops re-rendering on its own (no async progress events
/// arrive any more), so it only advances when some unrelated event
/// — mouse hover, keypress — re-renders the frame. From the user's
/// perspective the indicator is frozen yet twitchy.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_stop_lsp_server_clears_stale_progress_spinner() -> anyhow::Result<()> {
    let temp_dir = tempfile::tempdir()?;
    let script_path = create_progress_stuck_server_script(temp_dir.path());
    let log_file = temp_dir.path().join("progress_stuck.log");
    let test_file = temp_dir.path().join("hello.rs");
    std::fs::write(&test_file, "fn main() {}\n")?;

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
            name: Some("progress-stuck".to_string()),
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

    harness.open_file(&test_file)?;
    harness.render()?;

    // Wait until the spinner branch wins, i.e. the editor has
    // received a `$/progress begin` and `lsp_progress` is populated
    // for `rust`. `has_active_lsp_progress` on the editor is the
    // authoritative view into that map; we use it as the readiness
    // signal rather than scraping spinner glyphs out of the screen.
    harness.wait_until(|h| h.editor().has_active_lsp_progress())?;

    // Sanity: the visible indicator is in spinner mode, not "(on)"
    // or "(off)". The spinner's exact glyph rotates with wall-clock,
    // so the stable signal is the ABSENCE of both paren'd states.
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("LSP (on)") && !screen.contains("LSP (off)"),
        "pre-condition: indicator should be in spinner state (no paren'd \
         label) while progress is active. Screen:\n{}",
        screen
    );

    // Stop via the same prompt path the command palette uses.
    harness.editor_mut().handle_stop_lsp_server("rust");
    harness.render()?;

    // After stop: progress state should be cleared on our side
    // (the server can't send `end` — it's dead) and the indicator
    // should read the configured-but-not-running "LSP (off)".
    let screen = harness.screen_to_string();
    assert!(
        !harness.editor().has_active_lsp_progress(),
        "BUG: `lsp_progress` still has entries for the stopped server. \
         That keeps the spinner branch in `compose_lsp_status` live \
         forever, since the `end` notification is never coming. \
         Screen:\n{}",
        screen
    );
    assert!(
        screen.contains("LSP (off)"),
        "After stop, the indicator should be 'LSP (off)'. Stuck spinner \
         means `compose_lsp_status` took the progress branch instead. \
         Screen:\n{}",
        screen
    );

    Ok(())
}
