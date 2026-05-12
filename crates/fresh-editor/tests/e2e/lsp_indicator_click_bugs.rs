//! E2E regression tests for the LSP status-bar indicator bugs reported
//! after the user clicked the indicator on a Rust file while
//! rust-analyzer was indexing.
//!
//! The five bugs covered here are:
//!
//!   1. **Stacked popups on click.** When a language plugin (e.g. the
//!      embedded `rust-lsp.ts`) handles `lsp_status_clicked` and pushes
//!      its own popup, the built-in `build_and_show_lsp_status_popup`
//!      still runs unconditionally right after the hook fires, leaving
//!      two popups on the buffer's popup stack.
//!
//!   2. **Plugin popup ignores the theme.** Every popup that flows
//!      through `Event::ShowPopup` → `convert_popup_data_to_popup`
//!      ends up with `background_style.bg = Color::Rgb(30, 30, 30)`,
//!      hardcoded at `state.rs:convert_popup_data_to_popup`. In a
//!      light theme that's a near-black rectangle in the middle of a
//!      near-white UI.
//!
//!   3. **Popup keeps showing "ready / indexing" after the server
//!      died externally** (SIGKILL by the OOM killer, `process_limits`,
//!      a crash, …). The editor doesn't react to stdout-EOF by
//!      flipping `lsp_server_statuses` to `Shutdown` or by pruning
//!      `lsp_progress`, so the popup keeps reading the same stale
//!      "● <name> (ready) ⏳ Indexing 18%" state until the user
//!      manually picks Stop.
//!
//!   4. **"Disable LSP for &lt;lang&gt;" persists but doesn't stop the
//!      running server.** The `dismiss:` branch of
//!      `handle_lsp_status_action` writes `enabled = false` to config
//!      and calls `save_config`, but never tears down the currently
//!      running server — so the user sees `Disabled` in the status bar
//!      while the same server keeps indexing, and re-opening the popup
//!      still shows the server as `(ready)`.
//!
//!   5. **Spinner doesn't auto-advance.** `compose_lsp_status` derives
//!      the braille spinner index from `SystemTime::now() / 100ms`,
//!      but nothing in the editor schedules a redraw on that 100ms
//!      cadence — the indicator only ticks when *some other* event
//!      causes a frame (keypress, mouse hover, an incoming progress
//!      notification, …). Once the source of progress notifications
//!      stops (e.g. server died, see #3), the spinner appears frozen
//!      and only twitches forward by one glyph on user input.
//!
//! Each test is written so that **it fails today** (i.e. the bug is
//! observable) and would pass once the bug is fixed.

use std::time::Duration;

use crate::common::harness::{EditorTestHarness, HarnessOptions};

// ---------------------------------------------------------------------------
// Fake LSP scripts
// ---------------------------------------------------------------------------

/// Fake LSP that, on `initialized`, emits a `$/progress` `begin` and
/// then a continuous stream of `report` notifications. Stays alive
/// until stdin closes or `shutdown`/`exit` arrives. Mirrors the
/// indexing flow of a real rust-analyzer (which is what triggered
/// every bug in this file).
///
/// The `LOG` env var (script arg #1) points at a per-test log file so
/// the test can read lifecycle breadcrumbs if it ever needs to.
fn create_indexing_server_script(dir: &std::path::Path) -> std::path::PathBuf {
    let script = r##"#!/bin/bash
LOG_FILE="${1:-/tmp/fake_indexing_log.txt}"
: > "$LOG_FILE"

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
        dd bs=1 count=$content_length 2>/dev/null
    fi
}

send_message() {
    local message="$1"
    local length=${#message}
    printf "Content-Length: $length\r\n\r\n%s" "$message"
}

# Spawn a background progress-emitter that fires `$/progress report`
# notifications every 200ms until the parent (us) exits. Without this,
# `lsp_progress` would only ever hold the initial `begin` entry and the
# spinner wouldn't drive any renders.
emit_progress() {
    local i=0
    while kill -0 $$ 2>/dev/null; do
        i=$(( (i + 1) % 100 ))
        send_message "{\"jsonrpc\":\"2.0\",\"method\":\"\$/progress\",\"params\":{\"token\":\"idx-1\",\"value\":{\"kind\":\"report\",\"message\":\"$i/100\",\"percentage\":$i}}}"
        sleep 0.2
    done
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
            send_message '{"jsonrpc":"2.0","method":"$/progress","params":{"token":"idx-1","value":{"kind":"begin","title":"Indexing","percentage":0}}}'
            emit_progress &
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

    let script_path = dir.join("fake_indexing_lsp.sh");
    std::fs::write(&script_path, script).expect("write fake LSP script");
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = std::fs::metadata(&script_path).unwrap().permissions();
        perms.set_mode(0o755);
        std::fs::set_permissions(&script_path, perms).unwrap();
    }
    script_path
}

/// Fake LSP that exits immediately with a non-zero status. Makes the
/// editor's spawn-and-watch path fire `lsp_server_error` (caught by
/// the embedded `rust-lsp.ts` plugin), without going down the
/// "command not found" branch that bypasses spawn entirely.
fn create_immediately_exiting_server_script(dir: &std::path::Path) -> std::path::PathBuf {
    let script = "#!/bin/bash\nexit 1\n";
    let script_path = dir.join("fake_exiting_lsp.sh");
    std::fs::write(&script_path, script).expect("write exiting LSP script");
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = std::fs::metadata(&script_path).unwrap().permissions();
        perms.set_mode(0o755);
        std::fs::set_permissions(&script_path, perms).unwrap();
    }
    script_path
}

/// Build a minimal `Config` that points the `rust` LSP at `command`
/// (any executable that speaks LSP framing), with `auto_start = true`
/// so opening a `.rs` file kicks the server off.
fn config_with_rust_lsp(command: &str) -> fresh::config::Config {
    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: command.to_string(),
            args: vec![],
            enabled: true,
            auto_start: true,
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
            env: Default::default(),
            language_id_overrides: Default::default(),
            root_markers: Default::default(),
            name: Some("fake-rust-analyzer".to_string()),
            only_features: None,
            except_features: None,
        }]),
    );
    config
}

/// Helper: count visible popups on the active buffer's popup stack.
fn active_popup_count(harness: &EditorTestHarness) -> usize {
    harness.editor().active_state().popups.all().len()
}

/// Helper: count popups on the editor-wide `global_popups` stack —
/// where `editor.showActionPopup` (and therefore the rust-lsp plugin's
/// "Not Found" popup) actually lands.
fn global_popup_count(harness: &EditorTestHarness) -> usize {
    harness.editor().global_popups().all().len()
}

/// Push a plugin action popup through the same path the rust-lsp.ts
/// plugin's `editor.showActionPopup` ends up at — i.e. through
/// `Editor::handle_plugin_command(PluginCommand::ShowActionPopup …)`.
/// This bypasses the buffer-local `Event::ShowPopup` path entirely and
/// lands the popup on `global_popups`, exactly where the production
/// bug surfaces.
fn push_plugin_action_popup(
    harness: &mut EditorTestHarness,
    popup_id: &str,
    title: &str,
) -> anyhow::Result<()> {
    use fresh_core::api::{ActionPopupAction, PluginCommand};
    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::ShowActionPopup {
            popup_id: popup_id.to_string(),
            title: title.to_string(),
            message: "plugin-side popup".to_string(),
            actions: vec![ActionPopupAction {
                id: "dismiss".to_string(),
                label: "Dismiss".to_string(),
            }],
        })?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Issue 1 — clicking the LSP indicator stacks two popups when the
//           rust-lsp plugin handles `lsp_status_clicked`.
// ---------------------------------------------------------------------------
//
// Reproduction shape:
//   * Configure `rust` LSP with a non-existent binary so the embedded
//     `rust-lsp.ts` plugin's `lsp_server_error` handler sets its
//     `rustLspError` state.
//   * Open a `.rs` file → the plugin fires `setStatus(...)`, the
//     indicator goes to `LSP (error)`.
//   * Trigger `show_lsp_status_popup` (same entry point as a click).
//   * The plugin's `editor.on("lsp_status_clicked", …)` handler runs
//     synchronously inside `show_lsp_status_popup` and calls
//     `editor.showActionPopup({ title: "Rust Language Server Not Found", … })`.
//   * Right after the hook, `build_and_show_lsp_status_popup` runs
//     unconditionally and pushes a second popup.
//
// Expected: at most one popup on screen for a single user gesture.

/// Reproduces the user's "several kinds of popups" complaint.
///
/// `show_lsp_status_popup` fires the `lsp_status_clicked` hook and then
/// — unconditionally — also pushes the built-in LSP Servers popup
/// (`popup_dialogs.rs:108`). If a plugin handler reacts to that hook by
/// pushing its own popup (the embedded `rust-lsp.ts` plugin does this
/// in production), the two popups end up stacked.
///
/// We can't reliably drive the plugin's TS handler from inside the
/// e2e harness (loading the embedded plugin set is best-effort under
/// test, and the spawn path that fires `lsp_server_error` is fragile),
/// so we exercise the exact same code path the plugin would: push a
/// popup via `show_popup` *before* calling `show_lsp_status_popup`. A
/// correctly-wired `show_lsp_status_popup` would notice that the hook
/// (or any prior step) left a popup on top of the stack and refrain
/// from stacking the LSP Servers popup over it.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn issue_1_click_stacks_plugin_popup_and_lsp_servers_popup() -> anyhow::Result<()> {
    use fresh::model::event::{
        PopupContentData, PopupData, PopupKindHint, PopupListItemData, PopupPositionData,
    };

    let temp = tempfile::tempdir()?;
    let file = temp.path().join("hello.rs");
    std::fs::write(&file, "fn main() {}\n")?;

    let mut harness = EditorTestHarness::create(
        140,
        40,
        HarnessOptions::new()
            .with_config(config_with_rust_lsp("rust-analyzer"))
            .with_working_dir(temp.path().to_path_buf()),
    )?;
    harness.open_file(&file)?;
    harness.render()?;

    // Simulate what the rust-lsp plugin does in response to
    // `lsp_status_clicked`: push an action popup.
    let plugin_popup = PopupData {
        kind: PopupKindHint::List,
        title: Some("Rust Language Server Not Found".to_string()),
        description: Some("plugin-side popup".to_string()),
        transient: false,
        content: PopupContentData::List {
            items: vec![PopupListItemData {
                text: "Disable Rust LSP".to_string(),
                detail: None,
                icon: None,
                data: Some("disable".to_string()),
            }],
            selected: 0,
        },
        position: PopupPositionData::Centered,
        width: 50,
        max_height: 6,
        bordered: true,
    };
    harness.editor_mut().show_popup(plugin_popup);
    let count_after_plugin_popup = active_popup_count(&harness);
    assert_eq!(
        count_after_plugin_popup, 1,
        "precondition: exactly one popup on the stack after the plugin's push"
    );

    // Now the editor proceeds with the rest of `show_lsp_status_popup`:
    // it calls `build_and_show_lsp_status_popup` unconditionally
    // (popup_dialogs.rs:108) — that's the line under test.
    harness.editor_mut().show_lsp_status_popup();
    harness.render()?;

    let count_after = active_popup_count(&harness);
    let screen = harness.screen_to_string();

    assert_eq!(
        count_after, 1,
        "BUG: `show_lsp_status_popup` stacks its built-in popup on top \
         of one already pushed by a plugin's `lsp_status_clicked` \
         handler — the user sees two popups for one click. Stack went \
         from 1 (plugin popup) to {count_after} after \
         `show_lsp_status_popup` ran. Screen:\n{screen}"
    );
    Ok(())
}

/// Run `condition` against the harness once every 50ms (with renders
/// between checks) until it returns true or `timeout` elapses.
/// Returns whether the condition was met.
fn wait_for<F>(harness: &mut EditorTestHarness, timeout: Duration, mut condition: F) -> bool
where
    F: FnMut(&EditorTestHarness) -> bool,
{
    let deadline = std::time::Instant::now() + timeout;
    while std::time::Instant::now() < deadline {
        let _ = harness.render();
        if condition(harness) {
            return true;
        }
        std::thread::sleep(Duration::from_millis(50));
        harness.advance_time(Duration::from_millis(50));
    }
    condition(harness)
}

// ---------------------------------------------------------------------------
// Issue 2 — popups created via the `PopupData` event use a hardcoded
//           dark background that ignores the theme.
// ---------------------------------------------------------------------------
//
// `state.rs::convert_popup_data_to_popup` sets
//     background_style: Style::default().bg(Color::Rgb(30, 30, 30))
// regardless of theme. Every plugin popup (rust-lsp's "Not Found",
// the Rust LSP mode chooser, every `editor.showActionPopup` call, …)
// flows through that conversion and ends up with the same near-black
// rectangle. In a light theme this is unmistakable on screen.

#[test]
fn issue_2_show_popup_ignores_theme_popup_bg() -> anyhow::Result<()> {
    use fresh::model::event::{
        PopupContentData, PopupData, PopupKindHint, PopupListItemData, PopupPositionData,
    };
    use ratatui::style::Color;

    let mut harness = EditorTestHarness::new(80, 24)?;

    let theme_popup_bg = harness.editor().theme().popup_bg;

    // Sanity: the theme has a `popup_bg` defined. If this ever became
    // `Color::Reset` we'd want a separate check; for now any
    // non-`Reset` value is fine — the bug is that the rendered popup
    // uses a *different* color (the hardcoded 30,30,30 dark grey).
    assert_ne!(
        theme_popup_bg,
        Color::Reset,
        "precondition: the harness's default theme should specify a popup_bg"
    );

    // Push a popup the same way `editor.showActionPopup` (and the LSP
    // confirmation popup) do — through the `PopupData` event path.
    let popup_data = PopupData {
        kind: PopupKindHint::List,
        title: Some("Probe Popup".to_string()),
        description: None,
        transient: false,
        content: PopupContentData::List {
            items: vec![PopupListItemData {
                text: "An item".to_string(),
                detail: None,
                icon: None,
                data: Some("noop".to_string()),
            }],
            selected: 0,
        },
        position: PopupPositionData::Centered,
        width: 30,
        max_height: 5,
        bordered: true,
    };
    harness.editor_mut().show_popup(popup_data);

    let popup = harness
        .editor()
        .active_state()
        .popups
        .top()
        .expect("popup should be on the stack after show_popup");

    let bg = popup.background_style.bg;

    assert_eq!(
        bg,
        Some(theme_popup_bg),
        "BUG: a popup created via `Event::ShowPopup` has background \
         {:?}, but the active theme's `popup_bg` is {:?}. \
         `convert_popup_data_to_popup` hardcodes \
         `Color::Rgb(30, 30, 30)` instead of reading the theme.",
        bg,
        theme_popup_bg
    );

    Ok(())
}

// ---------------------------------------------------------------------------
// Issue 3 — popup keeps showing "ready / indexing" after the LSP
//           process has died externally.
// ---------------------------------------------------------------------------
//
// Trigger:
//   * Spawn the fake indexing server (above) → it sends `$/progress
//     begin` + a stream of `report`s. Editor's `lsp_progress` fills in,
//     `lsp_server_statuses` flips to `Running`.
//   * `kill -9` the server process (same effect as the OS OOM killer or
//     `process_limits` enforcement). The stdout pipe closes with EOF.
//   * Open the LSP-servers popup.
//
// Today the popup still shows `● fake-rust-analyzer (ready)` and the
// `⏳ Indexing 18/100` row, because the EOF path doesn't prune
// `lsp_progress` and doesn't flip the server status to `Shutdown`.
// The user has to manually pick Stop to clean things up.
//
// Expected: when the process is gone, the popup must reflect that —
// either the row says "not running" / "error", or the progress row
// disappears.

#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn issue_3_external_kill_leaves_popup_state_stale() -> anyhow::Result<()> {
    let temp = tempfile::tempdir()?;
    let script = create_indexing_server_script(temp.path());
    let log = temp.path().join("idx.log");
    let file = temp.path().join("hello.rs");
    std::fs::write(&file, "fn main() {}\n")?;

    let mut config = config_with_rust_lsp(&script.to_string_lossy());
    if let Some(lsp_cfg) = config.lsp.get_mut("rust") {
        for c in lsp_cfg.as_mut_slice() {
            c.args = vec![log.to_string_lossy().to_string()];
        }
    }

    let mut harness = EditorTestHarness::create(
        140,
        30,
        HarnessOptions::new()
            .with_config(config)
            .with_working_dir(temp.path().to_path_buf()),
    )?;

    harness.open_file(&file)?;

    // Wait for progress to be active — that's the signal the server is
    // up and emitting `$/progress` notifications.
    harness.wait_until(|h| h.editor().active_window().has_active_lsp_progress())?;

    // Find the server pid via the script-log marker (the script's
    // background `emit_progress` runs in the same process group).
    // Easier: just SIGKILL anything whose argv contains our script
    // path. The harness's child-LSP process is the one we want.
    let script_name = script.file_name().unwrap().to_string_lossy().to_string();
    let _ = std::process::Command::new("pkill")
        .args(["-9", "-f", &script_name])
        .status();

    // Give the OS a moment to actually deliver the signal and for the
    // editor's stdout-read loop to see EOF, then for the async
    // bridge to deliver the resulting `LspStatusUpdate { Error }` to
    // the editor's tick handler. We use `wait_until` so each iteration
    // both sleeps a bit AND pumps async messages via `editor_tick`,
    // which is what handles the status update — calling `render`
    // alone wouldn't drain the async queue.
    let _ = harness.wait_until(|h| !h.editor().active_window().has_active_lsp_progress());

    // The popup the user would see if they click the indicator right
    // now. Equivalent of `show_lsp_status_popup` from the click path.
    harness.editor_mut().show_lsp_status_popup();
    harness.render()?;

    let screen = harness.screen_to_string();

    // BUG: the popup still shows "(ready)" for a server whose process
    // is dead.
    assert!(
        !screen.contains("(ready)"),
        "BUG: the LSP servers popup says `(ready)` after the server's \
         process was SIGKILLed. EOF on stdout should flip the server's \
         status to Shutdown / Error.\nScreen:\n{screen}"
    );
    // BUG: progress entries should not survive the server's death —
    // an `end` notification will never arrive.
    assert!(
        !harness.editor().active_window().has_active_lsp_progress(),
        "BUG: `lsp_progress` still has entries for the dead server. The \
         editor must drop them on EOF.\nScreen:\n{screen}"
    );
    Ok(())
}

// ---------------------------------------------------------------------------
// Issue 4 — "Disable LSP for &lt;lang&gt;" leaves the running server
//           running.
// ---------------------------------------------------------------------------
//
// Trigger:
//   * Server up and indexing.
//   * Invoke the popup's `dismiss:rust` action (the action key for the
//     "Disable LSP for rust" row).
//
// Today: `enabled = false` is persisted to config, the status bar reads
// "LSP disabled for rust.", but the server *keeps running* — re-opening
// the popup shows `● fake-rust-analyzer (ready) ⏳ Indexing` and the
// indicator keeps spinning. The user has to pick Stop separately to
// actually kill the process.
//
// Expected: picking Disable must imply Stop for any currently-running
// servers of that language.

#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn issue_4_disable_lsp_does_not_stop_running_server() -> anyhow::Result<()> {
    let temp = tempfile::tempdir()?;
    let script = create_indexing_server_script(temp.path());
    let log = temp.path().join("idx.log");
    let file = temp.path().join("hello.rs");
    std::fs::write(&file, "fn main() {}\n")?;

    let mut config = config_with_rust_lsp(&script.to_string_lossy());
    if let Some(lsp_cfg) = config.lsp.get_mut("rust") {
        for c in lsp_cfg.as_mut_slice() {
            c.args = vec![log.to_string_lossy().to_string()];
        }
    }

    let mut harness = EditorTestHarness::create(
        140,
        30,
        HarnessOptions::new()
            .with_config(config)
            .with_working_dir(temp.path().to_path_buf()),
    )?;

    harness.open_file(&file)?;
    harness.wait_until(|h| h.editor().active_window().is_lsp_server_ready("rust"))?;

    // Sanity preconditions.
    assert!(
        harness.editor().active_window().is_lsp_server_ready("rust"),
        "precondition: rust LSP must be ready before we disable it"
    );

    // Trigger exactly what the popup's "Disable LSP for rust" row
    // dispatches. `handle_lsp_status_action` reads the action key,
    // strips the `dismiss:` prefix, and runs the disable path.
    harness
        .editor_mut()
        .handle_lsp_status_action("dismiss:rust");
    harness.render()?;

    // Half-of-bug: the config should have been flipped (this part
    // works today and we want to keep it that way).
    let enabled_after_disable = harness
        .editor()
        .config()
        .lsp
        .get("rust")
        .map(|cfg| cfg.as_slice().iter().any(|c| c.enabled))
        .unwrap_or(false);
    assert!(
        !enabled_after_disable,
        "Disable must persist `enabled=false` in config"
    );

    // The bug: the still-running server should have been torn down.
    assert!(
        !harness.editor().active_window().is_lsp_server_ready("rust"),
        "BUG: after `dismiss:rust` the rust LSP server is still \
         reported as ready. Disable must imply Stop for any running \
         servers, otherwise the user sees `LSP disabled` in the \
         status bar while the same server keeps indexing in the \
         background."
    );

    // Re-opening the popup should not show an active progress row
    // either — there's nothing to be indexing for.
    harness.editor_mut().show_lsp_status_popup();
    harness.render()?;
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("(ready)"),
        "BUG: after Disable, the popup still shows the server as `(ready)`.\nScreen:\n{screen}"
    );
    assert!(
        !screen.contains("⏳"),
        "BUG: after Disable, the popup still shows an in-flight progress row.\nScreen:\n{screen}"
    );
    Ok(())
}

// ---------------------------------------------------------------------------
// Issue 5 — spinner doesn't auto-advance: it only ticks on input.
// ---------------------------------------------------------------------------
//
// `lsp_status::compose_lsp_status` derives the braille glyph from
// `SystemTime::now() / 100ms`, so the *value* changes every 100ms — but
// the editor must actually call render for the screen to reflect that.
// In production, between two unrelated events the main loop blocks on
// `poll_event` with a long timeout — so the indicator only ticks when
// a user / LSP event fires.
//
// The contract a fix must honour: **while `lsp_progress` is active,
// the editor must report a sub-spinner-period redraw deadline so the
// main event loop knows to wake up at ~100ms cadence and re-render.**
//
// `Editor::next_periodic_redraw_deadline()` is the API. While progress
// is active it must return `Some(deadline)` with `deadline - now ≤
// 120ms` (a small slack over the 100ms spinner period).

#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn issue_5_spinner_has_no_auto_redraw_schedule() -> anyhow::Result<()> {
    let temp = tempfile::tempdir()?;
    let script = create_indexing_server_script(temp.path());
    let log = temp.path().join("idx.log");
    let file = temp.path().join("hello.rs");
    std::fs::write(&file, "fn main() {}\n")?;

    let mut config = config_with_rust_lsp(&script.to_string_lossy());
    if let Some(lsp_cfg) = config.lsp.get_mut("rust") {
        for c in lsp_cfg.as_mut_slice() {
            c.args = vec![log.to_string_lossy().to_string()];
        }
    }

    let mut harness = EditorTestHarness::create(
        140,
        30,
        HarnessOptions::new()
            .with_config(config)
            .with_working_dir(temp.path().to_path_buf()),
    )?;
    harness.open_file(&file)?;
    harness.wait_until(|h| h.editor().active_window().has_active_lsp_progress())?;
    harness.render()?;

    // Capture the indicator glyph from the status row, for the
    // diagnostic message.
    let glyph_now = current_spinner_glyph(&harness);

    let deadline = harness.editor().next_periodic_redraw_deadline();
    let deadline = deadline.unwrap_or_else(|| {
        panic!(
            "BUG: while LSP `$/progress` is in flight the editor returns \
             `next_periodic_redraw_deadline() = None`, so the main event \
             loop has no signal to wake up and advance the spinner. \
             `compose_lsp_status` recomputes the glyph every 100ms from \
             wall-clock time but only when *something* causes a frame; \
             without a periodic deadline the indicator looks frozen \
             between unrelated events. glyph at t=0: {glyph_now}"
        )
    });
    let until = deadline.saturating_duration_since(std::time::Instant::now());
    assert!(
        until <= Duration::from_millis(120),
        "BUG: while LSP progress is active the redraw deadline must \
         land within ~one spinner period (100ms), but \
         `next_periodic_redraw_deadline()` returned a deadline {}ms \
         away. The main loop would sleep through too many frames to \
         keep the spinner moving.",
        until.as_millis()
    );
    Ok(())
}

// ---------------------------------------------------------------------------
// Issue 1 (variant b) — plugin popup goes through `global_popups`,
//                       not `active_state().popups`.
// ---------------------------------------------------------------------------
//
// The original `issue_1_click_stacks_plugin_popup_and_lsp_servers_popup`
// test simulated the plugin by pre-pushing a popup to the *buffer-local*
// `active_state().popups` stack. That caught one half of the bug, but
// the embedded `rust-lsp.ts` plugin's `editor.showActionPopup` in
// practice lands in the *editor-wide* `global_popups` stack
// (see `handle_show_action_popup` in `app/plugin_dispatch.rs`). A fix
// that only checks `active_state().popups` would still leave the
// double-popup behaviour the user reported:
//
//   1. user clicks the LSP indicator
//   2. `show_lsp_status_popup` builds the "LSP Servers (rust)" popup
//      and pushes it to `active_state().popups`
//   3. the plugin's `lsp_status_clicked` handler runs async, lands a
//      `PluginCommand::ShowActionPopup` in the editor's queue
//   4. next tick: `handle_show_action_popup` pushes the "Rust Language
//      Server Not Found" popup to `global_popups`
//   5. render: both popups visible
//
// Contract: at the end of the sequence, exactly one popup should be
// drawn across the two stacks.

#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn issue_1_plugin_popup_lands_on_global_popups_after_lsp_servers_popup() -> anyhow::Result<()> {
    let temp = tempfile::tempdir()?;
    let file = temp.path().join("hello.rs");
    std::fs::write(&file, "fn main() {}\n")?;

    let mut harness = EditorTestHarness::create(
        140,
        40,
        HarnessOptions::new()
            .with_config(config_with_rust_lsp("rust-analyzer"))
            .with_working_dir(temp.path().to_path_buf()),
    )?;
    harness.open_file(&file)?;
    harness.render()?;

    // Step (a): the user clicks the indicator; the editor builds the
    // LSP Servers popup synchronously and pushes it to
    // `active_state().popups`.
    harness.editor_mut().show_lsp_status_popup();
    assert_eq!(
        active_popup_count(&harness),
        1,
        "precondition: LSP Servers popup must land on active_state \
         when no plugin popup is in flight"
    );
    assert_eq!(
        global_popup_count(&harness),
        0,
        "precondition: nothing on global_popups yet"
    );

    // Step (b): the plugin's async `editor.showActionPopup` arrives on
    // the next tick. We invoke `handle_plugin_command` directly — the
    // same dispatcher the async bridge would call — so the test
    // doesn't depend on plugin loading at all.
    push_plugin_action_popup(
        &mut harness,
        "rust-lsp-help",
        "Rust Language Server Not Found",
    )?;

    let total = active_popup_count(&harness) + global_popup_count(&harness);
    let screen = {
        harness.render()?;
        harness.screen_to_string()
    };

    assert_eq!(
        total,
        1,
        "BUG: after the plugin's async-arriving popup pushes to \
         `global_popups`, the previously-built LSP Servers popup on \
         `active_state().popups` is still there — the user sees TWO \
         popups (plugin one on top, LSP Servers one underneath) for \
         one indicator click. Total popups across both stacks: \
         {total} (active={}, global={}).\nScreen:\n{screen}",
        active_popup_count(&harness),
        global_popup_count(&harness),
    );
    Ok(())
}

// ---------------------------------------------------------------------------
// Issue 4 — repeated clicks stack identical plugin popups on top of
//           each other.
// ---------------------------------------------------------------------------
//
// `handle_show_action_popup` uses `self.global_popups.show(popup_obj)`
// — plain `show`, not `show_or_replace`. Each click that fires the
// `lsp_status_clicked` hook while the plugin's `rustLspError` is set
// re-pushes the same "Rust Language Server Not Found" popup. Dismiss
// one and the next identical popup is revealed underneath, exactly
// matching the user's "if I click many times this popup sometimes
// stacks - dismissing it I see the same popup underneath".
//
// Contract: pushing N action popups with the same `popup_id` must
// leave at most one of them on the stack — the de-dup is keyed on
// the `popup_id`, which is exactly what `PopupResolver::PluginAction
// { popup_id }` already carries on each popup.

#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn issue_4_repeated_plugin_action_popup_pushes_stack_instead_of_replace() -> anyhow::Result<()> {
    let temp = tempfile::tempdir()?;
    let file = temp.path().join("hello.rs");
    std::fs::write(&file, "fn main() {}\n")?;

    let mut harness = EditorTestHarness::create(
        140,
        40,
        HarnessOptions::new()
            .with_config(config_with_rust_lsp("rust-analyzer"))
            .with_working_dir(temp.path().to_path_buf()),
    )?;
    harness.open_file(&file)?;
    harness.render()?;

    // Simulate three rapid indicator-clicks while the plugin's
    // `rustLspError` is set: each one fires `lsp_status_clicked`,
    // which on the plugin side calls `editor.showActionPopup` with
    // the same `popup_id` "rust-lsp-help".
    for _ in 0..3 {
        push_plugin_action_popup(
            &mut harness,
            "rust-lsp-help",
            "Rust Language Server Not Found",
        )?;
    }

    let depth = global_popup_count(&harness);
    assert_eq!(
        depth, 1,
        "BUG: three `showActionPopup` calls with the SAME `popup_id` \
         stacked 3 popups on `global_popups`. The user dismisses one, \
         sees the same popup underneath, dismisses it again, sees yet \
         another — matching the user-reported \"this popup sometimes \
         stacks; dismissing it I see the same popup underneath\". \
         `handle_show_action_popup` should de-dup by `popup_id` (use \
         `show_or_replace`-style logic on `global_popups`). Stack \
         depth: {depth}",
    );
    Ok(())
}

// ---------------------------------------------------------------------------
// Follow-up — opening a prompt (e.g. clicking the language indicator
//             while the LSP-Servers popup is up) must dismiss the popup.
// ---------------------------------------------------------------------------
//
// User-reported after the previous round of fixes:
//   "when the lsp indicator popup is open -> click on the language
//    indicator, which brings up a prompt -> the lsp popup stays open
//    and overlaps the prompt. it should close the popup. same for any
//    other popup probably?"
//
// The fix is `Editor::dismiss_menu_popups_for_prompt`, called from
// every non-LSP status-bar indicator's click branch in
// `handle_click_status_bar`. This test asserts the behaviour through
// a status-bar mouse click — the exact path the user follows.

#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn status_indicator_click_dismisses_open_lsp_popup_before_opening_prompt() -> anyhow::Result<()> {
    let temp = tempfile::tempdir()?;
    let file = temp.path().join("hello.rs");
    std::fs::write(&file, "fn main() {}\n")?;

    let mut harness = EditorTestHarness::create(
        140,
        40,
        HarnessOptions::new()
            .with_config(config_with_rust_lsp("rust-analyzer"))
            .with_working_dir(temp.path().to_path_buf()),
    )?;
    harness.open_file(&file)?;
    harness.render()?;

    // Step (a): open the LSP-Servers popup directly (no need to wait
    // for any particular LSP status — `show_lsp_status_popup` builds
    // the popup from whatever state is present, including "configured
    // but not running" / "error").
    harness.editor_mut().show_lsp_status_popup();
    harness.render()?;
    assert!(
        active_popup_count(&harness) >= 1,
        "precondition: LSP Servers popup should be open"
    );
    assert!(
        harness.screen_to_string().contains("LSP Servers (rust)"),
        "precondition: LSP Servers popup should be drawn"
    );

    // Step (b): the user clicks the language indicator on the status
    // bar. Find its column on the rendered screen and inject a mouse
    // click at that cell.
    let screen = harness.screen_to_string();
    let status_row_idx = screen
        .lines()
        .enumerate()
        .find(|(_, l)| l.contains("Rust") && l.contains("LSP"))
        .map(|(i, _)| i as u16)
        .expect("status row with Rust language indicator must be present");
    let status_row = screen.lines().nth(status_row_idx as usize).unwrap();
    let language_col = status_row
        .find("Rust")
        .expect("status row should contain Rust language label") as u16;
    harness.mouse_click(language_col, status_row_idx)?;
    harness.render()?;

    // Expected: the LSP-Servers popup must be gone, and the editor
    // should now be in prompt-mode for the language picker.
    let screen_after = harness.screen_to_string();
    assert!(
        !screen_after.contains("LSP Servers (rust)"),
        "BUG: after clicking the language indicator while the LSP \
         Servers popup was open, the popup remained on screen and \
         overlapped the language-picker prompt.\nScreen:\n{screen_after}"
    );
    assert_eq!(
        active_popup_count(&harness),
        0,
        "BUG: LSP Servers popup state should be gone from the popup \
         stack — got {} popup(s) remaining.\nScreen:\n{screen_after}",
        active_popup_count(&harness)
    );
    assert!(
        harness.editor().is_prompting(),
        "precondition: the language indicator click should have \
         opened the language-picker prompt. Screen:\n{screen_after}"
    );
    Ok(())
}

/// Extract the braille spinner glyph from the rendered status bar.
/// Returns the character immediately after the "LSP " literal on the
/// status row, or "?" if the indicator isn't visible.
fn current_spinner_glyph(harness: &EditorTestHarness) -> String {
    let bar = harness.get_status_bar();
    if let Some(pos) = bar.rfind("LSP ") {
        bar[pos + 4..]
            .chars()
            .next()
            .map(|c| c.to_string())
            .unwrap_or_else(|| "?".to_string())
    } else {
        "?".to_string()
    }
}
