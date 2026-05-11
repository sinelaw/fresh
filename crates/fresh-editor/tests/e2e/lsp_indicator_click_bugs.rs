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

#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn issue_1_click_stacks_plugin_popup_and_lsp_servers_popup() -> anyhow::Result<()> {
    let temp = tempfile::tempdir()?;
    let file = temp.path().join("hello.rs");
    std::fs::write(&file, "fn main() {}\n")?;

    let mut harness = EditorTestHarness::create(
        140,
        40,
        HarnessOptions::new()
            .with_config(config_with_rust_lsp("/definitely/not/a/real/rust-analyzer"))
            .with_working_dir(temp.path().to_path_buf()),
    )?;

    harness.open_file(&file)?;
    // Wait for the spawn attempt to fail and for the plugin's
    // `lsp_server_error` hook to run (it sets the status message
    // "Rust LSP server '…' not found. Click status bar for help.").
    harness.wait_until(|h| {
        h.get_status_bar()
            .contains("not found. Click status bar for help.")
            || h.screen_to_string()
                .contains("not found. Click status bar for help.")
    })?;

    // Click the indicator. This fires the `lsp_status_clicked` hook AND
    // unconditionally builds the LSP-servers popup.
    harness.editor_mut().show_lsp_status_popup();
    harness.render()?;

    let count = active_popup_count(&harness);
    let screen = harness.screen_to_string();

    // The plugin's popup title is "Rust Language Server Not Found"; the
    // built-in popup's title is "LSP Servers (rust)". Both appear today.
    let plugin_popup_visible = screen.contains("Rust Language Server Not Found");
    let lsp_servers_popup_visible = screen.contains("LSP Servers (rust)");

    assert!(
        !(plugin_popup_visible && lsp_servers_popup_visible),
        "BUG: clicking the LSP indicator showed BOTH the rust-lsp plugin \
         popup AND the built-in LSP Servers popup at the same time. \
         A single gesture should produce at most one popup.\n\
         popup stack depth = {count}\n\
         Screen:\n{screen}"
    );
    Ok(())
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
    harness.wait_until(|h| h.editor().has_active_lsp_progress())?;

    // Find the server pid via the script-log marker (the script's
    // background `emit_progress` runs in the same process group).
    // Easier: just SIGKILL anything whose argv contains our script
    // path. The harness's child-LSP process is the one we want.
    let script_name = script.file_name().unwrap().to_string_lossy().to_string();
    let _ = std::process::Command::new("pkill")
        .args(["-9", "-f", &script_name])
        .status();

    // Give the OS a moment to actually deliver the signal and for the
    // editor's stdout-read loop to see EOF. The fix should propagate
    // that EOF into `lsp_server_statuses` / `lsp_progress` cleanup —
    // *that* is what we're testing for. Use wall-clock waits, not
    // wait_until, because we want to assert behaviour at a specific
    // point, not "eventually".
    for _ in 0..40 {
        std::thread::sleep(Duration::from_millis(50));
        harness.render()?;
    }

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
        !harness.editor().has_active_lsp_progress(),
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
    harness.wait_until(|h| h.editor().is_lsp_server_ready("rust"))?;

    // Sanity preconditions.
    assert!(
        harness.editor().is_lsp_server_ready("rust"),
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
        !harness.editor().is_lsp_server_ready("rust"),
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
// There's no animation/timer registered for the case "LSP progress is
// active", so in real-world use, between two unrelated events the
// indicator looks frozen.
//
// We can't directly assert "the terminal redraws on its own" from a
// test harness (the harness drives `render` explicitly). What we *can*
// assert is the underlying contract that any fix needs to honour:
// **while `lsp_progress` is active, the editor must request the next
// frame to land within roughly the spinner period (≤ ~120ms)**.
//
// Today that contract isn't expressed anywhere — there's no
// `Editor::next_redraw_deadline()` that reports a sub-second deadline
// when progress is in flight. The most direct symptom we can hit from
// a test is the `view::animation` runner: if a frame schedule existed,
// `animations.is_active()` would be true while progress is active.
// We assert that, knowing it fails today and that any fix should make
// it pass.

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
    harness.wait_until(|h| h.editor().has_active_lsp_progress())?;

    // Drain any pending input/IO so the next render reflects steady
    // state — we want to measure "what would happen if the user does
    // nothing".
    harness.render()?;

    // Capture the indicator glyph from the status row.
    let glyph_now = current_spinner_glyph(&harness);

    // Wall-clock pause longer than the spinner's documented 100ms
    // period, *without* sending any input or LSP traffic. In real life
    // this is the user just looking at the screen.
    std::thread::sleep(Duration::from_millis(350));

    // Render once. If the editor had requested a scheduled frame at
    // ~100ms (the fix), this re-render is exactly what that scheduler
    // would have triggered — and the glyph would now be different.
    // Today nothing requests it, but if a user-driven re-render
    // happens, the glyph WILL be different because the formula uses
    // wall-clock time and we slept 350ms. So this test passes today
    // in the harness even though it doesn't in real life.
    //
    // Hence the actual assertion: the editor must EXPOSE a way to
    // know it wants a redraw. We probe `has_active_lsp_progress` →
    // there's no companion `next_render_deadline()` accessor. Until
    // such an accessor exists, the indicator can't tick on its own.
    //
    // Concretely we assert: while progress is active, the active
    // window's animation runner should be active (so the main loop
    // re-renders at frame rate). This is one viable hook for the
    // fix and is currently false.
    let animations_active = harness.editor().active_window().animations.is_active();
    assert!(
        animations_active,
        "BUG: while LSP `$/progress` is in flight, the editor exposes \
         no scheduled-redraw signal — `animations.is_active()` is \
         false. `compose_lsp_status` recomputes the spinner glyph \
         every 100ms from wall-clock time, but nothing in the main \
         loop knows to re-render at that cadence, so in real use the \
         indicator only advances when an unrelated event causes a \
         frame. glyph at t=0: {glyph_now}"
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
