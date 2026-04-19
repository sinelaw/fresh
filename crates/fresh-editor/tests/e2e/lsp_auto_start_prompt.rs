//! E2E tests: auto-prompt when opening a file for a language with a
//! configured LSP server that's enabled but `auto_start = false`.
//!
//! When a user has configured an LSP server for a language but left
//! `auto_start = false`, opening a matching file used to produce a
//! silent `LSP (off)` pill and nothing else — the user could easily
//! miss that a server is one command away. These tests pin the
//! behaviour that the LSP status popup auto-shows on first file open
//! for such languages, and that the popup offers the pair of actions:
//!
//!   * "Start <server> (always)" — persist `auto_start = true` AND
//!                                  start now. Listed first and
//!                                  pre-selected, because
//!                                  persistent-start is what most
//!                                  users want.
//!   * "Start <server> once"     — start now, config unchanged.
//!
//! named as siblings so the difference ("just this session" vs.
//! "and every future session") is legible at a glance.

use crate::common::harness::{EditorTestHarness, HarnessOptions};

/// Build a harness and flip the auto-prompt flag ON for it — the
/// test-harness ctor disables auto-prompt process-wide so unrelated
/// tests don't get popup keystroke interception. Tests in this file
/// specifically assert on the auto-prompt, so they re-enable it on
/// their own editor instance.
fn harness_with_auto_prompt(
    width: u16,
    height: u16,
    options: HarnessOptions,
) -> anyhow::Result<EditorTestHarness> {
    let mut harness = EditorTestHarness::create(width, height, options)?;
    harness.editor_mut().set_lsp_auto_prompt_enabled(true);
    Ok(harness)
}

fn make_config_with_dormant_rust_lsp() -> fresh::config::Config {
    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: "rust-analyzer".to_string(),
            args: vec![],
            enabled: true,
            auto_start: false,
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
            env: Default::default(),
            language_id_overrides: Default::default(),
            root_markers: Default::default(),
            name: Some("rust-analyzer".to_string()),
            only_features: None,
            except_features: None,
        }]),
    );
    config
}

/// Collect the currently-visible popup's list item text lines, in order.
fn popup_items(harness: &EditorTestHarness) -> Vec<(String, Option<String>, bool)> {
    harness
        .editor()
        .active_state()
        .popups
        .top()
        .map(|p| match &p.content {
            fresh::view::popup::PopupContent::List { items, .. } => items
                .iter()
                .map(|i| (i.text.clone(), i.data.clone(), i.disabled))
                .collect(),
            _ => Vec::new(),
        })
        .unwrap_or_default()
}

/// Opening a file whose language has a configured, enabled, but
/// `auto_start = false` LSP server MUST auto-show the LSP status popup
/// on first open. Without this prompt the user sees a silent `LSP
/// (off)` pill and may not notice that a server is one action away.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_popup_auto_shows_on_open_for_dormant_lsp() -> anyhow::Result<()> {
    let temp = tempfile::tempdir()?;
    let file = temp.path().join("hello.rs");
    std::fs::write(&file, "fn main() {}\n")?;

    let mut harness = harness_with_auto_prompt(
        120,
        30,
        HarnessOptions::new()
            .with_config(make_config_with_dormant_rust_lsp())
            .with_working_dir(temp.path().to_path_buf()),
    )?;

    harness.open_file(&file)?;
    harness.render()?;

    // The auto-shown popup is the same List popup used by the
    // status-bar click path (the existing LSP status popup), so its
    // items match the shape that `popup_items` knows how to decode.
    let items = popup_items(&harness);
    assert!(
        !items.is_empty(),
        "LSP status popup should auto-show when opening a file whose language has \
         an enabled-but-auto_start=false server. Got no visible list popup."
    );
    // Sanity: popup's header row mentions our configured server, so
    // we're looking at an LSP popup and not some unrelated prompt.
    assert!(
        items.iter().any(|(t, _, _)| t.contains("rust-analyzer")),
        "popup should list the configured server rust-analyzer. Items: {:#?}",
        items
    );

    Ok(())
}

/// The auto-shown popup must offer BOTH a "Start <server> (always)"
/// row (persists `auto_start = true`) and a "Start <server> once" row
/// (session-only), so the user can pick between persistent and one-off
/// behaviour directly from the prompt without editing the on-disk
/// config by hand.
///
/// Order matters: "(always)" is listed first so it's the default
/// selection — the common case is "yes, I want this server from now
/// on", which should be achievable with Enter alone.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_popup_offers_start_always_action() -> anyhow::Result<()> {
    let temp = tempfile::tempdir()?;
    let file = temp.path().join("hello.rs");
    std::fs::write(&file, "fn main() {}\n")?;

    let mut harness = harness_with_auto_prompt(
        120,
        30,
        HarnessOptions::new()
            .with_config(make_config_with_dormant_rust_lsp())
            .with_working_dir(temp.path().to_path_buf()),
    )?;

    harness.open_file(&file)?;
    harness.render()?;

    let items = popup_items(&harness);

    // "Start <server> (always)" — persistent sibling.
    let always_row = items
        .iter()
        .find(|(_, data, _)| data.as_deref() == Some("autostart:rust/rust-analyzer"))
        .unwrap_or_else(|| {
            panic!(
                "popup should offer a 'Start rust-analyzer (always)' action with \
                 data 'autostart:rust/rust-analyzer'. Items: {:#?}",
                items
            )
        });
    let (always_label, _, always_disabled) = always_row;
    assert!(
        !always_disabled,
        "the Start (always) row must be actionable (not disabled). Label: {:?}",
        always_label
    );
    assert!(
        always_label.contains("Start rust-analyzer (always)"),
        "row label must read 'Start <server> (always)'. Label: {:?}",
        always_label
    );

    // "Start <server> once" — session-only sibling.
    let once_row = items
        .iter()
        .find(|(_, data, _)| data.as_deref() == Some("start:rust"))
        .unwrap_or_else(|| {
            panic!(
                "popup must also offer a 'start:rust' row. Items: {:#?}",
                items
            )
        });
    let (once_label, _, once_disabled) = once_row;
    assert!(!once_disabled, "the 'Start once' row must be actionable");
    assert!(
        once_label.contains("Start rust-analyzer once"),
        "row label must read 'Start <server> once' to distinguish it from the \
         (always) sibling. Label: {:?}",
        once_label
    );

    // Order + default selection: "(always)" must come before "once"
    // in the popup, so the pre-selected row (the first actionable
    // item) is the persistent one.
    let always_idx = items
        .iter()
        .position(|(_, data, _)| data.as_deref() == Some("autostart:rust/rust-analyzer"))
        .expect("always row present");
    let once_idx = items
        .iter()
        .position(|(_, data, _)| data.as_deref() == Some("start:rust"))
        .expect("once row present");
    assert!(
        always_idx < once_idx,
        "'(always)' should come before 'once' so Enter picks the persistent \
         option. always_idx={}, once_idx={}, items={:#?}",
        always_idx,
        once_idx,
        items
    );

    Ok(())
}

/// The popup's default selection lands on the "(always)" row so the
/// user can just press Enter — separate from position ordering (a
/// popup could in theory list "(always)" first but pre-select a
/// later row). Verified by checking the popup's `selected` index
/// matches the "(always)" row.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_popup_preselects_start_always() -> anyhow::Result<()> {
    let temp = tempfile::tempdir()?;
    let file = temp.path().join("hello.rs");
    std::fs::write(&file, "fn main() {}\n")?;

    let mut harness = harness_with_auto_prompt(
        120,
        30,
        HarnessOptions::new()
            .with_config(make_config_with_dormant_rust_lsp())
            .with_working_dir(temp.path().to_path_buf()),
    )?;

    harness.open_file(&file)?;
    harness.render()?;

    let popup = harness
        .editor()
        .active_state()
        .popups
        .top()
        .expect("popup should be visible");
    let (items, selected) = match &popup.content {
        fresh::view::popup::PopupContent::List { items, selected } => (items, *selected),
        _ => panic!("expected a List popup"),
    };
    let selected_item = items.get(selected).expect("selected index in range");
    assert_eq!(
        selected_item.data.as_deref(),
        Some("autostart:rust/rust-analyzer"),
        "default selection should be the '(always)' row so Enter kicks off \
         persistent-start. Selected row: {:?}, all items: {:#?}",
        selected_item,
        items
    );

    Ok(())
}

/// Invoking "Start <server> automatically" must persist
/// `auto_start = true` in the live config so the server starts
/// automatically on subsequent opens (and, by extension, gets saved to
/// disk via the same save_config path used by other LSP popup actions).
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_enable_autostart_action_sets_flag_in_config() -> anyhow::Result<()> {
    let temp = tempfile::tempdir()?;
    let file = temp.path().join("hello.rs");
    std::fs::write(&file, "fn main() {}\n")?;

    let mut harness = harness_with_auto_prompt(
        120,
        30,
        HarnessOptions::new()
            .with_config(make_config_with_dormant_rust_lsp())
            .with_working_dir(temp.path().to_path_buf()),
    )?;

    harness.open_file(&file)?;
    harness.render()?;

    // Precondition: auto_start is false.
    let before = harness
        .editor()
        .config()
        .lsp
        .get("rust")
        .map(|cfg| cfg.as_slice()[0].auto_start)
        .expect("rust config must be present");
    assert!(!before, "precondition: auto_start must start false");

    harness
        .editor_mut()
        .handle_lsp_status_action("autostart:rust/rust-analyzer");

    let after = harness
        .editor()
        .config()
        .lsp
        .get("rust")
        .map(|cfg| cfg.as_slice()[0].auto_start)
        .expect("rust config must still be present");
    assert!(
        after,
        "after invoking the autostart action, config must have auto_start=true"
    );

    Ok(())
}

/// Invoking "Start <server> automatically" must do BOTH things it
/// advertises: persist `auto_start = true` AND start the server now,
/// so LSP features come alive in the current buffer without the user
/// having to re-open the file or toggle it by hand.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_enable_autostart_action_also_spawns_server() -> anyhow::Result<()> {
    let temp = tempfile::tempdir()?;

    // Minimal fake LSP: writes a PID to the log on spawn, replies
    // to the initialize handshake, and idles. The log file's
    // existence is the witness that the server actually started.
    let script_path = temp.path().join("fake_autostart_lsp.sh");
    let script = r##"#!/bin/bash
LOG_FILE="$1"
echo "$$" >> "$LOG_FILE"

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
    std::fs::write(&script_path, script)?;
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = std::fs::metadata(&script_path)?.permissions();
        perms.set_mode(0o755);
        std::fs::set_permissions(&script_path, perms)?;
    }

    let spawn_log = temp.path().join("spawn.log");
    let file = temp.path().join("hello.rs");
    std::fs::write(&file, "fn main() {}\n")?;

    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: script_path.to_string_lossy().to_string(),
            args: vec![spawn_log.to_string_lossy().to_string()],
            enabled: true,
            auto_start: false,
            name: Some("rust-analyzer".to_string()),
            ..Default::default()
        }]),
    );

    let mut harness = harness_with_auto_prompt(
        120,
        30,
        HarnessOptions::new()
            .with_config(config)
            .with_working_dir(temp.path().to_path_buf()),
    )?;

    harness.open_file(&file)?;
    harness.render()?;

    // Precondition: the auto-prompt popup is open (that's the path
    // under test); server has NOT spawned yet because auto_start=false.
    assert!(
        !spawn_log.exists(),
        "precondition: server must not have spawned before action"
    );

    harness
        .editor_mut()
        .handle_lsp_status_action("autostart:rust/rust-analyzer");

    // Semantic wait: spawn log exists iff the server process started.
    harness.wait_until(|_| spawn_log.exists())?;

    Ok(())
}

/// If the configured LSP already has `auto_start = true`, no prompt is
/// needed — the server starts (or tries to) automatically, so auto-
/// popping the popup would be a distraction. This is the negative
/// control: opening a file for an `auto_start=true` server must NOT
/// auto-show the LSP popup.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_popup_does_not_auto_show_when_auto_start_true() -> anyhow::Result<()> {
    let temp = tempfile::tempdir()?;
    let file = temp.path().join("hello.rs");
    std::fs::write(&file, "fn main() {}\n")?;

    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            // Point at a missing binary so nothing actually spawns and
            // the test stays hermetic. The field under test is the
            // prompt-or-not decision, which keys off `auto_start`, not
            // binary availability.
            command: "this-binary-definitely-does-not-exist-xyz999".to_string(),
            args: vec![],
            enabled: true,
            auto_start: true,
            name: Some("rust-analyzer".to_string()),
            ..Default::default()
        }]),
    );

    let mut harness = harness_with_auto_prompt(
        120,
        30,
        HarnessOptions::new()
            .with_config(config)
            .with_working_dir(temp.path().to_path_buf()),
    )?;

    harness.open_file(&file)?;
    harness.render()?;

    assert!(
        harness.editor().active_state().popups.top().is_none(),
        "no auto-prompt should appear when auto_start=true is already configured"
    );

    Ok(())
}

/// When the user has dismissed the LSP pill for this language (via the
/// popup's "Disable LSP pill for …" action), the auto-prompt must
/// respect that dismissal and stay silent on subsequent file opens —
/// otherwise the mute surface would be useless, since the popup would
/// re-pop every time the user opens another file of the same language.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_auto_prompt_respects_user_dismissal() -> anyhow::Result<()> {
    let temp = tempfile::tempdir()?;
    let first = temp.path().join("a.rs");
    let second = temp.path().join("b.rs");
    std::fs::write(&first, "fn main() {}\n")?;
    std::fs::write(&second, "fn main() {}\n")?;

    let mut harness = harness_with_auto_prompt(
        120,
        30,
        HarnessOptions::new()
            .with_config(make_config_with_dormant_rust_lsp())
            .with_working_dir(temp.path().to_path_buf()),
    )?;

    harness.open_file(&first)?;
    harness.render()?;

    // First open: popup shows.
    assert!(
        harness.editor().active_state().popups.top().is_some(),
        "first open should auto-show the prompt"
    );

    // User dismisses the pill and closes the popup.
    harness
        .editor_mut()
        .handle_lsp_status_action("dismiss:rust");
    harness.editor_mut().hide_popup();
    harness.render()?;

    // Second file of the same language — no re-prompt.
    harness.open_file(&second)?;
    harness.render()?;

    assert!(
        harness.editor().active_state().popups.top().is_none(),
        "after the user dismissed the LSP pill for rust, opening another rust file \
         must not re-pop the prompt"
    );

    Ok(())
}

/// Regression: when a session (re)opens multiple buffers of the same
/// language back-to-back — the common shape of a session restore —
/// the auto-prompt must end up on whichever buffer the user is
/// looking at, not stuck on the first one that happened to be active
/// when the prompt fired.
///
/// Today the prompt fires inside `notify_lsp_file_opened` and the
/// popup attaches to `active_buffer()` at that instant. The
/// once-per-session guard then suppresses subsequent fires for the
/// same language, so if the active buffer flips after the first open
/// (as it does when a later file is opened and takes focus) the popup
/// is orphaned on a background buffer and the user sees nothing.
///
/// The test opens two `.rs` files in sequence — the second one wins
/// focus, so if you imagine this as a session restore, the user lands
/// on `b.rs`. The active buffer at that point must have the prompt;
/// otherwise the auto-prompt is effectively invisible for anyone
/// with more than one buffer open.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_auto_prompt_follows_active_buffer_on_session_restore() -> anyhow::Result<()> {
    let temp = tempfile::tempdir()?;
    let first = temp.path().join("a.rs");
    let second = temp.path().join("b.rs");
    std::fs::write(&first, "fn main() {}\n")?;
    std::fs::write(&second, "fn other() {}\n")?;

    let mut harness = harness_with_auto_prompt(
        120,
        30,
        HarnessOptions::new()
            .with_config(make_config_with_dormant_rust_lsp())
            .with_working_dir(temp.path().to_path_buf()),
    )?;

    // Simulate a session restore that reopens two rust buffers in a
    // row. Each call's `notify_lsp_file_opened` runs with that file
    // already the active buffer — but only the first open will fire
    // the prompt (per the once-per-session guard), and it attaches
    // the popup to `a.rs`. The second open flips active → `b.rs`.
    harness.open_file(&first)?;
    harness.render()?;
    harness.open_file(&second)?;
    harness.render()?;

    // Sanity: the last opened file is what the user is looking at.
    // Asserted via the rendered tab bar rather than internal state
    // so the test speaks the user's language ("what's on my
    // screen") — not the editor's.
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("b.rs"),
        "precondition: active tab should be b.rs. Screen:\n{}",
        screen
    );

    // The user is on `b.rs` with a dormant rust LSP. The auto-prompt
    // must be visible here — not stuck on `a.rs` in the background.
    // `active_state().popups.top()` returns the popup stack for the
    // currently-visible buffer only, so a `None` here means the
    // popup exists elsewhere (or not at all) and the user sees
    // nothing.
    assert!(
        harness.editor().active_state().popups.top().is_some(),
        "auto-prompt should be visible on whichever rust buffer is currently \
         active, not orphaned on a background buffer the user may never visit. \
         Screen:\n{}",
        screen
    );

    Ok(())
}

/// Picking "Disable LSP for <lang>" in the auto-prompt popup must do
/// two user-visible things:
///
///   1. The row label drops the word "pill" — from the user's
///      perspective they're disabling the LSP, not some pill widget.
///   2. It flips `enabled = false` in the persisted config. The
///      old behaviour (`user_dismissed_lsp_languages`, a session-
///      scoped HashSet) didn't survive a restart, so the user got
///      re-prompted the next time they opened the editor and
///      rightly concluded the "Disable" action didn't actually do
///      what it said.
///
/// After invoking the action, the in-memory config (which is what
/// `save_config` writes to disk) must have `enabled = false` on the
/// rust LSP entry — and the auto-prompt path must therefore stay
/// silent on subsequent file opens.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_disable_action_persists_enabled_false() -> anyhow::Result<()> {
    let temp = tempfile::tempdir()?;
    let file = temp.path().join("hello.rs");
    std::fs::write(&file, "fn main() {}\n")?;

    let mut harness = harness_with_auto_prompt(
        120,
        30,
        HarnessOptions::new()
            .with_config(make_config_with_dormant_rust_lsp())
            .with_working_dir(temp.path().to_path_buf()),
    )?;

    harness.open_file(&file)?;
    harness.render()?;

    // Precondition: the popup is up and its disable row exists —
    // asserted in `test_popup_auto_shows_on_open_for_dormant_lsp`
    // and `test_popup_offers_start_always_action`. Here we focus on
    // the label + side-effect of the disable row.

    // 1. Label: the visible row should NOT mention "pill".
    let items = popup_items(&harness);
    let disable_row = items
        .iter()
        .find(|(_, data, _)| data.as_deref() == Some("dismiss:rust"))
        .unwrap_or_else(|| {
            panic!(
                "popup should offer a 'dismiss:rust' action. Items: {:#?}",
                items
            )
        });
    let (disable_label, _, _) = disable_row;
    assert!(
        !disable_label.contains("pill"),
        "row label shouldn't expose the internal 'pill' noun. Label: {:?}",
        disable_label
    );
    assert!(
        disable_label.contains("Disable LSP for rust"),
        "row label should read 'Disable LSP for <lang>'. Label: {:?}",
        disable_label
    );

    // Precondition for (2): the config currently has enabled=true.
    assert!(
        harness
            .editor()
            .config()
            .lsp
            .get("rust")
            .map(|cfg| cfg.as_slice()[0].enabled)
            .unwrap(),
        "precondition: rust LSP should start as enabled=true"
    );

    harness
        .editor_mut()
        .handle_lsp_status_action("dismiss:rust");

    // 2a. The action flipped enabled=false in the live config (the
    //     same config `save_config` serializes to disk).
    let enabled_after = harness
        .editor()
        .config()
        .lsp
        .get("rust")
        .map(|cfg| cfg.as_slice()[0].enabled)
        .expect("rust config must still be present");
    assert!(
        !enabled_after,
        "BUG: 'Disable LSP for rust' must set enabled=false in the config \
         so the change survives an editor restart. The old session-only \
         `user_dismissed_lsp_languages` HashSet meant the next session \
         re-prompted the user, contradicting the action's label."
    );

    // 2b. Effect: subsequent file opens in this same session no
    //     longer pop the auto-prompt, because enabled=false takes
    //     the spawn flow down the `Failed` path rather than
    //     `NotAutoStart`. (A second harness + fresh config-reload
    //     would assert the cross-restart half, but touching the
    //     disk layer is disproportionate here; the config-state
    //     assertion above already pins the invariant that gets
    //     serialized.)
    harness.editor_mut().hide_popup();
    harness.render()?;

    let second = temp.path().join("b.rs");
    std::fs::write(&second, "fn main() {}\n")?;
    harness.open_file(&second)?;
    harness.render()?;

    assert!(
        harness.editor().active_state().popups.top().is_none(),
        "after 'Disable LSP for rust', opening another rust file must not \
         re-pop the auto-prompt"
    );

    Ok(())
}

/// The popup must expose its own "Dismiss" affordance as a selectable
/// row — not rely on the user to know Esc closes it — and the row
/// label must advertise the current keybinding so the UI stays
/// truthful when the user has rebound things.
///
/// Specifically:
///   1. A row with data `cancel_popup` is present, actionable, and
///      sits at the end of the popup (after all the server-specific
///      and language-level actions).
///   2. The row label reads "Dismiss (<key>)" where <key> is the
///      currently-bound key for `Action::PopupCancel` in the Popup
///      key context — looked up via the keybinding resolver, not
///      hardcoded. On a stock keymap that's "Esc".
///   3. Invoking the row (via `handle_popup_confirm`, the same path
///      the Enter key goes through) closes the popup.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_popup_offers_dismiss_row_with_dynamic_keybinding() -> anyhow::Result<()> {
    let temp = tempfile::tempdir()?;
    let file = temp.path().join("hello.rs");
    std::fs::write(&file, "fn main() {}\n")?;

    let mut harness = harness_with_auto_prompt(
        120,
        30,
        HarnessOptions::new()
            .with_config(make_config_with_dormant_rust_lsp())
            .with_working_dir(temp.path().to_path_buf()),
    )?;

    harness.open_file(&file)?;
    harness.render()?;

    let items = popup_items(&harness);

    // (1) Row present, actionable, and last.
    let dismiss_idx = items
        .iter()
        .position(|(_, data, _)| data.as_deref() == Some("cancel_popup"))
        .unwrap_or_else(|| {
            panic!(
                "popup should offer a 'cancel_popup' row so users can dismiss \
                 via selection, not only via Esc. Items: {:#?}",
                items
            )
        });
    let (label, _, disabled) = &items[dismiss_idx];
    assert!(
        !disabled,
        "the Dismiss row must be actionable. Label: {:?}",
        label
    );
    assert_eq!(
        dismiss_idx,
        items.len() - 1,
        "Dismiss should be the LAST row — it's a fallback out, not a \
         server-specific action. Items: {:#?}",
        items
    );

    // (2) Label mentions Dismiss + the bound key looked up dynamically
    //     from the keymap system. On the default keymap PopupCancel is
    //     bound to Esc, which is what `format_keybinding` stringifies
    //     for `KeyCode::Esc`.
    assert!(
        label.contains("Dismiss"),
        "label should mention 'Dismiss'. Label: {:?}",
        label
    );
    assert!(
        label.contains("Esc"),
        "label should include the dynamically-resolved keybinding for \
         Action::PopupCancel (default: Esc). Hardcoding 'Esc' here is a \
         smell, but the test pins the default; a keymap change would \
         flip the label and this assertion would guide the update. \
         Label: {:?}",
        label
    );

    // (3) Invoking the row closes the popup. `show_lsp_status_popup`
    //     toggles based on `pending_lsp_status_popup`, so after
    //     confirm the popup should be gone.
    harness
        .editor_mut()
        .active_state_mut()
        .popups
        .top_mut()
        .expect("popup visible")
        .select_index(dismiss_idx);
    harness.editor_mut().handle_popup_confirm();
    harness.render()?;

    assert!(
        harness.editor().active_state().popups.top().is_none(),
        "selecting the Dismiss row should close the popup"
    );

    Ok(())
}
