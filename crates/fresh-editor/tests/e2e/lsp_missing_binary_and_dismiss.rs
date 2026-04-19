//! E2E tests: status-bar LSP indicator pre-click binary probe and the
//! user-dismiss / re-enable flow.
//!
//! These tests cover the second-phase improvement documented in
//! `CHANGELOG.md`: configured-but-dormant LSP servers are now probed
//! for binary presence when the user opens the status popup, and the
//! user has a surface in the popup to mute ("Disable LSP pill for …")
//! or restore ("Enable LSP pill for …") the indicator without editing
//! their on-disk config.
//!
//! The tests intentionally don't spawn real language servers — they
//! drive the `LspServerConfig` + runtime state directly and read back
//! via the public harness + editor accessors. The goal is to pin the
//! *UX*: what rows appear in the popup, what happens to the indicator
//! state when a row is invoked, and that the state transitions round
//! trip cleanly.

use crate::common::harness::{EditorTestHarness, HarnessOptions};

fn make_config_with_missing_rust_lsp() -> fresh::config::Config {
    let mut config = fresh::config::Config::default();
    // Deliberately point at a path that does NOT resolve on $PATH or
    // disk, so the pre-click binary probe buckets this server into
    // "missing". The command name includes a unique suffix so unrelated
    // binaries installed on the test host can't accidentally satisfy
    // the probe.
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: "this-binary-definitely-does-not-exist-xyz123".to_string(),
            args: vec![],
            enabled: true,
            auto_start: false,
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

/// Opening the LSP status popup for a language whose configured server
/// binary cannot be found must:
///   1. Annotate the server row with "binary not in PATH".
///   2. Replace the usual actionable "Start …" row with a disabled
///      advisory "Install … to enable".
///   3. Offer a "Disable LSP pill for {lang}" action.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_missing_binary_popup_shows_advisory_and_dismiss() -> anyhow::Result<()> {
    let temp = tempfile::tempdir()?;
    let file = temp.path().join("hello.rs");
    std::fs::write(&file, "fn main() {}\n")?;

    let mut harness = EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new()
            .with_config(make_config_with_missing_rust_lsp())
            .with_working_dir(temp.path().to_path_buf()),
    )?;
    // Opt this test into the LSP auto-prompt; the harness ctor
    // disables it by default for the rest of the suite.
    harness.editor_mut().set_lsp_auto_prompt_enabled(true);

    harness.open_file(&file)?;

    // The dormant indicator should appear — pre-condition for the rest
    // of the test. Without it, the popup contents are a distraction
    // because the real issue is upstream.
    harness.wait_until(|h| h.get_status_bar().contains("LSP (off)"))?;

    // The LSP auto-prompt already opens the popup on the first
    // file open for a language with enabled+auto_start=false
    // configured, so we don't need to invoke show_lsp_status_popup
    // ourselves — doing so would toggle the popup closed.

    let items = popup_items(&harness);
    assert!(!items.is_empty(), "LSP status popup should have items");

    // 1. Header row reports the missing binary.
    let header_row = items
        .iter()
        .find(|(t, _, _)| t.contains("fake-rust-analyzer"))
        .unwrap_or_else(|| {
            panic!(
                "expected a header row for fake-rust-analyzer, got: {:#?}",
                items
            )
        });
    assert!(
        header_row.0.contains("binary not in PATH"),
        "header row must annotate the missing binary. Row: {:?}",
        header_row
    );

    // 2. No actionable "Start …" row; instead, a disabled advisory.
    let start_row = items.iter().find(|(_, data, _)| {
        data.as_deref()
            .map(|d| d.starts_with("start:"))
            .unwrap_or(false)
    });
    assert!(
        start_row.is_none(),
        "must NOT add a Start row for a missing-binary language. Items: {:#?}",
        items
    );
    let install_row = items
        .iter()
        .find(|(t, _, _)| t.contains("Install fake-rust-analyzer"));
    assert!(
        install_row.is_some() && install_row.unwrap().2,
        "expected a disabled 'Install …' advisory row. Items: {:#?}",
        items
    );

    // 3. Dismiss action present.
    let dismiss_row = items.iter().find(|(_, data, _)| {
        data.as_deref()
            .map(|d| d == "dismiss:rust")
            .unwrap_or(false)
    });
    assert!(
        dismiss_row.is_some(),
        "expected a 'Disable LSP for rust' row. Items: {:#?}",
        items
    );

    Ok(())
}

/// Disable → Enable round-trips through the popup: "Disable LSP for
/// <lang>" flips `enabled = false` in the live config (persisted via
/// `save_config`, so the choice survives a restart), and the
/// complementary "Enable LSP for <lang>" restores `enabled = true`.
/// The status-bar pill stays visible in both states — once as the
/// dimmed `LSP (off)` (disabled), and once as the normal `LSP (off)`
/// (re-enabled but not running) — so the user always has a surface
/// to toggle the state back. Hiding the pill on disable would
/// strand the Enable action.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_dismiss_then_enable_round_trip() -> anyhow::Result<()> {
    let temp = tempfile::tempdir()?;
    let file = temp.path().join("hello.rs");
    std::fs::write(&file, "fn main() {}\n")?;

    let mut harness = EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new()
            .with_config(make_config_with_missing_rust_lsp())
            .with_working_dir(temp.path().to_path_buf()),
    )?;
    // Opt this test into the LSP auto-prompt; the harness ctor
    // disables it by default for the rest of the suite.
    harness.editor_mut().set_lsp_auto_prompt_enabled(true);

    harness.open_file(&file)?;
    harness.wait_until(|h| h.get_status_bar().contains("LSP (off)"))?;

    // Precondition: enabled=true in config.
    let enabled_initial = harness
        .editor()
        .config()
        .lsp
        .get("rust")
        .map(|cfg| cfg.as_slice()[0].enabled)
        .expect("rust config present");
    assert!(
        enabled_initial,
        "precondition: rust LSP should start enabled=true"
    );

    // Disable via the action handler — the path the popup dispatches
    // when the user picks the "Disable LSP for rust" row.
    harness
        .editor_mut()
        .handle_lsp_status_action("dismiss:rust");
    let enabled_after_disable = harness
        .editor()
        .config()
        .lsp
        .get("rust")
        .map(|cfg| cfg.as_slice()[0].enabled)
        .unwrap();
    assert!(
        !enabled_after_disable,
        "Disable LSP for rust must flip enabled=false in config so the \
         choice persists across restarts"
    );

    // Pill should stay visible even when every configured server is
    // `enabled = false`: hiding it would leave the user with no
    // on-screen surface to re-enable later. The variant flips to
    // `OffDismissed` (dimmed) so the state change is still visible;
    // the text itself remains `LSP (off)`.
    harness.render()?;
    assert!(
        harness.get_status_bar().contains("LSP (off)"),
        "after disable, the pill text should still read 'LSP (off)' so the \
         user retains a surface to Enable. Status bar: {}",
        harness.get_status_bar()
    );

    // Re-enable via the symmetric action.
    harness.editor_mut().handle_lsp_status_action("enable:rust");
    let enabled_after_reenable = harness
        .editor()
        .config()
        .lsp
        .get("rust")
        .map(|cfg| cfg.as_slice()[0].enabled)
        .unwrap();
    assert!(
        enabled_after_reenable,
        "Enable LSP for rust must flip enabled=true in config"
    );

    // Pill should come back.
    harness.wait_until(|h| h.get_status_bar().contains("LSP (off)"))?;

    Ok(())
}

/// Reopen-the-popup case: when the language is already disabled in
/// config (e.g. on a fresh session where the persisted
/// `enabled = false` is the only signal — the session-level
/// `user_dismissed_lsp_languages` is empty), the popup must offer
/// "Enable LSP for <lang>", not "Disable". Showing "Disable" while
/// the language is already disabled would leave the user with no
/// surface to undo their previous choice.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_popup_offers_enable_when_config_already_disabled() -> anyhow::Result<()> {
    let temp = tempfile::tempdir()?;
    let file = temp.path().join("hello.rs");
    std::fs::write(&file, "fn main() {}\n")?;

    // Simulates "previous session disabled it" — `enabled = false`
    // but `user_dismissed_lsp_languages` empty. This is exactly the
    // state a user lands in after a restart following a Disable.
    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: "rust-analyzer".to_string(),
            args: vec![],
            enabled: false,
            auto_start: false,
            name: Some("rust-analyzer".to_string()),
            ..Default::default()
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
    harness.render()?;

    // Open the popup explicitly (the auto-prompt is suppressed by the
    // harness ctor, and `enabled = false` would route through the
    // `Failed` spawn path anyway, not `NotAutoStart`).
    harness.editor_mut().show_lsp_status_popup();
    harness.render()?;

    let items = popup_items(&harness);

    let has_enable = items
        .iter()
        .any(|(label, data, _)| data.as_deref() == Some("enable:rust") && label.contains("Enable"));
    let has_disable = items
        .iter()
        .any(|(_, data, _)| data.as_deref() == Some("dismiss:rust"));

    assert!(
        has_enable,
        "popup should offer 'Enable LSP for rust' when every server is \
         disabled in config, regardless of the session-only dismissal \
         flag. Items: {:#?}",
        items
    );
    assert!(
        !has_disable,
        "popup must NOT offer 'Disable LSP for rust' when the language is \
         already disabled — that label leaves the user with no way to \
         re-enable. Items: {:#?}",
        items
    );

    Ok(())
}
