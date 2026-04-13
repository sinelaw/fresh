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

    harness.open_file(&file)?;

    // The dormant indicator should appear — pre-condition for the rest
    // of the test. Without it, the popup contents are a distraction
    // because the real issue is upstream.
    harness.wait_until(|h| h.get_status_bar().contains("LSP (off)"))?;

    // Open the popup the same way the status-bar click handler would.
    harness.editor_mut().show_lsp_status_popup();

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
        "expected a 'Disable LSP pill for rust' row. Items: {:#?}",
        items
    );

    Ok(())
}

/// Dismissing a language transitions the indicator to the muted
/// `OffDismissed` variant and surfaces an "Enable LSP pill for …"
/// action in the popup. Re-enabling restores the yellow `Off` variant
/// and the original "Disable …" action.
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

    harness.open_file(&file)?;
    harness.wait_until(|h| h.get_status_bar().contains("LSP (off)"))?;

    // Precondition: not dismissed.
    assert!(
        !harness.editor().is_lsp_language_user_dismissed("rust"),
        "precondition: should not be dismissed"
    );

    // Dismiss directly through the action handler — this is what the
    // popup dispatches when the user picks the "Disable LSP pill" row.
    harness
        .editor_mut()
        .handle_lsp_status_action("dismiss:rust");
    assert!(
        harness.editor().is_lsp_language_user_dismissed("rust"),
        "after dismiss, language should be marked dismissed"
    );

    // Text of the pill stays `LSP (off)` — only the style changes,
    // which is carried by `LspIndicatorState::OffDismissed` inside the
    // render path (not observable from plain text).
    let _ = harness.render();
    assert!(
        harness.get_status_bar().contains("LSP (off)"),
        "text does not change on dismiss. status bar: {}",
        harness.get_status_bar()
    );

    // Re-enable round-trips cleanly.
    harness.editor_mut().handle_lsp_status_action("enable:rust");
    assert!(
        !harness.editor().is_lsp_language_user_dismissed("rust"),
        "after enable, language should no longer be dismissed"
    );

    // Open the popup and confirm the action row text flipped back to
    // the "Disable" form.
    harness.editor_mut().show_lsp_status_popup();
    let items = popup_items(&harness);
    assert!(
        items
            .iter()
            .any(|(_, data, _)| data.as_deref() == Some("dismiss:rust")),
        "re-enabled state should show the Disable action again. Items: {:#?}",
        items
    );

    // Dismiss once more and verify the popup now offers Enable.
    // `show_lsp_status_popup` toggles, so call it to close first.
    harness.editor_mut().show_lsp_status_popup();
    harness
        .editor_mut()
        .handle_lsp_status_action("dismiss:rust");
    harness.editor_mut().show_lsp_status_popup();
    let items = popup_items(&harness);
    assert!(
        items
            .iter()
            .any(|(_, data, _)| data.as_deref() == Some("enable:rust")),
        "dismissed state should offer Enable action. Items: {:#?}",
        items
    );

    Ok(())
}
