//! E2E tests for LSP code action modal behavior.
//!
//! Reproduces issue #1405: pressing numbers in the code action modal does nothing.
//! The code action popup is displayed as read-only text (PopupKind::Text) with
//! numbered items, but number key presses are consumed without any effect.

use crate::common::fake_lsp::FakeLspServer;
use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Set up an editor with a fake LSP server that supports code actions,
/// wait for LSP readiness, then trigger code actions and wait for the popup.
/// Returns (harness, temp_dir) — temp_dir must be kept alive for the LSP process.
fn setup_with_code_action_popup() -> anyhow::Result<(EditorTestHarness, tempfile::TempDir)> {
    let temp_dir = tempfile::tempdir()?;

    let _fake_server = FakeLspServer::spawn_with_code_actions(temp_dir.path())?;

    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn main() {\n    let x = 5;\n}\n")?;

    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: FakeLspServer::code_actions_script_path(temp_dir.path())
                .to_string_lossy()
                .to_string(),
            args: vec![],
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
        80,
        24,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    harness.open_file(&test_file)?;
    harness.render()?;

    // Wait for LSP to be ready (semantic wait — no timeout)
    harness.wait_for_screen_contains("ready")?;

    // Position cursor on "let x = 5;" (line 2)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.render()?;

    // Request code actions with Ctrl+.
    harness.send_key(KeyCode::Char('.'), KeyModifiers::CONTROL)?;
    harness.render()?;

    // Wait for the code action popup to appear (semantic wait — no timeout)
    harness.wait_for_screen_contains("Extract function")?;

    Ok((harness, temp_dir))
}

/// Issue #1405: pressing number keys in the code action modal does nothing.
///
/// The code action popup displays numbered items (e.g. "1. Extract function")
/// but pressing the corresponding number key doesn't select or execute the action.
/// The number key is consumed (modal behavior) but has no effect.
#[test]
#[cfg_attr(
    target_os = "windows",
    ignore = "FakeLspServer uses a Bash script which is not available on Windows"
)]
fn test_code_action_number_keys_do_nothing() -> anyhow::Result<()> {
    let (mut harness, _temp_dir) = setup_with_code_action_popup()?;

    let screen = harness.screen_to_string();
    println!("Screen after code action popup:\n{screen}");

    // Verify the popup is visible with numbered code actions
    assert!(
        screen.contains("1. Extract function"),
        "Expected numbered code action items in popup"
    );

    // Record the buffer content before pressing a number
    let buffer_before = harness.get_buffer_content().unwrap();

    // Press '1' to try to select the first code action
    harness.send_key(KeyCode::Char('1'), KeyModifiers::NONE)?;
    harness.render()?;

    // BUG: The popup should have closed and the action should have been applied.
    // Instead, the number key is consumed but nothing happens.
    let buffer_after = harness.get_buffer_content().unwrap();

    // The buffer is unchanged because the number key did nothing
    assert_eq!(
        buffer_before, buffer_after,
        "Buffer should be unchanged because number key selection is not implemented"
    );

    // The popup is still visible — the number key was consumed but had no effect
    let screen_after = harness.screen_to_string();
    assert!(
        screen_after.contains("Extract function"),
        "BUG: popup remains open because number key selection is not implemented (issue #1405)"
    );

    // Verify Escape still works to dismiss it
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE)?;
    harness.render()?;

    let screen_dismissed = harness.screen_to_string();
    assert!(
        !screen_dismissed.contains("Extract function"),
        "Popup should be dismissed after pressing Escape"
    );

    Ok(())
}

/// Verify that arrow key navigation and Enter don't apply any code action,
/// because the popup is rendered as read-only text, not as a selectable list.
#[test]
#[cfg_attr(
    target_os = "windows",
    ignore = "FakeLspServer uses a Bash script which is not available on Windows"
)]
fn test_code_action_arrow_keys_no_selection() -> anyhow::Result<()> {
    let (mut harness, _temp_dir) = setup_with_code_action_popup()?;

    // Press Down arrow to try to navigate, then Enter to confirm
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.render()?;

    let buffer_before = harness.get_buffer_content().unwrap();
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.render()?;

    let buffer_after = harness.get_buffer_content().unwrap();

    // The popup is a Text popup, so even Enter+arrow navigation doesn't apply actions.
    // The popup just closes without doing anything useful.
    assert_eq!(
        buffer_before, buffer_after,
        "No code action should have been applied because the popup is text-only"
    );

    Ok(())
}

/// Verify that Escape properly dismisses the code action popup.
#[test]
#[cfg_attr(
    target_os = "windows",
    ignore = "FakeLspServer uses a Bash script which is not available on Windows"
)]
fn test_code_action_escape_dismisses() -> anyhow::Result<()> {
    let (mut harness, _temp_dir) = setup_with_code_action_popup()?;

    // Popup should be visible (code actions shown on screen)
    harness.assert_screen_contains("Code Actions");

    // Press Escape
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE)?;
    harness.render()?;

    // Popup should no longer be on screen
    harness.assert_screen_not_contains("Code Actions");

    Ok(())
}
