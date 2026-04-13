//! E2E tests for LSP code action modal behavior.
//!
//! Tests for issue #1405: pressing numbers in the code action modal should
//! select and apply the corresponding action.

use crate::common::fake_lsp::FakeLspServer;
use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Issue #1405: pressing a number key should select, dismiss the popup,
/// and apply the workspace edit from the code action.
///
/// The fake LSP returns "Add missing import" (action 3) with a workspace edit
/// that inserts "use std::io;\n" at the top of the file.
#[test]
#[cfg_attr(
    target_os = "windows",
    ignore = "FakeLspServer uses a Bash script which is not available on Windows"
)]
fn test_code_action_number_key_selects_and_applies() -> anyhow::Result<()> {
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

    let mut harness = EditorTestHarness::create(
        // 120×24 (not 80) so the status bar has room for the new
        // 11-cell `LSP (on)` indicator on the right alongside the
        // file name + cursor + message + language pill. At 80 cols
        // the status bar truncates and `wait_for_screen_contains("LSP (on)")`
        // never matches, hanging the test until the CI 180s timeout
        // (matches the widening commit 8ab5337 did for visual-regression
        // and settings/markdown_compose tests).
        120,
        24,
        crate::common::harness::HarnessOptions::new()
            .with_config(config)
            .with_working_dir(temp_dir.path().to_path_buf()),
    )?;

    harness.open_file(&test_file)?;
    harness.render()?;

    // Wait for LSP to be ready
    harness.wait_for_screen_contains("LSP (on)")?;

    // Position cursor on "let x = 5;" (line 2)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.render()?;

    // Trigger code actions via Alt+.
    harness.send_key(KeyCode::Char('.'), KeyModifiers::ALT)?;
    harness.render()?;

    // Wait for code action popup
    harness.wait_for_screen_contains("Extract function")?;

    // Verify the popup shows numbered code actions
    harness.assert_screen_contains("1. Extract function");
    harness.assert_screen_contains("3. Add missing import");

    // Press '3' to select "Add missing import" which has a real workspace edit
    harness.send_key(KeyCode::Char('3'), KeyModifiers::NONE)?;
    harness.render()?;

    // The popup should be dismissed
    harness.assert_screen_not_contains("Code Actions");

    // The workspace edit should have been applied: "use std::io;\n" inserted at top
    let buffer = harness.get_buffer_content().unwrap();
    assert_eq!(
        buffer, "use std::io;\nfn main() {\n    let x = 5;\n}\n",
        "Expected 'use std::io;' to be inserted at the top of the file by the code action"
    );

    Ok(())
}

/// Arrow-down + Enter should navigate to an action and apply it.
#[test]
#[cfg_attr(
    target_os = "windows",
    ignore = "FakeLspServer uses a Bash script which is not available on Windows"
)]
fn test_code_action_arrow_enter_applies() -> anyhow::Result<()> {
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

    let mut harness = EditorTestHarness::create(
        // 120×24 (not 80): status bar room for `LSP (on)`. See sibling
        // test `test_code_action_number_key_selects_and_applies` for
        // the longer rationale.
        120,
        24,
        crate::common::harness::HarnessOptions::new()
            .with_config(config)
            .with_working_dir(temp_dir.path().to_path_buf()),
    )?;

    harness.open_file(&test_file)?;
    harness.render()?;

    // Wait for LSP to be ready
    harness.wait_for_screen_contains("LSP (on)")?;

    // Position cursor
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.render()?;

    // Trigger code actions via Alt+.
    harness.send_key(KeyCode::Char('.'), KeyModifiers::ALT)?;
    harness.render()?;

    harness.wait_for_screen_contains("Extract function")?;

    // Navigate down twice to "Add missing import"
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.render()?;

    // Press Enter to confirm
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.render()?;

    // Popup should be dismissed and edit applied
    harness.assert_screen_not_contains("Code Actions");

    let buffer = harness.get_buffer_content().unwrap();
    assert_eq!(
        buffer, "use std::io;\nfn main() {\n    let x = 5;\n}\n",
        "Expected 'use std::io;' to be inserted at the top of the file by the code action"
    );

    Ok(())
}
