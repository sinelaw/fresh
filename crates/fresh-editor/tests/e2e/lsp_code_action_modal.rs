//! E2E tests for LSP code action modal behavior.
//!
//! Reproduces issue #1405: pressing numbers in the code action modal does nothing.
//! This test asserts the *desired* behavior, so it FAILS until the bug is fixed.

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

/// Issue #1405: pressing a number key should select and dismiss the code action popup.
///
/// Currently FAILS because the popup stays open — the number key is consumed but ignored.
#[test]
#[cfg_attr(
    target_os = "windows",
    ignore = "FakeLspServer uses a Bash script which is not available on Windows"
)]
fn test_code_action_number_key_selects_and_dismisses() -> anyhow::Result<()> {
    let (mut harness, _temp_dir) = setup_with_code_action_popup()?;

    // Verify the popup shows numbered code actions
    harness.assert_screen_contains("1. Extract function");
    harness.assert_screen_contains("2. Inline variable");
    harness.assert_screen_contains("3. Add missing import");

    // Press '1' to select the first code action
    harness.send_key(KeyCode::Char('1'), KeyModifiers::NONE)?;
    harness.render()?;

    // The popup should be dismissed after selecting an action
    harness.assert_screen_not_contains("Extract function");

    Ok(())
}
