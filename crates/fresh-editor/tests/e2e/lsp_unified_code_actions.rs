//! E2E tests for unified LSP code actions from multiple servers.
//!
//! When two LSP servers are configured for a single language, their code actions
//! should be merged into a single popup instead of showing separate popups.

use crate::common::fake_lsp::FakeLspServer;
use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Two LSP servers for the same language should show all code actions in one popup.
///
/// Server A returns: "Extract function", "Inline variable", "Add missing import"
/// Server B returns: "Format imports", "Convert to arrow function"
/// The popup should contain all 5 actions.
#[test]
#[cfg_attr(
    target_os = "windows",
    ignore = "FakeLspServer uses a Bash script which is not available on Windows"
)]
fn test_code_actions_merged_from_two_servers() -> anyhow::Result<()> {
    let temp_dir = tempfile::tempdir()?;
    let _fake_server_a = FakeLspServer::spawn_with_code_actions(temp_dir.path())?;
    let _fake_server_b = FakeLspServer::spawn_with_code_actions_b(temp_dir.path())?;

    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn main() {\n    let x = 5;\n}\n")?;

    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![
            fresh::services::lsp::LspServerConfig {
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
                name: Some("server-a".to_string()),
                only_features: None,
                except_features: None,
            },
            fresh::services::lsp::LspServerConfig {
                command: FakeLspServer::code_actions_b_script_path(temp_dir.path())
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
                name: Some("server-b".to_string()),
                only_features: None,
                except_features: None,
            },
        ]),
    );

    let mut harness = EditorTestHarness::create(
        // 120×24 (not 80): the status bar at width 80 truncates the
        // right-side `LSP (on)` indicator, so the
        // `wait_for_screen_contains("LSP (on)")` poll below never
        // matches and the test hangs to CI's 180s timeout. Same
        // widening pattern as 8ab5337.
        120,
        24,
        crate::common::harness::HarnessOptions::new()
            .with_config(config)
            .with_working_dir(temp_dir.path().to_path_buf()),
    )?;

    harness.open_file(&test_file)?;
    harness.render()?;

    // Wait for both LSP servers to be ready
    harness.wait_for_screen_contains("LSP (on)")?;

    // Position cursor on "let x = 5;" (line 2)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.render()?;

    // Trigger code actions via Alt+.
    harness.send_key(KeyCode::Char('.'), KeyModifiers::ALT)?;
    harness.render()?;

    // Wait for code action popup — actions from server A
    harness.wait_for_screen_contains("Extract function")?;

    // Wait a bit for server B's response to arrive and merge
    // Use semantic waiting: wait until server B's action appears
    harness.wait_for_screen_contains("Format imports")?;

    // Verify that actions from BOTH servers appear in the same popup
    // Server A actions:
    harness.assert_screen_contains("Extract function");
    harness.assert_screen_contains("Add missing import");
    // Server B actions:
    harness.assert_screen_contains("Format imports");
    harness.assert_screen_contains("Convert to arrow function");

    Ok(())
}
