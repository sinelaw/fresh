//! E2E test for passing environment variables to LSP server processes.
//!
//! Verifies that the `env` field in `LspServerConfig` is forwarded to the
//! spawned LSP child process by using a fake LSP that echoes an env var
//! in its hover response.

use crate::common::fake_lsp::FakeLspServer;
use crate::common::harness::EditorTestHarness;

/// Test that environment variables configured in LspServerConfig.env
/// are passed through to the spawned LSP process.
///
/// Uses a fake LSP server that reads `FRESH_TEST_ENV_VAR` from its
/// environment and includes the value in hover responses. The test
/// sets this variable in the config and verifies the hover popup
/// displays the expected value.
#[test]
#[cfg_attr(
    target_os = "windows",
    ignore = "FakeLspServer uses a Bash script which is not available on Windows"
)]
fn test_lsp_env_vars_passed_to_server() -> anyhow::Result<()> {
    let temp_dir = tempfile::tempdir()?;
    let _fake_server = FakeLspServer::spawn_env_echo(temp_dir.path())?;
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn example_function() {}\n")?;

    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::services::lsp::LspServerConfig {
            command: FakeLspServer::env_echo_script_path(temp_dir.path())
                .to_string_lossy()
                .to_string(),
            args: vec![],
            enabled: true,
            auto_start: true,
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
            env: std::collections::HashMap::from([(
                "FRESH_TEST_ENV_VAR".to_string(),
                "hello_from_config".to_string(),
            )]),
            language_id_overrides: Default::default(),
            root_markers: Default::default(),
        },
    );

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    harness.open_file(&test_file)?;
    harness.render()?;

    // Move mouse over the symbol "example_function" to trigger hover state
    harness.mouse_move(10, 2)?;
    harness.render()?;

    // Force check mouse hover to bypass the 500ms timer and send the request
    harness.editor_mut().force_check_mouse_hover();

    // Wait for hover popup to appear (LSP response received)
    harness.wait_until(|h| h.editor().active_state().popups.is_visible())?;

    harness.render()?;
    let screen = harness.screen_to_string();

    // The hover response from our env-echo fake LSP includes the value of
    // FRESH_TEST_ENV_VAR. If it was passed correctly, we see "hello_from_config".
    assert!(
        screen.contains("hello_from_config"),
        "Hover popup should contain the env var value 'hello_from_config'. Screen:\n{}",
        screen
    );

    Ok(())
}
