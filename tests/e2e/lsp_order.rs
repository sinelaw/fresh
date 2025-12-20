//! E2E tests for LSP message ordering
//!
//! These tests verify that LSP messages are sent in the correct order,
//! particularly that didOpen is sent before any requests for a file.

use crate::common::fake_lsp::FakeLspServer;
use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Test that didOpen is sent before hover request
///
/// This test verifies that when opening a file and triggering hover,
/// the LSP client sends textDocument/didOpen before textDocument/hover.
#[test]
fn test_did_open_sent_before_hover() -> std::io::Result<()> {
    // Spawn fake LSP server with logging
    let _fake_server = FakeLspServer::spawn_with_logging()?;

    // Create unique log file for this test
    let log_file = std::env::temp_dir().join("lsp_order_test_log.txt");

    // Create temp dir and test file
    let temp_dir = tempfile::tempdir()?;
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn main() {\n    let x = 5;\n}\n")?;

    // Configure editor to use the logging fake LSP server
    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::services::lsp::client::LspServerConfig {
            command: FakeLspServer::logging_script_path()
                .to_string_lossy()
                .to_string(),
            args: vec![log_file.to_string_lossy().to_string()],
            enabled: true,
            auto_start: false,
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
        },
    );

    // Create harness with config
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    // Open the test file (this should trigger didOpen)
    harness.open_file(&test_file)?;
    harness.render()?;

    // Wait for LSP to initialize
    for _ in 0..10 {
        harness.process_async_and_render()?;
        harness.sleep(std::time::Duration::from_millis(50));
    }

    // Trigger hover with Alt+K (default keybinding for lsp_hover)
    harness.send_key(KeyCode::Char('k'), KeyModifiers::ALT)?;
    harness.render()?;

    // Process async messages to let hover request go through
    for _ in 0..20 {
        harness.process_async_and_render()?;
        harness.sleep(std::time::Duration::from_millis(50));
    }

    // Read the log file and verify order
    let log_content = std::fs::read_to_string(&log_file).unwrap_or_default();
    let methods: Vec<&str> = log_content.lines().collect();

    println!("LSP methods received: {:?}", methods);

    // Find indices of didOpen and hover
    let did_open_index = methods.iter().position(|m| *m == "textDocument/didOpen");
    let hover_index = methods.iter().position(|m| *m == "textDocument/hover");

    // Verify didOpen was received
    assert!(
        did_open_index.is_some(),
        "Expected textDocument/didOpen to be sent, but it was not found in log. Methods: {:?}",
        methods
    );

    // Verify hover was received
    assert!(
        hover_index.is_some(),
        "Expected textDocument/hover to be sent, but it was not found in log. Methods: {:?}",
        methods
    );

    // Verify didOpen came before hover
    let did_open_idx = did_open_index.unwrap();
    let hover_idx = hover_index.unwrap();
    assert!(
        did_open_idx < hover_idx,
        "Expected textDocument/didOpen (index {}) to come before textDocument/hover (index {}). Methods: {:?}",
        did_open_idx,
        hover_idx,
        methods
    );

    Ok(())
}
