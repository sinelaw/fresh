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
    // Initialize tracing for debugging
    let _ = tracing_subscriber::fmt()
        .with_env_filter("fresh=debug")
        .try_init();

    eprintln!("[TEST] Starting test_did_open_sent_before_hover");

    // Spawn fake LSP server with logging
    eprintln!("[TEST] Spawning fake LSP server");
    let _fake_server = FakeLspServer::spawn_with_logging()?;
    eprintln!("[TEST] Fake LSP server spawned");

    // Create unique log file for this test
    let log_file = std::env::temp_dir().join("lsp_order_test_log.txt");
    eprintln!("[TEST] LSP log file: {:?}", log_file);

    // Create temp dir and test file
    let temp_dir = tempfile::tempdir()?;
    let test_file = temp_dir.path().join("test.rs");
    eprintln!("[TEST] Creating test file: {:?}", test_file);
    std::fs::write(&test_file, "fn main() {\n    let x = 5;\n}\n")?;

    // Configure editor to use the logging fake LSP server
    eprintln!("[TEST] Configuring LSP server");
    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::services::lsp::LspServerConfig {
            command: FakeLspServer::logging_script_path()
                .to_string_lossy()
                .to_string(),
            args: vec![log_file.to_string_lossy().to_string()],
            enabled: true,
            auto_start: true,
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
        },
    );

    // Create harness with config
    eprintln!("[TEST] Creating editor harness");
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;
    eprintln!("[TEST] Editor harness created");

    // Open the test file (this should trigger didOpen)
    eprintln!("[TEST] Opening test file: {:?}", test_file);
    harness.open_file(&test_file)?;
    harness.render()?;
    eprintln!("[TEST] File opened, waiting for didOpen message");

    // Wait for LSP to initialize and didOpen to be logged
    let mut did_open_wait_count = 0;
    loop {
        harness.process_async_and_render()?;
        harness.sleep(std::time::Duration::from_millis(50));
        did_open_wait_count += 1;

        let log_content = std::fs::read_to_string(&log_file).unwrap_or_default();
        if did_open_wait_count % 20 == 0 {
            eprintln!(
                "[TEST] Still waiting for didOpen ({}s). Log content:\n{}",
                did_open_wait_count * 50 / 1000,
                if log_content.is_empty() {
                    "<empty>".to_string()
                } else {
                    log_content.clone()
                }
            );
        }

        if log_content.contains("textDocument/didOpen") {
            eprintln!("[TEST] didOpen message received!");
            break;
        }
    }

    // Trigger hover with Alt+K (default keybinding for lsp_hover)
    eprintln!("[TEST] Triggering hover with Alt+K");
    harness.send_key(KeyCode::Char('k'), KeyModifiers::ALT)?;
    harness.render()?;
    eprintln!("[TEST] Hover triggered, waiting for hover message");

    // Wait for hover request to be logged
    let mut hover_wait_count = 0;
    loop {
        harness.process_async_and_render()?;
        harness.sleep(std::time::Duration::from_millis(50));
        hover_wait_count += 1;

        let log_content = std::fs::read_to_string(&log_file).unwrap_or_default();
        if hover_wait_count % 20 == 0 {
            eprintln!(
                "[TEST] Still waiting for hover ({}s). Log content:\n{}",
                hover_wait_count * 50 / 1000,
                log_content
            );
        }

        if log_content.contains("textDocument/hover") {
            eprintln!("[TEST] Hover message received!");
            break;
        }
    }

    // Read the log file and verify order
    eprintln!("[TEST] Verifying message order");
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
    eprintln!(
        "[TEST] didOpen at index {}, hover at index {}",
        did_open_idx, hover_idx
    );
    assert!(
        did_open_idx < hover_idx,
        "Expected textDocument/didOpen (index {}) to come before textDocument/hover (index {}). Methods: {:?}",
        did_open_idx,
        hover_idx,
        methods
    );

    eprintln!("[TEST] Test completed successfully");
    Ok(())
}
