//! E2E tests for LSP message ordering
//!
//! These tests verify that LSP messages are sent in the correct order,
//! particularly that didOpen is sent before any requests for a file.

use crate::common::fake_lsp::FakeLspServer;
use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// A config whose `rust` server is the logging fake from
/// [`FakeLspServer::spawn_with_logging`], writing each method it receives to
/// `log_file`, one per line.
fn logging_lsp_config(dir: &std::path::Path, log_file: &std::path::Path) -> fresh::config::Config {
    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: FakeLspServer::logging_script_path(dir)
                .to_string_lossy()
                .to_string(),
            args: Some(vec![log_file.to_string_lossy().to_string()]),
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
    config
}

/// Test that didOpen is sent before hover request
///
/// This test verifies that when opening a file and triggering hover,
/// the LSP client sends textDocument/didOpen before textDocument/hover.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses Bash-based fake LSP server
fn test_did_open_sent_before_hover() -> anyhow::Result<()> {
    // Initialize tracing for debugging
    let _ = tracing_subscriber::fmt()
        .with_env_filter("fresh=debug")
        .try_init();

    eprintln!("[TEST] Starting test_did_open_sent_before_hover");

    // Create temp dir and test file
    let temp_dir = tempfile::tempdir()?;

    // Spawn fake LSP server with logging
    eprintln!("[TEST] Spawning fake LSP server");
    let _fake_server = FakeLspServer::spawn_with_logging(temp_dir.path())?;
    eprintln!("[TEST] Fake LSP server spawned");

    // Create unique log file for this test in the per-test temp directory
    let log_file = temp_dir.path().join("lsp_order_test_log.txt");
    eprintln!("[TEST] LSP log file: {:?}", log_file);
    let test_file = temp_dir.path().join("test.rs");
    eprintln!("[TEST] Creating test file: {:?}", test_file);
    std::fs::write(&test_file, "fn main() {\n    let x = 5;\n}\n")?;

    // Configure editor to use the logging fake LSP server
    eprintln!("[TEST] Configuring LSP server");
    let config = logging_lsp_config(temp_dir.path(), &log_file);

    // Create harness with empty plugins dir to prevent loading embedded
    // plugins (unnecessary for this test and improves isolation/speed).
    // Embedded plugins may send additional LSP requests that the fake
    // bash script doesn't handle, causing pending request buildup.
    eprintln!("[TEST] Creating editor harness");
    let mut harness = EditorTestHarness::create(
        120,
        30,
        crate::common::harness::HarnessOptions::new()
            .with_config(config)
            .with_working_dir(temp_dir.path().to_path_buf()),
    )?;
    eprintln!("[TEST] Editor harness created");

    // Open the test file (this should trigger didOpen)
    eprintln!("[TEST] Opening test file: {:?}", test_file);
    harness.open_file(&test_file)?;
    harness.render()?;
    eprintln!("[TEST] File opened, waiting for didOpen message");

    // Wait for LSP to initialize and didOpen to be logged
    eprintln!("[TEST] Waiting for didOpen message");
    harness.wait_until(|_| {
        let log_content = std::fs::read_to_string(&log_file).unwrap_or_default();
        log_content.contains("textDocument/didOpen")
    })?;
    eprintln!("[TEST] didOpen message received!");

    // Trigger hover with Alt+K (default keybinding for lsp_hover)
    eprintln!("[TEST] Triggering hover with Alt+K");
    harness.send_key(KeyCode::Char('k'), KeyModifiers::ALT)?;
    harness.render()?;
    eprintln!("[TEST] Hover triggered, waiting for hover message");

    // Wait for hover request to be logged
    eprintln!("[TEST] Waiting for hover message");
    harness.wait_until(|_| {
        let log_content = std::fs::read_to_string(&log_file).unwrap_or_default();
        log_content.contains("textDocument/hover")
    })?;
    eprintln!("[TEST] Hover message received!");

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

/// **A hover asked for while the server is still starting is sent once it
/// has started**, rather than dropped. Until the editor has handled the
/// server's `initialize` answer it routes no requests to it; Alt+K pressed
/// in that window used to find no server and do nothing, so a user who
/// asked for hover right after opening a file got no answer, and
/// `test_did_open_sent_before_hover` hung whenever the editor's main loop
/// fell behind the server's log. Alt+K here is pressed straight after the
/// open, before the editor can have heard back from the (bash) server.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses Bash-based fake LSP server
fn test_hover_pressed_before_server_initialized_is_sent() -> anyhow::Result<()> {
    let temp_dir = tempfile::tempdir()?;
    let _fake_server = FakeLspServer::spawn_with_logging(temp_dir.path())?;
    let log_file = temp_dir.path().join("lsp_order_test_log.txt");
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn main() {\n    let x = 5;\n}\n")?;

    let mut harness = EditorTestHarness::create(
        120,
        30,
        crate::common::harness::HarnessOptions::new()
            .with_config(logging_lsp_config(temp_dir.path(), &log_file))
            .with_working_dir(temp_dir.path().to_path_buf()),
    )?;
    harness.open_file(&test_file)?;
    harness.send_key(KeyCode::Char('k'), KeyModifiers::ALT)?;

    harness.wait_until(|_| {
        std::fs::read_to_string(&log_file)
            .unwrap_or_default()
            .contains("textDocument/hover")
    })?;
    let log_content = std::fs::read_to_string(&log_file)?;
    let methods: Vec<&str> = log_content.lines().collect();
    let did_open = methods.iter().position(|m| *m == "textDocument/didOpen");
    let hover = methods.iter().position(|m| *m == "textDocument/hover");
    assert!(
        matches!((did_open, hover), (Some(d), Some(h)) if d < h),
        "didOpen must precede the replayed hover. Methods: {:?}",
        methods
    );
    Ok(())
}
