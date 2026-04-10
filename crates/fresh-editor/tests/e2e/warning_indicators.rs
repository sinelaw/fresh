//! E2E tests for warning indicator functionality
//!
//! Tests the warning domain system including:
//! - ShowWarnings command
//! - ShowLspStatus command
//! - ClearWarnings command
//! - Status bar warning badge display

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use std::io::Write;

/// Test that ShowWarnings command appears in command palette
#[test]
fn test_show_warnings_command_exists() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type to filter for warnings command
    harness.type_text("show warn").unwrap();
    harness.render().unwrap();

    // Should show the Show Warnings command
    harness.assert_screen_contains("Show Warnings");
}

/// Test that ShowLspStatus command appears in command palette
#[test]
fn test_show_lsp_status_command_exists() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type to filter for LSP status command
    harness.type_text("lsp status").unwrap();
    harness.render().unwrap();

    // Should show the Show LSP Status command
    harness.assert_screen_contains("Show LSP Status");
}

/// Test that ClearWarnings command appears in command palette
#[test]
fn test_clear_warnings_command_exists() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type to filter for clear warnings command
    harness.type_text("clear warn").unwrap();
    harness.render().unwrap();

    // Should show the Clear Warnings command
    harness.assert_screen_contains("Clear Warnings");
}

/// Test ShowWarnings command execution when no warnings exist
#[test]
fn test_show_warnings_no_warnings() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Execute Show Warnings command
    harness.type_text("Show Warnings").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show "No warnings" status message
    harness.assert_screen_contains("No warnings");
}

/// Test ShowLspStatus command execution when no LSP active
#[test]
fn test_show_lsp_status_no_lsp() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Execute Show LSP Status command
    harness.type_text("Show LSP Status").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show "No LSP server active" status message
    harness.assert_screen_contains("No LSP server active");
}

/// Test that LSP indicator shows "LSP off" when configured but not started,
/// and that the popup shows the server with a "Start" action.
#[test]
#[cfg_attr(
    target_os = "windows",
    ignore = "FakeLspServer uses a Bash script which is not available on Windows"
)]
fn test_lsp_indicator_off_shows_start_action() -> anyhow::Result<()> {
    use crate::common::fake_lsp::FakeLspServer;

    let temp_dir = tempfile::tempdir()?;
    let _fake_server = FakeLspServer::spawn(temp_dir.path())?;

    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn main() {}\n")?;

    // Configure LSP but with auto_start=false so it doesn't start automatically
    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: FakeLspServer::script_path(temp_dir.path())
                .to_string_lossy()
                .to_string(),
            args: vec![],
            enabled: true,
            auto_start: false,
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
        100,
        24,
        crate::common::harness::HarnessOptions::new()
            .with_config(config)
            .with_working_dir(temp_dir.path().to_path_buf()),
    )?;

    harness.open_file(&test_file)?;
    harness.render()?;

    // Status bar should show "LSP off" since LSP is configured but not running
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("LSP off"),
        "Status bar should show 'LSP off' when configured but not started. Screen:\n{}",
        screen
    );

    // Open the LSP status popup via command palette
    harness.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)?;
    harness.type_text("Show LSP Status")?;
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.render()?;

    let screen = harness.screen_to_string();

    // Popup should show the server as "not running" with a Start action
    assert!(
        screen.contains("LSP Servers"),
        "Popup should have title. Screen:\n{}",
        screen
    );
    assert!(
        screen.contains("not running"),
        "Popup should show server as not running. Screen:\n{}",
        screen
    );
    assert!(
        screen.contains("Start"),
        "Popup should offer Start action. Screen:\n{}",
        screen
    );

    // Dismiss and verify cleanup
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE)?;
    harness.render()?;
    harness.assert_screen_not_contains("LSP Servers");

    Ok(())
}

/// Test that LSP indicator shows simplified "LSP" when running, and that
/// the popup shows server details with Restart/Stop/View Log actions.
#[test]
#[cfg_attr(
    target_os = "windows",
    ignore = "FakeLspServer uses a Bash script which is not available on Windows"
)]
fn test_lsp_indicator_on_shows_server_actions() -> anyhow::Result<()> {
    use crate::common::fake_lsp::FakeLspServer;

    let temp_dir = tempfile::tempdir()?;
    let _fake_server = FakeLspServer::spawn(temp_dir.path())?;

    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn main() {}\n")?;

    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: FakeLspServer::script_path(temp_dir.path())
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
        100,
        24,
        crate::common::harness::HarnessOptions::new()
            .with_config(config)
            .with_working_dir(temp_dir.path().to_path_buf()),
    )?;

    harness.open_file(&test_file)?;
    harness.render()?;

    // Wait for LSP to be ready (fake server sends status notification)
    harness.wait_for_screen_contains(" LSP ")?;

    let screen = harness.screen_to_string();

    // Should show just "LSP" — not the old "LSP [rust: ready]" format
    assert!(
        !screen.contains("LSP ["),
        "Should NOT show old detailed format. Screen:\n{}",
        screen
    );

    // Open the LSP status popup
    harness.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)?;
    harness.type_text("Show LSP Status")?;
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.render()?;

    let screen = harness.screen_to_string();

    // Popup should show the running server with management actions
    assert!(
        screen.contains("LSP Servers"),
        "Popup should have title. Screen:\n{}",
        screen
    );
    assert!(
        screen.contains("Restart"),
        "Popup should offer Restart. Screen:\n{}",
        screen
    );
    assert!(
        screen.contains("Stop"),
        "Popup should offer Stop. Screen:\n{}",
        screen
    );
    assert!(
        screen.contains("View Log"),
        "Popup should offer View Log. Screen:\n{}",
        screen
    );

    // Dismiss
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE)?;
    harness.render()?;
    harness.assert_screen_not_contains("LSP Servers");

    Ok(())
}

/// Test that status log buffer stays read-only after revert
///
/// Reproduces the bug where opening a log file via the status bar sets
/// editing_disabled, but when the file is updated and reverted,
/// the flag was lost because revert_file() replaces the entire EditorState.
#[test]
fn test_status_log_stays_read_only_after_revert() {
    let mut harness = EditorTestHarness::with_temp_project(100, 24).unwrap();
    let project_dir = harness.project_dir().unwrap();
    let log_path = project_dir.join("status.log");

    // Create initial log file
    {
        let mut f = std::fs::File::create(&log_path).unwrap();
        f.write_all(b"2025-01-01 00:00:00 Initial status\n")
            .unwrap();
        f.sync_all().unwrap();
    }

    // Set the status log path and open it
    harness.editor_mut().set_status_log_path(log_path.clone());
    harness.editor_mut().open_status_log();
    harness.render().unwrap();

    // Verify the buffer is read-only
    assert!(
        harness.editor().is_editing_disabled(),
        "Status log buffer should be read-only immediately after opening"
    );

    // Update the file on disk (simulating new status messages being appended)
    {
        let mut f = std::fs::File::create(&log_path).unwrap();
        f.write_all(b"2025-01-01 00:00:00 Initial status\n2025-01-01 00:00:01 New status\n")
            .unwrap();
        f.sync_all().unwrap();
    }

    // Trigger a revert (this is what auto-revert does when it detects the file changed)
    let reverted = harness.editor_mut().revert_file().unwrap();
    assert!(reverted, "revert_file should succeed");
    harness.render().unwrap();

    // Verify the buffer content was updated
    let content = harness.get_buffer_content().unwrap_or_default();
    assert!(
        content.contains("New status"),
        "Buffer should contain reverted content"
    );

    // The key assertion: editing_disabled must survive the revert
    assert!(
        harness.editor().is_editing_disabled(),
        "Status log buffer should remain read-only after revert"
    );
}

/// Test ClearWarnings command execution
#[test]
fn test_clear_warnings_command() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    // Trigger the command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Execute Clear Warnings command
    harness.type_text("Clear Warnings").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show "Warnings cleared" status message
    harness.assert_screen_contains("Warnings cleared");
}
