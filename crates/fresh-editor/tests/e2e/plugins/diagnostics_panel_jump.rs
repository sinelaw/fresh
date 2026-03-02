//! E2E tests for diagnostics panel navigation.
//!
//! Tests that:
//! 1. Pressing Enter on a diagnostic entry jumps to the location and focuses the editor
//! 2. Moving up/down in the panel scrolls the editor to show the diagnostic location
//!    while keeping focus in the panel

use crate::common::fake_lsp::FakeLspServer;
use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

/// Reproduce: diagnostics panel "Jumped to" message appears but cursor
/// does not actually move to the diagnostic location.
///
/// Setup:
/// 1. Open a file, move cursor to line 10
/// 2. Fake LSP sends diagnostics at lines 0-2
/// 3. Open diagnostics panel, press Enter on line 1 diagnostic
/// 4. Expect cursor to jump to line 1 — but it stays on line 10 (the bug)
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses Bash-based fake LSP server
fn test_diagnostics_panel_enter_does_not_jump() {
    init_tracing_from_env();

    // Create fake LSP that sends diagnostics on didOpen/didChange
    // This server sends diagnostics at lines 0, 0, 1 (2 per line, 3 total)
    let _fake_server = FakeLspServer::spawn_many_diagnostics(3).unwrap();

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    // Set up plugins
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "diagnostics_panel");
    copy_plugin_lib(&plugins_dir);

    // Create a test file with enough lines
    let mut content = String::new();
    for i in 0..20 {
        content.push_str(&format!("line {} content here\n", i));
    }
    let test_file = project_root.join("test.rs");
    fs::write(&test_file, &content).unwrap();

    // Configure fake LSP
    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::services::lsp::LspServerConfig {
            command: FakeLspServer::many_diagnostics_script_path()
                .to_string_lossy()
                .to_string(),
            args: vec![],
            enabled: true,
            auto_start: true,
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
            env: Default::default(),
        },
    );

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 30, config, project_root).unwrap();

    // Open the test file
    harness.open_file(&test_file).unwrap();
    harness.render().unwrap();

    // Wait for diagnostics to arrive
    harness
        .wait_until(|h| {
            let overlays = h.editor().active_state().overlays.all();
            let diagnostic_ns = fresh::services::lsp::diagnostics::lsp_diagnostic_namespace();
            overlays
                .iter()
                .any(|o| o.namespace.as_ref() == Some(&diagnostic_ns))
        })
        .unwrap();

    // Move cursor down to line 10 so we can detect if jump works
    for _ in 0..10 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    // Verify cursor moved away from line 1
    let (_, cursor_y_before) = harness.screen_cursor_position();
    eprintln!("[TEST] Cursor Y before panel: {}", cursor_y_before);

    // Open command palette and run "Show Diagnostics Panel"
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Show Diagnostics Panel").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for diagnostics panel to open
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("*Diagnostics*") || screen.contains("Diagnostics (")
        })
        .unwrap();

    let panel_screen = harness.screen_to_string();
    eprintln!("[TEST] Panel screen:\n{}", panel_screen);

    // Verify diagnostics are visible in the panel
    assert!(
        panel_screen.contains("[E]"),
        "Expected error diagnostics in the panel.\nScreen:\n{}",
        panel_screen
    );

    // Press Enter on the first diagnostic entry (line 1) to "jump" to it
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.render().unwrap();

    let after_screen = harness.screen_to_string();
    eprintln!("[TEST] Screen after Enter:\n{}", after_screen);

    // The status bar shows "Jumped to ..." — confirming the handler ran
    assert!(
        after_screen.contains("Jumped to"),
        "Expected 'Jumped to' status message after pressing Enter.\nScreen:\n{}",
        after_screen
    );

    // Close the diagnostics panel to return focus to the editor
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Wait for focus to return to the editor (panel tab may still be visible,
    // but the editor buffer should be the active split)
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            // Status bar should show file line info, not panel info
            screen.contains("Ln ")
        })
        .unwrap();

    let final_screen = harness.screen_to_string();
    eprintln!("[TEST] Screen after closing panel:\n{}", final_screen);

    // After pressing Enter on a diagnostic at line 1, the editor should
    // navigate to line 1 of the file. The viewport should show "line 0 content
    // here" near the top (the file's first line).
    assert!(
        final_screen.contains("line 0 content here"),
        "After pressing Enter on a diagnostic at line 1, the editor should \
         have navigated to line 1 (showing 'line 0 content here' in the viewport), \
         but the viewport did not change.\nScreen:\n{}",
        final_screen
    );
}

/// Test that moving the cursor up/down in the diagnostics panel scrolls
/// the editor viewport to show the diagnostic location, while keeping
/// focus in the diagnostics panel.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_diagnostics_panel_cursor_move_scrolls_editor() {
    init_tracing_from_env();

    let _fake_server = FakeLspServer::spawn_many_diagnostics(3).unwrap();

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "diagnostics_panel");
    copy_plugin_lib(&plugins_dir);

    // Create a file with 50 lines so diagnostics at line 1 vs line 2 produce
    // visibly different scroll positions
    let mut content = String::new();
    for i in 0..50 {
        content.push_str(&format!("line {} content here\n", i));
    }
    let test_file = project_root.join("test.rs");
    fs::write(&test_file, &content).unwrap();

    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::services::lsp::LspServerConfig {
            command: FakeLspServer::many_diagnostics_script_path()
                .to_string_lossy()
                .to_string(),
            args: vec![],
            enabled: true,
            auto_start: true,
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
            env: Default::default(),
        },
    );

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 30, config, project_root).unwrap();

    harness.open_file(&test_file).unwrap();
    harness.render().unwrap();

    // Wait for diagnostics
    harness
        .wait_until(|h| {
            let overlays = h.editor().active_state().overlays.all();
            let diagnostic_ns = fresh::services::lsp::diagnostics::lsp_diagnostic_namespace();
            overlays
                .iter()
                .any(|o| o.namespace.as_ref() == Some(&diagnostic_ns))
        })
        .unwrap();

    // Move cursor far away from diagnostic lines
    for _ in 0..30 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    // Open diagnostics panel
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Show Diagnostics Panel").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("[E]")
        })
        .unwrap();

    // The panel cursor starts on the header/title line. Move down to the
    // first diagnostic entry so the cursor is on an item with a location.
    // Panel layout: line 1=title, line 2=blank, line 3=filename, line 4=first item
    for _ in 0..3 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }

    // Wait for the status bar to show item info, confirming cursor is on an item
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Item 1/")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    eprintln!("[TEST] Screen after cursor move to item:\n{}", screen);

    // The editor viewport (top split) should have scrolled to show the
    // diagnostic location (line 1 of the file → "line 0 content here")
    assert!(
        screen.contains("line 0 content here"),
        "Editor viewport should have scrolled to show the diagnostic location \
         when cursor moved to a diagnostic entry in the panel.\nScreen:\n{}",
        screen
    );

    // Verify focus is still in the diagnostics panel (not the editor)
    assert!(
        screen.contains("Item 1/"),
        "Focus should still be in the diagnostics panel.\nScreen:\n{}",
        screen
    );
}
