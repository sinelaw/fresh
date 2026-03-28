//! E2E tests reproducing diagnostics panel usability bugs.
//!
//! These tests reproduce the following issues discovered during manual usability testing:
//!
//! 1. **isOpen state desync**: Pressing Escape closes the panel visually but the plugin's
//!    `isOpen` flag remains `true`, so "Show Diagnostics Panel" silently fails to reopen.
//!
//! 2. **Jump-to-location wrong line**: Pressing Enter on a diagnostic jumps to the wrong
//!    line — the panel's internal line number is used instead of the diagnostic's source
//!    line number.

use crate::common::fake_lsp::FakeLspServer;
use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

/// Set up a standard test environment with fake LSP diagnostics.
///
/// Uses `spawn_many_diagnostics(dir, 3)` which produces 3 diagnostics:
///   - Error 0 at line 0, chars 0-5
///   - Error 1 at line 0, chars 10-15
///   - Error 2 at line 1, chars 0-5
fn setup_harness(temp_dir: &tempfile::TempDir) -> (EditorTestHarness, std::path::PathBuf) {
    let project_root = temp_dir.path().to_path_buf();

    // Set up plugins
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "diagnostics_panel");
    copy_plugin_lib(&plugins_dir);

    // Create test file with 30 lines
    let mut content = String::new();
    for i in 0..30 {
        content.push_str(&format!("line {:02} content here\n", i));
    }
    let test_file = project_root.join("test.rs");
    fs::write(&test_file, &content).unwrap();

    // Configure LSP to use the fake server
    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: FakeLspServer::many_diagnostics_script_path(temp_dir.path())
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

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, config, project_root).unwrap();

    harness.open_file(&test_file).unwrap();
    harness.render().unwrap();

    // Wait for diagnostics to arrive (overlays created by LSP)
    harness
        .wait_until(|h| {
            let overlays = h.editor().active_state().overlays.all();
            let diagnostic_ns = fresh::services::lsp::diagnostics::lsp_diagnostic_namespace();
            overlays
                .iter()
                .any(|o| o.namespace.as_ref() == Some(&diagnostic_ns))
        })
        .unwrap();

    (harness, test_file)
}

/// Helper: open the diagnostics panel via command palette.
fn open_diagnostics_panel(harness: &mut EditorTestHarness) {
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

    // Wait for panel to appear with diagnostics visible
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Diagnostics (") && screen.contains("[E]")
        })
        .unwrap();
}

// ─── Bug 1: isOpen state desyncs when closing with Escape ───────────────────

/// After closing the diagnostics panel with Escape, "Show Diagnostics Panel"
/// should reopen it. Currently, Escape closes the visual panel but the plugin's
/// `isOpen` flag stays `true`, so the show command silently does nothing.
///
/// This test will hang (timeout in CI) if the bug is present, because the
/// second `open_diagnostics_panel` call waits for the panel to appear but it
/// never does.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_diagnostics_panel_reopen_after_escape() {
    init_tracing_from_env();

    let temp_dir = tempfile::TempDir::new().unwrap();
    let _fake_server = FakeLspServer::spawn_many_diagnostics(temp_dir.path(), 3).unwrap();
    let (mut harness, _test_file) = setup_harness(&temp_dir);

    // Open the diagnostics panel
    open_diagnostics_panel(&mut harness);

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Diagnostics ("),
        "Panel should be open.\nScreen:\n{}",
        screen
    );

    // Close the panel with Escape
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Wait for panel to disappear (the split should close)
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            !screen.contains("Enter:select | Esc:close")
        })
        .unwrap();

    let screen_after_close = harness.screen_to_string();
    eprintln!(
        "[TEST] Screen after Escape close:\n{}",
        screen_after_close
    );

    // Now try to reopen with "Show Diagnostics Panel"
    // BUG: This hangs because the plugin thinks the panel is still open (isOpen=true)
    // and just calls provider.notify() on a dead panel instead of reopening.
    open_diagnostics_panel(&mut harness);

    let screen_after_reopen = harness.screen_to_string();
    eprintln!(
        "[TEST] Screen after reopen attempt:\n{}",
        screen_after_reopen
    );

    // The panel should be visible again
    assert!(
        screen_after_reopen.contains("Diagnostics ("),
        "After closing with Escape and reopening with 'Show Diagnostics Panel', \
         the panel should be visible again. The isOpen state is desynced: \
         Escape closes the visual panel but the plugin still thinks it's open.\n\
         Screen:\n{}",
        screen_after_reopen
    );
}

// ─── Bug 2: Jump-to-location navigates to wrong line ────────────────────────

/// Pressing Enter on the first diagnostic should jump to line 1 (display) in the
/// source file (LSP line 0). The test moves the cursor far from line 1 first, then
/// opens the panel and presses Enter. If the jump works correctly, the cursor should
/// be back near the top of the file.
///
/// Uses the existing `test_diagnostics_panel_enter_does_not_jump` approach but with
/// a clearer assertion on the cursor's final position.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_diagnostics_panel_jump_to_correct_line() {
    init_tracing_from_env();

    let temp_dir = tempfile::TempDir::new().unwrap();
    // spawn_many_diagnostics(dir, 3) creates diagnostics at lines 0 and 1
    let _fake_server = FakeLspServer::spawn_many_diagnostics(temp_dir.path(), 3).unwrap();
    let (mut harness, _test_file) = setup_harness(&temp_dir);

    // Move cursor far away from the diagnostic lines (line 0-1) to line 20
    for _ in 0..20 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    // Verify cursor moved away
    harness.assert_screen_contains("Ln 21");

    // Open diagnostics panel
    open_diagnostics_panel(&mut harness);

    let panel_screen = harness.screen_to_string();
    eprintln!("[TEST] Panel screen:\n{}", panel_screen);

    // Navigate to the first diagnostic item
    // Panel layout: line 1=title, line 2=blank, line 3=file header, line 4=first [E] item
    for _ in 0..10 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
        let screen = harness.screen_to_string();
        if screen.contains("Item 1/") {
            break;
        }
    }

    let on_item = harness.screen_to_string();
    eprintln!("[TEST] On item 1:\n{}", on_item);
    assert!(
        on_item.contains("Item 1/"),
        "Should be on a diagnostic item.\nScreen:\n{}",
        on_item
    );

    // Press Enter to jump to the diagnostic location (line 0 = display Ln 1)
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.render().unwrap();

    let after_jump = harness.screen_to_string();
    eprintln!("[TEST] After Enter jump:\n{}", after_jump);

    // The "Jumped to" message should appear
    assert!(
        after_jump.contains("Jumped to"),
        "Expected 'Jumped to' status message.\nScreen:\n{}",
        after_jump
    );

    // After jumping, the viewport should show the file's first lines
    // (since the diagnostic is at line 0, "line 00 content here" should be visible)
    assert!(
        after_jump.contains("line 00 content here"),
        "After jumping to a diagnostic at line 0, the viewport should show \
         'line 00 content here'. The jump may have gone to the wrong line \
         (using the panel's internal cursor line instead of the diagnostic's \
         source line).\nScreen:\n{}",
        after_jump
    );

    // The status bar should show Ln 1 (1-indexed display for LSP line 0)
    assert!(
        after_jump.contains("Ln 1,"),
        "After jumping to LSP line 0, the status bar should show 'Ln 1'. \
         The cursor jumped to the wrong line.\nScreen:\n{}",
        after_jump
    );
}
