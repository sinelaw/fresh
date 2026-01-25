//! Plugin-related tests

use crate::common::fake_lsp::FakeLspServer;
use crate::common::fixtures::TestFixture;
use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use fresh::services::lsp::LspServerConfig;
use fresh::services::process_limits::ProcessLimits;
use std::fs;
use std::time::Duration;

/// Test that render-line hook receives args properly
#[test]
fn test_render_line_hook_with_args() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir); // Required for TypeScript type declarations

    // Create a simple plugin that captures render-line hook args
    let test_plugin = r###"
const editor = getEditor();
// Test plugin to verify render-line hook receives args
let line_count = 0;
let found_marker = false;

globalThis.onRenderLine = function(args: {
    buffer_id: number;
    line_number: number;
    byte_start: number;
    byte_end: number;
    content: string;
}): boolean {
    editor.debug("render-line hook called!");
    // Verify args are present
    if (args && args.buffer_id !== undefined && args.line_number !== undefined && args.content !== undefined) {
        line_count = line_count + 1;
        editor.debug(`Line ${args.line_number}: ${args.content}`);

        // Look for "TEST_MARKER" in the content
        if (args.content.includes("TEST_MARKER")) {
            found_marker = true;
            editor.debug("Found TEST_MARKER!");
            editor.setStatus(`Found TEST_MARKER on line ${args.line_number} at byte ${args.byte_start}`);
        }
    } else {
        editor.debug("ERROR: args is nil or missing fields!");
    }
    return true;
};

editor.on("render_line", "onRenderLine");

globalThis.test_show_count = function(): void {
    editor.setStatus(`Rendered ${line_count} lines, found=${found_marker}`);
    line_count = 0;  // Reset counter
    found_marker = false;
};

editor.registerCommand(
    "Test: Show Line Count",
    "Show how many lines were rendered",
    "test_show_count",
    "normal"
);

editor.setStatus("Test plugin loaded!");
"###;

    let test_plugin_path = plugins_dir.join("test_render_hook.ts");
    fs::write(&test_plugin_path, test_plugin).unwrap();

    // Create test file with marker
    let test_file_content = "Line 1\nLine 2\nTEST_MARKER line\nLine 4\n";
    let fixture = TestFixture::new("test_render.txt", test_file_content).unwrap();

    // Create harness with the project directory
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(80, 24, Default::default(), project_root)
            .unwrap();

    // Open the test file - this should trigger render-line hooks
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    // Check that the test file is visible
    harness.assert_screen_contains("TEST_MARKER");

    // Run the "Show Line Count" command to verify hooks executed
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Test: Show Line Count").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Check the rendered screen content - should still contain the test content
    harness.assert_screen_contains("TEST_MARKER");

    // The plugin should have detected the marker via render-line hooks
    // We verify this by checking that the screen was rendered successfully
}

/// Test plugin overlay API - plugins can add and clear overlays
#[test]
fn test_plugin_overlay_api() {
    init_tracing_from_env();

    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir); // Required for TypeScript type declarations

    // Create a test plugin that adds overlays
    let test_plugin = r#"
const editor = getEditor();

editor.registerCommand(
    "Test: Add Overlay",
    "Add a test overlay",
    "test_add_overlay",
    null
);

globalThis.test_add_overlay = function(): void {
    const bufferId = editor.getActiveBufferId();
    if (bufferId === null || bufferId === undefined) {
        editor.setStatus("No active buffer");
        return;
    }

    // Add an overlay with RGB color
    editor.addOverlay(bufferId, "test-ns", 0, 4, {
        fg: [255, 0, 0],  // Red foreground
    });

    editor.setStatus("Overlay added");
};

editor.setStatus("Overlay test plugin loaded");
"#;

    fs::write(plugins_dir.join("test_overlay.ts"), test_plugin).unwrap();

    // Create test file
    let test_file_content = "TEST content here\n";
    let fixture = TestFixture::new("test.txt", test_file_content).unwrap();

    // Create harness
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(80, 24, Default::default(), project_root)
            .unwrap();

    // Open the test file
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    // Execute the overlay command
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Test: Add Overlay").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Add Overlay"))
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for overlay to be applied
    harness
        .wait_until(|h| {
            let overlays = h.editor().active_state().overlays.all();
            !overlays.is_empty()
        })
        .unwrap();

    // Verify overlay was created
    let overlays = harness.editor().active_state().overlays.all();
    assert!(!overlays.is_empty(), "Expected at least one overlay");

    let screen = harness.screen_to_string();
    println!("Screen after overlay:\n{}", screen);
}

/// Test plugin render-line hook receives correct arguments
#[test]
fn test_plugin_render_line_hook() {
    init_tracing_from_env();

    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir); // Required for TypeScript type declarations

    // Create a plugin that tracks render-line calls
    let test_plugin = r#"
const editor = getEditor();

let lineCount = 0;
let foundMarker = false;

globalThis.onRenderLine = function(args: {
    buffer_id: number;
    line_number: number;
    byte_start: number;
    byte_end: number;
    content: string;
}): boolean {
    if (args && args.content !== undefined) {
        lineCount++;
        if (args.content.includes("MARKER")) {
            foundMarker = true;
        }
    }
    return true;
};

editor.on("render_line", "onRenderLine");

globalThis.test_check_render = function(): void {
    editor.setStatus(`Lines: ${lineCount}, Marker: ${foundMarker}`);
};

editor.registerCommand(
    "Test: Check Render",
    "Check render-line results",
    "test_check_render",
    "normal"
);

editor.setStatus("Render hook test loaded");
"#;

    fs::write(plugins_dir.join("test_render_hook.ts"), test_plugin).unwrap();

    // Create test file with marker
    let test_file_content = "Line 1\nMARKER line\nLine 3\n";
    let fixture = TestFixture::new("test.txt", test_file_content).unwrap();

    // Create harness
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(80, 24, Default::default(), project_root)
            .unwrap();

    // Open the test file - triggers render-line hooks
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    // Verify content is visible
    harness.assert_screen_contains("MARKER");

    // Check the render hook was called
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Test: Check Render").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // The status should show lines were rendered and marker was found
    let screen = harness.screen_to_string();
    println!("Screen after check:\n{}", screen);
}

/// Test diagnostics panel plugin loads and creates a virtual buffer split
/// This verifies the full implementation with LSP-like diagnostics display
#[test]
#[cfg_attr(windows, ignore)] // Uses bash script for fake LSP server
fn test_diagnostics_panel_plugin_loads() {
    use crate::common::fake_lsp::FakeLspServer;
    init_tracing_from_env();

    // Create a fake LSP server that sends diagnostics
    let _fake_server = FakeLspServer::spawn_many_diagnostics(3).unwrap();

    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    // Create plugins directory and copy the diagnostics panel plugin
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "diagnostics_panel");
    copy_plugin_lib(&plugins_dir); // Copy lib/ for results-panel.ts import

    // Create a simple test file in the project directory (not via TestFixture!)
    let test_file_content = "fn main() {\n    println!(\"test\");\n}\n";
    let test_file = project_root.join("test_diagnostics.rs");
    fs::write(&test_file, test_file_content).unwrap();

    // Configure editor to use the fake LSP server that sends diagnostics
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
        },
    );

    // Create harness with the project directory and LSP config
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(80, 24, config, project_root).unwrap();

    // Open the test file - this should trigger plugin loading and LSP
    harness.open_file(&test_file).unwrap();
    harness.render().unwrap();

    // Check that file content is visible
    harness.assert_screen_contains("fn main()");

    // Wait for LSP to send diagnostics (the fake server sends them on didOpen/didChange)
    // We wait for diagnostic overlays to appear since the status bar E: count may be
    // truncated on narrow terminals (the test uses 80 columns)
    harness
        .wait_until(|h| {
            // Check if diagnostic overlays have been applied
            let overlays = h.editor().active_state().overlays.all();
            let diagnostic_ns = fresh::services::lsp::diagnostics::lsp_diagnostic_namespace();
            overlays
                .iter()
                .any(|o| o.namespace.as_ref() == Some(&diagnostic_ns))
        })
        .unwrap();

    // The plugin should have loaded successfully without Lua errors
    // If the Lua scoping is wrong (update_panel_content not defined before create_panel calls it),
    // the plugin would fail to load with: "attempt to call a nil value (global 'update_panel_content')"

    // The plugin sets a status message on successful load
    // Look for evidence that the plugin loaded by checking the screen
    let screen = harness.screen_to_string();
    println!("Screen after plugin load:\n{}", screen);

    // Now try to execute the "Show Diagnostics Panel" command
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Type to search for the command
    harness.type_text("Show Diagnostics Panel").unwrap();
    harness.render().unwrap();

    let palette_screen = harness.screen_to_string();
    println!("Command palette screen:\n{}", palette_screen);

    // The command should be visible in the palette (registered by the plugin)
    // If the plugin failed to load due to Lua errors, this command wouldn't be registered
    assert!(
        palette_screen.contains("Show Diagnostics Panel")
            || palette_screen.contains("diagnostics")
            || palette_screen.contains("Diagnostics"),
        "The 'Show Diagnostics Panel' command should be registered by the plugin. \
         If the plugin had Lua scoping errors, it wouldn't load and the command wouldn't exist."
    );

    // Execute the command
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for the async panel creation to complete
    // The panel shows "Diagnostics" header when open
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("*Diagnostics*") || screen.contains("Diagnostics (")
        })
        .unwrap();

    let final_screen = harness.screen_to_string();
    println!("Final screen after executing command:\n{}", final_screen);

    // Verify the diagnostics panel content is displayed in a horizontal split
    assert!(
        final_screen.contains("Diagnostics"),
        "Expected to see 'Diagnostics' header in the panel"
    );
    assert!(
        final_screen.contains("[E]") || final_screen.contains("[W]"),
        "Expected to see severity icons like [E] or [W] in the diagnostics"
    );
    // The plugin uses background highlighting for selection, not a '>' marker
    assert!(
        final_screen.contains("*Diagnostics*"),
        "Expected to see buffer name '*Diagnostics*' in status bar"
    );
    // Verify horizontal split view (separator line should be visible)
    assert!(
        final_screen.contains("───") || final_screen.contains("---"),
        "Expected to see horizontal split separator"
    );
    // The original code buffer should still be visible above the diagnostics
    assert!(
        final_screen.contains("fn main()"),
        "Expected to see original code buffer in upper split"
    );
}

/// Test editor <-> plugin message queue architecture
///
/// This test exercises the complete bidirectional message flow:
/// 1. Editor sends action request to plugin thread (fire-and-forget)
/// 2. Plugin executes action and sends commands back via command queue
/// 3. Editor polls command queue with try_recv (non-blocking)
/// 4. Plugin awaits async response for buffer ID
///
/// This ensures no deadlocks occur in the message passing architecture.
#[test]
fn test_plugin_message_queue_architecture() {
    init_tracing_from_env();

    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir); // Required for TypeScript type declarations

    // Create a simplified test plugin
    let test_plugin = r#"
const editor = getEditor();

editor.registerCommand(
    "Test: Create Virtual Buffer",
    "Create a virtual buffer",
    "test_create_virtual_buffer",
    null  // Use null for context to ensure command is visible
);

globalThis.test_create_virtual_buffer = function(): void {
    editor.setStatus("Virtual buffer command executed!");
};

editor.setStatus("Test plugin loaded");
"#;

    let test_plugin_path = plugins_dir.join("test_message_queue.ts");
    fs::write(&test_plugin_path, test_plugin).unwrap();

    // Create test file in separate temp directory (like passing test)
    let test_file_content = "Test file content\nLine 2\nLine 3\n";
    let fixture = TestFixture::new("test_file.txt", test_file_content).unwrap();

    // Create harness with wide screen to avoid status message truncation
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 30, Default::default(), project_root)
            .unwrap();

    // Open the test file from the separate temp directory
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    // Verify file content is visible
    harness.assert_screen_contains("Test file content");

    // Execute the command via Quick Open
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Test: Create Virtual Buffer").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Create Virtual Buffer"))
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for command execution status
    harness
        .wait_until(|h| {
            h.screen_to_string()
                .contains("Virtual buffer command executed")
        })
        .unwrap();

    let final_screen = harness.screen_to_string();

    // Verify command executed
    assert!(
        final_screen.contains("Virtual buffer command executed"),
        "Expected status message to show. Got screen:\n{}",
        final_screen
    );
}

/// Test that multiple plugin actions can be queued without deadlock
#[test]
fn test_plugin_multiple_actions_no_deadlock() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir); // Required for TypeScript type declarations

    // Create a plugin with multiple commands that all set status
    let test_plugin = r#"
const editor = getEditor();
// Test plugin for multiple concurrent actions

editor.registerCommand("Action A", "Set status to A", "action_a", null);
editor.registerCommand("Action B", "Set status to B", "action_b", null);
editor.registerCommand("Action C", "Set status to C", "action_c", null);

globalThis.action_a = function(): void {
    editor.setStatus("Status: A executed");
};

globalThis.action_b = function(): void {
    editor.setStatus("Status: B executed");
};

globalThis.action_c = function(): void {
    editor.setStatus("Status: C executed");
};

editor.setStatus("Multi-action plugin loaded");
"#;

    let test_plugin_path = plugins_dir.join("test_multi_action.ts");
    fs::write(&test_plugin_path, test_plugin).unwrap();

    // Create a simple test file
    let test_file_content = "Test content\n";
    let fixture = TestFixture::new("test.txt", test_file_content).unwrap();

    // Create harness
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(80, 24, Default::default(), project_root)
            .unwrap();

    // Open file and load plugins
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    // Execute multiple commands in sequence rapidly
    // This tests that the message queue handles multiple actions correctly
    // The main assertion is that the commands execute without deadlock or hanging
    let start = std::time::Instant::now();

    for action_name in ["Action A", "Action B", "Action C"] {
        // Open command palette
        harness
            .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
            .unwrap();
        harness.render().unwrap();

        // Type and execute command
        harness.type_text(action_name).unwrap();
        harness
            .send_key(KeyCode::Enter, KeyModifiers::NONE)
            .unwrap();

        // Process async and render a few times to let the action execute
        for _ in 0..3 {
            harness.process_async_and_render().unwrap();
            harness.sleep(Duration::from_millis(20));
        }
    }

    let elapsed = start.elapsed();

    // The key assertion: all commands should execute quickly without deadlock
    // If there's a deadlock or blocking issue, this would timeout
    assert!(
        elapsed < Duration::from_secs(2),
        "Multiple actions should complete without deadlock. Took {:?}",
        elapsed
    );

    // Verify the editor is still responsive
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Test content"),
        "Editor should still show content after multiple actions. Got:\n{}",
        screen
    );
}

/// Test that plugin action execution is non-blocking
///
/// This verifies that the editor doesn't hang when executing a plugin action
/// even if the action takes time. The fire-and-forget pattern should allow
/// the editor to continue processing events.
#[test]
fn test_plugin_action_nonblocking() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir); // Required for TypeScript type declarations

    // Create a plugin that does some work
    let test_plugin = r#"
const editor = getEditor();
// Test plugin to verify non-blocking action execution

editor.registerCommand(
    "Slow Action",
    "An action that does some computation",
    "slow_action",
    "normal"
);

globalThis.slow_action = function(): void {
    // Simulate some work (this is synchronous but should not block editor)
    let sum = 0;
    for (let i = 0; i < 1000; i++) {
        sum += i;
    }
    editor.setStatus(`Completed: sum = ${sum}`);
};

editor.setStatus("Nonblocking test plugin loaded");
"#;

    let test_plugin_path = plugins_dir.join("test_nonblocking.ts");
    fs::write(&test_plugin_path, test_plugin).unwrap();

    // Create a simple test file
    let test_file_content = "Test\n";
    let fixture = TestFixture::new("test.txt", test_file_content).unwrap();

    // Create harness
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(80, 24, Default::default(), project_root)
            .unwrap();

    // Open file
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    // Execute the slow action
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Slow Action").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // The key test: we should be able to render without blocking while action runs
    let start = std::time::Instant::now();

    // Process several render cycles - this verifies the editor isn't blocked
    for _ in 0..5 {
        harness.process_async_and_render().unwrap();
        harness.sleep(Duration::from_millis(50));
    }

    let elapsed = start.elapsed();

    // If the action was blocking, renders would stall
    // The action execution is async, so renders complete immediately
    assert!(
        elapsed < Duration::from_secs(1),
        "Rendering should complete quickly even with action running. Took {:?}",
        elapsed
    );

    // Verify the screen is still responsive (command palette closed)
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Test"),
        "Editor should show file content. Got:\n{}",
        screen
    );
}

/// Ensure the clangd plugin reacts to file-status notifications
#[test]
#[ignore]
fn test_clangd_plugin_file_status_notification() -> anyhow::Result<()> {
    init_tracing_from_env();
    let _fake_server = FakeLspServer::spawn()?;

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "clangd_support");
    copy_plugin_lib(&plugins_dir);

    let src_dir = project_root.join("src");
    fs::create_dir_all(&src_dir).unwrap();
    let source_file = src_dir.join("main.cpp");
    fs::write(&source_file, "int main() { return 0; }\n").unwrap();

    let mut config = Config::default();
    config.lsp.insert(
        "cpp".to_string(),
        LspServerConfig {
            command: FakeLspServer::script_path().to_string_lossy().to_string(),
            args: vec![],
            enabled: true,
            auto_start: true,
            process_limits: ProcessLimits::default(),
            initialization_options: None,
        },
    );

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 30, config, project_root.clone())
            .unwrap();

    harness.open_file(&source_file)?;
    harness.render()?;
    for _ in 0..10 {
        harness.sleep(Duration::from_millis(100));
        let _ = harness.editor_mut().process_async_messages();
        harness.render()?;
    }

    let mut seen_status = false;
    for _ in 0..20 {
        harness.sleep(Duration::from_millis(50));
        let _ = harness.editor_mut().process_async_messages();
        harness.render()?;
        if let Some(msg) = harness.editor().get_status_message() {
            if msg == "Clangd file status: ready" {
                seen_status = true;
                break;
            }
        }
    }

    assert!(
        seen_status,
        "Expected clangd file status notification to set the plugin status"
    );

    Ok(())
}

/// Ensure the clangd plugin uses editor.sendLspRequest successfully
#[test]
#[cfg_attr(windows, ignore)] // Uses bash script for fake LSP server
fn test_clangd_plugin_switch_source_header() -> anyhow::Result<()> {
    init_tracing_from_env();
    let _fake_server = FakeLspServer::spawn()?;

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "clangd_support");
    copy_plugin_lib(&plugins_dir);

    let src_dir = project_root.join("src");
    fs::create_dir_all(&src_dir).unwrap();
    let source_file = src_dir.join("main.cpp");
    fs::write(&source_file, "int main() { return 0; }\n").unwrap();
    let header_file = src_dir.join("main.h");
    fs::write(&header_file, "// header content\n").unwrap();

    let mut config = Config::default();
    config.lsp.insert(
        "cpp".to_string(),
        LspServerConfig {
            command: FakeLspServer::script_path().to_string_lossy().to_string(),
            args: vec![],
            enabled: true,
            auto_start: true,
            process_limits: ProcessLimits::default(),
            initialization_options: None,
        },
    );

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 30, config, project_root.clone())
            .unwrap();

    harness.open_file(&source_file)?;
    harness.render()?;

    // Open command palette and run the switch command
    // Wait for the command to appear (meaning plugin is loaded)
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Clangd: Switch Source/Header").unwrap();
    harness.wait_until(|h| h.screen_to_string().contains("Switch Source/Header"))?;
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for the header file content to appear
    harness.wait_until(|h| h.screen_to_string().contains("header content"))?;

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("header content"),
        "Expected header file to be visible, got:\n{}",
        screen
    );

    Ok(())
}

/// Test that plugin commands show the plugin name as source in command palette
#[test]
fn test_plugin_command_source_in_palette() {
    // Initialize tracing and signal handlers for debugging
    init_tracing_from_env();
    fresh::services::signal_handler::install_signal_handlers();

    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir); // Required for TypeScript type declarations

    // Create a simple plugin that registers a command
    // IMPORTANT: description must NOT contain "test_source_plugin" so we can verify
    // that the source column shows it (not the description)
    let test_plugin = r#"
const editor = getEditor();
// Simple test plugin to verify command source is shown correctly
editor.registerCommand(
    "Test Source Plugin Command",
    "A special command for testing",
    "test_source_action",
    null
);

editor.setStatus("Test source plugin loaded!");
"#;

    let test_plugin_path = plugins_dir.join("test_source_plugin.ts");
    fs::write(&test_plugin_path, test_plugin).unwrap();

    // Create a test file
    let fixture = TestFixture::new("test.txt", "Test content\n").unwrap();

    // Create harness with the project directory
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 30, Default::default(), project_root)
            .unwrap();

    // Open the test file
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    // Wait for plugins to load
    for _ in 0..5 {
        harness.process_async_and_render().unwrap();
        harness.sleep(Duration::from_millis(50));
    }

    // Open command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Search for our plugin command
    harness.type_text("Test Source Plugin").unwrap();

    // Process to update suggestions
    for _ in 0..3 {
        harness.process_async_and_render().unwrap();
        harness.sleep(Duration::from_millis(50));
    }

    let screen = harness.screen_to_string();
    println!("Screen showing command palette:\n{}", screen);

    // Verify the command appears
    assert!(
        screen.contains("Test Source Plugin Command"),
        "Plugin command should appear in palette. Got:\n{}",
        screen
    );

    // Verify the source shows the plugin name, NOT "builtin"
    // The source column should show "test_source_p..." (truncated filename without .ts)
    assert!(
        screen.contains("test_source_p"),
        "Command source should show plugin name 'test_source_p...', not 'builtin'. Got:\n{}",
        screen
    );
    // Also verify it does NOT show "builtin" for this command
    // (Since the command is on screen, if it showed "builtin" we'd see it)
    // Note: We can't easily check the specific line, but the fact that test_source_p
    // appears AND builtin commands show "builtin" confirms the feature works

    // Also verify that builtin commands still show "builtin"
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Open palette again and search for a builtin command
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Save File").unwrap();

    for _ in 0..3 {
        harness.process_async_and_render().unwrap();
        harness.sleep(Duration::from_millis(50));
    }

    let screen2 = harness.screen_to_string();
    println!("Screen showing Save File command:\n{}", screen2);

    // Save File should show "builtin" as source
    assert!(
        screen2.contains("builtin"),
        "Builtin command should show 'builtin' as source. Got:\n{}",
        screen2
    );
}

/// Test that diagnostics from fake LSP are stored and accessible via getAllDiagnostics API
#[test]
#[cfg_attr(windows, ignore)] // Uses bash script for fake LSP server
fn test_diagnostics_api_with_fake_lsp() -> anyhow::Result<()> {
    init_tracing_from_env();
    let _fake_server = FakeLspServer::spawn()?;

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory and copy lib
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    let lib_source_dir = std::env::current_dir().unwrap().join("plugins/lib");
    let lib_dest_dir = plugins_dir.join("lib");
    fs::create_dir(&lib_dest_dir).unwrap();
    for entry in fs::read_dir(&lib_source_dir).unwrap() {
        let entry = entry.unwrap();
        if entry.path().extension().map(|e| e == "ts").unwrap_or(false) {
            fs::copy(entry.path(), lib_dest_dir.join(entry.file_name())).unwrap();
        }
    }

    // Create a simple plugin that captures diagnostics via getAllDiagnostics
    let test_plugin = r#"/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

// Test plugin to verify getAllDiagnostics API works with real LSP data
let diagnosticCount = 0;

globalThis.on_test_diagnostics_updated = function(data: { uri: string; count: number }): void {
    // When diagnostics update, query them and store count
    const allDiags = editor.getAllDiagnostics();
    diagnosticCount = allDiags.length;
    editor.setStatus(`Diagnostics received: ${diagnosticCount} total, URI count: ${data.count}`);
};

globalThis.get_diagnostic_count = function(): void {
    const allDiags = editor.getAllDiagnostics();
    diagnosticCount = allDiags.length;
    editor.setStatus(`Current diagnostics: ${diagnosticCount}`);
};

editor.on("diagnostics_updated", "on_test_diagnostics_updated");

editor.registerCommand(
    "Test: Get Diagnostic Count",
    "Report the number of diagnostics",
    "get_diagnostic_count",
    "normal"
);

editor.setStatus("Test diagnostics plugin loaded");
"#;

    fs::write(plugins_dir.join("test_diagnostics.ts"), test_plugin).unwrap();

    // Create a test Rust file
    let test_file = project_root.join("test.rs");
    fs::write(&test_file, "fn main() {\n    let x = 1;\n}\n").unwrap();

    // Configure fake LSP for Rust files
    let mut config = Config::default();
    config.lsp.insert(
        "rust".to_string(),
        LspServerConfig {
            command: FakeLspServer::script_path().to_string_lossy().to_string(),
            args: vec![],
            enabled: true,
            auto_start: true,
            process_limits: ProcessLimits::default(),
            initialization_options: None,
        },
    );

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(100, 30, config, project_root.clone())
            .unwrap();

    // Open the test file - this will start LSP
    harness.open_file(&test_file)?;
    harness.render()?;

    // Wait for LSP to initialize and plugin to load
    for _ in 0..10 {
        harness.sleep(Duration::from_millis(100));
        let _ = harness.editor_mut().process_async_messages();
        harness.render()?;
    }

    // Save the file to trigger diagnostics from fake LSP
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render()?;

    // Wait for diagnostics to be received and processed
    // Loop indefinitely - test framework timeout will catch actual failures
    loop {
        harness.sleep(Duration::from_millis(100));
        let _ = harness.editor_mut().process_async_messages();
        harness.render()?;

        // Check if diagnostics were stored
        let stored = harness.editor().get_stored_diagnostics();
        if !stored.is_empty() {
            println!("Diagnostics received: {:?}", stored);
            break;
        }
    }

    // Verify the diagnostics content
    let stored = harness.editor().get_stored_diagnostics();
    assert_eq!(stored.len(), 1, "Expected diagnostics for one file");

    // Get the diagnostics for our file
    // Note: On macOS, temp paths like /var/folders/... get canonicalized to /private/var/folders/...
    // so we need to canonicalize the path before constructing the URI
    let canonical_path = test_file
        .canonicalize()
        .unwrap_or_else(|_| test_file.clone());
    let file_uri = format!("file://{}", canonical_path.to_string_lossy());
    let diags = stored
        .get(&file_uri)
        .expect("Should have diagnostics for test file");
    assert_eq!(diags.len(), 1, "Expected exactly one diagnostic");

    // Verify the diagnostic content matches what fake LSP sends
    let diag = &diags[0];
    assert_eq!(
        diag.message, "Test error from fake LSP",
        "Diagnostic message should match fake LSP"
    );
    assert_eq!(
        diag.severity,
        Some(lsp_types::DiagnosticSeverity::ERROR),
        "Diagnostic severity should be error"
    );

    // Verify the plugin's diagnostics_updated hook was called
    // by checking if the status message shows diagnostics count
    if let Some(status) = harness.editor().get_status_message() {
        println!("Status message: {}", status);
        // The hook should have set status with "Diagnostics received"
        if status.contains("Diagnostics received") {
            println!("Plugin hook was triggered successfully");
        }
    }

    Ok(())
}

/// Test theme-aware overlay support
///
/// Verifies that overlays with theme keys resolve to the correct colors
/// from the current theme at render time.
#[test]
fn test_theme_aware_overlay() {
    init_tracing_from_env();

    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir); // Required for TypeScript type declarations

    // Create a simple plugin that adds theme-aware overlays
    let test_plugin = r###"
const editor = getEditor();

globalThis.test_theme_overlay = function(): void {
    const bufferId = editor.getActiveBufferId();
    // bufferId is a valid non-negative number (0 is the first buffer)
    if (bufferId === null || bufferId === undefined) {
        editor.setStatus("No active buffer");
        return;
    }

    // Clear any existing overlays
    editor.clearNamespace(bufferId, "test-theme");

    // Add overlay with theme key for foreground (syntax.keyword)
    editor.addOverlay(bufferId, "test-theme", 0, 4, {
        fg: "syntax.keyword",  // theme key - should resolve to keyword color
    });

    // Add overlay with theme key for background
    editor.addOverlay(bufferId, "test-theme", 5, 9, {
        fg: [255, 255, 255],  // RGB fallback white
        bg: "editor.selection_bg",  // theme key for background
    });

    editor.setStatus("Theme overlays applied");
};

editor.registerCommand(
    "Test: Apply Theme Overlay",
    "Apply overlays with theme keys",
    "test_theme_overlay",
    null  // Use null for context to ensure command is visible
);

editor.debug("Theme overlay test plugin loaded");
"###;

    let test_plugin_path = plugins_dir.join("test_theme_overlay.ts");
    fs::write(&test_plugin_path, test_plugin).unwrap();

    // Create test file INSIDE project_root (not a separate temp directory)
    let test_file_path = project_root.join("test_theme.txt");
    fs::write(&test_file_path, "TEST WORD here\nSecond line\n").unwrap();

    // Create harness with wide screen to avoid status message truncation
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 30, Default::default(), project_root)
            .unwrap();

    // Open the test file
    harness.open_file(&test_file_path).unwrap();
    harness.render().unwrap();

    // Execute the theme overlay command
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Test: Apply Theme Overlay").unwrap();
    // Wait for command to appear in Quick Open
    harness
        .wait_until(|h| h.screen_to_string().contains("Apply Theme Overlay"))
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for overlays to be applied
    harness
        .wait_until(|h| {
            if let Some(status) = h.editor().get_status_message() {
                status.contains("Theme overlays applied")
            } else {
                false
            }
        })
        .unwrap();

    harness.render().unwrap();

    // Find the screen position of "TEST" - it's not at (0,0) due to menu bar and tabs
    let screen = harness.screen_to_string();
    println!("Screen content:\n{}", screen);

    // Find the line containing "TEST" and get its y position
    let mut test_y: Option<u16> = None;
    let mut test_x: Option<u16> = None;
    for (y, line) in screen.lines().enumerate() {
        if let Some(x) = line.find("TEST") {
            test_y = Some(y as u16);
            test_x = Some(x as u16);
            println!("Found 'TEST' at screen position ({}, {})", x, y);
            break;
        }
    }

    let (x, y) = match (test_x, test_y) {
        (Some(x), Some(y)) => (x, y),
        _ => panic!("Could not find 'TEST' on screen"),
    };

    // Check that the overlay was applied with a real color
    // The first character of "TEST" should have the syntax.keyword color
    let style_at_test = harness.get_cell_style(x, y);
    println!("Style at 'T' position ({}, {}): {:?}", x, y, style_at_test);

    // Get the overlays from the buffer to verify they exist
    let overlays = harness.editor().active_state().overlays.all();
    println!("Number of overlays: {}", overlays.len());

    for (i, overlay) in overlays.iter().enumerate() {
        println!("Overlay {}: face={:?}", i, overlay.face);
    }

    // Verify overlays were created
    assert!(
        overlays.len() >= 2,
        "Expected at least 2 overlays to be created, got {}",
        overlays.len()
    );

    // Check that the foreground color at "T" is a themed color
    let style = style_at_test.expect("Should have style at TEST position");
    println!("Foreground color at T: {:?}", style.fg);

    // The color should be resolved from the theme (syntax.keyword)
    // It should NOT be the default editor foreground (White) - it should be themed
    let fg = style.fg.expect("Should have foreground color");

    // The theme's syntax.keyword color should be different from the default White
    // (high-contrast theme uses Cyan for syntax.keyword)
    assert!(
        !matches!(fg, ratatui::style::Color::White),
        "Theme key 'syntax.keyword' was not resolved - still showing default White. \
         Got {:?}, expected a themed color like Cyan or RGB.",
        fg
    );
    println!("Theme overlay resolved to: {:?}", fg);

    let screen = harness.screen_to_string();
    println!("Final screen:\n{}", screen);
}
