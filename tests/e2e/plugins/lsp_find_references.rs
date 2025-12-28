//! LSP find references tests that depend on the find_references plugin

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Test find references functionality with a fake LSP server
///
/// This test verifies that the find references feature works correctly:
/// 1. LSP server responds to textDocument/references
/// 2. The find_references plugin receives the results
/// 3. The references panel opens without hanging
///
/// TODO: This test needs investigation - the fake LSP server and plugin loading
/// may not be working correctly in the test environment.
#[test]
#[ignore = "Needs investigation: fake LSP and plugin loading issues"]
#[cfg_attr(windows, ignore)] // Uses bash script for fake LSP server
fn test_lsp_find_references() -> std::io::Result<()> {

    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new()?;
    let project_root = temp_dir.path().to_path_buf();

    // Create plugins directory and copy find_references plugin
    let plugins_dir = project_root.join("plugins");
    std::fs::create_dir(&plugins_dir)?;

    let plugin_source = std::env::current_dir()?.join("plugins/find_references.ts");
    let plugin_dest = plugins_dir.join("find_references.ts");
    std::fs::copy(&plugin_source, &plugin_dest)?;

    // Create a fake LSP server script that responds to references requests
    let fake_lsp_script = r#"#!/bin/bash

# Function to read a message
read_message() {
    local content_length=0
    while IFS=: read -r key value; do
        key=$(echo "$key" | tr -d '\r\n')
        value=$(echo "$value" | tr -d '\r\n ')
        if [ "$key" = "Content-Length" ]; then
            content_length=$value
        fi
        if [ -z "$key" ]; then
            break
        fi
    done

    if [ $content_length -gt 0 ]; then
        dd bs=1 count=$content_length 2>/dev/null
    fi
}

# Function to send a message
send_message() {
    local message="$1"
    local length=${#message}
    echo -en "Content-Length: $length\r\n\r\n$message"
}

# Main loop
while true; do
    msg=$(read_message)

    if [ -z "$msg" ]; then
        break
    fi

    method=$(echo "$msg" | grep -o '"method":"[^"]*"' | cut -d'"' -f4)
    msg_id=$(echo "$msg" | grep -o '"id":[0-9]*' | cut -d':' -f2)

    case "$method" in
        "initialize")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":{"capabilities":{"referencesProvider":true,"textDocumentSync":1}}}'
            ;;
        "initialized")
            # No response needed
            ;;
        "textDocument/didOpen"|"textDocument/didChange"|"textDocument/didSave")
            # No response needed for notifications
            ;;
        "textDocument/diagnostic")
            # Handle pull diagnostics - return empty diagnostics
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":{"kind":"full","items":[]}}'
            ;;
        "textDocument/references")
            uri=$(echo "$msg" | grep -o '"uri":"[^"]*"' | head -1 | cut -d'"' -f4)
            # Return some fake references
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":[{"uri":"'$uri'","range":{"start":{"line":0,"character":4},"end":{"line":0,"character":14}}},{"uri":"'$uri'","range":{"start":{"line":2,"character":8},"end":{"line":2,"character":18}}},{"uri":"'$uri'","range":{"start":{"line":4,"character":12},"end":{"line":4,"character":22}}}]}'
            ;;
        "shutdown")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":null}'
            break
            ;;
    esac
done
"#;

    let script_path = project_root.join("fake_lsp.sh");
    std::fs::write(&script_path, fake_lsp_script)?;

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = std::fs::metadata(&script_path)?.permissions();
        perms.set_mode(0o755);
        std::fs::set_permissions(&script_path, perms)?;
    }

    // Create test rust file
    let test_file = project_root.join("test.rs");
    std::fs::write(
        &test_file,
        r#"fn test_func() {
    println!("hello");
    test_func();
    let x = 1;
    test_func();
}
"#,
    )?;

    // Create config with the fake LSP
    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::services::lsp::LspServerConfig {
            command: script_path.to_string_lossy().to_string(),
            args: vec![],
            enabled: true,
            auto_start: true, // Auto-start so LSP starts when file is opened
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
        },
    );

    // Create harness with config and working directory
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(100, 30, config, project_root.clone())?;

    // Open the test file
    harness.open_file(&test_file)?;
    harness.process_async_and_render()?;

    // Give LSP time to start - the fake LSP doesn't show "ready" in status bar
    // We don't need to wait for LSP ready; we wait for the references panel to appear
    for _ in 0..5 {
        harness.process_async_and_render()?;
    }

    // Move cursor to the function name (line 1, after "fn ")
    harness.send_key(KeyCode::Right, KeyModifiers::NONE)?;
    harness.send_key(KeyCode::Right, KeyModifiers::NONE)?;
    harness.send_key(KeyCode::Right, KeyModifiers::NONE)?;
    harness.process_async_and_render()?;

    // Trigger find references via command palette
    harness.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)?;
    harness.process_async_and_render()?;
    harness.type_text("Find References")?;
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;

    // Wait for the references panel to appear with actual results using semantic waiting
    // The panel should show references content (═══ References header or test.rs: file references)
    harness.wait_until(|h| {
        let screen = h.screen_to_string();
        screen.contains("═══ References") || screen.contains("test.rs:")
    })?;

    Ok(())
}

/// Test find references with real rust-analyzer on a cargo project
///
/// This test creates a temporary cargo project, starts rust-analyzer,
/// and tests the find references functionality end-to-end.
///
/// Requires rust-analyzer to be installed on the system.
#[test]
#[ignore] // Run with: cargo test test_find_references_with_rust_analyzer -- --ignored --nocapture
fn test_find_references_with_rust_analyzer() -> std::io::Result<()> {
    use std::process::Command;

    // Check if rust-analyzer is available
    let ra_check = Command::new("rust-analyzer").arg("--version").output();
    if ra_check.is_err() || !ra_check.unwrap().status.success() {
        eprintln!("rust-analyzer not found, skipping test");
        return Ok(());
    }

    // Create a temporary cargo project
    let temp_dir = tempfile::TempDir::new()?;
    let project_root = temp_dir.path().to_path_buf();

    // Create Cargo.toml
    let cargo_toml = r#"[package]
name = "test_project"
version = "0.1.0"
edition = "2021"

[dependencies]
"#;
    std::fs::write(project_root.join("Cargo.toml"), cargo_toml)?;

    // Create src directory
    let src_dir = project_root.join("src");
    std::fs::create_dir(&src_dir)?;

    // Create main.rs with a function that has multiple references
    let main_rs = r#"fn helper_function(value: i32) -> i32 {
    value * 2
}

fn main() {
    let x = helper_function(5);
    let y = helper_function(10);
    let z = helper_function(x + y);
    println!("Result: {}", z);
}
"#;
    let main_rs_path = src_dir.join("main.rs");
    std::fs::write(&main_rs_path, main_rs)?;

    // Create plugins directory and copy find_references plugin
    let plugins_dir = project_root.join("plugins");
    std::fs::create_dir(&plugins_dir)?;

    let plugin_source = std::env::current_dir()?.join("plugins/find_references.ts");
    let plugin_dest = plugins_dir.join("find_references.ts");
    std::fs::copy(&plugin_source, &plugin_dest)?;

    // Use default config (which includes rust-analyzer)
    let config = fresh::config::Config::default();

    // Create harness with config and working directory
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, config, project_root.clone())?;

    // Open the test file
    harness.open_file(&main_rs_path)?;
    harness.process_async_and_render()?;

    // Wait for LSP to initialize using semantic waiting
    harness.wait_until(|h| {
        let screen = h.screen_to_string();
        screen.contains("LSP [rust: ready]") || screen.contains("rust: ready")
    })?;

    // Move cursor to the function name "helper_function" on line 1
    // The function starts at column 3 (after "fn "), move to column 7 to be clearly inside
    for _ in 0..7 {
        harness.send_key(KeyCode::Right, KeyModifiers::NONE)?;
    }
    harness.render()?;

    // Trigger find references via command palette
    harness.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)?;
    harness.process_async_and_render()?;
    harness.type_text("Find References")?;
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;

    // Wait for the references panel to appear with actual results using semantic waiting
    // The panel should show references to helper_function
    harness.wait_until(|h| {
        let screen = h.screen_to_string();
        screen.contains("═══ References")
            || (screen.contains("helper_function") && screen.contains("main.rs:"))
    })?;

    // Verify the panel shows helper_function in results
    let final_screen = harness.screen_to_string();
    assert!(
        final_screen.contains("helper_function"),
        "Panel should show 'helper_function' in results. Screen:\n{}",
        final_screen
    );

    Ok(())
}
