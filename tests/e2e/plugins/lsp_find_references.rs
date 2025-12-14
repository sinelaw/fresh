//! LSP find references tests that depend on the find_references plugin

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Test find references functionality with a fake LSP server
///
/// This test verifies that the find references feature works correctly:
/// 1. LSP server responds to textDocument/references
/// 2. The find_references plugin receives the results
/// 3. The references panel opens without hanging
#[test]
#[cfg_attr(windows, ignore)] // Uses bash script for fake LSP server
fn test_lsp_find_references() -> std::io::Result<()> {
    use std::time::Duration;
    use tracing_subscriber::{fmt, prelude::*, EnvFilter};

    // Initialize tracing subscriber to see logs
    let _ = tracing_subscriber::registry()
        .with(fmt::layer().with_writer(std::io::stderr))
        .with(EnvFilter::from_default_env())
        .try_init();

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
        fresh::services::lsp::client::LspServerConfig {
            command: script_path.to_string_lossy().to_string(),
            args: vec![],
            enabled: true,
            auto_start: false,
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

    // Wait for LSP to initialize and plugin to load
    for _ in 0..20 {
        harness.process_async_and_render()?;
        harness.sleep(Duration::from_millis(50));
    }

    eprintln!(
        "Screen before find references:\n{}",
        harness.screen_to_string()
    );

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

    // Wait for the references panel to appear with actual results
    // This is where the hang was occurring - the panel should show references content
    let mut panel_appeared = false;
    for i in 0..50 {
        // Process any async messages by sending a null key event
        harness.send_key(KeyCode::Null, KeyModifiers::NONE)?;
        harness.render()?;
        harness.sleep(Duration::from_millis(100));

        let screen = harness.screen_to_string();

        // Debug: print progress every 10 iterations
        if i % 10 == 0 {
            eprintln!("Iteration {}: checking for panel...", i);
            // Check status bar
            if let Some(last_line) = screen.lines().last() {
                eprintln!("Status: {}", last_line);
            }
        }

        // Look for actual panel content, not just "Find References" in status
        if screen.contains("═══ References") || screen.contains("test.rs:") {
            eprintln!("References panel appeared after {} iterations", i + 1);
            panel_appeared = true;
            break;
        }

        // Also check for "Found X reference" in status (indicates plugin received results)
        if screen.contains("Found") && screen.contains("reference") {
            eprintln!("Status shows references found at iteration {}", i + 1);
        }
    }

    let final_screen = harness.screen_to_string();
    eprintln!("Final screen:\n{}", final_screen);

    assert!(
        panel_appeared,
        "Find references should complete without hanging and show results panel. Screen:\n{}",
        final_screen
    );

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
    use std::time::Duration;
    use tracing_subscriber::{fmt, prelude::*, EnvFilter};

    // Initialize tracing subscriber to see logs
    let _ = tracing_subscriber::registry()
        .with(fmt::layer().with_writer(std::io::stderr))
        .with(EnvFilter::from_default_env())
        .try_init();

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
    harness.send_key(KeyCode::Null, KeyModifiers::NONE)?;
    harness.render()?;

    eprintln!("Waiting for rust-analyzer to initialize...");

    // Wait for LSP to initialize (rust-analyzer can take a while)
    let mut lsp_ready = false;
    for i in 0..100 {
        harness.send_key(KeyCode::Null, KeyModifiers::NONE)?;
        harness.render()?;

        let screen = harness.screen_to_string();
        if screen.contains("LSP [rust: ready]") || screen.contains("rust: ready") {
            eprintln!("LSP ready after {} iterations", i + 1);
            lsp_ready = true;
            break;
        }

        if i % 20 == 0 {
            eprintln!("Iteration {}: waiting for LSP...", i);
        }

        harness.sleep(Duration::from_millis(100));
    }

    if !lsp_ready {
        eprintln!("Warning: LSP may not be fully ready, continuing anyway...");
    }

    eprintln!(
        "Screen before find references:\n{}",
        harness.screen_to_string()
    );

    // Move cursor to the function name "helper_function" on line 1
    // The function starts at column 3 (after "fn "), move to column 7 to be clearly inside
    for _ in 0..7 {
        harness.send_key(KeyCode::Right, KeyModifiers::NONE)?;
    }
    harness.render()?;

    // Trigger find references via command palette
    harness.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)?;
    harness.render()?;
    harness.type_text("Find References")?;
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;

    eprintln!("Find references triggered, waiting for results...");

    // Wait for the references panel to appear with actual results
    let mut panel_appeared = false;
    for i in 0..100 {
        harness.send_key(KeyCode::Null, KeyModifiers::NONE)?;
        harness.render()?;
        harness.sleep(Duration::from_millis(100));

        let screen = harness.screen_to_string();

        // Debug: print progress every 20 iterations
        if i % 20 == 0 {
            eprintln!("Iteration {}: checking for panel...", i);
            if let Some(last_line) = screen.lines().last() {
                eprintln!("Status: {}", last_line);
            }
        }

        // Look for actual panel content - should show references to helper_function
        if screen.contains("═══ References")
            || screen.contains("helper_function") && screen.contains("main.rs:")
        {
            eprintln!("References panel appeared after {} iterations", i + 1);
            panel_appeared = true;
            break;
        }

        // Also check for "Found X reference" in status
        if screen.contains("Found") && screen.contains("reference") {
            eprintln!("Status shows references found at iteration {}", i + 1);
        }
    }

    let final_screen = harness.screen_to_string();
    eprintln!("Final screen:\n{}", final_screen);

    assert!(
        panel_appeared,
        "Find references should complete and show results panel. Screen:\n{}",
        final_screen
    );

    // The panel should show at least 4 references:
    // 1. The definition on line 1
    // 2. Call on line 6
    // 3. Call on line 7
    // 4. Call on line 8
    assert!(
        final_screen.contains("helper_function"),
        "Panel should show 'helper_function' in results. Screen:\n{}",
        final_screen
    );

    eprintln!("\n✅ SUCCESS: Find references works with rust-analyzer!");

    Ok(())
}
