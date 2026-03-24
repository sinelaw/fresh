//! E2E test for issue #1344: Goto Definition does not work from Read Only file
//!
//! Root cause: files in library paths (site-packages, node_modules, .cargo, etc.)
//! are detected by `is_library_path` and have `lsp_enabled` set to `false`, which
//! prevents any LSP operations (including Goto Definition) from working in those
//! buffers.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Issue #1344: Goto Definition does not work from a read-only library file.
///
/// When the user navigates to a file in site-packages (or similar library path)
/// via Goto Definition, the opened file gets `lsp_enabled = false` because
/// `is_library_path` returns true. This prevents any further LSP operations
/// (including Goto Definition) from working in that buffer.
///
/// Steps:
/// 1. Open a writable main.py, wait for LSP to initialize
/// 2. Goto Definition from main.py -> jumps to lib.py inside site-packages/
/// 3. lib.py is opened as read-only with LSP disabled (library file)
/// 4. From lib.py, Goto Definition again -> fails because LSP is disabled
///
/// The test will hang (timeout) at step 4, reproducing the bug.
#[test]
#[cfg_attr(windows, ignore)] // Uses bash script for fake LSP server
fn test_goto_definition_from_readonly_file() -> anyhow::Result<()> {
    let temp_dir = tempfile::TempDir::new()?;
    let project_root = temp_dir.path().to_path_buf();

    // Create main.py (writable, in project root)
    let main_file = project_root.join("main.py");
    std::fs::write(
        &main_file,
        "import lib\n\ndef main():\n    lib.helper()\n",
    )?;

    // Create lib.py inside a site-packages directory to trigger is_library_path.
    // This is the key: files in site-packages/ are detected as library files
    // and have lsp_enabled=false, which is the root cause of issue #1344.
    let site_packages_dir = project_root.join("venv/lib/python3.11/site-packages/mylib");
    std::fs::create_dir_all(&site_packages_dir)?;
    let lib_file = site_packages_dir.join("lib.py");
    std::fs::write(
        &lib_file,
        "def helper():\n    return 42\n\ndef caller():\n    helper()\n",
    )?;

    // Build URIs
    let main_uri = format!("file://{}", main_file.to_str().unwrap());
    let lib_uri = format!("file://{}", lib_file.to_str().unwrap());

    let log_file = project_root.join("lsp_log.txt");
    let log_path = log_file.to_str().unwrap();

    // Fake LSP server that:
    // - From main.py -> points to lib.py in site-packages (line 3, caller function)
    // - From lib.py  -> points to lib.py line 0 (helper definition, same file)
    let script = format!(
        r##"#!/bin/bash

MAIN_URI="{main_uri}"
LIB_URI="{lib_uri}"
LOG_FILE="{log_path}"

> "$LOG_FILE"

read_message() {{
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
}}

send_message() {{
    local message="$1"
    local length=${{#message}}
    printf "Content-Length: $length\r\n\r\n%s" "$message"
}}

while true; do
    msg=$(read_message)

    if [ -z "$msg" ]; then
        break
    fi

    method=$(echo "$msg" | grep -o '"method":"[^"]*"' | cut -d'"' -f4)
    msg_id=$(echo "$msg" | grep -o '"id":[0-9]*' | cut -d':' -f2)

    echo "RECV: method=$method id=$msg_id" >> "$LOG_FILE"

    case "$method" in
        "initialize")
            send_message '{{"jsonrpc":"2.0","id":'$msg_id',"result":{{"capabilities":{{"definitionProvider":true,"textDocumentSync":1}}}}}}'
            echo "SENT: initialize response" >> "$LOG_FILE"
            ;;
        "initialized")
            echo "ACTION: initialized" >> "$LOG_FILE"
            ;;
        "textDocument/didOpen"|"textDocument/didChange"|"textDocument/didClose")
            uri=$(echo "$msg" | grep -o '"uri":"[^"]*"' | head -1 | cut -d'"' -f4)
            echo "ACTION: $method uri=$uri" >> "$LOG_FILE"
            ;;
        "textDocument/definition")
            uri=$(echo "$msg" | grep -o '"uri":"[^"]*"' | head -1 | cut -d'"' -f4)
            echo "RECV: definition request from uri=$uri" >> "$LOG_FILE"
            if [ "$uri" = "$MAIN_URI" ]; then
                # From main.py -> jump to lib.py in site-packages (line 3)
                send_message '{{"jsonrpc":"2.0","id":'$msg_id',"result":{{"uri":"'"$LIB_URI"'","range":{{"start":{{"line":3,"character":4}},"end":{{"line":3,"character":10}}}}}}}}'
                echo "SENT: definition -> lib.py:3" >> "$LOG_FILE"
            else
                # From lib.py -> jump to lib.py line 0 (helper definition)
                send_message '{{"jsonrpc":"2.0","id":'$msg_id',"result":{{"uri":"'"$LIB_URI"'","range":{{"start":{{"line":0,"character":4}},"end":{{"line":0,"character":10}}}}}}}}'
                echo "SENT: definition -> lib.py:0" >> "$LOG_FILE"
            fi
            ;;
        "textDocument/diagnostic")
            send_message '{{"jsonrpc":"2.0","id":'$msg_id',"result":{{"kind":"full","items":[]}}}}'
            ;;
        "textDocument/inlayHint")
            send_message '{{"jsonrpc":"2.0","id":'$msg_id',"result":[]}}'
            ;;
        "textDocument/semanticTokens/full"|"textDocument/semanticTokens/full/delta"|"textDocument/semanticTokens/range")
            send_message '{{"jsonrpc":"2.0","id":'$msg_id',"result":{{"data":[]}}}}'
            ;;
        "shutdown")
            send_message '{{"jsonrpc":"2.0","id":'$msg_id',"result":null}}'
            echo "ACTION: shutdown" >> "$LOG_FILE"
            break
            ;;
    esac
done
"##
    );

    let script_path = project_root.join("fake_goto_def_lsp.sh");
    std::fs::write(&script_path, &script)?;

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = std::fs::metadata(&script_path)?.permissions();
        perms.set_mode(0o755);
        std::fs::set_permissions(&script_path, perms)?;
    }

    // Configure editor with the fake LSP for Python
    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "python".to_string(),
        fresh::services::lsp::LspServerConfig {
            command: script_path.to_string_lossy().to_string(),
            args: vec![],
            enabled: true,
            auto_start: true,
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
            env: Default::default(),
            language_id_overrides: Default::default(),
        },
    );

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(100, 30, config, project_root)?;

    // Open main.py
    harness.open_file(&main_file)?;

    // Wait for LSP to initialize
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.contains("ACTION: initialized")
    })?;

    // Move cursor onto "helper" on line 4 (the function call)
    // Content: "import lib\n\ndef main():\n    lib.helper()\n"
    // Line 4: "    lib.helper()"
    for _ in 0..3 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    }
    for _ in 0..8 {
        harness.send_key(KeyCode::Right, KeyModifiers::NONE)?;
    }
    harness.process_async_and_render()?;

    // Step 1: Goto Definition (F12) from main.py -> jumps to lib.py in site-packages
    harness.send_key(KeyCode::F(12), KeyModifiers::NONE)?;

    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.contains("SENT: definition -> lib.py:3")
    })?;

    // Wait until lib.py appears on screen
    harness.wait_until(|h| h.screen_to_string().contains("lib.py"))?;

    // Verify lib.py is read-only (library file in site-packages)
    assert!(
        harness.editor().is_active_buffer_read_only(),
        "lib.py in site-packages should be read-only"
    );

    // Step 2: From the read-only library file, Goto Definition on "helper()" call.
    // Move cursor to line 4 col 4 where helper() is called.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    for _ in 0..4 {
        harness.send_key(KeyCode::Right, KeyModifiers::NONE)?;
    }
    harness.process_async_and_render()?;

    // Trigger Goto Definition from the read-only library file
    harness.send_key(KeyCode::F(12), KeyModifiers::NONE)?;

    // Issue #1344: LSP is disabled for library files (lsp_enabled=false in
    // BufferMetadata), so this request is never sent to the LSP server.
    // The test will hang here (timeout) until the bug is fixed.
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.contains("SENT: definition -> lib.py:0")
    })?;

    Ok(())
}
