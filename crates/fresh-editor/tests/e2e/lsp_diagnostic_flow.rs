//! E2E tests for the full LSP diagnostic flow
//!
//! These tests use a fake LSP server that replays actual responses recorded from
//! rust-analyzer (v1.92.0). The recording was done against a Rust file with a
//! type error (`let x: i32 = "hello";`) and captures the exact diagnostic
//! messages, severity levels, and ranges that rust-analyzer produces.
//!
//! The fake server implements the key diagnostic patterns:
//! 1. Push diagnostics via `textDocument/publishDiagnostics` (cargo check results)
//! 2. Pull diagnostics via `textDocument/diagnostic` (native RA diagnostics)
//! 3. `workspace/diagnostic/refresh` server→client requests
//! 4. Diagnostic clearing after the error is fixed

use crate::common::harness::EditorTestHarness;

/// Create a fake LSP server that replays recorded rust-analyzer responses.
///
/// The server behavior is based on actual recordings from rust-analyzer v1.92.0:
/// - initialize response includes `diagnosticProvider` capability
/// - After didOpen: sends `workspace/diagnostic/refresh` then `publishDiagnostics`
///   with the recorded E0308 diagnostics (mismatched types)
/// - Pull diagnostics (`textDocument/diagnostic`) return empty initially
/// - After didChange: sends `workspace/diagnostic/refresh` then `publishDiagnostics`
///   with empty diagnostics (clearing the errors)
fn create_ra_replay_server_script(dir: &std::path::Path) -> std::path::PathBuf {
    // The publishDiagnostics payload is taken verbatim from the recording,
    // with the URI made dynamic (replaced at runtime with the actual file URI).
    let script = r##"#!/bin/bash

# Fake LSP server replaying recorded rust-analyzer responses
# Log file path (passed as first argument)
LOG_FILE="${1:-/tmp/fake_ra_log.txt}"

# Clear log file at start
> "$LOG_FILE"

# Track state
DID_OPEN_URI=""
VERSION=0

# Function to read a JSON-RPC message
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

# Function to send a JSON-RPC message
# Uses printf '%s' for the body to avoid echo -e interpreting \n in JSON strings
send_message() {
    local message="$1"
    local length=${#message}
    printf "Content-Length: $length\r\n\r\n%s" "$message"
}

# Main message loop
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
            echo "ACTION: responding with rust-analyzer-like capabilities" >> "$LOG_FILE"

            # Respond with capabilities matching actual rust-analyzer recording
            # Key: diagnosticProvider with identifier "rust-analyzer"
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":{"capabilities":{"positionEncoding":"utf-16","textDocumentSync":{"openClose":true,"change":2,"save":{}},"completionProvider":{"resolveProvider":false,"triggerCharacters":[":",".","(","'"'"'"]},"hoverProvider":true,"definitionProvider":true,"referencesProvider":true,"diagnosticProvider":{"identifier":"rust-analyzer","interFileDependencies":true,"workspaceDiagnostics":false},"inlayHintProvider":{"resolveProvider":false}}}}'
            ;;
        "initialized")
            echo "ACTION: client initialized" >> "$LOG_FILE"
            ;;
        "textDocument/didOpen")
            DID_OPEN_URI=$(echo "$msg" | grep -o '"uri":"[^"]*"' | head -1 | cut -d'"' -f4)
            VERSION=1
            echo "ACTION: didOpen uri=$DID_OPEN_URI" >> "$LOG_FILE"

            # 1. Send workspace/diagnostic/refresh (server→client request, like RA does)
            #    RA sends this 3 times; we send it once for simplicity
            send_message '{"jsonrpc":"2.0","id":1000,"method":"workspace/diagnostic/refresh","params":{}}'
            echo "SENT: workspace/diagnostic/refresh" >> "$LOG_FILE"

            # 2. Send publishDiagnostics with recorded E0308 diagnostics
            #    These are the exact diagnostics rust-analyzer produces for: let x: i32 = "hello";
            #    Diagnostic 1: severity=1 (error) on "hello" (line 1, char 17-24)
            #    Diagnostic 2: severity=4 (hint) on "i32" (line 1, char 11-14)
            send_message '{"jsonrpc":"2.0","method":"textDocument/publishDiagnostics","params":{"uri":"'"$DID_OPEN_URI"'","diagnostics":[{"range":{"start":{"line":1,"character":17},"end":{"line":1,"character":24}},"severity":1,"code":"E0308","codeDescription":{"href":"https://doc.rust-lang.org/error-index.html#E0308"},"source":"rustc","message":"mismatched types\nexpected `i32`, found `&str`","relatedInformation":[{"location":{"uri":"'"$DID_OPEN_URI"'","range":{"start":{"line":1,"character":11},"end":{"line":1,"character":14}}},"message":"expected due to this"}]},{"range":{"start":{"line":1,"character":11},"end":{"line":1,"character":14}},"severity":4,"code":"E0308","codeDescription":{"href":"https://doc.rust-lang.org/error-index.html#E0308"},"source":"rustc","message":"expected due to this","relatedInformation":[{"location":{"uri":"'"$DID_OPEN_URI"'","range":{"start":{"line":1,"character":17},"end":{"line":1,"character":24}}},"message":"original diagnostic"}]}],"version":'"$VERSION"'}}'
            echo "SENT: publishDiagnostics with 2 diagnostics (E0308)" >> "$LOG_FILE"
            ;;
        "textDocument/didChange")
            VERSION=$((VERSION + 1))
            echo "ACTION: didChange version=$VERSION" >> "$LOG_FILE"

            # After a change, send workspace/diagnostic/refresh
            send_message '{"jsonrpc":"2.0","id":1001,"method":"workspace/diagnostic/refresh","params":{}}'
            echo "SENT: workspace/diagnostic/refresh (post-change)" >> "$LOG_FILE"

            # Then send publishDiagnostics with empty diagnostics (error fixed)
            send_message '{"jsonrpc":"2.0","method":"textDocument/publishDiagnostics","params":{"uri":"'"$DID_OPEN_URI"'","diagnostics":[],"version":'"$VERSION"'}}'
            echo "SENT: publishDiagnostics with 0 diagnostics (cleared)" >> "$LOG_FILE"
            ;;
        "textDocument/diagnostic")
            # Pull diagnostics - return empty items initially (like RA does)
            # Real diagnostics come via push (publishDiagnostics)
            echo "ACTION: responding to pull diagnostic request with empty items" >> "$LOG_FILE"
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":{"kind":"full","resultId":"rust-analyzer","items":[]}}'
            ;;
        "textDocument/inlayHint")
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":[]}'
            ;;
        "textDocument/didSave")
            echo "ACTION: didSave" >> "$LOG_FILE"
            ;;
        "$/cancelRequest")
            ;;
        "shutdown")
            echo "ACTION: shutdown" >> "$LOG_FILE"
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":null}'
            break
            ;;
        *)
            # Only respond to requests (which have both a method and an id).
            # Skip responses (no method) — these are replies to our
            # workspace/diagnostic/refresh server→client requests.
            if [ -n "$method" ] && [ -n "$msg_id" ]; then
                send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":null}'
            fi
            ;;
    esac
done

echo "SERVER: exiting" >> "$LOG_FILE"
"##;

    let script_path = dir.join("fake_ra_replay_server.sh");
    std::fs::write(&script_path, script).expect("Failed to write RA replay server script");

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = std::fs::metadata(&script_path)
            .expect("Failed to get script metadata")
            .permissions();
        perms.set_mode(0o755);
        std::fs::set_permissions(&script_path, perms).expect("Failed to set script permissions");
    }

    script_path
}

/// Test that push diagnostics (publishDiagnostics) from rust-analyzer are displayed.
///
/// This test replays actual rust-analyzer responses for a file with a type error:
/// ```rust
/// fn main() {
///     let x: i32 = "hello";
///     println!("{}", x);
/// }
/// ```
///
/// The recorded diagnostic is E0308 "mismatched types" at line 1, characters 17-24.
/// The editor should display this as a diagnostic overlay and show in the status bar.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses Bash-based fake LSP server
fn test_rust_analyzer_push_diagnostics_displayed() -> anyhow::Result<()> {
    let _ = tracing_subscriber::fmt()
        .with_env_filter("fresh=debug")
        .try_init();

    let temp_dir = tempfile::tempdir()?;
    let script_path = create_ra_replay_server_script(temp_dir.path());
    let log_file = temp_dir.path().join("ra_replay_log.txt");
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(
        &test_file,
        "fn main() {\n    let x: i32 = \"hello\";\n    println!(\"{}\", x);\n}\n",
    )?;

    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: script_path.to_string_lossy().to_string(),
            args: vec![log_file.to_string_lossy().to_string()],
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

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    // Open the Rust test file (triggers initialize + didOpen)
    harness.open_file(&test_file)?;
    harness.render()?;

    // Wait for the fake server to send publishDiagnostics
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.contains("SENT: publishDiagnostics with 2 diagnostics")
    })?;

    // Wait for the editor to process and display the diagnostics
    // The status bar should show "E:1" (1 error) or similar diagnostic indicator
    harness.wait_until(|h| {
        let screen = h.screen_to_string();
        screen.contains("E:1")
    })?;

    // Read the server log to verify the full flow
    let log = std::fs::read_to_string(&log_file)?;
    eprintln!("[TEST] RA replay server log:\n{}", log);

    // Verify the server received initialize and didOpen
    assert!(
        log.contains("RECV: method=initialize"),
        "Expected server to receive initialize.\nLog:\n{}",
        log
    );
    assert!(
        log.contains("ACTION: didOpen"),
        "Expected server to receive didOpen.\nLog:\n{}",
        log
    );

    // Verify diagnostics were sent
    assert!(
        log.contains("SENT: publishDiagnostics with 2 diagnostics"),
        "Expected server to send publishDiagnostics.\nLog:\n{}",
        log
    );

    Ok(())
}

/// Test that diagnostics are cleared when the error is fixed.
///
/// This test replays the full rust-analyzer flow:
/// 1. Open file with error → diagnostics appear
/// 2. Edit file to fix error → diagnostics are cleared
///
/// The fake server sends empty publishDiagnostics after didChange,
/// matching the actual rust-analyzer behavior when an error is resolved.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses Bash-based fake LSP server
fn test_rust_analyzer_diagnostics_cleared_after_fix() -> anyhow::Result<()> {
    let _ = tracing_subscriber::fmt()
        .with_env_filter("fresh=debug")
        .try_init();

    let temp_dir = tempfile::tempdir()?;
    let script_path = create_ra_replay_server_script(temp_dir.path());
    let log_file = temp_dir.path().join("ra_clear_log.txt");
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(
        &test_file,
        "fn main() {\n    let x: i32 = \"hello\";\n    println!(\"{}\", x);\n}\n",
    )?;

    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: script_path.to_string_lossy().to_string(),
            args: vec![log_file.to_string_lossy().to_string()],
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

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    // Open the Rust test file
    harness.open_file(&test_file)?;
    harness.render()?;

    // Wait for initial diagnostics to appear
    harness.wait_until(|h| {
        let screen = h.screen_to_string();
        screen.contains("E:1")
    })?;

    eprintln!("[TEST] Initial diagnostics visible, now simulating fix...");

    // Simulate fixing the error: select all and replace with corrected code
    // Use Ctrl+A to select all, then type the fixed content
    harness.send_key(
        crossterm::event::KeyCode::Char('a'),
        crossterm::event::KeyModifiers::CONTROL,
    )?;
    harness.type_text("fn main() {\n    let x: i32 = 42;\n    println!(\"{}\", x);\n}\n")?;
    harness.render()?;

    // Wait for the server to actually receive the didChange before expecting its response
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.contains("ACTION: didChange")
    })?;

    // Wait for the server to send cleared diagnostics
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.contains("SENT: publishDiagnostics with 0 diagnostics")
    })?;

    // Wait for the editor to clear the diagnostic indicator
    harness.wait_until(|h| {
        let screen = h.screen_to_string();
        !screen.contains("E:1")
    })?;

    let log = std::fs::read_to_string(&log_file)?;
    eprintln!("[TEST] RA replay server log:\n{}", log);

    // Verify the full flow in the server log
    assert!(
        log.contains("SENT: publishDiagnostics with 2 diagnostics"),
        "Expected initial diagnostics.\nLog:\n{}",
        log
    );
    assert!(
        log.contains("ACTION: didChange"),
        "Expected didChange after edit.\nLog:\n{}",
        log
    );
    assert!(
        log.contains("SENT: publishDiagnostics with 0 diagnostics"),
        "Expected diagnostics cleared after fix.\nLog:\n{}",
        log
    );

    Ok(())
}

/// Create a fake LSP server for the edit/save/edit/save flow.
///
/// This server simulates how rust-analyzer behaves across multiple edits and
/// saves. Diagnostic state only changes on `didSave` events (matching how
/// cargo check runs on save in real rust-analyzer):
/// - After didOpen: sends initial error diagnostics (E0308)
/// - After didChange: no diagnostics (cargo check only runs on save)
/// - After didSave #1: re-sends error diagnostics (cargo check confirms error)
/// - After didSave #2: sends cleared diagnostics (error was fixed before save)
///
/// The diagnostic content uses actual rust-analyzer E0308 formatting recorded
/// from rust-analyzer v1.92.0.
fn create_ra_edit_save_server_script(dir: &std::path::Path) -> std::path::PathBuf {
    let script = r##"#!/bin/bash

# Fake LSP server for edit/save/edit/save flow testing
# Diagnostic state only changes on didSave (simulates cargo check on save)
LOG_FILE="${1:-/tmp/fake_ra_edit_save_log.txt}"
> "$LOG_FILE"

DID_OPEN_URI=""
VERSION=0
SAVE_COUNT=0

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

send_message() {
    local message="$1"
    local length=${#message}
    printf "Content-Length: $length\r\n\r\n%s" "$message"
}

# Send error diagnostics (E0308 mismatched types - recorded from rust-analyzer)
send_error_diagnostics() {
    send_message '{"jsonrpc":"2.0","method":"textDocument/publishDiagnostics","params":{"uri":"'"$DID_OPEN_URI"'","diagnostics":[{"range":{"start":{"line":1,"character":17},"end":{"line":1,"character":24}},"severity":1,"code":"E0308","source":"rustc","message":"mismatched types\nexpected `i32`, found `&str`"},{"range":{"start":{"line":1,"character":11},"end":{"line":1,"character":14}},"severity":4,"code":"E0308","source":"rustc","message":"expected due to this"}],"version":'"$VERSION"'}}'
}

# Send cleared diagnostics (no errors)
send_clear_diagnostics() {
    send_message '{"jsonrpc":"2.0","method":"textDocument/publishDiagnostics","params":{"uri":"'"$DID_OPEN_URI"'","diagnostics":[],"version":'"$VERSION"'}}'
}

while true; do
    msg=$(read_message)
    if [ -z "$msg" ]; then break; fi

    method=$(echo "$msg" | grep -o '"method":"[^"]*"' | cut -d'"' -f4)
    msg_id=$(echo "$msg" | grep -o '"id":[0-9]*' | cut -d':' -f2)

    echo "RECV: method=$method id=$msg_id" >> "$LOG_FILE"

    case "$method" in
        "initialize")
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":{"capabilities":{"positionEncoding":"utf-16","textDocumentSync":{"openClose":true,"change":2,"save":{}},"diagnosticProvider":{"identifier":"rust-analyzer","interFileDependencies":true,"workspaceDiagnostics":false},"inlayHintProvider":{"resolveProvider":false}}}}'
            echo "ACTION: initialized" >> "$LOG_FILE"
            ;;
        "initialized")
            ;;
        "textDocument/didOpen")
            DID_OPEN_URI=$(echo "$msg" | grep -o '"uri":"[^"]*"' | head -1 | cut -d'"' -f4)
            VERSION=1
            echo "ACTION: didOpen uri=$DID_OPEN_URI" >> "$LOG_FILE"
            send_error_diagnostics
            echo "SENT: publishDiagnostics with errors (initial)" >> "$LOG_FILE"
            ;;
        "textDocument/didChange")
            VERSION=$((VERSION + 1))
            echo "ACTION: didChange version=$VERSION" >> "$LOG_FILE"
            # No publishDiagnostics on didChange — cargo check only runs on save.
            # (Avoids generating a workspace/diagnostic/refresh round-trip for
            # every character typed, which overwhelms the slow dd-based reader.)
            ;;
        "textDocument/didSave")
            SAVE_COUNT=$((SAVE_COUNT + 1))
            echo "ACTION: didSave save_count=$SAVE_COUNT" >> "$LOG_FILE"

            # On save, cargo check reruns and sends fresh diagnostics
            send_message '{"jsonrpc":"2.0","id":4000,"method":"workspace/diagnostic/refresh","params":{}}'
            echo "SENT: workspace/diagnostic/refresh (post-save)" >> "$LOG_FILE"
            # Save #1: error still present → re-send error diagnostics
            # Save #2: error was fixed → send cleared diagnostics
            if [ $SAVE_COUNT -le 1 ]; then
                send_error_diagnostics
                echo "SENT: publishDiagnostics with errors (after save $SAVE_COUNT)" >> "$LOG_FILE"
            else
                send_clear_diagnostics
                echo "SENT: publishDiagnostics cleared (after save $SAVE_COUNT)" >> "$LOG_FILE"
            fi
            ;;
        "textDocument/diagnostic")
            echo "ACTION: pull diagnostic request" >> "$LOG_FILE"
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":{"kind":"full","resultId":"rust-analyzer","items":[]}}'
            ;;
        "textDocument/inlayHint")
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":[]}'
            ;;
        "$/cancelRequest")
            ;;
        "shutdown")
            echo "ACTION: shutdown" >> "$LOG_FILE"
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":null}'
            break
            ;;
        *)
            # Only respond to requests (which have both a method and an id).
            # Skip responses (no method) — these are replies to our
            # workspace/diagnostic/refresh server→client requests.
            if [ -n "$method" ] && [ -n "$msg_id" ]; then
                send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":null}'
            fi
            ;;
    esac
done
echo "SERVER: exiting" >> "$LOG_FILE"
"##;

    let script_path = dir.join("fake_ra_edit_save_server.sh");
    std::fs::write(&script_path, script).expect("Failed to write RA edit/save server script");

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = std::fs::metadata(&script_path)
            .expect("Failed to get script metadata")
            .permissions();
        perms.set_mode(0o755);
        std::fs::set_permissions(&script_path, perms).expect("Failed to set script permissions");
    }

    script_path
}

/// Test the edit/save/edit/save diagnostic flow.
///
/// This test simulates a realistic editing session matching cargo check behavior:
/// 1. Open file with type error → diagnostics appear (E:1)
/// 2. Edit (add a comment, error still present) → diagnostics persist (no cargo check)
/// 3. Save → cargo check reruns → diagnostics re-sent (E:1 persists)
/// 4. Edit (fix the type error) → diagnostics still shown (cargo check hasn't rerun)
/// 5. Save → cargo check confirms fix → diagnostics cleared
///
/// This covers the full round-trip that a developer would experience with
/// rust-analyzer, where cargo check errors are updated on save.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses Bash-based fake LSP server
fn test_edit_save_edit_save_diagnostic_flow() -> anyhow::Result<()> {
    let _ = tracing_subscriber::fmt()
        .with_env_filter("fresh=debug")
        .try_init();

    let temp_dir = tempfile::tempdir()?;
    let script_path = create_ra_edit_save_server_script(temp_dir.path());
    let log_file = temp_dir.path().join("ra_edit_save_log.txt");
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(
        &test_file,
        "fn main() {\n    let x: i32 = \"hello\";\n    println!(\"{}\", x);\n}\n",
    )?;

    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: script_path.to_string_lossy().to_string(),
            args: vec![log_file.to_string_lossy().to_string()],
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

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    // === Step 1: Open file with error ===
    harness.open_file(&test_file)?;
    harness.render()?;

    // Wait for initial diagnostics
    harness.wait_until(|h| h.screen_to_string().contains("E:1"))?;
    eprintln!("[TEST] Step 1: Initial diagnostics visible (E:1)");

    // === Step 2: Edit (add a comment, error still present) ===
    harness.send_key(
        crossterm::event::KeyCode::End,
        crossterm::event::KeyModifiers::NONE,
    )?;
    harness.type_text(" // comment")?;
    harness.render()?;

    // Wait for at least one didChange to be processed
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.contains("ACTION: didChange")
    })?;

    // Diagnostics should persist (no cargo check has rerun, old diagnostics still active)
    harness.wait_until(|h| h.screen_to_string().contains("E:1"))?;
    eprintln!("[TEST] Step 2: After edit, diagnostics still visible (E:1)");

    // === Step 3: Save (cargo check reruns, error still present) ===
    harness.send_key(
        crossterm::event::KeyCode::Char('s'),
        crossterm::event::KeyModifiers::CONTROL,
    )?;
    harness.render()?;

    // Wait for save to be processed and diagnostics re-sent
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.contains("SENT: publishDiagnostics with errors (after save 1)")
    })?;

    // Diagnostics should persist after first save
    harness.wait_until(|h| h.screen_to_string().contains("E:1"))?;
    eprintln!("[TEST] Step 3: After first save, diagnostics persist (E:1)");

    // === Step 4: Edit to fix the error ===
    harness.send_key(
        crossterm::event::KeyCode::Char('a'),
        crossterm::event::KeyModifiers::CONTROL,
    )?;
    harness.type_text("fn main() {\n    let x: i32 = 42;\n    println!(\"{}\", x);\n}\n")?;
    harness.render()?;

    eprintln!("[TEST] Step 4: Error fixed in editor, diagnostics may still show (before save)");

    // === Step 5: Save (cargo check confirms fix, diagnostics cleared) ===
    harness.send_key(
        crossterm::event::KeyCode::Char('s'),
        crossterm::event::KeyModifiers::CONTROL,
    )?;
    harness.render()?;

    // Wait for second save to clear diagnostics
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.contains("SENT: publishDiagnostics cleared (after save 2)")
    })?;

    // Diagnostics should be cleared
    harness.wait_until(|h| !h.screen_to_string().contains("E:1"))?;
    eprintln!("[TEST] Step 5: After second save, diagnostics cleared");

    // Verify the complete flow in the server log
    let log = std::fs::read_to_string(&log_file)?;
    eprintln!("[TEST] Full server log:\n{}", log);

    assert!(log.contains("ACTION: didOpen"), "Missing didOpen");
    assert!(
        log.contains("SENT: publishDiagnostics with errors (initial)"),
        "Missing initial diagnostics"
    );
    assert!(log.contains("ACTION: didChange"), "Missing didChange");
    assert!(
        log.contains("ACTION: didSave save_count=1"),
        "Missing first save"
    );
    assert!(
        log.contains("SENT: publishDiagnostics with errors (after save 1)"),
        "Missing diagnostics after first save"
    );
    assert!(
        log.contains("ACTION: didSave save_count=2"),
        "Missing second save"
    );
    assert!(
        log.contains("SENT: publishDiagnostics cleared (after save 2)"),
        "Missing cleared diagnostics after second save"
    );

    Ok(())
}

/// Test that workspace/diagnostic/refresh triggers re-pulling diagnostics.
///
/// This verifies the editor correctly handles the workspace/diagnostic/refresh
/// server→client request that rust-analyzer sends after project loading.
/// The editor should respond with null and then re-pull diagnostics.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses Bash-based fake LSP server
fn test_workspace_diagnostic_refresh_handled() -> anyhow::Result<()> {
    let _ = tracing_subscriber::fmt()
        .with_env_filter("fresh=debug")
        .try_init();

    let temp_dir = tempfile::tempdir()?;
    let script_path = create_ra_replay_server_script(temp_dir.path());
    let log_file = temp_dir.path().join("ra_refresh_log.txt");
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(
        &test_file,
        "fn main() {\n    let x: i32 = \"hello\";\n    println!(\"{}\", x);\n}\n",
    )?;

    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: script_path.to_string_lossy().to_string(),
            args: vec![log_file.to_string_lossy().to_string()],
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

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    // Open the Rust test file
    harness.open_file(&test_file)?;
    harness.render()?;

    // Wait for the server to have sent the refresh request
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.contains("SENT: workspace/diagnostic/refresh")
    })?;

    // The editor should handle the refresh by re-pulling diagnostics
    // Wait for a pull diagnostic request to appear in the log
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.contains("RECV: method=textDocument/diagnostic")
    })?;

    let log = std::fs::read_to_string(&log_file)?;
    eprintln!("[TEST] RA replay server log:\n{}", log);

    // Verify the refresh → pull flow
    assert!(
        log.contains("SENT: workspace/diagnostic/refresh"),
        "Expected server to send workspace/diagnostic/refresh.\nLog:\n{}",
        log
    );
    assert!(
        log.contains("RECV: method=textDocument/diagnostic"),
        "Expected editor to pull diagnostics after refresh.\nLog:\n{}",
        log
    );

    Ok(())
}

/// Create a fake LSP server that simulates delayed diagnostic responses during rapid typing.
///
/// This server deliberately sends stale diagnostics: when it receives didChange, it
/// delays 500ms before sending publishDiagnostics with the *old* version number.
/// Meanwhile, the editor continues sending didChange with newer versions.
/// The editor should drop these stale diagnostics because the version is older
/// than the current document version.
fn create_stale_diagnostics_server_script(dir: &std::path::Path) -> std::path::PathBuf {
    let script = r##"#!/bin/bash

# Fake LSP server that sends stale diagnostics
# On the 2nd didChange, sends diagnostics tagged with the 1st didChange's version
LOG_FILE="${1:-/tmp/fake_stale_diag_log.txt}"
> "$LOG_FILE"

DID_OPEN_URI=""
VERSION=0
CHANGE_COUNT=0

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

send_message() {
    local message="$1"
    local length=${#message}
    printf "Content-Length: $length\r\n\r\n%s" "$message"
}

while true; do
    msg=$(read_message)
    if [ -z "$msg" ]; then break; fi

    method=$(echo "$msg" | grep -o '"method":"[^"]*"' | cut -d'"' -f4)
    msg_id=$(echo "$msg" | grep -o '"id":[0-9]*' | cut -d':' -f2)

    echo "RECV: method=$method id=$msg_id" >> "$LOG_FILE"

    case "$method" in
        "initialize")
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":{"capabilities":{"positionEncoding":"utf-16","textDocumentSync":{"openClose":true,"change":2,"save":{}},"diagnosticProvider":{"identifier":"test","interFileDependencies":false,"workspaceDiagnostics":false},"inlayHintProvider":{"resolveProvider":false}}}}'
            echo "ACTION: initialized" >> "$LOG_FILE"
            ;;
        "initialized")
            ;;
        "textDocument/didOpen")
            DID_OPEN_URI=$(echo "$msg" | grep -o '"uri":"[^"]*"' | head -1 | cut -d'"' -f4)
            VERSION=1
            echo "ACTION: didOpen uri=$DID_OPEN_URI version=$VERSION" >> "$LOG_FILE"
            ;;
        "textDocument/didChange")
            CHANGE_COUNT=$((CHANGE_COUNT + 1))
            VERSION=$((VERSION + 1))
            echo "ACTION: didChange count=$CHANGE_COUNT version=$VERSION" >> "$LOG_FILE"

            # On the first didChange, record the version as stale.
            if [ $CHANGE_COUNT -eq 1 ]; then
                STALE_VERSION=$VERSION
                echo "RECORDED: stale version=$STALE_VERSION" >> "$LOG_FILE"
            fi

            # On the second didChange, send diagnostics tagged with the stale
            # version from the first change.  By now the document version is
            # higher, so the editor should drop these as outdated.  Sending
            # synchronously avoids the concurrent-stdout-write race that the
            # previous background-subshell approach suffered from.
            if [ $CHANGE_COUNT -eq 2 ]; then
                send_message '{"jsonrpc":"2.0","method":"textDocument/publishDiagnostics","params":{"uri":"'"$DID_OPEN_URI"'","diagnostics":[{"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":7}},"severity":1,"code":"E999","source":"test","message":"STALE: Expected module name after import"}],"version":'"$STALE_VERSION"'}}'
                echo "SENT: stale publishDiagnostics version=$STALE_VERSION" >> "$LOG_FILE"
            fi
            ;;
        "textDocument/diagnostic")
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":{"kind":"full","resultId":"test","items":[]}}'
            ;;
        "textDocument/inlayHint")
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":[]}'
            ;;
        "$/cancelRequest")
            ;;
        "shutdown")
            echo "ACTION: shutdown" >> "$LOG_FILE"
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":null}'
            break
            ;;
        *)
            # Only respond to requests (which have both a method and an id).
            # Skip responses (no method) — these are replies to our
            # server→client requests.
            if [ -n "$method" ] && [ -n "$msg_id" ]; then
                send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":null}'
            fi
            ;;
    esac
done
echo "SERVER: exiting" >> "$LOG_FILE"
"##;

    let script_path = dir.join("fake_stale_diag_server.sh");
    std::fs::write(&script_path, script).expect("Failed to write stale diagnostics server script");

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = std::fs::metadata(&script_path)
            .expect("Failed to get script metadata")
            .permissions();
        perms.set_mode(0o755);
        std::fs::set_permissions(&script_path, perms).expect("Failed to set script permissions");
    }

    script_path
}

/// Test that stale diagnostics are dropped during rapid typing.
///
/// Scenario: User types "import os" in a Python file. The LSP server sends
/// diagnostics tagged with version 2 (from the first didChange) while the editor
/// has already moved on to version 3+. The editor should drop the stale
/// diagnostics because version 2 < current version 3+.
///
/// The fake server:
/// 1. On 1st didChange: records the version (2) as stale
/// 2. On 2nd didChange: sends error diagnostics with stale version 2
/// 3. By then the editor's document version is 3+, so diagnostics are dropped
/// 4. The screen should NOT show "E:1" because the stale diagnostics were filtered
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_stale_diagnostics_dropped_during_rapid_typing() -> anyhow::Result<()> {
    let _ = tracing_subscriber::fmt()
        .with_env_filter("fresh=debug")
        .try_init();

    let temp_dir = tempfile::tempdir()?;
    let script_path = create_stale_diagnostics_server_script(temp_dir.path());
    let log_file = temp_dir.path().join("stale_diag_log.txt");
    let test_file = temp_dir.path().join("test.py");
    std::fs::write(&test_file, "")?;

    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "python".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: script_path.to_string_lossy().to_string(),
            args: vec![log_file.to_string_lossy().to_string()],
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

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    // Open the Python file (triggers initialize + didOpen)
    harness.open_file(&test_file)?;
    harness.render()?;

    // Wait for didOpen to be processed
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.contains("ACTION: didOpen")
    })?;

    // Type rapidly: "import os" — each character triggers a didChange.
    // On the 2nd didChange, the server sends diagnostics tagged with the
    // 1st didChange's version, which is already stale.
    harness.type_text("import os")?;
    harness.render()?;

    // Wait for the server to have sent the stale diagnostics
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.contains("SENT: stale publishDiagnostics")
    })?;

    // Process the stale diagnostics message that was already sent
    harness.process_async_and_render()?;

    let screen = harness.screen_to_string();
    eprintln!("[TEST] Screen after rapid typing:\n{}", screen);

    let log = std::fs::read_to_string(&log_file)?;
    eprintln!("[TEST] Server log:\n{}", log);

    // Verify the server DID send stale diagnostics (so the test is meaningful)
    assert!(
        log.contains("SENT: stale publishDiagnostics"),
        "Expected server to send stale diagnostics.\nLog:\n{}",
        log
    );

    // Verify that multiple didChange events were received (rapid typing)
    let change_count = log.matches("ACTION: didChange").count();
    assert!(
        change_count >= 2,
        "Expected at least 2 didChange events for rapid typing, got {}.\nLog:\n{}",
        change_count,
        log
    );

    // The stale diagnostics should have been dropped — no error indicator on screen
    assert!(
        !screen.contains("E:1"),
        "Stale diagnostics should have been dropped! Screen should NOT show E:1.\nScreen:\n{}",
        screen
    );

    Ok(())
}

/// Fake LSP server that both publishes a diagnostic and answers hover requests.
///
/// Used by the hover+diagnostic fusion test: when the user hovers a symbol
/// that also carries an error, the hover popup should show the diagnostic
/// message *and* the hover body together.
///
/// The diagnostic is placed at line 1, characters 17–24 — matching the RA
/// replay script's error range — so the same test file layout works.
fn create_hover_plus_diagnostic_server_script(dir: &std::path::Path) -> std::path::PathBuf {
    let script = r##"#!/bin/bash
LOG_FILE="${1:-/tmp/fake_hover_diag_log.txt}"
> "$LOG_FILE"
DID_OPEN_URI=""
VERSION=0

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

send_message() {
    local message="$1"
    local length=${#message}
    printf "Content-Length: $length\r\n\r\n%s" "$message"
}

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
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":{"capabilities":{"positionEncoding":"utf-16","textDocumentSync":{"openClose":true,"change":2,"save":{}},"hoverProvider":true,"definitionProvider":true}}}'
            ;;
        "initialized")
            ;;
        "textDocument/didOpen")
            DID_OPEN_URI=$(echo "$msg" | grep -o '"uri":"[^"]*"' | head -1 | cut -d'"' -f4)
            VERSION=1
            echo "ACTION: didOpen uri=$DID_OPEN_URI" >> "$LOG_FILE"
            # Error at line 1, characters 17-24 (the "hello" string in `let x: i32 = "hello";`).
            send_message '{"jsonrpc":"2.0","method":"textDocument/publishDiagnostics","params":{"uri":"'"$DID_OPEN_URI"'","diagnostics":[{"range":{"start":{"line":1,"character":17},"end":{"line":1,"character":24}},"severity":1,"source":"rustc","message":"mismatched types\nexpected `i32`, found `&str`"}],"version":'"$VERSION"'}}'
            echo "SENT: publishDiagnostics" >> "$LOG_FILE"
            ;;
        "textDocument/hover")
            # Respond regardless of position — the client sends hover at a
            # single position and that's the only one we care about here.
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":{"contents":{"kind":"markdown","value":"```rust\nlet x: i32\n```\n\nA bound integer variable."},"range":{"start":{"line":1,"character":8},"end":{"line":1,"character":9}}}}'
            echo "SENT: hover" >> "$LOG_FILE"
            ;;
        "shutdown")
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":null}'
            break
            ;;
        *)
            if [ -n "$method" ] && [ -n "$msg_id" ]; then
                send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":null}'
            fi
            ;;
    esac
done
"##;

    let script_path = dir.join("fake_hover_diag_server.sh");
    std::fs::write(&script_path, script).expect("Failed to write hover+diag server script");

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = std::fs::metadata(&script_path)
            .expect("Failed to get script metadata")
            .permissions();
        perms.set_mode(0o755);
        std::fs::set_permissions(&script_path, perms).expect("Failed to set script permissions");
    }

    script_path
}

/// Hover+diagnostic fusion: when the cursor sits on a symbol carrying an
/// error, the hover popup should show the diagnostic message in addition to
/// the LSP hover body. Previously the two lived in separate UIs, so users
/// hovering an offending symbol saw type info only and had to leave hover
/// to find the error.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses Bash-based fake LSP server
fn test_hover_popup_fuses_overlapping_diagnostic() -> anyhow::Result<()> {
    let _ = tracing_subscriber::fmt()
        .with_env_filter("fresh=debug")
        .try_init();

    let temp_dir = tempfile::tempdir()?;
    let script_path = create_hover_plus_diagnostic_server_script(temp_dir.path());
    let log_file = temp_dir.path().join("hover_diag_log.txt");
    let test_file = temp_dir.path().join("test.rs");
    // `let x: i32 = "hello";` on line 1 — the diagnostic covers chars 17-24
    // (the literal string) and the hover request lands inside that range.
    std::fs::write(
        &test_file,
        "fn main() {\n    let x: i32 = \"hello\";\n    println!(\"{}\", x);\n}\n",
    )?;

    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: script_path.to_string_lossy().to_string(),
            args: vec![log_file.to_string_lossy().to_string()],
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

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    harness.open_file(&test_file)?;
    harness.render()?;

    // Wait for the diagnostic round-trip so stored_diagnostics is populated.
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.contains("SENT: publishDiagnostics")
    })?;
    harness.wait_until(|h| h.screen_to_string().contains("E:1"))?;

    // Position the cursor inside the diagnostic range: line 1 (0-indexed),
    // character 18 (inside the "hello" literal at 17..24).
    // Move down once from line 0, then across 18 chars.
    use crossterm::event::{KeyCode, KeyModifiers};
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    for _ in 0..18 {
        harness.send_key(KeyCode::Right, KeyModifiers::NONE)?;
    }

    harness.editor_mut().request_hover()?;

    // Wait for the hover popup to materialize.
    harness.wait_until(|h| h.editor().active_state().popups.is_visible())?;
    harness.render()?;

    let screen = harness.screen_to_string();

    // Fused popup should contain BOTH the diagnostic prefix and the hover body.
    assert!(
        screen.contains("mismatched types"),
        "Hover popup must show the overlapping diagnostic message.\nScreen:\n{}",
        screen
    );
    assert!(
        screen.contains("Error"),
        "Hover popup must label the diagnostic severity.\nScreen:\n{}",
        screen
    );
    assert!(
        screen.contains("bound integer variable"),
        "Hover popup must also show the hover body (not just the diagnostic).\nScreen:\n{}",
        screen
    );

    Ok(())
}

/// When there's no hover content but the cursor does sit on a diagnostic,
/// the popup should still appear — showing the diagnostic — instead of
/// silently saying "No hover information available" via the status bar.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_hover_shows_diagnostic_even_when_hover_is_empty() -> anyhow::Result<()> {
    let _ = tracing_subscriber::fmt()
        .with_env_filter("fresh=debug")
        .try_init();

    // Server that returns diagnostic but empty hover.
    let script = r##"#!/bin/bash
LOG_FILE="${1:-/tmp/fake_empty_hover_log.txt}"
> "$LOG_FILE"
DID_OPEN_URI=""

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

send_message() {
    local message="$1"
    local length=${#message}
    printf "Content-Length: $length\r\n\r\n%s" "$message"
}

while true; do
    msg=$(read_message)
    if [ -z "$msg" ]; then
        break
    fi
    method=$(echo "$msg" | grep -o '"method":"[^"]*"' | cut -d'"' -f4)
    msg_id=$(echo "$msg" | grep -o '"id":[0-9]*' | cut -d':' -f2)
    case "$method" in
        "initialize")
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":{"capabilities":{"positionEncoding":"utf-16","textDocumentSync":{"openClose":true,"change":2,"save":{}},"hoverProvider":true}}}'
            ;;
        "textDocument/didOpen")
            DID_OPEN_URI=$(echo "$msg" | grep -o '"uri":"[^"]*"' | head -1 | cut -d'"' -f4)
            send_message '{"jsonrpc":"2.0","method":"textDocument/publishDiagnostics","params":{"uri":"'"$DID_OPEN_URI"'","diagnostics":[{"range":{"start":{"line":1,"character":17},"end":{"line":1,"character":24}},"severity":2,"source":"clippy","message":"consider using a named constant"}],"version":1}}'
            echo "SENT: publishDiagnostics" >> "$LOG_FILE"
            ;;
        "textDocument/hover")
            # Empty hover — server has nothing to say about this symbol.
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":null}'
            ;;
        "shutdown")
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":null}'
            break
            ;;
        *)
            if [ -n "$method" ] && [ -n "$msg_id" ]; then
                send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":null}'
            fi
            ;;
    esac
done
"##;
    let temp_dir = tempfile::tempdir()?;
    let script_path = temp_dir.path().join("fake_empty_hover_server.sh");
    std::fs::write(&script_path, script)?;
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = std::fs::metadata(&script_path)?.permissions();
        perms.set_mode(0o755);
        std::fs::set_permissions(&script_path, perms)?;
    }

    let log_file = temp_dir.path().join("empty_hover_log.txt");
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(
        &test_file,
        "fn main() {\n    let x: i32 = \"hello\";\n    println!(\"{}\", x);\n}\n",
    )?;

    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: script_path.to_string_lossy().to_string(),
            args: vec![log_file.to_string_lossy().to_string()],
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

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    harness.open_file(&test_file)?;
    harness.render()?;

    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.contains("SENT: publishDiagnostics")
    })?;
    harness.wait_until(|h| h.screen_to_string().contains("W:1"))?;

    // Cursor inside diagnostic range (line 1, char 18).
    use crossterm::event::{KeyCode, KeyModifiers};
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    for _ in 0..18 {
        harness.send_key(KeyCode::Right, KeyModifiers::NONE)?;
    }
    harness.editor_mut().request_hover()?;

    // Popup must appear (even though LSP hover is empty) because we have a
    // diagnostic to show.
    harness.wait_until(|h| h.editor().active_state().popups.is_visible())?;
    harness.render()?;
    let screen = harness.screen_to_string();

    assert!(
        screen.contains("named constant"),
        "Empty hover + diagnostic should still render a popup with the diagnostic.\nScreen:\n{}",
        screen
    );
    assert!(
        screen.contains("Warning"),
        "Popup should label warning severity.\nScreen:\n{}",
        screen
    );

    Ok(())
}
