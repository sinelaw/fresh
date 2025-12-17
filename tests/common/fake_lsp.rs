#![cfg(test)]
//! Fake LSP server for E2E testing
//!
//! This module provides a simple fake LSP server that responds to LSP requests
//! with predefined responses. It's used for testing LSP features without requiring
//! a real language server.

use std::sync::mpsc;
use std::thread;

/// A fake LSP server process for testing
pub struct FakeLspServer {
    /// Handle to the server process
    handle: Option<thread::JoinHandle<()>>,
    /// Channel to stop the server
    stop_tx: mpsc::Sender<()>,
}

impl FakeLspServer {
    /// Spawn a new fake LSP server
    ///
    /// The server will listen on stdin/stdout and respond to LSP requests.
    /// It uses a Bash script that acts as a simple JSON-RPC server.
    pub fn spawn() -> std::io::Result<Self> {
        let (stop_tx, stop_rx) = mpsc::channel();

        // Create a Bash script that acts as a fake LSP server
        // This script reads JSON-RPC messages and sends predefined responses
        let script = r#"#!/bin/bash

# Function to read a message
read_message() {
    # Read headers
    local content_length=0
    while IFS=: read -r key value; do
        key=$(echo "$key" | tr -d '\r\n')
        value=$(echo "$value" | tr -d '\r\n ')
        if [ "$key" = "Content-Length" ]; then
            content_length=$value
        fi
        # Empty line marks end of headers
        if [ -z "$key" ]; then
            break
        fi
    done

    # Read content
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
    # Read incoming message
    msg=$(read_message)

    if [ -z "$msg" ]; then
        break
    fi

    # Extract method from JSON
    method=$(echo "$msg" | grep -o '"method":"[^"]*"' | cut -d'"' -f4)
    msg_id=$(echo "$msg" | grep -o '"id":[0-9]*' | cut -d':' -f2)

case "$method" in
    "initialize")
        # Send initialize response
        send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":{"capabilities":{"completionProvider":{"triggerCharacters":[".",":",":"]},"definitionProvider":true,"hoverProvider":true,"textDocumentSync":1}}}'
        ;;
    "textDocument/hover")
        # Send hover response with range
        # Extract position from request
        line=$(echo "$msg" | grep -o '"line":[0-9]*' | head -1 | cut -d':' -f2)
        char=$(echo "$msg" | grep -o '"character":[0-9]*' | head -1 | cut -d':' -f2)
        # Return hover with a range spanning 10 characters from the position
        end_char=$((char + 10))
        send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":{"contents":{"kind":"markdown","value":"Test hover content"},"range":{"start":{"line":'$line',"character":'$char'},"end":{"line":'$line',"character":'$end_char'}}}}'
        ;;
    "textDocument/completion")
        # Send completion response with sample items
        send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":{"items":[{"label":"test_function","kind":3,"detail":"fn test_function()","insertText":"test_function"},{"label":"test_variable","kind":6,"detail":"let test_variable","insertText":"test_variable"},{"label":"test_struct","kind":22,"detail":"struct TestStruct","insertText":"test_struct"}]}}'
        ;;
    "textDocument/definition")
        # Send definition response (points to line 0, col 0)
        uri=$(echo "$msg" | grep -o '"uri":"[^"]*"' | head -1 | cut -d'"' -f4)
        send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":{"uri":"'$uri'","range":{"start":{"line":0,"character":0},"end":{"line":0,"character":10}}}}'
        ;;
    "textDocument/didSave")
        # Send diagnostics after save
        uri=$(echo "$msg" | grep -o '"uri":"[^"]*"' | head -1 | cut -d'"' -f4)
        send_message '{"jsonrpc":"2.0","method":"textDocument/publishDiagnostics","params":{"uri":"'$uri'","diagnostics":[{"range":{"start":{"line":0,"character":4},"end":{"line":0,"character":5}},"severity":1,"message":"Test error from fake LSP"}]}}'
        ;;
    "textDocument/didOpen")
        uri=$(echo "$msg" | grep -o '"uri":"[^"]*"' | head -1 | cut -d'"' -f4)
        send_message '{"jsonrpc":"2.0","method":"textDocument/clangd.fileStatus","params":{"uri":"'$uri'","status":"ready"}}'
        ;;
    "textDocument/diagnostic")
        # Respond with empty diagnostics
        uri=$(echo "$msg" | grep -o '"uri":"[^"]*"' | head -1 | cut -d'"' -f4)
        send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":{"uri":"'$uri'","items":[],"resultId":null}}'
        ;;
    "textDocument/inlayHint")
        uri=$(echo "$msg" | grep -o '"uri":"[^"]*"' | head -1 | cut -d'"' -f4)
        send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":[]}'
        ;;
    "textDocument/switchSourceHeader")
        uri=$(echo "$msg" | grep -o '"uri":"[^"]*"' | head -1 | cut -d'"' -f4)
        header="${uri%.*}.h"
        send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":"'"$header"'" }'
        ;;
    "shutdown")
        send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":null}'
        break
        ;;
esac
done
"#;

        // Write script to a temporary file
        let script_path = std::env::temp_dir().join("fake_lsp_server.sh");
        std::fs::write(&script_path, script)?;

        // Make it executable
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let mut perms = std::fs::metadata(&script_path)?.permissions();
            perms.set_mode(0o755);
            std::fs::set_permissions(&script_path, perms)?;
        }

        // Note: This server doesn't actually start a process.
        // Instead, tests should use the script path to start the server themselves.
        // For now, we just return a handle that does nothing.

        let handle = Some(thread::spawn(move || {
            // Wait for stop signal
            let _ = stop_rx.recv();
        }));

        Ok(Self { handle, stop_tx })
    }

    /// Get the path to the fake LSP server script
    pub fn script_path() -> std::path::PathBuf {
        std::env::temp_dir().join("fake_lsp_server.sh")
    }

    /// Spawn a blocking fake LSP server that never responds to requests
    ///
    /// This version simulates a completely stuck/unresponsive LSP server.
    /// It responds to the initialize request so the client thinks it's working,
    /// but then blocks forever on all other requests without responding.
    /// This is useful for testing that the editor UI remains responsive even
    /// when the LSP server is completely stuck.
    pub fn spawn_blocking() -> std::io::Result<Self> {
        let (stop_tx, stop_rx) = mpsc::channel();

        // Create a Bash script that acts as a fake LSP server that blocks forever
        let script = r#"#!/bin/bash

# Function to read a message
read_message() {
    # Read headers
    local content_length=0
    while IFS=: read -r key value; do
        key=$(echo "$key" | tr -d '\r\n')
        value=$(echo "$value" | tr -d '\r\n ')
        if [ "$key" = "Content-Length" ]; then
            content_length=$value
        fi
        # Empty line marks end of headers
        if [ -z "$key" ]; then
            break
        fi
    done

    # Read content
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
    # Read incoming message
    msg=$(read_message)

    if [ -z "$msg" ]; then
        break
    fi

    # Extract method from JSON
    method=$(echo "$msg" | grep -o '"method":"[^"]*"' | cut -d'"' -f4)
    msg_id=$(echo "$msg" | grep -o '"id":[0-9]*' | cut -d':' -f2)

    case "$method" in
        "initialize")
            # Send initialize response - this is the only thing that works
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":{"capabilities":{"completionProvider":{"triggerCharacters":[".",":",":"]},"definitionProvider":true,"textDocumentSync":1}}}'
            ;;
        "shutdown")
            # Respond to shutdown so we can clean up
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":null}'
            break
            ;;
        *)
            # For ALL other requests (didSave, didOpen, didChange, completion, etc.)
            # we simply do NOTHING - no response, just block forever
            # This simulates a completely stuck LSP server
            ;;
    esac
done
"#;

        // Write script to a temporary file
        let script_path = std::env::temp_dir().join("fake_lsp_server_blocking.sh");
        std::fs::write(&script_path, script)?;

        // Make it executable
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let mut perms = std::fs::metadata(&script_path)?.permissions();
            perms.set_mode(0o755);
            std::fs::set_permissions(&script_path, perms)?;
        }

        let handle = Some(thread::spawn(move || {
            // Wait for stop signal
            let _ = stop_rx.recv();
        }));

        Ok(Self { handle, stop_tx })
    }

    /// Get the path to the blocking fake LSP server script
    pub fn blocking_script_path() -> std::path::PathBuf {
        std::env::temp_dir().join("fake_lsp_server_blocking.sh")
    }

    /// Spawn a fake LSP server that generates many diagnostics
    ///
    /// This version responds to didChange notifications with a large number of diagnostics
    /// across many lines. This is useful for testing performance with many diagnostics.
    pub fn spawn_many_diagnostics(diagnostic_count: usize) -> std::io::Result<Self> {
        let (stop_tx, stop_rx) = mpsc::channel();

        // Generate JSON for many diagnostics
        let mut diagnostics_json = String::from("[");
        for i in 0..diagnostic_count {
            if i > 0 {
                diagnostics_json.push(',');
            }
            // Spread diagnostics across different lines
            let line = i / 2; // 2 diagnostics per line
            let char_start = (i % 2) * 10;
            let char_end = char_start + 5;
            diagnostics_json.push_str(&format!(
                r#"{{"range":{{"start":{{"line":{},"character":{}}},"end":{{"line":{},"character":{}}}}},"severity":1,"message":"Error {} from fake LSP"}}"#,
                line, char_start, line, char_end, i
            ));
        }
        diagnostics_json.push(']');

        // Create a Bash script that sends many diagnostics on didChange
        let script = format!(
            r#"#!/bin/bash

# Function to read a message
read_message() {{
    # Read headers
    local content_length=0
    while IFS=: read -r key value; do
        key=$(echo "$key" | tr -d '\r\n')
        value=$(echo "$value" | tr -d '\r\n ')
        if [ "$key" = "Content-Length" ]; then
            content_length=$value
        fi
        # Empty line marks end of headers
        if [ -z "$key" ]; then
            break
        fi
    done

    # Read content
    if [ $content_length -gt 0 ]; then
        dd bs=1 count=$content_length 2>/dev/null
    fi
}}

# Function to send a message
send_message() {{
    local message="$1"
    local length=${{#message}}
    echo -en "Content-Length: $length\r\n\r\n$message"
}}

# Main loop
while true; do
    # Read incoming message
    msg=$(read_message)

    if [ -z "$msg" ]; then
        break
    fi

    # Extract method from JSON
    method=$(echo "$msg" | grep -o '"method":"[^"]*"' | cut -d'"' -f4)
    msg_id=$(echo "$msg" | grep -o '"id":[0-9]*' | cut -d':' -f2)

    case "$method" in
        "initialize")
            # Send initialize response
            send_message '{{"jsonrpc":"2.0","id":'$msg_id',"result":{{"capabilities":{{"textDocumentSync":2}}}}}}'
            ;;
        "textDocument/didChange"|"textDocument/didOpen")
            # Send many diagnostics on every change
            uri=$(echo "$msg" | grep -o '"uri":"[^"]*"' | head -1 | cut -d'"' -f4)
            send_message '{{"jsonrpc":"2.0","method":"textDocument/publishDiagnostics","params":{{"uri":"'$uri'","diagnostics":{diagnostics}}}}}'
            ;;
        "shutdown")
            send_message '{{"jsonrpc":"2.0","id":'$msg_id',"result":null}}'
            break
            ;;
    esac
done
"#,
            diagnostics = diagnostics_json
        );

        // Write script to a temporary file
        let script_path = std::env::temp_dir().join("fake_lsp_server_many_diags.sh");
        std::fs::write(&script_path, script)?;

        // Make it executable
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let mut perms = std::fs::metadata(&script_path)?.permissions();
            perms.set_mode(0o755);
            std::fs::set_permissions(&script_path, perms)?;
        }

        let handle = Some(thread::spawn(move || {
            // Wait for stop signal
            let _ = stop_rx.recv();
        }));

        Ok(Self { handle, stop_tx })
    }

    /// Get the path to the many-diagnostics fake LSP server script
    pub fn many_diagnostics_script_path() -> std::path::PathBuf {
        std::env::temp_dir().join("fake_lsp_server_many_diags.sh")
    }

    /// Spawn a fake LSP server that sends progress notifications
    ///
    /// This version sends progress notifications (begin, report, end) after initialization.
    /// This is useful for testing LSP progress display in the status bar.
    pub fn spawn_with_progress() -> std::io::Result<Self> {
        let (stop_tx, stop_rx) = mpsc::channel();

        // Create a Bash script that sends progress notifications
        let script = r#"#!/bin/bash

# Function to read a message
read_message() {
    # Read headers
    local content_length=0
    while IFS=: read -r key value; do
        key=$(echo "$key" | tr -d '\r\n')
        value=$(echo "$value" | tr -d '\r\n ')
        if [ "$key" = "Content-Length" ]; then
            content_length=$value
        fi
        # Empty line marks end of headers
        if [ -z "$key" ]; then
            break
        fi
    done

    # Read content
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
    # Read incoming message
    msg=$(read_message)

    if [ -z "$msg" ]; then
        break
    fi

    # Extract method from JSON
    method=$(echo "$msg" | grep -o '"method":"[^"]*"' | cut -d'"' -f4)
    msg_id=$(echo "$msg" | grep -o '"id":[0-9]*' | cut -d':' -f2)

    case "$method" in
        "initialize")
            # Send initialize response with workDoneProgress support
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":{"capabilities":{"textDocumentSync":1}}}'
            ;;
        "initialized")
            # After initialized notification, send progress notifications
            # Send progress begin
            send_message '{"jsonrpc":"2.0","method":"$/progress","params":{"token":"indexing-1","value":{"kind":"begin","title":"Indexing","message":"Loading workspace","percentage":0}}}'

            # Small delay to simulate work
            sleep 0.1

            # Send progress report (25%)
            send_message '{"jsonrpc":"2.0","method":"$/progress","params":{"token":"indexing-1","value":{"kind":"report","message":"Analyzing dependencies","percentage":25}}}'

            sleep 0.1

            # Send progress report (50%)
            send_message '{"jsonrpc":"2.0","method":"$/progress","params":{"token":"indexing-1","value":{"kind":"report","message":"Building index","percentage":50}}}'

            sleep 0.1

            # Send progress report (75%)
            send_message '{"jsonrpc":"2.0","method":"$/progress","params":{"token":"indexing-1","value":{"kind":"report","message":"Finalizing","percentage":75}}}'

            sleep 0.1

            # Send progress end
            send_message '{"jsonrpc":"2.0","method":"$/progress","params":{"token":"indexing-1","value":{"kind":"end","message":"Done"}}}'
            ;;
        "shutdown")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":null}'
            break
            ;;
    esac
done
"#;

        // Write script to a temporary file
        let script_path = std::env::temp_dir().join("fake_lsp_server_progress.sh");
        std::fs::write(&script_path, script)?;

        // Make it executable
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let mut perms = std::fs::metadata(&script_path)?.permissions();
            perms.set_mode(0o755);
            std::fs::set_permissions(&script_path, perms)?;
        }

        let handle = Some(thread::spawn(move || {
            // Wait for stop signal
            let _ = stop_rx.recv();
        }));

        Ok(Self { handle, stop_tx })
    }

    /// Get the path to the progress fake LSP server script
    pub fn progress_script_path() -> std::path::PathBuf {
        std::env::temp_dir().join("fake_lsp_server_progress.sh")
    }

    /// Spawn a fake LSP server that crashes after initialization
    ///
    /// This version initializes successfully but then crashes (exits with non-zero)
    /// after receiving any subsequent request. This is useful for testing LSP server
    /// crash detection and auto-restart functionality.
    pub fn spawn_crashing() -> std::io::Result<Self> {
        let (stop_tx, stop_rx) = mpsc::channel();

        // Create a Bash script that crashes after init
        let script = r#"#!/bin/bash

# Function to read a message
read_message() {
    # Read headers
    local content_length=0
    while IFS=: read -r key value; do
        key=$(echo "$key" | tr -d '\r\n')
        value=$(echo "$value" | tr -d '\r\n ')
        if [ "$key" = "Content-Length" ]; then
            content_length=$value
        fi
        # Empty line marks end of headers
        if [ -z "$key" ]; then
            break
        fi
    done

    # Read content
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

# Track whether we've initialized
initialized=0

# Main loop
while true; do
    # Read incoming message
    msg=$(read_message)

    if [ -z "$msg" ]; then
        break
    fi

    # Extract method from JSON
    method=$(echo "$msg" | grep -o '"method":"[^"]*"' | cut -d'"' -f4)
    msg_id=$(echo "$msg" | grep -o '"id":[0-9]*' | cut -d':' -f2)

    case "$method" in
        "initialize")
            # Send initialize response
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":{"capabilities":{"textDocumentSync":1,"completionProvider":{"triggerCharacters":["."]}}}}'
            ;;
        "initialized")
            initialized=1
            ;;
        "textDocument/didOpen"|"textDocument/didChange")
            # After receiving a document notification, crash!
            if [ $initialized -eq 1 ]; then
                # Exit with error to simulate crash
                exit 1
            fi
            ;;
        "shutdown")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":null}'
            break
            ;;
    esac
done
"#;

        // Write script to a temporary file
        let script_path = std::env::temp_dir().join("fake_lsp_server_crashing.sh");
        std::fs::write(&script_path, script)?;

        // Make it executable
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let mut perms = std::fs::metadata(&script_path)?.permissions();
            perms.set_mode(0o755);
            std::fs::set_permissions(&script_path, perms)?;
        }

        let handle = Some(thread::spawn(move || {
            // Wait for stop signal
            let _ = stop_rx.recv();
        }));

        Ok(Self { handle, stop_tx })
    }

    /// Get the path to the crashing fake LSP server script
    pub fn crashing_script_path() -> std::path::PathBuf {
        std::env::temp_dir().join("fake_lsp_server_crashing.sh")
    }

    /// Spawn a fake LSP server that supports pull diagnostics (textDocument/diagnostic)
    ///
    /// This version responds to textDocument/diagnostic requests with diagnostic results.
    /// It also tracks result_id for incremental updates and returns "unchanged" responses
    /// when the same result_id is passed. This is useful for testing LSP 3.17+ pull
    /// diagnostics functionality.
    pub fn spawn_with_pull_diagnostics() -> std::io::Result<Self> {
        let (stop_tx, stop_rx) = mpsc::channel();

        // Create a Bash script that supports pull diagnostics
        let script = r#"#!/bin/bash

# Function to read a message
read_message() {
    # Read headers
    local content_length=0
    while IFS=: read -r key value; do
        key=$(echo "$key" | tr -d '\r\n')
        value=$(echo "$value" | tr -d '\r\n ')
        if [ "$key" = "Content-Length" ]; then
            content_length=$value
        fi
        # Empty line marks end of headers
        if [ -z "$key" ]; then
            break
        fi
    done

    # Read content
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

# Track result_id for incremental updates
# Using a simple counter as the result_id
result_id_counter=1

# Store the last result_id we sent per URI (using a simple file-based approach)
last_result_id_file="/tmp/fake_lsp_result_ids"
echo "" > "$last_result_id_file"

# Main loop
while true; do
    # Read incoming message
    msg=$(read_message)

    if [ -z "$msg" ]; then
        break
    fi

    # Extract method from JSON
    method=$(echo "$msg" | grep -o '"method":"[^"]*"' | cut -d'"' -f4)
    msg_id=$(echo "$msg" | grep -o '"id":[0-9]*' | cut -d':' -f2)

    case "$method" in
        "initialize")
            # Send initialize response with diagnosticProvider capability
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":{"capabilities":{"textDocumentSync":1,"diagnosticProvider":{"interFileDependencies":false,"workspaceDiagnostics":false}}}}'
            ;;
        "initialized")
            # No response needed for notification
            ;;
        "textDocument/didOpen"|"textDocument/didChange"|"textDocument/didSave")
            # No response for notifications - client will use pull diagnostics
            ;;
        "textDocument/diagnostic")
            # Handle pull diagnostics request
            uri=$(echo "$msg" | grep -o '"uri":"[^"]*"' | head -1 | cut -d'"' -f4)
            prev_result_id=$(echo "$msg" | grep -o '"previousResultId":"[^"]*"' | cut -d'"' -f4)

            # Check if we have a stored result_id for this URI
            stored_id=$(grep "^$uri:" "$last_result_id_file" | cut -d':' -f2)

            # If previousResultId matches our stored id, return unchanged
            if [ -n "$prev_result_id" ] && [ "$prev_result_id" = "$stored_id" ]; then
                # Return unchanged response
                send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":{"kind":"unchanged","resultId":"'$stored_id'"}}'
            else
                # Return full diagnostics with new result_id
                new_result_id="result-$result_id_counter"
                result_id_counter=$((result_id_counter + 1))

                # Store the new result_id
                sed -i "/^$uri:/d" "$last_result_id_file" 2>/dev/null || true
                echo "$uri:$new_result_id" >> "$last_result_id_file"

                # Send full diagnostic response with a sample diagnostic
                send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":{"kind":"full","resultId":"'$new_result_id'","items":[{"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":5}},"severity":2,"message":"Pull diagnostic warning from fake LSP"}]}}'
            fi
            ;;
        "shutdown")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":null}'
            # Clean up
            rm -f "$last_result_id_file"
            break
            ;;
    esac
done
"#;

        // Write script to a temporary file
        let script_path = std::env::temp_dir().join("fake_lsp_server_pull_diag.sh");
        std::fs::write(&script_path, script)?;

        // Make it executable
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let mut perms = std::fs::metadata(&script_path)?.permissions();
            perms.set_mode(0o755);
            std::fs::set_permissions(&script_path, perms)?;
        }

        let handle = Some(thread::spawn(move || {
            // Wait for stop signal
            let _ = stop_rx.recv();
        }));

        Ok(Self { handle, stop_tx })
    }

    /// Get the path to the pull diagnostics fake LSP server script
    pub fn pull_diagnostics_script_path() -> std::path::PathBuf {
        std::env::temp_dir().join("fake_lsp_server_pull_diag.sh")
    }

    /// Spawn a fake LSP server that supports inlay hints (textDocument/inlayHint)
    ///
    /// This version responds to textDocument/inlayHint requests with sample hints.
    /// This is useful for testing LSP 3.17+ inlay hints functionality.
    pub fn spawn_with_inlay_hints() -> std::io::Result<Self> {
        let (stop_tx, stop_rx) = mpsc::channel();

        // Create a Bash script that supports inlay hints
        let script = r#"#!/bin/bash

# Function to read a message
read_message() {
    # Read headers
    local content_length=0
    while IFS=: read -r key value; do
        key=$(echo "$key" | tr -d '\r\n')
        value=$(echo "$value" | tr -d '\r\n ')
        if [ "$key" = "Content-Length" ]; then
            content_length=$value
        fi
        # Empty line marks end of headers
        if [ -z "$key" ]; then
            break
        fi
    done

    # Read content
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
    # Read incoming message
    msg=$(read_message)

    if [ -z "$msg" ]; then
        break
    fi

    # Extract method from JSON
    method=$(echo "$msg" | grep -o '"method":"[^"]*"' | cut -d'"' -f4)
    msg_id=$(echo "$msg" | grep -o '"id":[0-9]*' | cut -d':' -f2)

    case "$method" in
        "initialize")
            # Send initialize response with inlayHintProvider capability
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":{"capabilities":{"textDocumentSync":1,"inlayHintProvider":true}}}'
            ;;
        "initialized")
            # No response needed for notification
            ;;
        "textDocument/didOpen"|"textDocument/didChange"|"textDocument/didSave")
            # No response for notifications
            ;;
        "textDocument/inlayHint")
            # Return sample inlay hints
            # Type hint at position (0, 5) - after "let x"
            # Parameter hint at position (1, 4) - before function argument
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":[{"position":{"line":0,"character":5},"label":"i32","kind":1},{"position":{"line":1,"character":4},"label":"count","kind":2}]}'
            ;;
        "shutdown")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":null}'
            break
            ;;
    esac
done
"#;

        // Write script to a temporary file
        let script_path = std::env::temp_dir().join("fake_lsp_server_inlay_hints.sh");
        std::fs::write(&script_path, script)?;

        // Make it executable
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let mut perms = std::fs::metadata(&script_path)?.permissions();
            perms.set_mode(0o755);
            std::fs::set_permissions(&script_path, perms)?;
        }

        let handle = Some(thread::spawn(move || {
            // Wait for stop signal
            let _ = stop_rx.recv();
        }));

        Ok(Self { handle, stop_tx })
    }

    /// Get the path to the inlay hints fake LSP server script
    pub fn inlay_hints_script_path() -> std::path::PathBuf {
        std::env::temp_dir().join("fake_lsp_server_inlay_hints.sh")
    }

    /// Stop the server
    pub fn stop(&mut self) {
        let _ = self.stop_tx.send(());
        if let Some(handle) = self.handle.take() {
            let _ = handle.join();
        }
    }
}

impl Drop for FakeLspServer {
    fn drop(&mut self) {
        self.stop();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fake_lsp_server_creation() {
        let server = FakeLspServer::spawn();
        assert!(server.is_ok());
    }

    #[test]
    fn test_script_path_exists() {
        let _server = FakeLspServer::spawn().unwrap();
        let path = FakeLspServer::script_path();
        assert!(path.exists());
    }
}
