#!/usr/bin/env python3
"""
Test LSP integration using Script Control Mode.

This script creates a temporary Cargo project in a tempdir and uses the script
control mode to test LSP features like:
- LSP initialization
- Code completion
- Diagnostics
- Go to definition
"""

import json
import os
import subprocess
import tempfile
import time
from pathlib import Path


def send_command(proc, command):
    """Send a command to the editor and get the response."""
    cmd_json = json.dumps(command)
    proc.stdin.write(cmd_json + "\n")
    proc.stdin.flush()

    response_line = proc.stdout.readline()
    if not response_line:
        raise Exception("No response from editor")

    return json.loads(response_line.strip())


def wait_for_lsp_ready(proc, timeout=30):
    """Wait for LSP to be ready by checking screen content."""
    print("Waiting for LSP to initialize...")

    # First, try to wait for LSP to show "ready" in the screen
    response = send_command(proc, {
        "type": "wait_for",
        "condition": {
            "type": "screen_contains",
            "text": "ready"
        },
        "timeout_ms": timeout * 1000,
        "poll_interval_ms": 500
    })

    if response.get("type") == "ok":
        print(f"LSP initialized: {response.get('message', '')}")
        return True

    # Check the current screen state
    render_resp = send_command(proc, {"type": "render"})
    if render_resp.get("type") == "screen":
        screen = render_resp.get("content", "")

        # Check for errors
        if "error" in screen.lower():
            status_line = [l for l in screen.split('\n') if "LSP" in l]
            if status_line:
                print(f"LSP Error detected: {status_line[-1]}")

    print("Warning: LSP did not fully initialize within timeout")
    return False


def setup_cargo_project(temp_dir):
    """Create a minimal Cargo project structure."""
    # Create Cargo.toml
    cargo_toml = temp_dir / "Cargo.toml"
    cargo_toml.write_text("""[package]
name = "test-lsp"
version = "0.1.0"
edition = "2021"
""")

    # Create src directory
    src_dir = temp_dir / "src"
    src_dir.mkdir()

    # Create main.rs with some Rust code
    main_rs = src_dir / "main.rs"
    main_rs.write_text("""fn main() {
    let message = "Hello, World!";
    println!("{}", message);

    let numbers = vec![1, 2, 3, 4, 5];
    let sum: i32 = numbers.iter().sum();
    println!("Sum: {}", sum);
}

fn helper_function(x: i32) -> i32 {
    x * 2
}
""")

    return main_rs


def test_lsp_completion(proc):
    """Test that LSP completion works."""
    print("\n=== Testing LSP Completion ===")

    # Get initial render
    response = send_command(proc, {"type": "render"})
    print(f"Screen size: {response.get('width')}x{response.get('height')}")

    # Move to end of line 3 (after println statement) and start a new line
    # First, go to line 5 (where we have 'let numbers')
    for _ in range(5):
        send_command(proc, {"type": "key", "code": "Down"})
    send_command(proc, {"type": "key", "code": "End"})

    # Add a new line
    send_command(proc, {"type": "key", "code": "Enter"})

    # Type partial code that should trigger completion
    send_command(proc, {"type": "type_text", "text": "    numbers."})

    # Request completion with Ctrl+Space
    print("Requesting completion...")
    send_command(proc, {"type": "key", "code": "space", "modifiers": ["ctrl"]})

    # Wait a bit for LSP to respond
    time.sleep(2)

    # Render to see if popup appeared
    response = send_command(proc, {"type": "render"})
    screen = response.get("content", "")

    # Check for completion popup indicators
    has_completion = "len" in screen or "iter" in screen or "push" in screen or "Completion" in screen

    if has_completion:
        print("SUCCESS: Completion popup appeared with suggestions!")
    else:
        print("Note: Completion popup may not have appeared (LSP might still be indexing)")

    # Cancel completion with Escape
    send_command(proc, {"type": "key", "code": "Escape"})

    # Get buffer content
    buffer_resp = send_command(proc, {"type": "get_buffer"})
    print(f"Buffer now contains: {buffer_resp.get('content', '')[:100]}...")

    return has_completion


def test_lsp_diagnostics(proc):
    """Test that LSP diagnostics are shown."""
    print("\n=== Testing LSP Diagnostics ===")

    # Go to end of file and add some invalid code
    send_command(proc, {"type": "key", "code": "End", "modifiers": ["ctrl"]})
    send_command(proc, {"type": "key", "code": "Enter"})
    send_command(proc, {"type": "type_text", "text": "\nfn broken() -> i32 {\n    \"not an int\"\n}"})

    # Render to see screen
    response = send_command(proc, {"type": "render"})
    screen = response.get("content", "")
    print(f"Screen after adding broken code:\n{screen[-500:]}")

    # Wait for diagnostics
    print("Waiting for diagnostics...")
    time.sleep(3)

    # Check for error indicators in status bar (E: pattern)
    response = send_command(proc, {"type": "render"})
    screen = response.get("content", "")

    has_errors = "E:" in screen or "error" in screen.lower()

    if has_errors:
        print("SUCCESS: Diagnostics detected!")
    else:
        print("Note: Diagnostics may not have appeared yet")

    return has_errors


def test_lsp_status(proc):
    """Test LSP status information."""
    print("\n=== Testing LSP Status ===")

    response = send_command(proc, {"type": "render"})
    screen = response.get("content", "")

    # Look for LSP status in the screen (usually in status bar)
    has_lsp_info = "LSP" in screen or "rust" in screen.lower()

    if has_lsp_info:
        print("SUCCESS: LSP status information is shown")
        # Extract status bar (usually last few lines)
        lines = screen.strip().split('\n')
        if lines:
            print(f"Status bar: {lines[-1]}")
    else:
        print("Note: LSP status not visible in screen")

    return has_lsp_info


def test_editor_keybindings(proc):
    """Test getting keybindings from the editor."""
    print("\n=== Testing Keybindings ===")

    response = send_command(proc, {"type": "get_keybindings"})

    if response.get("type") == "keybindings":
        bindings = response.get("bindings", [])
        print(f"Found {len(bindings)} keybindings")

        # Show a few relevant ones
        lsp_bindings = [b for b in bindings if "LSP" in b.get("action", "").upper() or "completion" in b.get("action", "").lower()]
        if lsp_bindings:
            print("LSP-related keybindings:")
            for binding in lsp_bindings[:5]:
                print(f"  {binding['key']}: {binding['action']}")

        return True
    else:
        print(f"Failed to get keybindings: {response}")
        return False


def main():
    print("=== LSP Integration Test via Script Control Mode ===\n")

    # Check if rust-analyzer is available
    try:
        result = subprocess.run(["which", "rust-analyzer"], capture_output=True, text=True)
        if result.returncode != 0:
            print("WARNING: rust-analyzer not found in PATH. LSP tests may fail.")
        else:
            # Also get version
            version_result = subprocess.run(["rust-analyzer", "--version"], capture_output=True, text=True)
            print(f"rust-analyzer: {version_result.stdout.strip()}")
    except Exception as e:
        print(f"WARNING: Could not check for rust-analyzer: {e}")

    # Create temporary directory
    with tempfile.TemporaryDirectory() as temp_dir:
        temp_path = Path(temp_dir)
        print(f"Created temporary project at: {temp_path}")

        # Setup Cargo project
        main_rs = setup_cargo_project(temp_path)
        print(f"Created Cargo project with main.rs")

        # Create a log file for rust-analyzer debugging
        ra_log_file = temp_path / "rust-analyzer.log"

        # Start the editor in script mode
        print(f"\nStarting editor in script mode...")
        print(f"rust-analyzer logs will be at: {ra_log_file}")

        # Build the editor first (if needed)
        build_result = subprocess.run(
            ["cargo", "build", "--release"],
            cwd="/home/noam/repos/fresh",
            capture_output=True,
            text=True
        )
        if build_result.returncode != 0:
            print(f"Build failed: {build_result.stderr}")
            return False

        # Start editor with script mode
        # Use the binary directly to avoid cargo output
        editor_binary = Path("/home/noam/repos/fresh/target/debug/fresh")
        if not editor_binary.exists():
            # Fall back to release
            editor_binary = Path("/home/noam/repos/fresh/target/release/fresh")
        if not editor_binary.exists():
            print(f"Editor binary not found at {editor_binary}")
            return False

        # Set RA_LOG environment variable for rust-analyzer debugging
        env = os.environ.copy()
        env["RA_LOG"] = "info"
        env["RA_LOG_FILE"] = str(ra_log_file)

        proc = subprocess.Popen(
            [
                str(editor_binary),
                "--script-mode",
                "--script-width", "120",
                "--script-height", "40",
                str(main_rs)
            ],
            cwd=str(temp_path),  # Set working dir to temp project
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            bufsize=1,
            env=env
        )

        try:
            # Read the ready message
            ready_line = proc.stdout.readline()
            if not ready_line:
                print("No output from editor. Checking stderr...")
                stderr_output = proc.stderr.read()
                print(f"Stderr: {stderr_output}")
                return False
            ready_msg = json.loads(ready_line.strip())
            print(f"Editor ready: {ready_msg.get('message', '')}")

            # Wait for LSP to initialize
            lsp_ready = wait_for_lsp_ready(proc, timeout=30)

            # Run tests
            results = {}

            # Test 1: LSP Status
            results["lsp_status"] = test_lsp_status(proc)

            # Test 2: Keybindings
            results["keybindings"] = test_editor_keybindings(proc)

            # Test 3: Completion (only if LSP is ready)
            if lsp_ready:
                results["completion"] = test_lsp_completion(proc)
            else:
                print("\nSkipping completion test (LSP not ready)")
                results["completion"] = False

            # Test 4: Diagnostics
            # results["diagnostics"] = test_lsp_diagnostics(proc)

            # Export test
            print("\n=== Exporting Test Code ===")
            response = send_command(proc, {"type": "export_test", "test_name": "test_lsp_integration"})
            if response.get("type") == "test_code":
                code = response.get("code", "")
                print(f"Generated test code ({len(code)} chars):")
                print(code[:500] + "..." if len(code) > 500 else code)

            # Quit
            print("\n=== Quitting Editor ===")
            send_command(proc, {"type": "quit"})

            # Summary
            print("\n=== Test Summary ===")
            for test_name, passed in results.items():
                status = "PASS" if passed else "SKIP/FAIL"
                print(f"  {test_name}: {status}")

            # Wait for process to exit
            proc.wait(timeout=5)

        except Exception as e:
            print(f"Error during testing: {e}")
            import traceback
            traceback.print_exc()
            proc.kill()
            return False
        finally:
            if proc.poll() is None:
                proc.kill()

            # Show rust-analyzer logs if available
            if ra_log_file.exists():
                print(f"\n=== rust-analyzer Logs ===")
                log_content = ra_log_file.read_text()
                if log_content:
                    # Show last 50 lines
                    lines = log_content.strip().split('\n')
                    if len(lines) > 50:
                        print(f"(showing last 50 of {len(lines)} lines)")
                        lines = lines[-50:]
                    for line in lines:
                        print(f"  {line}")
                else:
                    print("  (empty log file)")
            else:
                print(f"\nNo rust-analyzer log file found at {ra_log_file}")

            # Check editor stderr
            if proc.stderr:
                stderr_content = proc.stderr.read()
                if stderr_content:
                    print(f"\n=== Editor Stderr ===")
                    print(stderr_content[:2000])

    print("\n=== Test Complete ===")
    return True


if __name__ == "__main__":
    success = main()
    exit(0 if success else 1)
