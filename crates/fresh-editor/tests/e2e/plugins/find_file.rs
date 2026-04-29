//! E2E tests for the Quick Open feature (native Rust implementation)
//!
//! Tests the Quick Open functionality which provides:
//! - Platform-agnostic file finding (git -> fd -> find -> manual)
//! - Unified finder with prefix mode switching (>, #, :)
//! - Buffer finder
//! - Frecency-based ranking
//!
//! Note: Quick Open now defaults to command mode (starts with ">")

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

// ============================================================================
// Command Mode Tests (> prefix - default mode)
// ============================================================================

/// Test command mode: Quick Open starts in command mode with > prefix
#[test]
fn test_quick_open_starts_in_command_mode() {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 30, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    let test_file = project_root.join("test.txt");
    fs::write(&test_file, "Test content\n").unwrap();
    harness.open_file(&test_file).unwrap();
    harness.render().unwrap();

    // Open Quick Open (defaults to command mode with >)
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should show commands immediately (already has > prefix)
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            // Commands should be visible
            s.contains("Save") || s.contains("Open") || s.contains("Close") || s.contains("Quit")
        })
        .unwrap();

    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// Test command mode: type command -> press Enter -> command executes
#[test]
fn test_quick_open_command_execute() {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 30, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    let test_file = project_root.join("test.txt");
    fs::write(&test_file, "Test content\n").unwrap();
    harness.open_file(&test_file).unwrap();
    harness.render().unwrap();

    // Open Quick Open (already in command mode)
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Type command to filter - look for "Go to Line" command
    harness.type_text("go to line").unwrap();

    // Should show filtered command
    harness
        .wait_until(|h| h.screen_to_string().contains("Go to Line"))
        .unwrap();

    // Press Enter to execute the command
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // The "Go to Line" command should open the go-to-line prompt
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("Line:") || s.contains("Go to line")
        })
        .unwrap();

    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// Test command mode: filter commands by typing
#[test]
fn test_quick_open_command_filter() {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 30, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    let test_file = project_root.join("test.txt");
    fs::write(&test_file, "Test\n").unwrap();
    harness.open_file(&test_file).unwrap();
    harness.render().unwrap();

    // Open Quick Open
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Type to filter for "save"
    harness.type_text("save").unwrap();

    // Should show Save command
    harness
        .wait_until(|h| h.screen_to_string().contains("Save"))
        .unwrap();

    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

// ============================================================================
// Go-to-Line Tests (: prefix)
// ============================================================================

/// Test go-to-line: type :N -> press Enter -> cursor moves to line N
#[test]
fn test_quick_open_goto_line_execute() {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 30, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create file with many lines
    let content = (1..=20)
        .map(|i| format!("Line number {}\n", i))
        .collect::<String>();
    let test_file = project_root.join("multiline.txt");
    fs::write(&test_file, &content).unwrap();

    harness.open_file(&test_file).unwrap();
    harness.render().unwrap();

    // Verify we start at line 1
    harness.assert_screen_contains("Ln 1");

    // Open Quick Open
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Clear > and type :15 for go-to-line mode
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text(":15").unwrap();

    // Should show go-to-line suggestion
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("Go to line 15") || s.contains("line 15")
        })
        .unwrap();

    // Press Enter to jump
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Verify cursor is now at line 15
    harness
        .wait_until(|h| h.screen_to_string().contains("Ln 15"))
        .unwrap();

    // Also verify the content of line 15 is visible
    harness.assert_screen_contains("Line number 15");
}

/// Test go-to-line with invalid input shows hint
#[test]
fn test_quick_open_goto_line_invalid() {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 30, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    let test_file = project_root.join("test.txt");
    fs::write(&test_file, "Line 1\nLine 2\nLine 3\n").unwrap();
    harness.open_file(&test_file).unwrap();
    harness.render().unwrap();

    // Open Quick Open and try invalid line number
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text(":abc").unwrap();

    // Should show invalid line hint
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("Invalid") || s.contains("Enter a line number")
        })
        .unwrap();

    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// Test go-to-line with just colon shows hint
#[test]
fn test_quick_open_goto_line_hint() {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 30, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    let test_file = project_root.join("test.txt");
    fs::write(&test_file, "Test\n").unwrap();
    harness.open_file(&test_file).unwrap();
    harness.render().unwrap();

    // Open Quick Open and switch to go-to-line mode
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text(":").unwrap();

    // Should show go-to-line hint
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("line") || s.contains("Line")
        })
        .unwrap();

    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// With relative_line_numbers enabled, Quick Open :N accepts negative numbers
/// to jump relative to current cursor position.
#[test]
fn test_quick_open_goto_line_relative_negative_offset() {
    use fresh::config::Config;

    let mut config = Config::default();
    config.editor.relative_line_numbers = true;

    let mut harness = EditorTestHarness::with_temp_project_and_config(100, 30, config).unwrap();
    let project_root = harness.project_dir().unwrap();

    let test_file = project_root.join("multiline.txt");
    let content = (1..=50)
        .map(|i| format!("Line number {}\n", i))
        .collect::<String>();
    fs::write(&test_file, &content).unwrap();

    harness.open_file(&test_file).unwrap();
    harness.render().unwrap();

    // Verify we start at line 1
    harness.assert_screen_contains("Ln 1");

    // Open Quick Open and type :-5 for relative line
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text(":-5").unwrap();

    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("Go to line -5") || s.contains("-5")
        })
        .unwrap();

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("Ln 1,"))
        .expect(":-5 should jump to line 1 (clamped from -5)");
}

/// With relative_line_numbers enabled, Quick Open :N accepts positive
/// relative offset with + prefix.
#[test]
fn test_quick_open_goto_line_relative_positive_offset() {
    use fresh::config::Config;

    let mut config = Config::default();
    config.editor.relative_line_numbers = true;

    let mut harness = EditorTestHarness::with_temp_project_and_config(100, 30, config).unwrap();
    let project_root = harness.project_dir().unwrap();

    let test_file = project_root.join("multiline.txt");
    let content = (1..=50)
        .map(|i| format!("Line number {}\n", i))
        .collect::<String>();
    fs::write(&test_file, &content).unwrap();

    harness.open_file(&test_file).unwrap();
    harness.render().unwrap();

    // Verify we start at line 1
    harness.assert_screen_contains("Ln 1");

    // Open Quick Open and type :+20 for relative line
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text(":+20").unwrap();

    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("Go to line +20") || s.contains("+20")
        })
        .unwrap();

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("Ln 21,"))
        .expect(":+20 should jump to line 21 (relative to cursor)");
}

/// Issue #1750: Quick Open `:+N`/`:-N` should always be a relative jump,
/// independent of the `relative_line_numbers` display setting.
#[test]
fn test_quick_open_goto_line_signed_is_relative_without_setting() {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 30, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    let test_file = project_root.join("multiline.txt");
    let content = (1..=50)
        .map(|i| format!("Line number {}\n", i))
        .collect::<String>();
    fs::write(&test_file, &content).unwrap();

    harness.open_file(&test_file).unwrap();
    harness.render().unwrap();

    // First jump to line 10 absolutely, then move +5 lines, then -3.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text(":10").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Ln 10,"))
        .unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text(":+5").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Ln 15,"))
        .expect("`:+5` should jump 5 lines forward regardless of the setting");

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text(":-3").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Ln 12,"))
        .expect("`:-3` should jump 3 lines backward regardless of the setting");
}

// ============================================================================
// Buffer Finder Tests (# prefix)
// ============================================================================

/// Test buffer finder shows open buffers
#[test]
fn test_quick_open_buffer_list() {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 30, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create and open multiple files
    let file1 = project_root.join("alpha.txt");
    let file2 = project_root.join("beta.txt");
    fs::write(&file1, "Alpha content\n").unwrap();
    fs::write(&file2, "Beta content\n").unwrap();

    harness.open_file(&file1).unwrap();
    harness.render().unwrap();
    harness.open_file(&file2).unwrap();
    harness.render().unwrap();

    // Open Quick Open and switch to buffer mode
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("#").unwrap();

    // Should show both buffers
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("alpha") || s.contains("beta")
        })
        .unwrap();

    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// Test buffer finder: select buffer switches to it
#[test]
fn test_quick_open_buffer_switch() {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 30, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create files with distinctive content
    let file1 = project_root.join("first.txt");
    let file2 = project_root.join("second.txt");
    fs::write(&file1, "FIRST_FILE_CONTENT\n").unwrap();
    fs::write(&file2, "SECOND_FILE_CONTENT\n").unwrap();

    // Open both files (we'll end up on second)
    harness.open_file(&file1).unwrap();
    harness.render().unwrap();
    harness.open_file(&file2).unwrap();
    harness.render().unwrap();

    // Verify we're on second file
    harness.assert_screen_contains("SECOND_FILE_CONTENT");

    // Open Quick Open, switch to buffer mode, find first file
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("#first").unwrap();

    // Wait for buffer to appear in list
    harness
        .wait_until(|h| h.screen_to_string().contains("first"))
        .unwrap();

    // Select it
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Should now show first file content
    harness
        .wait_until(|h| h.screen_to_string().contains("FIRST_FILE_CONTENT"))
        .unwrap();
}

// ============================================================================
// File Finder Tests (empty prefix)
// ============================================================================

// Note: File finder tests that rely on file discovery are unreliable in temp
// directories because git/fd/find may not find files quickly enough.
// The core file finder functionality is tested via buffer switching which
// uses the same code paths but with already-known buffer data.

// ============================================================================
// Mode Switching Tests
// ============================================================================

/// Test switching between modes by changing prefix
#[test]
fn test_quick_open_mode_switching() {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 30, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    let test_file = project_root.join("test.txt");
    fs::write(&test_file, "Test\n").unwrap();
    harness.open_file(&test_file).unwrap();
    harness.render().unwrap();

    // Open Quick Open (starts in command mode with >)
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Verify command mode (shows commands)
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("Save") || s.contains("Open")
        })
        .unwrap();

    // Switch to go-to-line mode: delete >, type :
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text(":").unwrap();

    // Should show go-to-line hint
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("line") || s.contains("Line")
        })
        .unwrap();

    // Switch to buffer mode: delete :, type #
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("#").unwrap();

    // Should show buffer (test.txt is open)
    harness
        .wait_until(|h| h.screen_to_string().contains("test"))
        .unwrap();

    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// Test Escape cancels Quick Open
#[test]
fn test_quick_open_cancel() {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 30, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    let test_file = project_root.join("test.txt");
    fs::write(&test_file, "Test content\n").unwrap();
    harness.open_file(&test_file).unwrap();
    harness.render().unwrap();

    // Open Quick Open
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Verify prompt is visible
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("Save") || s.contains("Open")
        })
        .unwrap();

    // Press Escape
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Should be back to normal editing
    harness.assert_screen_contains("Test content");
}

/// Test Ctrl+P again closes Quick Open (toggle behavior)
#[test]
fn test_quick_open_toggle() {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 30, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    let test_file = project_root.join("test.txt");
    fs::write(&test_file, "Test content\n").unwrap();
    harness.open_file(&test_file).unwrap();
    harness.render().unwrap();

    // Open Quick Open
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Verify it's open (shows the hints line with >command)
    harness.assert_screen_contains(">command");

    // Press Ctrl+P again to close
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should be closed, back to file content (hints should be gone)
    harness.assert_screen_contains("Test content");
    assert!(
        !harness.screen_to_string().contains(">command"),
        "Quick Open should be closed (hints gone)"
    );
}

/// Test buffer switch with # prefix and autocomplete by buffer name
#[test]
fn test_quick_open_buffer_autocomplete() {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 30, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create and open two files with distinct names
    let file1 = project_root.join("alpha_file.txt");
    let file2 = project_root.join("beta_file.txt");
    fs::write(&file1, "ALPHA_CONTENT\n").unwrap();
    fs::write(&file2, "BETA_CONTENT\n").unwrap();

    harness.open_file(&file1).unwrap();
    harness.open_file(&file2).unwrap();
    harness.render().unwrap();

    // Verify we're on second file (beta)
    harness.assert_screen_contains("BETA_CONTENT");

    // Open Quick Open, clear the > prefix, type # to enter buffer mode with partial name
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("#alp").unwrap();
    harness.render().unwrap();

    // Should show alpha_file in suggestions (matching by name, not index)
    harness.assert_screen_contains("alpha_file");

    // Press Enter to confirm selection (first match)
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should now show alpha file content
    harness.assert_screen_contains("ALPHA_CONTENT");
}

// ============================================================================
// Prefix Probe Tests (file mode — query treated as a literal path prefix)
// ============================================================================

/// End-to-end regression test for the Quick Open prefix probe.
///
/// When the user types a path-like query (e.g. `etc/hosts`), the file
/// provider should check the filesystem directly for files whose path
/// starts with the query and surface them at the top of the suggestion
/// list — even when scattered fuzzy matches elsewhere in the corpus
/// contain the same characters in the same order.
///
/// This reproduces the bug found during manual tmux testing: running
/// the editor rooted at `/` and typing `etc/hosts` should show
/// `etc/hosts`, `etc/host.conf`, `etc/hostname` at the top, not
/// `usr/local/go/.../host_test.go`-style scattered matches.
#[test]
fn test_quick_open_file_prefix_probe_ranks_at_top() {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(120, 40, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Build a small corpus that would confuse a naive fuzzy matcher:
    //   - `etc/hosts` is the literal file we want to surface
    //   - `etc/hosts.allow` and `etc/hosts.deny` are basename-prefix
    //     siblings (basename starts with "hosts") that the probe
    //     should also surface.
    //   - `etc/passwd` lives in the same directory but must not match.
    //   - `usr/local/go/src/net/http/cgi/host_test.go` scatter-matches
    //     the characters of "etc/hosts" across multiple path components
    //     and would rank first under a pure fuzzy scorer.
    fs::create_dir_all(project_root.join("etc")).unwrap();
    fs::write(project_root.join("etc/hosts"), "127.0.0.1 localhost\n").unwrap();
    fs::write(project_root.join("etc/hosts.allow"), "").unwrap();
    fs::write(project_root.join("etc/hosts.deny"), "").unwrap();
    fs::write(project_root.join("etc/passwd"), "").unwrap();

    fs::create_dir_all(project_root.join("usr/local/go/src/net/http/cgi")).unwrap();
    fs::write(
        project_root.join("usr/local/go/src/net/http/cgi/host_test.go"),
        "",
    )
    .unwrap();
    fs::write(
        project_root.join("usr/local/go/src/net/http/cgi/host.go"),
        "",
    )
    .unwrap();

    // Open something so the harness is in a rendered state.
    let readme = project_root.join("README.md");
    fs::write(&readme, "readme\n").unwrap();
    harness.open_file(&readme).unwrap();
    harness.render().unwrap();

    // Open Quick Open (starts in command mode with `>`), delete the
    // prefix to switch to file mode, then type the prefix query.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("etc/hosts").unwrap();

    // Wait for the probe to populate the suggestion list with
    // filesystem-confirmed matches.  `etc/hosts` is the one we most
    // care about — its absence would indicate the probe is broken.
    harness
        .wait_until(|h| h.screen_to_string().contains("etc/hosts"))
        .unwrap();

    let screen = harness.screen_to_string();
    let pos_hosts = screen
        .find("etc/hosts")
        .expect("etc/hosts should appear in suggestions");

    // Prefix-probe siblings should also be present.  They are
    // filesystem-confirmed by the basename prefix scan.
    assert!(
        screen.contains("etc/hosts.allow"),
        "etc/hosts.allow should appear in suggestions, screen:\n{screen}"
    );
    assert!(
        screen.contains("etc/hosts.deny"),
        "etc/hosts.deny should appear in suggestions, screen:\n{screen}"
    );
    // `etc/passwd` lives alongside the matches but its basename does
    // not start with "hosts", so it must be filtered out.
    assert!(
        !screen.contains("etc/passwd"),
        "etc/passwd must not appear for query 'etc/hosts', screen:\n{screen}"
    );

    // The scattered fuzzy match must not rank above the prefix match:
    // if `host_test.go` shows up at all, it must be *after* `etc/hosts`
    // in the rendered list.
    if let Some(pos_scattered) = screen.find("host_test.go") {
        assert!(
            pos_hosts < pos_scattered,
            "etc/hosts should rank above the scattered match, got\n{screen}"
        );
    }

    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

// ============================================================================
// QuickOpenBuffers / QuickOpenFiles Action Tests
// ============================================================================

/// Test QuickOpenBuffers action opens Quick Open in buffer mode (shows ONLY open buffers)
#[test]
fn test_quick_open_buffers_action() {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 30, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create 3 files but only open 2 of them
    let file1 = project_root.join("alpha.txt");
    let file2 = project_root.join("beta.txt");
    let file3 = project_root.join("gamma.txt");
    fs::write(&file1, "Alpha content\n").unwrap();
    fs::write(&file2, "Beta content\n").unwrap();
    fs::write(&file3, "Gamma content\n").unwrap();

    // Only open alpha and beta (NOT gamma)
    harness.open_file(&file1).unwrap();
    harness.render().unwrap();
    harness.open_file(&file2).unwrap();
    harness.render().unwrap();

    // Dispatch the QuickOpenBuffers action directly
    harness
        .editor_mut()
        .dispatch_action_for_tests(fresh::input::keybindings::Action::QuickOpenBuffers);
    harness.render().unwrap();

    // Should show buffer mode (with # prefix)
    harness
        .wait_until(|h| h.screen_to_string().contains('#'))
        .unwrap();

    // Should show both opened buffers
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("alpha") && s.contains("beta")
        })
        .unwrap();

    // gamma should NOT appear since it wasn't opened
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            !s.contains("gamma")
        })
        .unwrap();

    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}

/// Test QuickOpenFiles action opens Quick Open in file mode (shows ALL project files)
#[test]
fn test_quick_open_files_action() {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 30, Default::default()).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create 3 files but don't open any of them
    let file1 = project_root.join("alpha.txt");
    let file2 = project_root.join("beta.txt");
    let file3 = project_root.join("gamma.txt");
    fs::write(&file1, "Alpha content\n").unwrap();
    fs::write(&file2, "Beta content\n").unwrap();
    fs::write(&file3, "Gamma content\n").unwrap();

    // Don't open any files - just stay on the welcome buffer
    harness.render().unwrap();

    // Dispatch the QuickOpenFiles action directly
    harness
        .editor_mut()
        .dispatch_action_for_tests(fresh::input::keybindings::Action::QuickOpenFiles);
    harness.render().unwrap();

    // Should show ALL 3 files even though none are open
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("alpha") && s.contains("beta") && s.contains("gamma")
        })
        .unwrap();

    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
}
