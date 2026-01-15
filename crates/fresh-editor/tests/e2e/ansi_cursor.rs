//! Test cursor visibility with ANSI escape codes in file content
//!
//! Bug: When a file starts with ANSI escape codes (like log files with colors),
//! the hardware cursor position is incorrectly set to (0, 0) instead of the
//! actual cursor position in the content area.

use crate::common::harness::EditorTestHarness;
use tempfile::TempDir;

/// Compare cursor position between ANSI and plain text files.
/// Both should have the cursor on the first character of content (row 2, after gutter).
///
/// This test reproduces a bug where files starting with ANSI escape codes
/// cause the cursor to be positioned at (0, 0) instead of the correct location.
#[test]
fn test_cursor_ansi_vs_plain_comparison() {
    eprintln!("[TEST] Starting test_cursor_ansi_vs_plain_comparison");
    let temp_dir = TempDir::new().unwrap();

    // Create both files
    let plain_path = temp_dir.path().join("plain.txt");
    let ansi_path = temp_dir.path().join("ansi.log");

    eprintln!("[TEST] Writing files...");
    std::fs::write(&plain_path, "Hello world\n").unwrap();
    // ANSI content: \x1b[2m is "dim", \x1b[0m is "reset"
    std::fs::write(&ansi_path, "\x1b[2m2025-11-23T17:51:33Z\x1b[0m INFO test\n").unwrap();

    // Test plain text first (baseline)
    eprintln!("[TEST] Creating first harness for plain file...");
    let mut plain_harness = EditorTestHarness::new(80, 24).unwrap();
    eprintln!("[TEST] Opening plain file...");
    plain_harness.open_file(&plain_path).unwrap();
    eprintln!("[TEST] Rendering plain file...");
    plain_harness.render().unwrap();
    eprintln!("[TEST] Getting plain cursor position...");
    let plain_cursor_pos = plain_harness.screen_cursor_position();
    eprintln!("[TEST] Plain cursor pos: {:?}", plain_cursor_pos);

    // Drop the first harness before creating the second to avoid multiple plugin threads
    eprintln!("[TEST] Dropping first harness...");
    drop(plain_harness);

    // Test ANSI file
    eprintln!("[TEST] Creating second harness for ANSI file...");
    let mut ansi_harness = EditorTestHarness::new(80, 24).unwrap();
    eprintln!("[TEST] Opening ANSI file...");
    ansi_harness.open_file(&ansi_path).unwrap();
    eprintln!("[TEST] Rendering ANSI file...");
    ansi_harness.render().unwrap();
    eprintln!("[TEST] Getting ANSI cursor position...");
    let ansi_cursor_pos = ansi_harness.screen_cursor_position();
    eprintln!("[TEST] ANSI cursor pos: {:?}", ansi_cursor_pos);

    // The Y coordinate (row) should be the same for both - cursor on content row
    assert_eq!(
        plain_cursor_pos.1, ansi_cursor_pos.1,
        "Cursor row should be the same for plain ({:?}) and ANSI ({:?}) files. \
         ANSI cursor is at (0,0) which indicates a bug in cursor position calculation \
         when file starts with escape codes.",
        plain_cursor_pos, ansi_cursor_pos
    );
}
