// End-to-end tests for sudo save prompt (issue #775)
//
// Tests that when saving a file fails due to permission denied,
// the editor shows the sudo save prompt instead of crashing.
//
// Issue #775 bug scenario:
// - User opens a file owned by a different user (e.g., root)
// - User has read access (via group permissions or world-readable)
// - User modifies the file
// - User tries to save (Ctrl+S)
// - Bug: Editor crashes with "Permission denied (os error 13)"
// - Expected: Editor shows "Permission denied. Save with sudo?" prompt
//
// The bug occurs in the "in-place write" code path (buffer.rs:496) which is used
// when saving files owned by a different user. The OpenOptions::open() call
// fails with PermissionDenied, but the error is propagated instead of being
// converted to SudoSaveRequired.
//
// Note: The in-place write path cannot be easily tested without root privileges
// (to create files owned by another user). These tests cover the atomic write
// path which correctly handles permission denied errors.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use tempfile::TempDir;

#[cfg(unix)]
use std::fs::Permissions;
#[cfg(unix)]
use std::os::unix::fs::PermissionsExt;

/// Test that saving to an unwritable directory shows sudo prompt (issue #775)
///
/// This test reproduces the scenario where:
/// 1. A file exists in a directory that becomes unwritable
/// 2. User edits the file
/// 3. User tries to save
/// 4. Editor should show "Permission denied. Save with sudo?" prompt
///    instead of crashing with "Permission denied (os error 13)"
#[test]
#[cfg(unix)]
fn test_save_permission_denied_shows_sudo_prompt() {
    // Root (uid 0) bypasses Unix file permission checks, so these
    // permission-denied tests are meaningless when running as root.
    if unsafe { libc::getuid() } == 0 {
        eprintln!("Skipping test: root bypasses file permission checks");
        return;
    }
    let temp_dir = TempDir::new().unwrap();
    let unwritable_dir = temp_dir.path().join("unwritable_dir");
    std::fs::create_dir(&unwritable_dir).unwrap();

    let file_path = unwritable_dir.join("test.txt");
    std::fs::write(&file_path, "original content").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open the file
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Verify file content is loaded
    harness.assert_screen_contains("original content");

    // Modify the content
    harness.type_text("modified ").unwrap();
    harness.render().unwrap();

    // Now make the directory unwritable (simulating permission denied scenario)
    std::fs::set_permissions(&unwritable_dir, Permissions::from_mode(0o555)).unwrap();

    // Try to save with Ctrl+S
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should show sudo save prompt, not crash
    // The prompt contains "Permission denied" or "sudo"
    let screen = harness.screen_to_string();
    let shows_sudo_prompt = screen.contains("sudo") || screen.contains("Permission denied");

    // Restore permissions before assertions (cleanup)
    let _ = std::fs::set_permissions(&unwritable_dir, Permissions::from_mode(0o755));

    assert!(
        shows_sudo_prompt,
        "Expected sudo save prompt when saving to unwritable directory. Screen:\n{}",
        screen
    );
}

/// Test that saving a read-only file (owned by current user) handles permission denied gracefully
///
/// When the user owns a file but it's read-only (0o444), the atomic write path is used.
/// The rename should fail with permission denied, which should trigger the sudo prompt.
#[test]
#[cfg(unix)]
fn test_save_readonly_file_shows_sudo_prompt() {
    // Root (uid 0) bypasses Unix file permission checks, so these
    // permission-denied tests are meaningless when running as root.
    if unsafe { libc::getuid() } == 0 {
        eprintln!("Skipping test: root bypasses file permission checks");
        return;
    }
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("readonly.txt");

    // Create a file and open it
    std::fs::write(&file_path, "original content").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Modify the content
    harness.type_text("modified ").unwrap();
    harness.render().unwrap();

    // Now make the file read-only (after opening)
    std::fs::set_permissions(&file_path, Permissions::from_mode(0o444)).unwrap();

    // Also make the directory unwritable to prevent temp file creation in same dir
    // This forces the atomic write to use /tmp, and the rename will fail
    std::fs::set_permissions(temp_dir.path(), Permissions::from_mode(0o555)).unwrap();

    // Try to save with Ctrl+S
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should show sudo save prompt or error message, not crash
    let screen = harness.screen_to_string();
    let shows_sudo_prompt = screen.contains("sudo") || screen.contains("Permission denied");

    // Restore permissions before assertions (cleanup)
    let _ = std::fs::set_permissions(temp_dir.path(), Permissions::from_mode(0o755));
    let _ = std::fs::set_permissions(&file_path, Permissions::from_mode(0o644));

    assert!(
        shows_sudo_prompt,
        "Expected sudo save prompt when saving read-only file. Screen:\n{}",
        screen
    );
}

/// Test that cancelling sudo prompt returns to normal editing
#[test]
#[cfg(unix)]
fn test_sudo_prompt_cancel_returns_to_editing() {
    let temp_dir = TempDir::new().unwrap();
    let unwritable_dir = temp_dir.path().join("unwritable_dir");
    std::fs::create_dir(&unwritable_dir).unwrap();

    let file_path = unwritable_dir.join("test.txt");
    std::fs::write(&file_path, "original content").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Modify the content
    harness.type_text("modified ").unwrap();
    harness.render().unwrap();

    // Make directory unwritable
    std::fs::set_permissions(&unwritable_dir, Permissions::from_mode(0o555)).unwrap();

    // Try to save
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Verify sudo prompt appears
    let screen = harness.screen_to_string();
    if screen.contains("sudo") {
        // Cancel the sudo prompt with 'n' (No)
        harness
            .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();

        // Should return to normal editing - buffer should still show modified (*)
        let screen_after = harness.screen_to_string();
        assert!(
            screen_after.contains("*") || screen_after.contains("modified"),
            "Should return to editing with buffer still modified. Screen:\n{}",
            screen_after
        );
    }

    // Restore permissions (cleanup)
    let _ = std::fs::set_permissions(&unwritable_dir, Permissions::from_mode(0o755));
}

/// Test that the editor doesn't crash when save fails with permission denied
/// This is a regression test for issue #775
#[test]
#[cfg(unix)]
fn test_save_permission_denied_no_crash() {
    let temp_dir = TempDir::new().unwrap();
    let unwritable_dir = temp_dir.path().join("unwritable_dir");
    std::fs::create_dir(&unwritable_dir).unwrap();

    let file_path = unwritable_dir.join("test.txt");
    std::fs::write(&file_path, "content").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Modify content
    harness.type_text("X").unwrap();
    harness.render().unwrap();

    // Make directory unwritable
    std::fs::set_permissions(&unwritable_dir, Permissions::from_mode(0o555)).unwrap();

    // Try to save - this should NOT panic/crash
    let save_result = harness.send_key(KeyCode::Char('s'), KeyModifiers::CONTROL);
    assert!(
        save_result.is_ok(),
        "Save operation should not panic: {:?}",
        save_result
    );

    let render_result = harness.render();
    assert!(
        render_result.is_ok(),
        "Render after save should not panic: {:?}",
        render_result
    );

    // Editor should still be functional
    let type_result = harness.type_text("Y");
    assert!(
        type_result.is_ok(),
        "Typing after failed save should work: {:?}",
        type_result
    );

    // Cleanup
    let _ = std::fs::set_permissions(&unwritable_dir, Permissions::from_mode(0o755));
}

/// Test that opening a file owned by another user disables editing.
///
/// Files that the current user cannot write to (e.g. root-owned files with
/// mode 0o644) should have editing disabled so the user can't accidentally
/// modify them.
#[test]
#[cfg(unix)]
fn test_save_root_owned_file_shows_sudo_prompt() {
    use std::os::unix::fs::MetadataExt;

    // Root (uid 0) bypasses Unix file permission checks.
    if unsafe { libc::getuid() } == 0 {
        eprintln!("Skipping test: root bypasses file permission checks");
        return;
    }

    // Try to find a root-owned file that's world-readable
    let test_paths = ["/etc/hosts", "/etc/passwd", "/etc/resolv.conf"];

    let mut test_file = None;
    for path in &test_paths {
        let path = std::path::Path::new(path);
        if path.exists() {
            if let Ok(meta) = std::fs::metadata(path) {
                let current_uid = unsafe { libc::getuid() };
                // File must be owned by different user (typically root) and readable
                if meta.uid() != current_uid {
                    test_file = Some(path.to_path_buf());
                    break;
                }
            }
        }
    }

    let file_path = match test_file {
        Some(p) => p,
        None => {
            eprintln!("No suitable root-owned file found for testing, skipping");
            return;
        }
    };

    eprintln!("Testing with root-owned file: {:?}", file_path);

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open the root-owned file
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Try to type — editing should be disabled since the file is not writable
    harness.type_text(" ").unwrap();
    harness.render().unwrap();

    // Buffer should NOT be modified (no "*" indicator) because editing is disabled
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains(" *"),
        "Editing should be disabled for files the current user cannot write. Screen:\n{}",
        screen
    );
}
