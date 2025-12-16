// End-to-end tests for file permission preservation during save

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs::Permissions;
use tempfile::TempDir;

#[cfg(unix)]
use std::os::unix::fs::PermissionsExt;

/// Test that saving a file preserves its original permissions (Unix)
#[test]
#[cfg(unix)]
fn test_save_preserves_file_permissions() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    // Create a file with specific permissions (e.g., 0o644 - rw-r--r--)
    std::fs::write(&file_path, "initial content").unwrap();
    std::fs::set_permissions(&file_path, Permissions::from_mode(0o644)).unwrap();

    // Verify initial permissions
    let initial_mode = std::fs::metadata(&file_path).unwrap().permissions().mode() & 0o777;
    assert_eq!(initial_mode, 0o644, "Initial permissions should be 0o644");

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open the file
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Modify the content
    harness.type_text("modified ").unwrap();
    harness.render().unwrap();

    // Save the file
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    // Wait for save to complete by checking file content changed
    let file_path_clone = file_path.clone();
    harness
        .wait_until(move |_| {
            std::fs::read_to_string(&file_path_clone)
                .map(|s| s.starts_with("modified "))
                .unwrap_or(false)
        })
        .unwrap();

    // Verify permissions are preserved
    let final_mode = std::fs::metadata(&file_path).unwrap().permissions().mode() & 0o777;
    assert_eq!(
        final_mode, 0o644,
        "Permissions should be preserved after save (expected 0o644, got 0o{:o})",
        final_mode
    );
}

/// Test that saving preserves executable permission (Unix)
#[test]
#[cfg(unix)]
fn test_save_preserves_executable_permission() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("script.sh");

    // Create an executable script (0o755 - rwxr-xr-x)
    std::fs::write(&file_path, "#!/bin/bash\necho hello").unwrap();
    std::fs::set_permissions(&file_path, Permissions::from_mode(0o755)).unwrap();

    let initial_mode = std::fs::metadata(&file_path).unwrap().permissions().mode() & 0o777;
    assert_eq!(
        initial_mode, 0o755,
        "Initial permissions should be 0o755 (executable)"
    );

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open the file
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Modify the content
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.type_text("\necho world").unwrap();
    harness.render().unwrap();

    // Save the file
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    // Wait for save to complete by checking file content changed
    let file_path_clone = file_path.clone();
    harness
        .wait_until(move |_| {
            std::fs::read_to_string(&file_path_clone)
                .map(|s| s.contains("echo world"))
                .unwrap_or(false)
        })
        .unwrap();

    // Verify executable permission is preserved
    let final_mode = std::fs::metadata(&file_path).unwrap().permissions().mode() & 0o777;
    assert_eq!(
        final_mode, 0o755,
        "Executable permission should be preserved (expected 0o755, got 0o{:o})",
        final_mode
    );
}

/// Test that saving preserves restricted permissions (Unix)
#[test]
#[cfg(unix)]
fn test_save_preserves_restricted_permissions() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("private.txt");

    // Create a file with restricted permissions (0o600 - rw-------)
    std::fs::write(&file_path, "secret data").unwrap();
    std::fs::set_permissions(&file_path, Permissions::from_mode(0o600)).unwrap();

    let initial_mode = std::fs::metadata(&file_path).unwrap().permissions().mode() & 0o777;
    assert_eq!(
        initial_mode, 0o600,
        "Initial permissions should be 0o600 (private)"
    );

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Open the file
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Modify the content
    harness.type_text("more ").unwrap();
    harness.render().unwrap();

    // Save the file
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    // Wait for save to complete by checking file content changed
    let file_path_clone = file_path.clone();
    harness
        .wait_until(move |_| {
            std::fs::read_to_string(&file_path_clone)
                .map(|s| s.starts_with("more "))
                .unwrap_or(false)
        })
        .unwrap();

    // Verify restricted permissions are preserved
    let final_mode = std::fs::metadata(&file_path).unwrap().permissions().mode() & 0o777;
    assert_eq!(
        final_mode, 0o600,
        "Restricted permissions should be preserved (expected 0o600, got 0o{:o})",
        final_mode
    );
}

/// Test that multiple saves preserve permissions each time (Unix)
#[test]
#[cfg(unix)]
fn test_multiple_saves_preserve_permissions() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("multi.txt");

    // Create a file with specific permissions
    std::fs::write(&file_path, "v1").unwrap();
    std::fs::set_permissions(&file_path, Permissions::from_mode(0o640)).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    // First edit and save
    harness.type_text(" edit1").unwrap();
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let mode_after_first = std::fs::metadata(&file_path).unwrap().permissions().mode() & 0o777;
    assert_eq!(
        mode_after_first, 0o640,
        "Permissions should be 0o640 after first save"
    );

    // Second edit and save
    harness.type_text(" edit2").unwrap();
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let mode_after_second = std::fs::metadata(&file_path).unwrap().permissions().mode() & 0o777;
    assert_eq!(
        mode_after_second, 0o640,
        "Permissions should be 0o640 after second save"
    );

    // Third edit and save
    harness.type_text(" edit3").unwrap();
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let mode_after_third = std::fs::metadata(&file_path).unwrap().permissions().mode() & 0o777;
    assert_eq!(
        mode_after_third, 0o640,
        "Permissions should be 0o640 after third save"
    );
}

/// Test various permission modes are preserved (Unix)
#[test]
#[cfg(unix)]
fn test_save_preserves_various_permission_modes() {
    let temp_dir = TempDir::new().unwrap();

    // Test a variety of permission modes
    let test_modes: &[u32] = &[
        0o777, // rwxrwxrwx
        0o755, // rwxr-xr-x
        0o700, // rwx------
        0o644, // rw-r--r--
        0o640, // rw-r-----
        0o600, // rw-------
        0o444, // r--r--r-- (read-only)
    ];

    for &mode in test_modes {
        let file_path = temp_dir.path().join(format!("test_{:o}.txt", mode));

        // Create file with specific mode
        std::fs::write(&file_path, "content").unwrap();
        std::fs::set_permissions(&file_path, Permissions::from_mode(mode)).unwrap();

        // For read-only files, we need to make them writable first to edit
        // Skip read-only mode in this test since we can't edit it
        if mode == 0o444 {
            continue;
        }

        let mut harness = EditorTestHarness::new(80, 24).unwrap();
        harness.open_file(&file_path).unwrap();
        harness.render().unwrap();

        // Modify and save
        harness.type_text("x").unwrap();
        harness
            .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
            .unwrap();
        harness.render().unwrap();

        // Verify permissions preserved
        let final_mode = std::fs::metadata(&file_path).unwrap().permissions().mode() & 0o777;
        assert_eq!(
            final_mode, mode,
            "Mode 0o{:o} should be preserved, got 0o{:o}",
            mode, final_mode
        );
    }
}
