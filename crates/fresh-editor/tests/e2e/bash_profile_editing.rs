// Regression test for issue #1469: .bash_profile opens as read-only
//
// The editor refuses to edit .bash_profile, showing "Editing disabled in this buffer"
// even though the file has normal permissions and is not binary.
//
// Root cause: is_library_path checks the canonical (symlink-resolved) path.
// If ~/.bash_profile is a symlink to /nix/store/... (common with home-manager),
// the canonical path matches a library pattern and the buffer becomes read-only.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use tempfile::TempDir;

/// Test that .bash_profile can be edited: type text and verify it appears on screen.
#[test]
fn test_bash_profile_is_editable() {
    let temp_dir = TempDir::new().unwrap();
    let bash_profile_path = temp_dir.path().join(".bash_profile");

    std::fs::write(
        &bash_profile_path,
        "# .bash_profile\nexport PATH=\"$HOME/bin:$PATH\"\n",
    )
    .unwrap();

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        std::fs::set_permissions(&bash_profile_path, std::fs::Permissions::from_mode(0o644))
            .unwrap();
    }

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&bash_profile_path).unwrap();
    harness.render().unwrap();

    // Type text via keyboard events
    harness
        .send_key(KeyCode::Char('h'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('i'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify the typed text appears on screen (not blocked by "Editing disabled")
    harness.assert_screen_contains("hi");
    harness.assert_screen_not_contains("Editing disabled");
}

/// Test that dotfiles symlinked into library paths are still editable (issue #1469).
///
/// On NixOS with home-manager, ~/.bash_profile is a symlink to /nix/store/...
/// The editor should check the user-visible path (not the canonical/resolved path)
/// for the library detection, so the file remains editable.
///
/// We simulate this with node_modules (uses `contains` matching, works in temp dirs)
/// instead of /nix/store/ (uses `starts_with`, can't be created in tests).
#[test]
#[cfg(unix)]
fn test_dotfile_symlinked_to_library_path_is_editable() {
    let temp_dir = TempDir::new().unwrap();

    // Create the actual file inside a library path (node_modules).
    // After canonicalization the symlink will resolve here, triggering is_library_path.
    let lib_dir = temp_dir.path().join("project/node_modules/dotfiles");
    std::fs::create_dir_all(&lib_dir).unwrap();

    let actual_file = lib_dir.join("bash_profile");
    std::fs::write(
        &actual_file,
        "# managed by dotfile manager\nexport PATH=$PATH\n",
    )
    .unwrap();

    // Create a symlink from a user-visible path (outside library dirs) to the library path.
    // This simulates: ~/.bash_profile -> <project>/node_modules/dotfiles/bash_profile
    let user_visible_path = temp_dir.path().join(".bash_profile");
    std::os::unix::fs::symlink(&actual_file, &user_visible_path).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&user_visible_path).unwrap();
    harness.render().unwrap();

    // Type a unique string — should succeed because the user-visible path is NOT in a library dir
    harness
        .send_key(KeyCode::Char('Q'), KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(KeyCode::Char('Z'), KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    // The typed text "QZ" should appear on screen, proving editing works.
    // "QZ" is chosen because it doesn't appear in the original file content.
    harness.assert_screen_contains("QZ");
}

/// Test that files directly opened from library paths remain read-only.
/// Typing should be blocked: the buffer content must not change.
#[test]
#[cfg(unix)]
fn test_file_in_library_path_stays_readonly() {
    let temp_dir = TempDir::new().unwrap();

    // Create a file directly inside a node_modules path
    let node_modules = temp_dir.path().join("project/node_modules/pkg");
    std::fs::create_dir_all(&node_modules).unwrap();
    let lib_file = node_modules.join("index.js");
    std::fs::write(&lib_file, "module.exports = {};").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&lib_file).unwrap();
    harness.render().unwrap();

    // Capture the screen before typing
    let screen_before = harness.screen_to_string();

    // Try to type — should be blocked
    harness
        .send_key(KeyCode::Char('x'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // The typed character 'x' should NOT appear in the buffer area.
    // The original content "module.exports = {};" should be unchanged.
    harness.assert_screen_contains("module.exports = {};");

    // Verify the buffer area didn't change (the first lines should be identical).
    // Extract content lines (skip status bar which may change).
    let screen_after = harness.screen_to_string();
    let content_before: Vec<&str> = screen_before.lines().take(5).collect();
    let content_after: Vec<&str> = screen_after.lines().take(5).collect();
    assert_eq!(
        content_before, content_after,
        "Buffer content should not change when typing in a read-only library file"
    );
}
