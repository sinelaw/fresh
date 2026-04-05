// Regression test for issue #1469: .bash_profile opens as read-only
//
// The editor refuses to edit .bash_profile, showing "Editing disabled in this buffer"
// even though the file has normal permissions and is not binary.

use crate::common::harness::EditorTestHarness;
use tempfile::TempDir;

/// Test that .bash_profile can be edited (not detected as read-only or binary)
#[test]
fn test_bash_profile_not_read_only() {
    let temp_dir = TempDir::new().unwrap();
    let bash_profile_path = temp_dir.path().join(".bash_profile");

    // Create a typical .bash_profile
    std::fs::write(
        &bash_profile_path,
        "# .bash_profile\nexport PATH=\"$HOME/bin:$PATH\"\n",
    )
    .unwrap();

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        std::fs::set_permissions(
            &bash_profile_path,
            std::fs::Permissions::from_mode(0o644),
        )
        .unwrap();
    }

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&bash_profile_path).unwrap();
    harness.render().unwrap();

    // The file should NOT be detected as binary
    assert!(
        !harness.editor().active_state().buffer.is_binary(),
        ".bash_profile should not be detected as binary"
    );

    // Editing should NOT be disabled
    assert!(
        !harness.editor().is_editing_disabled(),
        ".bash_profile should be editable (issue #1469)"
    );

    // Should be able to type text
    harness.type_text("# test").unwrap();
    harness.render().unwrap();

    // Verify the text was inserted
    let content = harness.get_buffer_content().unwrap();
    assert!(
        content.contains("# test"),
        "Should be able to type in .bash_profile buffer"
    );
}

/// Test that .bash_profile opened from session restore is editable
/// This is the specific scenario from issue #1469
#[test]
fn test_bash_profile_editable_after_session_restore() {
    let temp_dir = TempDir::new().unwrap();
    let bash_profile_path = temp_dir.path().join(".bash_profile");

    // Create a typical .bash_profile
    std::fs::write(
        &bash_profile_path,
        "# .bash_profile\nexport PATH=\"$HOME/bin:$PATH\"\n",
    )
    .unwrap();

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        std::fs::set_permissions(
            &bash_profile_path,
            std::fs::Permissions::from_mode(0o644),
        )
        .unwrap();
    }

    // First session: open the file
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&bash_profile_path).unwrap();
    harness.render().unwrap();

    // Verify it's editable in first session
    assert!(
        !harness.editor().is_editing_disabled(),
        ".bash_profile should be editable in first session"
    );

    // Second session: simulate session restore by opening the file again
    let mut harness2 = EditorTestHarness::new(80, 24).unwrap();
    harness2.open_file(&bash_profile_path).unwrap();
    harness2.render().unwrap();

    // Verify it's editable in second session
    assert!(
        !harness2.editor().is_editing_disabled(),
        ".bash_profile should be editable after session restore (issue #1469)"
    );

    // Verify typing works
    harness2.type_text("# edit").unwrap();
    harness2.render().unwrap();
    let content = harness2.get_buffer_content().unwrap();
    assert!(
        content.contains("# edit"),
        "Should be able to type in restored .bash_profile buffer"
    );
}

/// Test that .bash_profile with 600 permissions is editable by owner
#[test]
#[cfg(unix)]
fn test_bash_profile_private_permissions_editable() {
    use std::os::unix::fs::PermissionsExt;

    let temp_dir = TempDir::new().unwrap();
    let bash_profile_path = temp_dir.path().join(".bash_profile");

    // Create .bash_profile with restrictive 600 permissions (common for dotfiles)
    std::fs::write(
        &bash_profile_path,
        "# .bash_profile\nexport SECRET=value\n",
    )
    .unwrap();
    std::fs::set_permissions(
        &bash_profile_path,
        std::fs::Permissions::from_mode(0o600),
    )
    .unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&bash_profile_path).unwrap();
    harness.render().unwrap();

    // Owner should be able to edit their own file
    assert!(
        !harness.editor().is_editing_disabled(),
        ".bash_profile with 0o600 should be editable by owner"
    );
}

/// Test that various shell dotfiles are editable
#[test]
fn test_shell_dotfiles_editable() {
    let temp_dir = TempDir::new().unwrap();

    let dotfiles = vec![
        ".bash_profile",
        ".bashrc",
        ".profile",
        ".zshrc",
        ".zprofile",
    ];

    for name in dotfiles {
        let path = temp_dir.path().join(name);
        std::fs::write(&path, format!("# {}\n", name)).unwrap();

        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o644)).unwrap();
        }

        let mut harness = EditorTestHarness::new(80, 24).unwrap();
        harness.open_file(&path).unwrap();
        harness.render().unwrap();

        assert!(
            !harness.editor().is_editing_disabled(),
            "{} should be editable",
            name
        );
    }
}

/// Test that dotfiles symlinked into library paths are still editable (issue #1469)
///
/// On NixOS with home-manager, ~/.bash_profile is a symlink to /nix/store/...
/// The editor should check the user-visible path (not the canonical/resolved path)
/// for the library detection, so the file remains editable.
#[test]
#[cfg(unix)]
fn test_dotfile_symlinked_to_library_path_is_editable() {
    let temp_dir = TempDir::new().unwrap();

    // Simulate a nix-store-like library path
    let nix_store = temp_dir.path().join("nix/store/abc123-bash-config");
    std::fs::create_dir_all(&nix_store).unwrap();

    // Create the actual file in the "library" path
    let actual_file = nix_store.join("bash_profile");
    std::fs::write(&actual_file, "# managed by home-manager\nexport PATH=$PATH\n").unwrap();

    // Create a symlink from a user-visible path to the library path
    // This simulates: ~/.bash_profile -> /nix/store/abc123-bash-config/bash_profile
    let user_visible_path = temp_dir.path().join(".bash_profile");
    std::os::unix::fs::symlink(&actual_file, &user_visible_path).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&user_visible_path).unwrap();
    harness.render().unwrap();

    // The file should be editable because the user-visible path (~/.bash_profile)
    // is NOT in a library directory, even though the canonical path resolves
    // to /nix/store/... which is a library path.
    assert!(
        !harness.editor().is_editing_disabled(),
        "Dotfile symlinked to nix store should be editable (issue #1469)"
    );
}

/// Test that files actually in library paths remain read-only
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

    // Files directly opened from library paths should remain read-only
    assert!(
        harness.editor().is_editing_disabled(),
        "File in node_modules should be read-only"
    );
}
