//! E2E tests for the Switch Project feature
//!
//! Tests the ability to switch the project root (working directory) using
//! the command palette or File menu.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers, MouseButton, MouseEvent, MouseEventKind};
use std::fs;
use tempfile::TempDir;
use tracing_subscriber::{fmt, prelude::*, EnvFilter};

/// Test that Switch Project command appears in the command palette
#[test]
fn test_switch_project_command_in_palette() {
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    // Open command palette with Ctrl+P
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Wait for palette to appear
    harness
        .wait_until(|h| h.screen_to_string().contains(">command"))
        .expect("Command palette should appear");

    // Type "switch project" to search
    harness.type_text("switch project").unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();

    // Switch Project command should appear
    assert!(
        screen.contains("Switch Project"),
        "Switch Project command should appear in palette"
    );
}

/// Test that the folder browser appears when Switch Project is selected
#[test]
fn test_switch_project_shows_folder_browser() {
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    // Create some directories
    fs::create_dir(project_root.join("subdir1")).unwrap();
    fs::create_dir(project_root.join("subdir2")).unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    // Open command palette and select Switch Project
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains(">command"))
        .expect("Command palette should appear");

    harness.type_text("switch project").unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for folder browser to appear with directory contents loaded
    // On Windows, async directory loading may take longer
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Navigation:")
                && screen.contains("Open")
                && (screen.contains("subdir1") || screen.contains("subdir2"))
        })
        .expect("Folder browser should appear with directories listed");

    let screen = harness.screen_to_string();

    // Should show the folder browser with directories
    assert!(
        screen.contains("Navigation:"),
        "Navigation section should be visible"
    );
    assert!(
        screen.contains("subdir1") || screen.contains("subdir2"),
        "Directories should be listed"
    );
}

/// Test that selecting a folder changes the working directory
#[test]
fn test_switch_project_changes_working_dir() {
    let _ = tracing_subscriber::registry()
        .with(fmt::layer())
        .with(EnvFilter::from_default_env())
        .try_init();

    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    // Create a subdirectory
    let subdir = project_root.join("myproject");
    fs::create_dir(&subdir).unwrap();
    fs::write(subdir.join("README.md"), "Project readme").unwrap();
    // Canonicalize to handle macOS /var -> /private/var symlinks
    let subdir = subdir.canonicalize().unwrap();

    tracing::info!("Creating harness with project_root: {:?}", project_root);
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        100, // Wider terminal to see full message
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    // Open command palette and select Switch Project
    tracing::info!("Opening command palette");
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains(">command"))
        .expect("Command palette should appear");
    tracing::info!("Command palette opened");

    tracing::info!("Typing 'switch project'");
    harness.type_text("switch project").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    tracing::info!("Pressed Enter to select command");

    // Wait for folder browser
    tracing::info!("Waiting for folder browser (Navigation:)");
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            tracing::debug!("Screen while waiting for Navigation:\n{}", screen);
            screen.contains("Navigation:")
        })
        .expect("Folder browser should appear");
    tracing::info!("Folder browser appeared");

    // Navigate to myproject subdirectory
    tracing::info!("Typing 'myproject'");
    harness.type_text("myproject").unwrap();
    harness.render().unwrap();

    // Press Enter to select the folder
    tracing::info!("Pressing Enter to select folder");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    tracing::info!("Pressed Enter, checking active window re-root");

    // Switch Project re-roots the ACTIVE window in place — it must NOT restart
    // the editor (the old design tore the whole process down and rebuilt every
    // window).
    assert!(
        !harness.editor().should_restart(),
        "Switch Project must not request an editor restart"
    );

    // The active window's project root is now the selected subdirectory.
    assert_eq!(
        harness.editor().working_dir(),
        subdir.as_path(),
        "Active window should be re-rooted to the selected directory (myproject)"
    );
    tracing::info!("Test completed successfully");
}

/// Test that pressing Enter with no selection uses current directory
#[test]
fn test_switch_project_select_current_directory() {
    let _ = tracing_subscriber::registry()
        .with(fmt::layer())
        .with(EnvFilter::from_default_env())
        .try_init();

    // Install signal handlers for backtrace on SIGINT
    fresh::services::signal_handler::install_signal_handlers();

    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    // Create a nested structure
    let subdir = project_root.join("current_test");
    fs::create_dir(&subdir).unwrap();
    // Canonicalize to handle macOS /var -> /private/var symlinks
    let subdir = subdir.canonicalize().unwrap();

    tracing::info!("Creating harness with subdir: {:?}", subdir);
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        100,
        24,
        Default::default(),
        subdir.clone(), // Start in the subdirectory
    )
    .unwrap();

    // Open project browser
    tracing::info!("Opening command palette");
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains(">command"))
        .expect("Command palette should appear");
    tracing::info!("Command palette opened");

    tracing::info!("Typing 'switch project'");
    harness.type_text("switch project").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    tracing::info!("Pressed Enter to select command");

    // Wait for folder browser
    tracing::info!("Waiting for folder browser (Navigation:)");
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            tracing::debug!("Screen while waiting for Navigation:\n{}", screen);
            screen.contains("Navigation:")
        })
        .expect("Folder browser should appear");
    tracing::info!("Folder browser appeared");

    // Press Enter immediately to select current directory
    tracing::info!("Pressing Enter to select current directory");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    tracing::info!("Pressed Enter, checking active window");

    // Selecting the directory the active window is already rooted at is a
    // no-op: nothing to re-root, and certainly no editor restart.
    assert!(
        !harness.editor().should_restart(),
        "Selecting the current directory must not request an editor restart"
    );
    assert_eq!(
        harness.editor().working_dir(),
        subdir.as_path(),
        "Active window stays rooted at the current directory"
    );
    tracing::info!("Test completed successfully");
}

/// Test that canceling folder browser with Escape doesn't change directory
#[test]
fn test_switch_project_cancel_preserves_directory() {
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    // 120×24: with `{remote}` on the default status bar the
    // "directory not changed" status message is truncated at 80 cols.
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    // Open project browser
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains(">command"))
        .expect("Command palette should appear");

    harness.type_text("switch project").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for folder browser
    harness
        .wait_until(|h| h.screen_to_string().contains("Navigation:"))
        .expect("Folder browser should appear");

    // Cancel with Escape
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Folder browser should be closed
    harness.assert_screen_not_contains("Navigation:");
    harness.assert_screen_contains("cancelled");
}

/// Test that folder browser can navigate using backspace to go to parent
#[test]
fn test_switch_project_backspace_goes_parent() {
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    // Create nested structure with a file in each directory
    let subdir = project_root.join("nested");
    fs::create_dir(&subdir).unwrap();
    fs::write(project_root.join("root_file.txt"), "root").unwrap();
    fs::write(subdir.join("nested_file.txt"), "nested").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Default::default(),
        subdir.clone(), // Start in nested directory
    )
    .unwrap();

    // Open project browser
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains(">command"))
        .expect("Command palette should appear");

    harness.type_text("switch project").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for folder browser to fully load (both UI and directory contents)
    // The nested directory contains nested_file.txt which we wait for
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Navigation:") && screen.contains("nested_file.txt")
        })
        .expect("Folder browser should appear with nested directory contents");

    // Press backspace to go to parent
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();

    // Wait for parent directory contents
    harness
        .wait_until(|h| h.screen_to_string().contains("root_file.txt"))
        .expect("Should navigate to parent and show root_file.txt");
}

/// Test that Switch Project appears in the File menu
#[test]
fn test_switch_project_in_file_menu() {
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    // Open File menu with F10
    harness
        .send_key(KeyCode::F(10), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("File"))
        .expect("Menu should appear");

    let screen = harness.screen_to_string();

    // Switch Project should appear in File menu
    assert!(
        screen.contains("Switch Project"),
        "Switch Project should be in File menu"
    );
}

// Note: File explorer integration test removed as it requires longer timeout
// The file explorer update is tested manually via tmux session

/// Test the full folder switching flow with session handling
///
/// This test verifies:
/// 1. Switch Project re-roots the active window in place (no editor restart)
/// 2. Sessions are saved per-working-directory
/// 3. Sessions are restored when starting in the same directory
/// 4. Switching folders provides a clean slate (no old buffers)
#[test]
fn test_switch_project_restart_flow_with_sessions() {
    // Create two project directories
    let temp_dir = TempDir::new().unwrap();
    let project_a = temp_dir.path().join("project_a");
    let project_b = temp_dir.path().join("project_b");
    fs::create_dir(&project_a).unwrap();
    fs::create_dir(&project_b).unwrap();
    // Canonicalize to handle macOS /var -> /private/var symlinks
    let project_a = project_a.canonicalize().unwrap();
    let project_b = project_b.canonicalize().unwrap();

    // Create files in each project
    let file_a = project_a.join("main_a.txt");
    let file_b = project_b.join("main_b.txt");
    fs::write(&file_a, "Content from Project A").unwrap();
    fs::write(&file_b, "Content from Project B").unwrap();

    // Create a shared directory context for consistent session storage (isolated for testing)
    let context_temp = TempDir::new().unwrap();
    let dir_context = fresh::config_io::DirectoryContext::for_testing(context_temp.path());
    fs::create_dir_all(dir_context.workspaces_dir()).unwrap();

    // Phase 1: Start in project_a, open file, save session
    {
        let mut harness = EditorTestHarness::with_shared_dir_context(
            100,
            24,
            Default::default(),
            project_a.clone(),
            dir_context.clone(),
        )
        .unwrap();

        // Open the file in project_a
        harness.open_file(&file_a).unwrap();
        harness.render().unwrap();

        // Verify file is opened
        harness.assert_screen_contains("main_a.txt");
        harness.assert_screen_contains("Content from Project A");

        // Save session for project_a
        harness.editor_mut().save_workspace().unwrap();
    }

    // Phase 2: Start fresh in project_a - session should restore
    {
        let mut harness = EditorTestHarness::with_shared_dir_context(
            100,
            24,
            Default::default(),
            project_a.clone(),
            dir_context.clone(),
        )
        .unwrap();

        // Restore session
        let restored = harness.editor_mut().try_restore_workspace().unwrap();
        assert!(restored, "Session should be restored for project_a");

        harness.render().unwrap();

        // Verify the file from project_a was restored
        harness.assert_screen_contains("main_a.txt");
    }

    // Phase 3: Start in project_a and switch to project_b via Switch Project
    {
        let mut harness = EditorTestHarness::with_shared_dir_context(
            100,
            24,
            Default::default(),
            project_a.clone(),
            dir_context.clone(),
        )
        .unwrap();

        // Restore session (project_a's file)
        harness.editor_mut().try_restore_workspace().unwrap();
        harness.render().unwrap();
        harness.assert_screen_contains("main_a.txt");

        // Open folder browser and switch to project_b
        harness
            .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
            .unwrap();
        harness
            .wait_until(|h| h.screen_to_string().contains(">command"))
            .expect("Command palette should appear");

        harness.type_text("switch project").unwrap();
        harness
            .send_key(KeyCode::Enter, KeyModifiers::NONE)
            .unwrap();

        // Wait for folder browser
        harness
            .wait_until(|h| h.screen_to_string().contains("Navigation:"))
            .expect("Folder browser should appear");

        // Type path to project_b
        let project_b_str = project_b.to_string_lossy().to_string();
        harness.type_text(&project_b_str).unwrap();
        harness
            .send_key(KeyCode::Enter, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();

        // Switch Project re-roots the active window in place — no restart.
        assert!(
            !harness.should_quit() && !harness.editor().should_restart(),
            "Switch Project must not quit/restart the editor"
        );
        // The active window is now rooted at project_b...
        assert!(
            harness.editor().working_dir().starts_with(&project_b)
                || project_b.starts_with(harness.editor().working_dir()),
            "Active window should be re-rooted to project_b: got {:?}, expected {:?}",
            harness.editor().working_dir(),
            project_b
        );
        // ...and gives a clean slate — project_a's restored file is gone.
        harness.assert_screen_not_contains("main_a.txt");
    }

    // Phase 4: Simulate main loop restart - create new editor in project_b
    {
        let mut harness = EditorTestHarness::with_shared_dir_context(
            100,
            24,
            Default::default(),
            project_b.clone(),
            dir_context.clone(),
        )
        .unwrap();

        // On restart, session restore is skipped (is_first_run = false in main loop)
        // So we get a fresh editor - verify no old files
        harness.render().unwrap();

        // Should NOT contain project_a's file
        harness.assert_screen_not_contains("main_a.txt");
        harness.assert_screen_not_contains("Content from Project A");

        // Open file in project_b and save session
        harness.open_file(&file_b).unwrap();
        harness.render().unwrap();
        harness.assert_screen_contains("main_b.txt");
        harness.assert_screen_contains("Content from Project B");

        // Save session for project_b
        harness.editor_mut().save_workspace().unwrap();
    }

    // Phase 5: Start fresh in project_b - session should restore project_b's file
    {
        let mut harness = EditorTestHarness::with_shared_dir_context(
            100,
            24,
            Default::default(),
            project_b.clone(),
            dir_context.clone(),
        )
        .unwrap();

        // Restore session
        let restored = harness.editor_mut().try_restore_workspace().unwrap();
        assert!(restored, "Session should be restored for project_b");

        harness.render().unwrap();

        // Verify project_b's file was restored
        harness.assert_screen_contains("main_b.txt");
        // Should NOT have project_a's file
        harness.assert_screen_not_contains("main_a.txt");
    }

    // Phase 6: Start fresh in project_a again - should restore project_a's session (not project_b's)
    {
        let mut harness = EditorTestHarness::with_shared_dir_context(
            100,
            24,
            Default::default(),
            project_a.clone(),
            dir_context.clone(),
        )
        .unwrap();

        // Restore session
        let restored = harness.editor_mut().try_restore_workspace().unwrap();
        assert!(restored, "Session should be restored for project_a");

        harness.render().unwrap();

        // Verify project_a's file was restored
        harness.assert_screen_contains("main_a.txt");
        // Should NOT have project_b's file
        harness.assert_screen_not_contains("main_b.txt");
    }
}

use fresh::config_io::DirectoryContext;

/// Helper to switch project via the command palette
fn switch_to_project(harness: &mut EditorTestHarness, project_path: &std::path::Path) {
    // Open command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains(">command"))
        .expect("Command palette should appear");

    // Search for Switch Project
    harness.type_text("switch project").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for folder browser
    harness
        .wait_until(|h| h.screen_to_string().contains("Navigation:"))
        .expect("Folder browser should appear");

    // Type path to project
    let project_str = project_path.to_string_lossy().to_string();
    harness.type_text(&project_str).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// Test that sessions persist when switching between projects multiple times
/// using the Switch Project command (not Quit).
///
/// This verifies that:
/// 1. Opening a file in project A, then switching to project B
/// 2. Opening a file in project B, then switching back to project A
/// 3. The file from project A is restored
/// 4. Switching to project B restores the file from project B
#[test]
fn test_session_persistence_across_project_switches() {
    // Create two project directories
    let temp_dir = TempDir::new().unwrap();
    let project_a = temp_dir.path().join("project_a");
    let project_b = temp_dir.path().join("project_b");
    fs::create_dir(&project_a).unwrap();
    fs::create_dir(&project_b).unwrap();
    // Canonicalize to handle macOS /var -> /private/var symlinks
    let project_a = project_a.canonicalize().unwrap();
    let project_b = project_b.canonicalize().unwrap();

    // Create files in each project
    let file_a = project_a.join("file_a.txt");
    let file_b = project_b.join("file_b.txt");
    fs::write(&file_a, "Content of file A").unwrap();
    fs::write(&file_b, "Content of file B").unwrap();

    // Create a shared directory context for session persistence (isolated for testing)
    let context_temp = TempDir::new().unwrap();
    let dir_context = DirectoryContext::for_testing(context_temp.path());
    fs::create_dir_all(dir_context.workspaces_dir()).unwrap();

    // Phase 1: Start in project A, open file, switch to project B
    {
        let mut harness = EditorTestHarness::with_shared_dir_context(
            100,
            24,
            Default::default(),
            project_a.clone(),
            dir_context.clone(),
        )
        .unwrap();

        // Open file_a.txt
        harness.open_file(&file_a).unwrap();
        harness.render().unwrap();
        harness.assert_screen_contains("file_a.txt");

        // Save session before switching
        harness.editor_mut().save_workspace().unwrap();

        // Switch to project B
        switch_to_project(&mut harness, &project_b);

        // Switch Project re-roots in place — no restart — and the active
        // window is now rooted at project_b.
        assert!(
            !harness.should_quit() && !harness.editor().should_restart(),
            "Switch Project must not restart the editor"
        );
        assert!(
            harness.editor().working_dir().starts_with(&project_b)
                || project_b.starts_with(harness.editor().working_dir()),
            "Active window should be re-rooted to project_b: got {:?}",
            harness.editor().working_dir()
        );
    }

    // Phase 2: Start in project B (simulating restart), open file
    {
        let mut harness = EditorTestHarness::with_shared_dir_context(
            100,
            24,
            Default::default(),
            project_b.clone(),
            dir_context.clone(),
        )
        .unwrap();

        // Open file_b.txt
        harness.open_file(&file_b).unwrap();
        harness.render().unwrap();
        harness.assert_screen_contains("file_b.txt");

        // Save session before switching
        harness.editor_mut().save_workspace().unwrap();

        // Switch back to project A
        switch_to_project(&mut harness, &project_a);

        // Switch Project re-roots in place — no restart — and the active
        // window is now rooted at project_a.
        assert!(
            !harness.should_quit() && !harness.editor().should_restart(),
            "Switch Project must not restart the editor"
        );
        assert!(
            harness.editor().working_dir().starts_with(&project_a)
                || project_a.starts_with(harness.editor().working_dir()),
            "Active window should be re-rooted to project_a: got {:?}",
            harness.editor().working_dir()
        );
    }

    // Phase 3: Return to project A - session should restore file_a.txt
    {
        let mut harness = EditorTestHarness::with_shared_dir_context(
            100,
            24,
            Default::default(),
            project_a.clone(),
            dir_context.clone(),
        )
        .unwrap();

        // Restore session
        let restored = harness.editor_mut().try_restore_workspace().unwrap();
        assert!(restored, "Session should be restored for project A");

        harness.render().unwrap();

        // Verify file_a.txt is restored
        harness.assert_screen_contains("file_a.txt");
        harness.assert_screen_not_contains("file_b.txt");

        // Save session and switch to project B
        harness.editor_mut().save_workspace().unwrap();
        switch_to_project(&mut harness, &project_b);
        assert!(
            !harness.should_quit() && !harness.editor().should_restart(),
            "Switch Project must not restart the editor"
        );
    }

    // Phase 4: Return to project B - session should restore file_b.txt
    {
        let mut harness = EditorTestHarness::with_shared_dir_context(
            100,
            24,
            Default::default(),
            project_b.clone(),
            dir_context.clone(),
        )
        .unwrap();

        // Restore session
        let restored = harness.editor_mut().try_restore_workspace().unwrap();
        assert!(restored, "Session should be restored for project B");

        harness.render().unwrap();

        // Verify file_b.txt is restored
        harness.assert_screen_contains("file_b.txt");
        harness.assert_screen_not_contains("file_a.txt");

        // Switch back to project A for one more verification
        harness.editor_mut().save_workspace().unwrap();
        switch_to_project(&mut harness, &project_a);
        assert!(
            !harness.should_quit() && !harness.editor().should_restart(),
            "Switch Project must not restart the editor"
        );
    }

    // Phase 5: Final return to project A - verify persistence
    {
        let mut harness = EditorTestHarness::with_shared_dir_context(
            100,
            24,
            Default::default(),
            project_a.clone(),
            dir_context.clone(),
        )
        .unwrap();

        // Restore session
        let restored = harness.editor_mut().try_restore_workspace().unwrap();
        assert!(restored, "Session should still be restored for project A");

        harness.render().unwrap();
        harness.assert_screen_contains("file_a.txt");
    }
}

/// Send two left-clicks at the same coordinates within the double-click window.
fn double_click_at(harness: &mut EditorTestHarness, col: u16, row: u16) {
    // Ensure we are outside any previous click's double-click window.
    let window = std::time::Duration::from_millis(
        harness
            .config()
            .editor
            .double_click_time_ms
            .saturating_mul(2),
    );
    harness.advance_time(window);
    let send = |h: &mut EditorTestHarness, kind: MouseEventKind| {
        h.send_mouse(MouseEvent {
            kind,
            column: col,
            row,
            modifiers: KeyModifiers::empty(),
        })
        .unwrap();
    };
    send(harness, MouseEventKind::Down(MouseButton::Left));
    send(harness, MouseEventKind::Up(MouseButton::Left));
    send(harness, MouseEventKind::Down(MouseButton::Left));
    send(harness, MouseEventKind::Up(MouseButton::Left));
    harness.render().unwrap();
}

/// Regression test for #1931: in Switch Project (folder-only) mode, double-clicking
/// a directory in the file list should navigate INTO it, not immediately select it
/// as the new project root.
#[test]
fn test_switch_project_double_click_navigates_into_folder() {
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    // Create a subdirectory with a unique marker file so we can tell when we
    // have actually navigated INTO it.
    let subdir = project_root.join("targetdir");
    fs::create_dir(&subdir).unwrap();
    fs::write(subdir.join("inside_marker.txt"), "marker").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    // Open Switch Project via command palette.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains(">command"))
        .expect("Command palette should appear");
    harness.type_text("switch project").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for the folder browser with the target directory listed.
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Navigation:") && screen.contains("targetdir")
        })
        .expect("Folder browser should appear with target directory listed");

    // Find the screen row containing the target directory and pick a click
    // column inside the directory name.
    let screen = harness.screen_to_string();
    let (row_idx, line) = screen
        .lines()
        .enumerate()
        .find(|(_, l)| l.contains("targetdir"))
        .expect("Should find row containing 'targetdir'");
    let col = line
        .find("targetdir")
        .expect("targetdir must be on its row") as u16
        + 1; // click on a character inside the name

    double_click_at(&mut harness, col, row_idx as u16);

    // After the double-click, the post-condition that observably
    // distinguishes "navigated into the folder" from "selected as new
    // project root" is: the Switch Project browser is still open
    // (so its "Navigation:" header is on screen) AND the target
    // directory's marker file is now listed. The bug version closes
    // the browser to commit the selection; with the fix, the
    // browser stays open at the new path. We wait for both
    // conditions in one semantic wait so the test settles to a
    // stable state before observing — no model accessors, no bare
    // snapshots, per CONTRIBUTING.md E2E rules.
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Navigation:") && screen.contains("inside_marker.txt")
        })
        .expect(
            "Folder browser must remain open and show the target directory's contents \
             after double-clicking into it",
        );
}

/// Regression test for #1931: in Switch Project mode, double-clicking the ".."
/// (parent) entry should navigate up to the parent directory, not select the
/// parent as the new project root.
#[test]
fn test_switch_project_double_click_parent_navigates_up() {
    let temp_dir = TempDir::new().unwrap();
    let root = temp_dir.path().to_path_buf();

    // Create a parent/child structure. Start the editor inside the child so the
    // ".." entry is meaningful, and place a marker in the parent that the
    // post-navigation wait keys off. The marker name is kept short on
    // purpose: a long filename can itself be truncated with an ellipsis in
    // a narrow browser column, so waiting on it would be sensitive to
    // rendering width. "upok" can't truncate and appears nowhere else.
    let child = root.join("child");
    fs::create_dir(&child).unwrap();
    fs::create_dir(root.join("upok")).unwrap();

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 24, Default::default(), child.clone())
            .unwrap();

    // Open Switch Project.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains(">command"))
        .expect("Command palette should appear");
    harness.type_text("switch project").unwrap();
    // Wait for the palette to finish filtering and render the "Switch Project"
    // command (title-cased, so it can't match the lowercase input echo) before
    // confirming. Pressing Enter before the async filter settles would commit a
    // stale/empty selection, so the folder browser would never open and the
    // wait below would run to nextest's external timeout.
    harness
        .wait_until(|h| h.screen_to_string().contains("Switch Project"))
        .expect("Command palette should list the Switch Project command");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for the folder browser's async directory read to FINISH before
    // clicking. While it is pending the list shows a "Loading..." placeholder,
    // and the only ".." on screen are dotted artifacts — the "Loading..."
    // ellipsis, the inline "Navigation: .." shortcut, and (on Windows, whose
    // temp paths are long enough to truncate) the path title's "[...]". None
    // of those are the clickable parent row, so a bare `contains("..")` gate
    // fires too early: the click then lands on the empty list, which commits
    // the current dir as the project root and closes the browser — the test
    // then hangs to nextest's external timeout. This reproduced only on
    // Windows, where the slower filesystem keeps "Loading..." on screen longer.
    //
    // Gate on the thing the click actually needs: the *loaded* parent entry. A
    // directory renders with a trailing slash, so the real row reads "../".
    // The placeholder, the nav shortcut, and the title ellipsis have no slash,
    // so "../" can only be the populated parent row.
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            let Some(nav_row) = screen.lines().position(|l| l.contains("Navigation:")) else {
                return false;
            };
            screen.lines().skip(nav_row + 1).any(|l| l.contains("../"))
        })
        .expect("Folder browser should finish loading and list the parent ('../') entry");

    // Locate the parent row by position (below the Navigation header) and the
    // "../" text within it. Anchoring below the header skips the path title on
    // the modal's top border; matching "../" (with slash) skips the inline
    // "Navigation: .." shortcut on the header line itself.
    let screen = harness.screen_to_string();
    let nav_row = screen
        .lines()
        .position(|l| l.contains("Navigation:"))
        .expect("Folder browser should have a Navigation header");
    let (row_idx, line) = screen
        .lines()
        .enumerate()
        .skip(nav_row + 1)
        .find(|(_, l)| l.contains("../"))
        .expect("Should find the '../' entry row below the Navigation header");
    let col = line.find("../").expect("'../' must be on its row") as u16 + 1;

    double_click_at(&mut harness, col, row_idx as u16);

    // After the double-click, the post-condition that observably
    // distinguishes "navigated up" from "selected parent as new
    // project root" is: the Switch Project browser is still open
    // (so its "Navigation:" header is on screen) AND the parent
    // directory's "upok" marker is now listed. The bug version
    // closes the browser to commit the selection; with the fix,
    // the browser stays open at the new path. We wait for both
    // conditions in one semantic wait so the test settles to a
    // stable state before observing — no model accessors, no bare
    // snapshots, per CONTRIBUTING.md E2E rules.
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Navigation:") && screen.contains("upok")
        })
        .expect(
            "Folder browser must remain open and show the parent directory's contents \
             (the 'upok' marker) after double-clicking '..'",
        );
}
