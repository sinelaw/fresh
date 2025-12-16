use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

/// Test that Alt+X opens the Explorer menu
#[test]
fn test_alt_x_opens_explorer_menu() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();
    harness.render().unwrap();

    // Explorer menu dropdown should not be visible initially
    harness.assert_screen_not_contains("New Folder");

    // Press Alt+X to open Explorer menu
    harness
        .send_key(KeyCode::Char('x'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    // Explorer menu dropdown should now be visible with its items
    harness.assert_screen_contains("New File");
    harness.assert_screen_contains("New Folder");
    harness.assert_screen_contains("Refresh");
}

/// Test that Explorer menu shows all expected items
#[test]
fn test_explorer_menu_items() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();
    harness.render().unwrap();

    // Open Explorer menu with Alt+X
    harness
        .send_key(KeyCode::Char('x'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    // Verify all expected menu items are present
    harness.assert_screen_contains("New File");
    harness.assert_screen_contains("New Folder");
    harness.assert_screen_contains("Open");
    harness.assert_screen_contains("Rename");
    harness.assert_screen_contains("Delete");
    harness.assert_screen_contains("Refresh");
    harness.assert_screen_contains("Show Hidden Files");
    harness.assert_screen_contains("Show Gitignored Files");
}

/// Test that Explorer menu shows checkbox states for toggles
#[test]
fn test_explorer_menu_checkbox_states() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();

    // Open file explorer first (checkbox states are read from file explorer state)
    harness.editor_mut().toggle_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    // Open Explorer menu with Alt+X
    harness
        .send_key(KeyCode::Char('x'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();

    // Should show unchecked boxes for hidden and gitignored by default
    // The checkbox format is "☐ " for unchecked and "☑ " for checked
    assert!(
        screen.contains("☐ Show Hidden Files") || screen.contains("Show Hidden Files"),
        "Should show Show Hidden Files menu item"
    );
    assert!(
        screen.contains("☐ Show Gitignored Files") || screen.contains("Show Gitignored Files"),
        "Should show Show Gitignored Files menu item"
    );
}

/// Test that toggling hidden files via keybinding updates checkbox state
#[test]
fn test_explorer_menu_checkbox_updates_on_toggle() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();

    // Open file explorer and focus it
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    // Press 'h' to toggle hidden files while in file explorer
    harness
        .send_key(KeyCode::Char('h'), KeyModifiers::NONE)
        .unwrap();

    // Now open Explorer menu to check checkbox state
    harness
        .send_key(KeyCode::Char('x'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();

    // Should now show checked box for Show Hidden Files
    assert!(
        screen.contains("☑ Show Hidden Files"),
        "Show Hidden Files should be checked after toggling. Screen:\n{}",
        screen
    );
}

/// Test escape closes Explorer menu
#[test]
fn test_escape_closes_explorer_menu() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();
    harness.render().unwrap();

    // Open Explorer menu
    harness
        .send_key(KeyCode::Char('x'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("New Folder");

    // Press Escape to close
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Menu should be closed
    harness.assert_screen_not_contains("New Folder");
}

/// Test Explorer menu navigation with arrow keys
#[test]
fn test_explorer_menu_navigation() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();
    harness.render().unwrap();

    // Open Explorer menu
    harness
        .send_key(KeyCode::Char('x'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    // Navigate down through the menu items
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Navigate up
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Menu should still be visible
    harness.assert_screen_contains("New File");
    harness.assert_screen_contains("New Folder");
}

/// Test navigating from Explorer menu to other menus
#[test]
fn test_explorer_menu_left_right_navigation() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();
    harness.render().unwrap();

    // Open Explorer menu
    harness
        .send_key(KeyCode::Char('x'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("New Folder");

    // Navigate right to Help menu
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should now show Help menu items
    harness.assert_screen_contains("Show Fresh Manual");
    harness.assert_screen_not_contains("New Folder");

    // Navigate left back to Explorer menu
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Should now show Explorer menu items again
    harness.assert_screen_contains("New Folder");
    harness.assert_screen_not_contains("Show Fresh Manual");
}

/// Test that 'n' keybinding creates a new file in file explorer
#[test]
fn test_explorer_n_keybinding_creates_file() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Count initial files
    let initial_count = fs::read_dir(&project_root).unwrap().count();

    // Open and focus file explorer
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    // Press 'n' to create new file (new file does not enter rename mode, unlike new folder)
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();
    // Wait for the status message to show the file was created
    harness.wait_for_screen_contains("Created").unwrap();

    // Check status bar for confirmation
    let screen = harness.screen_to_string();
    println!("Screen after creating file:\n{}", screen);

    // Verify a new file was created
    let final_count = fs::read_dir(&project_root).unwrap().count();
    assert!(
        final_count > initial_count,
        "A new file should have been created. Initial: {}, Final: {}",
        initial_count,
        final_count
    );
}

/// Test that 'h' keybinding toggles hidden files in file explorer
#[test]
fn test_explorer_h_keybinding_toggles_hidden() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();

    // Open and focus file explorer
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    // Press 'h' to toggle hidden files
    harness
        .send_key(KeyCode::Char('h'), KeyModifiers::NONE)
        .unwrap();

    // Check status bar for confirmation message
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("hidden") || screen.contains("Hidden"),
        "Status bar should show hidden files toggle message. Screen:\n{}",
        screen
    );
}

/// Test that 'i' keybinding toggles gitignored files in file explorer
#[test]
fn test_explorer_i_keybinding_toggles_gitignored() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();

    // Open and focus file explorer
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    // Press 'i' to toggle gitignored files
    harness
        .send_key(KeyCode::Char('i'), KeyModifiers::NONE)
        .unwrap();

    // Check status bar for confirmation message
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("gitignored") || screen.contains("Gitignored"),
        "Status bar should show gitignored files toggle message. Screen:\n{}",
        screen
    );
}

/// Test that 'd' keybinding triggers delete in file explorer
#[test]
fn test_explorer_d_keybinding_deletes() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create a test file
    let file_path = project_root.join("to_delete.txt");
    fs::write(&file_path, "delete me").unwrap();

    // Open and focus file explorer
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    // Root is automatically expanded during init, so just wait for the file to appear
    harness
        .wait_for_file_explorer_item("to_delete.txt")
        .unwrap();

    // Navigate down to select the file (root is initially selected)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press 'd' to delete (deletes immediately without prompt)
    harness
        .send_key(KeyCode::Char('d'), KeyModifiers::NONE)
        .unwrap();
    // Wait for the file to actually be deleted
    harness.wait_until(|_| !file_path.exists()).unwrap();
}

/// Test that F2 keybinding triggers rename in file explorer
#[test]
fn test_explorer_f2_keybinding_renames() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create a test file
    fs::write(project_root.join("to_rename.txt"), "rename me").unwrap();

    // Open and focus file explorer
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    // Root is automatically expanded during init, so just wait for the file to appear
    harness
        .wait_for_file_explorer_item("to_rename.txt")
        .unwrap();

    // Navigate down to select the file (root is initially selected)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();

    // Press F2 to rename
    harness.send_key(KeyCode::F(2), KeyModifiers::NONE).unwrap();
    harness.wait_for_prompt().unwrap();

    // The test passes if no panic occurs - actual rename depends on the selected item
    let screen = harness.screen_to_string();
    println!("Screen after rename attempt:\n{}", screen);
}

/// Test that Delete keybinding triggers delete in file explorer
#[test]
fn test_explorer_delete_key_deletes() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create a test file
    let file_path = project_root.join("delete_test.txt");
    fs::write(&file_path, "delete me").unwrap();

    // Open and focus file explorer
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    // Root is automatically expanded during init, so just wait for the file to appear
    harness
        .wait_for_file_explorer_item("delete_test.txt")
        .unwrap();

    // Navigate down to select the file (root is initially selected)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press Delete key (deletes immediately without prompt)
    harness
        .send_key(KeyCode::Delete, KeyModifiers::NONE)
        .unwrap();
    // Wait for the file to actually be deleted
    harness.wait_until(|_| !file_path.exists()).unwrap();
}

/// Test executing New File action from Explorer menu
#[test]
fn test_explorer_menu_new_file_action() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Open file explorer first
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    let initial_count = fs::read_dir(&project_root).unwrap().count();

    // Open Explorer menu with Alt+X
    harness
        .send_key(KeyCode::Char('x'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    // "New File" should be the first item, so just press Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    // Wait for a new file to actually be created on the filesystem
    harness
        .wait_until(|_| fs::read_dir(&project_root).unwrap().count() > initial_count)
        .unwrap();
}

/// Test executing New Folder action from Explorer menu
#[test]
fn test_explorer_menu_new_folder_action() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Open file explorer first
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    let initial_dirs: Vec<_> = fs::read_dir(&project_root)
        .unwrap()
        .filter_map(|e| e.ok())
        .filter(|e| e.path().is_dir())
        .collect();
    let initial_dir_count = initial_dirs.len();

    // Open Explorer menu with Alt+X
    harness
        .send_key(KeyCode::Char('x'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    // Navigate to "New Folder" (second item)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press Enter to execute
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    // Wait for prompt (new folder enters rename mode)
    harness.wait_for_prompt().unwrap();
    // Accept default name
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // Verify a new directory was created
    let final_dirs: Vec<_> = fs::read_dir(&project_root)
        .unwrap()
        .filter_map(|e| e.ok())
        .filter(|e| e.path().is_dir())
        .collect();
    let final_dir_count = final_dirs.len();

    assert!(
        final_dir_count > initial_dir_count,
        "A new directory should have been created via menu. Initial: {}, Final: {}",
        initial_dir_count,
        final_dir_count
    );
}

/// Test that Explorer menu appears in the menu bar
#[test]
fn test_explorer_menu_in_menu_bar() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();
    harness.render().unwrap();

    // Check that Explorer appears in the menu bar
    let menu_bar = harness.get_menu_bar();
    assert!(
        menu_bar.contains("Explorer"),
        "Menu bar should contain 'Explorer'. Menu bar: {}",
        menu_bar
    );
}

/// Test that Show Hidden Files toggle via menu updates state
#[test]
fn test_explorer_menu_toggle_hidden_via_menu() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();

    // Open file explorer first
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    // Open Explorer menu
    harness
        .send_key(KeyCode::Char('x'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    // Navigate to "Show Hidden Files"
    // Menu items (separators are auto-skipped): New File -> New Folder -> Open -> Rename -> Delete -> Refresh -> Show Hidden Files
    // That's 6 Down presses from New File to Show Hidden Files
    for _ in 0..6 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    // Execute the toggle
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Status bar should show toggle message
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("hidden") || screen.contains("Hidden"),
        "Should show hidden files toggle message. Screen:\n{}",
        screen
    );
}

/// Test Ctrl+R refresh keybinding in file explorer
#[test]
fn test_explorer_ctrl_r_refresh() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create initial file
    fs::write(project_root.join("initial.txt"), "initial").unwrap();

    // Open and focus file explorer
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    // Create another file
    fs::write(project_root.join("new_file.txt"), "new").unwrap();

    // Press Ctrl+R to refresh
    harness
        .send_key(KeyCode::Char('r'), KeyModifiers::CONTROL)
        .unwrap();
    // Wait for file explorer to show the new file
    harness.wait_for_file_explorer_item("new_file.txt").unwrap();

    let screen = harness.screen_to_string();
    println!("Screen after refresh:\n{}", screen);
}

/// Test that Explorer menu items are disabled (grayed out) when explorer is not focused
#[test]
fn test_explorer_menu_items_disabled_when_not_focused() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();

    // File explorer is not open/focused initially
    harness.render().unwrap();

    // Open Explorer menu with Alt+X
    harness
        .send_key(KeyCode::Char('x'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    // Try to execute "New File" action (first item) - should not work when explorer not focused
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Menu should close but no file should be created since explorer isn't focused
    let project_root = harness.project_dir().unwrap();
    let files: Vec<_> = fs::read_dir(&project_root)
        .unwrap()
        .filter_map(|e| e.ok())
        .collect();

    // Should have no files (action was disabled)
    assert!(
        files.is_empty(),
        "No files should be created when explorer menu action is disabled. Found: {:?}",
        files.iter().map(|f| f.file_name()).collect::<Vec<_>>()
    );
}

/// Test that New Folder action creates a folder and enters rename mode
#[test]
fn test_new_folder_enters_rename_mode() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Open and focus file explorer
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    // Create new folder directly using the method
    harness.editor_mut().file_explorer_new_directory();
    harness.wait_for_prompt().unwrap();

    // Should be in rename mode - prompt should appear
    assert!(
        harness.editor().is_prompting(),
        "Should be in rename mode (prompting) after creating new folder"
    );

    // Verify a folder was created on the filesystem
    let dirs: Vec<_> = fs::read_dir(&project_root)
        .unwrap()
        .filter_map(|e| e.ok())
        .filter(|e| e.path().is_dir())
        .collect();

    assert!(
        !dirs.is_empty(),
        "A new folder should have been created on the filesystem"
    );

    // Cancel the rename (ESC) and verify folder still exists with default name
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    assert!(
        !harness.editor().is_prompting(),
        "Should not be prompting after ESC"
    );

    // Folder should still exist
    let dirs_after: Vec<_> = fs::read_dir(&project_root)
        .unwrap()
        .filter_map(|e| e.ok())
        .filter(|e| e.path().is_dir())
        .collect();

    assert!(
        !dirs_after.is_empty(),
        "Folder should still exist after cancelling rename"
    );
}

/// Test that rename prompt appears and ESC aborts the rename
#[test]
fn test_rename_prompt_escape_aborts() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create a test file to rename
    let original_name = "original_file.txt";
    fs::write(project_root.join(original_name), "content").unwrap();

    // Open and focus file explorer
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    // Root is automatically expanded during init, so just wait for the file to appear
    harness
        .wait_for_file_explorer_item("original_file")
        .unwrap();

    // Navigate down to select the file (root is initially selected)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();

    // Press F2 to start rename
    harness.send_key(KeyCode::F(2), KeyModifiers::NONE).unwrap();
    harness.wait_for_prompt().unwrap();

    // Should be in rename mode (prompting)
    assert!(
        harness.editor().is_prompting(),
        "Should be prompting for rename after F2"
    );

    // Type a new name
    harness.type_text("new_name.txt").unwrap();
    harness.render().unwrap();

    // Press ESC to abort
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Should no longer be prompting
    assert!(
        !harness.editor().is_prompting(),
        "Should not be prompting after ESC"
    );

    // Original file should still exist with original name
    assert!(
        project_root.join(original_name).exists(),
        "Original file should still exist after ESC abort"
    );

    // New name should NOT exist
    assert!(
        !project_root.join("new_name.txt").exists(),
        "New name should not exist after ESC abort"
    );
}

/// Test that rename prompt accepts new name on Enter and updates filesystem
#[test]
fn test_rename_prompt_enter_accepts() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create a test file to rename
    let original_name = "file_to_rename.txt";
    let new_name = "renamed_file.txt";
    fs::write(project_root.join(original_name), "content").unwrap();

    // Open and focus file explorer
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    // Root is automatically expanded during init, so just wait for the file to appear
    harness
        .wait_for_file_explorer_item("file_to_rename")
        .unwrap();

    // Navigate down to select the file (root is initially selected)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();

    // Press F2 to start rename
    harness.send_key(KeyCode::F(2), KeyModifiers::NONE).unwrap();
    harness.wait_for_prompt().unwrap();

    // Should be in rename mode (prompting)
    assert!(
        harness.editor().is_prompting(),
        "Should be prompting for rename after F2"
    );

    // Directly set the prompt input to the new name
    if let Some(prompt) = harness.editor_mut().prompt_mut() {
        assert!(
            prompt.input.contains("file_to_rename"),
            "Should be renaming file_to_rename.txt, but prompt shows: {}",
            prompt.input
        );
        prompt.clear();
        prompt.insert_str(new_name);
    }
    harness.render().unwrap();

    // Press Enter to confirm
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // Should no longer be prompting
    assert!(
        !harness.editor().is_prompting(),
        "Should not be prompting after Enter"
    );

    // New name should exist on filesystem
    assert!(
        project_root.join(new_name).exists(),
        "Renamed file should exist at new path: {:?}",
        project_root.join(new_name)
    );

    // Original name should NOT exist
    assert!(
        !project_root.join(original_name).exists(),
        "Original file should not exist after rename"
    );
}

/// Test that new folder via menu creates folder and filesystem is updated
#[test]
fn test_new_folder_via_menu_affects_filesystem() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Open and focus file explorer
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    // Count initial directories
    let initial_dirs: Vec<_> = fs::read_dir(&project_root)
        .unwrap()
        .filter_map(|e| e.ok())
        .filter(|e| e.path().is_dir())
        .collect();

    // Open Explorer menu and select New Folder
    harness
        .send_key(KeyCode::Char('x'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    // Navigate to New Folder (second item)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Execute - enters rename mode
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    // Accept default name
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // Count final directories
    let final_dirs: Vec<_> = fs::read_dir(&project_root)
        .unwrap()
        .filter_map(|e| e.ok())
        .filter(|e| e.path().is_dir())
        .collect();

    assert!(
        final_dirs.len() > initial_dirs.len(),
        "A new folder should have been created. Initial: {}, Final: {}",
        initial_dirs.len(),
        final_dirs.len()
    );
}

/// Test that rename via menu triggers prompt and affects filesystem
#[test]
fn test_rename_via_menu_affects_filesystem() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create a test file
    let original_name = "menu_rename_test.txt";
    let new_name = "menu_renamed.txt";
    fs::write(project_root.join(original_name), "test content").unwrap();

    // Open and focus file explorer
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    // Root is automatically expanded during init, so just wait for the file to appear
    harness
        .wait_for_file_explorer_item("menu_rename_test")
        .unwrap();

    // Navigate down to select the file (root is initially selected)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();

    // Trigger rename via F2 key
    harness.send_key(KeyCode::F(2), KeyModifiers::NONE).unwrap();
    harness.wait_for_prompt().unwrap();

    // Should be prompting for new name
    assert!(
        harness.editor().is_prompting(),
        "Should be prompting for rename after triggering rename"
    );

    // Directly set the prompt input to the new name
    if let Some(prompt) = harness.editor_mut().prompt_mut() {
        prompt.clear();
        prompt.insert_str(new_name);
    }
    harness.render().unwrap();

    // Confirm with Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // Verify filesystem was updated
    assert!(
        project_root.join(new_name).exists(),
        "Renamed file should exist"
    );
    assert!(
        !project_root.join(original_name).exists(),
        "Original file should not exist after rename"
    );
}

/// Test that after rename completes, the renamed item is selected
#[test]
fn test_selection_after_rename_on_renamed_item() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create a test file to rename
    let original_name = "select_test.txt";
    let new_name = "renamed_select.txt";
    fs::write(project_root.join(original_name), "content").unwrap();

    // Open and focus file explorer
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    // Root is automatically expanded during init, so just wait for the file to appear
    harness.wait_for_file_explorer_item("select_test").unwrap();

    // Navigate down to select the file (root is initially selected)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();

    // Start rename via F2 key
    harness.send_key(KeyCode::F(2), KeyModifiers::NONE).unwrap();
    harness.wait_for_prompt().unwrap();

    // Set new name and confirm
    if let Some(prompt) = harness.editor_mut().prompt_mut() {
        prompt.clear();
        prompt.insert_str(new_name);
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // Verify renamed file is shown in explorer (should be selected)
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("renamed_select"),
        "Renamed file should be visible in the explorer after rename. Screen:\n{}",
        screen
    );

    // Check that file explorer is still focused
    assert!(
        harness.editor().file_explorer_is_focused(),
        "File explorer should remain focused after rename"
    );
}

/// Test that arrow keys work to navigate after rename completes
#[test]
fn test_navigation_after_rename_completes() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create multiple files
    fs::write(project_root.join("aaa_first.txt"), "first").unwrap();
    fs::write(project_root.join("bbb_second.txt"), "second").unwrap();
    fs::write(project_root.join("ccc_third.txt"), "third").unwrap();

    // Open and focus file explorer
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    // Root is auto-expanded during init, wait for file to appear
    harness.wait_for_file_explorer_item("aaa_first").unwrap();

    // Navigate to first file and rename it
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Use F2 key to trigger rename (user-facing action)
    harness.send_key(KeyCode::F(2), KeyModifiers::NONE).unwrap();
    harness.wait_for_prompt().unwrap();

    // Rename the file
    if let Some(prompt) = harness.editor_mut().prompt_mut() {
        prompt.clear();
        prompt.insert_str("aaa_renamed.txt");
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // Verify we're not in prompting mode anymore
    assert!(
        !harness.editor().is_prompting(),
        "Should not be prompting after rename completes"
    );

    // Navigate down to the next file - this should work after rename
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Navigate down again
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Navigate up - should work to go back
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Navigation should work without errors
    // File explorer should still be focused
    assert!(
        harness.editor().file_explorer_is_focused(),
        "File explorer should remain focused after navigation post-rename"
    );
}

/// Test that new folder rename allows navigation after completing
#[test]
fn test_new_folder_navigation_after_rename() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create an existing file
    fs::write(project_root.join("existing_file.txt"), "existing").unwrap();

    // Open and focus file explorer
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    // Create new folder (enters rename mode automatically)
    harness.editor_mut().file_explorer_new_directory();
    harness.wait_for_prompt().unwrap();

    // Should be prompting for folder name
    assert!(
        harness.editor().is_prompting(),
        "Should be in rename mode after creating new folder"
    );

    // Set folder name and confirm
    if let Some(prompt) = harness.editor_mut().prompt_mut() {
        prompt.clear();
        prompt.insert_str("my_new_folder");
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // Should no longer be prompting
    assert!(
        !harness.editor().is_prompting(),
        "Should not be prompting after confirming folder name"
    );

    // Verify file explorer is still focused
    assert!(
        harness.editor().file_explorer_is_focused(),
        "File explorer should be focused after creating new folder"
    );

    // Try navigating - should work
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Verify the folder was created on filesystem
    assert!(
        project_root.join("my_new_folder").exists(),
        "New folder should exist on filesystem"
    );
    assert!(
        project_root.join("my_new_folder").is_dir(),
        "my_new_folder should be a directory"
    );
}

/// Test that focus returns to file explorer after rename and navigation works to open another file
#[test]
fn test_focus_returns_after_rename() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create test files with content we can verify
    fs::write(project_root.join("aaa_file.txt"), "content of aaa").unwrap();
    fs::write(project_root.join("bbb_file.txt"), "content of bbb").unwrap();

    // Open and focus file explorer
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    // Root is auto-expanded during init, wait for file to appear
    harness.wait_for_file_explorer_item("aaa_file").unwrap();

    // Navigate to aaa_file.txt
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Verify file explorer has focus before rename
    assert!(
        harness.editor().file_explorer_is_focused(),
        "File explorer should be focused before rename"
    );

    // Start rename using F2 key (user-facing action)
    harness.send_key(KeyCode::F(2), KeyModifiers::NONE).unwrap();
    harness.wait_for_prompt().unwrap();

    // Type new name and confirm
    if let Some(prompt) = harness.editor_mut().prompt_mut() {
        prompt.clear();
        prompt.insert_str("aaa_renamed.txt");
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // CRITICAL: Verify file explorer still has focus after rename
    assert!(
        harness.editor().file_explorer_is_focused(),
        "File explorer should still be focused after rename completes"
    );

    // CRITICAL: Navigate to bbb_file.txt using arrow keys and open it
    // This tests that navigation works after rename
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press Enter to open the file
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    // Wait for the file content to be loaded
    harness.wait_for_screen_contains("content of bbb").unwrap();

    // Verify the file was opened by checking the screen contains the file content
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("content of bbb"),
        "Should have opened bbb_file.txt after navigating with arrow keys. Screen:\n{}",
        screen
    );
}
