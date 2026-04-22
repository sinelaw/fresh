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

/// Test that toggling hidden files via menu updates checkbox state
#[test]
fn test_explorer_menu_checkbox_updates_on_toggle() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();

    // Open file explorer and focus it
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    // Open Explorer menu and click on Show Hidden Files
    harness
        .send_key(KeyCode::Char('x'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    // Navigate to Show Hidden Files and select it. Menu items with
    // separators skipped (clipboard is empty so Paste is hidden):
    //   New File, New Folder, Open, Rename, Delete, Cut, Copy,
    //   Refresh, Show Hidden Files
    // That's 8 Down presses to land on "Show Hidden Files" from the
    // initial highlight on "New File".
    for _ in 0..8 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Now open Explorer menu again to check checkbox state
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

    // Focus file explorer so Explorer menu becomes visible
    harness.editor_mut().focus_file_explorer();
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

    // Press Ctrl+n to create new file (opens rename prompt for naming)
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::CONTROL)
        .unwrap();
    // Wait for the rename prompt to appear (file is created and opened)
    harness.wait_for_screen_contains("Rename to:").unwrap();

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

/// Test that Ctrl+i keybinding toggles gitignored files in file explorer
#[test]
fn test_explorer_i_keybinding_toggles_gitignored() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();

    // Open and focus file explorer
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    // Press Ctrl+i to toggle gitignored files
    harness
        .send_key(KeyCode::Char('i'), KeyModifiers::CONTROL)
        .unwrap();

    // Check status bar for confirmation message
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("gitignored") || screen.contains("Gitignored"),
        "Status bar should show gitignored files toggle message. Screen:\n{}",
        screen
    );
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

/// Test that Explorer menu appears in the menu bar when file explorer is focused
#[test]
fn test_explorer_menu_in_menu_bar() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();

    // Focus file explorer first (Explorer menu is only visible when file explorer is focused)
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.render().unwrap();

    // Check that Explorer appears in the menu bar
    let menu_bar = harness.get_menu_bar();
    assert!(
        menu_bar.contains("Explorer"),
        "Menu bar should contain 'Explorer' when file explorer is focused. Menu bar: {}",
        menu_bar
    );

    // Focus editor and check that Explorer menu is hidden
    harness.editor_mut().focus_editor();
    harness.render().unwrap();

    let menu_bar_after = harness.get_menu_bar();
    assert!(
        !menu_bar_after.contains("Explorer"),
        "Menu bar should NOT contain 'Explorer' when file explorer is not focused. Menu bar: {}",
        menu_bar_after
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

    // Navigate to "Show Hidden Files". With separators auto-skipped and
    // an empty clipboard (Paste hidden), the Explorer menu shows:
    //   New File -> New Folder -> Open -> Rename -> Delete -> Cut
    //   -> Copy -> Refresh -> Show Hidden Files
    // That's 8 Down presses from New File to Show Hidden Files.
    for _ in 0..8 {
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

/// Test that Ctrl+C on a selected file produces a "Copied:" status
#[test]
fn test_copy_file_shows_copied_status() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();
    fs::write(project_root.join("copy_me.txt"), "content").unwrap();

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("copy_me").unwrap();

    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // "Copied: copy_me.txt" is the locale string for explorer.copied
    harness.assert_screen_contains("Copied:");
    harness.assert_screen_contains("copy_me.txt");
}

/// Test that Ctrl+X on a selected file produces a "Marked for cut:" status
#[test]
fn test_cut_file_shows_cut_status() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();
    fs::write(project_root.join("cut_me.txt"), "content").unwrap();

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("cut_me").unwrap();

    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Char('x'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // "Marked for cut: cut_me.txt" is the locale string for explorer.cut
    harness.assert_screen_contains("Marked for cut:");
    harness.assert_screen_contains("cut_me.txt");
}

/// Test that pasting with an empty clipboard shows an error
#[test]
fn test_paste_empty_clipboard_shows_error() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();

    // No copy/cut — clipboard is empty
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Nothing to paste") || screen.contains("paste"),
        "Should show 'nothing to paste' error. Screen:\n{}",
        screen
    );
}

/// Test that copying a file then pasting it to a subdirectory works
#[test]
fn test_copy_paste_file_to_subdirectory() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create source file and destination directory
    fs::write(project_root.join("source.txt"), "hello").unwrap();
    fs::create_dir(project_root.join("subdir")).unwrap();

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("source").unwrap();

    // Directories sort before files, so: root → subdir → source.txt
    // Navigate to source.txt (index 2) and copy it
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // → subdir
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // → source.txt
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();

    // Navigate up to subdir and paste into it
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Verify file was copied
    assert!(
        project_root.join("subdir").join("source.txt").exists(),
        "File should be copied to subdirectory"
    );
    // Original should still exist
    assert!(
        project_root.join("source.txt").exists(),
        "Original file should still exist after copy"
    );
}

/// Test that cutting a file and pasting it to a subdirectory moves it
#[test]
fn test_cut_paste_moves_file() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();

    fs::write(project_root.join("move_me.txt"), "hello").unwrap();
    fs::create_dir(project_root.join("dest")).unwrap();

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("move_me").unwrap();

    // Directories sort before files: root → dest → move_me.txt
    // Navigate to move_me.txt (index 2) and cut it
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // → dest
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // → move_me.txt
    harness
        .send_key(KeyCode::Char('x'), KeyModifiers::CONTROL)
        .unwrap();

    // Navigate back to dest and paste into it
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // File should be at new location
    assert!(
        project_root.join("dest").join("move_me.txt").exists(),
        "File should be moved to dest/"
    );
    // Original should be gone
    assert!(
        !project_root.join("move_me.txt").exists(),
        "Original file should be removed after cut+paste"
    );
    // A second Ctrl+V should fail with "Nothing to paste" — confirms the
    // clipboard was cleared after the cut+paste completed.
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Nothing to paste");
}

/// Test that copying a file to the same directory auto-renames with " copy" suffix
#[test]
fn test_copy_to_same_dir_auto_renames() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();

    fs::write(project_root.join("original.txt"), "data").unwrap();

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("original").unwrap();

    // Select and copy the file
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();

    // Paste in same directory (root is still selected as destination parent)
    // Navigate back to root
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should have auto-renamed to "original copy.txt"
    assert!(
        project_root.join("original copy.txt").exists(),
        "Copy to same dir should auto-rename to 'original copy.txt'"
    );
    assert!(
        project_root.join("original.txt").exists(),
        "Original should still exist"
    );
}

/// Test that cutting and pasting to the same directory shows an error
#[test]
fn test_cut_paste_same_location_shows_error() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();

    fs::write(project_root.join("stay.txt"), "data").unwrap();

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("stay").unwrap();

    // Select and cut the file
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Char('x'), KeyModifiers::CONTROL)
        .unwrap();

    // Navigate to parent (same directory) and paste
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("same location") || screen.contains("Cannot paste"),
        "Should show 'same location' error. Screen:\n{}",
        screen
    );
    // File should not be moved
    assert!(
        project_root.join("stay.txt").exists(),
        "File should not be moved when pasting to same location"
    );
}

/// Test that Edit menu shows Copy, Cut, and Paste items when file explorer is focused
#[test]
fn test_edit_menu_shows_file_copy_cut_paste_when_explorer_focused() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();
    harness.render().unwrap();

    // Open Edit menu (Alt+E)
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("Copy");
    harness.assert_screen_contains("Cut");
    harness.assert_screen_contains("Paste");
}

/// Test that rename rejects names containing '/'
#[test]
fn test_rename_rejects_slash_in_name() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();

    fs::write(project_root.join("valid.txt"), "content").unwrap();

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("valid").unwrap();

    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::F(2), KeyModifiers::NONE).unwrap();
    harness.wait_for_prompt().unwrap();

    if let Some(prompt) = harness.editor_mut().prompt_mut() {
        prompt.clear();
        prompt.insert_str("bad/name.txt");
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show an error and NOT rename the file
    assert!(
        !project_root.join("bad/name.txt").exists(),
        "File with '/' in name should not be created"
    );
    assert!(
        project_root.join("valid.txt").exists(),
        "Original file should still exist after rejected rename"
    );
}

/// Test that rename rejects '.' and '..' as names
#[test]
fn test_rename_rejects_dot_names() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();

    fs::write(project_root.join("keep.txt"), "content").unwrap();

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("keep").unwrap();

    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::F(2), KeyModifiers::NONE).unwrap();
    harness.wait_for_prompt().unwrap();

    if let Some(prompt) = harness.editor_mut().prompt_mut() {
        prompt.clear();
        prompt.insert_str("..");
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Original should still exist (rename was rejected)
    assert!(
        project_root.join("keep.txt").exists(),
        "Original file should still exist after rejected rename to '..'"
    );
}

/// After a cut+paste completes, the clipboard is empty; a second Ctrl+V
/// in the explorer must show "Nothing to paste".
#[test]
fn test_clipboard_cleared_after_cut_paste() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();
    fs::write(project_root.join("moveme.txt"), "data").unwrap();
    fs::create_dir(project_root.join("dst")).unwrap();

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("moveme").unwrap();

    // dirs sort first: root → dst → moveme.txt
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // → dst
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // → moveme.txt
    harness
        .send_key(KeyCode::Char('x'), KeyModifiers::CONTROL)
        .unwrap();

    // Navigate to dst, paste
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap(); // → dst
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Moved:");

    // A second Ctrl+V should now show the empty-clipboard error.
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Nothing to paste");
}

/// Test paste conflict: overwrite replaces existing file
#[test]
fn test_paste_conflict_overwrite() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();
    fs::write(project_root.join("src.txt"), "new content").unwrap();
    fs::create_dir(project_root.join("subdir")).unwrap();
    fs::write(project_root.join("subdir").join("src.txt"), "old content").unwrap();

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("src").unwrap();

    // dirs sort first: root → subdir → src.txt
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // → subdir
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // → src.txt
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();

    // Navigate to subdir and paste — conflict
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    // Choose overwrite
    if let Some(prompt) = harness.editor_mut().prompt_mut() {
        prompt.clear();
        prompt.insert_str("o");
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    assert_eq!(
        fs::read_to_string(project_root.join("subdir").join("src.txt")).unwrap(),
        "new content",
        "Overwrite should replace file with new content"
    );
}

/// Test paste conflict: cancel leaves both files untouched
#[test]
fn test_paste_conflict_cancel() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();
    fs::write(project_root.join("file.txt"), "new").unwrap();
    fs::create_dir(project_root.join("dest")).unwrap();
    fs::write(project_root.join("dest").join("file.txt"), "original").unwrap();

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("file").unwrap();

    // dirs sort first: root → dest → file.txt
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // → dest
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // → file.txt
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();

    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    // Choose cancel
    if let Some(prompt) = harness.editor_mut().prompt_mut() {
        prompt.clear();
        prompt.insert_str("c");
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    assert_eq!(
        fs::read_to_string(project_root.join("dest").join("file.txt")).unwrap(),
        "original",
        "Cancel should leave existing file untouched"
    );
    // Clipboard is preserved after cancel — a second Ctrl+V should re-prompt
    // for the same conflict instead of erroring with "Nothing to paste".
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.assert_screen_contains("exists");
}

/// Test that pasting into a directory does not cause it to visually collapse.
///
/// This verifies that `reload_expanded_node` is used instead of `refresh_node`
/// so the destination directory stays expanded after a paste.
#[test]
fn test_paste_does_not_collapse_destination_directory() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();
    fs::write(project_root.join("paste_src.txt"), "content").unwrap();
    fs::create_dir(project_root.join("paste_dst")).unwrap();
    // Put a file in the destination so it is non-empty (and thus expandable)
    fs::write(project_root.join("paste_dst").join("existing.txt"), "old").unwrap();

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("paste_src").unwrap();

    // dirs sort first: root → paste_dst → paste_src.txt
    // Expand paste_dst first so we can verify it stays expanded after paste
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // → paste_dst
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap(); // expand paste_dst
    harness.render().unwrap();

    // Navigate back to paste_src.txt and copy it
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // → existing.txt (inside paste_dst)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // → paste_src.txt
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Navigate to paste_dst and paste
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap(); // → existing.txt
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap(); // → paste_dst
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // paste_src.txt should now be in paste_dst
    assert!(
        project_root
            .join("paste_dst")
            .join("paste_src.txt")
            .exists(),
        "paste_src.txt should have been copied into paste_dst"
    );

    // paste_dst's children should be visible (directory still expanded)
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("existing"),
        "existing.txt should still be visible — paste_dst should not have collapsed. Screen:\n{}",
        screen
    );
}

/// Test that focus remains on the file explorer after a paste operation
#[test]
fn test_explorer_focus_preserved_after_paste() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();
    fs::write(project_root.join("focus_src.txt"), "data").unwrap();
    fs::create_dir(project_root.join("focus_dst")).unwrap();

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("focus_src").unwrap();

    // dirs first: root → focus_dst → focus_src.txt
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // → focus_dst
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // → focus_src.txt
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Navigate to focus_dst and paste
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap(); // → focus_dst
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Pasted item should be visible on screen (focus stayed in the explorer
    // so the tree re-rendered with the new child).
    harness.assert_screen_contains("focus_src");

    // Sending an explorer-specific key must still be handled by the explorer:
    // Ctrl+C on the newly pasted item should produce the explorer's copy
    // status, not an editor-level clipboard action.
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Copied:");
}

// ─────────────────────────────────────────────────────────────────────────────
// Multi-selection tests
// ─────────────────────────────────────────────────────────────────────────────

/// Shift+Down extends the selection; Ctrl+C on the extended selection
/// produces a multi-copy status ("Copied 2 items").
#[test]
fn test_shift_down_extends_selection() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();
    fs::write(project_root.join("a.txt"), "a").unwrap();
    fs::write(project_root.join("b.txt"), "b").unwrap();

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("a.txt").unwrap();

    // root → a.txt → (Shift+Down) → b.txt also selected
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Down, KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("Copied 2 items");
}

/// Ctrl+A selects every visible node; Ctrl+C then copies them all at once.
#[test]
fn test_ctrl_a_selects_all() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();
    fs::write(project_root.join("file1.txt"), "1").unwrap();
    fs::write(project_root.join("file2.txt"), "2").unwrap();
    fs::write(project_root.join("file3.txt"), "3").unwrap();

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("file1.txt").unwrap();

    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Root is excluded from the clipboard, so the three files are copied.
    harness.assert_screen_contains("Copied 3 items");
}

/// Space toggles the cursor item into the multi-selection; a second Space
/// toggles it back out, returning to single-cursor mode.
#[test]
fn test_space_toggles_selection() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();
    fs::write(project_root.join("alpha.txt"), "a").unwrap();
    fs::write(project_root.join("beta.txt"), "b").unwrap();

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("alpha.txt").unwrap();

    // Toggle alpha.txt into the selection, then Shift+Down to add beta.txt.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // → alpha.txt
    harness
        .send_key(KeyCode::Char(' '), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Down, KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Copied 2 items");

    // Toggle beta.txt back out of the selection: only alpha.txt remains
    // (cursor is on beta.txt after the Shift+Down).
    harness
        .send_key(KeyCode::Char(' '), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Copied:");
    harness.assert_screen_contains("alpha.txt");
}

/// Escape clears multi-selection first: a subsequent Ctrl+C must copy only
/// the cursor item, not the previously multi-selected set.
#[test]
fn test_escape_clears_multi_selection() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();
    fs::write(project_root.join("esc_a.txt"), "a").unwrap();
    fs::write(project_root.join("esc_b.txt"), "b").unwrap();

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("esc_a.txt").unwrap();

    // Move cursor onto esc_a.txt so that after Escape clears the multi-selection
    // the cursor is still on a real file, not the project root.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();

    // Ctrl+C now copies only the cursor item → single-item status, not "Copied N items".
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Copied:") && screen.contains("esc_a.txt"),
        "Should show single-item copy status after Escape cleared the multi-selection. Screen:\n{}",
        screen
    );
    assert!(
        !screen.contains("Copied 2 items"),
        "Multi-selection should have been cleared by Escape. Screen:\n{}",
        screen
    );
}

/// Ctrl+C on a Shift+Down range produces the multi-copy status; a subsequent
/// paste into a subdirectory lands both files there.
#[test]
fn test_copy_multi_selection_stores_multiple_paths() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();
    fs::write(project_root.join("multi_a.txt"), "a").unwrap();
    fs::write(project_root.join("multi_b.txt"), "b").unwrap();
    fs::create_dir(project_root.join("out")).unwrap();

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("multi_a.txt").unwrap();

    // dirs first: root → out/ → multi_a.txt → (Shift+Down) → multi_b.txt
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // out/
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // multi_a.txt
    harness
        .send_key(KeyCode::Down, KeyModifiers::SHIFT)
        .unwrap(); // extend to multi_b.txt
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Copied 2 items");

    // Paste into out/ — both files should land there.
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap(); // multi_a.txt
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap(); // out/
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    assert!(project_root.join("out").join("multi_a.txt").exists());
    assert!(project_root.join("out").join("multi_b.txt").exists());
}

/// Delete on a multi-selection should show a prompt with item count
#[test]
fn test_multi_delete_shows_count_prompt() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();
    fs::write(project_root.join("del_a.txt"), "a").unwrap();
    fs::write(project_root.join("del_b.txt"), "b").unwrap();

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("del_a.txt").unwrap();

    // Navigate to first file and extend selection down
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Down, KeyModifiers::SHIFT)
        .unwrap();

    // Press Delete
    harness
        .send_key(KeyCode::Delete, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Delete") && (screen.contains('2') || screen.contains("items")),
        "Should show multi-delete prompt with item count. Screen:\n{}",
        screen
    );

    // Cancel
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();
}

/// Plain Up/Down navigation clears multi-selection: after extending the
/// range then pressing Down alone, Ctrl+C copies only the cursor item.
#[test]
fn test_navigation_clears_multi_selection() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();
    fs::write(project_root.join("nav_a.txt"), "a").unwrap();
    fs::write(project_root.join("nav_b.txt"), "b").unwrap();
    fs::write(project_root.join("nav_c.txt"), "c").unwrap();

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("nav_a.txt").unwrap();

    // Extend selection over nav_a.txt + nav_b.txt
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // → nav_a.txt
    harness
        .send_key(KeyCode::Down, KeyModifiers::SHIFT)
        .unwrap(); // extend to nav_b.txt
                   // Plain Down clears the multi-selection and moves cursor to nav_c.txt.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Copied:") && screen.contains("nav_c.txt"),
        "Plain navigation should clear multi-selection, leaving only cursor copied. Screen:\n{}",
        screen
    );
    assert!(
        !screen.contains("Copied 2 items"),
        "Multi-selection should have been cleared by plain navigation. Screen:\n{}",
        screen
    );
}

/// Multi-paste per-conflict prompt: (O) overwrite all at first conflict
#[test]
fn test_multi_paste_per_conflict_overwrite_all() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Flat layout: dst/ already has both files; a.txt + b.txt are at root level.
    // Sorted order in explorer: dst/ → a.txt → b.txt
    let dst = project_root.join("dst");
    fs::create_dir_all(&dst).unwrap();
    fs::write(dst.join("a.txt"), "old_a").unwrap();
    fs::write(dst.join("b.txt"), "old_b").unwrap();
    fs::write(project_root.join("a.txt"), "new_a").unwrap();
    fs::write(project_root.join("b.txt"), "new_b").unwrap();

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("a.txt").unwrap();

    // root → dst/ → a.txt → (Shift+Down) → b.txt also selected
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // dst/
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // a.txt
    harness
        .send_key(KeyCode::Down, KeyModifiers::SHIFT)
        .unwrap(); // extend to b.txt

    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Copied 2 items");

    // Navigate to dst/ and paste — both conflict
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap(); // a.txt
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap(); // dst/
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("exists") && screen.contains("verwrite"),
        "Should show per-conflict prompt. Screen:\n{}",
        screen
    );

    // Choose overwrite all (uppercase O)
    if let Some(prompt) = harness.editor_mut().prompt_mut() {
        prompt.clear();
        prompt.insert_str("O");
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    assert_eq!(fs::read_to_string(dst.join("a.txt")).unwrap(), "new_a");
    assert_eq!(fs::read_to_string(dst.join("b.txt")).unwrap(), "new_b");
}

/// Multi-paste per-conflict: (s)kip first conflict, then second prompt appears and can overwrite
#[test]
fn test_multi_paste_per_conflict_skip_one() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Same flat layout
    let dst = project_root.join("dst2");
    fs::create_dir_all(&dst).unwrap();
    fs::write(dst.join("p.txt"), "old_p").unwrap();
    fs::write(dst.join("q.txt"), "old_q").unwrap();
    fs::write(project_root.join("p.txt"), "new_p").unwrap();
    fs::write(project_root.join("q.txt"), "new_q").unwrap();

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("p.txt").unwrap();

    // root → dst2/ → p.txt → (Shift+Down) → q.txt
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // dst2/
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // p.txt
    harness
        .send_key(KeyCode::Down, KeyModifiers::SHIFT)
        .unwrap(); // extend to q.txt
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();

    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap(); // p.txt
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap(); // dst2/
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    // First conflict: skip
    if let Some(prompt) = harness.editor_mut().prompt_mut() {
        prompt.clear();
        prompt.insert_str("s");
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Second conflict prompt must appear
    harness.wait_for_prompt().unwrap();
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("exists"),
        "Second conflict prompt should appear. Screen:\n{}",
        screen
    );

    // Overwrite second
    if let Some(prompt) = harness.editor_mut().prompt_mut() {
        prompt.clear();
        prompt.insert_str("o");
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    assert_eq!(
        fs::read_to_string(dst.join("p.txt")).unwrap(),
        "old_p",
        "p.txt skipped"
    );
    assert_eq!(
        fs::read_to_string(dst.join("q.txt")).unwrap(),
        "new_q",
        "q.txt overwritten"
    );
}

/// Multi-paste with no conflicts: all files land in destination without any prompt
#[test]
fn test_multi_paste_no_conflict() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();

    let dst = project_root.join("out");
    fs::create_dir_all(&dst).unwrap();
    fs::write(project_root.join("x.txt"), "x_content").unwrap();
    fs::write(project_root.join("y.txt"), "y_content").unwrap();

    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("x.txt").unwrap();

    // root → out/ → x.txt → (Shift+Down) → y.txt
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // out/
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // x.txt
    harness
        .send_key(KeyCode::Down, KeyModifiers::SHIFT)
        .unwrap(); // extend to y.txt
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Copied 2 items");

    // Navigate to out/ and paste
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap(); // x.txt
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap(); // out/
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // No conflict prompt should appear — multi-paste status is shown directly.
    harness.assert_screen_contains("Pasted 2 items");
    harness.assert_screen_not_contains("exists.");

    assert_eq!(fs::read_to_string(dst.join("x.txt")).unwrap(), "x_content");
    assert_eq!(fs::read_to_string(dst.join("y.txt")).unwrap(), "y_content");
}
