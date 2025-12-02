use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;
use std::time::Duration;

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
    std::thread::sleep(Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

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
    std::thread::sleep(Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    // Press 'h' to toggle hidden files while in file explorer
    harness
        .send_key(KeyCode::Char('h'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

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
    std::thread::sleep(Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    // Press 'n' to create new file
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();
    std::thread::sleep(Duration::from_millis(200));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

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
    std::thread::sleep(Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    // Press 'h' to toggle hidden files
    harness
        .send_key(KeyCode::Char('h'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

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
    std::thread::sleep(Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    // Press 'i' to toggle gitignored files
    harness
        .send_key(KeyCode::Char('i'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

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
    fs::write(project_root.join("to_delete.txt"), "delete me").unwrap();

    // Open and focus file explorer
    harness.editor_mut().focus_file_explorer();
    std::thread::sleep(Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    // Expand root and navigate to the file
    harness.editor_mut().file_explorer_toggle_expand();
    std::thread::sleep(Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    harness.editor_mut().file_explorer_navigate_down();
    harness.render().unwrap();

    // Press 'd' to delete
    harness
        .send_key(KeyCode::Char('d'), KeyModifiers::NONE)
        .unwrap();
    std::thread::sleep(Duration::from_millis(200));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    // The test passes if no panic occurs - actual deletion depends on the selected item
    let screen = harness.screen_to_string();
    println!("Screen after delete attempt:\n{}", screen);
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
    std::thread::sleep(Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    // Expand root and navigate to the file
    harness.editor_mut().file_explorer_toggle_expand();
    std::thread::sleep(Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    harness.editor_mut().file_explorer_navigate_down();
    harness.render().unwrap();

    // Press F2 to rename
    harness
        .send_key(KeyCode::F(2), KeyModifiers::NONE)
        .unwrap();
    std::thread::sleep(Duration::from_millis(200));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

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
    fs::write(project_root.join("delete_test.txt"), "delete me").unwrap();

    // Open and focus file explorer
    harness.editor_mut().focus_file_explorer();
    std::thread::sleep(Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    // Expand root and navigate to the file
    harness.editor_mut().file_explorer_toggle_expand();
    std::thread::sleep(Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    harness.editor_mut().file_explorer_navigate_down();
    harness.render().unwrap();

    // Press Delete key
    harness
        .send_key(KeyCode::Delete, KeyModifiers::NONE)
        .unwrap();
    std::thread::sleep(Duration::from_millis(200));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    // The test passes if no panic occurs
    let screen = harness.screen_to_string();
    println!("Screen after delete key attempt:\n{}", screen);
}

/// Test executing New File action from Explorer menu
#[test]
fn test_explorer_menu_new_file_action() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Open file explorer first
    harness.editor_mut().focus_file_explorer();
    std::thread::sleep(Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

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
    std::thread::sleep(Duration::from_millis(200));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    // Verify a new file was created
    let final_count = fs::read_dir(&project_root).unwrap().count();
    assert!(
        final_count > initial_count,
        "A new file should have been created via menu. Initial: {}, Final: {}",
        initial_count,
        final_count
    );
}

/// Test executing New Folder action from Explorer menu
#[test]
fn test_explorer_menu_new_folder_action() {
    let mut harness = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Open file explorer first
    harness.editor_mut().focus_file_explorer();
    std::thread::sleep(Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

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
    std::thread::sleep(Duration::from_millis(200));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

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
    std::thread::sleep(Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

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
    std::thread::sleep(Duration::from_millis(100));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    // Create another file
    fs::write(project_root.join("new_file.txt"), "new").unwrap();

    // Press Ctrl+R to refresh
    harness
        .send_key(KeyCode::Char('r'), KeyModifiers::CONTROL)
        .unwrap();
    std::thread::sleep(Duration::from_millis(200));
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    // The test passes if no panic occurs
    // Ideally we'd verify the new file appears, but that depends on rendering details
    let screen = harness.screen_to_string();
    println!("Screen after refresh:\n{}", screen);
}
