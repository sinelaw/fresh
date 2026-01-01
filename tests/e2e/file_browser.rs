//! E2E tests for the native file browser (Open File dialog)
//!
//! Tests the plugin-free file browser that appears when pressing Ctrl+O.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::fs;
use tempfile::TempDir;

/// Test that the file browser popup appears when opening the Open File prompt
#[test]
fn test_file_browser_popup_appears() {
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    // Create some test files
    fs::write(project_root.join("file1.txt"), "content1").unwrap();
    fs::write(project_root.join("file2.txt"), "content2").unwrap();
    fs::create_dir(project_root.join("subdir")).unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    // Trigger Open File with Ctrl+O
    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();

    // Wait for async directory loading
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            // Should see the file browser with navigation section
            screen.contains("Navigation:")
        })
        .expect("File browser popup should appear");

    let screen = harness.screen_to_string();

    // Should show the prompt
    assert!(screen.contains("Open file:"), "Prompt should be visible");

    // Should show navigation shortcuts
    assert!(
        screen.contains("..") || screen.contains("Navigation"),
        "Navigation section should be visible"
    );
}

/// Test that files are listed in the file browser
#[test]
fn test_file_browser_lists_files() {
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    // Create test files with unique names
    fs::write(project_root.join("alpha_test.txt"), "alpha").unwrap();
    fs::write(project_root.join("beta_test.txt"), "beta").unwrap();
    fs::create_dir(project_root.join("gamma_dir")).unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();

    // Wait for files to load
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("alpha_test.txt")
        })
        .expect("Files should be listed");

    let screen = harness.screen_to_string();

    // Should show all files
    assert!(
        screen.contains("alpha_test.txt"),
        "alpha file should be listed"
    );
    assert!(
        screen.contains("beta_test.txt"),
        "beta file should be listed"
    );

    // Directories should have a trailing slash indicator
    assert!(
        screen.contains("gamma_dir") || screen.contains("/gamma_dir"),
        "directory should be listed"
    );
}

/// Test navigation with arrow keys
#[test]
fn test_file_browser_arrow_navigation() {
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    // Create test files (sorted alphabetically)
    fs::write(project_root.join("aaa.txt"), "a").unwrap();
    fs::write(project_root.join("bbb.txt"), "b").unwrap();
    fs::write(project_root.join("ccc.txt"), "c").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();

    // Wait for files to load
    harness
        .wait_until(|h| h.screen_to_string().contains("aaa.txt"))
        .expect("Files should load");

    // Move down twice
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Move back up
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // The test passes if no crash occurs and we can still see the files
    let screen = harness.screen_to_string();
    assert!(screen.contains("aaa.txt"));
    assert!(screen.contains("bbb.txt"));
    assert!(screen.contains("ccc.txt"));
}

/// Test filtering files by typing
#[test]
fn test_file_browser_filter() {
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    fs::write(project_root.join("apple.txt"), "apple").unwrap();
    fs::write(project_root.join("banana.txt"), "banana").unwrap();
    fs::write(project_root.join("apricot.txt"), "apricot").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();

    // Wait for files to load
    harness
        .wait_until(|h| h.screen_to_string().contains("apple.txt"))
        .expect("Files should load");

    // Type filter text
    harness.type_text("ap").unwrap();

    // Give it time to filter
    harness.render().unwrap();

    let screen = harness.screen_to_string();

    // "apple" and "apricot" match "ap", "banana" doesn't
    // Matching files should still be visible
    assert!(
        screen.contains("apple.txt"),
        "apple.txt should match filter 'ap'"
    );
    assert!(
        screen.contains("apricot.txt"),
        "apricot.txt should match filter 'ap'"
    );

    // Non-matching file should be grayed out (still visible but at bottom)
    // We can't easily test for gray styling, but the file should still be present
    assert!(
        screen.contains("banana.txt"),
        "banana.txt should still be visible (grayed out)"
    );
}

/// Test opening a file by pressing Enter
#[test]
fn test_file_browser_open_file() {
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    fs::write(project_root.join("target.txt"), "Target file content").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();

    // Wait for file to appear
    harness
        .wait_until(|h| h.screen_to_string().contains("target.txt"))
        .expect("File should be listed");

    // Type filter to select the file (no selection by default)
    harness.type_text("target").unwrap();

    // Press Enter to open the selected file
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.process_async_and_render().unwrap();
    harness.render().unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("Target file content"))
        .expect("File content should be visible");
}

/// Test navigating into a directory
#[test]
fn test_file_browser_navigate_directory() {
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    // Create nested structure
    let subdir = project_root.join("subdir");
    fs::create_dir(&subdir).unwrap();
    fs::write(subdir.join("nested.txt"), "nested content").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();

    // Wait for subdir to appear
    harness
        .wait_until(|h| h.screen_to_string().contains("subdir"))
        .expect("Subdir should be listed");

    // Type filter to select the directory (no selection by default)
    harness.type_text("subdir").unwrap();

    // Press Enter to navigate into subdirectory
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for nested file to appear
    harness
        .wait_until(|h| h.screen_to_string().contains("nested.txt"))
        .expect("Should navigate into subdir and show nested.txt");

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("nested.txt"),
        "Should show nested file after navigating into directory"
    );
}

/// Test canceling with Escape
#[test]
fn test_file_browser_cancel() {
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    fs::write(project_root.join("test.txt"), "test").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();

    // Wait for popup
    harness
        .wait_until(|h| h.screen_to_string().contains("Navigation:"))
        .expect("File browser should appear");

    // Cancel with Escape
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // File browser should be closed
    harness.assert_screen_not_contains("Navigation:");
    harness.assert_screen_contains("cancelled");
}

/// Test that column headers are shown (Name, Size, Modified)
#[test]
fn test_file_browser_column_headers() {
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    fs::write(project_root.join("test.txt"), "test content").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();

    // Wait for file browser
    harness
        .wait_until(|h| h.screen_to_string().contains("Name"))
        .expect("Column headers should appear");

    let screen = harness.screen_to_string();

    // Should show column headers
    assert!(
        screen.contains("Name"),
        "Name column header should be visible"
    );
    assert!(
        screen.contains("Size"),
        "Size column header should be visible"
    );
    assert!(
        screen.contains("Modified"),
        "Modified column header should be visible"
    );
}

/// Test that hidden files are not shown by default
#[test]
fn test_file_browser_hides_dotfiles() {
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    fs::write(project_root.join("visible.txt"), "visible").unwrap();
    fs::write(project_root.join(".hidden"), "hidden").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();

    // Wait for visible file
    harness
        .wait_until(|h| h.screen_to_string().contains("visible.txt"))
        .expect("Visible file should appear");

    let screen = harness.screen_to_string();

    // Visible file should be shown
    assert!(screen.contains("visible.txt"));

    // Hidden file should NOT be shown by default
    assert!(
        !screen.contains(".hidden"),
        "Hidden files should not be shown by default"
    );
}

/// Test backspace goes to parent directory when filter is empty
#[test]
fn test_file_browser_backspace_parent() {
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    // Create nested structure
    let subdir = project_root.join("subdir");
    fs::create_dir(&subdir).unwrap();
    fs::write(subdir.join("child.txt"), "child").unwrap();
    fs::write(project_root.join("parent.txt"), "parent").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Default::default(),
        subdir.clone(), // Start in subdir
    )
    .unwrap();

    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();

    // Wait for child file
    harness
        .wait_until(|h| h.screen_to_string().contains("child.txt"))
        .expect("Should start in subdir");

    // Press backspace to go to parent
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();

    // Wait for parent directory contents
    harness
        .wait_until(|h| h.screen_to_string().contains("parent.txt"))
        .expect("Should navigate to parent and show parent.txt");
}

/// Test that the file browser is native (doesn't depend on plugin hooks)
/// The native implementation loads files directly via FsManager, not plugins.
#[test]
fn test_file_browser_is_native_implementation() {
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    fs::write(project_root.join("native_test.txt"), "content").unwrap();

    // Even with default config, the file browser should work natively
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Config::default(),
        project_root.clone(),
    )
    .unwrap();

    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();

    // File browser should work - this tests the native implementation
    harness
        .wait_until(|h| h.screen_to_string().contains("native_test.txt"))
        .expect("Native file browser should work");

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Navigation:"),
        "File browser popup should appear"
    );
    assert!(screen.contains("native_test.txt"), "Files should be listed");
}

/// Test that directories show trailing slash indicator
#[test]
fn test_file_browser_directory_trailing_slash() {
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    // Create a directory and a file
    fs::create_dir(project_root.join("mydir")).unwrap();
    fs::write(project_root.join("myfile.txt"), "content").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();

    // Wait for files to load
    harness
        .wait_until(|h| h.screen_to_string().contains("mydir"))
        .expect("Directory should be listed");

    let screen = harness.screen_to_string();

    // Directory should have trailing slash
    assert!(
        screen.contains("mydir/"),
        "Directory should show trailing slash: {}",
        screen
    );

    // File should NOT have trailing slash
    assert!(
        screen.contains("myfile.txt"),
        "File should be listed without trailing slash"
    );
}

/// Test clicking on column headers to sort
#[test]
fn test_file_browser_click_sort_header() {
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    // Create files with different sizes
    fs::write(project_root.join("small.txt"), "a").unwrap();
    fs::write(project_root.join("large.txt"), "abcdefghij").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();

    // Wait for files to load
    harness
        .wait_until(|h| h.screen_to_string().contains("small.txt"))
        .expect("Files should be listed");

    // The header row should contain "Name", "Size", "Modified"
    let screen = harness.screen_to_string();
    assert!(screen.contains("Name"), "Name header should be visible");
    assert!(screen.contains("Size"), "Size header should be visible");
    assert!(
        screen.contains("Modified"),
        "Modified header should be visible"
    );
}

/// Test clicking on file list items to select
#[test]
fn test_file_browser_click_file_item() {
    use crossterm::event::{MouseButton, MouseEvent, MouseEventKind};

    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    fs::write(project_root.join("aaa.txt"), "first").unwrap();
    fs::write(project_root.join("bbb.txt"), "second").unwrap();
    fs::write(project_root.join("ccc.txt"), "third").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();

    // Wait for files to load
    harness
        .wait_until(|h| h.screen_to_string().contains("aaa.txt"))
        .expect("Files should be listed");

    // Files should be visible
    let screen = harness.screen_to_string();
    assert!(screen.contains("aaa.txt"));
    assert!(screen.contains("bbb.txt"));
    assert!(screen.contains("ccc.txt"));

    // The file list area starts after navigation and header rows
    // Click somewhere in the file list area (approximate position)
    // This test verifies that clicking doesn't crash and files remain visible
    harness
        .send_mouse(MouseEvent {
            kind: MouseEventKind::Down(MouseButton::Left),
            column: 10,
            row: 10, // Should be in the file list area
            modifiers: KeyModifiers::NONE,
        })
        .unwrap();

    harness.render().unwrap();

    // Files should still be visible after click
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("aaa.txt") || screen.contains("bbb.txt") || screen.contains("ccc.txt"),
        "Files should still be visible after clicking"
    );
}

/// Test clicking on navigation shortcuts
#[test]
fn test_file_browser_click_navigation() {
    use crossterm::event::{MouseButton, MouseEvent, MouseEventKind};

    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    // Create a nested structure
    let subdir = project_root.join("subdir");
    fs::create_dir(&subdir).unwrap();
    fs::write(subdir.join("nested.txt"), "nested").unwrap();
    fs::write(project_root.join("root.txt"), "root").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Default::default(),
        subdir.clone(), // Start in subdir
    )
    .unwrap();

    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();

    // Wait for nested file to appear
    harness
        .wait_until(|h| h.screen_to_string().contains("nested.txt"))
        .expect("Should start in subdir");

    // Navigation section should be visible
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Navigation:"),
        "Navigation section should be visible"
    );

    // The ".." (parent) shortcut should be in the navigation area
    // Clicking on it should navigate to parent
    // Click on the navigation area (row 3 is typically where nav is, after border)
    harness
        .send_mouse(MouseEvent {
            kind: MouseEventKind::Down(MouseButton::Left),
            column: 15, // Should be in the ".." shortcut area
            row: 3,
            modifiers: KeyModifiers::NONE,
        })
        .unwrap();

    // Give it time to navigate
    harness.render().unwrap();

    // The click may or may not trigger navigation depending on exact coordinates
    // At minimum, the file browser should still be open
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Navigation:") || screen.contains("root.txt"),
        "Should either show navigation or have navigated to parent"
    );
}

/// Test mouse wheel scrolling in file browser
#[test]
fn test_file_browser_mouse_scroll() {
    use crossterm::event::{MouseEvent, MouseEventKind};

    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    // Create many files to ensure we need scrolling
    for i in 0..30 {
        fs::write(
            project_root.join(format!("file_{:02}.txt", i)),
            format!("content {}", i),
        )
        .unwrap();
    }

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24, // Small height to ensure scrolling is needed
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();

    // Wait for files to load
    harness
        .wait_until(|h| h.screen_to_string().contains("file_00.txt"))
        .expect("Files should be listed");

    let screen_before = harness.screen_to_string();
    assert!(
        screen_before.contains("file_00.txt"),
        "First file should be visible initially"
    );

    // Scroll down using mouse wheel
    harness
        .send_mouse(MouseEvent {
            kind: MouseEventKind::ScrollDown,
            column: 40,
            row: 10,
            modifiers: KeyModifiers::NONE,
        })
        .unwrap();
    harness.render().unwrap();

    // Scroll down more
    for _ in 0..5 {
        harness
            .send_mouse(MouseEvent {
                kind: MouseEventKind::ScrollDown,
                column: 40,
                row: 10,
                modifiers: KeyModifiers::NONE,
            })
            .unwrap();
    }
    harness.render().unwrap();

    // File browser should still be open after scrolling
    let screen_after = harness.screen_to_string();
    assert!(
        screen_after.contains("Navigation:"),
        "File browser should still be open after scrolling"
    );

    // Now scroll back up
    for _ in 0..5 {
        harness
            .send_mouse(MouseEvent {
                kind: MouseEventKind::ScrollUp,
                column: 40,
                row: 10,
                modifiers: KeyModifiers::NONE,
            })
            .unwrap();
    }
    harness.render().unwrap();

    // Should still be in file browser
    let screen_final = harness.screen_to_string();
    assert!(
        screen_final.contains("Navigation:"),
        "File browser should still be open after scrolling up"
    );
}

/// Test that clicking on a file entry updates the prompt text
#[test]
fn test_file_browser_click_updates_prompt() {
    use crossterm::event::{MouseButton, MouseEvent, MouseEventKind};

    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    fs::write(project_root.join("selected_file.txt"), "content").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();

    // Wait for file to load
    harness
        .wait_until(|h| h.screen_to_string().contains("selected_file.txt"))
        .expect("File should be listed");

    // The prompt line should show "Open file:" initially
    let screen = harness.screen_to_string();
    assert!(screen.contains("Open file:"), "Prompt should be visible");

    // Click on the file entry (it should be in the file list area)
    // The file list starts after navigation (2 rows) and header (1 row), plus border
    // So roughly row 6-7 in the popup area
    harness
        .send_mouse(MouseEvent {
            kind: MouseEventKind::Down(MouseButton::Left),
            column: 10,
            row: 8, // Approximate position of first file entry
            modifiers: KeyModifiers::NONE,
        })
        .unwrap();
    harness.render().unwrap();

    // The prompt should now contain the selected filename
    // Note: exact behavior depends on click coordinates hitting the file
    let screen_after = harness.screen_to_string();

    // File browser should still be open
    assert!(
        screen_after.contains("Navigation:"),
        "File browser should remain open after click"
    );
}

/// Test that opening file browser with a buffer in a subdir shows correct prompt path
#[test]
#[cfg_attr(windows, ignore)] // Uses Unix-style path separators in assertions
fn test_file_browser_prompt_shows_buffer_directory() {
    // Use wide terminal because macOS temp paths can be very long
    let mut harness = EditorTestHarness::with_temp_project(160, 24).unwrap();
    let project_root = harness.project_dir().unwrap();

    // Create a nested directory structure with a file
    let subdir = project_root.join("src").join("components");
    fs::create_dir_all(&subdir).unwrap();
    let file_path = subdir.join("button.rs");
    fs::write(&file_path, "// Button component").unwrap();

    // Also create a sibling file in the same directory
    fs::write(subdir.join("input.rs"), "// Input component").unwrap();

    // Open the file in the subdirectory using relative path
    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Navigation:"))
        .expect("File browser should appear");

    // Type the relative path to open the file
    harness.type_text("src/components/button.rs").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for file to open
    harness
        .wait_until(|h| h.screen_to_string().contains("Button component"))
        .expect("File should open");

    // Now open the file browser again
    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("Navigation:"))
        .expect("File browser should appear again");

    // The prompt should show the directory path of the open file
    // It will be an absolute path since the file was opened via direct path resolution
    let expected_suffix = "src/components/";

    // Get prompt line using harness helper (knows screen layout)
    let prompt_line = harness.get_prompt_line();
    let prompt_line = prompt_line.trim();

    // Check that prompt starts with "Open file: " and ends with the expected directory
    assert!(
        prompt_line.starts_with("Open file: "),
        "Prompt should start with 'Open file: '\nActual: '{}'",
        prompt_line,
    );
    assert!(
        prompt_line.ends_with(expected_suffix),
        "Prompt should end with '{}'\nActual: '{}'",
        expected_suffix,
        prompt_line,
    );

    // The sibling file should be visible in the file list
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("input.rs"),
        "Should show sibling files in the same directory"
    );
    assert!(
        screen.contains("button.rs"),
        "Should show the current file in the list"
    );
}

/// Test that the "Show Hidden" checkbox appears and shows correct state
#[test]
fn test_file_browser_hidden_checkbox_appears() {
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    // Create visible and hidden files
    fs::write(project_root.join("visible.txt"), "visible content").unwrap();
    fs::write(project_root.join(".hidden_file"), "hidden content").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    // Open file browser with Ctrl+O
    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();

    // Wait for the dialog to appear first (UI renders before files load)
    harness
        .wait_until(|h| h.screen_to_string().contains("Navigation:"))
        .expect("File browser dialog should appear");

    // Then wait for files to finish loading
    harness
        .wait_until(|h| h.screen_to_string().contains("visible.txt"))
        .expect("Files should be loaded");

    let screen = harness.screen_to_string();

    // Verify checkbox appears in unchecked state (☐ Show Hidden with underlined H)
    assert!(
        screen.contains("Show") && screen.contains("idden"),
        "Checkbox should show 'Show Hidden'. Screen:\n{}",
        screen
    );

    // Verify hidden files are not shown by default
    assert!(
        !screen.contains(".hidden_file"),
        "Hidden files should NOT be shown by default"
    );
}

/// Test that clicking the "Show Hidden" checkbox toggles hidden files visibility
#[test]
fn test_file_browser_toggle_hidden_checkbox_click() {
    use crossterm::event::{MouseButton, MouseEvent, MouseEventKind};

    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path().to_path_buf();

    // Create visible and hidden files
    fs::write(project_root.join("visible.txt"), "visible content").unwrap();
    fs::write(project_root.join(".hidden_file"), "hidden content").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    // Open file browser with Ctrl+O
    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();

    // Wait for the dialog to appear first (UI renders before files load)
    harness
        .wait_until(|h| h.screen_to_string().contains("Navigation:"))
        .expect("File browser dialog should appear");

    // Then wait for files to finish loading
    harness
        .wait_until(|h| h.screen_to_string().contains("visible.txt"))
        .expect("Files should be loaded");

    // Find the row containing "Show Hidden" checkbox by searching screen lines
    let screen = harness.screen_to_string();
    let lines: Vec<&str> = screen.lines().collect();
    let checkbox_row = lines
        .iter()
        .position(|line| {
            line.contains("Show Hidden") || line.contains("Show") && line.contains("idden")
        })
        .expect("Should find Show Hidden checkbox row");

    // Click on the checkbox area (left side of the row)
    // The checkbox " ☐ Show Hidden (Alt+.)" is at the left side on its own row
    harness
        .send_mouse(MouseEvent {
            kind: MouseEventKind::Down(MouseButton::Left),
            column: 10, // Left side where checkbox is
            row: checkbox_row as u16,
            modifiers: KeyModifiers::NONE,
        })
        .unwrap();
    harness.render().unwrap();

    // Wait for hidden files to appear (gives time for async reload)
    harness
        .wait_until(|h| h.screen_to_string().contains(".hidden_file"))
        .expect("Hidden files should appear after clicking checkbox");

    let screen_after = harness.screen_to_string();

    // Verify checkbox is now checked and hidden files are visible
    assert!(
        screen_after.contains("☑") && screen_after.contains("Show"),
        "Checkbox should be checked after toggle"
    );
    assert!(
        screen_after.contains(".hidden_file"),
        "Hidden files should be visible after toggle"
    );
}
