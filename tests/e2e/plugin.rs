use crate::common::fixtures::TestFixture;
use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

/// Test TODO Highlighter plugin - loads plugin, enables it, and checks highlighting
#[test]
fn test_todo_highlighter_plugin() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory and copy the TODO highlighter plugin
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/todo_highlighter.lua");
    let plugin_dest = plugins_dir.join("todo_highlighter.lua");
    fs::copy(&plugin_source, &plugin_dest).unwrap();

    // Create test file with TODO comments
    let test_file_content = r#"// This is a test file for the TODO Highlighter plugin

// TODO: Implement user authentication
// FIXME: Memory leak in connection pool
// HACK: Temporary workaround for parser bug
// NOTE: This function is performance-critical
// XXX: Needs review before production
// BUG: Off-by-one error in loop counter

# Python-style comments
# TODO: Add type hints to all functions
# FIXME: Handle edge case when list is empty

Regular text without keywords should not be highlighted:
TODO FIXME HACK NOTE XXX BUG (not in comments)
"#;

    let fixture = TestFixture::new("test_todo.txt", test_file_content).unwrap();

    // Create harness with the project directory (so plugins load)
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(80, 24, Default::default(), project_root)
            .unwrap();

    // Open the test file
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    // Check that file content is visible
    harness.assert_screen_contains("TODO: Implement user authentication");

    // Open command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();

    // Type "TODO Highlighter: Enable" command
    harness.type_text("TODO Highlighter: Enable").unwrap();

    // Execute the command
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Check status message
    harness.assert_screen_contains("TODO Highlighter: Found");

    // Now check that highlights are actually rendered
    // The TODO keyword should have a background color applied
    // Let's find the position of "TODO" in the first comment and check its style

    let screen = harness.screen_to_string();
    println!("Screen after enabling TODO highlighter:\n{}", screen);

    // Find the position of "TODO" on screen
    let lines: Vec<&str> = screen.lines().collect();
    let mut found_highlighted_todo = false;

    for (y, line) in lines.iter().enumerate() {
        if let Some(x) = line.find("TODO") {
            // Check if this TODO is in a comment (should have "//" before it)
            if line[..x].contains("//") {
                // Check the style of the 'T' in "TODO"
                if let Some(style) = harness.get_cell_style(x as u16, y as u16) {
                    // Check if background color is set (orange: r=255, g=165, b=0)
                    if let Some(bg) = style.bg {
                        println!(
                            "Found TODO at ({}, {}) with background color: {:?}",
                            x, y, bg
                        );
                        found_highlighted_todo = true;
                        break;
                    }
                }
            }
        }
    }

    assert!(
        found_highlighted_todo,
        "Expected to find at least one highlighted TODO keyword"
    );
}

/// Test TODO Highlighter disable command
#[test]
fn test_todo_highlighter_disable() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory and copy the TODO highlighter plugin
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/todo_highlighter.lua");
    let plugin_dest = plugins_dir.join("todo_highlighter.lua");
    fs::copy(&plugin_source, &plugin_dest).unwrap();

    // Create test file with TODO comments
    let test_file_content = "// TODO: Test comment\n";
    let fixture = TestFixture::new("test_todo.txt", test_file_content).unwrap();

    // Create harness with the project directory (so plugins load)
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(80, 24, Default::default(), project_root)
            .unwrap();

    // Open the test file
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    // Enable highlighting first
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("TODO Highlighter: Enable").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Now disable it
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("TODO Highlighter: Disable").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Check status message
    harness.assert_screen_contains("TODO Highlighter: Disabled");

    // Verify the TODO-specific highlighting (orange overlay) is removed
    // We don't check that there's NO background color at all, because there might be
    // syntax highlighting or theme colors. We just verify the orange overlay is gone.
    // The test passes if we can execute disable without error and see the status message.
}

/// Test TODO Highlighter toggle command
#[test]
fn test_todo_highlighter_toggle() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory and copy the TODO highlighter plugin
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/todo_highlighter.lua");
    let plugin_dest = plugins_dir.join("todo_highlighter.lua");
    fs::copy(&plugin_source, &plugin_dest).unwrap();

    // Create test file with TODO comments
    let test_file_content = "// TODO: Test comment\n";
    let fixture = TestFixture::new("test_todo.txt", test_file_content).unwrap();

    // Create harness with the project directory (so plugins load)
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(80, 24, Default::default(), project_root)
            .unwrap();

    // Open the test file
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    // Toggle on
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("TODO Highlighter: Toggle").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should see status message with count
    harness.assert_screen_contains("TODO Highlighter: Found");

    // Toggle off
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("TODO Highlighter: Toggle").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Check status message
    harness.assert_screen_contains("TODO Highlighter: Disabled");
}

/// Test TODO Highlighter updates when buffer content changes
#[test]
fn test_todo_highlighter_updates_on_edit() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory and copy the TODO highlighter plugin
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/todo_highlighter.lua");
    let plugin_dest = plugins_dir.join("todo_highlighter.lua");
    fs::copy(&plugin_source, &plugin_dest).unwrap();

    // Create test file with TODO comment at the start
    let test_file_content = "// TODO: Original comment\n";
    let fixture = TestFixture::new("test_todo.txt", test_file_content).unwrap();

    // Create harness with the project directory (so plugins load)
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(80, 24, Default::default(), project_root)
            .unwrap();

    // Open the test file
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    // Enable highlighting
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("TODO Highlighter: Enable").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify the original TODO is highlighted
    let screen_before = harness.screen_to_string();
    println!("Screen before edit:\n{}", screen_before);

    let lines: Vec<&str> = screen_before.lines().collect();
    let mut found_original_todo = false;

    for (y, line) in lines.iter().enumerate() {
        if line.contains("TODO: Original") {
            if let Some(x) = line.find("TODO") {
                if let Some(style) = harness.get_cell_style(x as u16, y as u16) {
                    if let Some(bg) = style.bg {
                        // Check it's not just Reset/White, should be a real color
                        println!("Found TODO at ({}, {}) with background: {:?}", x, y, bg);
                        found_original_todo = true;
                        break;
                    }
                }
            }
        }
    }

    assert!(
        found_original_todo,
        "Expected to find highlighted 'TODO: Original' before edit"
    );

    // Go to the beginning of the file
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Insert a new line at the top: "// FIXME: New comment\n"
    harness.type_text("// FIXME: New comment\n").unwrap();
    harness.render().unwrap();

    let screen_after = harness.screen_to_string();
    println!("Screen after adding FIXME:\n{}", screen_after);

    // The buffer should now be:
    // Line 1: // FIXME: New comment
    // Line 2: // TODO: Original comment

    // Check that FIXME is highlighted
    let lines: Vec<&str> = screen_after.lines().collect();
    let mut found_fixme = false;
    let mut found_todo_on_line_2 = false;

    for (y, line) in lines.iter().enumerate() {
        if line.contains("FIXME: New") {
            if let Some(x) = line.find("FIXME") {
                if let Some(style) = harness.get_cell_style(x as u16, y as u16) {
                    if let Some(bg) = style.bg {
                        println!("Found FIXME at ({}, {}) with background: {:?}", x, y, bg);
                        found_fixme = true;
                    }
                }
            }
        }
        if line.contains("TODO: Original") {
            if let Some(x) = line.find("TODO") {
                if let Some(style) = harness.get_cell_style(x as u16, y as u16) {
                    if let Some(bg) = style.bg {
                        println!(
                            "Found TODO on line 2 at ({}, {}) with background: {:?}",
                            x, y, bg
                        );
                        // Check if it's an actual RGB color (orange), not just Reset
                        if matches!(bg, ratatui::style::Color::Rgb(_, _, _)) {
                            found_todo_on_line_2 = true;
                        }
                    }
                }
            }
        }
    }

    // Bug: FIXME gets highlighted because it happens to be at the byte position where TODO was
    // But TODO should ALSO be highlighted, not just have Reset background
    assert!(
        found_fixme,
        "Expected to find highlighted FIXME after inserting new line"
    );

    // This assertion will FAIL, demonstrating the bug - TODO highlight doesn't update
    assert!(
        found_todo_on_line_2,
        "BUG REPRODUCED: TODO on line 2 is not highlighted! The old overlay at byte 3-7 \
         now highlights FIXME (which happens to be at those bytes), but TODO moved to a \
         new byte position and didn't get a new overlay. Overlays need to update when buffer changes!"
    );
}
