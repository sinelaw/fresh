//! E2E tests for popup text selection and copy functionality

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::view::popup::{Popup, PopupPosition};

/// Test LSP hover popup text selection and copy with real LSP flow
/// This tests the actual user scenario: hover triggers popup, user selects text, presses Ctrl+C
#[test]
#[cfg_attr(
    windows,
    ignore = "FakeLspServer uses a Bash script which is not available on Windows"
)]
fn test_lsp_hover_popup_text_selection_copy() -> anyhow::Result<()> {
    use crate::common::fake_lsp::FakeLspServer;

    // Spawn fake LSP server (has hover support)
    let _fake_server = FakeLspServer::spawn()?;

    // Create temp dir and test file
    let temp_dir = tempfile::tempdir()?;
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn example_function() {}\n")?;

    // Configure editor to use the fake LSP server
    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::services::lsp::LspServerConfig {
            command: FakeLspServer::script_path().to_string_lossy().to_string(),
            args: vec![],
            enabled: true,
            auto_start: true,
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
            env: Default::default(),
        },
    );

    // Create harness with config
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    // Enable internal-only clipboard for testing
    harness.editor_mut().set_clipboard_for_test("".to_string());

    harness.open_file(&test_file)?;
    harness.render()?;

    // Move mouse over the symbol "example_function" to trigger hover state
    harness.mouse_move(10, 2)?;
    harness.render()?;

    // Force check mouse hover to bypass the 500ms timer and send the request
    harness.editor_mut().force_check_mouse_hover();

    // Wait for hover popup to appear (LSP response received)
    let popup_appeared = harness.wait_until(|h| h.editor().active_state().popups.is_visible());
    assert!(popup_appeared.is_ok(), "Hover popup should appear");

    harness.render()?;
    let screen = harness.screen_to_string();
    println!("Screen after hover popup appeared:\n{}", screen);

    // Find the popup content by searching for "Test hover content" (from fake LSP)
    fn find_popup_content_position(screen: &str) -> Option<(u16, u16)> {
        for (row, line) in screen.lines().enumerate() {
            // Look for the popup content from fake LSP
            if let Some(col) = line.find("Test hover content") {
                return Some((col as u16, row as u16));
            }
        }
        None
    }

    let (popup_content_x, popup_content_y) =
        find_popup_content_position(&screen).expect("Should find popup content position");

    println!(
        "Popup content found at ({}, {})",
        popup_content_x, popup_content_y
    );

    // Mouse drag to select text in the popup
    // Select "Test" (first 4 characters of the content)
    let start_col = popup_content_x;
    let start_row = popup_content_y;
    let end_col = popup_content_x + 4;
    let end_row = popup_content_y;

    println!(
        "Mouse drag from ({}, {}) to ({}, {})",
        start_col, start_row, end_col, end_row
    );

    harness.mouse_drag(start_col, start_row, end_col, end_row)?;
    harness.render()?;

    // Check that popup still exists and has selection
    {
        let popup_visible = harness.editor().active_state().popups.is_visible();
        println!("Popup visible after mouse drag: {}", popup_visible);
        assert!(
            popup_visible,
            "Popup should still be visible after mouse drag"
        );

        if let Some(popup) = harness.editor().active_state().popups.top() {
            println!("Popup has_selection: {}", popup.has_selection());
            println!("Popup text_selection: {:?}", popup.text_selection);
            if let Some(text) = popup.get_selected_text() {
                println!("Selected text: {:?}", text);
            }
            assert!(
                popup.has_selection(),
                "Mouse drag should create selection in popup"
            );
        }
    }

    // Press Ctrl+C to copy
    println!("Pressing Ctrl+C...");
    harness.send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)?;
    harness.render()?;

    // Check clipboard content
    let clipboard_content = harness.editor_mut().clipboard_content_for_test();
    println!("Clipboard content: {:?}", clipboard_content);

    assert!(
        !clipboard_content.is_empty(),
        "Clipboard should not be empty after Ctrl+C with popup selection. Got empty string."
    );

    Ok(())
}

/// Test that selecting text in a popup and pressing Ctrl+C copies the selected text
#[test]
fn test_popup_text_selection_copy() {
    let mut harness = EditorTestHarness::new(80, 30).unwrap();

    // Create a popup with text content directly
    {
        let editor = harness.editor_mut();
        let theme = editor.theme().clone();

        // Create a simple text popup with known content
        let popup = Popup::text(
            vec![
                "Line 0: Hello World".to_string(),
                "Line 1: Test Content".to_string(),
                "Line 2: More Text".to_string(),
            ],
            &theme,
        )
        .with_position(PopupPosition::Fixed { x: 10, y: 5 })
        .with_width(40)
        .with_max_height(10);

        editor.active_state_mut().popups.show(popup);

        // Enable internal-only clipboard for testing
        editor.set_clipboard_for_test("".to_string());
    }

    harness.render().unwrap();

    // Verify popup is visible
    let screen = harness.screen_to_string();
    println!("Screen after showing popup:\n{}", screen);
    assert!(
        screen.contains("Hello World"),
        "Popup should be visible with 'Hello World' text"
    );

    // Find the popup content area
    // Popup is at x=10, y=5 with width=40
    // With borders, inner content starts at x=11, y=6
    let inner_x = 11u16;
    let inner_y = 6u16;

    // Click at the start of "Hello" (column 8 in "Line 0: Hello World")
    // The popup inner area starts at inner_x, so "Hello" starts at inner_x + 8
    let start_col = inner_x + 8;
    let start_row = inner_y;

    // Drag to select "Hello World" (ends at column 19)
    let end_col = inner_x + 19;
    let end_row = inner_y;

    println!(
        "Dragging from ({}, {}) to ({}, {})",
        start_col, start_row, end_col, end_row
    );

    // Mouse drag to select text
    harness
        .mouse_drag(start_col, start_row, end_col, end_row)
        .unwrap();
    harness.render().unwrap();

    let screen_after_drag = harness.screen_to_string();
    println!("Screen after drag:\n{}", screen_after_drag);

    // Check that popup still has selection
    {
        let editor = harness.editor_mut();
        let popup = editor.active_state().popups.top();
        assert!(popup.is_some(), "Popup should still be visible");
        let popup = popup.unwrap();
        println!("Popup has_selection: {}", popup.has_selection());
        println!("Popup text_selection: {:?}", popup.text_selection);
        if let Some(text) = popup.get_selected_text() {
            println!("Selected text from popup: {:?}", text);
        }

        // Verify selection was actually created by mouse drag
        assert!(
            popup.has_selection(),
            "Mouse drag should have created a selection in the popup"
        );
    }

    // Press Ctrl+C to copy
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Check clipboard content
    {
        let editor = harness.editor_mut();
        // Access internal clipboard directly for testing
        let clipboard_content = editor.clipboard_content_for_test();
        println!("Clipboard content: {:?}", clipboard_content);

        assert!(
            clipboard_content.contains("Hello"),
            "Clipboard should contain 'Hello' after Ctrl+C. Got: {:?}",
            clipboard_content
        );
    }
}

/// Test that Ctrl+C in popup with selection does NOT copy from editor
#[test]
fn test_popup_copy_does_not_copy_from_editor() {
    let mut harness = EditorTestHarness::new(80, 30).unwrap();

    // Type some content in editor and select it
    harness.type_text("Editor content here").unwrap();
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap(); // Select all

    // Create a popup with text content
    {
        let editor = harness.editor_mut();
        let theme = editor.theme().clone();

        let popup = Popup::text(vec!["Popup text".to_string()], &theme)
            .with_position(PopupPosition::Fixed { x: 10, y: 5 })
            .with_width(30)
            .with_max_height(5);

        editor.active_state_mut().popups.show(popup);
        editor.set_clipboard_for_test("".to_string());
    }

    harness.render().unwrap();

    // Mouse drag to select "Popup" from the popup content
    // Popup at x=10, y=5 with borders means inner content at x=11, y=6
    let inner_x = 11u16;
    let inner_y = 6u16;
    harness
        .mouse_drag(inner_x, inner_y, inner_x + 5, inner_y)
        .unwrap();
    harness.render().unwrap();

    // Press Ctrl+C
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Check clipboard - should have popup text, NOT editor text
    {
        let editor = harness.editor_mut();
        let clipboard_content = editor.clipboard_content_for_test();
        println!("Clipboard content: {:?}", clipboard_content);

        assert!(
            clipboard_content.contains("Popup"),
            "Clipboard should contain 'Popup' from popup selection, not editor content. Got: {:?}",
            clipboard_content
        );
        assert!(
            !clipboard_content.contains("Editor"),
            "Clipboard should NOT contain editor content. Got: {:?}",
            clipboard_content
        );
    }
}

/// Test markdown popup (like LSP hover) text selection and copy
#[test]
fn test_markdown_popup_text_selection_copy() {
    let mut harness = EditorTestHarness::new(80, 30).unwrap();

    // Create a markdown popup like LSP hover uses
    {
        let editor = harness.editor_mut();
        let theme = editor.theme().clone();

        // Create a markdown popup with known content (like LSP hover)
        // Pass None for grammar_registry since we don't need syntax highlighting for this test
        let markdown_content = "# Function\n\n`hello_world()`\n\nThis is a test function.";
        let popup = Popup::markdown(markdown_content, &theme, None)
            .with_position(PopupPosition::Fixed { x: 10, y: 5 })
            .with_width(40)
            .with_max_height(10)
            .with_transient(true); // LSP hover popups are transient

        editor.active_state_mut().popups.show(popup);
        editor.set_clipboard_for_test("".to_string());
    }

    harness.render().unwrap();

    // Verify popup is visible
    let screen = harness.screen_to_string();
    println!("Screen after showing markdown popup:\n{}", screen);
    assert!(
        screen.contains("Function") || screen.contains("hello_world"),
        "Markdown popup should be visible"
    );

    // Mouse drag to select "Function" from the popup content
    // Popup at x=10, y=5 with borders means inner content at x=11, y=6
    let inner_x = 11u16;
    let inner_y = 6u16;
    harness
        .mouse_drag(inner_x, inner_y, inner_x + 8, inner_y)
        .unwrap();
    harness.render().unwrap();

    // Check popup state before Ctrl+C
    {
        let editor = harness.editor_mut();
        let popup_visible = editor.active_state().popups.is_visible();
        println!("Popup visible before Ctrl+C: {}", popup_visible);
        if let Some(popup) = editor.active_state().popups.top() {
            println!(
                "Popup has_selection before Ctrl+C: {}",
                popup.has_selection()
            );
            println!("Selection: {:?}", popup.text_selection);
            if let Some(text) = popup.get_selected_text() {
                println!("get_selected_text returned: {:?}", text);
            }
        }
    }

    // Press Ctrl+C to copy
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();

    // Check popup state after Ctrl+C
    {
        let editor = harness.editor_mut();
        let popup_visible = editor.active_state().popups.is_visible();
        println!("Popup visible after Ctrl+C: {}", popup_visible);
        if let Some(popup) = editor.active_state().popups.top() {
            println!(
                "Popup has_selection after Ctrl+C: {}",
                popup.has_selection()
            );
        }
    }

    harness.render().unwrap();

    // Check clipboard content
    {
        let editor = harness.editor_mut();
        let clipboard_content = editor.clipboard_content_for_test();
        println!("Clipboard content: {:?}", clipboard_content);

        assert!(
            clipboard_content.contains("Function"),
            "Clipboard should contain 'Function' from markdown popup. Got: {:?}",
            clipboard_content
        );
    }
}

/// Test transient popup (like LSP hover) with mouse drag selection and Ctrl+C copy
/// This is the real scenario users experience
#[test]
fn test_transient_popup_mouse_drag_and_copy() {
    let mut harness = EditorTestHarness::new(80, 30).unwrap();

    // Create a transient markdown popup like LSP hover uses
    {
        let editor = harness.editor_mut();
        let theme = editor.theme().clone();

        let markdown_content = "# Function\n\nhello_world()\n\nThis is a test.";
        let popup = Popup::markdown(markdown_content, &theme, None)
            .with_position(PopupPosition::Fixed { x: 10, y: 5 })
            .with_width(40)
            .with_max_height(10)
            .with_transient(true); // This is key - LSP hover popups are transient

        editor.active_state_mut().popups.show(popup);
        editor.set_clipboard_for_test("".to_string());
    }

    harness.render().unwrap();

    // Verify popup is visible
    let screen = harness.screen_to_string();
    println!("Screen after showing transient popup:\n{}", screen);
    assert!(
        screen.contains("Function"),
        "Transient popup should be visible"
    );

    // Get popup inner area position
    // Popup at x=10, y=5 with borders means inner content at x=11, y=6
    let inner_x = 11u16;
    let inner_y = 6u16;

    // Mouse drag to select "Function" (first 8 characters of line 0)
    let start_col = inner_x;
    let start_row = inner_y;
    let end_col = inner_x + 8;
    let end_row = inner_y;

    println!(
        "Mouse drag from ({}, {}) to ({}, {})",
        start_col, start_row, end_col, end_row
    );

    // Perform mouse drag
    harness
        .mouse_drag(start_col, start_row, end_col, end_row)
        .unwrap();
    harness.render().unwrap();

    // Check popup state after drag
    {
        let editor = harness.editor_mut();
        let popup_visible = editor.active_state().popups.is_visible();
        println!("Popup visible after mouse drag: {}", popup_visible);

        assert!(
            popup_visible,
            "Transient popup should still be visible after mouse drag"
        );

        if let Some(popup) = editor.active_state().popups.top() {
            println!("Popup has_selection after drag: {}", popup.has_selection());
            println!("Popup text_selection: {:?}", popup.text_selection);
            if let Some(text) = popup.get_selected_text() {
                println!("Selected text: {:?}", text);
            }

            assert!(
                popup.has_selection(),
                "Mouse drag should have created a selection in the transient popup"
            );
        }
    }

    // Now press Ctrl+C to copy
    println!("Pressing Ctrl+C...");
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Check clipboard content
    {
        let editor = harness.editor_mut();
        let clipboard_content = editor.clipboard_content_for_test();
        println!("Clipboard content after Ctrl+C: {:?}", clipboard_content);

        assert!(
            clipboard_content.contains("Function"),
            "Clipboard should contain 'Function' from transient popup after mouse drag + Ctrl+C. Got: {:?}",
            clipboard_content
        );
    }
}
