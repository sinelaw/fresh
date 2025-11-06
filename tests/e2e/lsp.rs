//! E2E tests for LSP features

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Test that completion popup text is not mangled
#[test]
fn test_lsp_completion_popup_text_not_mangled() -> std::io::Result<()> {
    use fresh::event::{Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData};

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Show a completion popup with realistic LSP data
    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            title: Some("Completion".to_string()),
            content: PopupContentData::List {
                items: vec![
                    PopupListItemData {
                        text: "test_function".to_string(),
                        detail: Some("fn test_function() -> i32".to_string()),
                        icon: Some("λ".to_string()),
                        data: Some("test_function".to_string()),
                    },
                    PopupListItemData {
                        text: "test_variable".to_string(),
                        detail: Some("let test_variable: String".to_string()),
                        icon: Some("v".to_string()),
                        data: Some("test_variable".to_string()),
                    },
                ],
                selected: 0,
            },
            position: PopupPositionData::Centered,
            width: 50,
            max_height: 15,
            bordered: true,
        },
    });

    harness.render()?;

    // Get the screen content
    let screen = harness.screen_to_string();

    // Debug: print the screen to see what's there
    println!("Screen content:\n{}", screen);

    // Verify the completion items are visible and not mangled
    assert!(
        screen.contains("test_function"),
        "Expected 'test_function' to be visible in popup"
    );
    assert!(
        screen.contains("test_variable"),
        "Expected 'test_variable' to be visible in popup"
    );

    // Check that icon is displayed (should be the lambda character or similar)
    // Note: This might render differently depending on terminal capabilities

    // Check for common mangled text patterns
    assert!(
        !screen.contains("\u{0}"),
        "Screen should not contain null characters"
    );
    assert!(
        !screen.contains("\u{1}"),
        "Screen should not contain control characters"
    );

    // Verify details are shown (if the popup implementation shows them)
    // The exact format depends on how the popup renders items

    Ok(())
}

/// Test that completion replaces current word, not appends
#[test]
fn test_lsp_completion_replaces_word() -> std::io::Result<()> {
    use fresh::event::{Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData};

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Type a partial word
    harness.type_text("test_f")?;
    harness.render()?;

    // Verify partial word is in buffer
    let buffer_before = harness.get_buffer_content();
    assert_eq!(buffer_before, "test_f");

    // Show completion popup
    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            title: Some("Completion".to_string()),
            content: PopupContentData::List {
                items: vec![PopupListItemData {
                    text: "test_function".to_string(),
                    detail: Some("fn test_function()".to_string()),
                    icon: Some("λ".to_string()),
                    data: Some("test_function".to_string()),
                }],
                selected: 0,
            },
            position: PopupPositionData::BelowCursor,
            width: 40,
            max_height: 10,
            bordered: true,
        },
    });

    harness.render()?;

    // Confirm selection with Enter
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.render()?;

    // Buffer should now contain the full word, not "test_ftest_function"
    let buffer_after = harness.get_buffer_content();
    assert_eq!(
        buffer_after, "test_function",
        "Expected completion to replace 'test_f' with 'test_function', but got '{}'",
        buffer_after
    );

    Ok(())
}

/// Test LSP diagnostics display in the editor
#[test]
fn test_lsp_diagnostics_display() -> std::io::Result<()> {
    use fresh::event::{Event, OverlayFace, UnderlineStyle};

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Type some text
    harness.type_text("let x = 5;")?;
    harness.render()?;

    // Manually add a diagnostic overlay (simulating what LSP would do)
    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::AddOverlay {
        overlay_id: "lsp-diagnostic-test".to_string(),
        range: 4..5, // "x"
        face: OverlayFace::Background {
            color: (40, 0, 0), // Dark red background
        },
        priority: 100,
        message: Some("unused variable: `x`".to_string()),
    });

    harness.render()?;

    // Verify the diagnostic is displayed in the status bar
    let screen = harness.screen_to_string();
    // Status bar should show "E:1" for one error
    assert!(
        screen.contains("E:1"),
        "Expected status bar to show 'E:1' for error count"
    );

    Ok(())
}

/// Test LSP completion popup display
#[test]
fn test_lsp_completion_popup() -> std::io::Result<()> {
    use fresh::event::{Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData};

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Type some text
    harness.type_text("test")?;
    harness.render()?;

    // Show a completion popup (simulating LSP response)
    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            title: Some("Completion".to_string()),
            content: PopupContentData::List {
                items: vec![
                    PopupListItemData {
                        text: "test_function".to_string(),
                        detail: Some("fn test_function()".to_string()),
                        icon: Some("λ".to_string()),
                        data: Some("test_function".to_string()),
                    },
                    PopupListItemData {
                        text: "test_variable".to_string(),
                        detail: Some("let test_variable".to_string()),
                        icon: Some("v".to_string()),
                        data: Some("test_variable".to_string()),
                    },
                ],
                selected: 0,
            },
            position: PopupPositionData::BelowCursor,
            width: 50,
            max_height: 15,
            bordered: true,
        },
    });

    harness.render()?;

    // Verify popup is visible
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Completion"),
        "Expected popup title 'Completion' to be visible"
    );
    assert!(
        screen.contains("test_function"),
        "Expected completion item to be visible"
    );

    // Navigate down in popup
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.render()?;

    // Select second item and confirm
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.render()?;

    // Verify the completion was inserted
    let buffer_content = harness.get_buffer_content();
    assert!(
        buffer_content.contains("test_variable"),
        "Expected 'test_variable' to be inserted into buffer, got: {}",
        buffer_content
    );

    Ok(())
}

/// Test LSP diagnostics summary in status bar
#[test]
fn test_lsp_diagnostics_status_bar() -> std::io::Result<()> {
    use fresh::event::{Event, OverlayFace};

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Type some text
    harness.type_text("let x = 5;\nlet y = 10;")?;
    harness.render()?;

    // Add error diagnostic
    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::AddOverlay {
        overlay_id: "lsp-diagnostic-error1".to_string(),
        range: 4..5,
        face: OverlayFace::Background {
            color: (40, 0, 0),
        },
        priority: 100, // Error priority
        message: Some("unused variable: `x`".to_string()),
    });

    // Add warning diagnostic
    state.apply(&Event::AddOverlay {
        overlay_id: "lsp-diagnostic-warning1".to_string(),
        range: 15..16,
        face: OverlayFace::Background {
            color: (40, 40, 0),
        },
        priority: 50, // Warning priority
        message: Some("unused variable: `y`".to_string()),
    });

    harness.render()?;

    // Verify status bar shows both error and warning counts
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("E:1"),
        "Expected status bar to show 'E:1' for error count"
    );
    assert!(
        screen.contains("W:1"),
        "Expected status bar to show 'W:1' for warning count"
    );

    Ok(())
}

/// Test that diagnostics are removed when cleared
#[test]
fn test_lsp_clear_diagnostics() -> std::io::Result<()> {
    use fresh::event::{Event, OverlayFace};

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Type some text
    harness.type_text("let x = 5;")?;
    harness.render()?;

    // Add diagnostic
    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::AddOverlay {
        overlay_id: "lsp-diagnostic-test".to_string(),
        range: 4..5,
        face: OverlayFace::Background {
            color: (40, 0, 0),
        },
        priority: 100,
        message: Some("test error".to_string()),
    });

    harness.render()?;

    // Verify diagnostic is shown
    let screen = harness.screen_to_string();
    assert!(screen.contains("E:1"), "Expected error count in status bar");

    // Clear diagnostics
    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::RemoveOverlay {
        overlay_id: "lsp-diagnostic-test".to_string(),
    });

    harness.render()?;

    // Verify diagnostic is removed
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("E:1"),
        "Expected error count to be removed from status bar"
    );

    Ok(())
}

/// Test multiple completion items navigation
#[test]
fn test_lsp_completion_navigation() -> std::io::Result<()> {
    use fresh::event::{Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData};

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Show completion popup with multiple items
    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            title: Some("Completion".to_string()),
            content: PopupContentData::List {
                items: vec![
                    PopupListItemData {
                        text: "item1".to_string(),
                        detail: None,
                        icon: None,
                        data: Some("item1".to_string()),
                    },
                    PopupListItemData {
                        text: "item2".to_string(),
                        detail: None,
                        icon: None,
                        data: Some("item2".to_string()),
                    },
                    PopupListItemData {
                        text: "item3".to_string(),
                        detail: None,
                        icon: None,
                        data: Some("item3".to_string()),
                    },
                ],
                selected: 0,
            },
            position: PopupPositionData::Centered,
            width: 30,
            max_height: 10,
            bordered: true,
        },
    });

    harness.render()?;

    // Navigate down twice
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.render()?;

    // Confirm selection (should insert item3)
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.render()?;

    // Verify item3 was inserted
    let buffer_content = harness.get_buffer_content();
    assert!(
        buffer_content.contains("item3"),
        "Expected 'item3' to be inserted, got: {}",
        buffer_content
    );

    Ok(())
}

/// Test popup cancel (Escape) doesn't insert anything
#[test]
fn test_lsp_completion_cancel() -> std::io::Result<()> {
    use fresh::event::{Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData};

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Type some text first
    harness.type_text("orig")?;
    harness.render()?;

    // Show completion popup
    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            title: Some("Completion".to_string()),
            content: PopupContentData::List {
                items: vec![PopupListItemData {
                    text: "completion_item".to_string(),
                    detail: None,
                    icon: None,
                    data: Some("completion_item".to_string()),
                }],
                selected: 0,
            },
            position: PopupPositionData::BelowCursor,
            width: 30,
            max_height: 10,
            bordered: true,
        },
    });

    harness.render()?;

    // Press Escape to cancel
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE)?;
    harness.render()?;

    // Verify original text is unchanged
    let buffer_content = harness.get_buffer_content();
    assert_eq!(
        buffer_content, "orig",
        "Expected buffer to remain 'orig' after canceling popup"
    );
    assert!(
        !buffer_content.contains("completion_item"),
        "Expected completion item NOT to be inserted"
    );

    Ok(())
}

/// Test completion after a dot preserves the prefix
#[test]
fn test_lsp_completion_after_dot() -> std::io::Result<()> {
    use fresh::event::{Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData};

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Type "args."
    harness.type_text("args.")?;
    harness.render()?;

    // Show completion popup with method-like completions
    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            title: Some("Completion".to_string()),
            content: PopupContentData::List {
                items: vec![
                    PopupListItemData {
                        text: "len".to_string(),
                        detail: Some("fn len(&self) -> usize".to_string()),
                        icon: Some("λ".to_string()),
                        data: Some("len".to_string()),
                    },
                    PopupListItemData {
                        text: "is_empty".to_string(),
                        detail: Some("fn is_empty(&self) -> bool".to_string()),
                        icon: Some("λ".to_string()),
                        data: Some("is_empty".to_string()),
                    },
                ],
                selected: 0,
            },
            position: PopupPositionData::BelowCursor,
            width: 40,
            max_height: 10,
            bordered: true,
        },
    });

    harness.render()?;

    // Confirm selection (should insert "len" after the dot)
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.render()?;

    // Verify "args." is preserved and "len" is appended
    let buffer_content = harness.get_buffer_content();
    assert_eq!(
        buffer_content, "args.len",
        "Expected 'args.len', got: {}",
        buffer_content
    );
    assert!(
        !buffer_content.contains(".."),
        "Should not have double dots"
    );

    Ok(())
}

/// Test completion after typing a partial identifier after dot
#[test]
fn test_lsp_completion_after_dot_with_partial() -> std::io::Result<()> {
    use fresh::event::{Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData};

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Type "args.le"
    harness.type_text("args.le")?;
    harness.render()?;

    // Show completion popup
    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            title: Some("Completion".to_string()),
            content: PopupContentData::List {
                items: vec![PopupListItemData {
                    text: "length".to_string(),
                    detail: Some("fn length(&self) -> usize".to_string()),
                    icon: Some("λ".to_string()),
                    data: Some("length".to_string()),
                }],
                selected: 0,
            },
            position: PopupPositionData::BelowCursor,
            width: 40,
            max_height: 10,
            bordered: true,
        },
    });

    harness.render()?;

    // Confirm selection (should replace "le" with "length")
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.render()?;

    // Verify "args." is preserved and "le" is replaced with "length"
    let buffer_content = harness.get_buffer_content();
    assert_eq!(
        buffer_content, "args.length",
        "Expected 'args.length', got: {}",
        buffer_content
    );

    Ok(())
}

/// Test that completion filtering only shows matching items by prefix
#[test]
fn test_lsp_completion_filtering() -> std::io::Result<()> {
    use fresh::event::{Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData};

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Type a prefix "test_"
    harness.type_text("test_")?;
    harness.render()?;

    // Manually show completion popup with mixed items (simulating what would be filtered)
    // In reality, the filtering happens in handle_completion_response, but we simulate
    // the expected result here to test the concept
    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            title: Some("Completion".to_string()),
            content: PopupContentData::List {
                items: vec![
                    // Only items matching "test_" prefix should appear
                    PopupListItemData {
                        text: "test_function".to_string(),
                        detail: Some("fn test_function()".to_string()),
                        icon: Some("λ".to_string()),
                        data: Some("test_function".to_string()),
                    },
                    PopupListItemData {
                        text: "test_variable".to_string(),
                        detail: Some("let test_variable".to_string()),
                        icon: Some("v".to_string()),
                        data: Some("test_variable".to_string()),
                    },
                    // These should NOT appear (different prefix):
                    // - "Self" (doesn't start with "test_")
                    // - "something_else" (doesn't start with "test_")
                ],
                selected: 0,
            },
            position: PopupPositionData::BelowCursor,
            width: 40,
            max_height: 10,
            bordered: true,
        },
    });

    harness.render()?;

    // Verify popup is shown with only matching items
    let state = harness.editor().active_state();
    assert!(
        state.popups.top().is_some(),
        "Expected completion popup to be shown"
    );

    if let Some(popup) = state.popups.top() {
        if let fresh::popup::PopupContent::List { items, .. } = &popup.content {
            // Should only have test_function and test_variable
            assert_eq!(
                items.len(),
                2,
                "Expected 2 filtered items, got {}",
                items.len()
            );
            assert!(
                items.iter().any(|i| i.text.contains("test_function")),
                "Expected to find test_function in completions"
            );
            assert!(
                items.iter().any(|i| i.text.contains("test_variable")),
                "Expected to find test_variable in completions"
            );
        } else {
            panic!("Expected popup to have List content");
        }
    }

    // Confirm first selection (test_function)
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.render()?;

    // Verify completion replaced "test_" with "test_function"
    let buffer_content = harness.get_buffer_content();
    assert_eq!(
        buffer_content, "test_function",
        "Expected 'test_function', got: {}",
        buffer_content
    );

    Ok(())
}

/// Test that popup size is appropriate for the number of filtered items
#[test]
fn test_lsp_completion_popup_size() -> std::io::Result<()> {
    use fresh::event::{Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData};

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Type a prefix
    harness.type_text("test_")?;
    harness.render()?;

    // Show completion popup with only 2 items but max_height of 15
    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            title: Some("Completion".to_string()),
            content: PopupContentData::List {
                items: vec![
                    PopupListItemData {
                        text: "test_function".to_string(),
                        detail: Some("fn test_function()".to_string()),
                        icon: Some("λ".to_string()),
                        data: Some("test_function".to_string()),
                    },
                    PopupListItemData {
                        text: "test_variable".to_string(),
                        detail: Some("let test_variable".to_string()),
                        icon: Some("v".to_string()),
                        data: Some("test_variable".to_string()),
                    },
                ],
                selected: 0,
            },
            position: PopupPositionData::BelowCursor,
            width: 40,
            max_height: 15,  // Much larger than needed for 2 items
            bordered: true,
        },
    });

    harness.render()?;

    // Get the screen content
    let screen = harness.screen_to_string();
    println!("Screen content:\n{}", screen);

    // Count the number of visible lines in the popup
    // The popup should show:
    // - 1 line for top border
    // - 2 lines for items
    // - 1 line for bottom border
    // Total: 4 lines
    // But currently it's showing max_height (15) lines

    // Let's check by counting the border characters
    let lines: Vec<&str> = screen.lines().collect();

    // Find the popup borders
    let mut popup_start_line = None;
    let mut popup_end_line = None;

    for (idx, line) in lines.iter().enumerate() {
        if line.contains("Completion") {
            popup_start_line = Some(idx);
        }
        if popup_start_line.is_some() && (line.contains("└") || line.contains("╰")) {
            popup_end_line = Some(idx);
            break;
        }
    }

    if let (Some(start), Some(end)) = (popup_start_line, popup_end_line) {
        let popup_height = end - start + 1;
        println!("Popup height: {} lines", popup_height);

        // The popup should be sized for content (2 items + 2 borders = 4)
        // not for max_height (15)
        assert_eq!(
            popup_height, 4,
            "Expected popup to be sized for content (4 lines), but got {} lines",
            popup_height
        );

        println!("✓ Popup is appropriately sized: {} lines for 2 items", popup_height);
    } else {
        panic!("Could not find popup borders in screen output");
    }

    Ok(())
}

/// Test that LSP waiting indicator appears in status bar
#[test]
fn test_lsp_waiting_indicator() -> std::io::Result<()> {
    let mut harness = EditorTestHarness::new(80, 24)?;

    // Open a test file
    let temp_dir = tempfile::tempdir()?;
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn main() {\n    \n}\n")?;

    harness.open_file(&test_file)?;
    harness.render()?;

    // Position cursor inside the function
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.send_key(KeyCode::End, KeyModifiers::NONE)?;
    harness.render()?;

    // Request completion using Ctrl+Space (which will set the LSP waiting indicator)
    // Since we don't have a real LSP server in this test, the indicator will stay set
    harness.send_key(KeyCode::Char(' '), KeyModifiers::CONTROL)?;

    // Render to update the screen
    harness.render()?;

    // Get the screen content and check for LSP indicator
    let screen = harness.screen_to_string();
    println!("Screen with LSP indicator:\n{}", screen);

    // Check that "LSP: completion..." appears in the status bar
    assert!(
        screen.contains("LSP: completion..."),
        "Expected LSP waiting indicator in status bar, got:\n{}",
        screen
    );

    Ok(())
}

/// Test that popup properly hides buffer text behind it
#[test]
fn test_lsp_completion_popup_hides_background() -> std::io::Result<()> {
    use fresh::event::{Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData};

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Insert text that would be visible behind the popup if not properly cleared
    harness.type_text("let args = Args::parse();\nargs.log_file.create_log();\nsome_other_code_here();")?;
    harness.render()?;

    // Position cursor at the start of line 2 where we'll show the popup
    harness.send_key(KeyCode::Up, KeyModifiers::NONE)?;
    harness.send_key(KeyCode::Up, KeyModifiers::NONE)?;
    harness.send_key(KeyCode::Home, KeyModifiers::NONE)?;
    harness.render()?;

    // Show a completion popup that will overlap with the buffer text
    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            title: Some("Completion".to_string()),
            content: PopupContentData::List {
                items: vec![
                    PopupListItemData {
                        text: "args".to_string(),
                        detail: Some("Args".to_string()),
                        icon: Some("v".to_string()),
                        data: Some("args".to_string()),
                    },
                    PopupListItemData {
                        text: "Args".to_string(),
                        detail: Some("Args".to_string()),
                        icon: Some("S".to_string()),
                        data: Some("Args".to_string()),
                    },
                ],
                selected: 0,
            },
            position: PopupPositionData::BelowCursor,
            width: 40,
            max_height: 10,
            bordered: true,
        },
    });

    harness.render()?;

    // Get the screen content
    let screen = harness.screen_to_string();
    println!("Screen content:\n{}", screen);

    // Find the popup area by looking for the popup border and title
    let lines: Vec<&str> = screen.lines().collect();
    let mut in_popup = false;
    let mut popup_lines: Vec<&str> = Vec::new();

    for line in &lines {
        if line.contains("Completion") {
            in_popup = true;
        }
        if in_popup {
            popup_lines.push(line);
            if line.contains("└") || line.contains("╰") {
                break;
            }
        }
    }

    // Join popup lines to check content
    let popup_content = popup_lines.join("\n");
    println!("Popup area content:\n{}", popup_content);

    // Verify that buffer text is NOT bleeding through in the popup area
    // These strings from the buffer should NOT appear within the popup borders
    assert!(
        !popup_content.contains("log_file"),
        "Buffer text 'log_file' should not be visible through popup"
    );
    assert!(
        !popup_content.contains("create_log"),
        "Buffer text 'create_log' should not be visible through popup"
    );
    assert!(
        !popup_content.contains("code_here"),
        "Buffer text 'code_here' should not be visible through popup, found:\n{}",
        popup_content
    );
    assert!(
        !popup_content.contains("parse()"),
        "Buffer text 'parse()' should not be visible through popup, found:\n{}",
        popup_content
    );

    // Verify the actual completion items ARE visible
    assert!(
        popup_content.contains("args"),
        "Completion item 'args' should be visible in popup"
    );
    assert!(
        popup_content.contains("Args"),
        "Completion item 'Args' should be visible in popup"
    );

    Ok(())
}

/// Test that LSP completion request is canceled when cursor moves
#[test]
fn test_lsp_completion_canceled_on_cursor_move() -> std::io::Result<()> {
    let mut harness = EditorTestHarness::new(80, 24)?;

    // Open a test file
    let temp_dir = tempfile::tempdir()?;
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn main() {\n    test_\n}\n")?;

    harness.open_file(&test_file)?;
    harness.render()?;

    // Position cursor after "test_"
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.send_key(KeyCode::End, KeyModifiers::NONE)?;
    harness.render()?;

    // Request completion (sets pending request)
    harness.send_key(KeyCode::Char(' '), KeyModifiers::CONTROL)?;
    harness.render()?;

    // Verify LSP indicator is showing
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("LSP: completion..."),
        "Expected LSP indicator before cursor move"
    );

    // Move cursor (should cancel the request)
    harness.send_key(KeyCode::Left, KeyModifiers::NONE)?;
    harness.render()?;

    // Verify LSP indicator is gone (request canceled)
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("LSP: completion..."),
        "Expected LSP indicator to be cleared after cursor move"
    );

    // Verify pending request is cleared in editor
    let editor = harness.editor();
    assert!(
        !editor.has_pending_lsp_requests(),
        "Expected no pending LSP requests after cursor move"
    );

    Ok(())
}

/// Test that cursor shows waiting animation while LSP is pending
#[test]
fn test_lsp_cursor_animation() -> std::io::Result<()> {
    let mut harness = EditorTestHarness::new(80, 24)?;

    // Open a test file  
    let temp_dir = tempfile::tempdir()?;
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn main() {\n    test_\n}\n")?;

    harness.open_file(&test_file)?;
    harness.render()?;

    // Position cursor after "test_"
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.send_key(KeyCode::End, KeyModifiers::NONE)?;
    harness.render()?;

    // Get screen before LSP request
    let screen_before = harness.screen_to_string();

    // Request completion
    harness.send_key(KeyCode::Char(' '), KeyModifiers::CONTROL)?;
    harness.render()?;

    // Get screen during LSP wait
    let screen_during = harness.screen_to_string();
    println!("Screen before LSP:\n{}", screen_before);
    println!("Screen during LSP wait:\n{}", screen_during);

    // The cursor character should be replaced with the waiting indicator
    // Look for the waiting character "⋯" in the buffer area
    assert!(
        screen_during.contains("⋯"),
        "Expected waiting cursor character '⋯' to appear during LSP wait"
    );

    Ok(())
}

/// Test that LSP completion request is canceled when text is edited
#[test]
fn test_lsp_completion_canceled_on_text_edit() -> std::io::Result<()> {
    let mut harness = EditorTestHarness::new(80, 24)?;

    // Open a test file
    let temp_dir = tempfile::tempdir()?;
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn main() {\n    test_\n}\n")?;

    harness.open_file(&test_file)?;
    harness.render()?;

    // Position cursor after "test_"
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.send_key(KeyCode::End, KeyModifiers::NONE)?;
    harness.render()?;

    // Request completion
    harness.send_key(KeyCode::Char(' '), KeyModifiers::CONTROL)?;
    harness.render()?;

    // Verify LSP indicator is showing
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("LSP: completion..."),
        "Expected LSP indicator before text edit"
    );

    // Type a character (should cancel the request)
    harness.type_text("x")?;
    harness.render()?;

    // Verify LSP indicator is gone
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("LSP: completion..."),
        "Expected LSP indicator to be cleared after text edit"
    );

    // Verify pending request is cleared
    let editor = harness.editor();
    assert!(
        !editor.has_pending_lsp_requests(),
        "Expected no pending LSP requests after text edit"
    );

    Ok(())
}

/// Test LSP rename with real rust-analyzer to reproduce "content modified" error
/// Skip if rust-analyzer is not installed
#[test]
fn test_rust_analyzer_rename_content_modified() -> std::io::Result<()> {
    use std::io::Write;
    use std::process::Command;

    // Check if rust-analyzer is installed
    let rust_analyzer_check = Command::new("which").arg("rust-analyzer").output();
    if rust_analyzer_check.is_err() || !rust_analyzer_check.unwrap().status.success() {
        eprintln!("Skipping test: rust-analyzer not found in PATH");
        return Ok(());
    }

    eprintln!("rust-analyzer found, running test...");

    // Create a temporary directory and Rust file
    let temp_dir = tempfile::tempdir()?;
    let test_file = temp_dir.path().join("test.rs");
    let mut file = std::fs::File::create(&test_file)?;
    writeln!(file, "fn calculate(value: i32) -> i32 {{")?;
    writeln!(file, "    let result = value * 2;")?;
    writeln!(file, "    println!(\"Value: {{}}\", value);")?;
    writeln!(file, "    result")?;
    writeln!(file, "}}")?;
    drop(file);

    let mut harness = EditorTestHarness::new(80, 30)?;

    // Open the Rust file - this should trigger LSP initialization
    harness.open_file(&test_file)?;
    harness.render()?;

    // Wait a bit for LSP to initialize
    std::thread::sleep(std::time::Duration::from_millis(2000));

    // Process any async messages
    harness.editor_mut().process_async_messages();

    // Position cursor on "value" parameter (column 13)
    harness.send_key(KeyCode::Home, KeyModifiers::CONTROL)?; // Go to document start
    for _ in 0..13 {
        harness.send_key(KeyCode::Right, KeyModifiers::NONE)?;
    }
    harness.render()?;

    println!("Cursor positioned on 'value' parameter");

    // Press F2 to enter rename mode
    harness.send_key(KeyCode::F(2), KeyModifiers::NONE)?;
    harness.render()?;

    println!("Entered rename mode");

    // Delete "value" and type "amount" - this modifies the buffer
    for _ in 0..5 {
        harness.send_key(KeyCode::Backspace, KeyModifiers::NONE)?;
    }
    harness.type_text("amount")?;
    harness.render()?;

    println!("Typed new name 'amount'");

    // Get buffer content - should still show original "value" (NOT "amount")
    let buffer_content = harness.get_buffer_content();
    println!("Buffer content before Enter:\n{}", buffer_content);

    // Verify the buffer was NOT modified - it should still contain "value"
    assert!(buffer_content.contains("fn calculate(value: i32)"),
            "Buffer should still contain original 'value' text (fix working!)");
    assert!(!buffer_content.contains("amount"),
            "Buffer should NOT contain 'amount' yet (not applied until LSP responds)");

    // Press Enter to confirm rename - this will send LSP request
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.render()?;

    println!("Pressed Enter to confirm rename");

    // Wait for LSP response (rust-analyzer can take several seconds)
    for i in 0..20 {
        std::thread::sleep(std::time::Duration::from_millis(500));
        harness.editor_mut().process_async_messages();
        harness.render()?;

        // Check if response has arrived
        let screen = harness.screen_to_string();
        if !screen.contains("LSP: rename...") {
            println!("LSP response received after {}ms", (i + 1) * 500);
            break;
        }
    }

    // Check screen - should NOT contain "content modified" error anymore
    let screen = harness.screen_to_string();
    println!("Screen output:\n{}", screen);

    // After fix, we should NOT see "content modified" error
    // The buffer content was not modified, so LSP can successfully rename
    if screen.contains("content modified") {
        panic!("Still got 'content modified' error - fix didn't work!");
    }

    // We should see either success message or pending LSP request
    assert!(screen.contains("LSP:") || screen.contains("Renamed") || screen.contains("Renaming"),
            "Should show LSP status message");

    // CRITICAL: Verify the buffer content was actually changed!
    let buffer_content_after = harness.get_buffer_content();
    println!("Buffer content after rename:\n{}", buffer_content_after);

    // The rename should have been applied - buffer should contain "amount" NOT "value"
    assert!(buffer_content_after.contains("fn calculate(amount: i32)"),
            "Buffer should contain 'amount' after successful rename! Got:\n{}", buffer_content_after);
    assert!(buffer_content_after.contains("println!(\"Value: {{}}\", amount)"),
            "All references to 'value' should be renamed to 'amount'! Got:\n{}", buffer_content_after);
    assert!(!buffer_content_after.contains("fn calculate(value: i32)"),
            "Buffer should NOT contain old 'value' parameter after rename! Got:\n{}", buffer_content_after);

    println!("\n========================================");
    println!("SUCCESS: Rename applied successfully!");
    println!("Buffer was NOT modified during typing");
    println!("LSP rename proceeded successfully");
    println!("All references renamed from 'value' to 'amount'");
    println!("========================================\n");

    Ok(())
}

/// Test that handle_rename_response correctly processes documentChanges
/// (This tests the fix for rust-analyzer which sends documentChanges instead of changes)
#[test]
fn test_handle_rename_response_with_document_changes() -> std::io::Result<()> {
    use lsp_types::{
        DocumentChanges, OneOf, OptionalVersionedTextDocumentIdentifier,
        Position, Range, TextDocumentEdit, TextEdit, Url, WorkspaceEdit,
    };

    let mut harness = EditorTestHarness::new(80, 30)?;

    // Create a temporary file with some Rust code
    let temp_dir = tempfile::tempdir()?;
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn calculate(value: i32) -> i32 {\n    let result = value * 2;\n    println!(\"Value: {}\", value);\n    result\n}\n")?;

    // Open the file
    harness.open_file(&test_file)?;
    harness.render()?;

    // Create a WorkspaceEdit with documentChanges (like rust-analyzer sends)
    let uri = Url::from_file_path(&test_file).unwrap();
    let text_edit_1 = TextEdit {
        range: Range {
            start: Position { line: 0, character: 13 },
            end: Position { line: 0, character: 18 },
        },
        new_text: "amount".to_string(),
    };
    let text_edit_2 = TextEdit {
        range: Range {
            start: Position { line: 2, character: 26 },
            end: Position { line: 2, character: 31 },
        },
        new_text: "amount".to_string(),
    };

    let text_doc_edit = TextDocumentEdit {
        text_document: OptionalVersionedTextDocumentIdentifier {
            uri,
            version: Some(1),
        },
        edits: vec![
            OneOf::Left(text_edit_1),
            OneOf::Left(text_edit_2),
        ],
    };

    let workspace_edit = WorkspaceEdit {
        changes: None,  // rust-analyzer doesn't send this
        document_changes: Some(DocumentChanges::Edits(vec![text_doc_edit])),
        change_annotations: None,
    };

    // Call handle_rename_response directly
    harness.editor_mut().handle_rename_response(0, Ok(workspace_edit))?;
    harness.render()?;

    // Verify the buffer was modified
    let buffer_content = harness.get_buffer_content();
    println!("Buffer content after rename:\n{}", buffer_content);

    assert!(buffer_content.contains("fn calculate(amount: i32)"),
            "Buffer should contain 'amount' in function parameter! Got:\n{}", buffer_content);
    assert!(buffer_content.contains("amount);"),
            "Buffer should contain 'amount' in println! Got:\n{}", buffer_content);
    assert!(buffer_content.contains("let result = value * 2"),
            "The second occurrence of 'value' should NOT be replaced (we only specified 2 edits)");
    assert!(!buffer_content.contains("value: i32") && !buffer_content.contains("value);"),
            "Buffer should NOT contain old 'value' in parameter or println! Got:\n{}", buffer_content);

    println!("SUCCESS: documentChanges handled correctly!");

    Ok(())
}
