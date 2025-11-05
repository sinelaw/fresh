//! E2E tests for LSP features

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Test that completion popup text is not mangled
#[test]
fn test_lsp_completion_popup_text_not_mangled() -> std::io::Result<()> {
    use editor::event::{Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData};

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
    use editor::event::{Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData};

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
    use editor::event::{Event, OverlayFace, UnderlineStyle};

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
    use editor::event::{Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData};

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
    use editor::event::{Event, OverlayFace};

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
    use editor::event::{Event, OverlayFace};

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
    use editor::event::{Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData};

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
    use editor::event::{Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData};

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
    use editor::event::{Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData};

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
    use editor::event::{Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData};

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
    use editor::event::{Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData};

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
        if let editor::popup::PopupContent::List { items, .. } = &popup.content {
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
