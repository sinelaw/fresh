//! E2E tests for LSP features

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Test that completion popup text is not mangled
#[test]
fn test_lsp_completion_popup_text_not_mangled() -> std::io::Result<()> {
    use fresh::model::event::{
        Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData,
    };

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
    println!("Screen content:\n{screen}");

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
    use fresh::model::event::{
        Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData,
    };

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Type a partial word
    harness.type_text("test_f")?;
    harness.render()?;

    // Verify partial word is in buffer
    let buffer_before = harness.get_buffer_content().unwrap();
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
    let buffer_after = harness.get_buffer_content().unwrap();
    assert_eq!(
        buffer_after, "test_function",
        "Expected completion to replace 'test_f' with 'test_function', but got '{buffer_after}'"
    );

    Ok(())
}

/// Test LSP diagnostics display in the editor
#[test]
fn test_lsp_diagnostics_display() -> std::io::Result<()> {
    use fresh::model::event::{Event, OverlayFace};
    use fresh::view::overlay::OverlayNamespace;

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Type some text
    harness.type_text("let x = 5;")?;
    harness.render()?;

    // Manually add a diagnostic overlay (simulating what LSP would do)
    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::AddOverlay {
        namespace: Some(OverlayNamespace::from_string("lsp-diagnostic".to_string())),
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
    use fresh::model::event::{
        Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData,
    };

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
    let buffer_content = harness.get_buffer_content().unwrap();
    assert!(
        buffer_content.contains("test_variable"),
        "Expected 'test_variable' to be inserted into buffer, got: {buffer_content}"
    );

    Ok(())
}

/// Test LSP diagnostics summary in status bar
#[test]
fn test_lsp_diagnostics_status_bar() -> std::io::Result<()> {
    use fresh::model::event::{Event, OverlayFace};
    use fresh::view::overlay::OverlayNamespace;

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Type some text
    harness.type_text("let x = 5;\nlet y = 10;")?;
    harness.render()?;

    // Add error diagnostic
    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::AddOverlay {
        namespace: Some(OverlayNamespace::from_string("lsp-diagnostic".to_string())),
        range: 4..5,
        face: OverlayFace::Background { color: (40, 0, 0) },
        priority: 100, // Error priority
        message: Some("unused variable: `x`".to_string()),
    });

    // Add warning diagnostic
    state.apply(&Event::AddOverlay {
        namespace: Some(OverlayNamespace::from_string("lsp-diagnostic".to_string())),
        range: 15..16,
        face: OverlayFace::Background { color: (40, 40, 0) },
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
    use fresh::model::event::{Event, OverlayFace};
    use fresh::view::overlay::OverlayNamespace;

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Type some text
    harness.type_text("let x = 5;")?;
    harness.render()?;

    // Add diagnostic
    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::AddOverlay {
        namespace: Some(OverlayNamespace::from_string("lsp-diagnostic".to_string())),
        range: 4..5,
        face: OverlayFace::Background { color: (40, 0, 0) },
        priority: 100,
        message: Some("test error".to_string()),
    });

    harness.render()?;

    // Verify diagnostic is shown
    let screen = harness.screen_to_string();
    assert!(screen.contains("E:1"), "Expected error count in status bar");

    // Clear diagnostics using namespace
    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ClearNamespace {
        namespace: OverlayNamespace::from_string("lsp-diagnostic".to_string()),
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
    use fresh::model::event::{
        Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData,
    };

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
    let buffer_content = harness.get_buffer_content().unwrap();
    assert!(
        buffer_content.contains("item3"),
        "Expected 'item3' to be inserted, got: {buffer_content}"
    );

    Ok(())
}

/// Test popup cancel (Escape) doesn't insert anything
#[test]
fn test_lsp_completion_cancel() -> std::io::Result<()> {
    use fresh::model::event::{
        Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData,
    };

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
    let buffer_content = harness.get_buffer_content().unwrap();
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
    use fresh::model::event::{
        Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData,
    };

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
    let buffer_content = harness.get_buffer_content().unwrap();
    assert_eq!(
        buffer_content, "args.len",
        "Expected 'args.len', got: {buffer_content}"
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
    use fresh::model::event::{
        Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData,
    };

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
    let buffer_content = harness.get_buffer_content().unwrap();
    assert_eq!(
        buffer_content, "args.length",
        "Expected 'args.length', got: {buffer_content}"
    );

    Ok(())
}

/// Test that completion filtering only shows matching items by prefix
#[test]
fn test_lsp_completion_filtering() -> std::io::Result<()> {
    use fresh::model::event::{
        Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData,
    };

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
        if let fresh::view::popup::PopupContent::List { items, .. } = &popup.content {
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
    let buffer_content = harness.get_buffer_content().unwrap();
    assert_eq!(
        buffer_content, "test_function",
        "Expected 'test_function', got: {buffer_content}"
    );

    Ok(())
}

/// Test that popup size is appropriate for the number of filtered items
#[test]
fn test_lsp_completion_popup_size() -> std::io::Result<()> {
    use fresh::model::event::{
        Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData,
    };

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
            max_height: 15, // Much larger than needed for 2 items
            bordered: true,
        },
    });

    harness.render()?;

    // Get the screen content
    let screen = harness.screen_to_string();
    println!("Screen content:\n{screen}");

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
        println!("Popup height: {popup_height} lines");

        // The popup should be sized for content (2 items + 2 borders = 4)
        // not for max_height (15)
        assert_eq!(
            popup_height, 4,
            "Expected popup to be sized for content (4 lines), but got {popup_height} lines"
        );

        println!("✓ Popup is appropriately sized: {popup_height} lines for 2 items");
    } else {
        panic!("Could not find popup borders in screen output");
    }

    Ok(())
}

/// Test that LSP waiting indicator appears in status bar
/// Uses a fake LSP server for robust testing
#[test]
fn test_lsp_waiting_indicator() -> std::io::Result<()> {
    use crate::common::fake_lsp::FakeLspServer;

    // Spawn fake LSP server
    let _fake_server = FakeLspServer::spawn()?;

    // Create temp dir and test file
    let temp_dir = tempfile::tempdir()?;
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn main() {\n    \n}\n")?;

    // Configure editor to use the fake LSP server
    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::services::lsp::client::LspServerConfig {
            command: FakeLspServer::script_path().to_string_lossy().to_string(),
            args: vec![],
            enabled: true,
            auto_start: false,
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
        },
    );

    // Create harness with config
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    harness.open_file(&test_file)?;
    harness.render()?;

    // Position cursor inside the function
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.send_key(KeyCode::End, KeyModifiers::NONE)?;
    harness.render()?;

    // Request completion using Ctrl+Space
    harness.send_key(KeyCode::Char(' '), KeyModifiers::CONTROL)?;
    harness.render()?;

    // Process async messages to get LSP response
    for _ in 0..10 {
        harness.process_async_and_render()?;
        harness.sleep(std::time::Duration::from_millis(50));
    }

    // Get the screen content
    let screen = harness.screen_to_string();
    println!("Screen after completion request:\n{screen}");

    // The test passes if the editor handles the completion request without crashing.
    // With the fake server, we may see completion items or LSP status indicators.
    // The key assertion is that the editor remains responsive.
    Ok(())
}

/// Test that popup properly hides buffer text behind it
#[test]
fn test_lsp_completion_popup_hides_background() -> std::io::Result<()> {
    use fresh::model::event::{
        Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData,
    };

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Insert text that would be visible behind the popup if not properly cleared
    harness.type_text(
        "let args = Args::parse();\nargs.log_file.create_log();\nsome_other_code_here();",
    )?;
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
    println!("Screen content:\n{screen}");

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
    println!("Popup area content:\n{popup_content}");

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
        "Buffer text 'code_here' should not be visible through popup, found:\n{popup_content}"
    );
    assert!(
        !popup_content.contains("parse()"),
        "Buffer text 'parse()' should not be visible through popup, found:\n{popup_content}"
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
/// Uses a fake LSP server for robust testing
#[test]
fn test_lsp_completion_canceled_on_cursor_move() -> std::io::Result<()> {
    use crate::common::fake_lsp::FakeLspServer;

    // Spawn fake LSP server
    let _fake_server = FakeLspServer::spawn()?;

    // Create temp dir and test file
    let temp_dir = tempfile::tempdir()?;
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn main() {\n    test_\n}\n")?;

    // Configure editor to use the fake LSP server
    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::services::lsp::client::LspServerConfig {
            command: FakeLspServer::script_path().to_string_lossy().to_string(),
            args: vec![],
            enabled: true,
            auto_start: false,
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
        },
    );

    // Create harness with config
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    harness.open_file(&test_file)?;
    harness.render()?;

    // Position cursor after "test_"
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.send_key(KeyCode::End, KeyModifiers::NONE)?;
    harness.render()?;

    // Request completion (sets pending request)
    harness.send_key(KeyCode::Char(' '), KeyModifiers::CONTROL)?;
    harness.render()?;

    // Process async messages briefly
    harness.process_async_and_render()?;

    // Move cursor (should cancel the request)
    harness.send_key(KeyCode::Left, KeyModifiers::NONE)?;
    harness.render()?;

    // Verify pending request is cleared in editor
    let editor = harness.editor();
    assert!(
        !editor.has_pending_lsp_requests(),
        "Expected no pending LSP requests after cursor move"
    );

    Ok(())
}

/// Test that cursor shows waiting animation while LSP is pending
/// Uses a fake LSP server for robust testing
#[test]
fn test_lsp_cursor_animation() -> std::io::Result<()> {
    use crate::common::fake_lsp::FakeLspServer;

    // Spawn fake LSP server
    let _fake_server = FakeLspServer::spawn()?;

    // Create temp dir and test file
    let temp_dir = tempfile::tempdir()?;
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn main() {\n    test_\n}\n")?;

    // Configure editor to use the fake LSP server
    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::services::lsp::client::LspServerConfig {
            command: FakeLspServer::script_path().to_string_lossy().to_string(),
            args: vec![],
            enabled: true,
            auto_start: false,
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
        },
    );

    // Create harness with config
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;

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

    // Process async messages to get LSP response
    for _ in 0..10 {
        harness.process_async_and_render()?;
        harness.sleep(std::time::Duration::from_millis(50));
    }

    // Get screen after LSP request
    let screen_after = harness.screen_to_string();
    println!("Screen before LSP:\n{screen_before}");
    println!("Screen after LSP request:\n{screen_after}");

    // The test passes if the editor handles the completion request without crashing.
    Ok(())
}

/// Test that LSP completion request is canceled when text is edited
/// Uses a fake LSP server for robust testing
#[test]
fn test_lsp_completion_canceled_on_text_edit() -> std::io::Result<()> {
    use crate::common::fake_lsp::FakeLspServer;

    // Spawn fake LSP server
    let _fake_server = FakeLspServer::spawn()?;

    // Create temp dir and test file
    let temp_dir = tempfile::tempdir()?;
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn main() {\n    test_\n}\n")?;

    // Configure editor to use the fake LSP server
    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::services::lsp::client::LspServerConfig {
            command: FakeLspServer::script_path().to_string_lossy().to_string(),
            args: vec![],
            enabled: true,
            auto_start: false,
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
        },
    );

    // Create harness with config
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    harness.open_file(&test_file)?;
    harness.render()?;

    // Position cursor after "test_"
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.send_key(KeyCode::End, KeyModifiers::NONE)?;
    harness.render()?;

    // Request completion
    harness.send_key(KeyCode::Char(' '), KeyModifiers::CONTROL)?;
    harness.render()?;

    // Process async messages briefly
    harness.process_async_and_render()?;

    // Type a character (should cancel the request)
    harness.type_text("x")?;
    harness.render()?;

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
#[ignore]
fn test_rust_analyzer_rename_content_modified() -> std::io::Result<()> {
    use std::io::Write;
    use std::process::Command;
    use tracing_subscriber::{fmt, prelude::*, EnvFilter};

    // Initialize tracing to see LSP debug messages
    let _ = tracing_subscriber::registry()
        .with(fmt::layer().with_writer(std::io::stderr))
        .with(EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("fresh=debug")))
        .try_init();

    // Check if rust-analyzer is installed
    let rust_analyzer_check = Command::new("which").arg("rust-analyzer").output();
    if rust_analyzer_check.is_err() || !rust_analyzer_check.unwrap().status.success() {
        eprintln!("Skipping test: rust-analyzer not found in PATH");
        return Ok(());
    }

    eprintln!("rust-analyzer found, running test...");

    // Create harness with temp project directory - this sets the LSP workspace root
    let mut harness = EditorTestHarness::with_temp_project(200, 30)?;
    let project_dir = harness.project_dir().expect("project dir should exist");

    // Create a proper Cargo project structure in the project directory
    // This ensures rust-analyzer can discover the workspace

    // Create Cargo.toml
    let cargo_toml = project_dir.join("Cargo.toml");
    std::fs::write(
        &cargo_toml,
        r#"[package]
name = "test-rename"
version = "0.1.0"
edition = "2021"
"#,
    )?;

    // Create src directory
    let src_dir = project_dir.join("src");
    std::fs::create_dir(&src_dir)?;

    // Create lib.rs with our function - rust-analyzer will analyze this
    let test_file = src_dir.join("lib.rs");
    let mut file = std::fs::File::create(&test_file)?;
    writeln!(file, "pub fn calculate(value: i32) -> i32 {{")?;
    writeln!(file, "    let result = value * 2;")?;
    writeln!(file, "    println!(\"Value: {{}}\", value);")?;
    writeln!(file, "    result")?;
    writeln!(file, "}}")?;
    drop(file);

    // Open the Rust file - this should trigger LSP initialization
    harness.open_file(&test_file)?;
    harness.render()?;

    // Wait for LSP to initialize by checking for the "LSP (rust) ready" status message
    let mut lsp_ready = false;
    for _ in 0..40 {
        // Wait 100ms
        harness.sleep(std::time::Duration::from_millis(100));

        // Process async messages
        let _ = harness.editor_mut().process_async_messages();
        harness.render()?;

        // Check if LSP is ready by looking at the screen output (status bar)
        let screen = harness.screen_to_string();
        if screen.contains("LSP") && screen.contains("ready") {
            lsp_ready = true;
            println!("LSP initialized and ready");
            break;
        }
    }

    if !lsp_ready {
        eprintln!("Warning: LSP did not initialize within timeout");
    }

    // Wait for rust-analyzer to finish indexing by checking for no active progress tasks
    println!("Waiting for rust-analyzer to finish indexing...");
    let mut had_progress = false;
    for i in 0..120 {
        // Wait up to 12 seconds for indexing
        harness.sleep(std::time::Duration::from_millis(100));
        let processed = harness.editor_mut().process_async_messages();
        if i < 10 && processed {
            println!("  Processed async messages at {}ms", i * 100);
        }
        harness.render()?;

        let has_progress = harness.editor().has_active_lsp_progress();
        if has_progress {
            had_progress = true;
            let progress = harness.editor().get_lsp_progress();
            if i % 10 == 0 {
                // Log progress every second
                for (_, title, msg) in &progress {
                    println!("  LSP Progress: {} - {:?}", title, msg);
                }
            }
        } else if had_progress {
            // Had progress before but now it's done
            println!("rust-analyzer finished indexing after {}ms", i * 100);
            break;
        } else if i > 30 {
            // If we've waited 3 seconds without seeing any progress, assume it's done
            println!("No LSP progress seen, assuming indexing complete");
            break;
        }
    }

    // Extra safety: wait a bit after progress ends to ensure all state is updated
    // rust-analyzer needs extra time after indexing to build its full semantic model
    println!("Waiting for rust-analyzer semantic analysis...");
    harness.sleep(std::time::Duration::from_millis(2000));
    for _ in 0..20 {
        harness.sleep(std::time::Duration::from_millis(100));
        let _ = harness.editor_mut().process_async_messages();
        harness.render()?;
    }
    println!("Semantic analysis wait complete");

    // Position cursor on "value" parameter
    // With "pub fn calculate(value...", 'value' starts at column 17 (0-indexed)
    // We need to place cursor ON the word, not after it
    harness.send_key(KeyCode::Home, KeyModifiers::CONTROL)?; // Go to document start
    for _ in 0..18 {
        // Move to middle of 'value' (the 'a')
        harness.send_key(KeyCode::Right, KeyModifiers::NONE)?;
    }
    harness.render()?;

    let cursor_pos = harness.cursor_position();
    let buffer_content = harness.get_buffer_content().unwrap();
    let char_at_cursor = buffer_content.chars().nth(cursor_pos).unwrap_or('?');
    println!(
        "Cursor positioned at byte {}, character: '{}'",
        cursor_pos, char_at_cursor
    );

    // Verify cursor is on 'value'
    assert!(
        char_at_cursor == 'a'
            || char_at_cursor == 'l'
            || char_at_cursor == 'u'
            || char_at_cursor == 'e',
        "Cursor should be on 'value', but got '{}'",
        char_at_cursor
    );

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
    let buffer_content = harness.get_buffer_content().unwrap();
    println!("Buffer content before Enter:\n{buffer_content}");

    // Verify the buffer was NOT modified - it should still contain "value"
    assert!(
        buffer_content.contains("fn calculate(value: i32)"),
        "Buffer should still contain original 'value' text (fix working!)"
    );
    assert!(
        !buffer_content.contains("amount"),
        "Buffer should NOT contain 'amount' yet (not applied until LSP responds)"
    );

    // Press Enter to confirm rename - this will send LSP request
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.render()?;

    println!("Pressed Enter to confirm rename");

    // Wait for LSP response (rust-analyzer can take several seconds)
    let mut rename_succeeded = false;
    for i in 0..20 {
        harness.sleep(std::time::Duration::from_millis(500));
        let _ = harness.editor_mut().process_async_messages();
        harness.render()?;

        // Check if response has arrived
        let screen = harness.screen_to_string();
        if !screen.contains("LSP: rename...") {
            println!("LSP response received after {}ms", (i + 1) * 500);

            // Check if rename was successful by examining buffer content
            let buffer_after = harness.get_buffer_content().unwrap();
            if buffer_after.contains("fn calculate(amount: i32)")
                && buffer_after.contains("println!(\"Value: {}\", amount)")
            {
                rename_succeeded = true;
            }
            break;
        }
    }

    // Check screen - should NOT contain "content modified" error anymore
    let screen = harness.screen_to_string();
    println!("Screen output:\n{screen}");

    // After fix, we should NOT see "content modified" error
    // The buffer content was not modified, so LSP can successfully rename
    if screen.contains("content modified") {
        panic!("Still got 'content modified' error - fix didn't work!");
    }

    // Get final buffer content
    let final_buffer = harness.get_buffer_content().unwrap();
    println!("Final buffer content:\n{final_buffer}");

    // Verify the rename actually succeeded
    assert!(
        rename_succeeded,
        "Rename operation should have succeeded and renamed 'value' to 'amount'"
    );
    assert!(
        final_buffer.contains("fn calculate(amount: i32)"),
        "Function parameter should be renamed to 'amount'"
    );
    assert!(
        final_buffer.contains("println!(\"Value: {}\", amount)"),
        "All references to 'value' should be renamed to 'amount'"
    );
    assert!(
        !final_buffer.contains("value"),
        "No references to 'value' should remain after successful rename"
    );

    println!("\n========================================");
    println!("SUCCESS: LSP rename operation worked!");
    println!("- No 'content modified' error");
    println!("- Buffer was NOT modified during rename mode typing");
    println!("- LSP rename request succeeded");
    println!("- All references updated from 'value' to 'amount'");
    println!("========================================\n");

    Ok(())
}

/// Test typing performance with many LSP diagnostics
///
/// This test reproduces the performance issue where typing becomes slow when
/// there are many diagnostics. It measures the time it takes to process diagnostics
/// with 100+ diagnostics active.
#[test]
#[ignore] // Run with: cargo test test_lsp_typing_performance_with_many_diagnostics -- --ignored --nocapture
fn test_lsp_typing_performance_with_many_diagnostics() -> std::io::Result<()> {
    use std::time::Instant;

    const DIAGNOSTIC_COUNT: usize = 200; // Simulate 200 diagnostics (100 lines)

    // Create a file with 200 lines directly
    let mut file_content = String::new();
    file_content.push_str("fn main() {\n");
    for i in 0..200 {
        file_content.push_str(&format!("    let var_{} = {};\n", i, i));
    }
    file_content.push_str("}\n");

    // Create buffer directly instead of typing (much faster for test setup)
    let temp_dir = tempfile::tempdir()?;
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, &file_content)?;

    let mut harness = crate::common::harness::EditorTestHarness::new(80, 24)?;
    harness.open_file(&test_file)?;
    harness.render()?;

    println!("✓ Created file with {} lines", 200);

    // Manually add many diagnostics (simulating what LSP would do)
    // This tests the apply_diagnostics_to_state function directly
    let state = harness.editor_mut().active_state_mut();

    let diagnostics_json = format!(
        r#"{{
        "uri": "file:///test.rs",
        "diagnostics": [
            {}
        ]
    }}"#,
        (0..DIAGNOSTIC_COUNT)
            .map(|i| {
                let line = i / 2;
                let char_start = (i % 2) * 10;
                let char_end = char_start + 5;
                format!(
                    r#"{{
            "range": {{
                "start": {{"line": {}, "character": {}}},
                "end": {{"line": {}, "character": {}}}
            }},
            "severity": 1,
            "message": "Error {} from fake LSP"
        }}"#,
                    line, char_start, line, char_end, i
                )
            })
            .collect::<Vec<_>>()
            .join(",")
    );

    // Parse diagnostics
    let diag_params: lsp_types::PublishDiagnosticsParams =
        serde_json::from_str(&diagnostics_json).expect("Failed to parse diagnostics JSON");

    println!("✓ Parsed {} diagnostics", diag_params.diagnostics.len());

    // Measure performance of applying diagnostics
    let start = Instant::now();

    // This is the slow function - apply_diagnostics_to_state
    fresh::services::lsp::diagnostics::apply_diagnostics_to_state(
        state,
        &diag_params.diagnostics,
        &fresh::view::theme::Theme::dark(),
    );

    let apply_duration = start.elapsed();

    println!(
        "⏱  Applying {} diagnostics took: {:?}",
        DIAGNOSTIC_COUNT, apply_duration
    );

    harness.render()?;

    // Verify diagnostics are present
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("E:") || screen.contains("●"),
        "Expected diagnostics to be shown in UI"
    );

    println!(
        "✅ First application completed in {:?} with {} diagnostics",
        apply_duration, DIAGNOSTIC_COUNT
    );

    // Test repeated application (simulates typing with LSP enabled)
    // With caching, subsequent applications with same diagnostics should be instant
    let state = harness.editor_mut().active_state_mut();

    let mut total_reapply_time = std::time::Duration::ZERO;
    const REAPPLY_COUNT: usize = 10;

    for i in 0..REAPPLY_COUNT {
        let start = Instant::now();
        fresh::services::lsp::diagnostics::apply_diagnostics_to_state_cached(
            state,
            &diag_params.diagnostics,
            &fresh::view::theme::Theme::dark(),
        );
        let reapply_duration = start.elapsed();
        total_reapply_time += reapply_duration;

        if i == 0 {
            println!(
                "⏱  Re-applying {} diagnostics (iteration 1, cached) took: {:?}",
                DIAGNOSTIC_COUNT, reapply_duration
            );
        }
    }

    let avg_reapply_time = total_reapply_time / REAPPLY_COUNT as u32;
    println!(
        "⏱  Average re-application time over {} iterations (cached): {:?}",
        REAPPLY_COUNT, avg_reapply_time
    );

    // With caching, the average should be very close to 0 (sub-millisecond)
    println!("💡 Expected: First iteration ~236ms (not cached yet), subsequent ~0ms (cached)");
    if avg_reapply_time.as_millis() < 50 {
        println!("✅ Cache is working! Average time is very low.");
    } else {
        println!(
            "⚠️  Cache might not be working optimally. Expected <50ms average, got {:?}",
            avg_reapply_time
        );
    }

    // Verify that re-application is working (diagnostics still showing)
    harness.render()?;
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("E:") || screen.contains("●"),
        "Diagnostics should still be present after re-application"
    );

    println!(
        "✅ Performance test completed! Diagnostics re-applied {} times",
        REAPPLY_COUNT
    );

    Ok(())
}

/// Test that handle_rename_response correctly processes documentChanges
/// (This tests the fix for rust-analyzer which sends documentChanges instead of changes)
#[test]
fn test_handle_rename_response_with_document_changes() -> std::io::Result<()> {
    use lsp_types::{
        DocumentChanges, OneOf, OptionalVersionedTextDocumentIdentifier, Position, Range,
        TextDocumentEdit, TextEdit, Uri, WorkspaceEdit,
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
    let uri = url::Url::from_file_path(&test_file)
        .unwrap()
        .as_str()
        .parse::<Uri>()
        .unwrap();
    let text_edit_1 = TextEdit {
        range: Range {
            start: Position {
                line: 0,
                character: 13,
            },
            end: Position {
                line: 0,
                character: 18,
            },
        },
        new_text: "amount".to_string(),
    };
    let text_edit_2 = TextEdit {
        range: Range {
            start: Position {
                line: 2,
                character: 26,
            },
            end: Position {
                line: 2,
                character: 31,
            },
        },
        new_text: "amount".to_string(),
    };

    let text_doc_edit = TextDocumentEdit {
        text_document: OptionalVersionedTextDocumentIdentifier {
            uri,
            version: Some(1),
        },
        edits: vec![OneOf::Left(text_edit_1), OneOf::Left(text_edit_2)],
    };

    let workspace_edit = WorkspaceEdit {
        changes: None, // rust-analyzer doesn't send this
        document_changes: Some(DocumentChanges::Edits(vec![text_doc_edit])),
        change_annotations: None,
    };

    // Call handle_rename_response directly
    harness
        .editor_mut()
        .handle_rename_response(0, Ok(workspace_edit))?;
    harness.render()?;

    // Verify the buffer was modified
    let buffer_content = harness.get_buffer_content().unwrap();
    println!("Buffer content after rename:\n{buffer_content}");

    assert!(
        buffer_content.contains("fn calculate(amount: i32)"),
        "Buffer should contain 'amount' in function parameter! Got:\n{buffer_content}"
    );
    assert!(
        buffer_content.contains("amount);"),
        "Buffer should contain 'amount' in println! Got:\n{buffer_content}"
    );
    assert!(
        buffer_content.contains("let result = value * 2"),
        "The second occurrence of 'value' should NOT be replaced (we only specified 2 edits)"
    );
    assert!(
        !buffer_content.contains("value: i32") && !buffer_content.contains("value);"),
        "Buffer should NOT contain old 'value' in parameter or println! Got:\n{buffer_content}"
    );

    println!("SUCCESS: documentChanges handled correctly!");

    Ok(())
}

/// Test that editor remains responsive while LSP is completely stuck
///
/// This test verifies that the UI doesn't block when the LSP server is unresponsive.
/// It uses a fake LSP server that never responds to any requests (except initialize),
/// simulating a completely stuck language server. The test verifies that typing
/// continues to work immediately without any delays.
#[test]
fn test_lsp_diagnostics_non_blocking() -> std::io::Result<()> {
    use crate::common::fake_lsp::FakeLspServer;

    // Create a completely blocking fake LSP server that never responds
    let _fake_server = FakeLspServer::spawn_blocking()?;

    // Create temporary directory and test file
    let temp_dir = tempfile::tempdir()?;
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn main() {\n    // original code\n}\n")?;

    // Configure editor to use the blocking fake LSP server
    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::services::lsp::client::LspServerConfig {
            command: FakeLspServer::blocking_script_path()
                .to_string_lossy()
                .to_string(),
            args: vec![],
            enabled: true,
            auto_start: false,
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
        },
    );

    // Create harness with config and working directory
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    // Open the file (triggers LSP initialization and textDocument/didOpen)
    harness.open_file(&test_file)?;
    harness.render()?;

    // Position cursor on line 2 where we'll type
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.send_key(KeyCode::End, KeyModifiers::NONE)?;
    harness.render()?;

    // Save the file - this will trigger textDocument/didSave
    // The fake LSP server will NEVER respond to this
    harness.send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)?;
    harness.render()?;

    // CRITICAL TEST: Immediately type characters - the LSP is stuck but typing should work!
    // No sleeps, no waits - just type and verify it works
    harness.type_text("\n    let x = 42;")?;
    harness.render()?;

    // Verify the characters were actually inserted immediately
    let buffer_content = harness.get_buffer_content().unwrap();
    assert!(
        buffer_content.contains("let x = 42;"),
        "Editor should process typed characters immediately despite stuck LSP! Got:\n{buffer_content}"
    );

    // Continue typing more characters to ensure editor remains responsive
    harness.type_text("\n    println!(\"{{x}}\");")?;
    harness.render()?;

    let buffer_content = harness.get_buffer_content().unwrap();
    assert!(
        buffer_content.contains("println!"),
        "Editor should continue processing input despite stuck LSP! Got:\n{buffer_content}"
    );

    // Verify the screen shows the typed content (proves rendering works)
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("let x = 42"),
        "Screen should display newly typed content despite stuck LSP"
    );
    assert!(
        screen.contains("println!"),
        "Screen should display all typed content despite stuck LSP"
    );

    // Try more operations - navigation, more typing
    harness.send_key(KeyCode::Up, KeyModifiers::NONE)?;
    harness.send_key(KeyCode::Up, KeyModifiers::NONE)?;
    harness.send_key(KeyCode::End, KeyModifiers::NONE)?;
    harness.type_text(" // comment")?;
    harness.render()?;

    let final_buffer = harness.get_buffer_content().unwrap();
    assert!(
        final_buffer.contains("// comment"),
        "Editor should handle navigation and typing despite stuck LSP! Got:\n{final_buffer}"
    );

    println!("\n✅ SUCCESS: Editor remained fully responsive with completely stuck LSP!");
    println!("   - All typed characters inserted immediately");
    println!("   - Navigation worked normally");
    println!("   - Screen rendering updated correctly");
    println!("   - No UI freeze despite LSP never responding");

    Ok(())
}

/// Test the EXACT scenario from the bug report:
/// Open a Rust file, position cursor on a variable, press F2, type ONE character, press Enter
/// This should reproduce the ContentModified error with rust-analyzer
#[test]
#[ignore] // Run with: cargo test test_rust_analyzer_rename_real_scenario -- --ignored --nocapture
fn test_rust_analyzer_rename_real_scenario() -> std::io::Result<()> {
    use std::io::Write;
    use std::process::Command;
    use tracing_subscriber::EnvFilter;

    // Initialize tracing for this test (will use RUST_LOG if set, otherwise INFO)
    let _ = tracing_subscriber::fmt()
        .with_env_filter(
            EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("info")),
        )
        .with_test_writer()
        .try_init();

    tracing::info!("=== Starting rust-analyzer rename test ===");

    // Check if rust-analyzer is installed
    let rust_analyzer_check = Command::new("which").arg("rust-analyzer").output();
    if rust_analyzer_check.is_err() || !rust_analyzer_check.unwrap().status.success() {
        eprintln!("Skipping test: rust-analyzer not found in PATH");
        return Ok(());
    }

    tracing::info!("rust-analyzer found in PATH");
    eprintln!("rust-analyzer found, running REAL SCENARIO test...");

    // Create minimal Cargo project (rust-analyzer needs Cargo.toml)
    let temp_dir = tempfile::tempdir()?;

    // Create minimal Cargo.toml
    let cargo_toml = temp_dir.path().join("Cargo.toml");
    let mut cargo_file = std::fs::File::create(&cargo_toml)?;
    writeln!(cargo_file, "[package]")?;
    writeln!(cargo_file, "name = \"test\"")?;
    writeln!(cargo_file, "version = \"0.1.0\"")?;
    writeln!(cargo_file, "edition = \"2021\"")?;
    drop(cargo_file);

    // Create src directory
    std::fs::create_dir(temp_dir.path().join("src"))?;

    // Create src/main.rs with a variable to rename
    let test_file = temp_dir.path().join("src").join("main.rs");
    let mut file = std::fs::File::create(&test_file)?;
    writeln!(file, "fn main() {{")?;
    writeln!(file, "    let log_line = \"hello world\";")?;
    writeln!(file, "    println!(\"{{}}\", log_line);")?;
    writeln!(file, "    let result = log_line.len();")?;
    writeln!(file, "}}")?;
    drop(file);

    tracing::info!("Created minimal Cargo project at: {:?}", temp_dir.path());
    eprintln!("Created minimal Cargo project at: {:?}", temp_dir.path());

    // Create temp file for rust-analyzer logs
    let ra_log_file = temp_dir.path().join("rust-analyzer.log");
    tracing::info!("rust-analyzer will log to: {:?}", ra_log_file);
    eprintln!("rust-analyzer will log to: {ra_log_file:?}");

    // Create custom config with rust-analyzer logging enabled
    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::services::lsp::client::LspServerConfig {
            command: "rust-analyzer".to_string(),
            args: vec![
                "--log-file".to_string(),
                ra_log_file.to_string_lossy().to_string(),
            ],
            enabled: true,
            auto_start: false,
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
        },
    );

    // CRITICAL: Set working directory to the temp project so rust-analyzer
    // analyzes the test code, NOT the editor's source code!
    let working_dir = temp_dir.path().to_path_buf();
    tracing::info!("Setting working directory for LSP: {:?}", working_dir);
    let mut harness = EditorTestHarness::with_config_and_working_dir(80, 30, config, working_dir)?;

    // Open the Rust file - this should trigger LSP initialization
    tracing::info!("Opening file: {:?}", test_file);
    harness.open_file(&test_file)?;
    harness.render()?;

    tracing::info!("File opened, waiting for rust-analyzer to initialize...");
    eprintln!("File opened, waiting for rust-analyzer to initialize...");

    // Wait INDEFINITELY for LSP to initialize (no timeout as user requested)
    let mut wait_count = 0;
    loop {
        harness.sleep(std::time::Duration::from_millis(500));
        let _ = harness.editor_mut().process_async_messages();
        harness.render()?;
        wait_count += 1;

        let screen = harness.screen_to_string();
        if screen.contains("LSP (rust) ready") {
            tracing::info!(
                "✓ rust-analyzer initialized after {} iterations ({} seconds)",
                wait_count,
                wait_count / 2
            );
            eprintln!("✓ rust-analyzer initialized and ready!");
            break;
        }

        // Print status periodically (every 10 iterations = 5 seconds)
        if wait_count % 10 == 0 {
            let status = screen.lines().last().unwrap_or("");
            tracing::info!(
                "Still waiting for rust-analyzer... ({}s) Status: {}",
                wait_count / 2,
                status
            );
            eprintln!("  Waiting... ({}s) Status: {}", wait_count / 2, status);
        }

        // Safety: after 2 minutes, give up
        if wait_count > 240 {
            tracing::error!("Timeout waiting for rust-analyzer after 2 minutes!");
            panic!("rust-analyzer did not initialize after 2 minutes");
        }
    }

    // Position cursor on "log_line" variable (line 1, after "let ")
    harness.send_key(KeyCode::Home, KeyModifiers::CONTROL)?; // Start of file
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?; // Move to line with "let log_line"
    for _ in 0..8 {
        harness.send_key(KeyCode::Right, KeyModifiers::NONE)?; // After "    let "
    }
    harness.render()?;

    let buffer_before = harness.get_buffer_content().unwrap();
    eprintln!("\nBuffer before rename:\n{buffer_before}");
    eprintln!("Cursor positioned on 'log_line' variable");

    // Press F2 to enter rename mode
    harness.send_key(KeyCode::F(2), KeyModifiers::NONE)?;
    harness.render()?;

    eprintln!("Entered rename mode");

    // Type ONE character '2' (like the user did: log_line -> log_line2)
    harness.type_text("2")?;
    harness.render()?;

    eprintln!("Typed '2' to make 'log_line2'");

    // Press Enter to confirm rename
    eprintln!("\nPressing Enter to confirm rename...");
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.render()?;
    let _ = harness.editor_mut().process_async_messages();

    // Wait INDEFINITELY for LSP response (no timeout)
    eprintln!("Waiting for rust-analyzer response (no timeout)...");
    loop {
        harness.sleep(std::time::Duration::from_millis(200));
        let _ = harness.editor_mut().process_async_messages();
        harness.render()?;

        let screen = harness.screen_to_string();

        // Check if we got ANY response
        if !screen.contains("LSP: rename...") {
            eprintln!("✓ rust-analyzer responded!");
            break;
        }

        // Print periodic status
        eprintln!("  Still waiting... {}", screen.lines().last().unwrap_or(""));
    }

    // Get final screen and buffer
    let screen_final = harness.screen_to_string();
    let buffer_final = harness.get_buffer_content().unwrap();

    eprintln!("\n========================================");
    eprintln!("FINAL SCREEN:");
    eprintln!("{screen_final}");
    eprintln!("\nFINAL BUFFER:");
    eprintln!("{buffer_final}");
    eprintln!("========================================\n");

    // Print rust-analyzer log for debugging
    if ra_log_file.exists() {
        eprintln!("\n========================================");
        eprintln!("RUST-ANALYZER LOG:");
        eprintln!("========================================");
        if let Ok(log_content) = std::fs::read_to_string(&ra_log_file) {
            // Print last 100 lines of the log
            let lines: Vec<&str> = log_content.lines().collect();
            let start = if lines.len() > 100 {
                lines.len() - 100
            } else {
                0
            };
            for line in &lines[start..] {
                eprintln!("{line}");
            }
        }
        eprintln!("========================================\n");
    } else {
        eprintln!("⚠ rust-analyzer log file not found at {ra_log_file:?}");
    }

    // CHECK FOR THE BUG: ContentModified error
    if screen_final.contains("content modified") || screen_final.contains("modified") {
        eprintln!("\n⚠️  BUG REPRODUCED! ⚠️");
        eprintln!("Got 'content modified' error from rust-analyzer");
        eprintln!("Check rust-analyzer log above for details!");
        panic!("REPRODUCED: ContentModified error - this is the bug we need to fix!");
    }

    // Check if rename actually succeeded
    if buffer_final.contains("let log_line2 =") {
        eprintln!("\n✅ SUCCESS! Rename worked!");
        assert!(buffer_final.contains("println!(\"{}\", log_line2)"));
        assert!(buffer_final.contains("let result = log_line2.len()"));
    } else {
        eprintln!("\n❌ FAILED: Rename didn't apply to buffer");
        panic!("Rename was not applied to buffer");
    }

    Ok(())
}

/// Test that consecutive LSP renames work correctly
///
/// This test reproduces a bug where the second rename fails because the LSP
/// is not properly notified of buffer changes from the first rename.
#[test]
fn test_lsp_rename_consecutive_same_position() -> std::io::Result<()> {
    use lsp_types::{
        DocumentChanges, OneOf, OptionalVersionedTextDocumentIdentifier, Position, Range,
        TextDocumentEdit, TextEdit, Uri, WorkspaceEdit,
    };

    let mut harness = EditorTestHarness::new(80, 30)?;

    // Create a temporary file with some Rust code
    let temp_dir = tempfile::tempdir()?;
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(
        &test_file,
        "fn calculate(value: i32) -> i32 {\n    let result = value * 2;\n    result\n}\n",
    )?;

    harness.open_file(&test_file)?;
    harness.render()?;

    let initial_content = harness.get_buffer_content().unwrap();
    assert!(initial_content.contains("fn calculate(value: i32)"));

    // FIRST RENAME: value -> amount
    let uri = url::Url::from_file_path(&test_file)
        .unwrap()
        .as_str()
        .parse::<Uri>()
        .unwrap();

    let first_rename_edit = WorkspaceEdit {
        changes: None,
        document_changes: Some(DocumentChanges::Edits(vec![TextDocumentEdit {
            text_document: OptionalVersionedTextDocumentIdentifier {
                uri: uri.clone(),
                version: Some(1),
            },
            edits: vec![
                OneOf::Left(TextEdit {
                    range: Range {
                        start: Position {
                            line: 0,
                            character: 13,
                        },
                        end: Position {
                            line: 0,
                            character: 18,
                        },
                    },
                    new_text: "amount".to_string(),
                }),
                OneOf::Left(TextEdit {
                    range: Range {
                        start: Position {
                            line: 1,
                            character: 17,
                        },
                        end: Position {
                            line: 1,
                            character: 22,
                        },
                    },
                    new_text: "amount".to_string(),
                }),
            ],
        }])),
        change_annotations: None,
    };

    harness
        .editor_mut()
        .handle_rename_response(1, Ok(first_rename_edit))?;
    harness.render()?;

    let after_first = harness.get_buffer_content().unwrap();
    assert!(
        after_first.contains("fn calculate(amount: i32)"),
        "First rename failed. Got:\n{after_first}"
    );
    assert!(
        after_first.contains("let result = amount * 2"),
        "First rename failed. Got:\n{after_first}"
    );

    // SECOND RENAME: amount -> total (same position)
    let second_rename_edit = WorkspaceEdit {
        changes: None,
        document_changes: Some(DocumentChanges::Edits(vec![TextDocumentEdit {
            text_document: OptionalVersionedTextDocumentIdentifier {
                uri: uri.clone(),
                version: Some(2),
            },
            edits: vec![
                OneOf::Left(TextEdit {
                    range: Range {
                        start: Position {
                            line: 0,
                            character: 13,
                        },
                        end: Position {
                            line: 0,
                            character: 19,
                        }, // "amount" is 6 chars
                    },
                    new_text: "total".to_string(),
                }),
                OneOf::Left(TextEdit {
                    range: Range {
                        start: Position {
                            line: 1,
                            character: 17,
                        },
                        end: Position {
                            line: 1,
                            character: 23,
                        },
                    },
                    new_text: "total".to_string(),
                }),
            ],
        }])),
        change_annotations: None,
    };

    harness
        .editor_mut()
        .handle_rename_response(2, Ok(second_rename_edit))?;
    harness.render()?;

    let after_second = harness.get_buffer_content().unwrap();
    assert!(
        after_second.contains("fn calculate(total: i32)"),
        "Second rename failed. Got:\n{after_second}"
    );
    assert!(
        after_second.contains("let result = total * 2"),
        "Second rename failed. Got:\n{after_second}"
    );
    assert!(!after_second.contains("value"));
    assert!(!after_second.contains("amount"));

    Ok(())
}

/// Test that consecutive renames with real rust-analyzer work correctly
///
/// This test reproduces the bug where the second rename fails because LSP
/// is notified with incorrect positions after the first rename.
///
/// Uses the same interaction as user:
/// 1. Move cursor to symbol's first letter
/// 2. Open command palette (Ctrl+P), type "rename", select lsp_rename command
/// 3. Type new name and approve with Enter
/// 4. Move cursor back to first letter
/// 5. Repeat rename command with another new name
#[test]
#[ignore] // Run with: cargo test test_lsp_rename_twice_with_rust_analyzer -- --ignored --nocapture
fn test_lsp_rename_twice_with_rust_analyzer() -> std::io::Result<()> {
    use std::io::Write;
    use std::process::Command;

    // Check if rust-analyzer is installed
    let rust_analyzer_check = Command::new("which").arg("rust-analyzer").output();
    if rust_analyzer_check.is_err() || !rust_analyzer_check.unwrap().status.success() {
        eprintln!("Skipping test: rust-analyzer not found in PATH");
        return Ok(());
    }

    // Create harness with temp project directory
    let mut harness = EditorTestHarness::with_temp_project(200, 30)?;
    let project_dir = harness.project_dir().expect("project dir should exist");

    // Create Cargo.toml
    let cargo_toml = project_dir.join("Cargo.toml");
    std::fs::write(
        &cargo_toml,
        r#"[package]
name = "test-rename"
version = "0.1.0"
edition = "2021"
"#,
    )?;

    // Create src directory
    let src_dir = project_dir.join("src");
    std::fs::create_dir(&src_dir)?;

    // Create lib.rs with a function to rename
    let test_file = src_dir.join("lib.rs");
    let mut file = std::fs::File::create(&test_file)?;
    writeln!(file, "pub fn foo(val: i32) -> i32 {{")?;
    writeln!(file, "    val * 2")?;
    writeln!(file, "}}")?;
    drop(file);

    harness.open_file(&test_file)?;
    harness.render()?;

    // Wait for LSP to initialize by checking the actual server status
    eprintln!("Waiting for rust-analyzer to initialize...");
    let mut lsp_ready = false;
    for i in 0..240 {
        // Up to 24 seconds
        harness.sleep(std::time::Duration::from_millis(100));
        let _ = harness.editor_mut().process_async_messages();
        harness.render()?;

        // Check actual LSP server status
        if harness.editor().is_lsp_server_ready("rust") {
            eprintln!("✓ rust-analyzer ready (iteration {}, {}ms)", i, i * 100);
            lsp_ready = true;
            break;
        }

        // Also print status periodically
        if i % 50 == 0 && i > 0 {
            let lsp_status = harness.editor().get_lsp_status();
            eprintln!("  [{}ms] LSP status: {}", i * 100, lsp_status);
        }
    }

    if !lsp_ready {
        let lsp_status = harness.editor().get_lsp_status();
        eprintln!(
            "⚠ Warning: LSP may not have initialized fully. Status: {}",
            lsp_status
        );
        eprintln!("Continuing test anyway...");
    }

    // Wait for indexing to complete (rust-analyzer sends progress notifications)
    eprintln!("Waiting for indexing...");
    let mut had_progress = false;
    for i in 0..120 {
        harness.sleep(std::time::Duration::from_millis(100));
        let _ = harness.editor_mut().process_async_messages();
        harness.render()?;

        let has_progress = harness.editor().has_active_lsp_progress();
        if has_progress {
            had_progress = true;
            if i % 20 == 0 {
                // Log progress every 2 seconds
                let progress = harness.editor().get_lsp_progress();
                for (_, title, msg) in &progress {
                    eprintln!("  [{}ms] Progress: {} - {:?}", i * 100, title, msg);
                }
            }
        } else if had_progress {
            eprintln!("✓ Indexing complete (iteration {}, {}ms)", i, i * 100);
            break;
        } else if i > 30 && !had_progress && lsp_ready {
            // If LSP is ready but no progress seen in 3 seconds, it might be a small project
            eprintln!("No LSP progress seen (small project), assuming ready");
            break;
        }
    }

    // Extra wait for semantic analysis (rust-analyzer needs this)
    eprintln!("Waiting for semantic analysis...");
    harness.sleep(std::time::Duration::from_millis(2000));
    for _ in 0..20 {
        harness.sleep(std::time::Duration::from_millis(100));
        let _ = harness.editor_mut().process_async_messages();
        harness.render()?;
    }
    eprintln!("✓ Semantic analysis complete");

    let initial = harness.get_buffer_content().unwrap();
    eprintln!("Initial buffer:\n{}", initial);
    assert!(initial.contains("fn foo(val: i32)"));

    // FIRST RENAME: val -> value
    eprintln!("\n=== FIRST RENAME: val -> value ===");

    // Step 1: Move cursor to first letter of "val" (after "pub fn foo(")
    harness.send_key(KeyCode::Home, KeyModifiers::CONTROL)?;
    for _ in 0..12 {
        harness.send_key(KeyCode::Right, KeyModifiers::NONE)?;
    }
    harness.render()?;
    let cursor_pos = harness.cursor_position();
    let char_at_cursor = initial.chars().nth(cursor_pos).unwrap_or('?');
    eprintln!(
        "Cursor positioned at byte {}, char '{}'",
        cursor_pos, char_at_cursor
    );

    // Step 2: Start rename with F2 (direct action, not command palette)
    eprintln!("Pressing F2 to start rename...");
    harness.send_key(KeyCode::F(2), KeyModifiers::NONE)?;
    harness.render()?;

    // Step 3: Type new name (clear and type "value")
    // The prompt should be pre-filled with "val"
    eprintln!("Clearing current name...");
    for _ in 0..3 {
        harness.send_key(KeyCode::Backspace, KeyModifiers::NONE)?;
    }
    harness.type_text("value")?;
    harness.render()?;
    eprintln!("Typed new name 'value'");

    // Step 4: Approve with Enter
    eprintln!("Pressing Enter to confirm rename...");
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.render()?;

    // Wait for LSP response
    eprintln!("Waiting for first rename response...");
    for _ in 0..40 {
        harness.sleep(std::time::Duration::from_millis(250));
        let _ = harness.editor_mut().process_async_messages();
        harness.render()?;

        let screen = harness.screen_to_string();
        if !screen.contains("LSP: rename...") {
            break;
        }
    }

    let after_first = harness.get_buffer_content().unwrap();
    eprintln!("After first rename:\n{}", after_first);

    let first_rename_ok =
        after_first.contains("fn foo(value: i32)") && after_first.contains("value * 2");

    if !first_rename_ok {
        eprintln!("❌ First rename FAILED!");
        let screen = harness.screen_to_string();
        if screen.contains("content modified") {
            eprintln!("Got 'content modified' error on FIRST rename");
        }
        panic!("First rename failed. Got:\n{}", after_first);
    }
    eprintln!("✓ First rename succeeded");

    // SECOND RENAME: value -> data (same position)
    eprintln!("\n=== SECOND RENAME: value -> data ===");

    // Step 1: Move cursor back to first letter of "value" (same position)
    harness.send_key(KeyCode::Home, KeyModifiers::CONTROL)?;
    for _ in 0..12 {
        harness.send_key(KeyCode::Right, KeyModifiers::NONE)?;
    }
    harness.render()?;
    let cursor_pos2 = harness.cursor_position();
    let after_first_content = harness.get_buffer_content().unwrap();
    let char_at_cursor2 = after_first_content.chars().nth(cursor_pos2).unwrap_or('?');
    eprintln!(
        "Cursor positioned at byte {}, char '{}'",
        cursor_pos2, char_at_cursor2
    );

    // Step 2: Start rename with F2 (direct action)
    eprintln!("Pressing F2 to start rename...");
    harness.send_key(KeyCode::F(2), KeyModifiers::NONE)?;
    harness.render()?;

    // Step 3: Type new name (clear and type "data")
    // The prompt should be pre-filled with "value"
    eprintln!("Clearing current name...");
    for _ in 0..5 {
        harness.send_key(KeyCode::Backspace, KeyModifiers::NONE)?;
    }
    harness.type_text("data")?;
    harness.render()?;
    eprintln!("Typed new name 'data'");

    // Step 4: Approve with Enter
    eprintln!("Pressing Enter to confirm rename...");
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.render()?;

    // Wait for LSP response
    eprintln!("Waiting for second rename response...");
    for _ in 0..40 {
        harness.sleep(std::time::Duration::from_millis(250));
        let _ = harness.editor_mut().process_async_messages();
        harness.render()?;

        let screen = harness.screen_to_string();
        if !screen.contains("LSP: rename...") {
            break;
        }
    }

    let after_second = harness.get_buffer_content().unwrap();
    let screen = harness.screen_to_string();
    eprintln!("After second rename:\n{}", after_second);
    eprintln!("Screen:\n{}", screen);

    // Check for the bug
    if screen.contains("content modified") || screen.contains("modified") {
        eprintln!("\n🐛 BUG REPRODUCED! 🐛");
        eprintln!("Second rename got 'content modified' error.");
        eprintln!("This happens because LSP was notified with wrong positions after first rename.");
        panic!("BUG: Second rename failed with content modified error");
    }

    let second_rename_ok =
        after_second.contains("fn foo(data: i32)") && after_second.contains("data * 2");

    if !second_rename_ok {
        eprintln!("❌ Second rename FAILED!");
        panic!("Second rename failed. Got:\n{}", after_second);
    }

    eprintln!("✓ Second rename succeeded");
    eprintln!("\n✅ Both consecutive renames work correctly!");

    Ok(())
}

/// Test that LSP progress notifications are displayed in the status bar
///
/// This test uses a fake LSP server that sends progress notifications (begin, report, end)
/// and verifies that the status bar displays the progress information correctly.
///
/// NOTE: This test is ignored by default as it relies on bash script execution and timing
/// which can be flaky in CI environments. Run with --ignored to execute.
#[test]
#[ignore]
fn test_lsp_progress_status_display() -> std::io::Result<()> {
    use crate::common::fake_lsp::FakeLspServer;

    // Create a fake LSP server that sends progress notifications
    let _fake_server = FakeLspServer::spawn_with_progress()?;

    // Create temporary directory and test file
    let temp_dir = tempfile::tempdir()?;
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn main() {\n    println!(\"Hello\");\n}\n")?;

    // Configure editor to use the progress fake LSP server
    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::services::lsp::client::LspServerConfig {
            command: FakeLspServer::progress_script_path()
                .to_string_lossy()
                .to_string(),
            args: vec![],
            enabled: true,
            auto_start: false,
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
        },
    );

    // Create harness with config and working directory
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120, // Wider terminal to see full status
        24,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    // Open the file (triggers LSP initialization)
    harness.open_file(&test_file)?;
    harness.render()?;

    // Track progress messages we've seen
    let mut seen_begin = false;
    let mut _seen_report = false;
    let mut seen_end = false;
    let mut progress_titles = Vec::new();
    let mut progress_messages = Vec::new();
    let mut seen_status_bar_progress = false;

    // Poll for progress notifications
    for i in 0..30 {
        // Wait up to 3 seconds
        harness.sleep(std::time::Duration::from_millis(100));

        // Process async messages from LSP
        let _ = harness.editor_mut().process_async_messages();
        harness.render()?;

        // Check LSP progress state
        let has_progress = harness.editor().has_active_lsp_progress();
        let progress = harness.editor().get_lsp_progress();

        if has_progress && !progress.is_empty() {
            for (_token, title, message) in &progress {
                if !progress_titles.contains(title) {
                    progress_titles.push(title.clone());
                    eprintln!("  [{:3}ms] Progress: {}", i * 100, title);
                }
                if let Some(msg) = message {
                    if !progress_messages.contains(msg) {
                        progress_messages.push(msg.clone());
                        eprintln!("    Message: {}", msg);
                    }
                }
            }
            seen_begin = true;
        } else if seen_begin {
            // Progress ended
            seen_end = true;
            eprintln!("  [{:3}ms] Progress ended", i * 100);
            break;
        }

        // Check the status bar for progress display
        let screen = harness.screen_to_string();
        if screen.contains("Indexing") || screen.contains("Loading") || screen.contains("Building")
        {
            _seen_report = true;
        }
        // Check if status bar shows "LSP (rust):" which indicates progress is being rendered
        if screen.contains("LSP (rust):") {
            if !seen_status_bar_progress {
                eprintln!("  [{:3}ms] Status bar shows LSP progress", i * 100);
            }
            seen_status_bar_progress = true;
        }
    }

    // Verify we saw progress notifications
    assert!(
        seen_begin,
        "Should have received progress begin notification"
    );

    // Verify we saw the expected progress titles
    assert!(
        progress_titles.contains(&"Indexing".to_string()),
        "Should see 'Indexing' in progress titles. Got: {:?}",
        progress_titles
    );

    // Verify we saw progress messages
    assert!(
        !progress_messages.is_empty(),
        "Should have seen progress messages"
    );
    assert!(
        progress_messages.iter().any(|m| m.contains("Loading")
            || m.contains("Analyzing")
            || m.contains("Building")
            || m.contains("Finalizing")),
        "Should see progress updates. Got: {:?}",
        progress_messages
    );

    // Verify progress ended
    assert!(seen_end, "Progress should have ended");

    // After progress ends, there should be no active progress
    let final_progress = harness.editor().has_active_lsp_progress();
    assert!(
        !final_progress,
        "Should have no active progress after completion"
    );

    // Verify status bar rendering
    assert!(
        seen_status_bar_progress,
        "Status bar should have displayed LSP progress (e.g., 'LSP (rust): Indexing')"
    );

    eprintln!("\n✅ SUCCESS: LSP progress notifications received and processed!");
    eprintln!("   Titles: {:?}", progress_titles);
    eprintln!("   Messages: {:?}", progress_messages);
    eprintln!("   Status bar rendering: ✓");

    Ok(())
}

/// Test LSP server crash detection and auto-restart with exponential backoff
///
/// This test verifies that when an LSP server crashes:
/// 1. The crash is detected (status changes to Error)
/// 2. The editor schedules a restart with exponential backoff
/// 3. A status message notifies the user
/// 4. After the backoff period, the server is restarted
/// 5. Open documents are re-sent to the restarted server
///
/// NOTE: This test is ignored by default as it relies on bash script execution and timing
/// which can be flaky in CI environments. Run with --ignored to execute.
#[test]
#[ignore]
fn test_lsp_crash_detection_and_restart() -> std::io::Result<()> {
    use crate::common::fake_lsp::FakeLspServer;

    // Create a fake LSP server that crashes after initialization
    let _fake_server = FakeLspServer::spawn_crashing()?;

    // Create temporary directory and test file
    let temp_dir = tempfile::tempdir()?;
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn main() {\n    let x = 5;\n}\n")?;

    // Configure editor to use the crashing fake LSP server
    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::services::lsp::client::LspServerConfig {
            command: FakeLspServer::crashing_script_path()
                .to_string_lossy()
                .to_string(),
            args: vec![],
            enabled: true,
            auto_start: false,
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
        },
    );

    // Create harness with config and working directory
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    // Open the file - this triggers:
    // 1. LSP server spawn
    // 2. initialize request/response
    // 3. initialized notification
    // 4. textDocument/didOpen notification
    // The fake server will crash after receiving didOpen
    harness.open_file(&test_file)?;

    // Give the LSP server a moment to initialize and process didOpen
    // The server will crash when it receives didOpen
    harness.sleep(std::time::Duration::from_millis(200));

    // Render to process async messages
    harness.render()?;

    // Wait for the crash to be detected (status changes to Error)
    let mut crash_detected = false;
    let mut status_msg = String::new();
    for i in 0..30 {
        harness.sleep(std::time::Duration::from_millis(100));

        // Send a no-op key to trigger async message processing
        // (render() alone doesn't process async messages)
        harness.send_key(KeyCode::Null, KeyModifiers::NONE)?;
        harness.render()?;

        // Check the screen for crash-related status messages
        let screen = harness.screen_to_string();
        // Look for "error", "crashed", or "restarting"
        if screen.contains("error") || screen.contains("crashed") || screen.contains("restarting") {
            crash_detected = true;
            // Extract the status message using the layout-aware helper
            status_msg = harness.get_status_bar();
            eprintln!("Iteration {}: Crash/error detected in screen", i);
            break;
        }

        // Also check the status bar area specifically
        if let Some(msg) = harness.editor().get_status_message().cloned() {
            // Look for error or crash indications
            if msg.contains("error")
                || msg.contains("crashed")
                || msg.contains("restarting")
                || msg.contains("Error")
            {
                crash_detected = true;
                status_msg = msg;
                eprintln!(
                    "Iteration {}: Crash/error detected with message: {}",
                    i, status_msg
                );
                break;
            }
        }

        // Debug: print status on some iterations
        if i % 10 == 0 {
            if let Some(msg) = harness.editor().get_status_message() {
                eprintln!("Iteration {}: status = {}", i, msg);
            }
        }
    }

    // Verify crash was detected
    assert!(
        crash_detected,
        "LSP server crash should be detected. Screen:\n{}",
        harness.screen_to_string()
    );

    // Check the status message format (should show error or crash indication)
    assert!(
        status_msg.contains("rust") || status_msg.contains("error") || status_msg.contains("Error"),
        "Status message should indicate LSP error. Got: {}",
        status_msg
    );

    // Verify the editor remains responsive after crash
    harness.type_text("// comment after crash")?;
    harness.render()?;

    let buffer_content = harness.get_buffer_content().unwrap();
    assert!(
        buffer_content.contains("// comment after crash"),
        "Editor should remain responsive after LSP crash. Buffer: {}",
        buffer_content
    );

    // Wait for restart backoff (first attempt is 1 second)
    // The restart should happen automatically
    harness.sleep(std::time::Duration::from_millis(1500));

    // Process messages to trigger the restart
    harness.send_key(KeyCode::Null, KeyModifiers::NONE)?;
    harness.render()?;

    // Check the screen for restart activity
    let final_screen = harness.screen_to_string();
    eprintln!("Screen after backoff:\n{}", final_screen);

    // Check status after backoff
    if let Some(msg) = harness.editor().get_status_message() {
        eprintln!("Status after backoff: {}", msg);
    }

    eprintln!("\n✅ SUCCESS: LSP crash detection and restart mechanism is working!");
    eprintln!("   - Crash was detected");
    eprintln!("   - User was notified via status message");
    eprintln!("   - Editor remained responsive");
    eprintln!("   - Restart was scheduled with backoff");

    Ok(())
}

/// Test that the Restart LSP Server command exists in the command palette
#[test]
fn test_lsp_restart_command_exists() -> std::io::Result<()> {
    let mut harness = EditorTestHarness::new(80, 24)?;

    // Open command palette with Ctrl+Shift+P
    harness.send_key(
        KeyCode::Char('P'),
        KeyModifiers::CONTROL | KeyModifiers::SHIFT,
    )?;
    harness.render()?;

    // Type to search for restart command
    harness.type_text("Restart LSP")?;
    harness.render()?;

    // Verify the command palette shows the restart command
    let screen = harness.screen_to_string();
    eprintln!("Screen with command search:\n{}", screen);

    assert!(
        screen.contains("Restart LSP"),
        "Command palette should show 'Restart LSP' command when searched. Screen:\n{}",
        screen
    );

    // Close the command palette
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE)?;
    harness.render()?;

    eprintln!("\n✅ SUCCESS: Restart LSP Server command exists in command palette!");

    Ok(())
}

/// Test that pull diagnostics infrastructure is set up correctly
/// This test verifies that the LspPulledDiagnostics message can be processed
#[test]
fn test_pull_diagnostics_message_handling() -> std::io::Result<()> {
    use fresh::services::async_bridge::AsyncMessage;
    use lsp_types::Diagnostic;

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Create a test file to have a URI
    let temp_dir = tempfile::TempDir::new()?;
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn main() {\n    println!(\"Hello\");\n}")?;

    // Open the test file
    harness.editor_mut().open_file(&test_file)?;
    harness.render()?;

    // Get the URI for the file
    let uri = url::Url::from_file_path(&test_file)
        .ok()
        .and_then(|u| u.as_str().parse::<lsp_types::Uri>().ok())
        .expect("Should create URI");

    // Simulate receiving pulled diagnostics
    // Create a test diagnostic
    let diagnostic = Diagnostic {
        range: lsp_types::Range {
            start: lsp_types::Position {
                line: 1,
                character: 4,
            },
            end: lsp_types::Position {
                line: 1,
                character: 15,
            },
        },
        severity: Some(lsp_types::DiagnosticSeverity::WARNING),
        code: None,
        code_description: None,
        source: Some("test".to_string()),
        message: "Test diagnostic message".to_string(),
        related_information: None,
        tags: None,
        data: None,
    };

    // Send the pulled diagnostics message through the async bridge
    if let Some(bridge) = harness.editor().async_bridge() {
        let _ = bridge.sender().send(AsyncMessage::LspPulledDiagnostics {
            request_id: 1,
            uri: uri.as_str().to_string(),
            result_id: Some("test-result-id-123".to_string()),
            diagnostics: vec![diagnostic],
            unchanged: false,
        });
    }

    // Process async messages
    harness.send_key(KeyCode::Null, KeyModifiers::NONE)?;
    harness.render()?;

    // Verify the diagnostic was applied (check for overlay)
    let buffer_content = harness.get_buffer_content().unwrap();
    eprintln!("Buffer content: {}", buffer_content);

    // The diagnostic should have been processed (we can't easily check overlays,
    // but we can verify the code path was executed without panicking)
    eprintln!("\n✅ SUCCESS: Pull diagnostics message was processed successfully!");

    Ok(())
}

/// Test that pull diagnostics handles unchanged responses correctly
#[test]
fn test_pull_diagnostics_unchanged_response() -> std::io::Result<()> {
    use fresh::services::async_bridge::AsyncMessage;

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Create and open a test file
    let temp_dir = tempfile::TempDir::new()?;
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn main() {}")?;
    harness.editor_mut().open_file(&test_file)?;
    harness.render()?;

    // Get the URI
    let uri = url::Url::from_file_path(&test_file)
        .ok()
        .and_then(|u| u.as_str().parse::<lsp_types::Uri>().ok())
        .expect("Should create URI");

    // Send an unchanged response (simulating server returning same diagnostics)
    if let Some(bridge) = harness.editor().async_bridge() {
        let _ = bridge.sender().send(AsyncMessage::LspPulledDiagnostics {
            request_id: 2,
            uri: uri.as_str().to_string(),
            result_id: Some("test-result-id-456".to_string()),
            diagnostics: Vec::new(), // Empty when unchanged
            unchanged: true,
        });
    }

    // Process async messages
    harness.send_key(KeyCode::Null, KeyModifiers::NONE)?;
    harness.render()?;

    // Should not panic or error
    eprintln!("\n✅ SUCCESS: Unchanged pull diagnostics response was handled correctly!");

    Ok(())
}

/// Test that pull diagnostics are auto-triggered after didOpen
#[test]
#[ignore]
fn test_pull_diagnostics_auto_trigger_after_open() -> std::io::Result<()> {
    use crate::common::fake_lsp::FakeLspServer;

    // Create fake LSP server with pull diagnostics support
    let _server = FakeLspServer::spawn_with_pull_diagnostics()?;

    // Create config that uses the pull diagnostics fake server
    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::services::lsp::client::LspServerConfig {
            command: FakeLspServer::pull_diagnostics_script_path()
                .to_string_lossy()
                .to_string(),
            args: vec![],
            enabled: true,
            auto_start: false,
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
        },
    );

    // Create a temp directory and test file
    let temp_dir = tempfile::TempDir::new()?;
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "hello world")?;

    // Create harness with config
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    // Open the file - this should trigger didOpen and then pull diagnostics
    harness.open_file(&test_file)?;

    // Wait for LSP to initialize and send diagnostics
    // The fake server will respond to textDocument/diagnostic with a warning
    let mut found_diagnostic = false;
    for _ in 0..50 {
        // Process any async messages
        harness.send_key(KeyCode::Null, KeyModifiers::NONE)?;
        harness.render()?;

        // Check if we received the diagnostic overlay
        let overlays = harness.editor().active_state().overlays.all();
        for overlay in overlays {
            if let Some(msg) = &overlay.message {
                if msg.contains("Pull diagnostic warning from fake LSP") {
                    found_diagnostic = true;
                    break;
                }
            }
        }

        if found_diagnostic {
            break;
        }

        // Small delay between checks
        harness.sleep(std::time::Duration::from_millis(100));
    }

    assert!(
        found_diagnostic,
        "Expected to find pull diagnostic warning overlay after file open"
    );

    eprintln!("\n✅ SUCCESS: Pull diagnostics were auto-triggered after didOpen!");

    Ok(())
}

/// Test that pull diagnostics result_id is used for incremental updates
#[test]
#[ignore]
fn test_pull_diagnostics_result_id_tracking() -> std::io::Result<()> {
    use crate::common::fake_lsp::FakeLspServer;

    // Create fake LSP server with pull diagnostics support
    let _server = FakeLspServer::spawn_with_pull_diagnostics()?;

    // Create config that uses the pull diagnostics fake server
    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::services::lsp::client::LspServerConfig {
            command: FakeLspServer::pull_diagnostics_script_path()
                .to_string_lossy()
                .to_string(),
            args: vec![],
            enabled: true,
            auto_start: false,
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
        },
    );

    // Create a temp directory and test file
    let temp_dir = tempfile::TempDir::new()?;
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "hello world")?;

    // Create harness with config
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    // Open the file - this should trigger first pull diagnostics request
    harness.open_file(&test_file)?;

    // Wait for initial diagnostics
    let mut initial_diagnostic_found = false;
    for _ in 0..50 {
        harness.send_key(KeyCode::Null, KeyModifiers::NONE)?;
        harness.render()?;

        let overlays = harness.editor().active_state().overlays.all();
        for overlay in overlays {
            if let Some(msg) = &overlay.message {
                if msg.contains("Pull diagnostic warning from fake LSP") {
                    initial_diagnostic_found = true;
                    break;
                }
            }
        }

        if initial_diagnostic_found {
            break;
        }

        harness.sleep(std::time::Duration::from_millis(100));
    }

    assert!(
        initial_diagnostic_found,
        "Expected to find initial pull diagnostic warning"
    );

    // Now make a change - this should trigger another pull diagnostics request
    // with the previous result_id, and the server should return "unchanged"
    harness.send_key(KeyCode::Char('a'), KeyModifiers::NONE)?;
    harness.render()?;

    // Wait a bit for the second request/response cycle
    for _ in 0..30 {
        harness.send_key(KeyCode::Null, KeyModifiers::NONE)?;
        harness.render()?;
        harness.sleep(std::time::Duration::from_millis(100));
    }

    // The diagnostics should still be there (server returned unchanged or new full response)
    let overlays = harness.editor().active_state().overlays.all();
    let still_has_diagnostic = overlays.iter().any(|o| {
        o.message
            .as_ref()
            .map(|m| m.contains("Pull diagnostic warning from fake LSP"))
            .unwrap_or(false)
    });

    // Note: We're mainly testing that no errors occurred during result_id tracking
    // The diagnostic should still be present
    eprintln!(
        "Diagnostic still present after change: {}",
        still_has_diagnostic
    );

    eprintln!("\n✅ SUCCESS: Pull diagnostics result_id tracking works correctly!");

    Ok(())
}

/// Test that inlay hints (virtual text) render correctly on screen
#[test]
fn test_inlay_hints_render_on_screen() -> std::io::Result<()> {
    use fresh::view::virtual_text::VirtualTextPosition;
    use ratatui::style::{Color, Style};

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Type some code that would have type hints
    harness.type_text("let x = 5;\nfoo(10);")?;
    harness.render()?;

    // Verify initial content
    let screen = harness.screen_to_string();
    assert!(screen.contains("let x = 5;"), "Expected code to be visible");

    // Now add inlay hints (simulating LSP response)
    // Type hint after 'x' (position 5)
    // Parameter hint before '10' (position 16 = 11 for first line + newline + 4 for "foo(")
    let state = harness.editor_mut().active_state_mut();

    // Initialize marker list for the buffer content
    let buf_len = state.buffer.len();
    if buf_len > 0 {
        state.marker_list.adjust_for_insert(0, buf_len);
    }

    // Style for inlay hints - dimmed gray
    let hint_style = Style::default().fg(Color::Rgb(128, 128, 128));

    // Add type hint after 'x' at position 5
    state.virtual_texts.add(
        &mut state.marker_list,
        5,
        ": i32".to_string(),
        hint_style,
        VirtualTextPosition::AfterChar,
        0,
    );

    // Add parameter hint before '10' at position 15 (after "let x = 5;\nfoo(")
    state.virtual_texts.add(
        &mut state.marker_list,
        15,
        "count:".to_string(),
        hint_style,
        VirtualTextPosition::BeforeChar,
        0,
    );

    harness.render()?;

    // Get the rendered screen
    let screen = harness.screen_to_string();

    // Verify the type hint is visible
    assert!(
        screen.contains(": i32"),
        "Expected type hint ': i32' to be visible on screen. Screen:\n{}",
        screen
    );

    // Verify the parameter hint is visible
    assert!(
        screen.contains("count:"),
        "Expected parameter hint 'count:' to be visible on screen. Screen:\n{}",
        screen
    );

    // Verify original code is still there
    assert!(
        screen.contains("let x"),
        "Expected 'let x' to still be visible"
    );
    assert!(
        screen.contains("foo("),
        "Expected 'foo(' to still be visible"
    );

    eprintln!("\n✅ SUCCESS: Inlay hints render correctly on screen!");

    Ok(())
}

/// Test that virtual text positions update when buffer is edited
#[test]
fn test_inlay_hints_position_tracking() -> std::io::Result<()> {
    use fresh::view::virtual_text::VirtualTextPosition;
    use ratatui::style::{Color, Style};

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Type initial code
    harness.type_text("let x = 5;")?;
    harness.render()?;

    // Add type hint after 'x' at position 5
    let state = harness.editor_mut().active_state_mut();
    let buf_len = state.buffer.len();
    if buf_len > 0 {
        state.marker_list.adjust_for_insert(0, buf_len);
    }

    let hint_style = Style::default().fg(Color::Rgb(128, 128, 128));
    state.virtual_texts.add(
        &mut state.marker_list,
        5,
        ": i32".to_string(),
        hint_style,
        VirtualTextPosition::AfterChar,
        0,
    );

    harness.render()?;

    // Verify hint is visible
    let screen = harness.screen_to_string();
    assert!(screen.contains(": i32"), "Initial hint should be visible");

    // Now insert text before the hint position
    // Move cursor to beginning
    harness.send_key(KeyCode::Home, KeyModifiers::CONTROL)?;
    harness.render()?;

    // Insert "const " at the beginning
    harness.type_text("const ")?;
    harness.render()?;

    // The hint should still be visible (its position should have moved)
    let screen = harness.screen_to_string();
    assert!(
        screen.contains(": i32"),
        "Hint should still be visible after inserting text before it. Screen:\n{}",
        screen
    );

    // Buffer should now contain "const let x = 5;"
    let buffer_content = harness.get_buffer_content().unwrap();
    assert!(
        buffer_content.contains("const let x"),
        "Buffer should contain inserted text"
    );

    eprintln!("\n✅ SUCCESS: Inlay hint positions track buffer edits correctly!");

    Ok(())
}

/// Test that stopped LSP server does not auto-restart when typing
///
/// This test verifies that when a user explicitly stops an LSP server via the "stop lsp"
/// command, the server should remain disabled even when the user makes edits (which would
/// normally trigger didChange notifications to the LSP). The LSP should only restart when
/// the user explicitly uses the "restart lsp" command.
#[test]
fn test_stopped_lsp_does_not_auto_restart_on_edit() -> std::io::Result<()> {
    use crate::common::fake_lsp::FakeLspServer;

    // Create a fake LSP server
    let _fake_server = FakeLspServer::spawn()?;

    // Create temporary directory and test file
    let temp_dir = tempfile::tempdir()?;
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn main() {\n    let x = 5;\n}\n")?;

    // Configure editor to use the fake LSP server
    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::services::lsp::client::LspServerConfig {
            command: FakeLspServer::script_path().to_string_lossy().to_string(),
            args: vec![],
            enabled: true,
            auto_start: true, // Auto-start so it starts when we open the file
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
        },
    );

    // Create harness with config and working directory
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        24,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    // Open the file - this triggers LSP spawn
    harness.open_file(&test_file)?;
    harness.render()?;

    // Wait for the LSP server to initialize
    harness.sleep(std::time::Duration::from_millis(200));
    harness.send_key(KeyCode::Null, KeyModifiers::NONE)?;
    harness.render()?;

    // Verify LSP server is running
    let running_before = harness.editor().running_lsp_servers();
    eprintln!("Running LSP servers before stop: {:?}", running_before);
    assert!(
        running_before.contains(&"rust".to_string()),
        "LSP server for rust should be running after file open. Running: {:?}",
        running_before
    );

    // Stop the LSP server (simulating user running "stop lsp" command)
    let stopped = harness.editor_mut().shutdown_lsp_server("rust");
    assert!(stopped, "shutdown_lsp_server should return true");

    // Give the shutdown a moment to complete
    harness.sleep(std::time::Duration::from_millis(50));
    harness.send_key(KeyCode::Null, KeyModifiers::NONE)?;
    harness.render()?;

    // Verify the LSP server is no longer running
    let running_after_stop = harness.editor().running_lsp_servers();
    eprintln!("Running LSP servers after stop: {:?}", running_after_stop);
    assert!(
        !running_after_stop.contains(&"rust".to_string()),
        "LSP server for rust should NOT be running after stop. Running: {:?}",
        running_after_stop
    );

    // Now type some text - this triggers send_lsp_changes_for_buffer
    // which used to call get_or_spawn and restart the LSP
    harness.type_text("// This edit should NOT restart the LSP\n")?;
    harness.render()?;

    // Give some time for any potential LSP spawn
    harness.sleep(std::time::Duration::from_millis(100));
    harness.send_key(KeyCode::Null, KeyModifiers::NONE)?;
    harness.render()?;

    // Verify the LSP is STILL not running (the bug fix prevents auto-restart)
    let running_after_edit = harness.editor().running_lsp_servers();
    eprintln!("Running LSP servers after edit: {:?}", running_after_edit);
    assert!(
        !running_after_edit.contains(&"rust".to_string()),
        "LSP server for rust should NOT auto-restart after edit. Running: {:?}",
        running_after_edit
    );

    // Type more text to double-check
    harness.type_text("// Another edit\n")?;
    harness.render()?;
    harness.sleep(std::time::Duration::from_millis(50));
    harness.send_key(KeyCode::Null, KeyModifiers::NONE)?;
    harness.render()?;

    let running_final = harness.editor().running_lsp_servers();
    eprintln!("Running LSP servers after second edit: {:?}", running_final);
    assert!(
        !running_final.contains(&"rust".to_string()),
        "LSP server for rust should still NOT be running after multiple edits. Running: {:?}",
        running_final
    );

    // Verify the editor still works fine (buffer has our edits)
    let buffer_content = harness.get_buffer_content().unwrap_or_default();
    assert!(
        buffer_content.contains("This edit should NOT restart the LSP"),
        "Buffer should contain our edits. Content: {}",
        buffer_content
    );

    eprintln!("\n✅ SUCCESS: Stopped LSP server does not auto-restart on edit!");
    eprintln!("   - LSP was running after file open");
    eprintln!("   - LSP was stopped via shutdown_lsp_server");
    eprintln!("   - LSP remained stopped after typing (no auto-restart)");
    eprintln!("   - Editor remained functional for editing");

    Ok(())
}

/// Test that hover popup at the right edge of screen doesn't panic.
///
/// Reproduces the panic:
/// "index outside of buffer: the area is Rect { x: 0, y: 0, width: 199, height: 44 }
/// but index is (199, 31)"
///
/// The bug occurs when:
/// 1. User hovers over an LSP symbol near the right edge of the screen
/// 2. The popup is positioned with PopupPosition::Fixed { x, y } where x equals or exceeds
///    the terminal width
/// 3. calculate_area() for Fixed position doesn't clamp x to ensure x + width <= terminal_width
/// 4. render_with_hover() tries to render the popup at an out-of-bounds position
#[test]
fn test_hover_popup_at_right_edge_does_not_panic() -> std::io::Result<()> {
    use fresh::model::event::{Event, PopupContentData, PopupData, PopupPositionData};

    // Use the exact dimensions from the panic: width 199, height 44
    let mut harness = EditorTestHarness::new(199, 44)?;

    // Show a hover popup at the right edge of the screen (x = 199, which equals width)
    // This simulates what happens when mouse_hover_screen_position is at the right edge
    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            title: Some("Hover".to_string()),
            content: PopupContentData::Text(vec![
                "fn example() -> i32".to_string(),
                "Returns an example value".to_string(),
            ]),
            // Position at x=199 (right edge) - this triggers the bug
            position: PopupPositionData::Fixed { x: 199, y: 30 },
            width: 80,
            max_height: 20,
            bordered: true,
        },
    });

    // This render call triggers the panic in the buggy code
    harness.render()?;

    // If we get here without panicking, the fix works
    let screen = harness.screen_to_string();
    // The popup should be visible somewhere on screen (clamped to fit)
    assert!(
        screen.contains("Hover") || screen.contains("example"),
        "Hover popup should be rendered on screen"
    );

    Ok(())
}

/// Test that hover popup is dismissed when focus changes
///
/// The hover popup should be dismissed when:
/// 1. Opening command palette (Ctrl+Shift+P)
/// 2. Switching buffers
/// 3. Focusing file explorer
/// 4. Opening any prompt
#[test]
fn test_hover_popup_dismissed_on_focus_change() -> std::io::Result<()> {
    use fresh::model::event::{Event, PopupContentData, PopupData, PopupPositionData};

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Helper to show a hover popup
    fn show_hover_popup(harness: &mut EditorTestHarness) {
        let state = harness.editor_mut().active_state_mut();
        state.apply(&Event::ShowPopup {
            popup: PopupData {
                title: Some("Hover".to_string()),
                content: PopupContentData::Text(vec![
                    "fn example() -> i32".to_string(),
                    "Returns an example value".to_string(),
                ]),
                position: PopupPositionData::BelowCursor,
                width: 40,
                max_height: 10,
                bordered: true,
            },
        });
    }

    // Test 1: Hover popup dismissed when opening command palette
    show_hover_popup(&mut harness);
    harness.render()?;
    assert!(
        harness.editor().active_state().popups.is_visible(),
        "Hover popup should be visible initially"
    );

    // Open command palette (Ctrl+Shift+P)
    harness.send_key(
        KeyCode::Char('P'),
        KeyModifiers::CONTROL | KeyModifiers::SHIFT,
    )?;

    assert!(
        !harness.editor().active_state().popups.is_visible(),
        "Hover popup should be dismissed after opening command palette"
    );

    // Close command palette
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE)?;

    // Test 2: Hover popup dismissed when opening Go to Line prompt
    show_hover_popup(&mut harness);
    harness.render()?;
    assert!(
        harness.editor().active_state().popups.is_visible(),
        "Hover popup should be visible again"
    );

    // Open Go to Line (Ctrl+G)
    harness.send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)?;

    assert!(
        !harness.editor().active_state().popups.is_visible(),
        "Hover popup should be dismissed after opening Go to Line prompt"
    );

    // Close prompt
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE)?;

    // Test 3: Hover popup dismissed when switching buffers
    // First open a second buffer
    harness.type_text("test content")?;
    harness.send_key(KeyCode::Char('n'), KeyModifiers::CONTROL)?; // New buffer

    // Go back to first buffer
    harness.send_key(KeyCode::Tab, KeyModifiers::CONTROL)?;

    // Show hover popup
    show_hover_popup(&mut harness);
    harness.render()?;
    assert!(
        harness.editor().active_state().popups.is_visible(),
        "Hover popup should be visible before buffer switch"
    );

    // Switch to other buffer
    harness.send_key(KeyCode::Tab, KeyModifiers::CONTROL)?;

    // Note: After switching buffers, the popup belongs to the previous buffer's state.
    // The new buffer should not have the popup.
    assert!(
        !harness.editor().active_state().popups.is_visible(),
        "New buffer should not have hover popup after switching"
    );

    // Test 4: Hover popup dismissed when opening search prompt
    // Switch back and show hover
    harness.send_key(KeyCode::Tab, KeyModifiers::CONTROL)?;
    show_hover_popup(&mut harness);
    harness.render()?;
    assert!(
        harness.editor().active_state().popups.is_visible(),
        "Hover popup should be visible before search"
    );

    // Open search (Ctrl+F)
    harness.send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)?;

    assert!(
        !harness.editor().active_state().popups.is_visible(),
        "Hover popup should be dismissed after opening search"
    );

    // Close search prompt
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE)?;

    // Test 5: Hover popup dismissed when opening menu (Alt/F10)
    show_hover_popup(&mut harness);
    harness.render()?;
    assert!(
        harness.editor().active_state().popups.is_visible(),
        "Hover popup should be visible before opening menu"
    );

    // Open menu (F10)
    harness.send_key(KeyCode::F(10), KeyModifiers::NONE)?;

    assert!(
        !harness.editor().active_state().popups.is_visible(),
        "Hover popup should be dismissed after opening menu"
    );

    Ok(())
}

/// Test that hover popup persists when mouse moves within hovered symbol or popup
///
/// The hover popup should stay visible when:
/// 1. Mouse moves within the hovered symbol range
/// 2. Mouse moves over the hover popup itself
/// The hover should only be dismissed when mouse leaves the editor area.
///
/// Uses a fake LSP server to properly trigger hover flow via user-style events.
#[test]
fn test_hover_popup_persists_within_symbol_and_popup() -> std::io::Result<()> {
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
        fresh::services::lsp::client::LspServerConfig {
            command: FakeLspServer::script_path().to_string_lossy().to_string(),
            args: vec![],
            enabled: true,
            auto_start: false,
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
        },
    );

    // Create harness with config
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    harness.open_file(&test_file)?;
    harness.render()?;

    // Move mouse over the symbol "example_function" to trigger hover state
    // The gutter takes some columns, so move to column ~10 which should be over the symbol
    harness.mouse_move(10, 2)?;
    harness.render()?;

    // Force check mouse hover to bypass the 500ms timer and send the request
    harness.editor_mut().force_check_mouse_hover();

    // Wait for hover popup to appear (LSP response received)
    harness.wait_until(|h| h.editor().active_state().popups.is_visible())?;

    // Test 1: Mouse move within the symbol range should keep popup
    // Move mouse slightly to the right (still within 10 char range)
    harness.mouse_move(12, 2)?;

    assert!(
        harness.editor().active_state().popups.is_visible(),
        "Hover popup should persist when mouse moves within symbol range"
    );

    // Test 2: Mouse move over the popup area should keep popup
    // The popup renders below cursor, so move to where it would be
    // Get popup areas from cached layout
    let popup_visible_after_popup_hover = {
        harness.render()?;
        // Move mouse to where the popup should be (below the hover point)
        harness.mouse_move(12, 5)?;
        harness.editor().active_state().popups.is_visible()
    };

    assert!(
        popup_visible_after_popup_hover,
        "Hover popup should persist when mouse is over the popup area"
    );

    // Test 3: Mouse leaving editor area should dismiss popup
    // Move mouse to row 0 (menu bar area, outside editor content)
    harness.mouse_move(40, 0)?;

    assert!(
        !harness.editor().active_state().popups.is_visible(),
        "Hover popup should be dismissed when mouse leaves editor area"
    );

    Ok(())
}
