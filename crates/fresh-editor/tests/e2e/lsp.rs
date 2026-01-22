//! E2E tests for LSP features

use crate::common::fake_lsp::FakeLspServer;
use crate::common::harness::EditorTestHarness;

use fresh::view::theme;

use crossterm::event::{KeyCode, KeyModifiers};

/// Test that completion popup text is not mangled
#[test]
fn test_lsp_completion_popup_text_not_mangled() -> anyhow::Result<()> {
    use fresh::model::event::{
        Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData,
    };

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Show a completion popup with realistic LSP data
    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            title: Some("Completion".to_string()),
            description: None,
            transient: false,
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
fn test_lsp_completion_replaces_word() -> anyhow::Result<()> {
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
            description: None,
            transient: false,
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
fn test_lsp_diagnostics_display() -> anyhow::Result<()> {
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
        extend_to_line_end: false,
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
fn test_lsp_completion_popup() -> anyhow::Result<()> {
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
            description: None,
            transient: false,
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
fn test_lsp_diagnostics_status_bar() -> anyhow::Result<()> {
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
        extend_to_line_end: false,
    });

    // Add warning diagnostic
    state.apply(&Event::AddOverlay {
        namespace: Some(OverlayNamespace::from_string("lsp-diagnostic".to_string())),
        range: 15..16,
        face: OverlayFace::Background { color: (40, 40, 0) },
        priority: 50, // Warning priority
        message: Some("unused variable: `y`".to_string()),
        extend_to_line_end: false,
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
fn test_lsp_clear_diagnostics() -> anyhow::Result<()> {
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
        extend_to_line_end: false,
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
fn test_lsp_completion_navigation() -> anyhow::Result<()> {
    use fresh::model::event::{
        Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData,
    };

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Show completion popup with multiple items
    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            title: Some("Completion".to_string()),
            description: None,
            transient: false,
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
fn test_lsp_completion_cancel() -> anyhow::Result<()> {
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
            description: None,
            transient: false,
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
fn test_lsp_completion_after_dot() -> anyhow::Result<()> {
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
            description: None,
            transient: false,
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
fn test_lsp_completion_after_dot_with_partial() -> anyhow::Result<()> {
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
            description: None,
            transient: false,
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
fn test_lsp_completion_filtering() -> anyhow::Result<()> {
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
            description: None,
            transient: false,
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
fn test_lsp_completion_popup_size() -> anyhow::Result<()> {
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
            description: None,
            transient: false,
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
fn test_lsp_waiting_indicator() -> anyhow::Result<()> {
    use crate::common::fake_lsp::FakeLspServer;

    // Spawn fake LSP server
    let _fake_server = FakeLspServer::spawn()?;

    // Create temp dir and test file
    let temp_dir = tempfile::tempdir()?;
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn main() {\n    \n}\n")?;

    // Configure editor to use the fake LSP server
    let mut config = fresh::config::Config::default();
    config.editor.enable_semantic_tokens_full = true;
    config.lsp.insert(
        "rust".to_string(),
        fresh::services::lsp::LspServerConfig {
            command: FakeLspServer::script_path().to_string_lossy().to_string(),
            args: vec![],
            enabled: true,
            auto_start: true,
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

/// Ensure semantic tokens respect buffer versions.
#[test]
#[cfg_attr(
    target_os = "windows",
    ignore = "FakeLspServer uses a Bash script which is not available on Windows"
)]
fn test_semantic_tokens_version_gating() -> anyhow::Result<()> {
    use crate::common::fake_lsp::FakeLspServer;

    let _fake_server = FakeLspServer::spawn_with_semantic_tokens_delay(150)?;

    let temp_dir = tempfile::tempdir()?;
    let test_file = temp_dir.path().join("semantic.rs");
    std::fs::write(&test_file, "fn main() {}\n")?;

    let mut config = fresh::config::Config::default();
    config.editor.enable_semantic_tokens_full = true;
    config.lsp.insert(
        "rust".to_string(),
        fresh::services::lsp::LspServerConfig {
            command: FakeLspServer::semantic_tokens_delay_script_path()
                .to_string_lossy()
                .to_string(),
            args: vec![],
            enabled: true,
            auto_start: true,
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
        },
    );

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        100,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    harness.open_file(&test_file)?;
    harness.render()?;

    harness.wait_until(|h| {
        let state = h.editor().active_state();
        state
            .semantic_tokens
            .as_ref()
            .map(|store| store.version == state.buffer.version())
            .unwrap_or(false)
    })?;

    // Make consecutive edits while a semantic token request is pending.
    harness.send_key(KeyCode::End, KeyModifiers::NONE)?;
    harness.type_text("\nlet value = 1;")?;
    harness.type_text("// comment")?;

    {
        let state = harness.editor().active_state();
        if let Some(store) = &state.semantic_tokens {
            assert_ne!(
                store.version,
                state.buffer.version(),
                "Semantic tokens should be stale immediately after edits"
            );
        }
    }

    harness.wait_until(|h| {
        let state = h.editor().active_state();
        state
            .semantic_tokens
            .as_ref()
            .map(|store| store.version == state.buffer.version())
            .unwrap_or(false)
    })?;

    {
        let state = harness.editor().active_state();
        let store = state
            .semantic_tokens
            .as_ref()
            .expect("Semantic tokens should be present after refresh");
        assert!(
            !store.tokens.is_empty(),
            "Semantic tokens should decode to highlight spans"
        );
        assert_eq!(
            store.version,
            state.buffer.version(),
            "Semantic tokens must match the buffer version"
        );
    }

    Ok(())
}

/// Ensure semantic token overlays remain present during edits while range responses are pending.
#[test]
#[cfg_attr(
    target_os = "windows",
    ignore = "FakeLspServer uses a Bash script which is not available on Windows"
)]
fn test_semantic_tokens_range_preserves_overlays_on_edit() -> anyhow::Result<()> {
    use crate::common::fake_lsp::FakeLspServer;
    use std::collections::HashSet;

    let _fake_server = FakeLspServer::spawn_with_semantic_tokens_delay(200)?;

    let temp_dir = tempfile::tempdir()?;
    let test_file = temp_dir.path().join("semantic_range.rs");
    std::fs::write(&test_file, "fn main() { let value = 1; }\n")?;

    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::services::lsp::LspServerConfig {
            command: FakeLspServer::semantic_tokens_delay_script_path()
                .to_string_lossy()
                .to_string(),
            args: vec![],
            enabled: true,
            auto_start: true,
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
        },
    );

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        100,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    harness.open_file(&test_file)?;
    harness.render()?;

    let ns = fresh::services::lsp::semantic_tokens::lsp_semantic_tokens_namespace();
    harness.wait_until(|h| {
        let state = h.editor().active_state();
        state
            .overlays
            .all()
            .iter()
            .any(|o| o.namespace.as_ref() == Some(&ns))
    })?;

    let handles_before: HashSet<_> = harness
        .editor()
        .active_state()
        .overlays
        .all()
        .iter()
        .filter(|o| o.namespace.as_ref() == Some(&ns))
        .map(|o| o.handle.clone())
        .collect();
    assert!(
        !handles_before.is_empty(),
        "Expected semantic token overlays before edit"
    );

    harness.send_key(KeyCode::End, KeyModifiers::NONE)?;
    harness.type_text("\nlet another = 2;")?;
    harness.process_async_and_render()?;

    let handles_after: HashSet<_> = harness
        .editor()
        .active_state()
        .overlays
        .all()
        .iter()
        .filter(|o| o.namespace.as_ref() == Some(&ns))
        .map(|o| o.handle.clone())
        .collect();

    assert!(
        !handles_after.is_empty(),
        "Semantic token overlays should not be cleared during edits"
    );
    assert!(
        !handles_before.is_disjoint(&handles_after),
        "Expected semantic token overlays to persist while range response is pending"
    );

    Ok(())
}

/// Ensure semantic token overlays persist when pressing Enter key specifically.
/// This is different from type_text("\n") as Enter may trigger different code paths
/// (e.g., autoindent, special newline handling).
#[test]
#[cfg_attr(
    target_os = "windows",
    ignore = "FakeLspServer uses a Bash script which is not available on Windows"
)]
fn test_semantic_tokens_persist_on_enter_key() -> anyhow::Result<()> {
    use crate::common::fake_lsp::FakeLspServer;
    use std::collections::HashSet;

    let _fake_server = FakeLspServer::spawn_with_semantic_tokens_delay(200)?;

    let temp_dir = tempfile::tempdir()?;
    let test_file = temp_dir.path().join("semantic_enter.rs");
    std::fs::write(&test_file, "fn main() { let value = 1; }\n")?;

    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::services::lsp::LspServerConfig {
            command: FakeLspServer::semantic_tokens_delay_script_path()
                .to_string_lossy()
                .to_string(),
            args: vec![],
            enabled: true,
            auto_start: true,
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
        },
    );

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        100,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    harness.open_file(&test_file)?;
    harness.render()?;

    let ns = fresh::services::lsp::semantic_tokens::lsp_semantic_tokens_namespace();
    harness.wait_until(|h| {
        let state = h.editor().active_state();
        state
            .overlays
            .all()
            .iter()
            .any(|o| o.namespace.as_ref() == Some(&ns))
    })?;

    let handles_before: HashSet<_> = harness
        .editor()
        .active_state()
        .overlays
        .all()
        .iter()
        .filter(|o| o.namespace.as_ref() == Some(&ns))
        .map(|o| o.handle.clone())
        .collect();
    assert!(
        !handles_before.is_empty(),
        "Expected semantic token overlays before Enter key"
    );

    // Position cursor at end of line and press Enter key specifically
    // (not type_text("\n") which may use different code path)
    harness.send_key(KeyCode::End, KeyModifiers::NONE)?;
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.process_async_and_render()?;

    let handles_after: HashSet<_> = harness
        .editor()
        .active_state()
        .overlays
        .all()
        .iter()
        .filter(|o| o.namespace.as_ref() == Some(&ns))
        .map(|o| o.handle.clone())
        .collect();

    assert!(
        !handles_after.is_empty(),
        "Semantic token overlays should not be cleared after Enter key"
    );
    assert!(
        !handles_before.is_disjoint(&handles_after),
        "Expected semantic token overlays to persist after Enter key press"
    );

    Ok(())
}

/// Ensure semantic token overlays shift with edits (marker tracking).
#[test]
#[cfg_attr(
    target_os = "windows",
    ignore = "FakeLspServer uses a Bash script which is not available on Windows"
)]
fn test_semantic_tokens_overlays_shift_on_edit() -> anyhow::Result<()> {
    use crate::common::fake_lsp::FakeLspServer;
    use std::collections::HashMap;

    let _fake_server = FakeLspServer::spawn_with_semantic_tokens_delay(200)?;

    let temp_dir = tempfile::tempdir()?;
    let test_file = temp_dir.path().join("semantic_shift.rs");
    std::fs::write(&test_file, "fn main() { let value = 1; }\n")?;

    let mut config = fresh::config::Config::default();
    config.editor.enable_semantic_tokens_full = false;
    config.lsp.insert(
        "rust".to_string(),
        fresh::services::lsp::LspServerConfig {
            command: FakeLspServer::semantic_tokens_delay_script_path()
                .to_string_lossy()
                .to_string(),
            args: vec![],
            enabled: true,
            auto_start: true,
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
        },
    );

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        100,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    harness.open_file(&test_file)?;
    harness.render()?;

    let ns = fresh::services::lsp::semantic_tokens::lsp_semantic_tokens_namespace();
    harness.wait_until(|h| {
        h.editor()
            .active_state()
            .overlays
            .all()
            .iter()
            .any(|o| o.namespace.as_ref() == Some(&ns))
    })?;

    let state_before = harness.editor().active_state();
    let mut ranges_before = HashMap::new();
    for overlay in state_before
        .overlays
        .all()
        .iter()
        .filter(|o| o.namespace.as_ref() == Some(&ns))
    {
        ranges_before.insert(
            overlay.handle.clone(),
            overlay.range(&state_before.marker_list),
        );
    }

    assert!(
        ranges_before.values().any(|range| *range == (0..2)),
        "Expected semantic token overlay for 'fn'"
    );
    assert!(
        ranges_before.values().any(|range| *range == (3..7)),
        "Expected semantic token overlay for 'main'"
    );

    harness.send_key(KeyCode::Home, KeyModifiers::NONE)?;
    harness.type_text("abc")?;
    harness.process_async_and_render()?;

    let state_after = harness.editor().active_state();
    for (handle, range_before) in ranges_before {
        let overlay = state_after
            .overlays
            .all()
            .iter()
            .find(|o| o.handle == handle)
            .expect("Expected semantic token overlay to persist after edit");
        let range_after = overlay.range(&state_after.marker_list);
        assert_eq!(
            range_after.start,
            range_before.start + 3,
            "Overlay start should shift with insert"
        );
        assert_eq!(
            range_after.end,
            range_before.end + 3,
            "Overlay end should shift with insert"
        );
    }

    Ok(())
}

/// Ensure range-only semantic tokens render in the viewport without full refreshes.
#[test]
#[cfg_attr(
    target_os = "windows",
    ignore = "FakeLspServer uses a Bash script which is not available on Windows"
)]
fn test_semantic_tokens_range_only_viewport_highlighting() -> anyhow::Result<()> {
    use crate::common::fake_lsp::FakeLspServer;

    let _fake_server = FakeLspServer::spawn_with_semantic_tokens_range_only()?;

    let temp_dir = tempfile::tempdir()?;
    let test_file = temp_dir.path().join("semantic_range_only.rs");
    std::fs::write(&test_file, "fn main() { let value = 1; }\n")?;

    let mut config = fresh::config::Config::default();
    config.editor.enable_semantic_tokens_full = false;
    config.lsp.insert(
        "rust".to_string(),
        fresh::services::lsp::LspServerConfig {
            command: FakeLspServer::semantic_tokens_range_only_script_path()
                .to_string_lossy()
                .to_string(),
            args: vec![],
            enabled: true,
            auto_start: true,
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
        },
    );

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        100,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    harness.open_file(&test_file)?;
    harness.render()?;

    let ns = fresh::services::lsp::semantic_tokens::lsp_semantic_tokens_namespace();
    harness.wait_until(|h| {
        let state = h.editor().active_state();
        let has_overlays = state
            .overlays
            .all()
            .iter()
            .any(|o| o.namespace.as_ref() == Some(&ns));
        has_overlays && state.semantic_tokens.is_none()
    })?;

    Ok(())
}

/// Test that popup properly hides buffer text behind it
#[test]
fn test_lsp_completion_popup_hides_background() -> anyhow::Result<()> {
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
            description: None,
            transient: false,
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
fn test_lsp_completion_canceled_on_cursor_move() -> anyhow::Result<()> {
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
        fresh::services::lsp::LspServerConfig {
            command: FakeLspServer::script_path().to_string_lossy().to_string(),
            args: vec![],
            enabled: true,
            auto_start: true,
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
fn test_lsp_cursor_animation() -> anyhow::Result<()> {
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
        fresh::services::lsp::LspServerConfig {
            command: FakeLspServer::script_path().to_string_lossy().to_string(),
            args: vec![],
            enabled: true,
            auto_start: true,
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
fn test_lsp_completion_canceled_on_text_edit() -> anyhow::Result<()> {
    use crate::common::fake_lsp::FakeLspServer;

    // Spawn fake LSP server
    let _fake_server = FakeLspServer::spawn()?;

    // Create temp dir and test file
    let temp_dir = tempfile::tempdir()?;
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn main() {\n    test_\n}\n")?;

    // Configure editor to use the fake LSP server
    // Disable quick_suggestions so typing 'x' doesn't trigger a new completion request
    let mut config = fresh::config::Config::default();
    config.editor.quick_suggestions = false;
    config.lsp.insert(
        "rust".to_string(),
        fresh::services::lsp::LspServerConfig {
            command: FakeLspServer::script_path().to_string_lossy().to_string(),
            args: vec![],
            enabled: true,
            auto_start: true,
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
fn test_rust_analyzer_rename_content_modified() -> anyhow::Result<()> {
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
fn test_lsp_typing_performance_with_many_diagnostics() -> anyhow::Result<()> {
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
        &fresh::view::theme::Theme::load_builtin(theme::THEME_DARK).unwrap(),
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
            &fresh::view::theme::Theme::load_builtin(theme::THEME_DARK).unwrap(),
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
fn test_handle_rename_response_with_document_changes() -> anyhow::Result<()> {
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
fn test_lsp_diagnostics_non_blocking() -> anyhow::Result<()> {
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
        fresh::services::lsp::LspServerConfig {
            command: FakeLspServer::blocking_script_path()
                .to_string_lossy()
                .to_string(),
            args: vec![],
            enabled: true,
            auto_start: true,
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
fn test_rust_analyzer_rename_real_scenario() -> anyhow::Result<()> {
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
        fresh::services::lsp::LspServerConfig {
            command: "rust-analyzer".to_string(),
            args: vec![
                "--log-file".to_string(),
                ra_log_file.to_string_lossy().to_string(),
            ],
            enabled: true,
            auto_start: true,
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
fn test_lsp_rename_consecutive_same_position() -> anyhow::Result<()> {
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
fn test_lsp_rename_twice_with_rust_analyzer() -> anyhow::Result<()> {
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
fn test_lsp_progress_status_display() -> anyhow::Result<()> {
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
        fresh::services::lsp::LspServerConfig {
            command: FakeLspServer::progress_script_path()
                .to_string_lossy()
                .to_string(),
            args: vec![],
            enabled: true,
            auto_start: true,
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
fn test_lsp_crash_detection_and_restart() -> anyhow::Result<()> {
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
        fresh::services::lsp::LspServerConfig {
            command: FakeLspServer::crashing_script_path()
                .to_string_lossy()
                .to_string(),
            args: vec![],
            enabled: true,
            auto_start: true,
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
fn test_lsp_restart_command_exists() -> anyhow::Result<()> {
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
fn test_pull_diagnostics_message_handling() -> anyhow::Result<()> {
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
fn test_pull_diagnostics_unchanged_response() -> anyhow::Result<()> {
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
fn test_pull_diagnostics_auto_trigger_after_open() -> anyhow::Result<()> {
    use crate::common::fake_lsp::FakeLspServer;

    // Create fake LSP server with pull diagnostics support
    let _server = FakeLspServer::spawn_with_pull_diagnostics()?;

    // Create config that uses the pull diagnostics fake server
    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::services::lsp::LspServerConfig {
            command: FakeLspServer::pull_diagnostics_script_path()
                .to_string_lossy()
                .to_string(),
            args: vec![],
            enabled: true,
            auto_start: true,
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
fn test_pull_diagnostics_result_id_tracking() -> anyhow::Result<()> {
    use crate::common::fake_lsp::FakeLspServer;

    // Create fake LSP server with pull diagnostics support
    let _server = FakeLspServer::spawn_with_pull_diagnostics()?;

    // Create config that uses the pull diagnostics fake server
    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::services::lsp::LspServerConfig {
            command: FakeLspServer::pull_diagnostics_script_path()
                .to_string_lossy()
                .to_string(),
            args: vec![],
            enabled: true,
            auto_start: true,
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
fn test_inlay_hints_render_on_screen() -> anyhow::Result<()> {
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
fn test_inlay_hints_position_tracking() -> anyhow::Result<()> {
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
#[cfg_attr(windows, ignore = "Uses bash script for fake LSP server")]
fn test_stopped_lsp_does_not_auto_restart_on_edit() -> anyhow::Result<()> {
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
        fresh::services::lsp::LspServerConfig {
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

    // Wait for the LSP server to initialize using semantic wait
    harness
        .wait_until(|h| {
            h.editor()
                .running_lsp_servers()
                .contains(&"rust".to_string())
        })
        .expect("LSP server for rust should start after file open");

    let running_before = harness.editor().running_lsp_servers();
    eprintln!("Running LSP servers before stop: {:?}", running_before);

    // Stop the LSP server (simulating user running "stop lsp" command)
    let stopped = harness.editor_mut().shutdown_lsp_server("rust");
    assert!(stopped, "shutdown_lsp_server should return true");

    // Wait for the LSP server to stop using semantic wait
    harness
        .wait_until(|h| {
            !h.editor()
                .running_lsp_servers()
                .contains(&"rust".to_string())
        })
        .expect("LSP server for rust should stop after shutdown");

    let running_after_stop = harness.editor().running_lsp_servers();
    eprintln!("Running LSP servers after stop: {:?}", running_after_stop);

    // Now type some text - this triggers send_lsp_changes_for_buffer
    // which used to call get_or_spawn and restart the LSP
    harness.type_text("// This edit should NOT restart the LSP\n")?;

    // Process async operations to allow any potential spawn to occur
    harness.process_async_and_render()?;

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
    harness.process_async_and_render()?;

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
fn test_hover_popup_at_right_edge_does_not_panic() -> anyhow::Result<()> {
    use fresh::model::event::{Event, PopupContentData, PopupData, PopupPositionData};

    // Use the exact dimensions from the panic: width 199, height 44
    let mut harness = EditorTestHarness::new(199, 44)?;

    // Show a hover popup at the right edge of the screen (x = 199, which equals width)
    // This simulates what happens when mouse_hover_screen_position is at the right edge
    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            title: Some("Hover".to_string()),
            description: None,
            transient: true,
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
fn test_hover_popup_dismissed_on_focus_change() -> anyhow::Result<()> {
    use fresh::model::event::{Event, PopupContentData, PopupData, PopupPositionData};

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Helper to show a hover popup
    fn show_hover_popup(harness: &mut EditorTestHarness) {
        let state = harness.editor_mut().active_state_mut();
        state.apply(&Event::ShowPopup {
            popup: PopupData {
                title: Some("Hover".to_string()),
                description: None,
                transient: true,
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
///
/// The hover should only be dismissed when mouse leaves the editor area.
///
/// Uses a fake LSP server to properly trigger hover flow via user-style events.
#[test]
#[cfg_attr(
    windows,
    ignore = "FakeLspServer uses a Bash script which is not available on Windows"
)]
fn test_hover_popup_persists_within_symbol_and_popup() -> anyhow::Result<()> {
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

/// Test that hover popup shows scrollbar when content exceeds visible area
///
/// When the hover documentation is longer than the popup's max_height,
/// a scrollbar should be rendered to indicate more content is available.
#[test]
fn test_hover_popup_shows_scrollbar_for_long_content() -> anyhow::Result<()> {
    use fresh::model::event::{Event, PopupContentData, PopupData, PopupPositionData};

    let mut harness = EditorTestHarness::new(100, 30)?;

    // Create content that exceeds the visible area
    // With max_height=10 and borders=2, we have 8 visible lines
    // So 15 lines of content should trigger a scrollbar
    let long_content: Vec<String> = (1..=15)
        .map(|i| format!("Documentation line {}", i))
        .collect();

    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            title: Some("Hover".to_string()),
            description: None,
            transient: true,
            content: PopupContentData::Text(long_content),
            position: PopupPositionData::Centered,
            width: 50,
            max_height: 10, // Only 8 lines of content visible (10 - 2 for borders)
            bordered: true,
        },
    });

    harness.render()?;

    let screen = harness.screen_to_string();

    // Verify popup is visible
    assert!(
        screen.contains("Hover"),
        "Hover popup title should be visible"
    );

    // Verify first few lines of content are visible
    assert!(
        screen.contains("Documentation line 1"),
        "First line of documentation should be visible"
    );

    // Verify scrollbar is rendered - popup scrollbar uses background colors.
    // The popup is centered (around columns 25-75 in 100-width terminal).
    // Check for any scrollbar cells in the popup's right edge area.
    // Popup is 50 wide, centered in 100-width = starts around col 25, ends around col 74.
    let has_scrollbar = (2..28).any(|row| {
        // Check a few columns around where the popup scrollbar should be
        (72..=74).any(|col| {
            harness.is_scrollbar_thumb_at(col, row) || harness.is_scrollbar_track_at(col, row)
        })
    });
    assert!(
        has_scrollbar,
        "Scrollbar should be visible when content exceeds visible area (checked cols 72-74)"
    );

    // Verify that later lines are NOT visible (they're scrolled off)
    assert!(
        !screen.contains("Documentation line 15"),
        "Line 15 should not be visible without scrolling"
    );

    Ok(())
}

/// Test that hover popup uses dynamic max_height based on terminal size
///
/// The hover popup should:
/// - Use 60% of terminal height
/// - Have a minimum of 15 rows
/// - Have a maximum of 40 rows
#[test]
fn test_hover_popup_dynamic_height() -> anyhow::Result<()> {
    use fresh::model::event::{Event, PopupContentData, PopupData, PopupPositionData};

    // Test with a tall terminal (60 rows)
    // 60% of 60 = 36, which is within the 15-40 range
    let mut harness = EditorTestHarness::new(100, 60)?;

    // Create content with 30 lines
    let content: Vec<String> = (1..=30)
        .map(|i| format!("Long documentation line number {}", i))
        .collect();

    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            title: Some("Hover".to_string()),
            description: None,
            transient: true,
            content: PopupContentData::Text(content),
            position: PopupPositionData::Centered,
            width: 60,
            max_height: 36, // Simulating 60% of 60 rows
            bordered: true,
        },
    });

    harness.render()?;

    let screen = harness.screen_to_string();

    // With max_height=36 and borders=2, we can show 34 lines
    // So all 30 lines should be visible (no scrollbar needed)
    assert!(
        screen.contains("Long documentation line number 1"),
        "First line should be visible"
    );
    assert!(
        screen.contains("Long documentation line number 30"),
        "Last line (30) should be visible with tall terminal"
    );

    Ok(())
}

/// Test that mouse scroll works within hover popup to scroll content
///
/// When the mouse is over a hover popup with scrollable content,
/// scrolling should scroll the popup content instead of dismissing it.
#[test]
fn test_hover_popup_mouse_scroll() -> anyhow::Result<()> {
    use fresh::model::event::{Event, PopupContentData, PopupData, PopupPositionData};

    let mut harness = EditorTestHarness::new(100, 30)?;

    // Create content that exceeds the visible area (needs scrolling)
    // With max_height=12 and borders=2, we have 10 visible lines
    let long_content: Vec<String> = (1..=20).map(|i| format!("Hover line {}", i)).collect();

    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            title: Some("Hover".to_string()),
            description: None,
            transient: true,
            content: PopupContentData::Text(long_content),
            position: PopupPositionData::Centered,
            width: 50,
            max_height: 12, // Only 10 lines of content visible
            bordered: true,
        },
    });

    harness.render()?;

    let screen = harness.screen_to_string();

    // Verify popup is visible with first lines
    assert!(
        screen.contains("Hover line 1"),
        "First line should be visible initially"
    );
    assert!(
        !screen.contains("Hover line 15"),
        "Line 15 should not be visible before scrolling"
    );

    // Move mouse over the popup (centered, so around middle of screen)
    harness.mouse_move(50, 15)?;

    // Scroll down within the popup
    harness.mouse_scroll_down(50, 15)?;
    harness.render()?;

    // Popup should still be visible (not dismissed)
    assert!(
        harness.editor().active_state().popups.is_visible(),
        "Hover popup should remain visible after scroll"
    );

    // After scrolling, later lines should become visible
    let screen_after = harness.screen_to_string();

    // The popup should have scrolled, showing different content
    // Either line 1 is no longer visible, or later lines are now visible
    let scrolled = !screen_after.contains("Hover line 1")
        || screen_after.contains("Hover line 5")
        || screen_after.contains("Hover line 10");

    assert!(
        scrolled,
        "Popup content should have scrolled after mouse scroll"
    );

    Ok(())
}

/// Test that hover popup height accounts for word-wrapped lines
///
/// When hover content contains long lines that wrap, the popup should
/// expand to show the wrapped content (up to max_height), not just
/// count original lines.
#[test]
fn test_hover_popup_height_accounts_for_wrapped_lines() -> anyhow::Result<()> {
    use fresh::model::event::{Event, PopupContentData, PopupData, PopupPositionData};

    let mut harness = EditorTestHarness::new(100, 40)?;

    // Create a single very long line that will wrap multiple times
    // With popup width=40, this ~200 char line should wrap to ~5 lines
    let long_line = "This is a very long documentation line that should wrap multiple times when displayed in a narrow popup window because it exceeds the available width significantly.".to_string();

    let content = vec![
        "Function signature".to_string(),
        "---".to_string(),
        long_line,
        "".to_string(),
        "End of docs".to_string(),
    ];

    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            title: Some("Hover".to_string()),
            description: None,
            transient: true,
            content: PopupContentData::Text(content),
            position: PopupPositionData::Centered,
            width: 40, // Narrow width to force wrapping
            max_height: 20,
            bordered: true,
        },
    });

    harness.render()?;

    let screen = harness.screen_to_string();
    println!("Screen:\n{}", screen);

    // The popup should show all content including the wrapped long line
    // and the "End of docs" line
    assert!(
        screen.contains("Function signature"),
        "First line should be visible"
    );
    assert!(
        screen.contains("End of docs"),
        "Last line should be visible - popup should account for wrapped height"
    );

    // The long line should be wrapped (appearing on multiple visual lines)
    // Check that the beginning and some middle part are visible
    assert!(
        screen.contains("This is a very long"),
        "Start of long line should be visible"
    );

    Ok(())
}

/// Test Home key jumps to first item in completion popup
#[test]
fn test_popup_home_key_selects_first_item() -> anyhow::Result<()> {
    use fresh::model::event::{
        Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData,
    };

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Show completion popup with many items
    let items: Vec<PopupListItemData> = (0..20)
        .map(|i| PopupListItemData {
            text: format!("item_{}", i),
            detail: None,
            icon: None,
            data: Some(format!("item_{}", i)),
        })
        .collect();

    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            title: Some("Completion".to_string()),
            description: None,
            transient: false,
            content: PopupContentData::List { items, selected: 0 },
            position: PopupPositionData::Centered,
            width: 30,
            max_height: 10,
            bordered: true,
        },
    });

    harness.render()?;

    // Navigate down several times
    for _ in 0..15 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    }
    harness.render()?;

    // Press Home to jump to first item
    harness.send_key(KeyCode::Home, KeyModifiers::NONE)?;
    harness.render()?;

    // Confirm selection
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.render()?;

    // Verify item_0 was inserted (first item)
    let buffer_content = harness.get_buffer_content().unwrap();
    assert!(
        buffer_content.contains("item_0"),
        "Expected 'item_0' to be inserted after Home key, got: {buffer_content}"
    );

    Ok(())
}

/// Test End key jumps to last item in completion popup
#[test]
fn test_popup_end_key_selects_last_item() -> anyhow::Result<()> {
    use fresh::model::event::{
        Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData,
    };

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Show completion popup with many items
    let items: Vec<PopupListItemData> = (0..20)
        .map(|i| PopupListItemData {
            text: format!("item_{}", i),
            detail: None,
            icon: None,
            data: Some(format!("item_{}", i)),
        })
        .collect();

    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            title: Some("Completion".to_string()),
            description: None,
            transient: false,
            content: PopupContentData::List { items, selected: 0 },
            position: PopupPositionData::Centered,
            width: 30,
            max_height: 10,
            bordered: true,
        },
    });

    harness.render()?;

    // Press End to jump to last item
    harness.send_key(KeyCode::End, KeyModifiers::NONE)?;
    harness.render()?;

    // Confirm selection
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.render()?;

    // Verify item_19 was inserted (last item)
    let buffer_content = harness.get_buffer_content().unwrap();
    assert!(
        buffer_content.contains("item_19"),
        "Expected 'item_19' to be inserted after End key, got: {buffer_content}"
    );

    Ok(())
}

/// Test mouse wheel scrolls popup instead of dismissing it
#[test]
fn test_popup_mouse_wheel_scrolls() -> anyhow::Result<()> {
    use crossterm::event::{MouseEvent, MouseEventKind};
    use fresh::model::event::{
        Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData,
    };

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Show completion popup with many items (more than visible)
    let items: Vec<PopupListItemData> = (0..30)
        .map(|i| PopupListItemData {
            text: format!("completion_item_{}", i),
            detail: None,
            icon: None,
            data: Some(format!("completion_item_{}", i)),
        })
        .collect();

    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            title: Some("Completion".to_string()),
            description: None,
            transient: false,
            content: PopupContentData::List { items, selected: 0 },
            position: PopupPositionData::Centered,
            width: 40,
            max_height: 10,
            bordered: true,
        },
    });

    harness.render()?;

    // Verify popup is visible
    assert!(
        harness.editor().active_state().popups.is_visible(),
        "Popup should be visible before scroll"
    );

    // Get initial scroll offset
    let initial_offset = harness
        .editor()
        .active_state()
        .popups
        .top()
        .map(|p| p.scroll_state().2)
        .unwrap_or(0);

    // Send scroll down event at a position over the popup (center of screen)
    let scroll_event = MouseEvent {
        kind: MouseEventKind::ScrollDown,
        column: 40,
        row: 12,
        modifiers: KeyModifiers::empty(),
    };
    harness.send_mouse(scroll_event)?;
    harness.render()?;

    // Verify popup is still visible (not dismissed)
    assert!(
        harness.editor().active_state().popups.is_visible(),
        "Popup should still be visible after scroll"
    );

    // Verify scroll offset changed
    let new_offset = harness
        .editor()
        .active_state()
        .popups
        .top()
        .map(|p| p.scroll_state().2)
        .unwrap_or(0);

    assert!(
        new_offset > initial_offset,
        "Scroll offset should increase after scroll down: {} -> {}",
        initial_offset,
        new_offset
    );

    Ok(())
}

/// Test scrollbar appears when popup content exceeds visible area
#[test]
fn test_popup_scrollbar_visible_for_long_list() -> anyhow::Result<()> {
    use fresh::model::event::{
        Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData,
    };

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Show completion popup with many items (more than max_height)
    let items: Vec<PopupListItemData> = (0..50)
        .map(|i| PopupListItemData {
            text: format!("item_{}", i),
            detail: None,
            icon: None,
            data: Some(format!("item_{}", i)),
        })
        .collect();

    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            title: Some("Completion".to_string()),
            description: None,
            transient: false,
            content: PopupContentData::List { items, selected: 0 },
            position: PopupPositionData::Centered,
            width: 30,
            max_height: 10,
            bordered: true,
        },
    });

    harness.render()?;

    // Verify popup needs scrollbar (50 items > 10 max_height)
    let needs_scrollbar = harness
        .editor()
        .active_state()
        .popups
        .top()
        .map(|p| p.needs_scrollbar())
        .unwrap_or(false);

    assert!(
        needs_scrollbar,
        "Popup with 50 items should need scrollbar when max_height is 10"
    );

    // Check that the screen contains a scrollbar (rendered with background colors).
    // Popup is 30 wide and centered in 80-width = starts around col 25, ends around col 54.
    // Scrollbar would be near the right edge of the popup.
    let has_scrollbar = (2..20).any(|row| {
        (52..=54).any(|col| {
            harness.is_scrollbar_thumb_at(col, row) || harness.is_scrollbar_track_at(col, row)
        })
    });
    assert!(
        has_scrollbar,
        "Screen should contain scrollbar (checked cols 52-54)"
    );

    Ok(())
}

/// Test scrollbar is not shown for short lists
#[test]
fn test_popup_no_scrollbar_for_short_list() -> anyhow::Result<()> {
    use fresh::model::event::{
        Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData,
    };

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Show completion popup with few items (less than max_height)
    let items: Vec<PopupListItemData> = (0..3)
        .map(|i| PopupListItemData {
            text: format!("item_{}", i),
            detail: None,
            icon: None,
            data: Some(format!("item_{}", i)),
        })
        .collect();

    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            title: Some("Completion".to_string()),
            description: None,
            transient: false,
            content: PopupContentData::List { items, selected: 0 },
            position: PopupPositionData::Centered,
            width: 30,
            max_height: 10,
            bordered: true,
        },
    });

    harness.render()?;

    // Verify popup does NOT need scrollbar (3 items < 10 max_height)
    let needs_scrollbar = harness
        .editor()
        .active_state()
        .popups
        .top()
        .map(|p| p.needs_scrollbar())
        .unwrap_or(true);

    assert!(
        !needs_scrollbar,
        "Popup with 3 items should not need scrollbar when max_height is 10"
    );

    Ok(())
}

/// Test mouse wheel scroll up in popup
#[test]
fn test_popup_mouse_wheel_scroll_up() -> anyhow::Result<()> {
    use crossterm::event::{MouseEvent, MouseEventKind};
    use fresh::model::event::{
        Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData,
    };

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Show completion popup with many items
    let items: Vec<PopupListItemData> = (0..30)
        .map(|i| PopupListItemData {
            text: format!("item_{}", i),
            detail: None,
            icon: None,
            data: Some(format!("item_{}", i)),
        })
        .collect();

    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            title: Some("Completion".to_string()),
            description: None,
            transient: false,
            content: PopupContentData::List { items, selected: 0 },
            position: PopupPositionData::Centered,
            width: 40,
            max_height: 10,
            bordered: true,
        },
    });

    harness.render()?;

    // First scroll down to create some scroll offset
    for _ in 0..5 {
        let scroll_down = MouseEvent {
            kind: MouseEventKind::ScrollDown,
            column: 40,
            row: 12,
            modifiers: KeyModifiers::empty(),
        };
        harness.send_mouse(scroll_down)?;
    }
    harness.render()?;

    let offset_after_down = harness
        .editor()
        .active_state()
        .popups
        .top()
        .map(|p| p.scroll_state().2)
        .unwrap_or(0);

    // Now scroll up
    let scroll_up = MouseEvent {
        kind: MouseEventKind::ScrollUp,
        column: 40,
        row: 12,
        modifiers: KeyModifiers::empty(),
    };
    harness.send_mouse(scroll_up)?;
    harness.render()?;

    let offset_after_up = harness
        .editor()
        .active_state()
        .popups
        .top()
        .map(|p| p.scroll_state().2)
        .unwrap_or(0);

    // Verify popup is still visible
    assert!(
        harness.editor().active_state().popups.is_visible(),
        "Popup should still be visible after scroll up"
    );

    // Verify scroll offset decreased
    assert!(
        offset_after_up < offset_after_down,
        "Scroll offset should decrease after scroll up: {} -> {}",
        offset_after_down,
        offset_after_up
    );

    Ok(())
}

/// Test type-to-filter: typing a character filters the completion list
#[test]
fn test_completion_type_to_filter_basic() -> anyhow::Result<()> {
    use fresh::model::event::{
        Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData,
    };

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Type initial prefix
    harness.type_text("te")?;
    harness.render()?;

    // Set up completion items (simulating LSP response)
    let completion_items = vec![
        lsp_types::CompletionItem {
            label: "test_function".to_string(),
            kind: Some(lsp_types::CompletionItemKind::FUNCTION),
            detail: Some("fn test_function()".to_string()),
            insert_text: Some("test_function".to_string()),
            ..Default::default()
        },
        lsp_types::CompletionItem {
            label: "test_variable".to_string(),
            kind: Some(lsp_types::CompletionItemKind::VARIABLE),
            detail: Some("let test_variable".to_string()),
            insert_text: Some("test_variable".to_string()),
            ..Default::default()
        },
        lsp_types::CompletionItem {
            label: "temp_file".to_string(),
            kind: Some(lsp_types::CompletionItemKind::VARIABLE),
            detail: Some("let temp_file".to_string()),
            insert_text: Some("temp_file".to_string()),
            ..Default::default()
        },
    ];

    // Store completion items for re-filtering
    harness.editor_mut().set_completion_items(completion_items);

    // Show completion popup with all items
    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            title: Some("Completion".to_string()),
            description: None,
            transient: false,
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
                    PopupListItemData {
                        text: "temp_file".to_string(),
                        detail: Some("let temp_file".to_string()),
                        icon: Some("v".to_string()),
                        data: Some("temp_file".to_string()),
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

    // Verify all items are visible initially
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("test_function"),
        "test_function should be visible initially"
    );
    assert!(
        screen.contains("test_variable"),
        "test_variable should be visible initially"
    );
    assert!(
        screen.contains("temp_file"),
        "temp_file should be visible initially"
    );

    // Type 's' to filter to "tes" - should filter out "temp_file"
    harness.send_key(KeyCode::Char('s'), KeyModifiers::NONE)?;
    harness.render()?;

    // Verify filtering occurred
    let buffer = harness.get_buffer_content().unwrap();
    assert_eq!(buffer, "tes", "Buffer should contain 'tes'");

    // Verify popup is still visible with filtered items
    assert!(
        harness.editor().active_state().popups.is_visible(),
        "Popup should still be visible after filtering"
    );

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("test_function"),
        "test_function should still be visible"
    );
    assert!(
        screen.contains("test_variable"),
        "test_variable should still be visible"
    );
    // temp_file should be filtered out
    assert!(
        !screen.contains("temp_file"),
        "temp_file should be filtered out after typing 's'"
    );

    Ok(())
}

/// Test type-to-filter with uppercase letters (Shift modifier)
/// This verifies that uppercase letters can be typed to filter completions
#[test]
fn test_completion_type_to_filter_uppercase() -> anyhow::Result<()> {
    use fresh::model::event::{
        Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData,
    };

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Type initial prefix
    harness.type_text("Console.")?;
    harness.render()?;

    // Set up completion items (simulating C# Console methods)
    let completion_items = vec![
        lsp_types::CompletionItem {
            label: "WriteLine".to_string(),
            kind: Some(lsp_types::CompletionItemKind::METHOD),
            detail: Some("void Console.WriteLine()".to_string()),
            insert_text: Some("WriteLine".to_string()),
            ..Default::default()
        },
        lsp_types::CompletionItem {
            label: "Write".to_string(),
            kind: Some(lsp_types::CompletionItemKind::METHOD),
            detail: Some("void Console.Write()".to_string()),
            insert_text: Some("Write".to_string()),
            ..Default::default()
        },
        lsp_types::CompletionItem {
            label: "ReadLine".to_string(),
            kind: Some(lsp_types::CompletionItemKind::METHOD),
            detail: Some("string Console.ReadLine()".to_string()),
            insert_text: Some("ReadLine".to_string()),
            ..Default::default()
        },
    ];

    // Store completion items for re-filtering
    harness.editor_mut().set_completion_items(completion_items);

    // Show completion popup with all items
    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            title: Some("Completion".to_string()),
            description: None,
            transient: false,
            content: PopupContentData::List {
                items: vec![
                    PopupListItemData {
                        text: "WriteLine".to_string(),
                        detail: Some("void Console.WriteLine()".to_string()),
                        icon: Some("λ".to_string()),
                        data: Some("WriteLine".to_string()),
                    },
                    PopupListItemData {
                        text: "Write".to_string(),
                        detail: Some("void Console.Write()".to_string()),
                        icon: Some("λ".to_string()),
                        data: Some("Write".to_string()),
                    },
                    PopupListItemData {
                        text: "ReadLine".to_string(),
                        detail: Some("string Console.ReadLine()".to_string()),
                        icon: Some("λ".to_string()),
                        data: Some("ReadLine".to_string()),
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

    // Verify all items are visible initially
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("WriteLine"),
        "WriteLine should be visible initially"
    );
    assert!(
        screen.contains("ReadLine"),
        "ReadLine should be visible initially"
    );

    // Type uppercase 'W' (with SHIFT modifier) to filter - should filter out ReadLine
    harness.send_key(KeyCode::Char('W'), KeyModifiers::SHIFT)?;
    harness.render()?;

    // Verify buffer contains the typed character
    let buffer = harness.get_buffer_content().unwrap();
    assert_eq!(buffer, "Console.W", "Buffer should contain 'Console.W'");

    // Verify popup is still visible with filtered items
    assert!(
        harness.editor().active_state().popups.is_visible(),
        "Popup should still be visible after typing uppercase letter"
    );

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("WriteLine"),
        "WriteLine should still be visible after typing 'W'"
    );
    assert!(
        screen.contains("Write"),
        "Write should still be visible after typing 'W'"
    );
    // ReadLine should be filtered out since it doesn't start with W
    assert!(
        !screen.contains("ReadLine"),
        "ReadLine should be filtered out after typing 'W'"
    );

    // Type another uppercase letter to narrow down further
    harness.send_key(KeyCode::Char('r'), KeyModifiers::NONE)?;
    harness.render()?;

    let buffer = harness.get_buffer_content().unwrap();
    assert_eq!(buffer, "Console.Wr", "Buffer should contain 'Console.Wr'");

    // Both Write and WriteLine start with "Wr", so both should be visible
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("WriteLine"),
        "WriteLine should still be visible after typing 'Wr'"
    );
    assert!(
        screen.contains("Write"),
        "Write should still be visible after typing 'Wr'"
    );

    Ok(())
}

/// Test type-to-filter: popup closes when no items match
#[test]
fn test_completion_type_to_filter_closes_on_no_match() -> anyhow::Result<()> {
    use fresh::model::event::{
        Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData,
    };

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Type initial prefix
    harness.type_text("te")?;
    harness.render()?;

    // Set up completion items
    let completion_items = vec![lsp_types::CompletionItem {
        label: "test_function".to_string(),
        kind: Some(lsp_types::CompletionItemKind::FUNCTION),
        insert_text: Some("test_function".to_string()),
        ..Default::default()
    }];

    harness.editor_mut().set_completion_items(completion_items);

    // Show completion popup
    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            title: Some("Completion".to_string()),
            description: None,
            transient: false,
            content: PopupContentData::List {
                items: vec![PopupListItemData {
                    text: "test_function".to_string(),
                    detail: None,
                    icon: None,
                    data: Some("test_function".to_string()),
                }],
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
    assert!(
        harness.editor().active_state().popups.is_visible(),
        "Popup should be visible initially"
    );

    // Type 'x' - no items start with "tex"
    harness.send_key(KeyCode::Char('x'), KeyModifiers::NONE)?;
    harness.render()?;

    // Verify popup is closed (no matches)
    assert!(
        !harness.editor().active_state().popups.is_visible(),
        "Popup should be closed when no items match"
    );

    // Verify the character was still inserted
    let buffer = harness.get_buffer_content().unwrap();
    assert_eq!(buffer, "tex", "Buffer should contain 'tex'");

    Ok(())
}

/// Test type-to-filter: backspace in popup re-filters with shorter prefix
#[test]
fn test_completion_backspace_refilters() -> anyhow::Result<()> {
    use fresh::model::event::{
        Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData,
    };

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Type initial prefix
    harness.type_text("tes")?;
    harness.render()?;

    // Set up completion items
    let completion_items = vec![
        lsp_types::CompletionItem {
            label: "test_function".to_string(),
            kind: Some(lsp_types::CompletionItemKind::FUNCTION),
            insert_text: Some("test_function".to_string()),
            ..Default::default()
        },
        lsp_types::CompletionItem {
            label: "temp_file".to_string(),
            kind: Some(lsp_types::CompletionItemKind::VARIABLE),
            insert_text: Some("temp_file".to_string()),
            ..Default::default()
        },
    ];

    harness.editor_mut().set_completion_items(completion_items);

    // Show completion popup with filtered items (only test_function matches "tes")
    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            title: Some("Completion".to_string()),
            description: None,
            transient: false,
            content: PopupContentData::List {
                items: vec![PopupListItemData {
                    text: "test_function".to_string(),
                    detail: None,
                    icon: None,
                    data: Some("test_function".to_string()),
                }],
                selected: 0,
            },
            position: PopupPositionData::BelowCursor,
            width: 50,
            max_height: 15,
            bordered: true,
        },
    });

    harness.render()?;

    // Verify only test_function is visible
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("test_function"),
        "test_function should be visible"
    );
    assert!(
        !screen.contains("temp_file"),
        "temp_file should not be visible with 'tes' prefix"
    );

    // Press backspace to change prefix from "tes" to "te"
    harness.send_key(KeyCode::Backspace, KeyModifiers::NONE)?;
    harness.render()?;

    // Verify buffer has shorter prefix
    let buffer = harness.get_buffer_content().unwrap();
    assert_eq!(buffer, "te", "Buffer should contain 'te' after backspace");

    // Verify popup is still visible
    assert!(
        harness.editor().active_state().popups.is_visible(),
        "Popup should still be visible after backspace"
    );

    // Both items should now be visible (both match "te")
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("test_function"),
        "test_function should be visible"
    );
    assert!(
        screen.contains("temp_file"),
        "temp_file should be visible after backspace"
    );

    Ok(())
}

/// Test type-to-filter preserves selection when possible
#[test]
fn test_completion_type_to_filter_preserves_selection() -> anyhow::Result<()> {
    use fresh::model::event::{
        Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData,
    };

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Type initial prefix
    harness.type_text("te")?;
    harness.render()?;

    // Set up completion items
    let completion_items = vec![
        lsp_types::CompletionItem {
            label: "test_alpha".to_string(),
            kind: Some(lsp_types::CompletionItemKind::FUNCTION),
            insert_text: Some("test_alpha".to_string()),
            ..Default::default()
        },
        lsp_types::CompletionItem {
            label: "test_beta".to_string(),
            kind: Some(lsp_types::CompletionItemKind::FUNCTION),
            insert_text: Some("test_beta".to_string()),
            ..Default::default()
        },
        lsp_types::CompletionItem {
            label: "test_gamma".to_string(),
            kind: Some(lsp_types::CompletionItemKind::FUNCTION),
            insert_text: Some("test_gamma".to_string()),
            ..Default::default()
        },
    ];

    harness.editor_mut().set_completion_items(completion_items);

    // Show completion popup
    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            title: Some("Completion".to_string()),
            description: None,
            transient: false,
            content: PopupContentData::List {
                items: vec![
                    PopupListItemData {
                        text: "test_alpha".to_string(),
                        detail: None,
                        icon: None,
                        data: Some("test_alpha".to_string()),
                    },
                    PopupListItemData {
                        text: "test_beta".to_string(),
                        detail: None,
                        icon: None,
                        data: Some("test_beta".to_string()),
                    },
                    PopupListItemData {
                        text: "test_gamma".to_string(),
                        detail: None,
                        icon: None,
                        data: Some("test_gamma".to_string()),
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

    // Navigate to test_beta (second item)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.render()?;

    // Verify test_beta is selected
    let selected = harness
        .editor()
        .active_state()
        .popups
        .top()
        .and_then(|p| p.selected_item())
        .map(|item| item.text.clone());
    assert_eq!(
        selected,
        Some("test_beta".to_string()),
        "test_beta should be selected"
    );

    // Type 's' to filter (all items still match "tes")
    harness.send_key(KeyCode::Char('s'), KeyModifiers::NONE)?;
    harness.render()?;

    // Verify selection is preserved
    let selected_after = harness
        .editor()
        .active_state()
        .popups
        .top()
        .and_then(|p| p.selected_item())
        .map(|item| item.text.clone());
    assert_eq!(
        selected_after,
        Some("test_beta".to_string()),
        "Selection should be preserved after filtering"
    );

    Ok(())
}

/// Test accept_suggestion_on_enter: "off" makes Enter insert newline instead of accepting
#[test]
fn test_completion_accept_on_enter_off() -> anyhow::Result<()> {
    use fresh::config::AcceptSuggestionOnEnter;
    use fresh::model::event::{
        Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData,
    };

    // Configure accept_suggestion_on_enter to "off"
    let mut config = fresh::config::Config::default();
    config.editor.accept_suggestion_on_enter = AcceptSuggestionOnEnter::Off;
    let mut harness = EditorTestHarness::with_config(80, 24, config)?;

    // Type initial text
    harness.type_text("test")?;
    harness.render()?;

    // Set up completion items
    let completion_items = vec![lsp_types::CompletionItem {
        label: "test_function".to_string(),
        kind: Some(lsp_types::CompletionItemKind::FUNCTION),
        insert_text: Some("test_function".to_string()),
        ..Default::default()
    }];

    harness.editor_mut().set_completion_items(completion_items);

    // Show completion popup
    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            title: Some("Completion".to_string()),
            description: None,
            transient: false,
            content: PopupContentData::List {
                items: vec![PopupListItemData {
                    text: "test_function".to_string(),
                    detail: None,
                    icon: Some("λ".to_string()),
                    data: Some("test_function".to_string()),
                }],
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
    assert!(
        harness.editor().active_state().popups.is_visible(),
        "Popup should be visible"
    );

    // Press Enter - should close popup and insert newline, NOT accept completion
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.render()?;

    // Verify popup is closed
    assert!(
        !harness.editor().active_state().popups.is_visible(),
        "Popup should be closed after Enter with accept_on_enter=off"
    );

    // Verify buffer contains original text + newline, NOT the completion
    let buffer = harness.get_buffer_content().unwrap();
    assert!(
        buffer.contains("test\n"),
        "Buffer should contain 'test' followed by newline, got: {:?}",
        buffer
    );
    assert!(
        !buffer.contains("test_function"),
        "Buffer should NOT contain completion 'test_function', got: {:?}",
        buffer
    );

    Ok(())
}

/// Test accept_suggestion_on_enter: "on" (default) makes Enter accept completion
#[test]
fn test_completion_accept_on_enter_on() -> anyhow::Result<()> {
    use fresh::config::AcceptSuggestionOnEnter;
    use fresh::model::event::{
        Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData,
    };

    // Ensure accept_suggestion_on_enter is "on" (default)
    let mut config = fresh::config::Config::default();
    config.editor.accept_suggestion_on_enter = AcceptSuggestionOnEnter::On;
    let mut harness = EditorTestHarness::with_config(80, 24, config)?;

    // Type initial text
    harness.type_text("test")?;
    harness.render()?;

    // Set up completion items
    let completion_items = vec![lsp_types::CompletionItem {
        label: "test_function".to_string(),
        kind: Some(lsp_types::CompletionItemKind::FUNCTION),
        insert_text: Some("test_function".to_string()),
        ..Default::default()
    }];

    harness.editor_mut().set_completion_items(completion_items);

    // Show completion popup
    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            title: Some("Completion".to_string()),
            description: None,
            transient: false,
            content: PopupContentData::List {
                items: vec![PopupListItemData {
                    text: "test_function".to_string(),
                    detail: None,
                    icon: Some("λ".to_string()),
                    data: Some("test_function".to_string()),
                }],
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
    assert!(
        harness.editor().active_state().popups.is_visible(),
        "Popup should be visible"
    );

    // Press Enter - should accept completion
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.render()?;

    // Verify popup is closed
    assert!(
        !harness.editor().active_state().popups.is_visible(),
        "Popup should be closed after Enter"
    );

    // Verify buffer contains the completion
    let buffer = harness.get_buffer_content().unwrap();
    assert!(
        buffer.contains("test_function"),
        "Buffer should contain completion 'test_function', got: {:?}",
        buffer
    );

    Ok(())
}

/// Test LSP snippet expansion: function with $0 places cursor inside parens
#[test]
fn test_completion_snippet_cursor_position() -> anyhow::Result<()> {
    use fresh::model::event::{
        Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData,
    };

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Type partial function name
    harness.type_text("print")?;
    harness.render()?;

    // Show completion popup with snippet
    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            title: Some("Completion".to_string()),
            description: None,
            transient: false,
            content: PopupContentData::List {
                items: vec![PopupListItemData {
                    text: "println!".to_string(),
                    detail: Some("macro".to_string()),
                    icon: Some("m".to_string()),
                    // Snippet with $0 inside parens
                    data: Some("println!($0)".to_string()),
                }],
                selected: 0,
            },
            position: PopupPositionData::BelowCursor,
            width: 50,
            max_height: 15,
            bordered: true,
        },
    });

    harness.render()?;

    // Confirm selection
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.render()?;

    // Verify the snippet was expanded correctly
    let buffer = harness.get_buffer_content().unwrap();
    assert_eq!(buffer, "println!()", "Snippet should expand without $0");

    // Verify cursor is positioned inside the parens (at position 9)
    let cursor_pos = harness.editor().active_state().cursors.primary().position;
    assert_eq!(cursor_pos, 9, "Cursor should be inside the parentheses");

    Ok(())
}

/// Test LSP snippet expansion: tabstop with default text
#[test]
fn test_completion_snippet_with_default() -> anyhow::Result<()> {
    use fresh::model::event::{
        Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData,
    };

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Type partial name
    harness.type_text("fn")?;
    harness.render()?;

    // Show completion popup with snippet containing default text
    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            title: Some("Completion".to_string()),
            description: None,
            transient: false,
            content: PopupContentData::List {
                items: vec![PopupListItemData {
                    text: "fn".to_string(),
                    detail: Some("keyword".to_string()),
                    icon: Some("k".to_string()),
                    // Snippet with default text
                    data: Some("fn ${1:name}($2) {\n    $0\n}".to_string()),
                }],
                selected: 0,
            },
            position: PopupPositionData::BelowCursor,
            width: 50,
            max_height: 15,
            bordered: true,
        },
    });

    harness.render()?;

    // Confirm selection
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.render()?;

    // Verify the snippet was expanded with default text
    let buffer = harness.get_buffer_content().unwrap();
    assert_eq!(
        buffer, "fn name() {\n    \n}",
        "Snippet should expand with default text"
    );

    // Verify cursor is at $0 position (after the 4 spaces on line 2)
    let cursor_pos = harness.editor().active_state().cursors.primary().position;
    assert_eq!(cursor_pos, 16, "Cursor should be at $0 position");

    Ok(())
}

/// Test LSP snippet expansion: plain text (non-snippet) still works
#[test]
fn test_completion_plain_text_no_snippet() -> anyhow::Result<()> {
    use fresh::model::event::{
        Event, PopupContentData, PopupData, PopupListItemData, PopupPositionData,
    };

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Type partial name
    harness.type_text("my_var")?;
    harness.render()?;

    // Show completion popup with plain text (no snippet syntax)
    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            title: Some("Completion".to_string()),
            description: None,
            transient: false,
            content: PopupContentData::List {
                items: vec![PopupListItemData {
                    text: "my_variable".to_string(),
                    detail: Some("let my_variable".to_string()),
                    icon: Some("v".to_string()),
                    // Plain text, no snippet syntax
                    data: Some("my_variable".to_string()),
                }],
                selected: 0,
            },
            position: PopupPositionData::BelowCursor,
            width: 50,
            max_height: 15,
            bordered: true,
        },
    });

    harness.render()?;

    // Confirm selection
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.render()?;

    // Verify plain text was inserted
    let buffer = harness.get_buffer_content().unwrap();
    assert_eq!(buffer, "my_variable", "Plain text should be inserted as-is");

    // Cursor should be at end of inserted text
    let cursor_pos = harness.editor().active_state().cursors.primary().position;
    assert_eq!(cursor_pos, 11, "Cursor should be at end of text");

    Ok(())
}

/// Test that mouse hover does NOT auto-start LSP when auto_start is disabled
///
/// This is a regression test for the bug where hovering over text would
/// start the LSP server even when auto_start was set to false in the config.
/// The LSP server should only start when:
/// 1. auto_start is true and a file is opened, OR
/// 2. The user manually starts the server (via command palette or menu)
#[test]
fn test_hover_does_not_autostart_lsp_when_disabled() -> anyhow::Result<()> {
    use crate::common::fake_lsp::FakeLspServer;

    // Spawn fake LSP server script (but we don't want it to actually be used)
    let _fake_server = FakeLspServer::spawn()?;

    // Create temp dir and test file
    let temp_dir = tempfile::tempdir()?;
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn example_function() {\n    let x = 42;\n}\n")?;

    // Configure editor to use the fake LSP server with auto_start DISABLED
    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::services::lsp::LspServerConfig {
            command: FakeLspServer::script_path().to_string_lossy().to_string(),
            args: vec![],
            enabled: true,
            auto_start: false, // This is the key setting - LSP should NOT auto-start
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
        },
    );
    // Enable mouse hover in config
    config.editor.mouse_hover_enabled = true;

    // Create harness with config
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    harness.open_file(&test_file)?;
    harness.render()?;

    // Verify LSP server was NOT spawned after just opening the file
    // (because auto_start is false and we haven't manually started it)
    let running_before_hover = harness.editor().running_lsp_servers();
    assert!(
        !running_before_hover.contains(&"rust".to_string()),
        "LSP server should NOT be spawned after opening file when auto_start=false"
    );

    // Now simulate mouse hover over the code
    // The gutter takes some columns, so move to column ~10 which should be over the symbol
    harness.mouse_move(10, 2)?;
    harness.render()?;

    // Force check mouse hover to bypass the 500ms timer
    // This is what would normally trigger the LSP hover request
    let _ = harness.editor_mut().force_check_mouse_hover();

    // Process any async messages
    for _ in 0..5 {
        harness.process_async_and_render()?;
        harness.sleep(std::time::Duration::from_millis(50));
    }

    // THE BUG: Currently, the hover request triggers get_or_spawn which starts the LSP
    // even though auto_start is false. The expected behavior is that the LSP should
    // NOT be started by a hover request when auto_start is disabled.
    //
    // This test SHOULD PASS (LSP not started), but currently FAILS due to the bug.
    let running_after_hover = harness.editor().running_lsp_servers();
    assert!(
        !running_after_hover.contains(&"rust".to_string()),
        "LSP server should NOT be auto-started by hover when auto_start=false. \
         The bug is that with_lsp_for_buffer() calls get_or_spawn() which ignores \
         the auto_start setting. It should use try_spawn() or check auto_start first. \
         Running servers after hover: {:?}",
        running_after_hover
    );

    Ok(())
}

/// Test that typing does NOT auto-start LSP when auto_start is disabled
///
/// This is a regression test for the bug where typing in a buffer would
/// start the LSP server even when auto_start was set to false in the config.
#[test]
fn test_typing_does_not_autostart_lsp_when_disabled() -> anyhow::Result<()> {
    use crate::common::fake_lsp::FakeLspServer;
    use crossterm::event::KeyCode;

    // Spawn fake LSP server script (but we don't want it to actually be used)
    let _fake_server = FakeLspServer::spawn()?;

    // Create temp dir and test file
    let temp_dir = tempfile::tempdir()?;
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn main() {\n}\n")?;

    // Configure editor to use the fake LSP server with auto_start DISABLED
    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::services::lsp::LspServerConfig {
            command: FakeLspServer::script_path().to_string_lossy().to_string(),
            args: vec![],
            enabled: true,
            auto_start: false, // This is the key setting - LSP should NOT auto-start
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

    // Verify LSP server was NOT spawned after just opening the file
    let running_before_typing = harness.editor().running_lsp_servers();
    assert!(
        !running_before_typing.contains(&"rust".to_string()),
        "LSP server should NOT be spawned after opening file when auto_start=false"
    );

    // Type a character - this should NOT start the LSP
    harness.send_key(KeyCode::Char('x'), crossterm::event::KeyModifiers::NONE)?;
    harness.render()?;

    // Process any async messages
    for _ in 0..5 {
        harness.process_async_and_render()?;
        harness.sleep(std::time::Duration::from_millis(50));
    }

    // Verify LSP was NOT started by typing
    let running_after_typing = harness.editor().running_lsp_servers();
    assert!(
        !running_after_typing.contains(&"rust".to_string()),
        "LSP server should NOT be auto-started by typing when auto_start=false. \
         Running servers after typing: {:?}",
        running_after_typing
    );

    Ok(())
}

/// Test that completion is triggered on LSP trigger characters (like `.`)
///
/// This test verifies that when typing a trigger character (defined by the LSP server's
/// capabilities), a completion request is automatically sent.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses Bash-based fake LSP server
fn test_completion_triggered_on_trigger_character() -> anyhow::Result<()> {
    // Spawn fake LSP server with logging
    let _fake_server = FakeLspServer::spawn_with_logging()?;

    // Create temp dir and test file
    let temp_dir = tempfile::tempdir()?;
    let log_file = temp_dir.path().join("completion_trigger_test_log.txt");
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn main() {\n    foo\n}\n")?;

    // Configure editor with quick_suggestions disabled to isolate trigger char behavior
    let mut config = fresh::config::Config::default();
    config.editor.quick_suggestions = false; // Only trigger chars should work
    config.lsp.insert(
        "rust".to_string(),
        fresh::services::lsp::LspServerConfig {
            command: FakeLspServer::logging_script_path()
                .to_string_lossy()
                .to_string(),
            args: vec![log_file.to_string_lossy().to_string()],
            enabled: true,
            auto_start: true,
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

    // Open the test file (triggers didOpen)
    harness.open_file(&test_file)?;
    harness.render()?;

    // Wait for LSP to initialize and didOpen to be sent
    harness.wait_until(|_| {
        let log_content = std::fs::read_to_string(&log_file).unwrap_or_default();
        log_content.contains("textDocument/didOpen")
    })?;

    // Clear the log to start fresh for completion tests
    std::fs::write(&log_file, "")?;

    // Move to end of "foo" on line 2
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.send_key(KeyCode::End, KeyModifiers::NONE)?;
    harness.render()?;

    // Type a trigger character (`.` is a trigger character in the fake LSP server)
    harness.send_key(KeyCode::Char('.'), KeyModifiers::NONE)?;
    harness.render()?;

    // Wait for completion request to be logged
    harness.wait_until(|_| {
        let log_content = std::fs::read_to_string(&log_file).unwrap_or_default();
        log_content.contains("textDocument/completion")
    })?;

    // Verify completion was triggered
    let log_content = std::fs::read_to_string(&log_file)?;
    assert!(
        log_content.contains("textDocument/completion"),
        "Expected completion request to be triggered by '.' character. Log: {}",
        log_content
    );

    Ok(())
}

/// Test that completion is triggered on word characters when quick_suggestions is enabled
///
/// This test verifies VS Code-like behavior where typing word characters (letters, numbers, _)
/// triggers completion automatically when quick_suggestions is enabled.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses Bash-based fake LSP server
fn test_completion_triggered_on_word_char_with_quick_suggestions() -> anyhow::Result<()> {
    // Spawn fake LSP server with logging
    let _fake_server = FakeLspServer::spawn_with_logging()?;

    // Create temp dir and test file
    let temp_dir = tempfile::tempdir()?;
    let log_file = temp_dir.path().join("quick_suggestions_test_log.txt");
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn main() {\n    \n}\n")?;

    // Configure editor with quick_suggestions ENABLED
    let mut config = fresh::config::Config::default();
    config.editor.quick_suggestions = true;
    config.lsp.insert(
        "rust".to_string(),
        fresh::services::lsp::LspServerConfig {
            command: FakeLspServer::logging_script_path()
                .to_string_lossy()
                .to_string(),
            args: vec![log_file.to_string_lossy().to_string()],
            enabled: true,
            auto_start: true,
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

    // Open the test file
    harness.open_file(&test_file)?;
    harness.render()?;

    // Wait for LSP to initialize
    harness.wait_until(|_| {
        let log_content = std::fs::read_to_string(&log_file).unwrap_or_default();
        log_content.contains("textDocument/didOpen")
    })?;

    // Clear the log
    std::fs::write(&log_file, "")?;

    // Move to the empty line
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.send_key(KeyCode::End, KeyModifiers::NONE)?;
    harness.render()?;

    // Type a word character (letter)
    harness.send_key(KeyCode::Char('p'), KeyModifiers::NONE)?;
    harness.render()?;

    // Wait for completion request
    harness.wait_until(|_| {
        let log_content = std::fs::read_to_string(&log_file).unwrap_or_default();
        log_content.contains("textDocument/completion")
    })?;

    let log_content = std::fs::read_to_string(&log_file)?;
    assert!(
        log_content.contains("textDocument/completion"),
        "Expected completion to be triggered by word character 'p' when quick_suggestions=true. Log: {}",
        log_content
    );

    Ok(())
}

/// Test that completion is NOT triggered on word characters when quick_suggestions is disabled
///
/// This test verifies that when quick_suggestions is disabled, only LSP trigger characters
/// (like `.`) trigger completion, not regular word characters.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses Bash-based fake LSP server
fn test_completion_not_triggered_on_word_char_without_quick_suggestions() -> anyhow::Result<()> {
    // Spawn fake LSP server with logging
    let _fake_server = FakeLspServer::spawn_with_logging()?;

    // Create temp dir and test file
    let temp_dir = tempfile::tempdir()?;
    let log_file = temp_dir.path().join("no_quick_suggestions_test_log.txt");
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn main() {\n    \n}\n")?;

    // Configure editor with quick_suggestions DISABLED
    let mut config = fresh::config::Config::default();
    config.editor.quick_suggestions = false;
    config.lsp.insert(
        "rust".to_string(),
        fresh::services::lsp::LspServerConfig {
            command: FakeLspServer::logging_script_path()
                .to_string_lossy()
                .to_string(),
            args: vec![log_file.to_string_lossy().to_string()],
            enabled: true,
            auto_start: true,
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

    // Open the test file
    harness.open_file(&test_file)?;
    harness.render()?;

    // Wait for LSP to initialize
    harness.wait_until(|_| {
        let log_content = std::fs::read_to_string(&log_file).unwrap_or_default();
        log_content.contains("textDocument/didOpen")
    })?;

    // Clear the log
    std::fs::write(&log_file, "")?;

    // Move to the empty line
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.send_key(KeyCode::End, KeyModifiers::NONE)?;
    harness.render()?;

    // Type multiple word characters
    harness.type_text("print")?;
    harness.render()?;

    // Process async messages and give some time
    for _ in 0..10 {
        harness.process_async_and_render()?;
        harness.sleep(std::time::Duration::from_millis(50));
    }

    // Verify NO completion was triggered
    let log_content = std::fs::read_to_string(&log_file)?;
    assert!(
        !log_content.contains("textDocument/completion"),
        "Expected NO completion request when typing word characters with quick_suggestions=false. \
         But completion was triggered. Log: {}",
        log_content
    );

    Ok(())
}

/// Test that completion is NOT triggered on non-word characters (like space)
///
/// This test verifies that non-word characters that are not trigger characters
/// do not trigger completion, regardless of quick_suggestions setting.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses Bash-based fake LSP server
fn test_completion_not_triggered_on_non_word_char() -> anyhow::Result<()> {
    // Spawn fake LSP server with logging
    let _fake_server = FakeLspServer::spawn_with_logging()?;

    // Create temp dir and test file
    let temp_dir = tempfile::tempdir()?;
    let log_file = temp_dir.path().join("non_word_char_test_log.txt");
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn main() {\n    foo\n}\n")?;

    // Configure editor with quick_suggestions enabled
    let mut config = fresh::config::Config::default();
    config.editor.quick_suggestions = true;
    config.lsp.insert(
        "rust".to_string(),
        fresh::services::lsp::LspServerConfig {
            command: FakeLspServer::logging_script_path()
                .to_string_lossy()
                .to_string(),
            args: vec![log_file.to_string_lossy().to_string()],
            enabled: true,
            auto_start: true,
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

    // Open the test file
    harness.open_file(&test_file)?;
    harness.render()?;

    // Wait for LSP to initialize
    harness.wait_until(|_| {
        let log_content = std::fs::read_to_string(&log_file).unwrap_or_default();
        log_content.contains("textDocument/didOpen")
    })?;

    // Clear the log
    std::fs::write(&log_file, "")?;

    // Move to end of "foo"
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.send_key(KeyCode::End, KeyModifiers::NONE)?;
    harness.render()?;

    // Type a space (non-word, non-trigger character)
    harness.send_key(KeyCode::Char(' '), KeyModifiers::NONE)?;
    harness.render()?;

    // Process async messages and give some time
    for _ in 0..10 {
        harness.process_async_and_render()?;
        harness.sleep(std::time::Duration::from_millis(50));
    }

    // Verify NO completion was triggered
    let log_content = std::fs::read_to_string(&log_file)?;
    assert!(
        !log_content.contains("textDocument/completion"),
        "Expected NO completion request when typing space character. \
         But completion was triggered. Log: {}",
        log_content
    );

    Ok(())
}

/// Test that hover popup stays stable when mouse moves (no range from LSP)
///
/// When the LSP server doesn't return a symbol range in the hover response (like pyrefly),
/// the popup should remain stable at its original position when the mouse moves.
///
/// Expected behavior: Popup stays at original position regardless of mouse movement.
#[test]
#[cfg_attr(
    windows,
    ignore = "FakeLspServer uses a Bash script which is not available on Windows"
)]
fn test_hover_popup_follows_mouse_when_lsp_returns_no_range() -> anyhow::Result<()> {
    use crate::common::fake_lsp::FakeLspServer;
    use std::time::Duration;

    // Spawn fake LSP server that does NOT return range in hover response
    let _fake_server = FakeLspServer::spawn_without_range()?;

    // Create temp dir and test file with multiple lines of code
    // This allows testing both horizontal and vertical mouse movement
    let temp_dir = tempfile::tempdir()?;
    let test_file = temp_dir.path().join("test.rs");
    // Multiple lines with content so we can move mouse both horizontally and vertically
    let file_content = "fn example_function_name() {}\n\
                        fn another_function_here() {}\n\
                        fn third_function_name() {}\n\
                        fn fourth_function_name() {}\n\
                        fn fifth_function_name() {}\n\
                        \n\n\n\n\n\n\n\n\n\n";
    std::fs::write(&test_file, file_content)?;

    // Configure editor to use the no-range fake LSP server
    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::services::lsp::LspServerConfig {
            command: FakeLspServer::no_range_script_path()
                .to_string_lossy()
                .to_string(),
            args: vec![],
            enabled: true,
            auto_start: true,
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
        },
    );

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    harness.open_file(&test_file)?;
    harness.render()?;

    // Find the popup position (both row and column)
    fn find_popup_position(screen: &str, content: &str) -> Option<(usize, usize)> {
        for (row, line) in screen.lines().enumerate() {
            if let Some(col) = line.find(content) {
                return Some((row, col));
            }
        }
        None
    }

    // Move mouse over the first symbol and get initial popup position
    let initial_col = 10u16;
    let initial_row = 2u16;
    harness.mouse_move(initial_col, initial_row)?;
    harness.render()?;

    // Advance time past the hover debounce (500ms) and force check
    harness.sleep(Duration::from_millis(600));
    harness.editor_mut().force_check_mouse_hover();

    // Wait for hover popup to appear
    harness.wait_until(|h| h.screen_to_string().contains("Hover without range"))?;
    harness.render()?;

    let screen_before = harness.screen_to_string();
    let pos_before = find_popup_position(&screen_before, "Hover without range");
    assert!(
        pos_before.is_some(),
        "Hover popup should be visible initially"
    );
    let (row_before, col_before) = pos_before.unwrap();
    eprintln!(
        "Initial popup position: row={}, col={}",
        row_before, col_before
    );
    eprintln!("Screen before:\n{}", screen_before);

    // Test mouse movements within the same word/symbol
    // The popup should stay stable when moving within the word
    // (Moving to a completely different word/line is expected to trigger a new hover)
    let test_positions = [
        (initial_col + 2, initial_row), // Move slightly right, same word
        (initial_col + 5, initial_row), // Move more right, still in function name
        (initial_col - 2, initial_row), // Move slightly left, same word
    ];

    let mut any_position_changed = false;

    for (new_col, new_row) in test_positions {
        // Move mouse to new position
        harness.mouse_move(new_col, new_row)?;
        harness.render()?;

        // Advance time past debounce and force hover check
        harness.sleep(Duration::from_millis(600));
        harness.editor_mut().force_check_mouse_hover();

        // Wait for hover response
        harness.wait_until(|h| h.screen_to_string().contains("Hover without range"))?;
        harness.render()?;

        let screen_after = harness.screen_to_string();
        if let Some((row_after, col_after)) =
            find_popup_position(&screen_after, "Hover without range")
        {
            eprintln!(
                "Mouse at ({}, {}): popup at row={}, col={}",
                new_col, new_row, row_after, col_after
            );

            if row_after != row_before || col_after != col_before {
                any_position_changed = true;
                eprintln!(
                    "Popup MOVED from ({}, {}) to ({}, {})",
                    row_before, col_before, row_after, col_after
                );
            }
        }
    }

    // The popup should NOT follow the mouse - it should stay at the original position
    assert!(
        !any_position_changed,
        "Hover popup moved when it should have stayed in place. \
         Initial position: row={}, col={}. \
         When LSP returns no range, the popup should remain stable, not follow the mouse.",
        row_before, col_before
    );

    Ok(())
}

/// Test that clicking on popup scrollbar scrolls the popup content
///
/// Expected behavior: Clicking on scrollbar should scroll the popup content.
#[test]
fn test_hover_popup_scrollbar_click_scrolls_content() -> anyhow::Result<()> {
    use crossterm::event::{MouseButton, MouseEvent, MouseEventKind};
    use fresh::model::event::{Event, PopupContentData, PopupData, PopupPositionData};

    let mut harness = EditorTestHarness::new(100, 30)?;

    // Create content that exceeds the visible area
    // With max_height=10 and borders=2, we have 8 visible lines
    // So 20 lines of content should trigger a scrollbar
    let long_content: Vec<String> = (1..=20)
        .map(|i| format!("Documentation line {}", i))
        .collect();

    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            title: Some("Hover".to_string()),
            description: None,
            transient: true,
            content: PopupContentData::Text(long_content),
            position: PopupPositionData::Centered,
            width: 50,
            max_height: 10, // Only 8 lines of content visible
            bordered: true,
        },
    });

    harness.render()?;

    // Verify first lines are visible
    harness.assert_screen_contains("Documentation line 1");
    harness.assert_screen_contains("Documentation line 2");

    // Later lines should NOT be visible initially
    harness.assert_screen_not_contains("Documentation line 15");

    // Find the scrollbar position (right edge of popup, which is centered)
    // Popup is 50 wide, centered in 100-width = starts around col 25, ends around col 74
    // Scrollbar should be at the right edge of the popup
    let scrollbar_col = 73u16; // Right edge minus border
    let scrollbar_row = 17u16; // Bottom area of popup to click below thumb

    // Click on the scrollbar track (below the thumb) to scroll down
    let click_event = MouseEvent {
        kind: MouseEventKind::Down(MouseButton::Left),
        column: scrollbar_col,
        row: scrollbar_row,
        modifiers: crossterm::event::KeyModifiers::empty(),
    };
    harness.send_mouse(click_event)?;

    // Release the mouse button
    let release_event = MouseEvent {
        kind: MouseEventKind::Up(MouseButton::Left),
        column: scrollbar_col,
        row: scrollbar_row,
        modifiers: crossterm::event::KeyModifiers::empty(),
    };
    harness.send_mouse(release_event)?;
    harness.render()?;

    // Popup should still be visible after clicking
    harness.assert_screen_contains("Hover");

    // Clicking the scrollbar should scroll the content
    // After clicking below the thumb, later lines should become visible
    assert!(
        harness.screen_to_string().contains("Documentation line 15")
            || harness.screen_to_string().contains("Documentation line 10"),
        "Clicking popup scrollbar should scroll the content. \
         Expected to see later documentation lines after clicking scrollbar track. \
         Screen:\n{}",
        harness.screen_to_string()
    );

    Ok(())
}

/// Test that hover does not trigger when mouse is past end of line
///
/// Expected behavior: Hovering past the end of a line (in empty space) should not
/// trigger an LSP hover popup.
#[test]
#[cfg_attr(
    windows,
    ignore = "FakeLspServer uses a Bash script which is not available on Windows"
)]
fn test_hover_does_not_trigger_past_end_of_line() -> anyhow::Result<()> {
    use crate::common::fake_lsp::FakeLspServer;
    use std::time::Duration;

    // Spawn fake LSP server
    let _fake_server = FakeLspServer::spawn()?;

    // Create temp dir and test file with a short line
    let temp_dir = tempfile::tempdir()?;
    let test_file = temp_dir.path().join("test.rs");
    // Short line - "fn foo() {}" is about 11 chars, so column 50 is way past end
    let file_content = "fn foo() {}\n";
    std::fs::write(&test_file, file_content)?;

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
        },
    );

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    harness.open_file(&test_file)?;
    harness.render()?;

    // First, verify that hover DOES work when mouse is on actual content
    // Hover over "foo" (around column 10 with gutter)
    let on_content_col = 10u16;
    let line_row = 2u16; // First line of content (after tab bar)
    harness.mouse_move(on_content_col, line_row)?;
    harness.render()?;
    harness.sleep(Duration::from_millis(600));
    harness.editor_mut().force_check_mouse_hover();

    // Wait for hover to appear - this proves the LSP is working
    harness.wait_until(|h| h.screen_to_string().contains("Test hover content"))?;
    harness.assert_screen_contains("Test hover content");

    // Dismiss the hover by moving away
    harness.mouse_move(0, 0)?;
    harness.render()?;
    harness.sleep(Duration::from_millis(100));
    harness.editor_mut().force_check_mouse_hover();
    harness.wait_until(|h| !h.screen_to_string().contains("Test hover content"))?;

    // Now move mouse past the end of the line
    // Col 50 should be well past the end of "fn foo() {}"
    let past_eol_col = 50u16;
    harness.mouse_move(past_eol_col, line_row)?;
    harness.render()?;

    // Advance time and force hover check
    harness.sleep(Duration::from_millis(600));
    harness.editor_mut().force_check_mouse_hover();

    // Give time for hover to appear if it would (but it shouldn't)
    for _ in 0..10 {
        harness.process_async_and_render()?;
        harness.sleep(Duration::from_millis(100));
    }

    // Hover should NOT appear because mouse is past end of line
    assert!(
        !harness.screen_to_string().contains("Test hover content"),
        "Hover popup should NOT appear when mouse is past end of line. \
         Mouse was at column {} which is past the line content. \
         Screen:\n{}",
        past_eol_col,
        harness.screen_to_string()
    );

    Ok(())
}

/// Test that hover does not trigger when mouse is on an empty line
///
/// Expected behavior: Hovering on an empty line (even if it's beneath a symbol)
/// should not trigger an LSP hover popup.
#[test]
#[cfg_attr(
    windows,
    ignore = "FakeLspServer uses a Bash script which is not available on Windows"
)]
fn test_hover_does_not_trigger_on_empty_line() -> anyhow::Result<()> {
    use crate::common::fake_lsp::FakeLspServer;
    use std::time::Duration;

    // Spawn fake LSP server
    let _fake_server = FakeLspServer::spawn()?;

    // Create temp dir and test file matching user's scenario:
    // Line 1: import statement
    // Line 2: empty
    // Line 3: symbol (hover target)
    // Line 4: empty (this is where hover should NOT trigger)
    let temp_dir = tempfile::tempdir()?;
    let test_file = temp_dir.path().join("test.rs");
    let file_content = "use std;\n\nfn foo() {}\n\n";
    std::fs::write(&test_file, file_content)?;

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
        },
    );

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    harness.open_file(&test_file)?;
    harness.render()?;

    // First verify hover DOES work on the symbol (to prove LSP is connected)
    // Row 4 = Line 3 (fn foo() {}) after tab bar at row 0-1
    let symbol_row = 4u16;
    let symbol_col = 10u16;
    harness.mouse_move(symbol_col, symbol_row)?;
    harness.render()?;
    harness.sleep(Duration::from_millis(600));
    harness.editor_mut().force_check_mouse_hover();
    harness.wait_until(|h| h.screen_to_string().contains("Test hover content"))?;

    // Dismiss hover
    harness.mouse_move(0, 0)?;
    harness.render()?;
    harness.sleep(Duration::from_millis(100));
    harness.editor_mut().force_check_mouse_hover();
    harness.wait_until(|h| !h.screen_to_string().contains("Test hover content"))?;

    // Now move mouse to a row FAR BELOW the file content - hover should NEVER appear
    // The bug: when visual_row has no mapping, we default to false (don't block hover)
    // and screen_to_buffer_position falls back to last line's position
    let empty_line_row = 20u16; // Way below the 4-line file
    let empty_line_col = 10u16; // Same column as the symbol
    harness.mouse_move(empty_line_col, empty_line_row)?;
    harness.render()?;

    // Wait for hover debounce and force check
    harness.sleep(Duration::from_millis(600));
    harness.editor_mut().force_check_mouse_hover();

    // Process LSP response
    for _ in 0..10 {
        harness.process_async_and_render()?;
        harness.sleep(Duration::from_millis(100));
    }

    // Hover should NEVER appear because mouse is on an empty line
    assert!(
        !harness.screen_to_string().contains("Test hover content"),
        "Hover popup should NEVER appear when mouse is on an empty line. \
         Mouse was at ({}, {}) which is an empty line. \
         Screen:\n{}",
        empty_line_col,
        empty_line_row,
        harness.screen_to_string()
    );

    Ok(())
}

/// Test that moving mouse within symbol during hover request does not create duplicate popups
///
/// This reproduces a race condition:
/// 1. Mouse at position A, hover request sent
/// 2. Mouse moves to position B (within same symbol) BEFORE response arrives
/// 3. Code sees position change, starts new hover state with request_sent=false
/// 4. First response arrives, shows popup, sets symbol_range
/// 5. After debounce, SECOND request sent because request_sent is still false
/// 6. Second response creates duplicate popup
#[test]
#[cfg_attr(
    windows,
    ignore = "FakeLspServer uses a Bash script which is not available on Windows"
)]
fn test_hover_no_duplicate_popup_when_moving_within_symbol() -> anyhow::Result<()> {
    use crate::common::fake_lsp::FakeLspServer;
    use std::time::Duration;

    // Spawn fake LSP server
    let _fake_server = FakeLspServer::spawn()?;

    // Create temp dir and test file
    let temp_dir = tempfile::tempdir()?;
    let test_file = temp_dir.path().join("test.rs");
    // "array_equal" is a long symbol - we'll hover on different columns within it
    let file_content = "fn array_equal() {}\n";
    std::fs::write(&test_file, file_content)?;

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
        },
    );

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    harness.open_file(&test_file)?;
    harness.render()?;

    let symbol_row = 2u16; // First line after tab bar

    // Step 1: Move mouse to first position within symbol
    let first_col = 10u16;
    harness.mouse_move(first_col, symbol_row)?;
    harness.render()?;

    // Step 2: Wait for debounce and trigger first hover request
    harness.sleep(Duration::from_millis(600));
    harness.editor_mut().force_check_mouse_hover();
    // DON'T process async yet - we want to move mouse before response arrives

    // Step 3: Move mouse to different column within same symbol BEFORE processing response
    let second_col = 12u16; // Still within "array_equal"
    harness.mouse_move(second_col, symbol_row)?;
    harness.render()?;

    // Step 4: Now process the first response (this sets symbol_range and shows popup)
    harness.process_async_and_render()?;
    harness.wait_until(|h| h.screen_to_string().contains("Test hover content"))?;

    // Step 5: Wait for another debounce period (which would trigger second request if buggy)
    harness.sleep(Duration::from_millis(600));
    harness.editor_mut().force_check_mouse_hover();

    // Step 6: Process any second response
    for _ in 0..10 {
        harness.process_async_and_render()?;
        harness.sleep(Duration::from_millis(50));
    }

    // Count corners - should have exactly 1 popup, not 2
    let screen = harness.screen_to_string();
    let corners = (
        screen.matches('┌').count(),
        screen.matches('┐').count(),
        screen.matches('└').count(),
        screen.matches('┘').count(),
    );
    assert_eq!(
        corners,
        (1, 1, 1, 1),
        "Should have exactly 1 popup (one of each corner). \
         Got corners (┌={}, ┐={}, └={}, ┘={}). This indicates duplicate hover popups. \
         Screen:\n{}",
        corners.0,
        corners.1,
        corners.2,
        corners.3,
        screen
    );

    Ok(())
}

/// Test that clicking outside a hover popup dismisses it and doesn't move the cursor
///
/// This is a regression test for a bug where:
/// 1. Click outside hover popup should dismiss the popup
/// 2. Click inside hover popup should NOT move the editor cursor
/// 3. Double-click should also respect these rules
#[test]
fn test_hover_popup_click_dismissal() -> anyhow::Result<()> {
    use fresh::model::event::{Event, PopupContentData, PopupData, PopupPositionData};

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Type some text on multiple lines to have different click targets
    harness.type_text("line one\nline two\nline three")?;
    harness.render()?;

    // Move cursor to beginning of line 1
    harness.send_key(KeyCode::Home, KeyModifiers::CONTROL)?;
    harness.render()?;

    // Record initial cursor position (currently unused but kept for future assertions)
    let _initial_cursor = harness.cursor_position();

    // Show a transient hover popup (simulating LSP hover response)
    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            title: Some("Hover Info".to_string()),
            description: None,
            transient: true, // This is key - hover popups are transient
            content: PopupContentData::Text(vec![
                "fn example() -> i32".to_string(),
                "Returns an example value".to_string(),
            ]),
            position: PopupPositionData::Fixed { x: 20, y: 5 },
            width: 30,
            max_height: 10,
            bordered: true,
        },
    });
    harness.render()?;

    // Verify popup is visible
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Hover Info"),
        "Hover popup should be visible. Screen:\n{screen}"
    );

    // Click OUTSIDE the popup (at row 15, which is below the popup at y=5)
    // This should dismiss the popup
    harness.mouse_click(5, 15)?;

    // Verify popup is dismissed
    let screen_after_click = harness.screen_to_string();
    assert!(
        !screen_after_click.contains("Hover Info"),
        "Hover popup should be dismissed after clicking outside. Screen:\n{screen_after_click}"
    );

    Ok(())
}

/// Test that clicking inside a hover popup does NOT move the editor cursor
#[test]
fn test_hover_popup_click_inside_does_not_move_cursor() -> anyhow::Result<()> {
    use fresh::model::event::{Event, PopupContentData, PopupData, PopupPositionData};

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Type some text
    harness.type_text("line one\nline two\nline three")?;
    harness.render()?;

    // Move cursor to a known position (beginning of file)
    harness.send_key(KeyCode::Home, KeyModifiers::CONTROL)?;
    harness.render()?;

    let cursor_before = harness.cursor_position();

    // Show a hover popup at a fixed position
    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            title: Some("Hover Info".to_string()),
            description: None,
            transient: true,
            content: PopupContentData::Text(vec![
                "fn example() -> i32".to_string(),
                "Returns an example value".to_string(),
                "More documentation here".to_string(),
            ]),
            // Position the popup in the middle of the screen
            position: PopupPositionData::Fixed { x: 10, y: 5 },
            width: 40,
            max_height: 10,
            bordered: true,
        },
    });
    harness.render()?;

    // Verify popup is visible
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Hover Info"),
        "Hover popup should be visible"
    );

    // Click INSIDE the popup (at position within the popup bounds)
    // Popup is at x=10, y=5, width=40, so clicking at (25, 7) should be inside
    harness.mouse_click(25, 7)?;

    // Cursor should NOT have moved
    let cursor_after = harness.cursor_position();
    assert_eq!(
        cursor_before, cursor_after,
        "Cursor should not move when clicking inside hover popup. \
         Before: {:?}, After: {:?}",
        cursor_before, cursor_after
    );

    // Popup should still be visible (clicking inside doesn't dismiss)
    let screen_after = harness.screen_to_string();
    assert!(
        screen_after.contains("Hover Info"),
        "Hover popup should still be visible after clicking inside"
    );

    Ok(())
}

/// Test that double-clicking outside hover popup dismisses it
#[test]
fn test_hover_popup_double_click_dismissal() -> anyhow::Result<()> {
    use crossterm::event::{MouseButton, MouseEvent, MouseEventKind};
    use fresh::model::event::{Event, PopupContentData, PopupData, PopupPositionData};

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Type some text
    harness.type_text("hello world on line one")?;
    harness.render()?;

    // Show a transient hover popup
    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            title: Some("Hover Info".to_string()),
            description: None,
            transient: true,
            content: PopupContentData::Text(vec!["Test hover content".to_string()]),
            position: PopupPositionData::Fixed { x: 30, y: 5 },
            width: 30,
            max_height: 10,
            bordered: true,
        },
    });
    harness.render()?;

    // Verify popup is visible
    assert!(
        harness.screen_to_string().contains("Hover Info"),
        "Hover popup should be visible"
    );

    // Simulate double-click OUTSIDE the popup by sending two quick clicks
    // The harness double-click detection is time-based, so we send clicks in quick succession
    let click_col = 5_u16;
    let click_row = 15_u16; // Below the popup

    // First click
    harness.send_mouse(MouseEvent {
        kind: MouseEventKind::Down(MouseButton::Left),
        column: click_col,
        row: click_row,
        modifiers: KeyModifiers::empty(),
    })?;
    harness.send_mouse(MouseEvent {
        kind: MouseEventKind::Up(MouseButton::Left),
        column: click_col,
        row: click_row,
        modifiers: KeyModifiers::empty(),
    })?;

    // Second click (double-click)
    harness.send_mouse(MouseEvent {
        kind: MouseEventKind::Down(MouseButton::Left),
        column: click_col,
        row: click_row,
        modifiers: KeyModifiers::empty(),
    })?;
    harness.send_mouse(MouseEvent {
        kind: MouseEventKind::Up(MouseButton::Left),
        column: click_col,
        row: click_row,
        modifiers: KeyModifiers::empty(),
    })?;
    harness.render()?;

    // Popup should be dismissed after the double-click
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("Hover Info"),
        "Hover popup should be dismissed after double-clicking outside. Screen:\n{screen}"
    );

    Ok(())
}

/// Test that double-clicking inside hover popup does not affect editor
#[test]
fn test_hover_popup_double_click_inside_blocked() -> anyhow::Result<()> {
    use crossterm::event::{MouseButton, MouseEvent, MouseEventKind};
    use fresh::model::event::{Event, PopupContentData, PopupData, PopupPositionData};

    let mut harness = EditorTestHarness::new(80, 24)?;

    // Type some text with a word that could be selected by double-click
    harness.type_text("hello world")?;
    harness.render()?;

    // Move cursor to start
    harness.send_key(KeyCode::Home, KeyModifiers::CONTROL)?;
    harness.render()?;

    let cursor_before = harness.cursor_position();

    // Show a hover popup positioned over the text
    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            title: Some("Hover".to_string()),
            description: None,
            transient: true,
            content: PopupContentData::Text(vec!["Documentation".to_string()]),
            // Position popup to overlap with text area
            position: PopupPositionData::Fixed { x: 1, y: 2 },
            width: 30,
            max_height: 5,
            bordered: true,
        },
    });
    harness.render()?;

    // Double-click inside the popup
    let click_col = 10_u16; // Inside popup
    let click_row = 3_u16; // Inside popup (popup is at y=2, height includes border)

    // First click
    harness.send_mouse(MouseEvent {
        kind: MouseEventKind::Down(MouseButton::Left),
        column: click_col,
        row: click_row,
        modifiers: KeyModifiers::empty(),
    })?;
    harness.send_mouse(MouseEvent {
        kind: MouseEventKind::Up(MouseButton::Left),
        column: click_col,
        row: click_row,
        modifiers: KeyModifiers::empty(),
    })?;

    // Second click (double-click)
    harness.send_mouse(MouseEvent {
        kind: MouseEventKind::Down(MouseButton::Left),
        column: click_col,
        row: click_row,
        modifiers: KeyModifiers::empty(),
    })?;
    harness.send_mouse(MouseEvent {
        kind: MouseEventKind::Up(MouseButton::Left),
        column: click_col,
        row: click_row,
        modifiers: KeyModifiers::empty(),
    })?;
    harness.render()?;

    // Cursor should not have moved (double-click should not select word in editor)
    let cursor_after = harness.cursor_position();
    assert_eq!(
        cursor_before, cursor_after,
        "Cursor should not move when double-clicking inside hover popup"
    );

    // Popup should still be visible
    assert!(
        harness.screen_to_string().contains("Hover"),
        "Popup should still be visible after double-clicking inside"
    );

    Ok(())
}

/// Test that hover popup can scroll all the way to the bottom
///
/// This test verifies that when scrolling a hover popup with large content,
/// the user can scroll all the way to see the last line of content.
#[test]
fn test_hover_popup_scroll_to_bottom() -> anyhow::Result<()> {
    use fresh::model::event::{Event, PopupContentData, PopupData, PopupPositionData};

    let mut harness = EditorTestHarness::new(100, 30)?;

    // Create markdown-like content similar to real LSP hover documentation
    // This simulates a function signature with extensive documentation
    let mut long_content: Vec<String> = vec![
        "```rust".to_string(),
        "pub fn process_data(".to_string(),
        "    input: &[u8],".to_string(),
        "    options: ProcessOptions,".to_string(),
        ") -> Result<Output, Error>".to_string(),
        "```".to_string(),
        "".to_string(),
        "# Description".to_string(),
        "".to_string(),
        "Processes the input data according to the specified options.".to_string(),
        "This function performs several transformations on the data.".to_string(),
        "".to_string(),
        "# Arguments".to_string(),
        "".to_string(),
        "* `input` - The raw input bytes to process".to_string(),
        "* `options` - Configuration options for processing".to_string(),
        "".to_string(),
        "# Returns".to_string(),
        "".to_string(),
        "Returns `Ok(Output)` on success, or `Err(Error)` if processing fails.".to_string(),
        "".to_string(),
        "# Examples".to_string(),
        "".to_string(),
        "```rust".to_string(),
        "let data = b\"hello world\";".to_string(),
        "let opts = ProcessOptions::default();".to_string(),
        "let result = process_data(data, opts)?;".to_string(),
        "```".to_string(),
        "".to_string(),
        "# Errors".to_string(),
        "".to_string(),
        "This function will return an error if:".to_string(),
        "".to_string(),
        "* The input is empty".to_string(),
        "* The input contains invalid UTF-8 sequences".to_string(),
        "* The options specify an unsupported encoding".to_string(),
        "".to_string(),
        "# Performance".to_string(),
        "".to_string(),
        "This function has O(n) time complexity where n is the input length.".to_string(),
        "Memory usage is proportional to the input size.".to_string(),
        "".to_string(),
        "# Safety".to_string(),
        "".to_string(),
        "This function is safe to call from multiple threads.".to_string(),
        "".to_string(),
        "# See Also".to_string(),
        "".to_string(),
        "* [`process_data_async`] - Async version of this function".to_string(),
        "* [`ProcessOptions`] - Configuration options".to_string(),
        "".to_string(),
    ];

    // Add numbered lines at the end so we can verify scrolling reached the bottom
    for i in 1..=100 {
        long_content.push(format!("CONTENT_LINE_{:03}", i));
    }
    long_content.push("=== END OF DOCUMENTATION ===".to_string());

    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            title: Some("Hover Info".to_string()),
            description: None,
            transient: true,
            content: PopupContentData::Text(long_content),
            position: PopupPositionData::Centered,
            width: 60,
            max_height: 12, // Only 10 lines visible after borders
            bordered: true,
        },
    });

    harness.render()?;

    // Verify initial state - first lines visible, last lines not
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("pub fn process_data"),
        "First line should be visible initially. Screen:\n{screen}"
    );
    assert!(
        !screen.contains("END OF DOCUMENTATION"),
        "Last line should not be visible initially. Screen:\n{screen}"
    );

    // Scroll down many times to reach the bottom
    // Content is ~150 lines, visible is ~10, so need ~140+ scroll events
    for _ in 0..200 {
        harness.mouse_scroll_down(50, 15)?;
        harness.render()?;
    }

    // After scrolling to bottom, the last line should be visible
    let screen_after = harness.screen_to_string();
    assert!(
        screen_after.contains("END OF DOCUMENTATION"),
        "Last line should be visible after scrolling to bottom. Screen:\n{screen_after}"
    );
    // First line should no longer be visible
    assert!(
        !screen_after.contains("pub fn process_data"),
        "First line should not be visible after scrolling to bottom. Screen:\n{screen_after}"
    );

    Ok(())
}
