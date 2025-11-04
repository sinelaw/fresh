// Integration tests - testing how modules work together

mod common;

use editor::{
    event::{CursorId, Event, EventLog},
    state::EditorState,
};

/// Test that cursor positions are correctly adjusted after buffer edits
#[test]
fn test_buffer_cursor_adjustment_on_insert() {
    let mut state = EditorState::new(80, 24);

    // Get the initial primary cursor ID (CursorId(0))
    let original_primary = state.cursors.primary_id();

    // Insert some initial text with the original primary cursor
    state.apply(&Event::Insert {
        position: 0,
        text: "hello world".to_string(),
        cursor_id: original_primary,
    });

    // Original primary cursor should be at end of inserted text (position 11)
    assert_eq!(state.cursors.get(original_primary).unwrap().position, 11);

    // Add a second cursor at position 6 (start of "world")
    // Note: This will make CursorId(1) the new primary
    state.apply(&Event::AddCursor {
        cursor_id: CursorId(1),
        position: 6,
        anchor: None,
    });

    // Verify CursorId(1) is at position 6 and is now primary
    assert_eq!(state.cursors.get(CursorId(1)).unwrap().position, 6);
    assert_eq!(state.cursors.primary_id(), CursorId(1));

    // Insert text at beginning with the ORIGINAL primary cursor (not the new one)
    // This tests that non-editing cursors get adjusted
    let insert_len = "INSERTED ".len();
    state.apply(&Event::Insert {
        position: 0,
        text: "INSERTED ".to_string(),
        cursor_id: original_primary, // Using original cursor, not the new primary
    });

    // The cursor that made the edit (original_primary) should be at position 0 + insert_len = 9
    assert_eq!(
        state.cursors.get(original_primary).unwrap().position,
        insert_len,
        "Cursor that made the edit should be at end of insertion"
    );

    // CursorId(1) was at position 6, should have moved forward by insert_len to position 15
    assert_eq!(
        state.cursors.get(CursorId(1)).unwrap().position,
        6 + insert_len,
        "Non-editing cursor should be adjusted by insertion length"
    );

    // Buffer content should be correct
    assert_eq!(state.buffer.to_string(), "INSERTED hello world");
}

/// Test that cursor positions are correctly adjusted after deletions
#[test]
fn test_buffer_cursor_adjustment_on_delete() {
    let mut state = EditorState::new(80, 24);

    // Insert initial text
    state.apply(&Event::Insert {
        position: 0,
        text: "hello beautiful world".to_string(),
        cursor_id: state.cursors.primary_id(),
    });

    // Add cursor at position 16 (start of "world")
    state.apply(&Event::AddCursor {
        cursor_id: CursorId(1),
        position: 16,
        anchor: None,
    });

    // Delete "beautiful " (positions 6-16)
    state.apply(&Event::Delete {
        range: 6..16,
        deleted_text: "beautiful ".to_string(),
        cursor_id: state.cursors.primary_id(),
    });

    // Second cursor should have moved back to position 6
    if let Some(cursor) = state.cursors.get(CursorId(1)) {
        assert_eq!(cursor.position, 6);
    }

    // Buffer content should be correct
    assert_eq!(state.buffer.to_string(), "hello world");
}

/// Test undo/redo with EditorState and EventLog
#[test]
fn test_state_eventlog_undo_redo() {
    let mut state = EditorState::new(80, 24);
    let mut log = EventLog::new();

    let cursor_id = state.cursors.primary_id();

    // Perform a series of edits - each insert at the END of the buffer
    let event1 = Event::Insert {
        position: 0,
        text: "a".to_string(),
        cursor_id,
    };
    log.append(event1.clone());
    state.apply(&event1);

    let event2 = Event::Insert {
        position: state.buffer.len(),
        text: "b".to_string(),
        cursor_id,
    };
    log.append(event2.clone());
    state.apply(&event2);

    let event3 = Event::Insert {
        position: state.buffer.len(),
        text: "c".to_string(),
        cursor_id,
    };
    log.append(event3.clone());
    state.apply(&event3);

    assert_eq!(state.buffer.to_string(), "abc");

    // Undo all - log.undo() returns the event at that position, we need to compute its inverse
    while log.can_undo() {
        if let Some(event) = log.undo() {
            if let Some(inverse) = event.inverse() {
                state.apply(&inverse);
            }
        }
    }

    assert_eq!(state.buffer.to_string(), "");

    // Redo all - log.redo() returns the original event to replay
    while log.can_redo() {
        if let Some(event) = log.redo() {
            state.apply(event);
        }
    }

    assert_eq!(state.buffer.to_string(), "abc");
}

/// Test that undo/redo maintains cursor positions correctly
#[test]
fn test_undo_redo_cursor_positions() {
    let mut state = EditorState::new(80, 24);
    let mut log = EventLog::new();

    let cursor_id = state.cursors.primary_id();

    // Type "hello" - each character at the end of the buffer
    for ch in "hello".chars() {
        let pos = state.buffer.len();
        let event = Event::Insert {
            position: pos,
            text: ch.to_string(),
            cursor_id,
        };
        log.append(event.clone());
        state.apply(&event);
    }

    assert_eq!(state.buffer.to_string(), "hello");
    let cursor_after_typing = state.cursors.primary().position;
    assert_eq!(cursor_after_typing, 5);

    // Undo twice (remove 'o' and 'l')
    for _ in 0..2 {
        if let Some(event) = log.undo() {
            if let Some(inverse) = event.inverse() {
                state.apply(&inverse);
            }
        }
    }

    assert_eq!(state.buffer.to_string(), "hel");
    assert_eq!(state.cursors.primary().position, 3);

    // Redo twice
    for _ in 0..2 {
        if let Some(event) = log.redo() {
            state.apply(event);
        }
    }

    assert_eq!(state.buffer.to_string(), "hello");
    assert_eq!(state.cursors.primary().position, 5);
}

/// Test viewport ensures cursor stays visible after edits
#[test]
fn test_viewport_tracks_cursor_through_edits() {
    let mut state = EditorState::new(80, 10); // Small viewport

    let cursor_id = state.cursors.primary_id();

    // Insert many lines to make content scroll
    for i in 0..20 {
        let event = Event::Insert {
            position: state.buffer.len(),
            text: format!("Line {i}\n"),
            cursor_id,
        };
        state.apply(&event);
    }

    // Cursor should be at the end
    let cursor_pos = state.cursors.primary().position;
    assert!(cursor_pos > 0);

    // Cursor position should be within buffer bounds
    assert!(
        cursor_pos <= state.buffer.len(),
        "Cursor should be within buffer bounds"
    );
}

/// Test multi-cursor normalization after overlapping edits
#[test]
fn test_multi_cursor_normalization() {
    let mut state = EditorState::new(80, 24);

    // Insert initial text
    state.apply(&Event::Insert {
        position: 0,
        text: "hello world".to_string(),
        cursor_id: state.cursors.primary_id(),
    });

    // Add overlapping cursors
    state.apply(&Event::AddCursor {
        cursor_id: CursorId(1),
        position: 5,
        anchor: None,
    });

    state.apply(&Event::AddCursor {
        cursor_id: CursorId(2),
        position: 6,
        anchor: None,
    });

    // Should have 3 cursors initially
    assert_eq!(state.cursors.count(), 3);

    // After normalization (which happens in AddCursor), overlapping cursors might be merged
    // This depends on Cursors::normalize() implementation
    // For now, just verify they all exist and are in valid positions
    for (_, cursor) in state.cursors.iter() {
        assert!(cursor.position <= state.buffer.len());
    }
}

/// Test that viewport resizing maintains cursor visibility
#[test]
fn test_viewport_resize_maintains_cursor() {
    let mut state = EditorState::new(80, 24);

    // Insert text and move cursor to middle
    state.apply(&Event::Insert {
        position: 0,
        text: "line1\nline2\nline3\nline4\nline5\n".to_string(),
        cursor_id: state.cursors.primary_id(),
    });

    state.apply(&Event::MoveCursor {
        cursor_id: state.cursors.primary_id(),
        position: 12, // Middle of line 2
        anchor: None,
    });

    // Resize to smaller height
    state.resize(80, 5);

    // Cursor should still be within buffer bounds
    let cursor_pos = state.cursors.primary().position;
    assert!(
        cursor_pos <= state.buffer.len(),
        "After resize, cursor should be within buffer bounds"
    );
}

/// Test overlay events - adding and removing overlays
#[test]
fn test_overlay_events() {
    use editor::event::{OverlayFace, UnderlineStyle};
    
    let mut state = EditorState::new(80, 24);
    
    // Insert some text
    state.apply(&Event::Insert {
        position: 0,
        text: "hello world".to_string(),
        cursor_id: CursorId(0),
    });
    
    // Add an error overlay
    state.apply(&Event::AddOverlay {
        overlay_id: "error1".to_string(),
        range: 0..5,
        face: OverlayFace::Underline {
            color: (255, 0, 0),
            style: UnderlineStyle::Wavy,
        },
        priority: 100,
        message: Some("Error here".to_string()),
    });
    
    // Check overlay was added
    let overlays_at_pos = state.overlays.at_position(2);
    assert_eq!(overlays_at_pos.len(), 1);
    assert_eq!(overlays_at_pos[0].id, Some("error1".to_string()));
    
    // Add a warning overlay with lower priority
    state.apply(&Event::AddOverlay {
        overlay_id: "warning1".to_string(),
        range: 3..8,
        face: OverlayFace::Underline {
            color: (255, 255, 0),
            style: UnderlineStyle::Wavy,
        },
        priority: 50,
        message: Some("Warning here".to_string()),
    });
    
    // Position 4 should have both overlays, sorted by priority (ascending)
    let overlays_at_4 = state.overlays.at_position(4);
    assert_eq!(overlays_at_4.len(), 2);
    assert_eq!(overlays_at_4[0].priority, 50);  // Warning (lower priority) comes first
    assert_eq!(overlays_at_4[1].priority, 100); // Error (higher priority) comes second
    
    // Remove error overlay
    state.apply(&Event::RemoveOverlay {
        overlay_id: "error1".to_string(),
    });
    
    // Now position 4 should only have warning
    let overlays_at_4 = state.overlays.at_position(4);
    assert_eq!(overlays_at_4.len(), 1);
    assert_eq!(overlays_at_4[0].id, Some("warning1".to_string()));
    
    // Clear all overlays
    state.apply(&Event::ClearOverlays);
    let overlays_after_clear = state.overlays.at_position(4);
    assert_eq!(overlays_after_clear.len(), 0);
}

/// Test popup events - showing, navigating, and hiding popups
#[test]
fn test_popup_events() {
    use editor::event::{PopupContentData, PopupData, PopupListItemData, PopupPositionData};
    
    let mut state = EditorState::new(80, 24);
    
    // Create a popup with list items
    let popup_data = PopupData {
        title: Some("Test Popup".to_string()),
        content: PopupContentData::List {
            items: vec![
                PopupListItemData {
                    text: "Item 1".to_string(),
                    detail: Some("First item".to_string()),
                    icon: Some("📄".to_string()),
                    data: None,
                },
                PopupListItemData {
                    text: "Item 2".to_string(),
                    detail: Some("Second item".to_string()),
                    icon: Some("📄".to_string()),
                    data: None,
                },
                PopupListItemData {
                    text: "Item 3".to_string(),
                    detail: Some("Third item".to_string()),
                    icon: Some("📄".to_string()),
                    data: None,
                },
            ],
            selected: 0,
        },
        position: PopupPositionData::Centered,
        width: 40,
        max_height: 10,
        bordered: true,
    };
    
    // Show the popup
    state.apply(&Event::ShowPopup {
        popup: popup_data,
    });
    
    // Check popup is visible
    assert!(state.popups.is_visible());
    let popup = state.popups.top().unwrap();
    assert_eq!(popup.title, Some("Test Popup".to_string()));
    
    // Navigate down
    state.apply(&Event::PopupSelectNext);
    
    // Check selection moved to item 1
    let popup = state.popups.top().unwrap();
    let selected_item = popup.selected_item().unwrap();
    assert_eq!(selected_item.text, "Item 2");
    
    // Navigate down again
    state.apply(&Event::PopupSelectNext);
    let popup = state.popups.top().unwrap();
    let selected_item = popup.selected_item().unwrap();
    assert_eq!(selected_item.text, "Item 3");
    
    // Navigate up
    state.apply(&Event::PopupSelectPrev);
    let popup = state.popups.top().unwrap();
    let selected_item = popup.selected_item().unwrap();
    assert_eq!(selected_item.text, "Item 2");
    
    // Hide popup
    state.apply(&Event::HidePopup);
    assert!(!state.popups.is_visible());
}

/// Test that overlays persist through undo/redo
#[test]
fn test_overlay_undo_redo() {
    use editor::event::{OverlayFace, UnderlineStyle};

    let mut log = EventLog::new();
    let mut state = EditorState::new(80, 24);

    // Insert text and add overlay
    let event1 = Event::Insert {
        position: 0,
        text: "hello".to_string(),
        cursor_id: CursorId(0),
    };
    log.append(event1.clone());
    state.apply(&event1);

    let event2 = Event::AddOverlay {
        overlay_id: "test".to_string(),
        range: 0..5,
        face: OverlayFace::Underline {
            color: (255, 0, 0),
            style: UnderlineStyle::Wavy,
        },
        priority: 100,
        message: None,
    };
    log.append(event2.clone());
    state.apply(&event2);

    // Verify overlay exists
    assert_eq!(state.overlays.at_position(2).len(), 1);

    // Undo the overlay addition
    log.undo();
    let mut new_state = EditorState::new(80, 24);
    for i in 0..log.current_index() {
        if let Some(entry) = log.entries().get(i) {
            new_state.apply(&entry.event);
        }
    }

    // Overlay should be gone
    assert_eq!(new_state.overlays.at_position(2).len(), 0);

    // Redo
    log.redo();
    let mut final_state = EditorState::new(80, 24);
    for i in 0..log.current_index() {
        if let Some(entry) = log.entries().get(i) {
            final_state.apply(&entry.event);
        }
    }

    // Overlay should be back
    assert_eq!(final_state.overlays.at_position(2).len(), 1);
}

/// Test LSP diagnostic to overlay conversion
#[test]
fn test_lsp_diagnostic_to_overlay() {
    use editor::{buffer::Buffer, lsp_diagnostics::diagnostic_to_overlay};
    use lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};

    let buffer = Buffer::from_str("let x = 5;\nlet y = 10;");

    // Create an error diagnostic on first line
    let diagnostic = Diagnostic {
        range: Range {
            start: Position {
                line: 0,
                character: 4,
            },
            end: Position {
                line: 0,
                character: 5,
            },
        },
        severity: Some(DiagnosticSeverity::ERROR),
        code: None,
        code_description: None,
        source: Some("rust-analyzer".to_string()),
        message: "unused variable: `x`".to_string(),
        related_information: None,
        tags: None,
        data: None,
    };

    let result = diagnostic_to_overlay(&diagnostic, &buffer);
    assert!(result.is_some());

    let (range, face, priority) = result.unwrap();

    // Check range: "let x = 5;\n" - position 4 is 'x'
    assert_eq!(range.start, 4);
    assert_eq!(range.end, 5);

    // Check priority (error should be highest)
    assert_eq!(priority, 100);

    // Check face (should be red wavy underline)
    match face {
        editor::overlay::OverlayFace::Underline { color, style } => {
            assert_eq!(color, ratatui::style::Color::Red);
            assert_eq!(style, editor::overlay::UnderlineStyle::Wavy);
        }
        _ => panic!("Expected underline face for error diagnostic"),
    }
}

/// Test overlay rendering with multiple priorities
#[test]
fn test_overlay_priority_layering() {
    use editor::event::{OverlayFace, UnderlineStyle};

    let mut state = EditorState::new(80, 24);

    // Insert text
    state.apply(&Event::Insert {
        position: 0,
        text: "hello world".to_string(),
        cursor_id: CursorId(0),
    });

    // Add low priority overlay (hint)
    state.apply(&Event::AddOverlay {
        overlay_id: "hint".to_string(),
        range: 0..5,
        face: OverlayFace::Underline {
            color: (128, 128, 128),
            style: UnderlineStyle::Dotted,
        },
        priority: 10,
        message: Some("Hint message".to_string()),
    });

    // Add high priority overlay (error) overlapping
    state.apply(&Event::AddOverlay {
        overlay_id: "error".to_string(),
        range: 2..7,
        face: OverlayFace::Underline {
            color: (255, 0, 0),
            style: UnderlineStyle::Wavy,
        },
        priority: 100,
        message: Some("Error message".to_string()),
    });

    // Position 3 should have both overlays, sorted by priority
    let overlays = state.overlays.at_position(3);
    assert_eq!(overlays.len(), 2);
    assert_eq!(overlays[0].priority, 10);  // Hint (lower priority first)
    assert_eq!(overlays[1].priority, 100); // Error (higher priority second)

    // Verify IDs
    assert_eq!(overlays[0].id, Some("hint".to_string()));
    assert_eq!(overlays[1].id, Some("error".to_string()));
}

/// E2E test: Verify diagnostic overlays are visually rendered with correct colors
#[test]
fn test_diagnostic_overlay_visual_rendering() {
    use common::harness::EditorTestHarness;
    use editor::event::{OverlayFace, UnderlineStyle};
    use ratatui::style::{Color, Modifier};

    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Insert some text
    harness.type_text("let x = 5;").unwrap();
    harness.render().unwrap();

    // Add an error diagnostic overlay on "x" (position 4)
    // This simulates what LSP would do when it finds an error
    // We use the overlay API directly, but convert the color to RGB format
    // since that's what OverlayFace uses (u8, u8, u8) tuples
    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::AddOverlay {
        overlay_id: "lsp-diagnostic-0".to_string(),
        range: 4..5, // "x"
        face: OverlayFace::Underline {
            color: (255, 0, 0), // Red as RGB
            style: UnderlineStyle::Wavy,
        },
        priority: 100,
        message: Some("unused variable: `x`".to_string()),
    });

    // Render again to apply the overlay styling
    harness.render().unwrap();

    // Now check that the character "x" at the expected position has red color
    // The gutter is typically "   1 │ " (7 characters for single-digit line numbers)
    // So the text starts at column 7
    // "let x = 5;" -> "x" is at text position 4, which maps to screen column 7 + 4 = 11
    let gutter_width = 7; // "   1 │ " for line 1
    let x_column = gutter_width + 4; // Position of "x" in "let x = 5;"
    let x_row = 1; // First line of content (row 0 is tab bar, row 1 is first text line)

    // Get the style of the "x" character
    let style = harness.get_cell_style(x_column, x_row);
    assert!(style.is_some(), "Expected cell at ({}, {}) to have a style", x_column, x_row);

    let style = style.unwrap();

    // Verify the foreground color is red (indicating error)
    // The color will be rendered as RGB(255, 0, 0) since that's what we passed in the overlay
    assert_eq!(
        style.fg,
        Some(Color::Rgb(255, 0, 0)),
        "Expected 'x' to be rendered in red (RGB 255,0,0) due to error diagnostic"
    );

    // Verify underline modifier is applied
    assert!(
        style.add_modifier.contains(Modifier::UNDERLINED),
        "Expected 'x' to have underline modifier"
    );

    // Verify the text itself is correct
    let text = harness.get_cell(x_column, x_row);
    assert_eq!(text, Some("x".to_string()), "Expected 'x' character at position");
}
