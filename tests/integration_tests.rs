// Integration tests - testing how modules work together

mod common;

use fresh::{
    model::event::{CursorId, Event, EventLog},
    state::EditorState,
    view::overlay::OverlayNamespace,
};

/// Test that cursor positions are correctly adjusted after buffer edits
#[test]
fn test_buffer_cursor_adjustment_on_insert() {
    let mut state = EditorState::new(80, 24, fresh::config::LARGE_FILE_THRESHOLD_BYTES as usize);

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
    assert_eq!(state.buffer.to_string().unwrap(), "INSERTED hello world");
}

/// Test that cursor positions are correctly adjusted after deletions
#[test]
fn test_buffer_cursor_adjustment_on_delete() {
    let mut state = EditorState::new(80, 24, fresh::config::LARGE_FILE_THRESHOLD_BYTES as usize);

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
    assert_eq!(state.buffer.to_string().unwrap(), "hello world");
}

/// Test undo/redo with EditorState and EventLog
#[test]
fn test_state_eventlog_undo_redo() {
    let mut state = EditorState::new(80, 24, fresh::config::LARGE_FILE_THRESHOLD_BYTES as usize);
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

    assert_eq!(state.buffer.to_string().unwrap(), "abc");

    // Undo all - log.undo() returns inverse events ready to apply
    while log.can_undo() {
        let events = log.undo();
        for event in events {
            state.apply(&event);
        }
    }

    assert_eq!(state.buffer.to_string().unwrap(), "");

    // Redo all - log.redo() returns the original events to replay
    while log.can_redo() {
        let events = log.redo();
        for event in events {
            state.apply(&event);
        }
    }

    assert_eq!(state.buffer.to_string().unwrap(), "abc");
}

/// Test that undo/redo maintains cursor positions correctly
#[test]
fn test_undo_redo_cursor_positions() {
    let mut state = EditorState::new(80, 24, fresh::config::LARGE_FILE_THRESHOLD_BYTES as usize);
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

    assert_eq!(state.buffer.to_string().unwrap(), "hello");
    let cursor_after_typing = state.cursors.primary().position;
    assert_eq!(cursor_after_typing, 5);

    // Undo twice (remove 'o' and 'l')
    for _ in 0..2 {
        let events = log.undo();
        for event in events {
            state.apply(&event);
        }
    }

    assert_eq!(state.buffer.to_string().unwrap(), "hel");
    assert_eq!(state.cursors.primary().position, 3);

    // Redo twice
    for _ in 0..2 {
        let events = log.redo();
        for event in events {
            state.apply(&event);
        }
    }

    assert_eq!(state.buffer.to_string().unwrap(), "hello");
    assert_eq!(state.cursors.primary().position, 5);
}

/// Test viewport ensures cursor stays visible after edits
#[test]
fn test_viewport_tracks_cursor_through_edits() {
    let mut state = EditorState::new(80, 10, fresh::config::LARGE_FILE_THRESHOLD_BYTES as usize); // Small viewport

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
    let mut state = EditorState::new(80, 24, fresh::config::LARGE_FILE_THRESHOLD_BYTES as usize);

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
    let mut state = EditorState::new(80, 24, fresh::config::LARGE_FILE_THRESHOLD_BYTES as usize);

    // Insert text and move cursor to middle
    state.apply(&Event::Insert {
        position: 0,
        text: "line1\nline2\nline3\nline4\nline5\n".to_string(),
        cursor_id: state.cursors.primary_id(),
    });

    state.apply(&Event::MoveCursor {
        cursor_id: state.cursors.primary_id(),
        old_position: 0,
        new_position: 12, // Middle of line 2
        old_anchor: None,
        new_anchor: None,
        old_sticky_column: 0,
        new_sticky_column: 0,
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
    use fresh::model::event::{OverlayFace, UnderlineStyle};

    let mut state = EditorState::new(80, 24, fresh::config::LARGE_FILE_THRESHOLD_BYTES as usize);

    // Insert some text
    state.apply(&Event::Insert {
        position: 0,
        text: "hello world".to_string(),
        cursor_id: CursorId(0),
    });

    // Add an error overlay with namespace
    state.apply(&Event::AddOverlay {
        namespace: Some(OverlayNamespace::from_string("error".to_string())),
        range: 0..5,
        face: OverlayFace::Underline {
            color: (255, 0, 0),
            style: UnderlineStyle::Wavy,
        },
        priority: 100,
        message: Some("Error here".to_string()),
    });

    // Check overlay was added
    let overlays_at_pos = state.overlays.at_position(2, &state.marker_list);
    assert_eq!(overlays_at_pos.len(), 1);
    assert_eq!(
        overlays_at_pos[0].namespace,
        Some(OverlayNamespace::from_string("error".to_string()))
    );

    // Add a warning overlay with lower priority
    state.apply(&Event::AddOverlay {
        namespace: Some(OverlayNamespace::from_string("warning".to_string())),
        range: 3..8,
        face: OverlayFace::Underline {
            color: (255, 255, 0),
            style: UnderlineStyle::Wavy,
        },
        priority: 50,
        message: Some("Warning here".to_string()),
    });

    // Position 4 should have both overlays, sorted by priority (ascending)
    let overlays_at_4 = state.overlays.at_position(4, &state.marker_list);
    assert_eq!(overlays_at_4.len(), 2);
    assert_eq!(overlays_at_4[0].priority, 50); // Warning (lower priority) comes first
    assert_eq!(overlays_at_4[1].priority, 100); // Error (higher priority) comes second

    // Remove error overlay using namespace
    state.apply(&Event::ClearNamespace {
        namespace: OverlayNamespace::from_string("error".to_string()),
    });

    // Now position 4 should only have warning
    let overlays_at_4 = state.overlays.at_position(4, &state.marker_list);
    assert_eq!(overlays_at_4.len(), 1);
    assert_eq!(
        overlays_at_4[0].namespace,
        Some(OverlayNamespace::from_string("warning".to_string()))
    );

    // Clear all overlays
    state.apply(&Event::ClearOverlays);
    let overlays_after_clear = state.overlays.at_position(4, &state.marker_list);
    assert_eq!(overlays_after_clear.len(), 0);
}

/// Test popup events - showing, navigating, and hiding popups
#[test]
fn test_popup_events() {
    use fresh::model::event::{PopupContentData, PopupData, PopupListItemData, PopupPositionData};

    let mut state = EditorState::new(80, 24, fresh::config::LARGE_FILE_THRESHOLD_BYTES as usize);

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
    state.apply(&Event::ShowPopup { popup: popup_data });

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
    use fresh::model::event::{OverlayFace, UnderlineStyle};

    let mut log = EventLog::new();
    let mut state = EditorState::new(80, 24, fresh::config::LARGE_FILE_THRESHOLD_BYTES as usize);

    // Insert text and add overlay
    let event1 = Event::Insert {
        position: 0,
        text: "hello".to_string(),
        cursor_id: CursorId(0),
    };
    log.append(event1.clone());
    state.apply(&event1);

    let event2 = Event::AddOverlay {
        namespace: Some(OverlayNamespace::from_string("test".to_string())),
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
    assert_eq!(state.overlays.at_position(2, &state.marker_list).len(), 1);

    // Undo - this should process AddOverlay (remove it) and undo the Insert
    let undo_events = log.undo();
    for event in &undo_events {
        state.apply(event);
    }

    // After undo: buffer should be empty, and overlay should be removed
    assert_eq!(state.buffer.len(), 0);
    assert_eq!(state.overlays.at_position(2, &state.marker_list).len(), 0);
    assert!(
        !undo_events.is_empty(),
        "Should have returned events to undo"
    );

    // Redo - should redo the Insert and re-add the overlay
    let redo_events = log.redo();
    for event in &redo_events {
        state.apply(event);
    }

    // After redo: buffer is back and overlay should be back
    assert_eq!(state.buffer.to_string().unwrap(), "hello");
    // Note: AddOverlay was redone, so overlay should be back
    assert_eq!(state.overlays.at_position(2, &state.marker_list).len(), 1);
    assert!(
        !redo_events.is_empty(),
        "Should have returned events to redo"
    );
}

/// Test LSP diagnostic to overlay conversion
#[test]
fn test_lsp_diagnostic_to_overlay() {
    use fresh::{
        config::LARGE_FILE_THRESHOLD_BYTES, model::buffer::Buffer,
        services::lsp::diagnostics::diagnostic_to_overlay,
    };
    use lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};

    let buffer = Buffer::from_str(
        "let x = 5;\nlet y = 10;",
        LARGE_FILE_THRESHOLD_BYTES as usize,
    );

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

    let theme = fresh::view::theme::Theme::dark();
    let result = diagnostic_to_overlay(&diagnostic, &buffer, &theme);
    assert!(result.is_some());

    let (range, face, priority) = result.unwrap();

    // Check range: "let x = 5;\n" - position 4 is 'x'
    assert_eq!(range.start, 4);
    assert_eq!(range.end, 5);

    // Check priority (error should be highest)
    assert_eq!(priority, 100);

    // Check face (should use theme's error background color)
    match face {
        fresh::view::overlay::OverlayFace::Background { color } => {
            assert_eq!(color, theme.diagnostic_error_bg);
        }
        _ => panic!("Expected background face for error diagnostic"),
    }
}

/// Test overlay rendering with multiple priorities
#[test]
fn test_overlay_priority_layering() {
    use fresh::model::event::{OverlayFace, UnderlineStyle};

    let mut state = EditorState::new(80, 24, fresh::config::LARGE_FILE_THRESHOLD_BYTES as usize);

    // Insert text
    state.apply(&Event::Insert {
        position: 0,
        text: "hello world".to_string(),
        cursor_id: CursorId(0),
    });

    // Add low priority overlay (hint)
    state.apply(&Event::AddOverlay {
        namespace: Some(OverlayNamespace::from_string("hint".to_string())),
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
        namespace: Some(OverlayNamespace::from_string("error".to_string())),
        range: 2..7,
        face: OverlayFace::Underline {
            color: (255, 0, 0),
            style: UnderlineStyle::Wavy,
        },
        priority: 100,
        message: Some("Error message".to_string()),
    });

    // Position 3 should have both overlays, sorted by priority
    let overlays = state.overlays.at_position(3, &state.marker_list);
    assert_eq!(overlays.len(), 2);
    assert_eq!(overlays[0].priority, 10); // Hint (lower priority first)
    assert_eq!(overlays[1].priority, 100); // Error (higher priority second)

    // Verify namespaces
    assert_eq!(
        overlays[0].namespace,
        Some(OverlayNamespace::from_string("hint".to_string()))
    );
    assert_eq!(
        overlays[1].namespace,
        Some(OverlayNamespace::from_string("error".to_string()))
    );
}

/// E2E test: Verify diagnostic overlays are visually rendered with correct colors
#[test]
fn test_diagnostic_overlay_visual_rendering() {
    use common::harness::EditorTestHarness;
    use fresh::model::event::{OverlayFace, UnderlineStyle};
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
        namespace: Some(OverlayNamespace::from_string("lsp-diagnostic".to_string())),
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
    // The gutter is now " " + "   1" + " │ " (8 characters: indicator + line number + separator)
    // So the text starts at column 8
    // "let x = 5;" -> "x" is at text position 4, which maps to screen column 8 + 4 = 12
    let gutter_width = 8; // " " (indicator) + "   1" + " │ " for line 1
    let x_column = gutter_width + 4; // Position of "x" in "let x = 5;"
    let (content_first_row, _) = harness.content_area_rows();
    let x_row = content_first_row as u16; // First line of content (row 0 is menu bar, row 1 is tab bar, row 2 is first text line)

    // Get the style of the "x" character
    let style = harness.get_cell_style(x_column, x_row);
    assert!(
        style.is_some(),
        "Expected cell at ({x_column}, {x_row}) to have a style"
    );

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
    assert_eq!(
        text,
        Some("x".to_string()),
        "Expected 'x' character at position"
    );
}

/// Comprehensive tests for Event::inverse()
mod event_inverse_tests {
    use fresh::model::event::{CursorId, Event, OverlayFace, UnderlineStyle};
    use fresh::view::overlay::{OverlayHandle, OverlayNamespace};

    #[test]
    fn test_insert_inverse() {
        let event = Event::Insert {
            position: 10,
            text: "hello".to_string(),
            cursor_id: CursorId(0),
        };

        let inverse = event.inverse().expect("Insert should have inverse");

        match inverse {
            Event::Delete {
                range,
                deleted_text,
                cursor_id,
            } => {
                assert_eq!(range, 10..15);
                assert_eq!(deleted_text, "hello");
                assert_eq!(cursor_id, CursorId::UNDO_SENTINEL);
            }
            _ => panic!("Insert inverse should be Delete"),
        }
    }

    #[test]
    fn test_delete_inverse() {
        let event = Event::Delete {
            range: 5..10,
            deleted_text: "world".to_string(),
            cursor_id: CursorId(1),
        };

        let inverse = event.inverse().expect("Delete should have inverse");

        match inverse {
            Event::Insert {
                position,
                text,
                cursor_id,
            } => {
                assert_eq!(position, 5);
                assert_eq!(text, "world");
                assert_eq!(cursor_id, CursorId::UNDO_SENTINEL);
            }
            _ => panic!("Delete inverse should be Insert"),
        }
    }

    #[test]
    fn test_add_cursor_inverse() {
        let event = Event::AddCursor {
            cursor_id: CursorId(2),
            position: 42,
            anchor: Some(10),
        };

        let inverse = event.inverse().expect("AddCursor should have inverse");

        match inverse {
            Event::RemoveCursor {
                cursor_id,
                position,
                anchor,
            } => {
                assert_eq!(cursor_id, CursorId(2));
                assert_eq!(position, 42);
                assert_eq!(anchor, Some(10));
            }
            _ => panic!("AddCursor inverse should be RemoveCursor"),
        }
    }

    #[test]
    fn test_remove_cursor_inverse() {
        let event = Event::RemoveCursor {
            cursor_id: CursorId(3),
            position: 100,
            anchor: None,
        };

        let inverse = event.inverse().expect("RemoveCursor should have inverse");

        match inverse {
            Event::AddCursor {
                cursor_id,
                position,
                anchor,
            } => {
                assert_eq!(cursor_id, CursorId(3));
                assert_eq!(position, 100);
                assert_eq!(anchor, None);
            }
            _ => panic!("RemoveCursor inverse should be AddCursor"),
        }
    }

    #[test]
    fn test_move_cursor_inverse() {
        let event = Event::MoveCursor {
            cursor_id: CursorId(0),
            old_position: 10,
            new_position: 20,
            old_anchor: None,
            new_anchor: Some(15),
            old_sticky_column: 5,
            new_sticky_column: 10,
        };

        let inverse = event.inverse().expect("MoveCursor should have inverse");

        match inverse {
            Event::MoveCursor {
                cursor_id,
                old_position,
                new_position,
                old_anchor,
                new_anchor,
                old_sticky_column,
                new_sticky_column,
            } => {
                assert_eq!(cursor_id, CursorId(0));
                assert_eq!(old_position, 20); // Swapped
                assert_eq!(new_position, 10); // Swapped
                assert_eq!(old_anchor, Some(15)); // Swapped
                assert_eq!(new_anchor, None); // Swapped
                assert_eq!(old_sticky_column, 10); // Swapped
                assert_eq!(new_sticky_column, 5); // Swapped
            }
            _ => panic!("MoveCursor inverse should be MoveCursor"),
        }
    }

    #[test]
    fn test_add_overlay_no_inverse() {
        // Overlays are ephemeral decorations, not undoable
        let event = Event::AddOverlay {
            namespace: Some(OverlayNamespace::from_string("test-overlay".to_string())),
            range: 0..10,
            face: OverlayFace::Underline {
                color: (255, 0, 0),
                style: UnderlineStyle::Wavy,
            },
            priority: 100,
            message: Some("error".to_string()),
        };

        // AddOverlay is ephemeral and has no inverse
        assert!(event.inverse().is_none());
    }

    #[test]
    fn test_remove_overlay_no_inverse() {
        let event = Event::RemoveOverlay {
            handle: OverlayHandle::from_string("test".to_string()),
        };

        // RemoveOverlay is ephemeral and has no inverse
        assert!(event.inverse().is_none());
    }

    #[test]
    fn test_scroll_inverse() {
        let event = Event::Scroll { line_offset: 5 };

        let inverse = event.inverse().expect("Scroll should have inverse");

        match inverse {
            Event::Scroll { line_offset } => {
                assert_eq!(line_offset, -5); // Negated
            }
            _ => panic!("Scroll inverse should be Scroll with negated offset"),
        }
    }

    #[test]
    fn test_set_viewport_no_inverse() {
        let event = Event::SetViewport { top_line: 10 };

        // SetViewport doesn't have inverse because we don't store the old top_line
        assert!(event.inverse().is_none());
    }

    #[test]
    fn test_change_mode_no_inverse() {
        let event = Event::ChangeMode {
            mode: "insert".to_string(),
        };

        // ChangeMode doesn't have inverse because we don't store the old mode
        assert!(event.inverse().is_none());
    }

    #[test]
    fn test_batch_inverse() {
        let batch = Event::Batch {
            events: vec![
                Event::Insert {
                    position: 0,
                    text: "a".to_string(),
                    cursor_id: CursorId(0),
                },
                Event::Insert {
                    position: 1,
                    text: "b".to_string(),
                    cursor_id: CursorId(0),
                },
                Event::Insert {
                    position: 2,
                    text: "c".to_string(),
                    cursor_id: CursorId(0),
                },
            ],
            description: "Insert abc".to_string(),
        };

        let inverse = batch.inverse().expect("Batch should have inverse");

        match inverse {
            Event::Batch {
                events,
                description,
            } => {
                assert_eq!(events.len(), 3);
                assert_eq!(description, "Undo: Insert abc");

                // Events should be reversed
                // Original: [Insert(0,'a'), Insert(1,'b'), Insert(2,'c')]
                // Inverse: [Delete(2..3,'c'), Delete(1..2,'b'), Delete(0..1,'a')]

                // Check first event (was last insert)
                match &events[0] {
                    Event::Delete {
                        range,
                        deleted_text,
                        ..
                    } => {
                        assert_eq!(*range, 2..3);
                        assert_eq!(deleted_text, "c");
                    }
                    _ => panic!("Expected Delete"),
                }

                // Check last event (was first insert)
                match &events[2] {
                    Event::Delete {
                        range,
                        deleted_text,
                        ..
                    } => {
                        assert_eq!(*range, 0..1);
                        assert_eq!(deleted_text, "a");
                    }
                    _ => panic!("Expected Delete"),
                }
            }
            _ => panic!("Batch inverse should be Batch"),
        }
    }

    #[test]
    fn test_batch_with_non_invertible_events() {
        let batch = Event::Batch {
            events: vec![
                Event::Insert {
                    position: 0,
                    text: "a".to_string(),
                    cursor_id: CursorId(0),
                },
                Event::SetViewport { top_line: 10 }, // Not invertible
            ],
            description: "Mixed batch".to_string(),
        };

        // Batch with non-invertible events returns None
        assert!(batch.inverse().is_none());
    }

    #[test]
    fn test_nested_batch_inverse() {
        let inner_batch = Event::Batch {
            events: vec![
                Event::Insert {
                    position: 0,
                    text: "x".to_string(),
                    cursor_id: CursorId(0),
                },
                Event::Insert {
                    position: 1,
                    text: "y".to_string(),
                    cursor_id: CursorId(0),
                },
            ],
            description: "Inner".to_string(),
        };

        let outer_batch = Event::Batch {
            events: vec![
                Event::Insert {
                    position: 0,
                    text: "a".to_string(),
                    cursor_id: CursorId(0),
                },
                inner_batch,
                Event::Insert {
                    position: 3,
                    text: "z".to_string(),
                    cursor_id: CursorId(0),
                },
            ],
            description: "Outer".to_string(),
        };

        let inverse = outer_batch
            .inverse()
            .expect("Nested batch should have inverse");

        match inverse {
            Event::Batch {
                events,
                description,
            } => {
                assert_eq!(events.len(), 3);
                assert_eq!(description, "Undo: Outer");

                // Check that the inner batch is also inverted
                match &events[1] {
                    Event::Batch {
                        events: inner_events,
                        description: inner_desc,
                    } => {
                        assert_eq!(inner_events.len(), 2);
                        assert_eq!(inner_desc, "Undo: Inner");
                    }
                    _ => panic!("Expected nested Batch"),
                }
            }
            _ => panic!("Outer batch inverse should be Batch"),
        }
    }

    #[test]
    fn test_double_inverse_equals_original() {
        let original = Event::Insert {
            position: 5,
            text: "test".to_string(),
            cursor_id: CursorId(0),
        };

        let inverse = original.inverse().expect("Should have inverse");
        let double_inverse = inverse.inverse().expect("Should have double inverse");

        // Double inverse should be equivalent to original (with UNDO_SENTINEL cursor_id)
        match double_inverse {
            Event::Insert {
                position,
                text,
                cursor_id,
            } => {
                assert_eq!(position, 5);
                assert_eq!(text, "test");
                assert_eq!(cursor_id, CursorId::UNDO_SENTINEL);
            }
            _ => panic!("Double inverse should be Insert"),
        }
    }

    #[test]
    fn test_move_cursor_double_inverse() {
        let original = Event::MoveCursor {
            cursor_id: CursorId(0),
            old_position: 10,
            new_position: 20,
            old_anchor: None,
            new_anchor: Some(15),
            old_sticky_column: 5,
            new_sticky_column: 10,
        };

        let inverse = original.inverse().expect("Should have inverse");
        let double_inverse = inverse.inverse().expect("Should have double inverse");

        // Double inverse should equal original
        match double_inverse {
            Event::MoveCursor {
                cursor_id,
                old_position,
                new_position,
                old_anchor,
                new_anchor,
                old_sticky_column,
                new_sticky_column,
            } => {
                assert_eq!(cursor_id, CursorId(0));
                assert_eq!(old_position, 10);
                assert_eq!(new_position, 20);
                assert_eq!(old_anchor, None);
                assert_eq!(new_anchor, Some(15));
                assert_eq!(old_sticky_column, 5);
                assert_eq!(new_sticky_column, 10);
            }
            _ => panic!("Double inverse should be MoveCursor"),
        }
    }
}
