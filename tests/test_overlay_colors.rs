/// Test that overlay colors are correctly applied and rendered
use fresh::config::LARGE_FILE_THRESHOLD_BYTES;
use fresh::model::event::CursorId;
use fresh::model::event::{Event, OverlayFace as EventOverlayFace};
use fresh::state::EditorState;
use fresh::view::overlay::OverlayNamespace;

#[test]
fn test_overlay_background_color_direct() {
    // Create a state with some content
    let mut state = EditorState::new(80, 24, LARGE_FILE_THRESHOLD_BYTES as usize);

    // Insert text using proper event so marker list is updated
    let text = "// TODO: test".to_string();
    state.apply(&Event::Insert {
        position: 0,
        text: text.clone(),
        cursor_id: CursorId(0),
    });

    println!("Buffer content: {:?}", state.buffer.to_string().unwrap());
    println!("Buffer size: {}", state.buffer.len());

    // Directly add an overlay with orange background
    state.apply(&Event::AddOverlay {
        namespace: Some(OverlayNamespace::from_string("test_todo".to_string())),
        range: 3..7, // "TODO"
        face: EventOverlayFace::Background {
            color: (255, 165, 0), // Orange
        },
        priority: 10,
        message: None,
    });

    // Check that overlay was created by checking all positions
    println!("Checking overlays at different positions:");
    for pos in 0..13 {
        let overlays_at_pos = state.overlays.at_position(pos, &state.marker_list);
        if !overlays_at_pos.is_empty() {
            println!("  Position {}: {} overlay(s)", pos, overlays_at_pos.len());
        }
    }

    let overlays = state.overlays.at_position(5, &state.marker_list); // Middle of "TODO"
    println!("Overlays at position 5: {}", overlays.len());

    assert_eq!(overlays.len(), 1, "Should have one overlay");

    // Check the overlay face
    let overlay = overlays[0];
    match &overlay.face {
        fresh::view::overlay::OverlayFace::Background { color } => {
            println!("Overlay color: {:?}", color);
            assert!(
                matches!(color, ratatui::style::Color::Rgb(255, 165, 0)),
                "Expected RGB(255, 165, 0) but got {:?}",
                color
            );
        }
        _ => panic!("Expected Background face"),
    }
}
