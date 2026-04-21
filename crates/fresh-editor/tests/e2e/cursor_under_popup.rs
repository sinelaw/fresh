//! The hardware cursor must not show through a popup.
//!
//! Popups are drawn on top of the buffer content, but the ratatui-managed
//! hardware cursor (`Frame::set_cursor_position`) is rendered by the real
//! terminal *on top of* every cell — including popup cells. If we leave
//! the cursor position set when a popup has covered that cell, the user
//! sees the cursor blink through the popup. The fix is to omit
//! `Frame::set_cursor_position` for the frame when the cursor would land
//! inside any popup rect, so `Terminal::draw` calls `hide_cursor`.

use crate::common::harness::EditorTestHarness;
use fresh::view::popup::{Popup, PopupPosition};

/// A popup placed over the editor cursor must cause the hardware cursor
/// to be hidden — otherwise the cursor shows through the popup.
#[test]
fn hardware_cursor_is_hidden_when_popup_covers_it() {
    let mut harness = EditorTestHarness::new(80, 30).unwrap();

    // Put something in the buffer so the cursor lives at a known, non-trivial
    // position (well inside the content area — never at (0, 0)).
    harness.type_text("hello world").unwrap();

    // Baseline: with no popup, ratatui should render the cursor (Some(..)).
    let (cx, cy) = harness
        .render_observing_cursor()
        .unwrap()
        .expect("hardware cursor should be visible when no popup is shown");

    // Place a popup so its rect contains (cx, cy).
    let popup_x = cx.saturating_sub(2);
    let popup_y = cy.saturating_sub(1);
    {
        let editor = harness.editor_mut();
        let theme = editor.theme().clone();
        let popup = Popup::text(
            vec![
                "I cover the cursor".to_string(),
                "Line 2 of popup".to_string(),
                "Line 3 of popup".to_string(),
            ],
            &theme,
        )
        .with_position(PopupPosition::Fixed {
            x: popup_x,
            y: popup_y,
        })
        .with_width(40)
        .with_max_height(10);
        editor.active_state_mut().popups.show(popup);
    }

    let cursor_after = harness.render_observing_cursor().unwrap();

    // Sanity: popup actually rendered on top of the buffer cell.
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("I cover the cursor"),
        "popup should be visible on screen, screen was:\n{screen}"
    );

    // The bug: hardware cursor is still placed at (cx, cy) — which is now
    // inside the popup — so it shows through. The fix: cursor should be
    // hidden (None) or moved outside the popup rect.
    match cursor_after {
        None => {}
        Some((hx, hy)) => {
            let inside_popup =
                hx >= popup_x && hx < popup_x + 40 && hy >= popup_y && hy < popup_y + 5; // 3 content lines + 2 borders
            assert!(
                !inside_popup,
                "hardware cursor at ({hx}, {hy}) is inside popup rect \
                 ({popup_x}, {popup_y})..({},{}) — it will show through",
                popup_x + 40,
                popup_y + 5,
            );
        }
    }
}
