//! Migration of `tests/e2e/cursor_under_popup.rs` — the hardware
//! cursor must not show through a popup that has been drawn over the
//! cell where the cursor would otherwise sit.
//!
//! Background: popups are drawn on top of the buffer content, but the
//! ratatui-managed hardware cursor (`Frame::set_cursor_position`) is
//! rendered by the real terminal *on top of* every cell — including
//! popup cells. If the cursor position stays set when a popup has
//! covered that cell, the user sees the cursor blink through the
//! popup. The fix omits `Frame::set_cursor_position` for the frame
//! when the cursor would land inside any popup rect, so
//! `Terminal::draw` calls `hide_cursor`.
//!
//! ## Harness-direct pattern
//!
//! `render_observing_cursor` returns the ratatui frame's hardware
//! cursor position — it has no `EditorTestApi` projection because
//! it probes the rendered `Frame` directly. `Popup::show` is also
//! a `view::popup` API that lives outside `EditorTestApi`. This
//! test therefore uses the harness-direct pattern (mirror of
//! `migrated_redraw_screen.rs` for the full-redraw flag).
//!
//! Source: `tests/e2e/cursor_under_popup.rs` (1 test migrated; no
//! tests deferred).

use crate::common::harness::EditorTestHarness;
use fresh::view::popup::{Popup, PopupPosition};

#[test]
fn migrated_hardware_cursor_is_hidden_when_popup_covers_it() {
    // Original: `hardware_cursor_is_hidden_when_popup_covers_it`.
    let mut harness = EditorTestHarness::new(80, 30).unwrap();

    // Put something in the buffer so the cursor lives at a known,
    // non-trivial position (well inside the content area — never at
    // (0, 0)).
    harness.type_text("hello world").unwrap();

    // Baseline: with no popup, ratatui should render the cursor
    // (Some(..)).
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

    // The bug: hardware cursor is still placed at (cx, cy) — which
    // is now inside the popup — so it shows through. The fix:
    // cursor should be hidden (None) or moved outside the popup
    // rect.
    match cursor_after {
        None => {}
        Some((hx, hy)) => {
            let inside_popup = hx >= popup_x
                && hx < popup_x + 40
                && hy >= popup_y
                && hy < popup_y + 5; // 3 content lines + 2 borders
            assert!(
                !inside_popup,
                "hardware cursor at ({hx}, {hy}) is inside popup \
                 rect ({popup_x}, {popup_y})..({},{}) — it will show \
                 through",
                popup_x + 40,
                popup_y + 5,
            );
        }
    }
}

/// Anti-test: drop the `popups.show(popup)` step. Without the popup,
/// the hardware cursor must remain at the post-`type_text` position
/// returned by the baseline `render_observing_cursor()` call — proves
/// the cursor-hide claim in the positive test is gated on the popup
/// actually being shown, not on harness construction incidentally
/// hiding the cursor.
#[test]
fn anti_hardware_cursor_without_popup_stays_visible() {
    let mut harness = EditorTestHarness::new(80, 30).unwrap();
    harness.type_text("hello world").unwrap();

    let baseline = harness
        .render_observing_cursor()
        .unwrap()
        .expect("baseline: hardware cursor should be visible after type_text");

    // No popup is shown — the cursor must remain visible on the
    // next render.
    let after = harness
        .render_observing_cursor()
        .unwrap()
        .expect(
            "anti: without a popup covering it, the hardware cursor \
             must remain visible (Some(..)) — the positive test's \
             cursor-hide claim depends on the popup actually being \
             shown",
        );

    assert_eq!(
        after, baseline,
        "anti: cursor position must not change between renders when \
         nothing is dispatched (baseline={baseline:?}, after={after:?})"
    );
}
