//! Regression tests for issue #3324: pressing a bare modifier key snapped a
//! wheel-scrolled view back to the cursor.
//!
//! A terminal speaking the kitty keyboard protocol with "report all keys"
//! sends a press for Shift, Ctrl, Alt and Super on their own. The editor has
//! nothing bound to a bare modifier, but `handle_key` cleared the viewport's
//! `skip_ensure_visible` flag before finding that out, so the next render
//! scrolled the cursor back into view. Holding Ctrl on the way to Ctrl+C was
//! enough to lose the scroll position.
//!
//! The tests read the screen: which lines are visible before and after.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers, ModifierKeyCode};

const WIDTH: u16 = 80;
const HEIGHT: u16 = 24;

/// A 1000-line buffer, cursor on line 1, wheel-scrolled until line 1 is off
/// screen.
fn harness_scrolled_away_from_cursor() -> EditorTestHarness {
    let mut harness = EditorTestHarness::new(WIDTH, HEIGHT).unwrap();
    let content: String = (1..=1000).map(|i| format!("line {i:04}\n")).collect();
    let _fixture = harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();
    assert!(
        harness.screen_to_string().contains("line 0001"),
        "setup: line 1 should be on screen before scrolling"
    );

    let (first_row, _) = harness.content_area_rows();
    for _ in 0..15 {
        harness.mouse_scroll_down(10, first_row as u16).unwrap();
    }
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("line 0001"),
        "setup: the wheel should have scrolled line 1 out of view. Screen:\n{screen}"
    );
    harness
}

#[test]
fn bare_modifier_press_does_not_scroll_back_to_the_cursor() {
    let cases = [
        (ModifierKeyCode::LeftShift, KeyModifiers::SHIFT),
        (ModifierKeyCode::LeftControl, KeyModifiers::CONTROL),
        (ModifierKeyCode::LeftAlt, KeyModifiers::ALT),
        (ModifierKeyCode::LeftSuper, KeyModifiers::SUPER),
        (ModifierKeyCode::RightShift, KeyModifiers::SHIFT),
    ];
    for (key, modifiers) in cases {
        let mut harness = harness_scrolled_away_from_cursor();
        let before = harness.screen_to_string();

        harness.send_key(KeyCode::Modifier(key), modifiers).unwrap();
        harness.render().unwrap();

        let after = harness.screen_to_string();
        assert_eq!(
            after, before,
            "pressing {key:?} alone should leave the scrolled view where it was"
        );
    }
}

/// The control: a key that does something still brings the cursor back into
/// view, so the fix did not leave the view stuck.
#[test]
fn real_key_after_bare_modifier_still_reveals_the_cursor() {
    let mut harness = harness_scrolled_away_from_cursor();

    harness
        .send_key(
            KeyCode::Modifier(ModifierKeyCode::LeftShift),
            KeyModifiers::SHIFT,
        )
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("line 0001") && screen.contains("Ln 1, Col 2"),
        "moving the cursor should scroll it back into view. Screen:\n{screen}"
    );
}
