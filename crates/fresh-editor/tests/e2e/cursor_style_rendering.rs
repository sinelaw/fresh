//! E2E tests for cursor style rendering (issue #851).
//!
//! When cursor_style is set to a non-block shape (bar or underline), the
//! primary cursor character cell must NOT have the REVERSED modifier.
//! REVERSED creates a block-like highlight that visually hides thin cursor
//! shapes.  Block cursor styles should still use REVERSED so the character
//! under the cursor remains readable.

use crate::common::harness::EditorTestHarness;
use fresh::config::{Config, CursorStyle};
use ratatui::style::Modifier;

/// Helper: type some text, move the cursor to the middle of the line, render,
/// and return the style of the cell under the hardware cursor.
fn cursor_cell_style(cursor_style: CursorStyle) -> (ratatui::style::Style, u16, u16) {
    let mut config = Config::default();
    config.editor.cursor_style = cursor_style;

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    harness.type_text("Hello World").unwrap();

    // Move cursor left so it sits on a character (the 'd' of "World")
    use crossterm::event::{KeyCode, KeyModifiers};
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    // Move to 'o' in "Hello" (5th char, the space)
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    let (cx, cy) = harness.screen_cursor_position();
    let style = harness
        .get_cell_style(cx, cy)
        .expect("cursor should be at a valid cell");
    (style, cx, cy)
}

/// Blinking bar cursor must NOT apply REVERSED to the primary cursor cell.
/// Before the fix this test fails: the cell had REVERSED, creating a block
/// highlight that hid the thin bar cursor.
#[test]
fn test_blinking_bar_no_reversed_on_primary_cursor() {
    let (style, cx, cy) = cursor_cell_style(CursorStyle::BlinkingBar);
    assert!(
        !style.add_modifier.contains(Modifier::REVERSED),
        "BlinkingBar: cell ({cx}, {cy}) must NOT have REVERSED modifier, \
         but style was {style:?}"
    );
}

/// Steady bar cursor must NOT apply REVERSED to the primary cursor cell.
#[test]
fn test_steady_bar_no_reversed_on_primary_cursor() {
    let (style, cx, cy) = cursor_cell_style(CursorStyle::SteadyBar);
    assert!(
        !style.add_modifier.contains(Modifier::REVERSED),
        "SteadyBar: cell ({cx}, {cy}) must NOT have REVERSED modifier, \
         but style was {style:?}"
    );
}

/// Blinking underline cursor must NOT apply REVERSED to the primary cursor cell.
#[test]
fn test_blinking_underline_no_reversed_on_primary_cursor() {
    let (style, cx, cy) = cursor_cell_style(CursorStyle::BlinkingUnderline);
    assert!(
        !style.add_modifier.contains(Modifier::REVERSED),
        "BlinkingUnderline: cell ({cx}, {cy}) must NOT have REVERSED modifier, \
         but style was {style:?}"
    );
}

/// Steady underline cursor must NOT apply REVERSED to the primary cursor cell.
#[test]
fn test_steady_underline_no_reversed_on_primary_cursor() {
    let (style, cx, cy) = cursor_cell_style(CursorStyle::SteadyUnderline);
    assert!(
        !style.add_modifier.contains(Modifier::REVERSED),
        "SteadyUnderline: cell ({cx}, {cy}) must NOT have REVERSED modifier, \
         but style was {style:?}"
    );
}

/// Block cursors MUST still use REVERSED so the character under the cursor
/// is readable (white text on white background would be invisible without it).
#[test]
fn test_blinking_block_keeps_reversed_on_primary_cursor() {
    let (style, cx, cy) = cursor_cell_style(CursorStyle::BlinkingBlock);
    assert!(
        style.add_modifier.contains(Modifier::REVERSED),
        "BlinkingBlock: cell ({cx}, {cy}) MUST have REVERSED modifier, \
         but style was {style:?}"
    );
}

/// Steady block cursor MUST still use REVERSED.
#[test]
fn test_steady_block_keeps_reversed_on_primary_cursor() {
    let (style, cx, cy) = cursor_cell_style(CursorStyle::SteadyBlock);
    assert!(
        style.add_modifier.contains(Modifier::REVERSED),
        "SteadyBlock: cell ({cx}, {cy}) MUST have REVERSED modifier, \
         but style was {style:?}"
    );
}
