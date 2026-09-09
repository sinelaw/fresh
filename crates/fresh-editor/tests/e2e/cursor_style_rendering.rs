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

/// Block cursors skip REVERSED on the primary cursor cell when a hardware
/// cursor is available (the default). The terminal's own block cursor provides
/// the visual indicator; adding REVERSED would cause double-inversion in
/// multiplexers like zellij, making the cursor invisible.
#[test]
fn test_blinking_block_skips_reversed_with_hardware_cursor() {
    let (style, cx, cy) = cursor_cell_style(CursorStyle::BlinkingBlock);
    assert!(
        !style.add_modifier.contains(Modifier::REVERSED),
        "BlinkingBlock: cell ({cx}, {cy}) must NOT have REVERSED modifier \
         when hardware cursor is available, but style was {style:?}"
    );
}

/// Steady block cursor also skips REVERSED when hardware cursor is available.
#[test]
fn test_steady_block_skips_reversed_with_hardware_cursor() {
    let (style, cx, cy) = cursor_cell_style(CursorStyle::SteadyBlock);
    assert!(
        !style.add_modifier.contains(Modifier::REVERSED),
        "SteadyBlock: cell ({cx}, {cy}) must NOT have REVERSED modifier \
         when hardware cursor is available, but style was {style:?}"
    );
}

/// **A cell the terminal inverts has to say what colour it is.**
///
/// With a block cursor in a terminal, the editor draws nothing at the caret
/// — it leaves the cell to the hardware cursor, which paints it by inverting
/// the cell's own two colours. Past the last character of a line no span had
/// ever covered the cell, so it carried only the ground's background: the
/// foreground half was `Reset`, i.e. *the terminal's* default foreground.
/// Inverting a cell whose foreground is the terminal's default draws the
/// block in that colour — white, in a terminal with a dark profile — so on
/// the `light` theme the cursor at end-of-line was white on white and simply
/// disappeared. (The same in a daemon session, for the same reason: the same
/// cells reach the same terminal.)
///
/// The ground now states both halves, so an end-of-line cursor inverts to
/// the theme's own foreground on its own background.
#[test]
fn end_of_line_cursor_cell_states_both_halves_of_the_ground() {
    let mut config = Config::default();
    config.theme = "light".into();
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    // The caret ends up one cell past the last character: end of line, which
    // is where nothing used to be painted.
    harness.type_text("hello").unwrap();
    harness.render().unwrap();

    let (cx, cy) = harness.screen_cursor_position();
    let style = harness
        .get_cell_style(cx, cy)
        .expect("the cursor should be at a valid cell");
    assert_eq!(
        style.fg,
        Some(ratatui::style::Color::Rgb(0, 0, 0)),
        "the cell an end-of-line cursor inverts must name the editor's \
         foreground, not fall through to the terminal's"
    );
    // The other half was never missing: here it is the current line's tint
    // (the caret's line is highlighted by default) rather than the plain
    // white ground, and either way it is a colour rather than `Reset`. What
    // matters is that the pair is complete, so the inversion is defined.
    assert_eq!(
        style.bg,
        Some(ratatui::style::Color::Rgb(245, 245, 245)),
        "the cursor's line carries the current-line tint"
    );
    assert_ne!(
        style.bg,
        Some(ratatui::style::Color::Reset),
        "a ground with an unstated half is what the inversion falls through"
    );
}
