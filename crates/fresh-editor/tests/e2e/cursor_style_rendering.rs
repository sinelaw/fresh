//! E2E tests for cursor style rendering (issue #851).
//!
//! When cursor_style is set to a non-block shape (bar or underline), the
//! primary cursor character cell must NOT have the REVERSED modifier.
//! REVERSED creates a block-like highlight that visually hides thin cursor
//! shapes.  Block cursor styles should still use REVERSED so the character
//! under the cursor remains readable.

use crate::common::harness::EditorTestHarness;
use fresh::config::{Config, CursorStyle};
use ratatui::style::{Color, Modifier};

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

/// A light (Dracula-pink) terminal cursor color. Light enough that a readable
/// glyph on top must be black.
const CURSOR_COLOR: Color = Color::Rgb(255, 121, 198);
/// A text foreground that collides with the cursor color (the original bug:
/// invisible glyph under the block cursor).
const COLLIDING_FG: Color = Color::Rgb(255, 121, 198);
/// A text foreground that already contrasts well with the cursor color — used to
/// prove the glyph is recolored regardless of its own color, not just on a
/// collision.
const CONTRASTING_FG: Color = Color::Rgb(20, 24, 60);
/// A distinct editor background so the recolored glyph is observable.
const EDITOR_BG: Color = Color::Rgb(40, 42, 54);

/// Render "Hello World" with `text_fg` as the foreground, move the cursor onto
/// a glyph, and return the rendered foreground of the cursor cell and of a
/// non-cursor neighbor cell. All colors are fixed constants the test controls,
/// so assertions read only from rendered output.
fn block_cursor_glyph_render(cursor_style: CursorStyle, text_fg: Color) -> (Color, Color) {
    use crossterm::event::{KeyCode, KeyModifiers};

    let mut config = Config::default();
    config.editor.cursor_style = cursor_style;
    // Keep the cursor cell bg as the plain editor bg (no current-line wash) so
    // the scenario is predictable.
    config.editor.highlight_current_line = false;
    config.editor.use_terminal_bg = false;

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    harness.editor_mut().override_theme_colors([
        ("editor.cursor", CURSOR_COLOR),
        ("editor.fg", text_fg),
        ("editor.bg", EDITOR_BG),
    ]);

    harness.type_text("Hello World").unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    let (cx, cy) = harness.screen_cursor_position();
    let cursor_fg = harness
        .get_cell_style(cx, cy)
        .and_then(|s| s.fg)
        .expect("cursor cell should have a foreground");
    // A glyph the cursor is NOT on must keep its syntax foreground — only the
    // single cursor cell is recolored.
    let other_fg = harness
        .get_cell_style(cx.saturating_sub(2), cy)
        .and_then(|s| s.fg)
        .expect("neighbor cell should have a foreground");
    (cursor_fg, other_fg)
}

/// A block cursor repaints its cell's glyph to a high-contrast color (black on a
/// light cursor) so the character stays readable — this is the original bug: a
/// glyph sharing the cursor color (e.g. a keyword in Dracula) was invisible.
#[test]
fn test_block_cursor_recolors_colliding_glyph() {
    let (cursor_fg, other_fg) = block_cursor_glyph_render(CursorStyle::SteadyBlock, COLLIDING_FG);
    assert_eq!(
        cursor_fg,
        Color::Black,
        "block cursor glyph must be repainted to contrast with the cursor color"
    );
    assert_eq!(
        other_fg, COLLIDING_FG,
        "non-cursor glyphs must keep their original foreground"
    );
}

/// The recolor depends only on the cursor color, not the glyph's own color: even
/// a glyph that already contrasts well with the cursor is repainted, so the
/// block cursor reads consistently like reverse video.
#[test]
fn test_block_cursor_recolors_regardless_of_glyph_color() {
    let (cursor_fg, other_fg) = block_cursor_glyph_render(CursorStyle::SteadyBlock, CONTRASTING_FG);
    assert_eq!(
        cursor_fg,
        Color::Black,
        "block cursor glyph is recolored to contrast with the cursor color regardless of its own color"
    );
    assert_eq!(
        other_fg, CONTRASTING_FG,
        "non-cursor glyphs keep their original foreground"
    );
}

/// A bar cursor leaves the glyph visible, so its foreground must NOT be
/// recolored even when it collides with the cursor color.
#[test]
fn test_bar_cursor_does_not_recolor_glyph() {
    let (cursor_fg, _) = block_cursor_glyph_render(CursorStyle::SteadyBar, COLLIDING_FG);
    assert_eq!(
        cursor_fg, COLLIDING_FG,
        "bar cursor must leave the glyph foreground untouched"
    );
}
