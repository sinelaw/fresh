//! Tests for the smart_home action.
//!
//! Smart home toggles between the first non-whitespace character and the line
//! start.  When line wrapping is enabled it must operate on the *visual*
//! (soft-wrapped) line, not the full physical line.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;

/// Helper to create a config with line wrapping enabled
fn config_with_line_wrap() -> Config {
    let mut config = Config::default();
    config.editor.line_wrap = true;
    config
}

/// With line wrapping on, pressing Home on a wrapped continuation line should
/// move to the start of the *visual* line, not jump to the beginning of the
/// physical line.
#[test]
fn test_smart_home_respects_soft_wrap() {
    const WIDTH: u16 = 40;
    const HEIGHT: u16 = 10;

    let mut harness =
        EditorTestHarness::with_config(WIDTH, HEIGHT, config_with_line_wrap()).unwrap();

    // Create a line with leading whitespace that is long enough to wrap.
    // With width 40, ~5 gutter, 1 scrollbar ≈ 34 chars visible per visual line.
    // "    " (4 spaces) + 60 'A's = 64 chars → wraps into at least 2 visual rows.
    let line = format!("    {}", "A".repeat(60));
    let _fixture = harness.load_buffer_from_text(&line).unwrap();
    harness.render().unwrap();

    // Move cursor to somewhere in the middle of the wrapped continuation line.
    // End key goes to end of the visual line; pressing Down then End lands on
    // the second visual row.  We'll use the right arrow to get into the second
    // visual row instead, which is simpler.
    // Position the cursor past the first visual row (at char 34+).
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Now cursor should be at the end of the first visual line (not the end of
    // the physical line, because line_wrap visual End stops at visual boundary).
    let pos_after_end = harness.cursor_position();

    // Move down to the second visual line, then move right a few chars
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();
    let pos_on_wrapped_line = harness.cursor_position();

    // The cursor should be past the first visual row boundary
    assert!(
        pos_on_wrapped_line > pos_after_end,
        "Cursor should be on the wrapped continuation line (pos {} should be > {})",
        pos_on_wrapped_line,
        pos_after_end
    );

    // Press Home — should go to start of the visual (wrapped) line, NOT to
    // byte 0 of the physical line.
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let pos_after_home = harness.cursor_position();

    // The position must still be in the second visual row (not the physical
    // line start at byte 0).
    assert!(
        pos_after_home > 0,
        "Smart home on a wrapped line should go to the visual line start, not byte 0. \
         Got position {}",
        pos_after_home
    );
    // Specifically, it should be >= the end of the first visual row
    assert!(
        pos_after_home >= pos_after_end,
        "Smart home should stay on the wrapped continuation line. \
         pos_after_home={} should be >= first visual line end={}",
        pos_after_home,
        pos_after_end
    );
}

/// Regression: smart home must run for every cursor in a multi-cursor edit,
/// not only the primary. Pre-fix, the primary moved to first-non-ws / line
/// start but secondaries stayed put, and a follow-up insert landed at
/// end-of-line on those secondaries instead of at the indent column.
#[test]
fn test_smart_home_runs_on_all_cursors() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Two indented lines. Use long content so cursors sit mid-line — placing
    // a cursor exactly at line-end lands it on the `\n` boundary which the
    // line iterator attributes to the *next* line.
    harness.type_text("    aaaaaa").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("    bbbbbb").unwrap();
    harness.assert_buffer_content("    aaaaaa\n    bbbbbb");

    // Place the primary cursor mid-way through line 1, then add a secondary
    // below. Both cursors now sit mid-line, past the first-non-whitespace
    // column — that's the precondition the bug needs to surface (a Home press
    // must move BOTH cursors back to col 4, not just the primary).
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    for _ in 0..9 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.editor_mut().add_cursor_below();

    // First Home press: smart_home should toggle BOTH cursors to first-non-ws
    // (column 4 on each line). Typing 'X' then inserts at column 4 on every
    // line, sliding the existing content right.
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.type_text("X").unwrap();
    harness.render().unwrap();
    harness.assert_buffer_content("    Xaaaaaa\n    Xbbbbbb");

    // Second pair of Home presses: from "just after X" (col 5), one Home
    // toggles back to first-non-ws (col 4 — the X), and a second Home toggles
    // to line start (col 0) on BOTH cursors. Typing 'Z' inserts at column 0
    // of every line.
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.type_text("Z").unwrap();
    harness.render().unwrap();
    harness.assert_buffer_content("Z    Xaaaaaa\nZ    Xbbbbbb");
}

/// With line wrapping on, an indent of multi-byte whitespace (here an
/// ideographic space, U+3000, three bytes in UTF-8) must put the cursor on
/// the character after it. Counting the indent in characters and adding the
/// count to a byte position put the cursor inside the space instead, and
/// the next edit there split the character.
#[test]
fn test_smart_home_multibyte_indent_with_line_wrap() {
    let mut harness = EditorTestHarness::with_config(80, 24, config_with_line_wrap()).unwrap();
    let _fixture = harness.load_buffer_from_text("\u{3000}abc").unwrap();
    harness.render().unwrap();

    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.type_text("X").unwrap();
    harness.assert_buffer_content("\u{3000}Xabc");

    // From just after X, Home goes back to the first non-whitespace
    // character, and again to the line start.
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.type_text("Z").unwrap();
    harness.assert_buffer_content("Z\u{3000}Xabc");
}
