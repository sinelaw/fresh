//! Migration of `tests/e2e/line_wrap_parity.rs` — parity invariants
//! between the `LineWrapCache` / `ViewLine` data the renderer paints
//! from and the scroll-math / cursor-positioning surfaces that the
//! rest of the editor reads (see `docs/internal/line-wrap-cache-plan.md`).
//!
//! Load-bearing claims preserved here:
//!
//!   1. **Cursor-on-screen parity.** On a word-wrapped buffer, the
//!      character at the cursor's hardware position must match the
//!      character the cursor logically points to. Pre-refactor,
//!      char-wrap inside `cursor_screen_position` disagreed with the
//!      renderer's word-wrap and could put the hardware cursor a row
//!      off — the user would see the cursor on a different cell than
//!      the buffer thought.
//!
//!   2. **Thumb-vs-content parity.** Dragging the scrollbar thumb to
//!      the bottom of the track must show the buffer's TAIL_MARKER.
//!      If the thumb's reported total visual rows underreported the
//!      buffer (as pre-refactor `wrap_line` did vs. the renderer's
//!      word-aware wrap), the drag would stop short.
//!
//!   3. **Thumb consistency across widths.** Drag-to-bottom must
//!      reach the tail marker at multiple widths — a stronger
//!      version of #2 that exercises the wrap-vs-thumb agreement
//!      under varied row counts.
//!
//! ## Harness-direct pattern
//!
//! All three claims need surfaces that have no `EditorTestApi`
//! projection: `mouse_drag` and `content_area_rows` for the scroll
//! sweep, `screen_cursor_position` + `get_screen_row` for the
//! hardware-cursor check, and `editor().active_cursors().primary().position`
//! for the byte the cursor logically points to. The migrated tests
//! take the harness-direct path (the same pattern
//! `migrated_horizontal_scrollbar.rs` uses for scrollbar geometry).
//!
//! Source: `tests/e2e/line_wrap_parity.rs` (3 tests migrated; no
//! tests deferred).

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;

fn config_with_wrap() -> Config {
    let mut config = Config::default();
    config.editor.line_wrap = true;
    config
}

/// Realistic word-wrapped buffer — the kind of text where
/// `wrap_line`'s char-wrap and the renderer's word-boundary wrap
/// disagreed before the refactor. The TAIL_MARKER lets a drag-to-end
/// test confirm the bottom of the buffer is reachable.
fn word_wrapped_buffer() -> String {
    let para: String = (0..25)
        .map(|i| format!("word{:02}", i))
        .collect::<Vec<_>>()
        .join(" ");
    let mut lines = Vec::new();
    for _ in 0..6 {
        lines.push(para.clone());
    }
    lines.push("TAIL_MARKER_XYZ".to_string());
    lines.join("\n")
}

#[test]
fn migrated_cursor_hardware_position_matches_content_under_cursor() {
    // Original: `cursor_hardware_position_matches_content_under_cursor`.
    // Multiple widths exercise wrap boundaries at different
    // positions — the old char-wrap/word-wrap drift surfaced near
    // mid-line on several widths.
    for &width in &[60u16, 80, 100] {
        let mut harness =
            EditorTestHarness::with_config(width, 20, config_with_wrap()).expect("harness");
        harness
            .load_buffer_from_text(&word_wrapped_buffer())
            .expect("load");
        harness.render().expect("render");

        // Ctrl+Home → start; then Down to enter the wrapped
        // paragraph; then 45 Right to land mid-line near a wrap
        // boundary on several widths.
        harness
            .send_key(KeyCode::Home, KeyModifiers::CONTROL)
            .expect("ctrl+home");
        harness
            .send_key(KeyCode::Down, KeyModifiers::NONE)
            .expect("down");
        for _ in 0..45 {
            harness
                .send_key(KeyCode::Right, KeyModifiers::NONE)
                .expect("right");
        }
        harness.render().expect("render");

        let cursor_byte = harness.editor().active_cursors().primary().position;
        let buffer_content = harness.get_buffer_content().expect("content");
        let expected_char = buffer_content.as_bytes().get(cursor_byte).copied();

        let (hw_col, hw_row) = harness.screen_cursor_position();
        let row_text = harness.get_screen_row(hw_row as usize);

        let row_chars: Vec<char> = row_text.chars().collect();
        let at_cursor = row_chars.get(hw_col as usize).copied();

        let expected = expected_char.map(|b| b as char);
        // If the cursor is at EOL (past last char) the expected
        // may be `\n` / `None`; skip the strict check there — we
        // only care about the printable-char case where drift was
        // visible.
        if let Some(exp) = expected {
            if !exp.is_ascii_whitespace() && exp != '\n' {
                assert_eq!(
                    at_cursor,
                    Some(exp),
                    "[w={width}] cursor hardware position ({hw_col},{hw_row}) \
                     shows {at_cursor:?} but the cursor byte ({cursor_byte}) \
                     is at {exp:?}.\nRow: {row_text:?}",
                );
            }
        }
    }
}

#[test]
fn migrated_scrollbar_thumb_reaches_bottom_on_word_wrapped_buffer() {
    // Original: `scrollbar_thumb_reaches_bottom_on_word_wrapped_buffer`.
    const WIDTH: u16 = 80;
    const HEIGHT: u16 = 20;

    let mut harness =
        EditorTestHarness::with_config(WIDTH, HEIGHT, config_with_wrap()).expect("harness");
    harness
        .load_buffer_from_text(&word_wrapped_buffer())
        .expect("load");
    harness.render().expect("render");

    let scrollbar_col = WIDTH - 1;
    let (first, last) = harness.content_area_rows();
    harness
        .mouse_drag(scrollbar_col, first as u16, scrollbar_col, last as u16)
        .expect("drag");
    harness.render().expect("render");

    let content: Vec<String> = (first..=last).map(|r| harness.get_screen_row(r)).collect();
    let visible = content.iter().any(|row| row.contains("TAIL_MARKER_XYZ"));
    assert!(
        visible,
        "after dragging the scrollbar thumb to the bottom of the track, \
         the buffer's TAIL_MARKER should be visible. Content:\n{}",
        content.join("\n"),
    );
}

#[test]
fn migrated_drag_to_bottom_reaches_end_at_multiple_widths() {
    // Original: `drag_to_bottom_reaches_end_at_multiple_widths`. Same
    // drag-to-end probe across multiple widths so the wrap-vs-thumb
    // agreement is exercised at varied row counts (narrower → more
    // wrapped rows → bigger total-row count for the thumb to track).
    for &width in &[50u16, 70, 90, 120] {
        let mut harness =
            EditorTestHarness::with_config(width, 20, config_with_wrap()).expect("harness");
        harness
            .load_buffer_from_text(&word_wrapped_buffer())
            .expect("load");
        harness.render().expect("render");

        let scrollbar_col = width - 1;
        let (first, last) = harness.content_area_rows();
        harness
            .mouse_drag(scrollbar_col, first as u16, scrollbar_col, last as u16)
            .expect("drag");
        harness.render().expect("render");

        let content: Vec<String> = (first..=last).map(|r| harness.get_screen_row(r)).collect();
        let visible = content.iter().any(|row| row.contains("TAIL_MARKER_XYZ"));
        assert!(
            visible,
            "[w={width}] thumb drag should reach TAIL_MARKER. Content:\n{}",
            content.join("\n"),
        );
    }
}

/// Anti-test: drop the `mouse_drag` call. Without the drag, the
/// viewport stays at the top of the buffer and the TAIL_MARKER must
/// NOT be visible in the content area — proves the visibility claim
/// in the positive thumb-reach test depends on the actual scrollbar
/// drag, not on the buffer accidentally fitting on screen.
#[test]
fn anti_scrollbar_thumb_without_drag_keeps_tail_off_screen() {
    const WIDTH: u16 = 80;
    const HEIGHT: u16 = 20;

    let mut harness =
        EditorTestHarness::with_config(WIDTH, HEIGHT, config_with_wrap()).expect("harness");
    harness
        .load_buffer_from_text(&word_wrapped_buffer())
        .expect("load");
    harness.render().expect("render");
    // No mouse_drag here — that's the load-bearing step we drop.

    let (first, last) = harness.content_area_rows();
    let content: Vec<String> = (first..=last).map(|r| harness.get_screen_row(r)).collect();
    let visible = content.iter().any(|row| row.contains("TAIL_MARKER_XYZ"));
    assert!(
        !visible,
        "anti: without the scrollbar drag, TAIL_MARKER (last line of \
         a 6-paragraph word-wrapped buffer at width=80 / height=20) \
         must NOT be visible in the initial viewport. Content:\n{}",
        content.join("\n"),
    );
}

/// Anti-test: drop the `Down` + 45 `Right` keystrokes that move the
/// cursor into a wrapped row. Without them, the cursor stays at
/// buffer offset 0 — proves the hardware-cursor match in the
/// positive test is gated on the cursor actually moving into a
/// wrapped position, not on the cursor sitting at (0,0) trivially.
#[test]
fn anti_cursor_parity_without_movement_stays_at_buffer_start() {
    let mut harness =
        EditorTestHarness::with_config(80, 20, config_with_wrap()).expect("harness");
    harness
        .load_buffer_from_text(&word_wrapped_buffer())
        .expect("load");
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .expect("ctrl+home");
    // No Down, no Right — the cursor should still be at byte 0.
    harness.render().expect("render");

    let cursor_byte = harness.editor().active_cursors().primary().position;
    assert_eq!(
        cursor_byte, 0,
        "anti: without Down + Right movement after Ctrl+Home, the \
         primary cursor must remain at byte 0 (got {cursor_byte}). \
         The positive parity test depends on the cursor actually \
         being moved into the wrapped paragraph."
    );
}
