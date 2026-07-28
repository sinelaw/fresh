//! Regression test for issue #2796: cursor movement duplicates when
//! `keyboard_report_event_types` is enabled.
//!
//! With the kitty keyboard protocol's event-type reporting on, one physical
//! keypress produces a press, a repeat per auto-repeat tick while held, and a
//! release. The arrows and the `CSI <n> ~` editing keys keep their *legacy*
//! escape-code form under that flag and carry the event type as a
//! sub-parameter of the modifier field, so a parser that reads only the
//! modifiers reports the release as a second press — and the cursor moves
//! twice for one keypress.
//!
//! These drive the editor with the exact byte stream such a terminal sends and
//! assert only on what the status bar shows.

use crate::common::harness::EditorTestHarness;
use crossterm::event::Event;
use fresh::server::input_parser::InputParser;

/// Feed raw terminal bytes through the same parser the event loops use, and
/// dispatch whatever events they produce into the editor.
fn feed(harness: &mut EditorTestHarness, parser: &mut InputParser, bytes: &[u8]) {
    for event in parser.parse(bytes) {
        if let Event::Key(key_event) = event {
            harness.send_key_event(key_event).unwrap();
        }
    }
}

/// The "Ln <n>, Col <n>" readout from the status bar, as rendered.
fn line_col(harness: &EditorTestHarness) -> String {
    let status = harness.get_status_bar();
    let start = status
        .find("Ln ")
        .unwrap_or_else(|| panic!("no line/column readout in status bar: {status:?}"));
    let rest = &status[start..];
    let end = rest.find("  ").unwrap_or(rest.len());
    rest[..end].trim().to_string()
}

/// One Down keypress — press then release, as the terminal sends it — must
/// move the cursor exactly one line.
#[test]
fn arrow_key_release_does_not_move_the_cursor_again() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();
    harness
        .load_buffer_from_text("aaaa\nbbbb\ncccc\ndddd\neeee\n")
        .unwrap();
    harness.render().unwrap();
    let mut parser = InputParser::new();

    assert_eq!(line_col(&harness), "Ln 1, Col 1");

    // Down: press (legacy form, unchanged by the flag) then release.
    feed(&mut harness, &mut parser, b"\x1b[B");
    assert_eq!(line_col(&harness), "Ln 2, Col 1", "press should move once");
    feed(&mut harness, &mut parser, b"\x1b[1;1:3B");
    assert_eq!(
        line_col(&harness),
        "Ln 2, Col 1",
        "release must not move the cursor a second time"
    );

    // Right, in the same shape.
    feed(&mut harness, &mut parser, b"\x1b[C");
    feed(&mut harness, &mut parser, b"\x1b[1;1:3C");
    assert_eq!(line_col(&harness), "Ln 2, Col 2");
}

/// Auto-repeat while a key is held is a keystroke and must still move the
/// cursor — the fix for the release must not silence repeats.
#[test]
fn arrow_key_repeat_still_moves_the_cursor() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();
    harness
        .load_buffer_from_text("aaaa\nbbbb\ncccc\ndddd\neeee\n")
        .unwrap();
    harness.render().unwrap();
    let mut parser = InputParser::new();

    // Press, two auto-repeat ticks, then release: three lines down in total.
    feed(&mut harness, &mut parser, b"\x1b[B");
    feed(&mut harness, &mut parser, b"\x1b[1;1:2B");
    feed(&mut harness, &mut parser, b"\x1b[1;1:2B");
    feed(&mut harness, &mut parser, b"\x1b[1;1:3B");

    assert_eq!(line_col(&harness), "Ln 4, Col 1");
}

/// The same duplication hit the `CSI <n> ~` editing keys, where it destroyed
/// text rather than just moving the cursor: one Delete removed two characters.
#[test]
fn delete_key_release_does_not_delete_again() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();
    harness.load_buffer_from_text("abcdef\n").unwrap();
    harness.render().unwrap();
    let mut parser = InputParser::new();

    let (content_first_row, _) = harness.content_area_rows();
    let text_row = |h: &EditorTestHarness| h.get_screen_row(content_first_row);
    assert!(text_row(&harness).contains("abcdef"));

    feed(&mut harness, &mut parser, b"\x1b[3~");
    feed(&mut harness, &mut parser, b"\x1b[3;1:3~");

    let row = text_row(&harness);
    assert!(
        row.contains("bcdef"),
        "one Delete keypress must remove exactly one character, got {row:?}"
    );
}
