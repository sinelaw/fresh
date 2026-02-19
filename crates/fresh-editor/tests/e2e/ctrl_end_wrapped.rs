//! Tests for Ctrl+End with soft-wrapped lines.
//!
//! Bug: when line wrapping is on and the file has long lines, Ctrl+End moves
//! the cursor to the correct byte position (document end) but the viewport
//! doesn't scroll far enough.  The cursor's visual row ends up hidden below
//! the visible area — the rendered cursor lands on the last visible content
//! row instead of on the actual last line of the file.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;

fn config_with_line_wrap() -> Config {
    let mut config = Config::default();
    config.editor.line_wrap = true;
    config
}

/// Build CSV-like content: 140 long lines (each wraps to ~5 visual rows)
/// followed by a trailing newline, so there is an empty final line 142.
fn make_csv_like_content_with_trailing_newline() -> String {
    let header = "Title,Word count,Type,Date published,First published in,Also published in,Transcription,Page scans,Notes";
    let mut lines = vec![header.to_string()];
    for i in 1..=140 {
        let line = format!(
            "Entry {i},123,Poetry,1810-01,THE WORKS OF REV JOHN NEWTON,Also in collection {i},https://example.com/ccel/newton/olneyhymns/entry_{i}.html,https://archive.org/details/worksofrevjohnne03newt/page/{i}/mode/1up,Notes for entry {i} with some extra descriptive text that makes this line longer",
        );
        lines.push(line);
    }
    lines.join("\n") + "\n" // trailing newline → empty final line
}

/// Ctrl+End on a file with many long wrapping lines and a trailing newline.
///
/// The cursor byte position goes to `buffer.len()` (the empty last line),
/// but the viewport must scroll far enough that the cursor's rendered row
/// actually shows that empty line — not a continuation of the previous
/// long line.
///
/// Reproduces: "cursor goes on the last visible line on screen, which is
/// somewhere in the middle of the last actual line of the file."
#[test]
fn test_ctrl_end_viewport_scrolls_to_show_cursor_line() {
    let content = make_csv_like_content_with_trailing_newline();
    let doc_end = content.len();

    let mut harness = EditorTestHarness::with_config(80, 24, config_with_line_wrap()).unwrap();
    let _fixture = harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();

    // Press Ctrl+End
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Logical position must be at document end
    let cursor_pos = harness.cursor_position();
    assert_eq!(
        cursor_pos, doc_end,
        "Ctrl+End: cursor byte should be {} (doc end), got {}",
        doc_end, cursor_pos
    );

    // Now inspect the RENDERED cursor row.
    // The cursor is logically on the empty final line (after the trailing \n).
    // The rendered row at the cursor must be that empty line — it must NOT
    // contain content from the previous data line (Entry 140).
    let (_cx, cy) = harness.screen_cursor_position();
    let cursor_row = harness.get_row_text(cy);

    // If the cursor row contains data-line content, the viewport didn't
    // scroll far enough — the empty last line is hidden below the screen.
    let has_data_content = cursor_row.contains("entry_")
        || cursor_row.contains("Entry ")
        || cursor_row.contains(".html")
        || cursor_row.contains("example.com")
        || cursor_row.contains("archive.org")
        || cursor_row.contains("NEWTON")
        || cursor_row.contains("Poetry")
        || cursor_row.contains("longer");

    assert!(
        !has_data_content,
        "Ctrl+End: the rendered cursor (row {}) should be on the empty final \
         line, but it shows content from a previous data line. The viewport \
         didn't scroll far enough.\n\
         Cursor row text: {:?}\n\
         Screen:\n{}",
        cy,
        cursor_row.trim(),
        harness.screen_to_string()
    );
}

/// After Ctrl+End, pressing Left moves to the end of the previous content
/// line (correct).  From there, pressing Down should return to the empty
/// trailing line — the same position Ctrl+End reached.
#[test]
fn test_down_from_last_content_line_reaches_trailing_empty_line() {
    let content = make_csv_like_content_with_trailing_newline();
    let doc_end = content.len();

    let mut harness = EditorTestHarness::with_config(80, 24, config_with_line_wrap()).unwrap();
    let _fixture = harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();

    // Ctrl+End → empty trailing line
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    assert_eq!(harness.cursor_position(), doc_end);

    // Left → end of previous content line
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let pos_after_left = harness.cursor_position();
    assert!(
        pos_after_left < doc_end,
        "Left from doc end should move into the previous line, got {}",
        pos_after_left
    );

    // Down → should return to the empty trailing line (doc_end)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let pos_after_down = harness.cursor_position();

    // Also check cursor row — should be on the empty trailing line
    let (_cx, cy) = harness.screen_cursor_position();
    let cursor_row = harness.get_row_text(cy);
    let has_data_content = cursor_row.contains("entry_")
        || cursor_row.contains("Entry ")
        || cursor_row.contains(".html")
        || cursor_row.contains("example.com")
        || cursor_row.contains("archive.org")
        || cursor_row.contains("NEWTON")
        || cursor_row.contains("Poetry")
        || cursor_row.contains("longer");

    assert_eq!(
        pos_after_down,
        doc_end,
        "Down from last content line should reach the trailing empty line ({}), \
         got {} (left was at {})\n\
         Cursor row text: {:?}\n\
         Screen:\n{}",
        doc_end,
        pos_after_down,
        pos_after_left,
        cursor_row.trim(),
        harness.screen_to_string()
    );

    assert!(
        !has_data_content,
        "Down: cursor row ({}) should be the empty trailing line, not data content.\n\
         Cursor row text: {:?}\n\
         Screen:\n{}",
        cy,
        cursor_row.trim(),
        harness.screen_to_string()
    );
}
