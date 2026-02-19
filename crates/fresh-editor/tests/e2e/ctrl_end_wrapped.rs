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

/// After Ctrl+End → Left the cursor is on the last content line.
/// Pressing Down should move to the trailing empty line.  Verify via
/// the rendered cursor row (not just the byte position).
#[test]
fn test_down_from_last_content_line_reaches_trailing_empty_line() {
    // Use a wider terminal (135x37) — the bug only manifests when the
    // content area is wide enough that lines wrap into fewer visual rows.
    let mut harness = EditorTestHarness::with_config(135, 37, config_with_line_wrap()).unwrap();
    // Load the real CSV file whose line lengths and content trigger the bug.
    let content = std::fs::read_to_string("/home/noam/Downloads/olney-book-1.csv")
        .expect("need olney-book-1.csv to reproduce this bug");
    let doc_end = content.len();
    let _fixture = harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();

    // Ctrl+End → empty trailing line
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Record the rendered cursor position at doc end — this is where Down
    // should return us to.
    let (_end_cx, end_cy) = harness.screen_cursor_position();
    let end_row_text = harness.get_row_text(end_cy);

    // Left → end of previous content line
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let pos_after_left = harness.cursor_position();
    let (_left_cx, left_cy) = harness.screen_cursor_position();

    // Down → should return to the empty trailing line (doc_end)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let pos_after_down = harness.cursor_position();
    let (_down_cx, down_cy) = harness.screen_cursor_position();
    let down_row_text = harness.get_row_text(down_cy);

    // The rendered cursor row must show the empty trailing line, not
    // content from the previous data line.
    let has_data_content = down_row_text.contains("entry_")
        || down_row_text.contains("Entry ")
        || down_row_text.contains(".html")
        || down_row_text.contains("example.com")
        || down_row_text.contains("archive.org")
        || down_row_text.contains("NEWTON")
        || down_row_text.contains("Poetry")
        || down_row_text.contains("longer");

    assert!(
        !has_data_content,
        "Down after Left from Ctrl+End: the rendered cursor (row {}) should be \
         on the empty final line, but it shows data content. The cursor did not \
         move down to the trailing empty line.\n\
         pos_after_left={}, pos_after_down={}, doc_end={}\n\
         left_cy={}, down_cy={}, end_cy={}\n\
         Ctrl+End row text: {:?}\n\
         Down row text: {:?}\n\
         Screen:\n{}",
        down_cy,
        pos_after_left,
        pos_after_down,
        doc_end,
        left_cy,
        down_cy,
        end_cy,
        end_row_text.trim(),
        down_row_text.trim(),
        harness.screen_to_string()
    );
}

/// After Ctrl+End with line wrapping on, disabling line wrap via the
/// command palette should keep the cursor on the trailing empty line —
/// not on a tilde row below it.
#[test]
fn test_ctrl_end_then_disable_line_wrap_cursor_row() {
    let mut harness = EditorTestHarness::with_config(135, 37, config_with_line_wrap()).unwrap();
    let content = std::fs::read_to_string("/home/noam/Downloads/olney-book-1.csv")
        .expect("need olney-book-1.csv to reproduce this bug");
    let doc_end = content.len();
    let _fixture = harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();

    // Ctrl+End → cursor on trailing empty line
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    assert_eq!(harness.cursor_position(), doc_end);

    // Toggle line wrap off via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Toggle Line Wrap").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Cursor byte position should still be at doc end
    assert_eq!(
        harness.cursor_position(),
        doc_end,
        "Cursor byte should remain at doc end after toggling line wrap off"
    );

    // The rendered cursor row must be the empty trailing line, not a tilde
    let (_cx, cy) = harness.screen_cursor_position();
    let cursor_row = harness.get_row_text(cy);

    assert!(
        !cursor_row.contains('~'),
        "After Ctrl+End then disabling line wrap, the cursor (row {}) landed on \
         a tilde row instead of the empty trailing line.\n\
         Cursor row text: {:?}\n\
         Screen:\n{}",
        cy,
        cursor_row.trim(),
        harness.screen_to_string()
    );
}
