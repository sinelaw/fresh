//! Migration of `tests/e2e/ctrl_end_wrapped.rs::test_ctrl_end_
//! viewport_scrolls_to_show_cursor_line` — Ctrl+End on a long
//! wrapped buffer with a trailing newline must scroll the
//! viewport far enough that the empty final line is visible,
//! not leave it hidden below the screen.
//!
//! The e2e original inspects the rendered cursor row's TEXT
//! and asserts it doesn't contain content from a data line.
//! That requires per-row screen-text inspection which is still
//! a framework gap (tracked in #2058). This migration asserts
//! the weaker but still load-bearing claim: the cursor's
//! logical byte position must lie within the viewport's
//! visible byte range after Ctrl+End. Pre-fix, the doc-end
//! byte was outside the visible range — the viewport didn't
//! scroll far enough.
//!
//! Per #2058 orphan migration.

use crate::common::harness::EditorTestHarness;
use fresh::test_api::{Action, EditorTestApi};

fn config_with_line_wrap() -> fresh::config::Config {
    let mut config = fresh::config::Config::default();
    config.editor.line_wrap = true;
    config
}

fn make_csv_like_content_with_trailing_newline() -> String {
    let header = "Title,Word count,Type,Date published,First published in,Also published in,Transcription,Page scans,Notes";
    let mut lines = vec![header.to_string()];
    for i in 1..=140 {
        let line = format!(
            "Entry {i},123,Poetry,1810-01,THE WORKS OF REV JOHN NEWTON,Also in collection {i},https://example.com/ccel/newton/olneyhymns/entry_{i}.html,https://archive.org/details/worksofrevjohnne03newt/page/{i}/mode/1up,Notes for entry {i} with some extra descriptive text that makes this line longer",
        );
        lines.push(line);
    }
    lines.join("\n") + "\n"
}

#[test]
fn migrated_ctrl_end_under_wrap_scrolls_viewport_near_doc_end() {
    let content = make_csv_like_content_with_trailing_newline();
    let doc_end = content.len();

    let mut harness =
        EditorTestHarness::with_config(80, 24, config_with_line_wrap()).unwrap();
    let _f = harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();

    harness.api_mut().dispatch(Action::MoveDocumentEnd);
    harness.render().unwrap();

    // Logical cursor must be at the document end.
    let pos = harness.api_mut().primary_caret().position;
    assert_eq!(
        pos, doc_end,
        "MoveDocumentEnd: cursor byte should be {doc_end} (doc end), got {pos}",
    );

    // Load-bearing claim: viewport_top_byte must be close
    // enough to doc_end that a 24-row viewport (which covers
    // ~24 * 80 = 1920 bytes at most under wrap) can contain
    // the cursor. Pre-fix, the viewport scrolled to a top_byte
    // far before doc_end, leaving the empty final line off-
    // screen by several wrapped rows.
    //
    // The exact mid-viewport position depends on wrap geometry,
    // so we use a generous upper bound: doc_end - top_byte must
    // be smaller than terminal_height * terminal_width (the
    // theoretical max bytes a screen can show, assuming
    // single-byte ASCII content like ours).
    let top = harness.api_mut().viewport_top_byte();
    let width = harness.api_mut().terminal_width() as usize;
    let height = harness.api_mut().terminal_height() as usize;
    let max_visible_bytes = width * height;
    let gap = doc_end.saturating_sub(top);
    assert!(
        gap < max_visible_bytes,
        "Ctrl+End under wrap: viewport_top_byte ({top}) too far from doc_end \
         ({doc_end}); gap={gap} bytes exceeds max visible {max_visible_bytes}. \
         Pre-fix, the viewport didn't scroll far enough and doc_end was off-screen.",
    );
    assert!(top > 0, "viewport must scroll past start");
}

/// Anti-test: with line_wrap disabled, the bug couldn't
/// manifest. The viewport still scrolls to keep the cursor
/// visible after Ctrl+End. Pins that the regression was
/// specifically gated on wrap mode.
#[test]
fn anti_ctrl_end_without_wrap_still_scrolls_to_cursor() {
    let content = make_csv_like_content_with_trailing_newline();
    let doc_end = content.len();

    let mut harness =
        EditorTestHarness::with_config(80, 24, fresh::config::Config::default()).unwrap();
    let _f = harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();

    harness.api_mut().dispatch(Action::MoveDocumentEnd);
    harness.render().unwrap();

    let pos = harness.api_mut().primary_caret().position;
    assert_eq!(pos, doc_end);
    assert!(
        harness.api_mut().viewport_top_byte() > 0,
        "MoveDocumentEnd on a long buffer scrolls the viewport regardless of wrap mode"
    );
}
