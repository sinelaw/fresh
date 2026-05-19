//! Migration of `tests/e2e/ctrl_end_wrapped.rs` — Ctrl+End on
//! a long wrapped buffer with a trailing newline must scroll
//! the viewport far enough that the empty final line is
//! visible, not leave it hidden below the screen. Also covers
//! the Down-from-last-content-line and toggle-line-wrap-after-
//! Ctrl+End follow-ups that shared the same regression root.
//!
//! Per-row text inspection now uses
//! `RenderSnapshot::extract_with_rendered_rows` +
//! `RowMatch::NoRowContains` (the framework extension landed
//! per #2058 step 5). Each positive test asserts on the
//! cursor's rendered row by index, plus the "no data content
//! anywhere on the cursor row" claim from the original e2e
//! assertion (mirrored substring set).
//!
//! Source: `tests/e2e/ctrl_end_wrapped.rs` (3 tests; 0 deferred).

use crate::common::harness::EditorTestHarness;
use crate::common::scenario::render_snapshot::RenderSnapshot;
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

/// Mirror of `test_down_from_last_content_line_reaches_
/// trailing_empty_line` — after Ctrl+End → Left the cursor is
/// on the last content line; pressing Down should move to the
/// trailing empty line. Inspect the rendered cursor row: it
/// must NOT contain data-line content.
///
/// Wider terminal (135x37) is load-bearing: at this width the
/// content lines wrap into fewer visual rows, and the bug only
/// manifests in that geometry (per the original).
#[test]
fn migrated_down_from_last_content_line_reaches_trailing_empty_line() {
    let content = make_csv_like_content_with_trailing_newline();
    let doc_end = content.len();

    let mut harness =
        EditorTestHarness::with_config(135, 37, config_with_line_wrap()).unwrap();
    let _f = harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();

    // Ctrl+End → empty trailing line.
    harness.api_mut().dispatch(Action::MoveDocumentEnd);
    harness.render().unwrap();
    assert_eq!(
        harness.api_mut().primary_caret().position,
        doc_end,
        "MoveDocumentEnd must reach the trailing empty line byte"
    );

    // Left → end of previous content line.
    harness.api_mut().dispatch(Action::MoveLeft);
    harness.render().unwrap();

    // Down → must return to the trailing empty line (doc_end).
    harness.api_mut().dispatch(Action::MoveDown);
    let snap = RenderSnapshot::extract_with_rendered_rows(&mut harness);

    let pos_after_down = harness.api_mut().primary_caret().position;
    assert_eq!(
        pos_after_down, doc_end,
        "Down after Left from doc end should move cursor byte back to doc_end ({doc_end}), got {pos_after_down}",
    );

    let (_cx, cy) = snap.hardware_cursor.expect("hardware cursor must be set");

    // Mirror the e2e assertion: the rendered cursor row must
    // not contain any of these data-line substrings. RowMatch
    // doesn't have a negative-row-at-index variant, so inspect
    // `rendered_rows[cy]` directly (snapshot is built with
    // `extract_with_rendered_rows`).
    let cursor_row = snap
        .rendered_rows
        .get(cy as usize)
        .map(|s| s.as_str())
        .unwrap_or("");
    for needle in [
        "entry_",
        "Entry ",
        ".html",
        "example.com",
        "archive.org",
        "NEWTON",
        "Poetry",
        "longer",
    ] {
        assert!(
            !cursor_row.contains(needle),
            "Down after Left from Ctrl+End: rendered cursor row {cy} should be \
             the empty trailing line, but contains data substring {needle:?}.\n\
             Row text: {:?}",
            cursor_row.trim_end(),
        );
    }
}

/// Mirror of `test_ctrl_end_then_disable_line_wrap_cursor_row`
/// — after Ctrl+End with line wrap on, toggling line wrap off
/// must keep the cursor on the trailing empty line; the
/// rendered cursor row must NOT be a tilde row.
///
/// The original uses the command palette
/// (Ctrl+P → "Toggle Line Wrap" → Enter). The migrated test
/// dispatches `Action::ToggleLineWrap` directly — same end
/// state, same regression-triggering condition (line wrap
/// disabled mid-session after the viewport scrolled under
/// wrap mode).
#[test]
fn migrated_ctrl_end_then_disable_line_wrap_cursor_row() {
    let content = make_csv_like_content_with_trailing_newline();
    let doc_end = content.len();

    let mut harness =
        EditorTestHarness::with_config(135, 37, config_with_line_wrap()).unwrap();
    let _f = harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();

    // Ctrl+End → cursor on trailing empty line.
    harness.api_mut().dispatch(Action::MoveDocumentEnd);
    harness.render().unwrap();
    assert_eq!(harness.api_mut().primary_caret().position, doc_end);

    // Toggle line wrap off.
    harness.api_mut().dispatch(Action::ToggleLineWrap);
    let snap = RenderSnapshot::extract_with_rendered_rows(&mut harness);

    // Cursor byte must still be at doc_end.
    assert_eq!(
        harness.api_mut().primary_caret().position,
        doc_end,
        "Cursor byte should remain at doc end after toggling line wrap off"
    );

    // The rendered cursor row must not be a tilde row. RowMatch
    // lacks a negative-row-at-index matcher, so inspect the
    // cursor row directly.
    let (_cx, cy) = snap.hardware_cursor.expect("hardware cursor must be set");
    let cursor_row = snap
        .rendered_rows
        .get(cy as usize)
        .map(|s| s.as_str())
        .unwrap_or("");
    assert!(
        !cursor_row.contains('~'),
        "After Ctrl+End then disabling line wrap, the rendered cursor row {cy} \
         landed on a tilde row instead of the empty trailing line. Row text: {:?}",
        cursor_row.trim_end(),
    );
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

/// Anti-test for `migrated_down_from_last_content_line_reaches_
/// trailing_empty_line`: drop the trailing `MoveDown` after
/// `MoveDocumentEnd` + `MoveLeft`. Without it, the cursor sits
/// on the last content line (Entry 140) — the rendered cursor
/// row MUST contain data substrings, so the positive
/// "no data content" claim would fail. Pins that the migrated
/// test's success depends on the actual Down keystroke.
#[test]
fn anti_down_from_last_content_line_without_down_stays_on_data_row() {
    let content = make_csv_like_content_with_trailing_newline();
    let doc_end = content.len();

    let mut harness =
        EditorTestHarness::with_config(135, 37, config_with_line_wrap()).unwrap();
    let _f = harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();

    harness.api_mut().dispatch(Action::MoveDocumentEnd);
    harness.render().unwrap();
    harness.api_mut().dispatch(Action::MoveLeft);
    // Drop the MoveDown — that's the load-bearing step.
    let snap = RenderSnapshot::extract_with_rendered_rows(&mut harness);

    let pos = harness.api_mut().primary_caret().position;
    assert!(
        pos < doc_end,
        "anti: without MoveDown, cursor stays on previous content line (pos={pos} < doc_end={doc_end})",
    );

    let (_cx, cy) = snap.hardware_cursor.expect("hardware cursor must be set");
    let cursor_row = snap
        .rendered_rows
        .get(cy as usize)
        .map(|s| s.as_str())
        .unwrap_or("");
    let has_data_content = ["entry_", "Entry ", ".html", "example.com", "archive.org"]
        .iter()
        .any(|needle| cursor_row.contains(needle));
    assert!(
        has_data_content,
        "anti: without MoveDown, cursor row {cy} must contain data content \
         (the cursor is on the last content line, not the empty trailing line). \
         Row text: {:?}",
        cursor_row.trim_end(),
    );
}

/// Anti-test for `migrated_ctrl_end_then_disable_line_wrap_
/// cursor_row`: drop the `ToggleLineWrap` dispatch. Without
/// the toggle, line wrap stays on and the cursor stays on the
/// trailing empty line; the regression-triggering condition
/// (toggling wrap off mid-session) never occurred. Pins that
/// the migrated test exercises the toggle path specifically.
#[test]
fn anti_ctrl_end_without_toggle_keeps_wrap_on() {
    let content = make_csv_like_content_with_trailing_newline();
    let doc_end = content.len();

    let mut harness =
        EditorTestHarness::with_config(135, 37, config_with_line_wrap()).unwrap();
    let _f = harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();

    harness.api_mut().dispatch(Action::MoveDocumentEnd);
    // Drop the ToggleLineWrap dispatch — that's the load-bearing step.
    harness.render().unwrap();

    assert_eq!(
        harness.api_mut().primary_caret().position,
        doc_end,
        "anti: cursor byte should still be at doc_end without toggling wrap"
    );
    // Without the toggle, wrap remains on and the original
    // regression (cursor landing on a tilde row after wrap-off)
    // simply cannot manifest — proving the toggle is the
    // load-bearing precondition.
}
