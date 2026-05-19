//! Migration of `tests/e2e/ctrl_end_wrapped.rs` — Ctrl+End on
//! a long wrapped buffer with a trailing newline must scroll
//! the viewport far enough that the empty final line is
//! visible, not leave it hidden below the screen. Also covers
//! the Down-from-last-content-line and toggle-line-wrap-after-
//! Ctrl+End follow-ups that shared the same regression root.
//!
//! The Ctrl+End test uses the declarative
//! `EditorTestApi`-based path (viewport-byte bounds suffice).
//! The Down-after-Left and toggle-wrap-off tests need per-row
//! text inspection of the cursor's terminal-absolute row, so
//! they use the harness-direct pattern (the same pattern
//! `migrated_line_wrap_parity.rs` uses):
//! `harness.screen_cursor_position()` + `harness.get_row_text()`
//! (the `EditorTestApi::hardware_cursor_position` accessor is
//! viewport-relative and would index the wrong row of the
//! terminal-absolute rendered output).
//!
//! Source: `tests/e2e/ctrl_end_wrapped.rs` (3 tests; 0 deferred).

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
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

    // Ctrl+End → empty trailing line. Mirror the original
    // e2e exactly via `send_key` — the bug is sensitive to the
    // full key-handling pipeline (sticky column tracking on
    // wrapped lines), not just the underlying Action.
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    assert_eq!(
        harness.api_mut().primary_caret().position,
        doc_end,
        "Ctrl+End must reach the trailing empty line byte"
    );

    // Left → end of previous content line.
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Down → must return to the trailing empty line (doc_end).
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let pos_after_down = harness.api_mut().primary_caret().position;
    assert_eq!(
        pos_after_down, doc_end,
        "Down after Left from doc end should move cursor byte back to doc_end ({doc_end}), got {pos_after_down}",
    );

    // Mirror the e2e exactly via harness-direct surfaces:
    // `screen_cursor_position` returns terminal-absolute
    // `(col, row)` (the `EditorTestApi::hardware_cursor_position`
    // accessor is viewport-relative, so its row would not match
    // the absolute row indices used by `get_row_text` or
    // `RenderSnapshot.rendered_rows`).
    let (_cx, cy) = harness.screen_cursor_position();
    let cursor_row = harness.get_row_text(cy);

    // Mirror the e2e assertion: the rendered cursor row must
    // not contain any of these data-line substrings.
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
/// Uses `send_key` for Ctrl+End and the command palette
/// sequence (Ctrl+P → "Toggle Line Wrap" → Enter) — exactly
/// mirroring the original e2e action sequence.
#[test]
fn migrated_ctrl_end_then_disable_line_wrap_cursor_row() {
    let content = make_csv_like_content_with_trailing_newline();
    let doc_end = content.len();

    let mut harness =
        EditorTestHarness::with_config(135, 37, config_with_line_wrap()).unwrap();
    let _f = harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();

    // Ctrl+End → cursor on trailing empty line.
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    assert_eq!(harness.api_mut().primary_caret().position, doc_end);

    // Toggle line wrap off via the command palette (Ctrl+P,
    // type "Toggle Line Wrap", Enter).
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

    // Cursor byte must still be at doc_end.
    assert_eq!(
        harness.api_mut().primary_caret().position,
        doc_end,
        "Cursor byte should remain at doc end after toggling line wrap off"
    );

    // Mirror the e2e exactly: harness-direct
    // `screen_cursor_position` + `get_row_text` for the cursor
    // row's terminal-absolute text. The rendered cursor row must
    // not be a tilde row.
    let (_cx, cy) = harness.screen_cursor_position();
    let cursor_row = harness.get_row_text(cy);
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
/// trailing_empty_line`: drop the trailing Down keystroke after
/// Ctrl+End + Left. Without it, the cursor sits on the last
/// content line (Entry 140) — the rendered cursor row MUST
/// contain data substrings, so the positive "no data content"
/// claim would fail. Pins that the migrated test's success
/// depends on the actual Down keystroke.
#[test]
fn anti_down_from_last_content_line_without_down_stays_on_data_row() {
    let content = make_csv_like_content_with_trailing_newline();
    let doc_end = content.len();

    let mut harness =
        EditorTestHarness::with_config(135, 37, config_with_line_wrap()).unwrap();
    let _f = harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    // Drop the Down keystroke — that's the load-bearing step.
    harness.render().unwrap();

    let pos = harness.api_mut().primary_caret().position;
    assert!(
        pos < doc_end,
        "anti: without Down keystroke, cursor stays on previous content line (pos={pos} < doc_end={doc_end})",
    );

    let (_cx, cy) = harness.screen_cursor_position();
    let cursor_row = harness.get_row_text(cy);
    // The cursor lands at the end of the last content line.
    // Under wrap, that's a wrapped row of Entry 140 — possibly
    // the very last wrapped chunk ("descriptive text that
    // makes this line longer"), so use the broader substring
    // list from the original e2e (which includes "longer").
    let has_data_content = [
        "entry_",
        "Entry ",
        ".html",
        "example.com",
        "archive.org",
        "NEWTON",
        "Poetry",
        "longer",
    ]
    .iter()
    .any(|needle| cursor_row.contains(needle));
    assert!(
        has_data_content,
        "anti: without Down keystroke, cursor row {cy} must contain data content \
         (the cursor is on the last content line, not the empty trailing line). \
         Row text: {:?}",
        cursor_row.trim_end(),
    );
}

/// Anti-test for `migrated_ctrl_end_then_disable_line_wrap_
/// cursor_row`: drop the Ctrl+End. Without Ctrl+End the
/// cursor stays at byte 0 (top of the buffer) and the
/// regression scenario (wrap-toggle-off while the cursor is
/// on the trailing empty line of a long wrapped buffer) cannot
/// occur. Pins that Ctrl+End is the load-bearing precondition.
#[test]
fn anti_disable_line_wrap_without_ctrl_end_leaves_cursor_at_top() {
    let content = make_csv_like_content_with_trailing_newline();
    let doc_end = content.len();

    let mut harness =
        EditorTestHarness::with_config(135, 37, config_with_line_wrap()).unwrap();
    let _f = harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();

    // Drop the Ctrl+End — that's the load-bearing step.
    // Toggle line wrap off via the command palette.
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

    let pos = harness.api_mut().primary_caret().position;
    assert!(
        pos < doc_end / 2,
        "anti: without Ctrl+End, cursor must stay near the top of the buffer \
         (pos={pos}, doc_end={doc_end}). The positive test's regression \
         depends on the cursor being on the trailing empty line."
    );
}
