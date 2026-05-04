//! Faithful migrations of `tests/e2e/issue_1147_wrapped_line_nav.rs`.
//!
//! Issue #1147: navigation bugs at end-of-file with wrapped lines.
//! Pre-fix:
//! - **Up-arrow scroll** drifted the viewport by ~one logical line
//!   per arrow press from end-of-file when wrapping was enabled.
//! - **Down-arrow skip** jumped past intermediate visual rows of a
//!   wrapped logical line straight to the next logical line.
//! - **End key** stuck on the first visual segment instead of
//!   advancing through subsequent wrapped segments.
//!
//! These migrations route through `EditorTestHarness::render` and
//! `EditorTestApi::{viewport_top_byte, primary_caret}` — the same
//! production path the user-facing keys hit. No mocks.

use crate::common::harness::EditorTestHarness;
use fresh::test_api::Action;

/// Issue #1147 reproduction content: 20 short lines + 3 long lines
/// that each wrap once + 3 very long lines that wrap multiple times.
fn make_issue_1147_content() -> String {
    let mut lines = Vec::new();
    for i in 1..=20 {
        lines.push(format!("Line {} - short line", i));
    }
    for i in 21..=23 {
        lines.push(format!(
            "Line {} - this is a longer line that should wrap once in an \
             80-column terminal because it needs to exceed eighty characters \
             total length here",
            i
        ));
    }
    for i in 24..=26 {
        lines.push(format!(
            "Line {} - this line is extremely long and should wrap twice in \
             an 80-column terminal, because it has enough characters to fill \
             up more than two full rows of display output in the terminal \
             window making it an excellent test case for wrapping behavior",
            i
        ));
    }
    lines.join("\n")
}

#[test]
fn migrated_issue_1147_up_arrow_does_not_drift_viewport_at_end_of_wrapped_file() {
    // Original: `test_issue_1147_up_arrow_should_not_scroll_at_end_of_wrapped_file`.
    // 4 Up presses from end-of-file must not scroll the viewport
    // by more than ~30 bytes (i.e. one short logical line of slack)
    // when the cursor remains inside the visible area.
    let mut harness = EditorTestHarness::with_temp_project(80, 25).unwrap();
    let _fixture = harness
        .load_buffer_from_text(&make_issue_1147_content())
        .unwrap();
    harness.render().unwrap();

    harness.api_mut().dispatch(Action::MoveDocumentEnd);
    harness.render().unwrap();
    let initial_top_byte = harness.api_mut().viewport_top_byte();

    for _ in 0..4 {
        harness.api_mut().dispatch(Action::MoveUp);
        harness.render().unwrap();
    }

    let final_top_byte = harness.api_mut().viewport_top_byte();
    let scroll_distance = initial_top_byte.saturating_sub(final_top_byte);
    assert!(
        scroll_distance <= 30,
        "Issue #1147: viewport drifted {scroll_distance} bytes after 4 Up presses \
         (initial_top_byte={initial_top_byte}, final={final_top_byte}). With the bug, \
         each Up press scrolls one row even though the cursor is still visible.",
    );
}

#[test]
fn migrated_issue_1147_down_arrow_traverses_wrapped_visual_lines() {
    // Original: `test_issue_1147_down_arrow_should_traverse_wrapped_visual_lines`.
    // Cursor at the start of line 24 (a line that wraps to several
    // visual rows). One Down press must keep the cursor *within*
    // line 24 (advancing one visual row), not skip directly to
    // line 25.
    let mut harness = EditorTestHarness::with_temp_project(80, 25).unwrap();
    let content = make_issue_1147_content();
    let _fixture = harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();

    let line_24_start = content
        .match_indices('\n')
        .nth(22)
        .map(|(i, _)| i + 1)
        .unwrap();
    let line_25_start = content
        .match_indices('\n')
        .nth(23)
        .map(|(i, _)| i + 1)
        .unwrap();

    // Use real GotoLine modal flow — same as Ctrl+G in the e2e.
    harness.api_mut().dispatch(Action::GotoLine);
    harness.api_mut().dispatch(Action::InsertChar('2'));
    harness.api_mut().dispatch(Action::InsertChar('4'));
    harness.api_mut().dispatch(Action::PromptConfirm);
    harness.render().unwrap();
    assert_eq!(
        harness.api_mut().primary_caret().position,
        line_24_start,
        "GotoLine 24 should park cursor at the start of line 24"
    );

    harness.api_mut().dispatch(Action::MoveDown);
    harness.render().unwrap();
    let after_down = harness.api_mut().primary_caret().position;
    assert!(
        after_down >= line_24_start && after_down < line_25_start,
        "Issue #1147: Down from start of wrapped line 24 must land within \
         line 24's wrapped rows (bytes {line_24_start}..{line_25_start}); \
         got byte {after_down} which is on line 25 or later",
    );

    // A second Down should still stay inside line 24 (it wraps to
    // ~4 visual rows in an 80-col terminal).
    harness.api_mut().dispatch(Action::MoveDown);
    harness.render().unwrap();
    let after_second = harness.api_mut().primary_caret().position;
    assert!(
        after_second >= line_24_start && after_second < line_25_start,
        "Issue #1147: 2nd Down from line 24 must still be inside line 24; \
         got byte {after_second}, line 25 starts at {line_25_start}",
    );
}

#[test]
fn migrated_issue_1147_end_key_advances_through_wrapped_visual_segments() {
    // Original: `test_issue_1147_end_key_advances_through_wrapped_segments`
    // (claim subset): pressing End on a wrapped line must eventually
    // reach the *logical* end of the line, not stick at the end of
    // the first visual segment.
    let mut harness = EditorTestHarness::with_temp_project(80, 25).unwrap();
    let content = make_issue_1147_content();
    let _fixture = harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();

    let line_24_start = content
        .match_indices('\n')
        .nth(22)
        .map(|(i, _)| i + 1)
        .unwrap();
    let line_25_start = content
        .match_indices('\n')
        .nth(23)
        .map(|(i, _)| i + 1)
        .unwrap();
    let line_24_end = line_25_start - 1; // byte before the trailing \n

    harness.api_mut().dispatch(Action::GotoLine);
    harness.api_mut().dispatch(Action::InsertChar('2'));
    harness.api_mut().dispatch(Action::InsertChar('4'));
    harness.api_mut().dispatch(Action::PromptConfirm);
    harness.render().unwrap();
    assert_eq!(harness.api_mut().primary_caret().position, line_24_start);

    // First MoveLineEnd advances at least past the first visual
    // segment (~72 chars wide on an 80-col terminal). Multiple
    // presses (or a single one, depending on the editor's
    // semantics) must eventually reach the logical line end.
    for _ in 0..6 {
        harness.api_mut().dispatch(Action::MoveLineEnd);
        harness.render().unwrap();
        if harness.api_mut().primary_caret().position == line_24_end {
            break;
        }
    }
    assert_eq!(
        harness.api_mut().primary_caret().position,
        line_24_end,
        "Issue #1147: repeated MoveLineEnd on a wrapped line must eventually \
         reach the logical line end (byte {line_24_end}); got \
         {} after 6 presses",
        harness.api_mut().primary_caret().position,
    );
}
