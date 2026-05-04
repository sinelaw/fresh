//! Faithful migration of `tests/e2e/issue_1574_compose_scroll.rs`.
//!
//! Issue #1574: arrow keys must NOT scroll the viewport while
//! the cursor remains inside the visible rows. Pre-fix, the
//! `Viewport::ensure_visible` scroll-margin gate fired even when
//! the cursor was already visible, causing the viewport to drift
//! by one visual row per arrow press in heavily-wrapped buffers.
//!
//! The migrations here run the production layout pipeline through
//! `EditorTestHarness::render` + `EditorTestApi::viewport_top_byte`.
//! No mocks: same paths the production binary walks.

use crate::common::harness::EditorTestHarness;
use fresh::test_api::Action;

fn long_wrapped_content() -> String {
    let para = "This is a deliberately long paragraph that must wrap across many \
                visual rows so the scroll math is exercised. It continues for \
                a while so a single logical line becomes many visual rows.";
    let mut s = String::from("# Test\n\n");
    for i in 1..=6 {
        s.push_str(&format!("Paragraph {i}: {para}\n\n"));
    }
    s.push_str("End of file.\n");
    s
}

#[test]
fn migrated_issue_1574_up_arrow_does_not_drift_viewport_when_visible() {
    // Original: `test_issue_1574_up_does_not_scroll_when_cursor_not_at_top`.
    // Default config has line_wrap=true; long paragraphs ensure the
    // bug's "heavy wrapping" precondition is met.
    let mut harness = EditorTestHarness::with_temp_project(80, 20).unwrap();
    let _fixture = harness
        .load_buffer_from_text(&long_wrapped_content())
        .unwrap();
    harness.render().unwrap();

    // Jump to end of file: viewport scrolls so the cursor is
    // visible, parking it near the bottom.
    harness.api_mut().dispatch(Action::MoveDocumentEnd);
    harness.render().unwrap();
    let top_after_end = harness.api_mut().viewport_top_byte();

    // Three Up presses while the cursor is still inside the
    // visible area must NOT change top_byte.
    for i in 1..=3 {
        let before = harness.api_mut().viewport_top_byte();
        harness.api_mut().dispatch(Action::MoveUp);
        harness.render().unwrap();
        let after = harness.api_mut().viewport_top_byte();
        assert_eq!(
            before, after,
            "Up #{i}: viewport drifted from {before} to {after} \
             even though the cursor is still inside the viewport \
             (issue #1574: scroll margin firing on visible cursor)",
        );
    }

    assert_eq!(
        harness.api_mut().viewport_top_byte(),
        top_after_end,
        "After 3 Up presses the viewport must be at the same top \
         it had right after MoveDocumentEnd"
    );
}

#[test]
fn migrated_issue_1574_down_arrow_does_not_drift_viewport_when_visible() {
    // Original: `test_issue_1574_down_does_not_scroll_when_cursor_not_at_bottom`.
    let mut harness = EditorTestHarness::with_temp_project(80, 20).unwrap();
    let _fixture = harness
        .load_buffer_from_text(&long_wrapped_content())
        .unwrap();
    harness.render().unwrap();

    harness.api_mut().dispatch(Action::MoveDocumentStart);
    harness.render().unwrap();
    let top_initial = harness.api_mut().viewport_top_byte();

    for i in 1..=3 {
        let before = harness.api_mut().viewport_top_byte();
        harness.api_mut().dispatch(Action::MoveDown);
        harness.render().unwrap();
        let after = harness.api_mut().viewport_top_byte();
        assert_eq!(
            before, after,
            "Down #{i}: viewport drifted from {before} to {after} \
             even though the cursor is still inside the viewport \
             (issue #1574: scroll margin firing on visible cursor)",
        );
    }

    assert_eq!(
        harness.api_mut().viewport_top_byte(),
        top_initial,
        "After 3 Down presses near the top, viewport must not have moved"
    );
}
