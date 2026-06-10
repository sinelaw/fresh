// Regression test for issue #2027.
//
// Undoing an on-save buffer rewrite (format-on-save, or the
// trim-trailing-whitespace-on-save that shares the same
// `replace_buffer_with_output` undo path) used to leave the primary cursor at
// the very end of the buffer, scrolling the view to the bottom of the file.
// The fix prepends a cursor-restore event to the rewrite batch so undo pins
// the cursor/view back to where it was before the format.

mod common;

use common::harness::EditorTestHarness;

#[test]
fn undo_of_on_save_rewrite_keeps_view_at_top() {
    let mut config = fresh::config::Config::default();
    // Trim-on-save rewrites the buffer through the exact code path
    // (`replace_buffer_with_output`) that format-on-save uses, without needing
    // an external formatter process — keeping the test deterministic.
    config.editor.trim_trailing_whitespace_on_save = true;

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();

    // A buffer taller than the 24-row viewport, with trailing whitespace on
    // every line so the on-save trim actually changes the content.
    let content: String = (1..=60).map(|i| format!("line {i:04}   \n")).collect();
    let _fixture = harness
        .load_buffer_from_text_named("repro.txt", &content)
        .unwrap();

    harness.render().unwrap();
    assert_eq!(
        harness.top_line_number(),
        0,
        "precondition: a freshly opened buffer starts scrolled to the top (line 0)",
    );

    // Save: triggers the on-save trim, which rewrites the whole buffer.
    harness.editor_mut().save().unwrap();
    harness.render().unwrap();
    assert_eq!(
        harness.top_line_number(),
        0,
        "the on-save rewrite itself should not move the view",
    );

    // Undo the on-save rewrite.
    harness.editor_mut().handle_undo();
    harness.render().unwrap();

    assert_eq!(
        harness.top_line_number(),
        0,
        "undoing an on-save format/trim must not scroll the view to the bottom (issue #2027)",
    );
}
