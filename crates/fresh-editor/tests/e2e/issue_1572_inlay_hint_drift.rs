//! Tests for issue #1572: Inlay hint position drifts with whitespace after
//! a closing brace.
//!
//! Root cause (pre-fix): inlay hints are stored as a byte-offset marker in
//! the buffer. `apply_inlay_hints_to_state` converts the LSP
//! `{line, character}` to a byte offset and anchors a `BeforeChar` marker
//! with *right* affinity there. When the LSP hands us a position that
//! points past the closing `}` on line 0 — i.e. right on the following
//! `\n` — the marker ends up anchored to that newline. A subsequent
//! `\n` insertion at the start of line 1 (byte offset = marker
//! position) drags the marker forward (right affinity), so the hint
//! now renders before a *different* newline on a *different* line.
//! To the user, the hint visibly jumps down by one row after any
//! whitespace edit below the brace.
//!
//! Fix: when the computed byte offset lands on a line terminator, anchor
//! the hint to the preceding character with `AfterChar` instead. That
//! way the marker stays attached to the `}` (or whichever glyph the
//! hint is annotating) and subsequent edits to later lines cannot shift
//! its screen column.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::app::Editor;
use lsp_types::{InlayHint, InlayHintLabel, Position};

fn make_hint(line: u32, character: u32, label: &str) -> InlayHint {
    InlayHint {
        position: Position { line, character },
        label: InlayHintLabel::String(label.to_string()),
        kind: None,
        text_edits: None,
        tooltip: None,
        padding_left: None,
        padding_right: None,
        data: None,
    }
}

/// Find the first occurrence of `needle` on the given screen row.
fn find_hint_column(harness: &EditorTestHarness, needle: &str, row: u16) -> Option<u16> {
    let text = harness.get_row_text(row);
    let byte_idx = text.find(needle)?;
    Some(text[..byte_idx].chars().count() as u16)
}

#[test]
fn test_issue_1572_inlay_hint_stays_put_across_below_line_edits() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Buffer: line 0 has code ending in `}`. Lines 1..=3 are empty.
    let content = "fn f() { let x = 1; }\n\n\n\n";
    let _fixture = harness.load_buffer_from_text(content).unwrap();
    harness.render().unwrap();

    // Publish an inlay hint just past the `}` on line 0.
    // `fn f() { let x = 1; }` is 21 characters; character 21 is the
    // position right after the `}` (pointing at the `\n`).
    let hint_text = "HINTX";
    let hints = vec![make_hint(0, 21, hint_text)];
    Editor::apply_inlay_hints_to_state(harness.editor_mut().active_state_mut(), &hints);
    harness.render().unwrap();

    // Find the hint on screen.
    let (first_row, last_row) = harness.content_area_rows();
    let mut hint_row = None;
    let mut hint_col_before = None;
    for row in first_row..=last_row {
        if let Some(col) = find_hint_column(&harness, hint_text, row as u16) {
            hint_row = Some(row as u16);
            hint_col_before = Some(col);
            break;
        }
    }
    let hint_row = hint_row.expect("inlay hint must be rendered after apply");
    let hint_col_before = hint_col_before.unwrap();

    // The hint must be on the same row as the `}` — i.e. on line 0.
    let brace_col_before = find_hint_column(&harness, "}", hint_row)
        .expect("closing brace must be on the same row as the hint after initial render");
    assert!(
        hint_col_before > brace_col_before,
        "hint must render to the right of the `}}` initially (row {hint_row}: \
         brace at col {brace_col_before}, hint at col {hint_col_before})\nScreen:\n{}",
        harness.screen_to_string(),
    );

    // Now edit whitespace adjacent to the brace: put the cursor at the
    // end of line 0 (right after `}`) and press Enter. This inserts a
    // `\n` at exactly the marker's byte offset. With the buggy right
    // affinity the marker is dragged one byte forward, so the hint now
    // renders on the *new* line 1 instead of line 0.
    {
        let state = harness.editor_mut().active_state_mut();
        let buf_len = state.buffer.len();
        // Bytes 0..21 are the code on line 0; byte 21 is the terminating
        // `\n`. The cursor ends up right after `}`, same byte as the hint.
        let end_of_line_0 = 21usize.min(buf_len);
        harness
            .editor_mut()
            .active_cursors_mut()
            .primary_mut()
            .position = end_of_line_0;
        harness
            .editor_mut()
            .active_cursors_mut()
            .primary_mut()
            .anchor = None;
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // The hint must still be on the same visual row and column as before.
    let hint_col_after = find_hint_column(&harness, hint_text, hint_row).unwrap_or_else(|| {
        panic!(
            "hint drifted off row {hint_row} after inserting \\n on line 1.\nScreen:\n{}",
            harness.screen_to_string()
        )
    });
    assert_eq!(
        hint_col_after, hint_col_before,
        "hint column drifted from {hint_col_before} to {hint_col_after} after a whitespace \
         edit below the `}}` (bug #1572)",
    );
    let brace_col_after =
        find_hint_column(&harness, "}", hint_row).expect("brace must still be on same row");
    assert_eq!(
        brace_col_after, brace_col_before,
        "sanity: the `}}` itself should not move",
    );
}
