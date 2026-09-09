//! Tests for issue #722: an inlay hint transiently renders in the wrong
//! place after an edit *at* the hint's anchor.
//!
//! Reproduction (rust-analyzer, `let d = Duration::from_secs(5);`): the
//! `: Duration` hint is anchored `BeforeChar` on the space after `d`.
//! Putting the cursor immediately after `d` — the hint's own anchor byte —
//! and pressing Enter used to drag the marker along with the inserted
//! newline (markers were right-gravity), so the hint jumped to the start
//! of the *next* line, rendering against ` = Duration::from_secs(5);`
//! until the debounced `inlayHint` refresh (~1s later) replaced it.
//!
//! Fix: the inlay-hint path asks for a left-gravity marker (see
//! `view::virtual_text::MarkerGravity`), so text inserted at the anchor
//! stays *after* the hint and the hint keeps annotating the glyph it was
//! attached to — no waiting for the server to correct the screen. Gravity
//! is chosen per entry rather than derived from which side the entry
//! renders on, so entries whose marker *is* a position that must travel
//! with the text — the async-paste placeholder — are unaffected.

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

/// Row and column of `needle` on screen, searching the content area.
fn find_hint(harness: &EditorTestHarness, needle: &str) -> Option<(u16, u16)> {
    let (first_row, last_row) = harness.content_area_rows();
    for row in first_row..=last_row {
        let text = harness.get_row_text(row as u16);
        if let Some(byte_idx) = text.find(needle) {
            return Some((row as u16, text[..byte_idx].chars().count() as u16));
        }
    }
    None
}

#[test]
fn test_issue_722_hint_stays_on_its_line_when_split_at_anchor() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Line 0 is the `let` binding; the hint belongs right after `d`.
    let content = "let d = Duration::from_secs(5);\n";
    let _fixture = harness.load_buffer_from_text(content).unwrap();
    harness.render().unwrap();

    // `let d` is 5 characters, so character 5 is the insertion point the
    // server reports for the type hint (the space before `=`).
    let hint_text = "HINTX";
    let hints = vec![make_hint(0, 5, hint_text)];
    Editor::apply_inlay_hints_to_state(harness.editor_mut().active_state_mut(), &hints);
    harness.render().unwrap();

    let (hint_row_before, hint_col_before) =
        find_hint(&harness, hint_text).expect("inlay hint must be rendered after apply");
    let (d_row, d_col) = find_hint(&harness, "let d").expect("code must be on screen");
    assert_eq!(
        hint_row_before,
        d_row,
        "sanity: hint starts on the same row as the binding\nScreen:\n{}",
        harness.screen_to_string(),
    );
    assert_eq!(
        hint_col_before,
        d_col + "let d".chars().count() as u16,
        "sanity: hint starts immediately after `let d`\nScreen:\n{}",
        harness.screen_to_string(),
    );

    // Put the cursor exactly on the hint's anchor byte (right after `d`)
    // and split the line there.
    {
        let cursors = harness.editor_mut().active_cursors_mut();
        cursors.primary_mut().position = 5;
        cursors.primary_mut().anchor = None;
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Without waiting for any LSP refresh, the hint must still sit on the
    // binding's own line, after `d` — not dragged onto the next line.
    let (hint_row_after, hint_col_after) = find_hint(&harness, hint_text).unwrap_or_else(|| {
        panic!(
            "hint disappeared after splitting the line at its anchor.\nScreen:\n{}",
            harness.screen_to_string()
        )
    });
    let (d_row_after, d_col_after) = find_hint(&harness, "let d").expect("code must be on screen");
    assert_eq!(
        hint_row_after,
        d_row_after,
        "hint jumped off the binding's line (row {hint_row_before} -> {hint_row_after}) after \
         Enter at its anchor (bug #722)\nScreen:\n{}",
        harness.screen_to_string(),
    );
    assert!(
        hint_col_after > d_col_after,
        "hint must still render after `let d`\nScreen:\n{}",
        harness.screen_to_string(),
    );

    // Stronger: the transient rendering must already match what the
    // debounced `inlayHint` refresh will produce for the edited buffer —
    // that refresh is what used to correct the screen ~1s later. The
    // server now reports the hint at the end of line 0 (character 5 of
    // `let d`), which is an end-of-line hint.
    let transient = harness.screen_to_string();
    Editor::apply_inlay_hints_to_state(
        harness.editor_mut().active_state_mut(),
        &[make_hint(0, 5, hint_text)],
    );
    harness.render().unwrap();
    let refreshed = harness.screen_to_string();
    assert_eq!(
        transient, refreshed,
        "the post-edit rendering should already equal the post-refresh rendering",
    );
}

#[test]
fn test_issue_722_hint_stays_ahead_of_text_typed_at_anchor() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // A parameter-name hint: it renders in front of the argument, so text
    // typed at its anchor belongs *after* the hint, not before it.
    let content = "f(5);\n";
    let _fixture = harness.load_buffer_from_text(content).unwrap();
    harness.render().unwrap();

    let hint_text = "count:";
    let hints = vec![make_hint(0, 2, hint_text)];
    Editor::apply_inlay_hints_to_state(harness.editor_mut().active_state_mut(), &hints);
    harness.render().unwrap();

    let (hint_row_before, hint_col_before) =
        find_hint(&harness, hint_text).expect("inlay hint must be rendered after apply");

    // Type a digit in front of the `5` — i.e. at the hint's anchor byte.
    {
        let cursors = harness.editor_mut().active_cursors_mut();
        cursors.primary_mut().position = 2;
        cursors.primary_mut().anchor = None;
    }
    harness
        .send_key(KeyCode::Char('1'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let (hint_row_after, hint_col_after) =
        find_hint(&harness, hint_text).expect("hint must survive typing at its anchor");
    assert_eq!(
        (hint_row_after, hint_col_after),
        (hint_row_before, hint_col_before),
        "parameter hint should stay in front of the typed text\nScreen:\n{}",
        harness.screen_to_string(),
    );
    let (_, digit_col) = find_hint(&harness, "15").expect("typed digit must be in the buffer");
    assert!(
        digit_col > hint_col_after,
        "the argument must render after the hint, not before it\nScreen:\n{}",
        harness.screen_to_string(),
    );
}
