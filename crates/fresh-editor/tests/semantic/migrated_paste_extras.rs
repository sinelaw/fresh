//! Copy/Paste round-trip scenarios *related to* — but NOT 1:1
//! ports of — `tests/e2e/paste.rs` cases that depend on the
//! test-only `set_clipboard_for_test()` shortcut. The e2e
//! originals preload the clipboard with content not present in
//! the buffer (e.g. paste " world" into an empty buffer to get
//! " world"). Honoring the no-back-doors directive, the
//! migrations below reshape each scenario as Copy-from-buffer
//! → Paste — which tests the same Copy and Paste production
//! paths end-to-end but is a *different theorem* than the e2e
//! original (clipboard contents come from the buffer, not from
//! external preload).
//!
//! Each test below pins a Copy→Paste round-trip claim that
//! exercises real `Action::Copy` / `Action::Paste` and the
//! production clipboard plumbing. Where the round-trip captures
//! something close to the e2e's intent, the doc-comment says
//! "Related to <test_name>"; where the round-trip exercises a
//! distinct property, no e2e cross-ref is claimed.
//!
//! The 5 e2e cases that genuinely require preloaded-clipboard
//! semantics (CRLF normalization, mixed line endings, external
//! paste preload, prompt-paste, column-mode) remain guarded by
//! tests/e2e/paste.rs and are not migratable in this shape —
//! tracked in #2058 as a coverage gap.

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, check_buffer_scenario, repeat, BufferScenario, CursorExpect,
};
use fresh::test_api::Action;

#[test]
fn migrated_paste_at_end_of_line_appends_copied_text() {
    // Related to `test_paste_at_end_of_line`. Different shape:
    // the e2e pastes a preloaded " world" into a buffer
    // containing only "hello"; this round-trip seeds " world"
    // in the buffer, copies it, and pastes at EOF. Both exercise
    // the Paste production path but the e2e tested "external
    // clipboard, then paste at EOF", while this tests "Copy a
    // 6-byte substring then Paste at EOF".
    let mut actions: Vec<Action> = repeat(Action::SelectRight, 6).collect();
    actions.push(Action::Copy);
    actions.push(Action::MoveDocumentEnd);
    actions.push(Action::Paste);

    assert_buffer_scenario(BufferScenario {
        description: "Copy ' world' from start, paste at EOF".into(),
        initial_text: " world\nhello".into(),
        actions,
        // Paste appends " world" → " world\nhello world".
        expected_text: " world\nhello world".into(),
        expected_primary: CursorExpect::at(" world\nhello world".len()),
        ..Default::default()
    });
}

#[test]
fn migrated_paste_in_middle_inserts_at_cursor() {
    // Original: `test_paste_in_middle`. Buffer is "helloworld";
    // select "hello" (5 chars), Copy, move to byte 5 (between
    // hello and world), Paste — yields "hellohelloworld".
    let mut actions: Vec<Action> = repeat(Action::SelectRight, 5).collect();
    actions.push(Action::Copy);
    // After Copy, the selection is still active. Paste with an
    // active selection REPLACES the selection, so we'd just put
    // "hello" back where it was. Deselect first by collapsing the
    // selection to its right edge with one MoveRight (which, per
    // issue #1566, lands at the right edge of the existing
    // selection — byte 5 — *without* advancing further).
    actions.push(Action::MoveRight);
    actions.push(Action::Paste);

    assert_buffer_scenario(BufferScenario {
        description: "Copy 5-byte selection, deselect, Paste at byte 5 yields a duplicate".into(),
        initial_text: "helloworld".into(),
        actions,
        expected_text: "hellohelloworld".into(),
        expected_primary: CursorExpect::at(10),
        ..Default::default()
    });
}

#[test]
fn migrated_paste_multiline_text_round_trip() {
    // Related to `test_paste_multiline_text`. Different shape:
    // the e2e preloads a multiline clipboard and pastes into an
    // *empty* buffer to test that the multiline payload inserts
    // correctly. This round-trip starts with a 3-line buffer,
    // SelectAll + Copy, then Paste at EOF — testing that the
    // round-trip duplicates lines rather than that a preloaded
    // multiline clipboard inserts into emptiness.
    let mut actions = vec![Action::SelectAll, Action::Copy, Action::MoveDocumentEnd];
    actions.push(Action::Paste);

    assert_buffer_scenario(BufferScenario {
        description: "Copy a 3-line buffer then Paste at end duplicates the lines".into(),
        initial_text: "line1\nline2\nline3".into(),
        actions,
        expected_text: "line1\nline2\nline3line1\nline2\nline3".into(),
        expected_primary: CursorExpect::at(34),
        ..Default::default()
    });
}

#[test]
fn migrated_paste_undo_round_trip_is_atomic() {
    // Original: `test_paste_undo_is_atomic`. Single Undo after a
    // Paste removes the entire pasted text, regardless of how
    // many bytes it was.
    let mut actions: Vec<Action> = repeat(Action::SelectRight, 6).collect();
    actions.push(Action::Copy);
    actions.push(Action::MoveDocumentEnd);
    actions.push(Action::Paste);
    actions.push(Action::Undo);

    assert_buffer_scenario(BufferScenario {
        description: "Single Undo after Paste removes the entire pasted run atomically".into(),
        initial_text: " world\nhello".into(),
        actions,
        // Buffer back to its pre-paste state.
        expected_text: " world\nhello".into(),
        // Cursor restored to the position it had right before Paste
        // (end of buffer at byte 12).
        expected_primary: CursorExpect::at(12),
        ..Default::default()
    });
}

#[test]
fn migrated_paste_replaces_selection_round_trip() {
    // Related to `test_paste_replaces_selection`. The
    // load-bearing claim — Paste over an active selection
    // replaces it — is faithful to the e2e. The clipboard
    // source differs: the e2e preloads "universe" via
    // set_clipboard_for_test; this round-trip seeds it in the
    // buffer and Copies from there.
    let mut actions: Vec<Action> = repeat(Action::SelectRight, 8).collect();
    actions.push(Action::Copy);
    actions.push(Action::MoveLineEnd);
    actions.extend(repeat(Action::SelectLeft, 6));
    actions.push(Action::Paste);

    assert_buffer_scenario(BufferScenario {
        description: "Paste replaces a 6-byte selection with the 8-byte clipboard".into(),
        initial_text: "universe-target".into(),
        actions,
        expected_text: "universe-universe".into(),
        // After replacement: cursor at byte 17 (end of inserted
        // 'universe' — replacement bumps cursor to end of insert).
        expected_primary: CursorExpect::at(17),
        ..Default::default()
    });
}

/// Anti-test: drops the `Copy` from the multiline round-trip. With
/// no Copy, the clipboard either holds previous content or nothing,
/// so Paste cannot duplicate the buffer's lines.
#[test]
fn anti_paste_round_trip_without_copy_yields_check_err() {
    let actions = vec![
        Action::SelectAll,
        // Copy intentionally omitted.
        Action::MoveDocumentEnd,
        Action::Paste,
    ];
    let scenario = BufferScenario {
        description: "anti: Copy dropped — Paste cannot duplicate the buffer".into(),
        initial_text: "line1\nline2\nline3".into(),
        actions,
        expected_text: "line1\nline2\nline3line1\nline2\nline3".into(),
        expected_primary: CursorExpect::at(34),
        ..Default::default()
    };
    assert!(
        check_buffer_scenario(scenario).is_err(),
        "anti-test: without Copy, the clipboard doesn't hold the 3-line text; \
         Paste can't produce the duplicated expectation"
    );
}
