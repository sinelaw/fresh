//! Migrations of `tests/e2e/paste.rs` cases not covered by
//! `migrated_paste.rs`. Excludes the cases that depend on the
//! test-only `set_clipboard_for_test()` shortcut: those would
//! require either extending `EditorTestApi` with a clipboard
//! setter (not done — the user's directive: no shortcuts) or
//! shaping each scenario as a real Copy → Paste round-trip
//! within the buffer. The migrations below take the round-trip
//! shape.

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, check_buffer_scenario, repeat, BufferScenario, CursorExpect,
};
use fresh::test_api::Action;

#[test]
fn migrated_paste_at_end_of_line_appends_copied_text() {
    // Original: `test_paste_at_end_of_line` (re-shaped as a
    // round-trip). Initial buffer has " world" + "hello"; copy
    // " world", move to end-of-buffer, paste — the buffer
    // duplicates " world" at the end.
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
    // Original: `test_paste_multiline_text` (re-shaped). Initial
    // buffer is "line1\nline2\nline3"; SelectAll + Copy gives a
    // multiline clipboard; Paste at end appends another copy.
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
    // Original: `test_paste_replaces_selection` (round-trip
    // re-shape). Buffer is "universe-target"; select "universe"
    // and Copy; SelectLineEnd → SelectLeft×6 selects "target"
    // backward; Paste replaces it. Result: "universe-universe".
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
