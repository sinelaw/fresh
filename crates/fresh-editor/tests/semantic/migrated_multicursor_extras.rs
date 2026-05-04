//! Faithful migrations of `tests/e2e/multicursor.rs` claims not
//! covered by `migrated_multicursor_full.rs` — focused on
//! `AddCursorNextMatch` (Ctrl+D) edge cases that have surfaced
//! real bugs in the past.
//!
//! Issue #210: AddCursorNextMatch from a *backward* selection used
//! to leave the new cursor at the *end* of its match (out of sync
//! with the original), making subsequent typing produce wrong
//! text. The fix puts both cursors at the same edge as the
//! original. The `_with_backward_selection` and
//! `_with_forward_selection` scenarios pin both directions.
//!
//! No mocks: `Action::AddCursorNextMatch` is the same dispatch
//! path the user-facing Ctrl+D binding hits.

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, check_buffer_scenario, repeat, BufferScenario, CursorExpect,
};
use fresh::test_api::Action;

#[test]
fn migrated_add_cursor_next_match_with_backward_selection() {
    // Original: `test_add_cursor_next_match_with_backward_selection`
    // (issue #210). Selection direction must be preserved when a
    // second cursor is created from a Ctrl+D match.
    //
    //   Buffer:  "foo bar foo"   (bytes 0..11)
    //   Original: cursor 0, anchor 3   ← backward selection of "foo"
    //   New     : cursor 8, anchor 11  ← matching backward selection
    //
    // The newly-added cursor is the primary; both cursors must
    // point at the *start* of their selections (cursor < anchor)
    // so typing replaces each match consistently.
    assert_buffer_scenario(BufferScenario {
        description: "AddCursorNextMatch from backward selection keeps both cursors at start (issue #210)".into(),
        initial_text: "foo bar foo".into(),
        actions: repeat(Action::MoveRight, 3)
            .chain(repeat(Action::SelectLeft, 3))
            .chain(std::iter::once(Action::AddCursorNextMatch))
            .collect(),
        expected_text: "foo bar foo".into(),
        // Primary = new cursor at the second "foo".
        expected_primary: CursorExpect::range(11, 8),
        // Secondary = original cursor at the first "foo".
        expected_extra_cursors: vec![CursorExpect::range(3, 0)],
        expected_selection_text: Some("foo\nfoo".into()),
        ..Default::default()
    });
}

#[test]
fn migrated_add_cursor_next_match_with_forward_selection() {
    // Original: `test_add_cursor_next_match_with_forward_selection`.
    // Mirror of the backward case — both cursors should be at the
    // *end* of their selection.
    //
    //   Original: cursor 3, anchor 0   ← forward selection of "foo"
    //   New     : cursor 11, anchor 8  ← matching forward selection
    // The newly-added cursor (8..11 forward) is the primary.
    assert_buffer_scenario(BufferScenario {
        description: "AddCursorNextMatch from forward selection keeps both cursors at end".into(),
        initial_text: "foo bar foo".into(),
        actions: repeat(Action::SelectRight, 3)
            .chain(std::iter::once(Action::AddCursorNextMatch))
            .collect(),
        expected_text: "foo bar foo".into(),
        expected_primary: CursorExpect::range(8, 11),
        expected_extra_cursors: vec![CursorExpect::range(0, 3)],
        expected_selection_text: Some("foo\nfoo".into()),
        ..Default::default()
    });
}

#[test]
fn migrated_typing_after_add_cursor_next_match_replaces_both_selections() {
    // Combined claim from both directions of the e2e test: typing
    // a single char while two cursors each hold a 3-byte selection
    // should replace each selection with that char. End buffer:
    // "X bar X".
    assert_buffer_scenario(BufferScenario {
        description: "Type after AddCursorNextMatch replaces both 'foo's with 'X' → 'X bar X'".into(),
        initial_text: "foo bar foo".into(),
        actions: repeat(Action::SelectRight, 3)
            .chain(std::iter::once(Action::AddCursorNextMatch))
            .chain(std::iter::once(Action::InsertChar('X')))
            .collect(),
        expected_text: "X bar X".into(),
        // After the dual replacement: the secondary (original)
        // lands at byte 1 — right after the X inserted at byte 0.
        // The primary (newly added by AddCursorNextMatch) was on
        // the second "foo", now at byte 7 in "X bar X".
        expected_primary: CursorExpect::at(7),
        expected_extra_cursors: vec![CursorExpect::at(1)],
        ..Default::default()
    });
}

#[test]
fn migrated_add_cursor_next_match_three_times_yields_three_cursors() {
    // Original: `test_add_cursor_next_match`. Three Ctrl+D presses
    // on a buffer with three "foo"s ⇒ three cursors, each on a
    // distinct match.
    let initial = "foo bar foo baz foo";
    assert_buffer_scenario(BufferScenario {
        description: "Three AddCursorNextMatch hops over three 'foo' occurrences".into(),
        initial_text: initial.into(),
        actions: repeat(Action::SelectRight, 3)
            .chain(repeat(Action::AddCursorNextMatch, 2))
            .collect(),
        expected_text: initial.into(),
        // "foo" appears at bytes 0, 8, 16. All three selections
        // forward. Primary = the most recently added cursor (last
        // AddCursorNextMatch lands on the third "foo" at 16..19).
        expected_primary: CursorExpect::range(16, 19),
        expected_extra_cursors: vec![
            CursorExpect::range(0, 3),
            CursorExpect::range(8, 11),
        ],
        expected_selection_text: Some("foo\nfoo\nfoo".into()),
        ..Default::default()
    });
}

#[test]
fn migrated_remove_secondary_cursors_returns_to_original_position() {
    // Original: `test_esc_returns_to_original_cursor_position`.
    // Multi-cursor invariant: after RemoveSecondaryCursors (Esc in
    // normal context) the surviving cursor is the *original* one,
    // not the most recently added — even though the added cursor
    // is the primary while the multi-cursor state exists.
    //
    //   Buffer:  "Line 1\nLine 2\nLine 3\nLine 4\nLine 5"
    //   Start :  cursor at byte 0
    //   After 3× AddCursorBelow:
    //       primary cursor on Line 4 (most recently added)
    //       3 secondaries on Lines 1, 2, 3
    //   After RemoveSecondaryCursors:
    //       1 cursor at byte 0 (Line 1, the original)
    assert_buffer_scenario(BufferScenario {
        description: "RemoveSecondaryCursors collapses to the original cursor, not the latest".into(),
        initial_text: "Line 1\nLine 2\nLine 3\nLine 4\nLine 5".into(),
        actions: repeat(Action::AddCursorBelow, 3)
            .chain(std::iter::once(Action::RemoveSecondaryCursors))
            .collect(),
        expected_text: "Line 1\nLine 2\nLine 3\nLine 4\nLine 5".into(),
        expected_primary: CursorExpect::at(0),
        ..Default::default()
    });
}

/// Anti-test: drops the `AddCursorNextMatch`. Without the action
/// only the first selection exists, so the
/// `expected_extra_cursors` list (one entry) cannot be filled.
#[test]
fn anti_add_cursor_next_match_dropping_action_yields_check_err() {
    let scenario = BufferScenario {
        description: "anti: AddCursorNextMatch dropped — second cursor cannot exist".into(),
        initial_text: "foo bar foo".into(),
        actions: repeat(Action::SelectRight, 3).collect(),
        expected_text: "foo bar foo".into(),
        // Identical expectation as the real test (primary = the
        // would-be-new cursor at 8..11) — the original cursor at
        // 0..3 is the only real one, so this can't satisfy.
        expected_primary: CursorExpect::range(8, 11),
        expected_extra_cursors: vec![CursorExpect::range(0, 3)],
        expected_selection_text: Some("foo\nfoo".into()),
        ..Default::default()
    };
    assert!(
        check_buffer_scenario(scenario).is_err(),
        "anti-test: with no AddCursorNextMatch the second cursor is absent — \
         expected_extra_cursors list cannot match"
    );
}
