//! Migrations of search-modal-driven claims that don't fit
//! `migrated_search.rs` (which only covers no-modal cases).
//!
//! These scenarios exercise the *real* search prompt flow:
//! `Action::Search` opens the prompt, `Action::InsertChar`
//! routes into the prompt (the editor's input handler dispatches
//! InsertChar into the active prompt automatically),
//! `Action::PromptConfirm` commits the search. No mocks — the
//! same code path the user-facing Ctrl+F binding walks.
//!
//! Issue #1697: `AddCursorNextMatch` (Ctrl+D) right after a
//! substring search must select just the substring, not the
//! whole word that contains it. Pre-fix, with the cursor sitting
//! on the start of "foo" inside "foobar", Ctrl+D would expand
//! the selection to "foobar" (word-boundary semantics) instead
//! of "foo" (search-substring semantics).

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, check_buffer_scenario, BufferScenario, CursorExpect,
};
use fresh::test_api::Action;

/// Helper: open the search prompt, type `query`, commit.
fn search_for(query: &str) -> Vec<Action> {
    let mut out = vec![Action::Search];
    out.extend(query.chars().map(Action::InsertChar));
    out.push(Action::PromptConfirm);
    out
}

#[test]
fn migrated_ctrl_d_after_substring_search_selects_match_not_word() {
    // Original: `test_ctrl_d_after_search_uses_match_not_word`
    // (issue #1697). After searching for "foo" the cursor lands
    // on the first match (byte 0). The first AddCursorNextMatch
    // doesn't ADD a cursor yet — it extends the existing cursor
    // to cover the search match (3 bytes for "foo"). Pre-fix this
    // would have selected the surrounding *word* "foobar" (6
    // bytes) instead of the search substring.
    let mut actions = search_for("foo");
    actions.push(Action::AddCursorNextMatch);

    assert_buffer_scenario(BufferScenario {
        description: "AddCursorNextMatch after substring-search selects 'foo' (3 bytes), not 'foobar' (6 bytes)".into(),
        initial_text: "foobar foo foo".into(),
        actions,
        expected_text: "foobar foo foo".into(),
        // Still one cursor; selection is the search match.
        expected_primary: CursorExpect::range(0, 3),
        expected_extra_cursors: vec![],
        expected_selection_text: Some("foo".into()),
        ..Default::default()
    });
}

#[test]
fn migrated_second_ctrl_d_adds_cursor_at_next_match() {
    // Two AddCursorNextMatch presses: the first selects the
    // current search match (no cursor added), the second adds a
    // new cursor at the *next* substring occurrence. End state:
    // 2 cursors with 3-byte selections at bytes 0..3 and 7..10.
    let mut actions = search_for("foo");
    actions.push(Action::AddCursorNextMatch);
    actions.push(Action::AddCursorNextMatch);

    assert_buffer_scenario(BufferScenario {
        description: "Two AddCursorNextMatch after substring-search yield two 3-byte matches at bytes 0 and 7".into(),
        initial_text: "foobar foo foo".into(),
        actions,
        expected_text: "foobar foo foo".into(),
        // Newly-added cursor is primary.
        expected_primary: CursorExpect::range(7, 10),
        expected_extra_cursors: vec![CursorExpect::range(0, 3)],
        expected_selection_text: Some("foo\nfoo".into()),
        ..Default::default()
    });
}

#[test]
fn migrated_third_ctrl_d_adds_cursor_at_third_match() {
    // Three AddCursorNextMatch: select-then-add-then-add. End:
    // 3 cursors over all three "foo" substrings (0..3, 7..10,
    // 11..14).
    let mut actions = search_for("foo");
    actions.push(Action::AddCursorNextMatch);
    actions.push(Action::AddCursorNextMatch);
    actions.push(Action::AddCursorNextMatch);

    assert_buffer_scenario(BufferScenario {
        description: "Three AddCursorNextMatch yield three 3-byte cursors over the three 'foo' substrings".into(),
        initial_text: "foobar foo foo".into(),
        actions,
        expected_text: "foobar foo foo".into(),
        expected_primary: CursorExpect::range(11, 14),
        expected_extra_cursors: vec![
            CursorExpect::range(0, 3),
            CursorExpect::range(7, 10),
        ],
        expected_selection_text: Some("foo\nfoo\nfoo".into()),
        ..Default::default()
    });
}

/// Anti-test: drops the search query (the InsertChar sequence)
/// while keeping the bracketing Action::Search /
/// Action::PromptConfirm and the AddCursorNextMatch. With no
/// query confirmed, AddCursorNextMatch has no search state to
/// consult, so the selection it produces cannot match the
/// "foo"-substring expectation. `check_buffer_scenario` must
/// return `Err`.
#[test]
fn anti_search_then_ctrl_d_dropping_query_yields_check_err() {
    let actions = vec![
        Action::Search,
        // Query InsertChars deliberately omitted.
        Action::PromptConfirm,
        Action::AddCursorNextMatch,
    ];
    let scenario = BufferScenario {
        description: "anti: search query dropped — substring selection must not match".into(),
        initial_text: "foobar foo foo".into(),
        actions,
        expected_text: "foobar foo foo".into(),
        // Same expectation as the real one-press scenario: 3-byte
        // "foo" selection. Without the search query confirmed
        // beforehand, AddCursorNextMatch falls back to word
        // semantics and selects "foobar" (6 bytes), so this 3-byte
        // expectation must NOT match.
        expected_primary: CursorExpect::range(0, 3),
        expected_extra_cursors: vec![],
        expected_selection_text: Some("foo".into()),
        ..Default::default()
    };
    assert!(
        check_buffer_scenario(scenario).is_err(),
        "anti-test: with no search query, AddCursorNextMatch falls back to \
         word-selection semantics — the substring expectation must NOT match"
    );
}
