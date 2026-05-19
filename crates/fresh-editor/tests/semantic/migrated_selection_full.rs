//! Faithful migration of word/line selection tests from
//! `tests/e2e/selection.rs`. Existing `selection.rs` semantic
//! file covers the ExpandSelection cases; this file adds the
//! per-character-class word-boundary tests
//! (`test_select_word_with_*`) the e2e file pins.

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, BufferScenario, CursorExpect,
};
use fresh::test_api::Action;

#[test]
fn migrated_select_word_with_hyphen_treats_hyphen_as_separator() {
    // Original: `test_select_word_with_hyphen`. Hyphen is a word
    // separator → SelectWord at byte 0 of "foo-bar" picks "foo".
    assert_buffer_scenario(BufferScenario {
        description: "SelectWord with hyphen separator picks 'foo' from 'foo-bar'".into(),
        initial_text: "foo-bar".into(),
        actions: vec![Action::MoveLineStart, Action::SelectWord],
        expected_text: "foo-bar".into(),
        expected_primary: CursorExpect::range(0, 3),
        expected_selection_text: Some("foo".into()),
        ..Default::default()
    });
}

#[test]
fn migrated_select_word_with_underscore_includes_it() {
    // Original: `test_select_word_with_underscore`. Underscore is
    // a word character → SelectWord on "baz_qux" picks the whole
    // identifier.
    assert_buffer_scenario(BufferScenario {
        description: "SelectWord with underscore in identifier picks the whole token".into(),
        initial_text: "baz_qux".into(),
        actions: vec![Action::MoveLineStart, Action::SelectWord],
        expected_text: "baz_qux".into(),
        expected_primary: CursorExpect::range(0, 7),
        expected_selection_text: Some("baz_qux".into()),
        ..Default::default()
    });
}

#[test]
fn migrated_select_word_with_numbers_includes_them() {
    // Original: `test_select_word_with_numbers`. Digits are word
    // chars → SelectWord on "var123" picks the whole identifier.
    assert_buffer_scenario(BufferScenario {
        description: "SelectWord on 'var123' picks the whole alphanumeric run".into(),
        initial_text: "var123".into(),
        actions: vec![Action::MoveLineStart, Action::SelectWord],
        expected_text: "var123".into(),
        expected_primary: CursorExpect::range(0, 6),
        expected_selection_text: Some("var123".into()),
        ..Default::default()
    });
}

#[test]
fn migrated_select_word_with_at_symbol_treats_as_separator() {
    // Original: `test_select_word_with_at_symbol`. '@' is a
    // separator → SelectWord on "user@host" picks "user".
    assert_buffer_scenario(BufferScenario {
        description: "SelectWord with '@' separator picks 'user' from 'user@host'".into(),
        initial_text: "user@host".into(),
        actions: vec![Action::MoveLineStart, Action::SelectWord],
        expected_text: "user@host".into(),
        expected_primary: CursorExpect::range(0, 4),
        expected_selection_text: Some("user".into()),
        ..Default::default()
    });
}

#[test]
fn migrated_select_word_with_dot_treats_as_separator() {
    // Original: `test_select_word_with_dot`.
    assert_buffer_scenario(BufferScenario {
        description: "SelectWord with '.' separator picks 'foo' from 'foo.bar'".into(),
        initial_text: "foo.bar".into(),
        actions: vec![Action::MoveLineStart, Action::SelectWord],
        expected_text: "foo.bar".into(),
        expected_primary: CursorExpect::range(0, 3),
        expected_selection_text: Some("foo".into()),
        ..Default::default()
    });
}

#[test]
fn migrated_select_word_at_start_picks_word_under_cursor() {
    // Original: `test_select_word_at_start` (tests/e2e/selection.rs:141).
    // The e2e positions cursor at byte 6 of "hello world" — start of
    // the *interior* word "world" — and asserts SelectWord picks "world".
    // The original migration used byte 0 of "foo bar" which is the
    // trivial buffer-start path. Pinning the interior boundary here.
    assert_buffer_scenario(BufferScenario {
        description: "SelectWord at byte 6 of 'hello world' (start of 'world') picks 'world'"
            .into(),
        initial_text: "hello world".into(),
        actions: vec![
            Action::MoveLineStart,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::SelectWord,
        ],
        expected_text: "hello world".into(),
        expected_primary: CursorExpect::range(6, 11),
        expected_selection_text: Some("world".into()),
        ..Default::default()
    });
}

#[test]
fn migrated_select_word_at_end_of_first_word_picks_first_word() {
    // Original: `test_select_word_at_end` (tests/e2e/selection.rs:171).
    // The e2e positions cursor at byte 5 of "hello world" — the
    // space *between* the two words — and asserts SelectWord picks
    // "hello". The original migration used MoveDocumentEnd which is
    // the buffer-end edge case, not the inter-word boundary.
    assert_buffer_scenario(BufferScenario {
        description: "SelectWord at byte 5 of 'hello world' (end of 'hello') picks 'hello'".into(),
        initial_text: "hello world".into(),
        actions: vec![
            Action::MoveLineStart,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::MoveRight,
            Action::SelectWord,
        ],
        expected_text: "hello world".into(),
        expected_primary: CursorExpect::range(0, 5),
        expected_selection_text: Some("hello".into()),
        ..Default::default()
    });
}

#[test]
fn migrated_select_line_first_includes_trailing_newline() {
    // Original: `test_select_line_first`. SelectLine on first
    // line of "alpha\nbravo" includes the trailing '\n'.
    assert_buffer_scenario(BufferScenario {
        description: "SelectLine on first of two lines includes trailing newline".into(),
        initial_text: "alpha\nbravo".into(),
        actions: vec![Action::SelectLine],
        expected_text: "alpha\nbravo".into(),
        expected_primary: CursorExpect::range(0, 6),
        expected_selection_text: Some("alpha\n".into()),
        ..Default::default()
    });
}

#[test]
fn migrated_select_line_last_no_trailing_newline_when_absent() {
    // Original: `test_select_line_last`.
    assert_buffer_scenario(BufferScenario {
        description: "SelectLine on last line without trailing newline picks just the text".into(),
        initial_text: "alpha\nbravo".into(),
        actions: vec![Action::MoveDown, Action::SelectLine],
        expected_text: "alpha\nbravo".into(),
        expected_primary: CursorExpect::range(6, 11),
        expected_selection_text: Some("bravo".into()),
        ..Default::default()
    });
}
