//! Sparse migration of `tests/e2e/smart_editing.rs` — covers the
//! auto-pair-quote contrast (text buffer = no pair; Rust buffer
//! with auto_close = pair). The full e2e (1622 lines) covers many
//! more bracket / quote / smart-indent permutations across
//! multiple languages — see #2058 for the coverage gap.
//!
//! Note: goto-matching-bracket migration lives in
//! `migrated_goto_matching_bracket.rs` (faithful, preserves issue
//! #1258). It does not live here.

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, BehaviorFlags, BufferScenario, CursorExpect,
};
use fresh::test_api::Action;

#[test]
fn migrated_typing_quotes_in_text_buffer_does_not_auto_pair() {
    // Original: test_auto_close_double_quotes (text-buffer leg).
    // In a text buffer (no language), quote chars don't auto-pair.
    assert_buffer_scenario(BufferScenario {
        description: "InsertChar('\"') in text buffer inserts one char".into(),
        initial_text: String::new(),
        actions: vec![Action::InsertChar('"')],
        expected_text: "\"".into(),
        expected_primary: CursorExpect::at(1),
        ..Default::default()
    });
}

#[test]
fn migrated_typing_quotes_in_rust_buffer_auto_pairs() {
    // Original: test_auto_close_double_quotes (rust-buffer leg).
    // Quote chars do auto-pair in language=rust with
    // auto_close=true; cursor lands inside the pair at byte 1.
    assert_buffer_scenario(BufferScenario {
        description: "InsertChar('\"') in .rs buffer with auto_close=true pairs the quote".into(),
        initial_text: String::new(),
        behavior: BehaviorFlags::production(),
        language: Some("x.rs".into()),
        actions: vec![Action::InsertChar('"')],
        expected_text: "\"\"".into(),
        expected_primary: CursorExpect::at(1),
        ..Default::default()
    });
}

/// Anti-test: dropping the language config means no auto-pair.
/// Pins that the auto-pair behavior is gated on the language
/// detection, not just on the auto_close flag.
#[test]
fn anti_quotes_without_language_do_not_auto_pair() {
    use crate::common::scenario::buffer_scenario::check_buffer_scenario;
    let scenario = BufferScenario {
        description: "anti: no language ⇒ no auto-pair even with auto_close=true".into(),
        initial_text: String::new(),
        behavior: BehaviorFlags::production(),
        // language: None,
        actions: vec![Action::InsertChar('"')],
        expected_text: "\"\"".into(),
        expected_primary: CursorExpect::at(1),
        ..Default::default()
    };
    assert!(
        check_buffer_scenario(scenario).is_err(),
        "anti-test: no-language buffer must NOT auto-pair the quote"
    );
}
