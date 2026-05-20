//! Sparse migration of `tests/e2e/smart_editing.rs` — covers the
//! auto-pair-quote contrast (text buffer = no pair; Rust buffer
//! with auto_close = pair). The full e2e (44 tests) covers many
//! more bracket / quote / smart-indent permutations across
//! multiple languages — see #2058 for the coverage gap.
//!
//! Note: goto-matching-bracket migration lives in
//! `migrated_goto_matching_bracket.rs` (faithful, preserves issue
//! #1258). It does not live here.
//!
//! UNMIGRATED GROUPS (no coverage lost — `tests/e2e/smart_editing.rs`
//! still exists and guards these; tracked in #2058):
//!   - Macro record/playback ×6: test_macro_recording_toggle,
//!     test_macro_record_and_playback, test_multiple_macro_slots,
//!     test_play_nonexistent_macro, test_toggle_macro_recording,
//!     test_macro_recording_hint_shows_correct_keybinding. Need
//!     macro-recording state + keybinding-hint observation that the
//!     BufferScenario action layer does not expose.
//!   - Jump-to-error ×5: test_jump_to_next_error,
//!     test_jump_to_previous_error, test_jump_to_next_error_wraps,
//!     test_jump_to_error_no_diagnostics,
//!     test_jump_through_multiple_errors. Require seeded LSP
//!     diagnostics, which a pure buffer scenario cannot inject.
//!   - Block (column) selection ×5: test_block_selection_start,
//!     test_block_selection_vertical, test_block_selection_rectangle,
//!     test_block_selection_left, test_block_selection_up. Driven by
//!     Alt+Shift mouse/arrow block-anchor input; block-selection
//!     migrations live in migrated_block_selection_{full,extras}.rs,
//!     not here.
//!   - Auto-close config-toggle ×4:
//!     test_no_auto_close_when_auto_close_config_disabled,
//!     test_no_skip_over_when_auto_close_config_disabled,
//!     test_no_auto_pair_delete_when_auto_close_config_disabled,
//!     test_auto_close_works_independently_from_auto_indent. Assert
//!     behavior under explicitly-disabled config; the negative
//!     (no-auto-close) legs overlap with the anti-test below, but the
//!     full disabled-config matrix is not yet ported.
//!   - Surround-selection ×4: test_surround_selection_parenthesis,
//!     test_surround_selection_square_bracket,
//!     test_surround_selection_double_quote,
//!     test_surround_selection_disabled. Need the auto_surround path
//!     plus an active selection wrapped by a typed bracket; not yet
//!     ported.

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
