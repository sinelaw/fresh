//! Faithful migration of `tests/e2e/case_conversion.rs`. The
//! existing `semantic/case_conversion.rs` covers only the basic
//! upper-case-on-selection case; this file covers the rest:
//! lowercase, mixed case, unicode, special chars, multi-line,
//! cursor preservation, and undo round-trip.
//!
//! All scenarios route through `Action::ToUpperCase` /
//! `Action::ToLowerCase` — the production action alphabet, no
//! mocks. Selection setup uses real `Action::SelectAll` /
//! `Action::SelectLine` / step-wise `SelectRight`.

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, check_buffer_scenario, repeat, BufferScenario, CursorExpect,
};
use fresh::test_api::Action;

#[test]
fn migrated_to_lowercase_on_selection() {
    // Original: `test_to_lowercase`. Select first 5 chars of
    // "HELLO WORLD" with SelectRight×5, ToLowerCase ⇒ "hello WORLD".
    assert_buffer_scenario(BufferScenario {
        description: "ToLowerCase on selected 'HELLO' yields 'hello WORLD'".into(),
        initial_text: "HELLO WORLD".into(),
        actions: repeat(Action::SelectRight, 5)
            .chain(std::iter::once(Action::ToLowerCase))
            .collect(),
        expected_text: "hello WORLD".into(),
        // ToUpperCase/ToLowerCase clears the selection and parks
        // the cursor at the end of the converted range.
        expected_primary: CursorExpect::at(5),
        ..Default::default()
    });
}

#[test]
fn migrated_to_uppercase_entire_line_via_select_line() {
    // Original: `test_to_uppercase_entire_line`. SelectLine first
    // (Ctrl+L), then ToUpperCase, on "hello world test".
    assert_buffer_scenario(BufferScenario {
        description: "SelectLine + ToUpperCase yields fully-uppercased line".into(),
        initial_text: "hello world test".into(),
        actions: vec![Action::SelectLine, Action::ToUpperCase],
        expected_text: "HELLO WORLD TEST".into(),
        expected_primary: CursorExpect::at(16),
        ..Default::default()
    });
}

#[test]
fn migrated_to_uppercase_mixed_case_select_all() {
    // Original: `test_to_uppercase_mixed_case`.
    assert_buffer_scenario(BufferScenario {
        description: "SelectAll + ToUpperCase normalises mixed-case to uppercase".into(),
        initial_text: "HeLLo WoRLd".into(),
        actions: vec![Action::SelectAll, Action::ToUpperCase],
        expected_text: "HELLO WORLD".into(),
        expected_primary: CursorExpect::at(11),
        ..Default::default()
    });
}

#[test]
fn migrated_to_lowercase_mixed_case_select_all() {
    // Original: `test_to_lowercase_mixed_case`.
    assert_buffer_scenario(BufferScenario {
        description: "SelectAll + ToLowerCase normalises mixed-case to lowercase".into(),
        initial_text: "HeLLo WoRLd".into(),
        actions: vec![Action::SelectAll, Action::ToLowerCase],
        expected_text: "hello world".into(),
        expected_primary: CursorExpect::at(11),
        ..Default::default()
    });
}

#[test]
fn migrated_to_uppercase_preserves_special_chars_and_digits() {
    // Original: `test_case_conversion_with_special_chars`. `_`,
    // `-`, `!`, and digits must round-trip unchanged.
    assert_buffer_scenario(BufferScenario {
        description: "ToUpperCase preserves _, -, !, and digits".into(),
        initial_text: "hello_world-123!".into(),
        actions: vec![Action::SelectAll, Action::ToUpperCase],
        expected_text: "HELLO_WORLD-123!".into(),
        expected_primary: CursorExpect::at(16),
        ..Default::default()
    });
}

#[test]
fn migrated_to_uppercase_unicode_with_diacritics() {
    // Original: `test_case_conversion_unicode`. "café résumé"
    // → "CAFÉ RÉSUMÉ". 'É' is U+00C9 (2 bytes UTF-8).
    let input = "café résumé";
    let expected = "CAFÉ RÉSUMÉ";
    assert_buffer_scenario(BufferScenario {
        description: "ToUpperCase upcases Latin-1 diacritics correctly (café → CAFÉ)".into(),
        initial_text: input.into(),
        actions: vec![Action::SelectAll, Action::ToUpperCase],
        expected_text: expected.into(),
        expected_primary: CursorExpect::at(expected.len()),
        ..Default::default()
    });
}

#[test]
fn migrated_to_uppercase_preserves_end_of_selection_cursor() {
    // Original: `test_case_conversion_preserves_cursor`. After
    // selecting "hello" and upcasing, the cursor stays at byte 5
    // (end of the converted range).
    assert_buffer_scenario(BufferScenario {
        description: "ToUpperCase keeps cursor at end-of-selection (byte 5 for 'hello')".into(),
        initial_text: "hello world".into(),
        actions: repeat(Action::SelectRight, 5)
            .chain(std::iter::once(Action::ToUpperCase))
            .collect(),
        expected_text: "HELLO world".into(),
        expected_primary: CursorExpect::at(5),
        ..Default::default()
    });
}

#[test]
fn migrated_to_uppercase_multiline_select_all() {
    // Original: `test_case_conversion_multiline`. Three short lines
    // joined by '\n'; SelectAll + ToUpperCase upcases every line.
    assert_buffer_scenario(BufferScenario {
        description: "ToUpperCase across multiple lines via SelectAll".into(),
        initial_text: "hello\nworld\ntest".into(),
        actions: vec![Action::SelectAll, Action::ToUpperCase],
        expected_text: "HELLO\nWORLD\nTEST".into(),
        // 5 + 1 + 5 + 1 + 4 = 16 bytes total
        expected_primary: CursorExpect::at(16),
        ..Default::default()
    });
}

#[test]
fn migrated_undo_after_to_uppercase_restores_original_text() {
    // Original: `test_case_conversion_undo`. SelectAll +
    // ToUpperCase, then Undo, must round-trip back to the
    // original buffer text.
    assert_buffer_scenario(BufferScenario {
        description: "Undo after ToUpperCase restores 'hello world'".into(),
        initial_text: "hello world".into(),
        actions: vec![Action::SelectAll, Action::ToUpperCase, Action::Undo],
        expected_text: "hello world".into(),
        // Undo restores the cursor + selection that were active
        // before the ToUpperCase. SelectAll with cursor starting
        // at 0 leaves anchor=0, position=11 (selection forward).
        expected_primary: CursorExpect::range(0, 11),
        ..Default::default()
    });
}

#[test]
fn migrated_to_uppercase_no_selection_upcases_word_under_cursor() {
    // Original: `test_case_conversion_no_selection`. With no
    // selection and the cursor at byte 0 of "hello world",
    // `ToUpperCase` uppercases the word under cursor — i.e. the
    // range [0, 5) ("hello"), leaving " world" untouched.
    //
    // Behavioral note: this contrasts with the prior pinning
    // claim in `migrated_bulk::bulk_uppercase_with_no_selection_uppercases_full_buffer`,
    // which observed full-buffer upcasing on a single-word input
    // ("hello"). The two are consistent — single-word buffers
    // happen to have word_end == buffer_end, so the "word under
    // cursor" rule coincides with "whole buffer" in that case.
    // The multi-word case here is the discriminating evidence:
    // the editor really does use word boundaries, not buffer
    // bounds. See finding #2 in
    // `docs/internal/scenario-migration-findings.md`.
    assert_buffer_scenario(BufferScenario {
        description:
            "ToUpperCase with no selection upcases the word under cursor: 'hello world' → 'HELLO world'"
                .into(),
        initial_text: "hello world".into(),
        actions: vec![Action::ToUpperCase],
        expected_text: "HELLO world".into(),
        // Cursor parks at end of the upcased range (byte 5).
        expected_primary: CursorExpect::at(5),
        ..Default::default()
    });
}

/// Migration of `test_case_conversion_from_command_palette`.
///
/// The e2e drives the keyboard path:
///   Ctrl+P → open palette
///   type "uppercase" → filter command list
///   Enter → execute the selected "Uppercase" command
///
/// The semantic translation routes through the production action
/// alphabet end-to-end, with no test-only back-doors:
///   `Action::SelectAll` to seed the selection,
///   `Action::CommandPalette` to open the prompt,
///   `Action::InsertChar(c)` per character of "uppercase",
///   `Action::PromptConfirm` to execute the highlighted command.
///
/// This validates that the palette's fuzzy match + confirm path
/// genuinely dispatches the buffer-mutating `ToUpperCase` action,
/// not just that it opens a prompt.
#[test]
fn migrated_to_uppercase_via_command_palette() {
    let actions: Vec<Action> = std::iter::once(Action::SelectAll)
        .chain(std::iter::once(Action::CommandPalette))
        .chain("uppercase".chars().map(Action::InsertChar))
        .chain(std::iter::once(Action::PromptConfirm))
        .collect();
    assert_buffer_scenario(BufferScenario {
        description: "CommandPalette → type 'uppercase' → PromptConfirm → buffer uppercased".into(),
        initial_text: "hello world".into(),
        actions,
        expected_text: "HELLO WORLD".into(),
        expected_primary: CursorExpect::at(11),
        ..Default::default()
    });
}

/// Anti-test: drops the `ToUpperCase` from the multiline
/// scenario. Without it, `SelectAll` alone leaves the buffer
/// unchanged ("hello\nworld\ntest"), so the all-caps expectation
/// must NOT match.
#[test]
fn anti_to_uppercase_dropping_action_yields_check_err() {
    let scenario = BufferScenario {
        description: "anti: ToUpperCase dropped — buffer must not be all-caps".into(),
        initial_text: "hello\nworld\ntest".into(),
        actions: vec![Action::SelectAll],
        expected_text: "HELLO\nWORLD\nTEST".into(),
        expected_primary: CursorExpect::at(16),
        ..Default::default()
    };
    assert!(
        check_buffer_scenario(scenario).is_err(),
        "anti-test: SelectAll alone cannot uppercase the buffer; \
         the all-caps expectation must NOT match"
    );
}

/// Anti-test: drops `ToUpperCase` from the no-selection
/// scenario; the buffer should remain "hello world", so claiming
/// "HELLO world" must NOT validate.
#[test]
fn anti_to_uppercase_no_selection_dropping_action_yields_check_err() {
    let scenario = BufferScenario {
        description: "anti: ToUpperCase dropped from no-selection scenario".into(),
        initial_text: "hello world".into(),
        actions: vec![],
        expected_text: "HELLO world".into(),
        expected_primary: CursorExpect::at(5),
        ..Default::default()
    };
    assert!(
        check_buffer_scenario(scenario).is_err(),
        "anti-test: without ToUpperCase, 'hello world' cannot become 'HELLO world'"
    );
}
