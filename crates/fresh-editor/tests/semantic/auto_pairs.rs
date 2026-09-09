//! Track B migration: rewrites of the auto-close / skip-over /
//! auto-pair-delete tests in `tests/e2e/smart_editing.rs`.
//!
//! These behaviors hinge on three config flags (`auto_close`,
//! `auto_indent`, `auto_surround`) that the default test harness
//! turns OFF for simpler unrelated tests. Scenarios here opt in by
//! setting `behavior: BehaviorFlags::production()` on the value.
//!
//! Quote auto-pair scenarios additionally set
//! `language: Some("x.rs".into())` so language detection resolves to
//! Rust (not "text"). `get_auto_close_char` suppresses auto-close
//! for `"`, `'`, `` ` `` when language == "text".

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, BehaviorFlags, BufferScenario, CursorExpect,
};
use fresh::test_api::Action;

// ─────────────────────────────────────────────────────────────────────────
// Auto-close opening delimiters
// ─────────────────────────────────────────────────────────────────────────

#[test]
fn theorem_typing_open_paren_auto_closes() {
    // Replaces test_auto_close_parenthesis.
    // Typing '(' on an empty buffer with auto_close=true inserts both
    // '(' and ')', leaving the cursor between them.
    assert_buffer_scenario(BufferScenario {
        behavior: BehaviorFlags::production(),
        description: "InsertChar('(') auto-inserts ')' and parks cursor between".into(),
        initial_text: "".into(),
        actions: vec![Action::InsertChar('(')],
        expected_text: "()".into(),
        expected_primary: CursorExpect::at(1),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
        ..Default::default()
    });
}

#[test]
fn theorem_typing_open_square_bracket_auto_closes() {
    // Replaces test_auto_close_square_bracket.
    assert_buffer_scenario(BufferScenario {
        behavior: BehaviorFlags::production(),
        description: "InsertChar('[') auto-inserts ']'".into(),
        initial_text: "".into(),
        actions: vec![Action::InsertChar('[')],
        expected_text: "[]".into(),
        expected_primary: CursorExpect::at(1),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
        ..Default::default()
    });
}

#[test]
fn theorem_typing_open_curly_brace_auto_closes() {
    // Replaces test_auto_close_curly_brace.
    assert_buffer_scenario(BufferScenario {
        behavior: BehaviorFlags::production(),
        description: "InsertChar('{') auto-inserts '}'".into(),
        initial_text: "".into(),
        actions: vec![Action::InsertChar('{')],
        expected_text: "{}".into(),
        expected_primary: CursorExpect::at(1),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
        ..Default::default()
    });
}

#[test]
fn theorem_no_auto_close_before_alphanumeric() {
    // Replaces test_no_auto_close_before_alphanumeric.
    // When the cursor sits before an alphanumeric char, opening a
    // bracket inserts only the open bracket — no auto-close.
    // Initial buffer: "abc", cursor at 0 (before 'a').
    assert_buffer_scenario(BufferScenario {
        behavior: BehaviorFlags::production(),
        description: "Auto-close suppressed before an alphanumeric char".into(),
        initial_text: "abc".into(),
        actions: vec![Action::InsertChar('(')],
        expected_text: "(abc".into(),
        expected_primary: CursorExpect::at(1),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
        ..Default::default()
    });
}

#[test]
fn theorem_auto_close_before_whitespace() {
    // Replaces test_auto_close_before_whitespace.
    // Whitespace doesn't suppress auto-close; cursor at 0 with a
    // space char-after still pairs the bracket.
    assert_buffer_scenario(BufferScenario {
        behavior: BehaviorFlags::production(),
        description: "Auto-close fires when char-after is whitespace".into(),
        initial_text: " abc".into(),
        actions: vec![Action::InsertChar('(')],
        expected_text: "() abc".into(),
        expected_primary: CursorExpect::at(1),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
        ..Default::default()
    });
}

#[test]
fn theorem_no_auto_close_when_config_disabled() {
    // Replaces test_no_auto_close_when_config_disabled.
    // With `auto_close: false`, opening bracket inserts a single
    // char — same as the harness default. Captured as a positive
    // theorem so a future regression that flips the default would
    // surface here.
    assert_buffer_scenario(BufferScenario {
        description: "auto_close=false makes '(' a single-char insert".into(),
        initial_text: "".into(),
        actions: vec![Action::InsertChar('(')],
        expected_text: "(".into(),
        expected_primary: CursorExpect::at(1),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
        ..Default::default()
    });
}

// ─────────────────────────────────────────────────────────────────────────
// Skip-over closing delimiters
// ─────────────────────────────────────────────────────────────────────────

#[test]
fn theorem_typing_closing_paren_skips_over_existing() {
    // Replaces test_skip_over_closing_parenthesis.
    // After auto-close inserts "()", typing ')' again must NOT add
    // a third paren — it should advance the cursor past the existing
    // close.
    assert_buffer_scenario(BufferScenario {
        behavior: BehaviorFlags::production(),
        description: "InsertChar(')') with cursor before ')' just advances the cursor".into(),
        initial_text: "".into(),
        actions: vec![Action::InsertChar('('), Action::InsertChar(')')],
        expected_text: "()".into(),
        expected_primary: CursorExpect::at(2),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
        ..Default::default()
    });
}

#[test]
fn theorem_typing_closing_bracket_skips_over_existing() {
    // Replaces test_skip_over_closing_bracket.
    assert_buffer_scenario(BufferScenario {
        behavior: BehaviorFlags::production(),
        description: "InsertChar(']') with cursor before ']' just advances the cursor".into(),
        initial_text: "".into(),
        actions: vec![Action::InsertChar('['), Action::InsertChar(']')],
        expected_text: "[]".into(),
        expected_primary: CursorExpect::at(2),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
        ..Default::default()
    });
}

#[test]
fn theorem_typing_closing_brace_skips_over_existing() {
    // Replaces test_skip_over_closing_brace.
    assert_buffer_scenario(BufferScenario {
        behavior: BehaviorFlags::production(),
        description: "InsertChar('}') with cursor before '}' just advances the cursor".into(),
        initial_text: "".into(),
        actions: vec![Action::InsertChar('{'), Action::InsertChar('}')],
        expected_text: "{}".into(),
        expected_primary: CursorExpect::at(2),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
        ..Default::default()
    });
}

#[test]
fn theorem_no_skip_over_when_next_char_differs() {
    // Replaces test_no_skip_when_different_char.
    // Initial "(x", cursor moves to position 1 (between '(' and 'x'),
    // then types ')'.  With auto_close on and char_after='x' (not ')'),
    // the skip-over branch does not fire and ')' is inserted normally.
    // FINDING: the original asserted only buffer text "()x" and
    // ignored cursor position; the theorem also pins cursor at 2.
    assert_buffer_scenario(BufferScenario {
        behavior: BehaviorFlags::production(),
        description: "Closing ')' with non-matching char-after inserts normally".into(),
        initial_text: "(x".into(),
        actions: vec![
            Action::MoveDocumentStart,
            Action::MoveRight,
            Action::InsertChar(')'),
        ],
        expected_text: "()x".into(),
        expected_primary: CursorExpect::at(2),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
        ..Default::default()
    });
}

// ─────────────────────────────────────────────────────────────────────────
// Auto-pair deletion (Backspace between empty matched pair)
// ─────────────────────────────────────────────────────────────────────────

#[test]
fn theorem_backspace_between_empty_parens_deletes_both() {
    // Replaces test_auto_pair_delete_parentheses.
    // After typing "(", auto-close yields "()" with cursor between.
    // Backspace must remove BOTH characters, not just the open paren.
    assert_buffer_scenario(BufferScenario {
        behavior: BehaviorFlags::production(),
        description: "Backspace between an empty () pair deletes both".into(),
        initial_text: "".into(),
        actions: vec![Action::InsertChar('('), Action::DeleteBackward],
        expected_text: "".into(),
        expected_primary: CursorExpect::at(0),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
        ..Default::default()
    });
}

#[test]
fn theorem_backspace_between_empty_brackets_deletes_both() {
    // Replaces test_auto_pair_delete_square_brackets.
    assert_buffer_scenario(BufferScenario {
        behavior: BehaviorFlags::production(),
        description: "Backspace between an empty [] pair deletes both".into(),
        initial_text: "".into(),
        actions: vec![Action::InsertChar('['), Action::DeleteBackward],
        expected_text: "".into(),
        expected_primary: CursorExpect::at(0),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
        ..Default::default()
    });
}

#[test]
fn theorem_backspace_between_empty_braces_deletes_both() {
    // Replaces test_auto_pair_delete_curly_braces.
    assert_buffer_scenario(BufferScenario {
        behavior: BehaviorFlags::production(),
        description: "Backspace between an empty {} pair deletes both".into(),
        initial_text: "".into(),
        actions: vec![Action::InsertChar('{'), Action::DeleteBackward],
        expected_text: "".into(),
        expected_primary: CursorExpect::at(0),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
        ..Default::default()
    });
}

#[test]
fn theorem_backspace_does_not_pair_delete_when_content_between() {
    // Replaces test_no_pair_delete_with_content_between.
    // Initial "(x)", cursor between '(' and 'x'.  Backspace removes
    // only '(' — pair-delete only fires when the pair is empty.
    assert_buffer_scenario(BufferScenario {
        behavior: BehaviorFlags::production(),
        description: "Backspace at non-empty pair deletes only the opening char".into(),
        initial_text: "(x)".into(),
        actions: vec![
            Action::MoveDocumentStart,
            Action::MoveRight,
            Action::DeleteBackward,
        ],
        expected_text: "x)".into(),
        expected_primary: CursorExpect::at(0),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
        ..Default::default()
    });
}

// ─────────────────────────────────────────────────────────────────────────
// Quote auto-pair (require non-"text" language → load via x.rs file)
// ─────────────────────────────────────────────────────────────────────────

#[test]
fn theorem_typing_double_quote_auto_closes_in_rust() {
    // Replaces test_auto_close_double_quotes.
    // `get_auto_close_char` only disables quote auto-close in language="text"
    // and language="markdown"/"mdx". For Rust (loaded via x.rs), '"' pairs.
    assert_buffer_scenario(BufferScenario {
        behavior: BehaviorFlags::production(),
        language: Some("x.rs".into()),
        description: "InsertChar('\"') auto-pairs in a Rust buffer".into(),
        initial_text: "".into(),
        actions: vec![Action::InsertChar('"')],
        expected_text: "\"\"".into(),
        expected_primary: CursorExpect::at(1),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
        ..Default::default()
    });
}

#[test]
fn theorem_typing_single_quote_auto_closes_in_rust() {
    // Replaces test_auto_close_single_quotes.
    assert_buffer_scenario(BufferScenario {
        behavior: BehaviorFlags::production(),
        language: Some("x.rs".into()),
        description: "InsertChar('\\'') auto-pairs in a Rust buffer".into(),
        initial_text: "".into(),
        actions: vec![Action::InsertChar('\'')],
        expected_text: "''".into(),
        expected_primary: CursorExpect::at(1),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
        ..Default::default()
    });
}

#[test]
fn theorem_typing_backtick_auto_closes_in_rust() {
    // Replaces test_auto_close_backtick.
    assert_buffer_scenario(BufferScenario {
        behavior: BehaviorFlags::production(),
        language: Some("x.rs".into()),
        description: "InsertChar('`') auto-pairs in a Rust buffer".into(),
        initial_text: "".into(),
        actions: vec![Action::InsertChar('`')],
        expected_text: "``".into(),
        expected_primary: CursorExpect::at(1),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
        ..Default::default()
    });
}

#[test]
fn theorem_typing_closing_quote_skips_over_existing() {
    // Replaces test_skip_over_closing_quote.
    // After auto-close inserts `""`, typing `"` again must just advance
    // the cursor — no third quote.
    assert_buffer_scenario(BufferScenario {
        behavior: BehaviorFlags::production(),
        language: Some("x.rs".into()),
        description: "Typing '\"' before existing '\"' just advances the cursor".into(),
        initial_text: "".into(),
        actions: vec![Action::InsertChar('"'), Action::InsertChar('"')],
        expected_text: "\"\"".into(),
        expected_primary: CursorExpect::at(2),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
        ..Default::default()
    });
}

#[test]
fn theorem_backspace_between_empty_double_quotes_deletes_both() {
    // Replaces test_auto_pair_delete_double_quotes.
    assert_buffer_scenario(BufferScenario {
        behavior: BehaviorFlags::production(),
        language: Some("x.rs".into()),
        description: "Backspace between empty \"\" pair deletes both".into(),
        initial_text: "".into(),
        actions: vec![Action::InsertChar('"'), Action::DeleteBackward],
        expected_text: "".into(),
        expected_primary: CursorExpect::at(0),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
        ..Default::default()
    });
}

#[test]
fn theorem_backspace_between_empty_single_quotes_deletes_both() {
    // Replaces test_auto_pair_delete_single_quotes.
    assert_buffer_scenario(BufferScenario {
        behavior: BehaviorFlags::production(),
        language: Some("x.rs".into()),
        description: "Backspace between empty '' pair deletes both".into(),
        initial_text: "".into(),
        actions: vec![Action::InsertChar('\''), Action::DeleteBackward],
        expected_text: "".into(),
        expected_primary: CursorExpect::at(0),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
        ..Default::default()
    });
}

// ─────────────────────────────────────────────────────────────────────────
// Markdown: auto-close off by default, surround still on
// ─────────────────────────────────────────────────────────────────────────

#[test]
fn theorem_typing_backtick_does_not_auto_close_in_markdown() {
    // `languages.markdown.auto_close` defaults to false: prose types
    // backticks as literal text far more often than as a pair.
    assert_buffer_scenario(BufferScenario {
        behavior: BehaviorFlags::production(),
        language: Some("x.md".into()),
        description: "InsertChar('`') inserts one backtick in a Markdown buffer".into(),
        initial_text: "".into(),
        actions: vec![Action::InsertChar('`')],
        expected_text: "`".into(),
        expected_primary: CursorExpect::at(1),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
        ..Default::default()
    });
}

#[test]
fn theorem_typing_open_paren_does_not_auto_close_in_markdown() {
    assert_buffer_scenario(BufferScenario {
        behavior: BehaviorFlags::production(),
        language: Some("x.md".into()),
        description: "InsertChar('(') inserts one paren in a Markdown buffer".into(),
        initial_text: "".into(),
        actions: vec![Action::InsertChar('(')],
        expected_text: "(".into(),
        expected_primary: CursorExpect::at(1),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
        ..Default::default()
    });
}

#[test]
fn theorem_typing_over_a_selection_replaces_it_in_markdown() {
    // `auto_surround` is off for Markdown too, so a typed delimiter replaces
    // the selection like any other character instead of wrapping it.
    assert_buffer_scenario(BufferScenario {
        behavior: BehaviorFlags::production(),
        language: Some("x.md".into()),
        description: "Selecting `code` and typing '`' replaces it in a Markdown buffer".into(),
        initial_text: "code".into(),
        actions: vec![
            Action::SelectRight,
            Action::SelectRight,
            Action::SelectRight,
            Action::SelectRight,
            Action::InsertChar('`'),
        ],
        expected_text: "`".into(),
        expected_primary: CursorExpect::at(1),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
        ..Default::default()
    });
}
