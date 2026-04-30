//! Track B migration: rewrites of the auto-close / skip-over /
//! auto-pair-delete tests in `tests/e2e/smart_editing.rs`.
//!
//! These behaviors hinge on three config flags (`auto_close`,
//! `auto_indent`, `auto_surround`) that the default test harness
//! turns OFF for simpler unrelated tests. Theorems here opt in via
//! `assert_buffer_theorem_with_behavior` + `BehaviorFlags::production()`.
//!
//! Quote-close tests are deferred: `get_auto_close_char` disables
//! auto-close for `"`, `'`, `` ` `` when `state.language == "text"`.
//! The semantic harness opens an unnamed `.txt` buffer, so the
//! language is always "text". Migrating the quote subset needs a
//! `load_buffer_from_text_named(name, content)` extension to set the
//! file extension and trigger non-"text" language detection.

use crate::common::theorem::buffer_theorem::{
    assert_buffer_theorem, assert_buffer_theorem_with_behavior, BehaviorFlags, BufferTheorem,
    CursorExpect,
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
    assert_buffer_theorem_with_behavior(
        BufferTheorem {
            description: "InsertChar('(') auto-inserts ')' and parks cursor between",
            initial_text: "",
            actions: vec![Action::InsertChar('(')],
            expected_text: "()",
            expected_primary: CursorExpect::at(1),
            expected_extra_cursors: vec![],
            expected_selection_text: None,
        },
        BehaviorFlags::production(),
    );
}

#[test]
fn theorem_typing_open_square_bracket_auto_closes() {
    // Replaces test_auto_close_square_bracket.
    assert_buffer_theorem_with_behavior(
        BufferTheorem {
            description: "InsertChar('[') auto-inserts ']'",
            initial_text: "",
            actions: vec![Action::InsertChar('[')],
            expected_text: "[]",
            expected_primary: CursorExpect::at(1),
            expected_extra_cursors: vec![],
            expected_selection_text: None,
        },
        BehaviorFlags::production(),
    );
}

#[test]
fn theorem_typing_open_curly_brace_auto_closes() {
    // Replaces test_auto_close_curly_brace.
    assert_buffer_theorem_with_behavior(
        BufferTheorem {
            description: "InsertChar('{') auto-inserts '}'",
            initial_text: "",
            actions: vec![Action::InsertChar('{')],
            expected_text: "{}",
            expected_primary: CursorExpect::at(1),
            expected_extra_cursors: vec![],
            expected_selection_text: None,
        },
        BehaviorFlags::production(),
    );
}

#[test]
fn theorem_no_auto_close_before_alphanumeric() {
    // Replaces test_no_auto_close_before_alphanumeric.
    // When the cursor sits before an alphanumeric char, opening a
    // bracket inserts only the open bracket — no auto-close.
    // Initial buffer: "abc", cursor at 0 (before 'a').
    assert_buffer_theorem_with_behavior(
        BufferTheorem {
            description: "Auto-close suppressed before an alphanumeric char",
            initial_text: "abc",
            actions: vec![Action::InsertChar('(')],
            expected_text: "(abc",
            expected_primary: CursorExpect::at(1),
            expected_extra_cursors: vec![],
            expected_selection_text: None,
        },
        BehaviorFlags::production(),
    );
}

#[test]
fn theorem_auto_close_before_whitespace() {
    // Replaces test_auto_close_before_whitespace.
    // Whitespace doesn't suppress auto-close; cursor at 0 with a
    // space char-after still pairs the bracket.
    assert_buffer_theorem_with_behavior(
        BufferTheorem {
            description: "Auto-close fires when char-after is whitespace",
            initial_text: " abc",
            actions: vec![Action::InsertChar('(')],
            expected_text: "() abc",
            expected_primary: CursorExpect::at(1),
            expected_extra_cursors: vec![],
            expected_selection_text: None,
        },
        BehaviorFlags::production(),
    );
}

#[test]
fn theorem_no_auto_close_when_config_disabled() {
    // Replaces test_no_auto_close_when_config_disabled.
    // With `auto_close: false`, opening bracket inserts a single
    // char — same as the harness default. Captured as a positive
    // theorem so a future regression that flips the default would
    // surface here.
    assert_buffer_theorem(BufferTheorem {
        description: "auto_close=false makes '(' a single-char insert",
        initial_text: "",
        actions: vec![Action::InsertChar('(')],
        expected_text: "(",
        expected_primary: CursorExpect::at(1),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
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
    assert_buffer_theorem_with_behavior(
        BufferTheorem {
            description: "InsertChar(')') with cursor before ')' just advances the cursor",
            initial_text: "",
            actions: vec![Action::InsertChar('('), Action::InsertChar(')')],
            expected_text: "()",
            expected_primary: CursorExpect::at(2),
            expected_extra_cursors: vec![],
            expected_selection_text: None,
        },
        BehaviorFlags::production(),
    );
}

#[test]
fn theorem_typing_closing_bracket_skips_over_existing() {
    // Replaces test_skip_over_closing_bracket.
    assert_buffer_theorem_with_behavior(
        BufferTheorem {
            description: "InsertChar(']') with cursor before ']' just advances the cursor",
            initial_text: "",
            actions: vec![Action::InsertChar('['), Action::InsertChar(']')],
            expected_text: "[]",
            expected_primary: CursorExpect::at(2),
            expected_extra_cursors: vec![],
            expected_selection_text: None,
        },
        BehaviorFlags::production(),
    );
}

#[test]
fn theorem_typing_closing_brace_skips_over_existing() {
    // Replaces test_skip_over_closing_brace.
    assert_buffer_theorem_with_behavior(
        BufferTheorem {
            description: "InsertChar('}') with cursor before '}' just advances the cursor",
            initial_text: "",
            actions: vec![Action::InsertChar('{'), Action::InsertChar('}')],
            expected_text: "{}",
            expected_primary: CursorExpect::at(2),
            expected_extra_cursors: vec![],
            expected_selection_text: None,
        },
        BehaviorFlags::production(),
    );
}

#[test]
fn theorem_no_skip_over_when_next_char_differs() {
    // Replaces test_no_skip_when_different_char.
    // Initial "(x", cursor moves to position 1 (between '(' and 'x'),
    // then types ')'.  With auto_close on and char_after='x' (not ')'),
    // the skip-over branch does not fire and ')' is inserted normally.
    // FINDING: the original asserted only buffer text "()x" and
    // ignored cursor position; the theorem also pins cursor at 2.
    assert_buffer_theorem_with_behavior(
        BufferTheorem {
            description: "Closing ')' with non-matching char-after inserts normally",
            initial_text: "(x",
            actions: vec![
                Action::MoveDocumentStart,
                Action::MoveRight,
                Action::InsertChar(')'),
            ],
            expected_text: "()x",
            expected_primary: CursorExpect::at(2),
            expected_extra_cursors: vec![],
            expected_selection_text: None,
        },
        BehaviorFlags::production(),
    );
}

// ─────────────────────────────────────────────────────────────────────────
// Auto-pair deletion (Backspace between empty matched pair)
// ─────────────────────────────────────────────────────────────────────────

#[test]
fn theorem_backspace_between_empty_parens_deletes_both() {
    // Replaces test_auto_pair_delete_parentheses.
    // After typing "(", auto-close yields "()" with cursor between.
    // Backspace must remove BOTH characters, not just the open paren.
    assert_buffer_theorem_with_behavior(
        BufferTheorem {
            description: "Backspace between an empty () pair deletes both",
            initial_text: "",
            actions: vec![Action::InsertChar('('), Action::DeleteBackward],
            expected_text: "",
            expected_primary: CursorExpect::at(0),
            expected_extra_cursors: vec![],
            expected_selection_text: None,
        },
        BehaviorFlags::production(),
    );
}

#[test]
fn theorem_backspace_between_empty_brackets_deletes_both() {
    // Replaces test_auto_pair_delete_square_brackets.
    assert_buffer_theorem_with_behavior(
        BufferTheorem {
            description: "Backspace between an empty [] pair deletes both",
            initial_text: "",
            actions: vec![Action::InsertChar('['), Action::DeleteBackward],
            expected_text: "",
            expected_primary: CursorExpect::at(0),
            expected_extra_cursors: vec![],
            expected_selection_text: None,
        },
        BehaviorFlags::production(),
    );
}

#[test]
fn theorem_backspace_between_empty_braces_deletes_both() {
    // Replaces test_auto_pair_delete_curly_braces.
    assert_buffer_theorem_with_behavior(
        BufferTheorem {
            description: "Backspace between an empty {} pair deletes both",
            initial_text: "",
            actions: vec![Action::InsertChar('{'), Action::DeleteBackward],
            expected_text: "",
            expected_primary: CursorExpect::at(0),
            expected_extra_cursors: vec![],
            expected_selection_text: None,
        },
        BehaviorFlags::production(),
    );
}

#[test]
fn theorem_backspace_does_not_pair_delete_when_content_between() {
    // Replaces test_no_pair_delete_with_content_between.
    // Initial "(x)", cursor between '(' and 'x'.  Backspace removes
    // only '(' — pair-delete only fires when the pair is empty.
    assert_buffer_theorem_with_behavior(
        BufferTheorem {
            description: "Backspace at non-empty pair deletes only the opening char",
            initial_text: "(x)",
            actions: vec![
                Action::MoveDocumentStart,
                Action::MoveRight,
                Action::DeleteBackward,
            ],
            expected_text: "x)",
            expected_primary: CursorExpect::at(0),
            expected_extra_cursors: vec![],
            expected_selection_text: None,
        },
        BehaviorFlags::production(),
    );
}
