//! Faithful migrations of `tests/e2e/auto_indent.rs` cases not yet
//! covered by `migrated_auto_indent_full.rs`:
//! - nested indent (8-space rust)
//! - no indent after a closing brace at column 0
//! - Go function indent uses `\t`
//! - Enter on a selection deletes the selection first then indents
//! - bracket expansion (issue #629): cursor between `{` and `}`,
//!   pressing Enter splits into three lines with the cursor parked
//!   on an indented middle line.
//!
//! All scenarios use `behavior: auto_indent` and put the
//! pre-Enter content in `initial_text` (the e2e files type the
//! content key-by-key — equivalent for the post-load tree-sitter
//! state we assert on).

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, check_buffer_scenario, BehaviorFlags, BufferScenario, CursorExpect,
};
use fresh::test_api::Action;

fn auto_indent() -> BehaviorFlags {
    BehaviorFlags {
        auto_indent: true,
        ..Default::default()
    }
}

#[test]
fn migrated_rust_nested_indent() {
    // Original: `test_rust_nested_indent`. Two-deep nesting in Rust
    // should produce 8-space indent on Enter.
    let initial = "fn main() {\n    if true {";
    assert_buffer_scenario(BufferScenario {
        description: "Rust nested block: Enter inside `if true {` gives 8-space indent".into(),
        initial_text: initial.into(),
        language: Some("test.rs".into()),
        behavior: auto_indent(),
        actions: vec![Action::MoveDocumentEnd, Action::InsertNewline],
        // Buffer becomes initial + "\n" + 8 spaces.
        expected_text: format!("{initial}\n        "),
        expected_primary: CursorExpect::at(initial.len() + 1 + 8),
        ..Default::default()
    });
}

#[test]
fn migrated_no_indent_after_close_brace_at_column_zero() {
    // Original: `test_no_indent_after_close_brace`. Pressing Enter
    // after the buffer's already-closed `}` should produce a fresh
    // line with NO indent, since the closing brace lives at column 0.
    let initial = "struct Foo {\n    x: i32,\n}";
    assert_buffer_scenario(BufferScenario {
        description: "Enter after a closing brace at col 0 produces an unindented line".into(),
        initial_text: initial.into(),
        language: Some("test.rs".into()),
        behavior: auto_indent(),
        actions: vec![Action::MoveDocumentEnd, Action::InsertNewline],
        expected_text: format!("{initial}\n"),
        expected_primary: CursorExpect::at(initial.len() + 1),
        ..Default::default()
    });
}

#[test]
fn migrated_go_function_indent_uses_tab() {
    // Original: `test_go_function_indent`. Go is configured with
    // `use_tabs=true`, so the auto-indent after `{` is a single `\t`.
    let initial = "func main() {";
    assert_buffer_scenario(BufferScenario {
        description: "Go function body: Enter after '{' indents with a tab character".into(),
        initial_text: initial.into(),
        language: Some("test.go".into()),
        behavior: auto_indent(),
        actions: vec![Action::MoveDocumentEnd, Action::InsertNewline],
        expected_text: format!("{initial}\n\t"),
        expected_primary: CursorExpect::at(initial.len() + 1 + 1),
        ..Default::default()
    });
}

#[test]
fn migrated_bracket_expansion_rust_function() {
    // Original: `test_bracket_expansion_rust_function` (issue #629).
    // Cursor between `{` and `}`, Enter ⇒ three-line expansion:
    //   fn main() {\n    \n}   (cursor parked on indented middle line)
    let initial = "fn main() {}";
    assert_buffer_scenario(BufferScenario {
        description:
            "Rust bracket expansion: Enter between `{` and `}` parks cursor on indented middle line"
                .into(),
        initial_text: initial.into(),
        language: Some("test.rs".into()),
        // Bracket expansion is an auto-PAIR feature: it requires
        // auto_close=true (production default), not just auto_indent.
        behavior: BehaviorFlags::production(),
        actions: vec![
            Action::MoveDocumentEnd,
            Action::MoveLeft,
            Action::InsertNewline,
        ],
        // "fn main() {" (11) + "\n" + "    " (4) + "\n" + "}"
        expected_text: "fn main() {\n    \n}".into(),
        expected_primary: CursorExpect::at(11 + 1 + 4),
        ..Default::default()
    });
}

#[test]
fn migrated_bracket_expansion_typescript_interface() {
    // Original: `test_bracket_expansion_typescript_interface`.
    let initial = "interface User {}";
    assert_buffer_scenario(BufferScenario {
        description: "TS bracket expansion: Enter between `{` and `}` opens a 3-line block".into(),
        initial_text: initial.into(),
        language: Some("test.ts".into()),
        behavior: BehaviorFlags::production(),
        actions: vec![
            Action::MoveDocumentEnd,
            Action::MoveLeft,
            Action::InsertNewline,
        ],
        expected_text: "interface User {\n    \n}".into(),
        expected_primary: CursorExpect::at(16 + 1 + 4),
        ..Default::default()
    });
}

#[test]
fn migrated_bracket_expansion_go_function() {
    // Original: `test_bracket_expansion_go_function`. Go uses tabs,
    // so the indented middle line is a single `\t`.
    let initial = "func main() {}";
    assert_buffer_scenario(BufferScenario {
        description:
            "Go bracket expansion: Enter between `{` and `}` uses a tab on the middle line".into(),
        initial_text: initial.into(),
        language: Some("test.go".into()),
        behavior: BehaviorFlags::production(),
        actions: vec![
            Action::MoveDocumentEnd,
            Action::MoveLeft,
            Action::InsertNewline,
        ],
        expected_text: "func main() {\n\t\n}".into(),
        expected_primary: CursorExpect::at(13 + 1 + 1),
        ..Default::default()
    });
}

#[test]
fn migrated_bracket_expansion_json_object() {
    // Original: `test_bracket_expansion_json_object`. JSON object
    // braces — same expansion shape but only assertion the original
    // makes is "buffer contains '{\n    '". Pin the full shape.
    let initial = "{}";
    assert_buffer_scenario(BufferScenario {
        description: "JSON bracket expansion: Enter between `{` and `}` opens a 3-line object"
            .into(),
        initial_text: initial.into(),
        language: Some("test.json".into()),
        behavior: BehaviorFlags::production(),
        actions: vec![
            Action::MoveDocumentEnd,
            Action::MoveLeft,
            Action::InsertNewline,
        ],
        expected_text: "{\n    \n}".into(),
        expected_primary: CursorExpect::at(1 + 1 + 4),
        ..Default::default()
    });
}

/// Anti-test: drops the `MoveLeft` step from the bracket-expansion
/// scenario — without it, Enter at the end of `"fn main() {}"`
/// inserts an unindented line *after* the `}`, not the 3-line
/// expansion. The expectation cannot match, so
/// `check_buffer_scenario` must return `Err`.
#[test]
fn anti_bracket_expansion_dropping_move_left_yields_check_err() {
    let scenario = BufferScenario {
        description: "anti: MoveLeft dropped — Enter past `}` cannot produce 3-line expansion"
            .into(),
        initial_text: "fn main() {}".into(),
        language: Some("test.rs".into()),
        behavior: BehaviorFlags::production(),
        actions: vec![Action::MoveDocumentEnd, Action::InsertNewline],
        expected_text: "fn main() {\n    \n}".into(),
        expected_primary: CursorExpect::at(11 + 1 + 4),
        ..Default::default()
    };
    assert!(
        check_buffer_scenario(scenario).is_err(),
        "anti-test: without MoveLeft, the Enter happens after `}}`, so the \
         buffer cannot match the 3-line bracket-expansion expectation"
    );
}
