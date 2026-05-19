//! Faithful migration of `tests/e2e/auto_indent.rs` (per-language
//! indent behavior subset).
//!
//! Each test sets `language: Some("x.<ext>")` so language
//! detection picks the right rules, and `behavior: { auto_indent:
//! true, .. }` to enable production auto-indent. The action
//! sequence is `MoveDocumentEnd + InsertNewline` — same as the
//! e2e's Ctrl+End + Enter.

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
fn migrated_rust_auto_indent_after_opening_brace() {
    // Original: `test_rust_auto_indent_after_brace`.
    // After "fn main() {" + Enter, expect "fn main() {\n    "
    // (4-space indent on next line).
    assert_buffer_scenario(BufferScenario {
        description: "Rust auto-indent: Enter after '{' gives 4-space indent".into(),
        initial_text: "fn main() {".into(),
        language: Some("x.rs".into()),
        behavior: auto_indent(),
        actions: vec![Action::MoveDocumentEnd, Action::InsertNewline],
        expected_text: "fn main() {\n    ".into(),
        expected_primary: CursorExpect::at(16),
        ..Default::default()
    });
}

#[test]
fn migrated_python_auto_indent_after_colon() {
    // Original: `test_python_auto_indent_after_colon`.
    assert_buffer_scenario(BufferScenario {
        description: "Python auto-indent: Enter after ':' gives 4-space indent".into(),
        initial_text: "def foo():".into(),
        language: Some("x.py".into()),
        behavior: auto_indent(),
        actions: vec![Action::MoveDocumentEnd, Action::InsertNewline],
        expected_text: "def foo():\n    ".into(),
        expected_primary: CursorExpect::at(15),
        ..Default::default()
    });
}

#[test]
fn migrated_javascript_auto_indent_after_opening_brace() {
    // Original: `test_javascript_auto_indent_after_brace`.
    assert_buffer_scenario(BufferScenario {
        description: "JS auto-indent: Enter after '{' gives 4-space indent".into(),
        initial_text: "function test() {".into(),
        language: Some("x.js".into()),
        behavior: auto_indent(),
        actions: vec![Action::MoveDocumentEnd, Action::InsertNewline],
        expected_text: "function test() {\n    ".into(),
        expected_primary: CursorExpect::at(22),
        ..Default::default()
    });
}

#[test]
fn migrated_typescript_interface_indent() {
    // Original: `test_typescript_interface_indent`.
    assert_buffer_scenario(BufferScenario {
        description: "TS auto-indent inside interface body".into(),
        initial_text: "interface Foo {".into(),
        language: Some("x.ts".into()),
        behavior: auto_indent(),
        actions: vec![Action::MoveDocumentEnd, Action::InsertNewline],
        expected_text: "interface Foo {\n    ".into(),
        expected_primary: CursorExpect::at(20),
        ..Default::default()
    });
}

#[test]
fn migrated_cpp_class_indent() {
    // Original: `test_cpp_class_indent`.
    assert_buffer_scenario(BufferScenario {
        description: "C++ auto-indent inside class body".into(),
        initial_text: "class Foo {".into(),
        language: Some("x.cpp".into()),
        behavior: auto_indent(),
        actions: vec![Action::MoveDocumentEnd, Action::InsertNewline],
        expected_text: "class Foo {\n    ".into(),
        expected_primary: CursorExpect::at(16),
        ..Default::default()
    });
}

#[test]
fn migrated_json_object_indent() {
    // Original: `test_json_object_indent`.
    assert_buffer_scenario(BufferScenario {
        description: "JSON auto-indent inside object body".into(),
        initial_text: "{".into(),
        language: Some("x.json".into()),
        behavior: auto_indent(),
        actions: vec![Action::MoveDocumentEnd, Action::InsertNewline],
        expected_text: "{\n    ".into(),
        expected_primary: CursorExpect::at(6),
        ..Default::default()
    });
}

#[test]
fn migrated_fallback_copies_previous_indent() {
    // Original: `test_fallback_copies_previous_indent`.
    // Plain text buffer: Enter at end of an indented line should
    // carry the indent over (not language-specific).
    assert_buffer_scenario(BufferScenario {
        description: "auto_indent on plain text copies previous line's indent".into(),
        initial_text: "    indented line".into(),
        // language=None ⇒ "text"; the fallback rule is just
        // "copy whitespace prefix of previous line".
        behavior: auto_indent(),
        actions: vec![Action::MoveDocumentEnd, Action::InsertNewline],
        expected_text: "    indented line\n    ".into(),
        expected_primary: CursorExpect::at(22),
        ..Default::default()
    });
}

/// Anti-test: drops `InsertNewline` from
/// `migrated_rust_auto_indent_after_opening_brace`. Without it,
/// the buffer stays at the initial "fn main() {" and the expected
/// post-newline-with-indent text "fn main() {\n    " cannot
/// match, so `check_buffer_scenario` must return Err — proving
/// the InsertNewline is what drives the auto-indent action.
#[test]
fn anti_auto_indent_dropping_insert_newline_yields_check_err() {
    let scenario = BufferScenario {
        description: "anti: InsertNewline dropped — auto-indent never fires".into(),
        initial_text: "fn main() {".into(),
        language: Some("x.rs".into()),
        behavior: auto_indent(),
        actions: vec![Action::MoveDocumentEnd],
        expected_text: "fn main() {\n    ".into(),
        expected_primary: CursorExpect::at(16),
        ..Default::default()
    };
    assert!(
        check_buffer_scenario(scenario).is_err(),
        "anti-test: without InsertNewline, the buffer stays at 'fn main() {{' \
         and the auto-indented '\\n    ' suffix cannot appear"
    );
}

#[test]
fn migrated_auto_indent_disabled_no_indent() {
    // Original: `test_auto_indent_disabled_by_config`. With
    // auto_indent=false, Enter does not carry indent.
    assert_buffer_scenario(BufferScenario {
        description: "auto_indent=false: Enter starts new line at col 0".into(),
        initial_text: "fn main() {".into(),
        language: Some("x.rs".into()),
        behavior: BehaviorFlags {
            auto_indent: false,
            ..Default::default()
        },
        actions: vec![Action::MoveDocumentEnd, Action::InsertNewline],
        expected_text: "fn main() {\n".into(),
        expected_primary: CursorExpect::at(12),
        ..Default::default()
    });
}
