//! Hand-curated corpus of `BufferScenario` values.
//!
//! The corpus is a starting set of scenarios that exercise the bulk
//! of the editor's pure-state surface. It feeds two consumers:
//!
//! - [`shadow_corpus`] — every shadow model runs against every
//!   applicable scenario and must agree with the live editor.
//! - [`corpus_dump`] — serialises the entire set to JSON so external
//!   drivers (proptest soak job, regression harness, CI dashboards)
//!   have a stable, version-controlled starting point.
//!
//! Each entry is a `BufferScenario` value with a unique
//! `description`. As the migration progresses, new scenarios are
//! added here in addition to (not instead of) their domain test
//! file — the per-domain file remains the home for human-readable
//! commentary; this file is the canonical machine-readable list.
//!
//! [`shadow_corpus`]: super::shadow_corpus
//! [`corpus_dump`]: super::corpus_dump

use crate::common::scenario::buffer_scenario::{BufferScenario, CursorExpect};
use fresh::test_api::Action;

pub fn buffer_scenarios() -> Vec<BufferScenario> {
    vec![
        BufferScenario {
            description: "identity: no actions on a non-empty buffer leaves text and cursor".into(),
            initial_text: "hello world".into(),
            actions: vec![],
            expected_text: "hello world".into(),
            expected_primary: CursorExpect::at(0),
            ..Default::default()
        },
        BufferScenario {
            description: "ToUpperCase on a 5-byte selection at byte 0".into(),
            initial_text: "hello world".into(),
            actions: vec![
                Action::SelectRight,
                Action::SelectRight,
                Action::SelectRight,
                Action::SelectRight,
                Action::SelectRight,
                Action::ToUpperCase,
            ],
            expected_text: "HELLO world".into(),
            expected_primary: CursorExpect::at(5),
            expected_selection_text: Some("".into()),
            ..Default::default()
        },
        BufferScenario {
            description: "MoveDocumentEnd then InsertChar appends".into(),
            initial_text: "ab".into(),
            actions: vec![Action::MoveDocumentEnd, Action::InsertChar('c')],
            expected_text: "abc".into(),
            expected_primary: CursorExpect::at(3),
            ..Default::default()
        },
        BufferScenario {
            description: "DeleteForward at start removes the first char".into(),
            initial_text: "abc".into(),
            actions: vec![Action::DeleteForward],
            expected_text: "bc".into(),
            expected_primary: CursorExpect::at(0),
            ..Default::default()
        },
        BufferScenario {
            description: "InsertNewline splits the buffer at the cursor".into(),
            initial_text: "abc".into(),
            actions: vec![Action::MoveRight, Action::InsertNewline],
            expected_text: "a\nbc".into(),
            expected_primary: CursorExpect::at(2),
            ..Default::default()
        },
        BufferScenario {
            description: "DeleteBackward in the middle removes the preceding char".into(),
            initial_text: "abcd".into(),
            actions: vec![Action::MoveRight, Action::MoveRight, Action::DeleteBackward],
            expected_text: "acd".into(),
            expected_primary: CursorExpect::at(1),
            ..Default::default()
        },
        BufferScenario {
            description: "SelectAll then DeleteForward clears the buffer".into(),
            initial_text: "hello".into(),
            actions: vec![Action::SelectAll, Action::DeleteForward],
            expected_text: "".into(),
            expected_primary: CursorExpect::at(0),
            expected_selection_text: Some("".into()),
            ..Default::default()
        },
        BufferScenario {
            description: "SelectRight x2 then DeleteBackward deletes the selection".into(),
            initial_text: "abcde".into(),
            actions: vec![
                Action::SelectRight,
                Action::SelectRight,
                Action::DeleteBackward,
            ],
            expected_text: "cde".into(),
            expected_primary: CursorExpect::at(0),
            expected_selection_text: Some("".into()),
            ..Default::default()
        },
        BufferScenario {
            description: "MoveDown then MoveLineEnd lands at end of the second line".into(),
            initial_text: "ab\ncde".into(),
            actions: vec![Action::MoveDown, Action::MoveLineEnd],
            expected_text: "ab\ncde".into(),
            expected_primary: CursorExpect::at(6),
            ..Default::default()
        },
        BufferScenario {
            description: "SelectLineEnd selects to the end of the first line".into(),
            initial_text: "hello\nworld".into(),
            actions: vec![Action::SelectLineEnd],
            expected_text: "hello\nworld".into(),
            expected_primary: CursorExpect::range(0, 5),
            expected_selection_text: Some("hello".into()),
            ..Default::default()
        },
        BufferScenario {
            description: "ToLowerCase on a select-all lowercases the buffer".into(),
            initial_text: "HELLO".into(),
            actions: vec![Action::SelectAll, Action::ToLowerCase],
            expected_text: "hello".into(),
            expected_primary: CursorExpect::at(5),
            expected_selection_text: Some("".into()),
            ..Default::default()
        },
        BufferScenario {
            description: "DuplicateLine duplicates the current line".into(),
            initial_text: "abc".into(),
            actions: vec![Action::DuplicateLine],
            expected_text: "abc\nabc".into(),
            // Cursor landing after duplicate is an impl detail; the
            // load-bearing claim is the duplicated text.
            skip_cursor_check: true,
            ..Default::default()
        },
        BufferScenario {
            description: "DeleteBackward removes a full CJK codepoint".into(),
            initial_text: "你好".into(),
            actions: vec![Action::MoveDocumentEnd, Action::DeleteBackward],
            expected_text: "你".into(),
            expected_primary: CursorExpect::at(3),
            ..Default::default()
        },
        BufferScenario {
            description: "InsertChar before a CJK codepoint".into(),
            initial_text: "好".into(),
            actions: vec![Action::InsertChar('a')],
            expected_text: "a好".into(),
            expected_primary: CursorExpect::at(1),
            ..Default::default()
        },
        BufferScenario {
            description: "AddCursorBelow x2 then InsertChar prefixes each line".into(),
            initial_text: "aaa\nbbb\nccc".into(),
            actions: vec![
                Action::AddCursorBelow,
                Action::AddCursorBelow,
                Action::InsertChar('x'),
            ],
            expected_text: "xaaa\nxbbb\nxccc".into(),
            // Surviving-cursor positions after a multi-insert are an
            // impl detail; the load-bearing claim is the text.
            skip_cursor_check: true,
            ..Default::default()
        },
    ]
}
