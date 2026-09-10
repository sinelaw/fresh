//! Track B migration: rewrites of `tests/e2e/duplicate_line.rs` as
//! declarative theorems.
//!
//! The original tests invoke "duplicate line" through the command
//! palette (Ctrl+P → "duplicate line" → Enter). The semantic action
//! `Action::DuplicateLine` exists and bypasses the palette entirely,
//! so the theorem version is dramatically shorter.
//!
//! Issue #591: Duplicate line or selected lines.

use crate::common::harness::EditorTestHarness;
use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, BufferScenario, CursorExpect,
};
use crate::common::scenario::trace_scenario::{assert_trace_scenario, TraceScenario};
use fresh::test_api::Action;
use fresh::test_api::Caret;

#[test]
fn theorem_duplicate_line_with_following_lines() {
    // Replaces tests/e2e/duplicate_line.rs::test_duplicate_line_with_newline.
    // Cursor on line 1 of a 3-line buffer. Only line 1 is duplicated;
    // following lines slide down.
    assert_buffer_scenario(BufferScenario {
        description: "DuplicateLine on line 1 of 3 leaves following lines untouched".into(),
        initial_text: "first\nsecond\nthird".into(),
        actions: vec![Action::MoveDocumentStart, Action::DuplicateLine],
        expected_text: "first\nfirst\nsecond\nthird".into(),
        expected_primary: CursorExpect::at(6),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
        ..Default::default()
    });
}

#[test]
fn theorem_duplicate_selected_lines_duplicates_each_selected_line() {
    // Replaces tests/e2e/duplicate_line.rs::test_duplicate_selected_lines.
    // A selection ending at the next line's start excludes that line,
    // and remains selected on the copied block.
    assert_buffer_scenario(BufferScenario {
        description: "DuplicateLine over a multi-line selection duplicates the selected block"
            .into(),
        initial_text: "line one\nline two\nline three\nline four".into(),
        // Move to start of line 2, then select two lines (down twice with shift).
        actions: vec![
            Action::MoveDocumentStart,
            Action::MoveDown,
            Action::SelectDown,
            Action::SelectDown,
            Action::DuplicateLine,
        ],
        expected_text: "line one\nline two\nline three\nline two\nline three\nline four".into(),
        expected_primary: CursorExpect::range(29, 49),
        expected_extra_cursors: vec![],
        expected_selection_text: Some("line two\nline three\n".into()),
        ..Default::default()
    });
}

#[test]
fn theorem_duplicate_line_then_typing_inserts_into_duplicate() {
    // Replaces tests/e2e/duplicate_line.rs::test_duplicate_line_cursor_on_new_line.
    // Asserts the cursor lands on the *new* (lower) duplicate, so a
    // subsequent insertion appears on that line, not the original.
    assert_buffer_scenario(BufferScenario {
        description: "After DuplicateLine, typing inserts on the duplicate line".into(),
        initial_text: "first\nsecond\nthird".into(),
        actions: vec![
            Action::MoveDocumentStart,
            Action::DuplicateLine,
            Action::InsertChar('X'),
        ],
        expected_text: "first\nXfirst\nsecond\nthird".into(),
        expected_primary: CursorExpect::at(7),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
        ..Default::default()
    });
}

#[test]
fn theorem_duplicate_line_undo_restores_original() {
    // Replaces tests/e2e/duplicate_line.rs::test_duplicate_line_undo.
    // DuplicateLine is a single undo unit — one Undo restores the input.
    assert_trace_scenario(TraceScenario {
        description: "DuplicateLine is one undo unit — Undo restores the input".into(),
        initial_text: "hello world".into(),
        actions: vec![Action::MoveDocumentEnd, Action::DuplicateLine],
        expected_text: "hello world\nhello world".into(),
        undo_count: 1,
    });
}

/// Exercise the native editor/history path, not sequential application of
/// individual events (multi-event actions use one bulk edit in the editor).
fn assert_duplicate_copies(
    initial: &str,
    setup: Vec<Action>,
    expected: &str,
    below: Vec<Caret>,
    above: Vec<Caret>,
) {
    for (action, expected_carets) in [
        (Action::DuplicateLine, below),
        (Action::DuplicateLineAbove, above),
    ] {
        let mut harness = EditorTestHarness::with_temp_project_no_plugins(80, 24).unwrap();
        let _fixture = harness
            .load_buffer_from_text_named("duplicate.txt", initial)
            .unwrap();
        harness.render().unwrap();
        for step in &setup {
            harness.api_mut().dispatch(step.clone());
            harness.render().unwrap();
        }
        let before = harness.api_mut().carets();
        let selected = harness.api_mut().selection_text();
        harness.api_mut().dispatch(action.clone());
        assert_eq!(harness.api_mut().buffer_text(), expected, "{action:?}");
        assert_eq!(harness.api_mut().carets(), expected_carets, "{action:?}");
        assert_eq!(harness.api_mut().selection_text(), selected, "{action:?}");
        for caret in &expected_carets {
            assert!(expected.is_char_boundary(caret.position));
            if let Some(anchor) = caret.anchor {
                assert!(expected.is_char_boundary(anchor));
            }
        }
        harness.api_mut().dispatch(Action::Undo);
        assert_eq!(harness.api_mut().buffer_text(), initial, "{action:?} undo");
        assert_eq!(harness.api_mut().carets(), before, "{action:?} undo");
        harness.api_mut().dispatch(Action::Redo);
        assert_eq!(harness.api_mut().buffer_text(), expected, "{action:?} redo");
        assert_eq!(
            harness.api_mut().carets(),
            expected_carets,
            "{action:?} redo"
        );
    }
}

#[test]
fn duplicate_copies_keep_interior_caret_offsets() {
    assert_duplicate_copies(
        "hello\nworld\n",
        vec![Action::MoveRight, Action::MoveRight],
        "hello\nhello\nworld\n",
        vec![Caret::at(8)],
        vec![Caret::at(2)],
    );
    assert_duplicate_copies(
        "foo\nfoo\n",
        vec![Action::AddCursorBelow, Action::MoveRight, Action::MoveRight],
        "foo\nfoo\nfoo\nfoo\n",
        vec![Caret::at(6), Caret::at(14)],
        vec![Caret::at(2), Caret::at(10)],
    );
}

#[test]
fn duplicate_copies_keep_both_selection_orientations() {
    for reverse in [false, true] {
        let mut setup = Vec::new();
        if reverse {
            setup.extend([Action::MoveRight, Action::MoveRight, Action::MoveRight]);
        }
        setup.extend(std::iter::repeat_n(
            if reverse {
                Action::SelectLeft
            } else {
                Action::SelectRight
            },
            3,
        ));
        setup.push(Action::AddCursorNextMatch);
        let caret = |start, end| {
            if reverse {
                Caret::range(end, start)
            } else {
                Caret::range(start, end)
            }
        };
        assert_duplicate_copies(
            "foo\nfoo\n",
            setup,
            "foo\nfoo\nfoo\nfoo\n",
            vec![caret(4, 7), caret(12, 15)],
            vec![caret(0, 3), caret(8, 11)],
        );
    }
}

#[test]
fn duplicate_copies_preserve_utf8_crlf_and_final_separator() {
    assert_duplicate_copies(
        "é猫\r\nz",
        vec![Action::MoveRight, Action::SelectRight],
        "é猫\r\né猫\r\nz",
        vec![Caret::range(9, 12)],
        vec![Caret::range(2, 5)],
    );
    assert_duplicate_copies(
        "first\r\né猫",
        vec![Action::MoveDocumentEnd],
        "first\r\né猫\r\né猫",
        vec![Caret::at(19)],
        vec![Caret::at(12)],
    );
    assert_duplicate_copies(
        "hello",
        vec![Action::MoveDocumentEnd],
        "hello\nhello",
        vec![Caret::at(11)],
        vec![Caret::at(5)],
    );
}

#[test]
fn duplicate_copies_exclude_unselected_next_line() {
    for reverse in [false, true] {
        let setup = if reverse {
            vec![Action::MoveDown, Action::SelectUp]
        } else {
            vec![Action::SelectDown]
        };
        assert_duplicate_copies(
            "foo\nbar\n",
            setup,
            "foo\nfoo\nbar\n",
            vec![if reverse {
                Caret::range(8, 4)
            } else {
                Caret::range(4, 8)
            }],
            vec![if reverse {
                Caret::range(4, 0)
            } else {
                Caret::range(0, 4)
            }],
        );
    }
}

#[test]
fn duplicate_copies_on_shared_line_keep_distinct_selected_copies() {
    for action in [Action::DuplicateLine, Action::DuplicateLineAbove] {
        let mut harness = EditorTestHarness::with_temp_project_no_plugins(80, 24).unwrap();
        let _fixture = harness
            .load_buffer_from_text_named("duplicate.txt", "foo foo\n")
            .unwrap();
        harness.render().unwrap();
        harness.api_mut().dispatch(Action::AddCursorNextMatch);
        harness.api_mut().dispatch(Action::AddCursorNextMatch);
        let original = harness.api_mut().carets();
        let above = matches!(action, Action::DuplicateLineAbove);
        harness.api_mut().dispatch(action);
        assert_eq!(
            harness.api_mut().buffer_text(),
            "foo foo\nfoo foo\nfoo foo\n"
        );
        let copied = harness.api_mut().carets();
        assert_eq!(copied.len(), 2);
        assert_eq!(harness.api_mut().selection_text(), "foo\nfoo");
        // Each selected occurrence keeps its column on a separate copy.
        // Copy ordering at a shared insertion point is not a user contract.
        let first_copy = if above { 0 } else { 8 };
        let mut offsets: Vec<_> = copied.iter().map(|caret| caret.position % 8).collect();
        offsets.sort_unstable();
        assert_eq!(offsets, vec![3, 7]);
        assert!(copied[0].position >= first_copy && copied[0].position < first_copy + 8);
        assert!(copied[1].position >= first_copy + 8 && copied[1].position < first_copy + 16);
        harness.api_mut().dispatch(Action::Undo);
        assert_eq!(harness.api_mut().buffer_text(), "foo foo\n");
        assert_eq!(harness.api_mut().carets(), original);
        harness.api_mut().dispatch(Action::Redo);
        assert_eq!(
            harness.api_mut().buffer_text(),
            "foo foo\nfoo foo\nfoo foo\n"
        );
        assert_eq!(harness.api_mut().carets(), copied);
    }
}

#[test]
fn duplicate_copies_do_not_absorb_neighboring_edits() {
    for action in [Action::DuplicateLine, Action::DuplicateLineAbove] {
        let mut harness = EditorTestHarness::with_temp_project_no_plugins(80, 24).unwrap();
        let _fixture = harness
            .load_buffer_from_text_named("duplicate.txt", "foo\nfoo\n")
            .unwrap();
        harness.render().unwrap();
        harness.api_mut().dispatch(Action::AddCursorBelow);
        harness.render().unwrap();
        harness.api_mut().dispatch(Action::InsertChar('X'));
        let before = harness.api_mut().buffer_text();
        harness.api_mut().dispatch(action);
        let copied = harness.api_mut().buffer_text();
        let carets = harness.api_mut().carets();
        harness.api_mut().dispatch(Action::InsertChar('Z'));
        harness.api_mut().dispatch(Action::Undo);
        assert_eq!(harness.api_mut().buffer_text(), copied);
        harness.api_mut().dispatch(Action::Undo);
        assert_eq!(harness.api_mut().buffer_text(), before);
        harness.api_mut().dispatch(Action::Undo);
        assert_eq!(harness.api_mut().buffer_text(), "foo\nfoo\n");
        harness.api_mut().dispatch(Action::Redo);
        assert_eq!(harness.api_mut().buffer_text(), before);
        harness.api_mut().dispatch(Action::Redo);
        assert_eq!(harness.api_mut().buffer_text(), copied);
        assert_eq!(harness.api_mut().carets(), carets);
    }
}

#[test]
fn duplicate_copies_preserve_overlapping_line_blocks() {
    // Disjoint selections "\nx" span overlapping full-line copy ranges.
    assert_duplicate_copies(
        "x\nx\nx\n",
        vec![
            Action::MoveRight,
            Action::SelectRight,
            Action::SelectRight,
            Action::AddCursorNextMatch,
        ],
        "x\nx\nx\nx\nx\nx\nx\n",
        vec![Caret::range(5, 7), Caret::range(11, 13)],
        vec![Caret::range(1, 3), Caret::range(7, 9)],
    );
}
