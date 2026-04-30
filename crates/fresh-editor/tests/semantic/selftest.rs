//! Meta-tests: prove that the theorem runners actually fail when the
//! expectations are wrong.
//!
//! Each test in this file is paired with one of the "positive" theorems
//! elsewhere under `tests/semantic/`. The positive version asserts a
//! correct expectation and passes. The version here corrupts exactly
//! one field of the same theorem and confirms the runner panics with
//! the message that field is supposed to produce.
//!
//! Without these, a silently-broken assertion (e.g., `assert_eq!`
//! accidentally comparing two clones of the same value) would let
//! every theorem appear to pass for the wrong reason. Every runner
//! assertion path should have a `#[should_panic]` twin in this file.
//!
//! The expected-substring is matched as a prefix of the panic
//! message; the format strings in the runners are stable.

use crate::common::theorem::buffer_theorem::{assert_buffer_theorem, BufferTheorem, CursorExpect};
use crate::common::theorem::layout_theorem::{assert_layout_theorem, LayoutTheorem};
use crate::common::theorem::trace_theorem::{assert_trace_theorem, TraceTheorem};
use fresh::test_api::Action;

// ─────────────────────────────────────────────────────────────────────────
// BufferTheorem — one negative twin per assertion path.
// ─────────────────────────────────────────────────────────────────────────

/// Reference: `case_conversion::theorem_to_uppercase_selection` (positive).
/// Confirms the *positive* shape passes when re-run from this module —
/// that proves the negative twins below differ from the positive only
/// in the single corrupted expectation.
#[test]
fn metatheorem_correct_buffer_theorem_passes() {
    assert_buffer_theorem(BufferTheorem {
        description: "control",
        initial_text: "hello world",
        actions: vec![
            Action::SelectRight,
            Action::SelectRight,
            Action::SelectRight,
            Action::SelectRight,
            Action::SelectRight,
            Action::ToUpperCase,
        ],
        expected_text: "HELLO world",
        expected_primary: CursorExpect::at(5),
        expected_extra_cursors: vec![],
        expected_selection_text: Some(""),
    });
}

#[test]
#[should_panic(expected = "buffer text mismatch")]
fn metatheorem_wrong_expected_text_panics() {
    assert_buffer_theorem(BufferTheorem {
        description: "should_panic: wrong text",
        initial_text: "hello world",
        actions: vec![Action::ToUpperCase],
        expected_text: "DEFINITELY WRONG",
        expected_primary: CursorExpect::at(0),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
    });
}

#[test]
#[should_panic(expected = "primary cursor mismatch")]
fn metatheorem_wrong_primary_cursor_panics() {
    // Correct end state: cursor at 5 with no selection (after collapse).
    // We claim the cursor sits at 999, which is impossible.
    assert_buffer_theorem(BufferTheorem {
        description: "should_panic: wrong primary",
        initial_text: "hello world",
        actions: vec![
            Action::SelectRight,
            Action::SelectRight,
            Action::SelectRight,
            Action::SelectRight,
            Action::SelectRight,
            Action::ToUpperCase,
        ],
        expected_text: "HELLO world",
        expected_primary: CursorExpect::at(999),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
    });
}

#[test]
#[should_panic(expected = "cursor count mismatch")]
fn metatheorem_wrong_cursor_count_panics() {
    // Single-cursor reality, but we claim two extras → 3 total expected.
    assert_buffer_theorem(BufferTheorem {
        description: "should_panic: wrong count",
        initial_text: "hello",
        actions: vec![],
        expected_text: "hello",
        expected_primary: CursorExpect::at(0),
        expected_extra_cursors: vec![CursorExpect::at(1), CursorExpect::at(2)],
        expected_selection_text: None,
    });
}

#[test]
#[should_panic(expected = "selection text mismatch")]
fn metatheorem_wrong_selection_text_panics() {
    // SelectRight ×3 from byte 0 selects "hel"; we claim "xxx".
    assert_buffer_theorem(BufferTheorem {
        description: "should_panic: wrong selection text",
        initial_text: "hello world",
        actions: vec![
            Action::SelectRight,
            Action::SelectRight,
            Action::SelectRight,
        ],
        expected_text: "hello world",
        expected_primary: CursorExpect::range(0, 3),
        expected_extra_cursors: vec![],
        expected_selection_text: Some("xxx"),
    });
}

// ─────────────────────────────────────────────────────────────────────────
// TraceTheorem — one twin per assertion path (forward + reverse).
// ─────────────────────────────────────────────────────────────────────────

#[test]
#[should_panic(expected = "forward trace failed")]
fn metatheorem_trace_wrong_forward_panics() {
    assert_trace_theorem(TraceTheorem {
        description: "should_panic: wrong forward",
        initial_text: "abc",
        actions: vec![Action::InsertChar('Z')],
        expected_text: "this is not what InsertChar('Z') produces",
        undo_count: 1,
    });
}

#[test]
#[should_panic(expected = "reverse trace failed")]
fn metatheorem_trace_wrong_undo_count_panics() {
    // Forward trace inserts 3 chars (3 undo units). Claiming undo_count
    // = 1 leaves the buffer with 2 chars still inserted, so reverse
    // trace fails its equality.
    assert_trace_theorem(TraceTheorem {
        description: "should_panic: too few undos",
        initial_text: "",
        actions: vec![
            Action::InsertChar('a'),
            Action::InsertChar('b'),
            Action::InsertChar('c'),
        ],
        expected_text: "abc",
        undo_count: 1,
    });
}

// ─────────────────────────────────────────────────────────────────────────
// LayoutTheorem — twin for the single layout assertion path.
// ─────────────────────────────────────────────────────────────────────────

#[test]
#[should_panic(expected = "viewport_top_byte mismatch")]
fn metatheorem_layout_wrong_top_byte_panics() {
    assert_layout_theorem(LayoutTheorem {
        description: "should_panic: wrong top_byte",
        initial_text: "alpha\nbravo\n",
        width: 80,
        height: 24,
        actions: vec![],
        expected_top_byte: 999_999,
    });
}

// ─────────────────────────────────────────────────────────────────────────
// External-driver shape: check_* returns typed Result without panic.
//
// These tests don't use #[should_panic]. They invoke the fallible
// runners directly and pattern-match on the returned TheoremFailure.
// This is the entry point an external prover/fuzzer would call: each
// theorem evaluation is a function call, never a stack unwind.
// ─────────────────────────────────────────────────────────────────────────

use crate::common::theorem::buffer_theorem::check_buffer_theorem;
use crate::common::theorem::failure::TheoremFailure;
use crate::common::theorem::layout_theorem::check_layout_theorem;
use crate::common::theorem::trace_theorem::check_trace_theorem;

#[test]
fn metatheorem_check_returns_ok_on_correct_theorem() {
    let result = check_buffer_theorem(BufferTheorem {
        description: "passes via check_*",
        initial_text: "hi",
        actions: vec![],
        expected_text: "hi",
        expected_primary: CursorExpect::at(0),
        expected_extra_cursors: vec![],
        expected_selection_text: Some(""),
    });
    assert!(result.is_ok(), "expected Ok, got {result:?}");
}

#[test]
fn metatheorem_check_returns_typed_buffer_text_failure() {
    let result = check_buffer_theorem(BufferTheorem {
        description: "wrong via check_*",
        initial_text: "hi",
        actions: vec![],
        expected_text: "WRONG",
        expected_primary: CursorExpect::at(0),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
    });
    match result {
        Err(TheoremFailure::BufferTextMismatch {
            ref expected,
            ref actual,
            ..
        }) => {
            assert_eq!(expected, "WRONG");
            assert_eq!(actual, "hi");
        }
        other => panic!("expected BufferTextMismatch, got {other:?}"),
    }
}

#[test]
fn metatheorem_check_returns_typed_primary_cursor_failure() {
    let result = check_buffer_theorem(BufferTheorem {
        description: "wrong primary via check_*",
        initial_text: "abc",
        actions: vec![],
        expected_text: "abc",
        expected_primary: CursorExpect::at(42),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
    });
    assert!(matches!(
        result,
        Err(TheoremFailure::PrimaryCursorMismatch { .. })
    ));
}

#[test]
fn metatheorem_check_returns_typed_forward_trace_failure() {
    let result = check_trace_theorem(TraceTheorem {
        description: "wrong forward via check_*",
        initial_text: "",
        actions: vec![Action::InsertChar('x')],
        expected_text: "WRONG",
        undo_count: 1,
    });
    assert!(matches!(
        result,
        Err(TheoremFailure::ForwardTraceFailed { .. })
    ));
}

#[test]
fn metatheorem_check_returns_typed_layout_failure() {
    let result = check_layout_theorem(LayoutTheorem {
        description: "wrong top_byte via check_*",
        initial_text: "x",
        width: 80,
        height: 24,
        actions: vec![],
        expected_top_byte: 999,
    });
    assert!(matches!(
        result,
        Err(TheoremFailure::ViewportTopByteMismatch {
            expected: 999,
            actual: 0,
            ..
        })
    ));
}

#[test]
fn metatheorem_check_can_be_called_in_a_loop_without_panic() {
    // The defining use case: an external driver calls check_* on
    // generated theorems and decides what to mutate next based on the
    // typed failure. This loop must terminate normally even when most
    // theorems fail — proving the panic-free contract.
    let cases = [
        ("hi", "hi", true),        // pass
        ("hi", "WRONG", false),    // fail
        ("hello", "HELLO", false), // fail
        ("hello", "hello", true),  // pass
    ];

    let mut pass_count = 0;
    let mut fail_count = 0;
    for (initial, expected, _should_pass) in cases {
        let r = check_buffer_theorem(BufferTheorem {
            description: "loop case",
            initial_text: initial,
            actions: vec![],
            expected_text: expected,
            expected_primary: CursorExpect::at(0),
            expected_extra_cursors: vec![],
            expected_selection_text: None,
        });
        match r {
            Ok(_) => pass_count += 1,
            Err(_) => fail_count += 1,
        }
    }
    assert_eq!(pass_count, 2);
    assert_eq!(fail_count, 2);
}
