//! Bug repros found by the semantic test framework, captured as
//! standalone regression tests.
//!
//! Each `#[ignore]`d test in this file has a clear `expected =`
//! reason pointing at the production bug. When that bug is fixed,
//! the developer removes the `#[ignore]` (and the corresponding
//! `#[ignore]` on any property test that surfaces it). If the test
//! still passes, great — the fix worked and we now have permanent
//! coverage. If the test panics, the fix was incomplete.
//!
//! Discovery context: each bug was first found by a proptest
//! property under `properties.rs`. Proptest's shrinking produced
//! the minimal action sequence captured here.

use crate::common::theorem::buffer_theorem::{check_buffer_theorem, BufferTheorem, CursorExpect};
use fresh::test_api::Action;

/// Production bug: `crates/fresh-editor/src/input/actions.rs:1613`
/// panics with `index out of bounds: the len is 0 but the index is N`
/// when DeleteBackward executes smart-dedent on a "phantom line"
/// reached by MoveDown past the last line of a buffer whose first
/// line is whitespace-only.
///
/// Discovered by `property_arbitrary_actions_do_not_panic` in 70s of
/// fuzzing. Proptest shrunk to 4 actions on a 4-byte buffer.
///
/// Diagnosis (educated guess from src/input/actions.rs:1600-1623):
/// `state.buffer.line_iterator(cursor.position, …).current_position()`
/// returns a value that is no longer in sync with the buffer after
/// the cursor moves past EOF; `prefix_len = cursor.position -
/// line_start` is computed as positive even though
/// `slice_bytes(line_start..cursor.position)` returns an empty Vec.
/// Then `prefix_bytes[prefix_len - 1]` indexes beyond the slice.
#[test]
#[ignore = "BUG: actions.rs:1613 — smart-dedent panics on phantom line"]
fn regression_smart_dedent_panic_on_phantom_line() {
    // The exact shrunk repro from
    // tests/semantic/properties.proptest-regressions.
    // We don't claim a specific expected_text — just that the
    // dispatch returns *at all* without unwinding the stack.
    let result = check_buffer_theorem(BufferTheorem {
        description: "shrunk repro: MoveDown past EOF then DeleteBackward",
        initial_text: "   \n",
        actions: vec![
            Action::SelectLineEnd,
            Action::InsertChar(' '),
            Action::MoveDown,
            Action::DeleteBackward,
        ],
        // Whatever the correct end state is, it shouldn't panic. We
        // pick a probable one; if the eventual fix produces a
        // different text the developer updates the expectation.
        expected_text: "    \n",
        expected_primary: CursorExpect::at(0),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
    });
    // Either Ok(()) or a TheoremFailure is acceptable — both prove
    // the panic is gone. Only an actual panic (which is what we're
    // tracking) would short-circuit this and never reach the assert.
    assert!(result.is_ok() || result.is_err(), "should not panic");
}
