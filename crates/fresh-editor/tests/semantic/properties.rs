//! Property-based theorems.
//!
//! These tests use proptest to generate `Vec<Action>` and check
//! invariants that should hold for *any* such sequence:
//!   - dispatch is deterministic (same input → same output),
//!   - insert-only sequences are perfectly undoable,
//!   - the action handler never panics on a syntactically valid
//!     action stream.
//!
//! When a generated case fails, proptest shrinks the action list to
//! a minimal counterexample. Because the runners return
//! `Result<(), TheoremFailure>` and `evaluate_actions` doesn't panic
//! on failure, shrinking works without `catch_unwind`.
//!
//! Property failures are saved to
//! `tests/property_theorem.proptest-regressions` and replayed on
//! subsequent runs (proptest's standard regression-tracking).

use crate::common::theorem::buffer_theorem::{check_buffer_theorem, BufferTheorem, CursorExpect};
use crate::common::theorem::failure::TheoremFailure;
use crate::common::theorem::property::{
    evaluate_actions, initial_text_strategy, insert_only_action_strategy, safe_action_strategy,
};
use fresh::test_api::Action;
use proptest::prelude::*;

proptest! {
    // Cap at 32 cases: each evaluation spins up a fresh harness with
    // a tempdir and a Buffer, so the per-case cost is real.
    #![proptest_config(ProptestConfig {
        cases: 32,
        max_shrink_iters: 256,
        .. ProptestConfig::default()
    })]

    /// Deterministic dispatch.
    ///
    /// Running the same (initial_text, actions) twice must produce
    /// the same end state. A failure here would mean the editor
    /// carries state across buffer instantiations, which is a
    /// real test-isolation bug.
    #[test]
    fn property_dispatch_is_deterministic(
        initial_text in initial_text_strategy(),
        actions in prop::collection::vec(safe_action_strategy(), 0..16),
    ) {
        let a = evaluate_actions(&initial_text, &actions);
        let b = evaluate_actions(&initial_text, &actions);
        prop_assert_eq!(a, b);
    }

    /// Insert-only undo identity.
    ///
    /// For an insert-only action sequence of length N, dispatching
    /// N Undo actions afterward must restore the initial buffer
    /// text exactly. This is the algebraic claim "every typed
    /// character is its own undo unit".
    ///
    /// If this property ever fails, shrinking produces the smallest
    /// insert sequence that doesn't undo cleanly — almost certainly
    /// pointing at a transaction-boundary bug.
    #[test]
    fn property_insert_only_undo_is_identity(
        initial_text in initial_text_strategy(),
        actions in prop::collection::vec(insert_only_action_strategy(), 0..12),
    ) {
        // Borrow the initial_text long enough for the leak workaround
        // not to be needed. We run the check by hand instead of
        // through TraceTheorem (which takes &'static str).
        let mut all_actions = actions.clone();
        all_actions.extend((0..actions.len()).map(|_| Action::Undo));

        let final_state = evaluate_actions(&initial_text, &all_actions);
        prop_assert_eq!(
            final_state.buffer_text,
            initial_text,
            "Undo identity violated: {} inserts followed by {} undos did not restore initial text",
            actions.len(),
            actions.len(),
        );
    }

    /// Robustness: arbitrary safe-action sequences don't panic.
    ///
    /// Calls evaluate_actions with up to 24 safe actions. We don't
    /// assert anything about the result — the property is just
    /// "this returns normally". A failure here means the editor
    /// panicked on a path the property generator reached, which is
    /// a real bug regardless of intended behavior.
    ///
    /// **Currently #[ignore]d**: the first run of this property
    /// (proptest seed cc f8324...) found a real production bug.
    /// Repro committed as `regression_smart_dedent_panic_on_phantom_line`
    /// in `tests/semantic/regressions.rs`. Re-enable this property
    /// (and the regression test) after that bug is fixed.
    #[test]
    #[ignore = "FOUND BUG: actions.rs:1613 panics on smart-dedent over phantom line. \
                See regressions::regression_smart_dedent_panic_on_phantom_line."]
    fn property_arbitrary_actions_do_not_panic(
        initial_text in initial_text_strategy(),
        actions in prop::collection::vec(safe_action_strategy(), 0..24),
    ) {
        let _ = evaluate_actions(&initial_text, &actions);
    }
}

// ─────────────────────────────────────────────────────────────────────────
// External-driver demonstration: run check_buffer_theorem in a loop
// over a hand-crafted batch and confirm pass/fail counts. This is what
// a fuzzer or proof-search loop would look like, in miniature.
// ─────────────────────────────────────────────────────────────────────────

#[test]
fn property_check_runner_drives_a_batch_without_panic() {
    let cases: Vec<(BufferTheorem, bool /* expect ok */)> = vec![
        (
            BufferTheorem {
                description: "case 1: identity",
                initial_text: "hello",
                actions: vec![],
                expected_text: "hello",
                expected_primary: CursorExpect::at(0),
                expected_extra_cursors: vec![],
                expected_selection_text: None,
            },
            true,
        ),
        (
            BufferTheorem {
                description: "case 2: typo in expected",
                initial_text: "hello",
                actions: vec![],
                expected_text: "WRONG",
                expected_primary: CursorExpect::at(0),
                expected_extra_cursors: vec![],
                expected_selection_text: None,
            },
            false,
        ),
        (
            BufferTheorem {
                description: "case 3: insert + correct end state",
                initial_text: "ab",
                actions: vec![Action::MoveDocumentEnd, Action::InsertChar('c')],
                expected_text: "abc",
                expected_primary: CursorExpect::at(3),
                expected_extra_cursors: vec![],
                expected_selection_text: None,
            },
            true,
        ),
    ];

    let mut report: Vec<(&str, Result<(), TheoremFailure>)> = Vec::new();
    for (theorem, _) in &cases {
        // Note: BufferTheorem fields are &'static str so we clone
        // each one explicitly. In a real driver these would be
        // generated owned strings against a different runner.
        let cloned = BufferTheorem {
            description: theorem.description,
            initial_text: theorem.initial_text,
            actions: theorem.actions.clone(),
            expected_text: theorem.expected_text,
            expected_primary: theorem.expected_primary,
            expected_extra_cursors: theorem.expected_extra_cursors.clone(),
            expected_selection_text: theorem.expected_selection_text,
        };
        report.push((theorem.description, check_buffer_theorem(cloned)));
    }

    // Confirm pass/fail counts match our predictions.
    for ((_, expected_ok), (desc, result)) in cases.iter().zip(report.iter()) {
        assert_eq!(
            result.is_ok(),
            *expected_ok,
            "{desc}: expected_ok={expected_ok}, got result={result:?}",
        );
    }
}
