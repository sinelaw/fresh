//! Class A.2 theorems: denotational reversibility (undo trace).
//!
//! A `TraceTheorem` declares: "after applying these actions, the buffer
//! reaches `expected_text`; *and* applying `undo_count` undo actions
//! restores the initial buffer text exactly."
//!
//! This is the algebraic claim that the action sequence's trace and the
//! undo trace are inverses on buffer text. It's the right shape for
//! tests of multi-cursor undo atomicity, bulk edits, and any operation
//! whose correctness depends on transactional grouping.
//!
//! Cursor-state restoration is intentionally NOT asserted here — undo
//! does not generally restore cursor state byte-for-byte (the redo
//! algorithm replays MoveCursor events). The harness's existing
//! shadow-validation logic explicitly resets cursors on undo for the
//! same reason (see `tests/common/harness.rs:1003-1015`). Tests that
//! care about post-undo cursor state should use `BufferTheorem`
//! directly.

use crate::common::harness::EditorTestHarness;
use crate::common::theorem::failure::TheoremFailure;
use fresh::test_api::Action;

pub struct TraceTheorem {
    pub description: &'static str,
    pub initial_text: &'static str,
    pub actions: Vec<Action>,
    pub expected_text: &'static str,
    /// Number of `Action::Undo` invocations to issue. Set this to the
    /// number of *editing* actions in the forward trace; an "atomic"
    /// transaction of N keystrokes typically takes N undos to fully
    /// roll back.
    pub undo_count: usize,
}

pub fn check_trace_theorem(t: TraceTheorem) -> Result<(), TheoremFailure> {
    let mut harness = EditorTestHarness::with_temp_project(80, 24)
        .expect("EditorTestHarness::with_temp_project failed");
    let _fixture = harness
        .load_buffer_from_text(t.initial_text)
        .expect("load_buffer_from_text failed");

    let api = harness.api_mut();

    // Forward trace.
    api.dispatch_seq(&t.actions);
    let after_forward = api.buffer_text();
    if after_forward != t.expected_text {
        return Err(TheoremFailure::ForwardTraceFailed {
            description: t.description.to_string(),
            expected: t.expected_text.to_string(),
            actual: after_forward,
        });
    }

    // Reverse trace.
    let undo_actions: Vec<Action> = (0..t.undo_count).map(|_| Action::Undo).collect();
    api.dispatch_seq(&undo_actions);
    let after_reverse = api.buffer_text();
    if after_reverse != t.initial_text {
        return Err(TheoremFailure::ReverseTraceFailed {
            description: t.description.to_string(),
            undo_count: t.undo_count,
            expected: t.initial_text.to_string(),
            actual: after_reverse,
        });
    }

    Ok(())
}

pub fn assert_trace_theorem(t: TraceTheorem) {
    if let Err(f) = check_trace_theorem(t) {
        panic!("{f}");
    }
}
