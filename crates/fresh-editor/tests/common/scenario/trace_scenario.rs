//! `TraceScenario` — denotational reversibility (undo trace).
//!
//! A `TraceScenario` declares: "after applying these actions, the
//! buffer reaches `expected_text`; *and* applying `undo_count` undo
//! actions restores the initial buffer text exactly."
//!
//! This is the algebraic claim that the action sequence's trace and
//! the undo trace are inverses on buffer text. It's the right shape
//! for tests of multi-cursor undo atomicity, bulk edits, and any
//! operation whose correctness depends on transactional grouping.
//!
//! Cursor-state restoration is intentionally NOT asserted here — undo
//! does not generally restore cursor state byte-for-byte (the redo
//! algorithm replays MoveCursor events). Tests that care about
//! post-undo cursor state should use [`BufferScenario`] directly.
//!
//! [`BufferScenario`]: crate::common::scenario::buffer_scenario::BufferScenario

use crate::common::harness::EditorTestHarness;
use crate::common::scenario::failure::ScenarioFailure;
use fresh::test_api::Action;

#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct TraceScenario {
    pub description: String,
    pub initial_text: String,
    pub actions: Vec<Action>,
    pub expected_text: String,
    /// Number of `Action::Undo` invocations to issue. Set this to the
    /// number of *editing* actions in the forward trace; an "atomic"
    /// transaction of N keystrokes typically takes N undos to fully
    /// roll back.
    pub undo_count: usize,
}

pub fn check_trace_scenario(s: TraceScenario) -> Result<(), ScenarioFailure> {
    let mut timer =
        crate::common::timing::Timer::start(format!("trace_scenario: {}", s.description));
    // TraceScenario asserts only on buffer text after a forward + undo
    // trace, both dispatched through core `Action`s. No observable
    // surface reaches plugin state, so we skip plugin loading. See
    // `EditorTestHarness::with_temp_project_no_plugins`.
    let mut harness = EditorTestHarness::with_temp_project_no_plugins(80, 24)
        .expect("EditorTestHarness::with_temp_project_no_plugins failed");
    timer.phase("harness_create");
    let _fixture = harness
        .load_buffer_from_text(&s.initial_text)
        .expect("load_buffer_from_text failed");
    timer.phase("load_buffer");

    let api = harness.api_mut();

    // Forward trace.
    api.dispatch_seq(&s.actions);
    timer.phase("forward_dispatch");
    let after_forward = api.buffer_text();
    if after_forward != s.expected_text {
        return Err(ScenarioFailure::ForwardTraceFailed {
            description: s.description,
            expected: s.expected_text,
            actual: after_forward,
        });
    }

    // Reverse trace.
    let undo_actions: Vec<Action> = (0..s.undo_count).map(|_| Action::Undo).collect();
    api.dispatch_seq(&undo_actions);
    timer.phase("undo_dispatch");
    let after_reverse = api.buffer_text();
    if after_reverse != s.initial_text {
        return Err(ScenarioFailure::ReverseTraceFailed {
            description: s.description,
            undo_count: s.undo_count,
            expected: s.initial_text,
            actual: after_reverse,
        });
    }

    drop(harness);
    timer.phase("harness_drop");
    timer.finish();
    Ok(())
}

pub fn assert_trace_scenario(s: TraceScenario) {
    if let Err(f) = check_trace_scenario(s) {
        panic!("{f}");
    }
}
