//! Combination meta-test — see `docs/internal/scenario-meta-testing.md`
//! ("3. Combination with active reset").
//!
//! Several full `BufferScenario`s run on ONE long-lived harness with an
//! *active reset* (`reset::reset_actions`, not a fresh harness) between
//! them. Each scenario asserts its **own** expectations at its
//! checkpoint — right after its actions, before the next reset — in
//! every permutation of the order. A scenario that passes alone but
//! fails after some predecessor means either the reset is incomplete
//! or the scenario depends on ambient state a predecessor left behind:
//! bugs the fresh-harness-per-test model can never surface.
//!
//! Scope: buffer-layer scenarios (text/cursor/selection), `actions`
//! only (no `events`). Undo log, modified flag, config, markers and
//! clipboard are out of reset's reach, so workloads avoid them.

use crate::common::scenario::buffer_scenario::{BufferScenario, CursorExpect};
use crate::common::scenario::property::run_scenarios_with_reset_between;
use fresh::test_api::Action;

fn workloads() -> Vec<BufferScenario> {
    use Action::*;
    vec![
        BufferScenario {
            description: "append ! at end of line".into(),
            initial_text: "hello world".into(),
            actions: vec![MoveDocumentEnd, InsertChar('!')],
            expected_text: "hello world!".into(),
            expected_primary: CursorExpect::at(12),
            expected_selection_text: Some(String::new()),
            ..Default::default()
        },
        BufferScenario {
            description: "newline + d at EOF".into(),
            initial_text: "a\nb\nc".into(),
            actions: vec![MoveDocumentEnd, InsertNewline, InsertChar('d')],
            expected_text: "a\nb\nc\nd".into(),
            expected_primary: CursorExpect::at(7),
            expected_selection_text: Some(String::new()),
            ..Default::default()
        },
        BufferScenario {
            description: "select last two chars".into(),
            initial_text: "abcde".into(),
            actions: vec![MoveDocumentEnd, SelectLeft, SelectLeft],
            expected_text: "abcde".into(),
            expected_primary: CursorExpect::range(5, 3),
            expected_selection_text: Some("de".into()),
            ..Default::default()
        },
        BufferScenario {
            description: "three-cursor prefix insert".into(),
            initial_text: "aaa\nbbb\nccc".into(),
            actions: vec![AddCursorBelow, AddCursorBelow, InsertChar('x')],
            expected_text: "xaaa\nxbbb\nxccc".into(),
            // Surviving-cursor positions after a multi-insert are an
            // impl detail; the load-bearing claim is the text.
            skip_cursor_check: true,
            ..Default::default()
        },
        BufferScenario {
            description: "select last CJK grapheme".into(),
            initial_text: "你好世界".into(),
            actions: vec![MoveDocumentEnd, SelectLeft],
            expected_text: "你好世界".into(),
            expected_primary: CursorExpect::range(12, 9),
            expected_selection_text: Some("界".into()),
            ..Default::default()
        },
        BufferScenario {
            description: "type into empty buffer".into(),
            initial_text: String::new(),
            actions: vec![InsertChar('z'), InsertChar('y')],
            expected_text: "zy".into(),
            expected_primary: CursorExpect::at(2),
            expected_selection_text: Some(String::new()),
            ..Default::default()
        },
    ]
}

/// Representative orders: forward, reverse, and a rotation. Each must
/// satisfy every scenario's own assertions.
fn orders(n: usize) -> Vec<Vec<usize>> {
    let fwd: Vec<usize> = (0..n).collect();
    let rev: Vec<usize> = (0..n).rev().collect();
    let rot: Vec<usize> = (0..n).map(|i| (i + n / 2) % n).collect();
    vec![fwd, rev, rot]
}

#[test]
fn combination_workloads_pass_their_own_assertions_under_reset() {
    let scenarios = workloads();
    for order in orders(scenarios.len()) {
        let results = run_scenarios_with_reset_between(&scenarios, &order);
        for (slot, &i) in order.iter().enumerate() {
            if let Err(f) = &results[slot] {
                panic!(
                    "workload {i} ({:?}) failed its own assertion under order {order:?}: {f}",
                    scenarios[i].description
                );
            }
        }
    }
}
