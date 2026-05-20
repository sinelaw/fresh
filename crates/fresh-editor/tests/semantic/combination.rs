//! Combination meta-test — see `docs/internal/scenario-meta-testing.md`
//! ("3. Combination with active reset").
//!
//! Several buffer workloads run on ONE long-lived harness with an
//! *active reset* (`reset::reset_actions`, not a fresh harness)
//! between them. Each workload's resulting observable must equal its
//! fresh-harness baseline, in every permutation of the order. A
//! mismatch means either the reset is incomplete or a workload
//! secretly depends on ambient state left by a predecessor — bugs the
//! fresh-harness-per-test model can never surface.
//!
//! Scope: buffer-layer workloads (text/cursor/selection). Undo log,
//! modified flag, config, markers and clipboard are out of reset's
//! reach, so workloads here must not depend on them.

use crate::common::scenario::property::{evaluate_actions, run_with_reset_between};
use fresh::test_api::Action;

fn workloads() -> Vec<(&'static str, Vec<Action>)> {
    use Action::*;
    vec![
        ("hello world", vec![MoveDocumentEnd, InsertChar('!')]),
        (
            "a\nb\nc",
            vec![MoveDocumentEnd, InsertNewline, InsertChar('d')],
        ),
        ("select me", vec![SelectAll, ToUpperCase]),
        (
            "aaa\nbbb\nccc",
            vec![AddCursorBelow, AddCursorBelow, InsertChar('x')],
        ),
        ("你好世界", vec![MoveDocumentEnd, SelectLeft, SelectLeft]),
        ("", vec![InsertChar('z'), InsertChar('y'), MoveLineStart]),
    ]
}

/// A few representative orders: forward, reverse, and a rotation. Each
/// must reproduce every workload's fresh-harness baseline.
fn orders(n: usize) -> Vec<Vec<usize>> {
    let fwd: Vec<usize> = (0..n).collect();
    let rev: Vec<usize> = (0..n).rev().collect();
    let rot: Vec<usize> = (0..n).map(|i| (i + n / 2) % n).collect();
    vec![fwd, rev, rot]
}

#[test]
fn combination_workloads_are_order_independent_under_reset() {
    let work = workloads();
    let baseline: Vec<_> = work
        .iter()
        .map(|(init, actions)| evaluate_actions(init, actions))
        .collect();

    for order in orders(work.len()) {
        let observed = run_with_reset_between(&work, &order);
        for (slot, &i) in order.iter().enumerate() {
            assert_eq!(
                observed[slot], baseline[i],
                "workload {i} ({:?}) diverged from its fresh-harness baseline under order {order:?} \
                 — reset is incomplete or the workload depends on ambient state",
                work[i].0,
            );
        }
    }
}
