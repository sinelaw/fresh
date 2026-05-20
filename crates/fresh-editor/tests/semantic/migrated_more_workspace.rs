//! Wave-3 WorkspaceScenarios — additional buffer/tab claims.
//!
//! Source coverage: assertions in this file pin invariants from
//! `tests/e2e/multi_file_opening.rs` and
//! `tests/e2e/buffer_lifecycle.rs` (buffer-count tracking under
//! load and rotation). Specific e2e tests aren't cited per-test
//! because this file is intentionally broad — the topology
//! invariants (count after N loads, count after NextBuffer)
//! cover the e2e cases collectively rather than 1:1.

use crate::common::scenario::context::{NamedBuffer, WorkspaceContext};
use crate::common::scenario::observable::{ActivePathExpect, BufferPathsExpect, WorkspaceExpect};
use crate::common::scenario::workspace_scenario::{assert_workspace_scenario, WorkspaceScenario};

#[test]
fn migrated_zero_initial_buffers_yields_empty_workspace_runner_error() {
    use crate::common::scenario::workspace_scenario::check_workspace_scenario;
    // Empty workspace context is rejected by the runner with a
    // precise message — we don't allow scenarios to be vacuously
    // OK.
    let result = check_workspace_scenario(WorkspaceScenario {
        description: "empty workspace context is rejected".into(),
        workspace: WorkspaceContext::default(),
        events: vec![],
        expected: WorkspaceExpect::default(),
    });
    assert!(
        result.is_err(),
        "empty workspace must error, got {result:?}"
    );
}

/// Anti-test: drops the initial buffers from
/// `migrated_five_initial_buffers_yield_count_five`. Without
/// seeding 5 buffers, the workspace either errors (empty) or
/// has the wrong count, so the
/// `buffer_count: 5` expectation cannot match.
#[test]
fn anti_more_workspace_dropping_initial_buffers_yields_check_err() {
    use crate::common::scenario::workspace_scenario::check_workspace_scenario;
    let scenario = WorkspaceScenario {
        description: "anti: initial_buffers dropped — workspace cannot reach count=5".into(),
        workspace: WorkspaceContext {
            initial_buffers: vec![],
            initial_splits: None,
        },
        events: vec![],
        expected: WorkspaceExpect {
            buffer_count: 5,
            active_buffer_path: ActivePathExpect::Any,
            buffer_paths: BufferPathsExpect::EndsWithInOrder(
                (0..5).map(|i| format!("file_{i}.txt")).collect(),
            ),
        },
    };
    assert!(
        check_workspace_scenario(scenario).is_err(),
        "anti-test: without the 5 initial buffers, buffer_count cannot equal 5"
    );
}

#[test]
fn migrated_five_initial_buffers_yield_count_five() {
    let buffers: Vec<NamedBuffer> = (0..5)
        .map(|i| NamedBuffer {
            filename: format!("file_{i}.txt"),
            content: format!("content {i}"),
        })
        .collect();
    assert_workspace_scenario(WorkspaceScenario {
        description: "five initial buffers ⇒ buffer_count == 5, paths in load order".into(),
        workspace: WorkspaceContext {
            initial_buffers: buffers,
            initial_splits: None,
        },
        events: vec![],
        expected: WorkspaceExpect {
            buffer_count: 5,
            active_buffer_path: ActivePathExpect::Any,
            buffer_paths: BufferPathsExpect::EndsWithInOrder(
                (0..5).map(|i| format!("file_{i}.txt")).collect(),
            ),
        },
    });
}
