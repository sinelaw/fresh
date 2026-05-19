//! Wave-3 WorkspaceScenarios — additional buffer/tab claims.

use crate::common::scenario::context::{NamedBuffer, WorkspaceContext};
use crate::common::scenario::observable::{
    ActivePathExpect, BufferPathsExpect, WorkspaceExpect,
};
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
