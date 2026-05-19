//! `WorkspaceScenario` — splits, tabs, and buffer-list state.
//!
//! Phase 7 minimal: asserts on the buffer count and the active
//! buffer's display path. Splits/tabs come incrementally as
//! scenarios that need them are added.

use crate::common::harness::EditorTestHarness;
use crate::common::scenario::context::WorkspaceContext;
use crate::common::scenario::failure::ScenarioFailure;
use crate::common::scenario::input_event::InputEvent;
use crate::common::scenario::observable::{
    ActivePathExpect, BufferPathsExpect, Observable, WorkspaceExpect, WorkspaceState,
};
use fresh::test_api::EditorTestApi;

#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct WorkspaceScenario {
    pub description: String,
    pub workspace: WorkspaceContext,
    pub events: Vec<InputEvent>,
    pub expected: WorkspaceExpect,
}

pub fn check_workspace_scenario(s: WorkspaceScenario) -> Result<(), ScenarioFailure> {
    if s.workspace.initial_buffers.is_empty() && s.workspace.initial_splits.is_none() {
        return Err(ScenarioFailure::InputProjectionFailed {
            description: s.description,
            reason: "WorkspaceScenario phase: empty workspace context (no buffers or splits)"
                .into(),
        });
    }

    let mut harness = EditorTestHarness::with_temp_project(80, 24)
        .expect("EditorTestHarness::with_temp_project failed");

    // Open every initial buffer; the first becomes active.
    for buf in &s.workspace.initial_buffers {
        let _ = harness
            .load_buffer_from_text_named(&buf.filename, &buf.content)
            .expect("load_buffer_from_text_named failed");
    }

    {
        let api: &mut dyn EditorTestApi = harness.api_mut();
        for ev in &s.events {
            match ev {
                InputEvent::Action(a) => api.dispatch(a.clone()),
                other => {
                    return Err(ScenarioFailure::InputProjectionFailed {
                        description: s.description,
                        reason: format!("WorkspaceScenario phase: {other:?} not yet routable"),
                    });
                }
            }
        }
    }

    let actual = WorkspaceState::extract(&mut harness);
    if actual.buffer_count != s.expected.buffer_count {
        return Err(ScenarioFailure::WorkspaceStateMismatch {
            description: s.description,
            field: "buffer_count".into(),
            expected: s.expected.buffer_count.to_string(),
            actual: actual.buffer_count.to_string(),
        });
    }

    match &s.expected.active_buffer_path {
        ActivePathExpect::Any => {}
        ActivePathExpect::None_ => {
            if actual.active_buffer_path.is_some() {
                return Err(ScenarioFailure::WorkspaceStateMismatch {
                    description: s.description,
                    field: "active_buffer_path".into(),
                    expected: "None".into(),
                    actual: format!("{:?}", actual.active_buffer_path),
                });
            }
        }
        ActivePathExpect::EndsWith(suffix) => match actual.active_buffer_path.as_deref() {
            None => {
                return Err(ScenarioFailure::WorkspaceStateMismatch {
                    description: s.description,
                    field: "active_buffer_path".into(),
                    expected: format!("EndsWith({suffix:?})"),
                    actual: "None".into(),
                });
            }
            Some(p) if !p.ends_with(suffix) => {
                return Err(ScenarioFailure::WorkspaceStateMismatch {
                    description: s.description,
                    field: "active_buffer_path".into(),
                    expected: format!("EndsWith({suffix:?})"),
                    actual: format!("{p:?}"),
                });
            }
            Some(_) => {}
        },
    }

    match &s.expected.buffer_paths {
        BufferPathsExpect::Any => {}
        BufferPathsExpect::EndsWithInOrder(expected) => {
            if actual.buffer_paths.len() != expected.len() {
                return Err(ScenarioFailure::WorkspaceStateMismatch {
                    description: s.description,
                    field: "buffer_paths.len".into(),
                    expected: expected.len().to_string(),
                    actual: format!("{} ({:?})", actual.buffer_paths.len(), actual.buffer_paths),
                });
            }
            for (i, (want, got)) in expected.iter().zip(actual.buffer_paths.iter()).enumerate() {
                if !got.ends_with(want) {
                    return Err(ScenarioFailure::WorkspaceStateMismatch {
                        description: s.description,
                        field: format!("buffer_paths[{i}]"),
                        expected: format!("EndsWith({want:?})"),
                        actual: format!("{got:?}"),
                    });
                }
            }
        }
    }

    Ok(())
}

pub fn assert_workspace_scenario(s: WorkspaceScenario) {
    if let Err(f) = check_workspace_scenario(s) {
        panic!("{f}");
    }
}
