//! Migrated workspace scenarios — buffer-list and active-buffer
//! claims from `tests/e2e/multi_file_opening.rs` and
//! `tests/e2e/buffer_lifecycle.rs`.

use crate::common::scenario::context::{NamedBuffer, WorkspaceContext};
use crate::common::scenario::input_event::InputEvent;
use crate::common::scenario::observable::{ActivePathExpect, BufferPathsExpect, WorkspaceExpect};
use crate::common::scenario::workspace_scenario::{
    assert_workspace_scenario, check_workspace_scenario, WorkspaceScenario,
};
use fresh::test_api::Action;

#[test]
fn migrated_one_buffer_yields_count_one() {
    assert_workspace_scenario(WorkspaceScenario {
        description: "one initial buffer ⇒ buffer_count == 1, active is that file".into(),
        workspace: WorkspaceContext {
            initial_buffers: vec![NamedBuffer {
                filename: "lonely.txt".into(),
                content: "hi".into(),
            }],
            initial_splits: None,
        },
        events: vec![],
        expected: WorkspaceExpect {
            buffer_count: 1,
            active_buffer_path: ActivePathExpect::EndsWith("lonely.txt".into()),
            buffer_paths: BufferPathsExpect::EndsWithInOrder(vec!["lonely.txt".into()]),
        },
    });
}

#[test]
fn migrated_three_buffers_yield_count_three() {
    assert_workspace_scenario(WorkspaceScenario {
        description: "three initial buffers ⇒ count == 3, paths in load order".into(),
        workspace: WorkspaceContext {
            initial_buffers: vec![
                NamedBuffer {
                    filename: "a.txt".into(),
                    content: "alpha".into(),
                },
                NamedBuffer {
                    filename: "b.txt".into(),
                    content: "bravo".into(),
                },
                NamedBuffer {
                    filename: "c.txt".into(),
                    content: "charlie".into(),
                },
            ],
            initial_splits: None,
        },
        events: vec![],
        expected: WorkspaceExpect {
            buffer_count: 3,
            active_buffer_path: ActivePathExpect::Any,
            buffer_paths: BufferPathsExpect::EndsWithInOrder(vec![
                "a.txt".into(),
                "b.txt".into(),
                "c.txt".into(),
            ]),
        },
    });
}

/// Anti-test: drops two of the three initial buffers from
/// `migrated_three_buffers_yield_count_three`. With only one
/// buffer seeded, the workspace cannot satisfy `buffer_count:
/// 3` — proves the initial_buffers list is what drives the
/// count.
#[test]
fn anti_workspace_dropping_initial_buffers_yields_check_err() {
    let scenario = WorkspaceScenario {
        description: "anti: 2 of 3 initial_buffers dropped — count cannot reach 3".into(),
        workspace: WorkspaceContext {
            initial_buffers: vec![NamedBuffer {
                filename: "a.txt".into(),
                content: "alpha".into(),
            }],
            initial_splits: None,
        },
        events: vec![],
        expected: WorkspaceExpect {
            buffer_count: 3,
            active_buffer_path: ActivePathExpect::Any,
            buffer_paths: BufferPathsExpect::EndsWithInOrder(vec![
                "a.txt".into(),
                "b.txt".into(),
                "c.txt".into(),
            ]),
        },
    };
    assert!(
        check_workspace_scenario(scenario).is_err(),
        "anti-test: with only 1 initial buffer, buffer_count cannot equal 3"
    );
}

#[test]
fn migrated_typing_leaves_buffer_count_unchanged() {
    // Editing doesn't change the workspace topology.
    assert_workspace_scenario(WorkspaceScenario {
        description: "typing inside a buffer doesn't alter buffer_count".into(),
        workspace: WorkspaceContext {
            initial_buffers: vec![NamedBuffer {
                filename: "x.txt".into(),
                content: "hi".into(),
            }],
            initial_splits: None,
        },
        events: vec![
            InputEvent::Action(Action::MoveDocumentEnd),
            InputEvent::Action(Action::InsertChar('!')),
        ],
        expected: WorkspaceExpect {
            buffer_count: 1,
            active_buffer_path: ActivePathExpect::EndsWith("x.txt".into()),
            buffer_paths: BufferPathsExpect::EndsWithInOrder(vec!["x.txt".into()]),
        },
    });
}
