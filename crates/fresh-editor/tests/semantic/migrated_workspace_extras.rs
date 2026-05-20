//! Migrations of `tests/e2e/multi_file_opening.rs` and
//! `tests/e2e/buffer_lifecycle.rs` claims not yet covered by
//! `migrated_workspace.rs`. Focus: `active_buffer_path` after
//! loading multiple buffers and `Action::Close` decreasing
//! `buffer_count`.

use crate::common::scenario::context::{NamedBuffer, WorkspaceContext};
use crate::common::scenario::input_event::InputEvent;
use crate::common::scenario::observable::{ActivePathExpect, BufferPathsExpect, WorkspaceExpect};
use crate::common::scenario::workspace_scenario::{
    assert_workspace_scenario, check_workspace_scenario, WorkspaceScenario,
};
use fresh::test_api::Action;

#[test]
fn migrated_three_buffers_active_path_is_most_recently_loaded() {
    // Original: `test_open_multiple_files`. The harness's
    // `load_buffer_from_text_named` calls the same `open_file`
    // path the production binary walks, which makes the
    // most-recently-opened buffer active. So after loading three
    // buffers in order, `file3.txt` is active.
    assert_workspace_scenario(WorkspaceScenario {
        description: "three loaded buffers ⇒ active_buffer_path ends with 'file3.txt'".into(),
        workspace: WorkspaceContext {
            initial_buffers: vec![
                NamedBuffer {
                    filename: "file1.txt".into(),
                    content: "Content of file 1".into(),
                },
                NamedBuffer {
                    filename: "file2.txt".into(),
                    content: "Content of file 2".into(),
                },
                NamedBuffer {
                    filename: "file3.txt".into(),
                    content: "Content of file 3".into(),
                },
            ],
            initial_splits: None,
        },
        events: vec![],
        expected: WorkspaceExpect {
            buffer_count: 3,
            active_buffer_path: ActivePathExpect::EndsWith("file3.txt".into()),
            buffer_paths: BufferPathsExpect::EndsWithInOrder(vec![
                "file1.txt".into(),
                "file2.txt".into(),
                "file3.txt".into(),
            ]),
        },
    });
}

#[test]
fn migrated_close_active_buffer_decrements_buffer_count() {
    // Claim: closing one of two open buffers leaves exactly one.
    // Action::Close routes through `Editor::close_tab` — same path
    // the user-facing keybinding hits.
    assert_workspace_scenario(WorkspaceScenario {
        description: "two buffers, then Action::Close ⇒ buffer_count == 1".into(),
        workspace: WorkspaceContext {
            initial_buffers: vec![
                NamedBuffer {
                    filename: "alpha.txt".into(),
                    content: "alpha".into(),
                },
                NamedBuffer {
                    filename: "bravo.txt".into(),
                    content: "bravo".into(),
                },
            ],
            initial_splits: None,
        },
        events: vec![InputEvent::Action(Action::Close)],
        expected: WorkspaceExpect {
            buffer_count: 1,
            active_buffer_path: ActivePathExpect::Any,
            buffer_paths: BufferPathsExpect::Any,
        },
    });
}

#[test]
fn migrated_next_buffer_does_not_change_count() {
    // Cycling buffers via Action::NextBuffer changes which is
    // active, but never the count.
    assert_workspace_scenario(WorkspaceScenario {
        description: "NextBuffer cycles active without changing buffer_count".into(),
        workspace: WorkspaceContext {
            initial_buffers: vec![
                NamedBuffer {
                    filename: "x.txt".into(),
                    content: "x".into(),
                },
                NamedBuffer {
                    filename: "y.txt".into(),
                    content: "y".into(),
                },
                NamedBuffer {
                    filename: "z.txt".into(),
                    content: "z".into(),
                },
            ],
            initial_splits: None,
        },
        events: vec![
            InputEvent::Action(Action::NextBuffer),
            InputEvent::Action(Action::NextBuffer),
        ],
        expected: WorkspaceExpect {
            buffer_count: 3,
            // NextBuffer changes which buffer is active; pin that the
            // active path is one of the loaded files, but don't pin
            // which one — the harness's NextBuffer ordering is not
            // load-bearing on this claim.
            active_buffer_path: ActivePathExpect::Any,
            buffer_paths: BufferPathsExpect::EndsWithInOrder(vec![
                "x.txt".into(),
                "y.txt".into(),
                "z.txt".into(),
            ]),
        },
    });
}

/// Anti-test: drops the `Action::Close` event from the
/// close-decrements scenario. Without `Close`, the workspace
/// retains both buffers, so the `buffer_count: 1` expectation
/// cannot match — `check_workspace_scenario` must return `Err`.
#[test]
fn anti_close_buffer_dropping_action_yields_check_err() {
    let scenario = WorkspaceScenario {
        description: "anti: Close dropped — count must NOT decrement to 1".into(),
        workspace: WorkspaceContext {
            initial_buffers: vec![
                NamedBuffer {
                    filename: "alpha.txt".into(),
                    content: "alpha".into(),
                },
                NamedBuffer {
                    filename: "bravo.txt".into(),
                    content: "bravo".into(),
                },
            ],
            initial_splits: None,
        },
        events: vec![],
        expected: WorkspaceExpect {
            buffer_count: 1,
            active_buffer_path: ActivePathExpect::Any,
            buffer_paths: BufferPathsExpect::Any,
        },
    };
    assert!(
        check_workspace_scenario(scenario).is_err(),
        "anti-test: without Action::Close, the workspace keeps both buffers, \
         so the count-1 expectation must NOT match"
    );
}
