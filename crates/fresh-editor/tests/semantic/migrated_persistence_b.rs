//! Multi-file PersistenceScenarios — saving across more than one
//! file and asserting the disk state for each.

use crate::common::scenario::context::{VirtualFile, VirtualFs};
use crate::common::scenario::input_event::InputEvent;
use crate::common::scenario::observable::FsState;
use crate::common::scenario::persistence_scenario::{
    assert_persistence_scenario, PersistenceScenario,
};
use fresh::test_api::Action;
use std::collections::BTreeMap;
use std::path::PathBuf;

#[test]
fn migrated_save_preserves_unedited_sibling_files() {
    let mut files = BTreeMap::new();
    files.insert(
        PathBuf::from("a.txt"),
        VirtualFile {
            content: "untouched".into(),
            mode: None,
            mtime_unix_secs: None,
        },
    );
    files.insert(
        PathBuf::from("b.txt"),
        VirtualFile {
            content: "edited me".into(),
            mode: None,
            mtime_unix_secs: None,
        },
    );
    assert_persistence_scenario(PersistenceScenario {
        description: "saving b.txt leaves a.txt unchanged on disk".into(),
        initial_fs: VirtualFs { files },
        initial_open: "b.txt".into(),
        events: vec![
            InputEvent::Action(Action::MoveDocumentEnd),
            InputEvent::Action(Action::InsertChar('!')),
            InputEvent::Action(Action::Save),
        ],
        expected_buffer: None,
        expected_fs: FsState {
            expected_files: [
                ("a.txt".into(), "untouched".into()),
                ("b.txt".into(), "edited me!".into()),
            ]
            .into_iter()
            .collect(),
        },
    });
}

#[test]
fn migrated_external_edits_to_two_files_both_visible_on_disk() {
    let mut files = BTreeMap::new();
    files.insert(
        PathBuf::from("one.txt"),
        VirtualFile {
            content: "v1".into(),
            mode: None,
            mtime_unix_secs: None,
        },
    );
    files.insert(
        PathBuf::from("two.txt"),
        VirtualFile {
            content: "v2".into(),
            mode: None,
            mtime_unix_secs: None,
        },
    );
    assert_persistence_scenario(PersistenceScenario {
        description: "external edits to two files land independently".into(),
        initial_fs: VirtualFs { files },
        initial_open: "one.txt".into(),
        events: vec![
            InputEvent::FsExternalEdit {
                path: PathBuf::from("one.txt"),
                content: "one-mod".into(),
            },
            InputEvent::FsExternalEdit {
                path: PathBuf::from("two.txt"),
                content: "two-mod".into(),
            },
        ],
        expected_buffer: None,
        expected_fs: FsState {
            expected_files: [
                ("one.txt".into(), "one-mod".into()),
                ("two.txt".into(), "two-mod".into()),
            ]
            .into_iter()
            .collect(),
        },
    });
}

#[test]
fn migrated_save_then_external_edit_then_check_external_wins_on_disk() {
    let mut files = BTreeMap::new();
    files.insert(
        PathBuf::from("doc.txt"),
        VirtualFile {
            content: "in".into(),
            mode: None,
            mtime_unix_secs: None,
        },
    );
    assert_persistence_scenario(PersistenceScenario {
        description: "FsExternalEdit after Save overrides the on-disk content".into(),
        initial_fs: VirtualFs { files },
        initial_open: "doc.txt".into(),
        events: vec![
            InputEvent::Action(Action::MoveDocumentEnd),
            InputEvent::Action(Action::InsertChar('A')),
            InputEvent::Action(Action::Save),
            InputEvent::FsExternalEdit {
                path: PathBuf::from("doc.txt"),
                content: "EXTERNAL".into(),
            },
        ],
        expected_buffer: None,
        expected_fs: FsState {
            expected_files: std::iter::once(("doc.txt".into(), "EXTERNAL".into())).collect(),
        },
    });
}

