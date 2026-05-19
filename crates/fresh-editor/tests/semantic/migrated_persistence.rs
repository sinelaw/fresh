//! Migrated persistence scenarios — the kinds of save/load
//! claims `tests/e2e/auto_revert.rs`,
//! `tests/e2e/external_file_save_as_tab.rs`, and
//! `tests/e2e/on_save_actions.rs` make.

use crate::common::scenario::context::{VirtualFile, VirtualFs};
use crate::common::scenario::input_event::InputEvent;
use crate::common::scenario::observable::FsState;
use crate::common::scenario::persistence_scenario::{
    assert_persistence_scenario, check_persistence_scenario, write_then_save, PersistenceScenario,
};
use fresh::test_api::Action;
use std::collections::BTreeMap;
use std::path::PathBuf;

#[test]
fn migrated_save_persists_typed_text() {
    assert_persistence_scenario(write_then_save(
        "type 'world' onto 'hello ' and save persists 'hello world'",
        "doc.txt",
        "hello ",
        "world",
        "hello world",
    ));
}

#[test]
fn migrated_save_unchanged_buffer_leaves_disk_unchanged() {
    assert_persistence_scenario(write_then_save(
        "Save without typing leaves disk content intact",
        "stable.txt",
        "untouched",
        "",
        "untouched",
    ));
}

/// Anti-test: drops the `FsExternalEdit` event from
/// `migrated_external_edit_visible_to_other_processes`. Without
/// the external edit, the file on disk stays at "v1"; the
/// expected post-edit content "v2" cannot match.
#[test]
fn anti_persistence_dropping_fs_external_edit_yields_check_err() {
    let mut files = BTreeMap::new();
    files.insert(
        PathBuf::from("a.txt"),
        VirtualFile {
            content: "v1".into(),
            mode: None,
            mtime_unix_secs: None,
        },
    );
    let scenario = PersistenceScenario {
        description: "anti: FsExternalEdit dropped — disk still shows v1, not v2".into(),
        initial_fs: VirtualFs { files },
        initial_open: "a.txt".into(),
        events: vec![],
        expected_buffer: None,
        expected_fs: FsState {
            expected_files: std::iter::once(("a.txt".into(), "v2".into())).collect(),
        },
    };
    assert!(
        check_persistence_scenario(scenario).is_err(),
        "anti-test: without the FsExternalEdit event, the disk file stays at 'v1'; \
         the expected 'v2' content cannot appear"
    );
}

#[test]
fn migrated_external_edit_visible_to_other_processes() {
    let mut files = BTreeMap::new();
    files.insert(
        PathBuf::from("a.txt"),
        VirtualFile {
            content: "v1".into(),
            mode: None,
            mtime_unix_secs: None,
        },
    );
    assert_persistence_scenario(PersistenceScenario {
        description: "FsExternalEdit replaces on-disk content for other observers".into(),
        initial_fs: VirtualFs { files },
        initial_open: "a.txt".into(),
        events: vec![InputEvent::FsExternalEdit {
            path: PathBuf::from("a.txt"),
            content: "v2".into(),
        }],
        expected_buffer: None,
        expected_fs: FsState {
            expected_files: std::iter::once(("a.txt".into(), "v2".into())).collect(),
        },
    });
}

#[test]
fn migrated_external_edit_after_save_persists_until_buffer_resaves() {
    // Save once, an external process clobbers the file. We don't
    // assert what happens on the next editor save — that's an
    // auto-revert / conflict-detection behavior with non-trivial
    // semantics. We only assert the external write *did* land on
    // disk, which is what other observers would see.
    let mut files = BTreeMap::new();
    files.insert(
        PathBuf::from("race.txt"),
        VirtualFile {
            content: "start".into(),
            mode: None,
            mtime_unix_secs: None,
        },
    );
    assert_persistence_scenario(PersistenceScenario {
        description: "FsExternalEdit lands on disk regardless of editor's save state".into(),
        initial_fs: VirtualFs { files },
        initial_open: "race.txt".into(),
        events: vec![
            InputEvent::Action(Action::MoveDocumentEnd),
            InputEvent::Action(Action::InsertChar('A')),
            InputEvent::Action(Action::Save),
            InputEvent::FsExternalEdit {
                path: PathBuf::from("race.txt"),
                content: "external".into(),
            },
        ],
        expected_buffer: None,
        expected_fs: FsState {
            expected_files: std::iter::once(("race.txt".into(), "external".into())).collect(),
        },
    });
}
