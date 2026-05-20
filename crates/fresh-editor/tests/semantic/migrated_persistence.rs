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
use crate::common::scenario::property::BufferState;
use fresh::test_api::{Action, Caret};
use std::collections::BTreeMap;
use std::path::PathBuf;

/// Build the expected post-revert `BufferState`: a single caret at
/// `position`, no selection, buffer text `text`. Used by the
/// external-edit scenarios to pin that auto-revert (the runner's
/// `notify_file_changed` after `FsExternalEdit`) actually reloaded
/// the editor buffer from the new disk content — mirroring the
/// `assert_buffer_content(new_content)` claims in the original
/// `tests/e2e/auto_revert.rs`.
fn reverted_buffer(text: &str, position: usize) -> BufferState {
    BufferState {
        buffer_text: text.to_string(),
        primary: Caret {
            position,
            anchor: None,
        },
        all_carets: vec![Caret {
            position,
            anchor: None,
        }],
        selection_text: String::new(),
    }
}

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

/// Anti-test making the auto-revert `expected_buffer` assertion in
/// `migrated_external_edit_visible_to_other_processes` load-bearing.
/// With the `FsExternalEdit` dropped, no external write + notify
/// happens, so the buffer is never reverted and still reads "v1".
/// The expected reverted buffer "v2" therefore cannot match — proving
/// the positive test's `expected_buffer` is genuinely pinning the
/// auto-revert reload, not a coincidence.
#[test]
fn anti_persistence_dropping_fs_external_edit_buffer_does_not_revert() {
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
        description: "anti: no FsExternalEdit — buffer stays 'v1', cannot equal reverted 'v2'"
            .into(),
        initial_fs: VirtualFs { files },
        initial_open: "a.txt".into(),
        events: vec![],
        expected_buffer: Some(reverted_buffer("v2", 0)),
        expected_fs: FsState {
            expected_files: std::iter::once(("a.txt".into(), "v1".into())).collect(),
        },
    };
    assert!(
        check_persistence_scenario(scenario).is_err(),
        "anti-test: without the FsExternalEdit event the buffer is never auto-reverted; \
         it stays 'v1' and cannot match the expected reverted 'v2'"
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
        description:
            "FsExternalEdit replaces on-disk content AND the open buffer auto-reverts to it".into(),
        initial_fs: VirtualFs { files },
        initial_open: "a.txt".into(),
        events: vec![InputEvent::FsExternalEdit {
            path: PathBuf::from("a.txt"),
            content: "v2".into(),
        }],
        // The buffer was unmodified, so `handle_file_changed`
        // auto-reverts it to the new disk content "v2" (cursor
        // resets to byte 0). This mirrors
        // `tests/e2e/auto_revert.rs::test_auto_revert_multiple_external_edits`'s
        // `assert_buffer_content(new_content)` claim.
        expected_buffer: Some(reverted_buffer("v2", 0)),
        expected_fs: FsState {
            expected_files: std::iter::once(("a.txt".into(), "v2".into())).collect(),
        },
    });
}

#[test]
fn migrated_external_edit_after_save_persists_until_buffer_resaves() {
    // Type 'A', Save (buffer now clean), then an external process
    // clobbers the file. Because the buffer is unmodified after
    // Save, the external edit + notify auto-reverts it to the new
    // disk content "external". The external write also lands on
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
        description:
            "After Save, FsExternalEdit lands on disk AND auto-reverts the clean buffer to it"
                .into(),
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
        // Post-Save the buffer was "startA" and clean; the external
        // edit reverts it to "external". The active-buffer revert
        // path preserves the cursor (byte 6, where 'A' left it),
        // clamped within the new content.
        expected_buffer: Some(reverted_buffer("external", 6)),
        expected_fs: FsState {
            expected_files: std::iter::once(("race.txt".into(), "external".into())).collect(),
        },
    });
}
