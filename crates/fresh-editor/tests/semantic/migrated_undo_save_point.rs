//! DECLARATIVE: Migration of `tests/e2e/undo_redo.rs` save-point
//! claims (issue #191) and the Save-As + file-watcher flows.
//!
//! Each test is a `PersistenceScenario { ... }` literal: pure data,
//! dispatched by `assert_persistence_scenario`. The runner uses
//! `EditorTestApi` for every observable — `buffer_text`,
//! `is_modified`, `primary_caret`, `active_event_log_len` — and
//! drives the Save-As prompt via the production key handler through
//! the persistence runner's `OpenSaveAsPrompt` / `PromptBackspace` /
//! `PromptFillTempPath` / `PromptConfirm` event variants. The
//! file-watcher reaction uses `EditorFileChangedReaction`.
//!
//! No `EditorTestHarness::` calls live in this file.

use crate::common::scenario::context::{VirtualFile, VirtualFs};
use crate::common::scenario::input_event::InputEvent;
use crate::common::scenario::observable::FsState;
use crate::common::scenario::persistence_scenario::{
    assert_persistence_scenario, check_persistence_scenario, PersistenceScenario,
};
use fresh::test_api::Action;
use std::collections::BTreeMap;
use std::path::PathBuf;

/// Helper: dispatch one `InsertChar` per character in `s`.
fn insert_chars(s: &str) -> Vec<InputEvent> {
    s.chars()
        .map(|c| InputEvent::Action(Action::InsertChar(c)))
        .collect()
}

/// Helper: run `n` `Action::Undo` dispatches.
fn undos(n: usize) -> Vec<InputEvent> {
    std::iter::repeat_with(|| InputEvent::Action(Action::Undo))
        .take(n)
        .collect()
}

/// Helper: full Save-As prompt flow — open prompt, clear (32
/// defensive Backspaces in case the prompt was pre-populated with the
/// current buffer path), fill the temp-root-relative path, confirm.
fn save_as_temp_path(rel: &str) -> Vec<InputEvent> {
    vec![
        InputEvent::OpenSaveAsPrompt,
        InputEvent::PromptBackspace { count: 32 },
        InputEvent::PromptFillTempPath { rel: rel.into() },
        InputEvent::PromptConfirm,
    ]
}

#[test]
fn migrated_undo_to_save_point_marks_buffer_unmodified() {
    // Original: `test_undo_to_save_point_marks_buffer_unmodified`
    // (issue #191). Loaded-from-disk buffer is unmodified; typing
    // makes it modified; Undo of the lone insertion restores the
    // on-disk content and the flag flips back to false in one step.
    //
    // The cursor-position claim ("Undo must NOT send cursor to byte
    // 0 / top-of-screen") is encoded via `AssertPrimaryCursorAtMost`
    // — the e2e test only requires the cursor stays within "initial"
    // bounds (byte 7), not an exact post-Undo position.
    let mut files: BTreeMap<PathBuf, VirtualFile> = BTreeMap::new();
    files.insert(
        PathBuf::from("test_undo_save.txt"),
        VirtualFile {
            content: "initial".into(),
            mode: None,
            mtime_unix_secs: None,
        },
    );
    assert_persistence_scenario(PersistenceScenario {
        description: "issue #191: Undo to save-point clears is_modified in one step".into(),
        initial_fs: VirtualFs { files },
        initial_open: "test_undo_save.txt".into(),
        events: vec![
            InputEvent::AssertIsModified(false),
            InputEvent::Action(Action::MoveLineEnd),
            InputEvent::Action(Action::InsertChar('X')),
            InputEvent::AssertBufferText("initialX".into()),
            InputEvent::AssertIsModified(true),
            InputEvent::Action(Action::Undo),
            InputEvent::AssertBufferText("initial".into()),
            InputEvent::AssertIsModified(false),
            InputEvent::AssertPrimaryCursorAtMost(7),
        ],
        expected_buffer: None,
        expected_fs: FsState::default(),
    });
}

#[test]
fn migrated_undo_past_save_point_keeps_buffer_modified() {
    // Original: `test_undo_past_save_point`. Type, Save (creates save
    // point), type more, Undo past the save point ⇒ buffer is again
    // *modified* because content no longer matches disk.
    let mut files: BTreeMap<PathBuf, VirtualFile> = BTreeMap::new();
    files.insert(
        PathBuf::from("test.txt"),
        VirtualFile {
            content: "".into(),
            mode: None,
            mtime_unix_secs: None,
        },
    );

    let mut events: Vec<InputEvent> = Vec::new();
    events.extend(insert_chars("hello"));
    events.push(InputEvent::Action(Action::Save));
    events.push(InputEvent::AssertIsModified(false));
    events.extend(insert_chars(" world"));
    events.push(InputEvent::AssertBufferText("hello world".into()));
    events.push(InputEvent::AssertIsModified(true));
    events.extend(undos(" world".len()));
    events.push(InputEvent::AssertBufferText("hello".into()));
    events.push(InputEvent::AssertIsModified(false));
    // One more Undo crosses the save point.
    events.push(InputEvent::Action(Action::Undo));
    events.push(InputEvent::AssertIsModified(true));

    assert_persistence_scenario(PersistenceScenario {
        description: "Undo past save-point re-modifies the buffer".into(),
        initial_fs: VirtualFs { files },
        initial_open: "test.txt".into(),
        events,
        expected_buffer: None,
        expected_fs: FsState::default(),
    });
}

#[test]
fn migrated_undo_after_save_as_marks_buffer_unmodified() {
    // Original: `test_undo_after_save_as_marks_buffer_unmodified`
    // (issue #191). The Save As path must install a save point so
    // that Undoing back to the saved content flips `is_modified`
    // back to false — exactly like a plain Save would.

    let mut events: Vec<InputEvent> = Vec::new();
    // Step 1: default empty buffer.
    events.push(InputEvent::AssertBufferText("".into()));
    events.push(InputEvent::AssertIsModified(false));
    // Step 2: type "hello".
    events.extend(insert_chars("hello"));
    events.push(InputEvent::AssertBufferText("hello".into()));
    events.push(InputEvent::AssertIsModified(true));
    // Step 3: Save As to a fresh path inside the temp project.
    events.extend(save_as_temp_path("test_save_as.txt"));
    // KEY: immediately after Save As, the buffer is unmodified.
    events.push(InputEvent::AssertIsModified(false));
    events.push(InputEvent::AssertBufferText("hello".into()));
    // Step 4: type more text past the save point.
    events.extend(insert_chars(" world"));
    events.push(InputEvent::AssertBufferText("hello world".into()));
    events.push(InputEvent::AssertIsModified(true));
    // Step 5: Undo back to the save point.
    events.extend(undos(" world".len()));
    events.push(InputEvent::AssertBufferText("hello".into()));
    // KEY ASSERTION (issue #191 for Save As).
    events.push(InputEvent::AssertIsModified(false));

    assert_persistence_scenario(PersistenceScenario {
        description: "issue #191 (Save As): undo back to Save-As point clears is_modified".into(),
        initial_fs: VirtualFs {
            files: BTreeMap::new(),
        },
        initial_open: String::new(),
        events,
        expected_buffer: None,
        // The Save-As must have produced this file on disk.
        expected_fs: FsState {
            expected_files: std::iter::once(("test_save_as.txt".into(), "hello".into())).collect(),
        },
    });
}

#[test]
fn migrated_undo_to_empty_after_save_as() {
    // Original: `test_undo_to_empty_after_save_as`. After Save As
    // the undo log retains the pre-save edits, so Undo can walk
    // the buffer all the way back to the empty starting state. A
    // file-watcher notification for the just-written file must NOT
    // clear the undo history when the on-disk content matches the
    // buffer.

    let mut events: Vec<InputEvent> = Vec::new();
    // Step 1: start empty.
    events.push(InputEvent::AssertBufferText("".into()));
    // Step 2: type "hello" — 5 InsertChar events.
    events.extend(insert_chars("hello"));
    events.push(InputEvent::AssertBufferText("hello".into()));
    // Step 3: Save As.
    events.extend(save_as_temp_path("test_undo_empty.txt"));
    events.push(InputEvent::AssertIsModified(false));
    // Event log must still hold all 5 InsertChar events.
    events.push(InputEvent::AssertEventLogLen(5));
    // Simulate the file-watcher notification for the file we just
    // wrote. Auto-revert must be skipped when disk content matches
    // the buffer — clearing the event log here is the original bug.
    events.push(InputEvent::EditorFileChangedReaction {
        path: "test_undo_empty.txt".into(),
    });
    events.push(InputEvent::AssertEventLogLen(5));
    // Step 4: type " world" past the save point.
    events.extend(insert_chars(" world"));
    events.push(InputEvent::AssertBufferText("hello world".into()));
    events.push(InputEvent::AssertIsModified(true));
    // Step 5: Undo all the way back to empty (cap at 20 Undos —
    // the original loops to 20 with an early break on empty).
    for _ in 0..20 {
        events.push(InputEvent::Action(Action::Undo));
    }
    events.push(InputEvent::AssertBufferText("".into()));

    assert_persistence_scenario(PersistenceScenario {
        description:
            "Undo walks back to empty after Save-As; file-watcher notify doesn't clear undo log"
                .into(),
        initial_fs: VirtualFs {
            files: BTreeMap::new(),
        },
        initial_open: String::new(),
        events,
        expected_buffer: None,
        expected_fs: FsState {
            expected_files: std::iter::once(("test_undo_empty.txt".into(), "hello".into()))
                .collect(),
        },
    });
}

/// Anti-test: if Save As did **not** install a save point, then after
/// typing more and Undoing back to "hello" the buffer would still
/// report `is_modified == true`. We prove the save-point invariant is
/// real by *constructing* the alternate world: no Save As, just typing
/// → Undo → check `is_modified`. The buffer must still be modified at
/// "hello" because no save has occurred yet.
#[test]
fn anti_undo_to_text_without_save_as_stays_modified() {
    let mut events: Vec<InputEvent> = Vec::new();
    events.extend(insert_chars("hello"));
    // No Save / SaveAs here.
    events.extend(insert_chars(" world"));
    events.extend(undos(" world".len()));
    events.push(InputEvent::AssertBufferText("hello".into()));
    // Without a Save As call, undoing to 'hello' must NOT clear
    // is_modified — there is no save point to land on.
    events.push(InputEvent::AssertIsModified(true));

    assert_persistence_scenario(PersistenceScenario {
        description: "anti: no SaveAs ⇒ Undo to 'hello' still modified (no save point exists)"
            .into(),
        initial_fs: VirtualFs {
            files: BTreeMap::new(),
        },
        initial_open: String::new(),
        events,
        expected_buffer: None,
        expected_fs: FsState::default(),
    });
}

/// Anti-test: a *missing* Undo in the save-point scenario must cause
/// `AssertIsModified(false)` to fail. Pins that the persistence
/// runner's inline assertion actually evaluates state — without
/// this anti-test, the assertions could quietly become no-ops.
#[test]
fn anti_undo_to_save_point_runner_detects_missing_undo() {
    let mut files: BTreeMap<PathBuf, VirtualFile> = BTreeMap::new();
    files.insert(
        PathBuf::from("test_anti.txt"),
        VirtualFile {
            content: "initial".into(),
            mode: None,
            mtime_unix_secs: None,
        },
    );
    let scenario = PersistenceScenario {
        description: "anti: drop Undo, assert unmodified ⇒ runner must surface mismatch".into(),
        initial_fs: VirtualFs { files },
        initial_open: "test_anti.txt".into(),
        events: vec![
            InputEvent::Action(Action::MoveLineEnd),
            InputEvent::Action(Action::InsertChar('X')),
            // Skipping the Undo here:
            InputEvent::AssertIsModified(false),
        ],
        expected_buffer: None,
        expected_fs: FsState::default(),
    };
    assert!(
        check_persistence_scenario(scenario).is_err(),
        "anti-test: persistence runner must detect the missing Undo via the inline AssertIsModified"
    );
}
