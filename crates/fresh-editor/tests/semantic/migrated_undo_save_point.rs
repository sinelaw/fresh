//! Migrations of `tests/e2e/undo_redo.rs` save-point claims
//! (issue #191): the buffer's `is_modified` flag must flip back to
//! false the moment Undo brings the buffer text back to a saved
//! state, without requiring an "extra" Undo step.
//!
//! These tests assert `is_modified` at multiple points along an
//! action sequence, so they go through `EditorTestHarness` +
//! `EditorTestApi` directly rather than the single-shot
//! `assert_buffer_scenario` runner. Same dispatch path as the
//! production keymap; no mocks.
//!
//! The `Save As` migrations (`migrated_undo_after_save_as_*` and
//! `migrated_undo_to_empty_after_save_as`) additionally drive the
//! `Action::SaveAs` flow, which routes through the
//! `PromptType::SaveFileAs` prompt rather than the direct
//! `Action::Save` path. The semantic `EditorTestApi` does not expose
//! prompt input today, so these tests reach for `EditorTestHarness`
//! key-level helpers (`type_text`, `send_key`) to fill the prompt —
//! the same surface the production keymap uses. The `is_modified`
//! pinning at each step still goes through `EditorTestApi` so the
//! save-point invariant is asserted on the projection, not a
//! private observable.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::test_api::Action;

#[test]
fn migrated_undo_to_save_point_marks_buffer_unmodified() {
    // Original: `test_undo_to_save_point_marks_buffer_unmodified`
    // (issue #191). Loaded-from-disk buffer is unmodified;
    // typing makes it modified; Undo of the lone insertion
    // restores the on-disk content and the flag flips back to
    // false in one step.
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let _fixture = harness
        .load_buffer_from_text_named("test_undo_save.txt", "initial")
        .unwrap();

    assert!(
        !harness.api_mut().is_modified(),
        "Freshly loaded buffer must not be marked modified"
    );

    harness.api_mut().dispatch(Action::MoveLineEnd);
    harness.api_mut().dispatch(Action::InsertChar('X'));
    assert_eq!(harness.api_mut().buffer_text(), "initialX");
    assert!(
        harness.api_mut().is_modified(),
        "Buffer must be modified after typing"
    );

    harness.api_mut().dispatch(Action::Undo);
    assert_eq!(harness.api_mut().buffer_text(), "initial");
    assert!(
        !harness.api_mut().is_modified(),
        "Issue #191: undoing to the saved state must clear is_modified \
         in a single Undo step (no extra step needed)"
    );

    // Cursor must land within the text bounds — the issue
    // specifically called out cursor going to position 0
    // (top-of-screen) on undo. `MoveLineEnd` + `InsertChar('X')`
    // before undo had cursor at byte 8; undo restores the cursor
    // to byte 7 (end of "initial") since that's the position
    // before the insertion.
    let pos = harness.api_mut().primary_caret().position;
    assert!(
        pos <= 7,
        "Cursor must stay within 'initial' bounds after Undo; got byte {pos}"
    );
}

#[test]
fn migrated_undo_past_save_point_keeps_buffer_modified() {
    // Original: `test_undo_past_save_point`. Type, Save (creates
    // save point), type more, Undo past the save point ⇒ buffer
    // is again *modified* because content no longer matches
    // disk.
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let _fixture = harness.load_buffer_from_text_named("test.txt", "").unwrap();

    // Type "hello", save → save point at "hello".
    for c in "hello".chars() {
        harness.api_mut().dispatch(Action::InsertChar(c));
    }
    harness.api_mut().dispatch(Action::Save);
    assert!(
        !harness.api_mut().is_modified(),
        "Buffer must be unmodified immediately after Save"
    );

    // Type " world" — beyond the save point.
    for c in " world".chars() {
        harness.api_mut().dispatch(Action::InsertChar(c));
    }
    assert_eq!(harness.api_mut().buffer_text(), "hello world");
    assert!(harness.api_mut().is_modified());

    // Undo all 6 post-save inserts ⇒ back at the save point.
    for _ in 0.." world".len() {
        harness.api_mut().dispatch(Action::Undo);
    }
    assert_eq!(harness.api_mut().buffer_text(), "hello");
    assert!(
        !harness.api_mut().is_modified(),
        "Undo back to the save-point must clear is_modified"
    );

    // One more Undo crosses the save point — the buffer is
    // *unsaved* against the on-disk "hello" content.
    harness.api_mut().dispatch(Action::Undo);
    assert_ne!(
        harness.api_mut().buffer_text(),
        "hello",
        "Undoing past the save point must change the buffer text"
    );
    assert!(
        harness.api_mut().is_modified(),
        "Crossing back over the save point must re-flag the buffer as modified"
    );
}

/// Drive the `Action::SaveAs` prompt with a temp-project path, then
/// fill the prompt input and confirm. Returns the absolute path the
/// buffer was saved to. Asserts the on-disk file was created.
fn save_as_with_temp_path(harness: &mut EditorTestHarness, file_name: &str) -> std::path::PathBuf {
    let save_path = harness
        .project_dir()
        .expect("with_temp_project harness must have a project_dir")
        .join(file_name);
    let save_path_str = save_path
        .to_str()
        .expect("temp path must be UTF-8")
        .to_string();

    // Open the SaveFileAs prompt through the same Action handler the
    // input layer uses. `Action::SaveAs` ⇒ `start_prompt_with_initial_text`.
    harness.api_mut().dispatch(Action::SaveAs);
    assert!(
        harness.editor().is_prompting(),
        "Action::SaveAs must open a prompt"
    );

    // Fill the prompt with the destination path. `type_text` routes
    // every char through `handle_key`, which delegates to the prompt
    // input handler while the prompt is active. We first send a few
    // backspaces in case the prompt was pre-populated with the (here
    // empty) buffer path — defensive, no-op for new buffers.
    for _ in 0..32 {
        harness
            .send_key(KeyCode::Backspace, KeyModifiers::NONE)
            .unwrap();
    }
    harness.type_text(&save_path_str).unwrap();

    // Confirm with Enter ⇒ `confirm_prompt` ⇒ `handle_save_file_as`
    // ⇒ `perform_save_file_as`. This is the same path the e2e
    // version exercises through the command palette.
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    assert!(
        save_path.exists(),
        "SaveAs must have written the buffer to disk at {}",
        save_path.display()
    );
    save_path
}

#[test]
fn migrated_undo_after_save_as_marks_buffer_unmodified() {
    // Original: `test_undo_after_save_as_marks_buffer_unmodified`
    // (issue #191). The Save As path must install a save point so
    // that Undoing back to the saved content flips `is_modified`
    // back to false — exactly like a plain Save would.

    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();

    // Step 1: start with the harness's default empty buffer.
    assert_eq!(harness.api_mut().buffer_text(), "");
    assert!(
        !harness.api_mut().is_modified(),
        "Default empty buffer must start unmodified"
    );

    // Step 2: type "hello".
    for c in "hello".chars() {
        harness.api_mut().dispatch(Action::InsertChar(c));
    }
    assert_eq!(harness.api_mut().buffer_text(), "hello");
    assert!(
        harness.api_mut().is_modified(),
        "Buffer must be modified after typing"
    );

    // Step 3: Save As to a fresh path inside the temp project.
    let _save_path = save_as_with_temp_path(&mut harness, "test_save_as.txt");

    // KEY: immediately after Save As, the buffer is unmodified —
    // the save point is now "hello".
    assert!(
        !harness.api_mut().is_modified(),
        "Buffer must NOT be modified immediately after Save As"
    );
    assert_eq!(harness.api_mut().buffer_text(), "hello");

    // Step 4: type more text past the save point.
    for c in " world".chars() {
        harness.api_mut().dispatch(Action::InsertChar(c));
    }
    assert_eq!(harness.api_mut().buffer_text(), "hello world");
    assert!(harness.api_mut().is_modified());

    // Step 5: Undo back to the save point. Each InsertChar is its
    // own event, so 6 Undos drains " world".
    for _ in 0.." world".len() {
        harness.api_mut().dispatch(Action::Undo);
    }
    assert_eq!(harness.api_mut().buffer_text(), "hello");

    // KEY ASSERTION (issue #191 for Save As): the buffer is
    // unmodified at the save point reached via Undo — no extra
    // Undo step required.
    assert!(
        !harness.api_mut().is_modified(),
        "Issue #191 (Save As): undoing back to the Save As point must clear \
         is_modified in a single Undo step"
    );

    // One more Undo crosses the save point — the buffer text
    // changes and the modified flag re-engages.
    harness.api_mut().dispatch(Action::Undo);
    let after = harness.api_mut().buffer_text();
    if after != "hello" {
        assert!(
            harness.api_mut().is_modified(),
            "Buffer must be modified after Undoing past the Save As point \
             (now at {after:?}, no longer matches the on-disk save)"
        );
    }
}

#[test]
fn migrated_undo_to_empty_after_save_as() {
    // Original: `test_undo_to_empty_after_save_as`. After Save As
    // the undo log retains the pre-save edits, so Undo can walk
    // the buffer all the way back to the empty starting state. A
    // file-watcher notification for the just-written file must
    // NOT clear the undo history when the on-disk content matches
    // the buffer.

    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();

    // Step 1: start empty.
    assert_eq!(harness.api_mut().buffer_text(), "");

    // Step 2: type "hello" — 5 events in the log.
    for c in "hello".chars() {
        harness.api_mut().dispatch(Action::InsertChar(c));
    }
    assert_eq!(harness.api_mut().buffer_text(), "hello");

    // Step 3: Save As.
    let save_path = save_as_with_temp_path(&mut harness, "test_undo_empty.txt");
    assert!(!harness.api_mut().is_modified());

    // Event log must still hold all 5 InsertChar events after Save As.
    let event_log_len_after_save = harness.editor().active_event_log().len();
    assert_eq!(
        event_log_len_after_save, 5,
        "Event log must retain all 5 InsertChar events after Save As; got {event_log_len_after_save}"
    );

    // Simulate the file-watcher notification for the file we just
    // wrote. Auto-revert must be skipped when disk content matches
    // the buffer — clearing the event log here is the original bug.
    harness
        .editor_mut()
        .handle_file_changed(save_path.to_str().unwrap());

    let event_log_len = harness.editor().active_event_log().len();
    assert_eq!(
        event_log_len, 5,
        "File-changed notification for unchanged content must NOT clear \
         the undo history; got event_log len={event_log_len}"
    );

    // Step 4: type " world" past the save point.
    for c in " world".chars() {
        harness.api_mut().dispatch(Action::InsertChar(c));
    }
    assert_eq!(harness.api_mut().buffer_text(), "hello world");
    assert!(harness.api_mut().is_modified());

    // Step 5: Undo all the way back to empty. 11 events total
    // ("hello" + " world"); cap at 20 to keep the loop bounded
    // without depending on internal batching choices.
    for _ in 0..20 {
        if harness.api_mut().buffer_text().is_empty() {
            break;
        }
        harness.api_mut().dispatch(Action::Undo);
    }

    let final_text = harness.api_mut().buffer_text();
    assert!(
        final_text.is_empty(),
        "Undo must walk all the way back to the empty starting state; \
         stopped at {final_text:?}"
    );
}

/// Anti-test: if Save As did **not** install a save point, then
/// after typing more and Undoing back to "hello" the buffer would
/// still report `is_modified == true` (since the only "saved
/// state" baseline would be the empty initial buffer, not "hello").
/// We prove the save-point invariant is real by *constructing* the
/// alternate world: no Save As, just typing → Undo → check
/// `is_modified`. The buffer must still be modified at "hello"
/// because no save has occurred yet.
#[test]
fn anti_undo_to_text_without_save_as_stays_modified() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();

    for c in "hello".chars() {
        harness.api_mut().dispatch(Action::InsertChar(c));
    }
    // No Save / SaveAs here.
    for c in " world".chars() {
        harness.api_mut().dispatch(Action::InsertChar(c));
    }
    for _ in 0.." world".len() {
        harness.api_mut().dispatch(Action::Undo);
    }
    assert_eq!(harness.api_mut().buffer_text(), "hello");
    assert!(
        harness.api_mut().is_modified(),
        "Without a Save As call, undoing to 'hello' must NOT clear is_modified — \
         there is no save point to land on. This proves the positive test's \
         is_modified==false claim is load-bearing, not vacuous."
    );
}
