//! Issue #3383: the copy an interrupted in-place save keeps is offered back.
//!
//! Before an in-place save truncates a file it stages a complete copy of the
//! new content in the recovery directory. When the write then fails part-way,
//! or the editor dies during it, that copy may be the only intact version of
//! what was being saved — yet nothing ever told the user it was there. A
//! session now starts by asking what to do with it: restore the file from
//! it, show the difference, discard it, or decide later.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::services::recovery::{path_hash, InplaceWriteRecovery};
use std::path::PathBuf;

/// No process has this pid (it's above any pid_max), so the save "crashed".
const DEAD_PID: u32 = 2_000_000_000;

const TORN: &str = "line one\nline tw";
const KEPT: &str = "line one\nline two\nline three\n";

struct Scene {
    harness: EditorTestHarness,
    file: PathBuf,
    copy: PathBuf,
    meta: PathBuf,
}

/// A torn `notes.txt` and the copy its interrupted save kept, then a session
/// start.
fn start_after_interrupted_save() -> Scene {
    let mut harness = EditorTestHarness::with_temp_project(120, 30).unwrap();
    let file = harness.project_dir().unwrap().join("notes.txt");
    std::fs::write(&file, TORN).unwrap();

    // The top-level recovery directory, where in-place saves stage — not
    // the session-scoped one below it.
    let recovery_dir = harness
        .recovery_dir()
        .unwrap()
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf();
    std::fs::create_dir_all(&recovery_dir).unwrap();
    let copy = recovery_dir.join(format!(".inplace-notes.txt-{DEAD_PID}-1.tmp"));
    std::fs::write(&copy, KEPT).unwrap();
    let mut recovery = InplaceWriteRecovery::new(file.clone(), copy.clone(), 0, 0, 0o644);
    recovery.pid = DEAD_PID;
    let meta = recovery_dir.join(format!("{}.inplace.json", path_hash(&file)));
    std::fs::write(&meta, serde_json::to_string(&recovery).unwrap()).unwrap();

    harness.startup(false, &[]).unwrap();
    Scene {
        harness,
        file,
        copy,
        meta,
    }
}

fn press(harness: &mut EditorTestHarness, c: char) {
    harness
        .send_key(KeyCode::Char(c), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

#[test]
fn interrupted_save_is_offered_and_restored() {
    let mut scene = start_after_interrupted_save();
    let harness = &mut scene.harness;

    harness.assert_screen_contains("Interrupted Save");
    harness.assert_screen_contains("A save of notes.txt was interrupted");
    for choice in ["Restore", "Show Diff", "Discard", "Later"] {
        harness.assert_screen_contains(choice);
    }

    // The difference, the file on disk beside the kept copy. The dialog
    // closes so a long diff can be scrolled, and the copy stays until the
    // user decides; the status bar says how to get back to the question.
    press(harness, 's');
    harness.assert_screen_contains("On disk");
    harness.assert_screen_contains("Kept copy");
    harness.assert_screen_contains("line three");
    harness.assert_screen_not_contains("was interrupted, so the file");
    harness.assert_screen_contains("Diff shown; run Review Interrupted Saves");
    harness
        .send_key(KeyCode::PageDown, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_not_contains("was interrupted, so the file");

    // Having read it, the user asks again from the command palette.
    harness
        .run_palette_command("Review Interrupted Saves")
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Interrupted Save");
    harness.assert_screen_contains("A save of notes.txt was interrupted");

    press(harness, 'r');
    harness.assert_screen_contains("Restored notes.txt from the kept copy");
    harness.assert_screen_not_contains("was interrupted, so the file");
    assert_eq!(std::fs::read_to_string(&scene.file).unwrap(), KEPT);
    assert!(!scene.copy.exists(), "the copy goes once the user decided");
    assert!(!scene.meta.exists(), "so does its metadata");

    // Nothing is left to review.
    harness
        .run_palette_command("Review Interrupted Saves")
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("No interrupted saves are waiting");
    harness.assert_screen_not_contains("was interrupted, so the file");
}

#[test]
fn interrupted_save_copy_can_be_discarded() {
    let mut scene = start_after_interrupted_save();
    let harness = &mut scene.harness;
    harness.assert_screen_contains("Interrupted Save");

    press(harness, 'd');

    harness.assert_screen_contains("Discarded the kept copy of notes.txt");
    harness.assert_screen_not_contains("was interrupted, so the file");
    assert_eq!(std::fs::read_to_string(&scene.file).unwrap(), TORN);
    assert!(!scene.copy.exists());
    assert!(!scene.meta.exists());
}

/// Deciding later keeps the copy and says where it is; the command (or the
/// next session) asks again.
#[test]
fn interrupted_save_decided_later_is_kept() {
    let mut scene = start_after_interrupted_save();
    let harness = &mut scene.harness;
    harness.assert_screen_contains("Interrupted Save");

    press(harness, 'l');

    harness.assert_screen_contains("Kept copy of notes.txt:");
    harness.assert_screen_not_contains("was interrupted, so the file");
    assert!(scene.copy.exists());
    assert!(scene.meta.exists());
    assert_eq!(std::fs::read_to_string(&scene.file).unwrap(), TORN);

    // The command asks about it again in the same session.
    harness
        .run_palette_command("Review Interrupted Saves")
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("A save of notes.txt was interrupted");
}

/// Plant the copy a save of `file` kept, written by `pid`, in the harness's
/// top-level recovery directory. Returns the copy's path.
fn plant_kept_copy(harness: &EditorTestHarness, file: &std::path::Path, pid: u32) -> PathBuf {
    let recovery_dir = harness
        .recovery_dir()
        .unwrap()
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf();
    std::fs::create_dir_all(&recovery_dir).unwrap();
    let copy = recovery_dir.join(format!(".inplace-notes.txt-{pid}-1.tmp"));
    std::fs::write(&copy, KEPT).unwrap();
    let mut recovery = InplaceWriteRecovery::new(file.to_path_buf(), copy.clone(), 0, 0, 0o644);
    recovery.pid = pid;
    let meta = recovery_dir.join(format!("{}.inplace.json", path_hash(file)));
    std::fs::write(&meta, serde_json::to_string(&recovery).unwrap()).unwrap();
    copy
}

/// A copy kept by a save of this very session is waiting too: its file's
/// saves are refused until the user decides, so "Review Interrupted Saves"
/// must offer it now, not only after a restart. (Saves are synchronous, so
/// none of this session's is still in flight when the command runs.)
#[test]
fn interrupted_save_of_this_session_is_offered() {
    let mut harness = EditorTestHarness::with_temp_project(120, 30).unwrap();
    let file = harness.project_dir().unwrap().join("notes.txt");
    std::fs::write(&file, TORN).unwrap();
    harness.startup(false, &[]).unwrap();
    plant_kept_copy(&harness, &file, std::process::id());

    harness
        .run_palette_command("Review Interrupted Saves")
        .unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("A save of notes.txt was interrupted");
    harness.assert_screen_not_contains("No interrupted saves are waiting");
}

/// Restoring the file from the kept copy rewrites it behind the back of a
/// buffer that already has it open. One with no unsaved changes is
/// reloaded, so it shows what the file now holds (a large one would
/// otherwise read the parts it never loaded at stale offsets).
#[test]
fn restoring_an_interrupted_save_reloads_its_open_buffer() {
    let mut harness = EditorTestHarness::with_temp_project(120, 30).unwrap();
    let file = harness.project_dir().unwrap().join("notes.txt");
    std::fs::write(&file, TORN).unwrap();
    harness.startup(false, &[]).unwrap();
    harness.open_file(&file).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("line tw");
    harness.assert_screen_not_contains("line three");
    plant_kept_copy(&harness, &file, DEAD_PID);

    harness
        .run_palette_command("Review Interrupted Saves")
        .unwrap();
    harness.render().unwrap();
    press(&mut harness, 'r');

    harness.assert_screen_contains("Restored notes.txt from the kept copy");
    harness.assert_screen_contains("line three");
}

/// A buffer with unsaved changes keeps them when its file is restored, and
/// saving it asks first: the file changed on disk.
#[test]
fn restoring_an_interrupted_save_keeps_unsaved_changes() {
    let mut harness = EditorTestHarness::with_temp_project(120, 30).unwrap();
    let file = harness.project_dir().unwrap().join("notes.txt");
    std::fs::write(&file, TORN).unwrap();
    harness.startup(false, &[]).unwrap();
    harness.open_file(&file).unwrap();
    harness.type_text("UNSAVED ").unwrap();
    harness.render().unwrap();
    plant_kept_copy(&harness, &file, DEAD_PID);

    harness
        .run_palette_command("Review Interrupted Saves")
        .unwrap();
    harness.render().unwrap();
    press(&mut harness, 'r');

    harness.assert_screen_contains("Restored notes.txt from the kept copy");
    harness.assert_screen_contains("UNSAVED line one");
    assert_eq!(std::fs::read_to_string(&file).unwrap(), KEPT);

    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("File Changed on Disk");
    assert_eq!(std::fs::read_to_string(&file).unwrap(), KEPT);
}
