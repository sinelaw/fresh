mod common;

use common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

/// `save_all` writes every modified, file-backed buffer to disk in one shot
/// and reports how many were saved. This is the core of the "Save All" command
/// requested in issue #2289.
#[test]
fn test_save_all_writes_every_modified_buffer() -> anyhow::Result<()> {
    let mut harness = EditorTestHarness::with_temp_project(80, 24)?;
    let dir = harness.project_dir().unwrap();

    let a = dir.join("a.txt");
    let b = dir.join("b.txt");
    fs::write(&a, "alpha")?;
    fs::write(&b, "beta")?;

    // Open and dirty both buffers.
    harness.open_file(&a)?;
    harness.type_text("A")?;
    harness.open_file(&b)?;
    harness.type_text("B")?;

    let (saved, failed) = harness.editor_mut().save_all()?;
    assert_eq!((saved, failed), (2, 0), "both modified files should save");

    // Both files on disk reflect the edits.
    assert!(
        fs::read_to_string(&a)?.contains('A'),
        "a.txt should be saved"
    );
    assert!(
        fs::read_to_string(&b)?.contains('B'),
        "b.txt should be saved"
    );

    Ok(())
}

/// With nothing modified, `save_all` is a no-op and reports zero saves.
#[test]
fn test_save_all_no_modified_buffers() -> anyhow::Result<()> {
    let mut harness = EditorTestHarness::with_temp_project(80, 24)?;
    let dir = harness.project_dir().unwrap();
    let a = dir.join("clean.txt");
    fs::write(&a, "untouched")?;

    harness.open_file(&a)?;

    let (saved, failed) = harness.editor_mut().save_all()?;
    assert_eq!((saved, failed), (0, 0), "clean buffer must not be saved");
    assert_eq!(fs::read_to_string(&a)?, "untouched");

    Ok(())
}

/// Unnamed (scratch) buffers have no path to save to, so `save_all` leaves
/// them untouched rather than failing or saving them somewhere arbitrary.
#[test]
fn test_save_all_skips_unnamed_buffer() -> anyhow::Result<()> {
    let mut harness = EditorTestHarness::with_temp_project(80, 24)?;
    let dir = harness.project_dir().unwrap();
    let a = dir.join("named.txt");
    fs::write(&a, "hello")?;

    harness.open_file(&a)?;
    harness.type_text("!")?;

    // A fresh scratch buffer with no file path.
    harness.new_buffer()?;
    harness.type_text("scratch")?;
    assert!(harness.editor().active_state().buffer.is_modified());

    let (saved, failed) = harness.editor_mut().save_all()?;
    assert_eq!(
        (saved, failed),
        (1, 0),
        "only the named buffer should be saved; the scratch buffer is skipped"
    );
    // The scratch buffer is still dirty — it was never written anywhere.
    assert!(
        harness.editor().active_state().buffer.is_modified(),
        "unnamed buffer should remain modified"
    );

    Ok(())
}

/// End-to-end: running "Save All" from the command palette saves every dirty
/// file and surfaces the result in the status bar. Asserts only on rendered
/// output and on-disk state.
#[test]
fn test_save_all_via_command_palette() -> anyhow::Result<()> {
    let mut harness = EditorTestHarness::with_temp_project(100, 24)?;
    let dir = harness.project_dir().unwrap();

    let a = dir.join("first.txt");
    let b = dir.join("second.txt");
    fs::write(&a, "one")?;
    fs::write(&b, "two")?;

    harness.open_file(&a)?;
    harness.type_text("X")?;
    harness.open_file(&b)?;
    harness.type_text("Y")?;

    // Open the command palette (Ctrl+P -> command mode), filter to Save All, run it.
    harness.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)?;
    harness.type_text("Save All")?;
    harness.render()?;
    harness.assert_screen_contains("Save All");

    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.render()?;

    // Status bar confirms two files were saved.
    let status = harness.get_status_bar();
    assert!(
        status.contains("Saved 2 files"),
        "status bar should report the save count, got: {status:?}"
    );

    // Both edits hit disk.
    assert!(
        fs::read_to_string(&a)?.contains('X'),
        "first.txt should be saved"
    );
    assert!(
        fs::read_to_string(&b)?.contains('Y'),
        "second.txt should be saved"
    );

    Ok(())
}
