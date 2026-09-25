//! A file changed on disk behind the editor's back is never silently
//! overwritten, and a change is noticed however its timestamp moved
//! (issue #3346).
//!
//! Plain `Ctrl+S` already asked before overwriting a newer file. These cover
//! the other ways a buffer reaches the disk — Save All, Save and Quit,
//! auto-save — and a replacement file whose mtime is *older* than the one the
//! editor recorded (`cp -p`, `rsync -t`, `tar x`, `mv` of an older file).

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::path::{Path, PathBuf};
use std::time::{Duration, SystemTime};

// Wide enough for the status bar to show the whole "Not saved" message.
const WIDTH: u16 = 160;
const HEIGHT: u16 = 30;

/// Another process rewrites `path`, stamping it clearly newer than anything
/// the editor could have recorded, whatever the filesystem's granularity.
fn change_externally(path: &Path, content: &str) {
    std::fs::write(path, content).unwrap();
    set_mtime(path, SystemTime::now() + Duration::from_secs(60));
}

/// Another process replaces `path` with a file carrying an *older* mtime, as
/// `cp -p` / `rsync -t` / `tar x` / `mv` of an older file do.
fn replace_with_older_file(path: &Path, content: &str) {
    let staged = path.with_file_name("staged-replacement");
    std::fs::write(&staged, content).unwrap();
    set_mtime(&staged, SystemTime::now() - Duration::from_secs(600));
    std::fs::rename(&staged, path).unwrap();
}

fn set_mtime(path: &Path, mtime: SystemTime) {
    std::fs::File::options()
        .write(true)
        .open(path)
        .unwrap()
        .set_times(std::fs::FileTimes::new().set_modified(mtime))
        .unwrap();
}

/// A harness with `names` opened, each edited so the buffer is dirty.
fn dirty_buffers(config: Config, names: &[&str]) -> (EditorTestHarness, Vec<PathBuf>) {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(WIDTH, HEIGHT, config).unwrap();
    let dir = harness.project_dir().unwrap();
    let mut files = Vec::new();
    for name in names {
        let file = dir.join(name);
        std::fs::write(&file, "orig1\norig2\n").unwrap();
        harness.open_file(&file).unwrap();
        harness.type_text("EDIT ").unwrap();
        files.push(file);
    }
    harness.render().unwrap();
    (harness, files)
}

fn run_command(harness: &mut EditorTestHarness, name: &str) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text(name).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains(name);
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

#[test]
fn save_all_leaves_a_file_changed_on_disk_alone() {
    let (mut harness, files) = dirty_buffers(Config::default(), &["mine.txt", "theirs.txt"]);
    let (mine, theirs) = (&files[0], &files[1]);
    change_externally(theirs, "orig1\norig2\nexternal\n");

    run_command(&mut harness, "Save All");

    assert_eq!(
        std::fs::read_to_string(theirs).unwrap(),
        "orig1\norig2\nexternal\n",
        "Save All must not overwrite a file that changed on disk"
    );
    assert_eq!(
        std::fs::read_to_string(mine).unwrap(),
        "EDIT orig1\norig2\n",
        "an unconflicted buffer is still saved"
    );
    let status = harness.get_status_bar();
    assert!(
        status.contains("Not saved") && status.contains("theirs.txt"),
        "the skipped file must be reported; status was {status:?}"
    );
}

#[test]
fn save_and_quit_stays_open_when_a_file_changed_on_disk() {
    let (mut harness, files) = dirty_buffers(Config::default(), &["notes.txt"]);
    change_externally(&files[0], "external\n");

    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("[ Save and Quit ]");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    assert_eq!(std::fs::read_to_string(&files[0]).unwrap(), "external\n");
    assert!(
        !harness.should_quit(),
        "quitting would drop the unsaved edits"
    );
    harness.assert_screen_contains("Not saved");
}

fn auto_save_without_hot_exit() -> Config {
    let mut config = Config::default();
    config.editor.auto_save_enabled = true;
    config.editor.hot_exit = false;
    config.editor.confirm_quit = false;
    config
}

/// With auto-save on, quitting doesn't ask about file-backed buffers: they
/// are saved on the way out. But that save skips a file changed on disk, and
/// without hot exit nothing else keeps the edits, so quitting unasked would
/// silently lose them. The quit must ask instead.
#[test]
fn quit_with_auto_save_asks_when_a_file_changed_on_disk() {
    let (mut harness, files) = dirty_buffers(auto_save_without_hot_exit(), &["notes.txt"]);
    change_externally(&files[0], "external\n");

    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    assert!(
        !harness.should_quit(),
        "quitting would silently drop the unsaved edits"
    );
    harness.assert_screen_contains("[ Save and Quit ]");
    assert_eq!(std::fs::read_to_string(&files[0]).unwrap(), "external\n");
}

/// The unchanged case still quits without asking: auto-save covers it.
#[test]
fn quit_with_auto_save_does_not_ask_when_nothing_changed_on_disk() {
    let (mut harness, _files) = dirty_buffers(auto_save_without_hot_exit(), &["notes.txt"]);

    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    assert!(harness.should_quit());
}

#[test]
fn save_on_close_keeps_the_buffer_when_its_file_changed_on_disk() {
    let (mut harness, files) = dirty_buffers(Config::default(), &["notes.txt"]);
    change_externally(&files[0], "external\n");

    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("[ Save ]");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    assert_eq!(std::fs::read_to_string(&files[0]).unwrap(), "external\n");
    // The buffer is still open with the edits, and the reason is on screen.
    harness.assert_screen_contains("EDIT orig1");
    harness.assert_screen_contains("Not saved");
}

#[test]
fn auto_save_skips_a_file_changed_on_disk() {
    let mut config = Config::default();
    config.editor.auto_save_enabled = true;
    config.editor.auto_save_interval_secs = 2;
    let (mut harness, files) = dirty_buffers(config, &["notes.txt"]);
    change_externally(&files[0], "external\n");

    harness.advance_time(Duration::from_secs(3));
    harness.tick_and_render().unwrap();

    assert_eq!(std::fs::read_to_string(&files[0]).unwrap(), "external\n");
    let status = harness.get_status_bar();
    assert!(
        status.contains("Not saved") && status.contains("notes.txt"),
        "status was {status:?}"
    );
}

#[test]
fn a_clean_buffer_reloads_when_replaced_by_an_older_file() {
    let mut harness = EditorTestHarness::with_temp_project(WIDTH, HEIGHT).unwrap();
    let file = harness.project_dir().unwrap().join("notes.txt");
    std::fs::write(&file, "orig1\norig2\n").unwrap();
    harness.open_file(&file).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("orig2");

    replace_with_older_file(&file, "replacement\n");

    harness
        .wait_until(|h| h.screen_to_string().contains("replacement"))
        .unwrap();
}

#[test]
fn saving_over_an_older_replacement_asks_first() {
    let (mut harness, files) = dirty_buffers(Config::default(), &["notes.txt"]);
    replace_with_older_file(&files[0], "replacement\n");

    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("File Changed on Disk");

    // Cancel is the armed button.
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    assert_eq!(std::fs::read_to_string(&files[0]).unwrap(), "replacement\n");
}
