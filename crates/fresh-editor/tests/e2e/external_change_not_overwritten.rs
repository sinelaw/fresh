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

/// Widen the screen so the file-change poll's "File <path> changed on disk
/// (buffer has unsaved changes)" fits whole in the status bar. It names the
/// file's full, canonical path, and the temp dirs on macOS
/// (`/private/var/folders/…`) and Windows (`C:\Users\…\AppData\Local\Temp`)
/// are long enough to cut it off at a fixed width, before the part a test
/// waits for.
fn fit_poll_message(harness: &mut EditorTestHarness, path: &Path) {
    let canonical = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
    let width = WIDTH.max(canonical.display().to_string().len() as u16 + 160);
    harness.resize(width, HEIGHT).unwrap();
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

/// Auto-save on, and `confirm_quit` to catch a stray `Ctrl+Q` (issue
/// #2030).
fn auto_save_with_confirm_quit() -> Config {
    let mut config = auto_save_without_hot_exit();
    config.editor.confirm_quit = true;
    config
}

/// The auto-save on quit ran before the "Quit?" confirmation, so a stray
/// `Ctrl+Q` the user then cancelled had already written their files.
/// Nothing may be written until the quit is confirmed.
#[test]
fn cancelled_quit_confirmation_writes_nothing_with_auto_save() {
    let (mut harness, files) = dirty_buffers(auto_save_with_confirm_quit(), &["notes.txt"]);

    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Quit Fresh?");
    assert_eq!(
        std::fs::read_to_string(&files[0]).unwrap(),
        "orig1\norig2\n",
        "nothing may be written while the quit is still being confirmed"
    );

    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    assert!(!harness.should_quit());
    assert_eq!(
        std::fs::read_to_string(&files[0]).unwrap(),
        "orig1\norig2\n",
        "a cancelled quit must not have saved anything"
    );
}

/// Confirming it saves and quits, as auto-save promises.
#[test]
fn confirmed_quit_auto_saves_and_quits() {
    let (mut harness, files) = dirty_buffers(auto_save_with_confirm_quit(), &["notes.txt"]);

    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Quit Fresh?");
    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    assert!(harness.should_quit());
    assert_eq!(
        std::fs::read_to_string(&files[0]).unwrap(),
        "EDIT orig1\norig2\n"
    );
}

/// A file changed on disk, which the auto-save would leave alone, is known
/// before anything is written: the quit asks about it straight away rather
/// than confirming a quit it would then have to interrupt.
#[test]
fn quit_confirmation_with_auto_save_asks_first_about_a_file_changed_on_disk() {
    let (mut harness, files) = dirty_buffers(auto_save_with_confirm_quit(), &["notes.txt"]);
    change_externally(&files[0], "external\n");

    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    assert!(!harness.should_quit());
    harness.assert_screen_contains("[ Save and Quit ]");
    assert_eq!(std::fs::read_to_string(&files[0]).unwrap(), "external\n");
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

/// Auto-save reports a skipped file once, not every interval: repeating it
/// kept overwriting whatever the status bar showed since.
#[test]
fn auto_save_reports_a_file_changed_on_disk_once() {
    let mut config = Config::default();
    config.editor.auto_save_enabled = true;
    config.editor.auto_save_interval_secs = 2;
    // Keep the file-change poll, which has its own message, out of the way.
    config.editor.auto_revert_poll_interval_ms = 3_600_000;
    let (mut harness, files) = dirty_buffers(config, &["notes.txt"]);
    change_externally(&files[0], "external\n");
    harness.advance_time(Duration::from_secs(3));
    harness.tick_and_render().unwrap();
    assert!(harness.get_status_bar().contains("Not saved"));

    harness
        .editor_mut()
        .set_status_message("something else".to_string());
    harness.advance_time(Duration::from_secs(3));
    harness.tick_and_render().unwrap();

    let status = harness.get_status_bar();
    assert!(
        status.contains("something else") && !status.contains("Not saved"),
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

/// **The file-change poll reports a change once, not on every poll**
/// (issue #3403). It re-found the same changed file every couple of seconds
/// and rewrote "changed on disk" into the status bar each time, so any other
/// message — here Save All's own report — was gone moments after it
/// appeared.
#[test]
fn the_file_change_poll_reports_a_modified_buffers_change_once() {
    let (mut harness, files) = dirty_buffers(Config::default(), &["notes.txt"]);
    fit_poll_message(&mut harness, &files[0]);
    // A clean file whose reload shows on screen when a later poll has run:
    // in a split of its own, since reloading the active buffer has a status
    // message of its own.
    let other = harness.project_dir().unwrap().join("other.txt");
    std::fs::write(&other, "other\n").unwrap();
    harness.open_file(&other).unwrap();
    run_command(&mut harness, "Split Vertical");
    harness.open_file(&files[0]).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("EDIT orig1");
    harness.assert_screen_contains("other");

    change_externally(&files[0], "external\n");
    harness
        .wait_until(|h| {
            h.get_status_bar()
                .contains("changed on disk (buffer has unsaved")
        })
        .unwrap();

    run_command(&mut harness, "Save All");
    assert!(harness
        .get_status_bar()
        .contains("Not saved, changed on disk: notes.txt"));

    // A later poll: it reloads other.txt, and sees notes.txt still changed.
    change_externally(&other, "reloaded\n");
    harness
        .wait_until(|h| h.screen_to_string().contains("reloaded"))
        .unwrap();

    let status = harness.get_status_bar();
    assert!(
        status.contains("Not saved, changed on disk: notes.txt"),
        "the poll must not repeat its message over Save All's; status was {status:?}"
    );
}

/// **What the poll has reported goes with the buffer** (issue #3403): a
/// buffer closed and opened again is told about a change to its file even
/// when the change carries the same timestamps as one reported to the
/// buffer before it. The report used to be kept per file in the window,
/// outliving the buffer, and silenced the new one.
#[test]
fn a_reopened_buffer_is_told_of_a_change_its_predecessor_was_told_of() {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(WIDTH, HEIGHT, Config::default()).unwrap();
    let file = &harness.project_dir().unwrap().join("notes.txt");
    std::fs::write(file, "").unwrap();
    fit_poll_message(&mut harness, file);
    let before = SystemTime::now() - Duration::from_secs(600);
    let after = SystemTime::now() + Duration::from_secs(600);
    let reported = |h: &EditorTestHarness| {
        h.get_status_bar()
            .contains("changed on disk (buffer has unsaved")
    };
    // The same change twice: `orig` at `before` becomes `external` at `after`.
    let change = |file: &Path| {
        std::fs::write(file, "external\n").unwrap();
        set_mtime(file, after);
    };

    // The buffer recorded `before` when it was opened, and is told once
    // the file changes.
    // Another file, for the editor to show once notes.txt is closed.
    let other = harness.project_dir().unwrap().join("other.txt");
    std::fs::write(&other, "other\n").unwrap();
    harness.open_file(&other).unwrap();
    std::fs::write(file, "orig1\norig2\n").unwrap();
    set_mtime(file, before);
    harness.open_file(file).unwrap();
    harness.type_text("EDIT ").unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("EDIT orig1");
    change(file);
    harness.wait_until(reported).unwrap();

    // Closed without saving, the file put back as it was, and opened and
    // edited again.
    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Discard");
    harness
        .send_key(KeyCode::Char('d'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_not_contains("EDIT orig1");
    harness.assert_screen_contains("other");
    std::fs::write(file, "orig1\norig2\n").unwrap();
    set_mtime(file, before);
    harness.open_file(file).unwrap();
    harness.type_text("AGAIN ").unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("AGAIN");
    assert!(!reported(&harness));

    change(file);
    harness.wait_until(reported).unwrap();
}
