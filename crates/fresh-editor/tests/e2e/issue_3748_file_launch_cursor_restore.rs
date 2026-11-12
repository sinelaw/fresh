//! Regression coverage for issue #3748: `fresh FILE` never remembered where
//! the cursor was. The per-file cursor/scroll state
//! (`PersistedFileWorkspace`) was only flushed on quit from inside the
//! workspace save, and a file launch — like `--no-restore` — skips that save
//! wholesale, so quitting with the tab still open wrote nothing and every
//! reopen started at line 1.
//!
//! `shutdown(false)` is the quit a file launch makes: `main.rs` passes
//! `workspace_enabled = !no_session && !file_launch` to
//! `Editor::persist_on_exit`. Observed the way the user does — the status
//! bar's `Ln` readout after relaunching on the same file.

use crate::common::global_state::{isolated_dir_context, DataDirPin};
use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config_io::DirectoryContext;
use std::fs;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

/// A project with `debian/patches/series` listing 80 patches, and a data
/// dir private to this test (where `file_states/` lives).
struct Project {
    _temp: TempDir,
    _data_pin: DataDirPin,
    dir_context: DirectoryContext,
    root: PathBuf,
    series: PathBuf,
}

fn project() -> Project {
    let temp = TempDir::new().unwrap();
    let base = temp.path().canonicalize().unwrap();
    let (dir_context, data_pin) = isolated_dir_context(&base);
    let root = base.join("proj");
    let patches = root.join("debian").join("patches");
    fs::create_dir_all(&patches).unwrap();
    let series = patches.join("series");
    let content: String = (1..=80).map(|n| format!("patch-{n}.diff\n")).collect();
    fs::write(&series, content).unwrap();
    Project {
        _temp: temp,
        _data_pin: data_pin,
        dir_context,
        root,
        series,
    }
}

fn launch(p: &Project) -> EditorTestHarness {
    EditorTestHarness::create(
        120,
        24,
        HarnessOptions::new()
            .with_working_dir(p.root.clone())
            .with_shared_dir_context(p.dir_context.clone()),
    )
    .unwrap()
}

/// Move from line 1 down to `line`, and check the status bar agrees.
fn go_to_line(h: &mut EditorTestHarness, line: usize) {
    for _ in 1..line {
        h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    h.render().unwrap();
    h.assert_screen_contains(&format!("Ln {line},"));
}

fn assert_on_line(h: &mut EditorTestHarness, line: usize, why: &str) {
    h.render().unwrap();
    let status = h.get_status_bar();
    assert!(
        status.contains(&format!("Ln {line},")),
        "{why}: expected Ln {line}, status bar was: {status:?}\nScreen:\n{}",
        h.screen_to_string()
    );
}

/// The reporter's flow: `fresh debian/patches/series`, comment out a patch
/// partway down, save, quit with the tab still open — and the next
/// `fresh debian/patches/series` opens on that line.
#[test]
fn file_launch_reopens_where_the_cursor_was_left() {
    let p = project();
    {
        let mut h = launch(&p);
        h.startup(false, std::slice::from_ref(&p.series)).unwrap();
        go_to_line(&mut h, 50);
        h.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
        h.type_text("# ").unwrap();
        h.send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
            .unwrap();
        h.render().unwrap();
        h.assert_screen_contains("# patch-50.diff");
        h.shutdown(false).unwrap();
    }

    let mut h = launch(&p);
    h.startup(false, std::slice::from_ref(&p.series)).unwrap();
    assert_on_line(&mut h, 50, "a file launch must reopen on the line it quit on");
    h.assert_screen_contains("# patch-50.diff");
}

/// `--no-restore` keeps no workspace, but per-file state is not a workspace
/// file: a file opened in such a session and left open at quit still
/// reopens where it was.
#[test]
fn no_restore_session_still_remembers_the_cursor() {
    let p = project();
    {
        let mut h = launch(&p);
        h.editor_mut().set_workspace_persistence(false);
        h.startup(false, &[]).unwrap();
        h.open_file(&p.series).unwrap();
        go_to_line(&mut h, 30);
        h.shutdown(false).unwrap();
    }

    let mut h = launch(&p);
    h.startup(false, std::slice::from_ref(&p.series)).unwrap();
    assert_on_line(&mut h, 30, "a --no-restore session must still save per-file state");
}

/// Files rewritten behind the editor's back (`.git/COMMIT_EDITMSG`, #2761)
/// stay excluded: quitting a `fresh .git/COMMIT_EDITMSG` launch with the tab
/// open must not make the next one open off line 1.
#[test]
fn file_launch_quit_still_skips_ephemeral_files() {
    let p = project();
    let msg = git_commit_editmsg(&p.root, "subject\nbody one\nbody two\n");
    {
        let mut h = launch(&p);
        h.startup(false, std::slice::from_ref(&msg)).unwrap();
        go_to_line(&mut h, 3);
        h.shutdown(false).unwrap();
    }

    let mut h = launch(&p);
    h.startup(false, std::slice::from_ref(&msg)).unwrap();
    assert_on_line(&mut h, 1, ".git/COMMIT_EDITMSG must not have its cursor restored");
}

fn git_commit_editmsg(root: &Path, content: &str) -> PathBuf {
    let git = root.join(".git");
    fs::create_dir_all(&git).unwrap();
    let msg = git.join("COMMIT_EDITMSG");
    fs::write(&msg, content).unwrap();
    msg
}
