//! A terminal the user `cd`'d elsewhere comes back in *that* directory after
//! an editor restart, not in the directory it was first spawned in.
//!
//! Scenario: Fresh is open on `~/repos/fresh`; a second terminal is opened
//! and `cd ~/repos/debian-salsa`. Quit Fresh completely and start it again:
//! both terminals are restored, but the second one used to come back in
//! `~/repos/fresh`. Its tab name was wrong too, since a shell that puts
//! `user@host:dir` in its title derives the title from the wrong directory.
//!
//! The cause was in `capture_workspace`: it saved the handle's *spawn* cwd
//! (`TerminalHandle::cwd`), which never changes after the PTY starts, instead
//! of where the shell is now.
//!
//! `/bin/sh` is the terminal's shell, so there are no rc files and `cd` and
//! `echo` behave the same on every host. The target directory is a sibling
//! of the project, like the scenario above. Linux-only and skipped without a
//! PTY, like the other terminal-restore tests.
#![cfg(target_os = "linux")]

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::{Config, TerminalShellConfig};
use fresh::config_io::DirectoryContext;
use portable_pty::{native_pty_system, PtySize};
use tempfile::TempDir;

fn pty_available() -> bool {
    native_pty_system()
        .openpty(PtySize {
            rows: 1,
            cols: 1,
            pixel_width: 0,
            pixel_height: 0,
        })
        .is_ok()
}

fn harness_in(dir: &std::path::Path, dir_context: &DirectoryContext) -> EditorTestHarness {
    let mut config = Config::default();
    config.editor.hot_exit = true;
    config.terminal.shell = Some(TerminalShellConfig {
        command: "/bin/sh".to_string(),
        args: Vec::new(),
    });
    EditorTestHarness::create(
        200,
        30,
        HarnessOptions::new()
            .with_config(config)
            .with_working_dir(dir.to_path_buf())
            .with_shared_dir_context(dir_context.clone())
            .without_empty_plugins_dir(),
    )
    .unwrap()
}

fn run_in_terminal(harness: &mut EditorTestHarness, line: &str) {
    harness.type_text(line).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
}

#[test]
fn a_terminal_cd_elsewhere_is_restored_in_that_directory() {
    if !pty_available() {
        eprintln!("Skipping terminal-cwd-restore test: PTY not available");
        return;
    }

    let temp_dir = TempDir::new().unwrap();
    let root = temp_dir.path().canonicalize().unwrap();
    let project_dir = root.join("path-a");
    let other_dir = root.join("path-b");
    std::fs::create_dir(&project_dir).unwrap();
    std::fs::create_dir(&other_dir).unwrap();
    let dir_context = DirectoryContext::for_testing(&root);

    // Session 1: open a terminal in the project, cd somewhere else, quit.
    {
        let mut harness = harness_in(&project_dir, &dir_context);
        harness.startup(true, &[]).unwrap();
        harness.editor_mut().open_terminal();
        harness.render().unwrap();

        // The typed line shows `$((1+1))` literally, so `CD2` on screen means
        // the shell ran the line and the `cd` succeeded.
        run_in_terminal(
            &mut harness,
            &format!("cd '{}' && echo CD$((1+1))", other_dir.display()),
        );
        harness.wait_for_screen_contains("CD2").unwrap();

        harness.shutdown(true).unwrap();
    }

    // Session 2: the restored shell should start in path-b.
    let mut harness = harness_in(&project_dir, &dir_context);
    harness.startup(true, &[]).unwrap();
    harness.render().unwrap();

    // `$(pwd)` is not expanded in the echoed input line or in session 1's
    // transcript, so `AT:<dir>` can only come from the restored shell.
    run_in_terminal(&mut harness, "echo \"AT:$(pwd):END\"");
    let expected = format!("AT:{}:END", other_dir.display());
    let wrong = format!("AT:{}:END", project_dir.display());
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains(&expected) || s.contains(&wrong)
        })
        .unwrap_or_else(|e| {
            panic!(
                "the restored terminal never answered `pwd`: {e}\nScreen:\n{}",
                harness.screen_to_string()
            )
        });
    let screen = harness.screen_to_string();
    assert!(
        screen.contains(&expected),
        "the restored terminal should be in {} (where the shell had cd'd), \
         not back in the project root {}.\nScreen:\n{screen}",
        other_dir.display(),
        project_dir.display(),
    );
}
