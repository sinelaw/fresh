//! An exited terminal's tab name survives an editor restart (#3074).
//!
//! `docs/features/terminal.md` and the 0.4.6 CHANGELOG both promise that a
//! terminal's tab name is preserved across restarts. It was — for a terminal
//! that was *live* at the quit, which comes back with its process and
//! re-derives the name from it, and for an explicitly-titled one, whose title
//! is persisted. An **auto-named** terminal that had already exited came back
//! as `*Terminal N*`: auto-naming reads the foreground process, a dead
//! terminal has none, and nothing had been saved to fall back on. With more
//! than one of them the restored tabs were also indistinguishable, since they
//! all fall back to the same base name.
//!
//! The name is now persisted as `SerializedTerminalWorkspace::auto_title` and
//! re-applied on restore *without* marking the tab explicitly titled, so
//! restarting the terminal still hands naming back to the reborn process.
//!
//! Two things make these deterministic rather than dependent on the host:
//!
//! * The terminal's "shell" is a script of this test's own that sets an OSC
//!   title and then waits for a sentinel file. The name therefore comes from
//!   that title, and the exit happens only once the test has seen the name on
//!   the tab — a plain shell would race the title poll against the exit.
//! * Auto-naming is turned back **on after construction**: the harness forces
//!   `terminal_auto_title` off so other tests get static `*Terminal N*` tabs,
//!   and it overrides whatever a constructor-time config asked for. This is
//!   the same opt-in `terminal::test_terminal_tab_title_follows_foreground_process`
//!   makes.
//!
//! Linux-only and PTY-gated, like the other terminal tests here: the
//! foreground-process half of a tab's name is read through `tcgetpgrp` and
//! `/proc`, which other platforms do not implement.
#![cfg(target_os = "linux")]

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use fresh::config::Config;
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

fn write_script(dir: &std::path::Path, name: &str, body: &str) -> String {
    let path = dir.join(name);
    std::fs::write(&path, body).unwrap();
    use std::os::unix::fs::PermissionsExt;
    std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o755)).unwrap();
    path.to_string_lossy().into_owned()
}

/// Turn tmux-style auto-naming back on for this harness.
///
/// It has to happen *after* construction: the harness sets
/// `terminal_auto_title = false` on the config it builds, whatever was passed
/// in, so that tabs elsewhere stay on their static `*Terminal N*` names.
fn enable_auto_title(harness: &mut EditorTestHarness) {
    let mut config = harness.editor().config().clone();
    config.editor.terminal_auto_title = true;
    harness.editor_mut().set_config(config);
}

/// Wait for the terminal tab to carry `name`, failing fast — with the screen
/// — if the terminal exits first instead.
///
/// The scripts below hold their terminal open until the test releases it, so
/// an exit here means the name is never coming, and waiting out the full
/// deadline would cost two minutes to say so.
fn wait_for_tab_name(harness: &mut EditorTestHarness, name: &str) {
    harness
        .wait_until(|h| {
            let tabs = h.get_tab_bar();
            tabs.contains(name) || tabs.contains("(exited)")
        })
        .unwrap_or_else(|e| panic!("waiting for the tab to read {name:?}: {e}"));
    let tabs = harness.get_tab_bar();
    assert!(
        tabs.contains(name),
        "the terminal should have named itself {name:?} from its OSC title, \
         but it exited first.\nTabs: {tabs}\nScreen:\n{}",
        harness.screen_to_string()
    );
}

/// An auto-named terminal that exited before the quit comes back with the
/// name it had, not the `*Terminal N*` default.
#[test]
fn an_exited_terminals_auto_derived_name_survives_a_restart() {
    if !pty_available() {
        eprintln!("Skipping terminal-name-restore test: PTY not available");
        return;
    }

    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();
    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // Names the tab through an OSC title, then holds the terminal open until
    // the test has seen that name and asks it to quit.
    let sentinel = temp_dir.path().join("quit-now");
    let script = write_script(
        temp_dir.path(),
        "titled.sh",
        &format!(
            "#!/bin/sh\n\
             printf '\\033]0;TITLE-ALPHA\\007'\n\
             while [ ! -f '{s}' ]; do sleep 0.1; done\n\
             exit 0\n",
            s = sentinel.display()
        ),
    );

    let harness_in = |dir: &std::path::Path| {
        let mut config = Config::default();
        config.editor.hot_exit = true;
        config.terminal.jump_to_end_on_output = false;
        // The terminal's shell, so "Open Terminal…" spawns this and the tab
        // is auto-named — a plugin-supplied command would make it an
        // *explicitly* titled tab, which is the case that already worked.
        config.terminal.shell = Some(fresh::config::TerminalShellConfig {
            command: script.clone(),
            args: Vec::new(),
        });
        let mut harness = EditorTestHarness::create(
            120,
            30,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(dir.to_path_buf())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();
        enable_auto_title(&mut harness);
        harness
    };

    // Session 1: the terminal names itself, then exits, then the editor quits.
    let before_restart = {
        let mut harness = harness_in(&project_dir);
        harness.startup(true, &[]).unwrap();
        harness
            .run_palette_command("Open Terminal to the Right")
            .unwrap();
        wait_for_tab_name(&mut harness, "TITLE-ALPHA");

        // Only now let it exit, so the name is on the tab before the exit
        // path captures it.
        std::fs::write(&sentinel, b"").unwrap();
        harness
            .wait_until(|h| h.get_tab_bar().contains("(exited)"))
            .expect("the terminal should exit and say so on its tab");

        let tabs = harness.get_tab_bar();
        assert!(
            tabs.contains("TITLE-ALPHA"),
            "an exited tab keeps its name within the session.\nTabs: {tabs}"
        );
        harness.shutdown(true).unwrap();
        tabs
    };

    // Session 2: the restored dead terminal reads as it did before.
    let mut harness = harness_in(&project_dir);
    harness.startup(true, &[]).unwrap();
    harness.render().unwrap();
    // The restore reopens the pane from its backing file; wait for the tab
    // rather than assuming it is there on the first frame.
    harness
        .wait_until(|h| h.get_tab_bar().contains("(exited)"))
        .expect("the exited terminal should be restored");

    let tabs = harness.get_tab_bar();
    assert!(
        tabs.contains("TITLE-ALPHA"),
        "a restored exited terminal must keep its name.\n\
         before the restart: {before_restart}\n\
         after the restart:  {tabs}"
    );
    assert!(
        !tabs.contains("*Terminal"),
        "a restored exited terminal must not fall back to the `*Terminal N*` \
         default.\nTabs: {tabs}"
    );
}

/// Two auto-named terminals that exited stay *distinguishable* across a
/// restart. This is the part of #3074 that made the loss more than cosmetic:
/// both tabs fell back to the same base name, so which transcript was which
/// was unrecoverable from the tab bar.
#[test]
fn two_exited_terminals_stay_distinguishable_across_a_restart() {
    if !pty_available() {
        eprintln!("Skipping terminal-name-restore test: PTY not available");
        return;
    }

    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();
    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // One script, two names: it takes its title from whether the first run
    // has claimed its name yet, so the two terminals name themselves
    // differently without the test having to configure two shells. The test
    // waits for the first name before opening the second terminal, so the
    // order is not a race.
    let sentinel = temp_dir.path().join("quit-now");
    let claimed = temp_dir.path().join("claimed");
    let script = write_script(
        temp_dir.path(),
        "titled.sh",
        &format!(
            "#!/bin/sh\n\
             if [ -f '{c}' ]; then name=TITLE-BETA; else name=TITLE-ALPHA; touch '{c}'; fi\n\
             printf '\\033]0;%s\\007' \"$name\"\n\
             while [ ! -f '{s}' ]; do sleep 0.1; done\n\
             exit 0\n",
            c = claimed.display(),
            s = sentinel.display()
        ),
    );

    let harness_in = |dir: &std::path::Path| {
        let mut config = Config::default();
        config.editor.hot_exit = true;
        config.terminal.jump_to_end_on_output = false;
        config.terminal.shell = Some(fresh::config::TerminalShellConfig {
            command: script.clone(),
            args: Vec::new(),
        });
        let mut harness = EditorTestHarness::create(
            140,
            30,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(dir.to_path_buf())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();
        enable_auto_title(&mut harness);
        harness
    };

    {
        let mut harness = harness_in(&project_dir);
        harness.startup(true, &[]).unwrap();
        harness
            .run_palette_command("Open Terminal to the Right")
            .unwrap();
        wait_for_tab_name(&mut harness, "TITLE-ALPHA");
        harness.run_palette_command("Open Terminal").unwrap();
        wait_for_tab_name(&mut harness, "TITLE-BETA");

        std::fs::write(&sentinel, b"").unwrap();
        harness
            .wait_until(|h| h.get_tab_bar().matches("(exited)").count() == 2)
            .expect("both terminals should exit");
        harness.shutdown(true).unwrap();
    }

    let mut harness = harness_in(&project_dir);
    harness.startup(true, &[]).unwrap();
    harness
        .wait_until(|h| h.get_tab_bar().matches("(exited)").count() == 2)
        .expect("both exited terminals should be restored");

    let tabs = harness.get_tab_bar();
    assert!(
        tabs.contains("TITLE-ALPHA") && tabs.contains("TITLE-BETA"),
        "both restored terminals must keep their own names, or the two dead \
         transcripts cannot be told apart.\nTabs: {tabs}"
    );
}
