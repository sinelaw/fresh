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
//! Determinism: the terminal's "shell" is a script of this test's own that
//! sets an OSC title and then waits for a sentinel file, so the name is read
//! from the title rather than from whatever the host's process table happens
//! to report, and the exit happens only once the test has seen the name on
//! the tab. A plain shell would race the title poll against the exit.
//!
//! Requires a working PTY (/dev/ptmx); skips when unavailable, like the other
//! terminal e2e tests.

use crate::common::harness::{layout, EditorTestHarness, HarnessOptions};
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
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o755)).unwrap();
    }
    path.to_string_lossy().into_owned()
}

fn tab_bar(harness: &EditorTestHarness) -> String {
    harness.screen_row_text(layout::TAB_BAR_ROW as u16)
}

/// An auto-named terminal that exited before the quit comes back with the
/// name it had, not the `*Terminal N*` default.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses a Unix shell script as the shell
fn an_exited_terminals_auto_derived_name_survives_a_restart() {
    if !pty_available() {
        eprintln!("Skipping terminal-name-restore test: PTY not available");
        return;
    }

    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();
    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // Sets the tab's name through an OSC title, then holds the terminal open
    // until the test has seen that name and asks it to quit.
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
        EditorTestHarness::create(
            120,
            30,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(dir.to_path_buf())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap()
    };

    // Session 1: the terminal names itself, then exits, then the editor quits.
    let before_restart = {
        let mut harness = harness_in(&project_dir);
        harness.startup(true, &[]).unwrap();
        harness
            .run_palette_command("Open Terminal to the Right")
            .unwrap();
        harness
            .wait_until(|h| tab_bar(h).contains("TITLE-ALPHA"))
            .expect("the terminal's OSC title should become its tab name");

        // Only now let it exit, so the name is on the tab before the exit
        // path captures it.
        std::fs::write(&sentinel, b"").unwrap();
        harness
            .wait_until(|h| tab_bar(h).contains("(exited)"))
            .expect("the terminal should exit and say so on its tab");

        let tabs = tab_bar(&harness);
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
        .wait_until(|h| tab_bar(h).contains("(exited)"))
        .unwrap_or_else(|_| {
            panic!(
                "the exited terminal should be restored.\nTabs: {}\nScreen:\n{}",
                tab_bar(&harness),
                harness.screen_to_string()
            )
        });

    let tabs = tab_bar(&harness);
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
#[cfg_attr(target_os = "windows", ignore)] // Uses a Unix shell script as the shell
fn two_exited_terminals_stay_distinguishable_across_a_restart() {
    if !pty_available() {
        eprintln!("Skipping terminal-name-restore test: PTY not available");
        return;
    }

    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();
    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // One script, two names: it takes its title from the first file that is
    // not yet claimed, so the two terminals name themselves differently
    // without the test having to configure two shells.
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
        EditorTestHarness::create(
            140,
            30,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(dir.to_path_buf())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap()
    };

    {
        let mut harness = harness_in(&project_dir);
        harness.startup(true, &[]).unwrap();
        harness
            .run_palette_command("Open Terminal to the Right")
            .unwrap();
        harness
            .wait_until(|h| tab_bar(h).contains("TITLE-ALPHA"))
            .expect("the first terminal should name itself");
        harness.run_palette_command("Open Terminal").unwrap();
        harness
            .wait_until(|h| tab_bar(h).contains("TITLE-BETA"))
            .expect("the second terminal should name itself differently");

        std::fs::write(&sentinel, b"").unwrap();
        harness
            .wait_until(|h| {
                let tabs = tab_bar(h);
                tabs.matches("(exited)").count() == 2
            })
            .expect("both terminals should exit");
        harness.shutdown(true).unwrap();
    }

    let mut harness = harness_in(&project_dir);
    harness.startup(true, &[]).unwrap();
    harness
        .wait_until(|h| tab_bar(h).matches("(exited)").count() == 2)
        .unwrap_or_else(|_| {
            panic!(
                "both exited terminals should be restored.\nTabs: {}",
                tab_bar(&harness)
            )
        });

    let tabs = tab_bar(&harness);
    assert!(
        tabs.contains("TITLE-ALPHA") && tabs.contains("TITLE-BETA"),
        "both restored terminals must keep their own names, or the two dead \
         transcripts cannot be told apart.\nTabs: {tabs}"
    );
}
