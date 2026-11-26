//! Fresh as an OSC 7577 ("Program Resume") terminal: a program running in
//! one of its terminals reports how to resume it, and when the workspace is
//! restored that terminal comes back running the program instead of a bare
//! shell — in the directory it reported — and returns to the shell once the
//! program quits.
//!
//! The "program" is a shell script that does what the protocol asks of a real
//! one (`nvim`, `lazygit`): it signs `pid=…:time=…:argv=…:cwd=…` with the
//! `OSC7577_TOKEN` its terminal handed it, writes the sequence, and keeps
//! running. A report signed with any other token must be ignored, so a program
//! that merely *prints* such a sequence (a `cat` of a log, a remote host) can
//! never plant a command.
//!
//! The terminal's shell is a wrapper that counts its starts on screen
//! (`SHELL-START-<n>`), so a restored terminal that came back as a shell, and
//! the shell returning after the resumed program, are both visible. Linux-only
//! and skipped without a PTY, like the other terminal-restore tests; the
//! scripts need `base64` and `sha256sum` (coreutils).
#![cfg(target_os = "linux")]

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::{Config, TerminalShellConfig};
use fresh::config_io::DirectoryContext;
use portable_pty::{native_pty_system, PtySize};
use std::path::{Path, PathBuf};
use tempfile::TempDir;

/// Whether these tests can say anything here: they need a PTY, and an
/// editor not running as root — as root it resumes nothing on purpose (the
/// spec asks for an explicit opt-in), so every outcome would be "a shell".
fn can_run() -> bool {
    if !pty_available() {
        eprintln!("Skipping terminal-program-resume test: PTY not available");
        return false;
    }
    // SAFETY: `geteuid` has no preconditions and cannot fail.
    if unsafe { libc::geteuid() } == 0 {
        eprintln!("Skipping terminal-program-resume test: running as root");
        return false;
    }
    true
}

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

/// The test's directories and scripts.
struct Fixture {
    _temp: TempDir,
    root: PathBuf,
    project: PathBuf,
    /// Where the reported program asks to run — not the project, so the
    /// restored terminal's directory proves the report's `cwd` was used.
    elsewhere: PathBuf,
    dir_context: DirectoryContext,
}

impl Fixture {
    fn new() -> Self {
        let temp = TempDir::new().unwrap();
        let root = temp.path().canonicalize().unwrap();
        let project = root.join("project");
        let elsewhere = root.join("elsewhere");
        std::fs::create_dir(&project).unwrap();
        std::fs::create_dir(&elsewhere).unwrap();
        let dir_context = DirectoryContext::for_testing(&root);

        // The terminal's shell: count the start, show it, become `/bin/sh`.
        std::fs::write(
            root.join("shell.sh"),
            format!(
                "n=$(cat '{count}' 2>/dev/null || echo 0)\n\
                 n=$((n+1))\n\
                 echo $n > '{count}'\n\
                 echo SHELL-START-$n\n\
                 exec /bin/sh\n",
                count = root.join("shell-starts").display()
            ),
        )
        .unwrap();

        // The program a restore should bring back, numbering its runs like
        // the shell does: `RESUMED-AT:<dir>:RUN-<n>`.
        std::fs::write(
            root.join("resumed.sh"),
            format!(
                "n=$(cat '{count}' 2>/dev/null || echo 0)\n\
                 n=$((n+1))\n\
                 echo $n > '{count}'\n\
                 echo \"RESUMED-AT:$(pwd):RUN-$n\"\n\
                 read line\n",
                count = root.join("resumed-runs").display()
            ),
        )
        .unwrap();

        // A program that reports `<$2> resumed.sh` in `elsewhere`, signed
        // with `$1`, then stays running — a record whose owner has exited is
        // dropped, so it has to be alive when the workspace is saved.
        std::fs::write(
            root.join("report.sh"),
            format!(
                "b() {{ printf '%s' \"$1\" | base64 | tr -d '\\n'; }}\n\
                 body=\"pid=$$:time=$(date +%s):argv=$(b \"$2\"),$(b '{resumed}'):cwd=$(b '{cwd}')\"\n\
                 mac=$(printf '%s:%s' \"$body\" \"$1\" | sha256sum | cut -c1-64)\n\
                 printf '\\033]7577;%s:mac=%s\\033\\\\' \"$body\" \"$mac\"\n\
                 echo REPORTED-$((40+2))\n\
                 exec sleep 600\n",
                resumed = root.join("resumed.sh").display(),
                cwd = elsewhere.display(),
            ),
        )
        .unwrap();

        Self {
            _temp: temp,
            root,
            project,
            elsewhere,
            dir_context,
        }
    }

    fn harness(&self, resume_programs: bool) -> EditorTestHarness {
        let mut config = Config::default();
        config.editor.hot_exit = true;
        config.terminal.resume_programs = resume_programs;
        config.terminal.shell = Some(TerminalShellConfig {
            command: "/bin/sh".to_string(),
            args: vec![self.root.join("shell.sh").display().to_string()],
        });
        EditorTestHarness::create(
            200,
            30,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(self.project.clone())
                .with_shared_dir_context(self.dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap()
    }

    /// Session 1: open a terminal, run the reporting program with `token`
    /// (a shell expression) reporting `program` to run `resumed.sh`, and quit
    /// while it is still running.
    fn report_and_quit(&self, token: &str, program: &str) {
        let mut harness = self.harness(true);
        harness.startup(true, &[]).unwrap();
        harness.editor_mut().open_terminal();
        harness.render().unwrap();
        harness.wait_for_screen_contains("SHELL-START-1").unwrap();
        run_in_terminal(
            &mut harness,
            &format!(
                "sh '{}' {token} {program}",
                self.root.join("report.sh").display()
            ),
        );
        harness.wait_for_screen_contains("REPORTED-42").unwrap();
        harness.shutdown(true).unwrap();
    }
}

fn run_in_terminal(harness: &mut EditorTestHarness, line: &str) {
    harness.type_text(line).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
}

/// What the resumed program prints on its `run`-th start.
fn resumed_marker(dir: &Path, run: u32) -> String {
    format!("RESUMED-AT:{}:RUN-{run}", dir.display())
}

/// Wait for whichever of the two restore outcomes shows first: the resumed
/// program, or a fresh shell in its place.
fn wait_for_restore(harness: &mut EditorTestHarness, resumed: &str) -> String {
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains(resumed) || s.contains("SHELL-START-2")
        })
        .unwrap_or_else(|e| {
            panic!(
                "the restored terminal ran neither the program nor the shell: {e}\nScreen:\n{}",
                harness.screen_to_string()
            )
        });
    harness.screen_to_string()
}

/// The protocol's purpose, end to end: the reported program comes back, in
/// its directory, and when it quits the terminal is its shell again.
#[test]
fn a_reported_program_is_resumed_and_the_shell_returns_after_it() {
    if !can_run() {
        return;
    }
    let fx = Fixture::new();
    fx.report_and_quit("\"$OSC7577_TOKEN\"", "/bin/sh");

    // Session 2: the program comes back, in its directory.
    let resumed = resumed_marker(&fx.elsewhere, 1);
    {
        let mut harness = fx.harness(true);
        harness.startup(true, &[]).unwrap();
        harness.render().unwrap();
        let screen = wait_for_restore(&mut harness, &resumed);
        assert!(
            screen.contains(&resumed),
            "the restored terminal should run the program its last program reported \
             (in {}), not a bare shell.\nScreen:\n{screen}",
            fx.elsewhere.display()
        );
        // Quit the editor while the resumed program still runs. It never
        // reports for itself, but the record it was resumed from is now its
        // own, so the next restore brings it back again.
        harness.shutdown(true).unwrap();
    }

    // Session 3: back once more, as its second run.
    let resumed = resumed_marker(&fx.elsewhere, 2);
    let mut harness = fx.harness(true);
    harness.startup(true, &[]).unwrap();
    harness.render().unwrap();
    let screen = wait_for_restore(&mut harness, &resumed);
    assert!(
        screen.contains(&resumed),
        "a resumed program keeps its record across another restart.\nScreen:\n{screen}"
    );

    // Quit the resumed program: the shell it replaced comes back.
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_screen_contains("SHELL-START-2").unwrap();
    run_in_terminal(&mut harness, "echo BACK$((1+2))");
    harness.wait_for_screen_contains("BACK3").unwrap();
}

/// A report signed with anything but the terminal's own token is ignored, so
/// the terminal comes back as the shell it was.
#[test]
fn a_report_with_the_wrong_token_is_ignored() {
    if !can_run() {
        return;
    }
    let fx = Fixture::new();
    fx.report_and_quit("not-the-token-this-terminal-gave", "/bin/sh");

    let mut harness = fx.harness(true);
    harness.startup(true, &[]).unwrap();
    harness.render().unwrap();
    let resumed = resumed_marker(&fx.elsewhere, 1);
    let screen = wait_for_restore(&mut harness, &resumed);
    assert!(
        !screen.contains(&resumed),
        "a forged report must not decide what a restored terminal runs.\nScreen:\n{screen}"
    );
}

/// `terminal.resume_programs` off: the report is kept but not acted on.
#[test]
fn resume_programs_off_restores_the_shell() {
    if !can_run() {
        return;
    }
    let fx = Fixture::new();
    fx.report_and_quit("\"$OSC7577_TOKEN\"", "/bin/sh");

    let mut harness = fx.harness(false);
    harness.startup(true, &[]).unwrap();
    harness.render().unwrap();
    let resumed = resumed_marker(&fx.elsewhere, 1);
    let screen = wait_for_restore(&mut harness, &resumed);
    assert!(
        !screen.contains(&resumed),
        "with terminal.resume_programs off the shell should come back.\nScreen:\n{screen}"
    );
}

/// A reported program that can't be started any more (uninstalled, or only
/// on a `PATH` the shell's startup files set) leaves the terminal as the
/// shell it was — not missing from the restored workspace.
#[test]
fn a_program_that_cannot_start_leaves_the_shell() {
    if !can_run() {
        return;
    }
    let fx = Fixture::new();
    fx.report_and_quit("\"$OSC7577_TOKEN\"", "/nonexistent/program-resume-test");

    let mut harness = fx.harness(true);
    harness.startup(true, &[]).unwrap();
    harness.render().unwrap();
    harness.wait_for_screen_contains("SHELL-START-2").unwrap();
    run_in_terminal(&mut harness, "echo BACK$((1+2))");
    harness.wait_for_screen_contains("BACK3").unwrap();
}
