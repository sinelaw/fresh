//! Fresh as an OSC 7577 ("Program Resume") program: run in a terminal that
//! offers the protocol (it sets `OSC7577_TOKEN`), fresh tells the terminal the
//! command that brings this launch back after the terminal restarts, signed
//! with that token, and withdraws it (`clear`) when the user quits.
//!
//! Driven through the real binary on a pty, reading the raw bytes it writes:
//! a report is an escape sequence addressed to the terminal, so it never
//! shows on the screen. Each report is checked with the same verifier fresh's
//! own terminals use, so a MAC, time or encoding mistake fails here.
//!
//! Linux-gated like the other binary-driving tests: `XDG_*` isolation of the
//! config, state and socket trees works there.
#![cfg(target_os = "linux")]

use crate::common::pty::{pty_available, spawn_on_pty, ChildStdin, PtyChild};
use fresh::services::program_resume::{check_report, unix_now, ResumeCommand, Verdict};
use std::path::{Path, PathBuf};
use std::process::Command;

/// The token "the terminal" (this test) hands fresh.
const TOKEN: &str = "test-token-0123456789abcdef";

const CTRL_Q: &[u8] = &[0x11];

/// The menu bar, i.e. "the editor has drawn".
const MENU_BAR: &str = "Help";

/// `fresh` confined to `home`, in `home/project`, with the protocol on.
fn isolated_fresh(home: &Path) -> Command {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_fresh"));
    cmd.current_dir(home.join("project"))
        .env("HOME", home)
        .env("TMPDIR", home)
        .env("XDG_CONFIG_HOME", home.join("config"))
        .env("XDG_DATA_HOME", home.join("data"))
        .env("XDG_STATE_HOME", home.join("state"))
        .env("XDG_CACHE_HOME", home.join("cache"))
        .env("XDG_RUNTIME_DIR", home.join("run"))
        .env("TERM", "xterm-256color")
        .env("LANG", "C.UTF-8")
        .env("SHELL", "/bin/sh")
        .env("OSC7577_TOKEN", TOKEN)
        .env_remove("FRESH_SESSION")
        .env_remove("FRESH_BIN");
    cmd
}

fn setup(home: &Path) -> PathBuf {
    let project = home.join("project");
    std::fs::create_dir_all(&project).unwrap();
    std::fs::create_dir_all(home.join("run")).unwrap();
    let config_dir = home.join("config").join("fresh");
    std::fs::create_dir_all(&config_dir).unwrap();
    std::fs::write(
        config_dir.join("config.json"),
        "{\n  \"check_for_updates\": false\n}\n",
    )
    .unwrap();
    project.canonicalize().unwrap()
}

/// Every OSC 7577 report in `output`, verified against [`TOKEN`], in order.
fn reports(output: &[u8]) -> Vec<(u32, Verdict)> {
    let text = String::from_utf8_lossy(output);
    text.split("\x1b]7577;")
        .skip(1)
        .filter_map(|rest| rest.split_once("\x1b\\").map(|(payload, _)| payload))
        .map(|payload| {
            let checked = check_report(payload, TOKEN, unix_now())
                .unwrap_or_else(|why| panic!("fresh sent a report a terminal ignores ({why:?}): {payload}"));
            (checked.pid, checked.verdict)
        })
        .collect()
}

fn wait_for_reports(client: &mut PtyChild, n: usize) -> Vec<(u32, Verdict)> {
    client
        .wait_for_output(|out| reports(out).len() >= n)
        .unwrap_or_else(|e| panic!("fresh never sent {n} OSC 7577 report(s): {e}"));
    reports(client.output())
}

fn binary() -> String {
    std::fs::canonicalize(env!("CARGO_BIN_EXE_fresh"))
        .unwrap()
        .display()
        .to_string()
}

fn resume(argv: &[&str], cwd: &Path) -> Verdict {
    Verdict::Resume(ResumeCommand {
        argv: argv.iter().map(|s| s.to_string()).collect(),
        cwd: Some(cwd.to_path_buf()),
    })
}

/// A workspace launch reports `fresh --restore` in its directory — forced,
/// so it reopens the workspace whatever `restore_previous_session` says —
/// carrying the flags that shaped it, and a deliberate quit clears it.
#[test]
fn a_workspace_launch_reports_restore_and_clears_on_quit() {
    if !pty_available() {
        eprintln!("Skipping: no PTY available in this environment");
        return;
    }
    let home = tempfile::tempdir().unwrap();
    let project = setup(home.path());
    let mut cmd = isolated_fresh(home.path());
    cmd.args(["--no-init", "--no-upgrade-check"]);
    let mut client = spawn_on_pty(cmd, ChildStdin::Terminal, 120, 30).expect("spawn fresh");
    client
        .wait_for_screen(|s| s.contains(MENU_BAR))
        .expect("the editor should draw");

    let sent = wait_for_reports(&mut client, 1);
    assert_eq!(sent[0].0, client.pid(), "the report names fresh's own pid");
    assert_eq!(
        sent[0].1,
        resume(&[&binary(), "--no-init", "--restore"], &project)
    );

    client.send(CTRL_Q).unwrap();
    let status = client.drain_and_wait().unwrap();
    assert!(status.success(), "Ctrl+Q should quit cleanly, got {status:?}");
    let sent = reports(client.output());
    assert_eq!(
        sent.last().map(|(_, v)| v),
        Some(&Verdict::Clear),
        "quitting on purpose withdraws the report"
    );
}

/// `fresh FILE` reports the same file, by absolute path.
#[test]
fn a_file_launch_reports_its_files() {
    if !pty_available() {
        eprintln!("Skipping: no PTY available in this environment");
        return;
    }
    let home = tempfile::tempdir().unwrap();
    let project = setup(home.path());
    std::fs::write(project.join("named.txt"), "from-the-named-file\n").unwrap();
    let mut cmd = isolated_fresh(home.path());
    cmd.args(["--no-upgrade-check", "named.txt"]);
    let mut client = spawn_on_pty(cmd, ChildStdin::Terminal, 120, 30).expect("spawn fresh");

    let sent = wait_for_reports(&mut client, 1);
    let named = project.join("named.txt").display().to_string();
    assert_eq!(sent[0].1, resume(&[&binary(), &named], &project));
    client.kill();
}

/// `--wait` is an `$EDITOR` launch: it belongs to the program waiting on it,
/// so it reports nothing to resume on its own.
#[test]
fn a_wait_launch_reports_nothing() {
    if !pty_available() {
        eprintln!("Skipping: no PTY available in this environment");
        return;
    }
    let home = tempfile::tempdir().unwrap();
    let project = setup(home.path());
    std::fs::write(project.join("msg.txt"), "commit-message-here\n").unwrap();
    let mut cmd = isolated_fresh(home.path());
    cmd.args(["--no-upgrade-check", "--wait", "msg.txt"]);
    let mut client = spawn_on_pty(cmd, ChildStdin::Terminal, 120, 30).expect("spawn fresh");
    client
        .wait_for_screen(|s| s.contains("commit-message-here"))
        .expect("the file should open");
    client.send(CTRL_Q).unwrap();
    client.drain_and_wait().unwrap();
    assert!(
        reports(client.output()).iter().all(|(_, v)| *v == Verdict::Clear),
        "a --wait launch must not report a command"
    );
}

/// The terminal's token stays with fresh: a shell in one of fresh's own
/// terminals gets a token of its own (fresh is its terminal), never the one
/// fresh was handed.
#[test]
fn fresh_terminals_get_their_own_token() {
    if !pty_available() {
        eprintln!("Skipping: no PTY available in this environment");
        return;
    }
    let home = tempfile::tempdir().unwrap();
    setup(home.path());
    let mut cmd = isolated_fresh(home.path());
    cmd.args(["--no-init", "--no-upgrade-check"]);
    let mut client = spawn_on_pty(cmd, ChildStdin::Terminal, 160, 30).expect("spawn fresh");
    client
        .wait_for_screen(|s| s.contains(MENU_BAR))
        .expect("the editor should draw");

    // Open Terminal in Utility Dock: Alt+` .
    client.send(b"\x1b`").unwrap();
    client
        .wait_for_screen(|s| s.contains("Terminal 0 opened"))
        .expect("a terminal should open");
    // The pty buffers the line until the shell reads it. `case` prints a
    // verdict rather than the token, which the screen may be too narrow for;
    // `$((…))` keeps each verdict off the echoed command line.
    client
        .send(
            format!(
                "case \"$OSC7577_TOKEN\" in {TOKEN}) echo INHERITED$((1+1));; \
                 ????????????????????????????????????????????????????????????????) \
                 echo OWN$((1+1));; *) echo MISSING$((1+1));; esac\r"
            )
            .as_bytes(),
        )
        .unwrap();
    let waited = client.wait_for_screen(|s| {
        s.contains("OWN2") || s.contains("INHERITED2") || s.contains("MISSING2")
    });
    let screen = client.screen();
    client.kill();
    waited.unwrap_or_else(|e| panic!("the shell never answered: {e}"));
    assert!(
        screen.contains("OWN2") && !screen.contains("INHERITED2"),
        "a fresh terminal should get its own 64-character token.\nScreen:\n{screen}"
    );
}

/// A daemon client reports `fresh -a NAME`: after the terminal restarts that
/// rejoins the daemon (or starts it again, restoring its workspace).
#[test]
fn a_daemon_client_reports_reattaching() {
    if !pty_available() {
        eprintln!("Skipping: no PTY available in this environment");
        return;
    }
    let home = tempfile::tempdir().unwrap();
    let project = setup(home.path());
    let session = "program-resume-report";
    let mut cmd = isolated_fresh(home.path());
    cmd.args(["--cmd", "daemon", "new", session]);
    let mut client = spawn_on_pty(cmd, ChildStdin::Terminal, 120, 30).expect("spawn fresh");
    let waited = client.wait_for_output(|out| !reports(out).is_empty());
    let sent = reports(client.output());
    client.kill();
    let pid_file = home.path().join("run").join("fresh").join(format!("{session}.pid"));
    if let Ok(pid) = std::fs::read_to_string(pid_file) {
        if let Ok(pid) = pid.trim().parse::<i32>() {
            // SAFETY: a plain kill(2); a dead pid is ESRCH, ignored.
            unsafe { libc::kill(pid, libc::SIGKILL) };
        }
    }
    waited.unwrap_or_else(|e| panic!("the client never reported: {e}"));
    assert_eq!(sent[0].1, resume(&[&binary(), "-a", session], &project));
}

/// A daemon that dies without saying `Quit` — crashed, or killed at logout —
/// is not the user ending fresh: its client keeps the report (no `clear`), so
/// the terminal can still bring it back.
#[test]
fn a_daemon_client_keeps_its_report_when_the_daemon_dies() {
    if !pty_available() {
        eprintln!("Skipping: no PTY available in this environment");
        return;
    }
    let home = tempfile::tempdir().unwrap();
    setup(home.path());
    let session = "program-resume-daemon-dies";
    let mut cmd = isolated_fresh(home.path());
    cmd.args(["--cmd", "daemon", "new", session]);
    let mut client = spawn_on_pty(cmd, ChildStdin::Terminal, 120, 30).expect("spawn fresh");
    client
        .wait_for_output(|out| !reports(out).is_empty())
        .unwrap_or_else(|e| panic!("the client never reported: {e}"));

    let pid_file = home.path().join("run").join("fresh").join(format!("{session}.pid"));
    let pid: i32 = std::fs::read_to_string(&pid_file)
        .expect("the daemon writes its pid file before it accepts a client")
        .trim()
        .parse()
        .unwrap();
    // SAFETY: a plain kill(2) of the daemon this test started.
    unsafe { libc::kill(pid, libc::SIGKILL) };

    client.drain_and_wait().unwrap();
    let sent = reports(client.output());
    assert!(
        sent.iter().all(|(_, v)| *v != Verdict::Clear),
        "a client whose daemon died must not withdraw its report: {sent:?}"
    );
}
