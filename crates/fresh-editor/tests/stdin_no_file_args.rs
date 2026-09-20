//! A pipe on stdin is input, never keystrokes (#3252).
//!
//! `echo 123 | fresh` — a pipe on fd 0, no file arguments and no `-` — fell
//! between the cases: `fresh -` reads the pipe and reopens stdin from
//! `/dev/tty`, a bare `fresh` on a terminal goes to the orchestrator, and
//! this went to neither. fd 0 stayed the pipe, so two things followed, both
//! fatal. The piped bytes reached the key decoder as keystrokes (`123\n`
//! landed in a buffer and left the cursor on line 2), and once the writer
//! closed, `poll` returned `POLLHUP` — immediately, forever, and without
//! `POLLIN`, so `services::tty_input::poll_readable` read it as a timeout
//! and asked again. The editor burned a core and never saw a key again, not
//! even the Ctrl+Q that would have let the now-dirty buffer be saved.
//!
//! A bare `fresh` with something piped in now means `fresh -`, and a launch
//! that opens files leaves the pipe alone but still takes the terminal for
//! keys. These tests check both from the outside, where the bug was visible:
//! fd 0 is a terminal, and the editor answers the keyboard.
//!
//! Linux-gated: `common::pty` needs `ptsname_r`, and fd 0 is read from
//! `/proc`.
#![cfg(target_os = "linux")]

use crate::common::pty::{pty_available, spawn_on_pty, ChildStdin, PtyChild};
use std::path::{Path, PathBuf};
use std::process::Command;

/// What the editor prints once stdin has been drained in full
/// (`stdin.read_complete`, with `LANG` pinned to English below).
const DRAINED: &str = "bytes from stdin";

/// What is piped in. Distinctive enough to tell "opened as a buffer" from
/// "typed at the keyboard" on the screen.
const PIPED: &[u8] = b"piped-into-fresh\n";

/// Wide enough for the drained message to fit beside the workspace dock —
/// see `stdin_spool_lifetime.rs`, where a narrower pty truncated it away.
const COLS: u16 = 140;
const ROWS: u16 = 30;

/// `fresh` with its temp dir, config and state confined to `home`, and its
/// language pinned so the English predicates below can match.
fn isolated_fresh(home: &Path) -> Command {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_fresh"));
    cmd.current_dir(home)
        .env("TMPDIR", home)
        .env("HOME", home)
        .env("XDG_CONFIG_HOME", home.join("config"))
        .env("XDG_DATA_HOME", home.join("data"))
        .env("XDG_STATE_HOME", home.join("state"))
        .env("XDG_CACHE_HOME", home.join("cache"))
        .env("TERM", "xterm-256color")
        .env("LANG", "C.UTF-8")
        .env_remove("LC_ALL")
        .env_remove("LC_MESSAGES")
        // A developer running these from inside Fresh's own terminal would
        // otherwise have the file-argument case forwarded to their editor
        // instead of opened here.
        .env_remove("FRESH_SESSION");
    cmd
}

/// Where `fd 0` points for a running child.
///
/// This is the whole bug in one string: `pipe:[…]` is the hung-up pipe the
/// editor polled forever, a terminal is what it should have taken. Which
/// terminal name shows up depends on how fd 0 was obtained — `/dev/tty`
/// for a descriptor reopened from it, `/dev/pts/N` for one inherited from
/// the pty — so both count and a pipe never does.
fn stdin_target(editor: &PtyChild) -> PathBuf {
    std::fs::read_link(format!("/proc/{}/fd/0", editor.pid())).expect("read fd 0 of the editor")
}

fn assert_stdin_is_a_terminal(editor: &PtyChild) {
    let target = stdin_target(editor);
    assert!(
        target == Path::new("/dev/tty") || target.starts_with("/dev/pts/"),
        "fd 0 should be the terminal the editor reads keys from, not {}",
        target.display()
    );
}

/// Launch `fresh` with `args` and `PIPED` on stdin, on a pty of its own.
fn fresh_with_piped_stdin(home: &Path, args: &[&str]) -> PtyChild {
    let mut cmd = isolated_fresh(home);
    cmd.args(["--no-session", "--no-init", "--no-upgrade-check"]);
    cmd.args(args);

    spawn_on_pty(cmd, ChildStdin::Piped(PIPED.to_vec()), COLS, ROWS)
        .expect("spawn fresh on a pty")
}

/// The reported case. No `-`, no file: the pipe is the input, and the
/// keyboard still works afterwards.
#[test]
fn a_bare_fresh_reads_the_pipe_and_keeps_the_keyboard() {
    if !pty_available() {
        eprintln!("Skipping: no PTY available in this environment");
        return;
    }
    let home = tempfile::tempdir().unwrap();
    let mut editor = fresh_with_piped_stdin(home.path(), &[]);

    editor
        .wait_for_screen(|screen| screen.contains(DRAINED))
        .expect("the pipe should be read into a buffer");
    editor
        .wait_for_screen(|screen| screen.contains("piped-into-fresh"))
        .expect("the piped text should be in the buffer");

    // The spin was a hung-up pipe on fd 0. There isn't one.
    assert_stdin_is_a_terminal(&editor);

    // And the keyboard reaches the buffer — this is what "input-dead" meant:
    // before the fix these bytes went into the same dead pipe's poll loop.
    editor.send(b"ZZZ").expect("type into the editor");
    editor
        .wait_for_screen(|screen| screen.contains("ZZZ"))
        .expect("typed keys should reach the buffer");
}

/// `echo … | fresh -`, the form that already worked, still does — same
/// buffer, same terminal on fd 0.
#[test]
fn an_explicit_dash_still_reads_the_pipe() {
    if !pty_available() {
        eprintln!("Skipping: no PTY available in this environment");
        return;
    }
    let home = tempfile::tempdir().unwrap();
    let mut editor = fresh_with_piped_stdin(home.path(), &["-"]);

    editor
        .wait_for_screen(|screen| screen.contains(DRAINED))
        .expect("the pipe should be read into a buffer");
    assert_stdin_is_a_terminal(&editor);

    editor.send(b"ZZZ").expect("type into the editor");
    editor
        .wait_for_screen(|screen| screen.contains("ZZZ"))
        .expect("typed keys should reach the buffer");
}

/// A file argument says what to open, so the pipe is not read as a buffer.
/// fd 0 still has to stop being that pipe, or this launch spins the same
/// way the reported one did.
#[test]
fn a_file_argument_wins_over_the_pipe_and_still_takes_the_terminal() {
    if !pty_available() {
        eprintln!("Skipping: no PTY available in this environment");
        return;
    }
    let home = tempfile::tempdir().unwrap();
    std::fs::write(home.path().join("named.txt"), "from-the-named-file\n").unwrap();

    let mut editor = fresh_with_piped_stdin(home.path(), &["named.txt"]);

    editor
        .wait_for_screen(|screen| screen.contains("from-the-named-file"))
        .expect("the named file should open");
    assert_stdin_is_a_terminal(&editor);

    editor.send(b"ZZZ").expect("type into the editor");
    editor
        .wait_for_screen(|screen| screen.contains("ZZZ"))
        .expect("typed keys should reach the buffer");

    let screen = editor.screen();
    assert!(
        !screen.contains("piped-into-fresh"),
        "the pipe should not have been opened as a buffer:\n{screen}"
    );
}

/// No controlling terminal at all: say so and exit, rather than coming up
/// with no way to take input. `setsid` between fork and exec is what makes
/// `/dev/tty` unopenable (ENXIO).
#[test]
fn no_controlling_terminal_fails_with_a_message() {
    use std::io::{Read, Write};
    use std::os::unix::process::CommandExt;
    use std::process::Stdio;

    let home = tempfile::tempdir().unwrap();

    let mut cmd = isolated_fresh(home.path());
    cmd.args(["--no-session", "--no-init", "--no-upgrade-check"])
        .stdin(Stdio::piped())
        .stdout(Stdio::null())
        .stderr(Stdio::piped());

    // SAFETY: between fork and exec; `setsid` is async-signal-safe.
    unsafe {
        cmd.pre_exec(|| {
            libc::setsid();
            Ok(())
        });
    }

    let mut child = cmd.spawn().expect("spawn fresh");
    child
        .stdin
        .take()
        .expect("piped stdin")
        .write_all(PIPED)
        .expect("write to fresh's stdin");

    let mut stderr = String::new();
    child
        .stderr
        .take()
        .expect("piped stderr")
        .read_to_string(&mut stderr)
        .expect("read fresh's stderr");
    let status = child.wait().expect("wait for fresh to exit");

    assert!(
        !status.success(),
        "expected the launch to fail without a controlling terminal, got {status:?}"
    );
    assert!(
        stderr.contains("/dev/tty"),
        "the failure should name the terminal it could not open, got:\n{stderr}"
    );
}
