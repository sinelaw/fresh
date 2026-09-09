//! The stdin spool never has a name to leave behind (#3134).
//!
//! `echo … | fresh -` drains the pipe into a scratch file, and the buffer
//! keeps reading chunks back off it — a large stdin is loaded lazily — so the
//! file has to outlive the buffer. It used to outlive it *by name*, which
//! made deleting it the process's job: quitting left it in `/tmp` and only
//! the 24h sweep in `log_dirs::cleanup_stale_logs` ever removed it.
//!
//! It is now unlinked the instant it is created and held open instead (see
//! `services::stdin_spool`), so the kernel frees it when the process goes,
//! whatever takes the process down. These tests drive the real binary and
//! check the outside world for wreckage: after each way out, and *while the
//! editor is still running*, `$TMPDIR` holds nothing.
//!
//! The while-running check is the one that would have failed before this
//! design and cannot fail after it — there is no window in which a name
//! exists, so there is no exit route left to get wrong. The exit routes are
//! still enumerated below, because each of them used to leak and a
//! regression would show up as a name reappearing.
//!
//! Linux-gated with `common::pty`, which needs `ptsname_r`.
#![cfg(target_os = "linux")]

use crate::common::pty::{pty_available, spawn_on_pty, ChildStdin, PtyChild};
use std::path::{Path, PathBuf};
use std::process::Command;

const CTRL_Q: &[u8] = &[0x11];

/// Anything in `dir` that looks like a spool file, by any of the names this
/// code has used. Empty is the only acceptable answer, at every moment.
fn leftovers(dir: &Path) -> Vec<PathBuf> {
    std::fs::read_dir(dir)
        .expect("read temp dir")
        .filter_map(|entry| {
            let path = entry.ok()?.path();
            let name = path.file_name()?.to_string_lossy().into_owned();
            name.starts_with("fresh-stdin-").then_some(path)
        })
        .collect()
}

/// `fresh` with its temp dir, config and state confined to `home`, so
/// anything found under `$TMPDIR` belongs to this test.
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
        // The screen predicates below are English. Pin the language the same
        // way `daemon_locale_from_config.rs` does, so a developer with
        // `LANG=ja_JP.UTF-8` does not get a menu bar these tests can never
        // match — and an interactive editor that never exits means the wait
        // never ends.
        .env("LANG", "C.UTF-8")
        .env_remove("LC_ALL")
        .env_remove("LC_MESSAGES");
    cmd
}

/// What the editor prints once stdin has been drained in full: `Read N bytes
/// from stdin` (`stdin.read_complete`, with `LANG` pinned to English above).
const DRAINED: &str = "bytes from stdin";

/// Launch `fresh -` with something on stdin, wait until the spool is fully
/// drained, and assert it is already nameless while the editor runs.
///
/// The wait is on the *drain completing*, not on the menu bar appearing.
/// Both are semantic waits, but only this one means the thing the test is
/// about has actually happened: the menu bar is drawn early, while plugins
/// are still loading and the pipe is still being read, so signalling there
/// raced the editor's own startup — the shape CONTRIBUTING warns about, and
/// it showed up as a rare abort inside the pre-existing SIGTERM backtrace
/// dump rather than as anything to do with the spool.
///
/// Asserting emptiness *here* is the heart of the design: the old spool was
/// created named and deleted on the way out, so at this point — editor up,
/// stdin fully read — it would have been sitting in `$TMPDIR`.
fn running_editor_with_piped_stdin(home: &Path) -> PtyChild {
    let mut cmd = isolated_fresh(home);
    cmd.args(["--no-session", "--no-init", "--no-upgrade-check", "-"]);

    let mut editor = spawn_on_pty(
        cmd,
        ChildStdin::Piped(b"hello-from-stdin\n".to_vec()),
        100,
        30,
    )
    .expect("spawn fresh on a pty");

    editor
        .wait_for_screen(|screen| screen.contains(DRAINED))
        .expect("stdin should be drained and reported");

    assert_eq!(
        leftovers(home),
        Vec::<PathBuf>::new(),
        "the spool should have no name even while the editor is running"
    );

    editor
}

/// Signal the running editor and wait for it to actually be reaped.
///
/// Reaping matters twice over: the directory can only be checked once the
/// process is really gone, and polling `kill(pid, 0)` would not tell us
/// that — it keeps succeeding for a zombie, which is exactly what a
/// signalled child of this test becomes until it is waited on.
fn signal_and_reap(editor: &mut PtyChild, signal: libc::c_int) -> std::process::ExitStatus {
    // SAFETY: signalling a child this test owns and has not yet reaped.
    assert_eq!(unsafe { libc::kill(editor.pid() as i32, signal) }, 0);
    editor.drain_and_wait().expect("wait for fresh to exit")
}

/// The reported case: pipe something in, quit with Ctrl+Q, nothing is left.
#[test]
fn quitting_leaves_nothing_behind() {
    if !pty_available() {
        eprintln!("Skipping: no PTY available in this environment");
        return;
    }
    let home = tempfile::tempdir().unwrap();
    let mut editor = running_editor_with_piped_stdin(home.path());

    editor.send(CTRL_Q).expect("send Ctrl+Q");
    let status = editor.drain_and_wait().expect("wait for fresh to exit");
    assert!(status.success(), "fresh exited abnormally: {status:?}");

    assert_eq!(leftovers(home.path()), Vec::<PathBuf>::new());
}

/// `pkill fresh`, a container stop, `tmux kill-session`. SIGTERM leaves by a
/// route that never unwinds `real_main`, which is why it used to leak.
#[test]
fn sigterm_leaves_nothing_behind() {
    if !pty_available() {
        eprintln!("Skipping: no PTY available in this environment");
        return;
    }
    let home = tempfile::tempdir().unwrap();
    let mut editor = running_editor_with_piped_stdin(home.path());

    // Deliberately no assertion on *how* it died. SIGTERM is caught by a
    // pre-existing diagnostic handler that dumps every thread's backtrace
    // before exiting, and that path is not reliable under load — it
    // allocates and takes locks from inside a signal handler, so it
    // sometimes aborts instead of reaching its `exit`. Pinning its exit
    // code here would make this test fail for a reason that has nothing to
    // do with the spool. What the spool guarantees is the line below, and
    // it holds however the process ends: `signal_and_reap` returning at all
    // means it is gone.
    signal_and_reap(&mut editor, libc::SIGTERM);

    assert_eq!(leftovers(home.path()), Vec::<PathBuf>::new());
}

/// Closing the terminal window, or an ssh session dropping.
///
/// SIGHUP has no handler — the default action terminates the editor, exactly
/// as it did before any of this. Nothing needs to run on the way out, which
/// is the point.
#[test]
fn sighup_leaves_nothing_behind() {
    use std::os::unix::process::ExitStatusExt;

    if !pty_available() {
        eprintln!("Skipping: no PTY available in this environment");
        return;
    }
    let home = tempfile::tempdir().unwrap();
    let mut editor = running_editor_with_piped_stdin(home.path());

    let status = signal_and_reap(&mut editor, libc::SIGHUP);
    assert_eq!(
        status.signal(),
        Some(libc::SIGHUP),
        "SIGHUP should terminate by SIGHUP, got {status:?}"
    );

    assert_eq!(leftovers(home.path()), Vec::<PathBuf>::new());
}

/// The one no cleanup path could ever have covered: `SIGKILL` runs nothing
/// at all. Only the kernel reclaiming an unlinked inode gets this right,
/// which is the whole reason for the redesign.
#[test]
fn sigkill_leaves_nothing_behind() {
    use std::os::unix::process::ExitStatusExt;

    if !pty_available() {
        eprintln!("Skipping: no PTY available in this environment");
        return;
    }
    let home = tempfile::tempdir().unwrap();
    let mut editor = running_editor_with_piped_stdin(home.path());

    let status = signal_and_reap(&mut editor, libc::SIGKILL);
    assert_eq!(
        status.signal(),
        Some(libc::SIGKILL),
        "SIGKILL should terminate by SIGKILL, got {status:?}"
    );

    assert_eq!(leftovers(home.path()), Vec::<PathBuf>::new());
}

/// `nohup fresh …` makes the process immune to SIGHUP, and nothing here
/// should take that away — there is no SIGHUP handler to install any more,
/// so the inherited `SIG_IGN` stands.
#[test]
fn sighup_ignored_by_the_parent_stays_ignored() {
    if !pty_available() {
        eprintln!("Skipping: no PTY available in this environment");
        return;
    }
    let home = tempfile::tempdir().unwrap();

    let mut cmd = isolated_fresh(home.path());
    cmd.args(["--no-session", "--no-init", "--no-upgrade-check", "-"]);
    // SAFETY: between fork and exec; `signal` is async-signal-safe. This is
    // what `nohup` does to its child.
    unsafe {
        use std::os::unix::process::CommandExt;
        cmd.pre_exec(|| {
            libc::signal(libc::SIGHUP, libc::SIG_IGN);
            Ok(())
        });
    }

    let mut editor = spawn_on_pty(
        cmd,
        ChildStdin::Piped(b"hello-from-stdin\n".to_vec()),
        100,
        30,
    )
    .expect("spawn fresh on a pty");
    editor
        .wait_for_screen(|screen| screen.contains(DRAINED))
        .expect("stdin should be drained and reported");

    // SAFETY: signalling a child this test owns and has not yet reaped.
    assert_eq!(unsafe { libc::kill(editor.pid() as i32, libc::SIGHUP) }, 0);

    // Still alive: quit it normally and check it exited that way rather than
    // by the signal.
    editor.send(CTRL_Q).expect("send Ctrl+Q");
    let status = editor.drain_and_wait().expect("wait for fresh to exit");
    assert!(
        status.success(),
        "SIGHUP inherited as SIG_IGN should not kill the editor; got {status:?}"
    );

    assert_eq!(leftovers(home.path()), Vec::<PathBuf>::new());
}

/// The spool is created before stdin is reopened from `/dev/tty`, so a launch
/// that fails right there used to leak it. There is nothing to leak now, but
/// the route is still worth walking.
#[test]
fn a_failed_launch_leaves_nothing_behind() {
    use std::io::Write;
    use std::os::unix::process::CommandExt;
    use std::process::Stdio;

    let home = tempfile::tempdir().unwrap();

    let mut cmd = isolated_fresh(home.path());
    cmd.arg("-")
        .stdin(Stdio::piped())
        .stdout(Stdio::null())
        .stderr(Stdio::null());

    // SAFETY: between fork and exec; `setsid` is async-signal-safe. Without
    // a controlling terminal, opening `/dev/tty` fails with ENXIO, so the
    // child cannot reach the editor loop and is guaranteed to exit.
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
        .write_all(b"hello-from-stdin\n")
        .expect("write to fresh's stdin");

    let status = child.wait().expect("wait for fresh to exit");
    assert!(
        !status.success(),
        "expected the launch to fail without a controlling terminal, got {status:?}"
    );

    assert_eq!(leftovers(home.path()), Vec::<PathBuf>::new());
}
