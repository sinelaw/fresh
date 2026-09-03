//! The stdin spool file does not outlive the process (#3134).
//!
//! `echo … | fresh -` drains the pipe into `$TMPDIR/fresh-stdin-<pid>.tmp`
//! and keeps reading chunks back off it — a large stdin is loaded lazily —
//! so the file has to survive the buffer and can only be removed by the
//! process itself. Nothing removed it: quitting left the file behind, and
//! the only thing that ever deleted it was the 24h sweep in
//! `log_dirs::cleanup_stale_logs`. Piping a decompressed log in meant a
//! copy of it sitting in `/tmp` for a day.
//!
//! This drives the real binary, because the property belongs to the
//! process. Every way out that a user can actually hit is covered: a clean
//! quit, an early failure that never reaches the editor, and the signals —
//! `pkill`/a container stop (SIGTERM) and closing the terminal window
//! (SIGHUP), neither of which unwinds `real_main` and so neither of which
//! the exit guard alone can catch.
//!
//! Linux-gated with `common::pty`, which needs `ptsname_r`.
#![cfg(target_os = "linux")]

use crate::common::pty::{pty_available, spawn_on_pty, ChildStdin, PtyChild};
use std::path::{Path, PathBuf};
use std::process::Command;

const CTRL_Q: &[u8] = &[0x11];

/// Spool files left in `dir`.
fn spool_files(dir: &Path) -> Vec<PathBuf> {
    std::fs::read_dir(dir)
        .expect("read temp dir")
        .filter_map(|entry| {
            let path = entry.ok()?.path();
            let name = path.file_name()?.to_string_lossy().into_owned();
            name.starts_with("fresh-stdin-").then_some(path)
        })
        .collect()
}

/// `fresh` with its temp dir, config and state confined to `home`, so the
/// only `fresh-stdin-*` file that can appear under `$TMPDIR` is this test's.
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

/// Launch `fresh -` with something on stdin and wait until it has drawn,
/// then assert it really did spool to a file — so a later "the file is
/// gone" assertion cannot pass vacuously.
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

    // Semantic wait: the editor is up once it has drawn its menu bar.
    editor
        .wait_for_screen(|screen| screen.contains("File") && screen.contains("Help"))
        .expect("editor should render");

    assert!(
        !spool_files(home).is_empty(),
        "expected a fresh-stdin-* file while the editor is running; \
         temp dir held: {:?}",
        std::fs::read_dir(home)
            .unwrap()
            .filter_map(|e| Some(e.ok()?.file_name()))
            .collect::<Vec<_>>()
    );

    editor
}

/// The reported case: pipe something in, quit with Ctrl+Q, and the spool
/// file is gone.
#[test]
fn quitting_deletes_the_stdin_spool_file() {
    if !pty_available() {
        eprintln!("Skipping: no PTY available in this environment");
        return;
    }
    let home = tempfile::tempdir().unwrap();
    let mut editor = running_editor_with_piped_stdin(home.path());

    editor.send(CTRL_Q).expect("send Ctrl+Q");
    let status = editor.drain_and_wait().expect("wait for fresh to exit");
    assert!(status.success(), "fresh exited abnormally: {status:?}");

    assert_eq!(
        spool_files(home.path()),
        Vec::<PathBuf>::new(),
        "the stdin spool file should be gone once fresh has exited"
    );
}

/// Signal the running editor and wait for it to actually be reaped.
///
/// Reaping matters twice over: the spool file can only be checked once the
/// process is really gone, and polling `kill(pid, 0)` would not tell us
/// that — it keeps succeeding for a zombie, which is exactly what a
/// signalled child of this test becomes until it is waited on.
fn signal_and_reap(editor: &mut PtyChild, signal: libc::c_int) -> std::process::ExitStatus {
    // SAFETY: signalling a child this test owns and has not yet reaped.
    assert_eq!(unsafe { libc::kill(editor.pid() as i32, signal) }, 0);
    editor.drain_and_wait().expect("wait for fresh to exit")
}

/// `pkill fresh`, a container stop, `tmux kill-session`. SIGTERM leaves by
/// a route that never unwinds `real_main`, so the exit guard alone does not
/// catch it — the signal handler has to sweep.
#[test]
fn sigterm_deletes_the_stdin_spool_file() {
    if !pty_available() {
        eprintln!("Skipping: no PTY available in this environment");
        return;
    }
    let home = tempfile::tempdir().unwrap();
    let mut editor = running_editor_with_piped_stdin(home.path());

    let status = signal_and_reap(&mut editor, libc::SIGTERM);
    // The pre-existing handler reports Ctrl+C's 130 for both signals it
    // takes; asserted so a change to that path shows up here rather than
    // silently turning this into a test of some other exit.
    assert_eq!(
        status.code(),
        Some(130),
        "expected the termination handler's exit, got {status:?}"
    );

    assert_eq!(
        spool_files(home.path()),
        Vec::<PathBuf>::new(),
        "SIGTERM should not leave a stdin spool file behind"
    );
}

/// Closing the terminal window, or an ssh session dropping. SIGHUP had no
/// handler at all, so this was the most ordinary leak of the lot.
#[test]
fn sighup_deletes_the_stdin_spool_file() {
    use std::os::unix::process::ExitStatusExt;

    if !pty_available() {
        eprintln!("Skipping: no PTY available in this environment");
        return;
    }
    let home = tempfile::tempdir().unwrap();
    let mut editor = running_editor_with_piped_stdin(home.path());

    let status = signal_and_reap(&mut editor, libc::SIGHUP);
    // Still *killed by* SIGHUP, not exited: the handler exists only to
    // sweep, and re-raises under `SA_RESETHAND` so the status a parent sees
    // is the one it would have seen with no handler at all.
    assert_eq!(
        status.signal(),
        Some(libc::SIGHUP),
        "SIGHUP should still terminate by SIGHUP, got {status:?}"
    );

    assert_eq!(
        spool_files(home.path()),
        Vec::<PathBuf>::new(),
        "SIGHUP should not leave a stdin spool file behind"
    );
}

/// `nohup fresh …` makes the process immune to SIGHUP, and installing a
/// handler must not take that away: `SA_RESETHAND` resets to `SIG_DFL`, not
/// back to `SIG_IGN`, so a handler installed over an inherited ignore would
/// turn "survives the terminal closing" into "killed by it".
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

    let mut editor =
        spawn_on_pty(cmd, ChildStdin::Piped(b"hello-from-stdin\n".to_vec()), 100, 30)
            .expect("spawn fresh on a pty");
    editor
        .wait_for_screen(|screen| screen.contains("File") && screen.contains("Help"))
        .expect("editor should render");

    // SAFETY: signalling a child this test owns and has not yet reaped.
    assert_eq!(
        unsafe { libc::kill(editor.pid() as i32, libc::SIGHUP) },
        0
    );

    // It should still be alive: quit it normally and check it exited that
    // way rather than by the signal.
    editor.send(CTRL_Q).expect("send Ctrl+Q");
    let status = editor.drain_and_wait().expect("wait for fresh to exit");
    assert!(
        status.success(),
        "SIGHUP inherited as SIG_IGN should not kill the editor; got {status:?}"
    );
}

/// The other way out: the spool file is created before stdin is reopened
/// from `/dev/tty`, so a launch that fails right there used to leak it too.
/// Here that failure is forced by giving the child no controlling terminal
/// at all, which is also what a `fresh -` run from a non-tty context does.
#[test]
fn a_failed_launch_does_not_leak_the_stdin_spool_file() {
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

    assert_eq!(
        spool_files(home.path()),
        Vec::<PathBuf>::new(),
        "a failed launch should not leave a stdin spool file behind"
    );
}
