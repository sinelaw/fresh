//! What a terminating signal costs (#3134 follow-up).
//!
//! `SIGINT`/`SIGTERM` are handled so the editor can leave a diagnostic
//! behind. The handler itself may do almost nothing — see
//! `services::signal_handler` — so the report is written by a thread that
//! was already waiting, and the expensive half of it, a backtrace from every
//! thread, is off by default: capturing one means allocating inside a signal
//! handler, which deadlocks against a thread interrupted mid-allocation and
//! used to leave `SIGTERM` ending in `SIGABRT`, `SIGSEGV`, or nothing at all.
//!
//! These drive the real binary, because the behaviour is the process's
//! rather than the library's, and read what the run left behind: the log,
//! which *is* the diagnostic, and the terminal the editor was handed, which
//! it has to give back on the way out.
//!
//! Linux-gated with `common::pty`, which needs `ptsname_r`.
#![cfg(target_os = "linux")]

use crate::common::pty::{pty_available, spawn_on_pty, ChildStdin, PtyChild};
use std::path::{Path, PathBuf};
use std::process::Command;

/// The banner the reporting thread writes before anything else.
const REPORTED: &str = "SIGNAL 15 RECEIVED";
/// The banner the per-thread sweep writes, when it runs at all.
const SWEPT: &str = "Thread Backtrace Dump";

/// `fresh` with its config, state and logs confined to `home`.
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
        // The wait below is on English text; pin it the way the other pty
        // tests do so a translated menu bar cannot stall the run.
        .env("LANG", "C.UTF-8")
        .env_remove("LC_ALL")
        .env_remove("LC_MESSAGES")
        // Whatever the developer running this has set, the default path
        // must be measured with the sweep off.
        .env_remove("FRESH_SIGNAL_BACKTRACES");
    cmd
}

/// Launch the editor and wait until it has drawn, so the signal lands on a
/// process that is actually running rather than one still starting up.
fn running_editor(cmd: Command) -> PtyChild {
    let mut editor = spawn_on_pty(cmd, ChildStdin::Terminal, 100, 30).expect("spawn fresh on a pty");
    editor
        .wait_for_screen(|screen| screen.contains("File") && screen.contains("Edit"))
        .expect("the editor should draw its menu bar");
    editor
}

/// Everything the run logged, concatenated. The reporting thread writes
/// through the same subscriber as the rest of the editor.
fn logged(home: &Path) -> String {
    let dir: PathBuf = home.join("state/fresh/logs");
    std::fs::read_dir(&dir)
        .unwrap_or_else(|e| panic!("read {}: {e}", dir.display()))
        .filter_map(|entry| std::fs::read_to_string(entry.ok()?.path()).ok())
        .collect()
}

/// SIGTERM the editor and wait for it to be reaped.
///
/// Reaping rather than polling `kill(pid, 0)`: that keeps succeeding for a
/// zombie, which is what a signalled child of this test becomes until it is
/// waited on.
fn sigterm_and_reap(editor: &mut PtyChild) -> std::process::ExitStatus {
    // SAFETY: signalling a child this test owns and has not yet reaped.
    assert_eq!(
        unsafe { libc::kill(editor.pid() as i32, libc::SIGTERM) },
        0
    );
    editor.drain_and_wait().expect("wait for fresh to exit")
}

/// The default: the signal is reported, and the process goes without asking
/// two dozen threads to unwind themselves inside a signal handler.
///
/// The absence of the sweep is the assertion that fails before this change —
/// it used to run on every terminating signal, whether or not anyone was
/// debugging a hang.
#[test]
fn a_terminating_signal_is_reported_without_sweeping_every_thread() {
    if !pty_available() {
        eprintln!("Skipping: no PTY available in this environment");
        return;
    }
    let home = tempfile::tempdir().unwrap();
    let mut editor = running_editor(isolated_fresh(home.path()));

    sigterm_and_reap(&mut editor);

    let log = logged(home.path());
    assert!(
        log.contains(REPORTED),
        "the signal should still be reported; log was:\n{log}"
    );
    assert!(
        !log.contains(SWEPT),
        "the per-thread sweep should not run unless asked for; log was:\n{log}"
    );
}

/// A signalled editor hands the terminal back before it goes.
///
/// The reporting thread leaves through `process::exit`, which runs no
/// destructors, so the guard that normally undoes the modes never gets to:
/// the shell came back in raw mode, still on the alternate screen, with
/// mouse reporting on. Typing echoed nothing and a click pasted an escape
/// burst at the prompt, and `reset` was the only way out.
///
/// The modes going *on* are asserted first, so a run where the editor never
/// took the terminal cannot pass this by leaving nothing to restore.
#[test]
fn a_terminating_signal_leaves_the_terminal_usable() {
    if !pty_available() {
        eprintln!("Skipping: no PTY available in this environment");
        return;
    }
    let home = tempfile::tempdir().unwrap();
    let mut editor = running_editor(isolated_fresh(home.path()));

    assert!(
        !editor.cooked(),
        "the editor should hold the terminal in raw mode while it runs"
    );
    assert!(
        editor.modes().alternate_screen(),
        "the editor should be drawing on the alternate screen"
    );

    sigterm_and_reap(&mut editor);

    assert!(
        editor.cooked(),
        "raw mode should be undone, or the shell that gets the terminal back echoes nothing"
    );
    assert!(
        !editor.modes().alternate_screen(),
        "the alternate screen should be left, or the shell draws over the editor's last frame"
    );
    assert_eq!(
        editor.modes().mouse_protocol_mode(),
        vt100::MouseProtocolMode::None,
        "mouse reporting should be off, or every click types an escape burst at the prompt"
    );
    assert!(
        !editor.modes().bracketed_paste(),
        "bracketed paste should be off, or a paste arrives at the shell wrapped in markers"
    );
}

/// ...and it is still there for whoever is chasing a hang.
#[test]
fn the_per_thread_sweep_is_available_on_request() {
    if !pty_available() {
        eprintln!("Skipping: no PTY available in this environment");
        return;
    }
    let home = tempfile::tempdir().unwrap();
    let mut cmd = isolated_fresh(home.path());
    cmd.env("FRESH_SIGNAL_BACKTRACES", "1");
    let mut editor = running_editor(cmd);

    sigterm_and_reap(&mut editor);

    let log = logged(home.path());
    assert!(
        log.contains(SWEPT),
        "the sweep was asked for and should have run; log was:\n{log}"
    );
    assert!(
        log.contains("--- Thread 1 "),
        "the sweep should report the threads it found; log was:\n{log}"
    );
}
