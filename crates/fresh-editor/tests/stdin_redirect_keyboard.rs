//! With stdin redirected, the keyboard still reaches the editor and the
//! editor stays idle, on macOS as on Linux (#3683).
//!
//! When stdin is not the terminal (`cmd | fresh`, `fresh file </dev/null`),
//! `main` reads or drops it and reopens fd 0 from `/dev/tty`. On macOS,
//! poll(2) cannot wait on `/dev/tty`: it returns at once with `POLLNVAL`.
//! The key reader counted only `POLLIN` as readable, so it never read a key,
//! Ctrl+Q included, and asked again immediately, spinning a core. An explicit
//! `fresh file </dev/tty` failed the same way without any reopen.
//!
//! `stdin_no_file_args.rs` already typed into these launches, but it is
//! Linux-gated (it reads fd 0 from `/proc`), and on Linux poll(2) works on
//! `/dev/tty` — so CI never ran the case that failed. These tests ask only
//! what a user sees, which every platform with a pty can answer: typed keys
//! land in the buffer and Ctrl+Q quits. The spinning core had the same cause
//! as the dead keyboard, so it is not measured separately: a CPU threshold
//! would be the time-sensitive kind of test CONTRIBUTING rules out.
#![cfg(any(target_os = "linux", target_os = "macos"))]

use crate::common::pty::{pty_available, spawn_on_pty, ChildStdin, PtyChild};
use std::path::Path;
use std::process::Command;

const CTRL_Q: &[u8] = &[0x11];

/// Wide enough for the status bar's stdin message beside the workspace dock
/// (see `stdin_spool_lifetime.rs`).
const COLS: u16 = 140;
const ROWS: u16 = 30;

/// The named file each launch opens, and a line from it to wait for.
const NAMED: &str = "named.txt";
const NAMED_TEXT: &str = "from-the-named-file";

/// `fresh` confined to `home` — config, state, data and temp — with its
/// language pinned so the English predicates match. `HOME` alone confines it
/// on macOS; the `XDG_*` variables do the same on Linux.
fn isolated_fresh(home: &Path, args: &[&str]) -> Command {
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
        .env_remove("FRESH_SESSION")
        .args(["--no-session", "--no-init", "--no-upgrade-check"])
        .args(args);
    cmd
}

/// Launch `fresh named.txt` with `stdin`, and wait for the file to show.
fn launch(home: &Path, stdin: ChildStdin) -> PtyChild {
    std::fs::write(home.join(NAMED), format!("{NAMED_TEXT}\n")).unwrap();
    let mut editor =
        spawn_on_pty(isolated_fresh(home, &[NAMED]), stdin, COLS, ROWS).expect("spawn fresh");
    editor
        .wait_for_screen(|screen| screen.contains(NAMED_TEXT))
        .expect("the named file should open");
    editor
}

/// Type into the editor and see it land, then quit with Ctrl+Q.
fn assert_keyboard_works(mut editor: PtyChild) {
    editor.send(b"ZZZ").expect("type into the editor");
    editor
        .wait_for_screen(|screen| screen.contains("ZZZ"))
        .expect("typed keys should reach the buffer");

    // Undo the edit so Ctrl+Q quits rather than asking to save it. Undo is
    // Ctrl+Z; three of them cover one undo step per character.
    editor.send(&[0x1a, 0x1a, 0x1a]).expect("undo");
    editor
        .wait_for_screen(|screen| !screen.contains("ZZZ"))
        .expect("undo should take the typed text back out");

    editor.send(CTRL_Q).expect("send Ctrl+Q");
    let status = editor.drain_and_wait().expect("wait for fresh to exit");
    assert!(status.success(), "Ctrl+Q should quit cleanly, got {status:?}");
}

fn check(stdin: ChildStdin) {
    if !pty_available() {
        eprintln!("Skipping: no PTY available in this environment");
        return;
    }
    let home = tempfile::tempdir().unwrap();
    let editor = launch(home.path(), stdin);
    assert_keyboard_works(editor);
}

/// `seq 1 10 | fresh named.txt`: the pipe is dropped and fd 0 reopened from
/// `/dev/tty`.
#[test]
fn keys_reach_the_editor_with_a_pipe_on_stdin() {
    check(ChildStdin::Piped(b"piped-into-fresh\n".to_vec()));
}

/// `fresh named.txt </dev/null`: not a terminal, so reopened from `/dev/tty`.
#[test]
fn keys_reach_the_editor_with_dev_null_on_stdin() {
    check(ChildStdin::Null);
}

/// `fresh named.txt </dev/tty`: already a terminal, so kept as is — and on
/// macOS that is the same device poll(2) cannot wait on.
#[test]
fn keys_reach_the_editor_with_dev_tty_on_stdin() {
    check(ChildStdin::DevTty);
}

/// `seq 1 2000 | fresh`, the launch in the report: the pipe becomes a
/// buffer, and the keyboard still works after it.
#[test]
fn keys_reach_a_bare_fresh_reading_a_pipe() {
    if !pty_available() {
        eprintln!("Skipping: no PTY available in this environment");
        return;
    }
    let home = tempfile::tempdir().unwrap();
    let mut editor = spawn_on_pty(
        isolated_fresh(home.path(), &[]),
        ChildStdin::Piped(b"piped-into-fresh\n".to_vec()),
        COLS,
        ROWS,
    )
    .expect("spawn fresh");
    editor
        .wait_for_screen(|screen| screen.contains("bytes from stdin"))
        .expect("the pipe should be read into a buffer");

    editor.send(b"ZZZ").expect("type into the editor");
    editor
        .wait_for_screen(|screen| screen.contains("ZZZ"))
        .expect("typed keys should reach the buffer");
}
