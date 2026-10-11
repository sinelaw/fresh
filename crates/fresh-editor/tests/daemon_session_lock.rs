//! A second daemon for a session that already has one stands down.
//!
//! Binding a session used to clear its socket and pid files whoever owned
//! them, so a second `fresh --server` for a running session (or the one a
//! `fresh -a` starts when it looks just before the first is listening) took
//! the session over. The first kept running, unsaved buffers and all, where
//! no client could reach it.
//!
//! Driven through the binary because the property spans two processes.
//! Linux-gated like the other binary-driving tests: `XDG_*` isolation of the
//! socket tree works there.
#![cfg(target_os = "linux")]

use crate::common::daemon::spawn_daemon;
use std::os::unix::fs::MetadataExt;
use std::path::Path;
use std::process::{Command, Stdio};

/// A `fresh --server` whose config, state and sockets all live under `home`.
fn isolated_daemon(home: &Path, session: &str) -> Command {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_fresh"));
    cmd.current_dir(home.join("project"))
        .env("HOME", home)
        .env("TMPDIR", home)
        .env("XDG_CONFIG_HOME", home.join("config"))
        .env("XDG_DATA_HOME", home.join("data"))
        .env("XDG_STATE_HOME", home.join("state"))
        .env("XDG_CACHE_HOME", home.join("cache"))
        .env("XDG_RUNTIME_DIR", home.join("run"))
        .env_remove("FRESH_SESSION")
        .env_remove("FRESH_BIN")
        .args(["--server", "--session-name", session, "--no-plugins"])
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null());
    cmd
}

#[test]
fn a_second_daemon_for_a_running_session_stands_down() {
    let home = tempfile::tempdir().unwrap();
    let home = home.path();
    std::fs::create_dir_all(home.join("project")).unwrap();
    std::fs::create_dir_all(home.join("run")).unwrap();
    let session = "taken";
    let socket_dir = home.join("run").join("fresh");
    let pid_file = socket_dir.join(format!("{session}.pid"));
    let control = socket_dir.join(format!("{session}.ctrl.sock"));

    let mut first = spawn_daemon(isolated_daemon(home, session), &pid_file);
    let control_before = std::fs::metadata(&control).unwrap().ino();

    // Waited on without a deadline: a second daemon that takes the session
    // over runs on, and nextest's timeout is the failure.
    let second = isolated_daemon(home, session)
        .status()
        .expect("run the second daemon");

    let pid_after = std::fs::read_to_string(&pid_file).unwrap_or_default();
    let control_after = std::fs::metadata(&control).map(|m| m.ino()).ok();
    let first_running = first.try_wait().unwrap().is_none();
    let _ = first.kill();
    let _ = first.wait();

    assert!(!second.success(), "the second daemon reported success");
    assert!(first_running, "the first daemon is gone");
    assert_eq!(
        pid_after.trim(),
        first.id().to_string(),
        "the pid file no longer names the first daemon"
    );
    assert_eq!(
        control_after,
        Some(control_before),
        "the first daemon's control socket was replaced"
    );
}
