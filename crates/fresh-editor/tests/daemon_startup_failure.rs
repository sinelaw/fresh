//! A client whose freshly spawned daemon dies before binding its sockets
//! reports it and exits, instead of waiting on a pid file that never comes.
//!
//! The daemon is made to fail with a runtime directory too long for a Unix
//! socket path, so its `bind()` fails. Linux-gated like the other
//! binary-driving tests: `XDG_*` isolation and `common::pty` are Linux-only.
#![cfg(target_os = "linux")]

use crate::common::pty::{pty_available, spawn_on_pty, ChildStdin};
use std::process::Command;
use std::time::Duration;

#[test]
fn a_client_reports_a_daemon_that_dies_during_startup() {
    if !pty_available() {
        eprintln!("Skipping: no PTY available in this environment");
        return;
    }
    let home = tempfile::tempdir().unwrap();
    let home = home.path().canonicalize().unwrap();
    std::fs::create_dir_all(home.join("project")).unwrap();
    // Longer than `sun_path` once the socket names are appended.
    let run = home.join("r".repeat(120));
    std::fs::create_dir_all(&run).unwrap();

    let mut cmd = Command::new(env!("CARGO_BIN_EXE_fresh"));
    cmd.current_dir(home.join("project"))
        .env("HOME", &home)
        .env("TMPDIR", &home)
        .env("XDG_CONFIG_HOME", home.join("config"))
        .env("XDG_DATA_HOME", home.join("data"))
        .env("XDG_STATE_HOME", home.join("state"))
        .env("XDG_CACHE_HOME", home.join("cache"))
        .env("XDG_RUNTIME_DIR", &run)
        .env("TERM", "xterm-256color")
        .env_remove("FRESH_SESSION")
        .env_remove("FRESH_BIN")
        .args(["--no-plugins", "-a", "startup-failure"]);
    let mut client = spawn_on_pty(cmd, ChildStdin::Terminal, 100, 30).expect("spawn fresh");

    // Bounded, because the failure mode is a client that never gives up.
    let reported = client.wait_for_cells_within(Duration::from_secs(60), |screen| {
        screen.contents().contains("exited during startup")
    });
    let screen = client.screen();
    client.kill();
    assert!(
        reported.is_ok(),
        "the client never reported the dead daemon:\n{screen}"
    );
    assert!(
        screen.contains("fresh-server-"),
        "the report should name the daemon's log:\n{screen}"
    );
}
