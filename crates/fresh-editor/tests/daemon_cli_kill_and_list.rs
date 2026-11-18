//! `fresh --cmd daemon kill/list` (#3782): `kill --all` terminates every
//! daemon, `kill NAME` on a crashed daemon removes its stale sockets, and
//! `list` leaves out directly launched editors' control sockets.
//!
//! Driven through the binary, Linux-gated like the other binary-driving
//! tests: `XDG_*` isolation of the socket tree works there.
#![cfg(target_os = "linux")]

use crate::common::daemon::spawn_daemon;
use std::path::Path;
use std::process::{Child, Command, Output, Stdio};
use std::time::{Duration, Instant};

/// A `fresh` command whose config, state and sockets all live under `home`.
fn isolated(home: &Path) -> Command {
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
        .stdin(Stdio::null());
    cmd
}

fn setup() -> tempfile::TempDir {
    let home = tempfile::tempdir().unwrap();
    std::fs::create_dir_all(home.path().join("project")).unwrap();
    std::fs::create_dir_all(home.path().join("run")).unwrap();
    home
}

fn socket_dir(home: &Path) -> std::path::PathBuf {
    home.join("run").join("fresh")
}

fn start_daemon(home: &Path, session: &str) -> Child {
    let mut cmd = isolated(home);
    cmd.args(["--server", "--session-name", session, "--no-plugins"])
        .stdout(Stdio::null())
        .stderr(Stdio::null());
    spawn_daemon(cmd, &socket_dir(home).join(format!("{session}.pid")))
}

fn daemon_cmd(home: &Path, args: &[&str]) -> Output {
    isolated(home)
        .arg("--cmd")
        .arg("daemon")
        .args(args)
        .output()
        .expect("run fresh --cmd daemon")
}

/// Wait up to a few seconds for `child` to exit.
fn exited(child: &mut Child) -> bool {
    let start = Instant::now();
    while start.elapsed() < Duration::from_secs(10) {
        if child.try_wait().unwrap().is_some() {
            return true;
        }
        std::thread::sleep(Duration::from_millis(20));
    }
    false
}

#[test]
fn kill_all_terminates_every_daemon() {
    let home = setup();
    let home = home.path();
    let mut first = start_daemon(home, "first");
    let mut second = start_daemon(home, "second");

    let out = daemon_cmd(home, &["kill", "--all"]);
    let first_exited = exited(&mut first);
    let second_exited = exited(&mut second);
    let _ = first.kill();
    let _ = second.kill();
    let _ = first.wait();
    let _ = second.wait();

    assert!(out.status.success(), "{:?}", out);
    assert!(first_exited, "daemon 'first' still running: {:?}", out);
    assert!(second_exited, "daemon 'second' still running: {:?}", out);
    assert!(
        String::from_utf8_lossy(&out.stdout).contains("Terminated 2 daemon(s)"),
        "{:?}",
        out
    );
}

#[test]
fn kill_on_a_crashed_daemon_removes_its_sockets() {
    let home = setup();
    let home = home.path();
    let mut daemon = start_daemon(home, "crashed");
    // SIGKILL: the daemon gets no chance to remove its sockets.
    daemon.kill().unwrap();
    daemon.wait().unwrap();
    let control = socket_dir(home).join("crashed.ctrl.sock");
    assert!(control.exists(), "precondition: the socket outlives the crash");

    let out = daemon_cmd(home, &["kill", "crashed"]);

    assert!(out.status.success(), "{:?}", out);
    assert!(!control.exists(), "stale control socket left behind");
    assert!(!socket_dir(home).join("crashed.data.sock").exists());
}

#[test]
fn list_leaves_out_direct_editors_control_sockets() {
    let home = setup();
    let home = home.path();
    let mut daemon = start_daemon(home, "real");

    // A live direct editor's control socket (its pid is this test process)
    // and a stale one (a pid that is not running).
    let dir = socket_dir(home);
    let live = format!("local-{}-abc123", std::process::id());
    let stale = "local-999999999-def456";
    for (name, pid) in [(live.as_str(), std::process::id()), (stale, 999_999_999)] {
        std::fs::write(dir.join(format!("{name}.ctrl.sock")), "").unwrap();
        std::fs::write(dir.join(format!("{name}.data.sock")), "").unwrap();
        std::fs::write(dir.join(format!("{name}.pid")), pid.to_string()).unwrap();
    }

    let out = daemon_cmd(home, &["list"]);
    let _ = daemon.kill();
    let _ = daemon.wait();

    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(out.status.success(), "{:?}", out);
    assert!(stdout.contains("real"), "the daemon is listed: {stdout}");
    assert!(!stdout.contains("local-"), "control sockets listed: {stdout}");
    assert!(
        !stderr.contains("stale daemon"),
        "a direct editor's socket isn't a stale daemon: {stderr}"
    );
    assert!(
        !dir.join(format!("{stale}.ctrl.sock")).exists(),
        "the dead editor's socket is cleaned up"
    );
    assert!(dir.join(format!("{live}.ctrl.sock")).exists());
}
