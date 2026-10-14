//! A detached daemon keeps a log: its stderr — tracing, the boot lines,
//! panics — goes to `fresh-server-<PID>.log` in the logs directory rather
//! than the `/dev/null` the spawner gives it.
//!
//! Linux-gated like the other binary-driving tests: `XDG_*` isolation of the
//! state and socket trees works there.
#![cfg(target_os = "linux")]

use std::path::Path;
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

/// A `fresh` whose config, state and sockets all live under `home`.
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
        .env_remove("RUST_LOG")
        .env_remove("FRESH_SESSION")
        .env_remove("FRESH_BIN");
    cmd
}

#[test]
fn a_detached_daemon_writes_its_log_file() {
    let home = tempfile::tempdir().unwrap();
    let home = home.path().canonicalize().unwrap();
    std::fs::create_dir_all(home.join("project")).unwrap();
    std::fs::create_dir_all(home.join("run")).unwrap();

    // Spawned the way `spawn_server_detached` does: stdio at /dev/null.
    let mut server = isolated_fresh(&home)
        .args(["--server", "--session-name", "log-file", "--no-plugins"])
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .expect("spawn the daemon");
    let log = home
        .join("state/fresh/logs")
        .join(format!("fresh-server-{}.log", server.id()));

    // Bounded, because the failure mode is a log that never appears.
    let deadline = Instant::now() + Duration::from_secs(30);
    let mut contents = String::new();
    while Instant::now() < deadline {
        contents = std::fs::read_to_string(&log).unwrap_or_default();
        if contents.contains("Editor server starting") {
            break;
        }
        std::thread::sleep(Duration::from_millis(50));
    }
    let _ = server.kill();
    let _ = server.wait();

    assert!(
        contents.contains("[server] Starting server process"),
        "the boot lines never reached {}:\n{contents}",
        log.display()
    );
    assert!(
        contents.contains("Editor server starting"),
        "tracing never reached {}:\n{contents}",
        log.display()
    );
}
