//! Starting a `fresh --server` of a test's own, ahead of its client.
//!
//! A test that runs its own daemon (to give it flags a client-spawned one
//! would not get, like `--no-plugins`) and then attaches with `fresh -a`
//! must not start the client until that daemon is listening. A client that
//! finds no live daemon for the session starts one itself, without those
//! flags, and when two daemons start for one session only one keeps it:
//! the client may be talking to its own.
//!
//! Linux-only, like its callers: the pid file sits in the `XDG_RUNTIME_DIR`
//! tree they isolate.
#![cfg(target_os = "linux")]

use std::path::Path;
use std::process::{Child, Command};
use std::time::Duration;

/// Spawn `command`, a `fresh --server …`, and return once it is listening
/// on its session's sockets.
///
/// Readiness is the daemon's own pid in `pid_file`, which it writes right
/// after binding its sockets: from then on a client sees it alive and
/// attaches rather than starting another. Waits for as long as that takes
/// (nextest supplies the outer timeout), and panics if the daemon exits
/// first (another daemon may own the session).
pub fn spawn_daemon(mut command: Command, pid_file: &Path) -> Child {
    let mut daemon = command.spawn().expect("spawn the daemon");
    let pid = daemon.id().to_string();
    loop {
        // Written with a plain `fs::write`, so it may be read half-done:
        // only a whole match counts.
        if std::fs::read_to_string(pid_file).is_ok_and(|s| s.trim() == pid) {
            return daemon;
        }
        if let Some(status) = daemon.try_wait().expect("poll the daemon") {
            panic!("the daemon exited before it was listening: {status}");
        }
        std::thread::sleep(Duration::from_millis(5));
    }
}
