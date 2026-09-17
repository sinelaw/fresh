//! A terminal opened in a daemon-backed editor must advertise
//! `FRESH_SESSION`, and a bare `fresh` typed inside one must not attach to
//! the daemon it is running in.
//!
//! Two halves of the same report. Orchestrator mode made the daemon the
//! ordinary way to run Fresh — a bare `fresh` hands the launch to the shared
//! daemon — and the daemon was the one editor process that never bound its
//! local control socket. `main.rs` (TUI), `gui::run` and `webui::run` all
//! call `local_control::start()`; `EditorServer::run` only ever *pumped* it.
//! So `local_session_id()` was `None` there and the terminal spawner, which
//! reads exactly that to decide, stamped `FRESH_BIN` into every child and
//! `FRESH_SESSION` into none. What the user saw in a terminal buffer was one
//! variable set and the other empty, and a nested `fresh FILE` opening a
//! second editor instead of handing the file to the one already on screen.
//!
//! The second half is what that empty variable was hiding. `fresh` with an
//! empty command line takes the Orchestrator launch, which attaches to the
//! shared daemon — and a terminal buffer *inside that daemon* is exactly
//! where it can be typed. The daemon then renders one shared screen into a
//! client living inside its own output: not a second editor but a feedback
//! loop, a pane of shredded frames. `FRESH_SESSION` is the signal that says
//! "you are already inside one", so the fix above is what makes this one
//! possible at all.
//!
//! Both are observed through the real binary, because both are about what a
//! separate process does: the first reads the environment a PTY child was
//! handed, the second asks whether a daemon was ever started.
//!
//! Linux-gated like the other binary-driving tests: `XDG_*` isolation of the
//! config and socket trees works there, and `common::pty` is Linux-only.
#![cfg(target_os = "linux")]

use crate::common::pty::{pty_available, spawn_on_pty, ChildStdin};
use std::path::Path;
use std::process::Command;

/// Printed by the terminal's own shell, so the assertion reads the value the
/// child actually got. The angle brackets are what make the two outcomes
/// distinguishable on screen: `<local-…>` is a session, `<>` is the bug, and
/// the echoed command line — which contains the variable's *name* — is
/// neither.
const MARKER: &str = "FRESH_SESSION_IS:";

/// The menu bar, i.e. "the editor has drawn".
const MENU_BAR: &str = "Help";

/// Default binding for "Open Terminal in Utility Dock": Alt+` .
const OPEN_TERMINAL_IN_DOCK: &[u8] = b"\x1b`";

/// The status message a terminal's arrival puts up. The rest of it —
/// "(Ctrl+Space to exit)" — is cut by the status bar's own field width at
/// any terminal size worth testing at, so the prefix is what can be waited
/// on.
const TERMINAL_OPENED: &str = "Terminal 0 opened";

/// A `fresh` whose config, state and sockets all live under `home`, so the
/// daemon this test starts cannot meet one from another test — or from the
/// developer running it.
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
        .env("TERM", "xterm-256color")
        .env("LANG", "C.UTF-8")
        .env_remove("FRESH_SESSION")
        .env_remove("FRESH_BIN");
    cmd
}

/// Build the isolated tree and a config that reaches for no network.
fn setup(home: &Path) {
    std::fs::create_dir_all(home.join("project")).unwrap();
    std::fs::create_dir_all(home.join("run")).unwrap();
    let config_dir = home.join("config").join("fresh");
    std::fs::create_dir_all(&config_dir).unwrap();
    std::fs::write(
        config_dir.join("config.json"),
        "{\n  \"check_for_updates\": false\n}\n",
    )
    .unwrap();
}

/// The pid file a daemon named `session` writes under this isolated tree.
fn daemon_pid_file(home: &Path, session: &str) -> std::path::PathBuf {
    home.join("run").join("fresh").join(format!("{session}.pid"))
}

/// Stop the daemon this test started, so it does not idle on in the
/// background holding a socket in the temp tree.
fn kill_daemon(home: &Path, session: &str) {
    if let Ok(pid) = std::fs::read_to_string(daemon_pid_file(home, session)) {
        if let Ok(pid) = pid.trim().parse::<i32>() {
            // SAFETY: a plain `kill(2)`; an already-dead pid just returns
            // ESRCH, which is ignored.
            unsafe { libc::kill(pid, libc::SIGKILL) };
        }
    }
}

/// The reported case: open a terminal in a daemon-backed editor and read
/// back what the shell in it was given.
#[test]
fn a_daemon_hosted_terminal_advertises_fresh_session() {
    if !pty_available() {
        eprintln!("Skipping: no PTY available in this environment");
        return;
    }
    let home = tempfile::tempdir().unwrap();
    let home = home.path();
    setup(home);
    let session = "terminal-session-advert";

    let mut cmd = isolated_fresh(home);
    cmd.args(["--cmd", "daemon", "new", session]);
    // Wider than the other two: the status bar gives its message a share of
    // the width, and at 100 columns the marker above is cut to "Te...".
    let mut client = spawn_on_pty(cmd, ChildStdin::Terminal, 140, 30).expect("spawn fresh on a pty");

    client
        .wait_for_screen(|screen| screen.contains(MENU_BAR))
        .unwrap_or_else(|e| panic!("daemon never rendered a menu bar: {e}"));

    client.send(OPEN_TERMINAL_IN_DOCK).unwrap();
    client
        .wait_for_screen(|screen| screen.contains(TERMINAL_OPENED))
        .unwrap_or_else(|e| panic!("no terminal opened in the dock: {e}"));

    // The shell may still be starting; the pty buffers the line until it
    // reads, so there is nothing to wait for here.
    client
        .send(format!("echo \"{MARKER}<$FRESH_SESSION>\"\r").as_bytes())
        .unwrap();

    // Wait for *either* outcome, so the regression fails with a screen
    // rather than hanging until the outer timeout.
    let waited = client.wait_for_screen(|screen| {
        screen.contains(&format!("{MARKER}<local-")) || screen.contains(&format!("{MARKER}<>"))
    });
    let screen = client.screen();
    client.kill();
    kill_daemon(home, session);

    waited.unwrap_or_else(|e| panic!("the terminal never echoed the variable: {e}"));
    assert!(
        screen.contains(&format!("{MARKER}<local-")),
        "a terminal in a daemon-backed editor got no FRESH_SESSION, so a nested \
         `fresh` cannot reach its parent; screen was:\n{screen}"
    );
}

/// A bare `fresh` inside a Fresh terminal is an ordinary inline editor, not
/// another client of the daemon it is sitting in.
///
/// Asserted on whether a daemon was started at all, which is the thing that
/// goes wrong: the rendering itself cannot be told apart from a working
/// editor by looking at one screen, but a shared daemon leaves a pid file.
#[test]
fn a_bare_fresh_inside_a_session_does_not_attach_to_the_daemon() {
    if !pty_available() {
        eprintln!("Skipping: no PTY available in this environment");
        return;
    }
    let home = tempfile::tempdir().unwrap();
    let home = home.path();
    setup(home);

    let mut cmd = isolated_fresh(home);
    // Any non-empty value: the point is that we are inside *a* Fresh
    // terminal, and an unreachable session still means the same thing.
    cmd.env("FRESH_SESSION", "local-1-deadbeef");
    let mut client = spawn_on_pty(cmd, ChildStdin::Terminal, 100, 30).expect("spawn fresh on a pty");

    let waited = client.wait_for_screen(|screen| screen.contains(MENU_BAR));
    let screen = client.screen();
    client.kill();
    let attached = daemon_pid_file(home, fresh::server::ORCHESTRATOR_DAEMON).exists();
    kill_daemon(home, fresh::server::ORCHESTRATOR_DAEMON);

    waited.unwrap_or_else(|e| panic!("the nested editor never rendered: {e}"));
    assert!(
        !attached,
        "a bare `fresh` inside a Fresh terminal attached to the shared daemon, \
         which renders the daemon's screen back into its own terminal buffer; \
         screen was:\n{screen}"
    );
}

/// The control: the same launch *outside* a Fresh terminal still takes the
/// Orchestrator path, so the assertion above is reading the guard and not a
/// mode that was never on.
#[test]
fn a_bare_fresh_outside_a_session_still_starts_the_daemon() {
    if !pty_available() {
        eprintln!("Skipping: no PTY available in this environment");
        return;
    }
    let home = tempfile::tempdir().unwrap();
    let home = home.path();
    setup(home);

    let cmd = isolated_fresh(home);
    let mut client = spawn_on_pty(cmd, ChildStdin::Terminal, 100, 30).expect("spawn fresh on a pty");

    let waited = client.wait_for_screen(|screen| screen.contains(MENU_BAR));
    let screen = client.screen();
    client.kill();
    let attached = daemon_pid_file(home, fresh::server::ORCHESTRATOR_DAEMON).exists();
    kill_daemon(home, fresh::server::ORCHESTRATOR_DAEMON);

    waited.unwrap_or_else(|e| panic!("the editor never rendered: {e}"));
    assert!(
        attached,
        "a bare `fresh` on a terminal should have launched into Orchestrator \
         mode; screen was:\n{screen}"
    );
}
