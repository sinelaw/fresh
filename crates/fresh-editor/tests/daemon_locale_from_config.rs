//! A daemon speaks the language the config file asks for (#3149).
//!
//! `locale` in `config.json` reached i18n in exactly one place —
//! `initialize_app` — and daemon mode returns before it: `run_server_command`
//! loads the very same config and never told i18n about it. The only init
//! that ran for a daemon was the pre-clap one, which reads argv and the
//! environment and has no config to consult. So `fresh calc.py` came up in
//! Japanese while `fresh --cmd daemon new mysession` came up in English, and
//! the documented way to pick a UI language silently did nothing on the
//! daemon path.
//!
//! The daemon is the process that renders the UI, so this has to be observed
//! through a real one: the client's screen is the daemon's screen. Driving
//! the binary is also the only way to see the second half of the fix — the
//! `--locale` the client was given has to be forwarded across the spawn, and
//! nothing in-process can show that.
//!
//! Linux-gated: `XDG_CONFIG_HOME`/`XDG_RUNTIME_DIR` are what isolate the
//! config the daemon reads and the socket it binds, and `dirs` honours them
//! only on Linux. `common::pty` is Linux-only for the same kind of reason.
#![cfg(target_os = "linux")]

use crate::common::pty::{pty_available, spawn_on_pty, ChildStdin};
use std::path::Path;
use std::process::Command;

/// The menu bar, in each language. `Help`/`ヘルプ` is the marker that the
/// editor has drawn at all; `File`/`ファイル` is what the locale decides.
const HELP_EN: &str = "Help";
const HELP_JA: &str = "ヘルプ";
const FILE_EN: &str = "File";
const FILE_JA: &str = "ファイル";

/// A `fresh` whose config, state and sockets all live under `home`, with an
/// environment that asks for no particular language — so anything but
/// English on screen came from the config file or the command line, not
/// from `LANG`.
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
        .env_remove("LC_ALL")
        .env_remove("LC_MESSAGES");
    cmd
}

/// Build the isolated tree and the `config.json` the daemon will read —
/// carrying `locale` when the test asks for one. The update check is off in
/// both shapes: the daemon reads this file for itself, and a test should not
/// depend on reaching the network.
fn setup(home: &Path, locale: Option<&str>) {
    std::fs::create_dir_all(home.join("project")).unwrap();
    std::fs::create_dir_all(home.join("run")).unwrap();
    let config_dir = home.join("config").join("fresh");
    std::fs::create_dir_all(&config_dir).unwrap();

    let locale = locale
        .map(|locale| format!("  \"locale\": \"{locale}\",\n"))
        .unwrap_or_default();
    std::fs::write(
        config_dir.join("config.json"),
        format!("{{\n{locale}  \"check_for_updates\": false\n}}\n"),
    )
    .unwrap();
}

/// Stop the daemon this test started, so it does not idle on in the
/// background holding a socket in the temp tree.
fn kill_daemon(home: &Path, session: &str) {
    let pid_file = home.join("run").join("fresh").join(format!("{session}.pid"));
    if let Ok(pid) = std::fs::read_to_string(&pid_file) {
        if let Ok(pid) = pid.trim().parse::<i32>() {
            // SAFETY: a plain `kill(2)`; an already-dead pid just returns
            // ESRCH, which is ignored.
            unsafe { libc::kill(pid, libc::SIGKILL) };
        }
    }
}

/// Attach to a freshly spawned daemon and return its rendered screen once
/// the menu bar is up.
fn daemon_screen(home: &Path, session: &str, extra_args: &[&str]) -> String {
    let mut cmd = isolated_fresh(home);
    cmd.args(extra_args);
    cmd.args(["--cmd", "daemon", "new", session]);

    let mut client = spawn_on_pty(cmd, ChildStdin::Terminal, 100, 30).expect("spawn fresh on a pty");

    // Semantic wait: the daemon has rendered once a menu bar is on screen,
    // in either language. Waiting on *both* spellings is what makes the
    // assertion afterwards meaningful — waiting on the Japanese one alone
    // would turn a regression into a hang.
    let rendered =
        client.wait_for_screen(|screen| screen.contains(HELP_EN) || screen.contains(HELP_JA));

    let screen = client.screen();
    client.kill();
    kill_daemon(home, session);

    rendered.unwrap_or_else(|e| panic!("daemon never rendered a menu bar: {e}"));
    screen
}

/// The reported case: `locale` in `config.json`, nothing on the command
/// line, nothing in the environment.
#[test]
fn daemon_takes_its_locale_from_the_config_file() {
    if !pty_available() {
        eprintln!("Skipping: no PTY available in this environment");
        return;
    }
    let home = tempfile::tempdir().unwrap();
    setup(home.path(), Some("ja"));

    let screen = daemon_screen(home.path(), "locale-from-config", &[]);

    assert!(
        screen.contains(FILE_JA),
        "expected a Japanese menu bar from config.json's locale, got:\n{screen}"
    );
}

/// The other half of the fix: `--locale` given to the client has to reach
/// the daemon it spawns, which is a different process with a different
/// command line.
#[test]
fn daemon_takes_its_locale_from_the_clients_flag() {
    if !pty_available() {
        eprintln!("Skipping: no PTY available in this environment");
        return;
    }
    let home = tempfile::tempdir().unwrap();
    setup(home.path(), None);

    let screen = daemon_screen(home.path(), "locale-from-flag", &["--locale", "ja"]);

    assert!(
        screen.contains(FILE_JA),
        "expected --locale to be forwarded to the daemon, got:\n{screen}"
    );
}

/// The control: with no `locale` in the config and none on the command
/// line, the daemon is English — so the two assertions above are reading a
/// locale that was actually applied, not one the environment supplied.
#[test]
fn daemon_is_english_without_a_locale_anywhere() {
    if !pty_available() {
        eprintln!("Skipping: no PTY available in this environment");
        return;
    }
    let home = tempfile::tempdir().unwrap();
    setup(home.path(), None);

    let screen = daemon_screen(home.path(), "locale-default", &[]);

    assert!(
        screen.contains(FILE_EN),
        "expected the default English menu bar, got:\n{screen}"
    );
}
