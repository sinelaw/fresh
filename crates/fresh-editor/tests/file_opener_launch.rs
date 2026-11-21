//! `fresh photo.fakeimg` opens the file with the plugin file opener that
//! claims it, not as bytes.
//!
//! At launch the plugins load on the plugin thread while the first frames
//! draw, so the file the command line names used to open before any plugin
//! had registered its opener: it landed in the byte view. The in-process
//! harness loads plugins synchronously and cannot see this, so this runs
//! the real binary on a pty, with the test opener plugin installed in an
//! isolated config directory.
#![cfg(any(target_os = "linux", target_os = "macos"))]

use crate::common::harness::copy_plugin_lib;
use crate::common::pty::{pty_available, spawn_on_pty, ChildStdin};
use std::path::Path;
use std::process::Command;

const CTRL_Q: &[u8] = &[0x11];
const COLS: u16 = 120;
const ROWS: u16 = 30;

const TEST_PLUGIN: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/tests/plugins/test_file_opener.ts"
));

/// `fresh` confined to `home`, with its language pinned so the English
/// predicates match.
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
        .env_remove("FRESH_BIN")
        .args(["--no-session", "--no-init", "--no-upgrade-check"])
        .args(args);
    cmd
}

#[test]
fn a_file_named_at_launch_opens_with_the_plugin_that_claims_it() {
    if !pty_available() {
        eprintln!("Skipping: no PTY available in this environment");
        return;
    }
    let home = tempfile::tempdir().unwrap();
    let plugins_dir = home.path().join("config").join("fresh").join("plugins");
    std::fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    std::fs::write(plugins_dir.join("test_file_opener.ts"), TEST_PLUGIN).unwrap();
    std::fs::write(
        home.path().join("photo.fakeimg"),
        b"\x89PNG\r\n\x1a\n\x00\x00\x00\rIHDR\x00\x01\x02\x03",
    )
    .unwrap();

    let mut editor = spawn_on_pty(
        isolated_fresh(home.path(), &["photo.fakeimg"]),
        ChildStdin::DevTty,
        COLS,
        ROWS,
    )
    .expect("spawn fresh on a pty");
    editor
        .wait_for_screen(|screen| screen.contains("VIEWER fake-viewer photo.fakeimg"))
        .expect("the file should open in the plugin's viewer");
    let screen = editor.screen();
    assert!(
        !screen.contains("[BIN]"),
        "the byte view should not be shown; screen:\n{screen}"
    );

    editor.send(CTRL_Q).expect("send Ctrl+Q");
    let status = editor.drain_and_wait().expect("wait for fresh to exit");
    assert!(status.success(), "Ctrl+Q should quit cleanly, got {status:?}");
}
