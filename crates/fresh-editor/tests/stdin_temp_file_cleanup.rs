//! The stdin spool file does not outlive the process (#3134).
//!
//! `echo … | fresh -` drains the pipe into `$TMPDIR/fresh-stdin-<pid>.tmp`
//! and keeps reading chunks back off it — a large stdin is loaded lazily —
//! so the file has to survive the buffer and can only be removed by the
//! process itself. Nothing removed it: quitting left the file behind, and
//! the only thing that ever deleted it was the 24h sweep in
//! `log_dirs::cleanup_stale_logs`. Piping a decompressed log in meant a
//! copy of it sitting in `/tmp` for a day.
//!
//! This drives the real binary, because the property belongs to the
//! process: registration happens where the file is created and the
//! deletion happens on the way out of `real_main`. Both paths out are
//! covered — a clean quit, and an early failure that never reaches the
//! editor.
//!
//! Linux-gated with `common::pty`, which needs `ptsname_r`.
#![cfg(target_os = "linux")]

use crate::common::pty::{spawn_on_pty, ChildStdin};
use std::path::{Path, PathBuf};
use std::process::Command;

const CTRL_Q: &[u8] = &[0x11];

/// Spool files left in `dir`.
fn spool_files(dir: &Path) -> Vec<PathBuf> {
    std::fs::read_dir(dir)
        .expect("read temp dir")
        .filter_map(|entry| {
            let path = entry.ok()?.path();
            let name = path.file_name()?.to_string_lossy().into_owned();
            name.starts_with("fresh-stdin-").then_some(path)
        })
        .collect()
}

/// `fresh` with its temp dir, config and state confined to `home`, so the
/// only `fresh-stdin-*` file that can appear under `$TMPDIR` is this test's.
fn isolated_fresh(home: &Path) -> Command {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_fresh"));
    cmd.current_dir(home)
        .env("TMPDIR", home)
        .env("HOME", home)
        .env("XDG_CONFIG_HOME", home.join("config"))
        .env("XDG_DATA_HOME", home.join("data"))
        .env("XDG_STATE_HOME", home.join("state"))
        .env("XDG_CACHE_HOME", home.join("cache"))
        .env("TERM", "xterm-256color");
    cmd
}

/// The reported case: pipe something in, quit with Ctrl+Q, and the spool
/// file is gone.
#[test]
fn quitting_deletes_the_stdin_spool_file() {
    let home = tempfile::tempdir().unwrap();

    let mut cmd = isolated_fresh(home.path());
    cmd.args(["--no-session", "--no-init", "--no-upgrade-check", "-"]);

    let mut editor = spawn_on_pty(
        cmd,
        ChildStdin::Piped(b"hello-from-stdin\n".to_vec()),
        100,
        30,
    )
    .expect("spawn fresh on a pty");

    // Semantic wait: the editor is up once it has drawn its menu bar.
    editor
        .wait_for_screen(|screen| screen.contains("File") && screen.contains("Help"))
        .expect("editor should render");

    // It really did spool stdin to a file — otherwise the assertion after
    // the quit would pass without testing anything.
    assert!(
        !spool_files(home.path()).is_empty(),
        "expected a fresh-stdin-* file while the editor is running; \
         temp dir held: {:?}",
        std::fs::read_dir(home.path())
            .unwrap()
            .filter_map(|e| Some(e.ok()?.file_name()))
            .collect::<Vec<_>>()
    );

    editor.send(CTRL_Q).expect("send Ctrl+Q");
    let status = editor.wait().expect("wait for fresh to exit");
    assert!(status.success(), "fresh exited abnormally: {status:?}");

    assert_eq!(
        spool_files(home.path()),
        Vec::<PathBuf>::new(),
        "the stdin spool file should be gone once fresh has exited"
    );
}

/// The other way out: the spool file is created before stdin is reopened
/// from `/dev/tty`, so a launch that fails right there used to leak it too.
/// Here that failure is forced by giving the child no controlling terminal
/// at all, which is also what a `fresh -` run from a non-tty context does.
#[test]
fn a_failed_launch_does_not_leak_the_stdin_spool_file() {
    use std::io::Write;
    use std::os::unix::process::CommandExt;
    use std::process::Stdio;

    let home = tempfile::tempdir().unwrap();

    let mut cmd = isolated_fresh(home.path());
    cmd.arg("-")
        .stdin(Stdio::piped())
        .stdout(Stdio::null())
        .stderr(Stdio::null());

    // SAFETY: between fork and exec; `setsid` is async-signal-safe. Without
    // a controlling terminal, opening `/dev/tty` fails with ENXIO, so the
    // child cannot reach the editor loop and is guaranteed to exit.
    unsafe {
        cmd.pre_exec(|| {
            libc::setsid();
            Ok(())
        });
    }

    let mut child = cmd.spawn().expect("spawn fresh");
    child
        .stdin
        .take()
        .expect("piped stdin")
        .write_all(b"hello-from-stdin\n")
        .expect("write to fresh's stdin");

    let status = child.wait().expect("wait for fresh to exit");
    assert!(
        !status.success(),
        "expected the launch to fail without a controlling terminal, got {status:?}"
    );

    assert_eq!(
        spool_files(home.path()),
        Vec::<PathBuf>::new(),
        "a failed launch should not leave a stdin spool file behind"
    );
}
