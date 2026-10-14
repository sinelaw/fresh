//! Regression tests: `editor.killBackgroundProcess(id)` must terminate the
//! OS process, and the `spawnBackgroundProcess` promise must settle.
//!
//! Previously `handle_spawn_background_process` (app/plugin_dispatch.rs)
//! spawned a `tokio::process::Command` without `.kill_on_drop(true)`, and
//! `handle_kill_background_process` only aborted the tokio task. Aborting
//! dropped the `Child` without killing it, so the process kept running as
//! an orphan, and the aborted task never sent `ProcessExit`, so the JS
//! promise returned by `spawnBackgroundProcess` never settled.
//!
//! Linux-only because it reads /proc to tell a live process from a zombie.

#![cfg(target_os = "linux")]

use crate::common::harness::{copy_plugin_lib, EditorTestHarness};
use fresh::config::Config;
use std::fs;
use std::path::{Path, PathBuf};

const PLUGIN_NAME: &str = "bgkill";

/// Kills via `editor.killBackgroundProcess(id)`, learning the id from the
/// first stdout line's hook payload.
const PLUGIN_SOURCE_BY_ID: &str = r#"
/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();
const PIDFILE = "__PIDFILE__";
let killed = false;

registerHandler("bgkill_on_stdout", (e: { process_id: number; data: string }) => {
    if (killed || e.data.indexOf("ready") < 0) return;
    killed = true;
    const ok = editor.killBackgroundProcess(e.process_id);
    editor.setStatus(`bgkill-sent ok=${ok}`);
});
editor.on("onProcessStdout", "bgkill_on_stdout");

editor
    .spawnBackgroundProcess("sh", ["-c", `echo $$ > '${PIDFILE}'; echo ready; exec sleep 300`])
    .then((r: BackgroundProcessResult) => {
        editor.setStatus(`bgkill-exit=${r.exit_code}`);
    });
"#;

/// Kills via the handle returned by `spawnBackgroundProcess`
/// (`handle.processId` / `handle.kill()`).
const PLUGIN_SOURCE_BY_HANDLE: &str = r#"
/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();
const PIDFILE = "__PIDFILE__";
let killed = false;

const handle = editor.spawnBackgroundProcess(
    "sh",
    ["-c", `echo $$ > '${PIDFILE}'; echo ready; exec sleep 300`],
);

registerHandler("bgkill_on_stdout", (e: { process_id: number; data: string }) => {
    if (killed || e.data.indexOf("ready") < 0) return;
    killed = true;
    const sameId = e.process_id === handle.processId;
    handle.kill().then((ok: boolean) => {
        editor.setStatus(`bgkill-sent ok=${ok} same_id=${sameId}`);
    });
});
editor.on("onProcessStdout", "bgkill_on_stdout");

handle.then((r: BackgroundProcessResult) => {
    editor.setStatus(`bgkill-exit=${r.exit_code}`);
});
"#;

/// SIGKILLs whatever pid the plugin recorded, so a failing run doesn't
/// leave a 300s `sleep` behind.
struct PidFileGuard(PathBuf);

impl Drop for PidFileGuard {
    fn drop(&mut self) {
        if let Some(pid) = read_pid(&self.0) {
            // SAFETY: plain kill(2); ESRCH for an already-dead pid is ignored.
            unsafe { libc::kill(pid, libc::SIGKILL) };
        }
    }
}

fn read_pid(path: &Path) -> Option<i32> {
    fs::read_to_string(path).ok()?.trim().parse::<i32>().ok()
}

/// True if `pid` exists and is not a zombie. A killed child may linger as
/// a zombie until it is reaped, so `kill(pid, 0)` alone would report it
/// alive; /proc/<pid>/stat's state field distinguishes it.
fn process_alive(pid: i32) -> bool {
    match fs::read_to_string(format!("/proc/{pid}/stat")) {
        Ok(stat) => {
            let state = stat
                .rsplit_once(')')
                .and_then(|(_, rest)| rest.trim_start().chars().next());
            !matches!(state, None | Some('Z') | Some('X'))
        }
        Err(_) => false,
    }
}

fn harness_with_plugin(plugin_source: &str) -> (EditorTestHarness, tempfile::TempDir, PathBuf) {
    let temp = tempfile::TempDir::new().expect("tempdir");
    let working_dir = temp.path().join("work");
    fs::create_dir_all(&working_dir).unwrap();
    let plugins_dir = working_dir.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    let pid_file = temp.path().join("bg.pid");

    let source = plugin_source.replace("__PIDFILE__", pid_file.to_str().unwrap());
    fs::write(plugins_dir.join(format!("{}.ts", PLUGIN_NAME)), source).unwrap();
    copy_plugin_lib(&plugins_dir);

    let harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Config::default(), working_dir)
            .expect("harness");
    (harness, temp, pid_file)
}

/// Waits until the process has started, written its pid and the plugin
/// has issued the kill; returns the OS pid.
fn spawn_and_kill(h: &mut EditorTestHarness, pid_file: &Path) -> i32 {
    h.wait_until(|h| read_pid(pid_file).is_some() && h.screen_to_string().contains("bgkill-sent"))
        .unwrap();
    h.assert_no_plugin_errors();
    read_pid(pid_file).unwrap()
}

#[test]
fn kill_background_process_terminates_os_process() {
    let (mut harness, _tmp, pid_file) = harness_with_plugin(PLUGIN_SOURCE_BY_ID);
    let _guard = PidFileGuard(pid_file.clone());
    let pid = spawn_and_kill(&mut harness, &pid_file);
    assert!(
        harness.screen_to_string().contains("bgkill-sent ok=true"),
        "killBackgroundProcess should report success. Screen:\n{}",
        harness.screen_to_string()
    );

    // Hangs (and nextest times out) if the process was orphaned.
    harness.wait_until(|_| !process_alive(pid)).unwrap();
}

#[test]
fn kill_background_process_settles_spawn_promise() {
    let (mut harness, _tmp, pid_file) = harness_with_plugin(PLUGIN_SOURCE_BY_ID);
    let _guard = PidFileGuard(pid_file.clone());
    spawn_and_kill(&mut harness, &pid_file);

    // Hangs (and nextest times out) if the promise never settles.
    harness
        .wait_until(|h| h.screen_to_string().contains("bgkill-exit="))
        .unwrap();
}

#[test]
fn background_process_handle_kill_terminates_and_settles() {
    let (mut harness, _tmp, pid_file) = harness_with_plugin(PLUGIN_SOURCE_BY_HANDLE);
    let _guard = PidFileGuard(pid_file.clone());
    let pid = spawn_and_kill(&mut harness, &pid_file);
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("bgkill-sent ok=true same_id=true"),
        "handle.processId should match the hook's process_id and handle.kill() \
         should succeed. Screen:\n{screen}"
    );

    harness.wait_until(|_| !process_alive(pid)).unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("bgkill-exit="))
        .unwrap();
}
