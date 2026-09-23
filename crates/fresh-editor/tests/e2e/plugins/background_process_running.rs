//! Regression tests: `editor.isProcessRunning(id)` must report `true` for a
//! background process that is still running, and `false` once it has
//! exited.
//!
//! Previously `QuickJsBackend::is_process_running`
//! (crates/fresh-plugin-runtime/src/backend/quickjs_backend.rs) was a stub
//! that unconditionally returned `false`, although docs/plugins/api/buffer.md
//! and docs/quickjs.md document it as working (the pre-QuickJS Deno op
//! `op_fresh_is_process_running` did `child.try_wait()`).

#![cfg(unix)]

use crate::common::harness::{copy_plugin_lib, EditorTestHarness};
use fresh::config::Config;
use std::fs;
use std::path::{Path, PathBuf};

const PLUGIN_NAME: &str = "bgrun";
const PLUGIN_SOURCE: &str = r#"
/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();
const PIDFILE = "__PIDFILE__";
let checked = false;

// Learn the id from the first stdout line's hook payload. The process is
// guaranteed alive here: it is about to `exec sleep 300`.
registerHandler("bgrun_on_stdout", (e: { process_id: number; data: string }) => {
    if (checked || e.data.indexOf("ready") < 0) return;
    checked = true;
    const running = editor.isProcessRunning(e.process_id);
    editor.killBackgroundProcess(e.process_id);
    const afterKill = editor.isProcessRunning(e.process_id);
    editor.setStatus(`bgrun-running=${running} after-kill=${afterKill}`);
});
editor.on("onProcessStdout", "bgrun_on_stdout");

editor.spawnBackgroundProcess("sh", ["-c", `echo $$ > '${PIDFILE}'; echo ready; exec sleep 300`]);
"#;

/// A process that exits on its own: once its promise settles it is no
/// longer running.
const EXITING_PLUGIN_SOURCE: &str = r#"
/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();
const handle = editor.spawnBackgroundProcess("sh", ["-c", "exit 3"]);
handle.then((r: BackgroundProcessResult) => {
    const running = editor.isProcessRunning(handle.processId);
    editor.setStatus(`bgrun-exited code=${r.exit_code} running=${running}`);
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

#[test]
fn is_process_running_reports_live_background_process() {
    let (mut harness, _tmp, pid_file) = harness_with_plugin(PLUGIN_SOURCE);
    let _guard = PidFileGuard(pid_file.clone());

    // Semantic wait: the handler always reports one way or the other.
    harness
        .wait_until(|h| h.screen_to_string().contains("bgrun-running="))
        .unwrap();
    harness.assert_no_plugin_errors();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("bgrun-running=true after-kill=false"),
        "isProcessRunning should be true for a live background process and \
         false once it has been killed (pid {:?}). Screen:\n{}",
        read_pid(&pid_file),
        screen
    );
}

#[test]
fn is_process_running_false_after_process_exits() {
    let (mut harness, _tmp, _pid_file) = harness_with_plugin(EXITING_PLUGIN_SOURCE);

    harness
        .wait_until(|h| h.screen_to_string().contains("bgrun-exited"))
        .unwrap();
    harness.assert_no_plugin_errors();
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("bgrun-exited code=3 running=false"),
        "isProcessRunning should be false once the process has exited. Screen:\n{screen}"
    );
}
