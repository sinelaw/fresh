//! Regression for issue #2028: when a user's init.ts disables the
//! dashboard auto-open via the exported plugin API
//! (`getPluginApi("dashboard")?.setAutoOpen(false)`), that
//! preference must be honored on the very first `ready` hook —
//! the dashboard must NOT briefly auto-open and then have to be
//! dismissed.
//!
//! Pre-fix, init.ts was queued through the asynchronous load
//! path and the editor fired `ready` before waiting for the
//! evaluation to settle. The dashboard's `ready` handler then
//! observed `autoOpenOverride === null` and opened itself.

mod common;

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use fresh::config::Config;
use fresh::config_io::DirectoryContext;
use std::fs;
use tempfile::TempDir;

#[test]
fn init_ts_set_auto_open_false_is_honored_on_first_ready() {
    let temp = TempDir::new().unwrap();
    let dir_context = DirectoryContext::for_testing(temp.path());
    let config_dir = dir_context.config_dir.clone();
    fs::create_dir_all(&config_dir).unwrap();

    let working_dir = temp.path().join("project");
    fs::create_dir_all(&working_dir).unwrap();
    let plugins_dir = working_dir.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "dashboard");
    copy_plugin_lib(&plugins_dir);

    // User init.ts opts out of dashboard auto-open. The optional
    // chaining matches the recommended snippet in docs.
    fs::write(
        config_dir.join("init.ts"),
        r#"const editor = getEditor();
(editor.getPluginApi("dashboard") as { setAutoOpen(b: boolean): void } | null)
    ?.setAutoOpen(false);
"#,
    )
    .unwrap();

    let mut harness = EditorTestHarness::with_shared_dir_context(
        120,
        40,
        Config::default(),
        working_dir.clone(),
        dir_context,
    )
    .expect("harness");

    // Production order (see main.rs::real_main): load init.ts,
    // then fire `ready`. The bug is purely about whether init.ts
    // has settled before `ready` reaches the dashboard handler.
    harness.editor_mut().load_init_script(true);
    harness.editor_mut().fire_ready_hook();

    // Give the editor a generous budget for everything to settle.
    // Using `wait_until` with a predicate that's always false
    // would just spin to timeout; instead, drive the async loop
    // for a fixed wall-clock window and then assert the
    // *invariant* (no Dashboard buffer exists) at the end. The
    // 1.5s budget covers a slow CI's full ready-handler round
    // trip without making a passing test slow.
    let deadline = std::time::Instant::now() + std::time::Duration::from_millis(1500);
    while std::time::Instant::now() < deadline {
        harness.tick_and_render().unwrap();
        std::thread::sleep(std::time::Duration::from_millis(50));
    }

    let has_dashboard = harness
        .editor()
        .active_window()
        .buffer_metadata
        .values()
        .any(|m| matches!(&m.kind, fresh::app::types::BufferKind::Virtual { mode } if mode == "dashboard"));
    assert!(
        !has_dashboard,
        "init.ts called setAutoOpen(false) before the first `ready`; the \
         dashboard buffer must not have been opened"
    );
}

/// Same expectation, but exercising the async load path that
/// production uses (`load_init_script_async`). Pre-fix, this
/// would queue init.ts behind the plugin-thread FIFO and then
/// immediately fire `ready` — racing the evaluation. The fix
/// has the editor block on init.ts settling before any
/// subsequent hook dispatches.
#[test]
fn init_ts_async_set_auto_open_false_wins_the_race_with_ready() {
    let temp = TempDir::new().unwrap();
    let dir_context = DirectoryContext::for_testing(temp.path());
    let config_dir = dir_context.config_dir.clone();
    fs::create_dir_all(&config_dir).unwrap();

    let working_dir = temp.path().join("project");
    fs::create_dir_all(&working_dir).unwrap();
    let plugins_dir = working_dir.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "dashboard");
    copy_plugin_lib(&plugins_dir);

    fs::write(
        config_dir.join("init.ts"),
        r#"const editor = getEditor();
(editor.getPluginApi("dashboard") as { setAutoOpen(b: boolean): void } | null)
    ?.setAutoOpen(false);
"#,
    )
    .unwrap();

    let mut harness = EditorTestHarness::with_shared_dir_context(
        120,
        40,
        Config::default(),
        working_dir.clone(),
        dir_context,
    )
    .expect("harness");

    // The async variant queues init.ts through the plugin
    // thread. fire_ready_hook fires immediately afterwards,
    // exactly like main.rs::real_main does in production.
    harness.editor_mut().load_init_script_async(true);
    harness.editor_mut().fire_ready_hook();

    let deadline = std::time::Instant::now() + std::time::Duration::from_millis(1500);
    while std::time::Instant::now() < deadline {
        harness.tick_and_render().unwrap();
        std::thread::sleep(std::time::Duration::from_millis(50));
    }

    let has_dashboard = harness
        .editor()
        .active_window()
        .buffer_metadata
        .values()
        .any(|m| matches!(&m.kind, fresh::app::types::BufferKind::Virtual { mode } if mode == "dashboard"));
    assert!(
        !has_dashboard,
        "init.ts (async) called setAutoOpen(false) before `ready`; the \
         dashboard buffer must not have been opened"
    );
}
