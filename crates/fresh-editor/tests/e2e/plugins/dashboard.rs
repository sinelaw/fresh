//! E2E tests for the bundled `dashboard` plugin.
//!
//! These tests simulate `main()`'s real boot sequence closely enough
//! to surface timing bugs around when the dashboard decides to open
//! itself — in particular, whether a CLI file argument keeps it
//! dormant (it should) or not.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use fresh::config::Config;
use std::fs;
use std::path::PathBuf;

/// Build a harness rooted at a scratch working directory that contains
/// the real `dashboard` plugin (copied from the repo). The plugin loads
/// and subscribes to its hooks during harness creation.
fn harness_with_dashboard_plugin() -> (EditorTestHarness, tempfile::TempDir) {
    let (harness, temp, _plugins_dir) = harness_with_dashboard_plugin_and_plugins_dir();
    (harness, temp)
}

/// Like `harness_with_dashboard_plugin`, but also returns the plugins
/// directory so tests can drop additional plugins alongside the
/// built-in dashboard (e.g. to exercise `registerSection`).
fn harness_with_dashboard_plugin_and_plugins_dir() -> (EditorTestHarness, tempfile::TempDir, PathBuf)
{
    let temp = tempfile::TempDir::new().expect("tempdir");
    let working_dir = temp.path().join("work");
    fs::create_dir_all(&working_dir).unwrap();
    let plugins_dir = working_dir.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "dashboard");
    copy_plugin_lib(&plugins_dir);

    let harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Config::default(), working_dir)
            .expect("harness");
    (harness, temp, plugins_dir)
}

/// `fresh my_file` must not pop the dashboard on top of the requested
/// file. Before the fix, `fire_ready_hook` ran while the CLI file was
/// still sitting in `pending_file_opens`, so the dashboard plugin's
/// `ready` handler saw "no real buffers" and opened the Dashboard tab
/// — which stole focus, leaving the user's file as a background tab.
#[test]
fn dashboard_stays_closed_when_cli_file_is_opening() {
    let (mut harness, _tmp) = harness_with_dashboard_plugin();

    // Create a file the "CLI" will ask us to open.
    let file_path = harness.editor().working_dir().join("my_file.txt");
    fs::write(&file_path, "hello from my_file\n").unwrap();

    // Reproduce production order in real_main: queue the CLI file,
    // fire the ready hook, THEN process the pending file open on the
    // first event-loop iteration. If the dashboard races ready and
    // opens before the file lands, the assertion below will fail.
    harness
        .editor_mut()
        .queue_file_open(file_path.clone(), None, None, None, None, None, None);
    harness.editor_mut().fire_ready_hook();
    // Drain any buffer-create commands the plugin may have queued in
    // response to the ready hook, so a dashboard-open won't hide
    // behind async plumbing and escape detection.
    harness.editor_mut().process_async_messages();
    harness.editor_mut().process_pending_file_opens();
    harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    let active = harness.editor().active_buffer();
    let active_name = harness.editor().get_buffer_display_name(active);
    assert_ne!(
        active_name, "Dashboard",
        "CLI-supplied file must remain the active tab — the dashboard \
         should not open when a file was requested on the command line"
    );
    assert!(
        active_name.contains("my_file.txt"),
        "active buffer should be the CLI file, got {active_name:?}"
    );
}

/// Sanity check for the fix above: when there is *no* CLI file (and
/// no real file of any kind) at ready-hook time, the dashboard does
/// still open. This guards against an over-eager suppression that
/// would silently break the `fresh` (no args) flow.
#[test]
fn dashboard_opens_when_no_file_is_queued() {
    let (mut harness, _tmp) = harness_with_dashboard_plugin();

    harness.editor_mut().fire_ready_hook();
    harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    let active = harness.editor().active_buffer();
    let active_name = harness.editor().get_buffer_display_name(active);
    assert_eq!(
        active_name, "Dashboard",
        "with no files queued, the dashboard should take the empty \
         workspace as its cue to open"
    );
}

/// Third-party plugins (and user init.ts) can add their own section
/// to the dashboard via the exported `registerSection` plugin API.
/// This test drops a sidecar plugin next to the dashboard that
/// registers a section with a deterministic body, opens the
/// dashboard, and verifies the section header and body text both
/// appear in the rendered frame.
#[test]
fn register_section_lets_other_plugins_add_rows() {
    let (mut harness, _tmp, plugins_dir) = harness_with_dashboard_plugin_and_plugins_dir();

    // Sidecar plugin. Declares a "dashboard" dep so it loads after the
    // main dashboard plugin — `getPluginApi` returns null if called
    // before the exporter's top-level code runs.
    let sidecar = r#"/// <reference path="./lib/fresh.d.ts" />
/// @depends-on dashboard
const editor = getEditor();

type Ctx = {
    kv: (label: string, value: string, color?: string) => void;
    text: (s: string, opts?: { color?: string; bold?: boolean; url?: string }) => void;
    newline: () => void;
    error: (message: string) => void;
};

const dash = editor.getPluginApi("dashboard") as
    | { registerSection: (name: string, refresh: (ctx: Ctx) => Promise<void>) => () => void }
    | null;

if (dash) {
    dash.registerSection("custom", async (ctx) => {
        ctx.kv("hello", "from sidecar", "ok");
    });
}
"#;
    fs::write(plugins_dir.join("sidecar.ts"), sidecar).unwrap();

    // Rebuild the harness so the plugin scanner picks up the new
    // sidecar.ts — the first harness in
    // `harness_with_dashboard_plugin_and_plugins_dir` already ran the
    // scan and won't re-scan on its own.
    drop(harness);
    let working_dir = plugins_dir.parent().unwrap().to_path_buf();
    harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Config::default(), working_dir)
            .expect("harness");

    harness.editor_mut().fire_ready_hook();
    harness.editor_mut().process_async_messages();
    // The sidecar's refresh is async — give the editor a couple of
    // async-message drains to let the initial registerSection refresh
    // resolve and paint.
    for _ in 0..5 {
        harness.editor_mut().process_async_messages();
        harness.render().unwrap();
    }

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("CUSTOM"),
        "custom section header should appear; screen was:\n{screen}"
    );
    assert!(
        screen.contains("hello") && screen.contains("from sidecar"),
        "custom section body should contain its kv row; screen was:\n{screen}"
    );
}
