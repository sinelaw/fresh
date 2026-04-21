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
    harness.editor_mut().process_pending_file_opens();

    // The async plumbing between QuickJS and the editor settles over
    // a few render ticks: the dashboard's `after_file_open` handler
    // has to reach the plugin thread, close the just-created virtual
    // buffer, and the resulting commands have to drain back to the
    // editor before the active buffer flips to the CLI file. Under
    // CI load the order isn't deterministic, so wait semantically
    // rather than banking on a fixed number of `process_async_messages`
    // drains (per CONTRIBUTING.md — no fixed-timer tests).
    harness
        .wait_until(|h| {
            let active = h.editor().active_buffer();
            h.editor()
                .get_buffer_display_name(active)
                .contains("my_file.txt")
        })
        .unwrap();

    let active = harness.editor().active_buffer();
    let active_name = harness.editor().get_buffer_display_name(active);
    assert_ne!(
        active_name, "Dashboard",
        "CLI-supplied file must remain the active tab — the dashboard \
         should not open when a file was requested on the command line"
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
    harness
        .wait_until(|h| {
            let active = h.editor().active_buffer();
            h.editor().get_buffer_display_name(active) == "Dashboard"
        })
        .unwrap();
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

    // The sidecar's refresh is async — wait until its section body
    // actually appears on screen rather than banking on a fixed
    // number of `process_async_messages` drains. Both CUSTOM header
    // and the kv row need to land.
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("CUSTOM") && screen.contains("hello") && screen.contains("from sidecar")
        })
        .unwrap();
}

/// TypeScript body for a sidecar plugin that renders a single row
/// with one clickable span ("CLICKME"). Clicking it sets the status
/// message to "CLICKED". The two tests below use this to observe
/// whether a particular click at a known (row, col) dispatches the
/// onClick handler.
const CLICKS_SIDECAR: &str = r#"/// <reference path="./lib/fresh.d.ts" />
/// @depends-on dashboard
const editor = getEditor();

type Ctx = {
    kv: (label: string, value: string, color?: string) => void;
    text: (s: string, opts?: { color?: string; bold?: boolean; url?: string; onClick?: () => void }) => void;
    newline: () => void;
    error: (message: string) => void;
};

const dash = editor.getPluginApi("dashboard") as
    | { registerSection: (name: string, refresh: (ctx: Ctx) => Promise<void>) => () => void }
    | null;

if (dash) {
    dash.registerSection("clicks", async (ctx) => {
        // Row layout: non-clickable padded label, then a clickable
        // token. Padding and whitespace past the token must stay
        // unclickable.
        ctx.text("    " + "link".padEnd(10), { color: "muted" });
        ctx.text("CLICKME", { color: "accent", onClick: () => editor.setStatus("CLICKED") });
        ctx.newline();
    });
}
"#;

/// Shared setup for the click-scoping tests. Returns a harness with
/// the dashboard + sidecar plugins loaded and the dashboard open,
/// plus the `(row, colStart, colEnd)` of "CLICKME" in the rendered
/// screen.
fn setup_clicks_scenario() -> (EditorTestHarness, tempfile::TempDir, u16, u16, u16) {
    let (harness, tmp, plugins_dir) = harness_with_dashboard_plugin_and_plugins_dir();
    fs::write(plugins_dir.join("sidecar.ts"), CLICKS_SIDECAR).unwrap();

    drop(harness);
    let working_dir = plugins_dir.parent().unwrap().to_path_buf();
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Config::default(), working_dir)
            .expect("harness");

    harness.editor_mut().fire_ready_hook();
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("CLICKS") && screen.contains("CLICKME")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    let click_row: u16 = screen
        .lines()
        .enumerate()
        .find_map(|(i, line)| line.contains("CLICKME").then_some(i as u16))
        .expect("CLICKME should be on some row");
    let row_text = screen.lines().nth(click_row as usize).expect("row text");
    let col_start = row_text.find("CLICKME").expect("col") as u16;
    let col_end = col_start + "CLICKME".len() as u16;

    (harness, tmp, click_row, col_start, col_end)
}

/// Positive: clicking on the "CLICKME" text fires the onClick
/// handler, turning the status bar into "CLICKED".
#[test]
fn click_on_clickable_span_fires_action() {
    let (mut harness, _tmp, row, col_start, col_end) = setup_clicks_scenario();
    let mid = (col_start + col_end) / 2;
    harness.mouse_click(mid, row).unwrap();
    harness
        .wait_until(|h| {
            h.editor()
                .get_status_message()
                .map(|s| s == "CLICKED")
                .unwrap_or(false)
        })
        .unwrap();
}

/// Negative: clicking on whitespace that shares a row with a
/// clickable span — the padded label cell to the left of "CLICKME" —
/// must NOT fire the onClick. The underline is the visual
/// affordance; behavior has to match.
#[test]
fn click_on_row_whitespace_does_not_fire_action() {
    let (mut harness, _tmp, row, col_start, _col_end) = setup_clicks_scenario();
    // Three cells left of "C" — firmly inside the trailing-padding
    // gap after "link          " and before "CLICKME".
    let whitespace = col_start - 3;
    harness.mouse_click(whitespace, row).unwrap();
    for _ in 0..10 {
        harness.editor_mut().process_async_messages();
        harness.render().unwrap();
    }
    let status = harness
        .editor()
        .get_status_message()
        .cloned()
        .unwrap_or_default();
    assert_ne!(
        status, "CLICKED",
        "click on whitespace in a row with a clickable span must NOT fire the action"
    );
}
