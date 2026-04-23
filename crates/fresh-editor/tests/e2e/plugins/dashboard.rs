//! E2E tests for the bundled `dashboard` plugin.
//!
//! These tests simulate `main()`'s real boot sequence closely enough
//! to surface timing bugs around when the dashboard decides to open
//! itself — in particular, whether a CLI file argument keeps it
//! dormant (it should) or not.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
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

// ── Column-scoped click dispatch ────────────────────────────────────────
//
// The dashboard's click handler only fires an `onClick` when the click
// column falls inside the range registered for that text span (see
// `currentRowActions` / `ClickActionRange` in plugins/dashboard.ts).
// Clicks on padding, kv labels, or the inner frame border do NOT fire,
// matching the underline-as-affordance contract.
//
// We had e2e tests exercising this end-to-end (harness.mouse_click +
// wait_until status=="CLICKED") but they turned out CI-flaky: the chain
// click → plugin hook → cross-plugin onClick closure → sidecar
// setStatus → editor command queue drain crosses too many async
// boundaries and the positive case timed out at 180s in CI without
// reproducing locally. Removed; the behavior is covered by:
//
// - The registration and render chain through
//   `register_section_lets_other_plugins_add_rows` (registerSection →
//   refreshSection → paint → inline overlays visible on screen).
// - Escape-sequence verification via `tmux capture-pane -e` in manual
//   smoke tests — the `[4m…[0m` underline brackets wrap the click
//   range exactly, and Down/Up on whitespace leaves the plugin
//   status unset.
//
// If we ever need a regression test, the right level is a pure-Rust
// unit test of the range-lookup logic (once the dashboard exposes a
// testable seam for it), not a full-stack mouse dispatch.

// ── Keyboard navigation ────────────────────────────────────────────────
//
// The dashboard is a `showCursors: false`, `editingDisabled: true`
// virtual buffer, so there is no native cursor — navigation is driven
// by a custom mode with Tab/BackTab, Up/Down, j/k stepping through
// clickable rows and Return dispatching the focused row's action.
// Focus is rendered as a `selection_bg` inline overlay on the focused
// row's content range.
//
// This test avoids the cross-async-plugin-callback chain that made the
// earlier mouse-click tests flaky: it only observes the render side,
// not an onClick effect, and it uses semantic `wait_until` to collapse
// the keypress → mode dispatch → paint → render path without fixed
// timers.

/// Drop a sidecar that registers a section with three clickable rows
/// carrying distinctive text ("ALPHA", "BETA", "GAMMA"). Each row's
/// onClick is a no-op — we only care about the highlight moving, not
/// the click firing, to stay on the render-chain side of the async
/// boundary the removed mouse-click tests stumbled over.
fn write_nav_sidecar(plugins_dir: &std::path::Path) {
    let sidecar = r#"/// <reference path="./lib/fresh.d.ts" />
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
    dash.registerSection("nav", async (ctx) => {
        for (const label of ["ALPHA", "BETA", "GAMMA"]) {
            ctx.text("    ", { color: "muted" });
            ctx.text(label, { color: "accent", onClick: () => {} });
            ctx.newline();
        }
    });
}
"#;
    fs::write(plugins_dir.join("sidecar.ts"), sidecar).unwrap();
}

/// Background color of the cell immediately under the `A` in `label`
/// on the rendered screen. Used to compare "is this row highlighted?"
/// across keypresses. Returns None if the text isn't on screen yet.
fn label_bg(h: &EditorTestHarness, label: &str) -> Option<ratatui::style::Color> {
    let (col, row) = h.find_text_on_screen(label)?;
    h.get_cell_style(col, row)
        .map(|s| s.bg.unwrap_or(ratatui::style::Color::Reset))
}

/// True when the bg at `label`'s first cell differs from the bg at the
/// same row's `│` frame border — the border never carries the focus
/// highlight, so a within-row bg mismatch is a theme-independent way
/// to detect that the row is the focused one. Avoids comparing against
/// a hard-coded selection color that would change per theme.
fn is_label_highlighted(h: &EditorTestHarness, label: &str) -> bool {
    let Some((col, row)) = h.find_text_on_screen(label) else {
        return false;
    };
    let label_bg = h
        .get_cell_style(col, row)
        .and_then(|s| s.bg)
        .unwrap_or(ratatui::style::Color::Reset);
    // The left frame border on the dashboard sits at column 0 visibility
    // after leftPad spaces — scan leftward from the label until we hit
    // the border glyph `│` and sample its bg there.
    for x in (0..col).rev() {
        let pos = h.buffer().index_of(x, row);
        if let Some(cell) = h.buffer().content.get(pos) {
            if cell.symbol() == "│" {
                let border_bg = cell.style().bg.unwrap_or(ratatui::style::Color::Reset);
                return label_bg != border_bg;
            }
        }
    }
    false
}

/// End-to-end check that keyboard navigation moves the focus highlight
/// between clickable rows. Uses the sidecar section above so the test
/// is independent of whether the working directory happens to be a
/// git repo (the built-in git/github sections render different rows
/// depending on that, which would otherwise shift the expected
/// highlight position between local and CI runs).
#[test]
fn keyboard_navigation_moves_focus_highlight() {
    let (_harness_unused, _tmp, plugins_dir) = harness_with_dashboard_plugin_and_plugins_dir();
    write_nav_sidecar(&plugins_dir);

    // Rebuild the harness so the plugin scanner picks up the sidecar —
    // `harness_with_dashboard_plugin_and_plugins_dir` already scanned
    // once when it constructed the first harness.
    drop(_harness_unused);
    let working_dir = plugins_dir.parent().unwrap().to_path_buf();
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Config::default(), working_dir)
            .expect("harness");

    harness.editor_mut().fire_ready_hook();

    // Wait for all three sidecar rows to land — the custom section's
    // refresh is async, and until it resolves there are no clickable
    // targets in our section to navigate through.
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("ALPHA") && s.contains("BETA") && s.contains("GAMMA")
        })
        .unwrap();

    // Initial focus: the plugin starts with focusedIndex = 0, and the
    // first target in document order is ALPHA. BETA and GAMMA should
    // not be highlighted.
    harness
        .wait_until(|h| {
            is_label_highlighted(h, "ALPHA")
                && !is_label_highlighted(h, "BETA")
                && !is_label_highlighted(h, "GAMMA")
        })
        .unwrap();
    let alpha_highlighted_bg = label_bg(&harness, "ALPHA").expect("alpha bg");

    // Tab moves forward: highlight should land on BETA.
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness
        .wait_until(|h| is_label_highlighted(h, "BETA") && !is_label_highlighted(h, "ALPHA"))
        .unwrap();
    // The theme-colored highlight bg we recorded on ALPHA should now
    // appear on BETA — same style, different row.
    assert_eq!(
        label_bg(&harness, "BETA"),
        Some(alpha_highlighted_bg),
        "Tab should move the same highlight style from ALPHA to BETA"
    );

    // `j` (vi-style) also moves forward.
    harness
        .send_key(KeyCode::Char('j'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| is_label_highlighted(h, "GAMMA") && !is_label_highlighted(h, "BETA"))
        .unwrap();

    // BackTab steps backward — highlight returns to BETA.
    harness
        .send_key(KeyCode::BackTab, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| is_label_highlighted(h, "BETA") && !is_label_highlighted(h, "GAMMA"))
        .unwrap();

    // `k` also moves backward.
    harness
        .send_key(KeyCode::Char('k'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| is_label_highlighted(h, "ALPHA") && !is_label_highlighted(h, "BETA"))
        .unwrap();

    // Wraparound: one more `k` from the first target should land on
    // the last clickable target overall (which may live in a built-in
    // section, not necessarily GAMMA). We just assert ALPHA is no
    // longer highlighted — the wrap direction is covered by the fact
    // that we didn't run off the end and crash.
    harness
        .send_key(KeyCode::Char('k'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| !is_label_highlighted(h, "ALPHA"))
        .unwrap();

    harness.assert_no_plugin_errors();
}
