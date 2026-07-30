//! E2E tests for the bundled `dashboard` plugin.
//!
//! These tests simulate `main()`'s real boot sequence closely enough
//! to surface timing bugs around when the dashboard decides to open
//! itself — in particular, whether a CLI file argument keeps it
//! dormant (it should) or not.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::{Config, PluginConfig};
use std::fs;
use std::path::PathBuf;

/// Config that opts the dashboard back into auto-open.
///
/// `autoOpen` defaults to `false` (the dashboard no longer pops on
/// startup), so tests that exercise the ambient open path must enable it
/// explicitly via the same `plugins.dashboard.settings` channel a user
/// would use in `config.json` / the Settings UI.
fn config_with_dashboard_autoopen() -> Config {
    let mut config = Config::default();
    config.plugins.insert(
        "dashboard".to_string(),
        PluginConfig {
            enabled: true,
            path: None,
            settings: serde_json::json!({ "autoOpen": true }),
        },
    );
    config
}

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

    let harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        config_with_dashboard_autoopen(),
        working_dir,
    )
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

/// End-to-end check for dashboard bringup. Opens the dashboard, waits
/// for it to become the active buffer, and confirms the ASCII "FRESH"
/// banner is rendered. The dashboard no longer animates on bringup —
/// the panel lands at its final position immediately — so we just wait
/// for the buffer to be active and the screen to reflect it.
#[test]
fn dashboard_bringup_renders_banner() {
    let (mut harness, _tmp) = harness_with_dashboard_plugin();

    harness.editor_mut().fire_ready_hook();

    // Dashboard is the active buffer once the ready hook fires and
    // its createVirtualBuffer round-trip resolves.
    harness
        .wait_until(|h| {
            let active = h.editor().active_buffer();
            h.editor().get_buffer_display_name(active) == "Dashboard"
        })
        .unwrap();

    // Wait until the banner makes it through the async paint plumbing
    // and onto the rendered screen.
    harness
        .wait_until(|h| h.screen_to_string().contains("FRESH"))
        .unwrap();

    harness.assert_no_plugin_errors();
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
    harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        config_with_dashboard_autoopen(),
        working_dir,
    )
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
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        config_with_dashboard_autoopen(),
        working_dir,
    )
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

// ── Frame integrity across terminal widths ──────────────────────────────

/// Every row of the dashboard frame, from the `╭` row to the `╰` row, must be
/// a well-formed box: the two vertical borders sit at the same two columns on
/// every row, and no row spills past the right border.
///
/// Returns `Err(reason)` naming the first broken row so a sweep failure says
/// which width and which row it was.
fn assert_frame_intact(screen: &str, width: u16) -> Result<(), String> {
    let rows: Vec<&str> = screen.lines().collect();
    let top = rows
        .iter()
        .position(|r| r.contains('╭'))
        .ok_or_else(|| format!("width {width}: no top border row on screen"))?;
    let bottom = rows
        .iter()
        .position(|r| r.contains('╰'))
        .ok_or_else(|| format!("width {width}: no bottom border row on screen"))?;
    if bottom <= top {
        return Err(format!(
            "width {width}: bottom border at row {bottom} is not below top at {top}"
        ));
    }

    // Column geometry is taken from the top border and every later row must
    // match it. Columns are counted in chars, which is what the screen dump
    // gives us; the frame is deliberately built from single-width glyphs.
    let cols = |row: &str, ch: char| -> Vec<usize> {
        row.chars()
            .enumerate()
            .filter(|(_, c)| *c == ch)
            .map(|(i, _)| i)
            .collect()
    };
    let left = *cols(rows[top], '╭')
        .first()
        .ok_or_else(|| format!("width {width}: top row lost its ╭"))?;
    let right = *cols(rows[top], '╮')
        .first()
        .ok_or_else(|| format!("width {width}: top row lost its ╮"))?;
    if right <= left {
        return Err(format!(
            "width {width}: top border corners inverted (left {left}, right {right})"
        ));
    }

    for (idx, row) in rows.iter().enumerate().take(bottom).skip(top + 1) {
        let bars = cols(row, '│');
        if bars.len() != 2 {
            return Err(format!(
                "width {width} row {idx}: expected exactly 2 │ borders, found {} — \
                 a row wider than the panel tore the box: {row:?}",
                bars.len()
            ));
        }
        if bars[0] != left || bars[1] != right {
            return Err(format!(
                "width {width} row {idx}: borders at {bars:?}, expected [{left}, {right}] — \
                 content pushed the right border out: {row:?}"
            ));
        }
        // Nothing but padding may live to the right of the closing border.
        let tail: String = row.chars().skip(right + 1).collect();
        if !tail.trim().is_empty() {
            return Err(format!(
                "width {width} row {idx}: content {tail:?} spilled past the right border"
            ));
        }
    }

    let bottom_left = *cols(rows[bottom], '╰')
        .first()
        .ok_or_else(|| format!("width {width}: bottom row lost its ╰"))?;
    let bottom_right = *cols(rows[bottom], '╯')
        .first()
        .ok_or_else(|| format!("width {width}: bottom row lost its ╯"))?;
    if bottom_left != left || bottom_right != right {
        return Err(format!(
            "width {width}: bottom corners at [{bottom_left}, {bottom_right}], \
             expected [{left}, {right}]"
        ));
    }
    Ok(())
}

/// Columns between the two vertical borders on the frame's top row.
fn frame_inner_width(screen: &str) -> usize {
    for row in screen.lines() {
        let left = row.chars().position(|c| c == '╭');
        let right = row.chars().position(|c| c == '╮');
        if let (Some(l), Some(r)) = (left, right) {
            return r - l - 1;
        }
    }
    0
}

/// The frame must stay intact — and adapt its width — across a contiguous
/// sweep of terminal widths, even with a section emitting rows far wider than
/// any panel.
///
/// `step_by(1)`: a coarser stride would sail past widths where a padding or
/// fill computation is off by one only for particular parities, which is
/// exactly the class of bug this guards.
#[test]
fn dashboard_frame_is_intact_across_width_sweep() {
    let (harness, _tmp, plugins_dir) = harness_with_dashboard_plugin_and_plugins_dir();

    // A section that deliberately emits rows far wider than any panel, with a
    // click target hanging off the right edge. The frame renderer has to clip
    // centrally; if it doesn't, the sweep below reports the torn row.
    let overflow = r#"/// <reference path="./lib/fresh.d.ts" />
/// @depends-on dashboard
const editor = getEditor();
const dash = editor.getPluginApi("dashboard") as {
    registerSection: (
        name: string,
        refresh: (ctx: {
            text: (s: string, o?: { color?: string; onClick?: () => void }) => void;
            newline: () => void;
        }) => Promise<void>,
        options?: { ttlMs?: number },
    ) => () => void;
} | null;
let wide = "";
while (wide.length < 400) wide += "WIDEROW0123456789";
if (dash) {
    dash.registerSection("overflow", async (ctx) => {
        for (let i = 0; i < 3; i++) {
            ctx.text(wide, { color: "accent", onClick: () => { } });
            ctx.newline();
        }
    }, { ttlMs: 1000 });
}
"#;
    fs::write(plugins_dir.join("overflow.ts"), overflow).unwrap();

    // Rebuild so the plugin scanner picks up the new sidecar.
    drop(harness);
    let working_dir = plugins_dir.parent().unwrap().to_path_buf();
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        config_with_dashboard_autoopen(),
        working_dir,
    )
    .expect("harness");
    harness.editor_mut().fire_ready_hook();
    harness
        .wait_until(|h| h.screen_to_string().contains("OVERFLOW"))
        .unwrap();

    let mut inner_widths: Vec<usize> = Vec::new();
    for width in (30u16..=140).step_by(1) {
        harness.resize(width, 40).unwrap();
        // The repaint is async (viewport_changed → plugin → setVirtualBuffer),
        // so wait for a frame drawn at the new width rather than banking on a
        // fixed number of ticks. The panel width is the observable signal that
        // the new geometry has landed.
        let expected_inner = expected_frame_inner(width);
        harness
            .wait_until(|h| frame_inner_width(&h.screen_to_string()) == expected_inner)
            .unwrap_or_else(|_| {
                panic!(
                    "width {width}: panel never repainted to inner {expected_inner} \
                     (saw {})\n{}",
                    frame_inner_width(&harness.screen_to_string()),
                    harness.screen_to_string()
                )
            });

        let screen = harness.screen_to_string();
        if let Err(reason) = assert_frame_intact(&screen, width) {
            panic!("{reason}\n{screen}");
        }
        inner_widths.push(frame_inner_width(&screen));
    }

    // Responsive, not fixed: the panel must actually grow with the terminal
    // rather than sitting at a hard-coded cap for the whole sweep.
    let first = inner_widths.first().copied().unwrap_or(0);
    let last = inner_widths.last().copied().unwrap_or(0);
    assert!(
        last > first,
        "panel width must adapt to the terminal: 30 cols gave inner {first}, \
         140 cols gave inner {last}"
    );
    // Monotone: a wider terminal never yields a narrower panel.
    for pair in inner_widths.windows(2) {
        assert!(
            pair[1] >= pair[0],
            "panel width must not shrink as the terminal grows: saw {pair:?}"
        );
    }

    harness.assert_no_plugin_errors();
}

/// Mirror of `frameWidth()` in dashboard.ts. Kept here so the sweep can wait
/// for the *expected* geometry instead of accepting whatever is on screen —
/// otherwise a stale pre-resize frame reads as a pass.
fn expected_frame_inner(viewport_w: u16) -> usize {
    const INNER_MIN: usize = 24;
    const INNER_MAX: usize = 110;
    let w = viewport_w as usize;
    let avail = w.saturating_sub(2).max(1);
    let target = ((w as f64 * 0.9).floor() as usize).saturating_sub(2);
    INNER_MAX.min(avail).min(target.max(INNER_MIN)).max(1)
}
