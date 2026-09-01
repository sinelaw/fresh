//! The provenance gate (Stage 0.0 of
//! `docs/internal/tui-retained-mode-migration-plan.md`).
//!
//! For each fixture this renders one frame with the cell recorder installed
//! (`fresh::app::provenance`), prints a table of who wrote the frame's painted
//! cells — the fold's two bands, the host painters the fold calls, and every
//! named legacy painter in `Editor::render` — and asserts three things:
//!
//! 1. **Every painted cell has a writer** the allowlist below names. A painter
//!    `render` does not bracket shows up as `<unattributed>`, and a new
//!    painter shows up under a name not in the list; either fails.
//! 2. **The legacy share is at or below its recorded baseline.** The baseline
//!    is the number in the plan; a stage that hands cells to a painter that
//!    the fold used to write fails here, and a stage that migrates cells
//!    lowers the number it should then record.
//! 3. The recorder agrees with the backend: the cells it counted as painted
//!    are the non-blank cells of the buffer the terminal received.
//!
//! Run with `--nocapture` to see the tables.

mod common;

use std::path::PathBuf;

use common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::app::provenance::{is_painted, Report, FOLD_BACKGROUND, FOLD_OVERLAY, UNATTRIBUTED};
use fresh::config::{Config, TerminalShellConfig};

const COLS: u16 = 120;
const ROWS: u16 = 40;

/// Every writer a frame may report. The fold's bands, the hosts the fold
/// calls, and the legacy painters `Editor::render` brackets, by the name each
/// bracket gives. `<unattributed>` is deliberately absent.
const ALLOWED_WRITERS: &[&str] = &[
    FOLD_BACKGROUND,
    FOLD_OVERLAY,
    // Hosts: `Draw::Host` items the fold hands to a legacy painter.
    "host:pane",
    "host:body",
    "host:prompt_line",
    "host:embed",
    "host:card",
    "host:dock",
    "host:status_bar",
    // Legacy painters between and after the bands, in paint order.
    "shade_scroll_edges",
    "render_dormant_shell_page",
    "render_preparing_shell_page",
    "render_terminal_splits",
    "render_split_widget_panel_scrollbars",
    "render_hover_highlights",
    "apply_dimming(overlay_prompt)",
    "render_overlay_prompt",
    "FileBrowserRenderer::render",
    "apply_dimming(settings)",
    "render_settings",
    "render_floating_widget_panel(dock)",
    "render_tab_drop_zone",
    "render_software_cursor_and_capture",
    "animations.apply_all",
    "apply_dimming(dock)",
    "render_floating_widget_panel(floating)",
    "convert_buffer_colors",
];

/// The recorded baseline: the highest legacy share (percent of painted cells
/// not written by the fold) each fixture may report, as measured at 120×40
/// and rounded up to a tenth. Copied into the plan under Stage 0.0; lower it
/// there and here when a stage brings it down.
const BASELINE_LEGACY_PERCENT: &[(&str, f64)] = &[
    ("empty", 94.9),
    ("highlighted_file", 94.9),
    ("four_splits", 94.9),
    ("explorer", 66.5),
    ("dock", 88.7),
    ("command_palette", 65.0),
    ("file_browser", 97.5),
    ("overlay_prompt", 100.0),
    ("settings", 41.0),
    ("terminal", 36.2),
];

fn baseline(fixture: &str) -> f64 {
    BASELINE_LEGACY_PERCENT
        .iter()
        .find(|(f, _)| *f == fixture)
        .map(|(_, p)| *p)
        .unwrap_or_else(|| panic!("no baseline recorded for fixture {fixture}"))
}

fn fixture_file(rel: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(rel)
}

fn pty_available() -> bool {
    use portable_pty::{native_pty_system, PtySize};
    native_pty_system()
        .openpty(PtySize {
            rows: 24,
            cols: 80,
            pixel_width: 0,
            pixel_height: 0,
        })
        .is_ok()
}

// ---------------------------------------------------------------------------
// Fixtures
// ---------------------------------------------------------------------------

fn empty() -> EditorTestHarness {
    EditorTestHarness::new(COLS, ROWS).unwrap()
}

fn highlighted_file() -> EditorTestHarness {
    let mut h = EditorTestHarness::create(
        COLS,
        ROWS,
        HarnessOptions::new().with_full_grammar_registry(),
    )
    .unwrap();
    h.open_file(&fixture_file("tests/fixtures/large.rs"))
        .unwrap();
    h.wait_for_screen_contains("large.rs").unwrap();
    // Highlighted, not merely open: the content rows carry more than one
    // foreground colour once the syntax pass has run.
    h.wait_until(|h| {
        let buf = h.buffer();
        let mut colours = std::collections::HashSet::new();
        for y in 2..ROWS - 2 {
            for x in 8..COLS {
                colours.insert(format!("{:?}", buf[(x, y)].fg));
            }
        }
        colours.len() >= 3
    })
    .unwrap();
    h
}

fn four_splits() -> EditorTestHarness {
    let mut h = EditorTestHarness::new(COLS, ROWS).unwrap();
    // Each split divides the active pane, so three of them make four.
    for cmd in ["Split Vertical", "Split Horizontal", "Split Vertical"] {
        h.run_palette_command(cmd).unwrap();
        h.wait_for_prompt_closed().unwrap();
    }
    h.render().unwrap();
    assert_eq!(
        h.editor().get_split_count(),
        4,
        "the four-splits fixture has four panes"
    );
    h
}

fn explorer() -> EditorTestHarness {
    let mut h = EditorTestHarness::with_temp_project(COLS, ROWS).unwrap();
    h.editor_mut().toggle_file_explorer();
    h.wait_for_file_explorer().unwrap();
    h.render().unwrap();
    h
}

/// The orchestrator's dock, opened the way `tests/e2e/dock_dropdown_mouse.rs`
/// opens it: a git project with the orchestrator plugin on disk.
fn dock() -> Option<EditorTestHarness> {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let root = temp_dir.path().join("project");
    std::fs::create_dir(&root).unwrap();
    let plugins_dir = root.join("plugins");
    std::fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "orchestrator");
    std::fs::write(root.join("readme.txt"), "hello\n").unwrap();
    let git = std::process::Command::new("git")
        .args(["init", "-q"])
        .current_dir(&root)
        .status();
    if !git.map(|s| s.success()).unwrap_or(false) {
        eprintln!("Skipping the dock fixture: git not available");
        return None;
    }
    let mut h =
        EditorTestHarness::with_config_and_working_dir(COLS, ROWS, Default::default(), root)
            .unwrap();
    h.render().unwrap();
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text("Orchestrator: Toggle Dock").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Toggle Dock"))
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Orchestrator") && h.editor().is_dock_focused())
        .unwrap();
    // The harness owns the temp dir's lifetime through the working dir it
    // was given; keep the guard alive alongside it.
    std::mem::forget(temp_dir);
    Some(h)
}

fn command_palette() -> EditorTestHarness {
    let mut h = EditorTestHarness::new(COLS, ROWS).unwrap();
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.render().unwrap();
    h
}

/// The Open File prompt: the one prompt whose popup is still a painter
/// (`FileBrowserRenderer::render`).
fn file_browser() -> EditorTestHarness {
    let mut h = EditorTestHarness::with_temp_project(COLS, ROWS).unwrap();
    let dir = h.project_dir().unwrap();
    for name in ["alpha.txt", "beta.txt", "gamma.txt"] {
        std::fs::write(dir.join(name), "text\n").unwrap();
    }
    h.run_palette_command("Open File").unwrap();
    h.wait_until(|h| h.editor().is_prompting() && h.screen_to_string().contains("alpha.txt"))
        .unwrap();
    h.render().unwrap();
    h
}

/// The overlay prompt (`render_overlay_prompt`): Live Grep's centred card,
/// opened the way `tests/e2e/blog_showcases.rs` opens it.
fn overlay_prompt() -> Option<EditorTestHarness> {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let root = temp_dir.path().join("project");
    std::fs::create_dir_all(root.join("src")).unwrap();
    std::fs::write(
        root.join("src/main.rs"),
        "fn main() {\n    let config = 1;\n    println!(\"{config}\");\n}\n",
    )
    .unwrap();
    let git = |args: &[&str]| {
        std::process::Command::new("git")
            .args(args)
            .current_dir(&root)
            .status()
            .map(|s| s.success())
            .unwrap_or(false)
    };
    if !(git(&["init", "-q"])
        && git(&["-c", "user.email=t@t", "-c", "user.name=t", "add", "."])
        && git(&[
            "-c",
            "user.email=t@t",
            "-c",
            "user.name=t",
            "commit",
            "-q",
            "-m",
            "seed",
        ]))
    {
        eprintln!("Skipping the overlay-prompt fixture: git not available");
        return None;
    }
    let plugins_dir = root.join("plugins");
    std::fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "live_grep");
    let mut h = EditorTestHarness::with_config_and_working_dir(
        COLS,
        ROWS,
        Default::default(),
        root.clone(),
    )
    .unwrap();
    h.open_file(&root.join("src/main.rs")).unwrap();
    h.render().unwrap();
    h.wait_until(|h| {
        let reg = h.editor().command_registry().read().unwrap();
        reg.get_all()
            .iter()
            .any(|c| c.get_localized_name().starts_with("Live Grep"))
    })
    .unwrap();
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text("Live Grep").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Live Grep (Find in Files)"))
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Search in:"))
        .unwrap();
    h.type_text("config").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("main.rs"))
        .unwrap();
    h.render().unwrap();
    std::mem::forget(temp_dir);
    Some(h)
}

fn settings() -> EditorTestHarness {
    let mut h = EditorTestHarness::new(COLS, ROWS).unwrap();
    h.open_settings().unwrap();
    h
}

/// A live PTY pane whose contents are deterministic: the shell is `sh`
/// printing one line and waiting, so no prompt or banner varies the count.
fn terminal() -> Option<EditorTestHarness> {
    if !pty_available() {
        eprintln!("Skipping the terminal fixture: PTY not available");
        return None;
    }
    let mut config = Config::default();
    config.terminal.shell = Some(TerminalShellConfig {
        command: "/bin/sh".to_string(),
        args: vec![
            "-c".to_string(),
            "printf 'hello from the pty\\n'; sleep 600".to_string(),
        ],
    });
    let mut h = EditorTestHarness::with_temp_project_and_config(COLS, ROWS, config).unwrap();
    h.editor_mut().open_terminal();
    h.wait_for_screen_contains("*Terminal 0*").unwrap();
    h.wait_for_screen_contains("hello from the pty").unwrap();
    h.render().unwrap();
    Some(h)
}

// ---------------------------------------------------------------------------
// The gate
// ---------------------------------------------------------------------------

/// Render one frame with the recorder on and return its report.
fn record(h: &mut EditorTestHarness) -> Report {
    h.editor_mut().record_cell_provenance();
    h.render_real().unwrap();
    let report = h
        .editor()
        .cell_provenance()
        .expect("a frame was rendered with the recorder installed");
    // The recorder's count of painted cells is the backend's.
    let backend_painted = h.buffer().content.iter().filter(|c| is_painted(c)).count();
    assert_eq!(
        report.painted, backend_painted,
        "the recorder and the terminal disagree about how many cells are painted"
    );
    assert_eq!(
        report.writers.iter().map(|(_, n)| n).sum::<usize>(),
        report.painted,
        "every painted cell is in exactly one writer's count"
    );
    report
}

fn percent(n: usize, of: usize) -> f64 {
    if of == 0 {
        0.0
    } else {
        100.0 * n as f64 / of as f64
    }
}

fn print_table(fixture: &str, r: &Report) {
    println!();
    println!(
        "fixture {fixture}: {} of {} cells painted, {} marks; legacy {:.1}%",
        r.painted,
        r.area,
        r.marks,
        percent(r.legacy(), r.painted)
    );
    println!("  {:<44} {:>7} {:>7}", "writer", "cells", "share");
    for (name, n) in &r.writers {
        println!("  {:<44} {:>7} {:>6.1}%", name, n, percent(*n, r.painted));
    }
    for (name, times) in &r.ran {
        if r.count(name) == 0 {
            println!("  {:<44} {:>7} {:>7}", name, 0, format!("ran x{times}"));
        }
    }
}

fn gate(fixture: &str, r: &Report) {
    print_table(fixture, r);
    let unknown: Vec<&str> = r
        .writers
        .iter()
        .map(|(n, _)| n.as_str())
        .filter(|n| !ALLOWED_WRITERS.contains(n))
        .collect();
    assert!(
        unknown.is_empty(),
        "fixture {fixture}: painted cells from a writer not in the allowlist: {unknown:?} \
         (a `<unattributed>` entry is a painter `Editor::render` does not bracket)"
    );
    assert_eq!(
        r.unattributed(),
        0,
        "fixture {fixture}: {} painted cells have no writer",
        r.count(UNATTRIBUTED)
    );
    let legacy = percent(r.legacy(), r.painted);
    let allowed = baseline(fixture);
    assert!(
        legacy <= allowed + 1e-9,
        "fixture {fixture}: the legacy share rose to {legacy:.2}% (baseline {allowed:.1}%). \
         A stage may only bring this number down; if a painter legitimately owns \
         more cells now, say why and re-record the baseline in the plan."
    );
}

#[test]
fn empty_editor() {
    let mut h = empty();
    gate("empty", &record(&mut h));
}

#[test]
fn highlighted_file_open() {
    let mut h = highlighted_file();
    gate("highlighted_file", &record(&mut h));
}

#[test]
fn four_splits_open() {
    let mut h = four_splits();
    gate("four_splits", &record(&mut h));
}

#[test]
fn explorer_open() {
    let mut h = explorer();
    gate("explorer", &record(&mut h));
}

#[test]
fn dock_open() {
    let Some(mut h) = dock() else {
        return;
    };
    gate("dock", &record(&mut h));
}

#[test]
fn command_palette_open() {
    let mut h = command_palette();
    gate("command_palette", &record(&mut h));
}

#[test]
fn file_browser_open() {
    let mut h = file_browser();
    gate("file_browser", &record(&mut h));
}

#[test]
fn overlay_prompt_open() {
    let Some(mut h) = overlay_prompt() else {
        return;
    };
    gate("overlay_prompt", &record(&mut h));
}

#[test]
fn settings_open() {
    let mut h = settings();
    gate("settings", &record(&mut h));
}

#[cfg(unix)]
#[test]
fn terminal_pane_open() {
    let Some(mut h) = terminal() else {
        return;
    };
    gate("terminal", &record(&mut h));
}

/// The recorder does not change what is painted: the same fixture rendered
/// with and without it produces the same cells.
#[test]
fn recording_does_not_change_the_frame() {
    let mut plain = command_palette();
    plain.render_real().unwrap();
    let without = plain.buffer().clone();

    let mut recorded = command_palette();
    recorded.editor_mut().record_cell_provenance();
    recorded.render_real().unwrap();
    let with = recorded.buffer().clone();

    assert_eq!(
        with, without,
        "recording provenance changed the painted frame"
    );
}
