//! E2E coverage for the Code Tour dock panel.
//!
//! The tour used to render each step in a floating action popup pinned
//! bottom-right at a fixed 60x15. Those tests assert the replacement: a
//! Utility Dock panel with clickable Prev/Next controls, markdown-rendered
//! prose, and a step rail — several tours at once, one dock tab each.
//!
//! Per CONTRIBUTING.md §2 every assertion here is on rendered output.
//!
//! Each test fails on the pre-dock plugin: the popup rendered `Next →` and
//! `Exit Tour` as bare list rows inside a `┌…┐` frame, never the `[ Next ▶ ]`
//! button chrome, the `Steps` rail, or a `*Tour: …*` dock tab.

use crate::common::blog_showcase::BlogShowcase;
use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;
use std::path::{Path, PathBuf};

/// A two-step tour over `src/main.rs`, with markdown in both explanations:
/// a heading, a bold run, inline code, a bullet list and a fenced block —
/// i.e. every construct the old popup mangled.
fn tour_json(root: &Path) -> String {
    let root = root.display().to_string();
    format!(
        r###"{{
  "title": "Pipeline Tour",
  "description": "How a request reaches the handler",
  "schema_version": "1.0",
  "steps": [
    {{
      "step_id": 1,
      "title": "Entry point",
      "file_path": "{root}/src/main.rs",
      "lines": [1, 3],
      "explanation": "## Where it starts\n\nThe listener is **built** here using `TcpListener`.\n\n- binds the socket\n- spawns the accept loop\n\n```rust\nlet l = TcpListener::bind(addr)?;\n```"
    }},
    {{
      "step_id": 2,
      "title": "The handler",
      "file_path": "{root}/src/wide.rs",
      "lines": [5, 44],
      "explanation": "## Handling\n\nEach connection is dispatched to `handle` on its own task."
    }}
  ]
}}"###
    )
}

fn second_tour_json(root: &Path) -> String {
    let root = root.display().to_string();
    format!(
        r###"{{
  "title": "Storage Tour",
  "description": "The storage layer",
  "schema_version": "1.0",
  "steps": [
    {{
      "step_id": 1,
      "title": "The store",
      "file_path": "{root}/src/store.rs",
      "lines": [1, 2],
      "explanation": "## The store\n\nKeys live in a `BTreeMap`."
    }}
  ]
}}"###
    )
}

const MAIN_RS: &str = "fn main() {\n    let l = listen();\n}\n\nfn handle() {\n    todo!()\n}\n";

/// Project with the code-tour plugin and a tour manifest at the root.
fn setup_tour_project() -> (tempfile::TempDir, PathBuf) {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();
    // Canonicalize before it reaches the manifest. On macOS a tempdir is
    // `/var/folders/...`, a symlink to `/private/var/...`; the editor stores
    // the resolved path on the buffer, so a manifest holding the unresolved
    // one never matches and no step ever finds its file.
    let project_root = fs::canonicalize(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "code-tour");

    fs::create_dir(project_root.join("src")).unwrap();
    fs::write(project_root.join("src/main.rs"), MAIN_RS).unwrap();
    // A file long enough for a step range taller than the visible window.
    let wide: String = (1..=60)
        .map(|n| format!("fn f{n}() {{ let _ = {n}; }}\n"))
        .collect();
    fs::write(project_root.join("src/wide.rs"), wide).unwrap();
    fs::write(
        project_root.join("src/store.rs"),
        "struct Store;\nimpl Store {}\n",
    )
    .unwrap();
    fs::write(
        project_root.join(".fresh-tour.json"),
        tour_json(&project_root),
    )
    .unwrap();
    fs::write(
        project_root.join("storage-tour.json"),
        second_tour_json(&project_root),
    )
    .unwrap();

    (temp_dir, project_root)
}

fn harness_in(project_root: &Path, width: u16, height: u16) -> EditorTestHarness {
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        width,
        height,
        Default::default(),
        project_root.to_path_buf(),
    )
    .unwrap();
    harness
        .open_file(&project_root.join("src/main.rs"))
        .unwrap();
    harness.render().unwrap();
    harness
}

/// Run a palette command by name.
fn run_command(harness: &mut EditorTestHarness, name: &str) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text(name).unwrap();
    // The query line echoes what was typed, so "the name is on screen" is true
    // the moment typing lands, whether or not the palette has filtered its list
    // yet — a wait that cannot fail and therefore gates nothing
    // (CONTRIBUTING.md §16). Enter then fires on whatever row happens to be
    // selected at that instant. Locally the filter finishes in the same frame
    // and it always works; on a loaded runner it need not, and the command
    // never runs, which is how this hung on CI with no failed assertion.
    //
    // Wait for the name *twice*: once as the query, once as the filtered row
    // Enter is about to activate.
    harness
        .wait_until(|h| h.screen_to_string().matches(name).count() >= 2)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
}

/// Load a tour manifest through `Tour: Load Definition...` and wait for its
/// dock panel to paint.
fn load_tour(harness: &mut EditorTestHarness, manifest: &str, tab_marker: &str) {
    run_command(harness, "Tour: Load Definition");
    // Wait for the plugin's own path prompt rather than any prompt — the
    // palette is still closing when the command fires.
    harness
        .wait_until(|h| h.screen_to_string().contains("tour file path"))
        .unwrap();
    // The prompt is prefilled with `.fresh-tour.json`; clear it before typing
    // a different manifest so the two tests can load different files.
    for _ in 0..64 {
        harness
            .send_key(KeyCode::Backspace, KeyModifiers::NONE)
            .unwrap();
    }
    harness.type_text(manifest).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    // The dock tab appears as soon as the buffer is created — a frame before
    // the widget spec paints. Wait for panel *content* (the hint bar is in
    // every layout, compact or not), or every assertion races the first render.
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains(tab_marker) && screen.contains("jump to code")
        })
        .unwrap();
    // Opening a step is an async chain — open the file, await, paint the
    // overlay, then hand focus back to the panel. Content on screen does not
    // mean that chain has finished, and until it has focus is still on the
    // editor, where a key types into the source file instead of stepping the
    // tour.
    //
    // Wait for where focus actually landed, read off the status bar: the tour
    // buffer is read-only plain text (`[RO]` … `Text`), while the editor shows
    // a cursor position and the file's language. Plugin-thread quiescence was
    // the obvious gate and is the wrong one — it is a heuristic that reports
    // quiet while the handoff is still in flight, which is exactly how this
    // timed out on CI but never locally.
    wait_for_panel_focus(harness);
}

/// Block until keyboard focus is on the tour panel, read off the status bar:
/// the tour buffer is read-only plain text (`[RO]` … `Text`), while the editor
/// split shows a cursor position and the file's language.
fn wait_for_panel_focus(harness: &mut EditorTestHarness) {
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("[RO]") && screen.contains("Text")
        })
        .unwrap();
}

/// Press a tour navigation key and wait until the step change has fully landed.
///
/// Changing step re-runs the same async chain as loading one: open the step's
/// file — which moves focus to the editor split — paint the overlay, then hand
/// focus back to the panel. The panel header updates at the *start* of that
/// chain, so waiting only for "Step N of M" returns while focus is still on the
/// editor, and the next key typed goes into the source file instead of stepping
/// the tour.
///
/// That is what hung this module on CI three runs running: the `n` was fine,
/// and the `p` after it landed in wide.rs, so "Step 1 of 2" never came back and
/// the wait blocked until nextest killed it at 180s. Only one test sends a
/// second navigation key, and only that test timed out.
fn press_step_key(harness: &mut EditorTestHarness, key: char, expect: &str) {
    harness
        .send_key(KeyCode::Char(key), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains(expect))
        .unwrap();
    // The header moved; let the rest of the chain finish before trusting focus.
    harness.wait_for_async_quiescence(3).unwrap();
    wait_for_panel_focus(harness);
}

/// Screen row (0-based) of the first line containing `needle`.
fn row_of(harness: &EditorTestHarness, needle: &str) -> usize {
    let screen = harness.screen_to_string();
    screen
        .lines()
        .position(|l| l.contains(needle))
        .unwrap_or_else(|| panic!("expected screen to contain '{needle}'\nScreen:\n{screen}"))
}

// ---------------------------------------------------------------------------
// The panel renders in the dock, below the editor, with widget chrome
// ---------------------------------------------------------------------------

/// Loading a tour paints a dock panel — not a floating popup — carrying the
/// tour title, a step counter, real Prev/Next/Exit buttons and a hint bar,
/// all *below* the editor split rather than on top of it.
#[test]
fn test_tour_renders_as_dock_panel_with_buttons() {
    let (_temp, project_root) = setup_tour_project();
    let mut harness = harness_in(&project_root, 160, 40);

    let manifest = project_root.join(".fresh-tour.json");
    load_tour(
        &mut harness,
        &manifest.display().to_string(),
        "*Tour: Pipeline Tour*",
    );

    let screen = harness.screen_to_string();

    // Dock chrome: the tour is a tab in the shared bottom dock.
    assert!(
        screen.contains("*Tour: Pipeline Tour*"),
        "expected a dock tab for the tour\nScreen:\n{screen}"
    );
    // Header: title + counter.
    assert!(
        screen.contains("Pipeline Tour"),
        "expected the tour title in the panel header\nScreen:\n{screen}"
    );
    assert!(
        screen.contains("Step 1 of 2"),
        "expected a step counter\nScreen:\n{screen}"
    );
    // Buttons, not list rows. `[ … ]` is the widget-library button chrome;
    // the old popup rendered these as bare `Next →` / `Exit Tour` rows.
    assert!(
        screen.contains("[ Next ▶ ]"),
        "expected a Next button\nScreen:\n{screen}"
    );
    assert!(
        screen.contains("[ ✕ Exit ]"),
        "expected an Exit button\nScreen:\n{screen}"
    );
    assert!(
        screen.contains("[ Jump to code ⏎ ]"),
        "expected the source-location Jump button\nScreen:\n{screen}"
    );
    // Hint bar.
    assert!(
        screen.contains("next") && screen.contains("prev"),
        "expected the keyboard hint bar\nScreen:\n{screen}"
    );

    // Geometry: the panel is *below* the editor, not floating over it.
    let editor_tab_row = row_of(&harness, "main.rs");
    let dock_tab_row = row_of(&harness, "*Tour: Pipeline Tour*");
    assert!(
        editor_tab_row < dock_tab_row,
        "expected the tour dock below the editor split \
         (editor_tab_row={editor_tab_row}, dock_tab_row={dock_tab_row})"
    );
}

// ---------------------------------------------------------------------------
// Markdown survives
// ---------------------------------------------------------------------------

/// The step explanation is authored markdown. The old popup dropped the head
/// of every wrapped line, so `- binds the socket` arrived as a fragment and
/// `**built**` kept its asterisks. Assert the rendered prose instead.
#[test]
fn test_step_prose_renders_markdown() {
    let (_temp, project_root) = setup_tour_project();
    // Tall enough that the whole explanation fits: the dock takes ~35% of the
    // height, and asserting on prose that scrolled below the fold would be
    // asserting on something the user cannot see.
    let mut harness = harness_in(&project_root, 160, 60);

    let manifest = project_root.join(".fresh-tour.json");
    load_tour(
        &mut harness,
        &manifest.display().to_string(),
        "*Tour: Pipeline Tour*",
    );
    let screen = harness.screen_to_string();

    // Heading: rendered as text, `##` stripped.
    assert!(
        screen.contains("Where it starts"),
        "expected the heading text\nScreen:\n{screen}"
    );
    assert!(
        !screen.contains("## Where it starts"),
        "expected the `##` marker to be consumed\nScreen:\n{screen}"
    );
    // Bold and inline-code markers are consumed, their text kept whole.
    assert!(
        screen.contains("The listener is built here using TcpListener."),
        "expected inline markers stripped and the sentence intact\nScreen:\n{screen}"
    );
    // Bullets render with a bullet glyph and, crucially, their full text —
    // this is the line the old popup truncated from the front.
    assert!(
        screen.contains("• binds the socket"),
        "expected a rendered bullet with its leading words\nScreen:\n{screen}"
    );
    assert!(
        screen.contains("• spawns the accept loop"),
        "expected the second bullet\nScreen:\n{screen}"
    );
    // Fenced code: the fence markers are chrome and drop out, the code stays.
    assert!(
        screen.contains("let l = TcpListener::bind(addr)?;"),
        "expected the fenced code line\nScreen:\n{screen}"
    );
    assert!(
        !screen.contains("```"),
        "expected fence markers to be consumed\nScreen:\n{screen}"
    );
}

// ---------------------------------------------------------------------------
// Navigation
// ---------------------------------------------------------------------------

/// `n` in the panel advances the step: the counter, the section label and the
/// source-location line all move to step 2, and Prev stops being disabled.
#[test]
fn test_next_key_advances_step() {
    let (_temp, project_root) = setup_tour_project();
    let mut harness = harness_in(&project_root, 160, 40);

    let manifest = project_root.join(".fresh-tour.json");
    load_tour(
        &mut harness,
        &manifest.display().to_string(),
        "*Tour: Pipeline Tour*",
    );
    assert!(harness.screen_to_string().contains("Step 1 of 2"));

    press_step_key(&mut harness, 'n', "Step 2 of 2");

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Handling"),
        "expected step 2's prose\nScreen:\n{screen}"
    );
    assert!(
        screen.contains("2/4 ·") || screen.contains("2/2 ·"),
        "expected the section label to track the step\nScreen:\n{screen}"
    );
    // Last step: Exit turns into Finish.
    assert!(
        screen.contains("[ ✓ Finish ]"),
        "expected Exit to become Finish on the last step\nScreen:\n{screen}"
    );

    // And back.
    press_step_key(&mut harness, 'p', "Step 1 of 2");
}

/// Clicking the `[ Next ▶ ]` button advances the step. The old popup's
/// actions were list rows; this asserts the button is a real hit target.
#[test]
fn test_clicking_next_button_advances_step() {
    let (_temp, project_root) = setup_tour_project();
    let mut harness = harness_in(&project_root, 160, 40);

    let manifest = project_root.join(".fresh-tour.json");
    load_tour(
        &mut harness,
        &manifest.display().to_string(),
        "*Tour: Pipeline Tour*",
    );

    let screen = harness.screen_to_string();
    let row = row_of(&harness, "[ Next ▶ ]");
    // Count characters, not bytes: the header's ▰▱ progress meter is multibyte,
    // so `str::find` returns a byte offset well past the real column.
    let line = screen.lines().nth(row).expect("button row");
    let byte_idx = line.find("[ Next").expect("Next button column");
    let col = line[..byte_idx].chars().count();
    // Click the middle of the label rather than the bracket.
    harness.mouse_click(col as u16 + 4, row as u16).unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("Step 2 of 2"))
        .unwrap();
}

// ---------------------------------------------------------------------------
// Multiple tours
// ---------------------------------------------------------------------------

/// Two manifests give two dock tabs — a tour is a buffer, not a singleton
/// panel — and the second does not evict the first.
#[test]
fn test_two_tours_coexist_as_dock_tabs() {
    let (_temp, project_root) = setup_tour_project();
    let mut harness = harness_in(&project_root, 160, 40);

    let manifest = project_root.join(".fresh-tour.json");
    load_tour(
        &mut harness,
        &manifest.display().to_string(),
        "*Tour: Pipeline Tour*",
    );
    let second = project_root.join("storage-tour.json");
    load_tour(
        &mut harness,
        &second.display().to_string(),
        "*Tour: Storage Tour*",
    );

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("*Tour: Pipeline Tour*") && screen.contains("*Tour: Storage Tour*"),
        "expected both tours as sibling dock tabs\nScreen:\n{screen}"
    );
    // Both tabs share one dock tab bar row.
    assert_eq!(
        row_of(&harness, "*Tour: Pipeline Tour*"),
        row_of(&harness, "*Tour: Storage Tour*"),
        "expected both tour tabs in the same dock tab bar\nScreen:\n{screen}"
    );
    // The newly opened tour is the visible one.
    assert!(
        screen.contains("The store"),
        "expected the second tour's content to be showing\nScreen:\n{screen}"
    );
}

// ---------------------------------------------------------------------------
// The Steps rail
// ---------------------------------------------------------------------------

/// At a wide terminal the panel shows a Steps rail listing every step, with
/// the current one marked — the "where am I in a long tour" affordance the
/// popup had no room for.
#[test]
fn test_steps_rail_lists_all_steps() {
    let (_temp, project_root) = setup_tour_project();
    let mut harness = harness_in(&project_root, 160, 40);

    let manifest = project_root.join(".fresh-tour.json");
    load_tour(
        &mut harness,
        &manifest.display().to_string(),
        "*Tour: Pipeline Tour*",
    );
    let screen = harness.screen_to_string();

    assert!(
        screen.contains("Steps"),
        "expected the Steps rail section\nScreen:\n{screen}"
    );
    assert!(
        screen.contains("Entry point") && screen.contains("The handler"),
        "expected every step title in the rail\nScreen:\n{screen}"
    );
    assert!(
        screen.contains("▸ "),
        "expected the current-step marker in the rail\nScreen:\n{screen}"
    );
}

/// Below the rail breakpoint the rail folds away and the prose takes the full
/// dock width, rather than being squeezed into a column too narrow to read.
#[test]
fn test_narrow_dock_folds_the_steps_rail() {
    let (_temp, project_root) = setup_tour_project();
    let mut harness = harness_in(&project_root, 96, 40);

    let manifest = project_root.join(".fresh-tour.json");
    load_tour(
        &mut harness,
        &manifest.display().to_string(),
        "*Tour: Pipeline Tour*",
    );
    let screen = harness.screen_to_string();

    assert!(
        !screen.contains("╭─ Steps"),
        "expected the Steps rail to fold away on a narrow dock\nScreen:\n{screen}"
    );
    // The prose is still there — folding the rail must not cost content.
    assert!(
        screen.contains("Where it starts"),
        "expected the step prose to survive the fold\nScreen:\n{screen}"
    );
}

// ---------------------------------------------------------------------------
// Teardown
// ---------------------------------------------------------------------------

/// `q` closes the tour: its dock tab goes away and the editor split is intact.
#[test]
fn test_q_closes_the_tour() {
    let (_temp, project_root) = setup_tour_project();
    let mut harness = harness_in(&project_root, 160, 40);

    let manifest = project_root.join(".fresh-tour.json");
    load_tour(
        &mut harness,
        &manifest.display().to_string(),
        "*Tour: Pipeline Tour*",
    );

    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| !h.screen_to_string().contains("*Tour: Pipeline Tour*"))
        .unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("main.rs"),
        "expected the editor split to survive closing the tour\nScreen:\n{screen}"
    );
}

// ---------------------------------------------------------------------------
// Clicks that are not on a control
// ---------------------------------------------------------------------------

/// A click inside the panel that lands on no control — a `labeledSection`
/// border — must change nothing.
///
/// Before the fix such a click fell through to ordinary cursor placement.
/// The panel's cursor is hidden but the viewport still follows it, so the
/// click scrolled the panel's own header, buttons and hint bar out of view
/// with no way to scroll them back.
#[test]
fn test_click_on_panel_border_changes_nothing() {
    let (_temp, project_root) = setup_tour_project();
    let mut harness = harness_in(&project_root, 160, 40);

    let manifest = project_root.join(".fresh-tour.json");
    load_tour(
        &mut harness,
        &manifest.display().to_string(),
        "*Tour: Pipeline Tour*",
    );

    let before = harness.screen_to_string();
    // The sections' bottom border — inside the panel, on no widget.
    let border_row = row_of(&harness, "╰─");
    harness.mouse_click(80, border_row as u16).unwrap();
    harness.wait_for_async_quiescence(3).unwrap();

    let after = harness.screen_to_string();
    assert_eq!(
        before, after,
        "a click on panel chrome must leave the screen untouched"
    );
    // Spelled out, because an equal-screens assertion is easy to satisfy
    // vacuously if the panel never rendered in the first place.
    assert!(
        after.contains("Step 1 of 2") && after.contains("[ Next ▶ ]"),
        "expected the panel header to survive the click\nScreen:\n{after}"
    );
}

/// A click in the prose column must not select a step in the rail beside it.
///
/// Two side-by-side lists put two row hits on the same buffer row. The
/// row-aware fallback used to return the first one regardless of column, so
/// every click in the right-hand column selected a row in the left-hand list.
#[test]
fn test_click_in_prose_column_does_not_select_a_rail_step() {
    let (_temp, project_root) = setup_tour_project();
    let mut harness = harness_in(&project_root, 160, 40);

    let manifest = project_root.join(".fresh-tour.json");
    load_tour(
        &mut harness,
        &manifest.display().to_string(),
        "*Tour: Pipeline Tour*",
    );
    assert!(harness.screen_to_string().contains("Step 1 of 2"));

    // The screen row carrying the rail's second step — and, to its right, the
    // prose column. Clicking the prose side must not act on the rail.
    let row = row_of(&harness, "2  The handler") as u16;
    harness.mouse_click(120, row).unwrap();
    harness.wait_for_async_quiescence(3).unwrap();
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Step 1 of 2"),
        "clicking the prose column must not select the rail row beside it\nScreen:\n{screen}"
    );

    // The seam between the two sections — the prose side's border cell — is
    // the boundary case: it is visibly not the rail, so it must not act on it.
    let seam = {
        let line = screen.lines().nth(row as usize).expect("rail row");
        let byte_idx = line.find("││").expect("column seam");
        line[..byte_idx].chars().count() + 1
    };
    harness.mouse_click(seam as u16, row).unwrap();
    harness.wait_for_async_quiescence(3).unwrap();
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Step 1 of 2"),
        "clicking the seam between the columns must not select a rail row\nScreen:\n{screen}"
    );

    // The rail itself still works — otherwise the assertions above would pass
    // for a panel whose rail had simply stopped routing clicks at all.
    harness.mouse_click(20, row).unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Step 2 of 2"))
        .unwrap();
}

// ---------------------------------------------------------------------------
// The step highlight
// ---------------------------------------------------------------------------

/// The step's line range is painted in the editor.
///
/// This never worked before the dock rewrite: the overlay looked its buffer up
/// with a repo-relative path that `findBufferByPath` cannot match, so nothing
/// was ever added. Step 2's range is deliberately taller than the window the
/// dock leaves visible — a single overlay spanning a tall range renders
/// unreliably, so the plugin paints one overlay per line.
#[test]
fn test_step_range_is_highlighted_in_the_editor() {
    let (_temp, project_root) = setup_tour_project();
    let mut harness = harness_in(&project_root, 160, 40);

    let manifest = project_root.join(".fresh-tour.json");
    load_tour(
        &mut harness,
        &manifest.display().to_string(),
        "*Tour: Pipeline Tour*",
    );

    // Step 2 — 40 lines over a 60-line file, more than the editor pane shows.
    press_step_key(&mut harness, 'n', "Step 2 of 2");

    // The highlight is a background colour, so read rendered cell styles
    // rather than the plain text. Painting it is the tail of an async chain,
    // so wait for it to land — waiting for *a* highlighted row and then
    // asserting *which* rows carry it keeps the check from passing vacuously.
    let highlighted = |h: &EditorTestHarness| -> Vec<u16> {
        (2..30u16)
            .filter(|row| {
                h.get_cell_style(20, *row)
                    .is_some_and(|s| s.bg == Some(ratatui::style::Color::Rgb(42, 74, 106)))
            })
            .collect()
    };
    harness.wait_until(|h| !highlighted(h).is_empty()).unwrap();

    // The step's range is taller than the pane, so every code row on screen
    // carries it — contiguously, with no unhighlighted gap in the middle.
    let rows = highlighted(&harness);
    let (first, last) = (rows[0], rows[rows.len() - 1]);
    assert_eq!(
        rows,
        (first..=last).collect::<Vec<_>>(),
        "the highlight must be one unbroken run, not scattered rows\nScreen:\n{}",
        harness.screen_to_string()
    );
    assert!(
        rows.len() > 10,
        "a step range taller than the pane should fill it, got {} rows\nScreen:\n{}",
        rows.len(),
        harness.screen_to_string()
    );
}

// =========================================================================
// Showcase
// =========================================================================
//
// An animated walkthrough of the tour, built on the same `BlogShowcase`
// machinery as the other feature demos, so it lands in the standard layout
// (`frames/` + `showcase.json`, both gitignored; the committed artifact is
// `showcase.gif`). `#[ignore]`d — it writes into docs/, so it runs on request:
//
//   cargo nextest run -p fresh-editor --test e2e_tests \
//       code_tour_showcase -- --ignored --nocapture
//   scripts/frames-to-gif.sh docs/blog/productivity/code-tour --colors 64 --dither none

/// A tour with enough shape to show every state: prose with headings, bullets
/// and a fence; a range taller than the pane; and a step whose file is gone.
fn showcase_tour_json(root: &Path) -> String {
    let root = root.display().to_string();
    format!(
        r###"{{
  "title": "Request Pipeline",
  "description": "How a request reaches a handler",
  "schema_version": "1.0",
  "steps": [
    {{
      "step_id": 1,
      "title": "Entry point",
      "file_path": "{root}/src/main.rs",
      "lines": [1, 3],
      "explanation": "## Where it starts\n\nThe listener is **built** here using `TcpListener`.\n\n- binds the socket\n- spawns the accept loop\n\n```rust\nlet l = TcpListener::bind(addr)?;\n```"
    }},
    {{
      "step_id": 2,
      "title": "Dispatch",
      "file_path": "{root}/src/wide.rs",
      "lines": [5, 44],
      "explanation": "## Handling\n\nEach connection is dispatched to `handle` on its own task.\n\nThe highlighted range is *taller than the pane* — it stays painted as you scroll."
    }},
    {{
      "step_id": 3,
      "title": "Storage",
      "file_path": "{root}/src/store.rs",
      "lines": [1, 2],
      "explanation": "## Persisting\n\nHandlers write through `Store`."
    }},
    {{
      "step_id": 4,
      "title": "A step whose file moved",
      "file_path": "{root}/src/deleted.rs",
      "lines": [1, 4],
      "explanation": "## Missing file\n\nA tour outlives the code it describes. The step still reads; only the jump is unavailable."
    }}
  ]
}}"###
    )
}

fn snap(h: &mut EditorTestHarness, s: &mut BlogShowcase, key: Option<&str>, ms: u32) {
    h.render().unwrap();
    let c = h.screen_cursor_position();
    s.capture_frame(h.buffer(), c, key, None, ms).unwrap();
}

fn hold(h: &mut EditorTestHarness, s: &mut BlogShowcase, count: usize, ms: u32) {
    h.render().unwrap();
    let c = h.screen_cursor_position();
    s.hold_frames(h.buffer(), c, None, None, count, ms).unwrap();
}

#[test]
#[ignore]
fn code_tour_showcase() {
    let (_temp, project_root) = setup_tour_project();
    fs::write(
        project_root.join(".fresh-tour.json"),
        showcase_tour_json(&project_root),
    )
    .unwrap();
    let manifest = project_root.join(".fresh-tour.json").display().to_string();

    // 40 rows: the dock needs the height to render its hint bar, which
    // `load_tour` waits on.
    let mut h = harness_in(&project_root, 120, 40);
    let mut s = BlogShowcase::new(
        "productivity/code-tour",
        "Code Tours",
        "Walk a codebase step by step, in a dock panel beside the code.",
    );

    hold(&mut h, &mut s, 4, 100);

    // Start it from the palette.
    run_command(&mut h, "Tour: Load Definition");
    h.wait_until(|h| h.screen_to_string().contains("tour file path"))
        .unwrap();
    snap(&mut h, &mut s, Some("Ctrl+P"), 150);
    hold(&mut h, &mut s, 8, 100);

    for _ in 0..64 {
        h.send_key(KeyCode::Backspace, KeyModifiers::NONE).unwrap();
    }
    h.type_text(&manifest).unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("*Tour: Request Pipeline*") && s.contains("jump to code")
    })
    .unwrap();
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("[RO]") && s.contains("Text")
    })
    .unwrap();
    snap(&mut h, &mut s, Some("Enter"), 200);
    // Step 1: the panel beside the code, its range highlighted above.
    hold(&mut h, &mut s, 22, 100);

    // Step 2: a range taller than the pane, painted end to end.
    h.send_key(KeyCode::Char('n'), KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Step 2 of 4"))
        .unwrap();
    snap(&mut h, &mut s, Some("n"), 200);
    hold(&mut h, &mut s, 22, 100);

    h.send_key(KeyCode::Char('n'), KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Step 3 of 4"))
        .unwrap();
    snap(&mut h, &mut s, Some("n"), 200);
    hold(&mut h, &mut s, 16, 100);

    // Step 4: the file is gone. The step still reads; the jump is not offered
    // and the previous step's highlight is torn down rather than left behind.
    h.send_key(KeyCode::Char('n'), KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Step 4 of 4"))
        .unwrap();
    snap(&mut h, &mut s, Some("n"), 200);
    hold(&mut h, &mut s, 24, 100);

    // A second tour opens beside the first in the dock's tab bar.
    load_tour(
        &mut h,
        &project_root.join("storage-tour.json").display().to_string(),
        "*Tour: Storage Tour*",
    );
    snap(&mut h, &mut s, None, 200);
    hold(&mut h, &mut s, 26, 100);

    s.finalize().unwrap();
}
