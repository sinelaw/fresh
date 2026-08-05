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
/// A path as a JSON string literal — quotes and escapes included.
///
/// These manifests are built by interpolation, and a Windows path interpolated
/// raw is not valid JSON: `C:\Users\...` carries `\U`, `\A`, `\T`, none of
/// which are escape sequences, so the plugin's parse throws, no panel is ever
/// mounted, and every wait in this module blocks forever. That took the whole
/// module out on windows-latest while ubuntu and macOS were green — the tests
/// are not Windows-specific, only the fixture was.
fn json_path(path: &Path) -> String {
    serde_json::to_string(&path.display().to_string()).expect("path is UTF-8")
}

fn tour_json(root: &Path) -> String {
    let main_rs = json_path(&root.join("src/main.rs"));
    let wide_rs = json_path(&root.join("src/wide.rs"));
    format!(
        r###"{{
  "title": "Pipeline Tour",
  "description": "How a request reaches the handler",
  "schema_version": "1.0",
  "steps": [
    {{
      "step_id": 1,
      "title": "Entry point",
      "file_path": {main_rs},
      "lines": [1, 3],
      "explanation": "## Where it starts\n\nThe listener is **built** here using `TcpListener`.\n\n- binds the socket\n- spawns the accept loop\n\n```rust\nlet l = TcpListener::bind(addr)?;\n```"
    }},
    {{
      "step_id": 2,
      "title": "The handler",
      "file_path": {wide_rs},
      "lines": [5, 44],
      "explanation": "## Handling\n\nEach connection is dispatched to `handle` on its own task."
    }}
  ]
}}"###
    )
}

fn second_tour_json(root: &Path) -> String {
    let store_rs = json_path(&root.join("src/store.rs"));
    format!(
        r###"{{
  "title": "Storage Tour",
  "description": "The storage layer",
  "schema_version": "1.0",
  "steps": [
    {{
      "step_id": 1,
      "title": "The store",
      "file_path": {store_rs},
      "lines": [1, 2],
      "explanation": "## The store\n\nKeys live in a `BTreeMap`."
    }}
  ]
}}"###
    )
}

const MAIN_RS: &str = "fn main() {\n    let l = listen();\n}\n\nfn handle() {\n    todo!()\n}\n";

/// A tour whose Steps rail *and* prose column both overflow their visible
/// rows: twelve steps, and an explanation long enough that most of it sits
/// below the fold. Step 1 also leads with an unbroken token far longer than
/// any prose column, so the wrap-vs-truncate behaviour is on screen without
/// scrolling. Titles are deliberately short — several assertions check that
/// no `…` appears anywhere, so nothing else may legitimately truncate.
fn overflow_tour_json(root: &Path) -> String {
    let main_rs = json_path(&root.join("src/main.rs"));
    let para = "The backend emits MoveTo only when the cell it is about to \
                draw is not immediately to the right of the last one it \
                drew, which means consecutive columns are printed as a run \
                with no repositioning at all.";
    let long_word = format!("wordwrap{}", "x".repeat(220));
    let explanation =
        format!("## Heading one\n\n{long_word}\n\n{para}\n\n{para}\n\n{para}\n\n{para}\n\n{para}");
    let steps = (1..=12)
        .map(|i| {
            format!(
                r###"{{
      "step_id": {i},
      "title": "S{i}",
      "file_path": {main_rs},
      "lines": [1, 3],
      "explanation": "{explanation}"
    }}"###,
                explanation = explanation.replace('\n', "\\n"),
            )
        })
        .collect::<Vec<_>>()
        .join(",\n    ");
    format!(
        r###"{{
  "title": "Overflow Tour",
  "description": "Both lists overflow",
  "schema_version": "1.0",
  "steps": [
    {steps}
  ]
}}"###
    )
}

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
    fs::write(
        project_root.join("overflow-tour.json"),
        overflow_tour_json(&project_root),
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
    // Wait for the plugin's file-pick browser rather than any prompt — the
    // palette is still closing when the command fires. Its filter input
    // starts empty; an absolute path typed into it resolves directly.
    harness
        .wait_until(|h| h.screen_to_string().contains("tour file path"))
        .unwrap();
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
    // Opening a step is an async chain — open the file, paint the overlay,
    // then hand focus back to the panel. Content on screen does not mean that
    // chain has finished; wait for its rendered end state before the caller
    // sends a key.
    wait_for_step_settled(harness);
}

/// Screen rows (within the editor area) whose column 20 carries the step
/// highlight's background colour — the theme key `ui.semantic_highlight_bg`,
/// which the harness's high-contrast theme resolves to rgb(0, 25, 55). The
/// highlight is painted `extendToLineEnd`, so column 20 is covered for every
/// fixture file in this module.
fn highlighted_rows(h: &EditorTestHarness) -> Vec<u16> {
    (2..30u16)
        .filter(|row| {
            h.get_cell_style(20, *row)
                .is_some_and(|s| s.bg == Some(ratatui::style::Color::Rgb(0, 25, 55)))
        })
        .collect()
}

/// Block until the current step's async chain has fully landed, read entirely
/// off rendered output: a step highlight is painted in the editor AND the
/// panel holds focus (status bar shows the tour buffer's `[RO]` … `Text`,
/// where the editor split would show a cursor position and the file's
/// language) — both in the same frame.
///
/// Panel focus alone is not a sound wait: the panel is focused *twice* per
/// step — transiently, before `revealStep` opens the step's file (focus →
/// editor split), and finally when the chain hands focus back. A wait that
/// returns on the transient state lets the next key race the chain — the key
/// is dispatched to the plugin, whose handler then resolves it against a
/// mid-flight active-buffer snapshot — which is how this module used to time
/// out on CI. The highlight only paints after the file open, so
/// highlight ∧ panel-focus is first true once the chain is done. (A
/// previously-loaded tour's highlight can satisfy the highlight half, so
/// tests that navigate by key load a single tour.)
///
/// Plugin-thread quiescence is deliberately not used here — it is a
/// heuristic that reports quiet while the handoff is still in flight.
fn wait_for_step_settled(harness: &mut EditorTestHarness) {
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            !highlighted_rows(h).is_empty() && screen.contains("[RO]") && screen.contains("Text")
        })
        .unwrap();
}

/// Press a tour navigation key and wait until the step change has fully landed.
///
/// Changing step re-runs the same async chain as loading one: open the step's
/// file — which moves focus to the editor split — paint the overlay, then hand
/// focus back to the panel. Waiting only for "Step N of M" is not enough: the
/// header can repaint mid-chain (a dock resize re-renders it with the new step
/// already set), so the wait continues to the chain's rendered end state
/// before the caller trusts focus or sends another key.
fn press_step_key(harness: &mut EditorTestHarness, key: char, expect: &str) {
    harness
        .send_key(KeyCode::Char(key), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains(expect))
        .unwrap();
    wait_for_step_settled(harness);
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
/// A manifest path must survive being written into JSON.
///
/// Interpolating a path straight into the manifest is fine until the path has
/// backslashes in it, at which point the manifest stops being JSON at all.
#[test]
fn json_path_escapes_a_windows_path() {
    let raw = r"C:\Users\runneradmin\AppData\Local\Temp\.tmpAb12\project_root\src\main.rs";
    let literal = json_path(Path::new(raw));
    let parsed: String =
        serde_json::from_str(&literal).expect("an interpolated path must still be valid JSON");
    assert_eq!(parsed, raw);
}

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
    // rather than the plain text. `press_step_key` already waited for *a*
    // highlighted row; step 2's range fills the pane, and waiting for more
    // rows than step 1's three-line range could ever paint keeps the check
    // from passing on step 1's leftovers.
    harness
        .wait_until(|h| highlighted_rows(h).len() > 10)
        .unwrap();

    // The step's range is taller than the pane, so every code row on screen
    // carries it — contiguously, with no unhighlighted gap in the middle.
    let rows = highlighted_rows(&harness);
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

// ---------------------------------------------------------------------------
// Scrolling: the wheel, the scrollbars, and the selection band
// ---------------------------------------------------------------------------

/// Load the overflow tour (12 steps, prose far taller than the visible
/// window) and return the screen row of the two lists' first content row —
/// the row directly below the sections' top border.
fn load_overflow_tour(harness: &mut EditorTestHarness, project_root: &Path) -> usize {
    let manifest = project_root.join("overflow-tour.json");
    load_tour(
        harness,
        &manifest.display().to_string(),
        "*Tour: Overflow Tour*",
    );
    row_of(harness, "1/12 ·") + 1
}

/// The two halves of a panel screen row, split at the `││` seam between
/// the Steps rail and the prose column.
fn split_at_seam(harness: &EditorTestHarness, row: usize) -> (String, String) {
    let seam_chars = prose_start_col(harness);
    let screen = harness.screen_to_string();
    let line = screen.lines().nth(row).expect("panel row");
    let seam_byte = line
        .char_indices()
        .nth(seam_chars)
        .map(|(i, _)| i)
        .unwrap_or(line.len());
    (line[..seam_byte].to_string(), line[seam_byte..].to_string())
}

/// Screen column (in chars == display cells here; the border row has no
/// wide glyphs) where the prose section begins — its `╭`, read off the
/// sections' shared top-border row. The rail's right border can't serve
/// as the seam anymore: the rail's overlay scrollbar paints over it.
fn prose_start_col(harness: &EditorTestHarness) -> usize {
    let screen = harness.screen_to_string();
    let border = screen
        .lines()
        .find(|l| l.contains("╭─ Steps"))
        .expect("rail top border row");
    border
        .chars()
        .enumerate()
        .filter(|(_, c)| *c == '╭')
        .map(|(i, _)| i)
        .nth(1)
        .expect("prose top border")
}

/// The mouse wheel scrolls the list under the pointer: over the prose it
/// scrolls the prose and leaves the rail alone; over the rail it scrolls
/// the rail.
///
/// Before the fix, wheel routing picked the *first* scrollable widget in
/// the panel spec — the Steps rail — so a wheel anywhere in the panel
/// scrolled the rail and the prose never moved.
#[test]
fn test_wheel_scrolls_the_hovered_list() {
    let (_temp, project_root) = setup_tour_project();
    let mut harness = harness_in(&project_root, 160, 40);
    let first_row = load_overflow_tour(&mut harness, &project_root);

    let (rail_before, prose_before) = split_at_seam(&harness, first_row);
    assert!(
        rail_before.contains("▸ ") && rail_before.contains("S1"),
        "rail must start at step 1\nrow: {rail_before}"
    );
    // The step's location leads the prose.
    assert!(
        prose_before.contains("lines 1–3"),
        "prose must start at its location line\nrow: {prose_before}"
    );

    // Wheel down over the prose column.
    let prose_col = (prose_start_col(&harness) + 4) as u16;
    harness
        .mouse_scroll_down(prose_col, first_row as u16)
        .unwrap();
    harness
        .wait_until(|h| !split_at_seam(h, first_row).1.contains("lines 1–3"))
        .unwrap();
    let (rail_after, _) = split_at_seam(&harness, first_row);
    assert_eq!(
        rail_before, rail_after,
        "a wheel over the prose column must not scroll the Steps rail"
    );

    // Wheel down over the rail: now the rail scrolls.
    harness.mouse_scroll_down(5, first_row as u16).unwrap();
    harness
        .wait_until(|h| !split_at_seam(h, first_row).0.contains("S1"))
        .unwrap();
}

/// Both overflowing lists paint a scrollbar in their rightmost inner
/// column. Before the fix, split-mounted widget panels dropped their
/// scroll-region geometry and painted no scrollbar at all.
#[test]
fn test_overflowing_lists_show_scrollbars() {
    let (_temp, project_root) = setup_tour_project();
    let mut harness = harness_in(&project_root, 160, 40);
    let first_row = load_overflow_tour(&mut harness, &project_root);

    // The sections' top-border row: `╭─ Steps ──…──╮╭─ 1/12 · S1 ──…──╮`.
    // Every glyph on it is single-width, so char index == screen column.
    let border_row = first_row - 1;
    let screen = harness.screen_to_string();
    let line = screen.lines().nth(border_row).expect("border row");
    let closes: Vec<usize> = line
        .chars()
        .enumerate()
        .filter(|(_, c)| *c == '╮')
        .map(|(i, _)| i)
        .collect();
    assert_eq!(closes.len(), 2, "expected both section borders\n{line}");

    // The scrollbar paints ON the section's border column (where the
    // `╮` sits), so nothing — not even the selection band — extends
    // past it.
    for close in closes {
        let sb_col = close as u16;
        assert!(
            harness.is_scrollbar_thumb_at(sb_col, first_row as u16)
                || harness.is_scrollbar_track_at(sb_col, first_row as u16),
            "expected a scrollbar cell on the border col {sb_col} row {first_row}, got {:?}\nScreen:\n{}",
            harness.get_cell_style(sb_col, first_row as u16),
            harness.screen_to_string()
        );
    }
}

/// The selected rows' highlight stays inside the panel. Before the fix,
/// the selection band's `extend_to_line_end` survived the row zipper and
/// the painter flooded every cell right of the panel border — a stray
/// highlight block at the screen's right edge.
#[test]
fn test_selection_highlight_stays_inside_the_panel() {
    let (_temp, project_root) = setup_tour_project();
    let mut harness = harness_in(&project_root, 160, 40);
    let first_row = load_overflow_tour(&mut harness, &project_root);

    // The selection band's colour is theme-dependent: read it off a cell
    // inside the rail's selected row (step 1, marked `▸`) rather than
    // hard-coding a theme value. An unselected row's cell pins down the
    // panel's plain background, proving the two really differ — without
    // that, "no band past the border" could pass vacuously.
    let selection_bg = harness
        .get_cell_style(5, first_row as u16)
        .and_then(|s| s.bg)
        .expect("the rail's selected row must carry a selection background");
    let plain_bg = harness
        .get_cell_style(5, first_row as u16 + 1)
        .and_then(|s| s.bg);
    assert_ne!(
        Some(selection_bg),
        plain_bg,
        "selected and unselected rail rows must differ for this test to bite"
    );

    // The cells past the prose section's right border must not carry the
    // band. Before the fix they did — the tail-fill flooded them.
    let border_row = first_row - 1;
    let screen = harness.screen_to_string();
    let line = screen.lines().nth(border_row).expect("border row");
    let panel_edge = line
        .chars()
        .enumerate()
        .filter(|(_, c)| *c == '╮')
        .map(|(i, _)| i)
        .last()
        .expect("prose top border");
    for col in (panel_edge + 1)..(panel_edge + 3) {
        let style = harness.get_cell_style(col as u16, first_row as u16);
        assert!(
            !style.is_some_and(|s| s.bg == Some(selection_bg)),
            "selection band leaked past the panel border at col {col}: {style:?}\nScreen:\n{}",
            harness.screen_to_string()
        );
    }
}

/// The prose column is a real (read-only) text area: Shift+Down extends
/// a visible selection over the rendered markdown, and `C-c` puts the
/// selected *rendered* text — never markdown markers or panel chrome —
/// on the clipboard. The text stays read-only: typing changes nothing.
#[test]
fn test_prose_selects_and_copies_rendered_text() {
    let (_temp, project_root) = setup_tour_project();
    let mut harness = harness_in(&project_root, 160, 40);
    let first_row = load_overflow_tour(&mut harness, &project_root);
    harness.editor_mut().set_clipboard_for_test(String::new());

    // The prose document is focused on load; the caret sits on its
    // first rendered line (the step's location). Extend the selection
    // three lines down, through the blank line and the heading.
    for _ in 0..3 {
        harness
            .send_key(KeyCode::Down, KeyModifiers::SHIFT)
            .unwrap();
    }
    // The selection band is visible on the prose's first row: some cell
    // right of the seam changed background versus an unselected row.
    harness.render().unwrap();
    let screen = harness.screen_to_string();
    let seam_col = prose_start_col(&harness);
    let sel_cell = harness.get_cell_style(seam_col as u16 + 4, first_row as u16);
    let plain_cell = harness.get_cell_style(seam_col as u16 + 4, first_row as u16 + 4);
    assert_ne!(
        sel_cell.and_then(|s| s.bg),
        plain_cell.and_then(|s| s.bg),
        "expected a visible selection band on the selected prose rows\nScreen:\n{screen}"
    );

    // Copy — the clipboard gets the rendered heading, with the `##`
    // marker consumed and no `│` chrome from the panel row.
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    let copied = harness.editor_mut().clipboard_content_for_test();
    assert!(
        copied.contains("Heading one"),
        "expected the rendered heading in the clipboard, got {copied:?}"
    );
    assert!(
        !copied.contains('#') && !copied.contains('│'),
        "clipboard must carry rendered text, not markers or chrome: {copied:?}"
    );

    // Read-only: typing must not change the document. Compare everything
    // above the status bar — the buffer's own editing-disabled guard may
    // put a message there, which is fine; the panel must be untouched.
    let strip_status = |screen: String| {
        let lines: Vec<&str> = screen.lines().collect();
        lines[..lines.len().saturating_sub(2)].join("\n")
    };
    let before = strip_status(harness.screen_to_string());
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_async_quiescence(3).unwrap();
    // `z` is unbound in the tour panel mode and the document rejects
    // insertion — the prose is untouched.
    let after = strip_status(harness.screen_to_string());
    assert_eq!(
        before, after,
        "typing into the read-only prose must change nothing"
    );
}

/// Moving the caret to the bottom of the prose scrolls only the prose
/// viewport — never the panel itself. Before the fix, the caret published
/// a hardware-cursor position and the panel *buffer's* viewport followed
/// it: the header scrolled off the top and `~` rows appeared below.
#[test]
fn test_prose_caret_never_scrolls_the_panel() {
    let (_temp, project_root) = setup_tour_project();
    let mut harness = harness_in(&project_root, 160, 40);
    let first_row = load_overflow_tour(&mut harness, &project_root);

    let screen = harness.screen_to_string();
    let tab_row = screen
        .lines()
        .position(|l| l.contains("*Tour: Overflow Tour*"))
        .expect("dock tab row");
    for _ in 0..30 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.wait_for_async_quiescence(3).unwrap();
    let after = harness.screen_to_string();
    assert_eq!(
        after
            .lines()
            .position(|l| l.contains("*Tour: Overflow Tour*")),
        Some(tab_row),
        "the dock tab must not move when the caret pages the prose\nScreen:\n{after}"
    );
    assert!(
        after.contains("Step 1 of 12"),
        "the panel header must stay visible\nScreen:\n{after}"
    );
    // The prose viewport itself scrolled (caret is far below the fold).
    let (_, prose_after) = split_at_seam(&harness, first_row);
    assert!(
        !prose_after.contains("Heading one"),
        "the prose viewport should have followed the caret\nScreen:\n{after}"
    );
}

/// Left / Right move the prose caret; they no longer step the tour.
/// `n`/`p` (and the buttons) remain the step keys.
#[test]
fn test_left_right_move_the_caret_not_the_step() {
    let (_temp, project_root) = setup_tour_project();
    let mut harness = harness_in(&project_root, 160, 40);
    load_overflow_tour(&mut harness, &project_root);

    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_async_quiescence(3).unwrap();
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Step 1 of 12"),
        "Right must not advance the step\nScreen:\n{screen}"
    );
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.wait_for_async_quiescence(3).unwrap();
    assert!(
        harness.screen_to_string().contains("Step 1 of 12"),
        "Left must not step back"
    );
    // Non-vacuous: `n` still advances.
    press_step_key(&mut harness, 'n', "Step 2 of 12");
}

/// With the Steps rail focused (`g`), ↑/↓ don't just move a silent
/// selection — they navigate to that step, exactly like clicking it,
/// and the rail keeps focus so the next arrow keeps browsing.
#[test]
fn test_rail_arrows_navigate_steps() {
    let (_temp, project_root) = setup_tour_project();
    let mut harness = harness_in(&project_root, 160, 40);

    let manifest = project_root.join(".fresh-tour.json");
    load_tour(
        &mut harness,
        &manifest.display().to_string(),
        "*Tour: Pipeline Tour*",
    );
    assert!(harness.screen_to_string().contains("Step 1 of 2"));

    harness
        .send_key(KeyCode::Char('g'), KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_async_quiescence(3).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Step 2 of 2"))
        .unwrap();
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Handling"),
        "the prose must follow the rail navigation\nScreen:\n{screen}"
    );
    // The rail kept focus: ↑ browses straight back.
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Step 1 of 2"))
        .unwrap();
}

/// A step whose range is taller than the pane keeps the range's FIRST
/// line (where the cursor lands) on screen, instead of centring the
/// range's middle and scrolling the top away.
#[test]
fn test_tall_step_range_keeps_its_first_line_visible() {
    let (_temp, project_root) = setup_tour_project();
    let mut harness = harness_in(&project_root, 160, 40);

    let manifest = project_root.join(".fresh-tour.json");
    load_tour(
        &mut harness,
        &manifest.display().to_string(),
        "*Tour: Pipeline Tour*",
    );
    // Step 2: wide.rs lines 5–44 — far taller than the ~23 visible rows.
    press_step_key(&mut harness, 'n', "Step 2 of 2");
    harness
        .wait_until(|h| h.screen_to_string().contains("fn f5()"))
        .unwrap();
}

/// Every line of the step range paints a full-width band — not just the
/// first. Before the fix a single range-wide overlay tail-filled only
/// the row it started on; the rest highlighted their text width alone.
#[test]
fn test_step_highlight_bands_are_full_width_on_every_line() {
    let (_temp, project_root) = setup_tour_project();
    let mut harness = harness_in(&project_root, 160, 40);

    let manifest = project_root.join(".fresh-tour.json");
    load_tour(
        &mut harness,
        &manifest.display().to_string(),
        "*Tour: Pipeline Tour*",
    );
    press_step_key(&mut harness, 'n', "Step 2 of 2");

    // wide.rs rows are ~28 columns of text; column 60 is well past every
    // line's end, so a band there proves the full-width fill. Require it
    // on at least three consecutive highlighted rows.
    let banded = |h: &EditorTestHarness| -> Vec<u16> {
        (2..25u16)
            .filter(|row| {
                h.get_cell_style(60, *row)
                    .is_some_and(|s| s.bg == Some(ratatui::style::Color::Rgb(0, 25, 55)))
            })
            .collect()
    };
    harness.wait_until(|h| banded(h).len() >= 3).unwrap();
    let rows = banded(&harness);
    let consecutive = rows.windows(2).all(|w| w[1] == w[0] + 1);
    assert!(
        consecutive,
        "full-width bands must cover consecutive range rows, got {rows:?}\nScreen:\n{}",
        harness.screen_to_string()
    );
}

/// Clicking the prose places a VISIBLE caret (a reversed cell) at the
/// clicked spot. The reversed flag used to be dropped on the way into
/// buffer overlays, so the caret existed but never painted.
#[test]
fn test_click_places_a_visible_caret_in_the_prose() {
    use ratatui::style::Modifier;
    let (_temp, project_root) = setup_tour_project();
    let mut harness = harness_in(&project_root, 160, 40);
    let first_row = load_overflow_tour(&mut harness, &project_root);

    let col = (prose_start_col(&harness) + 6) as u16;
    harness.mouse_click(col, first_row as u16).unwrap();
    harness.wait_for_async_quiescence(3).unwrap();
    harness.render().unwrap();
    let reversed_on_row = (0..160u16).any(|x| {
        harness
            .get_cell_style(x, first_row as u16)
            .is_some_and(|s| s.add_modifier.contains(Modifier::REVERSED))
    });
    assert!(
        reversed_on_row,
        "expected a reversed caret cell on the clicked prose row\nScreen:\n{}",
        harness.screen_to_string()
    );
}

/// Clicking a step in the rail keeps the rail focused, so ↑/↓ right
/// after the click keep browsing steps instead of moving the prose
/// caret.
#[test]
fn test_rail_click_keeps_focus_for_arrow_browsing() {
    let (_temp, project_root) = setup_tour_project();
    let mut harness = harness_in(&project_root, 160, 40);

    let manifest = project_root.join(".fresh-tour.json");
    load_tour(
        &mut harness,
        &manifest.display().to_string(),
        "*Tour: Pipeline Tour*",
    );
    let row = row_of(&harness, "2  The handler") as u16;
    harness.mouse_click(20, row).unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Step 2 of 2"))
        .unwrap();
    harness.wait_for_async_quiescence(3).unwrap();
    // The rail kept focus: Up browses straight back to step 1.
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Step 1 of 2"))
        .unwrap();
}

/// Long prose lines word-wrap; none is ellipsis-truncated. The host
/// renders the panel at `viewport.width - 2`, and the plugin used to
/// mirror the column math from the unreduced viewport width — at widths
/// where the rounding differed, every full-width wrapped line lost its
/// last character to a `…`. 219 columns is such a width.
#[test]
fn test_long_prose_lines_wrap_without_truncation() {
    let (_temp, project_root) = setup_tour_project();
    let mut harness = harness_in(&project_root, 219, 50);
    load_overflow_tour(&mut harness, &project_root);

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("wordwrapxxx"),
        "the long token must be on screen for the assertion to bite\nScreen:\n{screen}"
    );
    assert!(
        !screen.contains('…'),
        "prose must word-wrap, never ellipsis-truncate\nScreen:\n{screen}"
    );
}

/// A blank line inside the step range keeps its full-width band even
/// when indentation guides are on. The guide synthesis for blank rows
/// painted its cells with the default bg before the tail fill ran, so
/// every guide column punched a hole in the band.
#[test]
fn test_blank_line_in_range_keeps_band_under_indent_guides() {
    use fresh::config::{Config, IndentationGuideMode};

    let (_temp, project_root) = setup_tour_project();
    fs::write(
        project_root.join("src/gap.rs"),
        "fn outer() {\n    let a = 1;\n\n    let b = 2;\n}\n",
    )
    .unwrap();
    let gap_rs = json_path(&project_root.join("src/gap.rs"));
    fs::write(
        project_root.join("gap-tour.json"),
        format!(
            r###"{{
  "title": "Gap Tour",
  "description": "Blank line in range",
  "schema_version": "1.0",
  "steps": [
    {{
      "step_id": 1,
      "title": "Gap",
      "file_path": {gap_rs},
      "lines": [1, 5],
      "explanation": "A blank line sits inside this range."
    }}
  ]
}}"###
        ),
    )
    .unwrap();

    let mut config = Config::default();
    config.editor.indentation_guide = IndentationGuideMode::All;
    // 120 columns doubles as the rail-breakpoint check: the Steps rail
    // must show at this width (the old 130 breakpoint folded it away).
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, config, project_root.to_path_buf())
            .unwrap();
    harness
        .open_file(&project_root.join("src/main.rs"))
        .unwrap();
    harness.render().unwrap();

    let manifest = project_root.join("gap-tour.json");
    load_tour(
        &mut harness,
        &manifest.display().to_string(),
        "*Tour: Gap Tour*",
    );
    assert!(
        harness.screen_to_string().contains("─ Steps"),
        "the Steps rail must show at 120 columns\nScreen:\n{}",
        harness.screen_to_string()
    );

    harness
        .wait_until(|h| h.screen_to_string().contains("let a = 1;"))
        .unwrap();
    let (x, row_a) = harness.find_text_on_screen("fn outer()").unwrap();
    let blank_row = row_a + 2; // fn outer() / let a / blank
                               // The blank row's guide column (line-start column) must carry the
                               // band's bg like the rest of the row.
    harness
        .wait_until(|h| {
            h.get_cell_style(x, blank_row)
                .is_some_and(|s| s.bg == Some(ratatui::style::Color::Rgb(0, 25, 55)))
        })
        .unwrap();
}

/// The load prompt is the editor's native Open File browser in pick
/// mode (`editor.pickFile`): anchored at the workspace root — where
/// tour files live, not the active file's directory — with hidden
/// files visible so the dotfile manifests are listed, and Enter
/// delivering the picked manifest to the plugin instead of opening it
/// as a buffer.
#[test]
fn test_load_prompt_is_the_native_file_browser() {
    let (_temp, project_root) = setup_tour_project();
    let mut harness = harness_in(&project_root, 160, 40);

    run_command(&mut harness, "Tour: Load Definition");
    // Anchored at the workspace root: the manifests sitting beside
    // src/ are listed directly — including the hidden .fresh-tour.json,
    // which the config's dotfile default would filter out.
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("tour file path")
                && s.contains("storage-tour.json")
                && s.contains(".fresh-tour.json")
        })
        .unwrap();

    // Confirm the manifest; the pick loads the tour, it does not open
    // the JSON as a buffer.
    harness.type_text("storage-tour.json").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("*Tour: Storage Tour*"))
        .unwrap();
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("storage-tour.json ×"),
        "the picked manifest must not open as an editor tab\nScreen:\n{screen}"
    );
}

/// Enter on a typed path that names nothing on disk must *reject* the
/// confirm — the browser stays open with the input intact — not resolve
/// the pick with the bogus path. Before the fix the browser closed and
/// the tour plugin failed the read after the fact, visible only in the
/// log.
#[test]
fn test_pick_rejects_a_nonexistent_path_and_stays_open() {
    let (_temp, project_root) = setup_tour_project();
    let mut harness = harness_in(&project_root, 160, 40);

    run_command(&mut harness, "Tour: Load Definition");
    harness
        .wait_until(|h| h.screen_to_string().contains("tour file path"))
        .unwrap();

    let bogus = "no-such-tour.json";
    harness.type_text(bogus).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // The browser survives the confirm. This render alone cannot prove
    // it (the close might not have painted yet); the recovery flow below
    // is the real gate — on the pre-fix build the browser is gone, the
    // corrections land in the source buffer, and no tour ever opens.
    harness.render().unwrap();
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("tour file path") && screen.contains(bogus),
        "the pick browser must survive Enter on a nonexistent path\nScreen:\n{screen}"
    );
    assert!(
        !screen.contains("*Tour:"),
        "no tour may open from a rejected pick\nScreen:\n{screen}"
    );

    // The surviving prompt is still a working picker: fix the name and
    // the pick goes through.
    for _ in 0..bogus.chars().count() {
        harness
            .send_key(KeyCode::Backspace, KeyModifiers::NONE)
            .unwrap();
    }
    harness
        .type_text(&project_root.join("storage-tour.json").display().to_string())
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("*Tour: Storage Tour*"))
        .unwrap();
}

/// A manifest's relative `file_path`s resolve against the manifest's own
/// directory — a tour authored next to the code it walks keeps working no
/// matter where the editor was started — while a tour written against the
/// old cwd-relative meaning still resolves through the fallback.
#[test]
fn test_relative_step_paths_resolve_against_the_manifest() {
    let (_temp, project_root) = setup_tour_project();
    fs::create_dir(project_root.join("tours")).unwrap();
    // Step 1 is manifest-relative (`../src/...`); step 2 keeps the legacy
    // cwd-relative form. Both must land on a real file.
    fs::write(
        project_root.join("tours/relative-tour.json"),
        r###"{
  "title": "Relative Tour",
  "description": "Paths relative to the manifest",
  "schema_version": "1.0",
  "steps": [
    {
      "step_id": 1,
      "title": "Manifest-relative",
      "file_path": "../src/wide.rs",
      "lines": [1, 3],
      "explanation": "Resolved against the tour file's directory."
    },
    {
      "step_id": 2,
      "title": "Cwd-relative legacy",
      "file_path": "src/store.rs",
      "lines": [1, 2],
      "explanation": "Old tours resolved against the working directory."
    }
  ]
}"###,
    )
    .unwrap();
    let mut harness = harness_in(&project_root, 160, 40);

    let manifest = project_root.join("tours/relative-tour.json");
    // `load_tour` waits for the step highlight — which only paints once the
    // step's file resolved and opened, so this alone fails without the fix
    // (the step reported its file missing and nothing opened).
    load_tour(
        &mut harness,
        &manifest.display().to_string(),
        "*Tour: Relative Tour*",
    );
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("fn f1()"),
        "step 1's ../src/wide.rs must open via the manifest's directory\nScreen:\n{screen}"
    );
    assert!(
        !screen.contains("is not in this working tree"),
        "a resolvable relative path must not report missing\nScreen:\n{screen}"
    );

    press_step_key(&mut harness, 'n', "Step 2 of 2");
    assert!(
        harness.screen_to_string().contains("struct Store;"),
        "step 2's legacy cwd-relative path must still resolve\nScreen:\n{}",
        harness.screen_to_string()
    );
}

/// The reclaimed vertical layout: no divider row under the header, the
/// step actions live in the header row, and the step's location is the
/// first line of the prose.
#[test]
fn test_panel_layout_spends_its_rows_on_prose() {
    let (_temp, project_root) = setup_tour_project();
    let mut harness = harness_in(&project_root, 160, 40);

    let manifest = project_root.join(".fresh-tour.json");
    load_tour(
        &mut harness,
        &manifest.display().to_string(),
        "*Tour: Pipeline Tour*",
    );

    let header_row = row_of(&harness, "Step 1 of 2");
    // Jump / Re-highlight moved up into the header row.
    assert_eq!(
        row_of(&harness, "[ Jump to code ⏎ ]"),
        header_row,
        "step actions must share the header row\nScreen:\n{}",
        harness.screen_to_string()
    );
    assert_eq!(
        row_of(&harness, "[ Re-highlight ]"),
        header_row,
        "step actions must share the header row\nScreen:\n{}",
        harness.screen_to_string()
    );
    // No divider row: the body's section border sits directly under the
    // header.
    let section_row = row_of(&harness, "╭");
    assert_eq!(
        section_row,
        header_row + 1,
        "the body must start directly under the header\nScreen:\n{}",
        harness.screen_to_string()
    );
    // The location leads the prose, directly under the section border.
    assert_eq!(
        row_of(&harness, "lines 1–3"),
        section_row + 1,
        "the step location must be the first prose line\nScreen:\n{}",
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
    let main_rs = json_path(&root.join("src/main.rs"));
    let wide_rs = json_path(&root.join("src/wide.rs"));
    let store_rs = json_path(&root.join("src/store.rs"));
    let deleted_rs = json_path(&root.join("src/deleted.rs"));
    format!(
        r###"{{
  "title": "Request Pipeline",
  "description": "How a request reaches a handler",
  "schema_version": "1.0",
  "steps": [
    {{
      "step_id": 1,
      "title": "Entry point",
      "file_path": {main_rs},
      "lines": [1, 3],
      "explanation": "## Where it starts\n\nThe listener is **built** here using `TcpListener`.\n\n- binds the socket\n- spawns the accept loop\n\n```rust\nlet l = TcpListener::bind(addr)?;\n```"
    }},
    {{
      "step_id": 2,
      "title": "Dispatch",
      "file_path": {wide_rs},
      "lines": [5, 44],
      "explanation": "## Handling\n\nEach connection is dispatched to `handle` on its own task.\n\nThe highlighted range is *taller than the pane* — it stays painted as you scroll."
    }},
    {{
      "step_id": 3,
      "title": "Storage",
      "file_path": {store_rs},
      "lines": [1, 2],
      "explanation": "## Persisting\n\nHandlers write through `Store`."
    }},
    {{
      "step_id": 4,
      "title": "A step whose file moved",
      "file_path": {deleted_rs},
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

// ---------------------------------------------------------------------------
// VS Code CodeTour manifests load, auto-detected and converted
// ---------------------------------------------------------------------------

/// A CodeTour-format manifest, shaped after the CodeTour extension's own
/// `.tours/intro.tour` (microsoft/codetour): a `$schema` key, a top-level
/// `description`, an *untitled content-only first step* (prose, no file),
/// then file steps. Step paths are workspace-root-relative by definition —
/// deliberately not interpolated, since resolving them against the root the
/// `.tours/` directory sits in is part of what is under test. A `selection`
/// and a `pattern` anchor cover the two range shapes `intro.tour` doesn't.
fn codetour_json() -> String {
    // Four hashes: the markdown headings inside put `"###` in the body,
    // which would close an r### string mid-JSON.
    r####"{
  "$schema": "https://aka.ms/codetour-schema",
  "title": "CT Getting Started",
  "description": "Getting Started",
  "steps": [
    {
      "description": "### Welcome aboard\n\nA **content** step: prose only, no file."
    },
    {
      "title": "Entry point",
      "file": "src/main.rs",
      "selection": {
        "start": { "line": 1, "character": 1 },
        "end": { "line": 3, "character": 2 }
      },
      "description": "### The listener\n\nBound at startup."
    },
    {
      "file": "src/main.rs",
      "pattern": "fn handle",
      "description": "### The handler\n\nAnchored by regex, not a line number."
    }
  ]
}"####
        .to_string()
}

/// Project with the code-tour plugin and a CodeTour manifest in `.tours/`,
/// and nothing else discoverable — no root `.fresh-tour.json`.
fn setup_codetour_project() -> (tempfile::TempDir, PathBuf) {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();
    let project_root = fs::canonicalize(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "code-tour");

    fs::create_dir(project_root.join("src")).unwrap();
    fs::write(project_root.join("src/main.rs"), MAIN_RS).unwrap();
    fs::create_dir(project_root.join(".tours")).unwrap();
    fs::write(
        project_root.join(".tours/getting-started.tour"),
        codetour_json(),
    )
    .unwrap();

    (temp_dir, project_root)
}

/// Loading a `.tour` file converts it: the tour opens under its CodeTour
/// title, the untitled content-only first step renders its prose with no
/// file-missing warning, no highlight and no Jump button, the `selection`
/// step highlights its line range, and the `pattern` step lands on the line
/// the regex matches — all through the ordinary Load Definition flow, no
/// format switch anywhere. On the pre-CodeTour plugin this manifest is
/// rejected ("Unsupported tour schema version: undefined") and no panel
/// ever mounts.
#[test]
fn test_codetour_manifest_loads_and_converts() {
    let (_temp, project_root) = setup_codetour_project();
    let mut harness = harness_in(&project_root, 160, 48);

    let manifest = project_root.join(".tours/getting-started.tour");
    run_command(&mut harness, "Tour: Load Definition");
    harness
        .wait_until(|h| h.screen_to_string().contains("tour file path"))
        .unwrap();
    harness.type_text(&manifest.display().to_string()).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    // A content-only first step opens no file and paints no highlight, so
    // `wait_for_step_settled` would wait for a frame that never comes. Its
    // rendered end state is the prose in a focused panel.
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("*Tour: CT Getting Started*")
                && s.contains("Welcome aboard")
                && s.contains("[RO]")
                && s.contains("Text")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Step 1 of 3"),
        "expected the converted tour to keep all three steps\nScreen:\n{screen}"
    );
    // The step title came from the description's markdown heading — the
    // manifest itself has no `title` on this step.
    assert!(
        screen.contains("Welcome aboard"),
        "expected the heading-derived step title/prose\nScreen:\n{screen}"
    );
    // Content step: no file was named, so nothing may claim one is missing…
    assert!(
        !screen.contains("is not in this working tree"),
        "a content-only step must not warn about a missing file\nScreen:\n{screen}"
    );
    // …no Jump button is offered (the lowercase hint-bar entry remains)…
    assert!(
        !screen.contains("[ Jump"),
        "a content-only step has no code location to jump to\nScreen:\n{screen}"
    );
    // …and nothing is highlighted in the editor.
    assert!(
        highlighted_rows(&harness).is_empty(),
        "a content-only step must not paint a highlight"
    );

    // Step 2: `selection` start/end lines become the [1, 3] range.
    press_step_key(&mut harness, 'n', "Step 2 of 3");
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Entry point"),
        "expected the explicit step title\nScreen:\n{screen}"
    );
    assert!(
        screen.contains("lines 1–3"),
        "expected the selection converted to a 1–3 line range\nScreen:\n{screen}"
    );
    assert_eq!(
        highlighted_rows(&harness).len(),
        3,
        "expected exactly the selection's three lines highlighted\nScreen:\n{screen}"
    );

    // Step 3: the `pattern` resolves against file content, not a stored
    // line number — `fn handle` lives on line 5 of the fixture.
    press_step_key(&mut harness, 'n', "Step 3 of 3");
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("The handler"),
        "expected the heading-derived title of the pattern step\nScreen:\n{screen}"
    );
    assert!(
        screen.contains("lines 5–5"),
        "expected the pattern to resolve to line 5\nScreen:\n{screen}"
    );
    let rows = highlighted_rows(&harness);
    assert_eq!(
        rows.len(),
        1,
        "expected a single-line highlight\nScreen:\n{screen}"
    );
    let highlighted_line = screen.lines().nth(rows[0] as usize).unwrap_or_default();
    assert!(
        highlighted_line.contains("fn handle"),
        "expected the highlight on the line the pattern matches, got row {}: '{}'\nScreen:\n{screen}",
        rows[0],
        highlighted_line
    );
}

// ---------------------------------------------------------------------------
// Workspace tour command browses — it never opens a tour on its own
// ---------------------------------------------------------------------------

/// "Tour: Open Workspace Tour..." opens the file browser and lets the
/// user choose — it must NOT auto-open a tour it found on its own, even
/// when there is exactly one: an opinionated guess that opens the wrong
/// manifest is worse than a browser that starts where tour files live.
/// Fails on the auto-open behavior, where the lone `.tours/` manifest
/// opened immediately and no browser ever appeared.
#[test]
fn test_workspace_tour_command_browses_instead_of_auto_opening() {
    let (_temp, project_root) = setup_codetour_project();
    let mut harness = harness_in(&project_root, 160, 48);

    run_command(&mut harness, "Tour: Open Workspace Tour");
    // The pick browser, anchored at the workspace root with hidden files
    // visible — the `.tours/` directory is on screen, no tour is open.
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("tour file path") && s.contains(".tours")
        })
        .unwrap();
    assert!(
        !harness
            .screen_to_string()
            .contains("*Tour: CT Getting Started*"),
        "the command must not open a tour without the user picking one\nScreen:\n{}",
        harness.screen_to_string()
    );
}

// ---------------------------------------------------------------------------
// The Load Definition picker can actually reach tour files
// ---------------------------------------------------------------------------

/// Tour files are dotfiles at the workspace root (`.fresh-tour.json`,
/// `.tours/*.tour`), but the pick browser used to open at the *active
/// file's* directory with hidden files off — a picker in `src/` showing
/// `main.rs` and nothing else, with every tour file in the workspace
/// unreachable. The picker must anchor at the workspace root with hidden
/// files visible, and a name typed there must resolve root-relative.
/// Fails on the old anchoring: `.fresh-tour.json` never appears on
/// screen, and the typed name resolved against `src/` and was rejected.
#[test]
fn test_load_definition_picker_reaches_hidden_tour_files() {
    let (_temp, project_root) = setup_tour_project();
    let mut harness = harness_in(&project_root, 160, 48);

    run_command(&mut harness, "Tour: Load Definition");
    harness
        .wait_until(|h| h.screen_to_string().contains("tour file path"))
        .unwrap();
    // The browser must list the workspace root's hidden tour manifest —
    // the directory listing is async, so wait for the entry itself.
    harness
        .wait_until(|h| h.screen_to_string().contains(".fresh-tour.json"))
        .unwrap();

    // Selecting by name must work too: typed input resolves against the
    // browser's directory, which is now the workspace root. (No hint-bar
    // assertion here — at some pane heights the hint row sits below the
    // fold; the tab plus a settled step is the proof the pick landed.)
    harness.type_text(".fresh-tour.json").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("*Tour: Pipeline Tour*"))
        .unwrap();
    wait_for_step_settled(&mut harness);
}
