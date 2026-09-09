//! End-to-end coverage for the `markdown_toc` plugin — the Markdown table of
//! contents mounted as a sidebar section under the file explorer
//! (docs/internal/sidebar-sections-design.md §5).
//!
//! The tests drive the editor the way a user does — open a file, show the
//! sidebar, move the cursor, click a row — and assert on rendered cells in
//! the sidebar column and on the buffer cursor. They cover the plugin's two
//! pure functions through their observable effect: the fence-aware heading
//! scan (a `# not a heading` line inside a fenced block must not become a
//! row; two headings with the same title must both appear) and the
//! "last heading at or before a byte" search (the selected row follows
//! `Ctrl+End` and go-to-line).
//!
//! Both Markdown modes are covered: source, and compose/preview toggled on
//! through the `markdown_compose` plugin's own command — the rows and the
//! selection behaviour must be identical, because every position the TOC
//! holds is a source byte offset and compose does not move bytes.
//!
//! What the host paints and emits, which the assertions are written to:
//! the section's header is the shared border row `├ ▼ Contents ───×─┤`; the
//! selected row of a `Tree` in a sidebar section wears the explorer's `▌` in
//! its first content column over the selection band (§5.1); a press
//! anywhere on a row's text is that row's `select`, tagged `via: "click"`,
//! while Up/Down fire `select` untagged — so a click jumps and the arrows
//! browse without moving the cursor until Enter (§5.6).

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness, HarnessOptions};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};

/// The selection marker the sidebar trees paint in their first content
/// column (the explorer's `▌`, which the Contents section shares — §5.1).
const SELECTION_GLYPH: &str = "▌";

/// A document with nested headings, a fenced block whose first line looks
/// like a heading, and a duplicated title (`Install` under both `Setup` and
/// `Usage`), so two rows share a title and none share a byte offset.
fn document() -> String {
    [
        "# Guide",
        "",
        "Intro paragraph.",
        "",
        "## Setup",
        "",
        "Setup text line.",
        "",
        "### Install",
        "",
        "Install text.",
        "",
        "```bash",
        "# not a heading",
        "echo hi",
        "```",
        "",
        "### Configure",
        "",
        "Configure text.",
        "",
        "## Usage",
        "",
        "Usage text.",
        "",
        "### Install",
        "",
        "More usage.",
        "",
        "# Appendix",
        "",
        "Final line.",
        "",
    ]
    .join("\n")
}

/// Titles the Contents section must list, in document order, without `#`.
const EXPECTED_ROWS: [&str; 7] = [
    "Guide",
    "Setup",
    "Install",
    "Configure",
    "Usage",
    "Install",
    "Appendix",
];

/// 1-based source line of the first line equal to `line`.
fn line_number_of(md: &str, line: &str) -> usize {
    md.lines()
        .position(|l| l == line)
        .map(|i| i + 1)
        .unwrap_or_else(|| panic!("{line:?} not in document"))
}

/// Byte offset of the first line equal to `line`.
fn byte_offset_of(md: &str, line: &str) -> usize {
    let mut offset = 0;
    for l in md.split('\n') {
        if l == line {
            return offset;
        }
        offset += l.len() + 1;
    }
    panic!("{line:?} not in document");
}

/// Open the document with the `markdown_toc` and `markdown_compose` plugins
/// loaded, the sidebar shown and the Contents section mounted.
fn toc_harness(md: &str) -> (EditorTestHarness, std::path::PathBuf, tempfile::TempDir) {
    init_tracing_from_env();

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project");
    std::fs::create_dir(&project_root).unwrap();
    let plugins_dir = project_root.join("plugins");
    std::fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "markdown_toc");
    copy_plugin(&plugins_dir, "markdown_compose");
    // The source-mode plugin binds Enter / Tab / Shift+Tab for the buffer.
    // Loaded on purpose: a focused section's Enter must reach the row, not
    // the buffer's list-continuation handler (found by driving, not here).
    copy_plugin(&plugins_dir, "markdown_source");
    copy_plugin_lib(&plugins_dir);

    let md_path = project_root.join("guide.md");
    std::fs::write(&md_path, md).unwrap();

    let mut harness = EditorTestHarness::create(
        120,
        40,
        HarnessOptions::new()
            .with_working_dir(project_root.clone())
            .without_empty_plugins_dir(),
    )
    .unwrap();

    harness.open_file(&md_path).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("guide.md");

    // The section lives in the sidebar column, so the column has to be
    // showing. Ctrl+E toggles it; only press it when the explorer is hidden.
    // Showing it gives it the keyboard, and the cursor moves below are the
    // buffer's, so Esc hands the keyboard back: the explorer's caret (its
    // `▌`, painted only while it is focused) going away is the proof.
    if !harness.screen_to_string().contains("File Explorer") {
        harness
            .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
            .unwrap();
        harness.wait_for_file_explorer().unwrap();
        harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
        harness
            .wait_until(|h| !explorer_has_caret(h))
            .expect("Esc returns the keyboard from the explorer to the editor");
    }

    // The plugin mounts on `after_file_open` / `buffer_activated` and scans
    // asynchronously; the header row appearing is the mount, the last title
    // appearing is the scan having landed.
    harness.wait_for_screen_contains("Contents").unwrap();
    harness
        .wait_until(|h| toc_rows(h).len() == EXPECTED_ROWS.len())
        .expect("the Contents section should list every heading");

    (harness, md_path, temp_dir)
}

/// Whether the file explorer is painting its focus caret: a `▌` in the
/// first content column of a row between its title and the Contents header.
fn explorer_has_caret(harness: &EditorTestHarness) -> bool {
    let screen = harness.screen_to_string();
    let mut in_explorer = false;
    for line in screen.lines() {
        if line.contains("File Explorer") {
            in_explorer = true;
            continue;
        }
        if line.contains("Contents") || line.starts_with('└') {
            break;
        }
        if in_explorer && line.chars().nth(1) == Some('▌') {
            return true;
        }
    }
    false
}

/// Width of the sidebar column in cells: the position of the explorer's
/// top-right corner on the row carrying its title.
fn sidebar_width(harness: &EditorTestHarness) -> usize {
    let screen = harness.screen_to_string();
    let title_row = screen
        .lines()
        .find(|l| l.contains("File Explorer"))
        .expect("the explorer's title row is on screen");
    title_row
        .char_indices()
        .find(|(_, c)| *c == '┐')
        .map(|(i, _)| title_row[..i].chars().count() + 1)
        .expect("the explorer's title row ends in its top-right corner")
}

/// The Contents section's body rows: the sidebar cells of every screen row
/// below the `Contents` header and above the column's bottom border, with
/// the borders stripped. Each entry is `(screen_row, text)`.
fn toc_body_rows(harness: &EditorTestHarness) -> Vec<(u16, String)> {
    let width = sidebar_width(harness);
    let screen = harness.screen_to_string();
    let mut rows = Vec::new();
    let mut in_body = false;
    for (row, line) in screen.lines().enumerate() {
        let cells: String = line.chars().take(width).collect();
        if !in_body {
            in_body = cells.contains("Contents");
            continue;
        }
        if cells.starts_with('└') || cells.starts_with('├') {
            break;
        }
        // Strip the left and right border cells.
        let inner: String = cells
            .chars()
            .skip(1)
            .take(width.saturating_sub(2))
            .collect();
        rows.push((row as u16, inner));
    }
    rows
}

/// The non-empty Contents rows, in order, as rendered.
fn toc_rows(harness: &EditorTestHarness) -> Vec<String> {
    toc_body_rows(harness)
        .into_iter()
        .map(|(_, text)| text.trim_end().to_string())
        .filter(|text| !text.trim().is_empty())
        .collect()
}

/// The title of the row currently wearing the selection marker, if any.
fn selected_toc_row(harness: &EditorTestHarness) -> Option<String> {
    toc_body_rows(harness)
        .into_iter()
        .find(|(_, text)| text.starts_with(SELECTION_GLYPH))
        .map(|(_, text)| text.trim().to_string())
}

/// Screen row of the `index`-th Contents row (document order).
fn toc_row_screen_row(harness: &EditorTestHarness, index: usize) -> u16 {
    toc_body_rows(harness)
        .into_iter()
        .filter(|(_, text)| !text.trim().is_empty())
        .nth(index)
        .map(|(row, _)| row)
        .expect("the Contents row is on screen")
}

fn goto_line(harness: &mut EditorTestHarness, line: usize) {
    harness
        .send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text(&line.to_string()).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();
}

fn wait_for_selected(harness: &mut EditorTestHarness, title: &str) {
    let want = title.to_string();
    harness
        .wait_until(|h| {
            selected_toc_row(h)
                .map(|row| {
                    row.trim_start_matches(SELECTION_GLYPH)
                        .trim()
                        .ends_with(want.as_str())
                })
                .unwrap_or(false)
        })
        .unwrap_or_else(|e| {
            panic!(
                "the Contents selection should land on {title:?}: {e}\n{}",
                harness.screen_to_string()
            )
        });
}

/// Every row's text must end with the expected title, in document order;
/// the `#` markers and the fenced `# not a heading` line must not appear.
fn assert_rows(harness: &EditorTestHarness) {
    let rows = toc_rows(harness);
    assert_eq!(
        rows.len(),
        EXPECTED_ROWS.len(),
        "one row per heading; saw {rows:?}\n{}",
        harness.screen_to_string()
    );
    for (row, want) in rows.iter().zip(EXPECTED_ROWS.iter()) {
        let text = row.trim_start_matches(SELECTION_GLYPH).trim();
        assert!(
            text.ends_with(want),
            "row {row:?} should read {want:?} (without its # markers)"
        );
        assert!(
            !text.contains('#'),
            "row {row:?} must not show the # markers"
        );
    }
    assert!(
        !rows.iter().any(|r| r.contains("not a heading")),
        "a # line inside a fenced block is not a heading; saw {rows:?}"
    );
    // Depth is one column per level: an H2's title starts one cell right of
    // an H1's. Measured from the title, not from leading blanks, because
    // the selection mark replaces the row's first cell whatever was there —
    // a depth-0 row's disclosure glyph included, as the explorer's caret
    // does on its root row.
    let indent = |row: &str, title: &str| {
        row.char_indices()
            .find(|(i, _)| row[*i..].starts_with(title))
            .map(|(i, _)| row[..i].chars().count())
            .unwrap_or_else(|| panic!("{title:?} not in row {row:?}"))
    };
    let (guide, setup, install) = (
        indent(&rows[0], "Guide"),
        indent(&rows[1], "Setup"),
        indent(&rows[2], "Install"),
    );
    assert!(
        guide < setup && setup < install,
        "rows indent by level; saw {rows:?}"
    );
}

/// Move the cursor with `Ctrl+End` and go-to-line and check the selected row
/// follows it to the last heading at or before the cursor.
fn assert_selection_follows_cursor(harness: &mut EditorTestHarness, md: &str) {
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    wait_for_selected(harness, "Guide");

    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    wait_for_selected(harness, "Appendix");

    goto_line(harness, line_number_of(md, "## Usage"));
    wait_for_selected(harness, "Usage");

    // A line *inside* a section — not on its heading — still selects the
    // heading whose span contains it.
    goto_line(harness, line_number_of(md, "Configure text."));
    wait_for_selected(harness, "Configure");

    // The second `Install` is a different row from the first: keys are
    // byte offsets, not titles.
    goto_line(harness, line_number_of(md, "More usage."));
    let selected_index = toc_body_rows(harness)
        .into_iter()
        .filter(|(_, text)| !text.trim().is_empty())
        .position(|(_, text)| text.starts_with(SELECTION_GLYPH));
    assert_eq!(
        selected_index,
        Some(5),
        "the Install under Usage is the sixth row\n{}",
        harness.screen_to_string()
    );
}

/// Click a row and check the buffer cursor lands on that heading; then
/// browse with the keyboard the click left in the section (§5.6): Down
/// moves the selected row and not the cursor, Enter jumps.
fn assert_click_jumps(harness: &mut EditorTestHarness, md: &str) {
    let setup_row = toc_row_screen_row(harness, 1);
    harness.mouse_click(3, setup_row).unwrap();
    let want = byte_offset_of(md, "## Setup");
    harness
        .wait_until(|h| h.cursor_position() == want)
        .unwrap_or_else(|e| {
            panic!(
                "clicking the Setup row should put the cursor on its heading (byte {want}): {e}\n{}",
                harness.screen_to_string()
            )
        });
    // The jump centred the pane, which is a viewport change; the row the
    // reader chose must not be traded for the heading at the viewport top.
    wait_for_selected(harness, "Setup");

    // The click kept the keyboard in the section: Down selects the next
    // row (the first Install) and leaves the cursor on Setup.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    wait_for_selected(harness, "Install");
    harness.render().unwrap();
    assert_eq!(
        harness.cursor_position(),
        want,
        "an arrow key browses the outline without moving the cursor\n{}",
        harness.screen_to_string()
    );

    // Enter is the jump: the cursor lands on the first Install.
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    let install = byte_offset_of(md, "### Install");
    harness
        .wait_until(|h| h.cursor_position() == install)
        .unwrap_or_else(|e| {
            panic!(
                "Enter on the Install row should put the cursor on it (byte {install}): {e}\n{}",
                harness.screen_to_string()
            )
        });

    // The duplicated title resolves by offset: the sixth row is the
    // `Install` under `Usage`, not the one under `Setup`.
    let second_install_row = toc_row_screen_row(harness, 5);
    harness.mouse_click(3, second_install_row).unwrap();
    let first = byte_offset_of(md, "### Install");
    let second = md[first + 1..]
        .find("### Install")
        .map(|i| first + 1 + i)
        .expect("two Install headings");
    harness
        .wait_until(|h| h.cursor_position() == second)
        .unwrap_or_else(|e| {
            panic!(
                "clicking the second Install row should jump to byte {second}: {e}\n{}",
                harness.screen_to_string()
            )
        });
}

#[test]
fn markdown_toc_lists_headings_and_follows_the_cursor_in_source_mode() {
    let md = document();
    let (mut harness, _path, _tmp) = toc_harness(&md);

    assert_rows(&harness);
    assert_selection_follows_cursor(&mut harness, &md);
    assert_click_jumps(&mut harness, &md);
}

#[test]
fn markdown_toc_lists_headings_and_follows_the_cursor_in_compose_mode() {
    let md = document();
    let (mut harness, _path, _tmp) = toc_harness(&md);

    // Toggle compose on through the compose plugin's own command. Move the
    // cursor off the first heading first: the cursor line reveals its
    // markers, and the wait below is for them to be concealed.
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.run_palette_command("Toggle Compose").unwrap();
    harness.wait_for_prompt_closed().unwrap();
    harness
        .wait_until_stable(|h| !h.screen_to_string().contains("## Setup"))
        .expect("compose mode conceals the # markers in the pane");

    // The rows are the same: compose conceals markers and re-lays lines but
    // does not move a byte, and the TOC rescans once on the mode flip.
    harness
        .wait_until(|h| toc_rows(h).len() == EXPECTED_ROWS.len())
        .expect("the Contents section survives the compose toggle");
    assert_rows(&harness);
    assert_selection_follows_cursor(&mut harness, &md);
    assert_click_jumps(&mut harness, &md);
}
