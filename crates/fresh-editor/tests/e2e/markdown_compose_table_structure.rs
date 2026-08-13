//! End-to-end: compose mode draws a table's frame edges from the *editor's*
//! table classification rather than from the lines a `lines_changed` batch
//! happens to contain.
//!
//! "Is this line a table row" was never the hard part — that is decided from
//! the line's own text. Which *edge* of the frame a row carries is: the
//! `┌─┬─┐` belongs above the header and the `└─┴─┘` below the last row, and
//! both are facts about the row's neighbours. A batch holds only the lines a
//! scroll or an edit touched, so whenever the neighbour was absent the plugin
//! had to guess — and the guess showed, as a table whose top edge turned into
//! an inter-row separator and stayed that way.
//!
//! These suites need the **full grammar registry**: the classification comes
//! from the Markdown grammar's own `meta.table` scope, so with the default
//! empty registry there is no Markdown syntax and no classification at all
//! (and a `wait_until` on it would hang rather than fail).
//!
//! All assertions are on rendered output only.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness, HarnessOptions};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};

/// Open a markdown document with the real `markdown_compose` plugin loaded and
/// compose mode enabled through the command palette.
#[cfg(feature = "plugins")]
fn compose_harness(md: &str, width: u16, height: u16) -> (EditorTestHarness, tempfile::TempDir) {
    init_tracing_from_env();

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project");
    std::fs::create_dir(&project_root).unwrap();
    let plugins_dir = project_root.join("plugins");
    std::fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "markdown_compose");
    copy_plugin_lib(&plugins_dir);

    let md_path = project_root.join("table.md");
    std::fs::write(&md_path, md).unwrap();

    let mut harness = EditorTestHarness::create(
        width,
        height,
        HarnessOptions::new()
            .with_working_dir(project_root.clone())
            .without_empty_plugins_dir()
            .with_full_grammar_registry(),
    )
    .unwrap();

    harness.open_file(&md_path).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Toggle Compose").unwrap();
    harness.wait_for_screen_contains("Toggle Compose").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    (harness, temp_dir)
}

/// The screen row index of the line holding `needle`.
#[cfg(feature = "plugins")]
fn row_of(screen: &str, needle: &str) -> usize {
    screen
        .lines()
        .position(|l| l.contains(needle))
        .unwrap_or_else(|| panic!("{needle:?} not on screen.\nScreen:\n{screen}"))
}

/// Paging away from a table and back must not cost it its top edge.
///
/// This is the reported "scrolling glitches the table" bug. `lines_changed` is
/// edge-triggered on byte ranges the editor has not reported before, so the
/// batch that brings a table's header back into view can begin *at* that
/// header — the blank line above it, the plugin's only evidence that this row
/// starts the table, having never left the screen and so never re-fired. The
/// batch-local rule then read the row as mid-table and replaced its `┌─┬─┐`
/// with an inter-row `├─┼─┤`, and it stayed that way: nothing re-fires that
/// line again until it is edited or scrolled away and back once more.
///
/// With the classification on the payload the row says "header" for itself, so
/// the batch's contents stop mattering.
#[cfg(feature = "plugins")]
#[test]
fn table_top_border_survives_paging_away_and_back() {
    let mut md = String::from("# Doc\n\nintro paragraph\n\n| Key | Value |\n|-----|-------|\n");
    for i in 0..40 {
        md.push_str(&format!("| k{i:02} | v{i:02} |\n"));
    }
    md.push_str("\nafter\n");

    let (mut harness, _tmp) = compose_harness(&md, 100, 30);
    harness
        .wait_until_stable(|h| h.screen_to_string().contains('┌'))
        .unwrap();

    // Sanity: the frame opens with a top border before any scrolling.
    let before = harness.screen_to_string();
    let header_row = row_of(&before, "Key");
    assert!(
        before.lines().nth(header_row - 1).unwrap().contains('┌'),
        "expected a top border above the header before scrolling.\nScreen:\n{before}"
    );

    for _ in 0..3 {
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();
    }

    for _ in 0..3 {
        harness
            .send_key(KeyCode::PageUp, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();
    }
    harness
        .wait_until_stable(|h| h.screen_to_string().contains("Key"))
        .unwrap();

    let after = harness.screen_to_string();
    let header_row = row_of(&after, "Key");
    let above = after.lines().nth(header_row - 1).unwrap();
    assert!(
        above.contains('┌') && !above.contains('├'),
        "paging away from the table and back replaced its top border with an \
         inter-row separator: the row above the header is {above:?}.\nScreen:\n{after}"
    );
}

/// The same property at the other edge: editing the last row must not cost the
/// table its bottom border, which needs the (absent) line *below* it.
#[cfg(feature = "plugins")]
#[test]
fn table_bottom_border_survives_an_edit_in_its_last_row() {
    let md = "\
intro paragraph

| Key | Value |
|-----|-------|
| a | b |
| c | d |

after
";
    let (mut harness, _tmp) = compose_harness(md, 100, 30);
    harness
        .wait_until_stable(|h| h.screen_to_string().contains('└'))
        .unwrap();

    // Line 5 (0-based) is the last data row; column 3 is just after its `c`.
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    for _ in 0..5 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    for _ in 0..3 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.type_text("z").unwrap();
    harness
        .wait_until_stable(|h| h.screen_to_string().contains("cz"))
        .unwrap();

    let after = harness.screen_to_string();
    let last_row = row_of(&after, "cz");
    let below = after.lines().nth(last_row + 1).unwrap();
    assert!(
        below.contains('└'),
        "editing inside the last row left the table unclosed: the row below it \
         is {below:?}.\nScreen:\n{after}"
    );
}
