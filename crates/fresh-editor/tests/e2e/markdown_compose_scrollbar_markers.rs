//! End-to-end proof that the scrollbar-marker API (issue #2713) works from a
//! real plugin: the `markdown_compose` plugin marks headings on the scrollbar.
//!
//! These drive the editor the way a user does — open a file, toggle compose
//! mode from the command palette, scroll — and assert only on rendered cells
//! in the scrollbar column.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};

/// The half-block glyph a scrollbar marker paints.
const MARKER_GLYPH: &str = "▌";

/// A document with headings spread from top to bottom, and enough filler that
/// it scrolls well past one screen.
fn document_with_headings(sections: usize, filler_lines: usize) -> String {
    let mut md = String::new();
    for s in 0..sections {
        md.push_str(&format!("# Section {s}\n\n"));
        for l in 0..filler_lines {
            md.push_str(&format!("Body line {l} of section {s}.\n"));
        }
        md.push('\n');
    }
    md
}

fn marker_rows(harness: &EditorTestHarness) -> Vec<u16> {
    let col = harness.buffer().area.width - 1;
    let (first, last) = harness.content_area_rows();
    (first..=last)
        .filter(|row| harness.get_cell(col, *row as u16).as_deref() == Some(MARKER_GLYPH))
        .map(|r| r as u16)
        .collect()
}

/// Open a markdown document with the real `markdown_compose` plugin loaded and
/// compose mode enabled through the command palette.
fn compose_harness(md: &str) -> (EditorTestHarness, tempfile::TempDir) {
    init_tracing_from_env();

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project");
    std::fs::create_dir(&project_root).unwrap();
    let plugins_dir = project_root.join("plugins");
    std::fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "markdown_compose");
    copy_plugin_lib(&plugins_dir);

    let md_path = project_root.join("headings.md");
    std::fs::write(&md_path, md).unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        30,
        Default::default(),
        project_root.clone(),
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

/// The headline end-to-end case for issue #2713: with compose mode on, the
/// plugin's heading marks appear as colored cells on the scrollbar.
#[test]
fn markdown_compose_marks_headings_on_the_scrollbar() {
    let (mut harness, _tmp) = compose_harness(&document_with_headings(12, 12));

    // Wait for the plugin's lines_changed pass to publish its marks.
    harness
        .wait_until(|h| !marker_rows(h).is_empty())
        .expect("markdown_compose should mark headings on the scrollbar");

    let rows = marker_rows(&harness);
    assert!(
        rows.len() >= 2,
        "several headings are on screen, so several marks are expected; saw {rows:?}"
    );
}

/// Marks accumulate as the document is explored: scrolling to parts of the
/// file the plugin had not yet seen adds their headings without dropping the
/// marks for sections already visited.
///
/// This is the property that `setScrollbarMarkersInRange` exists for — a
/// whole-namespace replace on every `lines_changed` batch would leave only the
/// headings near the viewport marked.
#[test]
fn heading_marks_accumulate_as_the_document_is_explored() {
    let (mut harness, _tmp) = compose_harness(&document_with_headings(20, 14));

    harness
        .wait_until(|h| !marker_rows(h).is_empty())
        .expect("initial heading marks");
    let initial = marker_rows(&harness).len();

    // Page down through the document so later sections enter the viewport,
    // letting the plugin's marks settle after each page.
    for _ in 0..12 {
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::NONE)
            .unwrap();
        let mut prev = usize::MAX;
        harness
            .wait_until_stable(|h| {
                let n = marker_rows(h).len();
                let stable = n == prev;
                prev = n;
                stable
            })
            .unwrap();
    }

    // The plugin republishes only the region it just saw, so headings from
    // sections already scrolled past keep their marks and the total grows.
    // With a whole-namespace replace this count stays flat — only the
    // headings near the viewport would survive each batch.
    let after = marker_rows(&harness);
    assert!(
        after.len() > initial,
        "marks should accumulate while scrolling: started with {initial}, \
         ended with {} ({after:?})",
        after.len()
    );
}

/// Turning compose mode back off removes the plugin's marks from the
/// scrollbar, so nothing lingers after the feature is disabled.
#[test]
fn disabling_compose_mode_clears_the_heading_marks() {
    let (mut harness, _tmp) = compose_harness(&document_with_headings(12, 12));

    harness
        .wait_until(|h| !marker_rows(h).is_empty())
        .expect("initial heading marks");

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

    harness
        .wait_until(|h| marker_rows(h).is_empty())
        .expect("marks should be cleared when compose mode is turned off");
}
