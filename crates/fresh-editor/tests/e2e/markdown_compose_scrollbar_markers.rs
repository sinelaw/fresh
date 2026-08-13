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

/// The track describes the whole document from the moment compose mode is on
/// — the headline fix for issue #2990.
///
/// Marks used to be published viewport by viewport out of `lines_changed`, so
/// the track only showed the parts already scrolled through. A structure map
/// that needs the reader to explore the document before it describes it is not
/// a structure map.
#[test]
fn every_heading_is_marked_without_scrolling() {
    // 30 sections of 20 lines: the opening viewport holds barely one of them.
    let (mut harness, _tmp) = compose_harness(&document_with_headings(30, 20));

    // The pre-scan reads the buffer over the async plugin bridge, so the full
    // set lands a few ticks after the toggle returns.
    harness
        .wait_until(|h| marker_rows(h).len() >= 10)
        .expect("compose mode should mark headings across the whole document");

    let rows = marker_rows(&harness);
    let (first, last) = harness.content_area_rows();
    let (first, last) = (first as u16, last as u16);
    let track = (last - first + 1) as f32;

    // Coverage reaches content that has never been on screen: the last
    // section's heading is at the end of the file, so its mark belongs near
    // the bottom of the track.
    let deepest = (*rows.last().unwrap() - first) as f32 / track;
    assert!(
        deepest > 0.8,
        "deepest mark should sit near the end of the track, was at \
         {deepest:.2} of it; rows {rows:?}"
    );
}

/// Only top-level headings are marked by default: a track cell covers many
/// lines, so marking every level packs several headings into one cell where
/// only the shallowest survives anyway.
#[test]
fn only_top_level_headings_are_marked_by_default() {
    let mut md = String::from("# The one top level heading\n\n");
    for s in 0..30 {
        md.push_str(&format!("## Sub {s}\n\n### Deeper {s}\n\n"));
        for l in 0..20 {
            md.push_str(&format!("Body line {l} of section {s}.\n"));
        }
    }

    let (mut harness, _tmp) = compose_harness(&md);

    harness
        .wait_until(|h| !marker_rows(h).is_empty())
        .expect("the `#` heading should be marked");
    // Give any deeper-level marks the same chance to appear before asserting
    // that they don't.
    let mut settle = usize::MAX;
    harness
        .wait_until_stable(|h| {
            let n = marker_rows(h).len();
            let stable = n == settle;
            settle = n;
            stable
        })
        .unwrap();

    let (first, _) = harness.content_area_rows();
    assert_eq!(
        marker_rows(&harness),
        vec![first as u16],
        "only the single `#` heading should be marked, at the top of the track"
    );
}

/// Exploring the document does not disturb the marks: scrolling neither drops
/// the headings outside the viewport nor adds to a set that is already
/// complete.
///
/// This is the property that `setScrollbarMarkersInRange` exists for — a
/// whole-namespace replace on every `lines_changed` batch would leave only the
/// headings near the viewport marked.
#[test]
fn heading_marks_survive_exploring_the_document() {
    let (mut harness, _tmp) = compose_harness(&document_with_headings(20, 14));

    harness
        .wait_until(|h| !marker_rows(h).is_empty())
        .expect("initial heading marks");
    let initial = marker_rows(&harness);

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

    // Each batch republishes only its own byte span, so the marks for the
    // sections scrolled past are left alone. With a whole-namespace replace
    // per batch, only the headings near the viewport would survive.
    let after = marker_rows(&harness);
    assert_eq!(
        after, initial,
        "the marked rows should be identical before and after scrolling"
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
