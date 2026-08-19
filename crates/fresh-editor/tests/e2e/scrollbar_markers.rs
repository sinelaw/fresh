//! End-to-end coverage for the plugin scrollbar-marker API (issue #2713).
//!
//! Everything here asserts on rendered cells in the scrollbar column — the
//! marker glyph and its colour — never on the marker store itself. The
//! projection math is unit-tested in `view::scrollbar_marker`; these tests
//! prove the pixels reach the screen and that they stay correct as the user
//! edits and scrolls.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use fresh_core::api::{OverlayColorSpec, PluginCommand, ScrollbarMarker};
use fresh_core::BufferId;
use ratatui::style::Color;

/// The half-block glyph a scrollbar marker paints.
const MARKER_GLYPH: &str = "▌";

fn long_content(num_lines: usize) -> String {
    (0..num_lines)
        .map(|i| format!("line {i:04}: lorem ipsum dolor sit amet"))
        .collect::<Vec<_>>()
        .join("\n")
}

fn marker(position: u32, color: OverlayColorSpec) -> ScrollbarMarker {
    ScrollbarMarker {
        position: Some(position),
        line: None,
        end: None,
        end_line: None,
        color,
        priority: None,
    }
}

/// Rows of the scrollbar column that currently show a marker glyph, paired
/// with the foreground colour they were painted in.
fn marker_rows(harness: &EditorTestHarness) -> Vec<(u16, Option<Color>)> {
    let col = harness.buffer().area.width - 1;
    let (first, last) = harness.content_area_rows();
    (first..=last)
        .filter_map(|row| {
            let row = row as u16;
            let ch = harness.get_cell(col, row)?;
            if ch == MARKER_GLYPH {
                Some((row, harness.get_cell_style(col, row).and_then(|s| s.fg)))
            } else {
                None
            }
        })
        .collect()
}

fn set_markers(harness: &mut EditorTestHarness, markers: Vec<ScrollbarMarker>) {
    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::SetScrollbarMarkers {
            buffer_id: BufferId(1),
            namespace: "test".into(),
            markers,
        })
        .expect("plugin command accepted");
}

/// The headline behaviour of the issue: a plugin marks positions and they
/// appear as colored cells on the scrollbar, at proportional positions.
#[test]
fn plugin_markers_paint_on_the_scrollbar_at_proportional_positions() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let text = long_content(400);
    harness.load_buffer_from_text(&text).unwrap();
    harness.render().unwrap();

    assert!(
        marker_rows(&harness).is_empty(),
        "no markers set yet, so the scrollbar must be unmarked"
    );

    // Three marks: at the very start, the middle, and near the end of the file.
    let len = text.len() as u32;
    set_markers(
        &mut harness,
        vec![
            marker(0, OverlayColorSpec::Rgb(0, 255, 0)),
            marker(len / 2, OverlayColorSpec::Rgb(0, 255, 0)),
            marker(len - 1, OverlayColorSpec::Rgb(0, 255, 0)),
        ],
    );
    harness.render().unwrap();

    let rows = marker_rows(&harness);
    assert_eq!(
        rows.len(),
        3,
        "expected three marked rows on the scrollbar, saw {rows:?}"
    );
    for (_, fg) in &rows {
        assert_eq!(*fg, Some(Color::Rgb(0, 255, 0)), "marker colour applied");
    }

    // Proportional: one near the top, one near the middle, one near the
    // bottom of the track.
    let (first, last) = harness.content_area_rows();
    let track_height = (last - first + 1) as f64;
    let positions: Vec<f64> = rows
        .iter()
        .map(|(r, _)| (*r as f64 - first as f64) / track_height)
        .collect();
    assert!(
        positions[0] < 0.15,
        "first marker near the top: {positions:?}"
    );
    assert!(
        (0.35..0.65).contains(&positions[1]),
        "second marker near the middle: {positions:?}"
    );
    assert!(
        positions[2] > 0.85,
        "third marker near the bottom: {positions:?}"
    );
}

/// Colors given as theme keys resolve against the live theme, so markers can
/// match the line highlight a plugin already draws.
#[test]
fn theme_key_colors_resolve_against_the_theme() {
    let config = Config {
        theme: "high-contrast".into(),
        ..Default::default()
    };
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    let expected = harness.editor().theme().resolve_theme_key("syntax.keyword");
    assert!(
        expected.is_some(),
        "test needs a theme key the theme actually defines"
    );

    harness.load_buffer_from_text(&long_content(400)).unwrap();
    set_markers(
        &mut harness,
        vec![marker(
            0,
            OverlayColorSpec::ThemeKey("syntax.keyword".into()),
        )],
    );
    harness.render().unwrap();

    let rows = marker_rows(&harness);
    assert_eq!(rows.len(), 1, "one marker expected, saw {rows:?}");
    assert_eq!(
        rows[0].1, expected,
        "theme key should resolve at paint time"
    );
}

/// Markers are byte-anchored in the editor's marker tree, so inserting text
/// above one moves its mark down the track without the plugin republishing.
/// This is what keeps a plugin's marks correct between refreshes.
#[test]
fn markers_follow_their_content_through_edits() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.load_buffer_from_text(&long_content(400)).unwrap();

    // Mark a spot a little way into the file.
    set_markers(
        &mut harness,
        vec![marker(200, OverlayColorSpec::Rgb(255, 0, 0))],
    );
    harness.render().unwrap();
    // Filtered by colour: the edit below also puts the editor's own
    // unsaved-change marks on the track, and this test is about the plugin's.
    let plugin_rows = |h: &EditorTestHarness| -> Vec<u16> {
        marker_rows(h)
            .into_iter()
            .filter(|(_, fg)| *fg == Some(Color::Rgb(255, 0, 0)))
            .map(|(row, _)| row)
            .collect()
    };
    let before = plugin_rows(&harness);
    assert_eq!(before.len(), 1, "one marker expected, saw {before:?}");

    // Type a large block of new text at the very top of the buffer. The
    // marked content is now much further down the file, so its mark must
    // move down the track — with no further plugin commands.
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    let inserted = long_content(400);
    harness.type_text(&format!("{inserted}\n")).unwrap();
    harness.render().unwrap();

    let after = plugin_rows(&harness);
    assert_eq!(
        after.len(),
        1,
        "marker should survive the edit, saw {after:?}"
    );
    assert!(
        after[0] > before[0],
        "marker should move down the track after inserting above it: {} -> {}",
        before[0],
        after[0]
    );
}

/// Clearing the namespace removes the marks from the screen.
#[test]
fn clearing_a_namespace_removes_the_marks() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.load_buffer_from_text(&long_content(400)).unwrap();
    set_markers(
        &mut harness,
        vec![marker(100, OverlayColorSpec::Rgb(255, 0, 0))],
    );
    harness.render().unwrap();
    assert_eq!(marker_rows(&harness).len(), 1);

    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::ClearScrollbarMarkers {
            buffer_id: BufferId(1),
            namespace: "test".into(),
        })
        .expect("plugin command accepted");
    harness.render().unwrap();
    assert!(
        marker_rows(&harness).is_empty(),
        "cleared markers must disappear from the scrollbar"
    );
}

/// A range marker paints a proportional streak rather than a single cell, so
/// a multi-line region (a diff hunk, a section) reads as a band.
#[test]
fn range_markers_paint_a_streak() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let text = long_content(400);
    let len = text.len() as u32;
    harness.load_buffer_from_text(&text).unwrap();

    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::SetScrollbarMarkers {
            buffer_id: BufferId(1),
            namespace: "test".into(),
            markers: vec![ScrollbarMarker {
                position: Some(0),
                line: None,
                end_line: None,
                end: Some(len / 2),
                color: OverlayColorSpec::Rgb(0, 0, 255),
                priority: None,
            }],
        })
        .expect("plugin command accepted");
    harness.render().unwrap();

    let rows = marker_rows(&harness);
    assert!(
        rows.len() > 5,
        "a half-file range should mark many rows, saw {} ({rows:?})",
        rows.len()
    );
}

/// Range-scoped publishing leaves markers outside the region alone — the
/// property that lets a viewport-driven plugin accumulate coverage as the
/// user scrolls instead of losing the parts of the file it already scanned.
#[test]
fn range_scoped_publish_preserves_markers_outside_the_range() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let text = long_content(400);
    let len = text.len() as u32;
    harness.load_buffer_from_text(&text).unwrap();

    // Marks near the start and near the end.
    set_markers(
        &mut harness,
        vec![
            marker(0, OverlayColorSpec::Rgb(255, 0, 0)),
            marker(len - 1, OverlayColorSpec::Rgb(255, 0, 0)),
        ],
    );
    harness.render().unwrap();
    assert_eq!(marker_rows(&harness).len(), 2);

    // Republish only the middle of the file, as a viewport-driven plugin
    // would when that region scrolls into view.
    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::SetScrollbarMarkersInRange {
            buffer_id: BufferId(1),
            namespace: "test".into(),
            start: (len / 3) as usize,
            end: (2 * len / 3) as usize,
            markers: vec![marker(len / 2, OverlayColorSpec::Rgb(255, 0, 0))],
        })
        .expect("plugin command accepted");
    harness.render().unwrap();

    let rows = marker_rows(&harness);
    assert_eq!(
        rows.len(),
        3,
        "the two out-of-range marks must survive alongside the new one, saw {rows:?}"
    );
}

// =============================================================================
// Unsaved changes
// =============================================================================

/// The colour the editor paints its own unsaved-change marks in — the same
/// cornflower blue as the gutter bar for the same change.
const UNSAVED_BLUE: Color = Color::Rgb(100, 149, 237);

/// The gutter can only speak for the lines on screen. Typing near the top of a
/// long file and scrolling away leaves no trace in the gutter — the scrollbar
/// mark is the only thing that still says "there is an unsaved edit up there".
#[test]
fn unsaved_edits_are_marked_on_the_scrollbar_even_when_scrolled_away() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.load_buffer_from_text(&long_content(400)).unwrap();
    harness.render().unwrap();

    assert!(
        marker_rows(&harness).is_empty(),
        "an unmodified buffer must leave the track clean"
    );

    harness.type_text("edited near the top\n").unwrap();
    harness.render().unwrap();

    let rows = marker_rows(&harness);
    assert_eq!(
        rows.iter().map(|(_, fg)| *fg).collect::<Vec<_>>(),
        vec![Some(UNSAVED_BLUE)],
        "the edit should put one blue mark on the track, saw {rows:?}"
    );
    let (first, last) = harness.content_area_rows();
    let position = (rows[0].0 as f64 - first as f64) / (last - first + 1) as f64;
    assert!(
        position < 0.15,
        "the edit is near the top of the file, so its mark is near the top of \
         the track: {position}"
    );

    // Scroll until the edit is off screen. The mark must survive: that is the
    // whole point — the gutter bar for it is gone from view.
    for _ in 0..6 {
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    assert!(
        !harness.screen_to_string().contains("edited near the top"),
        "the test needs the edited line scrolled out of view"
    );
    let rows = marker_rows(&harness);
    assert_eq!(
        rows.iter().map(|(_, fg)| *fg).collect::<Vec<_>>(),
        vec![Some(UNSAVED_BLUE)],
        "the mark must survive scrolling the edit off screen, saw {rows:?}"
    );
}

/// Saving clears the marks: they track the diff against what is on disk, so
/// once the file matches, nothing is left over.
#[test]
fn saving_clears_the_unsaved_change_marks() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let fixture = harness.load_buffer_from_text(&long_content(400)).unwrap();

    harness.type_text("an unsaved edit\n").unwrap();
    harness.render().unwrap();
    assert!(
        !marker_rows(&harness).is_empty(),
        "the edit should be marked before saving"
    );

    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| marker_rows(h).is_empty())
        .expect("saving should clear the unsaved-change marks");

    drop(fixture);
}

/// Plugin markers and the editor's own marks share one track: a git hunk
/// (priority 10) keeps its colour on a cell the unsaved-change mark (5) also
/// wants, and the unsaved mark still shows everywhere else.
#[test]
fn plugin_markers_and_unsaved_marks_coexist() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let text = long_content(400);
    harness.load_buffer_from_text(&text).unwrap();

    // Edit at the top, so the unsaved mark lands on the track's first row.
    harness.type_text("edited\n").unwrap();
    harness.render().unwrap();

    // A plugin marks the same top-of-file position, plus a spot near the end.
    let len = text.len() as u32;
    set_markers(
        &mut harness,
        vec![
            ScrollbarMarker {
                position: Some(0),
                line: None,
                end: None,
                end_line: None,
                color: OverlayColorSpec::Rgb(255, 0, 0),
                priority: Some(10),
            },
            marker(len - 1, OverlayColorSpec::Rgb(255, 0, 0)),
        ],
    );
    harness.render().unwrap();

    let rows = marker_rows(&harness);
    assert_eq!(
        rows.first().map(|(_, fg)| *fg),
        Some(Some(Color::Rgb(255, 0, 0))),
        "the higher-priority plugin marker wins the cell they share: {rows:?}"
    );
    assert_eq!(
        rows.last().map(|(_, fg)| *fg),
        Some(Some(Color::Rgb(255, 0, 0))),
        "the plugin's other marker is unaffected: {rows:?}"
    );
}

/// Past 5 000 lines the scrollbar switches from the wrapped-row basis to
/// logical lines, and that path pre-resolves every byte the projection will
/// ask about into a map. A regression here is not a wrong mark but a panic in
/// the render loop, so the regime gets its own coverage.
#[test]
fn unsaved_marks_work_on_the_logical_line_basis() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.load_buffer_from_text(&long_content(6_000)).unwrap();
    harness.render().unwrap();

    harness.type_text("edited\n").unwrap();
    harness.render().unwrap();

    let rows = marker_rows(&harness);
    assert_eq!(
        rows.iter().map(|(_, fg)| *fg).collect::<Vec<_>>(),
        vec![Some(UNSAVED_BLUE)],
        "the edit should be marked on a logical-line-basis scrollbar too, saw {rows:?}"
    );
}
