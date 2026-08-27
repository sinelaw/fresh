//! Shaded pane edges, and the wheel gesture that carries text through
//! them.
//!
//! The edge shading is a constant: the two rows at each edge of a pane
//! are painted part of the way up from their background whether the view
//! is moving or still, so text trails off at the edge instead of being
//! cut mid-line. These read the rendered cells, since colour is the
//! whole observable.
//!
//! Harness-direct rather than declarative for the same reason
//! `migrated_animation.rs` is: nothing in `RenderSnapshot` carries
//! per-cell colour, and the wheel's pending-gesture state has no
//! `EditorTestApi` projection.

use crate::common::harness::EditorTestHarness;
use fresh::config::Config;
use ratatui::style::Color;

/// Text that fills well past a screen, one distinguishable line each.
fn numbered_lines(count: usize) -> String {
    (1..=count)
        .map(|i| format!("line {:03} alpha bravo charlie\n", i))
        .collect()
}

fn edge_config() -> Config {
    let mut config = Config::default();
    // Relative numbering would make the line labels ambiguous.
    config.editor.relative_line_numbers = false;
    config
}

/// Foreground and background of the cell at `(col, row)`.
fn colors_at(harness: &EditorTestHarness, col: u16, row: u16) -> (Color, Color) {
    let style = harness
        .get_cell_style(col, row)
        .unwrap_or_else(|| panic!("no cell at ({}, {})", col, row));
    (
        style.fg.expect("cell must carry a foreground"),
        style.bg.expect("cell must carry a background"),
    )
}

/// How far a cell's foreground sits from its background, toward
/// `painted`: 0.0 is on the background, 1.0 is fully painted.
fn level(harness: &EditorTestHarness, col: u16, row: u16, painted: Color) -> f32 {
    let (fg, bg) = colors_at(harness, col, row);
    let channel = |c: Color| match c {
        Color::Rgb(r, _, _) => r as f32,
        other => panic!("expected an RGB colour, got {:?}", other),
    };
    let (fg, bg, painted) = (channel(fg), channel(bg), channel(painted));
    (fg - bg) / (painted - bg)
}

/// The lowest-numbered line currently on screen.
fn top_visible_line(harness: &EditorTestHarness, total: usize) -> usize {
    let screen = harness.screen_to_string();
    (1..=total)
        .find(|i| screen.contains(&format!("line {:03}", i)))
        .expect("some numbered line must be on screen")
}

/// Screen rows the pane's text occupies, as (first, last).
fn text_rows(harness: &EditorTestHarness, total: usize) -> (u16, u16) {
    let screen = harness.screen_to_string();
    let visible: Vec<usize> = (1..=total)
        .filter(|i| screen.contains(&format!("line {:03}", i)))
        .collect();
    let locate = |n: usize| {
        harness
            .find_text_on_screen(&format!("line {:03}", n))
            .expect("visible line must be locatable")
            .1
    };
    (
        locate(*visible.first().expect("some line visible")),
        locate(*visible.last().expect("some line visible")),
    )
}

/// Open a scrollable buffer, scrolled far enough from the top that both
/// edges have content beyond them.
fn harness_scrolled_into_the_middle(lines: usize, config: Config) -> EditorTestHarness {
    let mut harness = EditorTestHarness::with_temp_project_and_config(80, 24, config).unwrap();
    let path = harness.project_dir().unwrap().join("long.txt");
    std::fs::write(&path, numbered_lines(lines)).unwrap();
    harness.open_file(&path).unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("line 001"))
        .unwrap();
    for _ in 0..8 {
        harness.mouse_scroll_down(20, 10).unwrap();
    }
    harness
        .wait_until(|h| !h.editor().has_pending_wheel_scroll())
        .unwrap();
    harness
}

/// Both edges of the pane are shaded, dimmest hard against the edge and
/// a step brighter one row in, with the third row already at full
/// strength.
#[test]
fn the_two_rows_at_each_edge_are_shaded_in_steps() {
    const LINES: usize = 200;
    const TEXT_COL: u16 = 20;
    let mut harness = harness_scrolled_into_the_middle(LINES, edge_config());

    let (first, last) = text_rows(&harness, LINES);
    // A row well clear of either band is the fully painted reference.
    let painted = colors_at(&harness, TEXT_COL, first + 6).0;

    let at = |row: u16| level(&harness, TEXT_COL, row, painted);

    // Top edge.
    assert!(
        at(first) < at(first + 1),
        "the top row is dimmer than the one below it: {:.2} vs {:.2}",
        at(first),
        at(first + 1)
    );
    assert!(
        at(first + 1) < 1.0,
        "the second row is shaded too, at {:.2}",
        at(first + 1)
    );
    assert_eq!(
        colors_at(&harness, TEXT_COL, first + 2).0,
        painted,
        "the third row is already fully painted, screen:\n{}",
        harness.screen_to_string()
    );

    // Bottom edge, the same the other way up.
    assert!(
        at(last) < at(last - 1),
        "the bottom row is dimmer than the one above it: {:.2} vs {:.2}",
        at(last),
        at(last - 1)
    );
    assert!(
        at(last - 1) < 1.0,
        "the second-from-bottom row is shaded too, at {:.2}",
        at(last - 1)
    );
    assert_eq!(
        colors_at(&harness, TEXT_COL, last - 2).0,
        painted,
        "the third row up is already fully painted, screen:\n{}",
        harness.screen_to_string()
    );
}

/// The shading is a property of where a row sits, not of anything that
/// happened: it is there on a screen that has been still for as long as
/// the test can make it, and unchanged after a scroll settles.
#[test]
fn the_shading_is_constant_rather_than_something_that_settles() {
    const LINES: usize = 200;
    const TEXT_COL: u16 = 20;
    let mut harness = harness_scrolled_into_the_middle(LINES, edge_config());

    let (first, _) = text_rows(&harness, LINES);
    let painted = colors_at(&harness, TEXT_COL, first + 6).0;
    let before = level(&harness, TEXT_COL, first, painted);

    // Nothing is in flight, and more frames change nothing.
    assert!(!harness.editor().active_window().animations.is_active());
    for _ in 0..10 {
        harness.render().unwrap();
    }
    let after_waiting = level(&harness, TEXT_COL, first, painted);
    assert_eq!(
        format!("{:.3}", before),
        format!("{:.3}", after_waiting),
        "a still screen keeps exactly the same shading"
    );

    // And a scroll moves text through the gradient without changing it.
    harness.mouse_scroll_down(20, 10).unwrap();
    harness
        .wait_until(|h| !h.editor().has_pending_wheel_scroll())
        .unwrap();
    let (first, _) = text_rows(&harness, LINES);
    assert_eq!(
        format!("{:.3}", before),
        format!("{:.3}", level(&harness, TEXT_COL, first, painted)),
        "the same row of the pane is shaded the same after scrolling"
    );
}

/// At the top of a file there is nothing above to trail off into, so the
/// first lines are left alone. Shading them would dim the opening of
/// every file that was never scrolled.
#[test]
fn the_top_edge_is_left_alone_when_nothing_is_above_it() {
    const LINES: usize = 200;
    const TEXT_COL: u16 = 20;
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(80, 24, edge_config()).unwrap();
    let path = harness.project_dir().unwrap().join("long.txt");
    std::fs::write(&path, numbered_lines(LINES)).unwrap();
    harness.open_file(&path).unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("line 001"))
        .unwrap();

    let (first, last) = text_rows(&harness, LINES);
    let painted = colors_at(&harness, TEXT_COL, first + 6).0;
    assert_eq!(
        colors_at(&harness, TEXT_COL, first).0,
        painted,
        "line 001 is the top of the file — nothing above it to fade \
         into, screen:\n{}",
        harness.screen_to_string()
    );
    // The bottom edge still shades: the file continues past it.
    assert!(
        level(&harness, TEXT_COL, last, painted) < 1.0,
        "the bottom edge shades even at the top of the file"
    );

    // Scroll off the first line and the top edge starts shading.
    harness.mouse_scroll_down(20, 10).unwrap();
    harness
        .wait_until(|h| !h.editor().has_pending_wheel_scroll())
        .unwrap();
    let (first, _) = text_rows(&harness, LINES);
    assert!(
        level(&harness, TEXT_COL, first, painted) < 1.0,
        "once there is something above, the top edge shades, screen:\n{}",
        harness.screen_to_string()
    );
}

/// The toggle turns the shading off entirely: every row paints at full
/// strength, edges included.
#[test]
fn viewport_edge_fade_disabled_paints_every_row_at_full_strength() {
    const LINES: usize = 200;
    const TEXT_COL: u16 = 20;
    let mut config = edge_config();
    config.editor.viewport_edge_fade = false;
    let mut harness = harness_scrolled_into_the_middle(LINES, config);

    let (first, last) = text_rows(&harness, LINES);
    let painted = colors_at(&harness, TEXT_COL, first + 6).0;
    for row in [first, first + 1, last - 1, last] {
        assert_eq!(
            colors_at(&harness, TEXT_COL, row).0,
            painted,
            "row {} must be fully painted with the shading off, \
             screen:\n{}",
            row,
            harness.screen_to_string()
        );
    }
}

/// A multi-line notch walks the view a line at a time instead of
/// jumping it. The first line lands on the frame the event produces —
/// the view answers the wheel immediately — and the rest follow over
/// the next frames.
#[test]
fn a_multi_line_notch_walks_the_view_a_line_at_a_time() {
    const LINES: usize = 200;
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(80, 24, edge_config()).unwrap();
    let path = harness.project_dir().unwrap().join("long.txt");
    std::fs::write(&path, numbered_lines(LINES)).unwrap();
    harness.open_file(&path).unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("line 001"))
        .unwrap();
    assert_eq!(
        harness.config().editor.mouse_wheel_scroll_lines,
        3,
        "this test is about a notch worth more than one line"
    );

    // `mouse_scroll_down` renders the frame the event produces.
    harness.mouse_scroll_down(20, 10).unwrap();
    assert_eq!(
        top_visible_line(&harness, LINES),
        2,
        "the first line of the notch lands right away, screen:\n{}",
        harness.screen_to_string()
    );

    // And the walk carries the view the rest of the notch.
    harness
        .wait_until(|h| top_visible_line(h, LINES) == 4)
        .unwrap();
}

/// With the walk turned off, a notch moves the view by the whole
/// `mouse_wheel_scroll_lines` at once.
#[test]
fn smooth_scroll_disabled_jumps_the_whole_notch() {
    const LINES: usize = 200;
    let mut config = edge_config();
    config.editor.smooth_scroll = false;

    let mut harness = EditorTestHarness::with_temp_project_and_config(80, 24, config).unwrap();
    let path = harness.project_dir().unwrap().join("long.txt");
    std::fs::write(&path, numbered_lines(LINES)).unwrap();
    harness.open_file(&path).unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("line 001"))
        .unwrap();

    harness.mouse_scroll_down(20, 10).unwrap();
    assert_eq!(
        top_visible_line(&harness, LINES),
        4,
        "the whole three-line notch lands at once, screen:\n{}",
        harness.screen_to_string()
    );
}

/// `mouse_wheel_scroll_lines` is how far one notch takes the view in
/// total, walk or no walk.
#[test]
fn one_wheel_notch_scrolls_the_configured_number_of_lines() {
    const LINES: usize = 200;

    let top_after_one_notch = |lines_per_notch: usize| -> usize {
        let mut config = edge_config();
        config.editor.mouse_wheel_scroll_lines = lines_per_notch;
        let mut harness = EditorTestHarness::with_temp_project_and_config(80, 24, config).unwrap();
        let path = harness.project_dir().unwrap().join("long.txt");
        std::fs::write(&path, numbered_lines(LINES)).unwrap();
        harness.open_file(&path).unwrap();
        harness
            .wait_until(|h| h.screen_to_string().contains("line 001"))
            .unwrap();
        harness.mouse_scroll_down(20, 10).unwrap();
        // Let the walk finish before reading where it landed.
        harness
            .wait_until(|h| !h.editor().has_pending_wheel_scroll())
            .unwrap();
        top_visible_line(&harness, LINES)
    };

    assert_eq!(top_after_one_notch(1), 2, "one line puts line 002 on top");
    assert_eq!(top_after_one_notch(5), 6, "five lines puts line 006 on top");
}
