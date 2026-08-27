//! Scroll fade — text rises from the background as a scroll brings it
//! into view.
//!
//! The observable is colour, so these drive a real mouse wheel and read
//! the rendered cells: a row the scroll just revealed arrives dimmed
//! toward its own background, while the rows the scroll merely shifted
//! keep their normal foreground. Once the animation settles, the
//! revealed row reads like any other.
//!
//! Harness-direct rather than declarative for the same reason
//! `migrated_animation.rs` is: the animation runner
//! (`animations.is_active()`) has no `EditorTestApi` projection, and
//! nothing in `RenderSnapshot` carries per-cell colour.
//!
//! Animations are off by default in the test harness, so these opt in
//! explicitly.

use crate::common::harness::EditorTestHarness;
use fresh::config::Config;
use ratatui::style::Color;

/// Text that fills well past a screen, one distinguishable line each.
fn numbered_lines(count: usize) -> String {
    (1..=count)
        .map(|i| format!("line {:03} alpha bravo charlie\n", i))
        .collect()
}

fn scroll_fade_config() -> Config {
    let mut config = Config::default();
    config.editor.animations = true;
    config.editor.scroll_fade_animation = true;
    // The gutter is not the subject here, and relative numbering would
    // make the line labels ambiguous.
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

/// How far a cell's foreground has climbed from its background toward
/// `painted`: 0.0 sits on the background, 1.0 is fully painted.
fn level(harness: &EditorTestHarness, col: u16, row: u16, painted: Color) -> f32 {
    let (fg, bg) = colors_at(harness, col, row);
    let channel = |c: Color| match c {
        Color::Rgb(r, _, _) => r as f32,
        other => panic!("expected an RGB colour, got {:?}", other),
    };
    let (fg, bg, painted) = (channel(fg), channel(bg), channel(painted));
    (fg - bg) / (painted - bg)
}

/// Screen position of the first character of `text`, which must be on
/// screen.
fn locate(harness: &EditorTestHarness, text: &str) -> (u16, u16) {
    harness
        .find_text_on_screen(text)
        .unwrap_or_else(|| panic!("{:?} not on screen:\n{}", text, harness.screen_to_string()))
}

/// The highest-numbered line currently on screen, and where it starts.
fn last_visible_line(harness: &EditorTestHarness, total: usize) -> (usize, (u16, u16)) {
    let screen = harness.screen_to_string();
    let n = (1..=total)
        .rev()
        .find(|i| screen.contains(&format!("line {:03}", i)))
        .expect("some numbered line must be on screen");
    (n, locate(harness, &format!("line {:03}", n)))
}

/// Open a scrollable buffer and settle every animation the open kicked
/// off, so the scroll under test starts from a static screen.
fn harness_with_scrollable_file(lines: usize) -> EditorTestHarness {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(80, 24, scroll_fade_config()).unwrap();
    let path = harness.project_dir().unwrap().join("long.txt");
    std::fs::write(&path, numbered_lines(lines)).unwrap();
    harness.open_file(&path).unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("line 001"))
        .unwrap();
    harness
        .wait_until(|h| !h.editor().active_window().animations.is_active())
        .unwrap();
    harness
}

/// One wheel notch down: the row that arrives at the bottom is dimmed
/// toward the background on the frame the scroll produces, and the rows
/// above it — the same text, one row higher — are not.
#[test]
fn scrolling_down_fades_the_revealed_row_in_from_the_background() {
    const LINES: usize = 200;
    let mut harness = harness_with_scrollable_file(LINES);

    // A line comfortably inside the viewport before and after the
    // scroll: the wheel shifts it up, it must not dim.
    let (settled_line, _) = last_visible_line(&harness, LINES);
    let shifted = format!("line {:03}", settled_line - 5);
    let (sx, sy) = locate(&harness, &shifted);
    let (painted, _) = colors_at(&harness, sx, sy);

    // Scroll over the text. `mouse_scroll_down` renders the resulting
    // frame, which is the first frame of the fade.
    harness.mouse_scroll_down(20, 10).unwrap();

    let (revealed_line, (rx, ry)) = last_visible_line(&harness, LINES);
    assert!(
        revealed_line > settled_line,
        "the wheel must have brought a new line in: was {}, now {}",
        settled_line,
        revealed_line
    );
    let arrived = level(&harness, rx, ry, painted);
    assert!(
        arrived > 0.0 && arrived < 0.5,
        "line {:03} just scrolled in — it arrives dimmed but readable, \
         at {:.2}, screen:\n{}",
        revealed_line,
        arrived,
        harness.screen_to_string()
    );

    let (sx, sy) = locate(&harness, &shifted);
    assert_eq!(
        colors_at(&harness, sx, sy).0,
        painted,
        "{} was already on screen — the scroll only moved it, so it \
         must keep its colour, screen:\n{}",
        shifted,
        harness.screen_to_string()
    );

    // And the fade lands on the normal foreground rather than leaving
    // the row dimmed.
    harness
        .wait_until(|h| !h.editor().active_window().animations.is_active())
        .unwrap();
    let (rx, ry) = locate(&harness, &format!("line {:03}", revealed_line));
    assert_eq!(
        colors_at(&harness, rx, ry).0,
        painted,
        "once the fade settles the revealed row reads exactly like the \
         rows around it"
    );
}

/// Scrolling back up dims the row arriving at the top, the same way.
#[test]
fn scrolling_up_fades_the_revealed_row_in_from_the_background() {
    const LINES: usize = 200;
    let mut harness = harness_with_scrollable_file(LINES);

    // Get away from the top of the file so there is something above.
    for _ in 0..8 {
        harness.mouse_scroll_down(20, 10).unwrap();
    }
    harness
        .wait_until(|h| !h.editor().active_window().animations.is_active())
        .unwrap();

    let screen = harness.screen_to_string();
    let first_visible = (1..=LINES)
        .find(|i| screen.contains(&format!("line {:03}", i)))
        .expect("some numbered line must be on screen");
    let (px, py) = locate(&harness, &format!("line {:03}", first_visible + 5));
    let (painted, _) = colors_at(&harness, px, py);

    harness.mouse_scroll_up(20, 10).unwrap();

    let screen = harness.screen_to_string();
    let revealed = (1..=LINES)
        .find(|i| screen.contains(&format!("line {:03}", i)))
        .expect("some numbered line must be on screen");
    assert!(
        revealed < first_visible,
        "scrolling up must expose an earlier line: was {}, now {}",
        first_visible,
        revealed
    );

    let (rx, ry) = locate(&harness, &format!("line {:03}", revealed));
    let arrived = level(&harness, rx, ry, painted);
    assert!(
        arrived > 0.0 && arrived < 0.5,
        "line {:03} arrived at the top — it arrives dimmed but \
         readable, at {:.2}, screen:\n{}",
        revealed,
        arrived,
        harness.screen_to_string()
    );
}

/// The dimmed band stays a fixed few rows deep however fast the scroll
/// runs. On a clock, a fast scroll piles up every row that arrived
/// within the fade duration, and a block of the viewport steps to dark
/// at once — which is what this rules out.
#[test]
fn a_fast_scroll_dims_only_a_shallow_band_at_the_edge() {
    const LINES: usize = 400;
    const TEXT_COL: u16 = 20;
    let mut harness = harness_with_scrollable_file(LINES);

    let (_, (px, py)) = last_visible_line(&harness, LINES);
    let (painted, _) = colors_at(&harness, px, py);

    // Eight notches back to back, far quicker than the fade clock.
    for _ in 0..8 {
        harness.mouse_scroll_down(20, 10).unwrap();
    }

    // Walk up from the last text row until the rows read at full
    // strength again; that run is the band.
    let (_, (_, bottom_row)) = last_visible_line(&harness, LINES);
    let dimmed = (2..=bottom_row)
        .rev()
        .take_while(|row| level(&harness, TEXT_COL, *row, painted) < 1.0)
        .count();
    assert!(
        dimmed > 0 && dimmed <= 6,
        "a fast scroll must dim a shallow band, not a block of the \
         viewport — {} rows dimmed, screen:\n{}",
        dimmed,
        harness.screen_to_string()
    );
}

/// The dedicated toggle turns the fade off on its own, leaving the rest
/// of the animations alone: a scrolled-in row is legible immediately.
#[test]
fn scroll_fade_disabled_paints_revealed_rows_at_full_strength() {
    const LINES: usize = 200;
    let mut config = scroll_fade_config();
    config.editor.scroll_fade_animation = false;

    let mut harness = EditorTestHarness::with_temp_project_and_config(80, 24, config).unwrap();
    let path = harness.project_dir().unwrap().join("long.txt");
    std::fs::write(&path, numbered_lines(LINES)).unwrap();
    harness.open_file(&path).unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("line 001"))
        .unwrap();
    harness
        .wait_until(|h| !h.editor().active_window().animations.is_active())
        .unwrap();

    let (_, (px, py)) = last_visible_line(&harness, LINES);
    let (painted, _) = colors_at(&harness, px, py);

    harness.mouse_scroll_down(20, 10).unwrap();

    let (revealed_line, (rx, ry)) = last_visible_line(&harness, LINES);
    assert_eq!(
        colors_at(&harness, rx, ry).0,
        painted,
        "with the toggle off, line {:03} appears at full strength, \
         screen:\n{}",
        revealed_line,
        harness.screen_to_string()
    );
}

/// `mouse_wheel_scroll_lines` is how far one notch takes the view, and
/// one line is the default: the wheel moves the view a line at a time
/// rather than jumping it in blocks.
#[test]
fn one_wheel_notch_scrolls_the_configured_number_of_lines() {
    const LINES: usize = 200;

    let top_after_one_notch = |lines_per_notch: Option<usize>| -> usize {
        let mut config = scroll_fade_config();
        if let Some(lines) = lines_per_notch {
            config.editor.mouse_wheel_scroll_lines = lines;
        }
        let mut harness = EditorTestHarness::with_temp_project_and_config(80, 24, config).unwrap();
        let path = harness.project_dir().unwrap().join("long.txt");
        std::fs::write(&path, numbered_lines(LINES)).unwrap();
        harness.open_file(&path).unwrap();
        harness
            .wait_until(|h| h.screen_to_string().contains("line 001"))
            .unwrap();
        harness.mouse_scroll_down(20, 10).unwrap();
        let screen = harness.screen_to_string();
        (1..=LINES)
            .find(|i| screen.contains(&format!("line {:03}", i)))
            .expect("some numbered line must be on screen")
    };

    assert_eq!(
        top_after_one_notch(None),
        2,
        "by default one notch moves the view one line, so line 002 is \
         now at the top"
    );
    assert_eq!(
        top_after_one_notch(Some(5)),
        6,
        "and the setting is how many lines a notch is worth"
    );
}
