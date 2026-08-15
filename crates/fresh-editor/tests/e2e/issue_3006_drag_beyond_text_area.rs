//! Regression tests for issue #3006 ("Additional" section): dragging the
//! mouse beyond the boundaries of the text area neither extends the
//! selection nor scrolls the viewport.
//!
//! Two independent defects make a drag past the top or bottom edge stop
//! dead, and each test below drives exactly one of them.
//!
//! 1. **A scroll by wheel or scrollbar freezes the drag.** Those set the
//!    viewport's `skip_ensure_visible` flag so the next render doesn't yank
//!    the viewport back to the cursor. Only a *key* press cleared it again,
//!    so after any wheel scroll a drag-select moved the selection head while
//!    the viewport stayed put — in both directions. This is what the issue
//!    reports as "upward drag is dead": the head is already clamped to the
//!    topmost visible line (see 2), and with the viewport frozen there is
//!    nothing left to move at all.
//!
//! 2. **The drag never asks for a line outside the viewport.** The row→line
//!    lookup can only name lines that are on screen, so it clamps to the
//!    first/last visible one. Any scrolling was therefore a side effect of
//!    the scroll-off margin pushing the cursor away from the edge — with a
//!    configured `scroll_offset = 0` the margin is gone and dragging past
//!    either edge does nothing whatsoever.
//!
//! Every assertion reads rendered output only (CONTRIBUTING.md Testing §2):
//! the window of `LINE nnn` markers on screen *is* the scroll position, and
//! the run of cells carrying `theme.selection_bg` *is* the selection. Each
//! test also drags to a row inside the text area and requires the viewport
//! to stay put there, so a fix that simply scrolled on every drag event
//! could not pass.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers, MouseButton, MouseEvent, MouseEventKind};

/// Lines in the fixture — enough that the viewport can travel in either
/// direction without hitting an end.
const LINES: usize = 200;

/// A button-down at a screen cell, which arms a text-selection drag.
fn press(harness: &mut EditorTestHarness, col: u16, row: u16) {
    harness
        .send_mouse(MouseEvent {
            kind: MouseEventKind::Down(MouseButton::Left),
            column: col,
            row,
            modifiers: KeyModifiers::empty(),
        })
        .unwrap();
    harness.render().unwrap();
}

/// One drag-motion event with the button held, exactly as a terminal
/// reports it (SGR `\x1b[<32;C;R M`).
fn drag_to(harness: &mut EditorTestHarness, col: u16, row: u16) {
    harness
        .send_mouse(MouseEvent {
            kind: MouseEventKind::Drag(MouseButton::Left),
            column: col,
            row,
            modifiers: KeyModifiers::empty(),
        })
        .unwrap();
    harness.render().unwrap();
}

/// The number of the topmost `LINE nnn` marker on screen. That marker is
/// the buffer's first visible line, so this *is* the rendered scroll
/// position — it changes the instant the viewport moves by one line and is
/// identical frame-to-frame while it stays put.
fn top_line(harness: &EditorTestHarness) -> u32 {
    let screen = harness.screen_to_string();
    screen
        .lines()
        .flat_map(|line| line.split("LINE ").skip(1))
        .filter_map(|rest| {
            let digits: String = rest.chars().take_while(|c| c.is_ascii_digit()).collect();
            digits.parse::<u32>().ok()
        })
        .next()
        .unwrap_or_else(|| panic!("no LINE marker on screen:\n{screen}"))
}

/// How many cells of screen `row` are painted with the selection
/// background. A fully selected `LINE nnn` contributes at least its eight
/// characters.
fn selected_cells_in_row(harness: &EditorTestHarness, row: u16) -> usize {
    let selection_bg = harness.editor().theme().selection_bg;
    let buffer = harness.buffer();
    let width = buffer.area.width;
    (0..width)
        .filter(|col| buffer.content[buffer.index_of(*col, row)].bg == selection_bg)
        .count()
}

/// Open a 200-line fixture in the harness's window and report its own temp
/// fixture (kept alive by the caller) plus the first and last screen row
/// that carry buffer text.
fn open_fixture(
    harness: &mut EditorTestHarness,
) -> (crate::common::fixtures::TestFixture, u16, u16) {
    let content: Vec<String> = (1..=LINES).map(|i| format!("LINE {i:03}")).collect();
    // `TestFixture` writes into its own temp dir, so tests stay isolated.
    let fixture = harness.load_buffer_from_text(&content.join("\n")).unwrap();
    harness.render().unwrap();
    let (first, last) = harness.content_area_rows();
    (fixture, first as u16, last as u16)
}

/// The issue's own repro: park the viewport in the middle of the file with
/// the wheel, then press inside the text and drag up onto the menu bar and
/// hold there. Every further motion event must keep pulling the viewport
/// toward the buffer's start and keep the top visible line selected.
///
/// Before the fix the wheel's `skip_ensure_visible` flag was still set, so
/// the viewport was pinned and the selection froze — the issue's "six
/// further motion events over the tab bar and menu bar change nothing".
#[test]
fn drag_above_the_text_area_scrolls_up_after_a_wheel_scroll() {
    let mut harness = EditorTestHarness::new(100, 20).unwrap();
    let (_fixture, first_row, _last_row) = open_fixture(&mut harness);

    // Scroll with the wheel — the way a user gets to the middle of a file
    // with the mouse, and the input that leaves the viewport flagged.
    for _ in 0..10 {
        harness.mouse_scroll_down(30, first_row + 2).unwrap();
    }
    harness.render().unwrap();
    let after_wheel = top_line(&harness);
    assert!(
        after_wheel > 10,
        "the wheel should have scrolled well into the file, top line is {after_wheel}. Screen:\n{}",
        harness.screen_to_string()
    );

    press(&mut harness, 10, first_row + 5);

    // Guard against over-fixing: a drag that stays inside the text area
    // must not scroll at all.
    drag_to(&mut harness, 10, first_row + 8);
    assert_eq!(
        top_line(&harness),
        after_wheel,
        "a drag inside the text area must not scroll the viewport. Screen:\n{}",
        harness.screen_to_string()
    );

    // Now hold the pointer above the text area. Each motion event must move
    // the viewport further toward the start of the buffer.
    let mut previous = after_wheel;
    for step in 0..3 {
        drag_to(&mut harness, 10, 0);
        let now = top_line(&harness);
        assert!(
            now < previous,
            "drag step {step} above the text area must scroll up \
             (top line was {previous}, now {now}). Screen:\n{}",
            harness.screen_to_string()
        );
        assert!(
            selected_cells_in_row(&harness, first_row) >= "LINE 001".len(),
            "the top visible line must be selected after dragging above the \
             text area. Screen:\n{}",
            harness.screen_to_string()
        );
        previous = now;
    }
}

/// The same gesture downward. The issue reports that the downward drag
/// "does work", but that measurement started from an unscrolled viewport;
/// once the wheel has been used the very same freeze applies, and the
/// selection cannot be dragged past the bottom edge either.
#[test]
fn drag_below_the_text_area_scrolls_down_after_a_wheel_scroll() {
    let mut harness = EditorTestHarness::new(100, 20).unwrap();
    let (_fixture, first_row, last_row) = open_fixture(&mut harness);

    for _ in 0..10 {
        harness.mouse_scroll_down(30, first_row + 2).unwrap();
    }
    harness.render().unwrap();
    let after_wheel = top_line(&harness);

    press(&mut harness, 10, first_row + 2);

    drag_to(&mut harness, 10, first_row + 6);
    assert_eq!(
        top_line(&harness),
        after_wheel,
        "a drag inside the text area must not scroll the viewport. Screen:\n{}",
        harness.screen_to_string()
    );

    // `last_row + 1` is the status bar: outside the text area, which is
    // where the pointer sits when a user drags past the bottom edge.
    let mut previous = after_wheel;
    for step in 0..3 {
        drag_to(&mut harness, 10, last_row + 1);
        let now = top_line(&harness);
        assert!(
            now > previous,
            "drag step {step} below the text area must scroll down \
             (top line was {previous}, now {now}). Screen:\n{}",
            harness.screen_to_string()
        );
        assert!(
            selected_cells_in_row(&harness, last_row) >= "LINE 001".len(),
            "the bottom visible line must be selected after dragging below \
             the text area. Screen:\n{}",
            harness.screen_to_string()
        );
        previous = now;
    }
}

/// With `scroll_offset = 0` the scroll-off margin that used to do the
/// scrolling by accident is gone, so this isolates defect 2: the drag has
/// to name a line outside the viewport by itself.
///
/// The viewport is parked with the keyboard here, not the wheel, so no
/// `skip_ensure_visible` flag is involved and only the row→line clamp is
/// under test.
#[test]
fn drag_past_the_edges_scrolls_with_scroll_off_disabled() {
    let mut config = fresh::config::Config::default();
    config.editor.scroll_offset = 0;
    let mut harness = EditorTestHarness::with_config(100, 20, config).unwrap();
    let (_fixture, first_row, last_row) = open_fixture(&mut harness);

    // Walk the cursor down so the viewport has room in both directions.
    for _ in 0..40 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();
    let start = top_line(&harness);
    assert!(
        start > 10,
        "the cursor walk should have scrolled into the file, top line is {start}. Screen:\n{}",
        harness.screen_to_string()
    );

    press(&mut harness, 10, first_row + 5);

    let mut previous = start;
    for step in 0..3 {
        drag_to(&mut harness, 10, 0);
        let now = top_line(&harness);
        assert!(
            now < previous,
            "with scroll_offset = 0, drag step {step} above the text area must \
             still scroll up (top line was {previous}, now {now}). Screen:\n{}",
            harness.screen_to_string()
        );
        previous = now;
    }
    assert!(
        selected_cells_in_row(&harness, first_row) >= "LINE 001".len(),
        "the top visible line must be selected after dragging above the text \
         area. Screen:\n{}",
        harness.screen_to_string()
    );

    // And back down past the bottom edge, from the same held button.
    let mut previous = top_line(&harness);
    for step in 0..3 {
        drag_to(&mut harness, 10, last_row + 1);
        let now = top_line(&harness);
        assert!(
            now > previous,
            "with scroll_offset = 0, drag step {step} below the text area must \
             still scroll down (top line was {previous}, now {now}). Screen:\n{}",
            harness.screen_to_string()
        );
        previous = now;
    }
}
