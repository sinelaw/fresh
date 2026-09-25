//! Regression tests for issue #3329: a drag along a row near the top (or
//! bottom) of the viewport scrolled the view on every motion, so the
//! selection ran away from the pointer — backwards from the anchor along
//! the top rows.
//!
//! Each drag event let the render pass apply the scroll-off margin
//! (`editor.scroll_offset`, 3 by default) to the selection head. A head on
//! one of the first three rows scrolled the view up three lines, and the
//! next motion at the same screen row then named a line three higher. The
//! anchor line ended up with only its prefix selected and the lines above
//! it fully selected.
//!
//! A drag *past* the edge still scrolls (issue #3006, covered by
//! `issue_3006_drag_beyond_text_area`); inside the text area the head is the
//! cell under the pointer, so nothing needs to scroll.
//!
//! Assertions read rendered output only: the gutter's line numbers and the
//! cells painted with the selection background.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyModifiers, MouseButton, MouseEvent, MouseEventKind};

const LINE: &str = "this is a text";

fn mouse(harness: &mut EditorTestHarness, kind: MouseEventKind, col: u16, row: u16) {
    harness
        .send_mouse(MouseEvent {
            kind,
            column: col,
            row,
            modifiers: KeyModifiers::NONE,
        })
        .unwrap();
    harness.render().unwrap();
}

/// The gutter's line number on screen `row`, if the row shows a line.
fn gutter_line(harness: &EditorTestHarness, row: u16) -> Option<u32> {
    harness
        .screen_row_text(row)
        .split_once('│')
        .and_then(|(gutter, _)| gutter.trim().parse().ok())
}

/// The characters painted with the selection background on `row` (the
/// in-selection whitespace marker folded back to a space).
fn selected_text(harness: &EditorTestHarness, row: u16) -> String {
    let bg = harness.editor().theme().selection_bg;
    (0..harness.buffer().area.width)
        .filter(|&col| harness.get_cell_style(col, row).and_then(|s| s.bg) == Some(bg))
        .filter_map(|col| harness.get_cell(col, row))
        .collect::<String>()
        .replace('·', " ")
}

/// Every content row, with its gutter line number and selected text, for
/// failure messages and for "nothing else is selected" checks.
fn selected_rows(harness: &EditorTestHarness) -> Vec<(u32, String)> {
    (0..harness.buffer().area.height)
        .filter_map(|row| Some((gutter_line(harness, row)?, selected_text(harness, row))))
        .filter(|(_, sel)| !sel.is_empty())
        .collect()
}

/// Open 50 copies of `LINE`, wheel down so the view is well away from line
/// 1, and return the harness, fixture and the screen rows of the first and
/// last visible text rows plus the column where `LINE` starts.
fn scrolled_fixture() -> (
    EditorTestHarness,
    crate::common::fixtures::TestFixture,
    u16,
    u16,
    u16,
) {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let fixture = harness
        .load_buffer_from_text(&format!("{LINE}\n").repeat(50))
        .unwrap();
    harness.render().unwrap();
    for _ in 0..5 {
        harness.mouse_scroll_down(30, 8).unwrap();
    }
    harness.render().unwrap();

    let (text_col, first_row) = harness.find_text_on_screen(LINE).unwrap();
    let last_row = (first_row..harness.buffer().area.height)
        .take_while(|&row| gutter_line(&harness, row).is_some())
        .last()
        .unwrap();
    assert!(
        gutter_line(&harness, first_row).unwrap() > 5,
        "the wheel should have scrolled well away from line 1:\n{}",
        harness.screen_to_string()
    );
    (harness, fixture, first_row, last_row, text_col)
}

/// Press on the `a` of `row`'s `this is a text` and drag right along the
/// same row to past the end of the line, a frame between each motion.
/// Returns the line the row showed before the press.
fn drag_along_row(harness: &mut EditorTestHarness, row: u16, text_col: u16) -> u32 {
    let line = gutter_line(harness, row).unwrap();
    let a_col = text_col + 8;
    mouse(harness, MouseEventKind::Down(MouseButton::Left), a_col, row);
    for col in [a_col + 3, a_col + 5, a_col + 6, a_col + 10, a_col + 12] {
        mouse(harness, MouseEventKind::Drag(MouseButton::Left), col, row);
    }
    mouse(
        harness,
        MouseEventKind::Up(MouseButton::Left),
        a_col + 12,
        row,
    );
    line
}

#[test]
fn test_drag_along_top_row_selects_suffix_without_scrolling() {
    let (mut harness, _fixture, first_row, _, text_col) = scrolled_fixture();
    let before = harness.screen_to_string();

    let line = drag_along_row(&mut harness, first_row, text_col);

    assert_eq!(
        gutter_line(&harness, first_row),
        Some(line),
        "a drag inside the text area must not scroll the view.\nBefore:\n{before}\nAfter:\n{}",
        harness.screen_to_string()
    );
    assert_eq!(
        selected_rows(&harness),
        vec![(line, "a text".to_string())],
        "only the suffix of the dragged line is selected.\nAfter:\n{}",
        harness.screen_to_string()
    );
}

#[test]
fn test_drag_along_bottom_row_selects_suffix_without_scrolling() {
    let (mut harness, _fixture, first_row, last_row, text_col) = scrolled_fixture();
    let top = gutter_line(&harness, first_row);
    let before = harness.screen_to_string();

    let line = drag_along_row(&mut harness, last_row, text_col);

    assert_eq!(
        gutter_line(&harness, first_row),
        top,
        "a drag inside the text area must not scroll the view.\nBefore:\n{before}\nAfter:\n{}",
        harness.screen_to_string()
    );
    assert_eq!(
        selected_rows(&harness),
        vec![(line, "a text".to_string())],
        "only the suffix of the dragged line is selected.\nAfter:\n{}",
        harness.screen_to_string()
    );
}

/// Dragging off the side of the pane leaves the text area, but the pointer
/// is still level with the row it pressed on: that row's line is on screen,
/// so the view holds still just as it does inside the text area. Letting
/// ensure-visible place the head there applied the scroll-off margin to the
/// top row, and each motion at the same screen row then named a line higher
/// up, so a sideways drag selected a run of lines above the anchor.
#[test]
fn test_drag_off_the_side_of_the_pane_along_top_row_does_not_scroll() {
    let (mut harness, _fixture, first_row, _, text_col) = scrolled_fixture();
    let before = harness.screen_to_string();
    let line = gutter_line(&harness, first_row).unwrap();
    let right = harness.buffer().area.width - 1;

    let a_col = text_col + 8;
    mouse(
        &mut harness,
        MouseEventKind::Down(MouseButton::Left),
        a_col,
        first_row,
    );
    mouse(
        &mut harness,
        MouseEventKind::Drag(MouseButton::Left),
        a_col + 3,
        first_row,
    );
    for _ in 0..4 {
        mouse(
            &mut harness,
            MouseEventKind::Drag(MouseButton::Left),
            right,
            first_row,
        );
    }
    mouse(
        &mut harness,
        MouseEventKind::Up(MouseButton::Left),
        right,
        first_row,
    );

    assert_eq!(
        gutter_line(&harness, first_row),
        Some(line),
        "a drag off the side of the pane must not scroll the view.\nBefore:\n{before}\nAfter:\n{}",
        harness.screen_to_string()
    );
    assert_eq!(
        selected_rows(&harness),
        vec![(line, "a text".to_string())],
        "only the suffix of the dragged line is selected.\nAfter:\n{}",
        harness.screen_to_string()
    );
}

// ---------------------------------------------------------------------------
// What holding the view inside the text area must not take away: the drag
// still follows the head sideways, and a text area with no chrome beyond an
// edge still scrolls when the pointer reaches that edge.
// ---------------------------------------------------------------------------

/// The `wNN` tokens of one long line, 4 cells each.
const TOKENS: usize = 60;

fn long_line() -> String {
    (0..TOKENS).map(|i| format!("w{i:02} ")).collect()
}

/// The number of the leftmost whole `wNN` token drawn on `row`'s text: the
/// rendered horizontal scroll position.
fn first_token(harness: &EditorTestHarness, row: u16) -> u32 {
    let text = harness.screen_row_text(row);
    let (_, content) = text.split_once('│').unwrap_or(("", &text));
    content
        .split('w')
        .skip(1)
        .find_map(|rest| {
            let digits: String = rest.chars().take_while(|c| c.is_ascii_digit()).collect();
            (digits.len() == 2).then(|| digits.parse().unwrap())
        })
        .unwrap_or_else(|| panic!("no token on row {row}:\n{}", harness.screen_to_string()))
}

fn no_wrap_fixture() -> (EditorTestHarness, crate::common::fixtures::TestFixture, u16) {
    let mut config = fresh::config::Config::default();
    config.editor.line_wrap = false;
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    let fixture = harness
        .load_buffer_from_text(&format!("{}\n", long_line()))
        .unwrap();
    harness.render().unwrap();
    let (_, row) = harness.find_text_on_screen("w00").unwrap();
    (harness, fixture, row)
}

/// With the line scrolled right, dragging left into the gutter scrolls the
/// view back left, the way it did before the view was held inside the text
/// area: the head is on screen there, but the horizontal follow is what
/// brings the rest of the line into view.
#[test]
fn test_drag_into_the_gutter_scrolls_a_no_wrap_line_left() {
    let (mut harness, _fixture, row) = no_wrap_fixture();
    harness
        .send_key(crossterm::event::KeyCode::End, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    let scrolled = first_token(&harness, row);
    assert!(
        scrolled > 20,
        "End scrolls the line right:\n{}",
        harness.screen_to_string()
    );

    let (text_col, _) = harness
        .find_text_on_screen(&format!("w{:02}", scrolled))
        .unwrap();
    mouse(
        &mut harness,
        MouseEventKind::Down(MouseButton::Left),
        text_col + 20,
        row,
    );
    let mut previous = scrolled;
    for step in 0..3 {
        // Column 1: inside the gutter.
        mouse(
            &mut harness,
            MouseEventKind::Drag(MouseButton::Left),
            1,
            row,
        );
        let now = first_token(&harness, row);
        assert!(
            now < previous,
            "drag step {step} into the gutter must scroll left (first token was \
             w{previous:02}, now w{now:02}):\n{}",
            harness.screen_to_string()
        );
        previous = now;
    }
}

/// Dragging to the right edge of the text scrolls the line right.
#[test]
fn test_drag_to_the_right_edge_scrolls_a_no_wrap_line_right() {
    let (mut harness, _fixture, row) = no_wrap_fixture();
    let (text_col, _) = harness.find_text_on_screen("w00").unwrap();
    assert_eq!(first_token(&harness, row), 0);

    mouse(
        &mut harness,
        MouseEventKind::Down(MouseButton::Left),
        text_col + 2,
        row,
    );
    // The last column the line's text is drawn in.
    let right = (0..harness.buffer().area.width)
        .rev()
        .find(|&col| {
            harness
                .get_cell(col, row)
                .is_some_and(|c| c.chars().all(|ch| ch.is_ascii_alphanumeric()))
        })
        .unwrap();
    let mut previous = 0;
    for step in 0..3 {
        mouse(
            &mut harness,
            MouseEventKind::Drag(MouseButton::Left),
            right,
            row,
        );
        let now = first_token(&harness, row);
        assert!(
            now > previous,
            "drag step {step} at the right edge must scroll right (first token was \
             w{previous:02}, now w{now:02}):\n{}",
            harness.screen_to_string()
        );
        previous = now;
    }
}

/// With the menu, tab and status bars hidden the text area runs from the
/// first screen row to the last: there is nowhere past its edge for the
/// pointer to go, so its edge rows are where a drag scrolls.
#[test]
fn test_drag_to_the_screen_edges_scrolls_with_the_chrome_hidden() {
    let mut config = fresh::config::Config::default();
    config.editor.show_menu_bar = false;
    config.editor.show_tab_bar = false;
    config.editor.show_status_bar = false;
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    // The harness keeps the prompt line up whatever the config says; hide it
    // as a user's default (auto-hide) does.
    harness
        .editor_mut()
        .active_window_mut()
        .toggle_prompt_line();
    let _fixture = harness
        .load_buffer_from_text(&format!("{LINE}\n").repeat(200))
        .unwrap();
    harness.render().unwrap();
    for _ in 0..20 {
        harness.mouse_scroll_down(30, 8).unwrap();
    }
    harness.render().unwrap();
    let bottom = harness.buffer().area.height - 1;
    assert!(
        gutter_line(&harness, 0).is_some() && gutter_line(&harness, bottom).is_some(),
        "the text area fills the screen:\n{}",
        harness.screen_to_string()
    );
    let start = gutter_line(&harness, 0).unwrap();
    assert!(start > 20, "{}", harness.screen_to_string());

    mouse(
        &mut harness,
        MouseEventKind::Down(MouseButton::Left),
        20,
        10,
    );
    let mut previous = start;
    for step in 0..3 {
        mouse(&mut harness, MouseEventKind::Drag(MouseButton::Left), 20, 0);
        let now = gutter_line(&harness, 0).unwrap();
        assert!(
            now < previous,
            "drag step {step} on the top row must scroll up (top line was \
             {previous}, now {now}):\n{}",
            harness.screen_to_string()
        );
        previous = now;
    }
    for step in 0..3 {
        mouse(
            &mut harness,
            MouseEventKind::Drag(MouseButton::Left),
            20,
            bottom,
        );
        let now = gutter_line(&harness, 0).unwrap();
        assert!(
            now > previous,
            "drag step {step} on the bottom row must scroll down (top line was \
             {previous}, now {now}):\n{}",
            harness.screen_to_string()
        );
        previous = now;
    }
}
