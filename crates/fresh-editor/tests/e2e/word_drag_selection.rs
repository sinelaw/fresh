//! Word-granularity drag selection: what a double-click arms, and what a
//! plain click does not.
//!
//! The gesture is "double-click a word, keep the button down, and drag". The
//! selection then grows a **whole word at a time** — dragging rightwards
//! through `quick brown fox` from a double-click on `quick` passes through
//! exactly `quick`, then `quick brown`, then `quick brown fox`. It is never
//! caught mid-word: a pointer parked over the `o` of `brown` still selects
//! all of `brown`. A plain single-click drag over the same cells selects one
//! character at a time, which is what makes the two granularities distinct
//! rather than one being a coarser rendering of the other.
//!
//! Every assertion reads rendered output only (CONTRIBUTING.md Testing §2):
//! the run of cells carrying `theme.selection_bg` on the text row *is* the
//! selection, so `selected_text` below reconstructs what the user sees
//! highlighted rather than asking the model what it selected. That matters
//! here beyond style — the defect this guards against is a selection whose
//! *edges* land inside a word, and an edge is a rendered column.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyModifiers, MouseButton, MouseEvent, MouseEventKind};

/// The fixture. Three words whose lengths differ, so a selection that
/// stopped at the wrong word boundary cannot coincidentally match the right
/// one's column.
const LINE: &str = "quick brown fox jumps over\n";

/// A harness showing [`LINE`], with the row and gutter the text was drawn at.
fn fixture() -> (EditorTestHarness, u16, u16) {
    let mut harness = EditorTestHarness::new_no_wrap(80, 24).unwrap();
    let _fixture = harness.load_buffer_from_text(LINE).unwrap();
    harness.render().unwrap();
    let (content_first_row, _) = harness.content_area_rows();
    let gutter = harness.editor().active_state().margins.left_total_width() as u16;
    (harness, content_first_row as u16, gutter)
}

fn press(harness: &mut EditorTestHarness, col: u16, row: u16) {
    harness
        .send_mouse(MouseEvent {
            kind: MouseEventKind::Down(MouseButton::Left),
            column: col,
            row,
            modifiers: KeyModifiers::NONE,
        })
        .unwrap();
    harness.render().unwrap();
}

fn release(harness: &mut EditorTestHarness, col: u16, row: u16) {
    harness
        .send_mouse(MouseEvent {
            kind: MouseEventKind::Up(MouseButton::Left),
            column: col,
            row,
            modifiers: KeyModifiers::NONE,
        })
        .unwrap();
    harness.render().unwrap();
}

fn drag_to(harness: &mut EditorTestHarness, col: u16, row: u16) {
    harness
        .send_mouse(MouseEvent {
            kind: MouseEventKind::Drag(MouseButton::Left),
            column: col,
            row,
            modifiers: KeyModifiers::NONE,
        })
        .unwrap();
    harness.render().unwrap();
}

/// Press, release, press at the same cell — the second press is the
/// double-click, and it is left *held* so the drag that follows extends the
/// word selection rather than starting a new one.
fn double_click_and_hold(harness: &mut EditorTestHarness, col: u16, row: u16) {
    press(harness, col, row);
    release(harness, col, row);
    press(harness, col, row);
}

/// The text of the cells on `row` painted with the selection background —
/// the selection exactly as the user sees it.
///
/// The in-selection whitespace indicator draws `·` over a selected space, so
/// it is folded back to a space; each indicator occupies one cell, so columns
/// are unaffected.
fn selected_text(harness: &EditorTestHarness, row: u16) -> String {
    let selection_bg = harness.editor().theme().selection_bg;
    let width = harness.buffer().area.width;
    (0..width)
        .filter(|col| {
            harness
                .get_cell_style(*col, row)
                .and_then(|s| s.bg)
                .is_some_and(|bg| bg == selection_bg)
        })
        .filter_map(|col| harness.get_cell(col, row))
        .collect::<String>()
        .replace('·', " ")
}

/// Double-click, then drag rightwards: the selection grows one whole word at
/// a time and is never caught mid-word.
///
/// The drag visits *two* cells inside each word — its first character and one
/// in its middle — and both must report the same, fully-selected word. A
/// character-granular extension would differ between the two, and so would an
/// implementation that snapped to word ends only on entering a word.
#[test]
fn double_click_drag_extends_selection_a_word_at_a_time() {
    let (mut harness, row, gutter) = fixture();

    // `quick` at +0..+5, `brown` at +6..+11, `fox` at +12..+15.
    let quick_mid = gutter + 2;
    double_click_and_hold(&mut harness, quick_mid, row);
    assert_eq!(
        selected_text(&harness, row),
        "quick",
        "a double-click selects the word under the pointer"
    );

    for (col, want) in [
        (gutter + 6, "quick brown"),  // first cell of `brown`
        (gutter + 9, "quick brown"),  // mid-`brown`: still the whole word
        (gutter + 10, "quick brown"), // last cell of `brown`
    ] {
        drag_to(&mut harness, col, row);
        assert_eq!(
            selected_text(&harness, row),
            want,
            "dragging to column {} of the text should select {want:?}",
            col - gutter
        );
    }

    for (col, want) in [
        (gutter + 12, "quick brown fox"), // first cell of `fox`
        (gutter + 13, "quick brown fox"), // mid-`fox`
    ] {
        drag_to(&mut harness, col, row);
        assert_eq!(
            selected_text(&harness, row),
            want,
            "dragging to column {} of the text should select {want:?}",
            col - gutter
        );
    }

    release(&mut harness, gutter + 13, row);
    assert_eq!(
        selected_text(&harness, row),
        "quick brown fox",
        "releasing keeps the words the drag had grown to"
    );
}

/// The same cells under a plain single-click drag select one character at a
/// time. Without this the word-granular test above would still pass against
/// an editor that had lost the distinction and snapped *every* drag to word
/// boundaries.
#[test]
fn single_click_drag_extends_selection_a_character_at_a_time() {
    let (mut harness, row, gutter) = fixture();

    // One press only — no double-click, so nothing arms word granularity.
    press(&mut harness, gutter, row);

    for (col, want) in [
        (gutter + 2, "qu"),
        (gutter + 4, "quic"),
        (gutter + 9, "quick bro"),
    ] {
        drag_to(&mut harness, col, row);
        assert_eq!(
            selected_text(&harness, row),
            want,
            "a plain drag to column {} should select {want:?}",
            col - gutter
        );
    }

    release(&mut harness, gutter + 9, row);
}
