//! Reproducer for #3329: a horizontal drag reverses after wheel scrolling.
//!
//! With the default three-row scroll margin, the first drag on the top
//! visible row scrolls the viewport upward. The next motion at the same
//! screen row then resolves against an earlier buffer line. The original
//! line's selected suffix becomes its prefix, with earlier lines also
//! selected. This is a viewport-dependent variant of the reported symptom.
//!
//! Manual reproduction (also checked in the installed 0.5.1 under tmux):
//! 1. Open a file containing 50 copies of `this is a text\n` with line wrap
//!    enabled and `editor.scroll_offset = 3` (the default).
//! 2. Wheel down until the first visible line is well past the file start.
//! 3. On that first visible row, press before `a`, drag to just before the
//!    final `t`, then move one more column right on the same screen row.
//!    Allow a frame between the two motions. The original line's selection
//!    changes from `a tex` to `this is `; the viewport scrolls at each motion.
//!
//! Run the intentionally failing reproducer with:
//! cargo test -p fresh-editor --test all_tests issue_3329_repro -- --include-ignored --nocapture

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyModifiers, MouseButton, MouseEvent, MouseEventKind};
use fresh::config::Config;

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

/// Read the highlighted cells, including the visible selected-space marker.
fn selected_text(harness: &EditorTestHarness, row: u16) -> String {
    let bg = harness.editor().theme().selection_bg;
    (0..harness.buffer().area.width)
        .filter(|&col| harness.get_cell_style(col, row).and_then(|s| s.bg) == Some(bg))
        .filter_map(|col| harness.get_cell(col, row))
        .collect::<String>()
        .replace('·', " ")
}

/// Locate the original line by its rendered gutter number after scrolling.
fn selected_on_line(harness: &EditorTestHarness, line_label: &str) -> String {
    let row = (0..harness.buffer().area.height)
        .find(|&row| {
            harness
                .screen_row_text(row)
                .split_once('│')
                .is_some_and(|(gutter, _)| gutter.trim() == line_label)
        })
        .expect("the original line must remain visible");
    selected_text(harness, row)
}

fn drag_to_eol(wheel_scrolls: usize, scroll_offset: usize) {
    let mut config = Config::default();
    config.editor.line_wrap = true;
    config.editor.scroll_offset = scroll_offset;
    let mut harness = EditorTestHarness::with_config(80, 12, config).unwrap();
    let _fixture = harness
        .load_buffer_from_text(&"this is a text\n".repeat(50))
        .unwrap();
    harness.render().unwrap();

    for _ in 0..wheel_scrolls {
        harness.mouse_scroll_down(30, 5).unwrap();
    }
    harness.render().unwrap();

    let (col, row) = harness.find_text_on_screen("this is a text").unwrap();
    let original_row = harness.screen_row_text(row);
    let line_label = original_row.split_once('│').unwrap().0.trim();
    let before = harness.screen_to_string();

    // Keep the pointer on one screen row, as in an ordinary horizontal drag.
    mouse(
        &mut harness,
        MouseEventKind::Down(MouseButton::Left),
        col + 8,
        row,
    );
    mouse(
        &mut harness,
        MouseEventKind::Drag(MouseButton::Left),
        col + 13,
        row,
    );
    assert_eq!(selected_on_line(&harness, line_label), "a tex");
    let mid = harness.screen_to_string();

    mouse(
        &mut harness,
        MouseEventKind::Drag(MouseButton::Left),
        col + 14,
        row,
    );
    mouse(
        &mut harness,
        MouseEventKind::Up(MouseButton::Left),
        col + 14,
        row,
    );
    assert_eq!(
        selected_on_line(&harness, line_label),
        "a text",
        "dragging right to EOL must keep selecting the suffix on line {line_label}; \
         wheel_scrolls={wheel_scrolls}, scroll_offset={scroll_offset}\n\
         Before:\n{before}\nMid-drag:\n{mid}\nAfter:\n{}",
        harness.screen_to_string(),
    );
}

#[test]
fn drag_to_eol_without_wheel_scrolling() {
    drag_to_eol(0, 3);
}

#[test]
fn drag_to_eol_after_wheel_scrolling_without_scroll_margin() {
    drag_to_eol(5, 0);
}

#[test]
#[ignore = "#3329 reproducer: horizontal drag selects the prefix after the viewport scrolls"]
fn drag_to_eol_after_wheel_scrolling_reverses_selection() {
    drag_to_eol(5, 3);
}
