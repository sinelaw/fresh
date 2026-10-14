//! The pointer the editor draws for the Linux console mouse (GPM).
//!
//! GPM cannot draw its pointer over a full-screen program, so the editor
//! paints the cell under it. It used to reverse that cell's own colors, which
//! shows nothing where the two look alike — and a dialog's dimmed backdrop is
//! exactly that on the 16-color console, so the pointer vanished everywhere
//! behind a dialog and only reappeared over the dialog itself (#3517).

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use fresh::view::color_support::ColorCapability;

/// An editor as a Linux console sees it: GPM's pointer, 16 colors.
fn console_harness() -> EditorTestHarness {
    let mut h = EditorTestHarness::with_temp_project_and_config(80, 24, Config::default()).unwrap();
    h.editor_mut().set_session_mode(true);
    h.editor_mut().set_gpm_active(true);
    h.editor_mut()
        .set_color_capability(ColorCapability::Color16);
    h.render().unwrap();
    h
}

/// The pointer stands apart from the cells around it.
fn assert_pointer_visible(h: &EditorTestHarness, col: u16, row: u16, what: &str) {
    let at = h.get_cell_style(col, row).unwrap();
    let beside = h.get_cell_style(col + 1, row).unwrap();
    assert_ne!(
        at.fg, at.bg,
        "{what}: the pointer's text must show on its background ({at:?})"
    );
    assert_ne!(
        at.bg,
        beside.bg,
        "{what}: the pointer must stand apart from the cell beside it \
         ({at:?} vs {beside:?})\n{}",
        h.screen_to_string()
    );
}

#[test]
fn the_pointer_shows_over_the_buffer() {
    let mut h = console_harness();
    h.mouse_move(10, 8).unwrap();
    assert_pointer_visible(&h, 10, 8, "over the buffer");
}

#[test]
fn the_pointer_shows_over_a_dialogs_dimmed_backdrop() {
    let mut h = console_harness();
    h.send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
        .unwrap();
    assert!(
        h.screen_to_string().contains("Detach"),
        "Ctrl+Q in a session opens the quit dialog\n{}",
        h.screen_to_string()
    );
    // The bottom-left of the editor is behind the dialog, not in it.
    h.mouse_move(3, 20).unwrap();
    assert_pointer_visible(&h, 3, 20, "over the dimmed backdrop");
}
