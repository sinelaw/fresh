//! A caret at the end of the buffer stays on its own line when a virtual
//! line is drawn below it.
//!
//! Reported against markdown compose: in an empty `bla.md`, type `- a` and
//! press backspace. The caret jumps two rows down and to the left edge, and
//! typing brings it back. Compose hangs a virtual line under a list block,
//! which is the whole of it — a heading, which gets none, does not do this.
//!
//! The renderer keeps `last_line_end` — the buffer's last *content* row and
//! whether it ended with a newline — and, when it did, draws the implicit
//! empty line that follows the buffer's final newline, which is where a
//! caret at the buffer end belongs. A plugin's virtual row was overwriting
//! that: it carries an injected newline, so a buffer with no trailing
//! newline at all looked newline-terminated, an implicit empty row was drawn
//! that no line of the document owns, and it took the caret.
//!
//! It only showed once the content row stopped claiming the caret itself,
//! which is what deleting the `a` does: the row then ends on the list
//! marker's space, and the position past a trailing space is not a cell the
//! row draws (`row_end_exclusive` — the caret there belongs to the
//! end-of-buffer resolution, not to a cell).
//!
//! Driven through the virtual-line seed rather than the markdown plugin: the
//! virtual row is the mechanism, and seeded it stays put instead of being
//! rebuilt a frame later, which is what made this look like a flicker.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::test_api::VirtualLineSpec;
use tempfile::TempDir;

/// The buffer: one list line, ending in a space once its `a` is deleted, and
/// no trailing newline — a file you have just started typing into.
const LINE: &str = "- a";

fn harness_with_a_virtual_line_below() -> (TempDir, EditorTestHarness) {
    let temp_dir = TempDir::new().unwrap();
    let file = temp_dir.path().join("bla.md");
    std::fs::write(&file, LINE).unwrap();

    let mut harness = EditorTestHarness::create(100, 24, HarnessOptions::new()).unwrap();
    harness.open_file(&file).unwrap();
    // What markdown compose hangs under a list block.
    harness.api_mut().seed_virtual_line(VirtualLineSpec {
        byte_offset: 0,
        text: "",
        fg: None,
        bg: None,
        placement: "below",
        namespace: "test-virtual",
        priority: 0,
    });
    harness.render().unwrap();
    (temp_dir, harness)
}

#[test]
fn the_caret_stays_on_its_line_after_deleting_the_last_character() {
    let (_temp_dir, mut harness) = harness_with_a_virtual_line_below();

    let (col, row) = harness
        .find_text_on_screen(LINE)
        .expect("the list line is on screen");

    // To the end of the line, then delete the `a`, leaving `- `.
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();

    let caret = harness
        .render_observing_cursor()
        .unwrap()
        .expect("the editor draws a caret");

    assert_eq!(
        caret,
        (col + 2, row),
        "the caret belongs just past `- `, on the line being edited — not on \
         the implicit empty row below the virtual line\n{}",
        harness.screen_to_string()
    );
}

/// The control: with the `a` still there the caret is claimed by the cell it
/// sits past, so the same buffer and the same virtual line place it right.
/// Without this, the assertion above could pass for a renderer that never
/// draws a caret at the end of a line at all.
#[test]
fn the_caret_is_at_the_end_of_the_line_before_the_delete() {
    let (_temp_dir, mut harness) = harness_with_a_virtual_line_below();

    let (col, row) = harness
        .find_text_on_screen(LINE)
        .expect("the list line is on screen");

    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();

    let caret = harness
        .render_observing_cursor()
        .unwrap()
        .expect("the editor draws a caret");

    assert_eq!(
        caret,
        (col + LINE.len() as u16, row),
        "the caret sits one cell past the line's last character\n{}",
        harness.screen_to_string()
    );
}
