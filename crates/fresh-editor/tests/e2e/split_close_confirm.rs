//! Clicking a split's `×` (close-split) button must not close the split
//! immediately — it pops a "Close split" / "Cancel" confirmation first
//! (fresh#2768). Only confirming closes the split; cancelling leaves both
//! panes in place.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Create a vertical split via the command palette (two side-by-side panes).
fn split_vertical(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("split vert").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// Number of split leaves in the active window.
fn leaf_count(harness: &EditorTestHarness) -> usize {
    harness
        .editor()
        .active_window()
        .buffers
        .split_manager()
        .expect("split layout")
        .root()
        .count_leaves()
}

/// Locate the left pane's close-split `×` button by finding the cluster's
/// maximize glyph `□`. The cluster is laid out `[gap] □ + [sep] > ×`, so the
/// `×` sits four columns to the right of the `□` (fresh#2768).
fn close_button_pos(harness: &EditorTestHarness) -> (u16, u16) {
    let height = harness.buffer().area.height;
    for row in 0..height {
        let text = harness.get_row_text(row);
        if let Some(idx) = text.find('□') {
            // `idx` is a byte offset; count chars up to it to get the column
            // (everything left of the cluster is ASCII, so this equals the
            // display column).
            let col = text[..idx].chars().count() as u16;
            return (col + 4, row);
        }
    }
    panic!("could not find the maximize glyph (□) in any row");
}

#[test]
fn clicking_close_split_pops_confirmation_not_immediate_close() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();
    split_vertical(&mut harness);
    assert_eq!(leaf_count(&harness), 2, "precondition: two split panes");

    let (x, row) = close_button_pos(&harness);
    harness.mouse_click(x, row).unwrap();
    harness.render().unwrap();

    // The confirmation popup is presented and the split is still open.
    assert!(
        harness.editor().active_window().close_split_menu.is_some(),
        "clicking × must present the close-split confirmation"
    );
    assert_eq!(
        leaf_count(&harness),
        2,
        "the split must not close until the user confirms"
    );
    harness.assert_screen_contains("Close split");
    harness.assert_screen_contains("Cancel");
}

#[test]
fn confirming_close_split_closes_the_pane() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();
    split_vertical(&mut harness);
    assert_eq!(leaf_count(&harness), 2);

    let (x, row) = close_button_pos(&harness);
    harness.mouse_click(x, row).unwrap();
    harness.render().unwrap();
    assert!(harness.editor().active_window().close_split_menu.is_some());

    // The confirm item ("Close split") is highlighted by default; Enter
    // activates it through the shared context-menu key path.
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    assert!(
        harness.editor().active_window().close_split_menu.is_none(),
        "the confirmation dismisses after choosing an item"
    );
    assert_eq!(leaf_count(&harness), 1, "confirming must close the split");
}

#[test]
fn cancelling_close_split_keeps_both_panes() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();
    split_vertical(&mut harness);
    assert_eq!(leaf_count(&harness), 2);

    let (x, row) = close_button_pos(&harness);
    harness.mouse_click(x, row).unwrap();
    harness.render().unwrap();
    assert!(harness.editor().active_window().close_split_menu.is_some());

    // Esc dismisses the confirmation without closing anything.
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    assert!(
        harness.editor().active_window().close_split_menu.is_none(),
        "Esc must dismiss the confirmation"
    );
    assert_eq!(
        leaf_count(&harness),
        2,
        "cancelling must leave both panes open"
    );
}
