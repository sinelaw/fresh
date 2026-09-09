//! Regression: the focused file explorer's hardware caret must not blink
//! through a modal drawn over the sidebar.
//!
//! The explorer paints at the top of the frame and parks the terminal's
//! hardware cursor on its selected row for terminal-native blinking. The
//! terminal draws that caret on top of *every* cell, so a modal painted
//! later in the same frame (the settings dialog, which dims the whole UI
//! behind it) cannot cover it by painting cells — the caret has to be
//! withheld for the frame instead. Before the fix the caret stayed on the
//! sidebar row and showed through the settings dialog.

use crate::common::harness::EditorTestHarness;

/// The explorer owns the caret while it's focused, and gives it up while the
/// settings modal is open.
#[test]
fn test_explorer_caret_hidden_while_settings_modal_is_open() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let project_root = harness.project_dir().unwrap();
    std::fs::write(project_root.join("a.txt"), "hello").unwrap();

    // Open and focus the file explorer.
    harness.editor_mut().focus_file_explorer();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("a.txt").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("File explorer"))
        .unwrap();

    // Baseline: the focused explorer parks the caret on its selected row.
    // Without this the assertion below would pass vacuously.
    let caret = harness
        .render_observing_cursor()
        .unwrap()
        .expect("focused file explorer should own the hardware caret");

    // Open settings over it. The explorer stays focused underneath (settings
    // is a modal; it doesn't change the window's key context), so the caret
    // has to be suppressed on account of the modal itself. The selected row
    // doesn't move while settings is up, so the explorer's caret cell is
    // still `caret` — the frame must not put the hardware cursor there.
    harness.open_settings().unwrap();

    let caret_with_settings = harness.render_observing_cursor().unwrap();
    assert_ne!(
        caret_with_settings,
        Some(caret),
        "the file explorer's caret is still parked on its selected row \
         {caret:?} with the settings modal open — it blinks through the dialog"
    );
}
