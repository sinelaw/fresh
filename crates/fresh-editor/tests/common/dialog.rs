//! Helpers for driving a Settings dialog by its field labels.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// The screen row containing `needle` (empty when none does).
pub fn row_with(harness: &EditorTestHarness, needle: &str) -> String {
    harness
        .screen_to_string()
        .lines()
        .find(|line| line.contains(needle))
        .unwrap_or("")
        .to_string()
}

/// True when the row showing `label` carries the dialog's focus marker
/// (`>`), between the dialog's border and the label.
pub fn row_is_focused(harness: &EditorTestHarness, label: &str) -> bool {
    let row = row_with(harness, label);
    let before_label = row.split(label).next().unwrap_or("");
    before_label.rsplit('│').next().unwrap_or("").contains('>')
}

/// Walk the dialog's field focus down until `label`'s row is the focused
/// one. Walked to rather than counted: list fields take a focus stop per
/// row, and a field far down a tall dialog scrolls into view on the way.
pub fn focus_field(harness: &mut EditorTestHarness, label: &str) {
    for _ in 0..80 {
        if row_is_focused(harness, label) {
            return;
        }
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }
    panic!(
        "could not focus the {label:?} field; screen was:\n{}",
        harness.screen_to_string()
    );
}
