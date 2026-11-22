//! Opening right-click menus and choosing their items the way a user does:
//! by clicking where the text is drawn.

use crate::common::harness::{layout, EditorTestHarness};

/// Right-click `name` where the file explorer draws it: the leftmost place
/// it is drawn below the tab bar.
pub fn right_click_explorer_entry(harness: &mut EditorTestHarness, name: &str) {
    let height = harness.terminal_height() as u16;
    let (col, row) = (layout::TAB_BAR_ROW as u16 + 1..height)
        .filter_map(|row| {
            let line = harness.screen_row_text(row);
            let byte = line.find(name)?;
            Some((line[..byte].chars().count() as u16, row))
        })
        .min()
        .unwrap_or_else(|| panic!("{name} not on screen:\n{}", harness.screen_to_string()));
    harness.mouse_right_click(col + 1, row).unwrap();
    harness.render().unwrap();
}

/// Right-click the tab whose title contains `name`.
pub fn right_click_tab(harness: &mut EditorTestHarness, name: &str) {
    let bar = harness.get_tab_bar();
    let byte = bar
        .find(name)
        .unwrap_or_else(|| panic!("{name} not in the tab bar: {bar}"));
    let col = bar[..byte].chars().count() as u16;
    harness
        .mouse_right_click(col + 1, layout::TAB_BAR_ROW as u16)
        .unwrap();
    harness.render().unwrap();
}

/// Click `text` where it is drawn, e.g. a menu row.
pub fn click_text(harness: &mut EditorTestHarness, text: &str) {
    let (col, row) = harness
        .find_text_on_screen(text)
        .unwrap_or_else(|| panic!("{text} not on screen:\n{}", harness.screen_to_string()));
    harness.mouse_click(col + 1, row).unwrap();
    harness.render().unwrap();
}
