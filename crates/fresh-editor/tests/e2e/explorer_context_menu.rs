use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

// ── coordinate helpers ───────────────────────────────────────────────────────
//
// Terminal: 100 × 30.  Default explorer width = 30 % × 100 = 30 cols.
// Layout (rows):
//   0      – menu bar
//   1–28   – main content (explorer left, editor right)
//   29     – status bar
//
// Explorer area: x = 0, width = 30, y = 1, height = 27.
//   Row 1 is the title bar (skipped for content clicks).
//   Content rows start at 2.
//
// A safe right-click inside the explorer content area:
const EXPLORER_COL: u16 = 10;
// Past the last entry of a small fixture, so it is the blank area: the root's.
const EXPLORER_ROW: u16 = 5;
// Row 2 is the project root, row 3 the first child under it.
const ENTRY_ROW: u16 = 3;

// The "Paste" item is present in every mode of the context menu (single,
// multi-selection, root).  Matching on " Paste " — with surrounding
// whitespace so it does NOT collide with status messages like "Pasted:" —
// is a reliable observe-only signal for "menu is visible on screen".
fn context_menu_visible(h: &EditorTestHarness) -> bool {
    h.screen_to_string().contains(" Paste ")
}

// ── open helper ──────────────────────────────────────────────────────────────

fn harness_with_explorer() -> EditorTestHarness {
    let mut h = EditorTestHarness::with_temp_project(100, 30).unwrap();
    h.editor_mut().focus_file_explorer();
    h.wait_for_file_explorer().unwrap();
    h.render().unwrap(); // populate cached_layout.file_explorer_area
    h
}

/// The same, with one file in it, so [`ENTRY_ROW`] is an entry: the bare project
/// has only the root row and everything below it is the blank area.
fn harness_with_entry() -> EditorTestHarness {
    let mut h = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let root = h.project_dir().unwrap();
    fs::write(root.join("entry.txt"), "data").unwrap();
    h.editor_mut().focus_file_explorer();
    h.wait_for_file_explorer().unwrap();
    h.wait_for_file_explorer_item("entry.txt").unwrap();
    h.render().unwrap();
    h
}

// ── menu open / close ────────────────────────────────────────────────────────

/// Right-clicking inside the file explorer opens the context menu.
#[test]
fn test_right_click_opens_context_menu() {
    let mut h = harness_with_explorer();

    assert!(!context_menu_visible(&h));

    h.mouse_right_click(EXPLORER_COL, EXPLORER_ROW).unwrap();

    assert!(
        context_menu_visible(&h),
        "Context menu should be open after right-click in file explorer"
    );
}

/// The context menu shows all expected items.
#[test]
fn test_context_menu_shows_all_items() {
    let mut h = harness_with_entry();
    h.mouse_right_click(EXPLORER_COL, ENTRY_ROW).unwrap();

    h.assert_screen_contains("New File");
    h.assert_screen_contains("New Directory");
    h.assert_screen_contains("Rename");
    h.assert_screen_contains("Cut");
    h.assert_screen_contains("Copy");
    h.assert_screen_contains("Paste");
    h.assert_screen_contains("Delete");
}

/// Right-clicking outside the explorer (in the editor area) closes the menu.
#[test]
fn test_right_click_outside_closes_menu() {
    let mut h = harness_with_explorer();
    h.mouse_right_click(EXPLORER_COL, EXPLORER_ROW).unwrap();
    assert!(context_menu_visible(&h));

    // Right-click in the editor area (right of the explorer)
    h.mouse_right_click(60, 10).unwrap();

    assert!(
        !context_menu_visible(&h),
        "Context menu should be closed after right-click outside the explorer"
    );
}

/// Left-clicking outside the context menu closes it.
#[test]
fn test_left_click_outside_closes_menu() {
    let mut h = harness_with_explorer();
    h.mouse_right_click(EXPLORER_COL, EXPLORER_ROW).unwrap();
    assert!(context_menu_visible(&h));

    // Left-click somewhere outside the menu
    h.mouse_click(60, 10).unwrap();

    assert!(
        !context_menu_visible(&h),
        "Context menu should be closed after left-click outside"
    );
}

/// Right-clicking in the explorer title row does NOT open the context menu.
#[test]
fn test_right_click_title_row_no_menu() {
    let mut h = harness_with_explorer();
    // Row 1 is the title / header row — content check skips it.
    h.mouse_right_click(EXPLORER_COL, 1).unwrap();

    assert!(
        !context_menu_visible(&h),
        "Right-clicking the title row should not open the context menu"
    );
}

/// When the explorer is not open, right-clicking at the same position does not
/// produce a context menu.
#[test]
fn test_no_menu_when_explorer_closed() {
    let mut h = EditorTestHarness::with_temp_project(100, 30).unwrap();
    // Explorer is not open; focus_file_explorer is NOT called.
    h.render().unwrap();

    h.mouse_right_click(EXPLORER_COL, EXPLORER_ROW).unwrap();

    assert!(
        !context_menu_visible(&h),
        "Context menu must not open when file explorer is not visible"
    );
}

// ── node selection on right-click ────────────────────────────────────────────

/// Right-clicking on a file node selects it (cursor moves to it).
#[test]
fn test_right_click_selects_node() {
    let mut h = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let root = h.project_dir().unwrap();
    fs::write(root.join("target.txt"), "data").unwrap();

    h.editor_mut().focus_file_explorer();
    h.wait_for_file_explorer().unwrap();
    h.wait_for_file_explorer_item("target.txt").unwrap();

    // The file should appear at content row 3 (row 1 = title, row 2 = root
    // node, row 3 = first child).  Right-click it.
    h.mouse_right_click(EXPLORER_COL, 3).unwrap();

    // The context menu opens (confirming a node was found at that row).
    assert!(
        context_menu_visible(&h),
        "Right-clicking a file node should open the context menu"
    );
}

// ── Copy via context menu ─────────────────────────────────────────────────────

/// Clicking "Copy" in the context menu copies the selected file.
#[test]
fn test_context_menu_copy_action() {
    let mut h = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let root = h.project_dir().unwrap();
    fs::write(root.join("file_to_copy.txt"), "content").unwrap();

    h.editor_mut().focus_file_explorer();
    h.wait_for_file_explorer().unwrap();
    h.wait_for_file_explorer_item("file_to_copy").unwrap();

    // Right-click selects the node and opens the context menu.
    // file_to_copy.txt is at content row index 1 → screen row 3.
    let file_row = 3u16;
    h.mouse_right_click(EXPLORER_COL, file_row).unwrap();

    // Menu opens at (EXPLORER_COL, file_row + 1) = (10, 4).
    // Copy is item index 4: border(4) + 1 + 4 = row 9.
    let menu_y = file_row + 1;
    let copy_row = menu_y + 1 + 4;
    h.mouse_click(EXPLORER_COL + 2, copy_row).unwrap();

    h.assert_screen_contains("Copied:");
    h.assert_screen_contains("file_to_copy.txt");
}

// ── Cut via context menu ──────────────────────────────────────────────────────

/// Clicking "Cut" in the context menu marks the file for cut.
#[test]
fn test_context_menu_cut_action() {
    let mut h = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let root = h.project_dir().unwrap();
    fs::write(root.join("file_to_cut.txt"), "content").unwrap();

    h.editor_mut().focus_file_explorer();
    h.wait_for_file_explorer().unwrap();
    h.wait_for_file_explorer_item("file_to_cut").unwrap();

    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();

    // Right-click to open context menu, then click Cut (4th item, index 3).
    // menu opens at (EXPLORER_COL, row + 1); with border, Cut is at menu_y + 1 + 3.
    // EXPLORER_ROW + 1 = menu_y; Cut row = menu_y + 4.
    let menu_y = 3 + 1u16; // right-click row 3, menu at row+1
    let cut_row = menu_y + 1 + 3; // border row + 3 items before Cut

    h.mouse_right_click(EXPLORER_COL, 3).unwrap();
    h.mouse_click(EXPLORER_COL + 2, cut_row).unwrap();

    h.assert_screen_contains("Marked for cut:");
    h.assert_screen_contains("file_to_cut.txt");
}

// ── New File via context menu ─────────────────────────────────────────────────

/// Clicking "New File" in the context menu creates a file (enters rename mode).
#[test]
fn test_context_menu_new_file_action() {
    let mut h = harness_with_explorer();
    let root = h.project_dir().unwrap();
    let initial_count = fs::read_dir(&root).unwrap().count();

    // New File is the first item (index 0): menu_y + 1 + 0 = menu_y + 1.
    let menu_y = EXPLORER_ROW + 1;
    let new_file_row = menu_y + 1;

    h.mouse_right_click(EXPLORER_COL, EXPLORER_ROW).unwrap();
    h.mouse_click(EXPLORER_COL + 2, new_file_row).unwrap();

    h.wait_for_prompt().unwrap();
    // The prompt opens empty — nothing is created until it is named — so the
    // name has to be typed where this used to accept a generated one.
    h.type_text("from_context_menu.txt").unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_for_prompt_closed().unwrap();

    h.wait_until(|_| fs::read_dir(&root).unwrap().count() > initial_count)
        .unwrap();
}

// ── New Directory via context menu ────────────────────────────────────────────

/// Clicking "New Directory" in the context menu creates a directory.
#[test]
fn test_context_menu_new_directory_action() {
    let mut h = harness_with_explorer();
    let root = h.project_dir().unwrap();
    let initial_dirs = fs::read_dir(&root)
        .unwrap()
        .filter_map(|e| e.ok())
        .filter(|e| e.path().is_dir())
        .count();

    // New Directory is item index 1: menu_y + 1 + 1 = menu_y + 2.
    let menu_y = EXPLORER_ROW + 1;
    let new_dir_row = menu_y + 2;

    h.mouse_right_click(EXPLORER_COL, EXPLORER_ROW).unwrap();
    h.mouse_click(EXPLORER_COL + 2, new_dir_row).unwrap();

    h.wait_for_prompt().unwrap();
    // The prompt opens empty — nothing is created until it is named — so the
    // name has to be typed where this used to accept a generated one.
    h.type_text("from_context_menu").unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_for_prompt_closed().unwrap();

    let final_dirs = fs::read_dir(&root)
        .unwrap()
        .filter_map(|e| e.ok())
        .filter(|e| e.path().is_dir())
        .count();

    assert!(
        final_dirs > initial_dirs,
        "A new directory should have been created via context menu"
    );
}

// ── Delete via context menu ───────────────────────────────────────────────────

/// Clicking "Delete" in the context menu triggers the delete confirmation.
#[test]
fn test_context_menu_delete_action() {
    let mut h = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let root = h.project_dir().unwrap();
    fs::write(root.join("to_delete.txt"), "bye").unwrap();

    h.editor_mut().focus_file_explorer();
    h.wait_for_file_explorer().unwrap();
    h.wait_for_file_explorer_item("to_delete").unwrap();

    // Navigate to the file (root → to_delete.txt at row 3).
    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();

    // Delete is item index 6: menu_y + 1 + 6 = menu_y + 7.
    let menu_y = 3 + 1u16;
    let delete_row = menu_y + 1 + 6;

    h.mouse_right_click(EXPLORER_COL, 3).unwrap();
    h.mouse_click(EXPLORER_COL + 2, delete_row).unwrap();

    // Should show delete confirmation prompt.
    h.wait_for_prompt().unwrap();
    let screen = h.screen_to_string();
    assert!(
        screen.contains("Delete") || screen.contains("delete"),
        "Delete confirmation prompt should appear. Screen:\n{}",
        screen
    );

    // Cancel the deletion via Esc (drives through the keyboard, not internal
    // prompt state).  ConfirmDeleteFile only deletes on "y"/"yes" input, so
    // Esc dismissing the prompt is sufficient to cancel.
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    h.wait_for_prompt_closed().unwrap();

    assert!(
        root.join("to_delete.txt").exists(),
        "File should still exist after cancelling delete"
    );
}

// ── Rename via context menu ───────────────────────────────────────────────────

/// Clicking "Rename" in the context menu triggers the rename prompt.
#[test]
fn test_context_menu_rename_action() {
    let mut h = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let root = h.project_dir().unwrap();
    fs::write(root.join("to_rename.txt"), "content").unwrap();

    h.editor_mut().focus_file_explorer();
    h.wait_for_file_explorer().unwrap();
    h.wait_for_file_explorer_item("to_rename").unwrap();

    // Navigate to the file.
    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();

    // Rename is item index 2: menu_y + 1 + 2 = menu_y + 3.
    let menu_y = 3 + 1u16;
    let rename_row = menu_y + 1 + 2;

    h.mouse_right_click(EXPLORER_COL, 3).unwrap();
    h.mouse_click(EXPLORER_COL + 2, rename_row).unwrap();

    // Should show the rename prompt with its "Rename to:" label.
    h.wait_for_prompt().unwrap();
    h.assert_screen_contains("Rename to");

    // Cancel.
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    h.wait_for_prompt_closed().unwrap();
}

// ── Paste via context menu ────────────────────────────────────────────────────

/// Clicking "Paste" with an empty clipboard shows the "nothing to paste" message.
#[test]
fn test_context_menu_paste_empty_clipboard() {
    let mut h = harness_with_entry();

    // Paste is item index 5 of an entry's menu: menu_y + 1 + 5 = menu_y + 6.
    let menu_y = ENTRY_ROW + 1;
    let paste_row = menu_y + 1 + 5;

    h.mouse_right_click(EXPLORER_COL, ENTRY_ROW).unwrap();
    h.mouse_click(EXPLORER_COL + 2, paste_row).unwrap();

    let screen = h.screen_to_string();
    assert!(
        screen.contains("Nothing to paste") || screen.contains("paste"),
        "Should show 'nothing to paste'. Screen:\n{}",
        screen
    );
}

// ── keyboard navigation ──────────────────────────────────────────────────────

/// Pressing Escape closes the context menu.
#[test]
fn test_keyboard_escape_closes_menu() {
    let mut h = harness_with_explorer();
    h.mouse_right_click(EXPLORER_COL, EXPLORER_ROW).unwrap();
    assert!(context_menu_visible(&h));

    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();

    assert!(
        !context_menu_visible(&h),
        "Escape should close the context menu"
    );
}

/// Pressing Down then Enter executes the second menu item (New Directory).
#[test]
fn test_keyboard_down_enter_executes_item() {
    let mut h = harness_with_explorer();
    let root = h.project_dir().unwrap();
    let initial_dirs = fs::read_dir(&root)
        .unwrap()
        .filter_map(|e| e.ok())
        .filter(|e| e.path().is_dir())
        .count();

    h.mouse_right_click(EXPLORER_COL, EXPLORER_ROW).unwrap();
    assert!(context_menu_visible(&h));

    // Down moves from index 0 (New File) to index 1 (New Directory).
    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    // Enter activates New Directory, which shows a prompt.
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();

    assert!(!context_menu_visible(&h), "Menu should close after Enter");

    h.wait_for_prompt().unwrap();
    // The prompt opens empty — nothing is created until it is named — so the
    // name has to be typed where this used to accept a generated one.
    h.type_text("from_keyboard").unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_for_prompt_closed().unwrap();

    let final_dirs = fs::read_dir(&root)
        .unwrap()
        .filter_map(|e| e.ok())
        .filter(|e| e.path().is_dir())
        .count();
    assert!(
        final_dirs > initial_dirs,
        "New Directory should have been created via keyboard navigation"
    );
}

/// Which item the open menu highlights, and how many it has. Reading both off
/// the menu is what makes the wrap tests below about wrapping rather than about
/// an item count that goes stale as items are appended.
fn menu_highlight(h: &EditorTestHarness) -> (usize, usize) {
    let menu = h
        .editor()
        .active_window()
        .file_explorer_context_menu
        .as_ref()
        .expect("an open file-explorer context menu");
    (menu.menu.highlighted, menu.menu.item_count)
}

/// Up key wraps from the first item to the last.
#[test]
fn test_keyboard_up_wraps() {
    // An entry, for the full menu: `EXPLORER_ROW` is the blank area here.
    let mut h = harness_with_entry();
    h.mouse_right_click(EXPLORER_COL, ENTRY_ROW).unwrap();
    let (highlighted, items) = menu_highlight(&h);
    assert_eq!(highlighted, 0, "a fresh menu highlights its first item");
    assert!(items > 3, "the entry menu, not the root's: {items} items");

    h.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    assert_eq!(menu_highlight(&h), (items - 1, items));
    assert!(
        context_menu_visible(&h),
        "Menu should remain open after Up key"
    );
}

/// Down key wraps from the last item back to the first.
#[test]
fn test_keyboard_down_wraps() {
    let mut h = harness_with_entry();
    h.mouse_right_click(EXPLORER_COL, ENTRY_ROW).unwrap();
    let (_, items) = menu_highlight(&h);

    for _ in 0..items - 1 {
        h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    assert_eq!(menu_highlight(&h), (items - 1, items));

    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    assert_eq!(menu_highlight(&h), (0, items));
    assert!(
        context_menu_visible(&h),
        "Menu should remain open after Down wraps around"
    );
}

// ── keyboard grab (#2587) ─────────────────────────────────────────────────────

/// While the context menu is open it grabs the keyboard: printable keys must
/// not leak into the tree's type-ahead find underneath. Without the grab,
/// typing activates the explorer search — the title renders ` /<query> ` —
/// and silently retargets the selection the menu's actions operate on.
#[test]
fn test_context_menu_grabs_keyboard_printable_keys() {
    let mut h = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let root = h.project_dir().unwrap();
    fs::write(root.join("alpha.txt"), "a").unwrap();
    fs::create_dir(root.join("subdir")).unwrap();

    h.editor_mut().focus_file_explorer();
    h.wait_for_file_explorer().unwrap();
    h.wait_for_file_explorer_item("alpha.txt").unwrap();

    // Open the context menu on a file row.
    h.mouse_right_click(EXPLORER_COL, 3).unwrap();
    assert!(context_menu_visible(&h), "context menu should open");

    // Type printable characters while the menu is open.
    for c in ['s', 'u', 'b'] {
        h.send_key(KeyCode::Char(c), KeyModifiers::NONE).unwrap();
    }
    h.render().unwrap();

    let screen = h.screen_to_string();
    // The type-ahead find renders its query as ` /<query> ` in the explorer
    // title — it must be absent, since the keys were aimed at the open menu.
    assert!(
        !screen.contains(" /sub"),
        "typing while the context menu is open must not activate the explorer \
         type-ahead find. Screen:\n{}",
        screen
    );
    // The menu is still up and usable.
    assert!(
        context_menu_visible(&h),
        "context menu should stay open after printable keys. Screen:\n{}",
        screen
    );
}

// ── hover highlight ──────────────────────────────────────────────────────────

/// Hovering over context menu items updates the highlighted item without
/// closing the menu.
#[test]
fn test_context_menu_hover_stays_open() {
    let mut h = harness_with_explorer();
    h.mouse_right_click(EXPLORER_COL, EXPLORER_ROW).unwrap();
    assert!(context_menu_visible(&h));

    // Hover over the second item (New Directory).
    let menu_y = EXPLORER_ROW + 1;
    let new_dir_item_row = menu_y + 1 + 1; // border + index 1
    h.mouse_move(EXPLORER_COL + 2, new_dir_item_row).unwrap();

    assert!(
        context_menu_visible(&h),
        "Context menu should remain open while hovering over items"
    );
}

// ── a second right-click ─────────────────────────────────────────────────────

/// A press inside the open menu is the menu's, so the menu stays up.
///
/// This test used to claim it proved the opposite — that a second right-click
/// "closes then reopens at the new position" — while clicking two rows down,
/// which is *inside* the box the first click opened. It passed either way and
/// so said nothing. What the second press actually does is covered below.
#[test]
fn test_a_right_click_inside_the_menu_leaves_it_open() {
    let mut h = harness_with_explorer();

    h.mouse_right_click(EXPLORER_COL, EXPLORER_ROW).unwrap();
    assert!(context_menu_visible(&h));

    // The menu is anchored just below the press, so two rows down is in it.
    h.mouse_right_click(EXPLORER_COL, EXPLORER_ROW + 2).unwrap();
    assert!(
        context_menu_visible(&h),
        "a press inside the menu should not dismiss it"
    );
}

/// A press outside the menu dismisses it, and is spent doing so.
///
/// Deliberate, and the library says why: "A click outside a menu is spent
/// closing the menu: that *is* the gesture, and the menu was in the way of
/// it" (`fresh_ui::Dismiss::pass_through`). So the press does not also open a
/// menu where it landed, the way a desktop file manager would — noted here
/// because it reads as a missing feature until you find that sentence.
#[test]
fn test_a_right_click_outside_the_menu_only_dismisses_it() {
    let mut h = harness_with_explorer();

    h.mouse_right_click(EXPLORER_COL, EXPLORER_ROW + 1).unwrap();
    assert!(context_menu_visible(&h));

    // The menu hangs below its anchor, so the row above the anchor is outside.
    h.mouse_right_click(EXPLORER_COL, EXPLORER_ROW).unwrap();
    assert!(
        !context_menu_visible(&h),
        "a press outside the menu should dismiss it"
    );
}

// ── multi-selection context menu ─────────────────────────────────────────────

/// When multiple files are selected (Space to toggle), the context menu only
/// shows Cut, Copy, Paste, Delete — not New File, New Directory, or Rename.
#[test]
fn test_multi_selection_hides_create_and_rename() {
    let mut h = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let root = h.project_dir().unwrap();
    fs::write(root.join("alpha.txt"), "a").unwrap();
    fs::write(root.join("beta.txt"), "b").unwrap();

    h.editor_mut().focus_file_explorer();
    h.wait_for_file_explorer().unwrap();
    h.wait_for_file_explorer_item("alpha.txt").unwrap();

    // Navigate to alpha.txt (row 2 = root node, row 3 = first file).
    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    // Space toggles the current node into multi-selection.
    h.send_key(KeyCode::Char(' '), KeyModifiers::NONE).unwrap();
    h.render().unwrap();

    h.mouse_right_click(EXPLORER_COL, 3).unwrap();

    assert!(
        context_menu_visible(&h),
        "Context menu should open in multi-selection mode"
    );

    let screen = h.screen_to_string();
    assert!(
        !screen.contains("New File"),
        "New File should be hidden in multi-selection mode. Screen:\n{}",
        screen
    );
    assert!(
        !screen.contains("New Directory"),
        "New Directory should be hidden in multi-selection mode. Screen:\n{}",
        screen
    );
    assert!(
        !screen.contains("Rename"),
        "Rename should be hidden in multi-selection mode. Screen:\n{}",
        screen
    );
    assert!(
        screen.contains("Cut"),
        "Cut should be visible. Screen:\n{}",
        screen
    );
    assert!(
        screen.contains("Copy"),
        "Copy should be visible. Screen:\n{}",
        screen
    );
    assert!(
        screen.contains("Paste"),
        "Paste should be visible. Screen:\n{}",
        screen
    );
    assert!(
        screen.contains("Delete"),
        "Delete should be visible. Screen:\n{}",
        screen
    );
}

/// Ctrl+A selects all nodes; the context menu then uses the multi-selection layout.
#[test]
fn test_select_all_triggers_multi_selection_menu() {
    let mut h = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let root = h.project_dir().unwrap();
    fs::write(root.join("one.txt"), "1").unwrap();
    fs::write(root.join("two.txt"), "2").unwrap();

    h.editor_mut().focus_file_explorer();
    h.wait_for_file_explorer().unwrap();
    h.wait_for_file_explorer_item("one.txt").unwrap();

    h.send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();

    // On an entry, which is what this test is about (the blank area would give
    // the same menu, since `is_multi` is read first).
    h.mouse_right_click(EXPLORER_COL, ENTRY_ROW).unwrap();

    let screen = h.screen_to_string();
    assert!(
        !screen.contains("New File"),
        "New File must be absent after Ctrl+A multi-select. Screen:\n{}",
        screen
    );
    assert!(
        screen.contains("Cut") && screen.contains("Copy") && screen.contains("Delete"),
        "Cut/Copy/Delete must be present. Screen:\n{}",
        screen
    );
}

// ── prompt wording ────────────────────────────────────────────────────────────

/// "New File" in the context menu prompts with "New file name:" (not "Rename to:").
#[test]
fn test_new_file_prompt_wording() {
    let mut h = harness_with_explorer();

    let menu_y = EXPLORER_ROW + 1;
    let new_file_row = menu_y + 1; // item index 0

    h.mouse_right_click(EXPLORER_COL, EXPLORER_ROW).unwrap();
    h.mouse_click(EXPLORER_COL + 2, new_file_row).unwrap();

    h.wait_for_prompt().unwrap();
    let screen = h.screen_to_string();
    assert!(
        screen.contains("New file name"),
        "Prompt should say 'New file name'. Screen:\n{}",
        screen
    );
    assert!(
        !screen.contains("Rename to"),
        "Prompt must not say 'Rename to'. Screen:\n{}",
        screen
    );

    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    h.wait_for_prompt_closed().unwrap();
}

/// "New Directory" in the context menu prompts with "New folder name:" (not "Rename to:").
#[test]
fn test_new_directory_prompt_wording() {
    let mut h = harness_with_explorer();

    let menu_y = EXPLORER_ROW + 1;
    let new_dir_row = menu_y + 2; // item index 1

    h.mouse_right_click(EXPLORER_COL, EXPLORER_ROW).unwrap();
    h.mouse_click(EXPLORER_COL + 2, new_dir_row).unwrap();

    h.wait_for_prompt().unwrap();
    let screen = h.screen_to_string();
    assert!(
        screen.contains("New folder name"),
        "Prompt should say 'New folder name'. Screen:\n{}",
        screen
    );
    assert!(
        !screen.contains("Rename to"),
        "Prompt must not say 'Rename to'. Screen:\n{}",
        screen
    );

    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    h.wait_for_prompt_closed().unwrap();
}

// ── root node protection ──────────────────────────────────────────────────────

// Root is always the first content row (row 2): title bar is row 1.
const ROOT_ROW: u16 = 2;

/// Right-clicking the project root shows only New File, New Directory, Paste —
/// Cut, Copy, Rename, and Delete are hidden (VS Code parity).
#[test]
fn test_root_menu_hides_destructive_items() {
    let mut h = harness_with_explorer();

    h.mouse_right_click(EXPLORER_COL, ROOT_ROW).unwrap();

    assert!(
        context_menu_visible(&h),
        "Context menu should open on root right-click"
    );

    let screen = h.screen_to_string();
    assert!(
        screen.contains("New File"),
        "New File must be visible for root. Screen:\n{}",
        screen
    );
    assert!(
        screen.contains("New Directory"),
        "New Directory must be visible for root. Screen:\n{}",
        screen
    );
    assert!(
        screen.contains("Paste"),
        "Paste must be visible for root. Screen:\n{}",
        screen
    );

    assert!(
        !screen.contains("Cut"),
        "Cut must be hidden for root. Screen:\n{}",
        screen
    );
    assert!(
        !screen.contains("Copy"),
        "Copy must be hidden for root. Screen:\n{}",
        screen
    );
    assert!(
        !screen.contains("Rename"),
        "Rename must be hidden for root. Screen:\n{}",
        screen
    );
    assert!(
        !screen.contains("Delete"),
        "Delete must be hidden for root. Screen:\n{}",
        screen
    );
}

/// Clicking "New File" from the root menu creates a file inside the root.
#[test]
fn test_root_menu_new_file_works() {
    let mut h = harness_with_explorer();
    let root = h.project_dir().unwrap();
    let initial_count = fs::read_dir(&root).unwrap().count();

    // New File is item index 0: menu_y + 1 + 0.
    let menu_y = ROOT_ROW + 1;
    let new_file_row = menu_y + 1;

    h.mouse_right_click(EXPLORER_COL, ROOT_ROW).unwrap();
    h.mouse_click(EXPLORER_COL + 2, new_file_row).unwrap();

    h.wait_for_prompt().unwrap();
    // The prompt opens empty — nothing is created until it is named — so the
    // name has to be typed where this used to accept a generated one.
    h.type_text("from_root_menu.txt").unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_for_prompt_closed().unwrap();

    h.wait_until(|_| fs::read_dir(&root).unwrap().count() > initial_count)
        .unwrap();
}

/// Clicking "New Directory" from the root menu creates a directory inside the root.
#[test]
fn test_root_menu_new_directory_works() {
    let mut h = harness_with_explorer();
    let root = h.project_dir().unwrap();
    let initial_dirs = fs::read_dir(&root)
        .unwrap()
        .filter_map(|e| e.ok())
        .filter(|e| e.path().is_dir())
        .count();

    // New Directory is item index 1: menu_y + 1 + 1.
    let menu_y = ROOT_ROW + 1;
    let new_dir_row = menu_y + 2;

    h.mouse_right_click(EXPLORER_COL, ROOT_ROW).unwrap();
    h.mouse_click(EXPLORER_COL + 2, new_dir_row).unwrap();

    h.wait_for_prompt().unwrap();
    // The prompt opens empty — nothing is created until it is named — so the
    // name has to be typed where this used to accept a generated one.
    h.type_text("from_root_menu").unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_for_prompt_closed().unwrap();

    let final_dirs = fs::read_dir(&root)
        .unwrap()
        .filter_map(|e| e.ok())
        .filter(|e| e.path().is_dir())
        .count();
    assert!(
        final_dirs > initial_dirs,
        "A new directory should have been created via root menu"
    );
}

// ── the blank area below the last entry ──────────────────────────────────────

/// The path the explorer's cursor is on, relative to the project root, with `/`
/// separators whatever the platform uses. By components rather than the string
/// the OS prints, so the same selection does not read `dir1\dir2` on Windows.
fn selected_relative_path(h: &EditorTestHarness) -> String {
    let explorer = h.editor().file_explorer().expect("an explorer");
    let entry = explorer.get_selected_entry().expect("a selection");
    let root = explorer.tree().root_path();
    let path = entry.path.strip_prefix(root).unwrap_or(&entry.path);
    path.components()
        .map(|c| c.as_os_str().to_string_lossy())
        .collect::<Vec<_>>()
        .join("/")
}

/// Right-clicking the blank area selects the project root and opens the root's
/// menu. It used to move no selection, so the entry menu opened against
/// whichever entry was selected before and acted on it.
#[test]
fn test_right_click_blank_area_selects_project_root() {
    let mut h = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let root = h.project_dir().unwrap();
    fs::write(root.join("target.txt"), "data").unwrap();

    h.editor_mut().focus_file_explorer();
    h.wait_for_file_explorer().unwrap();
    h.wait_for_file_explorer_item("target.txt").unwrap();

    // Select an entry first: what the stale menu used to be about.
    h.mouse_click(EXPLORER_COL, ENTRY_ROW).unwrap();
    assert_eq!(selected_relative_path(&h), "target.txt");

    h.mouse_right_click(EXPLORER_COL, EXPLORER_ROW + 10)
        .unwrap();

    assert_eq!(
        selected_relative_path(&h),
        "",
        "the blank area is the project root's"
    );
    let screen = h.screen_to_string();
    assert!(
        screen.contains("New File") && screen.contains("New Directory"),
        "the root menu must offer the create actions. Screen:\n{}",
        screen
    );
    assert!(
        !screen.contains("Rename") && !screen.contains("Delete"),
        "the root menu must not offer an entry's actions. Screen:\n{}",
        screen
    );
}

/// A reader's multi-selection survives a right-press on the blank area — which
/// includes the panel's walls, so clearing it there would lose the set to a
/// one-column miss. Only the cursor moves.
#[test]
fn test_right_click_blank_area_keeps_a_multi_selection() {
    let mut h = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let root = h.project_dir().unwrap();
    fs::write(root.join("one.txt"), "1").unwrap();
    fs::write(root.join("two.txt"), "2").unwrap();

    h.editor_mut().focus_file_explorer();
    h.wait_for_file_explorer().unwrap();
    h.wait_for_file_explorer_item("one.txt").unwrap();

    h.send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();
    let selected = |h: &EditorTestHarness| {
        h.editor()
            .file_explorer()
            .expect("an explorer")
            .multi_selection()
            .len()
    };
    let before = selected(&h);
    assert!(before > 1, "Ctrl+A selects the tree: {before}");

    h.mouse_right_click(EXPLORER_COL, EXPLORER_ROW + 10)
        .unwrap();

    assert_eq!(
        selected(&h),
        before,
        "the set is the reader's, not the menu's"
    );
    assert_eq!(
        selected_relative_path(&h),
        "",
        "the cursor still moves to the project root"
    );
}

// ── compact directory chains ─────────────────────────────────────────────────

/// Right-clicking one name of a compact `dir1/dir2/dir3` row selects that
/// directory, not the deepest one.
#[test]
fn test_right_click_compact_chain_segment_selects_that_directory() {
    let mut h = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let root = h.project_dir().unwrap();
    fs::create_dir_all(root.join("dir1/dir2/dir3")).unwrap();
    fs::write(root.join("dir1/dir2/dir3/deep.txt"), "deep").unwrap();

    h.editor_mut().focus_file_explorer();
    h.wait_for_file_explorer().unwrap();
    h.wait_for_file_explorer_item("dir1").unwrap();

    // Clicking `dir1` expands the whole single-child chain onto one row.
    h.mouse_click(EXPLORER_COL, ENTRY_ROW).unwrap();
    h.wait_for_file_explorer_item("deep.txt").unwrap();
    h.render().unwrap();

    // Where each name sits on screen, read off the row that was drawn.
    let screen = h.screen_to_string();
    let (row, line) = screen
        .lines()
        .enumerate()
        .find(|(_, l)| l.contains("dir1/dir2/dir3"))
        .map(|(i, l)| (i as u16, l.to_string()))
        .unwrap_or_else(|| panic!("no compact row on screen:\n{screen}"));
    // Each name appears once on the row, so the first match is the segment.
    let column_of = |name: &str| {
        line.char_indices()
            .filter(|(i, _)| line[*i..].starts_with(name))
            .map(|(i, _)| line[..i].chars().count() as u16)
            .next()
            .unwrap_or_else(|| panic!("{name} not on the row: {line:?}"))
    };
    for (name, expected) in [
        ("dir1", "dir1"),
        ("dir2", "dir1/dir2"),
        ("dir3", "dir1/dir2/dir3"),
    ] {
        h.mouse_right_click(column_of(name) + 1, row).unwrap();
        assert_eq!(
            selected_relative_path(&h),
            expected,
            "right-clicking {name} must select {expected}"
        );
        h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    }
}

// ── drag to move ─────────────────────────────────────────────────────────────

/// Dragging an entry onto a directory moves it there.
///
/// The first of #3427's unimplemented items: before this, the press selected
/// and previewed the entry and the release did nothing, so the only way to
/// move anything was cut and paste.
#[test]
fn test_dragging_an_entry_onto_a_directory_moves_it() {
    let mut h = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let root = h.project_dir().unwrap();
    fs::create_dir_all(root.join("dest")).unwrap();
    fs::write(root.join("moved.txt"), "payload").unwrap();

    h.editor_mut().focus_file_explorer();
    h.wait_for_file_explorer().unwrap();
    h.wait_for_file_explorer_item("moved.txt").unwrap();
    h.render().unwrap();

    let (from, to) = (
        explorer_row_of(&h, "moved.txt").expect("the file's row"),
        explorer_row_of(&h, "dest").expect("the directory's row"),
    );
    h.mouse_drag(EXPLORER_COL, from, EXPLORER_COL, to).unwrap();
    h.render().unwrap();

    assert!(
        root.join("dest/moved.txt").is_file(),
        "the entry should have moved into the directory it was dropped on.\n{}",
        h.screen_to_string()
    );
    assert_eq!(
        fs::read_to_string(root.join("dest/moved.txt")).unwrap(),
        "payload",
        "it should be the same file"
    );
    assert!(
        !root.join("moved.txt").exists(),
        "and it should be gone from the root"
    );
}

/// A press and release on one row is a click, not a move.
///
/// The press takes the pointer so a drag *can* start, so the thing to prove is
/// that an ordinary click still reads as one and nothing is moved.
#[test]
fn test_a_click_on_a_row_moves_nothing() {
    let mut h = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let root = h.project_dir().unwrap();
    fs::create_dir_all(root.join("dest")).unwrap();
    fs::write(root.join("stay.txt"), "payload").unwrap();

    h.editor_mut().focus_file_explorer();
    h.wait_for_file_explorer().unwrap();
    h.wait_for_file_explorer_item("stay.txt").unwrap();
    h.render().unwrap();

    let row = explorer_row_of(&h, "stay.txt").expect("the file's row");
    h.mouse_click(EXPLORER_COL, row).unwrap();
    h.render().unwrap();

    assert!(
        root.join("stay.txt").is_file(),
        "a click must leave the entry where it is.\n{}",
        h.screen_to_string()
    );
    assert!(!root.join("dest/stay.txt").exists());
}

/// Dropping an entry back into the directory it already lives in does nothing,
/// and says nothing: it is not a move and not an error.
#[test]
fn test_dropping_an_entry_where_it_already_is_does_nothing() {
    let mut h = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let root = h.project_dir().unwrap();
    fs::write(root.join("a.txt"), "a").unwrap();
    fs::write(root.join("b.txt"), "b").unwrap();

    h.editor_mut().focus_file_explorer();
    h.wait_for_file_explorer().unwrap();
    h.wait_for_file_explorer_item("b.txt").unwrap();
    h.render().unwrap();

    let (from, to) = (
        explorer_row_of(&h, "a.txt").expect("a's row"),
        explorer_row_of(&h, "b.txt").expect("b's row"),
    );
    h.mouse_drag(EXPLORER_COL, from, EXPLORER_COL, to).unwrap();
    h.render().unwrap();

    assert!(root.join("a.txt").is_file(), "a.txt should still be there");
    assert_eq!(
        fs::read_to_string(root.join("b.txt")).unwrap(),
        "b",
        "and b.txt must not have been replaced by it"
    );
}

/// A drop onto a name that is taken asks the one-entry question — the same one
/// a paste of a single file asks, "keep both" included — and leaves a cut the
/// reader is still holding alone.
///
/// The drag reuses paste's machinery; the one thing it must not reuse is
/// paste's ownership of the clipboard.
#[test]
fn test_dropping_onto_a_taken_name_asks_and_spares_the_clipboard() {
    let mut h = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let root = h.project_dir().unwrap();
    fs::create_dir_all(root.join("dest")).unwrap();
    fs::write(root.join("dest/same.txt"), "theirs").unwrap();
    fs::write(root.join("same.txt"), "mine").unwrap();
    fs::write(root.join("held.txt"), "held").unwrap();

    h.editor_mut().focus_file_explorer();
    h.wait_for_file_explorer().unwrap();
    h.wait_for_file_explorer_item("same.txt").unwrap();
    h.render().unwrap();

    // Something cut and waiting, which the drag has no business touching.
    let held = explorer_row_of(&h, "held.txt").expect("the held file's row");
    h.mouse_click(EXPLORER_COL, held).unwrap();
    h.send_key(KeyCode::Char('x'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();

    let (from, to) = (
        explorer_row_of(&h, "same.txt").expect("the file's row"),
        explorer_row_of(&h, "dest").expect("the directory's row"),
    );
    h.mouse_drag(EXPLORER_COL, from, EXPLORER_COL, to).unwrap();
    h.wait_for_prompt().unwrap();

    let asked = h.screen_to_string();
    assert!(
        asked.contains("Rename"),
        "one colliding entry should be asked about one at a time, so that \
         keeping both is on offer.\n{asked}"
    );

    // Keep both. The name the prompt starts with is the one that collided, so
    // adding to it is enough whichever end the cursor sits at.
    h.send_key(KeyCode::Char('r'), KeyModifiers::NONE).unwrap();
    h.type_text("kept-").unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_for_prompt_closed().unwrap();
    h.render().unwrap();

    let mut landed: Vec<(String, String)> = fs::read_dir(root.join("dest"))
        .unwrap()
        .filter_map(|e| e.ok())
        .map(|e| {
            (
                e.file_name().to_string_lossy().to_string(),
                fs::read_to_string(e.path()).unwrap_or_default(),
            )
        })
        .collect();
    landed.sort();
    let contents: Vec<&str> = landed.iter().map(|(_, c)| c.as_str()).collect();
    assert_eq!(
        contents.len(),
        2,
        "both files should be in the directory now: {landed:?}"
    );
    assert!(
        contents.contains(&"theirs") && contents.contains(&"mine"),
        "neither of them overwritten: {landed:?}"
    );
    assert!(
        !root.join("same.txt").exists(),
        "and the dragged entry is gone from where it was"
    );

    // The cut is still the reader's to paste.
    let dest = explorer_row_of(&h, "dest").expect("the directory's row");
    h.mouse_click(EXPLORER_COL, dest).unwrap();
    h.send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();
    assert!(
        root.join("dest/held.txt").is_file(),
        "the drag must not have emptied the clipboard.\n{}",
        h.screen_to_string()
    );
}

/// The screen row an entry is drawn on, by its name.
fn explorer_row_of(h: &EditorTestHarness, name: &str) -> Option<u16> {
    h.screen_to_string()
        .lines()
        .enumerate()
        .find(|(_, line)| {
            let lane: String = line.chars().take(30).collect();
            lane.contains(name)
        })
        .map(|(i, _)| i as u16)
}
