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
// ...well inside the content rows, and past the last entry of a small fixture,
// which makes it the *blank area*: the project root's, with the root's narrow
// menu (issue #3427).
const EXPLORER_ROW: u16 = 5;
// A test about an entry's menu right-clicks an entry. Row 2 is the project root
// and row 3 the first child under it — see `harness_with_entry`.
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

/// The same, with one file in it — so [`ENTRY_ROW`] is an entry. The bare
/// project has only the root row, and every row below it is the blank area,
/// which is the root's (issue #3427).
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

    // A file should have been created (the explorer creates it immediately
    // then enters rename mode).
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

    // Wait for prompt (rename mode) then accept default name.
    h.wait_for_prompt().unwrap();
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

    // Accept the default folder name.
    h.wait_for_prompt().unwrap();
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

/// Which item the open menu highlights, and how many it has.
///
/// Asserting on the highlight rather than on "a menu is still on screen" is
/// what makes the two wrap tests below about wrapping: with the count read off
/// the menu itself, neither depends on how many items the entry menu has today
/// (`FileExplorerContextMenuItem::all()` is append-only, and both tests used to
/// name a stale count in a comment).
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
    // An entry, for the full menu: `EXPLORER_ROW` is the blank area in this
    // fixture, whose menu is the root's three items.
    let mut h = harness_with_entry();
    h.mouse_right_click(EXPLORER_COL, ENTRY_ROW).unwrap();
    let (highlighted, items) = menu_highlight(&h);
    assert_eq!(highlighted, 0, "a fresh menu highlights its first item");
    assert!(items > 3, "the entry menu, not the root's: {items} items");

    // Up from index 0 wraps to the last item.
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

    // Down to the last item, however many the menu has.
    for _ in 0..items - 1 {
        h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    assert_eq!(menu_highlight(&h), (items - 1, items));

    // One more Down wraps to index 0 — menu stays open.
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

// ── second right-click replaces menu ─────────────────────────────────────────

/// A second right-click at a different position replaces the existing menu
/// (the menu closes then reopens at the new position).
#[test]
fn test_second_right_click_replaces_menu() {
    let mut h = harness_with_explorer();

    h.mouse_right_click(EXPLORER_COL, EXPLORER_ROW).unwrap();
    assert!(context_menu_visible(&h));

    // Right-click at a different row — should reopen at the new position.
    h.mouse_right_click(EXPLORER_COL, EXPLORER_ROW + 2).unwrap();
    assert!(
        context_menu_visible(&h),
        "Context menu should be open after second right-click"
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

    // On an entry. (The blank area would give the same menu — `is_multi` is
    // read first — but an entry is what this test is about.)
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

/// The path the explorer's cursor is on, relative to the project root.
fn selected_relative_path(h: &EditorTestHarness) -> String {
    let explorer = h.editor().file_explorer().expect("an explorer");
    let entry = explorer.get_selected_entry().expect("a selection");
    let root = explorer.tree().root_path();
    entry
        .path
        .strip_prefix(root)
        .map(|p| p.to_string_lossy().into_owned())
        .unwrap_or_else(|_| entry.path.to_string_lossy().into_owned())
}

/// **Right-clicking the blank area selects the project root** and opens the
/// root's narrow menu — the same menu the root row gives.
///
/// It used to move no selection at all, so the *entry* menu opened against
/// whichever entry was selected before and every action in it — Rename, Delete,
/// the copy-path items — acted on that entry (issue #3427).
#[test]
fn test_right_click_blank_area_selects_project_root() {
    let mut h = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let root = h.project_dir().unwrap();
    fs::write(root.join("target.txt"), "data").unwrap();

    h.editor_mut().focus_file_explorer();
    h.wait_for_file_explorer().unwrap();
    h.wait_for_file_explorer_item("target.txt").unwrap();

    // Select an entry first: it is what the stale menu used to be about.
    h.mouse_click(EXPLORER_COL, ENTRY_ROW).unwrap();
    assert_eq!(selected_relative_path(&h), "target.txt");

    // Then right-click well below the last entry.
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

/// **A reader's multi-selection survives a right-press on the blank area.**
///
/// "The blank area" is every press inside the panel that no row claimed, which
/// is also the scrollbar's lane and the panel's walls — so clearing the set here
/// would lose it to a one-column miss. Only the cursor moves; a press on a row
/// leaves the set alone too.
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

/// **Right-clicking one name of a compact `dir1/dir2/dir3` row selects that
/// directory**, not the deepest one (issue #3427). Which segment the pointer
/// was on is resolved from the label the row rendered, so the indent and the
/// expand indicator in front of the names do not shift it.
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

    // Where each name sits on screen, read off the row that was drawn — the
    // same label the press resolves against.
    let screen = h.screen_to_string();
    let (row, line) = screen
        .lines()
        .enumerate()
        .find(|(_, l)| l.contains("dir1/dir2/dir3"))
        .map(|(i, l)| (i as u16, l.to_string()))
        .expect("the compact row");
    // Each of `dir1`, `dir2` and `dir3` appears once on the row, so the first
    // match is the segment.
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
