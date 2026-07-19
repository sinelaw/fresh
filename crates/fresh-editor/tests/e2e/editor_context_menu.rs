//! E2E tests for the editor (buffer) right-click context menu (issue #315).
//!
//! A plain right-click inside a text-buffer split opens a small VSCode-style
//! popup offering Cut / Copy / Paste / Select All. These tests drive real
//! mouse/keyboard events and assert only on rendered output, per the project's
//! "observe, not inspect" testing rule.
//!
//! Terminal: 120 × 40. With no file explorer open, the buffer split fills the
//! whole content region, so a right-click well inside the screen (col 40,
//! row 12) always lands in the split content area.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

// Right-click anchor — comfortably inside the editor content area.
const RC_COL: u16 = 40;
const RC_ROW: u16 = 12;

// The menu opens at (RC_COL, RC_ROW + 1). With a top border at that row, the
// items render at the rows below it, in declaration order:
//   Cut(0) Copy(1) Paste(2) Select All(3)
const MENU_Y: u16 = RC_ROW + 1;
const CUT_ROW: u16 = MENU_Y + 1;
const COPY_ROW: u16 = MENU_Y + 2;
const PASTE_ROW: u16 = MENU_Y + 3;
const SELECT_ALL_ROW: u16 = MENU_Y + 4;
const ITEM_COL: u16 = RC_COL + 2; // a column inside the menu body

/// "Select All" is unique to this menu — the file-explorer menu has no such
/// item and the top menu bar only shows category names, not item labels — so
/// it's a reliable observe-only signal that the editor context menu is open.
fn menu_visible(h: &EditorTestHarness) -> bool {
    h.screen_to_string().contains("Select All")
}

fn harness_with_buffer(content: &str) -> (EditorTestHarness, crate::common::fixtures::TestFixture) {
    let mut h = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let fixture = h.load_buffer_from_text(content).unwrap();
    h.render().unwrap();
    (h, fixture)
}

// ── menu open / close ─────────────────────────────────────────────────────

/// Right-clicking inside the editor buffer opens the context menu with all
/// four clipboard items.
#[test]
fn test_right_click_opens_editor_context_menu() {
    let (mut h, _fix) = harness_with_buffer("hello world\nsecond line\nthird line\n");

    assert!(
        !menu_visible(&h),
        "menu should be closed before right-click"
    );

    h.mouse_right_click(RC_COL, RC_ROW).unwrap();

    assert!(
        menu_visible(&h),
        "right-click in the editor should open the context menu. Screen:\n{}",
        h.screen_to_string()
    );
    h.assert_screen_contains("Cut");
    h.assert_screen_contains("Copy");
    h.assert_screen_contains("Paste");
    h.assert_screen_contains("Select All");
}

/// Right-clicking the menu bar row (row 0) is not inside any buffer split, so
/// no context menu opens.
#[test]
fn test_right_click_menu_bar_row_no_menu() {
    let (mut h, _fix) = harness_with_buffer("hello world\n");

    h.mouse_right_click(RC_COL, 0).unwrap();

    assert!(
        !menu_visible(&h),
        "right-click on the menu bar must not open the editor context menu"
    );
}

/// Escape closes the menu.
#[test]
fn test_escape_closes_menu() {
    let (mut h, _fix) = harness_with_buffer("hello world\n");
    h.mouse_right_click(RC_COL, RC_ROW).unwrap();
    assert!(menu_visible(&h));

    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();

    assert!(!menu_visible(&h), "Escape should close the context menu");
}

/// Left-clicking outside the menu closes it.
#[test]
fn test_left_click_outside_closes_menu() {
    let (mut h, _fix) = harness_with_buffer("hello world\nsecond line\n");
    h.mouse_right_click(RC_COL, RC_ROW).unwrap();
    assert!(menu_visible(&h));

    // Click far from the menu body.
    h.mouse_click(RC_COL, 2).unwrap();

    assert!(
        !menu_visible(&h),
        "left-click outside the menu should close it"
    );
}

/// Typing a normal character dismisses the menu (and falls through to normal
/// input handling rather than being swallowed).
#[test]
fn test_typing_dismisses_menu() {
    let (mut h, _fix) = harness_with_buffer("abc\n");
    h.mouse_right_click(RC_COL, RC_ROW).unwrap();
    assert!(menu_visible(&h));

    h.type_text("Z").unwrap();

    assert!(
        !menu_visible(&h),
        "typing should dismiss the editor context menu"
    );
}

// ── actions ───────────────────────────────────────────────────────────────

/// Clicking "Select All" then typing replaces the entire buffer — proving the
/// menu's Select All selected everything.
#[test]
fn test_select_all_via_menu_replaces_on_type() {
    let (mut h, _fix) = harness_with_buffer("hello world\nsecond line\n");

    h.mouse_right_click(RC_COL, RC_ROW).unwrap();
    h.mouse_click(ITEM_COL, SELECT_ALL_ROW).unwrap();
    assert!(
        !menu_visible(&h),
        "menu should close after activating an item"
    );

    // With everything selected, typing replaces the whole document.
    h.type_text("Z").unwrap();

    let screen = h.screen_to_string();
    assert!(
        !screen.contains("hello world"),
        "Select All + type should have replaced the buffer. Screen:\n{}",
        screen
    );
    assert!(
        screen.contains('Z'),
        "the typed replacement text should be visible. Screen:\n{}",
        screen
    );
}

/// Copy via the menu, then Paste via the menu, round-trips the text through
/// the clipboard (duplicating the line).
#[test]
fn test_copy_then_paste_via_menu() {
    let (mut h, _fix) = harness_with_buffer("Hello");
    // Isolate from the host/CI system clipboard: internal-only mode makes
    // copy()/paste() round-trip through the in-editor clipboard. Passing an
    // empty string just flips the mode on without seeding any content.
    h.editor_mut().set_clipboard_for_test(String::new());

    // Select everything via keyboard, then Copy via the menu.
    h.send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    h.mouse_right_click(RC_COL, RC_ROW).unwrap();
    h.mouse_click(ITEM_COL, COPY_ROW).unwrap();

    // Collapse the selection to the line end, then Paste via the menu.
    h.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    h.mouse_right_click(RC_COL, RC_ROW).unwrap();
    h.mouse_click(ITEM_COL, PASTE_ROW).unwrap();

    h.assert_screen_contains("HelloHello");
}

/// Cut via the menu removes the selected text from the buffer.
#[test]
fn test_cut_via_menu_removes_text() {
    let (mut h, _fix) = harness_with_buffer("DeleteMe");

    h.send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    h.mouse_right_click(RC_COL, RC_ROW).unwrap();
    h.mouse_click(ITEM_COL, CUT_ROW).unwrap();

    let screen = h.screen_to_string();
    assert!(
        !screen.contains("DeleteMe"),
        "Cut should have removed the text. Screen:\n{}",
        screen
    );
}

// ── keyboard navigation ─────────────────────────────────────────────────────

/// Enter activates the highlighted item. With the menu freshly opened, Cut
/// (index 0) is highlighted; Enter on a full selection clears the buffer.
#[test]
fn test_enter_activates_highlighted_item() {
    let (mut h, _fix) = harness_with_buffer("ZapThis");

    h.send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    h.mouse_right_click(RC_COL, RC_ROW).unwrap();
    assert!(menu_visible(&h));

    // Default highlight is Cut (item 0); Enter cuts the selection.
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();

    assert!(!menu_visible(&h), "menu should close after Enter");
    let screen = h.screen_to_string();
    assert!(
        !screen.contains("ZapThis"),
        "Enter on Cut should have removed the text. Screen:\n{}",
        screen
    );
}

/// Down arrow keeps the menu open (navigates the highlight).
#[test]
fn test_down_arrow_keeps_menu_open() {
    let (mut h, _fix) = harness_with_buffer("hello world\n");
    h.mouse_right_click(RC_COL, RC_ROW).unwrap();
    assert!(menu_visible(&h));

    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();

    assert!(
        menu_visible(&h),
        "the menu should stay open while navigating with Down"
    );
}

// ── hover ───────────────────────────────────────────────────────────────────

/// Hovering over items keeps the menu open.
#[test]
fn test_hover_keeps_menu_open() {
    let (mut h, _fix) = harness_with_buffer("hello world\n");
    h.mouse_right_click(RC_COL, RC_ROW).unwrap();
    assert!(menu_visible(&h));

    h.mouse_move(ITEM_COL, PASTE_ROW).unwrap();

    assert!(
        menu_visible(&h),
        "hovering over a menu item should not close the menu"
    );
}

// ── re-open ───────────────────────────────────────────────────────────────

/// A second right-click at a different position re-anchors the menu.
#[test]
fn test_second_right_click_reopens_menu() {
    let (mut h, _fix) = harness_with_buffer("hello world\nsecond line\nthird line\n");

    h.mouse_right_click(RC_COL, RC_ROW).unwrap();
    assert!(menu_visible(&h));

    h.mouse_right_click(RC_COL + 5, RC_ROW + 2).unwrap();
    assert!(
        menu_visible(&h),
        "the menu should still be visible after a second right-click"
    );
}
