//! Reproducers for goal-column (sticky column) unit bugs.
//!
//! `Cursor::sticky_column` is defined as a *visual* column (wide-char
//! aware), and vertical movement resolves it with
//! `byte_offset_at_visual_column`. Several producers used to store byte
//! columns (PageUp/PageDown, mouse click/drag) or even a byte *position*
//! (bracket expansion) in it, sending later vertical moves to the wrong
//! column — or, for PageUp/PageDown over wide chars, landing the cursor in
//! the middle of a multi-byte character.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

const WIDE_LINE: &str = "你好好def";

/// PageDown must resolve the goal column visually: from the end of "你a"
/// (visual column 3) it lands after the first 好 (a char boundary), not at
/// raw byte offset 4 (inside 好).
#[test]
fn test_page_down_keeps_visual_column_over_wide_chars() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let content = format!("你a\n{}", vec![WIDE_LINE; 40].join("\n"));
    harness.load_buffer_from_text(&content).unwrap();

    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::PageDown, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("X").unwrap();

    // Goal column 3 falls inside the first 好 (columns 2..4); it must snap
    // to the char boundary after it. A byte-column goal inserted X inside
    // the character, corrupting the line.
    harness.assert_screen_contains("你好X好def");
}

/// Same as above for PageUp.
#[test]
fn test_page_up_keeps_visual_column_over_wide_chars() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let content = format!("{}\n你a", vec![WIDE_LINE; 40].join("\n"));
    harness.load_buffer_from_text(&content).unwrap();

    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::PageUp, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("X").unwrap();

    harness.assert_screen_contains("你好X好def");
}

/// A mouse click after wide chars must set a *visual* goal column, so a
/// following ArrowDown lands at the same on-screen column.
#[test]
fn test_click_then_down_keeps_visual_column() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness
        .load_buffer_from_text("你好你好xyz\nabcdefghijkl")
        .unwrap();

    let (x0, y0) = harness
        .find_text_on_screen("你好你好")
        .expect("wide line visible");
    // 你好你好 spans 8 screen cells; clicking right after it puts the
    // cursor at byte 12, visual column 8.
    harness.mouse_click(x0 + 8, y0).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.type_text("X").unwrap();

    // Visual goal column 8 → X lands between 'h' and 'i'. The old byte
    // goal (12) landed it at the end of the line.
    harness.assert_screen_contains("abcdefghXijkl");
}

/// A mouse drag must also record the goal column visually.
#[test]
fn test_drag_then_down_keeps_visual_column() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness
        .load_buffer_from_text("你好你好xyz\nabcdefghijkl")
        .unwrap();

    let (x0, y0) = harness
        .find_text_on_screen("你好你好")
        .expect("wide line visible");
    harness.mouse_drag(x0, y0, x0 + 8, y0).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.type_text("X").unwrap();

    harness.assert_screen_contains("abcdefghXijkl");
}

/// Bracket expansion ({| }, Enter) used to store the cursor's byte
/// *position* as the goal column, so later vertical movement jumped to a
/// far-right column instead of staying near the indentation.
#[test]
fn test_bracket_expansion_leaves_no_bogus_goal_column() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    // The long first line inflates byte positions so the old bug (goal
    // column = byte position) is far from any real column.
    harness
        .load_buffer_from_text("0123456789012345678901234567\n()\nabcdefghijklmnop")
        .unwrap();

    let (px, py) = harness.find_text_on_screen("()").expect("parens visible");
    // Click on ')' → cursor between the parens.
    harness.mouse_click(px + 1, py).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Mark the indent column so the assertion adapts to whatever
    // auto-indent produced.
    harness.type_text("Y").unwrap();
    let (yx, _) = harness.find_text_on_screen("Y").expect("marker visible");

    let (tx, _) = harness
        .find_text_on_screen("abcdefghijklmnop")
        .expect("target line visible");

    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.type_text("X").unwrap();

    // The cursor after typing Y sits one past the marker's column; two
    // ArrowDowns must keep that column on the target line.
    let target = "abcdefghijklmnop";
    let col = (yx - tx) as usize + 1;
    assert!(col < target.len(), "indent unexpectedly wide");
    let expected = format!("{}X{}", &target[..col], &target[col..]);
    harness.assert_screen_contains(&expected);
}
