//! Faithful migration of `tests/e2e/triple_click.rs`.
//!
//! Original drives `harness.mouse_click(col, row)` three times
//! rapidly at the same position; the editor's click-detection
//! recognises the sequence as a triple-click and dispatches the
//! `SelectLine` semantics. The scenario equivalent uses
//! `EditorTestApi::dispatch_mouse_click(col, row)` three times
//! through the same `Editor::handle_mouse` path.
//!
//! Issue #597: Support click 3 times to select the whole line.

use crate::common::harness::EditorTestHarness;

/// First content row depends on chrome (menu/tab bar). The
/// editor exposes it via `harness.content_area_rows()`.
fn content_first_row(harness: &EditorTestHarness) -> u16 {
    harness.content_area_rows().0 as u16
}

#[test]
fn migrated_triple_click_selects_first_line() {
    // Original: `test_triple_click_selects_line`.
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let _f = harness
        .load_buffer_from_text("First line here\nSecond line here\nThird line here\n")
        .unwrap();
    harness.render().unwrap();
    let row = content_first_row(&harness);

    // Three rapid clicks at the same position.
    harness.api_mut().dispatch_mouse_click(12, row);
    harness.api_mut().dispatch_mouse_click(12, row);
    harness.api_mut().dispatch_mouse_click(12, row);

    let api = harness.api_mut();
    let selection = api.selection_text();
    assert!(
        selection.contains("First line here"),
        "triple-click should select the first line; got {selection:?}"
    );
}

#[test]
fn migrated_triple_click_selects_middle_line() {
    // Original: `test_triple_click_middle_line`.
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let _f = harness
        .load_buffer_from_text("Line one\nLine two\nLine three\n")
        .unwrap();
    harness.render().unwrap();
    let row = content_first_row(&harness) + 1; // second line

    harness.api_mut().dispatch_mouse_click(12, row);
    harness.api_mut().dispatch_mouse_click(12, row);
    harness.api_mut().dispatch_mouse_click(12, row);

    let api = harness.api_mut();
    let selection = api.selection_text();
    assert!(
        selection.contains("Line two"),
        "triple-click on middle line should select it; got {selection:?}"
    );
}

#[test]
fn migrated_double_click_selects_word_not_line() {
    // Original: `test_double_click_still_selects_word`.
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let _f = harness.load_buffer_from_text("hello world test\n").unwrap();
    harness.render().unwrap();
    let row = content_first_row(&harness);

    // Two rapid clicks ⇒ double-click semantics ⇒ SelectWord.
    harness.api_mut().dispatch_mouse_click(12, row);
    harness.api_mut().dispatch_mouse_click(12, row);

    let api = harness.api_mut();
    let selection = api.selection_text();
    assert!(
        !selection.contains("hello world test"),
        "double-click should not select the whole line; got {selection:?}"
    );
}

/// Anti-test (harness-direct): drops the third click from
/// `migrated_triple_click_selects_first_line`. Only two clicks
/// trigger double-click (SelectWord) semantics, NOT triple-click
/// (SelectLine), so the selection must NOT contain the entire
/// "First line here" — proves the third click is what promotes
/// the gesture to line-select.
#[test]
fn anti_triple_click_dropping_third_click_does_not_select_full_line() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let _f = harness
        .load_buffer_from_text("First line here\nSecond line here\nThird line here\n")
        .unwrap();
    harness.render().unwrap();
    let row = content_first_row(&harness);

    // Only TWO clicks (third dropped).
    harness.api_mut().dispatch_mouse_click(12, row);
    harness.api_mut().dispatch_mouse_click(12, row);

    let api = harness.api_mut();
    let selection = api.selection_text();
    assert!(
        !selection.contains("First line here"),
        "anti-test: with only 2 clicks (no triple-click) the selection must NOT \
         span the full line; got {selection:?}"
    );
}
