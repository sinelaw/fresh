use crate::common::harness::EditorTestHarness;

/// Test triple-click selects the entire line
/// Issue #597: Support click 3 times to select the whole line
#[test]
fn test_triple_click_selects_line() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Load content with multiple lines
    let content = "First line here\nSecond line here\nThird line here\n";
    let _fixture = harness.load_buffer_from_text(content).unwrap();
    harness.render().unwrap();

    let (content_first_row, _) = harness.content_area_rows();
    let row = content_first_row as u16;

    // Triple-click on the first line (after gutter ~8 chars)
    // Click 3 times rapidly at the same position
    harness.mouse_click(12, row).unwrap();
    harness.mouse_click(12, row).unwrap();
    harness.mouse_click(12, row).unwrap();
    harness.render().unwrap();

    // Should have a selection that covers the entire first line
    assert!(
        harness.has_selection(),
        "Triple-click should create a selection"
    );

    let selected = harness.get_selected_text();
    // SelectLine selects the line including the newline
    assert!(
        selected.contains("First line here"),
        "Triple-click should select the entire line. Got: '{}'",
        selected
    );
}

/// Test triple-click on middle line
#[test]
fn test_triple_click_middle_line() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    let content = "Line one\nLine two\nLine three\n";
    let _fixture = harness.load_buffer_from_text(content).unwrap();
    harness.render().unwrap();

    let (content_first_row, _) = harness.content_area_rows();
    let row = content_first_row as u16 + 1; // Second line

    // Triple-click on the second line
    harness.mouse_click(12, row).unwrap();
    harness.mouse_click(12, row).unwrap();
    harness.mouse_click(12, row).unwrap();
    harness.render().unwrap();

    assert!(
        harness.has_selection(),
        "Triple-click should create a selection"
    );

    let selected = harness.get_selected_text();
    assert!(
        selected.contains("Line two"),
        "Triple-click on second line should select it. Got: '{}'",
        selected
    );
}

/// Test that regular double-click still works (selects word, not line)
#[test]
fn test_double_click_still_selects_word() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    let content = "hello world test\n";
    let _fixture = harness.load_buffer_from_text(content).unwrap();
    harness.render().unwrap();

    let (content_first_row, _) = harness.content_area_rows();
    let row = content_first_row as u16;

    // Double-click (not triple)
    harness.mouse_click(12, row).unwrap();
    harness.mouse_click(12, row).unwrap();
    harness.render().unwrap();

    assert!(
        harness.has_selection(),
        "Double-click should create a selection"
    );

    let selected = harness.get_selected_text();
    // Double-click should select a word, not the entire line
    assert!(
        !selected.contains("hello world test"),
        "Double-click should select a word, not the whole line. Got: '{}'",
        selected
    );
}
