use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use tempfile::TempDir;

/// Helper: create a harness, open a file with content, position cursor, render.
fn setup(content: &str) -> (EditorTestHarness, TempDir) {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();
    (harness, temp_dir)
}

/// Send Ctrl+] (goto matching bracket)
fn goto_matching_bracket(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char(']'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
}

/// Move cursor to a specific byte offset using Home then Right arrows.
fn move_cursor_to(harness: &mut EditorTestHarness, offset: usize) {
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    for _ in 0..offset {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();
}

// =============================================================================
// Existing behavior: cursor ON a bracket
// =============================================================================

#[test]
fn test_goto_matching_bracket_from_opening_paren() {
    // Content: foo(bar)
    // Cursor at position 3 (on '('), should jump to position 7 (on ')')
    let (mut harness, _tmp) = setup("foo(bar)");
    move_cursor_to(&mut harness, 3);
    assert_eq!(harness.cursor_position(), 3);

    goto_matching_bracket(&mut harness);
    assert_eq!(
        harness.cursor_position(),
        7,
        "Should jump from '(' to matching ')'"
    );
}

#[test]
fn test_goto_matching_bracket_from_closing_paren() {
    // Content: foo(bar)
    // Cursor at position 7 (on ')'), should jump to position 3 (on '(')
    let (mut harness, _tmp) = setup("foo(bar)");
    move_cursor_to(&mut harness, 7);

    goto_matching_bracket(&mut harness);
    assert_eq!(
        harness.cursor_position(),
        3,
        "Should jump from ')' to matching '('"
    );
}

// =============================================================================
// New behavior: cursor INSIDE brackets (issue #1258)
// =============================================================================

#[test]
fn test_goto_matching_bracket_from_inside_parens() {
    // Content: foo(bar)
    // Cursor at position 4 (on 'b' inside parens), should jump to nearest closing ')'
    let (mut harness, _tmp) = setup("foo(bar)");
    move_cursor_to(&mut harness, 4);
    assert_eq!(harness.cursor_position(), 4);

    goto_matching_bracket(&mut harness);
    assert_eq!(
        harness.cursor_position(),
        7,
        "From inside parens, should jump to nearest closing ')'"
    );
}

#[test]
fn test_goto_matching_bracket_from_inside_curly_braces() {
    // Content: fn main() { hello }
    // Cursor at position 13 (on 'h' inside braces), should jump to closing '}'
    let (mut harness, _tmp) = setup("fn main() { hello }");
    move_cursor_to(&mut harness, 13);

    goto_matching_bracket(&mut harness);
    assert_eq!(
        harness.cursor_position(),
        18,
        "From inside braces, should jump to nearest closing '}}'"
    );
}

#[test]
fn test_goto_matching_bracket_from_inside_square_brackets() {
    // Content: arr[1, 2, 3]
    // Cursor at position 5 (on ',' inside brackets), should jump to closing ']'
    let (mut harness, _tmp) = setup("arr[1, 2, 3]");
    move_cursor_to(&mut harness, 5);

    goto_matching_bracket(&mut harness);
    assert_eq!(
        harness.cursor_position(),
        11,
        "From inside brackets, should jump to nearest closing ']'"
    );
}

#[test]
fn test_goto_matching_bracket_from_inside_nested() {
    // Content: foo(bar[baz])
    // Cursor at position 8 (on 'b' of 'baz', inside []), should jump to ']' at position 11
    let (mut harness, _tmp) = setup("foo(bar[baz])");
    move_cursor_to(&mut harness, 8);

    goto_matching_bracket(&mut harness);
    assert_eq!(
        harness.cursor_position(),
        11,
        "From inside nested brackets, should jump to nearest closing ']'"
    );
}

#[test]
fn test_goto_matching_bracket_from_inside_outer_of_nested() {
    // Content: foo(bar[baz])
    // Cursor at position 4 (on 'b' of 'bar', inside () but outside [])
    // Should jump to ')' at position 12
    let (mut harness, _tmp) = setup("foo(bar[baz])");
    move_cursor_to(&mut harness, 4);

    goto_matching_bracket(&mut harness);
    assert_eq!(
        harness.cursor_position(),
        12,
        "From inside outer parens, should jump to nearest closing ')'"
    );
}
