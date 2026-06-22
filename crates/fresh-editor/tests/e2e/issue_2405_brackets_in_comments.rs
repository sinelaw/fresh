//! E2E regression test for issue #2405.
//!
//! Rainbow bracket colorization (and bracket-match highlighting) must not
//! apply to brackets that live inside comments or strings. Those characters
//! are prose/data, not structural brackets, so they should keep their
//! syntax color (e.g. the comment color) instead of being painted with a
//! rainbow color.

use crate::common::harness::{EditorTestHarness, HarnessOptions};

/// Open a small Rust file that contains a bracket in real code (line 0) and a
/// bracket inside a line comment (line 1). The code bracket should be
/// rainbow-colored; the comment bracket should keep the comment color.
#[test]
fn brackets_in_comments_are_not_rainbow_colored() {
    let mut harness = EditorTestHarness::create(
        80,
        24,
        HarnessOptions::new()
            .with_project_root()
            .with_full_grammar_registry(),
    )
    .unwrap();
    let project_dir = harness.project_dir().unwrap();

    // Line 0: a real expression with structural brackets.
    // Line 1: a comment that merely mentions brackets.
    let file = project_dir.join("brackets.rs");
    std::fs::write(&file, "let x = (1);\n// y = (2)\n").unwrap();
    harness.open_file(&file).unwrap();
    harness.render().unwrap();

    let theme = harness.editor().theme();
    let comment_color = theme.syntax_comment;
    let rainbow_colors = [
        theme.bracket_rainbow_1,
        theme.bracket_rainbow_2,
        theme.bracket_rainbow_3,
        theme.bracket_rainbow_4,
        theme.bracket_rainbow_5,
        theme.bracket_rainbow_6,
    ];

    let buffer = &harness.editor().active_state().buffer;
    let gutter = harness.editor().active_viewport().gutter_width(buffer) as u16;
    let (first_row, _) = harness.content_area_rows();
    let first_row = first_row as u16;

    // Sanity: the code bracket '(' is at line 0, column 8.
    let code_x = gutter + 8;
    assert_eq!(
        harness.get_cell(code_x, first_row),
        Some("(".to_string()),
        "expected '(' of the code expression at line 0 col 8"
    );
    let code_style = harness
        .get_cell_style(code_x, first_row)
        .expect("style for code bracket cell");
    assert!(
        rainbow_colors.contains(&code_style.fg.unwrap()),
        "structural bracket in code should keep a rainbow color, got {:?}",
        code_style.fg
    );

    // The comment bracket '(' is at line 1, column 7.
    let comment_x = gutter + 7;
    let comment_row = first_row + 1;
    assert_eq!(
        harness.get_cell(comment_x, comment_row),
        Some("(".to_string()),
        "expected '(' inside the comment at line 1 col 7"
    );
    let comment_style = harness
        .get_cell_style(comment_x, comment_row)
        .expect("style for comment bracket cell");
    assert!(
        !rainbow_colors.contains(&comment_style.fg.unwrap()),
        "bracket inside a comment must not be rainbow-colored, got {:?}",
        comment_style.fg
    );
    assert_eq!(
        comment_style.fg,
        Some(comment_color),
        "bracket inside a comment should keep the comment color"
    );
}
