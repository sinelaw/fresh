//! E2E regression test for issue #3090.
//!
//! The rainbow classifier counted `<` and `>` as brackets in every language.
//! In C that made a comparison operator push a nesting level nothing popped:
//! the `)` closing `(a < b` no longer shared its opening bracket's colour,
//! and because the `<` stayed unclosed every bracket on the following lines
//! shifted too. One comparison operator recoloured the whole file below it.
//!
//! Angle brackets are structural in markup, and stay coloured there.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};

fn harness() -> EditorTestHarness {
    EditorTestHarness::create(
        120,
        24,
        HarnessOptions::new()
            .with_project_root()
            .with_full_grammar_registry(),
    )
    .unwrap()
}

/// Foreground colour of the cell at `col` of content row `line`.
fn fg_at(harness: &EditorTestHarness, line: u16, col: u16) -> (String, ratatui::style::Color) {
    let buffer = &harness.editor().active_state().buffer;
    let gutter = harness.editor().active_viewport().gutter_width(buffer) as u16;
    let (first_row, _) = harness.content_area_rows();
    let x = gutter + col;
    let y = first_row as u16 + line;
    let cell = harness.get_cell(x, y).unwrap_or_default();
    let style = harness
        .get_cell_style(x, y)
        .unwrap_or_else(|| panic!("no style at line {line} col {col}"));
    (cell, style.fg.expect("a foreground colour"))
}

#[test]
fn a_comparison_does_not_recolour_the_brackets_around_it() {
    let mut harness = harness();
    let project_dir = harness.project_dir().unwrap();
    let file = project_dir.join("br2.c");
    std::fs::write(
        &file,
        "int f(void) { return 1; }\n\
         int g(void) { if (a < b) { return 2; } }\n\
         int h(void) { if (a < b && c > d) { return 3; } }\n",
    )
    .unwrap();
    harness.open_file(&file).unwrap();
    harness.render().unwrap();

    // Line 1: `int g(void) { if (a < b) { return 2; } }`
    //                          ^17 ^20 ^23
    let (open, open_fg) = fg_at(&harness, 1, 17);
    let (angle, angle_fg) = fg_at(&harness, 1, 20);
    let (close, close_fg) = fg_at(&harness, 1, 23);
    assert_eq!((open.as_str(), close.as_str()), ("(", ")"));
    assert_eq!(angle, "<");
    assert_eq!(
        open_fg,
        close_fg,
        "the `)` closing `(a < b` must share its opening bracket's colour\n{}",
        harness.screen_to_string()
    );
    assert_ne!(
        angle_fg,
        open_fg,
        "the comparison operator is not a bracket\n{}",
        harness.screen_to_string()
    );

    // The unclosed `<` on line 1 must not deepen line 2: both lines open
    // their function body at the same nesting level, so at the same colour.
    let (l1_open, l1_fg) = fg_at(&harness, 1, 5);
    let (l2_open, l2_fg) = fg_at(&harness, 2, 5);
    assert_eq!((l1_open.as_str(), l2_open.as_str()), ("(", "("));
    assert_eq!(
        l1_fg,
        l2_fg,
        "a `<` on one line must not shift the brackets on the next\n{}",
        harness.screen_to_string()
    );
}

/// The jump command reads the same table. It had one of its own, so after the
/// classifier stopped counting `<`, go-to-matching-bracket still walked into
/// the `<` of `if (a < b)`, called that comparison the enclosing bracket, and
/// looked for a `>` — landing anywhere but the `)` that actually encloses the
/// cursor.
#[test]
fn the_jump_command_agrees_about_what_a_bracket_is() {
    let mut harness = harness();
    let project_dir = harness.project_dir().unwrap();
    let file = project_dir.join("jump.c");
    // `(` at column 17, `<` at 20, `b` at 22, `)` at 23.
    std::fs::write(&file, "int g(void) { if (a < b) { return 2; } }\n").unwrap();
    harness.open_file(&file).unwrap();
    harness.render().unwrap();

    // Put the caret on the `b` inside the parens.
    for _ in 0..22 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();
    let (before_x, before_y) = harness.screen_cursor_position();
    assert_eq!(
        harness.get_cell(before_x, before_y).as_deref(),
        Some("b"),
        "the caret starts inside the parens\n{}",
        harness.screen_to_string()
    );

    // Ctrl+] — go to matching bracket.
    harness
        .send_key(KeyCode::Char(']'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let (after_x, after_y) = harness.screen_cursor_position();
    assert_eq!(
        (harness.get_cell(after_x, after_y).as_deref(), after_y),
        (Some(")"), before_y),
        "the jump lands on the `)` that encloses the caret\n{}",
        harness.screen_to_string()
    );
}

/// And in markup the same command still jumps between a tag's delimiters.
#[test]
fn the_jump_command_still_pairs_markup_delimiters() {
    let mut harness = harness();
    let project_dir = harness.project_dir().unwrap();
    let file = project_dir.join("jump.html");
    std::fs::write(&file, "<span>hi</span>\n").unwrap();
    harness.open_file(&file).unwrap();
    harness.render().unwrap();

    // The caret starts on the opening `<` of `<span>`.
    harness
        .send_key(KeyCode::Char(']'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let (x, y) = harness.screen_cursor_position();
    assert_eq!(
        harness.get_cell(x, y).as_deref(),
        Some(">"),
        "in HTML the tag's delimiters are still a pair\n{}",
        harness.screen_to_string()
    );
}

#[test]
fn markup_keeps_its_angle_brackets() {
    let mut harness = harness();
    let project_dir = harness.project_dir().unwrap();
    let file = project_dir.join("t.html");
    std::fs::write(&file, "<span>hi</span>\n").unwrap();
    harness.open_file(&file).unwrap();
    harness.render().unwrap();

    let theme = harness.editor().theme();
    let rainbow = [
        theme.bracket_rainbow_1,
        theme.bracket_rainbow_2,
        theme.bracket_rainbow_3,
        theme.bracket_rainbow_4,
        theme.bracket_rainbow_5,
        theme.bracket_rainbow_6,
    ];

    let (open, open_fg) = fg_at(&harness, 0, 0);
    let (close, close_fg) = fg_at(&harness, 0, 5);
    assert_eq!((open.as_str(), close.as_str()), ("<", ">"));
    assert!(
        rainbow.contains(&open_fg),
        "a tag delimiter is a bracket in HTML\n{}",
        harness.screen_to_string()
    );
    assert_eq!(
        open_fg,
        close_fg,
        "`<` and `>` of one tag share a nesting level\n{}",
        harness.screen_to_string()
    );
}
