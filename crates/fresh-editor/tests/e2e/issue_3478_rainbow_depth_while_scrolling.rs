//! E2E regression test for issue #3478.
//!
//! A rainbow bracket's colour is its nesting depth, so it must not depend on
//! where the viewport is. It used to: the depth stack was seeded empty one
//! viewport-height above the viewport, so once a scope's opener scrolled
//! further up than that, every bracket inside it lost a level and changed
//! colour.
//!
//! Everything here is read off the screen. The colour of each depth is
//! learned from the top of the file, where a nest six deep is on screen at
//! once, and every bracket drawn afterwards is checked against the colour of
//! its true depth, worked out from the file's text.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use ratatui::style::Color;

const WIDTH: u16 = 90;
const HEIGHT: u16 = 20;

/// A JSON file whose first lines nest six deep (one bracket per depth, so
/// one per rainbow colour) followed by a long array of one-line objects.
/// Some strings hold brackets, which are text, not structure.
fn document(entries: usize) -> String {
    let mut text = String::from("{\n  \"meta\": [[[[[\"deep\"]]]]],\n  \"bindings\": [\n");
    for i in 0..entries {
        let note = if i % 7 == 0 {
            "}] in a string"
        } else {
            "plain"
        };
        let comma = if i + 1 == entries { "" } else { "," };
        text.push_str(&format!(
            "    {{ \"key\": \"k{i}\", \"note\": \"{note}\", \"args\": {{}} }}{comma}\n"
        ));
    }
    text.push_str("  ]\n}\n");
    text
}

/// The nesting depth of every structural bracket, by `(line, column)` with
/// lines counted from 1 as the gutter shows them. Mirrors the editor's
/// rule: a closer takes the depth of the opener it closes, and brackets in
/// strings don't count.
fn bracket_depths(text: &str) -> std::collections::HashMap<(usize, usize), usize> {
    let mut depths = std::collections::HashMap::new();
    let mut stack: Vec<char> = Vec::new();
    let mut in_string = false;
    let mut escaped = false;
    for (line_idx, line) in text.lines().enumerate() {
        for (col, ch) in line.chars().enumerate() {
            if in_string {
                if escaped {
                    escaped = false;
                } else if ch == '\\' {
                    escaped = true;
                } else if ch == '"' {
                    in_string = false;
                }
                continue;
            }
            match ch {
                '"' => in_string = true,
                '{' | '[' | '(' => {
                    depths.insert((line_idx + 1, col), stack.len());
                    stack.push(ch);
                }
                '}' | ']' | ')' => {
                    depths.insert((line_idx + 1, col), stack.len().saturating_sub(1));
                    let open = match ch {
                        '}' => '{',
                        ']' => '[',
                        _ => '(',
                    };
                    if stack.last() == Some(&open) {
                        stack.pop();
                    }
                }
                _ => {}
            }
        }
    }
    depths
}

/// For each content row that shows a buffer line: the line number from the
/// gutter and the screen column where the line's text begins.
fn visible_lines(harness: &EditorTestHarness) -> Vec<(u16, usize, u16)> {
    let (first, last) = harness.content_area_rows();
    (first..=last)
        .filter_map(|row| {
            let row = row as u16;
            let text = harness.screen_row_text(row);
            let bar = text.find('│')?;
            let number: usize = text[..bar]
                .trim()
                .trim_start_matches(|c: char| !c.is_ascii_digit())
                .parse()
                .ok()?;
            // The bar is one cell; the text starts one space after it.
            let text_col = text[..bar].chars().count() as u16 + 2;
            Some((row, number, text_col))
        })
        .collect()
}

fn fg_at(harness: &EditorTestHarness, col: u16, row: u16) -> Option<Color> {
    harness.get_cell_style(col, row).and_then(|style| style.fg)
}

/// The colour of each depth 0..6, read from the top of the file: line 1's
/// `{` is depth 0 and line 2 holds `[` at depths 1 through 5.
fn palette_from_top(harness: &EditorTestHarness) -> [Color; 6] {
    let lines = visible_lines(harness);
    let (row1, _, col1) = *lines.iter().find(|(_, n, _)| *n == 1).expect("line 1");
    let (row2, _, col2) = *lines.iter().find(|(_, n, _)| *n == 2).expect("line 2");
    let meta = harness.screen_row_text(row2);
    let first_bracket = meta
        .chars()
        .skip(col2 as usize)
        .position(|c| c == '[')
        .unwrap() as u16;
    let mut palette = [fg_at(harness, col1, row1).expect("depth 0 colour"); 6];
    for (depth, slot) in palette.iter_mut().enumerate().skip(1) {
        *slot = fg_at(harness, col2 + first_bracket + depth as u16 - 1, row2)
            .expect("nested bracket colour");
    }
    let distinct: std::collections::HashSet<_> = palette.iter().collect();
    assert_eq!(distinct.len(), 6, "six depths, six colours: {palette:?}");
    palette
}

/// Every structural bracket on screen has its depth's colour.
fn assert_brackets_coloured_by_depth(
    harness: &EditorTestHarness,
    text: &str,
    palette: &[Color; 6],
    when: &str,
) {
    let depths = bracket_depths(text);
    let mut checked = 0;
    for (row, line, text_col) in visible_lines(harness) {
        let row_text: Vec<char> = harness.screen_row_text(row).chars().collect();
        for (col, ch) in row_text.iter().enumerate().skip(text_col as usize) {
            let column = col - text_col as usize;
            let Some(&depth) = depths.get(&(line, column)) else {
                continue;
            };
            checked += 1;
            let got = fg_at(harness, col as u16, row);
            assert_eq!(
                got,
                Some(palette[depth % 6]),
                "{when}: `{ch}` on line {line} (column {column}) is at depth {depth}\n{}",
                harness.screen_to_string()
            );
        }
    }
    assert!(
        checked > 0,
        "{when}: no brackets on screen\n{}",
        harness.screen_to_string()
    );
}

fn open(text: &str) -> EditorTestHarness {
    // The edge fade shades the rows nearest a scrolled pane's edges toward
    // the background, which would read as a wrong colour here.
    let mut config = Config::default();
    config.editor.viewport_edge_fade = false;
    // A typed opener stays unclosed, which is the edit under test.
    config.editor.auto_close = false;
    let mut harness = EditorTestHarness::create(
        WIDTH,
        HEIGHT,
        HarnessOptions::new()
            .with_config(config)
            .with_project_root()
            .with_full_grammar_registry(),
    )
    .unwrap();
    let file = harness.project_dir().unwrap().join("bindings.json");
    std::fs::write(&file, text).unwrap();
    harness.open_file(&file).unwrap();
    harness.render().unwrap();
    harness
}

/// Wheel-scroll through the file one notch at a time: no position may
/// recolour a bracket.
#[test]
fn rainbow_colours_do_not_change_while_wheel_scrolling() {
    let text = document(400);
    let mut harness = open(&text);
    let palette = palette_from_top(&harness);
    assert_brackets_coloured_by_depth(&harness, &text, &palette, "top of file");

    for notch in 1..=60 {
        harness.mouse_scroll_down(40, 10).unwrap();
        assert_brackets_coloured_by_depth(
            &harness,
            &text,
            &palette,
            &format!("after {notch} notches"),
        );
    }
}

/// Jumping straight to the end — far past any window above the viewport —
/// keeps every depth, and so does scrolling back up from there.
#[test]
fn rainbow_colours_hold_at_the_end_of_a_long_file() {
    let text = document(3000);
    let mut harness = open(&text);
    let palette = palette_from_top(&harness);

    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    assert_brackets_coloured_by_depth(&harness, &text, &palette, "end of file");

    for notch in 1..=10 {
        harness.mouse_scroll_up(40, 10).unwrap();
        assert_brackets_coloured_by_depth(
            &harness,
            &text,
            &palette,
            &format!("{notch} notches up from the end"),
        );
    }
}

/// An edit above the viewport changes the depth of everything after it, and
/// undoing it changes it back. A stale record of depths from before the edit
/// would show the old colours further down.
#[test]
fn rainbow_colours_follow_an_edit_and_its_undo() {
    let text = document(1500);
    let mut harness = open(&text);
    let palette = palette_from_top(&harness);

    // Scroll down first, so depths further down have been worked out once
    // already before the edit invalidates them.
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    assert_brackets_coloured_by_depth(&harness, &text, &palette, "end, before the edit");

    // Open an extra `[` at the start of line 3 — every bracket after it is
    // now one level deeper.
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.type_text("[").unwrap();
    harness.render().unwrap();
    let edited = {
        let mut lines: Vec<&str> = text.lines().collect();
        let third = format!("[{}", lines[2]);
        lines[2] = &third;
        lines.join("\n") + "\n"
    };
    assert_brackets_coloured_by_depth(&harness, &edited, &palette, "top, after the edit");

    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    assert_brackets_coloured_by_depth(&harness, &edited, &palette, "end, after the edit");
    for notch in 1..=5 {
        harness.mouse_scroll_up(40, 10).unwrap();
        assert_brackets_coloured_by_depth(
            &harness,
            &edited,
            &palette,
            &format!("{notch} notches up, after the edit"),
        );
    }

    // Undo the edit; the original depths come back everywhere.
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    assert_brackets_coloured_by_depth(&harness, &text, &palette, "end, after undo");
}
