use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::{Config, IndentationGuideMode};
use tempfile::TempDir;

#[test]
fn indentation_guide_render_configured_glyph_in_editor_flow() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("guides.rs");
    std::fs::write(
        &file_path,
        "fn main() {\n    let child = 1;\n        let grand = child + 1;\n}\n",
    )
    .unwrap();

    let mut config = Config::default();
    config.editor.indentation_guide = IndentationGuideMode::All;
    config.editor.indentation_guide_glyph = "┊".to_string();

    let mut harness =
        EditorTestHarness::create(80, 24, HarnessOptions::new().with_config(config)).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("┊   let child = 1;"),
        "configured indentation guide glyph should render on the child line\n{screen}"
    );
    assert!(
        screen.contains("┊   ┊   let grand = child + 1;"),
        "configured indentation guide glyph should render at nested indentation levels\n{screen}"
    );
}

#[test]
fn indentation_guide_keeps_subdued_color_inside_selection() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("guides_selected.rs");
    std::fs::write(
        &file_path,
        "fn main() {\n    let child = 1;\n        let grand = child + 1;\n}\n",
    )
    .unwrap();

    let mut config = Config::default();
    config.editor.indentation_guide = IndentationGuideMode::All;
    config.editor.indentation_guide_glyph = "┊".to_string();

    let mut harness =
        EditorTestHarness::create(80, 24, HarnessOptions::new().with_config(config)).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Locate a guide cell on the deeply-indented "grand" line.
    let (grand_col, grand_row) = harness
        .find_text_on_screen("let grand")
        .expect("expected the nested 'grand' line on screen");
    let guide_col = (0..grand_col)
        .find(|&x| harness.get_cell(x, grand_row).as_deref() == Some("┊"))
        .expect("expected an indentation guide glyph before the 'grand' line text");

    // Style of the guide while it is NOT selected.
    let unselected = harness
        .get_cell_style(guide_col, grand_row)
        .expect("guide cell should have a style");

    // Select the whole buffer so the leading-whitespace guide cells fall
    // inside the selection.
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // The glyph must still be drawn at the same cell.
    assert_eq!(
        harness.get_cell(guide_col, grand_row).as_deref(),
        Some("┊"),
        "indentation guide glyph should remain visible inside a selection"
    );

    let selected = harness
        .get_cell_style(guide_col, grand_row)
        .expect("guide cell should have a style while selected");

    // The selection must actually cover this cell (background changed)...
    assert_ne!(
        selected.bg, unselected.bg,
        "selecting the indentation should apply the selection background to the guide cell"
    );
    // ...but the guide keeps its subdued foreground rather than lighting up to
    // the selection's foreground color.
    assert_eq!(
        selected.fg, unselected.fg,
        "indentation guide should keep its subdued foreground color inside a selection"
    );
}

#[test]
fn indentation_guide_blank_line_does_not_shift_cursor_column_on_move_down() {
    // Regression (issue #2564): a *genuinely empty* line inside an indented
    // block renders synthesized indentation-guide cells that map to no source
    // byte. The visual-column lookup used by vertical movement must ignore
    // those virtual cells; otherwise moving Down off the blank line lands one
    // column too far right and the shift sticks.
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("guides_blank_cursor.rs");
    // Line 3 is genuinely empty (0 chars) between two 4-space-indented lines.
    std::fs::write(
        &file_path,
        "fn main() {\n    let a = 1;\n\n    let b = 2;\n}\n",
    )
    .unwrap();

    let mut config = Config::default();
    config.editor.indentation_guide = IndentationGuideMode::All;

    let mut harness =
        EditorTestHarness::create(80, 24, HarnessOptions::new().with_config(config)).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Cursor starts at line 1, column 0. Step onto the first indented line and
    // record the cursor's on-screen column there.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // -> line 2 "    let a = 1;"
    let (x_indented, _) = harness.screen_cursor_position();

    // Continue down across the blank line onto the next indented line.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // -> line 3 (blank)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // -> line 4 "    let b = 2;"
    let (x_after_blank, _) = harness.screen_cursor_position();

    assert_eq!(
        x_after_blank, x_indented,
        "moving Down across a blank line inside an indented block must keep the cursor in \
         the same column (column 0); the synthesized indentation guide shifted it right"
    );
}

#[test]
fn indentation_guide_all_mode_continues_through_blank_line_in_editor_flow() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("guides_blank.rs");
    // The middle line is whitespace-only (four spaces) inside the indented
    // block, so its column-0 guide cell exists and must be drawn.
    std::fs::write(&file_path, "fn main()\n    above\n    \n    below\n").unwrap();

    let mut config = Config::default();
    config.editor.indentation_guide = IndentationGuideMode::All;

    let mut harness =
        EditorTestHarness::create(80, 24, HarnessOptions::new().with_config(config)).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    let lines: Vec<&str> = screen.lines().collect();
    let above_row = lines
        .iter()
        .position(|line| line.contains("▏   above"))
        .unwrap_or_else(|| panic!("expected a guided 'above' row\n{screen}"));

    // The blank row sits directly below `above` and must carry the guide too,
    // rather than leaving a one-row gap in the vertical line.
    let blank_row = lines[above_row + 1];
    assert!(
        blank_row.contains('▏'),
        "indentation guide should continue through the blank line\nblank row: {blank_row:?}\n{screen}"
    );
    assert!(
        screen.contains("▏   below"),
        "indentation guide should resume on the line after the blank\n{screen}"
    );
}

#[test]
fn indentation_guide_renders_independently_of_line_numbers() {
    // Indentation guides and the line-number gutter are independent preferences:
    // turning line numbers off must NOT take the guides with it. A user can want
    // a chrome-free gutter and still rely on the guides to read code structure.
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("guides_no_line_numbers.rs");
    std::fs::write(&file_path, "fn main() {\n    let child = 1;\n}\n").unwrap();

    for line_numbers in [true, false] {
        let mut config = Config::default();
        config.editor.indentation_guide = IndentationGuideMode::All;
        config.editor.line_numbers = line_numbers;

        let mut harness =
            EditorTestHarness::create(80, 24, HarnessOptions::new().with_config(config)).unwrap();
        harness.open_file(&file_path).unwrap();
        harness.render().unwrap();

        let screen = harness.screen_to_string();
        assert!(
            screen.contains("▏   let child = 1;"),
            "indentation guide should render with line_numbers={line_numbers}\n{screen}"
        );
    }
}

#[test]
fn indentation_guide_all_mode_continues_through_wrapped_line() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("guides_wrap.rs");
    // A doubly-nested line long enough to soft-wrap at the narrow viewport. Its
    // wrapped continuation rows align under the original indent (`wrap_indent`)
    // and must keep the guides of the two enclosing blocks rather than leaving a
    // gap in the vertical lines.
    std::fs::write(
        &file_path,
        "fn main() {\n    if flag {\n        let s = \"aaaa bbbb cccc dddd eeee ffff gggg hhhh iiii jjjj kkkk\";\n    }\n}\n",
    )
    .unwrap();

    let mut config = Config::default();
    config.editor.indentation_guide = IndentationGuideMode::All;
    config.editor.indentation_guide_glyph = "┊".to_string();
    // line_wrap / wrap_indent are on by default; pin them so the test is
    // explicit about the configuration it exercises.
    config.editor.line_wrap = true;
    config.editor.wrap_indent = true;

    // A narrow viewport forces the long `let` line to wrap onto continuation rows.
    let mut harness =
        EditorTestHarness::create(40, 24, HarnessOptions::new().with_config(config)).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    let lines: Vec<&str> = screen.lines().collect();

    // First visual row of the `let` line: guides at the two enclosing levels
    // (columns 0 and 4) — "┊   ┊   let s = ...".
    let let_row = lines
        .iter()
        .position(|line| line.contains("┊   ┊   let s ="))
        .unwrap_or_else(|| panic!("expected a guided 'let' row\n{screen}"));

    // The wrapped continuation row sits directly below and must carry the same
    // two guides; the only `┊` glyphs on it are guides (the wrapped text has none).
    let cont_row = lines[let_row + 1];
    assert!(
        cont_row.contains("┊   ┊"),
        "indent guides should continue through the wrapped continuation row\ncont row: {cont_row:?}\n{screen}"
    );
}

#[test]
fn indentation_guide_active_mode_continues_through_wrapped_line() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("guides_wrap_active.rs");
    std::fs::write(
        &file_path,
        "fn main() {\n    if flag {\n        let s = \"aaaa bbbb cccc dddd eeee ffff gggg hhhh iiii jjjj kkkk\";\n    }\n}\n",
    )
    .unwrap();

    let mut config = Config::default();
    config.editor.indentation_guide = IndentationGuideMode::Active;
    config.editor.indentation_guide_glyph = "┊".to_string();
    config.editor.line_wrap = true;
    config.editor.wrap_indent = true;

    let mut harness =
        EditorTestHarness::create(40, 24, HarnessOptions::new().with_config(config)).unwrap();
    harness.open_file(&file_path).unwrap();
    // Move the cursor onto the wrapped `let` line so its enclosing `if` block
    // becomes the single active guide.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    let lines: Vec<&str> = screen.lines().collect();

    // The `let` line carries the single active guide of the enclosing block.
    let let_row = lines
        .iter()
        .position(|line| line.contains("┊") && line.contains("let s ="))
        .unwrap_or_else(|| panic!("expected an active-guided 'let' row\n{screen}"));
    let guide_col = lines[let_row]
        .find('┊')
        .expect("expected the active guide glyph on the 'let' row");

    // Its wrapped continuation row must carry the same active guide at the same
    // column instead of leaving a gap.
    let cont_row = lines[let_row + 1];
    assert_eq!(
        cont_row.find('┊'),
        Some(guide_col),
        "active indent guide should continue through the wrapped continuation row\ncont row: {cont_row:?}\n{screen}"
    );
}
