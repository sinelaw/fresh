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
