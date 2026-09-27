//! E2E coverage for the `Bracket Matching` settings (issue #2842).
//!
//! `editor.highlight_matching_brackets` and `editor.rainbow_brackets` were
//! editable in the Settings UI and persisted to the config, but the render
//! pipeline hard-coded both to on, so neither toggle changed what was drawn.
//!
//! These tests only look at rendered cells: the six brackets of `({[]})` and
//! how many distinct foreground colors they carry.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::collections::HashSet;

const BRACKETS: &str = "({[]})";

/// Foreground colors of the six bracket cells, in screen order.
fn bracket_colors(harness: &EditorTestHarness) -> Vec<Option<ratatui::style::Color>> {
    let (col, row) = harness
        .find_text_on_screen(BRACKETS)
        .expect("bracket line should be on screen");
    (0..BRACKETS.len() as u16)
        .map(|offset| {
            harness
                .get_cell_style(col + offset, row)
                .unwrap_or_default()
                .fg
        })
        .collect()
}

fn distinct_bracket_colors(harness: &EditorTestHarness) -> usize {
    bracket_colors(harness)
        .into_iter()
        .collect::<HashSet<_>>()
        .len()
}

/// The reported flow: flip "Rainbow Brackets" off in the Settings UI and the
/// already-open buffer must stop rendering depth colors.
#[test]
fn settings_ui_toggle_disables_rainbow_brackets() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    harness.type_text(BRACKETS).unwrap();
    harness.render().unwrap();

    assert!(
        distinct_bracket_colors(&harness) > 1,
        "nesting depths should start out in different colors, got {:?}",
        bracket_colors(&harness)
    );

    harness.open_settings().unwrap();
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    harness.type_text("rainbow brackets").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_for_screen_contains("Rainbow Brackets")
        .unwrap();

    // Enter on the focused row flips the checkbox.
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    let screen = harness.screen_to_string();
    let toggled_off = screen
        .lines()
        .find(|line| line.contains("Rainbow Brackets"))
        .expect("settings row should be visible");
    assert!(
        toggled_off.contains("[ ]"),
        "Rainbow Brackets checkbox should read as off, got: {toggled_off}"
    );

    // Save and close the dialog.
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    assert_eq!(
        distinct_bracket_colors(&harness),
        1,
        "brackets should render in a single color once rainbow is off, got {:?}",
        bracket_colors(&harness)
    );
}

/// Same setting straight from the config file.
#[test]
fn rainbow_brackets_config_false_renders_uniform_brackets() {
    let mut config = Config::default();
    config.editor.rainbow_brackets = false;

    let mut harness =
        EditorTestHarness::create(80, 24, HarnessOptions::new().with_config(config)).unwrap();
    // Cursor stays past the last bracket, so nothing is match-highlighted.
    harness.type_text(BRACKETS).unwrap();
    harness.render().unwrap();

    assert_eq!(
        distinct_bracket_colors(&harness),
        1,
        "rainbow_brackets=false should leave every bracket the default color, got {:?}",
        bracket_colors(&harness)
    );
}

/// With rainbow off, the cursor's own pair still gets the match color — the
/// two settings are independent knobs, not one.
#[test]
fn matching_brackets_still_highlight_when_rainbow_is_off() {
    let mut config = Config::default();
    config.editor.rainbow_brackets = false;

    let mut harness =
        EditorTestHarness::create(80, 24, HarnessOptions::new().with_config(config)).unwrap();
    harness.type_text(BRACKETS).unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let colors = bracket_colors(&harness);
    assert_eq!(
        colors[0], colors[5],
        "the cursor's bracket and its match should share the match color, got {colors:?}"
    );
    assert_ne!(
        colors[0], colors[1],
        "the matched pair should stand out from the untouched brackets, got {colors:?}"
    );
    assert_eq!(
        colors[1..5].iter().collect::<HashSet<_>>().len(),
        1,
        "the brackets the cursor isn't on should all be the default color, got {colors:?}"
    );
}

/// `highlight_matching_brackets` is the master switch: with it off, neither
/// the cursor's pair nor the depth colors are drawn.
#[test]
fn highlight_matching_brackets_config_false_disables_all_bracket_colors() {
    let mut config = Config::default();
    config.editor.highlight_matching_brackets = false;

    let mut harness =
        EditorTestHarness::create(80, 24, HarnessOptions::new().with_config(config)).unwrap();
    harness.type_text(BRACKETS).unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    assert_eq!(
        distinct_bracket_colors(&harness),
        1,
        "no bracket coloring should survive the master toggle, got {:?}",
        bracket_colors(&harness)
    );
}

/// With rainbow on, the cursor's pair must still stand out. It used to be
/// re-painted in its own depth color — exactly what the rainbow pass had
/// already given it — so landing on a bracket changed nothing (issue #3084).
#[test]
fn matching_brackets_stand_out_when_rainbow_is_on() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    // Two identical lines: the second line's brackets sit at the same
    // depths as the first's and are not part of the cursor's pair.
    harness.type_text("({[]})\n({[]})").unwrap();
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let (col, row) = harness
        .find_text_on_screen(BRACKETS)
        .expect("bracket line should be on screen");
    // Foreground and text attributes; the rows' backgrounds differ anyway
    // (current-line highlight).
    let look = |x: u16, y: u16| {
        let style = harness.get_cell_style(x, y).unwrap_or_default();
        (style.fg, style.add_modifier)
    };

    // The `)` matching the cursor's `(` vs the same-depth `)` on line 2.
    let matched = look(col + 5, row);
    let unmatched = look(col + 5, row + 1);
    assert_ne!(
        matched, unmatched,
        "the matched bracket must look different from an unmatched one at the same depth"
    );
    assert_eq!(
        matched.0, unmatched.0,
        "the pair keeps its rainbow depth color"
    );
    // The brackets the cursor isn't on are unaffected.
    assert_eq!(look(col + 1, row), look(col + 1, row + 1));
}

/// A theme that extends a built-in one and names only the match emphasis.
const ITALIC_MATCH_THEME_JSON: &str = r#"{
    "name": "italic-match",
    "extends": "dark",
    "editor": {
        "bracket_rainbow_match_modifier": ["italic"]
    },
    "ui": {},
    "search": {},
    "diagnostic": {},
    "syntax": {}
}"#;

/// The rainbow pair's emphasis is the theme's (`editor.bracket_rainbow_match_
/// modifier`), not a fixed bold + underline.
#[test]
fn the_theme_decides_how_the_rainbow_pair_stands_out() {
    use fresh::config_io::DirectoryContext;
    use ratatui::style::Modifier;

    let temp_dir = tempfile::TempDir::new().unwrap();
    let dir_context = DirectoryContext::for_testing(temp_dir.path());
    let themes_dir = temp_dir.path().join("config").join("themes");
    std::fs::create_dir_all(&themes_dir).unwrap();
    std::fs::write(
        themes_dir.join("italic-match.json"),
        ITALIC_MATCH_THEME_JSON,
    )
    .unwrap();
    let project_root = temp_dir.path().join("project_root");
    std::fs::create_dir_all(project_root.join("plugins")).unwrap();

    let config = Config {
        theme: "italic-match".to_string().into(),
        ..Default::default()
    };
    let mut harness = EditorTestHarness::create(
        80,
        24,
        HarnessOptions::new()
            .with_config(config)
            .with_working_dir(project_root)
            .with_shared_dir_context(dir_context)
            .without_empty_plugins_dir(),
    )
    .unwrap();
    harness.type_text("({[]})\n({[]})").unwrap();
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let (col, row) = harness
        .find_text_on_screen(BRACKETS)
        .expect("bracket line should be on screen");
    let modifier = |x: u16, y: u16| {
        harness
            .get_cell_style(x, y)
            .unwrap_or_default()
            .add_modifier
    };
    // The `)` matching the cursor's `(`, and the same-depth `)` on line 2.
    let matched = modifier(col + 5, row);
    let unmatched = modifier(col + 5, row + 1);
    assert!(
        matched.contains(Modifier::ITALIC) && !unmatched.contains(Modifier::ITALIC),
        "the matched bracket carries the theme's attributes: {matched:?} vs {unmatched:?}"
    );
    assert!(
        !matched.intersects(Modifier::BOLD | Modifier::UNDERLINED),
        "the default emphasis is replaced, not added to: {matched:?}"
    );
}
