//! A theme's `chrome` section and `file_icons`, as the screen shows them:
//! which borders exist, the glyphs the chrome is drawn with (and their Nerd
//! Font set, used only while the user says they have the font), and the
//! file-type icons.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use fresh::config_io::DirectoryContext;
use ratatui::style::Color;
use std::fs;

struct Session {
    harness: EditorTestHarness,
    _dir: tempfile::TempDir,
}

/// The editor on `theme` (a theme file's JSON, named "t"), with `config`
/// adjusted by `tweak`, showing `notes.txt` and the file explorer.
fn open(theme: &str, tweak: impl FnOnce(&mut Config)) -> Session {
    open_sized(120, theme, tweak)
}

fn open_sized(width: u16, theme: &str, tweak: impl FnOnce(&mut Config)) -> Session {
    let dir = tempfile::TempDir::new().unwrap();
    let dir_context = DirectoryContext::for_testing(dir.path());
    let themes_dir = dir.path().join("config").join("themes");
    fs::create_dir_all(&themes_dir).unwrap();
    fs::write(themes_dir.join("t.json"), theme).unwrap();

    let project_root = dir.path().join("project_root");
    fs::create_dir_all(project_root.join("plugins")).unwrap();
    fs::create_dir_all(project_root.join("folder")).unwrap();
    fs::write(project_root.join("notes.txt"), "hello world\n").unwrap();

    let mut config = Config {
        theme: "t".to_string().into(),
        ..Default::default()
    };
    tweak(&mut config);
    let mut harness = EditorTestHarness::create(
        width,
        30,
        HarnessOptions::new()
            .with_config(config)
            .with_working_dir(project_root.clone())
            .with_shared_dir_context(dir_context)
            .without_empty_plugins_dir(),
    )
    .unwrap();
    harness.open_file(&project_root.join("notes.txt")).unwrap();
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    // A folder row: it carries its `>` with or without the explorer's box,
    // which is what the helper recognizes a tree row by.
    harness.wait_for_file_explorer_item("folder").unwrap();
    // Leave the explorer, so the editor's own chrome is what is focused.
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    Session { harness, _dir: dir }
}

fn line_with(h: &EditorTestHarness, needle: &str) -> String {
    h.screen_to_string()
        .lines()
        .find(|l| l.contains(needle))
        .unwrap_or_else(|| panic!("no line with {needle:?}:\n{}", h.screen_to_string()))
        .to_string()
}

/// With `gutter_separator` and `file_explorer_border` off, the `│` column
/// and the explorer's box are gone, and the cells with them: the text
/// starts two columns sooner, and the explorer's rows at the screen's edge.
#[test]
fn borderless_chrome_gives_the_cells_back() {
    let boxed = open(r#"{"name": "t", "extends": "builtin://dark"}"#, |_| {});
    let flat = open(
        r#"{"name": "t", "extends": "builtin://dark",
            "chrome": {"gutter_separator": false, "file_explorer_border": false}}"#,
        |_| {},
    );
    let boxed_row = line_with(&boxed.harness, "hello world");
    let flat_row = line_with(&flat.harness, "hello world");
    assert!(boxed_row.contains("│ hello"), "{boxed_row:?}");
    assert!(!flat_row.contains("1 │"), "no separator: {flat_row:?}");
    // From the line number to the text: ` │ ` boxed, one pad cell flat.
    // In cells (characters), not bytes: `│` is three bytes.
    let gap = |row: &str| {
        let text = row.find("hello").unwrap();
        row[row[..text].rfind('1').unwrap()..text].chars().count()
    };
    assert_eq!(
        gap(&flat_row) + 2,
        gap(&boxed_row),
        "the text moves into the separator's two columns: {boxed_row:?} / {flat_row:?}"
    );

    let boxed_explorer = line_with(&boxed.harness, "folder");
    let flat_explorer = line_with(&flat.harness, "folder");
    assert!(boxed_explorer.starts_with('│'), "{boxed_explorer:?}");
    assert!(
        !flat_explorer.starts_with('│') && !flat_explorer.contains("│ "),
        "no explorer walls: {flat_explorer:?}"
    );
}

/// The theme's separators are drawn between tabs and status items, and the
/// user's own `status_bar.separator` wins over the theme's.
#[test]
fn separators_come_from_the_theme_unless_the_user_set_one() {
    let theme = r#"{"name": "t", "extends": "builtin://dark",
        "chrome": {"tab_separator": "¦", "status_bar_separator": "•"}}"#;
    let s = open(theme, |_| {});
    let status = line_with(&s.harness, "Ln 1");
    assert!(status.contains(" • "), "theme separator: {status:?}");

    let mine = open(theme, |c| c.editor.status_bar.separator = "|".into());
    let status = line_with(&mine.harness, "Ln 1");
    assert!(
        status.contains(" | ") && !status.contains('•'),
        "user's wins: {status:?}"
    );
}

/// The `nerd_font` glyphs are drawn only while `editor.nerd_font_icons` is
/// on; without it the plain glyphs beside them are.
#[test]
fn nerd_font_glyphs_follow_the_user_setting() {
    let theme = r#"{"name": "t", "extends": "builtin://dark",
        "chrome": {"panel_refresh": "®", "nerd_font": {"panel_refresh": "Ꞧ"}}}"#;
    let plain = open(theme, |_| {});
    let header = line_with(&plain.harness, "File Explorer");
    assert!(header.contains('®') && !header.contains('Ꞧ'), "{header:?}");

    let nerd = open(theme, |c| c.editor.nerd_font_icons = true);
    let header = line_with(&nerd.harness, "File Explorer");
    assert!(header.contains('Ꞧ') && !header.contains('®'), "{header:?}");
}

/// With the Nerd Font setting on, explorer rows and tabs show the theme's
/// file icons: its glyph, in its color.
#[test]
fn file_icons_follow_the_theme() {
    let theme = r#"{"name": "t", "extends": "builtin://dark",
        "file_icons": {"by_extension": {"txt": {"glyph": "Ŧ", "color": [1, 2, 3]}}}}"#;
    let s = open(theme, |c| c.editor.nerd_font_icons = true);
    let h = &s.harness;
    let screen = h.screen_to_string();
    // The explorer row (the one with the tree's indent) and the tab both
    // carry the icon before the name.
    let rows: Vec<(usize, &str)> = screen
        .lines()
        .enumerate()
        .filter(|(_, l)| l.contains('Ŧ') && l.contains("notes.txt"))
        .collect();
    assert!(
        rows.len() >= 2,
        "on the tab as well as in the explorer:\n{screen}"
    );
    for (y, line) in rows {
        let x = line.chars().position(|c| c == 'Ŧ').unwrap() as u16;
        assert_eq!(
            h.get_cell_style(x, y as u16).and_then(|s| s.fg),
            Some(Color::Rgb(1, 2, 3)),
            "the icon in the theme's color on {line:?}"
        );
    }
}

/// In a column too narrow for the explorer's whole title, the title gives
/// way (`…`) and the header keeps its refresh and close buttons — boxed or
/// not — so the panel can still be closed with the pointer.
#[test]
fn a_narrow_explorer_keeps_its_header_buttons() {
    for theme in [
        r#"{"name": "t", "extends": "builtin://dark"}"#,
        r#"{"name": "t", "extends": "builtin://dark",
            "chrome": {"file_explorer_border": false}}"#,
    ] {
        let s = open_sized(70, theme, |_| {});
        let header = line_with(&s.harness, "File Explor");
        assert!(
            header.contains('…') && header.contains('×') && header.contains('⟳'),
            "title elided, buttons kept: {header:?}"
        );
        let after = header.split('…').nth(1).and_then(|r| r.chars().next());
        assert!(
            matches!(after, Some(' ' | '─')),
            "a gap between the elided title and the buttons: {header:?}"
        );
    }
}

/// Borderless, the explorer's title starts at the edge like the rows under
/// it: there is no box corner to step past.
#[test]
fn a_borderless_explorer_title_has_no_left_padding() {
    let s = open(
        r#"{"name": "t", "extends": "builtin://dark",
            "chrome": {"file_explorer_border": false}}"#,
        |_| {},
    );
    let header = line_with(&s.harness, "File Explorer");
    assert!(header.starts_with("File Explorer"), "{header:?}");
}
