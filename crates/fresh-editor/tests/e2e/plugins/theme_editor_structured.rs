//! The theme editor on the parts of a theme that are not flat colors: the
//! `chrome` options and glyphs (with their `nerd_font` set), the
//! `file_icons` maps, and a color's text attributes edited from its picker.
//!
//! Every test drives the editor the way a user does — keys in, screen and
//! saved file out.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config_io::DirectoryContext;
use std::fs;

/// The theme every test edits: a user theme with a chrome glyph and icon
/// entries that leave halves unset.
const THEME: &str = r#"{
    "name": "ticons",
    "extends": "builtin://dark",
    "chrome": { "tab_close": "x" },
    "file_icons": {
        "by_extension": {
            "rs": { "color": [1, 2, 3] },
            "md": { "glyph": "M" },
            "zz": { "glyph": "Z" }
        },
        "by_name": {
            "notes": { "glyph": "N" },
            "notes_modifier": { "glyph": "Q" }
        }
    }
}"#;

/// The built-in Rust icon (`nf-dev-rust`) and the built-in Markdown color.
const RUST_GLYPH: &str = "\u{e7a8}";
const MARKDOWN_COLOR: &str = "#DDDDDD";

struct Session {
    harness: EditorTestHarness,
    dir_context: DirectoryContext,
    _dirs: (tempfile::TempDir, tempfile::TempDir),
}

/// The editor, open on the `ticons` theme.
fn open_editor() -> Session {
    let context_temp = tempfile::TempDir::new().unwrap();
    let dir_context = DirectoryContext::for_testing(context_temp.path());
    fs::create_dir_all(dir_context.themes_dir()).unwrap();
    fs::write(dir_context.themes_dir().join("ticons.json"), THEME).unwrap();

    let project_temp = tempfile::TempDir::new().unwrap();
    let project_root = project_temp.path().join("project_root");
    fs::create_dir(&project_root).unwrap();
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "theme_editor");

    let mut harness = EditorTestHarness::with_shared_dir_context(
        160,
        50,
        Default::default(),
        project_root,
        dir_context.clone(),
    )
    .unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Edit Theme").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Select theme to edit"))
        .unwrap();
    harness.type_text("ticons").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            // The tree's header and the picker's first hint: both panels
            // are filled.
            s.contains("Theme Editor: ticons") && s.contains("Select a")
        })
        .unwrap();
    Session {
        harness,
        dir_context,
        _dirs: (context_temp, project_temp),
    }
}

fn key(h: &mut EditorTestHarness, code: KeyCode) {
    h.send_key(code, KeyModifiers::NONE).unwrap();
    h.wait_for_async_quiescence(3).unwrap();
}

/// The tree row the cursor is on: the one marked `▸`.
fn selected_row(h: &EditorTestHarness) -> Option<String> {
    h.screen_to_string()
        .lines()
        .find(|l| l.contains('▸'))
        .map(str::to_string)
}

/// Move the cursor down to the first row (from where it is) whose text
/// contains `needle`.
fn select_row(h: &mut EditorTestHarness, needle: &str) {
    for _ in 0..250 {
        if selected_row(h).is_some_and(|r| r.contains(needle)) {
            return;
        }
        key(h, KeyCode::Down);
    }
    panic!(
        "no row containing {needle:?} below the cursor. Screen:\n{}",
        h.screen_to_string()
    );
}

/// Filter the tree to `text`.
fn filter(h: &mut EditorTestHarness, text: &str) {
    key(h, KeyCode::Char('/'));
    h.wait_until(|h| h.screen_to_string().contains("Filter fields:"))
        .unwrap();
    h.send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    h.type_text(text).unwrap();
    key(h, KeyCode::Enter);
}

/// Open (or close) the header the cursor is on.
fn toggle(h: &mut EditorTestHarness, header: &str) {
    select_row(h, header);
    key(h, KeyCode::Enter);
}

/// Answer an open prompt with `text`, replacing what it was pre-filled with.
fn answer(h: &mut EditorTestHarness, text: &str) {
    h.send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    h.type_text(text).unwrap();
    key(h, KeyCode::Enter);
}

/// Save and read the saved theme file back. A theme the registry lists as
/// built-in is saved under a new name (the editor asks), else in place.
fn save(s: &mut Session) -> serde_json::Value {
    let h = &mut s.harness;
    h.send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_until(|h| {
        let screen = h.screen_to_string();
        screen.contains("saved") || screen.contains("Save theme as:")
    })
    .unwrap();
    let name = if h.screen_to_string().contains("Save theme as:") {
        answer(h, "ticons-saved");
        h.wait_until(|h| h.screen_to_string().contains("saved"))
            .unwrap();
        "ticons-saved"
    } else {
        "ticons"
    };
    let path = s.dir_context.themes_dir().join(format!("{name}.json"));
    serde_json::from_str(&fs::read_to_string(path).unwrap()).unwrap()
}

/// Open the File Icons section and its `by_extension` map, filtered to it.
fn open_by_extension(h: &mut EditorTestHarness) {
    filter(h, "by_extension");
    toggle(h, "File Icons");
}

/// An icon entry's unset glyph is edited from the built-in glyph it draws:
/// confirming the prompt unchanged keeps the Rust icon instead of writing an
/// empty glyph over it.
#[test]
fn unset_icon_glyph_is_edited_from_the_builtin_one() {
    let mut s = open_editor();
    let h = &mut s.harness;
    open_by_extension(h);
    toggle(h, " rs");
    select_row(h, "glyph");
    assert!(
        selected_row(h).unwrap().contains("(built-in)"),
        "the unset glyph shows what it draws: {:?}",
        selected_row(h)
    );
    key(h, KeyCode::Enter);
    h.wait_until(|h| h.screen_to_string().contains("(any text):"))
        .unwrap();
    // The prompt's own line, not the suggestion list below it (which offers
    // the built-in glyphs either way).
    let prompt = h
        .screen_to_string()
        .lines()
        .find(|l| l.contains("(any text):"))
        .unwrap()
        .to_string();
    assert!(
        prompt.contains(RUST_GLYPH),
        "the prompt starts at the built-in glyph: {prompt:?}"
    );
    key(h, KeyCode::Enter);

    let theme = save(&mut s);
    assert_eq!(
        theme["file_icons"]["by_extension"]["rs"]["glyph"], RUST_GLYPH,
        "confirming unchanged keeps the built-in glyph"
    );
}

/// An icon entry's unset color is edited from the built-in color: the
/// prompt starts at Markdown's own color, not a placeholder gray.
#[test]
fn unset_icon_color_is_edited_from_the_builtin_one() {
    let mut s = open_editor();
    let h = &mut s.harness;
    open_by_extension(h);
    toggle(h, " md");
    select_row(h, "color");
    key(h, KeyCode::Enter);
    h.wait_until(|h| h.screen_to_string().contains("(#RRGGBB or named)"))
        .unwrap();
    let prompt = h
        .screen_to_string()
        .lines()
        .find(|l| l.contains("(#RRGGBB or named)"))
        .unwrap()
        .to_string();
    assert!(
        prompt.contains(MARKDOWN_COLOR),
        "the prompt starts at the built-in color: {prompt:?}"
    );
}

/// Keys are written the way the theme loader reads them: an extension
/// without its dot and in lower case, a file name in lower case. A file
/// name the built-in table knows shows its built-in icon.
#[test]
fn added_keys_are_normalized_like_the_loader_reads_them() {
    let mut s = open_editor();
    let h = &mut s.harness;
    filter(h, "by_");
    toggle(h, "File Icons");
    select_row(h, "Add…");
    key(h, KeyCode::Enter);
    h.wait_until(|h| h.screen_to_string().contains("New entry in by_extension"))
        .unwrap();
    answer(h, ".PY");
    select_row(h, "by_name");
    select_row(h, "Add…");
    key(h, KeyCode::Enter);
    h.wait_until(|h| h.screen_to_string().contains("New entry in by_name"))
        .unwrap();
    answer(h, "Cargo.toml");
    h.wait_until(|h| h.screen_to_string().contains("cargo.toml"))
        .unwrap();
    assert!(
        h.screen_to_string()
            .lines()
            .any(|l| l.contains("cargo.toml") && l.contains(RUST_GLYPH)),
        "the entry previews the built-in Cargo.toml icon. Screen:\n{}",
        h.screen_to_string()
    );

    let theme = save(&mut s);
    let icons = &theme["file_icons"];
    assert!(icons["by_extension"].get("py").is_some(), "{icons}");
    assert!(icons["by_extension"].get(".PY").is_none(), "{icons}");
    assert!(icons["by_name"].get("cargo.toml").is_some(), "{icons}");
    assert!(icons["by_name"].get("Cargo.toml").is_none(), "{icons}");
}

/// A filter that matches a group's name shows what the group holds; one
/// that matches a map entry's key finds the entry.
#[test]
fn filter_finds_group_contents_and_map_entries() {
    let mut s = open_editor();
    let h = &mut s.harness;
    filter(h, "nerd");
    toggle(h, "Chrome");
    h.wait_until(|h| h.screen_to_string().contains("nerd_font"))
        .unwrap();
    assert!(
        h.screen_to_string().contains("tab_cap_left"),
        "a group whose name matched shows its fields. Screen:\n{}",
        h.screen_to_string()
    );

    filter(h, "zz");
    toggle(h, "File Icons");
    h.wait_until(|h| h.screen_to_string().contains("by_extension"))
        .unwrap();
    let screen = h.screen_to_string();
    assert!(
        screen.lines().any(|l| l.contains(" zz ")),
        "the entry is found by its key. Screen:\n{screen}"
    );
    assert!(
        !screen.lines().any(|l| l.contains(" rs ")),
        "and only the entries that match. Screen:\n{screen}"
    );
}

/// An attribute the editor does not know reopens the prompt rather than
/// dropping the input.
#[test]
fn unknown_attribute_reopens_the_prompt() {
    let mut s = open_editor();
    let h = &mut s.harness;
    filter(h, "comment");
    select_row(h, "comment");
    key(h, KeyCode::Tab);
    key(h, KeyCode::Up);
    key(h, KeyCode::Enter);
    h.wait_until(|h| h.screen_to_string().contains("Text attributes for"))
        .unwrap();
    // Confirming closes the prompt; the editor opens it again for the
    // correction (the reopened prompt covers the status line that names the
    // unknown word).
    answer(h, "bolt");
    assert!(
        h.screen_to_string().contains("Text attributes for"),
        "the prompt is open again. Screen:\n{}",
        h.screen_to_string()
    );
}

/// Attributes edited from the picker are saved bundled with their color.
#[test]
fn picker_attributes_are_saved_with_their_color() {
    let mut s = open_editor();
    let h = &mut s.harness;
    filter(h, "comment");
    select_row(h, "comment");
    key(h, KeyCode::Tab);
    key(h, KeyCode::Up);
    key(h, KeyCode::Enter);
    h.wait_until(|h| h.screen_to_string().contains("Text attributes for"))
        .unwrap();
    answer(h, "bold, italic");
    h.wait_until(|h| h.screen_to_string().contains("+bold,italic"))
        .unwrap();

    let theme = save(&mut s);
    assert_eq!(
        theme["syntax"]["comment"]["modifier"],
        serde_json::json!(["bold", "italic"]),
        "{}",
        theme["syntax"]["comment"]
    );
}

/// Resetting a chrome glyph shows the default it now draws, not "(unset)".
#[test]
fn reset_chrome_glyph_shows_its_default() {
    let mut s = open_editor();
    let h = &mut s.harness;
    filter(h, "tab_close");
    toggle(h, "Chrome");
    // The theme's own `tab_close` (its `nerd_font` copy sorts first).
    select_row(h, "\"x\"");
    key(h, KeyCode::Delete);
    h.wait_until(|h| {
        selected_row(h).is_some_and(|r| r.contains("\"×\"") && r.contains("(default)"))
    })
    .unwrap();
}

/// Removing a map entry removes that entry only: an entry whose name ends in
/// `_modifier` is a different entry, not an attribute of the first.
#[test]
fn removing_an_entry_keeps_its_modifier_named_sibling() {
    let mut s = open_editor();
    let h = &mut s.harness;
    filter(h, "by_name");
    toggle(h, "File Icons");
    select_row(h, " notes ");
    key(h, KeyCode::Delete);
    h.wait_until(|h| !h.screen_to_string().lines().any(|l| l.contains(" notes  ")))
        .unwrap();

    let theme = save(&mut s);
    let by_name = &theme["file_icons"]["by_name"];
    assert!(by_name.get("notes").is_none(), "{by_name}");
    assert_eq!(by_name["notes_modifier"]["glyph"], "Q", "{by_name}");
}
