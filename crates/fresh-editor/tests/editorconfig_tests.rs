// End-to-end tests for `.editorconfig` support (issue #959).
//
// Per CONTRIBUTING.md ("E2E Tests Observe, Not Inspect"), these drive real
// keyboard events and assert only on *rendered* output — never on model state.
// The observable signal is Fresh's default leading-whitespace rendering: a
// leading tab is drawn as the `→` glyph (`whitespace_tabs_leading` defaults to
// true) while leading spaces are not, so `indent_style` is directly visible on
// screen.
//
// Files use a language-less extension (`.dat` / `.zzz`) so the assertion
// isolates `.editorconfig` behavior from any language-specific Tab handling
// (e.g. Markdown list indentation). Non-visible details (parsing, glob
// matching, size mapping, precedence) are covered by unit tests on the
// resolver in `services::editorconfig`.

mod common;

use common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::fs;

const MARK: &str = "ZZMARK";
const TAB_GLYPH: char = '→';

/// Type an indent (Tab) followed by a unique marker, render, and return the
/// rendered screen row that contains the marker.
fn indent_and_render_row(harness: &mut EditorTestHarness) -> String {
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.type_text(MARK).unwrap();
    harness.render().unwrap();
    harness
        .screen_to_string()
        .lines()
        .find(|line| line.contains(MARK))
        .unwrap_or_else(|| panic!("no rendered row contained the marker {MARK:?}"))
        .to_string()
}

/// `indent_style = tab` makes the indent render as a tab (the `→` glyph),
/// overriding the editor default (spaces, no glyph).
#[test]
fn editorconfig_tab_style_renders_as_tab() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let dir = harness.temp_dir_path().unwrap().to_path_buf();

    fs::write(
        dir.join(".editorconfig"),
        "root = true\n[*]\nindent_style = tab\n",
    )
    .unwrap();
    let file = dir.join("notes.dat");
    fs::write(&file, "").unwrap();

    harness.open_file(&file).unwrap();
    let row = indent_and_render_row(&mut harness);

    assert!(
        row.contains(TAB_GLYPH),
        "indent_style=tab should render a tab glyph; got row {row:?}"
    );
}

/// `indent_style = space` overrides an editor configured to use tabs: the
/// indent renders as spaces (no `→` glyph).
#[test]
fn editorconfig_space_style_overrides_default_tabs() {
    // Start from an editor whose global default is tabs, so the space override
    // is observable (without it, the row would render a tab glyph).
    let mut config = Config::default();
    config.editor.use_tabs = true;

    let mut harness = EditorTestHarness::with_temp_project_and_config(80, 24, config).unwrap();
    let dir = harness.temp_dir_path().unwrap().to_path_buf();

    fs::write(
        dir.join(".editorconfig"),
        "root = true\n[*]\nindent_style = space\nindent_size = 2\n",
    )
    .unwrap();
    let file = dir.join("notes.dat");
    fs::write(&file, "").unwrap();

    harness.open_file(&file).unwrap();
    let row = indent_and_render_row(&mut harness);

    assert!(
        !row.contains(TAB_GLYPH),
        "indent_style=space should render spaces, not a tab glyph; got row {row:?}"
    );
}

/// A section glob scopes settings to matching files only: a matching file
/// renders a tab, a non-matching file keeps the editor default (spaces).
#[test]
fn editorconfig_section_glob_scopes_settings() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let dir = harness.temp_dir_path().unwrap().to_path_buf();

    // Tabs only for *.dat; other files keep the editor default (spaces).
    fs::write(
        dir.join(".editorconfig"),
        "root = true\n[*.dat]\nindent_style = tab\n",
    )
    .unwrap();

    let matching = dir.join("matches.dat");
    fs::write(&matching, "").unwrap();
    harness.open_file(&matching).unwrap();
    let row = indent_and_render_row(&mut harness);
    assert!(
        row.contains(TAB_GLYPH),
        "*.dat matches the section and should render a tab; got row {row:?}"
    );

    // A different, language-less extension that does not match [*.dat].
    let other = dir.join("other.zzz");
    fs::write(&other, "").unwrap();
    harness.open_file(&other).unwrap();
    let row = indent_and_render_row(&mut harness);
    assert!(
        !row.contains(TAB_GLYPH),
        "non-matching files keep the default space indentation; got row {row:?}"
    );
}
