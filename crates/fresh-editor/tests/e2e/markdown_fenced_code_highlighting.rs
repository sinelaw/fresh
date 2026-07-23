//! E2E: Markdown fenced code blocks are highlighted with the fenced
//! language's grammar (issue #2689), driven end-to-end through rendering.
//!
//! Before the embedded-language-region mechanism, the whole fence body was
//! painted uniformly with the raw-code (string) color, so keyword/number/
//! string tokens inside a ```rust block all had the same foreground.
//! These tests assert on rendered cell styles only.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use ratatui::style::Color;
use std::path::PathBuf;

fn fixture_path(filename: &str) -> PathBuf {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    PathBuf::from(manifest_dir)
        .join("tests/fixtures/syntax_highlighting")
        .join(filename)
}

fn create_harness() -> EditorTestHarness {
    EditorTestHarness::create(
        120,
        40,
        HarnessOptions::new()
            .with_project_root()
            .with_full_grammar_registry(),
    )
    .unwrap()
}

/// Foreground color of the first cell of `text` on screen.
fn fg_at(harness: &EditorTestHarness, text: &str) -> Color {
    let (col, row) = harness
        .find_text_on_screen(text)
        .unwrap_or_else(|| panic!("'{text}' not found on screen"));
    harness
        .get_cell_style(col, row)
        .and_then(|s| s.fg)
        .unwrap_or_else(|| panic!("no fg style at '{text}' ({col},{row})"))
}

/// Tokens of different kinds inside a ```rust fence must render with
/// different foregrounds (keyword vs string vs number). With the old
/// uniform raw-code styling they were all the same color.
#[test]
fn test_markdown_rust_fence_has_language_colors() {
    let mut harness = create_harness();
    harness.open_file(&fixture_path("fenced_code.md")).unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("fn answer");
    let keyword_fg = fg_at(&harness, "fn answer");
    let string_fg = fg_at(&harness, "hello");
    let number_fg = fg_at(&harness, "42");

    assert_ne!(
        keyword_fg, string_fg,
        "keyword and string inside a rust fence must differ — a uniform \
         color means the fence body still uses raw-code styling"
    );
    assert_ne!(keyword_fg, number_fg, "keyword vs number must differ");
}

/// A fence naming an unknown language keeps the uniform raw-code styling.
#[test]
fn test_markdown_unknown_fence_language_stays_uniform() {
    let mut harness = create_harness();
    harness.open_file(&fixture_path("fenced_code.md")).unwrap();
    harness.render().unwrap();

    // The nosuchlanguage block contains "fn answer() -> u32 { 42 }" on one
    // line; keyword and number positions must render identically there.
    let (col, row) = harness
        .find_text_on_screen("fn answer() -> u32 { 42 }")
        .expect("unknown-language fence content not on screen");
    let fg_fn = harness.get_cell_style(col, row).and_then(|s| s.fg);
    let number_col = col + "fn answer() -> u32 { ".len() as u16;
    let fg_num = harness.get_cell_style(number_col, row).and_then(|s| s.fg);
    assert_eq!(
        fg_fn, fg_num,
        "unknown fence language must keep uniform raw-code styling"
    );
}

/// Typing new code inside a fence gets language highlighting immediately
/// (exercises the engine's incremental partial-update path end-to-end).
#[test]
fn test_typing_inside_fence_is_highlighted() {
    let mut harness = create_harness();
    harness.open_file(&fixture_path("fenced_code.md")).unwrap();
    harness.render().unwrap();

    // Move to the "42" line (line 8) inside the rust fence and add a line
    // below it.
    harness
        .send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("8").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("    return 7;").unwrap();
    harness.render().unwrap();

    let keyword_fg = fg_at(&harness, "return");
    let number_fg = fg_at(&harness, "7;");
    assert_ne!(
        keyword_fg, number_fg,
        "code typed into a fence must be highlighted by the fence language"
    );
}
