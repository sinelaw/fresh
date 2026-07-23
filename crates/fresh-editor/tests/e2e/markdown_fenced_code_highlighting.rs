//! E2E: embedded-language regions are highlighted with the named
//! language's grammar, driven end-to-end through rendering — Markdown
//! fenced code blocks (issue #2689) and Vue `<script>`/`<style>` blocks.
//!
//! Before the embedded-language-region mechanism, a fence body was
//! painted uniformly with the raw-code (string) color, and Vue blocks
//! were styled by a hand-rolled keyword list that left identifiers and
//! CSS properties unstyled. These tests assert on rendered cell styles
//! only.

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

/// Vue `<script>`/`<style>` blocks are parsed with real JS/CSS grammars.
/// The fixture's script is `lang="ts"` (no TextMate TS grammar exists),
/// exercising the fall-back-to-default-language path end-to-end. On the
/// old hand-rolled Vue grammar, function names and CSS property names
/// rendered with the default foreground.
#[test]
fn test_vue_script_and_style_blocks_are_language_highlighted() {
    let mut harness = create_harness();
    harness.open_file(&fixture_path("hello.vue")).unwrap();
    harness.render().unwrap();

    // Reference color: plain template text, unstyled by any grammar.
    let plain_fg = fg_at(&harness, "Click me");

    // `function greet() {` — the *name* is entity.name.function under the
    // real JS grammar; find the unique "function greet" occurrence and
    // probe the name's first cell.
    let (col, row) = harness
        .find_text_on_screen("function greet")
        .expect("script content not on screen");
    let name_col = col + "function ".len() as u16;
    let name_fg = harness
        .get_cell_style(name_col, row)
        .and_then(|s| s.fg)
        .expect("no fg at function name");
    assert_ne!(
        name_fg, plain_fg,
        "JS function name inside <script lang=\"ts\"> must be highlighted \
         (default-language fallback); default fg means the block wasn't \
         parsed by a real grammar"
    );

    // `color: blue;` — property names are support.type under the real CSS
    // grammar; the old grammar left them unstyled.
    let css_fg = fg_at(&harness, "color: blue");
    assert_ne!(
        css_fg, plain_fg,
        "CSS property name inside <style> must be highlighted"
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
