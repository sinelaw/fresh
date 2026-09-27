//! JavaScript / TypeScript highlighting through the vendored TextMate grammars.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use ratatui::style::Color;
use std::path::Path;

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

fn open(harness: &mut EditorTestHarness, name: &str, content: &str) {
    let path = harness.project_dir().unwrap().join(name);
    std::fs::write(&path, content).unwrap();
    harness.open_file(Path::new(&path)).unwrap();
    harness.render().unwrap();
}

/// Foreground colour of the first cell of the lowest on-screen `needle`;
/// the lowest, because the rows at the top edge fade out when scrolled.
fn fg_of(harness: &EditorTestHarness, needle: &str) -> Option<Color> {
    let (x, y) = (0..harness.terminal_height() as u16)
        .rev()
        .find_map(|y| {
            let row = harness.get_row_text(y);
            row.find(needle)
                .map(|i| (row[..i].chars().count() as u16, y))
        })
        .unwrap_or_else(|| panic!("{needle:?} not on screen:\n{}", harness.screen_to_string()));
    harness.get_cell_style(x, y).and_then(|s| s.fg)
}

const FILLER_FUNCTIONS: usize = 6000;

/// A TypeScript file several hundred KB long ending in a multi-line construct
/// (`opening` .. `closing`) and a few short functions.
fn large_ts_source(opening: &str, body_line: &str, closing: &str) -> String {
    let mut src = String::new();
    for i in 0..FILLER_FUNCTIONS {
        src.push_str(&format!(
            "export function filler{i}(x: number): number {{ return x * {i}; }}\n"
        ));
    }
    src.push_str(opening);
    src.push('\n');
    for _ in 0..40 {
        src.push_str(body_line);
        src.push('\n');
    }
    src.push_str(closing);
    src.push('\n');
    for i in 0..8 {
        src.push_str(&format!(
            "function tail{i}(): string {{ return \"t{i}\"; }}\n"
        ));
    }
    src
}

/// Typing deep in a large `.ts` file keeps highlighting correct on both sides
/// of the edit and re-parses only around the viewport, never from byte 0.
fn assert_typing_stays_incremental(
    opening: &str,
    body_line: &str,
    closing: &str,
    body_needle: &str,
    body_is_comment: bool,
) {
    let src = large_ts_source(opening, body_line, closing);
    let mut harness = create_harness();
    open(&mut harness, "big.ts", &src);
    // Ctrl+End then up to the start of `function tail4`; the construct's
    // opening line is scrolled off above the viewport.
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    for _ in 0..4 {
        harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    }
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    assert!(
        harness.find_text_on_screen(opening).is_none(),
        "the construct must open above the viewport"
    );

    let theme = harness.editor().theme().clone();
    let body_colour = if body_is_comment {
        theme.syntax_comment
    } else {
        theme.syntax_string
    };
    assert_eq!(fg_of(&harness, body_needle), Some(body_colour));
    assert_eq!(
        fg_of(&harness, "function tail4"),
        Some(theme.syntax_keyword)
    );

    harness.reset_highlight_stats();
    let typed = "let edited = 1; ";
    for ch in typed.chars() {
        harness
            .send_key(KeyCode::Char(ch), KeyModifiers::NONE)
            .unwrap();
    }

    let cursor = harness.cursor_position();
    assert!(
        cursor > src.len() / 2,
        "edit should be deep in the file (at {cursor} of {})",
        src.len()
    );
    let stats = harness.highlight_stats().expect("TextMate engine").clone();
    assert!(
        stats.cache_misses >= 1,
        "edits must re-highlight: {stats:?}"
    );
    // One re-parse from the top of the file would alone exceed this.
    assert!(
        stats.bytes_parsed < 64 * 1024,
        "{} keystrokes at byte {cursor} must re-parse only near the viewport: {stats:?}",
        typed.len()
    );

    // The construct opened above the viewport still colours its body, the
    // typed text is highlighted, and the token after the edit keeps its category.
    assert_eq!(fg_of(&harness, body_needle), Some(body_colour));
    assert_eq!(fg_of(&harness, "let edited"), Some(theme.syntax_keyword));
    assert_eq!(
        fg_of(&harness, "function tail4"),
        Some(theme.syntax_keyword)
    );
    assert_eq!(fg_of(&harness, "tail4"), Some(theme.syntax_function));

    // A render with no edit in between re-parses nothing.
    harness.reset_highlight_stats();
    harness.render().unwrap();
    assert_eq!(harness.highlight_stats().unwrap().bytes_parsed, 0);
}

#[test]
fn test_typing_in_large_ts_file_below_block_comment() {
    assert_typing_stays_incremental(
        "/*",
        " * commentary line inside a long block comment",
        " */",
        "commentary line",
        true,
    );
}

#[test]
fn test_typing_in_large_ts_file_below_template_literal() {
    assert_typing_stays_incremental(
        "const banner = `",
        "templated banner text line",
        "`;",
        "templated banner",
        false,
    );
}

/// Issue #899: a class field whose initialiser is an arrow function returning
/// a template literal must not leave the rest of the file coloured as string.
#[test]
fn test_js_class_field_template_literal_does_not_leak() {
    let mut harness = create_harness();
    open(
        &mut harness,
        "greeter.js",
        "class Greeter {\n  greet = (name) => `Hello, ${name}!`;\n  wave = () => `bye`;\n}\nfunction after() {\n  return 1;\n}\n",
    );
    let theme = harness.editor().theme().clone();
    assert_eq!(fg_of(&harness, "`bye"), Some(theme.syntax_string));
    assert_eq!(
        fg_of(&harness, "function after"),
        Some(theme.syntax_keyword)
    );
    assert_eq!(fg_of(&harness, "return"), Some(theme.syntax_keyword));
}

/// `.tsx` uses the TypeScriptReact grammar, so JSX parses as markup; `.ts`
/// keeps the plain TypeScript grammar, where `<T>expr` is a type assertion.
#[test]
fn test_tsx_uses_jsx_aware_grammar_and_ts_does_not() {
    let mut harness = create_harness();
    open(
        &mut harness,
        "view.tsx",
        "const el = <div className=\"box\">{label}</div>;\nfunction after(): number { return 1; }\n",
    );
    let theme = harness.editor().theme().clone();
    assert_eq!(fg_of(&harness, "className"), Some(theme.syntax_constant));
    assert_eq!(fg_of(&harness, "\"box\""), Some(theme.syntax_string));
    assert_eq!(
        fg_of(&harness, "function after"),
        Some(theme.syntax_keyword)
    );

    let mut harness = create_harness();
    open(
        &mut harness,
        "cast.ts",
        "const n = <number>value;\nfunction after(): number { return 1; }\n",
    );
    assert_eq!(fg_of(&harness, "number>"), Some(theme.syntax_type));
    assert_eq!(
        fg_of(&harness, "function after"),
        Some(theme.syntax_keyword)
    );
}
