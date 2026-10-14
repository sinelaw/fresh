//! Pascal auto-indentation, end to end.
//!
//! The unit tests in `primitives::indent_pascal` cover the frame stack case by
//! case; this file checks the two things only the whole editor can show —
//! that a `.pas` buffer is actually routed to that tier when the user presses
//! Enter, and that the electric dedent places a typed closer on its construct.
//!
//! The headline case is the one a one-reference-line rule cannot express:
//!
//! ```pascal
//! begin
//!     if a then
//!         if b then
//!             X;
//!     (here)
//! ```
//!
//! `(here)` belongs at the `begin`'s body level, two levels out from the line
//! above it. Deriving the indent from `X;` — whatever is done to it — can only
//! ever produce `X;`'s own level or one unit either side of it.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use tempfile::TempDir;

/// Open `content` as a `.pas` file with auto-indent on, cursor at the end.
fn pascal(content: &str) -> (EditorTestHarness, TempDir) {
    let temp = TempDir::new().unwrap();
    let path = temp.path().join("unit.pas");
    std::fs::write(&path, content).unwrap();

    let mut config = Config::default();
    config.editor.auto_indent = true;
    // **The full registry is load-bearing.** The indent tier is reached
    // through `highlighter.syntax_name()`; with the default test registry a
    // `.pas` buffer has no syntect grammar, that returns `None`, and the
    // editor falls all the way through to the language-agnostic heuristic —
    // which copies the previous line's indent and knows nothing about
    // `begin`. Every test here then measures the fallback instead of Pascal.
    let mut harness = EditorTestHarness::create(
        90,
        24,
        HarnessOptions::new()
            .with_config(config)
            .with_full_grammar_registry()
            .without_empty_plugins_dir(),
    )
    .unwrap();
    harness.open_file(&path).unwrap();
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    (harness, temp)
}

/// Columns of leading whitespace on the buffer's last line.
fn last_line_indent(harness: &EditorTestHarness) -> usize {
    let content = harness.get_buffer_content().unwrap();
    content
        .lines()
        .next_back()
        .map(|line| line.len() - line.trim_start().len())
        .unwrap_or(0)
}

/// Pressing Enter after a satisfied one-statement body comes back out to the
/// enclosing block, however deeply the satisfied bodies were nested.
#[test]
fn a_new_line_returns_to_the_enclosing_block() {
    let (mut harness, _t) =
        pascal("program D;\nbegin\n    if a then\n        if b then\n            X;");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    assert_eq!(
        last_line_indent(&harness),
        4,
        "two `then` bodies were opened and both satisfied, so the next \
         statement belongs to the `begin`. Buffer:\n{}",
        harness.get_buffer_content().unwrap()
    );
}

/// …and a body that has *not* been satisfied keeps its promise: Enter after a
/// trailing `then` indents.
#[test]
fn a_new_line_after_then_indents() {
    let (mut harness, _t) = pascal("program D;\nbegin\n    if a then");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    assert_eq!(
        last_line_indent(&harness),
        8,
        "the `then` promises a statement, which goes one level in. Buffer:\n{}",
        harness.get_buffer_content().unwrap()
    );
}

/// Typing `end` puts it on its `begin`, not one unit left of the line above.
#[test]
fn a_typed_end_lands_on_its_begin() {
    let (mut harness, _t) =
        pascal("program D;\nbegin\n    if a then\n        if b then\n            X;");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    for c in "end".chars() {
        harness
            .send_key(KeyCode::Char(c), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    let last = content.lines().next_back().unwrap();
    assert_eq!(
        last, "end",
        "`end` closes the `begin` at column 0. Buffer:\n{content}"
    );
}

/// `until` finds its `repeat` the same way.
#[test]
fn a_typed_until_lands_on_its_repeat() {
    let (mut harness, _t) = pascal("program D;\nrepeat\n    X;");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    for c in "until".chars() {
        harness
            .send_key(KeyCode::Char(c), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    assert_eq!(
        content.lines().next_back().unwrap(),
        "until",
        "Buffer:\n{content}"
    );
}

/// A `begin…end` block indents its contents, which is the case the old
/// line-oriented rules already got right — kept so the rewrite cannot regress
/// it while fixing the harder ones.
#[test]
fn a_begin_still_indents_its_body() {
    let (mut harness, _t) = pascal("program D;\nbegin");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    assert_eq!(last_line_indent(&harness), 4);
}
