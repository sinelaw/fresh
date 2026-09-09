//! E2E coverage for `[languages.<id>] textmate_grammar` (issue #2842).
//!
//! The field was parsed, merged, copied into `BufferConfig` and offered in the
//! schema, but nothing ever loaded the file it pointed at, so a configured
//! grammar never highlighted anything.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use fresh::config::{Config, LanguageConfig};
use std::collections::HashSet;
use tempfile::TempDir;

/// A grammar that scopes `SPECIAL` as a keyword and leaves everything else be.
const GRAMMAR: &str = r#"%YAML 1.2
---
name: Widget
file_extensions: [widget]
scope: source.widget
contexts:
  main:
    - match: '\bSPECIAL\b'
      scope: keyword.control.widget
"#;

/// Foreground colors of the `n` cells starting at `text` on screen.
fn colors_of(harness: &EditorTestHarness, text: &str) -> Vec<Option<ratatui::style::Color>> {
    let (col, row) = harness.find_text_on_screen(text).unwrap_or_else(|| {
        panic!(
            "expected {text:?} on screen:\n{}",
            harness.screen_to_string()
        )
    });
    (0..text.len() as u16)
        .map(|offset| {
            harness
                .get_cell_style(col + offset, row)
                .unwrap_or_default()
                .fg
        })
        .collect()
}

fn harness_with_grammar(grammar_path: Option<std::path::PathBuf>) -> EditorTestHarness {
    let mut config = Config::default();
    config.languages.insert(
        "widget".to_string(),
        LanguageConfig {
            extensions: vec!["widget".to_string()],
            textmate_grammar: grammar_path,
            ..Default::default()
        },
    );
    EditorTestHarness::create(80, 24, HarnessOptions::new().with_config(config)).unwrap()
}

#[test]
fn configured_grammar_file_highlights_the_language() {
    let dir = TempDir::new().unwrap();
    let grammar_path = dir.path().join("Widget.sublime-syntax");
    std::fs::write(&grammar_path, GRAMMAR).unwrap();
    let file_path = dir.path().join("thing.widget");
    std::fs::write(&file_path, "SPECIAL plain\n").unwrap();

    let mut harness = harness_with_grammar(Some(grammar_path));
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    let special = colors_of(&harness, "SPECIAL");
    let plain = colors_of(&harness, "plain");
    assert_eq!(
        special.iter().collect::<HashSet<_>>().len(),
        1,
        "the whole keyword should share one color, got {special:?}"
    );
    assert_ne!(
        special[0], plain[0],
        "the configured grammar scopes SPECIAL as a keyword, so it must not \
         render in the plain-text color (got {special:?} vs {plain:?})"
    );
}

/// Without `textmate_grammar` the same file has no grammar to highlight it, so
/// the test above is measuring the setting rather than some default rule.
#[test]
fn without_configured_grammar_the_language_is_unhighlighted() {
    let dir = TempDir::new().unwrap();
    let file_path = dir.path().join("thing.widget");
    std::fs::write(&file_path, "SPECIAL plain\n").unwrap();

    let mut harness = harness_with_grammar(None);
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    let special = colors_of(&harness, "SPECIAL");
    let plain = colors_of(&harness, "plain");
    assert_eq!(
        special[0], plain[0],
        "with no grammar configured nothing should stand out, got {special:?} vs {plain:?}"
    );
}
