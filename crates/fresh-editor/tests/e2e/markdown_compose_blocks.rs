//! E2E: block-level markdown elements are rendered, not shown as source, in
//! compose/preview mode (issue #2967, items 2-4).
//!
//! Emphasis, links and inline code were already prettified in compose mode,
//! but block-level syntax was left literal: `# Heading` kept its hashes,
//! `> quote` kept its angle bracket, `---` rendered as three dashes, and list
//! bullets stayed as `-`/`*`. These tests drive the real plugin and assert on
//! rendered output only.
//!
//! Note on the cursor: markup on the cursor's own line is deliberately
//! revealed so the source stays editable. Every fixture here therefore keeps
//! the cursor on line 1 (a plain paragraph) and puts the elements under test
//! further down.

#![cfg(feature = "plugins")]

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use ratatui::style::Modifier;

/// Open `content` as markdown in a harness carrying the real
/// `markdown_compose` plugin, and enable compose mode via the command palette.
///
/// Returns the harness and the temp dir (kept alive for the test's duration).
fn composed(content: &str, width: u16, height: u16) -> (EditorTestHarness, tempfile::TempDir) {
    init_tracing_from_env();

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project");
    std::fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    std::fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "markdown_compose");
    copy_plugin_lib(&plugins_dir);

    let md_path = project_root.join("blocks.md");
    std::fs::write(&md_path, content).unwrap();

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(width, height, Default::default(), project_root)
            .unwrap();
    harness.open_file(&md_path).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("blocks.md");

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Toggle Compose").unwrap();
    harness.wait_for_screen_contains("Toggle Compose").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // `**` disappearing is the established "the conceal pass has run" signal.
    harness
        .wait_until_stable(|h| !h.screen_to_string().contains("**"))
        .unwrap();
    harness.wait_for_async_quiescence(4).unwrap();

    (harness, temp_dir)
}

/// The screen line containing `needle`, panicking with the screen if absent.
fn line_with(harness: &EditorTestHarness, needle: &str) -> String {
    let screen = harness.screen_to_string();
    screen
        .lines()
        .find(|l| l.contains(needle))
        .unwrap_or_else(|| panic!("'{needle}' not on screen.\nScreen:\n{screen}"))
        .to_string()
}

// ---------------------------------------------------------------------------
// Headings (issue item 2, the highest-value one)
// ---------------------------------------------------------------------------

const HEADINGS_MD: &str = "\
Opening paragraph with **emphasis** so the cursor has a home on line 1.

# Alpha Heading

Body under alpha.

## Beta Heading

Body under beta.

### Gamma Heading

Body under gamma.
";

/// ATX `#` markers are concealed, leaving just the heading text.
#[test]
fn test_headings_conceal_hash_markers() {
    let (harness, _tmp) = composed(HEADINGS_MD, 100, 30);

    for heading in ["Alpha Heading", "Beta Heading", "Gamma Heading"] {
        let line = line_with(&harness, heading);
        assert!(
            !line.contains('#'),
            "heading '{heading}' should render without its `#` marker, got: {line:?}\n\
             Screen:\n{}",
            harness.screen_to_string(),
        );
    }
}

/// Heading text is styled by level: bold, and each of the first three levels
/// gets its own colour. Before this change heading lines carried no overlay at
/// all, so they rendered in the plain body foreground.
#[test]
fn test_headings_are_styled_by_level() {
    let (harness, _tmp) = composed(HEADINGS_MD, 100, 30);

    let probe = |text: &str| {
        let (col, row) = harness
            .find_text_on_screen(text)
            .unwrap_or_else(|| panic!("'{text}' not on screen"));
        harness
            .get_cell_style(col, row)
            .unwrap_or_else(|| panic!("no style at '{text}'"))
    };

    let alpha = probe("Alpha Heading");
    let beta = probe("Beta Heading");
    let gamma = probe("Gamma Heading");
    let body = probe("Body under alpha");

    for (name, style) in [("alpha", alpha), ("beta", beta), ("gamma", gamma)] {
        assert!(
            style.add_modifier.contains(Modifier::BOLD),
            "{name} heading should render bold",
        );
    }

    assert_ne!(
        alpha.fg, body.fg,
        "heading colour must differ from body text colour",
    );
    assert_ne!(alpha.fg, beta.fg, "h1 and h2 must be coloured differently");
    assert_ne!(beta.fg, gamma.fg, "h2 and h3 must be coloured differently");
}

/// A `#` that isn't a heading (no space, so CommonMark reads it as text) must
/// be left alone — otherwise `#hashtag` would silently lose its marker.
#[test]
fn test_non_heading_hash_is_left_alone() {
    let md = "\
Opening paragraph with **emphasis** on line 1.

Tagged as #hashtag in the body.
";
    let (harness, _tmp) = composed(md, 100, 30);

    let line = line_with(&harness, "hashtag");
    assert!(
        line.contains("#hashtag"),
        "a bare `#hashtag` is not a heading and must keep its `#`, got: {line:?}",
    );
}
