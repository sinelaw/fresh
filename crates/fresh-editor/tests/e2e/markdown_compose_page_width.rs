//! E2E: compose/preview mode opens at a readable measure by default
//! (issue #2967, item 1).
//!
//! Toggling compose used to leave the plugin's compose width unset, which the
//! render layer reads as "no page width" and lays the document out across the
//! full pane. Every other markdown preview (Obsidian, VS Code, Zed) constrains
//! the measure by default, and long lines at 160+ columns are genuinely hard to
//! read.
//!
//! The plugin now resolves an unset width from the editor config
//! (`languages.<lang>.page_width`, else `editor.page_width`, whose own default
//! is 80), so compose centers by default. `editor.page_width = None` still
//! selects the old full-width behaviour.
//!
//! Both tests assert on rendered output only: whether the document's text is
//! inset from column 0.

#![cfg(feature = "plugins")]

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;

/// Distinctive heading text — unique on screen, so its indent is unambiguous.
const HEADING: &str = "Readable Measure";

const MD_CONTENT: &str = "\
# Readable Measure

Body text with **emphasis** under the heading, long enough that a full-width
layout and a centered eighty-column layout are plainly different renderings.
";

/// Build a project dir containing the real `markdown_compose` plugin and a
/// markdown file, then open it in a harness at `width` x `height`.
fn compose_harness(
    config: Config,
    width: u16,
    height: u16,
) -> (EditorTestHarness, tempfile::TempDir) {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project");
    std::fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    std::fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "markdown_compose");
    copy_plugin_lib(&plugins_dir);

    let md_path = project_root.join("measure.md");
    std::fs::write(&md_path, MD_CONTENT).unwrap();

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(width, height, config, project_root)
            .unwrap();
    harness.open_file(&md_path).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("measure.md");

    (harness, temp_dir)
}

/// Enable compose mode through the command palette, then wait until the body's
/// `**` emphasis markers are concealed — the signal that the plugin's
/// decoration pass has run and the compose layout has settled.
fn toggle_compose(harness: &mut EditorTestHarness) {
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

    harness
        .wait_until_stable(|h| !h.screen_to_string().contains("**"))
        .unwrap();
    harness.wait_for_async_quiescence(4).unwrap();
}

/// Left margin of the heading line, in columns.
///
/// Measured as the line's leading whitespace rather than as the offset of the
/// heading text: the heading sits on line 1, where the cursor is, so its `#`
/// marker is deliberately revealed and would otherwise be counted as two
/// columns of "indent". Counted in chars, not bytes — `str::find` returns a
/// byte offset, which inflates any line carrying a multi-byte glyph.
fn heading_indent(harness: &EditorTestHarness) -> usize {
    let screen = harness.screen_to_string();
    let line = screen
        .lines()
        .find(|l| l.contains(HEADING))
        .unwrap_or_else(|| panic!("heading not on screen.\nScreen:\n{screen}"));
    line.chars().take_while(|c| *c == ' ').count()
}

/// With a default config, toggling compose must inset the text — the document
/// is centered at the configured page width (80) inside a 140-column pane, so
/// the heading starts around column 30, not column 0.
///
/// Before the fix the plugin passed no compose width and the heading rendered
/// flush at column 0.
#[test]
fn test_compose_defaults_to_readable_measure() {
    init_tracing_from_env();

    let (mut harness, _tmp) = compose_harness(Config::default(), 140, 30);
    toggle_compose(&mut harness);

    let indent = heading_indent(&harness);
    assert!(
        indent > 10,
        "compose mode should center the document at the default page width, \
         leaving a wide left margin; heading started at column {indent} \
         (flush-left means no page width was applied).\nScreen:\n{}",
        harness.screen_to_string(),
    );
}

/// The pre-existing full-width rendering stays reachable: clearing
/// `editor.page_width` (config `0` / `null`) makes compose use the whole pane
/// again, so the heading renders flush left.
#[test]
fn test_compose_full_width_still_reachable_via_config() {
    init_tracing_from_env();

    let mut config = Config::default();
    config.editor.page_width = None;

    let (mut harness, _tmp) = compose_harness(config, 140, 30);
    toggle_compose(&mut harness);

    let indent = heading_indent(&harness);
    assert_eq!(
        indent,
        0,
        "with editor.page_width cleared, compose must use the full pane width \
         and render flush left; heading started at column {indent}.\nScreen:\n{}",
        harness.screen_to_string(),
    );
}

/// An explicit language-level `page_width` still wins over the global default.
/// A narrow 40-column measure inside a 140-column pane centers with a ~50
/// column margin — visibly wider than the default-80 margin.
#[test]
fn test_language_page_width_overrides_default() {
    init_tracing_from_env();

    let mut config = Config::default();
    let lang = config.languages.entry("markdown".to_string()).or_default();
    lang.page_width = Some(40);

    let (mut harness, _tmp) = compose_harness(config, 140, 30);
    toggle_compose(&mut harness);

    let indent = heading_indent(&harness);
    assert!(
        indent > 40,
        "an explicit languages.markdown.page_width of 40 should center more \
         narrowly than the default 80; heading started at column {indent}.\n\
         Screen:\n{}",
        harness.screen_to_string(),
    );
}
