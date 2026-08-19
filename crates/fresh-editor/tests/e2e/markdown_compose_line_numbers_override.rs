//! E2E: Page View must not discard an explicit per-buffer line-number pin
//! (issue #2931).
//!
//! Page View hides the line-number gutter, and the markdown compose plugin
//! re-asserts that on every `buffer_activated` — including the one fired when
//! you switch back to a tab you already had open. That re-assertion used to be
//! written straight onto the rendered flag, so it wiped out an explicit "Toggle
//! Line Numbers (Current Buffer)" the user had made while composing. The
//! numbers survived the switch-back frame and only vanished on the next redraw,
//! which is what made the report read as a mouse-move bug.
//!
//! The pair below is deliberate: the Page View test is the reproducer (it fails
//! before the fix), and the Source View test is the control — the same flow on
//! a buffer the plugin leaves alone, which passed before the fix and must keep
//! passing after it.
//!
//! Both assert only on rendered output: whether the gutter separator is drawn
//! immediately ahead of the document's first line.

#![cfg(feature = "plugins")]

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;

/// First body line of `one.md`, distinctive enough to locate unambiguously.
const ONE_BODY: &str = "Alpha document body.";
/// First body line of `two.md`.
const TWO_BODY: &str = "Bravo document body.";

/// Both documents carry an emphasis span on their last line — away from the
/// cursor, which sits on line 1 — so that "the compose plugin has decorated
/// this buffer" is observable as the `**` markers being concealed.
const ONE_MD: &str = "Alpha document body.\n\nSecond alpha paragraph with **strong** text.\n";
const TWO_MD: &str = "Bravo document body.\n\nSecond bravo paragraph with **strong** text.\n";

/// The gutter separator immediately before a line of text. Its presence on the
/// row holding `body` is the only on-screen evidence of line numbers.
fn gutter_before(body: &str) -> String {
    format!("│ {body}")
}

/// Build a project holding the real `markdown_compose` plugin plus `one.md`
/// and `two.md`, open both (leaving `two.md` focused), and return the harness.
fn compose_harness(config: Config) -> (EditorTestHarness, tempfile::TempDir) {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project");
    std::fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    std::fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "markdown_compose");
    copy_plugin_lib(&plugins_dir);

    let one = project_root.join("one.md");
    let two = project_root.join("two.md");
    std::fs::write(&one, ONE_MD).unwrap();
    std::fs::write(&two, TWO_MD).unwrap();

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(100, 24, config, project_root).unwrap();
    harness.open_file(&one).unwrap();
    harness.open_file(&two).unwrap();
    harness.render().unwrap();

    (harness, temp_dir)
}

/// Run a command-palette entry by fuzzy-typing its full name and pressing Enter.
fn run_command(harness: &mut EditorTestHarness, name: &str) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text(name).unwrap();
    harness.wait_for_screen_contains(name).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();
}

/// Switch to the neighbouring tab and wait for its text to be on screen and
/// the frame to hold still. Stability matters: a tab switch is animated as a
/// horizontal slide, so the incoming document's text is briefly on screen at
/// an offset with its gutter still scrolled out of view.
fn switch_tab(harness: &mut EditorTestHarness, code: KeyCode, expect_body: &str) {
    harness.send_key(code, KeyModifiers::CONTROL).unwrap();
    harness
        .wait_until_stable(|h| h.screen_to_string().contains(expect_body))
        .unwrap();
}

/// Append ` **x**` to the document's last line and put the cursor back on
/// line 1.
///
/// This is the "press any key" half of the report: the plugin's reaction to the
/// tab switch is only rendered on the next redraw-triggering event. It also
/// gives the tests a *positive* signal to wait on. Nothing here depends on the
/// setting under test.
fn type_at_end_of_document(harness: &mut EditorTestHarness) {
    harness
        .send_key_repeat(KeyCode::Down, KeyModifiers::NONE, 2)
        .unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.type_text(" **x**").unwrap();
    harness
        .send_key_repeat(KeyCode::Up, KeyModifiers::NONE, 2)
        .unwrap();
}

/// Reproducer for issue #2931.
///
/// `languages.markdown.page_view` opens both documents in Page View, where the
/// plugin hides line numbers. Pinning them back on for `one.md` and then
/// leaving and re-entering that tab must leave the pin intact.
#[test]
fn test_page_view_keeps_per_buffer_line_numbers_across_tab_switch() {
    init_tracing_from_env();

    let mut config = Config::default();
    config.editor.line_numbers = true;
    let markdown = config
        .languages
        .get_mut("markdown")
        .expect("default config defines the markdown language");
    markdown.page_view = Some(true);
    markdown.page_width = Some(60);

    let (mut harness, _temp) = compose_harness(config);

    // Compose has taken over two.md: its emphasis markers are concealed, and
    // the gutter is hidden even though `editor.line_numbers` is true.
    harness
        .wait_until_stable(|h| !h.screen_to_string().contains("**"))
        .unwrap();
    harness.assert_screen_not_contains(&gutter_before(TWO_BODY));

    // Same on one.md, which is where the pin goes.
    switch_tab(&mut harness, KeyCode::PageUp, ONE_BODY);
    harness
        .wait_until_stable(|h| !h.screen_to_string().contains("**"))
        .unwrap();
    harness.assert_screen_not_contains(&gutter_before(ONE_BODY));

    run_command(&mut harness, "Toggle Line Numbers (Current Buffer)");
    harness
        .wait_for_screen_contains(&gutter_before(ONE_BODY))
        .unwrap();

    // Leave the tab and come back. Returning fires `buffer_activated`, which
    // has the plugin re-apply compose — that must not undo the pin.
    switch_tab(&mut harness, KeyCode::PageDown, TWO_BODY);
    switch_tab(&mut harness, KeyCode::PageUp, ONE_BODY);

    // Drive a redraw and wait for the plugin to conceal the emphasis just
    // typed. That conceal cannot exist until the plugin has worked through
    // this activation's queue, so the frame asserted on below is the settled
    // one rather than the stale pre-switch frame.
    type_at_end_of_document(&mut harness);
    harness
        .wait_until_stable(|h| !h.screen_to_string().contains("**"))
        .unwrap();

    harness.assert_screen_contains(&gutter_before(ONE_BODY));
    harness.assert_no_plugin_errors();
}

/// Control: the same flow in Source View, where no plugin touches the buffer's
/// line numbers. This passed before the fix and must keep passing after it —
/// the fix must not make an ordinary per-buffer pin any less durable.
#[test]
fn test_source_view_keeps_per_buffer_line_numbers_across_tab_switch() {
    init_tracing_from_env();

    let mut config = Config::default();
    config.editor.line_numbers = true;

    let (mut harness, _temp) = compose_harness(config);

    // No Page View: both buffers show the gutter from the global default, and
    // the emphasis markers stay literal.
    harness.assert_screen_contains(&gutter_before(TWO_BODY));
    harness.assert_screen_contains("**strong**");

    // Go to one.md and pin its line numbers *off*.
    switch_tab(&mut harness, KeyCode::PageUp, ONE_BODY);
    harness.assert_screen_contains(&gutter_before(ONE_BODY));

    run_command(&mut harness, "Toggle Line Numbers (Current Buffer)");
    harness
        .wait_until_stable(|h| !h.screen_to_string().contains(&gutter_before(ONE_BODY)))
        .unwrap();

    // Leave the tab and come back; the pin survives.
    switch_tab(&mut harness, KeyCode::PageDown, TWO_BODY);
    harness.assert_screen_contains(&gutter_before(TWO_BODY));

    switch_tab(&mut harness, KeyCode::PageUp, ONE_BODY);
    type_at_end_of_document(&mut harness);
    harness.wait_for_screen_contains(" **x**").unwrap();

    harness.assert_screen_contains(ONE_BODY);
    harness.assert_screen_not_contains(&gutter_before(ONE_BODY));
    harness.assert_no_plugin_errors();
}
