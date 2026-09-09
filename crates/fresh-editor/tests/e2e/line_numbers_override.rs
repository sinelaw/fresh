//! E2E: how the three opinions about the line-number gutter compose.
//!
//! There are three of them, and they have to be kept apart:
//!
//!   1. the global `editor.line_numbers` setting,
//!   2. a *mode's default* — markdown compose hides the gutter while it has
//!      the buffer, and re-asserts that on every `buffer_activated`, including
//!      the one fired when you switch back to a tab you already had open,
//!   3. the *user's own* per-buffer pin — "Toggle Line Numbers (Current
//!      Buffer)", or vi's `:set number` / `:set nonumber`.
//!
//! Issue #2931 was (2) being written straight onto the rendered flag, so
//! returning to a compose tab wiped out a pin made while composing. The
//! numbers survived the switch-back frame and only vanished on the next
//! redraw, which is what made the report read as a mouse-move bug.
//!
//! The fix must not go too far the other way: (3) has to keep beating (2) *and*
//! keep working when it arrives through a plugin. `:set number` is a user
//! command that happens to be typed at a plugin's prompt, and it must turn the
//! gutter on whether or not the buffer is already pinned.
//!
//! Every test here asserts only on rendered output: whether the gutter
//! separator is drawn immediately ahead of the document's first line.

#![cfg(feature = "plugins")]

use crate::common::fixtures::TestFixture;
use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::{Config, PluginConfig};
use fresh::input::keybindings::Action::PluginAction;

/// First body line of `one.md`, distinctive enough to locate unambiguously.
const ONE_BODY: &str = "Alpha document body.";
/// First body line of `two.md`.
const TWO_BODY: &str = "Bravo document body.";
/// Sole body line of the plain-text buffer the vi tests drive.
const PLAIN_BODY: &str = "Charlie plain body.";

/// Both documents carry an emphasis span on their last line — away from the
/// cursor, which sits on line 1 — so that "the compose plugin has decorated
/// this buffer" is observable as the `**` markers being concealed.
const ONE_MD: &str = "Alpha document body.\n\nSecond alpha paragraph with **strong** text.\n";
const TWO_MD: &str = "Bravo document body.\n\nSecond bravo paragraph with **strong** text.\n";

/// The command that records the user's explicit per-buffer pin.
const TOGGLE_PIN: &str = "Toggle Line Numbers (Current Buffer)";
/// The command that flips the global `editor.line_numbers` preference.
///
/// Typing a command's full name selects it outright, so this does not land on
/// the longer [`TOGGLE_PIN`] that shares its prefix.
const TOGGLE_GLOBAL: &str = "Toggle Line Numbers";

/// The gutter separator immediately before a line of text. Its presence on the
/// row holding `body` is the only on-screen evidence of line numbers.
fn gutter_before(body: &str) -> String {
    format!("│ {body}")
}

/// A config with the given global `editor.line_numbers` and markdown opening
/// in Page View, which is what puts the compose plugin in charge of the gutter.
fn page_view_config(line_numbers: bool) -> Config {
    let mut config = Config::default();
    config.editor.line_numbers = line_numbers;
    let markdown = config
        .languages
        .get_mut("markdown")
        .expect("default config defines the markdown language");
    markdown.page_view = Some(true);
    markdown.page_width = Some(60);
    config
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

/// Build a project holding the real `vi_mode` plugin and open a one-line plain
/// text file in it.
///
/// Plain text keeps every other mode's opinion out of these tests: the only
/// things touching the gutter are the global setting, the pin, and `:set`.
fn vi_harness(mut config: Config) -> (EditorTestHarness, tempfile::TempDir, TestFixture) {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project");
    std::fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    std::fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "vi_mode");
    copy_plugin_lib(&plugins_dir);

    config.plugins.insert(
        "vi_mode".to_string(),
        PluginConfig {
            enabled: true,
            path: None,
            settings: serde_json::json!({}),
        },
    );

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(100, 24, config, project_root).unwrap();
    // Internal-only clipboard, so vi's yank/put registers cannot reach the host.
    harness.editor_mut().set_clipboard_for_test(String::new());

    let fixture = TestFixture::new("plain.txt", &format!("{PLAIN_BODY}\n")).unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    (harness, temp_dir, fixture)
}

/// Turn vi mode on and wait until the editor is in vi-normal.
fn enable_vi_mode(harness: &mut EditorTestHarness) {
    // `run_palette_command` re-filters until the row shows up, so it already
    // tolerates a late registration; waiting on the action first just means a
    // plugin that never loads fails as "vi_mode_toggle never registered"
    // rather than as an unexplained palette timeout.
    harness
        .wait_until(|h| {
            let commands = h.editor().command_registry().read().unwrap().get_all();
            commands
                .iter()
                .any(|c| c.action == PluginAction("vi_mode_toggle".to_string()))
        })
        .unwrap();
    harness.run_palette_command("Toggle Vi mode").unwrap();
    harness.wait_for_prompt_closed().unwrap();
    harness
        .wait_until(|h| h.editor().editor_mode() == Some("vi-normal".to_string()))
        .unwrap();
}

/// Run a vi ex command (`:` + `cmd` + Enter) from vi-normal mode.
fn run_ex_command(harness: &mut EditorTestHarness, cmd: &str) {
    harness
        .send_key(KeyCode::Char(':'), KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text(cmd).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();
}

/// Run the per-buffer pin command and wait for the palette to close.
fn pin_line_numbers(harness: &mut EditorTestHarness) {
    harness.run_palette_command(TOGGLE_PIN).unwrap();
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

    let (mut harness, _temp) = compose_harness(page_view_config(true));

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

    pin_line_numbers(&mut harness);
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

/// Isolation control, not regression coverage: the same flow in Source View,
/// where no plugin touches the buffer's line numbers.
///
/// It deliberately reaches none of the code the fix changed — the compose
/// plugin returns early on a buffer that is not in page view, so
/// `handle_set_line_numbers` is never called. Its only job is to show that a
/// failure of the page-view test above is about the plugin path rather than
/// about per-buffer pins in general. The pin-versus-plugin coverage lives in
/// the `:set number` tests below.
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

    pin_line_numbers(&mut harness);
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

/// `:set number` is a user command, not a mode's default, and must win over an
/// existing per-buffer pin.
///
/// This is the other half of #2931: the pin the reporter makes with Ctrl+L is
/// exactly the state in which vi's `:set number` becomes a silent no-op that
/// still reports "line numbers on" if `setLineNumbers` is treated as a
/// default. The pin is persisted per file, so such a buffer stays stuck across
/// restarts with no way back from inside vi.
#[test]
fn test_vi_set_number_beats_a_per_buffer_pin() {
    init_tracing_from_env();

    let mut config = Config::default();
    config.editor.line_numbers = true;

    let (mut harness, _temp, _fixture) = vi_harness(config);

    // The gutter is on by default; pin it off for this buffer.
    harness
        .wait_for_screen_contains(&gutter_before(PLAIN_BODY))
        .unwrap();
    pin_line_numbers(&mut harness);
    harness
        .wait_until_stable(|h| !h.screen_to_string().contains(&gutter_before(PLAIN_BODY)))
        .unwrap();

    enable_vi_mode(&mut harness);
    run_ex_command(&mut harness, "set number");

    harness
        .wait_for_screen_contains(&gutter_before(PLAIN_BODY))
        .unwrap();
    harness.assert_no_plugin_errors();
}

/// The same in the other direction: `:set nonumber` must clear a pin that is
/// holding the gutter *on*.
#[test]
fn test_vi_set_nonumber_beats_a_per_buffer_pin() {
    init_tracing_from_env();

    let mut config = Config::default();
    config.editor.line_numbers = false;

    let (mut harness, _temp, _fixture) = vi_harness(config);

    // The gutter is off globally; pin it on for this buffer.
    harness.wait_for_screen_contains(PLAIN_BODY).unwrap();
    harness.assert_screen_not_contains(&gutter_before(PLAIN_BODY));
    pin_line_numbers(&mut harness);
    harness
        .wait_for_screen_contains(&gutter_before(PLAIN_BODY))
        .unwrap();

    enable_vi_mode(&mut harness);
    run_ex_command(&mut harness, "set nonumber");

    harness
        .wait_until_stable(|h| !h.screen_to_string().contains(&gutter_before(PLAIN_BODY)))
        .unwrap();
    harness.assert_screen_contains(PLAIN_BODY);
    harness.assert_no_plugin_errors();
}

/// The *global* "Toggle Line Numbers" must not make the gutter appear and then
/// silently vanish on the next return to the tab.
///
/// The handler used to write `show_line_numbers` directly, without consulting
/// [`BufferViewState::line_numbers_visible`]. Compose re-asserts its own default
/// from `buffer_activated`, which fires every time focus comes back to a tab
/// that is already open, so the very next resolve undid the toggle: the numbers
/// appeared, survived until the next switch-back, and then went away with
/// nothing on screen to explain it.
///
/// The invariant asserted is that the gutter looks the *same* right after the
/// toggle as it does after leaving the tab and returning — a stronger and more
/// honest statement than "it never appears", and one that fails on the old
/// behaviour in exactly the way the report describes.
#[test]
fn test_global_toggle_line_numbers_survives_a_tab_switch_while_composing() {
    init_tracing_from_env();

    let (mut harness, _temp) = compose_harness(page_view_config(true));

    // Compose has taken over two.md: emphasis concealed, gutter hidden even
    // though `editor.line_numbers` is true.
    harness
        .wait_until_stable(|h| !h.screen_to_string().contains("**"))
        .unwrap();
    harness.assert_screen_not_contains(&gutter_before(TWO_BODY));

    harness.run_palette_command(TOGGLE_GLOBAL).unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // Settle the frame the same way the post-round-trip frame is settled
    // below, so the two observations are taken under identical conditions.
    type_at_end_of_document(&mut harness);
    harness
        .wait_until_stable(|h| !h.screen_to_string().contains("**"))
        .unwrap();
    let gutter_after_toggle = harness
        .screen_to_string()
        .contains(&gutter_before(TWO_BODY));

    // Leave the tab and come back. Returning fires `buffer_activated`, where
    // compose re-asserts its default.
    switch_tab(&mut harness, KeyCode::PageUp, ONE_BODY);
    switch_tab(&mut harness, KeyCode::PageDown, TWO_BODY);

    // Drive a redraw and wait for the plugin to conceal the emphasis just
    // typed, so the frame observed is the settled one rather than the stale
    // pre-switch frame.
    type_at_end_of_document(&mut harness);
    harness
        .wait_until_stable(|h| !h.screen_to_string().contains("**"))
        .unwrap();
    let gutter_after_round_trip = harness
        .screen_to_string()
        .contains(&gutter_before(TWO_BODY));

    assert_eq!(
        gutter_after_toggle,
        gutter_after_round_trip,
        "the global toggle's effect on the gutter changed by itself across a \
         tab switch: visible={gutter_after_toggle} right after the toggle, \
         visible={gutter_after_round_trip} after leaving the tab and coming \
         back\n{}",
        harness.screen_to_string()
    );
    harness.assert_no_plugin_errors();
}

/// The global toggle must derive its new value from the global setting, not
/// from whatever the current view happens to be rendering.
///
/// In a compose buffer the rendered flag is `false` while `editor.line_numbers`
/// is `true`, so `!rendered` "flipped" the preference from true to true — and
/// since the handler persists to the user's config layer, that wrote back a
/// value the user never chose and left them unable to turn line numbers off at
/// all from a compose buffer.
///
/// The evidence is on screen rather than in the config struct: a buffer opened
/// *after* the toggle takes its gutter straight from `editor.line_numbers`, so
/// it renders whatever the toggle actually wrote there.
#[test]
fn test_global_toggle_line_numbers_flips_the_global_setting_from_a_compose_buffer() {
    init_tracing_from_env();

    let (mut harness, temp) = compose_harness(page_view_config(true));

    // The starting state that made the old arithmetic wrong: global setting
    // on, gutter off because compose says so.
    harness
        .wait_until_stable(|h| !h.screen_to_string().contains("**"))
        .unwrap();
    harness.assert_screen_not_contains(&gutter_before(TWO_BODY));

    // The setting is on, so one press of the global toggle means "off".
    harness.run_palette_command(TOGGLE_GLOBAL).unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // A plain-text buffer opened now has no mode with an opinion, so its
    // gutter shows the global setting as the toggle left it.
    let plain = temp.path().join("project").join("plain.txt");
    std::fs::write(&plain, format!("{PLAIN_BODY}\n")).unwrap();
    harness.open_file(&plain).unwrap();
    harness
        .wait_until_stable(|h| h.screen_to_string().contains(PLAIN_BODY))
        .unwrap();

    harness.assert_screen_contains(PLAIN_BODY);
    harness.assert_screen_not_contains(&gutter_before(PLAIN_BODY));
    harness.assert_no_plugin_errors();
}
