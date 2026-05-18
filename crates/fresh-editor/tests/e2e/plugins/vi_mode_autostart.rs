//! E2E: the vi_mode plugin's `autoStart` config field.
//!
//! When `plugins.vi_mode.settings.autoStart = true` lands in the
//! resolved config BEFORE the plugin runs, the plugin's
//! `editor.defineConfigBoolean("autoStart", { default: false })` call
//! sees the user-set `true` (via the snapshot) and enables vi mode
//! immediately.
//!
//! The observable: with vi-normal active, typing `i` enters insert
//! mode without inserting the letter `i`. So `iX<Esc>` on an empty
//! buffer leaves just `X`. Without vi, `iX<Esc>` leaves `iX`. We
//! distinguish the two by scanning the rendered buffer for the
//! literal `iX` substring.

use crate::common::fixtures::TestFixture;
use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::{Config, PluginConfig};
use std::fs;

fn build_harness(auto_start: bool) -> (EditorTestHarness, tempfile::TempDir) {
    init_tracing_from_env();
    let temp = tempfile::TempDir::new().unwrap();
    let project_root = temp.path().join("project_root");
    fs::create_dir_all(&project_root).unwrap();
    let plugins_dir = project_root.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "vi_mode");
    copy_plugin_lib(&plugins_dir);

    let mut config = Config::default();
    // Preset the plugin's config slot so `editor.defineConfigBoolean`
    // sees `autoStart=<auto_start>` via the state snapshot the first
    // time the plugin runs.
    config.plugins.insert(
        "vi_mode".to_string(),
        PluginConfig {
            enabled: true,
            path: None,
            settings: serde_json::json!({ "autoStart": auto_start }),
        },
    );

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, config, project_root).unwrap();
    harness.editor_mut().set_clipboard_for_test(String::new());
    (harness, temp)
}

/// Drive `iX<Esc>` on an open empty file and return the rendered
/// screen. Includes a `wait_until` for the vi_mode plugin command to
/// be registered, so the plugin's top-level body (including the
/// `if (autoStart) enableVi()` line) is guaranteed to have run.
fn rendered_after_ix_esc(h: &mut EditorTestHarness) -> String {
    use fresh::input::keybindings::Action::PluginAction;
    h.wait_until(|h| {
        let cmds = h.editor().command_registry().read().unwrap().get_all();
        cmds.iter()
            .any(|c| c.action == PluginAction("vi_mode_toggle".to_string()))
    })
    .unwrap();
    h.send_key(KeyCode::Char('i'), KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Char('X'), KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    h.screen_to_string()
}

/// autoStart=true → vi mode is on at first render; `iX<Esc>` leaves
/// just `X` in the buffer (the `i` was the vi insert command).
#[test]
fn vi_mode_autostart_true_enables_vi_immediately() {
    let (mut harness, _tmp) = build_harness(true);

    let fixture = TestFixture::new("scratch.txt", "").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    let screen = rendered_after_ix_esc(&mut harness);
    // The buffer area renders content after ` N │ `. Pull the first
    // line's content.
    // The buffer row renders as `... N │ <content>`. There may be a
    // border `│` from the chrome on the same line, so take everything
    // after the LAST `│` rather than the first.
    let line1 = screen
        .lines()
        .find(|l| l.contains("1 │"))
        .expect("expected line 1 in render");
    let content = line1.rsplit('│').next().unwrap_or("").trim();
    assert_eq!(
        content, "X",
        "autoStart=true: vi-normal `i` should swallow the keystroke, \
         leaving only the trailing `X`. Got buffer content {content:?}. \
         Screen:\n{screen}"
    );
}

/// autoStart=false (default) → vi mode stays dormant; the same
/// keystrokes type both letters: `iX<Esc>` leaves `iX` in the buffer.
/// Same plugin, same harness setup, just the opposite flag — proves
/// the autoStart field is wired and isn't a no-op.
#[test]
fn vi_mode_autostart_false_leaves_vi_dormant() {
    let (mut harness, _tmp) = build_harness(false);

    let fixture = TestFixture::new("scratch.txt", "").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    let screen = rendered_after_ix_esc(&mut harness);
    // The buffer row renders as `... N │ <content>`. There may be a
    // border `│` from the chrome on the same line, so take everything
    // after the LAST `│` rather than the first.
    let line1 = screen
        .lines()
        .find(|l| l.contains("1 │"))
        .expect("expected line 1 in render");
    let content = line1.rsplit('│').next().unwrap_or("").trim();
    assert_eq!(
        content, "iX",
        "autoStart=false: vi mode should stay off, both `i` and `X` \
         get inserted as text. Got buffer content {content:?}. \
         Screen:\n{screen}"
    );
}
