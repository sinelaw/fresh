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
    // `trim_end_matches('▌')` drops the scrollbar's unsaved-change mark: the
    // buffer is dirty at this point, so the track carries one in the last
    // column of the same row.
    let content = line1
        .rsplit('│')
        .next()
        .unwrap_or("")
        .trim_end_matches('▌')
        .trim();
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
    // `trim_end_matches('▌')` drops the scrollbar's unsaved-change mark: the
    // buffer is dirty at this point, so the track carries one in the last
    // column of the same row.
    let content = line1
        .rsplit('│')
        .next()
        .unwrap_or("")
        .trim_end_matches('▌')
        .trim();
    assert_eq!(
        content, "iX",
        "autoStart=false: vi mode should stay off, both `i` and `X` \
         get inserted as text. Got buffer content {content:?}. \
         Screen:\n{screen}"
    );
}

/// vi_mode (autoStart) and the Orchestrator together, the dock mounted from
/// `ready` and `two_lines.txt` open in the editor.
fn vi_with_orchestrator_dock() -> (EditorTestHarness, tempfile::TempDir) {
    use crate::common::harness::HarnessOptions;

    init_tracing_from_env();
    let temp = tempfile::TempDir::new().unwrap();
    let project_root = temp.path().join("project_root");
    fs::create_dir_all(&project_root).unwrap();
    let plugins_dir = project_root.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "vi_mode");
    copy_plugin(&plugins_dir, "orchestrator");
    copy_plugin_lib(&plugins_dir);
    let file = project_root.join("two_lines.txt");
    fs::write(&file, "alpha\nbeta\n").unwrap();
    // A git project, so the dock lists this workspace as a session row.
    let ok = std::process::Command::new("git")
        .args(["init", "-q"])
        .current_dir(&project_root)
        .status()
        .unwrap()
        .success();
    assert!(ok);

    let mut config = Config::default();
    config.plugins.insert(
        "vi_mode".to_string(),
        PluginConfig {
            enabled: true,
            path: None,
            settings: serde_json::json!({ "autoStart": true }),
        },
    );
    let mut h = EditorTestHarness::create(
        120,
        32,
        HarnessOptions::new()
            .with_config(config)
            .with_working_dir(project_root)
            .without_empty_plugins_dir()
            .with_startup_chrome(),
    )
    .unwrap();
    h.editor_mut().set_clipboard_for_test(String::new());

    // vi_mode has run its top-level body (and so `enableVi()`) once its
    // toggle command is registered; the dock mounts from `ready`, after.
    {
        use fresh::input::keybindings::Action::PluginAction;
        h.wait_until(|h| {
            let cmds = h.editor().command_registry().read().unwrap().get_all();
            cmds.iter()
                .any(|c| c.action == PluginAction("vi_mode_toggle".to_string()))
        })
        .unwrap();
    }
    h.editor_mut().fire_ready_hook();
    h.wait_until(|h| h.screen_to_string().contains("+ New"))
        .unwrap();

    h.open_file(&file).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("two_lines.txt"))
        .unwrap();
    (h, temp)
}

/// With the cursor on line 1 of `two_lines.txt` and the editor holding the
/// keyboard, `j` must be vi-normal's move-down, not a typed `j`.
fn assert_vi_j_moves_down(h: &mut EditorTestHarness) {
    h.send_key(KeyCode::Char('j'), KeyModifiers::NONE).unwrap();
    // Either outcome settles the question: the cursor moved, or a `j` was
    // typed into the buffer.
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("Ln 2, Col 1") || s.contains("jalpha")
    })
    .unwrap();
    let screen = h.screen_to_string();
    assert!(
        !screen.contains("jalpha") && screen.contains("Ln 2, Col 1"),
        "vi-normal `j` moves down instead of typing:\n{screen}"
    );
}

/// Give the dock the keyboard (Alt+O, vi-normal leaves it to the editor).
fn focus_dock(h: &mut EditorTestHarness) {
    h.send_key(KeyCode::Char('o'), KeyModifiers::ALT).unwrap();
    h.wait_until(|h| h.editor().is_dock_focused()).unwrap();
}

/// autoStart with the Orchestrator dock up: the dock's mount (from `ready`,
/// after vi_mode has enabled itself at load) used to reset the editor mode
/// to none, so the status bar said vi was on while `j` typed a `j`
/// (issue #3305).
#[test]
fn vi_mode_autostart_survives_the_orchestrator_dock_mount() {
    let (mut h, _tmp) = vi_with_orchestrator_dock();
    assert_vi_j_moves_down(&mut h);
}

/// An Orchestrator dialog opened and cancelled from the dock hands vi its
/// mode back: the dialog used to take the window's one mode slot for its
/// keymap and empty it on close, so after Alt+N, Esc, Esc a `j` typed a `j`.
#[test]
fn vi_mode_survives_an_orchestrator_dialog_opened_from_the_dock() {
    let (mut h, _tmp) = vi_with_orchestrator_dock();
    focus_dock(&mut h);
    h.send_key(KeyCode::Char('n'), KeyModifiers::ALT).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("New Workspace"))
        .unwrap();
    // Esc cancels the form and hands the keyboard back to the dock…
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| {
        !h.screen_to_string().contains("New Workspace") && h.editor().is_dock_focused()
    })
    .unwrap();
    // …and a second Esc leaves the dock for the editor.
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| !h.editor().is_dock_focused()).unwrap();
    assert_vi_j_moves_down(&mut h);
}

/// Enter on the dock's session row hands the keyboard to the editor and
/// must leave vi's mode in place (it used to empty the mode slot).
#[test]
fn vi_mode_survives_enter_on_a_dock_row() {
    let (mut h, _tmp) = vi_with_orchestrator_dock();
    focus_dock(&mut h);
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| !h.editor().is_dock_focused()).unwrap();
    assert_vi_j_moves_down(&mut h);
}

/// The dock's F2 context menu keeps its own keys with vi on: ↓ walks the
/// entries and Esc closes it. The menu has no mode of its own, so the host
/// resolved the keys it leaves against the window's mode — vi-normal's —
/// and ↓ / Esc went to vi instead.
#[test]
fn dock_context_menu_navigates_with_vi_mode_on() {
    let (mut h, _tmp) = vi_with_orchestrator_dock();
    focus_dock(&mut h);

    // Esc closes the menu.
    h.send_key(KeyCode::F(2), KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Move to Folder"))
        .unwrap();
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| !h.screen_to_string().contains("Move to Folder"))
        .unwrap();

    // ↓↓ walks Visit… → Rename… → Move to Folder…, and Enter runs that one:
    // the "move to" dropdown replaces the menu. Had ↓ gone to vi, Enter
    // would have run Visit… instead.
    h.wait_until(|h| h.editor().is_dock_focused()).unwrap();
    h.send_key(KeyCode::F(2), KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Move to Folder"))
        .unwrap();
    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Top level"))
        .unwrap();
}
