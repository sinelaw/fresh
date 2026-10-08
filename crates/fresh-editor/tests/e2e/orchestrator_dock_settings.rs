//! E2E coverage for the orchestrator dock's user-facing settings
//! (`plugins.orchestrator.settings.*`, rendered by the Settings UI under
//! "orchestrator" under "Plugins"):
//!
//! * `autoOpenDock` — whether the dock opens on the `ready` hook,
//!   unfocused: `auto`, `always` or `never`, plus the booleans it replaced;
//! * `defaultView` — the density (`card` / `compact`) the dock opens at;
//! * `showAllWorktrees` / `showEmptyWorkspaces` — the initial state of the
//!   two Filters checkboxes.
//!
//! Each is only a *default*: the dock's own controls still win once the
//! user touches them. These tests pin the "where does it start" half,
//! which is what the settings buy; `orchestrator_dock.rs` already covers
//! the toggles themselves.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness, HarnessOptions};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::{Config, PluginConfig};
use std::fs;
use std::path::PathBuf;

/// A git project with the orchestrator plugin (+ shared lib) installed,
/// and `settings` preset in the plugin's config slot so the plugin's
/// `defineConfigX` calls see the user values the first time it runs.
fn setup(settings: serde_json::Value) -> (tempfile::TempDir, PathBuf, Config) {
    init_tracing_from_env();
    let temp_dir = tempfile::TempDir::new().unwrap();
    let root = temp_dir.path().join("alphaproj");
    fs::create_dir(&root).unwrap();
    let plugins_dir = root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "orchestrator");
    fs::write(root.join("readme.txt"), "hello\n").unwrap();
    let ok = std::process::Command::new("git")
        .args(["init", "-q"])
        .current_dir(&root)
        .status()
        .unwrap()
        .success();
    assert!(ok);

    let mut config = Config::default();
    config.plugins.insert(
        "orchestrator".to_string(),
        PluginConfig {
            enabled: true,
            path: None,
            settings,
        },
    );
    (temp_dir, root, config)
}

/// A harness with the host's startup chrome kept, so the `ready` the test
/// fires opens the dock the way `main` would.
fn launch(config: Config, root: PathBuf) -> EditorTestHarness {
    EditorTestHarness::create(
        120,
        32,
        HarnessOptions::new()
            .with_config(config)
            .with_working_dir(root)
            .without_empty_plugins_dir()
            .with_startup_chrome(),
    )
    .unwrap()
}

/// The same, as a bare `fresh` (Orchestrator mode).
fn launch_orchestrator_mode(config: Config, root: PathBuf) -> EditorTestHarness {
    EditorTestHarness::create(
        120,
        32,
        HarnessOptions::new()
            .with_config(config)
            .with_working_dir(root)
            .without_empty_plugins_dir()
            .with_startup_chrome()
            .with_orchestrator_mode(),
    )
    .unwrap()
}

/// Toggle the dock open via the command palette and wait for it to render
/// *and* take keyboard focus (mirrors `orchestrator_dock::open_dock`).
fn open_dock(h: &mut EditorTestHarness) {
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text("Orchestrator: Toggle Dock").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Toggle Dock"))
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("+ New") && h.editor().is_dock_focused())
        .unwrap();
}

/// Open the dock header's Menu, which holds the density rows and the
/// two show switches (the applied ones wear a `●`).
fn open_dock_menu(h: &mut EditorTestHarness) {
    let (mcol, mrow) = h
        .find_text_on_screen("Menu ▾")
        .unwrap_or_else(|| panic!("screen missing 'Menu ▾':\n{}", h.screen_to_string()));
    h.mouse_click(mcol, mrow).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Machines…"))
        .unwrap();
}

/// `defaultView: "compact"` opens the dock in list density — without it
/// the dock always started at "card" and the user had to click "view"
/// on every launch.
#[test]
fn default_view_setting_opens_dock_compact() {
    let (_tmp, root, config) = setup(serde_json::json!({ "defaultView": "compact" }));
    let mut h = EditorTestHarness::with_config_and_working_dir(120, 32, config, root).unwrap();
    h.render().unwrap();
    open_dock(&mut h);
    open_dock_menu(&mut h);
    h.wait_until(|h| h.screen_to_string().contains("(•) Compact"))
        .unwrap();
    h.assert_screen_not_contains("(•) Cards");
}

/// No setting ⇒ compact density. The dock is a switcher first, and one line
/// per workspace fits several times as many rows in the same column; the
/// card's extra lines are detail you go looking for. An explicit
/// `defaultView: "card"` still gets cards.
#[test]
fn default_view_setting_absent_opens_dock_compact() {
    let (_tmp, root, config) = setup(serde_json::json!({}));
    let mut h = EditorTestHarness::with_config_and_working_dir(120, 32, config, root).unwrap();
    h.render().unwrap();
    open_dock(&mut h);
    open_dock_menu(&mut h);
    h.wait_until(|h| h.screen_to_string().contains("(•) Compact"))
        .unwrap();
    h.assert_screen_not_contains("(•) Cards");
}

#[test]
fn default_view_setting_card_opens_dock_card() {
    let (_tmp, root, config) = setup(serde_json::json!({ "defaultView": "card" }));
    let mut h = EditorTestHarness::with_config_and_working_dir(120, 32, config, root).unwrap();
    h.render().unwrap();
    open_dock(&mut h);
    open_dock_menu(&mut h);
    h.wait_until(|h| h.screen_to_string().contains("(•) Cards"))
        .unwrap();
}

/// The two show switches start where the settings say: "all worktrees"
/// on, "show empty" off — the inverse of both shipped defaults, so a
/// stuck default would fail this.
#[test]
fn filter_checkbox_settings_seed_the_dock() {
    let (_tmp, root, config) = setup(serde_json::json!({
        "showAllWorktrees": true,
        "showEmptyWorkspaces": false,
    }));
    let mut h = EditorTestHarness::with_config_and_working_dir(120, 32, config, root).unwrap();
    h.render().unwrap();
    open_dock(&mut h);
    open_dock_menu(&mut h);
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("[✓] All worktrees") && s.contains("[ ] Empty workspaces")
    })
    .unwrap();
}

/// `autoOpenDock: true` brings the dock up on the `ready` hook, and
/// leaves the keyboard with the editor — it's a switcher, not something
/// to type into.
#[test]
fn auto_open_setting_shows_dock_unfocused_at_startup() {
    let (_tmp, root, config) = setup(serde_json::json!({ "autoOpenDock": true }));
    let mut h = launch(config, root);
    h.render().unwrap();
    h.editor_mut().fire_ready_hook();
    h.wait_until(|h| h.screen_to_string().contains("+ New"))
        .unwrap();
    assert!(
        !h.editor().is_dock_focused(),
        "auto-opened dock must not steal keyboard focus"
    );
}

/// Auto-open is the default: the ready hook alone brings the dock up,
/// unfocused — a switcher nobody knows to open is not one.
#[test]
fn auto_open_defaults_on() {
    let (_tmp, root, config) = setup(serde_json::json!({}));
    let mut h = launch(config, root);
    h.render().unwrap();
    h.editor_mut().fire_ready_hook();
    h.wait_until(|h| h.screen_to_string().contains("+ New"))
        .unwrap();
    assert!(
        !h.editor().is_dock_focused(),
        "the auto-opened dock must not steal keyboard focus"
    );
}

/// `autoOpenDock: false` keeps the dock closed until it is toggled.
#[test]
fn auto_open_can_be_switched_off() {
    let (_tmp, root, config) = setup(serde_json::json!({ "autoOpenDock": false }));
    let mut h = launch(config, root);
    h.render().unwrap();
    h.editor_mut().fire_ready_hook();
    // Let the ready hook round-trip through the plugin thread with a
    // command that does not touch the dock — the Machines dialog — and
    // only then look: a dock that wrongly auto-opened is on screen now,
    // and the assertion fails instead of the toggle below closing it and
    // the wait after it hanging.
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text("Orchestrator: Machines").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Orchestrator: Machines"))
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    // The dialog's own button, not the palette row that also says
    // "Machines".
    h.wait_until(|h| h.screen_to_string().contains("Add machine"))
        .unwrap();
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| !h.screen_to_string().contains("Add machine"))
        .unwrap();
    h.assert_screen_not_contains("+ New");
    // Then the dock the normal way, and closed again.
    open_dock(&mut h);
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text("Orchestrator: Toggle Dock").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Toggle Dock"))
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    // A dock auto-opened at ready would have stayed mounted behind the
    // toggle; with auto-open off there is nothing left on screen.
    h.wait_until(|h| !h.screen_to_string().contains("+ New"))
        .unwrap();
}

/// #3442: a bare `fresh` used to open the dock whatever the setting said.
/// Driven with both spellings — the `never` mode, and the legacy `false` an
/// upgrading user still has on disk.
fn dock_stays_closed_in_orchestrator_mode(setting: serde_json::Value) {
    let (_tmp, root, config) = setup(serde_json::json!({ "autoOpenDock": setting }));
    let mut h = launch_orchestrator_mode(config, root);
    h.render().unwrap();
    h.editor_mut().fire_ready_hook();
    // Round-trip the hook through the plugin thread with a command that does
    // not touch the dock, so a wrongly auto-opened dock is on screen before
    // we assert it is absent.
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text("Orchestrator: Machines").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Orchestrator: Machines"))
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Add machine"))
        .unwrap();
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| !h.screen_to_string().contains("Add machine"))
        .unwrap();
    h.assert_screen_not_contains("+ New");
}

#[test]
fn never_keeps_the_dock_closed_in_orchestrator_mode() {
    dock_stays_closed_in_orchestrator_mode(serde_json::json!("never"));
}

#[test]
fn a_legacy_false_keeps_the_dock_closed_in_orchestrator_mode() {
    dock_stays_closed_in_orchestrator_mode(serde_json::json!(false));
}

/// Walk the Settings-UI category list until `name` is the selected row
/// (mirrors `plugins/config_changed_adoption.rs`).
fn focus_category(h: &mut EditorTestHarness, name: &str) {
    for _ in 0..40 {
        if h.screen_to_string()
            .lines()
            .any(|line| line.contains('>') && line.contains(name))
        {
            return;
        }
        h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        h.render().unwrap();
    }
    panic!(
        "category {name:?} never became selected. Screen:\n{}",
        h.screen_to_string()
    );
}

/// Move `AutoOpenDock` (the orchestrator page's first field; options `auto`,
/// `always`, `never` in that order) by `steps` through the Settings UI by
/// keyboard, save and close. Only the Settings UI's own save fires the
/// `config_changed` the plugin reacts to.
fn move_auto_open_dock_in_settings(h: &mut EditorTestHarness, steps: i32) {
    h.open_settings().unwrap();
    focus_category(h, "orchestrator");
    h.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    // Enter opens the list, arrows move the selection, Enter keeps it.
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    let key = if steps > 0 { KeyCode::Down } else { KeyCode::Up };
    for _ in 0..steps.unsigned_abs() {
        h.send_key(key, KeyModifiers::NONE).unwrap();
        h.render().unwrap();
    }
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    h.send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Settings saved"))
        .unwrap_or_else(|e| panic!("settings never saved: {e}\n{}", h.screen_to_string()));
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| !h.screen_to_string().contains("Settings ["))
        .unwrap();
}

/// A Settings edit to `never` / `always` is an instruction like the toggle,
/// so it closes or opens the dock now, not only at the next start.
#[test]
fn a_settings_edit_opens_and_closes_the_dock() {
    let (_tmp, root, config) = setup(serde_json::json!({ "autoOpenDock": "always" }));
    let mut h = launch_orchestrator_mode(config, root);
    h.render().unwrap();
    h.editor_mut().fire_ready_hook();
    h.wait_until(|h| h.screen_to_string().contains("+ New"))
        .unwrap();

    // always → never
    move_auto_open_dock_in_settings(&mut h, 1);
    h.wait_until(|h| !h.screen_to_string().contains("+ New"))
        .unwrap_or_else(|e| {
            panic!(
                "choosing `never` must close the dock: {e}\n{}",
                h.screen_to_string()
            )
        });

    // never → always
    move_auto_open_dock_in_settings(&mut h, -1);
    h.wait_until(|h| h.screen_to_string().contains("+ New"))
        .unwrap_or_else(|e| {
            panic!(
                "choosing `always` must reopen the dock: {e}\n{}",
                h.screen_to_string()
            )
        });
}
