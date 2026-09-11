//! E2E coverage for the orchestrator dock's user-facing settings
//! (`plugins.orchestrator.settings.*`, rendered by the Settings UI under
//! "Plugin: orchestrator"):
//!
//! * `autoOpenDock` — open the dock on the `ready` hook, unfocused;
//! * `defaultView` — the density (`card` / `compact`) the dock opens at;
//! * `showAllWorktrees` / `showEmptyWorkspaces` — the initial state of the
//!   two Filters checkboxes.
//!
//! Each is only a *default*: the dock's own controls still win once the
//! user touches them. These tests pin the "where does it start" half,
//! which is what the settings buy; `orchestrator_dock.rs` already covers
//! the toggles themselves.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
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

/// Open the dock header's `⋯` menu, which holds the density rows and the
/// two show switches (the applied ones wear a `●`).
fn open_dock_menu(h: &mut EditorTestHarness) {
    let (mcol, mrow) = h
        .find_text_on_screen("⋯")
        .unwrap_or_else(|| panic!("screen missing '⋯':\n{}", h.screen_to_string()));
    h.mouse_click(mcol, mrow).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Manage workspaces"))
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
    h.wait_until(|h| h.screen_to_string().contains("● compact view"))
        .unwrap();
    h.assert_screen_not_contains("● card view");
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
    h.wait_until(|h| h.screen_to_string().contains("● compact view"))
        .unwrap();
    h.assert_screen_not_contains("● card view");
}

#[test]
fn default_view_setting_card_opens_dock_card() {
    let (_tmp, root, config) = setup(serde_json::json!({ "defaultView": "card" }));
    let mut h = EditorTestHarness::with_config_and_working_dir(120, 32, config, root).unwrap();
    h.render().unwrap();
    open_dock(&mut h);
    open_dock_menu(&mut h);
    h.wait_until(|h| h.screen_to_string().contains("● card view"))
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
        s.contains("● all worktrees") && s.contains("show empty") && !s.contains("● show empty")
    })
    .unwrap();
}

/// `autoOpenDock: true` brings the dock up on the `ready` hook, and
/// leaves the keyboard with the editor — it's a switcher, not something
/// to type into.
#[test]
fn auto_open_setting_shows_dock_unfocused_at_startup() {
    let (_tmp, root, config) = setup(serde_json::json!({ "autoOpenDock": true }));
    let mut h = EditorTestHarness::with_config_and_working_dir(120, 32, config, root).unwrap();
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
    let mut h = EditorTestHarness::with_config_and_working_dir(120, 32, config, root).unwrap();
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
    let mut h = EditorTestHarness::with_config_and_working_dir(120, 32, config, root).unwrap();
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
