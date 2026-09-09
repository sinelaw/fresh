//! E2E: shipped plugins adopt the `config_changed` hook, so their
//! settings take effect on save instead of at the next editor restart.
//!
//! Two shapes are covered, because "re-read the setting" is necessary
//! but not sufficient in either:
//!
//! * `vi_mode` bakes `arrowKeys` / `searchWordUnderCursor` into mode
//!   binding tables at `defineMode` time, so it has to re-emit the modes.
//! * `git_explorer` already re-reads `colorNames`, but only inside its
//!   decoration refresh — nothing re-ran that refresh on a settings save,
//!   so the explorer kept its old colors until the next file event.

use crate::common::git_test_helper::GitTestRepo;
use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::{Config, PluginConfig};
use std::fs;

/// Navigate the Settings UI category list until `name` is highlighted.
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
        "category {:?} never became selected. Screen:\n{}",
        name,
        h.screen_to_string()
    );
}

/// Open Settings, land on the plugin's category, move focus into the
/// panel, step down `steps` fields, toggle with Enter, save, and close.
///
/// `category` is matched as a substring, so pass a prefix short enough to
/// survive the category pane's truncation ("Plugin: git_ex", not
/// "Plugin: git_explorer" — that renders as `Plugin: git_ex...`).
fn toggle_plugin_setting(h: &mut EditorTestHarness, category: &str, steps: usize) {
    h.open_settings().unwrap();
    focus_category(h, category);
    h.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    for _ in 0..steps {
        h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        h.render().unwrap();
    }
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    h.send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Settings saved"))
        .unwrap();
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| !h.screen_to_string().contains("Settings ["))
        .unwrap();
}

/// Column reported by the status bar ("Ln 1, Col 7" → 7).
fn status_col(h: &EditorTestHarness) -> usize {
    let screen = h.screen_to_string();
    let marker = "Col ";
    let idx = screen
        .find(marker)
        .unwrap_or_else(|| panic!("status bar has no column readout. Screen:\n{screen}"));
    screen[idx + marker.len()..]
        .chars()
        .take_while(|c| c.is_ascii_digit())
        .collect::<String>()
        .parse()
        .unwrap_or_else(|_| panic!("unparsable column readout. Screen:\n{screen}"))
}

/// vi_mode: turning `arrowKeys` off must stop Left/Right driving the
/// cursor in vi-normal *without an editor restart*. The setting is baked
/// into the `vi-normal` binding table at load, so this only passes if the
/// plugin re-emits its modes from the `config_changed` handler.
#[test]
fn vi_mode_arrow_keys_setting_applies_without_restart() {
    init_tracing_from_env();
    let temp = tempfile::TempDir::new().unwrap();
    let project_root = temp.path().join("project_root");
    fs::create_dir_all(&project_root).unwrap();
    let plugins_dir = project_root.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "vi_mode");
    copy_plugin_lib(&plugins_dir);

    let mut config = Config::default();
    config.plugins.insert(
        "vi_mode".to_string(),
        PluginConfig {
            enabled: true,
            path: None,
            // vi on from the start, arrows bound — the state the user is
            // in when they go turn arrows off.
            settings: serde_json::json!({ "autoStart": true, "arrowKeys": true }),
        },
    );

    let file = project_root.join("scratch.txt");
    fs::write(&file, "hello world\n").unwrap();
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, config, project_root).unwrap();
    harness.open_file(&file).unwrap();
    harness.render().unwrap();

    // `autoStart` runs at the very end of the plugin body, after the mode
    // definitions and command registrations, so the enabled banner is a
    // sound barrier for "vi-normal is live and its bindings exist".
    harness
        .wait_until(|h| h.screen_to_string().contains("Vi mode enabled"))
        .unwrap();

    // Arrow keys are live while `arrowKeys` is on.
    let before = status_col(&harness);
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    let moved = status_col(&harness);
    assert!(
        moved > before,
        "with arrowKeys on, Right should move the cursor ({before} -> {moved})"
    );

    // Turn `arrowKeys` off. Fields render alphabetically: arrowKeys is
    // the first, so no Down steps.
    toggle_plugin_setting(&mut harness, "Plugin: vi_mode", 0);

    // Same keypress, now unbound in vi-normal (which is read-only, so it
    // can't fall through to inserting text either).
    let after_save = status_col(&harness);
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    assert_eq!(
        status_col(&harness),
        after_save,
        "after turning arrowKeys off and saving, Right must no longer move \
         the cursor — the vi modes should have been re-emitted. Screen:\n{}",
        harness.screen_to_string()
    );
}

/// git_explorer: flipping `colorNames` must repaint the explorer on save.
/// The plugin re-reads the setting, but only during a decoration refresh,
/// so before the hook the names kept their old color until the next file
/// open/save/explorer change.
#[test]
#[cfg_attr(windows, ignore)] // Git plugin tests are flaky on Windows CI
fn git_explorer_color_names_setting_repaints_on_save() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    repo.setup_git_explorer_plugin();
    repo.create_file("changed.txt", "one");
    repo.git_add_all();
    repo.git_commit("Initial commit");
    fs::write(repo.path.join("changed.txt"), "two").unwrap();

    let mut harness = EditorTestHarness::with_working_dir(120, 40, repo.path.clone()).unwrap();
    harness.editor_mut().toggle_file_explorer();
    harness
        .wait_until(|h| h.screen_to_string().contains("File Explorer"))
        .unwrap();
    // The decorations land asynchronously (the plugin shells out to git).
    harness
        .wait_until(|h| {
            h.screen_to_string()
                .lines()
                .any(|line| line.contains("changed.txt") && line.contains('M'))
        })
        .unwrap();

    let name_fg = |h: &EditorTestHarness| -> Option<ratatui::style::Color> {
        let screen = h.screen_to_string();
        let row = screen
            .lines()
            .position(|l| l.contains("changed.txt"))
            .unwrap_or_else(|| panic!("explorer row missing. Screen:\n{screen}"));
        let col = screen
            .lines()
            .nth(row)
            .unwrap()
            .find("changed.txt")
            .unwrap();
        h.buffer()[(col as u16, row as u16)].style().fg
    };

    // `colorNames` defaults off: the name paints in the explorer's normal
    // foreground, not the git-status color.
    let plain = name_fg(&harness);

    // Turn `colorNames` on — the plugin's only setting, so no Down steps.
    toggle_plugin_setting(&mut harness, "Plugin: git_ex", 0);

    harness
        .wait_until(|h| name_fg(h) != plain)
        .unwrap_or_else(|_| {
            panic!(
                "enabling colorNames must repaint the modified file's name on \
                 save (fg stayed {plain:?}). Screen:\n{}",
                harness.screen_to_string()
            )
        });
}
