//! The theme editor's live preview ends with the editor, however it is
//! closed: the theme the user was running comes back exactly as it was, the
//! pane the editor opened goes with it, and the user's config is not
//! written.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use fresh::config_io::DirectoryContext;
use ratatui::style::Color;
use std::fs;

/// A user theme whose file (`mine.json`, what config names) and name
/// (`my-nord`) differ, so it cannot be found again by the name config gives
/// it: only the theme that was actually running is right to put back.
const MINE: &str = r#"{"name": "my-nord", "extends": "builtin://dark",
    "editor": {"bg": [120, 0, 0]}}"#;
const MINE_BG: Color = Color::Rgb(120, 0, 0);

struct Session {
    harness: EditorTestHarness,
    dir_context: DirectoryContext,
    _dirs: (tempfile::TempDir, tempfile::TempDir),
}

/// The editor on `mine.json`, with the theme editor open on the built-in
/// `light` — previewing it over the whole editor.
fn editing_light() -> Session {
    let context_temp = tempfile::TempDir::new().unwrap();
    let dir_context = DirectoryContext::for_testing(context_temp.path());
    fs::create_dir_all(dir_context.themes_dir()).unwrap();
    fs::write(dir_context.themes_dir().join("mine.json"), MINE).unwrap();

    let project_temp = tempfile::TempDir::new().unwrap();
    let project_root = project_temp.path().join("project_root");
    fs::create_dir(&project_root).unwrap();
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "theme_editor");

    let config = Config {
        theme: "mine.json".to_string().into(),
        ..Default::default()
    };
    let mut h = EditorTestHarness::with_shared_dir_context(
        140,
        40,
        config,
        project_root,
        dir_context.clone(),
    )
    .unwrap();
    h.render().unwrap();
    assert_eq!(
        text_bg(&h),
        Some(MINE_BG),
        "running my-nord first (theme {:?}):\n{}",
        h.editor().theme().name,
        h.screen_to_string()
    );

    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.type_text("Edit Theme").unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Select theme to edit"))
        .unwrap();
    h.type_text("light").unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Theme Editor: light"))
        .unwrap();
    h.wait_until(|h| text_bg(h).is_some_and(|bg| bg != MINE_BG))
        .unwrap();
    Session {
        harness: h,
        dir_context,
        _dirs: (context_temp, project_temp),
    }
}

/// The ground of the user's text: the first line of the first pane.
fn text_bg(h: &EditorTestHarness) -> Option<Color> {
    h.get_cell_style(20, 2).and_then(|s| s.bg)
}

/// After the theme editor is gone: my-nord again, one pane, no config.
fn assert_restored(s: &mut Session) {
    let h = &mut s.harness;
    h.wait_until(|h| !h.screen_to_string().contains("Theme Editor"))
        .unwrap();
    h.wait_until(|h| text_bg(h) == Some(MINE_BG))
        .unwrap_or_else(|e| {
            panic!(
                "my-nord is running again ({e}), got {:?}:\n{}",
                text_bg(h),
                h.screen_to_string()
            )
        });
    assert_eq!(
        h.editor()
            .active_window()
            .buffers
            .split_view_states()
            .map_or(0, |panes| panes.len()),
        1,
        "the theme editor's pane closed with it:\n{}",
        h.screen_to_string()
    );
    assert!(
        !s.dir_context.config_dir.join("config.json").exists(),
        "closing without saving writes no config"
    );
}

#[test]
fn closing_with_escape_puts_back_the_running_theme() {
    let mut s = editing_light();
    s.harness
        .send_key(KeyCode::Esc, KeyModifiers::NONE)
        .unwrap();
    assert_restored(&mut s);
}

#[test]
fn closing_the_buffer_puts_back_the_running_theme() {
    let mut s = editing_light();
    let h = &mut s.harness;
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.type_text("Close Buffer").unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    assert_restored(&mut s);
}
