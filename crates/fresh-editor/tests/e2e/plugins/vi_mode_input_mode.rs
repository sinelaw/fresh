//! E2E: vi is an editor-wide input mode, not a window's editor mode
//! (sinelaw/fresh#3395).
//!
//! vi_mode keeps one plugin-wide state, and "vi is on" is a preference about
//! how the user types. It used to live in the active window's editor-mode
//! slot, so a window created after vi was enabled had no mode at all (`j`
//! typed a `j` while the status bar said `-- NORMAL --`), and anything that
//! wrote that slot in one window was invisible to — or clobbered — another.
//! vi now sets the editor-wide input mode (`setInputMode`); a window's own
//! editor mode, a buffer's mode and a focused panel's mode still outrank it
//! where they are set, and only there.
//!
//! `tests/plugins/test_input_mode_scope.ts` supplies a window-scoped mode and
//! a panel buffer with its own mode, both binding `j` to a status marker.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::{Config, PluginConfig};
use fresh::input::keybindings::Action::PluginAction;
use fresh_core::WindowId;
use std::fs;
use std::path::PathBuf;

struct Setup {
    h: EditorTestHarness,
    window_a: WindowId,
    other_root: PathBuf,
    b_file: PathBuf,
    _temp: tempfile::TempDir,
}

/// vi_mode with `autoStart`, the scope test plugin, and `a.txt` open in the
/// first window. A second project directory holding `b.txt` is ready for a
/// window created later.
fn setup() -> Setup {
    setup_with(&[], true)
}

fn setup_with(extra_plugins: &[&str], auto_start: bool) -> Setup {
    init_tracing_from_env();
    let temp = tempfile::TempDir::new().unwrap();
    let project_root = temp.path().join("project_root");
    let plugins_dir = project_root.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "vi_mode");
    for plugin in extra_plugins {
        copy_plugin(&plugins_dir, plugin);
    }
    copy_plugin_lib(&plugins_dir);
    fs::write(
        plugins_dir.join("test_input_mode_scope.ts"),
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/tests/plugins/test_input_mode_scope.ts"
        )),
    )
    .unwrap();
    let a_file = project_root.join("a.txt");
    fs::write(&a_file, "a1\na2\na3\na4\n").unwrap();
    let other_root = temp.path().join("other_root");
    fs::create_dir_all(&other_root).unwrap();
    let b_file = other_root.join("b.txt");
    fs::write(&b_file, "b1\nb2\nb3\nb4\n").unwrap();

    let mut config = Config::default();
    config.plugins.insert(
        "vi_mode".to_string(),
        PluginConfig {
            enabled: true,
            path: None,
            settings: serde_json::json!({ "autoStart": auto_start }),
        },
    );
    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 32, config, project_root).unwrap();
    h.editor_mut().set_clipboard_for_test(String::new());
    h.wait_until(|h| {
        let cmds = h.editor().command_registry().read().unwrap().get_all();
        ["vi_mode_toggle", "tim_window_mode", "tim_open_panel"]
            .iter()
            .all(|name| {
                cmds.iter()
                    .any(|c| c.action == PluginAction(name.to_string()))
            })
    })
    .unwrap();
    // autoStart enables vi from the plugin's top-level body; wait for it to
    // say so before driving keys.
    if auto_start {
        h.wait_until(|h| h.screen_to_string().contains("Vi mode enabled"))
            .unwrap();
    }
    h.open_file(&a_file).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("a.txt"))
        .unwrap();
    let window_a = h.editor().active_window_id();
    Setup {
        h,
        window_a,
        other_root,
        b_file,
        _temp: temp,
    }
}

/// Create a second window (after vi was enabled), make it active and open
/// `b.txt` there.
fn open_window_b(s: &mut Setup) -> WindowId {
    let window_b =
        s.h.editor_mut()
            .create_window_at(s.other_root.clone(), "other".into());
    s.h.editor_mut().set_active_window(window_b);
    s.h.open_file(&s.b_file).unwrap();
    s.h.wait_until(|h| h.screen_to_string().contains("b.txt"))
        .unwrap();
    window_b
}

fn switch_to(h: &mut EditorTestHarness, window: WindowId, file: &str) {
    h.editor_mut().set_active_window(window);
    h.render().unwrap();
    h.wait_until(|h| h.screen_to_string().contains(file))
        .unwrap();
}

/// Press `j` in a buffer whose cursor is at column 1 and wait for either
/// outcome: vi-normal moved the cursor down to `line`, or a `j` was typed
/// (the cursor went to column 2). Returns the screen.
fn press_j(h: &mut EditorTestHarness, line: usize) -> String {
    let moved = format!("Ln {line}, Col 1");
    h.send_key(KeyCode::Char('j'), KeyModifiers::NONE).unwrap();
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains(&moved) || s.contains(", Col 2")
    })
    .unwrap();
    h.screen_to_string()
}

fn assert_vi_j_moves_to(h: &mut EditorTestHarness, line: usize, where_: &str) {
    let screen = press_j(h, line);
    assert!(
        screen.contains(&format!("Ln {line}, Col 1")),
        "{where_}: vi-normal `j` must move the cursor to line {line}, not type a `j`.\n\
         Screen:\n{screen}"
    );
}

/// vi enabled at startup, then a new window: `j` moves the cursor there.
#[test]
fn vi_autostart_applies_in_a_window_created_later() {
    let mut s = setup();
    open_window_b(&mut s);
    assert_vi_j_moves_to(&mut s.h, 2, "the new window");
}

/// Switching windows back and forth keeps vi active in both.
#[test]
fn vi_stays_active_across_window_switches() {
    let mut s = setup();
    let window_a = s.window_a;
    assert_vi_j_moves_to(&mut s.h, 2, "window A");
    let window_b = open_window_b(&mut s);
    assert_vi_j_moves_to(&mut s.h, 2, "window B");
    switch_to(&mut s.h, window_a, "a.txt");
    assert_vi_j_moves_to(&mut s.h, 3, "window A, after visiting B");
    switch_to(&mut s.h, window_b, "b.txt");
    assert_vi_j_moves_to(&mut s.h, 3, "window B, after returning to A");
}

/// A panel with its own mode in window A takes `j` there, doesn't affect
/// window B's keys, and closing it leaves vi active.
#[test]
fn a_panel_mode_in_one_window_leaves_vi_alone_elsewhere_and_after_close() {
    let mut s = setup();
    let window_a = s.window_a;
    let window_b = open_window_b(&mut s);
    switch_to(&mut s.h, window_a, "a.txt");

    s.h.editor_mut()
        .dispatch_action_for_tests(PluginAction("tim_open_panel".to_string()));
    s.h.wait_until(|h| h.screen_to_string().contains("PANEL-CONTENT"))
        .unwrap();
    s.h.send_key(KeyCode::Char('j'), KeyModifiers::NONE)
        .unwrap();
    s.h.wait_until(|h| h.screen_to_string().contains("PANEL-MODE-J"))
        .unwrap();

    switch_to(&mut s.h, window_b, "b.txt");
    assert_vi_j_moves_to(&mut s.h, 2, "window B, with a panel mode up in A");

    switch_to(&mut s.h, window_a, "PANEL-CONTENT");
    s.h.editor_mut()
        .dispatch_action_for_tests(PluginAction("tim_close_panel".to_string()));
    s.h.wait_until(|h| !h.screen_to_string().contains("PANEL-CONTENT"))
        .unwrap();
    assert_vi_j_moves_to(&mut s.h, 2, "window A, after closing the panel");
}

/// A window-scoped plugin mode set in window A holds there and doesn't leak
/// into window B, where vi keeps the keys.
#[test]
fn a_window_scoped_mode_does_not_leak_into_another_window() {
    let mut s = setup();
    let window_a = s.window_a;
    let window_b = open_window_b(&mut s);
    switch_to(&mut s.h, window_a, "a.txt");

    s.h.editor_mut()
        .dispatch_action_for_tests(PluginAction("tim_window_mode".to_string()));
    s.h.wait_until(|h| h.screen_to_string().contains("WINDOW-MODE-ON"))
        .unwrap();
    s.h.send_key(KeyCode::Char('j'), KeyModifiers::NONE)
        .unwrap();
    s.h.wait_until(|h| h.screen_to_string().contains("WINDOW-MODE-J"))
        .unwrap();

    switch_to(&mut s.h, window_b, "b.txt");
    assert_vi_j_moves_to(&mut s.h, 2, "window B, with a window mode set in A");
}

/// Turning vi off in one window turns it off in every window.
#[test]
fn disabling_vi_takes_effect_in_every_window() {
    let mut s = setup();
    let window_a = s.window_a;
    let window_b = open_window_b(&mut s);
    assert_vi_j_moves_to(&mut s.h, 2, "window B, before disabling vi");

    s.h.editor_mut()
        .dispatch_action_for_tests(PluginAction("vi_mode_toggle".to_string()));
    s.h.wait_until(|h| h.screen_to_string().contains("Vi mode disabled"))
        .unwrap();
    s.h.send_key(KeyCode::Char('j'), KeyModifiers::NONE)
        .unwrap();
    s.h.wait_until(|h| h.screen_to_string().contains("jb2"))
        .unwrap();

    switch_to(&mut s.h, window_a, "a.txt");
    let screen = press_j(&mut s.h, 2);
    assert!(
        screen.contains("ja1") && screen.contains("Ln 1, Col 2"),
        "vi was turned off in window B, so `j` in window A must type a `j`.\n\
         Screen:\n{screen}"
    );
    let _ = window_b;
}

/// markdown-source is a window-scoped mode, which outranks the input mode, so
/// it has to step aside for vi: turning vi on while a markdown file is open
/// must give vi the keys there, not leave markdown-source shadowing it.
#[test]
fn turning_vi_on_in_a_markdown_file_gives_vi_the_keys() {
    let mut s = setup_with(&["markdown_source"], false);
    let md = s.other_root.join("notes.md");
    fs::write(&md, "m1\nm2\nm3\n").unwrap();
    s.h.open_file(&md).unwrap();
    s.h.wait_until(|h| h.screen_to_string().contains("notes.md"))
        .unwrap();
    // Synchronisation only: markdown-source has claimed the window.
    s.h.wait_until(|h| h.editor().editor_mode().as_deref() == Some("markdown-source"))
        .unwrap();

    s.h.editor_mut()
        .dispatch_action_for_tests(PluginAction("vi_mode_toggle".to_string()));
    s.h.wait_until(|h| h.screen_to_string().contains("Vi mode enabled"))
        .unwrap();
    // Synchronisation only: markdown-source hears `input_mode_changed`
    // through the plugin queue, after vi's status line is already up.
    s.h.wait_until(|h| h.editor().editor_mode().is_none())
        .unwrap();
    assert_vi_j_moves_to(&mut s.h, 2, "a markdown file, vi turned on there");
}
