//! A widget that opts into hover (`WidgetSpec::Button::hoverable`) gets
//! `widget_event { event_type: "hover", payload: { hovered } }` as the
//! pointer enters and leaves it; one that doesn't opt in gets nothing.
//!
//! Hover is host state — it changes on mouse motion with no plugin
//! round-trip — so it reaches the renderer through `RenderContext`
//! alongside focus, and reaches the *plugin* only for widgets that asked.
//! That gate is the half worth guarding: without it every panel would
//! wake its plugin each time the pointer crossed any control.
//!
//! The `test_widget_hover_events.ts` plugin renders `ENTERS=<n>
//! LEAVES=<n> STRAY=<n>` above two buttons — one `hoverable`, one not —
//! so this drives the mouse and asserts purely on rendered output
//! (CONTRIBUTING §2).

use crate::common::harness::{copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

/// Install the hover-event test plugin into the project's plugin dir.
fn install_plugin(project_root: &std::path::Path) {
    let plugins_dir = project_root.join("plugins");
    fs::create_dir_all(&plugins_dir).expect("create plugins dir");
    copy_plugin_lib(&plugins_dir);

    const SRC: &str = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/tests/plugins/test_widget_hover_events.ts"
    ));
    let dst = plugins_dir.join("test_widget_hover_events.ts");
    fs::write(&dst, SRC).unwrap_or_else(|e| panic!("Failed to write test plugin to {dst:?}: {e}"));
}

/// Screen (col, row) of the first cell of `needle`, or panic with the screen.
fn pos_of(h: &EditorTestHarness, needle: &str) -> (u16, u16) {
    let screen = h.screen_to_string();
    for (row, line) in screen.lines().enumerate() {
        if let Some(byte_idx) = line.find(needle) {
            let col = line[..byte_idx].chars().count();
            return (col as u16, row as u16);
        }
    }
    panic!("screen missing '{needle}':\n{screen}");
}

#[test]
fn hover_events_reach_only_the_widget_that_opted_in() {
    init_tracing_from_env();

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();
    install_plugin(&project_root);

    let mut h =
        EditorTestHarness::with_config_and_working_dir(100, 32, Default::default(), project_root)
            .unwrap();
    h.render().unwrap();

    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();
    h.type_text("TestHover: Mount").unwrap();
    h.render().unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("ENTERS=0 LEAVES=0 STRAY=0"))
        .unwrap();

    let (watched_col, watched_row) = pos_of(&h, "WATCHED");
    let (quiet_col, quiet_row) = pos_of(&h, "QUIET");

    // Enter the opted-in button.
    h.mouse_move(watched_col, watched_row).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("ENTERS=1 LEAVES=0"))
        .unwrap();

    // Moving *within* the same button is not a new transition — the host
    // compares the resolved widget key before doing anything, so sliding
    // across its label must not re-fire.
    h.mouse_move(watched_col + 1, watched_row).unwrap();
    h.mouse_move(watched_col + 2, watched_row).unwrap();
    h.wait_until_stable(|_| true).unwrap();
    let screen = h.screen_to_string();
    assert!(
        screen.contains("ENTERS=1 LEAVES=0"),
        "moving inside one widget must not re-fire hover:\n{screen}"
    );

    // Cross to the button that did NOT opt in: the leave fires for the
    // watched button, and nothing at all fires for the quiet one.
    h.mouse_move(quiet_col, quiet_row).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("ENTERS=1 LEAVES=1"))
        .unwrap();
    h.wait_until_stable(|_| true).unwrap();
    let screen = h.screen_to_string();
    assert!(
        screen.contains("STRAY=0"),
        "a widget without `hoverable` must never wake the plugin:\n{screen}"
    );

    // Back onto the watched button: enter fires again, so the tracking is
    // live rather than latching after the first crossing.
    h.mouse_move(watched_col, watched_row).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("ENTERS=2 LEAVES=1"))
        .unwrap();

    // Leaving the panel entirely still delivers the matching leave — a
    // plugin that got an enter must always get its leave.
    h.mouse_move(0, 0).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("ENTERS=2 LEAVES=2"))
        .unwrap();
    let screen = h.screen_to_string();
    assert!(
        screen.contains("STRAY=0"),
        "no stray hover events over the whole run:\n{screen}"
    );
}
