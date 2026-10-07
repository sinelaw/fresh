//! Issue #3452: the empty pane left after closing the last buffer stayed at
//! `Color::Reset`, so the settings scrim could not darken it.

use crate::common::harness::EditorTestHarness;
use fresh::config::Config;
use fresh::input::keybindings::Action;
use ratatui::style::Color;

/// One buffer, closed, with the auto-created replacement and the explorer
/// both suppressed so the placeholder pane survives. `orchestrator_mode` is
/// off so the pane spans the frame and column 5 is the pane, not the dock.
fn empty_workspace(use_terminal_bg: bool) -> EditorTestHarness {
    let mut config = Config::default();
    config.editor.auto_create_empty_buffer_on_last_buffer_close = false;
    config.file_explorer.auto_open_on_last_buffer_close = false;
    config.editor.use_terminal_bg = use_terminal_bg;
    config.orchestrator_mode = false;

    let mut harness = EditorTestHarness::with_config(100, 32, config).unwrap();
    harness.load_buffer_from_text("line one\nline two").unwrap();
    harness
        .editor_mut()
        .dispatch_action_for_tests(Action::CloseTab);
    harness.render().unwrap();
    harness
}

/// Guards the setup: without this the assertions could pass over an ordinary
/// buffer pane and prove nothing.
fn shows_placeholder(harness: &EditorTestHarness) -> bool {
    let buffer = harness.editor().active_buffer();
    harness
        .editor()
        .active_window()
        .buffer_metadata
        .get(&buffer)
        .is_some_and(|m| m.synthetic_placeholder)
}

#[test]
fn empty_pane_carries_the_editor_ground() {
    let harness = empty_workspace(false);
    assert!(shows_placeholder(&harness), "setup: expected a placeholder");

    let (first, _last) = harness.content_area_rows();
    assert_eq!(
        harness
            .get_cell_style(5, first as u16 + 1)
            .and_then(|s| s.bg),
        Some(harness.editor().theme().editor_bg),
    );
}

/// `use_terminal_bg` hands the background to the terminal, as it does for the
/// rows past EOF (#779). Filling `editor.bg` here would put an opaque block
/// over it.
#[test]
fn empty_pane_follows_use_terminal_bg() {
    let harness = empty_workspace(true);
    assert!(shows_placeholder(&harness), "setup: expected a placeholder");

    let (first, _last) = harness.content_area_rows();
    assert_eq!(
        harness
            .get_cell_style(5, first as u16 + 1)
            .and_then(|s| s.bg),
        Some(Color::Reset),
    );
}
