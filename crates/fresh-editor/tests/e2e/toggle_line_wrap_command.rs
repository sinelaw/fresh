//! Regression test: the `Toggle Line Wrap` command must actually change
//! how the open buffer is rendered.
//!
//! Bug: when `editor.line_wrap = true` is set in the user config, opening
//! a file shows long lines wrapped (as expected), but running the
//! `Toggle Line Wrap` command from the command palette updates
//! `config.editor.line_wrap` and the status message — yet the buffer
//! keeps rendering with the previous wrap layout, because the per-leaf
//! wrap state on the viewport is updated without invalidating the
//! line-wrap cache / view layout that drives rendering.
//!
//! Reproduced interactively in tmux with the release binary; this test
//! is the automation of that reproduction.
//!
//! The assertion is purely on what shows up in the rendered screen:
//! we plant a unique `END-MARKER` far past the right edge of the
//! viewport, so it can only appear when the line wraps to additional
//! visual rows. Toggling wrap off must remove `END-MARKER` from the
//! screen; toggling it back on must restore it.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::fs;
use tempfile::TempDir;

const WIDTH: u16 = 60;
const HEIGHT: u16 = 24;

/// Run a command from the command palette by typing its name and pressing Enter.
fn run_command(harness: &mut EditorTestHarness, command_name: &str) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text(command_name).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// File content whose long line is much wider than `WIDTH` so it must
/// wrap onto multiple visual rows when wrapping is enabled. The
/// `END-MARKER` token sits past the right edge of the viewport — it
/// can only become visible if wrapping pushes it to a new visual row.
fn long_line_fixture() -> String {
    let filler = "filler ".repeat(30); // ~210 chars of filler past the screen edge
    format!("short before\nBEGIN-MARKER {filler}END-MARKER tail\nshort after\n")
}

fn open_long_file(harness: &mut EditorTestHarness) -> TempDir {
    let dir = TempDir::new().unwrap();
    let path = dir.path().join("long.txt");
    fs::write(&path, long_line_fixture()).unwrap();
    harness.open_file(&path).unwrap();
    harness.render().unwrap();
    dir
}

/// Toggling line wrap OFF from a config that starts with wrap ON must
/// stop wrapping the buffer on screen.
#[test]
fn toggle_line_wrap_off_actually_unwraps_buffer() {
    let mut config = Config::default();
    config.editor.line_wrap = true;

    let mut harness = EditorTestHarness::with_config(WIDTH, HEIGHT, config).unwrap();
    let _dir = open_long_file(&mut harness);

    let initial = harness.screen_to_string();
    assert!(
        initial.contains("BEGIN-MARKER"),
        "start of the long line should always be on screen.\nScreen:\n{}",
        initial
    );
    assert!(
        initial.contains("END-MARKER"),
        "with line_wrap=true, the wrapped tail of the long line should be on screen \
         (END-MARKER sits far past the right edge and is only reachable via wrapping).\nScreen:\n{}",
        initial
    );

    run_command(&mut harness, "Toggle Line Wrap");

    let after = harness.screen_to_string();
    assert!(
        after.contains("BEGIN-MARKER"),
        "start of the long line should still be visible after toggling wrap off.\nScreen:\n{}",
        after
    );
    assert!(
        !after.contains("END-MARKER"),
        "after Toggle Line Wrap with wrap previously on, the line must no longer wrap, \
         so END-MARKER (well past the right edge) must be off-screen.\nScreen:\n{}",
        after
    );
}

/// Symmetric case: toggling line wrap ON from a config that starts with
/// wrap OFF must start wrapping the buffer on screen.
#[test]
fn toggle_line_wrap_on_actually_wraps_buffer() {
    let mut config = Config::default();
    config.editor.line_wrap = false;

    let mut harness = EditorTestHarness::with_config(WIDTH, HEIGHT, config).unwrap();
    let _dir = open_long_file(&mut harness);

    let initial = harness.screen_to_string();
    assert!(
        initial.contains("BEGIN-MARKER"),
        "start of the long line should always be on screen.\nScreen:\n{}",
        initial
    );
    assert!(
        !initial.contains("END-MARKER"),
        "with line_wrap=false, END-MARKER must be off-screen (past the right edge).\nScreen:\n{}",
        initial
    );

    run_command(&mut harness, "Toggle Line Wrap");

    let after = harness.screen_to_string();
    assert!(
        after.contains("END-MARKER"),
        "after Toggle Line Wrap with wrap previously off, the line must now wrap, \
         so END-MARKER must appear on a continuation visual row.\nScreen:\n{}",
        after
    );
}
