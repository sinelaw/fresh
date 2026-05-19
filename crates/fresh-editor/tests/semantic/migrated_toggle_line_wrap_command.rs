//! Migration of `tests/e2e/toggle_line_wrap_command.rs` — the
//! `Toggle Line Wrap` command from the command palette must actually
//! change how the open buffer is rendered.
//!
//! Bug under regression: when `editor.line_wrap = true` is set in the
//! user config, opening a file shows long lines wrapped (as expected),
//! but running `Toggle Line Wrap` from the command palette updates
//! `config.editor.line_wrap` and the status message — yet the buffer
//! keeps rendering with the previous wrap layout, because the per-leaf
//! wrap state on the viewport is updated without invalidating the
//! line-wrap cache / view layout that drives rendering.
//!
//! Load-bearing claims preserved here:
//!
//!   1. **Toggling wrap OFF actually unwraps.** Starting from a config
//!      with `line_wrap = true`, the planted `END-MARKER` (far past
//!      the right edge of the 60-col viewport) is initially visible
//!      because the long line wraps to a continuation row. After
//!      running `Toggle Line Wrap`, `END-MARKER` must disappear from
//!      the screen (no wrap = no continuation = off-screen).
//!
//!   2. **Toggling wrap ON actually wraps.** Symmetric: starting with
//!      `line_wrap = false`, `END-MARKER` starts off-screen. After
//!      `Toggle Line Wrap`, it must appear (wrap forces a continuation
//!      row that holds the marker).
//!
//! ## Harness-direct pattern
//!
//! The command-palette routing (Ctrl+P → type → Enter), `open_file`
//! against a real `TempDir`, and screen-text inspection via
//! `screen_to_string()` all live on `EditorTestHarness` with no
//! `EditorTestApi` projections. Migrated as harness-direct so the
//! palette routing exercises the same key handler the e2e drove.
//!
//! Source: `tests/e2e/toggle_line_wrap_command.rs` (2 tests
//! migrated; no tests deferred).

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::fs;
use tempfile::TempDir;

const WIDTH: u16 = 60;
const HEIGHT: u16 = 24;

/// Run a command from the command palette by typing its name and
/// pressing Enter — mirrors the e2e's helper.
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

/// Long-line fixture: a unique `END-MARKER` token sits well past the
/// right edge of a 60-col viewport. The only way for it to become
/// visible is via wrapping pushing the tail to a continuation row.
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

#[test]
fn migrated_toggle_line_wrap_off_actually_unwraps_buffer() {
    // Original: `toggle_line_wrap_off_actually_unwraps_buffer`.
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

#[test]
fn migrated_toggle_line_wrap_on_actually_wraps_buffer() {
    // Original: `toggle_line_wrap_on_actually_wraps_buffer`.
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

/// Anti-test: drop the `run_command("Toggle Line Wrap")` step in the
/// wrap-on→off scenario. Without the toggle, the line stays wrapped
/// and `END-MARKER` must remain visible — proves the disappearance
/// in the positive test is caused by the palette command, not by
/// some incidental rerender or by `open_file` accidentally
/// unwrapping the buffer.
#[test]
fn anti_toggle_line_wrap_without_command_keeps_end_marker_visible() {
    let mut config = Config::default();
    config.editor.line_wrap = true;

    let mut harness = EditorTestHarness::with_config(WIDTH, HEIGHT, config).unwrap();
    let _dir = open_long_file(&mut harness);

    let initial = harness.screen_to_string();
    assert!(
        initial.contains("END-MARKER"),
        "precondition: with line_wrap=true END-MARKER should start \
         visible via wrapping.\nScreen:\n{}",
        initial
    );

    // No run_command here — the load-bearing step we drop.
    harness.render().unwrap();

    let after = harness.screen_to_string();
    assert!(
        after.contains("END-MARKER"),
        "anti: without `Toggle Line Wrap` END-MARKER must stay \
         visible — the disappearance in the positive test is caused \
         by the palette command, not by an incidental rerender.\n\
         Screen:\n{}",
        after
    );
}
