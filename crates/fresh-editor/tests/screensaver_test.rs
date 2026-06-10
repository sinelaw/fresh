// Tests for the idle wave-animation screensaver.
//
// The screensaver is time-driven (start after N idle minutes), so the
// decision is exposed as `Editor::maybe_start_screensaver(idle)` which
// takes the elapsed idle duration explicitly. These tests drive that
// method with explicit durations — no sleeping, no wall-clock timing — and
// assert on the resulting state.
mod common;

use common::harness::EditorTestHarness;
use std::time::Duration;

/// With the screensaver enabled and a 5-minute threshold, being idle past
/// the threshold starts the wave; below it does nothing; and it does not
/// restart while already running.
#[test]
fn screensaver_starts_wave_after_configured_idle() {
    let mut config = fresh::config::Config::default();
    config.editor.screensaver_enabled = true;
    config.editor.screensaver_idle_minutes = 5;
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    // Render once so the editor knows its terminal size (the wave needs a
    // non-empty area to start).
    harness.render().unwrap();

    // Below the threshold: nothing happens.
    assert!(!harness
        .editor_mut()
        .maybe_start_screensaver(Duration::from_secs(4 * 60)));
    assert!(!harness.editor().wave_animation_active());

    // At/over the threshold: the wave kicks in.
    assert!(harness
        .editor_mut()
        .maybe_start_screensaver(Duration::from_secs(5 * 60)));
    assert!(harness.editor().wave_animation_active());

    // Already running: a further idle tick must not start a second wave.
    assert!(!harness
        .editor_mut()
        .maybe_start_screensaver(Duration::from_secs(20 * 60)));
}

/// The screensaver is opt-in: with the default config it never starts, no
/// matter how long the editor has been idle.
#[test]
fn screensaver_disabled_by_default() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();
    assert!(harness.editor().screensaver_idle_timeout().is_none());
    assert!(!harness
        .editor_mut()
        .maybe_start_screensaver(Duration::from_secs(60 * 60)));
    assert!(!harness.editor().wave_animation_active());
}

/// A zero-minute threshold disables the screensaver even when the enable
/// flag is set, so it can't fire on every idle poll.
#[test]
fn screensaver_zero_minutes_is_disabled() {
    let mut config = fresh::config::Config::default();
    config.editor.screensaver_enabled = true;
    config.editor.screensaver_idle_minutes = 0;
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    harness.render().unwrap();
    assert!(harness.editor().screensaver_idle_timeout().is_none());
    assert!(!harness
        .editor_mut()
        .maybe_start_screensaver(Duration::from_secs(60 * 60)));
    assert!(!harness.editor().wave_animation_active());
}
