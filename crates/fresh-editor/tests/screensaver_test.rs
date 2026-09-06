// Tests for the idle wave-animation screensaver.
//
// The screensaver is time-driven (start after N idle minutes), so the
// decision is exposed as `Editor::maybe_start_screensaver(idle)` which
// takes the elapsed idle duration explicitly. These tests drive that
// method with explicit durations — no sleeping, no wall-clock timing — and
// assert on the resulting state.

use crate::common::harness::EditorTestHarness;
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

/// The wave runs until the reader does something, and the dismissal rule is
/// one function every frontend calls — so drive that function with the raw
/// events a terminal actually delivers.
///
/// Bare pointer motion (`MouseEventKind::Moved`, what a terminal reports
/// under DECSET 1003) is the dismissal the status line promises first, and
/// the event is *consumed*: it stops the show without also acting on the
/// editor.
#[test]
fn mouse_move_dismisses_the_wave_and_is_consumed() {
    use crossterm::event::{KeyModifiers, MouseEvent, MouseEventKind};
    use fresh::server::input_parser::Event;

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();
    harness.editor_mut().trigger_wave_animation();
    assert!(harness.editor().wave_animation_active());

    let moved = Event::Mouse(MouseEvent {
        kind: MouseEventKind::Moved,
        column: 10,
        row: 5,
        modifiers: KeyModifiers::empty(),
    });
    assert!(
        harness.editor_mut().maybe_dismiss_wave_animation(&moved),
        "a bare mouse move must dismiss the wave and be consumed"
    );
    assert!(!harness.editor().wave_animation_active());

    // With no wave running the same event is nobody's business but the
    // editor's — the dismissal path must hand it back.
    assert!(!harness.editor_mut().maybe_dismiss_wave_animation(&moved));
}

/// Coming back to the window ends the show even when no pointer report ever
/// arrives. A terminal reports motion only over its own window, so a reader
/// returning from another app moves the mouse *there*, not here; what fresh
/// receives is `FocusGained`. Without this the screensaver outlived the
/// return until a key was pressed.
///
/// `FocusLost` is the opposite signal and must leave the wave alone.
#[test]
fn focus_gained_dismisses_the_wave_but_focus_lost_does_not() {
    use fresh::server::input_parser::Event;

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();
    harness.editor_mut().trigger_wave_animation();
    assert!(harness.editor().wave_animation_active());

    assert!(
        !harness
            .editor_mut()
            .maybe_dismiss_wave_animation(&Event::FocusLost),
        "losing focus is the reader leaving, not returning"
    );
    assert!(harness.editor().wave_animation_active());

    // Focus ends the show but is *not* consumed: the editor's own
    // on-return housekeeping still needs the event, so the dismissal path
    // hands it back (`false`) after cancelling.
    assert!(!harness
        .editor_mut()
        .maybe_dismiss_wave_animation(&Event::FocusGained));
    assert!(!harness.editor().wave_animation_active());
}

/// A bracketed paste is the clipboard telling the same story: it ends the
/// show, and the text still lands — the event is not swallowed.
#[test]
fn a_paste_dismisses_the_wave_without_swallowing_the_text() {
    use fresh::server::input_parser::Event;

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();
    harness.editor_mut().trigger_wave_animation();

    assert!(!harness
        .editor_mut()
        .maybe_dismiss_wave_animation(&Event::Paste("hello".to_string())));
    assert!(!harness.editor().wave_animation_active());
}

/// The status line the wave posts is an instruction for a show that is now
/// over ("press any key or move the mouse to stop"), so dismissing takes it
/// down with the animation instead of leaving it until the next action
/// writes one.
#[test]
fn dismissing_the_wave_clears_the_status_line_it_posted() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();
    harness.editor_mut().trigger_wave_animation();
    assert_eq!(
        harness.editor().get_status_message().map(String::as_str),
        Some(fresh_i18n::t!("wave.triggered").as_ref())
    );

    harness.editor_mut().cancel_wave_animation();
    assert_eq!(harness.editor().get_status_message(), None);
}

/// A message posted *after* the wave started belongs to whatever posted it;
/// dismissing the wave must not wipe it.
#[test]
fn dismissing_the_wave_leaves_a_later_status_message_alone() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();
    harness.editor_mut().trigger_wave_animation();
    harness
        .editor_mut()
        .set_status_message("saved 3 files".to_string());

    harness.editor_mut().cancel_wave_animation();
    assert_eq!(
        harness.editor().get_status_message().map(String::as_str),
        Some("saved 3 files")
    );
}
