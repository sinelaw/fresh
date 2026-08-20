//! End-to-end coverage for the Emacs keymap itself — the keys, not the
//! actions behind them. Everything here drives real keystrokes through the
//! `emacs` map and asserts on what lands on screen.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;

fn emacs_harness() -> EditorTestHarness {
    let config = Config {
        active_keybinding_map: "emacs".into(),
        ..Default::default()
    };
    EditorTestHarness::create(
        80,
        24,
        HarnessOptions::new()
            .with_config(config)
            .with_preserved_keybinding_map(),
    )
    .unwrap()
}

fn key(harness: &mut EditorTestHarness, code: KeyCode, modifiers: KeyModifiers) {
    harness.send_key(code, modifiers).unwrap();
    harness.render().unwrap();
}

fn ctrl(harness: &mut EditorTestHarness, c: char) {
    key(harness, KeyCode::Char(c), KeyModifiers::CONTROL);
}

fn alt(harness: &mut EditorTestHarness, c: char) {
    key(harness, KeyCode::Char(c), KeyModifiers::ALT);
}

/// `C-k` is Emacs `kill-line`: it kills from point to end of line. It used to
/// be bound to `delete_line`, which also threw away the text *before* point.
#[test]
fn ctrl_k_kills_only_to_end_of_line() {
    let mut harness = emacs_harness();
    harness.type_text("alpha beta gamma").unwrap();
    harness.render().unwrap();

    // C-a to line start, then C-f five times to sit just after "alpha".
    ctrl(&mut harness, 'a');
    for _ in 0..5 {
        ctrl(&mut harness, 'f');
    }
    ctrl(&mut harness, 'k');

    harness.assert_screen_contains("alpha");
    harness.assert_screen_not_contains("beta");
}

/// `M-<` / `M->` move to the ends of the buffer. Both were dead: the keymap
/// spelled them as the *unshifted* character plus a `shift` modifier, but a
/// terminal sends `ESC <`, i.e. the shifted glyph with Alt.
#[test]
fn meta_angle_brackets_jump_to_buffer_ends() {
    let mut harness = emacs_harness();
    harness.type_text("one\ntwo\nthree").unwrap();
    harness.render().unwrap();

    alt(&mut harness, '<');
    harness.type_text("TOP").unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("TOPone");

    alt(&mut harness, '>');
    harness.type_text("END").unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("threeEND");
}

/// `C-x h` is `mark-whole-buffer`. Typing then replaces everything, which is
/// how the selection becomes visible on screen.
#[test]
fn ctrl_x_h_marks_the_whole_buffer() {
    let mut harness = emacs_harness();
    harness.type_text("alpha\nbeta").unwrap();
    harness.render().unwrap();

    ctrl(&mut harness, 'x');
    key(&mut harness, KeyCode::Char('h'), KeyModifiers::NONE);
    harness.type_text("Z").unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("Z");
    harness.assert_screen_not_contains("alpha");
    harness.assert_screen_not_contains("beta");
}

/// `C-x u` is undo — one of the `C-x` commands the map didn't carry.
#[test]
fn ctrl_x_u_undoes() {
    let mut harness = emacs_harness();
    harness.type_text("keep").unwrap();
    harness.render().unwrap();
    harness.type_text("DROP").unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("keepDROP");

    ctrl(&mut harness, 'x');
    key(&mut harness, KeyCode::Char('u'), KeyModifiers::NONE);
    harness.assert_screen_not_contains("DROP");
}

/// The Emacs map inherits `default`, so keys it has no opinion about still
/// work. `M-u` (upcase-word) is one of them — before the keymap inherited,
/// it did nothing at all.
#[test]
fn inherited_default_bindings_still_fire() {
    let mut harness = emacs_harness();
    harness.type_text("alpha beta").unwrap();
    harness.render().unwrap();

    ctrl(&mut harness, 'a');
    alt(&mut harness, 'u');

    harness.assert_screen_contains("ALPHA beta");
}
