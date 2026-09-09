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

// ─────────────────────────────────────────────────────────────────────────
// Minibuffer and menu: a keymap binding has to actually reach them.
//
// Both surfaces run their own input handler ahead of keybinding resolution —
// the menu's is capture-all — so a `prompt` or `menu` binding on anything but
// the handler's own hardcoded keys used to do nothing at all.
// ─────────────────────────────────────────────────────────────────────────

/// Open the goto-line prompt with `M-g g`.
fn open_goto_line(harness: &mut EditorTestHarness) {
    key(harness, KeyCode::Char('g'), KeyModifiers::ALT);
    key(harness, KeyCode::Char('g'), KeyModifiers::NONE);
}

#[test]
fn minibuffer_ctrl_b_and_ctrl_f_move_the_caret() {
    let mut harness = emacs_harness();
    open_goto_line(&mut harness);
    harness.type_text("123").unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("123");

    // C-b twice, then a digit: it lands between "1" and "2".
    ctrl(&mut harness, 'b');
    ctrl(&mut harness, 'b');
    harness.type_text("9").unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("192");

    // C-f moves back over the "2"; the next digit lands after it.
    ctrl(&mut harness, 'f');
    harness.type_text("7").unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("1927");
}

#[test]
fn minibuffer_ctrl_a_is_beginning_of_line_not_select_all() {
    // The prompt widget hardcodes Ctrl+A to select-all and runs before
    // keybinding resolution, so the Emacs binding could never win. Typing
    // after C-a proves which one did: beginning-of-line keeps the text.
    let mut harness = emacs_harness();
    open_goto_line(&mut harness);
    harness.type_text("123").unwrap();
    harness.render().unwrap();

    ctrl(&mut harness, 'a');
    harness.type_text("X").unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("X123");
}

#[test]
fn minibuffer_ctrl_g_cancels() {
    let mut harness = emacs_harness();
    open_goto_line(&mut harness);
    harness.type_text("123").unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("123");

    ctrl(&mut harness, 'g');
    harness.assert_screen_not_contains("Go to line");
}

#[test]
fn menu_ctrl_n_moves_the_highlight() {
    // The menu handler consumes every key it doesn't recognise, so `menu`
    // bindings never fired. Proven by what Enter runs: with C-n working, the
    // second File item (Open File…) opens its prompt; without it, Enter runs
    // the first item and makes a new buffer instead.
    let mut harness = emacs_harness();
    key(&mut harness, KeyCode::F(10), KeyModifiers::NONE);
    harness.assert_screen_contains("Open File");

    ctrl(&mut harness, 'n');
    key(&mut harness, KeyCode::Enter, KeyModifiers::NONE);
    harness.assert_screen_contains("Open file");
}

#[test]
fn menu_ctrl_g_closes_the_menu() {
    let mut harness = emacs_harness();
    key(&mut harness, KeyCode::F(10), KeyModifiers::NONE);
    harness.assert_screen_contains("Open File");

    ctrl(&mut harness, 'g');
    harness.assert_screen_not_contains("Open File");
}

/// The precedence change must not disturb the default keymap, which binds
/// `prompt` Ctrl+A to `prompt_select_all` — the same thing the widget does.
#[test]
fn default_keymap_minibuffer_ctrl_a_still_selects_all() {
    let mut harness = EditorTestHarness::create(
        80,
        24,
        HarnessOptions::new()
            .with_config(Config {
                active_keybinding_map: "default".into(),
                ..Default::default()
            })
            .with_preserved_keybinding_map(),
    )
    .unwrap();

    // Ctrl+G is goto-line in the default keymap.
    key(&mut harness, KeyCode::Char('g'), KeyModifiers::CONTROL);
    harness.type_text("123").unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("123");

    // Select-all, so the next character replaces the whole line.
    key(&mut harness, KeyCode::Char('a'), KeyModifiers::CONTROL);
    harness.type_text("X").unwrap();
    harness.render().unwrap();
    harness.assert_screen_not_contains("123");
    harness.assert_screen_contains("X");
}
