//! The single source of truth for "which key performs which text-editing
//! operation" across every text surface in the editor.
//!
//! Historically two independent handlers each had their own `key → op`
//! table: the Settings UI's `handle_text_editing_input` and the plugin
//! widget runtime's `handle_widget_text_key`. They drifted — the Settings
//! table was missing `Home`/`End` and routed `Delete` to list-item removal,
//! so a scalar field like *Terminal ▸ Command* silently ignored those keys
//! while the widget path handled them fine.
//!
//! [`apply_text_key`] is that table, extracted once. Both handlers now feed
//! it a `KeyEvent` and a [`TextSurface`] (their focused editor), so the
//! motion / mutation / selection semantics can no longer diverge — a new
//! surface gets the whole behavior for free, and a missing arm is a
//! compile-time `match` gap, not a per-site omission.
//!
//! Only the *universal* text keys live here — caret motion, text mutation,
//! and shift-selection. Keys that legitimately mean different things per
//! surface (Enter = commit-a-field vs insert-a-newline, Tab = focus-advance,
//! Esc = revert, and the clipboard chords, which ride each surface's own
//! clipboard plumbing) stay in the outer handlers: [`apply_text_key`]
//! returns [`TextKeyResult::Ignored`] for them so the caller can fall
//! through to its own chrome.
//!
//! This module is `runtime`-gated because it speaks crossterm's `KeyEvent`;
//! both callers (`view::settings::input`, `app::widget_runtime`) are
//! `runtime`-only, so the wasm build never needs it.

use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

/// A text-editing surface the shared key router can drive. Implemented by
/// the raw [`TextEdit`](crate::primitives::text_edit::TextEdit) engine (the
/// plugin widget runtime's editors) and by
/// [`TextInputState`](crate::view::controls::TextInputState) (the Settings
/// UI's single-line control), so one `key → op` table serves both.
///
/// Each method is a *primitive* editing operation; the routing from a key
/// chord to the right primitive lives in [`apply_text_key`] alone.
pub trait TextSurface {
    /// Insert printable text at the caret (a single grapheme, or an
    /// IME/paste chunk). Newlines are flattened by single-line surfaces.
    fn insert_text(&mut self, text: &str);
    /// Delete the grapheme before the caret (Backspace).
    fn backspace(&mut self);
    /// Delete the grapheme at the caret (Delete).
    fn delete_forward(&mut self);
    /// Delete the word before the caret (Ctrl+Backspace).
    fn delete_word_backward(&mut self);
    /// Delete the word at/after the caret (Ctrl+Delete).
    fn delete_word_forward(&mut self);

    /// Move the caret one grapheme left / right.
    fn move_left(&mut self);
    fn move_right(&mut self);
    /// Move the caret one visual line up / down (multi-line only).
    fn move_up(&mut self);
    fn move_down(&mut self);
    /// Move the caret to the start / end of the line.
    fn move_home(&mut self);
    fn move_end(&mut self);
    /// Move the caret one word left / right.
    fn move_word_left(&mut self);
    fn move_word_right(&mut self);

    /// Extend the selection by the matching motion (the Shift+… chords).
    fn extend_left(&mut self);
    fn extend_right(&mut self);
    fn extend_up(&mut self);
    fn extend_down(&mut self);
    fn extend_home(&mut self);
    fn extend_end(&mut self);
    fn extend_word_left(&mut self);
    fn extend_word_right(&mut self);
}

/// Per-call routing context — currently just whether the surface is
/// multi-line. Single-line surfaces return [`TextKeyResult::Ignored`] for
/// Up/Down so the caller can repurpose them (field-focus navigation in
/// Settings, picker navigation in widget panels).
#[derive(Debug, Clone, Copy)]
pub struct TextKeyContext {
    /// True when the surface has more than one row.
    pub multiline: bool,
}

impl TextKeyContext {
    /// A single-line text surface.
    pub fn single_line() -> Self {
        Self { multiline: false }
    }

    /// A multi-line text surface (Up/Down move the caret between rows).
    pub fn multiline(multiline: bool) -> Self {
        Self { multiline }
    }
}

/// Outcome of routing one key through [`apply_text_key`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TextKeyResult {
    /// The key was a text-editing key and was applied to the surface.
    Handled,
    /// Not a universal text-editing key — the caller should handle it
    /// (Enter/Tab/Esc/clipboard chords, or Up/Down on a single-line
    /// surface).
    Ignored,
}

/// Route one key event to the matching text-editing operation on `surface`.
///
/// This is the *only* place the `key → op` mapping is defined; both the
/// Settings input handler and the plugin widget runtime call it. Returns
/// [`TextKeyResult::Handled`] when the key was a universal text-editing key
/// (and was applied), or [`TextKeyResult::Ignored`] when the caller should
/// handle it as chrome.
///
/// Ctrl/Alt-modified printable chars (Ctrl+A/C/V/X, Alt-chords) are
/// deliberately *not* inserted — they fall through to `Ignored` so each
/// surface can apply its own select-all / clipboard behavior.
pub fn apply_text_key<S: TextSurface + ?Sized>(
    surface: &mut S,
    key: &KeyEvent,
    ctx: TextKeyContext,
) -> TextKeyResult {
    let ctrl = key.modifiers.contains(KeyModifiers::CONTROL);
    let shift = key.modifiers.contains(KeyModifiers::SHIFT);
    let alt = key.modifiers.contains(KeyModifiers::ALT);

    match key.code {
        // Printable insertion — plain or Shift-modified (uppercase). Ctrl/
        // Alt chords are left for the caller's clipboard/select handling.
        KeyCode::Char(c) if !ctrl && !alt => {
            let mut buf = [0u8; 4];
            surface.insert_text(c.encode_utf8(&mut buf));
            TextKeyResult::Handled
        }

        KeyCode::Backspace if ctrl => {
            surface.delete_word_backward();
            TextKeyResult::Handled
        }
        KeyCode::Backspace => {
            surface.backspace();
            TextKeyResult::Handled
        }
        KeyCode::Delete if ctrl => {
            surface.delete_word_forward();
            TextKeyResult::Handled
        }
        KeyCode::Delete => {
            surface.delete_forward();
            TextKeyResult::Handled
        }

        KeyCode::Left if ctrl && shift => {
            surface.extend_word_left();
            TextKeyResult::Handled
        }
        KeyCode::Left if ctrl => {
            surface.move_word_left();
            TextKeyResult::Handled
        }
        KeyCode::Left if shift => {
            surface.extend_left();
            TextKeyResult::Handled
        }
        KeyCode::Left => {
            surface.move_left();
            TextKeyResult::Handled
        }

        KeyCode::Right if ctrl && shift => {
            surface.extend_word_right();
            TextKeyResult::Handled
        }
        KeyCode::Right if ctrl => {
            surface.move_word_right();
            TextKeyResult::Handled
        }
        KeyCode::Right if shift => {
            surface.extend_right();
            TextKeyResult::Handled
        }
        KeyCode::Right => {
            surface.move_right();
            TextKeyResult::Handled
        }

        KeyCode::Home if shift => {
            surface.extend_home();
            TextKeyResult::Handled
        }
        KeyCode::Home => {
            surface.move_home();
            TextKeyResult::Handled
        }
        KeyCode::End if shift => {
            surface.extend_end();
            TextKeyResult::Handled
        }
        KeyCode::End => {
            surface.move_end();
            TextKeyResult::Handled
        }

        // Up/Down only edit text on a multi-line surface; single-line
        // surfaces leave them for the caller (field / picker navigation).
        KeyCode::Up if ctx.multiline && shift => {
            surface.extend_up();
            TextKeyResult::Handled
        }
        KeyCode::Up if ctx.multiline => {
            surface.move_up();
            TextKeyResult::Handled
        }
        KeyCode::Down if ctx.multiline && shift => {
            surface.extend_down();
            TextKeyResult::Handled
        }
        KeyCode::Down if ctx.multiline => {
            surface.move_down();
            TextKeyResult::Handled
        }

        _ => TextKeyResult::Ignored,
    }
}

impl TextSurface for crate::primitives::text_edit::TextEdit {
    fn insert_text(&mut self, text: &str) {
        self.insert_str(text);
    }
    fn backspace(&mut self) {
        self.backspace();
    }
    fn delete_forward(&mut self) {
        self.delete();
    }
    fn delete_word_backward(&mut self) {
        self.delete_word_backward();
    }
    fn delete_word_forward(&mut self) {
        self.delete_word_forward();
    }
    fn move_left(&mut self) {
        self.move_left();
    }
    fn move_right(&mut self) {
        self.move_right();
    }
    fn move_up(&mut self) {
        self.move_up();
    }
    fn move_down(&mut self) {
        self.move_down();
    }
    fn move_home(&mut self) {
        self.move_home();
    }
    fn move_end(&mut self) {
        self.move_end();
    }
    fn move_word_left(&mut self) {
        self.move_word_left();
    }
    fn move_word_right(&mut self) {
        self.move_word_right();
    }
    fn extend_left(&mut self) {
        self.move_left_selecting();
    }
    fn extend_right(&mut self) {
        self.move_right_selecting();
    }
    fn extend_up(&mut self) {
        self.move_up_selecting();
    }
    fn extend_down(&mut self) {
        self.move_down_selecting();
    }
    fn extend_home(&mut self) {
        self.move_home_selecting();
    }
    fn extend_end(&mut self) {
        self.move_end_selecting();
    }
    fn extend_word_left(&mut self) {
        self.move_word_left_selecting();
    }
    fn extend_word_right(&mut self) {
        self.move_word_right_selecting();
    }
}

impl TextSurface for crate::view::controls::TextInputState {
    fn insert_text(&mut self, text: &str) {
        self.insert_str(text);
    }
    fn backspace(&mut self) {
        self.backspace();
    }
    fn delete_forward(&mut self) {
        self.delete();
    }
    fn delete_word_backward(&mut self) {
        self.delete_word_backward();
    }
    fn delete_word_forward(&mut self) {
        self.delete_word_forward();
    }
    fn move_left(&mut self) {
        self.move_left();
    }
    fn move_right(&mut self) {
        self.move_right();
    }
    fn move_up(&mut self) {
        self.move_up();
    }
    fn move_down(&mut self) {
        self.move_down();
    }
    fn move_home(&mut self) {
        self.move_home();
    }
    fn move_end(&mut self) {
        self.move_end();
    }
    fn move_word_left(&mut self) {
        self.move_word_left();
    }
    fn move_word_right(&mut self) {
        self.move_word_right();
    }
    fn extend_left(&mut self) {
        self.move_left_selecting();
    }
    fn extend_right(&mut self) {
        self.move_right_selecting();
    }
    fn extend_up(&mut self) {
        self.move_up_selecting();
    }
    fn extend_down(&mut self) {
        self.move_down_selecting();
    }
    fn extend_home(&mut self) {
        self.move_home_selecting();
    }
    fn extend_end(&mut self) {
        self.move_end_selecting();
    }
    fn extend_word_left(&mut self) {
        self.move_word_left_selecting();
    }
    fn extend_word_right(&mut self) {
        self.move_word_right_selecting();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::primitives::text_edit::TextEdit;

    fn ev(code: KeyCode, mods: KeyModifiers) -> KeyEvent {
        KeyEvent::new(code, mods)
    }

    fn plain(code: KeyCode) -> KeyEvent {
        ev(code, KeyModifiers::NONE)
    }

    /// Drive a single-line `TextEdit` through the router and read back its
    /// value + flat cursor, the two things a user actually sees.
    fn single_line(text: &str) -> TextEdit {
        let mut e = TextEdit::single_line_with_text(text);
        e.move_end();
        e
    }

    #[test]
    fn home_moves_caret_to_start() {
        let mut e = single_line("bash");
        assert_eq!(
            apply_text_key(&mut e, &plain(KeyCode::Home), TextKeyContext::single_line()),
            TextKeyResult::Handled
        );
        apply_text_key(
            &mut e,
            &plain(KeyCode::Char('X')),
            TextKeyContext::single_line(),
        );
        assert_eq!(e.value(), "Xbash");
    }

    #[test]
    fn end_moves_caret_to_end() {
        let mut e = single_line("bash");
        apply_text_key(&mut e, &plain(KeyCode::Home), TextKeyContext::single_line());
        apply_text_key(&mut e, &plain(KeyCode::End), TextKeyContext::single_line());
        apply_text_key(
            &mut e,
            &plain(KeyCode::Char('Z')),
            TextKeyContext::single_line(),
        );
        assert_eq!(e.value(), "bashZ");
    }

    #[test]
    fn delete_forward_removes_char_at_caret() {
        let mut e = single_line("abcde");
        apply_text_key(&mut e, &plain(KeyCode::Home), TextKeyContext::single_line());
        assert_eq!(
            apply_text_key(
                &mut e,
                &plain(KeyCode::Delete),
                TextKeyContext::single_line()
            ),
            TextKeyResult::Handled
        );
        assert_eq!(e.value(), "bcde");
    }

    #[test]
    fn backspace_removes_char_before_caret() {
        let mut e = single_line("abc");
        apply_text_key(
            &mut e,
            &plain(KeyCode::Backspace),
            TextKeyContext::single_line(),
        );
        assert_eq!(e.value(), "ab");
    }

    #[test]
    fn shift_arrows_and_home_end_select_then_replace() {
        // Select "de" with Shift+Left twice, then typing replaces it.
        let mut e = single_line("abcde");
        apply_text_key(
            &mut e,
            &ev(KeyCode::Left, KeyModifiers::SHIFT),
            TextKeyContext::single_line(),
        );
        apply_text_key(
            &mut e,
            &ev(KeyCode::Left, KeyModifiers::SHIFT),
            TextKeyContext::single_line(),
        );
        apply_text_key(
            &mut e,
            &plain(KeyCode::Char('Q')),
            TextKeyContext::single_line(),
        );
        assert_eq!(e.value(), "abcQ");

        // Shift+Home selects to start; Backspace deletes the selection.
        let mut e = single_line("hello");
        apply_text_key(
            &mut e,
            &ev(KeyCode::Home, KeyModifiers::SHIFT),
            TextKeyContext::single_line(),
        );
        apply_text_key(
            &mut e,
            &plain(KeyCode::Backspace),
            TextKeyContext::single_line(),
        );
        assert_eq!(e.value(), "");
    }

    #[test]
    fn ctrl_arrows_move_by_word() {
        let mut e = single_line("foo bar baz");
        apply_text_key(
            &mut e,
            &ev(KeyCode::Left, KeyModifiers::CONTROL),
            TextKeyContext::single_line(),
        );
        apply_text_key(
            &mut e,
            &plain(KeyCode::Char('|')),
            TextKeyContext::single_line(),
        );
        assert_eq!(e.value(), "foo bar |baz");
    }

    #[test]
    fn ctrl_backspace_deletes_word() {
        let mut e = single_line("foo bar");
        apply_text_key(
            &mut e,
            &ev(KeyCode::Backspace, KeyModifiers::CONTROL),
            TextKeyContext::single_line(),
        );
        assert_eq!(e.value(), "foo ");
    }

    #[test]
    fn up_down_ignored_on_single_line_but_handled_on_multiline() {
        let mut e = single_line("abc");
        assert_eq!(
            apply_text_key(&mut e, &plain(KeyCode::Up), TextKeyContext::single_line()),
            TextKeyResult::Ignored
        );

        let mut m = TextEdit::with_text("line1\nline2");
        m.move_end(); // end of line2
        assert_eq!(
            apply_text_key(&mut m, &plain(KeyCode::Up), TextKeyContext::multiline(true)),
            TextKeyResult::Handled
        );
        apply_text_key(
            &mut m,
            &plain(KeyCode::Char('X')),
            TextKeyContext::multiline(true),
        );
        assert_eq!(m.value(), "line1X\nline2");
    }

    #[test]
    fn ctrl_and_alt_chars_are_ignored_for_clipboard_and_chords() {
        let mut e = single_line("hi");
        assert_eq!(
            apply_text_key(
                &mut e,
                &ev(KeyCode::Char('a'), KeyModifiers::CONTROL),
                TextKeyContext::single_line()
            ),
            TextKeyResult::Ignored
        );
        assert_eq!(
            apply_text_key(
                &mut e,
                &ev(KeyCode::Char('v'), KeyModifiers::ALT),
                TextKeyContext::single_line()
            ),
            TextKeyResult::Ignored
        );
        // Value untouched — neither chord typed a letter.
        assert_eq!(e.value(), "hi");
    }

    #[test]
    fn enter_tab_esc_are_ignored_as_chrome() {
        let mut e = single_line("hi");
        for code in [KeyCode::Enter, KeyCode::Tab, KeyCode::Esc] {
            assert_eq!(
                apply_text_key(&mut e, &plain(code), TextKeyContext::single_line()),
                TextKeyResult::Ignored
            );
        }
        assert_eq!(e.value(), "hi");
    }
}
