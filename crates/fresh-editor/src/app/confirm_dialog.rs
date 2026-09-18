//! Driving the confirmation modal: its keyboard, and what a button press does.
//!
//! **Answering is still `confirm_prompt`.** The dialog does not know what any
//! of its choices mean. Activating one writes that choice's `input` into the
//! prompt's query and runs `Action::PromptConfirm`, which is bit for bit what
//! typing the old single letter and pressing Enter produced — so every arm of
//! `prompt_actions::handle_prompt_confirm_input` is reached unchanged, and a
//! prompt that has not been converted to a dialog yet still works the way it
//! always did. The dialog is a way of *asking*; it is not a second way of
//! answering.
//!
//! See [`crate::view::confirm`] for why the question moved off the bottom row.

use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use fresh_i18n::t;

use super::Editor;
use crate::view::confirm::{Choice, Confirm, Tone};

/// The retreat, spelled the same way everywhere.
///
/// **Its answer is the empty string.** Every confirm handler reads an answer
/// it does not recognise as "do nothing", which is exactly what Esc fed them
/// before, so the empty string is the one answer that cannot be mistaken for
/// an outcome in any locale. A letter cannot say that: the handlers lower-case
/// what they compare, and Czech ships `discard = "z"` beside `cancel = "Z"`
/// (Russian the same with `о`/`О`) — a Cancel that sent a letter would
/// *discard the user's work* there.
///
/// Its accelerator is the `C` of its own label, assigned with every other
/// button's and reserved ahead of them, so Cancel is the same key in every
/// dialog that has one.
///
/// The one prompt that cannot take this is the large-file encoding
/// confirmation, whose handler reads an empty answer as "load anyway"; it
/// names its own cancel answer.
pub fn cancel() -> Choice {
    Choice::new(t!("dialog.btn.cancel").into_owned(), "", Tone::Safe)
}

/// The single-file paste conflict, asked from three places: the explorer's
/// paste, and twice more while walking a queue of them.
pub fn paste_conflict(name: &str) -> Confirm {
    Confirm::new(
        t!("dialog.title.name_conflict").into_owned(),
        t!("explorer.paste_conflict", name = name).into_owned(),
        vec![
            Choice::new(
                t!("dialog.btn.overwrite").into_owned(),
                "o",
                Tone::Destructive,
            ),
            Choice::new(t!("dialog.btn.rename").into_owned(), "r", Tone::Safe),
            cancel(),
        ],
    )
}

/// One conflict out of a multi-file paste. `o`/`O` and `s`/`S` differ only in
/// case — the one place in the editor where that is true, and why
/// [`Confirm::by_mnemonic`] tries an exact match before a loose one.
pub fn multi_paste_conflict(name: &str) -> Confirm {
    Confirm::new(
        t!("dialog.title.name_conflict").into_owned(),
        t!("explorer.paste_conflict_multi", name = name).into_owned(),
        vec![
            Choice::new(
                t!("dialog.btn.overwrite").into_owned(),
                "o",
                Tone::Destructive,
            ),
            Choice::new(
                t!("dialog.btn.overwrite_all").into_owned(),
                "O",
                Tone::Destructive,
            ),
            Choice::new(t!("dialog.btn.skip").into_owned(), "s", Tone::Safe),
            Choice::new(t!("dialog.btn.skip_all").into_owned(), "S", Tone::Safe),
            cancel(),
        ],
    )
}

/// Loading a large file in an encoding that cannot be resynchronised, so the
/// whole of it has to come into memory. Asked from four places — every one of
/// them a different route into `open_file` — so the question is built once.
pub fn large_file_encoding(encoding: &str, size_bytes: usize, path: &std::path::Path) -> Confirm {
    let size_mb = size_bytes as f64 / (1024.0 * 1024.0);
    Confirm::new(
        t!("dialog.title.large_file").into_owned(),
        t!(
            "file.large_encoding_prompt",
            encoding = encoding,
            size = format!("{:.0}", size_mb)
        )
        .into_owned(),
        vec![
            Choice::new(
                t!("dialog.btn.load").into_owned(),
                t!("file.large_encoding.key.load").into_owned(),
                Tone::Safe,
            ),
            Choice::new(
                t!("dialog.btn.choose_encoding").into_owned(),
                t!("file.large_encoding.key.encoding").into_owned(),
                Tone::Safe,
            ),
            // The exception to `cancel()`: this handler reads an empty
            // answer as "load anyway", so the retreat has to name itself.
            Choice::new(
                t!("dialog.btn.cancel").into_owned(),
                t!("file.large_encoding.key.cancel").into_owned(),
                Tone::Safe,
            ),
        ],
    )
    .detail(path.display().to_string())
}

/// Saving into a folder that is not there yet.
pub fn create_directory(dir_name: &str) -> Confirm {
    Confirm::new(
        t!("dialog.title.folder_missing").into_owned(),
        t!("buffer.create_directory_confirm", name = dir_name).into_owned(),
        vec![
            Choice::new(t!("dialog.btn.create").into_owned(), "c", Tone::Safe),
            cancel(),
        ],
    )
    // The row prompt spelled this `(c)reate, (A)bort?` — the capital was the
    // default, and Enter on an empty line aborted. Keep that: the dialog is a
    // change to how the question is *asked*, not to what Enter answers.
    .selecting(1)
}

/// Deleting, from the file explorer. **Opens on Cancel**: the dialog can
/// appear right under the pointer, and an armed `Delete` one stray Enter away
/// is how a modal turns into the accident it exists to prevent.
pub fn delete(body: String) -> Confirm {
    Confirm::new(
        t!("dialog.title.delete").into_owned(),
        body,
        vec![
            Choice::new(t!("dialog.btn.delete").into_owned(), "y", Tone::Destructive),
            cancel(),
        ],
    )
    .selecting(1)
}

impl Editor {
    /// Ask whether to scan a large file for exact line numbers.
    ///
    /// Two callers — `Action::GotoLine` on a buffer with no line index, and
    /// the same question re-asked after a Quick Open `:line` jump — so the
    /// question is built once rather than drifting apart in two places.
    pub fn start_goto_line_scan_confirm(&mut self) {
        let body = t!("goto.scan_confirm_prompt").to_string();
        let confirm = Confirm::new(
            t!("dialog.title.go_to_line").into_owned(),
            body.clone(),
            vec![
                Choice::new(t!("dialog.btn.scan").into_owned(), "y", Tone::Safe),
                // Not a cancel: declining the scan still goes somewhere —
                // the byte-offset prompt, which needs no index.
                Choice::new(t!("dialog.btn.byte_offset").into_owned(), "n", Tone::Safe),
            ],
        )
        // **Neither button is the retreat here.** Both answers go somewhere —
        // one scans, the other opens the byte-offset prompt — so Esc dismisses
        // the question without answering it, which is what Esc did when this
        // was a row prompt.
        .escaping(None);
        self.start_confirm_prompt(
            body,
            crate::view::prompt::PromptType::GotoLineScanConfirm,
            confirm,
        );
    }

    /// The active prompt's confirmation, when it has one.
    fn active_confirm(&self) -> Option<&crate::view::confirm::Confirm> {
        self.active_window().prompt.as_ref()?.confirm.as_ref()
    }

    /// Keys for the confirmation modal.
    ///
    /// The layer is `Modality::Exclusive` and `modal::keys` claims whatever
    /// its interior declines, so this sees every keystroke while the dialog
    /// is up and nothing falls through to the buffer behind it. That is the
    /// point of a modal, and it is also why Esc has to be handled here rather
    /// than left to the prompt's `PromptCancel` binding.
    pub fn handle_confirm_dialog_key(&mut self, event: &KeyEvent) {
        if self.active_confirm().is_none() {
            return;
        }
        // Ctrl/Alt combinations are nobody's accelerator here. Swallowed
        // rather than passed on: a modal that lets Ctrl+W close the window
        // behind it is not modal.
        let plain = !event
            .modifiers
            .intersects(KeyModifiers::CONTROL | KeyModifiers::ALT);
        match event.code {
            KeyCode::Left | KeyCode::Up | KeyCode::BackTab => self.confirm_dialog_move(false),
            KeyCode::Right | KeyCode::Down => self.confirm_dialog_move(true),
            KeyCode::Tab => match event.modifiers.contains(KeyModifiers::SHIFT) {
                true => self.confirm_dialog_move(false),
                false => self.confirm_dialog_move(true),
            },
            KeyCode::Home => self.confirm_dialog_select(0),
            KeyCode::End => {
                let last = self
                    .active_confirm()
                    .map(|c| c.choices.len().saturating_sub(1))
                    .unwrap_or(0);
                self.confirm_dialog_select(last);
            }
            KeyCode::Enter | KeyCode::Char(' ') => {
                let i = self.active_confirm().map(|c| c.selected).unwrap_or(0);
                self.confirm_dialog_choose(i);
            }
            KeyCode::Esc => self.confirm_dialog_escape(),
            KeyCode::Char(c) if plain => {
                // The old single-letter answer, still an answer — and still
                // the *whole* answer: it acts immediately rather than moving
                // the selection, because that is what it did on the row.
                if let Some(i) = self.active_confirm().and_then(|d| d.by_mnemonic(c)) {
                    self.confirm_dialog_choose(i);
                }
            }
            _ => {}
        }
    }

    /// Light the button under the pointer, or clear the light.
    ///
    /// **Hover is not a selection.** It says where a click would land; Enter
    /// still takes the armed button. A pointer crossing "Delete" on its way
    /// somewhere else must not leave it one keystroke from happening.
    pub fn confirm_dialog_hover(&mut self, i: Option<usize>) {
        if let Some(p) = self.active_window_mut().prompt.as_mut() {
            if let Some(c) = p.confirm.as_mut() {
                // A Leave from one button and an Enter into the next arrive in
                // that order, so a stale `None` must not erase the new one.
                if i.is_some() || c.hovered.is_some() {
                    c.hovered = i;
                }
            }
        }
    }

    fn confirm_dialog_move(&mut self, forward: bool) {
        if let Some(p) = self.active_window_mut().prompt.as_mut() {
            if let Some(c) = p.confirm.as_mut() {
                match forward {
                    true => c.select_next(),
                    false => c.select_prev(),
                }
            }
        }
    }

    fn confirm_dialog_select(&mut self, i: usize) {
        if let Some(p) = self.active_window_mut().prompt.as_mut() {
            if let Some(c) = p.confirm.as_mut() {
                if i < c.choices.len() {
                    c.selected = i;
                }
            }
        }
    }

    /// Esc: the choice the dialog nominates as its retreat, or — when it
    /// nominates none — cancelling the prompt outright, which is what every
    /// one of these prompts did on Esc before.
    fn confirm_dialog_escape(&mut self) {
        match self.active_confirm().and_then(|c| c.escape) {
            Some(i) => self.confirm_dialog_choose(i),
            None => self.cancel_prompt(),
        }
    }

    /// Activate choice `i`: feed its `input` to the prompt and confirm.
    pub fn confirm_dialog_choose(&mut self, i: usize) {
        let Some(input) = self
            .active_confirm()
            .and_then(|c| c.choices.get(i))
            .map(|c| c.input.clone())
        else {
            return;
        };
        if let Some(p) = self.active_window_mut().prompt.as_mut() {
            p.set_input_plain(input);
        }
        #[allow(clippy::let_underscore_must_use)]
        let _ = self.handle_action(crate::input::keybindings::Action::PromptConfirm);
    }
}
