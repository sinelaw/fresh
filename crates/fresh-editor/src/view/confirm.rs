//! What a confirmation *asks*, separated from where it is drawn.
//!
//! **A question the user cannot see is a question they answer by accident.**
//! Every destructive fork in the editor — quitting with unsaved work, closing
//! a modified buffer, deleting a file, overwriting one that changed on disk —
//! used to be one line of grey text on the terminal's last row, ending in a
//! run of parenthesised letters (`(s)ave, (d)iscard, (C)ancel?`). On a tall
//! terminal that row is forty lines below where the user is looking, it is
//! the same row a transient status message uses, and the letters are the only
//! statement of what the possible answers even are. People reported the quit
//! prompt as a hang: the editor had asked, and nothing on screen said so.
//!
//! So a confirmation is now a modal dialog over a dimmed frame, and this is
//! the half of it that has no geometry: the question, the choices, and which
//! one is in hand. [`crate::view::shell::confirm`] draws it and
//! `Editor::handle_confirm_dialog_key` drives it.
//!
//! **The choices carry the old single-letter answers as data.** Each
//! [`Choice`] owns the exact string the prompt's existing
//! `Editor::confirm_prompt` arm compares against, so activating a button is
//! the same event as typing that letter and pressing Enter always was —
//! every handler in `app::prompt_actions` is untouched, and the letters keep
//! working as accelerators for anyone with the muscle memory.

/// What a choice costs if it is the wrong one.
///
/// Not decoration: it is the only thing that distinguishes "Discard" from
/// "Save" at a glance, and the default selection is deliberately never a
/// [`Tone::Destructive`] one.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Tone {
    /// Reversible, or the thing the user almost certainly meant.
    Safe,
    /// Work is lost, a file is replaced, or something is deleted.
    Destructive,
}

/// One button on the dialog.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Choice {
    /// What the button says, already localized and spelled in full —
    /// "Discard and Quit", not "(d)iscard".
    pub label: String,
    /// The accelerator key, which is also the letter the bottom-row prompt
    /// used to ask for. `None` for a choice with no letter of its own.
    ///
    /// Case is significant: the multi-file paste conflict distinguishes `o`
    /// (this one) from `O` (all of them), and that is the one prompt in the
    /// editor that ever did.
    pub mnemonic: Option<char>,
    /// The string handed to `Editor::confirm_prompt` when this choice wins.
    /// This is what keeps every existing confirm handler working unchanged.
    pub input: String,
    pub tone: Tone,
}

impl Choice {
    /// A choice whose accelerator is the first character of its `input`.
    pub fn new(label: impl Into<String>, input: impl Into<String>, tone: Tone) -> Self {
        let input: String = input.into();
        Choice {
            label: label.into(),
            mnemonic: input.chars().next(),
            input,
            tone,
        }
    }

    /// A choice whose accelerator is not its input's first character.
    pub fn with_mnemonic(
        label: impl Into<String>,
        mnemonic: char,
        input: impl Into<String>,
        tone: Tone,
    ) -> Self {
        Choice {
            label: label.into(),
            mnemonic: Some(mnemonic),
            input: input.into(),
            tone,
        }
    }

    /// Where the accelerator letter appears in the label, as a byte range, so
    /// the renderer can mark it. `None` when the letter is not in the label at
    /// all — a localized label need not contain the English key it answers to,
    /// and an accelerator that is absent from the text is still an accelerator.
    pub fn mnemonic_span(&self) -> Option<(usize, usize)> {
        let m = self.mnemonic?;
        self.label
            .char_indices()
            .find(|(_, c)| c.eq_ignore_ascii_case(&m))
            .map(|(i, c)| (i, i + c.len_utf8()))
    }
}

/// A confirmation, as the dialog states it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Confirm {
    /// The dialog's caption — "Unsaved Changes", "Delete File".
    pub title: String,
    /// The question, in full sentences. Wrapped by the tree, so this is
    /// written as prose and not pre-broken into lines.
    pub body: String,
    /// A second, quieter line under the question: the path at risk, the list
    /// of files, the workspaces the unsaved work is in. Empty for none.
    pub detail: String,
    pub choices: Vec<Choice>,
    /// Which button has the keyboard.
    pub selected: usize,
    /// Which choice Esc, and a click on the scrim, resolve to. `None` means
    /// Esc cancels the prompt outright (`Editor::cancel_prompt`), which is
    /// what every one of these prompts did before: cancelling fed the empty
    /// string, and every handler's `else` arm read that as "do nothing".
    pub escape: Option<usize>,
}

impl Confirm {
    /// A confirmation whose first choice is in hand and whose last choice is
    /// what Esc means.
    ///
    /// **The last choice is the safe one, by construction.** Every call site
    /// lists the outcomes in order of consequence and puts the retreat at the
    /// end, so "Esc is the last button" is a rule the dialog can state once
    /// rather than each prompt repeating it.
    pub fn new(title: impl Into<String>, body: impl Into<String>, choices: Vec<Choice>) -> Self {
        let escape = choices.len().checked_sub(1);
        Confirm {
            title: title.into(),
            body: body.into(),
            detail: String::new(),
            choices,
            selected: 0,
            escape,
        }
    }

    /// The quieter second line — a path, a file list, a workspace name.
    pub fn detail(mut self, detail: impl Into<String>) -> Self {
        self.detail = detail.into();
        self
    }

    /// Open with a different button in hand.
    ///
    /// Used where the first-listed outcome is not the likely one: the
    /// large-file encoding prompt leads with "Load", which is also its
    /// default, while the delete prompts lead with "Delete" and open on
    /// "Cancel" instead — a modal that appears under the pointer must never
    /// have a destructive button armed.
    pub fn selecting(mut self, selected: usize) -> Self {
        self.selected = selected.min(self.choices.len().saturating_sub(1));
        self
    }

    /// The choice in hand, if the dialog has any at all.
    pub fn current(&self) -> Option<&Choice> {
        self.choices.get(self.selected)
    }

    /// Move the selection one button right, wrapping.
    pub fn select_next(&mut self) {
        if self.choices.is_empty() {
            return;
        }
        self.selected = (self.selected + 1) % self.choices.len();
    }

    /// Move the selection one button left, wrapping.
    pub fn select_prev(&mut self) {
        if self.choices.is_empty() {
            return;
        }
        self.selected = (self.selected + self.choices.len() - 1) % self.choices.len();
    }

    /// The choice a typed character activates.
    ///
    /// **Exact case first, then insensitively.** `o` and `O` are different
    /// answers in the multi-file paste conflict and the same answer
    /// everywhere else; trying the exact match before the loose one gets both
    /// right without either prompt knowing about the other. The loose pass is
    /// skipped when it would be ambiguous, so a stray Shift can never pick
    /// the wrong one of a case-distinguishing pair.
    pub fn by_mnemonic(&self, c: char) -> Option<usize> {
        if let Some(i) = self.choices.iter().position(|ch| ch.mnemonic == Some(c)) {
            return Some(i);
        }
        let mut loose = self
            .choices
            .iter()
            .enumerate()
            .filter(|(_, ch)| ch.mnemonic.is_some_and(|m| m.eq_ignore_ascii_case(&c)));
        let first = loose.next()?;
        match loose.next() {
            Some(_) => None,
            None => Some(first.0),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn abc() -> Confirm {
        Confirm::new(
            "T",
            "B",
            vec![
                Choice::new("Save", "s", Tone::Safe),
                Choice::new("Discard", "d", Tone::Destructive),
                Choice::new("Cancel", "C", Tone::Safe),
            ],
        )
    }

    #[test]
    fn esc_is_the_last_choice_by_default() {
        assert_eq!(abc().escape, Some(2));
    }

    #[test]
    fn selection_wraps_in_both_directions() {
        let mut c = abc();
        c.select_prev();
        assert_eq!(c.selected, 2);
        c.select_next();
        assert_eq!(c.selected, 0);
    }

    #[test]
    fn a_mnemonic_matches_regardless_of_case_when_unambiguous() {
        let c = abc();
        assert_eq!(c.by_mnemonic('s'), Some(0));
        assert_eq!(c.by_mnemonic('S'), Some(0));
        assert_eq!(c.by_mnemonic('c'), Some(2));
        assert_eq!(c.by_mnemonic('C'), Some(2));
        assert_eq!(c.by_mnemonic('z'), None);
    }

    #[test]
    fn case_distinguishing_choices_keep_their_own_letters() {
        // The multi-file paste conflict: `o` is this file, `O` is all of them.
        let c = Confirm::new(
            "T",
            "B",
            vec![
                Choice::new("Overwrite", "o", Tone::Destructive),
                Choice::new("Overwrite All", "O", Tone::Destructive),
                Choice::new("Skip", "s", Tone::Safe),
                Choice::new("Skip All", "S", Tone::Safe),
                Choice::new("Cancel", "c", Tone::Safe),
            ],
        );
        assert_eq!(c.by_mnemonic('o'), Some(0));
        assert_eq!(c.by_mnemonic('O'), Some(1));
        assert_eq!(c.by_mnemonic('s'), Some(2));
        assert_eq!(c.by_mnemonic('S'), Some(3));
    }

    #[test]
    fn the_mnemonic_is_found_in_the_label_when_it_is_there() {
        let c = Choice::new("Discard", "d", Tone::Destructive);
        assert_eq!(c.mnemonic_span(), Some((0, 1)));
        // "Cancel" answers to `C`, which is its first letter.
        let c = Choice::new("Cancel", "C", Tone::Safe);
        assert_eq!(c.mnemonic_span(), Some((0, 1)));
        // A localized label need not contain the letter at all.
        let c = Choice::with_mnemonic("Отмена", 'C', "C", Tone::Safe);
        assert_eq!(c.mnemonic_span(), None);
    }

    #[test]
    fn a_destructive_lead_can_open_on_the_retreat() {
        let c = abc().selecting(2);
        assert_eq!(c.current().map(|c| c.label.as_str()), Some("Cancel"));
    }
}
