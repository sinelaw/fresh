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
    /// Further letters that also activate this choice, but are not the one
    /// marked in the label.
    ///
    /// The row prompts sometimes advertised two spellings of one answer —
    /// `prompt.quit_confirm` said `(y)es` while its handler equally accepts
    /// the `Action::Quit` letter — and a modal that swallows every key would
    /// turn the unlisted one into a dead key.
    pub aliases: Vec<char>,
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
            aliases: Vec::new(),
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
            aliases: Vec::new(),
            tone,
        }
    }

    /// Another letter that activates this choice without being marked in the
    /// label. See [`Choice::aliases`].
    pub fn also(mut self, c: char) -> Self {
        self.aliases.push(c);
        self
    }

    /// Where the accelerator letter appears in the label, as a byte range, so
    /// the renderer can mark it. `None` when the letter is not in the label at
    /// all — a localized label need not contain the English key it answers to,
    /// and an accelerator that is absent from the text is still an accelerator.
    ///
    /// `exact` forbids the case-insensitive match. Use
    /// [`Confirm::mnemonic_span`] rather than this directly: whether the loose
    /// match is safe depends on the *other* choices, which a `Choice` cannot
    /// see.
    fn span(&self, exact: bool) -> Option<(usize, usize)> {
        let m = self.mnemonic?;
        self.label
            .char_indices()
            .find(|(_, c)| match exact {
                true => *c == m,
                false => c.eq_ignore_ascii_case(&m),
            })
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
    /// Which choice Esc resolves to. `None` means Esc cancels the prompt
    /// outright (`Editor::cancel_prompt`), which is what every one of these
    /// prompts did before: cancelling fed the empty string, and every
    /// handler's `else` arm read that as "do nothing".
    ///
    /// A press on the scrim does *not* resolve to it, deliberately: the
    /// layer is `Modality::Exclusive` and swallows outside presses without
    /// dismissing, the way the workspace-trust prompt does. A misaimed click
    /// should not answer a question about unsaved work.
    pub escape: Option<usize>,
}

/// The button a dialog may open with, given the one it asked for.
///
/// **A dialog never opens on an outcome that loses work.** The card appears
/// under the pointer and takes the keyboard, so whatever is armed is one
/// reflexive Enter — or one click of a mouse already moving — away. Callers
/// list their outcomes in order of consequence, which puts the most
/// consequential first, so "the first choice" and "the choice it is safe to
/// arm" are frequently not the same button: `Ctrl+S` onto a file that changed
/// underneath you led with Overwrite, and either paste conflict led with
/// Overwrite over a file the user had not looked at.
///
/// Stated here rather than at each call site because a call site that forgets
/// it is a silently dangerous dialog, and there is no version of this rule
/// that some future prompt should be allowed to opt out of.
fn armable(choices: &[Choice], want: usize, escape: Option<usize>) -> usize {
    let destructive = |i: usize| choices.get(i).is_some_and(|c| c.tone == Tone::Destructive);
    if !destructive(want) {
        return want;
    }
    // The retreat, when there is one and it is itself safe to arm.
    match escape.filter(|e| !destructive(*e)) {
        Some(e) => e,
        // Otherwise the last outcome that costs nothing; a dialog whose every
        // button is destructive has nothing safer to offer and keeps its lead.
        None => choices
            .iter()
            .rposition(|c| c.tone != Tone::Destructive)
            .unwrap_or(want),
    }
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
        let selected = armable(&choices, 0, escape);
        Confirm {
            title: title.into(),
            body: body.into(),
            detail: String::new(),
            choices,
            selected,
            escape,
        }
    }

    /// The quieter second line — a path, a file list, a workspace name.
    pub fn detail(mut self, detail: impl Into<String>) -> Self {
        self.detail = detail.into();
        self
    }

    /// What Esc means, when the last choice is not it.
    ///
    /// `None` makes Esc dismiss the prompt without answering — for a question
    /// whose every button *does* something, like the large-file scan prompt,
    /// where declining the scan still opens the byte-offset prompt.
    pub fn escaping(mut self, escape: Option<usize>) -> Self {
        self.escape = escape;
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
        let want = selected.min(self.choices.len().saturating_sub(1));
        self.selected = armable(&self.choices, want, self.escape);
        self
    }

    /// Where to mark choice `i`'s accelerator inside its label.
    ///
    /// **The mark has to agree with the keyboard.** `by_mnemonic` is
    /// case-aware, because Czech ships `discard = "z"` beside `cancel = "Z"`
    /// (Russian the same with `о`/`О`); a loose match when drawing would put
    /// an underline under the capital `Z` of *both* "Zahodit a ukončit" and
    /// "Zrušit", advertising one letter for two outcomes when only one of
    /// them is what typing it does. So when another choice answers to the
    /// same letter in the other case, the mark is exact or there is no mark.
    /// Everywhere else — which is every locale but those two, and every
    /// dialog in them whose letters do not collide — "Discard" still
    /// underlines its `D` for the accelerator `d`.
    pub fn mnemonic_span(&self, i: usize) -> Option<(usize, usize)> {
        let choice = self.choices.get(i)?;
        let m = choice.mnemonic?;
        let collides = self.choices.iter().enumerate().any(|(j, other)| {
            j != i
                && other
                    .mnemonic
                    .is_some_and(|o| o != m && o.eq_ignore_ascii_case(&m))
        });
        choice.span(collides)
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
        // An unmarked second spelling of an answer the row prompt advertised.
        // Exact, and after the marked letters, so it can never take a key one
        // of those owns.
        if let Some(i) = self.choices.iter().position(|ch| ch.aliases.contains(&c)) {
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
        // `d` marks the `D` of "Discard": the loose match is what makes the
        // underline work for an ordinary capitalised label.
        assert_eq!(abc().mnemonic_span(1), Some((0, 1)));
        assert_eq!(abc().mnemonic_span(2), Some((0, 1)));
        // A localized label need not contain the letter at all.
        let c = Confirm::new(
            "T",
            "B",
            vec![Choice::with_mnemonic("Отмена", 'C', "", Tone::Safe)],
        );
        assert_eq!(c.mnemonic_span(0), None);
    }

    /// The mark has to agree with the keyboard. Czech ships `discard = "z"`
    /// beside `cancel = "Z"`, and a loose match when drawing would underline
    /// the capital `Z` of both labels — advertising one letter for two
    /// outcomes when only one of them is what typing it does.
    #[test]
    fn a_colliding_pair_is_marked_exactly_or_not_at_all() {
        let cs = Confirm::new(
            "Neuložené změny",
            "B",
            vec![
                Choice::with_mnemonic("Uložit a ukončit", 'u', "u", Tone::Safe),
                Choice::with_mnemonic("Zahodit a ukončit", 'z', "z", Tone::Destructive),
                Choice::with_mnemonic("Zrušit", 'Z', "", Tone::Safe),
            ],
        );
        // `Z` is what typing `Z` does, so `Z` is marked on "Zrušit" only.
        assert_eq!(cs.mnemonic_span(2), Some((0, 1)));
        assert_eq!(
            cs.mnemonic_span(1),
            None,
            "the discard button must not advertise a letter that cancels"
        );
        // The non-colliding letter is unaffected.
        assert_eq!(cs.mnemonic_span(0), Some((0, 1)));
        // And the keyboard still tells them apart.
        assert_eq!(cs.by_mnemonic('z'), Some(1));
        assert_eq!(cs.by_mnemonic('Z'), Some(2));
    }

    /// A second spelling the row prompt advertised stays live inside a modal
    /// that swallows every key.
    #[test]
    fn an_alias_activates_its_choice_without_being_marked() {
        let c = Confirm::new(
            "Quit Fresh",
            "B",
            vec![
                Choice::new("Quit", "q", Tone::Safe).also('y'),
                Choice::with_mnemonic("Cancel", 'n', "", Tone::Safe),
            ],
        );
        assert_eq!(c.by_mnemonic('q'), Some(0));
        assert_eq!(c.by_mnemonic('y'), Some(0), "`y` was the advertised key");
        assert_eq!(c.by_mnemonic('n'), Some(1));
        // The alias is not what the label marks — `Quit` marks its `Q`.
        assert_eq!(c.mnemonic_span(0), Some((0, 1)));
    }

    /// Esc dismisses rather than answering when every button does something.
    #[test]
    fn a_dialog_with_no_retreat_has_no_escape_choice() {
        let c = Confirm::new(
            "Go to Line",
            "B",
            vec![
                Choice::new("Scan", "y", Tone::Safe),
                Choice::new("Go to Byte Offset", "n", Tone::Safe),
            ],
        )
        .escaping(None);
        assert_eq!(c.escape, None);
    }

    #[test]
    fn a_destructive_lead_can_open_on_the_retreat() {
        let c = abc().selecting(2);
        assert_eq!(c.current().map(|c| c.label.as_str()), Some("Cancel"));
    }

    /// A dialog whose most consequential outcome is listed first must not
    /// open on it. `Ctrl+S` onto a file that changed underneath you, and
    /// either paste conflict, all led with Overwrite.
    #[test]
    fn a_dialog_never_opens_on_a_destructive_button() {
        let c = Confirm::new(
            "File Changed on Disk",
            "B",
            vec![
                Choice::new("Overwrite", "o", Tone::Destructive),
                Choice::new("Cancel", "C", Tone::Safe),
            ],
        );
        assert_eq!(c.current().map(|c| c.label.as_str()), Some("Cancel"));
        // Asking for it explicitly does not get around the rule either.
        let c = c.selecting(0);
        assert_eq!(c.current().map(|c| c.label.as_str()), Some("Cancel"));
    }

    /// The retreat is not always last, and not always safe to arm — the rule
    /// falls back to the last outcome that costs nothing.
    #[test]
    fn the_fallback_is_the_last_harmless_outcome() {
        let c = Confirm::new(
            "Name Conflict",
            "B",
            vec![
                Choice::new("Overwrite", "o", Tone::Destructive),
                Choice::new("Skip", "s", Tone::Safe),
                Choice::new("Overwrite All", "O", Tone::Destructive),
            ],
        );
        assert_eq!(c.current().map(|c| c.label.as_str()), Some("Skip"));
    }

    /// A dialog with nothing safe to offer keeps its lead rather than
    /// silently arming some other destructive button.
    #[test]
    fn a_dialog_of_only_destructive_outcomes_keeps_its_lead() {
        let c = Confirm::new(
            "T",
            "B",
            vec![
                Choice::new("Delete", "d", Tone::Destructive),
                Choice::new("Delete All", "D", Tone::Destructive),
            ],
        );
        assert_eq!(c.selected, 0);
    }

    /// The safe outcomes still open where they were asked to.
    #[test]
    fn a_harmless_lead_is_left_alone() {
        let c = Confirm::new(
            "Large File",
            "B",
            vec![
                Choice::new("Load", "L", Tone::Safe),
                Choice::new("Choose Encoding…", "e", Tone::Safe),
                Choice::new("Cancel", "c", Tone::Safe),
            ],
        );
        assert_eq!(c.current().map(|c| c.label.as_str()), Some("Load"));
    }
}
