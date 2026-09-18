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

/// Where a choice's accelerator sits in its own label.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Mnemonic {
    /// The letter itself, matched case-insensitively.
    pub ch: char,
    /// Its byte range in the label, so the renderer marks the character the
    /// user is being told to press rather than searching for it again.
    pub at: (usize, usize),
}

/// One button on the dialog.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Choice {
    /// What the button says, already localized and spelled in full —
    /// "Discard and Quit", not "(d)iscard".
    pub label: String,
    /// The accelerator, assigned from this label by [`assign_mnemonics`] when
    /// the dialog is built. Never set by a caller.
    pub mnemonic: Option<Mnemonic>,
    /// The string handed to `Editor::confirm_prompt` when this choice wins.
    /// This is what keeps every existing confirm handler working unchanged.
    ///
    /// **Not a key.** It is the answer this prompt's handler already
    /// understood, and no user ever types it.
    pub input: String,
    pub tone: Tone,
}

impl Choice {
    pub fn new(label: impl Into<String>, input: impl Into<String>, tone: Tone) -> Self {
        Choice {
            label: label.into(),
            mnemonic: None,
            input: input.into(),
            tone,
        }
    }
}

/// The letters a label offers as an accelerator, best first: the initial of
/// each word, then every other character left to right.
///
/// Word initials first is what keeps the second-choice letter legible — a
/// dialog with both "Cancel" and "Create Folder" marks the `F` of Folder
/// rather than the `r` of Create.
fn candidates(label: &str) -> Vec<(usize, char)> {
    let (mut initials, mut rest) = (Vec::new(), Vec::new());
    let mut starting = true;
    for (i, c) in label.char_indices() {
        match c.is_alphanumeric() {
            true => {
                // Only ASCII is offered: a terminal delivers `s` as a bare key
                // press, but `保` or `ก` arrive through an input method, if at
                // all. Marking one would advertise a key nobody can press.
                if c.is_ascii_alphanumeric() {
                    match starting {
                        true => initials.push((i, c)),
                        false => rest.push((i, c)),
                    }
                }
                starting = false;
            }
            false => starting = true,
        }
    }
    initials.extend(rest);
    initials
}

/// Case-insensitive in every alphabet, not just this one.
///
/// `eq_ignore_ascii_case` is plain `==` outside ASCII, which let a dialog hand
/// `Ü` to one button and `ü` to another: German's paste conflict marked
/// **Ü**berschreiben, and the `ü` a keyboard actually produces answered
/// "Alle überschreiben" instead — the visible letter pointing at the *more*
/// destructive button, which is the exact fault this dialog exists to remove.
fn same_letter(a: char, b: char) -> bool {
    a == b || a.to_lowercase().eq(b.to_lowercase())
}

/// Give every choice a letter **of its own label**, unique within the dialog.
///
/// **The accelerator is whatever the button says it is.** The letters used to
/// be inherited from the bottom-row prompts, which chose them to be *typed* —
/// `y` for Delete, `y`/`n` for the scan prompt, `o`/`O` for the two paste
/// overwrites. A letter that is not in the word cannot be marked in it, so
/// four dialogs advertised nothing at all and the multi-file paste conflict
/// marked only its two `All` variants, pointing the one visible `O` at the
/// more destructive button. Deriving the letter from the label instead means
/// a button can always show what it answers to.
///
/// Derived rather than tabulated because the labels are localized: a table
/// would need a letter per label per locale, and any locale it missed would
/// go back to advertising nothing.
///
/// **The retreat picks first.** `Cancel` is `c` in every dialog that has one,
/// so a caller cannot shift it by adding a button that shares its initial —
/// which is exactly what "Create" did to it.
fn assign_mnemonics(choices: &mut [Choice], escape: Option<usize>) {
    let mut taken: Vec<char> = Vec::new();
    for i in escape.into_iter().chain(0..choices.len()) {
        if choices[i].mnemonic.is_some() {
            continue;
        }
        let free = candidates(&choices[i].label)
            .into_iter()
            .find(|(_, c)| !taken.iter().any(|t| same_letter(*t, *c)));
        if let Some((at, ch)) = free {
            taken.push(ch);
            choices[i].mnemonic = Some(Mnemonic {
                ch,
                at: (at, at + ch.len_utf8()),
            });
        }
    }
    number_the_rest(choices, &mut taken, escape);
}

/// Give a button whose label offers no typeable letter a number, and show it.
///
/// A label written in a script the keyboard cannot produce — `保存`, `저장`,
/// `ยกเลิก` — has no letter to mark, and the ASCII letters the bottom-row
/// prompts used are gone with the prompts. Rather than leave those locales
/// with buttons that answer to nothing, the label grows a `(1)` and the digit
/// is the accelerator: still derived, still shown, still typeable.
///
/// **The retreat is numbered first too**, for the same reason it picks its
/// letter first: a Japanese user's `1` should be Cancel on every dialog, not
/// whichever position Cancel happens to occupy.
fn number_the_rest(choices: &mut [Choice], taken: &mut Vec<char>, escape: Option<usize>) {
    for i in escape.into_iter().chain(0..choices.len()) {
        if choices[i].mnemonic.is_some() {
            continue;
        }
        let Some(d) = ('1'..='9').find(|d| !taken.iter().any(|t| same_letter(*t, *d))) else {
            continue;
        };
        taken.push(d);
        let at = choices[i].label.len() + " (".len();
        choices[i].label.push_str(&format!(" ({d})"));
        choices[i].mnemonic = Some(Mnemonic {
            ch: d,
            at: (at, at + d.len_utf8()),
        });
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
    /// Which button the pointer is over, if any.
    ///
    /// **Hover shows, it does not arm.** Moving the pointer across the card
    /// lights the button under it so a click is predictable, but Enter still
    /// takes the armed one — the same separation the workspace-trust prompt
    /// makes between pointing at an option and consenting to it, and what
    /// keeps a pointer that happens to cross "Delete" from putting it one
    /// keystroke away.
    pub hovered: Option<usize>,
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
        let mut choices = choices;
        let escape = choices.len().checked_sub(1);
        assign_mnemonics(&mut choices, escape);
        let selected = armable(&choices, 0, escape);
        Confirm {
            title: title.into(),
            body: body.into(),
            detail: String::new(),
            choices,
            selected,
            hovered: None,
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
    /// Always `Some` for a choice whose label has a letter to spare, because
    /// the accelerator was taken *from* that label — there is nothing to
    /// search for and nothing that can fail to be found.
    pub fn mnemonic_span(&self, i: usize) -> Option<(usize, usize)> {
        Some(self.choices.get(i)?.mnemonic?.at)
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
    /// One pass, case-insensitively: [`assign_mnemonics`] already guaranteed
    /// the letters are distinct that way, so there is no ambiguity left to
    /// resolve and no unmarked spelling to try afterwards. What the button
    /// shows is the whole of what it answers to.
    pub fn by_mnemonic(&self, c: char) -> Option<usize> {
        self.choices
            .iter()
            .position(|ch| ch.mnemonic.is_some_and(|m| same_letter(m.ch, c)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn choice(label: &str, tone: Tone) -> Choice {
        Choice::new(label, "x", tone)
    }

    fn abc() -> Confirm {
        Confirm::new(
            "T",
            "B",
            vec![
                choice("Save", Tone::Safe),
                choice("Discard", Tone::Destructive),
                choice("Cancel", Tone::Safe),
            ],
        )
    }

    /// The letter a choice answers to, as the button shows it.
    fn marked(c: &Confirm) -> Vec<String> {
        c.choices
            .iter()
            .map(|ch| match ch.mnemonic {
                Some(m) => format!("{}[{}]", ch.label, &ch.label[m.at.0..m.at.1]),
                None => format!("{}[-]", ch.label),
            })
            .collect()
    }

    #[test]
    fn esc_is_the_last_choice_by_default() {
        assert_eq!(abc().escape, Some(2));
    }

    #[test]
    fn selection_wraps_in_both_directions() {
        let mut c = abc();
        c.selected = 0;
        c.select_prev();
        assert_eq!(c.selected, 2);
        c.select_next();
        assert_eq!(c.selected, 0);
    }

    /// **Every button shows the letter it answers to.** The letters used to be
    /// inherited from the row prompts — `y` for Delete, `y`/`n` for the scan
    /// prompt — and a letter that is not in the word cannot be marked in it.
    #[test]
    fn every_button_marks_a_letter_of_its_own_label() {
        for c in [
            abc(),
            Confirm::new(
                "T",
                "B",
                vec![
                    choice("Delete", Tone::Destructive),
                    choice("Cancel", Tone::Safe),
                ],
            ),
            Confirm::new(
                "T",
                "B",
                vec![
                    choice("Scan", Tone::Safe),
                    choice("Go to Byte Offset", Tone::Safe),
                ],
            ),
            Confirm::new(
                "T",
                "B",
                vec![
                    choice("Save with sudo", Tone::Safe),
                    choice("Cancel", Tone::Safe),
                ],
            ),
        ] {
            for ch in &c.choices {
                let m = ch.mnemonic.expect("every button has a letter");
                assert_eq!(
                    &ch.label[m.at.0..m.at.1],
                    m.ch.to_string(),
                    "the marked span must be the letter itself, in {:?}",
                    ch.label
                );
            }
        }
    }

    /// One letter, one meaning: `Cancel` is `c` even next to a button whose
    /// own initial is `C`, because the retreat picks before the rest.
    #[test]
    fn cancel_keeps_its_letter_against_a_rival_initial() {
        let c = Confirm::new(
            "Folder Not Found",
            "B",
            vec![
                choice("Create Folder", Tone::Safe),
                choice("Cancel", Tone::Safe),
            ],
        );
        assert_eq!(marked(&c), ["Create Folder[F]", "Cancel[C]"]);

        let c = Confirm::new(
            "Large File",
            "B",
            vec![
                choice("Load", Tone::Safe),
                choice("Choose Encoding…", Tone::Safe),
                choice("Cancel", Tone::Safe),
            ],
        );
        assert_eq!(marked(&c), ["Load[L]", "Choose Encoding…[E]", "Cancel[C]"]);
    }

    /// The five-button paste conflict: the two primary answers used to show
    /// nothing while the marks sat on the `All` variants, pointing the one
    /// visible `O` at the more destructive button.
    #[test]
    fn the_paste_conflict_marks_all_five_distinctly() {
        let c = Confirm::new(
            "Name Conflict",
            "B",
            vec![
                choice("Overwrite", Tone::Destructive),
                choice("Overwrite All", Tone::Destructive),
                choice("Skip", Tone::Safe),
                choice("Skip All", Tone::Safe),
                choice("Cancel", Tone::Safe),
            ],
        );
        assert_eq!(
            marked(&c),
            [
                "Overwrite[O]",
                "Overwrite All[A]",
                "Skip[S]",
                "Skip All[k]",
                "Cancel[C]"
            ]
        );
        // Distinct letters mean the keyboard is unambiguous without any
        // case-sensitivity trick.
        assert_eq!(c.by_mnemonic('o'), Some(0));
        assert_eq!(c.by_mnemonic('a'), Some(1));
        assert_eq!(c.by_mnemonic('s'), Some(2));
        assert_eq!(c.by_mnemonic('k'), Some(3));
        assert_eq!(c.by_mnemonic('c'), Some(4));
    }

    /// The marked letter is what answers, in either case, and nothing else is.
    #[test]
    fn only_the_marked_letters_answer() {
        let c = abc();
        assert_eq!(c.by_mnemonic('s'), Some(0));
        assert_eq!(c.by_mnemonic('S'), Some(0));
        assert_eq!(c.by_mnemonic('d'), Some(1));
        assert_eq!(c.by_mnemonic('c'), Some(2));
        // `y` was the delete prompts' answer for years. It is not a key now.
        assert_eq!(c.by_mnemonic('y'), None);
        assert_eq!(c.by_mnemonic('z'), None);
    }

    /// A label with no letter of its own is numbered rather than left mute,
    /// and must not silently steal another button's key.
    #[test]
    fn a_label_with_nothing_to_offer_is_numbered() {
        let c = Confirm::new(
            "T",
            "B",
            vec![choice("…", Tone::Safe), choice("Cancel", Tone::Safe)],
        );
        assert_eq!(marked(&c), ["… (1)[1]", "Cancel[C]"]);
        assert_eq!(c.by_mnemonic('c'), Some(1));
        assert_eq!(c.by_mnemonic('1'), Some(0));
    }

    /// Esc dismisses rather than answering when every button does something.
    #[test]
    fn a_dialog_with_no_retreat_has_no_escape_choice() {
        let c = Confirm::new(
            "Go to Line",
            "B",
            vec![
                choice("Scan", Tone::Safe),
                choice("Go to Byte Offset", Tone::Safe),
            ],
        )
        .escaping(None);
        assert_eq!(c.escape, None);
        assert_eq!(marked(&c), ["Scan[S]", "Go to Byte Offset[G]"]);
    }

    #[test]
    fn a_destructive_lead_can_open_on_the_retreat() {
        let c = abc().selecting(2);
        assert_eq!(c.current().map(|c| c.label.as_str()), Some("Cancel"));
    }

    /// A dialog whose most consequential outcome is listed first must not
    /// open on it.
    #[test]
    fn a_dialog_never_opens_on_a_destructive_button() {
        let c = Confirm::new(
            "File Changed on Disk",
            "B",
            vec![
                choice("Overwrite", Tone::Destructive),
                choice("Cancel", Tone::Safe),
            ],
        );
        assert_eq!(c.current().map(|c| c.label.as_str()), Some("Cancel"));
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
                choice("Overwrite", Tone::Destructive),
                choice("Skip", Tone::Safe),
                choice("Overwrite All", Tone::Destructive),
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
                choice("Delete", Tone::Destructive),
                choice("Delete All", Tone::Destructive),
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
                choice("Load", Tone::Safe),
                choice("Choose Encoding…", Tone::Safe),
                choice("Cancel", Tone::Safe),
            ],
        );
        assert_eq!(c.current().map(|c| c.label.as_str()), Some("Load"));
    }

    /// Folding is case-insensitive in every alphabet, not just ASCII.
    ///
    /// `eq_ignore_ascii_case` is plain `==` outside ASCII, so it called `Ü`
    /// and `ü` different keys. Nothing assigns non-ASCII letters any more, so
    /// this guards the rule itself rather than a dialog that could reach it.
    #[test]
    fn folding_is_case_insensitive_beyond_ascii() {
        assert!(same_letter('Ü', 'ü'));
        assert!(same_letter('О', 'о'));
        assert!(same_letter('A', 'a'));
        assert!(!same_letter('Ü', 'U'));
        assert!(!same_letter('o', 'о'), "Latin o is not Cyrillic о");
    }

    /// German's paste conflict: every button gets a distinct key the keyboard
    /// can actually send, and the umlauts answer nothing.
    ///
    /// This dialog is where the old rule went wrong: `Überschreiben` was
    /// marked on `Ü` while `Alle überschreiben` took `ü`, so the `ü` a
    /// keyboard produces fired the *more* destructive button.
    #[test]
    fn german_umlauts_never_become_accelerators() {
        let c = Confirm::new(
            "Namenskonflikt",
            "B",
            vec![
                choice("Überschreiben", Tone::Destructive),
                choice("Alle überschreiben", Tone::Destructive),
                choice("Überspringen", Tone::Safe),
                choice("Alle überspringen", Tone::Safe),
                choice("Abbrechen", Tone::Safe),
            ],
        )
        .escaping(Some(4));

        let marks: Vec<char> = c
            .choices
            .iter()
            .map(|x| x.mnemonic.expect("every button answers to something").ch)
            .collect();
        for m in &marks {
            assert!(
                m.is_ascii_alphanumeric(),
                "{m:?} is not a key a terminal sends"
            );
        }
        for (i, a) in marks.iter().enumerate() {
            for b in &marks[i + 1..] {
                assert!(!same_letter(*a, *b), "{a} and {b} are one key");
            }
        }
        assert_eq!(c.by_mnemonic('ü'), None, "an umlaut must answer nothing");
        assert_eq!(c.by_mnemonic('Ü'), None);
        // The retreat picked first, so it keeps the initial of its own word.
        assert_eq!(c.by_mnemonic('a'), Some(4));
    }

    /// An all-Cyrillic dialog is numbered throughout, retreat first.
    ///
    /// Russian's `Отмена` and `Пропустить все` used to take `О` and `о` —
    /// one key between them — so typing `о` to cancel skipped every file.
    #[test]
    fn cyrillic_labels_are_numbered_not_shadowed() {
        let c = Confirm::new(
            "Namenskonflikt",
            "B",
            vec![
                choice("Перезаписать", Tone::Destructive),
                choice("Пропустить все", Tone::Safe),
                choice("Отмена", Tone::Safe),
            ],
        )
        .escaping(Some(2));

        assert_eq!(c.by_mnemonic('1'), Some(2), "the retreat is numbered first");
        assert_eq!(
            c.by_mnemonic('о'),
            None,
            "a Cyrillic letter answers nothing"
        );
        for ch in &c.choices {
            let m = ch.mnemonic.expect("every button answers to something");
            assert!(m.ch.is_ascii_digit());
            assert!(ch.label.contains(m.ch), "{} must show its key", ch.label);
        }
    }

    /// A label the keyboard cannot produce still gets a key, and shows it.
    #[test]
    fn a_label_without_ascii_is_numbered() {
        let c = Confirm::new(
            "Unsaved Changes",
            "B",
            vec![
                choice("保存", Tone::Safe),
                choice("破棄", Tone::Destructive),
                choice("キャンセル", Tone::Safe),
            ],
        )
        .escaping(Some(2));

        for ch in &c.choices {
            let m = ch.mnemonic.expect("every button answers to something");
            assert!(
                m.ch.is_ascii_alphanumeric(),
                "{} got {:?}, which no terminal can send",
                ch.label,
                m.ch
            );
            assert!(
                ch.label.contains(m.ch),
                "{} does not show the key it answers to",
                ch.label
            );
        }
        // The retreat is numbered first, so it is 1 wherever it appears.
        assert_eq!(c.by_mnemonic('1').map(|i| i), Some(2));
    }

    /// A Latin label is left alone — numbering is the fallback, not the rule.
    #[test]
    fn ascii_labels_are_never_numbered() {
        let c = abc();
        for ch in &c.choices {
            assert!(
                !ch.label.contains('('),
                "{} was numbered needlessly",
                ch.label
            );
        }
    }
}
