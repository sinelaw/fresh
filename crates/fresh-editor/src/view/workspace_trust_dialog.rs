//! The workspace-trust prompt's options.
//!
//! A bespoke security modal (radio group + descriptions + an OK button and a
//! secondary button), rendered on a dimmed backdrop in the modal z-band. As
//! the mandatory open-time gate the secondary button is "Quit" (exit the
//! editor) and there is no undecided outcome. Every other trigger — opening it
//! voluntarily from the command palette, or activating another workspace on an
//! already-running editor (an Orchestrator session, a project switch) — uses
//! "Cancel" (dismiss without changing the current level), because there are
//! other open sessions that must survive the dismissal.
//!
//! The dialog is a description now — `view::shell::trust` — so its painter,
//! its `TrustDialogLayout` of recorded click rectangles, its `Vec<Seg>` row
//! plan (built once to count and once to draw), its own greedy word-wrap and
//! its scroll clamp are all gone. The three options are the one thing here
//! that was never geometry: localized labels with their mnemonic letters, and
//! the descriptions beneath them.
//!
//! Two helpers went with the painter, and their tests with them: `pad_to`,
//! which stretched a radio's label so the selection bar spanned the row (a
//! themed row does that), and `truncate_middle`, which cut the path against a
//! width the painter had computed. The path is elided by the tree now, at
//! whatever width the dialog turned out to be — from the head rather than the
//! middle, so `…/chunky/fresh` shows the folder and its parent instead of the
//! root and the leaf. For "this folder can execute code", the near end is the
//! end that identifies it.

use fresh_i18n::t;

/// One selectable trust option: its radio label and the one-line description
/// shown beneath it. The mnemonic letter is appended in parentheses (e.g.
/// "(T)") so the keypress works the same in every locale while the surrounding
/// text is translatable.
pub struct TrustOption {
    pub label: String,
    pub description: String,
}

pub fn options() -> [TrustOption; 3] {
    [
        TrustOption {
            label: t!("trust.dialog.opt_trust_label").into_owned(),
            description: t!("trust.dialog.opt_trust_desc").into_owned(),
        },
        TrustOption {
            label: t!("trust.dialog.opt_restrict_label").into_owned(),
            description: t!("trust.dialog.opt_restrict_desc").into_owned(),
        },
        TrustOption {
            label: t!("trust.dialog.opt_block_label").into_owned(),
            description: t!("trust.dialog.opt_block_desc").into_owned(),
        },
    ]
}
