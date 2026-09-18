//! The confirmation modal: a question where the user is looking.
//!
//! **The bottom row was the bug.** Every destructive confirmation in the
//! editor asked on the terminal's last line — the same line a status message
//! flashes on, forty rows below the caret on a tall terminal, in the prompt's
//! own low-contrast colours — and stated its answers as parenthesised letters
//! run together in a sentence. The quit confirmation is the one people
//! reported, because it is the one where nothing happening *looks* like a
//! hang; it was never specific to quitting.
//!
//! So the question is a card in the middle of the frame, over a `Scrim::Dim`
//! pass on everything behind it, and the answers are buttons. The shape is
//! the workspace-trust prompt's (`super::trust`), deliberately: an
//! `Modality::Exclusive` layer, a bordered column, and `super::modal::keys`
//! at the top of it so a key nothing inside answers is named as this
//! surface's rather than falling through to the buffer.
//!
//! **What is NOT here: the query-replace confirm.** `Y/n/!/q` per match is
//! the one prompt whose whole job is to keep the buffer visible while it
//! asks — a modal over the match it is asking about would hide the thing
//! being confirmed. It stays on the row.

use std::rc::Rc;

use fresh_ui::{
    col, gesture, layout_reader, row, text, text_runs, Align, Anchor, Event, GestureKind, Key,
    LayoutInfo, Modality, MouseButton, Node, Place, PointerMode, Run, Scrim, Sizing,
};

use crate::app::shell_host::shell_theme::{attrs, pair};
use crate::primitives::display_width::str_width;

use super::msg::{UiFact, UiMsg};

/// The card's usual width. A dialog whose buttons need more than this gets
/// more — see [`width_for`] — and a frame too narrow for either takes what is
/// left, less the two columns kept clear on each side.
pub const DIALOG_WIDTH: u16 = 64;

/// How wide a button's cell is: `[ ` + label + ` ]`.
fn cell_width(label: &str) -> usize {
    str_width(label) + 4
}

/// How wide the card wants to be, given its buttons and the frame it is in.
///
/// **The default width is a floor, not a ceiling.** Four spelled-out outcomes
/// ("Save and Quit", "Discard and Quit", "Quit (recoverable)", "Cancel") come
/// to more than 64 columns, and a card that clipped the last one would hide
/// exactly the button a frightened user is looking for. Where the frame has
/// the room, the card takes it; where it does not, [`buttons`] wraps the row.
pub fn width_for(labels: &[String], frame_width: u16) -> u16 {
    let gaps = labels.len().saturating_sub(1);
    // Two border columns, one of padding inside each, and one gutter between
    // each pair of buttons.
    let natural = labels.iter().map(|l| cell_width(l)).sum::<usize>() + gaps + 4;
    let want = DIALOG_WIDTH.max(u16::try_from(natural).unwrap_or(u16::MAX));
    want.min(frame_width.saturating_sub(4)).max(1)
}

/// One button.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Button {
    pub label: String,
    /// The accelerator's byte span inside `label`, when the letter is in the
    /// text. Marked so the key is discoverable without a legend row.
    pub mnemonic: Option<(usize, usize)>,
    /// Drawn in the error colour, and never the button that opens in hand.
    pub destructive: bool,
    /// The pointer is over this button. A third state, distinct from armed:
    /// it says where a click would land, not what Enter would take.
    pub hovered: bool,
}

/// What the dialog shows, with every string already resolved.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Confirm {
    pub title: String,
    /// The question. Wrapped by the tree at whatever width the card turns out
    /// to be, so it arrives as prose.
    pub body: String,
    /// A quieter second paragraph — the path at risk, the files, the
    /// workspaces. Empty for none.
    pub detail: String,
    pub buttons: Vec<Button>,
    pub selected: usize,
    /// The card's width, already resolved against the frame.
    pub width: u16,
    /// The tallest the card may be, likewise.
    pub max_height: u16,
}

pub fn key() -> Key {
    Key::Str("confirm_dialog".into())
}

pub fn button_key(i: usize) -> Key {
    Key::Pair("confirm_button".into(), i as u64)
}

/// The dialog as a layer: centred, dimming, and exclusive.
///
/// **The scrim dims and swallows; it does not dismiss.** `Modality::Exclusive`
/// makes every press outside the card a no-op, and no gesture is declared on
/// the scrim itself — the same as the workspace-trust prompt. A misaimed click
/// should not answer a question about unsaved work, so the only ways out are
/// a button and Esc.
pub fn layer(c: &Confirm) -> Node<UiMsg> {
    fresh_ui::layer()
        .anchor(Anchor::Screen(Align::Center))
        .place(Place::Over)
        .modality(Modality::Exclusive)
        .scrim(Some(Scrim::Dim))
        .child(super::modal::keys(
            super::modal::KeySlot::Confirm,
            card(c).w(Sizing::Cells(c.width)).key(key()),
        ))
}

/// **The question scrolls; the answers never do.**
///
/// The buttons sit outside the viewport, so a card too short for its message
/// clips the message — which can be scrolled — rather than the row of
/// outcomes, which could only be reached by guessing. A confirmation with no
/// visible answer is the fault this whole dialog exists to remove, and a tall
/// question on a short terminal is exactly where it would have come back.
fn card(c: &Confirm) -> Node<UiMsg> {
    let ring = pair("ui.popup_border_fg", "ui.popup_bg");
    let rows_of_buttons = buttons(c);
    let body = fresh_ui::viewport(col().children(rows(c)))
        .max_h(body_budget(c.max_height, rows_of_buttons.len()))
        .scrollbar();
    let mut kids = vec![body];
    kids.extend(rows_of_buttons);
    col().theme(ring).border().children(kids)
}

/// How many rows the scrolling question gets: the card, less its two border
/// rows, less every row the buttons need. At least one, so a card squeezed
/// past all reason still renders something above the answers.
fn body_budget(max_height: u16, button_rows: usize) -> u16 {
    max_height
        .saturating_sub(2)
        .saturating_sub(button_rows as u16)
        .max(1)
}

fn rows(c: &Confirm) -> Vec<Node<UiMsg>> {
    let ring = pair("ui.popup_border_fg", "ui.popup_bg");
    let ink = pair("ui.popup_text_fg", "ui.popup_bg");
    let dim = attrs("ui.popup_text_fg", "ui.popup_bg", &["dim"]);
    let mut out: Vec<Node<UiMsg>> = vec![
        line(
            format!(" {}", c.title),
            attrs("ui.popup_border_fg", "ui.popup_bg", &["bold"]),
        ),
        rule(ring.clone()),
        blank(),
        // `wrap` with `Sizing::Auto`: the column's height is however many rows
        // the question takes at this width, which is what stops a long path or
        // a five-workspace summary from being cut off the way the row cut it.
        text(format!(" {}", c.body))
            .wrap()
            .theme(ink)
            .h(Sizing::Auto),
    ];
    if !c.detail.is_empty() {
        out.push(blank());
        out.push(
            text(format!(" {}", c.detail))
                .wrap()
                .theme(dim)
                .h(Sizing::Auto),
        );
    }
    out.push(blank());
    out.push(rule(ring));
    out
}

fn line(s: String, theme: String) -> Node<UiMsg> {
    text(s).theme(theme).h(Sizing::Cells(1))
}

fn blank() -> Node<UiMsg> {
    row().h(Sizing::Cells(1))
}

/// A rule across the card, as wide as the card turns out to be.
fn rule(theme: String) -> Node<UiMsg> {
    layout_reader(move |info: LayoutInfo| {
        let w = info.constraints.max_w.max(1) as usize;
        text("─".repeat(w)).theme(theme.clone())
    })
    .h(Sizing::Cells(1))
}

/// The buttons, right-aligned, wrapping onto further rows when the card is
/// too narrow to hold them all.
///
/// **Right-aligned, and the retreat is last.** Every caller lists its choices
/// in order of consequence with the safe one at the end, so the rightmost
/// button is always the way out — which is also where Esc lands and where a
/// pointer reaching for "no" expects to find it.
fn buttons(c: &Confirm) -> Vec<Node<UiMsg>> {
    pack(&c.buttons, row_budget(c.width))
        .into_iter()
        .map(|indices| {
            let mut kids: Vec<Node<UiMsg>> =
                vec![row().flex(1).pointer_mode(PointerMode::Transparent)];
            for (n, i) in indices.iter().enumerate() {
                if n > 0 {
                    kids.push(gutter());
                }
                kids.push(button(*i, &c.buttons[*i], *i == c.selected));
            }
            // One column of padding inside the right border, so the armed
            // button's bracket does not sit against it.
            kids.push(gutter());
            row().h(Sizing::Cells(1)).children(kids)
        })
        .collect()
}

/// How many columns a row of buttons has to work with.
///
/// Two border columns, and the one column of padding each row keeps inside the
/// right border — so a row packed to this still fits with its trailing gutter
/// drawn rather than clipped.
fn row_budget(width: u16) -> usize {
    width.saturating_sub(3).max(1) as usize
}

/// Which buttons go on which row, given the width inside the card's border.
///
/// Greedy, in declaration order, so the outcomes keep the sequence the caller
/// put them in however many rows that takes — a wrapped dialog still reads
/// left to right, most consequential to least, with the retreat last.
fn pack(buttons: &[Button], inner: usize) -> Vec<Vec<usize>> {
    let mut lines: Vec<Vec<usize>> = Vec::new();
    let mut line: Vec<usize> = Vec::new();
    let mut used = 0usize;
    for (i, b) in buttons.iter().enumerate() {
        let w = cell_width(&b.label);
        // `used + w < inner` is "this button and the one-column gutter before
        // it still fit", in the form clippy prefers to `used + w + 1 <= inner`.
        match line.is_empty() || used + w < inner {
            true => used += if line.is_empty() { w } else { w + 1 },
            false => {
                lines.push(std::mem::take(&mut line));
                used = w;
            }
        }
        line.push(i);
    }
    if !line.is_empty() {
        lines.push(line);
    }
    lines
}

fn gutter() -> Node<UiMsg> {
    text(" ")
        .theme(pair("ui.popup_text_fg", "ui.popup_bg"))
        .pointer_mode(PointerMode::Transparent)
}

fn button(i: usize, b: &Button, focused: bool) -> Node<UiMsg> {
    // Three things a button can be, and they have to stay distinguishable:
    // armed (what Enter takes), hovered (where a click would land), and
    // destructive (what it costs). Armed wins the ground; hover takes the
    // quieter selection ground so it reads as "under the pointer" rather than
    // "about to happen"; destructive keeps the error colour in every state.
    let (base, accel) = match (focused, b.hovered, b.destructive) {
        (true, _, true) => (
            attrs("ui.popup_bg", "diagnostic.error_fg", &["bold"]),
            attrs("ui.popup_bg", "diagnostic.error_fg", &["bold", "underline"]),
        ),
        (true, _, false) => (
            attrs("ui.popup_bg", "ui.help_key_fg", &["bold"]),
            attrs("ui.popup_bg", "ui.help_key_fg", &["bold", "underline"]),
        ),
        (false, true, true) => (
            attrs("diagnostic.error_fg", "ui.popup_selection_bg", &["bold"]),
            attrs(
                "diagnostic.error_fg",
                "ui.popup_selection_bg",
                &["bold", "underline"],
            ),
        ),
        (false, true, false) => (
            pair("ui.popup_selection_fg", "ui.popup_selection_bg"),
            attrs(
                "ui.popup_selection_fg",
                "ui.popup_selection_bg",
                &["underline"],
            ),
        ),
        (false, false, true) => (
            pair("diagnostic.error_fg", "ui.popup_bg"),
            attrs("diagnostic.error_fg", "ui.popup_bg", &["underline"]),
        ),
        (false, false, false) => (
            pair("ui.popup_text_fg", "ui.popup_bg"),
            attrs("ui.popup_text_fg", "ui.popup_bg", &["underline"]),
        ),
    };
    // The armed button wears the brackets, so which one Enter takes survives a
    // terminal that drops colour or a theme whose accent is close to its
    // ground — and so the dialog is still readable in a monochrome capture.
    let (open, close) = match focused {
        true => ("[", "]"),
        false => (" ", " "),
    };
    let mut runs = vec![Run::themed(format!("{open} "), base.clone())];
    match b.mnemonic {
        Some((s, e)) if e <= b.label.len() => {
            runs.push(Run::themed(&b.label[..s], base.clone()));
            runs.push(Run::themed(&b.label[s..e], accel));
            runs.push(Run::themed(&b.label[e..], base.clone()));
        }
        _ => runs.push(Run::themed(b.label.clone(), base.clone())),
    }
    runs.push(Run::themed(format!(" {close}"), base));
    gesture(text_runs(runs).h(Sizing::Cells(1)))
        .key(button_key(i))
        .on(
            GestureKind::Press,
            Rc::new(move |e: &Event| {
                if e.button != MouseButton::Left {
                    return None;
                }
                e.stop();
                Some(UiMsg::Ui(UiFact::ConfirmChoose(i)))
            }),
        )
        // Enter and Leave fire on the node itself, so one pair per button is
        // the whole of it — no motion handler, no recorded rectangles.
        .on(
            GestureKind::Enter,
            Rc::new(move |_: &Event| Some(UiMsg::Ui(UiFact::ConfirmHover(Some(i))))),
        )
        .on(
            GestureKind::Leave,
            Rc::new(|_: &Event| Some(UiMsg::Ui(UiFact::ConfirmHover(None)))),
        )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn btn(label: &str) -> Button {
        Button {
            label: label.into(),
            mnemonic: None,
            destructive: false,
            hovered: false,
        }
    }

    fn quit_buttons() -> Vec<Button> {
        [
            "Save and Quit",
            "Discard and Quit",
            "Quit (recoverable)",
            "Cancel",
        ]
        .into_iter()
        .map(btn)
        .collect()
    }

    fn labels(buttons: &[Button]) -> Vec<String> {
        buttons.iter().map(|b| b.label.clone()).collect()
    }

    /// The reported case: the quit dialog's four outcomes do not fit the
    /// default width, and the one that would have been clipped is "Cancel".
    #[test]
    fn the_card_grows_to_fit_its_buttons() {
        let w = width_for(&labels(&quit_buttons()), 100);
        assert!(
            w > DIALOG_WIDTH,
            "four spelled-out outcomes need more than the default {DIALOG_WIDTH}; got {w}"
        );
        assert!(
            w <= 96,
            "and never more than the frame less its margins; got {w}"
        );
        assert_eq!(pack(&quit_buttons(), w as usize - 2).len(), 1);
    }

    /// A dialog with little to say still gets the default width, so small
    /// confirmations do not come out as thin slivers.
    #[test]
    fn a_small_dialog_keeps_the_default_width() {
        assert_eq!(
            width_for(&labels(&[btn("Delete"), btn("Cancel")]), 100),
            DIALOG_WIDTH
        );
    }

    /// A frame too narrow for the card clamps rather than overflowing, and
    /// the buttons wrap onto further rows instead of being cut off.
    #[test]
    fn a_narrow_frame_clamps_and_wraps() {
        let w = width_for(&labels(&quit_buttons()), 40);
        assert_eq!(w, 36, "the card takes the frame less two columns each side");
        let rows = pack(&quit_buttons(), row_budget(w));
        assert!(rows.len() > 1, "the buttons must wrap, not be clipped");
        // Every button is placed exactly once, in order.
        let placed: Vec<usize> = rows.concat();
        assert_eq!(placed, vec![0, 1, 2, 3]);
        // And no row overflows the card.
        for r in &rows {
            let used: usize = r
                .iter()
                .map(|i| cell_width(&quit_buttons()[*i].label))
                .sum::<usize>()
                + r.len().saturating_sub(1);
            assert!(used <= row_budget(w), "row {r:?} overflows {w}");
        }
    }

    /// One button per row is the floor: a card narrower than a single button
    /// still places each of them rather than dropping any.
    #[test]
    fn an_absurdly_narrow_card_still_places_every_button() {
        let rows = pack(&quit_buttons(), 4);
        assert_eq!(rows.len(), 4);
        assert_eq!(rows.concat(), vec![0, 1, 2, 3]);
    }

    /// A card too short for its question gives up question rows, never button
    /// rows: the body's budget always leaves every packed button row standing.
    #[test]
    fn the_buttons_keep_their_rows_on_a_short_card() {
        for max_height in 3u16..24 {
            for button_rows in 1usize..5 {
                let body = body_budget(max_height, button_rows);
                assert!(body >= 1, "the question never vanishes entirely");
                // Two border rows plus the buttons plus the body fit, except
                // where the floor of one body row has to overrun a tiny card.
                let needed = 2 + button_rows as u16 + body;
                assert!(
                    needed <= max_height || body == 1,
                    "h={max_height} rows={button_rows}: needs {needed}"
                );
            }
        }
    }

    /// The tall-question case the scrim was hiding: a four-row button block on
    /// a short card leaves the question one row, and the buttons all four.
    #[test]
    fn a_wrapped_button_block_is_never_squeezed_out() {
        assert_eq!(body_budget(7, 4), 1);
        assert_eq!(body_budget(12, 4), 6);
        assert_eq!(body_budget(24, 1), 21);
    }
}
