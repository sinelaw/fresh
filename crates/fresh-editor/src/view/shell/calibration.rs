//! The input calibration wizard.
//!
//! The smallest of the three full-screen modal interiors, and the one with no
//! mouse at all: it is driven entirely by single-letter keys, so nothing here
//! answers a press. What it had instead was the other half of the pattern —
//! a painter that computed its own rectangle, split it into three bands by
//! hand, and clamped its own scroll offset against the middle band's height.
//!
//! **The three bands are a column.** `Layout::vertical([Length(5), Min(8),
//! Length(4)])` is `Cells(5)`, `flex(1)`, `Cells(4)`, and the middle one
//! stopped being a rectangle a caller had to be handed.
//!
//! **The scroll was arithmetic on that rectangle.** The capture phase kept the
//! key it is waiting on visible with
//! `if key_idx >= available_height { key_idx - available_height + 1 }` — a
//! window computed from a height the painter had, against a list it also drew.
//! A `viewport` owns the window and `Anchor::reveal` says the only thing left:
//! put this row in it. The two blank-then-step-info footer rows the
//! `saturating_sub(2)` accounted for are ordinary rows now, so nothing has to
//! remember they are there.
//!
//! The confirmation dialog is the same box with different contents and a
//! warning-coloured ring, which is what it always was.

use fresh_ui::{
    behavior, col, layout_reader, row, text, Align, Anchor, Component, LayoutInfo, Modality, Node,
    Place, Scrim, Sizing,
};

use crate::app::shell_host::shell_theme::{attrs, pair};

use super::msg::UiMsg;

/// The dialog's widest form. A narrower frame takes what is left, less the two
/// columns the painter kept clear on either side.
pub const DIALOG_WIDTH: u16 = 60;
/// And its tallest.
pub const DIALOG_HEIGHT: u16 = 20;

/// One row of a key list: a status glyph, a name, and the ink they share.
#[derive(Clone, Debug, PartialEq)]
pub struct KeyRow {
    pub glyph: String,
    pub name: String,
    /// A theme name, already resolved from the key's status.
    pub theme: String,
}

/// One `[k] label` control in the footer.
#[derive(Clone, Debug, PartialEq)]
pub struct Control {
    pub key: String,
    pub label: String,
    /// The bracketed key's colour. The label beside it is the dialog's own.
    pub key_theme: String,
}

/// What the wizard shows. Every string is resolved and every colour named
/// before it gets here: a description is a pure function of what it is handed.
#[derive(Clone, Debug, PartialEq)]
pub enum Phase {
    /// Waiting for a key press, with the current group's list beside it.
    Capture {
        group_label: String,
        group_name: String,
        press_prompt: String,
        target_name: String,
        keys: Vec<KeyRow>,
        /// Which of `keys` the wizard is waiting on — the one the window
        /// follows.
        at: usize,
        step_info: String,
    },
    /// Checking the captured keys translate.
    Verify {
        title: String,
        instructions: String,
        translations_label: String,
        translations: String,
        verified_line: String,
        keys: Vec<KeyRow>,
    },
    /// Nothing needed translating.
    AllOk { title: String, message: String },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Calibration {
    pub title: String,
    pub phase: Phase,
    pub controls: Vec<Control>,
    pub status: String,
    /// The destructive-action confirmation, when one is pending. It replaces
    /// the whole dialog rather than sitting over it, which is what the painter
    /// did by returning early.
    pub confirm: Option<Confirm>,
    /// Resolved against the frame the dialog is centred in.
    pub width: u16,
    pub height: u16,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Confirm {
    pub title: String,
    pub message: String,
    pub confirm_key: String,
    pub confirm_label: String,
    pub cancel_key: String,
    pub cancel_label: String,
}

pub fn key() -> fresh_ui::Key {
    fresh_ui::Key::Str("calibration_dialog".into())
}

/// The row the capture phase's window follows.
pub fn key_row_key(i: usize) -> fresh_ui::Key {
    fresh_ui::Key::Pair("calibration_key".into(), i as u64)
}

/// The wizard as a layer: centred, dimming, and exclusive.
///
/// `apply_dimming` over the whole frame is `Scrim::Dim`, and the capture the
/// modal band gave it is `Modality::Exclusive` — the wizard takes no pointer
/// events itself, and exclusivity is what keeps everything behind it from
/// taking them either.
pub fn layer(c: &Calibration) -> Node<UiMsg> {
    fresh_ui::layer()
        .anchor(Anchor::Screen(Align::Center))
        .place(Place::Over)
        .modality(Modality::Exclusive)
        .scrim(Some(Scrim::Dim))
        // **The keys are the wizard's, and they are raw.** It exists to
        // record which physical key the user pressed, so nothing about them
        // is an intent; the claim says whose they are and the interior reads
        // the crossterm event the editor already had.
        .child(super::modal::keys(
            super::modal::KeySlot::Calibration,
            dialog(c)
                .w(Sizing::Cells(c.width))
                .h(Sizing::Cells(c.height))
                .key(key()),
        ))
}

fn dialog(c: &Calibration) -> Node<UiMsg> {
    match &c.confirm {
        Some(k) => confirm_box(k),
        None => wizard_box(c),
    }
}

fn confirm_box(k: &Confirm) -> Node<UiMsg> {
    let ring = pair("diagnostic.warning_fg", "ui.popup_bg");
    let ink = pair("ui.popup_text_fg", "ui.popup_bg");
    col().theme(ring).border().children([
        titled(&k.title, "diagnostic.warning_fg"),
        blank(),
        line(
            k.message.clone(),
            attrs("ui.popup_text_fg", "ui.popup_bg", &["bold"]),
        ),
        blank(),
        blank(),
        controls_row(
            &[
                Control {
                    key: k.confirm_key.clone(),
                    label: k.confirm_label.clone(),
                    key_theme: "diagnostic.error_fg".into(),
                },
                Control {
                    key: k.cancel_key.clone(),
                    label: k.cancel_label.clone(),
                    key_theme: "ui.help_key_fg".into(),
                },
            ],
            &ink,
        ),
    ])
}

fn wizard_box(c: &Calibration) -> Node<UiMsg> {
    let ring = pair("ui.popup_border_fg", "ui.popup_bg");
    let ink = pair("ui.popup_text_fg", "ui.popup_bg");
    // `Length(5)`, `Min(8)`, `Length(4)` — the painter's own three bands, in
    // the order it split them.
    let (head, body) = match &c.phase {
        Phase::Capture {
            group_label,
            group_name,
            press_prompt,
            target_name,
            keys,
            at,
            step_info,
        } => (
            col().h(Sizing::Cells(5)).children([
                row().h(Sizing::Cells(1)).children([
                    text(format!("{group_label}: ")).theme(ink.clone()),
                    text(group_name.clone()).theme(pair("ui.help_key_fg", "ui.popup_bg")),
                ]),
                blank(),
                line(
                    press_prompt.clone(),
                    attrs("ui.popup_text_fg", "ui.popup_bg", &["bold"]),
                ),
                line(
                    format!("  {target_name}"),
                    attrs("diagnostic.warning_fg", "ui.popup_bg", &["bold"]),
                ),
                blank(),
            ]),
            fresh_ui::ComponentExt::node(KeyList {
                rows: std::rc::Rc::new(
                    keys.iter()
                        .cloned()
                        .chain([
                            KeyRow {
                                glyph: String::new(),
                                name: String::new(),
                                theme: ink.clone(),
                            },
                            KeyRow {
                                glyph: String::new(),
                                name: step_info.clone(),
                                theme: pair("editor.line_number_fg", "ui.popup_bg"),
                            },
                        ])
                        .collect(),
                ),
                at: Some(*at),
            })
            .flex(1),
        ),
        Phase::Verify {
            title,
            instructions,
            translations_label,
            translations,
            verified_line,
            keys,
        } => (
            col().h(Sizing::Cells(5)).children([
                line(
                    title.clone(),
                    attrs("ui.popup_text_fg", "ui.popup_bg", &["bold"]),
                ),
                blank(),
                line(instructions.clone(), ink.clone()),
                blank(),
                row().h(Sizing::Cells(1)).children([
                    text(format!("{translations_label}: ")).theme(ink.clone()),
                    text(translations.clone()).theme(pair("diagnostic.info_fg", "ui.popup_bg")),
                ]),
            ]),
            fresh_ui::ComponentExt::node(KeyList {
                rows: std::rc::Rc::new(
                    [
                        KeyRow {
                            glyph: String::new(),
                            name: verified_line.clone(),
                            theme: ink.clone(),
                        },
                        KeyRow {
                            glyph: String::new(),
                            name: String::new(),
                            theme: ink.clone(),
                        },
                    ]
                    .into_iter()
                    .chain(keys.iter().cloned())
                    .collect(),
                ),
                at: None,
            })
            .flex(1),
        ),
        Phase::AllOk { title, message } => (
            col().h(Sizing::Cells(5)).children([
                blank(),
                line(
                    title.clone(),
                    attrs("diagnostic.info_fg", "ui.popup_bg", &["bold"]),
                ),
                blank(),
                text(message.clone()).wrap().theme(ink.clone()),
            ]),
            row().flex(1),
        ),
    };
    col().theme(ring).border().children([
        titled(&c.title, "ui.popup_border_fg"),
        head,
        body,
        col().h(Sizing::Cells(4)).children([
            controls_row(&c.controls, &ink),
            blank(),
            line(c.status.clone(), ink.clone()),
        ]),
    ])
}

/// The legend the `Block`'s `.title()` drew, as a row of the box.
fn titled(title: &str, theme: &str) -> Node<UiMsg> {
    line(title.to_string(), attrs(theme, "ui.popup_bg", &["bold"]))
}

fn controls_row(controls: &[Control], ink: &str) -> Node<UiMsg> {
    let mut kids: Vec<Node<UiMsg>> = Vec::with_capacity(controls.len() * 2);
    for c in controls {
        kids.push(text(format!("[{}]", c.key)).theme(pair(&c.key_theme, "ui.popup_bg")));
        kids.push(text(format!(" {} ", c.label)).theme(ink.to_string()));
    }
    row().h(Sizing::Cells(1)).children(kids)
}

fn line(s: String, theme: String) -> Node<UiMsg> {
    text(s).theme(theme).h(Sizing::Cells(1))
}

fn blank() -> Node<UiMsg> {
    row().h(Sizing::Cells(1))
}

#[derive(Default)]
struct KeyListState {
    anchor: Option<std::rc::Rc<behavior::Anchor>>,
    revealed: behavior::Cache<usize, ()>,
}

/// The key list in a window that owns its own scroll.
///
/// The painter kept the current key visible by computing an offset from the
/// band's height — `key_idx - available_height + 1`, with a
/// `saturating_sub(2)` for the two footer rows it knew it had drawn. The
/// window is the viewport's now, the footer rows are ordinary rows, and the
/// only thing left to say is which row to reveal.
struct KeyList {
    rows: std::rc::Rc<Vec<KeyRow>>,
    at: Option<usize>,
}

impl Component<UiMsg> for KeyList {
    type State = KeyListState;

    fn init(&self, cx: &mut fresh_ui::InitCx<'_, UiMsg>) -> KeyListState {
        KeyListState {
            anchor: Some(cx.register(behavior::Anchor::default())),
            ..KeyListState::default()
        }
    }

    fn build(&self, s: &KeyListState, _cx: &mut fresh_ui::BuildCx<'_, UiMsg>) -> Node<UiMsg> {
        if let (Some(a), Some(i)) = (s.anchor.clone(), self.at) {
            s.revealed.get_or(i, move || a.reveal(i as u32));
        }
        let rows = self.rows.clone();
        let body = col().children(rows.iter().enumerate().map(|(i, r)| {
            row()
                .key(key_row_key(i))
                .h(Sizing::Cells(1))
                .theme(r.theme.clone())
                .children([
                    text(match r.glyph.is_empty() {
                        true => String::new(),
                        false => format!(" {} ", r.glyph),
                    }),
                    text(r.name.clone()),
                ])
        }));
        let v = fresh_ui::viewport(body).scrollbar();
        match s.anchor.clone() {
            Some(a) => v.anchor_to(a),
            None => v,
        }
    }
}

/// The dialog's resolved size in a frame of `area`.
///
/// The painter's own two lines, kept because they are the rule rather than the
/// arithmetic: the dialog is 60×20 unless the frame is too small for it, and
/// then it takes what is left with two columns and two rows to spare.
pub fn fit(area: LayoutInfo) -> (u16, u16) {
    let (w, h) = (area.constraints.max_w, area.constraints.max_h);
    (
        DIALOG_WIDTH.min(w.saturating_sub(4)),
        DIALOG_HEIGHT.min(h.saturating_sub(4)),
    )
}

/// The wizard, sized against whatever it is placed in.
pub fn sized(c: &Calibration) -> Node<UiMsg> {
    let c = c.clone();
    layout_reader(move |info: LayoutInfo| {
        let (w, h) = fit(info);
        layer(&Calibration {
            width: w,
            height: h,
            ..c.clone()
        })
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::view::shell::frame::{frame_tree, Frame};
    use fresh_ui::{Size, Ui};

    fn keys(n: usize) -> Vec<KeyRow> {
        (0..n)
            .map(|i| KeyRow {
                glyph: ">".into(),
                name: format!("key{i}"),
                theme: pair("ui.popup_text_fg", "ui.popup_bg"),
            })
            .collect()
    }

    fn capture(n: usize, at: usize) -> Calibration {
        Calibration {
            title: "Calibrate".into(),
            phase: Phase::Capture {
                group_label: "Group".into(),
                group_name: "Navigation".into(),
                press_prompt: "Press:".into(),
                target_name: "Ctrl+Left".into(),
                keys: keys(n),
                at,
                step_info: format!("Step {}/{n}", at + 1),
            },
            controls: vec![Control {
                key: "a".into(),
                label: "Abort".into(),
                key_theme: "diagnostic.error_fg".into(),
            }],
            status: "waiting".into(),
            confirm: None,
            width: DIALOG_WIDTH,
            height: DIALOG_HEIGHT,
        }
    }

    fn laid_out(c: Calibration, w: u16, h: u16) -> Ui<UiMsg> {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            frame_tree(Frame {
                calibration: Some(c),
                ..Frame::default()
            }),
            Size::new(w, h),
        );
        ui
    }

    fn rect(ui: &Ui<UiMsg>, k: &fresh_ui::Key) -> fresh_ui::Rect {
        ui.rect_of(ui.find_by_key(k).unwrap_or_else(|| panic!("{k:?}")))
    }

    /// **Centred, 60×20, and clamped to a small frame** — the painter's two
    /// lines of arithmetic, kept as the rule and applied where the extent is.
    #[test]
    fn the_dialog_is_centred_at_its_documented_size() {
        let ui = laid_out(capture(4, 0), 120, 40);
        let r = rect(&ui, &key());
        assert_eq!((r.w, r.h), (DIALOG_WIDTH, DIALOG_HEIGHT));
        assert_eq!(r.x, (120 - DIALOG_WIDTH as i32) / 2, "centred across");
        assert_eq!(r.y, (40 - DIALOG_HEIGHT as i32) / 2, "and down");
    }

    /// A frame too small for it gives it what is left, less the two columns
    /// and two rows the painter kept clear.
    #[test]
    fn a_small_frame_gives_the_dialog_what_is_left() {
        let ui = laid_out(capture(4, 0), 40, 12);
        let r = rect(&ui, &key());
        assert_eq!((r.w, r.h), (36, 8));
    }

    /// **The window follows the key the wizard is waiting on.** The painter
    /// computed an offset from the band's height —
    /// `key_idx - available_height + 1`, with a `saturating_sub(2)` for the
    /// two footer rows it knew it had drawn. This asks for a row instead.
    #[test]
    fn the_key_list_scrolls_to_the_key_it_is_waiting_on() {
        // Far more keys than the middle band can hold.
        let ui = laid_out(capture(40, 30), 120, 40);
        let r = rect(&ui, &key_row_key(30));
        let dialog = rect(&ui, &key());
        assert!(
            r.y >= dialog.y && r.y < dialog.y + dialog.h as i32,
            "row 30 is inside the dialog, at {}",
            r.y
        );
        // And the top of the list is not: the window moved.
        let first = rect(&ui, &key_row_key(0));
        assert!(first.y < dialog.y, "row 0 scrolled off the top");
    }

    /// A list that fits does not scroll, and shows its first row.
    #[test]
    fn a_short_key_list_does_not_scroll() {
        let ui = laid_out(capture(3, 0), 120, 40);
        let dialog = rect(&ui, &key());
        let first = rect(&ui, &key_row_key(0));
        assert!(first.y > dialog.y, "row 0 is in view at {}", first.y);
    }

    /// **The confirmation replaces the dialog rather than sitting over it**,
    /// which is what the painter did by returning early — and it keeps the
    /// warning ring that told the user this one is destructive.
    #[test]
    fn a_pending_confirmation_replaces_the_dialogs_contents() {
        let mut c = capture(4, 0);
        c.confirm = Some(Confirm {
            title: "Discard?".into(),
            message: "Progress will be lost".into(),
            confirm_key: "d".into(),
            confirm_label: "Discard".into(),
            cancel_key: "c".into(),
            cancel_label: "Cancel".into(),
        });
        let ui = laid_out(c, 120, 40);
        assert!(
            ui.find_by_key(&key_row_key(0)).is_none(),
            "the key list is gone"
        );
        let painted: Vec<String> = ui
            .spec()
            .layers()
            .iter()
            .filter_map(|i| match &i.draw {
                fresh_ui::Draw::Lines(l) => {
                    Some(l.iter().map(|s| s.to_string()).collect::<Vec<_>>())
                }
                _ => None,
            })
            .flatten()
            .collect();
        assert!(
            painted.iter().any(|t| t.contains("Progress will be lost")),
            "the message is there: {painted:?}"
        );
        assert!(
            painted.iter().any(|t| t.contains("[d]")),
            "and so is the key that confirms it"
        );
    }

    /// **Nothing behind it is interactive.** The wizard takes no pointer
    /// events of its own — it is driven by single letters — so exclusivity is
    /// the whole of what stops a click reaching the editor underneath.
    #[test]
    fn nothing_behind_the_wizard_takes_a_press() {
        let mut ui = laid_out(capture(4, 0), 120, 40);
        let got = ui.dispatch(fresh_ui::Input::press(
            fresh_ui::Point::new(2, 2),
            fresh_ui::MouseButton::Left,
            fresh_ui::Mods::NONE,
        ));
        assert!(
            got.msgs.is_empty(),
            "a press outside the dialog says nothing: {:?}",
            got.msgs
        );
    }
}
