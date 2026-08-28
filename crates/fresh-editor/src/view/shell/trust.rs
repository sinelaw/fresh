//! The workspace-trust prompt.
//!
//! A blocking security modal, and the first of the five that owned the whole
//! mouse channel through `ChromeComponent::capture_mouse` — a band that
//! preempts every walk, the shell's included. Capture is what a modal *is*,
//! and the tree states it directly: `Modality::Exclusive` makes everything
//! outside the layer non-interactive, and `Scrim::Dim` is the pass over the
//! frame that `view::dimming::apply_dimming` made by hand.
//!
//! The dialog sized itself to its content — "no fixed height to drift out of
//! sync", as the painter's own comment put it — by building a `Vec<Seg>`,
//! counting it, and then walking it a second time against a scroll offset it
//! also had to clamp. A column of rows in a viewport is both halves: the
//! column's height *is* the count, and the viewport owns the window and emits
//! its scrollbar exactly when the rows overflow it.
//!
//! **A radio click selects and does not consent.** That is the painter's
//! ruling, kept: `[ OK ]` commits, and the two-step is the same one the
//! keyboard already had. Losing it would make "Trust folder & Allow Tooling" a
//! one-click grant of execution rights on a security prompt.

use std::rc::Rc;

use fresh_ui::{
    col, gesture, layout_reader, row, text, Align, Anchor, Event, GestureKind, Key, LayoutInfo,
    Modality, MouseButton, Node, Place, PointerMode, Scrim, Sizing,
};

use crate::app::shell_host::shell_theme::{attrs, pair};

use super::msg::{UiFact, UiMsg};

/// The prompt's widest form. Narrower frames take what is left, less the two
/// columns the painter kept clear on either side.
pub const DIALOG_WIDTH: u16 = 68;

/// One of the three choices.
#[derive(Clone, Debug, PartialEq)]
pub struct Opt {
    pub label: String,
    pub description: String,
}

/// What the prompt shows, with every string already resolved.
#[derive(Clone, Debug, PartialEq)]
pub struct Trust {
    /// 0 = Trust, 1 = Restricted, 2 = Block.
    pub selected: usize,
    pub title: String,
    /// The "this folder can execute code" line.
    pub can_execute: String,
    pub path_label: String,
    pub path: String,
    /// Why the prompt fired, when markers were detected.
    pub detected: Option<String>,
    pub how_proceed: String,
    pub options: Vec<Opt>,
    pub ok_label: String,
    /// Cancel when the prompt was opened voluntarily; Quit at startup.
    pub secondary_label: String,
    /// The dialog's width, already resolved against the frame.
    pub width: u16,
    /// The tallest the dialog may be, likewise.
    pub max_height: u16,
}

pub fn key() -> Key {
    Key::Str("trust_dialog".into())
}

pub fn radio_key(i: usize) -> Key {
    Key::Pair("trust_radio".into(), i as u64)
}

pub fn ok_key() -> Key {
    Key::Str("trust_ok".into())
}

pub fn secondary_key() -> Key {
    Key::Str("trust_secondary".into())
}

/// The prompt as a layer: centred, dimming, and exclusive.
pub fn layer(t: &Trust) -> Node<UiMsg> {
    fresh_ui::layer()
        .anchor(Anchor::Screen(Align::Center))
        .place(Place::Over)
        .modality(Modality::Exclusive)
        .scrim(Some(Scrim::Dim))
        .child(dialog(t).w(Sizing::Cells(t.width)).key(key()))
}

fn dialog(t: &Trust) -> Node<UiMsg> {
    let ring = pair("ui.popup_border_fg", "ui.popup_bg");
    let body = fresh_ui::viewport(col().children(rows(t)))
        .max_h(t.max_height.saturating_sub(2))
        .scrollbar();
    col().theme(ring).border().child(body)
}

fn rows(t: &Trust) -> Vec<Node<UiMsg>> {
    let ring = pair("ui.popup_border_fg", "ui.popup_bg");
    let ink = pair("ui.popup_text_fg", "ui.popup_bg");
    let dim = attrs("ui.popup_text_fg", "ui.popup_bg", &["dim"]);
    let mut out: Vec<Node<UiMsg>> = vec![
        // The title takes the dialog's own border accent rather than the
        // status bar's warning colour, so the frame reads as one palette.
        line(
            format!(" ⚠  {}", t.title),
            attrs("ui.popup_border_fg", "ui.popup_bg", &["bold"]),
        ),
        rule(ring.clone()),
        line(format!(" {}", t.can_execute), ink.clone()),
        row().h(Sizing::Cells(1)).children([
            text(format!(" {} ", t.path_label)).theme(ink.clone()),
            text(t.path.clone())
                .theme(attrs("ui.popup_text_fg", "ui.popup_bg", &["bold"]))
                .elide(fresh_ui::Elide::Head),
        ]),
    ];
    if let Some(d) = &t.detected {
        // Wrapped by the tree at whatever width the dialog turned out to be.
        out.push(
            text(format!(" {d}"))
                .wrap()
                .theme(dim.clone())
                .h(Sizing::Auto),
        );
    }
    out.push(blank());
    out.push(line(format!(" {}", t.how_proceed), ink.clone()));
    out.push(blank());
    for (i, opt) in t.options.iter().enumerate() {
        out.push(radio(i, opt, i == t.selected));
        // Six spaces of indent, which `Wrap::Hanging` carries to every
        // continuation row — the painter prefixed each wrapped line by hand.
        out.push(
            text(format!("      {}", opt.description))
                .wrap_hanging()
                .theme(dim.clone())
                .h(Sizing::Auto),
        );
        out.push(blank());
    }
    out.push(rule(ring));
    out.push(buttons(t));
    out
}

fn line(s: String, theme: String) -> Node<UiMsg> {
    text(s).theme(theme).h(Sizing::Cells(1))
}

fn blank() -> Node<UiMsg> {
    row().h(Sizing::Cells(1))
}

/// A rule across the dialog, as wide as the dialog turns out to be.
///
/// `"─".repeat(iw)` needed the inner width, which the painter had and a
/// description does not — this is what `layout_reader` is for.
fn rule(theme: String) -> Node<UiMsg> {
    layout_reader(move |info: LayoutInfo| {
        let w = info.constraints.max_w.max(1) as usize;
        text("─".repeat(w)).theme(theme.clone())
    })
    .h(Sizing::Cells(1))
}

fn radio(i: usize, opt: &Opt, selected: bool) -> Node<UiMsg> {
    let theme = match selected {
        true => attrs("ui.popup_selection_fg", "ui.popup_selection_bg", &["bold"]),
        false => pair("ui.popup_text_fg", "ui.popup_bg"),
    };
    let marker = match selected {
        true => "(*)",
        false => "( )",
    };
    // The row is themed, not the text: the painter padded the label out to the
    // full inner width so the selection bar spanned the row, which is what a
    // themed row is.
    gesture(
        row()
            .theme(theme.clone())
            .h(Sizing::Cells(1))
            .child(text(format!(" {marker} {}", opt.label)).theme(theme)),
    )
    .key(radio_key(i))
    .on(
        GestureKind::Press,
        Rc::new(move |e: &Event| {
            if e.button != MouseButton::Left {
                return None;
            }
            e.stop();
            Some(UiMsg::Ui(UiFact::TrustSelect(i)))
        }),
    )
}

/// `[ OK ]` centred in the left half, the secondary button in the right.
///
/// The painter put each at a quarter of the row and subtracted half its own
/// width; two halves that each centre their button say the same thing without
/// the arithmetic, and stay right when a label is localized.
fn buttons(t: &Trust) -> Node<UiMsg> {
    let ink = attrs("ui.popup_text_fg", "ui.popup_bg", &["bold"]);
    let half = |label: String, k: Key, fact: UiFact| {
        row()
            .flex(1)
            .pointer_mode(PointerMode::Transparent)
            .children([
                row().flex(1).pointer_mode(PointerMode::Transparent),
                gesture(text(format!("[ {label} ]")).theme(ink.clone()))
                    .key(k)
                    .on(
                        GestureKind::Press,
                        Rc::new(move |e: &Event| {
                            if e.button != MouseButton::Left {
                                return None;
                            }
                            e.stop();
                            Some(UiMsg::Ui(fact.clone()))
                        }),
                    ),
                row().flex(1).pointer_mode(PointerMode::Transparent),
            ])
    };
    row().h(Sizing::Cells(1)).children([
        half(t.ok_label.clone(), ok_key(), UiFact::TrustConfirm),
        half(
            t.secondary_label.clone(),
            secondary_key(),
            UiFact::TrustSecondary,
        ),
    ])
}

/// The prompt's rectangles, read back off the laid-out tree.
///
/// What `TrustDialogLayout` was: four rects the painter recorded so a hit test
/// could compare a click against them. The TUI's clicks are the nodes' own
/// now, so the only caller left is the web projection — which draws from
/// rectangles, and gets them from the layout that placed the controls.
#[derive(Debug, Clone, Default)]
pub struct Rects {
    pub dialog: ratatui::layout::Rect,
    pub radios: [ratatui::layout::Rect; 3],
    pub ok: ratatui::layout::Rect,
    pub secondary: ratatui::layout::Rect,
}

pub fn rects(ui: &fresh_ui::Ui<UiMsg>, size: ratatui::layout::Rect) -> Option<Rects> {
    let at = |k: Key| super::rect_of(ui, &k, size).unwrap_or_default();
    let dialog = super::rect_of(ui, &key(), size)?;
    Some(Rects {
        dialog,
        radios: [at(radio_key(0)), at(radio_key(1)), at(radio_key(2))],
        ok: at(ok_key()),
        secondary: at(secondary_key()),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::view::shell::fold::{fold_native, Band};
    use crate::view::shell::frame::{frame_tree, Frame};
    use fresh_ui::{Input, Mods, Point, Size, Ui};
    use ratatui::buffer::Buffer;

    fn prompt(selected: usize) -> Trust {
        Trust {
            selected,
            title: "Security Warning".into(),
            can_execute: "This folder can execute code.".into(),
            path_label: "Path:".into(),
            path: "/home/u/projects/fresh".into(),
            detected: Some("Detected: .git, package.json".into()),
            how_proceed: "How would you like to proceed?".into(),
            options: vec![
                Opt {
                    label: "Trust folder (T)".into(),
                    description: "Full tooling, formatters and language servers run.".into(),
                },
                Opt {
                    label: "Restricted (K)".into(),
                    description: "Open read-only; nothing executes.".into(),
                },
                Opt {
                    label: "Block (B)".into(),
                    description: "Do not open this folder at all.".into(),
                },
            ],
            ok_label: "OK".into(),
            secondary_label: "Quit (Ctrl+Q)".into(),
            width: 68,
            max_height: 22,
        }
    }

    fn laid_out(t: Trust, w: u16, h: u16) -> Ui<UiMsg> {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            frame_tree(Frame {
                trust: Some(t),
                ..Frame::default()
            }),
            Size::new(w, h),
        );
        ui
    }

    fn facts(d: fresh_ui::Dispatch<UiMsg>) -> Vec<UiFact> {
        d.msgs
            .into_iter()
            .filter_map(|m| match m {
                UiMsg::Ui(f) => Some(f),
                _ => None,
            })
            .collect()
    }

    /// **The dialog is as tall as its rows, and nothing counts them.**
    ///
    /// The painter built a `Vec<Seg>`, took `segs.len()` for the height, then
    /// walked the same vector again to draw — with a comment saying the count
    /// existed so there was "no fixed height to drift out of sync".
    #[test]
    fn it_centres_at_its_own_width_and_its_rows_height() {
        let ui = laid_out(prompt(1), 100, 40);
        let r = ui.rect_of(ui.find_by_key(&key()).expect("the dialog"));
        assert_eq!(r.w, 68, "its resolved width");
        assert_eq!(r.x, (100 - 68) / 2, "centred");
        assert!(r.h > 10 && r.h <= 22, "sized to its rows, capped: {}", r.h);
        assert_eq!(r.y, (40 - r.h as i32) / 2, "centred vertically");
    }

    /// A radio press selects and does **not** confirm — the ruling the painter
    /// recorded, and the reason it is a two-step at all.
    #[test]
    fn a_radio_press_selects_and_does_not_consent() {
        let mut ui = laid_out(prompt(1), 100, 40);
        let r = ui.rect_of(ui.find_by_key(&radio_key(0)).expect("the first radio"));
        assert_eq!(
            facts(ui.dispatch(Input::press(
                Point::new(r.x + 2, r.y),
                MouseButton::Left,
                Mods::NONE
            ))),
            vec![UiFact::TrustSelect(0)],
            "selecting is not consenting"
        );
    }

    /// The two buttons answer their own presses, where four recorded
    /// rectangles were compared against a click by hand.
    #[test]
    fn the_buttons_answer_their_own_presses() {
        let mut ui = laid_out(prompt(1), 100, 40);
        let ok = ui.rect_of(ui.find_by_key(&ok_key()).expect("ok"));
        let sec = ui.rect_of(ui.find_by_key(&secondary_key()).expect("secondary"));
        assert_eq!(ok.y, sec.y, "one row");
        assert!(ok.x < sec.x, "OK on the left");
        assert_eq!(
            facts(ui.dispatch(Input::press(
                Point::new(ok.x + 1, ok.y),
                MouseButton::Left,
                Mods::NONE
            ))),
            vec![UiFact::TrustConfirm],
        );
        assert_eq!(
            facts(ui.dispatch(Input::press(
                Point::new(sec.x + 1, sec.y),
                MouseButton::Left,
                Mods::NONE
            ))),
            vec![UiFact::TrustSecondary],
        );
    }

    /// **Nothing outside it is interactive.** That is what `capture_mouse`
    /// was: a band ahead of every walk, claiming the whole mouse channel.
    /// `Modality::Exclusive` is the same statement, made by the layer.
    #[test]
    fn a_press_outside_reaches_nothing() {
        let mut ui = laid_out(prompt(1), 100, 40);
        let got = ui.dispatch(Input::press(
            Point::new(2, 1),
            MouseButton::Left,
            Mods::NONE,
        ));
        assert!(facts(got).is_empty(), "nothing outside answers");
    }

    /// It dims the whole frame, which `view::dimming::apply_dimming` did by
    /// hand immediately before painting the dialog.
    #[test]
    fn it_dims_the_frame_behind_it() {
        let ui = laid_out(prompt(1), 100, 40);
        let scrims = ui
            .spec()
            .items
            .iter()
            .filter(|i| matches!(i.draw, fresh_ui::Draw::Scrim(fresh_ui::Scrim::Dim)))
            .count();
        assert_eq!(scrims, 1, "one dim pass over the frame");
    }

    /// The selected option's row carries the selection ink across its whole
    /// width — the painter padded the label out to the inner width to get it.
    #[test]
    fn the_selected_row_is_themed_edge_to_edge() {
        let ui = laid_out(prompt(2), 100, 40);
        let r = ui.rect_of(ui.find_by_key(&radio_key(2)).expect("the third radio"));
        let dialog = ui.rect_of(ui.find_by_key(&key()).expect("the dialog"));
        assert_eq!(r.w, dialog.w - 2, "the full inner width");
        let mut buf = Buffer::empty(ratatui::layout::Rect::new(0, 0, 100, 40));
        let palette =
            |k: &fresh_ui::ThemeKey| crate::view::shell::fold::test_palette::of(k.as_str());
        fold_native(ui.spec(), &mut buf, &palette, Band::Overlay);
        let row: String = (r.x..r.x + r.w as i32)
            .map(|x| buf[(x as u16, r.y as u16)].symbol().to_string())
            .collect();
        assert!(
            row.contains("(*)"),
            "the marker is on the selected row: {row:?}"
        );
    }
}
