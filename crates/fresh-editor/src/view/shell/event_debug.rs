//! The event-debug dialog.
//!
//! Structurally the calibration wizard's twin, and migrated for the same
//! reason: no mouse, no recorded rectangles, three bands split by hand out of
//! a rectangle the painter centred itself. What is different is that its
//! middle band is a *history* — newest first, the newest one marked — and it
//! could overrun the band it was drawn into with no way to see the rest. It is
//! a `viewport` with a bar now, so a long history scrolls instead of being
//! silently cut.

use fresh_ui::{
    col, layout_reader, row, text, Align, Anchor, LayoutInfo, Modality, Node, Place, Scrim, Sizing,
};

use crate::app::shell_host::shell_theme::{attrs, pair};

use super::msg::UiMsg;

pub const DIALOG_WIDTH: u16 = 70;
pub const DIALOG_HEIGHT: u16 = 18;

/// One recorded event, as the dialog shows it.
#[derive(Clone, Debug, PartialEq)]
pub struct Event {
    pub description: String,
    /// The form keybinding lookup matches against, when normalising changed
    /// the key. `None` when it did not, which is what the painter's `if let`
    /// said.
    pub normalized: Option<String>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct EventDebug {
    pub title: String,
    pub instructions: String,
    pub help_text: String,
    /// The heading over the history, with its count already formatted. `None`
    /// when there is nothing recorded — then `empty` is shown instead.
    pub recent_label: Option<String>,
    pub empty_label: String,
    pub history: Vec<Event>,
    pub controls: Vec<(String, String)>,
    /// The raw form of the most recent event, when there is one.
    pub details: Option<String>,
    pub width: u16,
    pub height: u16,
}

pub fn key() -> fresh_ui::Key {
    fresh_ui::Key::Str("event_debug_dialog".into())
}

/// The dialog as a layer: centred, dimming, and exclusive.
pub fn layer(d: &EventDebug) -> Node<UiMsg> {
    fresh_ui::layer()
        .anchor(Anchor::Screen(Align::Center))
        .place(Place::Over)
        .modality(Modality::Exclusive)
        .scrim(Some(Scrim::Dim))
        .child(
            dialog(d)
                .w(Sizing::Cells(d.width))
                .h(Sizing::Cells(d.height))
                .key(key()),
        )
}

fn dialog(d: &EventDebug) -> Node<UiMsg> {
    let ring = pair("ui.popup_border_fg", "ui.popup_bg");
    let ink = pair("ui.popup_text_fg", "ui.popup_bg");
    let dim = pair("editor.line_number_fg", "ui.popup_bg");

    let mut history: Vec<Node<UiMsg>> = Vec::new();
    match &d.recent_label {
        None => history.push(line(d.empty_label.clone(), dim.clone())),
        Some(label) => {
            history.push(line(
                label.clone(),
                attrs("ui.help_key_fg", "ui.popup_bg", &["bold"]),
            ));
            history.push(blank());
            for (i, e) in d.history.iter().enumerate() {
                let newest = i == 0;
                let style = match newest {
                    true => attrs("diagnostic.info_fg", "ui.popup_bg", &["bold"]),
                    false => ink.clone(),
                };
                let mut spans: Vec<Node<UiMsg>> = vec![
                    text(match newest {
                        true => "> ",
                        false => "  ",
                    })
                    .theme(style.clone()),
                    text("Raw: ").theme(dim.clone()),
                    text(e.description.clone()).theme(style.clone()),
                ];
                if let Some(n) = &e.normalized {
                    spans.push(text("    (Normalized: ").theme(dim.clone()));
                    spans.push(text(n.clone()).theme(pair("diagnostic.hint_fg", "ui.popup_bg")));
                    spans.push(text(")").theme(dim.clone()));
                }
                history.push(row().h(Sizing::Cells(1)).children(spans));
            }
        }
    }

    let mut controls: Vec<Node<UiMsg>> = Vec::new();
    for (k, label) in &d.controls {
        controls.push(text(format!("[{k}]")).theme(pair("ui.help_key_fg", "ui.popup_bg")));
        controls.push(text(format!(" {label}  ")).theme(ink.clone()));
    }

    let mut footer: Vec<Node<UiMsg>> = vec![row().h(Sizing::Cells(1)).children(controls)];
    if let Some(details) = &d.details {
        footer.push(blank());
        footer.push(text(details.clone()).wrap().theme(dim.clone()));
    }

    col().theme(ring).border().children([
        line(
            d.title.clone(),
            attrs("ui.popup_border_fg", "ui.popup_bg", &["bold"]),
        ),
        col().h(Sizing::Cells(3)).children([
            line(
                d.instructions.clone(),
                attrs("ui.popup_text_fg", "ui.popup_bg", &["bold"]),
            ),
            text(d.help_text.clone()).wrap().theme(ink.clone()),
        ]),
        // **The history scrolls.** It was a `Paragraph` in a `Min(8)` band: a
        // history longer than the band was drawn past its bottom edge and
        // clipped, with nothing to say there was more.
        fresh_ui::viewport(col().children(history))
            .scrollbar()
            .flex(1),
        col().h(Sizing::Cells(4)).children(footer),
    ])
}

fn line(s: String, theme: String) -> Node<UiMsg> {
    text(s).theme(theme).h(Sizing::Cells(1))
}

fn blank() -> Node<UiMsg> {
    row().h(Sizing::Cells(1))
}

/// The dialog, sized against whatever it is placed in — 70×18 unless the
/// frame is too small, and then what is left with two columns and two rows to
/// spare, which is the painter's own `min(area - 4)`.
pub fn sized(d: &EventDebug) -> Node<UiMsg> {
    let d = d.clone();
    layout_reader(move |info: LayoutInfo| {
        layer(&EventDebug {
            width: DIALOG_WIDTH.min(info.constraints.max_w.saturating_sub(4)),
            height: DIALOG_HEIGHT.min(info.constraints.max_h.saturating_sub(4)),
            ..d.clone()
        })
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::view::shell::frame::{frame_tree, Frame};
    use fresh_ui::{Size, Ui};

    fn dialog_of(n: usize) -> EventDebug {
        EventDebug {
            title: "Event debug".into(),
            instructions: "Press any key".into(),
            help_text: "It will be recorded".into(),
            recent_label: (n > 0).then(|| format!("Recent events ({n})")),
            empty_label: "No events yet".into(),
            history: (0..n)
                .map(|i| Event {
                    description: format!("Key({i})"),
                    normalized: (i % 2 == 0).then(|| format!("Alt+Shift+{i}")),
                })
                .collect(),
            controls: vec![("q".into(), "Close".into()), ("c".into(), "Clear".into())],
            details: Some("raw bytes".into()),
            width: DIALOG_WIDTH,
            height: DIALOG_HEIGHT,
        }
    }

    fn laid_out(d: EventDebug, w: u16, h: u16) -> Ui<UiMsg> {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            frame_tree(Frame {
                event_debug: Some(d),
                ..Frame::default()
            }),
            Size::new(w, h),
        );
        ui
    }

    fn rows(ui: &Ui<UiMsg>) -> Vec<String> {
        let mut pieces: Vec<(i32, i32, String)> = Vec::new();
        for item in ui.spec().layers() {
            if let fresh_ui::Draw::Lines(lines) = &item.draw {
                for (i, l) in lines.iter().enumerate() {
                    pieces.push((item.rect.y + i as i32, item.rect.x, l.to_string()));
                }
            }
        }
        pieces.sort_by_key(|(y, x, _)| (*y, *x));
        let mut out: Vec<String> = Vec::new();
        let mut at: Option<i32> = None;
        for (y, _, s) in pieces {
            match at {
                Some(prev) if prev == y => out.last_mut().unwrap().push_str(&s),
                _ => {
                    out.push(s);
                    at = Some(y);
                }
            }
        }
        out
    }

    /// Centred at 70×18, and clamped to a frame too small for it.
    #[test]
    fn the_dialog_is_centred_at_its_documented_size() {
        let ui = laid_out(dialog_of(3), 120, 40);
        let r = ui.rect_of(ui.find_by_key(&key()).expect("the dialog"));
        assert_eq!((r.w, r.h), (DIALOG_WIDTH, DIALOG_HEIGHT));
        assert_eq!(r.x, (120 - DIALOG_WIDTH as i32) / 2);

        let ui = laid_out(dialog_of(3), 50, 14);
        let r = ui.rect_of(ui.find_by_key(&key()).expect("the dialog"));
        assert_eq!((r.w, r.h), (46, 10));
    }

    /// The newest event is marked and comes first, and a normalised form is
    /// shown only when normalising changed the key.
    #[test]
    fn the_newest_event_leads_and_only_changed_keys_show_a_normalised_form() {
        let ui = laid_out(dialog_of(3), 120, 40);
        let painted = rows(&ui);
        let newest = painted
            .iter()
            .find(|r| r.contains("Key(0)"))
            .expect("the newest event");
        assert!(newest.starts_with("> "), "marked: {newest:?}");
        assert!(newest.contains("Normalized:"), "and it changed: {newest:?}");
        let second = painted
            .iter()
            .find(|r| r.contains("Key(1)"))
            .expect("the one before it");
        assert!(!second.contains("Normalized:"), "this one did not");
    }

    /// **A long history scrolls.** It was a `Paragraph` in a fixed band: past
    /// the band's bottom edge it was clipped, with nothing to say there was
    /// more.
    #[test]
    fn a_long_history_gets_a_bar() {
        let bar = |n: usize| {
            let ui = laid_out(dialog_of(n), 120, 40);
            ui.spec()
                .items
                .iter()
                .any(|i| matches!(i.draw, fresh_ui::Draw::Scrollbar { .. }))
        };
        assert!(!bar(2), "a short history needs no bar");
        assert!(bar(40), "a long one does");
    }

    /// An empty history says so.
    #[test]
    fn an_empty_history_says_so() {
        let mut d = dialog_of(0);
        d.details = None;
        let ui = laid_out(d, 120, 40);
        assert!(
            rows(&ui).iter().any(|r| r.contains("No events yet")),
            "{:?}",
            rows(&ui)
        );
    }

    /// Nothing behind it is interactive.
    #[test]
    fn nothing_behind_the_dialog_takes_a_press() {
        let mut ui = laid_out(dialog_of(3), 120, 40);
        let got = ui.dispatch(fresh_ui::Input::press(
            fresh_ui::Point::new(2, 2),
            fresh_ui::MouseButton::Left,
            fresh_ui::Mods::NONE,
        ));
        assert!(got.msgs.is_empty(), "{:?}", got.msgs);
    }
}
