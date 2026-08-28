//! The theme inspector's popup (Ctrl+Right-Click).
//!
//! A debug instrument, and it had the migration's signature defect in the
//! plainest form: `render_theme_info_popup` built the lines and drew them,
//! `theme_info_popup_rect` counted the same lines again to say where the box
//! was, and a comment on the second one — "must match render_theme_info_popup
//! logic" — was the whole of what kept them agreeing. Every `if` in the
//! builder had a matching `line_count +=` in the counter, including the one
//! that decides whether there is a button to click.
//!
//! Here the lines are built once and the box is as tall as they are, so there
//! is no second derivation to keep in step. The button's row is a node: it
//! reports its own hover and answers its own press, where the counter used to
//! hand back a row offset for `chrome::ThemeInfo` to compare a `row` against.

use std::rc::Rc;

use fresh_ui::{
    col, row, text, text_runs, Anchor, Dismiss, Event, Fit, GestureKind, Key, MouseButton, Node,
    Place, PointerMode, Sizing,
};

use crate::app::shell_host::shell_theme::pair;
use crate::view::markdown::StyledLine;

use super::msg::{UiFact, UiMsg};
use super::popup::{absorb, styled_runs};

/// The inspector's fixed width. Its content is key names, which are long and
/// uniform; nothing here wants to be measured.
pub const WIDTH: u16 = 40;

/// What the inspector shows, and where.
#[derive(Clone, Debug, PartialEq)]
pub struct ThemeInfo {
    /// The popup's top-left when it fits — already offset down-right of the
    /// inspected cell by `show_theme_info_popup`.
    pub at: (u16, u16),
    /// One styled line per row of the body. The colours in them are the
    /// inspected cell's *own*, so they are literals rather than theme keys —
    /// which is what a colour swatch is for.
    pub lines: Vec<StyledLine>,
    /// The action row, when the inspected cell had a key the theme editor
    /// could open. A cell with no recorded key shows the message in `lines`
    /// and no button, rather than one that silently does nothing.
    pub button: Option<Button>,
}

/// The "Open in Theme Editor" row.
#[derive(Clone, Debug, PartialEq)]
pub struct Button {
    pub label: String,
    pub hovered: bool,
}

/// The popup's key, for tests and for callers that want its rectangle.
pub fn key() -> Key {
    Key::Str("theme_info".into())
}

/// The action row's key.
pub fn button_key() -> Key {
    Key::Str("theme_info_button".into())
}

/// The inspector as a layer.
///
/// **Dismissed by a press outside it or by any key, and both go on.** That is
/// what `chrome:theme_info_guard` returned `PassAfter` for and what
/// `ThemeInfo::on_key` said by dismissing and then returning `None`: the
/// inspector is in the way of nothing, so getting rid of it should not cost
/// the user the click or the keystroke that did it.
pub fn layer(t: &ThemeInfo) -> Node<UiMsg> {
    fresh_ui::layer()
        .anchor(Anchor::Point(t.at.0, t.at.1))
        .place(Place::Over)
        .fit(Fit::CLAMP)
        .dismiss(
            Dismiss {
                outside_pointer: true,
                any_key: true,
                ..Dismiss::NONE
            }
            .passing_through(),
        )
        .on_dismiss(|_| UiMsg::Ui(UiFact::ThemeInfoDismiss))
        .child(popup(t).w(Sizing::Cells(WIDTH)).key(key()))
}

fn popup(t: &ThemeInfo) -> Node<UiMsg> {
    let mut rows: Vec<Node<UiMsg>> = t
        .lines
        .iter()
        .map(|l| text_runs(styled_runs(l)).h(Sizing::Cells(1)))
        .collect();
    if let Some(b) = &t.button {
        rows.push(button_row(b));
    }
    // Absorbing inside the frame, for the reason `popup::body` states: the
    // title strip's path does not reach the content, so an absorb around the
    // outside would claim on that path before the content's was offered.
    let framed = absorb(super::popup::frame(true, col().children(rows)));
    fresh_ui::stack().children([framed, title_strip()])
}

/// The row that opens the theme editor on the key it is showing.
///
/// Its highlight is its own hover, reported by the tree. The component read it
/// off `mouse_state.hover_target`, which the chrome hover walk set from a row
/// offset the *counter* produced — three places agreeing about one row.
fn button_row(b: &Button) -> Node<UiMsg> {
    let theme = match b.hovered {
        true => pair("ui.popup_selection_fg", "ui.popup_selection_bg"),
        false => pair("ui.popup_text_fg", "ui.popup_bg"),
    };
    fresh_ui::gesture(text(b.label.clone()).theme(theme))
        .h(Sizing::Cells(1))
        .key(button_key())
        .on(
            GestureKind::Press,
            Rc::new(|e: &Event| {
                if e.button != MouseButton::Left {
                    return None;
                }
                e.stop();
                Some(UiMsg::Ui(UiFact::ThemeInfoOpenEditor))
            }),
        )
        .on_enter(Rc::new(|_: &Event| {
            Some(UiMsg::Ui(UiFact::ThemeInfoButtonHover(true)))
        }))
        .on_leave(Rc::new(|_: &Event| {
            Some(UiMsg::Ui(UiFact::ThemeInfoButtonHover(false)))
        }))
}

/// The title on the top border, where `Block::title` put it. Decoration to the
/// last cell — there is no close button here, so the whole strip is inert.
///
/// **One row tall, and nothing below it.** `popup::border_strip` ends in a
/// `flex(1)` filler so its `Ignore` covers the rest of the popup; that filler
/// makes the strip greedy, and a stack is as big as its biggest child — which
/// is invisible there only because the popups' layer is given an explicit
/// height. This one is measured, so a greedy strip would make it the whole
/// frame. A strip that stops after its row leaves the content plainly the
/// thing behind it, which is what the filler was arranging anyway.
fn title_strip() -> Node<UiMsg> {
    let ring = pair("ui.popup_border_fg", "ui.popup_bg");
    col().pointer_mode(PointerMode::Transparent).children([row()
        .h(Sizing::Cells(1))
        .pointer_mode(PointerMode::Transparent)
        .children([
            row()
                .w(Sizing::Cells(1))
                .pointer_mode(PointerMode::Transparent),
            text(" Theme Info ")
                .theme(ring)
                .pointer_mode(PointerMode::Transparent),
            row().flex(1).pointer_mode(PointerMode::Transparent),
        ])])
}

/// Ctrl+Right-Click anywhere opens the inspector on the cell under the pointer.
///
/// A capture-phase listener on the frame, for the same reason the box it
/// replaces was full-frame at the very top of the bands: the instrument's whole
/// point is to inspect the cell under *any* chrome, so it has to see the press
/// before the chrome does. Unlike the dock's blur observer this one stops the
/// flow — a Ctrl+Right-Click is the gesture, not a side effect of one.
///
/// One surface used to be exempt: the box sat at `z = 190`, below the overlay
/// prompt's right-click guard at 195, so Ctrl+Right-Click over an open overlay
/// inspected nothing. That guard is gone with the prompt's boxes, and the
/// exemption goes with it rather than being restated — "under any chrome"
/// reads better with no exceptions than with one.
pub fn inspect_trigger(frame: Node<UiMsg>) -> Node<UiMsg> {
    fresh_ui::gesture(frame).on_capture(
        GestureKind::Press,
        Rc::new(|e: &Event| {
            if e.button != MouseButton::Right || !e.mods.ctrl {
                return None;
            }
            e.stop();
            Some(UiMsg::Ui(UiFact::ThemeInspect {
                x: e.pos.x.max(0) as u16,
                y: e.pos.y.max(0) as u16,
            }))
        }),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::view::markdown::StyledSpan;
    use crate::view::shell::fold::{fold_native, Band};
    use crate::view::shell::frame::{frame_tree, Frame};
    use fresh_ui::{Input, KeyCode, KeyPress, Mods, Point, Size, Ui};
    use ratatui::buffer::Buffer;
    use ratatui::style::Style;

    fn line(s: &str) -> StyledLine {
        StyledLine {
            spans: vec![StyledSpan {
                text: s.to_string(),
                style: Style::default(),
                link_url: None,
            }],
        }
    }

    fn info(lines: &[&str], button: bool) -> ThemeInfo {
        ThemeInfo {
            at: (10, 4),
            lines: lines.iter().map(|s| line(s)).collect(),
            button: button.then(|| Button {
                label: " ▶ Open in Theme Editor ".to_string(),
                hovered: false,
            }),
        }
    }

    fn laid_out(t: ThemeInfo, w: u16, h: u16) -> Ui<UiMsg> {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            frame_tree(Frame {
                menu_bar: false,
                status_bar: false,
                theme_info: Some(t),
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

    /// **The box is as tall as its content, and nothing counts the lines.**
    ///
    /// `theme_info_popup_rect` re-walked every `if` in the painter with a
    /// `line_count` to answer this, under a comment saying the two had to
    /// match. Three lines and a button is four rows plus the ring.
    #[test]
    fn the_box_is_as_tall_as_the_lines_it_was_given() {
        let ui = laid_out(info(&["a", "b", "c"], true), 80, 24);
        let r = ui.rect_of(ui.find_by_key(&key()).expect("the popup"));
        assert_eq!((r.x, r.y), (10, 4), "at the position it was given");
        assert_eq!(r.w, WIDTH);
        assert_eq!(r.h, 6, "three lines, a button, and two border rows");
    }

    /// And a body with no button is a row shorter — the case the counter had
    /// its own branch for.
    #[test]
    fn a_body_with_no_button_is_a_row_shorter() {
        let ui = laid_out(info(&["a", "b", "c"], false), 80, 24);
        let r = ui.rect_of(ui.find_by_key(&key()).expect("the popup"));
        assert_eq!(r.h, 5);
    }

    /// The action row answers its own press, where a row offset used to be
    /// handed back for a `row ==` comparison to make.
    #[test]
    fn the_action_row_answers_its_own_press() {
        let mut ui = laid_out(info(&["a", "b"], true), 80, 24);
        let b = ui.rect_of(ui.find_by_key(&button_key()).expect("the button"));
        assert_eq!(
            facts(ui.dispatch(Input::press(
                Point::new(b.x + 2, b.y),
                MouseButton::Left,
                Mods::NONE
            ))),
            vec![UiFact::ThemeInfoOpenEditor],
        );
    }

    /// A press elsewhere inside the popup dies there rather than reaching the
    /// buffer underneath — `chrome:theme_info`'s `pointer_opaque`.
    #[test]
    fn a_press_elsewhere_inside_is_absorbed() {
        let mut ui = laid_out(info(&["a", "b"], true), 80, 24);
        let got = ui.dispatch(Input::press(
            Point::new(12, 6),
            MouseButton::Left,
            Mods::NONE,
        ));
        assert!(got.claimed, "the popup takes it");
        assert!(facts(got).is_empty(), "and does nothing with it");
    }

    /// **Dismissed by a press outside or by any key, and both go on.**
    ///
    /// The guard returned `PassAfter` and `on_key` returned `None`; both said
    /// the same thing, that getting rid of the inspector should not cost the
    /// user the input that did it.
    #[test]
    fn either_input_dismisses_it_and_neither_is_spent() {
        let mut ui = laid_out(info(&["a"], true), 80, 24);
        let got = ui.dispatch(Input::press(
            Point::new(60, 20),
            MouseButton::Left,
            Mods::NONE,
        ));
        assert!(!got.claimed, "the press is still going somewhere");
        assert_eq!(facts(got), vec![UiFact::ThemeInfoDismiss]);

        let mut ui = laid_out(info(&["a"], true), 80, 24);
        let got = ui.dispatch(Input::Key(KeyPress::with(KeyCode::Char('j'), Mods::NONE)));
        assert!(!got.claimed, "and so is the keystroke");
        assert_eq!(facts(got), vec![UiFact::ThemeInfoDismiss]);
    }

    /// Ctrl+Right-Click anywhere asks for the cell under the pointer, and it
    /// *is* the gesture — nothing below sees it.
    #[test]
    fn ctrl_right_click_inspects_the_cell_under_it() {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            frame_tree(Frame {
                menu_bar: false,
                status_bar: false,
                ..Frame::default()
            }),
            Size::new(80, 24),
        );
        let got = ui.dispatch(Input::press(
            Point::new(7, 9),
            MouseButton::Right,
            Mods::CTRL,
        ));
        assert!(got.claimed);
        assert_eq!(facts(got), vec![UiFact::ThemeInspect { x: 7, y: 9 }]);
    }

    /// A plain right-click is not the trigger, and passes.
    #[test]
    fn a_plain_right_click_is_not_the_trigger() {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            frame_tree(Frame {
                menu_bar: false,
                status_bar: false,
                ..Frame::default()
            }),
            Size::new(80, 24),
        );
        let got = ui.dispatch(Input::press(
            Point::new(7, 9),
            MouseButton::Right,
            Mods::NONE,
        ));
        assert!(facts(got).is_empty());
    }

    /// The title sits on the top border where `Block::title` put it.
    #[test]
    fn the_title_is_on_the_top_border() {
        let ui = laid_out(info(&["a"], false), 80, 24);
        let mut buf = Buffer::empty(ratatui::layout::Rect::new(0, 0, 80, 24));
        let palette =
            |k: &fresh_ui::ThemeKey| crate::view::shell::fold::test_palette::of(k.as_str());
        fold_native(ui.spec(), &mut buf, &palette, Band::Overlay);
        let row: String = (10..40).map(|x| buf[(x, 4)].symbol().to_string()).collect();
        assert!(row.contains("Theme Info"), "got {row:?}");
    }
}
