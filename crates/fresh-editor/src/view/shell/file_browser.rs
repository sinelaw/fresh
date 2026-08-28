//! The file-open browser dialog.
//!
//! **Its surface, not its content.** The dialog's interior is a painter that
//! records the cell span of every element it lays out — localized labels with
//! live keybinding strings in them, so their widths are not knowable ahead of
//! the render — and the hit tests read those spans back. That seam stays; it is
//! the same one the overlay prompt's toolbar band sits on
//! (`UiFact::CardToolbarPress`), and it is honest: the tree cannot say where a
//! label ends when the painter is the thing that measured it.
//!
//! What moves is everything the tree *can* say. Where the dialog goes — above
//! the prompt line, as wide as it, which the painter wrote as
//! `prompt_area.x` and a comment explaining that this is "right of a left
//! dock, if any". Whether a press is inside it. Whether the wheel over it
//! reaches the buffer underneath. All of which `chrome:file_browser` said as a
//! rectangle recorded during the previous frame's paint — so on the frame that
//! opened the dialog there was no rectangle yet, and the component fell back to
//! a full-frame box to absorb strays until one existed. A description is built
//! before the paint, so there is no first frame to cover for.

use std::rc::Rc;

use fresh_ui::{row, Anchor, Event, Fit, GestureKind, Key, MouseButton, Node, Place, Sizing};

use super::frame::{region_key, HostRegion};
use super::msg::{UiFact, UiMsg};

/// The dialog, as the shell states it.
#[derive(Clone, Debug, PartialEq)]
pub struct Browser {
    /// How tall the dialog is. App state rather than a measurement: the rule
    /// is "the space above the prompt, less the menu bar's row, capped at 20",
    /// and the row it must not cover is a fact about the frame's contents
    /// rather than about the dialog's own.
    pub height: u16,
}

/// The dialog's key, for tests and for the rectangle the painter is given.
pub fn key() -> Key {
    Key::Str("file_browser".into())
}

/// The dialog as a layer above the prompt row, as wide as that row.
///
/// `stretch_to_anchor` is what "anchor to the prompt line's x (right of a left
/// dock, if any) so the picker never overlaps the dock column" was doing by
/// hand: the prompt row already starts right of the dock, and taking its whole
/// extent is one statement rather than an `x` and a `width` that have to agree
/// with it.
pub fn layer(b: &Browser) -> Node<UiMsg> {
    fresh_ui::layer()
        .anchor(Anchor::Node(region_key(HostRegion::PromptLine)))
        .place(Place::Above)
        .stretch_to_anchor()
        .fit(Fit::CLAMP)
        .child(surface().h(Sizing::Cells(b.height)).key(key()))
}

/// The dialog's pointer surface: it paints nothing and claims everything.
///
/// **Everything**, where the box claimed only what its arms answered. A right
/// press and a triple-click fell through to the split underneath, which the
/// component recorded as "the pre-existing quirk, preserved deliberately" — a
/// modal dialog that lets a triple-click select a line of the buffer behind it
/// is not a behaviour worth carrying over.
fn surface() -> Node<UiMsg> {
    fresh_ui::gesture(row())
        .on(
            GestureKind::Press,
            Rc::new(|e: &Event| {
                e.stop();
                let (x, y) = (e.pos.x.max(0) as u16, e.pos.y.max(0) as u16);
                (e.button == MouseButton::Left).then(|| {
                    UiMsg::Ui(UiFact::BrowserPress {
                        x,
                        y,
                        double: e.clicks >= 2,
                    })
                })
            }),
        )
        .on(
            GestureKind::Move,
            Rc::new(|e: &Event| {
                Some(UiMsg::Ui(UiFact::BrowserHover {
                    x: e.pos.x.max(0) as u16,
                    y: e.pos.y.max(0) as u16,
                }))
            }),
        )
        .on(
            GestureKind::Wheel,
            Rc::new(|e: &Event| {
                // Both axes are taken. The list has no horizontal extent, and
                // the component absorbed the sideways delta for exactly this
                // reason: it must not pan the buffer hidden beneath.
                e.stop();
                (e.axis == fresh_ui::Axis::Vertical)
                    .then(|| UiMsg::Ui(UiFact::BrowserScroll(e.delta)))
            }),
        )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::view::shell::frame::{frame_tree, region_key, Frame};
    use fresh_ui::{Input, Mods, Point, Size, Ui};

    fn laid_out(height: u16, dock: Option<u16>, w: u16, h: u16) -> Ui<UiMsg> {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            frame_tree(Frame {
                prompt_line: true,
                dock,
                browser: Some(Browser { height }),
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

    /// **The dialog is the prompt row's width, sitting on top of it.**
    ///
    /// The painter copied the prompt row's `x`, took its `width` from the
    /// chrome area beside it, and subtracted the height back off its `y` —
    /// three numbers that had to agree with a rectangle it already had.
    #[test]
    fn it_sits_above_the_prompt_row_and_is_as_wide_as_it() {
        let ui = laid_out(20, None, 120, 40);
        let prompt = ui.rect_of(
            ui.find_by_key(&region_key(HostRegion::PromptLine))
                .expect("the prompt row"),
        );
        let b = ui.rect_of(ui.find_by_key(&key()).expect("the dialog"));
        assert_eq!((b.x, b.w), (prompt.x, prompt.w), "as wide as the row");
        assert_eq!(b.y + b.h as i32, prompt.y, "directly above it");
        assert_eq!(b.h, 20);
    }

    /// And with a dock it starts right of the column, because the prompt row
    /// does — which is what the copied `x` and its comment were arranging.
    #[test]
    fn a_dock_moves_it_right_because_the_prompt_row_moves_right() {
        let ui = laid_out(20, Some(24), 120, 40);
        let b = ui.rect_of(ui.find_by_key(&key()).expect("the dialog"));
        assert_eq!(b.x, 24, "right of the dock column");
        assert_eq!(b.w, 120 - 24);
    }

    /// A press inside reports where, and a double press says so — the two
    /// arms the component dispatched on `PointerPress`.
    #[test]
    fn a_press_reports_where_and_whether_it_was_a_double() {
        let mut ui = laid_out(20, None, 120, 40);
        let b = ui.rect_of(ui.find_by_key(&key()).expect("the dialog"));
        let at = Point::new(b.x + 12, b.y + 5);
        assert_eq!(
            facts(ui.dispatch(Input::press(at, MouseButton::Left, Mods::NONE))),
            vec![UiFact::BrowserPress {
                x: (b.x + 12) as u16,
                y: (b.y + 5) as u16,
                double: false,
            }],
        );
        let d = ui.dispatch(Input::Press {
            pos: at,
            button: MouseButton::Left,
            mods: Mods::NONE,
            clicks: 2,
        });
        assert_eq!(
            facts(d),
            vec![UiFact::BrowserPress {
                x: (b.x + 12) as u16,
                y: (b.y + 5) as u16,
                double: true,
            }],
        );
    }

    /// **A right press and a triple stop here.** The box was not opaque, so
    /// both fell through to the split beneath — a modal dialog letting a
    /// triple-click select a line of the buffer behind it.
    #[test]
    fn a_right_press_no_longer_falls_through_to_the_buffer() {
        let mut ui = laid_out(20, None, 120, 40);
        let b = ui.rect_of(ui.find_by_key(&key()).expect("the dialog"));
        let got = ui.dispatch(Input::press(
            Point::new(b.x + 12, b.y + 5),
            MouseButton::Right,
            Mods::NONE,
        ));
        assert!(got.claimed, "the dialog takes it");
        assert!(facts(got).is_empty(), "and does nothing with it");
    }

    /// The wheel scrolls the list, and a sideways wheel is absorbed rather
    /// than panning the buffer hidden beneath.
    #[test]
    fn the_wheel_scrolls_the_list_and_the_sideways_one_is_absorbed() {
        let mut ui = laid_out(20, None, 120, 40);
        let b = ui.rect_of(ui.find_by_key(&key()).expect("the dialog"));
        let at = Point::new(b.x + 12, b.y + 5);
        let down = ui.dispatch(Input::Wheel {
            pos: at,
            delta: 3,
            axis: fresh_ui::Axis::Vertical,
            mods: Mods::NONE,
        });
        assert!(down.claimed);
        assert_eq!(facts(down), vec![UiFact::BrowserScroll(3)]);
        let side = ui.dispatch(Input::Wheel {
            pos: at,
            delta: 3,
            axis: fresh_ui::Axis::Horizontal,
            mods: Mods::NONE,
        });
        assert!(side.claimed, "absorbed");
        assert!(facts(side).is_empty(), "and it means nothing here");
    }
}
