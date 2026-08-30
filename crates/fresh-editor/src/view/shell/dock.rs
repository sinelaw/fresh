//! The left dock column.
//!
//! The column's *content* is a plugin's `WidgetSpec`, laid out by the widget
//! runtime, so the region is still a `Host` leaf — that translation is the M6
//! wave and a plugin-API change with it. What moves here is everything around
//! the content: the column's own pointer surface, and the width grip on its
//! right edge.
//!
//! **The grip is a node, not a rectangle.** It was `chrome:dock_border`, a box
//! pushed *before* `chrome:dock_column` so that within a band the earlier push
//! won — a rule about the builder's insertion order standing in for "the grip
//! is on top of the column". Here it is on top of the column, because it is
//! declared after it in a [`stack`].
//!
//! **The blur observer is a capture-phase listener on the frame**, not a
//! full-frame box ranked above every consuming surface. Same contract — act,
//! then let the press go on to whatever it was aimed at — and it now holds for
//! presses the tree claims. It had stopped: the shell is offered the pointer
//! before the chrome walk, so once the menu bar, status bar, explorer and
//! popups became nodes, clicking any of them left a focused dock focused.
//!
//! What is *not* here: the dock's keys (`chrome::Dock::on_layer_key`, which
//! rides the layer walk) and its scrollbar-reveal hover, which reads zones the
//! plugin publishes from inside the panel. Both belong with the content.

use std::rc::Rc;

use fresh_ui::{
    gesture, host, row, stack, Event, GestureKind, Key, MouseButton, Node, PointerMode, Sizing,
};

use super::frame::HostRegion;
use super::msg::{UiFact, UiMsg};

/// The grip's key, for tests and for callers that want its rectangle.
pub fn grip_key() -> Key {
    Key::Str("dock_grip".into())
}

/// The column: the panel's `Host` leaf under a pointer surface, with the width
/// grip on top of its right edge.
pub fn dock() -> Node<UiMsg> {
    stack().children([column(), grip_strip()])
}

/// The panel's own pointer surface.
///
/// Left and right presses report *where*, in screen columns and rows — which
/// for a column pinned to `x = 0` is also the panel's own space. That is all
/// the tree can say about a press until the panel's widgets are nodes: the
/// runtime hit-tests its own boxes, the same seam the overlay prompt's toolbar
/// band sits on (`UiFact::CardToolbarPress`).
fn column() -> Node<UiMsg> {
    gesture(host(HostRegion::Dock.id()))
        .on(
            GestureKind::Press,
            Rc::new(|e: &Event| {
                let x = e.pos.x.max(0) as u16;
                let y = e.pos.y.max(0) as u16;
                match e.button {
                    MouseButton::Left => {
                        e.stop();
                        Some(UiMsg::Ui(UiFact::DockPress { x, y }))
                    }
                    MouseButton::Right => {
                        e.stop();
                        Some(UiMsg::Ui(UiFact::DockContext { x, y }))
                    }
                    _ => None,
                }
            }),
        )
        .on(
            GestureKind::Wheel,
            Rc::new(|e: &Event| {
                // The column takes the wheel whether or not the panel finds
                // something to scroll with it. The old arm passed a declined
                // wheel on down the walk, where nothing under a dock column
                // could answer it either — so this is the same outcome, said
                // once, and it keeps a wheel over the dock from ever reaching
                // the buffer beside it.
                e.stop();
                Some(UiMsg::Ui(UiFact::DockScroll {
                    delta: e.delta,
                    x: e.pos.x.max(0) as u16,
                    y: e.pos.y.max(0) as u16,
                }))
            }),
        )
}

/// One transparent column of the panel's width with the grip in its last cell.
///
/// The grip paints nothing: the column's right border is the legacy painter's,
/// and this only claims the pointer that lands on it — and then keeps it,
/// through every move and the release, because a resize drag leaves the grip's
/// own cell on its first step.
fn grip_strip() -> Node<UiMsg> {
    // The width and the key go on the OUTSIDE, on the gesture node `draggable`
    // returns: it is the node that hit-tests, and an unconstrained one would
    // stretch across the whole strip and swallow presses meant for the panel
    // beside it.
    let grip = super::grip::draggable(
        super::msg::Grip::DockWidth,
        row(),
        Rc::new(|_: &Event| Some(UiMsg::Ui(UiFact::DockResizeBegin))),
    )
    .w(Sizing::Cells(1))
    .key(grip_key());
    row()
        .pointer_mode(PointerMode::Transparent)
        .children([row().flex(1).pointer_mode(PointerMode::Transparent), grip])
}

/// Blur the dock when a press lands outside its column.
///
/// Wraps the whole frame and listens in the **capture** phase, so it runs
/// before anything the press is actually aimed at and — because it never stops
/// the flow — leaves that press to go on and do its job. "Act, and claim only
/// when the act was the whole of it": the rule the wheel and the pass-through
/// dismissals already follow.
///
/// The width is the only thing "outside" needs, and the description has it.
/// Whether the dock is *focused* is app state, and stays with the applier.
pub fn blur_observer(width: u16, frame: Node<UiMsg>) -> Node<UiMsg> {
    gesture(frame).on_capture(
        GestureKind::Press,
        Rc::new(move |e: &Event| {
            (e.button == MouseButton::Left && e.pos.x >= width as i32)
                .then_some(UiMsg::Ui(UiFact::DockBlur))
        }),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::view::shell::frame::{frame_tree, region_key, Frame};
    use fresh_ui::{Input, Mods, Point, Size, Ui};

    fn laid_out(dock: Option<u16>, w: u16, h: u16) -> Ui<UiMsg> {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            frame_tree(Frame {
                menu_bar: false,
                status_bar: false,
                dock,
                ..Frame::default()
            }),
            Size::new(w, h),
        );
        ui
    }

    fn press(ui: &mut Ui<UiMsg>, x: u16, y: u16, b: MouseButton) -> Vec<UiFact> {
        ui.dispatch(Input::press(Point::new(x as i32, y as i32), b, Mods::NONE))
            .msgs
            .into_iter()
            .filter_map(|m| match m {
                UiMsg::Ui(f) => Some(f),
                _ => None,
            })
            .collect()
    }

    /// The grip is the column's last cell, top to bottom.
    ///
    /// It was a `LayoutBox` at `(0, width - 1, 1, frame_height)`, arithmetic
    /// the builder did by hand from the placement's `width_cols`. Here it is
    /// where the layout puts it, which is the same place — and stays there
    /// when the column's width comes from somewhere else.
    #[test]
    fn the_grip_is_the_columns_last_cell_full_height() {
        let ui = laid_out(Some(24), 100, 30);
        let grip = ui.rect_of(ui.find_by_key(&grip_key()).expect("the grip"));
        assert_eq!((grip.x, grip.w), (23, 1), "the column's last cell");
        assert_eq!((grip.y, grip.h), (0, 30), "top to bottom");
    }

    /// A press on that cell starts the width drag rather than reaching the
    /// panel — the ordering `chrome:dock_border` got from being pushed first.
    #[test]
    fn the_grip_outranks_the_column_it_sits_on() {
        let mut ui = laid_out(Some(24), 100, 30);
        assert_eq!(
            press(&mut ui, 23, 10, MouseButton::Left),
            vec![UiFact::DockResizeBegin],
            "the grip's cell is the grip's"
        );
        // …and it *keeps* the pointer until the button comes up, which is what
        // a resize drag needs and what the next press has to be clear of.
        ui.dispatch(Input::release(
            Point::new(23, 10),
            MouseButton::Left,
            Mods::NONE,
        ));
        assert_eq!(
            press(&mut ui, 22, 10, MouseButton::Left),
            vec![UiFact::DockPress { x: 22, y: 10 }],
            "the cell beside it is the panel's"
        );
    }

    /// A right-press in the column asks the plugin for its session menu, and
    /// says where — the panel hit-tests its own widget boxes with that.
    #[test]
    fn a_right_press_in_the_column_reports_where() {
        let mut ui = laid_out(Some(24), 100, 30);
        // The frame-wide right-click observer fires wherever the click lands
        // (`shell::splits::tab_menu_guard`); what the column says is the rest.
        let said: Vec<_> = press(&mut ui, 5, 7, MouseButton::Right)
            .into_iter()
            .filter(|f| *f != UiFact::ClearTabMenus)
            .collect();
        assert_eq!(said, vec![UiFact::DockContext { x: 5, y: 7 }]);
    }

    /// **A press the tree claims still blurs the dock.**
    ///
    /// `chrome:dock_blur` was a full-frame box ranked above every consuming
    /// surface, in a walk the shell runs *before*. So once the surfaces
    /// beside the dock became nodes, a press on one of them claimed the event
    /// and the blur never ran. The observer is in the tree now, in the
    /// capture phase, and it fires whatever the press goes on to do.
    #[test]
    fn a_press_outside_the_column_blurs_and_does_not_claim() {
        let mut ui = laid_out(Some(24), 100, 30);
        let got = ui.dispatch(Input::press(
            Point::new(50, 10),
            MouseButton::Left,
            Mods::NONE,
        ));
        assert!(
            got.msgs
                .iter()
                .any(|m| matches!(m, UiMsg::Ui(UiFact::DockBlur))),
            "outside the column, got {:?}",
            got.msgs
        );
        assert!(!got.claimed, "the press goes on to what it was aimed at");
    }

    /// And a press *inside* it does not blur — the whole point of the column.
    #[test]
    fn a_press_inside_the_column_does_not_blur() {
        let mut ui = laid_out(Some(24), 100, 30);
        let got = press(&mut ui, 5, 10, MouseButton::Left);
        assert_eq!(got, vec![UiFact::DockPress { x: 5, y: 10 }]);
    }

    /// A hidden dock is still a region with a rectangle, and carries neither
    /// a grip nor an observer.
    #[test]
    fn no_dock_means_no_grip_and_no_blur() {
        let mut ui = laid_out(None, 100, 30);
        assert!(ui.find_by_key(&grip_key()).is_none(), "no grip");
        assert!(
            ui.find_by_key(&region_key(HostRegion::Dock)).is_some(),
            "the region still reports a rectangle"
        );
        assert!(
            press(&mut ui, 50, 10, MouseButton::Left).is_empty(),
            "nothing to blur"
        );
    }
}
