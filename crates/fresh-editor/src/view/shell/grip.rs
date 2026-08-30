//! A grip: a strip you pull to resize something.
//!
//! Three surfaces have one — the dock's right edge, a split separator, the
//! file explorer's right edge — and all three drove their drag the same way:
//! the press set a flag on the `Editor`, and a central ladder
//! (`chrome::pointer_grab`) read every flag on every subsequent event to
//! decide where it went. That ladder is a hand-rolled pointer capture,
//! ordered by hand and documented as "checked in the old drag ladder's order
//! so precedence is unchanged when (rarely) two flags coexist" — which is the
//! tell: a mechanism that needs a tie-break between things that cannot
//! legitimately be true at once.
//!
//! The library has the real thing. A node that calls `capture_pointer` on its
//! press keeps every move and the release, wherever the pointer travels,
//! until it lets go — so there is nothing to rank, and a drag that leaves the
//! grip's cell (which every resize does immediately) still arrives at the
//! grip.
//!
//! **What stays app-side is the state, not the routing.** Whether a drag is
//! in progress, and what it is dragging, is a fact about the editor and the
//! applier keeps it; the grip's `Move` fires on a bare hover too, and it is
//! that state which says whether the move means anything. What goes is the
//! ladder.

use std::rc::Rc;

use fresh_ui::{gesture, Event, GestureKind, MouseButton, Node};

use super::msg::{Grip, UiFact, UiMsg};

/// Wrap a grip's node so that pressing it captures the pointer and every move
/// and the release come back to it.
///
/// `press` is the surface's own — the three differ in what they have to say
/// when a drag starts (a separator names the container it divides; the two
/// width grips have only the fact that they were pressed) and none of that
/// belongs here.
///
/// **Size and key the result, not `n`.** What comes back is a gesture node
/// wrapping `n`, and the gesture node is the one that hit-tests and the one a
/// key names. Left unconstrained it stretches to its parent's bounds with the
/// one-cell child parked inside it, and takes every press in that parent.
pub fn draggable(
    which: Grip,
    n: Node<UiMsg>,
    press: Rc<dyn Fn(&Event) -> Option<UiMsg>>,
) -> Node<UiMsg> {
    gesture(n)
        .on(
            GestureKind::Press,
            Rc::new(move |e: &Event| {
                if e.button != MouseButton::Left {
                    return None;
                }
                // The whole drag mechanism, in one call.
                e.capture_pointer();
                e.stop();
                press(e)
            }),
        )
        .on(
            GestureKind::Move,
            Rc::new(move |e: &Event| {
                e.stop();
                Some(UiMsg::Ui(UiFact::GripDrag {
                    which,
                    x: e.pos.x.max(0) as u16,
                    y: e.pos.y.max(0) as u16,
                }))
            }),
        )
        .on(
            GestureKind::Release,
            Rc::new(move |e: &Event| {
                e.stop();
                Some(UiMsg::Ui(UiFact::GripRelease { which }))
            }),
        )
}

#[cfg(test)]
mod tests {
    use super::*;
    use fresh_ui::{col, row, Input, Mods, Point, Size, Sizing, Ui};

    /// **A resize drag leaves the grip on its first step**, and that is the
    /// whole reason capture exists: the pointer is two panes away by the
    /// second event, and the moves still have to arrive at the thing being
    /// dragged. `chrome::pointer_grab` arranged this by reading an app-side
    /// flag on every event and ranking it against nine others.
    #[test]
    fn a_grip_keeps_the_pointer_it_took() {
        let mk = || -> Node<UiMsg> {
            col().children([
                draggable(
                    Grip::DockWidth,
                    row(),
                    Rc::new(|_: &Event| Some(UiMsg::Ui(UiFact::DockResizeBegin))),
                )
                .w(Sizing::Cells(1))
                .h(Sizing::Cells(10)),
                row().flex(1),
            ])
        };
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(mk(), Size::new(40, 10));

        let facts = |d: fresh_ui::Dispatch<UiMsg>| -> Vec<UiFact> {
            d.msgs
                .into_iter()
                .filter_map(|m| match m {
                    UiMsg::Ui(f) => Some(f),
                    _ => None,
                })
                .collect()
        };

        let got = ui.dispatch(Input::press(
            Point::new(0, 3),
            MouseButton::Left,
            Mods::NONE,
        ));
        assert_eq!(facts(got), vec![UiFact::DockResizeBegin]);

        // Thirty columns away — nowhere near the grip, and still the grip's.
        let got = ui.dispatch(Input::Move {
            pos: Point::new(30, 6),
            mods: Mods::NONE,
        });
        assert!(got.claimed, "the captured grip owns the move");
        assert_eq!(
            facts(got),
            vec![UiFact::GripDrag {
                which: Grip::DockWidth,
                x: 30,
                y: 6
            }]
        );

        let got = ui.dispatch(Input::release(
            Point::new(30, 6),
            MouseButton::Left,
            Mods::NONE,
        ));
        assert_eq!(
            facts(got),
            vec![UiFact::GripRelease {
                which: Grip::DockWidth
            }]
        );

        // And the capture is over: a move somewhere else is nobody's.
        let got = ui.dispatch(Input::Move {
            pos: Point::new(30, 6),
            mods: Mods::NONE,
        });
        assert!(facts(got).is_empty(), "the release let the pointer go");
    }
}
