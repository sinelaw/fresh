//! The full-screen modals: Settings, the keybinding editor, and the floating
//! plugin panel.
//!
//! The calibration wizard was here and is not any more — its interior had no
//! mouse and no recorded rectangles, so once its box became a description
//! there was nothing left behind the seam and it carries its own exclusivity
//! (`view::shell::calibration`). That is what each of the remaining three is
//! for.
//!
//! Each owned the whole mouse channel through `ChromeComponent::capture_mouse`
//! — a band ahead of every walk, the shell's included, and the reason
//! `placed_surface_outranks_shell` had to exist at all. Capture is what a
//! modal *is*, and `Modality::Exclusive` is the tree saying it: nothing
//! outside the layer is interactive, so nothing outside it is offered the
//! pointer.
//!
//! **Their interiors are not here.** Settings is eleven modules, the
//! keybinding editor is a table with its own scrollbar and its own
//! double-click semantics, and both hit-test rectangles their own painters
//! recorded. So the tree answers *which* surface an event belongs to, and the
//! surface answers what it means — the same seam as the overlay prompt's
//! toolbar band, at the scale of a whole dialog.
//!
//! The event itself never leaves the host: it is routed, not transported. A
//! tree `Event` cannot carry a crossterm one faithfully in any case — the
//! library reports a drag as a move on purpose, because it routes drags by
//! pointer capture — so what the fact says is "this modal has it", and the
//! applier reads the event the editor already had. That side channel goes when
//! the interiors do.

use std::rc::Rc;

use fresh_ui::{gesture, row, Align, Anchor, Event, GestureKind, Key, Modality, Node, Place};

use super::msg::{UiFact, UiMsg};

/// Which modal is up. At most one is: the band this replaces offered the
/// capture in rank order and stopped at the first that took it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Slot {
    Settings,
    KeybindingEditor,
    FloatingPanel,
}

pub fn key() -> Key {
    Key::Str("modal".into())
}

/// The modal as a layer: the whole frame, exclusive, painting nothing.
///
/// It paints nothing because its interior still does — a layer is in the
/// overlay band, so anything it drew would land on top of the painter that
/// owns the surface. What it contributes is the claim.
pub fn layer(slot: Slot) -> Node<UiMsg> {
    fresh_ui::layer()
        .anchor(Anchor::Screen(Align::Start))
        .place(Place::Fill)
        .modality(Modality::Exclusive)
        .child(surface(slot).key(key()))
}

fn surface(slot: Slot) -> Node<UiMsg> {
    let claim = move |e: &Event| {
        e.stop();
        Some(UiMsg::Ui(UiFact::ModalPointer(slot)))
    };
    let mut n = gesture(row());
    for kind in [
        GestureKind::Press,
        GestureKind::Release,
        GestureKind::Move,
        GestureKind::Wheel,
    ] {
        n = n.on(kind, Rc::new(claim));
    }
    n
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::view::shell::frame::{frame_tree, Frame};
    use fresh_ui::{Input, Mods, MouseButton, Point, Size, Ui};

    fn laid_out(modal: Option<Slot>) -> Ui<UiMsg> {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            frame_tree(Frame {
                modal,
                ..Frame::default()
            }),
            Size::new(80, 24),
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

    /// **Every event, wherever it lands.** That is what the capture band did,
    /// and it is what the modal's layer does — the difference being that this
    /// happens in the same walk as everything else rather than ahead of it.
    #[test]
    fn a_modal_takes_the_whole_channel() {
        let mut ui = laid_out(Some(Slot::Settings));
        for (label, input) in [
            (
                "a press",
                Input::press(Point::new(4, 3), MouseButton::Left, Mods::NONE),
            ),
            (
                "a press in the far corner",
                Input::press(Point::new(79, 23), MouseButton::Left, Mods::NONE),
            ),
            (
                "a move",
                Input::Move {
                    pos: Point::new(10, 10),
                    mods: Mods::NONE,
                },
            ),
            (
                "a wheel",
                Input::Wheel {
                    pos: Point::new(10, 10),
                    delta: 3,
                    axis: fresh_ui::Axis::Vertical,
                    mods: Mods::NONE,
                },
            ),
        ] {
            let got = ui.dispatch(input);
            assert!(got.claimed, "{label} is the modal's");
            assert_eq!(
                facts(got),
                vec![UiFact::ModalPointer(Slot::Settings)],
                "{label} names the modal it belongs to"
            );
        }
    }

    /// And with no modal up, nothing claims on its behalf.
    #[test]
    fn no_modal_means_no_claim() {
        let mut ui = laid_out(None);
        let got = ui.dispatch(Input::press(
            Point::new(4, 3),
            MouseButton::Left,
            Mods::NONE,
        ));
        assert!(
            !facts(got)
                .iter()
                .any(|f| matches!(f, UiFact::ModalPointer(_))),
            "no modal, no claim"
        );
    }
}
