//! The full-screen modals: Settings and the floating plugin panel.
//!
//! The calibration wizard and the keybinding editor were here and are not any
//! more — its interior had no
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
    FloatingPanel,
}

pub fn key() -> Key {
    Key::Str("modal".into())
}

/// Which surface a key belongs to.
///
/// Separate from [`Slot`] because the two answer different questions and no
/// longer have the same members: the calibration wizard and the keybinding
/// editor carry their own exclusivity in their own modules and never wanted a
/// pointer slot, while the floating panel's keys go to the widget runtime
/// rather than to a dialog's dispatcher.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum KeySlot {
    Settings,
    KeybindingEditor,
    Calibration,
    WorkspaceTrust,
}

/// Wrap a modal's contents so that a key nothing inside it answered is that
/// modal's.
///
/// **This is the keyboard's `surface`.** The pointer's version claims by
/// gesture on a full-frame node; the keyboard's claims by *focus containment*,
/// which is the same statement one channel down: a modal layer owns the
/// keyboard (`Modality::Exclusive` implies it), so focus goes inside it, and
/// an `on_key` at the top of that subtree sees everything the subtree itself
/// declined.
///
/// A field inside the modal that answers its own key stops it first —
/// listeners run from the focused element outward — so this never gets in the
/// way of an interior that has migrated. That is what makes it the seam and
/// not a shim: the surface keeps whatever it has taken over, and this catches
/// the rest.
pub fn keys(slot: KeySlot, content: Node<UiMsg>) -> Node<UiMsg> {
    fresh_ui::focusable(content).autofocus().on_key(move |e| {
        e.stop();
        Some(UiMsg::Ui(UiFact::ModalKey(slot)))
    })
}

/// The modal as a layer: the whole frame, exclusive, painting nothing.
///
/// It paints nothing because its interior still does — a layer is in the
/// overlay band, so anything it drew would land on top of the painter that
/// owns the surface. What it contributes is the claim.
pub fn layer(slot: Slot) -> Node<UiMsg> {
    let content = surface(slot).key(key());
    let content = match slot {
        Slot::Settings => keys(KeySlot::Settings, content),
        // The floating panel's keys are the widget runtime's, and reach it
        // through `dispatch_floating_widget_key` — a different interior from
        // a dialog's own dispatcher, and one that declines rather than
        // swallowing (an unhandled shortcut blurs the dock).
        Slot::FloatingPanel => content,
    };
    fresh_ui::layer()
        .anchor(Anchor::Screen(Align::Start))
        .place(Place::Fill)
        .modality(Modality::Exclusive)
        .child(content)
}

/// A caption on a box's top border, where `Block::title` drew one.
///
/// **A caption is not a ring.** `Draw::Border` says "an outline around this
/// rectangle" and carries no text, so a described box cannot put a title in
/// its edge the way a `Block` did. Stacking a one-row strip over the box is
/// how the floating panel's frame already does it, and it is the only form
/// that keeps the box's *interior* the size the painter's was: a title given
/// a row of its own is a row the content no longer has, and everything below
/// it sits one line lower than the surface it replaced.
///
/// That is not cosmetic. The keybinding editor's table shifted down a row, so
/// its scrollbar track began one row below where every caller computes it —
/// clicking the top of the track hit nothing, and a drag from there never
/// started.
///
/// One row, deliberately: a transparent node still produces a hit path, and a
/// full-height strip would offer that path over the whole interior before the
/// interior's own.
pub fn title_strip(title: impl Into<String>, ink: String) -> Node<UiMsg> {
    let clear = |n: Node<UiMsg>| n.pointer_mode(fresh_ui::PointerMode::Transparent);
    row()
        .h(fresh_ui::Sizing::Cells(1))
        .pointer_mode(fresh_ui::PointerMode::Transparent)
        .children([
            // `Block::title` starts one cell in from the corner.
            clear(row().w(fresh_ui::Sizing::Cells(1))),
            clear(fresh_ui::text(title.into()).theme(ink)),
            clear(row().flex(1)),
        ])
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

    /// **The keyboard, on the same terms as the pointer.** A key nothing
    /// inside the modal answered is that modal's — by containment, not by a
    /// rank in a central list.
    #[test]
    fn a_modal_takes_the_keyboard_too() {
        use fresh_ui::{KeyCode, KeyPress};
        let mut ui = laid_out(Some(Slot::Settings));
        for code in [KeyCode::Char('x'), KeyCode::Esc, KeyCode::Down] {
            let got = ui.dispatch(Input::Key(KeyPress::with(code, Mods::NONE)));
            assert!(got.claimed, "{code:?} is the modal's");
            assert_eq!(facts(got), vec![UiFact::ModalKey(KeySlot::Settings)]);
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
