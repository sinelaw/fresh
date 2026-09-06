//! The keyboard seam of the full-screen modals, and the caption a described
//! box wears.
//!
//! **The pointer slot that was here is gone.** Settings and the floating
//! plugin panel each owned the whole mouse channel through
//! `ChromeComponent::capture_mouse` — a band ahead of every walk — and this
//! module carried that forward as a full-frame layer that claimed every
//! pointer event and reported which modal it belonged to
//! (`UiFact::ModalPointer`), for a host handler to hit-test against
//! rectangles the interior's painter had recorded. Every interior is a
//! description now: the settings dialog's nodes answer their own presses and
//! its layer is `Modality::Exclusive` with its own scrim; the floating panel's
//! layer is `Modality::Pointer` and dismisses an anchored popup on an outside
//! press itself. Nothing is left to route, so nothing routes.
//!
//! What stays is the keyboard's seam, [`keys`]: an `on_key` at the top of a
//! modal's subtree, reached by focus containment, that names the surface a
//! key nothing inside it answered belongs to — `dispatch_settings_key` and
//! its siblings are bespoke dispatchers over vocabularies the tree's
//! `KeyPress` does not carry, so the key is routed, not transported. And
//! [`title_strip`], the caption every described box stacks on its top edge.

use fresh_ui::{row, Key, Node};

use super::msg::{UiFact, UiMsg};

/// Which surface a key belongs to.
///
/// The floating plugin panel is not here: its keys go to the widget runtime
/// through its own layer (`panel::keys_layer`) rather than to a dialog's
/// dispatcher.
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
/// **This is the keyboard's seam.** It claims by *focus containment*: a modal
/// layer owns the keyboard (`Modality::Exclusive` implies it), so focus goes
/// inside it, and an `on_key` at the top of that subtree sees everything the
/// subtree itself declined.
///
/// A field inside the modal that answers its own key stops it first —
/// listeners run from the focused element outward — so this never gets in the
/// way of an interior that has migrated. That is what makes it the seam and
/// not a shim: the surface keeps whatever it has taken over, and this catches
/// the rest.
pub fn keys(slot: KeySlot, content: Node<UiMsg>) -> Node<UiMsg> {
    fresh_ui::focusable(content)
        .key(keys_key(slot))
        .autofocus()
        .on_key(move |e| {
            e.stop();
            Some(UiMsg::Ui(UiFact::ModalKey(slot)))
        })
}

/// The seam's key, which is how the host reads *whose* keyboard focus sits
/// in: `frame::key_context_of` maps it to the surface's `KeyContext`.
pub fn keys_key(slot: KeySlot) -> Key {
    Key::Str(
        match slot {
            KeySlot::Settings => "keys:settings",
            KeySlot::KeybindingEditor => "keys:keybinding_editor",
            KeySlot::Calibration => "keys:calibration",
            KeySlot::WorkspaceTrust => "keys:workspace_trust",
        }
        .into(),
    )
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
