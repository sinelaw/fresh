//! What a handler in the shell's tree returns.
//!
//! `fresh-ui` handlers produce a message; the application applies it. In the
//! editor that message cannot simply *be* an [`Action`]: `Action` is the
//! rebinding and serialization currency, so it deliberately carries no
//! positional variants — no click-at-byte, no drag-to-offset, no
//! select-tab-(leaf, index). Those are UI facts, meaningful only to this
//! frame, and putting them in `Action` would pollute the keybinding namespace
//! with things nobody can bind.
//!
//! So the tree's message type wraps it. Everything a user could bind stays an
//! `Action` and goes through the existing pipeline untouched; everything
//! positional is a `Ui` variant that `update` consumes and never serializes.
//! The library's own demo makes the same split for the same reason.

use crate::input::keybindings::Action;

/// A message from the shell's widget tree.
#[derive(Clone, Debug)]
pub enum UiMsg {
    /// Something the user could have bound a key to. Applied through the
    /// existing `handle_action` pipeline, which is unchanged by this
    /// migration.
    Action(Action),
    /// A UI fact with no meaning outside this frame. Consumed where messages
    /// are applied and never serialized.
    ///
    Ui(UiFact),
}

/// The positional half: facts about *where*, which never become keybindings.
///
/// `PartialEq` but not `Eq`: `HoverTarget` carries paths and is only partially
/// comparable, and tests compare facts.
#[derive(Clone, Debug, PartialEq)]
pub enum UiFact {
    /// Dismiss the open context menu.
    CloseContextMenu,
    /// Move the open context menu's highlight to a row (hover).
    HighlightContextMenuItem(usize),
    /// Activate a row — the same path a keyboard Enter takes.
    ActivateContextMenuItem(usize),
    /// Move the highlight one row up or down.
    StepContextMenu(MenuStep),

    // -- menu bar ------------------------------------------------------------
    /// The pointer entered or left something the menu reacts to.
    ///
    /// Carries a `HoverTarget` because the reaction is the existing state
    /// machine (`menu_hover_reaction`): bar auto-switch, submenu open/close,
    /// highlight. Migrating *where the pointer is* does not require rewriting
    /// *what the menu does about it*, and the machine is the part with the
    /// subtle cases (staying put on a submenu's parent so it does not blink).
    MenuHover(Option<crate::app::types::HoverTarget>),
    /// A **press** on a bar label. Toggles that menu.
    ///
    /// Press, not click, and that is what makes the toggle work. The layer's
    /// outside-pointer dismissal fires on the press too, so both land in one
    /// dispatch and the applier can look at what was open *before* either of
    /// them ran. On the release it could not: the menu is closed by then, and
    /// the frame in between has already rebuilt the tree — so a label that
    /// carried its own open-ness would carry a stale answer and reopen the
    /// menu it was meant to shut.
    ///
    /// It is also what the pre-migration code did: menu-bar routing ran off
    /// `MouseEventKind::Down`. Pressing the bar and releasing over an item —
    /// the way a menu bar is used — needs exactly this split, the bar acting
    /// on the press and the row on the release.
    MenuBarPress { index: usize },
    /// A click on a dropdown row, named by its level and position.
    MenuItemClick { depth: usize, index: usize },
    /// Close the open menu (an outside click, or a click on an inert cell of
    /// the dropdown's own box).
    CloseMenu,
}

/// Which way a menu's highlight moves.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MenuStep {
    Prev,
    Next,
}

impl From<Action> for UiMsg {
    fn from(a: Action) -> UiMsg {
        UiMsg::Action(a)
    }
}
