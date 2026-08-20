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
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UiFact {
    /// Dismiss the open context menu.
    CloseContextMenu,
    /// Move the open context menu's highlight to a row (hover).
    HighlightContextMenuItem(usize),
    /// Activate a row — the same path a keyboard Enter takes.
    ActivateContextMenuItem(usize),
    /// Move the highlight one row up or down.
    StepContextMenu(MenuStep),
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
