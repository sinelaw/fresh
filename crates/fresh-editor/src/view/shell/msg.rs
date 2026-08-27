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

    // -- pointer position ----------------------------------------------------
    /// The pointer entered or left something a migrated surface reacts to.
    ///
    /// Carries a `HoverTarget` because the *reactions* are the existing ones:
    /// the menu's auto-switch and submenu machine, the search row's restyle.
    /// Migrating *where the pointer is* does not require rewriting *what each
    /// surface does about it*, and those machines hold the subtle cases (a
    /// submenu's parent must not blink when the pointer rests on it).
    ///
    /// One fact for every surface, because only one thing is under the pointer
    /// at a time — the tree's answer, kept apart from the legacy walk's in
    /// `Editor::shell_hover`.
    Hover(Option<crate::app::types::HoverTarget>),
    /// A click on a status-bar element that answers one.
    ///
    /// The id, not an `Action`: the dispatch behind it is not a pure mapping —
    /// most segments dismiss any open menu-style popup first, and the LSP,
    /// remote and read-only menus deliberately do not, because each owns a
    /// toggle that dismissing would defeat. That table stays where it is; this
    /// only says which element was pressed.
    StatusBarClicked(crate::view::ui::status_bar::StatusBarClickable),
    /// A click on a plugin-registered status-bar token, by its registry key
    /// (`"<plugin>:<token>"`). Fires the `status_bar_token_clicked` hook, so a
    /// plugin's chip is as clickable as a built-in indicator.
    StatusBarTokenClicked(String),

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

    // -- file explorer -------------------------------------------------------
    /// A left press on a tree row, named by its **viewport** index — the same
    /// number `FileTreeView::get_display_node_at_viewport_row` takes.
    ///
    /// One fact for what used to be two routes (single click and double
    /// click). `clicks` is which press of a run this is, straight off
    /// `Event::clicks` — the editor counts the run, the library carries it,
    /// and the handler reads it, so the two routes cannot disagree about which
    /// row they mean.
    ExplorerRowPress { index: usize, clicks: u8 },
    /// A right click on a tree row: select it and open its context menu at the
    /// pointer.
    ExplorerRowContext { index: usize, x: u16, y: u16 },
    /// The `×` on the panel's title line.
    ExplorerClose,
    /// A press on the panel's right-edge grip: start a width drag from here.
    /// The drag itself is still the legacy one — see `shell::file_explorer`.
    ExplorerResizeBegin { x: u16, y: u16 },
    /// The wheel over the panel. Positive is down, matching `Input::Wheel`.
    /// Carries the pointer so the plugin `wheel` hook still gets a position.
    ExplorerScroll { delta: i32, x: u16, y: u16 },
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
