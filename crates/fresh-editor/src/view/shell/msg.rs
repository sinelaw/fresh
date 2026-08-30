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
use crate::model::event::LeafId;

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
    /// A press landed on a plugin widget, carrying the widget's own hit.
    ///
    /// **What replaces the byte-range scan.** The runtime recorded a
    /// `HitArea` per interactive range and resolved a click by walking those
    /// ranges against a row and a byte offset; the tree hit-tests a rectangle
    /// it laid out and the node hands over the hit it was built with. The
    /// dispatch behind it — `deliver_widget_hit`, which all three frontends
    /// share — is unchanged, so the byte range stops being a hit-test and
    /// becomes what it always was: a payload.
    WidgetHit {
        slot: super::widgets::Slot,
        hit: crate::widgets::HitArea,
        /// The column the press landed on, within the hit's own piece.
        ///
        /// A press on a text field means "put the caret here", and the runtime
        /// answered that by comparing the screen column against geometry the
        /// painter had stamped. The piece the gesture is on *is* that
        /// geometry, so the offset inside it is what is left to say. `None`
        /// for a hit that arrived without a pointer behind it.
        at: Option<u16>,
    },
    /// The floating plugin panel's `[×]` was pressed.
    ///
    /// It dismisses exactly as Esc and Cancel do, firing the panel's `cancel`
    /// widget event. This replaces `close_button_rect` — a rectangle the
    /// painter computed, filed on the panel, and a mouse arm compared against
    /// before the general panel hit-test so the press could not also focus a
    /// widget underneath. The node stops the event, which says the same thing
    /// without the ordering.
    PanelClosed,
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
    // -- a pane's tab strip ---------------------------------------------------
    /// A left press on a pane's tab strip, at a cell.
    ///
    /// The strip is one node per pane; its *interior* — the tabs, their close
    /// buttons, the "+", the scroll arrows and the split controls drawn over
    /// them — is laid out by the tab renderer and hit-tested against what it
    /// recorded. So the fact says which strip and where, and the two handlers
    /// behind it are the ones the boxes dispatched to, in the order their `z`
    /// used to express: the split controls sit on top of the tab row.
    PaneTabsPress {
        pane: LeafId,
        x: u16,
        y: u16,
    },
    /// A right press on a pane's tab strip: the tab's context menu, on the tab
    /// under the pointer. Dismissing it from elsewhere is still the base
    /// surface's, which this claim simply keeps out of the way of.
    PaneTabsSecondary {
        pane: LeafId,
        x: u16,
        y: u16,
    },
    /// The `□` / `⧉` button on a pane's strip. No coordinates: the button is
    /// a node and it knows its pane.
    PaneMaximize(LeafId),
    /// The `×` beside it. Pops a confirmation rather than closing outright.
    PaneClose(LeafId),
    /// The pointer is on a pane's tab strip at a cell, or has left one.
    ///
    /// Which tab, which close button, which split control is the tab
    /// renderer's hit test — resolved where that layout lives, not here.
    PaneTabsHover(Option<(LeafId, u16, u16)>),
    /// A vertical wheel over a pane's tab strip pans it: up walks toward the
    /// first tab, down toward the last.
    PaneTabsWheel {
        pane: LeafId,
        x: u16,
        y: u16,
        delta: i32,
    },
    /// A sideways wheel over the strip pans it the same way, without the
    /// popup dismissal and plugin hook the vertical one carries.
    PaneTabsPan {
        pane: LeafId,
        delta: i32,
    },

    /// A left press on a pane's content, and which press of a run it is: one
    /// places the caret, two selects the word, three the line — or toggles a
    /// fold, when the cell is a folded line's gutter indicator.
    ///
    /// The pane is the node's. The content *rectangle* the handlers take is
    /// read back from the same node, because click-to-byte is a projection
    /// through the view pipeline and needs the extent, not just the cell.
    PaneContentPress {
        pane: LeafId,
        x: u16,
        y: u16,
        clicks: u8,
    },

    // -- a pane's scrollbars, and its wheel -----------------------------------
    /// A left press on one of a pane's scrollbars.
    ///
    /// The pane is the node's; where the thumb is, and how wide the content
    /// is, are reads of the scroll state at paint time and stay recorded.
    PaneScrollbarPress {
        pane: LeafId,
        axis: fresh_ui::Axis,
        x: u16,
        y: u16,
    },
    /// The pointer is on a pane's vertical scrollbar at a row, or has left it.
    /// Thumb or track is decided from the recorded thumb extent.
    PaneScrollbarHover(Option<(LeafId, u16)>),
    /// A wheel notch over a pane — its content, either of its bars, whichever
    /// part reported it. They all mean the same thing: move this pane's
    /// surface. Carries the pointer's cell for the plugin `mouse_wheel` hook.
    PaneWheel {
        pane: LeafId,
        x: u16,
        y: u16,
        delta: i32,
    },
    /// A sideways wheel over a pane pans its surface. No popup dismissal and
    /// no terminal live/scrollback transition — panning is not reading.
    PanePan {
        pane: LeafId,
        delta: i32,
    },

    /// A right-click landed somewhere — anywhere — so the three transient tab
    /// menus close: the "+" new-tab menu, the close-split confirmation, and a
    /// tab's context menu.
    ///
    /// An observation, not a claim: the click goes on to whatever it was aimed
    /// at, which is how the same press that clears a tab's context menu can go
    /// on to open the next one. See `shell::splits::tab_menu_guard`.
    ClearTabMenus,
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

    /// One step of menu-bar navigation, named by what it means rather than by
    /// the key that produced it.
    ///
    /// The key → meaning half is the keymap's, declared on the chain as
    /// `.shortcut(key, intent)`; the meaning → effect half is the applier's.
    /// That split is the whole point: a user who binds `C-n` to `menu_down`
    /// gets a shortcut for `Intent::Down`, and nothing has to consult the
    /// keymap from inside a key handler to find out.
    MenuNav(MenuNav),

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
    MenuBarPress {
        index: usize,
    },
    /// A click on a dropdown row, named by its level and position.
    MenuItemClick {
        depth: usize,
        index: usize,
    },
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
    ExplorerRowPress {
        index: usize,
        clicks: u8,
    },
    /// A right click on a tree row: select it and open its context menu at the
    /// pointer.
    ExplorerRowContext {
        index: usize,
        x: u16,
        y: u16,
    },
    /// A right-press on the panel that did not land on a row.
    ///
    /// The old component bound its right-press to the *whole* explorer — its
    /// comment said "the union box spans the whole explorer" — so a click
    /// past the last entry still opened the menu, in its root mode. Binding
    /// only to rows dropped that: empty space below the files answered
    /// nothing. The row index is resolved app-side from the panel's own
    /// rectangle, as the component resolved `relative_row`, because the
    /// description cannot read geometry.
    ExplorerBodyContext {
        x: u16,
        y: u16,
    },
    /// A left-press on the panel that did not land on a row.
    ///
    /// The same union-box rule as `ExplorerBodyContext`, for the other button:
    /// `handle_file_explorer_click` called `take_focus_for_file_explorer()`
    /// for *any* left click inside the panel and only then looked for a row,
    /// so clicking the empty space below the tree focused the explorer.
    /// Binding the left press to rows alone dropped that — and with it the
    /// only way a test (or a user) can click back into a panel whose files do
    /// not fill it.
    ExplorerBodyPress,
    /// A popup list row was chosen. By index, because the row knows which one
    /// it is — the coordinate hit-test in `chrome::Popups` recovered an index
    /// the row already had.
    PopupSelect(usize),
    /// A pointer landed outside a transient popup, which is what dismisses one.
    /// The layer declares the condition; hiding the popup is the app's move.
    PopupDismissTransient,
    /// A press inside a text or markdown popup, in the content's own
    /// coordinates. What it means is the host's: a link if one is there, and
    /// the start of a text selection otherwise. The rectangle it used to be
    /// hit-tested against is the tree's now, so only the cell is reported.
    PopupTextPress {
        line: usize,
        col: usize,
    },
    /// The pointer moving while that press is still held. Extends the
    /// selection the press began.
    PopupTextDrag {
        line: usize,
        col: usize,
    },
    /// A press on the overlay card's toolbar band, in the band's own
    /// coordinates. The controls are a plugin's `WidgetSpec`, laid out by the
    /// widget runtime rather than by the tree, so the host hit-tests its own
    /// boxes — the band reports where, which is all the tree can know until
    /// `WidgetSpec` becomes a `Node`.
    CardToolbarPress {
        x: u16,
        y: u16,
    },
    /// A wheel over the overlay card's preview pane. The pane is a painter's
    /// still, so it has no window for the wheel to chain into.
    CardPreviewScroll(i32),
    /// A pointer moved the suggestion selection to this row.
    SuggestionSelect(usize),
    /// A double-click confirmed this suggestion — the same path Enter takes.
    SuggestionConfirm(usize),
    /// The `×` on the panel's title line.
    ExplorerClose,
    /// A press on the panel's right-edge grip: start a width drag from here.
    /// The drag itself is still the legacy one — see `shell::file_explorer`.
    ExplorerResizeBegin {
        x: u16,
        y: u16,
    },
    /// The wheel over the panel. Positive is down, matching `Input::Wheel`.
    /// Carries the pointer so the plugin `wheel` hook still gets a position.
    ExplorerScroll {
        delta: i32,
        x: u16,
        y: u16,
    },
    /// A left press inside the dock column, in screen coordinates. The panel's
    /// widgets are a plugin's `WidgetSpec` rather than nodes, so the runtime
    /// hit-tests its own boxes and the tree reports only where — the same seam
    /// as `CardToolbarPress`.
    DockPress {
        x: u16,
        y: u16,
    },
    /// A right press inside the dock column: the plugin raises a per-session
    /// context menu from it.
    DockContext {
        x: u16,
        y: u16,
    },
    /// The wheel over the dock column. Positive is down, and the pointer rides
    /// along for the panel's own hit test.
    DockScroll {
        delta: i32,
        x: u16,
        y: u16,
    },
    /// A press on the dock's right-edge grip: start a width drag. The drag
    /// itself is still the legacy grab — see `shell::dock`.
    DockResizeBegin,
    /// A left press landed outside the dock column. Blurs a focused dock and
    /// does nothing to one already blurred; either way the press goes on.
    DockBlur,
    /// A press outside the theme inspector, or any key while it is up. Both
    /// dismiss it and both go on to what they were aimed at.
    ThemeInfoDismiss,
    /// The inspector's action row was clicked: open the theme editor on the
    /// key it is showing.
    ThemeInfoOpenEditor,
    /// The pointer entered or left that row.
    ThemeInfoButtonHover(bool),
    /// Ctrl+Right-Click: inspect the theme keys behind this screen cell.
    ThemeInspect {
        x: u16,
        y: u16,
    },
    /// A left press in the file-open dialog, in screen coordinates. The
    /// dialog's elements are cell spans its painter recorded, so the tree
    /// reports where and the hit test is the painter's — the same seam as
    /// `CardToolbarPress`.
    BrowserPress {
        x: u16,
        y: u16,
        double: bool,
    },
    /// The pointer moved over the dialog. The hover target is resolved against
    /// the same recorded spans.
    BrowserHover {
        x: u16,
        y: u16,
    },
    /// The wheel over the dialog. Positive is down.
    BrowserScroll(i32),
    /// A radio row in the workspace-trust prompt was clicked. **Selection is
    /// not consent**: this moves the selection and leaves the prompt up, the
    /// same two-step the keyboard has.
    TrustSelect(usize),
    /// `[ OK ]`: commit the current selection.
    TrustConfirm,
    /// The secondary button — Cancel when the prompt was opened voluntarily,
    /// Quit for the mandatory gate at startup.
    TrustSecondary,
    /// A press on a split divider: start the width drag on *this* container.
    ///
    /// The node knows which container it is, so nothing hit-tests a recorded
    /// list of separator rectangles to find out — which is what
    /// `handle_click_split_separator` did, comparing the click against each in
    /// turn. The drag itself is still the legacy grab.
    SeparatorPress {
        container: crate::model::event::ContainerId,
        direction: crate::model::event::SplitDirection,
        x: u16,
        y: u16,
    },
    /// The pointer entered or left a divider. Drives the hover highlight the
    /// split renderer paints.
    SeparatorHover(
        Option<(
            crate::model::event::ContainerId,
            crate::model::event::SplitDirection,
        )>,
    ),
    /// A pointer event belongs to this full-screen modal, whose interior is
    /// still a painter's and hit-tests rectangles that painter recorded. The
    /// event itself never left the host — see `shell::modal`.
    ModalPointer(super::modal::Slot),
    /// A press on one of the keybinding editor's dialogs.
    ///
    /// **The dialogs answer for themselves, the table does not — yet.** Five
    /// of the ten rectangles that modal's painter recorded belong to these
    /// three boxes, and the mouse arm behind them was a chain of
    /// `point_in_rect` against each. They are nodes now; the table and the
    /// search bar still go through `ModalPointer`.
    KeybindingDialog(super::keybinding::Target),
    /// A press on a row of the keybinding editor's table, by display index.
    ///
    /// The arm behind it was `(row - table_first_row_y) + scroll.offset`,
    /// against two rectangles the painter recorded — the second of which
    /// existed only because the window belonged to the painter. The row knows
    /// its own index.
    KeybindingRow(usize),
    /// A press on the keybinding editor's search row, which starts a search.
    /// The last of that modal's ten recorded rectangles.
    KeybindingSearch,
    /// A press on one of the settings dialogs' buttons.
    ///
    /// **These were laid out twice**: the painter placed them, and
    /// `get_confirm_dialog_button_at` placed them again to find which one a
    /// cell was on — with the comment "must match `render_confirm_dialog`"
    /// beside the copy. The button is the node now.
    SettingsDialog(super::settings::Target),
    /// The pointer entered or left one of them.
    SettingsDialogHover(Option<super::settings::Target>),
    /// A press on one of the settings modal's footer buttons.
    SettingsButton(super::settings::Button),
    /// The pointer entered or left one of them.
    SettingsButtonHover(Option<super::settings::Button>),
    /// A press on a row of the settings modal's category tree.
    ///
    /// **The row's own index, not a cell compared against a rectangle.** The
    /// painter filed one rectangle per visible row in `layout.categories` and
    /// `layout.sections`, and the arm behind them walked both lists.
    SettingsCategory(usize),
    /// A press on a section row under a category, by `(category, section)`.
    SettingsCategorySection(usize, usize),
    /// A press on a category's `▶`/`▼`, which expands it rather than
    /// selecting it. This was `layout.disclosures` — a one-column rectangle
    /// per expandable row, filed so a chain of `point_in_rect` could tell the
    /// chevron from the label beside it.
    SettingsCategoryDisclosure(usize),
    /// A press on the settings panel header's `[Clear …]`. It was
    /// `layout.clear_category_button` — a rectangle the painter filed as it
    /// drew the button, for a chain of `point_in_rect` to find again.
    SettingsClearCategory,
    /// A press on a settings card — anywhere on it that a control did not
    /// answer for. It selects the item, which is what `SettingsHit::Item` did.
    SettingsItem(usize),
    /// The pointer entered a card, or left the one it was on. The painter
    /// learned this by hit-testing the pointer's cell against every item's
    /// rectangle on every move; entering and leaving are the two things that
    /// actually happen.
    SettingsItemHover(Option<usize>),
    /// A press on a nullable setting's `[Inherit]`, which unsets it.
    SettingsInherit(usize),
    /// The pointer is on that button.
    SettingsInheritHover(usize),
    /// A press on a field of the settings entry-edit dialog — anywhere on it
    /// a control did not answer for. It focuses the field.
    SettingsEntryItem(usize),
    /// The pointer entered a field, or left the one it was on. The painter
    /// re-walked every item's rows on every move to learn this, and its walk
    /// omitted the section headers the renderer had drawn.
    SettingsEntryItemHover(Option<usize>),
    /// A press on one of the dialog's own buttons: Save, Cancel, Delete.
    SettingsEntryButton(usize),
    SettingsEntryButtonHover(Option<usize>),
    /// A press on a field's `[Reset]` / `[Inherit]`: `(field, action)`.
    SettingsEntryFieldAction(usize, usize),
    /// A press on one of the search's results, by its absolute index.
    ///
    /// **Absolute, and it always was** — the painter filed a rectangle per
    /// *visible* card, so the position in that list was a viewport slot and,
    /// once the list had scrolled, not the result's index (#2860). A list row
    /// knows its own index whether it is on screen or not.
    SettingsSearchResult(usize),
}

/// What a menu-bar navigation step does to the open chain.
///
/// `Back` and `Forward` are one step each rather than four, because at the top
/// level they move between menus and inside a submenu they close or open one —
/// which is what Left and Right have always meant here, and stating it as two
/// facts rather than four keeps the applier from re-deciding which case it is.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MenuNav {
    PrevItem,
    NextItem,
    /// Close a submenu, or step to the previous menu at the top level.
    Back,
    /// Open a submenu, or step to the next menu at the top level.
    Forward,
    First,
    Last,
    /// Open the highlighted submenu, or run the highlighted action and close.
    Activate,
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
