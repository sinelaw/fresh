//! The editor frame as a `fresh-ui` description.
//!
//! Every region is a `Host` leaf: the shell owns the *layout*, the existing
//! painters keep owning the *content*. Regions are then replaced by native
//! descriptions one at a time (stages S2–S5 of the migration doc).
//!
//! The rectangles this produces are asserted equal to the ones
//! `Editor::render`'s ratatui `Layout` calls produce, over a sweep of sizes and
//! visibility combinations, in `tests/ui_shell_frame_parity.rs`.

use fresh_ui::{col, host, row, HostId, Node, Sizing};

use super::msg::UiMsg;

/// A region of the frame the host still paints itself.
///
/// The discriminants are the `HostId` values carried in `Draw::Host`, so the
/// fold can map an item straight back to the painter that owns it.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u64)]
pub enum HostRegion {
    Dock = 1,
    MenuBar = 2,
    Explorer = 3,
    /// The split grid: buffers, terminals, tabs, scrollbars. The last region
    /// that will still be a `Host` when the migration finishes.
    Body = 4,
    StatusBar = 5,
    SearchOptions = 6,
    PromptLine = 7,
}

impl HostRegion {
    pub const ALL: [HostRegion; 7] = [
        HostRegion::Dock,
        HostRegion::MenuBar,
        HostRegion::Explorer,
        HostRegion::Body,
        HostRegion::StatusBar,
        HostRegion::SearchOptions,
        HostRegion::PromptLine,
    ];

    pub fn from_host_id(id: HostId) -> Option<HostRegion> {
        HostRegion::ALL.into_iter().find(|r| r.id() == id.0)
    }

    pub fn id(self) -> u64 {
        self as u64
    }
}

impl From<HostRegion> for HostId {
    fn from(r: HostRegion) -> HostId {
        HostId(r.id())
    }
}

/// What a `Draw::Host` in this frame addresses.
///
/// A region is one of the seven fixed slots above. A **pane** is not: there is
/// one per visible leaf, they come and go as the user splits and closes, and
/// each paints only its own buffer. So the id space is split — regions keep
/// the small discriminants they always had, and a pane's id is its `LeafId`
/// tagged into the high half, which cannot collide with them.
///
/// This is what lets the fold keep its "a host id with no region" assertion
/// honest: an id that resolves to neither is still a painter that would draw
/// nothing, in silence.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum HostTarget {
    Region(HostRegion),
    /// One pane's content, by the leaf showing it.
    Pane(crate::model::event::LeafId),
}

/// The bit that separates a pane's id from a region's.
///
/// Regions are 1..=7 and `LeafId`s are dense small integers from the same
/// counter, so the two would otherwise overlap immediately.
const PANE_TAG: u64 = 1 << 32;

/// The `HostId` a pane's content leaf carries.
pub fn pane_host_id(id: crate::model::event::LeafId) -> HostId {
    HostId(PANE_TAG | id.0 .0 as u64)
}

impl HostTarget {
    /// Which painter owns this id, or `None` when it names neither — which is
    /// the fold's assertion, not a case to handle.
    pub fn from_host_id(id: HostId) -> Option<HostTarget> {
        if id.0 & PANE_TAG != 0 {
            let leaf = (id.0 & (PANE_TAG - 1)) as usize;
            return Some(HostTarget::Pane(crate::model::event::LeafId(
                fresh_core::SplitId(leaf),
            )));
        }
        HostRegion::from_host_id(id).map(HostTarget::Region)
    }
}

/// Which regions are visible, and how wide the sized ones are.
///
/// Every field here is *app state*: `build()` cannot read geometry, so
/// decisions that today read `size` at the top of `render` — the dock's
/// bail-out, the explorer's column count — are resolved from state before the
/// description is built. See [`Frame::resolve_dock`].
// Neither `Eq` nor `PartialEq`. Nothing compared frames — the doc comment here
// said so while the derive stayed — and now something cannot: a mounted plugin
// panel carries its `WidgetSpec`, which is `Clone + Debug` and not comparable,
// because comparing two of them is not a question anything asks. A frame is
// built fresh and handed to `frame_tree`; identity short-circuiting is
// `.shared()`'s job (0.1), on subtrees, not on the whole description.
#[derive(Clone, Debug)]
pub struct Frame {
    pub menu_bar: bool,
    pub status_bar: bool,
    /// The search-options row's toggles, or `None` when the row is hidden.
    ///
    /// Content, not a flag: this row is measured by the tree, so what it says
    /// is what decides how wide each toggle is. The row's *existence* is
    /// `is_some`.
    pub search_options: Option<super::search_options::SearchOptions>,
    /// The status bar's elements, when it has any.
    ///
    /// Content, not visibility — the same split as `menu_bar` /
    /// `menu_bar_items` above: `status_bar` decides whether the row exists,
    /// this decides what it says. `None` with `status_bar: true` is a row the
    /// legacy painter fills as a `Host`; it does not hide anything. (The
    /// comment here used to claim this field replaced `status_bar: false`,
    /// which `frame_tree` never did — both arms take their height from the
    /// bool.)
    pub status_bar_items: Option<super::status_bar::StatusBar>,
    pub prompt_line: bool,
    /// Whether a prompt is up, and so whether the keyboard's owner is the
    /// prompt. Separate from `prompt_line` — the overlay form of the prompt
    /// draws no row and still owns the keyboard. See
    /// [`super::prompt::keys_layer`].
    pub prompt_keys: bool,
    /// Whether the dock has keyboard focus, and so whether its layer is the
    /// keyboard's owner. `chrome::Dock::layers`' `owns_keyboard`, said where
    /// the precedence is now derived. See [`super::panel::keys_layer`].
    pub dock_keys: bool,
    /// The same for the centred plugin panel.
    pub panel_keys: bool,
    /// Column width, already resolved against the frame width.
    pub dock: Option<u16>,
    /// The dock's content as a description, when the adapter covers every
    /// variant of the orchestrator's spec. `None` leaves the `Host` leaf the
    /// painter fills.
    pub dock_interior: Option<super::panel::Interior>,
    /// Whether the pointer is on the dock's resize grip; the grip paints its
    /// own `│` from this, the way the file explorer's does.
    pub dock_grip_hovered: bool,
    /// Whether the dock has keyboard focus; its divider wears the accent then,
    /// the way the file explorer's border does.
    pub dock_focused: bool,
    /// The sidebar's content, or `None` when it is hidden. Like the
    /// search-options row, content rather than a flag: the tree measures the
    /// panel's rows and reads their rectangles back.
    pub explorer: Option<super::file_explorer::Explorer>,
    /// The open context menu, if any. An overlay is an ordinary child of the
    /// tree rather than a separately-ranked surface — which is the whole point
    /// of moving them here.
    pub menu: Option<super::context_menu::Menu>,
    /// The open menu-bar dropdown chain, outermost level first. Empty when no
    /// menu is open.
    pub dropdowns: Vec<super::menu::DropdownLevel>,
    /// The `menu` section of the keymap, as shortcuts on the open chain.
    pub menu_keys: Vec<super::menu::MenuShortcut>,
    /// The menu bar's labels. Content, not visibility: `menu_bar` above says
    /// whether the row exists at all, and an existing row with no labels is a
    /// blank row of the bar's own colour.
    pub menu_bar_items: super::menu::MenuBar,
    /// The prompt's suggestion list, or `None` when no prompt is offering one.
    /// Content rather than a flag, like the other migrated surfaces: the rows
    /// are what the tree measures, and the layer's presence is `is_some`.
    pub suggestions: Option<super::prompt::Suggestions>,
    /// Every popup on screen, in paint order: the buffer's stack, then the
    /// top of the global one over it — the order `render_buffer_popups` and
    /// `render_top_global_popup` already run in.
    pub popups: Vec<super::popup::Placed>,
    /// The floating-overlay prompt's card, or `None` when no overlay prompt is
    /// open. Its bands are still `Host` leaves — the input line, the plugin's
    /// toolbar, the preview pane and the plugin's footer are all painters — so
    /// what the tree owns here is the arithmetic that placed them, which is
    /// what two copies of it disagreed about.
    pub card: Option<super::overlay_prompt::Card>,
    /// The theme inspector's popup, when Ctrl+Right-Click has opened one. Over
    /// everything: it inspects the cell under *any* chrome, so it has to be
    /// visible over that chrome too.
    pub theme_info: Option<super::theme_info::ThemeInfo>,
    /// The file-open dialog, when one is open. Its interior is still a
    /// painter's; what the tree owns is where it goes and what it absorbs.
    pub browser: Option<super::file_browser::Browser>,
    /// The workspace-trust prompt. A blocking modal: it dims the whole frame
    /// and nothing outside it is interactive.
    pub trust: Option<super::trust::Trust>,
    /// The split grid, when there is one. Its *content* is the body's `Host`
    /// leaf still; what the tree carries is the panes' geometry and the
    /// dividers, which answer their own presses.
    pub splits: Option<super::splits::Splits>,
    /// The full-screen modal that has the pointer, if any. At most one: the
    /// capture band this replaces stopped at the first taker in rank order.
    pub modal: Option<super::modal::Slot>,
    /// Whether the settings dialog is open. As `keybinding`: the tree carries
    /// the box its twenty-odd recorded rectangles are measured from, and
    /// nothing else of it yet.
    /// The settings dialog, when it is open: its title and its search row.
    /// The body between them is still the painter's.
    pub settings: Option<super::settings::Chrome>,
    /// Its open dialog, when it has one. Three of them are here — the
    /// unsaved-changes prompt, the reset prompt and the help overlay — and the
    /// entry-dialog stack is not.
    pub settings_dialog: Option<super::settings::Dialog>,
    /// The entry-edit dialogs open over it, innermost last. A settings map
    /// opens one to edit an entry, and an entry can open another.
    pub settings_entry: Vec<super::entry::Dialog>,
    /// Whether the keybinding editor is open. Its *interior* is still a
    /// painter's — a table with its own scrollbar and ten recorded
    /// rectangles — so what the tree carries is the box those rectangles are
    /// measured from, and the claim.
    /// The keybinding editor, when it is open: its title, its three header
    /// rows and its footer. The whole modal is the tree's now — box, chrome,
    /// table and dialogs — so the flag became the content.
    pub keybinding: Option<super::keybinding::Chrome>,
    /// Its table, when no dialog covers it. `None` while one does: a dialog is
    /// a layer over this one, so the table would be under it and building it
    /// would be work for cells nobody sees.
    pub keybinding_table: Option<super::keybinding::Table>,
    /// The keybinding editor's open dialog, when it has one. **These are the
    /// tree's and the rest of the interior is not**, which is a statement
    /// about paint order rather than about how far the migration got: the
    /// overlay band is folded after every legacy painter, so a described
    /// dialog lands on top of the table the painter drew — where it belongs —
    /// and a described *table* would have covered the painter's dialogs.
    pub keybinding_dialog: Option<super::keybinding::Dialog>,
    /// The event-debug dialog. Like the calibration wizard, its interior is
    /// here too: no mouse, no recorded rectangles.
    pub event_debug: Option<super::event_debug::EventDebug>,
    /// The input calibration wizard. Unlike the other three modals its
    /// *interior* is here too — it has no mouse and no recorded rectangles,
    /// so there was nothing left behind the seam once the box moved.
    pub calibration: Option<super::calibration::Calibration>,
    /// **Which window the window-owned half of this frame belongs to.**
    ///
    /// The editor is N independent workspaces and there is one retained tree,
    /// reconciled each frame against whichever is active. Nothing in the
    /// description named a window, so two workspaces' subtrees matched each
    /// other: reconciliation is by `(type, key)` at a position, and
    /// `SplitManager::next_split_id` starts at 1 in every window — window A's
    /// first pane and window B's first pane carry the *same* key.
    ///
    /// This is the key that bounds identity and the persistence scope that
    /// lets a window's incidental view state survive being switched away
    /// from. `fresh_ui::scope` is one node for both, because declaring only
    /// one of them is silently wrong in either direction.
    ///
    /// `None` is "one unnamed window", which is what every test that does not
    /// care about workspaces gets from `Frame::default`.
    pub window: Option<u64>,
    /// The floating plugin panel's frame, when one is mounted. Its *interior*
    /// is still the widget runtime's; what the tree owns is the box — where it
    /// goes, its ring, its title and its `[×]`.
    pub panel: Option<super::panel::Panel>,
}

impl Default for Frame {
    fn default() -> Self {
        Frame {
            menu_bar: true,
            status_bar: true,
            search_options: None,
            status_bar_items: None,
            prompt_line: false,
            prompt_keys: false,
            dock_keys: false,
            panel_keys: false,
            dock: None,
            dock_interior: None,
            dock_grip_hovered: false,
            dock_focused: false,
            explorer: None,
            menu: None,
            dropdowns: Vec::new(),
            menu_keys: Vec::new(),
            menu_bar_items: super::menu::MenuBar::default(),
            suggestions: None,
            popups: Vec::new(),
            card: None,
            theme_info: None,
            browser: None,
            trust: None,
            modal: None,
            settings: None,
            settings_dialog: None,
            settings_entry: Vec::new(),
            keybinding: None,
            keybinding_table: None,
            keybinding_dialog: None,
            event_debug: None,
            calibration: None,
            splits: None,
            window: None,
            panel: None,
        }
    }
}

/// How many one-cell rows the chrome column spends, given which are visible.
///
/// The free form exists because `render` needs this number *before* it has a
/// `Frame` — the explorer's viewport row count is model state, and the
/// description cannot be built without it. It was written out there as a sum
/// of the same four bools, next to a comment pointing at `Frame::fixed_rows`
/// as the rule; now it is the rule, and [`Frame::fixed_rows`] is the form for
/// callers that already hold a description.
pub fn fixed_rows(menu_bar: bool, status_bar: bool, search_options: bool, prompt: bool) -> u16 {
    menu_bar as u16 + status_bar as u16 + search_options as u16 + prompt as u16
}

/// Columns the editor keeps for itself, whatever the dock asks for.
pub const EDITOR_MIN: u16 = 20;
/// Narrower than this and a dock is not worth showing at all.
pub const DOCK_MIN: u16 = 24;

/// How wide the dock actually gets, or `None` when it does not fit.
///
/// **The one copy of the rule.** It lived here *and* in `compute_dock_split`,
/// each with its own `EDITOR_MIN`/`DOCK_MIN`, and the frame-parity test
/// exercised this copy while the editor painted from the other — so the test
/// could stay green through a divergence in the very geometry it exists to
/// pin. `compute_dock_split` now carves its rectangles from this answer.
pub fn dock_width(requested: Option<u16>, frame_width: u16) -> Option<u16> {
    let requested = requested?;
    let max_dock = frame_width.saturating_sub(EDITOR_MIN);
    (max_dock >= DOCK_MIN).then(|| requested.min(max_dock).max(1))
}

impl Frame {
    /// The dock's bail-out rule, applied to this description.
    ///
    /// This is **app logic keyed on the frame width**, not a layout
    /// constraint — it decides whether a dock exists at all. `build()` cannot
    /// read geometry, so it is resolved here, from the last known frame width,
    /// before the description is built.
    pub fn resolve_dock(mut self, frame_width: u16) -> Frame {
        self.dock = dock_width(self.dock, frame_width);
        self
    }

    /// Rows whose height is fixed at one cell when visible.
    ///
    /// When the frame is shorter than this, `fresh-ui` and ratatui starve
    /// *different* rows (pinned by `squeeze_band_starves_a_different_row_than_ratatui`).
    /// Callers that care decide which rows to drop themselves rather than
    /// inheriting either engine's starvation order.
    pub fn fixed_rows(&self) -> u16 {
        fixed_rows(
            self.menu_bar,
            self.status_bar,
            self.search_options.is_some(),
            self.prompt_line,
        )
    }
}

/// The chrome column: everything right of the dock.
///
/// Named so a layer can be confined to it. The painter said "beside the dock"
/// by being handed `chrome_area` instead of the whole frame; a layer says it
/// with `within`, which puts the statement where the placing happens.
pub fn chrome_key() -> fresh_ui::Key {
    fresh_ui::Key::Str("chrome_column".into())
}

/// The name of a window's identity key and persistence scope.
///
/// One function because two callers have to agree and neither can check the
/// other: `frame_tree` writes the scope into the tree, and
/// `Editor::forget_window_ui_state` drops it when the window closes. Spelled
/// apart, a rename in one place leaks every closed window's values forever and
/// nothing fails.
pub fn window_scope(id: u64) -> String {
    format!("window:{id}")
}

/// The frame description: one `Host` per region.
///
/// **Every** region is present, hidden ones at zero size, mirroring the
/// `Length(0)` constraints the ratatui layout uses. A hidden row still has a
/// position and callers use it — the suggestions popup anchors to the prompt
/// row whether or not that row is drawn — so omitting it would silently move
/// whatever hangs off it.
pub fn frame_tree(f: Frame) -> Node<UiMsg> {
    let cells = |on: bool| Sizing::Cells(on as u16);
    // Native: the panel paints itself and answers its own pointer. It keeps
    // the region key, so every caller that asks for `HostRegion::Explorer`'s
    // rectangle still gets one.
    let sidebar = |e: &super::file_explorer::Explorer| {
        named(HostRegion::Explorer, super::file_explorer::explorer(e)).w(Sizing::Cells(e.cols))
    };
    // The body: the grid, with a `Host` under it for what belongs to no pane.
    // Each pane carries its own `Host` (see `splits::live_pane`), so the
    // rectangle a pane is painted at is the rectangle layout gave it. What
    // this one is left is the panes' shared preamble and the separators
    // between them — the gaps, which belong to neither side.
    let body_region = |f: &Frame| -> Node<UiMsg> {
        match &f.splits {
            Some(s) => named(
                HostRegion::Body,
                fresh_ui::stack()
                    .children([host(HostRegion::Body.id()), super::splits::overlay(s)]),
            ),
            None => region(HostRegion::Body),
        }
    };
    let body: Node<UiMsg> = match &f.explorer {
        Some(e) if e.on_left => row()
            .flex(1)
            .children([sidebar(e), body_region(&f).flex(1)]),
        Some(e) => row()
            .flex(1)
            .children([body_region(&f).flex(1), sidebar(e)]),
        // No sidebar: the explorer is still in the tree taking nothing, so it
        // has a rectangle to report and the body's own is unaffected.
        None => row().flex(1).children([
            body_region(&f).flex(1),
            region(HostRegion::Explorer).w(Sizing::Cells(0)),
        ]),
    };
    let chrome = col().flex(1).key(chrome_key()).children([
        // Native: the bar's own row. It keeps the region key so every caller
        // that asks for `HostRegion::MenuBar`'s rectangle still gets one — a
        // region that has gone native is still a region.
        named(
            HostRegion::MenuBar,
            super::menu::menu_bar(&f.menu_bar_items),
        )
        .h(cells(f.menu_bar)),
        body,
        // Native: the tree measures the bar from its own elements. The row
        // keeps its region key, so every caller that asks for
        // `HostRegion::StatusBar`'s rectangle still gets one.
        match &f.status_bar_items {
            Some(bar) => named(HostRegion::StatusBar, super::status_bar::status_bar(bar))
                .h(cells(f.status_bar)),
            None => region(HostRegion::StatusBar).h(cells(f.status_bar)),
        },
        // Native: the tree measures this row from its own text. See
        // `shell::search_options`.
        named(
            HostRegion::SearchOptions,
            super::search_options::search_options(
                f.search_options.as_ref().unwrap_or(&Default::default()),
            ),
        )
        .h(cells(f.search_options.is_some())),
        region(HostRegion::PromptLine).h(cells(f.prompt_line)),
    ]);
    // Overlays, in paint order. Menu-bar dropdowns first, then a context menu
    // over them — the order `layer_rank::MENU` below `layer_rank::CONTEXT_MENU`
    // states in the precedence table, expressed here as the order they are
    // declared in.
    //
    // **The two panel keyboards lead, because they are the floor of the
    // routable band.** `DOCK` was the lowest rank and `FLOATING_MODAL` the
    // next, both under `POPUP` — the R1 rank-inversion fix, which says a
    // prompt, a popup or a menu takes a key before a focused dock or centred
    // modal does. Declared first, they are exactly that: `Modality::Focus`
    // confines the keyboard to the topmost such layer, and the topmost is the
    // one declared last.
    //
    // They carry no state, which is why the window scope around this column
    // is not the problem it would be for the dock's *content*: a keyboard
    // seam has nothing to lose when a workspace switch rebuilds it, and it
    // re-autofocuses on the next frame.
    let chrome = match f.dock_keys {
        true => chrome.child(super::panel::keys_layer(super::widgets::Slot::Dock)),
        false => chrome,
    };
    let chrome = match f.panel_keys {
        true => chrome.child(super::panel::keys_layer(super::widgets::Slot::Floating)),
        false => chrome,
    };
    // The overlay prompt's card, over everything the frame holds and under the
    // menus — a context menu opened from inside it still paints on top, the
    // same declaration-order rule the dropdowns follow.
    //
    // **Before the suggestion list, because the list can be anchored to one of
    // its bands.** A layer is placed against a rectangle the layout has
    // already produced, and the overlay form of the list names the card's
    // results band as that rectangle (`prompt::Place::InCard`). Declared the
    // other way round the anchor names a node that has not been laid out yet,
    // and the list lands nowhere — with the card's own paint on top of it,
    // which is how it went unnoticed. Paint order agrees: the list belongs
    // over the card it sits in.
    let chrome = match &f.card {
        Some(c) => chrome.child(super::overlay_prompt::card(c)),
        None => chrome,
    };
    // The suggestion list, above the prompt row it belongs to. Declared before
    // the menus so a context menu opened over it still paints on top — the
    // same "order of declaration is paint order" rule the dropdowns follow.
    let chrome = match &f.suggestions {
        Some(s) => chrome.child(super::prompt::suggestions_layer(s)),
        None => chrome,
    };
    // Popups sit over the frame and over the prompt's list, and under the
    // menus — a context menu opened from a popup row still paints on top, the
    // same declaration-order rule everything else here follows.
    let chrome = chrome.children(super::popup::placed_layers(&f.popups));
    // **The prompt's keyboard, over the popups and under the menus.** This is
    // `layer_rank`'s `MENU > PROMPT > POPUP` said as declaration order instead
    // of as three integers: a `Modality::Focus` layer confines the keyboard to
    // itself, and `topmost_modal` picks the one declared last, so the ordering
    // *is* the precedence. It paints nothing — the prompt's row, card and
    // suggestion list are described above and elsewhere.
    let chrome = match f.prompt_keys {
        true => chrome.child(super::prompt::keys_layer()),
        false => chrome,
    };
    let chrome = match super::menu::dropdown_chain(&f.dropdowns, &f.menu_keys) {
        Some(chain) => chrome.child(chain),
        None => chrome,
    };
    let chrome = match &f.menu {
        Some(menu) => chrome.child(super::context_menu::context_menu(menu)),
        None => chrome,
    };
    // The file-open dialog, over the frame and under the inspector — the
    // order `chrome:file_browser`'s `z = 130` and `chrome:theme_inspect`'s
    // 190 had.
    let chrome = match &f.browser {
        Some(b) => chrome.child(super::file_browser::layer(b)),
        None => chrome,
    };
    // **The window's half of the frame, under one key and one persistence
    // scope.** Everything above belongs to the active workspace: its chrome
    // column, its splits, its explorer, and the overlays that hang off them.
    // A layer is out of flow, so carrying them on the column rather than on
    // the row moves no rectangle — what it moves is which scope they are in
    // and which key bounds their identity.
    //
    // Without this the tree names no window at all, and two workspaces'
    // subtrees match each other: `SplitManager::next_split_id` starts at 1 in
    // every window, so window A's first pane and window B's first pane carry
    // the same `(type, key)` at the same position. `fresh_ui::scope` is one
    // node for the key and the `PersistenceScope`, because a subtree that
    // declares only one of them is silently wrong in either direction.
    let window_area = match f.window {
        Some(id) => fresh_ui::scope(window_scope(id), chrome),
        None => chrome,
    };
    // Native around a `Host` content leaf: the column answers its own pointer
    // and carries its width grip, while the panel's widgets stay the widget
    // runtime's until `WidgetSpec` becomes a `Node`. A hidden dock is still in
    // the tree at zero width, like every other region.
    //
    // **Outside the window scope, deliberately.** The dock is editor-global —
    // its state is `Editor.dock`, it lists and switches between *all* windows,
    // and it is meant to survive a workspace switch. Inside the window key its
    // element state would follow whichever window is active, and its sessions
    // list would lose its scroll on every switch.
    let frame = row().children([
        match f.dock {
            Some(w) => named(
                HostRegion::Dock,
                super::dock::dock(f.dock_interior.clone(), f.dock_grip_hovered, f.dock_focused),
            )
            .w(Sizing::Cells(w)),
            None => region(HostRegion::Dock).w(Sizing::Cells(0)),
        },
        window_area,
    ]);
    // From here down: editor-scoped, like the dock. Each covers or dims the
    // *whole* frame, each is state on the `Editor` rather than on a `Window`,
    // and none of them should be discarded because the active workspace
    // changed.
    // The inspector, over everything — the trigger that opens it fires under
    // any chrome, so the answer has to be visible over that chrome. This is
    // what `chrome:theme_inspect`'s `z = 190` said.
    let frame = match &f.theme_info {
        Some(t) => frame.child(super::theme_info::layer(t)),
        None => frame,
    };
    // A full-screen modal, over everything but the trust prompt — which
    // outranks every one of them and is the security gate that must.
    let frame = match f.modal {
        Some(slot) => frame.child(super::modal::layer(slot)),
        None => frame,
    };
    // The settings dialog's box. Like the keybinding editor's below it, this
    // contributes a rectangle and nothing else — `PointerMode::Ignore`, so the
    // modal slot behind it is still the one asked.
    let frame = match &f.settings {
        Some(c) => frame.child(super::settings::layer(Some(c))),
        None => frame,
    };
    // The entry-edit stack, over the box and under the prompts: each level
    // dims what is below it, which is one `Scrim` per layer rather than the
    // painter's `apply_dimming` once around the loop.
    let mut frame = frame;
    for d in &f.settings_entry {
        frame = frame.child(super::entry::layer(d));
    }
    // Its open dialog, **after** the box for the same reason the keybinding
    // editor's is: layers are offered the pointer in reverse declaration
    // order, so a dialog declared first would be covered by the box. And
    // after the entry stack, because the two prompts it opens sit over it.
    let frame = match &f.settings_dialog {
        Some(d) => frame.child(super::settings::dialog_layer(d)),
        None => frame,
    };
    // The keybinding editor's box, over the modal slot that routes its
    // pointer — the same order and for the same reason as the floating panel
    // below: the box is asked first, and the slot behind it catches whatever
    // the box does not answer.
    let frame = match &f.keybinding {
        Some(c) => frame.child(super::keybinding::layer(c, f.keybinding_table.as_ref())),
        None => frame,
    };
    // Its open dialog, **after** the box: layers are offered the pointer in
    // reverse declaration order, so a dialog declared before the box would be
    // covered by it and its fields would never see a press.
    let frame = match &f.keybinding_dialog {
        Some(d) => frame.child(super::keybinding::dialog_layer(d)),
        None => frame,
    };
    // The event-debug dialog, which like the wizard below carries its own
    // exclusivity and its own scrim.
    let frame = match &f.event_debug {
        Some(d) => frame.child(super::event_debug::sized(d)),
        None => frame,
    };
    // The calibration wizard, over the modal slot it shares a rank with. It
    // brings its own exclusivity and its own scrim, so the slot beneath it
    // contributes nothing but the routing the applier no longer needs.
    let frame = match &f.calibration {
        Some(c) => frame.child(super::calibration::sized(c)),
        None => frame,
    };
    // The floating plugin panel's frame, over the modal slot that routes its
    // pointer. **After it, deliberately**: layers are offered the pointer in
    // reverse declaration order, so the `[×]` here is asked before the modal's
    // claim-everything surface, and every other node of the frame is
    // transparent and falls through to it. That is what lets one button
    // migrate without the interior having to.
    let frame = match &f.panel {
        Some(p) => frame.child(super::panel::layer_for(p)),
        None => frame,
    };
    // The trust prompt, over everything the frame holds. It is drawn dead last
    // today for the same reason — it dims the *entire* frame, the dock
    // included, and centres in the whole window rather than beside the dock.
    let frame = match &f.trust {
        Some(t) => frame.child(super::trust::layer(t)),
        None => frame,
    };
    // Two capture-phase observers, outermost and last: each sees the press
    // before anything below it. The inspector's trigger is inside the dock's
    // blur observer, which is the order their `z` values had (190 under 195),
    // and it is the one that stops the flow — Ctrl+Right-Click *is* the
    // gesture, where the blur is a side effect of one aimed elsewhere.
    let frame = super::theme_info::inspect_trigger(frame);
    // Outermost observer of the right-click channel: it clears the two
    // left-click-only menus and lets the click continue, so it must see the
    // click before the surface it is aimed at claims it.
    let frame = super::splits::tab_menu_guard(frame);
    match f.dock {
        Some(w) => super::dock::blur_observer(w, frame),
        None => frame,
    }
}

fn region(r: HostRegion) -> Node<UiMsg> {
    named(r, host(r.id()))
}

/// Tag a region's node with the region's name, whether it is still a `Host`
/// leaf or has gone native.
///
/// The name is what [`regions_of`] looks up, so migrating a region does not
/// change how its rectangle is found: it is a layout query either way, and a
/// region that paints nothing at all — a hidden row, a bar with no labels —
/// still has one.
fn named(r: HostRegion, n: Node<UiMsg>) -> Node<UiMsg> {
    n.key(region_key(r))
}

/// The display-list key a native region carries.
pub fn region_key(r: HostRegion) -> fresh_ui::Key {
    fresh_ui::Key::Pair("region".into(), r.id())
}

/// The rectangle the shell assigns each visible region, for a frame of `size`.
///
/// This is the shell's answer to the question `Editor::render` currently
/// answers with `compute_dock_split` + a vertical `Layout` +
/// `split_file_explorer_area`. Running both and comparing is how the frame
/// migrates onto `fresh-ui` without a flag day: see
/// [`assert_parity`].
/// Every region's rectangle, read off a tree the caller already laid out.
///
/// A layout query, not a paint one: `Ui::rect_of` is the rectangle layout
/// assigned, so a region reports it whether it paints a cell or not. Reading
/// the display list instead would lose exactly the regions that paint nothing
/// — a hidden row, a menu bar with no labels — and lose them silently.
///
/// [`region_rects`] is the standalone form, for tests and for callers with no
/// `Ui` of their own; this is the form `render` uses, so the frame is laid out
/// once and both the rectangles and the painted output come from it.
pub fn regions_of(
    ui: &fresh_ui::Ui<UiMsg>,
    size: ratatui::layout::Rect,
) -> Vec<(HostRegion, ratatui::layout::Rect)> {
    HostRegion::ALL
        .into_iter()
        .filter_map(|r| {
            let e = ui.find_by_key(&region_key(r))?;
            let rect = ui.rect_of(e);
            Some((r, super::screen_rect(rect, size)))
        })
        .collect()
}

pub fn region_rects(
    f: Frame,
    size: ratatui::layout::Rect,
) -> Vec<(HostRegion, ratatui::layout::Rect)> {
    use fresh_ui::{Size, Ui};

    let mut ui: Ui<UiMsg> = Ui::new();
    ui.frame(frame_tree(f), Size::new(size.width, size.height));
    regions_of(&ui, size)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::event::BufferId;
    use crate::view::shell::msg::{UiFact, UiMsg};
    use crate::view::shell::splits::{leaf_key, PaneControls, Splits};
    use crate::view::shell::widgets::Slot;
    use crate::view::split::SplitNode;
    use fresh_core::SplitId;
    use fresh_ui::{Size, Ui};

    /// One pane, so the two windows collide at the *first* split id — which is
    /// the case that matters: `SplitManager::next_split_id` starts at 1 in
    /// every window, so this is the default layout of two workspaces rather
    /// than a contrived one.
    fn one_pane_in(window: Option<u64>) -> Frame {
        Frame {
            window,
            splits: Some(Splits {
                root: SplitNode::leaf(BufferId(1), SplitId(1)),
                maximized: None,
                chrome: Default::default(),
                controls: PaneControls {
                    maximize: false,
                    close: false,
                },
                groups: Default::default(),
            }),
            ..Frame::default()
        }
    }

    /// The element the first pane is reconciled onto, across two frames.
    fn pane_across(
        a: Frame,
        b: Frame,
    ) -> (Option<fresh_ui::ElementId>, Option<fresh_ui::ElementId>) {
        let key = leaf_key(crate::model::event::LeafId(SplitId(1)));
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(frame_tree(a), Size::new(80, 24));
        let first = ui.find_by_key(&key);
        ui.frame(frame_tree(b), Size::new(80, 24));
        (first, ui.find_by_key(&key))
    }

    /// **The bug the window scope exists to prevent.** Reconciliation is by
    /// `(type, key)` at a position; two windows' first panes carry the same
    /// key at the same position, so without something naming the window the
    /// tree matches them and window B's pane inherits window A's element —
    /// and, once panes own state, its scroll offset.
    #[test]
    fn two_windows_first_panes_are_not_one_element() {
        let (a, b) = pane_across(one_pane_in(Some(1)), one_pane_in(Some(2)));
        assert!(a.is_some() && b.is_some(), "both frames describe a pane");
        assert_ne!(
            a, b,
            "window 2's pane must not be reconciled onto window 1's element"
        );
    }

    /// The control, and the reason the assertion above is not vacuous: with no
    /// window named, the two frames *do* land on one element. This is what the
    /// tree did before the scope, spelled out so a future change that quietly
    /// stops keying the window fails the test above instead of passing it for
    /// the wrong reason.
    #[test]
    fn with_no_window_named_the_two_frames_share_the_element() {
        let (a, b) = pane_across(one_pane_in(None), one_pane_in(None));
        assert!(a.is_some());
        assert_eq!(a, b, "nothing distinguishes the two frames");
    }

    /// Rebuilding the *same* window is not a switch: the pane keeps its
    /// element, so nothing a component owns is thrown away on an ordinary
    /// frame. A scope that discarded every frame would be worse than none.
    #[test]
    fn the_same_window_keeps_its_pane_across_frames() {
        let (a, b) = pane_across(one_pane_in(Some(1)), one_pane_in(Some(1)));
        assert!(a.is_some());
        assert_eq!(a, b, "same window, same element");
    }

    /// The tree's scope name and the editor's `forget_window_ui_state` have to
    /// agree, and neither can check the other — so the shared spelling is
    /// pinned here. If this changes, every closed window's values leak and
    /// nothing else fails.
    #[test]
    fn a_windows_scope_is_named_after_its_id() {
        assert_eq!(window_scope(7), "window:7");
        assert_ne!(window_scope(1), window_scope(10));
    }

    /// **This is `layer_rank` now**, and the order below is the whole of it
    /// for the surfaces that used to be dispatched by integer. Each is a
    /// `Modality::Focus` layer, so `topmost_modal` picks the one declared
    /// last, and declaration order *is* keyboard precedence.
    ///
    /// The case pinned here is the one that broke: a focused dock with a
    /// plugin panel over it — the orchestrator's right-click context menu.
    /// `FLOATING_MODAL > DOCK` said the panel takes the key, and it has to,
    /// because the dock's own `widget_panel_key` answers Escape by blurring
    /// and would eat the key the menu needs to close on.
    #[test]
    fn a_panels_keyboard_outranks_a_focused_docks() {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            frame_tree(Frame {
                dock: Some(30),
                dock_keys: true,
                panel_keys: true,
                menu_bar: false,
                status_bar: false,
                ..Frame::default()
            }),
            Size::new(120, 40),
        );
        let got = ui.dispatch(fresh_ui::Input::Key(fresh_ui::KeyPress {
            code: fresh_ui::KeyCode::Esc,
            mods: fresh_ui::Mods::NONE,
        }));
        assert!(
            got.msgs
                .iter()
                .any(|m| matches!(m, UiMsg::Ui(UiFact::PanelKey(Slot::Floating)))),
            "the panel's layer is the one containment finds: {:?}",
            got.msgs
        );
        assert!(
            !got.msgs
                .iter()
                .any(|m| matches!(m, UiMsg::Ui(UiFact::PanelKey(Slot::Dock)))),
            "and the dock's does not also get it: {:?}",
            got.msgs
        );
    }

    /// The partner: with no panel up, the focused dock's layer is the one
    /// that answers. Without this the test above would pass on a frame that
    /// had stopped declaring the dock's layer at all.
    #[test]
    fn a_focused_dock_answers_when_no_panel_is_over_it() {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            frame_tree(Frame {
                dock: Some(30),
                dock_keys: true,
                menu_bar: false,
                status_bar: false,
                ..Frame::default()
            }),
            Size::new(120, 40),
        );
        let got = ui.dispatch(fresh_ui::Input::Key(fresh_ui::KeyPress {
            code: fresh_ui::KeyCode::Esc,
            mods: fresh_ui::Mods::NONE,
        }));
        assert!(
            got.msgs
                .iter()
                .any(|m| matches!(m, UiMsg::Ui(UiFact::PanelKey(Slot::Dock)))),
            "the dock's layer answers: {:?}",
            got.msgs
        );
    }

    /// And the prompt beats both, which is `PROMPT > FLOATING_MODAL > DOCK`
    /// — the R1 rank-inversion fix, kept as declaration order.
    #[test]
    fn a_prompt_outranks_both_panels() {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            frame_tree(Frame {
                dock: Some(30),
                dock_keys: true,
                panel_keys: true,
                prompt_keys: true,
                menu_bar: false,
                status_bar: false,
                ..Frame::default()
            }),
            Size::new(120, 40),
        );
        let got = ui.dispatch(fresh_ui::Input::Key(fresh_ui::KeyPress {
            code: fresh_ui::KeyCode::Esc,
            mods: fresh_ui::Mods::NONE,
        }));
        assert!(
            got.msgs
                .iter()
                .any(|m| matches!(m, UiMsg::Ui(UiFact::PromptKey))),
            "the prompt's layer is the topmost: {:?}",
            got.msgs
        );
        assert!(
            !got.msgs
                .iter()
                .any(|m| matches!(m, UiMsg::Ui(UiFact::PanelKey(_)))),
            "and neither panel also gets it: {:?}",
            got.msgs
        );
    }

    /// **The exclusive slot must not steal the keyboard from the layer that
    /// wants it.** `modal::layer(FloatingPanel)` claims the pointer for the
    /// panel; saying `Exclusive` there made it the focus scope, and with
    /// nothing focusable inside it focus was dropped and the panel's own
    /// keyboard layer stopped being found. `Modality::Pointer` is the claim
    /// it actually makes, and this is the frame that proves it.
    #[test]
    fn the_panels_pointer_slot_leaves_its_keyboard_layer_alone() {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            frame_tree(Frame {
                dock: Some(30),
                dock_keys: true,
                panel_keys: true,
                modal: Some(crate::view::shell::modal::Slot::FloatingPanel),
                menu_bar: false,
                status_bar: false,
                ..Frame::default()
            }),
            Size::new(120, 40),
        );
        let got = ui.dispatch(fresh_ui::Input::Key(fresh_ui::KeyPress {
            code: fresh_ui::KeyCode::Esc,
            mods: fresh_ui::Mods::NONE,
        }));
        assert!(
            got.msgs
                .iter()
                .any(|m| matches!(m, UiMsg::Ui(UiFact::PanelKey(Slot::Floating)))),
            "the slot's pointer claim did not take the keyboard: {:?}",
            got.msgs
        );
    }
}
