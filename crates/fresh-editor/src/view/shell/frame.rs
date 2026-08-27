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

/// Which regions are visible, and how wide the sized ones are.
///
/// Every field here is *app state*: `build()` cannot read geometry, so
/// decisions that today read `size` at the top of `render` — the dock's
/// bail-out, the explorer's column count — are resolved from state before the
/// description is built. See [`Frame::resolve_dock`].
#[derive(Clone, Debug, PartialEq, Eq)]
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
    /// Column width, already resolved against the frame width.
    pub dock: Option<u16>,
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
}

impl Default for Frame {
    fn default() -> Self {
        Frame {
            menu_bar: true,
            status_bar: true,
            search_options: None,
            status_bar_items: None,
            prompt_line: false,
            dock: None,
            explorer: None,
            menu: None,
            dropdowns: Vec::new(),
            menu_keys: Vec::new(),
            menu_bar_items: super::menu::MenuBar::default(),
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
    let body: Node<UiMsg> = match &f.explorer {
        Some(e) if e.on_left => row()
            .flex(1)
            .children([sidebar(e), region(HostRegion::Body).flex(1)]),
        Some(e) => row()
            .flex(1)
            .children([region(HostRegion::Body).flex(1), sidebar(e)]),
        // No sidebar: the explorer is still in the tree taking nothing, so it
        // has a rectangle to report and the body's own is unaffected.
        None => row().flex(1).children([
            region(HostRegion::Body).flex(1),
            region(HostRegion::Explorer).w(Sizing::Cells(0)),
        ]),
    };
    let chrome = col().flex(1).children([
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
    let frame = row().children([
        region(HostRegion::Dock).w(Sizing::Cells(f.dock.unwrap_or(0))),
        chrome,
    ]);
    // Overlays, in paint order. Menu-bar dropdowns first, then a context menu
    // over them — the order `layer_rank::MENU` below `layer_rank::CONTEXT_MENU`
    // states in the precedence table, expressed here as the order they are
    // declared in.
    let frame = match super::menu::dropdown_chain(&f.dropdowns, &f.menu_keys) {
        Some(chain) => frame.child(chain),
        None => frame,
    };
    match &f.menu {
        Some(menu) => frame.child(super::context_menu::context_menu(menu)),
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
