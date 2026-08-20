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
    pub search_options: bool,
    pub prompt_line: bool,
    /// Column width, already resolved against the frame width.
    pub dock: Option<u16>,
    /// (columns, on_left)
    pub explorer: Option<(u16, bool)>,
    /// The open context menu, if any. An overlay is an ordinary child of the
    /// tree rather than a separately-ranked surface — which is the whole point
    /// of moving them here.
    pub menu: Option<super::context_menu::Menu>,
    /// The open menu-bar dropdown chain, outermost level first. Empty when no
    /// menu is open.
    pub dropdowns: Vec<super::menu::DropdownLevel>,
}

impl Default for Frame {
    fn default() -> Self {
        Frame {
            menu_bar: true,
            status_bar: true,
            search_options: false,
            prompt_line: false,
            dock: None,
            explorer: None,
            menu: None,
            dropdowns: Vec::new(),
        }
    }
}

impl Frame {
    /// The dock's bail-out rules from `compute_dock_split`.
    ///
    /// This is **app logic keyed on the frame width**, not a layout
    /// constraint — it decides whether a dock exists at all. `build()` cannot
    /// read geometry, so it is resolved here, from the last known frame width,
    /// before the description is built.
    pub fn resolve_dock(mut self, frame_width: u16) -> Frame {
        const EDITOR_MIN: u16 = 20;
        const DOCK_MIN: u16 = 24;
        self.dock = self.dock.and_then(|requested| {
            let max_dock = frame_width.saturating_sub(EDITOR_MIN);
            (max_dock >= DOCK_MIN).then(|| requested.min(max_dock).max(1))
        });
        self
    }

    /// Rows whose height is fixed at one cell when visible.
    ///
    /// When the frame is shorter than this, `fresh-ui` and ratatui starve
    /// *different* rows (pinned by `squeeze_band_starves_a_different_row_than_ratatui`).
    /// Callers that care decide which rows to drop themselves rather than
    /// inheriting either engine's starvation order.
    pub fn fixed_rows(&self) -> u16 {
        self.menu_bar as u16
            + self.status_bar as u16
            + self.search_options as u16
            + self.prompt_line as u16
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
    let body: Node<UiMsg> = match f.explorer {
        Some((cols, true)) => row().flex(1).children([
            region(HostRegion::Explorer).w(Sizing::Cells(cols)),
            region(HostRegion::Body).flex(1),
        ]),
        Some((cols, false)) => row().flex(1).children([
            region(HostRegion::Body).flex(1),
            region(HostRegion::Explorer).w(Sizing::Cells(cols)),
        ]),
        // No sidebar: the explorer is still in the tree taking nothing, so it
        // has a rectangle to report and the body's own is unaffected.
        None => row().flex(1).children([
            region(HostRegion::Body).flex(1),
            region(HostRegion::Explorer).w(Sizing::Cells(0)),
        ]),
    };
    let chrome = col().flex(1).children([
        region(HostRegion::MenuBar).h(cells(f.menu_bar)),
        body,
        region(HostRegion::StatusBar).h(cells(f.status_bar)),
        region(HostRegion::SearchOptions).h(cells(f.search_options)),
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
    let frame = frame.children(super::menu::dropdown_chain(&f.dropdowns));
    match &f.menu {
        Some(menu) => frame.child(super::context_menu::context_menu(menu)),
        None => frame,
    }
}

fn region(r: HostRegion) -> Node<UiMsg> {
    host(r.id())
}

/// The rectangle the shell assigns each visible region, for a frame of `size`.
///
/// This is the shell's answer to the question `Editor::render` currently
/// answers with `compute_dock_split` + a vertical `Layout` +
/// `split_file_explorer_area`. Running both and comparing is how the frame
/// migrates onto `fresh-ui` without a flag day: see
/// [`assert_parity`].
/// The host regions of a display list the caller already produced.
///
/// [`region_rects`] is the standalone form, for tests and for callers with no
/// `Ui` of their own; this is the form `render` uses, so the frame is laid out
/// once and both the rectangles and the painted output come from it.
pub fn regions_of(
    spec: &fresh_ui::LayoutSpec,
    size: ratatui::layout::Rect,
) -> Vec<(HostRegion, ratatui::layout::Rect)> {
    use fresh_ui::Draw;
    let mut out: Vec<(HostRegion, ratatui::layout::Rect)> = spec
        .items
        .iter()
        .filter_map(|it| match &it.draw {
            Draw::Host(id) => HostRegion::from_host_id(*id).map(|r| {
                (
                    r,
                    ratatui::layout::Rect {
                        x: size.x.saturating_add(it.rect.x.max(0) as u16),
                        y: size.y.saturating_add(it.rect.y.max(0) as u16),
                        width: it.rect.w,
                        height: it.rect.h,
                    },
                )
            }),
            _ => None,
        })
        .collect();
    out.sort_by_key(|(r, _)| *r);
    out
}

pub fn region_rects(
    f: Frame,
    size: ratatui::layout::Rect,
) -> Vec<(HostRegion, ratatui::layout::Rect)> {
    use fresh_ui::{Size, Ui};

    let mut ui: Ui<UiMsg> = Ui::new();
    let spec = ui.frame(frame_tree(f), Size::new(size.width, size.height));
    regions_of(spec, size)
}
