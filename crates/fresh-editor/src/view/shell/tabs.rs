//! A pane's tab strip, as a description.
//!
//! **The last painter-recorded-rectangle hit test.** `TabsRenderer` laid the
//! tabs out — names resolved and elided, a `×` beside each, the `+` after the
//! last, `<` and `>` when they overflowed — painted them, and filed every
//! rectangle it had just placed into a `TabLayout` for `hit_test` to compare
//! a cell against on the next event. The strip's node above it reported a
//! coordinate (`PaneTabsPress { x, y }`), and the applier resolved it against
//! that record: which tab, its name or its `×`, the `+`, an arrow. The same
//! record fed the web's tab bar, the hover, the context menu, and the drag's
//! drop zone.
//!
//! Every one of those is a node now. A tab's name and its `×` are gestures
//! keyed by the tab's target, so a press names the tab it landed on, a right
//! press names the tab to open a menu for, and the pointer's Enter and Leave
//! are the hover. A press on a name captures the pointer, which is the drag:
//! the moves and the release come back to that node, and the drop zone is
//! computed from rectangles read off the tree by key. The web reads the same
//! rectangles ([`rects`]).
//!
//! What stays a model function is what was never paint: which name a tab
//! shows (`resolve_tab_names` — a path's shortest unique tail), what its
//! label reads as, and the scroll offset the editor keeps so a newly active
//! tab is brought into view (`ensure_active_tab_visible`). The strip is laid
//! out inside a `layout_reader`, because the one thing it needs that only
//! layout knows is its width: the name cap — full names when they all fit,
//! twenty-five columns each when they do not — and the visible window over
//! the tabs both depend on it.

use std::rc::Rc;

use fresh_ui::{gesture, row, text, Event, GestureKind, Key, MouseButton, Node, Sizing};

use crate::app::shell_host::shell_theme::{attrs, pair};
use crate::app::types::HoverTarget;
use crate::model::event::LeafId;
use crate::view::split::TabTarget;
use crate::view::ui::tabs::{elided_tab_name, NEW_TAB_BUTTON_WIDTH, TAB_NAME_MAX_COLS};

use super::msg::{UiFact, UiMsg};
use super::splits::{close_key, maximize_key, PaneControls};

/// One tab, as content: what its label is made of.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Tab {
    pub target: TabTarget,
    /// The resolved name — a filename, a path's shortest unique tail, or a
    /// group's name — before elision. The strip elides it against the width
    /// it is given.
    pub name: String,
    pub modified: bool,
    pub preview: bool,
    pub binary: bool,
}

/// A pane's strip: its tabs and what the pane says about them.
///
/// Not `PartialEq`, for the reason `Splits` is not: it carries the handle its
/// window is addressed by, and an `Anchor` is an identity rather than a value.
#[derive(Clone, Default)]
pub struct Strip {
    pub tabs: Vec<Tab>,
    /// The tab the pane is showing. `None` only for a pane the editor
    /// described no strip for, which lays out as an empty one.
    pub active: Option<TabTarget>,
    /// Whether this is the window's active pane: the active tab wears the
    /// accent only there.
    pub active_pane: bool,
    /// `(target, on its close button)`, from `HoverTarget::TabName` and
    /// `TabCloseButton` — which the tab's own nodes report.
    pub hover: Option<(TabTarget, bool)>,
    /// The pointer is on this pane's `+`.
    pub hover_plus: bool,
    /// Whether the tabs, at their full names, are wider than the strip —
    /// [`natural_width`] against the window's outer width, from the frame
    /// before this one.
    ///
    /// **Feedback, the same shape as the palette's column widths.** The cap is
    /// a rule about whether the names *fit*, so it needs the room; the room is
    /// layout's answer and the description is what layout is about to run on.
    /// Read back from the last frame it is one frame late after a resize and
    /// unset on the very first, which shows whole names — the conservative
    /// direction, and the window scrolls either way. Capping unconditionally
    /// instead, which is what this replaces, elided a 26-column name on a
    /// 160-column screen showing one tab.
    pub cap_names: bool,
    /// The handle the strip's window is addressed by, so the host can ask it
    /// to show a tab (`Anchor::reveal_key`).
    ///
    /// **This is all that is left of the scroll offset.** The editor used to
    /// hold one in columns and move it with `ensure_active_tab_visible`, which
    /// measured every tab a second time to find out where the active one was.
    /// Where the tabs are is the window's own answer; which one to show is the
    /// pane's, and that is what this carries. `None` for a strip described
    /// without one, which simply never reveals.
    pub reveal: Option<Rc<fresh_ui::behavior::Anchor>>,
    /// The word a preview tab carries after its name, localized.
    pub preview_label: String,
}

impl Strip {
    /// The cap to build this strip's labels with: `TAB_NAME_MAX_COLS` when the
    /// names do not fit, and no cap at all when they do.
    fn name_cap(&self) -> usize {
        match self.cap_names {
            true => TAB_NAME_MAX_COLS,
            false => usize::MAX,
        }
    }
}

impl std::fmt::Debug for Strip {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Strip")
            .field("tabs", &self.tabs)
            .field("active", &self.active)
            .field("active_pane", &self.active_pane)
            .field("hover", &self.hover)
            .field("hover_plus", &self.hover_plus)
            .field("reveal", &self.reveal.is_some())
            .field("preview_label", &self.preview_label)
            .finish()
    }
}

/// The right-hand cluster's state: which buttons it has and how they read.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Cluster {
    pub controls: PaneControls,
    /// The maximize button shows `⧉` (restore) while a pane is maximized.
    pub maximized: bool,
    pub hover_maximize: bool,
    pub hover_close: bool,
}

/// A tab's target as an ordinal, so a key can name it: buffers even, groups
/// odd.
fn ordinal(t: TabTarget) -> u64 {
    match t {
        TabTarget::Buffer(id) => (id.0 as u64) << 1,
        TabTarget::Group(leaf) => ((leaf.0 .0 as u64) << 1) | 1,
    }
}

pub fn tab_key(pane: LeafId, t: TabTarget) -> Key {
    Key::Pair(format!("tab:{}", pane.0 .0).into(), ordinal(t))
}

/// The whole tab — its name and its `×` — as one keyed span.
///
/// **What "show me this tab" means.** [`tab_key`] names the label alone, which
/// is the rectangle the drag's drop zone and the web read; revealing *that*
/// leaves the close button's last cell past the window's edge, so the window
/// correctly reports there is more and caps the end when you are looking at
/// the last tab. A tab is one thing, and this is the thing.
pub fn tab_span_key(pane: LeafId, t: TabTarget) -> Key {
    Key::Pair(format!("tab_span:{}", pane.0 .0).into(), ordinal(t))
}

pub fn close_tab_key(pane: LeafId, t: TabTarget) -> Key {
    Key::Pair(format!("tab_close:{}", pane.0 .0).into(), ordinal(t))
}

pub fn new_tab_key(pane: LeafId) -> Key {
    Key::Pair("tab_new".into(), pane.0 .0 as u64)
}

/// The window the tabs scroll inside — the strip less its control cluster.
///
/// Named so a reader can ask what is *on screen*: the tabs are ordinary nodes
/// in a window that does not virtualise, so every tab has a rectangle whether
/// or not it is showing, and only this box says which of them you can see.
pub fn tab_window_key(pane: LeafId) -> Key {
    Key::Pair("tab_window".into(), pane.0 .0 as u64)
}

/// The text a tab shows: the painter's `" {name}{*}{ preview}{ [BIN]} "`.
///
/// `cap` is `usize::MAX` for a name shown whole. The doc here used to say this
/// was "shared with the model's width arithmetic", naming `calculate_tab_widths`
/// and `ensure_active_tab_visible` — both deleted with the strip's hand layout.
/// What shares it now is [`natural_width`], which asks what these labels would
/// measure uncapped so the strip can decide whether to cap them at all.
pub fn label(t: &Tab, cap: usize, preview_label: &str) -> String {
    let name = elided_tab_name(&t.name, cap);
    let modified = if t.modified { "*" } else { "" };
    let preview = if t.preview {
        format!(" {preview_label}")
    } else {
        String::new()
    };
    let binary = if t.binary { " [BIN]" } else { "" };
    format!(" {name}{modified}{preview}{binary} ")
}

/// Columns a press on the strip's `<` or `>` moves the window by.
///
/// The painter's step, kept: a cap is a nudge along the strip, where the
/// library's default for a window — a whole screenful, as pressing a
/// scrollbar's track gives — would skip past every tab you were looking for.
pub const TAB_SCROLL_STEP_COLUMNS: u16 = 10;

const CLOSE: &str = "× ";
const PLUS: &str = " + ";

/// The strip's ground.
fn ground() -> String {
    pair("ui.tab_inactive_fg", "ui.tab_separator_bg")
}

/// A tab's own ink — `tab_styles`, in theme names.
fn tab_ink(s: &Strip, i: usize) -> (String, String) {
    let t = &s.tabs[i];
    let active = Some(t.target) == s.active;
    let (hover_name, hover_close) = match s.hover {
        Some((h, close)) if h == t.target => (!close, close),
        _ => (false, false),
    };
    let (fg, bg, mut a): (&str, &str, Vec<&str>) = if active {
        match s.active_pane {
            true => ("ui.tab_active_fg", "ui.tab_active_bg", vec!["bold"]),
            false => ("ui.tab_inactive_fg", "ui.tab_inactive_bg", vec!["bold"]),
        }
    } else if hover_name {
        ("ui.tab_inactive_fg", "ui.tab_hover_bg", vec![])
    } else {
        ("ui.tab_inactive_fg", "ui.tab_inactive_bg", vec![])
    };
    if t.preview {
        a.push("italic");
    }
    let name = attrs(fg, bg, &a);
    let close = match hover_close {
        true => attrs("ui.tab_close_hover_fg", bg, &a),
        false => name.clone(),
    };
    (name, close)
}

fn press(fact: UiFact) -> fresh_ui::Handler<UiMsg> {
    Rc::new(move |e: &Event| {
        if e.button != MouseButton::Left {
            return None;
        }
        e.stop();
        Some(UiMsg::Ui(fact.clone()))
    })
}

fn hover(t: Option<HoverTarget>) -> fresh_ui::Handler<UiMsg> {
    Rc::new(move |_: &Event| Some(UiMsg::Ui(UiFact::Hover(t.clone()))))
}

/// A tab's name: a press activates it and takes the pointer for the drag
/// that may follow; a right press opens its menu.
///
/// **Capture is the drag.** `PointerGrab::TabDrag` was a flag the legacy walk
/// read on every motion report to route it to `handle_tab_drag`; the node
/// that saw the press keeps the moves and the release, and the applier keeps
/// the same threshold — a press that never moved past it is a click.
fn name_node(pane: LeafId, t: TabTarget, s: String, ink: String) -> Node<UiMsg> {
    gesture(text(s).theme(ink))
        .key(tab_key(pane, t))
        .on(
            GestureKind::Press,
            Rc::new(move |e: &Event| {
                let (x, y) = (e.pos.x.max(0) as u16, e.pos.y.max(0) as u16);
                match e.button {
                    MouseButton::Left => {
                        e.capture_pointer();
                        e.stop();
                        Some(UiMsg::Ui(UiFact::PaneTabPress {
                            pane,
                            target: t,
                            x,
                            y,
                        }))
                    }
                    MouseButton::Right => {
                        e.stop();
                        Some(UiMsg::Ui(UiFact::PaneTabMenu {
                            pane,
                            target: t,
                            x,
                            y,
                        }))
                    }
                    _ => None,
                }
            }),
        )
        .on(
            GestureKind::Move,
            Rc::new(|e: &Event| {
                // The drag is the pointer the press captured; a bare move
                // over a tab is its hover, reported on enter.
                if !e.captured {
                    return None;
                }
                Some(UiMsg::Ui(UiFact::PaneTabDrag {
                    x: e.pos.x.max(0) as u16,
                    y: e.pos.y.max(0) as u16,
                }))
            }),
        )
        .on(
            GestureKind::Release,
            Rc::new(|e: &Event| {
                e.stop();
                Some(UiMsg::Ui(UiFact::PaneTabDrop))
            }),
        )
        .on_enter(hover(Some(HoverTarget::TabName(t, pane))))
        .on_leave(hover(None))
}

/// A tab's `×`. A right press here opens the tab's menu too, as the painter's
/// hit test answered `CloseButton` for the menu the same as `TabName`.
fn close_node(pane: LeafId, t: TabTarget, s: String, ink: String) -> Node<UiMsg> {
    gesture(text(s).theme(ink))
        .key(close_tab_key(pane, t))
        .on(
            GestureKind::Press,
            Rc::new(move |e: &Event| {
                let (x, y) = (e.pos.x.max(0) as u16, e.pos.y.max(0) as u16);
                e.stop();
                match e.button {
                    MouseButton::Left => Some(UiMsg::Ui(UiFact::PaneTabClose { pane, target: t })),
                    MouseButton::Right => Some(UiMsg::Ui(UiFact::PaneTabMenu {
                        pane,
                        target: t,
                        x,
                        y,
                    })),
                    _ => None,
                }
            }),
        )
        .on_enter(hover(Some(HoverTarget::TabCloseButton(t, pane))))
        .on_leave(hover(None))
}

/// The `+` after the last tab. It lights on hover, as the `×` beside a tab
/// name does and as the strip's own `<`/`>` caps do — the three are the same
/// kind of thing and read the same way.
fn plus_node(pane: LeafId, hovered: bool, s: String) -> Node<UiMsg> {
    let fg = match hovered {
        true => "ui.tab_close_hover_fg",
        false => "ui.tab_inactive_fg",
    };
    gesture(text(s).theme(pair(fg, "ui.tab_inactive_bg")))
        .key(new_tab_key(pane))
        .on_enter(hover(Some(HoverTarget::NewTabButton(pane))))
        .on_leave(hover(None))
        .on(
            GestureKind::Press,
            Rc::new(move |e: &Event| {
                if e.button != MouseButton::Left {
                    return None;
                }
                e.stop();
                Some(UiMsg::Ui(UiFact::PaneNewTab {
                    pane,
                    x: e.pos.x.max(0) as u16,
                    y: e.pos.y.max(0) as u16,
                }))
            }),
        )
}

/// A cell of the right-hand cluster: `□`/`⧉` or `×`, answering its own press
/// and reporting its own hover.
fn control(glyph: &'static str, hovered: bool, target: HoverTarget, fact: UiFact) -> Node<UiMsg> {
    let fg = match hovered {
        true => "ui.tab_close_hover_fg",
        false => "editor.line_number_fg",
    };
    gesture(text(glyph).theme(pair(fg, "ui.tab_separator_bg")))
        .on(GestureKind::Press, press(fact))
        .on_enter(hover(Some(target)))
        .on_leave(hover(None))
}

/// The cluster: `[gap] [gap] □ × [trail]`, over the columns reserved for it.
///
/// It used to carry the `>` itself, drawn when the tabs overflowed and
/// stepping the editor's own offset. The strip is a window now and the window
/// caps its own ends (`Draw::Overflow`), which lands the `>` in the cell
/// immediately left of this — where the cluster drew it — without anyone
/// deciding whether the tabs overflow. The column stays reserved so the
/// buttons sit where they always have.
fn cluster(pane: LeafId, c: Cluster) -> Node<UiMsg> {
    let one = Sizing::Cells(1);
    let mut cells: Vec<Node<UiMsg>> = vec![row().w(one), row().w(one)];
    if c.controls.maximize {
        let glyph = if c.maximized { "⧉" } else { "□" };
        cells.push(
            control(
                glyph,
                c.hover_maximize,
                HoverTarget::MaximizeSplitButton(pane),
                UiFact::PaneMaximize(pane),
            )
            .key(maximize_key(pane))
            .w(one),
        );
    }
    if c.controls.close {
        cells.push(
            control(
                "×",
                c.hover_close,
                HoverTarget::CloseSplitButton(pane),
                UiFact::PaneClose(pane),
            )
            .key(close_key(pane))
            .w(one),
        );
    }
    cells.push(row().w(one));
    row()
        .w(Sizing::Cells(c.controls.reserve()))
        .theme(ground())
        .children(cells)
}

/// What the strip's content would measure with every name shown whole.
///
/// **The one measurement left, and it is not a fitting decision.** The cap on
/// a tab name exists so one long filename cannot push every other tab off the
/// strip (issue #2650), which is a question about whether the names fit — so
/// something has to compare them against the room. What was deleted was the
/// strip *laying itself out* from that comparison: slicing labels by column,
/// placing the `+`, deciding which arrows to draw. This decides one boolean,
/// and it is deliberately a function of the names alone — the caller compares
/// it against the window's **outer** width, which is what the strip row gives
/// the viewport after the cluster and does not depend on the names. So the
/// predicate cannot feed itself: capping never changes the answer, which is
/// what keeps a frame from capping, fitting, un-capping and overflowing again.
///
/// Counts what [`strip`] builds: each label plus its `×`, a cell between
/// tabs, and a cell plus the `+` after the last one.
pub fn natural_width(tabs: &[Tab], preview_label: &str) -> usize {
    use crate::primitives::display_width::str_width;
    if tabs.is_empty() {
        return str_width(PLUS);
    }
    let names: usize = tabs
        .iter()
        .map(|t| str_width(&label(t, usize::MAX, preview_label)) + str_width(CLOSE))
        .sum();
    names + (tabs.len() - 1) + 1 + str_width(PLUS)
}

/// The strip: the tabs in a window that scrolls across, then the cluster.
///
/// **Nothing here measures anything.** It used to be a `layout_reader` that
/// took the strip's width, decided a name cap from it, built the whole logical
/// strip as `(Piece, String)` pairs, summed their widths, worked out whether
/// the `<` and `>` arrows appeared, sliced every piece by column against a
/// scroll offset the editor held, and emitted fixed `Sizing::Cells` nodes —
/// a finished picture, handed to a tree that had nothing left to lay out. The
/// offset could not come from layout because layout never decided the widths,
/// so a second full measurement ran in `view::ui::tabs` to produce it
/// (`calculate_tab_widths` and friends), and the two were held in step by
/// comments reading "or widths drift".
///
/// The tabs are ordinary nodes at their natural widths inside a horizontal
/// viewport. Layout settles the widths, the window is the viewport's, the
/// `<` and `>` are the window's own overflow caps, and the editor's one
/// remaining say is *which tab to show* — `Anchor::reveal_key` on the active
/// tab, which is a fact about the pane and not about columns.
pub fn strip(pane: LeafId, s: &Strip, c: Cluster) -> Node<UiMsg> {
    let mut cells: Vec<Node<UiMsg>> = Vec::new();
    for (i, t) in s.tabs.iter().enumerate() {
        if i > 0 {
            cells.push(text(" ".to_string()).theme(ground()));
        }
        let (name_ink, close_ink) = tab_ink(s, i);
        // The name and its `×` in one keyed span, so `reveal_key` brings the
        // whole tab into the window rather than stopping with its label flush
        // against the edge. The two keep their own keys inside it.
        cells.push(row().key(tab_span_key(pane, t.target)).children([
            name_node(
                pane,
                t.target,
                label(t, s.name_cap(), &s.preview_label),
                name_ink,
            ),
            close_node(pane, t.target, CLOSE.to_string(), close_ink),
        ]));
    }

    // **The `+` is the last thing on the strip, after the last tab.** It rides
    // in the window with them, so it sits beside the tab it follows rather
    // than against an edge it has nothing to do with. The painter pinned it to
    // the right whenever the tabs overflowed — a placement that depended on
    // the overflow, which is the window's own answer and not something a
    // description can know.
    if !s.tabs.is_empty() {
        cells.push(text(" ".to_string()).theme(ground()));
    }
    cells.push(
        plus_node(pane, s.hover_plus, PLUS.to_string())
            .w(Sizing::Cells(NEW_TAB_BUTTON_WIDTH as u16)),
    );

    let mut window = fresh_ui::viewport(row().children(cells))
        .key(tab_window_key(pane))
        .scroll_axis(fresh_ui::Axis::Horizontal)
        .scrollbar()
        // A cap on a tab strip is a nudge, not a page: a screenful skips past
        // every tab you were looking for. The painter's `<`/`>` stepped by
        // this many columns and so does this.
        .scroll_step(TAB_SCROLL_STEP_COLUMNS)
        // **A cap is the `+` pointing sideways.** Same ground, same two
        // foregrounds, same padded width — `<`, `>` and `+` are one kind of
        // button on this strip, so nothing about meeting one should tell you
        // which it was. That the release drew a one-cell arrow on the
        // separator's ground is not a reason to keep two answers.
        .scroll_cap_width(NEW_TAB_BUTTON_WIDTH as u16)
        .scrollbar_theme(pair("ui.tab_inactive_fg", "ui.tab_inactive_bg"))
        .scrollbar_hover_theme(pair("ui.tab_close_hover_fg", "ui.tab_inactive_bg"))
        .flex(1)
        .h(Sizing::Cells(1));
    if let Some(a) = &s.reveal {
        window = window.anchor_to(a.clone());
    }

    row()
        .theme(ground())
        .h(Sizing::Cells(1))
        .children([window, cluster(pane, c)])
}

/// A tab's rectangles, read back off the tree.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TabRect {
    pub target: TabTarget,
    /// The label as painted — elided against the strip's width.
    pub label: String,
    pub name: ratatui::layout::Rect,
    pub close: ratatui::layout::Rect,
}

/// Where each of `targets` landed on `pane`'s strip. A tab scrolled off the
/// strip has no rectangle and is not listed.
///
/// What `TabLayout::tabs` recorded, for the two readers that want geometry
/// rather than a press: the drag's drop zone and the web's tab bar.
///
/// **Clipped to the window, because the tree does not do it for us.** The tabs
/// are ordinary nodes in a `ScrollMode::Cells` viewport, which shows part of
/// its content rather than building only the part it shows — so a tab scrolled
/// off the end still has a full-width rectangle, one that `rect_of` keeps
/// (it drops only *empty* ones) and `screen_rect` folds back onto the frame
/// when its `x` has gone negative. Left unclipped, the first tab of a
/// scrolled strip reports a phantom box at the strip's left edge and the
/// drag's drop zone resolves a drop there to the wrong tab. The press path
/// never had this problem: a hit test asks the tree, which clips.
pub fn rects(
    ui: &fresh_ui::Ui<UiMsg>,
    size: ratatui::layout::Rect,
    pane: LeafId,
    targets: &[TabTarget],
) -> Vec<TabRect> {
    let window = ui
        .find_by_key(&tab_window_key(pane))
        .map(|e| ui.rect_of(e))
        .unwrap_or(fresh_ui::Rect::ZERO);
    // Non-empty after clipping, in screen coordinates, or nothing.
    let showing = |r: fresh_ui::Rect| {
        let r = r.intersect(window);
        (r.w > 0 && r.h > 0).then(|| super::screen_rect(r, size))
    };
    targets
        .iter()
        .filter_map(|&t| {
            let key = tab_key(pane, t);
            let name = showing(ui.find_by_key(&key).map(|e| ui.rect_of(e))?)?;
            let close = ui
                .find_by_key(&close_tab_key(pane, t))
                .map(|e| ui.rect_of(e))
                .and_then(showing)
                .unwrap_or(ratatui::layout::Rect::new(
                    name.x + name.width,
                    name.y,
                    0,
                    1,
                ));
            let label = ui
                .spec()
                .index
                .iter()
                .find(|(k, _)| *k == key)
                .and_then(|(_, r)| {
                    ui.spec().items[r.clone()]
                        .iter()
                        .find_map(|i| match &i.draw {
                            fresh_ui::Draw::Lines(l) => l.first().map(|s| s.trim().to_string()),
                            _ => None,
                        })
                })
                .unwrap_or_default();
            Some(TabRect {
                target: t,
                label,
                name,
                close,
            })
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::event::{BufferId, SplitId};
    use crate::primitives::display_width::str_width;
    use crate::view::shell::frame::{frame_tree, Frame};
    use crate::view::shell::splits::{tabs_key, Splits};
    use crate::view::split::SplitNode;
    use fresh_ui::{Input, Mods, Point, Size, Ui};

    fn pane() -> LeafId {
        LeafId(SplitId(0))
    }

    fn buf(i: usize) -> TabTarget {
        TabTarget::Buffer(BufferId(i))
    }

    fn tabs(n: usize) -> Vec<Tab> {
        (0..n)
            .map(|i| Tab {
                target: buf(i),
                name: format!("file_{i}.rs"),
                modified: i == 1,
                preview: false,
                binary: false,
            })
            .collect()
    }

    /// Every test below lays this out in a bar these tabs overflow, which is
    /// the state the strip is interesting in — so the names are capped, as the
    /// editor would have decided from the frame before.
    fn strip_of(n: usize) -> Strip {
        Strip {
            tabs: tabs(n),
            active: Some(buf(0)),
            active_pane: true,
            hover: None,
            hover_plus: false,
            cap_names: true,
            reveal: None,
            preview_label: "(preview)".into(),
        }
    }

    /// The same strip, addressable: the handle the host asks to show a tab.
    fn strip_with(n: usize, a: &Rc<fresh_ui::behavior::Anchor>) -> Strip {
        Strip {
            reveal: Some(a.clone()),
            ..strip_of(n)
        }
    }

    /// Where the strip's window sits, and which ends it caps.
    fn window_caps(ui: &Ui<UiMsg>) -> Vec<fresh_ui::End> {
        ui.spec()
            .items
            .iter()
            .filter_map(|i| match i.draw {
                fresh_ui::Draw::Overflow { end, .. } => Some(end),
                _ => None,
            })
            .collect()
    }

    fn laid_out(s: Strip, controls: PaneControls, w: u16) -> Ui<UiMsg> {
        let mut ui: Ui<UiMsg> = Ui::new();
        let mut chrome = std::collections::HashMap::new();
        chrome.insert(
            pane(),
            crate::view::shell::splits::PaneChrome {
                tabs: true,
                vscroll: true,
                hscroll: false,
            },
        );
        let mut strips = std::collections::HashMap::new();
        strips.insert(pane(), s);
        ui.frame(
            frame_tree(Frame {
                menu_bar: false,
                status_bar: false,
                splits: Some(Splits {
                    root: SplitNode::Leaf {
                        split_id: pane(),
                        buffer_id: BufferId(0),
                        role: None,
                    },
                    maximized: None,
                    active: Some(pane()),
                    chrome,
                    controls,
                    groups: Default::default(),
                    interiors: Default::default(),
                    strips,
                    hover: None,
                    drop_zone: None,
                    hosts: Default::default(),
                }),
                ..Frame::default()
            }),
            Size::new(w, 10),
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
            .filter(|f| *f != UiFact::ClearTabMenus)
            .collect()
    }

    fn rect(ui: &Ui<UiMsg>, k: Key) -> fresh_ui::Rect {
        ui.rect_of(
            ui.find_by_key(&k)
                .unwrap_or_else(|| panic!("{k:?} in the tree")),
        )
    }

    fn size(w: u16) -> ratatui::layout::Rect {
        ratatui::layout::Rect::new(0, 0, w, 10)
    }

    /// **The tabs sit where the painter put them**: on the strip's row, the
    /// first at its left edge, each name followed by its `×` and a one-cell
    /// separator, the `+` after the last.
    #[test]
    fn the_tabs_are_laid_out_as_the_painter_laid_them() {
        let ui = laid_out(strip_of(2), PaneControls::default(), 80);
        let strip = rect(&ui, tabs_key(pane()));
        let n0 = rect(&ui, tab_key(pane(), buf(0)));
        let c0 = rect(&ui, close_tab_key(pane(), buf(0)));
        let n1 = rect(&ui, tab_key(pane(), buf(1)));
        let plus = rect(&ui, new_tab_key(pane()));
        assert_eq!((n0.x, n0.y), (strip.x, strip.y));
        assert_eq!(n0.w as usize, str_width(" file_0.rs "));
        assert_eq!(c0.x, n0.x + n0.w as i32);
        assert_eq!(c0.w, 2);
        assert_eq!(
            n1.x,
            c0.x + c0.w as i32 + 1,
            "one separator cell between tabs"
        );
        assert_eq!(
            n1.w as usize,
            str_width(" file_1.rs* "),
            "the modified marker rides in the name"
        );
        let c1 = rect(&ui, close_tab_key(pane(), buf(1)));
        assert_eq!(
            plus.x,
            c1.x + c1.w as i32 + 1,
            "the + follows the last tab, a separator cell after its ×"
        );
        assert_eq!(plus.w, 3);
        assert!(window_caps(&ui).is_empty(), "two tabs fit, so nothing caps");
    }

    /// A press on a name names the tab and takes the pointer; a press on its
    /// `×` names the tab to close; a right press on either names the tab to
    /// open a menu for.
    #[test]
    fn the_tabs_answer_their_own_presses() {
        let mut ui = laid_out(strip_of(2), PaneControls::default(), 80);
        let n1 = rect(&ui, tab_key(pane(), buf(1)));
        let at = Point::new(n1.x + 2, n1.y);
        let got = facts(ui.dispatch(Input::press(at, MouseButton::Left, Mods::NONE)));
        assert_eq!(
            got,
            vec![UiFact::PaneTabPress {
                pane: pane(),
                target: buf(1),
                x: at.x as u16,
                y: at.y as u16
            }]
        );
        // The pointer is captured: a move far from the strip still comes
        // back as the drag, and the release ends it. (The first move also
        // reports the pointer entering the tab it pressed.)
        let got = facts(ui.dispatch(Input::Move {
            pos: Point::new(40, 8),
            mods: Mods::NONE,
        }));
        assert!(
            got.contains(&UiFact::PaneTabDrag { x: 40, y: 8 }),
            "{got:?}"
        );
        let got = facts(ui.dispatch(Input::release(
            Point::new(40, 8),
            MouseButton::Left,
            Mods::NONE,
        )));
        assert!(got.contains(&UiFact::PaneTabDrop), "{got:?}");
        let c1 = rect(&ui, close_tab_key(pane(), buf(1)));
        let got = facts(ui.dispatch(Input::press(
            Point::new(c1.x, c1.y),
            MouseButton::Left,
            Mods::NONE,
        )));
        assert_eq!(
            got,
            vec![UiFact::PaneTabClose {
                pane: pane(),
                target: buf(1)
            }]
        );
        let got = facts(ui.dispatch(Input::press(at, MouseButton::Right, Mods::NONE)));
        assert_eq!(
            got,
            vec![UiFact::PaneTabMenu {
                pane: pane(),
                target: buf(1),
                x: at.x as u16,
                y: at.y as u16
            }]
        );
        let plus = rect(&ui, new_tab_key(pane()));
        let got = facts(ui.dispatch(Input::press(
            Point::new(plus.x + 1, plus.y),
            MouseButton::Left,
            Mods::NONE,
        )));
        assert_eq!(
            got,
            vec![UiFact::PaneNewTab {
                pane: pane(),
                x: (plus.x + 1) as u16,
                y: plus.y as u16
            }]
        );
    }

    /// **An overflowing strip is a window, and the window says so.**
    ///
    /// This used to assert the painter's picture: a `<` node leading the row,
    /// the tab under the edge cut to the cells that fit, a `+` the strip
    /// pinned itself, an `overflow_key` marker the applier looked for, and
    /// arrows that stepped an offset the editor held. All of it was the strip
    /// measuring itself. What is left to check is that the window overflows,
    /// caps the end that has more, and moves when asked to show a tab.
    #[test]
    fn an_overflowing_strip_caps_its_ends_and_reveals_on_request() {
        let a = fresh_ui::behavior::Anchor::new();
        let ui = laid_out(strip_with(8, &a), PaneControls::default(), 40);
        assert_eq!(
            window_caps(&ui),
            vec![fresh_ui::End::After],
            "eight tabs overflow forty cells, and only the far end has more"
        );
        // The `+` rides in the window after the last tab, so on an
        // overflowing strip it is off to the right with them.
        assert!(
            ui.find_by_key(&new_tab_key(pane())).is_some(),
            "the + is in the tree, at the end of the content"
        );
        let r = rects(&ui, size(40), pane(), &(0..8).map(buf).collect::<Vec<_>>());
        assert!(
            r.len() < 8,
            "only the tabs on screen have rectangles: {}",
            r.len()
        );
        assert_eq!(r[0].label, "file_0.rs");

        // Asking for the last tab moves the window to it — no width passed in,
        // and no offset held anywhere in the editor.
        a.reveal_key(tab_key(pane(), buf(7)));
        let ui = laid_out(strip_with(8, &a), PaneControls::default(), 40);
        let r = rects(&ui, size(40), pane(), &(0..8).map(buf).collect::<Vec<_>>());
        assert!(
            r.iter().any(|t| t.target == buf(7)),
            "the last tab is on screen: {r:?}"
        );
        // **And no further than asked.** `tab_key` names the label alone, and
        // the `×`, the separator and the `+` come after it — so the shortest
        // move that shows the label leaves those behind the far edge, and the
        // window says so with both caps. This used to assert one cap, and
        // passed only because `keyed_band` measured the band a cap's width
        // further along than it was and overshot the end.
        assert_eq!(
            window_caps(&ui),
            vec![fresh_ui::End::Before, fresh_ui::End::After],
            "the label is in, and what follows it is not"
        );

        // Reaching the end is asking for the end. `Window::reveal_active_tab`
        // names the `+` when the active tab is the last one, for exactly this
        // reason: the button follows the tab, so revealing it brings the tab.
        a.reveal_key(new_tab_key(pane()));
        let ui = laid_out(strip_with(8, &a), PaneControls::default(), 40);
        assert_eq!(
            window_caps(&ui),
            vec![fresh_ui::End::Before],
            "and now it is the near end that has more behind it"
        );
        let r = rects(&ui, size(40), pane(), &(0..8).map(buf).collect::<Vec<_>>());
        assert!(
            r.iter().any(|t| t.target == buf(7)),
            "with the last tab still on screen: {r:?}"
        );
    }

    /// With a control cluster the strip yields its right columns to it and
    /// the two buttons answer for themselves. The `>` the cluster used to
    /// carry is the window's own cap now, in the cell immediately before it.
    #[test]
    fn the_cluster_takes_the_right_columns_and_the_overflow_arrow() {
        let controls = PaneControls {
            maximize: true,
            close: true,
        };
        let mut ui = laid_out(strip_of(8), controls, 40);
        let strip = rect(&ui, tabs_key(pane()));
        let close = rect(&ui, close_key(pane()));
        let max = rect(&ui, maximize_key(pane()));
        assert_eq!(
            close.x,
            strip.x + strip.w as i32 - 2,
            "× before the trailing blank"
        );
        assert_eq!(max.x, close.x - 1);
        // **The `+` is at the end of the content, which on an overflowing
        // strip is off to the right.** It used to be pinned inside the window
        // ahead of the cluster, and this asserted that placement — a placement
        // that depended on the overflow, which is the window's answer and not
        // something a description can know. What belongs to the cluster is the
        // column before it: the window's trailing cap.
        assert!(
            ui.find_by_key(&new_tab_key(pane())).is_some(),
            "the + is in the tree, at the end of the content"
        );
        let cap = ui
            .spec()
            .items
            .iter()
            .find(|i| matches!(i.draw, fresh_ui::Draw::Overflow { .. }))
            .map(|i| i.rect)
            .expect("eight tabs overflow forty cells, so the far end is capped");
        let window = rect(&ui, tab_window_key(pane()));
        assert_eq!(
            cap.right(),
            window.right(),
            "the `>` is flush with the window's trailing edge"
        );
        assert!(
            window.right() <= max.x,
            "and the window ends before the cluster's buttons: {window:?} vs {max:?}"
        );
        let got = facts(ui.dispatch(Input::press(
            Point::new(close.x, close.y),
            MouseButton::Left,
            Mods::NONE,
        )));
        assert_eq!(got, vec![UiFact::PaneClose(pane())]);
        let got = facts(ui.dispatch(Input::press(
            Point::new(max.x, max.y),
            MouseButton::Left,
            Mods::NONE,
        )));
        assert_eq!(got, vec![UiFact::PaneMaximize(pane())]);
    }

    /// **A name is capped at twenty-five columns, always.**
    ///
    /// The cap used to depend on the strip's width — full names when they all
    /// fit, twenty-five when they did not — which meant measuring every label
    /// before the description existed, and was half of why the strip was a
    /// `layout_reader`. A window can show what does not fit, so the cap is a
    /// rule about tab names rather than about the room they have: it stops one
    /// 151-character name from being a scroll of its own (issue #2650).
    #[test]
    /// **A name is capped when the names do not fit, and whole when they do.**
    ///
    /// This asserted the cap applied at every width, which is what the strip
    /// did for a while: `label` truncated the string as the description was
    /// built, so there was nothing to decide it against. That elided a
    /// 26-column name on a 160-column screen showing one tab, and the rule it
    /// replaced — cap only on overflow — is what four tests elsewhere were
    /// written against. The decision is the caller's now (`Strip::cap_names`,
    /// from `natural_width` against the window the last frame gave it), and
    /// what this checks is that the strip honours it both ways.
    #[test]
    fn a_name_is_capped_only_when_the_names_do_not_fit() {
        let long = |n: usize| Tab {
            target: buf(n),
            name: format!("{}_{n}.rs", "a".repeat(40)),
            modified: false,
            preview: false,
            binary: false,
        };
        let mut s = strip_of(0);
        s.tabs = vec![long(0)];

        s.cap_names = false;
        let ui = laid_out(s.clone(), PaneControls::default(), 120);
        let whole = rect(&ui, tab_key(pane(), buf(0)));
        assert_eq!(
            whole.w as usize,
            str_width(&label(&long(0), usize::MAX, "(preview)")),
            "room for the name, so the name"
        );

        s.cap_names = true;
        let ui = laid_out(s.clone(), PaneControls::default(), 120);
        let capped = rect(&ui, tab_key(pane(), buf(0)));
        assert_eq!(
            capped.w as usize,
            TAB_NAME_MAX_COLS + 2,
            "and capped when not"
        );
    }

    /// And the caller's arithmetic: what the strip would measure with every
    /// name whole, which is the only thing `cap_names` is decided from.
    #[test]
    fn natural_width_counts_what_the_strip_builds() {
        let s = strip_of(3);
        let ui = laid_out(
            Strip {
                cap_names: false,
                ..s.clone()
            },
            PaneControls::default(),
            400,
        );
        let content: usize = (0..3)
            .map(|i| rect(&ui, tab_span_key(pane(), buf(i))).w as usize)
            .sum::<usize>()
            + 2 // the cell between each pair of tabs
            + 1 // and the one before the `+`
            + str_width(PLUS);
        assert_eq!(natural_width(&s.tabs, &s.preview_label), content);
    }

    #[test]
    fn every_theme_name_is_a_real_key() {
        use crate::view::theme::Theme;
        let theme = Theme::from_json(r#"{"name":"test"}"#).expect("defaults");
        let mut s = strip_of(3);
        s.tabs[2].preview = true;
        let mut names = vec![ground(), pair("ui.tab_inactive_fg", "ui.tab_inactive_bg")];
        for hover in [None, Some((buf(1), false)), Some((buf(1), true))] {
            s.hover = hover;
            for active_pane in [true, false] {
                s.active_pane = active_pane;
                for i in 0..3 {
                    let (n, c) = tab_ink(&s, i);
                    names.push(n);
                    names.push(c);
                }
            }
        }
        for name in names {
            let (fg, bg) = crate::app::shell_host::shell_theme::names(&name);
            for half in [fg, bg] {
                let half = half.unwrap_or_else(|| panic!("{name:?} has an unnamed half"));
                assert!(
                    theme.resolve_theme_key(&half).is_some(),
                    "{half:?} (in {name:?}) is not a theme key"
                );
            }
        }
    }
}
