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

use fresh_ui::{
    gesture, layout_reader, row, text, Event, GestureKind, Key, LayoutInfo, MouseButton, Node,
    Sizing,
};

use crate::app::shell_host::shell_theme::{attrs, pair};
use crate::app::types::HoverTarget;
use crate::model::event::LeafId;
use crate::primitives::display_width::str_width;
use crate::view::split::TabTarget;
use crate::view::ui::tabs::{
    elided_tab_name, tabs_render_width, NEW_TAB_BUTTON_WIDTH, TAB_NAME_MAX_COLS,
};
use unicode_segmentation::UnicodeSegmentation;

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
#[derive(Clone, Debug, Default, PartialEq, Eq)]
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
    /// `SplitViewState::tab_scroll_offset`: how far the tabs are scrolled, in
    /// columns of the logical strip. The editor owns it, as it owns every
    /// pane's scroll (§8): `ensure_active_tab_visible` moves it when the
    /// active tab changes and the arrows and the wheel step it.
    pub offset: usize,
    /// The word a preview tab carries after its name, localized.
    pub preview_label: String,
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

pub fn close_tab_key(pane: LeafId, t: TabTarget) -> Key {
    Key::Pair(format!("tab_close:{}", pane.0 .0).into(), ordinal(t))
}

pub fn new_tab_key(pane: LeafId) -> Key {
    Key::Pair("tab_new".into(), pane.0 .0 as u64)
}

pub fn scroll_left_key(pane: LeafId) -> Key {
    Key::Pair("tab_scroll_left".into(), pane.0 .0 as u64)
}

pub fn scroll_right_key(pane: LeafId) -> Key {
    Key::Pair("tab_scroll_right".into(), pane.0 .0 as u64)
}

/// Present in the tree exactly when the tabs overflow the strip's right edge
/// — what `TabLayout::right_overflow` recorded, as a node the applier can
/// look for.
pub fn overflow_key(pane: LeafId) -> Key {
    Key::Pair("tab_overflow".into(), pane.0 .0 as u64)
}

/// The text a tab shows: the painter's `" {name}{*}{ preview}{ [BIN]} "`.
///
/// **Shared with the model's width arithmetic.** `calculate_tab_widths`, which
/// `ensure_active_tab_visible` reads to bring the active tab into view,
/// measures the same string, so the offset the editor picks and the window
/// the strip shows agree by construction.
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

const CLOSE: &str = "× ";
const PLUS: &str = " + ";

/// Full names when every tab fits at full width, otherwise each name is
/// capped — the painter's `tab_name_cap`, against the width the strip has.
fn name_cap(s: &Strip, width: usize) -> usize {
    let full: usize = s
        .tabs
        .iter()
        .map(|t| str_width(&label(t, usize::MAX, &s.preview_label)) + str_width(CLOSE))
        .sum();
    let with_seps = full + s.tabs.len().saturating_sub(1);
    if with_seps <= tabs_render_width(with_seps, width) {
        usize::MAX
    } else {
        TAB_NAME_MAX_COLS
    }
}

/// One piece of the logical strip, before scrolling: what it is, and what it
/// reads.
#[derive(Clone, Debug)]
enum Piece {
    Name(usize),
    Close(usize),
    Sep,
    Plus,
}

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

fn plus_node(pane: LeafId, s: String) -> Node<UiMsg> {
    gesture(text(s).theme(pair("ui.tab_inactive_fg", "ui.tab_inactive_bg")))
        .key(new_tab_key(pane))
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

fn arrow(pane: LeafId, glyph: &'static str, delta: i32, key: Key) -> Node<UiMsg> {
    gesture(text(glyph).theme(ground())).key(key).on(
        GestureKind::Press,
        press(UiFact::PaneTabsScroll { pane, delta }),
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

/// The cluster: `[gap] > □ × [trail]`, over the columns reserved for it. The
/// `>` shows only when the tabs overflow; its column is held either way so
/// the cluster does not shift as they scroll.
fn cluster(pane: LeafId, c: Cluster, overflow: bool) -> Node<UiMsg> {
    let one = Sizing::Cells(1);
    let mut cells: Vec<Node<UiMsg>> = vec![row().w(one)];
    cells.push(match overflow {
        true => arrow(pane, ">", 1, scroll_right_key(pane)).w(one),
        false => row().w(one),
    });
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

/// The strip, laid out against the width the tree gives it.
///
/// The row's shape is the painter's: `<` when scrolled, the visible window
/// over the tabs (a tab cut by the window's edge shows the cells that fit,
/// as `build_visible_line` cut its span), the `+` inline after the last tab
/// or pinned to the right edge when they overflow, `>` when they overflow and
/// no cluster carries it, then the cluster.
pub fn strip(pane: LeafId, s: &Strip, c: Cluster) -> Node<UiMsg> {
    let s = Rc::new(s.clone());
    layout_reader(move |info: LayoutInfo| lay_out(pane, &s, c, info.constraints.max_w as usize))
        .h(Sizing::Cells(1))
}

/// `txt` from `skip` columns in, at most `room` columns wide, cut on
/// grapheme boundaries by display width — never by `char`: a wide
/// character taken as one char is two columns, and a piece cut that way
/// was wider than the room the strip had for it, pushing the arrow and the
/// pinned `+` out of the row.
fn columns(txt: &str, skip: usize, room: usize) -> String {
    let mut out = String::new();
    let (mut seen, mut taken) = (0usize, 0usize);
    for g in txt.graphemes(true) {
        let w = str_width(g);
        if seen < skip {
            seen += w;
            continue;
        }
        if taken + w > room {
            break;
        }
        out.push_str(g);
        taken += w;
    }
    out
}

fn lay_out(pane: LeafId, s: &Strip, c: Cluster, total_w: usize) -> Node<UiMsg> {
    let external = c.controls.reserve() > 0;
    let width = total_w.saturating_sub(c.controls.reserve() as usize);
    let cap = name_cap(s, width);

    // The logical strip, before scrolling.
    let mut pieces: Vec<(Piece, String)> = Vec::new();
    for (i, t) in s.tabs.iter().enumerate() {
        if i > 0 {
            pieces.push((Piece::Sep, " ".into()));
        }
        pieces.push((Piece::Name(i), label(t, cap, &s.preview_label)));
        pieces.push((Piece::Close(i), CLOSE.into()));
    }
    let tabs_total: usize = pieces.iter().map(|(_, t)| str_width(t)).sum();
    let max_width = tabs_render_width(tabs_total, width);
    let pin_plus = max_width < width;
    if !pin_plus {
        if !s.tabs.is_empty() {
            pieces.push((Piece::Sep, " ".into()));
        }
        pieces.push((Piece::Plus, PLUS.into()));
    }

    // The window over it.
    let total: usize = pieces.iter().map(|(_, t)| str_width(t)).sum();
    let offset = s.offset.min(total);
    let show_left = offset > 0;
    let overflow = total.saturating_sub(offset) > max_width;
    let draw_right = overflow && !external;
    let available = max_width
        .saturating_sub(show_left as usize)
        .saturating_sub(draw_right as usize);

    let mut cells: Vec<Node<UiMsg>> = Vec::new();
    if show_left {
        cells.push(arrow(pane, "<", -1, scroll_left_key(pane)));
    }
    let mut skip = offset;
    let mut rendered = 0usize;
    for (piece, txt) in pieces {
        let w = str_width(&txt);
        if skip >= w {
            skip -= w;
            continue;
        }
        let room = available.saturating_sub(rendered);
        if room == 0 {
            break;
        }
        let shown = columns(&txt, skip, room);
        let shown_w = str_width(&shown);
        skip = 0;
        let node = match piece {
            Piece::Name(i) => {
                let (ink, _) = tab_ink(s, i);
                name_node(pane, s.tabs[i].target, shown, ink)
            }
            Piece::Close(i) => {
                let (_, ink) = tab_ink(s, i);
                close_node(pane, s.tabs[i].target, shown, ink)
            }
            Piece::Sep => text(shown).theme(ground()),
            Piece::Plus => plus_node(pane, shown),
        };
        cells.push(node.w(Sizing::Cells(shown_w as u16)));
        rendered += shown_w;
        if rendered >= available {
            break;
        }
    }
    if draw_right {
        cells.push(arrow(pane, ">", 1, scroll_right_key(pane)));
    }
    // The rest of the row is the strip's ground; the pinned `+` sits on its
    // last three columns.
    cells.push(row().flex(1));
    if pin_plus {
        cells.push(plus_node(pane, PLUS.into()).w(Sizing::Cells(NEW_TAB_BUTTON_WIDTH as u16)));
    }
    if overflow {
        cells.push(row().w(Sizing::Cells(0)).key(overflow_key(pane)));
    }
    let tabs = row()
        .w(Sizing::Cells(width as u16))
        .theme(ground())
        .children(cells);
    match external {
        true => row()
            .theme(ground())
            .children([tabs, cluster(pane, c, overflow)]),
        false => tabs,
    }
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
pub fn rects(
    ui: &fresh_ui::Ui<UiMsg>,
    size: ratatui::layout::Rect,
    pane: LeafId,
    targets: &[TabTarget],
) -> Vec<TabRect> {
    targets
        .iter()
        .filter_map(|&t| {
            let key = tab_key(pane, t);
            let name = super::rect_of(ui, &key, size)?;
            let close = super::rect_of(ui, &close_tab_key(pane, t), size).unwrap_or(
                ratatui::layout::Rect::new(name.x + name.width, name.y, 0, 1),
            );
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
    use crate::view::shell::frame::{frame_tree, Frame};
    use crate::view::shell::splits::{tabs_key, Splits};
    use crate::view::split::SplitNode;
    use fresh_ui::{Input, Mods, Point, Size, Ui};

    fn pane() -> LeafId {
        LeafId(SplitId(0))
    }

    /// A wide-character name is cut by columns, so the piece is never wider
    /// than its room and a window into it starts where the offset says.
    #[test]
    fn a_piece_is_cut_by_columns_not_chars() {
        assert_eq!(columns("日本語ファイル.txt", 0, 3), "日");
        assert_eq!(str_width(&columns("日本語ファイル.txt", 0, 5)), 4);
        assert_eq!(columns("日本語ファイル.txt", 2, 4), "本語");
        assert_eq!(columns("abc.txt", 1, 3), "bc.");
        assert_eq!(columns("abc", 5, 3), "");
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

    fn strip_of(n: usize, offset: usize) -> Strip {
        Strip {
            tabs: tabs(n),
            active: Some(buf(0)),
            active_pane: true,
            hover: None,
            offset,
            preview_label: "(preview)".into(),
        }
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
        let ui = laid_out(strip_of(2, 0), PaneControls::default(), 80);
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
        assert_eq!(plus.x, c1.x + c1.w as i32 + 1);
        assert_eq!(plus.w, 3);
        assert!(
            ui.find_by_key(&overflow_key(pane())).is_none(),
            "two tabs fit"
        );
    }

    /// A press on a name names the tab and takes the pointer; a press on its
    /// `×` names the tab to close; a right press on either names the tab to
    /// open a menu for.
    #[test]
    fn the_tabs_answer_their_own_presses() {
        let mut ui = laid_out(strip_of(2, 0), PaneControls::default(), 80);
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

    /// **The window over an overflowing strip is the painter's.** Scrolled
    /// past the first tab, a `<` leads the row, the tab under the edge shows
    /// only the cells that fit, the `+` is pinned to the right edge and the
    /// overflow marker is in the tree; the arrows step the offset.
    #[test]
    fn an_overflowing_strip_scrolls_with_arrows_and_pins_the_plus() {
        let ui = laid_out(strip_of(8, 0), PaneControls::default(), 40);
        assert!(
            ui.find_by_key(&overflow_key(pane())).is_some(),
            "eight tabs overflow forty cells"
        );
        assert!(
            ui.find_by_key(&scroll_left_key(pane())).is_none(),
            "nothing to the left yet"
        );
        let plus = rect(&ui, new_tab_key(pane()));
        assert_eq!(plus.x + plus.w as i32, 40, "pinned to the right edge");
        let right = rect(&ui, scroll_right_key(pane()));
        assert!(right.x < plus.x, "the > sits before the pinned +");
        let r = rects(&ui, size(40), pane(), &(0..8).map(buf).collect::<Vec<_>>());
        assert!(
            r.len() < 8,
            "only the tabs on screen have rectangles: {}",
            r.len()
        );
        assert_eq!(r[0].label, "file_0.rs");

        let mut ui = laid_out(strip_of(8, 5), PaneControls::default(), 40);
        let left = rect(&ui, scroll_left_key(pane()));
        assert_eq!(left.x, 0, "a < leads the scrolled row");
        let n0 = rect(&ui, tab_key(pane(), buf(0)));
        assert_eq!(n0.x, 1);
        assert_eq!(
            n0.w as usize,
            str_width(" file_0.rs ") - 5,
            "the first tab shows the cells past the offset"
        );
        let got = facts(ui.dispatch(Input::press(
            Point::new(0, left.y),
            MouseButton::Left,
            Mods::NONE,
        )));
        assert_eq!(
            got,
            vec![UiFact::PaneTabsScroll {
                pane: pane(),
                delta: -1
            }]
        );
    }

    /// With a control cluster the strip yields its right columns to it, the
    /// `>` moves into the cluster's slot, and the two buttons answer for
    /// themselves.
    #[test]
    fn the_cluster_takes_the_right_columns_and_the_overflow_arrow() {
        let controls = PaneControls {
            maximize: true,
            close: true,
        };
        let mut ui = laid_out(strip_of(8, 0), controls, 40);
        let strip = rect(&ui, tabs_key(pane()));
        let close = rect(&ui, close_key(pane()));
        let max = rect(&ui, maximize_key(pane()));
        assert_eq!(
            close.x,
            strip.x + strip.w as i32 - 2,
            "× before the trailing blank"
        );
        assert_eq!(max.x, close.x - 1);
        let right = rect(&ui, scroll_right_key(pane()));
        assert_eq!(right.x, max.x - 1, "> in the cluster's reserved slot");
        let plus = rect(&ui, new_tab_key(pane()));
        assert!(
            plus.x + (plus.w as i32) < right.x,
            "the + pins inside the tabs' width"
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

    /// Names are capped at twenty-five columns only when the full labels do
    /// not fit — the painter's `tab_name_cap`.
    #[test]
    fn names_are_elided_only_when_the_full_labels_overflow() {
        let long = |n: usize| Tab {
            target: buf(n),
            name: format!("{}_{n}.rs", "a".repeat(40)),
            modified: false,
            preview: false,
            binary: false,
        };
        let mut s = strip_of(0, 0);
        s.tabs = vec![long(0)];
        let ui = laid_out(s.clone(), PaneControls::default(), 120);
        let n0 = rect(&ui, tab_key(pane(), buf(0)));
        assert_eq!(n0.w as usize, 40 + 5 + 2, "one long name fits in full");
        s.tabs = vec![long(0), long(1), long(2)];
        let ui = laid_out(s, PaneControls::default(), 120);
        let n0 = rect(&ui, tab_key(pane(), buf(0)));
        assert_eq!(
            n0.w as usize,
            TAB_NAME_MAX_COLS + 2,
            "three do not, and each is capped"
        );
    }

    #[test]
    fn every_theme_name_is_a_real_key() {
        use crate::view::theme::Theme;
        let theme = Theme::from_json(r#"{"name":"test"}"#).expect("defaults");
        let mut s = strip_of(3, 0);
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
