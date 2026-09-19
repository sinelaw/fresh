//! The file explorer sidebar as a description.
//!
//! The biggest surface migrated so far, and the first with real *content*
//! rather than a row of controls: a bordered panel, a title that doubles as a
//! search box, one row per visible tree node, and a status slot on the right
//! of each row whose position nobody was able to state without computing it
//! twice.
//!
//! # What the tree measures
//!
//! Each row is `row([ left_runs, gap.flex(1), trailing?, error? ])`. The gap is
//! a flex spacer, so the trailing slot is pushed to the right edge by *layout*.
//! That deletes `FileExplorerRenderer::trailing_slot_screen_bounds` — 45 lines
//! that re-derived the slot's column from the indicator width, the leading
//! slot's width, the compact chain's width, the name's width and the padding
//! rule, purely so a hover could find it. The slot is a keyed node now, and its
//! rectangle is read back with [`slot_rect`].
//!
//! # What the window is, and whose
//!
//! The rows sit in a `fresh_ui::viewport`, declared to the library the way
//! every other scrolling surface's is: the tree's row count, a **controlled**
//! offset ([`Node::scroll`](fresh_ui::Node::scroll) plus
//! [`on_scroll`](fresh_ui::Node::on_scroll)), and the sticky ancestors as
//! **pinned** rows ([`Node::pinned`](fresh_ui::Node::pinned)). The bar, its
//! gutter, its thumb, its hit-testing and the wheel are the library's for it
//! — this module used to hand-build the bar as a parallel column of coloured
//! cells beside the rows, two structures kept in step by hand.
//!
//! Three things stay the model's, and the three are why this panel was the
//! last surface to declare its window:
//!
//! - **The offset.** `FileTreeView` owns it; keys, search, reveal and the
//!   `follow_active_buffer` setting all write it, and it survives rebuilds.
//!   So the window is *controlled*: the description states the model's
//!   offset every frame, and a wheel or a bar drag is reported back as
//!   [`UiFact::ExplorerScrollTo`], which the model clamps and stores. The
//!   window is where the model says, one frame after the model is told.
//! - **Which rows are pinned.** `FileTreeView::sticky_display_indices` —
//!   the expanded ancestors of the first scrolled row — is a fact about the
//!   tree, and a function of the offset. The model names them; the window
//!   makes room and derives its ceiling from them. That ceiling is exactly
//!   `FileTreeView::max_scroll_offset` for the offset the window is at: the
//!   smallest offset whose run reaches the last row is past `total - rows`
//!   by the rows the pins took.
//! - **What a row says.** `describe_row` needs the tree, the decoration and
//!   slot caches, the theme and the config, none of which a `'static` row
//!   builder can borrow. So the model describes the rows *it* would window —
//!   the same `viewport_display_indices()` as before, at the section height
//!   the frame resolves before the description exists — and the builder the
//!   viewport calls during layout answers an index out of that set. The
//!   library still decides which indices it asks for; the app decides what
//!   each one looks like, which is the split the row builder is for.
//!
//! # What it does not measure
//!
//! **The chrome.** The border, the title strip and the width grip are the
//! sidebar column's (`super::sidebar`), because the explorer is one section
//! of that column and the border row above its rows is a section header. What
//! is here is the *content*: the rows, the caret, and the union box that
//! answers a press no row took.
//!
//! # Colour
//!
//! Every colour here is a real theme key except two: `ExplorerSlot`'s `fg` and
//! the name-colour hint, which arrive already resolved to a `Color` because
//! `resolve_overlay_color` collapses a plugin's `OverlayColorSpec` long before
//! a description exists. Those are written as `#rrggbb` literals — see
//! [`crate::app::shell_host::shell_theme`], which documents the literal as an
//! interim and names what replaces it.

use std::rc::Rc;

use fresh_ui::{
    col, gesture, layout_reader, row, stack, text, text_runs, viewport, Event, GestureKind, Key,
    Node, Run, Sizing,
};

use crate::app::shell_host::shell_theme::{attrs, pair};
use crate::app::types::HoverTarget;

use super::msg::{UiFact, UiMsg};
use super::rect_of;

/// A `(text, theme name)` pair — the same shape the menu bar's labels use.
pub type Runs = Vec<(String, String)>;

/// One visible row of the tree.
///
/// `index` is the row's index in the tree's flattened display order — what
/// `FileTreeView::get_display_nodes` is indexed by and what the window counts
/// in — so a row's key, its hit answer, the window's offset and the model's
/// lookup are all the same number. A pinned ancestor keeps its own index
/// wherever the window draws it.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Row {
    pub index: usize,
    /// The row's own ground: selection, multi-selection or the panel's.
    pub theme: String,
    /// Indicator, leading slot, compact chain and name, in order.
    pub left: Runs,
    /// The status slot pushed to the right edge, if the providers gave one.
    pub trailing: Option<Slot>,
    /// `" [Error]"` for a node that failed to load.
    pub error: Option<(String, String)>,
}

/// A row's trailing status slot: what it says, how it looks, and which path's
/// tooltip it opens.
///
/// The path travels with the slot because the *slot* is what the pointer
/// enters — the old walk had to find the row, then re-derive the slot's
/// columns, then look the node up again to get the path.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Slot {
    pub text: String,
    pub theme: String,
    pub path: std::path::PathBuf,
}

/// What fills the panel.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Body {
    /// The tree is still being built (initial async build, or expand-to-path).
    /// The panel's chrome is already final — that is the point of this state,
    /// so a slow remote build never paints the window in two stages.
    Loading(String),
    Rows(Vec<Row>),
}

impl Default for Body {
    fn default() -> Body {
        // Not an empty row list: a panel with no tree yet is *loading*, and the
        // two look different on purpose.
        Body::Loading(String::new())
    }
}

/// Where the tree's window sits: what the description declares to the
/// viewport, in tree rows. All three are the model's (see the module docs:
/// *what the window is, and whose*).
///
/// Whether there is a bar is no longer stated here — the viewport draws one
/// when `total` overflows the rows it has, which is the same rule stated
/// once, by the thing that knows how many rows it has.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Scroll {
    /// The first row of the scrolled run — the model's offset, already
    /// clamped to its own ceiling.
    pub offset: usize,
    /// Rows in the whole tree.
    pub total: usize,
    /// The ancestors pinned above the run, in the order they are drawn.
    ///
    /// **This is why the ceiling is not `total - rows`.** The pinned rows
    /// eat part of the window, so the last offset is larger than the naive
    /// ceiling by however many there are — which the viewport derives from
    /// this list, and which `FileTreeView::max_scroll_offset` computes for
    /// the model. A bar that assumed the naive ceiling parked its thumb at
    /// the bottom of the track while the list was still moving.
    pub pinned: Vec<usize>,
}

/// The explorer's content: what the sidebar's first section holds.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Explorer {
    pub body: Body,
    /// The row the caret sits on, by [`Row::index`], when the panel owns the
    /// keyboard.
    pub caret_row: Option<usize>,
    /// Where the window is. `None` is a window over the rows in `body`
    /// alone, from the top — what a fixture built from a handful of rows
    /// means, and what the viewport makes of a tree that fits: no bar (issue
    /// #2859).
    pub scroll: Option<Scroll>,
}

impl Explorer {
    /// The panel's ground — the background every row and the border sit on.
    pub fn panel() -> String {
        pair("editor.fg", "editor.bg")
    }
}

/// The keys the readers below look elements up by.
pub fn row_key(index: usize) -> Key {
    Key::Pair("explorer_row".into(), index as u64)
}

pub fn slot_key(index: usize) -> Key {
    Key::Pair("explorer_slot".into(), index as u64)
}

fn hover_msg(t: Option<HoverTarget>) -> fresh_ui::Handler<UiMsg> {
    Rc::new(move |_: &Event| Some(UiMsg::Ui(UiFact::Hover(t.clone()))))
}

fn runs_of(runs: &Runs) -> Vec<Run> {
    runs.iter()
        .map(|(t, theme)| Run::themed(t.clone(), theme))
        .collect()
}

/// The rows as a description: one per visible tree node, or the loading
/// placeholder while the tree is still being built.
///
/// **Memoised on the explorer's state.** The tree is rebuilt every frame
/// and changes only when the listing, the cursor or a hover does.
/// `Explorer` is `PartialEq` and is the whole of what this reads.
pub fn rows(e: &Explorer) -> Node<UiMsg> {
    fresh_ui::memo(e.clone(), build_rows)
}

fn build_rows(e: &Explorer) -> Node<UiMsg> {
    let rows = match &e.body {
        Body::Loading(text_) => {
            return col().child(
                text(text_.clone())
                    .theme(pair("editor.line_number_fg", "editor.bg"))
                    .h(Sizing::Cells(1)),
            )
        }
        Body::Rows(rows) => rows,
    };
    // The window as the model declares it, or — for a body that came with
    // no window state — the rows themselves, from the top.
    let (offset, total, pinned): (u32, u32, Vec<u32>) = match &e.scroll {
        Some(s) => (
            narrow(s.offset),
            narrow(s.total),
            s.pinned.iter().map(|&i| narrow(i)).collect(),
        ),
        None => (0, narrow(rows.len()), Vec::new()),
    };
    // **The rows the model windowed, by index.** The builder below runs
    // during layout, for whichever indices the viewport asks for: the pins it
    // was given and the run under them. Those are exactly the indices the
    // model described (module docs: *what the window is, and whose*), so the
    // answer is a lookup; an index outside the set — a frame where the
    // section's height and the model's disagree — draws an empty row rather
    // than nothing at all, so the grid the offset addresses holds.
    let by_index: Rc<std::collections::HashMap<usize, Row>> =
        Rc::new(rows.iter().map(|r| (r.index, r.clone())).collect());
    let caret_row = e.caret_row;
    let pins = pinned.clone();
    let reader = layout_reader(move |info| {
        let win = info.scroll_window.unwrap_or_default();
        let first = win.y.max(0) as u32;
        let indices = pins[..(info.pinned as usize).min(pins.len())]
            .iter()
            .copied()
            .chain(first..first.saturating_add(u32::from(win.h)).min(total));
        col().children(indices.map(|i| match by_index.get(&(i as usize)) {
            Some(r) => node_row(caret_row, r),
            None => row().h(Sizing::Cells(1)),
        }))
    });
    // A gesture around the window rather than a listener on each row: the
    // wheel is the window's, wherever over it the pointer is — the rows, the
    // empty space under the last one, the bar. It does not `stop()`: the
    // library's scroll chain runs only for a wheel nothing claimed, and the
    // chain is what moves the window and reports where it went. What this
    // listener carries is the part the library cannot know about — the
    // plugin `mouse_scroll` hook wants the pointer, and a wheel over the
    // panel dismisses a transient popup — as it did when the rows claimed it.
    gesture(
        viewport(reader)
            .items(total)
            .scroll(offset)
            .pinned(&pinned)
            .on_scroll(|offset| UiMsg::Ui(UiFact::ExplorerScrollTo(offset as usize)))
            // The bar takes a column of its own rather than floating over the
            // rows: a row's trailing status slot is pushed flush to the right
            // edge by layout, so an overlay bar would sit exactly on top of
            // the git markers. One column narrower is what a gutter costs,
            // and the rows are measured at the narrower width by the same
            // layout that answers a press — nothing re-derives a column.
            //
            // **A bar is two background colours, not two glyphs** — the fold
            // paints the thumb in the pair's foreground and the track in its
            // background, both as the cell's ground, because box-drawing
            // glyphs leave gaps between rows in some terminals and every test
            // that finds a scrollbar on screen finds it by that background.
            .scrollbar()
            .scrollbar_theme(pair("ui.scrollbar_thumb_fg", "ui.scrollbar_track_fg")),
    )
    .on(
        GestureKind::Wheel,
        Rc::new(move |e: &Event| {
            Some(UiMsg::Ui(UiFact::ExplorerWheel {
                delta: e.delta,
                x: e.pos.x.max(0) as u16,
                y: e.pos.y.max(0) as u16,
            }))
        }),
    )
}

fn narrow(n: usize) -> u32 {
    u32::try_from(n).unwrap_or(u32::MAX)
}

/// **The union box.** A right-press anywhere on the panel opens the menu,
/// which is what the component did ("the union box spans the whole
/// explorer") and what binding the gesture to rows alone lost: a click on
/// the empty space below the last file answered nothing, so every test that
/// right-clicks a row the fixture does not have saw no menu at all.
///
/// Rows `stop()` their own right-press, so this fires only where no row
/// did. The title row is excluded app-side against the panel's rectangle,
/// exactly as the component excluded it with `ev.row <= explorer_area.y`.
pub fn union_box(n: Node<UiMsg>) -> Node<UiMsg> {
    gesture(n)
        .on(
            GestureKind::Press,
            Rc::new(|ev: &Event| {
                if ev.button != fresh_ui::MouseButton::Right || ev.mods.ctrl {
                    return None;
                }
                ev.stop();
                Some(UiMsg::Ui(UiFact::ExplorerBodyContext {
                    x: ev.pos.x.max(0) as u16,
                    y: ev.pos.y.max(0) as u16,
                }))
            }),
        )
        // And the same for the left button, which the component also bound to
        // the whole panel: `handle_file_explorer_click` took focus for any
        // click inside the rectangle before it looked for a row, so clicking
        // the empty space below the tree focused the explorer. Rows `stop()`
        // their own left press, so this fires only where no row did.
        .on(
            GestureKind::Press,
            Rc::new(|ev: &Event| {
                if ev.button != fresh_ui::MouseButton::Left {
                    return None;
                }
                ev.stop();
                Some(UiMsg::Ui(UiFact::ExplorerBodyPress))
            }),
        )
}

/// The caret glyph's ink: the row's own, with only the foreground moved.
///
/// The caret marks the selected row; it does not cut a hole in the highlight.
/// `pair("editor.cursor", "editor.bg")` did cut one, and on a focused panel
/// that made the selected row's first cell indistinguishable from every
/// unselected row's. A row whose ink is unreadable keeps its name rather than
/// gaining a caret in nobody's colours.
fn caret_ink(row: &str) -> String {
    use crate::app::shell_host::shell_theme::{Ink, Paint};
    match Ink::parse(row) {
        Some(ink) => ink.with_fg(Paint::key("editor.cursor")).to_string(),
        None => row.to_string(),
    }
}

fn node_row(caret_row: Option<usize>, r: &Row) -> Node<UiMsg> {
    let mut children: Vec<Node<UiMsg>> = vec![
        text_runs(runs_of(&r.left)),
        // **The padding rule, as layout.** The old walk computed
        // `content_width - left_side_width - total_right_width` and a second
        // function computed it again to find the slot; a flex spacer states it
        // once and both the cells and the rectangle come out of it — including
        // the `min_gap = 1` floor, which is `min_w` rather than a `max()` in
        // two places.
        row().flex(1).min_w(1),
    ];
    if let Some(slot) = &r.trailing {
        let path = slot.path.clone();
        children.push(
            gesture(text(slot.text.clone()).theme(slot.theme.clone()))
                // Keyed so a caller can ask layout where the slot ended up
                // rather than re-deriving the column.
                .key(slot_key(r.index))
                // The slot answers its own hover, so the tooltip opens on the
                // cells that actually carry the status — no bounds function in
                // between. It does not claim: a press here still selects the
                // row, because the row's handler is up the same path.
                .on_enter(hover_msg(Some(HoverTarget::FileExplorerStatusIndicator(
                    path.clone(),
                ))))
                .on_leave(hover_msg(None)),
        );
    }
    if let Some((t, theme)) = &r.error {
        children.push(text(t.clone()).theme(theme.clone()));
    }
    let index = r.index;
    let caret = caret_row == Some(index);
    let body = row()
        .theme(r.theme.clone())
        .h(Sizing::Cells(1))
        .children(children);
    // The caret indicator the panel paints under the hardware cursor when it
    // owns the keyboard. It replaces the left-most cell of the row, which is
    // what the old `Paragraph::new("▌")` overwrote — and it places the
    // hardware cursor on that cell (`cursor_byte`), so the row the keyboard
    // is on is the display list's caret, not arithmetic over the region's
    // origin and the box's border.
    let body = if caret {
        stack().h(Sizing::Cells(1)).children([
            body,
            row().h(Sizing::Cells(1)).children([text("▌")
                .theme(caret_ink(&r.theme))
                .w(Sizing::Cells(1))
                .cursor_byte(0)]),
        ])
    } else {
        body
    };
    gesture(body)
        .key(row_key(index))
        // Left only, and it stops: the press selects and opens, which is what
        // the chrome component reported `Consumed` for. A right press is the
        // context menu's, and a modifier-less right press must still reach the
        // theme inspector's pre-band, so it is answered separately below.
        .on(
            GestureKind::Press,
            Rc::new(move |e: &Event| {
                if e.button != fresh_ui::MouseButton::Left {
                    return None;
                }
                e.stop();
                Some(UiMsg::Ui(UiFact::ExplorerRowPress {
                    index,
                    clicks: e.clicks,
                }))
            }),
        )
        // The context menu opens on the **press**, which is when
        // `MouseEventKind::Down(Right)` opened it before.
        //
        // Except with Ctrl held. Ctrl+Right-click is the theme inspector's
        // gesture, and the inspector rides the very top of the legacy bands
        // precisely so it can be reached under any surface — but the tree now
        // runs *before* those bands, so "above everything" has to be said here,
        // by declining, instead of by rank. Declining is also not claiming, so
        // the press travels on untouched.
        .on(
            GestureKind::Press,
            Rc::new(move |e: &Event| {
                if e.button != fresh_ui::MouseButton::Right || e.mods.ctrl {
                    return None;
                }
                e.stop();
                Some(UiMsg::Ui(UiFact::ExplorerRowContext {
                    index,
                    x: e.pos.x.max(0) as u16,
                    y: e.pos.y.max(0) as u16,
                }))
            }),
        )
    // The wheel is not the row's: it is the window's, declared in
    // `build_rows`, and the library moves the window for it.
}

// -- the styles, as names ----------------------------------------------------

/// Title and border for the panel chrome.
///
/// Same three cases `FileExplorerRenderer::panel_chrome_styles` had: a
/// disconnected remote shouts, a focused panel inverts its title and accents
/// its border, and a blurred one recedes.
pub fn chrome_themes(remote_disconnected: bool, focused: bool) -> (String, String) {
    if remote_disconnected {
        (
            attrs(
                "ui.status_error_indicator_fg",
                "ui.status_error_indicator_bg",
                &["bold"],
            ),
            pair("ui.status_error_indicator_bg", "editor.bg"),
        )
    } else if focused {
        (
            attrs("editor.bg", "editor.fg", &["bold"]),
            pair("editor.cursor", "editor.bg"),
        )
    } else {
        (
            pair("editor.line_number_fg", "editor.bg"),
            pair("ui.split_separator_fg", "editor.bg"),
        )
    }
}

/// The close button's own colour.
pub fn close_theme(hovered: bool) -> String {
    if hovered {
        pair("ui.tab_close_hover_fg", "editor.bg")
    } else {
        pair("editor.line_number_fg", "editor.bg")
    }
}

/// A row's ground.
///
/// The old painter said this twice — `ListItem::style` for the item and
/// `List::highlight_style` for the cursor row — and the two disagreed for a
/// blurred multi-selection. Stated once here, matching what the pair actually
/// produced on screen.
pub fn row_theme(is_cursor: bool, is_multi: bool, focused: bool) -> String {
    if is_cursor && focused {
        pair("editor.fg", "editor.selection_bg")
    } else if is_cursor {
        pair("editor.fg", "editor.current_line_bg")
    } else if is_multi && focused {
        pair("editor.fg", "editor.selection_bg")
    } else {
        Explorer::panel()
    }
}

/// The foreground a node's name takes when nothing overrides it: hidden files
/// recede, symlinks take the type colour, directories the keyword colour.
pub fn neutral_key(is_hidden: bool, is_symlink: bool, is_dir: bool) -> &'static str {
    if is_hidden {
        "editor.line_number_fg"
    } else if is_symlink {
        "syntax.type"
    } else if is_dir {
        "syntax.keyword"
    } else {
        "editor.fg"
    }
}

// -- reading the layout back -------------------------------------------------

/// Where layout put a row's trailing status slot.
///
/// This is the whole of what `trailing_slot_screen_bounds` computed, and the
/// reason that function could exist at all was that the padding rule lived in
/// two places. It lives in the flex spacer now.
pub fn slot_rect(
    ui: &fresh_ui::Ui<UiMsg>,
    index: usize,
    size: ratatui::layout::Rect,
) -> Option<ratatui::layout::Rect> {
    rect_of(ui, &slot_key(index), size)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::view::shell::fold::{fold_native, Band};
    use crate::view::shell::frame::{frame_tree, Frame};
    use crate::view::shell::sidebar::{close_key, grip_key, Sidebar};
    use fresh_ui::{Input, Mods, MouseButton, Point, Size, Ui};
    use ratatui::buffer::Buffer;
    use ratatui::layout::Rect;

    fn row_of(index: usize, name: &str, trailing: Option<&str>) -> Row {
        Row {
            index,
            theme: Explorer::panel(),
            left: vec![
                ("  ".to_string(), Explorer::panel()),
                (name.to_string(), Explorer::panel()),
            ],
            trailing: trailing.map(|t| Slot {
                text: t.to_string(),
                theme: pair("diagnostic.warning_fg", "editor.bg"),
                path: std::path::PathBuf::from(name),
            }),
            error: None,
        }
    }

    /// The explorer alone in its column, in the shape the frame builds.
    fn panel_of(rows: Vec<Row>, cols: u16) -> Sidebar {
        let mut s = Sidebar::explorer_only(
            cols,
            true,
            Explorer {
                body: Body::Rows(rows),
                caret_row: None,
                scroll: None,
            },
        );
        s.sections[0].title = " Files ".to_string();
        s
    }

    /// **A right-press below the last row still opens the menu.**
    ///
    /// The component bound its right-press to the whole explorer, so a click
    /// past the last entry opened the menu in its root form. Binding to rows
    /// alone lost that, and it took out the whole `explorer_context_menu`
    /// e2e file — those tests right-click a fixed row (10, 5) that a small
    /// fixture does not have, so they saw no menu at all.
    ///
    /// The assertion is that *something* is said, on empty space. A test that
    /// only right-clicked a row that exists would keep passing with this bug.
    #[test]
    fn a_right_press_below_the_last_row_still_asks_for_a_menu() {
        // Two rows, a panel eight tall: y=6 is inside the panel, below both.
        let e = panel_of(vec![row_of(0, "a.rs", None), row_of(1, "b.rs", None)], 30);
        let mut ui = laid_out(e, 30, 8);
        let got = ui.dispatch(Input::press(
            Point::new(4, 6),
            MouseButton::Right,
            Mods::NONE,
        ));
        assert!(
            got.msgs
                .iter()
                .any(|m| matches!(m, UiMsg::Ui(UiFact::ExplorerBodyContext { .. }))),
            "empty space must still ask for a menu, got {:?}",
            got.msgs
        );
    }

    /// And a right-press *on* a row still reports that row — the panel-level
    /// handler must not swallow or duplicate what a row already answered.
    #[test]
    fn a_right_press_on_a_row_still_reports_that_row() {
        let e = panel_of(vec![row_of(0, "a.rs", None), row_of(1, "b.rs", None)], 30);
        let mut ui = laid_out(e, 30, 8);
        let r = ui.rect_of(ui.find_by_key(&row_key(1)).expect("row 1"));
        let got = ui.dispatch(Input::press(
            Point::new(r.x + 1, r.y),
            MouseButton::Right,
            Mods::NONE,
        ));
        let facts: Vec<_> = got
            .msgs
            .iter()
            .filter_map(|m| match m {
                UiMsg::Ui(f) => Some(f),
                _ => None,
            })
            .collect();
        assert!(
            facts
                .iter()
                .any(|f| matches!(f, UiFact::ExplorerRowContext { index: 1, .. })),
            "got {facts:?}"
        );
        assert!(
            !facts
                .iter()
                .any(|f| matches!(f, UiFact::ExplorerBodyContext { .. })),
            "the row claimed it; the panel must not answer too: {facts:?}"
        );
    }

    fn laid_out(s: Sidebar, w: u16, h: u16) -> Ui<UiMsg> {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            frame_tree(Frame {
                menu_bar: false,
                status_bar: false,
                sidebar: Some(s),
                ..Frame::default()
            }),
            Size::new(w, h),
        );
        ui
    }

    /// A panel whose tree is taller than its body, scrolled to `offset`.
    fn scrolled_panel(total: usize, rows_shown: usize, offset: usize, cols: u16) -> Sidebar {
        scrolled_panel_with_max(total, rows_shown, offset, total - rows_shown, cols)
    }

    /// The same, for a tree whose pinned ancestors push the model's last
    /// offset past `total - rows`.
    ///
    /// The model states the last offset by naming the ancestors it pins:
    /// `max_offset - (total - rows_shown)` of them, the first rows of the
    /// tree, which is what pins a scrolled tree's expanded ancestors are. The
    /// rows described are the ones the model would have windowed — the pins,
    /// then the run under them.
    fn scrolled_panel_with_max(
        total: usize,
        rows_shown: usize,
        offset: usize,
        max_offset: usize,
        cols: u16,
    ) -> Sidebar {
        let pinned: Vec<usize> = (0..max_offset - (total - rows_shown)).collect();
        let run = rows_shown - pinned.len();
        let rows: Vec<Row> = pinned
            .iter()
            .copied()
            .chain(offset..(offset + run).min(total))
            .map(|i| row_of(i, &format!("f{i}"), None))
            .collect();
        let mut s = Sidebar::explorer_only(
            cols,
            true,
            Explorer {
                body: Body::Rows(rows),
                caret_row: None,
                scroll: Some(Scroll {
                    offset,
                    total,
                    pinned,
                }),
            },
        );
        s.sections[0].title = " Files ".to_string();
        s
    }

    /// The background of every cell in the panel's last inner column — the
    /// bar's lane — from the first body row down.
    fn bar_column(e: Sidebar, w: u16, h: u16) -> Vec<ratatui::style::Color> {
        let ui = laid_out(e, w, h);
        let spec = ui.spec().clone();
        let mut buf = Buffer::empty(Rect::new(0, 0, w, h));
        let palette = |k: &fresh_ui::ThemeKey| super::super::fold::test_palette::of(k.as_str());
        fold_native(&spec, &mut buf, &palette, Band::Background);
        // Column 0 is the left wall and the last column is the right one, so
        // the bar's lane is the one before it. Rows: the title is row 0 and
        // the bottom border the last.
        let x = w - 2;
        (1..h - 1).map(|y| buf[(x, y)].bg).collect()
    }

    /// What a thumb cell and a track cell actually carry, painted.
    fn bar_colours() -> (ratatui::style::Color, ratatui::style::Color) {
        let of = |key: &str| {
            super::super::fold::test_palette::painted(&pair(key, key))
                .bg
                .expect("a bar colour")
        };
        (of("ui.scrollbar_thumb_fg"), of("ui.scrollbar_track_fg"))
    }

    /// Issue #2859: a tree taller than the panel draws a bar, and the thumb
    /// is where the window is.
    #[test]
    fn an_overflowing_tree_draws_a_scrollbar() {
        let (thumb, track) = bar_colours();
        assert_ne!(thumb, track, "the two bar colours must differ");

        // 8 rows of body (10 tall, less title and bottom border) onto 40.
        let got = bar_column(scrolled_panel(40, 8, 0, 20), 20, 10);
        assert_eq!(got.len(), 8);
        assert!(
            got.contains(&thumb) && got.contains(&track),
            "an overflowing tree draws a thumb on a track, got {got:?}"
        );
        let first_thumb = got.iter().position(|bg| *bg == thumb);
        assert_eq!(first_thumb, Some(0), "unscrolled, the thumb is at the top");

        // Scrolled to the end, the thumb sits flush against the bottom.
        let got = bar_column(scrolled_panel(40, 8, 32, 20), 20, 10);
        assert_eq!(
            got.last().copied(),
            Some(thumb),
            "fully scrolled, the thumb reaches the track's end, got {got:?}"
        );
    }

    /// Issue #2859, follow-up: the thumb reaches the end of the track exactly
    /// when the *model* is at its last offset — which pinned sticky ancestors
    /// push past `total - rows`. Assuming the naive ceiling parked the thumb
    /// at the bottom while the wheel could still move the list.
    #[test]
    fn the_thumb_reaches_the_end_only_at_the_models_last_offset() {
        let (thumb, _track) = bar_colours();
        // 8 body rows onto 40, with two ancestors pinned: the model scrolls to
        // 34, not to 32.
        let max_offset = 34;
        let at_naive_end = bar_column(scrolled_panel_with_max(40, 8, 32, max_offset, 20), 20, 10);
        assert_ne!(
            at_naive_end.last().copied(),
            Some(thumb),
            "at offset 32 the tree still has rows below, so the thumb is not at the end: {at_naive_end:?}"
        );

        let at_real_end = bar_column(
            scrolled_panel_with_max(40, 8, max_offset, max_offset, 20),
            20,
            10,
        );
        assert_eq!(
            at_real_end.last().copied(),
            Some(thumb),
            "at the model's last offset the thumb is flush with the track's end: {at_real_end:?}"
        );
    }

    /// **The wheel is the window's.** A notch over the rows is not claimed
    /// by a row; it reaches the viewport, which moves its window and reports
    /// where it went, and the panel's own reaction rides along unclaimed —
    /// two facts, in that order: the hook's, then the window's. The window
    /// is controlled, so the report is the *proposal* the model clamps and
    /// stores; the description that follows says where the window is.
    #[test]
    fn a_wheel_over_the_rows_reports_the_window_the_library_moved_to() {
        let mut ui = laid_out(scrolled_panel(40, 8, 3, 20), 20, 10);
        let got = ui.dispatch(Input::Wheel {
            pos: Point::new(4, 3),
            delta: 2,
            axis: fresh_ui::Axis::Vertical,
            mods: Mods::NONE,
        });
        assert!(got.claimed, "a wheel that moved the window is claimed");
        let facts: Vec<_> = got
            .msgs
            .iter()
            .filter_map(|m| match m {
                UiMsg::Ui(f) => Some(f.clone()),
                _ => None,
            })
            .collect();
        assert_eq!(
            facts,
            vec![
                UiFact::ExplorerWheel {
                    delta: 2,
                    x: 4,
                    y: 3
                },
                UiFact::ExplorerScrollTo(5),
            ],
            "the panel's hook, then the window's report"
        );

        // And over the empty space under the last row, which no row ever
        // answered for: the window is the whole body.
        let mut ui = laid_out(scrolled_panel(40, 8, 3, 20), 20, 12);
        let got = ui.dispatch(Input::Wheel {
            pos: Point::new(4, 10),
            delta: 1,
            axis: fresh_ui::Axis::Vertical,
            mods: Mods::NONE,
        });
        assert!(
            got.msgs
                .iter()
                .any(|m| matches!(m, UiMsg::Ui(UiFact::ExplorerScrollTo(4)))),
            "got {:?}",
            got.msgs
        );
    }

    /// A press on the bar is the library's: it jumps the window and reports
    /// the offset, and nothing else on the panel answers the press.
    #[test]
    fn a_press_on_the_bar_reports_the_jump() {
        let mut ui = laid_out(scrolled_panel(40, 8, 0, 20), 20, 10);
        // The bar's lane is the column before the right wall; the last body
        // row is the track's end.
        let got = ui.dispatch(Input::press(
            Point::new(18, 8),
            MouseButton::Left,
            Mods::NONE,
        ));
        assert!(got.claimed);
        assert!(
            matches!(
                got.msgs.as_slice(),
                [UiMsg::Ui(UiFact::ExplorerScrollTo(o))] if *o > 0
            ),
            "one report, toward the end: {:?}",
            got.msgs
        );
    }

    /// **Pinned ancestors are drawn at the top, and answer as themselves.**
    /// The rows the viewport asks the builder for are the pins and then the
    /// run, so window row 1 is the second pinned ancestor rather than
    /// `offset + 1` — and a press there names that ancestor's own index,
    /// because the row's handler carries it and no arithmetic sits between.
    #[test]
    fn pinned_ancestors_sit_above_the_run_and_a_press_names_them() {
        // 8 body rows onto 40, two ancestors (0, 1) pinned, the run from 32.
        let ui = laid_out(scrolled_panel_with_max(40, 8, 32, 34, 20), 20, 10);
        let names: Vec<String> = lines_of(&ui, 20, 10)[1..9]
            .iter()
            .map(|l| l.trim_matches(|c| c == '│' || c == ' ').to_string())
            .collect();
        assert_eq!(
            names,
            ["f0", "f1", "f32", "f33", "f34", "f35", "f36", "f37"],
            "the pins, then six rows of the run"
        );

        let mut ui = ui;
        let got = ui.dispatch(Input::press(
            Point::new(4, 2),
            MouseButton::Left,
            Mods::NONE,
        ));
        assert!(
            matches!(
                got.msgs.as_slice(),
                [UiMsg::Ui(UiFact::ExplorerRowPress { index: 1, .. })]
            ),
            "window row 1 is pinned ancestor 1: {:?}",
            got.msgs
        );
        let got = ui.dispatch(Input::press(
            Point::new(4, 3),
            MouseButton::Left,
            Mods::NONE,
        ));
        assert!(
            matches!(
                got.msgs.as_slice(),
                [UiMsg::Ui(UiFact::ExplorerRowPress { index: 32, .. })]
            ),
            "and the row under the pins is the first of the run: {:?}",
            got.msgs
        );
    }

    /// The painted lines of a laid-out panel — `lines`, for a tree the test
    /// already holds.
    fn lines_of(ui: &Ui<UiMsg>, w: u16, h: u16) -> Vec<String> {
        let spec = ui.spec().clone();
        let mut buf = Buffer::empty(Rect::new(0, 0, w, h));
        let palette = |k: &fresh_ui::ThemeKey| super::super::fold::test_palette::of(k.as_str());
        fold_native(&spec, &mut buf, &palette, Band::Background);
        (0..h)
            .map(|y| {
                (0..w)
                    .map(|x| buf[(x, y)].symbol().to_string())
                    .collect::<String>()
            })
            .collect()
    }

    /// And a tree that fits draws none: no bar cells at all, in either colour.
    #[test]
    fn a_tree_that_fits_draws_no_scrollbar() {
        let (thumb, track) = bar_colours();
        let got = bar_column(panel_of(vec![row_of(0, "a.rs", None)], 20), 20, 10);
        assert!(
            got.iter().all(|bg| *bg != thumb && *bg != track),
            "no bar when the whole tree fits, got {got:?}"
        );
    }

    fn lines(e: Sidebar, w: u16, h: u16) -> Vec<String> {
        let ui = laid_out(e, w, h);
        let spec = ui.spec().clone();
        let mut buf = Buffer::empty(Rect::new(0, 0, w, h));
        let palette = |k: &fresh_ui::ThemeKey| super::super::fold::test_palette::of(k.as_str());
        fold_native(&spec, &mut buf, &palette, Band::Background);
        (0..h)
            .map(|y| {
                (0..w)
                    .map(|x| buf[(x, y)].symbol().to_string())
                    .collect::<String>()
            })
            .collect()
    }

    /// The panel's chrome: a bordered box, the title on the top border where a
    /// ratatui `Block` drew it, and the close button three cells from the right.
    #[test]
    fn the_panel_draws_its_border_title_and_close_button() {
        let got = lines(panel_of(vec![row_of(0, "src", None)], 20), 20, 5);
        assert_eq!(got[0], "┌ Files ─────────×─┐", "title line");
        assert_eq!(got[1], "│  src             │", "first row");
        assert_eq!(got[4], "└──────────────────┘", "bottom border");
    }

    /// A row's status slot is pushed to the right edge by layout, and the gap
    /// before it never closes — `min_w(1)` is the old walk's `min_gap`.
    #[test]
    fn the_status_slot_is_pushed_right_and_keeps_its_gap() {
        let got = lines(panel_of(vec![row_of(0, "a-file", Some("M"))], 20), 20, 4);
        assert_eq!(got[1], "│  a-file         M│");
        // Squeezed until the row no longer fits, the gap still holds its cell
        // — which is what `min_w` is for — and the row overflows.
        //
        // **The border holds and the overflow is clipped** — the same as the
        // ratatui painter, which rendered into the `Block`'s `inner()`.
        //
        // This used to assert the opposite. `.border()` inset its children
        // without clipping them, so a row wider than the panel painted over
        // the frame and turned the right border into a letter; the assertion
        // pinned that as "the behaviour, not endorsed as the design". #3095
        // made `border()` imply `clip`, and its motivating example is this
        // exact shape: a name, a gap that will not close below one cell, and a
        // status slot. So the workaround this test recorded is gone, and the
        // expectation is the painter's again.
        let got = lines(
            panel_of(vec![row_of(0, "a-long-name", Some("M"))], 16),
            16,
            4,
        );
        assert_eq!(
            got[1], "│  a-long-name │",
            "the gap holds and so does the border"
        );
    }

    /// The slot's rectangle is read back off the tree — this is what replaced
    /// `trailing_slot_screen_bounds`, which re-derived the same column from the
    /// indicator width, the leading slot, the chain and the padding rule.
    #[test]
    fn the_slot_rect_comes_from_layout() {
        let ui = laid_out(panel_of(vec![row_of(0, "a-file", Some("M"))], 20), 20, 4);
        let size = Rect::new(0, 0, 20, 4);
        let slot = slot_rect(&ui, 0, size).expect("the slot");
        assert_eq!((slot.x, slot.y, slot.width), (18, 1, 1));
        // A row without a slot reports none, rather than a zero-width sliver
        // that would hit-test.
        let ui = laid_out(panel_of(vec![row_of(0, "a-file", None)], 20), 20, 4);
        assert!(slot_rect(&ui, 0, size).is_none());
    }

    /// A press on a row names the row and carries the run count the host
    /// reported — one fact where the old walk had a single-click route and a
    /// double-click route that derived the row separately.
    #[test]
    fn a_row_press_names_the_row_and_the_run() {
        let mut ui = laid_out(
            panel_of(vec![row_of(0, "a", None), row_of(1, "b", None)], 20),
            20,
            6,
        );
        let e = ui.find_by_key(&row_key(1)).expect("the row");
        let r = ui.rect_of(e);
        let got = ui.dispatch(Input::press_n(
            Point::new(r.x + 2, r.y),
            MouseButton::Left,
            Mods::default(),
            2,
        ));
        assert!(got.claimed);
        assert!(
            matches!(
                got.msgs.as_slice(),
                [UiMsg::Ui(UiFact::ExplorerRowPress {
                    index: 1,
                    clicks: 2
                })]
            ),
            "got {:?}",
            got.msgs
        );
    }

    /// **The title line is not a row.** Pressing it used to select the panel's
    /// first row, because `row - (area.y + 1)` clamps to zero there, while the
    /// right-click and double-click paths guarded it out explicitly. Now it is
    /// decoration and all three agree.
    ///
    /// It is still *the panel*, though, and this asserted it was not a target
    /// at all — which was stricter than the component ever was.
    /// `handle_file_explorer_click` took focus for any left click inside the
    /// rectangle, title line included, before it looked for a row. Selecting
    /// nothing is the rule; answering nothing was an accident of binding the
    /// press to rows alone.
    #[test]
    fn the_title_line_selects_nothing() {
        let mut ui = laid_out(panel_of(vec![row_of(0, "a", None)], 20), 20, 5);
        let got = ui.dispatch(Input::press(
            Point::new(4, 0),
            MouseButton::Left,
            Mods::default(),
        ));
        assert!(
            matches!(got.msgs.as_slice(), [UiMsg::Ui(UiFact::ExplorerBodyPress)]),
            "the title line focuses the panel and selects nothing, got {:?}",
            got.msgs
        );
    }

    /// The close button absorbs its own three cells, and the grip absorbs the
    /// right edge below the title — but the strip carrying them passes
    /// everything else through to the rows beneath.
    #[test]
    fn the_overlay_absorbs_only_its_controls() {
        let mut ui = laid_out(panel_of(vec![row_of(0, "a", None)], 20), 20, 5);
        let close = ui.rect_of(ui.find_by_key(&close_key(0)).expect("close"));
        let got = ui.dispatch(Input::press(
            Point::new(close.x, close.y),
            MouseButton::Left,
            Mods::default(),
        ));
        assert!(
            matches!(got.msgs.as_slice(), [UiMsg::Ui(UiFact::ExplorerClose)]),
            "got {:?}",
            got.msgs
        );

        let grip = ui.rect_of(ui.find_by_key(&grip_key()).expect("grip"));
        assert_eq!(grip.w, 1, "one column");
        assert!(grip.y > close.y, "below the title line");
        let got = ui.dispatch(Input::press(
            Point::new(grip.x, grip.y),
            MouseButton::Left,
            Mods::default(),
        ));
        assert!(
            matches!(
                got.msgs.as_slice(),
                [UiMsg::Ui(UiFact::ExplorerResizeBegin { .. })]
            ),
            "got {:?}",
            got.msgs
        );
        // The grip holds the pointer for the whole drag, so let go of it before
        // asking where the *next* press lands.
        ui.dispatch(Input::release(
            Point::new(grip.x, grip.y),
            MouseButton::Left,
            Mods::default(),
        ));

        // …and the strip between them is not a target: a press on the row
        // underneath the title strip's empty middle reaches the row.
        let row = ui.rect_of(ui.find_by_key(&row_key(0)).expect("row"));
        let got = ui.dispatch(Input::press(
            Point::new(row.x + 1, row.y),
            MouseButton::Left,
            Mods::default(),
        ));
        assert!(
            matches!(
                got.msgs.as_slice(),
                [UiMsg::Ui(UiFact::ExplorerRowPress { index: 0, .. })]
            ),
            "got {:?}",
            got.msgs
        );
    }

    /// Every name this panel paints in resolves against the real theme table.
    #[test]
    fn every_name_resolves() {
        let theme = crate::view::theme::Theme::from_json(r#"{"name":"test"}"#)
            .expect("a theme of nothing but defaults");
        let mut names = vec![
            Explorer::panel(),
            close_theme(true),
            close_theme(false),
            row_theme(true, false, true),
            row_theme(true, false, false),
            row_theme(false, true, true),
            pair("diagnostic.warning_fg", "editor.bg"),
            pair("diagnostic.error_fg", "editor.bg"),
            pair("search.match_fg", "search.match_bg"),
            pair("editor.line_number_fg", "editor.bg"),
        ];
        for disconnected in [true, false] {
            for focused in [true, false] {
                let (t, b) = chrome_themes(disconnected, focused);
                names.push(t);
                names.push(b);
            }
        }
        for k in [true, false] {
            for s in [true, false] {
                for d in [true, false] {
                    names.push(pair(neutral_key(k, s, d), "editor.bg"));
                }
            }
        }
        for name in names {
            let pair_part = name.split('+').next().unwrap_or(&name);
            let (fg, bg) = pair_part.split_once('/').expect("a pair");
            assert!(theme.resolve_theme_key(fg).is_some(), "unknown fg {fg:?}");
            assert!(theme.resolve_theme_key(bg).is_some(), "unknown bg {bg:?}");
        }
    }

    /// The grip repaints the column it sits on, for its whole run — which it
    /// can only do because `layout_reader` runs its builder during layout, with
    /// the extent in hand.
    #[test]
    fn the_hovered_grip_paints_the_wall_and_leaves_the_corners() {
        let mut e = panel_of(vec![row_of(0, "src", None), row_of(1, "lib", None)], 12);
        e.grip_hovered = true;
        let got = lines(e, 12, 5);
        let col = |y: usize| got[y].chars().nth(11).expect("twelve columns");
        for y in 1..4 {
            assert_eq!(col(y), '│', "row {y} is the grip's");
        }
        // The corners are the frame's. The post-pass this replaced walked
        // `0..explorer_area.height` and recoloured both of them.
        assert_eq!(col(0), '┐', "the top corner survives hover");
        assert_eq!(col(4), '┘', "and so does the bottom one");
    }

    /// At rest it paints nothing, rather than painting the wall's `│` a
    /// second time — but it still claims its column for input.
    ///
    /// The wall itself is painted: it is the section's, drawn as text now
    /// that the column's border is assembled from shared rows rather than one
    /// `.border()` box. So what this asserts is *whose* the glyphs in the
    /// grip's column are — every one of them belongs to a node outside the
    /// grip's subtree.
    #[test]
    fn the_resting_grip_leaves_the_border_to_the_border() {
        let e = panel_of(vec![row_of(0, "src", None)], 12);
        assert!(!e.grip_hovered, "the default");
        let ui = laid_out(e, 12, 4);
        let grip_el = ui.find_by_key(&grip_key()).expect("the grip");
        let grip = rect_of(&ui, &grip_key(), Rect::new(0, 0, 12, 4)).expect("the grip");
        assert_eq!(
            (grip.x, grip.width),
            (11, 1),
            "it still claims its column for input"
        );
        let inside_grip = |mut id: fresh_ui::ElementId| loop {
            if id == grip_el {
                return true;
            }
            match ui.parent(id) {
                Some(p) => id = p,
                None => return false,
            }
        };
        let in_column: Vec<_> = ui
            .spec()
            .items
            .iter()
            .filter(|i| matches!(&i.draw, fresh_ui::Draw::Lines(_)))
            .filter(|i| i.rect.x == 11 && i.rect.y > 0 && i.rect.y < 3)
            .collect();
        assert!(!in_column.is_empty(), "the wall is painted by someone");
        assert!(
            in_column.iter().all(|i| !inside_grip(i.id)),
            "the resting grip paints nothing of its own"
        );
    }

    /// Hovering changes the grip's ink, not its glyphs — the wall was already
    /// `│`, drawn by the border.
    #[test]
    fn hover_changes_the_grips_ink_not_its_glyphs() {
        let at_rest = lines(panel_of(vec![row_of(0, "src", None)], 12), 12, 4);
        let mut hot = panel_of(vec![row_of(0, "src", None)], 12);
        hot.grip_hovered = true;
        assert_eq!(at_rest, lines(hot, 12, 4));
    }
}
