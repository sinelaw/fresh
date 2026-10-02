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
//! The rows are a [`fresh_ui::Tree::windowed`] over the model's projection
//! of the tree, and the window is the list's, as every other list's is:
//!
//! - **The offset.** The list owns it. The wheel and the bar move it, and a
//!   selection that moves — a key, a search, the `follow_active_buffer`
//!   setting — asks it to show the selected row. It reports where it went
//!   as [`UiFact::ExplorerScrollTo`], which the model only records: a list
//!   mounted again (after a background expand hands the tree out, or on a
//!   window switch) starts there, and the workspace saves it.
//! - **Which rows are pinned.** The expanded ancestors of the run's first
//!   row, asked at layout, where the offset is known — from the parent of
//!   each row, which the model's projection carries. The ceiling is
//!   layout's too: the first offset whose run, under its own pins, reaches
//!   the last row.
//! - **What a row says.** `describe_row` works from the projection's copy of
//!   the node and handles to the caches, so the window describes the rows it
//!   holds, when it holds them. No window height is known before layout, and
//!   none is needed.
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
    col, gesture, row, stack, text, text_runs, ComponentExt, Event, GestureKind, Key, Node,
    PointerMode, Run, Sizing,
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
/// the model's projection is indexed by and what the window counts in — so
/// a row's hit answer, the window's offset and the model's lookup are the
/// same number. A pinned ancestor keeps its own index wherever the window
/// draws it. Its key is the path it shows ([`row_key`]).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Row {
    pub index: usize,
    /// The row's own ground: selection, multi-selection or the panel's.
    pub theme: String,
    /// Indicator, leading slot, compact chain and name, in order.
    pub left: Runs,
    /// Where each compact-chain segment sits in the concatenation of `left`,
    /// outermost first, each taking its own separator.
    ///
    /// Byte ranges, because the library answers a press with the byte of the
    /// label under the pointer ([`fresh_ui::Event::text_byte`]). Deriving a
    /// column from the path instead would have to re-guess the indent, the
    /// indicator and any decoration, and would be wrong on a wide glyph.
    pub chain: Vec<std::ops::Range<usize>>,
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
#[derive(Clone, Debug)]
pub enum Body {
    /// The tree is still being built (initial async build, or expand-to-path).
    /// The panel's chrome is already final — that is the point of this state,
    /// so a slow remote build never paints the window in two stages.
    Loading(String),
    Tree(Tree),
}

impl Default for Body {
    fn default() -> Body {
        // Not an empty tree: a panel with no tree yet is *loading*, and the
        // two look different on purpose.
        Body::Loading(String::new())
    }
}

/// The tree as the window reads it: how many rows there are, and per index
/// a key, a row and the row's parent — answered when layout asks, for the
/// rows the window holds and no others.
///
/// **The window is the list's.** Which rows are on screen, how far down it
/// is, which ancestors are pinned above the run and where its ceiling is
/// are layout's answers: the list windows `count` rows, pins the expanded
/// ancestors of the run's first row (from `parent`), and reports where it
/// went through [`UiFact::ExplorerScrollTo`]. The model keeps that report
/// only to put a list mounted again back where it was (`start`).
#[derive(Clone)]
pub struct Tree {
    pub count: usize,
    pub key: Rc<dyn Fn(usize) -> Key>,
    /// What row `i` says. Asked at layout, by the window.
    pub row: Rc<dyn Fn(usize) -> Row>,
    /// Row `i`'s parent, by row index — what the sticky ancestors are.
    pub parent: Rc<dyn Fn(usize) -> Option<usize>>,
    /// Depth, children and expansion of row `i`, as the tree widget asks.
    pub node: Rc<dyn Fn(usize) -> fresh_ui::widgets::TreeRow>,
    /// The row the keyboard is on.
    pub selected: Option<usize>,
    /// The window follows the selected row whenever this changes, until the
    /// wheel takes it elsewhere. The model moves it whenever it acts on the
    /// selection; a selection moved without it is not brought into view.
    pub reveal: u64,
    /// The `reveal` the window at `start` already answers — so a list
    /// mounted again does not pull the window back to a selection the
    /// reader wheeled away from.
    pub answered: u64,
    /// Whether the panel owns the keyboard, and so draws the caret.
    pub caret: bool,
    /// Where the window starts when the list mounts.
    pub start: usize,
    /// Which window's explorer this is: each window's tree is its own list,
    /// with its own window.
    pub owner: u64,
    /// Where the owner's page keys ask how far a page is.
    pub pager: Option<Rc<fresh_ui::behavior::Pager>>,
}

impl std::fmt::Debug for Tree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Tree")
            .field("count", &self.count)
            .field("selected", &self.selected)
            .field("caret", &self.caret)
            .field("start", &self.start)
            .field("owner", &self.owner)
            .finish_non_exhaustive()
    }
}

#[cfg(test)]
impl Tree {
    /// A tree over `rows` for a test, each keyed by its name (the second of
    /// its left runs), with `parent` for the pins and the window starting at
    /// `start`.
    pub(crate) fn fixture(
        rows: Vec<Row>,
        parent: impl Fn(usize) -> Option<usize> + 'static,
        start: usize,
    ) -> Tree {
        let rows = Rc::new(rows);
        let (keys, described) = (rows.clone(), rows.clone());
        Tree {
            count: rows.len(),
            key: Rc::new(move |i| row_key(std::path::Path::new(&keys[i].left[1].0))),
            row: Rc::new(move |i| described[i].clone()),
            parent: Rc::new(parent),
            node: Rc::new(|_| fresh_ui::widgets::TreeRow {
                depth: 0,
                has_children: false,
                open: false,
            }),
            selected: None,
            reveal: 0,
            answered: 0,
            caret: false,
            start,
            owner: 1,
            pager: None,
        }
    }
}

/// Match VS Code's default upper bound for explorer sticky-scroll rows. The
/// window always leaves at least one row for the run under them.
pub const MAX_STICKY_ANCESTORS: usize = 7;

/// The explorer's content: what the sidebar's first section holds.
#[derive(Clone, Debug, Default)]
pub struct Explorer {
    pub body: Body,
}

impl Explorer {
    /// The panel's ground — the background every row and the border sit on.
    pub fn panel() -> String {
        pair("editor.fg", "editor.bg")
    }
}

/// A row's key: the path it shows. A row is the same row wherever an
/// expansion above it moves it.
pub fn row_key(path: &std::path::Path) -> Key {
    Key::Str(format!("explorer_row{}", path_text(path)).into())
}

/// The key of a row's trailing status slot.
pub fn slot_key(path: &std::path::Path) -> Key {
    Key::Str(format!("explorer_slot{}", path_text(path)).into())
}

/// A path as key text, **losslessly**: `:` and the path when it is UTF-8,
/// else `~` and its raw bytes in hex. `display()` would fold every name that
/// is not UTF-8 onto its replacement characters, so two such files could
/// share a key, and neither could be found again by it.
fn path_text(path: &std::path::Path) -> String {
    match path.to_str() {
        Some(s) => format!(":{s}"),
        None => {
            let hex: String = os_bytes(path.as_os_str())
                .iter()
                .map(|b| format!("{b:02x}"))
                .collect();
            format!("~{hex}")
        }
    }
}

/// The path a row key's text names — [`path_text`] read back.
fn text_path(text: &str) -> Option<std::path::PathBuf> {
    if let Some(s) = text.strip_prefix(':') {
        return Some(std::path::PathBuf::from(s));
    }
    let hex = text.strip_prefix('~')?;
    let bytes = (0..hex.len())
        .step_by(2)
        .map(|i| u8::from_str_radix(hex.get(i..i + 2)?, 16).ok())
        .collect::<Option<Vec<u8>>>()?;
    Some(std::path::PathBuf::from(os_string(bytes)))
}

#[cfg(unix)]
fn os_bytes(s: &std::ffi::OsStr) -> Vec<u8> {
    std::os::unix::ffi::OsStrExt::as_bytes(s).to_vec()
}

#[cfg(unix)]
fn os_string(b: Vec<u8>) -> std::ffi::OsString {
    std::os::unix::ffi::OsStringExt::from_vec(b)
}

#[cfg(windows)]
fn os_bytes(s: &std::ffi::OsStr) -> Vec<u8> {
    std::os::windows::ffi::OsStrExt::encode_wide(s)
        .flat_map(u16::to_le_bytes)
        .collect()
}

#[cfg(windows)]
fn os_string(b: Vec<u8>) -> std::ffi::OsString {
    let wide: Vec<u16> = b
        .chunks_exact(2)
        .map(|c| u16::from_le_bytes([c[0], c[1]]))
        .collect();
    std::os::windows::ffi::OsStringExt::from_wide(&wide)
}

#[cfg(not(any(unix, windows)))]
fn os_bytes(s: &std::ffi::OsStr) -> Vec<u8> {
    s.to_string_lossy().into_owned().into_bytes()
}

#[cfg(not(any(unix, windows)))]
fn os_string(b: Vec<u8>) -> std::ffi::OsString {
    String::from_utf8_lossy(&b).into_owned().into()
}

/// The list's own key, per window: see [`Tree::owner`].
pub fn list_key(owner: u64) -> Key {
    Key::Pair("explorer_list".into(), owner)
}

fn hover_msg(t: Option<HoverTarget>) -> fresh_ui::Handler<UiMsg> {
    Rc::new(move |_: &Event| Some(UiMsg::Ui(UiFact::Hover(t.clone()))))
}

fn runs_of(runs: &Runs) -> Vec<Run> {
    runs.iter()
        .map(|(t, theme)| Run::themed(t.clone(), theme))
        .collect()
}

/// The rows as a description: a window onto the tree, or the loading
/// placeholder while the tree is still being built.
pub fn rows(e: &Explorer) -> Node<UiMsg> {
    let t = match &e.body {
        Body::Loading(text_) => {
            return col().child(
                text(text_.clone())
                    .theme(pair("editor.line_number_fg", "editor.bg"))
                    .h(Sizing::Cells(1)),
            )
        }
        Body::Tree(t) => t.clone(),
    };
    let (describe, caret, selected) = (t.row.clone(), t.caret, t.selected);
    let parent = t.parent.clone();
    let node = t.node.clone();
    let mut list = fresh_ui::Tree::windowed(
        t.count,
        {
            let key = t.key.clone();
            move |i| key(i)
        },
        move |i| node(i),
        move |i, _, _| {
            // Clip hit targets as well as ink to the row's lane. A long
            // row's status slot can otherwise answer hover beneath the
            // scrollbar. A column, so the row is as wide as the lane and its
            // gap pushes the status slot to the edge.
            col()
                .clip(true)
                .child(node_row(caret && selected == Some(i), &describe(i)))
        },
    )
    .sticky(MAX_STICKY_ANCESTORS, move |i| parent(i))
    .list()
    .selection(t.selected)
    // Only the model's requests bring the selection into view: a
    // right-click picks the row its menu is about without scrolling it.
    .follow_on(t.reveal)
    .follow_answered(t.answered)
    // The keys are the editor's keymap's; the rows answer the mouse.
    .focusable(false)
    // Each row paints its own ground; what the list would stamp is the
    // panel's.
    .row_theme(|_, _| Explorer::panel())
    .start_at(t.start)
    .on_scroll(|offset| UiMsg::Ui(UiFact::ExplorerScrollTo(offset)))
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
    .scrollbar_theme(pair("ui.scrollbar_thumb_fg", "ui.scrollbar_track_fg"));
    if let Some(p) = &t.pager {
        list = list.pager(p.clone());
    }
    // A gesture around the window rather than a listener on each row: the
    // wheel is the window's, wherever over it the pointer is — the rows, the
    // empty space under the last one, the bar. It does not `stop()`: the
    // library's scroll chain runs only for a wheel nothing claimed, and the
    // chain is what moves the window and reports where it went. What this
    // listener carries is the part the library cannot know about — the
    // plugin `mouse_scroll` hook wants the pointer, and a wheel over the
    // panel dismisses a transient popup — as it did when the rows claimed it.
    gesture(list.node().key(list_key(t.owner))).on(
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

fn node_row(caret: bool, r: &Row) -> Node<UiMsg> {
    // The padding rule, as layout: a flex spacer, so the cells and the slot's
    // rectangle both come out of it rather than being computed twice.
    //
    // No floor under it. A one-cell floor reserved the lane's last cell, which
    // a label too long for the lane paints over while the hit goes to the
    // spacer; the space that holds a name off the status slot is part of the
    // label instead (`describe_row`).
    let mut children: Vec<Node<UiMsg>> = vec![text_runs(runs_of(&r.left)), row().flex(1)];
    if let Some(slot) = &r.trailing {
        let path = slot.path.clone();
        children.push(
            gesture(text(slot.text.clone()).theme(slot.theme.clone()))
                // Keyed so a caller can ask layout where the slot ended up
                // rather than re-deriving the column.
                .key(slot_key(&slot.path))
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
    let chain = r.chain.clone();
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
    //
    // `Ignore`, not `Transparent`: a transparent node is still hit — it ends
    // the hit path — so on the selected row the press resolved against this
    // overlay, which holds no text, and `Event::text_byte` came back empty.
    // `Ignore` takes the whole subtree out of the hit.
    let body = if caret {
        stack().h(Sizing::Cells(1)).children([
            body,
            row()
                .h(Sizing::Cells(1))
                .pointer_mode(PointerMode::Ignore)
                .children([text("▌")
                    .theme(caret_ink(&r.theme))
                    .w(Sizing::Cells(1))
                    .cursor_byte(0)]),
        ])
    } else {
        body
    };
    // The row's key is on the list's node around this one: see `rows`.
    gesture(body)
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
        //
        // Which segment of a compact row it landed on travels with it. The
        // dispatcher fills `text_byte` from the event's *target*, so this
        // listener up the chain reads it without the segments needing
        // listeners of their own.
        .on(
            GestureKind::Press,
            Rc::new(move |e: &Event| {
                if e.button != fresh_ui::MouseButton::Right || e.mods.ctrl {
                    return None;
                }
                e.stop();
                Some(UiMsg::Ui(UiFact::ExplorerRowContext {
                    index,
                    // From the anchor end, so `1` is the segment next to the
                    // row's own name: see `FileTreeView::chain_segment_node`.
                    segment: e.text_byte.and_then(|b| {
                        chain
                            .iter()
                            .rev()
                            .position(|seg| seg.contains(&b))
                            .map(|back| back + 1)
                    }),
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
    path: &std::path::Path,
    size: ratatui::layout::Rect,
) -> Option<ratatui::layout::Rect> {
    rect_of(ui, &slot_key(path), size)
}

/// The rows the explorer's window holds, as layout placed them: the first
/// row of the run, and every row on screen top to bottom — the pinned
/// ancestors, then the run — by path. `None` when the tree is not laid out.
///
/// **The window is the list's**, so this is where it is read: the run is
/// the viewport's published window, and the pins are the rows the list
/// drew above it, as many as the box has rows the run does not use.
pub fn window_rows(
    ui: &fresh_ui::Ui<UiMsg>,
    owner: u64,
) -> Option<(usize, usize, Vec<std::path::PathBuf>)> {
    let list = ui.find_by_key(&list_key(owner))?;
    let run = ui.window(list)?;
    let pinned = (ui.rect_of(list).h as usize).saturating_sub(run.h as usize);
    let spec = ui.spec();
    let within = spec
        .index
        .iter()
        .find(|(k, _)| *k == list_key(owner))?
        .1
        .clone();
    let rows: Vec<std::path::PathBuf> = spec
        .index
        .iter()
        .filter(|(_, r)| r.start >= within.start && r.end <= within.end)
        .filter_map(|(k, _)| match k {
            Key::Str(s) => s.strip_prefix("explorer_row").and_then(text_path),
            _ => None,
        })
        .collect();
    let first = run.y.max(0) as usize;
    let height = ui.rect_of(list).h as usize;
    // The pins come first in the column; then the run, of which only the
    // window's rows are on screen (the rest is overscan).
    let shown = rows.into_iter().take(pinned + run.h as usize).collect();
    Some((first, height, shown))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A row key names its path exactly, and gives it back — for names that
    /// are not UTF-8 too, which `display()` folded together.
    #[test]
    fn a_row_key_names_its_path_losslessly() {
        let plain = std::path::Path::new("/p/src/main.rs");
        assert_eq!(
            row_key(plain),
            Key::Str("explorer_row:/p/src/main.rs".into())
        );
        let Key::Str(k) = row_key(plain) else {
            unreachable!()
        };
        assert_eq!(
            k.strip_prefix("explorer_row")
                .and_then(text_path)
                .as_deref(),
            Some(plain)
        );
        #[cfg(unix)]
        {
            let odd = |b: &[u8]| std::path::PathBuf::from(os_string(b.to_vec()));
            let (a, b) = (odd(b"/p/\xff"), odd(b"/p/\xfe"));
            assert_ne!(row_key(&a), row_key(&b), "two such names, two keys");
            let Key::Str(k) = row_key(&a) else {
                unreachable!()
            };
            assert_eq!(k.strip_prefix("explorer_row").and_then(text_path), Some(a));
        }
    }
    use crate::view::shell::fold::{fold_native, Band};
    use crate::view::shell::frame::{frame_tree, Frame};
    use crate::view::shell::sidebar::{close_key, grip_key, SectionKind, Sidebar};
    use fresh_ui::{Input, Mods, MouseButton, Point, Size, Ui};
    use ratatui::buffer::Buffer;
    use ratatui::layout::Rect;

    fn row_of(index: usize, name: &str, trailing: Option<&str>) -> Row {
        let row = Row {
            index,
            theme: Explorer::panel(),
            left: vec![
                ("  ".to_string(), Explorer::panel()),
                (name.to_string(), Explorer::panel()),
            ],
            chain: Vec::new(),
            trailing: None,
            error: None,
        };
        match trailing {
            Some(t) => with_marker(row, t),
            None => row,
        }
    }

    /// Give a row a status marker the way `describe_row` does: the slot, plus
    /// the space before it as the label's last run.
    fn with_marker(mut r: Row, text: &str) -> Row {
        let path = std::path::PathBuf::from(&r.left.last().expect("a label").0);
        r.left.push((" ".to_string(), Explorer::panel()));
        r.trailing = Some(Slot {
            text: text.to_string(),
            theme: pair("diagnostic.warning_fg", "editor.bg"),
            path,
        });
        r
    }

    /// A compact-chain row, built as `describe_row` builds one. What keeps the
    /// two from drifting is `ui::file_explorer`'s
    /// `a_compact_rows_chain_ranges_index_its_rendered_label`, which pins
    /// `describe_row`'s own output: if that fails and these pass, this fixture
    /// is the stale copy.
    fn chain_row_of(index: usize, segments: &[&str], name: &str) -> Row {
        let mut left: Runs = vec![
            ("  ".to_string(), Explorer::panel()),
            ("▼ ".to_string(), Explorer::panel()),
        ];
        let mut chain = Vec::new();
        let mut at: usize = left.iter().map(|(t, _)| t.len()).sum();
        for seg in segments {
            let start = at;
            at += seg.len() + "/".len();
            chain.push(start..at);
            left.push((seg.to_string(), Explorer::panel()));
            left.push(("/".to_string(), Explorer::panel()));
        }
        left.push((name.to_string(), Explorer::panel()));
        Row {
            index,
            theme: Explorer::panel(),
            left,
            chain,
            trailing: None,
            error: None,
        }
    }

    fn tree_of(
        rows: Vec<Row>,
        parent: impl Fn(usize) -> Option<usize> + 'static,
        start: usize,
    ) -> Tree {
        Tree::fixture(rows, parent, start)
    }

    /// The explorer alone in its column, in the shape the frame builds.
    fn panel_with(tree: Tree, cols: u16) -> Sidebar {
        let mut s = Sidebar::explorer_only(
            cols,
            true,
            Explorer {
                body: Body::Tree(tree),
            },
        );
        s.sections[0].title = " Files ".to_string();
        s
    }

    fn panel_of(rows: Vec<Row>, cols: u16) -> Sidebar {
        panel_with(tree_of(rows, |_| None, 0), cols)
    }

    fn key_of(name: &str) -> Key {
        row_key(std::path::Path::new(name))
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
        let r = ui.rect_of(ui.find_by_key(&key_of("b.rs")).expect("row 1"));
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

    /// A right-press on one segment of a compact row names that segment, with
    /// the multi-byte indicator in front of the names.
    #[test]
    fn a_right_press_on_a_chain_segment_names_that_segment() {
        let e = panel_of(
            vec![
                row_of(0, "proj", None),
                chain_row_of(1, &["dir1", "dir2"], "dir3"),
            ],
            30,
        );
        let mut ui = laid_out(e, 30, 8);
        // The chain row is the one below `proj`, and its label starts at the
        // lane's left edge: two cells of indent, two of indicator, then
        // `dir1/dir2/dir3`.
        let lane = ui.rect_of(ui.find_by_key(&key_of("proj")).expect("row 0"));
        let (x, y) = (lane.x, lane.y + 1);
        let segment_at = |ui: &mut Ui<UiMsg>, col: i32| {
            let got = ui.dispatch(Input::press(
                Point::new(x + col, y),
                MouseButton::Right,
                Mods::NONE,
            ));
            got.msgs
                .iter()
                .find_map(|m| match m {
                    UiMsg::Ui(UiFact::ExplorerRowContext { index, segment, .. }) => {
                        Some((*index, *segment))
                    }
                    _ => None,
                })
                .unwrap_or_else(|| panic!("no menu for column {col}: {:?}", got.msgs))
        };

        // `dir1` is cells 4..8, its separator is cell 8, `dir2` is 9..13 and
        // its separator 13; `dir3`, the row's own name, starts at 14. The
        // segment is counted up from the anchor, so `dir2` is 1 and `dir1` 2.
        assert_eq!(segment_at(&mut ui, 5), (1, Some(2)), "on dir1");
        assert_eq!(segment_at(&mut ui, 8), (1, Some(2)), "dir1's separator");
        assert_eq!(segment_at(&mut ui, 10), (1, Some(1)), "on dir2");
        assert_eq!(
            segment_at(&mut ui, 15),
            (1, None),
            "on dir3, the row itself"
        );
        // The indent and the indicator are no segment's: they are the row's.
        assert_eq!(segment_at(&mut ui, 0), (1, None), "the indent");
        assert_eq!(segment_at(&mut ui, 2), (1, None), "the indicator");
        // And a plain row has no segments to name at all.
        let got = ui.dispatch(Input::press(
            Point::new(lane.x + 3, lane.y),
            MouseButton::Right,
            Mods::NONE,
        ));
        assert!(
            got.msgs.iter().any(|m| matches!(
                m,
                UiMsg::Ui(UiFact::ExplorerRowContext {
                    index: 0,
                    segment: None,
                    ..
                })
            )),
            "got {:?}",
            got.msgs
        );
    }

    /// A label too long for the lane still names the segment drawn in its last
    /// cell, which the gap's old one-cell floor had taken while paint drew the
    /// label over it.
    #[test]
    fn a_label_wider_than_the_lane_still_names_its_last_visible_segment() {
        // `  ` + `▼ ` + `averylongone/` puts `another` at cells 17..23 of the
        // row, and `third` — the anchor's own name — at 25. A 24-cell panel
        // leaves a 22-cell lane, so the last cell it has, 21, draws a character
        // of `another`.
        let rows = vec![
            row_of(0, "proj", None),
            chain_row_of(1, &["averylongone", "another"], "third"),
        ];
        let mut ui = laid_out(panel_with(tree_of(rows, |_| None, 0), 24), 24, 8);
        let lane = ui.rect_of(ui.find_by_key(&key_of("proj")).expect("row 0"));
        let seg = |ui: &mut Ui<UiMsg>, col: i32| {
            let got = ui.dispatch(Input::press(
                Point::new(lane.x + col, lane.y + 1),
                MouseButton::Right,
                Mods::NONE,
            ));
            got.msgs
                .iter()
                .find_map(|m| match m {
                    UiMsg::Ui(UiFact::ExplorerRowContext { segment, .. }) => Some(*segment),
                    _ => None,
                })
                .unwrap_or_else(|| panic!("no menu for column {col}: {:?}", got.msgs))
        };
        assert_eq!(seg(&mut ui, 5), Some(2), "on averylongone");
        assert_eq!(seg(&mut ui, 18), Some(1), "on another");
        assert_eq!(
            seg(&mut ui, 21),
            Some(1),
            "the lane's last cell, still `another`"
        );
    }

    /// And the same row carrying a status marker, which a directory gets from
    /// the files under it. (On an overflowing row the marker itself ends up
    /// zero-width and undrawn — pre-existing, and not what this pins.)
    #[test]
    fn an_overflowing_label_with_a_status_marker_answers_the_same() {
        let long = with_marker(chain_row_of(1, &["averylongone", "another"], "third"), "M");
        let mut ui = laid_out(
            panel_with(
                tree_of(vec![row_of(0, "proj", None), long], |_| None, 0),
                24,
            ),
            24,
            8,
        );
        let lane = ui.rect_of(ui.find_by_key(&key_of("proj")).expect("row 0"));
        let drawn = lines_of(&ui, 24, 8)[lane.y as usize + 1].clone();
        let seg = |ui: &mut Ui<UiMsg>, col: i32| {
            let got = ui.dispatch(Input::press(
                Point::new(lane.x + col, lane.y + 1),
                MouseButton::Right,
                Mods::NONE,
            ));
            got.msgs
                .iter()
                .find_map(|m| match m {
                    UiMsg::Ui(UiFact::ExplorerRowContext { segment, .. }) => Some(*segment),
                    _ => None,
                })
                .unwrap_or_else(|| panic!("no menu for column {col}: {:?}", got.msgs))
        };
        assert_eq!(seg(&mut ui, 5), Some(2), "on averylongone: {drawn:?}");
        assert_eq!(seg(&mut ui, 18), Some(1), "on another: {drawn:?}");
        assert_eq!(seg(&mut ui, 21), Some(1), "the lane's last cell: {drawn:?}");
    }

    /// And on the row the caret is on — the row a reader is most likely to
    /// right-click twice. It takes a fixture with the caret drawn to catch it:
    /// a transparent overlay is still hit, and the press resolved against it.
    #[test]
    fn the_caret_does_not_hide_the_segment_under_it() {
        let rows = vec![
            row_of(0, "proj", None),
            chain_row_of(1, &["dir1", "dir2"], "dir3"),
        ];
        let mut tree = tree_of(rows, |_| None, 0);
        tree.selected = Some(1);
        tree.caret = true;
        let mut ui = laid_out(panel_with(tree, 30), 30, 8);
        let lane = ui.rect_of(ui.find_by_key(&key_of("proj")).expect("row 0"));
        let got = ui.dispatch(Input::press(
            Point::new(lane.x + 5, lane.y + 1),
            MouseButton::Right,
            Mods::NONE,
        ));
        assert!(
            got.msgs.iter().any(|m| matches!(
                m,
                UiMsg::Ui(UiFact::ExplorerRowContext {
                    index: 1,
                    segment: Some(2),
                    ..
                })
            )),
            "the caret's row must still name dir1: {:?}",
            got.msgs
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

    /// A tree of `total` rows `f0`, `f1`, …, its window starting at
    /// `offset`.
    fn scrolled_panel(total: usize, offset: usize, cols: u16) -> Sidebar {
        pinned_panel(total, 0, offset, cols)
    }

    /// The same, for a tree whose first `pins` rows are a chain of expanded
    /// ancestors of every row below them — so a window scrolled past them
    /// pins all of them, and its last offset is past `total - rows` by as
    /// many.
    fn pinned_panel(total: usize, pins: usize, offset: usize, cols: u16) -> Sidebar {
        let rows: Vec<Row> = (0..total)
            .map(|i| row_of(i, &format!("f{i}"), None))
            .collect();
        let parent = move |i: usize| match i {
            0 => None,
            i if i < pins => Some(i - 1),
            _ if pins > 0 => Some(pins - 1),
            _ => None,
        };
        panel_with(tree_of(rows, parent, offset), cols)
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
        let got = bar_column(scrolled_panel(40, 0, 20), 20, 10);
        assert_eq!(got.len(), 8);
        assert!(
            got.contains(&thumb) && got.contains(&track),
            "an overflowing tree draws a thumb on a track, got {got:?}"
        );
        let first_thumb = got.iter().position(|bg| *bg == thumb);
        assert_eq!(first_thumb, Some(0), "unscrolled, the thumb is at the top");

        // Scrolled to the end, the thumb sits flush against the bottom.
        let got = bar_column(scrolled_panel(40, 32, 20), 20, 10);
        assert_eq!(
            got.last().copied(),
            Some(thumb),
            "fully scrolled, the thumb reaches the track's end, got {got:?}"
        );
    }

    /// Issue #2859, follow-up: the thumb reaches the end of the track exactly
    /// at the window's last offset — which pinned sticky ancestors push past
    /// `total - rows`. Assuming the naive ceiling parked the thumb at the
    /// bottom while the wheel could still move the list.
    #[test]
    fn the_thumb_reaches_the_end_only_at_the_last_offset() {
        let (thumb, _track) = bar_colours();
        // 8 body rows onto 40, with two ancestors pinned: the window scrolls
        // to 34, not to 32.
        let max_offset = 34;
        let at_naive_end = bar_column(pinned_panel(40, 2, 32, 20), 20, 10);
        assert_ne!(
            at_naive_end.last().copied(),
            Some(thumb),
            "at offset 32 the tree still has rows below, so the thumb is not at the end: {at_naive_end:?}"
        );

        let at_real_end = bar_column(pinned_panel(40, 2, max_offset, 20), 20, 10);
        assert_eq!(
            at_real_end.last().copied(),
            Some(thumb),
            "at the last offset the thumb is flush with the track's end: {at_real_end:?}"
        );
    }

    /// **The wheel is the window's.** A notch over the rows is not claimed
    /// by a row; it reaches the viewport, which moves its window and reports
    /// where it went, and the panel's own reaction rides along unclaimed —
    /// two facts, in that order: the hook's, then the window's. The window
    /// is the list's; the report is what the model records.
    #[test]
    fn a_wheel_over_the_rows_reports_the_window_the_library_moved_to() {
        let mut ui = laid_out(scrolled_panel(40, 3, 20), 20, 10);
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
        let mut ui = laid_out(scrolled_panel(40, 3, 20), 20, 12);
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
        let mut ui = laid_out(scrolled_panel(40, 0, 20), 20, 10);
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

    /// And a right-press on the bar is the bar's too: it opens no menu. The
    /// panel's catch-all is behind the gutter, so a one-column miss used to
    /// answer with the panel's menu and move the cursor to the root.
    #[test]
    fn a_right_press_on_the_bar_opens_nothing() {
        let mut ui = laid_out(scrolled_panel(40, 0, 20), 20, 10);
        let got = ui.dispatch(Input::press(
            Point::new(18, 8),
            MouseButton::Right,
            Mods::NONE,
        ));
        assert!(got.claimed, "the bar spends the press");
        assert!(
            !got.msgs.iter().any(|m| matches!(
                m,
                UiMsg::Ui(UiFact::ExplorerBodyContext { .. })
                    | UiMsg::Ui(UiFact::ExplorerRowContext { .. })
            )),
            "and asks for no menu: {:?}",
            got.msgs
        );
        // The row beside it still answers, one column to the left.
        let got = ui.dispatch(Input::press(
            Point::new(17, 8),
            MouseButton::Right,
            Mods::NONE,
        ));
        assert!(
            got.msgs
                .iter()
                .any(|m| matches!(m, UiMsg::Ui(UiFact::ExplorerRowContext { .. }))),
            "the lane's last row column is still the row's: {:?}",
            got.msgs
        );
    }

    /// **Pinned ancestors are drawn at the top, and answer as themselves.**
    /// The window asks for the expanded ancestors of the run's first row, at
    /// layout, and draws them above the run — so window row 1 is the second
    /// pinned ancestor rather than `offset + 1`, and a press there names that
    /// ancestor's own index, because the row's handler carries it and no
    /// arithmetic sits between.
    #[test]
    fn pinned_ancestors_sit_above_the_run_and_a_press_names_them() {
        // 8 body rows onto 40, two ancestors (0, 1) pinned, the run from 32.
        let ui = laid_out(pinned_panel(40, 2, 32, 20), 20, 10);
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

    /// **A selection far down is shown under its ancestors.** The window
    /// follows a selection that moved, and the run it reveals it in is the
    /// run under the pins of the offset it lands on — so the selected row is
    /// on screen however many ancestors that offset pins. The model states
    /// only the selection, and that it wants it shown. And [`window_rows`] reads back what layout put
    /// on screen: the pins, then the run.
    #[test]
    fn a_selection_far_down_is_shown_under_its_pinned_ancestors() {
        let mut tree = match pinned_panel(40, 2, 0, 20).sections[0].kind.clone() {
            SectionKind::Explorer(Explorer {
                body: Body::Tree(t),
            }) => t,
            _ => unreachable!("the fixture is an explorer"),
        };
        let mut ui = laid_out(panel_with(tree.clone(), 20), 20, 10);
        let (first, height, shown) = window_rows(&ui, 1).expect("laid out");
        assert_eq!(first, 0);
        assert_eq!(height, 8, "the body's height");
        assert_eq!(shown.len(), 8, "a body of eight rows: {shown:?}");

        // A keyboard move: the model asks for the selection to be shown.
        tree.selected = Some(39);
        tree.reveal += 1;
        ui.frame(
            frame_tree(Frame {
                menu_bar: false,
                status_bar: false,
                sidebar: Some(panel_with(tree, 20)),
                ..Frame::default()
            }),
            Size::new(20, 10),
        );
        let (first, height, shown) = window_rows(&ui, 1).expect("laid out");
        assert_eq!(height, 8, "the body's height, pins and all");
        let names: Vec<String> = shown.iter().map(|p| p.display().to_string()).collect();
        assert_eq!(first, 34, "the last offset, past `40 - 8` by the two pins");
        assert_eq!(
            names,
            ["f0", "f1", "f34", "f35", "f36", "f37", "f38", "f39"],
            "the pins, then a run that ends on the selection"
        );
    }

    /// The explorer's tree out of a fixture panel.
    fn tree_in(panel: Sidebar) -> Tree {
        match panel.sections[0].kind.clone() {
            SectionKind::Explorer(Explorer {
                body: Body::Tree(t),
            }) => t,
            _ => unreachable!("the fixture is an explorer"),
        }
    }

    /// Lay `tree` out again in `ui`, as the next frame does.
    fn reframe(ui: &mut Ui<UiMsg>, tree: Tree) {
        ui.frame(
            frame_tree(Frame {
                menu_bar: false,
                status_bar: false,
                sidebar: Some(panel_with(tree, 20)),
                ..Frame::default()
            }),
            Size::new(20, 10),
        );
    }

    /// **Mounted again, the explorer is where the reader left it** — a window
    /// switch or a hidden sidebar builds the list anew. The model's saved
    /// window answers every reveal it asked before it, so the list starts
    /// there; a reveal asked since (a background expand-to-path) is shown.
    #[test]
    fn mounted_again_the_explorer_keeps_its_window_unless_asked_since() {
        let mut tree = tree_in(pinned_panel(40, 0, 30, 20));
        tree.selected = Some(0);
        tree.reveal = 5;
        tree.answered = 5;
        let ui = laid_out(panel_with(tree.clone(), 20), 20, 10);
        assert_eq!(window_rows(&ui, 1).expect("laid out").0, 30, "where it was");

        tree.reveal = 6;
        let ui = laid_out(panel_with(tree, 20), 20, 10);
        assert_eq!(window_rows(&ui, 1).expect("laid out").0, 0, "the selection");
    }

    /// **A right-click on a pinned folder does not scroll.** It picks the row
    /// the menu is about — the model moves the selection without asking for
    /// it to be shown — so the rows under the menu stay where they were.
    #[test]
    fn a_selection_moved_without_a_reveal_leaves_the_window() {
        let mut tree = tree_in(pinned_panel(40, 2, 32, 20));
        tree.selected = Some(35);
        tree.reveal = 1;
        tree.answered = 1;
        let mut ui = laid_out(panel_with(tree.clone(), 20), 20, 10);
        assert_eq!(window_rows(&ui, 1).expect("laid out").0, 32);

        tree.selected = Some(1);
        reframe(&mut ui, tree.clone());
        assert_eq!(
            window_rows(&ui, 1).expect("laid out").0,
            32,
            "no reveal asked"
        );

        tree.reveal = 2;
        reframe(&mut ui, tree);
        assert_eq!(window_rows(&ui, 1).expect("laid out").0, 1, "asked: shown");
    }

    /// **Collapsing a pinned folder keeps it on screen.** Collapsed, the
    /// tree is shorter than the window was scrolled; the folder stays
    /// selected and the window shows it.
    #[test]
    fn a_collapsed_pinned_folder_stays_on_screen() {
        let mut tree = tree_in(pinned_panel(40, 2, 32, 20));
        tree.selected = Some(1);
        tree.reveal = 1;
        tree.answered = 1;
        let mut ui = laid_out(panel_with(tree, 20), 20, 10);
        // Collapsed: the folder at row 1 has no rows under it.
        let mut folded = tree_in(pinned_panel(3, 2, 32, 20));
        folded.selected = Some(1);
        folded.reveal = 2;
        folded.answered = 1;
        reframe(&mut ui, folded);
        let (_, _, shown) = window_rows(&ui, 1).expect("laid out");
        assert!(
            shown.iter().any(|p| p == std::path::Path::new("f1")),
            "the folder is on screen: {shown:?}"
        );
    }

    /// **A page is the run the window was given**, recorded at layout for
    /// the owner's page keys to ask — not a height the model wrote down
    /// while describing the frame.
    #[test]
    fn a_page_is_the_run_layout_gave_the_tree() {
        let pager = fresh_ui::behavior::Pager::new();
        let mut tree = match pinned_panel(40, 2, 32, 20).sections[0].kind.clone() {
            SectionKind::Explorer(Explorer {
                body: Body::Tree(t),
            }) => t,
            _ => unreachable!("the fixture is an explorer"),
        };
        tree.pager = Some(pager.clone());
        assert_eq!(pager.target(0, 1, 40), None, "not laid out: no page");
        let _ui = laid_out(panel_with(tree, 20), 20, 10);
        // Eight body rows, two of them pins at offset 32: a run of six.
        assert_eq!(pager.target(0, 1, 40), Some(6));
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
        let slot = slot_rect(&ui, std::path::Path::new("a-file"), size).expect("the slot");
        assert_eq!((slot.x, slot.y, slot.width), (18, 1, 1));
        // A row without a slot reports none, rather than a zero-width sliver
        // that would hit-test.
        let ui = laid_out(panel_of(vec![row_of(0, "a-file", None)], 20), 20, 4);
        assert!(slot_rect(&ui, std::path::Path::new("a-file"), size).is_none());
    }

    #[test]
    fn a_scrollbar_keeps_the_status_slot_hittable_beside_it() {
        let rows = std::iter::once(row_of(0, "a-file", Some("M")))
            .chain((1..40).map(|i| row_of(i, &format!("f{i}"), None)))
            .collect();
        let mut tree = tree_of(rows, |_| None, 0);
        // The caret is a paint-only overlay. It must not turn the selected
        // row into one opaque hit target and hide the status gesture below.
        tree.selected = Some(0);
        tree.caret = true;
        let mut ui = laid_out(panel_with(tree, 20), 20, 4);
        let size = Rect::new(0, 0, 20, 4);
        let slot = slot_rect(&ui, std::path::Path::new("a-file"), size).expect("the slot");
        assert_eq!((slot.x, slot.y, slot.width), (17, 1, 1));

        let got = ui.dispatch(Input::Move {
            pos: Point::new(slot.x as i32, slot.y as i32),
            mods: Mods::NONE,
        });
        assert!(
            got.msgs.iter().any(|m| matches!(
                m,
                UiMsg::Ui(UiFact::Hover(Some(
                    HoverTarget::FileExplorerStatusIndicator(path)
                ))) if path == std::path::Path::new("a-file")
            )),
            "got {:?}",
            got.msgs
        );

        // The bar's own column answers nothing: the row is clipped to its lane.
        let bar = ui.dispatch(Input::Move {
            pos: Point::new(slot.x as i32 + 1, slot.y as i32),
            mods: Mods::NONE,
        });
        assert!(
            !bar.msgs.iter().any(|m| matches!(
                m,
                UiMsg::Ui(UiFact::Hover(Some(
                    HoverTarget::FileExplorerStatusIndicator(_)
                )))
            )),
            "the scrollbar column claimed a status marker: {:?}",
            bar.msgs
        );
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
        let e = ui.find_by_key(&key_of("b")).expect("the row");
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
        let row = ui.rect_of(ui.find_by_key(&key_of("a")).expect("row"));
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
