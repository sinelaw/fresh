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
//! # What it does not measure
//!
//! **Which rows are visible.** `FileTreeView::viewport_display_indices()`
//! already windows the tree — including its sticky-ancestor rows — and the
//! scroll offset is app state that survives rebuilds. Handing the tree a
//! million-row list and a `Viewport` would be the wrong trade here: the
//! windowing is a model concern (which ancestors are sticky, what the search
//! filter admits), not a layout one.
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
    col, gesture, row, stack, text, text_runs, Event, GestureKind, Key, Node, Run, Sizing,
};

use crate::app::shell_host::shell_theme::{attrs, pair};
use crate::app::types::HoverTarget;

use super::msg::{UiFact, UiMsg};
use super::rect_of;

/// A `(text, theme name)` pair — the same shape the menu bar's labels use.
pub type Runs = Vec<(String, String)>;

/// One visible row of the tree.
///
/// `index` is the **viewport** index, which is what
/// `FileTreeView::get_display_node_at_viewport_row` takes — so a row's key,
/// its hit answer and the model's lookup are all the same number.
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

/// The explorer's content: what the sidebar's first section holds.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Explorer {
    pub body: Body,
    /// The viewport row the caret sits on, when the panel owns the keyboard.
    pub caret_row: Option<usize>,
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
    match &e.body {
        Body::Loading(text_) => col().child(
            text(text_.clone())
                .theme(pair("editor.line_number_fg", "editor.bg"))
                .h(Sizing::Cells(1)),
        ),
        Body::Rows(rows) => col().children(rows.iter().map(|r| node_row(e, r))),
    }
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

fn node_row(e: &Explorer, r: &Row) -> Node<UiMsg> {
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
    let caret = e.caret_row == Some(index);
    let body = row()
        .theme(r.theme.clone())
        .h(Sizing::Cells(1))
        .children(children);
    // The caret indicator the panel paints under the hardware cursor when it
    // owns the keyboard. It replaces the left-most cell of the row, which is
    // what the old `Paragraph::new("▌")` overwrote.
    let body = if caret {
        stack().h(Sizing::Cells(1)).children([
            body,
            row()
                .h(Sizing::Cells(1))
                .children([text("▌").theme(caret_ink(&r.theme)).w(Sizing::Cells(1))]),
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
        // The panel scrolls its own viewport — the surface's wheel, with the
        // surface. `stop()` claims it, as the component's `Consumed` did.
        .on(
            GestureKind::Wheel,
            Rc::new(move |e: &Event| {
                e.stop();
                Some(UiMsg::Ui(UiFact::ExplorerScroll {
                    delta: e.delta,
                    x: e.pos.x.max(0) as u16,
                    y: e.pos.y.max(0) as u16,
                }))
            }),
        )
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
