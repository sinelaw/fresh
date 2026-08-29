//! The status bar as a description — the last of S2, and the last surface
//! whose geometry was computed twice.
//!
//! `StatusBarRenderer::render_status` placed every element itself: a running
//! `used_left` cursor, a separator width added between items, a right side
//! measured backwards from the edge, and a `left_max_width` budget derived
//! from the two. It emitted the painted spans *and* a [`StatusBarLayout`]
//! carrying every clickable segment's `(row, start, end)`. Because a click
//! arrives long after the paint, the same walk ran again at event time
//! through `compute_status_layout` → `status_bar_layout_now`, on live state
//! that may have moved since. Two runs of one walk, reconciled by nothing.
//!
//! Here the bar says what is *on* it — the pieces of each element, their
//! colours, and the identity each one answers to — and layout decides every
//! column. `clickable_rects` and `segments` read the result back, which is
//! what a hover, a click, a popup anchor and the web projection all use.
//!
//! **What stays app-side, and why it is not geometry.** Which right-hand
//! elements appear at all is a *content* decision the bar makes from measured
//! text: when the right side would crowd the left below its budget, the
//! lowest-priority right elements are dropped. That is the same rule as
//! before and it still lives in the editor — a description that listed
//! elements layout would then silently discard would be lying about what is
//! on the bar. What moved here is where the surviving elements land.

use std::rc::Rc;

use fresh_ui::{gesture, row, text_runs, Event, GestureKind, Key, Node, Run, Sizing};

use super::rect_of;
use crate::app::types::HoverTarget;
use crate::view::ui::status_bar::StatusBarClickable;

use super::msg::{UiFact, UiMsg};

/// Which side an element was tiled on. Carried rather than re-derived from a
/// midpoint of `x`, so the web orders segments exactly as the terminal does.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Side {
    Left,
    Right,
}

impl Side {
    pub fn name(self) -> &'static str {
        match self {
            Side::Left => "left",
            Side::Right => "right",
        }
    }
}

/// One element on the bar.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Item {
    /// The element's text in pieces, each with the theme name it paints in.
    /// One piece for most elements; several where an element styles part of
    /// itself (a filename with a modified marker, a diagnostic count).
    pub runs: Vec<(String, String)>,
    /// The stable semantic name `status_view` projects — "lsp", "warning",
    /// "language", …
    pub name: &'static str,
    /// Set when the element answers a click.
    pub clickable: Option<StatusBarClickable>,
    /// `"<plugin>:<token>"` for a plugin-registered token, which is how the
    /// click rail finds it to fire `status_bar_token_clicked`.
    pub token_key: Option<String>,
}

impl Item {
    /// The element's whole text, for the web projection's segment.
    pub fn text(&self) -> String {
        self.runs.iter().map(|(t, _)| t.as_str()).collect()
    }
}

/// The bar, as the tree will measure it.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct StatusBar {
    pub left: Vec<Item>,
    pub right: Vec<Item>,
    /// Drawn between elements, verbatim from config. Empty disables
    /// separators and consumes no width — which layout gets for free, because
    /// an empty run measures zero.
    pub separator: String,
    /// The bar's own ground, laid under everything so gaps and padding
    /// resolve to it.
    pub base_theme: String,
    /// The separator glyph's own colour, so it can be dimmed against the bar.
    pub sep_theme: String,
}

/// The key an element is looked up by. Side and index, because that is what
/// identifies a position on the bar; `clickable` and `token_key` are
/// properties *of* the element rather than its address.
pub fn item_key(side: Side, index: usize) -> Key {
    let tag = match side {
        Side::Left => "status_left",
        Side::Right => "status_right",
    };
    Key::Pair(tag.into(), index as u64)
}

fn element(bar: &StatusBar, it: &Item, key: Key) -> Node<UiMsg> {
    let runs = text_runs(
        it.runs
            .iter()
            .map(|(t, theme)| Run::themed(t.clone(), theme.clone())),
    )
    .h(Sizing::Cells(1))
    .key(key);
    let _ = bar;
    // What a press on this element means: a built-in indicator names its id, a
    // plugin token names its registry key, and anything else is inert — still
    // keyed, because the web projection and the theme inspector read every
    // element back, not only the ones that answer a press.
    let fact = match (it.clickable, it.token_key.clone()) {
        (Some(id), _) => UiFact::StatusBarClicked(id),
        (None, Some(key)) => UiFact::StatusBarTokenClicked(key),
        (None, None) => return runs,
    };
    let hover = it.clickable.map(HoverTarget::StatusBarClickable);
    gesture(runs)
        // **Press, not `Click`.** The old `chrome::StatusBar::on_pointer` fired
        // on `PointerPress::Left` — a mouse-*down* — and every other migrated
        // surface kept that (the explorer's rows, the menu bar's labels, the
        // search-options row). A terminal sends a press and a release, so
        // `Click` looked equivalent there; the web frontend synthesises the
        // press alone at the segment's cell, so a `Click` handler never fired
        // and the Remote/LSP/read-only menus stopped opening in the browser.
        .on(
            GestureKind::Press,
            Rc::new(move |e: &Event| {
                if e.button != fresh_ui::MouseButton::Left {
                    return None;
                }
                e.stop();
                Some(UiMsg::Ui(fact.clone()))
            }),
        )
        .on_enter(hover_msg(hover))
        .on_leave(hover_msg(None))
}

fn hover_msg(t: Option<HoverTarget>) -> fresh_ui::Handler<UiMsg> {
    Rc::new(move |_: &Event| Some(UiMsg::Ui(UiFact::Hover(t.clone()))))
}

/// How much of each left-hand element survives once the right side is
/// reserved — the rule `render_status` spelled `left_max_width`.
///
/// Returns one width per *surviving* left element, in order: each is the
/// element's natural width except possibly the last, which is the truncated
/// remainder. Elements past the end of the returned slice do not fit at all.
///
/// **Why this is not layout's job.** The flex gap places the right side
/// against the edge, but placement is not priority: `prim.rs` resolves
/// children in order against `avail - fixed_used`, so the left side (first)
/// takes its natural width and the right side gets what is left, down to
/// zero. Deciding *who yields* is a content decision, and it is made here
/// from measured text, before any node exists.
///
/// Ported from the deleted `render_status`, boundaries included: below 15
/// cells nothing is reserved, and an element that cannot fit ends the side.
pub fn left_budget(
    left_widths: &[usize],
    right_width: usize,
    sep_w: usize,
    available: usize,
) -> Vec<usize> {
    let left_max = if available < 15 {
        available
    } else if available > right_width + 1 {
        available - right_width - 1
    } else {
        1
    };
    let mut out = Vec::with_capacity(left_widths.len());
    let mut used = 0usize;
    for (idx, &w) in left_widths.iter().enumerate() {
        let sep = if idx == 0 { 0 } else { sep_w };
        if used + sep >= left_max {
            break;
        }
        used += sep;
        let remaining = left_max - used;
        if w <= remaining {
            used += w;
            out.push(w);
        } else {
            out.push(remaining);
            break;
        }
    }
    out
}

fn separator(bar: &StatusBar) -> Node<UiMsg> {
    text_runs([Run::themed(bar.separator.clone(), bar.sep_theme.clone())]).h(Sizing::Cells(1))
}

/// The bar's row.
///
/// Left elements, then a flexible gap, then right elements. The gap is the
/// whole of what `left_max_width = available - right_width - 1` used to
/// compute: layout gives the fixed elements their width and the gap whatever
/// is left, so the right side sits against the edge without anyone measuring
/// backwards from it.
pub fn status_bar(bar: &StatusBar) -> Node<UiMsg> {
    let mut kids: Vec<Node<UiMsg>> = Vec::new();
    for (i, it) in bar.left.iter().enumerate() {
        if i > 0 {
            kids.push(separator(bar));
        }
        kids.push(element(bar, it, item_key(Side::Left, i)));
    }
    // The gap. It takes no minimum: on a bar too narrow for both sides it
    // closes completely, and the elements themselves are what layout then
    // clamps — which is the behaviour the old `left_max_width` produced by
    // arithmetic.
    kids.push(row().flex(1));
    for (i, it) in bar.right.iter().enumerate() {
        if i > 0 {
            kids.push(separator(bar));
        }
        kids.push(element(bar, it, item_key(Side::Right, i)));
    }
    // **The row claims its own gaps.** Every element answers its own press,
    // and between them is the flexible gap and the padding either side of a
    // separator — a press there means nothing, and letting it through put the
    // caret in the buffer below. `chrome:status_bar` was a rectangle whose
    // only job was to be in the way; a gesture on the row is the same rule
    // where the row is.
    fresh_ui::gesture(
        row()
            .theme(bar.base_theme.clone())
            .h(Sizing::Cells(1))
            .children(kids),
    )
    .on(
        fresh_ui::GestureKind::Press,
        std::rc::Rc::new(|e: &fresh_ui::Event| {
            e.stop();
            None
        }),
    )
}

// ── reading the laid-out bar back ──────────────────────────────────────────

/// Every clickable element's screen rectangle, in render order.
///
/// This is what `StatusBarLayout::clickable` was — but read from the tree that
/// painted rather than recomputed by a second walk over state that has moved
/// on. That type and its walk are deleted; this is the only source now.
pub fn clickable_rects(
    ui: &fresh_ui::Ui<UiMsg>,
    bar: &StatusBar,
    size: ratatui::layout::Rect,
) -> Vec<(StatusBarClickable, ratatui::layout::Rect)> {
    sides(bar)
        .filter_map(|(side, i, it)| {
            let id = it.clickable?;
            Some((id, rect_of(ui, &item_key(side, i), size)?))
        })
        .collect()
}

/// The theme-key provenance of every painted cell on the bar, in paint order:
/// the bar's own ground first, then each element and separator over it.
///
/// The old recorder emitted exactly these runs *during* the paint walk. They
/// come from the laid-out tree now, which is why the walk could go.
pub fn provenance_runs(
    ui: &fresh_ui::Ui<UiMsg>,
    bar: &StatusBar,
    size: ratatui::layout::Rect,
    row: ratatui::layout::Rect,
) -> Vec<(u16, u16, u16, Option<String>, Option<String>)> {
    let mut out = vec![(
        row.x,
        row.y,
        row.width,
        Some("ui.status_bar_fg".to_string()),
        Some("ui.status_bar_bg".to_string()),
    )];
    for (side, i, it) in sides(bar) {
        let Some(r) = rect_of(ui, &item_key(side, i), size) else {
            continue;
        };
        // Read out of the run's own theme rather than from a field beside it.
        // The first run's names stand for the element: a run whose colour has
        // no name reports `None`, which is what the inspector should say about
        // a colour nobody named.
        let theme = it.runs.first().map(|(_, t)| t.as_str()).unwrap_or("");
        let (fg, bg) = crate::app::shell_host::shell_theme::names(theme);
        out.push((r.x, r.y, r.width, fg, bg));
    }
    out
}

/// Every element in screen order with its name, text and cells — the bar's
/// semantic model, which the web renders directly instead of scraping cells.
pub fn segments(
    ui: &fresh_ui::Ui<UiMsg>,
    bar: &StatusBar,
    size: ratatui::layout::Rect,
) -> Vec<crate::view::ui::status_bar::StatusSegmentInfo> {
    sides(bar)
        .filter_map(|(side, i, it)| {
            let r = rect_of(ui, &item_key(side, i), size)?;
            Some(crate::view::ui::status_bar::StatusSegmentInfo {
                name: it.name,
                key: it.token_key.clone(),
                text: it.text(),
                x: r.x,
                w: r.width,
                side: side.name(),
            })
        })
        .collect()
}

fn sides(bar: &StatusBar) -> impl Iterator<Item = (Side, usize, &Item)> {
    bar.left
        .iter()
        .enumerate()
        .map(|(i, it)| (Side::Left, i, it))
        .chain(
            bar.right
                .iter()
                .enumerate()
                .map(|(i, it)| (Side::Right, i, it)),
        )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::view::shell::fold::{fold_native, Band};
    use crate::view::shell::frame::{frame_tree, region_key, Frame, HostRegion};
    use fresh_ui::{Size, Ui};
    use ratatui::buffer::Buffer;
    use ratatui::layout::Rect;

    /// **The regression this rule exists for.** A long message must not cost
    /// the right side its place.
    ///
    /// `visual_comprehensive_a` caught it as a snapshot diff: the message
    /// rendered in full and `LSP (off)  Palette: Ctrl+P` was pushed off the
    /// edge, where the old bar truncated the message to `...` instead. The
    /// assertion is the *right side's* survival, not the message's width —
    /// asserting the latter is what let this through, because a bar that
    /// drops its right half also "renders the message correctly".
    #[test]
    fn a_long_message_yields_to_the_right_side() {
        // 100 cells; right side wants 30; a 60-cell message on the left.
        let got = left_budget(&[8, 60], 30, 2, 100);
        let total: usize = got.iter().sum::<usize>() + 2 * got.len().saturating_sub(1);
        assert!(
            total + 30 < 100,
            "left {got:?} (total {total}) must leave room for the right side's 30"
        );
        assert_eq!(got[0], 8, "the short element keeps its width");
        assert!(got[1] < 60, "the message is the one that yields");
    }

    /// The partner: when both sides fit, nobody is truncated.
    #[test]
    fn a_bar_with_room_truncates_nothing() {
        assert_eq!(left_budget(&[8, 12], 30, 2, 100), vec![8, 12]);
    }

    /// An element that cannot fit at all ends the side — the rest are dropped
    /// rather than being squeezed to nothing.
    #[test]
    fn an_element_that_cannot_fit_ends_the_side() {
        let got = left_budget(&[40, 40, 40], 30, 2, 100);
        assert_eq!(got.len(), 2, "the third does not fit: {got:?}");
        assert_eq!(got[0], 40);
        assert!(got[1] < 40, "the second is truncated to the remainder");
    }

    /// Below 15 cells the old bar reserved nothing and gave the left side the
    /// whole row. Kept verbatim: a boundary is behaviour.
    #[test]
    fn a_very_narrow_bar_reserves_nothing() {
        assert_eq!(left_budget(&[10], 30, 2, 14), vec![10]);
        // …and at 15 the reservation switches on.
        assert_eq!(left_budget(&[10], 30, 2, 15), vec![1]);
    }

    fn plain(text: &str, name: &'static str) -> Item {
        Item {
            runs: vec![(text.to_string(), base())],
            name,
            clickable: None,
            token_key: None,
        }
    }

    fn base() -> String {
        crate::app::shell_host::shell_theme::pair("ui.status_bar_fg", "ui.status_bar_bg")
    }

    fn clicky(text: &str, id: StatusBarClickable) -> Item {
        Item {
            clickable: Some(id),
            ..plain(text, "text")
        }
    }

    fn bar_of(left: Vec<Item>, right: Vec<Item>) -> StatusBar {
        StatusBar {
            left,
            right,
            separator: " | ".to_string(),
            base_theme: base(),
            sep_theme: base(),
        }
    }

    fn laid_out(bar: StatusBar, w: u16, h: u16) -> Ui<UiMsg> {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            frame_tree(Frame {
                status_bar: true,
                status_bar_items: Some(bar),
                ..Frame::default()
            }),
            Size::new(w, h),
        );
        ui
    }

    /// **Reconciliation, not first paint.**
    ///
    /// The editor keeps ONE `Ui` for the life of the window and calls `frame`
    /// again every render (`render.rs`: `shell_ui.take()` … `ui.frame(..)` …
    /// put back), so every frame after the first is a *reconcile* against the
    /// previous tree. Every other test in this file — and in the whole shell —
    /// builds `Ui::new()` and frames once, so none of them exercises that.
    ///
    /// A changed message must reach the cells on the second frame. It did not,
    /// **whenever a layer was present in the tree** — open in both frames, or
    /// open in the first and gone in the second, either way the status bar
    /// kept painting the first frame's row. Without a layer it passed, which
    /// is why nothing caught it.
    ///
    /// That was the shape of every remaining e2e failure: a context menu is
    /// open, a click both runs the item and closes the menu, and the assertion
    /// checks the screen changed.
    ///
    /// The cause was in `fresh-ui`'s layout drain, not in this crate:
    /// `drain_layout` gave up on the rest of its dirty list the moment one
    /// boundary had no cached constraints to re-enter on. Reconciliation was
    /// fine all along — `update_render` pushed the new runs into the text
    /// object — but `TextRender` shapes its rows at *measure* time and paints
    /// from them, so a boundary that never re-measured painted last frame's
    /// rows. The layer is what made the difference: it dirties the root, the
    /// root sorts first and has no cache, and the status bar's boundary was
    /// dropped behind it. See `fresh-ui/src/render/layout.rs`.
    #[test]
    fn a_second_frame_repaints_a_changed_message() {
        let mk = |msg: &str| {
            bar_of(
                vec![plain(" Trusted ", "trusted"), plain(msg, "message")],
                Vec::new(),
            )
        };
        let (w, h) = (60u16, 4u16);
        let mut ui: Ui<UiMsg> = Ui::new();
        // The real transition: a context menu is OPEN when the item is
        // clicked, and the click both closes it and changes the message. So
        // frame 1 carries the layer and frame 2 does not.
        let frame_of = |bar: StatusBar, menu: bool| {
            frame_tree(Frame {
                status_bar: true,
                status_bar_items: Some(bar),
                menu: menu.then(|| crate::view::shell::context_menu::Menu {
                    x: 4,
                    y: 1,
                    width: 20,
                    highlighted: 0,
                    items: vec!["Copy".into(), "Paste".into()],
                }),
                ..Frame::default()
            })
        };
        let palette = |k: &fresh_ui::ThemeKey| super::super::fold::test_palette::of(k.as_str());
        // **frame → fold → frame → fold**, which is the editor's real cycle
        // (`render.rs` folds `ui.spec()` every draw). Framing twice and
        // folding once does not exercise it.
        ui.frame(frame_of(mk(" Opened rel.txt "), true), Size::new(w, h));
        let mut buf = Buffer::empty(Rect::new(0, 0, w, h));
        fold_native(ui.spec(), &mut buf, &palette, Band::Background);

        ui.frame(
            frame_of(mk(" Copied path: rel.txt "), false),
            Size::new(w, h),
        );
        let mut buf = Buffer::empty(Rect::new(0, 0, w, h));
        fold_native(ui.spec(), &mut buf, &palette, Band::Background);
        let y = {
            let e = ui
                .find_by_key(&region_key(HostRegion::StatusBar))
                .expect("the bar");
            ui.rect_of(e).y as u16
        };
        let row: String = (0..w).map(|x| buf[(x, y)].symbol().to_string()).collect();
        assert!(
            row.contains("Copied path: rel.txt"),
            "the second frame's message must reach the cells, got {row:?}"
        );
    }

    /// **A press alone activates a segment — no release needed.**
    ///
    /// The web frontend forwards a chrome click as a synthetic mouse-*down* at
    /// the segment's cell and never sends the matching up (`web-ui/js` —
    /// every chrome surface does this, and the document-level `mouseup`
    /// handler skips chrome). The old `chrome::StatusBar::on_pointer` fired on
    /// `PointerPress::Left`, so that worked; a `GestureKind::Click` handler
    /// needs the release and silently did nothing, which took out the browser's
    /// Remote / LSP / read-only menus while every terminal test still passed.
    #[test]
    fn a_press_with_no_release_activates_a_segment() {
        use fresh_ui::{Input, Mods, MouseButton, Point};
        let bar = bar_of(
            vec![clicky(" Remote ", StatusBarClickable::RemoteIndicator)],
            Vec::new(),
        );
        let mut ui = laid_out(bar, 40, 3);
        let r = ui.rect_of(
            ui.find_by_key(&item_key(Side::Left, 0))
                .expect("the segment"),
        );
        let got = ui.dispatch(Input::press(
            Point::new(r.x + 1, r.y),
            MouseButton::Left,
            Mods::NONE,
        ));
        assert!(
            got.msgs.iter().any(|m| matches!(
                m,
                UiMsg::Ui(UiFact::StatusBarClicked(
                    StatusBarClickable::RemoteIndicator
                ))
            )),
            "a press alone must activate, got {:?}",
            got.msgs
        );
    }

    fn row_text(bar: StatusBar, w: u16, h: u16) -> String {
        let ui = laid_out(bar, w, h);
        let spec = ui.spec().clone();
        let mut buf = Buffer::empty(Rect::new(0, 0, w, h));
        let palette = |k: &fresh_ui::ThemeKey| super::super::fold::test_palette::of(k.as_str());
        fold_native(&spec, &mut buf, &palette, Band::Background);
        let y = {
            let e = ui
                .find_by_key(&region_key(HostRegion::StatusBar))
                .expect("the bar");
            ui.rect_of(e).y as u16
        };
        (0..w).map(|x| buf[(x, y)].symbol().to_string()).collect()
    }

    /// Left elements from the left edge, right elements against the right one,
    /// separators between neighbours on the same side and nowhere else — the
    /// cells `render_status`'s `used_left` cursor and backwards-measured right
    /// side produced, now from one flexible gap.
    #[test]
    fn the_two_sides_tile_from_their_own_edges() {
        let got = row_text(
            bar_of(
                vec![plain("main.rs", "text"), plain("1:1", "text")],
                vec![plain("UTF-8", "encoding"), plain("LF", "lineEnding")],
            ),
            40,
            3,
        );
        assert_eq!(got, "main.rs | 1:1                 UTF-8 | LF");
    }

    /// A separator sits *between* elements, so one element on a side has none
    /// — and an empty separator consumes no width at all, which layout gets
    /// for free because an empty run measures zero.
    #[test]
    fn a_separator_goes_between_and_an_empty_one_is_free() {
        let one = row_text(bar_of(vec![plain("only", "text")], vec![]), 12, 3);
        assert_eq!(one, "only        ", "no leading or trailing separator");

        let mut bar = bar_of(vec![plain("a", "text"), plain("b", "text")], vec![]);
        bar.separator = String::new();
        assert_eq!(row_text(bar, 6, 3), "ab    ");
    }

    /// The gap closes before anything is dropped: the sides meet in the middle
    /// and the elements themselves are what layout clamps.
    #[test]
    fn a_narrow_bar_closes_the_gap_first() {
        let bar = bar_of(
            vec![plain("main.rs", "text")],
            vec![plain("UTF-8", "encoding")],
        );
        assert_eq!(row_text(bar.clone(), 13, 3), "main.rs UTF-8");
        assert_eq!(row_text(bar, 12, 3), "main.rsUTF-8");
    }

    /// Every element's rectangle is read back from the tree, which is what a
    /// hover, a click, a popup anchor and the web projection all use. The old
    /// `StatusBarLayout` spelled these out during paint and then spelled them
    /// again at event time.
    #[test]
    fn the_rectangles_come_from_layout() {
        let bar = bar_of(
            vec![plain("main.rs", "text")],
            vec![
                clicky("UTF-8", StatusBarClickable::Encoding),
                clicky("LF", StatusBarClickable::LineEnding),
            ],
        );
        let ui = laid_out(bar.clone(), 40, 3);
        let size = Rect::new(0, 0, 40, 3);

        let clicks = clickable_rects(&ui, &bar, size);
        let ids: Vec<_> = clicks.iter().map(|(id, _)| *id).collect();
        assert_eq!(
            ids,
            vec![StatusBarClickable::Encoding, StatusBarClickable::LineEnding],
            "in render order, and only the elements that answer a press"
        );
        // "UTF-8 | LF" ends at the right edge: LF at 38..40, UTF-8 at 30..35.
        assert_eq!((clicks[0].1.x, clicks[0].1.width), (30, 5));
        assert_eq!((clicks[1].1.x, clicks[1].1.width), (38, 2));

        let segs = segments(&ui, &bar, size);
        assert_eq!(segs.len(), 3, "every element, clickable or not");
        assert_eq!(
            (segs[0].text.as_str(), segs[0].x, segs[0].side),
            ("main.rs", 0, "left")
        );
        assert_eq!(
            (segs[2].text.as_str(), segs[2].x, segs[2].side),
            ("LF", 38, "right")
        );
    }

    /// A press on a clickable element names it; a press on one that is not
    /// clickable produces nothing rather than the nearest id.
    #[test]
    fn a_press_names_the_element_under_it() {
        let bar = bar_of(
            vec![plain("main.rs", "text")],
            vec![clicky("UTF-8", StatusBarClickable::Encoding)],
        );
        let mut ui = laid_out(bar.clone(), 40, 3);
        let size = Rect::new(0, 0, 40, 3);
        let at = clickable_rects(&ui, &bar, size)[0].1;

        let got = ui.dispatch(fresh_ui::Input::press(
            fresh_ui::Point::new(at.x as i32, at.y as i32),
            fresh_ui::MouseButton::Left,
            fresh_ui::Mods::default(),
        ));
        let got = {
            let mut msgs = got.msgs;
            msgs.extend(
                ui.dispatch(fresh_ui::Input::release(
                    fresh_ui::Point::new(at.x as i32, at.y as i32),
                    fresh_ui::MouseButton::Left,
                    fresh_ui::Mods::default(),
                ))
                .msgs,
            );
            msgs
        };
        assert!(
            matches!(
                got.as_slice(),
                [UiMsg::Ui(UiFact::StatusBarClicked(
                    StatusBarClickable::Encoding
                ))]
            ),
            "got {got:?}"
        );

        // The filename is not clickable.
        let got = ui.dispatch(fresh_ui::Input::press(
            fresh_ui::Point::new(1, at.y as i32),
            fresh_ui::MouseButton::Left,
            fresh_ui::Mods::default(),
        ));
        assert!(got.msgs.is_empty(), "got {:?}", got.msgs);
    }
}
