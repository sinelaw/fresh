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
        .on(
            GestureKind::Click,
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
    row()
        .theme(bar.base_theme.clone())
        .h(Sizing::Cells(1))
        .children(kids)
}

// ── reading the laid-out bar back ──────────────────────────────────────────

fn rect_of(
    ui: &fresh_ui::Ui<UiMsg>,
    key: &Key,
    size: ratatui::layout::Rect,
) -> Option<ratatui::layout::Rect> {
    let e = ui.find_by_key(key)?;
    let r = ui.rect_of(e);
    (r.w > 0 && r.h > 0).then(|| ratatui::layout::Rect {
        x: size.x.saturating_add(r.x.max(0) as u16),
        y: size.y.saturating_add(r.y.max(0) as u16),
        width: r.w,
        height: r.h,
    })
}

/// Every clickable element's screen rectangle, in render order.
///
/// This is `StatusBarLayout::clickable` — but read from the tree that painted
/// rather than recomputed by a second walk over state that has moved on.
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

/// Plugin token areas as `(row, start_col, end_col)`, keyed by registry key.
pub fn plugin_token_areas(
    ui: &fresh_ui::Ui<UiMsg>,
    bar: &StatusBar,
    size: ratatui::layout::Rect,
) -> std::collections::HashMap<String, (u16, u16, u16)> {
    sides(bar)
        .filter_map(|(side, i, it)| {
            let key = it.token_key.clone()?;
            let r = rect_of(ui, &item_key(side, i), size)?;
            Some((key, (r.y, r.x, r.x.saturating_add(r.width))))
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
        out.push((
            r.x,
            r.y,
            r.width,
            fg.map(str::to_string),
            bg.map(str::to_string),
        ));
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
