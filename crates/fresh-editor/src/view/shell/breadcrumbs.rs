//! A pane's symbol breadcrumb row, as nodes.
//!
//! Each crumb is its own node carrying its own LSP position, so a press is
//! answered by the node under the pointer rather than by re-deriving the row's
//! layout against a recorded rectangle. The trail is laid out once, by the
//! tree, and that layout is what both the paint and the press read.
//!
//! The row keeps the *innermost* symbols when it runs out of width: the
//! function you are in matters more than the module it lives in, so crumbs are
//! dropped from the left and an `… > ` marks the cut.

use std::rc::Rc;

use fresh_core::api::BreadcrumbItem;
use fresh_ui::{
    gesture, layout_reader, row, text, Elide, Event, GestureKind, Key, LayoutInfo, MouseButton,
    Node, Sizing,
};

use crate::app::shell_host::shell_theme::{attrs, pair};
use crate::app::types::HoverTarget;
use crate::model::event::LeafId;
use crate::primitives::display_width::str_width;

use super::msg::{UiFact, UiMsg};

/// The separator between two crumbs, and the marker for crumbs dropped off
/// the left. Their widths are layout, so they are named once.
const SEP: &str = " > ";
const CUT: &str = "… > ";
/// The root the trail hangs off. The row opens with it whatever the caret is
/// inside, so the trail reads as a path and the row keeps its height while the
/// caret sits between symbols — where there is no trail to name.
const ROOT: &str = ">";
/// The gap between the root and the first crumb.
const ROOT_GAP: &str = " ";

/// The row's ground: the ancestors and the space between them.
fn ground() -> String {
    pair("ui.breadcrumb_fg", "ui.breadcrumb_bg")
}

/// The innermost crumb — where the caret actually is.
fn current(hovered: bool) -> String {
    attrs("ui.breadcrumb_current_fg", bg(hovered), &["bold"])
}

/// An ancestor crumb, which is a link: it lights under the pointer.
fn ancestor(hovered: bool) -> String {
    pair("ui.breadcrumb_fg", bg(hovered))
}

fn bg(hovered: bool) -> &'static str {
    match hovered {
        true => "ui.breadcrumb_hover_bg",
        false => "ui.breadcrumb_bg",
    }
}

/// A pane's breadcrumb row.
///
/// Laid out through a reader because which crumbs fit is a function of the
/// width the row is given, and only the layout knows it.
pub fn surface(pane: LeafId, items: &[BreadcrumbItem], hover: Option<usize>) -> Node<UiMsg> {
    let items: Rc<[BreadcrumbItem]> = Rc::from(items.to_vec());
    layout_reader(move |info: LayoutInfo| {
        lay_out(pane, &items, hover, info.constraints.max_w as usize)
    })
    .h(Sizing::Cells(1))
}

/// The first crumb that still lets the whole trail fit, dropping from the
/// left. Returns `items.len() - 1` at worst: the innermost crumb is always
/// offered, even when it has to be truncated to fit.
fn first_visible(items: &[BreadcrumbItem], available: usize) -> usize {
    let width_from = |start: usize| {
        items[start..]
            .iter()
            .map(|item| str_width(&item.label))
            .sum::<usize>()
            + str_width(SEP) * items.len().saturating_sub(start + 1)
            + if start > 0 { str_width(CUT) } else { 0 }
    };
    let mut first = 0usize;
    while first + 1 < items.len() && width_from(first) > available {
        first += 1;
    }
    first
}

fn lay_out(
    pane: LeafId,
    items: &[BreadcrumbItem],
    hover: Option<usize>,
    total_w: usize,
) -> Node<UiMsg> {
    // One column of ground either side, as the painted row had.
    let available = total_w.saturating_sub(1);
    if available == 0 {
        return row().theme(ground());
    }

    let mut cells: Vec<Node<UiMsg>> = vec![gap(1)];
    cells.push(text(ROOT).theme(ground()).w(cells_of(ROOT)));
    let mut remaining = available - str_width(ROOT);

    // A caret between symbols has no trail, and the root alone is the whole
    // row — the point being that the row is still there.
    if items.is_empty() || remaining <= str_width(ROOT_GAP) {
        cells.push(row().flex(1));
        return row().theme(ground()).children(cells);
    }
    cells.push(text(ROOT_GAP).theme(ground()).w(cells_of(ROOT_GAP)));
    remaining -= str_width(ROOT_GAP);

    let first = first_visible(items, remaining);
    if first > 0 && remaining >= str_width(CUT) {
        cells.push(text(CUT).theme(ground()).w(cells_of(CUT)));
        remaining -= str_width(CUT);
    }

    for (nth, item) in items[first..].iter().enumerate() {
        if nth > 0 {
            if remaining < str_width(SEP) {
                break;
            }
            cells.push(text(SEP).theme(ground()).w(cells_of(SEP)));
            remaining -= str_width(SEP);
        }
        // Leave room for the separators the crumbs after this one still need,
        // or a long ancestor eats the trail's tail.
        let later = items.len().saturating_sub(first + nth + 1);
        let room = remaining.saturating_sub(later * str_width(SEP));
        if room == 0 {
            break;
        }
        let width = str_width(&item.label).min(room);
        if width == 0 {
            break;
        }
        let index = first + nth;
        let innermost = index + 1 == items.len();
        let hovered = hover == Some(index);
        cells.push(crumb(pane, index, item, innermost, hovered).w(Sizing::Cells(width as u16)));
        remaining -= width;
    }

    cells.push(row().flex(1));
    row().theme(ground()).children(cells)
}

fn cells_of(s: &str) -> Sizing {
    Sizing::Cells(str_width(s) as u16)
}

fn gap(n: u16) -> Node<UiMsg> {
    row().w(Sizing::Cells(n)).theme(ground())
}

/// One crumb: the label, and the press that jumps to it.
///
/// The position travels in the node, so the fact the editor receives names a
/// symbol rather than a cell — there is nothing left to hit-test.
fn crumb(
    pane: LeafId,
    index: usize,
    item: &BreadcrumbItem,
    innermost: bool,
    hovered: bool,
) -> Node<UiMsg> {
    let (line, character) = (item.line, item.character);
    let label = item.label.clone();
    let ink = match innermost {
        true => current(hovered),
        false => ancestor(hovered),
    };
    gesture(
        text(item.label.replace(['\n', '\r'], " "))
            .elide(Elide::Tail)
            .theme(ink),
    )
    .key(crumb_key(pane, index))
    .on_enter(hover_fact(Some(HoverTarget::Breadcrumb(pane, index))))
    .on_leave(hover_fact(None))
    .on(
        GestureKind::Press,
        Rc::new(move |e: &Event| {
            if e.button != MouseButton::Left {
                return None;
            }
            e.stop();
            Some(UiMsg::Ui(UiFact::PaneBreadcrumbPress {
                pane,
                line,
                character,
                label: label.clone(),
            }))
        }),
    )
}

fn hover_fact(t: Option<HoverTarget>) -> fresh_ui::Handler<UiMsg> {
    Rc::new(move |_: &Event| Some(UiMsg::Ui(UiFact::Hover(t.clone()))))
}

/// A crumb names itself by its depth in the trail — unique by construction,
/// and unchanged by elision, which decides *which* crumbs are placed but not
/// how they are numbered. Two symbols can share a position; none can share a
/// depth.
pub fn crumb_key(pane: LeafId, index: usize) -> Key {
    Key::from(format!("pane:{}:crumb:{}", pane.0 .0, index))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn items(labels: &[&str]) -> Vec<BreadcrumbItem> {
        labels
            .iter()
            .enumerate()
            .map(|(i, l)| BreadcrumbItem {
                label: (*l).into(),
                line: i as u32,
                character: 0,
            })
            .collect()
    }

    #[test]
    fn a_trail_that_fits_keeps_every_crumb() {
        let it = items(&["Outer", "inner"]);
        assert_eq!(first_visible(&it, 40), 0);
    }

    #[test]
    fn a_trail_too_wide_drops_its_outermost_crumbs() {
        let it = items(&["AVeryLongModuleName", "Klass", "method"]);
        // Room for the tail but not the head: the outermost goes first.
        assert!(first_visible(&it, 20) > 0);
        // The innermost is never dropped, however tight it gets.
        assert_eq!(first_visible(&it, 1), it.len() - 1);
    }

    /// **The tree places the crumbs**, which is the whole point of the move:
    /// a press is answered by the node under it, so where a crumb *is* has to
    /// be something the layout decided and can be read back.
    #[test]
    fn each_crumb_is_placed_by_the_tree_at_its_own_rect() {
        use fresh_ui::{Size, Ui};
        let pane = LeafId(fresh_core::SplitId(0));
        let it = items(&["Outer", "inner"]);
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(surface(pane, &it, None), Size::new(40, 1));

        let rect = |i: usize| {
            let e = ui
                .find_by_key(&crumb_key(pane, i))
                .expect("a crumb for every item that fit");
            ui.rect_of(e)
        };
        let (outer, inner) = (rect(0), rect(1));

        // One column of ground, the root and its gap, then the crumbs in
        // order, separated by " > ".
        assert_eq!((outer.x, outer.w), (3, 5), "Outer");
        assert_eq!(inner.x, outer.x + outer.w as i32 + str_width(SEP) as i32);
        assert_eq!(inner.w, 5, "inner");
        assert_eq!(outer.h, 1);
    }

    /// A crumb elided off the left is not in the tree at all, so nothing can
    /// press it — the old row painted an `…` and still hit-tested behind it.
    #[test]
    fn a_dropped_crumb_has_no_node() {
        use fresh_ui::{Size, Ui};
        let pane = LeafId(fresh_core::SplitId(0));
        let it = items(&["AVeryLongModuleName", "Klass", "method"]);
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(surface(pane, &it, None), Size::new(20, 1));

        assert!(ui.find_by_key(&crumb_key(pane, 0)).is_none());
        assert!(
            ui.find_by_key(&crumb_key(pane, it.len() - 1)).is_some(),
            "the innermost crumb is always placed"
        );
    }

    /// The pointer lights the crumb it is on, and only that crumb's cells.
    ///
    /// Painted rather than inspected: the hover has to survive the fold to be
    /// worth anything, and the columns say which crumb it landed on.
    #[test]
    fn hover_lights_only_the_crumb_under_the_pointer() {
        use crate::view::shell::fold::{fold_native, Band};
        use fresh_ui::{Size, ThemeKey, Ui};
        use ratatui::buffer::Buffer;
        use ratatui::layout::Rect;
        use ratatui::style::{Color, Style};

        // Only the hover background is given a colour, so a lit cell is
        // unambiguous.
        fn ink(k: &ThemeKey) -> Style {
            match k.0.as_deref() {
                Some(name) if name.contains("breadcrumb_hover_bg") => {
                    Style::default().bg(Color::Red)
                }
                _ => Style::default(),
            }
        }

        let pane = LeafId(fresh_core::SplitId(0));
        let it = items(&["Outer", "inner"]);
        let lit = |hover: Option<usize>| {
            let mut ui: Ui<UiMsg> = Ui::new();
            let spec = ui
                .frame(surface(pane, &it, hover), Size::new(40, 1))
                .clone();
            let mut buf = Buffer::empty(Rect::new(0, 0, 40, 1));
            fold_native(&spec, &mut buf, &ink, Band::Background);
            (0..40)
                .filter(|x| buf[(*x, 0)].style().bg == Some(Color::Red))
                .collect::<Vec<u16>>()
        };

        assert!(
            lit(None).is_empty(),
            "nothing lights when nothing is hovered"
        );
        // "Outer" sits at column 3, past the root, and is five wide; " > "
        // then "inner".
        assert_eq!(lit(Some(0)), vec![3, 4, 5, 6, 7], "the outer crumb");
        assert_eq!(lit(Some(1)), vec![11, 12, 13, 14, 15], "the inner crumb");
    }

    /// A caret between symbols has no trail, and the row still has to be
    /// there — a row that came and went as the caret crossed a blank line was
    /// the thing to fix. It draws the root alone.
    #[test]
    fn an_empty_trail_still_draws_a_row() {
        use fresh_ui::{Size, Ui};
        let pane = LeafId(fresh_core::SplitId(0));
        let mut ui: Ui<UiMsg> = Ui::new();
        let spec = ui.frame(surface(pane, &[], None), Size::new(40, 1)).clone();

        use crate::view::shell::fold::{fold_native, Band};
        use fresh_ui::ThemeKey;
        use ratatui::buffer::Buffer;
        use ratatui::layout::Rect;
        use ratatui::style::Style;
        fn ink(_: &ThemeKey) -> Style {
            Style::default()
        }
        let mut buf = Buffer::empty(Rect::new(0, 0, 40, 1));
        fold_native(&spec, &mut buf, &ink, Band::Background);
        let row: String = (0..40).map(|x| buf[(x, 0)].symbol()).collect();
        assert_eq!(row.trim_end(), " >", "the root, and nothing else");
    }

    #[test]
    fn width_is_measured_in_columns_not_chars() {
        let wide = items(&["日本語", "x"]);
        // Three CJK glyphs are six columns, not three.
        assert_eq!(first_visible(&wide, 6), 1);
        assert_eq!(first_visible(&wide, 40), 0);
    }
}
