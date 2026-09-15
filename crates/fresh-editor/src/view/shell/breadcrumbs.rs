//! A pane's symbol breadcrumb row, as nodes.
//!
//! Each crumb is its own node carrying its own byte offset, so a press is
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
use crate::model::event::LeafId;
use crate::primitives::display_width::str_width;

use super::msg::{UiFact, UiMsg};

/// The separator between two crumbs, and the marker for crumbs dropped off
/// the left. Their widths are layout, so they are named once.
const SEP: &str = " > ";
const CUT: &str = "… > ";

/// The row's ground: the ancestors and the space between them.
///
/// The keys are the ones the painter reached for directly — the row has no
/// colours of its own in the theme, and giving it some is a theme change, not
/// this one.
fn ground() -> String {
    pair("editor.line_number_fg", "ui.tab_separator_bg")
}

/// The innermost crumb — where the caret actually is.
fn current() -> String {
    attrs("ui.tab_inactive_fg", "ui.tab_separator_bg", &["bold"])
}

/// A pane's breadcrumb row.
///
/// Laid out through a reader because which crumbs fit is a function of the
/// width the row is given, and only the layout knows it.
pub fn surface(pane: LeafId, items: &[BreadcrumbItem]) -> Node<UiMsg> {
    let items: Rc<[BreadcrumbItem]> = Rc::from(items.to_vec());
    layout_reader(move |info: LayoutInfo| lay_out(pane, &items, info.constraints.max_w as usize))
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

fn lay_out(pane: LeafId, items: &[BreadcrumbItem], total_w: usize) -> Node<UiMsg> {
    // One column of ground either side, as the painted row had.
    let available = total_w.saturating_sub(1);
    if items.is_empty() || available == 0 {
        return row().theme(ground());
    }

    let first = first_visible(items, available);
    let mut cells: Vec<Node<UiMsg>> = vec![gap(1)];
    let mut remaining = available;

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
        cells.push(crumb(pane, index, item, innermost).w(Sizing::Cells(width as u16)));
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
/// The offset travels in the node, so the fact the editor receives names a
/// symbol rather than a cell — there is nothing left to hit-test.
fn crumb(pane: LeafId, index: usize, item: &BreadcrumbItem, innermost: bool) -> Node<UiMsg> {
    let position = item.position as usize;
    let ink = match innermost {
        true => current(),
        false => ground(),
    };
    gesture(
        text(item.label.replace(['\n', '\r'], " "))
            .elide(Elide::Tail)
            .theme(ink),
    )
    .key(crumb_key(pane, index))
    .on(
        GestureKind::Press,
        Rc::new(move |e: &Event| {
            if e.button != MouseButton::Left {
                return None;
            }
            e.stop();
            Some(UiMsg::Ui(UiFact::PaneBreadcrumbPress { pane, position }))
        }),
    )
}

/// A crumb names itself by its depth in the trail — unique by construction,
/// and unchanged by elision, which decides *which* crumbs are placed but not
/// how they are numbered. Two symbols can share a byte offset; none can share
/// a depth.
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
                position: i as u64 * 10,
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
        ui.frame(surface(pane, &it), Size::new(40, 1));

        let rect = |i: usize| {
            let e = ui
                .find_by_key(&crumb_key(pane, i))
                .expect("a crumb for every item that fit");
            ui.rect_of(e)
        };
        let (outer, inner) = (rect(0), rect(1));

        // One column of ground, then the crumbs in order, separated by " > ".
        assert_eq!((outer.x, outer.w), (1, 5), "Outer");
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
        ui.frame(surface(pane, &it), Size::new(20, 1));

        assert!(ui.find_by_key(&crumb_key(pane, 0)).is_none());
        assert!(
            ui.find_by_key(&crumb_key(pane, it.len() - 1)).is_some(),
            "the innermost crumb is always placed"
        );
    }

    #[test]
    fn width_is_measured_in_columns_not_chars() {
        let wide = items(&["日本語", "x"]);
        // Three CJK glyphs are six columns, not three.
        assert_eq!(first_visible(&wide, 6), 1);
        assert_eq!(first_visible(&wide, 40), 0);
    }
}
