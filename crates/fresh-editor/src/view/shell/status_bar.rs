//! The status bar as a description.
//!
//! The bar says what is *on* it — each element's runs, their theme keys, and
//! the identity it answers to — and layout decides every column: the right
//! side is reserved by [`yields_last`] and the left elides into the rest. A
//! popup anchors to an element's key, and `segments` reads the result back for
//! the web projection.
//!
//! Which right-hand elements appear at all is decided in
//! `Editor::status_bar_description`, from measured text: a description that
//! listed elements layout then silently discarded would be lying about what
//! is on the bar.

use std::rc::Rc;

use fresh_ui::{gesture, row, text_runs, Elide, Event, GestureKind, Key, Node, Run, Sizing};

use super::rect_of;
use crate::app::types::HoverTarget;
use crate::view::ui::status_bar::StatusBarClickable;

use super::msg::{UiFact, UiMsg};
use crate::app::shell_host::shell_theme::{Ink, Paint};
use ratatui::style::Color;

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
    /// The theme's section glyphs (`chrome.status_bar_section_*`,
    /// `chrome.status_bar_cap_*`). Empty, the default, draws nothing.
    pub sections: SectionGlyphs,
    /// What the bar's grounds resolve to ([`StatusBar::resolve_grounds`]):
    /// two grounds meet as one when their colours do, whatever their names.
    pub grounds: Vec<(Paint, Color)>,
}

impl StatusBar {
    /// Resolve every ground on the bar — its own and each element's — under
    /// `theme`, so neighbours are told apart by colour. Two names for one
    /// colour (`ui.status_outer_bg` set to the bar's own background) are one
    /// ground: a section glyph between them would be drawn in the colour it
    /// sits on, an invisible cell where the separator used to be.
    pub fn resolve_grounds(&mut self, theme: &crate::view::theme::Theme) {
        let mut grounds: Vec<(Paint, Color)> = Vec::new();
        let paints = base_ground(self).into_iter().chain(
            self.left
                .iter()
                .chain(self.right.iter())
                .flat_map(|it| it.runs.iter())
                .filter_map(|(_, theme)| Ink::parse(theme))
                .map(|ink| ink.bg),
        );
        for paint in paints {
            if grounds.iter().any(|(p, _)| *p == paint) {
                continue;
            }
            if let Some(color) = paint.color(theme) {
                grounds.push((paint, color));
            }
        }
        self.grounds = grounds;
    }
}

/// Whether two grounds are one: the same colour once resolved, or — for one
/// the bar has not resolved — the same name.
fn same_ground(bar: &StatusBar, a: &Paint, b: &Paint) -> bool {
    let color = |p: &Paint| bar.grounds.iter().find(|(q, _)| q == p).map(|(_, c)| *c);
    match (color(a), color(b)) {
        (Some(ca), Some(cb)) => ca == cb,
        _ => a == b,
    }
}

/// The glyphs drawn where the bar's background changes — lualine's section
/// separators. Each is drawn in one neighbour's background over the
/// other's, so the two segments read as one shape flowing into the next.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct SectionGlyphs {
    /// Between left items on different grounds, and after the last left
    /// item when its ground is not the bar's (`\u{e0b0}`, `\u{e0b4}`).
    pub left: String,
    /// The mirror on the right side (`\u{e0b2}`, `\u{e0b6}`).
    pub right: String,
    /// Before the first left item, when its ground is not the bar's.
    pub cap_left: String,
    /// After the last right item, when its ground is not the bar's.
    pub cap_right: String,
}

/// The key an inert element or a plugin token is looked up by: its side and
/// index, which is its position on the bar.
pub fn item_key(side: Side, index: usize) -> Key {
    let tag = match side {
        Side::Left => "status_left",
        Side::Right => "status_right",
    };
    Key::Pair(tag.into(), index as u64)
}

/// The key a built-in clickable element is looked up by: its id, so a popup
/// it opens can hang off it without knowing where it sits.
pub fn clickable_key(id: StatusBarClickable) -> Key {
    Key::Pair("status_click".into(), id as u64)
}

/// The key `it`, at `index` on `side`, is built with.
pub fn key_of(side: Side, index: usize, it: &Item) -> Key {
    match it.clickable {
        Some(id) => clickable_key(id),
        None => item_key(side, index),
    }
}

fn element(it: &Item, key: Key, side: Side) -> Node<UiMsg> {
    let runs = text_runs(
        it.runs
            .iter()
            .map(|(t, theme)| Run::themed(t.clone(), theme.clone())),
    )
    .h(Sizing::Cells(1))
    .key(key)
    // The left side is what shrinks, so it is what marks its cut. The right
    // side is sized first and clips rather than eliding, as it always did: a
    // right element too wide for the whole row is not a truncation decision,
    // it is a bar with nothing to show.
    .elide(match side {
        Side::Left => Elide::Tail,
        Side::Right => Elide::None,
    });
    // **On whatever the row's child turns out to be.** A row reads `priority`
    // off its direct children, and a clickable element's is the gesture
    // wrapper — priority set on the runs inside it would be read by nobody and
    // the right side would quietly stop being reserved.
    let prio = match side {
        Side::Left => yields_last::LEFT,
        Side::Right => yields_last::RIGHT,
    };
    // What a press on this element means: a built-in indicator names its id, a
    // plugin token names its registry key, and anything else is inert — still
    // keyed, because the web projection and the theme inspector read every
    // element back, not only the ones that answer a press.
    let fact = match (it.clickable, it.token_key.clone()) {
        (Some(id), _) => UiFact::StatusBarClicked(id),
        (None, Some(key)) => UiFact::StatusBarTokenClicked(key),
        (None, None) => return runs.priority(prio),
    };
    let hover = it.clickable.map(HoverTarget::StatusBarClickable);
    gesture(runs)
        // **Press, not `Click`.** The web frontend synthesises the press
        // alone at the segment's cell, with no release, so a `Click` handler
        // never fires there.
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
        .priority(prio)
}

fn hover_msg(t: Option<HoverTarget>) -> fresh_ui::Handler<UiMsg> {
    Rc::new(move |_: &Event| Some(UiMsg::Ui(UiFact::Hover(t.clone()))))
}

/// The narrowest bar that hosts both sides. Below it, a right side that will
/// not fit beside the left is not on the bar at all (see
/// `Editor::status_bar_description`), rather than being clipped to a few cells
/// of itself; the left side is what survives.
pub const BOTH_SIDES_MIN: usize = 15;

/// Who keeps its width when the row runs out, for [`Node::priority`]:
/// **higher yields last.** A row sizes its non-flex children in descending
/// priority against what is left and still paints them in declaration order,
/// so the right side is sized first and the left takes the remainder, eliding
/// (`Elide::Tail`) where it is cut. An element past the remainder gets no
/// width at all.
mod yields_last {
    /// The right side. Sized first, so the right half of the bar survives a
    /// long message on the left.
    pub const RIGHT: u8 = 1;
    /// The left side: it absorbs the squeeze and elides. Default, stated for
    /// symmetry with the above.
    pub const LEFT: u8 = 0;
}

/// The glyph between two elements on the same side. It yields with the side
/// it belongs to, or a right-hand separator would be sized after the left and
/// come out narrower than the gap it fills.
fn separator(bar: &StatusBar, side: Side) -> Node<UiMsg> {
    glyph(bar.separator.clone(), bar.sep_theme.clone(), side)
}

fn glyph(text: String, theme: String, side: Side) -> Node<UiMsg> {
    text_runs([Run::themed(text, theme)])
        .h(Sizing::Cells(1))
        .priority(match side {
            Side::Left => yields_last::LEFT,
            Side::Right => yields_last::RIGHT,
        })
}

/// The ground an element's edge sits on: its first run's background
/// (`first`), or its last run's.
fn edge_ground(it: &Item, first: bool) -> Option<Paint> {
    let run = if first {
        it.runs.first()
    } else {
        it.runs.last()
    };
    run.and_then(|(_, theme)| Ink::parse(theme))
        .map(|ink| ink.bg)
}

/// A glyph in `fg`'s ground color, over `bg`'s ground.
fn transition(text: &str, fg: Paint, bg: Paint, side: Side) -> Node<UiMsg> {
    glyph(text.to_string(), Ink::new(fg, bg).to_string(), side)
}

/// What goes between two neighbours on one side: a section glyph where their
/// grounds differ and the theme has one, else the plain separator — on the
/// neighbours' ground when they share one that is not the bar's.
fn between(bar: &StatusBar, a: &Item, b: &Item, side: Side) -> Node<UiMsg> {
    let (ga, gb) = (edge_ground(a, false), edge_ground(b, true));
    let section = match side {
        Side::Left => &bar.sections.left,
        Side::Right => &bar.sections.right,
    };
    match (ga, gb) {
        (Some(ga), Some(gb)) if !same_ground(bar, &ga, &gb) && !section.is_empty() => match side {
            // The glyph points into the next segment: the left side's from
            // `a`, the right side's from `b`.
            Side::Left => transition(section, ga, gb, side),
            Side::Right => transition(section, gb, ga, side),
        },
        (Some(ga), _) if !base_ground(bar).is_some_and(|base| same_ground(bar, &ga, &base)) => {
            let sep = Ink::parse(&bar.sep_theme)
                .map(|ink| ink.with_bg(ga).to_string())
                .unwrap_or_else(|| bar.sep_theme.clone());
            glyph(bar.separator.clone(), sep, side)
        }
        _ => separator(bar, side),
    }
}

/// Give the bar lualine's sections: the outermost item on each side wears
/// `ui.status_outer_*` (lualine's a and z), the next one in
/// `ui.status_inner_*` (b and y). Only runs on the bar's own ground are
/// recolored, so an item with colors of its own — a warning count, the LSP
/// indicator — keeps them. Run after the right side has dropped what does not
/// fit, so "outermost" is what is actually shown.
pub fn assign_sections(left: &mut [Item], right: &mut [Item]) {
    let n = right.len();
    for (i, it) in left.iter_mut().enumerate() {
        recolor(it, i);
    }
    for (i, it) in right.iter_mut().enumerate() {
        recolor(it, n - 1 - i);
    }
}

fn recolor(it: &mut Item, depth: usize) {
    let (fg, bg) = match depth {
        0 => ("ui.status_outer_fg", "ui.status_outer_bg"),
        1 => ("ui.status_inner_fg", "ui.status_inner_bg"),
        _ => return,
    };
    for (_, theme) in &mut it.runs {
        let Some(ink) = Ink::parse(theme) else {
            continue;
        };
        if ink.bg != Paint::key("ui.status_bar_bg") {
            continue;
        }
        let ink_fg = if ink.fg == Paint::key("ui.status_bar_fg") {
            Paint::key(fg)
        } else {
            ink.fg.clone()
        };
        *theme = Ink::new(ink_fg, Paint::key(bg))
            .with_attrs(ink.attrs)
            .to_string();
    }
}

/// The bar's own ground.
fn base_ground(bar: &StatusBar) -> Option<Paint> {
    Ink::parse(&bar.base_theme).map(|ink| ink.bg)
}

/// An element's edge meeting the bar's own ground, when the element has a
/// ground of its own and the theme a glyph for it.
fn edge_to_bar(
    bar: &StatusBar,
    it: &Item,
    first: bool,
    text: &str,
    side: Side,
) -> Option<Node<UiMsg>> {
    let ground = edge_ground(it, first)?;
    let base = base_ground(bar)?;
    (!text.is_empty() && !same_ground(bar, &ground, &base))
        .then(|| transition(text, ground, base, side))
}

/// The bar's row.
///
/// Left elements, then a flexible gap, then right elements. The gap is what
/// puts the right side against the edge, and `yields_last` is what decides who
/// gives way when there is not enough room for both.
///
/// There is no reserved cell between the sides: the gap closes before
/// anything is dropped (`a_narrow_bar_closes_the_gap_first`), so on a width
/// that fits both sides exactly they meet.
///
/// **Memoised on the bar itself.** The description is rebuilt every frame and
/// changes on very few of them; `StatusBar` is `PartialEq` and is the whole of
/// what this function reads, which is the contract `memo` asks for.
pub fn status_bar(bar: &StatusBar) -> Node<UiMsg> {
    fresh_ui::memo(bar.clone(), build)
}

fn build(bar: &StatusBar) -> Node<UiMsg> {
    let mut kids: Vec<Node<UiMsg>> = Vec::new();
    let g = &bar.sections;
    if let Some(cap) = bar
        .left
        .first()
        .and_then(|it| edge_to_bar(bar, it, true, &g.cap_left, Side::Left))
    {
        kids.push(cap);
    }
    for (i, it) in bar.left.iter().enumerate() {
        if i > 0 {
            kids.push(between(bar, &bar.left[i - 1], it, Side::Left));
        }
        kids.push(element(it, key_of(Side::Left, i, it), Side::Left));
    }
    if let Some(end) = bar
        .left
        .last()
        .and_then(|it| edge_to_bar(bar, it, false, &g.left, Side::Left))
    {
        kids.push(end);
    }
    // The gap, which closes completely on a bar too narrow for both sides.
    kids.push(row().flex(1));
    if let Some(start) = bar
        .right
        .first()
        .and_then(|it| edge_to_bar(bar, it, true, &g.right, Side::Right))
    {
        kids.push(start);
    }
    for (i, it) in bar.right.iter().enumerate() {
        if i > 0 {
            kids.push(between(bar, &bar.right[i - 1], it, Side::Right));
        }
        kids.push(element(it, key_of(Side::Right, i, it), Side::Right));
    }
    if let Some(cap) = bar
        .right
        .last()
        .and_then(|it| edge_to_bar(bar, it, false, &g.cap_right, Side::Right))
    {
        kids.push(cap);
    }
    // **The row claims its own gaps.** A press between elements means
    // nothing, and letting it through would put the caret in the buffer
    // below.
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

/// Every element with visible text, in screen order, with its name, trimmed
/// text and cells — the bar's semantic model, which the web renders directly
/// instead of scraping cells.
pub fn segments(
    ui: &fresh_ui::Ui<UiMsg>,
    bar: &StatusBar,
    size: ratatui::layout::Rect,
) -> Vec<crate::view::scene::StatusSegment> {
    sides(bar)
        .filter_map(|(side, i, it)| {
            let text = it.text().trim().to_string();
            if text.is_empty() {
                return None;
            }
            let r = rect_of(ui, &key_of(side, i, it), size)?;
            Some(crate::view::scene::StatusSegment {
                name: it.name,
                key: it.token_key.clone(),
                text,
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

    /// An element `n` cells wide, so a width is stated once per test rather
    /// than counted out of a string literal.
    fn wide(n: usize, name: &'static str) -> Item {
        plain(&"x".repeat(n), name)
    }

    /// What each side's elements were actually given, in order. Zero for an
    /// element layout had no room left for.
    fn fitted(bar: &StatusBar, w: u16) -> (Vec<u16>, Vec<u16>) {
        let ui = laid_out(bar.clone(), w, 4);
        let size = Rect::new(0, 0, w, 4);
        let side = |s: Side, items: &[Item]| -> Vec<u16> {
            items
                .iter()
                .enumerate()
                .map(|(i, it)| {
                    rect_of(&ui, &key_of(s, i, it), size)
                        .map(|r| r.width)
                        .unwrap_or(0)
                })
                .collect()
        };
        (side(Side::Left, &bar.left), side(Side::Right, &bar.right))
    }

    /// A long message must not cost the right side its place. The assertion is
    /// the *right side's* survival, not the message's width: a bar that drops
    /// its right half also "renders the message correctly".
    #[test]
    fn a_long_message_yields_to_the_right_side() {
        // 100 cells; right side wants 30; a 60-cell message on the left.
        let bar = bar_of(
            vec![wide(8, "short"), wide(60, "message")],
            vec![wide(30, "right")],
        );
        let (left, right) = fitted(&bar, 100);
        assert_eq!(right, vec![30], "the right side keeps its width");
        assert_eq!(left[0], 8, "the short element keeps its width");
        assert!(left[1] < 60, "the message is the one that yields: {left:?}");
    }

    /// The partner: when both sides fit, nobody is cut.
    #[test]
    fn a_bar_with_room_truncates_nothing() {
        let bar = bar_of(
            vec![wide(8, "short"), wide(12, "other")],
            vec![wide(30, "right")],
        );
        assert_eq!(fitted(&bar, 100), (vec![8, 12], vec![30]));
    }

    /// An element with no room left gets none — the side ends there rather
    /// than every element being squeezed a little.
    #[test]
    fn an_element_that_cannot_fit_gets_nothing() {
        let bar = bar_of(
            vec![wide(40, "a"), wide(40, "b"), wide(40, "c")],
            vec![wide(30, "right")],
        );
        let (left, right) = fitted(&bar, 100);
        assert_eq!(right, vec![30], "the right side is still reserved");
        assert_eq!(left[0], 40, "the first element is untouched");
        assert!(left[1] < 40, "the second takes the remainder: {left:?}");
        assert_eq!(left[2], 0, "the third has nothing left: {left:?}");
    }

    /// A bar with no right side gives the left the whole row. This is the
    /// tree's half of the narrow-bar rule; `Editor::status_bar_description`
    /// decides, below `BOTH_SIDES_MIN`, that the right side is not there.
    #[test]
    fn a_bar_with_no_right_side_gives_the_left_the_row() {
        let bar = bar_of(vec![wide(10, "left")], Vec::new());
        assert_eq!(fitted(&bar, 14), (vec![10], Vec::new()));
    }

    /// **A clickable right element is reserved like any other.**
    ///
    /// A row reads `priority` off its *direct* children, and a clickable
    /// element's direct child is the gesture wrapper `element` puts around its
    /// runs. Set on the runs instead, the priority is read by nobody and a long
    /// message walks over the encoding and line-ending indicators.
    #[test]
    fn a_clickable_right_element_is_reserved_too() {
        let bar = bar_of(
            vec![wide(120, "message")],
            vec![clicky(&"x".repeat(20), StatusBarClickable::Encoding)],
        );
        let (left, right) = fitted(&bar, 100);
        assert_eq!(right, vec![20], "the clickable element keeps its width");
        assert_eq!(left, vec![80], "and the message takes exactly the rest");
    }

    /// And the reservation itself: the right side keeps its width and the left
    /// gives way, meeting it exactly — the gap closes before anything is
    /// dropped, so there is no spare column between them.
    #[test]
    fn the_left_gives_way_and_the_right_keeps_its_width() {
        let bar = bar_of(vec![wide(60, "message")], vec![wide(30, "right")]);
        let w = 50u16;
        let ui = laid_out(bar.clone(), w, 4);
        let size = Rect::new(0, 0, w, 4);
        let l = rect_of(&ui, &item_key(Side::Left, 0), size).expect("the left element");
        let r = rect_of(&ui, &item_key(Side::Right, 0), size).expect("the right element");
        assert_eq!(
            r.width, 30,
            "the right side keeps every column it asked for"
        );
        assert_eq!(r.x + r.width, w, "and sits against the edge");
        assert_eq!(
            l.x + l.width,
            r.x,
            "the left takes the rest and stops where the right begins"
        );
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
            sections: SectionGlyphs::default(),
            grounds: Vec::new(),
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

    /// **Reconciliation, not first paint.** The editor keeps one `Ui` and
    /// frames it every render, so every frame after the first reconciles. A
    /// changed message must reach the cells on the second frame, including
    /// when a layer (here a context menu) is open in the first: `fresh-ui`'s
    /// layout drain once dropped the status bar's boundary behind the root
    /// the layer dirtied, and the bar kept painting the first frame's row.
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

    /// **A press alone activates a segment — no release needed.** The web
    /// frontend forwards a chrome click as a mouse-down only; a `Click`
    /// handler needs the release and never fired in the browser.
    #[test]
    fn a_press_with_no_release_activates_a_segment() {
        use fresh_ui::{Input, Mods, MouseButton, Point};
        let bar = bar_of(
            vec![clicky(" Remote ", StatusBarClickable::RemoteIndicator)],
            Vec::new(),
        );
        let mut ui = laid_out(bar, 40, 3);
        let r = ui.rect_of(
            ui.find_by_key(&clickable_key(StatusBarClickable::RemoteIndicator))
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
    /// separators between neighbours on the same side and nowhere else.
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

    /// Every element's rectangle is read back from the tree: a clickable one
    /// by its id, which is what a popup anchors to, and every one through
    /// `segments` for the web projection.
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

        let at = |id| rect_of(&ui, &clickable_key(id), size).expect("keyed by its id");
        // "UTF-8 | LF" ends at the right edge: LF at 38..40, UTF-8 at 30..35.
        let (enc, le) = (
            at(StatusBarClickable::Encoding),
            at(StatusBarClickable::LineEnding),
        );
        assert_eq!((enc.x, enc.width), (30, 5));
        assert_eq!((le.x, le.width), (38, 2));

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
        let at = rect_of(&ui, &clickable_key(StatusBarClickable::Encoding), size).unwrap();

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
