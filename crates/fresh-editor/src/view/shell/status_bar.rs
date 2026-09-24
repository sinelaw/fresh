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
//! column. A popup anchors to an element's key, and `segments` reads the
//! result back for the web projection.
//!
//! **What stays app-side, and why it is not geometry.** Which right-hand
//! elements appear at all is a *content* decision the bar makes from measured
//! text: when the right side would crowd the left below its budget, the
//! lowest-priority right elements are dropped, and below [`BOTH_SIDES_MIN`]
//! the last of them may go too. That is the same rule as before and it still
//! lives in the editor — a description that listed elements layout would then
//! silently discard would be lying about what is on the bar. What moved here
//! is where the surviving elements land.
//!
//! **And every width is layout's, including the cut.** `Editor::status_bar_description`
//! used to finish the job the painter had started: it reserved the right side,
//! spent the remainder on the left through `left_budget`, truncated the
//! element that straddled the boundary and dropped the ones past it — over
//! measured text, before any node existed. A description carrying a pre-fitted
//! string is still a picture, which is this project's own stated failure
//! criterion, and the status bar was the surface it was stated about. The
//! reservation is [`yields_last`] (`Node::priority`, whose doc names this bar
//! as the case it was built for) and the cut is `Elide::Tail`, applied at
//! paint to the width layout settled on.

use std::rc::Rc;

use fresh_ui::{gesture, row, text_runs, Elide, Event, GestureKind, Key, Node, Run, Sizing};

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
        .priority(prio)
}

fn hover_msg(t: Option<HoverTarget>) -> fresh_ui::Handler<UiMsg> {
    Rc::new(move |_: &Event| Some(UiMsg::Ui(UiFact::Hover(t.clone()))))
}

/// Who keeps its width when the row runs out, for [`Node::priority`]:
/// **higher yields last.**
///
/// This is the whole of what `left_max_width` computed by hand.
/// `render_status` measured the right side, subtracted it and a spacing cell
/// from the row, and spent the remainder on the left — truncating the element
/// that straddled the boundary and dropping the ones past it. The migration
/// ported that arithmetic into `left_budget` and ran it over measured text
/// *before the description was built*, which is a pre-fitted picture and the
/// project's own stated failure criterion.
///
/// `priority` is the concept that was missing, and this surface is the one its
/// doc names: a row sizes its non-flex children in descending priority against
/// what is left, and still paints them in declaration order. So the right side
/// is sized first — reserved, without anyone measuring backwards from the edge
/// — the spacing cell with it, and the left takes the remainder. An element
/// past the remainder is sized to zero, which is what dropping it looked like.
///
/// The cut itself is layout's too: `Elide::Tail` is applied at paint, to the
/// width layout settled on, and walks fragments whole so a styled element keeps
/// its pieces' colours. The hand-rolled loop that did that — and appended
/// `...` where the tree writes `…` — is gone with the budget.
/// The narrowest bar that hosts both sides.
///
/// `render_status` spelled this as "below 15 cells reserve nothing for the
/// right", which gave the left side the whole row — and `left_budget` kept the
/// boundary verbatim, because a boundary is behaviour. It is not a width
/// budget any more: [`yields_last`] reserves the right side at every width, so
/// what the boundary says now is a decision about *which elements are on the
/// bar*, which is the editor's (`Editor::status_bar_description`) and is made
/// from measured text like the rest of the right-hand drop rule. Below this,
/// a right side that will not fit beside the left is not on the bar at all,
/// rather than being kept and clipped to a few cells of itself.
pub const BOTH_SIDES_MIN: usize = 15;

mod yields_last {
    /// The right side. Sized first, so the right half of the bar survives a
    /// long message on the left — which is the regression the budget existed
    /// for.
    pub const RIGHT: u8 = 1;
    /// The left side: it absorbs the squeeze and elides. Default, stated for
    /// symmetry with the above.
    pub const LEFT: u8 = 0;
}

/// The glyph between two elements on the same side. It yields with the side
/// it belongs to, or a right-hand separator would be sized after the left and
/// come out narrower than the gap it fills.
fn separator(bar: &StatusBar, side: Side) -> Node<UiMsg> {
    text_runs([Run::themed(bar.separator.clone(), bar.sep_theme.clone())])
        .h(Sizing::Cells(1))
        .priority(match side {
            Side::Left => yields_last::LEFT,
            Side::Right => yields_last::RIGHT,
        })
}

/// The bar's row.
///
/// Left elements, then a flexible gap, then right elements. The gap is what
/// puts the right side against the edge, and `yields_last` is what decides who
/// gives way when there is not enough room for both.
///
/// **There is no reserved cell between the sides, and there was one here
/// briefly.** `left_max_width = available - right_width - 1` had a `- 1`, and
/// turning that into a child of its own looked like the faithful reading. It
/// is not: the bar's rule is that *the gap closes before anything is dropped*
/// (`a_narrow_bar_closes_the_gap_first`), so on a width that fits both sides
/// exactly they meet — and a cell that outranked the left side kept a blank
/// column alive by eliding a character out of the filename instead.
///
/// **Memoised on the bar itself.** The status bar is rebuilt on every frame —
/// which, in this editor, means on every terminal tick — and changes on very
/// few of them: a cursor move, a mode change, a new diagnostic count. `Node`
/// identity cannot express that, because the description is derived from the
/// frame's state and so is a new value each time; `memo` compares the state
/// instead. `StatusBar` is `PartialEq` and is the whole of what this function
/// reads, which is the contract `memo` asks for.
pub fn status_bar(bar: &StatusBar) -> Node<UiMsg> {
    fresh_ui::memo(bar.clone(), build)
}

fn build(bar: &StatusBar) -> Node<UiMsg> {
    let mut kids: Vec<Node<UiMsg>> = Vec::new();
    for (i, it) in bar.left.iter().enumerate() {
        if i > 0 {
            kids.push(separator(bar, Side::Left));
        }
        kids.push(element(it, key_of(Side::Left, i, it), Side::Left));
    }
    // The gap, which closes completely on a bar too narrow for both sides.
    kids.push(row().flex(1));
    for (i, it) in bar.right.iter().enumerate() {
        if i > 0 {
            kids.push(separator(bar, Side::Right));
        }
        kids.push(element(it, key_of(Side::Right, i, it), Side::Right));
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
    /// element layout had no room left for — which is what "dropped" looked
    /// like when the editor computed a budget and stopped early.
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

    /// **The regression the budget existed for, now the yield order's.** A
    /// long message must not cost the right side its place.
    ///
    /// `visual_comprehensive_a` caught it as a snapshot diff: the message
    /// rendered in full and `LSP (off)  Palette: Ctrl+P` was pushed off the
    /// edge. The assertion is the *right side's* survival, not the message's
    /// width — asserting the latter is what let it through, because a bar that
    /// drops its right half also "renders the message correctly".
    ///
    /// Asked of the tree rather than of an arithmetic helper: `yields_last`
    /// sizes the right side first and the left elides into the remainder, so
    /// this is now a property of the widths layout settled on.
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

    /// **The narrow-bar boundary, kept but restated.**
    ///
    /// `render_status` reserved nothing for the right below 15 cells, which
    /// gave the left side the row and left the right whatever the left had not
    /// taken — a few cells of a clipped indicator. `left_budget` ported that
    /// verbatim. The reservation is `yields_last` now and applies at every
    /// width, so what the boundary says is which elements are on the bar:
    /// below `BOTH_SIDES_MIN` a right side that will not fit beside the left
    /// is not on it. The left surviving is the part that was behaviour.
    ///
    /// This is the tree's half — a bar with no right side gives the left the
    /// row, spacing cell included. `Editor::status_bar_description` is what
    /// decides the right side is not there; see `keep` beside the drop loop.
    #[test]
    fn a_bar_with_no_right_side_gives_the_left_the_row() {
        let bar = bar_of(vec![wide(10, "left")], Vec::new());
        assert_eq!(fitted(&bar, 14), (vec![10], Vec::new()));
    }

    /// **A clickable right element is reserved like any other.**
    ///
    /// The bug this exists for: a row reads `priority` off its *direct*
    /// children, and a clickable element's direct child is the gesture wrapper
    /// `element` puts around its runs. Set on the runs instead, the priority
    /// is read by nobody, the right side stops being reserved and a long
    /// message walks over the encoding and line-ending indicators — the exact
    /// regression `a_long_message_yields_to_the_right_side` guards, but only
    /// for the inert elements it happens to use.
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
    /// gives way, meeting it exactly.
    ///
    /// **This asserted that the sides never touch, and that was wrong.** It
    /// was written alongside a spacing cell that outranked the left side, on
    /// the reading that the old `left_max_width`'s `- 1` was a column of its
    /// own. The bar's older and stated rule is the opposite — the gap closes
    /// before anything is dropped — and a bar squeezed to exactly both sides
    /// has no column left to spare. What is actually guaranteed is that the
    /// squeeze lands on the left.
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
