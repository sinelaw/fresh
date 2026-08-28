//! Popups, as descriptions.
//!
//! See `docs/internal/fresh-ui-parity-ledger-popups.md` for the full rule
//! inventory. This module starts where the surface is most obviously a
//! restatement: **placement**.
//!
//! `Popup::calculate_area` is six strategies, each of which ends by clamping to
//! the area's absolute edges:
//!
//! ```text
//!   let right  = area.x + area.width;
//!   let x      = if cursor_x + width > right { right - width } else { cursor_x }
//!                   .max(area.x);
//! ```
//!
//! written out six times, plus — for `BelowCursor` — thirteen lines of "not
//! enough space below, put above". Those are `Fit::CLAMP` and `Fit::FLIP`, and
//! the strategies themselves are `Anchor` plus `Place`:
//!
//! ```text
//!   AtCursor              -> Anchor::Point(caret)  + Place::Over
//!   BelowCursor           -> Anchor::Point(caret)  + Place::Below + FLIP
//!   AboveCursor           -> Anchor::Point(caret)  + Place::Above
//!   Fixed { x, y }        -> Anchor::Point(x, y)   + Place::Over
//!   Centered              -> Anchor::Screen(Center)
//!   CenteredOverlay       -> Anchor::Screen(Center) + Sizing::Pct
//!   BottomRight           -> Anchor::Screen(End)
//!   AboveStatusBarAt      -> Anchor::Node(status segment) + Place::Above
//! ```
//!
//! The last one is the ledger's finding A: it looks like it needs an anchor
//! that is a node on one axis and a point on the other, and it does not. The
//! status bar is migrated and its elements are keyed, so the popup hangs off
//! *the segment that opened it* — which is what the feature means. Its `x` and
//! `status_row` parameters exist only because a popup could not name a node.

use std::rc::Rc;

use fresh_ui::widgets::RowState;
use fresh_ui::{col, row, Align, Anchor, Elide, Fit, Key, Node, Place, Run, Sizing};

use crate::app::shell_host::shell_theme::{pair, Attrs, Ink, Paint};
use crate::view::popup::{PopupContent, PopupListItem, PopupPosition};

use super::msg::{UiFact, UiMsg};

/// The caret's screen position, when the frame has one.
///
/// `calculate_area` falls back to the middle of the area when it does not,
/// which is a rule about the *anchor* rather than about the popup, so it is
/// stated here rather than inside each strategy.
pub type Caret = Option<(u16, u16)>;

/// Which point in the buffer a cursor-anchored popup hangs off.
///
/// Two, not one: a completion list lines up with the start of the word being
/// completed, so its column is the word's and its row is the caret's, while
/// everything else uses the caret itself. `render_buffer_popups` chose between
/// them with an `if` on the popup's kind and then passed a pair of numbers;
/// naming the two points instead is what lets the description say *which* one
/// without knowing where either is.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum CaretAnchor {
    /// The text caret.
    Caret,
    /// The start of the word a completion popup is completing.
    CompletionWord,
}

impl CaretAnchor {
    /// What a popup of this kind hangs off.
    pub fn for_kind(kind: crate::view::popup::PopupKind) -> CaretAnchor {
        match kind {
            crate::view::popup::PopupKind::Completion => CaretAnchor::CompletionWord,
            _ => CaretAnchor::Caret,
        }
    }

    /// The key the tree knows it by, and the host publishes against.
    pub fn key(self) -> Key {
        Key::Pair(
            "caret".into(),
            match self {
                CaretAnchor::Caret => 0,
                CaretAnchor::CompletionWord => 1,
            },
        )
    }
}

/// Where a popup goes, as a layer's anchor and placement.
///
/// Returns the layer with its geometry set and no child; the caller adds the
/// content. Split out because placement is the half of this surface that is
/// pure restatement, and it is worth being able to test it against
/// `calculate_area` on its own.
pub fn placed(position: &PopupPosition, at: CaretAnchor) -> Node<UiMsg> {
    let l = fresh_ui::layer();
    // Both cursor-relative families and `Fixed` clamp; the screen-anchored ones
    // cannot fall outside a frame they are measured against.
    //
    // **The caret is named, not measured.** It lives inside the buffer's host
    // leaf, so the tree cannot find it and the description must not carry its
    // coordinates — the buffer publishes a rectangle for this key once it knows
    // (`Ui::set_host_anchor`), and the layer resolves against it. The published
    // rectangle is one cell, which is the whole of the old `Anchor::Cell`
    // distinction: below the caret is the row *after* it, not its own row.
    let caret_cell = || Anchor::Node(at.key());
    let caret_at = caret_cell;
    match position {
        PopupPosition::AtCursor => l.anchor(caret_at()).place(Place::Over).fit(Fit::CLAMP),
        // "Not enough space below, put above" — thirteen lines that `Fit::FLIP`
        // is the name of, against the caret's cell.
        PopupPosition::BelowCursor => l
            .anchor(caret_cell())
            .place(Place::Below)
            .fit(Fit::FLIP.or(Fit::CLAMP)),
        PopupPosition::AboveCursor => l.anchor(caret_cell()).place(Place::Above).fit(Fit::CLAMP),
        PopupPosition::Fixed { x, y } => l
            .anchor(Anchor::Point(*x, *y))
            .place(Place::Over)
            .fit(Fit::CLAMP),
        PopupPosition::Centered | PopupPosition::CenteredOverlay { .. } => {
            l.anchor(Anchor::Screen(Align::Center)).place(Place::Over)
        }
        // Above the status bar, flush with its right edge. The painter's `y`
        // is `height - popup_height - 2`, and the 2 is "leave room for the
        // status bar" — a guess at the bar's position rather than a reference
        // to it, and wrong by a row whenever the prompt line's visibility
        // moves the bar. Naming the bar is both simpler and correct; the
        // right edge is `align_to_anchor(End)`.
        PopupPosition::BottomRight => l
            .anchor(Anchor::Node(super::frame::region_key(
                super::frame::HostRegion::StatusBar,
            )))
            .place(Place::Above)
            .align_to_anchor(Align::End)
            .fit(Fit::CLAMP),
        // The segment that opened it. See the module docs and the ledger's
        // finding A; the two numbers in this variant are a rectangle the caller
        // already had and threw away.
        //
        // **Confined to the area left of the editor's scrollbar.** Clamping to
        // the frame puts this popup's right border on the scrollbar's column —
        // `calculate_area` reserved it with a `saturating_sub(1)` and a comment
        // saying why. Naming the region says the same thing without the
        // arithmetic, and it is the only strategy that reserves anything, so it
        // is the only one that names it.
        PopupPosition::AboveStatusBarAt { .. } => l
            .anchor(Anchor::Node(super::status_bar::item_key(
                super::status_bar::Side::Right,
                0,
            )))
            .place(Place::Above)
            .within(clear_of_scrollbar_key())
            .fit(Fit::FLIP.or(Fit::CLAMP)),
    }
}

/// One popup's geometry, as the shell states it.
///
/// **The size is the popup's own answer, not the tree's — for now.** Placement
/// and measurement are two different migrations and this is the first of them:
/// `calculate_area`'s six strategies become an `Anchor` and a `Place`, while
/// `content_height` keeps computing how tall the box is. Measuring in the tree
/// means the content nodes have to be *in* the tree, which is the step where
/// the painter stops painting — and doing both at once would leave a
/// disagreement with nowhere to localise it.
#[derive(Clone, Debug, PartialEq)]
pub struct Placed {
    pub position: PopupPosition,
    /// Which buffer point the cursor-anchored strategies hang off. The
    /// screen-anchored ones never look at it.
    pub at: CaretAnchor,
    /// What the popup asks to occupy, in cells: `(width, height)`.
    pub size: (u16, u16),
    /// What it shows.
    pub body: Body,
    /// Whether a pointer landing outside it dismisses it — a hover popup or
    /// signature help, as against an action popup that waits to be answered.
    pub transient: bool,
}

/// A popup's content, as the shell describes it.
#[derive(Clone, Debug, PartialEq)]
pub struct Body {
    /// Drawn into the top border. `render_title` already resolved it.
    pub title: Option<String>,
    /// A line of muted text above the content, and a blank one after it.
    pub description: Option<String>,
    pub content: PopupContent,
    pub bordered: bool,
    /// Whether the `[×]` shows. The workspace-trust prompt is a forced choice
    /// with no dismiss, so it has none.
    pub dismissible: bool,
    /// The hint appended to the selected row of a list.
    pub selected_hint: Option<String>,
}

/// The region a popup may occupy when it must leave the editor's vertical
/// scrollbar alone: the frame, less its rightmost column.
///
/// Published by the editor rather than found in the tree, because the split's
/// scrollbar is not in the tree yet — it is painted by `render_content`, which
/// is the last thing to migrate. When the split grid becomes a subtree the
/// region is an ordinary element and this key finds it instead, with nothing
/// here changing.
pub fn clear_of_scrollbar_key() -> Key {
    Key::Str("popup_area_clear_of_scrollbar".into())
}

/// The key a placed popup carries, by its index in paint order.
pub fn popup_key(i: usize) -> Key {
    Key::Pair("popup".into(), i as u64)
}

/// Each popup as a layer that occupies its rectangle and paints nothing.
///
/// The overlay prompt's card taught this the hard way: a layer is in the
/// overlay band, so anything it draws lands *on top of* the painter that still
/// owns the surface and erases it. Until the content moves, the layer's whole
/// job is to have a rectangle the painter can be told about.
pub fn placed_layers(ps: &[Placed]) -> Vec<Node<UiMsg>> {
    ps.iter()
        .enumerate()
        .map(|(i, p)| {
            let mut l = placed(&p.position, p.at).key(popup_key(i));
            if p.transient {
                // **Dismissed by a press outside, and the press goes on.**
                // The guards this replaces returned `PassAfter` for exactly
                // this reason: clicking into the buffer while a tooltip is up
                // hides it *and* moves the caret. Spending the press would
                // charge the user a click to get rid of a tooltip.
                l = l
                    .dismiss(fresh_ui::Dismiss::OUTSIDE_POINTER.passing_through())
                    .on_dismiss(|_| UiMsg::Ui(UiFact::PopupDismissTransient));
            }
            l.child(
                body(&p.body)
                    .w(Sizing::Cells(p.size.0))
                    .h(Sizing::Cells(p.size.1)),
            )
        })
        .collect()
}

/// A popup: its ring, its ground, the strip on its top border, and its content.
///
/// The strip is the library's own answer to a title on a border — "an overlay
/// strip carrying a title", transparent to the pointer except where the close
/// button is. `Block::title` put the text *in* the ring and a second
/// `Paragraph` put `[×]` over it three cells from the right; both are one row
/// stacked over the frame, and the row says where each sits instead of two
/// widgets each computing an `x`.
pub fn body(b: &Body) -> Node<UiMsg> {
    let inner = col().children([
        match &b.description {
            Some(d) => description(d),
            None => col().h(Sizing::Cells(0)),
        },
        content(&b.content, b.selected_hint.as_deref()).flex(1),
    ]);
    // Absorbing *inside* the frame, not around the whole thing: the stacked
    // paths are tried in order and the first that claims ends it, so an absorb
    // on the outside would catch the title strip's own path — which does not
    // reach the content — before the content's path was ever offered.
    let framed = absorb(frame(b.bordered, inner));
    // Only a bordered popup has a border to write on.
    match b.bordered {
        false => framed,
        true => fresh_ui::stack().children([framed, border_strip(b)]),
    }
}

/// A press inside the popup that nothing else claimed dies here.
///
/// The rows, the close button and the text claim their own; this is the
/// padding, the border and the description — everywhere a press means nothing,
/// and where letting it through would put the caret in the buffer underneath or
/// word-select through it on a double click. `chrome:popups` was a rectangle
/// carrying `pointer_opaque` for the same purpose, which is the tree property
/// this is.
fn absorb(n: Node<UiMsg>) -> Node<UiMsg> {
    fresh_ui::gesture(n).on(
        fresh_ui::GestureKind::Press,
        Rc::new(|e: &fresh_ui::Event| {
            e.stop();
            None
        }),
    )
}

/// The top border's overlay: the title where `Block::title` put it, and the
/// close button where the painter's `area.width - 4` put it.
fn border_strip(b: &Body) -> Node<UiMsg> {
    let ring = pair("ui.popup_border_fg", "ui.popup_bg");
    // Decoration, every cell of it but the close button — and each cell has to
    // say so itself: the hit walk stops at the first child that blocks, so one
    // opaque title glyph hides the whole frame behind the strip and a click on
    // the title falls past the popup into the buffer.
    let decoration = |n: Node<UiMsg>| n.pointer_mode(fresh_ui::PointerMode::Transparent);
    let mut cells: Vec<Node<UiMsg>> = vec![
        // `Block::title` starts one cell in from the corner.
        decoration(row().w(Sizing::Cells(1))),
        decoration(match &b.title {
            Some(t) => fresh_ui::text(t.clone()).theme(ring.clone()),
            None => row().w(Sizing::Cells(0)),
        }),
        decoration(row().flex(1)),
    ];
    if b.dismissible {
        cells.push(
            fresh_ui::gesture(fresh_ui::text("[×]").theme(ring.clone()))
                .key(CLOSE_KEY.with(|k| k.clone()))
                .on(
                    fresh_ui::GestureKind::Press,
                    Rc::new(|ev: &fresh_ui::Event| {
                        if ev.button != fresh_ui::MouseButton::Left {
                            return None;
                        }
                        ev.stop();
                        Some(UiMsg::Action(
                            crate::input::keybindings::Action::PopupCancel,
                        ))
                    }),
                ),
        );
        // The painter left the last column clear.
        cells.push(decoration(row().w(Sizing::Cells(1))));
    }
    // **Transparent all the way down, container included.** The strip lies
    // over the whole popup so its one row can sit on the top border, and the
    // hit walk stops at the first child that blocks — so an opaque container
    // here hides the content behind it entirely: no wheel, no row clicks, no
    // scrollbar. Every node of the strip but the close button is decoration.
    col()
        .pointer_mode(fresh_ui::PointerMode::Transparent)
        .children([
            row()
                .h(Sizing::Cells(1))
                .pointer_mode(fresh_ui::PointerMode::Transparent)
                .children(cells),
            row()
                .flex(1)
                // Inert, not transparent: below the title row the strip is not
                // there at all, so a press over the content produces one path
                // rather than two and the content is plainly what answers it.
                .pointer_mode(fresh_ui::PointerMode::Ignore),
        ])
}

thread_local! {
    static CLOSE_KEY: Key = Key::Str("popup_close".into());
}

/// Where the tree put each popup, in the order they were declared.
///
/// The partner of [`super::frame::regions_of`]. A popup whose layer is missing
/// reports an empty rectangle rather than being absent, so the caller indexes
/// by the same position it built.
pub fn rects_of(ui: &fresh_ui::Ui<UiMsg>, n: usize) -> Vec<ratatui::layout::Rect> {
    (0..n)
        .map(|i| {
            let r = ui
                .find_by_key(&popup_key(i))
                .map(|id| ui.rect_of(id))
                .unwrap_or_default();
            ratatui::layout::Rect {
                x: r.x.max(0) as u16,
                y: r.y.max(0) as u16,
                width: r.w,
                height: r.h,
            }
        })
        .collect()
}

/// A popup's frame: its ring and its ground.
pub fn frame(bordered: bool, body: Node<UiMsg>) -> Node<UiMsg> {
    let n = col()
        .theme(pair("ui.popup_border_fg", "ui.popup_bg"))
        .child(body);
    if bordered {
        n.border()
    } else {
        n
    }
}

/// The row of ink a popup's `description` occupies, plus the blank one after
/// it.
///
/// The painter word-wrapped it to `inner.width - 2`, drew each line into a
/// one-row rect of its own, and then adjusted a running `content_start_y` by
/// the line count — arithmetic whose only job was to put the content after it.
/// A `col()` puts the content after it.
fn description(text: &str) -> Node<UiMsg> {
    col().children([
        // The `- 2` the painter wrapped to was padding it then had to leave
        // room for by hand; `pad` states it and the wrap follows the width it
        // is given.
        fresh_ui::text(text.to_string())
            .wrap()
            .theme(pair("ui.help_separator_fg", "ui.popup_bg")),
        row().h(Sizing::Cells(1)),
    ])
}

/// One row of a `PopupContent::List`.
///
/// Every style here is the painter's, read off `render_with_hover` rather than
/// invented — `every_popup_theme_name_is_a_real_key` is the guard, the same one
/// the prompt's list carries after an earlier draft of *that* module used five
/// keys that exist nowhere.
fn list_row(item: &PopupListItem, row_theme: &str, hint: Option<&str>) -> Node<UiMsg> {
    // The row's ink, and the same ink with the muted foreground. A row whose
    // name is unreadable would have no ink to layer on, so it keeps its name
    // and the decorations below are simply not applied.
    let row_ink = Ink::parse(row_theme);
    let muted_ink = row_ink
        .clone()
        .map(|i| i.with_fg(Paint::key("ui.help_separator_fg")));
    let muted = muted_ink
        .as_ref()
        .map(Ink::to_string)
        .unwrap_or_else(|| row_theme.to_string());
    let mut cells: Vec<Node<UiMsg>> = Vec::new();
    if let Some(icon) = &item.icon {
        cells.push(fresh_ui::text(format!("{icon} ")).theme(row_theme.to_string()));
    }
    // Leading whitespace is kept out of the underline: an indented row is a
    // nested one, and underlining its indent makes the link look ragged.
    let trimmed = item.text.trim_start();
    let indent = item.text.len() - trimmed.len();
    if indent > 0 {
        cells.push(fresh_ui::text(&item.text[..indent]).theme(row_theme.to_string()));
    }
    // A row with a `data` payload acts on click, so it reads as a link; a
    // disabled one recedes and takes the muted foreground with it.
    let mut extra = Attrs::NONE;
    if item.data.is_some() && !item.disabled {
        extra = extra | Attrs::UNDERLINE;
    }
    let base = if item.disabled {
        extra = extra | Attrs::DIM;
        muted_ink.clone()
    } else {
        row_ink.clone()
    };
    let text_theme = base
        .map(|i| i.plus(extra).to_string())
        .unwrap_or_else(|| row_theme.to_string());
    cells.push(fresh_ui::text(trimmed).theme(text_theme).elide(Elide::Tail));
    if let Some(detail) = &item.detail {
        cells.push(fresh_ui::text(format!(" {detail}")).theme(muted.clone()));
    }
    // The gap that right-aligns the hint. The painter measured every span it
    // had emitted, subtracted, and emitted that many spaces — and skipped the
    // hint entirely when the sum did not fit. A flexible gap does both: it
    // takes what is left, and what is left is nothing when nothing is left.
    cells.push(row().flex(1).min_w(1));
    if let Some(h) = hint {
        cells.push(fresh_ui::text(format!("({h})")).theme(muted));
    }
    row().h(Sizing::Cells(1)).children(cells)
}

/// A popup's content, whichever kind it is.
///
/// One viewport around all three: the painter kept a `scroll_offset` on the
/// `Popup` and sliced by hand — `.skip(self.scroll_offset).take(height)` for
/// text, a `ListState` offset for a list — and decided separately whether a
/// scrollbar was needed by re-wrapping the content at a width that assumed one.
/// A viewport owns the window, and emits the bar exactly when the content
/// overflows.
pub fn content(c: &PopupContent, selected_hint: Option<&str>) -> Node<UiMsg> {
    match c {
        // A row per line, not one text node carrying newlines: a viewport
        // measures its child against the window, so a single node reports the
        // window's height and the content never overflows — no scrollbar, and
        // a wheel with nowhere to go. A column's natural height is the sum of
        // its children, which is what the window has to be compared against.
        // Each line still wraps on its own, so a long one takes the rows it
        // needs and the sum stays honest.
        PopupContent::Text(lines) => {
            fresh_ui::viewport(selectable(col().children(lines.iter().map(|l| {
                fresh_ui::text(l.clone())
                    .wrap()
                    .theme(pair("ui.popup_text_fg", "ui.popup_bg"))
            }))))
            .scrollbar()
        }
        PopupContent::Markdown(lines) => fresh_ui::viewport(selectable(
            col().children(
                lines
                    .iter()
                    .map(|l| fresh_ui::text_runs(styled_runs(l)).wrap()),
            ),
        ))
        .scrollbar(),
        PopupContent::List { items, selected } => {
            let rows: Rc<Vec<PopupListItem>> = Rc::new(items.clone());
            let for_row = rows.clone();
            let sel = *selected;
            let hint = selected_hint.map(str::to_string);
            let list = fresh_ui::widgets::List::windowed(
                rows.len(),
                |i| Key::Pair("popup_item".into(), i as u64),
                move |i| match for_row.get(i) {
                    Some(it) => list_row(
                        it,
                        &row_theme(i == sel, false),
                        (i == sel).then(|| hint.as_deref()).flatten(),
                    ),
                    None => row().h(Sizing::Cells(1)),
                },
            )
            .selected(sel)
            .scrollbar()
            // **The keyboard is the popup's, and the popup is not in this
            // tree.** Its selection is set here every frame and its keys —
            // Up, Down, Home, End, Tab to accept — are answered by
            // `dispatch_popup_keys`. A list in the focus ring would only be
            // somewhere for Tab to land, and Tab accepts a completion.
            .focusable(false)
            .row_theme(move |i, st| row_theme(i == sel, st == RowState::Hover))
            .on_select(|i| UiMsg::Ui(UiFact::PopupSelect(i)));
            col().child(fresh_ui::ComponentExt::node(list))
        }
    }
}

/// Text a press can land in, and a drag can sweep across.
///
/// Wrapped *inside* the viewport, so `Event::local` is already in the content's
/// coordinates: the row is the line, the column is the column, and the window's
/// offset never enters into it. That is the whole of what
/// `handle_click_buffer_popups` recovered by subtracting an inner rectangle's
/// origin and adding a stored `scroll_offset` back on.
///
/// The press captures the pointer, so a sweep that leaves the popup keeps
/// reporting positions relative to this node — which is what selecting by drag
/// means, and what `PointerGrab::PopupSelect` existed to arrange.
///
/// What a press *means* stays the host's, per the ledger's finding B: the
/// library says where selecting is meaningful and holds no selection model.
fn selectable(content: Node<UiMsg>) -> Node<UiMsg> {
    fn cell(e: &fresh_ui::Event) -> (usize, usize) {
        (e.local.y.max(0) as usize, e.local.x.max(0) as usize)
    }
    fresh_ui::gesture(content)
        .on(
            fresh_ui::GestureKind::Press,
            Rc::new(|e: &fresh_ui::Event| {
                if e.button != fresh_ui::MouseButton::Left {
                    return None;
                }
                e.capture_pointer();
                e.stop();
                let (line, col) = cell(e);
                Some(UiMsg::Ui(UiFact::PopupTextPress { line, col }))
            }),
        )
        .on(
            fresh_ui::GestureKind::Move,
            Rc::new(|e: &fresh_ui::Event| {
                let (line, col) = cell(e);
                Some(UiMsg::Ui(UiFact::PopupTextDrag { line, col }))
            }),
        )
}

/// The painter's row ladder, in the painter's own keys.
fn row_theme(selected: bool, hovered: bool) -> String {
    match (selected, hovered) {
        (true, _) => pair("ui.popup_selection_fg", "ui.popup_selection_bg"),
        (false, true) => pair("ui.menu_hover_fg", "ui.menu_hover_bg"),
        (false, false) => pair("ui.popup_text_fg", "ui.popup_bg"),
    }
}

/// A markdown line's spans, as runs.
///
/// **Literals, and honestly so.** A `StyledSpan` carries a ratatui `Style` —
/// the markdown renderer already chose the colours, and there is no theme key
/// behind them. `shell_theme::literal` writes each as the `#rrggbb` / `#i42` /
/// `#Name` form the grammar reads back, which loses nothing that exists:
/// `names()` reports `None` for such a half, and that is the true answer for a
/// colour nobody named. A span that sets only a foreground keeps the popup's
/// background, which is what a ratatui `Style` with one field set already
/// meant.
fn styled_runs(line: &crate::view::markdown::StyledLine) -> Vec<Run> {
    line.spans
        .iter()
        .map(|s| {
            // The popup's own ink, with only what the span actually mentions
            // moved. A ratatui `Style` with one field set already meant "leave
            // the rest alone", and that is now what this says.
            let mut ink = Ink::keys("ui.popup_text_fg", "ui.popup_bg")
                .with_attrs(Attrs::from_modifier(s.style.add_modifier));
            if let Some(c) = s.style.fg {
                ink = ink.with_fg(Paint::Lit(c));
            }
            if let Some(c) = s.style.bg {
                ink = ink.with_bg(Paint::Lit(c));
            }
            Run::themed(s.text.clone(), ink.to_string())
        })
        .collect()
}

/// Every name this module can hand to `shell_theme`, for the guard test.
#[cfg(test)]
fn every_theme_name() -> Vec<String> {
    let mut out = vec![
        pair("ui.popup_border_fg", "ui.popup_bg"),
        pair("ui.help_separator_fg", "ui.popup_bg"),
    ];
    for sel in [false, true] {
        for hov in [false, true] {
            let t = row_theme(sel, hov);
            out.push(
                Ink::parse(&t)
                    .expect("a row ladder entry is readable")
                    .with_fg(Paint::key("ui.help_separator_fg"))
                    .to_string(),
            );
            out.push(t);
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use fresh_ui::{row, Size, Ui};

    const FRAME: (u16, u16) = (80, 24);

    /// Lay a popup of exactly `w`×`h` out at `position`, and report where it
    /// landed. The size is fixed rather than measured so this compares
    /// *placement* with `calculate_area`'s placement and nothing else.
    fn placed_rect(
        position: &PopupPosition,
        caret: Caret,
        w: u16,
        h: u16,
    ) -> ratatui::layout::Rect {
        let key = fresh_ui::Key::from(7u64);
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            col().child(
                placed(position, CaretAnchor::Caret).key(key.clone()).child(
                    row()
                        .w(Sizing::Cells(w))
                        .h(Sizing::Cells(h))
                        .theme("ui.popup_border_fg/ui.popup_bg"),
                ),
            ),
            Size::new(FRAME.0, FRAME.1),
        );
        // The buffer's leaf publishes the caret; here the test stands in for
        // it. One cell, which is what a caret is.
        if let Some((x, y)) = caret {
            ui.set_host_anchor(
                CaretAnchor::Caret.key(),
                fresh_ui::Rect::new(x as i32, y as i32, 1, 1),
            );
        }
        let spec = ui.place_layers(Size::new(FRAME.0, FRAME.1)).clone();
        let r = spec
            .index
            .iter()
            .find(|(k, _)| *k == key)
            .and_then(|(_, range)| spec.items.get(range.start))
            .map(|i| i.rect)
            .unwrap_or_default();
        ratatui::layout::Rect {
            x: r.x.max(0) as u16,
            y: r.y.max(0) as u16,
            width: r.w,
            height: r.h,
        }
    }

    /// **Ledger rules 1–4: the cursor-relative family, its flip and its
    /// clamp.**
    ///
    /// The table *is* the rule. It was a sweep against `calculate_area`, which
    /// is how the port was checked; that function is gone now — its six
    /// strategies are an `Anchor`, a `Place` and a `Fit` — so what is left is
    /// the rectangles themselves, at the edges where the six hand-written
    /// clamps were most likely to have disagreed with each other.
    ///
    /// The bottom rows are the ones the original sweep could not reach: every
    /// caret in it sat far enough from the bottom that `Fit::FLIP` never fired,
    /// so the flip the port replaced thirteen lines of "not enough space below,
    /// put above" with was never actually compared.
    #[test]
    fn the_cursor_relative_placements() {
        // caret, size, expected origin.
        let at_cursor: &[((u16, u16), (u16, u16), (u16, u16))] = &[
            ((0, 0), (20, 5), (0, 0)),
            ((1, 1), (20, 5), (1, 1)),
            // Wider than the frame: pinned to the left edge, not hanging off
            // the right.
            ((1, 1), (FRAME.0, 3), (0, 1)),
            ((40, 12), (40, 10), (40, 12)),
            // Against the right edge: pulled back by its own width.
            ((FRAME.0 - 1, 12), (20, 5), (FRAME.0 - 20, 12)),
            ((FRAME.0 - 3, 2), (40, 10), (FRAME.0 - 40, 2)),
            // Against the bottom edge: pulled up by its own height. See the
            // divergence test below for what the painter did instead.
            ((40, FRAME.1 - 2), (20, 5), (40, FRAME.1 - 5)),
            ((2, FRAME.1 - 4), (40, 10), (2, FRAME.1 - 10)),
        ];
        for &(c, (w, h), want) in at_cursor {
            assert_eq!(
                placed_rect(&PopupPosition::AtCursor, Some(c), w, h),
                ratatui::layout::Rect::new(want.0, want.1, w, h),
                "AtCursor caret={c:?} size={w}x{h}"
            );
        }

        let below: &[((u16, u16), (u16, u16), (u16, u16))] = &[
            // The row *after* the caret's, which is what makes the anchor a
            // cell rather than a point.
            ((0, 0), (20, 5), (0, 1)),
            ((40, 12), (40, 10), (40, 13)),
            ((FRAME.0 - 1, 12), (20, 5), (FRAME.0 - 20, 13)),
            // No room below: it flips above the caret, clearing its row.
            ((40, FRAME.1 - 2), (20, 5), (40, FRAME.1 - 7)),
            ((40, FRAME.1 - 1), (20, 5), (40, FRAME.1 - 6)),
        ];
        for &(c, (w, h), want) in below {
            assert_eq!(
                placed_rect(&PopupPosition::BelowCursor, Some(c), w, h),
                ratatui::layout::Rect::new(want.0, want.1, w, h),
                "BelowCursor caret={c:?} size={w}x{h}"
            );
        }
    }

    /// **Ledger rule 5: centred.**
    #[test]
    fn the_screen_placements() {
        for (w, h) in [(20u16, 5u16), (41, 11), (FRAME.0, FRAME.1)] {
            assert_eq!(
                placed_rect(&PopupPosition::Centered, None, w, h),
                ratatui::layout::Rect::new((FRAME.0 - w) / 2, (FRAME.1 - h) / 2, w, h),
                "Centered size={w}x{h}"
            );
        }
    }

    /// **A popup near the bottom is moved up rather than cut off.** A
    /// divergence, found by the sweep above and kept deliberately.
    ///
    /// `calculate_area` clamps `x` for every strategy and never clamps `y` for
    /// `AtCursor`: the rectangle is allowed to run off the bottom, and
    /// `clamp_rect_to_bounds` then *truncates* it at paint —
    /// `height: rect.height.min(max_height)` with the origin left where it
    /// was. So a five-line popup opened on the last row shows one line.
    ///
    /// `Fit::CLAMP` pulls the whole box back inside instead, so all five show.
    /// That is what clamping means, it is what every other strategy here
    /// already does on the horizontal axis, and a popup that is cut to one
    /// line is not a popup anyone wanted. Recorded rather than quietly
    /// changed.
    #[test]
    fn a_popup_at_the_bottom_edge_moves_up_instead_of_being_truncated() {
        let caret = (40, FRAME.1 - 1);
        let (w, h) = (20u16, 5u16);
        // What the painter did, written down rather than computed: it left the
        // origin on the caret's row — `y = FRAME.1 - 1` — and let
        // `clamp_rect_to_bounds` cut the box to the one row that fitted.
        assert_eq!(
            placed_rect(&PopupPosition::AtCursor, Some(caret), w, h),
            ratatui::layout::Rect::new(40, FRAME.1 - h, w, h),
            "the description pulls the whole box inside"
        );
    }

    /// **Every name is a real theme key.**
    ///
    /// `shell_theme::resolve` falls back to the editor's plain ground for a
    /// name it cannot resolve — silently. The prompt's list shipped a draft
    /// with five keys that exist nowhere and nothing failed; every row would
    /// simply have painted in the default colour. This is the only thing that
    /// catches that, and it is why the popup's ladder was read off
    /// `render_with_hover` rather than written from memory.
    #[test]
    fn every_popup_theme_name_is_a_real_key() {
        use crate::app::shell_host::shell_theme::names;
        let theme = crate::view::theme::Theme::from_json(r#"{"name":"t"}"#).unwrap();
        for name in every_theme_name() {
            let (fg, bg) = names(&name);
            for half in [fg, bg] {
                let half = half.unwrap_or_else(|| panic!("{name:?} has an unnamed half"));
                assert!(
                    theme.resolve_theme_key(&half).is_some(),
                    "{half:?} (in {name:?}) is not a theme key"
                );
            }
        }
    }

    /// **A list row's ink is the painter's ladder.** Selected, hovered and
    /// plain each have their own pair, and a disabled row is muted and dim —
    /// `Modifier::DIM` applied by hand in the painter, now a name the theme can
    /// reach.
    #[test]
    fn a_disabled_row_is_muted_and_a_clickable_one_is_underlined() {
        let item = |text: &str, data: Option<&str>, disabled: bool| PopupListItem {
            text: text.into(),
            detail: None,
            icon: None,
            data: data.map(str::to_string),
            disabled,
        };
        let c = PopupContent::List {
            items: vec![
                item("plain", None, false),
                item("clickable", Some("go"), false),
                item("off", Some("go"), true),
            ],
            selected: 0,
        };
        let mut ui: Ui<UiMsg> = Ui::new();
        let spec = ui.frame(content(&c, None), Size::new(40, 6)).clone();
        let theme_of = |needle: &str| {
            spec.items
                .iter()
                .find(|i| matches!(&i.draw, fresh_ui::Draw::Lines(l) if l.iter().any(|s| s.contains(needle))))
                .map(|i| i.theme.as_str().to_string())
                .unwrap_or_else(|| panic!("no run for {needle:?}"))
        };
        assert_eq!(
            theme_of("plain"),
            row_theme(true, false),
            "row 0 is selected"
        );
        assert!(
            theme_of("clickable").ends_with("+underline"),
            "a row with a payload reads as a link: {}",
            theme_of("clickable")
        );
        let off = theme_of("off");
        assert!(
            off.starts_with("ui.help_separator_fg/") && off.contains("+dim"),
            "a disabled row is muted and dim: {off}"
        );
    }

    /// **Ledger rule 12: clicking a row reports that row, by index.**
    #[test]
    fn a_click_on_a_popup_row_reports_it() {
        let c = PopupContent::List {
            items: (0..5)
                .map(|i| PopupListItem {
                    text: format!("item {i}"),
                    detail: None,
                    icon: None,
                    data: Some("go".into()),
                    disabled: false,
                })
                .collect(),
            selected: 0,
        };
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(content(&c, None), Size::new(40, 6));
        let r = ui.rect_of(
            ui.find_by_key(&Key::Pair("popup_item".into(), 2))
                .expect("row 2"),
        );
        let at = fresh_ui::Point::new(r.x + 1, r.y);
        let mut msgs = ui
            .dispatch(fresh_ui::Input::press(
                at,
                fresh_ui::MouseButton::Left,
                fresh_ui::Mods::NONE,
            ))
            .msgs;
        msgs.extend(
            ui.dispatch(fresh_ui::Input::release(
                at,
                fresh_ui::MouseButton::Left,
                fresh_ui::Mods::NONE,
            ))
            .msgs,
        );
        assert!(
            msgs.iter()
                .any(|m| matches!(m, UiMsg::Ui(UiFact::PopupSelect(2)))),
            "got {msgs:?}"
        );
    }

    /// **Ledger rule 11: a scrollbar when the content overflows, not
    /// otherwise.** The painter decided this by re-wrapping the content at a
    /// width that *assumed* a scrollbar and comparing line counts; a viewport
    /// emits the bar exactly when its content does not fit, and reserves the
    /// lane itself.
    #[test]
    fn a_popup_scrolls_only_when_its_content_overflows() {
        let bar = |n: usize| {
            let c = PopupContent::Text((0..n).map(|i| format!("line {i}")).collect());
            let mut ui: Ui<UiMsg> = Ui::new();
            let spec = ui.frame(content(&c, None), Size::new(30, 5)).clone();
            spec.items
                .iter()
                .any(|i| matches!(i.draw, fresh_ui::Draw::Scrollbar { .. }))
        };
        assert!(!bar(3), "three lines in five rows: no bar");
        assert!(bar(50), "fifty lines in five rows: a bar");
    }

    /// **`AboveCursor` clears the caret's row.** A divergence where the
    /// painter disagrees with its own comment.
    ///
    /// ```text
    ///   PopupPosition::AboveCursor => {
    ///       // Position so bottom of popup is one row above cursor
    ///       (cursor_y + 1).saturating_sub(height)
    ///   }
    /// ```
    ///
    /// `cursor_y + 1 - height` puts the popup's *last* row on `cursor_y` — on
    /// the caret, not one row above it. `Anchor::Cell` + `Place::Above` gives
    /// `cursor_y - height`, which is what the comment says and what the
    /// sibling `BelowCursor` already does in the other direction
    /// (`cursor_y + 1`, clearing the caret's cell).
    ///
    /// A popup that covers the character it is anchored to is the bug
    /// `Anchor::Cell` exists to make unsayable, so this is kept.
    #[test]
    fn a_popup_above_the_caret_does_not_cover_it() {
        let caret = (40u16, 12u16);
        let (w, h) = (20u16, 5u16);
        let got = placed_rect(&PopupPosition::AboveCursor, Some(caret), w, h);
        assert_eq!(
            got.y + got.height - 1,
            caret.1 - 1,
            "the description leaves the caret's row clear"
        );
        // The painter's last row was `caret.1` itself — `(cursor_y + 1) - h`,
        // one row lower than this.
        assert_eq!(got.y, caret.1 - h);
    }

    /// **Ledger rule: `Fixed` goes where it is told, and no further than the
    /// frame's edges.**
    #[test]
    fn a_fixed_placement_clamps_to_the_frame() {
        for (w, h) in [(20u16, 5u16), (40, 10)] {
            let clamped = |x: u16, y: u16| {
                ratatui::layout::Rect::new(x.min(FRAME.0 - w), y.min(FRAME.1 - h), w, h)
            };
            for (x, y) in [
                (0u16, 0u16),
                (10, 4),
                (FRAME.0 - 2, FRAME.1 - 2),
                // Far outside: pulled back to the last position that fits.
                (200, 200),
            ] {
                assert_eq!(
                    placed_rect(&PopupPosition::Fixed { x, y }, None, w, h),
                    clamped(x, y),
                    "Fixed {x},{y} size={w}x{h}"
                );
            }
        }
    }

    /// **Ledger finding H: a popup is bounded by the window, not by the
    /// chrome column.** A divergence, and the deliberate direction of it.
    ///
    /// `calculate_area` is handed the chrome area — the column right of a left
    /// dock — and clamps into it. A layer is placed against the frame. With no
    /// dock the two are the same rectangle and nothing here differs; with one,
    /// they part company in exactly two places, and a sweep over every
    /// reachable caret column and a range of widths says they are the only two.
    ///
    /// **Centred popups.** The painter centres them in the chrome column, so a
    /// modal slides sideways when a dock opens and sits off-centre in the
    /// window while it is there. The surfaces that use `Centered` and
    /// `CenteredOverlay` are modals — workspace trust, Live Grep's 80% overlay
    /// — and a modal centres on the window everywhere else in software. The
    /// tree's answer is the conventional one.
    ///
    /// **A popup wider than the chrome column.** The painter pins it to the
    /// column's left edge and lets `clamp_rect_to_bounds` cut the right end
    /// off; the tree pushes it flush to the window's right edge, whole, lying
    /// over the dock. Truncation loses content and overlap loses nothing — a
    /// floating surface over a panel is what floating means.
    ///
    /// Not a divergence: a caret to the *left* of the dock's edge. The painter
    /// guards against it with a `.max(area.x)`, but the caret is in the buffer
    /// and the buffer is inside the chrome, so the input is unreachable.
    #[test]
    fn a_popup_is_bounded_by_the_window_not_by_the_chrome_column() {
        const DOCK: u16 = 20;
        const CHROME_W: u16 = FRAME.0 - DOCK;

        // Centred: the window's centre. The painter's answer was the column's,
        // `DOCK + (CHROME_W - w) / 2` — ten columns to the right of this, and
        // moving whenever the dock opened or closed.
        let w = 30;
        assert_eq!(
            placed_rect(&PopupPosition::Centered, None, w, 1).x,
            (FRAME.0 - w) / 2
        );
        assert_ne!((FRAME.0 - w) / 2, DOCK + (CHROME_W - w) / 2);

        // Wider than the column: whole and overlapping. The painter pinned it
        // to the column's left edge — `x = DOCK` — and let the right end be
        // cut off.
        let wide = CHROME_W + 10;
        let caret = Some((FRAME.0 - 2, 5));
        assert_eq!(
            placed_rect(&PopupPosition::BelowCursor, caret, wide, 1).x,
            FRAME.0 - wide,
            "the whole popup is on screen"
        );
        assert_ne!(FRAME.0 - wide, DOCK);

        // Everything that fits in the column lands where it did before: inside
        // the column, the two rules cannot differ, and the sweep that proved it
        // ran over every reachable caret column.
        for w in [10u16, 30, CHROME_W] {
            for cx in DOCK..(FRAME.0 - w).max(DOCK + 1) {
                assert_eq!(
                    placed_rect(&PopupPosition::BelowCursor, Some((cx, 5)), w, 1).x,
                    cx,
                    "width {w}, caret column {cx}"
                );
            }
        }
    }
}
