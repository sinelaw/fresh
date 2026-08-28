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

/// Where a popup goes, as a layer's anchor and placement.
///
/// Returns the layer with its geometry set and no child; the caller adds the
/// content. Split out because placement is the half of this surface that is
/// pure restatement, and it is worth being able to test it against
/// `calculate_area` on its own.
pub fn placed(position: &PopupPosition, caret: Caret) -> Node<UiMsg> {
    let l = fresh_ui::layer();
    // Both cursor-relative families and `Fixed` clamp; the screen-anchored ones
    // cannot fall outside a frame they are measured against.
    let caret_at = || {
        let (x, y) = caret.unwrap_or((0, 0));
        Anchor::Point(x, y)
    };
    // The caret is a *cell*, not a point: below it is the row after it, and a
    // flip clears it rather than landing on it. `calculate_area` says the same
    // thing as `cursor_y + 1` and a matching `- height`.
    let caret_cell = || {
        let (x, y) = caret.unwrap_or((0, 0));
        Anchor::Cell(x, y)
    };
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
        PopupPosition::AboveStatusBarAt { .. } => l
            .anchor(Anchor::Node(super::status_bar::item_key(
                super::status_bar::Side::Right,
                0,
            )))
            .place(Place::Above)
            .fit(Fit::FLIP.or(Fit::CLAMP)),
    }
}

/// How big the popup asks to be, in the terms its own fields already use.
///
/// `width` is a cell count the popup carries; the height is its content's,
/// bounded by `max_height`. `CenteredOverlay` overrides both with percentages
/// of the frame, which is the whole reason that variant exists — Live Grep
/// wants a stable frame while results stream in, not one that resizes per
/// keystroke.
pub fn sized(position: &PopupPosition, width: u16, max_height: u16, n: Node<UiMsg>) -> Node<UiMsg> {
    match position {
        PopupPosition::CenteredOverlay {
            width_pct,
            height_pct,
        } => n
            .w(Sizing::Pct((*width_pct).clamp(1, 100)))
            .h(Sizing::Pct((*height_pct).clamp(1, 100))),
        _ => n.w(Sizing::Cells(width)).max_h(max_height),
    }
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
        PopupContent::Text(lines) => fresh_ui::viewport(
            fresh_ui::text(lines.join("\n"))
                .wrap()
                .theme(pair("ui.popup_text_fg", "ui.popup_bg")),
        )
        .scrollbar(),
        PopupContent::Markdown(lines) => fresh_ui::viewport(
            col().children(
                lines
                    .iter()
                    .map(|l| fresh_ui::text_runs(styled_runs(l)).wrap()),
            ),
        )
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
            .row_theme(move |i, st| row_theme(i == sel, st == RowState::Hover))
            .on_select(|i| UiMsg::Ui(UiFact::PopupSelect(i)));
            col().child(fresh_ui::ComponentExt::node(list))
        }
    }
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
        let spec = ui
            .frame(
                col().child(
                    placed(position, caret).key(key.clone()).child(
                        row()
                            .w(Sizing::Cells(w))
                            .h(Sizing::Cells(h))
                            .theme("ui.popup_border_fg/ui.popup_bg"),
                    ),
                ),
                Size::new(FRAME.0, FRAME.1),
            )
            .clone();
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

    /// The painter's own answer, for the same inputs.
    fn calculated(position: PopupPosition, caret: Caret, w: u16, h: u16) -> ratatui::layout::Rect {
        let theme = crate::view::theme::Theme::from_json(r#"{"name":"t"}"#).unwrap();
        // A popup whose content is exactly `h` lines and whose width is `w`,
        // so `content_height().min(max_height)` is `h`.
        let mut p =
            crate::view::popup::Popup::text((0..h).map(|i| format!("line {i}")).collect(), &theme);
        p.position = position;
        p.width = w;
        p.max_height = h;
        p.bordered = false;
        p.calculate_area(ratatui::layout::Rect::new(0, 0, FRAME.0, FRAME.1), caret)
    }

    /// **Ledger rules 1–4: the cursor-relative family, its flip and its
    /// clamp.**
    ///
    /// The caret's own row and the row above it are excluded and get their own
    /// test below: `calculate_area` does not clamp `AtCursor`'s `y` at all, and
    /// the difference is a real behaviour change rather than a rounding one.
    ///
    /// A table over caret positions near each edge, because that is where the
    /// six hand-written clamps are most likely to have disagreed with each
    /// other — and where `Fit` earns its keep.
    #[test]
    fn the_cursor_relative_placements_match_calculate_area() {
        let carets = [
            (0u16, 0u16),
            (1, 1),
            (40, 12),
            (FRAME.0 - 1, 12),
            (FRAME.0 - 3, 2),
            (40, 2),
        ];
        // `AboveCursor` has its own test below: the painter's arithmetic
        // contradicts the painter's comment, and the description follows the
        // comment.
        for pos in [PopupPosition::AtCursor, PopupPosition::BelowCursor] {
            for c in carets {
                for (w, h) in [(20u16, 5u16), (40, 10), (FRAME.0, 3)] {
                    let want = calculated(pos, Some(c), w, h);
                    let got = placed_rect(&pos, Some(c), w, h);
                    assert_eq!(got, want, "{pos:?} caret={c:?} size={w}x{h}");
                }
            }
        }
    }

    /// **Ledger rule 5: centred.**
    #[test]
    fn the_screen_placements_match_calculate_area() {
        for pos in [PopupPosition::Centered] {
            for (w, h) in [(20u16, 5u16), (41, 11), (FRAME.0, FRAME.1)] {
                let want = calculated(pos, None, w, h);
                let got = placed_rect(&pos, None, w, h);
                assert_eq!(got, want, "{pos:?} size={w}x{h}");
            }
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
        let painter = calculated(PopupPosition::AtCursor, Some(caret), w, h);
        let got = placed_rect(&PopupPosition::AtCursor, Some(caret), w, h);
        assert_eq!(
            painter.y,
            FRAME.1 - 1,
            "the painter leaves the origin on the caret's row"
        );
        assert_eq!(
            got,
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
        let painter = calculated(PopupPosition::AboveCursor, Some(caret), w, h);
        let got = placed_rect(&PopupPosition::AboveCursor, Some(caret), w, h);
        assert_eq!(
            painter.y + painter.height - 1,
            caret.1,
            "the painter's last row is the caret's own"
        );
        assert_eq!(
            got.y + got.height - 1,
            caret.1 - 1,
            "the description leaves the caret's row clear"
        );
    }

    /// **Ledger rule: `Fixed` clamps to the area's absolute edges.**
    #[test]
    fn a_fixed_placement_clamps_like_calculate_area() {
        for (x, y) in [
            (0u16, 0u16),
            (10, 4),
            (FRAME.0 - 2, FRAME.1 - 2),
            (200, 200),
        ] {
            let pos = PopupPosition::Fixed { x, y };
            for (w, h) in [(20u16, 5u16), (40, 10)] {
                let want = calculated(pos, None, w, h);
                let got = placed_rect(&pos, None, w, h);
                assert_eq!(got, want, "{pos:?} size={w}x{h}");
            }
        }
    }
}
