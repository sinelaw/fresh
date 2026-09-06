//! The floating-overlay prompt's card, as a description.
//!
//! The card is where the *other* suggestion list lives. It used to be drawn by
//! `SuggestionsRenderer`, and the reason it could not move was that its
//! rectangle was computed by this card's painter halfway through the frame,
//! long after the shell tree had been laid out. The card carves its bands here
//! now, so the list anchors to the results band by name (`Place::InCard`) and
//! both lists are the tree's.
//!
//! What moves here is the arithmetic *inside* the card, which is what was
//! duplicated:
//!
//! ```text
//!   header = input(1) + toolbar(n) + separator(1)     -> three rows in a col
//!   footer = 1 when the plugin set one                -> a row, or no row
//!   body   = inner - header - footer                  -> flex(1)
//!   body splits results | preview above 120 cols      -> two flexes, or one
//! ```
//!
//! `render_overlay_prompt` computed all four and `chrome::Prompt::collect`
//! re-derived the preview's rectangle from the cached copy, so a change to one
//! band's height had to be made in two places or the wheel routed to the wrong
//! pane.
//!
//! The card's *outer* rectangle stays where it is. `centered_overlay_rect` has
//! exactly one caller and carries floors (`max(20)`, `max(8)`) that a
//! percentage does not express; it is a rule with one home, which is not what
//! the migration is here to remove.

use std::rc::Rc;

use fresh_ui::{
    col, host, layout_reader, row, stack, text, text_runs, Event, LayoutInfo, Node, PointerMode,
    Rect, Run, Scrim, Sizing,
};

use crate::app::shell_host::shell_theme::{attrs, pair, Attrs, Ink, Paint};
use crate::primitives::display_width::str_width;

use super::msg::{UiFact, UiMsg};

/// A band of the card that a painter still owns.
///
/// The same idea as [`super::frame::HostRegion`], scoped to the card: every
/// band is in the tree whether or not it is visible, so it has a rectangle to
/// report, and a band that goes native keeps its key.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum CardRegion {
    /// The prompt's input line, full inner width.
    Input = 1,
    /// The plugin's widget toolbar. Zero rows when there is none.
    Toolbar = 2,
    /// Where the suggestion list goes.
    Results = 3,
    /// The preview pane, when the card is wide enough for one.
    Preview = 4,
    /// The plugin's footer row, when it set one.
    Footer = 5,
}

impl CardRegion {
    pub const ALL: [CardRegion; 5] = [
        CardRegion::Input,
        CardRegion::Toolbar,
        CardRegion::Results,
        CardRegion::Preview,
        CardRegion::Footer,
    ];

    pub fn id(self) -> u64 {
        self as u64
    }

    /// The inverse, for [`super::frame::HostTarget::from_host_id`].
    pub fn from_id(id: u64) -> Option<CardRegion> {
        CardRegion::ALL.into_iter().find(|r| r.id() == id)
    }
}

/// Below this many columns the preview is hidden and the results take the whole
/// body. The design doc's §5, "preview pane size when terminal is narrow".
pub const PREVIEW_MIN_COLS: u16 = 120;

/// What the card is showing. Content — never a rectangle, except the one the
/// card sits in.
#[derive(Clone, Debug, Default)]
pub struct Card {
    /// Where `centered_overlay_rect` put the card.
    pub at: Rect,
    /// The plugin's toolbar, when it set one: the interior of its toolbar
    /// panel (`PROMPT_TOOLBAR_PANEL_ID`), described in the header band and
    /// as tall as its controls need. The band measures it; nothing counts
    /// rows for it.
    pub toolbar: Option<super::panel::Interior>,
    /// Without a toolbar, whether the header band holds the painter's
    /// styled-text title row.
    pub title_row: bool,
    /// Whether the plugin set a footer.
    pub footer: bool,
    /// Whether the prompt is a search prompt, which names its key section
    /// (`prompt::keys_key`).
    pub search: bool,
    /// Whether the query input holds the keyboard — the toolbar panel's focus
    /// fact says no control does. The input's focus holder marks `autofocus`
    /// exactly then, so the ring rests on it unless a toolbar control has
    /// been given the keyboard.
    pub input_focused: bool,
    /// The caption on the top edge — and the title row under the input when
    /// there is no toolbar — as runs in the shell's names.
    pub title: Vec<Run>,
    /// The plugin's footer row, as runs; `footer` says whether it shows.
    pub footer_runs: Vec<Run>,
    /// The input row's content: the message, the query and its caret.
    pub input: super::prompt_line::PromptRow,
    /// The plugin's search status, at the right of the input row.
    pub status: String,
    /// `(selected + 1, total)` while there are suggestions, shown after the
    /// status.
    pub count: Option<(usize, usize)>,
}

/// A plugin's styled text as runs: every colour a name the shell grammar
/// reads, so the fold resolves it like any other run's. A segment with no
/// style paints in the node's own theme; a theme key names the entry, a
/// literal colour is a literal, and the attributes ride along.
pub fn styled_runs(
    segs: &[fresh_core::api::StyledText],
    default_fg: &str,
    default_bg: &str,
) -> Vec<Run> {
    use fresh_core::api::OverlayColorSpec;
    use std::borrow::Cow;
    let paint = |spec: Option<&OverlayColorSpec>, default: &str| -> Paint {
        match spec {
            None => Paint::Key(Cow::Owned(default.to_string())),
            Some(OverlayColorSpec::Rgb(r, g, b)) => {
                Paint::Lit(ratatui::style::Color::Rgb(*r, *g, *b))
            }
            Some(OverlayColorSpec::ThemeKey(k)) => {
                match crate::view::theme::named_color_from_str(k) {
                    Some(c) => Paint::Lit(c),
                    None => Paint::Key(Cow::Owned(k.clone())),
                }
            }
        }
    };
    segs.iter()
        .map(|s| {
            let Some(o) = &s.style else {
                return Run::plain(&s.text);
            };
            let mut words: Vec<&str> = Vec::new();
            if o.bold {
                words.push("bold");
            }
            if o.italic {
                words.push("italic");
            }
            if o.underline {
                words.push("underline");
            }
            let ink = Ink {
                fg: paint(o.fg.as_ref(), default_fg),
                bg: paint(o.bg.as_ref(), default_bg),
                attrs: Attrs::all_named(words),
            };
            Run::themed(&s.text, ink.to_string())
        })
        .collect()
}

impl Card {
    /// Whether the preview pane shows at this width.
    ///
    /// Read off the card's own width rather than passed in, so the rule has one
    /// home. `render_overlay_prompt` asked the same question of
    /// `overlay_rect.width` and `chrome::Prompt` believed the answer.
    pub fn preview(&self) -> bool {
        self.at.w >= PREVIEW_MIN_COLS
    }
}

pub fn region_key(r: CardRegion) -> fresh_ui::Key {
    fresh_ui::Key::Pair("card".into(), r.id())
}

/// The card's own key — what the prompt's keyboard layer confines the ring
/// to while the card is up (`prompt::keys_layer`).
pub fn card_key() -> fresh_ui::Key {
    CARD_KEY.with(|k| k.clone())
}

/// A band that only holds a rectangle: the results band, which the
/// suggestion list's own layer anchors to and paints.
fn band(r: CardRegion) -> Node<UiMsg> {
    row().key(region_key(r))
}

/// The preview pane: the one band of the card that is still cells — a
/// buffer rendered by the text pipeline into the rectangle layout gives it,
/// the way a pane's content is (`HostTarget::Card(CardRegion::Preview)`).
fn preview_host() -> Node<UiMsg> {
    host(super::frame::card_host_id(CardRegion::Preview)).key(region_key(CardRegion::Preview))
}

/// The card's ring and ground, and the title on its top edge.
fn ring() -> String {
    pair("ui.popup_border_fg", "ui.suggestion_bg")
}

fn caption_ink() -> String {
    attrs("ui.prompt_fg", "ui.suggestion_bg", &["bold"])
}

/// The title, on the top border: `Block::title` started one cell in from the
/// corner. Transparent to the pointer, as `modal::title_strip` explains.
fn caption(c: &Card) -> Node<UiMsg> {
    let clear = |n: Node<UiMsg>| n.pointer_mode(PointerMode::Transparent);
    row()
        .h(Sizing::Cells(1))
        .pointer_mode(PointerMode::Transparent)
        .children([
            clear(row().w(Sizing::Cells(1))),
            clear(text_runs(c.title.clone()).theme(caption_ink())),
            clear(row().flex(1)),
        ])
}

/// The card, as a layer over the chrome column.
///
/// Anchored at the corner `centered_overlay_rect` chose and sized to it — the
/// same shape [`super::prompt::Place::Inside`] uses, and for the same reason:
/// the rectangle is somebody else's answer and this only occupies it.
///
/// **The card is the tree's, ring and all.** The ring, the ground, the caption
/// on the top edge, the input row with its caret, the title row, the
/// separator and the footer are nodes; the suggestion list is a layer of its
/// own anchored to the results band; the preview is the one band still made
/// of cells — a buffer the text pipeline renders into the rectangle layout
/// gives it, as a pane's content is. `render_overlay_prompt`, which drew the
/// rest between the two fold bands, is deleted.
pub fn card(c: &Card) -> Node<UiMsg> {
    use fresh_ui::{layer, Anchor, Place};
    layer()
        .key(CARD_KEY.with(|k| k.clone()))
        .anchor(Anchor::Point(c.at.x.max(0) as u16, c.at.y.max(0) as u16))
        .place(Place::Over)
        // Everything behind the card recedes — the painter's
        // `apply_dimming_excluding(frame, overlay_rect)`, as the fold's own
        // dim (the settings dialog's rule, §3.6).
        .scrim(Some(Scrim::Dim))
        .child(
            // Absorbing outside the sizing, because a gesture is not a box and
            // the ring belongs to the box.
            absorb(
                stack()
                    .theme(ring())
                    .w(Sizing::Cells(c.at.w))
                    .h(Sizing::Cells(c.at.h))
                    .children([
                        col()
                            .border()
                            .clip(true)
                            // The bands fill the card: the middle one is a
                            // flex child, and flex divides what is left, so
                            // the column holding it takes the card's height
                            // rather than its own natural one (rule L15).
                            .child(body(c).h(Sizing::Flex(1))),
                        caption(c),
                    ]),
            ),
        )
}

/// The card's bands, top to bottom.
fn body(c: &Card) -> Node<UiMsg> {
    // The separator closing the header band: a rule across the card, in the
    // ring's ink.
    let separator = layout_reader(|info: LayoutInfo| {
        text("─".repeat(usize::from(info.constraints.max_w))).theme(ring())
    })
    .h(Sizing::Cells(1));
    let middle = match c.preview() {
        // Half and half — but *which* half gets the odd column is not a
        // detail. `body.width / 2` truncates, so the painter's results pane
        // was the narrower one at an odd width and the preview took the spare
        // column; two equal `flex(1)` children hand it to the first child
        // instead, which the parity sweep caught at 121 columns. `Pct(50)`
        // truncates the same way the division did, and the preview takes what
        // is left.
        true => row().flex(1).children([
            band(CardRegion::Results).w(Sizing::Pct(50)),
            preview_pane().flex(1),
        ]),
        // The preview is still in the tree taking nothing, so it has a
        // rectangle to report and the results' own is unaffected — the rule
        // `frame_tree` states for a hidden region.
        false => row().flex(1).children([
            band(CardRegion::Results).flex(1),
            preview_pane().w(Sizing::Cells(0)),
        ]),
    };
    let footer = text_runs(c.footer_runs.clone())
        .theme(pair("ui.suggestion_fg", "ui.suggestion_bg"))
        .key(region_key(CardRegion::Footer))
        .h(Sizing::Cells(c.footer as u16));
    col().children([input_band(c), toolbar_band(c), separator, middle, footer])
}

/// The preview pane: the rule on its left edge — the painter's
/// `Borders::LEFT` — and the buffer's host beside it, which is what the
/// `Preview` band names.
fn preview_pane() -> Node<UiMsg> {
    let rule = layout_reader(|info: LayoutInfo| {
        col().children(
            (0..info.constraints.max_h)
                .map(|_| text("│").theme(ring()))
                .collect::<Vec<_>>(),
        )
    })
    .w(Sizing::Cells(1));
    row().children([rule, preview(preview_host().flex(1))])
}

/// The input row's content: the message in the card's ink, the query in the
/// editor's with the caret stated inside it, and the plugin's status and the
/// selection count against the right edge — the painter's row, as runs.
fn input_row(c: &Card) -> Node<UiMsg> {
    let c = Rc::new(c.clone());
    layout_reader(move |info: LayoutInfo| input_row_at(&c, info.constraints.max_w))
        .h(Sizing::Cells(1))
}

fn input_row_at(c: &Card, width: u16) -> Node<UiMsg> {
    let message_w = str_width(&c.input.message).min(usize::from(width)) as u16;
    let count = c
        .count
        .map(|(sel, total)| format!("{sel} / {total}"))
        .unwrap_or_default();
    let count_w = str_width(&count);
    // One trailing column, so the count does not sit flush against the ring.
    let right_gap = usize::from(count_w > 0);
    let status_w = str_width(&c.status);
    let status_gap = if status_w > 0 && count_w > 0 { 2 } else { 0 };
    let cluster_w =
        (status_w + status_gap + count_w + right_gap).min(usize::from(width - message_w)) as u16;
    let input_cols = width - message_w - cluster_w;
    let dim = pair("ui.popup_border_fg", "editor.bg");
    let cluster = text_runs([
        Run::themed(&c.status, dim.clone()),
        Run::plain(" ".repeat(status_gap)),
        Run::themed(&count, dim),
        Run::plain(" ".repeat(right_gap)),
    ]);
    row().theme(pair("editor.fg", "editor.bg")).children([
        text_runs([Run::plain(&c.input.message)])
            .theme(pair("ui.suggestion_fg", "ui.suggestion_bg"))
            .w(Sizing::Cells(message_w)),
        super::prompt_line::input_window(&c.input, input_cols, c.input_focused)
            .w(Sizing::Cells(input_cols)),
        cluster.w(Sizing::Cells(cluster_w)),
    ])
}

/// The input row: the painter's line, and **the prompt's focus holder**.
///
/// A bottom-row prompt's keyboard layer holds a sink that takes every key for
/// `dispatch_prompt_key`; with a card up the layer confines the ring to the
/// card instead, and this band is that sink — keyed the same way, so
/// `frame::key_context_of` reads the same key section off it. It marks
/// `autofocus` while no toolbar control has the keyboard, and reports when
/// the ring lands back on it so the toolbar's focus fact can be cleared
/// (`UiFact::CardInputFocus`).
fn input_band(c: &Card) -> Node<UiMsg> {
    let ring = c.toolbar.is_some();
    let n = fresh_ui::focusable(input_row(c).key(region_key(CardRegion::Input)))
        .h(Sizing::Cells(1))
        .key(super::prompt::keys_key(c.search))
        .on_key(move |e: &Event| {
            // **Tab is the ring's while there is a ring to walk.** With a
            // toolbar in the card, Tab and Shift+Tab are declined here so
            // the tree's traversal moves onto its controls — the
            // interception `handle_overlay_toolbar_key` did ahead of the
            // prompt. Without one, Tab is the prompt's, as it is on the
            // bottom row (path completion, `sync_input_on_navigate`).
            let tab = e.key.is_some_and(|k| {
                matches!(k.code, fresh_ui::KeyCode::Tab | fresh_ui::KeyCode::BackTab)
            });
            if ring && tab {
                return None;
            }
            e.stop();
            Some(UiMsg::Ui(UiFact::PromptKey))
        })
        .on_focus_change(
            |e: &Event| match e.kind == fresh_ui::GestureKind::FocusGained {
                true => Some(UiMsg::Ui(UiFact::CardInputFocus)),
                false => None,
            },
        );
    match c.input_focused {
        true => n.autofocus(),
        false => n,
    }
}

/// The header band under the input row: the plugin's toolbar, described, or
/// the painter's title row, or nothing.
///
/// **The toolbar is a panel's interior, on the prompt's ring.** Its controls
/// are the same nodes a dock panel's are (`widgets::node`), its presses and
/// keys route to the toolbar panel through `Slot::PromptToolbar`, and the
/// band is as tall as they lay out — the count `render_spec_no_autofocus`
/// used to produce for the band's height was a second layout of the same
/// spec. Its capture-leg rule is the surface's: a key that navigates the
/// results or types is the query input's, and pressing it on a focused
/// toggle hands the keyboard back with the key — the ring the old
/// `handle_overlay_toolbar_key` walked by hand, said on the node.
fn toolbar_band(c: &Card) -> Node<UiMsg> {
    let Some(i) = c.toolbar.clone() else {
        // Without a toolbar the band holds the title row, when there is a
        // title: the caption's runs again, under the input.
        //
        // **A hidden row holds no runs, not runs in a rectangle of no rows.**
        // The band keeps its key either way, because a region always reports a
        // rectangle (`frame_tree`'s rule) — but a `Draw::Lines` in a
        // zero-height rect is an item in the display list that paints nothing
        // and reads as the caption to anything scanning for the title's text.
        return text_runs(match c.title_row {
            true => c.title.clone(),
            false => Vec::new(),
        })
        .theme(caption_ink())
        .key(region_key(CardRegion::Toolbar))
        .h(Sizing::Cells(c.title_row as u16));
    };
    let body = fresh_ui::layout_reader(move |info: fresh_ui::LayoutInfo| {
        let inner_w = info.constraints.max_w.max(1);
        super::widgets::node(
            &i.spec,
            inner_w,
            &super::widgets::Ctx {
                slot: super::widgets::Slot::PromptToolbar,
                states: &i.states,
                focus_key: i.focus_key.clone(),
                keyboard: i.keyboard,
                hovered_key: i.hovered_key.clone(),
                marker_gutter: i.marker_gutter,
                hovered_item_key: i.hovered_item_key.clone(),
                hovered_popup_row: i.hovered_popup_row.clone(),
                avail_height: None,
                scrollbar_reveal: None,
                surface: super::widgets::panel_surface(),
                markdown: i.markdown.as_ref().map(|m| m.ctx()),
            },
        )
        .w(Sizing::Cells(inner_w))
    });
    let to_input: super::panel::Capture = Rc::new(|e: &Event| {
        let k = e.key?;
        let input_key = match k.code {
            fresh_ui::KeyCode::Up
            | fresh_ui::KeyCode::Down
            | fresh_ui::KeyCode::PageUp
            | fresh_ui::KeyCode::PageDown => true,
            // Space activates the focused control; every other character
            // types into the query.
            fresh_ui::KeyCode::Char(ch) => ch != ' ',
            _ => false,
        };
        if !input_key {
            return None;
        }
        e.stop();
        Some(UiMsg::Ui(UiFact::PromptKey))
    });
    super::panel::interior_capturing(
        super::widgets::Slot::PromptToolbar,
        Some(to_input),
        false,
        body,
    )
    .key(region_key(CardRegion::Toolbar))
    .h(Sizing::Auto)
}

/// A press anywhere on the card that no band claimed dies here, and so does a
/// wheel that found no window.
///
/// This is `chrome:overlay_prompt_scrim`, `chrome:overlay_prompt_modal` and
/// `chrome:overlay_rclick_guard` — three boxes for one rule, split by gesture
/// because a box could only say one thing at a time. The rule is that the
/// overlay is mouse-modal: while it is up, nothing under it hears the pointer,
/// whichever button and however many clicks.
///
/// The wheel is the same rule with one exception, which is why it is here and
/// not left to fall through: the preview pane is still a painter's, so it has
/// no window the wheel could chain into, and the pane has to be told.
fn absorb(n: Node<UiMsg>) -> Node<UiMsg> {
    fresh_ui::gesture(n)
        .on(
            fresh_ui::GestureKind::Press,
            Rc::new(|e: &Event| {
                e.stop();
                None
            }),
        )
        .on(
            fresh_ui::GestureKind::Wheel,
            Rc::new(|e: &Event| {
                e.stop();
                None
            }),
        )
}

/// The preview pane takes the wheel and hands it to the painter that owns it.
///
/// A viewport would chain the wheel itself, and this band will be one when its
/// content moves. Until then the pane has no window in the tree, so the notch
/// has to be delivered rather than chained — which is exactly what
/// `chrome:prompt_preview` was: a rectangle whose only job was to know which
/// pane the pointer was over.
fn preview(n: Node<UiMsg>) -> Node<UiMsg> {
    fresh_ui::gesture(n).on(
        fresh_ui::GestureKind::Wheel,
        Rc::new(|e: &Event| {
            e.stop();
            Some(UiMsg::Ui(UiFact::CardPreviewScroll(e.delta)))
        }),
    )
}

thread_local! {
    static CARD_KEY: fresh_ui::Key = fresh_ui::Key::Str("overlay_prompt_card".into());
}

/// Each band's rectangle, read off a tree the caller already laid out.
///
/// The partner of [`super::frame::regions_of`]. Bands that are not showing
/// report an empty rectangle rather than being absent, so a caller never has to
/// ask whether one exists before asking where it is.
pub fn regions_of(ui: &fresh_ui::Ui<UiMsg>) -> Vec<(CardRegion, ratatui::layout::Rect)> {
    CardRegion::ALL
        .into_iter()
        .map(|r| {
            let rect = ui
                .find_by_key(&region_key(r))
                .map(|id| ui.rect_of(id))
                .unwrap_or_default();
            (
                r,
                ratatui::layout::Rect {
                    x: rect.x.max(0) as u16,
                    y: rect.y.max(0) as u16,
                    width: rect.w,
                    height: rect.h,
                },
            )
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use fresh_ui::{Size, Ui};

    fn laid_out(c: &Card) -> Vec<(CardRegion, ratatui::layout::Rect)> {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(col().child(card(c)), Size::new(200, 60));
        regions_of(&ui)
    }

    fn at(rs: &[(CardRegion, ratatui::layout::Rect)], r: CardRegion) -> ratatui::layout::Rect {
        rs.iter().find(|(k, _)| *k == r).unwrap().1
    }

    /// A toolbar of `rows` toggles, one per row — the shape the tests count
    /// bands by. `0` is no toolbar at all.
    fn toolbar_of(rows: u16) -> Option<super::super::panel::Interior> {
        use fresh_core::api::WidgetSpec;
        if rows == 0 {
            return None;
        }
        let children = (0..rows)
            .map(|i| WidgetSpec::Toggle {
                checked: false,
                label: format!("T{i}"),
                focused: false,
                indeterminate: false,
                label_first: false,
                label_width: 0,
                key: Some(format!("t{i}")),
            })
            .collect();
        Some(super::super::panel::Interior {
            spec: Rc::new(WidgetSpec::Col {
                children,
                key: None,
            }),
            states: Rc::new(Default::default()),
            focus_key: String::new(),
            keyboard: true,
            page: None,
            hovered_key: None,
            hovered_item_key: String::new(),
            hovered_popup_row: String::new(),
            marker_gutter: false,
            avail_height: None,
            scrollbar_reveal: None,
            keymap: None,
            markdown: None,
        })
    }

    /// A card whose header band holds `toolbar_rows` rows.
    fn card_of(at: Rect, toolbar_rows: u16, footer: bool) -> Card {
        Card {
            at,
            toolbar: toolbar_of(toolbar_rows),
            title_row: false,
            footer,
            search: false,
            input_focused: true,
            ..Card::default()
        }
    }

    /// **The header band's height is the sum of its rows, not a constant.**
    ///
    /// `render_overlay_prompt` said `header_h = 2 + toolbar_h` and, forty lines
    /// earlier, `chrome_rows = 4 + toolbar_rows + footer` for the same card —
    /// two spellings of one fact, and the second one also had to know about the
    /// border and the footer. Stacked, the sum is what stacking does.
    #[test]
    fn the_toolbar_pushes_the_body_down_by_its_own_height() {
        let base = card_of(Rect::new(10, 4, 150, 40), 0, false);
        let tall = Card {
            toolbar: toolbar_of(3),
            ..base.clone()
        };
        let a = laid_out(&base);
        let b = laid_out(&tall);
        assert_eq!(
            at(&b, CardRegion::Results).y - at(&a, CardRegion::Results).y,
            3,
            "three toolbar rows move the results down three"
        );
        assert_eq!(
            at(&b, CardRegion::Results).height + 3,
            at(&a, CardRegion::Results).height,
            "and take those rows out of the body"
        );
    }

    /// **The footer takes its row off the bottom of the body.**
    #[test]
    fn a_footer_shortens_the_body_by_one_row() {
        let base = card_of(Rect::new(10, 4, 150, 40), 2, false);
        let with = Card {
            footer: true,
            ..base.clone()
        };
        let a = laid_out(&base);
        let b = laid_out(&with);
        assert_eq!(
            at(&b, CardRegion::Results).height + 1,
            at(&a, CardRegion::Results).height
        );
        assert_eq!(at(&b, CardRegion::Footer).height, 1);
        assert_eq!(at(&a, CardRegion::Footer).height, 0, "no footer, no row");
    }

    /// **Below the threshold the results take the whole body, and the preview
    /// still has a rectangle.**
    ///
    /// A hidden band reports an empty rect rather than nothing, so the wheel
    /// router asks where the preview is without first asking whether there is
    /// one — the rule `frame_tree` states for a hidden row.
    #[test]
    fn a_narrow_card_hides_the_preview_without_losing_it() {
        let wide = laid_out(&card_of(
            Rect::new(0, 0, PREVIEW_MIN_COLS + 10, 30),
            1,
            false,
        ));
        let narrow = laid_out(&card_of(
            Rect::new(0, 0, PREVIEW_MIN_COLS - 1, 30),
            1,
            false,
        ));
        assert!(
            at(&wide, CardRegion::Preview).width > 0,
            "wide enough for a preview"
        );
        // Half and half, less the rule between them: the preview's band is
        // the host beside its left-edge rule, so the column the rule occupies
        // comes off the preview's side (`preview_pane`).
        let body_w = PREVIEW_MIN_COLS + 10 - 2;
        assert_eq!(
            at(&wide, CardRegion::Results).width,
            body_w / 2,
            "the results take half the body"
        );
        assert_eq!(
            at(&wide, CardRegion::Preview).width,
            body_w - body_w / 2 - 1,
            "and the preview the rest, less its rule"
        );
        assert_eq!(at(&narrow, CardRegion::Preview).width, 0);
        assert_eq!(
            at(&narrow, CardRegion::Results).width,
            PREVIEW_MIN_COLS - 1 - 2,
            "the results take the whole body, inside the ring"
        );
    }

    /// **The bands are what `render_overlay_prompt` computed.**
    ///
    /// The painter's own arithmetic, written out here and compared against the
    /// description over a sweep of card sizes and band counts. The same
    /// standing proof `ui_shell_frame_parity` is for the frame: two
    /// derivations, one of which is about to be deleted, and this is what says
    /// the deletion changes nothing.
    ///
    /// ```text
    ///   inner    = card - border(1)
    ///   header_h = 2 + toolbar_h            (input + toolbar + separator)
    ///   footer_h = 1 when the plugin set one
    ///   body     = inner.y + header_h, height inner.h - header_h - footer_h
    ///   results  = body.width / 2 when card.w >= 120, else all of body
    /// ```
    #[test]
    fn the_bands_match_the_painters_own_arithmetic() {
        for w in [60u16, 100, 119, 120, 121, 160, 199] {
            for h in [10u16, 24, 40, 59] {
                for toolbar_rows in [0u16, 1, 3] {
                    for footer in [false, true] {
                        let c = card_of(Rect::new(1, 1, w, h), toolbar_rows, footer);
                        let rs = laid_out(&c);

                        // The painter's version.
                        let inner = ratatui::layout::Rect {
                            x: 2,
                            y: 2,
                            width: w.saturating_sub(2),
                            height: h.saturating_sub(2),
                        };
                        let header_h = 2 + toolbar_rows;
                        let footer_h = u16::from(footer);
                        let body = ratatui::layout::Rect {
                            x: inner.x,
                            y: inner.y.saturating_add(header_h),
                            width: inner.width,
                            height: inner.height.saturating_sub(header_h + footer_h),
                        };
                        let show_preview = w >= PREVIEW_MIN_COLS && body.height > 0;
                        let want_results = if show_preview {
                            ratatui::layout::Rect {
                                width: body.width / 2,
                                ..body
                            }
                        } else {
                            body
                        };

                        let got = at(&rs, CardRegion::Results);
                        assert_eq!(
                            got, want_results,
                            "results for {w}x{h} toolbar={toolbar_rows} footer={footer}"
                        );
                        if show_preview {
                            // The band is the pane inside its rule: one
                            // column in from the painter's outer area.
                            let want_preview = ratatui::layout::Rect {
                                x: body.x + body.width / 2 + 1,
                                y: body.y,
                                width: (body.width - body.width / 2).saturating_sub(1),
                                height: body.height,
                            };
                            assert_eq!(
                                at(&rs, CardRegion::Preview),
                                want_preview,
                                "preview for {w}x{h} toolbar={toolbar_rows} footer={footer}"
                            );
                        }
                    }
                }
            }
        }
    }

    /// **The card paints its own ring and caption.** The border is the box's,
    /// in the card's ink, at the card's rectangle; the caption's runs sit one
    /// cell in from the corner on the top edge.
    #[test]
    fn the_card_paints_its_ring_and_its_caption() {
        use fresh_ui::Draw;
        let c = Card {
            title: vec![Run::plain("Hints")],
            ..card_of(Rect::new(10, 4, 150, 40), 0, false)
        };
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(col().child(card(&c)), Size::new(200, 60));
        let spec = ui.spec();
        assert!(
            spec.items
                .iter()
                .any(|i| matches!(i.draw, Draw::Border(_)) && i.rect == Rect::new(10, 4, 150, 40)),
            "the ring at the card's rectangle"
        );
        let caption = spec
            .items
            .iter()
            .find(|i| matches!(&i.draw, Draw::Lines(l) if l.join("") == "Hints"))
            .expect("the caption is painted");
        assert_eq!((caption.rect.x, caption.rect.y), (11, 4), "on the top edge");
        assert!(
            spec.items
                .iter()
                .any(|i| matches!(i.draw, Draw::Scrim(Scrim::Dim))),
            "and everything behind it recedes"
        );
    }

    /// **The input row carries the caret.** The message, then the query with
    /// the caret stated inside it, so the display list's cursor is the cell
    /// after the typed text — and none when a toolbar control has the
    /// keyboard.
    #[test]
    fn the_input_row_places_the_caret_after_the_query() {
        let mut c = Card {
            input: super::super::prompt_line::PromptRow {
                message: "Grep: ".into(),
                input: "abc".into(),
                cursor: 3,
                selection: None,
                dir: None,
            },
            status: "Searching…".into(),
            count: Some((1, 9)),
            ..card_of(Rect::new(10, 4, 150, 40), 0, false)
        };
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(col().child(card(&c)), Size::new(200, 60));
        assert_eq!(
            ui.spec().cursor.map(|k| (k.pos.x, k.pos.y)),
            Some((11 + 6 + 3, 5)),
            "after the message and the query, inside the ring"
        );
        let text = |ui: &Ui<UiMsg>| -> String {
            let mut cells: Vec<(i32, String)> = ui
                .spec()
                .items
                .iter()
                .filter(|i| i.rect.y == 5)
                .filter_map(|i| match &i.draw {
                    fresh_ui::Draw::Lines(l) => Some((i.rect.x, l.join(""))),
                    _ => None,
                })
                .collect();
            cells.sort_by_key(|(x, _)| *x);
            cells.into_iter().map(|(_, s)| s).collect()
        };
        let row = text(&ui);
        assert!(row.starts_with("Grep: abc"), "{row:?}");
        assert!(row.trim_end().ends_with("Searching…  1 / 9"), "{row:?}");

        c.input_focused = false;
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(col().child(card(&c)), Size::new(200, 60));
        assert_eq!(ui.spec().cursor, None, "a toolbar control has the keyboard");
    }

    /// A plugin's styled text becomes runs the shell grammar reads: a theme
    /// key names the entry, a literal colour is a literal, attributes ride
    /// along, and an unstyled segment paints in the node's own ink.
    #[test]
    fn styled_text_becomes_runs_in_the_shells_names() {
        use fresh_core::api::{OverlayColorSpec, OverlayOptions, StyledText};
        let segs = vec![
            StyledText {
                text: "plain".into(),
                style: None,
            },
            StyledText {
                text: "key".into(),
                style: Some(OverlayOptions {
                    fg: Some(OverlayColorSpec::ThemeKey("ui.help_key_fg".into())),
                    bold: true,
                    ..OverlayOptions::default()
                }),
            },
            StyledText {
                text: "lit".into(),
                style: Some(OverlayOptions {
                    fg: Some(OverlayColorSpec::Rgb(1, 2, 3)),
                    ..OverlayOptions::default()
                }),
            },
        ];
        let runs = styled_runs(&segs, "ui.prompt_fg", "ui.suggestion_bg");
        assert_eq!(runs[0], Run::plain("plain"));
        let key = runs[1]
            .theme
            .as_ref()
            .map(|t| t.as_str().to_string())
            .unwrap();
        assert!(
            key.contains("ui.help_key_fg") && key.contains("ui.suggestion_bg"),
            "{key}"
        );
        assert!(key.contains("bold"), "{key}");
        let lit = runs[2]
            .theme
            .as_ref()
            .map(|t| t.as_str().to_string())
            .unwrap();
        assert!(!lit.contains("ui.prompt_fg"), "{lit}");
    }

    /// **Every band sits inside the ring.** The painter took `block.inner(..)`
    /// once and every row below it inherited the inset; here `border()` is what
    /// says it, and it clips as well as insets.
    #[test]
    fn the_bands_are_inside_the_border() {
        let c = card_of(Rect::new(10, 4, 150, 40), 2, true);
        let rs = laid_out(&c);
        for r in CardRegion::ALL {
            let b = at(&rs, r);
            if b.width == 0 || b.height == 0 {
                continue;
            }
            assert!(b.x >= 11, "{r:?} clears the left ring: {b:?}");
            assert!(b.y >= 5, "{r:?} clears the top ring: {b:?}");
            assert!(b.right() <= 10 + 150 - 1, "{r:?} clears the right: {b:?}");
            assert!(b.bottom() <= 4 + 40 - 1, "{r:?} clears the bottom: {b:?}");
        }
    }
}
