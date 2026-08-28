//! The floating-overlay prompt's card, as a description.
//!
//! The card is where the *other* suggestion list lives — the one
//! `SuggestionsRenderer` still draws — and the reason it still draws it is that
//! the list's rectangle is computed by this card's painter, halfway through the
//! frame, long after the shell tree has been laid out. So the card's rows have
//! to move before its list can.
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

use fresh_ui::{col, host, row, text, Node, Rect, Sizing};

use crate::app::shell_host::shell_theme::pair;

use super::msg::UiMsg;

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
}

/// Below this many columns the preview is hidden and the results take the whole
/// body. The design doc's §5, "preview pane size when terminal is narrow".
pub const PREVIEW_MIN_COLS: u16 = 120;

/// What the card is showing. Content and counts — never a rectangle, except the
/// one the card sits in.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Card {
    /// Where `centered_overlay_rect` put the card.
    pub at: Rect,
    /// How many rows the plugin's toolbar renders to at this width. Measured
    /// by the widget runtime, which is the only thing that knows: a toolbar is
    /// two rows on a wide terminal and wraps to more on a narrow one.
    pub toolbar_rows: u16,
    /// Whether the plugin set a footer.
    pub footer: bool,
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

fn region(r: CardRegion) -> Node<UiMsg> {
    host(r.id()).key(region_key(r))
}

/// The card, as a layer over the chrome column.
///
/// Anchored at the corner `centered_overlay_rect` chose and sized to it — the
/// same shape [`super::prompt::Place::Inside`] uses, and for the same reason:
/// the rectangle is somebody else's answer and this only occupies it.
pub fn card(c: &Card) -> Node<UiMsg> {
    use fresh_ui::{layer, Anchor, Place};
    layer()
        .key(CARD_KEY.with(|k| k.clone()))
        .anchor(Anchor::Point(c.at.x.max(0) as u16, c.at.y.max(0) as u16))
        .place(Place::Over)
        .child(
            body(c)
                .w(Sizing::Cells(c.at.w))
                .h(Sizing::Cells(c.at.h))
                .border()
                .theme(pair("ui.popup_border_fg", "ui.suggestion_bg")),
        )
}

/// The card's bands, top to bottom.
fn body(c: &Card) -> Node<UiMsg> {
    // The separator closing the header band. Native already: it is a row of
    // one glyph in one style, and `Sizing::Auto` on a text run would measure
    // the string, so it is the fill of a themed row that draws it edge to
    // edge — the same way the painter's `"─".repeat(inner.width)` did, without
    // needing to know the width.
    let separator = row()
        .h(Sizing::Cells(1))
        .theme(pair("ui.popup_border_fg", "ui.suggestion_bg"))
        .child(text("").flex(1));
    let middle = match c.preview() {
        // Half and half — but *which* half gets the odd column is not a
        // detail. `body.width / 2` truncates, so the painter's results pane
        // was the narrower one at an odd width and the preview took the spare
        // column; two equal `flex(1)` children hand it to the first child
        // instead, which the parity sweep caught at 121 columns. `Pct(50)`
        // truncates the same way the division did, and the preview takes what
        // is left.
        true => row().flex(1).children([
            region(CardRegion::Results).w(Sizing::Pct(50)),
            region(CardRegion::Preview).flex(1),
        ]),
        // The preview is still in the tree taking nothing, so it has a
        // rectangle to report and the results' own is unaffected — the rule
        // `frame_tree` states for a hidden region.
        false => row().flex(1).children([
            region(CardRegion::Results).flex(1),
            region(CardRegion::Preview).w(Sizing::Cells(0)),
        ]),
    };
    col().children([
        region(CardRegion::Input).h(Sizing::Cells(1)),
        region(CardRegion::Toolbar).h(Sizing::Cells(c.toolbar_rows)),
        separator,
        middle,
        region(CardRegion::Footer).h(Sizing::Cells(c.footer as u16)),
    ])
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

    /// **The header band's height is the sum of its rows, not a constant.**
    ///
    /// `render_overlay_prompt` said `header_h = 2 + toolbar_h` and, forty lines
    /// earlier, `chrome_rows = 4 + toolbar_rows + footer` for the same card —
    /// two spellings of one fact, and the second one also had to know about the
    /// border and the footer. Stacked, the sum is what stacking does.
    #[test]
    fn the_toolbar_pushes_the_body_down_by_its_own_height() {
        let base = Card {
            at: Rect::new(10, 4, 150, 40),
            toolbar_rows: 0,
            footer: false,
        };
        let tall = Card {
            toolbar_rows: 3,
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
        let base = Card {
            at: Rect::new(10, 4, 150, 40),
            toolbar_rows: 2,
            footer: false,
        };
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
        let wide = laid_out(&Card {
            at: Rect::new(0, 0, PREVIEW_MIN_COLS + 10, 30),
            toolbar_rows: 1,
            footer: false,
        });
        let narrow = laid_out(&Card {
            at: Rect::new(0, 0, PREVIEW_MIN_COLS - 1, 30),
            toolbar_rows: 1,
            footer: false,
        });
        assert!(
            at(&wide, CardRegion::Preview).width > 0,
            "wide enough for a preview"
        );
        assert_eq!(
            at(&wide, CardRegion::Results).width,
            at(&wide, CardRegion::Preview).width,
            "half and half"
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
                        let c = Card {
                            at: Rect::new(1, 1, w, h),
                            toolbar_rows,
                            footer,
                        };
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
                            let want_preview = ratatui::layout::Rect {
                                x: body.x + body.width / 2,
                                y: body.y,
                                width: body.width - body.width / 2,
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

    /// **Every band sits inside the ring.** The painter took `block.inner(..)`
    /// once and every row below it inherited the inset; here `border()` is what
    /// says it, and it clips as well as insets.
    #[test]
    fn the_bands_are_inside_the_border() {
        let c = Card {
            at: Rect::new(10, 4, 150, 40),
            toolbar_rows: 2,
            footer: true,
        };
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
