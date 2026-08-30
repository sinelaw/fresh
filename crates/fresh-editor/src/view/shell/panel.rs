//! The floating plugin panel's *frame*: where it goes, its ring, its title and
//! its `[×]`.
//!
//! The panel's interior is the widget runtime's — nineteen `WidgetSpec`
//! variants painted by `render_floating_widget_panel` — and stays that way
//! until C.1. What was never the runtime's is the box around it, and the
//! painter owned that too: it derived the rectangle from a width percentage
//! and a content row count, drew a `Block`, wrote `[×]` into the top border,
//! and then **filed that button's rectangle in `close_button_rect` so a mouse
//! handler could compare against it**. That last step is the migration's
//! signature defect — geometry computed by a painter, recorded, and hit-tested
//! later — and it is what this module removes.
//!
//! **What stays with the painter, deliberately.** The dimming pass. A scrim is
//! the tree's answer and this layer could carry one, but the dock's own panel
//! is painted *after* the tree's overlay band, so a scrim declared here would
//! be overpainted by the dock and the frame would read half-dimmed. The dock's
//! content is C.5b; the scrim goes when it does, and until then the painter's
//! two `apply_dimming` calls stay where they can still see the dock. Recorded
//! here rather than left to be rediscovered.

use std::rc::Rc;

use fresh_ui::{
    col, gesture, layer, row, text, Align, Anchor, Event, Fit, GestureKind, Key, MouseButton, Node,
    Place, PointerMode, Sizing,
};

use crate::app::shell_host::shell_theme::{attrs, pair};

use super::msg::{UiFact, UiMsg};

/// Where the panel sits, with the content measurements the placement needs.
///
/// The percentage and the counts are **content**, not geometry: a plugin
/// mounts at `{widthPct, heightPct}` because it does not know how tall its
/// content will be, and only the editor can count the rows the spec produced.
/// What the tree does with them is the arithmetic that used to be the
/// painter's.
///
/// `heightPct` is absent on purpose. The painter passed it to
/// `centered_overlay_rect` and then threw the resulting height away — the box
/// is as tall as its content, clamped to the frame, and has been since the
/// fit-to-content fix. Carrying it here would carry a value nothing reads.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Spot {
    /// Centred in its bounds: as wide as the request, as tall as the content.
    Centered {
        width_pct: u8,
        /// Rows the spec produced, borders excluded.
        content_rows: u16,
    },
    /// An unobtrusive context-menu popup at an absolute screen cell. It hugs
    /// its items rather than taking a percentage, and it is clamped so the
    /// whole box stays on screen.
    Anchored {
        x: u16,
        y: u16,
        /// The widest entry, borders excluded.
        content_cols: u16,
        content_rows: u16,
    },
}

/// The panel's frame, with everything resolved from live state.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Panel {
    pub spot: Spot,
    /// Rendered into the top border when centred. An anchored popup wears no
    /// title, which is the painter's rule kept.
    pub title: Option<String>,
    /// Whether the `[×]` is offered — centred panels only. An anchored popup
    /// is dismissed by clicking away from it.
    pub closable: bool,
    /// A focused panel lights its ring with the accent, so exactly one chrome
    /// region wears it at a time.
    pub focused: bool,
    /// Whether the panel lays into the whole frame or into the chrome column
    /// beside the dock. The orchestrator's global modals opt into the former
    /// so they are not cramped into the region right of their own dock.
    pub fullscreen: bool,
}

/// The box itself. Its rectangle is what the painter used to call
/// `overlay_rect`.
pub fn key() -> Key {
    Key::Str("panel_frame".into())
}

/// The content area, for the interior painter to read its rectangle from.
/// What `last_inner_rect` recorded, derived instead.
pub fn body_key() -> Key {
    Key::Str("panel_body".into())
}

pub fn close_key() -> Key {
    Key::Str("panel_close".into())
}

impl Panel {
    /// The box's height, borders included. The request is a hint in this
    /// direction and always has been: shorter content shrinks the box, taller
    /// content grows it up to the frame.
    fn rows(&self) -> u16 {
        let content = match self.spot {
            Spot::Centered { content_rows, .. } | Spot::Anchored { content_rows, .. } => {
                content_rows
            }
        };
        content.saturating_add(2).max(3)
    }
}

/// The panel's frame as a layer.
///
/// **It does not claim the pointer**, and that is not an oversight: the
/// panel's whole channel is already claimed by `modal::layer`, which routes it
/// to the interior's own hit-testing. This layer is declared after that one,
/// so it is offered the pointer first, and every node of it but the `[×]` is
/// transparent — so the button takes its own press and everything else falls
/// through to the routing that was already there. One button migrates without
/// the interior having to.
///
/// `within` is how "beside the dock" is said. The painter expressed it by
/// being handed `chrome_area` instead of the frame; a layer that names the
/// region it may be placed inside says the same thing, in the place that does
/// the placing.
pub fn layer_for(p: &Panel) -> Node<UiMsg> {
    let l = layer();
    match &p.spot {
        Spot::Centered { width_pct, .. } => {
            let l = match p.fullscreen {
                true => l,
                false => l.within(super::frame::chrome_key()),
            };
            l.anchor(Anchor::Screen(Align::Center))
                .place(Place::Over)
                .child(
                    frame_box(p)
                        .w(Sizing::Pct(*width_pct))
                        .min_w(20)
                        .h(Sizing::Cells(p.rows()))
                        .key(key()),
                )
        }
        // Full-frame whatever the dock is doing: the anchor is an absolute
        // screen cell that may sit over the dock column.
        Spot::Anchored {
            x, y, content_cols, ..
        } => l
            .anchor(Anchor::Point(*x, *y))
            .place(Place::Over)
            .fit(Fit::CLAMP)
            .child(
                frame_box(p)
                    .w(Sizing::Cells(content_cols.saturating_add(2).max(6)))
                    .h(Sizing::Cells(p.rows()))
                    .key(key()),
            ),
    }
}

/// The ring, its ground, and the strip that sits on its top edge.
fn frame_box(p: &Panel) -> Node<UiMsg> {
    let ring = ring_theme(p);
    let framed = col()
        .theme(ring)
        .border()
        .pointer_mode(PointerMode::Transparent)
        .child(body());
    fresh_ui::stack()
        .pointer_mode(PointerMode::Transparent)
        .children([framed, border_strip(p)])
}

fn ring_theme(p: &Panel) -> String {
    // The same accent the file explorer's focused border wears.
    match p.focused {
        true => pair("ui.cursor", "ui.suggestion_bg"),
        false => pair("ui.popup_border_fg", "ui.suggestion_bg"),
    }
}

/// The top border's overlay: the title where `Block::title` put it, and `[×]`
/// where the painter's `overlay_rect.width - 4` put it.
///
/// Transparent all the way down, container included, except the button — the
/// hit walk stops at the first child that blocks, so one opaque cell here
/// hides the interior behind the whole strip.
fn border_strip(p: &Panel) -> Node<UiMsg> {
    let centred = matches!(p.spot, Spot::Centered { .. });
    let clear = |n: Node<UiMsg>| n.pointer_mode(PointerMode::Transparent);
    let mut cells: Vec<Node<UiMsg>> = vec![
        // `Block::title` starts one cell in from the corner.
        clear(row().w(Sizing::Cells(1))),
        clear(match (centred, p.title.as_deref()) {
            (true, Some(t)) => text(format!(" {t} ")).theme(attrs(
                "ui.popup_border_fg",
                "ui.suggestion_bg",
                &["bold"],
            )),
            _ => row().w(Sizing::Cells(0)),
        }),
        clear(row().flex(1)),
    ];
    if centred && p.closable {
        cells.push(close_button(p));
        // The painter left the last column clear.
        cells.push(clear(row().w(Sizing::Cells(1))));
    }
    col().pointer_mode(PointerMode::Transparent).children([
        row()
            .h(Sizing::Cells(1))
            .pointer_mode(PointerMode::Transparent)
            .children(cells),
        clear(row().flex(1)),
    ])
}

/// The `[×]`, answering its own press.
///
/// It dismisses exactly as Esc and Cancel do — the same
/// `dismiss_floating_panel_with_cancel` path, which fires the panel's `cancel`
/// widget event. The old arm checked this rectangle *before* the general panel
/// hit-test so the click could not also focus a widget underneath; the node
/// stops the event, which is the same statement without the ordering.
fn close_button(p: &Panel) -> Node<UiMsg> {
    gesture(text("[×]").theme(ring_theme(p)))
        .key(close_key())
        .on(
            GestureKind::Press,
            Rc::new(|ev: &Event| {
                if ev.button != MouseButton::Left {
                    return None;
                }
                ev.stop();
                Some(UiMsg::Ui(UiFact::PanelClosed))
            }),
        )
}

/// The content area, transparent: the interior is a painter that hit-tests
/// itself through `UiFact::ModalPointer`, the same seam the other modal
/// interiors use.
fn body() -> Node<UiMsg> {
    row()
        .flex(1)
        .pointer_mode(PointerMode::Transparent)
        .key(body_key())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::view::shell::frame::{frame_tree, Frame};
    use fresh_ui::{Input, Mods, MouseButton, Point, Size, Ui};
    use ratatui::layout::Rect;

    const FRAME: Rect = Rect {
        x: 0,
        y: 0,
        width: 100,
        height: 30,
    };

    /// The arithmetic this module replaced, kept verbatim as the oracle.
    ///
    /// Copied out of `render_floating_widget_panel` rather than paraphrased:
    /// a parity test whose oracle is a re-derivation of what the code now does
    /// proves the code agrees with itself.
    mod painter {
        use ratatui::layout::Rect;

        pub fn centered_overlay_rect(area: Rect, width_pct: u8, height_pct: u8) -> Rect {
            let w_pct = width_pct.clamp(1, 100) as u32;
            let h_pct = height_pct.clamp(1, 100) as u32;
            let w = ((area.width as u32 * w_pct) / 100) as u16;
            let h = ((area.height as u32 * h_pct) / 100) as u16;
            let w = w.max(20).min(area.width);
            let h = h.max(8).min(area.height);
            Rect {
                x: area.x + (area.width.saturating_sub(w)) / 2,
                y: area.y + (area.height.saturating_sub(h)) / 2,
                width: w,
                height: h,
            }
        }

        pub fn centered(area: Rect, width_pct: u8, rows: u16) -> Rect {
            let requested = centered_overlay_rect(area, width_pct, 50);
            let needed_h = rows.saturating_add(2);
            let effective_h = needed_h.clamp(3, area.height.max(3));
            Rect {
                x: requested.x,
                y: area.y + (area.height.saturating_sub(effective_h)) / 2,
                width: requested.width,
                height: effective_h,
            }
        }

        pub fn anchored(area: Rect, x: u16, y: u16, cols: u16, rows: u16) -> Rect {
            let w = cols.saturating_add(2).clamp(6, area.width);
            let h = rows.saturating_add(2).clamp(3, area.height);
            let max_x = area.x + area.width.saturating_sub(w);
            let max_y = area.y + area.height.saturating_sub(h);
            Rect {
                x: x.clamp(area.x, max_x),
                y: y.clamp(area.y, max_y),
                width: w,
                height: h,
            }
        }
    }

    fn panel(spot: Spot) -> Panel {
        Panel {
            spot,
            title: Some("A Dialog".into()),
            closable: true,
            focused: true,
            fullscreen: true,
        }
    }

    fn laid_out(p: Option<Panel>) -> Ui<UiMsg> {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            frame_tree(Frame {
                panel: p,
                // The modal slot the panel's pointer routing rides, as `render`
                // sets it whenever a panel is mounted.
                modal: Some(super::super::modal::Slot::FloatingPanel),
                ..Frame::default()
            }),
            Size::new(FRAME.width, FRAME.height),
        );
        ui
    }

    fn rect(ui: &Ui<UiMsg>, k: &Key) -> Option<Rect> {
        super::super::rect_of(ui, k, FRAME)
    }

    /// **The box lands where the painter put it.** Every shape the painter had
    /// a branch for: content shorter than the request, content taller than the
    /// frame, and the narrow-width floor.
    #[test]
    fn a_centred_panel_is_placed_where_the_arithmetic_put_it() {
        for (pct, rows) in [(50u8, 6u16), (90, 3), (30, 40), (10, 5), (100, 1)] {
            let ui = laid_out(Some(panel(Spot::Centered {
                width_pct: pct,
                content_rows: rows,
            })));
            assert_eq!(
                rect(&ui, &key()),
                Some(painter::centered(FRAME, pct, rows)),
                "centred at {pct}% with {rows} rows"
            );
        }
    }

    /// And an anchored popup, including both clamps — a cell near the right
    /// edge and one near the bottom, where the box would otherwise hang off.
    #[test]
    fn an_anchored_panel_is_clamped_the_way_the_painter_clamped_it() {
        for (x, y, cols, rows) in [
            (10u16, 5u16, 12u16, 4u16),
            (95, 5, 12, 4),
            (10, 28, 12, 4),
            (99, 29, 30, 10),
            (0, 0, 2, 1),
        ] {
            let ui = laid_out(Some(panel(Spot::Anchored {
                x,
                y,
                content_cols: cols,
                content_rows: rows,
            })));
            assert_eq!(
                rect(&ui, &key()),
                Some(painter::anchored(FRAME, x, y, cols, rows)),
                "anchored at ({x},{y}) sized {cols}x{rows}"
            );
        }
    }

    /// The content area is the box less its ring — what `Block::inner` gave the
    /// painter, derived from the same layout that placed the box rather than
    /// asked of a second widget.
    #[test]
    fn the_content_area_is_the_box_less_its_border() {
        let ui = laid_out(Some(panel(Spot::Centered {
            width_pct: 60,
            content_rows: 8,
        })));
        let box_rect = rect(&ui, &key()).expect("a box");
        let body = rect(&ui, &body_key()).expect("a content area");
        assert_eq!(
            body,
            Rect {
                x: box_rect.x + 1,
                y: box_rect.y + 1,
                width: box_rect.width - 2,
                height: box_rect.height - 2,
            }
        );
    }

    /// **`[×]` where `overlay_rect.width - 4` put it**, which is the rectangle
    /// the painter filed and a mouse arm compared against.
    #[test]
    fn the_close_button_sits_where_the_painter_recorded_it() {
        let ui = laid_out(Some(panel(Spot::Centered {
            width_pct: 60,
            content_rows: 8,
        })));
        let box_rect = rect(&ui, &key()).expect("a box");
        assert_eq!(
            rect(&ui, &close_key()),
            Some(Rect {
                x: box_rect.x + box_rect.width - 4,
                y: box_rect.y,
                width: 3,
                height: 1,
            })
        );
    }

    fn facts(d: fresh_ui::Dispatch<UiMsg>) -> Vec<UiFact> {
        d.msgs
            .into_iter()
            .filter_map(|m| match m {
                UiMsg::Ui(f) => Some(f),
                _ => None,
            })
            .collect()
    }

    /// **The button answers its own press, and it wins.** The panel's whole
    /// channel is claimed by the modal layer underneath; this is the one cell
    /// that is not the interior's, and the ordering that used to be a comment
    /// ("checked BEFORE the general panel hit-test") is now the tree's.
    #[test]
    fn a_press_on_the_close_button_is_the_buttons_and_not_the_interiors() {
        let mut ui = laid_out(Some(panel(Spot::Centered {
            width_pct: 60,
            content_rows: 8,
        })));
        let cb = rect(&ui, &close_key()).expect("a button");
        let got = facts(ui.dispatch(Input::press(
            Point::new(cb.x as i32 + 1, cb.y as i32),
            MouseButton::Left,
            Mods::NONE,
        )));
        assert_eq!(got, vec![UiFact::PanelClosed]);
    }

    /// And a press one cell to the left of it is the interior's, as every
    /// other cell of the panel is. This is the assertion that would fail if
    /// the strip were opaque — the failure the popup wave already paid for
    /// once, where one solid title cell hid the whole frame behind it.
    #[test]
    fn a_press_beside_the_close_button_falls_through_to_the_interior() {
        let mut ui = laid_out(Some(panel(Spot::Centered {
            width_pct: 60,
            content_rows: 8,
        })));
        let cb = rect(&ui, &close_key()).expect("a button");
        let got = facts(ui.dispatch(Input::press(
            Point::new(cb.x as i32 - 2, cb.y as i32),
            MouseButton::Left,
            Mods::NONE,
        )));
        assert_eq!(
            got,
            vec![UiFact::ModalPointer(
                super::super::modal::Slot::FloatingPanel
            )],
            "the frame is decoration; only the button is not"
        );
    }

    /// A press in the middle of the content area, likewise — the interior
    /// hit-tests itself and must still be reached.
    #[test]
    fn a_press_in_the_content_area_reaches_the_interior() {
        let mut ui = laid_out(Some(panel(Spot::Centered {
            width_pct: 60,
            content_rows: 8,
        })));
        let body = rect(&ui, &body_key()).expect("a content area");
        let got = facts(ui.dispatch(Input::press(
            Point::new(
                body.x as i32 + body.width as i32 / 2,
                body.y as i32 + body.height as i32 / 2,
            ),
            MouseButton::Left,
            Mods::NONE,
        )));
        assert_eq!(
            got,
            vec![UiFact::ModalPointer(
                super::super::modal::Slot::FloatingPanel
            )]
        );
    }

    /// An anchored popup wears neither title nor button — the painter's rule,
    /// and the reason is that it is dismissed by clicking away from it.
    #[test]
    fn an_anchored_popup_has_no_close_button() {
        let ui = laid_out(Some(panel(Spot::Anchored {
            x: 10,
            y: 5,
            content_cols: 12,
            content_rows: 4,
        })));
        assert!(rect(&ui, &close_key()).is_none());
    }

    /// No panel, no box.
    #[test]
    fn no_panel_means_no_frame_in_the_tree() {
        let ui = laid_out(None);
        assert!(rect(&ui, &key()).is_none());
        assert!(rect(&ui, &body_key()).is_none());
    }
}
