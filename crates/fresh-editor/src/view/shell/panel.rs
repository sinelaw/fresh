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
#[derive(Clone, Debug)]
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
    /// **The interior, when it is described rather than painted.**
    ///
    /// `Some` when the panel's spec uses only variants
    /// `view::shell::widgets` describes; `None` sends the whole panel down
    /// the runtime's path, which is what still paints a `WindowEmbed`. A
    /// panel is one or the other and never half of each — see
    /// `widgets::covered`.
    pub interior: Option<Interior>,
}

/// A described interior: the spec, and the host state it reads.
#[derive(Clone, Debug)]
pub struct Interior {
    pub spec: std::rc::Rc<fresh_core::api::WidgetSpec>,
    pub states: std::rc::Rc<std::collections::HashMap<String, crate::widgets::WidgetInstanceState>>,
    pub focus_key: String,
    pub hovered_key: Option<String>,
    pub hovered_item_key: String,
    /// The open dropdown pop-over's hovered option, as a decimal index, or
    /// empty. See [`super::widgets::Ctx::hovered_popup_row`].
    pub hovered_popup_row: String,
    pub marker_gutter: bool,
    pub avail_height: Option<u32>,
    /// See [`super::widgets::Ctx::scrollbar_reveal`].
    pub scrollbar_reveal: Option<bool>,
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
/// **A focused panel's keyboard — confinement without a swallow.**
///
/// `chrome::Dock::on_layer_key` and `chrome::FloatingModal::on_layer_key`
/// were offered every key by the ranked overlay walk while their layer said
/// `owns_keyboard`, and both end in `dispatch_floating_widget_key`, which
/// **declines**: a shortcut the panel does not bind blurs the dock and falls
/// through to the editor's own resolution. That is the same shape the prompt
/// has, and the same [`fresh_ui::Modality::Focus`] says it — see
/// `super::prompt::keys_layer` for the two halves and why the claim is
/// completed host-side.
///
/// What the ranks said, the frame's declaration order says: these are
/// declared under the overlay prompt's card and the popups
/// (`POPUP > FLOATING_MODAL > DOCK` — the R1 rank-inversion fix, kept), the
/// dock's under the floating panel's, and both under the menus and the modal
/// band. Nothing consults an integer.
///
/// Paints nothing and takes no pointer: the panel's frame, its `[×]` and its
/// described interior are all elsewhere, and every one of them answers its
/// own press.
pub fn keys_layer(slot: super::widgets::Slot) -> Node<UiMsg> {
    use fresh_ui::Modality;
    layer()
        .anchor(Anchor::Screen(Align::Start))
        .place(Place::Fill)
        .pointer_mode(PointerMode::Ignore)
        .modality(Modality::Focus)
        .child(
            fresh_ui::focusable(row())
                .pointer_mode(PointerMode::Ignore)
                .autofocus()
                .on_key(move |_| Some(UiMsg::Ui(UiFact::PanelKey(slot)))),
        )
}

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
///
/// **The box claims its own presses**, and it has to. Layers are hit-tested
/// top down and the *first one with any path at the point wins* — a
/// transparent node still produces a path — so a decorative layer over the
/// modal's claim-everything surface does not fall through to it, it swallows
/// the press and nothing handles it. Only a press *outside* this box reaches
/// the layer below.
///
/// So the box says what a press on it means: the interior's, when the
/// interior is a painter that hit-tests itself through `UiFact::ModalPointer`;
/// nobody's, when the interior is described and its widgets have already
/// declined it. Either way the press stops here rather than reaching the
/// buffer behind.
fn frame_box(p: &Panel) -> Node<UiMsg> {
    let ring = ring_theme(p);
    let framed = col().theme(ring).border().child(body(p));
    let described = p.interior.is_some();
    let claim = move |e: &Event| {
        e.stop();
        match described {
            true => None,
            false => Some(UiMsg::Ui(UiFact::ModalPointer(
                super::modal::Slot::FloatingPanel,
            ))),
        }
    };
    let mut g = gesture(fresh_ui::stack().children([framed, border_strip(p)]));
    for kind in [GestureKind::Press, GestureKind::Release, GestureKind::Move] {
        g = g.on(kind, Rc::new(claim));
    }
    // **The wheel only when a painter is behind the seam.**
    //
    // Scrolling is framework-owned: the library runs its scroll chain for a
    // notch *nothing claimed*, so a catch-all that stops the wheel stops
    // every viewport inside this box from scrolling. That is fine while the
    // interior is a painter — the claim carries the notch to it through
    // `ModalPointer`, which is the whole seam — and wrong once the interior
    // is described, where the claim produces no message and the notch simply
    // disappears.
    //
    // Nothing is lost by letting it through: the chain stops at the first
    // out-of-flow node, and this box is inside a layer, so a notch that
    // scrolls nothing here is still absorbed rather than reaching the buffer
    // behind. `keybinding::swallow` had the same bug and the same fix.
    if !described {
        g = g.on(GestureKind::Wheel, Rc::new(claim));
    }
    g
}

fn ring_theme(p: &Panel) -> String {
    // The same accent the file explorer's focused border wears.
    match p.focused {
        true => pair("editor.cursor", "ui.suggestion_bg"),
        false => pair("ui.popup_border_fg", "ui.suggestion_bg"),
    }
}

/// The top border's overlay: the title where `Block::title` put it, and `[×]`
/// where the painter's `overlay_rect.width - 4` put it.
///
/// Transparent all the way down, container included, except the button — the
/// hit walk stops at the first child that blocks, so one opaque cell here
/// hides the interior behind the whole strip.
///
/// **And it is one row tall, which is the only row it has anything on.** It
/// used to be a column with a flexible filler under it, so the strip covered
/// the whole box; a transparent node still *produces a path*, and that path
/// runs up through the frame's catch-all gesture, which swallows what the
/// interior does not answer. Paths are tried topmost-first and the first one
/// to claim ends the dispatch — so the filler's path was offered before the
/// interior's, the catch-all took it, and **every press on a described
/// widget died one node short of the widget it landed on**. A strip that
/// ends where its content ends has no path below its row to offer.
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
    row()
        .h(Sizing::Cells(1))
        .pointer_mode(PointerMode::Transparent)
        .children(cells)
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

/// The content area.
///
/// **Transparent when the interior is a painter**, because that painter
/// hit-tests itself through `UiFact::ModalPointer` — the same seam the other
/// modal interiors use. **Opaque when it is described**, because then the
/// widgets answer their own presses and a press that reaches the area but no
/// widget is the panel's to swallow, not the buffer's.
fn body(p: &Panel) -> Node<UiMsg> {
    let area = row().flex(1).key(body_key());
    let Some(i) = p.interior.clone() else {
        return area.pointer_mode(PointerMode::Transparent);
    };
    // **The width the widgets are laid out at is layout's answer, not the
    // caller's.** A centred panel is a percentage of its bounds, so nobody
    // knows the content width until the box has been measured — and the
    // collectors need it as a number before they can render a row. That is
    // what `layout_reader` is for: build the subtree from the constraints the
    // node was given. The alternative is the caller computing the percentage
    // itself, which is the second layout this migration exists to remove.
    area.child(fresh_ui::layout_reader(
        move |info: fresh_ui::LayoutInfo| {
            super::widgets::node(
                &i.spec,
                info.constraints.max_w.max(1),
                &super::widgets::Ctx {
                    slot: super::widgets::Slot::Floating,
                    states: &i.states,
                    focus_key: i.focus_key.clone(),
                    hovered_key: i.hovered_key.clone(),
                    marker_gutter: i.marker_gutter,
                    hovered_item_key: i.hovered_item_key.clone(),
                    hovered_popup_row: i.hovered_popup_row.clone(),
                    avail_height: i.avail_height,
                    scrollbar_reveal: i.scrollbar_reveal,
                    surface: super::widgets::panel_surface(),
                },
            )
        },
    ))
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
            interior: None,
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

    /// **The interior is in the tree.** A described panel's widget rows are
    /// laid out inside the content area — not painted over it afterwards by a
    /// second pass — so they have rectangles the pointer can be tested
    /// against, which is the whole point of describing them.
    #[test]
    fn a_described_interior_lands_inside_the_content_area() {
        use fresh_core::api::WidgetSpec;
        let spec = WidgetSpec::Col {
            children: vec![WidgetSpec::Raw {
                entries: vec![
                    fresh_core::text_property::TextPropertyEntry::text("alpha"),
                    fresh_core::text_property::TextPropertyEntry::text("beta"),
                ],
                key: None,
            }],
            key: None,
        };
        let mut p = panel(Spot::Centered {
            width_pct: 60,
            content_rows: 4,
        });
        p.interior = Some(Interior {
            spec: std::rc::Rc::new(spec),
            states: Default::default(),
            focus_key: String::new(),
            hovered_key: None,
            hovered_item_key: String::new(),
            hovered_popup_row: String::new(),
            marker_gutter: false,
            avail_height: None,
            scrollbar_reveal: None,
        });
        let ui = laid_out(Some(p));
        let body = rect(&ui, &body_key()).expect("a content area");
        for text in ["alpha", "beta"] {
            // The panel is a layer, so its content is in the layer band —
            // which is the whole point: it is placed, not painted afterwards.
            let at = ui
                .spec()
                .in_flow()
                .iter()
                .chain(ui.spec().layers().iter())
                .find_map(|i| match &i.draw {
                    fresh_ui::Draw::Lines(l) if l.iter().any(|s| &**s == text) => Some(i.rect),
                    _ => None,
                })
                .unwrap_or_else(|| panic!("no row for {text}"));
            assert!(
                at.x >= body.x as i32
                    && at.y >= body.y as i32
                    && at.y < (body.y + body.height) as i32,
                "{text} at {at:?} is outside the content area {body:?}"
            );
        }
    }

    /// **A press on a described widget delivers that widget's hit.** The area
    /// keeping the press (below) and a widget answering it are two different
    /// facts, and only the first was asserted: the panel's frame wraps its
    /// whole box in one gesture that swallows what the interior does not
    /// answer, and if that gesture is reached before the widget's the press
    /// dies one node short of the thing it landed on.
    #[test]
    fn a_press_on_a_described_widget_delivers_its_hit() {
        use fresh_core::api::WidgetSpec;
        let spec = WidgetSpec::Button {
            label: "Go".into(),
            focused: false,
            intent: Default::default(),
            key: Some("go".into()),
            disabled: false,
            focusable: true,
            bare: false,
            full_width: false,
            hover_style: None,
        };
        let mut p = panel(Spot::Centered {
            width_pct: 60,
            content_rows: 4,
        });
        p.interior = Some(Interior {
            spec: std::rc::Rc::new(spec),
            states: Default::default(),
            focus_key: String::new(),
            hovered_key: None,
            hovered_item_key: String::new(),
            hovered_popup_row: String::new(),
            marker_gutter: false,
            avail_height: None,
            scrollbar_reveal: None,
        });
        let mut ui = laid_out(Some(p));
        let at = ui
            .spec()
            .in_flow()
            .iter()
            .chain(ui.spec().layers().iter())
            .find_map(|i| match &i.draw {
                fresh_ui::Draw::Lines(l) if l.iter().any(|s| s.contains("Go")) => Some(i.rect),
                _ => None,
            })
            .expect("the button paints");
        let got = facts(ui.dispatch(Input::press(
            Point::new(at.x + 1, at.y),
            MouseButton::Left,
            Mods::NONE,
        )));
        assert!(
            got.iter().any(|f| matches!(
                f,
                UiFact::WidgetHit { hit, .. } if hit.widget_key == "go"
            )),
            "a press on the button at {at:?} should be its hit, got {got:?}"
        );
    }

    /// A described panel's content area is **opaque**: the widgets answer
    /// their own presses, and one that reaches the area but no widget is the
    /// panel's to swallow. An undescribed one is transparent, because the
    /// painter behind it hit-tests itself through `ModalPointer`.
    #[test]
    fn a_described_content_area_does_not_let_presses_through() {
        use fresh_core::api::WidgetSpec;
        let mut p = panel(Spot::Centered {
            width_pct: 60,
            content_rows: 4,
        });
        p.interior = Some(Interior {
            spec: std::rc::Rc::new(WidgetSpec::Raw {
                entries: vec![fresh_core::text_property::TextPropertyEntry::text("x")],
                key: None,
            }),
            states: Default::default(),
            focus_key: String::new(),
            hovered_key: None,
            hovered_item_key: String::new(),
            hovered_popup_row: String::new(),
            marker_gutter: false,
            avail_height: None,
            scrollbar_reveal: None,
        });
        let mut ui = laid_out(Some(p));
        let body = rect(&ui, &body_key()).expect("a content area");
        let got = facts(ui.dispatch(Input::press(
            Point::new(body.x as i32 + 1, body.y as i32 + 1),
            MouseButton::Left,
            Mods::NONE,
        )));
        assert!(
            !got.iter().any(|f| matches!(f, UiFact::ModalPointer(_))),
            "the described area keeps the press, got {got:?}"
        );
    }

    /// No panel, no box.
    #[test]
    fn no_panel_means_no_frame_in_the_tree() {
        let ui = laid_out(None);
        assert!(rect(&ui, &key()).is_none());
        assert!(rect(&ui, &body_key()).is_none());
    }
}
