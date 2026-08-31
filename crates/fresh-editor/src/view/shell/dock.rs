//! The left dock column.
//!
//! The column's *content* is a plugin's `WidgetSpec`, laid out by the widget
//! runtime, so the region is still a `Host` leaf — that translation is the M6
//! wave and a plugin-API change with it. What moves here is everything around
//! the content: the column's own pointer surface, and the width grip on its
//! right edge.
//!
//! **The grip is a node, not a rectangle.** It was `chrome:dock_border`, a box
//! pushed *before* `chrome:dock_column` so that within a band the earlier push
//! won — a rule about the builder's insertion order standing in for "the grip
//! is on top of the column". Here it is on top of the column, because it is
//! declared after it in a [`stack`].
//!
//! **The blur observer is a capture-phase listener on the frame**, not a
//! full-frame box ranked above every consuming surface. Same contract — act,
//! then let the press go on to whatever it was aimed at — and it now holds for
//! presses the tree claims. It had stopped: the shell is offered the pointer
//! before the chrome walk, so once the menu bar, status bar, explorer and
//! popups became nodes, clicking any of them left a focused dock focused.
//!
//! What is *not* here: the dock's keys — they are a layer of their own
//! (`super::panel::keys_layer`, declared in the frame under everything that
//! outranks a focused dock) rather than a node of this column, because the
//! keyboard's owner is not the same question as where the column sits. Nor is
//! the scrollbar-reveal hover, which reads zones the plugin publishes from
//! inside the panel and belongs with the content.

use std::rc::Rc;

use fresh_ui::{
    gesture, host, row, stack, Event, GestureKind, Key, MouseButton, Node, PointerMode, Sizing,
};

use super::frame::HostRegion;
use super::msg::{UiFact, UiMsg};

/// The grip's key, for tests and for callers that want its rectangle.
pub fn grip_key() -> Key {
    Key::Str("dock_grip".into())
}

/// The column: the panel's interior under a pointer surface, with the width
/// grip on top of its right edge.
///
/// `interior` is the orchestrator's `WidgetSpec` as a description, when every
/// variant in it is one the adapter covers. `None` keeps the `Host` leaf the
/// painter fills — the same safety valve the floating panel has had since
/// M6a, and the reason this flip cannot half-land: `panel_interior` returns
/// `None` for an uncovered spec and the dock stays exactly as it was.
pub fn dock(
    interior: Option<super::panel::Interior>,
    grip_hovered: bool,
    focused: bool,
) -> Node<UiMsg> {
    let described = interior.is_some();
    stack().children([
        column(interior),
        grip_strip(grip_hovered, focused, described),
    ])
}

/// The panel's own pointer surface.
///
/// **What a left press means depends on what is under it.** While the
/// interior is a painter, the press reports *where* and the runtime
/// hit-tests its own boxes — the same seam the overlay prompt's toolbar band
/// sits on (`UiFact::CardToolbarPress`). Once the interior is described its
/// widgets answer their own presses and stop the flow, so what reaches here
/// is a press they declined: the column's dead space. That still focuses the
/// dock, which is the half of `DockPress` that was never about geometry —
/// `handle_floating_widget_click` already returns without doing anything
/// when its probe finds no widget.
///
/// **A right press reports the same fact either way, but not by the same
/// route.** The column emits `DockContext` on both sides of the seam; what
/// answers it differs. While the interior is a painter,
/// `handle_floating_widget_context_click` probes the runtime's boxes. Once it
/// is described that probe stands down — it would answer from a *second*
/// layout that reads the runtime's own scroll offset, which the description
/// does not, so a scrolled list would raise the menu for a different row from
/// the one clicked — and the menu comes from the widget's own
/// `UiFact::WidgetContext` instead.
fn column(interior: Option<super::panel::Interior>) -> Node<UiMsg> {
    let described = interior.is_some();
    let body = match interior {
        None => host(HostRegion::Dock.id()),
        Some(i) => fresh_ui::layout_reader(move |info: fresh_ui::LayoutInfo| {
            // **The interior is not the column.** The runtime lays this same
            // spec at `floating_panel_inner_width` — `width_cols - 2` for a
            // left dock — and the painter draws the divider into the column's
            // last cell. Handed the whole column, the description came out two
            // columns wider than the boxes `probe_floating_widget` resolves
            // hover and right-click against, and anything a `flexSpacer` pins
            // to the right edge — the title bar's `[×]` above all — was laid
            // out past the visible edge and clipped away entirely.
            let inner_w = info.constraints.max_w.saturating_sub(DIVIDER_COLS).max(1);
            super::widgets::node(
                &i.spec,
                inner_w,
                &super::widgets::Ctx {
                    slot: super::widgets::Slot::Dock,
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
            // **One width, laid and wrapped, and it now reaches the
            // divider.** The first attempt at the slack column widened this
            // `.w(...)` alone and left the number above it at the old value,
            // so the header rule — text of exactly the width it is passed —
            // stayed short while the title bar's `×` pinned to the new edge
            // (`dock_title_bar_close_button_hides_the_dock`). Both come from
            // `inner_w` now, and so do a hovered row's band and the overlay
            // bar's column.
            .w(Sizing::Cells(inner_w))
        }),
    };
    gesture(body)
        .on(
            GestureKind::Press,
            Rc::new(move |e: &Event| {
                let x = e.pos.x.max(0) as u16;
                let y = e.pos.y.max(0) as u16;
                match e.button {
                    MouseButton::Left => {
                        e.stop();
                        match described {
                            true => Some(UiMsg::Ui(UiFact::DockFocus)),
                            false => Some(UiMsg::Ui(UiFact::DockPress { x, y })),
                        }
                    }
                    MouseButton::Right => {
                        e.stop();
                        Some(UiMsg::Ui(UiFact::DockContext { x, y }))
                    }
                    _ => None,
                }
            }),
        )
        .on(
            GestureKind::Wheel,
            Rc::new(move |e: &Event| {
                // **A described interior scrolls itself.** Its sessions list
                // is a `viewport`, and the library chains a notch into one
                // only when *nothing claimed it* — so a catch-all that calls
                // `e.stop()` here is the whole reason the dock stopped
                // scrolling. Worse than nothing: `DockScroll` moved the
                // runtime's own `WidgetInstanceState::scroll_offset`, which
                // the description does not read but `probe_floating_widget`
                // still does, so a few notches put the hover highlight and the
                // right-click menu on a different row from the one drawn.
                //
                // The same ruling the settings dialog's wheel got. While the
                // interior is a painter there is no viewport to chain into and
                // the runtime's offset *is* the scroll, so that arm stays.
                if described {
                    return None;
                }
                e.stop();
                Some(UiMsg::Ui(UiFact::DockScroll {
                    delta: e.delta,
                    x: e.pos.x.max(0) as u16,
                    y: e.pos.y.max(0) as u16,
                }))
            }),
        )
        // **The overlay scrollbar's zone**, and the whole of what reveals it.
        // Enter and Leave fire on this node whenever the pointer crosses the
        // column's edge — a node stays hovered while the pointer is over any
        // of its descendants — so what the painter did by recording each
        // list's rectangle and testing every motion event against it is one
        // pair of listeners on the surface that already owns the column.
        .on(
            GestureKind::Enter,
            Rc::new(|_: &Event| Some(UiMsg::Ui(UiFact::DockHover(true)))),
        )
        .on(
            GestureKind::Leave,
            Rc::new(|_: &Event| Some(UiMsg::Ui(UiFact::DockHover(false)))),
        )
}

/// The column's last cell, which the painter draws the draggable divider into
/// and the interior therefore may not use.
///
/// **One, not the runtime's two.** `floating_panel_inner_width` takes two for a
/// left dock — the divider, and a column of slack it wraps against — and the
/// description took the same two so that a `flexSpacer` and a `divider()`
/// would agree about where the right edge is. They agreed by both stopping
/// short of it, which left the slack column empty: a hovered row's band ended
/// one column before the divider, and the overlay scrollbar sat inboard of it.
///
/// The disagreement was never about *this* number. It was that the first
/// attempt to reach the slack column widened only the laid node
/// (`.w(Sizing::Cells(inner_w + 1))`) and went on passing the old `inner_w`
/// as the wrap width — so the header rule, which is text of exactly the
/// number it is given, stayed short while the `×` pinned to the new edge by
/// flex. Two numbers, two edges. One number moves everything together, and
/// then the only column the description may not have is the painter's
/// divider itself.
const DIVIDER_COLS: u16 = 1;

/// One transparent column of the panel's width with the grip in its last cell.
///
/// The grip paints nothing: the column's right border is the legacy painter's,
/// and this only claims the pointer that lands on it — and then keeps it,
/// through every move and the release, because a resize drag leaves the grip's
/// own cell on its first step.
/// The divider the dock is dragged by, and what it looks like.
///
/// **It is the tree's now, for a described dock.** The painter drew it as a
/// `Block::borders(RIGHT)` from `render_floating_widget_panel`, which runs
/// *after* the overlay band folds — so with the settings box open over the
/// dock, this one column of the dock came back through the middle of the
/// dialog. The column is already this node's; drawing it here puts it in the
/// background band with the rest of the dock's content, where a modal covers
/// it like everything else.
///
/// The colours are the painter's: the accent `editor.cursor` while the dock
/// has focus (the same one the file explorer's focused border wears, so
/// exactly one region wears it at a time), and `ui.split_separator_hover_fg`
/// under the pointer — which is the affordance the grip did not have at all.
/// A column you can drag has to say so before you drag it.
///
/// While the interior is still a painter the border stays the painter's, so
/// this draws nothing but the hover: two nodes painting one cell is how they
/// drift apart.
///
/// **The one thing that interrupts it is the active card's tab** (F.8), and
/// it is not this node's business which rows those are. The dock's active
/// session reads as a browser tab merging into the editor: its card's rows
/// lose their right border and the divider is scooped away across them, `╯`
/// above and `╮` below. That band is the *card's*, and only the card can say
/// where it is — it moves with the tree's scroll, and a card scrolled half
/// out of the list has no tab at all — so the card declares the scoop itself,
/// anchored to its own block, as a layer over this column
/// (`widgets::tab_scoop`). This still draws the divider whole: the rule "the
/// dock has a wall down its last cell" is one fact with one author, and what
/// the tab says is the separate fact that the active card is open on that
/// side. The alternative — passing a row band from the interior to here —
/// would be the two halves of one rectangle computed twice, which is what
/// the painter did and what F.8 was.
fn grip_ink(hovered: bool, focused: bool, described: bool) -> Node<UiMsg> {
    use crate::app::shell_host::shell_theme::pair;
    let fg = match (hovered, focused) {
        (true, _) => "ui.split_separator_hover_fg",
        (false, true) => "editor.cursor",
        (false, false) => "ui.popup_border_fg",
    };
    if !hovered && !described {
        return row();
    }
    let ink = pair(fg, "editor.bg");
    fresh_ui::layout_reader(move |c: fresh_ui::LayoutInfo| {
        fresh_ui::col().children(
            (0..c.constraints.max_h)
                .map(|_| fresh_ui::text("│").theme(ink.clone()).h(Sizing::Cells(1))),
        )
    })
}

fn grip_strip(hovered: bool, focused: bool, described: bool) -> Node<UiMsg> {
    // The width and the key go on the OUTSIDE, on the gesture node `draggable`
    // returns: it is the node that hit-tests, and an unconstrained one would
    // stretch across the whole strip and swallow presses meant for the panel
    // beside it.
    let hover = |t: Option<crate::app::types::HoverTarget>| -> fresh_ui::Handler<UiMsg> {
        Rc::new(move |_: &Event| Some(UiMsg::Ui(UiFact::Hover(t.clone()))))
    };
    let grip = super::grip::draggable(
        super::msg::Grip::DockWidth,
        grip_ink(hovered, focused, described),
        Rc::new(|_: &Event| Some(UiMsg::Ui(UiFact::DockResizeBegin))),
    )
    .w(Sizing::Cells(1))
    .key(grip_key())
    .on_enter(hover(Some(crate::app::types::HoverTarget::DockBorder)))
    .on_leave(hover(None));
    row()
        .pointer_mode(PointerMode::Transparent)
        .children([row().flex(1).pointer_mode(PointerMode::Transparent), grip])
}

/// Blur the dock when a press lands outside its column.
///
/// Wraps the whole frame and listens in the **capture** phase, so it runs
/// before anything the press is actually aimed at and — because it never stops
/// the flow — leaves that press to go on and do its job. "Act, and claim only
/// when the act was the whole of it": the rule the wheel and the pass-through
/// dismissals already follow.
///
/// The width is the only thing "outside" needs, and the description has it.
/// Whether the dock is *focused* is app state, and stays with the applier.
pub fn blur_observer(width: u16, frame: Node<UiMsg>) -> Node<UiMsg> {
    gesture(frame).on_capture(
        GestureKind::Press,
        Rc::new(move |e: &Event| {
            (e.button == MouseButton::Left && e.pos.x >= width as i32)
                .then_some(UiMsg::Ui(UiFact::DockBlur))
        }),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::view::shell::frame::{frame_tree, region_key, Frame};
    use fresh_ui::{Input, Mods, Point, Size, Ui};

    fn laid_out(dock: Option<u16>, w: u16, h: u16) -> Ui<UiMsg> {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            frame_tree(Frame {
                menu_bar: false,
                status_bar: false,
                dock,
                ..Frame::default()
            }),
            Size::new(w, h),
        );
        ui
    }

    fn press(ui: &mut Ui<UiMsg>, x: u16, y: u16, b: MouseButton) -> Vec<UiFact> {
        ui.dispatch(Input::press(Point::new(x as i32, y as i32), b, Mods::NONE))
            .msgs
            .into_iter()
            .filter_map(|m| match m {
                UiMsg::Ui(f) => Some(f),
                _ => None,
            })
            .collect()
    }

    /// A dock whose spec the adapter covers, for the two tests that care
    /// which side of the seam the column is on.
    fn described(dock: Option<u16>, w: u16, h: u16) -> Ui<UiMsg> {
        use fresh_core::api::WidgetSpec;
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            frame_tree(Frame {
                menu_bar: false,
                status_bar: false,
                dock,
                dock_interior: Some(super::super::panel::Interior {
                    spec: Rc::new(WidgetSpec::Raw {
                        entries: Vec::new(),
                        key: None,
                    }),
                    states: Rc::new(Default::default()),
                    focus_key: String::new(),
                    hovered_key: None,
                    hovered_item_key: String::new(),
                    hovered_popup_row: String::new(),
                    marker_gutter: false,
                    avail_height: None,
                    scrollbar_reveal: None,
                }),
                ..Frame::default()
            }),
            Size::new(w, h),
        );
        ui
    }

    /// **A press on the column means two different things**, and which one
    /// depends on what is behind the seam.
    ///
    /// While the interior is a painter it has to say *where*, because the
    /// runtime hit-tests its own boxes with the cell. Once the interior is
    /// described the widgets answer their own presses, so a press that
    /// reaches the column is one they declined — dead space — and the only
    /// thing left to do with it is focus the dock.
    #[test]
    fn a_press_says_where_only_while_a_painter_is_behind_it() {
        let mut ui = laid_out(Some(24), 100, 30);
        assert_eq!(
            press(&mut ui, 10, 5, MouseButton::Left),
            vec![UiFact::DockPress { x: 10, y: 5 }],
            "the painter needs the cell to hit-test with"
        );

        let mut ui = described(Some(24), 100, 30);
        assert_eq!(
            press(&mut ui, 10, 5, MouseButton::Left),
            vec![UiFact::DockFocus],
            "the widgets answered their own; this is dead space"
        );
    }

    /// **The column says the same thing on either side of the seam.**
    ///
    /// This is about the *fact*, not the mechanism: `DockContext` carries the
    /// cell, and the column emits it described or not. What consumes it
    /// diverges — see `column`'s own doc, and
    /// `handle_floating_widget_context_click`'s early-out for a described
    /// panel — so this asserts what it can see and no more.
    #[test]
    fn a_right_press_reports_where_on_either_side_of_the_seam() {
        for mut ui in [described(Some(24), 100, 30), laid_out(Some(24), 100, 30)] {
            // `ClearTabMenus` rides along on either side: it is the
            // frame-wide right-click observer (`shell::splits::tab_menu_guard`)
            // firing wherever the click lands, not something the column says.
            // Filtered here for the same reason
            // `a_right_press_in_the_column_reports_where` filters it.
            let said: Vec<_> = press(&mut ui, 5, 7, MouseButton::Right)
                .into_iter()
                .filter(|f| *f != UiFact::ClearTabMenus)
                .collect();
            assert_eq!(said, vec![UiFact::DockContext { x: 5, y: 7 }]);
        }
    }

    /// The grip is the column's last cell, top to bottom.
    ///
    /// It was a `LayoutBox` at `(0, width - 1, 1, frame_height)`, arithmetic
    /// the builder did by hand from the placement's `width_cols`. Here it is
    /// where the layout puts it, which is the same place — and stays there
    /// when the column's width comes from somewhere else.
    #[test]
    fn the_grip_is_the_columns_last_cell_full_height() {
        let ui = laid_out(Some(24), 100, 30);
        let grip = ui.rect_of(ui.find_by_key(&grip_key()).expect("the grip"));
        assert_eq!((grip.x, grip.w), (23, 1), "the column's last cell");
        assert_eq!((grip.y, grip.h), (0, 30), "top to bottom");
    }

    /// A press on that cell starts the width drag rather than reaching the
    /// panel — the ordering `chrome:dock_border` got from being pushed first.
    #[test]
    fn the_grip_outranks_the_column_it_sits_on() {
        let mut ui = laid_out(Some(24), 100, 30);
        assert_eq!(
            press(&mut ui, 23, 10, MouseButton::Left),
            vec![UiFact::DockResizeBegin],
            "the grip's cell is the grip's"
        );
        // …and it *keeps* the pointer until the button comes up, which is what
        // a resize drag needs and what the next press has to be clear of.
        ui.dispatch(Input::release(
            Point::new(23, 10),
            MouseButton::Left,
            Mods::NONE,
        ));
        assert_eq!(
            press(&mut ui, 22, 10, MouseButton::Left),
            vec![UiFact::DockPress { x: 22, y: 10 }],
            "the cell beside it is the panel's"
        );
    }

    /// A right-press in the column asks the plugin for its session menu, and
    /// says where — the panel hit-tests its own widget boxes with that.
    #[test]
    fn a_right_press_in_the_column_reports_where() {
        let mut ui = laid_out(Some(24), 100, 30);
        // The frame-wide right-click observer fires wherever the click lands
        // (`shell::splits::tab_menu_guard`); what the column says is the rest.
        let said: Vec<_> = press(&mut ui, 5, 7, MouseButton::Right)
            .into_iter()
            .filter(|f| *f != UiFact::ClearTabMenus)
            .collect();
        assert_eq!(said, vec![UiFact::DockContext { x: 5, y: 7 }]);
    }

    /// **A press the tree claims still blurs the dock.**
    ///
    /// `chrome:dock_blur` was a full-frame box ranked above every consuming
    /// surface, in a walk the shell runs *before*. So once the surfaces
    /// beside the dock became nodes, a press on one of them claimed the event
    /// and the blur never ran. The observer is in the tree now, in the
    /// capture phase, and it fires whatever the press goes on to do.
    #[test]
    fn a_press_outside_the_column_blurs_and_does_not_claim() {
        let mut ui = laid_out(Some(24), 100, 30);
        let got = ui.dispatch(Input::press(
            Point::new(50, 10),
            MouseButton::Left,
            Mods::NONE,
        ));
        assert!(
            got.msgs
                .iter()
                .any(|m| matches!(m, UiMsg::Ui(UiFact::DockBlur))),
            "outside the column, got {:?}",
            got.msgs
        );
        assert!(!got.claimed, "the press goes on to what it was aimed at");
    }

    /// And a press *inside* it does not blur — the whole point of the column.
    #[test]
    fn a_press_inside_the_column_does_not_blur() {
        let mut ui = laid_out(Some(24), 100, 30);
        let got = press(&mut ui, 5, 10, MouseButton::Left);
        assert_eq!(got, vec![UiFact::DockPress { x: 5, y: 10 }]);
    }

    /// A hidden dock is still a region with a rectangle, and carries neither
    /// a grip nor an observer.
    #[test]
    fn no_dock_means_no_grip_and_no_blur() {
        let mut ui = laid_out(None, 100, 30);
        assert!(ui.find_by_key(&grip_key()).is_none(), "no grip");
        assert!(
            ui.find_by_key(&region_key(HostRegion::Dock)).is_some(),
            "the region still reports a rectangle"
        );
        assert!(
            press(&mut ui, 50, 10, MouseButton::Left).is_empty(),
            "nothing to blur"
        );
    }
}
