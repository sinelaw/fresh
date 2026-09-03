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
    gesture, row, stack, Event, GestureKind, Key, MouseButton, Node, PointerMode, Sizing,
};

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
/// **A left press focuses the dock and says nothing else.** The column's
/// widgets answer their own presses and stop the flow, so what reaches here
/// is a press they declined: the column's dead space. It used to report the
/// *cell* whenever the interior was a painter, for the runtime to hit-test
/// its own boxes with — the same seam the overlay prompt's toolbar band still
/// sits on (`UiFact::CardToolbarPress`). That branch is gone with the probe
/// behind it (S7): `panel_interior` is `None` only for a column with no panel
/// mounted at all, where there are no boxes to hit-test and nothing but the
/// focus to do.
///
/// **A right press still carries its cell, and nothing reads it.** The column
/// emits `DockContext { x, y }` on both sides of the seam. The menu comes
/// from the widget's own `UiFact::WidgetContext`, whose hit the node carried;
/// what `DockContext` does is focus the dock first, so a mirror of dock-focus
/// state is current before the menu the press raises reads it. The runtime's
/// own right-press probe over the box arena — which would have answered from
/// a *second* layout reading the runtime's scroll offset, so a scrolled list
/// raised the menu for a different row from the one clicked — has been gone
/// since 2.4, and the applier destructures the cell away.
/// The dock column's key — what the web reads the dock's display-list items
/// under (`Editor::tree_view`).
pub fn column_key() -> Key {
    Key::Str("dock_column".into())
}

fn column(interior: Option<super::panel::Interior>) -> Node<UiMsg> {
    let described = interior.is_some();
    let scoped = interior
        .as_ref()
        .map(|i| (i.keymap.clone(), i.keyboard && i.focus_key.is_empty()));
    let body = match interior {
        // An empty slot: a column with nothing in it, which nothing paints.
        None => row(),
        Some(i) => fresh_ui::layout_reader(move |info: fresh_ui::LayoutInfo| {
            // **The interior is not the column.** The runtime lays this same
            // spec at `floating_panel_inner_width` — `width_cols - 2` for a
            // left dock — and the painter draws the divider into the column's
            // last cell. Handed the whole column, the description came out two
            // columns wider than that, and anything a `flexSpacer` pins to the
            // right edge — the title bar's `[×]` above all — was laid out past
            // the visible edge and clipped away entirely. (It was also two
            // columns wider than the box arena the runtime's own hover and
            // right-click probes resolved against, back when there were any.)
            let inner_w = info.constraints.max_w.saturating_sub(DIVIDER_COLS).max(1);
            super::widgets::node(
                &i.spec,
                inner_w,
                &super::widgets::Ctx {
                    slot: super::widgets::Slot::Dock,
                    states: &i.states,
                    h_pan: &i.h_pan,
                    focus_key: i.focus_key.clone(),
                    keyboard: i.keyboard,

                    hovered_key: i.hovered_key.clone(),
                    marker_gutter: i.marker_gutter,
                    hovered_item_key: i.hovered_item_key.clone(),
                    hovered_popup_row: i.hovered_popup_row.clone(),
                    avail_height: i.avail_height,
                    scrollbar_reveal: i.scrollbar_reveal,
                    surface: super::widgets::panel_surface(),
                    markdown: i.markdown.as_ref().map(|m| m.ctx()),
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
    // The scope its keyboard layer names, and the fallback for every key its
    // widgets decline — every described interior, whether or not anything
    // in it is a Tab stop (see `panel::floating_body`).
    let body = match scoped {
        Some((keymap, rests_empty)) => {
            super::panel::interior(super::widgets::Slot::Dock, keymap, rests_empty, body)
        }
        None => body,
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
                        Some(UiMsg::Ui(UiFact::DockFocus))
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
                // scrolling. Worse than nothing: the fact this used to raise
                // moved the runtime's own scroll offset, which the description
                // does not read but the runtime's probes did, so a few notches
                // put the hover highlight and the right-click menu on a
                // different row from the one drawn. (The fact and the probes
                // are deleted now; the window is the viewport's, and this arm
                // stands down so the chain can run.)
                //
                // A column with nothing mounted has nothing to scroll, and the
                // notch must not leak through to the window beneath it: the
                // dock is not a modal, so nothing else swallows it.
                if described {
                    return None;
                }
                e.stop();
                None
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
        .key(column_key())
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
                    h_pan: Default::default(),
                    focus_key: String::new(),
                    keyboard: true,

                    page: None,
                    reading: None,
                    compose: None,
                    hovered_key: None,
                    hovered_item_key: String::new(),
                    hovered_popup_row: String::new(),
                    marker_gutter: false,
                    avail_height: None,
                    scrollbar_reveal: None,
                    keymap: None,
                    markdown: None,
                }),
                ..Frame::default()
            }),
            Size::new(w, h),
        );
        ui
    }

    /// **A press on the column says one thing on either side of the seam**,
    /// and the cell is not it.
    ///
    /// It used to say *where* whenever the interior was a painter, because
    /// the runtime hit-tested its own boxes with that cell. Nothing does any
    /// more: an interior that is not described is a column with no panel
    /// mounted, which has no boxes and no rows, so the press has nothing to
    /// resolve against and the focus is the whole of it (S7).
    #[test]
    fn a_press_on_the_column_focuses_it_either_side_of_the_seam() {
        let mut ui = laid_out(Some(24), 100, 30);
        assert_eq!(
            press(&mut ui, 10, 5, MouseButton::Left),
            vec![UiFact::DockFocus],
            "an empty column: there is nothing under the cell to name"
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
    /// cell, and the column emits it described or not. Nothing consumes the
    /// cell any more — see `column`'s own doc — so this asserts what it can
    /// see and no more.
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
            vec![UiFact::DockFocus],
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
        assert_eq!(got, vec![UiFact::DockFocus]);
    }

    /// A hidden dock is still a region with a rectangle, and carries neither
    /// a grip nor an observer.
    #[test]
    fn no_dock_means_no_grip_and_no_blur() {
        let mut ui = laid_out(None, 100, 30);
        assert!(ui.find_by_key(&grip_key()).is_none(), "no grip");
        assert!(
            ui.find_by_key(&region_key(crate::view::shell::frame::HostRegion::Dock))
                .is_some(),
            "the region still reports a rectangle"
        );
        assert!(
            press(&mut ui, 50, 10, MouseButton::Left).is_empty(),
            "nothing to blur"
        );
    }

    /// A described dock holding two ordinary buttons, with its keyboard layer
    /// up — the shape the scope exists for.
    fn described_with_buttons() -> Ui<UiMsg> {
        use fresh_core::api::WidgetSpec;
        let button = |label: &str, key: &str| WidgetSpec::Button {
            label: label.into(),
            focused: false,
            intent: Default::default(),
            key: Some(key.into()),
            disabled: false,
            focusable: true,
            bare: false,
            full_width: false,
            hover_style: None,
            style: None,
        };
        let spec = WidgetSpec::Col {
            children: vec![button("one", "one"), button("two", "two")],
            key: None,
        };
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            frame_tree(Frame {
                menu_bar: false,
                status_bar: false,
                dock: Some(30),
                dock_keys: true,
                dock_interior: Some(super::super::panel::Interior {
                    spec: Rc::new(spec),
                    states: Rc::new(Default::default()),
                    h_pan: Default::default(),
                    focus_key: String::new(),
                    keyboard: true,

                    page: None,
                    reading: None,
                    compose: None,
                    hovered_key: None,
                    hovered_item_key: String::new(),
                    hovered_popup_row: String::new(),
                    marker_gutter: false,
                    avail_height: None,
                    scrollbar_reveal: None,
                    keymap: None,
                    markdown: None,
                }),
                ..Frame::default()
            }),
            Size::new(120, 40),
        );
        ui
    }

    /// **Tab in a focused dock steps along the widgets.**
    ///
    /// The state before this: `panel::keys_layer` is a `Modality::Focus` layer
    /// whose only child was an autofocused key sink, and confinement is
    /// containment — so the dock's focus scope held that one node, Tab could
    /// not reach a widget, and `apply_autofocus` pulled a click-focused widget
    /// back out on the next frame. Every widget was focusable and none was
    /// reachable.
    ///
    /// The layer names the interior as its scope now. This asserts both halves:
    /// the ring is the panel's widgets, and Tab moves along it rather than
    /// being claimed by a sink.
    #[test]
    fn tab_in_a_focused_dock_steps_along_the_widgets() {
        let mut ui = described_with_buttons();

        let ring: Vec<String> = ui
            .focus_scope()
            .ordered()
            .into_iter()
            .filter_map(|e| ui.key_of(e))
            .map(|k| format!("{k:?}"))
            .collect();
        assert_eq!(
            ring.len(),
            2,
            "the ring is the panel's two buttons, not a sink: {ring:?}"
        );

        // Focus lands inside the scope, and Tab moves it to the other control
        // rather than being swallowed.
        let first = ui.focused().expect("the scope took focus");
        let got = ui.dispatch(fresh_ui::Input::Key(fresh_ui::KeyPress {
            code: fresh_ui::KeyCode::Tab,
            mods: fresh_ui::Mods::NONE,
        }));
        assert_ne!(ui.focused(), Some(first), "Tab moved focus");
        assert!(
            !got.msgs.iter().any(|m| matches!(
                m,
                UiMsg::Ui(UiFact::PanelKey(super::super::widgets::Slot::Dock))
            )),
            "Tab was resolved by the tree, not handed to the runtime: {:?}",
            got.msgs
        );
    }

    /// **Which ring is authoritative is a question only the tree can answer,
    /// and this is the answer the host reads.**
    ///
    /// `Editor::advance_panel_focus_in_tree` routes a plugin's
    /// `WidgetAction::FocusAdvance` — and every other host-driven advance —
    /// to `Ui::move_focus` when the tree is holding this panel's focus, and
    /// leaves it to the runtime's box arena when it is not. The test is that
    /// the two states are *distinguishable from the tree alone*: a described
    /// interior with something focusable in it carries the scope key and has
    /// focus inside it; a panel that kept its key sink has no such element at
    /// all, so there is nothing to ask and nothing to move.
    #[test]
    fn the_tree_says_whether_it_is_holding_the_panels_focus() {
        let scope = super::super::panel::interior_key(super::super::widgets::Slot::Dock);

        let ui = described_with_buttons();
        let el = ui
            .find_by_key(&scope)
            .expect("a described interior with focusables names the scope");
        assert!(
            ui.has_focus_within(el),
            "and focus settles inside it, so the ring is the tree's"
        );

        // The same dock with a keyboard and *nothing focusable* in its
        // interior: `column` leaves the body unwrapped, `keys_layer` keeps the
        // sink, and the scope key is not in the tree.
        let mut sink: Ui<UiMsg> = Ui::new();
        sink.frame(
            frame_tree(Frame {
                menu_bar: false,
                status_bar: false,
                dock: Some(30),
                dock_keys: true,
                ..Frame::default()
            }),
            Size::new(120, 40),
        );
        assert!(
            sink.find_by_key(&scope).is_none(),
            "no scope: the arena is the only ring there is"
        );
    }

    /// **A move the host asks for imperatively reports its landing the same
    /// way a Tab does.**
    ///
    /// The two rings agreed on nothing before: the arena wrote the registry's
    /// focus key and the tree's focus stayed where it was. Routing through
    /// `move_focus` makes the registry a mirror of the same fact Tab writes —
    /// and the fact travels on `Ui::take_messages`, which is what
    /// `advance_panel_focus_in_tree` drains into the ordinary applier.
    #[test]
    fn an_imperative_move_reports_the_new_holder() {
        let mut ui = described_with_buttons();
        // The interior names no focused widget, so the frame rested focus on
        // the scope itself and said nothing — "nothing focused" is a state
        // the description carries, not a gain the registry is told about.
        let settled = ui.take_messages();
        assert!(
            settled.is_empty(),
            "nothing named, nothing gained: the frame rested on the scope"
        );
        let first = ui.focused().expect("the scope took focus");
        assert_eq!(
            Some(first),
            ui.find_by_key(&super::super::panel::interior_key(
                super::super::widgets::Slot::Dock
            )),
            "on the interior, outside the ring"
        );
        // From outside the ring, the first move lands on the first button and
        // the second on the second.
        assert!(ui.move_focus(fresh_ui::FocusDir::Next));
        assert!(
            ui.move_focus(fresh_ui::FocusDir::Next),
            "two buttons: the ring can serve the move"
        );
        assert_ne!(ui.focused(), Some(first), "focus actually moved");

        let named: Vec<String> = ui
            .take_messages()
            .into_iter()
            .filter_map(|m| match m {
                UiMsg::Ui(UiFact::WidgetFocus { slot, widget })
                    if slot == super::super::widgets::Slot::Dock =>
                {
                    Some(widget)
                }
                _ => None,
            })
            .collect();
        assert_eq!(
            named,
            vec!["one".to_string(), "two".to_string()],
            "each landing names the widget the ring reached"
        );
    }

    /// Every other key is still the runtime's, and by the same route.
    ///
    /// Nothing in a described panel attaches a key handler of its own — the
    /// kinds' key handling is host-side — so the fallback claims the rest and
    /// `PanelKey` reaches the router exactly as it did.
    #[test]
    fn a_key_the_tree_does_not_resolve_still_reaches_the_runtime() {
        let mut ui = described_with_buttons();
        let got = ui.dispatch(fresh_ui::Input::Key(fresh_ui::KeyPress {
            code: fresh_ui::KeyCode::Enter,
            mods: fresh_ui::Mods::NONE,
        }));
        assert!(
            got.msgs.iter().any(|m| matches!(
                m,
                UiMsg::Ui(UiFact::PanelKey(super::super::widgets::Slot::Dock))
            )),
            "Enter is the runtime's: {:?}",
            got.msgs
        );
    }
}
