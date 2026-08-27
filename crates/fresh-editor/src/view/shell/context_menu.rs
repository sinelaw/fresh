//! Context menus as real `Layer`s — the first overlay in the tree (wave M2).
//!
//! The plan calls this wave the go/no-go: it is the first to use a layer, its
//! modality and its dismissal together, and if the model holds here the later
//! surfaces apply the same mechanisms.
//!
//! All four input channels have migrated. Paint comes from the tree, the rows
//! answer the pointer, `Modality::Exclusive` does what the full-frame
//! close-guard box did, `OUTSIDE_POINTER` dismissal replaces the outside-click
//! arm, and the arrow/Enter grab that used to run as a pre-band stage is now an
//! `on_key` on a focused child of the layer. Nothing about the menus is
//! dispatched by hand any more.
//!
//! Geometry is the layer's. The menu anchors at the raw right-click point and
//! `Fit::CLAMP` pulls it back inside the frame — which is all
//! `ContextMenu::clamped_position` ever did, and that function is gone. Its
//! other caller, the web `Scene`, now reads [`menu_rect`] off the retained
//! spec, so there is one place a menu's rectangle is decided and everyone else
//! reads it.
//!
//! `layer_rank::CONTEXT_MENU` is the last of the old implementation, and it
//! only survives because the PTY gate reads `blocks_terminal_input` off the
//! overlay stack — see the note at its site.

use fresh_ui::{
    col, focusable, gesture, layer, text, Anchor, Dismiss, Fit, Key, KeyCode, LayoutSpec, Modality,
    Node, Rect, Sizing,
};

use super::msg::{MenuStep, UiFact, UiMsg};

/// The key the menu's layer carries, so a rectangle consumer can find its
/// items in the display list.
const MENU_KEY: &str = "context_menu";

/// A row's identity across rebuilds.
///
/// The box was keyed and its rows were not, so a row's identity was its
/// position — and this menu rebuilds on every highlight move. Keying them the
/// way the explorer keys its rows means the element that was under the pointer
/// stays the same element when the list beneath it changes.
pub fn item_key(index: usize) -> Key {
    Key::Pair("context_menu_item".into(), index as u64)
}

/// What one menu needs to draw: where it goes, what is in it, and which row is
/// highlighted.
///
/// `x`/`y` are the **raw** anchor point — where the click was. Keeping the
/// whole box on screen is the layer's `Fit::CLAMP`, not the caller's.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Menu {
    pub x: u16,
    pub y: u16,
    pub width: u16,
    pub highlighted: usize,
    pub items: Vec<String>,
}

/// The label as the old painter wrote it: one leading space, left-aligned, and
/// padded to the box's inner width.
///
/// Reproduced verbatim rather than re-derived — the row is what the cells
/// actually contain, and this is the migration's acceptance bar.
fn row_label(label: &str, width: u16) -> String {
    let content_width = (width as usize).saturating_sub(2);
    format!(" {:<pad$}", label, pad = content_width.saturating_sub(1))
}

/// One menu, as a description.
///
/// The rows answer the pointer themselves, and the layer's own properties do
/// what a full-frame guard box used to: `Modality::Inert` makes everything
/// outside non-interactive, and `OUTSIDE_POINTER` dismissal turns a click out
/// there into a close. Neither is a rule anyone wrote down for this surface —
/// they are declared properties of the layer, which is the whole argument for
/// moving overlays into the tree.
pub fn context_menu(menu: &Menu) -> Node<UiMsg> {
    let highlighted = menu.highlighted;
    let rows: Vec<Node<UiMsg>> = menu
        .items
        .iter()
        .enumerate()
        .map(|(i, label)| {
            let theme = if i == menu.highlighted {
                crate::view::ui::MenuRowStyle::Highlighted.shell_theme()
            } else {
                crate::view::ui::MenuRowStyle::Normal.shell_theme()
            };
            gesture(
                text(row_label(label, menu.width))
                    .theme(theme)
                    .h(Sizing::Cells(1)),
            )
            .key(item_key(i))
            // A click moves the highlight and activates, exactly as the old
            // click handler did — activation runs the same path Enter does.
            .on_click(move |_| UiMsg::Ui(UiFact::ActivateContextMenuItem(i)))
            // Only when it actually moves. The handler that this replaced
            // gated on `core.highlighted != idx`; without the same gate every
            // pointer move over the row it is already on reports a change and
            // marks the tree dirty.
            .on_enter(std::rc::Rc::new(move |_: &fresh_ui::Event| {
                (i != highlighted).then_some(UiMsg::Ui(UiFact::HighlightContextMenuItem(i)))
            }))
        })
        .collect();

    layer()
        .key(MENU_KEY)
        // The raw point, and `fit` keeps the box on screen. One rectangle,
        // decided by layout — `clamped_position` was the same arithmetic
        // written a second time, and everything that needs the answer reads it
        // back through `menu_rect`.
        .anchor(Anchor::Point(menu.x, menu.y))
        .fit(Fit::CLAMP)
        // Everything outside is non-interactive while the menu is up, and no
        // host leaf behind it takes raw input. That second half is what the
        // old layer spelled `blocks_terminal_input: true`; the library derives
        // it from the modality instead of taking it on trust.
        .modality(Modality::Exclusive)
        .dismiss(Dismiss::OUTSIDE_POINTER.or(Dismiss::ESCAPE))
        .on_dismiss(|_| UiMsg::Ui(UiFact::CloseContextMenu))
        .child(
            focusable(
                gesture(
                    col()
                        .border()
                        .theme(crate::app::shell_host::shell_theme::pair(
                            "ui.menu_dropdown_fg",
                            "ui.menu_dropdown_bg",
                        ))
                        .w(Sizing::Cells(menu.width))
                        .children(rows),
                )
                // A right-click inside an open menu is swallowed so the menu stays
                // put rather than being re-opened or re-targeted. Stopping is the
                // whole of it — the dispatcher reports the claim, so there is
                // nothing to say.
                .on(
                    fresh_ui::GestureKind::SecondaryClick,
                    std::rc::Rc::new(|e: &fresh_ui::Event| {
                        e.stop();
                        None
                    }),
                ),
            )
            // The menu owns the keyboard while it is up: arrows move the
            // highlight, Enter activates, and everything else is swallowed. That
            // last part is why claim had to become something the library reports —
            // a swallowed key produces no message, and inferring "claimed" from
            // "said something" would let it through.
            //
            // Escape is not here: the layer declares `ESCAPE` dismissal, and a key
            // that dismisses a layer is answered by that layer.
            .autofocus()
            .on_key(move |e: &fresh_ui::Event| {
                let Some(k) = e.key else { return None };
                if k.mods != fresh_ui::Mods::NONE {
                    return None;
                }
                // Escape is the one key this handler must *not* stop: stopping
                // claims it here, and the layer's `ESCAPE` dismissal never runs.
                if k.code == KeyCode::Esc {
                    return None;
                }
                e.stop();
                match k.code {
                    KeyCode::Up => Some(UiMsg::Ui(UiFact::StepContextMenu(MenuStep::Prev))),
                    KeyCode::Down => Some(UiMsg::Ui(UiFact::StepContextMenu(MenuStep::Next))),
                    KeyCode::Enter => Some(UiMsg::Ui(UiFact::ActivateContextMenuItem(highlighted))),
                    // Modal: swallowed, with nothing to say about it.
                    _ => None,
                }
            }),
        )
}

/// Where the open menu actually landed, read off the display list.
///
/// The one geometry authority for a context menu: layout placed it (anchor
/// plus `Fit::CLAMP`), and every consumer that is not the fold — the web
/// `Scene`, today — asks here instead of recomputing the placement.
pub fn menu_rect(spec: &LayoutSpec) -> Option<Rect> {
    let items = spec.items_for(&Key::Str(MENU_KEY.into()));
    if items.is_empty() {
        return None;
    }
    let x = items.iter().map(|i| i.rect.x).min()?;
    let y = items.iter().map(|i| i.rect.y).min()?;
    let right = items.iter().map(|i| i.rect.x + i.rect.w as i32).max()?;
    let bottom = items.iter().map(|i| i.rect.y + i.rect.h as i32).max()?;
    Some(Rect::new(x, y, (right - x) as u16, (bottom - y) as u16))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The padding is the old painter's, character for character.
    #[test]
    fn a_row_is_padded_exactly_as_before() {
        // width 12 => content_width 10 => " " + label padded to 9.
        assert_eq!(row_label("Copy", 12), " Copy     ");
        assert_eq!(row_label("Copy", 12).chars().count(), 10);
    }

    /// Degenerate widths must not panic or produce a negative pad.
    #[test]
    fn narrow_menus_do_not_underflow() {
        for w in 0u16..4 {
            let _ = row_label("x", w);
        }
    }
}

#[cfg(test)]
mod paint_tests {
    use super::*;
    use crate::view::shell::fold::{fold_native, Band};
    use crate::view::shell::frame::{frame_tree, Frame};
    use fresh_ui::{Size, ThemeKey, Ui};
    use ratatui::buffer::Buffer;
    use ratatui::layout::Rect;
    use ratatui::style::Style;

    fn plain(_: &ThemeKey) -> Style {
        Style::default()
    }

    fn render(menu: Menu, w: u16, h: u16) -> Buffer {
        let mut ui: Ui<UiMsg> = Ui::new();
        let frame = Frame {
            menu: Some(menu),
            ..Frame::default()
        };
        let spec = ui.frame(frame_tree(frame), Size::new(w, h)).clone();
        let mut buf = Buffer::empty(Rect::new(0, 0, w, h));
        // Both bands: a test that renders the whole frame wants the whole
        // display list, and the cut only matters where legacy painters go
        // between them.
        fold_native(&spec, &mut buf, &plain, Band::Background);
        fold_native(&spec, &mut buf, &plain, Band::Overlay);
        buf
    }

    fn row(buf: &Buffer, y: u16) -> String {
        (0..buf.area.width)
            .map(|x| buf[(x, y)].symbol().to_string())
            .collect()
    }

    /// The menu paints where it was told to, with the plain border glyphs the
    /// rest of the editor draws, and its labels padded as before.
    #[test]
    fn a_menu_paints_a_bordered_box_at_its_point() {
        let buf = render(
            Menu {
                x: 2,
                y: 1,
                width: 10,
                highlighted: 0,
                items: vec!["Copy".into(), "Paste".into()],
            },
            20,
            6,
        );
        assert_eq!(row(&buf, 1), "  ┌────────┐        ", "top border");
        assert_eq!(row(&buf, 2), "  │ Copy   │        ", "first item");
        assert_eq!(row(&buf, 3), "  │ Paste  │        ", "second item");
        assert_eq!(row(&buf, 4), "  └────────┘        ", "bottom border");
    }

    /// **The highlighted row looks different, and the test can see it.**
    ///
    /// Rendering through a palette that answered `Style::default()` for every
    /// name meant the highlight was asserted only by the *name* the
    /// description carried, never by anything a user would see. A row whose
    /// theme is wired to the wrong palette entry, or a fold that dropped the
    /// style, would have gone through unnoticed.
    #[test]
    fn the_highlighted_row_is_styled_differently_from_the_others() {
        use crate::view::shell::fold::{test_palette, Band};
        let mut ui: Ui<UiMsg> = Ui::new();
        let spec = ui
            .frame(
                frame_tree(Frame {
                    menu: Some(Menu {
                        x: 0,
                        y: 0,
                        width: 10,
                        highlighted: 1,
                        items: vec!["Copy".into(), "Paste".into()],
                    }),
                    ..Frame::default()
                }),
                Size::new(20, 6),
            )
            .clone();
        let mut buf = Buffer::empty(Rect::new(0, 0, 20, 6));
        fold_native(&spec, &mut buf, &test_palette::palette, Band::Overlay);

        assert_eq!(
            buf[(2, 1)].style(),
            test_palette::painted(&crate::view::ui::MenuRowStyle::Normal.shell_theme())
        );
        assert_eq!(
            buf[(2, 2)].style(),
            test_palette::painted(&crate::view::ui::MenuRowStyle::Highlighted.shell_theme()),
            "the highlighted row is the one the menu says is highlighted"
        );
        assert_ne!(buf[(2, 1)].style(), buf[(2, 2)].style());
    }

    /// **The clamp, as a layer property.** A menu opened near the right/bottom
    /// edge is pulled back inside the frame — the whole of what
    /// `ContextMenu::clamped_position` computed, now `Fit::CLAMP` on the layer.
    /// The old arithmetic was `x.min(frame_w - box_w).max(0)` and the same in
    /// y over a box `items + 2` tall; these are the cells it produced.
    #[test]
    fn a_menu_near_the_edge_is_pulled_inside_the_frame() {
        // Frame 20x6, box 10 wide and 4 tall (2 items + borders), opened at
        // (14, 4): the old clamp gave (20-10, 6-4) = (10, 2).
        let buf = render(
            Menu {
                x: 14,
                y: 4,
                width: 10,
                highlighted: 0,
                items: vec!["Copy".into(), "Paste".into()],
            },
            20,
            6,
        );
        assert_eq!(row(&buf, 2), "          ┌────────┐", "top border");
        assert_eq!(row(&buf, 5), "          └────────┘", "bottom border");
    }

    /// A box larger than the frame is pinned at the origin rather than pushed
    /// off the left edge — `saturating_sub` in the old code, `.max(0)` in the
    /// layer's fit.
    #[test]
    fn a_menu_wider_than_the_frame_starts_at_the_origin() {
        let buf = render(
            Menu {
                x: 3,
                y: 1,
                width: 30,
                highlighted: 0,
                items: vec!["Copy".into()],
            },
            10,
            4,
        );
        // y clamps to 1 (frame 4 tall, box 3), x pins to 0.
        assert_eq!(
            row(&buf, 1).chars().next(),
            Some('\u{250c}'),
            "pinned to column 0"
        );
    }

    /// **One rectangle, read back.** `menu_rect` is what a consumer that is
    /// not the fold — the web `Scene` — asks instead of re-deriving the
    /// placement, so it must name the cells the fold actually painted.
    #[test]
    fn menu_rect_names_the_box_that_was_painted() {
        let mut ui: Ui<UiMsg> = Ui::new();
        let spec = ui
            .frame(
                frame_tree(Frame {
                    menu: Some(Menu {
                        x: 14,
                        y: 4,
                        width: 10,
                        highlighted: 0,
                        items: vec!["Copy".into(), "Paste".into()],
                    }),
                    ..Frame::default()
                }),
                Size::new(20, 6),
            )
            .clone();
        assert_eq!(menu_rect(&spec), Some(fresh_ui::Rect::new(10, 2, 10, 4)));
    }

    /// Nothing open, nothing to report — the `Scene` must not be handed a
    /// stale rectangle when the menu has closed.
    #[test]
    fn menu_rect_is_none_with_no_menu_open() {
        let mut ui: Ui<UiMsg> = Ui::new();
        let spec = ui
            .frame(frame_tree(Frame::default()), Size::new(20, 6))
            .clone();
        assert_eq!(menu_rect(&spec), None);
    }

    /// An overlay is out of flow: it does not disturb the regions around it.
    #[test]
    fn a_menu_does_not_move_the_frame() {
        use crate::view::shell::frame::{region_rects, HostRegion};
        let size = Rect::new(0, 0, 30, 8);
        let without = region_rects(Frame::default(), size);
        let with = region_rects(
            Frame {
                menu: Some(Menu {
                    x: 3,
                    y: 2,
                    width: 8,
                    highlighted: 0,
                    items: vec!["One".into()],
                }),
                ..Frame::default()
            },
            size,
        );
        for region in [HostRegion::Body, HostRegion::StatusBar, HostRegion::MenuBar] {
            let a = without.iter().find(|(r, _)| *r == region).unwrap().1;
            let b = with.iter().find(|(r, _)| *r == region).unwrap().1;
            assert_eq!(a, b, "{region:?} moved when a menu opened");
        }
    }
}

#[cfg(test)]
mod input_tests {
    use super::*;
    use crate::view::shell::frame::{frame_tree, Frame};
    use fresh_ui::{Input, Mods, MouseButton, Point, Size, Ui};

    fn menu() -> Menu {
        Menu {
            x: 2,
            y: 1,
            width: 10,
            highlighted: 0,
            items: vec!["Copy".into(), "Paste".into()],
        }
    }

    fn open(w: u16, h: u16) -> Ui<UiMsg> {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            frame_tree(Frame {
                menu: Some(menu()),
                ..Frame::default()
            }),
            Size::new(w, h),
        );
        ui
    }

    /// Both halves of the click. Dismissal is evaluated on the *press* (as the
    /// close-guard box also was) while activation lands on the release, so a
    /// helper that watched only one would miss half the behaviour.
    fn click(ui: &mut Ui<UiMsg>, x: i32, y: i32) -> Vec<UiMsg> {
        let pos = Point::new(x, y);
        let mut out = ui
            .dispatch(Input::press(pos, MouseButton::Left, Mods::NONE))
            .msgs;
        out.extend(
            ui.dispatch(Input::release(pos, MouseButton::Left, Mods::NONE))
                .msgs,
        );
        out
    }

    fn facts(msgs: Vec<UiMsg>) -> Vec<UiFact> {
        msgs.into_iter()
            .map(|m| match m {
                UiMsg::Ui(f) => f,
                other => panic!("unexpected message {other:?}"),
            })
            .collect()
    }

    /// Clicking a row activates it, and the row it names is the row under the
    /// pointer — the box's border is one cell, so the first item is at y+1.
    #[test]
    fn clicking_a_row_activates_that_row() {
        let mut ui = open(20, 8);
        let got = facts(click(&mut ui, 4, 2));
        assert!(
            matches!(got.as_slice(), [UiFact::ActivateContextMenuItem(0)]),
            "got {got:?}"
        );

        let mut ui = open(20, 8);
        let got = facts(click(&mut ui, 4, 3));
        assert!(
            matches!(got.as_slice(), [UiFact::ActivateContextMenuItem(1)]),
            "got {got:?}"
        );
    }

    /// **The close-guard box, replaced by a property.** A click outside the
    /// menu dismisses it — declared as `OUTSIDE_POINTER` on the layer rather
    /// than simulated with a full-frame box that has to be pushed, ranked and
    /// kept in sync.
    #[test]
    fn clicking_outside_dismisses() {
        let mut ui = open(20, 8);
        let got = facts(click(&mut ui, 18, 7));
        assert!(
            got.contains(&UiFact::CloseContextMenu),
            "a click outside must close the menu, got {got:?}"
        );
    }

    /// Hovering a row moves the highlight, which the old component did through
    /// a hover-target walk and a `HoverTarget::ContextMenuItem` round trip.
    #[test]
    fn hovering_a_row_highlights_it() {
        let mut ui = open(20, 8);
        let got = facts(
            ui.dispatch(Input::Move {
                pos: Point::new(4, 3),
                mods: Mods::NONE,
            })
            .msgs,
        );
        assert!(
            got.contains(&UiFact::HighlightContextMenuItem(1)),
            "got {got:?}"
        );
    }

    /// Hovering the row that is *already* highlighted says nothing. The
    /// handler this replaced gated on the same condition; without it, every
    /// pointer move over the current row reports a change and marks the tree
    /// dirty.
    #[test]
    fn hovering_the_current_row_reports_nothing() {
        let mut ui = open(20, 8);
        // Row 0 is highlighted to begin with, and sits at y+1 past the border.
        let got = facts(
            ui.dispatch(Input::Move {
                pos: Point::new(4, 2),
                mods: Mods::NONE,
            })
            .msgs,
        );
        assert!(got.is_empty(), "no change, so nothing to report: {got:?}");
    }

    /// A right-click inside is swallowed so the menu stays put rather than
    /// being re-opened or re-targeted.
    #[test]
    fn a_right_click_inside_is_swallowed() {
        let mut ui = open(20, 8);
        let pos = Point::new(4, 2);
        // A right-click inside is claimed, and claiming is now what the
        // dispatcher reports — there is no message to look for.
        let press = ui.dispatch(Input::press(pos, MouseButton::Right, Mods::NONE));
        let release = ui.dispatch(Input::release(pos, MouseButton::Right, Mods::NONE));
        assert!(
            release.claimed,
            "the menu must swallow a right-click inside it"
        );
        let mut msgs = press.msgs;
        msgs.extend(release.msgs);
        let got = facts(msgs);
        assert!(
            got.is_empty(),
            "swallowing needs no message now that claim is reported, got {got:?}"
        );
    }

    fn key(ui: &mut Ui<UiMsg>, code: fresh_ui::KeyCode) -> fresh_ui::Dispatch<UiMsg> {
        ui.dispatch(Input::Key(fresh_ui::KeyPress::with(code, Mods::NONE)))
    }

    /// Arrows move the highlight and Enter activates — the handler this
    /// replaced did both from a pre-band keyboard grab.
    #[test]
    fn arrows_step_and_enter_activates() {
        use crate::view::shell::msg::MenuStep;
        let mut ui = open(20, 8);
        assert!(facts(key(&mut ui, fresh_ui::KeyCode::Down).msgs)
            .contains(&UiFact::StepContextMenu(MenuStep::Next)));
        assert!(facts(key(&mut ui, fresh_ui::KeyCode::Up).msgs)
            .contains(&UiFact::StepContextMenu(MenuStep::Prev)));
        assert!(facts(key(&mut ui, fresh_ui::KeyCode::Enter).msgs)
            .contains(&UiFact::ActivateContextMenuItem(0)));
    }

    /// **Why claim had to become something the library reports.** An open menu
    /// is modal: every other key is swallowed, producing no message at all.
    /// Inferring "claimed" from "said something" would let those keys straight
    /// through to the editor beneath.
    #[test]
    fn every_other_key_is_swallowed_silently() {
        let mut ui = open(20, 8);
        let d = key(&mut ui, fresh_ui::KeyCode::Char('x'));
        assert!(d.msgs.is_empty(), "nothing to say");
        assert!(d.claimed, "but the menu still owns the key");
    }

    /// Escape closes it, declared as layer dismissal rather than handled as a
    /// key — and a key that dismisses a layer is answered by that layer.
    #[test]
    fn escape_dismisses_and_is_claimed() {
        let mut ui = open(20, 8);
        let d = key(&mut ui, fresh_ui::KeyCode::Esc);
        assert!(facts(d.msgs).contains(&UiFact::CloseContextMenu));
        assert!(d.claimed);
    }

    /// **The regression this cost us.** A right-click outside an open menu
    /// closes it *and* must go on to open the new one — every platform does
    /// both from that one press. While claim was inferred from "did anything
    /// say something", the dismissal message read as a claim and the second
    /// half never happened: two right-clicks where one used to do.
    ///
    /// The library now dismisses for any button but claims only for the
    /// primary one, so the press stays available to whatever opens the next
    /// menu.
    #[test]
    fn a_right_click_outside_dismisses_without_swallowing_the_press() {
        let mut ui = open(20, 8);
        let outside = Point::new(18, 7);
        let press = ui.dispatch(Input::press(outside, MouseButton::Right, Mods::NONE));
        assert!(
            facts(press.msgs.clone()).contains(&UiFact::CloseContextMenu),
            "the menu must still close"
        );
        assert!(
            !press.claimed,
            "but the press must remain available to open the next menu"
        );
    }

    /// A *left* click outside is spent closing the menu, and does claim — the
    /// close-guard box consumed it too.
    #[test]
    fn a_left_click_outside_is_spent_closing_the_menu() {
        let mut ui = open(20, 8);
        let press = ui.dispatch(Input::press(
            Point::new(18, 7),
            MouseButton::Left,
            Mods::NONE,
        ));
        assert!(press.claimed, "closing is the whole of a left click here");
    }
}
