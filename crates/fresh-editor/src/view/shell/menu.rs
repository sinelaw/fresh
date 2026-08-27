//! Menu-bar dropdowns as `Layer`s — the second overlay in the tree (wave M3).
//!
//! A dropdown chain is several boxes at once: the menu's own list, plus one
//! box per open submenu level, each placed against the one before it. Under
//! the old renderer that chain was a loop that painted, recorded hit rects and
//! decided placement in the same pass; here each level is a `Layer`, out of
//! flow and painted in declaration order, and the chain is just their order.
//!
//! The bar row above them is a native region too, in the fold's background
//! band, and both answer the pointer: the labels toggle their menus on the
//! press, the rows activate on the release, and the outermost level's
//! `OUTSIDE_POINTER` dismissal is what the full-frame
//! `chrome:menu_close_guard` box used to be.
//!
//! **Placement has not migrated**, and the reason is upstream of placement:
//! this chain has no content model in the tree to place. `DropdownLevel`
//! carries `x`, `y` and `width` — a rect on a description type, which the
//! design doc names as its own stop sign — plus strings already fitted to a
//! width nothing measured. `Place::RightOf` places against a *measured* box,
//! so it is not merely unused here, it is not yet expressible.
//!
//! An earlier version of this note claimed `Anchor::Node` could not express
//! the one-row rise a submenu's border needs. It can: anchor to the row
//! *above*. That is not the blocker; the content model is. See §6.2 of the
//! migration doc for the order the pieces have to move in, and for the
//! measured divergence between this walk's flip rule and `Fit::FLIP`.

use std::rc::Rc;

use fresh_ui::{
    col, gesture, layer, row, text, text_runs, Anchor, Dismiss, Event, GestureKind, Modality, Node,
    Run, Sizing,
};

use crate::app::types::HoverTarget;

use super::msg::{UiFact, UiMsg};

fn hover(t: Option<HoverTarget>) -> fresh_ui::Handler<UiMsg> {
    Rc::new(move |_: &Event| Some(UiMsg::Ui(UiFact::Hover(t.clone()))))
}

/// One label on the menu bar: `" Label "`, cut into runs so a mnemonic
/// character can be underlined inside the label rather than beside it.
///
/// Cut here rather than in the description because the cut is the renderer's
/// decision — which character the mnemonic resolver picked — and the shell
/// only needs to know that a run exists and what to call it.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BarItem {
    /// `(text, theme name)`, in order. Usually one run; three when a mnemonic
    /// splits the label.
    pub runs: Vec<(String, String)>,
    /// Which menu this label opens.
    pub index: usize,
}

/// The menu bar row: its labels, and the ground they sit on.
///
/// Empty is meaningful — the row still exists and still has a rectangle, which
/// is what the frame's other regions are measured against.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct MenuBar {
    pub items: Vec<BarItem>,
}

/// The bar row as a description.
///
/// A background surface: it paints in the fold's `Background` band, under
/// every legacy painter, which is what the two-pass fold made possible. Its
/// own dropdowns are `Layer`s and paint in the other band, over them.
pub fn menu_bar(bar: &MenuBar) -> Node<UiMsg> {
    let labels: Vec<Node<UiMsg>> = bar
        .items
        .iter()
        .map(|it| {
            let runs: Vec<Run> = it
                .runs
                .iter()
                .map(|(t, theme)| Run::themed(t.clone(), theme))
                .collect();
            let index = it.index;
            gesture(text_runs(runs))
                // Stops, because the row behind it closes the menu: a press
                // bubbles to every handler on its path, so a label that only
                // *answered* would be followed by the ground's close and the
                // menu would open and shut in one gesture.
                //
                // Left only, like the pre-migration routing
                // (`handle_click_menu_bar` was reached from
                // `MouseEventKind::Down(Left)` alone). Without the guard a
                // right-click on a label opens its menu *and* claims the
                // press, so it never reaches the theme inspector's pre-band —
                // the same regression the search-options row's toggles have,
                // where issue #2362's inspector test caught it.
                .on(
                    GestureKind::Press,
                    Rc::new(move |e: &Event| {
                        if e.button != fresh_ui::MouseButton::Left {
                            return None;
                        }
                        e.stop();
                        Some(UiMsg::Ui(UiFact::MenuBarPress { index }))
                    }),
                )
                .on_enter(hover(Some(HoverTarget::MenuBarItem(index))))
                // The partner of `on_enter`: the tree owns this hover
                // outright, so nothing else will clear it when the pointer
                // moves off into the gap between labels.
                .on_leave(hover(None))
                .into()
        })
        .collect();

    // The row names its own ground, so the cells between and after the labels
    // carry the bar's background — the `Paragraph`'s `.style(bg)` did that.
    //
    // A click on that ground closes an open menu, which is what the old
    // `row == 0` arm of `handle_click_menu_bar` did; a label above answers
    // first, because a click is derived per path and the label is the deeper
    // one.
    gesture(
        row()
            .theme(crate::app::shell_host::shell_theme::pair(
                "ui.menu_fg",
                "ui.menu_bg",
            ))
            .children(labels),
    )
    // On the press, with the labels above it: the whole bar acts on the
    // same gesture, so the dismissal, the close and the toggle are one
    // dispatch and cannot see each other's aftermath.
    .on(
        GestureKind::Press,
        Rc::new(|_: &Event| Some(UiMsg::Ui(UiFact::CloseMenu))),
    )
    .on_enter(hover(None))
}

/// One row of one dropdown: what it says, and the name of how it looks.
///
/// Both halves come from the renderer's own derivation
/// (`MenuRenderer::dropdown_item_text` and `MenuRowStyle`), so a row here says
/// character-for-character what the old painter wrote.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DropdownRow {
    pub text: String,
    pub theme: String,
}

/// One level of an open dropdown chain: the bordered box and its rows.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DropdownLevel {
    pub x: u16,
    pub y: u16,
    pub width: u16,
    pub rows: Vec<DropdownRow>,
}

/// The open chain, **nested**: each level is declared inside the one it opened
/// from, not beside it.
///
/// Nesting is not a stylistic choice, it is what makes the chain one surface.
/// `OUTSIDE_POINTER` is an ancestor test — a press is "outside" a layer when
/// the layer is not on the hit path — so with the levels declared as siblings a
/// press inside a *submenu* is outside the level above it, and the outermost
/// level dismisses the whole chain. Dismissal lands on the press, so by the
/// release there is no open menu left and the row's own click finds nothing to
/// activate: clicking a submenu item with the mouse did nothing at all.
///
/// Declaring the child inside its parent's subtree puts every level on the
/// path, so a press anywhere in the chain is inside all of it. Paint order is
/// unchanged — `resolve_layers` walks a worklist that grows as it goes, so a
/// nested layer resolves after its parent and paints after it too.
pub fn dropdown_chain(levels: &[DropdownLevel]) -> Option<Node<UiMsg>> {
    let mut inner: Option<Node<UiMsg>> = None;
    for (depth, level) in levels.iter().enumerate().rev() {
        inner = Some(dropdown(depth, level, inner.take()));
    }
    inner
}

fn dropdown(depth: usize, level: &DropdownLevel, nested: Option<Node<UiMsg>>) -> Node<UiMsg> {
    let rows: Vec<Node<UiMsg>> = level
        .rows
        .iter()
        .enumerate()
        .map(|(index, r)| {
            gesture(
                text(r.text.clone())
                    .theme(r.theme.clone())
                    .h(Sizing::Cells(1)),
            )
            // Stops, for the same reason the bar's labels do: the box
            // behind the rows closes the menu, and a row that only
            // answered would be followed by that close — which would shut
            // the menu on the way into a submenu.
            .on(
                GestureKind::Click,
                Rc::new(move |e: &Event| {
                    e.stop();
                    Some(UiMsg::Ui(UiFact::MenuItemClick { depth, index }))
                }),
            )
            // The hover machine decides what a row under the pointer
            // means — highlight, open a submenu, close the deeper ones.
            .on_leave(hover(None))
            .on_enter(hover(Some(if depth == 0 {
                // The bar index the reaction fills in for itself; it knows
                // which menu is open.
                HoverTarget::MenuDropdownItem(0, index)
            } else {
                HoverTarget::SubmenuItem(depth, index)
            })))
            .into()
        })
        .collect();

    let mut l = layer()
        .key(fresh_ui::Key::Pair("menu_dropdown".into(), depth as u64))
        // The rectangle the old placement walk chose, not a fresh one: while
        // the keyboard half is still legacy it must keep agreeing with the
        // cells.
        .anchor(Anchor::Point(level.x, level.y))
        .child(
            gesture({
                let mut b = col()
                    .border()
                    // Border ink over the dropdown ground; the fill draws
                    // spaces, so only the background of this key reaches the
                    // eye there.
                    .theme(crate::app::shell_host::shell_theme::pair(
                        "ui.menu_border_fg",
                        "ui.menu_dropdown_bg",
                    ))
                    .w(Sizing::Cells(level.width))
                    .children(rows);
                // The level this one opened, inside it. A layer is out of
                // flow, so it takes none of this box's space — it is here for
                // ancestry, which is what dismissal tests.
                if let Some(child) = nested {
                    b = b.child(child);
                }
                b
            })
            // An inert cell of the box — its border — closes the menu, which
            // is what a click inside the dropdown that hit no item always did.
            .on_click(|_| UiMsg::Ui(UiFact::CloseMenu)),
        );
    if depth == 0 {
        // **The close guard, replaced by a property.** A click anywhere else
        // closes the menu and is spent doing so.
        //
        // `Modality::None`, not `Exclusive`: the bar underneath must stay
        // live, because clicking another label is how a user switches menus,
        // and every platform does that in one press. Dismissal runs first and
        // the label's own click follows, so the pair reads "close this, open
        // that" — which is why the label's handler carries the menu's
        // open-ness from build time rather than asking after the close.
        l = l
            .modality(Modality::None)
            .dismiss(Dismiss::OUTSIDE_POINTER)
            .on_dismiss(|_| UiMsg::Ui(UiFact::CloseMenu));
    }
    l
}

/// The names a bar label carries, spelled once for the test fixtures below.
/// They are ordinary theme keys — the point of the grammar is that a test can
/// write them out and mean exactly what the editor means.
#[cfg(test)]
const ITEM: &str = "ui.menu_fg/ui.menu_bg";
#[cfg(test)]
const BAR: &str = "ui.menu_fg/ui.menu_bg";
#[cfg(test)]
const MNEMONIC: &str = "ui.menu_fg/ui.menu_bg+underline";
/// The active label is bold *and* its mnemonic underlined — two structural
/// attributes composing on one pair, which is the whole reason the grammar
/// replaced a name per combination.
#[cfg(test)]
const ACTIVE: &str = "ui.menu_active_fg/ui.menu_active_bg+bold";
#[cfg(test)]
const ACTIVE_MNEMONIC: &str = "ui.menu_active_fg/ui.menu_active_bg+bold+underline";

#[cfg(test)]
mod tests {
    use super::*;
    use crate::view::shell::fold::{fold_native, Band};
    use crate::view::shell::frame::{frame_tree, Frame};
    use crate::view::shell::msg::UiMsg;
    use fresh_ui::{Size, ThemeKey, Ui};
    use ratatui::buffer::Buffer;
    use ratatui::layout::Rect;
    use ratatui::style::Style;

    fn plain(_: &ThemeKey) -> Style {
        Style::default()
    }

    fn row_of(text: &str) -> DropdownRow {
        DropdownRow {
            text: text.to_string(),
            theme: crate::app::shell_host::shell_theme::pair(
                "ui.menu_dropdown_fg",
                "ui.menu_dropdown_bg",
            ),
        }
    }

    fn render(levels: Vec<DropdownLevel>, w: u16, h: u16) -> Buffer {
        let mut ui: Ui<UiMsg> = Ui::new();
        let frame = Frame {
            dropdowns: levels,
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

    fn line(buf: &Buffer, y: u16) -> String {
        (0..buf.area.width)
            .map(|x| buf[(x, y)].symbol().to_string())
            .collect()
    }

    /// A level lands on the cells its rectangle names, with the plain border
    /// glyphs the ratatui `Block` drew and its rows already padded to the
    /// box's content width — which is what `dropdown_item_text` produced for
    /// the painter this replaces.
    #[test]
    fn a_level_paints_its_box_where_it_was_placed() {
        let buf = render(
            vec![DropdownLevel {
                x: 1,
                y: 1,
                width: 12,
                rows: vec![row_of(" New      "), row_of(" Open     ")],
            }],
            20,
            8,
        );
        assert_eq!(line(&buf, 1), " ┌──────────┐       ", "top border");
        assert_eq!(line(&buf, 2), " │ New      │       ", "first row");
        assert_eq!(line(&buf, 3), " │ Open     │       ", "second row");
        assert_eq!(line(&buf, 4), " └──────────┘       ", "bottom border");
    }

    /// The bar row: `" Label "` per menu with a space between, on the bar's
    /// own ground — character for character what the `Paragraph` wrote.
    #[test]
    fn the_bar_paints_its_labels() {
        let mut ui: Ui<UiMsg> = Ui::new();
        let frame = Frame {
            menu_bar_items: MenuBar {
                items: vec![
                    BarItem {
                        runs: vec![
                            (" ".into(), ITEM.to_string()),
                            ("File".into(), ITEM.to_string()),
                            (" ".into(), ITEM.to_string()),
                            (" ".into(), BAR.to_string()),
                        ],
                        index: 0,
                    },
                    BarItem {
                        runs: vec![
                            (" ".into(), ITEM.to_string()),
                            ("Edit".into(), ITEM.to_string()),
                            (" ".into(), ITEM.to_string()),
                            (" ".into(), BAR.to_string()),
                        ],
                        index: 1,
                    },
                ],
            },
            ..Frame::default()
        };
        let spec = ui.frame(frame_tree(frame), Size::new(20, 4)).clone();
        let mut buf = Buffer::empty(Rect::new(0, 0, 20, 4));
        fold_native(&spec, &mut buf, &plain, Band::Background);
        // `" File "` plus the separator space is 7 cells, exactly the stride
        // the label-area walk advances by.
        assert_eq!(line(&buf, 0), " File   Edit        ");
    }

    /// **The bar's labels are styled, and the test can see it.**
    ///
    /// Every shell test used to render through a palette that returned
    /// `Style::default()`, so a highlighted row, a bold label and an
    /// underlined mnemonic all came out identical and no assertion could tell
    /// them apart. The mnemonic run is the sharpest case: it differs from the
    /// characters either side of it *only* by its style.
    #[test]
    fn a_bar_label_carries_its_runs_styles() {
        use crate::view::shell::fold::test_palette;
        let bar = MenuBar {
            items: vec![BarItem {
                runs: vec![
                    (" ".into(), ACTIVE.to_string()),
                    ("F".into(), ACTIVE_MNEMONIC.to_string()),
                    ("ile".into(), ACTIVE.to_string()),
                ],
                index: 0,
            }],
        };
        let mut ui: Ui<UiMsg> = Ui::new();
        let spec = ui
            .frame(
                frame_tree(Frame {
                    menu_bar_items: bar,
                    ..Frame::default()
                }),
                Size::new(20, 4),
            )
            .clone();
        let mut buf = Buffer::empty(Rect::new(0, 0, 20, 4));
        fold_native(&spec, &mut buf, &test_palette::palette, Band::Background);

        assert_eq!(
            buf[(1, 0)].style(),
            test_palette::painted(ACTIVE_MNEMONIC),
            "the mnemonic is underlined and bold"
        );
        assert_eq!(
            buf[(2, 0)].style(),
            test_palette::painted(ACTIVE),
            "the character beside it is only bold"
        );
        assert_ne!(
            buf[(1, 0)].style(),
            buf[(2, 0)].style(),
            "and the two differ, which is the whole point of a run"
        );
    }

    /// **A display list is not a diff.** `Cell::set_style` patches, so an item
    /// painted over cells a legacy painter left behind inherited their
    /// modifiers — a dropdown over the active tab came out bold. The fold
    /// resets first; this is the cell-level assertion that catches it, and no
    /// test could make it while every palette style was `Style::default()`.
    #[test]
    fn a_dropdown_row_replaces_the_style_beneath_it_rather_than_patching_it() {
        use crate::view::shell::fold::test_palette;
        use ratatui::style::{Modifier, Style};

        let mut ui: Ui<UiMsg> = Ui::new();
        let spec = ui
            .frame(
                frame_tree(Frame {
                    dropdowns: vec![DropdownLevel {
                        x: 0,
                        y: 0,
                        width: 10,
                        rows: vec![DropdownRow {
                            text: " New    ".into(),
                            theme: crate::app::shell_host::shell_theme::pair(
                                "ui.menu_dropdown_fg",
                                "ui.menu_dropdown_bg",
                            ),
                        }],
                    }],
                    ..Frame::default()
                }),
                Size::new(20, 4),
            )
            .clone();

        // A legacy painter got here first and left bold cells behind.
        let mut buf = Buffer::empty(Rect::new(0, 0, 20, 4));
        for x in 0..20u16 {
            for y in 0..4u16 {
                buf[(x, y)].set_style(Style::default().add_modifier(Modifier::BOLD));
            }
        }
        fold_native(&spec, &mut buf, &test_palette::palette, Band::Overlay);

        assert_eq!(
            buf[(2, 1)].style(),
            test_palette::painted(&crate::view::ui::MenuRowStyle::Normal.shell_theme()),
            "the row says what its cells look like outright"
        );
        assert!(
            !buf[(2, 1)].style().add_modifier.contains(Modifier::BOLD),
            "the bold underneath is gone, not inherited"
        );
    }

    /// **A style inside a run.** The mnemonic is one underlined character in
    /// the middle of a label — text styled *within* itself, which is what
    /// `text_runs` exists for. Laying the three pieces out as siblings would
    /// let them wrap and truncate independently.
    #[test]
    fn a_mnemonic_is_its_own_run_inside_the_label() {
        let bar = MenuBar {
            items: vec![BarItem {
                runs: vec![
                    (" ".into(), ITEM.to_string()),
                    ("F".into(), MNEMONIC.to_string()),
                    ("ile".into(), ITEM.to_string()),
                    (" ".into(), ITEM.to_string()),
                    (" ".into(), BAR.to_string()),
                ],
                index: 0,
            }],
        };
        let mut ui: Ui<UiMsg> = Ui::new();
        let spec = ui
            .frame(
                frame_tree(Frame {
                    menu_bar_items: bar,
                    ..Frame::default()
                }),
                Size::new(20, 4),
            )
            .clone();
        let mut buf = Buffer::empty(Rect::new(0, 0, 20, 4));
        fold_native(&spec, &mut buf, &plain, Band::Background);
        assert_eq!(line(&buf, 0), " File               ");
        // The underline is a theme name, not a glyph: the run carrying it is
        // its own item, so a backend can style it alone.
        let items = spec.items_for(&crate::view::shell::frame::region_key(
            crate::view::shell::frame::HostRegion::MenuBar,
        ));
        assert!(
            items.iter().any(|i| i.theme.as_str() == MNEMONIC),
            "the mnemonic run must reach the display list under its own name"
        );
    }

    /// A migrated region is still a region: everything that asks for the menu
    /// bar's rectangle by name keeps getting an answer, now that no
    /// `Draw::Host` announces it.
    ///
    /// **And it is the chrome column's top row**, dock or no dock — which is
    /// what lets `shell_frame` derive the rect it walks the menu with instead
    /// of reading it back off the previous frame's tree. Build must not depend
    /// on layout; this is the fact that makes it unnecessary.
    #[test]
    fn the_bar_is_the_chrome_columns_top_row() {
        use crate::view::shell::frame::{region_rects, HostRegion};
        let bar_of = |f: Frame, size: Rect| {
            region_rects(f, size)
                .iter()
                .find(|(r, _)| *r == HostRegion::MenuBar)
                .expect("the menu bar still has a rectangle")
                .1
        };
        assert_eq!(
            bar_of(Frame::default(), Rect::new(0, 0, 30, 8)),
            Rect::new(0, 0, 30, 1)
        );
        // With a dock carved off the left, the bar starts where the chrome
        // column does and is only as wide as what is left.
        assert_eq!(
            bar_of(
                Frame {
                    dock: Some(9),
                    ..Frame::default()
                },
                Rect::new(0, 0, 40, 8)
            ),
            Rect::new(9, 0, 31, 1)
        );
    }

    /// **Declaration order is paint order.** A submenu opens to the right of
    /// the level it came from and overlaps its edge by one column; the deeper
    /// box must win those cells. The old renderer got this from painting the
    /// chain in order, and so does this — no rank states it.
    #[test]
    fn a_submenu_paints_over_the_level_it_opened_from() {
        let buf = render(
            vec![
                DropdownLevel {
                    x: 0,
                    y: 0,
                    width: 10,
                    rows: vec![row_of(" File   "), row_of(" More  >")],
                },
                DropdownLevel {
                    x: 9,
                    y: 1,
                    width: 10,
                    rows: vec![row_of(" Deep   ")],
                },
            ],
            22,
            8,
        );
        // Column 9 is the parent's right border on row 1, and the submenu's
        // left border lands on it.
        assert_eq!(line(&buf, 1), "│ File   ┌────────┐   ");
        assert_eq!(line(&buf, 2), "│ More  >│ Deep   │   ");
    }

    /// An overlay is out of flow: opening a menu does not move the frame
    /// underneath it.
    #[test]
    fn a_dropdown_does_not_move_the_frame() {
        use crate::view::shell::frame::{region_rects, HostRegion};
        let size = Rect::new(0, 0, 30, 8);
        let without = region_rects(Frame::default(), size);
        let with = region_rects(
            Frame {
                dropdowns: vec![DropdownLevel {
                    x: 2,
                    y: 1,
                    width: 10,
                    rows: vec![row_of(" New     ")],
                }],
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

    fn bar_item(label: &str, index: usize) -> BarItem {
        BarItem {
            runs: vec![
                (" ".into(), ITEM.to_string()),
                (label.into(), ITEM.to_string()),
                (" ".into(), ITEM.to_string()),
                (" ".into(), BAR.to_string()),
            ],
            index,
        }
    }

    /// A bar with `File` and `Edit`, and `File`'s dropdown open below it.
    fn open_menu(active: Option<usize>) -> Ui<UiMsg> {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            frame_tree(Frame {
                menu_bar_items: MenuBar {
                    items: vec![bar_item("File", 0), bar_item("Edit", 1)],
                },
                dropdowns: active
                    .map(|_| {
                        vec![DropdownLevel {
                            x: 0,
                            y: 1,
                            width: 12,
                            rows: vec![
                                DropdownRow {
                                    text: " New      ".into(),
                                    theme: crate::app::shell_host::shell_theme::pair(
                                        "ui.menu_dropdown_fg",
                                        "ui.menu_dropdown_bg",
                                    ),
                                },
                                DropdownRow {
                                    text: " Open     ".into(),
                                    theme: crate::app::shell_host::shell_theme::pair(
                                        "ui.menu_dropdown_fg",
                                        "ui.menu_dropdown_bg",
                                    ),
                                },
                            ],
                        }]
                    })
                    .unwrap_or_default(),
                ..Frame::default()
            }),
            Size::new(30, 10),
        );
        ui
    }

    fn facts(msgs: Vec<UiMsg>) -> Vec<UiFact> {
        msgs.into_iter()
            .map(|m| match m {
                UiMsg::Ui(f) => f,
                other => panic!("unexpected {other:?}"),
            })
            .collect()
    }

    fn press(ui: &mut Ui<UiMsg>, x: i32, y: i32) -> fresh_ui::Dispatch<UiMsg> {
        ui.dispatch(Input::press(
            Point::new(x, y),
            MouseButton::Left,
            Mods::NONE,
        ))
    }

    fn click(ui: &mut Ui<UiMsg>, x: i32, y: i32) -> Vec<UiFact> {
        let pos = Point::new(x, y);
        let mut out = ui
            .dispatch(Input::press(pos, MouseButton::Left, Mods::NONE))
            .msgs;
        out.extend(
            ui.dispatch(Input::release(pos, MouseButton::Left, Mods::NONE))
                .msgs,
        );
        out.into_iter()
            .map(|m| match m {
                UiMsg::Ui(f) => f,
                other => panic!("unexpected {other:?}"),
            })
            .collect()
    }

    /// A label toggles its menu — and says *only* that.
    ///
    /// **The exact list matters.** A press bubbles to every handler on its
    /// path, and the row behind the labels closes the menu. A label that
    /// answered without stopping produced `[MenuBarPress, CloseMenu]`: the
    /// menu opened and shut in one gesture. Asserting `contains` passed that
    /// happily; asserting the list is what catches it.
    #[test]
    fn pressing_a_bar_label_toggles_that_menu_and_says_nothing_else() {
        let mut ui = open_menu(None);
        let got = facts(press(&mut ui, 1, 0).msgs);
        assert_eq!(got, vec![UiFact::MenuBarPress { index: 0 }]);
    }

    /// A **right** press on a label opens nothing and claims nothing.
    ///
    /// The claim is the part that matters. Ctrl+Right-click is the theme
    /// inspector's gesture and it reaches the inspector through the legacy
    /// pre-band, which only runs on events the tree declined. Pre-migration
    /// the bar was routed from `MouseEventKind::Down(Left)` alone, so a right
    /// press never touched it; without the button guard the migrated label
    /// opens its menu *and* swallows the inspector.
    #[test]
    fn a_right_press_on_a_label_opens_nothing_and_is_not_claimed() {
        let mut ui = open_menu(None);
        let got = ui.dispatch(Input::press(
            Point::new(1, 0),
            MouseButton::Right,
            Mods::NONE,
        ));
        assert!(
            !got.claimed,
            "a right press must reach the legacy pre-band, not stop at the bar"
        );
        assert!(
            !got.msgs
                .iter()
                .any(|m| matches!(m, UiMsg::Ui(UiFact::MenuBarPress { .. }))),
            "got {:?}",
            got.msgs
        );
    }

    /// **The toggle is one gesture, and one dispatch.** Pressing the open
    /// menu's own label produces the dismissal *and* the toggle together, so
    /// the applier can still see which menu was open before either ran. Split
    /// across press and release it could not: the menu is shut by then, and
    /// the frame in between has rebuilt the tree.
    #[test]
    fn pressing_the_open_menus_label_reports_dismissal_and_toggle_together() {
        let mut ui = open_menu(Some(0));
        let got = facts(press(&mut ui, 1, 0).msgs);
        assert_eq!(
            got,
            vec![UiFact::CloseMenu, UiFact::MenuBarPress { index: 0 }]
        );
    }

    /// **The close guard, replaced by a property, without breaking the switch.**
    /// Clicking another label while a menu is open closes the first and opens
    /// the second from that one press — which is why the dropdown declares
    /// `Modality::None` rather than `Exclusive`: an exclusive layer would make
    /// the bar underneath inert and cost the user a click.
    #[test]
    fn clicking_another_label_closes_one_menu_and_opens_the_other() {
        let mut ui = open_menu(Some(0));
        let got = facts(press(&mut ui, 8, 0).msgs);
        // Dismissal first, then the label: close this, open that. Nothing
        // after, or the open would be undone.
        assert_eq!(
            got,
            vec![UiFact::CloseMenu, UiFact::MenuBarPress { index: 1 }]
        );
    }

    /// A click outside everything closes the menu, and is spent doing it —
    /// what the full-frame `chrome:menu_close_guard` box did, now declared.
    #[test]
    fn clicking_outside_closes_and_is_spent() {
        let mut ui = open_menu(Some(0));
        let d = press(&mut ui, 25, 8);
        let facts: Vec<UiFact> = d
            .msgs
            .into_iter()
            .map(|m| match m {
                UiMsg::Ui(f) => f,
                other => panic!("unexpected {other:?}"),
            })
            .collect();
        assert!(facts.contains(&UiFact::CloseMenu), "got {facts:?}");
        assert!(d.claimed, "closing is the whole of that click");
    }

    /// A dropdown row activates itself, named by its level and position rather
    /// than by a cell the hit-test has to turn back into an index.
    /// A row activates itself and says nothing else. The box behind the rows
    /// closes the menu, so a row that did not stop would shut the menu on the
    /// way *into* a submenu.
    #[test]
    fn clicking_a_dropdown_row_activates_it_and_says_nothing_else() {
        let mut ui = open_menu(Some(0));
        assert_eq!(
            click(&mut ui, 3, 2),
            vec![UiFact::MenuItemClick { depth: 0, index: 0 }]
        );
        let mut ui = open_menu(Some(0));
        assert_eq!(
            click(&mut ui, 3, 3),
            vec![UiFact::MenuItemClick { depth: 0, index: 1 }]
        );
    }

    /// A click on the box but not on a row — its border — closes the menu,
    /// which is what any non-item click inside the dropdown always did.
    #[test]
    fn clicking_an_inert_cell_of_the_box_closes_the_menu() {
        let mut ui = open_menu(Some(0));
        let got = click(&mut ui, 0, 1);
        assert!(got.contains(&UiFact::CloseMenu), "got {got:?}");
        assert!(
            !got.iter()
                .any(|f| matches!(f, UiFact::MenuItemClick { .. })),
            "the border is not a row: {got:?}"
        );
    }

    /// Hovering reports where the pointer is; what the menu does about it is
    /// the existing reaction, which did not have to move.
    #[test]
    fn hovering_reports_the_target_under_the_pointer() {
        use crate::app::types::HoverTarget;
        let mut ui = open_menu(Some(0));
        let msgs = ui
            .dispatch(Input::Move {
                pos: Point::new(3, 3),
                mods: Mods::NONE,
            })
            .msgs;
        assert!(
            msgs.iter().any(|m| matches!(
                m,
                UiMsg::Ui(UiFact::Hover(Some(HoverTarget::MenuDropdownItem(_, 1))))
            )),
            "got {msgs:?}"
        );
    }

    /// **A migrated surface reports its hover without swallowing the move.**
    ///
    /// Claiming looked right — the surface owns its cells — and cost far more
    /// than it bought: a `Move` claimed at the top row killed the plugin
    /// `mouse_move` hook, the terminal-link and LSP hover trackers, and any
    /// text-selection drag whose pointer crossed row 0 (issue #3006's own test
    /// drags to row 0). The tree reports; it does not consume.
    #[test]
    fn a_move_over_the_bar_reports_hover_without_claiming() {
        use crate::app::types::HoverTarget;
        let mut ui = open_menu(None);
        let d = ui.dispatch(Input::Move {
            pos: Point::new(1, 0),
            mods: Mods::NONE,
        });
        assert!(!d.claimed, "a hover is not a claim");
        // The row is entered before the label inside it, so the ground's
        // "nothing" arrives first and the label's answer overwrites it.
        assert!(
            matches!(
                d.msgs.last(),
                Some(UiMsg::Ui(UiFact::Hover(Some(HoverTarget::MenuBarItem(0)))))
            ),
            "got {:?}",
            d.msgs
        );
    }

    /// **Moving *within* one label says nothing, and that is the point.**
    ///
    /// `Enter` fires once on the way in. A scheme that asked "did the tree
    /// answer this event?" therefore reported no on every motion after the
    /// first, and whatever else owned the field cleared the highlight — which
    /// is why the hover the tree reports has a home of its own rather than a
    /// flag saying who wrote last.
    #[test]
    fn moving_within_a_label_reports_nothing_further() {
        let mut ui = open_menu(None);
        let _ = ui.dispatch(Input::Move {
            pos: Point::new(1, 0),
            mods: Mods::NONE,
        });
        let d = ui.dispatch(Input::Move {
            pos: Point::new(2, 0),
            mods: Mods::NONE,
        });
        assert!(
            d.msgs.is_empty(),
            "same label, nothing changed: {:?}",
            d.msgs
        );
    }

    /// Leaving a label clears it: the tree owns this hover outright, so it
    /// must say when there is nothing under the pointer too.
    #[test]
    fn leaving_a_label_clears_the_hover() {
        let mut ui = open_menu(None);
        let _ = ui.dispatch(Input::Move {
            pos: Point::new(1, 0),
            mods: Mods::NONE,
        });
        let d = ui.dispatch(Input::Move {
            pos: Point::new(60, 5),
            mods: Mods::NONE,
        });
        assert!(
            d.msgs
                .iter()
                .any(|m| matches!(m, UiMsg::Ui(UiFact::Hover(None)))),
            "got {:?}",
            d.msgs
        );
    }
}

#[cfg(test)]
mod submenu_regression {
    use super::*;
    use crate::view::shell::frame::{frame_tree, Frame};
    use fresh_ui::{Input, Mods, MouseButton, Point, Size, Ui};

    /// A two-level chain: File's dropdown at (0,1), its submenu to the right.
    fn open_chain() -> Ui<UiMsg> {
        let row = |t: &str| DropdownRow {
            text: t.to_string(),
            theme: crate::app::shell_host::shell_theme::pair(
                "ui.menu_dropdown_fg",
                "ui.menu_dropdown_bg",
            ),
        };
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            frame_tree(Frame {
                dropdowns: vec![
                    DropdownLevel {
                        x: 0,
                        y: 1,
                        width: 12,
                        rows: vec![row(" New      "), row(" More    >")],
                    },
                    DropdownLevel {
                        x: 11,
                        y: 2,
                        width: 12,
                        rows: vec![row(" Deep     ")],
                    },
                ],
                ..Frame::default()
            }),
            Size::new(40, 12),
        );
        ui
    }

    fn facts(msgs: Vec<UiMsg>) -> Vec<UiFact> {
        msgs.into_iter()
            .map(|m| match m {
                UiMsg::Ui(f) => f,
                other => panic!("unexpected {other:?}"),
            })
            .collect()
    }

    fn click(ui: &mut Ui<UiMsg>, x: i32, y: i32) -> Vec<UiFact> {
        let pos = Point::new(x, y);
        let mut out = ui
            .dispatch(Input::press(pos, MouseButton::Left, Mods::NONE))
            .msgs;
        out.extend(
            ui.dispatch(Input::release(pos, MouseButton::Left, Mods::NONE))
                .msgs,
        );
        facts(out)
    }

    /// **Clicking a submenu row activates it, and does not close the chain.**
    ///
    /// The levels were declared as sibling layers, and `OUTSIDE_POINTER` is an
    /// ancestor test — so a press inside the *submenu* counted as outside the
    /// level above it and the outermost layer dismissed the lot. Dismissal
    /// lands on the press, so by the release there was no open menu and the
    /// row's own click found nothing to activate: clicking a submenu item with
    /// the mouse did nothing at all. Every submenu test was keyboard-driven,
    /// so nothing caught it.
    #[test]
    fn clicking_a_submenu_row_activates_it_and_keeps_the_chain_open() {
        let mut ui = open_chain();
        // The depth-1 box spans x 11..23, y 2..5; its one row sits at y 3.
        assert_eq!(
            click(&mut ui, 14, 3),
            vec![UiFact::MenuItemClick { depth: 1, index: 0 }]
        );
    }

    /// The parent level still answers its own rows, now that its child is
    /// declared inside it.
    #[test]
    fn clicking_a_parent_row_still_activates_that_row() {
        let mut ui = open_chain();
        assert_eq!(
            click(&mut ui, 4, 2),
            vec![UiFact::MenuItemClick { depth: 0, index: 0 }]
        );
    }

    /// **Nesting must not cost the close guard.** A press genuinely outside
    /// the whole chain still dismisses it — that is the outermost layer's
    /// `OUTSIDE_POINTER`, and nesting only changed what counts as inside.
    #[test]
    fn clicking_outside_the_whole_chain_still_dismisses() {
        let mut ui = open_chain();
        let press = ui.dispatch(Input::press(
            Point::new(35, 10),
            MouseButton::Left,
            Mods::NONE,
        ));
        assert!(
            facts(press.msgs).contains(&UiFact::CloseMenu),
            "outside the chain is still outside"
        );
    }
}
