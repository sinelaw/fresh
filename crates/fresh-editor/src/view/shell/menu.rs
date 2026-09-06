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
//! **Placement is the tree's.** A level carries no rectangle: it says what it
//! hangs off — the bar label that opened it, or the parent row of the level
//! above — and the rows it holds, as content. The box is as wide as its
//! widest row (`Sizing::Auto`), the top level hangs `Place::Below` its label,
//! a submenu `Place::RightOf` its parent row with the one-row rise its border
//! needs said as an anchor offset, and `Fit` keeps every level on screen —
//! clamped for the top level, flipped to the left for a submenu that would
//! run off the right edge. [`describe`] derives the content from the menu
//! state; the walk that decided all of this by hand
//! (`MenuRenderer::compute_layout`) is deleted, and the web reads the same
//! rectangles the tree produced (design §3.4).

use std::rc::Rc;

use fresh_ui::{
    col, gesture, layer, row, text, text_runs, Anchor, Dismiss, Event, GestureKind, Modality, Node,
    Run, Sizing,
};

use crate::app::types::HoverTarget;

use super::msg::{MenuNav, UiFact, UiMsg};

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
/// **Memoised on the bar.** The menu bar is rebuilt every frame and
/// changes on almost none of them — a hover moving between labels, a menu
/// opening. `MenuBar` is `PartialEq` and is the whole of what this reads.
pub fn menu_bar(bar: &MenuBar) -> Node<UiMsg> {
    fresh_ui::memo(bar.clone(), build_menu_bar)
}

fn build_menu_bar(bar: &MenuBar) -> Node<UiMsg> {
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
            gesture(text_runs(runs).key(menu_label_key(index)))
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
                // the regression issue #2362's inspector test caught on the
                // search-options row, which carries the same guard.
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
    // Left only, like the labels above it and for the same reason: the
    // pre-migration close ran off `MouseEventKind::Down(Left)`. Answering a
    // right press here would close the menu on the way to the theme
    // inspector's Ctrl+Right — the gesture reaches it through the legacy
    // pre-band, which only sees what the tree declined.
    .on(
        GestureKind::Press,
        Rc::new(|e: &Event| {
            if e.button != fresh_ui::MouseButton::Left {
                return None;
            }
            Some(UiMsg::Ui(UiFact::CloseMenu))
        }),
    )
    .on_enter(hover(None))
}

/// A dropdown row's identity across rebuilds, unique per level.
///
/// `Pair` rather than a formatted string: this runs for every row of every
/// open level on every rebuild, and the chain rebuilds on each highlight move.
/// Depth and index pack into the one `u64` the key already carries.
pub fn dropdown_item_key(depth: usize, index: usize) -> fresh_ui::Key {
    fresh_ui::Key::Pair("menu_item".into(), ((depth as u64) << 32) | index as u64)
}

/// One row of one dropdown: what it says, and the name of how it looks.
///
/// The style half is [`MenuRowStyle`](crate::view::ui::MenuRowStyle)'s name;
/// the content half is what the row reads, with no width fitted into it — the
/// box is as wide as its widest row, and a row's slack is laid out, not
/// written as spaces.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DropdownRow {
    pub body: RowBody,
    pub theme: String,
}

/// What a dropdown row reads.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RowBody {
    /// `text` at the left edge, `trail` at the right (an accelerator, the
    /// submenu arrow), and the box's slack between them. An action's `text`
    /// carries its checkbox glyph; a label's has no trail.
    Item { text: String, trail: String },
    /// A rule across the box.
    Separator,
}

impl DropdownRow {
    /// A plain row with no trail, in the ordinary dropdown ink.
    pub fn plain(text: impl Into<String>) -> DropdownRow {
        DropdownRow {
            body: RowBody::Item {
                text: text.into(),
                trail: String::new(),
            },
            theme: crate::view::ui::MenuRowStyle::Normal.shell_theme(),
        }
    }
}

/// One level of an open dropdown chain: what it hangs off, and its rows.
///
/// **No rectangle.** `from` is the index of the bar label this level opened
/// from (depth 0) or of the parent row in the level above (deeper), and the
/// tree places the box against that node: below the label, right of the row.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DropdownLevel {
    pub from: usize,
    pub rows: Vec<DropdownRow>,
}

/// A bar label's identity: what a top-level dropdown anchors to, and what the
/// web reads a label's rectangle by.
pub fn menu_label_key(index: usize) -> fresh_ui::Key {
    fresh_ui::Key::Pair("menu_label".into(), index as u64)
}

/// A dropdown box's identity, by depth: the layer that is the box.
pub fn dropdown_key(depth: usize) -> fresh_ui::Key {
    fresh_ui::Key::Pair("menu_dropdown".into(), depth as u64)
}

/// A key the open menu answers, and what it means.
///
/// **This is the whole fix for the precedence bug.** The menu used to own a
/// capture-all key handler, and the `menu` section of the keymap was consulted
/// from inside the *legacy* walk — which runs after the shell is offered the
/// key. Anything a user bound there was swallowed before the keymap was asked.
///
/// Now the keymap flows *down* into the description as shortcuts, the way
/// state is supposed to reach a description, and the tree resolves key →
/// intent → action with no handler in front of it. A binding cannot be
/// pre-empted by the surface it is bound for.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct MenuShortcut {
    pub key: fresh_ui::KeyPress,
    pub intent: fresh_ui::Intent,
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
pub fn dropdown_chain(levels: &[DropdownLevel], keys: &[MenuShortcut]) -> Option<Node<UiMsg>> {
    let mut inner: Option<Node<UiMsg>> = None;
    for (depth, level) in levels.iter().enumerate().rev() {
        inner = Some(dropdown(depth, level, inner.take(), keys));
    }
    inner
}

/// The intents an open menu carries out, declared once on the outermost level.
///
/// Nothing here names a key. `Intent::Up` is "move to the previous item"
/// whether it arrived as Up, as `k`, or as whatever the user bound — the
/// binding table decides that, and it decides it in one place.
fn menu_intents(n: Node<UiMsg>, keys: &[MenuShortcut]) -> Node<UiMsg> {
    use fresh_ui::Intent;
    let mut n = fresh_ui::focusable(n)
        .autofocus()
        .action(Intent::Up, |_| nav(MenuNav::PrevItem))
        .action(Intent::Down, |_| nav(MenuNav::NextItem))
        .action(Intent::Left, |_| nav(MenuNav::Back))
        .action(Intent::Right, |_| nav(MenuNav::Forward))
        .action(Intent::Home, |_| nav(MenuNav::First))
        .action(Intent::End, |_| nav(MenuNav::Last))
        .action(Intent::Confirm, |_| nav(MenuNav::Activate))
        // Every key that means "close", not only `Esc`. The layer's `ESCAPE`
        // dismissal answered `Esc` alone, so a keymap's own close binding —
        // emacs Ctrl+G → `menu_close` → `Intent::Cancel` — resolved to an
        // intent nothing acted on.
        .action(Intent::Cancel, |_| UiMsg::Ui(UiFact::CloseMenu));
    //
    // `hjkl` were hard-coded arms in the handler this replaced and are bound in
    // no keymap, so without these they would simply have stopped working. They
    // are declared *before* the keymap's own shortcuts below, so a user who
    // binds those letters to something else still wins.
    for (ch, intent) in [
        ('h', Intent::Left),
        ('j', Intent::Down),
        ('k', Intent::Up),
        ('l', Intent::Right),
    ] {
        n = n.shortcut(fresh_ui::KeyPress::new(fresh_ui::KeyCode::Char(ch)), intent);
    }
    for s in keys {
        n = n.shortcut(s.key, s.intent);
    }
    n
}

fn nav(n: MenuNav) -> UiMsg {
    UiMsg::Ui(UiFact::MenuNav(n))
}

fn dropdown(
    depth: usize,
    level: &DropdownLevel,
    nested: Option<Node<UiMsg>>,
    keys: &[MenuShortcut],
) -> Node<UiMsg> {
    let rows: Vec<Node<UiMsg>> = level
        .rows
        .iter()
        .enumerate()
        .map(|(index, r)| {
            gesture(row_node(r))
                // Keyed for the same reason the dropdown box itself is: the chain
                // rebuilds whenever the highlight moves or a submenu opens, and an
                // unkeyed row's identity is its position in a list that changes.
                .key(dropdown_item_key(depth, index))
                // Stops, for the same reason the bar's labels do: the box
                // behind the rows closes the menu, and a row that only
                // answered would be followed by that close — which would shut
                // the menu on the way into a submenu.
                //
                // Left only: without the guard a *middle* press activates the
                // item — something no menu has ever done, here or before the
                // migration.
                //
                // **Press, not `Click`,** like the bar's labels and every other
                // migrated surface: `handle_menu_dropdown_click` ran from the
                // `Down(Left)` arm. A terminal sends press *and* release so the
                // two look alike there, but the web frontend synthesises the
                // press alone at the row's cell, and a `Click` handler never
                // fires for it.
                .on(
                    GestureKind::Press,
                    Rc::new(move |e: &Event| {
                        if e.button != fresh_ui::MouseButton::Left {
                            return None;
                        }
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

    let content = gesture({
        let mut b = col()
            .border()
            // Border ink over the dropdown ground; the fill draws spaces, so
            // only the background of this key reaches the eye there.
            .theme(crate::app::shell_host::shell_theme::pair(
                "ui.menu_border_fg",
                "ui.menu_dropdown_bg",
            ))
            // As wide as the widest row, as tall as the rows — and no taller
            // than the room it is placed in: a menu longer than the screen
            // shows what fits, which is what the old walk's `items_to_show`
            // did. **The width is the box's own answer now** (rule L15): a
            // row's slack is a flex spacer, and flex contributes nothing but
            // its floor to an intrinsic measure, so `Auto` is the widest row's
            // text and its trail — which is what `content_width` computed by
            // hand, in a second walk of the same rows, and is what its
            // deletion says.
            .w(Sizing::Auto)
            .clip(true)
            .children(rows);
        // The level this one opened, inside it. A layer is out of flow, so it
        // takes none of this box's space — it is here for ancestry, which is
        // what dismissal tests.
        if let Some(child) = nested {
            b = b.child(child);
        }
        b
    })
    // An inert cell of the box — its border — closes the menu, which is what a
    // click inside the dropdown that hit no item always did.
    //
    // On the **press**, like the rows it sits behind. It has to be the same
    // gesture as theirs: a row `stop()`s what it answers, and a stop only
    // reaches handlers of the gesture that was stopped. With the rows on
    // `Press` and this on `Click`, activating a row said
    // `[MenuItemClick, CloseMenu]` — the item ran and the menu shut on the way
    // into its own submenu.
    //
    // Every button but Right, which is what `Click` was derived for; a right
    // press is the context menu's.
    .on(
        GestureKind::Press,
        Rc::new(|e: &Event| {
            if e.button == fresh_ui::MouseButton::Right {
                return None;
            }
            Some(UiMsg::Ui(UiFact::CloseMenu))
        }),
    );

    // The chain's keyboard, on the level that is always present. Focus
    // properties belong to a `Focusable`, and the whole chain is one surface —
    // a submenu is declared *inside* the level that opened it, so the
    // outermost level's focusable covers every level.
    let content = if depth == 0 {
        menu_intents(content, keys)
    } else {
        content
    };

    // Where the box may go: the frame below the bar. The top level cannot be
    // pushed up over the bar by the clamp, and a box is measured against that
    // room, so a long menu is clipped to it rather than spilling off the
    // bottom.
    let mut l = layer()
        .key(dropdown_key(depth))
        .within(super::frame::below_bar_key())
        .child(content);
    l = match depth {
        // Under the label that opened it, pulled back inside the frame at
        // the right edge — `fit_dropdown_area`'s clamp.
        0 => l
            .anchor(Anchor::Node(menu_label_key(level.from)))
            .place(fresh_ui::Place::Below)
            .fit(fresh_ui::Fit::CLAMP),
        // Right of the parent row, its top border one row above that row so
        // its first item aligns with it — the rise `render_dropdown_chain`
        // computed as `dropdown_rect.y + submenu_idx`. Its left border lands
        // on the parent's right border column, because the row ends one
        // column inside it. Off the right edge it flips to the left, where
        // its right border shares the parent's left one for the same reason.
        d => l
            .anchor(Anchor::Node(dropdown_item_key(d - 1, level.from)))
            .place(fresh_ui::Place::RightOf)
            .offset(0, -1)
            .fit(fresh_ui::Fit::FLIP.or(fresh_ui::Fit::CLAMP)),
    };
    if depth == 0 {
        // **The close guard, replaced by a property.** A click anywhere else
        // closes the menu and is spent doing so.
        //
        // `Modality::Keyboard`: an open menu owns every key — a printable one
        // must not reach the buffer underneath and type into the document —
        // while the bar underneath stays live to the pointer, because
        // clicking another label is how a user switches menus and every
        // platform does that in one press. Dismissal runs first and the
        // label's own click follows, so the pair reads "close this, open
        // that" — which is why the label's handler carries the menu's
        // open-ness from build time rather than asking after the close.
        //
        // It read `Modality::None` while the library had one knob for both
        // channels, and the cost was a whole input handler
        // (`view::ui::menu_input`) whose only remaining job was to answer
        // "consumed" for every key the menu had nothing to say about.
        l = l
            .modality(Modality::Keyboard)
            // Pointer only. Closing on a *key* is `Intent::Cancel`, handled in
            // `menu_intents` — one mechanism for every key that means "close",
            // not just for `Esc`.
            //
            // `ESCAPE` dismissal used to be declared here instead, and it
            // closed on `Esc` correctly. What it could not do is close on
            // anything else: the emacs keymap binds Ctrl+G in the `menu`
            // context to `menu_close`, which `menu_shortcuts` already turns
            // into `Intent::Cancel` — and with `Cancel` unhandled that intent
            // reached nobody, so Ctrl+G left the menu open
            // (`menu_ctrl_g_closes_the_menu`). `Esc` still closes: the library
            // maps it to `Intent::Cancel` intrinsically
            // (`focus/intent.rs`), so it arrives by the same route now rather
            // than by a second one.
            .dismiss(Dismiss::OUTSIDE_POINTER)
            .on_dismiss(|_| UiMsg::Ui(UiFact::CloseMenu));
    }
    l
}

/// A dropdown row as a node: one cell high, the row's own ink across its
/// whole width, and its content laid out inside it.
///
/// An item is its text at the left and its trail at the right with the box's
/// slack between them, so an accelerator sits against the border whatever
/// the widest row is — which the old walk got by padding every string to a
/// width it had computed first. A separator is a rule across the box, read
/// off the width layout settled rather than repeated to a count.
fn row_node(r: &DropdownRow) -> Node<UiMsg> {
    let n = match &r.body {
        RowBody::Item { text: lead, trail } => {
            let mut children: Vec<Node<UiMsg>> = vec![text(lead.clone()).h(Sizing::Cells(1))];
            if !trail.is_empty() {
                children.push(fresh_ui::widgets::spacer(1).h(Sizing::Cells(1)));
                children.push(text(format!("  {trail}")).h(Sizing::Cells(1)));
            }
            row().children(children)
        }
        RowBody::Separator => row().children([
            text(" ").h(Sizing::Cells(1)),
            fresh_ui::layout_reader(|info: fresh_ui::LayoutInfo| {
                text("\u{2500}".repeat(info.constraints.max_w as usize)).h(Sizing::Cells(1))
            })
            .w(Sizing::Flex(1)),
        ]),
    };
    n.h(Sizing::Cells(1)).theme(r.theme.clone())
}

/// What the menu shows, derived from its state.
pub struct MenuModel<'a> {
    /// Every menu, config and plugin, already expanded.
    pub menus: &'a [crate::config::Menu],
    pub state: &'a crate::view::ui::MenuState,
    pub keybindings: &'a crate::input::keybindings::KeybindingResolver,
    pub hover: Option<&'a HoverTarget>,
    pub mnemonics: bool,
}

/// The bar's labels and the open chain, as content.
///
/// **The walk `MenuRenderer::compute_layout` did, minus every rectangle.**
/// Which menus are visible, which label is active or hovered and where its
/// mnemonic is cut, which rows each open level holds and how each is styled —
/// all of that is the menu state's to say. Where a label sits and how wide a
/// box is are the tree's, and are read back off it (`scene::menu_view`).
pub fn describe(m: &MenuModel<'_>) -> (MenuBar, Vec<DropdownLevel>) {
    use crate::config::MenuItem;
    use crate::view::ui::menu::{
        is_checkbox_checked, is_menu_item_enabled, BarLabelStyle, MenuRowStyle,
    };
    let visible: Vec<bool> = m
        .menus
        .iter()
        .map(|menu| match &menu.when {
            Some(condition) => m.state.context.get(condition),
            None => true,
        })
        .collect();

    let mut bar = MenuBar::default();
    for (idx, menu) in m.menus.iter().enumerate() {
        if !visible[idx] {
            continue;
        }
        let is_active = m.state.active_menu == Some(idx);
        let is_hovered = matches!(m.hover, Some(HoverTarget::MenuBarItem(i)) if *i == idx);
        let style = BarLabelStyle::of(is_active, is_hovered);
        let mnemonic = match m.mnemonics {
            true => m.keybindings.find_menu_mnemonic(&menu.label),
            false => None,
        };
        // `" Label "` plus the separating space, cut at the mnemonic so that
        // one character can be underlined *inside* the label.
        let plain = style.shell_theme(false);
        let under = style.shell_theme(true);
        let mut runs: Vec<(String, String)> = vec![(" ".to_string(), plain.clone())];
        match mnemonic {
            Some(mn) => {
                let mut found = false;
                for c in menu.label.chars() {
                    let hit = !found && c.to_ascii_lowercase() == mn;
                    found |= hit;
                    let theme = if hit { &under } else { &plain };
                    match runs.last_mut() {
                        // Neighbouring characters in the same theme are one
                        // run: the cut exists for the mnemonic, not per
                        // character.
                        Some((t, th)) if th == theme => t.push(c),
                        _ => runs.push((c.to_string(), theme.clone())),
                    }
                }
            }
            None => runs.push((menu.label.clone(), plain.clone())),
        }
        runs.push((" ".to_string(), plain));
        // The gap to the next label wears the bar's ground, not this
        // label's.
        runs.push((
            " ".to_string(),
            crate::app::shell_host::shell_theme::pair("ui.menu_fg", "ui.menu_bg"),
        ));
        bar.items.push(BarItem { runs, index: idx });
    }

    let mut levels = Vec::new();
    let Some(active) = m.state.active_menu else {
        return (bar, levels);
    };
    let Some(menu) = m.menus.get(active) else {
        return (bar, levels);
    };
    let mut items: &[MenuItem] = &menu.items;
    let mut from = active;
    for depth in 0..=m.state.submenu_path.len() {
        let is_deepest = depth == m.state.submenu_path.len();
        let highlighted = match is_deepest {
            true => m.state.highlighted_item,
            false => Some(m.state.submenu_path[depth]),
        };
        let rows = items
            .iter()
            .enumerate()
            .map(|(idx, item)| {
                let has_open_submenu =
                    depth < m.state.submenu_path.len() && m.state.submenu_path[depth] == idx;
                let is_hovered = match depth {
                    0 => matches!(
                        m.hover,
                        Some(HoverTarget::MenuDropdownItem(mi, ii)) if *mi == active && *ii == idx
                    ),
                    _ => matches!(
                        m.hover,
                        Some(HoverTarget::SubmenuItem(d, ii)) if *d == depth && *ii == idx
                    ),
                };
                let enabled = is_menu_item_enabled(item, &m.state.context);
                let style = MenuRowStyle::of(
                    item,
                    enabled,
                    highlighted == Some(idx),
                    is_hovered,
                    has_open_submenu,
                );
                let body = match item {
                    MenuItem::Action {
                        label,
                        action,
                        checkbox,
                        ..
                    } => {
                        let accel = m
                            .keybindings
                            .find_keybinding_for_action(
                                action,
                                crate::input::keybindings::KeyContext::Normal,
                            )
                            .unwrap_or_default();
                        let icon = match checkbox {
                            Some(_) if is_checkbox_checked(checkbox, &m.state.context) => {
                                "\u{2611} "
                            }
                            Some(_) => "\u{2610} ",
                            None => "",
                        };
                        RowBody::Item {
                            text: format!(" {icon}{label}"),
                            trail: accel,
                        }
                    }
                    MenuItem::Separator { .. } => RowBody::Separator,
                    MenuItem::Submenu { label, .. } | MenuItem::DynamicSubmenu { label, .. } => {
                        RowBody::Item {
                            text: format!(" {label}"),
                            trail: ">  ".to_string(),
                        }
                    }
                    MenuItem::Label { info } => RowBody::Item {
                        text: format!(" {info}"),
                        trail: String::new(),
                    },
                };
                DropdownRow {
                    body,
                    theme: style.shell_theme(),
                }
            })
            .collect();
        levels.push(DropdownLevel { from, rows });
        if is_deepest {
            break;
        }
        let idx = m.state.submenu_path[depth];
        match items.get(idx) {
            Some(MenuItem::Submenu { items: sub, .. }) => {
                items = sub;
                from = idx;
            }
            // A dynamic submenu is expanded before it is entered
            // (`refresh_menu_content`); an unexpanded one has no level.
            _ => break,
        }
    }
    (bar, levels)
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
        DropdownRow::plain(text)
    }

    /// A bar with one `File` label, which is what a top-level dropdown
    /// hangs off: `" File "` starts at cell 0, so the box does too.
    fn a_bar() -> MenuBar {
        MenuBar {
            items: vec![BarItem {
                runs: vec![
                    (" ".into(), ITEM.to_string()),
                    ("File".into(), ITEM.to_string()),
                    (" ".into(), ITEM.to_string()),
                    (" ".into(), BAR.to_string()),
                ],
                index: 0,
            }],
        }
    }

    fn render(levels: Vec<DropdownLevel>, w: u16, h: u16) -> Buffer {
        let mut ui: Ui<UiMsg> = Ui::new();
        let frame = Frame {
            menu_bar_items: a_bar(),
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

    /// **A press alone activates a dropdown row — no release needed.**
    ///
    /// The same rule the status bar's segments hold, for the same reason:
    /// `handle_menu_dropdown_click` ran from the `Down(Left)` arm, and the web
    /// frontend forwards a chrome click as a synthetic mouse-down with no
    /// matching up. A `GestureKind::Click` handler needs the release.
    #[test]
    fn a_press_with_no_release_activates_a_dropdown_row() {
        use crate::view::shell::msg::UiFact;
        use fresh_ui::{Input, Mods, MouseButton, Point};
        let mut ui: Ui<UiMsg> = Ui::new();
        let frame = Frame {
            menu_bar_items: a_bar(),
            dropdowns: vec![DropdownLevel {
                from: 0,
                rows: vec![row_of(" New"), row_of(" Open")],
            }],
            ..Frame::default()
        };
        ui.frame(frame_tree(frame), Size::new(20, 8));
        let r = ui.rect_of(ui.find_by_key(&dropdown_item_key(0, 1)).expect("row 1"));
        let got = ui.dispatch(Input::press(
            Point::new(r.x + 1, r.y),
            MouseButton::Left,
            Mods::NONE,
        ));
        assert!(
            got.msgs
                .iter()
                .any(|m| matches!(m, UiMsg::Ui(UiFact::MenuItemClick { depth: 0, index: 1 }))),
            "a press alone must activate, got {:?}",
            got.msgs
        );
    }

    fn line(buf: &Buffer, y: u16) -> String {
        (0..buf.area.width)
            .map(|x| buf[(x, y)].symbol().to_string())
            .collect()
    }

    /// **A level hangs under the label that opened it, as wide as its widest
    /// row.** The box's left edge is the label's, its top row is the one
    /// under the bar, and its content width is the longest row's — the
    /// placement and the width the old walk computed by hand, read off the
    /// tree.
    #[test]
    fn a_level_paints_its_box_under_its_label_as_wide_as_its_rows() {
        let buf = render(
            vec![DropdownLevel {
                from: 0,
                rows: vec![row_of(" New"), row_of(" Open")],
            }],
            20,
            8,
        );
        assert_eq!(line(&buf, 1), "┌─────┐             ", "top border");
        assert_eq!(line(&buf, 2), "│ New │             ", "first row");
        assert_eq!(line(&buf, 3), "│ Open│             ", "second row");
        assert_eq!(line(&buf, 4), "└─────┘             ", "bottom border");
    }

    /// **An accelerator sits against the right border, whatever the widest
    /// row is.** A row is its text, the box's slack, and its trail — so the
    /// trail of a short row lands where the longest row ends, which the old
    /// walk got by padding every string to a width it had computed first.
    #[test]
    fn a_rows_trail_sits_at_the_right_edge_of_the_box() {
        let item = |text: &str, trail: &str| DropdownRow {
            body: RowBody::Item {
                text: text.into(),
                trail: trail.into(),
            },
            theme: crate::view::ui::MenuRowStyle::Normal.shell_theme(),
        };
        let buf = render(
            vec![DropdownLevel {
                from: 0,
                rows: vec![
                    item(" New", "C-n"),
                    item(" Reload with Encoding...", ""),
                    DropdownRow {
                        body: RowBody::Separator,
                        theme: crate::view::ui::MenuRowStyle::Separator.shell_theme(),
                    },
                ],
            }],
            40,
            8,
        );
        assert_eq!(line(&buf, 2), "│ New                 C-n│              ");
        assert_eq!(line(&buf, 3), "│ Reload with Encoding...│              ");
        assert_eq!(line(&buf, 4), "│ ───────────────────────│              ");
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
                    menu_bar_items: a_bar(),
                    dropdowns: vec![DropdownLevel {
                        from: 0,
                        rows: vec![row_of(" New")],
                    }],
                    ..Frame::default()
                }),
                Size::new(20, 5),
            )
            .clone();

        // A legacy painter got here first and left bold cells behind.
        let mut buf = Buffer::empty(Rect::new(0, 0, 20, 5));
        for x in 0..20u16 {
            for y in 0..5u16 {
                buf[(x, y)].set_style(Style::default().add_modifier(Modifier::BOLD));
            }
        }
        fold_native(&spec, &mut buf, &test_palette::palette, Band::Overlay);

        // The bar is row 0, the box's top border row 1, its first row 2.
        assert_eq!(
            buf[(2, 2)].style(),
            test_palette::painted(&crate::view::ui::MenuRowStyle::Normal.shell_theme()),
            "the row says what its cells look like outright"
        );
        assert!(
            !buf[(2, 2)].style().add_modifier.contains(Modifier::BOLD),
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

    /// **A submenu opens right of its parent row, one row up, and paints
    /// over the level it came from.** Its left border lands on the parent's
    /// right border column (the row ends one cell inside it), its top border
    /// sits one row above the parent row so its first item aligns with it,
    /// and — declaration order being paint order — the deeper box wins the
    /// shared column.
    #[test]
    fn a_submenu_paints_over_the_level_it_opened_from() {
        let more = DropdownRow {
            body: RowBody::Item {
                text: " More".into(),
                trail: ">".into(),
            },
            theme: crate::view::ui::MenuRowStyle::Highlighted.shell_theme(),
        };
        let buf = render(
            vec![
                DropdownLevel {
                    from: 0,
                    rows: vec![row_of(" File"), more],
                },
                DropdownLevel {
                    from: 1,
                    rows: vec![row_of(" Deep")],
                },
            ],
            22,
            8,
        );
        assert_eq!(line(&buf, 1), "┌────────┐            ");
        assert_eq!(line(&buf, 2), "│ File   ┌─────┐      ");
        assert_eq!(line(&buf, 3), "│ More  >│ Deep│      ");
        assert_eq!(line(&buf, 4), "└────────└─────┘      ");
    }

    /// **A submenu that would run off the right edge opens to the left.**
    /// Its right border then shares the parent's left border column, the
    /// mirror of the ordinary case.
    #[test]
    fn a_submenu_flips_left_at_the_right_edge() {
        let more = DropdownRow {
            body: RowBody::Item {
                text: " More".into(),
                trail: ">".into(),
            },
            theme: crate::view::ui::MenuRowStyle::Highlighted.shell_theme(),
        };
        // Two labels; the menu opens from `Edit`, whose label starts at cell
        // 7, so its box is ten wide at cells 7..17. A seven-wide submenu
        // right of the `More` row would end at cell 23 in a frame of 20.
        let mut ui: Ui<UiMsg> = Ui::new();
        let frame = Frame {
            menu_bar_items: MenuBar {
                items: [("File", 0), ("Edit", 1)]
                    .into_iter()
                    .map(|(label, index)| BarItem {
                        runs: vec![
                            (" ".into(), ITEM.to_string()),
                            (label.into(), ITEM.to_string()),
                            (" ".into(), ITEM.to_string()),
                            (" ".into(), BAR.to_string()),
                        ],
                        index,
                    })
                    .collect(),
            },
            dropdowns: vec![
                DropdownLevel {
                    from: 1,
                    rows: vec![row_of(" File"), more],
                },
                DropdownLevel {
                    from: 1,
                    rows: vec![row_of(" Deep")],
                },
            ],
            ..Frame::default()
        };
        let spec = ui.frame(frame_tree(frame), Size::new(20, 8)).clone();
        let mut buf = Buffer::empty(Rect::new(0, 0, 20, 8));
        fold_native(&spec, &mut buf, &plain, Band::Background);
        fold_native(&spec, &mut buf, &plain, Band::Overlay);
        // Left of the `More` row, one row up: the submenu's right border on
        // the parent's left border column, cell 7, and its top border on the
        // parent's first row.
        assert_eq!(line(&buf, 2), " ┌─────┐ File   │   ");
        assert_eq!(line(&buf, 3), " │ Deep│ More  >│   ");
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
                menu_bar_items: a_bar(),
                dropdowns: vec![DropdownLevel {
                    from: 0,
                    rows: vec![row_of(" New")],
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
                // Under `File`: a box seven cells wide, rows at y=2 and y=3.
                dropdowns: active
                    .map(|_| {
                        vec![DropdownLevel {
                            from: 0,
                            rows: vec![DropdownRow::plain(" New"), DropdownRow::plain(" Open")],
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

    /// A **right** press on the bar's ground says nothing and claims nothing.
    ///
    /// Tested with **no menu open**, deliberately. With one open the layer's
    /// own `OUTSIDE_POINTER` dismissal closes it on any press out there, which
    /// is the layer's business and not this handler's — asserting against that
    /// would be asserting the wrong thing. What this pins is the handler: a
    /// right press must not produce a message or a claim, so Ctrl+Right-click
    /// still reaches the theme inspector through the legacy pre-band.
    #[test]
    fn a_right_press_on_the_bar_ground_says_nothing() {
        let mut ui = open_menu(None);
        // x=20 is past both labels: the bar's ground, not a label.
        let got = ui.dispatch(Input::press(
            Point::new(20, 0),
            MouseButton::Right,
            Mods::NONE,
        ));
        // Bar-ground facts only: the frame-wide right-click observer
        // (`shell::splits::tab_menu_guard`) fires wherever the click lands.
        let said: Vec<_> = facts(got.msgs)
            .into_iter()
            .filter(|f| *f != UiFact::ClearTabMenus)
            .collect();
        assert!(said.is_empty(), "got {said:?}");
        assert!(!got.claimed, "a right press must reach the legacy pre-band");
    }

    /// A **left** press on that same ground still closes.
    ///
    /// The partner of the test above: the guard must not cost the close it
    /// was guarding.
    #[test]
    fn a_left_press_on_the_bar_ground_still_closes() {
        let mut ui = open_menu(None);
        let got = facts(press(&mut ui, 20, 0).msgs);
        assert_eq!(got, vec![UiFact::CloseMenu]);
    }

    /// A **middle** click on a dropdown row activates nothing.
    ///
    /// `Click` is derived for every button but Right, so the row answered a
    /// middle click by running its item — and `stop()`ing, so nothing else saw
    /// it either. (3,2) is inside the first row: the box is at y=1 and its
    /// border takes that line, so rows start at y=2.
    #[test]
    fn a_middle_click_on_a_dropdown_row_activates_nothing() {
        let mut ui = open_menu(Some(0));
        let pos = Point::new(3, 2);
        let mut msgs = ui
            .dispatch(Input::press(pos, MouseButton::Middle, Mods::NONE))
            .msgs;
        msgs.extend(
            ui.dispatch(Input::release(pos, MouseButton::Middle, Mods::NONE))
                .msgs,
        );
        assert!(
            !msgs
                .iter()
                .any(|m| matches!(m, UiMsg::Ui(UiFact::MenuItemClick { .. }))),
            "got {msgs:?}"
        );
    }

    /// A **left** click on that same row still activates it — the guard must
    /// not cost the activation it was guarding.
    #[test]
    fn a_left_click_on_a_dropdown_row_still_activates_it() {
        let mut ui = open_menu(Some(0));
        assert_eq!(
            click(&mut ui, 3, 2),
            vec![UiFact::MenuItemClick { depth: 0, index: 0 }]
        );
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

    /// **The toggle is one gesture, and one fact.** The dropdown hangs off
    /// its label by `Anchor::Node`, and the library does not count a press on
    /// a layer's anchor as outside it — pressing the button that opened a
    /// menu is "close it", on every platform — so no dismissal fires and the
    /// label's own press is the whole of what arrives. The applier closes on
    /// it because the menu was open when the press came in
    /// (`menu_open_before`).
    #[test]
    fn pressing_the_open_menus_label_reports_the_toggle_alone() {
        let mut ui = open_menu(Some(0));
        let got = facts(press(&mut ui, 1, 0).msgs);
        assert_eq!(got, vec![UiFact::MenuBarPress { index: 0 }]);
    }

    /// **The close guard, replaced by a property, without breaking the switch.**
    /// Clicking another label while a menu is open closes the first and opens
    /// the second from that one press — which is why the dropdown declares
    /// `Modality::Keyboard` rather than `Inert` or `Exclusive`: those take the
    /// pointer away from what is behind them, which would make the bar inert
    /// and cost the user a click. The keyboard is the only channel an open
    /// menu wants.
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
        let row = DropdownRow::plain;
        let more = DropdownRow {
            body: RowBody::Item {
                text: " More".into(),
                trail: ">".into(),
            },
            theme: crate::view::ui::MenuRowStyle::Highlighted.shell_theme(),
        };
        // `File`'s box: ten cells wide under cell 0, rows at y=2 (` New`)
        // and y=3 (` More  >`). The submenu: right of the `More` row, so
        // its left border is at x=9, its top border at y=2 and ` Deep` at
        // y=3, x=10.
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            frame_tree(Frame {
                menu_bar_items: MenuBar {
                    items: vec![BarItem {
                        runs: vec![
                            (" ".into(), ITEM.to_string()),
                            ("File".into(), ITEM.to_string()),
                            (" ".into(), ITEM.to_string()),
                            (" ".into(), BAR.to_string()),
                        ],
                        index: 0,
                    }],
                },
                dropdowns: vec![
                    DropdownLevel {
                        from: 0,
                        rows: vec![row(" New"), more],
                    },
                    DropdownLevel {
                        from: 1,
                        rows: vec![row(" Deep")],
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

#[cfg(test)]
mod intent_tests {
    use super::*;
    use crate::view::shell::frame::{frame_tree, Frame};
    use fresh_ui::{Input, Intent, KeyCode, KeyPress, Mods, Size, Ui};

    fn level() -> DropdownLevel {
        DropdownLevel {
            from: 0,
            rows: vec![DropdownRow::plain("New")],
        }
    }

    fn open(keys: Vec<MenuShortcut>) -> Ui<UiMsg> {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            frame_tree(Frame {
                menu_bar: true,
                dropdowns: vec![level()],
                menu_keys: keys,
                ..Frame::default()
            }),
            Size::new(40, 10),
        );
        ui
    }

    fn press(ui: &mut Ui<UiMsg>, code: KeyCode, mods: Mods) -> Vec<UiMsg> {
        ui.dispatch(Input::Key(KeyPress::with(code, mods))).msgs
    }

    /// The built-in arrows work with no keymap at all: they are the library's
    /// default shortcuts resolving to the intents the chain declares.
    #[test]
    fn the_default_arrows_navigate() {
        let mut ui = open(Vec::new());
        assert!(matches!(
            press(&mut ui, KeyCode::Down, Mods::NONE).as_slice(),
            [UiMsg::Ui(UiFact::MenuNav(MenuNav::NextItem))]
        ));
        assert!(matches!(
            press(&mut ui, KeyCode::Up, Mods::NONE).as_slice(),
            [UiMsg::Ui(UiFact::MenuNav(MenuNav::PrevItem))]
        ));
    }

    /// **An open menu owns the keys it declines**, which is what modal means
    /// to a keyboard: a printable key must not reach the buffer underneath
    /// and type into the document.
    ///
    /// This was a whole input handler (`view::ui::menu_input`) whose only
    /// remaining job was to return "consumed" — it existed because
    /// `Modality` was one knob for two channels, and the chain could not take
    /// the keyboard without also taking the pointer from the bar it hangs
    /// from. `Modality::Keyboard` says the one without the other, so the
    /// claim is the layer's property and the handler is gone.
    #[test]
    fn an_open_menu_swallows_what_it_does_not_act_on() {
        let mut ui = open(Vec::new());
        let d = ui.dispatch(Input::Key(KeyPress::with(KeyCode::Char('x'), Mods::NONE)));
        assert!(d.msgs.is_empty(), "nothing in the chain acts on it");
        assert!(
            d.claimed,
            "and it stops here rather than typing into the buffer"
        );
    }

    /// The other half: with no menu open the same key is nobody's, so the
    /// editor's own pipeline gets it.
    #[test]
    fn a_closed_menu_claims_nothing() {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            frame_tree(Frame {
                menu_bar: true,
                ..Frame::default()
            }),
            Size::new(40, 10),
        );
        assert!(
            !ui.dispatch(Input::Key(KeyPress::with(KeyCode::Char('x'), Mods::NONE)))
                .claimed
        );
    }

    /// **The bug this migration fixes.** A user binds `C-n` to `menu_down` in
    /// the `menu` section. Before, the menu's capture-all key handler ran
    /// first and swallowed it, and the keymap — consulted from inside the
    /// legacy walk, which runs *after* the shell sees the key — never got
    /// asked. The binding now arrives as a shortcut on the chain and resolves
    /// like any other.
    #[test]
    fn a_user_bound_key_reaches_the_menu() {
        let mut ui = open(vec![MenuShortcut {
            key: KeyPress::with(KeyCode::Char('n'), Mods::CTRL),
            intent: Intent::Down,
        }]);
        assert!(
            matches!(
                press(&mut ui, KeyCode::Char('n'), Mods::CTRL).as_slice(),
                [UiMsg::Ui(UiFact::MenuNav(MenuNav::NextItem))]
            ),
            "a menu-context binding must not be pre-empted by the menu"
        );
    }

    /// A binding overrides the default meaning of the same key rather than
    /// competing with it — one table decides, and it is the keymap's.
    #[test]
    fn a_binding_overrides_the_default_for_that_key() {
        let mut ui = open(vec![MenuShortcut {
            key: KeyPress::with(KeyCode::Down, Mods::NONE),
            intent: Intent::Up,
        }]);
        assert!(matches!(
            press(&mut ui, KeyCode::Down, Mods::NONE).as_slice(),
            [UiMsg::Ui(UiFact::MenuNav(MenuNav::PrevItem))],
        ));
    }

    /// Escape closes the menu, and it does so as the layer's dismissal rather
    /// than as an intent the chain claims.
    ///
    /// The first version of this test asserted only that no `MenuNav` was
    /// produced — which passes just as well when Escape does *nothing at all*,
    /// and that is exactly what it did: the chain declared `OUTSIDE_POINTER`
    /// dismissal and no `ESCAPE`, so deleting the old handler's `Esc` arm left
    /// the key unanswered by anyone. Assert the effect, not the absence.
    #[test]
    fn escape_closes_the_menu() {
        let mut ui = open(Vec::new());
        let got = press(&mut ui, KeyCode::Esc, Mods::NONE);
        assert!(
            matches!(got.as_slice(), [UiMsg::Ui(UiFact::CloseMenu)]),
            "got {got:?}"
        );
    }

    /// `hjkl` navigate, as they did before the migration — they were hard-coded
    /// arms in the old handler and are bound in no keymap.
    #[test]
    fn hjkl_navigate() {
        let mut ui = open(Vec::new());
        for (ch, want) in [
            ('j', MenuNav::NextItem),
            ('k', MenuNav::PrevItem),
            ('h', MenuNav::Back),
            ('l', MenuNav::Forward),
        ] {
            let got = press(&mut ui, KeyCode::Char(ch), Mods::NONE);
            assert!(
                matches!(got.as_slice(), [UiMsg::Ui(UiFact::MenuNav(n))] if *n == want),
                "{ch}: got {got:?}"
            );
        }
    }
}
