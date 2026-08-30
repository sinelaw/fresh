//! The settings dialog's box.
//!
//! Eleven modules and twenty thousand lines behind it; what moves here is the
//! outermost of the twenty-odd rectangles its painter records, which is the
//! one every other one is measured from. Same order as C.6 and the keybinding
//! editor: the frame first, the interior after.
//!
//! Ninety percent of the area, capped at 160 columns, centred with `area.x`
//! and `area.y` added back — the comment beside that addition says what it was
//! for: "centring with bare `area.width / 2` placed the modal at the FRAME
//! origin, where the dock then over-drew its left edge — hiding the title bar
//! and clipping the rounded top-left corner". Naming the region the layer may
//! occupy is that, said where the placing happens.
//!
//! **A rectangle, not a surface.** The interior is hit-tested against the
//! rectangles the painter records, and the modal slot is what routes a press
//! to it. A layer is offered the pointer before the ones below it and the
//! first with a path at the point wins, so a box that merely existed here
//! would swallow every click in the dialog. `PointerMode::Ignore` is what
//! keeps it geometry.

use std::rc::Rc;

use fresh_ui::{
    col, gesture, layout_reader, row, stack, text, viewport, Align, Anchor, Event, GestureKind,
    LayoutInfo, Modality, MouseButton, Node, Place, PointerMode, Scrim, Sizing,
};

use crate::app::shell_host::shell_theme::{attrs, pair};

use super::msg::UiFact;

use super::msg::UiMsg;

/// Below this many columns *inside* the box, the dialog lays itself out
/// narrow: its categories become a horizontal strip and its five footer
/// buttons stack. `inner_area.width < 60` is where the painter's two
/// renderers parted, and it is the same number here.
pub const NARROW_BELOW: u16 = 60;

/// Never wider than this, however wide the area is.
pub const MAX_WIDTH: u16 = 160;
/// Below this the dialog does not open at all — the painter writes
/// "[Terminal too small for settings]" instead.
pub const MIN_AREA: (u16, u16) = (40, 10);

pub fn key() -> fresh_ui::Key {
    fresh_ui::Key::Str("settings_modal".into())
}

/// The box's size in an area of `info`'s extent, or `None` when the area is
/// too small for the dialog to open.
pub fn fit(info: LayoutInfo) -> Option<(u16, u16)> {
    let (w, h) = (info.constraints.max_w, info.constraints.max_h);
    if w < MIN_AREA.0 || h < MIN_AREA.1 {
        return None;
    }
    Some(((w * 90 / 100).min(MAX_WIDTH), h * 90 / 100))
}

/// The dialog's box as a layer: centred beside the dock, with the chrome the
/// tree owns inside it and the painter's body between.
///
/// **Everything the tree does not own routes to the modal slot.** The panels
/// are still hit-tested by `settings/mouse.rs` against rectangles the painter
/// recorded, so a press there has to reach `handle_settings_mouse` — and a
/// layer is the first thing asked at a point, so it cannot simply let the hit
/// fall through: it has to say where it goes. The chrome nodes that *are* the
/// tree's stop their own presses before they get that far.
///
/// While there is no chrome — the box is empty, which is how it started — the
/// layer is `PointerMode::Ignore` instead: a rectangle and nothing else.
pub fn layer(c: Option<&Chrome>) -> Node<UiMsg> {
    let c = c.cloned();
    let l = fresh_ui::layer()
        .within(super::frame::chrome_key())
        .anchor(Anchor::Screen(Align::Center))
        .place(Place::Over);
    let l = match c.is_some() {
        true => l,
        false => l.pointer_mode(PointerMode::Ignore),
    };
    l.child(layout_reader(move |info: LayoutInfo| {
        let (w, h) = fit(info).unwrap_or((0, 0));
        // **Which layout is a question about this box, and it is answered
        // where the box is measured.** It used to be answered by asking the
        // *previous* frame's tree for this node's rectangle — so on the frame
        // that opened the dialog there was no node, the answer was "narrow",
        // and the dialog opened with a horizontal category strip and a
        // stacked footer at any terminal size, correcting itself only once
        // some later input rebuilt the description. `fit` has just derived
        // the width from the same constraints, so the question is already
        // answered here, one frame earlier and without a layout read.
        let wide = w.saturating_sub(2) >= NARROW_BELOW;
        let n = col().w(Sizing::Cells(w)).h(Sizing::Cells(h)).key(key());
        match &c {
            None => n.pointer_mode(PointerMode::Ignore),
            // One row of border, the search row, a blank gap, and then the
            // painter's body — `search_header_height + search_gap` in the
            // renderer, which is where it puts everything else.
            Some(c) => {
                // The body band: the category tree down its left, the
                // painter's panel to the right of the divider between them.
                // `Layout::horizontal([Length(24), Length(1), Min(40)])` is
                // where those three numbers come from — the tree's rectangle
                // was `chunks[0]` and the panel's `chunks[2]`, and the
                // painter reads the second back rather than splitting again.
                // The page: its header, then its cards in the window that
                // scrolls them. Both are the tree's in either layout.
                let page = || {
                    // A running search replaces the whole band with its
                    // results; otherwise it is the page's header and cards.
                    if let Some(r) = &c.results {
                        return col()
                            .w(Sizing::Flex(1))
                            .child(results(r).h(Sizing::Flex(1)));
                    }
                    let mut kids: Vec<Node<UiMsg>> = Vec::with_capacity(2);
                    if let Some(p) = &c.page {
                        kids.push(page_header(p));
                    }
                    kids.push(match &c.items {
                        Some(it) => items(it).h(Sizing::Flex(1)),
                        None => row().h(Sizing::Flex(1)).key(items_key()),
                    });
                    col().w(Sizing::Flex(1)).children(kids)
                };
                // **Which layout, not which parts are described.** The two
                // arrange the page differently — beside the category column,
                // or under the horizontal strip — and a search takes the tree
                // away without changing that. Reading `categories.is_none()`
                // as "narrow" put the wide layout's search results at the
                // box's left edge, straight over the divider.
                let body = match wide {
                    true => row().flex(1).children([
                        col()
                            .w(Sizing::Cells(CATEGORY_COLS))
                            .children(c.categories.iter().map(categories).collect::<Vec<_>>()),
                        // The divider column, which the painter draws.
                        row().w(Sizing::Cells(1)),
                        page().key(panel_key()),
                    ]),
                    // The narrow layout's categories are a horizontal strip:
                    // three rows with a rule under them, and the page below.
                    false => col().flex(1).children([
                        match &c.strip {
                            Some(s) => strip_band(s).h(Sizing::Cells(4)),
                            None => row().h(Sizing::Cells(4)),
                        },
                        page().h(Sizing::Flex(1)).key(panel_key()),
                    ]),
                };
                let mut rows: Vec<Node<UiMsg>> = vec![
                    // The box's own top border, which the painter draws.
                    row().h(Sizing::Cells(1)),
                    row()
                        .h(Sizing::Cells(1))
                        .children([row().w(Sizing::Cells(1)), search_row(&c.search)]),
                    // The blank row under the search bar —
                    // `search_header_height + search_gap` in the painter,
                    // which is where it measures its content band from.
                    row().h(Sizing::Cells(1)),
                    // The content band, inset by the box's border on each
                    // side. **Both insets are load-bearing now that the tree
                    // draws the body**: the band is what the painter reads
                    // back as its own `content_area`, and a band one column
                    // wide and one row tall too much put the panel over the
                    // divider between the two columns.
                    row().flex(1).children([
                        row().w(Sizing::Cells(1)),
                        body,
                        row().w(Sizing::Cells(1)),
                    ]),
                ];
                if let Some(f) = &c.footer {
                    // The separator the painter drew one row above the
                    // buttons, then the buttons, then the border. The rule is
                    // inset by the same cell as the buttons: the box's border
                    // is the painter's and a full-width rule drew straight
                    // over both of its sides.
                    rows.push(row().h(Sizing::Cells(1)).children([
                        row().w(Sizing::Cells(1)),
                        rule().flex(1),
                        row().w(Sizing::Cells(1)),
                    ]));
                    // One row across, five rows down — `footer_height` was 2
                    // and 7, and the second cell of indent is the narrow
                    // footer's `modal_area.x + 2`.
                    rows.push(
                        row()
                            .h(Sizing::Cells(match wide {
                                false => 5,
                                true => 1,
                            }))
                            .children([
                                row().w(Sizing::Cells(1 + !wide as u16)),
                                match wide {
                                    false => footer_column(f).flex(1),
                                    true => footer_row(f).flex(1),
                                },
                                row().w(Sizing::Cells(1)),
                            ]),
                    );
                    // The border's own row, which the painter draws.
                    rows.push(row().h(Sizing::Cells(1)));
                }
                route(n.children(rows))
            }
        }
    }))
}

/// Send every pointer event that reaches this node to the modal slot, which
/// routes it to `handle_settings_mouse`. The parts the tree owns stop the flow
/// before it gets here.
fn route(n: Node<UiMsg>) -> Node<UiMsg> {
    let mut g = gesture(n);
    for kind in [
        GestureKind::Press,
        GestureKind::Release,
        GestureKind::Move,
        GestureKind::Wheel,
    ] {
        g = g.on(
            kind,
            Rc::new(|e: &Event| {
                e.stop();
                Some(UiMsg::Ui(UiFact::ModalPointer(
                    super::modal::Slot::Settings,
                )))
            }),
        );
    }
    g
}

/// The width the painter's `Constraint::Length(24)` gave the tree.
pub const CATEGORY_COLS: u16 = 24;

pub fn categories_key() -> fresh_ui::Key {
    fresh_ui::Key::Str("settings_categories".into())
}

/// The settings panel's rectangle — the body band right of the divider.
///
/// The painter split the content area into `[Length(24), Length(1), Min(40)]`
/// and kept `chunks[2]`; the tree lays the same three out, so this is the
/// answer rather than a second split of the same row.
pub fn panel_key() -> fresh_ui::Key {
    fresh_ui::Key::Str("settings_panel".into())
}

/// The category tree as a `widgets::List` in the window it scrolls in.
///
/// **The rows answer their own presses and the list owns its window**, which
/// is the whole of what the painter recorded here: `layout.categories`,
/// `layout.sections`, `layout.disclosures`, `categories_scrollbar_area` and
/// `categories_panel_area` were five families of rectangle so a chain of
/// `point_in_rect` could turn a cell back into a row — and, for the
/// disclosure, back into a *column of* a row. A row knows its own index and a
/// chevron is a node beside the label.
///
/// The cursor is revealed by the list rather than by
/// `categories_scroll.ensure_visible`: the window is the element's, so the
/// wheel moves it without moving the selection and a keyboard move brings the
/// selection back into view. That is the behaviour the two of them had
/// between them, said once.
pub fn categories(c: &Categories) -> Node<UiMsg> {
    let rows = Rc::new(c.rows.clone());
    let focused = c.focused;
    let n = rows.len();
    let list = fresh_ui::List::windowed(
        n,
        |i| fresh_ui::Key::Pair("settings_cat".into(), i as u64),
        {
            let rows = rows.clone();
            let selected = c.selected;
            move |i| cat_row(&rows[i], selected == Some(i), focused)
        },
    )
    .focusable(false)
    .scrollbar()
    .row_theme(move |_, st| match (st, focused) {
        (
            fresh_ui::widgets::RowState::Selected | fresh_ui::widgets::RowState::SelectedBlur,
            true,
        ) => pair("ui.menu_highlight_fg", "ui.menu_highlight_bg"),
        (
            fresh_ui::widgets::RowState::Selected | fresh_ui::widgets::RowState::SelectedBlur,
            false,
        ) => pair("ui.menu_fg", "ui.selection_bg"),
        _ => ink(),
    })
    .on_select({
        let rows = rows.clone();
        move |i| match &rows[i] {
            CatRow::Category { idx, .. } => UiMsg::Ui(UiFact::SettingsCategory(*idx)),
            CatRow::Section { cat, section, .. } => {
                UiMsg::Ui(UiFact::SettingsCategorySection(*cat, *section))
            }
        }
    });
    let list = match (n, c.selected) {
        (0, _) | (_, None) => list,
        (_, Some(i)) => list.selected(i.min(n - 1)),
    };
    fresh_ui::ComponentExt::node(list).key(categories_key())
}

pub fn clear_key() -> fresh_ui::Key {
    fresh_ui::Key::Str("settings_clear_category".into())
}

/// The band the settings themselves are painted into.
///
/// The painter derived it by counting the header rows it had just drawn —
/// `header_height = y - header_start_y`, then `available_height = area.height
/// - header_height`. The header is a description now, so the band under it is
/// layout's answer and this is the key to read it back by.
/// One entry dialog's field window, re-exported so the host reads both
/// windows through one module.
pub fn entry_items_key(level: usize) -> fresh_ui::Key {
    super::entry::items_key(level)
}

pub fn items_key() -> fresh_ui::Key {
    fresh_ui::Key::Str("settings_items".into())
}

pub fn results_key() -> fresh_ui::Key {
    fresh_ui::Key::Str("settings_results".into())
}

/// Three rows a result: its name, its breadcrumb and its category. The list
/// is the one place that says so — the count row reads its window back rather
/// than dividing a band by it.
pub const RESULT_ROWS: u16 = 3;

/// The search's results: a card list, three rows to a card.
///
/// **A `List` is the window, the bar, the wheel and the drag at once.** The
/// painter windowed the results by hand (`skip(scroll_offset)`, break at
/// `height - 3`), drew its own scrollbar beside them, and filed a rectangle
/// per *visible* card — which is why the hit had to carry the absolute index
/// separately (#2860: the position in the filed list is a viewport slot, and
/// once scrolled it is not the result's index).
pub fn results(r: &Results) -> Node<UiMsg> {
    let rows: std::rc::Rc<Vec<ResultRow>> = std::rc::Rc::new(r.rows.clone());
    let n = rows.len();
    let list = fresh_ui::List::windowed(
        n,
        |i| fresh_ui::Key::Pair("settings_result".into(), i as u64),
        move |i| result_card(&rows[i]),
    )
    .row_rows(RESULT_ROWS)
    .focusable(false)
    .scrollbar()
    .scrollbar_theme(pair("ui.split_separator_fg", "ui.popup_bg"))
    .row_theme(|_, st| match st {
        fresh_ui::widgets::RowState::Selected | fresh_ui::widgets::RowState::SelectedBlur => {
            pair("ui.settings_selected_fg", "ui.settings_selected_bg")
        }
        fresh_ui::widgets::RowState::Hover => pair("ui.menu_hover_fg", "ui.menu_hover_bg"),
        _ => pair("ui.popup_text_fg", "ui.popup_bg"),
    })
    .on_select(|i| UiMsg::Ui(UiFact::SettingsSearchResult(i)))
    .on_activate(|i| UiMsg::Ui(UiFact::SettingsSearchResult(i)));
    let list = match n {
        0 => list,
        _ => list.selected(r.selected.min(n - 1)),
    };
    fresh_ui::ComponentExt::node(list).key(results_key())
}

/// One result's three rows. The `▸` marks the cursor; the row's own theme
/// says whether it is the selected one, so the marker is the only thing here
/// that has to know.
fn result_card(r: &ResultRow) -> Node<UiMsg> {
    let dim = pair("ui.line_number_fg", "ui.popup_bg");
    let mut name: Vec<Node<UiMsg>> = vec![text("  ")];
    name.extend(
        r.name
            .iter()
            .map(|s| text(s.text.clone()).theme(s.theme.clone())),
    );
    col().children([
        row().h(Sizing::Cells(1)).children(name),
        line(
            format!("  {}", r.breadcrumb),
            attrs("ui.line_number_fg", "ui.popup_bg", &["italic"]),
        ),
        match &r.desc {
            Some(d) => line(format!("  {d}"), dim).elide(fresh_ui::Elide::Tail),
            None => row().h(Sizing::Cells(1)),
        },
    ])
}

/// The narrow layout's category band: the names, the key hint, a blank row
/// and the rule under them — the painter's three-row strip plus its
/// separator.
///
/// **Each name answers its own press.** The painter tracked a rectangle per
/// category as it laid the row out and called the result "approximate" in its
/// own comment, which it was: it advanced by `name.len()`, so a category with
/// a non-ASCII name put every rectangle after it out by the difference
/// between its bytes and its columns.
fn strip_band(s: &Strip) -> Node<UiMsg> {
    let mut kids: Vec<Node<UiMsg>> = Vec::with_capacity(s.cats.len() * 2);
    for (n, c) in s.cats.iter().enumerate() {
        if n > 0 {
            kids.push(text(" │ ").theme(pair("ui.split_separator_fg", "ui.popup_bg")));
        }
        let label = match (c.selected, s.focused) {
            (true, true) => attrs("ui.menu_highlight_fg", "ui.menu_highlight_bg", &["bold"]),
            (true, false) => attrs("ui.menu_highlight_fg", "ui.popup_bg", &["bold"]),
            (false, _) => pair("ui.popup_text_fg", "ui.popup_bg"),
        };
        // The dot takes the highlight's colour whatever the name is doing.
        let dot = match c.dirty {
            true => pair("ui.menu_highlight_fg", "ui.popup_bg"),
            false => label.clone(),
        };
        let idx = c.idx;
        kids.push(
            gesture(
                row().children([
                    text(match c.dirty {
                        true => "● ",
                        false => "  ",
                    })
                    .theme(dot),
                    text(c.label.clone()).theme(label),
                ]),
            )
            .on(
                GestureKind::Press,
                Rc::new(move |e: &Event| {
                    if e.button != MouseButton::Left {
                        return None;
                    }
                    e.stop();
                    Some(UiMsg::Ui(UiFact::SettingsCategory(idx)))
                }),
            ),
        );
    }
    col().children([
        row().h(Sizing::Cells(1)).children(kids),
        line(s.hint.clone(), pair("ui.line_number_fg", "ui.popup_bg")),
        row().h(Sizing::Cells(1)),
        rule(),
    ])
}

/// The settings panel's header, and the band under it the painter still owns.
///
/// **The `[Clear …]` button answers its own press.** It was
/// `layout.clear_category_button` — a rectangle filed as the painter drew it,
/// for `hit_test` to compare a cell against.
fn page_header(p: &Page) -> Node<UiMsg> {
    let mut rows: Vec<Node<UiMsg>> = vec![line(
        p.title.clone(),
        attrs("ui.editor_fg", "ui.popup_bg", &["bold"]),
    )];
    if let Some(label) = &p.clear {
        let theme = match p.clear_hovered {
            true => pair("ui.menu_hover_fg", "ui.menu_hover_bg"),
            false => pair("ui.line_number_fg", "ui.popup_bg"),
        };
        rows.push(
            row().h(Sizing::Cells(1)).children([
                gesture(text(label.clone()).theme(theme))
                    .key(clear_key())
                    .on(
                        GestureKind::Press,
                        Rc::new(|e: &Event| {
                            if e.button != MouseButton::Left {
                                return None;
                            }
                            e.stop();
                            Some(UiMsg::Ui(UiFact::SettingsClearCategory))
                        }),
                    ),
                row().flex(1),
            ]),
        );
    }
    // The painter's blank row between the header and the first card.
    rows.push(row().h(Sizing::Cells(1)));
    col().children(rows)
}

/// One card's band, so the host can ask where it is — which is how the window
/// is moved to it (`Anchor::reveal_key`) and how "which card is at the top of
/// the window" is answered without a second measurement of every height.
pub fn card_key(i: usize) -> fresh_ui::Key {
    fresh_ui::Key::Pair("settings_card".into(), i as u64)
}

/// The page's cards, in the window that scrolls them.
///
/// **This is what `ItemBox` was.** The painter planned each item as five row
/// counts and five `_y()` accessors — section header, top border, control,
/// description, bottom border — computed by `SettingItem::layout_box`,
/// re-derived by `ScrollablePanel` to find the content's height, and clipped
/// band by band against a `BandViewport` while it drew. All three were the
/// same arithmetic in three places, which is the second layout tree goal 5
/// forbids. A `col` of cards in a `viewport` is one: the column measures each
/// card once, and the window is the thing that scrolls rather than an offset
/// every band subtracts for itself.
pub fn items(it: &Items) -> Node<UiMsg> {
    let body = col().children(it.cards.iter().map(card).collect::<Vec<_>>());
    let v = viewport(body)
        .key(items_key())
        .scrollbar()
        .scrollbar_theme(pair("ui.split_separator_fg", "ui.popup_bg"));
    match &it.anchor {
        Some(a) => v.anchor_to(a.clone()),
        None => v,
    }
}

/// One setting: its section heading when it starts one, and its card.
fn card(c: &Card) -> Node<UiMsg> {
    let mut rows: Vec<Node<UiMsg>> = Vec::new();
    if let Some(name) = &c.section {
        // The blank row goes *above* the heading, so the title reads as
        // "belongs to what is below" rather than to the card it follows. That
        // is what the painter's two-row `section_header_rows` band was.
        rows.push(row().h(Sizing::Cells(1)));
        rows.push(line(
            name.clone(),
            attrs("ui.editor_fg", "ui.popup_bg", &["bold"]),
        ));
    }
    rows.push(card_box(c));
    let idx = c.index;
    let mut g = gesture(col().key(card_key(c.index)).children(rows)).on(
        GestureKind::Press,
        Rc::new(move |e: &Event| {
            if e.button != MouseButton::Left {
                return None;
            }
            e.stop();
            Some(UiMsg::Ui(UiFact::SettingsItem(idx)))
        }),
    );
    // Hover is what the card's highlight reads, and the painter learned it by
    // hit-testing the pointer's cell against every item's rectangle on every
    // move. Entering and leaving are the two things that actually happen.
    g = g.on_enter(Rc::new(move |_: &Event| {
        Some(UiMsg::Ui(UiFact::SettingsItemHover(Some(idx))))
    }));
    g.on_leave(Rc::new(move |_: &Event| {
        Some(UiMsg::Ui(UiFact::SettingsItemHover(None)))
    }))
}

/// The bordered box: the three-column indicator gutter down its left, the
/// control, and the wrapped description under it.
fn card_box(c: &Card) -> Node<UiMsg> {
    let band = band_bg(c);
    let mut content: Vec<Node<UiMsg>> = vec![control(c, &band)];
    if let Some(d) = description(c) {
        content.push(d);
    }
    let mut body = col().w(Sizing::Flex(1)).children(content);
    // The `(Inherited)` badge and the `[Inherit]` button sit at the right end
    // of the control's *first* row — over a control that may be many rows
    // tall, so they are a one-row layer over the top of the column rather
    // than a sibling in it. Last in the stack, which is first at a point.
    if let Some(i) = &c.inherit {
        body = stack()
            .w(Sizing::Flex(1))
            .children([body, inherit(c.index, i, &band)]);
    }
    let inner = row().children([gutter(c, &band), body]);
    match c.bordered {
        true => col()
            .border()
            .theme(pair("ui.split_separator_fg", "ui.popup_bg"))
            .child(inner),
        false => inner,
    }
}

/// The background the card's first row is painted on: the selection band, the
/// hover band, or the dialog's own ground.
///
/// **The band is the control's, not a wash under it.** The painter drew an
/// empty highlighted row and then painted the control's cells over it, and
/// the cells whose background was *unset* let the band through. Every run of
/// a description carries both halves, so the band has to be the background the
/// control's own rows are built from — which is what [`super::widgets::Ctx`]'s
/// surface is for.
fn band_bg(c: &Card) -> String {
    match (c.selected, c.hovered) {
        (true, _) => "ui.settings_selected_bg".to_string(),
        (_, true) => "ui.menu_hover_bg".to_string(),
        _ => "ui.popup_bg".to_string(),
    }
}

/// The `>` cursor and the `●` pending-change dot, in the three columns the
/// painter reserved for them.
fn gutter(c: &Card, band: &str) -> Node<UiMsg> {
    let mark = match c.selected {
        true => ">",
        false => " ",
    };
    let dot = match c.dirty {
        true => "●",
        false => " ",
    };
    // One row tall, and no filler under it: the card's height comes from the
    // control beside it, and a flexible filler inside a `viewport` — whose
    // children are measured against an unbounded main axis — asks for every
    // row there could ever be.
    col()
        .w(Sizing::Cells(3))
        .child(row().h(Sizing::Cells(1)).children([
            text(mark).theme(attrs("ui.settings_selected_fg", band, &["bold"])),
            text(dot).theme(pair("ui.settings_selected_fg", band)),
            text(" ").theme(pair("ui.popup_text_fg", band)),
        ]))
}

/// The control itself — the same `WidgetSpec` the widget mapping produced,
/// described by the same adapter a plugin's panel goes through.
fn control(c: &Card, band: &str) -> Node<UiMsg> {
    let spec = c.spec.clone();
    let focus_key = c.focus_key.clone();
    let surface =
        crate::app::shell_host::shell_theme::Ink::keys("ui.popup_text_fg", band.to_string());
    layout_reader(move |info: LayoutInfo| {
        let cx = super::widgets::Ctx {
            slot: super::widgets::Slot::Settings,
            states: super::widgets::no_state(),
            focus_key: focus_key.clone(),
            hovered_key: None,
            marker_gutter: false,
            hovered_item_key: String::new(),
            avail_height: None,
            surface: surface.clone(),
        };
        super::widgets::node(&spec, info.constraints.max_w.max(1), &cx)
    })
}

/// The wrapped description, with the layer the value came from after it.
///
/// The painter wrapped the text itself and then pushed ` (user)` onto the last
/// line it had produced — so on a narrow card the label ran off the end. Here
/// it is part of the text and wraps with it.
fn description(c: &Card) -> Option<Node<UiMsg>> {
    // **Reflowed, not re-broken.** A schema description is written with
    // newlines for the sake of the JSON file it lives in, not as layout, and
    // the painter's `wrap_text` split on whitespace — which threw them away.
    // Keeping them costs a row per paragraph in a card whose height is the
    // reason the page scrolls at all.
    let mut body = c
        .description
        .as_deref()
        .map(|d| d.split_whitespace().collect::<Vec<_>>().join(" "))
        .unwrap_or_default();
    if let Some(l) = c.layer {
        match body.is_empty() {
            true => body = format!("({l})"),
            false => body.push_str(&format!(" ({l})")),
        }
    }
    if body.is_empty() {
        return None;
    }
    Some(
        row().children([
            text(body)
                .theme(pair("ui.line_number_fg", "ui.popup_bg"))
                .wrap()
                .w(Sizing::Flex(1)),
            // The painter's `description_right_padding_cols`, so wrapped text does
            // not butt against the card's right border.
            row().w(Sizing::Cells(2)),
        ]),
    )
}

/// `(Inherited)` or `[Inherit]`, flush right on the control's first row.
fn inherit(idx: usize, i: &Inherit, band: &str) -> Node<UiMsg> {
    let node = match i {
        Inherit::Badge(label) => {
            text(label.clone()).theme(attrs("ui.line_number_fg", band, &["italic"]))
        }
        Inherit::Button { label, hovered } => {
            let theme = match hovered {
                true => pair("ui.menu_hover_fg", "ui.menu_hover_bg"),
                false => pair("ui.line_number_fg", band),
            };
            gesture(text(label.clone()).theme(theme))
                .on(
                    GestureKind::Press,
                    Rc::new(move |e: &Event| {
                        if e.button != MouseButton::Left {
                            return None;
                        }
                        e.stop();
                        Some(UiMsg::Ui(UiFact::SettingsInherit(idx)))
                    }),
                )
                .on_enter(Rc::new(move |_: &Event| {
                    Some(UiMsg::Ui(UiFact::SettingsInheritHover(idx)))
                }))
        }
    };
    row().h(Sizing::Cells(1)).children([
        row().w(Sizing::Flex(1)).pointer_mode(PointerMode::Ignore),
        node,
        // The painter's trailing column, so the affordance is not flush
        // against the border.
        row().w(Sizing::Cells(1)).pointer_mode(PointerMode::Ignore),
    ])
}

/// One row: the cursor's `>`, the indent, the chevron, the dirty dot, the
/// icon and the label — the painter's own span order.
fn cat_row(r: &CatRow, selected: bool, focused: bool) -> Node<UiMsg> {
    // The row's own theme comes from `row_theme`; a run that differs from it
    // says so, and only two do.
    let marker = match selected && focused {
        true => ">",
        false => " ",
    };
    match r {
        CatRow::Category {
            idx,
            chevron,
            expandable,
            dirty,
            icon,
            label,
            elide,
        } => {
            // The chevron is its own target: the painter recorded a
            // one-column rectangle for it so a click there expanded the
            // category instead of selecting it.
            let idx = *idx;
            let chev = text(format!("{chevron} "));
            let chev = match expandable {
                true => gesture(chev).on(
                    GestureKind::Press,
                    Rc::new(move |e: &Event| {
                        if e.button != MouseButton::Left {
                            return None;
                        }
                        e.stop();
                        Some(UiMsg::Ui(UiFact::SettingsCategoryDisclosure(idx)))
                    }),
                ),
                false => chev,
            };
            let mut kids: Vec<Node<UiMsg>> = vec![text(marker), chev];
            kids.push(match dirty {
                true => text("● ").theme(pair("ui.menu_highlight_fg", "ui.popup_bg")),
                false => text("  "),
            });
            kids.push(text(icon.to_string()).theme(pair("ui.popup_border_fg", "ui.popup_bg")));
            // A plugin page's name is longer than the tree is wide, so the
            // painter clipped it with an ellipsis. `Sizing::Flex` gives the
            // label the rest of the row and the fold clips it; the ellipsis
            // is what the two do not share, and it stays in the description.
            kids.push(match elide {
                true => text(label.clone()).flex(1),
                false => text(label.clone()),
            });
            row().h(Sizing::Cells(1)).children(kids)
        }
        CatRow::Section { label, .. } => row().h(Sizing::Cells(1)).children([
            text(marker),
            // Four columns of indent, then the chevron column the category
            // rows spend on their arrow.
            text("     "),
            text("  "),
            text(" "),
            text(label.clone()),
        ]),
    }
}

fn search_row(s: &Search) -> Node<UiMsg> {
    let mut kids: Vec<Node<UiMsg>> = Vec::new();
    match s {
        Search::Hint(v) => kids.extend(
            v.iter()
                .map(|s| text(s.text.clone()).theme(s.theme.clone())),
        ),
        Search::Active { field, suffix } => {
            // Through the same adapter a plugin's field goes through. The
            // width it is given is the row's, which is what
            // `render_spec_no_autofocus(.., area.width)` was handed.
            kids.push(layout_reader({
                let field = field.clone();
                move |info: LayoutInfo| {
                    super::widgets::node(
                        &field,
                        info.constraints.max_w,
                        &super::widgets::Ctx::plain(super::widgets::Slot::Floating),
                    )
                }
            }));
            kids.extend(
                suffix
                    .iter()
                    .map(|s| text(s.text.clone()).theme(s.theme.clone())),
            );
        }
    }
    row().h(Sizing::Cells(1)).key(search_key()).children(kids)
}

/// One styled run of the modal's chrome.
#[derive(Clone, Debug, PartialEq)]
pub struct Span {
    pub text: String,
    pub theme: String,
}

impl Span {
    pub fn new(text: impl Into<String>, theme: impl Into<String>) -> Self {
        Span {
            text: text.into(),
            theme: theme.into(),
        }
    }
}

/// The modal's top row: a live query field, or the hint that opens one.
#[derive(Clone, Debug)]
pub enum Search {
    Hint(Vec<Span>),
    /// **The query is a `WidgetSpec::Text`**, which is how it was already
    /// painted — "the same `WidgetSpec` + `render_spec` path every settings
    /// field now uses, instead of hand-rolled cursor spans". It is a *node*
    /// now, through the same adapter a plugin's field goes through, which is
    /// what "there is no privileged internal surface" means.
    Active {
        field: std::rc::Rc<fresh_core::api::WidgetSpec>,
        /// The result count and scroll arrows, painted after the field at its
        /// rendered width.
        suffix: Vec<Span>,
    },
}

/// The settings modal's chrome: what the tree draws around a body the painter
/// still owns.
#[derive(Clone, Debug)]
pub struct Chrome {
    pub title: String,
    pub search: Search,
    /// The footer's buttons. `None` in the narrow layout, whose footer is
    /// seven rows rather than two and has not crossed.
    pub footer: Option<Footer>,
    /// The category tree down the left of the box. `None` in the narrow
    /// layout, whose categories are a horizontal strip, and while a search is
    /// running, when the painter replaces the whole body with its results.
    pub categories: Option<Categories>,
    /// That strip, in the narrow layout — the other half of the same choice.
    pub strip: Option<Strip>,
    /// The page's own header, above the settings. `None` while a search is
    /// running or the entry-dialog stack is up.
    pub page: Option<Page>,
    /// The search's results, in place of the page. `Some` exactly when the
    /// page is `None` for a running search — the two are the same band.
    pub results: Option<Results>,
    /// The page's settings, as cards.
    ///
    /// `None` under the same two conditions as the header: a search replaces
    /// the body with its results, and the entry-dialog stack is still the
    /// painter's — the tree is folded after every painter, so a described
    /// body would be drawn over the dialog covering it.
    pub items: Option<Items>,
}

/// The settings panel's header: the page's title, the button that clears a
/// nullable category, and the cards under both.
#[derive(Clone, Debug)]
pub struct Page {
    pub title: String,
    /// `[Clear …]`, offered on a nullable category that has values.
    pub clear: Option<String>,
    /// True while the pointer is on that button.
    pub clear_hovered: bool,
}

/// The page's cards and the handle its window is addressed by.
#[derive(Clone)]
pub struct Items {
    pub cards: Vec<Card>,
    /// The window's handle, so the host can move it to a card — which is the
    /// whole of what `ScrollablePanel::ensure_focused_visible` did, minus its
    /// copy of every item's height.
    pub anchor: Option<Rc<fresh_ui::behavior::Anchor>>,
}

impl std::fmt::Debug for Items {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Items")
            .field("cards", &self.cards)
            .finish_non_exhaustive()
    }
}

/// One setting, as the card that shows it.
#[derive(Clone, Debug)]
pub struct Card {
    /// The item's position on the page — the number every settings hit
    /// carries, and what `card_key` is built from.
    pub index: usize,
    /// The heading above this card, when it is the first of its section.
    pub section: Option<String>,
    /// The control, exactly as `widget_map` produced it.
    pub spec: fresh_core::api::WidgetSpec,
    /// The widget key the runtime should treat as focused: the control's own
    /// key while it is being edited, empty otherwise. It is what makes a text
    /// field paint its caret.
    pub focus_key: String,
    pub description: Option<String>,
    /// `user`, `project` or `session` — the layer the value came from, shown
    /// after the description. `None` for a schema default.
    pub layer: Option<&'static str>,
    /// The keyboard cursor is on this card and the body has the keyboard.
    pub selected: bool,
    /// The pointer is on it.
    pub hovered: bool,
    /// It has an unsaved change.
    pub dirty: bool,
    pub inherit: Option<Inherit>,
    /// Whether the card draws its box. The flat style drops it.
    pub bordered: bool,
}

/// What a nullable setting offers at the right of its first row.
#[derive(Clone, Debug, PartialEq)]
pub enum Inherit {
    /// `(Inherited)` — a label on a value that is already unset.
    Badge(String),
    /// `[Inherit]` — unsets it.
    Button { label: String, hovered: bool },
}

/// The category tree: the rows, the keyboard cursor, and whether that cursor
/// is the one with the keyboard.
#[derive(Clone, Debug, PartialEq)]
pub struct Categories {
    pub rows: Vec<CatRow>,
    /// The display index the cursor is on. The painter derived it from
    /// `selected_category` and `tree_cursor_section` at every row; it is one
    /// number here, and the row that matches it is the selected one.
    pub selected: Option<usize>,
    /// Whether the categories panel has the keyboard. It decides both the
    /// highlight's colour and whether the `>` is drawn, exactly as it did.
    pub focused: bool,
}

/// The search's results, which replace the page while a search is running.
#[derive(Clone, Debug, PartialEq)]
pub struct Results {
    pub rows: Vec<ResultRow>,
    pub selected: usize,
}

/// One result: three rows — its name with the query's matches picked out, the
/// breadcrumb that says where it lives, and its description.
#[derive(Clone, Debug, PartialEq)]
pub struct ResultRow {
    /// Already split at the match positions by the search, which is domain
    /// knowledge: what a fuzzy match *is* belongs to the matcher.
    pub name: Vec<Span>,
    pub breadcrumb: String,
    pub desc: Option<String>,
}

/// The narrow layout's categories: one row of names across the top, in place
/// of the column the wide layout puts down its left.
#[derive(Clone, Debug, PartialEq)]
pub struct Strip {
    pub cats: Vec<StripCat>,
    /// Whether the strip has the keyboard, which decides whether the selected
    /// name is banded or merely bright.
    pub focused: bool,
    /// The `←→: Switch category` line under it.
    pub hint: String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StripCat {
    pub idx: usize,
    pub label: String,
    /// A dot before a category with unsaved changes in it.
    pub dirty: bool,
    pub selected: bool,
}

/// One row of the category tree.
#[derive(Clone, Debug, PartialEq)]
pub enum CatRow {
    Category {
        idx: usize,
        /// `▼`, `▶` or a space — the painter's own three states.
        chevron: &'static str,
        expandable: bool,
        /// A dot beside a category with unsaved changes in it.
        dirty: bool,
        icon: &'static str,
        label: String,
        /// A plugin's page has a long name and the painter elided it.
        elide: bool,
    },
    Section {
        cat: usize,
        section: usize,
        label: String,
    },
}

pub fn search_key() -> fresh_ui::Key {
    fresh_ui::Key::Str("settings_search".into())
}

/// One of the modal's footer buttons.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Button {
    /// Cycles which config layer the dialog writes to.
    Layer,
    /// Resets the selected setting — labelled "Inherit" when it can be.
    Reset,
    Save,
    Cancel,
    /// Opens the layer's config file in the editor.
    Edit,
}

/// The footer's five buttons, with the labels and states already resolved.
#[derive(Clone, Debug, PartialEq)]
pub struct Footer {
    /// In the order the painter placed them: Edit is left-aligned and the
    /// rest are flush right, so the list is `[Edit, Layer, Reset, Save,
    /// Cancel]` and the row does the aligning.
    pub layer: String,
    pub reset: String,
    pub save: String,
    pub cancel: String,
    pub edit: String,
    /// Which one the keyboard is on, when the footer holds focus.
    pub focused: Option<Button>,
    pub hovered: Option<Button>,
    /// The key hints between `[ Edit ]` and the right-hand group, in the
    /// painter's own `Key:Action  Key:Action` form.
    pub help: String,
}

pub fn footer_key(b: Button) -> fresh_ui::Key {
    fresh_ui::Key::Pair(
        "settings_footer".into(),
        match b {
            Button::Layer => 0,
            Button::Reset => 1,
            Button::Save => 2,
            Button::Cancel => 3,
            Button::Edit => 4,
        },
    )
}

/// The footer row: `[ Edit ]` at the left, the rest flush right, and the
/// separator above it.
///
/// **Which buttons fit is a rule, not a measurement.** The painter summed the
/// widths and dropped Edit, then Layer, then Reset as the room ran out; that
/// stays, resolved against the width layout gives rather than against a
/// rectangle passed in. What goes is the five recorded rectangles and the
/// `hit_test` that compared a cell against each.
/// The narrow layout's footer: the same five buttons, one per row.
///
/// The painter's own order and its one dimmed button. Nothing about which
/// buttons fit is decided here — below sixty columns all five are shown and
/// the hints are not.
fn footer_column(f: &Footer) -> Node<UiMsg> {
    col().children(
        [
            (Button::Layer, &f.layer),
            (Button::Save, &f.save),
            (Button::Reset, &f.reset),
            (Button::Cancel, &f.cancel),
            (Button::Edit, &f.edit),
        ]
        .into_iter()
        .map(|(b, label)| {
            row()
                .h(Sizing::Cells(1))
                .children([button(f, b, label), row().flex(1)])
        })
        .collect::<Vec<_>>(),
    )
}

fn footer_row(f: &Footer) -> Node<UiMsg> {
    let f = f.clone();
    layout_reader(move |info: LayoutInfo| {
        let w = info.constraints.max_w;
        let width = |s: &str, b: Button| {
            crate::primitives::display_width::str_width(s) as u16 + u16::from(f.focused == Some(b))
        };
        let (save_w, cancel_w) = (
            width(&f.save, Button::Save),
            width(&f.cancel, Button::Cancel),
        );
        let (reset_w, layer_w, edit_w) = (
            width(&f.reset, Button::Reset),
            width(&f.layer, Button::Layer),
            width(&f.edit, Button::Edit),
        );
        let gap = 2u16;
        let min = save_w + gap + cancel_w;
        let show_reset = w >= reset_w + gap + min;
        let show_layer = w >= layer_w + gap + reset_w + gap + min;
        let show_edit = w >= edit_w + gap + layer_w + gap + reset_w + gap + min;

        let mut kids: Vec<Node<UiMsg>> = Vec::new();
        if show_edit {
            kids.push(button(&f, Button::Edit, &f.edit));
            kids.push(text("  ").theme(ink()));
        }
        // The hints, between `[ Edit ]` and the right-hand group, **inside**
        // the flexible child rather than beside it. The painter worked out the
        // gap's start and end from whichever buttons it had decided to show
        // and clipped the text to it; a flexible child *containing* them is
        // that gap, and the hints are clipped to it. Laid beside a bare
        // spacer they are fixed-width instead, so a help string longer than
        // the slack pushed the right-hand group off the box's edge and the
        // last button came out cut in half.
        kids.push(row().flex(1).children(keyhints(&f.help)));
        for (on, b, label) in [
            (show_layer, Button::Layer, &f.layer),
            (show_reset, Button::Reset, &f.reset),
            (true, Button::Save, &f.save),
            (true, Button::Cancel, &f.cancel),
        ] {
            if on {
                kids.push(button(&f, b, label));
                if b != Button::Cancel {
                    kids.push(text("  ").theme(ink()));
                }
            }
        }
        row().h(Sizing::Cells(1)).children(kids)
    })
    .h(Sizing::Cells(1))
}

/// `Key:Action  Key:Action` as runs: the key reverse-videoed, the action dim.
fn keyhints(text_: &str) -> Vec<Node<UiMsg>> {
    let key = pair("ui.popup_text_fg", "ui.split_separator_fg");
    let desc = pair("ui.line_number_fg", "ui.popup_bg");
    let mut out: Vec<Node<UiMsg>> = Vec::new();
    for (i, seg) in text_.split("  ").enumerate() {
        let seg = seg.trim();
        if seg.is_empty() {
            continue;
        }
        if i > 0 {
            out.push(text(" ").theme(desc.clone()));
        }
        match seg.find(':') {
            Some(at) => {
                out.push(text(format!(" {} ", &seg[..at])).theme(key.clone()));
                out.push(text(seg[at + 1..].to_string()).theme(desc.clone()));
            }
            None => out.push(text(seg.to_string()).theme(desc.clone())),
        }
    }
    out
}

fn button(f: &Footer, b: Button, label: &str) -> Node<UiMsg> {
    let focused = f.focused == Some(b);
    let theme = match (focused, f.hovered == Some(b)) {
        (true, _) => attrs("ui.menu_highlight_fg", "ui.menu_highlight_bg", &["bold"]),
        (false, true) => pair("ui.menu_hover_fg", "ui.menu_hover_bg"),
        (false, false) => ink(),
    };
    let marker = match focused {
        true => ">",
        false => "",
    };
    gesture(text(format!("{marker}{label}")).theme(theme))
        .key(footer_key(b))
        .on(
            GestureKind::Press,
            Rc::new(move |e: &Event| {
                if e.button != MouseButton::Left {
                    return None;
                }
                e.stop();
                Some(UiMsg::Ui(UiFact::SettingsButton(b)))
            }),
        )
        .on_enter(Rc::new(move |_: &Event| {
            Some(UiMsg::Ui(UiFact::SettingsButtonHover(Some(b))))
        }))
        .on_leave(Rc::new(move |_: &Event| {
            Some(UiMsg::Ui(UiFact::SettingsButtonHover(None)))
        }))
}

/// What a press on one of the settings dialogs asks for.
///
/// **These were computed twice.** The painter laid each dialog out, and the
/// mouse handler laid it out *again* to find the button — `get_confirm_dialog_
/// button_at` carries the comment "same as in `render_confirm_dialog`" and
/// "must match `render_confirm_dialog`", which is the duplication stated
/// outright. The nodes are the buttons now, and the fact says which.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Target {
    /// The unsaved-changes prompt: save, discard, cancel.
    Confirm(usize),
    /// The reset prompt: reset, cancel.
    Reset(usize),
    /// An entry dialog's "discard your edits?" prompt: keep editing, discard.
    EntryDiscard(usize),
    /// Its "delete this entry?" prompt: cancel, delete.
    EntryDelete(usize),
}

/// One `label   description` pair of the help overlay. An empty `desc` is a
/// section heading.
#[derive(Clone, Debug, PartialEq)]
pub struct HelpLine {
    pub key: String,
    pub desc: String,
    pub heading: bool,
}

/// A choice prompt: a question, the changes it is about, and the buttons.
#[derive(Clone, Debug, PartialEq)]
pub struct Choice {
    pub title: String,
    pub prompt: String,
    /// One line per pending change, listed under the prompt.
    pub changes: Vec<String>,
    pub buttons: Vec<String>,
    pub selected: usize,
    pub hovered: Option<usize>,
    pub help: String,
}

/// A two-button prompt where one of them destroys something.
///
/// **These had no mouse at all** — the painter drew them and only the keyboard
/// answered — which in a dialog where every other button is clickable reads as
/// a gap rather than a design. They answer a press now.
#[derive(Clone, Debug, PartialEq)]
pub struct Destructive {
    pub title: String,
    pub message: String,
    pub buttons: Vec<String>,
    pub selected: usize,
    /// Which button is the destructive one, tinted apart from the rest.
    pub destructive: usize,
    pub help: String,
    /// Whether the box's ring reads as an error rather than a warning.
    pub grave: bool,
    pub width: u16,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Dialog {
    Confirm(Choice),
    Reset(Choice),
    Help { title: String, lines: Vec<HelpLine> },
    EntryDiscard(Destructive),
    EntryDelete(Destructive),
}

pub fn dialog_key() -> fresh_ui::Key {
    fresh_ui::Key::Str("settings_dialog".into())
}

pub fn button_key(i: usize) -> fresh_ui::Key {
    fresh_ui::Key::Pair("settings_dialog_button".into(), i as u64)
}

/// A dialog as a layer over the settings box.
///
/// `apply_dimming(frame, modal_area)` before each is the scrim; `within` the
/// box is what "centre it in the modal, not the frame" means, and it is the
/// same `parent_area` every one of these was handed.
pub fn dialog_layer(d: &Dialog) -> Node<UiMsg> {
    let d = d.clone();
    fresh_ui::layer()
        .within(key())
        .anchor(Anchor::Screen(Align::Center))
        .place(Place::Over)
        .modality(Modality::Exclusive)
        .scrim(Some(Scrim::Dim))
        // **The topmost exclusive layer is where focus is**, so the claim has
        // to be on each of them, not only on the modal band underneath: with
        // a dialog up, the band is no longer on the focus path. They all name
        // the same slot because they are all the same interior — the settings
        // dispatcher answers its own sub-dialogs by priority.
        .child(super::modal::keys(
            super::modal::KeySlot::Settings,
            layout_reader(move |info: LayoutInfo| {
                // 50 wide, and as tall as it needs within 20 — the painter's own
                // two lines, with its `saturating_sub(4)` margin.
                let want_w = match &d {
                    Dialog::EntryDiscard(k) | Dialog::EntryDelete(k) => k.width,
                    _ => 50,
                };
                let w = want_w.min(info.constraints.max_w.saturating_sub(4));
                let want = match &d {
                    Dialog::Help { .. } => 20,
                    // Two borders, the title, the prompt, the blank under it,
                    // the rule, the buttons and the help line — then a row per
                    // change. The painter's own figure was seven, because it hung
                    // its title *in* the top border and this box gives it a row of
                    // its own; at seven the changes list was squeezed to nothing
                    // and the prompt promised a list that was not there.
                    Dialog::Confirm(c) | Dialog::Reset(c) => (8 + c.changes.len() as u16).min(20),
                    Dialog::EntryDiscard(_) | Dialog::EntryDelete(_) => 7,
                };
                let h = want.min(info.constraints.max_h.saturating_sub(4));
                let warn = pair("ui.status_warning_fg", "ui.popup_bg");
                let (ring, node) = match &d {
                    Dialog::Help { title, lines } => (
                        pair("ui.menu_highlight_fg", "ui.popup_bg"),
                        help_box(title, lines),
                    ),
                    Dialog::Confirm(c) => (warn.clone(), choice_box(c, Target::Confirm)),
                    Dialog::Reset(c) => (warn.clone(), choice_box(c, Target::Reset)),
                    Dialog::EntryDiscard(k) => (ring_of(k), grave_box(k, Target::EntryDiscard)),
                    Dialog::EntryDelete(k) => (ring_of(k), grave_box(k, Target::EntryDelete)),
                };
                col()
                    .theme(ring)
                    .border()
                    .w(Sizing::Cells(w))
                    .h(Sizing::Cells(h))
                    .key(dialog_key())
                    .children([node])
            }),
        ))
}

fn ink() -> String {
    pair("ui.popup_text_fg", "ui.popup_bg")
}

fn line(s: String, theme: String) -> Node<UiMsg> {
    text(s).theme(theme).h(Sizing::Cells(1))
}

fn rule() -> Node<UiMsg> {
    layout_reader(|info: LayoutInfo| {
        text("─".repeat(info.constraints.max_w.max(1) as usize))
            .theme(pair("ui.split_separator_fg", "ui.popup_bg"))
    })
    .h(Sizing::Cells(1))
}

fn help_box(title: &str, lines: &[HelpLine]) -> Node<UiMsg> {
    let mut rows: Vec<Node<UiMsg>> = vec![line(
        format!(" {title} "),
        attrs("ui.menu_highlight_fg", "ui.popup_bg", &["bold"]),
    )];
    for l in lines {
        rows.push(match l.heading {
            true => line(
                l.key.clone(),
                attrs("ui.popup_text_fg", "ui.popup_bg", &["bold"]),
            ),
            false => row().h(Sizing::Cells(1)).children([
                text(format!("  {:14}", l.key)).theme(attrs(
                    "ui.help_key_fg",
                    "ui.popup_bg",
                    &["bold"],
                )),
                text(l.desc.clone()).theme(ink()),
            ]),
        });
    }
    // The list can be taller than the box on a short frame, and the painter
    // clipped it; a viewport says there is more.
    fresh_ui::viewport(col().children(rows)).scrollbar().flex(1)
}

fn choice_box(c: &Choice, target: impl Fn(usize) -> Target + 'static) -> Node<UiMsg> {
    let mut rows: Vec<Node<UiMsg>> = vec![
        line(
            format!(" {} ", c.title),
            attrs("ui.status_warning_fg", "ui.popup_bg", &["bold"]),
        ),
        line(c.prompt.clone(), ink()),
        blank(),
    ];
    // The changes, in a window: the painter clipped the list at the dialog's
    // height, which is capped at twenty however many there are.
    rows.push(
        fresh_ui::viewport(
            col().children(
                c.changes
                    .iter()
                    .map(|d| line(format!("  {d}"), ink()))
                    .collect::<Vec<_>>(),
            ),
        )
        .scrollbar()
        .flex(1),
    );
    rows.push(rule());
    rows.push(buttons(c, target));
    rows.push(line(
        c.help.clone(),
        pair("ui.line_number_fg", "ui.popup_bg"),
    ));
    col().flex(1).children(rows)
}

fn ring_of(k: &Destructive) -> String {
    match k.grave {
        true => pair("ui.diagnostic_error_fg", "ui.popup_bg"),
        false => pair("ui.status_warning_fg", "ui.popup_bg"),
    }
}

fn grave_box(k: &Destructive, target: impl Fn(usize) -> Target + 'static) -> Node<UiMsg> {
    let target = Rc::new(target);
    let mut kids: Vec<Node<UiMsg>> = vec![row().flex(1)];
    for (i, label) in k.buttons.iter().enumerate() {
        let theme = match (i == k.selected, i == k.destructive) {
            (true, true) => attrs("ui.diagnostic_error_fg", "ui.popup_selection_bg", &["bold"]),
            (true, false) => attrs("ui.popup_selection_fg", "ui.popup_selection_bg", &["bold"]),
            (false, true) => attrs("ui.diagnostic_error_fg", "ui.popup_bg", &["bold"]),
            (false, false) => ink(),
        };
        let marker = match i == k.selected {
            true => ">",
            false => " ",
        };
        let t = target.clone();
        kids.push(
            gesture(text(format!("{marker}[ {label} ]")).theme(theme))
                .key(button_key(i))
                .on(
                    GestureKind::Press,
                    Rc::new(move |e: &Event| {
                        if e.button != MouseButton::Left {
                            return None;
                        }
                        e.stop();
                        Some(UiMsg::Ui(UiFact::SettingsDialog(t(i))))
                    }),
                ),
        );
        kids.push(text("  ").theme(ink()));
    }
    kids.push(row().flex(1));
    col().flex(1).children([
        line(
            format!(" {} ", k.title),
            attrs(
                match k.grave {
                    true => "ui.diagnostic_error_fg",
                    false => "ui.status_warning_fg",
                },
                "ui.popup_bg",
                &["bold"],
            ),
        ),
        line(k.message.clone(), ink()),
        row().flex(1),
        row().h(Sizing::Cells(1)).children(kids),
        line(k.help.clone(), pair("ui.line_number_fg", "ui.popup_bg")),
    ])
}

fn blank() -> Node<UiMsg> {
    row().h(Sizing::Cells(1))
}

/// The centred `[ label ]` row. The painter summed the labels' widths and
/// divided; a row of naturally-sized children between two flexible gaps is
/// the same centring, and each button is where its own press lands.
fn buttons(c: &Choice, target: impl Fn(usize) -> Target + 'static) -> Node<UiMsg> {
    let target = Rc::new(target);
    let mut kids: Vec<Node<UiMsg>> = vec![row().flex(1)];
    for (i, label) in c.buttons.iter().enumerate() {
        let theme = match (i == c.selected, c.hovered == Some(i)) {
            (true, _) => attrs("ui.menu_highlight_fg", "ui.menu_highlight_bg", &["bold"]),
            (false, true) => pair("ui.menu_hover_fg", "ui.menu_hover_bg"),
            (false, false) => ink(),
        };
        let marker = match i == c.selected {
            true => ">",
            false => " ",
        };
        let t = target.clone();
        kids.push(
            gesture(text(format!("{marker}[ {label} ]")).theme(theme))
                .key(button_key(i))
                .on(
                    GestureKind::Press,
                    Rc::new(move |e: &Event| {
                        if e.button != MouseButton::Left {
                            return None;
                        }
                        e.stop();
                        Some(UiMsg::Ui(UiFact::SettingsDialog(t(i))))
                    }),
                )
                .on_enter({
                    let t = target.clone();
                    Rc::new(move |_: &Event| {
                        Some(UiMsg::Ui(UiFact::SettingsDialogHover(Some(t(i)))))
                    })
                })
                .on_leave(Rc::new(move |_: &Event| {
                    Some(UiMsg::Ui(UiFact::SettingsDialogHover(None)))
                })),
        );
        kids.push(text("  ").theme(ink()));
    }
    kids.push(row().flex(1));
    row().h(Sizing::Cells(1)).children(kids)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::view::shell::frame::{frame_tree, Frame};
    use crate::view::shell::modal::Slot;
    use crate::view::shell::msg::UiFact;
    use fresh_ui::{Size, Ui};

    fn chrome() -> Chrome {
        let dim = pair("ui.line_number_fg", "ui.popup_bg");
        Chrome {
            title: " Settings [user] ".into(),
            search: Search::Hint(vec![Span::new("Press / to search settings...", dim)]),
            footer: Some(footer()),
            categories: Some(tree()),
            strip: None,
            results: None,
            items: None,
            page: Some(Page {
                title: "General".into(),
                clear: Some("[Clear]".into()),
                clear_hovered: false,
            }),
        }
    }

    /// A category tree with one expandable page, its two sections, and a
    /// second page under them.
    fn tree() -> Categories {
        Categories {
            rows: vec![
                CatRow::Category {
                    idx: 0,
                    chevron: "▼",
                    expandable: true,
                    dirty: true,
                    icon: "⚙",
                    label: "General".into(),
                    elide: false,
                },
                CatRow::Section {
                    cat: 0,
                    section: 0,
                    label: "Startup".into(),
                },
                CatRow::Section {
                    cat: 0,
                    section: 1,
                    label: "Appearance".into(),
                },
                CatRow::Category {
                    idx: 1,
                    chevron: " ",
                    expandable: false,
                    dirty: false,
                    icon: "✂",
                    label: "Clipboard".into(),
                    elide: false,
                },
            ],
            selected: Some(0),
            focused: true,
        }
    }

    fn searching(query: &str) -> Chrome {
        Chrome {
            title: " Settings [user] ".into(),
            search: Search::Active {
                field: std::rc::Rc::new(fresh_core::api::WidgetSpec::Text {
                    value: query.into(),
                    cursor_byte: query.len() as i32,
                    focused: true,
                    label: String::new(),
                    placeholder: None,
                    rows: 1,
                    field_width: 0,
                    max_visible_chars: 0,
                    full_width: false,
                    completions: Vec::new(),
                    completions_visible_rows: 0,
                    block_caret: true,
                    sel_start: -1,
                    sel_end: -1,
                    label_width: 0,
                    read_only: false,
                    markdown: false,
                    key: None,
                }),
                suffix: vec![Span::new(
                    " (3 results)",
                    pair("ui.line_number_fg", "ui.popup_bg"),
                )],
            },
            // A search replaces the body, so neither the tree nor the page
            // header is described.
            footer: Some(footer()),
            categories: None,
            strip: None,
            results: None,
            page: None,
            items: None,
        }
    }

    fn footer() -> Footer {
        Footer {
            layer: "[ user ]".into(),
            reset: "[ Reset ]".into(),
            save: "[ Save ]".into(),
            cancel: "[ Cancel ]".into(),
            edit: "[ Edit ]".into(),
            focused: None,
            hovered: None,
            help: "Enter:Edit  Esc:Close".into(),
        }
    }

    fn laid_out(w: u16, h: u16, dock: Option<u16>) -> Ui<UiMsg> {
        with_chrome(chrome(), w, h, dock)
    }

    fn with_chrome(c: Chrome, w: u16, h: u16, dock: Option<u16>) -> Ui<UiMsg> {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            frame_tree(Frame {
                settings: Some(c),
                modal: Some(Slot::Settings),
                dock,
                menu_bar: false,
                status_bar: false,
                ..Frame::default()
            }),
            Size::new(w, h),
        );
        ui
    }

    /// A page of `n` one-line toggles, with a handle on its window.
    fn paged(n: usize, anchor: &Rc<fresh_ui::behavior::Anchor>) -> Chrome {
        let mut c = chrome();
        c.items = Some(Items {
            anchor: Some(anchor.clone()),
            cards: (0..n)
                .map(|index| Card {
                    index,
                    section: None,
                    spec: fresh_core::api::WidgetSpec::Raw {
                        entries: vec![fresh_core::text_property::TextPropertyEntry::text(format!(
                            "setting {index}"
                        ))],
                        key: None,
                    },
                    focus_key: String::new(),
                    description: None,
                    layer: None,
                    selected: index == 0,
                    hovered: false,
                    dirty: false,
                    inherit: None,
                    bordered: true,
                })
                .collect(),
        });
        c
    }

    /// The rows the body's window is showing, in order.
    fn body_rows(ui: &Ui<UiMsg>) -> Vec<String> {
        let vp = ui.find_by_key(&items_key()).expect("the body window");
        let band = ui.rect_of(vp);
        let mut rows: Vec<(i32, String)> = ui
            .spec()
            .visible()
            .filter_map(|i| match &i.draw {
                fresh_ui::Draw::Lines(l) => {
                    let t = l.first()?.trim().to_string();
                    (!t.is_empty() && i.rect.y >= band.y && i.rect.y < band.y + band.h as i32)
                        .then_some((i.rect.y, t))
                }
                _ => None,
            })
            .collect();
        rows.sort_by_key(|(y, _)| *y);
        rows.into_iter().map(|(_, t)| t).collect()
    }

    /// **The window is moved to a card, not to a row.** The painter kept the
    /// offset itself and found the row to put it at by walking every item's
    /// `ScrollItem::height` — a second copy of the heights it drew the cards
    /// with. The cards are measured once now, and "show me this one" is a
    /// message to the window that measured them.
    #[test]
    fn the_body_window_moves_to_the_card_it_is_asked_for() {
        let anchor = fresh_ui::behavior::Anchor::new();
        let mut ui: Ui<UiMsg> = Ui::new();
        let frame = |c: Chrome| {
            frame_tree(Frame {
                settings: Some(c),
                modal: Some(Slot::Settings),
                dock: None,
                menu_bar: false,
                status_bar: false,
                ..Frame::default()
            })
        };
        ui.frame(frame(paged(40, &anchor)), Size::new(100, 40));
        let first = body_rows(&ui);
        assert!(
            first.iter().any(|r| r.contains("setting 0")),
            "the window starts at the top: {first:?}"
        );
        assert!(
            !first.iter().any(|r| r.contains("setting 30")),
            "and does not reach the thirtieth card: {first:?}"
        );

        anchor.reveal_key(card_key(30));
        ui.frame(frame(paged(40, &anchor)), Size::new(100, 40));
        let after = body_rows(&ui);
        assert!(
            after.iter().any(|r| r.contains("setting 30")),
            "the window moved to the card it was asked for: {after:?}"
        );
    }

    /// **The results' window follows the selection**, and what it ended up at
    /// is readable — which is the two halves of what `search_scroll_offset`
    /// used to be. The painter windowed the list by hand
    /// (`skip(scroll_offset)`, break at `height - 3`) and the state nudged the
    /// offset by `max_visible` whenever the selection left it; the count row
    /// then read that offset back to say "(4-6 of 12)".
    #[test]
    fn the_results_window_follows_the_selected_result() {
        let results = |selected: usize| {
            let mut c = chrome();
            c.results = Some(Results {
                selected,
                rows: (0..12)
                    .map(|i| ResultRow {
                        name: vec![Span::new(
                            format!("result {i}"),
                            pair("ui.popup_text_fg", "ui.popup_bg"),
                        )],
                        breadcrumb: format!("General > setting_{i}"),
                        desc: None,
                    })
                    .collect(),
            });
            c
        };
        let shown = |ui: &Ui<UiMsg>| -> Vec<String> {
            ui.spec()
                .visible()
                .filter_map(|i| match &i.draw {
                    fresh_ui::Draw::Lines(l) => {
                        let t = l.first()?.trim().to_string();
                        t.starts_with("result ").then_some(t)
                    }
                    _ => None,
                })
                .collect()
        };
        // The band is short enough that twelve results cannot all fit.
        let mut ui: Ui<UiMsg> = Ui::new();
        let frame = |c: Chrome| {
            frame_tree(Frame {
                settings: Some(c),
                modal: Some(Slot::Settings),
                dock: None,
                menu_bar: false,
                status_bar: false,
                ..Frame::default()
            })
        };
        ui.frame(frame(results(0)), Size::new(100, 24));
        let first = shown(&ui);
        assert!(
            first.iter().any(|r| r == "result 0"),
            "the window starts at the top: {first:?}"
        );
        assert!(
            !first.iter().any(|r| r == "result 11"),
            "and does not reach the last result: {first:?}"
        );

        ui.frame(frame(results(11)), Size::new(100, 24));
        let after = shown(&ui);
        assert!(
            after.iter().any(|r| r == "result 11"),
            "selecting the last result brought it into the window: {after:?}"
        );
        // And what the count row reports — the first result in the window,
        // and how many of them fit — is the window's own answer rather than a
        // pair of numbers kept beside it. Both are in *results*: an
        // index-scrolled window counts in the items it holds.
        let el = ui.find_by_key(&results_key()).expect("the results list");
        let (scroll, window) = ui.scroll(el);
        let offset = scroll.y.max(0) as usize;
        assert_eq!(
            window.h as usize,
            after.len(),
            "the window reports as many results as are on screen"
        );
        assert!(
            offset > 0,
            "the window moved, so the count row starts past the first result"
        );
        assert!(
            !after.iter().any(|r| *r == format!("result {}", offset - 1)),
            "and the result above the window is not on screen: {after:?}"
        );
    }

    /// An open dropdown's options float over the cards below it.
    ///
    /// The settings painter reserved inline rows for them and grew the item;
    /// the widget adapter surfaces the same list as the screen-level pop-over
    /// every other dropdown in the editor opens. One dropdown, everywhere.
    #[test]
    fn an_open_dropdown_shows_its_options() {
        let anchor = fresh_ui::behavior::Anchor::new();
        let mut c = paged(6, &anchor);
        if let Some(it) = c.items.as_mut() {
            it.cards[1].spec = fresh_core::api::WidgetSpec::Dropdown {
                options: vec!["lf".into(), "crlf".into()],
                selected_index: 0,
                label: "Default Line Ending".into(),
                focused: false,
                label_width: 24,
                open: true,
                scroll_offset: 0,
                key: Some("/editor/default_line_ending".into()),
            };
        }
        let ui = with_chrome(c, 120, 40, None);
        let painted: Vec<String> = ui
            .spec()
            .visible()
            .filter_map(|i| match &i.draw {
                fresh_ui::Draw::Lines(l) => l.first().map(|s| s.trim().to_string()),
                _ => None,
            })
            .collect();
        assert!(
            painted.iter().any(|r| r == "crlf"),
            "the option list is on screen: {painted:?}"
        );
    }

    fn boxed(ui: &Ui<UiMsg>) -> fresh_ui::Rect {
        ui.rect_of(ui.find_by_key(&key()).expect("the box"))
    }

    /// Ninety percent, capped at 160, centred.
    #[test]
    fn the_box_is_ninety_percent_capped_and_centred() {
        let ui = laid_out(200, 60, None);
        let r = boxed(&ui);
        assert_eq!(r.w, MAX_WIDTH, "capped however wide the frame is");
        assert_eq!(r.h, 54);
        assert_eq!(r.x, (200 - MAX_WIDTH as i32) / 2);
    }

    /// **Beside the dock.** The painter added `area.x` back by hand because
    /// centring on the frame put the modal's left edge under the dock, which
    /// over-drew its title bar and clipped its rounded corner.
    #[test]
    fn the_box_centres_beside_the_dock() {
        let ui = laid_out(200, 60, Some(40));
        let r = boxed(&ui);
        assert!(r.x >= 40, "clear of the dock, at {}", r.x);
        assert_eq!(r.x, 40 + (160 - (160 * 90 / 100)) / 2);
    }

    /// An area below the guard has no dialog in it — the painter writes that
    /// it is too small instead.
    #[test]
    fn an_area_below_the_guard_has_no_box() {
        let ui = laid_out(30, 8, None);
        assert_eq!(boxed(&ui).w, 0, "nothing to place");
    }

    /// **A rectangle, not a surface**: a press inside it reaches the slot that
    /// routes to `handle_settings_mouse`, which hit-tests the interior's own
    /// recorded rectangles.
    #[test]
    fn a_press_inside_the_box_reaches_the_modal_router() {
        let mut ui = laid_out(200, 60, None);
        let r = boxed(&ui);
        let got = ui.dispatch(fresh_ui::Input::press(
            fresh_ui::Point::new(r.x + 4, r.y + 4),
            fresh_ui::MouseButton::Left,
            fresh_ui::Mods::NONE,
        ));
        assert!(
            got.msgs
                .iter()
                .any(|m| matches!(m, UiMsg::Ui(UiFact::ModalPointer(Slot::Settings)))),
            "the slot behind it answers: {:?}",
            got.msgs
        );
    }

    fn choice(n: usize, selected: usize, hovered: Option<usize>) -> Choice {
        Choice {
            title: "Unsaved changes".into(),
            prompt: "Save before closing?".into(),
            changes: (0..n).map(|i| format!("editor.setting{i} → {i}")).collect(),
            buttons: vec!["Save".into(), "Discard".into(), "Abandon".into()],
            selected,
            hovered,
            help: "←/→/Tab: Select".into(),
        }
    }

    fn with_dialog(d: Dialog, w: u16, h: u16) -> Ui<UiMsg> {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            frame_tree(Frame {
                settings: Some(chrome()),
                settings_dialog: Some(d),
                modal: Some(Slot::Settings),
                menu_bar: false,
                status_bar: false,
                ..Frame::default()
            }),
            Size::new(w, h),
        );
        ui
    }

    fn facts(got: fresh_ui::Dispatch<UiMsg>) -> Vec<UiFact> {
        got.msgs
            .into_iter()
            .filter_map(|m| match m {
                UiMsg::Ui(f) => Some(f),
                _ => None,
            })
            .collect()
    }

    /// **The `[Clear …]` answers its own press**, where the painter filed a
    /// rectangle for it as it drew, and the band under the header is layout's
    /// answer rather than a height counted back from the rows just painted.
    #[test]
    fn the_page_header_sits_above_the_band_the_painter_fills() {
        let mut ui = laid_out(200, 60, None);
        let panel = ui.rect_of(ui.find_by_key(&panel_key()).expect("the panel"));
        let items = ui.rect_of(ui.find_by_key(&items_key()).expect("the item band"));
        assert_eq!(items.x, panel.x, "the band is the panel's width");
        assert_eq!(
            items.y,
            panel.y + 3,
            "title, clear button, blank — then the band"
        );
        assert!(items.h > 0, "and it has room to fill");

        let at = ui.rect_of(ui.find_by_key(&clear_key()).expect("the clear button"));
        let got = facts(ui.dispatch(fresh_ui::Input::press(
            fresh_ui::Point::new(at.x, at.y),
            fresh_ui::MouseButton::Left,
            fresh_ui::Mods::NONE,
        )));
        assert!(
            got.contains(&UiFact::SettingsClearCategory),
            "the button is its own target, got {got:?}"
        );
    }

    /// **Below sixty columns the five buttons stack**, which is the whole of
    /// what `render_footer_vertical` was: the same five, one per row, in the
    /// painter's own order, with the hints dropped. Two painters differing in
    /// a direction are one description and a width.
    ///
    /// The width is what decides it — nothing sets a flag here. A 60-column
    /// frame gives a 54-column box, whose 52-column interior is under
    /// [`NARROW_BELOW`].
    #[test]
    fn the_narrow_footer_stacks_its_five_buttons() {
        let ui = with_chrome(chrome(), 60, 40, None);
        let ys: Vec<i32> = [
            Button::Layer,
            Button::Save,
            Button::Reset,
            Button::Cancel,
            Button::Edit,
        ]
        .into_iter()
        .map(|b| {
            ui.rect_of(
                ui.find_by_key(&footer_key(b))
                    .unwrap_or_else(|| panic!("{b:?} is on screen")),
            )
            .y
        })
        .collect();
        let mut sorted = ys.clone();
        sorted.sort();
        sorted.dedup();
        assert_eq!(sorted, ys, "one per row, in the painter's order: {ys:?}");
        assert_eq!(ys[4] - ys[0], 4, "five consecutive rows, not a row of five");
    }

    /// The category tree sits down the left of the body band, twenty-four
    /// columns wide, with the painter's panel to the right of the divider —
    /// `Layout::horizontal([Length(24), Length(1), Min(40)])`, which the
    /// painter now *reads* rather than splits a second time.
    #[test]
    fn the_tree_and_the_panel_share_the_body_band() {
        let ui = laid_out(200, 60, None);
        let boxed = ui.rect_of(ui.find_by_key(&key()).expect("the box"));
        let tree = ui.rect_of(ui.find_by_key(&categories_key()).expect("the tree"));
        let panel = ui.rect_of(ui.find_by_key(&panel_key()).expect("the panel"));
        assert_eq!(tree.w, CATEGORY_COLS, "the tree's width is the painter's");
        assert_eq!(
            tree.x,
            boxed.x + 1,
            "inside the box's border, where the painter measures its content \
             band from"
        );
        assert_eq!(
            panel.x,
            tree.x + tree.w as i32 + 1,
            "the panel starts one divider column right of the tree"
        );
        assert!(panel.h > 0 && tree.h > 0, "both have a band to fill");
    }

    /// **A press on a row is that row's identity**, which is what the four
    /// families of rectangle the painter filed were reconstructing.
    #[test]
    fn a_press_on_a_row_names_it() {
        for (row, want) in [
            (0usize, UiFact::SettingsCategory(0)),
            (1, UiFact::SettingsCategorySection(0, 0)),
            (2, UiFact::SettingsCategorySection(0, 1)),
            (3, UiFact::SettingsCategory(1)),
        ] {
            let mut ui = laid_out(200, 60, None);
            let at = ui.rect_of(
                ui.find_by_key(&fresh_ui::Key::Pair("settings_cat".into(), row as u64))
                    .expect("a row"),
            );
            // A `List` selects on the click, not on the press.
            let p = fresh_ui::Point::new(at.x + 6, at.y);
            let _ = ui.dispatch(fresh_ui::Input::press(
                p,
                fresh_ui::MouseButton::Left,
                fresh_ui::Mods::NONE,
            ));
            let got = facts(ui.dispatch(fresh_ui::Input::release(
                p,
                fresh_ui::MouseButton::Left,
                fresh_ui::Mods::NONE,
            )));
            assert!(
                got.contains(&want),
                "row {row} should be {want:?}, got {got:?}"
            );
        }
    }

    /// **The chevron is its own target.** The painter recorded a one-column
    /// rectangle for it so a click there expanded the category instead of
    /// selecting it; here it is a node beside the label that stops the press.
    #[test]
    fn a_press_on_the_chevron_expands_rather_than_selects() {
        let mut ui = laid_out(200, 60, None);
        let at = ui.rect_of(
            ui.find_by_key(&fresh_ui::Key::Pair("settings_cat".into(), 0u64))
                .expect("the first row"),
        );
        // Column 0 is the cursor marker; the chevron is the one after it.
        let got = facts(ui.dispatch(fresh_ui::Input::press(
            fresh_ui::Point::new(at.x + 1, at.y),
            fresh_ui::MouseButton::Left,
            fresh_ui::Mods::NONE,
        )));
        assert!(
            got.contains(&UiFact::SettingsCategoryDisclosure(0)),
            "the chevron toggles, got {got:?}"
        );
    }

    /// **The cursor wears a `>` when the tree has the keyboard**, and only
    /// then: the painter drew the marker on `is_selected && focus_panel ==
    /// Categories` and the highlight on `is_selected` alone.
    #[test]
    fn the_cursor_wears_its_marker_only_while_the_tree_has_the_keyboard() {
        let marks = |focused: bool| {
            let mut c = chrome();
            if let Some(t) = c.categories.as_mut() {
                t.focused = focused;
            }
            let ui = with_chrome(c, 200, 60, None);
            ui.spec()
                .layers()
                .iter()
                .filter(|i| match &i.draw {
                    fresh_ui::Draw::Lines(l) => l.iter().any(|s| &**s == ">"),
                    _ => false,
                })
                .count()
        };
        assert_eq!(marks(true), 1, "one marked row while focused");
        assert_eq!(marks(false), 0, "and none while the body has the keyboard");
    }

    /// **Fifty wide, as tall as it needs within twenty, centred in the box** —
    /// the painter's own two lines, and the same `parent_area` it was handed.
    #[test]
    fn a_prompt_is_centred_in_the_box_at_its_documented_size() {
        let ui = with_dialog(Dialog::Confirm(choice(3, 0, None)), 200, 60);
        let d = ui.rect_of(ui.find_by_key(&dialog_key()).expect("the dialog"));
        let b = ui.rect_of(ui.find_by_key(&key()).expect("the box"));
        assert_eq!(d.w, 50);
        // Two borders, the title, the prompt, the blank under it, the rule,
        // the buttons and the help line — then one row per change. The
        // painter's figure was seven because its title rode *in* the top
        // border; this box gives it a row.
        assert_eq!(d.h, 11, "eight plus one per change");
        assert_eq!(d.x, b.x + (b.w as i32 - 50) / 2, "centred in the box");
    }

    /// A long change list does not make the dialog grow past twenty — the
    /// painter's `.min(20)` — and the list scrolls inside it instead of being
    /// drawn past the bottom edge.
    #[test]
    fn a_long_change_list_is_capped_and_scrolls() {
        let ui = with_dialog(Dialog::Confirm(choice(40, 0, None)), 200, 60);
        let d = ui.rect_of(ui.find_by_key(&dialog_key()).expect("the dialog"));
        assert_eq!(d.h, 20);
        assert!(
            ui.spec()
                .items
                .iter()
                .any(|i| matches!(i.draw, fresh_ui::Draw::Scrollbar { .. })),
            "forty changes in a dialog of twenty scroll"
        );
    }

    /// **Each button answers its own press.** The arm behind them re-derived
    /// the painter's layout — "must match `render_confirm_dialog`" — to work
    /// out which one a cell was on.
    #[test]
    fn each_button_answers_its_own_press() {
        for i in 0..3 {
            let mut ui = with_dialog(Dialog::Confirm(choice(2, 0, None)), 200, 60);
            let r = ui.rect_of(ui.find_by_key(&button_key(i)).expect("a button"));
            let got = facts(ui.dispatch(fresh_ui::Input::press(
                fresh_ui::Point::new(r.x + 1, r.y),
                fresh_ui::MouseButton::Left,
                fresh_ui::Mods::NONE,
            )));
            assert!(
                got.contains(&UiFact::SettingsDialog(Target::Confirm(i))),
                "button {i}: {got:?}"
            );
        }
    }

    /// The reset prompt's two say `Reset`, not `Confirm` — the same buttons,
    /// a different question.
    #[test]
    fn the_reset_prompts_buttons_are_its_own() {
        let mut c = choice(1, 0, None);
        c.buttons = vec!["Reset".into(), "Cancel".into()];
        let mut ui = with_dialog(Dialog::Reset(c), 200, 60);
        let r = ui.rect_of(ui.find_by_key(&button_key(0)).expect("reset"));
        let got = facts(ui.dispatch(fresh_ui::Input::press(
            fresh_ui::Point::new(r.x + 1, r.y),
            fresh_ui::MouseButton::Left,
            fresh_ui::Mods::NONE,
        )));
        assert!(
            got.contains(&UiFact::SettingsDialog(Target::Reset(0))),
            "{got:?}"
        );
    }

    /// The buttons are centred as a group, which is what summing their widths
    /// and dividing did — said as two flexible gaps.
    #[test]
    fn the_buttons_are_centred_as_a_group() {
        let ui = with_dialog(Dialog::Confirm(choice(2, 0, None)), 200, 60);
        let d = ui.rect_of(ui.find_by_key(&dialog_key()).expect("the dialog"));
        let first = ui.rect_of(ui.find_by_key(&button_key(0)).expect("first"));
        let last = ui.rect_of(ui.find_by_key(&button_key(2)).expect("last"));
        let left = first.x - d.x;
        let right = (d.x + d.w as i32) - (last.x + last.w as i32);
        assert!(
            (left - right).abs() <= 3,
            "left {left} right {right} in a dialog {} wide",
            d.w
        );
    }

    /// **Nothing behind it is interactive**, which is what dimming the modal
    /// and swallowing every event meant.
    #[test]
    fn a_press_on_the_backdrop_does_nothing() {
        let mut ui = with_dialog(Dialog::Confirm(choice(2, 0, None)), 200, 60);
        let got = facts(ui.dispatch(fresh_ui::Input::press(
            fresh_ui::Point::new(1, 1),
            fresh_ui::MouseButton::Left,
            fresh_ui::Mods::NONE,
        )));
        assert!(
            !got.iter().any(|f| matches!(f, UiFact::SettingsDialog(_))),
            "{got:?}"
        );
    }

    /// The help overlay lists its shortcuts, and scrolls when the box is too
    /// short for all fifteen lines.
    #[test]
    fn the_help_overlay_lists_its_shortcuts() {
        let help = || Dialog::Help {
            title: "Keyboard Shortcuts".into(),
            lines: (0..15)
                .map(|i| HelpLine {
                    key: format!("k{i}"),
                    desc: format!("does {i}"),
                    heading: i % 5 == 0,
                })
                .collect(),
        };
        let ui = with_dialog(help(), 200, 60);
        let painted: Vec<String> = ui
            .spec()
            .layers()
            .iter()
            .filter_map(|i| match &i.draw {
                fresh_ui::Draw::Lines(l) => {
                    Some(l.iter().map(|s| s.to_string()).collect::<Vec<_>>())
                }
                _ => None,
            })
            .flatten()
            .collect();
        assert!(painted.iter().any(|r| r.contains("does 1")), "{painted:?}");
    }

    fn grave(selected: usize) -> Destructive {
        Destructive {
            title: "Delete \"rust\"?".into(),
            message: "This will permanently remove \"rust\".".into(),
            buttons: vec!["Cancel".into(), "Delete".into()],
            selected,
            destructive: 1,
            help: "Tab/←→: Select".into(),
            grave: true,
            width: 60,
        }
    }

    /// **The two entry prompts answer a press**, which they never did: the
    /// painter drew them and only the keyboard replied, in a dialog where
    /// every other button is clickable.
    #[test]
    fn the_entry_prompts_buttons_answer_a_press() {
        for (i, want) in [(0, Target::EntryDelete(0)), (1, Target::EntryDelete(1))] {
            let mut ui = with_dialog(Dialog::EntryDelete(grave(0)), 200, 60);
            let r = ui.rect_of(ui.find_by_key(&button_key(i)).expect("a button"));
            let got = facts(ui.dispatch(fresh_ui::Input::press(
                fresh_ui::Point::new(r.x + 1, r.y),
                fresh_ui::MouseButton::Left,
                fresh_ui::Mods::NONE,
            )));
            assert!(got.contains(&UiFact::SettingsDialog(want)), "{i}: {got:?}");
        }
    }

    /// The delete prompt is sixty wide and the discard prompt fifty — the
    /// painter's two numbers, and the only thing that differed between them
    /// besides the words.
    #[test]
    fn the_two_entry_prompts_keep_their_widths() {
        let ui = with_dialog(Dialog::EntryDelete(grave(0)), 200, 60);
        assert_eq!(ui.rect_of(ui.find_by_key(&dialog_key()).unwrap()).w, 60);
        let mut d = grave(0);
        d.width = 50;
        d.grave = false;
        let ui = with_dialog(Dialog::EntryDiscard(d), 200, 60);
        assert_eq!(ui.rect_of(ui.find_by_key(&dialog_key()).unwrap()).w, 50);
    }

    /// **The search row is where the painter put it**: one row of border, then
    /// the row, then a blank gap before the body.
    #[test]
    fn the_search_row_is_the_first_row_inside_the_border() {
        let ui = laid_out(200, 60, None);
        let b = ui.rect_of(ui.find_by_key(&key()).expect("the box"));
        let s = ui.rect_of(ui.find_by_key(&search_key()).expect("the search row"));
        assert_eq!(s.y, b.y + 1, "inside the border");
        assert_eq!(s.h, 1);
    }

    /// **The live query is a node, through the adapter a plugin's field goes
    /// through.** It was already a `WidgetSpec::Text` rendered by
    /// `render_spec`; what changed is that the tree lays it out.
    #[test]
    fn an_active_search_paints_its_query_and_its_count() {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            frame_tree(Frame {
                settings: Some(searching("theme")),
                modal: Some(Slot::Settings),
                menu_bar: false,
                status_bar: false,
                ..Frame::default()
            }),
            Size::new(200, 60),
        );
        let painted: Vec<String> = ui
            .spec()
            .layers()
            .iter()
            .filter_map(|i| match &i.draw {
                fresh_ui::Draw::Lines(l) => {
                    Some(l.iter().map(|s| s.to_string()).collect::<Vec<_>>())
                }
                _ => None,
            })
            .flatten()
            .collect();
        assert!(painted.iter().any(|r| r.contains("theme")), "{painted:?}");
        assert!(
            painted.iter().any(|r| r.contains("(3 results)")),
            "the count rides after the field: {painted:?}"
        );
    }

    /// **The body band is invisible to the pointer, not a claim.** The panels
    /// under it are still hit-tested against rectangles the painter recorded,
    /// so a press there has to reach the slot that routes to them — taking it
    /// here would make the whole settings body inert.
    #[test]
    fn a_press_on_the_body_reaches_the_modal_router() {
        let mut ui = laid_out(200, 60, None);
        let b = ui.rect_of(ui.find_by_key(&key()).expect("the box"));
        let got = facts(ui.dispatch(fresh_ui::Input::press(
            fresh_ui::Point::new(b.x + 10, b.y + 10),
            fresh_ui::MouseButton::Left,
            fresh_ui::Mods::NONE,
        )));
        assert!(
            got.iter()
                .any(|f| matches!(f, UiFact::ModalPointer(Slot::Settings))),
            "the slot answers: {got:?}"
        );
    }

    /// **The footer's five buttons sit where the painter put them**: `[ Edit ]`
    /// flush left, the rest flush right, on the second-to-last row.
    #[test]
    fn the_footer_buttons_are_left_and_right_aligned() {
        let ui = laid_out(200, 60, None);
        let b = ui.rect_of(ui.find_by_key(&key()).expect("the box"));
        let at = |x: Button| ui.rect_of(ui.find_by_key(&footer_key(x)).expect("a button"));
        let edit = at(Button::Edit);
        let cancel = at(Button::Cancel);
        assert_eq!(edit.y, b.y + b.h as i32 - 2, "the row above the border");
        assert_eq!(cancel.y, edit.y, "all on one row");
        assert_eq!(edit.x, b.x + 1, "flush left inside the border");
        assert_eq!(
            cancel.x + cancel.w as i32,
            b.x + b.w as i32 - 1,
            "and Cancel flush right"
        );
        assert!(at(Button::Save).x < cancel.x, "Save before Cancel");
        assert!(at(Button::Reset).x < at(Button::Save).x);
        assert!(at(Button::Layer).x < at(Button::Reset).x);
    }

    /// **Which buttons fit is a rule, and it drops them in order.** The
    /// painter summed the widths and hid Edit, then Layer, then Reset as the
    /// room ran out; Save and Cancel are the two that always stay.
    ///
    /// The room runs out because the *labels* are long, not because the box
    /// is small: a box narrow enough to squeeze five English labels is
    /// already under [`NARROW_BELOW`], where the footer stacks and drops
    /// nothing. What this rule is actually for is a locale whose five labels
    /// do not fit a box that is otherwise comfortably wide, so that is what
    /// it is measured against.
    #[test]
    fn a_narrow_footer_drops_its_buttons_in_order() {
        // 66 wide leaves 64 inside the border, which is enough for everything.
        let wide = laid_out(74, 60, None);
        assert!(wide.find_by_key(&footer_key(Button::Edit)).is_some());
        // The same box, with labels a translator might land on.
        let mut c = chrome();
        if let Some(f) = c.footer.as_mut() {
            f.edit = "[ Bearbeiten ]".into();
            f.layer = "[ Benutzerebene ]".into();
            f.reset = "[ Zurücksetzen ]".into();
            f.save = "[ Speichern ]".into();
            f.cancel = "[ Abbrechen ]".into();
        }
        let tight = with_chrome(c, 74, 60, None);
        assert!(
            tight.find_by_key(&footer_key(Button::Save)).is_some()
                && tight.find_by_key(&footer_key(Button::Cancel)).is_some(),
            "these two always stay"
        );
        assert!(
            tight.find_by_key(&footer_key(Button::Edit)).is_none(),
            "Edit is the first to go"
        );
        assert!(
            tight.find_by_key(&footer_key(Button::Layer)).is_none(),
            "then Layer"
        );
    }

    /// Each answers its own press, and reports its own hover — five rectangles
    /// the painter filed for `SettingsLayout::hit_test`.
    #[test]
    fn each_footer_button_answers_its_own_press() {
        for b in [
            Button::Edit,
            Button::Layer,
            Button::Reset,
            Button::Save,
            Button::Cancel,
        ] {
            let mut ui = laid_out(200, 60, None);
            let r = ui.rect_of(ui.find_by_key(&footer_key(b)).expect("a button"));
            let got = facts(ui.dispatch(fresh_ui::Input::press(
                fresh_ui::Point::new(r.x + 1, r.y),
                fresh_ui::MouseButton::Left,
                fresh_ui::Mods::NONE,
            )));
            assert!(got.contains(&UiFact::SettingsButton(b)), "{b:?}: {got:?}");
        }
    }

    /// The key hints ride between `[ Edit ]` and the right-hand group, with
    /// the key reverse-videoed — `Key:Action`, split on the double space.
    #[test]
    fn the_footer_shows_its_key_hints() {
        let ui = laid_out(200, 60, None);
        let painted: Vec<String> = ui
            .spec()
            .layers()
            .iter()
            .filter_map(|i| match &i.draw {
                fresh_ui::Draw::Lines(l) => {
                    Some(l.iter().map(|s| s.to_string()).collect::<Vec<_>>())
                }
                _ => None,
            })
            .flatten()
            .collect();
        assert!(painted.iter().any(|r| r.contains("Enter")), "{painted:?}");
        assert!(painted.iter().any(|r| r.contains("Edit")), "{painted:?}");
    }
}
