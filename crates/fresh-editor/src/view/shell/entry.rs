//! The settings entry-edit dialog stack, as descriptions.
//!
//! The last of B.1. A settings map or object array opens one of these to edit
//! one entry, and an entry can open another — so it is a *stack*, dimmed
//! between levels, with only the top one live.
//!
//! **What it replaces is three copies of one layout.** The painter drew the
//! button row and filed nothing; `entry_dialog_update_hover` re-derived the
//! same row to find which button the pointer was on; and
//! `handle_entry_dialog_click` re-derived it a third time for the press. The
//! item positions were walked twice more — once by the renderer with its
//! section headers and its separator, once by the hover walk *without* them,
//! which is why hovering a dialog with sections had been two rows out per
//! section since the sections were added.
//!
//! And the sharpest one: a press inside a `TextList` row had to decide whether
//! it landed on the trailing `[x]`, and the code that decided carried its own
//! apology — "computing the exact column would need the actual_field_width,
//! which we don't carry here" — followed by a guess and a fallback guess. The
//! row is built by [`crate::view::settings::widget_map`] and the button's
//! columns come from the same constants that build it.

use std::rc::Rc;

use fresh_ui::{
    col, gesture, layout_reader, row, stack, text, viewport, Align, Anchor, Event, GestureKind,
    LayoutInfo, Modality, MouseButton, Node, Place, PointerMode, Scrim, Sizing,
};

use crate::app::shell_host::shell_theme::{attrs, pair, Ink};

use super::msg::{UiFact, UiMsg};

/// One level of the stack.
#[derive(Clone)]
pub struct Dialog {
    /// Which level this is. Its key, and what a fact names.
    pub level: usize,
    /// Already resolved, `• modified` and all.
    pub title: String,
    /// A dirty form rings in the warning colour.
    pub dirty: bool,
    pub items: Vec<Item>,
    pub buttons: Vec<Button>,
    /// One line of contextual help above the buttons: the focused field's
    /// description, or what Enter does on a list's pending row.
    pub helper: Option<String>,
    pub legend: Legend,
    /// The window's handle, so the keyboard can move it to a field.
    pub anchor: Option<Rc<fresh_ui::behavior::Anchor>>,
}

impl std::fmt::Debug for Dialog {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Dialog")
            .field("level", &self.level)
            .field("title", &self.title)
            .field("items", &self.items.len())
            .finish_non_exhaustive()
    }
}

/// One field of the form.
#[derive(Clone, Debug)]
pub struct Item {
    pub index: usize,
    /// The rule the painter drew once, between the read-only fields and the
    /// editable ones.
    pub divider_above: bool,
    /// `── Section ──`, when this field starts one.
    pub section: Option<String>,
    /// The control, exactly as the widget mapping produced it.
    pub spec: fresh_core::api::WidgetSpec,
    /// The widget key the runtime treats as focused, or empty.
    pub focus_key: String,
    pub focused: bool,
    pub hovered: bool,
    pub modified: bool,
    pub read_only: bool,
    /// Which row of the control the cursor sits on. Zero for a scalar; a
    /// composite's cursor walks its rows, and the `>` follows it.
    pub cursor_row: u16,
    /// `(Inherited)`, or the `[Reset]` / `[Inherit]` this field offers.
    pub affordance: Option<Affordance>,
}

/// What a field offers at the right of its first row.
#[derive(Clone, Debug, PartialEq)]
pub enum Affordance {
    /// A label on a value that already inherits.
    Badge(String),
    /// The actions that lead somewhere different from where the value is.
    Actions(Vec<Action>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Action {
    pub label: String,
    pub focused: bool,
}

/// One of the dialog's own buttons.
#[derive(Clone, Debug, PartialEq)]
pub struct Button {
    pub label: String,
    pub focused: bool,
    pub hovered: bool,
    /// Delete keeps its colour while selected, and stands apart from the
    /// others so it cannot be reached by one Tab too many.
    pub destructive: bool,
}

/// The row under the buttons.
#[derive(Clone, Debug, PartialEq)]
pub enum Legend {
    /// The key hints for whatever the form is doing.
    Keys(String),
    /// A field that will not parse.
    Warn(String),
}

pub fn key(level: usize) -> fresh_ui::Key {
    fresh_ui::Key::Pair("settings_entry".into(), level as u64)
}

pub fn items_key(level: usize) -> fresh_ui::Key {
    fresh_ui::Key::Pair("settings_entry_items".into(), level as u64)
}

/// One field's band, so the window can be moved to it by name.
pub fn item_key(i: usize) -> fresh_ui::Key {
    fresh_ui::Key::Pair("settings_entry_item".into(), i as u64)
}

pub fn button_key(i: usize) -> fresh_ui::Key {
    fresh_ui::Key::Pair("settings_entry_button".into(), i as u64)
}

/// The dialog as a layer over the settings box.
///
/// `apply_dimming(frame, modal_area)` between levels is the scrim, and it was
/// applied once per level in a loop; a layer per level says the same thing and
/// says which one is on top.
pub fn layer(d: &Dialog) -> Node<UiMsg> {
    let d = d.clone();
    fresh_ui::layer()
        .within(super::settings::key())
        .anchor(Anchor::Screen(Align::Center))
        .place(Place::Over)
        .modality(Modality::Exclusive)
        .scrim(Some(Scrim::Dim))
        // The stack's topmost level is where focus is, so each level carries
        // the claim; they all name the settings slot because the settings
        // dispatcher is what answers an entry dialog's keys.
        .child(super::modal::keys(
            super::modal::KeySlot::Settings,
            layout_reader(move |info: LayoutInfo| {
                // The painter's own figures, against the settings box it centres
                // in: eighty-five percent of the width between fifty and ninety,
                // ninety percent of the height with a floor of fifteen.
                let w = (info.constraints.max_w * 85 / 100).clamp(50, 90);
                let h = (info.constraints.max_h * 90 / 100).max(15);
                // One name for the ring and its caption: a dirty form rings
                // and titles in the warning colour together.
                let ring_fg = match d.dirty {
                    true => "ui.diagnostic_warning_fg",
                    false => "ui.popup_border_fg",
                };
                let ring = pair(ring_fg, "ui.popup_bg");
                let boxed = col().theme(ring).border().child(body(&d));
                fresh_ui::stack()
                    .w(Sizing::Cells(w))
                    .h(Sizing::Cells(h))
                    .key(key(d.level))
                    .children([
                        boxed,
                        super::modal::title_strip(
                            d.title.clone(),
                            attrs(ring_fg, "ui.popup_bg", &["bold"]),
                        ),
                    ])
            }),
        ))
}

/// Everything inside the border: the fields in their window, then the three
/// rows the painter reserved at the bottom.
///
/// **The legend has come inside.** The painter wrote it at `button_y + 1`,
/// which is the box's own bottom border row, so the row it sat on was drawn
/// twice and the border lost its bottom edge under the text. Here it is a row
/// of the column like the two above it, and the border closes.
fn body(d: &Dialog) -> Node<UiMsg> {
    let items = viewport(col().children(d.items.iter().map(item).collect::<Vec<_>>()))
        .key(items_key(d.level))
        .scrollbar()
        .scrollbar_theme(pair("ui.split_separator_fg", "ui.popup_bg"));
    let items = match &d.anchor {
        Some(a) => items.anchor_to(a.clone()),
        None => items,
    };
    let helper = match &d.helper {
        Some(t) => text(t.clone())
            .theme(attrs("ui.line_number_fg", "ui.popup_bg", &["italic"]))
            .elide(fresh_ui::Elide::Tail)
            .h(Sizing::Cells(1)),
        None => row().h(Sizing::Cells(1)),
    };
    let legend = match &d.legend {
        Legend::Keys(t) => text(t.clone()).theme(pair("ui.line_number_fg", "ui.popup_bg")),
        Legend::Warn(t) => text(t.clone()).theme(pair("ui.diagnostic_warning_fg", "ui.popup_bg")),
    };
    // The painter's `inner` starts two columns in from the box, which is one
    // past its border.
    col().pad(1, 0).children([
        items.h(Sizing::Flex(1)),
        helper,
        buttons(&d.buttons),
        legend.elide(fresh_ui::Elide::Tail).h(Sizing::Cells(1)),
    ])
}

/// One field: its divider and heading, then the indicator gutter beside the
/// control, with the field's own affordance over the control's first row.
fn item(it: &Item) -> Node<UiMsg> {
    let mut rows: Vec<Node<UiMsg>> = Vec::with_capacity(4);
    if it.divider_above {
        rows.push(rule());
    }
    if let Some(name) = &it.section {
        rows.push(
            text(format!("── {name} ──"))
                .theme(attrs("ui.line_number_fg", "ui.popup_bg", &["bold"]))
                .h(Sizing::Cells(1)),
        );
        rows.push(row().h(Sizing::Cells(1)));
    }
    let band = band_bg(it);
    let mut content = control(it, &band);
    if let Some(a) = &it.affordance {
        content = stack()
            .w(Sizing::Flex(1))
            .children([content, affordance(it.index, a, &band)]);
    }
    rows.push(row().children([gutter(it, &band), content]));

    let idx = it.index;
    let node = col().key(item_key(it.index)).children(rows);
    // A read-only field is a label: it neither highlights nor answers.
    if it.read_only {
        return node.pointer_mode(PointerMode::Ignore);
    }
    gesture(node)
        .on(
            GestureKind::Press,
            Rc::new(move |e: &Event| {
                if e.button != MouseButton::Left {
                    return None;
                }
                e.stop();
                Some(UiMsg::Ui(UiFact::SettingsEntryItem(idx)))
            }),
        )
        .on_enter(Rc::new(move |_: &Event| {
            Some(UiMsg::Ui(UiFact::SettingsEntryItemHover(Some(idx))))
        }))
        .on_leave(Rc::new(move |_: &Event| {
            Some(UiMsg::Ui(UiFact::SettingsEntryItemHover(None)))
        }))
}

/// The rule between the read-only fields and the editable ones.
fn rule() -> Node<UiMsg> {
    layout_reader(|info: LayoutInfo| {
        text("─".repeat(info.constraints.max_w.max(1) as usize))
            .theme(pair("ui.line_number_fg", "ui.popup_bg"))
    })
    .h(Sizing::Cells(1))
}

/// The background the field's control is painted on.
fn band_bg(it: &Item) -> String {
    match (it.focused, it.hovered) {
        (true, _) => "ui.settings_selected_bg".to_string(),
        (_, true) => "ui.menu_hover_bg".to_string(),
        _ => "ui.popup_bg".to_string(),
    }
}

/// The `>` cursor and the `●` modified dot, in the three columns the painter
/// reserved. The cursor follows a composite control's own row; the dot is
/// always on the field's first.
fn gutter(it: &Item, band: &str) -> Node<UiMsg> {
    let mark = attrs("ui.settings_selected_fg", band, &["bold"]);
    let dot = pair("ui.settings_selected_fg", band);
    let plain = pair("ui.popup_text_fg", band);
    let rows: Vec<Node<UiMsg>> = (0..=it.cursor_row)
        .map(|r| {
            let cursor = match it.focused && r == it.cursor_row {
                true => ">",
                false => " ",
            };
            let modified = match it.modified && r == 0 {
                true => "●",
                false => " ",
            };
            row().h(Sizing::Cells(1)).children([
                text(cursor).theme(mark.clone()),
                text(modified).theme(dot.clone()),
                text(" ").theme(plain.clone()),
            ])
        })
        .collect();
    col().w(Sizing::Cells(3)).children(rows)
}

/// The control, through the adapter a plugin's field goes through.
fn control(it: &Item, band: &str) -> Node<UiMsg> {
    let spec = it.spec.clone();
    let focus_key = it.focus_key.clone();
    let surface = Ink::keys("ui.popup_text_fg", band.to_string());
    layout_reader(move |info: LayoutInfo| {
        let cx = super::widgets::Ctx {
            slot: super::widgets::Slot::SettingsEntry,
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
    .w(Sizing::Flex(1))
}

/// `(Inherited)`, or the field's `[Reset]` / `[Inherit]`, flush right on the
/// control's first row — a one-row layer over a control that may be many.
fn affordance(item: usize, a: &Affordance, band: &str) -> Node<UiMsg> {
    let mut kids: Vec<Node<UiMsg>> =
        vec![row().w(Sizing::Flex(1)).pointer_mode(PointerMode::Ignore)];
    match a {
        Affordance::Badge(label) => {
            kids.push(text(label.clone()).theme(attrs("ui.line_number_fg", band, &["italic"])))
        }
        Affordance::Actions(actions) => {
            for (i, action) in actions.iter().enumerate() {
                let theme = match action.focused {
                    true => attrs("ui.menu_hover_fg", "ui.menu_hover_bg", &["bold"]),
                    false => pair("ui.line_number_fg", band),
                };
                kids.push(gesture(text(action.label.clone()).theme(theme)).on(
                    GestureKind::Press,
                    Rc::new(move |e: &Event| {
                        if e.button != MouseButton::Left {
                            return None;
                        }
                        e.stop();
                        Some(UiMsg::Ui(UiFact::SettingsEntryFieldAction(item, i)))
                    }),
                ));
                kids.push(row().w(Sizing::Cells(1)).pointer_mode(PointerMode::Ignore));
            }
        }
    }
    row().h(Sizing::Cells(1)).children(kids)
}

/// `[ Save ] [ Cancel ]` and, when the entry can be removed, a `[ Delete … ]`
/// set further apart so it is not one Tab away from Cancel.
///
/// **The cursor's `>` is part of the button now.** The painter drew it two
/// columns *left* of the button, outside the group it had centred, so the
/// group's width and the thing drawn in it disagreed by two columns whenever
/// the keyboard was on the buttons. Two cells in front of every label keeps
/// the row still while the cursor moves along it.
fn buttons(bs: &[Button]) -> Node<UiMsg> {
    let mut kids: Vec<Node<UiMsg>> = vec![row().w(Sizing::Flex(1))];
    for (i, b) in bs.iter().enumerate() {
        if i > 0 {
            let gap = match b.destructive {
                true => 6,
                false => 2,
            };
            kids.push(row().w(Sizing::Cells(gap)));
        }
        let marker = match b.focused {
            true => "> ",
            false => "  ",
        };
        // Selected Delete keeps its red as a "still destructive" cue while the
        // band signals the keyboard is on it.
        let theme = match (b.focused, b.hovered, b.destructive) {
            (true, _, true) => attrs("ui.diagnostic_error_fg", "ui.popup_selection_bg", &["bold"]),
            (true, _, false) => attrs("ui.popup_selection_fg", "ui.popup_selection_bg", &["bold"]),
            (_, true, true) => attrs("ui.diagnostic_error_fg", "ui.menu_hover_bg", &["bold"]),
            (_, true, false) => pair("ui.menu_hover_fg", "ui.menu_hover_bg"),
            (_, _, true) => attrs("ui.diagnostic_error_fg", "ui.popup_bg", &["bold"]),
            _ => pair("ui.editor_fg", "ui.popup_bg"),
        };
        let label = format!("{marker}{}", b.label);
        kids.push(
            gesture(text(label).theme(theme))
                .key(button_key(i))
                .on(
                    GestureKind::Press,
                    Rc::new(move |e: &Event| {
                        if e.button != MouseButton::Left {
                            return None;
                        }
                        e.stop();
                        Some(UiMsg::Ui(UiFact::SettingsEntryButton(i)))
                    }),
                )
                .on_enter(Rc::new(move |_: &Event| {
                    Some(UiMsg::Ui(UiFact::SettingsEntryButtonHover(Some(i))))
                }))
                .on_leave(Rc::new(move |_: &Event| {
                    Some(UiMsg::Ui(UiFact::SettingsEntryButtonHover(None)))
                })),
        );
    }
    kids.push(row().w(Sizing::Flex(1)));
    row().h(Sizing::Cells(1)).children(kids)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::view::shell::frame::{frame_tree, Frame};
    use crate::view::shell::modal::Slot;
    use fresh_ui::{Size, Ui};

    fn field(index: usize, name: &str) -> Item {
        Item {
            index,
            divider_above: false,
            section: None,
            spec: fresh_core::api::WidgetSpec::Raw {
                entries: vec![fresh_core::text_property::TextPropertyEntry::text(name)],
                key: None,
            },
            focus_key: String::new(),
            focused: index == 0,
            hovered: false,
            modified: false,
            read_only: false,
            cursor_row: 0,
            affordance: None,
        }
    }

    fn dialog(n: usize) -> Dialog {
        Dialog {
            level: 0,
            title: " Rust ".into(),
            dirty: false,
            items: (0..n).map(|i| field(i, &format!("field {i}"))).collect(),
            buttons: vec![
                Button {
                    label: "[ Save ]".into(),
                    focused: false,
                    hovered: false,
                    destructive: false,
                },
                Button {
                    label: "[ Cancel ]".into(),
                    focused: false,
                    hovered: false,
                    destructive: false,
                },
                Button {
                    label: "[ Delete rust ]".into(),
                    focused: false,
                    hovered: false,
                    destructive: true,
                },
            ],
            helper: Some("What this field is for".into()),
            legend: Legend::Keys("↑↓:Navigate  Esc:Cancel".into()),
            anchor: None,
        }
    }

    fn laid_out(d: Dialog, w: u16, h: u16) -> Ui<UiMsg> {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            frame_tree(Frame {
                settings: Some(crate::view::shell::settings::Chrome {
                    wide: true,
                    title: " Settings ".into(),
                    search: crate::view::shell::settings::Search::Hint(Vec::new()),
                    footer: None,
                    categories: None,
                    strip: None,
                    results: None,
                    page: None,
                    items: None,
                }),
                settings_entry: vec![d],
                modal: Some(Slot::Settings),
                menu_bar: false,
                status_bar: false,
                ..Frame::default()
            }),
            Size::new(w, h),
        );
        ui
    }

    /// Every visible row of the frame, top to bottom.
    fn rows(ui: &Ui<UiMsg>) -> Vec<(i32, String)> {
        let mut out: Vec<(i32, String)> = ui
            .spec()
            .visible()
            .filter_map(|i| match &i.draw {
                fresh_ui::Draw::Lines(l) => {
                    let t = l.first()?.trim().to_string();
                    (!t.is_empty()).then_some((i.rect.y, t))
                }
                _ => None,
            })
            .collect();
        out.sort_by_key(|(y, _)| *y);
        out
    }

    /// **The title is on the top border, not in a row of its own.**
    ///
    /// `Block::title` wrote it into the ring's top edge, and every test that
    /// waits for this stack — thirteen of them wait for the words "Edit
    /// Value" — reads the screen for it. Describing the box without it
    /// painted a nameless frame: the dialog was up, and nothing on screen
    /// said which.
    #[test]
    fn the_title_rides_the_top_border() {
        let ui = laid_out(dialog(3), 160, 50);
        let boxed = ui.rect_of(ui.find_by_key(&key(0)).expect("the dialog"));
        let (y, painted) = rows(&ui)
            .into_iter()
            .find(|(_, t)| t == "Rust")
            .expect("the title on screen");
        assert_eq!(y, boxed.y, "on the box's own top row: {painted}");
    }

    /// **The legend is inside the box.** The painter wrote it at
    /// `button_y + 1`, which is the box's own bottom border row — so the
    /// border lost its bottom edge under the text, every time.
    #[test]
    fn the_legend_sits_inside_the_border() {
        let ui = laid_out(dialog(3), 160, 50);
        let boxed = ui.rect_of(ui.find_by_key(&key(0)).expect("the dialog"));
        let painted = rows(&ui);
        let (legend_y, _) = painted
            .iter()
            .find(|(_, t)| t.contains("↑↓:Navigate"))
            .expect("the legend");
        assert!(
            *legend_y < boxed.y + boxed.h as i32 - 1,
            "the legend is above the bottom border, not on it"
        );
    }

    /// The three rows the painter reserved at the bottom, in its order:
    /// the field's description, the buttons, then the keys.
    #[test]
    fn the_bottom_three_rows_keep_their_order() {
        let ui = laid_out(dialog(3), 160, 50);
        let painted = rows(&ui);
        let y = |needle: &str| {
            painted
                .iter()
                .find(|(_, t)| t.contains(needle))
                .unwrap_or_else(|| panic!("{needle} on screen: {painted:?}"))
                .0
        };
        assert!(y("What this field is for") < y("[ Save ]"));
        assert!(y("[ Save ]") < y("↑↓:Navigate"));
    }

    /// **Each button answers its own press.** The painter laid the row out,
    /// and the hover handler and the click handler each laid it out again to
    /// find which button a cell was on — three statements of one row.
    #[test]
    fn each_button_answers_its_own_press() {
        for (i, label) in [
            (0usize, "[ Save ]"),
            (1, "[ Cancel ]"),
            (2, "[ Delete rust ]"),
        ] {
            let mut ui = laid_out(dialog(3), 160, 50);
            let at = ui.rect_of(ui.find_by_key(&button_key(i)).expect("a button"));
            // Past the two-cell cursor gutter in front of the label.
            let got = ui.dispatch(fresh_ui::Input::press(
                fresh_ui::Point::new(at.x + 3, at.y),
                fresh_ui::MouseButton::Left,
                fresh_ui::Mods::NONE,
            ));
            let facts: Vec<_> = got
                .msgs
                .into_iter()
                .filter_map(|m| match m {
                    UiMsg::Ui(f) => Some(f),
                    _ => None,
                })
                .collect();
            assert!(
                facts.contains(&UiFact::SettingsEntryButton(i)),
                "{label} names itself: {facts:?}"
            );
        }
    }

    /// A press anywhere on a field that its control does not answer for
    /// focuses that field — which is what the painter's walk of every item's
    /// height was for.
    #[test]
    fn a_press_on_a_field_names_it() {
        let mut ui = laid_out(dialog(4), 160, 50);
        let at = ui.rect_of(ui.find_by_key(&item_key(2)).expect("a field"));
        let got = ui.dispatch(fresh_ui::Input::press(
            fresh_ui::Point::new(at.x + 1, at.y),
            fresh_ui::MouseButton::Left,
            fresh_ui::Mods::NONE,
        ));
        let facts: Vec<_> = got
            .msgs
            .into_iter()
            .filter_map(|m| match m {
                UiMsg::Ui(f) => Some(f),
                _ => None,
            })
            .collect();
        assert!(
            facts.contains(&UiFact::SettingsEntryItem(2)),
            "the third field names itself: {facts:?}"
        );
    }

    /// An open dropdown in a field shows its options.
    ///
    /// The regression this replaces (#2765) was the mirror image: the painter
    /// reserved inline rows and the widget framework discarded them, so the
    /// dropdown opened to an empty box. There is one dropdown now, and it
    /// floats.
    #[test]
    fn an_open_dropdown_shows_its_options() {
        let mut d = dialog(3);
        d.items[1].spec = fresh_core::api::WidgetSpec::Dropdown {
            options: vec!["dark".into(), "light".into(), "my-cool-theme".into()],
            selected_index: 0,
            label: "Theme".into(),
            focused: false,
            label_width: 10,
            open: true,
            scroll_offset: 0,
            key: Some("/ui/theme".into()),
        };
        let ui = laid_out(d, 160, 50);
        let painted: Vec<String> = rows(&ui).into_iter().map(|(_, t)| t).collect();
        for option in ["dark", "light", "my-cool-theme"] {
            assert!(
                painted.iter().any(|r| r == option),
                "{option} is on screen: {painted:?}"
            );
        }
    }
}
