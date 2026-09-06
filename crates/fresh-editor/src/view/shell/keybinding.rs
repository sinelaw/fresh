//! The keybinding editor's box.
//!
//! **The frame first, the interior after** — the same order the floating
//! plugin panel took (C.6). The editor is a table with its own scrollbar, its
//! own double-click semantics and ten rectangles its painter records for a
//! mouse handler to compare against; what moves here is the outermost of them,
//! which is the one both the painter and the handler used.
//!
//! `keybinding_modal_area` was four lines of arithmetic — ninety percent of
//! the area it was handed, capped at 120 columns, floored at 60 by 20, then
//! centred with `area.x`/`area.y` added back so it lands beside the dock
//! rather than under it. The floor and the cap are the *rule* and they stay;
//! the centring and the offsets are what a layer does, and naming the region
//! it may occupy is what "beside the dock" means.
//!
//! The cap has no property to be: `min_w` exists and `max_w` does not, so the
//! width is resolved from the extent the way §4.4 sanctions — a
//! `layout_reader`, which is content resolved from a *known* extent rather
//! than geometry recorded from a paint. `view::shell::calibration` does the
//! same thing for the same reason.

use std::rc::Rc;

use fresh_ui::{
    col, gesture, layout_reader, row, text, Align, Anchor, Event, GestureKind, LayoutInfo,
    Modality, MouseButton, Node, Place, Scrim, Sizing,
};

use crate::app::shell_host::shell_theme::{attrs, pair};

use super::msg::UiFact;

use super::msg::UiMsg;

/// Never wider than this, however wide the area is.
pub const MAX_WIDTH: u16 = 120;
/// And never smaller than this, however small it is.
pub const MIN_WIDTH: u16 = 60;
pub const MIN_HEIGHT: u16 = 20;

/// What a press on one of the editor's dialogs asks for.
///
/// Each was a rectangle the painter filed in a `KeybindingEditorLayout` and the
/// mouse arm compared a cell against, in a chain of `point_in_rect`. They are
/// where the nodes are now, and the fact says what was pressed rather than
/// where.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Target {
    /// The edit dialog's key field: focus it and start recording.
    KeyField,
    /// Its action field: focus it and start editing.
    ActionField,
    /// Its context field: focus it and start editing.
    ContextField,
    /// Its `[ Save ]`, which applies the edit.
    Save,
    /// Its `[ Cancel ]`, which closes it.
    Cancel,
    /// The unsaved-changes confirmation's three.
    ConfirmSave,
    ConfirmDiscard,
    ConfirmCancel,
}

/// One `key  description` row of the help overlay. An empty `desc` is a
/// section heading, which is how the painter's `is_header` flag read.
#[derive(Clone, Debug, PartialEq)]
pub struct HelpLine {
    pub key: String,
    pub desc: String,
    pub heading: bool,
}

/// The help overlay: a static list of bindings, and nothing else.
#[derive(Clone, Debug, PartialEq)]
pub struct Help {
    pub title: String,
    pub lines: Vec<HelpLine>,
}

/// One field of the edit dialog.
#[derive(Clone, Debug, PartialEq)]
pub struct Field {
    pub label: String,
    pub value: String,
    /// A hint shown beside the value while the field has focus.
    pub hint: Option<String>,
    pub focused: bool,
    /// The value reads as an error — the action name did not resolve.
    pub invalid: bool,
    /// A caret is drawn after the value, for a field being typed into.
    pub caret: bool,
    pub target: Target,
}

/// The edit / add binding dialog.
#[derive(Clone, Debug, PartialEq)]
pub struct Edit {
    pub title: String,
    pub instructions: String,
    pub key_field: Field,
    pub action_field: Field,
    /// The resolved action's human-readable form, when it differs from what
    /// was typed.
    pub action_description: Option<String>,
    pub context_field: Field,
    /// The action-name error, shown above the conflicts.
    pub error: Option<String>,
    pub conflicts_label: String,
    pub conflicts: Vec<String>,
    pub save_label: String,
    pub cancel_label: String,
    /// Which button is focused, when the buttons are.
    pub focused_button: Option<usize>,
    /// The action field's autocomplete, when it is open.
    pub autocomplete: Option<Autocomplete>,
}

/// The action field's suggestion list.
#[derive(Clone, Debug, PartialEq)]
pub struct Autocomplete {
    pub suggestions: Vec<String>,
    pub selected: Option<usize>,
}

/// How many suggestions the popup shows at once.
pub const AUTOCOMPLETE_VISIBLE: usize = 8;

/// The unsaved-changes confirmation.
#[derive(Clone, Debug, PartialEq)]
pub struct Confirm {
    pub title: String,
    pub message: String,
    /// Save, discard, cancel — with the selected one marked.
    pub buttons: Vec<String>,
    pub selected: usize,
}

/// The editor's dialogs, when any is open. At most one is: the painter
/// returned early on the help overlay and the input handler gates the other
/// two the same way.
#[derive(Clone, Debug, PartialEq)]
pub enum Dialog {
    Help(Help),
    Edit(Edit),
    Confirm(Confirm),
}

pub fn key() -> fresh_ui::Key {
    fresh_ui::Key::Str("keybinding_modal".into())
}

/// The box's size in an area of `info`'s extent.
///
/// `keybinding_modal_area`'s own two lines, kept because they are the rule
/// rather than the placement: ninety percent, capped, floored, and never
/// wider than the area less the two columns it keeps clear.
pub fn fit(info: LayoutInfo) -> (u16, u16) {
    let (w, h) = (info.constraints.max_w, info.constraints.max_h);
    let width = ((w as f32 * 0.90) as u16)
        .min(MAX_WIDTH)
        .max(MIN_WIDTH)
        .min(w.saturating_sub(2));
    let height = ((h as f32 * 0.90) as u16)
        .max(MIN_HEIGHT)
        .min(h.saturating_sub(2));
    (width, height)
}

/// The editor's box as a layer, with its table inside it when it has one.
///
/// **Invisible to the pointer where it is empty.**
///
/// It paints nothing — the interior still does, and a layer is in the overlay
/// band, so anything drawn here would land on top of the painter that owns the
/// surface. What it contributes is the rectangle, which the painter reads back
/// instead of computing.
///
/// **And only the rectangle.** `hit_paths` returns the first layer with any
/// path at the point, so a box that merely *existed* over the modal slot would
/// take every press inside itself and the slot behind it — the one that routes
/// to `handle_keybinding_editor_mouse` — would never be asked. That is not a
/// missing handler, it is a surface that swallows every click in the editor it
/// is standing in for. `PointerMode::Ignore` takes the subtree out of
/// hit-testing entirely, which is what "a rectangle, not a surface" means.
///
/// The exclusivity is the slot's, not this layer's, for the same reason: two
/// claims to the same modality is one too many, and the slot is the one that
/// carries the routing.
pub fn layer(c: &Chrome, t: Option<&Table>) -> Node<UiMsg> {
    let c = c.clone();
    let t = t.cloned();
    fresh_ui::layer()
        .within(super::frame::chrome_key())
        .anchor(Anchor::Screen(Align::Center))
        .place(Place::Over)
        // What the capture band was: nothing outside the modal is
        // interactive, so a press on the editor behind it goes nowhere.
        .modality(Modality::Exclusive)
        // The keyboard is this modal's too, by the same containment: a key
        // nothing in the box answered is the editor's own dispatcher's.
        .child(super::modal::keys(
            super::modal::KeySlot::KeybindingEditor,
            layout_reader(move |info: LayoutInfo| {
                let (w, h) = fit(info);
                // `Layout::vertical([Length(3), Min(5), Length(1)])` inside a
                // bordered block — the painter's own three bands, and the border
                // it drew around them. A **column**: it was a `row()` while the box
                // was empty and it did not matter, and it would have laid the
                // header band beside the table rather than above it.
                let body = match &t {
                    Some(t) => col().flex(1).children([table(t)]),
                    // A dialog covers the table, so there is nothing to build: the
                    // band is a claim surface, keeping a press off whatever is
                    // behind the modal.
                    None => claim().flex(1),
                };
                let inner = col().theme(ring()).border().children([
                    col().h(Sizing::Cells(3)).children([
                        row().h(Sizing::Cells(1)).children(spans(&c.path)),
                        search_row(&c.search),
                        row().h(Sizing::Cells(1)).children(spans(&c.filters)),
                    ]),
                    body,
                    row().h(Sizing::Cells(1)).children(spans(&c.footer)),
                ]);
                // **The box consumes what its parts do not.** A modal absorbs every
                // press inside it — the capture band's whole job — and the parts
                // that mean something stop the flow before it reaches here. The
                // size and the key go on the outside, or the box would be a
                // full-bounds wrapper with a small child parked in its corner.
                // The title rides the top border, as `Block::title` drew it
                // — not a row of its own, which would take a row from the
                // table and put its scrollbar track one row below where every
                // caller computes it.
                let titled = fresh_ui::stack().children([
                    inner,
                    super::modal::title_strip(
                        format!(" {} ", c.title),
                        attrs("ui.popup_border_fg", "ui.popup_bg", &["bold"]),
                    ),
                ]);
                swallow(titled)
                    .w(Sizing::Cells(w))
                    .h(Sizing::Cells(h))
                    .key(key())
            }),
        ))
}

/// The dialogs as layers over the editor's box.
///
/// **They are layers because of paint order.** The editor's interior is still
/// the painter's, and the tree's overlay band is folded after every legacy
/// painter — so a described dialog lands on top of the table the painter drew,
/// which is exactly where it was drawn before. The other direction would not
/// work: describing the *table* first would have covered the painter's
/// dialogs. That ordering is why these go first.
///
/// `apply_dimming(frame, modal_area)` before the edit and confirm dialogs is
/// the scrim; the help overlay had none and still has none.
pub fn dialog_layer(d: &Dialog) -> Node<UiMsg> {
    let (w, h, node, scrim) = match d {
        Dialog::Help(x) => (52, 22, help_box(x), None),
        Dialog::Edit(x) => (56, 18, edit_box(x), Some(Scrim::Dim)),
        Dialog::Confirm(x) => (44, 7, confirm_box(x), Some(Scrim::Dim)),
    };
    fresh_ui::layer()
        .within(key())
        .anchor(Anchor::Screen(Align::Center))
        .place(Place::Over)
        .modality(Modality::Exclusive)
        .scrim(scrim)
        // A dialog is the topmost exclusive layer while it is up, so the
        // claim rides on it too — the editor's dispatcher answers its keys
        // the same way it answers the table's.
        .child(super::modal::keys(
            super::modal::KeySlot::KeybindingEditor,
            layout_reader(move |info: LayoutInfo| {
                // The painter's own `min(area - 4)`, against the box it
                // centres in.
                let node = node.clone();
                node.w(Sizing::Cells(
                    w.min(info.constraints.max_w.saturating_sub(4)),
                ))
                .h(Sizing::Cells(
                    h.min(info.constraints.max_h.saturating_sub(4)),
                ))
            }),
        ))
}

/// Take every pointer event that reaches this node and do nothing with it.
///
/// **The wheel is not on the list, and must not be.** Scrolling is
/// framework-owned: the library runs its scroll chain only for a notch
/// *nothing claimed*, so a catch-all that stops the wheel stops the table
/// inside this box from scrolling at all. There is nothing to replace it
/// with, either — a notch that scrolls nothing inside an `Exclusive` layer is
/// already absorbed by that layer, which is what the chain's `Contained`
/// says. Claiming it here only took the scroll away.
fn swallow(n: Node<UiMsg>) -> Node<UiMsg> {
    let mut g = gesture(n);
    for kind in [GestureKind::Press, GestureKind::Release, GestureKind::Move] {
        g = g.on(
            kind,
            Rc::new(|e: &Event| {
                e.stop();
                None
            }),
        );
    }
    g
}

/// A band that takes a press and does nothing with it, which is what a modal
/// backdrop is.
fn claim() -> Node<UiMsg> {
    // Full width, because an empty box measures to nothing and a band with no
    // width claims no cell.
    let mut n = gesture(col().w(Sizing::Pct(100)));
    for kind in [
        GestureKind::Press,
        GestureKind::Release,
        GestureKind::Move,
        GestureKind::Wheel,
    ] {
        n = n.on(
            kind,
            Rc::new(|e: &Event| {
                e.stop();
                None
            }),
        );
    }
    n
}

fn ring() -> String {
    pair("ui.popup_border_fg", "ui.popup_bg")
}

fn ink() -> String {
    pair("ui.popup_text_fg", "ui.popup_bg")
}

fn line(s: String, theme: String) -> Node<UiMsg> {
    text(s).theme(theme).h(Sizing::Cells(1))
}

fn blank() -> Node<UiMsg> {
    row().h(Sizing::Cells(1))
}

fn titled(s: &str, theme: &str) -> Node<UiMsg> {
    line(format!(" {s} "), attrs(theme, "ui.popup_bg", &["bold"]))
}

fn help_box(h: &Help) -> Node<UiMsg> {
    let mut rows: Vec<Node<UiMsg>> = vec![titled(&h.title, "ui.popup_border_fg")];
    for l in &h.lines {
        rows.push(match l.heading {
            true => line(
                l.key.clone(),
                attrs("ui.popup_text_fg", "ui.popup_bg", &["bold"]),
            ),
            false => row().h(Sizing::Cells(1)).children([
                text(format!("{:16}", l.key)).theme(attrs(
                    "ui.help_key_fg",
                    "ui.popup_bg",
                    &["bold"],
                )),
                text(l.desc.clone()).theme(ink()),
            ]),
        });
    }
    // The list is longer than the box on a short frame, and the painter simply
    // clipped it. A viewport says there is more.
    col()
        .theme(ring())
        .border()
        .child(fresh_ui::viewport(col().children(rows)).scrollbar().flex(1))
}

/// One field row: a padded label, the value, and an optional hint — with the
/// whole row taking the focused background when it has focus, which is what
/// the painter's "paint an empty `Paragraph` in `field_bg` first" was.
fn field_row(f: &Field) -> Node<UiMsg> {
    let bg = match f.focused {
        true => "ui.popup_selection_bg",
        false => "ui.popup_bg",
    };
    let label = match f.focused {
        true => attrs("ui.help_key_fg", bg, &["bold"]),
        false => pair("ui.popup_text_fg", bg),
    };
    let value = match f.invalid {
        true => pair("diagnostic.error_fg", bg),
        false => pair("ui.popup_text_fg", bg),
    };
    let mut spans: Vec<Node<UiMsg>> = vec![
        text(format!("   {:9}", f.label)).theme(label),
        text(f.value.clone()).theme(value),
    ];
    if f.caret {
        spans.push(text("_").theme(pair("editor.cursor", bg)));
    }
    if let Some(hint) = &f.hint {
        spans.push(text(format!("  {hint}")).theme(pair("ui.popup_text_fg", bg)));
    }
    let target = f.target;
    gesture(
        row()
            .h(Sizing::Cells(1))
            .theme(pair("ui.popup_text_fg", bg))
            .children(spans),
    )
    .on(
        GestureKind::Press,
        Rc::new(move |e: &Event| {
            if e.button != MouseButton::Left {
                return None;
            }
            e.stop();
            Some(UiMsg::Ui(UiFact::KeybindingDialog(target)))
        }),
    )
}

fn button(label: &str, focused: bool, target: Target) -> Node<UiMsg> {
    let theme = match focused {
        true => attrs("ui.popup_bg", "ui.help_key_fg", &["bold"]),
        false => ink(),
    };
    gesture(text(format!(" {label} ")).theme(theme)).on(
        GestureKind::Press,
        Rc::new(move |e: &Event| {
            if e.button != MouseButton::Left {
                return None;
            }
            e.stop();
            Some(UiMsg::Ui(UiFact::KeybindingDialog(target)))
        }),
    )
}

fn edit_box(e: &Edit) -> Node<UiMsg> {
    let mut info: Vec<Node<UiMsg>> = Vec::new();
    if let Some(err) = &e.error {
        info.push(line(
            format!("   ✗ {err}"),
            attrs("diagnostic.error_fg", "ui.popup_bg", &["bold"]),
        ));
    }
    if !e.conflicts.is_empty() {
        info.push(line(
            format!("   {}", e.conflicts_label),
            attrs("diagnostic.warning_fg", "ui.popup_bg", &["bold"]),
        ));
        for c in &e.conflicts {
            info.push(line(
                format!("     {c}"),
                pair("diagnostic.warning_fg", "ui.popup_bg"),
            ));
        }
    }

    let described = match &e.action_description {
        Some(d) => line(
            format!("            → {d}"),
            attrs("ui.popup_text_fg", "ui.popup_bg", &["italic"]),
        ),
        None => blank(),
    };

    // The action field, with its suggestion list hanging off it. The painter
    // placed the popup at `action_field.x + 12, action_field.y + 1` — under
    // the field, past the label — which is `Place::Below` and an offset.
    let action = match &e.autocomplete {
        None => field_row(&e.action_field),
        Some(a) => row()
            .h(Sizing::Cells(1))
            .children([field_row(&e.action_field), autocomplete_layer(a)]),
    };

    col().theme(ring()).border().children([
        titled(&e.title, "ui.popup_border_fg"),
        line(format!(" {}", e.instructions), ink()),
        blank(),
        field_row(&e.key_field),
        action,
        described,
        field_row(&e.context_field),
        blank(),
        col().flex(1).children(info),
        row().h(Sizing::Cells(1)).children([
            text("   ").theme(ink()),
            button(&e.save_label, e.focused_button == Some(0), Target::Save),
            text("  ").theme(ink()),
            button(&e.cancel_label, e.focused_button == Some(1), Target::Cancel),
        ]),
    ])
}

fn autocomplete_layer(a: &Autocomplete) -> Node<UiMsg> {
    // The painter windowed the list by hand — `selected - VISIBLE + 1` — and
    // drew at most eight. A `List` with the selection controlled reveals it,
    // and the window is the viewport's.
    let items = std::rc::Rc::new(a.suggestions.clone());
    let n = items.len();
    let list = fresh_ui::List::windowed(n, |i| fresh_ui::Key::Str(i.to_string().into()), {
        let items = items.clone();
        move |i| text(items[i].clone())
    })
    .focusable(false)
    .scrollbar()
    .row_theme(|_, st| match st {
        fresh_ui::widgets::RowState::Selected | fresh_ui::widgets::RowState::SelectedBlur => {
            attrs("ui.popup_bg", "ui.help_key_fg", &["bold"])
        }
        _ => pair("ui.popup_text_fg", "ui.popup_bg"),
    });
    let list = match a.selected {
        Some(i) => list.selected(i),
        None => list,
    };
    fresh_ui::layer()
        .anchor(Anchor::Parent)
        .place(Place::Below)
        // Past the label, which is where the painter put it: `x + 12`.
        .offset(12, 0)
        .fit(fresh_ui::Fit::FLIP.or(fresh_ui::Fit::CLAMP))
        .child(
            col()
                .theme(ring())
                .border()
                .w(Sizing::Cells(36))
                .h(Sizing::Cells(
                    (n.min(AUTOCOMPLETE_VISIBLE) as u16).saturating_add(2),
                ))
                .child(fresh_ui::ComponentExt::node(list).flex(1)),
        )
}

fn confirm_box(c: &Confirm) -> Node<UiMsg> {
    let targets = [
        Target::ConfirmSave,
        Target::ConfirmDiscard,
        Target::ConfirmCancel,
    ];
    let mut kids: Vec<Node<UiMsg>> = vec![text(" ").theme(ink())];
    for (i, label) in c.buttons.iter().enumerate() {
        kids.push(button(
            label,
            i == c.selected,
            *targets.get(i).unwrap_or(&Target::ConfirmCancel),
        ));
        kids.push(text("  ").theme(ink()));
    }
    col()
        .theme(pair("diagnostic.warning_fg", "ui.popup_bg"))
        .border()
        .children([
            titled(&c.title, "diagnostic.warning_fg"),
            line(format!(" {}", c.message), ink()),
            blank(),
            row().flex(1),
            row().h(Sizing::Cells(1)).children(kids),
        ])
}

/// One styled run of a header or footer line.
#[derive(Clone, Debug, PartialEq)]
pub struct Span {
    pub text: String,
    /// A theme name, already resolved from what the run *is*.
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

/// The modal's three header rows and its footer.
#[derive(Clone, Debug, PartialEq)]
pub struct Chrome {
    pub title: String,
    /// Row one: where the config lives, and which keymaps are active.
    pub path: Vec<Span>,
    /// Row two: the search bar, or the hint that says how to open it.
    pub search: Vec<Span>,
    /// Row three: the filters and the counts.
    pub filters: Vec<Span>,
    /// The key hints along the bottom.
    pub footer: Vec<Span>,
}

pub fn search_key() -> fresh_ui::Key {
    fresh_ui::Key::Str("keybinding_search".into())
}

fn spans(v: &[Span]) -> Vec<Node<UiMsg>> {
    v.iter()
        .map(|s| text(s.text.clone()).theme(s.theme.clone()))
        .collect()
}

/// The search row, which is the one part of the chrome that answers a press:
/// clicking it starts a search, which is what the painter's recorded
/// `search_bar` rectangle was for.
fn search_row(v: &[Span]) -> Node<UiMsg> {
    gesture(
        row()
            .h(Sizing::Cells(1))
            .key(search_key())
            .children(spans(v)),
    )
    .on(
        GestureKind::Press,
        Rc::new(|e: &Event| {
            if e.button != MouseButton::Left {
                return None;
            }
            e.stop();
            Some(UiMsg::Ui(UiFact::KeybindingSearch))
        }),
    )
}

/// One row of the table.
#[derive(Clone, Debug, PartialEq)]
pub enum Row {
    /// A plugin's collapsible group heading.
    Section {
        chevron: String,
        label: String,
        count: usize,
    },
    /// A binding: five columns, padded to the widths the table resolved.
    Binding {
        key: String,
        action: String,
        description: String,
        context: String,
        source: String,
        /// Whether the source reads as an accent — a custom or plugin
        /// binding, which the painter coloured apart from a keymap's.
        source_accent: bool,
    },
}

/// The table: a header, a rule, and the rows under them.
#[derive(Clone, Debug, PartialEq)]
pub struct Table {
    /// Key, Action, Description, Context, Source.
    pub columns: [String; 5],
    pub rows: Vec<Row>,
    pub selected: usize,
}

/// The five column widths for a table of this inner width.
///
/// The painter's own fractions, kept because they are the rule: sixteen
/// percent for the key capped at twenty, twenty-two for the action name capped
/// at twenty-eight, eighteen for the context clamped to fourteen-to-thirty,
/// eight fixed for the source, and the description takes what is left.
pub fn columns(inner: u16) -> [u16; 5] {
    let key = ((inner as f32 * 0.16).min(20.0)) as u16;
    let action = ((inner as f32 * 0.22).min(28.0)) as u16;
    let context = ((inner as f32 * 0.18).clamp(14.0, 30.0)) as u16;
    let source = 8u16;
    // +5 for the single-column gaps between the five.
    let desc = inner.saturating_sub(key + action + context + source + 5);
    [key, action, desc, context, source]
}

/// How many rows of the table fit in a box of `box_h`.
///
/// The bands the painter split by hand: two border rows, three of header, one
/// of footer, and two more for the table's own header and rule. Stated once so
/// the page a `PgUp` moves by and the window the rows fill cannot disagree.
pub fn table_rows(box_h: u16) -> u16 {
    box_h.saturating_sub(2 + 3 + 1 + 2)
}

fn pad(s: &str, w: usize) -> String {
    let n = s.chars().count();
    match n >= w {
        true => s.chars().take(w).collect(),
        false => format!("{s}{}", " ".repeat(w - n)),
    }
}

/// The table as a node, at whatever width it is given.
pub fn table(t: &Table) -> Node<UiMsg> {
    let t = t.clone();
    layout_reader(move |info: LayoutInfo| {
        // One column reserved for the bar, which is what `inner_width`'s
        // `saturating_sub(2)` was approximating.
        let w = info.constraints.max_w;
        let cols = columns(w.saturating_sub(2));
        let head = attrs("ui.help_key_fg", "ui.popup_bg", &["bold"]);
        let header = row().h(Sizing::Cells(1)).children(
            std::iter::once(text(" ").theme(ink()))
                .chain(
                    t.columns
                        .iter()
                        .zip(cols)
                        .enumerate()
                        .flat_map(|(i, (c, cw))| {
                            let mut out = vec![text(pad(c, cw as usize)).theme(head.clone())];
                            if i + 1 < 5 {
                                out.push(text(" ").theme(ink()));
                            }
                            out
                        }),
                )
                .collect::<Vec<_>>(),
        );
        let rule = text(format!(" {}", "─".repeat(w.saturating_sub(2) as usize))).theme(ink());
        let rows = std::rc::Rc::new(t.rows.clone());
        let n = rows.len();
        let selected = t.selected;
        let list = fresh_ui::List::windowed(n, |i| fresh_ui::Key::Str(i.to_string().into()), {
            let rows = rows.clone();
            move |i| table_row(&rows[i], &cols, i == selected)
        })
        .focusable(false)
        .scrollbar()
        .row_theme(|_, st| match st {
            fresh_ui::widgets::RowState::Selected | fresh_ui::widgets::RowState::SelectedBlur => {
                pair("ui.popup_text_fg", "ui.popup_selection_bg")
            }
            _ => ink(),
        })
        .on_select(|i| UiMsg::Ui(UiFact::KeybindingRow(i)));
        let list = match n {
            0 => list,
            _ => list.selected(t.selected.min(n - 1)),
        };
        col().children([
            header,
            rule.h(Sizing::Cells(1)),
            fresh_ui::ComponentExt::node(list).flex(1),
        ])
    })
}

/// One row of the table, with the selection's `>` in its first column.
///
/// **The marker is a glyph, not only a highlight.** The painter drew
/// `if is_selected { ">" } else { " " }` in the indicator column *and*
/// restyled the row; the row theme crossed and the glyph did not, so the
/// selection was legible on screen and invisible to anything reading the
/// cells — which is most of this modal's coverage.
fn table_row(r: &Row, cols: &[u16; 5], selected: bool) -> Node<UiMsg> {
    let indicator = match selected {
        true => ">",
        false => " ",
    };
    match r {
        // A section heading is one bold run after the indicator column; the
        // painter drew it that way too, ignoring the column grid.
        Row::Section {
            chevron,
            label,
            count,
        } => row().h(Sizing::Cells(1)).children([
            text(indicator).theme(pair("ui.help_key_fg", "ui.popup_bg")),
            text(format!("{chevron} {label} ({count})")).theme(attrs(
                "ui.help_key_fg",
                "ui.popup_bg",
                &["bold"],
            )),
        ]),
        Row::Binding {
            key,
            action,
            description,
            context,
            source,
            source_accent,
        } => {
            let accent = |on: bool, name: &str| match on {
                true => pair(name, "ui.popup_bg"),
                false => ink(),
            };
            row().h(Sizing::Cells(1)).children([
                text(indicator).theme(pair("ui.help_key_fg", "ui.popup_bg")),
                text(pad(key, cols[0] as usize)).theme(pair("ui.help_key_fg", "ui.popup_bg")),
                text(" ").theme(ink()),
                text(pad(action, cols[1] as usize))
                    .theme(pair("diagnostic.info_fg", "ui.popup_bg")),
                text(" ").theme(ink()),
                text(pad(description, cols[2] as usize)).theme(ink()),
                text(" ").theme(ink()),
                text(pad(context, cols[3] as usize)).theme(ink()),
                text(" ").theme(ink()),
                text(pad(source, cols[4] as usize))
                    .theme(accent(*source_accent, "diagnostic.info_fg")),
            ])
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::view::shell::frame::{frame_tree, Frame};
    use fresh_ui::{Size, Ui};

    fn chrome() -> Chrome {
        let ink = pair("ui.popup_text_fg", "ui.popup_bg");
        Chrome {
            title: "Keybindings — [default]".into(),
            path: vec![Span::new(" Config: /tmp/fresh.toml", ink.clone())],
            search: vec![Span::new(" Press / to search", ink.clone())],
            filters: vec![Span::new(" Scope: [All]  12 bindings", ink.clone())],
            footer: vec![Span::new(" Enter:Edit  Esc:Close", ink)],
        }
    }

    fn laid_out(w: u16, h: u16, dock: Option<u16>) -> Ui<UiMsg> {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            frame_tree(Frame {
                keybinding: Some(chrome()),
                dock,
                menu_bar: false,
                status_bar: false,
                ..Frame::default()
            }),
            Size::new(w, h),
        );
        ui
    }

    /// **Ninety percent, capped, floored, centred** — the painter's own rule,
    /// arrived at by layout instead of by four lines of arithmetic.
    #[test]
    fn the_box_is_ninety_percent_capped_and_centred() {
        let ui = laid_out(200, 60, None);
        let r = ui.rect_of(ui.find_by_key(&key()).expect("the box"));
        assert_eq!(r.w, MAX_WIDTH, "capped at 120 however wide the frame is");
        assert_eq!(r.h, 54, "ninety percent of sixty");
        assert_eq!(r.x, (200 - MAX_WIDTH as i32) / 2, "centred across");
    }

    /// A frame too small for the cap gets ninety percent of itself.
    #[test]
    fn a_narrow_frame_gets_ninety_percent_of_itself() {
        let ui = laid_out(100, 40, None);
        let r = ui.rect_of(ui.find_by_key(&key()).expect("the box"));
        assert_eq!((r.w, r.h), (90, 36));
    }

    /// And one too small for the floor gets the floor, less the two columns
    /// the painter kept clear.
    #[test]
    fn a_tiny_frame_gets_the_floor_less_its_margin() {
        let ui = laid_out(50, 15, None);
        let r = ui.rect_of(ui.find_by_key(&key()).expect("the box"));
        assert_eq!((r.w, r.h), (48, 13));
    }

    /// **Beside the dock, not under it.** The painter added `area.x` back by
    /// hand because it was handed the post-dock chrome area; naming the region
    /// the layer may occupy says the same thing where the placing happens.
    ///
    /// This is `modal_centres_within_offset_area_left_of_dock`, moved: the
    /// modal used to be placed relative to column 0 and bled left under the
    /// dock.
    #[test]
    fn the_box_centres_beside_the_dock() {
        let ui = laid_out(200, 60, Some(40));
        let r = ui.rect_of(ui.find_by_key(&key()).expect("the box"));
        assert!(r.x >= 40, "clear of a forty-column dock, at {}", r.x);
        assert_eq!(
            r.x,
            40 + (160 - MAX_WIDTH as i32) / 2,
            "centred in what is left"
        );
    }

    /// **The box is a surface now, and it consumes.** It was a rectangle and
    /// nothing else while its interior was the painter's — and it had to be,
    /// because a layer is the first thing asked at a point and the modal slot
    /// behind it would never have been reached. The interior is here now, so
    /// the box answers: its own parts by name, and everything else by taking
    /// the press and doing nothing, which is what a modal backdrop is.
    #[test]
    fn a_press_inside_the_box_is_consumed_by_it() {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            frame_tree(Frame {
                keybinding: Some(chrome()),
                menu_bar: false,
                status_bar: false,
                ..Frame::default()
            }),
            Size::new(200, 60),
        );
        let r = ui.rect_of(ui.find_by_key(&key()).expect("the box"));
        // Row four inside the box is the filters row, which says nothing.
        let got = ui.dispatch(fresh_ui::Input::press(
            fresh_ui::Point::new(r.x + 4, r.y + 4),
            fresh_ui::MouseButton::Left,
            fresh_ui::Mods::NONE,
        ));
        assert!(got.claimed, "the box takes it");
        assert!(got.msgs.is_empty(), "and says nothing: {:?}", got.msgs);
    }

    /// The search row is the exception: clicking it starts a search, which is
    /// the last of the ten rectangles the painter recorded.
    #[test]
    fn pressing_the_search_row_starts_a_search() {
        use crate::view::shell::msg::UiFact;
        let mut ui = laid_out(200, 60, None);
        let r = ui.rect_of(ui.find_by_key(&search_key()).expect("the search row"));
        let got = facts(ui.dispatch(fresh_ui::Input::press(
            fresh_ui::Point::new(r.x + 2, r.y),
            fresh_ui::MouseButton::Left,
            fresh_ui::Mods::NONE,
        )));
        assert!(got.contains(&UiFact::KeybindingSearch), "{got:?}");
    }

    /// **Nothing outside the modal is interactive**, which is what the capture
    /// band was: it preempted every walk, the shell's included, and consumed
    /// whatever it did not use. `Modality::Exclusive` says it in the tree.
    #[test]
    fn a_press_outside_the_box_reaches_nothing() {
        let mut ui = laid_out(200, 60, None);
        let got = ui.dispatch(fresh_ui::Input::press(
            fresh_ui::Point::new(2, 2),
            fresh_ui::MouseButton::Left,
            fresh_ui::Mods::NONE,
        ));
        assert!(
            got.msgs.is_empty(),
            "the editor behind it hears nothing: {:?}",
            got.msgs
        );
    }

    fn field(label: &str, value: &str, focused: bool, target: Target) -> Field {
        Field {
            label: label.into(),
            value: value.into(),
            hint: None,
            focused,
            invalid: false,
            caret: false,
            target,
        }
    }

    fn edit() -> Edit {
        Edit {
            title: "Edit binding".into(),
            instructions: "Press a key".into(),
            key_field: field("Key:", "Ctrl+S", true, Target::KeyField),
            action_field: field("Action:", "save", false, Target::ActionField),
            action_description: Some("Store the file".into()),
            context_field: field("Context:", "[normal]", false, Target::ContextField),
            error: None,
            conflicts_label: "Conflicts:".into(),
            conflicts: Vec::new(),
            save_label: "Apply".into(),
            cancel_label: "Dismiss".into(),
            focused_button: None,
            autocomplete: None,
        }
    }

    fn with_dialog(d: Dialog, w: u16, h: u16) -> Ui<UiMsg> {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            frame_tree(Frame {
                keybinding: Some(chrome()),
                keybinding_dialog: Some(d),
                menu_bar: false,
                status_bar: false,
                ..Frame::default()
            }),
            Size::new(w, h),
        );
        ui
    }

    fn facts(got: fresh_ui::Dispatch<UiMsg>) -> Vec<crate::view::shell::msg::UiFact> {
        got.msgs
            .into_iter()
            .filter_map(|m| match m {
                UiMsg::Ui(f) => Some(f),
                _ => None,
            })
            .collect()
    }

    fn painted(ui: &Ui<UiMsg>) -> Vec<String> {
        ui.spec()
            .layers()
            .iter()
            .filter_map(|i| match &i.draw {
                fresh_ui::Draw::Lines(l) => {
                    Some(l.iter().map(|s| s.to_string()).collect::<Vec<_>>())
                }
                _ => None,
            })
            .flatten()
            .collect()
    }

    /// **Every field and button answers its own press.** Each was a rectangle
    /// the painter filed and the mouse arm compared a cell against, in three
    /// chains of `point_in_rect`; the fact says what was pressed rather than
    /// where.
    #[test]
    fn the_edit_dialogs_fields_and_buttons_answer_for_themselves() {
        use crate::view::shell::msg::UiFact;
        for (needle, want) in [
            ("Key:", Target::KeyField),
            ("Action:", Target::ActionField),
            ("Context:", Target::ContextField),
            ("Apply", Target::Save),
            ("Dismiss", Target::Cancel),
        ] {
            let mut ui = with_dialog(Dialog::Edit(edit()), 120, 40);
            // Find the row carrying the label, and press inside it.
            let at = ui
                .spec()
                .layers()
                .iter()
                .find_map(|i| match &i.draw {
                    fresh_ui::Draw::Lines(l)
                        if l.iter().any(|s| s.contains(needle)) && i.rect.w > 0 =>
                    {
                        Some((i.rect.x, i.rect.y))
                    }
                    _ => None,
                })
                .unwrap_or_else(|| panic!("a row carrying {needle:?}"));
            let got = facts(ui.dispatch(fresh_ui::Input::press(
                fresh_ui::Point::new(at.0, at.1),
                fresh_ui::MouseButton::Left,
                fresh_ui::Mods::NONE,
            )));
            assert!(
                got.contains(&UiFact::KeybindingDialog(want)),
                "{needle:?} asks for {want:?}, got {got:?}"
            );
        }
    }

    /// The confirmation's three buttons likewise, with the selected one marked.
    #[test]
    fn the_confirmations_buttons_answer_for_themselves() {
        use crate::view::shell::msg::UiFact;
        let confirm = || {
            Dialog::Confirm(Confirm {
                title: "Unsaved changes".into(),
                message: "Save before closing?".into(),
                buttons: vec!["Save".into(), "Discard".into(), "Cancel".into()],
                selected: 1,
            })
        };
        let mut ui = with_dialog(confirm(), 120, 40);
        let at = ui
            .spec()
            .layers()
            .iter()
            .find_map(|i| match &i.draw {
                fresh_ui::Draw::Lines(l) if l.iter().any(|s| s.contains("Discard")) => {
                    Some((i.rect.x, i.rect.y))
                }
                _ => None,
            })
            .expect("the discard button");
        let got = facts(ui.dispatch(fresh_ui::Input::press(
            fresh_ui::Point::new(at.0, at.1),
            fresh_ui::MouseButton::Left,
            fresh_ui::Mods::NONE,
        )));
        assert!(
            got.contains(&UiFact::KeybindingDialog(Target::ConfirmDiscard)),
            "{got:?}"
        );
    }

    /// **The dialog is exclusive**, which is what `apply_dimming` plus a
    /// `Clear` over the modal meant: a press on the backdrop is consumed and
    /// does nothing.
    #[test]
    fn a_press_on_the_backdrop_does_nothing() {
        use crate::view::shell::msg::UiFact;
        let mut ui = with_dialog(Dialog::Edit(edit()), 120, 40);
        let got = facts(ui.dispatch(fresh_ui::Input::press(
            fresh_ui::Point::new(1, 1),
            fresh_ui::MouseButton::Left,
            fresh_ui::Mods::NONE,
        )));
        assert!(
            !got.iter().any(|f| matches!(f, UiFact::KeybindingDialog(_))),
            "no field claims it: {got:?}"
        );
    }

    /// The resolved action's readable form is shown only when it says
    /// something the typed name does not — the painter's own comparison, kept
    /// on the description side of the seam.
    #[test]
    fn the_action_description_is_optional() {
        let with = painted(&with_dialog(Dialog::Edit(edit()), 120, 40));
        assert!(
            with.iter().any(|r| r.contains("→ Store the file")),
            "{with:?}"
        );
        let mut e = edit();
        e.action_description = None;
        let without = painted(&with_dialog(Dialog::Edit(e), 120, 40));
        assert!(!without.iter().any(|r| r.contains("→")), "{without:?}");
    }

    /// **The autocomplete hangs off the action field**, one row down and past
    /// the label — where the painter put it with `x + 12, y + 1`.
    #[test]
    fn the_autocomplete_hangs_off_the_action_field() {
        let mut e = edit();
        e.autocomplete = Some(Autocomplete {
            suggestions: (0..20).map(|i| format!("action_{i}")).collect(),
            selected: Some(0),
        });
        let ui = with_dialog(Dialog::Edit(e), 120, 40);
        let rows = painted(&ui);
        assert!(
            rows.iter().any(|r| r.contains("action_0")),
            "the suggestions are there: {rows:?}"
        );
        // Twenty suggestions in a window of eight: a bar says there is more.
        assert!(
            ui.spec()
                .items
                .iter()
                .any(|i| matches!(i.draw, fresh_ui::Draw::Scrollbar { .. })),
            "a long suggestion list scrolls"
        );
    }

    /// The help overlay is a list of bindings and takes no press of its own.
    #[test]
    fn the_help_overlay_lists_its_bindings() {
        use crate::view::shell::msg::UiFact;
        let help = Dialog::Help(Help {
            title: "Help".into(),
            lines: vec![
                HelpLine {
                    key: "Navigation".into(),
                    desc: String::new(),
                    heading: true,
                },
                HelpLine {
                    key: "  ↑ / ↓".into(),
                    desc: "Move".into(),
                    heading: false,
                },
            ],
        });
        let mut ui = with_dialog(help, 120, 40);
        let rows = painted(&ui);
        assert!(rows.iter().any(|r| r.contains("Navigation")), "{rows:?}");
        assert!(rows.iter().any(|r| r.contains("Move")), "{rows:?}");
        let got = facts(ui.dispatch(fresh_ui::Input::press(
            fresh_ui::Point::new(60, 20),
            fresh_ui::MouseButton::Left,
            fresh_ui::Mods::NONE,
        )));
        assert!(
            !got.iter().any(|f| matches!(f, UiFact::KeybindingDialog(_))),
            "{got:?}"
        );
    }

    fn a_table(n: usize, selected: usize) -> Table {
        Table {
            columns: [
                "Key".into(),
                "Action".into(),
                "Description".into(),
                "Context".into(),
                "Source".into(),
            ],
            rows: (0..n)
                .map(|i| match i % 5 {
                    0 => Row::Section {
                        chevron: "▼".into(),
                        label: format!("plugin{i}"),
                        count: 4,
                    },
                    _ => Row::Binding {
                        key: format!("Ctrl+{i}"),
                        action: format!("act{i}"),
                        description: format!("does {i}"),
                        context: "normal".into(),
                        source: "Custom".into(),
                        source_accent: true,
                    },
                })
                .collect(),
            selected,
        }
    }

    fn with_table(t: Table, w: u16, h: u16) -> Ui<UiMsg> {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            frame_tree(Frame {
                keybinding: Some(chrome()),
                keybinding_table: Some(t),
                menu_bar: false,
                status_bar: false,
                ..Frame::default()
            }),
            Size::new(w, h),
        );
        ui
    }

    /// **The columns are the painter's fractions**, resolved from the width
    /// layout gave rather than from a rectangle handed in.
    #[test]
    fn the_columns_are_the_documented_fractions() {
        // 100 inner: key 16, action 22, context 18, source 8, +5 gaps → 31
        assert_eq!(columns(100), [16, 22, 31, 18, 8]);
        // Wide enough for the caps to bite: key 20, action 28.
        assert_eq!(columns(200)[0], 20);
        assert_eq!(columns(200)[1], 28);
        // And narrow enough for the context's floor.
        assert_eq!(columns(40)[3], 14);
    }

    /// The page a `PgUp` moves by is the box less the bands around the rows —
    /// two borders, three of header, one of footer, and the table's own header
    /// and rule.
    #[test]
    fn the_page_is_the_box_less_its_bands() {
        assert_eq!(table_rows(20), 12);
        assert_eq!(table_rows(8), 0);
    }

    /// **A row knows its own index.** The arm behind this was
    /// `(row - table_first_row_y) + scroll.offset`, against two rectangles the
    /// painter recorded — the second of which existed only because the window
    /// belonged to the painter.
    #[test]
    fn pressing_a_row_names_it() {
        use crate::view::shell::msg::UiFact;
        let mut ui = with_table(a_table(30, 0), 160, 50);
        let r = ui.rect_of(
            ui.find_by_key(&fresh_ui::Key::Str("3".into()))
                .expect("row 3"),
        );
        let at = fresh_ui::Point::new(r.x + 2, r.y);
        ui.dispatch(fresh_ui::Input::press(
            at,
            fresh_ui::MouseButton::Left,
            fresh_ui::Mods::NONE,
        ));
        let got = facts(ui.dispatch(fresh_ui::Input::release(
            at,
            fresh_ui::MouseButton::Left,
            fresh_ui::Mods::NONE,
        )));
        assert!(
            got.contains(&UiFact::KeybindingRow(3)),
            "row 3 names itself: {got:?}"
        );
    }

    /// **The window is the viewport's**, so a table longer than the box gets a
    /// bar and the selection is inside the window whatever its index.
    #[test]
    fn a_long_table_scrolls_to_its_selection() {
        let ui = with_table(a_table(200, 150), 160, 50);
        let bar = ui
            .spec()
            .items
            .iter()
            .any(|i| matches!(i.draw, fresh_ui::Draw::Scrollbar { .. }));
        assert!(bar, "two hundred rows in a box of fifty overflow");
        let selected = ui.rect_of(
            ui.find_by_key(&fresh_ui::Key::Str("150".into()))
                .expect("row 150"),
        );
        let boxed = ui.rect_of(ui.find_by_key(&key()).expect("the box"));
        assert!(
            selected.y >= boxed.y && selected.y < boxed.y + boxed.h as i32,
            "the selected row is in view at {}",
            selected.y
        );
    }

    /// **The selected row wears a `>`, not only a highlight.** The painter
    /// drew both — `if is_selected { ">" } else { " " }` in the indicator
    /// column *and* a restyled row — and only the restyle crossed, so the
    /// selection was legible on screen and invisible to every test that reads
    /// the cells. Which is most of this modal's coverage.
    #[test]
    fn the_selected_row_wears_the_indicator() {
        let ui = with_table(a_table(30, 4), 160, 50);
        let marked: Vec<i32> = ui
            .spec()
            .layers()
            .iter()
            .filter_map(|i| match &i.draw {
                fresh_ui::Draw::Lines(l) if l.iter().any(|s| &**s == ">") => Some(i.rect.y),
                _ => None,
            })
            .collect();
        assert_eq!(marked.len(), 1, "exactly one row is marked: {marked:?}");
        let row = ui.rect_of(
            ui.find_by_key(&fresh_ui::Key::Str("4".into()))
                .expect("row 4"),
        );
        assert_eq!(marked[0], row.y, "and it is the selected one");
    }

    /// **The table sits under the three header rows and over the footer**,
    /// where `Layout::vertical([Length(3), Min(5), Length(1)])` put it —
    /// inside the border, which the box now draws.
    #[test]
    fn the_table_sits_between_the_header_and_the_footer() {
        let ui = with_table(a_table(30, 0), 160, 50);
        let boxed = ui.rect_of(ui.find_by_key(&key()).expect("the box"));
        let search = ui.rect_of(ui.find_by_key(&search_key()).expect("the search row"));
        let first = ui.rect_of(
            ui.find_by_key(&fresh_ui::Key::Str("0".into()))
                .expect("row 0"),
        );
        assert!(search.y > boxed.y, "the header is inside the border");
        assert!(first.y > search.y + 1, "and the table is under it");
        assert!(
            first.y < boxed.y + boxed.h as i32 - 1,
            "with the footer below"
        );
    }
}
