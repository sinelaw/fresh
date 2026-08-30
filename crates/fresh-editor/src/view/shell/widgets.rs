//! Plugin widgets as descriptions — the first half of C.1.
//!
//! `crates/fresh-editor/src/widgets/` is a complete widget runtime: seventeen
//! thousand lines that lay a `WidgetSpec` out, paint it into
//! `TextPropertyEntry` rows, record a `HitArea` per interactive range and a
//! `LayoutBox` arena beside it, and hit-test a click by scanning byte ranges.
//! It is the largest thing this migration has left, and goal 5 — one source of
//! geometry — is what it is in tension with.
//!
//! **What moves and what does not.** The runtime's *formatting* is domain
//! knowledge and stays: `render_hint_bar` knows what a hint row looks like,
//! `Raw` is entries the plugin wrote, a `List`'s items arrive pre-rendered.
//! What moves is layout, paint and hit — the three things the tree does. So a
//! variant's migration is usually "call the same formatter, carry its row as
//! runs" rather than a rewrite, which is why this is far less than seventeen
//! thousand lines of new code.
//!
//! **How it is checked.** Every variant here is asserted equal to
//! `widgets::render_spec`'s own answer, over the shapes that runtime branches
//! on — the same arrangement that made the split separators a safe swap
//! (`the_dividers_are_where_the_separators_are`). The runtime is the oracle
//! while it is still the implementation, so a variant cannot be migrated
//! wrongly without a red test, and the oracle goes when the last variant does.
//!
//! **Coverage is explicit** ([`covered`]) because a panel is either described
//! or painted, never half of each: a spec using a variant this module has not
//! reached takes the old path whole. That is the same seam as a `Host` leaf.
//! It now answers `true` for everything but `WindowEmbed`, which is a `Host`
//! leaf by rule and never crosses — so the gate is what remains of a boundary
//! that has closed rather than a list of things still to do.

use std::borrow::Cow;

use fresh_core::api::{OverlayColorSpec, OverlayOptions, WidgetSpec};
use fresh_core::text_property::TextPropertyEntry;
use fresh_ui::{col, row, text_runs, Node, Run, Sizing};

use crate::app::shell_host::shell_theme::{Attrs, Ink, Paint};

use super::msg::UiMsg;

/// The panel surface's own colours, which every row starts from.
const BASE_FG: &str = "ui.suggestion_fg";
const BASE_BG: &str = "ui.suggestion_bg";

/// Which panel a description belongs to.
///
/// The view layer's own spelling of `app::PanelSlot`, mirrored the way
/// `modal::Slot` is, so a description carries no app types.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Slot {
    Dock,
    Floating,
}

/// What a panel's widgets need beyond their spec.
///
/// All of it is host state the runtime read off a `RenderContext`: which
/// widget has the panel's focus, which one the pointer is on, whether the
/// focus-marker gutter is reserved. Passed down rather than looked up,
/// because a description is a pure function of what it is handed.
#[derive(Clone, Debug)]
pub struct Ctx<'a> {
    pub slot: Slot,
    /// The panel's widget instance state, by key.
    ///
    /// **Read, not written.** Some kinds are authoritative once they have
    /// rendered — a `Number`'s clamped value, a `List`'s scroll offset and
    /// selection — and the spec's field is a seed the first time only. The
    /// runtime read it out of `prev` and wrote the next value into
    /// `next_state` in the same walk; a description is a pure function of what
    /// it is handed, so it reads and the host resolves. Turning these into
    /// element state proper is C.2.
    pub states: &'a std::collections::HashMap<String, crate::widgets::WidgetInstanceState>,
    /// The panel's focused widget key, or empty.
    pub focus_key: String,
    /// The widget key the pointer is over, if any.
    pub hovered_key: Option<String>,
    /// Whether focusable controls reserve the `▸ ` gutter.
    pub marker_gutter: bool,
    /// The `List`/`Tree` row the pointer is over. Every row of one list
    /// shares the list's own key, so the row identity travels separately.
    pub hovered_item_key: String,
    /// Row budget for auto-sized `List`/`Tree` widgets, when the host knows
    /// the surface's inner height.
    pub avail_height: Option<u32>,
}

/// The empty instance-state map, for a spec with no host state behind it.
///
/// A settings field or a one-off `Text` is *stateless*: the caller hands the
/// whole value down every frame and nothing is authoritative but the spec. The
/// map is what the stateful kinds read, and there is nothing to read.
pub fn no_state() -> &'static std::collections::HashMap<String, crate::widgets::WidgetInstanceState>
{
    use std::sync::OnceLock;
    static EMPTY: OnceLock<std::collections::HashMap<String, crate::widgets::WidgetInstanceState>> =
        OnceLock::new();
    EMPTY.get_or_init(Default::default)
}

impl Ctx<'static> {
    /// A context for a spec that carries all of its own state: no instance
    /// map, no focus key, no hover, no gutter, no row budget.
    pub fn plain(slot: Slot) -> Self {
        Ctx {
            slot,
            states: no_state(),
            focus_key: String::new(),
            hovered_key: None,
            marker_gutter: false,
            hovered_item_key: String::new(),
            avail_height: None,
        }
    }
}

impl Ctx<'_> {
    fn is_focused(&self, key: Option<&str>) -> bool {
        key.is_some_and(|k| !k.is_empty() && k == self.focus_key)
    }

    fn is_hovered(&self, key: Option<&str>) -> bool {
        match (key, self.hovered_key.as_deref()) {
            (Some(k), Some(h)) => !k.is_empty() && k == h,
            _ => false,
        }
    }
}

/// Whether every node of this spec is a variant this module describes.
///
/// A panel is described or painted, never half of each — a `Row` of migrated
/// children with one unmigrated child among them has nothing sensible to be.
/// So the whole tree is asked, and the answer gates the panel.
///
/// **The one no left is `WindowEmbed`**, which is a real editor window inside
/// a panel: cells, like every other `Host`, and G's rule says it never
/// migrates. Everything else crossed — see the arms below for what each one
/// needed, and `every_variant_but_the_host_leaf_is_covered` for the list.
pub fn covered(spec: &WidgetSpec) -> bool {
    match spec {
        WidgetSpec::Row { children, .. } | WidgetSpec::Col { children, .. } => {
            children.iter().all(covered)
        }
        WidgetSpec::LabeledSection { child, .. } => covered(child),
        WidgetSpec::Component { child, .. }
        | WidgetSpec::Overlay { child, .. }
        | WidgetSpec::Popup { child, .. } => covered(child),
        WidgetSpec::Button { .. } | WidgetSpec::Toggle { .. } | WidgetSpec::Number { .. } => true,
        WidgetSpec::Spacer { .. }
        | WidgetSpec::Divider { .. }
        | WidgetSpec::HintBar { .. }
        | WidgetSpec::Raw { .. } => true,

        // **The scrollable kinds cross when their state does, not before.**
        //
        // Where the *runtime* owns the scroll, it windows the rows itself and
        // reports the offset on a `LayoutBox` for the painter to draw a bar
        // from. The adapter turns rows into nodes and has nothing to say about
        // a bar, so describing one of those would render it correctly and
        // silently lose its scrollbar — worse than painting it whole. Wrapping
        // already-windowed rows in a `viewport` does not rescue it either:
        // there would be nothing to scroll, so the bar would be wrong rather
        // than missing.
        //
        // `List` has crossed because `widgets::List` windows its own rows out
        // of a viewport — the scroll is the element's and `scrollbar()` is the
        // bar. A list of *cards* has not: its rows are multi-row subtrees with
        // their own selection marking, which is the next substitution.
        // A list of *cards* crosses on `List::row_rows`: an item is a band of
        // rows rather than one, and everything else — the window in items, the
        // selection, the press — is the list above.
        WidgetSpec::List { .. } => true,
        // A multi-line field crosses on the same window the card tree does:
        // the collector is asked for the whole document and the `viewport`
        // owns which of it shows. That was the last bar the panel's painter
        // drew, so the coverage boundary closes here.
        WidgetSpec::Text { .. } => true,
        // A tree is a *flat, controlled* list of pre-rendered rows — its
        // expansion is the plugin's — so it crosses on `widgets::List` too.
        //
        // **`card_borders` scrolls in rows, so it is a viewport rather than a
        // list.** With it a tree's rows are heterogeneous — a card node takes
        // `item_height + 2` and a folder header takes one — and the runtime's
        // offset is a *row* into the flattened list, so a card straddling
        // either edge is emitted and clipped. `widgets::List` snaps to whole
        // items, which would be a different behaviour; the cells-scrolling
        // `viewport` is the same one, and it owns the offset. (`item_height >
        // 1` without `card_borders` does not occur — the only producer sets
        // the two together — so there is no third arm.)
        WidgetSpec::Tree { .. } => true,
        // **`DualList` does not scroll**, which is why it crosses through the
        // adapter with no substitution at all. It emits every row — its body
        // is `max(available, included, visible_rows)` tall and there is no
        // offset in its instance state — so there is no bar to lose. It was
        // excluded with the scrollable kinds on an assumption; the source says
        // otherwise.
        //
        // What it *does* need is the multi-hit row: each of its rows is two
        // cells side by side, one per column, each with its own `dual_focus`
        // hit over a byte range in the same row. That is exactly what
        // `entry_row_hits` gives, and without it only the left column would
        // have answered.
        WidgetSpec::DualList { .. } => true,
        // **`Dropdown`'s pop-over windows its options and paints no bar.** It
        // has a `scroll_offset`, which is why it was held back with the
        // scrollable kinds — but the boundary is the *scrollbar*, not the
        // offset, and the host's pop-over pass draws a border and the rows and
        // nothing else. `render_dropdown` clamps the scroll and slices, and
        // hands over each visible row with its absolute index; describing that
        // reproduces it exactly and loses nothing.
        WidgetSpec::Dropdown { .. } => true,

        // `WindowEmbed` is a `Host` leaf by rule and never crosses.
        _ => false,
    }
}

/// The description for a covered spec.
///
/// `width` is the panel's inner content width, which two variants need before
/// layout can run: a `Divider` is as wide as the panel by definition, and the
/// runtime pads rows to it. Passing it in rather than reading it back is the
/// rule §4.4 states — this is *content* resolved from a known extent, not
/// geometry recorded from a paint.
pub fn node(spec: &WidgetSpec, width: u16, cx: &Ctx<'_>) -> Node<UiMsg> {
    match spec {
        WidgetSpec::Row { children, wrap, .. } => {
            let r = row().children(
                children
                    .iter()
                    .map(|c| node(c, width, cx))
                    .collect::<Vec<_>>(),
            );
            match wrap {
                true => r.wrap_children(),
                false => r,
            }
        }
        WidgetSpec::Col { children, .. } => col().children(
            children
                .iter()
                .map(|c| node(c, width, cx))
                .collect::<Vec<_>>(),
        ),
        // `flex` fills the row's remainder; `cols` is a fixed gap. The runtime
        // spells the first one by handing the row a width to divide, which is
        // what `Sizing::Flex` is.
        WidgetSpec::Spacer { cols, flex, .. } => match flex {
            true => row().flex(1),
            false => row().w(Sizing::Cells(*cols as u16)),
        },
        // Full width by definition — "so the separator always matches the
        // rendered width, including a user-dragged dock, without the plugin
        // computing the width itself".
        WidgetSpec::Divider { ch, style, .. } => {
            let glyph = match ch.is_empty() {
                true => "─",
                false => ch.as_str(),
            };
            let n = width as usize / glyph.chars().count().max(1);
            let ink = match style {
                Some(o) => ink_of(o, &Ink::new(Paint::key(BASE_FG), Paint::key(BASE_BG))),
                None => Ink::new(Paint::key(BASE_FG), Paint::key(BASE_BG)),
            };
            text_runs([Run::themed(glyph.repeat(n), ink.to_string())]).h(Sizing::Cells(1))
        }
        // The formatter is the runtime's own: what a hint row *says* is domain
        // knowledge and does not move.
        WidgetSpec::HintBar { entries, .. } => entry_row(&crate::widgets::render_hint_bar(entries)),
        // Entries the plugin wrote, inlined without interpretation. That is
        // the variant's whole contract, and it is one row per entry.
        WidgetSpec::Raw { entries, .. } => {
            col().children(entries.iter().map(entry_row).collect::<Vec<_>>())
        }
        // **The first variant whose value the host owns.** Instance state is
        // authoritative once the widget has rendered and the spec's `value` is
        // a seed only, so the current value is read from the state map rather
        // than from the spec. The runtime read it and wrote the clamped result
        // back in the same walk; the description only reads, and the host
        // resolves — which is C.2's shape before C.2 lands.
        //
        // The hit is the value cell alone: "a click on the value cell begins
        // in-place editing host-side", and a click on the label does not.
        WidgetSpec::Number {
            value,
            min,
            max,
            integer,
            percent,
            label,
            focused,
            label_width,
            edit_text,
            edit_cursor,
            edit_sel_start,
            edit_sel_end,
            key,
            ..
        } => {
            let key = key.as_deref();
            let is_focused = match key.is_some_and(|k| !k.is_empty()) {
                true => cx.is_focused(key),
                false => *focused,
            };
            let cur = match key.filter(|k| !k.is_empty()).and_then(|k| cx.states.get(k)) {
                Some(crate::widgets::WidgetInstanceState::Number { value }) => *value,
                _ => *value,
            };
            let cur = crate::widgets::clamp_number(cur, *min, *max);
            let rendered = crate::widgets::render_number(
                cur,
                *integer,
                *percent,
                label,
                is_focused,
                *label_width,
                edit_text.as_deref().map(|t| crate::widgets::NumberEdit {
                    text: t,
                    cursor: *edit_cursor,
                    sel_start: *edit_sel_start,
                    sel_end: *edit_sel_end,
                }),
                cx.marker_gutter,
            );
            entry_row_hit(
                &rendered.entry,
                rendered.value_range,
                cx.slot,
                crate::widgets::HitArea {
                    row_target: false,
                    context_click: false,
                    overlay: false,
                    widget_key: key.unwrap_or("").to_string(),
                    widget_kind: "number",
                    buffer_row: 0,
                    byte_start: rendered.value_range.0,
                    byte_end: rendered.value_range.1,
                    payload: serde_json::json!({}),
                    event_type: "number_value",
                    owner_key: None,
                },
            )
        }
        // **Not the library's `Component`, and it must not become one.**
        //
        // Its documented job is two things: trap Tab among the focusables
        // *inside* it so a picker or dialog subtree keeps its own ring, and
        // name the subtree for keyed reconciliation. Those are `focus_scope`
        // and `key`, exactly. It owns no state, so making it a library
        // `Component` would hand a plugin's subtree host state it never asked
        // for — the names collide, the concepts do not.
        WidgetSpec::Component { child, key } => {
            let n = fresh_ui::focusable(node(child, width, cx)).focus_scope();
            match key.as_deref().filter(|k| !k.is_empty()) {
                Some(k) => n.key(fresh_ui::Key::Str(k.into())),
                None => n,
            }
        }
        // **Floats over the rows it would have occupied.** "Placed inside a
        // `Col`, the overlay anchors at the row it would have occupied if it
        // were a regular child — but the rows below it do not shift down."
        // That is a layer anchored to its own slot: out of flow, so the column
        // lays out as if it were not there, placed where it would have been.
        //
        // The runtime says the same thing by collecting the child's rows into
        // a separate `overlays` list carrying an anchor row, which the host
        // paints after the main entries — a second paint pass, ordered by
        // hand, for what paint order already does.
        WidgetSpec::Overlay { child, key } => {
            let k = match key.as_deref() {
                Some(k) if !k.is_empty() => fresh_ui::Key::Str(k.into()),
                _ => fresh_ui::Key::Str("overlay".into()),
            };
            let anchor = row().h(Sizing::Cells(0)).key(k.clone());
            fresh_ui::stack().children([
                anchor,
                fresh_ui::layer()
                    .anchor(fresh_ui::Anchor::Node(k))
                    .place(fresh_ui::Place::Over)
                    .child(node(child, width, cx)),
            ])
        }
        // **The same node, and its two modes are one property.** A popup is
        // an `Overlay` that may escape the panel's clipping: `screen_space`
        // "escapes the panel's clipping and is painted at screen level",
        // otherwise it "keeps panel-clipped like `Overlay`". A layer already
        // distinguishes those — `within` names the region it may be placed
        // inside, and its absence means the frame. Before that existed these
        // would have been two mechanisms; the runtime has two (`overlays` and
        // `popups`), which is why.
        WidgetSpec::Popup {
            child,
            key,
            anchor,
            screen_space,
        } => {
            let k = match key.as_deref() {
                Some(k) if !k.is_empty() => fresh_ui::Key::Str(k.into()),
                _ => fresh_ui::Key::Str("popup".into()),
            };
            let slot = row().h(Sizing::Cells(0)).key(k.clone());
            // **Panel-inner coordinates, and the panel is what they are inner
            // to.** A description cannot turn one into a frame coordinate — it
            // does not know where the panel is — so it anchors to the body and
            // says how far inside. Which is also the difference `screen_space`
            // names: both are positioned in the panel's space, and only one is
            // *confined* to it.
            let l = fresh_ui::layer()
                .place(fresh_ui::Place::Over)
                .fit(fresh_ui::Fit::CLAMP);
            let l = match anchor {
                Some([r, c]) => l
                    .anchor(fresh_ui::Anchor::Node(super::panel::body_key()))
                    .offset(*c as i16, *r as i16),
                None => l.anchor(fresh_ui::Anchor::Node(k)),
            };
            let l = match screen_space {
                true => l,
                false => l.within(super::panel::body_key()),
            };
            fresh_ui::stack().children([slot, l.child(node(child, width, cx))])
        }
        // **A hit that is not the whole row.** Form layout (`label: [v]`)
        // restricts the press to the chip so a click on the label does not
        // flip the value — the settings dialog's long-standing contract — and
        // the runtime said that with a pair of byte offsets it compared a
        // clicked byte against. `entry_row_hit` splits the row there instead,
        // so the restriction is where the nodes are.
        WidgetSpec::Toggle {
            checked,
            label,
            focused,
            indeterminate,
            label_first,
            label_width,
            key,
        } => {
            let key = key.as_deref();
            let is_focused = match key.is_some_and(|k| !k.is_empty()) {
                true => cx.is_focused(key),
                false => *focused,
            };
            let (mut entry, chip) = match label_first {
                true => crate::widgets::render_toggle_form(
                    *checked,
                    *indeterminate,
                    label,
                    is_focused,
                    *label_width,
                    width as u32,
                    cx.marker_gutter,
                ),
                false => {
                    let e = crate::widgets::render_toggle(
                        *checked,
                        label,
                        is_focused,
                        cx.marker_gutter,
                    );
                    let end = e.text.len();
                    (e, (0, end))
                }
            };
            // The pointer lights the whole chip and label the way it lights a
            // button. Focus paints its own band, so hover only shows where
            // focus is not.
            if cx.is_hovered(key) && !is_focused {
                crate::widgets::apply_hover_band(&mut entry);
            }
            entry_row_hit(
                &entry,
                chip,
                cx.slot,
                crate::widgets::HitArea {
                    row_target: false,
                    context_click: false,
                    overlay: false,
                    widget_key: key.unwrap_or("").to_string(),
                    widget_kind: "toggle",
                    buffer_row: 0,
                    byte_start: chip.0,
                    byte_end: chip.1,
                    payload: serde_json::json!({ "checked": !checked }),
                    event_type: "toggle",
                    owner_key: None,
                },
            )
        }
        // **The first interactive variant, and the seam the rest ride.**
        //
        // Its *text* is the runtime's own — `render_button` and
        // `render_bare_button` know what a framed action looks like, and that
        // is domain knowledge. What moves is the hit: the runtime recorded a
        // `HitArea` spanning the row's bytes and a click was resolved by
        // scanning those ranges; the node carries the same `HitArea` and hands
        // it over when it is pressed, so everything downstream —
        // `deliver_widget_hit`, the kind's `on_pointer`, the plugin's
        // `widget_event` — is untouched. The byte range stops being a
        // hit-test and becomes what it always was: a payload.
        //
        // A disabled button has no hit at all, matching the runtime: the
        // renderer excludes it from the tab cycle, so a click that focused and
        // activated it would be acting on a stale focus.
        WidgetSpec::Button {
            label,
            focused,
            intent,
            key,
            disabled,
            bare,
            full_width,
            hover_style,
            ..
        } => {
            let key = key.as_deref();
            let is_focused = !disabled
                && match key.is_some_and(|k| !k.is_empty()) {
                    true => cx.is_focused(key),
                    false => *focused,
                };
            // A `hover_style` applies only while the pointer is on this
            // widget, and never to a disabled one — an inert control
            // advertising itself as live would lie.
            let hovered = !disabled && cx.is_hovered(key);
            let hover = hover_style.as_ref().filter(|_| hovered);
            // Stretched by padding the *label*, before the chrome goes on, so
            // the finished control is exactly `width` columns and the focus
            // band spans the row rather than hugging the word.
            let filled = full_width.then(|| {
                crate::widgets::fill_button_label(label, *bare, cx.marker_gutter, width as u32)
            });
            let label = filled.as_deref().unwrap_or(label);
            let entry = match bare {
                true => crate::widgets::render_bare_button(
                    label, is_focused, *intent, *disabled, hover, hovered,
                ),
                false => crate::widgets::render_button(
                    label,
                    is_focused,
                    *intent,
                    *disabled,
                    cx.marker_gutter,
                    hover,
                    hovered,
                ),
            };
            let n = entry_row(&entry);
            match disabled {
                true => n,
                false => hit_node(
                    n,
                    cx.slot,
                    crate::widgets::HitArea {
                        row_target: false,
                        context_click: false,
                        overlay: false,
                        widget_key: key.unwrap_or("").to_string(),
                        widget_kind: "button",
                        buffer_row: 0,
                        byte_start: 0,
                        byte_end: entry.text.len(),
                        payload: serde_json::json!({}),
                        event_type: "activate",
                        owner_key: None,
                    },
                ),
            }
        }
        // **A border, drawn as a border.** The runtime draws this one as
        // *text*: `render_section_top_border` writes `╭─ label ─…─╮` into an
        // entry and `wrap_in_side_border` wraps every child row in `│ … │`,
        // because entries are all it has. So this is the first variant whose
        // migration is not cell-for-cell — the tree has a border, and using it
        // is the point.
        //
        // What is preserved exactly is the *geometry*, which is what anything
        // downstream depends on: one column of ring plus one of padding on
        // each side, so the child gets `panel_width - 4`, offset a row down
        // and two columns in. That is `inner_width` and `shift_channels`'
        // translation, stated as layout instead of as an arithmetic shift
        // applied to six recorded channels.
        WidgetSpec::LabeledSection { label, child, .. } => {
            let ring = Ink::new(Paint::key(BASE_FG), Paint::key(BASE_BG)).to_string();
            let framed = col().theme(ring.clone()).border().pad(1, 0).child(node(
                child,
                width.saturating_sub(4).max(1),
                cx,
            ));
            match label.is_empty() {
                true => framed,
                // The legend rides the top edge, the way every other titled
                // frame in the shell does it — a transparent strip stacked
                // over the box rather than text spliced into the ring.
                false => fresh_ui::stack().children([
                    framed,
                    col()
                        .pointer_mode(fresh_ui::PointerMode::Transparent)
                        .children([
                            row().h(Sizing::Cells(1)).children([
                                row().w(Sizing::Cells(2)),
                                text_runs([Run::themed(format!(" {label} "), ring)]),
                            ]),
                            row().flex(1),
                        ]),
                ]),
            }
        }
        // **The first kind whose state the tree owns** (C.2), and the reason
        // it could cross the boundary the others have not: `widgets::List`
        // windows its own rows out of a `viewport`, so the scroll is the
        // element's and `scrollbar()` is the bar — where the runtime windowed
        // the rows itself and reported an offset for the painter to draw one
        // from.
        //
        // Selection stays *controlled*: the plugin sets it, the host's key
        // dispatch moves it, and it arrives here each frame. The list's own
        // `Anchor` reveals it whenever it moves — "the owner passing a new one
        // down" included — which is the auto-clamp the runtime did by hand.
        //
        // The rows are still the runtime's: a `List`'s items arrive
        // pre-rendered, and what a row *says* is not this migration's
        // business. What moved is where it is, what a press on it means, and
        // who owns the window it sits in.
        WidgetSpec::List {
            items,
            item_specs,
            item_keys,
            selected_index,
            visible_rows,
            key,
            ..
        } if item_specs.is_empty() => {
            use std::rc::Rc;
            let n = items.len();
            let rows = Rc::new(items.clone());
            let keys = Rc::new(item_keys.clone());
            let list_key = key.clone().unwrap_or_default();
            let slot = cx.slot;
            let sel = *selected_index;
            let hit_keys = keys.clone();
            let list = fresh_ui::List::windowed(
                n,
                {
                    let keys = keys.clone();
                    move |i| {
                        fresh_ui::Key::Str(
                            keys.get(i).cloned().unwrap_or_else(|| i.to_string()).into(),
                        )
                    }
                },
                {
                    let rows = rows.clone();
                    move |i| entry_row(&rows[i])
                },
            )
            // The panel's focus is the host's — the runtime resolves a focus
            // key across every widget — so the list declines the ring and
            // keeps its mouse, which is what that flag means since #3108.
            .focusable(false)
            .scrollbar()
            .row_theme(|_, st| match st {
                fresh_ui::widgets::RowState::Selected
                | fresh_ui::widgets::RowState::SelectedBlur => Ink::new(
                    Paint::key("ui.popup_selection_fg"),
                    Paint::key("ui.popup_selection_bg"),
                )
                .to_string(),
                _ => Ink::new(Paint::key(BASE_FG), Paint::key(BASE_BG)).to_string(),
            })
            .on_activate_handler(Rc::new(move |i| {
                Some(UiMsg::Ui(super::msg::UiFact::WidgetHit {
                    slot,
                    hit: crate::widgets::HitArea {
                        row_target: true,
                        context_click: false,
                        overlay: false,
                        widget_key: hit_keys.get(i).cloned().unwrap_or_default(),
                        widget_kind: "list",
                        buffer_row: i as u32,
                        byte_start: 0,
                        byte_end: 0,
                        payload: serde_json::json!({
                            "index": i,
                            "key": hit_keys.get(i).cloned().unwrap_or_default(),
                        }),
                        event_type: "select",
                        // A row's hit names the List that owns it: focus moves
                        // there, and the arrows after a row click keep driving
                        // the list's selection.
                        owner_key: Some(list_key.clone()),
                    },
                }))
            }));
            let list = match sel >= 0 {
                true => list.selected(sel as usize),
                false => list,
            };
            let node = fresh_ui::ComponentExt::node(list);
            match visible_rows {
                Some(r) => node.h(Sizing::Cells(*r as u16)),
                None => node.flex(1),
            }
        }
        // **A card list is a list whose items are blocks.**
        //
        // `item_specs` makes each item a `WidgetSpec` rendered into a band of
        // rows — a rounded pill with a title, a line of detail and a rule.
        // Everything else about it is the list above: the window is in items,
        // the selection is the owner's, a press anywhere on the card selects
        // it. What it needed was for the library's `List` to stop stamping one
        // cell on every row, which is `row_rows`.
        //
        // **The card's own rows stay the runtime's**, and so does the way a
        // selected one is marked: `mark_list_card_selected` swaps the light
        // box glyphs for heavy ones and adds bold and an accent, because "no
        // background band — it reads garish over a multi-row card". That is
        // not a theme name and could not be one, so the list's own row states
        // are overridden to the base ink and the marking is applied where the
        // rows are made.
        //
        // **The gutter is reserved whether the bar is there or not.** The
        // runtime re-rendered every card one column narrower the moment the
        // list overflowed, so adding one session reflowed all of them; a
        // stable gutter is the same column, always, which is what that
        // reflow was an accident of.
        WidgetSpec::List {
            item_specs,
            item_keys,
            selected_index,
            visible_rows,
            key,
            ..
        } if !item_specs.is_empty() => {
            use std::rc::Rc;
            let card_width = (width as u32).saturating_sub(1).max(1);
            let mut cards: Vec<Vec<TextPropertyEntry>> = Vec::with_capacity(item_specs.len());
            let mut item_height: u16 = 1;
            for item in item_specs.iter() {
                let mut scratch = std::collections::HashMap::new();
                let rows = crate::widgets::render::render_collected(
                    item,
                    cx.states,
                    &mut scratch,
                    crate::widgets::RenderContext {
                        focus_key: &cx.focus_key,
                        hover_key: cx.hovered_key.as_deref().unwrap_or(""),
                        hover_item_key: &cx.hovered_item_key,
                        markdown: None,
                        marker_gutter: cx.marker_gutter,
                        avail_height: cx.avail_height,
                    },
                    card_width,
                )
                .entries;
                item_height = item_height.max((rows.len() as u16).max(1));
                cards.push(rows);
            }
            let n = cards.len();
            let cards = Rc::new(cards);
            let keys = Rc::new(item_keys.clone());
            let list_key = key.clone().unwrap_or_default();
            let slot = cx.slot;
            let sel = *selected_index;
            let hit_keys = keys.clone();
            let list = fresh_ui::List::windowed(
                n,
                {
                    let keys = keys.clone();
                    move |i| {
                        fresh_ui::Key::Str(
                            keys.get(i).cloned().unwrap_or_else(|| i.to_string()).into(),
                        )
                    }
                },
                {
                    let cards = cards.clone();
                    move |i| {
                        let selected = i as i32 == sel;
                        col().children((0..item_height as usize).map(|r| {
                            let mut e = cards[i]
                                .get(r)
                                .cloned()
                                .unwrap_or_else(crate::widgets::render::blank_list_row);
                            e.normalize_widths();
                            if selected {
                                crate::widgets::render::mark_list_card_selected(&mut e);
                            }
                            entry_row(&e)
                        }))
                    }
                },
            )
            .focusable(false)
            .row_rows(item_height)
            .scrollbar_gutter()
            .row_theme(|_, st| match st {
                fresh_ui::widgets::RowState::Hover => {
                    Ink::new(Paint::key(BASE_FG), Paint::key("ui.menu_hover_bg")).to_string()
                }
                // A selected card is marked in its own glyphs, not by a band.
                _ => Ink::new(Paint::key(BASE_FG), Paint::key(BASE_BG)).to_string(),
            })
            .on_activate_handler(Rc::new(move |i| {
                let item_key = hit_keys.get(i).cloned().unwrap_or_default();
                Some(UiMsg::Ui(super::msg::UiFact::WidgetHit {
                    slot,
                    hit: crate::widgets::HitArea {
                        row_target: true,
                        context_click: true,
                        overlay: false,
                        widget_key: item_key.clone(),
                        widget_kind: "list",
                        buffer_row: i as u32,
                        byte_start: 0,
                        byte_end: 0,
                        payload: serde_json::json!({
                            "index": i,
                            "key": item_key,
                            "list_key": list_key,
                        }),
                        event_type: "select",
                        owner_key: Some(list_key.clone()),
                    },
                }))
            }));
            let list = match sel >= 0 {
                true => list.selected(sel as usize),
                false => list,
            };
            let node = fresh_ui::ComponentExt::node(list);
            match visible_rows {
                Some(r) => node.h(Sizing::Cells(*r as u16)),
                None => node.flex(1),
            }
        }
        // **A tree is a flat list whose expansion belongs to the plugin.**
        //
        // `WidgetSpec::Tree` is not the library's `Tree`: it arrives already
        // flattened, each node carrying a `depth` and a `has_children` flag,
        // and `expanded_keys` comes down in the spec and goes back through
        // `WidgetMutation`. The library's `Tree` builds its own nesting and
        // owns `expanded` in element state, so it would fight the plugin for
        // the one fact the plugin is authoritative for. What this is, is a
        // controlled list of pre-rendered rows — `widgets::List`, again.
        //
        // Which nodes are *visible* is the plugin's `expanded_keys` applied to
        // the flat array, and `collect_visible_tree_indices` is that rule.
        // Reused rather than restated: an ancestor-open walk written twice is
        // two answers to "what is on screen".
        //
        // Each row carries up to three hits — the disclosure glyph expands,
        // the checkbox toggles, the rest selects — which is why a row had to
        // stop being one target.
        // **A tree of cards scrolls in rows, so the rows are the content.**
        //
        // With `card_borders` a node is a folder header one row tall or a
        // bordered card `item_height + 2` tall, and the runtime's offset is a
        // *row* into the flattened list: a card straddling either edge is
        // emitted and clipped. A window that snapped to whole nodes would be a
        // different behaviour, so this is a `viewport` over every visible row
        // — the library's cells-scrolling window, which clips at both edges by
        // construction and owns the offset itself.
        //
        // The rows are the runtime's, marking included: a selected card gets
        // the heavy box frame rather than a band, and those heavy glyphs are
        // also the marker `paint_dock_seamless_active_tab` keys on to merge
        // the active card into the editor beside it.
        WidgetSpec::Tree {
            nodes,
            item_keys,
            selected_index,
            visible_rows,
            key,
            expanded_keys,
            checkable,
            indent_cols,
            item_height,
            card_borders,
        } if *card_borders && *item_height > 1 => {
            let expanded: std::collections::HashSet<String> =
                expanded_keys.iter().cloned().collect();
            let visible = crate::widgets::collect_visible_tree_indices(nodes, item_keys, &expanded);
            let tree_key = key.clone().unwrap_or_default();
            let mut blocks: Vec<Chunk> = Vec::with_capacity(visible.len());
            let mut at: u32 = 0;
            let mut selected: Option<usize> = None;
            for (i, &abs) in visible.iter().enumerate() {
                let mut n = nodes[abs].clone();
                n.text.normalize_widths();
                for line in n.extra_lines.iter_mut() {
                    line.normalize_widths();
                }
                let item_key = item_keys.get(abs).cloned().unwrap_or_default();
                let open = n.has_children && !item_key.is_empty() && expanded.contains(&item_key);
                let r = crate::widgets::render_tree_row(
                    &n,
                    open,
                    *checkable,
                    *item_height,
                    true,
                    width as u32,
                    *indent_cols,
                );
                let is_selected = abs as i32 == *selected_index;
                if is_selected {
                    selected = Some(i);
                }
                // A card marks selection in its glyphs; a folder header takes
                // the band. Hover lights every row of the block, because the
                // block selects as one unit and so must light as one — and
                // selection outranks it.
                let as_card = crate::widgets::render::tree_node_is_card(&n, *checkable);
                let hovered = !is_selected
                    && !cx.hovered_item_key.is_empty()
                    && cx.hovered_item_key == item_key;
                let dress = |e: &mut TextPropertyEntry| {
                    if is_selected {
                        match as_card {
                            true => crate::widgets::render::mark_list_card_selected(e),
                            false => {
                                let mut st = e.style.clone().unwrap_or_default();
                                st.bg = Some(OverlayColorSpec::theme_key("ui.popup_selection_bg"));
                                st.extend_to_line_end = true;
                                e.style = Some(st);
                            }
                        }
                    } else if hovered {
                        crate::widgets::render::apply_hover_band(e);
                    }
                };
                let select = |a: usize, b: usize, row_target: bool| crate::widgets::HitArea {
                    row_target,
                    context_click: row_target,
                    overlay: false,
                    widget_key: tree_key.clone(),
                    widget_kind: "tree",
                    buffer_row: 0,
                    byte_start: a,
                    byte_end: b,
                    payload: serde_json::json!({ "index": abs, "key": item_key }),
                    event_type: "select",
                    owner_key: None,
                };
                let mut rows: Vec<Node<UiMsg>> = Vec::new();
                let mut primary = r.entry.clone();
                dress(&mut primary);
                let end = primary.text.len();
                let mut hits: Vec<((usize, usize), crate::widgets::HitArea)> = Vec::new();
                if let Some((a, b)) = r.disclosure_range {
                    let mut h = select(a, b, false);
                    h.event_type = "expand";
                    h.payload =
                        serde_json::json!({ "index": abs, "key": item_key, "expanded": !open });
                    hits.push(((a, b), h));
                }
                if let Some((a, b)) = r.checkbox_range {
                    let mut h = select(a, b, false);
                    h.event_type = "toggle";
                    h.payload = serde_json::json!({
                        "index": abs,
                        "key": item_key,
                        "checked": !n.checked.unwrap_or(false),
                    });
                    hits.push(((a, b), h));
                }
                // The body starts after whatever prefix the glyphs took —
                // the collector's own rule, so a press on the glyph is the
                // glyph's and the rest of the row is the card's.
                let body = match (r.checkbox_range, r.disclosure_range) {
                    (Some((_, e)), _) => e + 1,
                    (None, Some((_, e))) => e,
                    (None, None) => 0,
                };
                if body < end {
                    hits.push(((body, end), select(body, end, true)));
                }
                rows.push(entry_row_hits(&primary, cx.slot, &hits));
                for extra in r.extra_entries.iter() {
                    let mut e = extra.clone();
                    dress(&mut e);
                    let b = e.text.len();
                    rows.push(match b > 0 {
                        true => entry_row_hit(&e, (0, b), cx.slot, select(0, b, true)),
                        false => entry_row(&e),
                    });
                }
                let h = rows.len() as u32;
                blocks.push(Chunk {
                    key: fresh_ui::Key::Str(
                        match item_key.is_empty() {
                            true => i.to_string(),
                            false => item_key.clone(),
                        }
                        .into(),
                    ),
                    start: at,
                    rows,
                });
                at += h;
            }
            let node = fresh_ui::ComponentExt::node(Scrolled {
                blocks: std::rc::Rc::new(blocks),
                selected,
            });
            match visible_rows {
                Some(r) => node.h(Sizing::Cells(*r as u16)),
                None => node.flex(1),
            }
        }
        WidgetSpec::Tree {
            nodes,
            item_keys,
            selected_index,
            visible_rows,
            key,
            expanded_keys,
            checkable,
            indent_cols,
            item_height,
            card_borders,
        } if *item_height <= 1 && !card_borders => {
            use std::rc::Rc;
            let expanded: std::collections::HashSet<String> =
                expanded_keys.iter().cloned().collect();
            let visible = Rc::new(crate::widgets::collect_visible_tree_indices(
                nodes, item_keys, &expanded,
            ));
            let nodes = Rc::new(nodes.clone());
            let keys = Rc::new(item_keys.clone());
            let tree_key = key.clone().unwrap_or_default();
            let (slot, checkable, indent) = (cx.slot, *checkable, *indent_cols);
            let sel_abs = *selected_index;
            let n = visible.len();

            let row_at = {
                let (nodes, keys, visible) = (nodes.clone(), keys.clone(), visible.clone());
                let tree_key = tree_key.clone();
                move |i: usize| -> Node<UiMsg> {
                    let abs = visible[i];
                    let mut node = nodes[abs].clone();
                    node.text.normalize_widths();
                    let item_key = keys.get(abs).cloned().unwrap_or_default();
                    let open =
                        node.has_children && !item_key.is_empty() && expanded.contains(&item_key);
                    let r = crate::widgets::render_tree_row(
                        &node,
                        open,
                        checkable,
                        1,
                        false,
                        width as u32,
                        indent,
                    );
                    let end = r.entry.text.len();
                    let hit = |kind: &'static str,
                               a: usize,
                               b: usize,
                               payload: serde_json::Value,
                               row_target: bool| {
                        (
                            (a, b),
                            crate::widgets::HitArea {
                                row_target,
                                context_click: row_target,
                                overlay: false,
                                widget_key: tree_key.clone(),
                                widget_kind: "tree",
                                buffer_row: i as u32,
                                byte_start: a,
                                byte_end: b,
                                payload,
                                event_type: kind,
                                owner_key: None,
                            },
                        )
                    };
                    // Order is the collector's: the narrow targets are named
                    // before the row-wide one, so a byte inside the glyph or
                    // the box belongs to it rather than to `select`.
                    let mut hits = Vec::new();
                    if let Some((a, b)) = r.disclosure_range {
                        hits.push(hit(
                            "expand",
                            a,
                            b,
                            serde_json::json!({
                                "index": abs, "key": item_key, "expanded": !open,
                            }),
                            false,
                        ));
                    }
                    if let Some((a, b)) = r.checkbox_range {
                        hits.push(hit(
                            "toggle",
                            a,
                            b,
                            serde_json::json!({
                                "index": abs,
                                "key": item_key,
                                "checked": !node.checked.unwrap_or(false),
                            }),
                            false,
                        ));
                    }
                    hits.push(hit(
                        "select",
                        0,
                        end,
                        serde_json::json!({ "index": abs, "key": item_key }),
                        true,
                    ));
                    entry_row_hits(&r.entry, slot, &hits)
                }
            };

            let list = fresh_ui::List::windowed(
                n,
                {
                    let (keys, visible) = (keys.clone(), visible.clone());
                    move |i| {
                        fresh_ui::Key::Str(
                            keys.get(visible[i])
                                .cloned()
                                .unwrap_or_else(|| i.to_string())
                                .into(),
                        )
                    }
                },
                row_at,
            )
            .focusable(false)
            .scrollbar()
            .row_theme(|_, st| match st {
                fresh_ui::widgets::RowState::Selected
                | fresh_ui::widgets::RowState::SelectedBlur => Ink::new(
                    Paint::key("ui.popup_selection_fg"),
                    Paint::key("ui.popup_selection_bg"),
                )
                .to_string(),
                _ => Ink::new(Paint::key(BASE_FG), Paint::key(BASE_BG)).to_string(),
            });
            // The spec's selection is an index into the *whole* array; the
            // list's is into the visible window, which is the same array with
            // the collapsed subtrees taken out.
            let list = match visible.iter().position(|&a| a as i32 == sel_abs) {
                Some(i) => list.selected(i),
                None => list,
            };
            let node = fresh_ui::ComponentExt::node(list);
            match visible_rows {
                Some(r) => node.h(Sizing::Cells(*r as u16)),
                None => node.flex(1),
            }
        }
        // **A multi-line field's window is the tree's.**
        //
        // The collector windows the document itself, reports the offset on a
        // `LayoutBox`, and the panel's scrollbar pass draws a bar over the
        // rightmost column from it — the last kind whose bar the painter drew.
        // So the collector is asked for the *whole* document instead: its
        // `rows` is the window, and handing it one as tall as the text makes
        // it emit every line and clamp its own scroll to zero. The window is
        // then a `viewport` of the row budget the spec asked for, with the
        // library's own bar, and the caret is what it reveals.
        //
        // A label stays out of it. The collector emits it as row zero and
        // windows only the text under it, so scrolling it away would be a
        // different field.
        WidgetSpec::Text {
            rows,
            label,
            value,
            key,
            ..
        } if *rows > 1 => {
            // As many lines as the text has, from whichever of the two is
            // authoritative — instance state once it exists, the spec before.
            let lines = key
                .as_deref()
                .filter(|k| !k.is_empty())
                .and_then(|k| cx.states.get(k))
                .and_then(|st| match st {
                    crate::widgets::WidgetInstanceState::Text { editor, .. } => {
                        Some(editor.line_count())
                    }
                    _ => None,
                })
                .unwrap_or_else(|| value.split('\n').count());
            let mut whole = spec.clone();
            if let WidgetSpec::Text { rows: r, .. } = &mut whole {
                *r = (lines as u32).max(*rows);
            }
            let mut scratch = std::collections::HashMap::new();
            let mut out = crate::widgets::render::render_collected(
                &whole,
                cx.states,
                &mut scratch,
                crate::widgets::RenderContext {
                    focus_key: &cx.focus_key,
                    hover_key: cx.hovered_key.as_deref().unwrap_or(""),
                    hover_item_key: &cx.hovered_item_key,
                    markdown: None,
                    marker_gutter: cx.marker_gutter,
                    avail_height: cx.avail_height,
                },
                width as u32,
            );
            let head = usize::from(!label.is_empty());
            let caret = out.focus_cursor.map(|c| c.byte_in_row as usize);
            let caret_row = out.focus_cursor.map(|c| c.buffer_row as usize);
            // **The rows are formatted once and built for the window only.**
            // Formatting the whole document is what asking the collector for
            // it costs, and it is padding and overlay arithmetic per line;
            // building a node per line is what would actually scale badly, and
            // `List::windowed` is the same window `widgets::List` gives every
            // other kind here. Its rows are one cell each, so its item scroll
            // *is* the row scroll the runtime had.
            let rows_src = std::rc::Rc::new(out.entries.split_off(head));
            let hits = std::rc::Rc::new(out.hits.clone());
            let n = rows_src.len();
            let slot = cx.slot;
            let sel = caret_row.and_then(|r| r.checked_sub(head));
            let list = fresh_ui::List::windowed(n, |i| fresh_ui::Key::Str(i.to_string().into()), {
                let rows_src = rows_src.clone();
                move |i| {
                    let mine: Vec<((usize, usize), crate::widgets::HitArea)> = hits
                        .iter()
                        .filter(|h| h.buffer_row as usize == i + head)
                        .map(|h| ((h.byte_start, h.byte_end), h.clone()))
                        .collect();
                    let at = caret.filter(|_| sel == Some(i));
                    match mine.is_empty() && at.is_none() {
                        true => entry_row(&rows_src[i]),
                        false => row_pieces(&rows_src[i], slot, &mine, at),
                    }
                }
            })
            .focusable(false)
            .scrollbar()
            // The rows carry their own colours — a focused field paints its
            // own background band per row — so the list's row states must not
            // paint over them.
            .row_theme(|_, _| Ink::new(Paint::key(BASE_FG), Paint::key(BASE_BG)).to_string());
            // "Selected" here means "where the caret is", which is what the
            // list reveals when it moves. That is the whole of the auto-clamp
            // the runtime did by hand.
            let list = match sel {
                Some(i) => list.selected(i),
                None => list,
            };
            let body = fresh_ui::ComponentExt::node(list).h(Sizing::Cells(*rows as u16));
            match head {
                0 => body,
                _ => col().children([entry_row(&out.entries[0]), body]),
            }
        }
        // The rest, with collectors of their own. See [`collected`].
        WidgetSpec::Text { .. }
        | WidgetSpec::List { .. }
        | WidgetSpec::Tree { .. }
        | WidgetSpec::Dropdown { .. }
        | WidgetSpec::DualList { .. } => collected(spec, width, cx),
        // `covered` gates this; reaching it is a bug in the caller rather than
        // a spec the plugin got wrong, so it is loud in debug and empty in
        // release rather than silently dropping a panel's content.
        other => {
            debug_assert!(false, "widget variant not covered: {other:?}");
            row().h(Sizing::Cells(0))
        }
    }
}

/// One addressable run of rows inside a [`Scrolled`]: a card tree's node, or
/// a single line of a text area. `start` is where it begins in the content.
struct Chunk {
    key: fresh_ui::Key,
    /// First row of this block within the whole tree's rows.
    start: u32,
    rows: Vec<Node<UiMsg>>,
}

#[derive(Default)]
struct ScrolledState {
    anchor: Option<std::rc::Rc<fresh_ui::behavior::Anchor>>,
    revealed: fresh_ui::behavior::Cache<usize, ()>,
}

/// Rows in a window that owns its own scroll, revealing one of them.
///
/// **A component, for one reason: the reveal.** The runtime scrolled to keep
/// the selection visible unless the user had scrolled by wheel, and it did
/// that by writing an offset into the side table it also read. Here the
/// offset is the viewport's, which is the whole point — so the only thing
/// left to say is "put this row in the window", which is `Anchor::reveal`,
/// and an anchor is registered at mount. The memo is what keeps the reveal a
/// statement about the *selection moving* rather than about every build:
/// asking on every one would fight the wheel, which is a statement about the
/// window.
///
/// The scroll survives a rebuild because the element does, which is what
/// retains it — no `scroll_offset` is carried anywhere.
struct Scrolled {
    blocks: std::rc::Rc<Vec<Chunk>>,
    /// Which block is selected, if any.
    selected: Option<usize>,
}

impl fresh_ui::Component<UiMsg> for Scrolled {
    type State = ScrolledState;

    fn init(&self, cx: &mut fresh_ui::InitCx<'_, UiMsg>) -> ScrolledState {
        ScrolledState {
            anchor: Some(cx.register(fresh_ui::behavior::Anchor::default())),
            ..ScrolledState::default()
        }
    }

    fn build(&self, s: &ScrolledState, _cx: &mut fresh_ui::BuildCx<'_, UiMsg>) -> Node<UiMsg> {
        if let (Some(a), Some(i)) = (s.anchor.clone(), self.selected) {
            if let Some(b) = self.blocks.get(i) {
                let (start, rows) = (b.start, b.rows.len() as u32);
                s.revealed.get_or(i, move || {
                    // The last row first, then the first: the shortest move
                    // that shows the end, then the one that shows the start,
                    // so a block taller than the window anchors to its top —
                    // which is the rule the runtime spelled with a `min`.
                    a.reveal(start + rows.saturating_sub(1));
                    a.reveal(start);
                });
            }
        }
        let mut content = col();
        for b in self.blocks.iter() {
            content = content.child(
                col()
                    .key(b.key.clone())
                    .children(b.rows.iter().map(|r| r.clone())),
            );
        }
        let body = fresh_ui::viewport(content).scrollbar();
        match s.anchor.clone() {
            Some(a) => body.anchor_to(a),
            None => body,
        }
    }
}

/// **Every remaining variant, through the collector it already has.**
///
/// `Text`, `List`, `Tree`, `Dropdown` and `DualList` are different in kind
/// from the nine written out above. Each of them already has a collector that
/// knows its rendering — where a list's rows come from, how a dropdown's
/// trigger reads, what a tree's indent guides look like — and reproducing that
/// by hand would be rewriting seven thousand lines to get the same cells.
///
/// So the collector runs, and this turns what it produced into nodes: its rows
/// become nodes, each `HitArea` becomes a gesture on the sub-range it names,
/// each overlay row becomes a layer at the row it anchors to. **That is what
/// deletes the byte-range scan and the `LayoutBox` arena** — for all five at
/// once rather than five times over — because after it a press is resolved by
/// hit-testing a rectangle layout produced, not by walking recorded ranges.
///
/// **It is a stage, not the end.** The runtime is a *formatter* here: it still
/// decides what a list row looks like, and the tree owns where it is and what
/// a press on it means. Replacing that formatting with `widgets::List` and
/// `widgets::Tree`, so a plugin's list is the list the settings form uses, is
/// the step after — and doing this first makes that a substitution rather than
/// a rewrite.
fn collected(spec: &WidgetSpec, width: u16, cx: &Ctx<'_>) -> Node<UiMsg> {
    // The collector writes the next instance state as it renders. A
    // description cannot own that write, so it goes to a scratch map and the
    // host resolves the real one — the same split `Number` makes, at the scale
    // of a subtree. C.2 is where this stops being a scratch map.
    let mut scratch = std::collections::HashMap::new();
    let out = crate::widgets::render::render_collected(
        spec,
        cx.states,
        &mut scratch,
        crate::widgets::RenderContext {
            focus_key: &cx.focus_key,
            hover_key: cx.hovered_key.as_deref().unwrap_or(""),
            hover_item_key: &cx.hovered_item_key,
            markdown: None,
            marker_gutter: cx.marker_gutter,
            avail_height: cx.avail_height,
        },
        width as u32,
    );
    rows_with_hits(
        &out.entries,
        &out.hits,
        cx,
        &out.overlays,
        &out.popups,
        out.focus_cursor,
    )
}

/// The rows of a collected subtree, each carrying whatever hits land on it.
///
/// A hit names a row and a byte range within it, which is exactly what
/// [`entry_row_hit`] turns into a gesture on a piece of that row. A row with
/// no hit is a plain styled row; a row with several — a list's rows each carry
/// their own — becomes as many pieces as there are ranges.
///
/// **The floats anchor to nodes, not to coordinates.** An overlay row and a
/// dropdown's pop-over both name a position *inside this sub-render* — row 3
/// of it, or the column the `[value ▼]` button starts at. Neither is a frame
/// coordinate, and a description cannot turn one into a frame coordinate
/// because it does not know where the panel is. So each hangs off the node it
/// actually belongs to — the sub-render's own box, or the trigger's row — and
/// `offset` says where inside that the real anchor is. That is also what makes
/// the pop-over's flip correct: it flips clear of the trigger's row.
fn rows_with_hits(
    entries: &[TextPropertyEntry],
    hits: &[crate::widgets::HitArea],
    cx: &Ctx<'_>,
    overlays: &[crate::widgets::OverlayRow],
    popups: &[crate::widgets::PanelPopup],
    caret: Option<crate::widgets::FocusCursor>,
) -> Node<UiMsg> {
    let mut kids: Vec<Node<UiMsg>> = Vec::with_capacity(entries.len());
    for (i, entry) in entries.iter().enumerate() {
        let mine: Vec<&crate::widgets::HitArea> =
            hits.iter().filter(|h| h.buffer_row as usize == i).collect();
        // The caret is on at most one row, and the marker goes in that row's
        // pieces so its cell comes from the glyphs rather than from measuring
        // them a second time.
        let at = caret
            .filter(|c| c.buffer_row as usize == i)
            .map(|c| c.byte_in_row as usize);
        let mut node = match mine.is_empty() && at.is_none() {
            true => entry_row(entry),
            // **Every hit on the row, not the first.** A tree row has three
            // and a dual list's has two; keeping only one silently made the
            // others unclickable.
            false => row_pieces(
                entry,
                cx.slot,
                &mine
                    .iter()
                    .map(|h| ((h.byte_start, h.byte_end), (*h).clone()))
                    .collect::<Vec<_>>(),
                at,
            ),
        };
        // An open dropdown's option list hangs off the row its trigger is on,
        // one row down and at the button's own column.
        for p in popups.iter().filter(|p| p.anchor_row as usize == i) {
            node = row()
                .h(Sizing::Cells(1))
                .children([node, popup_layer(p, cx)]);
        }
        kids.push(node);
    }
    let body = col().children(kids);
    // A pop-over whose anchor row is past the collector's own rows has no row
    // to hang off; it falls back to the body, which is where the runtime put
    // it too (`inner.y + anchor_row`).
    let stray: Vec<&crate::widgets::PanelPopup> = popups
        .iter()
        .filter(|p| p.anchor_row as usize >= entries.len())
        .collect();
    if overlays.is_empty() && stray.is_empty() {
        return body;
    }
    let mut stack = vec![body];
    // Rows the collector floated: they anchor at a row of the sub-render and
    // paint over what is there, without having consumed its height. A layer
    // says both — and `offset` says which row, because a completion list runs
    // past the rows its own sub-render has (a one-line text input's popup is
    // anchored at rows 1..n) and there is no node at row 4 of a one-row box.
    for o in overlays {
        stack.push(
            fresh_ui::layer()
                .anchor(fresh_ui::Anchor::Parent)
                .place(fresh_ui::Place::Over)
                .offset(0, o.buffer_row as i16)
                .fit(fresh_ui::Fit::CLAMP)
                .child(
                    row()
                        .h(Sizing::Cells(1))
                        .theme(Ink::new(Paint::key(BASE_FG), Paint::key("ui.popup_bg")).to_string())
                        .child(entry_row(&o.entry)),
                ),
        );
    }
    for p in stray {
        stack.push(
            popup_layer(p, cx)
                .anchor(fresh_ui::Anchor::Parent)
                .place(fresh_ui::Place::Over)
                .offset(p.anchor_col as i16, p.anchor_row as i16 + 1),
        );
    }
    fresh_ui::stack().children(stack)
}

/// An open dropdown's option list, as a layer hanging off its trigger's row.
///
/// **Not confined to the panel**: it "extends past the panel/modal border
/// instead of growing/clipping it", which is a layer that names no region —
/// the frame is its bounds. Each row selects an absolute option index, which
/// is the payload the runtime's own hit carries.
///
/// The rows arrive already windowed: `render_dropdown` clamps the scroll and
/// slices, and `row_indices` carries the absolute index of each. So there is
/// no scroll for the tree to own here and no bar to lose — which is why
/// `Dropdown` crosses through the adapter rather than waiting for a
/// substitution, unlike the kinds whose scrollbar the painter draws.
fn popup_layer(p: &crate::widgets::PanelPopup, cx: &Ctx<'_>) -> Node<UiMsg> {
    let rows: Vec<Node<UiMsg>> = p
        .entries
        .iter()
        .enumerate()
        .map(|(i, e)| match p.row_indices.get(i) {
            Some(idx) => entry_row_hit(
                e,
                (0, e.text.len()),
                cx.slot,
                crate::widgets::HitArea {
                    row_target: true,
                    context_click: false,
                    overlay: true,
                    widget_key: p.widget_key.clone(),
                    widget_kind: "dropdown",
                    buffer_row: i as u32,
                    byte_start: 0,
                    byte_end: e.text.len(),
                    payload: serde_json::json!({ "index": idx }),
                    event_type: "dropdown_select",
                    owner_key: None,
                },
            ),
            None => entry_row(e),
        })
        .collect();
    // A press anywhere in the box that is not on an option — its border — is
    // swallowed rather than allowed through: "so the modal isn't dismissed and
    // the list stays open". The runtime said that by testing the recorded
    // `popup_rect` before anything else; here the box is the rectangle and
    // taking the press is what claiming it means.
    let box_node = fresh_ui::gesture(
        col()
            .theme(
                Ink::new(Paint::key("ui.popup_border_fg"), Paint::key("ui.popup_bg")).to_string(),
            )
            .border()
            .children(rows),
    )
    .on(
        fresh_ui::GestureKind::Press,
        std::rc::Rc::new(|e: &fresh_ui::Event| {
            e.stop();
            None
        }),
    );
    fresh_ui::layer()
        .anchor(fresh_ui::Anchor::Parent)
        .place(fresh_ui::Place::Below)
        .offset(p.anchor_col as i16, 0)
        .fit(fresh_ui::Fit::FLIP.or(fresh_ui::Fit::CLAMP))
        .child(box_node)
}

/// Wrap a widget's node so a press on it delivers the widget's own hit.
///
/// This is what replaces the byte-range scan. `deliver_widget_hit` — the
/// dispatch all three frontends share — takes a `HitArea` and does the rest:
/// focus the owner, run the kind's `on_pointer`, fire the plugin's event. It
/// does not change; what changes is that the tree *finds* the widget, by
/// hit-testing a rectangle it laid out, instead of the host reconstructing it
/// from a row and a byte offset.
fn hit_node(n: Node<UiMsg>, slot: Slot, hit: crate::widgets::HitArea) -> Node<UiMsg> {
    fresh_ui::gesture(n).on(
        fresh_ui::GestureKind::Press,
        std::rc::Rc::new(move |e: &fresh_ui::Event| {
            if e.button != fresh_ui::MouseButton::Left {
                return None;
            }
            e.stop();
            Some(UiMsg::Ui(super::msg::UiFact::WidgetHit {
                slot,
                hit: hit.clone(),
            }))
        }),
    )
}

/// One styled row, from a `TextPropertyEntry`.
///
/// **The load-bearing helper**: most variants of the runtime end in an entry,
/// so most of them migrate through here. It is the span walk
/// `render_widget_entry_line` does — split at inline-overlay boundaries, merge
/// overlapping overlays per property in declaration order — with the theme
/// *names* kept instead of resolved colours, because the fold resolves them
/// and that is what makes the row inspectable and the web able to paint it.
pub fn entry_row(entry: &TextPropertyEntry) -> Node<UiMsg> {
    text_runs(entry_runs(entry, &[]).into_iter().map(|(_, r)| r)).h(Sizing::Cells(1))
}

/// One styled row whose `range` of bytes answers a press with `hit`.
///
/// **A byte range becomes a rectangle here.** The runtime kept the range and
/// compared a clicked byte against it; a toggle in form layout (`label: [v]`)
/// restricts its hit to the chip so clicking the label does not flip the
/// value, and that restriction was a pair of byte offsets. The row is split at
/// those offsets into up to three pieces and the middle one is the gesture, so
/// the same rule is expressed as where the nodes are.
pub fn entry_row_hit(
    entry: &TextPropertyEntry,
    range: (usize, usize),
    slot: Slot,
    hit: crate::widgets::HitArea,
) -> Node<UiMsg> {
    entry_row_hits(entry, slot, &[(range, hit)])
}

/// One styled row carrying several hits, each over the bytes it names.
///
/// **A row is not one target.** A tree row has three — the disclosure glyph
/// expands, the checkbox toggles, the rest selects — and the runtime told them
/// apart by comparing a clicked byte against three ranges. The row is split at
/// every range's edges and each piece carries its own hit, so the same three
/// answers come from three rectangles.
pub fn entry_row_hits(
    entry: &TextPropertyEntry,
    slot: Slot,
    hits: &[((usize, usize), crate::widgets::HitArea)],
) -> Node<UiMsg> {
    row_pieces(entry, slot, hits, None)
}

/// The key of the caret's cell, for the host that has to place a hardware
/// cursor there.
///
/// **The caret is a node, because its cell is layout's answer.** The runtime
/// reported a row and a byte offset and the painter turned that into a screen
/// cell with `inner.x + byte_to_screen_col(...)` — a second measurement of
/// text the row had already measured to paint it. A zero-width marker at the
/// caret's byte lands where the glyphs put it, and the host reads the
/// rectangle back rather than recomputing it.
pub fn caret_key() -> fresh_ui::Key {
    fresh_ui::Key::Str("widget_caret".into())
}

/// One row split into its hit pieces, with the caret's marker at `caret` if it
/// falls on this row.
fn row_pieces(
    entry: &TextPropertyEntry,
    slot: Slot,
    hits: &[((usize, usize), crate::widgets::HitArea)],
    caret: Option<usize>,
) -> Node<UiMsg> {
    let mut cuts: Vec<usize> = Vec::with_capacity(hits.len() * 2 + 1);
    for ((a, b), _) in hits {
        cuts.push(*a);
        cuts.push(*b);
    }
    cuts.extend(caret);
    let runs = entry_runs(entry, &cuts);
    // Group consecutive runs by which hit covers them. A byte covered by two
    // ranges takes the first that names it, which is the order the collector
    // pushed them — the same precedence the byte-range scan had.
    let owner = |at: &std::ops::Range<usize>| -> Option<usize> {
        hits.iter()
            .position(|((a, b), _)| at.start >= *a && at.end <= *b && b > a)
    };
    let marker = || {
        row()
            .key(caret_key())
            .w(Sizing::Cells(0))
            .h(Sizing::Cells(1))
    };
    let mut kids: Vec<Node<UiMsg>> = Vec::new();
    let mut group: Vec<Run> = Vec::new();
    let mut group_of: Option<usize> = None;
    let flush = |kids: &mut Vec<Node<UiMsg>>, group: &mut Vec<Run>, of: Option<usize>| {
        if group.is_empty() {
            return;
        }
        let piece = text_runs(std::mem::take(group)).h(Sizing::Cells(1));
        kids.push(match of {
            Some(i) => hit_node(piece, slot, hits[i].1.clone()),
            None => piece,
        });
    };
    let mut end = 0usize;
    for (at, run) in runs {
        let of = owner(&at);
        // The caret sits *between* two runs, so the group before it has to
        // close whether or not the hit changes there.
        if of != group_of || caret == Some(at.start) {
            flush(&mut kids, &mut group, group_of);
            group_of = of;
        }
        if caret == Some(at.start) {
            kids.push(marker());
        }
        end = at.end;
        group.push(run);
    }
    flush(&mut kids, &mut group, group_of);
    // A caret past the last glyph — the usual place for one — is the row's
    // end, which no run starts at.
    if caret.is_some_and(|c| c >= end) {
        kids.push(marker());
    }
    row().h(Sizing::Cells(1)).children(kids)
}

/// The styled pieces of an entry, each with the byte range it covers.
///
/// **The load-bearing helper**: most variants of the runtime end in an entry,
/// so most of them migrate through here. It is the span walk
/// `render_widget_entry_line` does — split at inline-overlay boundaries, snap
/// each to a grapheme cluster, merge overlapping overlays per property in
/// declaration order so a later one can set `bg` without wiping an earlier
/// one's italic — with the theme **names** kept rather than resolved to
/// colours, because the fold resolves them and that is what makes the row
/// inspectable and lets the web paint it.
///
/// `extra` are additional byte offsets to split at, for a caller that needs a
/// piece boundary the overlays do not provide.
fn entry_runs(entry: &TextPropertyEntry, extra: &[usize]) -> Vec<(std::ops::Range<usize>, Run)> {
    let mut normalized = entry.clone();
    normalized.normalize_widths();
    let mut text = normalized.text.clone();
    while text.ends_with('\n') {
        text.pop();
    }

    let base = match normalized.style.as_ref() {
        Some(o) => ink_of(o, &Ink::new(Paint::key(BASE_FG), Paint::key(BASE_BG))),
        None => Ink::new(Paint::key(BASE_FG), Paint::key(BASE_BG)),
    };

    if text.is_empty() {
        return vec![(0..0, Run::themed("", base.to_string()))];
    }

    // Snap every boundary to a grapheme cluster. An overlay offset can land
    // mid-codepoint after a row is truncated with a multi-byte `…` — the
    // overlay's end is not re-clamped to the new text — and slicing there
    // panics. The runtime floors to the previous boundary; so does this.
    let snap = |i: usize| {
        let i = i.min(text.len());
        match text.is_char_boundary(i) {
            true => i,
            false => crate::primitives::grapheme::prev_grapheme_boundary(&text, i),
        }
    };
    let bounds: Vec<usize> = std::iter::once(0)
        .chain(std::iter::once(text.len()))
        .chain(extra.iter().map(|i| snap(*i)))
        .chain(
            normalized
                .inline_overlays
                .iter()
                .flat_map(|o| [snap(o.start), snap(o.end)]),
        )
        .collect::<std::collections::BTreeSet<usize>>()
        .into_iter()
        .collect();

    let mut out: Vec<(std::ops::Range<usize>, Run)> = Vec::with_capacity(bounds.len());
    for w in bounds.windows(2) {
        let (a, b) = (w[0], w[1]);
        if a >= b {
            continue;
        }
        // Merge, do not replace: a later overlay overrides individual
        // properties without wiping the earlier one's others. The text-input
        // renderer relies on it — a placeholder sets fg + italic and the
        // focused overlay sets bg only, and replacing would clear the italic.
        let mut ink = base.clone();
        for o in &normalized.inline_overlays {
            let (os, oe) = (o.start.min(text.len()), o.end.min(text.len()));
            if a >= os && b <= oe && oe > os {
                ink = ink_of(&o.style, &ink);
            }
        }
        out.push((a..b, Run::themed(&text[a..b], ink.to_string())));
    }
    out
}

/// Apply an overlay's properties over an existing ink.
///
/// A colour the overlay does not set is inherited, which is the merge the
/// painter does. A `ThemeKey` stays a name; an `Rgb` becomes a literal, which
/// is the one thing in the display list with no theme entry behind it and is
/// honest about that (F.2).
fn ink_of(o: &OverlayOptions, under: &Ink) -> Ink {
    let paint = |c: &OverlayColorSpec| match c {
        OverlayColorSpec::ThemeKey(k) => Paint::key(Cow::Owned(k.clone())),
        OverlayColorSpec::Rgb(r, g, b) => Paint::Lit(ratatui::style::Color::Rgb(*r, *g, *b)),
    };
    let mut attrs = under.attrs;
    for (on, a) in [
        (o.bold, Attrs::BOLD),
        (o.italic, Attrs::ITALIC),
        (o.underline, Attrs::UNDERLINE),
        (o.strikethrough, Attrs::STRIKETHROUGH),
    ] {
        if on {
            attrs = attrs | a;
        }
    }
    Ink {
        fg: o.fg.as_ref().map(paint).unwrap_or_else(|| under.fg.clone()),
        bg: o.bg.as_ref().map(paint).unwrap_or_else(|| under.bg.clone()),
        attrs,
    }
}

#[cfg(test)]
mod tests {
    use super::super::msg::UiFact;
    use super::*;
    use fresh_core::api::HintEntry;
    use fresh_ui::{Size, Ui};

    const WIDTH: u16 = 40;

    fn no_state() -> &'static std::collections::HashMap<String, crate::widgets::WidgetInstanceState>
    {
        use std::sync::OnceLock;
        static EMPTY: OnceLock<
            std::collections::HashMap<String, crate::widgets::WidgetInstanceState>,
        > = OnceLock::new();
        EMPTY.get_or_init(Default::default)
    }

    fn cx() -> Ctx<'static> {
        Ctx {
            slot: Slot::Floating,
            states: no_state(),
            focus_key: String::new(),
            hovered_key: None,
            marker_gutter: false,
            hovered_item_key: String::new(),
            avail_height: None,
        }
    }

    /// What the runtime says this spec renders as: one string per row, with
    /// the trailing newlines its entries carry stripped.
    ///
    /// This is the oracle. It is the implementation still, which is what makes
    /// it worth asserting against: a variant cannot be migrated wrongly here
    /// without the two disagreeing.
    fn runtime_rows(spec: &WidgetSpec) -> Vec<String> {
        let out = crate::widgets::render_spec(spec, &Default::default(), "", WIDTH as u32);
        out.entries
            .iter()
            .map(|e| {
                let mut n = e.clone();
                n.normalize_widths();
                n.text.trim_end_matches('\n').to_string()
            })
            .collect()
    }

    /// What the tree says, laid out at the same width: the text of each row of
    /// the display list, in paint order.
    fn tree_rows(spec: &WidgetSpec) -> Vec<String> {
        tree_text(spec, &cx())
    }

    fn hint(keys: &str, label: &str) -> HintEntry {
        HintEntry {
            keys: keys.into(),
            label: label.into(),
        }
    }

    fn raw(text: &str) -> TextPropertyEntry {
        TextPropertyEntry::text(text)
    }

    fn col_of(children: Vec<WidgetSpec>) -> WidgetSpec {
        WidgetSpec::Col {
            children,
            key: None,
        }
    }

    /// Every covered variant, in the shapes the runtime branches on, asserted
    /// against the runtime itself.
    #[test]
    fn the_covered_variants_render_what_the_runtime_renders() {
        let cases: Vec<(&str, WidgetSpec)> = vec![
            (
                "one raw row",
                col_of(vec![WidgetSpec::Raw {
                    entries: vec![raw("hello")],
                    key: None,
                }]),
            ),
            (
                "several raw rows",
                col_of(vec![WidgetSpec::Raw {
                    entries: vec![raw("one"), raw("two"), raw("three")],
                    key: None,
                }]),
            ),
            (
                "an empty raw",
                col_of(vec![WidgetSpec::Raw {
                    entries: vec![],
                    key: None,
                }]),
            ),
            (
                "a hint bar",
                col_of(vec![WidgetSpec::HintBar {
                    entries: vec![hint("Tab", "next"), hint("Esc", "cancel")],
                    key: None,
                }]),
            ),
            (
                "a hint bar with one entry",
                col_of(vec![WidgetSpec::HintBar {
                    entries: vec![hint("Enter", "submit")],
                    key: None,
                }]),
            ),
            (
                "a default divider",
                col_of(vec![WidgetSpec::Divider {
                    ch: "─".into(),
                    style: None,
                    key: None,
                }]),
            ),
            (
                "a divider with another glyph",
                col_of(vec![WidgetSpec::Divider {
                    ch: "=".into(),
                    style: None,
                    key: None,
                }]),
            ),
            (
                "rows and dividers together",
                col_of(vec![
                    WidgetSpec::Raw {
                        entries: vec![raw("above")],
                        key: None,
                    },
                    WidgetSpec::Divider {
                        ch: "─".into(),
                        style: None,
                        key: None,
                    },
                    WidgetSpec::Raw {
                        entries: vec![raw("below")],
                        key: None,
                    },
                ]),
            ),
        ];
        for (label, spec) in cases {
            assert!(covered(&spec), "{label} should be covered");
            assert_eq!(tree_rows(&spec), runtime_rows(&spec), "{label}");
        }
    }

    /// **The coverage gate is the point of `covered`.** A panel is described
    /// or painted, never half of each, so one unmigrated child takes the whole
    /// spec down the old path.
    #[test]
    fn one_uncovered_child_makes_the_whole_spec_uncovered() {
        let covered_leaf = WidgetSpec::Raw {
            entries: vec![raw("x")],
            key: None,
        };
        assert!(covered(&covered_leaf));

        // Any variant this module has not reached yet. `WindowEmbed` is the
        // one that never will — it is a `Host` leaf by G's rule — so it stays
        // a valid example of "not described here" for the life of C.1.
        let uncovered = WidgetSpec::WindowEmbed {
            window_id: 1,
            rows: 3,
            key: None,
        };
        assert!(!covered(&uncovered));
        assert!(
            !covered(&col_of(vec![covered_leaf, uncovered])),
            "a column with one unmigrated child is not covered"
        );
    }

    /// The tree's rows for a spec under a context.
    ///
    /// **Grouped by row, because a styled row is many items.** A `text_runs`
    /// node emits one display item per run — that is how a run carries its own
    /// theme — so a toggle whose chip is styled differently from its label
    /// arrives as two items on one line. The runtime's unit is the entry,
    /// which is a line, so the comparison has to be made at that unit.
    fn tree_text(spec: &WidgetSpec, c: &Ctx<'_>) -> Vec<String> {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(node(spec, WIDTH, c), Size::new(WIDTH, 24));
        rows_of(&ui)
    }

    /// Every line the display list paints, left to right, top to bottom.
    fn rows_of(ui: &Ui<UiMsg>) -> Vec<String> {
        let mut pieces: Vec<(i32, i32, String)> = Vec::new();
        for item in ui.spec().in_flow() {
            if let fresh_ui::Draw::Lines(lines) = &item.draw {
                for (i, l) in lines.iter().enumerate() {
                    pieces.push((item.rect.y + i as i32, item.rect.x, l.to_string()));
                }
            }
        }
        pieces.sort_by_key(|(y, x, _)| (*y, *x));
        let mut out: Vec<String> = Vec::new();
        let mut at: Option<i32> = None;
        for (y, _, s) in pieces {
            match at {
                Some(prev) if prev == y => out.last_mut().unwrap().push_str(&s),
                _ => {
                    out.push(s);
                    at = Some(y);
                }
            }
        }
        out
    }

    /// The runtime's, under the same context.
    fn runtime_text(spec: &WidgetSpec, c: &Ctx<'_>) -> Vec<String> {
        crate::widgets::render_spec_with_options(
            spec,
            &Default::default(),
            WIDTH as u32,
            crate::widgets::RenderOptions {
                prev_focus_key: &c.focus_key,
                auto_focus_first: false,
                marker_gutter: c.marker_gutter,
                hover_key: c.hovered_key.as_deref().unwrap_or(""),
                ..Default::default()
            },
        )
        .entries
        .iter()
        .map(|e| {
            let mut n = e.clone();
            n.normalize_widths();
            n.text.trim_end_matches('\n').to_string()
        })
        .collect()
    }

    fn button(label: &str, key: Option<&str>, disabled: bool, bare: bool) -> WidgetSpec {
        WidgetSpec::Button {
            label: label.into(),
            focused: false,
            intent: Default::default(),
            key: key.map(|k| k.into()),
            disabled,
            focusable: true,
            bare,
            full_width: false,
            hover_style: None,
        }
    }

    /// A button says what the runtime says it says — framed, bare, disabled,
    /// focused, and stretched. The label is `render_button`'s to decide and
    /// stays that way; only the hit moved.
    #[test]
    fn a_button_renders_what_the_runtime_renders() {
        let cases: Vec<(&str, WidgetSpec, Ctx<'static>)> = vec![
            ("framed", button("Go", Some("go"), false, false), cx()),
            ("bare", button("×", Some("x"), false, true), cx()),
            ("disabled", button("Go", Some("go"), true, false), cx()),
            ("keyless", button("Go", None, false, false), cx()),
            (
                "focused",
                button("Go", Some("go"), false, false),
                Ctx {
                    focus_key: "go".into(),
                    ..cx()
                },
            ),
            (
                "hovered",
                button("Go", Some("go"), false, false),
                Ctx {
                    hovered_key: Some("go".into()),
                    ..cx()
                },
            ),
            (
                "with the marker gutter",
                button("Go", Some("go"), false, false),
                Ctx {
                    marker_gutter: true,
                    ..cx()
                },
            ),
            (
                "full width",
                {
                    let mut b = button("Go", Some("go"), false, false);
                    if let WidgetSpec::Button { full_width, .. } = &mut b {
                        *full_width = true;
                    }
                    b
                },
                cx(),
            ),
        ];
        for (label, spec, c) in cases {
            assert!(covered(&spec));
            assert_eq!(tree_text(&spec, &c), runtime_text(&spec, &c), "{label}");
        }
    }

    /// **The seam.** A press delivers the widget's own hit — the same
    /// `HitArea` the runtime recorded — so `deliver_widget_hit` behind it does
    /// not change. What changed is that the tree found the widget.
    #[test]
    fn pressing_a_button_delivers_the_hit_the_runtime_recorded() {
        let spec = button("Go", Some("go"), false, false);
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(node(&spec, WIDTH, &cx()), Size::new(WIDTH, 24));
        let got: Vec<UiFact> = ui
            .dispatch(fresh_ui::Input::press(
                fresh_ui::Point::new(1, 0),
                fresh_ui::MouseButton::Left,
                fresh_ui::Mods::NONE,
            ))
            .msgs
            .into_iter()
            .filter_map(|m| match m {
                UiMsg::Ui(f) => Some(f),
                _ => None,
            })
            .collect();
        let UiFact::WidgetHit { slot, hit } = got.first().expect("a hit") else {
            panic!("expected a widget hit, got {got:?}");
        };
        assert_eq!(*slot, Slot::Floating);
        assert_eq!(hit.widget_key, "go");
        assert_eq!(hit.widget_kind, "button");
        assert_eq!(hit.event_type, "activate");
    }

    /// A disabled button has no hit at all — the runtime excludes it from the
    /// tab cycle, so a click that focused and activated it would be acting on
    /// a stale focus. The node simply is not a gesture.
    #[test]
    fn a_disabled_button_answers_no_press() {
        let spec = button("Go", Some("go"), true, false);
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(node(&spec, WIDTH, &cx()), Size::new(WIDTH, 24));
        let got = ui.dispatch(fresh_ui::Input::press(
            fresh_ui::Point::new(1, 0),
            fresh_ui::MouseButton::Left,
            fresh_ui::Mods::NONE,
        ));
        assert!(
            !got.msgs
                .iter()
                .any(|m| matches!(m, UiMsg::Ui(UiFact::WidgetHit { .. }))),
            "a disabled button is inert"
        );
    }

    fn toggle(label: &str, checked: bool, label_first: bool) -> WidgetSpec {
        WidgetSpec::Toggle {
            checked,
            label: label.into(),
            focused: false,
            indeterminate: false,
            label_first,
            label_width: 0,
            key: Some("t".into()),
        }
    }

    /// A toggle says what the runtime says it says, in both layouts and both
    /// states, focused and hovered.
    #[test]
    fn a_toggle_renders_what_the_runtime_renders() {
        let mut cases: Vec<(String, WidgetSpec, Ctx<'static>)> = Vec::new();
        for label_first in [false, true] {
            for checked in [false, true] {
                cases.push((
                    format!("label_first={label_first} checked={checked}"),
                    toggle("wrap", checked, label_first),
                    cx(),
                ));
            }
        }
        cases.push((
            "focused".into(),
            toggle("wrap", false, false),
            Ctx {
                focus_key: "t".into(),
                ..cx()
            },
        ));
        cases.push((
            "hovered".into(),
            toggle("wrap", false, false),
            Ctx {
                hovered_key: Some("t".into()),
                ..cx()
            },
        ));
        cases.push((
            "indeterminate".into(),
            {
                let mut t = toggle("wrap", false, true);
                if let WidgetSpec::Toggle { indeterminate, .. } = &mut t {
                    *indeterminate = true;
                }
                t
            },
            cx(),
        ));
        for (label, spec, c) in cases {
            assert!(covered(&spec));
            assert_eq!(tree_text(&spec, &c), runtime_text(&spec, &c), "{label}");
        }
    }

    /// **The chip, and only the chip.** In form layout a click on the label
    /// must not flip the value — the settings dialog's contract, which the
    /// runtime kept as a byte range and this keeps as where the nodes are.
    #[test]
    fn a_form_toggle_answers_on_its_chip_and_not_on_its_label() {
        let spec = toggle("wrap", false, true);
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(node(&spec, WIDTH, &cx()), Size::new(WIDTH, 24));
        let hit_at = |ui: &mut Ui<UiMsg>, x: i32| -> Option<UiFact> {
            ui.dispatch(fresh_ui::Input::press(
                fresh_ui::Point::new(x, 0),
                fresh_ui::MouseButton::Left,
                fresh_ui::Mods::NONE,
            ))
            .msgs
            .into_iter()
            .find_map(|m| match m {
                UiMsg::Ui(f @ UiFact::WidgetHit { .. }) => Some(f),
                _ => None,
            })
        };
        // The label is first, so column 0 is on it.
        assert!(
            hit_at(&mut ui, 0).is_none(),
            "a press on the label does not flip the value"
        );
        // The chip is at the end of the row; find its column from the runtime's
        // own byte range rather than guessing.
        let out = crate::widgets::render_spec_with_options(
            &spec,
            &Default::default(),
            WIDTH as u32,
            crate::widgets::RenderOptions {
                prev_focus_key: "",
                auto_focus_first: false,
                ..Default::default()
            },
        );
        let h = out.hits.first().expect("a hit");
        let chip_col = out.entries[0].text[..h.byte_start].chars().count() as i32;
        let got = hit_at(&mut ui, chip_col).expect("a press on the chip is the toggle's");
        let UiFact::WidgetHit { hit, .. } = got else {
            unreachable!()
        };
        assert_eq!(hit.widget_kind, "toggle");
        assert_eq!(hit.event_type, "toggle");
    }

    /// A number field says what the runtime says, in every shape its
    /// formatter branches on — integer, percent, clamped, labelled, focused,
    /// and mid-edit.
    #[test]
    fn a_number_renders_what_the_runtime_renders() {
        let base = |integer: bool, percent: bool| WidgetSpec::Number {
            value: 42.0,
            min: Some(0.0),
            max: Some(100.0),
            step: 1.0,
            integer,
            percent,
            label: "size".into(),
            focused: false,
            label_width: 8,
            edit_text: None,
            edit_cursor: -1,
            edit_sel_start: -1,
            edit_sel_end: -1,
            key: Some("n".into()),
        };
        let mut cases: Vec<(String, WidgetSpec, Ctx<'static>)> = vec![
            ("integer".into(), base(true, false), cx()),
            ("float".into(), base(false, false), cx()),
            ("percent".into(), base(false, true), cx()),
            (
                "focused".into(),
                base(true, false),
                Ctx {
                    focus_key: "n".into(),
                    ..cx()
                },
            ),
        ];
        // Above the max: the runtime clamps, and so must this.
        let mut over = base(true, false);
        if let WidgetSpec::Number { value, .. } = &mut over {
            *value = 999.0;
        }
        cases.push(("clamped".into(), over, cx()));
        // Mid-edit: the buffer being typed replaces the value cell.
        let mut editing = base(true, false);
        if let WidgetSpec::Number {
            edit_text,
            edit_cursor,
            ..
        } = &mut editing
        {
            *edit_text = Some("7".into());
            *edit_cursor = 1;
        }
        cases.push(("editing".into(), editing, cx()));

        for (label, spec, c) in cases {
            assert!(covered(&spec));
            assert_eq!(tree_text(&spec, &c), runtime_text(&spec, &c), "{label}");
        }
    }

    /// **The value cell, and only the value cell.** "A click on the value cell
    /// begins in-place editing"; a click on the label does not.
    #[test]
    fn a_number_answers_on_its_value_and_not_on_its_label() {
        let spec = WidgetSpec::Number {
            value: 42.0,
            min: None,
            max: None,
            step: 1.0,
            integer: true,
            percent: false,
            label: "size".into(),
            focused: false,
            label_width: 8,
            edit_text: None,
            edit_cursor: -1,
            edit_sel_start: -1,
            edit_sel_end: -1,
            key: Some("n".into()),
        };
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(node(&spec, WIDTH, &cx()), Size::new(WIDTH, 24));
        let press = |ui: &mut Ui<UiMsg>, x: i32| {
            ui.dispatch(fresh_ui::Input::press(
                fresh_ui::Point::new(x, 0),
                fresh_ui::MouseButton::Left,
                fresh_ui::Mods::NONE,
            ))
            .msgs
            .into_iter()
            .find_map(|m| match m {
                UiMsg::Ui(f @ UiFact::WidgetHit { .. }) => Some(f),
                _ => None,
            })
        };
        assert!(press(&mut ui, 0).is_none(), "the label is not the value");
        let out = crate::widgets::render_spec_with_options(
            &spec,
            &Default::default(),
            WIDTH as u32,
            crate::widgets::RenderOptions {
                prev_focus_key: "",
                auto_focus_first: false,
                ..Default::default()
            },
        );
        let h = out.hits.first().expect("a hit");
        let col = out.entries[0].text[..h.byte_start].chars().count() as i32;
        let UiFact::WidgetHit { hit, .. } = press(&mut ui, col).expect("the value cell") else {
            unreachable!()
        };
        assert_eq!(hit.widget_kind, "number");
        assert_eq!(hit.event_type, "number_value");
    }

    /// **`Component` is a focus scope with a key, and nothing else.** It
    /// renders its child transparently — no chrome, no rows of its own — so
    /// the rows are exactly the child's.
    #[test]
    fn a_component_is_transparent_and_adds_no_rows() {
        let inner = WidgetSpec::Raw {
            entries: vec![raw("one"), raw("two")],
            key: None,
        };
        let wrapped = WidgetSpec::Component {
            child: Box::new(inner.clone()),
            key: Some("picker".into()),
        };
        assert!(covered(&wrapped));
        assert_eq!(tree_text(&wrapped, &cx()), tree_text(&inner, &cx()));
    }

    /// **An overlay consumes no vertical space.** "The rows below it do not
    /// shift down" — so a column containing one lays out as though it were not
    /// there, and the floated rows are placed over what follows.
    #[test]
    fn an_overlay_does_not_push_the_rows_below_it_down() {
        let plain = col_of(vec![
            WidgetSpec::Raw {
                entries: vec![raw("first")],
                key: None,
            },
            WidgetSpec::Raw {
                entries: vec![raw("second")],
                key: None,
            },
        ]);
        let floated = col_of(vec![
            WidgetSpec::Raw {
                entries: vec![raw("first")],
                key: None,
            },
            WidgetSpec::Overlay {
                child: Box::new(WidgetSpec::Raw {
                    entries: vec![raw("hint")],
                    key: None,
                }),
                key: None,
            },
            WidgetSpec::Raw {
                entries: vec![raw("second")],
                key: None,
            },
        ]);
        assert!(covered(&floated));
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(node(&floated, WIDTH, &cx()), Size::new(WIDTH, 24));
        let row_of = |ui: &Ui<UiMsg>, text: &str| -> i32 {
            ui.spec()
                .in_flow()
                .iter()
                .chain(ui.spec().layers().iter())
                .find_map(|i| match &i.draw {
                    fresh_ui::Draw::Lines(l) if l.iter().any(|s| &**s == text) => Some(i.rect.y),
                    _ => None,
                })
                .unwrap_or(-1)
        };
        assert_eq!(row_of(&ui, "second"), 1, "the row below did not shift");
        let _ = plain;
    }

    /// **The five that go through their own collectors.** Their rows are the
    /// runtime's — reproducing seven thousand lines of formatting by hand
    /// would be rewriting it to get the same cells — so what this asserts is
    /// that routing them through the adapter changes none of them.
    #[test]
    fn the_collected_variants_render_what_the_runtime_renders() {
        let cases: Vec<(&str, WidgetSpec)> = vec![(
            "a text field",
            WidgetSpec::Text {
                value: "hello".into(),
                cursor_byte: -1,
                focused: false,
                label: "name".into(),
                placeholder: None,
                rows: 1,
                field_width: 12,
                max_visible_chars: 0,
                full_width: false,
                completions: Vec::new(),
                completions_visible_rows: 0,
                block_caret: false,
                sel_start: -1,
                sel_end: -1,
                label_width: 0,
                read_only: false,
                markdown: false,
                key: Some("t".into()),
            },
        )];
        for (label, spec) in cases {
            assert!(covered(&spec), "{label} should be covered");
            assert_eq!(
                tree_text(&spec, &cx()),
                runtime_text(&spec, &cx()),
                "{label}"
            );
        }
    }

    /// **The boundary is closed.** Every variant but `WindowEmbed` is
    /// described.
    ///
    /// It was the *scrollbar*, never the scroll offset: a kind whose painter
    /// drew a bar from a recorded offset could not be described without
    /// silently losing it. Each of the five that were behind it turned out to
    /// be on the near side for its own reason — a dual list does not scroll, a
    /// dropdown's pop-over paints no bar, a plain list and a plain tree are
    /// `widgets::List`, a card list is that with a taller row — and the last
    /// two, a card tree and a multi-line field, crossed by giving the window
    /// to a `viewport`, which is what owning the scroll means.
    ///
    /// `WindowEmbed` is a `Host` leaf by G's rule and never crosses.
    #[test]
    fn every_variant_but_the_host_leaf_is_covered() {
        let plain_list = WidgetSpec::List {
            items: vec![raw("one")],
            item_specs: Vec::new(),
            item_keys: vec!["a".into()],
            selected_index: 0,
            visible_rows: Some(1),
            key: Some("l".into()),
            focusable: true,
        };
        let multiline = |rows: u32| WidgetSpec::Text {
            value: "a\nb\nc".into(),
            cursor_byte: -1,
            focused: false,
            label: String::new(),
            placeholder: None,
            rows,
            field_width: 10,
            max_visible_chars: 0,
            full_width: false,
            completions: Vec::new(),
            completions_visible_rows: 0,
            block_caret: false,
            sel_start: -1,
            sel_end: -1,
            label_width: 0,
            read_only: false,
            markdown: false,
            key: Some("t2".into()),
        };
        for (what, spec) in [
            ("a plain list", plain_list),
            ("a card list", card_list(3, 0, 9)),
            ("an open dropdown", dropdown(&["fast", "slow"], 0, true, 0)),
            ("a single-line field", multiline(1)),
            ("a multi-line field", multiline(6)),
            ("a card tree", card_tree(3, 0, 15)),
        ] {
            assert!(covered(&spec), "{what}");
        }

        // And the one that does not, which takes its panel with it.
        let embed = WidgetSpec::WindowEmbed {
            window_id: 1,
            rows: 3,
            key: None,
        };
        assert!(!covered(&embed), "a window embed is cells");
        assert!(
            !covered(&col_of(vec![
                WidgetSpec::Raw {
                    entries: vec![raw("x")],
                    key: None
                },
                embed
            ])),
            "one uncovered node takes its panel with it"
        );
    }

    fn a_list(n: usize, selected: i32, visible: u32) -> WidgetSpec {
        WidgetSpec::List {
            items: (0..n).map(|i| raw(&format!("row{i}"))).collect(),
            item_specs: Vec::new(),
            item_keys: (0..n).map(|i| format!("k{i}")).collect(),
            selected_index: selected,
            visible_rows: Some(visible),
            key: Some("l".into()),
            focusable: true,
        }
    }

    /// **The bar comes free, and that is the whole reason `List` crossed.**
    /// The viewport owns the window, so an overflowing list gets a scrollbar
    /// from the library rather than from a painter reading a recorded offset.
    #[test]
    fn an_overflowing_list_has_a_scrollbar() {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(node(&a_list(50, 0, 5), WIDTH, &cx()), Size::new(WIDTH, 24));
        assert!(
            ui.spec()
                .in_flow()
                .iter()
                .any(|i| matches!(i.draw, fresh_ui::Draw::Scrollbar { .. })),
            "fifty rows in five do not fit"
        );

        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(node(&a_list(3, 0, 5), WIDTH, &cx()), Size::new(WIDTH, 24));
        assert!(
            !ui.spec()
                .in_flow()
                .iter()
                .any(|i| matches!(i.draw, fresh_ui::Draw::Scrollbar { .. })),
            "three rows in five do"
        );
    }

    /// The window follows the selection without anyone clamping it by hand:
    /// the list's own `Anchor` reveals a selection that moved, "the owner
    /// passing a new one down" included, which is what the runtime's
    /// auto-clamp did.
    #[test]
    fn the_window_follows_a_selection_it_is_given() {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(node(&a_list(50, 0, 5), WIDTH, &cx()), Size::new(WIDTH, 24));
        ui.frame(node(&a_list(50, 40, 5), WIDTH, &cx()), Size::new(WIDTH, 24));
        let shown = tree_text(&a_list(50, 40, 5), &cx());
        // Row 40 is in the window, and row 0 is not.
        assert!(
            shown.iter().any(|r| r.contains("row40")),
            "the selection was revealed, got {shown:?}"
        );
    }

    /// A row press carries the hit `deliver_widget_hit` expects, naming the
    /// List as its owner — so focus moves to the list and the arrows after a
    /// row click keep driving its selection.
    #[test]
    fn a_list_row_press_names_the_list_that_owns_it() {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(node(&a_list(4, 0, 4), WIDTH, &cx()), Size::new(WIDTH, 24));
        let at = fresh_ui::Point::new(1, 2);
        let mut msgs = ui
            .dispatch(fresh_ui::Input::press(
                at,
                fresh_ui::MouseButton::Left,
                fresh_ui::Mods::NONE,
            ))
            .msgs;
        msgs.extend(
            ui.dispatch(fresh_ui::Input::release(
                at,
                fresh_ui::MouseButton::Left,
                fresh_ui::Mods::NONE,
            ))
            .msgs,
        );
        let hit = msgs
            .into_iter()
            .find_map(|m| match m {
                UiMsg::Ui(UiFact::WidgetHit { hit, .. }) => Some(hit),
                _ => None,
            })
            .expect("a row press");
        assert_eq!(hit.widget_kind, "list");
        assert_eq!(hit.event_type, "select");
        assert_eq!(hit.owner_key.as_deref(), Some("l"));
        assert_eq!(hit.payload["index"], 2);
        assert_eq!(hit.payload["key"], "k2");
    }

    fn tree_node(text: &str, depth: u32, has_children: bool) -> fresh_core::api::TreeNode {
        fresh_core::api::TreeNode {
            text: raw(text),
            depth,
            has_children,
            checked: None,
            extra_lines: Vec::new(),
        }
    }

    fn a_tree(expanded: &[&str], selected: i32) -> WidgetSpec {
        WidgetSpec::Tree {
            nodes: vec![
                tree_node("root", 0, true),
                tree_node("child", 1, false),
                tree_node("sibling", 0, false),
            ],
            item_keys: vec!["r".into(), "c".into(), "s".into()],
            selected_index: selected,
            visible_rows: Some(5),
            key: Some("tr".into()),
            expanded_keys: expanded.iter().map(|s| s.to_string()).collect(),
            checkable: false,
            item_height: 1,
            card_borders: false,
            indent_cols: 2,
        }
    }

    /// **Expansion stays the plugin's.** A collapsed root hides its child; an
    /// expanded one shows it. Nothing in the tree owns that — `expanded_keys`
    /// arrives in the spec, and `collect_visible_tree_indices` is the same
    /// rule the runtime applies, reused rather than restated.
    #[test]
    fn a_trees_visible_rows_are_the_plugins_expansion() {
        let collapsed = tree_text(&a_tree(&[], -1), &cx());
        assert!(
            !collapsed.iter().any(|r| r.contains("child")),
            "a collapsed root hides its child, got {collapsed:?}"
        );
        assert!(collapsed.iter().any(|r| r.contains("sibling")));

        let open = tree_text(&a_tree(&["r"], -1), &cx());
        assert!(
            open.iter().any(|r| r.contains("child")),
            "an expanded one shows it, got {open:?}"
        );
    }

    /// A tree row's three targets, over the ranges the runtime named: the
    /// disclosure glyph expands, the rest selects.
    #[test]
    fn a_tree_rows_glyph_expands_and_its_label_selects() {
        let spec = a_tree(&[], -1);
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(node(&spec, WIDTH, &cx()), Size::new(WIDTH, 24));
        let at = |ui: &mut Ui<UiMsg>, x: i32| -> Option<(&'static str, serde_json::Value)> {
            ui.dispatch(fresh_ui::Input::press(
                fresh_ui::Point::new(x, 0),
                fresh_ui::MouseButton::Left,
                fresh_ui::Mods::NONE,
            ))
            .msgs
            .into_iter()
            .find_map(|m| match m {
                UiMsg::Ui(UiFact::WidgetHit { hit, .. }) => {
                    Some((hit.event_type, hit.payload.clone()))
                }
                _ => None,
            })
        };
        // The runtime's own answer for where the glyph is.
        let r = crate::widgets::render_tree_row(
            &tree_node("root", 0, true),
            false,
            false,
            1,
            false,
            WIDTH as u32,
            2,
        );
        let (gs, _) = r.disclosure_range.expect("a branch has a glyph");
        let col = r.entry.text[..gs].chars().count() as i32;
        let (kind, payload) = at(&mut ui, col).expect("the glyph");
        assert_eq!(kind, "expand");
        assert_eq!(payload["key"], "r");
        assert_eq!(payload["expanded"], true, "pressing it opens the node");

        let end = r.entry.text.chars().count() as i32;
        let (kind, payload) = at(&mut ui, end - 1).expect("the label");
        assert_eq!(kind, "select");
        assert_eq!(payload["index"], 0);
    }

    /// The selection is an index into the *whole* array, and the list's is
    /// into the visible window — the same array with the collapsed subtrees
    /// taken out. Selecting `sibling` (absolute 2) must land on it whether or
    /// not the root is open.
    #[test]
    fn a_trees_selection_survives_the_collapse_of_what_is_above_it() {
        for (expanded, label) in [(&[][..], "collapsed"), (&["r"][..], "expanded")] {
            let shown = tree_text(&a_tree(expanded, 2), &cx());
            assert!(
                shown.iter().any(|r| r.contains("sibling")),
                "{label}: the selected node is on screen, got {shown:?}"
            );
        }
    }

    fn a_dual_list() -> WidgetSpec {
        WidgetSpec::DualList {
            options: vec![
                fresh_core::api::DualListOption {
                    value: "a".into(),
                    label: "Alpha".into(),
                },
                fresh_core::api::DualListOption {
                    value: "b".into(),
                    label: "Beta".into(),
                },
            ],
            included: vec!["b".into()],
            excluded: Vec::new(),
            label: "cols".into(),
            focused: false,
            active_included: false,
            available_cursor: 0,
            included_cursor: 0,
            hint: String::new(),
            visible_rows: 3,
            key: Some("dl".into()),
        }
    }

    /// A dual list renders what the runtime renders, through the adapter and
    /// with no substitution: it emits every row, so there is no window and no
    /// bar to lose.
    #[test]
    fn a_dual_list_renders_what_the_runtime_renders() {
        let spec = a_dual_list();
        assert!(covered(&spec));
        assert_eq!(tree_text(&spec, &cx()), runtime_text(&spec, &cx()));
    }

    /// **Both columns answer.** Each row is two cells with a `dual_focus` hit
    /// apiece over its own byte range — the case that needed a row to stop
    /// being one target, and the one where keeping only the first hit would
    /// have left the right-hand column dead.
    #[test]
    fn both_columns_of_a_dual_list_answer_a_press() {
        let spec = a_dual_list();
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(node(&spec, WIDTH, &cx()), Size::new(WIDTH, 24));
        let out = crate::widgets::render_spec_with_options(
            &spec,
            &Default::default(),
            WIDTH as u32,
            crate::widgets::RenderOptions {
                prev_focus_key: "",
                auto_focus_first: false,
                ..Default::default()
            },
        );
        let mut seen: Vec<String> = Vec::new();
        for h in out.hits.iter().filter(|h| h.event_type == "dual_focus") {
            let text = &out.entries[h.buffer_row as usize].text;
            let col = text[..h.byte_start].chars().count() as i32;
            let got = ui
                .dispatch(fresh_ui::Input::press(
                    fresh_ui::Point::new(col, h.buffer_row as i32),
                    fresh_ui::MouseButton::Left,
                    fresh_ui::Mods::NONE,
                ))
                .msgs
                .into_iter()
                .find_map(|m| match m {
                    UiMsg::Ui(UiFact::WidgetHit { hit, .. }) => {
                        Some(hit.payload["column"].as_str().unwrap_or("").to_string())
                    }
                    _ => None,
                });
            if let Some(c) = got {
                seen.push(c);
            }
        }
        assert!(
            seen.iter().any(|c| c == "available") && seen.iter().any(|c| c == "included"),
            "both columns answered, got {seen:?}"
        );
    }

    /// **A row is not one target.** A tree row has three — the disclosure
    /// glyph expands, the checkbox toggles, the rest selects — and the runtime
    /// told them apart by comparing a clicked byte against three ranges. Three
    /// rectangles give the same three answers, and keeping only the first
    /// would make the other two unclickable without failing anything.
    #[test]
    fn a_row_with_several_hits_answers_each_over_its_own_bytes() {
        let e = raw("[v] > label");
        let hit = |kind: &'static str, a: usize, b: usize| crate::widgets::HitArea {
            row_target: false,
            context_click: false,
            overlay: false,
            widget_key: "t".into(),
            widget_kind: "tree",
            buffer_row: 0,
            byte_start: a,
            byte_end: b,
            payload: serde_json::json!({}),
            event_type: kind,
            owner_key: None,
        };
        let node = entry_row_hits(
            &e,
            Slot::Floating,
            &[
                ((0, 3), hit("toggle", 0, 3)),
                ((4, 5), hit("expand", 4, 5)),
                ((6, 11), hit("select", 6, 11)),
            ],
        );
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(node, Size::new(WIDTH, 4));
        let at = |ui: &mut Ui<UiMsg>, x: i32| -> Option<&'static str> {
            ui.dispatch(fresh_ui::Input::press(
                fresh_ui::Point::new(x, 0),
                fresh_ui::MouseButton::Left,
                fresh_ui::Mods::NONE,
            ))
            .msgs
            .into_iter()
            .find_map(|m| match m {
                UiMsg::Ui(UiFact::WidgetHit { hit, .. }) => Some(hit.event_type),
                _ => None,
            })
        };
        assert_eq!(at(&mut ui, 1), Some("toggle"), "the checkbox");
        assert_eq!(at(&mut ui, 4), Some("expand"), "the disclosure glyph");
        assert_eq!(at(&mut ui, 8), Some("select"), "the label");
    }

    /// **The variant whose parity is geometric, not textual.** The runtime
    /// draws this frame as text — `╭─ label ─…─╮` in an entry, `│ … │` around
    /// every child row — because entries are all it has. The tree has a
    /// border and uses it, so the cells differ on purpose.
    ///
    /// What must not differ is the content rectangle, because everything
    /// downstream is addressed against it: the runtime gives the child
    /// `panel_width - 4` and then shifts six recorded channels by one row and
    /// the `│ ` prefix. Layout says the same thing once, and this is the
    /// assertion that it says the same thing.
    #[test]
    fn a_labeled_section_gives_its_child_the_rectangle_the_runtime_gave_it() {
        let inner_key = fresh_ui::Key::Str("ls_child".into());
        for label in ["", "Options"] {
            let spec = WidgetSpec::LabeledSection {
                label: label.into(),
                child: Box::new(WidgetSpec::Raw {
                    entries: vec![raw("body")],
                    key: None,
                }),
                width_pct: None,
                key: None,
            };
            assert!(covered(&spec));
            let mut ui: Ui<UiMsg> = Ui::new();
            ui.frame(
                node(&spec, WIDTH, &cx()).key(fresh_ui::Key::Str("ls".into())),
                Size::new(WIDTH, 24),
            );
            // The child is the only text the section contains besides the
            // legend, so find it by content rather than by index — the strip
            // changes the shape of the tree when a label is present.
            let body = ui
                .spec()
                .in_flow()
                .iter()
                .find_map(|i| match &i.draw {
                    fresh_ui::Draw::Lines(l) if l.iter().any(|s| &**s == "body") => Some(i.rect),
                    _ => None,
                })
                .expect("the child's row");
            assert_eq!(
                (body.x, body.y),
                (2, 1),
                "a column of ring and a column of padding, label={label:?}"
            );
            let _ = &inner_key;
        }
    }

    /// The child is laid out at `panel_width - 4`, which is `inner_width` —
    /// the number the runtime hands down before it starts shifting channels.
    #[test]
    fn a_labeled_sections_child_is_four_columns_narrower_than_the_panel() {
        let spec = WidgetSpec::LabeledSection {
            label: "L".into(),
            child: Box::new(WidgetSpec::Divider {
                ch: "─".into(),
                style: None,
                key: None,
            }),
            width_pct: None,
            key: None,
        };
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(node(&spec, WIDTH, &cx()), Size::new(WIDTH, 24));
        // A divider is as wide as the width it was given, so its glyph count
        // reports that width back.
        let rule = ui
            .spec()
            .in_flow()
            .iter()
            .find_map(|i| match &i.draw {
                fresh_ui::Draw::Lines(l) if l.iter().any(|s| s.starts_with('─')) => {
                    Some(l[0].chars().count())
                }
                _ => None,
            })
            .expect("the rule");
        assert_eq!(rule, (WIDTH - 4) as usize);
    }

    /// An entry's inline overlays become runs, split at the overlay
    /// boundaries and merged in declaration order — the walk the painter does,
    /// with the theme *names* kept so the fold resolves them.
    #[test]
    fn inline_overlays_become_runs_at_their_boundaries() {
        use fresh_core::text_property::InlineOverlay;
        let mut e = raw("abcdef");
        e.inline_overlays = vec![InlineOverlay {
            start: 2,
            end: 4,
            style: OverlayOptions {
                bold: true,
                ..Default::default()
            },
            properties: Default::default(),
            unit: Default::default(),
        }];
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(entry_row(&e), Size::new(WIDTH, 4));
        let texts: Vec<String> = ui
            .spec()
            .in_flow()
            .iter()
            .filter_map(|i| match &i.draw {
                fresh_ui::Draw::Lines(l) => {
                    Some(l.iter().map(|s| s.to_string()).collect::<Vec<_>>())
                }
                _ => None,
            })
            .flatten()
            .collect();
        assert_eq!(
            texts,
            vec!["ab".to_string(), "cd".to_string(), "ef".to_string()],
            "three runs, split where the overlay starts and ends"
        );
    }

    fn dropdown(options: &[&str], selected: i32, open: bool, scroll: u32) -> WidgetSpec {
        WidgetSpec::Dropdown {
            options: options.iter().map(|o| (*o).into()).collect(),
            selected_index: selected,
            label: "Mode".into(),
            focused: false,
            label_width: 0,
            open,
            scroll_offset: scroll,
            key: Some("mode".into()),
        }
    }

    /// The runtime's own pop-over for a spec: the rows it windowed and the
    /// absolute index each of them selects.
    fn runtime_popup(spec: &WidgetSpec) -> crate::widgets::PanelPopup {
        crate::widgets::render_spec(spec, &Default::default(), "", WIDTH as u32)
            .popup
            .expect("an open dropdown has a pop-over")
    }

    /// Every line the layers paint, grouped by row the way [`rows_of`] groups
    /// the in-flow half.
    fn layer_rows(ui: &Ui<UiMsg>) -> Vec<String> {
        let mut pieces: Vec<(i32, i32, String)> = Vec::new();
        for item in ui.spec().layers() {
            if let fresh_ui::Draw::Lines(lines) = &item.draw {
                for (i, l) in lines.iter().enumerate() {
                    pieces.push((item.rect.y + i as i32, item.rect.x, l.to_string()));
                }
            }
        }
        pieces.sort_by_key(|(y, x, _)| (*y, *x));
        let mut out: Vec<String> = Vec::new();
        let mut at: Option<i32> = None;
        for (y, _, s) in pieces {
            match at {
                Some(prev) if prev == y => out.last_mut().unwrap().push_str(&s),
                _ => {
                    out.push(s);
                    at = Some(y);
                }
            }
        }
        out
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

    /// A closed dropdown is one row, and it is the row the runtime renders.
    #[test]
    fn a_closed_dropdown_is_the_trigger_row_the_runtime_renders() {
        let spec = dropdown(&["fast", "slow"], 1, false, 0);
        assert_eq!(tree_rows(&spec), runtime_rows(&spec));
    }

    /// Open, the trigger row is still the runtime's — the option list floats
    /// rather than growing the panel, exactly as the runtime's does.
    #[test]
    fn an_open_dropdowns_trigger_row_is_still_the_runtimes() {
        let spec = dropdown(&["fast", "slow", "off"], 0, true, 0);
        assert_eq!(tree_rows(&spec), runtime_rows(&spec));
    }

    /// **The pop-over's rows are the collector's, verbatim.** The runtime is
    /// the formatter here: it clamps the scroll, slices the window and renders
    /// each option. What the tree adds is where the box goes and what a press
    /// on a row means.
    #[test]
    fn the_pop_over_paints_the_rows_the_collector_windowed() {
        let spec = dropdown(&["fast", "slow", "off"], 0, true, 0);
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(node(&spec, WIDTH, &cx()), Size::new(WIDTH, 24));
        let want: Vec<String> = runtime_popup(&spec)
            .entries
            .iter()
            .map(|e| {
                let mut n = e.clone();
                n.normalize_widths();
                n.text.trim_end_matches('\n').to_string()
            })
            .collect();
        let got = layer_rows(&ui);
        assert_eq!(
            got,
            want.iter().map(|w| w.to_string()).collect::<Vec<_>>(),
            "the option rows, verbatim"
        );
    }

    /// **The box hangs off the trigger's row, at the button's own column.**
    /// The runtime worked this out in screen coordinates — `inner.y +
    /// anchor_row + 1`, `inner.x + anchor_col` — from a rectangle it had
    /// recorded at paint time. The description has neither, and does not need
    /// them: the row is a node and the column within it is the collector's own
    /// answer.
    #[test]
    fn the_pop_over_opens_under_the_trigger_at_the_buttons_column() {
        let spec = dropdown(&["fast", "slow", "off"], 0, true, 0);
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(node(&spec, WIDTH, &cx()), Size::new(WIDTH, 24));
        let boxes = ui.spec().layers();
        let top = boxes
            .iter()
            .map(|i| i.rect)
            .min_by_key(|r| (r.y, r.x))
            .expect("a pop-over");
        assert_eq!(top.y, 1, "the row after the trigger's");
        assert_eq!(
            top.x,
            runtime_popup(&spec).anchor_col as i32,
            "the column the `[` is at"
        );
    }

    /// A press on an option row delivers `dropdown_select` with the option's
    /// **absolute** index — the window's offset is already in `row_indices`,
    /// which is why a scrolled list selects the right thing.
    #[test]
    fn pressing_an_option_selects_its_absolute_index() {
        let opts: Vec<String> = (0..12).map(|i| format!("opt{i}")).collect();
        let refs: Vec<&str> = opts.iter().map(|s| s.as_str()).collect();
        let spec = dropdown(&refs, 0, true, 4);
        let popup = runtime_popup(&spec);
        assert!(
            popup.entries.len() < opts.len(),
            "this list is windowed, or the test proves nothing"
        );
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(node(&spec, WIDTH, &cx()), Size::new(WIDTH, 24));
        // Row 0 of the box's interior: one row down and one column in from the
        // box's own corner, because the box has a border.
        let boxes = ui.spec().layers();
        let top = boxes
            .iter()
            .map(|i| i.rect)
            .min_by_key(|r| (r.y, r.x))
            .expect("a pop-over");
        let got = facts(ui.dispatch(fresh_ui::Input::press(
            fresh_ui::Point::new(top.x + 1, top.y + 1),
            fresh_ui::MouseButton::Left,
            fresh_ui::Mods::NONE,
        )));
        let UiFact::WidgetHit { hit, .. } = got.first().expect("a hit") else {
            panic!("expected a widget hit, got {got:?}");
        };
        assert_eq!(hit.event_type, "dropdown_select");
        assert_eq!(hit.widget_key, "mode");
        assert_eq!(
            hit.payload.get("index").and_then(|v| v.as_i64()),
            Some(popup.row_indices[0] as i64),
            "the first *visible* row's absolute index"
        );
    }

    /// A press on the box's border is swallowed, not passed on. It selects
    /// nothing and — the reason the runtime tested `popup_rect` before
    /// anything else — it must not reach whatever dismissal is behind it.
    #[test]
    fn a_press_on_the_pop_overs_border_is_swallowed() {
        let spec = dropdown(&["fast", "slow", "off"], 0, true, 0);
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(node(&spec, WIDTH, &cx()), Size::new(WIDTH, 24));
        let top = ui
            .spec()
            .layers()
            .iter()
            .map(|i| i.rect)
            .min_by_key(|r| (r.y, r.x))
            .expect("a pop-over");
        let got = ui.dispatch(fresh_ui::Input::press(
            fresh_ui::Point::new(top.x, top.y),
            fresh_ui::MouseButton::Left,
            fresh_ui::Mods::NONE,
        ));
        assert!(got.claimed, "the box takes the press");
        assert!(
            !got.msgs
                .iter()
                .any(|m| matches!(m, UiMsg::Ui(UiFact::WidgetHit { .. }))),
            "and does nothing with it: {:?}",
            got.msgs
        );
    }

    /// A press on the trigger toggles the list, which is the runtime's own
    /// `dropdown_toggle` hit over the `[value ▼]` button and not the label.
    #[test]
    fn pressing_the_trigger_toggles_and_the_label_does_not() {
        let spec = dropdown(&["fast", "slow"], 0, false, 0);
        let out = crate::widgets::render_spec(&spec, &Default::default(), "", WIDTH as u32);
        let h = out.hits.first().expect("a toggle hit");
        assert_eq!(h.event_type, "dropdown_toggle");
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(node(&spec, WIDTH, &cx()), Size::new(WIDTH, 24));
        let on_button = facts(ui.dispatch(fresh_ui::Input::press(
            fresh_ui::Point::new(h.byte_start as i32, 0),
            fresh_ui::MouseButton::Left,
            fresh_ui::Mods::NONE,
        )));
        assert!(
            matches!(
                on_button.first(),
                Some(UiFact::WidgetHit { hit, .. }) if hit.event_type == "dropdown_toggle"
            ),
            "the button toggles: {on_button:?}"
        );
        assert!(
            facts(ui.dispatch(fresh_ui::Input::press(
                fresh_ui::Point::new(0, 0),
                fresh_ui::MouseButton::Left,
                fresh_ui::Mods::NONE,
            )))
            .is_empty(),
            "the label does not"
        );
    }

    fn card(title: &str) -> WidgetSpec {
        WidgetSpec::LabeledSection {
            label: title.into(),
            child: Box::new(WidgetSpec::Raw {
                entries: vec![raw(title)],
                key: None,
            }),
            width_pct: None,
            key: None,
        }
    }

    fn card_list(n: usize, selected: i32, visible: u32) -> WidgetSpec {
        WidgetSpec::List {
            items: Vec::new(),
            item_specs: (0..n).map(|i| card(&format!("card{i}"))).collect(),
            item_keys: (0..n).map(|i| format!("c{i}")).collect(),
            selected_index: selected,
            visible_rows: Some(visible),
            key: Some("cards".into()),
            focusable: true,
        }
    }

    /// **A card is a band of rows, and the band is the item.** The runtime maps
    /// item `i` to rows `i * item_height ..`; `List::row_rows` is the same
    /// statement, made once, where the rows are placed.
    #[test]
    fn a_card_lists_items_take_a_band_of_rows_each() {
        let spec = card_list(6, 0, 9);
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(node(&spec, WIDTH, &cx()), Size::new(WIDTH, 24));
        let band = |k: &str| {
            let id = ui
                .find_by_key(&fresh_ui::Key::Str(k.into()))
                .unwrap_or_else(|| panic!("card {k}"));
            let r = ui.rect_of(id);
            (r.y, r.h)
        };
        let (y0, h0) = band("c0");
        assert!(h0 > 1, "a card is taller than a line: {h0}");
        assert_eq!(band("c1"), (y0 + h0 as i32, h0), "the next band, stacked");
        assert_eq!(band("c2"), (y0 + 2 * h0 as i32, h0));
    }

    /// The rows themselves are the runtime's, marking included: a selected
    /// card is drawn in heavy box glyphs rather than banded, because a band
    /// "reads garish over a multi-row card".
    #[test]
    fn a_selected_card_is_marked_in_its_own_glyphs() {
        let plain = tree_rows(&card_list(3, -1, 12));
        let picked = tree_rows(&card_list(3, 0, 12));
        assert!(
            plain.iter().any(|r| r.contains('╭')),
            "unselected cards keep the light box: {plain:?}"
        );
        assert!(
            picked.iter().any(|r| r.contains('┏')),
            "the selected one is heavy: {picked:?}"
        );
        assert_eq!(
            plain.len(),
            picked.len(),
            "and marking does not change the layout"
        );
    }

    /// A press anywhere on a card selects it, and says which — the same
    /// `select` hit with the same payload the runtime recorded for every row
    /// of the band.
    #[test]
    fn pressing_a_card_selects_that_item() {
        let spec = card_list(6, 0, 9);
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(node(&spec, WIDTH, &cx()), Size::new(WIDTH, 24));
        let r = ui.rect_of(
            ui.find_by_key(&fresh_ui::Key::Str("c1".into()))
                .expect("c1"),
        );
        // The card's *last* row, to prove the whole band is the target.
        let at = fresh_ui::Point::new(2, r.y + r.h as i32 - 1);
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
        let hit = got
            .iter()
            .find_map(|f| match f {
                UiFact::WidgetHit { hit, .. } => Some(hit),
                _ => None,
            })
            .unwrap_or_else(|| panic!("a select, got {got:?}"));
        assert_eq!(hit.event_type, "select");
        assert_eq!(hit.widget_key, "c1");
        assert_eq!(hit.owner_key.as_deref(), Some("cards"));
        assert_eq!(hit.payload.get("index").and_then(|v| v.as_i64()), Some(1));
    }

    /// The bar reads in items. Nine rows of window over three-row cards is a
    /// window of three *cards*, and a thumb sized from the nine would say the
    /// list is three times as visible as it is.
    #[test]
    fn a_card_lists_bar_measures_the_window_in_cards() {
        let spec = card_list(20, 0, 9);
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(node(&spec, WIDTH, &cx()), Size::new(WIDTH, 24));
        let bar = ui
            .spec()
            .items
            .iter()
            .find_map(|i| match i.draw {
                fresh_ui::Draw::Scrollbar {
                    offset,
                    content,
                    window,
                } => Some((offset, content, window)),
                _ => None,
            })
            .expect("twenty cards in nine rows overflow");
        let h0 = ui
            .rect_of(
                ui.find_by_key(&fresh_ui::Key::Str("c0".into()))
                    .expect("c0"),
            )
            .h;
        assert_eq!(bar.0, 0);
        assert_eq!(bar.1, 20, "twenty items");
        assert_eq!(bar.2, 9 / h0, "however many of them fit");
    }

    fn card_tree(n: usize, selected: i32, visible: u32) -> WidgetSpec {
        use fresh_core::api::TreeNode;
        WidgetSpec::Tree {
            nodes: (0..n)
                .map(|i| TreeNode {
                    text: raw(&format!("session {i}")),
                    depth: 0,
                    has_children: false,
                    checked: None,
                    extra_lines: vec![raw(&format!("branch-{i}")), raw("2 files")],
                })
                .collect(),
            item_keys: (0..n).map(|i| format!("s{i}")).collect(),
            selected_index: selected,
            visible_rows: Some(visible),
            key: Some("sessions".into()),
            expanded_keys: Vec::new(),
            checkable: false,
            indent_cols: 2,
            item_height: 3,
            card_borders: true,
        }
    }

    /// **A card node is a bordered block, and the blocks stack.** Three
    /// content rows plus a top and a bottom border is five, which is
    /// `tree_node_rows`' answer, arrived at by laying the rows out rather
    /// than by computing a band.
    #[test]
    fn a_card_trees_nodes_are_bordered_blocks() {
        let spec = card_tree(6, 0, 15);
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(node(&spec, WIDTH, &cx()), Size::new(WIDTH, 24));
        let band = |k: &str| {
            let id = ui
                .find_by_key(&fresh_ui::Key::Str(k.into()))
                .unwrap_or_else(|| panic!("node {k}"));
            let r = ui.rect_of(id);
            (r.y, r.h)
        };
        let (y0, h0) = band("s0");
        assert_eq!(h0, 5, "three content rows between two borders");
        assert_eq!(band("s1"), (y0 + 5, 5), "and the next block under it");
    }

    /// The selected card is framed in heavy glyphs — the marker
    /// `paint_dock_seamless_active_tab` keys on, so a background band here
    /// would silently lose the seamless-tab treatment.
    #[test]
    fn the_selected_card_is_framed_in_heavy_glyphs() {
        let picked = tree_rows(&card_tree(3, 1, 20));
        assert!(
            picked.iter().any(|r| r.contains('┏')),
            "a heavy frame somewhere: {picked:?}"
        );
        let plain = tree_rows(&card_tree(3, -1, 20));
        assert!(
            !plain.iter().any(|r| r.contains('┏')),
            "and only when something is selected: {plain:?}"
        );
    }

    /// **A press anywhere on the card selects it**, continuation rows
    /// included — "a card selects as a unit, so clicking its branch line must
    /// behave like clicking its title line".
    #[test]
    fn pressing_any_row_of_a_card_selects_the_node() {
        let spec = card_tree(6, 0, 15);
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(node(&spec, WIDTH, &cx()), Size::new(WIDTH, 24));
        let r = ui.rect_of(
            ui.find_by_key(&fresh_ui::Key::Str("s1".into()))
                .expect("s1"),
        );
        // Its second content row, which is a continuation line.
        let got = facts(ui.dispatch(fresh_ui::Input::press(
            fresh_ui::Point::new(4, r.y + 3),
            fresh_ui::MouseButton::Left,
            fresh_ui::Mods::NONE,
        )));
        let hit = got
            .iter()
            .find_map(|f| match f {
                UiFact::WidgetHit { hit, .. } => Some(hit),
                _ => None,
            })
            .unwrap_or_else(|| panic!("a select, got {got:?}"));
        assert_eq!(hit.event_type, "select");
        assert_eq!(hit.widget_kind, "tree");
        assert_eq!(hit.payload.get("index").and_then(|v| v.as_i64()), Some(1));
    }

    /// **The window scrolls to the selection, and the window is the tree's.**
    /// The runtime kept a row offset in its side table and wrote it as it
    /// rendered; here the offset is the viewport's and the only thing said is
    /// "put this row in the window".
    #[test]
    fn the_window_follows_a_selected_card_out_of_view() {
        let mut ui: Ui<UiMsg> = Ui::new();
        // Ten five-row cards in a ten-row window: two fit.
        ui.frame(
            node(&card_tree(10, 0, 10), WIDTH, &cx()),
            Size::new(WIDTH, 24),
        );
        let top = |ui: &Ui<UiMsg>, k: &str| {
            ui.rect_of(ui.find_by_key(&fresh_ui::Key::Str(k.into())).expect(k))
                .y
        };
        assert_eq!(top(&ui, "s0"), 0, "the window starts at the top");
        // Select the eighth: it is far below the window and the window moves.
        ui.frame(
            node(&card_tree(10, 7, 10), WIDTH, &cx()),
            Size::new(WIDTH, 24),
        );
        let y = top(&ui, "s7");
        assert!(
            (0..10).contains(&y),
            "the selected card is inside the window, at {y}"
        );
    }

    /// A card tree that fits needs no bar; one that overflows gets the
    /// viewport's, measured in rows because that is what it scrolls in.
    #[test]
    fn a_card_tree_that_overflows_shows_the_viewports_bar() {
        let bar_of = |spec: &WidgetSpec, h: u16| {
            let mut ui: Ui<UiMsg> = Ui::new();
            ui.frame(node(spec, WIDTH, &cx()), Size::new(WIDTH, h));
            ui.spec().items.iter().find_map(|i| match i.draw {
                fresh_ui::Draw::Scrollbar {
                    offset,
                    content,
                    window,
                } => Some((offset, content, window)),
                _ => None,
            })
        };
        assert_eq!(
            bar_of(&card_tree(2, 0, 10), 24),
            None,
            "two cards fit in ten rows"
        );
        assert_eq!(
            bar_of(&card_tree(10, 0, 10), 24),
            Some((0, 50, 10)),
            "ten five-row cards in a ten-row window"
        );
    }

    fn text_field(value: &str, cursor: i32, focused: bool) -> WidgetSpec {
        WidgetSpec::Text {
            value: value.into(),
            cursor_byte: cursor,
            focused,
            label: String::new(),
            placeholder: None,
            rows: 1,
            field_width: 20,
            max_visible_chars: 0,
            full_width: false,
            completions: Vec::new(),
            completions_visible_rows: 0,
            block_caret: false,
            sel_start: -1,
            sel_end: -1,
            label_width: 0,
            read_only: false,
            markdown: false,
            key: Some("field".into()),
        }
    }

    /// **The caret is a node, and its cell is where the glyphs put it.**
    ///
    /// The runtime reported a row and a byte offset and the painter turned
    /// that into a screen cell by measuring the row's text a second time. A
    /// zero-width marker at the caret's byte lands after the same glyphs the
    /// row painted, so the host reads a rectangle instead of re-measuring.
    #[test]
    fn the_caret_is_a_cell_the_layout_placed() {
        let focused = Ctx {
            focus_key: "field".into(),
            ..cx()
        };
        let at = |value: &str, cursor: i32| {
            let mut ui: Ui<UiMsg> = Ui::new();
            ui.frame(
                node(&text_field(value, cursor, true), WIDTH, &focused),
                Size::new(WIDTH, 8),
            );
            ui.find_by_key(&caret_key()).map(|id| ui.rect_of(id).x)
        };
        let head = at("hello", 0).expect("a caret at the head");
        let mid = at("hello", 3).expect("a caret in the middle");
        let end = at("hello", 5).expect("a caret past the last glyph");
        assert_eq!(mid - head, 3, "three glyphs to the left of it");
        assert_eq!(end - head, 5, "and five at the end");
    }

    /// A caret only where there is one: an unfocused field has no marker, so
    /// the host has nothing to place and parks its cursor instead.
    #[test]
    fn an_unfocused_field_has_no_caret_node() {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            node(&text_field("hello", -1, false), WIDTH, &cx()),
            Size::new(WIDTH, 8),
        );
        assert!(ui.find_by_key(&caret_key()).is_none());
    }

    fn text_area(value: &str, cursor: i32, rows: u32, label: &str) -> WidgetSpec {
        WidgetSpec::Text {
            value: value.into(),
            cursor_byte: cursor,
            focused: false,
            label: label.into(),
            placeholder: None,
            rows,
            field_width: 20,
            max_visible_chars: 0,
            full_width: false,
            completions: Vec::new(),
            completions_visible_rows: 0,
            block_caret: false,
            sel_start: -1,
            sel_end: -1,
            label_width: 0,
            read_only: false,
            markdown: false,
            key: Some("doc".into()),
        }
    }

    /// **A multi-line field takes the rows it asked for and no more**, however
    /// long the document is — which is what a window is.
    #[test]
    fn a_text_areas_height_is_its_row_budget() {
        let doc: String = (0..30).map(|i| format!("line {i}\n")).collect();
        let k = fresh_ui::Key::Str("area".into());
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            node(&text_area(&doc, -1, 6, ""), WIDTH, &cx()).key(k.clone()),
            Size::new(WIDTH, 40),
        );
        assert_eq!(
            ui.rect_of(ui.find_by_key(&k).expect("the area")).h,
            6,
            "six rows of a thirty-line document"
        );
    }

    /// And the bar is the viewport's, measured in the rows it scrolls.
    #[test]
    fn a_text_area_that_overflows_shows_the_viewports_bar() {
        let bar = |lines: usize| {
            let doc: String = (0..lines).map(|i| format!("line {i}\n")).collect();
            let mut ui: Ui<UiMsg> = Ui::new();
            ui.frame(
                node(&text_area(&doc, -1, 6, ""), WIDTH, &cx()),
                Size::new(WIDTH, 40),
            );
            ui.spec().items.iter().find_map(|i| match i.draw {
                fresh_ui::Draw::Scrollbar {
                    content, window, ..
                } => Some((content, window)),
                _ => None,
            })
        };
        assert_eq!(bar(3), None, "a document that fits needs no bar");
        let (content, window) = bar(30).expect("a thirty-line document overflows six rows");
        assert!(
            content > window as u32,
            "{content} rows in a window of {window}"
        );
        assert_eq!(window, 6);
    }

    /// **The label does not scroll.** The collector emits it as row zero and
    /// windows only the text under it, so it stays outside the viewport.
    #[test]
    fn a_text_areas_label_stays_out_of_the_window() {
        let doc: String = (0..30).map(|i| format!("line {i}\n")).collect();
        let k = fresh_ui::Key::Str("area".into());
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            col().child(node(&text_area(&doc, -1, 6, "Notes"), WIDTH, &cx()).key(k.clone())),
            Size::new(WIDTH, 40),
        );
        let rows = rows_of(&ui);
        assert!(rows[0].starts_with("Notes"), "the label leads: {rows:?}");
        assert!(rows[1].starts_with("line 0"), "then the window: {rows:?}");
        assert_eq!(
            ui.rect_of(ui.find_by_key(&k).expect("the area")).h,
            7,
            "the label plus the window's six"
        );
    }

    /// **The window follows the caret**, which is the whole of what the
    /// runtime's auto-clamp did — said as "put this row in the window" rather
    /// than as an offset written into the side table it also read.
    #[test]
    fn the_window_follows_the_caret() {
        let doc: String = (0..30).map(|i| format!("line {i}\n")).collect();
        let focused = Ctx {
            focus_key: "doc".into(),
            ..cx()
        };
        // The caret on line 25: byte offset is 25 lines of "line NN\n".
        let at: i32 = doc
            .char_indices()
            .filter(|(_, c)| *c == '\n')
            .nth(24)
            .map(|(i, _)| i as i32 + 1)
            .expect("line 25");
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            node(&text_area(&doc, at, 6, ""), WIDTH, &focused),
            Size::new(WIDTH, 40),
        );
        let rows = rows_of(&ui);
        assert!(
            rows.iter().any(|r| r.starts_with("line 25")),
            "the caret's line is in the window: {rows:?}"
        );
        assert!(
            !rows.iter().any(|r| r.starts_with("line 0 ")),
            "and the top of the document is not"
        );
    }
}
