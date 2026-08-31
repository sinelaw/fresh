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
//! or painted, never half of each: a spec using a variant this module has no
//! arm for takes the old path whole. That is the same seam as a `Host` leaf.
//! It now answers `true` for everything but `WindowEmbed`, which is a `Host`
//! leaf by rule and never crosses.
//!
//! **Read `covered` as what it says: the adapter has an arm for this variant.**
//! It does not say the arm is native, and this doc twice used to imply more
//! than was true — first that the gate was "what remains of a boundary that has
//! closed", then that three variants still passed whole through a generic
//! adapter over the immediate-mode runtime.
//!
//! Every variant is now written out below as nodes. The generic adapter is
//! gone: nothing routes a whole widget through `render_collected` and rebuilds
//! it from the cells that came back, so no press is resolved by matching a byte
//! range in a row the painter produced.
//!
//! What has *not* gone is the runtime as a **formatter**. Each arm still asks
//! it what a row says — `render_dropdown` for a trigger, `render_text_input`
//! for a field, the tree's own row renderer for indent guides — because what a
//! widget's row says is domain knowledge, and rewriting it would be rewriting
//! thousands of lines to get the same cells. What moved is where the row is,
//! what a press on it means, and who owns the window it sits in.
//!
//! Two calls into the runtime remain, and both render a *nested spec* rather
//! than routing a widget: a card list asks it for each item's subtree, and a
//! multi-line field asks it for the whole document the viewport windows. Those
//! are the last of it on this path.
//!
//! `Dropdown` and `Text` have already left it (Phase 2.2): each is built from
//! its own spec, calling the runtime's *formatter* through the pure functions
//! `kinds::dropdown::{resolve, anchor_col, popup_of}` and
//! `kinds::text::{resolve, single_line, completion_popup}`, which the
//! collector calls too — one copy of each rule rather than two that can
//! drift. A multi-line `Text` is the exception still: its rows come from
//! `render_collected` because a text *area* is a wrapping engine, and it is
//! windowed here rather than described.

use fresh_core::api::{OverlayColorSpec, OverlayOptions, WidgetSpec};
use fresh_core::text_property::TextPropertyEntry;
use fresh_ui::{col, row, text_runs, Node, Run, Sizing};

use crate::app::shell_host::shell_theme::{pair, Attrs, Ink, Paint};

use super::msg::UiMsg;

/// The plugin panel surface's own colours, which every row of one starts
/// from. Other surfaces say their own — see [`Ctx::surface`].
const BASE_FG: &str = "ui.suggestion_fg";
const BASE_BG: &str = "ui.suggestion_bg";

/// The ink a plugin panel's rows start from: the default [`Ctx::surface`].
pub fn panel_surface() -> Ink {
    Ink::keys(BASE_FG, BASE_BG)
}

/// The ink a panel *mounted into a pane* starts from.
///
/// **A mounted panel is not standing on a panel.** Its rows were the text of a
/// virtual buffer, painted on the editor's own ground like every other line of
/// every other buffer — so the surface it says is the editor's, not the
/// suggestion palette the floating and dock panels sit on. Naming
/// [`panel_surface`] here would repaint the search panel in the popup's
/// background and make it look like a floating box welded to the bottom of the
/// window.
pub fn pane_surface() -> Ink {
    Ink::keys("editor.fg", "editor.bg")
}

/// Which panel a description belongs to.
///
/// The view layer's own spelling of `app::PanelSlot`, mirrored the way
/// `modal::Slot` is, so a description carries no app types.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Slot {
    Dock,
    Floating,
    /// The settings dialog's card body. Not a plugin panel — the same
    /// `WidgetSpec`s, on a different surface, whose hits become
    /// `SettingsHit`s rather than reaching a plugin's `widget_event`.
    Settings,
    /// A settings entry-edit dialog's field. The same again, one surface in:
    /// its item indices are the dialog's, not the page's.
    SettingsEntry,
    /// A panel mounted into a pane's buffer (`mountWidgetPanel`) — the
    /// review-diff sidebar, Search & Replace.
    ///
    /// **It names the pane, because that is the only thing about it that is
    /// different.** Everything in this module is slot-agnostic: a `Ctx` says
    /// where the facts go and nothing in the translation asks where the
    /// surface lives, which is what made C.5 a decision about drawing rather
    /// than about the vocabulary. What the host cannot recover without this
    /// is *which* mounted panel a hit belongs to, and a pane is one buffer.
    Pane(crate::model::event::LeafId),
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
    /// The open dropdown pop-over's hovered option, as a decimal index, or
    /// empty. A pop-over's rows are not panel rows, so the runtime's own hover
    /// probe never sees them; the tree reports this one and hands it back here
    /// for the sub-render that draws the rows.
    pub hovered_popup_row: String,
    /// Row budget for auto-sized `List`/`Tree` widgets, when the host knows
    /// the surface's inner height.
    pub avail_height: Option<u32>,
    /// Whether this panel's scrollbars are *overlay* bars, and if so whether
    /// they are being revealed right now.
    ///
    /// `None` — a bar whenever the content overflows, which is what every
    /// panel but the dock has. `Some(shown)` — the dock's: a bar that appears
    /// while the pointer is over the column or a keyboard move just flashed
    /// it, and is gone otherwise, *even while the list holds focus*. Focus is
    /// not attention: the dock keeps the keyboard for as long as you are
    /// working in it, and a bar that stayed for all of that would be a
    /// permanent stripe down the column rather than an answer to "how far
    /// through am I".
    ///
    /// It is a fact rather than a policy because neither half is the tree's
    /// to know: the flash is a deadline the plugin arms through
    /// `WidgetEffects::flash_scrollbar` and the editor ticks, and the hover
    /// is a memo the dock's own column reports.
    pub scrollbar_reveal: Option<bool>,
    /// The ink every row on this panel starts from.
    ///
    /// **A widget does not know what it is sitting on.** The same
    /// `WidgetSpec` is a row of a plugin's dock panel, a floating panel, and
    /// a settings card — three surfaces with three backgrounds — and the
    /// runtime painted entries whose `bg` was simply *unset*, so whatever the
    /// painter had already put in the cell showed through. A description has
    /// no "already": every run carries both halves, so the surface has to be
    /// said rather than inherited. This is where it is said.
    pub surface: Ink,
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
            hovered_popup_row: String::new(),
            avail_height: None,
            scrollbar_reveal: None,
            surface: panel_surface(),
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

/// The editor's one pair of scrollbar colours.
///
/// **A bar with no name of its own is a solid stripe.** The fold writes the
/// thumb in the item's foreground and the track in its background, both as
/// the cell's ground — so a bar that inherits the ambient ink inherits a
/// foreground and a background that were chosen for *text*, and the two come
/// out the same colour with no thumb to see. Named here for the same reason
/// the overlay prompt's list names it: it is the editor's one scrollbar,
/// wherever it appears.
fn bar_ink() -> String {
    pair("ui.scrollbar_thumb_fg", "ui.scrollbar_track_fg")
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
/// The element key for a stateful widget, from the spec's own key.
///
/// **Identity is declared, not positional.** A `List`, `Tree` or scrolling
/// `Text` is the only thing in a panel's subtree that owns element state — a
/// scroll offset, a hover, a reveal — and reconciliation matched them by
/// position among their siblings and by component *type*. Two consequences,
/// both live before this: a plugin re-emitting its spec with one extra sibling
/// above a list shifted every following node and **remounted** it, silently
/// resetting its scroll; and two different lists at the same position updated
/// in place, so one inherited the other's offset. Plugins rebuild their spec
/// trees freely and nothing stopped either.
///
/// A widget with no key of its own stays positional, because there is nothing
/// stable to name it by — that is the plugin's choice to make, and a synthetic
/// index would only move the same bug behind a name that looks deliberate.
fn state_key(key: &Option<String>) -> Option<fresh_ui::Key> {
    key.as_deref()
        .filter(|k| !k.is_empty())
        .map(|k| fresh_ui::Key::Str(k.into()))
}

/// The same, for a node built where only the spec is in hand.
fn spec_state_key(spec: &WidgetSpec) -> Option<fresh_ui::Key> {
    match spec {
        WidgetSpec::Text { key, .. }
        | WidgetSpec::List { key, .. }
        | WidgetSpec::Tree { key, .. } => state_key(key),
        _ => None,
    }
}

/// Apply [`state_key`]'s answer, if there is one.
fn keyed(node: Node<UiMsg>, key: Option<fresh_ui::Key>) -> Node<UiMsg> {
    match key {
        Some(k) => node.key(k),
        None => node,
    }
}

/// **A tree's `visibleRows` is a window, not a height** — and a list's is a
/// height.
///
/// The two kinds differ, in the runtime, in a way nothing in the vocabulary
/// says out loud: `kinds/list.rs` pads its output to the advertised row count
/// ("so the List occupies its full `visible_rows`"), while `kinds/tree.rs`
/// emits at most that many and stops. A plugin can and does depend on the
/// difference — the orchestrator pads the gap *below* its dock tree with blank
/// rows so its hint bar lands on the dock's last two, and its comment says
/// why: "The host tree renders only its actual content rows (it does not pad
/// itself out to `visibleRows`)".
///
/// Giving both a fixed height made the dock's column taller than the dock by
/// exactly the padding, which pushed the hint bar off the bottom and hung four
/// e2e tests waiting for it. So a tree is as tall as its content, capped.
fn tree_rows(content: u32, visible: u32) -> u16 {
    content.min(visible).min(u16::MAX as u32) as u16
}

/// The description for a covered spec.
///
/// `width` is the panel's inner content width, which two variants need before
/// layout can run: a `Divider` is as wide as the panel by definition, and the
/// runtime pads rows to it. Passing it in rather than reading it back is the
/// rule §4.4 states — this is *content* resolved from a known extent, not
/// geometry recorded from a paint.
pub fn node(spec: &WidgetSpec, width: u16, cx: &Ctx<'_>) -> Node<UiMsg> {
    // A spec with no container above it is laid into a column: that is what
    // the panel body and the settings field are, and what the runtime's own
    // collector assumes when it walks a bare spec.
    node_in(spec, width, cx, Site::ROOT)
}

/// Which way the container above a node runs.
///
/// **Only a flexible spacer needs this, and it needs it absolutely.**
/// `Node::flex` sets *both* axes, and on a container's cross axis
/// `Sizing::Flex` means "fill the extent" — so a `flexSpacer()` inside a row
/// asked that row to be as tall as everything left in the column above it. One
/// of those in the New-Workspace form's tab row made the row twenty-six cells
/// tall, and every field under it was laid out at zero height against the
/// panel's bottom edge. A spacer flexes along the axis it sits on; nothing
/// else in the adapter cares which that is.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Axis {
    Across,
    Down,
}

/// Where in the panel a node is being laid: the axis of the container above
/// it, and how far left of it a floating overlay row starts.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct Site {
    axis: Axis,
    /// **The columns of chrome an overlay row re-adds.**
    ///
    /// A focused field's completion popup is emitted as *overlay rows* whose
    /// glyphs are `panel_width + 4` wide — "re-add section chrome", in
    /// `emit_completion_overlays`' own words — because the runtime draws every
    /// row of a panel as one flat, full-width entry and a `LabeledSection`'s
    /// `│ … │` is part of that text. The tree draws the section as a *box*
    /// instead, so its child starts two columns in, and an overlay hung off
    /// that child landed two columns right of the frame it is drawn to line up
    /// with. This is that offset, stated by the node that creates it.
    escape: u16,
}

impl Site {
    const ROOT: Site = Site {
        axis: Axis::Down,
        escape: 0,
    };
    fn across(self) -> Site {
        Site {
            axis: Axis::Across,
            ..self
        }
    }
    fn down(self) -> Site {
        Site {
            axis: Axis::Down,
            ..self
        }
    }
}

/// The ground a row's own runs are built from, for each state the list reports.
///
/// **It has to agree with `row_theme` cell for cell.** The theme names the row
/// node, which fills the row; the builder's runs then paint over that fill, and
/// every run built from an editor `TextPropertyEntry` carries *both* halves —
/// a description has no "already" for a fill to show through. Built from the
/// plain ground while the theme filled a selection colour, a selected row came
/// out highlighted only in the gaps between its glyphs. That is what the
/// orchestrator dock's active session looked like: a band on the padding after
/// the name and nowhere else.
/// The whole-entry style a `Row`'s leading child would have tinted its merged
/// line with, if there is one.
///
/// **A Row of inline pieces is one line, and its ground is its leading
/// child's.** The runtime collapses such a row into a single
/// `TextPropertyEntry` (`assemble_inline_row`), and `merge_inline` keeps the
/// *first* piece's whole-entry `style` while dropping every later one's — so a
/// plugin tints the whole strip by putting the style on the first child and
/// says so out loud: "a Row's inline collapse keeps the leading child's entry
/// style for the merged line ... `base` tints the title, the spacer, and the
/// button alike" (`orchestrator.ts`, `dockTitleRow`).
///
/// Laid as separate nodes, that tint stopped where the first child's text
/// stopped: the dock's title bar was a band the width of the word
/// "Orchestrator" with the rest of the row on the editor's ground.
fn leading_row_style(children: &[WidgetSpec]) -> Option<&fresh_core::api::OverlayOptions> {
    match children.first()? {
        WidgetSpec::Raw { entries, .. } => entries.first()?.style.as_ref(),
        _ => None,
    }
}

fn row_surface(st: fresh_ui::widgets::RowState, plain: &Ink) -> Ink {
    use fresh_ui::widgets::RowState;
    match st {
        RowState::Selected | RowState::SelectedBlur => Ink::new(
            Paint::key("ui.popup_selection_fg"),
            Paint::key("ui.popup_selection_bg"),
        ),
        RowState::Hover => Ink::new(
            Paint::key("ui.menu_hover_fg"),
            Paint::key("ui.menu_hover_bg"),
        ),
        RowState::Normal => plain.clone(),
    }
}

fn node_in(spec: &WidgetSpec, width: u16, cx: &Ctx<'_>, site: Site) -> Node<UiMsg> {
    on_the_ring(spec, cx, node_body(spec, width, cx, site))
}

/// **Put a plugin widget on the tree's focus ring, if it is one.**
///
/// Eight kinds are focusable — button, toggle, number, text, dropdown, list,
/// tree and dual list — and two of them conditionally: a disabled or opted-out
/// button is not, and a list is only when its spec says so. Those rules are
/// *not* restated here. `box_meta` is where each kind already answers "am I
/// focusable, and what is my key", and it is the same call the runtime's own
/// ring makes, so asking it is what keeps there being one answer. Writing the
/// eight arms out again would be the ninth copy of a rule, which is the shape
/// this migration exists to remove.
///
/// The wrapper reports only a *gain*, and reports it as the widget's key. That
/// demotes the registry's focused-key string from an authority — resolved
/// host-side across a whole spec, read back by this description — to a mirror
/// of what the tree decided. Same move Phase 2.1 made for the scroll folds.
///
/// **It goes on every interactive kind at once, and that is deliberate.** A
/// ring holding only buttons would let Tab cycle buttons and step over the
/// fields between them, which is worse than a ring the tree does not own: the
/// unit of this change is a panel, not a widget kind.
fn on_the_ring(spec: &WidgetSpec, cx: &Ctx<'_>, n: Node<UiMsg>) -> Node<UiMsg> {
    let meta = crate::widgets::kinds::behavior(spec).box_meta(spec);
    let Some(k) = meta.key.filter(|k| !k.is_empty() && meta.focusable) else {
        return n;
    };
    let (slot, widget) = (cx.slot, k.clone());
    // **The wrapper adopts the child's sizing, because it is not transparent.**
    //
    // Focus properties only attach to a `Focusable` node, so putting a widget
    // on the ring means a node *around* it rather than a flag on it — and a
    // wrapper left at its default `Auto` is a second opinion about the child's
    // extent. A text area asked for six rows of a thirty-line document and the
    // wrapper reported all thirty, because `Auto` measured the content the
    // viewport exists to window.
    let (w, h) = (n.w, n.h);
    fresh_ui::focusable(n)
        .w(w)
        .h(h)
        .key(fresh_ui::Key::Str(format!("widget_focus:{k}").into()))
        .on_focus_change(move |e: &fresh_ui::Event| {
            match e.kind == fresh_ui::GestureKind::FocusGained {
                // A loss is not reported: focus is never nowhere while a panel
                // is up, so a loss paired with the matching gain would race,
                // and the gain is the one that names the new holder.
                false => None,
                true => Some(UiMsg::Ui(super::msg::UiFact::WidgetFocus {
                    slot,
                    widget: widget.clone(),
                })),
            }
        })
}

fn node_body(spec: &WidgetSpec, width: u16, cx: &Ctx<'_>, site: Site) -> Node<UiMsg> {
    let axis = site.axis;
    match spec {
        WidgetSpec::Row { children, wrap, .. } => {
            // **A row of blocks splits its width; a row of inline pieces does
            // not.** `allocate_row_child_widths` is the runtime's own rule —
            // a `LabeledSection` with a `width_pct` takes its declared share,
            // the rest split what is left equally, and an inline child gets
            // the full width as a soft cap — and it is *called* here rather
            // than restated, because the plugin API's `width_pct` means what
            // that function says it means.
            //
            // Laid out as `Auto` instead, a block measured against the room
            // that was left and the first one took all of it: the
            // orchestrator's two-column picker came out as its sessions list
            // alone, with the preview pane — and the bulk-action bar inside
            // it — allocated no width at all.
            let widths = crate::widgets::kinds::containers::allocate_row_child_widths(
                children,
                width as u32,
            );
            // An inline-only row's ground is its leading child's — see
            // `leading_row_style`. A themed node emits its own fill, so the
            // band spans the row; the children are built *from* the same ink
            // so their own runs do not paint the editor's ground back over it.
            let inline_only = !children
                .iter()
                .any(crate::widgets::kinds::containers::predicts_block);
            let band = match inline_only {
                true => leading_row_style(children).map(|st| ink_of(st, &cx.surface)),
                false => None,
            };
            let inner = Ctx {
                surface: band.clone().unwrap_or_else(|| cx.surface.clone()),
                states: cx.states,
                focus_key: cx.focus_key.clone(),
                hovered_key: cx.hovered_key.clone(),
                hovered_item_key: cx.hovered_item_key.clone(),
                hovered_popup_row: cx.hovered_popup_row.clone(),
                ..*cx
            };
            let r = row().children(
                children
                    .iter()
                    .zip(widths)
                    .map(|(c, w)| {
                        let w = (w as u16).max(1);
                        let n = node_in(c, w, &inner, site.across());
                        match crate::widgets::kinds::containers::predicts_block(c) {
                            true => n.w(Sizing::Cells(w)),
                            false => n,
                        }
                    })
                    .collect::<Vec<_>>(),
            );
            let r = match &band {
                Some(ink) => r.theme(ink.to_string()),
                None => r,
            };
            match wrap {
                true => r.wrap_children(),
                false => r,
            }
        }
        WidgetSpec::Col { children, .. } => col().children(
            children
                .iter()
                .map(|c| node_in(c, width, cx, site.down()))
                .collect::<Vec<_>>(),
        ),
        // `flex` fills the container's remainder *on its own axis*. A fixed
        // spacer is `cols` blank cells in a row and **one blank line** in a
        // column, however wide it says it is: `kinds::spacer` pushes exactly
        // one entry, and a column of entries is a column of rows. Sized by
        // `cols` in both, every `spacer(0)` between a form's sections
        // vanished and the fields closed up against each other.
        WidgetSpec::Spacer { cols, flex, .. } => match (flex, axis) {
            (true, Axis::Across) => row().w(Sizing::Flex(1)),
            (true, Axis::Down) => row().h(Sizing::Flex(1)),
            (false, Axis::Across) => row().w(Sizing::Cells(*cols as u16)),
            (false, Axis::Down) => row().h(Sizing::Cells(1)),
        },
        // Full width by definition — "so the separator always matches the
        // rendered width, including a user-dragged dock, without the plugin
        // computing the width itself".
        // **A rule is as wide as the panel, and `width` is that width.**
        //
        // It is text of a computed length rather than something that fills,
        // which is fine and stays fine *because* the caller passes the same
        // number it lays the subtree out at. Where those two diverge — the
        // dock, laid one column short of the painter's divider — everything
        // else pins to the laid width by flex and this pins to the parameter,
        // and the title bar's `×` comes to rest one column off the rule it
        // lines up with. The fix is to keep them equal, not to make this fill;
        // see `shell::dock::DIVIDER_COLS`.
        WidgetSpec::Divider { ch, style, .. } => {
            let glyph = match ch.is_empty() {
                true => "─",
                false => ch.as_str(),
            };
            let n = width as usize / glyph.chars().count().max(1);
            let ink = match style {
                Some(o) => ink_of(o, &cx.surface),
                None => cx.surface.clone(),
            };
            text_runs([Run::themed(glyph.repeat(n), ink.to_string())]).h(Sizing::Cells(1))
        }
        // The formatter is the runtime's own: what a hint row *says* is domain
        // knowledge and does not move.
        WidgetSpec::HintBar { entries, .. } => {
            entry_row(&crate::widgets::render_hint_bar(entries), &cx.surface)
        }
        // Entries the plugin wrote, inlined without interpretation. That is
        // the variant's whole contract, and it is one row per entry.
        WidgetSpec::Raw { entries, .. } => col().children(
            entries
                .iter()
                .map(|e| entry_row(e, &cx.surface))
                .collect::<Vec<_>>(),
        ),
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
                &cx.surface,
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
            // Transparent to layout, so it is transparent to the axis too:
            // a `component()` wrapping a `flexSpacer()` sits wherever the
            // component sits.
            let n = fresh_ui::focusable(node_in(child, width, cx, site)).focus_scope();
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
                &cx.surface,
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
            let n = entry_row(&entry, &cx.surface);
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
            let ring = cx.surface.to_string();
            // The child is handed the width less this box's four columns of
            // chrome, which is the contract `emit_completion_overlays` reads
            // when it widens a completion popup back out by four. Two of those
            // columns are on this side, so an overlay the child floats starts
            // two left of where the child does.
            // **Rounded, because that is what this box has always been.**
            // `render_section_top_border` writes `╭─ label ─…─╮`; the fold's
            // border was unconditionally `┌┐└┘`, so describing the section
            // squared it off. `BorderStyle` is the description saying which
            // corner set it meant — see `fresh_ui::BorderStyle`.
            let framed = col()
                .theme(ring.clone())
                .border_style(fresh_ui::BorderStyle::Rounded)
                .pad(1, 0)
                .child(node_in(
                    child,
                    width.saturating_sub(4).max(1),
                    cx,
                    Site {
                        axis: Axis::Down,
                        escape: 2,
                    },
                ));
            match label.is_empty() {
                true => framed,
                // The legend rides the top edge, the way every other titled
                // frame in the shell does it — a transparent strip stacked
                // over the box rather than text spliced into the ring.
                // The strip is one row and says so. A `flex` filler under it
                // — there to push it to the top edge, which is where a
                // stack's children start anyway — made the legend column ask
                // for every row left in the form, and the box it labels is
                // three.
                false => fresh_ui::stack().children([
                    framed,
                    col()
                        .pointer_mode(fresh_ui::PointerMode::Transparent)
                        .h(Sizing::Cells(1))
                        .children([row().h(Sizing::Cells(1)).children([
                            row().w(Sizing::Cells(2)),
                            text_runs([Run::themed(format!(" {label} "), ring)]),
                        ])]),
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
            let list = fresh_ui::List::windowed_stateful(
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
                    let surface = cx.surface.clone();
                    move |i, st| entry_row(&rows[i], &row_surface(st, &surface))
                },
            )
            // The panel's focus is the host's — the runtime resolves a focus
            // key across every widget — so the list declines the ring and
            // keeps its mouse, which is what that flag means since #3108.
            .focusable(false)
            .scrollbar_when(cx.scrollbar_reveal)
            .scrollbar_theme(bar_ink())
            .row_theme({
                let plain = cx.surface.clone();
                move |_, st| row_surface(st, &plain).to_string()
            })
            .on_activate_handler(Rc::new(move |i, e: &fresh_ui::Event| {
                Some(UiMsg::Ui(super::msg::UiFact::WidgetHit {
                    slot,
                    hit: crate::widgets::HitArea {
                        row_target: true,
                        // **What the runtime's own row says** (`kinds/list.rs`
                        // sets it too). It cost nothing while the probe
                        // supplied the hit for a right press; now that the
                        // tree is the only answer and the probe stands down
                        // for a described panel, a row that does not declare
                        // the capability raises no context menu at all.
                        context_click: true,
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
                    at: None,
                    clicks: e.clicks,
                }))
            }));
            // **`-1` is a controlled empty selection, not "no opinion".** A
            // `WidgetSpec::List` says which row is selected and says `-1` when
            // none is — the settings `[+] Add new` sentinel is a one-row list
            // that is only selected when the arrows are on it. Leaving the
            // element to its own selection highlighted row zero, so the
            // sentinel looked focused whether it was or not, and on a selected
            // card the real row highlight was the same colour as the band it
            // sat in.
            let list = list.selection(match sel >= 0 {
                true => Some(sel as usize),
                false => None,
            });
            let node = keyed(fresh_ui::ComponentExt::node(list), state_key(key));
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
                        hover_popup_row: "",
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
                    let surface = cx.surface.clone();
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
                            entry_row(&e, &surface)
                        }))
                    }
                },
            )
            .focusable(false)
            .row_rows(item_height)
            .scrollbar_gutter()
            .scrollbar_when(cx.scrollbar_reveal)
            .scrollbar_theme(bar_ink())
            .row_theme({
                let plain = cx.surface.to_string();
                let hover = cx
                    .surface
                    .clone()
                    .with_bg(Paint::key("ui.menu_hover_bg"))
                    .to_string();
                move |_, st| match st {
                    fresh_ui::widgets::RowState::Hover => hover.clone(),
                    // A selected card is marked in its own glyphs, not by a
                    // band.
                    _ => plain.clone(),
                }
            })
            .on_activate_handler(Rc::new(move |i, e: &fresh_ui::Event| {
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
                    at: None,
                    clicks: e.clicks,
                }))
            }));
            let list = match sel >= 0 {
                true => list.selected(sel as usize),
                false => list,
            };
            let node = keyed(fresh_ui::ComponentExt::node(list), state_key(key));
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
        // The rows are the runtime's, and so is most of the marking: a
        // selected card gets a box frame rather than a band, because a band
        // "reads garish over a multi-row card". Which frame is this module's,
        // and it depends on what the card is standing next to — the heavy one
        // everywhere, and in the dock the seamless tab, which is
        // [`open_card_edge`] and [`tab_scoop`] together.
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
        } if *card_borders => {
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
                // **In the dock, the selected card is the seamless tab.**
                // There the card sits against a wall — `dock::grip_ink`'s
                // divider, in the column's last cell — and the active session
                // is the one mirrored in the editor beside it, so its card
                // opens onto the editor instead of being boxed off from it
                // (F.8). That is the whole marker, and it is made of glyphs:
                // nothing here depends on a colour. Everywhere else there is
                // no wall to open onto, so the heavy frame stays what
                // selection looks like.
                let tab = is_selected && as_card && matches!(cx.slot, Slot::Dock);
                let hovered = !is_selected
                    && !cx.hovered_item_key.is_empty()
                    && cx.hovered_item_key == item_key;
                let dress = |e: &mut TextPropertyEntry| {
                    if is_selected {
                        match (as_card, tab) {
                            (true, true) => open_card_edge(e),
                            (true, false) => crate::widgets::render::mark_list_card_selected(e),
                            (false, _) => {
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
                rows.push(entry_row_hits(&primary, cx.slot, &cx.surface, &hits));
                for extra in r.extra_entries.iter() {
                    let mut e = extra.clone();
                    dress(&mut e);
                    let b = e.text.len();
                    rows.push(match b > 0 {
                        true => entry_row_hit(&e, (0, b), cx.slot, &cx.surface, select(0, b, true)),
                        false => entry_row(&e, &cx.surface),
                    });
                }
                let h = rows.len() as u32;
                let block = fresh_ui::Key::Str(
                    match item_key.is_empty() {
                        true => i.to_string(),
                        false => item_key.clone(),
                    }
                    .into(),
                );
                blocks.push(Chunk {
                    edge: tab.then(|| tab_scoop(block.clone(), at, h, &cx.surface)),
                    key: block,
                    start: at,
                    rows,
                });
                at += h;
            }
            let node = keyed(
                fresh_ui::ComponentExt::node(Scrolled {
                    blocks: std::rc::Rc::new(blocks),
                    selected,
                    reveal: cx.scrollbar_reveal,
                }),
                state_key(key),
            );
            // **As wide as the panel, not as wide as its rows.** The rows
            // arrive pre-rendered at the runtime's wrap width, so a window
            // sized to its content stops where the text does — and the two
            // things that belong at the panel's edge, the row band and the
            // overlay scrollbar, stop with it.
            let node = node.w(Sizing::Pct(100));
            match visible_rows {
                Some(r) => node.h(Sizing::Cells(tree_rows(at, *r))),
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
        } if !*card_borders => {
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
            let surface = cx.surface.clone();
            let sel_abs = *selected_index;
            let n = visible.len();

            let row_at = {
                let (nodes, keys, visible) = (nodes.clone(), keys.clone(), visible.clone());
                let tree_key = tree_key.clone();
                move |i: usize, st: fresh_ui::widgets::RowState| -> Node<UiMsg> {
                    let surface = row_surface(st, &surface);
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
                    entry_row_hits(&r.entry, slot, &surface, &hits)
                }
            };

            let list = fresh_ui::List::windowed_stateful(
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
            .scrollbar_when(cx.scrollbar_reveal)
            .scrollbar_theme(bar_ink())
            .row_theme({
                let plain = cx.surface.clone();
                move |_, st| row_surface(st, &plain).to_string()
            });
            // The spec's selection is an index into the *whole* array; the
            // list's is into the visible window, which is the same array with
            // the collapsed subtrees taken out.
            // A selection the window does not contain is *no* selection here,
            // not the element's own — see the `List` arm above.
            let list = list.selection(visible.iter().position(|&a| a as i32 == sel_abs));
            let node = keyed(fresh_ui::ComponentExt::node(list), state_key(key));
            match visible_rows {
                Some(r) => node.h(Sizing::Cells(tree_rows(n as u32, *r))),
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
                    hover_popup_row: "",
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
            let surface = cx.surface.clone();
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
                        true => entry_row(&rows_src[i], &surface),
                        false => {
                            row_pieces(&rows_src[i], slot, &surface, &mine, at, Fill::ToRowEnd)
                        }
                    }
                }
            })
            .focusable(false)
            .scrollbar_when(cx.scrollbar_reveal)
            .scrollbar_theme(bar_ink())
            // The rows carry their own colours — a focused field paints its
            // own background band per row — so the list's row states must not
            // paint over them.
            .row_theme({
                let plain = cx.surface.to_string();
                move |_, _| plain.clone()
            });
            // "Selected" here means "where the caret is", which is what the
            // list reveals when it moves. That is the whole of the auto-clamp
            // the runtime did by hand.
            let list = list.selection(sel);
            let body = keyed(fresh_ui::ComponentExt::node(list), spec_state_key(spec))
                .h(Sizing::Cells(*rows as u16));
            match head {
                0 => body,
                _ => col().children([entry_row(&out.entries[0], &cx.surface), body]),
            }
        }
        // **The first of the five collected variants to stop being collected.**
        //
        // A `Dropdown` is a trigger row and, when it is up, a floating list —
        // and neither half needed the collector. What it needed was the
        // collector's *formatter* (`render_dropdown` for the `[value ▼]` row,
        // and the option rows the pop-over paints), which is a pure function
        // of the spec and the resolved state, and the two rules the collector
        // buried in its walk: where the selection and the open flag come from,
        // and which column the list drops under. Those are now
        // `kinds::dropdown`'s `resolve`, `popup_of` and `anchor_col`, called
        // from here and from the collector both, so there is one copy of each.
        //
        // What goes away is the round trip. `collected` ran the whole
        // immediate-mode render to get rows and byte ranges, threw the state it
        // wrote into a scratch map, and then rebuilt nodes from the cells — so
        // a press was resolved by matching a byte range in a row the painter
        // had produced. Here the trigger *is* the node the press lands on, and
        // the pop-over hangs off it.
        //
        // The state is still read from `cx.states` rather than held by the
        // element: `selected_index` is what the plugin is told about through
        // `dropdown_select`, so it is model state and belongs to the host. The
        // *open* flag is view state and should become the element's — that is
        // 2.1, and it needs the runtime to stop writing it in its own walk
        // first.
        WidgetSpec::Dropdown {
            options,
            selected_index,
            label,
            focused,
            label_width,
            open,
            scroll_offset,
            key,
        } => {
            use crate::widgets::kinds::dropdown as dd;
            let key = key.as_deref();
            // A keyed widget takes focus from the host's resolved focus key; an
            // unkeyed one falls back to the spec's initial-only `focused` hint.
            let is_focused = match key.is_some_and(|k| !k.is_empty()) {
                true => cx.is_focused(key),
                false => *focused,
            };
            let st = dd::resolve(options, *selected_index, *open, key, cx.states, is_focused);
            let rendered = crate::widgets::render_dropdown(
                options,
                st.selected,
                label,
                is_focused,
                *label_width,
                st.open,
                *scroll_offset,
                cx.marker_gutter,
            );
            let widget_key = key.unwrap_or("").to_string();
            // A click on the `[value ▼]` button toggles the option list; a
            // click on the label does not. The hit is the button's range, and
            // `deliver_widget_hit` does the rest — the kind's `on_pointer`
            // owns both the open flag and the index, so the plugin never sees
            // this raw.
            let trigger = entry_row_hit(
                &rendered.entry,
                rendered.button_range,
                cx.slot,
                &cx.surface,
                crate::widgets::HitArea {
                    row_target: false,
                    context_click: false,
                    overlay: false,
                    widget_key: widget_key.clone(),
                    widget_kind: "dropdown",
                    buffer_row: 0,
                    byte_start: rendered.button_range.0,
                    byte_end: rendered.button_range.1,
                    payload: serde_json::json!({}),
                    event_type: "dropdown_toggle",
                    owner_key: None,
                },
            );
            if !st.open {
                return trigger;
            }
            let popup = dd::popup_of(
                options,
                st.selected,
                rendered.scroll_offset as u32,
                &cx.hovered_popup_row,
                &widget_key,
                dd::anchor_col(&rendered.entry.text, rendered.button_range.0),
            );
            // **The row is named, not just the parent** — the same reason
            // `rows_with_hits` names it. The layer resolves to this rectangle
            // either way, but naming it says so to the *dismissal* as well: a
            // press on the trigger is a press on the thing the list belongs
            // to, so it closes the list once instead of dismissing it and
            // letting the trigger's own toggle re-open it in the same press.
            let anchor = dropdown_anchor_key(cx.slot, &widget_key);
            row().key(anchor.clone()).h(Sizing::Cells(1)).children([
                trigger,
                popup_layer(&popup, cx).anchor(fresh_ui::Anchor::Node(anchor)),
            ])
        }
        // **The single-line field stops going through the collector** — the
        // second of the five to leave it, after `Dropdown`.
        //
        // Only this half. The multi-line arm above owns the window and the
        // scrollbar but still asks `render_collected` for its rows, because a
        // text *area* is a wrapping engine and wrapping is not what this phase
        // is moving. A one-row field has nothing to wrap.
        //
        // A single-line field is one row and, when the plugin has pushed
        // candidates, a list floating under it — and neither half needed the
        // collector. What it needed was the collector's *formatter*
        // (`render_text_input` for the `label: [value]` cell, and the
        // separator / item / border rows the completion box paints), plus the
        // rules the collector had buried in its walk: where the value, cursor
        // and candidate list come from, how the label is padded into the form
        // column, how wide the value cell is left, where the focus-marker
        // gutter shifts everything, and how the candidate window is clamped.
        // Those are now `kinds::text`'s `resolve`, `single_line` and
        // `completion_popup`, called from here and from the collector both, so
        // there is one copy of each.
        //
        // What goes away is the round trip. `collected` ran the whole
        // immediate-mode render to get one row and its byte ranges, threw the
        // state it wrote into a scratch map, and rebuilt the node from the
        // cells — so a click that has to land on a *column* of the value was
        // resolved by matching a byte range in a row the painter had produced.
        //
        // The state is still read from `cx.states`: the value and the cursor
        // are what the plugin is told about through `text_change`, and the
        // candidate list is what it pushed through `SetCompletions`. Making
        // the field's own view state — the horizontal window — the element's
        // is 2.1, and it needs the runtime to stop writing it in its own walk
        // first. Until then the window `render_text_input` picks here is
        // *read*, never written back: the runtime's pass persists it, and this
        // description reads what that pass last left.
        WidgetSpec::Text {
            value,
            cursor_byte,
            focused,
            label,
            placeholder,
            rows,
            field_width,
            max_visible_chars,
            full_width,
            completions: _,
            completions_visible_rows,
            block_caret,
            sel_start,
            sel_end,
            label_width,
            read_only: _,
            // `markdown` only means anything to a multi-line field — the arm
            // above owns that — so a one-row field renders as input chrome
            // whatever it says.
            markdown: _,
            key,
        } if *rows <= 1 => {
            use crate::widgets::kinds::text as tx;
            let key = key.as_deref();
            // A keyed widget takes focus from the host's resolved focus key; an
            // unkeyed one falls back to the spec's initial-only `focused` hint.
            let is_focused = match key.is_some_and(|k| !k.is_empty()) {
                true => cx.is_focused(key),
                false => *focused,
            };
            let st = tx::resolve(value, *cursor_byte, false, key, cx.states);
            // **The window becomes the element's here.**
            //
            // Which character the value starts at is a fold — "move just far
            // enough that the caret shows" depends on where the window already
            // was — and it lived in the runtime's registry, written by its
            // walk at the width the *registry* recorded while this description
            // draws at the width layout gave. The two agree only while those
            // widths do. [`windowed`] gives it to the element, which is the
            // one party that renders it, at the one width that is real. The
            // registry's value seeds the cell on mount and is not read again.
            //
            // The candidate list's forward-only offset is the same shape and
            // is *not* moved: it still comes from `st`, because it is the
            // runtime's `SetCompletions` state that carries it. Named so the
            // remaining half is visible.
            let ed = st.editor.clone();
            let label_s = label.clone();
            let placeholder_s = placeholder.clone();
            let key_s = key.map(|k| k.to_string());
            let (fw, mvc, fullw, bc, sel, lw, gutter, w32) = (
                *field_width,
                *max_visible_chars,
                *full_width,
                *block_caret,
                (*sel_start, *sel_end),
                *label_width,
                cx.marker_gutter,
                width as u32,
            );
            let build_line = move |window: u32| {
                tx::single_line(
                    &ed,
                    window,
                    &label_s,
                    placeholder_s.as_deref(),
                    fw,
                    mvc,
                    fullw,
                    bc,
                    sel,
                    lw,
                    is_focused,
                    key_s.as_deref(),
                    gutter,
                    w32,
                )
            };
            // One hit or none — an unkeyed field emits none, because a hit
            // with no widget to name could not say what it focused — and the
            // caret's marker rides in the same split, exactly as
            // `rows_with_hits` places it: the `block_caret` overlay is already
            // on the entry, and this is the *cell* the host drops a hardware
            // cursor into.
            let field = {
                let (slot, surface) = (cx.slot, cx.surface.clone());
                windowed(
                    state_key(&key.map(|k| k.to_string())),
                    st.scroll,
                    move |window| {
                        let line = build_line(window);
                        let next = line.scroll;
                        let hits: Vec<((usize, usize), crate::widgets::HitArea)> = line
                            .hit
                            .into_iter()
                            .map(|h| ((h.byte_start, h.byte_end), h))
                            .collect();
                        let node = match hits.is_empty() && line.caret.is_none() {
                            true => entry_row(&line.entry, &surface),
                            false => row_pieces(
                                &line.entry,
                                slot,
                                &surface,
                                &hits,
                                line.caret,
                                Fill::ToRowEnd,
                            ),
                        };
                        (node, next)
                    },
                )
            };
            let Some(popup) = tx::completion_popup(
                &st.completions,
                *completions_visible_rows,
                width as u32,
                st.completion_index,
                st.completion_navigated,
                st.completion_scroll,
                cx.marker_gutter,
            ) else {
                return field;
            };
            // **The candidate list is one float, not one float per row.**
            //
            // The collector emits it as N+2 *overlay rows* and
            // `rows_with_hits` gives each its own layer, which is the shape an
            // immediate-mode renderer can express — a row is all it has. A box
            // that clamps row by row is not the same box: near the frame's
            // bottom edge each row clamps independently and the list piles up
            // on the last one. One layer holding the rows clamps as a unit,
            // which is what a floating box means, and it is the shape the
            // described `Dropdown`'s pop-over already has.
            //
            // It is *not* [`popup_layer`], though, because this box's chrome
            // is not that box's. A completion list has no top border of its
            // own: its first row is a dashed separator that paints *over* the
            // enclosing `LabeledSection`'s bottom border, so the two read as
            // one frame — and its side walls and its scrollbar column live in
            // the row text itself (`render_completion_item_overlay`), not in a
            // `Draw::Border`. Wrapping it in a bordered box would draw a
            // second frame around a frame. Nor does it take
            // `Dismiss::OUTSIDE_POINTER`: a completion list is closed by
            // Escape (`Text::on_key`) or by the plugin sending an empty list,
            // never by a press landing elsewhere.
            //
            // The rows are `panel_width + 4` wide because they re-add the
            // section chrome they paint over, so the float starts `escape`
            // columns left of the child — see [`Site::escape`].
            let ground = Ink::new(Paint::key(BASE_FG), Paint::key("ui.popup_bg")).to_string();
            let box_rows: Vec<Node<UiMsg>> = popup
                .rows
                .iter()
                .map(|e| {
                    row()
                        .h(Sizing::Cells(1))
                        .theme(ground.clone())
                        .child(entry_row(e, &cx.surface))
                })
                .collect();
            fresh_ui::stack().children([
                field,
                fresh_ui::layer()
                    .anchor(fresh_ui::Anchor::Parent)
                    .place(fresh_ui::Place::Over)
                    // Row 1 of the sub-render: directly under the input, where
                    // the section's bottom border is.
                    .offset(-(site.escape as i16), 1)
                    .fit(fresh_ui::Fit::CLAMP)
                    .child(float_route(col().children(box_rows), cx.slot)),
            ])
        }
        // **The dual list stops going through the collector** — the third of
        // the five, after `Dropdown` and the single-line field.
        //
        // It never scrolled, which is why it crossed the coverage boundary
        // early and then sat behind the adapter anyway: it emits every row it
        // has, so there was no bar to lose and nothing but the round trip
        // keeping it there.
        //
        // The rules the collector buried in its walk are now
        // `kinds::dual_list`'s `resolve`, `header_row`, `body_row`,
        // `label_row` and `hint_row`, called from the collector and from here
        // both. `resolve` is the interesting one: which column the keyboard
        // drives and where each cursor sits survive in instance state, the
        // included set is sanitized against *this* spec's options, and each
        // cursor is clamped into its own column — because the option set can
        // change under a stored state and a cursor left past the end of a
        // shortened column would mark a row that is no longer there.
        //
        // **Each row carries two press targets, and the ranges come back with
        // the row.** A dual list's row is two cells side by side, each its own
        // `dual_focus` target, and `Some` means exactly what the collector's
        // `is_some()` guard meant: a value occupies the cell, so it can be
        // pressed. Handing the ranges back with the row is the point — a
        // caller given only the rendered text would have to measure it to find
        // where the columns are, and re-deriving geometry from painted output
        // is the duplication this migration exists to remove.
        WidgetSpec::DualList {
            options,
            included,
            excluded,
            label,
            focused,
            active_included,
            available_cursor,
            included_cursor,
            hint,
            visible_rows,
            key,
        } => {
            use crate::widgets::kinds::dual_list as dl;
            let key = key.as_deref();
            // A keyed widget takes focus from the host's resolved focus key; an
            // unkeyed one falls back to the spec's initial-only `focused` hint.
            let is_focused = match key.is_some_and(|k| !k.is_empty()) {
                true => cx.is_focused(key),
                false => *focused,
            };
            let st = dl::resolve(
                options,
                &dl::DualListSeed {
                    included,
                    excluded,
                    active_included: *active_included,
                    available_cursor: *available_cursor as usize,
                    included_cursor: *included_cursor as usize,
                },
                key,
                cx.states,
                is_focused,
            );
            let col_w = crate::widgets::render::dual_col_width(width as u32);
            let widget_key = key.unwrap_or("").to_string();
            let mut kids: Vec<Node<UiMsg>> = Vec::new();
            if let Some(e) = dl::label_row(label) {
                kids.push(entry_row(&e, &cx.surface));
            }
            kids.push(entry_row(&dl::header_row(&st, col_w), &cx.surface));
            for i in 0..st.body_rows(*visible_rows) {
                let r = dl::body_row(options, &st, i, col_w);
                let hits: Vec<((usize, usize), crate::widgets::HitArea)> =
                    [(r.available, "available"), (r.included, "included")]
                        .into_iter()
                        .filter_map(|(range, column)| {
                            let (byte_start, byte_end) = range?;
                            Some((
                                (byte_start, byte_end),
                                crate::widgets::HitArea {
                                    row_target: false,
                                    context_click: false,
                                    overlay: false,
                                    widget_key: widget_key.clone(),
                                    widget_kind: "dual_list",
                                    buffer_row: 0,
                                    byte_start,
                                    byte_end,
                                    payload: serde_json::json!({
                                        "column": column,
                                        "index": i,
                                    }),
                                    event_type: "dual_focus",
                                    owner_key: None,
                                },
                            ))
                        })
                        .collect();
                kids.push(match hits.is_empty() {
                    true => entry_row(&r.entry, &cx.surface),
                    false => entry_row_hits(&r.entry, cx.slot, &cx.surface, &hits),
                });
            }
            if let Some(e) = dl::hint_row(hint) {
                kids.push(entry_row(&e, &cx.surface));
            }
            col().children(kids)
        }
        // **A hole in the panel, at the rectangle layout gives it.**
        //
        // The runtime reserved this by emitting `rows` blank lines the width
        // of the panel and then *overlaying* the window's own paint on top of
        // them afterwards, from a rectangle it reconstructed out of the
        // panel's inner area plus the row and column the blanks landed on.
        // That is the reconstruct-from-paint shape this migration exists to
        // remove, and here it is simply a leaf: the description says how tall
        // the hole is, layout says where it is, and the fold hands the window
        // painter the rectangle it produced.
        //
        // Width is the panel's, which is what the blanks said too. Height is
        // the spec's `rows`, unchanged — a plugin sizes its own embed.
        WidgetSpec::WindowEmbed {
            window_id, rows, ..
        } => fresh_ui::host(super::frame::embed_host_id(*window_id))
            .h(Sizing::Cells((*rows).min(u16::MAX as u32) as u16)),
        // `covered` gates this; reaching it is a bug in the caller rather than
        // a spec the plugin got wrong, so it is loud in debug and empty in
        // release rather than silently dropping a panel's content.
        #[allow(unreachable_patterns)]
        other => {
            debug_assert!(false, "widget variant not covered: {other:?}");
            row().h(Sizing::Cells(0))
        }
    }
}

/// One row of the dock's active card, with its right edge opened.
///
/// **The card keeps its own light glyphs; what changes is the last one.** A
/// border row's closing corner becomes another `─`, so the rule runs on to
/// where the wall is and [`tab_scoop`] turns it back with `╯` above and `╮`
/// below; a content row's closing `│` becomes a space, so the row flows into
/// the editor with no wall between them. The left border, the text and every
/// overlay on it are untouched — and so are the byte offsets, because the
/// glyph replaced is the row's last.
///
/// The emphasis is [`crate::widgets::render::mark_list_card_selected`]'s,
/// minus its glyph swap: the heavy frame is a marker for a card that has
/// nothing else to say selection with, and the tab says it by *shape*, which
/// no theme can wash out either.
fn open_card_edge(entry: &mut TextPropertyEntry) {
    let end = entry.text.trim_end_matches('\n').len();
    if let Some(edge) = entry.text[..end].chars().next_back() {
        let open = match edge {
            '╮' | '╯' => Some('─'),
            '│' => Some(' '),
            _ => None,
        };
        if let Some(open) = open {
            entry
                .text
                .replace_range(end - edge.len_utf8()..end, open.encode_utf8(&mut [0u8; 4]));
        }
    }
    let mut st = entry.style.clone().unwrap_or_default();
    st.bold = true;
    // `trim_start`: tree cards indent nested rows by depth, so the border
    // glyph may sit after leading spaces.
    let head = entry.text.trim_start();
    if head.starts_with('╭') || head.starts_with('╰') {
        // A pure border row, so a whole-row fg tints the whole rule.
        st.fg = Some(OverlayColorSpec::theme_key("ui.popup_border_fg"));
        entry.style = Some(st);
        return;
    }
    // A content row holds the session's text after the left border. A
    // whole-row fg would repaint that text, so only the border glyph is
    // tinted — and there is only one of them now.
    entry.style = Some(st);
    if let Some(bar) = entry.text.find('│') {
        entry
            .inline_overlays
            .push(fresh_core::text_property::InlineOverlay {
                start: bar,
                end: bar + '│'.len_utf8(),
                style: OverlayOptions {
                    fg: Some(OverlayColorSpec::theme_key("ui.popup_border_fg")),
                    bold: true,
                    ..Default::default()
                },
                properties: Default::default(),
                unit: fresh_core::text_property::OffsetUnit::Byte,
            });
    }
}

/// The dock's divider, interrupted across the active card's rows: `╯` where
/// the card's top rule meets it, spaces beside the card's open rows, `╮`
/// where its bottom rule does.
///
/// **The band never travels.** The two halves of the seamless tab are the
/// card's rows ([`open_card_edge`]) and this, and what they have to agree
/// about is *which rows* — a fact that was a screen band in the painter, read
/// back off the cells it had just written. Here the scoop is declared by the
/// card itself and anchored to the card's own block, so layout answers where
/// the band is, in the same frame that drew it: [`fresh_ui::Anchor::Node`] on
/// the key the block already carries, placed `RightOf` it, which is the
/// column's last cell because the block is as wide as the panel's rows.
///
/// **A layer, because the divider is drawn under it.** `dock::grip_ink` draws
/// one `│` per row of the whole column and does not know a card is there;
/// this is out of flow, so it paints above that column and takes those cells
/// back. It claims no pointer: the cell it covers is still the width grip's
/// to drag.
///
/// `start` and `rows` are the block's own place in the tree's content, and
/// they are here for the one thing layout will not do: a card scrolled half
/// out of the window still *has* a rectangle, and a scoop placed against it
/// would land on the toolbar above the list or the column below it. The
/// enclosing viewport's window is what says whether the whole card is on
/// screen, and inside the viewport it is there for the asking — this is the
/// guard the painter spelled as "only when both border rows survived".
fn tab_scoop(block: fresh_ui::Key, start: u32, rows: u32, surface: &Ink) -> Node<UiMsg> {
    let ink = surface
        .clone()
        .with_fg(Paint::key("ui.popup_border_fg"))
        .to_string();
    fresh_ui::layout_reader(move |i: fresh_ui::LayoutInfo| {
        let hidden = || row().h(Sizing::Cells(0));
        // Three rows at the least: a rule, something between, a rule.
        let Some(win) = i.scroll_window.filter(|_| rows >= 3) else {
            return hidden();
        };
        let (top, bottom) = (i64::from(start), i64::from(start + rows));
        let (win_top, win_bottom) = (i64::from(win.y), i64::from(win.y) + i64::from(win.h));
        if top < win_top || bottom > win_bottom {
            return hidden();
        }
        let ink = ink.clone();
        fresh_ui::layer()
            .anchor(fresh_ui::Anchor::Node(block.clone()))
            .place(fresh_ui::Place::RightOf)
            .pointer_mode(fresh_ui::PointerMode::Ignore)
            .child(col().children((0..rows).map(|r| {
                let glyph = match r {
                    0 => "╯",
                    r if r + 1 == rows => "╮",
                    _ => " ",
                };
                fresh_ui::text(glyph)
                    .theme(ink.clone())
                    .w(Sizing::Cells(1))
                    .h(Sizing::Cells(1))
            })))
    })
    .h(Sizing::Cells(0))
}

/// One addressable run of rows inside a [`Scrolled`]: a card tree's node, or
/// a single line of a text area. `start` is where it begins in the content.
struct Chunk {
    key: fresh_ui::Key,
    /// First row of this block within the whole tree's rows.
    start: u32,
    rows: Vec<Node<UiMsg>>,
    /// What this block does to the column's right edge, if anything: the
    /// dock's active card scoops the divider away across its own rows (see
    /// [`tab_scoop`]). Out of flow, so it is not one of `rows` — the row
    /// count is what `start` and the reveal are counted in.
    edge: Option<Node<UiMsg>>,
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
    /// See [`Ctx::scrollbar_reveal`]. Carried rather than read from a context
    /// because a component builds outside the one that described it.
    reveal: Option<bool>,
}

/// **A fold the element owns**, for a builder that computes one while it
/// renders.
///
/// Most of what the widget runtime kept in its instance-state map is a
/// *derivation* — a clamp, a sanitize, a focus gate — recomputed identically on
/// every read, so storing it only made the render walk a second writer. Those
/// have been deleted. What is left is the other kind: a value whose own
/// previous value is an input. "Move the window just far enough that the caret
/// is visible" answers differently for the same caret depending on where the
/// window already sat, so it cannot be recomputed and has to be remembered.
///
/// A component's state is the right home for it, and the argument is the one
/// `fresh_ui::widgets::FieldState` already makes about a caret: it exists
/// exactly as long as the thing is on screen. Held instead in a host-side map,
/// the fold is written by whoever renders first at whatever width *they* were
/// working at, and read by whoever renders second — which for a text field
/// meant the runtime deciding the window at the width its registry recorded
/// while the description drew it at the width layout gave.
///
/// `build` is handed the current value and returns its node together with the
/// value this render chose. `seed` is used once, on mount.
fn windowed(
    key: Option<fresh_ui::Key>,
    seed: u32,
    build: impl Fn(u32) -> (Node<UiMsg>, u32) + 'static,
) -> Node<UiMsg> {
    struct Windowed {
        seed: u32,
        build: std::rc::Rc<dyn Fn(u32) -> (Node<UiMsg>, u32)>,
    }
    impl fresh_ui::Component<UiMsg> for Windowed {
        type State = std::cell::Cell<u32>;

        fn init(&self, _cx: &mut fresh_ui::InitCx<'_, UiMsg>) -> std::cell::Cell<u32> {
            std::cell::Cell::new(self.seed)
        }

        /// **Written during the build, through a `Cell`.**
        ///
        /// `build` takes `&Self::State`, which is the right signature: a
        /// component's state is not the reconciler's to mutate mid-build, and
        /// nothing here asks it to. This is the component's own scratch — no
        /// reconciliation decision reads it, and `memo` compares props — so
        /// interior mutability is the whole mechanism, and no library change
        /// was needed to own a fold.
        fn build(
            &self,
            w: &std::cell::Cell<u32>,
            _cx: &mut fresh_ui::BuildCx<'_, UiMsg>,
        ) -> Node<UiMsg> {
            let (node, next) = (self.build)(w.get());
            w.set(next);
            node
        }
    }
    let n = fresh_ui::ComponentExt::node(Windowed {
        seed,
        build: std::rc::Rc::new(build),
    });
    keyed(n, key)
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
            let block = col().key(b.key.clone()).children(b.rows.iter().cloned());
            content = content.child(match b.edge.clone() {
                Some(e) => block.child(e),
                None => block,
            });
        }
        let body = fresh_ui::viewport(content)
            .scrollbar_when(self.reveal)
            .scrollbar_theme(bar_ink());
        match s.anchor.clone() {
            Some(a) => body.anchor_to(a),
            None => body,
        }
    }
}

/// Send a float's wheel and hover where the rows behind it would have sent
/// them.
///
/// **A layer is the first thing asked at a point, and a hit does not continue
/// past it.** So a pop-over or an overlay row that answers only presses does
/// not let a wheel *through* — it eats it, and the list under the pointer
/// stops scrolling the moment a float covers it. Every float in a panel owes
/// this, which is why it is one function: the same statement `shell::settings`
/// makes about its own box, for the same reason.
fn float_route(n: Node<UiMsg>, slot: Slot) -> Node<UiMsg> {
    let route = move |e: &fresh_ui::Event| -> Option<UiMsg> {
        e.stop();
        match (slot, e.kind) {
            // The modal owns the whole pointer channel and replays the event
            // it was handed, so a wheel and a hover both land where they did.
            (Slot::Floating, _) => Some(UiMsg::Ui(super::msg::UiFact::ModalPointer(
                super::modal::Slot::FloatingPanel,
            ))),
            // The dock is not a modal: its column answers each gesture with a
            // fact of its own, and the wheel is the one a float covers.
            (Slot::Dock, fresh_ui::GestureKind::Wheel) => {
                Some(UiMsg::Ui(super::msg::UiFact::DockScroll {
                    delta: e.delta,
                    x: e.pos.x.max(0) as u16,
                    y: e.pos.y.max(0) as u16,
                }))
            }
            (Slot::Dock, _) => None,
            // A pane-mounted panel's float covers the panel and nothing else:
            // there is no scrolling surface *behind* it to hand the notch on
            // to the way the dock's column takes it, and `e.stop()` above has
            // already kept it off the pane underneath. So the float swallows
            // it, which is what no fact means here.
            (Slot::Pane(_), _) => None,
            // The settings dialog is a modal too, and its own box already
            // routes everything the tree does not answer for to that slot.
            (Slot::Settings | Slot::SettingsEntry, _) => Some(UiMsg::Ui(
                super::msg::UiFact::ModalPointer(super::modal::Slot::Settings),
            )),
        }
    };
    let mut n = fresh_ui::gesture(n);
    for kind in [fresh_ui::GestureKind::Wheel, fresh_ui::GestureKind::Move] {
        n = n.on(kind, std::rc::Rc::new(route));
    }
    n
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
/// The row an open pop-over hangs off, by name.
///
/// Keyed per slot and per row because two panels can each have a list open,
/// and a key that collided would anchor one to the other's trigger.
/// The anchor a *described* `Dropdown`'s pop-over hangs off.
///
/// [`popup_anchor_key`] names a pop-over by the collector row it was emitted
/// at, which is what distinguishes two of them in one panel on that path. A
/// described dropdown has no row index — it is a node, built from its spec
/// alone — so it is named by the thing that actually identifies it: its widget
/// key. Two dropdowns in one panel therefore anchor apart whenever the plugin
/// keyed them, which is the same condition under which they can hold separate
/// instance state at all; an unkeyed pair shares the collector path's row-0
/// name, and shares its open flag too, so there is nothing further to tell
/// apart.
fn dropdown_anchor_key(slot: Slot, widget_key: &str) -> fresh_ui::Key {
    if widget_key.is_empty() {
        return popup_anchor_key(slot, 0);
    }
    let scope = match slot {
        Slot::Dock => "dock".to_string(),
        Slot::Floating => "floating".to_string(),
        Slot::Settings => "settings".to_string(),
        Slot::SettingsEntry => "settings_entry".to_string(),
        Slot::Pane(leaf) => format!("pane:{}", leaf.0 .0),
    };
    fresh_ui::Key::Str(format!("widget_dropdown_anchor:{scope}:{widget_key}").into())
}

fn popup_anchor_key(slot: Slot, row: usize) -> fresh_ui::Key {
    let tag = match slot {
        Slot::Dock => "widget_popup_anchor:dock",
        Slot::Floating => "widget_popup_anchor:floating",
        Slot::Settings => "widget_popup_anchor:settings",
        Slot::SettingsEntry => "widget_popup_anchor:settings_entry",
        // A pane's tag carries its leaf, because two panes *can* each hold an
        // open pop-over: a mounted panel is a subtree now, and a `Dropdown`
        // inside one raises the same layer the dock's does.
        Slot::Pane(leaf) => {
            return fresh_ui::Key::Pair(
                format!("widget_popup_anchor:pane:{}", leaf.0 .0).into(),
                row as u64,
            )
        }
    };
    fresh_ui::Key::Pair(tag.into(), row as u64)
}

fn popup_layer(p: &crate::widgets::PanelPopup, cx: &Ctx<'_>) -> Node<UiMsg> {
    // **A float sits on its own ground, not on its trigger's.** `cx.surface`
    // is whatever the widget that opened this is standing on — and in the
    // settings dialog that is the selected card's *band*, so every option in
    // the list came out painted in the selection colour and the one actually
    // selected was indistinguishable from the rest. The box names its ground
    // one line below; the rows are built from the same.
    let ground = Ink::new(Paint::key(BASE_FG), Paint::key("ui.popup_bg"));
    fn hover_row(slot: Slot, index: Option<usize>) -> fresh_ui::Handler<UiMsg> {
        std::rc::Rc::new(move |_: &fresh_ui::Event| {
            Some(UiMsg::Ui(super::msg::UiFact::WidgetPopupHover {
                slot,
                index,
            }))
        })
    }
    let rows: Vec<Node<UiMsg>> = p
        .entries
        .iter()
        .enumerate()
        .map(|(i, e)| match p.row_indices.get(i) {
            Some(idx) => entry_row_hit_boxed(
                e,
                (0, e.text.len()),
                cx.slot,
                &ground,
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
            None => entry_row(e, &ground),
        })
        // The row's own hover, which nothing else can report: the runtime's
        // probe walks the panel's entries and a pop-over floats beside them.
        // See `UiFact::WidgetPopupHover`. A `gesture` of its own rather than
        // listeners on the piece the hit produced, because a row split into
        // several hit pieces is a plain `row` and only a gesture node listens.
        .enumerate()
        .map(|(i, n)| {
            fresh_ui::gesture(n)
                .on_enter(hover_row(cx.slot, p.row_indices.get(i).copied()))
                .on_leave(hover_row(cx.slot, None))
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
    let box_node = float_route(box_node, cx.slot);
    let slot = cx.slot;
    fresh_ui::layer()
        .anchor(fresh_ui::Anchor::Parent)
        .place(fresh_ui::Place::Below)
        .offset(p.anchor_col as i16, 0)
        .fit(fresh_ui::Fit::FLIP.or(fresh_ui::Fit::CLAMP))
        // **When it closes is the layer's to say.** A press outside an open
        // option list is spent closing it — that *is* the gesture, and the
        // list was in the way of it — and Escape means the same thing. Written
        // by hand it was written once (the settings dialog's own outside-click
        // rule) and missed once (the panel runtime had none, so a press inside
        // the dock but outside its dropdown left the list up).
        .dismiss(fresh_ui::Dismiss::OUTSIDE_POINTER.or(fresh_ui::Dismiss::ESCAPE))
        .on_dismiss(move |_| UiMsg::Ui(super::msg::UiFact::WidgetPopupDismiss { slot }))
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
    // **And the hover, for the same reason.** A widget's rectangle is what
    // answers "is the pointer on it", and this is the one wrapper every hit
    // piece goes through — a button, a list row, each of a tree row's three
    // targets — so one pair of listeners here is the whole vocabulary. What it
    // replaces is `update_widget_hover`: a second layout of the panel's spec
    // per motion event, hit-tested against the boxes it produced, followed by
    // a re-render request to the plugin.
    //
    // Only the panels. The settings dialog's rows carry their own hover facts
    // (`SettingsItemHover` and its siblings) onto settings state, and its
    // `Ctx` has no `hovered_key` to read, so a fact from here would be noise.
    let hover = matches!(slot, Slot::Dock | Slot::Floating).then(|| {
        let (widget, item) = (
            hit.widget_key.clone(),
            hit.payload
                .get("key")
                .and_then(|v| v.as_str())
                .unwrap_or_default()
                .to_string(),
        );
        move |entered: bool| {
            let (slot, widget, item) = (slot, widget.clone(), item.clone());
            std::rc::Rc::new(move |_: &fresh_ui::Event| {
                Some(UiMsg::Ui(super::msg::UiFact::WidgetHover {
                    slot,
                    widget: widget.clone(),
                    item: item.clone(),
                    entered,
                }))
            }) as fresh_ui::Handler<UiMsg>
        }
    });
    let n = fresh_ui::gesture(n).on(
        fresh_ui::GestureKind::Press,
        std::rc::Rc::new(move |e: &fresh_ui::Event| match e.button {
            fresh_ui::MouseButton::Left => {
                e.stop();
                Some(UiMsg::Ui(super::msg::UiFact::WidgetHit {
                    slot,
                    hit: hit.clone(),
                    at: Some(e.local.x.max(0) as u16),
                    clicks: e.clicks,
                }))
            }
            // **Only the kinds that declared the capability.** A right press
            // on a button or on padding is not this widget's, so it is left
            // unclaimed and reaches the column behind — which is where a
            // right press with no widget under it has always gone.
            fresh_ui::MouseButton::Right if hit.context_click => {
                e.stop();
                Some(UiMsg::Ui(super::msg::UiFact::WidgetContext {
                    slot,
                    hit: hit.clone(),
                    x: e.pos.x.max(0) as u16,
                    y: e.pos.y.max(0) as u16,
                }))
            }
            _ => None,
        }),
    );
    match hover {
        None => n,
        Some(h) => n.on_enter(h(true)).on_leave(h(false)),
    }
}

/// The ground an entry asks to keep past the end of its text, if it asks.
///
/// **`extend_to_line_end` is a word the ink grammar does not have.** The
/// painter drew every widget row as a `Paragraph` over the whole row rect with
/// the entry's fill style, so an overlay carrying this flag coloured the
/// trailing cells too. A description's runs stop at the glyphs — there is no
/// rect behind them — so a hover band ended at the toggle's chip, a selected
/// dropdown option was highlighted over its word and not its padding, and a
/// selected folder header in the dock's card tree lit only its name.
///
/// Only a background can extend: a fill paints spaces, and a foreground on a
/// space is nothing. So this reports the ink of the last overlay that asks and
/// carries one, and the row wears it — under its own runs, which paint over it
/// exactly where they have glyphs.
fn extended_ground(entry: &TextPropertyEntry, base: &Ink) -> Option<Ink> {
    entry
        .inline_overlays
        .iter()
        .filter(|o| o.style.extend_to_line_end && o.style.bg.is_some())
        .next_back()
        .map(|o| ink_of(&o.style, base))
}

/// One styled row, from a `TextPropertyEntry`.
/// One styled row, from a `TextPropertyEntry`.
///
/// **The load-bearing helper**: most variants of the runtime end in an entry,
/// so most of them migrate through here. It is the span walk
/// `render_widget_entry_line` does — split at inline-overlay boundaries, merge
/// overlapping overlays per property in declaration order — with the theme
/// *names* kept instead of resolved colours, because the fold resolves them
/// and that is what makes the row inspectable and the web able to paint it.
pub fn entry_row(entry: &TextPropertyEntry, surface: &Ink) -> Node<UiMsg> {
    let n =
        text_runs(entry_runs(entry, &[], surface).into_iter().map(|(_, r)| r)).h(Sizing::Cells(1));
    match extended_ground(entry, surface) {
        // Flexed as well as themed: a fill only reaches the end of the line if
        // the node does, and "to the end of the line" is the whole claim.
        Some(ink) => row()
            .h(Sizing::Cells(1))
            .w(Sizing::Flex(1))
            .theme(ink.to_string())
            .child(n),
        None => n,
    }
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
    surface: &Ink,
    hit: crate::widgets::HitArea,
) -> Node<UiMsg> {
    entry_row_hits(entry, slot, surface, &[(range, hit)])
}

/// [`entry_row_hit`] for a row inside a box that hugs it — a pop-over's
/// options. See [`Fill`].
fn entry_row_hit_boxed(
    entry: &TextPropertyEntry,
    range: (usize, usize),
    slot: Slot,
    surface: &Ink,
    hit: crate::widgets::HitArea,
) -> Node<UiMsg> {
    row_pieces(entry, slot, surface, &[(range, hit)], None, Fill::ToText)
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
    surface: &Ink,
    hits: &[((usize, usize), crate::widgets::HitArea)],
) -> Node<UiMsg> {
    row_pieces(entry, slot, surface, hits, None, Fill::ToRowEnd)
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
///
/// **Keyed per surface, because more than one can carry a caret at once.**
/// A described panel emits this marker whenever *its own* focused field says
/// so, and that is plugin state rather than a claim on the keyboard: the
/// search panel mounted in a pane goes on reporting a focused query field
/// while you type in the pane above it. One key for all of them would let the
/// wrong surface's marker answer the lookup, and a hardware cursor would sit
/// in a panel nobody is editing.
pub fn caret_key(slot: Slot) -> fresh_ui::Key {
    let tag = match slot {
        Slot::Dock => "widget_caret:dock",
        Slot::Floating => "widget_caret:floating",
        Slot::Settings => "widget_caret:settings",
        Slot::SettingsEntry => "widget_caret:settings_entry",
        Slot::Pane(leaf) => {
            return fresh_ui::Key::Pair("widget_caret:pane".into(), leaf.0 .0 as u64)
        }
    };
    fresh_ui::Key::Str(tag.into())
}

/// Whether a row's hit reaches the end of the row it is drawn in.
///
/// **A row in a panel's flow fills; a row inside a box does not.** The panel's
/// rows are as wide as the panel, so a row-wide hit has to be stated past the
/// text or a click in the empty part of the row lands on nothing. A pop-over's
/// option rows are the opposite case: the box hugs them, and a row that asks
/// to fill makes the *box* grow to whatever it is offered — the whole frame
/// for the settings dialog's dropdown, which then clamps to column 0 and
/// stops looking like it belongs to its trigger at all.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Fill {
    /// The row spans its container; a row-wide hit spans it too.
    ToRowEnd,
    /// The row is the width of its text, and its container is the width of the
    /// row. Nothing to reach past.
    ToText,
}

/// One row split into its hit pieces, with the caret's marker at `caret` if it
/// falls on this row.
fn row_pieces(
    entry: &TextPropertyEntry,
    slot: Slot,
    surface: &Ink,
    hits: &[((usize, usize), crate::widgets::HitArea)],
    caret: Option<usize>,
    fill: Fill,
) -> Node<UiMsg> {
    let mut cuts: Vec<usize> = Vec::with_capacity(hits.len() * 2 + 1);
    for ((a, b), _) in hits {
        cuts.push(*a);
        cuts.push(*b);
    }
    cuts.extend(caret);
    let runs = entry_runs(entry, &cuts, surface);
    // Group consecutive runs by which hit covers them. A byte covered by two
    // ranges takes the first that names it, which is the order the collector
    // pushed them — the same precedence the byte-range scan had.
    let owner = |at: &std::ops::Range<usize>| -> Option<usize> {
        hits.iter()
            .position(|((a, b), _)| at.start >= *a && at.end <= *b && b > a)
    };
    let marker = || {
        row()
            .key(caret_key(slot))
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
    // **A row-wide hit has to be stated past the row's text.**
    //
    // A piece is a byte range, and past the last glyph there are no bytes —
    // so a row built only from its pieces answers for exactly as many columns
    // as it has characters. A compact dock row is a short name in a wide
    // column, and a right-click "past the end of its short text — where most
    // of a compact row's width is empty and where a user naturally aims"
    // (`dock_right_click_opens_context_menu_in_compact_mode`) landed on
    // nothing at all.
    //
    // The runtime answered this with a *second* resolver: `hit_test_row_aware`
    // tries an exact byte hit and falls back to `row_select_hit`, "the
    // row-body `select` hit of a list/tree row, regardless of column". The
    // description needs no fallback and no second pass — the row simply
    // extends, carrying the hit its `row_target` flag already declares.
    //
    // The last such hit wins, which is the collector's own precedence: "the
    // narrow targets are named before the row-wide one, so a byte inside the
    // glyph or the box belongs to it rather than to `select`".
    if fill == Fill::ToRowEnd {
        if let Some((_, h)) = hits.iter().rev().find(|(_, h)| h.row_target) {
            kids.push(hit_node(
                row().w(Sizing::Flex(1)).h(Sizing::Cells(1)),
                slot,
                h.clone(),
            ));
        }
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
fn entry_runs(
    entry: &TextPropertyEntry,
    extra: &[usize],
    surface: &Ink,
) -> Vec<(std::ops::Range<usize>, Run)> {
    let mut normalized = entry.clone();
    normalized.normalize_widths();
    let mut text = normalized.text.clone();
    while text.ends_with('\n') {
        text.pop();
    }

    let base = match normalized.style.as_ref() {
        Some(o) => ink_of(o, surface),
        None => surface.clone(),
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
/// painter does. An `Rgb` becomes a literal, which is the one thing in the
/// display list with no theme entry behind it and is honest about that (F.2).
///
/// **A `ThemeKey` here is a name a *plugin* chose**, so it becomes
/// [`Paint::Asked`] rather than [`Paint::Key`]: the editor's table is under no
/// obligation to know it, and one the theme has never had must not take the
/// run's other half down with it. `git_history.ts` colours commit hashes
/// `syntax.number`, which is not a theme key and never has been — the painter
/// left the row's own foreground in place, and this is that fallback said out
/// loud rather than left to an unset field.
fn ink_of(o: &OverlayOptions, under: &Ink) -> Ink {
    let paint = |c: &OverlayColorSpec, beneath: &Paint| match c {
        OverlayColorSpec::ThemeKey(k) => Paint::asked(k.clone(), beneath.clone()),
        OverlayColorSpec::Rgb(r, g, b) => Paint::Lit(ratatui::style::Color::Rgb(*r, *g, *b)),
    };
    let mut attrs = under.attrs;
    for (on, a) in [
        (o.bold, Attrs::BOLD),
        (o.italic, Attrs::ITALIC),
        (o.underline, Attrs::UNDERLINE),
        (o.strikethrough, Attrs::STRIKETHROUGH),
        // The block caret. Dropping it left a focused field's bracketed cell
        // empty while it was being typed into — see `Attrs::REVERSED`.
        (o.reversed, Attrs::REVERSED),
    ] {
        if on {
            attrs = attrs | a;
        }
    }
    Ink {
        fg: o
            .fg
            .as_ref()
            .map(|c| paint(c, &under.fg))
            .unwrap_or_else(|| under.fg.clone()),
        bg: o
            .bg
            .as_ref()
            .map(|c| paint(c, &under.bg))
            .unwrap_or_else(|| under.bg.clone()),
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
            hovered_popup_row: String::new(),
            avail_height: None,
            scrollbar_reveal: None,
            surface: panel_surface(),
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
                n.text.trim_end_matches('\n').trim_end().to_string()
            })
            .collect()
    }

    /// What the tree says, laid out at the same width: the text of each row of
    /// the display list, in paint order.
    fn tree_rows(spec: &WidgetSpec) -> Vec<String> {
        tree_text(spec, &cx())
    }

    /// **A title bar's tint spans its strip, and its button stays at the
    /// right edge.**
    ///
    /// The runtime collapses an inline-only `Row` into one entry and keeps the
    /// *leading* child's whole-entry style for the merged line, which is how
    /// the orchestrator dock tints its title bar and pins its `[×]`. Both
    /// halves have to survive the tree: the oracle is the runtime itself.
    #[test]
    fn a_title_rows_band_spans_it_and_its_button_stays_right() {
        use fresh_core::api::{OverlayColorSpec, OverlayOptions};
        let mut title = TextPropertyEntry::text("Orchestrator");
        title.style = Some(OverlayOptions {
            fg: Some(OverlayColorSpec::theme_key("ui.menu_fg")),
            bg: Some(OverlayColorSpec::theme_key("ui.menu_bg")),
            ..Default::default()
        });
        let spec = WidgetSpec::Row {
            children: vec![
                WidgetSpec::Raw {
                    entries: vec![title],
                    key: None,
                },
                WidgetSpec::Spacer {
                    cols: 0,
                    flex: true,
                    key: None,
                },
                WidgetSpec::Button {
                    label: "\u{d7}".into(),
                    focused: false,
                    intent: Default::default(),
                    key: Some("dock-close".into()),
                    disabled: false,
                    focusable: false,
                    bare: true,
                    full_width: false,
                    hover_style: None,
                },
            ],
            key: None,
            wrap: false,
        };
        assert_eq!(runtime_rows(&spec), tree_rows(&spec));
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
            assert_eq!(tree_rows(&spec), runtime_rows(&spec), "{label}");
        }
    }

    /// **A form's rows are the runtime's rows.** Every variant was asserted
    /// against `render_spec` on its own, and both of the faults that reached
    /// CI were in how they *compose*, where a one-variant case cannot look.
    ///
    /// A `flexSpacer()` inside a row asked the row to be as tall as
    /// everything left in the column above it — `Node::flex` sets both axes,
    /// and on a container's *cross* axis `Sizing::Flex` means "fill the
    /// extent" — so the New-Workspace form's tab row came out twenty-six
    /// cells tall and every field under it was laid out at zero height
    /// against the panel's bottom edge. A `spacer(0)` between two sections
    /// measured nothing, where `kinds::spacer` pushes one entry: a blank
    /// line. Neither is visible in a single widget and both move every row
    /// under them in a real panel.
    #[test]
    fn a_forms_rows_are_the_runtimes_rows() {
        let gap = |cols: u32, flex: bool| WidgetSpec::Spacer {
            cols,
            flex,
            key: None,
        };
        let field = |label: &str, body: &str| WidgetSpec::LabeledSection {
            label: label.into(),
            child: Box::new(WidgetSpec::Raw {
                entries: vec![raw(body)],
                key: None,
            }),
            width_pct: None,
            key: None,
        };
        let cases: Vec<(&str, WidgetSpec)> = vec![
            (
                "a flexible spacer between two runs",
                col_of(vec![
                    WidgetSpec::Row {
                        children: vec![
                            WidgetSpec::Raw {
                                entries: vec![raw("Run in:")],
                                key: None,
                            },
                            gap(0, true),
                            WidgetSpec::Raw {
                                entries: vec![raw("switch")],
                                key: None,
                            },
                        ],
                        wrap: false,
                        key: None,
                    },
                    WidgetSpec::Raw {
                        entries: vec![raw("under it")],
                        key: None,
                    },
                ]),
            ),
            (
                "fixed spacers between a column's sections",
                col_of(vec![
                    WidgetSpec::Raw {
                        entries: vec![raw("first")],
                        key: None,
                    },
                    gap(0, false),
                    WidgetSpec::Raw {
                        entries: vec![raw("second")],
                        key: None,
                    },
                    gap(4, false),
                    WidgetSpec::Raw {
                        entries: vec![raw("third")],
                        key: None,
                    },
                ]),
            ),
            (
                "two fields side by side in a row",
                col_of(vec![WidgetSpec::Row {
                    children: vec![field("Left", "one"), field("Right", "two")],
                    wrap: false,
                    key: None,
                }]),
            ),
            (
                "a form: a tab row, spacers, two fields and a button",
                col_of(vec![
                    WidgetSpec::Row {
                        children: vec![
                            WidgetSpec::Raw {
                                entries: vec![raw("Run in:")],
                                key: None,
                            },
                            gap(0, true),
                            WidgetSpec::Raw {
                                entries: vec![raw("switch")],
                                key: None,
                            },
                        ],
                        wrap: false,
                        key: None,
                    },
                    gap(0, false),
                    field("Host", "build-01"),
                    field("Remote Path", "/srv"),
                    gap(0, false),
                    button("Create", Some("create"), false, false),
                ]),
            ),
        ];
        // **Corners, and only corners, are normalised.** `fold::border` writes
        // the editor's plain set (`┌┐└┘`) and the widget runtime's
        // `LabeledSection` wrote the rounded one — a difference C.6 already
        // made on the panel's own frame, and a deliberate one: "a rounded set
        // would be a visible change on the first surface that migrates". It
        // is recorded here rather than asserted away, because everything else
        // about the frame — where it starts, how wide it is, where its legend
        // sits — is compared exactly.
        let plain = |rows: Vec<String>| -> Vec<String> {
            rows.into_iter()
                .map(|r| {
                    r.replace('╭', "┌")
                        .replace('╮', "┐")
                        .replace('╰', "└")
                        .replace('╯', "┘")
                })
                .collect()
        };
        for (label, spec) in cases {
            assert_eq!(
                plain(tree_rows(&spec)),
                plain(runtime_rows(&spec)),
                "{label}"
            );
        }
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

    /// The display list folded into a grid, and the grid read back as rows.
    ///
    /// **A tiny `fold_native`, and it has to be**: the runtime's unit is an
    /// entry — a line of text, spaces and box glyphs included — so anything
    /// less than a grid cannot see what it is being compared against. Joining
    /// a row's runs end to end cannot see the gap a `Spacer` opened between
    /// them; skipping a row nothing painted on cannot see a blank line, which
    /// is what a `Spacer` in a column *is*; and ignoring `Draw::Border`
    /// cannot see a `LabeledSection`'s frame at all.
    fn rows_of(ui: &Ui<UiMsg>) -> Vec<String> {
        const H: usize = 64;
        let mut grid: Vec<Vec<char>> = vec![vec![' '; WIDTH as usize]; H];
        let mut bottom: Option<usize> = None;
        let put =
            |grid: &mut Vec<Vec<char>>, bottom: &mut Option<usize>, x: i32, y: i32, c: char| {
                if x < 0 || y < 0 || y as usize >= H || x >= WIDTH as i32 {
                    return;
                }
                grid[y as usize][x as usize] = c;
                *bottom = Some(bottom.map_or(y as usize, |b: usize| b.max(y as usize)));
            };
        for item in ui.spec().in_flow() {
            let r = item.rect;
            match &item.draw {
                fresh_ui::Draw::Lines(lines) => {
                    for (i, l) in lines.iter().enumerate() {
                        for (j, c) in l.chars().enumerate() {
                            put(&mut grid, &mut bottom, r.x + j as i32, r.y + i as i32, c);
                        }
                    }
                }
                // The glyphs `fold::border` writes, so the two agree on what a
                // bordered node looks like — including which corner set, now
                // that a description can name one. A mirror that hard-coded
                // `┌┐└┘` would put square corners into the text a user copies
                // out of a rounded box.
                fresh_ui::Draw::Border(bs) if r.w >= 2 && r.h >= 2 => {
                    let (right, low) = (r.x + r.w as i32 - 1, r.y + r.h as i32 - 1);
                    let (h, v, tl, tr, br, bl) = bs.glyphs();
                    for x in r.x..=right {
                        put(&mut grid, &mut bottom, x, r.y, h);
                        put(&mut grid, &mut bottom, x, low, h);
                    }
                    for y in r.y..=low {
                        put(&mut grid, &mut bottom, r.x, y, v);
                        put(&mut grid, &mut bottom, right, y, v);
                    }
                    put(&mut grid, &mut bottom, r.x, r.y, tl);
                    put(&mut grid, &mut bottom, right, r.y, tr);
                    put(&mut grid, &mut bottom, r.x, low, bl);
                    put(&mut grid, &mut bottom, right, low, br);
                }
                _ => {}
            }
        }
        let Some(bottom) = bottom else {
            return Vec::new();
        };
        grid[..=bottom]
            .iter()
            .map(|r| r.iter().collect::<String>().trim_end().to_string())
            .collect()
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
            n.text.trim_end_matches('\n').trim_end().to_string()
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
        let UiFact::WidgetHit { slot, hit, .. } = got.first().expect("a hit") else {
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

    /// **A described single-line field says what the collector said.**
    ///
    /// It used to *be* the collector — this test was written when a `Text`
    /// went through `collected`, and its cases are unchanged. Now the row is
    /// built from the spec through `kinds::text::single_line`, and the runtime
    /// is the oracle it has to keep agreeing with: label column, value cell,
    /// focus gutter, placeholder and the horizontal window are all rules the
    /// description would otherwise have restated slightly wrong.
    #[test]
    fn a_single_line_field_renders_what_the_runtime_renders() {
        let field = |label: &str,
                     value: &str,
                     placeholder: Option<&str>,
                     field_width: u32,
                     full_width: bool,
                     label_width: u32| WidgetSpec::Text {
            value: value.into(),
            cursor_byte: -1,
            focused: false,
            label: label.into(),
            placeholder: placeholder.map(|p| p.into()),
            rows: 1,
            field_width,
            max_visible_chars: 0,
            full_width,
            completions: Vec::new(),
            completions_visible_rows: 0,
            block_caret: false,
            sel_start: -1,
            sel_end: -1,
            label_width,
            read_only: false,
            markdown: false,
            key: Some("t".into()),
        };
        let cases: Vec<(&str, WidgetSpec)> = vec![
            ("a text field", field("name", "hello", None, 12, false, 0)),
            ("unlabelled", field("", "hello", None, 12, false, 0)),
            // The form-column rule: the label is padded to `label_width` and
            // terminated with `:` so the `[` lines up with its siblings'.
            (
                "in a form column",
                field("name", "hello", None, 12, false, 10),
            ),
            // A label wider than the column it is given, which `fit_label`
            // has to cut rather than let overflow the control's right edge.
            (
                "a label wider than its column",
                field("a very long field label indeed", "hi", None, 12, false, 8),
            ),
            // `full_width` sizes the value cell against the *padded* label,
            // the brackets, the cursor park and the gutter reserve.
            ("full width", field("name", "hello", None, 0, true, 0)),
            (
                "full width in a form column",
                field("name", "hello", None, 0, true, 10),
            ),
            // Empty and unfocused is the placeholder's only state.
            (
                "a placeholder",
                field("path", "", Some("~/project"), 12, false, 0),
            ),
            // Longer than the cell: the horizontal window and its `…`.
            (
                "a value past the window",
                field(
                    "name",
                    "a value far wider than its cell",
                    None,
                    12,
                    false,
                    0,
                ),
            ),
        ];
        for (label, spec) in cases {
            assert_eq!(
                tree_text(&spec, &cx()),
                runtime_text(&spec, &cx()),
                "{label}"
            );
            // Focused, with the marker gutter the forms that want
            // capture-legible focus turn on: the `▸ ` is four bytes and two
            // columns, and every overlay on the row shifts by the bytes while
            // the cell shrinks by the columns. Getting either half wrong
            // moves the brackets.
            let marked = Ctx {
                focus_key: "t".into(),
                marker_gutter: true,
                ..cx()
            };
            assert_eq!(
                tree_text(&spec, &marked),
                runtime_text(&spec, &marked),
                "{label}, focused with the marker gutter"
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
    fn every_variant_is_covered() {
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
        ] {}

        // And the one that used to be excluded: `WindowEmbed` is a `Host`
        // leaf, which is what "paints its own cells" has meant everywhere else
        // in this migration. It was held out on the reading that cells and a
        // description are alternatives; they are not, and that exclusion was
        // the last thing keeping a whole second panel painter alive. There is
        // no coverage predicate left to assert it against — every variant is
        // described, so the gate has been deleted — and this case simply
        // renders, like the rest.
        let embed = WidgetSpec::WindowEmbed {
            window_id: 1,
            rows: 3,
            key: None,
        };
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(node(&embed, WIDTH, &cx()), Size::new(WIDTH, 24));
        assert!(
            ui.spec()
                .in_flow()
                .iter()
                .any(|i| matches!(i.draw, fresh_ui::Draw::Host(_))),
            "a window embed describes a host leaf for the fold to hand a rect"
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

    /// **A list's scroll follows its key, not its position.**
    ///
    /// Reconciliation matched these by position among their siblings and by
    /// component type, so a plugin that re-emitted its spec with one extra
    /// sibling above a list remounted it — dropping the scroll offset the
    /// element existed to hold — and two lists that swapped places swapped
    /// offsets with each other. Both are things plugins do freely.
    #[test]
    fn a_lists_scroll_survives_a_sibling_appearing_above_it() {
        let scrolled = |ui: &Ui<UiMsg>| {
            ui.spec()
                .in_flow()
                .iter()
                .filter_map(|i| match &i.draw {
                    fresh_ui::Draw::Lines(l) => l.first().map(|r| r.to_string()),
                    _ => None,
                })
                .find(|t| t.starts_with("row"))
                .expect("some row is on screen")
        };
        // Alone in a column, then with a sibling inserted above it. The list
        // is the same list both times, and says so with its key.
        let alone = WidgetSpec::Col {
            children: vec![a_list(50, 0, 5)],
            key: None,
        };
        let with_sibling = WidgetSpec::Col {
            children: vec![
                WidgetSpec::Raw {
                    entries: vec![raw("a new heading")],
                    key: None,
                },
                a_list(50, 0, 5),
            ],
            key: None,
        };
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(node(&alone, WIDTH, &cx()), Size::new(WIDTH, 24));
        assert_eq!(scrolled(&ui), "row0", "starts at the top");

        // Scroll it, so there is state worth losing.
        let at = ui.rect_of(
            ui.find_by_key(&fresh_ui::Key::Str("l".into()))
                .expect("the list is keyed"),
        );
        ui.dispatch(fresh_ui::Input::Wheel {
            pos: fresh_ui::Point::new(at.x + 1, at.y + 1),
            delta: 3,
            axis: fresh_ui::Axis::Vertical,
            mods: fresh_ui::Mods::NONE,
        });
        ui.frame(node(&alone, WIDTH, &cx()), Size::new(WIDTH, 24));
        let after_wheel = scrolled(&ui);
        assert_ne!(after_wheel, "row0", "the wheel moved the window");

        // Now the sibling appears. Same list, one position further down.
        ui.frame(node(&with_sibling, WIDTH, &cx()), Size::new(WIDTH, 24));
        assert_eq!(
            scrolled(&ui),
            after_wheel,
            "the list kept its window when a sibling appeared above it"
        );
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

    /// **A right press on a row is that row's, and it carries the screen
    /// cell.** The probe used to answer which row a right press belonged to,
    /// from a second layout of the same spec; the node has the rectangle, so
    /// it answers itself. The cell rides along because the plugin anchors its
    /// popup at the click and the payload carries only the row index.
    #[test]
    fn a_right_press_on_a_tree_row_raises_its_own_context_menu() {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(node(&a_tree(&["r"], 0), WIDTH, &cx()), Size::new(WIDTH, 24));
        let at = fresh_ui::Point::new(8, 2);
        let msgs = ui
            .dispatch(fresh_ui::Input::press(
                at,
                fresh_ui::MouseButton::Right,
                fresh_ui::Mods::NONE,
            ))
            .msgs;
        let (hit, x, y) = msgs
            .into_iter()
            .find_map(|m| match m {
                UiMsg::Ui(UiFact::WidgetContext { hit, x, y, .. }) => Some((hit, x, y)),
                _ => None,
            })
            .expect("a right press on a tree row");
        assert_eq!(hit.widget_kind, "tree");
        assert!(hit.context_click, "only a kind that declared it claims");
        assert_eq!((x, y), (8, 2), "the screen cell, not the widget's own");
    }

    /// And a widget that declared no context menu leaves the press alone, so
    /// it reaches the surface behind — which is where a right press with no
    /// widget under it has always gone.
    #[test]
    fn a_right_press_on_a_button_is_left_to_the_surface_behind_it() {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            node(&button("Go", Some("go"), false, false), WIDTH, &cx()),
            Size::new(WIDTH, 24),
        );
        let msgs = ui
            .dispatch(fresh_ui::Input::press(
                fresh_ui::Point::new(2, 0),
                fresh_ui::MouseButton::Right,
                fresh_ui::Mods::NONE,
            ))
            .msgs;
        assert!(
            !msgs
                .iter()
                .any(|m| matches!(m, UiMsg::Ui(UiFact::WidgetContext { .. }))),
            "a button raises no menu: {msgs:?}"
        );
    }

    /// **The hover the runtime probed for.** Entering a row names the widget
    /// *and* the row, because every row of one tree shares the tree's key;
    /// leaving names the same pair, so two pieces of one row can hand the
    /// hover between them without the leave undoing the enter.
    #[test]
    fn entering_a_tree_row_reports_it_and_leaving_gives_it_back() {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(node(&a_tree(&["r"], 0), WIDTH, &cx()), Size::new(WIDTH, 24));
        let hover = |ui: &mut Ui<UiMsg>, x: i32, y: i32| -> Vec<(String, String, bool)> {
            ui.dispatch(fresh_ui::Input::Move {
                pos: fresh_ui::Point::new(x, y),
                mods: fresh_ui::Mods::NONE,
            })
            .msgs
            .into_iter()
            .filter_map(|m| match m {
                UiMsg::Ui(UiFact::WidgetHover {
                    widget,
                    item,
                    entered,
                    ..
                }) => Some((widget, item, entered)),
                _ => None,
            })
            .collect()
        };

        let entered = hover(&mut ui, 8, 2);
        assert!(
            entered.iter().any(|(w, _, e)| w == "tr" && *e),
            "entering a row names the tree it belongs to: {entered:?}"
        );

        // Off every row: the same pair comes back, released.
        let left = hover(&mut ui, 8, 20);
        assert!(
            left.iter().any(|(w, _, e)| w == "tr" && !*e),
            "leaving names what it is leaving: {left:?}"
        );
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
            &panel_surface(),
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
        ui.frame(entry_row(&e, &panel_surface()), Size::new(WIDTH, 4));
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
                // Not `trim_end`: the pop-over pads every row to the widest
                // option, and that padding is where the selected row's
                // highlight reaches the box's edge.
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

    /// **The list opens under the trigger wherever the trigger is.**
    ///
    /// The collector path named a pop-over's anchor by the *row of the
    /// collector's own output* it was emitted at, which is only the panel's
    /// row because the collector rendered the whole panel. A described
    /// dropdown is a node built from its own spec and knows nothing about the
    /// rows above it, so it is named by its widget key and the column places
    /// it. With rows above it that distinction is visible: the box must drop
    /// under the trigger's row, not under row 0.
    #[test]
    fn the_pop_over_follows_its_trigger_down_the_panel() {
        let spec = col_of(vec![
            WidgetSpec::Raw {
                entries: vec![raw("above one")],
                key: None,
            },
            WidgetSpec::Raw {
                entries: vec![raw("above two")],
                key: None,
            },
            dropdown(&["fast", "slow", "off"], 0, true, 0),
        ]);
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(node(&spec, WIDTH, &cx()), Size::new(WIDTH, 24));
        let top = ui
            .spec()
            .layers()
            .iter()
            .map(|i| i.rect)
            .min_by_key(|r| (r.y, r.x))
            .expect("a pop-over");
        assert_eq!(
            top.y, 3,
            "two rows above, the trigger, then the list: {top:?}"
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

    /// The selected card is framed in heavy glyphs, wherever there is no wall
    /// for it to open onto: a background band "reads garish over a multi-row
    /// card", and the frame is a marker no theme can wash out. In the dock it
    /// is the seamless tab instead — see below.
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

    /// **In the dock the selected card is the seamless tab** (F.8): its rows
    /// keep the light box and lose the right border, so the active session's
    /// card flows into the editor mirroring it, and the column's divider is
    /// scooped away across exactly those rows — `╯` where the card's top rule
    /// meets it, `╮` where its bottom rule does.
    ///
    /// Both halves are asserted from one laid-out frame, because the whole
    /// point is that they agree without a band travelling between them: the
    /// scoop is anchored to the card's own block, so it is at the block's
    /// right edge and as tall as the block by construction rather than by
    /// arithmetic.
    #[test]
    fn the_docks_selected_card_opens_onto_the_editor() {
        let dock = Ctx {
            slot: Slot::Dock,
            ..cx()
        };
        let spec = card_tree(3, 1, 20);
        let rows = tree_text(&spec, &dock);
        assert!(
            !rows.iter().any(|r| r.contains('┏')),
            "the tab is the marker here, not a heavy frame: {rows:?}"
        );
        let open: Vec<&String> = rows
            .iter()
            .filter(|r| r.starts_with('╭') || r.starts_with('╰'))
            .collect();
        assert!(
            open.iter().any(|r| r.ends_with('─')),
            "the selected card's rules run on to the wall: {rows:?}"
        );
        assert!(
            open.iter().any(|r| r.ends_with('╮')) && open.iter().any(|r| r.ends_with('╯')),
            "and every other card still closes its box: {rows:?}"
        );

        // The scoop itself: one column wide, at the right edge of the card it
        // belongs to, top and bottom turning back into the divider.
        // The dock's own geometry: the panel is `WIDTH` wide and the column
        // has one more cell, which is the divider's.
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            node(&spec, WIDTH, &dock).w(Sizing::Cells(WIDTH)),
            Size::new(WIDTH + 1, 24),
        );
        let card = ui.rect_of(
            ui.find_by_key(&fresh_ui::Key::Str("s1".into()))
                .expect("the selected card's block"),
        );
        let scoop: Vec<(i32, i32, String)> = ui
            .spec()
            .layers()
            .iter()
            .filter_map(|i| match &i.draw {
                fresh_ui::Draw::Lines(l) => Some((i.rect.x, i.rect.y, l.concat())),
                _ => None,
            })
            .collect();
        assert!(
            scoop.iter().all(|(x, ..)| *x == card.x + card.w as i32),
            "the scoop sits in the column's last cell, past the card: {scoop:?} vs {card:?}"
        );
        let glyphs: Vec<&str> = scoop.iter().map(|(_, _, g)| g.as_str()).collect();
        assert_eq!(
            glyphs,
            vec!["╯", " ", " ", " ", "╮"],
            "the divider turns away above the card and back below it"
        );
        assert_eq!(
            (scoop.first().map(|(_, y, _)| *y), scoop.len()),
            (Some(card.y), card.h as usize),
            "across the card's rows and no others"
        );
    }

    /// …and only in the dock: everywhere else the card has no wall beside it,
    /// so nothing is scooped and the heavy frame stays the marker.
    #[test]
    fn no_other_surface_scoops_a_divider() {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            node(&card_tree(3, 1, 20), WIDTH, &cx()),
            Size::new(WIDTH, 24),
        );
        assert!(
            ui.spec().layers().is_empty(),
            "a floating panel's selected card declares no edge: {:?}",
            ui.spec().layers()
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
            ui.find_by_key(&caret_key(Slot::Floating))
                .map(|id| ui.rect_of(id).x)
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
        assert!(ui.find_by_key(&caret_key(Slot::Floating)).is_none());
    }

    /// **A press on the field carries the value's layout, not just its key.**
    ///
    /// Click-to-position-cursor (#2573) needs to translate a clicked *column*
    /// back to a byte of the value, and a windowed field is showing a tail
    /// view with an `…` in front of it — so the row's own bytes are not the
    /// value's. The four breadcrumbs are how the click handler bridges that,
    /// and they are measured against the row *after* the focus-marker gutter
    /// was prepended. The runtime's own hit is the oracle: the description has
    /// to name the same numbers or a click lands on the wrong character.
    #[test]
    fn a_field_press_carries_the_value_layout_the_click_handler_reads() {
        let spec = text_field("a value far wider than its cell", 0, true);
        let focused = Ctx {
            focus_key: "field".into(),
            marker_gutter: true,
            ..cx()
        };
        let want = crate::widgets::render_spec_with_options(
            &spec,
            &Default::default(),
            WIDTH as u32,
            crate::widgets::RenderOptions {
                prev_focus_key: "field",
                auto_focus_first: false,
                marker_gutter: true,
                ..Default::default()
            },
        )
        .hits
        .into_iter()
        .next()
        .expect("the runtime records one focus hit for a keyed field");
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(node(&spec, WIDTH, &focused), Size::new(WIDTH, 8));
        let got = facts(ui.dispatch(fresh_ui::Input::press(
            fresh_ui::Point::new(4, 0),
            fresh_ui::MouseButton::Left,
            fresh_ui::Mods::NONE,
        )));
        let UiFact::WidgetHit { hit, .. } = got.first().expect("a hit") else {
            panic!("expected a widget hit, got {got:?}");
        };
        assert_eq!(hit.event_type, "focus");
        assert_eq!(hit.widget_key, "field");
        assert_eq!(hit.payload, want.payload, "the value-layout breadcrumbs");
    }

    /// An unkeyed field answers nothing: `key.filter(|k| !k.is_empty())` gates
    /// the hit, because a hit with no widget to name could not say what it
    /// focused. The runtime records none either.
    #[test]
    fn an_unkeyed_field_answers_no_press() {
        let mut spec = text_field("hello", -1, false);
        if let WidgetSpec::Text { key, .. } = &mut spec {
            *key = None;
        }
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(node(&spec, WIDTH, &cx()), Size::new(WIDTH, 8));
        let got = facts(ui.dispatch(fresh_ui::Input::press(
            fresh_ui::Point::new(2, 0),
            fresh_ui::MouseButton::Left,
            fresh_ui::Mods::NONE,
        )));
        assert!(
            !got.iter().any(|f| matches!(f, UiFact::WidgetHit { .. })),
            "an unkeyed field emits no hit: {got:?}"
        );
    }

    /// A field's instance state carrying a completion list.
    ///
    /// There is no other way to have one: candidates never come from the spec
    /// — plugins push them through `SetCompletions` — so a described field
    /// reads them from the same state map the collector does.
    fn field_states(
        value: &str,
        items: &[&str],
        selected: usize,
        navigated: bool,
        scroll: u32,
    ) -> std::collections::HashMap<String, crate::widgets::WidgetInstanceState> {
        let mut editor = crate::primitives::text_edit::TextEdit::single_line_with_text(value);
        editor.set_cursor_from_flat(value.len());
        let mut m = std::collections::HashMap::new();
        m.insert(
            "field".to_string(),
            crate::widgets::WidgetInstanceState::Text {
                editor,
                scroll: 0,
                completions: items.iter().map(|s| (*s).into()).collect(),
                completion_selected_index: selected,
                completion_scroll_offset: scroll,
                completion_navigated: navigated,
                user_scrolled: false,
            },
        );
        m
    }

    /// A keyed single-line field inside the section that wraps a real form
    /// field, so the completion box's four columns of re-added chrome land on
    /// the section's own borders — which is the whole point of `Site::escape`.
    fn sectioned_field() -> WidgetSpec {
        WidgetSpec::LabeledSection {
            label: String::new(),
            child: Box::new(text_field("he", 2, true)),
            width_pct: None,
            key: None,
        }
    }

    /// **The completion list paints the rows the collector windowed.**
    ///
    /// Every rule about which candidates show — the forward-only scroll, the
    /// window clamp, the scrollbar thumb, the highlight that only appears once
    /// the user has stepped into the list — lives in `completion_popup`, which
    /// the collector calls too. So the runtime's own overlay rows are the
    /// oracle, and the box the tree floats has to be made of exactly them.
    #[test]
    fn the_completion_list_paints_the_rows_the_collector_windowed() {
        let items: Vec<String> = (0..9).map(|i| format!("candidate{i}")).collect();
        let refs: Vec<&str> = items.iter().map(|s| s.as_str()).collect();
        let states = field_states("he", &refs, 6, true, 0);
        let spec = sectioned_field();
        let ctx = Ctx {
            focus_key: "field".into(),
            states: Box::leak(Box::new(states.clone())),
            ..cx()
        };
        let want: Vec<String> = crate::widgets::render_spec_with_options(
            &spec,
            &states,
            WIDTH as u32,
            crate::widgets::RenderOptions {
                prev_focus_key: "field",
                auto_focus_first: false,
                ..Default::default()
            },
        )
        .overlays
        .iter()
        .map(|o| {
            let mut n = o.entry.clone();
            n.normalize_widths();
            n.text.trim_end_matches('\n').trim_end().to_string()
        })
        .collect();
        assert!(
            want.len() == 7,
            "five of nine candidates, a separator and a border: {want:?}"
        );
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(node(&spec, WIDTH, &ctx), Size::new(WIDTH, 24));
        assert_eq!(layer_rows(&ui), want);
    }

    /// **The list is one float, and it drops under the field wherever the
    /// field is.**
    ///
    /// The collector emits it as N+2 overlay *rows*, each of which
    /// `rows_with_hits` gives its own layer — a shape an immediate-mode
    /// renderer is stuck with, and one that clamps row by row near a frame
    /// edge. A described field floats the box whole, so its rows are
    /// contiguous and start one row under the input: the section's bottom
    /// border, which the dim separator is drawn to paint over.
    #[test]
    fn the_completion_list_opens_under_the_field_wherever_it_is() {
        let states = field_states("he", &["hello", "help", "hedge"], 0, false, 0);
        let spec = col_of(vec![
            WidgetSpec::Raw {
                entries: vec![raw("above one")],
                key: None,
            },
            WidgetSpec::Raw {
                entries: vec![raw("above two")],
                key: None,
            },
            sectioned_field(),
        ]);
        let ctx = Ctx {
            focus_key: "field".into(),
            states: Box::leak(Box::new(states)),
            ..cx()
        };
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(node(&spec, WIDTH, &ctx), Size::new(WIDTH, 24));
        let rects: Vec<fresh_ui::Rect> = ui.spec().layers().iter().map(|i| i.rect).collect();
        let top = rects
            .iter()
            .min_by_key(|r| (r.y, r.x))
            .copied()
            .expect("a completion box");
        // Two rows above, the section's top border, the input row: the
        // separator takes the row after it.
        assert_eq!(top.y, 4, "under the field, not under row 0: {rects:?}");
        // Four columns wider than the section's interior, which is what
        // "re-add section chrome" means — so it starts at the section's own
        // left border rather than two columns in.
        assert_eq!(top.x, 0, "flush with the section's frame: {rects:?}");
        assert!(
            rects.iter().all(|r| r.y >= top.y && r.y < top.y + 5),
            "one contiguous box, not a layer per row: {rects:?}"
        );
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

    /// **The interactive widgets are on the tree's ring, and the ring is asked
    /// rather than restated.**
    ///
    /// Eight kinds are focusable and two of them conditionally, and those rules
    /// live in each kind's `box_meta` — the same answer the runtime's own ring
    /// reads. The adapter asks it, so there is one copy; this pins that a
    /// keyed, focusable widget acquires focus and a decorative one does not,
    /// which is what would break if the rules were ever written out a second
    /// time and drifted.
    #[test]
    fn a_keyed_interactive_widget_is_on_the_focus_ring() {
        let field = WidgetSpec::Text {
            value: "hi".into(),
            cursor_byte: -1,
            focused: false,
            label: String::new(),
            placeholder: None,
            rows: 1,
            field_width: 8,
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
            key: Some("f".into()),
        };
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(node(&field, WIDTH, &cx()), Size::new(WIDTH, 8));
        assert!(
            ui.find_by_key(&fresh_ui::Key::Str("widget_focus:f".into()))
                .is_some(),
            "a keyed text field declares itself focusable"
        );

        // A divider has no key and no business holding focus. `box_meta` says
        // so; nothing here repeats the reason.
        let rule = WidgetSpec::Divider {
            ch: String::new(),
            style: None,
            key: None,
        };
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(node(&rule, WIDTH, &cx()), Size::new(WIDTH, 8));
        assert!(
            ui.spec().in_flow().iter().next().is_some_and(|_| ui
                .find_by_key(&fresh_ui::Key::Str("widget_focus:".into()))
                .is_none()),
            "a decorative widget stays off the ring"
        );
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
