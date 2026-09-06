//! Panel registry — maps a panel's composite identity (owning plugin,
//! plugin-local `panel_id`) to mounted spec and hit-area data for click
//! routing.
//!
//! The registry is the source of truth for "which panels exist, what
//! spec are they currently rendering, and which buffer rows belong
//! to which widget." It does *not* own the virtual buffer the
//! rendered output goes into — the plugin still owns the virtual
//! buffer and passes its `BufferId` at mount time.
//!
//! **Two types where there was one.** [`WidgetEvent`] is what a press
//! *means* — a pure function of the spec and the instance state, which is
//! why the three `synthesize_*_hit` functions can rebuild one from the spec
//! alone. [`HitArea`] is where the text projection *drew* one, in the rows of
//! a virtual buffer. They used to be one struct, and the surfaces that have
//! no such rows carried four fields they could not interpret — and
//! interpreted two of them anyway, adding a byte offset on at the press and
//! taking it off again at the dispatch. Splitting them is what lets a
//! described widget hand over an event with no coordinate space attached.

use crate::primitives::text_edit::TextEdit;
use fresh_core::api::WidgetSpec;
use fresh_core::BufferId;
use std::collections::{HashMap, HashSet};

/// Plugin-allocated panel identifier. Unique within a plugin; the
/// editor does not interpret the value.
pub type PanelId = u64;

/// Composite panel identity: panel ids are plugin-local, so the
/// registry key is (owning plugin, id). The owner is recorded host-side
/// at mount time from the calling plugin's identity — never trusted
/// from the JS side.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PanelKey {
    /// Name of the plugin that mounted the panel.
    pub plugin: String,
    /// The plugin-local panel id.
    pub id: PanelId,
}

/// The panel id of a plugin's floating-overlay prompt toolbar
/// (`editor.setPromptToolbar`). A plugin's own panel ids start at 1, so the
/// toolbar is the plugin's panel 0 — the id its toolbar `widget_event`s have
/// always carried — and it lives in the registry like any other panel of
/// that plugin's: its toggles' focus is the registry's fact, its presses and
/// keys are the same dispatch, and the tree describes it in the prompt card's
/// header band.
pub const PROMPT_TOOLBAR_PANEL_ID: PanelId = 0;

impl PanelKey {
    pub fn new(plugin: impl Into<String>, id: PanelId) -> Self {
        Self {
            plugin: plugin.into(),
            id,
        }
    }
}

impl std::fmt::Display for PanelKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.plugin, self.id)
    }
}

/// **What a press means**, with nothing about where it was drawn.
///
/// This is the identity half of what used to be one `HitArea`: which widget
/// the press belongs to, which part of it, what event that fires and with
/// what payload. Every field is a pure function of `(spec, instance state)` —
/// `Editor::synthesize_list_hit` and its two siblings already rebuild exactly
/// this from the spec when no hit was recorded, and they are the proof: none
/// of them can name a row or a byte, and none of them needs to.
///
/// **Why it is its own type.** The other half — `buffer_row`, `byte_start`,
/// `byte_end`, `overlay` — is meaningful only in one coordinate space: the
/// rows of the *text projection*, `WidgetSpec` rendered into a virtual
/// buffer. A described surface has no such rows; its widgets are nodes and
/// the tree hit-tests their rectangles. While the two halves shared a struct
/// the described path carried four numbers it could not interpret, and did
/// interpret two of them: it added `byte_start` to the pressed byte at
/// `shell_host.rs` and subtracted it again in
/// `reposition_widget_text_cursor_from_click`, a round trip through a
/// coordinate space that surface is not in. Splitting the type is what makes
/// that unstateable rather than merely unused.
///
/// `PartialEq` because an event travels as a `UiFact` — the tree finds the
/// widget a press landed on and the fact carries what the byte-range scan
/// used to reconstruct — and facts are compared in tests.
#[derive(Debug, Clone, PartialEq)]
pub struct WidgetEvent {
    /// Stable widget key from the spec, or empty when the spec did
    /// not assign one.
    pub widget_key: String,
    /// Widget kind discriminator: `"toggle"` or `"button"`.
    pub widget_kind: &'static str,
    /// Event payload to deliver with the `widget_event` hook.
    /// For `"toggle"`: `{ "checked": <new value> }`. For
    /// `"button"`: `{}`.
    pub payload: serde_json::Value,
    /// Event type to deliver with the `widget_event` hook
    /// (`"toggle"` or `"activate"`).
    pub event_type: &'static str,
    /// The spec key of the widget that OWNS this event — where focus
    /// moves, whose instance state a click mutates, and the key the
    /// default event fires against. `None` means `widget_key` is
    /// already the owner, which is every kind except `List`: a list
    /// row's `widget_key` is the per-item key (row hover and pointer
    /// resolution key off it), so the row's event names its List here.
    /// Set by the kind's own `collect` — the pointer dispatcher never
    /// inspects kinds to find the owner.
    pub owner_key: Option<String>,
    /// Capability, DECLARED BY THE KIND at collect: this is a row-wide
    /// gesture target — a press anywhere on its row resolves to it even
    /// past the target's own byte range (List/Tree row `select` events,
    /// markdown document line `focus` events).
    ///
    /// Read by the description: `view::shell::widgets::row_pieces` uses it
    /// to decide which event the row's trailing `Flex(1)` piece carries. The
    /// text projection's nearest-row fallback read it too, until that
    /// resolver was deleted with the panel class it served.
    pub row_target: bool,
    /// Capability, declared by the kind: a right-click raises the
    /// plugin's context menu (fires a `context` widget_event) —
    /// List/Tree row selects. The right-click seam keys off this
    /// instead of matching kind strings.
    ///
    /// SCOPE: consumed today only by the DOCK slot's right-click arm
    /// (`view::shell::widgets::hit_node` → `UiFact::WidgetContext`).
    /// Split-mounted panels have no right-click seam (Base's tab menu
    /// takes the gesture), and the centered modal swallows right
    /// -clicks whole — wiring those is part of the recorded
    /// mounted-panel arc, not an oversight at the producer sites,
    /// which declare the capability wherever a row select exists.
    pub context_click: bool,
}

impl WidgetEvent {
    /// The owning widget's spec key: `owner_key` when the kind set
    /// one, otherwise `widget_key`.
    pub fn owner(&self) -> &str {
        self.owner_key.as_deref().unwrap_or(&self.widget_key)
    }
}

/// **Where a [`WidgetEvent`]'s target was drawn** — in the rows of the text
/// projection, and nowhere else.
///
/// The collector renders a `WidgetSpec` into `TextPropertyEntry` rows inside
/// a virtual buffer, and this is the byte range one interactive target
/// occupies in them. Hit-test is `(buffer_row, buffer_col_byte) ∈ range`; the
/// bytes are UTF-8 bytes within the row's text, matching the coordinate space
/// `mouse_click` already delivers to plugins
/// (`HookArgs::MouseClick::buffer_col`).
///
/// **Nothing resolves a press through that space any more.** It was real for
/// one class of surface — a pane-mounted panel that rode the *buffer's*
/// scroll, whose rows were buffer lines and whose cursor was the plugin's
/// selection model — and that class is retired (design §3.5): every mounted
/// panel is described, answers its own presses from the rectangle layout gave
/// it, and never constructs one of these. The registry no longer stores them
/// either. What the collector still emits rides its `RenderOutput` until the
/// projection itself goes; the identity half lives in `event`, which is the
/// part the description carries on its nodes.
///
/// Layout containers (`Row`, `Col`, `Spacer`, `HintBar`, `Raw`) emit no hit
/// areas of their own; their children's bubble up with row/byte offsets
/// adjusted to reflect the final on-screen position (`kinds::containers`).
/// The `event` half is **not** shifted by that pass and must not be: its
/// payload's `valueInnerStart` stays relative to the field's own rendered
/// text. That is why a caller resolving a press through these ranges
/// subtracts the matched area's `byte_start` from its click before handing it
/// to `Editor::deliver_widget_hit` — the two numbers have to be in one space,
/// and the event's is the field's.
#[derive(Debug, Clone, PartialEq)]
pub struct HitArea {
    /// 0-indexed row within the rendered virtual buffer.
    pub buffer_row: u32,
    /// First UTF-8 byte (inclusive) within the row's text.
    pub byte_start: usize,
    /// Last UTF-8 byte (exclusive) within the row's text.
    pub byte_end: usize,
    /// True when this area came from an `Overlay` child - a popup the
    /// renderer paints *over* the rows beneath it without reflowing
    /// them (the dock's "New Task... " and "Move to Folder..." dropdowns).
    /// Its byte range is measured against the overlay's own row text,
    /// not the text of the row it covers, so a resolver over these
    /// ranges had to keep the two apart (the deleted `hit_test_row_aware`
    /// took the surface as a parameter).
    pub overlay: bool,
    /// What a press here means. Placing it does not change it.
    pub event: WidgetEvent,
}

/// **The window one keyed `List`/`Tree` was last painted into.**
///
/// Not the widget's state — the *painter's*. A scroll offset is a fold over
/// its own previous value, and the row window, the item window and the
/// measured card band are derivations over geometry; all four used to sit in
/// [`WidgetInstanceState`] and in a parallel `effective_rows` map, where they
/// read as things a plugin's spec could set and a handler could own. They are
/// neither. Naming them the last paint's window is the whole point of the
/// type: a reader that wants "how big was the window" is asking about a
/// *paint*, and it can now say so.
///
/// **Three quantities, kept apart on purpose** (§6i of the retained-mode
/// plan is the record of what conflating two of them costs): `rows` is how
/// tall the widget was painted, `items` is how many things that showed, and
/// `offset` counts in the kind's own scroll unit — items for a `List`, whose
/// window steps a card at a time, and *rows* for a `Tree`, which scrolls line
/// by line so a bordered card can sit clipped at either edge.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct PaintedWindow {
    /// Rows the widget windowed to: the spec's explicit `visible_rows`,
    /// the auto-size height budget, or the legacy fallback.
    pub rows: u32,
    /// How many items those rows showed. For a `Tree` with bordered cards
    /// of unequal height this is the conservative estimate its paging has
    /// always used (`rows / rows-per-node`), never an overshoot.
    pub items: u32,
    /// First painted position, in the kind's own scroll unit (items for a
    /// `List`, rows for a `Tree`). The scroll fold's previous value: the
    /// next paint reads it back, clamps it, and republishes it.
    pub offset: u32,
    // Rows one item occupies was here — the measured card band. It was the
    // fourth of the four numbers S1 moved out of `WidgetInstanceState`, and
    // the only one that never acquired a reader on this side: the division
    // from rows to items happens where the window is resolved, so `items`
    // above is the answer everything downstream wanted and the band itself
    // was never asked for again. `Editor::widget_viewport`'s spec fallback
    // takes a tree's rows-per-node from the *spec*, which is where a `Tree`
    // declares it; a card list's band is measured, and measuring is layout's.
}

/// Widget instance state retained across spec updates, keyed by
/// the widget's stable `key`. This is the "Spec/instance separation"
/// described in §6 of the design doc — a plugin can rebuild its
/// `WidgetSpec` from scratch on every model change without losing
/// scroll offset, cursor position, expanded keys, or focus, because
/// stateful widgets look up their instance state by `key`.
#[derive(Debug, Clone, Default)]
pub enum WidgetInstanceState {
    /// Empty/placeholder — never persisted, used as a default.
    #[default]
    None,
    /// `List` instance state: the host-owned selected index, and
    /// whether the user has taken the window away from it.
    /// `selected_index` becomes authoritative once a handler decides
    /// one — same correctness reasoning as `TextInput`'s host-owned
    /// value (the host can mutate it via `WidgetCommand::SelectMove`
    /// without racing the plugin's spec round-trip); until then the
    /// spec's value is the seed and there is no entry here at all.
    ///
    /// **The window is not here.** The scroll offset, the row window
    /// and the measured card height used to sit in this variant, and
    /// none of them is the widget's state: they are what the last
    /// paint did, so they live in [`PaintedWindow`] on the panel.
    List {
        selected_index: i32,
        /// True once the user has scrolled the list by mouse (wheel or
        /// scrollbar) without moving the selection. While set, the
        /// renderer respects the painted offset as-is instead of
        /// snapping it back to keep `selected_index` in view — so a
        /// mouse scroll can push the selected card off-screen. Cleared
        /// whenever the selection itself moves (keyboard nav, click, or
        /// a plugin `SetSelectedIndex`), which re-arms
        /// scroll-follows-selection.
        user_scrolled: bool,
    },
    /// `Text` instance state: host-owned `TextEdit` (value + cursor
    /// row/col + selection anchor + multiline flag), plus a viewport
    /// scroll offset. For multi-line (`rows > 1`) variants that's the
    /// row index of the first visible line; for a single-line field
    /// it's the first visible value *char* — the left edge of the
    /// horizontal window that follows the caret across a value too
    /// long for the field (`0` whenever the whole value fits).
    ///
    /// Becomes authoritative once the widget mounts; the spec's
    /// `value` / `cursor_byte` are *initial-only* (used at first
    /// render and ignored thereafter). This guarantees correctness
    /// under concurrent keystrokes — the plugin's spec round-trip
    /// can't race against multiple in-flight `WidgetCommand`
    /// mutations because the host doesn't read from the spec for
    /// value at all once instance state exists.
    ///
    /// Switching from a naive `(String, u32)` to `TextEdit` is what
    /// gives the widget framework selection support, word
    /// navigation, and clipboard ops "for free" — every keybinding
    /// the legacy Settings UI accepted via `TextEdit` now applies
    /// to widget-backed text inputs too.
    Text {
        editor: TextEdit,
        /// **The last of the paint's numbers still living in instance
        /// state**, and it stays because it still has a painted reader.
        ///
        /// It is the same shape as the offsets S1 moved to
        /// [`PaintedWindow`] — a fold the *painter* owns — and for a
        /// *described* panel it is already dead: the description gives the
        /// window to the element (`view::shell::widgets`'s `windowed`), which
        /// seeds from this once at mount and never reads it again, and no
        /// handler writes it there either (`Text::on_wheel`'s document branch
        /// needs a box arena a described panel does not have). What keeps the
        /// field is the painted path: a pane-mounted panel the tree does not
        /// describe still resolves its text window through
        /// `render_widget_text_area`, which reads this back and republishes
        /// it. It leaves when that renderer does — S8's wrapped viewport —
        /// not with the collector's other outputs.
        scroll: u32,
        /// Completion popup candidates the plugin most recently
        /// pushed via `WidgetMutation::SetCompletions`. Empty =
        /// popup closed. The list is stored host-side rather
        /// than read from each `WidgetSpec` so the host can
        /// keep painting the popup across renders that don't
        /// re-push it, and so `Up`/`Down` selection survives a
        /// spec refresh.
        completions: Vec<fresh_core::api::CompletionItem>,
        /// Host-managed selection cursor into `completions`.
        /// Reset to 0 every time `SetCompletions` runs with a
        /// non-empty list; clamped on every render in case the
        /// list shrank.
        completion_selected_index: usize,
        /// Index of the first candidate row currently painted.
        /// Up/Down adjusts this implicitly (the renderer auto-
        /// scrolls to keep selection in view); the mouse wheel
        /// scrolls it directly without moving the selection.
        ///
        /// **Not a paint's window, despite looking like one.** The candidate
        /// list is the one window in a described panel that is not a viewport
        /// — `SetCompletions` is host state no element holds — so this is
        /// read by the description (`completion_popup` slices its rows out of
        /// it) and written by `Text::on_wheel` through `UiFact::WidgetWheel`.
        /// §6i is the record of why it cannot become the tree's.
        completion_scroll_offset: u32,
        /// Whether the user has *explicitly* moved into the open
        /// completion popup (via ↑/↓ or the mouse wheel). Reset to
        /// `false` every time the popup (re)opens from typing, so a
        /// freshly-surfaced dropdown isn't "entered": Tab and Enter
        /// then act on the *form* (advance / submit) instead of
        /// accepting a candidate, and the popup paints no highlighted
        /// row. The first ↓ flips it true — the dropdown is now
        /// navigable, the selected row highlights, and Enter accepts.
        completion_navigated: bool,
        /// True once the user wheel-scrolled a multi-line (markdown)
        /// text viewport without moving the caret. While set, the
        /// renderer respects `scroll` as-is instead of snapping it
        /// back to keep the caret visible. Cleared whenever the caret
        /// itself moves (keys or click), re-arming follow-the-caret.
        /// Same contract as `List`/`Tree`'s flag.
        user_scrolled: bool,
    },
    /// `Tree` instance state: host-owned selected index and the set
    /// of expanded item keys. Both become authoritative once a
    /// handler decides one — the spec's `selected_index` /
    /// `expanded_keys` are seed values until then, and an untouched
    /// tree records no entry at all. `expanded_keys` is a `HashSet`
    /// because expansion is set-membership semantically (a key is
    /// either expanded or not); ordering doesn't matter and we
    /// hit-test on contains.
    ///
    /// The scroll offset that used to sit here is the painter's, not
    /// the tree's — see [`PaintedWindow`].
    Tree {
        selected_index: i32,
        expanded_keys: HashSet<String>,
        /// True once the user has scrolled the tree by mouse (wheel or
        /// scrollbar) without moving the selection. While set, the
        /// renderer respects the painted offset as-is instead of snapping
        /// it back to keep `selected_index` in view — so a mouse scroll
        /// can push the selected node off-screen. Cleared whenever the
        /// selection itself moves (keyboard nav, click, or a plugin
        /// `SetSelectedIndex` to a *different* index), which re-arms
        /// scroll-follows-selection. Same semantics as `List`'s flag —
        /// without it, a background spec refresh that re-pins the same
        /// selection (the orchestrator dock's probe poll) yanked a
        /// wheel-scrolled dock back to the selected card.
        user_scrolled: bool,
    },
    /// `Number` instance state: the host-owned current value. Becomes
    /// authoritative after first render — the spec's `value` is a
    /// seed only, same correctness reasoning as `Text`/`List` (the
    /// host can step it via `WidgetCommand::Key` or a click without
    /// racing the plugin's spec round-trip).
    Number {
        value: f64,
        /// The in-place edit, while one is open: the digits being typed,
        /// with their caret and selection, in the value's *display* units
        /// (a percent field edits `25`, not `0.25`). `None` = display mode.
        /// Enter commits it into `value` (parsed, scaled, clamped) and Tab
        /// commits before it advances; Escape and a blur abandon it. The
        /// draft lives here rather than on the spec because it is the
        /// widget's own state, not the plugin's word — the plugin sees the
        /// `change` a commit fires, and nothing before it.
        edit: Option<TextEdit>,
    },
    /// `Dropdown` instance state: the host-owned selected index plus
    /// whether the option popup is open. Authoritative after first
    /// render; the spec's `selected_index` is a seed only. The popup
    /// paints as `OverlayRow`s below the inline cycler — reusing the
    /// same overlay-paint path as `Text` completions, no separate
    /// compositor.
    Dropdown {
        selected_index: i32,
        open: bool,
        /// The index the list opened on, kept while it is open so Escape
        /// can put it back: Up/Down move the selection live (the trigger
        /// shows it and `change` fires), and Escape is the word that the
        /// move was not meant. `None` while closed, and after Enter or a
        /// click commits the live selection.
        restore: Option<i32>,
    },
    /// `DualList` instance state: the host-owned ordered included set
    /// plus which column is active and each column's cursor. The
    /// included order is the widget's meaningful output; the spec's
    /// `included` is a seed only.
    DualList {
        included: Vec<String>,
        /// True when the Included column is active (Available when
        /// false).
        active_included: bool,
        available_cursor: u32,
        included_cursor: u32,
    },
}

/// Per-panel state retained between renders. The reconciler will use
/// the previous spec to compute the minimum mutation when a future
/// `UpdateWidgetPanel` arrives.
#[derive(Debug, Clone)]
pub struct WidgetPanelState {
    /// The virtual buffer this panel renders into — `None` for a
    /// host-owned surface ([`WidgetPanelState::surface`]), which has no
    /// buffer because nothing paints it but the tree.
    pub buffer_id: Option<BufferId>,
    /// The currently-mounted spec.
    pub spec: WidgetSpec,
    /// Widget instance state by widget `key`. Survives re-renders —
    /// see `WidgetInstanceState` for what's stored.
    pub instance_states: HashMap<String, WidgetInstanceState>,
    /// Which widget holds this panel's focus — **the fact**, with one writer
    /// ([`WidgetRegistry::decide_focus`]) and the tree as its projection.
    /// Empty when nothing is focused: a panel with no focusable widgets, or
    /// one that declared `autoFocusFirst: false` and has not been given a
    /// focus, in which case the description marks its own interior and the
    /// tree rests there. Re-clamped onto a widget the spec still has when
    /// the spec changes (`resolve_panel`).
    pub focus_key: String,
    /// The window each keyed `List`/`Tree` was last painted into, by
    /// widget key — see [`PaintedWindow`]. The scroll fold's own previous
    /// value lives here, and so does one of the three answers to "how big is
    /// this widget's window": the *paint's*. A described panel's comes from
    /// the tree instead and this map is empty for one, because the walk that
    /// filled it does not run (`Editor::resolve_described_panel`); the spec
    /// answers last, for a widget nothing has laid out yet
    /// ([`super::kinds::Viewport::from_spec`]).
    pub painted: HashMap<String, PaintedWindow>,
    /// The panel's layout-box tree from the most recent render
    /// (root-last arena; see [`crate::widgets::layout_box`]).
    /// Structure + panel-relative geometry for hit-tested dispatch.
    ///
    /// **Empty for a described panel**, which never had a use for it: its
    /// rectangles are the tree's, the wheel router declines it outright, and
    /// the Tab ring it used to supply is now a walk of the spec
    /// (`Ui::next_in` over the interior's registrations) — the same two `box_meta` facts,
    /// asked of the thing that states them.
    pub boxes: Vec<crate::widgets::LayoutBox>,
    /// This panel's [`WidgetPanelOptions::auto_focus_first`], kept so
    /// every later repaint resolves focus the same way the mount did.
    pub auto_focus_first: bool,
    /// This panel's [`WidgetPanelOptions::page`]: its content scrolls as
    /// one page in a window the description owns, and its lists take
    /// their natural height.
    pub page: bool,
    /// This panel's [`WidgetPanelOptions::focus_follows_cursor`]: focus
    /// and the buffer's caret are the same thing, in both directions.
    /// Read by the host on every focus move and every cursor move; the
    /// panel itself never acts on it.
    pub focus_follows_cursor: bool,
    /// Widget the pointer is over, `""` for none.
    ///
    /// Floating and dock panels keep this on their `FloatingWidgetPanel`
    /// instead; it lives here for panels mounted into a BUFFER, which
    /// have no such struct — and which, until this field existed, could
    /// not light anything under the pointer at all.
    pub hovered_widget_key: String,
    /// The hovered ROW's own key, for kinds whose rows share one widget
    /// key (`List`, `Tree`). Empty for everything else.
    pub hovered_item_key: String,
}

impl WidgetPanelState {
    /// **A host-owned surface's store.** The settings dialog's page and each
    /// of its entry dialogs are forms of the same `WidgetSpec`s a plugin's
    /// panel is made of, and their controls are edited by the same kinds —
    /// so they keep their state in the same shape the kinds read and write,
    /// and `on_key` / `on_pointer` / `on_text` run against it unchanged.
    /// What such a surface is *not* is a plugin's panel: it has no buffer,
    /// no registry entry, no plugin to fire events at (its host applies
    /// the `KeyFx` itself), and its `spec` is whatever the host describes
    /// this frame.
    pub fn surface(spec: WidgetSpec) -> Self {
        WidgetPanelState {
            buffer_id: None,
            spec,
            instance_states: HashMap::new(),
            focus_key: String::new(),
            painted: HashMap::new(),
            boxes: Vec::new(),
            auto_focus_first: false,
            page: false,
            // A host surface has no reading row to keep focus in step with:
            // it is not a document, and nothing scrolls it as one.
            focus_follows_cursor: false,
            hovered_widget_key: String::new(),
            hovered_item_key: String::new(),
        }
    }

    /// The window the keyed widget is being driven against, read off
    /// the last paint — `None` for a widget this panel has never painted.
    ///
    /// **Absent is an answer, not a default.** It used to fall back to the
    /// spec's own `visible_rows` here, which made one function two: "what did
    /// the paint measure" and "what does the spec ask for". The caller
    /// (`app::Editor::widget_viewport`) now asks the tree first and
    /// falls through to [`super::kinds::Viewport::from_spec`] last, and a
    /// fallback buried in the middle of that chain could only shadow the
    /// tree's answer with a stale one.
    pub fn painted_viewport(&self, key: &str) -> Option<super::kinds::Viewport> {
        self.painted.get(key).map(|w| super::kinds::Viewport {
            rows: w.rows,
            items: w.items.max(1),
        })
    }

    /// The painted window for `key`, for a handler that is about to move
    /// it.
    ///
    /// A widget the panel has never painted still gets a window: the one
    /// the host just delivered. That is the honest answer — the handler
    /// is acting inside a window somebody resolved, and recording that
    /// window is not a claim that a paint happened, only that this is the
    /// frame the offset is measured in. It is also what keeps a wheel
    /// notch on a not-yet-painted list from silently going nowhere, which
    /// is what it did when the offset lived in an absent instance state.
    pub fn window_mut(
        &mut self,
        key: &str,
        viewport: super::kinds::Viewport,
    ) -> &mut PaintedWindow {
        self.painted
            .entry(key.to_string())
            .or_insert(PaintedWindow {
                rows: viewport.rows,
                items: viewport.items,
                offset: 0,
            })
    }

    /// Latch "the user moved this window by hand" on a `List` or `Tree`.
    ///
    /// **Seeded from the spec when there is no entry**, because now that
    /// the render walk stops writing derivations an untouched widget has
    /// none — and a wheel notch is not a reason to forget which row the
    /// plugin said was selected. Every handler that folds this state has
    /// the same obligation; this is the one that has no spec-shaped
    /// resolver of its own to route through.
    pub fn latch_user_scrolled(&mut self, key: &str) {
        if let Some(
            WidgetInstanceState::List { user_scrolled, .. }
            | WidgetInstanceState::Tree { user_scrolled, .. },
        ) = self.instance_states.get_mut(key)
        {
            *user_scrolled = true;
            return;
        }
        if self.instance_states.contains_key(key) {
            return;
        }
        let seeded = match crate::widgets::find_widget_by_key(&self.spec, key) {
            Some(WidgetSpec::List { selected_index, .. }) => WidgetInstanceState::List {
                selected_index: *selected_index,
                user_scrolled: true,
            },
            Some(WidgetSpec::Tree {
                selected_index,
                expanded_keys,
                ..
            }) => WidgetInstanceState::Tree {
                selected_index: *selected_index,
                expanded_keys: expanded_keys.iter().cloned().collect(),
                user_scrolled: true,
            },
            _ => return,
        };
        self.instance_states.insert(key.to_string(), seeded);
    }

    /// Set the host-owned selected index for a `List` or `Tree`
    /// instance, dispatching on the *existing* instance variant so a
    /// Tree keeps its expanded-keys set. Shared by the pointer select
    /// path (`Tree::on_pointer`) and the `SetSelectedIndex` mutation
    /// in the plugin dispatcher so both move Tree selections, not
    /// just List ones. Does not re-render; callers decide when to
    /// repaint.
    ///
    /// **What it carries is now only what it owns.** It used to shuttle
    /// a scroll offset and a measured item height through both arms;
    /// those are the painter's and live in [`PaintedWindow`], so a
    /// selection move no longer has to know they exist — and cannot
    /// reset them by forgetting to copy one.
    pub fn set_selected_index(&mut self, widget_key: &str, index: i32) {
        // **The widget's kind decides which state this records, not what
        // happens to be in the map.** An untouched `Tree` has no entry at
        // all, and `tree::resolve` ignores a `List` one — so writing `List`
        // here left the selection reading back as the spec's seed, i.e. the
        // write silently did nothing.
        let tree_spec = crate::widgets::find_widget_by_key(&self.spec, widget_key)
            .filter(|spec| matches!(spec, WidgetSpec::Tree { .. }));
        if let Some(spec) = tree_spec {
            // Resolved, not defaulted: a first write must inherit the spec's
            // `expanded_keys` seed rather than record an empty set.
            let current =
                crate::widgets::kinds::tree::resolve(spec, widget_key, &self.instance_states);
            let new_state = WidgetInstanceState::Tree {
                expanded_keys: current.expanded,
                // Re-pinning the *same* index (which the orchestrator
                // dock's `refreshOpenDialog` does on every probe-poll
                // repaint) must preserve a user scroll — otherwise the
                // refresh would snap the view back to the selection a
                // beat after a mouse scroll. Only an actual selection
                // change re-arms scroll-follows-selection. Mirrors the
                // List branch below.
                user_scrolled: current.user_scrolled && index == current.selected,
                selected_index: index,
            };
            self.instance_states
                .insert(widget_key.to_string(), new_state);
            return;
        }
        let new_state = match self.instance_states.get(widget_key) {
            Some(WidgetInstanceState::Tree {
                selected_index,
                expanded_keys,
                user_scrolled,
            }) => WidgetInstanceState::Tree {
                expanded_keys: expanded_keys.clone(),
                // A key with no spec mounted here: nothing to resolve
                // against, so the stored shape is all there is.
                user_scrolled: *user_scrolled && index == *selected_index,
                selected_index: index,
            },
            other => {
                let (prev_index, prev_user_scrolled) = match other {
                    Some(WidgetInstanceState::List {
                        selected_index,
                        user_scrolled,
                    }) => (*selected_index, *user_scrolled),
                    _ => (-1, false),
                };
                // Re-pinning the *same* index (which `refreshOpenDialog`
                // does on every repaint) must preserve a user scroll —
                // otherwise a probe-poll refresh would snap the view back
                // to the selection a beat after a mouse scroll. Only an
                // actual selection change re-arms scroll-follows-selection.
                WidgetInstanceState::List {
                    selected_index: index,
                    user_scrolled: prev_user_scrolled && index == prev_index,
                }
            }
        };
        self.instance_states
            .insert(widget_key.to_string(), new_state);
    }
}

/// Global registry of mounted widget panels, keyed by composite
/// (plugin, panel id) identity — two plugins reusing the same local id
/// coexist without evicting each other.
#[derive(Debug, Default)]
pub struct WidgetRegistry {
    panels: HashMap<PanelKey, WidgetPanelState>,
}

impl WidgetRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    /// Mount or replace a panel. Returns the previous state if the
    /// panel was already mounted (the dispatcher may use this to
    /// detect re-mounts on the same id).
    ///
    /// The wide parameter list is the price of `WidgetPanelState`
    /// being public — every field is plainly named at the call
    /// site rather than buried inside an opaque builder. The
    /// dispatcher always populates them all from one `RenderOutput`,
    /// so the apparent verbosity stays at the boundary.
    #[allow(clippy::too_many_arguments)]
    pub fn mount(
        &mut self,
        panel_key: PanelKey,
        buffer_id: BufferId,
        spec: WidgetSpec,
        instance_states: HashMap<String, WidgetInstanceState>,
        focus_key: String,
        painted: HashMap<String, PaintedWindow>,
        boxes: Vec<crate::widgets::LayoutBox>,
        auto_focus_first: bool,
        page: bool,
        focus_follows_cursor: bool,
    ) -> Option<WidgetPanelState> {
        // A re-mount under a stationary pointer keeps its highlight:
        // the pointer has not moved, so neither should what it lights.
        let (hovered_widget_key, hovered_item_key) = self
            .panels
            .get(&panel_key)
            .map(|p| (p.hovered_widget_key.clone(), p.hovered_item_key.clone()))
            .unwrap_or_default();
        self.panels.insert(
            panel_key,
            WidgetPanelState {
                buffer_id: Some(buffer_id),
                spec,
                instance_states,
                focus_key,
                painted,
                boxes,
                auto_focus_first,
                page,
                focus_follows_cursor,
                hovered_widget_key,
                hovered_item_key,
            },
        )
    }

    /// What the pointer is over in this panel: `(widget key, row key)`,
    /// both empty when nothing is.
    pub fn hover_keys(&self, panel_key: &PanelKey) -> (String, String) {
        self.panels
            .get(panel_key)
            .map(|p| (p.hovered_widget_key.clone(), p.hovered_item_key.clone()))
            .unwrap_or_default()
    }

    /// Record what the pointer is over. Returns true when that changed —
    /// the caller re-renders only on the enter/leave transition, so
    /// pointer movement across a panel costs a hit-test and nothing else.
    pub fn set_hover_keys(&mut self, panel_key: &PanelKey, widget: String, item: String) -> bool {
        match self.panels.get_mut(panel_key) {
            Some(p) if p.hovered_widget_key != widget || p.hovered_item_key != item => {
                p.hovered_widget_key = widget;
                p.hovered_item_key = item;
                true
            }
            _ => false,
        }
    }

    /// Replace the spec and rendered metadata on an already-mounted
    /// panel. Returns `Ok(buffer_id)` to render into, or `Err(())`
    /// if no panel exists for that id (caller should drop the
    /// update — the plugin re-emitted after unmount). The unit
    /// error is sufficient: there's exactly one failure mode and
    /// no payload to attach.
    #[allow(clippy::result_unit_err)]
    #[allow(clippy::too_many_arguments)]
    pub fn update(
        &mut self,
        panel_key: &PanelKey,
        spec: WidgetSpec,
        instance_states: HashMap<String, WidgetInstanceState>,
        focus_key: String,
        painted: HashMap<String, PaintedWindow>,
        boxes: Vec<crate::widgets::LayoutBox>,
    ) -> Result<BufferId, ()> {
        match self.panels.get_mut(panel_key) {
            Some(state) => {
                state.spec = spec;
                state.instance_states = instance_states;
                state.focus_key = focus_key;
                state.painted = painted;
                state.boxes = boxes;
                state.buffer_id.ok_or(())
            }
            None => Err(()),
        }
    }

    /// Read-only access to the instance state for a panel — used by
    /// the dispatcher to thread previous scroll offsets / cursor
    /// positions into the next render so they persist.
    pub fn instance_states(
        &self,
        panel_key: &PanelKey,
    ) -> Option<&HashMap<String, WidgetInstanceState>> {
        self.panels.get(panel_key).map(|s| &s.instance_states)
    }

    /// Read-only access to the previous render's focus key.
    pub fn focus_key(&self, panel_key: &PanelKey) -> Option<&str> {
        self.panels.get(panel_key).map(|s| s.focus_key.as_str())
    }

    /// Decide which widget holds this panel's focus.
    ///
    /// **The one writer of the fact, for every decision.** Two callers, each
    /// a decision and not a mirror: `Editor::set_panel_focus_and_notify` —
    /// the host's door for a Tab, a click, a key policy, a `FocusAdvance`,
    /// and the tree's own ring reporting a landing — and the plugin's
    /// `WidgetMutation::SetFocusKey`. The third way the fact changes is a
    /// spec change re-clamping it (`resolve_panel`, through `update`), which
    /// is the same fact sanitised against a new spec, not a new decision.
    ///
    /// The tree follows: the description marks the widget this names
    /// `autofocus`, and `fresh_ui` re-settles focus onto a mark that moved.
    /// Nothing else writes `focus_key`, and nothing writes the tree.
    pub fn decide_focus(&mut self, panel_key: &PanelKey, key: String) {
        if let Some(state) = self.panels.get_mut(panel_key) {
            state.focus_key = key;
        }
    }

    /// Update side-effects (instance_states, focus_key)
    /// without taking ownership of the spec. Used by `rerender_widget_panel`
    /// after an in-place spec mutation: the spec in the registry is already
    /// current (mutation helpers like `append_tree_nodes_in_spec` mutate it
    /// in place), so cloning it back through `update()` just to write the
    /// same value would waste a 5 000-node deep clone for every IPC.
    #[allow(clippy::too_many_arguments)]
    pub fn update_side_effects(
        &mut self,
        panel_key: &PanelKey,
        instance_states: HashMap<String, WidgetInstanceState>,
        focus_key: String,
        painted: HashMap<String, PaintedWindow>,
        boxes: Vec<crate::widgets::LayoutBox>,
    ) -> Option<BufferId> {
        let state = self.panels.get_mut(panel_key)?;
        state.instance_states = instance_states;
        state.focus_key = focus_key;
        // Host-driven rerenders (focus moves, hover, wheel) refresh the
        // painted windows and geometry too — previously the window
        // sizes were only written on the plugin-driven mount/update
        // paths and went stale across every host-side rerender.
        state.painted = painted;
        state.boxes = boxes;
        state.buffer_id
    }

    /// Borrow the current spec + return the buffer id. Companion to
    /// `update_side_effects` — render with the borrow and then write
    /// back only the side-effects, avoiding the deep clone of the spec
    /// that `buffer_and_spec()` does.
    pub fn buffer_and_spec_ref(&self, panel_key: &PanelKey) -> Option<(BufferId, &WidgetSpec)> {
        let s = self.panels.get(panel_key)?;
        Some((s.buffer_id?, &s.spec))
    }

    /// Find the buffer and current spec for a panel — used by the
    /// dispatcher to re-render after a focus advance / activate
    /// command without the plugin needing to send an UpdateWidgetPanel.
    pub fn buffer_and_spec(&self, panel_key: &PanelKey) -> Option<(BufferId, WidgetSpec)> {
        let s = self.panels.get(panel_key)?;
        Some((s.buffer_id?, s.spec.clone()))
    }

    /// Tear down a panel. Returns the buffer_id the panel was
    /// rendering into, so the caller can clear the buffer if it
    /// owns it.
    pub fn unmount(&mut self, panel_key: &PanelKey) -> Option<BufferId> {
        self.panels.remove(panel_key).and_then(|s| s.buffer_id)
    }

    /// Read-only access to a panel's current state.
    pub fn get(&self, panel_key: &PanelKey) -> Option<&WidgetPanelState> {
        self.panels.get(panel_key)
    }

    /// Mutable access — used by `WidgetCommand` handlers that
    /// update widget instance state (e.g. TextInput value/cursor)
    /// directly without round-tripping through the plugin.
    pub fn get_mut(&mut self, panel_key: &PanelKey) -> Option<&mut WidgetPanelState> {
        self.panels.get_mut(panel_key)
    }

    /// All currently-mounted panel keys — useful for theme-change
    /// re-render passes (every panel re-renders against the new
    /// theme without plugin involvement).
    pub fn panel_keys(&self) -> Vec<PanelKey> {
        self.panels.keys().cloned().collect()
    }

    /// Panels rendering into `buffer_id`. Used by mouse-wheel
    /// routing to find which widget panel sits under the pointer.
    pub fn panels_for_buffer(&self, buffer_id: BufferId) -> Vec<PanelKey> {
        self.panels
            .iter()
            .filter(|(_, s)| s.buffer_id == Some(buffer_id))
            .map(|(key, _)| key.clone())
            .collect()
    }

    /// Whether any panel mounted into `buffer_id` asked for
    /// `focusFollowsCursor`.
    ///
    /// The cheap gate in front of the geometry: it is asked on **every
    /// reading-row move**, almost all of them in ordinary buffers no panel is
    /// mounted into, so it allocates nothing and never touches the buffer.
    pub fn has_focus_follower(&self, buffer_id: BufferId) -> bool {
        self.panels
            .values()
            .any(|p| p.buffer_id == Some(buffer_id) && p.focus_follows_cursor)
    }

    /// The one panel mounted into `buffer_id` that asked for
    /// `focusFollowsCursor`, or `None`.
    ///
    /// **`None` when two of them share a buffer**, deliberately. Two panels
    /// both tracking one reading row is not a state with a right answer —
    /// each would seat it where the other did not want it — and picking one
    /// of them here would pick it out of a `HashMap`, so the pair could
    /// resolve differently on two consecutive calls in the same frame.
    /// Answering "no" and saying so in the log leaves the reader alone, which
    /// is the one behaviour that cannot be wrong.
    pub fn focus_follower_of(&self, buffer_id: BufferId) -> Option<PanelKey> {
        let mut found: Option<&PanelKey> = None;
        for (key, state) in &self.panels {
            if state.buffer_id != Some(buffer_id) || !state.focus_follows_cursor {
                continue;
            }
            if found.is_some() {
                tracing::warn!(
                    "two focusFollowsCursor panels in buffer {:?}; neither will track the reader",
                    buffer_id
                );
                return None;
            }
            found = Some(key);
        }
        found.cloned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn empty_spec() -> WidgetSpec {
        WidgetSpec::Col {
            children: vec![],
            key: None,
        }
    }

    #[test]
    fn same_local_id_from_two_plugins_coexists() {
        // Panel ids are plugin-local: a second plugin mounting the same
        // local id must NOT evict the first plugin's panel, and each key
        // resolves to its own plugin's panel.
        let mut reg = WidgetRegistry::new();
        reg.mount(
            PanelKey::new("alpha", 1),
            BufferId(10),
            empty_spec(),
            HashMap::new(),
            String::new(),
            HashMap::new(),
            Vec::new(),
            true,
            false,
            false,
        );
        let evicted = reg.mount(
            PanelKey::new("beta", 1),
            BufferId(20),
            empty_spec(),
            HashMap::new(),
            String::new(),
            HashMap::new(),
            Vec::new(),
            true,
            false,
            false,
        );
        assert!(evicted.is_none(), "beta:1 must not evict alpha:1");

        assert_eq!(
            reg.get(&PanelKey::new("alpha", 1))
                .and_then(|p| p.buffer_id),
            Some(BufferId(10))
        );
        assert_eq!(
            reg.get(&PanelKey::new("beta", 1)).and_then(|p| p.buffer_id),
            Some(BufferId(20))
        );
        assert_eq!(
            reg.panels_for_buffer(BufferId(10)),
            vec![PanelKey::new("alpha", 1)]
        );

        // Unmounting one plugin's panel leaves the other untouched.
        reg.unmount(&PanelKey::new("beta", 1));
        assert!(reg.get(&PanelKey::new("beta", 1)).is_none());
        assert!(reg.get(&PanelKey::new("alpha", 1)).is_some());
    }
}
