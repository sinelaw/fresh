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
    /// Read in both projections, and it is the same capability in each:
    /// `WidgetRegistry::row_select_hit` uses it for the text projection's
    /// nearest-row fallback, and `view::shell::widgets::row_pieces` uses it
    /// to decide which event the row's trailing `Flex(1)` piece carries.
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
/// **That space is real for exactly one class of surface**: a pane-mounted
/// panel that rides the *buffer's* scroll, whose rows genuinely are buffer
/// lines and whose cursor is the plugin's selection model
/// (`Editor::pane_panel_owns_its_scroll`). `WidgetRegistry::hit_test_row_aware`
/// is the resolver for that class. Every described surface answers its own
/// presses from the rectangle layout gave it and never constructs one of
/// these — which is why the geometry lives here and the identity lives in
/// `event`, rather than both living in one struct that half its readers had
/// to ignore.
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
    /// not the text of the row it covers, so click resolution has to
    /// keep the two apart - `hit_test_row_aware` takes the surface as
    /// a parameter, decided by the panel's layout-box tree (a z>0 box
    /// covers the base rows beneath it).
    pub overlay: bool,
    /// What a press here means. Placing it does not change it.
    pub event: WidgetEvent,
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
    /// `List` instance state: host-owned scroll offset *and*
    /// selected index. `selected_index` becomes authoritative
    /// after first render — same correctness reasoning as
    /// `TextInput`'s host-owned value (host can mutate it via
    /// `WidgetCommand::SelectMove` without racing the plugin's
    /// spec round-trip).
    List {
        scroll_offset: u32,
        selected_index: i32,
        /// Rows each item occupies in the last render: 1 for a classic
        /// one-row-per-item list, or the uniform card height for an
        /// `item_specs` (card) list. The renderer writes it; mouse
        /// handlers read it to convert the row-denominated `visible_rows`
        /// into a per-item scroll window (so wheel/scrollbar bounds are
        /// right for card lists, and an un-scrollable list still lets the
        /// wheel bubble to an enclosing scrollable pane).
        item_height: u32,
        /// True once the user has scrolled the list by mouse (wheel or
        /// scrollbar) without moving the selection. While set, the
        /// renderer respects `scroll_offset` as-is instead of snapping
        /// it back to keep `selected_index` in view — so a mouse scroll
        /// can push the selected card off-screen. Cleared whenever the
        /// selection itself moves (keyboard nav, click, or a plugin
        /// `SetSelectedIndex`), which re-arms scroll-follows-selection.
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
    /// `Tree` instance state: host-owned scroll offset, selected
    /// index, and the set of expanded item keys. All three become
    /// authoritative after first render — the spec's
    /// `selected_index` / `expanded_keys` are seed values only.
    /// `expanded_keys` is a `HashSet` because expansion is
    /// set-membership semantically (a key is either expanded or
    /// not); ordering doesn't matter and we hit-test on contains.
    Tree {
        scroll_offset: u32,
        selected_index: i32,
        expanded_keys: HashSet<String>,
        /// True once the user has scrolled the tree by mouse (wheel or
        /// scrollbar) without moving the selection. While set, the
        /// renderer respects `scroll_offset` as-is instead of snapping
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
    Number { value: f64 },
    /// `Dropdown` instance state: the host-owned selected index plus
    /// whether the option popup is open. Authoritative after first
    /// render; the spec's `selected_index` is a seed only. The popup
    /// paints as `OverlayRow`s below the inline cycler — reusing the
    /// same overlay-paint path as `Text` completions, no separate
    /// compositor.
    Dropdown { selected_index: i32, open: bool },
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
    /// The virtual buffer this panel renders into.
    pub buffer_id: BufferId,
    /// The currently-mounted spec.
    pub spec: WidgetSpec,
    /// **The text projection's output**: one [`HitArea`] per interactive
    /// target in the rows the collector rendered, in declaration order.
    /// Hit-test scans linearly — the small N (one per interactive widget per
    /// panel) doesn't justify a spatial index.
    ///
    /// Two readers, and they read different halves:
    ///
    /// * [`Self::hit_test_row_aware`](WidgetRegistry::hit_test_row_aware)
    ///   reads the geometry, for the one surface whose rows are real buffer
    ///   lines — a pane-mounted panel that rides the buffer's scroll
    ///   (`app::click_handlers`).
    /// * `view::scene::widgets_view` and the two `deliver_widget_hit_*`
    ///   entry points read only [`HitArea::event`]: the web lays the spec out
    ///   itself and sends back an index plus the identity, so what crosses to
    ///   it is the identity half and an ordering.
    ///
    /// Nothing else reads this. A described surface's widgets are nodes and
    /// answer their own presses from the rectangles layout gave them; the
    /// event they carry is the same value as `hits[i].event`, stated where the
    /// widget is rather than looked up by a row and a byte.
    pub hits: Vec<HitArea>,
    /// Widget instance state by widget `key`. Survives re-renders —
    /// see `WidgetInstanceState` for what's stored.
    pub instance_states: HashMap<String, WidgetInstanceState>,
    /// Currently-focused widget key within this panel. Empty when
    /// the panel has no focusable widgets, or before the first
    /// render. Maintained by the renderer (clamps to a valid
    /// tabbable key on every render) and by `widget_focus_advance`
    /// (cycles through tabbables on Tab / Shift+Tab).
    pub focus_key: String,
    /// Tabbable widget keys collected from the most recent render,
    /// in declaration order. The Tab-cycle command finds the
    /// current `focus_key`'s position in this list and advances by
    /// the requested delta (with wraparound).
    pub tabbable: Vec<String>,
    /// Effective rows each keyed `List`/`Tree` actually windowed to in
    /// the most recent render — the spec's explicit value, the
    /// auto-size height budget, or the legacy fallback. Key/mouse
    /// handlers read this for scroll/page bounds instead of the spec,
    /// because an auto-sized widget's spec carries no number at all.
    pub effective_rows: HashMap<String, u32>,
    /// The panel's layout-box tree from the most recent render
    /// (root-last arena; see [`crate::widgets::layout_box`]).
    /// Structure + panel-relative geometry for hit-tested dispatch
    /// and the derived focus ring.
    pub boxes: Vec<crate::widgets::LayoutBox>,
}

impl WidgetPanelState {
    /// Rows the keyed widget actually windowed to in the last render,
    /// falling back to the spec's explicit value and then the legacy
    /// default (a panel that has never rendered).
    pub fn effective_visible_rows(&self, key: &str, spec_visible: Option<u32>) -> u32 {
        self.effective_rows
            .get(key)
            .copied()
            .or(spec_visible)
            .unwrap_or(fresh_core::api::LEGACY_VISIBLE_ROWS_FALLBACK)
    }

    /// Set the host-owned selected index for a `List` or `Tree`
    /// instance, dispatching on the *existing* instance variant so a
    /// Tree keeps its scroll + expanded-keys set (and a List keeps
    /// its item height / user-scroll flag). Shared by the pointer
    /// select path (`Tree::on_pointer`) and the `SetSelectedIndex`
    /// mutation in the plugin dispatcher so both move Tree
    /// selections, not just List ones. Does not re-render; callers
    /// decide when to repaint.
    pub fn set_selected_index(&mut self, widget_key: &str, index: i32) {
        let new_state = match self.instance_states.get(widget_key) {
            Some(WidgetInstanceState::Tree {
                scroll_offset,
                selected_index,
                expanded_keys,
                user_scrolled,
            }) => WidgetInstanceState::Tree {
                scroll_offset: *scroll_offset,
                expanded_keys: expanded_keys.clone(),
                // Re-pinning the *same* index (which the orchestrator
                // dock's `refreshOpenDialog` does on every probe-poll
                // repaint) must preserve a user scroll — otherwise the
                // refresh would snap the view back to the selection a
                // beat after a mouse scroll. Only an actual selection
                // change re-arms scroll-follows-selection. Mirrors the
                // List branch below.
                user_scrolled: *user_scrolled && index == *selected_index,
                selected_index: index,
            },
            other => {
                let (prev_scroll, prev_index, prev_item_height, prev_user_scrolled) = match other {
                    Some(WidgetInstanceState::List {
                        scroll_offset,
                        selected_index,
                        item_height,
                        user_scrolled,
                    }) => (
                        *scroll_offset,
                        *selected_index,
                        *item_height,
                        *user_scrolled,
                    ),
                    _ => (0, -1, 1, false),
                };
                // Re-pinning the *same* index (which `refreshOpenDialog`
                // does on every repaint) must preserve a user scroll —
                // otherwise a probe-poll refresh would snap the view back
                // to the selection a beat after a mouse scroll. Only an
                // actual selection change re-arms scroll-follows-selection.
                WidgetInstanceState::List {
                    scroll_offset: prev_scroll,
                    selected_index: index,
                    item_height: prev_item_height,
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
        hits: Vec<HitArea>,
        instance_states: HashMap<String, WidgetInstanceState>,
        focus_key: String,
        tabbable: Vec<String>,
        effective_rows: HashMap<String, u32>,
        boxes: Vec<crate::widgets::LayoutBox>,
    ) -> Option<WidgetPanelState> {
        self.panels.insert(
            panel_key,
            WidgetPanelState {
                buffer_id,
                spec,
                hits,
                instance_states,
                focus_key,
                tabbable,
                effective_rows,
                boxes,
            },
        )
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
        hits: Vec<HitArea>,
        instance_states: HashMap<String, WidgetInstanceState>,
        focus_key: String,
        tabbable: Vec<String>,
        effective_rows: HashMap<String, u32>,
        boxes: Vec<crate::widgets::LayoutBox>,
    ) -> Result<BufferId, ()> {
        match self.panels.get_mut(panel_key) {
            Some(state) => {
                state.spec = spec;
                state.hits = hits;
                state.instance_states = instance_states;
                state.focus_key = focus_key;
                state.tabbable = tabbable;
                state.effective_rows = effective_rows;
                state.boxes = boxes;
                Ok(state.buffer_id)
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

    /// Set the focus key directly (used by `widget_focus_advance`
    /// and click-driven focus moves). Updates the in-place state;
    /// the next render reads it via `focus_key()`.
    pub fn set_focus_key(&mut self, panel_key: &PanelKey, key: String) {
        if let Some(state) = self.panels.get_mut(panel_key) {
            state.focus_key = key;
        }
    }

    /// Host-driven scroll of a `List` widget (e.g. a scrollbar drag).
    /// Sets the list's `scroll_offset` and, when the list has a live
    /// selection, clamps `selected_index` into the new visible window
    /// `[scroll, scroll + visible)` so the next render's
    /// ensure-selected-visible doesn't snap the thumb back.
    ///
    /// Returns the post-clamp `selected_index` when the list has a
    /// selection that moved (so the caller can notify the plugin to
    /// keep its own selection mirror + preview in sync), else `None`.
    pub fn set_list_scroll(
        &mut self,
        panel_key: &PanelKey,
        list_key: &str,
        scroll_offset: u32,
        visible: u32,
    ) -> Option<i32> {
        let _ = visible;
        let state = self.panels.get_mut(panel_key)?;
        // Mouse scroll moves the *view* only — the selection stays put
        // (and may scroll out of view). `user_scrolled` tells the
        // renderer not to snap the offset back to the selection. Never
        // returns a moved selection, so no `select`/live-switch fires.
        // Trees get the same treatment so a scrollbar drag on a tree
        // panel (the orchestrator dock) sticks instead of no-opping.
        match state.instance_states.get_mut(list_key)? {
            WidgetInstanceState::List {
                scroll_offset: so,
                user_scrolled,
                ..
            }
            | WidgetInstanceState::Tree {
                scroll_offset: so,
                user_scrolled,
                ..
            } => {
                *so = scroll_offset;
                *user_scrolled = true;
            }
            _ => return None,
        }
        None
    }

    /// Update side-effects (hits, instance_states, focus_key, tabbable)
    /// without taking ownership of the spec. Used by `rerender_widget_panel`
    /// after an in-place spec mutation: the spec in the registry is already
    /// current (mutation helpers like `append_tree_nodes_in_spec` mutate it
    /// in place), so cloning it back through `update()` just to write the
    /// same value would waste a 5 000-node deep clone for every IPC.
    #[allow(clippy::too_many_arguments)]
    pub fn update_side_effects(
        &mut self,
        panel_key: &PanelKey,
        hits: Vec<HitArea>,
        instance_states: HashMap<String, WidgetInstanceState>,
        focus_key: String,
        tabbable: Vec<String>,
        effective_rows: HashMap<String, u32>,
        boxes: Vec<crate::widgets::LayoutBox>,
    ) -> Option<BufferId> {
        let state = self.panels.get_mut(panel_key)?;
        state.hits = hits;
        state.instance_states = instance_states;
        state.focus_key = focus_key;
        state.tabbable = tabbable;
        // Host-driven rerenders (focus moves, hover, wheel) refresh the
        // window sizes and geometry too — previously `effective_rows`
        // was only written on the plugin-driven mount/update paths and
        // went stale across every host-side rerender.
        state.effective_rows = effective_rows;
        state.boxes = boxes;
        Some(state.buffer_id)
    }

    /// Borrow the current spec + return the buffer id. Companion to
    /// `update_side_effects` — render with the borrow and then write
    /// back only the side-effects, avoiding the deep clone of the spec
    /// that `buffer_and_spec()` does.
    pub fn buffer_and_spec_ref(&self, panel_key: &PanelKey) -> Option<(BufferId, &WidgetSpec)> {
        self.panels.get(panel_key).map(|s| (s.buffer_id, &s.spec))
    }

    /// Find the buffer and current spec for a panel — used by the
    /// dispatcher to re-render after a focus advance / activate
    /// command without the plugin needing to send an UpdateWidgetPanel.
    pub fn buffer_and_spec(&self, panel_key: &PanelKey) -> Option<(BufferId, WidgetSpec)> {
        self.panels
            .get(panel_key)
            .map(|s| (s.buffer_id, s.spec.clone()))
    }

    /// Tear down a panel. Returns the buffer_id the panel was
    /// rendering into, so the caller can clear the buffer if it
    /// owns it.
    pub fn unmount(&mut self, panel_key: &PanelKey) -> Option<BufferId> {
        self.panels.remove(panel_key).map(|s| s.buffer_id)
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
            .filter(|(_, s)| s.buffer_id == buffer_id)
            .map(|(key, _)| key.clone())
            .collect()
    }

    /// Hit-test the given buffer-local position against every
    /// currently-mounted panel rendering into `buffer_id`. Returns
    /// the matching panel id and a clone of the hit area on a hit,
    /// `None` otherwise.
    ///
    /// Linear scan: panel count is typically 1 per buffer; per-panel
    /// hit count is small (one per interactive widget). A spatial
    /// index would be over-engineering at this scale.
    pub fn hit_test(
        &self,
        buffer_id: BufferId,
        row: u32,
        col_byte: u32,
    ) -> Option<(PanelKey, HitArea)> {
        self.surface_hit(buffer_id, row, col_byte, false)
    }

    /// One byte-ranged scan for both surfaces: `on_overlay` selects
    /// whether the byte ranges are measured against the popup rows an
    /// `Overlay` painted (`hit.overlay == true`) or the base rows
    /// beneath. Which surface the pointer is on is the layout-box
    /// tree's call (a z>0 box covers the base) — made by the caller,
    /// not re-derived here.
    ///
    /// Returns the whole [`HitArea`], geometry included, because the caller
    /// needs `byte_start` to rebase its click out of the *composed row's*
    /// coordinate space and into the widget's own before handing it to
    /// `deliver_widget_hit`.
    fn surface_hit(
        &self,
        buffer_id: BufferId,
        row: u32,
        col_byte: u32,
        on_overlay: bool,
    ) -> Option<(PanelKey, HitArea)> {
        for (key, state) in &self.panels {
            if state.buffer_id != buffer_id {
                continue;
            }
            for hit in &state.hits {
                if hit.overlay == on_overlay
                    && hit.buffer_row == row
                    && (col_byte as usize) >= hit.byte_start
                    && (col_byte as usize) < hit.byte_end
                {
                    return Some((key.clone(), hit.clone()));
                }
            }
        }
        None
    }

    /// Resolve a click at `(row, col_byte)` to the hit that should
    /// receive it, making list/tree rows clickable across their *full
    /// width* rather than only where their text happens to reach.
    ///
    /// Prefers an exact byte-ranged hit (button, toggle, text field,
    /// tree disclosure/checkbox, or an in-text row body). When the click
    /// lands *past* a list/tree row's text — a compact row is far
    /// narrower than its panel — it falls back to that row's body
    /// `select` hit ([`row_select_hit`](Self::row_select_hit)), so the
    /// whole row is a target.
    ///
    /// **This is the text projection's resolver, and it should stay one
    /// surface's.** It was every click path's: while a panel's interior was
    /// painted rows, a press anywhere — dock, floating modal, mounted pane —
    /// arrived as a screen cell and had to be turned back into a widget by
    /// scanning these ranges. A described surface does not, because its
    /// widgets are nodes; what is left is the class of panel whose rows
    /// genuinely *are* buffer lines, riding the buffer's own scroll, whose
    /// cursor and `mouse_click` coordinates are a contract plugins read
    /// (`Editor::pane_panel_owns_its_scroll`, `plugins/git_log.ts`). For
    /// those, `(row, col_byte)` is not a second layout — it is the only
    /// coordinate space there is, and this scan is the right way to answer in
    /// it. `app::click_handlers` is that caller.
    ///
    /// (`Editor::probe_floating_widget` compiled against it until S7, for a
    /// dock or modal whose panel the adapter could not describe. It is gone:
    /// `render_floating_widget_panel` records `last_inner_rect` only for a
    /// *described* panel or the web's no-paint pass, while the two facts that
    /// reached the probe — `DockPress`, and `ModalPointer` from the panel's
    /// own box — were emitted only when the interior was **not** described.
    /// No path could reach the probe with a rectangle to test against, and
    /// the stored overlay rows it read went with it.)
    ///
    /// The caller holds a byte in a *composed row* and rebases it by the
    /// matched hit's `byte_start` before dispatch: `valueInnerStart` and
    /// `deliver_widget_hit` are both in the widget's own row's space.
    ///
    /// The "a row is clickable across its width" invariant lives here rather
    /// than at each call site. It regressed once precisely because it did not:
    /// the right-click context path grew a `row_select_hit` fallback while
    /// the left-click path stayed byte-exact, so compact dock rows
    /// silently ignored left-clicks past their label.
    /// `on_overlay` = the pointer sits on a popup surface (decided by
    /// the panel's layout-box tree). Overlay surfaces are opaque: only
    /// hits the popup itself contributed are reachable, with no
    /// row-wide fallback — a click on its border/padding resolves to
    /// nothing and the caller swallows it, never reaching the rows
    /// the popup covers. Callers must map the click column to
    /// `col_byte` through the text of the surface they name.
    pub fn hit_test_row_aware(
        &self,
        buffer_id: BufferId,
        row: u32,
        col_byte: u32,
        on_overlay: bool,
    ) -> Option<(PanelKey, HitArea)> {
        if on_overlay {
            return self.surface_hit(buffer_id, row, col_byte, true);
        }
        self.surface_hit(buffer_id, row, col_byte, false)
            .or_else(|| self.row_select_hit(buffer_id, row, col_byte))
    }

    /// The row-body `select` hit of a list/tree row in `buffer_id`,
    /// regardless of column. Row-level gestures (a right-click context
    /// menu) target the ROW, not a byte — a compact tree row's text is
    /// much narrower than the panel, so a click past the text end has no
    /// byte-ranged hit to land on even though it is visually "on the
    /// row". Prefer [`hit_test_row_aware`](Self::hit_test_row_aware),
    /// which tries an exact hit first and only falls back to this.
    pub fn row_select_hit(
        &self,
        buffer_id: BufferId,
        row: u32,
        col_byte: u32,
    ) -> Option<(PanelKey, HitArea)> {
        // Two side-by-side lists (a Row of `labeledSection`s — a step rail
        // beside a prose pane) put two row hits on the SAME buffer row, so
        // "first hit on this row" would hand every click in the right-hand
        // column to the left-hand list.
        //
        // Pick the row hit *nearest* the click instead. Distance is zero when
        // the click is inside a hit's own span, so a single-column panel is
        // unaffected. Choosing by "last hit starting at or before the click"
        // was almost right, but resolved the seam between two columns — the
        // right-hand section's border cell — to the left-hand list, which is
        // the column the user visibly did not click.
        fn distance(hit: &HitArea, col: usize) -> usize {
            if col < hit.byte_start {
                hit.byte_start - col
            } else if col >= hit.byte_end {
                col - hit.byte_end + 1
            } else {
                0
            }
        }
        let col = col_byte as usize;
        let mut best: Option<(&PanelKey, &HitArea, usize)> = None;
        for (key, state) in &self.panels {
            if state.buffer_id != buffer_id {
                continue;
            }
            for hit in &state.hits {
                // Row-wide targets are a capability the KIND declares
                // on the hit (`row_target`): List/Tree row selects, and
                // markdown document line `focus` hits (their caret
                // placement competes too — without them, a click on the
                // seam beside a document column would resolve to a
                // *list* in the neighbouring column).
                if hit.buffer_row != row || !hit.event.row_target {
                    continue;
                }
                let d = distance(hit, col);
                // Ties go to the leftmost hit, so the panel's leading margin
                // keeps belonging to the first column.
                if best
                    .is_none_or(|(_, b, bd)| d < bd || (d == bd && hit.byte_start < b.byte_start))
                {
                    best = Some((key, hit, d));
                }
            }
        }
        best.map(|(k, h, _)| (k.clone(), h.clone()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn pk(id: PanelId) -> PanelKey {
        PanelKey::new("test-plugin", id)
    }

    fn empty_spec() -> WidgetSpec {
        WidgetSpec::Col {
            children: vec![],
            key: None,
        }
    }

    fn make_hit(row: u32, byte_start: usize, byte_end: usize, key: &str) -> HitArea {
        HitArea {
            overlay: false,
            buffer_row: row,
            byte_start,
            byte_end,
            event: WidgetEvent {
                row_target: false,
                context_click: false,
                widget_key: key.into(),
                widget_kind: "button",
                payload: json!({}),
                event_type: "activate",
                owner_key: None,
            },
        }
    }

    #[test]
    fn hit_test_finds_widget_inside_range() {
        let mut reg = WidgetRegistry::new();
        reg.mount(
            pk(42),
            BufferId(7),
            empty_spec(),
            vec![make_hit(0, 0, 5, "a"), make_hit(0, 7, 12, "b")],
            HashMap::new(),
            String::new(),
            Vec::new(),
            HashMap::new(),
            Vec::new(),
        );
        let hit = reg.hit_test(BufferId(7), 0, 8).expect("inside b");
        assert_eq!(hit.0, pk(42));
        assert_eq!(hit.1.event.widget_key, "b");
    }

    #[test]
    fn hit_test_returns_none_when_outside_range() {
        let mut reg = WidgetRegistry::new();
        reg.mount(
            pk(1),
            BufferId(0),
            empty_spec(),
            vec![make_hit(0, 0, 5, "a")],
            HashMap::new(),
            String::new(),
            Vec::new(),
            HashMap::new(),
            Vec::new(),
        );
        assert!(
            reg.hit_test(BufferId(0), 0, 5).is_none(),
            "byte_end is exclusive"
        );
        assert!(reg.hit_test(BufferId(0), 0, 100).is_none());
        assert!(reg.hit_test(BufferId(0), 1, 0).is_none(), "wrong row");
        assert!(reg.hit_test(BufferId(99), 0, 0).is_none(), "wrong buffer");
    }

    fn make_row_select_hit(row: u32, byte_end: usize, key: &str) -> HitArea {
        HitArea {
            overlay: false,
            buffer_row: row,
            byte_start: 0,
            byte_end,
            event: WidgetEvent {
                row_target: true,
                context_click: true,
                widget_key: key.into(),
                widget_kind: "tree",
                payload: json!({ "index": row as i64 }),
                event_type: "select",
                owner_key: None,
            },
        }
    }

    #[test]
    fn hit_test_row_aware_falls_back_to_row_body_past_text() {
        // A compact tree row's `select` hit only spans its (narrow) text.
        // A click *past* the text has no exact byte hit, but must still land
        // on the row — the regression this guards: left-clicks past a dock
        // row's label were dropped while right-clicks worked.
        let mut reg = WidgetRegistry::new();
        reg.mount(
            pk(3),
            BufferId(2),
            empty_spec(),
            vec![make_row_select_hit(0, 10, "session-a")],
            HashMap::new(),
            String::new(),
            Vec::new(),
            HashMap::new(),
            Vec::new(),
        );
        // Byte 10 is the exclusive end, so `hit_test` alone misses...
        assert!(reg.hit_test(BufferId(2), 0, 10).is_none());
        // ...but the row-aware resolver falls back to the row's body select.
        let (_, hit) = reg
            .hit_test_row_aware(BufferId(2), 0, 40, false)
            .expect("click past text still lands on the row");
        assert_eq!(hit.event.widget_key, "session-a");
        assert_eq!(hit.event.event_type, "select");
    }

    /// **The scan answers in the row's coordinate space, and says where the
    /// widget's own begins.**
    ///
    /// This is the whole of what geometry is still for, and the reason
    /// [`hit_test_row_aware`](WidgetRegistry::hit_test_row_aware) returns a
    /// `HitArea` rather than a bare [`WidgetEvent`]. Two fields can share one
    /// rendered line — Search and Replace do — and the container pass shifts
    /// each field's byte range by the line-so-far *without* shifting the
    /// payload its `focus` event carries, whose `valueInnerStart` stays
    /// measured from the field's own text. So a caller holding a byte in the
    /// composed line has to subtract the matched area's `byte_start` before
    /// handing it to `Editor::deliver_widget_hit`, and that subtraction is
    /// what the geometry is for.
    ///
    /// The identity half is what it always was: neither field's event knows
    /// or needs to know where on the line it was drawn.
    #[test]
    fn a_composed_row_names_where_each_field_s_own_row_begins() {
        let mut reg = WidgetRegistry::new();
        reg.mount(
            pk(11),
            BufferId(5),
            empty_spec(),
            // `[search]` at bytes 0..20 of the line, `[replace]` at 20..40.
            vec![make_hit(0, 0, 20, "search"), make_hit(0, 20, 40, "replace")],
            HashMap::new(),
            String::new(),
            Vec::new(),
            HashMap::new(),
            Vec::new(),
        );
        let bcol = 27u32;
        let (_, hit) = reg
            .hit_test_row_aware(BufferId(5), 0, bcol, false)
            .expect("byte 27 of the line is in the second field");
        assert_eq!(hit.event.widget_key, "replace");
        assert_eq!(
            (bcol as usize).saturating_sub(hit.byte_start),
            7,
            "byte 7 of the replace field's own row, which is the space its \
             `valueInnerStart` is measured in"
        );
    }

    #[test]
    fn hit_test_row_aware_prefers_exact_hit_over_row_fallback() {
        // An exact hit (e.g. a button embedded on the row) wins over the
        // row-body fallback so it isn't swallowed by the whole-row target.
        let mut reg = WidgetRegistry::new();
        reg.mount(
            pk(4),
            BufferId(3),
            empty_spec(),
            vec![make_hit(0, 0, 5, "btn"), make_row_select_hit(0, 12, "row")],
            HashMap::new(),
            String::new(),
            Vec::new(),
            HashMap::new(),
            Vec::new(),
        );
        let (_, hit) = reg
            .hit_test_row_aware(BufferId(3), 0, 2, false)
            .expect("on button");
        assert_eq!(hit.event.widget_key, "btn");
        assert_eq!(hit.event.event_type, "activate");
    }

    #[test]
    fn hit_test_row_aware_none_on_empty_row() {
        // A row with no hits at all (blank padding below the last item)
        // stays a no-op — the fallback must not invent a target.
        let mut reg = WidgetRegistry::new();
        reg.mount(
            pk(5),
            BufferId(4),
            empty_spec(),
            vec![make_row_select_hit(0, 8, "only-row")],
            HashMap::new(),
            String::new(),
            Vec::new(),
            HashMap::new(),
            Vec::new(),
        );
        assert!(reg.hit_test_row_aware(BufferId(4), 3, 0, false).is_none());
    }

    #[test]
    fn overlay_surface_is_opaque_and_separate() {
        let mut reg = WidgetRegistry::default();
        let mut base = make_hit(0, 0, 10, "under");
        base.event.event_type = "select";
        base.event.widget_kind = "list";
        let mut popup = make_hit(0, 2, 6, "option");
        popup.overlay = true;
        reg.mount(
            pk(9),
            BufferId(9),
            empty_spec(),
            vec![base, popup],
            HashMap::new(),
            String::new(),
            Vec::new(),
            HashMap::new(),
            Vec::new(),
        );
        // On the overlay surface only the popup's own hits resolve…
        let (_, hit) = reg
            .hit_test_row_aware(BufferId(9), 0, 3, true)
            .expect("popup option");
        assert_eq!(hit.event.widget_key, "option");
        // …and a miss (border/padding) is swallowed — no row fallback
        // to the covered list row.
        assert!(reg.hit_test_row_aware(BufferId(9), 0, 8, true).is_none());
        // On the base surface the popup's hits are invisible.
        let (_, hit) = reg
            .hit_test_row_aware(BufferId(9), 0, 3, false)
            .expect("covered row");
        assert_eq!(hit.event.widget_key, "under");
    }

    fn mount_with_list(reg: &mut WidgetRegistry, scroll: u32, sel: i32) {
        let mut states = HashMap::new();
        states.insert(
            "lst".to_string(),
            WidgetInstanceState::List {
                scroll_offset: scroll,
                selected_index: sel,
                item_height: 1,
                user_scrolled: false,
            },
        );
        reg.mount(
            pk(7),
            BufferId(0),
            empty_spec(),
            Vec::new(),
            states,
            String::new(),
            Vec::new(),
            HashMap::new(),
            Vec::new(),
        );
    }

    fn list_state(reg: &WidgetRegistry) -> (u32, i32) {
        match reg.instance_states(&pk(7)).unwrap().get("lst").unwrap() {
            WidgetInstanceState::List {
                scroll_offset,
                selected_index,
                ..
            } => (*scroll_offset, *selected_index),
            _ => panic!("not a list"),
        }
    }

    #[test]
    fn set_list_scroll_moves_view_only_not_selection() {
        // Mouse scroll moves the *view* and never the selection — even
        // when the selection (row 2) ends up above the dragged-to window
        // [10, 18). No move is reported, so no `select`/live-switch
        // fires; the selection is allowed to leave the visible range.
        let mut reg = WidgetRegistry::new();
        mount_with_list(&mut reg, 0, 2);
        let moved = reg.set_list_scroll(&pk(7), "lst", 10, 8);
        assert_eq!(moved, None);
        assert_eq!(list_state(&reg), (10, 2));
    }

    #[test]
    fn set_list_scroll_leaves_in_view_selection_untouched() {
        // Selection already inside the new window — offset updates,
        // selection stays, and no move is reported.
        let mut reg = WidgetRegistry::new();
        mount_with_list(&mut reg, 0, 12);
        let moved = reg.set_list_scroll(&pk(7), "lst", 10, 8); // window [10,18)
        assert_eq!(moved, None);
        assert_eq!(list_state(&reg), (10, 12));
    }

    #[test]
    fn set_list_scroll_ignores_selectionless_list() {
        // A display-only list (selected_index < 0) just scrolls; no
        // selection clamp, no reported move.
        let mut reg = WidgetRegistry::new();
        mount_with_list(&mut reg, 0, -1);
        let moved = reg.set_list_scroll(&pk(7), "lst", 5, 8);
        assert_eq!(moved, None);
        assert_eq!(list_state(&reg), (5, -1));
    }

    #[test]
    fn same_local_id_from_two_plugins_coexists() {
        // Panel ids are plugin-local: a second plugin mounting the same
        // local id must NOT evict the first plugin's panel, and the
        // hit-test must resolve each buffer's hit to its owning plugin.
        let mut reg = WidgetRegistry::new();
        reg.mount(
            PanelKey::new("alpha", 1),
            BufferId(10),
            empty_spec(),
            vec![make_hit(0, 0, 5, "a-btn")],
            HashMap::new(),
            String::new(),
            Vec::new(),
            HashMap::new(),
            Vec::new(),
        );
        let evicted = reg.mount(
            PanelKey::new("beta", 1),
            BufferId(20),
            empty_spec(),
            vec![make_hit(0, 0, 5, "b-btn")],
            HashMap::new(),
            String::new(),
            Vec::new(),
            HashMap::new(),
            Vec::new(),
        );
        assert!(evicted.is_none(), "beta:1 must not evict alpha:1");

        let (key_a, hit_a) = reg.hit_test(BufferId(10), 0, 2).expect("alpha hit");
        assert_eq!(key_a, PanelKey::new("alpha", 1));
        assert_eq!(hit_a.event.widget_key, "a-btn");
        let (key_b, hit_b) = reg.hit_test(BufferId(20), 0, 2).expect("beta hit");
        assert_eq!(key_b, PanelKey::new("beta", 1));
        assert_eq!(hit_b.event.widget_key, "b-btn");

        // Unmounting one plugin's panel leaves the other untouched.
        reg.unmount(&PanelKey::new("beta", 1));
        assert!(reg.hit_test(BufferId(20), 0, 2).is_none());
        assert!(reg.hit_test(BufferId(10), 0, 2).is_some());
    }

    #[test]
    fn unmount_clears_hits() {
        let mut reg = WidgetRegistry::new();
        reg.mount(
            pk(5),
            BufferId(2),
            empty_spec(),
            vec![make_hit(0, 0, 3, "x")],
            HashMap::new(),
            String::new(),
            Vec::new(),
            HashMap::new(),
            Vec::new(),
        );
        assert!(reg.hit_test(BufferId(2), 0, 1).is_some());
        reg.unmount(&pk(5));
        assert!(reg.hit_test(BufferId(2), 0, 1).is_none());
    }

    #[test]
    fn update_replaces_hits() {
        let mut reg = WidgetRegistry::new();
        reg.mount(
            pk(5),
            BufferId(2),
            empty_spec(),
            vec![make_hit(0, 0, 3, "old")],
            HashMap::new(),
            String::new(),
            Vec::new(),
            HashMap::new(),
            Vec::new(),
        );
        reg.update(
            &pk(5),
            empty_spec(),
            vec![make_hit(1, 4, 9, "new")],
            HashMap::new(),
            String::new(),
            Vec::new(),
            HashMap::new(),
            Vec::new(),
        )
        .expect("mounted");
        // Old hit gone; new hit visible.
        assert!(reg.hit_test(BufferId(2), 0, 1).is_none());
        let hit = reg.hit_test(BufferId(2), 1, 5).unwrap();
        assert_eq!(hit.1.event.widget_key, "new");
    }
}
