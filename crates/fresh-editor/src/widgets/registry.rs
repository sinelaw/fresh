//! Panel registry — maps a panel's composite identity (owning plugin,
//! plugin-local `panel_id`) to mounted spec and hit-area data for click
//! routing.
//!
//! The registry is the source of truth for "which panels exist, what
//! spec are they currently rendering, and which buffer rows belong
//! to which widget." It does *not* own the virtual buffer the
//! rendered output goes into — the plugin still owns the virtual
//! buffer and passes its `BufferId` at mount time.

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

/// One clickable rectangle within a rendered widget panel.
///
/// The renderer produces one `HitArea` per interactive widget node
/// (`Toggle`, `Button` in v1). Layout containers (`Row`, `Col`,
/// `Spacer`, `HintBar`, `Raw`) emit no hit areas of their own; their
/// children's hit areas bubble up with row/byte offsets adjusted to
/// reflect the final on-screen position.
///
/// Hit-test is `(buffer_row, buffer_col_byte) ∈ rectangle`; the byte
/// range is in UTF-8 bytes within the row's text, matching the
/// coordinate space `mouse_click` already delivers
/// (`HookArgs::MouseClick::buffer_col`).
#[derive(Debug, Clone)]
pub struct HitArea {
    /// Stable widget key from the spec, or empty when the spec did
    /// not assign one.
    pub widget_key: String,
    /// Widget kind discriminator: `"toggle"` or `"button"`.
    pub widget_kind: &'static str,
    /// 0-indexed row within the rendered virtual buffer.
    pub buffer_row: u32,
    /// First UTF-8 byte (inclusive) within the row's text.
    pub byte_start: usize,
    /// Last UTF-8 byte (exclusive) within the row's text.
    pub byte_end: usize,
    /// Event payload to deliver with the `widget_event` hook.
    /// For `"toggle"`: `{ "checked": <new value> }`. For
    /// `"button"`: `{}`.
    pub payload: serde_json::Value,
    /// Event type to deliver with the `widget_event` hook
    /// (`"toggle"` or `"activate"`).
    pub event_type: &'static str,
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
    /// scroll offset that's only meaningful for multi-line
    /// (`rows > 1`) variants — the row index of the first visible
    /// line. Single-line text widgets always render from value
    /// byte 0 and rely on render-time head-truncate scrolling, so
    /// they leave `scroll` at `0`.
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
    /// Click rectangles for the rendered output, in declaration
    /// order. Hit-test scans linearly — the small N (one per
    /// interactive widget per panel) doesn't justify a spatial
    /// index.
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
    ) -> Result<BufferId, ()> {
        match self.panels.get_mut(panel_key) {
            Some(state) => {
                state.spec = spec;
                state.hits = hits;
                state.instance_states = instance_states;
                state.focus_key = focus_key;
                state.tabbable = tabbable;
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
    pub fn update_side_effects(
        &mut self,
        panel_key: &PanelKey,
        hits: Vec<HitArea>,
        instance_states: HashMap<String, WidgetInstanceState>,
        focus_key: String,
        tabbable: Vec<String>,
    ) -> Option<BufferId> {
        let state = self.panels.get_mut(panel_key)?;
        state.hits = hits;
        state.instance_states = instance_states;
        state.focus_key = focus_key;
        state.tabbable = tabbable;
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
        for (key, state) in &self.panels {
            if state.buffer_id != buffer_id {
                continue;
            }
            for hit in &state.hits {
                if hit.buffer_row == row
                    && (col_byte as usize) >= hit.byte_start
                    && (col_byte as usize) < hit.byte_end
                {
                    return Some((key.clone(), hit.clone()));
                }
            }
        }
        None
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
            widget_key: key.into(),
            widget_kind: "button",
            buffer_row: row,
            byte_start,
            byte_end,
            payload: json!({}),
            event_type: "activate",
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
        );
        let hit = reg.hit_test(BufferId(7), 0, 8).expect("inside b");
        assert_eq!(hit.0, pk(42));
        assert_eq!(hit.1.widget_key, "b");
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
        );
        assert!(
            reg.hit_test(BufferId(0), 0, 5).is_none(),
            "byte_end is exclusive"
        );
        assert!(reg.hit_test(BufferId(0), 0, 100).is_none());
        assert!(reg.hit_test(BufferId(0), 1, 0).is_none(), "wrong row");
        assert!(reg.hit_test(BufferId(99), 0, 0).is_none(), "wrong buffer");
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
        );
        let evicted = reg.mount(
            PanelKey::new("beta", 1),
            BufferId(20),
            empty_spec(),
            vec![make_hit(0, 0, 5, "b-btn")],
            HashMap::new(),
            String::new(),
            Vec::new(),
        );
        assert!(evicted.is_none(), "beta:1 must not evict alpha:1");

        let (key_a, hit_a) = reg.hit_test(BufferId(10), 0, 2).expect("alpha hit");
        assert_eq!(key_a, PanelKey::new("alpha", 1));
        assert_eq!(hit_a.widget_key, "a-btn");
        let (key_b, hit_b) = reg.hit_test(BufferId(20), 0, 2).expect("beta hit");
        assert_eq!(key_b, PanelKey::new("beta", 1));
        assert_eq!(hit_b.widget_key, "b-btn");

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
        );
        reg.update(
            &pk(5),
            empty_spec(),
            vec![make_hit(1, 4, 9, "new")],
            HashMap::new(),
            String::new(),
            Vec::new(),
        )
        .expect("mounted");
        // Old hit gone; new hit visible.
        assert!(reg.hit_test(BufferId(2), 0, 1).is_none());
        let hit = reg.hit_test(BufferId(2), 1, 5).unwrap();
        assert_eq!(hit.1.widget_key, "new");
    }
}
