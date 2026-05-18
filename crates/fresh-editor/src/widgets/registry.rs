//! Panel registry — maps plugin-allocated `panel_id` to mounted spec
//! and hit-area data for click routing.
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

/// Global registry of mounted widget panels.
#[derive(Debug, Default)]
pub struct WidgetRegistry {
    panels: HashMap<PanelId, WidgetPanelState>,
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
        panel_id: PanelId,
        buffer_id: BufferId,
        spec: WidgetSpec,
        hits: Vec<HitArea>,
        instance_states: HashMap<String, WidgetInstanceState>,
        focus_key: String,
        tabbable: Vec<String>,
    ) -> Option<WidgetPanelState> {
        self.panels.insert(
            panel_id,
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
        panel_id: PanelId,
        spec: WidgetSpec,
        hits: Vec<HitArea>,
        instance_states: HashMap<String, WidgetInstanceState>,
        focus_key: String,
        tabbable: Vec<String>,
    ) -> Result<BufferId, ()> {
        match self.panels.get_mut(&panel_id) {
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
        panel_id: PanelId,
    ) -> Option<&HashMap<String, WidgetInstanceState>> {
        self.panels.get(&panel_id).map(|s| &s.instance_states)
    }

    /// Read-only access to the previous render's focus key.
    pub fn focus_key(&self, panel_id: PanelId) -> Option<&str> {
        self.panels.get(&panel_id).map(|s| s.focus_key.as_str())
    }

    /// Set the focus key directly (used by `widget_focus_advance`
    /// and click-driven focus moves). Updates the in-place state;
    /// the next render reads it via `focus_key()`.
    pub fn set_focus_key(&mut self, panel_id: PanelId, key: String) {
        if let Some(state) = self.panels.get_mut(&panel_id) {
            state.focus_key = key;
        }
    }

    /// Update side-effects (hits, instance_states, focus_key, tabbable)
    /// without taking ownership of the spec. Used by `rerender_widget_panel`
    /// after an in-place spec mutation: the spec in the registry is already
    /// current (mutation helpers like `append_tree_nodes_in_spec` mutate it
    /// in place), so cloning it back through `update()` just to write the
    /// same value would waste a 5 000-node deep clone for every IPC.
    pub fn update_side_effects(
        &mut self,
        panel_id: PanelId,
        hits: Vec<HitArea>,
        instance_states: HashMap<String, WidgetInstanceState>,
        focus_key: String,
        tabbable: Vec<String>,
    ) -> Result<BufferId, ()> {
        match self.panels.get_mut(&panel_id) {
            Some(state) => {
                state.hits = hits;
                state.instance_states = instance_states;
                state.focus_key = focus_key;
                state.tabbable = tabbable;
                Ok(state.buffer_id)
            }
            None => Err(()),
        }
    }

    /// Borrow the current spec + return the buffer id. Companion to
    /// `update_side_effects` — render with the borrow and then write
    /// back only the side-effects, avoiding the deep clone of the spec
    /// that `buffer_and_spec()` does.
    pub fn buffer_and_spec_ref(&self, panel_id: PanelId) -> Option<(BufferId, &WidgetSpec)> {
        self.panels.get(&panel_id).map(|s| (s.buffer_id, &s.spec))
    }

    /// Find the buffer and current spec for a panel — used by the
    /// dispatcher to re-render after a focus advance / activate
    /// command without the plugin needing to send an UpdateWidgetPanel.
    pub fn buffer_and_spec(&self, panel_id: PanelId) -> Option<(BufferId, WidgetSpec)> {
        self.panels
            .get(&panel_id)
            .map(|s| (s.buffer_id, s.spec.clone()))
    }

    /// Tear down a panel. Returns the buffer_id the panel was
    /// rendering into, so the caller can clear the buffer if it
    /// owns it.
    pub fn unmount(&mut self, panel_id: PanelId) -> Option<BufferId> {
        self.panels.remove(&panel_id).map(|s| s.buffer_id)
    }

    /// Read-only access to a panel's current state.
    pub fn get(&self, panel_id: PanelId) -> Option<&WidgetPanelState> {
        self.panels.get(&panel_id)
    }

    /// Mutable access — used by `WidgetCommand` handlers that
    /// update widget instance state (e.g. TextInput value/cursor)
    /// directly without round-tripping through the plugin.
    pub fn get_mut(&mut self, panel_id: PanelId) -> Option<&mut WidgetPanelState> {
        self.panels.get_mut(&panel_id)
    }

    /// All currently-mounted panel ids — useful for theme-change
    /// re-render passes (every panel re-renders against the new
    /// theme without plugin involvement).
    pub fn panel_ids(&self) -> Vec<PanelId> {
        self.panels.keys().copied().collect()
    }

    /// Panels rendering into `buffer_id`. Used by mouse-wheel
    /// routing to find which widget panel sits under the pointer.
    pub fn panels_for_buffer(&self, buffer_id: BufferId) -> Vec<PanelId> {
        self.panels
            .iter()
            .filter(|(_, s)| s.buffer_id == buffer_id)
            .map(|(pid, _)| *pid)
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
    ) -> Option<(PanelId, HitArea)> {
        for (pid, state) in &self.panels {
            if state.buffer_id != buffer_id {
                continue;
            }
            for hit in &state.hits {
                if hit.buffer_row == row
                    && (col_byte as usize) >= hit.byte_start
                    && (col_byte as usize) < hit.byte_end
                {
                    return Some((*pid, hit.clone()));
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
            42,
            BufferId(7),
            empty_spec(),
            vec![make_hit(0, 0, 5, "a"), make_hit(0, 7, 12, "b")],
            HashMap::new(),
            String::new(),
            Vec::new(),
        );
        let hit = reg.hit_test(BufferId(7), 0, 8).expect("inside b");
        assert_eq!(hit.0, 42);
        assert_eq!(hit.1.widget_key, "b");
    }

    #[test]
    fn hit_test_returns_none_when_outside_range() {
        let mut reg = WidgetRegistry::new();
        reg.mount(
            1,
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

    #[test]
    fn unmount_clears_hits() {
        let mut reg = WidgetRegistry::new();
        reg.mount(
            5,
            BufferId(2),
            empty_spec(),
            vec![make_hit(0, 0, 3, "x")],
            HashMap::new(),
            String::new(),
            Vec::new(),
        );
        assert!(reg.hit_test(BufferId(2), 0, 1).is_some());
        reg.unmount(5);
        assert!(reg.hit_test(BufferId(2), 0, 1).is_none());
    }

    #[test]
    fn update_replaces_hits() {
        let mut reg = WidgetRegistry::new();
        reg.mount(
            5,
            BufferId(2),
            empty_spec(),
            vec![make_hit(0, 0, 3, "old")],
            HashMap::new(),
            String::new(),
            Vec::new(),
        );
        reg.update(
            5,
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
