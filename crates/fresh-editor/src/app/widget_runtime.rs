//! Shared widget / floating-panel runtime methods on `Editor`.
//!
//! This module holds the editor-side widget runtime that backs both the
//! plugin widget API and the built-in UI. It is intentionally NOT gated
//! behind the `plugins` feature: these methods are invoked from non-plugin
//! input/mouse/lifecycle code and must compile in plugin-less builds.
//!
//! The plugin-only command dispatch (`handle_plugin_command` and the
//! per-command handlers reachable only from it) lives in the
//! `plugins`-gated `plugin_dispatch` / `plugin_commands` modules.

use crate::model::event::{BufferId, LeafId, SplitId};

use super::Editor;

/// Walk a `Tree`'s flat `nodes` and return the absolute indices of
/// nodes that are currently visible — i.e. every ancestor is in
/// `expanded`. Mirrors the renderer's filter so dispatcher and
/// renderer agree on what's selectable.
/// First `Tree` or `List` widget key in `spec`, scanning in
/// declaration order. Used by mouse-wheel routing to pick which
/// widget inside a panel absorbs the scroll.
fn find_scrollable_widget_key(spec: &fresh_core::api::WidgetSpec) -> Option<String> {
    use fresh_core::api::WidgetSpec;
    match spec {
        WidgetSpec::Tree { key: Some(k), .. } | WidgetSpec::List { key: Some(k), .. }
            if !k.is_empty() =>
        {
            return Some(k.clone());
        }
        _ => {}
    }
    spec.children().find_map(find_scrollable_widget_key)
}

fn collect_visible_tree_indices(
    nodes: &[fresh_core::api::TreeNode],
    item_keys: &[String],
    expanded: &std::collections::HashSet<String>,
) -> Vec<usize> {
    let mut ancestor_open: Vec<bool> = Vec::new();
    let mut visible: Vec<usize> = Vec::with_capacity(nodes.len());
    for (i, node) in nodes.iter().enumerate() {
        let depth = node.depth as usize;
        ancestor_open.truncate(depth);
        if ancestor_open.iter().all(|open| *open) {
            visible.push(i);
        }
        let key = item_keys.get(i).cloned().unwrap_or_default();
        let is_open = if node.has_children {
            !key.is_empty() && expanded.contains(&key)
        } else {
            true
        };
        ancestor_open.push(is_open);
    }
    visible
}

/// Translate the plugin-facing animation description to the internal
/// `AnimationKind` the runner consumes.
pub(super) fn translate_plugin_animation_kind(
    kind: fresh_core::api::PluginAnimationKind,
) -> crate::view::animation::AnimationKind {
    use crate::view::animation::{AnimationKind, Edge};
    use fresh_core::api::{PluginAnimationEdge, PluginAnimationKind};
    use std::time::Duration;
    match kind {
        PluginAnimationKind::SlideIn {
            from,
            duration_ms,
            delay_ms,
        } => AnimationKind::SlideIn {
            from: match from {
                PluginAnimationEdge::Top => Edge::Top,
                PluginAnimationEdge::Bottom => Edge::Bottom,
                PluginAnimationEdge::Left => Edge::Left,
                PluginAnimationEdge::Right => Edge::Right,
            },
            duration: Duration::from_millis(duration_ms as u64),
            delay: Duration::from_millis(delay_ms as u64),
        },
    }
}

impl Editor {
    /// Process a resolved widget hit (from a TUI cell click or a native-frontend
    /// click): move focus to the clicked widget, apply host-owned state changes
    /// (tree expand / list selection) and fire the plugin's `widget_event`. This
    /// is the single dispatch path shared by the buffer-cell click handler and
    /// the web `/widget` route, so a click delivers identical behaviour in both.
    pub(crate) fn deliver_widget_hit(
        &mut self,
        panel_key: &crate::widgets::PanelKey,
        hit: &crate::widgets::HitArea,
    ) {
        // Click-to-focus: if the clicked widget has a stable, tabbable key, move
        // focus there before firing the event so the next render reflects it.
        if !hit.widget_key.is_empty() {
            let is_tabbable = self
                .widget_registry
                .get(panel_key)
                .map(|p| p.tabbable.iter().any(|k| k == &hit.widget_key))
                .unwrap_or(false);
            if is_tabbable {
                self.set_panel_focus_and_notify(panel_key, hit.widget_key.clone());
            }
            self.rerender_widget_panel(panel_key);
        }
        // Tree disclosure click: the host owns expansion state, so toggle it
        // (the toggle handler fires its own `expand` event with the post-toggle
        // state). Tree row-body (`select`) and other kinds fall through.
        let mut handled_specially = false;
        if hit.widget_kind == "tree" && hit.event_type == "expand" {
            if let Some(item_key) = hit.payload.get("key").and_then(|v| v.as_str()) {
                self.handle_widget_tree_expand_toggle(panel_key, &hit.widget_key, item_key);
                handled_specially = true;
            }
        }
        // List row click: the host owns the List's selected index; a click only
        // yields a `select` hit, so sync the selection (and repaint) then fall
        // through to fire `select` with the List's *spec* key (per-item key stays
        // in payload) — identical to keyboard nav.
        let mut event_widget_key = hit.widget_key.clone();
        if hit.widget_kind == "list" && hit.event_type == "select" {
            if let Some(list_key) = hit.payload.get("list_key").and_then(|v| v.as_str()) {
                event_widget_key = list_key.to_string();
                if let Some(idx) = hit.payload.get("index").and_then(|v| v.as_i64()) {
                    self.set_widget_list_selected_index(panel_key, list_key, idx as i32);
                }
            }
        }
        if !handled_specially {
            self.fire_widget_event(
                panel_key,
                event_widget_key,
                hit.event_type.to_string(),
                hit.payload.clone(),
            );
        }
    }

    /// Native-frontend entry point: deliver the hit at `hit_index` in panel
    /// `(plugin, panel_id)`'s recorded hit list — the same hits `widgets_view`
    /// shipped to the frontend. Runs the shared `deliver_widget_hit` path.
    pub fn deliver_widget_hit_by_index(&mut self, plugin: &str, panel_id: u64, hit_index: usize) {
        let panel_key = crate::widgets::PanelKey::new(plugin, panel_id);
        let hit = self
            .widget_registry
            .get(&panel_key)
            .and_then(|p| p.hits.get(hit_index).cloned());
        if let Some(hit) = hit {
            self.deliver_widget_hit(&panel_key, &hit);
        }
    }

    /// Deliver a `widget_event` hook to the plugin owning `panel_key` —
    /// and to that plugin only. Panel ids are plugin-local, so the event
    /// carries the bare id; no other plugin ever sees it.
    pub(crate) fn fire_widget_event(
        &self,
        panel_key: &crate::widgets::PanelKey,
        widget_key: String,
        event_type: String,
        payload: serde_json::Value,
    ) {
        let pm = self.plugin_manager.read().unwrap();
        if !pm.has_hook_handlers("widget_event") {
            return;
        }
        pm.run_hook_for_plugin(
            &panel_key.plugin,
            "widget_event",
            fresh_core::hooks::HookArgs::WidgetEvent {
                panel_id: panel_key.id,
                widget_key,
                event_type,
                payload,
            },
        );
    }

    /// Apply a `RenderOutput`'s focus-cursor position to the panel
    /// buffer + every split rendering it. When a `TextInput` is
    /// focused, the dispatcher flips `show_cursors=true` and moves
    /// the primary cursor to the right byte. When no TextInput is
    /// focused, the cursor is hidden (`show_cursors=false`) — the
    /// focused widget's own bg overlay shows where focus is.
    ///
    /// Must be called *after* `set_virtual_buffer_content` so the
    /// buffer's text matches the row/byte coordinates the renderer
    /// produced.
    pub(super) fn apply_widget_focus_cursor(
        &mut self,
        buffer_id: BufferId,
        entries: &[fresh_core::text_property::TextPropertyEntry],
        focus_cursor: Option<crate::widgets::FocusCursor>,
    ) {
        // If the plugin has taken explicit control of this buffer's cursor
        // (via `setBufferShowCursors`), the widget runtime must not touch
        // its visibility or position — the plugin owns it. This lets a
        // widget-panel pane be cursor-driven (e.g. git log's commit list)
        // without each repaint clearing the cursor.
        let locked = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.get(&buffer_id))
            .map(|s| s.cursor_visibility_locked)
            .unwrap_or(false);
        if locked {
            return;
        }

        let absolute_byte = focus_cursor.map(|fc| {
            let row = fc.buffer_row as usize;
            let prefix: usize = entries.iter().take(row).map(|e| e.text.len()).sum();
            prefix + fc.byte_in_row as usize
        });

        if let Some(state) = self
            .windows
            .get_mut(&self.active_window)
            .map(|w| &mut w.buffers)
            .expect("active window present")
            .get_mut(&buffer_id)
        {
            state.show_cursors = absolute_byte.is_some();
        }

        if let Some(byte) = absolute_byte {
            for vs in self
                .windows
                .get_mut(&self.active_window)
                .and_then(|w| w.split_view_states_mut())
                .expect("active window must have a populated split layout")
                .values_mut()
            {
                if vs.buffer_state(buffer_id).is_some() {
                    let cursor = vs.cursors.primary_mut();
                    cursor.position = byte;
                }
            }
        }
    }

    /// Best-effort width for a buffer's containing split. Returns
    /// the most recent `SplitViewState::viewport.width` for any
    /// split rendering this buffer; falls back to terminal width
    /// when the buffer hasn't been rendered yet (e.g. mid-mount).
    /// Subtracts 2 columns to account for gutter/scrollbar/border
    /// padding the renderer adds — leaving the right edge clear
    /// instead of pushing content into the chrome. This is what
    /// flex `Spacer`s inside `Row` use to size their fill.
    pub(super) fn widget_panel_width(&self, buffer_id: BufferId) -> u32 {
        let raw = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .values()
            .find(|vs| vs.buffer_state(buffer_id).is_some() && vs.viewport.width > 0)
            .map(|vs| vs.viewport.width as u32)
            .unwrap_or_else(|| self.terminal_width.max(1) as u32);
        // Reserve 2 cols for gutter/scrollbar/border. Saturate to
        // avoid 0 width on tiny panels.
        raw.saturating_sub(2).max(10)
    }

    /// Re-render an existing widget panel after an in-host state
    /// change (focus advance, scroll move, etc.) without the plugin
    /// re-emitting the spec. Reads the panel's current spec from
    /// the registry, runs `render_spec` against the (possibly
    /// updated) prev state / focus key, writes the result back.
    pub(super) fn rerender_widget_panel(&mut self, panel_key: &crate::widgets::PanelKey) {
        // The spec already lives in the registry — mutations (e.g.
        // `append_tree_nodes_in_spec`) edit it in place. Borrow it for
        // render, then write back only the side-effects (hits, instance
        // states, focus key, tabbable). The previous shape cloned the
        // whole spec out, rendered, then moved it back — for a Tree
        // with 5 000 nodes that's a multi-MB deep clone per IPC, which
        // dominates the host's per-mutation cost during a streaming
        // search.
        let (buffer_id, _is_floating, panel_width, out_pieces) = {
            let (buffer_id, spec) = match self.widget_registry.buffer_and_spec_ref(panel_key) {
                Some(s) => s,
                None => return,
            };
            let prev = self
                .widget_registry
                .instance_states(panel_key)
                .cloned()
                .unwrap_or_default();
            let prev_focus = self
                .widget_registry
                .focus_key(panel_key)
                .map(|s| s.to_string())
                .unwrap_or_default();
            let panel_slot = Self::slot_for_panel_buffer(buffer_id);
            let is_floating = panel_slot.is_some();
            let panel_width = if let Some(slot) = panel_slot {
                self.floating_panel_inner_width(slot)
            } else {
                self.widget_panel_width(buffer_id)
            };
            let out = crate::widgets::render_spec(spec, &prev, &prev_focus, panel_width);
            (buffer_id, is_floating, panel_width, out)
        };
        let _ = panel_width;
        let panel_slot = Self::slot_for_panel_buffer(buffer_id);
        let focus_cursor = out_pieces.focus_cursor;
        let entries = out_pieces.entries;
        let embeds = out_pieces.embeds;
        let overlays = out_pieces.overlays;
        let scroll_regions = out_pieces.scroll_regions;
        if self
            .widget_registry
            .update_side_effects(
                panel_key,
                out_pieces.hits,
                out_pieces.instance_states,
                out_pieces.focus_key,
                out_pieces.tabbable,
            )
            .is_err()
        {
            tracing::warn!("rerender_widget_panel({}) lost panel mid-call", panel_key);
            return;
        }
        if let Some(slot) = panel_slot {
            if let Some(fwp) = self.panel_mut(slot) {
                if &fwp.panel_key == panel_key {
                    fwp.entries = entries;
                    fwp.focus_cursor = focus_cursor;
                    fwp.embeds = embeds;
                    fwp.overlays = overlays;
                    fwp.scroll_regions = scroll_regions;
                }
            }
            return;
        }
        if let Err(e) = self.set_virtual_buffer_content(buffer_id, entries.clone()) {
            tracing::error!("rerender_widget_panel({}) failed: {}", panel_key, e);
        }
        self.apply_widget_focus_cursor(buffer_id, &entries, focus_cursor);
    }

    pub(super) fn handle_widget_command(
        &mut self,
        panel_key: &crate::widgets::PanelKey,
        action: fresh_core::api::WidgetAction,
    ) {
        use fresh_core::api::WidgetAction;
        match action {
            WidgetAction::FocusAdvance { delta } => {
                self.handle_widget_focus_advance(panel_key, delta);
            }
            WidgetAction::Activate => {
                self.handle_widget_activate(panel_key);
            }
            WidgetAction::SelectMove { delta } => {
                self.handle_widget_select_move(panel_key, delta);
            }
            WidgetAction::TextInputKey { key } => {
                self.handle_widget_text_key(panel_key, &key);
            }
            WidgetAction::TextInputChar { text } => {
                self.handle_widget_text_char(panel_key, &text);
            }
            WidgetAction::Key { key } => {
                self.handle_widget_key(panel_key, &key);
            }
        }
    }

    fn handle_widget_key(&mut self, panel_key: &crate::widgets::PanelKey, key: &str) {
        // Smart key dispatch — route to the right specialized
        // handler based on focused widget kind. See WidgetAction::Key
        // doc for the dispatch table.
        let panel = match self.widget_registry.get(panel_key) {
            Some(p) => p,
            None => return,
        };
        let focus_key = panel.focus_key.clone();
        let widget = if focus_key.is_empty() {
            None
        } else {
            crate::widgets::find_widget_by_key(&panel.spec, &focus_key)
        };
        // Completion-popup short-circuit: when the focused Text
        // widget has an open completion popup, intercept Tab /
        // Up / Down / Enter / Esc so they drive the popup instead
        // of falling through to the widget's default key
        // behaviour. Tab fires `completion_accept`, Enter/Esc
        // dismiss, Up/Down move the host-managed selection. Any
        // other key (printable, Backspace, etc.) still goes to
        // the text editor, which lets the user keep typing to
        // refine the candidate list.
        let completions_open = matches!(key, "Tab" | "Up" | "Down" | "Enter" | "Escape")
            && self.focused_text_completions_open(panel_key);
        if completions_open {
            match key {
                "Tab" => {
                    self.fire_completion_accept(panel_key);
                    // The plugin's accept handler typically calls
                    // setValue + (maybe) setCompletions — those
                    // mutations re-render on their own, so we
                    // don't force a render here.
                    return;
                }
                "Up" => {
                    self.move_focused_text_completion_index(panel_key, -1);
                    // Selection moved host-side; force a repaint
                    // so the highlight + scroll-into-view shift
                    // is visible without waiting for the next
                    // unrelated mutation.
                    self.rerender_widget_panel(panel_key);
                    return;
                }
                "Down" => {
                    self.move_focused_text_completion_index(panel_key, 1);
                    self.rerender_widget_panel(panel_key);
                    return;
                }
                "Enter" | "Escape" => {
                    self.dismiss_focused_text_completions(panel_key);
                    self.rerender_widget_panel(panel_key);
                    return;
                }
                _ => {}
            }
        }
        match key {
            "Tab" => self.handle_widget_focus_advance(panel_key, 1),
            "Shift+Tab" => self.handle_widget_focus_advance(panel_key, -1),
            "Up" | "Down" => {
                let delta = if key == "Up" { -1 } else { 1 };
                match widget {
                    Some(fresh_core::api::WidgetSpec::List { .. }) => {
                        self.handle_widget_select_move(panel_key, delta);
                    }
                    Some(fresh_core::api::WidgetSpec::Tree { .. }) => {
                        self.handle_widget_tree_select_move(panel_key, delta);
                    }
                    Some(fresh_core::api::WidgetSpec::Text { rows, .. }) if *rows > 1 => {
                        // Multi-line Text: line nav. Single-line
                        // is filtered out — TextEdit::move_up /
                        // move_down would no-op on the single
                        // line, but skipping the dispatch keeps
                        // the change-event quiet.
                        self.handle_widget_text_key(panel_key, key);
                    }
                    _ => {
                        // Picker-style nav: when the focused widget
                        // doesn't have a meaningful Up/Down (single-
                        // line Text, Button, Toggle, or no focus),
                        // route the arrow to the first scrollable
                        // widget in the panel. Lets a filter input
                        // stay focused for typing while arrows
                        // navigate the adjacent list.
                        let scrollable = self
                            .widget_registry
                            .get(panel_key)
                            .and_then(|p| find_scrollable_widget_key(&p.spec));
                        if let Some(target_key) = scrollable {
                            let target_kind = self.widget_registry.get(panel_key).and_then(|p| {
                                crate::widgets::find_widget_by_key(&p.spec, &target_key).cloned()
                            });
                            match target_kind {
                                Some(fresh_core::api::WidgetSpec::List { .. }) => {
                                    self.handle_widget_select_move_for_key(
                                        panel_key,
                                        &target_key,
                                        delta,
                                    );
                                }
                                Some(fresh_core::api::WidgetSpec::Tree { .. }) => {
                                    self.handle_widget_tree_select_move_for_key(
                                        panel_key,
                                        &target_key,
                                        delta,
                                    );
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }
            "PageUp" | "PageDown" => {
                // Page step = visible_rows - 1 (one row of overlap so
                // the user keeps a visual anchor across pages). Ignored
                // for non-scrollable widgets.
                let page = match widget {
                    Some(fresh_core::api::WidgetSpec::List { visible_rows, .. })
                    | Some(fresh_core::api::WidgetSpec::Tree { visible_rows, .. }) => {
                        visible_rows.saturating_sub(1).max(1) as i32
                    }
                    _ => 0,
                };
                if page == 0 {
                    return;
                }
                let delta = if key == "PageUp" { -page } else { page };
                match widget {
                    Some(fresh_core::api::WidgetSpec::List { .. }) => {
                        self.handle_widget_select_move(panel_key, delta);
                    }
                    Some(fresh_core::api::WidgetSpec::Tree { .. }) => {
                        self.handle_widget_tree_select_move(panel_key, delta);
                    }
                    _ => {}
                }
            }
            "Left" | "Right" => match widget {
                Some(fresh_core::api::WidgetSpec::Text { .. }) => {
                    self.handle_widget_text_key(panel_key, key);
                }
                Some(fresh_core::api::WidgetSpec::Tree { .. }) => {
                    self.handle_widget_tree_lateral(panel_key, key == "Right");
                }
                _ => {}
            },
            "Backspace" | "Delete" | "Home" | "End" => match widget {
                Some(fresh_core::api::WidgetSpec::Text { .. }) => {
                    self.handle_widget_text_key(panel_key, key);
                }
                _ => {}
            },
            "Enter" => match widget {
                Some(fresh_core::api::WidgetSpec::Button { .. })
                | Some(fresh_core::api::WidgetSpec::Toggle { .. }) => {
                    self.handle_widget_activate(panel_key);
                }
                Some(fresh_core::api::WidgetSpec::List { .. }) => {
                    self.fire_list_activate(panel_key, &focus_key);
                }
                Some(fresh_core::api::WidgetSpec::Tree { .. }) => {
                    self.fire_tree_activate(panel_key, &focus_key);
                }
                Some(fresh_core::api::WidgetSpec::Text { rows, .. }) => {
                    if *rows > 1 {
                        // Multi-line: Enter inserts a newline at the
                        // cursor. Plugins that want Enter to submit
                        // can intercept it in their mode binding
                        // before dispatching through the smart-key
                        // router.
                        self.handle_widget_text_key(panel_key, "Enter");
                    } else if let Some(target_key) = self
                        .widget_registry
                        .get(panel_key)
                        .and_then(|p| find_scrollable_widget_key(&p.spec))
                    {
                        // Picker-style activate: a single-line filter
                        // input paired with a List/Tree fires that
                        // scrollable's activate event on Enter, so the
                        // user can type-then-Enter without tabbing
                        // focus to the list.
                        let kind = self.widget_registry.get(panel_key).and_then(|p| {
                            crate::widgets::find_widget_by_key(&p.spec, &target_key).cloned()
                        });
                        match kind {
                            Some(fresh_core::api::WidgetSpec::List { .. }) => {
                                self.fire_list_activate(panel_key, &target_key);
                            }
                            Some(fresh_core::api::WidgetSpec::Tree { .. }) => {
                                self.fire_tree_activate(panel_key, &target_key);
                            }
                            _ => {}
                        }
                    } else {
                        // Form-like UX: Enter commits the field and
                        // moves to the next tabbable widget.
                        self.handle_widget_focus_advance(panel_key, 1);
                    }
                }
                _ => {}
            },
            "Space" => match widget {
                Some(fresh_core::api::WidgetSpec::Button { .. })
                | Some(fresh_core::api::WidgetSpec::Toggle { .. }) => {
                    self.handle_widget_activate(panel_key);
                }
                Some(fresh_core::api::WidgetSpec::Text { .. }) => {
                    self.handle_widget_text_char(panel_key, " ");
                }
                Some(fresh_core::api::WidgetSpec::List { .. }) => {
                    self.fire_list_activate(panel_key, &focus_key);
                }
                Some(fresh_core::api::WidgetSpec::Tree { .. }) => {
                    // On a checkable Tree, Space is the conventional
                    // checkbox key — fire `toggle` for the focused row
                    // (matching what a click on its `[v]`/`[ ]` glyph
                    // would do). Falls back to `activate` for trees
                    // that aren't checkable, or rows that don't have
                    // a checkbox glyph (`checked: None`).
                    if !self.fire_tree_toggle_if_checkable(panel_key, &focus_key) {
                        self.fire_tree_activate(panel_key, &focus_key);
                    }
                }
                _ => {}
            },
            _ => {} // unrecognised key — quietly ignore
        }
    }

    fn handle_widget_focus_advance(&mut self, panel_key: &crate::widgets::PanelKey, delta: i32) {
        let panel = match self.widget_registry.get(panel_key) {
            Some(p) => p,
            None => return,
        };
        if panel.tabbable.is_empty() {
            return;
        }
        let cur_idx = panel
            .tabbable
            .iter()
            .position(|k| k == &panel.focus_key)
            .unwrap_or(0) as i32;
        let n = panel.tabbable.len() as i32;
        let new_idx = ((cur_idx + delta) % n + n) % n;
        let new_key = panel.tabbable[new_idx as usize].clone();
        self.set_panel_focus_and_notify(panel_key, new_key);
        self.rerender_widget_panel(panel_key);
    }

    /// Update the panel's focused widget AND fire a
    /// `widget_event { event_type: "focus" }` so plugins can
    /// react. Used by every host-driven focus move — key-driven
    /// Tab / Shift-Tab / Enter focus-advance, click-driven
    /// focus moves, etc. — so plugins never have to predict the
    /// host's focus rules to keep a local mirror in sync.
    ///
    /// No-op when the key isn't actually changing (avoids
    /// spurious events on every render that touches focus).
    pub(crate) fn set_panel_focus_and_notify(
        &mut self,
        panel_key: &crate::widgets::PanelKey,
        new_key: String,
    ) {
        let old_key = self
            .widget_registry
            .focus_key(panel_key)
            .map(|s| s.to_string())
            .unwrap_or_default();
        if old_key == new_key {
            tracing::debug!(
                target: "fresh::dock",
                panel = %panel_key,
                key = %new_key,
                "set_panel_focus_and_notify: no-op (old == new)"
            );
            return;
        }
        tracing::debug!(
            target: "fresh::dock",
            panel = %panel_key,
            old = %old_key,
            new = %new_key,
            "set_panel_focus_and_notify: firing `focus` widget_event"
        );
        self.widget_registry
            .set_focus_key(panel_key, new_key.clone());
        self.fire_widget_event(
            panel_key,
            new_key,
            "focus".to_string(),
            serde_json::json!({ "previous": old_key }),
        );
    }

    fn handle_widget_activate(&mut self, panel_key: &crate::widgets::PanelKey) {
        // Fire `widget_event` based on the focused widget's kind.
        // Button → "activate"; Toggle → "toggle" (with the
        // computed-new payload); other kinds: no-op.
        let panel = match self.widget_registry.get(panel_key) {
            Some(p) => p,
            None => return,
        };
        let focus_key = panel.focus_key.clone();
        if focus_key.is_empty() {
            return;
        }
        let widget = crate::widgets::find_widget_by_key(&panel.spec, &focus_key);
        let (event_type, payload) = match widget {
            // Disabled buttons don't fire activate. The renderer
            // already excludes them from the tab cycle and skips
            // their hit area, so the only way `focus_key` could
            // still point at a disabled button is a stale focus
            // from before the disable transition — drop the event
            // in that race.
            Some(fresh_core::api::WidgetSpec::Button { disabled: true, .. }) => return,
            Some(fresh_core::api::WidgetSpec::Button { .. }) => ("activate", serde_json::json!({})),
            Some(fresh_core::api::WidgetSpec::Toggle { checked, .. }) => {
                ("toggle", serde_json::json!({ "checked": !checked }))
            }
            _ => return,
        };
        self.fire_widget_event(panel_key, focus_key, event_type.to_string(), payload);
    }

    /// Fire a `widget_event { event_type: "activate", payload: {
    /// index, key } }` for the focused List, using its instance-state
    /// selection (or spec selection on first render). The plugin's
    /// activate handler does the actual user-visible thing — open
    /// the matched file, expand/collapse a tree node, etc.
    /// True when the focused widget on `panel_key` is a Text input
    /// whose host-managed completion popup is currently open
    /// (instance state has at least one candidate). Lets the
    /// smart-key dispatcher route Tab/Enter/Up/Down/Esc to the
    /// popup-specific paths before falling through to the
    /// widget's default key behaviour.
    fn focused_text_completions_open(&self, panel_key: &crate::widgets::PanelKey) -> bool {
        let panel = match self.widget_registry.get(panel_key) {
            Some(p) => p,
            None => return false,
        };
        if panel.focus_key.is_empty() {
            return false;
        }
        matches!(
            panel.instance_states.get(&panel.focus_key),
            Some(crate::widgets::WidgetInstanceState::Text { completions, .. })
                if !completions.is_empty()
        )
    }

    /// Move the selected-index cursor of the focused Text widget's
    /// completion popup by `delta` (Up = -1, Down = +1). Clamps
    /// at the ends rather than wrapping — Down past the last
    /// candidate stays on the last candidate, Up past the first
    /// stays on the first. Wraparound on a popup-style picker
    /// reads as "I scrolled past the bottom and now I'm at the
    /// top" which is jarring when the user is actively comparing
    /// items they expect to be in monotonic positions. No-op
    /// when the focused widget isn't a Text-with-open-
    /// completions.
    fn move_focused_text_completion_index(
        &mut self,
        panel_key: &crate::widgets::PanelKey,
        delta: i32,
    ) {
        // First read the spec's visible-rows cap so we can pull
        // scroll back into view if the new selection lands above
        // the current scroll offset. (The renderer only does
        // forward-pull — it would otherwise fight the mouse-
        // wheel handler which deliberately diverges scroll from
        // selection.)
        let panel = match self.widget_registry.get(panel_key) {
            Some(p) => p,
            None => return,
        };
        let focus_key = panel.focus_key.clone();
        if focus_key.is_empty() {
            return;
        }
        let spec_visible_rows = match crate::widgets::find_widget_by_key(&panel.spec, &focus_key) {
            Some(fresh_core::api::WidgetSpec::Text {
                completions_visible_rows,
                ..
            }) => *completions_visible_rows,
            _ => 0,
        };
        let visible = if spec_visible_rows == 0 {
            5u32
        } else {
            spec_visible_rows
        };
        let panel = match self.widget_registry.get_mut(panel_key) {
            Some(p) => p,
            None => return,
        };
        if let Some(crate::widgets::WidgetInstanceState::Text {
            completions,
            completion_selected_index,
            completion_scroll_offset,
            ..
        }) = panel.instance_states.get_mut(&focus_key)
        {
            if completions.is_empty() {
                return;
            }
            let max = (completions.len() - 1) as i32;
            let cur = *completion_selected_index as i32;
            let next = (cur + delta).clamp(0, max);
            *completion_selected_index = next as usize;
            // Keyboard-driven selection move: if the new
            // selection sits above the current scroll window,
            // pull the scroll back so the selection stays
            // visible. Forward-pull is handled by the renderer.
            let next_u = next as u32;
            if next_u < *completion_scroll_offset {
                *completion_scroll_offset = next_u;
            } else if next_u >= *completion_scroll_offset + visible {
                *completion_scroll_offset = next_u + 1 - visible;
            }
        }
    }

    /// Clear the focused Text widget's completion popup (close it)
    /// and fire a `completion_dismiss` event so the plugin can
    /// sync its own state (e.g. invalidate any in-flight fetch
    /// token, so a late-arriving result doesn't re-open the
    /// popup the user just closed). Used by Enter and Escape on
    /// a Text-with-open-completions.
    fn dismiss_focused_text_completions(&mut self, panel_key: &crate::widgets::PanelKey) {
        let focus_key = {
            let panel = match self.widget_registry.get_mut(panel_key) {
                Some(p) => p,
                None => return,
            };
            let focus_key = panel.focus_key.clone();
            if focus_key.is_empty() {
                return;
            }
            if let Some(crate::widgets::WidgetInstanceState::Text {
                completions,
                completion_selected_index,
                ..
            }) = panel.instance_states.get_mut(&focus_key)
            {
                if completions.is_empty() {
                    return;
                }
                completions.clear();
                *completion_selected_index = 0;
            } else {
                return;
            }
            focus_key
        };
        self.fire_widget_event(
            panel_key,
            focus_key,
            "completion_dismiss".into(),
            serde_json::json!({}),
        );
    }

    /// Fire `completion_accept` on the focused Text widget's
    /// currently-selected candidate. Used by Tab on a Text-with-
    /// open-completions — the plugin's handler is expected to
    /// apply the accepted value to the field (typically via
    /// `WidgetMutation::SetValue`). The host does NOT close the
    /// popup automatically: directory-descent style flows (the
    /// orchestrator's Project Path acceptance of `/foo/` re-
    /// fetches children for the new path) want the popup to
    /// stay alive so the user can keep Tab-ing. Plugins that
    /// want a one-shot accept close the popup themselves with
    /// `setCompletions(key, [])`.
    fn fire_completion_accept(&mut self, panel_key: &crate::widgets::PanelKey) {
        let (focus_key, value) = {
            let panel = match self.widget_registry.get(panel_key) {
                Some(p) => p,
                None => return,
            };
            let focus_key = panel.focus_key.clone();
            if focus_key.is_empty() {
                return;
            }
            match panel.instance_states.get(&focus_key) {
                Some(crate::widgets::WidgetInstanceState::Text {
                    completions,
                    completion_selected_index,
                    ..
                }) if !completions.is_empty() => {
                    let idx = (*completion_selected_index).min(completions.len() - 1);
                    (focus_key, completions[idx].value.clone())
                }
                _ => return,
            }
        };
        self.fire_widget_event(
            panel_key,
            focus_key,
            "completion_accept".into(),
            serde_json::json!({ "value": value }),
        );
    }

    fn fire_list_activate(&mut self, panel_key: &crate::widgets::PanelKey, focus_key: &str) {
        let panel = match self.widget_registry.get(panel_key) {
            Some(p) => p,
            None => return,
        };
        let widget = crate::widgets::find_widget_by_key(&panel.spec, focus_key);
        let (spec_sel, item_keys) = match widget {
            Some(fresh_core::api::WidgetSpec::List {
                selected_index,
                item_keys,
                ..
            }) => (*selected_index, item_keys.clone()),
            _ => return,
        };
        let sel = match panel.instance_states.get(focus_key) {
            Some(crate::widgets::WidgetInstanceState::List { selected_index, .. }) => {
                *selected_index
            }
            _ => spec_sel,
        };
        if sel < 0 {
            return;
        }
        let item_key = item_keys.get(sel as usize).cloned().unwrap_or_default();
        self.fire_widget_event(
            panel_key,
            focus_key.to_string(),
            "activate".into(),
            serde_json::json!({ "index": sel, "key": item_key, }),
        );
    }

    fn handle_widget_select_move(&mut self, panel_key: &crate::widgets::PanelKey, delta: i32) {
        let focus_key = match self.widget_registry.get(panel_key) {
            Some(p) => p.focus_key.clone(),
            None => return,
        };
        if focus_key.is_empty() {
            return;
        }
        self.handle_widget_select_move_for_key(panel_key, &focus_key, delta);
    }

    /// Set a `List` widget's selected index to an absolute item index,
    /// preserving its scroll offset, and repaint. Used by the click
    /// path: a row click only produces a `select` hit and — unlike
    /// keyboard nav via [`handle_widget_select_move_for_key`] — does
    /// not move the host-owned selection. Without this the highlight
    /// would not follow a click and a subsequent Up/Down would resume
    /// from the stale index.
    pub(super) fn set_widget_list_selected_index(
        &mut self,
        panel_key: &crate::widgets::PanelKey,
        widget_key: &str,
        index: i32,
    ) {
        if let Some(panel) = self.widget_registry.get_mut(panel_key) {
            let (prev_scroll, prev_item_height) = match panel.instance_states.get(widget_key) {
                Some(crate::widgets::WidgetInstanceState::List {
                    scroll_offset,
                    item_height,
                    ..
                }) => (*scroll_offset, *item_height),
                _ => (0, 1),
            };
            panel.instance_states.insert(
                widget_key.to_string(),
                crate::widgets::WidgetInstanceState::List {
                    scroll_offset: prev_scroll,
                    selected_index: index,
                    item_height: prev_item_height,
                    // A deliberate selection re-arms scroll-follows-selection.
                    user_scrolled: false,
                },
            );
        }
        self.rerender_widget_panel(panel_key);
    }

    /// Same as [`handle_widget_select_move`] but targets an explicit
    /// `List` widget key instead of the panel's focused widget. Used
    /// by the picker-style smart-key dispatch — `Up`/`Down` on a
    /// focused filter input route to the first scrollable widget in
    /// the panel without changing focus.
    fn handle_widget_select_move_for_key(
        &mut self,
        panel_key: &crate::widgets::PanelKey,
        widget_key: &str,
        delta: i32,
    ) {
        let panel = match self.widget_registry.get(panel_key) {
            Some(p) => p,
            None => return,
        };
        let widget = crate::widgets::find_widget_by_key(&panel.spec, widget_key);
        let (spec_sel, total, item_keys) = match widget {
            Some(fresh_core::api::WidgetSpec::List {
                selected_index,
                items,
                item_specs,
                item_keys,
                ..
            }) => {
                // Item count is in *items* (cards override the plain
                // `items` rows; see `WidgetSpec::List::item_specs`).
                let total = if item_specs.is_empty() {
                    items.len()
                } else {
                    item_specs.len()
                };
                (*selected_index, total as i32, item_keys.clone())
            }
            _ => return,
        };
        if total == 0 {
            return;
        }
        let cur_sel = match panel.instance_states.get(widget_key) {
            Some(crate::widgets::WidgetInstanceState::List { selected_index, .. }) => {
                *selected_index
            }
            _ => spec_sel,
        };
        let raw = if cur_sel < 0 { 0 } else { cur_sel + delta };
        let new_sel = raw.clamp(0, total - 1);
        let new_key = item_keys.get(new_sel as usize).cloned().unwrap_or_default();
        if let Some(panel_mut) = self.widget_registry.get_mut(panel_key) {
            let (cur_scroll, cur_item_height) = match panel_mut.instance_states.get(widget_key) {
                Some(crate::widgets::WidgetInstanceState::List {
                    scroll_offset,
                    item_height,
                    ..
                }) => (*scroll_offset, *item_height),
                _ => (0, 1),
            };
            panel_mut.instance_states.insert(
                widget_key.to_string(),
                crate::widgets::WidgetInstanceState::List {
                    scroll_offset: cur_scroll,
                    selected_index: new_sel,
                    item_height: cur_item_height,
                    // Keyboard nav re-arms scroll-follows-selection so the
                    // renderer brings the new selection back into view.
                    user_scrolled: false,
                },
            );
        }
        self.rerender_widget_panel(panel_key);
        // A clamped move at the list's top/bottom edge leaves the
        // selection where it was. Still re-render above (re-arming
        // `user_scrolled = false` snaps a scrolled-away view back to the
        // selection), but don't fire a `select` event for a no-op move:
        // holding ↑/↓ against the boundary would otherwise spam the
        // plugin with same-index selections — each one re-runs the
        // plugin's preview / live-switch work (in the Orchestrator dock
        // it schedules a redundant `scheduleDockSwitch`). Mirrors the
        // Tree handler's "No change → bail (don't fire spurious select)".
        if new_sel != cur_sel {
            self.fire_widget_event(
                panel_key,
                widget_key.to_string(),
                "select".into(),
                serde_json::json!({ "index": new_sel, "key": new_key }),
            );
        }
    }

    /// Move the focused Tree's selection up/down, skipping
    /// descendants of collapsed nodes. Selection is the *absolute*
    /// `nodes` index; we walk the visible-flat order to find the
    /// neighbour. Mirrors the List handler shape but tree-aware.
    fn handle_widget_tree_select_move(&mut self, panel_key: &crate::widgets::PanelKey, delta: i32) {
        let focus_key = match self.widget_registry.get(panel_key) {
            Some(p) => p.focus_key.clone(),
            None => return,
        };
        if focus_key.is_empty() {
            return;
        }
        self.handle_widget_tree_select_move_for_key(panel_key, &focus_key, delta);
    }

    /// Tree counterpart of [`handle_widget_select_move_for_key`].
    fn handle_widget_tree_select_move_for_key(
        &mut self,
        panel_key: &crate::widgets::PanelKey,
        widget_key: &str,
        delta: i32,
    ) {
        let panel = match self.widget_registry.get(panel_key) {
            Some(p) => p,
            None => return,
        };
        let widget = crate::widgets::find_widget_by_key(&panel.spec, widget_key);
        let (spec_sel, nodes, item_keys) = match widget {
            Some(fresh_core::api::WidgetSpec::Tree {
                selected_index,
                nodes,
                item_keys,
                ..
            }) => (*selected_index, nodes.clone(), item_keys.clone()),
            _ => return,
        };
        if nodes.is_empty() {
            return;
        }
        let (cur_sel, cur_scroll, expanded) = match panel.instance_states.get(widget_key) {
            Some(crate::widgets::WidgetInstanceState::Tree {
                selected_index,
                scroll_offset,
                expanded_keys,
            }) => (*selected_index, *scroll_offset, expanded_keys.clone()),
            _ => (spec_sel, 0u32, std::collections::HashSet::<String>::new()),
        };
        let visible_indices = collect_visible_tree_indices(&nodes, &item_keys, &expanded);
        if visible_indices.is_empty() {
            return;
        }
        let cur_pos = if cur_sel < 0 {
            if delta > 0 {
                -1
            } else {
                visible_indices.len() as i32
            }
        } else {
            visible_indices
                .iter()
                .position(|&v| v as i32 == cur_sel)
                .map(|p| p as i32)
                .unwrap_or(-1)
        };
        let new_pos = (cur_pos + delta).clamp(0, (visible_indices.len() as i32) - 1);
        let new_abs = visible_indices[new_pos as usize];
        let new_key = item_keys.get(new_abs).cloned().unwrap_or_default();
        if let Some(panel_mut) = self.widget_registry.get_mut(panel_key) {
            panel_mut.instance_states.insert(
                widget_key.to_string(),
                crate::widgets::WidgetInstanceState::Tree {
                    scroll_offset: cur_scroll,
                    selected_index: new_abs as i32,
                    expanded_keys: expanded,
                },
            );
        }
        self.rerender_widget_panel(panel_key);
        self.fire_widget_event(
            panel_key,
            widget_key.to_string(),
            "select".into(),
            serde_json::json!({ "index": new_abs as i64, "key": new_key }),
        );
    }

    /// Mouse-wheel scroll over a widget panel buffer. Finds the
    /// first `Tree`/`List` in any panel rendering into `buffer_id`
    /// and shifts its viewport by `delta` rows. Drags the selection
    /// to stay inside the new visible window so the renderer's
    /// auto-scroll doesn't snap the offset back. No focus change,
    /// no `widget_event` fires — wheel is viewport navigation, not
    /// selection.
    ///
    /// Returns `true` if any panel consumed the scroll.
    pub(super) fn handle_widget_panel_wheel(
        &mut self,
        buffer_id: crate::model::event::BufferId,
        delta: i32,
    ) -> bool {
        let panels = self.widget_registry.panels_for_buffer(buffer_id);
        let mut consumed = false;
        for panel_key in panels {
            // First chance: a focused Text widget with an open
            // completion popup absorbs the wheel — scrolling the
            // candidate list when the popup is what the user is
            // pointing at takes priority over scrolling a
            // sibling List/Tree elsewhere on the panel.
            if self.focused_text_completions_open(&panel_key) {
                self.scroll_focused_text_completions(&panel_key, delta);
                // The renderer reads `completion_scroll_offset`
                // out of the Text widget's instance state on
                // each paint, so flushing a rerender here is
                // what actually puts the new scroll on screen
                // — without this, the cached overlay rows on
                // the floating panel stay pinned to the old
                // offset until the user's next keystroke
                // happens to re-render for some other reason.
                self.rerender_widget_panel(&panel_key);
                consumed = true;
                continue;
            }
            let spec = match self.widget_registry.get(&panel_key) {
                Some(p) => p.spec.clone(),
                None => continue,
            };
            let Some(widget_key) = find_scrollable_widget_key(&spec) else {
                continue;
            };
            let widget = crate::widgets::find_widget_by_key(&spec, &widget_key);
            match widget {
                Some(fresh_core::api::WidgetSpec::Tree { .. }) => {
                    // Only claim the wheel if the widget actually scrolled.
                    // A List/Tree that declares `visible_rows >= total`
                    // (e.g. Git Log, which renders every row and relies on
                    // its scrollable region's buffer scroll instead) has
                    // nothing to scroll here; swallowing the event would
                    // leave the wheel dead. Falling through lets the
                    // underlying buffer scroll handle it.
                    consumed |= self.handle_widget_tree_wheel(&panel_key, &widget_key, delta);
                }
                Some(fresh_core::api::WidgetSpec::List { .. }) => {
                    consumed |= self.handle_widget_list_wheel(&panel_key, &widget_key, delta);
                }
                _ => {}
            }
        }
        consumed
    }

    /// Shift the focused Text widget's completion popup scroll
    /// offset by `delta` rows. The renderer reads the visible-
    /// rows cap from the Text spec; we approximate it here as
    /// "5 if zero / unset" to mirror the renderer's default —
    /// the cap matters for clamping the max scroll so the
    /// thumb doesn't drift past the end.
    fn scroll_focused_text_completions(
        &mut self,
        panel_key: &crate::widgets::PanelKey,
        delta: i32,
    ) {
        let panel = match self.widget_registry.get(panel_key) {
            Some(p) => p,
            None => return,
        };
        let focus_key = panel.focus_key.clone();
        if focus_key.is_empty() {
            return;
        }
        let spec_visible_rows = match crate::widgets::find_widget_by_key(&panel.spec, &focus_key) {
            Some(fresh_core::api::WidgetSpec::Text {
                completions_visible_rows,
                ..
            }) => *completions_visible_rows,
            _ => 0,
        };
        let visible = if spec_visible_rows == 0 {
            5u32
        } else {
            spec_visible_rows
        };
        let panel = match self.widget_registry.get_mut(panel_key) {
            Some(p) => p,
            None => return,
        };
        if let Some(crate::widgets::WidgetInstanceState::Text {
            completions,
            completion_scroll_offset,
            ..
        }) = panel.instance_states.get_mut(&focus_key)
        {
            if completions.is_empty() {
                return;
            }
            let total = completions.len() as u32;
            let max_scroll = total.saturating_sub(visible.min(total));
            let next = (*completion_scroll_offset as i32 + delta).clamp(0, max_scroll as i32);
            *completion_scroll_offset = next as u32;
        }
    }

    /// Shift a Tree's `scroll_offset` by `delta` rows. If the
    /// selection would fall outside the new viewport, drag it to
    /// the edge so the renderer's keep-selection-visible logic
    /// doesn't snap the offset back.
    fn handle_widget_tree_wheel(
        &mut self,
        panel_key: &crate::widgets::PanelKey,
        widget_key: &str,
        delta: i32,
    ) -> bool {
        let panel = match self.widget_registry.get(panel_key) {
            Some(p) => p,
            None => return false,
        };
        let widget = crate::widgets::find_widget_by_key(&panel.spec, widget_key);
        let (visible_rows, nodes, item_keys) = match widget {
            Some(fresh_core::api::WidgetSpec::Tree {
                visible_rows,
                nodes,
                item_keys,
                ..
            }) => (*visible_rows, nodes.clone(), item_keys.clone()),
            _ => return false,
        };
        if nodes.is_empty() {
            return false;
        }
        let (cur_sel, cur_scroll, expanded) = match panel.instance_states.get(widget_key) {
            Some(crate::widgets::WidgetInstanceState::Tree {
                selected_index,
                scroll_offset,
                expanded_keys,
            }) => (*selected_index, *scroll_offset, expanded_keys.clone()),
            _ => (-1, 0, std::collections::HashSet::<String>::new()),
        };
        let visible_indices = collect_visible_tree_indices(&nodes, &item_keys, &expanded);
        if visible_indices.is_empty() {
            return false;
        }
        let visible = visible_rows.max(1);
        let total_visible = visible_indices.len() as u32;
        let max_scroll = total_visible.saturating_sub(visible);
        let new_scroll = (cur_scroll as i32 + delta).clamp(0, max_scroll as i32) as u32;
        if new_scroll == cur_scroll {
            return false;
        }
        // Drag selection to stay inside the new viewport.
        let cur_pos: Option<u32> = if cur_sel >= 0 {
            visible_indices
                .iter()
                .position(|&v| v as i32 == cur_sel)
                .map(|p| p as u32)
        } else {
            None
        };
        let new_sel_abs = match cur_pos {
            Some(pos) if pos < new_scroll => visible_indices[new_scroll as usize] as i32,
            Some(pos) if pos >= new_scroll + visible => {
                visible_indices[(new_scroll + visible - 1) as usize] as i32
            }
            _ => cur_sel,
        };
        if let Some(panel_mut) = self.widget_registry.get_mut(panel_key) {
            panel_mut.instance_states.insert(
                widget_key.to_string(),
                crate::widgets::WidgetInstanceState::Tree {
                    scroll_offset: new_scroll,
                    selected_index: new_sel_abs,
                    expanded_keys: expanded,
                },
            );
        }
        self.rerender_widget_panel(panel_key);
        true
    }

    /// List counterpart of `handle_widget_tree_wheel`. Returns true if the
    /// list's scroll offset actually changed (the wheel was consumed).
    fn handle_widget_list_wheel(
        &mut self,
        panel_key: &crate::widgets::PanelKey,
        widget_key: &str,
        delta: i32,
    ) -> bool {
        let panel = match self.widget_registry.get(panel_key) {
            Some(p) => p,
            None => return false,
        };
        let widget = crate::widgets::find_widget_by_key(&panel.spec, widget_key);
        let (visible_rows, total) = match widget {
            Some(fresh_core::api::WidgetSpec::List {
                visible_rows,
                items,
                item_specs,
                ..
            }) => {
                let total = if item_specs.is_empty() {
                    items.len()
                } else {
                    item_specs.len()
                };
                (*visible_rows, total as u32)
            }
            _ => return false,
        };
        if total == 0 {
            return false;
        }
        let (cur_sel, cur_scroll, item_height) = match panel.instance_states.get(widget_key) {
            Some(crate::widgets::WidgetInstanceState::List {
                selected_index,
                scroll_offset,
                item_height,
                ..
            }) => (*selected_index, *scroll_offset, (*item_height).max(1)),
            _ => (-1, 0, 1),
        };
        // Convert the row-denominated viewport into a per-item window so
        // the bound is right for card lists (item_height > 1), and so a
        // list that already shows everything (max_scroll == 0, e.g. the
        // Git Log which sets visible_rows == commit count and scrolls via
        // its enclosing pane) reports "can't scroll" and lets the wheel
        // bubble to that pane rather than swallowing it.
        let visible_items = (visible_rows.max(1) / item_height).max(1);
        let max_scroll = total.saturating_sub(visible_items);
        let new_scroll = (cur_scroll as i64 + delta as i64).clamp(0, max_scroll as i64) as u32;
        if new_scroll == cur_scroll {
            return false;
        }
        // Wheel scrolls the *view* only — the selection stays put (and
        // may leave the visible window); `user_scrolled` tells the
        // renderer not to snap the offset back to it.
        if let Some(panel_mut) = self.widget_registry.get_mut(panel_key) {
            panel_mut.instance_states.insert(
                widget_key.to_string(),
                crate::widgets::WidgetInstanceState::List {
                    scroll_offset: new_scroll,
                    selected_index: cur_sel,
                    item_height,
                    user_scrolled: true,
                },
            );
        }
        self.rerender_widget_panel(panel_key);
        true
    }

    /// Right/Left arrow on a focused Tree.
    ///
    /// * Right: if the selected node has children and is collapsed,
    ///   expand it. Else no-op.
    /// * Left: if the selected node has children and is expanded,
    ///   collapse it. Else move selection up to the parent.
    ///
    /// Both update host instance state, re-render, and (when a
    /// change happened) fire `widget_event { event_type: "expand" }`.
    fn handle_widget_tree_lateral(&mut self, panel_key: &crate::widgets::PanelKey, is_right: bool) {
        let panel = match self.widget_registry.get(panel_key) {
            Some(p) => p,
            None => return,
        };
        let focus_key = panel.focus_key.clone();
        if focus_key.is_empty() {
            return;
        }
        let widget = crate::widgets::find_widget_by_key(&panel.spec, &focus_key);
        let (spec_sel, nodes, item_keys) = match widget {
            Some(fresh_core::api::WidgetSpec::Tree {
                selected_index,
                nodes,
                item_keys,
                ..
            }) => (*selected_index, nodes.clone(), item_keys.clone()),
            _ => return,
        };
        if nodes.is_empty() {
            return;
        }
        let (cur_sel, cur_scroll, mut expanded) = match panel.instance_states.get(&focus_key) {
            Some(crate::widgets::WidgetInstanceState::Tree {
                selected_index,
                scroll_offset,
                expanded_keys,
            }) => (*selected_index, *scroll_offset, expanded_keys.clone()),
            _ => (spec_sel, 0u32, std::collections::HashSet::<String>::new()),
        };
        if cur_sel < 0 {
            return;
        }
        let sel_idx = cur_sel as usize;
        let node = match nodes.get(sel_idx) {
            Some(n) => n,
            None => return,
        };
        let key = item_keys.get(sel_idx).cloned().unwrap_or_default();
        let was_expanded = !key.is_empty() && expanded.contains(&key);

        let mut new_sel = cur_sel;
        let mut expansion_changed: Option<bool> = None; // Some(new_state)
        if is_right {
            if node.has_children && !was_expanded && !key.is_empty() {
                expanded.insert(key.clone());
                expansion_changed = Some(true);
            }
        } else if node.has_children && was_expanded && !key.is_empty() {
            expanded.remove(&key);
            expansion_changed = Some(false);
        } else if let Some(parent_idx) = crate::widgets::tree_parent_index(&nodes, sel_idx) {
            new_sel = parent_idx as i32;
        }
        // No change → bail (don't fire spurious select/expand).
        if expansion_changed.is_none() && new_sel == cur_sel {
            return;
        }
        let final_key = item_keys.get(new_sel as usize).cloned().unwrap_or_default();
        if let Some(panel_mut) = self.widget_registry.get_mut(panel_key) {
            panel_mut.instance_states.insert(
                focus_key.clone(),
                crate::widgets::WidgetInstanceState::Tree {
                    scroll_offset: cur_scroll,
                    selected_index: new_sel,
                    expanded_keys: expanded,
                },
            );
        }
        self.rerender_widget_panel(panel_key);
        if let Some(now_expanded) = expansion_changed {
            self.fire_widget_event(
                panel_key,
                focus_key.clone(),
                "expand".into(),
                serde_json::json!({
                    "index": cur_sel as i64,
                    "key": key,
                    "expanded": now_expanded,
                }),
            );
        } else if new_sel != cur_sel {
            self.fire_widget_event(
                panel_key,
                focus_key,
                "select".into(),
                serde_json::json!({
                    "index": new_sel as i64,
                    "key": final_key,
                }),
            );
        }
    }

    /// Toggle a Tree node's expansion state, re-render, and fire
    /// `widget_event { event_type: "expand" }`. Used by the click
    /// handler when the user clicks the disclosure column.
    pub(crate) fn handle_widget_tree_expand_toggle(
        &mut self,
        panel_key: &crate::widgets::PanelKey,
        widget_key: &str,
        item_key: &str,
    ) {
        if widget_key.is_empty() || item_key.is_empty() {
            return;
        }
        let now_expanded = {
            let panel = match self.widget_registry.get_mut(panel_key) {
                Some(p) => p,
                None => return,
            };
            let (cur_scroll, cur_sel, mut expanded) = match panel.instance_states.get(widget_key) {
                Some(crate::widgets::WidgetInstanceState::Tree {
                    scroll_offset,
                    selected_index,
                    expanded_keys,
                }) => (*scroll_offset, *selected_index, expanded_keys.clone()),
                _ => (0u32, -1i32, std::collections::HashSet::<String>::new()),
            };
            let next = if expanded.contains(item_key) {
                expanded.remove(item_key);
                false
            } else {
                expanded.insert(item_key.to_string());
                true
            };
            panel.instance_states.insert(
                widget_key.to_string(),
                crate::widgets::WidgetInstanceState::Tree {
                    scroll_offset: cur_scroll,
                    selected_index: cur_sel,
                    expanded_keys: expanded,
                },
            );
            next
        };
        self.rerender_widget_panel(panel_key);
        self.fire_widget_event(
            panel_key,
            widget_key.to_string(),
            "expand".into(),
            serde_json::json!({ "key": item_key, "expanded": now_expanded, }),
        );
    }

    /// Fire `widget_event { event_type: "activate" }` for the focused
    /// Tree's currently-selected node. Mirrors `fire_list_activate`
    /// — the plugin's handler decides what "activate" means
    /// (open the file, run an action, etc.).
    /// If the focused Tree row is checkable (parent tree has
    /// `checkable: true` *and* the row's `checked` is `Some(_)`),
    /// fire `widget_event { event_type: "toggle" }` with the
    /// inverted value and return `true`. Otherwise return `false`
    /// so the caller falls back to `activate`.
    ///
    /// Mirrors what a click on the row's `[v]`/`[ ]` glyph would
    /// do — Space is the conventional checkbox key, so on a
    /// checkable tree Space toggles instead of activating.
    fn fire_tree_toggle_if_checkable(
        &mut self,
        panel_key: &crate::widgets::PanelKey,
        focus_key: &str,
    ) -> bool {
        let panel = match self.widget_registry.get(panel_key) {
            Some(p) => p,
            None => return false,
        };
        let widget = crate::widgets::find_widget_by_key(&panel.spec, focus_key);
        let (spec_sel, nodes, item_keys, checkable) = match widget {
            Some(fresh_core::api::WidgetSpec::Tree {
                selected_index,
                nodes,
                item_keys,
                checkable,
                ..
            }) => (*selected_index, nodes, item_keys.clone(), *checkable),
            _ => return false,
        };
        if !checkable {
            return false;
        }
        let sel = match panel.instance_states.get(focus_key) {
            Some(crate::widgets::WidgetInstanceState::Tree { selected_index, .. }) => {
                *selected_index
            }
            _ => spec_sel,
        };
        if sel < 0 {
            return false;
        }
        let cur_checked = match nodes.get(sel as usize).and_then(|n| n.checked) {
            Some(b) => b,
            None => return false, // No checkbox glyph on this row — let activate fire.
        };
        let new_checked = !cur_checked;
        let item_key = item_keys.get(sel as usize).cloned().unwrap_or_default();
        self.fire_widget_event(
            panel_key,
            focus_key.to_string(),
            "toggle".into(),
            serde_json::json!({ "index": sel, "key": item_key, "checked": new_checked, }),
        );
        true
    }

    fn fire_tree_activate(&mut self, panel_key: &crate::widgets::PanelKey, focus_key: &str) {
        let panel = match self.widget_registry.get(panel_key) {
            Some(p) => p,
            None => return,
        };
        let widget = crate::widgets::find_widget_by_key(&panel.spec, focus_key);
        let (spec_sel, item_keys) = match widget {
            Some(fresh_core::api::WidgetSpec::Tree {
                selected_index,
                item_keys,
                ..
            }) => (*selected_index, item_keys.clone()),
            _ => return,
        };
        let sel = match panel.instance_states.get(focus_key) {
            Some(crate::widgets::WidgetInstanceState::Tree { selected_index, .. }) => {
                *selected_index
            }
            _ => spec_sel,
        };
        if sel < 0 {
            return;
        }
        let item_key = item_keys.get(sel as usize).cloned().unwrap_or_default();
        self.fire_widget_event(
            panel_key,
            focus_key.to_string(),
            "activate".into(),
            serde_json::json!({ "index": sel, "key": item_key, }),
        );
    }

    /// Walk every panel rendering into `buffer_id` and return the
    /// first one whose currently-focused widget is a `Text`.
    /// Returns `None` when no such panel exists (e.g. when the
    /// buffer is a regular text buffer, or the panel has focus on
    /// a `Button` / `List` / etc.).
    ///
    /// This is the universal hook the clipboard ops use to route
    /// Paste / Copy / Cut / Select-All to a focused widget text
    /// field instead of the underlying buffer. Same idea as the
    /// existing Prompt and FileExplorer branches in the clipboard
    /// path, generalised: any plugin-mounted Text widget that has
    /// focus wins over the underlying buffer.
    pub(super) fn focused_text_widget_panel_for_buffer(
        &self,
        buffer_id: crate::model::event::BufferId,
    ) -> Option<crate::widgets::PanelKey> {
        for panel_key in self.widget_registry.panels_for_buffer(buffer_id) {
            if self.panel_focused_widget_is_text(&panel_key) {
                return Some(panel_key);
            }
        }
        None
    }

    /// True when `panel_key`'s currently-focused widget is a `Text`
    /// field (so it can accept clipboard insertion). `false` when the
    /// panel is gone, has no focus, or focus rests on a non-text
    /// widget (`Button` / `List` / `Toggle` / …). This is the shared
    /// predicate behind both the buffer-mounted paste routing
    /// (`focused_text_widget_panel_for_buffer`) and the floating-panel
    /// bracketed-paste routing (`paste_bracketed_into_focused_panel`).
    pub(super) fn panel_focused_widget_is_text(
        &self,
        panel_key: &crate::widgets::PanelKey,
    ) -> bool {
        let Some(panel) = self.widget_registry.get(panel_key) else {
            return false;
        };
        if panel.focus_key.is_empty() {
            return false;
        }
        matches!(
            crate::widgets::find_widget_by_key(&panel.spec, &panel.focus_key),
            Some(fresh_core::api::WidgetSpec::Text { .. })
        )
    }

    /// Read the currently-selected text from the focused `Text`
    /// widget on the given panel, or `None` when nothing is
    /// selected (no anchor, or anchor == cursor). Used by the
    /// host-side Copy / Cut routing path.
    pub(super) fn focused_widget_selected_text(
        &self,
        panel_key: &crate::widgets::PanelKey,
    ) -> Option<String> {
        let panel = self.widget_registry.get(panel_key)?;
        if panel.focus_key.is_empty() {
            return None;
        }
        match panel.instance_states.get(&panel.focus_key) {
            Some(crate::widgets::WidgetInstanceState::Text { editor, .. }) => {
                editor.selected_text()
            }
            _ => None,
        }
    }

    /// Select-all in the focused widget Text. Returns true when
    /// applied (focus was a Text widget). The op fires a `change`
    /// event only if the selection range actually changed; an
    /// already-fully-selected widget is a no-op.
    pub(super) fn handle_widget_select_all(
        &mut self,
        panel_key: &crate::widgets::PanelKey,
    ) -> bool {
        // SelectAll moves the cursor to end-of-value and sets anchor
        // at start — `with_focused_text_editor` will skip re-render
        // when nothing changed, which is fine.
        self.with_focused_text_editor(panel_key, |editor| editor.select_all())
    }

    /// Copy the focused widget Text's current selection to the
    /// internal clipboard. Returns true when copy ran (even when
    /// the selection was empty — the action is consumed either way
    /// so it doesn't fall through to the buffer's copy path).
    pub(super) fn handle_widget_copy(&mut self, panel_key: &crate::widgets::PanelKey) -> bool {
        if self.widget_registry.get(panel_key).is_none() {
            return false;
        }
        if let Some(text) = self.focused_widget_selected_text(panel_key) {
            self.clipboard.copy(text);
        }
        true
    }

    /// Cut the focused widget Text's current selection — copy then
    /// delete. With no selection, this is a no-op consume.
    pub(super) fn handle_widget_cut(&mut self, panel_key: &crate::widgets::PanelKey) -> bool {
        if self.widget_registry.get(panel_key).is_none() {
            return false;
        }
        if let Some(text) = self.focused_widget_selected_text(panel_key) {
            self.clipboard.copy(text);
            self.with_focused_text_editor(panel_key, |editor| {
                editor.delete_selection();
            });
        }
        true
    }

    /// Insert `text` at the focused widget Text's cursor (replacing
    /// any active selection). Used by the host-side Paste routing
    /// path; `text` is already line-ending-normalised by the
    /// caller (CRLF / CR → LF). `TextEdit::insert_str` strips
    /// embedded newlines when the editor is single-line.
    pub(super) fn handle_widget_insert_str(
        &mut self,
        panel_key: &crate::widgets::PanelKey,
        text: &str,
    ) -> bool {
        if self.widget_registry.get(panel_key).is_none() {
            return false;
        }
        let owned = text.to_string();
        self.with_focused_text_editor(panel_key, move |editor| {
            editor.insert_str(&owned);
        });
        true
    }

    /// Ensure `panel.instance_states[focus_key]` is a seeded
    /// `Text { editor, .. }` for the focused widget. If instance
    /// state already has the entry, no-op. If not, seeds from the
    /// spec's `value` / `cursor_byte` / `rows`. Returns true on
    /// success (focus is a Text widget that's now in instance state),
    /// false otherwise.
    fn ensure_focused_text_seeded(
        &mut self,
        panel_key: &crate::widgets::PanelKey,
        focus_key: &str,
    ) -> bool {
        let panel = match self.widget_registry.get_mut(panel_key) {
            Some(p) => p,
            None => return false,
        };
        if matches!(
            panel.instance_states.get(focus_key),
            Some(crate::widgets::WidgetInstanceState::Text { .. })
        ) {
            return true;
        }
        let widget = crate::widgets::find_widget_by_key(&panel.spec, focus_key);
        let (value, cursor_byte, multiline) = match widget {
            Some(fresh_core::api::WidgetSpec::Text {
                value,
                cursor_byte,
                rows,
                ..
            }) => (value.clone(), *cursor_byte, *rows > 1),
            _ => return false,
        };
        let mut editor = if multiline {
            crate::primitives::text_edit::TextEdit::with_text(&value)
        } else {
            crate::primitives::text_edit::TextEdit::single_line_with_text(&value)
        };
        let seed = if cursor_byte < 0 {
            value.len()
        } else {
            (cursor_byte as usize).min(value.len())
        };
        editor.set_cursor_from_flat(seed);
        panel.instance_states.insert(
            focus_key.to_string(),
            crate::widgets::WidgetInstanceState::Text {
                editor,
                scroll: 0,
                completions: Vec::new(),
                completion_selected_index: 0,
                completion_scroll_offset: 0,
            },
        );
        true
    }

    /// Apply a mutating operation to the focused `Text` widget's
    /// `TextEdit`. Handles seeding the editor from the spec on first
    /// touch, no-op detection (skips rerender + change event), and
    /// firing the `widget_event` "change" hook with the post-state.
    ///
    /// Returns true when the op ran *and* produced a visible change.
    pub(super) fn with_focused_text_editor<F>(
        &mut self,
        panel_key: &crate::widgets::PanelKey,
        op: F,
    ) -> bool
    where
        F: FnOnce(&mut crate::primitives::text_edit::TextEdit),
    {
        let focus_key = match self.widget_registry.get(panel_key) {
            Some(p) if !p.focus_key.is_empty() => p.focus_key.clone(),
            _ => return false,
        };
        if !self.ensure_focused_text_seeded(panel_key, &focus_key) {
            return false;
        }
        let (before_value, before_cursor) = {
            let panel = self.widget_registry.get(panel_key).unwrap();
            match panel.instance_states.get(&focus_key) {
                Some(crate::widgets::WidgetInstanceState::Text { editor, .. }) => {
                    (editor.value(), editor.flat_cursor_byte())
                }
                _ => return false,
            }
        };
        {
            let panel = self.widget_registry.get_mut(panel_key).unwrap();
            match panel.instance_states.get_mut(&focus_key) {
                Some(crate::widgets::WidgetInstanceState::Text { editor, .. }) => op(editor),
                _ => return false,
            }
        }
        let (after_value, after_cursor) = {
            let panel = self.widget_registry.get(panel_key).unwrap();
            match panel.instance_states.get(&focus_key) {
                Some(crate::widgets::WidgetInstanceState::Text { editor, .. }) => {
                    (editor.value(), editor.flat_cursor_byte())
                }
                _ => return false,
            }
        };
        if after_value == before_value && after_cursor == before_cursor {
            return false;
        }
        self.rerender_widget_panel(panel_key);
        self.fire_widget_event(
            panel_key,
            focus_key.clone(),
            "change".into(),
            serde_json::json!({ "value": after_value, "cursorByte": after_cursor as i64, }),
        );
        true
    }

    /// Apply a non-printable editing key to the focused text widget
    /// by dispatching to the corresponding `TextEdit` method. The
    /// single/multi-line discriminator is carried by `TextEdit`'s
    /// `multiline` field, so the same set of methods serves both
    /// kinds — single-line just no-ops on Up/Down/Enter.
    fn handle_widget_text_key(&mut self, panel_key: &crate::widgets::PanelKey, key: &str) {
        self.with_focused_text_editor(panel_key, |editor| match key {
            "Backspace" => editor.backspace(),
            "Delete" => editor.delete(),
            "Left" => editor.move_left(),
            "Right" => editor.move_right(),
            "Up" => editor.move_up(),
            "Down" => editor.move_down(),
            "Home" => editor.move_home(),
            "End" => editor.move_end(),
            "Enter" => editor.insert_char('\n'),
            _ => { /* unknown key — no-op */ }
        });
    }

    /// Insert printable / IME-committed text at the focused text
    /// widget's cursor. Same path for single-line and multi-line —
    /// `TextEdit::insert_str` strips `\n` automatically when the
    /// editor was constructed single-line. `text` may be a single
    /// codepoint, a grapheme cluster, or a multi-codepoint IME
    /// commit; `insert_str` handles each identically.
    fn handle_widget_text_char(&mut self, panel_key: &crate::widgets::PanelKey, text: &str) {
        if text.is_empty() {
            return;
        }
        let text = text.to_string();
        self.with_focused_text_editor(panel_key, move |editor| {
            editor.insert_str(&text);
        });
    }

    /// Inner-rect column budget for a floating panel render — the
    /// terminal width × `width_pct`, minus 2 cols for the frame
    /// border. Mirrors the `widget_panel_width` reservation; never
    /// goes below 10 cols so flex spacers don't collapse to zero on
    /// narrow terminals.
    pub(super) fn floating_panel_inner_width(&self, slot: super::PanelSlot) -> u32 {
        // A left-dock panel wraps its content to the dock's fixed
        // column width rather than a percentage of the terminal.
        if let Some(super::PanelPlacement::LeftDock { width_cols }) =
            self.panel(slot).map(|f| f.placement)
        {
            return (width_cols as u32).saturating_sub(2).max(10);
        }
        let term_w = self.terminal_width.max(1) as u32;
        let pct = self
            .panel(slot)
            .map(|f| f.width_pct.clamp(1, 100) as u32)
            .unwrap_or(80);
        let w = (term_w * pct) / 100;
        w.saturating_sub(2).max(10)
    }

    /// Restore keyboard focus to a (docked) floating panel that was
    /// previously blurred — typically a mouse click landing back inside
    /// the dock's column after the user dived into the editor. Sets
    /// the panel's `focused` flag and fires a `focus` widget_event so
    /// the owning plugin can update any mirror of the focused state
    /// (the orchestrator's `dockBlurred`, for instance). Symmetric
    /// with [`Editor::blur_floating_panel`], which has always fired
    /// `blur` on the inverse transition.
    ///
    /// Unlike [`Editor::set_panel_focus_and_notify`] this fires the
    /// `focus` event even when the *inner* focus_key hasn't changed —
    /// the dive only flipped overall focus, not the active widget, so
    /// the inner key is identical on re-focus and the "key-changed"
    /// short-circuit would silently drop the event. That short-circuit
    /// was the original bug: the host updated `dock.focused` but the
    /// plugin's mirror stayed stale, and the dock's debounced
    /// dock-switch then aborted at its `dockBlurred` guard.
    pub(super) fn refocus_floating_panel(&mut self, slot: super::PanelSlot) {
        let Some(panel_key) = self.panel(slot).map(|f| f.panel_key.clone()) else {
            return;
        };
        if let Some(f) = self.panel_mut(slot) {
            f.focused = true;
        }
        let widget_key = self
            .widget_registry
            .get(&panel_key)
            .map(|p| p.focus_key.clone())
            .unwrap_or_default();
        tracing::debug!(
            target: "fresh::dock",
            panel = %panel_key,
            ?slot,
            widget_key = %widget_key,
            "refocus_floating_panel: firing unconditional `focus` widget_event"
        );
        self.fire_widget_event(
            &panel_key,
            widget_key,
            "focus".to_string(),
            serde_json::json!({ "previous": "(re-focus)" }),
        );
    }

    /// Return keyboard focus to the editor while leaving a (docked)
    /// floating panel visible. Clears the panel's `focused` flag and
    /// fires a `blur` widget_event so the owning plugin can react
    /// (e.g. drop its editor mode). No-op when no panel is mounted.
    /// Shared by the Esc handler, the editor-click handler, and the
    /// `FloatingPanelControl{op:"blur"}` command.
    pub(super) fn blur_floating_panel(&mut self, slot: super::PanelSlot) {
        let Some(panel_key) = self.panel(slot).map(|f| f.panel_key.clone()) else {
            return;
        };
        if let Some(f) = self.panel_mut(slot) {
            f.focused = false;
        }
        tracing::debug!(
            target: "fresh::dock",
            panel = %panel_key,
            ?slot,
            "blur_floating_panel: firing `blur` widget_event"
        );
        let widget_key = self
            .widget_registry
            .get(&panel_key)
            .map(|p| p.focus_key.clone())
            .unwrap_or_default();
        self.fire_widget_event(
            &panel_key,
            widget_key,
            "blur".to_string(),
            serde_json::json!({}),
        );
    }

    /// Handle CloseSplit command
    pub(super) fn handle_close_split(&mut self, split_id: SplitId) {
        // Plugin sends arbitrary SplitId — convert to LeafId at the boundary
        let leaf_id = LeafId(split_id);
        match self
            .windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_manager_mut())
            .expect("active window must have a populated split layout")
            .close_split(leaf_id)
        {
            Ok(()) => {
                // Clean up the view state for the closed split
                self.windows
                    .get_mut(&self.active_window)
                    .and_then(|w| w.split_view_states_mut())
                    .expect("active window must have a populated split layout")
                    .remove(&leaf_id);
                tracing::info!("Closed split {:?}", split_id);
            }
            Err(e) => {
                tracing::warn!("Failed to close split {:?}: {}", split_id, e);
            }
        }
    }

    /// Handle RefreshLines command
    pub(super) fn handle_refresh_lines(&mut self, buffer_id: BufferId) {
        // Clear seen_byte_ranges for this buffer so all visible lines will be re-processed
        // on the next render. This is useful when a plugin is enabled and needs to
        // process lines that were already marked as seen.
        self.active_window_mut().seen_byte_ranges.remove(&buffer_id);
        // Request a render so the lines_changed hook fires
        #[cfg(feature = "plugins")]
        {
            self.plugin_render_requested = true;
        }
    }

    /// Flush pending grammars: spawn a background rebuild if any ReloadGrammars
    /// commands were received during this command batch.
    ///
    /// Called after processing all plugin commands in a batch, so that multiple
    /// RegisterGrammar+ReloadGrammars pairs result in only one rebuild.
    /// The rebuild happens on a background thread; when complete, a
    /// `GrammarRegistryBuilt` message swaps in the new registry.
    ///
    /// On the first call, this triggers the deferred full grammar build
    /// (user grammars + language packs + any plugin grammars accumulated so far).
    pub(super) fn flush_pending_grammars(&mut self) {
        // On the first call, start the deferred full grammar build.
        // This includes any plugin grammars that were registered during init,
        // so we get everything in a single builder.build() pass.
        if self.needs_full_grammar_build {
            self.needs_full_grammar_build = false;
            self.grammar_reload_pending = false;

            // Drain all pending grammars to include in the initial build
            let additional: Vec<_> = self
                .pending_grammars
                .drain(..)
                .map(|g| crate::primitives::grammar::GrammarSpec {
                    language: g.language.clone(),
                    path: std::path::PathBuf::from(g.grammar_path),
                    extensions: g.extensions.clone(),
                })
                .collect();

            // Update config.languages with the extensions so detect_language() works
            for crate::primitives::grammar::GrammarSpec {
                language,
                extensions,
                ..
            } in &additional
            {
                let lang_config = self
                    .config_mut()
                    .languages
                    .entry(language.clone())
                    .or_default();
                for ext in extensions {
                    if !lang_config.extensions.contains(ext) {
                        lang_config.extensions.push(ext.clone());
                    }
                }
            }

            let callback_ids: Vec<_> = self.pending_grammar_callbacks.drain(..).collect();
            self.start_background_grammar_build(additional, callback_ids);
            return;
        }

        if !self.grammar_reload_pending {
            return;
        }
        self.grammar_reload_pending = false;

        // If a background build is already in progress, it will call
        // flush_pending_grammars() again when it completes — so just
        // re-arm the flag and return.
        if self.grammar_build_in_progress {
            self.grammar_reload_pending = true;
            tracing::debug!("Grammar build in progress, deferring flush");
            return;
        }

        use std::path::PathBuf;

        if self.pending_grammars.is_empty() {
            tracing::debug!("Grammar reload requested but no pending grammars");
            return;
        }

        // Deduplicate: skip grammars whose extensions are all already mapped
        // in the current registry (meaning the grammar was already loaded by
        // for_editor or a previous build).
        let pending_before = self.pending_grammars.len();
        self.pending_grammars.retain(|g| {
            // Check if ALL extensions for this grammar are already mapped
            let all_mapped = !g.extensions.is_empty()
                && g.extensions
                    .iter()
                    .all(|ext| self.grammar_registry.find_by_extension(ext).is_some());
            if all_mapped {
                tracing::debug!(
                    "Skipping already-loaded grammar '{}' (extensions {:?} already mapped)",
                    g.language,
                    g.extensions
                );
                false
            } else {
                true
            }
        });
        if pending_before != self.pending_grammars.len() {
            tracing::info!(
                "Deduplicated pending grammars: {} -> {}",
                pending_before,
                self.pending_grammars.len()
            );
        }

        if self.pending_grammars.is_empty() {
            tracing::info!(
                "All pending grammars already loaded, resolving callbacks without rebuild"
            );
            // Resolve callbacks immediately — no rebuild needed
            #[cfg(feature = "plugins")]
            for cb_id in self.pending_grammar_callbacks.drain(..) {
                self.plugin_manager
                    .read()
                    .unwrap()
                    .resolve_callback(cb_id, "null".to_string());
            }
            #[cfg(not(feature = "plugins"))]
            self.pending_grammar_callbacks.clear();
            return;
        }

        tracing::info!(
            "Flushing {} pending grammars via background rebuild",
            self.pending_grammars.len()
        );

        // Collect pending grammars
        let additional: Vec<crate::primitives::grammar::GrammarSpec> = self
            .pending_grammars
            .drain(..)
            .map(|g| crate::primitives::grammar::GrammarSpec {
                language: g.language.clone(),
                path: PathBuf::from(g.grammar_path),
                extensions: g.extensions.clone(),
            })
            .collect();

        // Update config.languages with the extensions so detect_language() works
        for crate::primitives::grammar::GrammarSpec {
            language,
            extensions,
            ..
        } in &additional
        {
            let lang_config = self
                .config_mut()
                .languages
                .entry(language.clone())
                .or_default();
            for ext in extensions {
                if !lang_config.extensions.contains(ext) {
                    lang_config.extensions.push(ext.clone());
                }
            }
        }

        // Collect pending callback IDs to resolve when build completes
        let callback_ids: Vec<_> = self.pending_grammar_callbacks.drain(..).collect();

        // Spawn background rebuild
        let base_registry = std::sync::Arc::clone(&self.grammar_registry);
        if let Some(bridge) = &self.async_bridge {
            let sender = bridge.sender();
            self.grammar_build_in_progress = true;
            std::thread::Builder::new()
                .name("grammar-rebuild".to_string())
                .spawn(move || {
                    use crate::primitives::grammar::GrammarRegistry;
                    match GrammarRegistry::with_additional_grammars(&base_registry, &additional) {
                        Some(new_registry) => {
                            // Ok to ignore: receiver may be gone if app is shutting down.
                            drop(sender.send(
                                crate::services::async_bridge::AsyncMessage::GrammarRegistryBuilt {
                                    registry: std::sync::Arc::new(new_registry),
                                    callback_ids,
                                },
                            ));
                        }
                        None => {
                            tracing::error!("Failed to rebuild grammar registry in background");
                            // Still send the message so callbacks get resolved (even on failure)
                            drop(sender.send(
                                crate::services::async_bridge::AsyncMessage::GrammarRegistryBuilt {
                                    registry: base_registry,
                                    callback_ids,
                                },
                            ));
                        }
                    }
                })
                .ok();
        }
    }

    // ==================== Project Grep ====================

    /// Retry deferred virtual-buffer animations now that split_areas has
    /// been recomputed. Called from render() after layout but before
    /// animations.apply_all so the first frame of the effect lands in
    /// the same render pass.
    pub(crate) fn drain_pending_vb_animations(&mut self) {
        if self.pending_vb_animations.is_empty() {
            return;
        }
        let pending = std::mem::take(&mut self.pending_vb_animations);
        for (id, buffer_id, kind) in pending {
            match self.virtual_buffer_screen_rect(buffer_id) {
                Some(area) => {
                    let animation_kind = translate_plugin_animation_kind(kind);
                    self.active_window_mut().animations.start_with_id(
                        crate::view::animation::AnimationId::from_raw(id),
                        area,
                        animation_kind,
                    );
                }
                None => {
                    // Still not visible; keep pending for next frame.
                    self.pending_vb_animations.push((id, buffer_id, kind));
                }
            }
        }
    }

    /// Look up the on-screen Rect currently occupied by `buffer_id`, if any.
    /// Reads from the cached split layout captured in the last render pass.
    pub(crate) fn virtual_buffer_screen_rect(
        &self,
        buffer_id: BufferId,
    ) -> Option<ratatui::layout::Rect> {
        self.active_layout()
            .split_areas
            .iter()
            .find(|(_, bid, _, _, _, _)| *bid == buffer_id)
            .map(|(_, _, content_rect, _, _, _)| *content_rect)
    }
}
