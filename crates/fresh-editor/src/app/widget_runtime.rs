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

use super::chrome::in_rect;
use super::Editor;

/// Render a floating panel's spec, choosing the marker-gutter
/// renderer when the panel opted into the `▸ ` focus marker (the
/// Orchestrator New Session form) and the plain renderer otherwise.
/// Centralised so the mount / update / rerender paths can't drift on
/// which renderer a given panel uses. Lives here (not in the
/// `plugins`-gated `plugin_dispatch`) so the non-plugin rerender path
/// can call it in plugin-less builds.
///
/// `hover_key` is the widget the pointer is over (`""` for none). It's
/// host state that changes with every mouse move, so it is applied
/// around the render rather than carried in the spec; only bare icon
/// buttons read it.
impl Editor {
    /// Render a buffer-mounted panel spec with the live theme + grammars
    /// threaded in (so `markdown: true` Text widgets render through the
    /// shared markdown engine). The read guard on the theme lives only
    /// for the render call.
    pub(super) fn render_panel_spec(
        &self,
        spec: &fresh_core::api::WidgetSpec,
        prev: &std::collections::HashMap<String, crate::widgets::WidgetInstanceState>,
        prev_focus_key: &str,
        panel_width: u32,
        avail_height: Option<u32>,
    ) -> crate::widgets::RenderOutput {
        let theme_guard = self.theme.read().unwrap();
        crate::widgets::render_spec_with_options(
            spec,
            prev,
            panel_width,
            crate::widgets::RenderOptions {
                prev_focus_key,
                auto_focus_first: true,
                markdown: Some(crate::widgets::MarkdownCtx {
                    theme: &theme_guard,
                    grammars: Some(self.grammar_registry.as_ref()),
                }),
                // Auto-size budget for `visible_rows: None` lists/trees:
                // the viewport height of the split currently showing the
                // panel's buffer (None when it isn't on screen — widgets
                // then keep the legacy fallback until it is).
                avail_height,
                ..Default::default()
            },
        )
    }
}

// Every parameter here is host state the spec doesn't carry (focus, hover,
// theme, geometry); bundling them into a struct would only move the same
// list one level out.
#[allow(clippy::too_many_arguments)]
pub(super) fn render_floating_spec(
    focus_marker: bool,
    spec: &fresh_core::api::WidgetSpec,
    prev: &std::collections::HashMap<String, crate::widgets::WidgetInstanceState>,
    prev_focus_key: &str,
    panel_width: u32,
    avail_height: Option<u32>,
    hover_key: &str,
    hover_item_key: &str,
    markdown: Option<crate::widgets::MarkdownCtx<'_>>,
) -> crate::widgets::RenderOutput {
    crate::widgets::render_spec_with_options(
        spec,
        prev,
        panel_width,
        crate::widgets::RenderOptions {
            prev_focus_key,
            hover_key,
            hover_item_key,
            marker_gutter: focus_marker,
            auto_focus_first: true,
            markdown,
            avail_height,
        },
    )
}

/// Walk a `Tree`'s flat `nodes` and return the absolute indices of
/// nodes that are currently visible — i.e. every ancestor is in
/// `expanded`. Mirrors the renderer's filter so dispatcher and
/// renderer agree on what's selectable.
/// First widget in `spec` (declaration order) whose KIND declares the
/// `picker_scroll_target` capability (`BoxMeta`) — List, Tree, and
/// markdown document views. Used by picker forwarding and the
/// positionless wheel to pick which widget inside a panel absorbs
/// the scroll. No kind matching here: the capability is the kind's
/// declaration.
/// Whether `spec` contains a `List`/`Tree` that omitted `visible_rows` —
/// the widgets whose row window is the host's to size, and so the only
/// ones a change of panel height can leave laid out wrongly.
fn spec_has_auto_sized_list(spec: &fresh_core::api::WidgetSpec) -> bool {
    use fresh_core::api::WidgetSpec;
    if matches!(
        spec,
        WidgetSpec::List {
            visible_rows: None,
            ..
        } | WidgetSpec::Tree {
            visible_rows: None,
            ..
        }
    ) {
        return true;
    }
    spec.children().any(spec_has_auto_sized_list)
}

fn find_scrollable_widget_key(spec: &fresh_core::api::WidgetSpec) -> Option<String> {
    let meta = crate::widgets::kinds::behavior(spec).box_meta(spec);
    if meta.picker_scroll_target {
        if let Some(k) = meta.key {
            return Some(k);
        }
    }
    spec.children().find_map(find_scrollable_widget_key)
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
    /// Process a resolved widget hit (from a TUI cell click, a floating
    /// panel click, or a native-frontend click): move focus to the hit's
    /// OWNING widget, run the owner kind's own pointer handler
    /// ([`crate::widgets::kinds::WidgetImpl::on_pointer`] — tree
    /// expansion, list/tree selection, dropdown open/commit, dual-list
    /// cursors all live with their kinds), apply the effects it
    /// requested, and — unless the kind consumed the hit — fire the
    /// recorded event tagged `via: "click"`. This is the single dispatch
    /// path shared by every frontend, so a click delivers identical
    /// behaviour in all of them. No per-kind decision happens here.
    pub(crate) fn deliver_widget_hit(
        &mut self,
        panel_key: &crate::widgets::PanelKey,
        hit: &crate::widgets::HitArea,
        clicked_byte: Option<usize>,
    ) {
        use crate::widgets::kinds::{PointerDisposition, PointerFx};
        let owner = hit.owner().to_string();
        // Click-to-focus: if the owning widget has a stable, tabbable
        // key, move focus there before anything else so the next render
        // reflects it. (A List row's owner is the List itself — a row
        // click focuses the list, and arrows right after it keep moving
        // the list's selection.)
        if !owner.is_empty() {
            let is_tabbable = self
                .widget_registry
                .get(panel_key)
                .map(|p| p.tabbable.iter().any(|k| k == &owner))
                .unwrap_or(false);
            if is_tabbable {
                self.set_panel_focus_and_notify(panel_key, owner.clone());
            }
            self.rerender_widget_panel(panel_key);
        }
        // The owner kind's own pointer handling, through the single
        // kind dispatch. A hit whose owner isn't resolvable in the
        // spec (keyless widget, stale hit) has no kind behaviour — the
        // recorded event fires as-is below.
        let widget = self
            .widget_registry
            .get(panel_key)
            .and_then(|p| crate::widgets::find_widget_by_key(&p.spec, &owner))
            .cloned();
        let mut fx = PointerFx::default();
        let mut disposition = PointerDisposition::Default;
        if let Some(spec) = &widget {
            if let Some(panel) = self.widget_registry.get_mut(panel_key) {
                disposition = crate::widgets::kinds::behavior(spec).on_pointer(
                    spec,
                    &owner,
                    panel,
                    hit.event_type,
                    &hit.payload,
                    &mut fx,
                );
            }
        }
        // Caret placement the kind requested (#2573): mapping the
        // clicked byte to a caret position is host knowledge — a
        // markdown document row places within the rendered line (and
        // arms drag-to-select); a plain field maps through the value
        // window. Only click paths that know the byte can honour it
        // (native by-index delivery passes `None`).
        if fx.place_caret {
            if let Some(byte) = clicked_byte {
                if let Some(line) = hit.payload.get("mdLine").and_then(|v| v.as_u64()) {
                    self.position_markdown_text_cursor_from_click(
                        panel_key,
                        &hit.widget_key,
                        line as usize,
                        byte.saturating_sub(hit.byte_start),
                    );
                } else {
                    self.reposition_widget_text_cursor_from_click(
                        panel_key,
                        &hit.widget_key,
                        byte,
                        hit.byte_start,
                        &hit.payload,
                    );
                }
            }
        }
        // Apply the handler's effects — the same interpretation the key
        // path's shell gives a `KeyFx`: host actions, repaint, deferred
        // events against the owner, then any focus advance.
        let key_fx = fx.key;
        if key_fx.flash_scrollbar {
            self.flash_dock_scrollbar(panel_key);
        }
        if let Some(text) = key_fx.clipboard_copy {
            self.clipboard.copy(text);
        }
        self.rerender_widget_panel(panel_key);
        for (event_type, payload) in key_fx.events {
            self.fire_widget_event(panel_key, owner.clone(), event_type, payload);
        }
        if let Some(delta) = key_fx.focus_advance {
            self.handle_widget_focus_advance(panel_key, delta);
        }
        if disposition == PointerDisposition::Default {
            // Tag the event as mouse-originated so a plugin can tell a
            // click apart from a keyboard move that emits the same
            // event/payload (arrows fire `select` without this marker).
            // e.g. Search & Replace opens a result on click but not on
            // arrow-move.
            let mut payload = hit.payload.clone();
            if let Some(obj) = payload.as_object_mut() {
                obj.insert("via".to_string(), serde_json::json!("click"));
            }
            self.fire_widget_event(panel_key, owner, hit.event_type.to_string(), payload);
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
            // Native frontends deliver by index, without a per-cell click
            // column, so there's no click-to-position payload to honour here.
            self.deliver_widget_hit(&panel_key, &hit, None);
        }
    }

    /// Native-frontend entry point, robust against hit-list drift: resolve
    /// the clicked hit by IDENTITY — `widget_key` + `event_type`, preferring
    /// exact `payload` equality — instead of by raw index. A raw index goes
    /// stale the moment the plugin re-renders between the pushed frame and
    /// the click; identity survives reordering by construction. `hit_index`
    /// (the index the frontend rendered from) stays as the last-resort
    /// tiebreaker for hits that carry no key.
    ///
    /// The recorded hit list is additionally a projection of the TUI's
    /// *visible* rows — `collect_list` windows its hits to the cell
    /// viewport's `[scroll, scroll+visible)` — while a native frontend
    /// renders the whole list in its own scroll container. A click on a row
    /// outside that window therefore matches NO recorded hit; for
    /// `list`/`select` clicks the hit is then synthesized from the panel's
    /// own spec (`synthesize_list_hit` rebuilds exactly the `HitArea` the
    /// renderer would have emitted for that row — the item key comes from
    /// the spec, never from frontend-supplied strings alone).
    pub fn deliver_widget_hit_semantic(
        &mut self,
        plugin: &str,
        panel_id: u64,
        widget_key: &str,
        event_type: &str,
        payload: &serde_json::Value,
        hit_index: Option<usize>,
    ) {
        let panel_key = crate::widgets::PanelKey::new(plugin, panel_id);
        let hit = {
            let Some(panel) = self.widget_registry.get(&panel_key) else {
                return;
            };
            let identity = |strict: bool| {
                panel
                    .hits
                    .iter()
                    .find(|h| {
                        h.event_type == event_type
                            && h.widget_key == widget_key
                            && (!strict || h.payload == *payload)
                            // Never loose-match across rows: tree hits all
                            // share the tree's spec key, so when both payloads
                            // carry an `index` they must agree — otherwise a
                            // click on an off-window row would deliver some
                            // other row's recorded hit.
                            && (strict
                                || match (h.payload.get("index"), payload.get("index")) {
                                    (Some(a), Some(b)) => a == b,
                                    _ => true,
                                })
                    })
                    .cloned()
            };
            // Exact match first; then key+event only (payload drift, e.g. a
            // toggle whose `checked` flipped — the CURRENT hit is the right
            // one to deliver, exactly as a TUI click would use it). The
            // loose tier requires a non-empty key so keyless hits can't
            // cross-match. Then the raw-index tiebreaker, then the
            // off-window list synthesis.
            identity(true)
                .or_else(|| {
                    if widget_key.is_empty() {
                        None
                    } else {
                        identity(false)
                    }
                })
                .or_else(|| hit_index.and_then(|i| panel.hits.get(i).cloned()))
                .or_else(|| Self::synthesize_list_hit(panel, event_type, payload))
                .or_else(|| Self::synthesize_tree_hit(panel, widget_key, event_type, payload))
                .or_else(|| Self::synthesize_control_hit(panel, widget_key, event_type, payload))
        };
        if let Some(hit) = hit {
            self.deliver_widget_hit(&panel_key, &hit, None);
        }
    }

    /// Copy the right-click screen cell (`col`/`row`) from a frontend
    /// `context` payload into the synthesized one, clamped to `u16` like a
    /// real terminal cell. The plugin uses it to anchor its popup; absent
    /// or malformed coordinates are simply omitted (the plugin falls back
    /// to a default anchor).
    fn copy_context_anchor_cell(from: &serde_json::Value, into: &mut serde_json::Value) {
        if let Some(obj) = into.as_object_mut() {
            for cell in ["col", "row"] {
                if let Some(v) = from.get(cell).and_then(|v| v.as_u64()) {
                    obj.insert(cell.to_string(), serde_json::json!(v.min(u16::MAX as u64)));
                }
            }
        }
    }

    /// Rebuild the `HitArea` that `collect_list` would have emitted for a
    /// list row that is outside the TUI's scroll window (so no hit was
    /// recorded), from the panel's own spec: the payload's `list_key` must
    /// name a `List` in the spec and `index` must be in bounds; the item key
    /// is read from the spec's `item_keys`. Returns `None` for anything
    /// that isn't a valid in-bounds list `select` or right-click `context`
    /// (which is never recorded as a hit — the TUI synthesizes it from a
    /// right-click as well).
    fn synthesize_list_hit(
        panel: &crate::widgets::WidgetPanelState,
        event_type: &str,
        payload: &serde_json::Value,
    ) -> Option<crate::widgets::HitArea> {
        if event_type != "select" && event_type != "context" {
            return None;
        }
        let list_key = payload.get("list_key")?.as_str()?;
        let index = payload.get("index")?.as_i64()?;
        let spec = crate::widgets::find_widget_by_key(&panel.spec, list_key)?;
        let fresh_core::api::WidgetSpec::List {
            items,
            item_specs,
            item_keys,
            ..
        } = spec
        else {
            return None;
        };
        let total = if item_specs.is_empty() {
            items.len()
        } else {
            item_specs.len()
        };
        if index < 0 || index as usize >= total {
            return None;
        }
        let item_key = item_keys.get(index as usize).cloned().unwrap_or_default();
        let mut row_payload = serde_json::json!({
            "index": index,
            "key": item_key,
            "list_key": list_key,
        });
        let event_type = if event_type == "context" {
            Self::copy_context_anchor_cell(payload, &mut row_payload);
            "context"
        } else {
            "select"
        };
        Some(crate::widgets::HitArea {
            row_target: true,
            context_click: true,
            overlay: false,
            widget_key: item_key.clone(),
            widget_kind: "list",
            buffer_row: 0,
            byte_start: 0,
            byte_end: 0,
            payload: row_payload,
            event_type,
            owner_key: Some(list_key.to_string()),
        })
    }

    /// Rebuild the `HitArea` `render_widget_tree` would have emitted for a
    /// tree row outside the TUI's scroll window (so no hit was recorded),
    /// from the panel's own spec: `widget_key` must name a `Tree`, the
    /// payload's `index` must be in bounds, and the row's item key comes
    /// from the spec's `item_keys`. Covers the row-body `select`, the
    /// disclosure `expand`, the checkbox `toggle`, and the right-click
    /// `context` events (the natively-scrolled web frontend can click any
    /// row, not just the TUI's visible window; no `context` hit is ever
    /// recorded — the TUI synthesizes those from a right-click too).
    fn synthesize_tree_hit(
        panel: &crate::widgets::WidgetPanelState,
        widget_key: &str,
        event_type: &str,
        payload: &serde_json::Value,
    ) -> Option<crate::widgets::HitArea> {
        if !matches!(event_type, "select" | "expand" | "toggle" | "context")
            || widget_key.is_empty()
        {
            return None;
        }
        let index = payload.get("index")?.as_i64()?;
        let spec = crate::widgets::find_widget_by_key(&panel.spec, widget_key)?;
        let fresh_core::api::WidgetSpec::Tree {
            nodes, item_keys, ..
        } = spec
        else {
            return None;
        };
        if index < 0 || index as usize >= nodes.len() {
            return None;
        }
        let item_key = item_keys.get(index as usize).cloned().unwrap_or_default();
        let (event_type, payload) = match event_type {
            "expand" => (
                "expand",
                serde_json::json!({
                    "index": index,
                    "key": item_key,
                    "expanded": payload.get("expanded").and_then(|v| v.as_bool()).unwrap_or(true),
                }),
            ),
            // Right-click: same shape the TUI's context path fires —
            // the row identity plus the click cell the plugin anchors
            // its popup at (see `handle_floating_widget_context_click`).
            "context" => {
                let mut p = serde_json::json!({ "index": index, "key": item_key });
                Self::copy_context_anchor_cell(payload, &mut p);
                ("context", p)
            }
            // Checkbox click: same shape the renderer's checkbox hit
            // carries — `checked` is the NEW value, derived from the
            // spec (the plugin's pushed truth), never from the frontend.
            // Only nodes that actually bear a checkbox (checked is
            // Some) get one, mirroring the renderer.
            "toggle" => {
                let current = nodes.get(index as usize).and_then(|n| n.checked)?;
                (
                    "toggle",
                    serde_json::json!({
                        "index": index,
                        "key": item_key,
                        "checked": !current,
                    }),
                )
            }
            _ => (
                "select",
                serde_json::json!({ "index": index, "key": item_key }),
            ),
        };
        Some(crate::widgets::HitArea {
            row_target: true,
            context_click: true,
            overlay: false,
            widget_key: widget_key.to_string(),
            widget_kind: "tree",
            buffer_row: 0,
            byte_start: 0,
            byte_end: 0,
            payload,
            event_type,
            owner_key: None,
        })
    }

    /// Rebuild the `HitArea` the renderer would have emitted for a keyed
    /// control widget that recorded no hit — because the TUI clipped it (a
    /// native frontend grows a floating panel to fit its content) or
    /// because the frontend renders states the TUI's hit window didn't
    /// (e.g. a dropdown's option rows). State comes from the panel's own
    /// spec: a disabled Button synthesizes nothing (the renderer records no
    /// hit for it either), a Toggle's `checked` and a Dropdown's option
    /// bounds are read from the spec, never trusted from the frontend.
    fn synthesize_control_hit(
        panel: &crate::widgets::WidgetPanelState,
        widget_key: &str,
        event_type: &str,
        payload: &serde_json::Value,
    ) -> Option<crate::widgets::HitArea> {
        if widget_key.is_empty() {
            return None;
        }
        let spec = crate::widgets::find_widget_by_key(&panel.spec, widget_key)?;
        use fresh_core::api::WidgetSpec as W;
        let (widget_kind, event_type, payload): (_, &'static str, _) = match (spec, event_type) {
            (W::Button { disabled, .. }, "activate") if !disabled => {
                ("button", "activate", serde_json::json!({}))
            }
            (W::Toggle { checked, .. }, "toggle") => (
                "toggle",
                "toggle",
                serde_json::json!({ "checked": !checked }),
            ),
            (W::Text { .. }, "focus") => ("text", "focus", serde_json::json!({})),
            (W::Number { .. }, "number_value") => ("number", "number_value", serde_json::json!({})),
            (W::Dropdown { .. }, "dropdown_toggle") => {
                ("dropdown", "dropdown_toggle", serde_json::json!({}))
            }
            (W::Dropdown { options, .. }, "dropdown_select") => {
                let index = payload.get("index")?.as_i64()?;
                if index < 0 || index as usize >= options.len() {
                    return None;
                }
                (
                    "dropdown",
                    "dropdown_select",
                    serde_json::json!({ "index": index }),
                )
            }
            (W::DualList { options, .. }, "dual_focus") => {
                let column = payload.get("column")?.as_str()?;
                if column != "available" && column != "included" {
                    return None;
                }
                let index = payload.get("index")?.as_i64()?;
                // Loose bound: either column's row count can never exceed
                // the full option universe; the click handler clamps to the
                // live column length itself.
                if index < 0 || index as usize >= options.len().max(1) {
                    return None;
                }
                (
                    "dual_list",
                    "dual_focus",
                    serde_json::json!({ "column": column, "index": index }),
                )
            }
            _ => return None,
        };
        Some(crate::widgets::HitArea {
            row_target: false,
            context_click: false,
            overlay: false,
            widget_key: widget_key.to_string(),
            widget_kind,
            buffer_row: 0,
            byte_start: 0,
            byte_end: 0,
            payload,
            event_type,
            owner_key: None,
        })
    }

    /// Native-frontend entry point: place a text widget's caret at a flat
    /// byte offset into its value. A browser input positions its caret
    /// natively on click (it owns the font metrics), then reports the
    /// position here so the host `TextEdit` — the single source of truth —
    /// follows. `set_cursor_from_flat` clamps, snaps to a grapheme
    /// boundary, and clears any selection (matching a plain GUI click).
    pub fn set_widget_text_cursor(
        &mut self,
        plugin: &str,
        panel_id: u64,
        widget_key: &str,
        byte: usize,
    ) {
        let panel_key = crate::widgets::PanelKey::new(plugin, panel_id);
        let Some(panel) = self.widget_registry.get_mut(&panel_key) else {
            return;
        };
        let Some(crate::widgets::WidgetInstanceState::Text { editor: te, .. }) =
            panel.instance_states.get_mut(widget_key)
        else {
            return;
        };
        te.set_cursor_from_flat(byte);
        self.rerender_widget_panel(&panel_key);
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
        // A widget panel is laid out to the panel's exact width and clipped
        // there, so its view has nothing to scroll sideways to. Pin it
        // before anything else: the focus cursor below can sit at the end
        // of a row that reaches the right edge, and cursor-following would
        // otherwise drag the whole panel — header included — left by a
        // column or two.
        self.pin_widget_panel_horizontal_scroll(buffer_id);

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

    /// Mark every view of `buffer_id` as non-horizontally-scrollable.
    ///
    /// Called on each widget-panel repaint rather than once at mount:
    /// a panel that is hidden and shown again gets a fresh
    /// `SplitViewState`, and the flag has to land on that one too.
    fn pin_widget_panel_horizontal_scroll(&mut self, buffer_id: BufferId) {
        for vs in self
            .windows
            .get_mut(&self.active_window)
            .and_then(|w| w.split_view_states_mut())
            .expect("active window must have a populated split layout")
            .values_mut()
        {
            if vs.buffer_state(buffer_id).is_none() {
                continue;
            }
            if vs.active_buffer == buffer_id {
                vs.viewport.horizontal_scroll_enabled = false;
                vs.viewport.left_column = 0;
            }
            if let Some(bs) = vs.keyed_states.get_mut(&buffer_id) {
                bs.viewport.horizontal_scroll_enabled = false;
                bs.viewport.left_column = 0;
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

    /// Height sibling of [`Self::widget_panel_width`]: the viewport
    /// height of a split currently rendering this buffer, or `None`
    /// when the buffer isn't on screen (auto-sized widgets then keep
    /// the legacy fallback until it is). No padding is subtracted —
    /// the viewport height is already the buffer's usable rows.
    pub(super) fn widget_panel_height(&self, buffer_id: BufferId) -> Option<u32> {
        // Prefer the rect the last draw actually gave this panel. The
        // split view-state's viewport is a seed the layout pass computes,
        // and for a buffer-group panel it can only be a guess: the group's
        // inner tree is stashed out of the main split tree, so
        // `apply_layout` finds no rect for those leaves and falls back to
        // the whole editor height. Sizing a list to that overshoots the
        // panel and clips its last rows.
        if let Some(painted) = self.painted_panel_height(buffer_id) {
            return Some(painted);
        }
        self.windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .and_then(|vs| {
                vs.values()
                    .find(|vs| vs.buffer_state(buffer_id).is_some() && vs.viewport.height > 0)
                    .map(|vs| vs.viewport.height as u32)
            })
    }

    /// Height of the content rect the last draw gave `buffer_id`, or
    /// `None` when it wasn't painted into a split at all (hidden panel,
    /// a group slot pointing at some other buffer).
    fn painted_panel_height(&self, buffer_id: BufferId) -> Option<u32> {
        self.active_layout()
            .split_areas
            .iter()
            .find(|(_, id, _, _, _, _)| *id == buffer_id)
            .map(|(_, _, content_rect, _, _, _)| content_rect.height as u32)
            .filter(|h| *h > 0)
    }

    /// Buffer-mounted widget panels whose split no longer matches the row
    /// budget their auto-sized (`visible_rows: None`) lists and trees were
    /// windowed to — a resize, a divider drag, a panel becoming visible.
    ///
    /// Deliberately narrow, because the repaint it drives happens mid-draw:
    ///
    /// * only panels currently painted into a split (a panel whose buffer
    ///   has been swapped out of its group's slot has no geometry to be
    ///   stale against, and must not be rewritten underneath the plugin);
    /// * only panels that actually *have* an auto-sized list or tree —
    ///   a spec that pins every `visible_rows` lays out the same at any
    ///   height, so repainting it would be work with no visible effect;
    /// * and the comparison is against the height the panel was last
    ///   *rendered* against, not the previous frame's viewport, so a panel
    ///   is repainted once per size change rather than once per frame.
    pub(super) fn widget_panels_with_stale_height(&self) -> Vec<crate::widgets::PanelKey> {
        self.widget_registry
            .panel_keys()
            .into_iter()
            .filter(|key| {
                let Some((buffer_id, spec)) = self.widget_registry.buffer_and_spec_ref(key) else {
                    return false;
                };
                // Floating and dock panels size themselves to their own
                // frame (`floating_panel_inner_height`) and are re-rendered
                // by the paths that move them; only the split-mounted ones
                // take their budget from a split.
                if Self::slot_for_panel_buffer(buffer_id).is_some() {
                    return false;
                }
                if !spec_has_auto_sized_list(spec) {
                    return false;
                }
                let Some(painted) = self.painted_panel_height(buffer_id) else {
                    return false;
                };
                self.widget_panel_render_heights.get(key) != Some(&painted)
            })
            .collect()
    }

    /// Record the row budget `panel_key` was just rendered against. Called
    /// from every path that renders a buffer-mounted panel, so
    /// [`Self::widget_panels_with_stale_height`] can tell a panel that has
    /// seen the current geometry from one that has not.
    pub(super) fn record_widget_panel_render_height(
        &mut self,
        panel_key: &crate::widgets::PanelKey,
        avail_height: Option<u32>,
    ) {
        match avail_height {
            Some(h) => {
                self.widget_panel_render_heights
                    .insert(panel_key.clone(), h);
            }
            None => {
                self.widget_panel_render_heights.remove(panel_key);
            }
        }
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
        let rendered_height: Option<u32>;
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
            // Floating panels that opted into the focus-marker gutter
            // (the Orchestrator New Session form) must re-render
            // through the same marker renderer on every host-driven
            // refresh — otherwise a Tab / focus advance would repaint
            // the panel without the gutter and the layout would jump.
            let focus_marker = panel_slot
                .and_then(|slot| self.panel(slot))
                .map(|f| f.focus_marker)
                .unwrap_or(false);
            // This is also the path a hover change re-renders through, so
            // the panel's tracked hover key has to reach the renderer here
            // — otherwise entering a `×` would repaint it unhighlighted.
            let hover_key = panel_slot
                .and_then(|slot| self.panel(slot))
                .map(|f| f.hovered_widget_key.clone())
                .unwrap_or_default();
            let hover_item_key = panel_slot
                .and_then(|slot| self.panel(slot))
                .map(|f| f.hovered_item_key.clone())
                .unwrap_or_default();
            // Row budget for auto-sized lists/trees: the floating
            // panel's inner height when this is a floating/dock slot,
            // else the split viewport height of the panel's buffer.
            let avail_height = match panel_slot {
                Some(slot) => self.floating_panel_inner_height(slot),
                None => self.widget_panel_height(buffer_id),
            };
            rendered_height = avail_height;
            let theme_guard = self.theme.read().unwrap();
            let out = render_floating_spec(
                focus_marker,
                spec,
                &prev,
                &prev_focus,
                panel_width,
                avail_height,
                &hover_key,
                &hover_item_key,
                Some(crate::widgets::MarkdownCtx {
                    theme: &theme_guard,
                    grammars: Some(self.grammar_registry.as_ref()),
                }),
            );
            (buffer_id, is_floating, panel_width, out)
        };
        let _ = panel_width;
        self.record_widget_panel_render_height(panel_key, rendered_height);
        let panel_slot = Self::slot_for_panel_buffer(buffer_id);
        let focus_cursor = out_pieces.focus_cursor;
        let entries = out_pieces.entries;
        let embeds = out_pieces.embeds;
        let overlays = out_pieces.overlays;
        let panel_boxes = out_pieces.boxes.clone();
        let popup = out_pieces.popup;
        if self
            .widget_registry
            .update_side_effects(
                panel_key,
                out_pieces.hits,
                out_pieces.instance_states,
                out_pieces.focus_key,
                out_pieces.tabbable,
                out_pieces.effective_rows,
                out_pieces.boxes,
            )
            .is_none()
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
                    fwp.boxes = panel_boxes;
                    fwp.popup = popup;
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
        // Kind-owned key handling (widget-framework-v2-review.md §4.3):
        // the focused widget's impl claims keys its own open popup
        // needs — Text's completion list, Dropdown's option list —
        // and passes everything else through. The popup short-circuit
        // ladders that used to live here are gone; no code at this
        // level knows those popups exist. `PassAfter` covers the
        // dismiss-then-act keys (Enter submitting the form, Tab
        // advancing focus, after closing a non-navigated popup).
        if !focus_key.is_empty() {
            let widget = crate::widgets::find_widget_by_key(&panel.spec, &focus_key).cloned();
            if let Some(widget) = widget {
                let mut fx = crate::widgets::kinds::KeyFx::default();
                let disposition = match self.widget_registry.get_mut(panel_key) {
                    Some(panel_mut) => crate::widgets::kinds::behavior(&widget)
                        .on_key(&widget, &focus_key, panel_mut, key, &mut fx),
                    None => return,
                };
                if fx.flash_scrollbar {
                    // Keyboard nav in the dock: flash its overlay
                    // scrollbar so the user sees where the selection
                    // sits in the overflowing list even though the
                    // pointer (whose hover normally reveals the bar)
                    // never moved.
                    self.flash_dock_scrollbar(panel_key);
                }
                if let Some(text) = fx.clipboard_copy.take() {
                    self.clipboard.copy(text);
                }
                if disposition != crate::widgets::kinds::KeyDisposition::Pass {
                    self.rerender_widget_panel(panel_key);
                }
                for (event_type, payload) in fx.events {
                    self.fire_widget_event(panel_key, focus_key.clone(), event_type, payload);
                }
                if let Some(delta) = fx.focus_advance {
                    self.handle_widget_focus_advance(panel_key, delta);
                }
                if disposition == crate::widgets::kinds::KeyDisposition::Consumed {
                    return;
                }
            }
        }
        // Re-fetch the focused widget for the main dispatch: the
        // kind-owned handler above ran `&mut self` (it may have closed
        // a popup), so we can't hold a borrow from before it. The spec
        // is unchanged by a dismiss, so this resolves to the same
        // widget.
        let panel = match self.widget_registry.get(panel_key) {
            Some(p) => p,
            None => return,
        };
        let widget = if focus_key.is_empty() {
            None
        } else {
            crate::widgets::find_widget_by_key(&panel.spec, &focus_key)
        };
        match key {
            "Tab" => self.handle_widget_focus_advance(panel_key, 1),
            "Shift+Tab" => self.handle_widget_focus_advance(panel_key, -1),
            "Up" | "Down" => {
                let delta = if key == "Up" { -1 } else { 1 };
                // Picker-style nav, capability-declared: the focused
                // kind says whether panel arrows should walk the focus
                // ring instead (`arrows_advance_focus` — Button/Toggle,
                // no vertical axis of their own), and the panel's
                // picker target says how an arrow reaches it
                // (`picker_nav`: List peeks, Tree takes focus). No
                // kind matching here — the capabilities are the kinds'
                // declarations.
                let arrows_advance = widget
                    .map(|w| crate::widgets::kinds::behavior(w).arrows_advance_focus())
                    .unwrap_or(false);
                let scrollable = self
                    .widget_registry
                    .get(panel_key)
                    .and_then(|p| find_scrollable_widget_key(&p.spec));
                if scrollable.is_none() && arrows_advance {
                    // Button-only popups (the dock's right-click
                    // context menu, confirm panes): arrows walk
                    // the controls like Tab / Shift+Tab, matching
                    // every other menu in the dock.
                    self.handle_widget_focus_advance(panel_key, delta);
                }
                if let Some(target_key) = scrollable {
                    let nav = self
                        .widget_registry
                        .get(panel_key)
                        .and_then(|p| crate::widgets::find_widget_by_key(&p.spec, &target_key))
                        .map(|w| crate::widgets::kinds::behavior(w).picker_nav())
                        .unwrap_or(crate::widgets::kinds::PickerNav::Skip);
                    match nav {
                        crate::widgets::kinds::PickerNav::Peek => {
                            self.handle_widget_select_move_for_key(panel_key, &target_key, delta);
                        }
                        crate::widgets::kinds::PickerNav::TakeFocus => {
                            // set_panel_focus_and_notify seeds the
                            // target's selection to the first visible
                            // row (the kind's on_focus_change).
                            self.set_panel_focus_and_notify(panel_key, target_key.clone());
                            self.rerender_widget_panel(panel_key);
                        }
                        crate::widgets::kinds::PickerNav::Skip => {}
                    }
                }
            }
            "Enter" => match widget {
                Some(fresh_core::api::WidgetSpec::Text { .. }) => {
                    // Multi-line Enter (newline, or markdown
                    // activate) is kind-owned in on_key; what
                    // reaches here is a single-line field.
                    if let Some(target_key) = self
                        .widget_registry
                        .get(panel_key)
                        .and_then(|p| find_scrollable_widget_key(&p.spec))
                    {
                        // Picker-style activate, capability-declared
                        // (`activates_on_picker_enter` +
                        // `picker_activate_event`): a single-line
                        // filter input paired with a picker fires that
                        // target's activation on Enter, so the user
                        // can type-then-Enter without tabbing focus.
                        self.fire_picker_activate(panel_key, &target_key);
                    } else {
                        // Form-like UX: Enter commits the field and
                        // moves to the next tabbable widget.
                        self.handle_widget_focus_advance(panel_key, 1);
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
        // The ring comes from the layout-box tree, scoped to the
        // nearest focus-trap ancestor of the focused box (a modal /
        // Component subtree contains Tab cycling; without traps this
        // is the whole panel's document order). Panels mounted without
        // a box tree (tests, legacy paths) fall back to the stored
        // flat ring.
        let ring = {
            let scoped =
                crate::widgets::layout_box::focus_ring_scoped(&panel.boxes, &panel.focus_key);
            if scoped.is_empty() {
                panel.tabbable.clone()
            } else {
                scoped
            }
        };
        if ring.is_empty() {
            return;
        }
        let cur_idx = ring.iter().position(|k| k == &panel.focus_key).unwrap_or(0) as i32;
        let n = ring.len() as i32;
        let new_idx = ((cur_idx + delta) % n + n) % n;
        let new_key = ring[new_idx as usize].clone();
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
        // Offer the transition to the kinds: the widget losing focus
        // and the one gaining it each get their `on_focus_change`
        // hook (Tree keeps its selected-row highlight coherent with
        // focus — exactly one focused element). Kind-blind: no Tree
        // match here.
        self.notify_widget_focus_change(panel_key, &old_key, &new_key);
        self.fire_widget_event(
            panel_key,
            new_key,
            "focus".to_string(),
            serde_json::json!({ "previous": old_key }),
        );
    }

    /// Offer a panel-focus transition to the kinds: the widget losing
    /// focus and the one gaining it each run their
    /// `WidgetImpl::on_focus_change` hook against the panel state.
    /// The per-kind policy (Tree's selection seeding/clearing) lives
    /// with the kind, not here.
    fn notify_widget_focus_change(
        &mut self,
        panel_key: &crate::widgets::PanelKey,
        old_key: &str,
        new_key: &str,
    ) {
        if old_key == new_key {
            return;
        }
        for (key, gained) in [(old_key, false), (new_key, true)] {
            if key.is_empty() {
                continue;
            }
            let Some(spec) = self
                .widget_registry
                .get(panel_key)
                .and_then(|p| crate::widgets::find_widget_by_key(&p.spec, key))
                .cloned()
            else {
                continue;
            };
            if let Some(panel) = self.widget_registry.get_mut(panel_key) {
                crate::widgets::kinds::behavior(&spec).on_focus_change(panel, key, gained);
            }
        }
    }

    fn handle_widget_activate(&mut self, panel_key: &crate::widgets::PanelKey) {
        // Fire the focused widget's own semantic activation event —
        // the kind decides through `WidgetImpl::activate_event`
        // (Button → "activate" unless disabled, Toggle → "toggle"
        // with the flipped value, everything else: no-op).
        let panel = match self.widget_registry.get(panel_key) {
            Some(p) => p,
            None => return,
        };
        let focus_key = panel.focus_key.clone();
        if focus_key.is_empty() {
            return;
        }
        let ev = crate::widgets::find_widget_by_key(&panel.spec, &focus_key)
            .and_then(|spec| crate::widgets::kinds::behavior(spec).activate_event(spec));
        if let Some((event_type, payload)) = ev {
            self.fire_widget_event(panel_key, focus_key, event_type.to_string(), payload);
        }
    }

    /// Fire the picker target's activation event for its current
    /// selection, capability-driven: the kind declares participation
    /// (`activates_on_picker_enter`) and supplies the event
    /// (`picker_activate_event`) — replaces the per-kind
    /// fire_list_activate / fire_tree_activate pair.
    fn fire_picker_activate(&mut self, panel_key: &crate::widgets::PanelKey, focus_key: &str) {
        let ev = self.widget_registry.get(panel_key).and_then(|panel| {
            let spec = crate::widgets::find_widget_by_key(&panel.spec, focus_key)?;
            let b = crate::widgets::kinds::behavior(spec);
            if !b.activates_on_picker_enter() {
                return None;
            }
            b.picker_activate_event(spec, focus_key, panel)
        });
        if let Some((event_type, payload)) = ev {
            self.fire_widget_event(panel_key, focus_key.to_string(), event_type, payload);
        }
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

    /// Shared shell for host paths that run a kind-owned mutation
    /// outside the focused-key dispatch (the picker-style Up/Down
    /// forwarding to a sibling scrollable): resolve the widget's spec
    /// node, run the mutation, repaint, and apply the queued `KeyFx`.
    fn with_kind_mutation(
        &mut self,
        panel_key: &crate::widgets::PanelKey,
        widget_key: &str,
        f: impl FnOnce(
            &fresh_core::api::WidgetSpec,
            &str,
            &mut crate::widgets::WidgetPanelState,
            &mut crate::widgets::kinds::KeyFx,
        ),
    ) {
        if widget_key.is_empty() {
            return;
        }
        let widget = match self
            .widget_registry
            .get(panel_key)
            .and_then(|p| crate::widgets::find_widget_by_key(&p.spec, widget_key))
        {
            Some(w) => w.clone(),
            None => return,
        };
        let mut fx = crate::widgets::kinds::KeyFx::default();
        match self.widget_registry.get_mut(panel_key) {
            Some(panel) => f(&widget, widget_key, panel, &mut fx),
            None => return,
        }
        if fx.flash_scrollbar {
            self.flash_dock_scrollbar(panel_key);
        }
        if let Some(text) = fx.clipboard_copy.take() {
            self.clipboard.copy(text);
        }
        self.rerender_widget_panel(panel_key);
        for (event_type, payload) in fx.events {
            self.fire_widget_event(panel_key, widget_key.to_string(), event_type, payload);
        }
        if let Some(delta) = fx.focus_advance {
            self.handle_widget_focus_advance(panel_key, delta);
        }
    }

    /// Same as [`handle_widget_select_move`] but targets an explicit
    /// `List` widget key instead of the panel's focused widget. Used
    /// by the picker-style smart-key dispatch — `Up`/`Down` on a
    /// focused filter input route to the first scrollable widget in
    /// the panel without changing focus. Thin shell over the same
    /// kind-owned mutation `List::on_key` uses.
    fn handle_widget_select_move_for_key(
        &mut self,
        panel_key: &crate::widgets::PanelKey,
        widget_key: &str,
        delta: i32,
    ) {
        self.with_kind_mutation(panel_key, widget_key, |spec, key, panel, fx| {
            crate::widgets::kinds::list::select_move(spec, key, panel, delta, fx);
        });
    }

    /// Arm the dock's keyboard scrollbar flash: if `panel_key` is the
    /// mounted dock panel, record a visibility deadline
    /// [`super::DOCK_SCROLLBAR_FLASH`] from now on the editor's
    /// `time_source` (so tests can drive expiry on the logical clock).
    /// No-op for the centered modal / anchored popups — their scrollbars
    /// are always visible.
    fn flash_dock_scrollbar(&mut self, panel_key: &crate::widgets::PanelKey) {
        let until = self.time_source().now() + super::DOCK_SCROLLBAR_FLASH;
        if let Some(dock) = self.dock.as_mut() {
            if &dock.panel_key == panel_key {
                dock.scrollbar_flash_until = Some(until);
            }
        }
    }

    /// Editor-tick check: clear an expired dock scrollbar flash and
    /// request a repaint so the bar disappears without waiting for the
    /// next input event. Returns `true` when a redraw is needed (the
    /// deadline just passed). While the flash is still live, no work is
    /// done here — the renderer keeps showing the bar and the main loop's
    /// idle poll (≤50ms) guarantees a tick lands shortly after expiry.
    pub(crate) fn check_dock_scrollbar_flash_expiry(&mut self) -> bool {
        let now = self.time_source().now();
        if let Some(dock) = self.dock.as_mut() {
            if dock.scrollbar_flash_until.is_some_and(|t| now >= t) {
                dock.scrollbar_flash_until = None;
                return true;
            }
        }
        false
    }

    /// Mouse-wheel scroll over a widget panel buffer. With `pos` —
    /// the pointer's panel-relative (row, display column) — the wheel
    /// scrolls the `List`/`Tree` whose rendered region contains the
    /// pointer, so two side-by-side lists (the code tour's Steps rail
    /// and prose column) each answer to the wheel hovering over them.
    /// Without a position, or when the pointer sits on panel chrome
    /// outside every list, it falls back to the first `Tree`/`List`
    /// in the spec (the pre-position behaviour). Sets the widget's
    /// `user_scrolled` flag so the renderer's auto-scroll doesn't
    /// snap the offset back to the selection. No focus change,
    /// no `widget_event` fires — wheel is viewport navigation, not
    /// selection.
    ///
    /// Returns `true` if any panel consumed the scroll.
    pub(super) fn handle_widget_panel_wheel_at(
        &mut self,
        buffer_id: crate::model::event::BufferId,
        pos: Option<(u32, u32)>,
        delta: i32,
    ) -> bool {
        let panels = self.widget_registry.panels_for_buffer(buffer_id);
        let mut consumed = false;
        for panel_key in panels {
            // Hit-tested routing: the deepest box under the pointer,
            // then bubbling outward — each scrollable ancestor gets the
            // delta until one consumes it (scroll chaining). A widget
            // already at its bound returns false from `on_wheel`, so a
            // List/Tree that shows everything (e.g. Git Log, which sets
            // visible_rows == total and scrolls via its enclosing pane)
            // lets the wheel keep bubbling instead of going dead. With
            // no position, or a pointer on chrome outside every box,
            // fall back to the first scrollable widget in the spec (the
            // pre-position behaviour).
            let (spec, mut candidates) = match self.widget_registry.get(&panel_key) {
                Some(p) => {
                    let along_path: Vec<String> = pos
                        .map(|(row, col)| {
                            crate::widgets::layout_box::hit_path(&p.boxes, row, col)
                                .into_iter()
                                .rev()
                                .filter(|&i| p.boxes[i].scrollable)
                                .filter_map(|i| p.boxes[i].key.clone())
                                .collect()
                        })
                        .unwrap_or_default();
                    (p.spec.clone(), along_path)
                }
                None => continue,
            };
            if candidates.is_empty() {
                if let Some(k) = find_scrollable_widget_key(&spec) {
                    candidates.push(k);
                }
            }
            for widget_key in candidates {
                let Some(widget) = crate::widgets::find_widget_by_key(&spec, &widget_key) else {
                    continue;
                };
                let Some(panel) = self.widget_registry.get_mut(&panel_key) else {
                    break;
                };
                if crate::widgets::kinds::behavior(widget).on_wheel(
                    widget,
                    &widget_key,
                    panel,
                    delta,
                ) {
                    self.rerender_widget_panel(&panel_key);
                    consumed = true;
                    break;
                }
            }
        }
        consumed
    }

    /// Fire `widget_event { event_type: "activate" }` for the focused
    /// Tree's currently-selected node. Mirrors `fire_list_activate`
    /// — the plugin's handler decides what "activate" means
    /// (open the file, run an action, etc.).
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
        self.widget_registry
            .panels_for_buffer(buffer_id)
            .into_iter()
            .find(|panel_key| self.panel_focused_widget_is_text(panel_key))
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
    /// Select-all in the focused widget. ONE owner for the behavior:
    /// the kind's own `C-a` arm (`Text::on_key`) — this shell only
    /// translates the host action into the kind vocabulary. Returns
    /// true when a panel existed to receive it (the action is
    /// consumed either way so it doesn't fall through to the buffer).
    pub(super) fn handle_widget_select_all(
        &mut self,
        panel_key: &crate::widgets::PanelKey,
    ) -> bool {
        if self.widget_registry.get(panel_key).is_none() {
            return false;
        }
        self.handle_widget_key(panel_key, "C-a");
        true
    }

    /// Copy in the focused widget — routed through the kind's `C-c`
    /// arm (one owner; the kind decides what "copy" means, including
    /// consuming with an empty selection so the action never falls
    /// through to the buffer's copy path).
    pub(super) fn handle_widget_copy(&mut self, panel_key: &crate::widgets::PanelKey) -> bool {
        if self.widget_registry.get(panel_key).is_none() {
            return false;
        }
        self.handle_widget_key(panel_key, "C-c");
        true
    }

    /// Cut in the focused widget — routed through the kind's `C-x`
    /// arm (one owner; the read-only/markdown cut-degrades-to-copy
    /// policy lives there, once).
    pub(super) fn handle_widget_cut(&mut self, panel_key: &crate::widgets::PanelKey) -> bool {
        if self.widget_registry.get(panel_key).is_none() {
            return false;
        }
        self.handle_widget_key(panel_key, "C-x");
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
        // Read-only / markdown documents accept no insertion — but the
        // paste is still consumed (it must not leak into the buffer
        // behind the panel).
        if self.focused_text_mode(panel_key).1 {
            return true;
        }
        let owned = text.to_string();
        self.with_focused_text_editor(panel_key, move |editor| {
            editor.insert_str(&owned);
        });
        true
    }

    /// Apply a mutating operation to the focused `Text` widget's
    /// `TextEdit` — the host shell over the kind-owned
    /// `kinds::text::apply_edit` (seeding, no-op detection, and the
    /// `change` event live there, once). Repaints and fires the
    /// queued events; returns true when the op produced a visible
    /// change.
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
        let widget = match self
            .widget_registry
            .get(panel_key)
            .and_then(|p| crate::widgets::find_widget_by_key(&p.spec, &focus_key))
        {
            Some(w) => w.clone(),
            None => return false,
        };
        let mut fx = crate::widgets::kinds::KeyFx::default();
        let changed = match self.widget_registry.get_mut(panel_key) {
            Some(panel) => {
                crate::widgets::kinds::text::apply_edit(&widget, &focus_key, panel, &mut fx, op)
            }
            None => return false,
        };
        if changed {
            self.rerender_widget_panel(panel_key);
        }
        for (event_type, payload) in fx.events {
            self.fire_widget_event(panel_key, focus_key.clone(), event_type, payload);
        }
        changed
    }

    /// Reposition a just-focused Text widget's cursor to the byte under
    /// a mouse click (#2573). `entry_byte` is the click's byte offset
    /// within the rendered row (as resolved by `hit_test`); `payload` is
    /// the `focus` HitArea payload, which carries the value-layout
    /// breadcrumbs the renderer stamped on it (`valueInnerStart` and the
    /// truncation fields). Maps the row byte back to a value byte, moves
    /// the cursor, and fires `change` so a plugin mirroring the cursor
    /// position (e.g. Search & Replace) stays in sync.
    ///
    /// A no-op for hits without the layout payload (older render paths,
    /// non-text widgets) or when the clicked widget isn't the focused
    /// one — the caller is expected to focus it first.
    pub(super) fn reposition_widget_text_cursor_from_click(
        &mut self,
        panel_key: &crate::widgets::PanelKey,
        widget_key: &str,
        entry_byte: usize,
        hit_byte_start: usize,
        payload: &serde_json::Value,
    ) {
        // `valueInnerStart` is relative to the *field's own* rendered
        // text (gutter + label + `[`). Fields can be composed
        // horizontally into a shared row (Search + Replace live on one
        // line), so `hit_byte_start` — the field's offset within that
        // composed row — rebases both the click and the value origin
        // into the same coordinate space.
        let inner_start = match payload.get("valueInnerStart").and_then(|v| v.as_u64()) {
            Some(v) => v as usize,
            None => return,
        };
        let offset_in_field = entry_byte.saturating_sub(hit_byte_start);
        // The cursor op below targets the panel's *focused* widget; guard
        // that focus already landed on the clicked field so a stray call
        // can't move an unrelated field's cursor.
        let is_focused = self
            .widget_registry
            .get(panel_key)
            .map(|p| p.focus_key == widget_key)
            .unwrap_or(false);
        if !is_focused {
            return;
        }
        let value_len = payload
            .get("valueLen")
            .and_then(|v| v.as_u64())
            .unwrap_or(0) as usize;
        let dropped = payload
            .get("valueDropped")
            .and_then(|v| v.as_u64())
            .unwrap_or(0) as usize;
        let ellipsis = payload
            .get("ellipsisBytes")
            .and_then(|v| v.as_u64())
            .unwrap_or(0) as usize;

        // Translate the click's field byte → value byte (shared with the
        // Settings UI via `crate::widgets`). `offset_in_field` already
        // rebased the click by `hit_byte_start`, so pass `byte_start = 0`.
        let value_byte = crate::widgets::row_byte_to_value_byte(
            offset_in_field,
            0,
            inner_start,
            dropped,
            ellipsis,
            value_len,
        );

        self.with_focused_text_editor(panel_key, |editor| editor.set_cursor_from_flat(value_byte));
    }

    /// Flat byte offset of `(line, byte_in_line)` within `value`,
    /// clamping the line into range and the byte onto a char boundary
    /// of that line. Newlines count one byte each, matching
    /// [`TextEdit::flat_cursor_byte`](crate::primitives::text_edit::TextEdit).
    fn markdown_line_byte_to_flat(value: &str, line: usize, byte_in_line: usize) -> usize {
        let mut flat = 0usize;
        for (i, l) in value.split('\n').enumerate() {
            if i == line {
                let mut b = byte_in_line.min(l.len());
                while b > 0 && !l.is_char_boundary(b) {
                    b -= 1;
                }
                return flat + b;
            }
            flat += l.len() + 1;
        }
        value.len()
    }

    /// A press on a markdown document row: focus already moved (the
    /// caller's tabbable path), so place the caret at the clicked byte
    /// of rendered line `line`, re-arm keep-caret-visible, and arm
    /// drag-to-select anchored at the press.
    pub(super) fn position_markdown_text_cursor_from_click(
        &mut self,
        panel_key: &crate::widgets::PanelKey,
        widget_key: &str,
        line: usize,
        byte_in_line: usize,
    ) {
        let is_focused = self
            .widget_registry
            .get(panel_key)
            .map(|p| p.focus_key == widget_key)
            .unwrap_or(false);
        if !is_focused {
            return;
        }
        let Some(flat) = ({
            let panel = self.widget_registry.get(panel_key);
            panel.and_then(|p| match p.instance_states.get(widget_key) {
                Some(crate::widgets::WidgetInstanceState::Text { editor, .. }) => Some(
                    Self::markdown_line_byte_to_flat(&editor.value(), line, byte_in_line),
                ),
                _ => None,
            })
        }) else {
            return;
        };
        self.clear_focused_text_user_scrolled(panel_key);
        let moved = self.with_focused_text_editor(panel_key, |editor| {
            editor.set_cursor_from_flat(flat);
        });
        // A click that lands on the caret's own cell still dismisses an
        // existing selection: `set_cursor_from_flat` cleared the anchor,
        // but `with_focused_text_editor` saw no cursor/value change, so
        // repaint explicitly.
        if !moved {
            self.rerender_widget_panel(panel_key);
        }
        self.widget_text_drag = Some(super::WidgetTextDrag {
            panel: panel_key.clone(),
            widget: widget_key.to_string(),
            anchor_flat: flat,
        });
    }

    /// Extend the drag selection of an armed widget-text drag to
    /// `(line, byte_in_line)`: caret moves there, anchor stays at the
    /// press position. Selection-only — no `change` event fires.
    pub(super) fn extend_widget_text_selection_to(
        &mut self,
        drag: &super::WidgetTextDrag,
        line: usize,
        byte_in_line: usize,
    ) {
        let Some(panel) = self.widget_registry.get_mut(&drag.panel) else {
            return;
        };
        let Some(crate::widgets::WidgetInstanceState::Text { editor, .. }) =
            panel.instance_states.get_mut(&drag.widget)
        else {
            return;
        };
        let value = editor.value();
        let head = Self::markdown_line_byte_to_flat(&value, line, byte_in_line);
        // Anchor (row, col) from its flat offset: park the cursor there
        // momentarily to reuse the flat→(row, col) clamping, then move
        // the cursor to the head and re-attach the anchor.
        editor.set_cursor_from_flat(drag.anchor_flat);
        let anchor_rc = (editor.cursor_row, editor.cursor_col);
        editor.set_cursor_from_flat(head);
        editor.selection_anchor = if head != drag.anchor_flat {
            Some(anchor_rc)
        } else {
            None
        };
        self.rerender_widget_panel(&drag.panel);
    }

    /// Apply a non-printable editing key to the focused text widget —
    /// the host shell over the kind-owned `kinds::text::text_key`
    /// (the shared text-key table, read-only gating, and markdown
    /// Enter-as-activate live there). Kept as an Editor entry point
    /// for the plugin-facing `WidgetAction::TextInputKey`.
    fn handle_widget_text_key(&mut self, panel_key: &crate::widgets::PanelKey, key: &str) {
        let focus_key = match self.widget_registry.get(panel_key) {
            Some(p) if !p.focus_key.is_empty() => p.focus_key.clone(),
            _ => return,
        };
        self.with_kind_mutation(panel_key, &focus_key, |spec, wkey, panel, fx| {
            crate::widgets::kinds::text::text_key(spec, wkey, panel, key, fx);
        });
    }

    /// `(markdown, read_only)` for the panel's focused widget —
    /// `kinds::text::mode` on its spec; `(false, false)` for a
    /// non-Text focus.
    fn focused_text_mode(&self, panel_key: &crate::widgets::PanelKey) -> (bool, bool) {
        let Some(panel) = self.widget_registry.get(panel_key) else {
            return (false, false);
        };
        if panel.focus_key.is_empty() {
            return (false, false);
        }
        match crate::widgets::find_widget_by_key(&panel.spec, &panel.focus_key) {
            Some(spec) => crate::widgets::kinds::text::mode(spec),
            None => (false, false),
        }
    }

    /// Clear the focused Text widget's `user_scrolled` flag (re-arming
    /// keep-caret-visible). Returns true when the flag was set.
    fn clear_focused_text_user_scrolled(&mut self, panel_key: &crate::widgets::PanelKey) -> bool {
        let Some(panel) = self.widget_registry.get_mut(panel_key) else {
            return false;
        };
        let focus_key = panel.focus_key.clone();
        if focus_key.is_empty() {
            return false;
        }
        crate::widgets::kinds::text::clear_user_scrolled(&focus_key, panel)
    }

    /// Insert printable / IME-committed text at the focused text
    /// widget's cursor. Same path for single-line and multi-line —
    /// `TextEdit::insert_str` strips `\n` automatically when the
    /// editor was constructed single-line. `text` may be a single
    /// codepoint, a grapheme cluster, or a multi-codepoint IME
    /// commit; `insert_str` handles each identically.
    pub(super) fn handle_widget_text_char(
        &mut self,
        panel_key: &crate::widgets::PanelKey,
        text: &str,
    ) {
        if text.is_empty() || self.focused_text_mode(panel_key).1 {
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

    /// Height sibling of [`Self::floating_panel_inner_width`]: the row
    /// budget auto-sized (`visible_rows: None`) lists/trees inside this
    /// panel size themselves to. A left dock spans the terminal height;
    /// a centered modal takes its `height_pct` share. 2 rows are
    /// reserved for the panel frame, mirroring the width helper's
    /// 2-column reservation.
    pub(super) fn floating_panel_inner_height(&self, slot: super::PanelSlot) -> Option<u32> {
        let term_h = (self.terminal_height.max(1)) as u32;
        let panel = self.panel(slot)?;
        let h = match panel.placement {
            super::PanelPlacement::LeftDock { .. } => term_h,
            _ => {
                let pct = panel.height_pct.clamp(1, 100) as u32;
                (term_h * pct) / 100
            }
        };
        Some(h.saturating_sub(2).max(3))
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
                // Drop the closed split from every terminal's scrollback set.
                self.active_window_mut()
                    .forget_split_terminal_modes(leaf_id);
                // The surviving panes just grew into the closed split's
                // space — reflow through the layout funnel so their
                // terminals are resized, same as `close_active_split`.
                self.relayout();
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

/// Where a screen cell lands inside a floating widget panel. See
/// [`Editor::probe_floating_widget`].
struct FloatingWidgetProbe {
    /// 0-indexed row within the panel's rendered entries.
    brow: u32,
    /// UTF-8 byte offset within that row's text (the overlay's text when
    /// an overlay covers the row).
    bcol: usize,
    /// The widget under the cell, or `None` for a cell on no widget.
    hit: Option<crate::widgets::HitArea>,
}

/// Panel pointer machinery shared by every mounted floating panel
/// (the dock and the centered modal): probe/hover resolution, list
/// scrollbar press/drag, dropdown pop-over clicks, wheel routing, and
/// dismissal. Behavior owned by the panel runtime (moved from
/// mouse_input.rs).
impl Editor {
    /// Hit-test a click against the floating widget panel. Clicks
    /// inside the panel's inner rect resolve to a widget row/byte
    /// and fire `widget_event` via the same path
    /// Forward a vertical-wheel scroll to the active floating
    /// widget panel — same plumbing the orchestrator's
    /// embedded-widget panels use, but the floating panel
    /// doesn't show up in `split_at_position` so it needs its
    /// own dispatch entry point. Returns `true` when the panel
    /// is active AND the mouse is inside its inner rect (so the
    /// caller knows the wheel was consumed and shouldn't fall
    /// through to buffer scrolling).
    pub(super) fn handle_floating_widget_panel_wheel(
        &mut self,
        slot: super::PanelSlot,
        col: u16,
        row: u16,
        delta: i32,
    ) -> bool {
        let inner = match self.panel(slot) {
            Some(fwp) => match fwp.last_inner_rect {
                Some(rect) => rect,
                None => return false,
            },
            None => return false,
        };
        if col < inner.x || col >= inner.x + inner.width {
            return false;
        }
        if row < inner.y || row >= inner.y + inner.height {
            return false;
        }
        // Panel-relative pointer position, so the wheel scrolls the
        // List/Tree under it rather than the first one in the spec.
        // Floating panels paint their entries from row 0 at `inner`,
        // so the translation is a plain offset.
        let pos = (u32::from(row - inner.y), u32::from(col - inner.x));
        let scrolled = self.handle_widget_panel_wheel_at(slot.buffer_id(), Some(pos), delta);
        // The non-modal dock must swallow the wheel whenever the pointer
        // is over it, even when the list is too short to scroll — the
        // scroll must never leak through to the active window beneath.
        let is_dock = matches!(
            self.panel(slot).map(|f| f.placement),
            Some(super::PanelPlacement::LeftDock { .. })
        );
        scrolled || is_dock
    }

    /// Route a vertical wheel to a widget panel mounted into an editor
    /// split (Settings, Search & Replace, the code-tour dock). Resolves
    /// the split under the pointer, translates the screen position into
    /// the panel's (buffer row, display column), and hands it to
    /// [`handle_widget_panel_wheel_at`](Self::handle_widget_panel_wheel_at)
    /// so the wheel scrolls the list the pointer is actually over —
    /// not the first list in the spec. Returns `true` when a panel
    /// consumed the scroll.
    pub(super) fn handle_split_widget_panel_wheel(
        &mut self,
        col: u16,
        row: u16,
        delta: i32,
    ) -> bool {
        // The pane the pointer is over, counting its scrollbar column — the
        // wheel scrolls a panel whose bar the pointer is on. `split_at_position`
        // answered this by scanning the two rectangles it recorded per pane.
        let Some(split_id) = self.pane_at(col, row) else {
            return false;
        };
        let Some(buffer_id) = self.active_window().pane_buffer(split_id) else {
            return false;
        };
        if self.widget_registry.panels_for_buffer(buffer_id).is_empty() {
            return false;
        }
        let content_rect = self.pane_content_rect(split_id);
        let pos = content_rect.and_then(|rect| {
            if !in_rect(col, row, rect) {
                return None;
            }
            // Buffer row = viewport top line + rows below the content
            // origin. Panels render one entry per line (no soft wrap)
            // and are normally pinned to the top, but honour a scrolled
            // viewport all the same.
            let top_byte = self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.buffers.splits())
                .map(|(_, vs)| vs)
                .and_then(|vs| vs.get(&split_id))
                .map(|vs| vs.viewport.top_byte())
                .unwrap_or(0);
            let top_line = self
                .buffers()
                .get(&buffer_id)
                .map(|s| s.buffer.get_line_number(top_byte))
                .unwrap_or(0);
            let gutter = self
                .buffers()
                .get(&buffer_id)
                .map(|s| s.margins.left_total_width() as u16)
                .unwrap_or(0);
            let panel_row = u32::from(row - rect.y).saturating_add(top_line as u32);
            let panel_col = u32::from(col.saturating_sub(rect.x).saturating_sub(gutter));
            Some((panel_row, panel_col))
        });
        self.handle_widget_panel_wheel_at(buffer_id, pos, delta)
    }

    /// Extend an armed widget-text drag selection to the pointer.
    ///
    /// Translates the screen position into the document's (rendered
    /// line, byte-in-line) through the widget's recorded scroll region
    /// — the same geometry wheel routing hit-tests — then hands the
    /// caret move to the runtime. Rows above/below the region clamp to
    /// its edges so a drag that overshoots keeps selecting.
    pub(super) fn handle_widget_text_selection_drag(&mut self, col: u16, row: u16) {
        use crate::primitives::display_width::grapheme_byte_at_visual_column;
        let Some(drag) = self.widget_text_drag.clone() else {
            return;
        };
        let Some(panel) = self.widget_registry.get(&drag.panel) else {
            return;
        };
        let buffer_id = panel.buffer_id;
        let Some(region) = panel
            .boxes
            .iter()
            .find(|b| b.scroll.is_some() && b.key.as_deref() == Some(drag.widget.as_str()))
            .cloned()
        else {
            return;
        };
        let Some(rect) = self
            .active_layout()
            .split_areas
            .iter()
            .find(|(_, bid, ..)| *bid == buffer_id)
            .map(|(_, _, rect, ..)| *rect)
        else {
            return;
        };
        let (top_line, gutter) = self
            .buffers()
            .get(&buffer_id)
            .map(|s| (0usize, s.margins.left_total_width() as u16))
            .unwrap_or((0, 0));
        // Buffer row under the pointer, clamped into the region's row
        // band (dragging past either edge selects to the visible edge).
        let Some(sc) = region.scroll else { return };
        let brow = top_line + usize::from(row.max(rect.y) - rect.y);
        let rel_row = brow
            .saturating_sub(region.row as usize)
            .min(region.height.saturating_sub(1) as usize);
        let line = (sc.offset + rel_row).min(sc.total.saturating_sub(1));
        // Byte within the rendered line, from the pointer's display
        // column within the widget's region.
        let widget_col = usize::from(col.saturating_sub(rect.x).saturating_sub(gutter))
            .saturating_sub(region.col as usize);
        let line_text = self
            .widget_registry
            .get(&drag.panel)
            .and_then(|p| match p.instance_states.get(&drag.widget) {
                Some(crate::widgets::WidgetInstanceState::Text { editor, .. }) => Some(
                    editor
                        .value()
                        .split('\n')
                        .nth(line)
                        .unwrap_or_default()
                        .to_string(),
                ),
                _ => None,
            })
            .unwrap_or_default();
        let byte_in_line = grapheme_byte_at_visual_column(&line_text, widget_col);
        self.extend_widget_text_selection_to(&drag, line, byte_in_line);
    }

    /// Try to start a floating-panel list scrollbar drag. Returns
    /// true if the press landed on a scrollbar track (so the caller
    /// skips row hit-testing — the bar overlaps the list's rightmost
    /// column). Reuses the canonical `ScrollbarMouse`/`ScrollbarState`.
    fn try_widget_scrollbar_press(&mut self, slot: super::PanelSlot, col: u16, row: u16) -> bool {
        use crate::view::ui::scrollbar::ScrollbarState;
        let (panel_key, tracks) = match self.panel(slot) {
            Some(fwp) => (fwp.panel_key.clone(), fwp.scrollbar_tracks.clone()),
            None => return false,
        };
        for t in &tracks {
            let state = ScrollbarState::new(t.total, t.visible, t.scroll);
            let pressed = self
                .panel_mut(slot)
                .and_then(|fwp| fwp.scrollbar_mouse.press(state, t.rect, col, row));
            if let Some(new_offset) = pressed {
                if let Some(fwp) = self.panel_mut(slot) {
                    fwp.scrollbar_drag_key = Some(t.list_key.clone());
                }
                self.apply_widget_scroll(&panel_key, &t.list_key, new_offset, t.visible);
                return true;
            }
        }
        false
    }

    /// Continue an in-flight floating-panel scrollbar drag. Returns
    /// true if a drag is active (the press captured a `list_key`).
    pub(super) fn try_widget_scrollbar_drag(&mut self, slot: super::PanelSlot, row: u16) -> bool {
        use crate::view::ui::scrollbar::ScrollbarState;
        let (panel_key, key) = match self.panel(slot) {
            Some(fwp) => match &fwp.scrollbar_drag_key {
                Some(k) => (fwp.panel_key.clone(), k.clone()),
                None => return false,
            },
            None => return false,
        };
        // The track geometry for the dragged list (its rect may have
        // shifted if the panel re-rendered between events).
        let track = self.panel(slot).and_then(|fwp| {
            fwp.scrollbar_tracks
                .iter()
                .find(|t| t.list_key == key)
                .cloned()
        });
        let Some(t) = track else {
            return false;
        };
        let state = ScrollbarState::new(t.total, t.visible, t.scroll);
        let new_offset = self
            .panel_mut(slot)
            .and_then(|fwp| fwp.scrollbar_mouse.drag(state, t.rect, row));
        if let Some(off) = new_offset {
            self.apply_widget_scroll(&panel_key, &key, off, t.visible);
        }
        true
    }

    /// Try to start a drag on a scrollbar painted over a *buffer-mounted*
    /// widget panel (the review-diff sidebar, Search & Replace). Returns
    /// true when the press landed on a track, so the caller skips the
    /// click it would otherwise have delivered to the panel underneath.
    ///
    /// The floating-panel twin is [`Self::try_widget_scrollbar_press`];
    /// the difference is only where the tracks live (on the editor here,
    /// on the panel struct there).
    pub(super) fn try_split_widget_scrollbar_press(&mut self, col: u16, row: u16) -> bool {
        use crate::view::ui::scrollbar::ScrollbarState;
        // Only tracks belonging to a keyed List/Tree: those are the ones
        // `apply_widget_scroll` can move. A press claimed for anything else
        // (a keyless box, an overflowing multi-line Text) would scroll
        // nothing while still swallowing the click the panel underneath
        // was owed.
        let Some((panel_key, track)) = self
            .split_widget_scrollbar_tracks
            .iter()
            .find(|(panel_key, t)| {
                crate::view::ui::point_in_rect(t.rect, col, row)
                    && self
                        .widget_registry
                        .buffer_and_spec_ref(panel_key)
                        .is_some_and(|(_, spec)| {
                            crate::widgets::find_widget_by_key(spec, &t.list_key).is_some_and(|w| {
                                matches!(
                                    w,
                                    fresh_core::api::WidgetSpec::List { .. }
                                        | fresh_core::api::WidgetSpec::Tree { .. }
                                )
                            })
                        })
            })
            .map(|(p, t)| (p.clone(), t.clone()))
        else {
            return false;
        };
        let state = ScrollbarState::new(track.total, track.visible, track.scroll);
        let Some(new_offset) = self
            .split_widget_scrollbar_mouse
            .press(state, track.rect, col, row)
        else {
            return false;
        };
        self.split_widget_scrollbar_drag = Some((panel_key.clone(), track.list_key.clone()));
        self.apply_widget_scroll(&panel_key, &track.list_key, new_offset, track.visible);
        true
    }

    /// Continue an in-flight buffer-mounted scrollbar drag. Returns true
    /// while one is active.
    pub(super) fn try_split_widget_scrollbar_drag(&mut self, row: u16) -> bool {
        use crate::view::ui::scrollbar::ScrollbarState;
        let Some((panel_key, list_key)) = self.split_widget_scrollbar_drag.clone() else {
            return false;
        };
        // Re-read the track: the panel re-renders as it scrolls, so its
        // recorded geometry is the one from the latest draw.
        let Some(track) = self
            .split_widget_scrollbar_tracks
            .iter()
            .find(|(p, t)| *p == panel_key && t.list_key == list_key)
            .map(|(_, t)| t.clone())
        else {
            return true;
        };
        let state = ScrollbarState::new(track.total, track.visible, track.scroll);
        if let Some(off) = self
            .split_widget_scrollbar_mouse
            .drag(state, track.rect, row)
        {
            self.apply_widget_scroll(&panel_key, &list_key, off, track.visible);
        }
        true
    }

    /// End any in-flight buffer-mounted scrollbar drag.
    pub(super) fn release_split_widget_scrollbar(&mut self) {
        self.split_widget_scrollbar_mouse.release();
        self.split_widget_scrollbar_drag = None;
    }

    /// End any in-flight floating-panel scrollbar drag.
    pub(super) fn release_widget_scrollbar(&mut self) {
        for fwp in [self.dock.as_mut(), self.floating_widget_panel.as_mut()]
            .into_iter()
            .flatten()
        {
            fwp.scrollbar_mouse.release();
            fwp.scrollbar_drag_key = None;
        }
    }

    /// Apply a host-driven scroll to a panel list (scrollbar press /
    /// drag): update the registry's instance state, re-render, and —
    /// when the list has a live selection that moved into the new
    /// window — notify the plugin so its own selection mirror +
    /// preview stay in sync with the thumb.
    fn apply_widget_scroll(
        &mut self,
        panel_key: &crate::widgets::PanelKey,
        list_key: &str,
        new_offset: usize,
        visible: usize,
    ) {
        let moved_sel = self.widget_registry.set_list_scroll(
            panel_key,
            list_key,
            new_offset as u32,
            visible as u32,
        );
        self.rerender_widget_panel(panel_key);
        if let Some(sel) = moved_sel {
            self.fire_widget_event(
                panel_key,
                list_key.to_string(),
                "select".to_string(),
                serde_json::json!({ "index": sel as i64 }),
            );
        }
    }

    /// Right-click hit-test against a floating widget panel. Resolves the
    /// cell under the cursor to a widget and — only when it lands on a
    /// `list` row — fires a `widget_event` with `event_type: "context"`
    /// (carrying the same `{ index, key, list_key }` payload a left-click
    /// "select" would). Plugins use this to raise a context menu for the
    /// right-clicked row. Returns `true` when a context event fired (so the
    /// caller swallows the click). Clicks on non-list widgets, padding, or
    /// outside the inner rect return `false`.
    pub(super) fn handle_floating_widget_context_click(
        &mut self,
        slot: super::PanelSlot,
        col: u16,
        row: u16,
    ) -> bool {
        let panel_key = match self.panel(slot) {
            Some(fwp) => fwp.panel_key.clone(),
            None => return false,
        };
        // One probe for every pointer gesture on a panel — the same
        // geometry, surface decision (base vs covering popup), and
        // row-aware resolution the left-click and hover paths use.
        // This path used to duplicate the geometry inline WITHOUT the
        // overlay check, so a right-click went through an open popup
        // to the rows it covered.
        let probe = match self.probe_floating_widget(slot, col, row) {
            Some(p) => p,
            None => return false,
        };
        // Keep only hits whose kind declared the context-click
        // capability (List/Tree rows): a right-click raises a menu for
        // a session row, not for a button or empty padding.
        let (mut payload, key, _kind) = match probe.hit.filter(|hit| hit.context_click) {
            Some(hit) => (hit.payload.clone(), hit.widget_key.clone(), hit.widget_kind),
            None => return false,
        };
        // Carry the screen cell so the plugin can anchor its popup at the
        // click (the list `select` payload only has the row index).
        if let Some(obj) = payload.as_object_mut() {
            obj.insert("col".to_string(), serde_json::json!(col));
            obj.insert("row".to_string(), serde_json::json!(row));
        }
        if !self
            .plugin_manager
            .read()
            .unwrap()
            .has_hook_handlers("widget_event")
        {
            return false;
        }
        self.fire_widget_event(&panel_key, key, "context".to_string(), payload);
        true
    }

    /// True when the centered (`Floating`) slot currently holds an
    /// anchored context-menu popup rather than a centered modal.
    pub(super) fn floating_panel_is_anchored(&self) -> bool {
        matches!(
            self.floating_widget_panel.as_ref().map(|f| f.placement),
            Some(super::PanelPlacement::Anchored { .. })
        )
    }

    /// True when `(col, row)` falls within the panel's drawn box — the
    /// last-rendered inner rect grown by its 1-cell border. False when the
    /// panel or its rect is absent.
    pub(super) fn point_in_floating_panel(
        &self,
        slot: super::PanelSlot,
        col: u16,
        row: u16,
    ) -> bool {
        let Some(inner) = self.panel(slot).and_then(|f| f.last_inner_rect) else {
            return false;
        };
        let x0 = inner.x.saturating_sub(1);
        let y0 = inner.y.saturating_sub(1);
        // inner.{x,y} + {width,height} already lands on the far border cell.
        col >= x0 && col <= inner.x + inner.width && row >= y0 && row <= inner.y + inner.height
    }

    /// Unmount the floating panel and fire a `cancel` widget_event so the
    /// owning plugin clears its state — the click-outside analogue of the
    /// Esc dismissal in `dispatch_floating_widget_key`.
    pub(super) fn dismiss_floating_panel_with_cancel(&mut self, slot: super::PanelSlot) {
        let panel_key = match self.panel(slot) {
            Some(f) => f.panel_key.clone(),
            None => return,
        };
        let widget_key = self
            .widget_registry
            .get(&panel_key)
            .map(|p| p.focus_key.clone())
            .unwrap_or_default();
        self.fire_widget_event(
            &panel_key,
            widget_key,
            "cancel".to_string(),
            serde_json::json!({}),
        );
        *self.panel_opt_mut(slot) = None;
        let _ = self.widget_registry.unmount(&panel_key);
    }

    /// `handle_editor_click` uses; clicks outside the rect are
    /// swallowed without dismissing the panel.
    /// Resolve a click against the open dropdown pop-over's screen rects
    /// (recorded by the last draw). A click on an option row delivers the
    /// same `dropdown_select` hit a TUI cell click on the old inline list
    /// would; a click elsewhere inside the box (border) is swallowed so it
    /// neither selects nor dismisses the modal. Returns true when the click
    /// was inside the pop-over box (and thus consumed).
    fn try_panel_popup_click(&mut self, slot: super::PanelSlot, col: u16, row: u16) -> bool {
        let (panel_key, key, hits, popup_rect) = match self.panel(slot) {
            Some(f) => (
                f.panel_key.clone(),
                f.popup.as_ref().map(|d| d.widget_key.clone()),
                f.popup_hits.clone(),
                f.popup_rect,
            ),
            None => return false,
        };
        let popup_rect = match popup_rect {
            Some(r) => r,
            None => return false,
        };
        let key = match key {
            Some(k) if !k.is_empty() => k,
            _ => return false,
        };
        // Option row → select that index (fires `change`) and close.
        if let Some(hit) = hits.iter().find(|h| in_rect(col, row, h.rect)) {
            let ha = crate::widgets::HitArea {
                overlay: false,
                row_target: false,
                context_click: false,
                widget_key: key,
                widget_kind: "dropdown",
                buffer_row: 0,
                byte_start: 0,
                byte_end: 0,
                payload: serde_json::json!({ "index": hit.index }),
                event_type: "dropdown_select",
                owner_key: None,
            };
            self.deliver_widget_hit(&panel_key, &ha, None);
            return true;
        }
        // Inside the box but not on a row (its border): consume so the
        // modal isn't dismissed and the list stays open.
        in_rect(col, row, popup_rect)
    }

    /// Resolve a screen cell against a mounted floating panel: the
    /// panel-local row and byte column it maps to, plus the widget hit
    /// there (`None` when the cell is over no widget).
    ///
    /// Returns `None` when the panel isn't mounted, hasn't been drawn yet,
    /// or the cell is outside its inner rect — i.e. "not this panel's cell"
    /// as distinct from "this panel's cell, no widget on it".
    ///
    /// Shared by the click path and the hover tracker so the two can never
    /// disagree about what the pointer is on. They did once for left- vs
    /// right-click (byte-exact vs row-wide resolution), and compact dock
    /// rows silently swallowed clicks past their label for it.
    fn probe_floating_widget(
        &self,
        slot: super::PanelSlot,
        col: u16,
        row: u16,
    ) -> Option<FloatingWidgetProbe> {
        let inner = self.panel(slot)?.last_inner_rect?;
        if col < inner.x || col >= inner.x + inner.width {
            return None;
        }
        if row < inner.y || row >= inner.y + inner.height {
            return None;
        }
        let brow = (row - inner.y) as u32;
        let local_screen_col = (col - inner.x) as usize;
        // Which surface is the pointer on? The panel's layout-box tree
        // answers: a z>0 box — an Overlay-promoted subtree (the dock's
        // "New Task… ▾" / "Move to Folder…" dropdowns) or the Text
        // completion popup — covers the base rows beneath it. This used
        // to be re-derived by scanning the painted overlay rows; the
        // box tree is the same fact, stated structurally.
        // Resolve the boxes through the slot's own panel key — never a
        // `panels_for_buffer(..).first()` pick, whose HashMap order is
        // arbitrary and could name a different panel than the one whose
        // overlays/entries the text lookup below reads.
        let slot_panel_key = self.panel(slot)?.panel_key.clone();
        let on_overlay = self.widget_registry.get(&slot_panel_key).is_some_and(|p| {
            // The covering surface is named by `pointer_opaque` on
            // the hit path — the popup boxes (overlay promotion,
            // completion list) carry it. z alone is not enough: a
            // future non-opaque cover (a pass-through tooltip)
            // must NOT capture the pointer.
            crate::widgets::layout_box::hit_path(&p.boxes, brow, local_screen_col as u32)
                .iter()
                .any(|&i| p.boxes[i].pointer_opaque && p.boxes[i].z > 0)
        });
        // The column maps to a byte offset through the text of the
        // surface the pointer is on — a covered row is DRAWN from the
        // overlay's text, and the overlay's hit areas were measured
        // against it. Mapping through the row underneath yields a byte
        // offset in a different string, which is why the dropdown
        // options were once unclickable.
        let panel = self.panel(slot)?;
        let row_text = if on_overlay {
            panel
                .overlays
                .iter()
                .find(|o| o.buffer_row == brow)
                .map(|o| o.entry.text.as_str())?
        } else {
            panel.entries.get(brow as usize).map(|e| e.text.as_str())?
        };
        let bcol = crate::primitives::display_width::grapheme_byte_at_visual_column(
            row_text,
            local_screen_col,
        );
        // Row-aware resolution on the base surface (a click past a
        // compact row's text still lands on the row); opaque popup
        // semantics on the overlay surface (only its own hits are
        // reachable, misses swallowed) — both inside
        // `hit_test_row_aware`, keyed by the surface parameter.
        let hit = self.widget_registry.hit_test_row_aware(
            slot.buffer_id(),
            brow,
            bcol as u32,
            on_overlay,
        );
        Some(FloatingWidgetProbe {
            brow,
            bcol,
            hit: hit.map(|(_, h)| h.clone()),
        })
    }

    /// Track which widget the pointer is over, per mounted panel, and
    /// re-render a panel whose hovered widget changed.
    ///
    /// Purely host state: the tracked key feeds `RenderContext::hover_key`,
    /// which the renderer compares against each widget's own key. A hover
    /// therefore costs a hit-test and — only when the pointer crosses a
    /// widget boundary — one spec re-render. Nothing crosses the plugin
    /// bridge, so panels pay nothing for pointer movement over them.
    ///
    /// The re-render is needed because the draw pass paints the panel's
    /// cached entries; a highlight change has to go back through the
    /// renderer to appear.
    ///
    /// `pointer_owner` names the slot the pointer is actually addressing,
    /// for callers that already know: a centered modal captures the mouse
    /// channel outright, and the dock it covers must not keep (or gain) a
    /// highlight from a pointer that is really over the modal. `None` —
    /// the normal, non-modal pipeline — leaves every mounted panel
    /// reachable.
    pub(super) fn update_widget_hover(
        &mut self,
        col: u16,
        row: u16,
        pointer_owner: Option<super::PanelSlot>,
    ) -> bool {
        let mut changed = false;
        for slot in [super::PanelSlot::Dock, super::PanelSlot::Floating] {
            // A slot the pointer isn't addressing resolves to "nothing
            // hovered", which also *clears* whatever it had highlighted.
            // The hovered *row* travels beside the hovered widget: a
            // list/tree hit carries its item key in the payload, and every
            // row of one tree shares the tree's own `widget_key`, so
            // without it the renderer could only light the whole list.
            let (now, now_item) = if pointer_owner.is_none_or(|owner| owner == slot) {
                self.probe_floating_widget(slot, col, row)
                    .and_then(|p| p.hit)
                    .map(|h| {
                        let item = h
                            .payload
                            .get("key")
                            .and_then(|v| v.as_str())
                            .unwrap_or_default()
                            .to_string();
                        (h.widget_key, item)
                    })
                    .unwrap_or_default()
            } else {
                (String::new(), String::new())
            };
            let panel_key = match self.panel(slot) {
                Some(fwp) if fwp.hovered_widget_key != now || fwp.hovered_item_key != now_item => {
                    fwp.panel_key.clone()
                }
                _ => continue,
            };
            if let Some(fwp) = self.panel_mut(slot) {
                fwp.hovered_widget_key = now;
                fwp.hovered_item_key = now_item;
            }
            self.rerender_widget_panel(&panel_key);
            changed = true;
        }
        changed
    }

    pub(super) fn handle_floating_widget_click(
        &mut self,
        slot: super::PanelSlot,
        col: u16,
        row: u16,
    ) {
        // An open dropdown's option list floats as a screen-level pop-over
        // that extends PAST the panel/modal border, so a click on one of
        // its option rows lands outside the panel's inner rect and would be
        // dropped by the gate below. Resolve it first, against the screen
        // rects recorded at draw time.
        if self.try_panel_popup_click(slot, col, row) {
            return;
        }
        // Scrollbar press wins over row hit-testing (the bar overlaps
        // the list's rightmost column).
        if self.try_widget_scrollbar_press(slot, col, row) {
            return;
        }
        let panel_key = match self.panel(slot) {
            Some(fwp) => fwp.panel_key.clone(),
            None => return,
        };
        let probe = match self.probe_floating_widget(slot, col, row) {
            Some(p) => p,
            None => return,
        };
        let (brow, bcol) = (probe.brow, probe.bcol);
        let Some(hit) = probe.hit else {
            tracing::debug!(
                target: "fresh::dock",
                ?slot, col, row, brow, bcol,
                "handle_floating_widget_click: hit_test found no widget"
            );
            return;
        };
        tracing::debug!(
            target: "fresh::dock",
            hit_key = %hit.widget_key,
            hit_kind = hit.widget_kind,
            hit_event = %hit.event_type,
            "handle_floating_widget_click: hit"
        );
        // The shared pointer dispatch — identical to a buffer-cell or
        // native-frontend click: focus the hit's owner, run the kind's
        // own `on_pointer`, place a text caret from the clicked byte
        // column (`bcol`), fire the recorded event unless consumed.
        // This TUI-native path used to hand-copy a subset of that
        // ladder and drifted (no list-selection sync, no dual-list
        // cursor move); delegating is what keeps the three frontends
        // behaving identically.
        self.deliver_widget_hit(&panel_key, &hit, Some(bcol));
    }
}
