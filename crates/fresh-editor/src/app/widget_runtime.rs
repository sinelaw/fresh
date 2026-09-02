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
    #[allow(clippy::too_many_arguments)]
    pub(super) fn render_panel_spec(
        &self,
        spec: &fresh_core::api::WidgetSpec,
        prev: &std::collections::HashMap<String, crate::widgets::WidgetInstanceState>,
        prev_painted: &std::collections::HashMap<String, crate::widgets::PaintedWindow>,
        prev_focus_key: &str,
        panel_width: u32,
        avail_height: Option<u32>,
        auto_focus_first: bool,
    ) -> crate::widgets::RenderOutput {
        let theme_guard = self.theme.read().unwrap();
        crate::widgets::render_spec_with_options(
            spec,
            prev,
            panel_width,
            crate::widgets::RenderOptions {
                prev_focus_key,
                // The panel's own policy: see `WidgetPanelOptions`. A
                // panel that says "nothing focused" is a real state
                // must not have that answer overwritten on every
                // repaint.
                auto_focus_first,
                markdown: Some(crate::widgets::MarkdownCtx {
                    theme: &theme_guard,
                    grammars: Some(self.grammar_registry.as_ref()),
                }),
                // Auto-size budget for `visible_rows: None` lists/trees:
                // the viewport height of the split currently showing the
                // panel's buffer (None when it isn't on screen — widgets
                // then keep the legacy fallback until it is).
                avail_height,
                // The windows the last paint left: a scroll offset folds
                // over its own previous value, so a repaint that did not
                // carry this would start every list back at the top.
                prev_painted: Some(prev_painted),
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
    prev_painted: &std::collections::HashMap<String, crate::widgets::PaintedWindow>,
    prev_focus_key: &str,
    panel_width: u32,
    avail_height: Option<u32>,
    hover_key: &str,
    hover_item_key: &str,
    hover_popup_row: &str,
    markdown: Option<crate::widgets::MarkdownCtx<'_>>,
    auto_focus_first: bool,
) -> crate::widgets::RenderOutput {
    crate::widgets::render_spec_with_options(
        spec,
        prev,
        panel_width,
        crate::widgets::RenderOptions {
            prev_focus_key,
            hover_key,
            hover_item_key,
            hover_popup_row,
            marker_gutter: focus_marker,
            // The panel's own policy — see `WidgetPanelOptions`. This
            // is the path a focus change, a hover change and every
            // host-driven refresh re-render through, mounted panels
            // included, so seeding here unconditionally undid a panel's
            // "nothing focused" the moment anything touched it.
            auto_focus_first,
            markdown,
            avail_height,
            prev_painted: Some(prev_painted),
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

/// Whether `spec` contains a **markdown document view** — a multi-row `Text`
/// with `markdown: true`.
///
/// The one widget a described panel does not fully describe. Its rows come
/// from `render_collected` called *inside* the description build (§6e's
/// remaining double render, whose replacement is S8's wrapped viewport), and
/// two host paths still read the box arena that produces them:
/// `Editor::handle_widget_text_selection_drag`, which is what makes the
/// prose drag-selectable, and `Text::on_wheel`'s document branch. Both go
/// silently dead against an empty arena, so a panel holding one keeps the
/// collector.
fn spec_has_markdown_document(spec: &fresh_core::api::WidgetSpec) -> bool {
    use fresh_core::api::WidgetSpec;
    if matches!(
        spec,
        WidgetSpec::Text {
            markdown: true,
            rows,
            ..
        } if *rows > 1
    ) {
        return true;
    }
    spec.children().any(spec_has_markdown_document)
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
    /// Process a resolved widget press (from a TUI cell click, a floating
    /// panel click, or a plugin's own `WidgetAction`): move focus to the event's
    /// OWNING widget, run the owner kind's own pointer handler
    /// ([`crate::widgets::kinds::WidgetImpl::on_pointer`] — tree
    /// expansion, list/tree selection, dropdown open/commit, dual-list
    /// cursors all live with their kinds), apply the effects it
    /// requested, and — unless the kind consumed the press — fire the
    /// event tagged `via: "click"`. This is the single dispatch
    /// path shared by every frontend, so a click delivers identical
    /// behaviour in all of them. No per-kind decision happens here.
    ///
    /// **`clicked_byte` is measured from the start of the widget's own
    /// rendered row**, which is the space the `focus` event's
    /// `valueInnerStart` breadcrumb is in. A described widget is its own node,
    /// so the byte the library reports for a press on it is already that; a
    /// caller resolving through the text projection's rows
    /// ([`crate::widgets::WidgetRegistry::hit_test_row_aware`]) holds a byte
    /// in a *composed* row and rebases it by the matched
    /// [`HitArea::byte_start`](crate::widgets::HitArea::byte_start) before
    /// calling. Rebasing here instead is what made the described path add
    /// `byte_start` on and this function take it straight back off.
    ///
    /// `None` when the press carries no byte at all — the web's by-index
    /// route, and a keyboard activation.
    pub(crate) fn deliver_widget_hit(
        &mut self,
        panel_key: &crate::widgets::PanelKey,
        hit: &crate::widgets::WidgetEvent,
        clicked_byte: Option<usize>,
    ) {
        use crate::widgets::kinds::{PointerDisposition, PointerFx};
        let owner = hit.owner().to_string();
        // Click-to-focus: if the owning widget can hold focus, move focus
        // there before anything else so the next render reflects it. (A List
        // row's owner is the List itself — a row click focuses the list, and
        // arrows right after it keep moving the list's selection.)
        //
        // **Asked of the spec, not of a recorded ring.** This read
        // `WidgetPanelState::tabbable`, which is the collector's ring as of
        // whatever render ran last; the spec in hand may have moved since. The
        // question is a property of the widget under the pointer, so it is put
        // to that widget — `kinds::focusable_key` is the same predicate the
        // tree's ring admits by, so a click can never focus something Tab
        // cannot reach, or fail to focus something it can.
        if !owner.is_empty() {
            let focusable = self
                .widget_registry
                .get(panel_key)
                .and_then(|p| crate::widgets::find_widget_by_key(&p.spec, &owner))
                .and_then(crate::widgets::kinds::focusable_key)
                .is_some();
            if focusable {
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
                        byte,
                    );
                } else {
                    self.reposition_widget_text_cursor_from_click(
                        panel_key,
                        &hit.widget_key,
                        byte,
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

    // **The three native-frontend entry points are deleted with the web's
    // plugin panels.** `deliver_widget_hit_by_index`, `deliver_widget_hit_
    // semantic` (with `synthesize_list_hit` / `synthesize_tree_hit` /
    // `synthesize_control_hit` behind it) and `set_widget_text_cursor` all
    // resolved an interaction the *browser* had laid out: an index or an
    // identity into `WidgetPanelState::hits`, or a byte the browser measured.
    // The scene stopped shipping a plugin panel to the browser, so nothing
    // can send one back, and `hits` stopped having a described-panel reader
    // with them. See `docs/internal/fresh-editor-retained-mode-plan.md`.
    //
    // The synthesizers are the loss worth naming: they were the derivation of
    // `HitArea`'s identity half from `(spec, instance state)` — the shape §4.1
    // of the end-state doc wants everything to use — and they are the only
    // written instance of it. Bringing the web back should re-derive from the
    // display list rather than restore them.

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
            // A composed buffer is painted into a narrower column than
            // its split: `compose_width` is what the renderer clips to,
            // so it is also what the widget layout has to size rows to.
            // Laying out against the split width instead let the two
            // disagree, and every consequence of that disagreement
            // looked like a widget bug rather than a width bug — a
            // `flexSpacer` filled to the split and pushed its row past
            // the column, so the host wrapped a centred row in half,
            // and a `divider` ruled across the pane instead of the
            // page. Plugins were left computing their own pads from a
            // width the host already knew.
            .map(|vs| (vs.compose_width, vs.viewport.width))
            .unwrap_or((None, self.terminal_width.max(1) as u16));
        match raw {
            // Composing, the compose layout has already flanked the
            // column with real margins, so the panel *is* the column.
            // Taking the full gutter here as well left two unused
            // columns inside the render area, and because both fell on
            // its right the page rode left of the column it was
            // supposedly centred in — measured on the welcome screen at
            // 100, 120 and 150 columns, a left margin four short of the
            // right one. One column is still held back: a row that
            // fills the area exactly wraps.
            (Some(cw), _) => (cw as u32).saturating_sub(1).max(10),
            // Not composing: reserve 2 cols for gutter/scrollbar/border.
            // Saturate to avoid 0 width on tiny panels.
            (None, vw) => (vw as u32).saturating_sub(2).max(10),
        }
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
        self.pane_content_rect_for_buffer(buffer_id)
            .map(|content_rect| content_rect.height as u32)
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
    /// the registry, resolves it against the (possibly updated) prev
    /// state / focus key, and writes the result back.
    ///
    /// **Two ways of doing that, and which one is right is whether the tree
    /// laid this panel out.** A described panel needs the three walks of
    /// [`Self::resolve_described_panel`] and nothing else; a painted one
    /// needs the whole text projection, which is what `render_floating_spec`
    /// below produces.
    pub(super) fn rerender_widget_panel(&mut self, panel_key: &crate::widgets::PanelKey) {
        if self.resolve_described_panel(panel_key) {
            return;
        }
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
            let prev_painted = self
                .widget_registry
                .get(panel_key)
                .map(|p| p.painted.clone())
                .unwrap_or_default();
            let prev_focus = self
                .widget_registry
                .focus_key(panel_key)
                .map(|s| s.to_string())
                .unwrap_or_default();
            let auto_focus_first = self
                .widget_registry
                .get(panel_key)
                .map(|p| p.auto_focus_first)
                .unwrap_or(true);
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
            // A buffer-mounted panel has no `FloatingWidgetPanel` to
            // carry this, so it keeps its hover on its registry state.
            let (hover_key, hover_item_key) = match panel_slot {
                Some(slot) => self
                    .panel(slot)
                    .map(|f| (f.hovered_widget_key.clone(), f.hovered_item_key.clone()))
                    .unwrap_or_default(),
                None => self.widget_registry.hover_keys(panel_key),
            };
            // The popup row is a floating-panel affordance; a mounted panel
            // drops the popup channel at mount, so there is none to carry.
            let hover_popup_row = panel_slot
                .and_then(|slot| self.panel(slot))
                .map(|f| f.hovered_popup_row.clone())
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
                &prev_painted,
                &prev_focus,
                panel_width,
                avail_height,
                &hover_key,
                &hover_item_key,
                &hover_popup_row,
                Some(crate::widgets::MarkdownCtx {
                    theme: &theme_guard,
                    grammars: Some(self.grammar_registry.as_ref()),
                }),
                auto_focus_first,
            );
            (buffer_id, is_floating, panel_width, out)
        };
        let _ = panel_width;
        self.record_widget_panel_render_height(panel_key, rendered_height);
        let panel_slot = Self::slot_for_panel_buffer(buffer_id);
        let focus_cursor = out_pieces.focus_cursor;
        let entries = out_pieces.entries;
        if self
            .widget_registry
            .update_side_effects(
                panel_key,
                out_pieces.hits,
                out_pieces.instance_states,
                out_pieces.focus_key,
                out_pieces.tabbable,
                out_pieces.painted,
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
                }
            }
            return;
        }
        if let Err(e) = self.set_virtual_buffer_content(buffer_id, entries.clone()) {
            tracing::error!("rerender_widget_panel({}) failed: {}", panel_key, e);
        }
        self.apply_widget_focus_cursor(buffer_id, &entries, focus_cursor);
    }

    /// Re-resolve a **described** panel, and report that the collector did
    /// not need to run.
    ///
    /// **The text projection has no reader for such a panel, so it is not
    /// produced.** Its rows are the tree's nodes, its hit areas are those
    /// nodes' own presses, its box arena answers no wheel
    /// ([`Self::handle_widget_panel_wheel_at`] declines a described panel and
    /// says why), and its painted windows are superseded by the viewport's
    /// (`Editor::widget_viewport` asks the tree first). What is left is
    /// [`crate::widgets::resolve_panel`]'s three: the state carry, the focus
    /// clamp, and the ring — none of which needs a row, a width or a height.
    ///
    /// **The one exception is stated rather than hidden.** An *anchored*
    /// floating panel — a plugin's right-click context menu — still takes its
    /// width from the mirror's widest row, because its interior is built by a
    /// `layout_reader` that needs a number before it can produce one and
    /// `Sizing::Auto` there would hand it the whole screen
    /// (`view::shell::panel::Panel::anchored_width` argues this at length, and
    /// names the change that retires it). That panel keeps the collector, and
    /// this returns `false` for it.
    ///
    /// A panel that becomes anchored *after* a host-driven re-render keeps the
    /// rows from the last render that produced any, which is the placement
    /// call's own ordering: `FloatingPanelControl("anchor")` follows the
    /// update that emitted the menu's spec, and that update runs the collector
    /// whatever the placement is. Stale rows are the right failure mode there
    /// — clearing them would size the pop-over to its six-column minimum
    /// instead of to a spec one frame old.
    ///
    /// **The second exception is the markdown document view**, and it is the
    /// same one §6e names: a described panel holding one still runs
    /// `render_collected` inside its own description, and two host paths read
    /// the box arena that walk produces — the drag-to-select on the prose
    /// (`Self::handle_widget_text_selection_drag`) and `Text::on_wheel`'s
    /// document branch. Neither errors against an empty arena; each simply
    /// stops working. See [`spec_has_markdown_document`].
    ///
    /// Returns `false` for a panel the tree does not describe, which then
    /// renders exactly as before.
    fn resolve_described_panel(&mut self, panel_key: &crate::widgets::PanelKey) -> bool {
        if !self.panel_is_the_trees(panel_key) {
            return false;
        }
        let slot = self.slot_of_panel(panel_key);
        if matches!(slot, Some(super::PanelSlot::Floating))
            && matches!(
                self.panel(super::PanelSlot::Floating).map(|p| p.placement),
                Some(super::PanelPlacement::Anchored { .. })
            )
        {
            return false;
        }
        let Some(state) = self.widget_registry.get(panel_key) else {
            return false;
        };
        if spec_has_markdown_document(&state.spec) {
            return false;
        }
        let out = crate::widgets::resolve_panel(
            &state.spec,
            &state.instance_states,
            &state.focus_key,
            // The panel's own policy, not a literal: this is the fourth
            // path that can re-resolve focus, and it is the one the
            // migration routes described panels through. Seeding here
            // discarded `autoFocusFirst: false` on every repaint of
            // exactly the panels the tree has already taken over.
            state.auto_focus_first,
        );
        // The row budget this panel was resolved against, for the resize
        // bookkeeping that decides when a pane-mounted panel has to be
        // re-rendered. A described panel auto-sizes in layout, so the number
        // no longer decides a window — but the record has to stay truthful or
        // `widget_panels_with_stale_height` reports the same panel forever.
        let avail_height = match slot {
            Some(slot) => self.floating_panel_inner_height(slot),
            None => self.widget_panel_height(state.buffer_id),
        };
        self.record_widget_panel_render_height(panel_key, avail_height);
        if self
            .widget_registry
            .update_side_effects(
                panel_key,
                Vec::new(),
                out.instance_states,
                out.focus_key,
                out.tabbable,
                std::collections::HashMap::new(),
                Vec::new(),
            )
            .is_none()
        {
            tracing::warn!("resolve_described_panel({}) lost panel mid-call", panel_key);
        }
        true
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

    /// **The window a widget's key and wheel handlers act inside.**
    ///
    /// One resolver, host-side, because "how big is this widget's window"
    /// is not something a kind can answer: it is a fact about a *layout*.
    /// Two layouts can answer it, and they are asked in the order of who
    /// actually laid the widget out.
    ///
    /// **The tree first**, through [`Self::described_widget_viewport`] — for
    /// a described panel the widget is a node the reconciler placed, and the
    /// viewport under it published its window during that layout. The paint's
    /// number for the same widget is the collector's separate resolution of
    /// the same question, and where the spec names no `visible_rows` the two
    /// can differ: the collector's is the panel's row budget minus what
    /// `collect_col`'s fill pass measured its siblings to occupy, the tree's
    /// is the `.flex(1)` share layout actually gave the node.
    ///
    /// **Then the last paint**, recorded in
    /// [`crate::widgets::PaintedWindow`], which is the whole answer for a
    /// panel the runtime still paints.
    ///
    /// **Then the spec**, for the frame between a mount and the first layout,
    /// where nothing has laid this widget out at all.
    ///
    /// **The division to items happens here, once.** Every branch reports
    /// both numbers, so no seam downstream has to divide a row count by an
    /// item height — which is exactly the arithmetic that paged a card
    /// list past its own end (§6h) because one of two copies had it and
    /// the other did not.
    pub(crate) fn widget_viewport(
        &self,
        panel_key: &crate::widgets::PanelKey,
        widget: &fresh_core::api::WidgetSpec,
        widget_key: &str,
    ) -> crate::widgets::kinds::Viewport {
        if let Some(v) = self.described_widget_viewport(panel_key, widget_key) {
            return v;
        }
        self.widget_registry
            .get(panel_key)
            .and_then(|panel| panel.painted_viewport(widget_key))
            .unwrap_or_else(|| crate::widgets::kinds::Viewport::from_spec(widget))
    }

    /// The window the *tree* gave this widget, for a panel the tree describes.
    ///
    /// **The question is put to the tree, and it either has the element or it
    /// does not.** Same shape as `advance_panel_focus_in_tree`'s
    /// `has_focus_within`: nothing here asks the runtime "is this panel
    /// described" or "does this spec have a described interior" and then
    /// trusts the answer — a second authority for a fact the tree holds is
    /// §6g's defect class, and it has shipped three times. The lookup *is* the
    /// test: a widget the tree laid out has a keyed element with a viewport
    /// under it, and a widget it did not has neither.
    ///
    /// **Scoped to this panel's own subtree**, because a key is unique only
    /// where its owner says it is ([`fresh_ui::Ui::find_by_key_in`] says so in
    /// as many words). The dock, the floating panel and every pane can each
    /// hold a described panel in the same frame, and two plugins may both key
    /// a list `"items"`; a frame-wide lookup would hand one panel's window to
    /// another's handler.
    ///
    /// `None` for a cell-scrolling viewport, by
    /// [`fresh_ui::Ui::item_window`]'s design — a `Tree` drawn as bordered
    /// cards scrolls line by line, so its window is not in items and
    /// answering with its height in rows would be the units conflated (§6i).
    /// Such a widget falls through to the paint, which publishes both.
    fn described_widget_viewport(
        &self,
        panel_key: &crate::widgets::PanelKey,
        widget_key: &str,
    ) -> Option<crate::widgets::kinds::Viewport> {
        if widget_key.is_empty() {
            return None;
        }
        let ui = self.shell_ui.as_ref()?;
        let root = self.panel_subtree_root(ui, panel_key)?;
        // **Only the vertical.** For an item-scrolling viewport the window
        // rectangle is mixed-unit — `y`/`h` count items, `w` counts cells —
        // so `h` is the item count and there is nothing across to read.
        // `cells` is the same window's height in the unit a `Tree`'s offset
        // moves in, which is why it comes back beside it rather than being
        // multiplied out of a band the host cannot see.
        let (window, cells) = ui.item_window_in(root, &fresh_ui::Key::Str(widget_key.into()))?;
        Some(crate::widgets::kinds::Viewport {
            items: (window.h as u32).max(1),
            rows: cells as u32,
        })
    }

    /// The element a described panel's widgets are laid out under, per
    /// surface — the root every by-key lookup into that panel is scoped to.
    ///
    /// Three surfaces, three roots, and the pane's is per-leaf for the same
    /// reason `splits::content_key`'s own doc gives: a grid puts several
    /// interiors in one tree, and an unscoped name finds whichever came
    /// first. `panel::interior_key` cannot serve a pane — it maps every
    /// `Slot::Pane` to one number.
    ///
    /// `None` when the surface has no described interior in this frame, which
    /// is the same answer as "the tree did not lay this panel out".
    fn panel_subtree_root(
        &self,
        ui: &fresh_ui::Ui<crate::view::shell::msg::UiMsg>,
        panel_key: &crate::widgets::PanelKey,
    ) -> Option<fresh_ui::ElementId> {
        use crate::view::shell::widgets::Slot;
        match self.slot_of_panel(panel_key) {
            Some(super::PanelSlot::Dock) => {
                ui.find_by_key(&crate::view::shell::panel::interior_key(Slot::Dock))
            }
            Some(super::PanelSlot::Floating) => {
                ui.find_by_key(&crate::view::shell::panel::interior_key(Slot::Floating))
            }
            Some(super::PanelSlot::Sidebar(i)) => {
                ui.find_by_key(&crate::view::shell::panel::interior_key(Slot::Sidebar(i)))
            }
            None => {
                let buffer = self.widget_registry.get(panel_key)?.buffer_id;
                let leaf = self
                    .window_panes()
                    .into_iter()
                    .find(|(_, b)| *b == buffer)
                    .map(|(leaf, _)| leaf)?;
                ui.find_by_key(&crate::view::shell::splits::content_key(leaf))
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
                let viewport = self.widget_viewport(panel_key, &widget, &focus_key);
                let disposition = match self.widget_registry.get_mut(panel_key) {
                    Some(panel_mut) => crate::widgets::kinds::behavior(&widget)
                        .on_key(&widget, &focus_key, panel_mut, viewport, key, &mut fx),
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

    /// Move this panel's focus by `delta` tab stops.
    ///
    /// **Two rings, and which one is authoritative is the tree's answer, not
    /// a count taken from the runtime.** Every host-driven focus move arrives
    /// here — the plugin's `WidgetAction::FocusAdvance`, a kind's
    /// `KeyFx::focus_advance` (Enter committing a field), the smart-key
    /// `Tab` / `Shift+Tab` a plugin's `defineMode` binding produces, and the
    /// arrows on a button-only popup — so this is the one place the choice
    /// can be made once.
    ///
    /// Since S2 a described panel's Tab is the *tree's*: the keyboard layer
    /// names the interior as its focus scope, the fallback declines Tab, and
    /// `default_for_intent` runs `Ui::move_focus`. The box arena's ring below
    /// still exists because a panel with no described interior — an empty
    /// dock, or one the adapter's scope has nothing focusable in — keeps the
    /// key sink and has no tree ring at all. Advancing on the arena while the
    /// tree holds focus writes the registry's key without moving the tree's
    /// focus, and the next Tab then starts from the widget the user has
    /// already left: two rings, disagreeing.
    ///
    /// So the tree is asked whether it is holding this panel's focus —
    /// `has_focus_within(interior_key(slot))`, which is a fact only the tree
    /// has — and if it is, the move is *its* `move_focus` and the registry's
    /// key follows through the `WidgetFocus` mirror. Deliberately not a
    /// "does this panel have focus targets" question put to the runtime: that
    /// is the two-sources-for-one-fact shape that shipped once already
    /// (`c89d25f`), and the runtime cannot know whether the tree's focus is
    /// where its ring would start.
    fn handle_widget_focus_advance(&mut self, panel_key: &crate::widgets::PanelKey, delta: i32) {
        if self.advance_panel_focus_in_tree(panel_key, delta) {
            return;
        }
        let panel = match self.widget_registry.get(panel_key) {
            Some(p) => p,
            None => return,
        };
        // The ring comes from the *spec*, scoped to the nearest focus-trap
        // ancestor of the focused widget (a modal / Component subtree
        // contains Tab cycling; without traps this is the whole panel's
        // declaration order). It read the box arena until S6, which was the
        // same two facts — focusable, and the enclosing trap — recovered from
        // the rectangles a paint left behind; a described panel produces no
        // rectangles and never needed them, because both facts are
        // `box_meta`'s and `box_meta` is a function of the spec.
        let ring = crate::widgets::focus_ring_scoped_in_spec(&panel.spec, &panel.focus_key);
        if ring.is_empty() {
            return;
        }
        let n = ring.len() as i32;
        // "Nothing focused" sits *outside* the ring, not on its first
        // entry — so the first Tab must land on the first widget and the
        // first Shift+Tab on the last.
        //
        // `position()` answering `None` for an unfocused panel used to
        // fall through to index 0, and the step was applied from there:
        // Tab went to `ring[1]`, skipping the first tabbable entirely,
        // and Shift+Tab to `ring[n - 1]`. That could not happen while
        // focus was always seeded, so `autoFocusFirst: false` is what
        // made this reachable.
        let new_key = match ring.iter().position(|k| k == &panel.focus_key) {
            Some(cur) => {
                let new_idx = ((cur as i32 + delta) % n + n) % n;
                ring[new_idx as usize].clone()
            }
            None if delta >= 0 => ring[0].clone(),
            None => ring[(n - 1) as usize].clone(),
        };
        self.set_panel_focus_and_notify(panel_key, new_key);
        self.rerender_widget_panel(panel_key);
    }

    /// Ask the tree to move this panel's focus along its own ring, and report
    /// whether it was the one to answer.
    ///
    /// **The question is `has_focus_within`, and it is put to the tree.**
    /// A panel is on the tree's ring when its keyboard layer named an
    /// interior as its focus scope *and* focus is currently inside it — which
    /// is one lookup and one containment test, both answered from the focus
    /// tree that would do the moving. Anything the host could compute instead
    /// (does this spec have tabbable widgets, is this panel `focused`) is a
    /// second source for a fact the tree already holds, and would be wrong in
    /// exactly the cases that matter: a scope with nothing focusable in it
    /// keeps the sink, and a panel that is not the focused surface has the
    /// tree's focus somewhere else entirely.
    ///
    /// `true` includes **the tree declining to move**: a scope holding one
    /// focusable answers every direction with the element focus is already
    /// on, and `Ui::move_focus` reports that as no move (its own doc says
    /// why — returning `true` there would claim the key). Falling through to
    /// the arena there is precisely the disagreement this exists to prevent:
    /// it would write a focus key the tree's focus is not on, and the user's
    /// next Tab would start from the widget they had already left.
    ///
    /// `false` means there is no tree ring for this panel — no described
    /// interior, nothing in it to focus, a pane-mounted panel, or the panel
    /// is not where focus is — and the box arena is the only ring there is.
    ///
    /// `panel::Interior::has_focus_targets` is *not* this question: it is a
    /// build input, deciding whether the description declares a scope at all,
    /// which has to be answered before the tree exists. It used to be a count
    /// the runtime recorded, which made it a second authority that could go
    /// stale against the spec; it is now the same predicate the tree applies
    /// to the same spec (`widgets::any_on_the_ring`), so it cannot disagree
    /// with the ring it is predicting. Where it is still *incomplete* — a
    /// focusable reachable only inside a card or a shut pop-over — the failure
    /// stays graceful in both directions: the scope is either not declared
    /// (the panel keeps its sink, as before) or declared with nothing in it,
    /// and the test below then answers `false` and this falls through to the
    /// arena rather than to a focus move nobody can see.
    ///
    /// **What this does not fix, stated rather than implied.** A panel that
    /// is mounted but not focused still advances on the arena, so its
    /// registry key (which is what the description paints the focus marker
    /// from) can name a widget the tree's focus is not on. It is settled the
    /// next time focus *enters* the panel, and correctly: the description
    /// marks the registry's focused widget `autofocus`
    /// (`view::shell::widgets::on_the_ring`), so `apply_autofocus` lands the
    /// tree where the marker already is rather than on the scope's first
    /// control — which is the seeding this paragraph used to say was missing,
    /// said as a description rather than as a second writer of the tree's
    /// focus.
    ///
    /// The other moment — a focus move made while the panel is *already*
    /// focused, which `apply_autofocus` cannot settle because it leaves focus
    /// alone once it is inside the scope — is the mirror's second direction,
    /// and it is [`Editor::focus_panel_widget_in_tree`]: every host-side write
    /// of the registry's focus key moves the tree's focus with it, under the
    /// same `has_focus_within` test this function asks.
    fn advance_panel_focus_in_tree(
        &mut self,
        panel_key: &crate::widgets::PanelKey,
        delta: i32,
    ) -> bool {
        use crate::view::shell::msg::{UiFact, UiMsg};
        use crate::view::shell::widgets::Slot;
        let slot = match self.slot_of_panel(panel_key) {
            Some(super::PanelSlot::Dock) => Slot::Dock,
            Some(super::PanelSlot::Floating) => Slot::Floating,
            Some(super::PanelSlot::Sidebar(i)) => Slot::Sidebar(i),
            // A pane-mounted panel has no keyboard layer, so it names no
            // scope; its keys arrive by the buffer's route and the arena is
            // its ring.
            None => return false,
        };
        let Some(mut ui) = self.shell_ui.take() else {
            return false;
        };
        let holds = ui
            .find_by_key(&crate::view::shell::panel::interior_key(slot))
            .is_some_and(|el| ui.has_focus_within(el));
        if !holds {
            self.shell_ui = Some(ui);
            return false;
        }
        // **Take whatever is pending first, and drop it, so what follows is
        // unambiguously this move's.** `Ui::pending_messages` is where
        // framework-initiated activity leaves its facts — what
        // `apply_autofocus` settled on a frame since the last input, and any
        // `request_focus` this dispatch has already made. The editor's own
        // drain is `Editor::apply_settled_shell_messages`, at the head of
        // every `shell_dispatch`; this path can run between two of those, so
        // there can be something here.
        //
        // Dropping it rather than applying it is safe *because of what these
        // facts are*: every one of them names where this tree's focus is, and
        // the move below is about to name that again and write the registry
        // from it. A superseded answer is not worth the re-entrancy of
        // applying messages in the middle of one. Anything else that ever
        // lands in this queue would have to be reconsidered here.
        let _superseded = ui.take_messages();
        let dir = match delta < 0 {
            true => fresh_ui::FocusDir::Prev,
            false => fresh_ui::FocusDir::Next,
        };
        // `delta` is a count of tab stops, not a direction — the arena moves
        // `|delta|` of them in one go, and answers a zero delta by staying
        // put — so the tree is stepped exactly that many times.
        // `WidgetAction::FocusAdvance`'s own doc only defines ±1, and nothing
        // bundled sends more.
        for _ in 0..delta.unsigned_abs() {
            if !ui.move_focus(dir) {
                break;
            }
        }
        let msgs = ui.take_messages();
        self.shell_ui = Some(ui);
        // **One landing, one `focus` event.** Each step raises a
        // `UiFact::WidgetFocus`, and applying them all would tell the plugin
        // about tab stops it was never told about when the arena did this —
        // and would run each intermediate kind's `on_focus_change`. Only the
        // last names where focus ended up; the rest are the walk.
        let last_focus = msgs
            .iter()
            .rposition(|m| matches!(m, UiMsg::Ui(UiFact::WidgetFocus { .. })));
        let msgs: Vec<UiMsg> = msgs
            .into_iter()
            .enumerate()
            .filter(|(i, m)| {
                !matches!(m, UiMsg::Ui(UiFact::WidgetFocus { .. })) || Some(*i) == last_focus
            })
            .map(|(_, m)| m)
            .collect();
        // Default `EventFacts`: they describe the pointer event a message was
        // produced *by*, and there isn't one — the host asked for this move.
        // Nothing on this path reads them (`WidgetFocus`'s applier does not),
        // and inventing a cell would be worse than saying there was none.
        self.apply_shell_messages(msgs, Default::default());
        true
    }

    /// Move the tree's focus to this panel's `widget`, when the tree is the
    /// ring holding the panel's focus.
    ///
    /// **The second direction of the focus mirror**, and the half that was
    /// missing. `UiFact::WidgetFocus` writes the registry from what the tree
    /// decided; nothing wrote the tree from what the *host* decided, so every
    /// focus move the host makes while the panel is already focused — a
    /// plugin's `setFocusKey`, the dock's `/` landing on its filter, a kind's
    /// own focus effect — left the two rings pointing at different widgets.
    /// The description then painted the marker where the registry said and
    /// the next Tab started from where the tree said: the dock's create
    /// dropdown closed back onto `sessions` while the tree still held the
    /// button above it, so three Tabs from the list landed one stop past the
    /// section header the user was aiming at.
    ///
    /// Scoped by the same question `advance_panel_focus_in_tree` asks — does
    /// the tree hold this panel's focus — because that is what makes the tree
    /// the ring at all. When it does not (a mounted but unfocused panel, a
    /// pane-mounted one, an interior with nothing focusable), there is
    /// nothing to move and the landing is settled by `autofocus` the next
    /// time focus enters the panel.
    ///
    /// The gain the move raises goes into `Ui::pending_messages` and is
    /// applied at the next dispatch (`Editor::apply_settled_shell_messages`),
    /// where it names the widget the registry already holds and the mirror's
    /// first direction is a no-op. It does not loop.
    ///
    /// **Two of the three writers of the registry's focus key call this, and
    /// the third does not need to.** This one and
    /// `WidgetMutation::SetFocusKey` are decisions. The third is
    /// `rerender_widget_panel`'s re-clamp onto the first tabbable when the
    /// focused widget is not in the new spec — and a widget that left the
    /// spec left the tree with it, so the tree's focused element is gone and
    /// `apply_autofocus` settles regardless; `on_the_ring`'s `autofocus` mark
    /// then lands it on the clamped key. Pushing from inside a render would
    /// buy nothing and would move focus during a paint.
    pub(super) fn focus_panel_widget_in_tree(
        &mut self,
        panel_key: &crate::widgets::PanelKey,
        widget: &str,
    ) {
        use crate::view::shell::widgets::Slot;
        let slot = match self.slot_of_panel(panel_key) {
            Some(super::PanelSlot::Dock) => Slot::Dock,
            Some(super::PanelSlot::Floating) => Slot::Floating,
            Some(super::PanelSlot::Sidebar(i)) => Slot::Sidebar(i),
            None => return,
        };
        let Some(mut ui) = self.shell_ui.take() else {
            return;
        };
        let holds = ui
            .find_by_key(&crate::view::shell::panel::interior_key(slot))
            .is_some_and(|el| ui.has_focus_within(el));
        if holds {
            if let Some(el) = ui.find_by_key(&crate::view::shell::widgets::widget_focus_key(widget))
            {
                // `SelectAll` is what the ring's own moves ask for
                // (`Ui::move_focus`), and a host-driven landing is not a
                // different kind of landing.
                ui.request_focus(el, fresh_ui::SelectionOnFocus::SelectAll);
            }
        }
        self.shell_ui = Some(ui);
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
        // The tree is the ring; the registry is its mirror. Written in step,
        // before the plugin is told, so nothing reads one against the other.
        self.focus_panel_widget_in_tree(panel_key, &new_key);
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
    /// **A panel the tree describes is skipped entirely, and the reason is a
    /// coordinate space.** `pos` is a row and a display column in the *text
    /// projection's* rows — the ones the collector laid out and, for a
    /// buffer-mounted panel, wrote into the buffer — and `boxes` is that same
    /// projection's arena. For a described panel the rows on screen were
    /// placed by the tree instead, at a different width and with its own
    /// viewport offsets, so the arena answers about a layout nobody is looking
    /// at: a notch over one list would move another, or move nothing while the
    /// list under the pointer sat still. Its wheel is its viewports', which
    /// `fresh-ui` chains into for any notch nothing claimed.
    ///
    /// **With one window that is not a viewport's**, and it is reached by name
    /// rather than by rectangle: a `Text`'s open candidate list is windowed
    /// out of `completion_scroll_offset`, host state the plugin's
    /// `SetCompletions` writes, so no element can hold it. The tree hit-tests
    /// the float it placed and says which widget the notch landed on;
    /// [`Self::wheel_widget_by_key`] is the other end. That is not a hole in
    /// this gate — it is the same `on_wheel`, told the widget instead of asked
    /// to find one in a layout that is not on screen.
    ///
    /// **The gate is here rather than at the callers, because there are three
    /// of them and each stood down on its own.** `dock::column` returns `None`
    /// for a described interior, `panel::frame_box` attaches no wheel gesture
    /// for one, and `splits::panel_content` never took the wheel — so no
    /// described panel reaches this today by any route I can find. That is
    /// four sites agreeing by construction, which is exactly the shape F.9
    /// named: a gate applied at every caller is not a gate, it is a
    /// coincidence maintained by hand. Stating it once, where the arena is
    /// actually read, is what makes it a rule.
    ///
    /// What still routes here is the class the arena is right for: a
    /// pane-mounted panel that rides the *buffer's* scroll (`git_log`), whose
    /// rows on screen really are the projection's rows.
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
            if self.panel_wheel_is_the_trees(&panel_key, buffer_id) {
                continue;
            }
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
                let viewport = self.widget_viewport(&panel_key, widget, &widget_key);
                let Some(panel) = self.widget_registry.get_mut(&panel_key) else {
                    break;
                };
                if crate::widgets::kinds::behavior(widget).on_wheel(
                    widget,
                    &widget_key,
                    panel,
                    viewport,
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

    /// Scroll one *named* widget's own window by a wheel notch.
    ///
    /// **The same `on_wheel`, reached by name instead of by rectangle.**
    /// [`Self::handle_widget_panel_wheel_at`] finds the widget by hit-testing
    /// the box arena, which a described panel does not have a layout for; the
    /// tree hit-tests its own nodes and says which widget the notch landed on,
    /// and this is the other end of that. Nothing else differs: the kind
    /// decides what a notch means and whether it took it, and a widget that
    /// moved gets the panel re-rendered.
    ///
    /// Returns `true` when the widget consumed the notch. Today the only
    /// caller is a `Text`'s open completion list — see
    /// [`UiFact::WidgetWheel`](crate::view::shell::msg::UiFact::WidgetWheel)
    /// for why that is the one window in a described panel the tree cannot
    /// own.
    pub(crate) fn wheel_widget_by_key(
        &mut self,
        panel_key: &crate::widgets::PanelKey,
        widget_key: &str,
        delta: i32,
    ) -> bool {
        if widget_key.is_empty() {
            return false;
        }
        let Some(spec) = self.widget_registry.get(panel_key).map(|p| p.spec.clone()) else {
            return false;
        };
        let Some(widget) = crate::widgets::find_widget_by_key(&spec, widget_key) else {
            return false;
        };
        let viewport = self.widget_viewport(panel_key, widget, widget_key);
        let Some(panel) = self.widget_registry.get_mut(panel_key) else {
            return false;
        };
        if !crate::widgets::kinds::behavior(widget)
            .on_wheel(widget, widget_key, panel, viewport, delta)
        {
            return false;
        }
        self.rerender_widget_panel(panel_key);
        true
    }

    /// Whether this panel's wheel belongs to the tree rather than to the box
    /// arena — which is the same question as "does the tree describe it".
    ///
    /// Two slots and one buffer test, because a panel is described under two
    /// different rules: [`Self::panel_is_described`] for the dock and the
    /// floating panel (mounted is described), and
    /// [`Self::pane_panel_is_described`] for a pane-mounted one (only the
    /// panels that own their own scroll are). See
    /// [`Self::handle_widget_panel_wheel_at`] for why the distinction is a
    /// coordinate space and not a preference.
    fn panel_wheel_is_the_trees(
        &self,
        panel_key: &crate::widgets::PanelKey,
        buffer_id: crate::model::event::BufferId,
    ) -> bool {
        match self.slot_of_panel(panel_key) {
            Some(slot) => self.panel_is_described(slot),
            None => self.pane_panel_is_described(buffer_id),
        }
    }

    /// **Does the tree describe this panel's interior** — the same two rules,
    /// asked of the panel rather than of a wheel's target buffer.
    ///
    /// [`Self::panel_wheel_is_the_trees`] takes the buffer the notch was
    /// aimed at, because a wheel arrives at a buffer and several panels can
    /// render into one. A re-render has a panel in hand and takes the buffer
    /// off it.
    pub(crate) fn panel_is_the_trees(&self, panel_key: &crate::widgets::PanelKey) -> bool {
        match self.slot_of_panel(panel_key) {
            Some(slot) => self.panel_is_described(slot),
            None => self
                .widget_registry
                .get(panel_key)
                .is_some_and(|p| self.pane_panel_is_described(p.buffer_id)),
        }
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
        // A read-only or markdown `Text` is somewhere you *read*, not
        // somewhere you type — and this predicate now decides whether a
        // printable key reaches the mode's bindings at all, so calling a
        // rendered document "focused text input" would divert every
        // character into a field that discards it.
        matches!(
            crate::widgets::find_widget_by_key(&panel.spec, &panel.focus_key),
            Some(fresh_core::api::WidgetSpec::Text {
                read_only: false,
                markdown: false,
                ..
            })
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
    /// a mouse click (#2573). `byte_in_field` is the click's byte offset
    /// within *the field's own* rendered row; `payload` is the `focus`
    /// event's payload, which carries the value-layout breadcrumbs the
    /// renderer stamped on it (`valueInnerStart` and the truncation
    /// fields). Maps that byte back to a value byte, moves the cursor, and
    /// fires `change` so a plugin mirroring the cursor position (e.g. Search
    /// & Replace) stays in sync.
    ///
    /// **One coordinate space, and the caller puts the click in it.**
    /// `valueInnerStart` is relative to the field's own rendered text
    /// (gutter + label + `[`), and the pass that composes two fields onto one
    /// line (Search + Replace) shifts the `HitArea`'s byte range without
    /// shifting the payload — so a caller resolving through the text
    /// projection's rows subtracts the matched hit's `byte_start` before
    /// calling. This used to take that offset as a parameter and subtract it
    /// here, which meant the described path, whose byte is field-relative
    /// already, had to add it on first.
    ///
    /// A no-op for events without the layout payload (older render paths,
    /// non-text widgets) or when the clicked widget isn't the focused
    /// one — the caller is expected to focus it first.
    pub(super) fn reposition_widget_text_cursor_from_click(
        &mut self,
        panel_key: &crate::widgets::PanelKey,
        widget_key: &str,
        byte_in_field: usize,
        payload: &serde_json::Value,
    ) {
        let inner_start = match payload.get("valueInnerStart").and_then(|v| v.as_u64()) {
            Some(v) => v as usize,
            None => return,
        };
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
        // Settings UI via `crate::widgets`). The click is already field-
        // relative, so there is no row offset left to take off: `byte_start`
        // is 0.
        let value_byte = crate::widgets::row_byte_to_value_byte(
            byte_in_field,
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
        // A sidebar section is the column's width less its two walls — laid
        // and wrapped at one number, the dock's rule inverted for two
        // borders (`view::shell::sidebar::body`).
        if let super::PanelSlot::Sidebar(_) = slot {
            return (self.sidebar_cols() as u32).saturating_sub(2).max(1);
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
    /// a centered modal takes its `height_pct` share.
    ///
    /// **Two rows come off, and only one arm has a frame to justify them.**
    /// For the centred and anchored panel the reservation is the box's own
    /// border, and the number is not in competition with layout: the box is
    /// `Sizing::Auto` (`view::shell::panel::Panel::height`), so the budget
    /// decides the content's height and `Auto` measures around it — one
    /// authority with the layout following, not two answers.
    ///
    /// The dock is the arm where that argument does not hold. Its rectangle is
    /// the full terminal height (`Editor::compute_dock_split`) and its column
    /// has no border — the divider is a *column*, which
    /// `floating_panel_inner_width` accounts for separately — so nothing in
    /// the dock's geometry spends these two rows. What does spend them is the
    /// orchestrator's own layout: it pads below its tree so its hint bar lands
    /// on the column's last two rows. That makes the number a plugin's
    /// convention wearing a frame's name, and the honest replacement is the
    /// one `splits::panel_content` already uses for a pane — take the budget
    /// from `LayoutInfo::constraints.max_h` inside `dock::column`'s existing
    /// `layout_reader`, which is the height the column really has. It is not
    /// done here because the two are not equal and the difference is the dock
    /// e2e suite's to adjudicate, not a reader's.
    pub(super) fn floating_panel_inner_height(&self, slot: super::PanelSlot) -> Option<u32> {
        let term_h = (self.terminal_height.max(1)) as u32;
        // A section's budget is the body rows the last frame resolved it
        // to: its height is the column's to decide, not a share of the
        // terminal's.
        if let super::PanelSlot::Sidebar(i) = slot {
            let sec = self.sidebar_sections.get(i)?;
            sec.panel.as_ref()?;
            return Some((sec.resolved as u32).max(1));
        }
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
        self.pane_content_rect_for_buffer(buffer_id)
    }
}

/// Panel pointer machinery shared by every mounted floating panel (the dock
/// and the centered modal): wheel routing, the text drag, and dismissal.
/// Behavior owned by the panel runtime (moved from mouse_input.rs).
///
/// **What is no longer here.** The cell→widget probe, the list scrollbar's
/// press and drag, and the dropdown pop-over's click all resolved a screen
/// cell against rectangles the interior painter had recorded. That painter
/// went in 2.4 and they went with it in S7 — a described panel's widgets are
/// nodes that answer their own presses, and its list's bar is the viewport's,
/// which captures the pointer itself. What survives here is what a *node*
/// cannot answer: a notch aimed at the panel's own scroll, a drag through
/// text inside a widget, and closing the panel.
impl Editor {
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
                | Some(super::PanelPlacement::SidebarSection { .. })
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
        let Some(rect) = self.pane_content_rect_for_buffer(buffer_id) else {
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
    /// Raise a widget's context menu from a hit the caller already has.
    ///
    /// **The half that was never the probe's.** Deciding *which* widget a
    /// right press belongs to is geometry; deciding what a right press on it
    /// means is not. The panel answers the first itself — its widgets are
    /// nodes with rectangles — and hands the hit here. There is no other
    /// producer left: the runtime's own right-press probe over the box arena
    /// went in 2.4 and the left-press one in S7, so this is the whole of the
    /// second half rather than the shared end of two paths.
    pub(super) fn fire_widget_context(
        &mut self,
        slot: super::PanelSlot,
        hit: &crate::widgets::WidgetEvent,
        col: u16,
        row: u16,
    ) -> bool {
        let panel_key = match self.panel(slot) {
            Some(fwp) => fwp.panel_key.clone(),
            None => return false,
        };
        // A right-click raises a menu for a session row, not for a button or
        // empty padding: only kinds that declared the capability answer.
        if !hit.context_click {
            return false;
        }
        let mut payload = hit.payload.clone();
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
        self.fire_widget_event(
            &panel_key,
            hit.widget_key.clone(),
            "context".to_string(),
            payload,
        );
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
        if let Some(o) = self.panel_opt_mut(slot) {
            *o = None;
        }
        let _ = self.widget_registry.unmount(&panel_key);
    }

    /// Track what the pointer is over inside a panel mounted into a
    /// BUFFER that the shell tree does not describe, and re-render it
    /// when the answer changes.
    ///
    /// A described pane answers its own hover: its rows are nodes and they
    /// report enter and leave, which is why the runtime's dock/floating
    /// probe could be deleted outright. But the flip is gated on panels
    /// that own their scroll (`pane_panel_owns_its_scroll`), and the pages
    /// whose selection rides the buffer cursor — the welcome screen, Git
    /// Log — stay on the painter. Nothing on the tree side reaches them,
    /// so without this every clickable thing in one stays dark under the
    /// pointer.
    ///
    /// Resolution goes through the same `screen_to_buffer_position` →
    /// `hit_test_row_aware` pair the mounted click path uses, so hover and
    /// click can never disagree about what the pointer is on.
    pub(super) fn update_mounted_widget_hover(&mut self, col: u16, row: u16) -> bool {
        // Which mounted panel is under the pointer, and on what.
        //
        // `pane_content_at` is the one answer to "which pane's content covers
        // this cell", and it reads the rectangle off the shell tree rather
        // than a recorded list — the same source the click path resolves
        // against, which is what keeps hover and click from disagreeing.
        let mut hit_for: Option<(BufferId, String, String)> = None;
        let pane = self.pane_content_at(col, row);
        let panes = self.window_panes();
        'probe: {
            let Some((split_id, content_rect)) = pane.as_ref().map(|(l, r)| (l, *r)) else {
                break 'probe;
            };
            let Some(buffer_id) = panes
                .iter()
                .find(|(leaf, _)| leaf == split_id)
                .map(|(_, b)| b)
            else {
                break 'probe;
            };
            if self
                .widget_registry
                .panels_for_buffer(*buffer_id)
                .is_empty()
            {
                break 'probe;
            }
            let cached_mappings = self
                .active_layout()
                .view_line_mappings
                .get(split_id)
                .cloned();
            let splits = self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.buffers.splits())
                .map(|(_, vs)| vs);
            let fallback = splits
                .and_then(|vs| vs.get(split_id))
                .map(|vs| vs.viewport.top_byte())
                .unwrap_or(0);
            let compose_width = splits
                .and_then(|vs| vs.get(split_id))
                .and_then(|vs| vs.compose_width);
            let gutter_width = self
                .buffers()
                .get(buffer_id)
                .map(|s| s.margins.left_total_width() as u16)
                .unwrap_or(0);
            let Some(byte_pos) = super::click_geometry::screen_to_buffer_position(
                col,
                row,
                content_rect,
                gutter_width,
                &cached_mappings,
                fallback,
                true,
                compose_width,
            ) else {
                break 'probe;
            };
            let Some(state) = self
                .windows
                .get(&self.active_window)
                .map(|w| &w.buffers)
                .and_then(|b| b.get(buffer_id))
            else {
                break 'probe;
            };
            let (brow, bcol) = state.buffer.position_to_line_col(byte_pos);
            // `on_overlay = false`: a mounted panel drops the overlay and
            // popup channels at mount, so there is no covering surface.
            if let Some((_, hit)) = self.widget_registry.hit_test_row_aware(
                *buffer_id,
                brow.min(u32::MAX as usize) as u32,
                bcol.min(u32::MAX as usize) as u32,
                false,
            ) {
                let item = hit
                    .event
                    .payload
                    .get("key")
                    .and_then(|v| v.as_str())
                    .unwrap_or_default()
                    .to_string();
                hit_for = Some((*buffer_id, hit.event.widget_key.clone(), item));
            }
        }
        // Every other mounted panel resolves to "nothing hovered", which
        // is what clears a highlight the pointer has left.
        let mut changed = Vec::new();
        for panel_key in self.widget_registry.panel_keys() {
            let Some(buffer_id) = self
                .widget_registry
                .buffer_and_spec_ref(&panel_key)
                .map(|(b, _)| b)
            else {
                continue;
            };
            if Self::slot_for_panel_buffer(buffer_id).is_some() {
                continue;
            }
            let (widget, item) = match &hit_for {
                Some((b, w, i)) if *b == buffer_id => (w.clone(), i.clone()),
                _ => (String::new(), String::new()),
            };
            if self
                .widget_registry
                .set_hover_keys(&panel_key, widget, item)
            {
                changed.push(panel_key);
            }
        }
        let any = !changed.is_empty();
        for panel_key in changed {
            self.rerender_widget_panel(&panel_key);
        }
        any
    }
}

#[cfg(test)]
mod tests {
    use super::Editor;
    use crate::config::Config;
    use crate::config_io::DirectoryContext;
    use fresh_core::api::WidgetSpec;
    use std::sync::Arc;
    use tempfile::TempDir;

    fn make_editor() -> (Editor, TempDir) {
        let temp_dir = TempDir::new().unwrap();
        let dir_context = DirectoryContext::for_testing(temp_dir.path());
        let fs: Arc<dyn crate::model::filesystem::FileSystem + Send + Sync> =
            Arc::new(crate::model::filesystem::StdFileSystem);
        let editor = Editor::new(
            Config::default(),
            80,
            24,
            dir_context,
            crate::view::color_support::ColorCapability::TrueColor,
            fs,
        )
        .unwrap();
        (editor, temp_dir)
    }

    fn dock_panel(panel_key: crate::widgets::PanelKey) -> crate::app::FloatingWidgetState {
        crate::app::FloatingWidgetState {
            panel_key,
            width_pct: 30,
            height_pct: 100,
            placement: crate::app::PanelPlacement::LeftDock { width_cols: 30 },
            focused: true,
            entries: Vec::new(),
            last_inner_rect: None,
            scrollbar_zone_hovered: false,
            scrollbar_flash_until: None,
            fullscreen: false,
            focus_marker: false,
            title: None,
            closable: false,
            hovered_widget_key: String::new(),
            hovered_item_key: String::new(),
            hovered_popup_row: String::new(),
        }
    }

    /// One frame of the shell's tree, without a terminal: the same two calls
    /// `Editor::render` makes around it.
    fn frame_the_shell(editor: &mut Editor) {
        use ratatui::layout::Rect;
        let dock = Rect::new(0, 0, 30, 24);
        let chrome = Rect::new(30, 0, 50, 24);
        let shell = editor.shell_frame((Some(dock), chrome));
        let mut ui = editor.shell_ui.take().expect("the shell tree");
        crate::view::shell::geometry::stats::note_shell_layout();
        ui.frame(
            crate::view::shell::frame::frame_tree(shell),
            fresh_ui::Size::new(80, 24),
        );
        editor.shell_ui = Some(ui);
    }

    fn button(key: &str) -> WidgetSpec {
        WidgetSpec::Button {
            label: key.into(),
            focused: false,
            intent: Default::default(),
            key: Some(key.into()),
            disabled: false,
            focusable: true,
            bare: false,
            full_width: false,
            hover_style: None,
            style: None,
        }
    }

    fn list_of(n: usize) -> WidgetSpec {
        WidgetSpec::List {
            items: (0..n)
                .map(|i| fresh_core::text_property::TextPropertyEntry::text(format!("row {i}")))
                .collect(),
            item_specs: Vec::new(),
            item_keys: (0..n).map(|i| format!("k{i}")).collect(),
            selected_index: 0,
            visible_rows: Some(4),
            focusable: true,
            key: Some("lst".into()),
        }
    }

    /// A list the plugin left the host to size — the `visible_rows: None`
    /// branch, which is the only one where the collector and the tree can
    /// disagree (an explicit count wins unconditionally in both).
    fn auto_list(n: usize) -> WidgetSpec {
        match list_of(n) {
            WidgetSpec::List {
                items,
                item_specs,
                item_keys,
                selected_index,
                focusable,
                key,
                ..
            } => WidgetSpec::List {
                items,
                item_specs,
                item_keys,
                selected_index,
                visible_rows: None,
                focusable,
                key,
            },
            other => other,
        }
    }

    fn mount_list_panel(
        editor: &mut Editor,
        panel_key: &crate::widgets::PanelKey,
        buffer_id: crate::model::event::BufferId,
    ) {
        let spec = list_of(40);
        let out = super::render_floating_spec(
            false,
            &spec,
            &Default::default(),
            &Default::default(),
            "",
            40,
            None,
            "",
            "",
            "",
            None,
            true,
        );
        editor.widget_registry.mount(
            panel_key.clone(),
            buffer_id,
            spec,
            out.hits,
            out.instance_states,
            out.focus_key,
            out.tabbable,
            out.painted,
            out.boxes,
            true,
        );
    }

    /// The list's scroll offset — the *painted* window's, which is where
    /// an offset lives now.
    fn list_scroll(editor: &Editor, panel_key: &crate::widgets::PanelKey) -> u32 {
        match editor
            .widget_registry
            .get(panel_key)
            .and_then(|p| p.painted.get("lst"))
        {
            Some(w) => w.offset,
            None => panic!("a painted window for the list"),
        }
    }

    /// **A described panel's wheel never reaches the box arena, and the arena
    /// is still there for the panel that needs it.**
    ///
    /// Both halves matter, which is why they are one test. The position handed
    /// to `handle_widget_panel_wheel_at` is a row and a column in the *text
    /// projection's* rows, and `boxes` is that projection's arena — so it is
    /// the right answer for a panel whose rows on screen are those rows, and
    /// an answer about an invisible layout for a panel the tree placed.
    ///
    /// The dock is described whenever a panel is mounted in it, so its notch
    /// belongs to the viewport the description built and this must decline it
    /// (the caller then lets `fresh-ui`'s scroll chain run). The same spec on
    /// an ordinary buffer is not described — nothing built a viewport for it —
    /// and the arena is the only thing that can say which list the pointer is
    /// over.
    #[test]
    fn the_arena_answers_a_wheel_only_where_the_tree_did_not_place_the_rows() {
        let (mut editor, _t) = make_editor();
        let described = crate::widgets::PanelKey::new("test-plugin", 1);
        let plain = crate::widgets::PanelKey::new("test-plugin", 2);
        let dock_buffer = crate::app::PanelSlot::Dock.buffer_id();
        let plain_buffer = crate::model::event::BufferId(9_999);
        mount_list_panel(&mut editor, &described, dock_buffer);
        mount_list_panel(&mut editor, &plain, plain_buffer);
        editor.dock = Some(dock_panel(described.clone()));
        assert!(
            editor.panel_is_described(crate::app::PanelSlot::Dock),
            "a mounted dock panel is described — the premise of the first half"
        );
        assert!(
            !editor.pane_panel_is_described(plain_buffer),
            "an ordinary buffer's panel is not — the premise of the second"
        );

        // Row 1, column 2: inside the list's own box in the projection's
        // arena, which is what makes this a hit rather than a miss.
        let took_dock = editor.handle_widget_panel_wheel_at(dock_buffer, Some((1, 2)), 3);
        assert!(
            !took_dock,
            "the described panel declines, so the caller can let the tree's \
             scroll chain have the notch"
        );
        assert_eq!(
            list_scroll(&editor, &described),
            0,
            "and it moved nothing: the registry's offset is not the window the \
             description draws from"
        );

        let took_plain = editor.handle_widget_panel_wheel_at(plain_buffer, Some((1, 2)), 3);
        assert!(took_plain, "the projection's own panel consumes its notch");
        assert_eq!(
            list_scroll(&editor, &plain),
            3,
            "and the arena resolved the pointer to the list under it"
        );
    }

    /// **The one window in a described panel the runtime still owns, reached
    /// by name.**
    ///
    /// The arena stands down for a described panel and that is right for every
    /// scrolling surface but this one: a `Text`'s candidate list is windowed
    /// by `completion_popup` out of `completion_scroll_offset`, which the
    /// plugin's `SetCompletions` writes and no viewport can hold. So the tree
    /// hit-tests the float it placed and says which widget the notch was on,
    /// and this is the end that moves it — the same `on_wheel` the arena would
    /// have called, reached by a key instead of by a rectangle.
    #[test]
    fn a_named_widget_takes_a_wheel_the_arena_would_have_declined() {
        let (mut editor, _t) = make_editor();
        let panel_key = crate::widgets::PanelKey::new("test-plugin", 1);
        let buffer = crate::app::PanelSlot::Dock.buffer_id();
        let spec = WidgetSpec::Text {
            value: "he".into(),
            cursor_byte: 2,
            focused: true,
            label: String::new(),
            placeholder: None,
            rows: 1,
            field_width: 20,
            max_visible_chars: 0,
            full_width: false,
            completions: Vec::new(),
            completions_visible_rows: 3,
            block_caret: false,
            sel_start: -1,
            sel_end: -1,
            label_width: 0,
            read_only: false,
            markdown: false,
            key: Some("field".into()),
        };
        let out = super::render_floating_spec(
            false,
            &spec,
            &Default::default(),
            &Default::default(),
            "field",
            40,
            None,
            "",
            "",
            "",
            None,
            true,
        );
        editor.widget_registry.mount(
            panel_key.clone(),
            buffer,
            spec,
            out.hits,
            out.instance_states,
            out.focus_key,
            out.tabbable,
            out.painted,
            out.boxes,
            true,
        );
        editor.dock = Some(dock_panel(panel_key.clone()));
        // Candidates never come from the spec — the plugin pushes them — so
        // this is the state a `SetCompletions` leaves behind.
        match editor
            .widget_registry
            .get_mut(&panel_key)
            .and_then(|p| p.instance_states.get_mut("field"))
        {
            Some(crate::widgets::WidgetInstanceState::Text { completions, .. }) => {
                *completions = (0..9).map(|i| format!("cand{i}").into()).collect();
            }
            other => panic!("a text instance state, got {other:?}"),
        }

        assert!(
            editor.wheel_widget_by_key(&panel_key, "field", 3),
            "the field takes the notch its open list is under"
        );
        let scroll = match editor
            .widget_registry
            .get(&panel_key)
            .and_then(|p| p.instance_states.get("field"))
        {
            Some(crate::widgets::WidgetInstanceState::Text {
                completion_scroll_offset,
                completion_navigated,
                ..
            }) => {
                assert!(
                    *completion_navigated,
                    "scrolling is stepping into the list: Enter now accepts a row"
                );
                *completion_scroll_offset
            }
            other => panic!("a text instance state, got {other:?}"),
        };
        assert_eq!(scroll, 3, "three rows, the notch the caller was handed");

        // A key that names nothing is not an error and moves nothing — the
        // tree can outlive a spec the plugin has already replaced.
        assert!(!editor.wheel_widget_by_key(&panel_key, "gone", 3));
        assert!(!editor.wheel_widget_by_key(&panel_key, "", 3));
    }

    /// **Where the tree has no ring, the arena is still the one that works.**
    ///
    /// This is the half of S6 that is *not* available to be deleted. A panel
    /// whose interior the tree does not describe — or describes with nothing
    /// focusable in it — keeps `panel::keys_layer`'s sink, and the sink is
    /// outside every scope: `Ui::move_focus` there has nowhere to go. The box
    /// arena is the only ring such a panel has, and every host-driven advance
    /// (`WidgetAction::FocusAdvance`, `KeyFx::focus_advance`, the smart-key
    /// `Tab`) has to keep landing on it.
    ///
    /// The editor here has never had a frame, so its tree carries no
    /// interior scope at all — which is exactly the shape
    /// `advance_panel_focus_in_tree` must decline, and it declines it by
    /// asking the tree rather than by asking the runtime how many tabbables
    /// the spec has.
    #[test]
    fn a_panel_the_tree_does_not_hold_advances_on_the_arena() {
        let (mut editor, _t) = make_editor();
        let panel_key = crate::widgets::PanelKey::new("test-plugin", 1);
        let spec = WidgetSpec::Col {
            children: vec![button("one"), button("two")],
            key: None,
        };
        let out = super::render_floating_spec(
            false,
            &spec,
            &Default::default(),
            &Default::default(),
            "",
            40,
            None,
            "",
            "",
            "",
            None,
            true,
        );
        assert_eq!(
            out.tabbable,
            vec!["one".to_string(), "two".to_string()],
            "the arena's ring, from the same `box_meta` the tree's is built from"
        );
        editor.widget_registry.mount(
            panel_key.clone(),
            crate::app::PanelSlot::Dock.buffer_id(),
            spec,
            out.hits,
            out.instance_states,
            out.focus_key,
            out.tabbable,
            out.painted,
            out.boxes,
            true,
        );
        assert_eq!(
            editor.widget_registry.focus_key(&panel_key),
            Some("one"),
            "the render clamps focus onto the first tabbable"
        );
        // In the dock's slot, so the routing resolves a slot and asks the
        // tree — and the tree, never having been framed, carries no interior
        // scope to answer with.
        editor.dock = Some(dock_panel(panel_key.clone()));

        editor.handle_widget_focus_advance(&panel_key, 1);
        assert_eq!(
            editor.widget_registry.focus_key(&panel_key),
            Some("two"),
            "the arena advanced it"
        );
        editor.handle_widget_focus_advance(&panel_key, -1);
        assert_eq!(
            editor.widget_registry.focus_key(&panel_key),
            Some("one"),
            "and back"
        );
    }

    /// **And where the tree does hold it, the tree is the one that moves.**
    ///
    /// The same panel, mounted in the dock slot and given a frame, so the
    /// keyboard layer names its interior and focus settles on a widget inside
    /// it. A plugin's `FocusAdvance` now moves *the tree's* focus and the
    /// registry's key follows as the mirror it is meant to be — where before,
    /// the arena wrote the key and the tree's focus stayed where it was, so
    /// the user's next Tab started from the widget they had already left.
    #[test]
    fn a_panel_the_tree_holds_advances_on_the_trees_ring() {
        let (mut editor, _t) = make_editor();
        let panel_key = crate::widgets::PanelKey::new("test-plugin", 1);
        let spec = WidgetSpec::Col {
            children: vec![button("one"), button("two")],
            key: None,
        };
        let out = super::render_floating_spec(
            false,
            &spec,
            &Default::default(),
            &Default::default(),
            "",
            30,
            None,
            "",
            "",
            "",
            None,
            true,
        );
        editor.widget_registry.mount(
            panel_key.clone(),
            crate::app::PanelSlot::Dock.buffer_id(),
            spec,
            out.hits,
            out.instance_states,
            out.focus_key,
            out.tabbable,
            out.painted,
            out.boxes,
            true,
        );
        editor.dock = Some(dock_panel(panel_key.clone()));
        frame_the_shell(&mut editor);

        let scope =
            crate::view::shell::panel::interior_key(crate::view::shell::widgets::Slot::Dock);
        let ui = editor.shell_ui.as_ref().expect("the tree");
        let el = ui
            .find_by_key(&scope)
            .expect("the interior names the scope");
        assert!(ui.has_focus_within(el), "and the frame settled focus in it");
        let before = ui.focused();

        editor.handle_widget_focus_advance(&panel_key, 1);

        let ui = editor.shell_ui.as_ref().expect("the tree");
        assert_ne!(ui.focused(), before, "the tree's focus is what moved");
        assert_eq!(
            editor.widget_registry.focus_key(&panel_key),
            Some("two"),
            "and the registry's key mirrors it"
        );
    }

    /// **A described panel's window is the tree's, and the paint's copy of it
    /// is not consulted.**
    ///
    /// The collector and the reconciler resolve the same auto-sized list two
    /// different ways — the collector takes the panel's row budget and
    /// subtracts what `collect_col`'s fill pass measured its siblings to
    /// occupy, the tree gives the node its `.flex(1)` share of what layout
    /// actually had — and until S5 the handlers were driven by the first even
    /// on a surface the second had drawn. The paint's record is deliberately
    /// poisoned here with a window the tree did not lay out: a reader that
    /// still consults it reports three.
    #[test]
    fn a_described_panels_window_is_the_trees_not_the_paints() {
        let (mut editor, _t) = make_editor();
        let panel_key = crate::widgets::PanelKey::new("test-plugin", 1);
        let spec = WidgetSpec::Col {
            children: vec![button("hdr"), auto_list(40)],
            key: None,
        };
        let out = super::render_floating_spec(
            false,
            &spec,
            &Default::default(),
            &Default::default(),
            "",
            30,
            Some(24),
            "",
            "",
            "",
            None,
            true,
        );
        // What the collector resolved for the same list, so the two numbers
        // are visible side by side: with a plain header above it they agree
        // (23 rows of the dock's 24, one taken by the button). The force of
        // this test is not that they differ here — it is that the paint's
        // record below is *not* what answers.
        let collector = out.painted.get("lst").copied().expect("collector window");
        editor.widget_registry.mount(
            panel_key.clone(),
            crate::app::PanelSlot::Dock.buffer_id(),
            spec.clone(),
            out.hits,
            out.instance_states,
            out.focus_key,
            out.tabbable,
            out.painted,
            out.boxes,
            true,
        );
        editor.dock = Some(dock_panel(panel_key.clone()));
        frame_the_shell(&mut editor);

        let widget = crate::widgets::find_widget_by_key(&spec, "lst")
            .cloned()
            .expect("the list is in the spec");
        // What the last paint left, replaced by a number no layout produced.
        editor
            .widget_registry
            .get_mut(&panel_key)
            .expect("the panel")
            .painted
            .insert(
                "lst".to_string(),
                crate::widgets::PaintedWindow {
                    rows: 3,
                    items: 3,
                    offset: 0,
                },
            );

        let vp = editor.widget_viewport(&panel_key, &widget, "lst");
        let tree = editor
            .shell_ui
            .as_ref()
            .expect("the tree")
            .item_window_in(
                editor
                    .shell_ui
                    .as_ref()
                    .unwrap()
                    .find_by_key(&crate::view::shell::panel::interior_key(
                        crate::view::shell::widgets::Slot::Dock,
                    ))
                    .expect("the dock's interior"),
                &fresh_ui::Key::Str("lst".into()),
            )
            .expect("the list's own viewport published its window");
        assert_eq!(
            (vp.items, vp.rows),
            (tree.0.h as u32, tree.1 as u32),
            "the window is the one the reconciler laid out"
        );
        assert!(
            vp.items > 3,
            "and not the paint's, which said three: got {}",
            vp.items
        );
        assert_eq!(
            vp.items, collector.items,
            "and in this shape the collector agrees — the divergence S5 is \
             about needs a Col whose children the two measure differently"
        );

        // The same panel with no tree: the paint is then the only layout
        // there is, and it is read exactly as before.
        editor.shell_ui = None;
        let vp = editor.widget_viewport(&panel_key, &widget, "lst");
        assert_eq!((vp.items, vp.rows), (3, 3), "the paint's window");
    }

    /// **A described panel re-renders without producing a text projection.**
    ///
    /// The rows, the hit areas and the box arena are what the collector is
    /// *for*, and for a panel the tree describes each of them has no reader:
    /// its rows are nodes, its presses are those nodes', and its arena answers
    /// no wheel. What a re-render still has to do is the three walks of
    /// `resolve_panel` — carry the state, clamp the focus, publish the ring —
    /// and this pins that it does them and produces nothing else.
    ///
    /// The same panel outside a slot, with no described interior, keeps the
    /// collector: the assertion at the end is the half that must not change.
    #[test]
    fn a_described_panel_resolves_instead_of_rendering() {
        let (mut editor, _t) = make_editor();
        let described = crate::widgets::PanelKey::new("test-plugin", 1);
        let plain = crate::widgets::PanelKey::new("test-plugin", 2);
        let plain_buffer = crate::model::event::BufferId(9_999);
        mount_list_panel(
            &mut editor,
            &described,
            crate::app::PanelSlot::Dock.buffer_id(),
        );
        mount_list_panel(&mut editor, &plain, plain_buffer);
        editor.dock = Some(dock_panel(described.clone()));
        // A selection the plugin's spec does not carry — the state a
        // re-render must not lose.
        editor
            .widget_registry
            .get_mut(&described)
            .expect("the panel")
            .instance_states
            .insert(
                "lst".to_string(),
                crate::widgets::WidgetInstanceState::List {
                    selected_index: 7,
                    user_scrolled: true,
                },
            );

        editor.rerender_widget_panel(&described);

        let panel = editor.widget_registry.get(&described).expect("the panel");
        assert!(
            panel.hits.is_empty() && panel.boxes.is_empty() && panel.painted.is_empty(),
            "no text projection: {} hits, {} boxes, {} painted windows",
            panel.hits.len(),
            panel.boxes.len(),
            panel.painted.len()
        );
        assert_eq!(panel.focus_key, "lst", "the focus clamp still ran");
        assert_eq!(panel.tabbable, vec!["lst".to_string()], "and the ring");
        assert!(
            matches!(
                panel.instance_states.get("lst"),
                Some(crate::widgets::WidgetInstanceState::List {
                    selected_index: 7,
                    user_scrolled: true,
                })
            ),
            "and the state was carried, not re-seeded from the spec"
        );

        editor.rerender_widget_panel(&plain);
        let panel = editor.widget_registry.get(&plain).expect("the panel");
        assert!(
            !panel.hits.is_empty() && !panel.boxes.is_empty(),
            "a panel the tree does not describe still gets its projection"
        );
    }

    /// **A widget nothing has laid out falls back to the spec, and a `Tree`
    /// divides.**
    ///
    /// The frame between a mount and the first layout: no element, no paint.
    /// A row budget is not an item count for a tree of bordered cards — the
    /// dock's card view is four rows a node — and reporting one as the other
    /// pages it four times too far on the first key after a mount.
    #[test]
    fn an_unlaid_out_widget_takes_the_specs_window_in_the_specs_own_units() {
        use crate::widgets::kinds::Viewport;
        let cards = WidgetSpec::Tree {
            nodes: Vec::new(),
            item_keys: Vec::new(),
            selected_index: 0,
            visible_rows: Some(12),
            key: Some("t".into()),
            expanded_keys: Vec::new(),
            checkable: false,
            indent_cols: 2,
            item_height: 2,
            card_borders: true,
        };
        assert_eq!(
            Viewport::from_spec(&cards),
            Viewport { rows: 12, items: 3 },
            "twelve rows of four-row cards is three cards"
        );
        let lines = match cards.clone() {
            WidgetSpec::Tree {
                nodes,
                item_keys,
                selected_index,
                visible_rows,
                key,
                expanded_keys,
                checkable,
                indent_cols,
                ..
            } => WidgetSpec::Tree {
                nodes,
                item_keys,
                selected_index,
                visible_rows,
                key,
                expanded_keys,
                checkable,
                indent_cols,
                item_height: 1,
                card_borders: false,
            },
            other => other,
        };
        assert_eq!(
            Viewport::from_spec(&lines),
            Viewport {
                rows: 12,
                items: 12
            },
            "and a single-line tree's rows are its nodes"
        );
    }

    /// **The mirror's other direction: a focus the host decides moves the
    /// tree's.**
    ///
    /// Not every focus move is a traversal. A plugin's `setFocusKey`, the
    /// dock's `/` landing on its filter, a kind's own focus effect — all of
    /// them write the registry through `set_panel_focus_and_notify`, and
    /// `apply_autofocus` cannot settle any of them, because it leaves focus
    /// alone once it is inside the scope. Left there, the panel painted its
    /// marker on the widget the registry named while the next Tab moved from
    /// the one the tree still held: the dock's create dropdown closed back
    /// onto the session list and three Tabs from it landed one stop past the
    /// section header they were aimed at.
    #[test]
    fn a_focus_the_host_decides_moves_the_trees_focus_too() {
        let (mut editor, _t) = make_editor();
        let panel_key = crate::widgets::PanelKey::new("test-plugin", 1);
        let spec = WidgetSpec::Col {
            children: vec![button("one"), button("two")],
            key: None,
        };
        let out = super::render_floating_spec(
            false,
            &spec,
            &Default::default(),
            &Default::default(),
            "",
            30,
            None,
            "",
            "",
            "",
            None,
            true,
        );
        editor.widget_registry.mount(
            panel_key.clone(),
            crate::app::PanelSlot::Dock.buffer_id(),
            spec,
            out.hits,
            out.instance_states,
            out.focus_key,
            out.tabbable,
            out.painted,
            out.boxes,
            true,
        );
        editor.dock = Some(dock_panel(panel_key.clone()));
        frame_the_shell(&mut editor);

        let ui = editor.shell_ui.as_ref().expect("the tree");
        let one = ui.find_by_key(&crate::view::shell::widgets::widget_focus_key("one"));
        assert_eq!(ui.focused(), one, "the frame settled on the first widget");

        editor.set_panel_focus_and_notify(&panel_key, "two".to_string());

        let ui = editor.shell_ui.as_ref().expect("the tree");
        assert_eq!(
            ui.focused(),
            ui.find_by_key(&crate::view::shell::widgets::widget_focus_key("two")),
            "the tree followed the host's write, so the next Tab starts here"
        );
    }
}
