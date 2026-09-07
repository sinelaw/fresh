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
        h_pan: &std::collections::HashMap<String, i32>,
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
                // The reader's sideways fold, for the same reason: a repaint
                // that dropped it would slide every row back to its resting
                // window under a reader who had panned away from it.
                h_pan: Some(h_pan),
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
    h_pan: Option<&std::collections::HashMap<String, i32>>,
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
            // The reader's sideways fold, for the same reason: a repaint that
            // dropped it would slide every row back to its resting window
            // under a reader who had panned away from it.
            h_pan,
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
    /// so the byte the library reports for a press on it is already that.
    /// (The caller that resolved through the text projection's composed rows
    /// and rebased by the matched area's `byte_start` is deleted with the
    /// panel class it served; the space is the field's own on every path.)
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
        // **Asked of the spec, not of a recorded ring.** The registry no longer
        // records a ring; the spec in hand is the only current answer. The
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
    // with them. See `docs/internal/retained-mode-ui.md` §3.9.
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

    /// The described page in the pane that holds the keyboard, if there is
    /// one.
    pub(crate) fn active_page_panel(&self) -> Option<crate::widgets::PanelKey> {
        let buffer = self.active_buffer();
        self.widget_registry
            .panels_for_buffer(buffer)
            .into_iter()
            .find(|k| self.widget_registry.get(k).is_some_and(|p| p.page))
    }

    /// **The page follows the caret.** Run after every event that could have
    /// moved it — an arrow key, a page key, a click on the text, and equally
    /// an edit, which moves the caret without being a `MoveCursor`.
    ///
    /// This is the whole of a page's navigation, and it is why the page binds
    /// nothing. The mirror buffer under a described pane is a real buffer with
    /// a real cursor, so every motion the editor already has — the arrows,
    /// `Home`/`End`, `Ctrl+Home`/`Ctrl+End`, the page keys, word motions, vi
    /// operators, and whatever the user rebound them to — moves it for free.
    /// The page's job is to *read* where that caret ended up: bring its row
    /// into the window, draw the caret there, and point the keyboard at
    /// whatever control the row carries.
    ///
    /// The caret is read off the buffer rather than out of the event. Those
    /// differ — an edit has no new-position field at all, a `Batch` can nest
    /// another whose last move is the one that counts, and a position can be
    /// clamped on the way in — and every one of those differences was a hole
    /// while the page matched on key names instead.
    pub(super) fn page_follows_caret(&mut self) {
        let Some(key) = self.active_page_panel() else {
            return;
        };
        let follows = self
            .widget_registry
            .get(&key)
            .is_some_and(|p| p.focus_follows_cursor);
        let position = self.active_window().active_cursors().primary().position;
        let st = self.active_state();
        let row = st.buffer.get_line_number(position);
        let col = st
            .buffer
            .line_start_offset(row)
            .map(|start| position.saturating_sub(start))
            .unwrap_or(0);
        let at = (row as u32, col.min(u16::MAX as usize) as u16);
        if self.page_reading.get(&key) == Some(&at) {
            return;
        }
        // **A page with no reader still follows one.** `focusFollowsCursor`
        // decides whether the caret is *drawn* and whether focus is seated on
        // its row; the window follows the caret either way, because a page
        // whose text scrolled away from the cursor it reports in the status
        // bar is a page that has lost its place.
        if follows {
            self.page_reading.insert(key.clone(), at);
        }
        if let Some(anchor) = self.page_anchors.get(&key) {
            anchor.reveal(at.0);
        }
        self.shell_description_stale = true;
        if follows {
            self.sync_widget_focus_to_reading_row(&key);
        }
    }

    /// An editor action a page answers for itself, applied to its window.
    ///
    /// **The page does not re-bind anything.** Its keys are the editor's —
    /// resolved once, by the one resolver, against the user's own keymap —
    /// and this is the short list of *actions* whose subject is the window
    /// rather than a buffer's cursor. Everything else the editor still does:
    /// a page mounted in a pane switches tabs, opens the palette and quits
    /// with the same keys as any other pane, because nothing here claims
    /// them.
    ///
    /// Returns whether the action was the page's.
    pub(crate) fn page_takes_action(&mut self, action: &crate::input::keybindings::Action) -> bool {
        use crate::input::keybindings::Action;
        // **A page key moves the window by a page too**, so the caret keeps
        // its place on screen — which is what paging *is* in every other
        // buffer, and the one thing a minimal reveal cannot do: revealed
        // alone, a caret a page further down lands on the window's last row,
        // and the page after that reads as one row of movement.
        //
        // The caret's own move stays the editor's, so a page key is not
        // consumed here: the window goes first and the action follows it.
        enum Step {
            Rows(i32),
            Pages(i32),
        }
        let (step, consumed) = match action {
            Action::ScrollUp => (Step::Rows(-1), true),
            Action::ScrollDown => (Step::Rows(1), true),
            Action::MovePageUp | Action::SelectPageUp => (Step::Pages(-1), false),
            Action::MovePageDown | Action::SelectPageDown => (Step::Pages(1), false),
            _ => return false,
        };
        let Some(key) = self.active_page_panel() else {
            return false;
        };
        let Some(anchor) = self.page_anchors.get(&key) else {
            return false;
        };
        match step {
            Step::Rows(n) => anchor.scroll_by(n),
            Step::Pages(n) => anchor.scroll_by_pages(n),
        }
        self.shell_description_stale = true;
        consumed
    }

    /// The column a composed buffer is painted into, or `None` when it is
    /// not composing.
    ///
    /// The narrow half of [`Self::widget_panel_width`]'s reading, on its own
    /// because a described panel needs the number the *pane* is flanked by
    /// rather than the number its rows are laid out to. Read from this
    /// buffer's state and not through the split's deref, for the reason
    /// argued there: `SplitViewState` answers for whichever buffer is active
    /// in the split, which is the neighbouring tab whenever this panel is
    /// behind one.
    pub(crate) fn buffer_compose_width(&self, buffer_id: BufferId) -> Option<u16> {
        self.windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)?
            .values()
            .find(|vs| vs.buffer_state(buffer_id).is_some() && vs.viewport.width > 0)
            .and_then(|vs| vs.buffer_state(buffer_id))
            .and_then(|b| b.compose_width)
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
            //
            // Read it from *this buffer's* state, not from the split's:
            // `SplitViewState` derefs to whichever buffer is active
            // there, so `vs.compose_width` was the neighbouring tab's
            // answer whenever the panel was not the one on screen. A
            // panel opened behind another buffer therefore laid out to
            // the whole split and then had its centred rows wrapped
            // against a compose column it never saw — and it stayed
            // that way, because nothing repaints a background tab. The
            // pane width beside it is a property of the split, so that
            // one is right to read through the deref.
            .map(|vs| {
                (
                    vs.buffer_state(buffer_id).and_then(|b| b.compose_width),
                    vs.viewport.width,
                )
            })
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
        // Whatever this re-resolves is read by the description; the tree
        // catches up on the next frame, and a key before it lays one out.
        self.shell_description_stale = true;
        if self.resolve_described_panel(panel_key) {
            return;
        }
        // The spec already lives in the registry — mutations (e.g.
        // `append_tree_nodes_in_spec`) edit it in place. Borrow it for
        // render, then write back only the side-effects (instance
        // states, focus key). The previous shape cloned the
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
            let h_pan = self
                .widget_registry
                .get(panel_key)
                .map(|p| p.h_pan.clone())
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
                Some(&h_pan),
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
                out_pieces.instance_states,
                out_pieces.focus_key,
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
    /// nodes' own presses, its wheel is its viewports' (the box arena that
    /// answered one is deleted), and its painted windows are superseded by the viewport's
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
            None => state.buffer_id.and_then(|b| self.widget_panel_height(b)),
        };
        self.record_widget_panel_render_height(panel_key, avail_height);
        if self
            .widget_registry
            .update_side_effects(
                panel_key,
                out.instance_states,
                out.focus_key,
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
            // `w` counts cells on both kinds of window — it is the one part
            // of an item-scrolling rectangle that is not in items — so it is
            // the width a sideways pan is measured against.
            cols: window.w as u32,
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
                let buffer = self.widget_registry.get(panel_key)?.buffer_id?;
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
        // Kind-owned key handling (`docs/internal/retained-mode-ui.md` §3.5):
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
        }
        .cloned();
        let widget = widget.as_ref();
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
        // The ring is read off the tree, so the tree has to carry the panel
        // as it is now — a mount or a spec update since the last frame is
        // laid out first. See `Editor::shell_description_stale`.
        self.lay_out_shell_if_stale();
        if self.advance_panel_focus_in_tree(panel_key, delta) {
            return;
        }
        // **The tree does not hold this panel's focus** — a mounted but
        // unfocused panel, or a pane-mounted one whose keyboard is the
        // buffer's — so the panel's focus *fact* advances instead, along the
        // ring the tree would walk if it did (`Ui::next_in`). One order and
        // one source: the same registrations, in the same policy's order, as
        // the Tab above. Confined to the nearest focus scope around the
        // focused widget, which is what a `Component`'s trap declares.
        let Some(slot) = self.described_slot_of_panel(panel_key) else {
            return;
        };
        let focus_key = self
            .widget_registry
            .focus_key(panel_key)
            .map(str::to_string)
            .unwrap_or_default();
        let follows_reader = self
            .widget_registry
            .get(panel_key)
            .is_some_and(|p| p.page && p.focus_follows_cursor);
        let Some(ui) = self.shell_ui.as_ref() else {
            return;
        };
        let Some(interior) = ui.find_by_key(&crate::view::shell::panel::interior_key(slot)) else {
            return;
        };
        let from = (!focus_key.is_empty())
            .then(|| ui.find_by_key(&crate::view::shell::widgets::widget_focus_key(&focus_key)))
            .flatten()
            .filter(|f| ui.contains(interior, *f));
        let root = from
            .and_then(|f| ui.enclosing_focus_scope(f))
            .filter(|s| ui.contains(interior, *s))
            .unwrap_or(interior);
        let dir = match delta < 0 {
            true => fresh_ui::FocusDir::Prev,
            false => fresh_ui::FocusDir::Next,
        };
        // "Nothing focused" sits *outside* the ring: the first Tab lands on
        // the first widget and the first Shift+Tab on the last, which is what
        // `next_in` answers for a `from` it does not find.
        //
        // **Except on a page whose focus follows its reader**, where nothing
        // focused is not the panel at rest — it is the reader on prose, which
        // on a page that is mostly prose is most rows. Starting at the ring's
        // end there would send every Tab from below the fold to the top of the
        // document: read down to the third level, press Tab, and you are back
        // on the startup switch. So the ring is seeded from where the reader
        // is, and Tab goes on from there.
        let seed = match from.is_none() && follows_reader {
            true => self.page_ring_seed(panel_key, delta >= 0),
            false => None,
        };
        let from = seed
            .as_deref()
            .and_then(|k| ui.find_by_key(&crate::view::shell::widgets::widget_focus_key(k)))
            .filter(|f| ui.contains(interior, *f))
            .or(from);
        let mut cur = from;
        for _ in 0..delta.unsigned_abs() {
            match ui.next_in(root, cur, dir) {
                Some(n) => cur = Some(n),
                None => break,
            }
        }
        let Some(new) = cur.filter(|c| Some(*c) != from) else {
            return;
        };
        let Some(new_key) = ui
            .key_of(new)
            .as_ref()
            .and_then(crate::view::shell::widgets::widget_key_of)
            .map(str::to_string)
        else {
            return;
        };
        self.set_panel_focus_and_notify(panel_key, new_key);
        self.rerender_widget_panel(panel_key);
    }

    /// The slot whose described interior this panel is, pane-mounted panels
    /// included — `slot_of_panel` answers only the three slots with a
    /// keyboard layer.
    fn described_slot_of_panel(
        &self,
        panel_key: &crate::widgets::PanelKey,
    ) -> Option<crate::view::shell::widgets::Slot> {
        use crate::view::shell::widgets::Slot;
        match self.slot_of_panel(panel_key) {
            Some(super::PanelSlot::Dock) => return Some(Slot::Dock),
            Some(super::PanelSlot::Floating) => return Some(Slot::Floating),
            Some(super::PanelSlot::Sidebar(i)) => return Some(Slot::Sidebar(i)),
            None => {}
        }
        let buffer = self.widget_registry.get(panel_key)?.buffer_id?;
        self.window_panes()
            .into_iter()
            .find(|(_, b)| *b == buffer)
            .map(|(leaf, _)| Slot::Pane(leaf))
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
    /// focused — is the description's too: the mark moves with the registry's
    /// key, and `fresh_ui` re-settles focus onto a mark that moved. No host
    /// call places focus in the tree.
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
            .decide_focus(panel_key, new_key.clone());
        // **The fact is written; the tree follows it on the next frame.** The
        // description marks the registry's focused widget `autofocus`
        // (`view::shell::widgets::on_the_ring`), and `fresh_ui` re-settles
        // focus whenever that mark moves — on the frame that carries it,
        // including a frame that builds the widget for the first time. No
        // host-side call places focus in the tree, so there is nothing to
        // hold back or replay when the element is not there yet.
        self.shell_description_stale = true;
        // Offer the transition to the kinds: the widget losing focus
        // and the one gaining it each get their `on_focus_change`
        // hook (Tree keeps its selected-row highlight coherent with
        // focus — exactly one focused element). Kind-blind: no Tree
        // match here.
        self.notify_widget_focus_change(panel_key, &old_key, &new_key);
        self.fire_widget_event(
            panel_key,
            new_key.clone(),
            "focus".to_string(),
            serde_json::json!({ "previous": old_key }),
        );
        // **Last, and this is re-entrant on purpose.** On a
        // `focusFollowsCursor` panel the caret is half of what focus is,
        // so it comes along — and moving it re-resolves focus from where
        // it landed, which can disagree with the widget we just focused
        // (`anchor_of_widget` answers a widget's top-left cell;
        // `focus_target_at` answers the nearest control to a column, and
        // on a row with several controls those are not inverses). So:
        //
        //     set_panel_focus_and_notify
        //       └ seat_cursor_on_focused_widget
        //           └ seat_buffer_cursor
        //               └ sync_widget_focus_to_cursor
        //                   └ set_panel_focus_and_notify   (at most once)
        //
        // It terminates because the inner call's guard compares against
        // the reading row the outer one just wrote, and the second-level
        // entry then finds focus and the reader already agreeing.
        // `seat_focus_depth` makes that an assertion rather than a hope.
        //
        // Placing it *after* the event is the whole reason this is the
        // last statement: with the seat in the middle, a disagreeing
        // resolve fired the inner (final) `focus` event before the outer
        // (already superseded) one, and every plugin mirroring focus from
        // these events — `welcome_screen.ts` does — was left holding the
        // stale key.
        self.seat_focus_depth += 1;
        debug_assert!(
            self.seat_focus_depth <= 2,
            "focus/reader sync recursed past its one re-entry: focus and the reading \
             row disagree after a seat, which means `page_anchor_of_widget` and \
             `page_focus_target_at` are not settling on the same widget"
        );
        self.seat_reading_row_on_focused_widget(panel_key, &new_key);
        self.seat_focus_depth -= 1;
    }

    /// The page's viewport element, from the anchor bound to it.
    ///
    /// The anchor **is** the host's handle on that window (§3.5), so it is
    /// also the only name the host has for the element: the description keys
    /// the widgets inside the page, not the viewport around them.
    fn page_viewport(&self, panel_key: &crate::widgets::PanelKey) -> Option<fresh_ui::ElementId> {
        self.page_anchors.get(panel_key)?.target()
    }

    /// Every region a focusable widget of this page occupies, as
    /// `(content row, rows, first column, columns, key)`.
    ///
    /// **Content coordinates, not screen ones.** A child's laid-out rectangle
    /// has the window's offset already taken off it, so putting the offset
    /// back is what turns "where it is on screen" into "where it is in the
    /// page" — which is the space a reading row is in, and the space the
    /// anchor's `reveal` counts.
    ///
    /// **A widget's region is every node carrying its key, not just the one
    /// that takes focus.** A card several rows tall is a Tab stop once and a
    /// place to be many times: the welcome screen's door cards emit a node per
    /// row, all under the card's key, with `focusable` on the row that names
    /// the action. Reading only the focusable node would make the card a
    /// one-row region, so moving down inside it would leave it and take the
    /// Enter the reader aimed at it with them.
    fn page_widget_spans(
        &self,
        panel_key: &crate::widgets::PanelKey,
    ) -> Vec<(u32, u16, u16, u16, String)> {
        let Some(slot) = self.described_slot_of_panel(panel_key) else {
            return Vec::new();
        };
        let (Some(ui), Some(vp)) = (self.shell_ui.as_ref(), self.page_viewport(panel_key)) else {
            return Vec::new();
        };
        let Some(interior) = ui.find_by_key(&crate::view::shell::panel::interior_key(slot)) else {
            return Vec::new();
        };
        let ring_key_of = |el| {
            ui.key_of(el)
                .as_ref()
                .and_then(crate::view::shell::widgets::widget_key_of)
                .map(str::to_string)
        };
        // The region's nodes are in the *node* namespace, the ring's wrappers
        // in the focus one; a widget is both.
        let key_of = |el| {
            ui.key_of(el)
                .as_ref()
                .and_then(crate::view::shell::widgets::widget_key_of_any)
                .map(str::to_string)
        };
        // Only what Tab can reach is a place to be: prose is where "nothing
        // focused" comes from, and a node that cannot take focus must not
        // silently become the answer for its row.
        let tabbable: std::collections::HashSet<String> = ui
            .traversal_order(interior)
            .into_iter()
            .filter_map(ring_key_of)
            .collect();
        let origin = ui.rect_of(vp);
        let (scroll, _) = ui.scroll(vp);
        let mut out = Vec::new();
        let mut stack = vec![interior];
        while let Some(el) = stack.pop() {
            stack.extend(ui.children(el));
            let Some(key) = key_of(el).filter(|k| tabbable.contains(k)) else {
                continue;
            };
            let r = ui.rect_of(el);
            if r.h == 0 || r.w == 0 {
                continue;
            }
            let row = (r.y - origin.y + scroll.y).max(0) as u32;
            let col = (r.x - origin.x + scroll.x).max(0) as u16;
            out.push((row, r.h, col, r.w, key));
        }
        out
    }

    /// How far `col` is from a span running `[start, start + width)`: zero
    /// inside it, and the distance to the nearer edge outside.
    fn column_distance(start: u16, width: u16, col: u16) -> u32 {
        let end = start.saturating_add(width);
        match col {
            c if c < start => (start - c) as u32,
            c if c >= end => (c - end) as u32 + 1,
            _ => 0,
        }
    }

    /// The widget in `panel_key` that a reader at `(row, col)` is on, or `""`
    /// for none.
    ///
    /// **The row is the region; the column decides which control on it.** That
    /// is a weaker rule than "a focus region is a widget's own placed
    /// rectangle", and the weaker one is the one that works: a reader moving
    /// down a page keeps whatever column they were in, and on this editor's own
    /// pages that column is very often a framed card's border or the margin
    /// left of an inset card. Requiring containment would mean moving down
    /// through a card focuses nothing in it — including its text field, which
    /// then does not take what you type.
    ///
    /// So containment wins where it applies (distance zero), and otherwise the
    /// nearest control **on the same row**, ties leftmost. There is no distance
    /// cap, which has a consequence worth stating rather than discovering: on a
    /// row carrying exactly one control, that control is the answer for every
    /// column of the row.
    ///
    /// An empty string is an answer, not the absence of one: a reader on a row
    /// with no control at all means *nothing* is focused, and a caller that
    /// read it as "leave focus alone" would keep the last Tab's target armed
    /// under an Enter aimed at prose.
    ///
    /// **A card several rows tall is one region because it is one node.** The
    /// welcome screen's door cards are keyed once and placed once, so their
    /// rectangle covers every row they occupy and a reader anywhere in the card
    /// is on the card. That fell out of the tree rather than having to be
    /// arranged: the projection this replaced had a hit per *row*, and being a
    /// region at all depended on those rows sharing a key.
    fn page_focus_target_at(
        &self,
        panel_key: &crate::widgets::PanelKey,
        row: u32,
        col: u16,
    ) -> String {
        let mut best: Option<(u32, u16, String)> = None;
        for (top, rows, start, width, key) in self.page_widget_spans(panel_key) {
            if row < top || row >= top + rows.max(1) as u32 {
                continue;
            }
            let d = Self::column_distance(start, width, col);
            if best
                .as_ref()
                .is_none_or(|(bd, bs, _)| d < *bd || (d == *bd && start < *bs))
            {
                best = Some((d, start, key));
            }
        }
        best.map(|(_, _, k)| k).unwrap_or_default()
    }

    /// Where the reader goes when this widget takes focus: the first cell of
    /// the node that *takes* the focus, which is not the top of its region.
    ///
    /// **The two differ for a card, and each is right for its own question.**
    /// A card's region is every row it occupies, because a reader anywhere in
    /// it is on it; the row it is *seated* on is the one naming the action,
    /// because that is the line the card is about — its frame's top edge is
    /// not somewhere to be. `focusable` marks that row, so the ring's own
    /// element answers this and the region answers the other.
    fn page_anchor_of_widget(
        &self,
        panel_key: &crate::widgets::PanelKey,
        key: &str,
    ) -> Option<(u32, u16)> {
        let slot = self.described_slot_of_panel(panel_key)?;
        let ui = self.shell_ui.as_ref()?;
        let vp = self.page_viewport(panel_key)?;
        let interior = ui.find_by_key(&crate::view::shell::panel::interior_key(slot))?;
        let origin = ui.rect_of(vp);
        let (scroll, _) = ui.scroll(vp);
        // Every node carrying the key, and then the *innermost* of them: a
        // card is a frame around rows that carry its key too, and the frame's
        // top edge is a border rather than a line to be on.
        let mut mine = Vec::new();
        let mut stack = vec![interior];
        while let Some(el) = stack.pop() {
            stack.extend(ui.children(el));
            if ui
                .key_of(el)
                .as_ref()
                .and_then(crate::view::shell::widgets::widget_key_of_any)
                == Some(key)
            {
                mine.push(el);
            }
        }
        mine.iter()
            .filter(|el| !mine.iter().any(|o| o != *el && ui.contains(**el, *o)))
            .map(|el| {
                let r = ui.rect_of(*el);
                (
                    (r.y - origin.y + scroll.y).max(0) as u32,
                    (r.x - origin.x + scroll.x).max(0) as u16,
                )
            })
            .min()
    }

    /// Move the reader onto the region of the widget that just took focus, on
    /// a page that asked for `focusFollowsCursor`.
    ///
    /// The guard is not "is the reader already on that row" but "does the
    /// reader's row already resolve to this widget", which is the same question
    /// the other direction asks — and it has to be, because a card several rows
    /// tall anchors at its top. Arriving at its last row from below focuses the
    /// card; seating the reader on the card's *top* row would then throw them
    /// back over everything they just walked past, and the next Up would do it
    /// again.
    ///
    /// Clearing focus never moves the reader. "Nothing is focused" is what a
    /// reader on prose means; there is no row to go to.
    pub(super) fn seat_reading_row_on_focused_widget(
        &mut self,
        panel_key: &crate::widgets::PanelKey,
        new_key: &str,
    ) {
        if new_key.is_empty() {
            return;
        }
        if !self
            .widget_registry
            .get(panel_key)
            .is_some_and(|p| p.focus_follows_cursor)
        {
            return;
        }
        // The spans are read off the tree, so the tree has to carry the panel
        // as it is now.
        self.lay_out_shell_if_stale();
        if let Some((row, col)) = self.page_reading.get(panel_key).copied() {
            if self.page_focus_target_at(panel_key, row, col) == new_key {
                return;
            }
        }
        let Some(at) = self.page_anchor_of_widget(panel_key, new_key) else {
            return;
        };
        self.move_page_reader_to(panel_key, at);
    }

    /// Where a Tab ring should start when nothing is focused and the reader is
    /// where they are: the first focusable at or after that point in the ring's
    /// own order (`forward`), or the last at or before it. Wraps.
    ///
    /// The order is the tree's traversal order, and so is the geometry, so
    /// "before the reader" is asked and answered in one place.
    fn page_ring_seed(
        &self,
        panel_key: &crate::widgets::PanelKey,
        forward: bool,
    ) -> Option<String> {
        let at = self.page_reading.get(panel_key).copied()?;
        // One entry per widget, at its own first cell: the ring has one stop
        // per widget however many nodes carry its key.
        let mut anchors: std::collections::HashMap<String, (u32, u16)> =
            std::collections::HashMap::new();
        for (row, _, col, _, key) in self.page_widget_spans(panel_key) {
            let at = anchors.entry(key).or_insert((row, col));
            *at = (*at).min((row, col));
        }
        let mut ordered: Vec<((u32, u16), String)> =
            anchors.into_iter().map(|(k, at)| (at, k)).collect();
        ordered.sort();
        let found = match forward {
            true => ordered
                .iter()
                .find(|(p, _)| *p >= at)
                .or_else(|| ordered.first()),
            false => ordered
                .iter()
                .rev()
                .find(|(p, _)| *p <= at)
                .or_else(|| ordered.last()),
        };
        found.map(|(_, k)| k.clone())
    }

    /// A press on `pane` at screen `(x, y)` moves that pane's page reader, if
    /// it has one and the press landed inside its window.
    ///
    /// Screen coordinates in, content coordinates out — the same conversion
    /// `page_widget_spans` does, in the other direction.
    pub(super) fn press_moved_the_page_reader(&mut self, pane: LeafId, x: u16, y: u16) {
        let Some(panel_key) = self
            .widget_registry
            .panel_keys()
            .into_iter()
            .find(|k| {
                self.described_slot_of_panel(k)
                    == Some(crate::view::shell::widgets::Slot::Pane(pane))
            })
            .filter(|k| {
                self.widget_registry
                    .get(k)
                    .is_some_and(|p| p.page && p.focus_follows_cursor)
            })
        else {
            return;
        };
        let at = {
            let (Some(ui), Some(vp)) = (self.shell_ui.as_ref(), self.page_viewport(&panel_key))
            else {
                return;
            };
            let window = ui.rect_of(vp);
            let (scroll, _) = ui.scroll(vp);
            let (x, y) = (x as i32, y as i32);
            if x < window.x || x >= window.right() || y < window.y || y >= window.bottom() {
                return;
            }
            (
                (y - window.y + scroll.y).max(0) as u32,
                (x - window.x + scroll.x).max(0) as u16,
            )
        };
        self.move_page_reader_to(&panel_key, at);
        self.sync_widget_focus_to_reading_row(&panel_key);
    }

    /// Put the reader at `(row, col)` and bring that row into the window.
    ///
    /// The reveal is minimal, which is what following is: a Tab between two
    /// controls of one card must not move the page under them.
    fn move_page_reader_to(&mut self, panel_key: &crate::widgets::PanelKey, at: (u32, u16)) {
        self.page_reading.insert(panel_key.clone(), at);
        if let Some(anchor) = self.page_anchors.get(panel_key) {
            anchor.reveal(at.0);
        }
        self.mirror_page_reader_into_buffer(panel_key, at);
        self.shell_description_stale = true;
    }

    /// Put the mirror buffer's cursor on the row the reader is on.
    ///
    /// **The mirror is where a reading position is *reported* from**, and it
    /// goes on being that: the status bar's `Ln`/`Col`, a plugin's
    /// `cursor_moved`, and a click's line are all read off the buffer under a
    /// described panel. What changed is the direction — the buffer's cursor
    /// used to be the reading position and now follows it — which is what
    /// breaks the loop the page had: nothing resolves focus from the mirror any
    /// more, so a cursor that arrives here cannot come back round as a focus
    /// move.
    ///
    /// The pane shows the tree rather than the mirror, so this scrolls nothing.
    fn mirror_page_reader_into_buffer(
        &mut self,
        panel_key: &crate::widgets::PanelKey,
        (row, col): (u32, u16),
    ) {
        let Some(buffer_id) = self
            .widget_registry
            .get(panel_key)
            .and_then(|p| p.buffer_id)
        else {
            return;
        };
        let Some(offset) = self.active_window().buffers.get(&buffer_id).and_then(|st| {
            let start = st.buffer.line_start_offset(row as usize)?;
            let end = match st.buffer.line_start_offset(row as usize + 1) {
                // Less the newline that begins the next row.
                Some(next) => next.saturating_sub(1),
                None => st.buffer.len(),
            };
            Some((start + col as usize).min(end))
        }) else {
            return;
        };
        self.seat_buffer_cursor(buffer_id, offset);
    }

    /// The other direction: the reader has landed somewhere on a
    /// `focusFollowsCursor` page, so focus goes to whatever focusable widget is
    /// on that row — or to nothing, when the row carries none.
    ///
    /// Clearing is the half that matters most. Without it a movement key leaves
    /// the last Tab's target armed under Enter, three cards up and off screen:
    /// the reader is looking at one thing and the keyboard is pointed at
    /// another.
    fn sync_widget_focus_to_reading_row(&mut self, panel_key: &crate::widgets::PanelKey) {
        let Some((row, col)) = self.page_reading.get(panel_key).copied() else {
            return;
        };
        let next = self.page_focus_target_at(panel_key, row, col);
        if self.widget_registry.focus_key(panel_key) == Some(next.as_str()) {
            return;
        }
        self.set_panel_focus_and_notify(panel_key, next);
        // The focus marker is painted by the render, so the panel has to
        // repaint for the move to be visible at all.
        self.rerender_widget_panel(panel_key);
    }

    /// Every split currently showing `buffer_id`, the leaves of grouped
    /// subtrees included.
    ///
    /// A grouped subtree's leaves are not in `splits_for_buffer` — the
    /// group host owns the outer leaf — so a panel buffer shown inside
    /// one would keep a stale caret without this.
    pub(super) fn splits_showing_buffer(&self, buffer_id: BufferId) -> Vec<LeafId> {
        let (manager, view_states) = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .expect("active window must have a populated split layout");
        let mut splits = manager.splits_for_buffer(buffer_id);
        for node in self.active_window().grouped_subtrees.values() {
            if let crate::view::split::SplitNode::Grouped { layout, .. } = node {
                for inner_leaf in layout.leaf_split_ids() {
                    if view_states
                        .get(&inner_leaf)
                        .is_some_and(|vs| vs.active_buffer == buffer_id)
                        && !splits.contains(&inner_leaf)
                    {
                        splits.push(inner_leaf);
                    }
                }
            }
        }
        splits
    }

    /// Put `buffer_id`'s caret at `position` in every split showing it.
    ///
    /// The editor writes a caret through two routes — the event pipeline
    /// (`Editor::apply_event`, which every user-visible move travels:
    /// arrows, page keys, a click, Goto Line, back/forward, a search
    /// hit) and this one, for the host and plugins seating it directly.
    /// Both end in `sync_widget_focus_to_cursor`, so a
    /// `focusFollowsCursor` panel cannot be reached by a caret move that
    /// leaves its focus behind.
    ///
    /// That is an invariant, not an observation, and it only holds while
    /// **every** direct write goes through here — including the ones that
    /// look like initialisation. `CreateVirtualBufferWithContent`'s
    /// `initialCursorLine` and the display-buffer path both used to call
    /// `set_buffer_cursor_in_splits` themselves, into buffers that can
    /// perfectly well carry a focus-following panel; they call this now.
    pub(super) fn seat_buffer_cursor(&mut self, buffer_id: BufferId, position: usize) {
        let splits = self.splits_showing_buffer(buffer_id);
        if splits.is_empty() {
            tracing::warn!("No splits found for buffer {:?}", buffer_id);
        }
        if self.active_window().buffers.get(&buffer_id).is_none() {
            tracing::warn!("Buffer {:?} not found for a caret move", buffer_id);
            return;
        }
        self.active_window_mut()
            .set_buffer_cursor_in_splits(buffer_id, position, &splits);
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

    /// Scroll one *named* widget's own window by a wheel notch.
    ///
    /// **The same `on_wheel`, reached by name instead of by rectangle.**
    /// The runtime used to find the widget by hit-testing the text
    /// projection's box arena, which a described panel has no layout for; the
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

    /// Pan the keyed widget sideways by `delta` display columns.
    ///
    /// The runtime's counterpart to [`Self::wheel_widget_by_key`], and it does
    /// not go through a kind: a pan moves the panel's own fold rather than a
    /// window a kind resolves, so there is nothing for a `Tree` or a `List` to
    /// decide. What each kind decides is whether it *honours* the fold, which
    /// it does at paint time by threading it into `render_tree_row` — and
    /// [`pan_bounds`] answers that same question from the spec, so a notch over
    /// a kind that cannot show a pan falls through instead of being swallowed.
    ///
    /// [`pan_bounds`]: crate::widgets::render::pan_bounds
    pub(crate) fn pan_widget_by_key(
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
        let bounds = crate::widgets::render::pan_bounds(widget, viewport.cols, None);
        let Some(panel) = self.widget_registry.get_mut(panel_key) else {
            return false;
        };
        if !panel.pan_h(widget_key, Some(delta), bounds) {
            return false;
        }
        self.rerender_widget_panel(panel_key);
        true
    }

    /// **Does the tree describe this panel's interior** — and it does for
    /// every mounted panel. The dock, the floating modal, a sidebar section
    /// and a pane all describe what is mounted in them; the pane-mounted class
    /// that rode the buffer's scroll and kept the text projection is gone
    /// (design §3.5). What is left to ask is whether the panel is mounted.
    pub(crate) fn panel_is_the_trees(&self, panel_key: &crate::widgets::PanelKey) -> bool {
        self.widget_registry.get(panel_key).is_some()
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

    /// The first panel rendering into `buffer_id` that has a focused widget
    /// of *any* kind.
    ///
    /// [`Self::focused_text_widget_panel_for_buffer`] answers the narrower
    /// question the clipboard path asks; this one is for a key addressed to
    /// whatever holds focus — a `Tree`'s pan keys, where the whole point is
    /// that focus is *not* on a text field.
    pub(super) fn focused_widget_panel_for_buffer(
        &self,
        buffer_id: crate::model::event::BufferId,
    ) -> Option<crate::widgets::PanelKey> {
        self.widget_registry
            .panels_for_buffer(buffer_id)
            .into_iter()
            .find(|k| {
                self.widget_registry
                    .get(k)
                    .is_some_and(|p| !p.focus_key.is_empty())
            })
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
    /// Typed text goes to the focused widget's kind (`WidgetImpl::on_text`):
    /// a field inserts it, a number cell reads its digits, and a kind with
    /// nothing to type into passes — the same dispatch a named key gets.
    pub(super) fn handle_widget_text_char(
        &mut self,
        panel_key: &crate::widgets::PanelKey,
        text: &str,
    ) {
        if text.is_empty() {
            return;
        }
        let Some(panel) = self.widget_registry.get(panel_key) else {
            return;
        };
        let focus_key = panel.focus_key.clone();
        let Some(widget) = crate::widgets::find_widget_by_key(&panel.spec, &focus_key).cloned()
        else {
            return;
        };
        let mut fx = crate::widgets::kinds::KeyFx::default();
        let disposition = match self.widget_registry.get_mut(panel_key) {
            Some(panel_mut) => crate::widgets::kinds::behavior(&widget)
                .on_text(&widget, &focus_key, panel_mut, text, &mut fx),
            None => return,
        };
        if disposition != crate::widgets::kinds::KeyDisposition::Pass {
            self.rerender_widget_panel(panel_key);
        }
        for (event_type, payload) in fx.events {
            self.fire_widget_event(panel_key, focus_key.clone(), event_type, payload);
        }
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
        // The panel's keyboard is a fact the description reads (its keys
        // layer, its marks), so the tree is stale until it is rebuilt.
        self.shell_description_stale = true;
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
        // The blur is a focus write: the description marks the pane behind
        // the panel now, and the tree must say so before the next key is
        // routed over it (`Editor::get_key_context`).
        self.shell_description_stale = true;
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

    /// Retry deferred virtual-buffer animations now that the frame's
    /// layout has placed the panes. Called from render() after layout but before
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
/// and the centered modal): the text drag and dismissal.
///
/// **What is no longer here.** The cell→widget probe, the list scrollbar's
/// press and drag, the dropdown pop-over's click and the wheel all resolved a
/// screen cell against rectangles the interior painter or the text projection
/// had recorded. Every mounted panel is described now: its widgets are nodes
/// that answer their own presses, its lists are viewports whose wheel the
/// library chains into and whose bar captures the pointer itself. What
/// survives here is what a *node* cannot answer: a drag through text inside a
/// widget, and closing the panel.
impl Editor {
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
        let Some(buffer_id) = panel.buffer_id else {
            return;
        };
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
            mode: None,
            entries: Vec::new(),
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

    /// One frame of the shell's tree, without a terminal.
    ///
    /// **`Editor::lay_out_shell_tree`, not a re-spelling of it.** This used to
    /// repeat the take/frame/put-back sequence `render` performs, which meant
    /// a step deleted from `render` was still taken here and the tests went on
    /// passing — including the one guarding #3137, whose whole subject is a
    /// call `render` makes after the frame. Calling the same function is what
    /// makes that test able to fail.
    fn frame_the_shell(editor: &mut Editor) {
        use ratatui::layout::Rect;
        let dock = Rect::new(0, 0, 30, 24);
        let chrome = Rect::new(30, 0, 50, 24);
        let shell = editor.shell_frame((Some(dock), chrome));
        editor.lay_out_shell_tree(shell, fresh_ui::Size::new(80, 24));
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
            None,
        );
        editor.widget_registry.mount(
            panel_key.clone(),
            buffer_id,
            spec,
            out.instance_states,
            out.focus_key,
            out.painted,
            out.boxes,
            true,
            false,
            false,
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
            None,
        );
        editor.widget_registry.mount(
            panel_key.clone(),
            buffer,
            spec,
            out.instance_states,
            out.focus_key,
            out.painted,
            out.boxes,
            true,
            false,
            false,
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
    /// **Blurring a panel moves the tree's focus out of it, and the key
    /// context follows.** The focus fact has one writer, and the tree is its
    /// projection: while the dock holds the keyboard, its focused widget is
    /// marked and the focus chain reads as `Dock`; when the host blurs it
    /// (`blur_floating_panel`, the dive), the dock stops marking, the active
    /// pane's content is the mark, and the next key is routed over a tree
    /// that says so — `get_key_context` reads `Normal`, so a typed character
    /// reaches the buffer and `Ctrl+P` opens the editor's palette. Before
    /// this, focus stayed on the blurred dock's widget (it was still there,
    /// still focusable) and every key after a dive resolved in the `Dock`
    /// context and died.
    #[test]
    fn blurring_a_panel_moves_the_trees_focus_to_the_pane_behind_it() {
        use crate::input::keybindings::KeyContext;
        let (mut editor, _t) = make_editor();
        let panel_key = crate::widgets::PanelKey::new("test-plugin", 1);
        mount_list_panel(
            &mut editor,
            &panel_key,
            crate::app::PanelSlot::Dock.buffer_id(),
        );
        editor.dock = Some(dock_panel(panel_key.clone()));
        frame_the_shell(&mut editor);
        assert_eq!(editor.get_key_context(), KeyContext::Dock);
        let focused = editor
            .shell_ui
            .as_ref()
            .and_then(|ui| ui.focused())
            .and_then(|e| editor.shell_ui.as_ref().unwrap().key_of(e));
        assert_eq!(
            focused,
            Some(crate::view::shell::widgets::widget_focus_key("lst")),
            "the dock's focused widget holds the tree's focus"
        );

        editor.blur_floating_panel(crate::app::PanelSlot::Dock);
        assert!(
            editor.shell_description_stale,
            "a focus write marks the tree stale"
        );
        // The context is read off a tree laid out after the write.
        assert_eq!(editor.get_key_context(), KeyContext::Normal);
        let focused = editor
            .shell_ui
            .as_ref()
            .and_then(|ui| ui.focused())
            .and_then(|e| editor.shell_ui.as_ref().unwrap().key_of(e));
        let active = editor
            .active_window()
            .buffers
            .splits()
            .map(|(m, _)| m.active_split())
            .expect("a pane");
        assert_eq!(
            focused,
            Some(crate::view::shell::splits::content_key(active)),
            "the active pane's content is where focus rests"
        );
        assert!(editor.editor_base_owns_keyboard());

        // And a frame that changes nothing leaves it there.
        frame_the_shell(&mut editor);
        let focused = editor
            .shell_ui
            .as_ref()
            .and_then(|ui| ui.focused())
            .and_then(|e| editor.shell_ui.as_ref().unwrap().key_of(e));
        assert_eq!(
            focused,
            Some(crate::view::shell::splits::content_key(active))
        );
    }

    /// **A panel the tree does not hold advances along the tree's ring
    /// anyway.** A mounted but unfocused panel has no keyboard layer, so
    /// `Ui::move_focus` is not the move — but its interior is described, its
    /// wrappers are registered, and `Ui::next_in` walks them in the same
    /// order Tab would. The panel's focus *fact* advances along that ring;
    /// there is no second ring walked over the spec.
    #[test]
    fn a_panel_the_tree_does_not_hold_advances_along_the_trees_ring() {
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
            None,
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
            out.instance_states,
            out.focus_key,
            out.painted,
            out.boxes,
            true,
            false,
            false,
        );
        assert_eq!(
            editor.widget_registry.focus_key(&panel_key),
            Some("one"),
            "the render clamps focus onto the first tabbable"
        );
        // In the dock's slot, unfocused: the tree describes the interior
        // and its ring, and does not hold the panel's focus.
        let mut dock = dock_panel(panel_key.clone());
        dock.focused = false;
        editor.dock = Some(dock);
        frame_the_shell(&mut editor);

        editor.handle_widget_focus_advance(&panel_key, 1);
        assert_eq!(
            editor.widget_registry.focus_key(&panel_key),
            Some("two"),
            "the fact advanced along the tree's ring"
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
            None,
        );
        editor.widget_registry.mount(
            panel_key.clone(),
            crate::app::PanelSlot::Dock.buffer_id(),
            spec,
            out.instance_states,
            out.focus_key,
            out.painted,
            out.boxes,
            true,
            false,
            false,
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
            None,
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
            out.instance_states,
            out.focus_key,
            out.painted,
            out.boxes,
            true,
            false,
            false,
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
                    cols: 0,
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
            panel.boxes.is_empty() && panel.painted.is_empty(),
            "no text projection: {} boxes, {} painted windows",
            panel.boxes.len(),
            panel.painted.len()
        );
        assert_eq!(panel.focus_key, "lst", "the focus clamp still ran");
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

        // The same panel outside a slot is a pane's, and a pane's panel is
        // described too — there is no mounted panel the tree does not lay
        // out, so no re-render runs the collector.
        editor.rerender_widget_panel(&plain);
        let panel = editor.widget_registry.get(&plain).expect("the panel");
        assert!(
            panel.boxes.is_empty() && panel.painted.is_empty(),
            "a pane-mounted panel is the tree's as well: {} boxes, {} painted windows",
            panel.boxes.len(),
            panel.painted.len()
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
            Viewport {
                rows: 12,
                items: 3,
                cols: 0,
            },
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
                items: 12,
                cols: 0,
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
            None,
        );
        editor.widget_registry.mount(
            panel_key.clone(),
            crate::app::PanelSlot::Dock.buffer_id(),
            spec,
            out.instance_states,
            out.focus_key,
            out.painted,
            out.boxes,
            true,
            false,
            false,
        );
        editor.dock = Some(dock_panel(panel_key.clone()));
        frame_the_shell(&mut editor);

        let ui = editor.shell_ui.as_ref().expect("the tree");
        let one = ui.find_by_key(&crate::view::shell::widgets::widget_focus_key("one"));
        assert_eq!(ui.focused(), one, "the frame settled on the first widget");

        editor.set_panel_focus_and_notify(&panel_key, "two".to_string());
        assert!(
            editor.shell_description_stale,
            "a decision the tree has not seen makes the description stale"
        );
        frame_the_shell(&mut editor);

        let ui = editor.shell_ui.as_ref().expect("the tree");
        assert_eq!(
            ui.focused(),
            ui.find_by_key(&crate::view::shell::widgets::widget_focus_key("two")),
            "the frame carried the host's write, so the next Tab starts here"
        );
        assert!(!editor.shell_description_stale, "and the frame cleared it");
    }

    /// **The tree's own move stands until a decision says otherwise.**
    ///
    /// A Tab moves the tree's focus; the registry learns of it through the
    /// `WidgetFocus` echo at the next drain, and until then the description
    /// still marks the widget focus *left*. That stale mark is not a
    /// decision — it has not moved — so the frame that carries it changes
    /// nothing. This is what separates "the mark moved" from "the mark
    /// disagrees with focus", and it is the property the library's fourth
    /// autofocus case is built on.
    #[test]
    fn the_trees_own_move_is_not_undone_by_a_stale_mark() {
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
            None,
        );
        editor.widget_registry.mount(
            panel_key.clone(),
            crate::app::PanelSlot::Dock.buffer_id(),
            spec,
            out.instance_states,
            out.focus_key,
            out.painted,
            out.boxes,
            true,
            false,
            false,
        );
        editor.dock = Some(dock_panel(panel_key.clone()));
        frame_the_shell(&mut editor);

        // The ring moves on its own, and the echo is deliberately not
        // drained: the registry still says "one".
        let ui = editor.shell_ui.as_mut().expect("the tree");
        assert!(ui.move_focus(fresh_ui::FocusDir::Next));
        let two = ui.find_by_key(&crate::view::shell::widgets::widget_focus_key("two"));
        assert_eq!(ui.focused(), two);
        let _undrained = ui.take_messages();
        assert_eq!(editor.widget_registry.focus_key(&panel_key), Some("one"));

        frame_the_shell(&mut editor);
        let ui = editor.shell_ui.as_ref().expect("the tree");
        assert_eq!(
            ui.focused(),
            two,
            "a mark that has not moved does not pull focus back"
        );
    }

    /// **A decision on a panel the tree is not focused in lands when the
    /// panel is entered.** The registry's key is the only record of where
    /// focus will land; the tree cannot write a fact about a subtree it is
    /// not in, and does not try.
    #[test]
    fn a_decision_on_an_unfocused_panel_lands_when_it_is_entered() {
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
            None,
        );
        editor.widget_registry.mount(
            panel_key.clone(),
            crate::app::PanelSlot::Dock.buffer_id(),
            spec,
            out.instance_states,
            out.focus_key,
            out.painted,
            out.boxes,
            true,
            false,
            false,
        );
        let mut dock = dock_panel(panel_key.clone());
        dock.focused = false;
        editor.dock = Some(dock);
        frame_the_shell(&mut editor);

        editor.set_panel_focus_and_notify(&panel_key, "two".to_string());
        frame_the_shell(&mut editor);
        assert_eq!(editor.widget_registry.focus_key(&panel_key), Some("two"));

        editor.dock.as_mut().expect("the dock").focused = true;
        frame_the_shell(&mut editor);
        let ui = editor.shell_ui.as_ref().expect("the tree");
        assert_eq!(
            ui.focused(),
            ui.find_by_key(&crate::view::shell::widgets::widget_focus_key("two")),
            "entering the panel lands on the fact, not on the first control"
        );
    }

    /// **`autoFocusFirst: false` with nothing focused is a resting state the
    /// tree's entry landing must not overwrite.** The description marks
    /// nothing, so the tree rests on the scope's own element and the
    /// `WidgetFocus` echo has nothing to say.
    #[test]
    fn an_empty_focus_the_panel_asked_for_is_not_reseeded_by_entry() {
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
            false,
            None,
        );
        assert_eq!(out.focus_key, "", "nothing seeded");
        editor.widget_registry.mount(
            panel_key.clone(),
            crate::app::PanelSlot::Dock.buffer_id(),
            spec,
            out.instance_states,
            out.focus_key,
            out.painted,
            out.boxes,
            false,
            false,
            false,
        );
        editor.dock = Some(dock_panel(panel_key.clone()));
        frame_the_shell(&mut editor);
        editor.apply_settled_shell_messages();
        assert_eq!(
            editor.widget_registry.focus_key(&panel_key),
            Some(""),
            "the tree's landing did not name a widget the panel did not"
        );
    }

    /// **Two decisions in one batch resolve the second from the first.** A
    /// host focus write and a Tab with no frame between them: the Tab must
    /// move from where the write put focus, not from where the tree last
    /// saw it. `shell_dispatch` lays the tree out before routing when the
    /// description is stale, which is the rule that keeps this true.
    #[test]
    fn a_tab_in_the_same_batch_as_a_focus_write_moves_from_the_written_focus() {
        let (mut editor, _t) = make_editor();
        let panel_key = crate::widgets::PanelKey::new("test-plugin", 1);
        let spec = WidgetSpec::Col {
            children: vec![button("one"), button("two"), button("three")],
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
            None,
        );
        editor.widget_registry.mount(
            panel_key.clone(),
            crate::app::PanelSlot::Dock.buffer_id(),
            spec,
            out.instance_states,
            out.focus_key,
            out.painted,
            out.boxes,
            true,
            false,
            false,
        );
        editor.dock = Some(dock_panel(panel_key.clone()));
        frame_the_shell(&mut editor);
        editor.apply_settled_shell_messages();

        editor.set_panel_focus_and_notify(&panel_key, "two".to_string());
        let tab = fresh_ui::Input::Key(fresh_ui::KeyPress::new(fresh_ui::KeyCode::Tab));
        editor.shell_dispatch(tab);
        assert_eq!(
            editor.widget_registry.focus_key(&panel_key),
            Some("three"),
            "the Tab started from the written focus"
        );
        let ui = editor.shell_ui.as_ref().expect("the tree");
        assert_eq!(
            ui.focused(),
            ui.find_by_key(&crate::view::shell::widgets::widget_focus_key("three"))
        );
    }

    /// **A focus onto a widget the spec has only just grown (#3137).**
    ///
    /// The dock's "Move to Folder…" dropdown is two plugin writes: a spec
    /// carrying the option rows, then `setFocusKey` onto the first of them.
    /// Both are applied before the frame that describes either, so the
    /// imperative half of the focus mirror asked the tree for an element the
    /// tree could not have yet and quietly did nothing. The tree kept the
    /// focus the dropdown was supposed to take, and its `FocusGained` fact
    /// then wrote *that* back over the registry — so ↓ was routed to the
    /// session list underneath, which dismissed the popup, moved the
    /// selection and live-switched the workspace.
    ///
    /// The description carries the move; the frame that builds the element
    /// is the frame that lands on it, and nothing is held or replayed.
    #[test]
    fn a_focus_onto_a_widget_the_next_frame_builds_lands_on_that_frame() {
        let (mut editor, _t) = make_editor();
        let panel_key = crate::widgets::PanelKey::new("test-plugin", 1);
        // Rendering a spec into the registry exactly as mount/update do.
        let render = |spec: &WidgetSpec, prev_focus: &str| {
            super::render_floating_spec(
                false,
                spec,
                &Default::default(),
                &Default::default(),
                prev_focus,
                30,
                None,
                "",
                "",
                "",
                None,
                true,
                None,
            )
        };
        // The dock before the dropdown: a list, and nothing else to focus.
        let closed = WidgetSpec::Col {
            children: vec![button("sessions")],
            key: None,
        };
        let out = render(&closed, "");
        editor.widget_registry.mount(
            panel_key.clone(),
            crate::app::PanelSlot::Dock.buffer_id(),
            closed,
            out.instance_states,
            out.focus_key,
            out.painted,
            out.boxes,
            true,
            false,
            false,
        );
        editor.dock = Some(dock_panel(panel_key.clone()));
        frame_the_shell(&mut editor);
        let ui = editor.shell_ui.as_ref().expect("the tree");
        assert_eq!(
            ui.focused(),
            ui.find_by_key(&crate::view::shell::widgets::widget_focus_key("sessions")),
            "the frame settled on the list, which is all there was"
        );

        // The dropdown opens: the spec grows an option row, and the plugin
        // hands it the keyboard. Both land before the next frame.
        let open = WidgetSpec::Col {
            children: vec![
                button("sessions"),
                WidgetSpec::Overlay {
                    child: Box::new(WidgetSpec::Col {
                        children: vec![button("menu-pick:move:root")],
                        key: None,
                    }),
                    key: Some("move-menu".into()),
                },
            ],
            key: None,
        };
        let out = render(&open, "sessions");
        editor
            .widget_registry
            .update(
                &panel_key,
                open,
                out.instance_states,
                out.focus_key,
                out.painted,
                out.boxes,
            )
            .expect("the panel is mounted");
        editor.set_panel_focus_and_notify(&panel_key, "menu-pick:move:root".to_string());

        // The tree cannot carry it yet — the option row is not in the frame
        // that is currently built. Nothing is held: the description marks
        // the row, and the frame that builds it is the one that lands there.
        assert!(editor.shell_description_stale);

        frame_the_shell(&mut editor);

        let ui = editor.shell_ui.as_ref().expect("the tree");
        assert_eq!(
            ui.focused(),
            ui.find_by_key(&crate::view::shell::widgets::widget_focus_key(
                "menu-pick:move:root"
            )),
            "the dropdown owns the keyboard, so ↑/↓ drive it and not the list"
        );
    }
}
