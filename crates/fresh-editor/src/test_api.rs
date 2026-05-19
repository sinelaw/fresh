//! Test-only observation API for the editor.
//!
//! The semantic test suite (under `tests/semantic/`) binds **only** to this
//! module — it must never reach into `crate::app::Editor`, `crate::model::*`,
//! or `crate::view::*` directly. That keeps the test/production contract
//! explicit and one-directional: production internals can be refactored
//! freely, the test API is the only thing that has to stay stable.
//!
//! See `docs/internal/e2e-test-migration-design.md` for the full rationale.
//!
//! # Layers
//!
//! Phase 2 (current) exposes only Class A — pure state observables:
//! `dispatch`, `dispatch_seq`, `buffer_text`, `primary_caret`, `carets`,
//! `selection_text`. Layout (`RenderSnapshot`) and styled-frame
//! (`StyledFrame`) observables are reserved for Phase 3+ and intentionally
//! not present here yet — adding them is a design decision that should be
//! made when the first theorem demanding them is written.
//!
//! # Determinism
//!
//! `carets()` returns cursors in ascending byte-position order so that
//! tests don't depend on `HashMap` iteration order (cursors are stored in
//! a hashmap internally).

// Re-export Action so semantic tests can `use fresh::test_api::Action`
// without reaching into `fresh::input::keybindings` directly. Keeping
// the action alphabet behind the test_api module is part of the
// one-directional contract documented in §2.1 of the design doc.
pub use crate::input::keybindings::Action;

/// A test-side projection of `crate::model::cursor::Cursor`.
///
/// Carries only the fields that semantic tests typically assert on
/// (position + selection anchor). Internal fields like `sticky_column`,
/// `deselect_on_move`, and `block_anchor` are intentionally hidden — if a
/// test needs them, the right fix is to extend this projection (with
/// review) rather than reach past it.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct Caret {
    /// Byte offset where edits happen.
    pub position: usize,
    /// Selection anchor, if a selection is active.
    pub anchor: Option<usize>,
}

/// Test-side projection of the editor's popup stack. Captures only
/// the fields scenario tests assert on — kind, title, items,
/// selection — so internal popup struct refactors don't break tests.
///
/// Two distinct modal channels live on the editor and both are
/// projected here:
///   - `top_popup` / `depth` come from the popup stacks
///     (`global_popups`, per-window `popups`) — completion,
///     hover, action, list, text overlays.
///   - `prompt` comes from `active_window().prompt` — the
///     minibuffer / floating-overlay prompts opened by actions
///     like `CommandPalette`, `QuickOpen`, `GotoLine`, `Search`,
///     `OpenLiveGrep`, `SaveAs`, `RecordMacro`, etc. These do not
///     live on the popup stacks; without projecting them, modal
///     scenarios that drive prompt flows pass by tautology.
#[derive(Debug, Clone, Default, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct ModalSnapshot {
    /// `None` ⇒ no popup visible on either popup stack.
    pub top_popup: Option<PopupView>,
    /// Popup-stack depth across both stacks (0 = no popups).
    /// Does NOT count an active prompt — see the `prompt` field
    /// for that.
    pub depth: usize,
    /// Active minibuffer prompt, if one is open. `None` ⇒ no
    /// prompt; the user is in normal editing mode.
    pub prompt: Option<PromptView>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct PopupView {
    /// Popup kind name: `"completion"`, `"hover"`, `"action"`,
    /// `"list"`, `"text"`. Stable strings (not the enum variant
    /// debug repr) so corpus JSON survives refactors.
    pub kind: String,
    pub title: Option<String>,
    /// List items as plain text. Empty for non-list popups.
    pub items: Vec<String>,
    pub selected_index: Option<usize>,
}

/// Test-side projection of an active minibuffer prompt.
///
/// `prompt_type` is the `PromptType` variant name as a stable
/// string (`"CommandPalette"`, `"QuickOpen"`, `"GotoLine"`,
/// `"Search"`, …) so corpus JSON survives variant renames in
/// production code. The runner inserts characters into the active
/// prompt via `Action::InsertChar` (the production input handler
/// routes those automatically when a prompt is open), so the
/// `input` / `cursor_pos` fields are the right level to assert on.
#[derive(Debug, Clone, Default, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct PromptView {
    pub prompt_type: String,
    pub input: String,
    pub cursor_pos: usize,
    /// Filtered suggestion texts shown to the user. Empty for
    /// prompts that don't filter (e.g. `GotoLine`).
    pub suggestions: Vec<String>,
    pub selected_suggestion: Option<usize>,
}

impl Caret {
    /// Cursor with no selection.
    pub fn at(position: usize) -> Self {
        Self {
            position,
            anchor: None,
        }
    }

    /// Cursor with a selection from `anchor` to `position`.
    /// Direction is preserved (anchor may be greater than position).
    pub fn range(anchor: usize, position: usize) -> Self {
        Self {
            position,
            anchor: Some(anchor),
        }
    }

    /// Sorted byte range covered by this caret's selection, if any.
    pub fn selection_range(&self) -> Option<std::ops::Range<usize>> {
        self.anchor.map(|a| {
            if a <= self.position {
                a..self.position
            } else {
                self.position..a
            }
        })
    }
}

/// The single observation surface for semantic theorem tests.
///
/// Implemented by `crate::app::Editor`. Tests obtain a
/// `&mut dyn EditorTestApi` from the test harness and never see the
/// underlying `Editor` type directly.
pub trait EditorTestApi {
    // ── Drive ────────────────────────────────────────────────────────────

    /// Apply a single semantic action, then drain async messages.
    fn dispatch(&mut self, action: Action);

    /// Apply a sequence of actions in order, draining async messages once
    /// at the end. Equivalent to calling `dispatch` per action but cheaper.
    fn dispatch_seq(&mut self, actions: &[Action]);

    // ── Class A: pure state observables ──────────────────────────────────

    /// Full buffer text. Panics if the buffer has unloaded regions
    /// (large-file mode); semantic theorems are not the right tool for
    /// large-file scenarios — write a layout/E2E test instead.
    fn buffer_text(&self) -> String;

    /// Primary cursor projected to a `Caret`.
    fn primary_caret(&self) -> Caret;

    /// All cursors projected to `Caret`s, sorted by ascending position.
    /// The primary cursor is included. Use `primary_caret()` if you only
    /// care about the primary; use `carets()` for multi-cursor theorems.
    fn carets(&self) -> Vec<Caret>;

    /// Concatenated selected text across all cursors, in ascending position
    /// order, joined by `\n`. Returns the empty string if no cursor has a
    /// selection.
    fn selection_text(&mut self) -> String;

    // ── Class B: layout observables ──────────────────────────────────────
    //
    // These reflect viewport state that is reconciled by the render
    // pipeline (`Viewport::ensure_visible_in_layout`), not by action
    // dispatch alone. The `LayoutTheorem` runner invokes
    // `EditorTestHarness::render` exactly once before reading them.
    //
    // This is intentionally a *thin* layout surface — just `top_byte`
    // for now. The `RenderSnapshot` design (see §9.1 of the migration
    // doc) is the right home for richer layout observables (gutter
    // spans, scrollbar geometry, hardware cursor row/col, popup
    // placement) and is reserved for a future expansion when a
    // theorem actually needs them.

    /// Byte offset of the first line currently visible in the active
    /// viewport. After the renderer has run, this is the viewport's
    /// scroll position. Without a render, this reflects the last
    /// reconciliation point.
    fn viewport_top_byte(&self) -> usize;

    /// Width of the active terminal in cells, as set at harness
    /// construction or via resize.
    fn terminal_width(&self) -> u16;

    /// Height of the active terminal in cells.
    fn terminal_height(&self) -> u16;

    /// Width of the line-number gutter in cells, computed from the
    /// active buffer's line count. Includes the trailing separator
    /// if the renderer adds one.
    fn gutter_width(&self) -> u16;

    /// Screen cell of the primary cursor, in `(col, row)`. None ⇒
    /// the cursor is off-screen (scrolled past). Requires a prior
    /// render to be meaningful.
    fn hardware_cursor_position(&mut self) -> Option<(u16, u16)>;

    /// `(start_byte, end_byte)` of the currently-visible buffer
    /// region. End is exclusive. None ⇒ unknown / not yet
    /// reconciled.
    fn visible_byte_range(&self) -> Option<(usize, usize)>;

    // ── Class C: modal observables (Phase 3) ─────────────────────────────

    /// Snapshot of the modal-popup stack visible to the user. Used
    /// by `ModalScenario` to assert on palette / picker / menu /
    /// completion state without screen scraping.
    fn modal_snapshot(&self) -> ModalSnapshot;

    // ── Class D: workspace observables (Phase 7) ─────────────────────────

    /// Number of buffers currently open across the workspace.
    fn buffer_count(&self) -> usize;

    /// Display path of the active buffer. None for unnamed buffers.
    fn active_buffer_path(&self) -> Option<String>;

    /// Display paths of every open buffer in stable insertion
    /// order. Unnamed buffers appear as `"<unnamed:NNN>"`.
    fn buffer_paths(&self) -> Vec<String>;

    // ── Class E: input dispatch (Phase 9) ────────────────────────────────

    /// Dispatch a mouse click projected through the active
    /// viewport. `(col, row)` are absolute screen coordinates;
    /// gutter offset is applied internally. Returns true if the
    /// editor consumed the event.
    fn dispatch_mouse_click(&mut self, col: u16, row: u16) -> bool;

    /// `true` if the active buffer has unsaved changes since it was
    /// last loaded from / saved to disk. The "save point" is the
    /// commit in the undo/redo log at which the buffer's on-disk
    /// representation matches its in-memory state. After loading a
    /// fresh file (no edits applied), this is `false`. After any
    /// edit it becomes `true`. Undoing back to the save point flips
    /// it back to `false` — the property under test in
    /// `tests/semantic/undo_redo.rs::theorem_undo_to_save_point_*`.
    fn is_modified(&self) -> bool;

    /// Consume the one-shot "full hardware redraw" flag the editor's
    /// event loop polls each frame: `Action::RedrawScreen` sets it,
    /// the loop's tick clears it. Returns the previous value and
    /// resets the flag to `false`.
    ///
    /// Exposed for `LayoutScenario`'s `expected_full_redraw_requested`
    /// assertion — the only stable observation surface for the
    /// "redraw screen" claim (issue #1070).
    fn take_full_redraw_request_for_tests(&mut self) -> bool;

    // ── Class F: marker observables (used by MarkerRoundtripScenario) ────
    //
    // Markers / line indicators track a byte position that survives
    // edits, undo, and redo. They have no `Action` projection — the
    // marker model is a production-internal thing — so the scenario
    // runner needs declarative setters/getters on the test API instead
    // of reaching into `editor.active_state_mut().margins`.

    /// Seed a line indicator (margin marker) at `byte_offset` with the
    /// given `symbol` (used as both the indicator glyph and the
    /// namespace) and a `color` name (`"red"`, `"green"`, `"blue"`,
    /// `"yellow"`; anything else falls back to `Color::Red`).
    fn seed_marker(&mut self, byte_offset: usize, symbol: &str, color: &str);

    /// Current byte positions of every marker whose namespace matches
    /// `symbol`, in ascending order. Empty if no marker was seeded for
    /// that namespace.
    fn marker_positions(&self, symbol: &str) -> Vec<usize>;

    /// Length (in events) of the active buffer's event log. Used by
    /// `PersistenceScenario` save-point claims that check no undo
    /// history is dropped on a no-op file-watcher notification.
    fn active_event_log_len(&self) -> usize;

    /// Notify the editor that `path` on disk has changed. Triggers the
    /// auto-revert path; the editor reads the file and updates its
    /// buffer (or skips when on-disk content already matches).
    fn notify_file_changed(&mut self, path: &str);

    // ── Class G: composite-buffer setup (used by diff layout scenarios) ──
    //
    // Side-by-side diff and unified-diff layout scenarios need a
    // composite buffer built from two virtual buffers + a hunk-derived
    // line alignment. Each accessor below is a stable, declarative
    // wrapper around the equivalent `Editor` / `Window` method so the
    // layout scenario runner can construct the composite from a data
    // spec (`CompositeDiffSpec`) without reaching into production
    // internals.

    /// Create a side-by-side composite diff view named `name` with
    /// mode `mode`. Builds two virtual buffers (OLD, NEW), seeds them
    /// with `old_content` / `new_content`, computes a line alignment
    /// from `hunks`, and switches the active buffer to the composite.
    /// Returns a 64-bit handle to the composite for follow-up calls
    /// (`composite_next_hunk_on`, `set_composite_initial_focus_hunk_on`).
    fn create_side_by_side_diff(
        &mut self,
        name: &str,
        mode: &str,
        old_content: &str,
        new_content: &str,
        hunks: &[(usize, usize, usize, usize)],
    ) -> usize;

    /// Set the composite buffer's `initial_focus_hunk` field — the
    /// one-shot "scroll to hunk N on first render" knob. The first
    /// render consumes the value and resets the field to `None`.
    fn set_composite_initial_focus_hunk_on(&mut self, composite_handle: usize, hunk_index: usize);

    /// Read the composite buffer's current `initial_focus_hunk`
    /// (after the first render this should be `None`, i.e. the field
    /// was consumed). Returns `None` for missing or non-composite
    /// buffers.
    fn composite_initial_focus_hunk_on(&self, composite_handle: usize) -> Option<usize>;

    /// Jump the active split's view of `composite_handle` to the next
    /// hunk. Mirrors the `n` / `]` keybinding semantics; returns
    /// `true` iff a next hunk existed.
    fn composite_next_hunk_active_on(&mut self, composite_handle: usize) -> bool;

    /// Jump back to the previous hunk; companion of
    /// `composite_next_hunk_active_on`.
    fn composite_prev_hunk_active_on(&mut self, composite_handle: usize) -> bool;

    /// Force-materialize composite view state across visible splits
    /// without performing a render. Lets a scenario reach hunk-nav
    /// state before any frame paints.
    fn flush_layout_for_tests(&mut self);
}

// ─────────────────────────────────────────────────────────────────────────
// Implementation on Editor.
//
// Implementation lives in this file (rather than next to Editor) so that
// the entire test-facing surface — trait + impl + projection types — is
// reviewable as one unit.
// ─────────────────────────────────────────────────────────────────────────

impl EditorTestApi for crate::app::Editor {
    fn dispatch(&mut self, action: Action) {
        // Routes through the same handle_action path the input layer
        // uses; dispatch_action_for_tests is the existing pub shim.
        self.dispatch_action_for_tests(action);
        let _ = self.process_async_messages();
    }

    fn dispatch_seq(&mut self, actions: &[Action]) {
        for a in actions {
            self.dispatch_action_for_tests(a.clone());
        }
        let _ = self.process_async_messages();
    }

    fn buffer_text(&self) -> String {
        self.active_state()
            .buffer
            .to_string()
            .expect("buffer_text(): buffer has unloaded regions; semantic tests do not support large-file mode")
    }

    fn primary_caret(&self) -> Caret {
        let c = self.active_cursors().primary();
        Caret {
            position: c.position,
            anchor: c.anchor,
        }
    }

    fn carets(&self) -> Vec<Caret> {
        let mut out: Vec<Caret> = self
            .active_cursors()
            .iter()
            .map(|(_, c)| Caret {
                position: c.position,
                anchor: c.anchor,
            })
            .collect();
        out.sort_by_key(|c| c.position);
        out
    }

    fn selection_text(&mut self) -> String {
        // Collect ranges first to avoid holding an immutable borrow of
        // `active_cursors` across the mutable `get_text_range` call.
        let mut ranges: Vec<std::ops::Range<usize>> = self
            .active_cursors()
            .iter()
            .filter_map(|(_, c)| c.selection_range())
            .collect();
        if ranges.is_empty() {
            return String::new();
        }
        ranges.sort_by_key(|r| r.start);

        let state = self.active_state_mut();
        let parts: Vec<String> = ranges
            .into_iter()
            .map(|r| state.get_text_range(r.start, r.end))
            .collect();
        parts.join("\n")
    }

    fn viewport_top_byte(&self) -> usize {
        self.active_viewport().top_byte
    }

    fn terminal_width(&self) -> u16 {
        self.active_viewport().width
    }

    fn terminal_height(&self) -> u16 {
        self.active_viewport().height
    }

    fn gutter_width(&self) -> u16 {
        let buffer = &self.active_state().buffer;
        u16::try_from(self.active_viewport().gutter_width(buffer)).unwrap_or(u16::MAX)
    }

    fn hardware_cursor_position(&mut self) -> Option<(u16, u16)> {
        // The viewport's `cursor_screen_position` requires
        // `&mut Buffer`. Cloning the viewport (cheap; mostly
        // primitives) lets us drop the immutable viewport borrow
        // before taking the mutable buffer borrow on the next
        // accessor call.
        let cursor = *self.active_cursors().primary();
        let viewport = self.active_viewport().clone();
        let viewport_height = viewport.height;
        let viewport_width = viewport.width;
        let buffer = &mut self.active_state_mut().buffer;
        let (col, row) = viewport.cursor_screen_position(buffer, &cursor);
        if row >= viewport_height || col >= viewport_width {
            None
        } else {
            Some((col, row))
        }
    }

    fn visible_byte_range(&self) -> Option<(usize, usize)> {
        // Viewport tracks `top_byte` exactly but the bottom of the
        // visible region depends on the wrapped view-line layout,
        // which only the renderer knows. Today we conservatively
        // return None until a future expansion plumbs the
        // last-visible byte through the test API.
        None
    }

    fn is_modified(&self) -> bool {
        self.active_state().buffer.is_modified()
    }

    fn take_full_redraw_request_for_tests(&mut self) -> bool {
        self.take_full_redraw_request()
    }

    fn modal_snapshot(&self) -> ModalSnapshot {
        // Two popup stacks live on the editor:
        // - `global_popups`: editor-wide modals (palette, file open, …)
        // - `active_state().popups`: per-buffer popups (completion, hover, …)
        // We return the topmost across both, choosing global first
        // since modal scenarios target the foreground stack.
        use crate::view::popup::{Popup, PopupContent, PopupKind};

        fn kind_name(kind: PopupKind) -> &'static str {
            match kind {
                PopupKind::Completion => "completion",
                PopupKind::Hover => "hover",
                PopupKind::Action => "action",
                PopupKind::List => "list",
                PopupKind::Text => "text",
            }
        }

        fn project(p: &Popup) -> PopupView {
            let (items, selected_index) = match &p.content {
                PopupContent::List { items, selected } => (
                    items.iter().map(|i| i.text.clone()).collect(),
                    Some(*selected),
                ),
                _ => (Vec::new(), None),
            };
            PopupView {
                kind: kind_name(p.kind).to_string(),
                title: p.title.clone(),
                items,
                selected_index,
            }
        }

        let global = self.global_popups.all();
        let local = &self.active_state().popups;
        let depth = global.len() + local.all().len();

        // `top()` of the global stack is highest-priority. Fall back
        // to per-buffer top if global is empty.
        let top = self
            .global_popups
            .top()
            .or_else(|| local.top())
            .map(project);

        // Project the active prompt (minibuffer / floating overlay).
        // Lives on `active_window().prompt`, not on the popup stacks.
        let prompt = self.active_window().prompt.as_ref().map(|p| PromptView {
            prompt_type: format!("{:?}", p.prompt_type),
            input: p.input.clone(),
            cursor_pos: p.cursor_pos,
            suggestions: p.suggestions.iter().map(|s| s.text.clone()).collect(),
            selected_suggestion: p.selected_suggestion,
        });

        ModalSnapshot {
            top_popup: top,
            depth,
            prompt,
        }
    }

    fn buffer_count(&self) -> usize {
        // `Editor::buffers` is the per-tab map; that's the count
        // the workspace surface advertises.
        self.buffer_count_for_tests()
    }

    fn active_buffer_path(&self) -> Option<String> {
        let id = self.active_buffer();
        let name = self.get_buffer_display_name(id);
        if name.is_empty() {
            None
        } else {
            Some(name)
        }
    }

    fn buffer_paths(&self) -> Vec<String> {
        self.all_buffer_ids_for_tests()
            .into_iter()
            .map(|id| {
                let name = self.get_buffer_display_name(id);
                if name.is_empty() {
                    format!("<unnamed:{}>", id.0)
                } else {
                    name
                }
            })
            .collect()
    }

    fn dispatch_mouse_click(&mut self, col: u16, row: u16) -> bool {
        use crossterm::event::{KeyModifiers, MouseButton, MouseEvent, MouseEventKind};
        let down = MouseEvent {
            kind: MouseEventKind::Down(MouseButton::Left),
            column: col,
            row,
            modifiers: KeyModifiers::NONE,
        };
        // Discard the down result; we only act on the up — but
        // explicitly use the value so clippy's
        // `let_underscore_must_use` is satisfied.
        if let Err(e) = self.handle_mouse(down) {
            tracing::trace!("mouse down errored in test dispatch: {e}");
        }
        let up = MouseEvent {
            kind: MouseEventKind::Up(MouseButton::Left),
            column: col,
            row,
            modifiers: KeyModifiers::NONE,
        };
        self.handle_mouse(up).unwrap_or(false)
    }

    fn seed_marker(&mut self, byte_offset: usize, symbol: &str, color: &str) {
        use crate::view::margin::LineIndicator;
        use ratatui::style::Color;
        let color = match color.to_ascii_lowercase().as_str() {
            "red" => Color::Red,
            "green" => Color::Green,
            "blue" => Color::Blue,
            "yellow" => Color::Yellow,
            "cyan" => Color::Cyan,
            "magenta" => Color::Magenta,
            "white" => Color::White,
            "black" => Color::Black,
            _ => Color::Red,
        };
        let indicator = LineIndicator::new(symbol, color, 10);
        let state = self.active_state_mut();
        let _ = state
            .margins
            .set_line_indicator(byte_offset, symbol.to_string(), indicator);
    }

    fn marker_positions(&self, symbol: &str) -> Vec<usize> {
        let margins = &self.active_state().margins;
        // Collect every marker whose namespace map contains `symbol`.
        // Iterate the indicator markers in the byte range covering the
        // whole buffer so positions come back sorted.
        let max = self
            .active_state()
            .buffer
            .to_string()
            .map(|s| s.len())
            .unwrap_or(usize::MAX);
        let mut out: Vec<usize> = margins
            .query_indicator_range(0, max.saturating_add(1))
            .into_iter()
            .filter_map(|(id, start, _end)| {
                // `id` is the MarkerId; look up whether `symbol` is one
                // of its namespaces via `get_indicator_position` for the
                // existence check plus a namespace-membership test.
                if margins.get_indicator_position(id).is_some()
                    && margins.namespaces_for_marker(id).iter().any(|n| n == symbol)
                {
                    Some(start)
                } else {
                    None
                }
            })
            .collect();
        out.sort_unstable();
        out
    }

    fn active_event_log_len(&self) -> usize {
        self.active_event_log().len()
    }

    fn notify_file_changed(&mut self, path: &str) {
        self.handle_file_changed(path);
    }

    fn create_side_by_side_diff(
        &mut self,
        name: &str,
        mode: &str,
        old_content: &str,
        new_content: &str,
        hunks: &[(usize, usize, usize, usize)],
    ) -> usize {
        use crate::model::composite_buffer::{
            CompositeLayout, DiffHunk, LineAlignment, PaneStyle, SourcePane,
        };
        use crate::primitives::text_property::TextPropertyEntry;

        let old_buffer_id = self.active_window_mut().create_virtual_buffer(
            "OLD".to_string(),
            "text".to_string(),
            true,
        );
        self.set_virtual_buffer_content(
            old_buffer_id,
            vec![TextPropertyEntry::text(old_content)],
        )
        .expect("seed OLD virtual buffer");

        let new_buffer_id = self.active_window_mut().create_virtual_buffer(
            "NEW".to_string(),
            "text".to_string(),
            true,
        );
        self.set_virtual_buffer_content(
            new_buffer_id,
            vec![TextPropertyEntry::text(new_content)],
        )
        .expect("seed NEW virtual buffer");

        let sources = vec![
            SourcePane::new(old_buffer_id, "OLD", false).with_style(PaneStyle::old_diff()),
            SourcePane::new(new_buffer_id, "NEW", false).with_style(PaneStyle::new_diff()),
        ];
        let layout = CompositeLayout::SideBySide {
            ratios: vec![0.5, 0.5],
            show_separator: true,
        };
        let composite_id =
            self.create_composite_buffer(name.to_string(), mode.to_string(), layout, sources);

        let hunk_vec: Vec<DiffHunk> = hunks
            .iter()
            .map(|(os, oc, ns, nc)| DiffHunk::new(*os, *oc, *ns, *nc))
            .collect();
        let old_line_count = old_content.lines().count();
        let new_line_count = new_content.lines().count();
        let alignment = LineAlignment::from_hunks(&hunk_vec, old_line_count, new_line_count);
        self.active_window_mut()
            .set_composite_alignment(composite_id, alignment);

        self.switch_buffer(composite_id);

        composite_id.0
    }

    fn set_composite_initial_focus_hunk_on(&mut self, composite_handle: usize, hunk_index: usize) {
        use crate::model::event::BufferId;
        let id = BufferId(composite_handle);
        if let Some(c) = self.active_window_mut().get_composite_mut(id) {
            c.initial_focus_hunk = Some(hunk_index);
        }
    }

    fn composite_initial_focus_hunk_on(&self, composite_handle: usize) -> Option<usize> {
        use crate::model::event::BufferId;
        let id = BufferId(composite_handle);
        self.active_window().get_composite(id).and_then(|c| c.initial_focus_hunk)
    }

    fn composite_next_hunk_active_on(&mut self, composite_handle: usize) -> bool {
        use crate::model::event::BufferId;
        let id = BufferId(composite_handle);
        self.active_window_mut().composite_next_hunk_active(id)
    }

    fn composite_prev_hunk_active_on(&mut self, composite_handle: usize) -> bool {
        use crate::model::event::BufferId;
        let id = BufferId(composite_handle);
        self.active_window_mut().composite_prev_hunk_active(id)
    }

    fn flush_layout_for_tests(&mut self) {
        self.flush_layout();
    }
}
