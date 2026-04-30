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

use crate::input::keybindings::Action;

/// A test-side projection of `crate::model::cursor::Cursor`.
///
/// Carries only the fields that semantic tests typically assert on
/// (position + selection anchor). Internal fields like `sticky_column`,
/// `deselect_on_move`, and `block_anchor` are intentionally hidden — if a
/// test needs them, the right fix is to extend this projection (with
/// review) rather than reach past it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Caret {
    /// Byte offset where edits happen.
    pub position: usize,
    /// Selection anchor, if a selection is active.
    pub anchor: Option<usize>,
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
}
