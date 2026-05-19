//! `Observable` trait + observable types for every scenario.
//!
//! Every scenario type is parametrised by what it asserts on. The
//! `Observable` trait pairs each observable type with a function
//! that extracts it from a live editor harness — that's the same
//! plumbing shadow models use to compare against the editor.
//!
//! Observable types are pure data: they `Serialize` + `Deserialize`,
//! they `PartialEq`, and they have no internal references to editor
//! state. This is what makes corpus scenarios replay-able and
//! shadow-comparable.

use crate::common::harness::EditorTestHarness;
use crate::common::scenario::property::BufferState;
use serde::{Deserialize, Serialize};

/// Anything a scenario runner can extract from a harness and assert
/// equality on.
///
/// Implementors typically hold the data they care about (cursors,
/// snapshot, modal state, etc.) and live in this module so the
/// extraction recipe sits next to the type definition.
pub trait Observable: Sized + PartialEq + std::fmt::Debug {
    /// Pull the observable's value out of a live harness. Called
    /// once per scenario, after every `InputEvent` has been
    /// dispatched.
    ///
    /// `&mut` because some observables (selection text, snapshot)
    /// require mutable access through `EditorTestApi`.
    fn extract(harness: &mut EditorTestHarness) -> Self;
}

// ─────────────────────────────────────────────────────────────────────
// BufferState — Phase 1; canonical pure-state observable.
// ─────────────────────────────────────────────────────────────────────

impl Observable for BufferState {
    fn extract(harness: &mut EditorTestHarness) -> Self {
        let api = harness.api_mut();
        BufferState {
            buffer_text: api.buffer_text(),
            primary: api.primary_caret(),
            all_carets: api.carets(),
            selection_text: api.selection_text(),
        }
    }
}

// ─────────────────────────────────────────────────────────────────────
// Pair observables for cross-cutting scenarios
// ─────────────────────────────────────────────────────────────────────

impl<A: Observable, B: Observable> Observable for (A, B) {
    fn extract(harness: &mut EditorTestHarness) -> Self {
        let a = A::extract(harness);
        let b = B::extract(harness);
        (a, b)
    }
}

// ─────────────────────────────────────────────────────────────────────
// Modal observable (Phase 3)
// ─────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct ModalState {
    /// `None` ⇒ no popup visible.
    pub top_popup: Option<PopupSnapshot>,
    /// Popup stack depth (top is index 0).
    pub depth: usize,
    /// Active minibuffer prompt, or `None` if no prompt is open.
    /// Mirrors `ModalSnapshot.prompt` from the editor test API. The
    /// runner asserts on this so scenarios that drive prompt flows
    /// (`CommandPalette` + filter + confirm) actually observe the
    /// state change rather than passing by tautology.
    pub prompt: Option<PromptSnapshot>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct PopupSnapshot {
    pub kind: String,
    pub title: Option<String>,
    pub items: Vec<String>,
    pub selected_index: Option<usize>,
    pub query: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct PromptSnapshot {
    pub prompt_type: String,
    pub input: String,
    pub cursor_pos: usize,
    pub suggestions: Vec<String>,
    pub selected_suggestion: Option<usize>,
}

impl Observable for ModalState {
    fn extract(harness: &mut crate::common::harness::EditorTestHarness) -> Self {
        let snap = harness.api_mut().modal_snapshot();
        ModalState {
            depth: snap.depth,
            top_popup: snap.top_popup.map(|p| PopupSnapshot {
                kind: p.kind,
                title: p.title,
                items: p.items,
                selected_index: p.selected_index,
                query: None,
            }),
            prompt: snap.prompt.map(|p| PromptSnapshot {
                prompt_type: p.prompt_type,
                input: p.input,
                cursor_pos: p.cursor_pos,
                suggestions: p.suggestions,
                selected_suggestion: p.selected_suggestion,
            }),
        }
    }
}

// ─────────────────────────────────────────────────────────────────────
// Workspace observable (Phase 7)
// ─────────────────────────────────────────────────────────────────────

/// Observed workspace state at a single point in time.
///
/// This is the value the runner extracts from the harness and
/// compares against the test's expectations. Tests should not
/// construct `WorkspaceState` directly as their expectation —
/// use `WorkspaceExpect` (below) so the wildcard vs. specific
/// match for each field is explicit.
#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct WorkspaceState {
    pub buffer_count: usize,
    pub active_buffer_path: Option<String>,
    /// Open buffer paths in stable order. Ordering matters for tab
    /// reordering tests.
    pub buffer_paths: Vec<String>,
}

impl Observable for WorkspaceState {
    fn extract(harness: &mut crate::common::harness::EditorTestHarness) -> Self {
        let api = harness.api_mut();
        WorkspaceState {
            buffer_count: api.buffer_count(),
            active_buffer_path: api.active_buffer_path(),
            buffer_paths: api.buffer_paths(),
        }
    }
}

/// Expectation for the workspace's active buffer path.
///
/// Path matching is suffix-based because the harness uses a temp
/// project directory whose absolute prefix isn't known to the
/// test author. `EndsWith("foo.txt")` matches both
/// `/tmp/xxx/foo.txt` and `/tmp/yyy/sub/foo.txt`.
#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum ActivePathExpect {
    /// Don't check the active buffer path. Must be explicit — there
    /// is no implicit wildcard from leaving a field unset.
    #[default]
    Any,
    /// Must have no active buffer (e.g. after closing the last one).
    None_,
    /// Must have an active buffer whose path ends with the given
    /// suffix.
    EndsWith(String),
}

/// Expectation for the open buffer-list.
#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum BufferPathsExpect {
    /// Don't check the list. Must be explicit.
    #[default]
    Any,
    /// Each entry of the expected vec must be a suffix of the
    /// actual entry at the same index. Vec length must match
    /// exactly. Ordering matters.
    EndsWithInOrder(Vec<String>),
}

/// What a test asserts about the workspace at the end of an event
/// sequence. Distinct from `WorkspaceState` so callers must make
/// every field's match policy explicit.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct WorkspaceExpect {
    /// Required exact buffer count.
    pub buffer_count: usize,
    pub active_buffer_path: ActivePathExpect,
    pub buffer_paths: BufferPathsExpect,
}

// ─────────────────────────────────────────────────────────────────────
// Filesystem observable (Phase 6)
// ─────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct FsState {
    /// Path → expected on-disk content. Only paths the scenario
    /// asserts on need to be listed; the runner reads each.
    pub expected_files: std::collections::BTreeMap<String, String>,
}

// ─────────────────────────────────────────────────────────────────────
// LSP traffic observable (Phase 5)
// ─────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct LspTraffic {
    /// Methods the editor sent to the server, in order.
    pub client_methods: Vec<String>,
    /// Notifications the server (fake) emitted, in order.
    pub server_notifications: Vec<String>,
}

// ─────────────────────────────────────────────────────────────────────
// Round-trip terminal grid (Phase 8)
// ─────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct RoundTripGrid {
    pub width: u16,
    pub height: u16,
    /// One string per visible row, after the editor's emitted ANSI
    /// has been parsed back through `vt100`. Trailing spaces are
    /// preserved so column-precise assertions work.
    pub rows: Vec<String>,
    pub hardware_cursor: Option<(u16, u16)>,
}

impl Observable for RoundTripGrid {
    fn extract(harness: &mut EditorTestHarness) -> Self {
        // Render through the real CrosstermBackend → ANSI → vt100
        // pipeline so the grid reflects what a terminal emulator
        // would actually display, not the abstract `ratatui::Buffer`.
        let _ = harness.render_real();
        let screen = harness.vt100_screen_to_string();
        let rows: Vec<String> = screen.split('\n').map(|s| s.to_string()).collect();
        let height = rows.len() as u16;
        let width = rows.iter().map(|r| r.chars().count()).max().unwrap_or(0) as u16;
        let hardware_cursor = harness.vt100_cursor_position();
        RoundTripGrid {
            width,
            height,
            rows,
            hardware_cursor,
        }
    }
}

// ─────────────────────────────────────────────────────────────────────
// Styled frame (Phase 4)
// ─────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct StyledFrame {
    pub width: u16,
    pub height: u16,
    pub cells: Vec<StyledCell>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct StyledCell {
    pub row: u16,
    pub col: u16,
    pub symbol: String,
    pub fg: Option<String>, // hex or palette name
    pub bg: Option<String>,
    pub role: CellRole,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum CellRole {
    #[default]
    Normal,
    Selection,
    Cursor,
    LineNumber,
    Decoration,
    Whitespace,
}
