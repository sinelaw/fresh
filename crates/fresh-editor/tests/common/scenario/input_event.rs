//! `InputEvent` — top-level alphabet for every scenario type.
//!
//! `Action` is the editor's existing 600-variant input alphabet for
//! semantic actions ("MoveLineEnd", "ToUpperCase", "Undo"). It's the
//! right level for `BufferScenario`s, but other scenario types need
//! events that aren't expressible as `Action`s — mouse clicks,
//! prompt navigation, clock ticks, scripted LSP injections, etc.
//!
//! Rather than expand `Action` (which is also the production
//! input-layer enum and would force production code to learn about
//! test-only events), we wrap `Action` in a superset that carries
//! the test-side variants alongside.
//!
//! Each non-`Action` variant is dispatched by the runner of the
//! scenario type that needs it. For variants whose required
//! production hook isn't built yet, the runner panics with an
//! explicit "Phase N not yet implemented: needs <hook>" message —
//! the variant exists in the data model so the corpus shape is
//! stable, but no scenario can dispatch it until the hook lands.

use serde::{Deserialize, Serialize};
use std::path::PathBuf;
use std::time::Duration;

use crate::common::scenario::context::{LspIncoming, MouseEvent, PromptKind, WaitCondition};

/// Serde-friendly mirror of `crossterm::event::KeyCode`, restricted
/// to the variants scenarios actually need. The runner maps this
/// onto `crossterm::event::KeyCode` before calling
/// `EditorTestHarness::send_key`.
///
/// Modelled as a plain enum (not wrapping the crossterm type)
/// because crossterm's `KeyCode` doesn't implement `Serialize` /
/// `Deserialize` and we need scenarios to round-trip through JSON
/// for the corpus replay infrastructure.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "kind", content = "data", rename_all = "snake_case")]
pub enum KeySpec {
    Char(char),
    Backspace,
    Enter,
    Left,
    Right,
    Up,
    Down,
    Home,
    End,
    PageUp,
    PageDown,
    Tab,
    BackTab,
    Delete,
    Insert,
    Esc,
}

/// Serde-friendly mirror of `crossterm::event::KeyModifiers`. Each
/// field is a boolean for the corresponding modifier bit; the
/// runner combines them into the bitflag before dispatch.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct KeyMods {
    #[serde(default)]
    pub ctrl: bool,
    #[serde(default)]
    pub shift: bool,
    #[serde(default)]
    pub alt: bool,
}

impl KeyMods {
    pub const NONE: Self = Self {
        ctrl: false,
        shift: false,
        alt: false,
    };
    pub const SHIFT: Self = Self {
        ctrl: false,
        shift: true,
        alt: false,
    };
    pub const CTRL: Self = Self {
        ctrl: true,
        shift: false,
        alt: false,
    };
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "kind", content = "data", rename_all = "snake_case")]
pub enum InputEvent {
    /// The original editor alphabet — everything covered by
    /// `BufferScenario` is built from these.
    Action(fresh::test_api::Action),

    // ── Modal scenarios (Phase 3) ─────────────────────────────────
    OpenPrompt(PromptKind),
    FilterPrompt(String),
    ConfirmPrompt,
    CancelPrompt,
    MenuSelect(usize),

    // ── Input scenarios (Phase 9) ─────────────────────────────────
    Mouse(MouseEvent),
    /// Multi-step IME composition seq, e.g. dead-key + base char.
    Compose(Vec<char>),

    /// Raw keystroke routed through the production `handle_key`
    /// path (the same path `EditorTestHarness::send_key` drives).
    /// Use this for tests whose subject is the keybinding layer
    /// itself — e.g. `normalize_key` SHIFT-stripping for
    /// `Shift+Backspace` (issue #1588). Action-level dispatch
    /// bypasses `normalize_key`, so it can't detect regressions
    /// there.
    SendKey {
        code: KeySpec,
        modifiers: KeyMods,
    },

    // ── Temporal scenarios (Phase 10) ─────────────────────────────
    AdvanceClock(Duration),

    // ── LSP scenarios (Phase 5) ───────────────────────────────────
    LspMessage(LspIncoming),

    // ── Persistence scenarios (Phase 6) ───────────────────────────
    FsExternalEdit {
        path: PathBuf,
        content: String,
    },
    /// Notify the editor's file-watcher path that `path` on disk has
    /// changed, **without** mutating the file. Used by save-point
    /// scenarios that need to assert the auto-revert handler does
    /// *not* clobber the undo log when on-disk content already
    /// matches the buffer (issue #191 follow-up). Distinct from
    /// `FsExternalEdit` which writes-then-notifies. The path is
    /// resolved relative to the scenario's temp root, matching the
    /// `FsExternalEdit` convention.
    EditorFileChangedReaction {
        path: String,
    },

    // ── Inline assertions (used by step-walking persistence flows) ─
    /// Pin the buffer text mid-stream. Lets persistence scenarios
    /// assert state at multiple points along a Save-As/Undo path
    /// without abandoning the data shape.
    AssertBufferText(String),
    /// Pin `EditorTestApi::is_modified` mid-stream. The core
    /// observable for issue #191 / save-point invariants.
    AssertIsModified(bool),
    /// Pin the active buffer's event-log length. Used by the file-
    /// watcher scenarios that assert a no-op notification does not
    /// clear undo history.
    AssertEventLogLen(usize),
    /// Pin "primary caret byte ≤ max" mid-stream. Issue #191
    /// specifically called out cursor going to byte 0 on Undo —
    /// asserting `<= max` rather than `== exact` matches the e2e
    /// test's intent (cursor must stay within text bounds, exact
    /// post-Undo position is implementation-defined).
    AssertPrimaryCursorAtMost(usize),

    // ── Prompt input (used by Save-As + similar prompt flows) ─────
    /// Open the SaveAs prompt by dispatching `Action::SaveAs`. The
    /// runner additionally asserts the prompt actually opened.
    OpenSaveAsPrompt,
    /// Backspace `count` characters in the active prompt. Routed
    /// through the production key handler. Used defensively to clear
    /// a prompt pre-populated with the buffer's current path before
    /// `PromptFillText` types in the new path.
    PromptBackspace {
        count: usize,
    },
    /// Type literal characters into the active prompt via
    /// `EditorTestHarness::type_text`, which routes each char
    /// through `handle_key` (so prompt input handlers see them).
    PromptFillText(String),
    /// Type the absolute path `<temp_root>/<rel>` into the active
    /// prompt. The runner resolves `rel` against the scenario's
    /// temp root, which is only known at runtime. Save-As prompts
    /// need an absolute path; without this variant the scenario
    /// data would have to embed a per-run absolute path.
    PromptFillTempPath {
        rel: String,
    },
    /// Confirm the active prompt with Enter, routed through the
    /// production key handler so the prompt receives the same
    /// `handle_key` it would in production.
    PromptConfirm,

    // ── Async / settle ────────────────────────────────────────────
    /// Wait for a *semantic* condition (popup appears, LSP
    /// publishes diagnostics, save completes). Never a wall-clock
    /// sleep — the condition is asserted on observable state.
    Wait(WaitCondition),

    // ── Composite-buffer hunk navigation (LayoutScenario) ─────────
    /// Jump to the next hunk in the scenario's composite buffer
    /// `count` times. The scenario must have `composite_buffer` set;
    /// the handle is resolved from the scenario context. Mirrors the
    /// `n` / `]` keybinding semantics that
    /// `composite_next_hunk_active` exposes.
    CompositeNextHunk {
        count: u16,
    },
    /// Companion of `CompositeNextHunk` — jumps back. `p` / `[`.
    CompositePrevHunk {
        count: u16,
    },
    /// Force-materialize composite view state for all visible splits
    /// without rendering. Mirrors `Editor::flush_layout`; lets a
    /// scenario reach hunk-nav state before the first frame paints.
    FlushLayout,
    /// Sleep `ms` milliseconds (wall-clock). Used by the scrollbar
    /// drag tests to clear the editor's double-click detection
    /// window between drags. The default harness config sets
    /// `double_click_time_ms = 10`, so a 25-30ms sleep is safe.
    SleepMs(u64),
    /// Record the current rendered rows under `slot`; a later
    /// `AssertRenderedRowsMatch { slot }` asserts that the rows at
    /// that point equal the slot's recording. Used by tests like
    /// "scrolling on the left pane and the right pane produces the
    /// same view" that need to compare two grids.
    RecordRenderedRows {
        slot: u32,
    },
    /// Assert that the current rendered rows equal the rows
    /// previously stored under `slot` (via `RecordRenderedRows`).
    AssertRenderedRowsMatch {
        slot: u32,
    },
}

impl From<fresh::test_api::Action> for InputEvent {
    fn from(a: fresh::test_api::Action) -> Self {
        InputEvent::Action(a)
    }
}
