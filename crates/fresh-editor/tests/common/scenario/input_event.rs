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
    SendKey { code: KeySpec, modifiers: KeyMods },

    // ── Temporal scenarios (Phase 10) ─────────────────────────────
    AdvanceClock(Duration),

    // ── LSP scenarios (Phase 5) ───────────────────────────────────
    LspMessage(LspIncoming),

    // ── Persistence scenarios (Phase 6) ───────────────────────────
    FsExternalEdit {
        path: PathBuf,
        content: String,
    },

    // ── Async / settle ────────────────────────────────────────────
    /// Wait for a *semantic* condition (popup appears, LSP
    /// publishes diagnostics, save completes). Never a wall-clock
    /// sleep — the condition is asserted on observable state.
    Wait(WaitCondition),
}

impl From<fresh::test_api::Action> for InputEvent {
    fn from(a: fresh::test_api::Action) -> Self {
        InputEvent::Action(a)
    }
}
