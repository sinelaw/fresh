//! `ScenarioContext` and the per-subsystem context types.
//!
//! Every scenario carries a context describing the editor's
//! *initial* environment — the buffer text, behavior flags,
//! optional workspace state, optional virtual filesystem, optional
//! scripted LSP exchange, etc.
//!
//! Subsystem fields are `Option<...>`. A buffer-only scenario
//! carries only `BufferContext`. A scenario that needs LSP carries
//! both. The runner picks them up only when present.

use crate::common::scenario::buffer_scenario::{BehaviorFlags, TerminalSize};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::PathBuf;
use std::time::Duration;

/// Every scenario at minimum carries this. Mirrors the fields
/// already on `BufferScenario` so a lifted scenario with no extra
/// context is structurally identical.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct BufferContext {
    pub initial_text: String,
    #[serde(default)]
    pub behavior: BehaviorFlags,
    #[serde(default)]
    pub language: Option<String>,
    #[serde(default)]
    pub terminal: TerminalSize,
}

/// Optional subsystem contexts. None of these are wired through the
/// runner yet for scenario types whose phase is still skeletal —
/// each `unimplemented!()` site names the production hook needed.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ScenarioContext {
    pub buffer: BufferContext,

    /// Phase 7. Populated when the scenario asserts on splits, tabs,
    /// or buffer-list state.
    #[serde(default)]
    pub workspace: Option<WorkspaceContext>,

    /// Phase 6. In-memory filesystem; the editor's FS adapter routes
    /// reads/writes through this when present.
    #[serde(default)]
    pub fs: Option<VirtualFs>,

    /// Phase 5. Scripted client↔server exchange for the fake LSP.
    #[serde(default)]
    pub lsp: Option<LspScript>,

    /// Phase 4. Theme to feed the `style()` projection. None ⇒ the
    /// editor's default theme at construction time.
    #[serde(default)]
    pub theme: Option<ThemeRef>,

    /// Phase 10. None ⇒ the system clock; Some ⇒ a `MockClock` seeded
    /// at this epoch that advances only on `InputEvent::AdvanceClock`.
    #[serde(default)]
    pub clock: Option<MockClock>,
}

// ─────────────────────────────────────────────────────────────────────
// Workspace (Phase 7)
// ─────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct WorkspaceContext {
    /// Initial set of buffers to open, in order. The first becomes
    /// the active buffer.
    pub initial_buffers: Vec<NamedBuffer>,
    /// Initial split layout. None ⇒ single buffer view.
    #[serde(default)]
    pub initial_splits: Option<SplitLayout>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct NamedBuffer {
    pub filename: String,
    pub content: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
#[derive(Default)]
pub enum SplitLayout {
    #[default]
    Single,
    Horizontal {
        left: Box<SplitLayout>,
        right: Box<SplitLayout>,
    },
    Vertical {
        top: Box<SplitLayout>,
        bottom: Box<SplitLayout>,
    },
}

// ─────────────────────────────────────────────────────────────────────
// Virtual filesystem (Phase 6)
// ─────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct VirtualFs {
    /// Files present at scenario start, keyed by absolute path.
    pub files: BTreeMap<PathBuf, VirtualFile>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct VirtualFile {
    pub content: String,
    #[serde(default)]
    pub mode: Option<u32>,
    #[serde(default)]
    pub mtime_unix_secs: Option<i64>,
}

// ─────────────────────────────────────────────────────────────────────
// LSP (Phase 5)
// ─────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct LspScript {
    /// Server name (`rust-analyzer`, `pyright`, …) the fake will
    /// announce. Affects nothing semantically; useful for
    /// scenario-specific routing.
    pub server: String,
    /// Pre-scripted exchanges in order. The runner asserts each
    /// expected client message arrives, then injects the
    /// corresponding server reply.
    pub exchanges: Vec<LspExchange>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LspExchange {
    /// JSON-RPC method or `notifications/<X>` the editor is expected
    /// to send next. Wildcard `"*"` accepts any.
    pub expect_method: String,
    /// Optional JSON-shape constraint on the params.
    #[serde(default)]
    pub expect_params: Option<serde_json::Value>,
    /// Reply or notification to inject after observing the expected
    /// client message. None ⇒ no reply (notifications etc.).
    #[serde(default)]
    pub server_reply: Option<LspIncoming>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct LspIncoming {
    pub method: String,
    pub params: serde_json::Value,
}

// ─────────────────────────────────────────────────────────────────────
// Theme (Phase 4)
// ─────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
#[derive(Default)]
pub enum ThemeRef {
    #[default]
    Default,
    Named(String),
    HighContrast,
}

// ─────────────────────────────────────────────────────────────────────
// Clock (Phase 10)
// ─────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct MockClock {
    /// Initial offset from the unix epoch in milliseconds.
    pub epoch_ms: u64,
}

// ─────────────────────────────────────────────────────────────────────
// Modal (Phase 3) — referenced by InputEvent variants
// ─────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum PromptKind {
    CommandPalette,
    FileOpen,
    FileSaveAs,
    Goto,
    LiveGrep,
    Search,
    Settings,
    Custom(String),
}

// ─────────────────────────────────────────────────────────────────────
// Mouse (Phase 9)
// ─────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum MouseEvent {
    Click {
        row: u16,
        col: u16,
        button: MouseButton,
    },
    Drag {
        from_row: u16,
        from_col: u16,
        to_row: u16,
        to_col: u16,
        button: MouseButton,
    },
    Wheel {
        row: u16,
        col: u16,
        dy: i16,
    },
    DoubleClick {
        row: u16,
        col: u16,
    },
    TripleClick {
        row: u16,
        col: u16,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum MouseButton {
    Left,
    Right,
    Middle,
}

// ─────────────────────────────────────────────────────────────────────
// Wait conditions
// ─────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum WaitCondition {
    PopupVisible,
    PopupHidden,
    BufferModified(bool),
    LspDiagnosticsPublished,
    /// Bounded semantic timeout — the runner times out after this
    /// duration of mock-clock advance, *not* wall-clock.
    Timeout(Duration),
}
