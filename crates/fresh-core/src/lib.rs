use serde::{Deserialize, Serialize};

use ts_rs::TS;

/// Unique identifier for a cursor
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, TS)]
#[ts(export)]
pub struct CursorId(pub usize);

impl CursorId {
    /// Sentinel value used for inverse events during undo/redo
    /// This indicates that the event shouldn't move any cursor
    pub const UNDO_SENTINEL: CursorId = CursorId(usize::MAX);
}

/// Unique identifier for a split pane (leaf or container)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, TS)]
#[ts(export)]
pub struct SplitId(pub usize);

/// A split pane that displays a buffer (leaf node in the split tree)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct LeafId(pub SplitId);

impl From<LeafId> for SplitId {
    fn from(id: LeafId) -> Self {
        id.0
    }
}

/// A split container that holds two children (internal node in the split tree)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ContainerId(pub SplitId);

impl From<ContainerId> for SplitId {
    fn from(id: ContainerId) -> Self {
        id.0
    }
}

/// Unique identifier for a buffer
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(transparent)]
#[derive(TS)]
#[ts(export)]
pub struct BufferId(pub usize);

/// Direction of a split
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, TS)]
#[ts(export)]
pub enum SplitDirection {
    Horizontal,
    Vertical,
}

pub mod action;
pub mod api;
pub mod command;
pub mod hooks;

/// Unique identifier for a terminal session
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize, TS)]
#[ts(export)]
pub struct TerminalId(pub usize);

impl std::fmt::Display for TerminalId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Terminal-{}", self.0)
    }
}

/// Unique identifier for an editor `Window` — a project-rooted unit
/// of editor state (file tree, LSP set, splits, buffer set, …) that
/// the user can switch between as a whole. Modelled on a VS Code
/// window. See `docs/internal/orchestrator-sessions-design.md`.
///
/// Windows are 1-indexed; the editor always boots with id=1 (the
/// "base" window) so the previous single-root behaviour is the
/// WindowId(1) special case. Ids are stable within a process and
/// monotonic — closing a window does not free its id.
///
/// Note on naming: Orchestrator presents windows as "agent sessions"
/// in its UX (matching the parallel-agents domain language), but
/// internally the editor calls them windows to disambiguate from
/// Fresh's pre-existing workspace-recovery and config-layer
/// "session" concepts.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize, TS)]
#[ts(export)]
pub struct WindowId(pub u64);

impl std::fmt::Display for WindowId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Window-{}", self.0)
    }
}

/// A terminal identified across the whole editor.
///
/// `TerminalId`s are only unique *within* their owning window's
/// `TerminalManager` — every window numbers its terminals from 0, so two
/// windows each have a `Terminal-0`. Any layer that resolves a terminal
/// without already holding its window — most importantly the async
/// PTY-output messages routed through the main loop — must carry this
/// `(window, terminal)` pair. Resolving by bare `TerminalId` across
/// windows is ambiguous: it silently attributes output to whichever
/// window happens to hold the same local id first.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct WindowTerminalId {
    pub window: WindowId,
    pub terminal: TerminalId,
}

impl WindowTerminalId {
    pub fn new(window: WindowId, terminal: TerminalId) -> Self {
        Self { window, terminal }
    }
}

impl std::fmt::Display for WindowTerminalId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}/{}", self.window, self.terminal)
    }
}

pub mod config;
pub mod display_width;
pub mod file_explorer;
pub mod file_uri;
pub mod menu;
pub mod overlay;
pub mod plugin_schemas;
pub mod services;
pub mod text_property;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn terminal_id_display_format() {
        assert_eq!(TerminalId(0).to_string(), "Terminal-0");
        assert_eq!(TerminalId(42).to_string(), "Terminal-42");
    }

    #[test]
    fn window_id_display_format() {
        assert_eq!(WindowId(1).to_string(), "Window-1");
        assert_eq!(WindowId(42).to_string(), "Window-42");
    }
}
