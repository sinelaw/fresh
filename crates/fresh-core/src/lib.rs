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

/// Unique identifier for an editor `Session` — a project-rooted unit
/// of editor state (file tree, LSP set, splits, buffer set, …) that
/// the user can switch between as a whole. See
/// `docs/internal/conductor-sessions-design.md`.
///
/// Sessions are 1-indexed; the editor always boots with id=1 (the
/// "base" session) so the previous single-root behaviour is the
/// SessionId(1) special case. Ids are stable within a process and
/// monotonic — closing a session does not free its id.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize, TS)]
#[ts(export)]
pub struct SessionId(pub u64);

impl std::fmt::Display for SessionId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Session-{}", self.0)
    }
}
pub mod config;
pub mod file_explorer;
pub mod file_uri;
pub mod menu;
pub mod overlay;
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
    fn session_id_display_format() {
        assert_eq!(SessionId(1).to_string(), "Session-1");
        assert_eq!(SessionId(42).to_string(), "Session-42");
    }
}
