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

/// Unique identifier for a split pane
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, TS)]
#[ts(export)]
pub struct SplitId(pub usize);

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
pub mod config;
pub mod file_explorer;
pub mod menu;
pub mod overlay;
pub mod services;
pub mod text_property;
