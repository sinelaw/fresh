use serde::{Deserialize, Serialize};
use std::path::PathBuf;
use ts_rs::TS;

/// Decoration metadata for a file explorer entry.
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
pub struct FileExplorerDecoration {
    pub path: PathBuf,
    pub symbol: String,
    pub color: (u8, u8, u8),
    pub priority: i32,
}
