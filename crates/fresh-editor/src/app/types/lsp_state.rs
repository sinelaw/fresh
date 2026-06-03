use crate::services::async_bridge::LspMessageType;

/// LSP progress information
#[derive(Debug, Clone)]
pub struct LspProgressInfo {
    pub language: String,
    pub title: String,
    pub message: Option<String>,
    pub percentage: Option<u32>,
}

// `LspMenuItem` lives in `fresh_core::api` (re-exported as
// `crate::app::LspMenuItem` for editor-internal use). See its docstring
// there for the full design — it's both the plugin-command payload
// and the internal storage type.
pub use fresh_core::api::LspMenuItem;

/// LSP message entry (for window messages and logs)
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct LspMessageEntry {
    pub language: String,
    pub message_type: LspMessageType,
    pub message: String,
    pub timestamp: std::time::Instant,
}
