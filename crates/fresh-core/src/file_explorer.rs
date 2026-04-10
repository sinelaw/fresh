use crate::api::OverlayColorSpec;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;
use ts_rs::TS;

/// Decoration metadata for a file explorer entry.
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(deny_unknown_fields)]
#[ts(export)]
pub struct FileExplorerDecoration {
    /// File path to decorate
    #[ts(type = "string")]
    pub path: PathBuf,
    /// Symbol to display (e.g., "●", "M", "A")
    pub symbol: String,
    /// Color as RGB array or theme key string (e.g., "ui.file_status_added_fg")
    pub color: OverlayColorSpec,
    /// Priority for display when multiple decorations exist (higher wins)
    #[serde(default)]
    pub priority: i32,
}

#[cfg(feature = "plugins")]
impl<'js> rquickjs::FromJs<'js> for FileExplorerDecoration {
    fn from_js(_ctx: &rquickjs::Ctx<'js>, value: rquickjs::Value<'js>) -> rquickjs::Result<Self> {
        rquickjs_serde::from_value(value).map_err(|e| rquickjs::Error::FromJs {
            from: "object",
            to: "FileExplorerDecoration",
            message: Some(e.to_string()),
        })
    }
}
