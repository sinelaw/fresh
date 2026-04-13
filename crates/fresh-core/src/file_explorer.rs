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

#[cfg(all(test, feature = "plugins"))]
mod tests {
    use super::*;
    use rquickjs::{Context, FromJs, Runtime, Value};

    /// `FileExplorerDecoration::from_js` reads every decoration field, not
    /// just returning a defaulted stub. Uses non-zero priority and a theme
    /// key colour to tie down the full conversion.
    #[test]
    fn from_js_decodes_all_visible_fields() {
        let rt = Runtime::new().unwrap();
        let ctx = Context::full(&rt).unwrap();
        ctx.with(|ctx| {
            let v: Value = ctx
                .eval::<Value, _>(
                    b"({path: '/tmp/a.rs', symbol: 'M', \
                       color: 'ui.file_status_added_fg', priority: 7})"
                        .as_slice(),
                )
                .unwrap();
            let got = FileExplorerDecoration::from_js(&ctx, v).unwrap();
            assert_eq!(got.path, PathBuf::from("/tmp/a.rs"));
            assert_eq!(got.symbol, "M");
            assert_eq!(got.priority, 7);
            assert_eq!(got.color.as_theme_key(), Some("ui.file_status_added_fg"));
        });
    }
}
