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

fn default_leading_slot_min_width() -> usize {
    2
}

/// Tooltip content shown when hovering a trailing file-explorer slot.
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[ts(export)]
pub struct FileExplorerTooltip {
    /// Tooltip title shown in the popup border.
    pub title: String,
    /// Body lines shown inside the popup.
    pub lines: Vec<String>,
}

/// Leading-slot content for a file explorer row.
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[ts(export)]
pub struct FileExplorerLeadingSlot {
    /// Text shown in the leading slot (for example, an icon glyph).
    pub text: String,
    /// Foreground colour for the leading slot.
    pub color: OverlayColorSpec,
    /// Minimum display width reserved for the leading slot.
    #[serde(default = "default_leading_slot_min_width")]
    pub min_width: usize,
}

/// Where a path-independent leading-slot rule gets its foreground colour.
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(untagged, deny_unknown_fields)]
#[ts(export)]
pub enum FileExplorerLeadingRuleColor {
    /// A fixed RGB value or theme key, resolved when the row is painted.
    Color(OverlayColorSpec),
    /// Inherit the final filename foreground after selection, path overrides,
    /// hidden/symlink styling, and pending-cut dimming are applied.
    Filename {
        source: FileExplorerLeadingRuleColorSource,
    },
}

/// Supported dynamic colour sources for a leading-slot rule.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename_all = "camelCase")]
pub enum FileExplorerLeadingRuleColorSource {
    Filename,
}

/// Leading-slot payload stored in a path-independent rule table.
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[ts(export)]
pub struct FileExplorerLeadingRuleSlot {
    /// Text shown in the leading slot (typically one icon glyph).
    pub text: String,
    /// Fixed colour or inheritance from the row's final filename colour.
    pub color: FileExplorerLeadingRuleColor,
    /// Minimum display width reserved for the leading slot.
    #[serde(default)]
    #[ts(optional)]
    pub min_width: Option<usize>,
}

/// Path-independent leading-slot rules registered by a plugin namespace.
///
/// Rules are normalized and compiled by the host at registration time. At
/// render time the host considers exact filenames, final extensions, and
/// directory basenames before consulting the relevant fallback.
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[ts(export)]
pub struct FileExplorerLeadingSlotRules {
    /// Namespace priority. Higher values win within the explicit tier or the
    /// fallback tier; explicit matches always beat every fallback.
    #[serde(default)]
    #[ts(optional)]
    pub priority: Option<i32>,
    /// Match keys case-sensitively. Defaults to false.
    #[serde(default)]
    #[ts(optional)]
    pub case_sensitive: Option<bool>,
    /// Exact file basenames, for example `.gitignore` or `Dockerfile`.
    #[serde(default)]
    #[ts(optional)]
    pub exact_files: Option<std::collections::HashMap<String, FileExplorerLeadingRuleSlot>>,
    /// Final extensions without a leading dot, for example `rs` or `tar`.
    #[serde(default)]
    #[ts(optional)]
    pub extensions: Option<std::collections::HashMap<String, FileExplorerLeadingRuleSlot>>,
    /// Exact directory basenames.
    #[serde(default)]
    #[ts(optional)]
    pub directory_names: Option<std::collections::HashMap<String, FileExplorerLeadingRuleSlot>>,
    /// Used for files only when no explicit rule from any namespace matched.
    #[serde(default)]
    #[ts(optional)]
    pub fallback_file: Option<FileExplorerLeadingRuleSlot>,
    /// Used for directories only when no explicit rule from any namespace matched.
    #[serde(default)]
    #[ts(optional)]
    pub fallback_directory: Option<FileExplorerLeadingRuleSlot>,
}

/// Trailing-slot content for a file explorer row.
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[ts(export)]
pub struct FileExplorerTrailingSlot {
    /// Text shown in the trailing slot (for example, a badge glyph).
    pub text: String,
    /// Foreground colour for the trailing slot.
    pub color: OverlayColorSpec,
    /// Optional tooltip shown when hovering the trailing slot.
    #[serde(default)]
    pub tooltip: Option<FileExplorerTooltip>,
}

/// Additive slot override for a file explorer entry.
///
/// Any field left as `None` falls back to the editor's compatibility providers,
/// so plugins can override just the piece they care about.
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
#[ts(export)]
pub struct FileExplorerSlotEntry {
    /// File or directory path to override.
    #[ts(type = "string")]
    pub path: PathBuf,
    /// Optional leading-slot override.
    #[serde(default)]
    pub leading: Option<FileExplorerLeadingSlot>,
    /// Explicitly suppress the compatibility leading slot for this path.
    #[serde(default)]
    pub suppress_leading: bool,
    /// Optional trailing-slot override.
    #[serde(default)]
    pub trailing: Option<FileExplorerTrailingSlot>,
    /// Explicitly suppress the compatibility trailing slot for this path.
    #[serde(default)]
    pub suppress_trailing: bool,
    /// Optional filename colour override.
    #[serde(default)]
    pub name_color: Option<OverlayColorSpec>,
    /// Explicitly suppress compatibility filename colouring for this path.
    #[serde(default)]
    pub suppress_name_color: bool,
    /// Priority for display when multiple overrides exist (higher wins).
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

#[cfg(feature = "plugins")]
impl<'js> rquickjs::FromJs<'js> for FileExplorerSlotEntry {
    fn from_js(_ctx: &rquickjs::Ctx<'js>, value: rquickjs::Value<'js>) -> rquickjs::Result<Self> {
        rquickjs_serde::from_value(value).map_err(|e| rquickjs::Error::FromJs {
            from: "object",
            to: "FileExplorerSlotEntry",
            message: Some(e.to_string()),
        })
    }
}

#[cfg(feature = "plugins")]
impl<'js> rquickjs::FromJs<'js> for FileExplorerLeadingSlotRules {
    fn from_js(_ctx: &rquickjs::Ctx<'js>, value: rquickjs::Value<'js>) -> rquickjs::Result<Self> {
        rquickjs_serde::from_value(value).map_err(|e| rquickjs::Error::FromJs {
            from: "object",
            to: "FileExplorerLeadingSlotRules",
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

    #[test]
    fn slot_entry_from_js_decodes_suppression_flags() {
        let rt = Runtime::new().unwrap();
        let ctx = Context::full(&rt).unwrap();
        ctx.with(|ctx| {
            let v: Value = ctx
                .eval::<Value, _>(
                    br#"({
                        path: '/tmp/a.rs',
                        suppressLeading: true,
                        suppressTrailing: true,
                        suppressNameColor: true,
                        priority: 5
                    })"#,
                )
                .unwrap();
            let got = FileExplorerSlotEntry::from_js(&ctx, v).unwrap();
            assert_eq!(got.path, PathBuf::from("/tmp/a.rs"));
            assert!(got.suppress_leading);
            assert!(got.suppress_trailing);
            assert!(got.suppress_name_color);
            assert_eq!(got.priority, 5);
        });
    }

    #[test]
    fn leading_rules_from_js_decodes_selectors_and_filename_color() {
        let rt = Runtime::new().unwrap();
        let ctx = Context::full(&rt).unwrap();
        ctx.with(|ctx| {
            let v: Value = ctx
                .eval::<Value, _>(
                    br#"({
                        priority: 7,
                        exactFiles: {
                            '.gitignore': { text: 'G', color: 'syntax.keyword' }
                        },
                        extensions: {
                            rs: { text: 'R', color: { source: 'filename' }, minWidth: 1 }
                        },
                        fallbackDirectory: { text: 'D', color: [1, 2, 3] }
                    })"#,
                )
                .unwrap();
            let got = FileExplorerLeadingSlotRules::from_js(&ctx, v).unwrap();
            assert_eq!(got.priority, Some(7));
            assert_eq!(got.case_sensitive, None);
            assert!(got.exact_files.as_ref().unwrap().contains_key(".gitignore"));
            assert!(matches!(
                got.extensions.as_ref().unwrap()["rs"].color,
                FileExplorerLeadingRuleColor::Filename {
                    source: FileExplorerLeadingRuleColorSource::Filename
                }
            ));
            assert_eq!(got.extensions.as_ref().unwrap()["rs"].min_width, Some(1));
            assert!(matches!(
                got.fallback_directory.unwrap().color,
                FileExplorerLeadingRuleColor::Color(OverlayColorSpec::Rgb(1, 2, 3))
            ));
        });
    }
}
