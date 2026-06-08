use std::collections::HashMap;
use std::path::{Path, PathBuf};

use super::cache::{build_bubbled_cache, insert_with_aliases};
use super::slots::{
    ExplorerSlotContext, ExplorerTooltipSummary, ExplorerTrailingSlotPayload,
    ExplorerTrailingSlotProvider, ExplorerTrailingSlotResolution,
    COMPATIBILITY_TRAILING_SLOT_HIT_WIDTH,
};
use crate::view::theme::Theme;
use ratatui::style::Color;

// Re-export from fresh-core for shared type usage
pub use fresh_core::file_explorer::FileExplorerDecoration;

#[derive(Debug, Clone, Copy)]
pub enum ResolvedExplorerStatus<'a> {
    Unsaved,
    Decoration(&'a FileExplorerDecoration),
    BubbledDecoration(&'a FileExplorerDecoration),
}

#[derive(Debug, Clone, Copy)]
pub struct ExplorerRowStatus<'a> {
    resolved: Option<ResolvedExplorerStatus<'a>>,
}

impl<'a> ExplorerRowStatus<'a> {
    pub fn resolve(
        path: &Path,
        is_dir: bool,
        has_unsaved: bool,
        decorations: &'a FileExplorerDecorationCache,
    ) -> Self {
        Self {
            resolved: resolve_explorer_status(path, is_dir, has_unsaved, decorations),
        }
    }

    pub fn resolved(&self) -> Option<ResolvedExplorerStatus<'a>> {
        self.resolved
    }

    pub fn compatibility_trailing_slot(
        &self,
        theme: &Theme,
        is_dir: bool,
    ) -> Option<ExplorerTrailingSlotPayload> {
        let (text, fg) = match self.resolved {
            Some(ResolvedExplorerStatus::Unsaved) => ("●".to_string(), theme.diagnostic_warning_fg),
            Some(ResolvedExplorerStatus::Decoration(decoration)) => (
                decoration_symbol(&decoration.symbol),
                compatibility_decoration_color(decoration, theme),
            ),
            Some(ResolvedExplorerStatus::BubbledDecoration(decoration)) => (
                "●".to_string(),
                compatibility_decoration_color(decoration, theme),
            ),
            None => return None,
        };

        Some(ExplorerTrailingSlotPayload {
            text,
            fg,
            tooltip: self.tooltip_summary(is_dir),
        })
    }

    pub fn tooltip_summary(&self, is_dir: bool) -> Option<ExplorerTooltipSummary> {
        let mut lines = Vec::new();

        match self.resolved {
            Some(ResolvedExplorerStatus::Unsaved) => {
                if is_dir {
                    lines.push("● - Contains unsaved changes".to_string());
                } else {
                    lines.push("● - Unsaved changes in editor".to_string());
                }
            }
            Some(ResolvedExplorerStatus::Decoration(decoration)) => {
                lines.push(format!(
                    "{} - {}",
                    decoration_symbol(&decoration.symbol),
                    decoration_tooltip(decoration)
                ));
            }
            Some(ResolvedExplorerStatus::BubbledDecoration(_)) => {
                lines.push("● - Contains modified files".to_string());
            }
            None => return None,
        }

        Some(ExplorerTooltipSummary {
            title: "Git Status".to_string(),
            lines,
        })
    }
}

pub struct CompatibilityTrailingSlotProvider;

pub static COMPATIBILITY_TRAILING_SLOT_PROVIDER: CompatibilityTrailingSlotProvider =
    CompatibilityTrailingSlotProvider;

impl ExplorerTrailingSlotProvider for CompatibilityTrailingSlotProvider {
    fn resolve(&self, context: &ExplorerSlotContext<'_>) -> ExplorerTrailingSlotResolution {
        let row_status = ExplorerRowStatus::resolve(
            context.path,
            context.is_dir,
            context.has_unsaved,
            context.decorations,
        );

        ExplorerTrailingSlotResolution {
            payload: row_status.compatibility_trailing_slot(context.theme, context.is_dir),
            name_color_hint: None,
        }
    }

    fn hit_test_width(&self) -> u16 {
        COMPATIBILITY_TRAILING_SLOT_HIT_WIDTH
    }
}

/// Cached decoration lookups for file explorer rendering.
#[derive(Debug, Default, Clone)]
pub struct FileExplorerDecorationCache {
    direct: HashMap<PathBuf, FileExplorerDecoration>,
    bubbled: HashMap<PathBuf, FileExplorerDecoration>,
}

impl FileExplorerDecorationCache {
    /// Rebuild the cache from a list of decorations.
    ///
    /// `symlink_mappings` maps symlink paths to their canonical targets.
    /// This allows decorations on canonical paths to also appear under symlink aliases.
    pub fn rebuild<I>(
        decorations: I,
        root: &Path,
        symlink_mappings: &HashMap<PathBuf, PathBuf>,
    ) -> Self
    where
        I: IntoIterator<Item = FileExplorerDecoration>,
    {
        let mut direct = HashMap::new();
        for decoration in decorations {
            if !decoration.path.starts_with(root) {
                continue;
            }
            insert_with_aliases(
                &mut direct,
                &decoration.path,
                &decoration,
                symlink_mappings,
                |map, path, mut decoration| {
                    decoration.path = path;
                    insert_best(map, decoration);
                },
            );
        }

        let bubbled = build_bubbled_cache(
            &direct,
            root,
            |map, _path, decoration| insert_best(map, decoration),
            |ancestor, decoration| FileExplorerDecoration {
                path: ancestor.to_path_buf(),
                symbol: decoration.symbol.clone(),
                color: decoration.color.clone(),
                priority: decoration.priority,
            },
        );

        Self { direct, bubbled }
    }

    /// Lookup a decoration for an exact path.
    pub fn direct_for_path(&self, path: &Path) -> Option<&FileExplorerDecoration> {
        self.direct.get(path)
    }

    /// Lookup a bubbled decoration for a path (direct or descendant).
    pub fn bubbled_for_path(&self, path: &Path) -> Option<&FileExplorerDecoration> {
        self.bubbled.get(path)
    }

    /// Direct decoration paths under `dir_path`, excluding `dir_path` itself.
    pub fn direct_paths_under(&self, dir_path: &Path) -> Vec<PathBuf> {
        let mut paths: Vec<PathBuf> = self
            .direct
            .keys()
            .filter(|path| path_is_strict_child_of(path, dir_path))
            .cloned()
            .collect();
        paths.sort();
        paths
    }
}

fn path_is_strict_child_of(child: &Path, parent: &Path) -> bool {
    if child == parent {
        return false;
    }
    if child.starts_with(parent) {
        return true;
    }

    // Git and the filesystem can disagree on macOS (/var vs /private/var).
    match (child.canonicalize(), parent.canonicalize()) {
        (Ok(child), Ok(parent)) => child.starts_with(&parent) && child != parent,
        _ => false,
    }
}

pub fn resolve_explorer_status<'a>(
    path: &Path,
    is_dir: bool,
    has_unsaved: bool,
    decorations: &'a FileExplorerDecorationCache,
) -> Option<ResolvedExplorerStatus<'a>> {
    if has_unsaved {
        return Some(ResolvedExplorerStatus::Unsaved);
    }

    if let Some(decoration) = decorations.direct_for_path(path) {
        return Some(ResolvedExplorerStatus::Decoration(decoration));
    }

    if is_dir {
        if let Some(decoration) = decorations.bubbled_for_path(path) {
            return Some(ResolvedExplorerStatus::BubbledDecoration(decoration));
        }
    }

    None
}

fn insert_best(
    map: &mut HashMap<PathBuf, FileExplorerDecoration>,
    decoration: FileExplorerDecoration,
) {
    let replace = match map.get(&decoration.path) {
        Some(existing) => decoration.priority >= existing.priority,
        None => true,
    };

    if replace {
        map.insert(decoration.path.clone(), decoration);
    }
}

pub fn compatibility_decoration_color(decoration: &FileExplorerDecoration, theme: &Theme) -> Color {
    match &decoration.color {
        fresh_core::api::OverlayColorSpec::Rgb(r, g, b) => Color::Rgb(*r, *g, *b),
        fresh_core::api::OverlayColorSpec::ThemeKey(key) => {
            theme.resolve_theme_key(key).unwrap_or(theme.editor_fg)
        }
    }
}

pub fn decoration_symbol(symbol: &str) -> String {
    symbol
        .chars()
        .next()
        .map(|c| c.to_string())
        .unwrap_or_else(|| " ".to_string())
}

pub fn decoration_tooltip(decoration: &FileExplorerDecoration) -> &'static str {
    match decoration.symbol.as_str() {
        "U" => "Untracked - File is not tracked by git",
        "M" if is_staged_modified_decoration(decoration) => "Modified - File has staged changes",
        "M" => "Modified - File has unstaged changes",
        "A" => "Added - File is staged for commit",
        "D" => "Deleted - File is staged for deletion",
        "R" => "Renamed - File has been renamed",
        "C" => "Copied - File has been copied",
        "!" => "Conflicted - File has merge conflicts",
        "●" => "Has changes - Contains modified files",
        _ => "Unknown status",
    }
}

fn is_staged_modified_decoration(decoration: &FileExplorerDecoration) -> bool {
    matches!(
        &decoration.color,
        fresh_core::api::OverlayColorSpec::ThemeKey(key)
            if key == "ui.file_status_added_fg"
    ) && decoration.symbol == "M"
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn resolves_unsaved_before_plugin_decoration() {
        let path = PathBuf::from("/repo/file.rs");
        let decorations = FileExplorerDecorationCache::rebuild(
            vec![FileExplorerDecoration {
                path: path.clone(),
                symbol: "M".to_string(),
                color: fresh_core::api::OverlayColorSpec::ThemeKey(
                    "ui.file_status_modified_fg".into(),
                ),
                priority: 50,
            }],
            Path::new("/repo"),
            &HashMap::new(),
        );

        let resolved = resolve_explorer_status(&path, false, true, &decorations);
        assert!(matches!(resolved, Some(ResolvedExplorerStatus::Unsaved)));
    }

    #[test]
    fn resolves_direct_decoration() {
        let path = PathBuf::from("/repo/file.rs");
        let decorations = FileExplorerDecorationCache::rebuild(
            vec![FileExplorerDecoration {
                path: path.clone(),
                symbol: "P".to_string(),
                color: fresh_core::api::OverlayColorSpec::ThemeKey(
                    "ui.file_status_added_fg".into(),
                ),
                priority: 99,
            }],
            Path::new("/repo"),
            &HashMap::new(),
        );

        let resolved = resolve_explorer_status(&path, false, false, &decorations);
        assert!(matches!(
            resolved,
            Some(ResolvedExplorerStatus::Decoration(decoration)) if decoration.symbol == "P"
        ));
    }

    #[test]
    fn lists_direct_paths_under_directory_in_sorted_order() {
        let cache = FileExplorerDecorationCache::rebuild(
            vec![
                FileExplorerDecoration {
                    path: PathBuf::from("/repo/src/zeta.ts"),
                    symbol: "M".to_string(),
                    color: fresh_core::api::OverlayColorSpec::ThemeKey(
                        "ui.file_status_modified_fg".into(),
                    ),
                    priority: 50,
                },
                FileExplorerDecoration {
                    path: PathBuf::from("/repo/src/nested/alpha.ts"),
                    symbol: "A".to_string(),
                    color: fresh_core::api::OverlayColorSpec::ThemeKey(
                        "ui.file_status_added_fg".into(),
                    ),
                    priority: 60,
                },
                FileExplorerDecoration {
                    path: PathBuf::from("/repo/README.md"),
                    symbol: "M".to_string(),
                    color: fresh_core::api::OverlayColorSpec::ThemeKey(
                        "ui.file_status_modified_fg".into(),
                    ),
                    priority: 50,
                },
            ],
            Path::new("/repo"),
            &HashMap::new(),
        );

        assert_eq!(
            cache.direct_paths_under(Path::new("/repo/src")),
            vec![
                PathBuf::from("/repo/src/nested/alpha.ts"),
                PathBuf::from("/repo/src/zeta.ts"),
            ]
        );
    }

    #[test]
    fn decoration_tooltip_treats_git_explorer_staged_modified_as_staged() {
        let decoration = FileExplorerDecoration {
            path: PathBuf::from("/repo/file.rs"),
            symbol: "M".to_string(),
            color: fresh_core::api::OverlayColorSpec::ThemeKey("ui.file_status_added_fg".into()),
            priority: 52,
        };

        assert_eq!(
            decoration_tooltip(&decoration),
            "Modified - File has staged changes"
        );
    }
}
