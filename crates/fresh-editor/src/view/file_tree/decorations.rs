use std::collections::HashMap;
use std::path::{Path, PathBuf};

use super::cache::{build_bubbled_cache, insert_with_aliases};
use super::git_status::{FileExplorerGitStatus, FileExplorerGitStatusCache};
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
    Git(FileExplorerGitStatus),
    BubbledDecoration(&'a FileExplorerDecoration),
    BubbledGit(FileExplorerGitStatus),
}

#[derive(Debug, Clone, Copy)]
pub struct ExplorerRowStatus<'a> {
    // The badge/tooltip owner after precedence resolution. This can be plugin
    // decoration state, native git state, or a bubbled directory summary.
    resolved: Option<ResolvedExplorerStatus<'a>>,
    // The git state that should color the filename when git-name coloring is
    // enabled. This intentionally stays separate from `resolved` so plugin
    // badges can coexist with native git filename colors.
    effective_git_status: Option<FileExplorerGitStatus>,
}

impl<'a> ExplorerRowStatus<'a> {
    pub fn resolve(
        path: &Path,
        is_dir: bool,
        has_unsaved: bool,
        decorations: &'a FileExplorerDecorationCache,
        git_statuses: &'a FileExplorerGitStatusCache,
    ) -> Self {
        let resolved =
            resolve_explorer_status(path, is_dir, has_unsaved, decorations, git_statuses);
        let effective_git_status =
            resolve_effective_git_status(path, is_dir, has_unsaved, git_statuses);

        Self {
            resolved,
            effective_git_status,
        }
    }

    pub fn resolved(&self) -> Option<ResolvedExplorerStatus<'a>> {
        self.resolved
    }

    pub fn effective_git_status(&self) -> Option<FileExplorerGitStatus> {
        self.effective_git_status
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
            Some(ResolvedExplorerStatus::Git(status)) => (
                status.symbol().to_string(),
                compatibility_git_status_name_color(status, theme),
            ),
            Some(ResolvedExplorerStatus::BubbledDecoration(decoration)) => (
                "●".to_string(),
                compatibility_decoration_color(decoration, theme),
            ),
            Some(ResolvedExplorerStatus::BubbledGit(status)) => (
                "●".to_string(),
                compatibility_git_status_name_color(status, theme),
            ),
            None => return None,
        };

        Some(ExplorerTrailingSlotPayload {
            text,
            fg,
            tooltip: self.tooltip_summary(is_dir),
        })
    }

    pub fn compatibility_name_color_hint(
        &self,
        theme: &Theme,
        color_git_status_names: bool,
    ) -> Option<Color> {
        if !color_git_status_names {
            return None;
        }

        self.effective_git_status
            .map(|status| compatibility_git_status_name_color(status, theme))
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
            Some(ResolvedExplorerStatus::Git(status)) => {
                lines.push(format!("{} - {}", status.symbol(), status.kind.tooltip()));
            }
            Some(ResolvedExplorerStatus::Decoration(decoration)) => {
                lines.push(format!(
                    "{} - {}",
                    decoration_symbol(&decoration.symbol),
                    decoration_tooltip(decoration)
                ));
            }
            Some(ResolvedExplorerStatus::BubbledDecoration(_))
            | Some(ResolvedExplorerStatus::BubbledGit(_)) => {
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
            context.git_statuses,
        );

        ExplorerTrailingSlotResolution {
            payload: row_status.compatibility_trailing_slot(context.theme, context.is_dir),
            name_color_hint: row_status
                .compatibility_name_color_hint(context.theme, context.color_git_status_names),
        }
    }

    fn hit_test_width(&self) -> u16 {
        COMPATIBILITY_TRAILING_SLOT_HIT_WIDTH
    }
}

fn resolve_effective_git_status(
    path: &Path,
    is_dir: bool,
    has_unsaved: bool,
    git_statuses: &FileExplorerGitStatusCache,
) -> Option<FileExplorerGitStatus> {
    if has_unsaved {
        return None;
    }

    git_statuses.direct_for_path(path).or_else(|| {
        is_dir
            .then(|| git_statuses.bubbled_for_path(path))
            .flatten()
    })
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
}

pub fn resolve_explorer_status<'a>(
    path: &Path,
    is_dir: bool,
    has_unsaved: bool,
    decorations: &'a FileExplorerDecorationCache,
    git_statuses: &'a FileExplorerGitStatusCache,
) -> Option<ResolvedExplorerStatus<'a>> {
    // Precedence is:
    // 1. Unsaved editor state
    // 2. Direct plugin decoration
    // 3. Direct native git state
    // 4. Bubbled plugin decoration for directories
    // 5. Bubbled native git state for directories
    //
    // This preserves existing plugin badge ownership while still letting the
    // core git cache fill in when no plugin decoration is present.
    if has_unsaved {
        return Some(ResolvedExplorerStatus::Unsaved);
    }

    if let Some(decoration) = decorations.direct_for_path(path) {
        return Some(ResolvedExplorerStatus::Decoration(decoration));
    }

    if let Some(status) = git_statuses.direct_for_path(path) {
        return Some(ResolvedExplorerStatus::Git(status));
    }

    if is_dir {
        if let Some(decoration) = decorations.bubbled_for_path(path) {
            return Some(ResolvedExplorerStatus::BubbledDecoration(decoration));
        }
        if let Some(status) = git_statuses.bubbled_for_path(path) {
            return Some(ResolvedExplorerStatus::BubbledGit(status));
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

pub fn compatibility_git_status_name_color(status: FileExplorerGitStatus, theme: &Theme) -> Color {
    match status.kind {
        super::git_status::GitStatusKind::Added
        | super::git_status::GitStatusKind::StagedModified => theme.file_status_added_fg,
        super::git_status::GitStatusKind::Modified => theme.file_status_modified_fg,
        super::git_status::GitStatusKind::Deleted => theme.file_status_deleted_fg,
        super::git_status::GitStatusKind::Renamed | super::git_status::GitStatusKind::Copied => {
            theme.file_status_renamed_fg
        }
        super::git_status::GitStatusKind::Untracked => theme.file_status_untracked_fg,
        super::git_status::GitStatusKind::Conflicted => theme.file_status_conflicted_fg,
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
    use crate::view::file_tree::{FileExplorerGitStatus, GitStatusKind};

    #[test]
    fn resolves_unsaved_before_git_or_plugin_state() {
        let path = PathBuf::from("/repo/file.rs");
        let decorations = FileExplorerDecorationCache::default();
        let git_statuses = FileExplorerGitStatusCache::rebuild(
            vec![(
                path.clone(),
                FileExplorerGitStatus {
                    kind: GitStatusKind::Modified,
                },
            )],
            Path::new("/repo"),
            &HashMap::new(),
        );

        let resolved = resolve_explorer_status(&path, false, true, &decorations, &git_statuses);
        assert!(matches!(resolved, Some(ResolvedExplorerStatus::Unsaved)));
    }

    #[test]
    fn resolves_plugin_decoration_before_git_state() {
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
        let git_statuses = FileExplorerGitStatusCache::rebuild(
            vec![(
                path.clone(),
                FileExplorerGitStatus {
                    kind: GitStatusKind::Modified,
                },
            )],
            Path::new("/repo"),
            &HashMap::new(),
        );

        let resolved = resolve_explorer_status(&path, false, false, &decorations, &git_statuses);
        assert!(matches!(
            resolved,
            Some(ResolvedExplorerStatus::Decoration(decoration)) if decoration.symbol == "P"
        ));
    }

    #[test]
    fn row_status_exposes_bubbled_git_as_effective_git_status() {
        let path = PathBuf::from("/repo/src");
        let decorations = FileExplorerDecorationCache::default();
        let git_statuses = FileExplorerGitStatusCache::rebuild(
            vec![(
                PathBuf::from("/repo/src/file.rs"),
                FileExplorerGitStatus {
                    kind: GitStatusKind::Modified,
                },
            )],
            Path::new("/repo"),
            &HashMap::new(),
        );

        let row_status =
            ExplorerRowStatus::resolve(&path, true, false, &decorations, &git_statuses);
        assert_eq!(
            row_status.effective_git_status().map(|status| status.kind),
            Some(GitStatusKind::Modified)
        );
    }

    #[test]
    fn row_status_keeps_git_name_coloring_when_plugin_badge_wins() {
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
        let git_statuses = FileExplorerGitStatusCache::rebuild(
            vec![(
                path.clone(),
                FileExplorerGitStatus {
                    kind: GitStatusKind::Modified,
                },
            )],
            Path::new("/repo"),
            &HashMap::new(),
        );

        let row_status =
            ExplorerRowStatus::resolve(&path, false, false, &decorations, &git_statuses);

        assert!(matches!(
            row_status.resolved(),
            Some(ResolvedExplorerStatus::Decoration(decoration)) if decoration.symbol == "P"
        ));
        assert_eq!(
            row_status.effective_git_status().map(|status| status.kind),
            Some(GitStatusKind::Modified)
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
