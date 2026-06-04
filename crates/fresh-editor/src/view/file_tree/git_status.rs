use std::collections::HashMap;
use std::path::{Path, PathBuf};

use super::cache::{build_bubbled_cache, insert_with_aliases};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GitStatusKind {
    Added,
    Modified,
    StagedModified,
    Deleted,
    Renamed,
    Copied,
    Untracked,
    Conflicted,
}

impl GitStatusKind {
    pub fn symbol(self) -> &'static str {
        match self {
            Self::Added => "A",
            Self::Modified => "M",
            Self::StagedModified => "M",
            Self::Deleted => "D",
            Self::Renamed => "R",
            Self::Copied => "C",
            Self::Untracked => "U",
            Self::Conflicted => "!",
        }
    }

    pub fn priority(self) -> u8 {
        match self {
            Self::Conflicted => 90,
            Self::Deleted => 80,
            Self::Added => 60,
            Self::StagedModified => 52,
            Self::Modified => 50,
            Self::Renamed | Self::Copied => 40,
            Self::Untracked => 30,
        }
    }

    pub fn tooltip(self) -> &'static str {
        match self {
            Self::Added => "Added - File is staged for commit",
            Self::Modified => "Modified - File has unstaged changes",
            Self::StagedModified => "Modified - File has staged changes",
            Self::Deleted => "Deleted - File is staged for deletion",
            Self::Renamed => "Renamed - File has been renamed",
            Self::Copied => "Copied - File has been copied",
            Self::Untracked => "Untracked - File is not tracked by git",
            Self::Conflicted => "Conflicted - File has merge conflicts",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FileExplorerGitStatus {
    pub kind: GitStatusKind,
}

impl FileExplorerGitStatus {
    pub fn symbol(self) -> &'static str {
        self.kind.symbol()
    }

    pub fn priority(self) -> u8 {
        self.kind.priority()
    }
}

#[derive(Debug, Default, Clone)]
pub struct FileExplorerGitStatusCache {
    direct: HashMap<PathBuf, FileExplorerGitStatus>,
    bubbled: HashMap<PathBuf, FileExplorerGitStatus>,
}

impl FileExplorerGitStatusCache {
    pub fn rebuild<I>(
        statuses: I,
        root: &Path,
        symlink_mappings: &HashMap<PathBuf, PathBuf>,
    ) -> Self
    where
        I: IntoIterator<Item = (PathBuf, FileExplorerGitStatus)>,
    {
        let mut direct = HashMap::new();
        for (path, status) in statuses {
            if !path.starts_with(root) {
                continue;
            }
            insert_with_aliases(
                &mut direct,
                &path,
                &status,
                symlink_mappings,
                |map, path, status| {
                    insert_best(map, path, status);
                },
            );
        }

        let bubbled = build_bubbled_cache(
            &direct,
            root,
            |map, path, status| insert_best(map, path, status),
            |_ancestor, status| *status,
        );

        Self { direct, bubbled }
    }

    pub fn direct_for_path(&self, path: &Path) -> Option<FileExplorerGitStatus> {
        self.direct.get(path).copied()
    }

    pub fn bubbled_for_path(&self, path: &Path) -> Option<FileExplorerGitStatus> {
        self.bubbled.get(path).copied()
    }

    pub fn direct_paths_under(&self, dir_path: &Path) -> Vec<PathBuf> {
        let mut paths: Vec<PathBuf> = self
            .direct
            .keys()
            .filter(|path| path.as_path() != dir_path && path.starts_with(dir_path))
            .cloned()
            .collect();
        paths.sort();
        paths
    }
}

pub fn parse_porcelain_z(output: &[u8], repo_root: &Path) -> Vec<(PathBuf, FileExplorerGitStatus)> {
    let text = String::from_utf8_lossy(output);
    let entries: Vec<&str> = text.split('\0').filter(|entry| !entry.is_empty()).collect();
    let mut statuses_by_path: HashMap<PathBuf, FileExplorerGitStatus> = HashMap::new();

    let mut i = 0usize;
    while i < entries.len() {
        let entry = entries[i];
        i += 1;

        if entry.len() < 3 {
            continue;
        }

        let bytes = entry.as_bytes();
        let x = bytes[0] as char;
        let y = bytes[1] as char;
        let mut path = entry[3..].to_string();

        if (x == 'R' || x == 'C') && i < entries.len() {
            path = entries[i].to_string();
            i += 1;
        } else if entry.contains(" -> ") && (x == 'R' || x == 'C' || y == 'R' || y == 'C') {
            if let Some(target) = entry.split(" -> ").last() {
                path = target.to_string();
            }
        }

        let Some(kind) = status_kind_from_xy(x, y) else {
            continue;
        };

        let absolute_path = repo_root.join(path);
        let status = FileExplorerGitStatus { kind };
        insert_best(&mut statuses_by_path, absolute_path, status);
    }

    statuses_by_path.into_iter().collect()
}

fn insert_best(
    map: &mut HashMap<PathBuf, FileExplorerGitStatus>,
    path: PathBuf,
    status: FileExplorerGitStatus,
) {
    let replace = match map.get(&path) {
        Some(existing) => status.priority() >= existing.priority(),
        None => true,
    };

    if replace {
        map.insert(path, status);
    }
}

fn status_kind_from_xy(x: char, y: char) -> Option<GitStatusKind> {
    if x == '?' && y == '?' {
        return Some(GitStatusKind::Untracked);
    }

    if is_conflicted_pair(x, y) {
        return Some(GitStatusKind::Conflicted);
    }

    if x != ' ' && x != '?' {
        return status_kind_from_code(x, true);
    }

    if y != ' ' {
        return status_kind_from_code(y, false);
    }

    None
}

fn is_conflicted_pair(x: char, y: char) -> bool {
    matches!(
        (x, y),
        ('U', _) | (_, 'U') | ('A', 'A') | ('D', 'D') | ('A', 'D') | ('D', 'A')
    )
}

fn status_kind_from_code(code: char, staged: bool) -> Option<GitStatusKind> {
    match code {
        'A' => Some(GitStatusKind::Added),
        'M' => Some(if staged {
            GitStatusKind::StagedModified
        } else {
            GitStatusKind::Modified
        }),
        'D' => Some(GitStatusKind::Deleted),
        'R' => Some(GitStatusKind::Renamed),
        'C' => Some(GitStatusKind::Copied),
        'U' => Some(GitStatusKind::Conflicted),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_extension_statuses_from_porcelain_output() {
        let repo_root = Path::new("/repo");
        let statuses = parse_porcelain_z(b" M src/schema.ts\0?? README.md\0", repo_root);

        let mut by_path = HashMap::new();
        for (path, status) in statuses {
            by_path.insert(path, status.kind);
        }

        assert_eq!(
            by_path.get(&PathBuf::from("/repo/src/schema.ts")),
            Some(&GitStatusKind::Modified)
        );
        assert_eq!(
            by_path.get(&PathBuf::from("/repo/README.md")),
            Some(&GitStatusKind::Untracked)
        );
    }

    #[test]
    fn parses_renamed_destination_from_nul_delimited_output() {
        let repo_root = Path::new("/repo");
        let statuses = parse_porcelain_z(b"R  old_name.ts\0new_name.ts\0", repo_root);

        assert_eq!(statuses.len(), 1);
        assert_eq!(statuses[0].0, PathBuf::from("/repo/new_name.ts"));
        assert_eq!(statuses[0].1.kind, GitStatusKind::Renamed);
    }

    #[test]
    fn parses_conflicted_entries() {
        let repo_root = Path::new("/repo");
        let statuses = parse_porcelain_z(b"UU src/conflict.rs\0", repo_root);

        assert_eq!(statuses.len(), 1);
        assert_eq!(statuses[0].1.kind, GitStatusKind::Conflicted);
        assert_eq!(statuses[0].1.symbol(), "!");
    }

    #[test]
    fn bubbles_highest_priority_status_to_ancestors() {
        let root = Path::new("/repo");
        let statuses = vec![
            (
                PathBuf::from("/repo/src/modified.ts"),
                FileExplorerGitStatus {
                    kind: GitStatusKind::Modified,
                },
            ),
            (
                PathBuf::from("/repo/src/conflict.ts"),
                FileExplorerGitStatus {
                    kind: GitStatusKind::Conflicted,
                },
            ),
        ];

        let cache = FileExplorerGitStatusCache::rebuild(statuses, root, &HashMap::new());

        assert_eq!(
            cache
                .bubbled_for_path(Path::new("/repo/src"))
                .map(|status| status.kind),
            Some(GitStatusKind::Conflicted)
        );
    }

    #[test]
    fn lists_direct_paths_under_directory_in_sorted_order() {
        let root = Path::new("/repo");
        let statuses = vec![
            (
                PathBuf::from("/repo/src/zeta.ts"),
                FileExplorerGitStatus {
                    kind: GitStatusKind::Modified,
                },
            ),
            (
                PathBuf::from("/repo/src/nested/alpha.ts"),
                FileExplorerGitStatus {
                    kind: GitStatusKind::Added,
                },
            ),
            (
                PathBuf::from("/repo/README.md"),
                FileExplorerGitStatus {
                    kind: GitStatusKind::Modified,
                },
            ),
        ];

        let cache = FileExplorerGitStatusCache::rebuild(statuses, root, &HashMap::new());

        assert_eq!(
            cache.direct_paths_under(Path::new("/repo/src")),
            vec![
                PathBuf::from("/repo/src/nested/alpha.ts"),
                PathBuf::from("/repo/src/zeta.ts"),
            ]
        );
    }
}
