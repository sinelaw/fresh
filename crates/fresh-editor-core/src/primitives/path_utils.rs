//! Path utilities for path expansion and normalization.

use std::borrow::Cow;
use std::path::{Path, PathBuf};

/// Expand tilde (~) in a path to the user's home directory.
///
/// # Examples
/// - `~/Documents` -> `/home/user/Documents`
/// - `~` -> `/home/user`
/// - `/absolute/path` -> `/absolute/path` (unchanged)
/// - `relative/path` -> `relative/path` (unchanged)
///
/// If the home directory cannot be determined, the path is returned unchanged.
pub fn expand_tilde(path: &str) -> PathBuf {
    if let Some(suffix) = path.strip_prefix('~') {
        if let Some(home) = dirs::home_dir() {
            home.join(suffix.trim_start_matches('/'))
        } else {
            PathBuf::from(path)
        }
    } else {
        PathBuf::from(path)
    }
}

/// Convert a Windows verbatim path (`\\?\C:\…` or `\\?\UNC\server\share\…`)
/// into its non-verbatim equivalent (`C:\…` or `\\server\share\…`).
///
/// Returns the input unchanged on non-Windows platforms or for paths that
/// have no verbatim prefix.
pub fn strip_verbatim_prefix(path: &Path) -> Cow<'_, Path> {
    #[cfg(windows)]
    {
        use std::path::{Component, Prefix};

        let mut components = path.components();
        let prefix = match components.next() {
            Some(Component::Prefix(p)) => p,
            _ => return Cow::Borrowed(path),
        };

        let mut rebuilt = PathBuf::new();
        match prefix.kind() {
            Prefix::VerbatimDisk(drive) => {
                rebuilt.push(format!("{}:\\", drive as char));
            }
            Prefix::VerbatimUNC(server, share) => {
                rebuilt.push(format!(
                    r"\\{}\{}\",
                    server.to_string_lossy(),
                    share.to_string_lossy()
                ));
            }
            _ => return Cow::Borrowed(path),
        }
        // Skip the original RootDir (which the rebuilt prefix already includes)
        // and append the rest of the components.
        for component in components {
            if matches!(component, Component::RootDir) {
                continue;
            }
            rebuilt.push(component.as_os_str());
        }
        Cow::Owned(rebuilt)
    }
    #[cfg(not(windows))]
    {
        Cow::Borrowed(path)
    }
}

/// A relative path as a `/`-separated string: the spelling the project walk,
/// the file finder and the glob matchers all name a file by.
///
/// Built from the path's components rather than by rewriting every `\` in the
/// whole string. On Windows a component boundary really is a `\`, but on Unix
/// a `\` is an ordinary character in a file name, and rewriting it renames the
/// file: `a\b.txt` came back as `a/b.txt`, a path that does not exist, and the
/// walk then disagreed with `read_dir` about the same file.
///
/// `rel` is meant to be relative, as [`Path::strip_prefix`] returns. A root or
/// prefix component is dropped and `.` segments collapse, neither of which
/// that output carries.
pub fn rel_to_slash_string(rel: &Path) -> String {
    use std::path::Component;

    let mut out = String::new();
    for component in rel.components() {
        let part: Cow<'_, str> = match component {
            Component::Normal(p) => p.to_string_lossy(),
            Component::ParentDir => Cow::Borrowed(".."),
            Component::CurDir | Component::RootDir | Component::Prefix(_) => continue,
        };
        if !out.is_empty() {
            out.push('/');
        }
        out.push_str(&part);
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_expand_tilde_with_path() {
        let result = expand_tilde("~/Documents/test.txt");
        if let Some(home) = dirs::home_dir() {
            assert_eq!(result, home.join("Documents/test.txt"));
        }
    }

    #[test]
    fn test_expand_tilde_just_tilde() {
        let result = expand_tilde("~");
        if let Some(home) = dirs::home_dir() {
            assert_eq!(result, home);
        }
    }

    #[test]
    fn test_expand_tilde_absolute_path() {
        let result = expand_tilde("/absolute/path");
        assert_eq!(result, PathBuf::from("/absolute/path"));
    }

    #[test]
    fn test_expand_tilde_relative_path() {
        let result = expand_tilde("relative/path");
        assert_eq!(result, PathBuf::from("relative/path"));
    }

    #[test]
    fn test_expand_tilde_with_slash() {
        let result = expand_tilde("~/");
        if let Some(home) = dirs::home_dir() {
            assert_eq!(result, home);
        }
    }

    #[cfg(not(windows))]
    #[test]
    fn strip_verbatim_prefix_is_noop_on_unix() {
        let p = Path::new("/home/user/project");
        assert_eq!(strip_verbatim_prefix(p).as_ref(), p);
    }

    #[cfg(windows)]
    #[test]
    fn strip_verbatim_prefix_removes_verbatim_disk() {
        let verbatim = PathBuf::from(r"\\?\C:\Users\HP\OneDrive\Desktop\PY'PGMS");
        let stripped = strip_verbatim_prefix(&verbatim);
        assert_eq!(
            stripped.as_ref(),
            Path::new(r"C:\Users\HP\OneDrive\Desktop\PY'PGMS"),
            "verbatim disk prefix should be replaced with plain drive form"
        );
    }

    #[cfg(windows)]
    #[test]
    fn strip_verbatim_prefix_removes_verbatim_unc() {
        let verbatim = PathBuf::from(r"\\?\UNC\server\share\dir\file");
        let stripped = strip_verbatim_prefix(&verbatim);
        assert_eq!(
            stripped.as_ref(),
            Path::new(r"\\server\share\dir\file"),
            "verbatim UNC prefix should be replaced with plain UNC form"
        );
    }

    #[cfg(windows)]
    #[test]
    fn strip_verbatim_prefix_passes_plain_paths_through() {
        let plain = PathBuf::from(r"C:\Users\HP\project");
        let result = strip_verbatim_prefix(&plain);
        assert_eq!(result.as_ref(), Path::new(r"C:\Users\HP\project"));
    }
    #[test]
    fn rel_to_slash_string_joins_components_with_slashes() {
        let rel = Path::new("a").join("b").join("c.txt");
        assert_eq!(rel_to_slash_string(&rel), "a/b/c.txt");
    }

    #[test]
    fn rel_to_slash_string_of_an_empty_path_is_empty() {
        assert_eq!(rel_to_slash_string(Path::new("")), "");
    }

    #[test]
    fn rel_to_slash_string_collapses_dot_segments() {
        assert_eq!(rel_to_slash_string(Path::new("a/./b.txt")), "a/b.txt");
    }

    /// Off Windows a backslash is an ordinary character in a file name, so it
    /// has to survive: rewriting it names a different, nonexistent file.
    #[cfg(not(windows))]
    #[test]
    fn rel_to_slash_string_keeps_a_backslash_in_a_name() {
        assert_eq!(rel_to_slash_string(Path::new("a\\b.txt")), "a\\b.txt");
        assert_eq!(
            rel_to_slash_string(&Path::new("dir").join("a\\b.txt")),
            "dir/a\\b.txt"
        );
    }

    /// On Windows a backslash *is* the component boundary, and the result is
    /// `/`-separated whichever separator the input used.
    #[cfg(windows)]
    #[test]
    fn rel_to_slash_string_normalizes_windows_separators() {
        assert_eq!(rel_to_slash_string(Path::new(r"a\b.txt")), "a/b.txt");
        assert_eq!(rel_to_slash_string(Path::new("a/b.txt")), "a/b.txt");
    }
}
