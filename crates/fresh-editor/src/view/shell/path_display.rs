//! Shortening a path for display: the first component, `[...]`, and as many
//! trailing components as fit. The prompt row's directory and the file
//! browser's header both show a path this way.

use std::path::Path;

/// Result of truncating a path for display
#[derive(Debug, Clone)]
pub struct TruncatedPath {
    /// The first component(s) of the path (e.g. "/home" or "C:\Users")
    pub prefix: String,
    /// Whether truncation occurred (if true, display "[...]" between prefix and suffix)
    pub truncated: bool,
    /// The last components of the path (e.g. "project/src")
    pub suffix: String,
    /// The path's own separator, reused when re-joining the prefix /
    /// ellipsis / suffix so a Windows `\`-path doesn't display with `/`.
    pub sep: char,
}

impl TruncatedPath {
    /// Get the full display string (without styling)
    pub fn to_string_plain(&self) -> String {
        if self.truncated {
            format!("{}{}[...]{}", self.prefix, self.sep, self.suffix)
        } else {
            format!("{}{}", self.prefix, self.suffix)
        }
    }
}

/// The separator a path string is written with: `\` for a Windows-style
/// path (so it round-trips natively), `/` otherwise. Splitting always
/// accepts both — this only decides how pieces are re-joined for display.
pub(crate) fn path_display_sep(path_str: &str) -> char {
    if path_str.contains('\\') {
        '\\'
    } else {
        '/'
    }
}

/// Truncate a path for display, showing the first component, [...], and last components
///
/// For example, `/private/var/folders/p6/nlmq.../T/.tmpNYt4Fc/project/file.txt`
/// becomes `/private/[...]/project/file.txt`
///
/// # Arguments
/// * `path` - The path to truncate
/// * `max_len` - Maximum length for the display string
///
/// # Returns
/// A TruncatedPath struct with prefix, truncation indicator, and suffix
pub fn truncate_path(path: &Path, max_len: usize) -> TruncatedPath {
    let path_str = path.to_string_lossy();
    // Re-join pieces with the path's own separator so a Windows `\`-path
    // doesn't render as `C:/[...]/x`. Splitting accepts both separators —
    // crucially so a `\`-path isn't treated as one giant component (which
    // previously forced the crude end-truncation branch on Windows).
    let sep = path_display_sep(&path_str);

    // If path fits, return as-is
    if path_str.len() <= max_len {
        return TruncatedPath {
            prefix: String::new(),
            truncated: false,
            suffix: path_str.to_string(),
            sep,
        };
    }

    let components: Vec<&str> = path_str
        .split(['/', '\\'])
        .filter(|s| !s.is_empty())
        .collect();

    if components.is_empty() {
        return TruncatedPath {
            prefix: sep.to_string(),
            truncated: false,
            suffix: String::new(),
            sep,
        };
    }

    // Keep "root + first directory" as the prefix, like the Unix display
    // (`/private/[...]`). A Windows drive letter ("C:") plays the part of
    // the root, so keep `C:\<firstdir>` to stay symmetric instead of just
    // the bare drive.
    let leading_sep = path_str.starts_with('/') || path_str.starts_with('\\');
    let is_drive = |c: &str| {
        let b = c.as_bytes();
        b.len() == 2 && b[1] == b':' && b[0].is_ascii_alphabetic()
    };
    let prefix_count = if !leading_sep && is_drive(components[0]) {
        2
    } else {
        1
    }
    .min(components.len());
    let sep_str = sep.to_string();
    let prefix = {
        let joined = components[..prefix_count].join(&sep_str);
        if leading_sep {
            format!("{}{}", sep, joined)
        } else {
            joined
        }
    };

    // The "<sep>[...]" marker takes 6 bytes (separator + "[...]").
    let ellipsis_len = sep.len_utf8() + "[...]".len();

    // Calculate how much space we have for the suffix
    let available_for_suffix = max_len.saturating_sub(prefix.len() + ellipsis_len);

    if available_for_suffix < 5 || components.len() <= prefix_count {
        // Not enough space or nothing past the prefix, just truncate the
        // end. Walk back to a char boundary so paths with non-ASCII
        // components (e.g. `/home/ユーザー/project`) don't byte-slice
        // through a multi-byte UTF-8 sequence and panic (same class as
        // #1718).
        let truncated_path = if path_str.len() > max_len.saturating_sub(3) {
            let cut = path_str.floor_char_boundary(max_len.saturating_sub(3));
            format!("{}...", &path_str[..cut])
        } else {
            path_str.to_string()
        };
        return TruncatedPath {
            prefix: String::new(),
            truncated: false,
            suffix: truncated_path,
            sep,
        };
    }

    // Build suffix from the last components that fit
    let mut suffix_parts: Vec<&str> = Vec::new();
    let mut suffix_len = 0;

    for component in components.iter().skip(prefix_count).rev() {
        let component_len = component.len() + 1; // +1 for the separator
        if suffix_len + component_len <= available_for_suffix {
            suffix_parts.push(component);
            suffix_len += component_len;
        } else {
            break;
        }
    }

    suffix_parts.reverse();

    // If we included all remaining components, no truncation needed
    if suffix_parts.len() == components.len() - prefix_count {
        return TruncatedPath {
            prefix: String::new(),
            truncated: false,
            suffix: path_str.to_string(),
            sep,
        };
    }

    let suffix = if suffix_parts.is_empty() {
        // Can't fit any suffix components, truncate the last component.
        // floor_char_boundary keeps the slice on a valid UTF-8 boundary
        // when `last` contains non-ASCII characters.
        let last = components.last().unwrap_or(&"");
        let truncate_to = available_for_suffix.saturating_sub(4); // "/.." and some chars
        if truncate_to > 0 && last.len() > truncate_to {
            let cut = last.floor_char_boundary(truncate_to);
            format!("{}{}...", sep, &last[..cut])
        } else {
            format!("{}{}", sep, last)
        }
    } else {
        format!("{}{}", sep, suffix_parts.join(&sep_str))
    };

    TruncatedPath {
        prefix,
        truncated: true,
        suffix,
        sep,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn test_truncate_path_short_path() {
        let path = PathBuf::from("/home/user/project");
        let result = truncate_path(&path, 50);

        assert!(!result.truncated);
        assert_eq!(result.suffix, "/home/user/project");
        assert!(result.prefix.is_empty());
    }

    #[test]
    fn test_truncate_path_long_path() {
        let path = PathBuf::from(
            "/private/var/folders/p6/nlmq3k8146990kpkxl73mq340000gn/T/.tmpNYt4Fc/project_root",
        );
        let result = truncate_path(&path, 40);

        assert!(result.truncated, "Path should be truncated");
        assert_eq!(result.prefix, "/private");
        assert!(
            result.suffix.contains("project_root"),
            "Suffix should contain project_root"
        );
    }

    #[test]
    fn test_truncate_path_preserves_last_components() {
        let path = PathBuf::from("/a/b/c/d/e/f/g/h/i/j/project/src");
        let result = truncate_path(&path, 30);

        assert!(result.truncated);
        // Should preserve the last components that fit
        assert!(
            result.suffix.contains("src"),
            "Should preserve last component 'src', got: {}",
            result.suffix
        );
    }

    #[test]
    fn test_truncate_path_display_len() {
        let path = PathBuf::from("/private/var/folders/deep/nested/path/here");
        let result = truncate_path(&path, 30);

        // The display length should not exceed max_len (approximately)
        let display = result.to_string_plain();
        assert!(
            display.len() <= 35, // Allow some slack for trailing slash
            "Display should be truncated to around 30 chars, got {} chars: {}",
            display.len(),
            display
        );
    }

    #[test]
    fn test_truncate_path_root_only() {
        let path = PathBuf::from("/");
        let result = truncate_path(&path, 50);

        assert!(!result.truncated);
        assert_eq!(result.suffix, "/");
    }

    #[test]
    fn test_truncate_path_multibyte_single_component_does_not_panic() {
        // Routes into the "truncate the end" branch (line 414): the prefix
        // alone exceeds max_len, so available_for_suffix becomes 0. Before
        // the fix, byte-slicing `path_str` at `max_len - 3 = 2` lands
        // inside the 3-byte UTF-8 sequence for `ユ` and panicked the
        // editor — same class as #1718.
        let path = PathBuf::from("/ユーザーのプロジェクト名前/file");
        let result = truncate_path(&path, 5);
        let display = result.to_string_plain();
        assert!(display.is_char_boundary(display.len()));
        assert!(display.ends_with("..."));
    }

    #[test]
    fn test_truncate_path_multibyte_last_component_does_not_panic() {
        // Routes into the "truncate the last component" branch (line 453):
        // available_for_suffix is large enough to enter the suffix-build
        // loop, but the only remaining component doesn't fit, so we fall
        // back to truncating it. Before the fix, byte-slicing the
        // non-ASCII component at `truncate_to = 1` lands inside the 3-byte
        // UTF-8 sequence for `ユ` and panicked.
        let path = PathBuf::from("/a/ユーザーのプロジェクト名前");
        let result = truncate_path(&path, 13);
        let display = result.to_string_plain();
        assert!(display.is_char_boundary(display.len()));
    }

    #[test]
    fn test_truncated_path_to_string_plain() {
        let truncated = TruncatedPath {
            prefix: "/home".to_string(),
            truncated: true,
            suffix: "/project/src".to_string(),
            sep: '/',
        };

        assert_eq!(truncated.to_string_plain(), "/home/[...]/project/src");
    }

    #[test]
    fn test_truncated_path_to_string_plain_no_truncation() {
        let truncated = TruncatedPath {
            prefix: String::new(),
            truncated: false,
            suffix: "/home/user/project".to_string(),
            sep: '/',
        };

        assert_eq!(truncated.to_string_plain(), "/home/user/project");
    }

    /// A Windows-style "\"-path must middle-truncate (keeping drive +
    /// first dir and the tail) and render with backslashes — not fall into
    /// the crude end-truncation that `split('/')` forced because a
    /// backslash path has no '/' to split on. (We can exercise this on any
    /// OS because `truncate_path` works on the path *string*.)
    #[test]
    fn test_truncate_path_windows_backslashes() {
        let path = Path::new(r"C:\Users\me\projects\fresh\crates\editor\src\main.rs");
        let t = truncate_path(path, 34);
        assert!(t.truncated, "long backslash path should middle-truncate");
        assert_eq!(t.sep, '\\', "should re-join with backslashes");
        let shown = t.to_string_plain();
        assert!(
            shown.starts_with(r"C:\Users"),
            "keeps drive + first dir: {shown}"
        );
        assert!(
            shown.contains(r"\[...]\"),
            "uses a backslash ellipsis: {shown}"
        );
        assert!(shown.ends_with("main.rs"), "keeps the tail: {shown}");
        assert!(!shown.contains('/'), "no forward slashes leak in: {shown}");
        assert!(shown.len() <= 34, "respects max_len: {shown}");
    }

    /// A short backslash path that fits is returned unchanged.
    #[test]
    fn test_truncate_path_windows_short_unchanged() {
        let path = Path::new(r"C:\a\b");
        let t = truncate_path(path, 80);
        assert!(!t.truncated);
        assert_eq!(t.to_string_plain(), r"C:\a\b");
    }
}
