//! Glob pattern matching for filename and path detection.
//!
//! Supports `*` (matches any sequence of characters) and `?` (matches exactly one character)
//! for filename matching, plus `**` (matches across directory boundaries) for path matching.

/// Check if a pattern string contains glob characters (`*` or `?`).
pub fn is_glob_pattern(pattern: &str) -> bool {
    pattern.contains('*') || pattern.contains('?')
}

/// Check if a pattern is a path pattern (should be matched against the full path, not just filename).
///
/// A pattern is considered a path pattern if it contains `/`, indicating it references
/// directory structure. Such patterns should be matched using [`path_glob_matches`] against
/// the full file path rather than [`filename_glob_matches`] against just the filename.
pub fn is_path_pattern(pattern: &str) -> bool {
    pattern.contains('/')
}

/// Match a glob pattern against a filename (not a full path).
///
/// Supports `*` (matches any sequence of characters) and `?` (matches exactly one character).
/// The match is performed against the entire filename.
///
/// Examples:
/// - `"*.conf"` matches `"nftables.conf"`, `"resolv.conf"`
/// - `"*rc"` matches `"lfrc"`, `".bashrc"`
/// - `"Dockerfile*"` matches `"Dockerfile"`, `"Dockerfile.dev"`
/// - `".env.*"` matches `".env.local"`, `".env.production"`
pub fn filename_glob_matches(pattern: &str, filename: &str) -> bool {
    glob_match_bytes(pattern.as_bytes(), filename.as_bytes())
}

/// Match a glob pattern against a full file path.
///
/// In path mode:
/// - `*` matches any sequence of characters **except** `/`
/// - `**` matches any sequence of characters **including** `/` (crosses directory boundaries)
/// - `?` matches exactly one character that is not `/`
/// - `**/` is treated as a unit that matches zero or more directory levels
///
/// Examples:
/// - `"/etc/**/rc.*"` matches `"/etc/rc.conf"`, `"/etc/init/rc.local"`
/// - `"/etc/*.conf"` matches `"/etc/nftables.conf"` but not `"/etc/sub/nftables.conf"`
/// - `"**/rc.*"` matches `"/etc/rc.conf"`, `"rc.conf"`
pub fn path_glob_matches(pattern: &str, path: &str) -> bool {
    path_glob_match_bytes(pattern.as_bytes(), path.as_bytes())
}

/// Iterative glob matching on byte slices using a backtracking algorithm.
/// Used for filename matching where `*` matches any character.
fn glob_match_bytes(pattern: &[u8], text: &[u8]) -> bool {
    let mut p = 0;
    let mut t = 0;
    // Track the last `*` position for backtracking
    let mut star_p = usize::MAX;
    let mut star_t = 0;

    while t < text.len() {
        if p < pattern.len() && (pattern[p] == b'?' || pattern[p] == text[t]) {
            p += 1;
            t += 1;
        } else if p < pattern.len() && pattern[p] == b'*' {
            star_p = p;
            star_t = t;
            p += 1;
        } else if star_p != usize::MAX {
            // Backtrack: consume one more char with the last `*`
            p = star_p + 1;
            star_t += 1;
            t = star_t;
        } else {
            return false;
        }
    }

    // Consume trailing `*`s in pattern
    while p < pattern.len() && pattern[p] == b'*' {
        p += 1;
    }

    p == pattern.len()
}

/// Path-aware glob matching where `*` does not cross `/` but `**` does.
///
/// When `**` is followed by `/`, the trailing `/` is consumed as part of the `**` token,
/// allowing `**/` to match zero or more complete directory levels.
fn path_glob_match_bytes(pattern: &[u8], text: &[u8]) -> bool {
    let mut p = 0;
    let mut t = 0;
    // Track the last `**` position for backtracking
    let mut dstar_p: Option<usize> = None;
    let mut dstar_t: usize = 0;
    // Track the last `*` position for backtracking
    let mut star_p: Option<usize> = None;
    let mut star_t: usize = 0;

    while t < text.len() {
        // Check for `**` (double star)
        if p + 1 < pattern.len() && pattern[p] == b'*' && pattern[p + 1] == b'*' {
            let mut next_p = p + 2;
            // Skip additional `*` characters
            while next_p < pattern.len() && pattern[next_p] == b'*' {
                next_p += 1;
            }
            // Skip trailing `/` so `**/` matches zero or more directory levels
            if next_p < pattern.len() && pattern[next_p] == b'/' {
                next_p += 1;
            }
            dstar_p = Some(next_p);
            dstar_t = t;
            p = next_p;
            // Reset single-star tracking since `**` subsumes it
            star_p = None;
            continue;
        }

        // Check for `*` (single star, does not cross `/`)
        if p < pattern.len() && pattern[p] == b'*' {
            star_p = Some(p + 1);
            star_t = t;
            p += 1;
            continue;
        }

        // Check for `?` (matches one non-`/` character)
        if p < pattern.len() && pattern[p] == b'?' && text[t] != b'/' {
            p += 1;
            t += 1;
            continue;
        }

        // Literal character match
        if p < pattern.len() && pattern[p] == text[t] {
            p += 1;
            t += 1;
            continue;
        }

        // Mismatch — try backtracking to single `*` first (if it won't cross `/`)
        if let Some(sp) = star_p {
            if text[star_t] != b'/' {
                star_t += 1;
                t = star_t;
                p = sp;
                continue;
            }
            // `*` can't help (would need to cross `/`), fall through to `**`
        }

        // Backtrack to `**` (can cross anything including `/`)
        if let Some(dp) = dstar_p {
            dstar_t += 1;
            t = dstar_t;
            p = dp;
            star_p = None;
            continue;
        }

        return false;
    }

    // Consume trailing `*`s and `**`s in pattern
    while p < pattern.len() && pattern[p] == b'*' {
        p += 1;
    }

    p == pattern.len()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_glob_pattern() {
        assert!(is_glob_pattern("*.conf"));
        assert!(is_glob_pattern("Dockerfile*"));
        assert!(is_glob_pattern("file?.txt"));
        assert!(is_glob_pattern("*"));
        assert!(!is_glob_pattern("Makefile"));
        assert!(!is_glob_pattern(".bashrc"));
        assert!(!is_glob_pattern(""));
    }

    #[test]
    fn test_is_path_pattern() {
        assert!(is_path_pattern("/etc/**/rc.*"));
        assert!(is_path_pattern("/etc/*.conf"));
        assert!(is_path_pattern("**/rc.*"));
        assert!(is_path_pattern("src/*.rs"));
        assert!(!is_path_pattern("*.conf"));
        assert!(!is_path_pattern("*rc"));
        assert!(!is_path_pattern("Makefile"));
    }

    #[test]
    fn test_star_prefix() {
        assert!(filename_glob_matches("*.conf", "nftables.conf"));
        assert!(filename_glob_matches("*.conf", "resolv.conf"));
        assert!(filename_glob_matches("*.conf", ".conf"));
        assert!(!filename_glob_matches("*.conf", "conf"));
        assert!(!filename_glob_matches("*.conf", "nftables.txt"));
    }

    #[test]
    fn test_star_suffix() {
        assert!(filename_glob_matches("Dockerfile*", "Dockerfile"));
        assert!(filename_glob_matches("Dockerfile*", "Dockerfile.dev"));
        assert!(!filename_glob_matches("Dockerfile*", "dockerfile"));
    }

    #[test]
    fn test_star_middle() {
        assert!(filename_glob_matches(".env.*", ".env.local"));
        assert!(filename_glob_matches(".env.*", ".env.production"));
        assert!(!filename_glob_matches(".env.*", ".env"));
    }

    #[test]
    fn test_star_suffix_pattern() {
        assert!(filename_glob_matches("*rc", "lfrc"));
        assert!(filename_glob_matches("*rc", ".bashrc"));
        assert!(filename_glob_matches("*rc", "rc"));
        assert!(!filename_glob_matches("*rc", "lfrc.bak"));
    }

    #[test]
    fn test_question_mark() {
        assert!(filename_glob_matches("file?.txt", "file1.txt"));
        assert!(filename_glob_matches("file?.txt", "fileA.txt"));
        assert!(!filename_glob_matches("file?.txt", "file.txt"));
        assert!(!filename_glob_matches("file?.txt", "file12.txt"));
    }

    #[test]
    fn test_bare_star() {
        assert!(filename_glob_matches("*", "anything"));
        assert!(filename_glob_matches("*", ""));
    }

    #[test]
    fn test_exact_match() {
        assert!(filename_glob_matches("Makefile", "Makefile"));
        assert!(!filename_glob_matches("Makefile", "makefile"));
    }

    #[test]
    fn test_multiple_stars() {
        assert!(filename_glob_matches("*.*", "file.txt"));
        assert!(filename_glob_matches("*.*", ".bashrc"));
        assert!(!filename_glob_matches("*.*", "Makefile"));
    }

    // --- Path glob matching tests ---

    #[test]
    fn test_path_doublestar_middle() {
        // /etc/**/rc.* should match across directory levels
        assert!(path_glob_matches("/etc/**/rc.*", "/etc/rc.conf"));
        assert!(path_glob_matches("/etc/**/rc.*", "/etc/init/rc.local"));
        assert!(path_glob_matches("/etc/**/rc.*", "/etc/a/b/c/rc.d"));
        assert!(!path_glob_matches("/etc/**/rc.*", "/var/rc.conf"));
        assert!(!path_glob_matches("/etc/**/rc.*", "/etc/init/nope"));
    }

    #[test]
    fn test_path_single_star_no_slash_crossing() {
        // * in path mode should NOT cross /
        assert!(path_glob_matches("/etc/*.conf", "/etc/nftables.conf"));
        assert!(path_glob_matches("/etc/*.conf", "/etc/resolv.conf"));
        assert!(!path_glob_matches("/etc/*.conf", "/etc/sub/nftables.conf"));
    }

    #[test]
    fn test_path_doublestar_prefix() {
        // **/filename matches the file anywhere in the tree
        assert!(path_glob_matches("**/rc.*", "/etc/rc.conf"));
        assert!(path_glob_matches("**/rc.*", "/etc/init/rc.local"));
        assert!(path_glob_matches("**/rc.*", "rc.conf"));
    }

    #[test]
    fn test_path_doublestar_suffix() {
        // /etc/** matches everything under /etc
        assert!(path_glob_matches("/etc/**", "/etc/foo"));
        assert!(path_glob_matches("/etc/**", "/etc/foo/bar"));
        assert!(path_glob_matches("/etc/**", "/etc/foo/bar/baz.conf"));
        assert!(!path_glob_matches("/etc/**", "/var/foo"));
    }

    #[test]
    fn test_path_question_mark() {
        // ? should not cross /
        assert!(path_glob_matches("/etc/rc.?", "/etc/rc.d"));
        assert!(!path_glob_matches("/etc/rc.?", "/etc/rc.dd"));
        assert!(!path_glob_matches("/etc/?", "/etc/ab"));
    }

    #[test]
    fn test_path_literal_match() {
        assert!(path_glob_matches("/etc/hosts", "/etc/hosts"));
        assert!(!path_glob_matches("/etc/hosts", "/etc/hostname"));
    }

    #[test]
    fn test_path_doublestar_and_single_star() {
        // Combine ** and *
        assert!(path_glob_matches("/etc/**/*.conf", "/etc/nftables.conf"));
        assert!(path_glob_matches("/etc/**/*.conf", "/etc/sub/nftables.conf"));
        assert!(path_glob_matches("/etc/**/*.conf", "/etc/a/b/c/foo.conf"));
        assert!(!path_glob_matches("/etc/**/*.conf", "/etc/a/b/c/foo.txt"));
        assert!(!path_glob_matches("/etc/**/*.conf", "/var/foo.conf"));
    }

    #[test]
    fn test_path_doublestar_zero_segments() {
        // ** matching zero directory levels
        assert!(path_glob_matches("**/Makefile", "Makefile"));
        assert!(path_glob_matches("**/Makefile", "/src/Makefile"));
        assert!(path_glob_matches("/src/**/main.rs", "/src/main.rs"));
    }

    #[test]
    fn test_path_multiple_doublestars() {
        assert!(path_glob_matches("/**/src/**/*.rs", "/home/user/src/main.rs"));
        assert!(path_glob_matches("/**/src/**/*.rs", "/src/lib.rs"));
        assert!(path_glob_matches("/**/src/**/*.rs", "/a/b/src/c/d/foo.rs"));
    }
}
