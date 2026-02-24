//! Simple glob pattern matching for filename detection.
//!
//! Supports `*` (matches any sequence of characters) and `?` (matches exactly one character).
//! Patterns are matched against filenames only, not full paths.

/// Check if a filename pattern string contains glob characters.
pub fn is_glob_pattern(pattern: &str) -> bool {
    pattern.contains('*') || pattern.contains('?')
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

/// Iterative glob matching on byte slices using a backtracking algorithm.
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
}
