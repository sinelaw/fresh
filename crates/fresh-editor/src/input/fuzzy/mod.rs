//! Fuzzy matching algorithm inspired by fzf.
//!
//! Provides substring-style fuzzy matching where query characters must appear
//! in order in the target string, but not necessarily consecutively.  Matching
//! is case-insensitive.
//!
//! # Hot-path usage
//!
//! For a single keystroke matched against many candidates, build a
//! [`FuzzyMatcher`] once and reuse it.  The matcher owns both the
//! [`PreparedPattern`] *and* two reusable `Vec<char>` scratch buffers, so
//! after the first call neither query preparation nor target-side scratch
//! touches the allocator:
//!
//! ```ignore
//! let mut matcher = FuzzyMatcher::new(user_input);
//! for file in files {
//!     let result = matcher.match_target(&file.path);
//!     // ...
//! }
//! ```
//!
//! The convenience wrapper [`fuzzy_match`] rebuilds the matcher on every
//! call — fine for one-shot use, wasteful in a loop.
//!
//! The legacy [`fuzzy_match_prepared`] entry point still exists for
//! callers that want query amortisation without scratch amortisation; it
//! internally clones the pattern into a throwaway [`FuzzyMatcher`].
//!
//! # Module layout
//!
//! - [`pattern`] owns [`PreparedPattern`] and the non-allocating subsequence
//!   rejection used as the hot-path gate.
//! - [`matcher`] owns the scoring DP (with an arena-based backpointer chain
//!   instead of cloning position vectors) and the contiguous-substring
//!   scorer that runs in parallel with it.

mod matcher;
mod pattern;

pub use matcher::FuzzyMatcher;
pub use pattern::PreparedPattern;

/// Score bonus constants for match quality ranking.
pub(crate) mod score {
    /// Bonus for consecutive character matches
    pub const CONSECUTIVE: i32 = 16;
    /// Bonus for matching at word boundary (after space, underscore, etc.)
    pub const WORD_BOUNDARY: i32 = 32;
    /// Bonus for matching at the start of the string
    pub const START_OF_STRING: i32 = 48;
    /// Bonus for matching a camelCase transition (lowercase -> uppercase)
    pub const CAMEL_CASE: i32 = 24;
    /// Penalty per gap between matched characters
    pub const GAP_PENALTY: i32 = -3;
    /// Penalty for starting a gap (first unmatched char after a match)
    pub const GAP_START_PENALTY: i32 = -5;
    /// Bonus for exact match (query matches entire target)
    pub const EXACT_MATCH: i32 = 100;
    /// Bonus for exact base name match (query matches filename without extension)
    pub const EXACT_BASENAME_MATCH: i32 = 80;
    /// Bonus for a contiguous substring match (all query chars are consecutive
    /// in the target but not necessarily from position 0). This ensures that
    /// e.g. "results" in "results.json" ranks above scattered r-e-s-u-l-t-s.
    pub const CONTIGUOUS_SUBSTRING: i32 = 64;
}

/// Result of a fuzzy match, containing match status and quality score
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuzzyMatch {
    /// Whether the query matched the target
    pub matched: bool,
    /// Quality score (higher is better). Only meaningful if matched is true.
    pub score: i32,
    /// Indices in the target string where query characters matched
    pub match_positions: Vec<usize>,
}

impl FuzzyMatch {
    /// Create a non-matching result
    pub fn no_match() -> Self {
        Self {
            matched: false,
            score: 0,
            match_positions: Vec::new(),
        }
    }
}

impl Ord for FuzzyMatch {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // Non-matches are always worse than matches
        match (self.matched, other.matched) {
            (true, false) => std::cmp::Ordering::Greater,
            (false, true) => std::cmp::Ordering::Less,
            (false, false) => std::cmp::Ordering::Equal,
            (true, true) => self.score.cmp(&other.score),
        }
    }
}

impl PartialOrd for FuzzyMatch {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

/// Perform fzf-style fuzzy matching of a query against a target string.
///
/// Convenience wrapper that builds a [`PreparedPattern`] internally.  For
/// hot paths matching one query against many targets, prefer building a
/// [`PreparedPattern`] once and calling [`fuzzy_match_prepared`] per target.
///
/// Returns a `FuzzyMatch` containing:
/// - `matched`: true if all query characters appear in order in the target
/// - `score`: quality score based on match positions (consecutive matches, word boundaries, etc.)
/// - `match_positions`: indices in target where each query character matched
///
/// The algorithm favors:
/// - Consecutive character matches
/// - Matches at word boundaries (after space, underscore, hyphen, or camelCase transitions)
/// - Matches at the start of the string
///
/// Queries containing spaces are split into separate terms. Each term is matched
/// independently and all terms must match for the overall match to succeed.
///
/// # Examples
/// ```
/// use fresh::input::fuzzy::fuzzy_match;
///
/// // Exact substring match
/// let result = fuzzy_match("save", "Save File");
/// assert!(result.matched);
///
/// // Sparse match (fzf-style)
/// let result = fuzzy_match("sf", "Save File");
/// assert!(result.matched);
///
/// // Non-matching
/// let result = fuzzy_match("xyz", "Save File");
/// assert!(!result.matched);
///
/// // Multi-term match (space-separated)
/// let result = fuzzy_match("features groups-view", "/features/groups/groups-view.tsx");
/// assert!(result.matched);
/// ```
pub fn fuzzy_match(query: &str, target: &str) -> FuzzyMatch {
    let pattern = PreparedPattern::new(query);
    fuzzy_match_prepared(&pattern, target)
}

/// Perform fuzzy matching using a pre-prepared pattern.
///
/// This is the hot-path entry point — build the [`PreparedPattern`] once
/// and call this per target to amortise query-preparation work.
pub fn fuzzy_match_prepared(pattern: &PreparedPattern, target: &str) -> FuzzyMatch {
    matcher::match_prepared(pattern, target)
}

/// Filter a list of items using fuzzy matching, returning sorted results.
///
/// Items are sorted by match quality (best matches first).
/// Non-matching items are excluded.
pub fn fuzzy_filter<T, F>(query: &str, items: &[T], get_text: F) -> Vec<(usize, FuzzyMatch)>
where
    F: Fn(&T) -> &str,
{
    let pattern = PreparedPattern::new(query);
    let mut results: Vec<(usize, FuzzyMatch)> = items
        .iter()
        .enumerate()
        .map(|(idx, item)| (idx, fuzzy_match_prepared(&pattern, get_text(item))))
        .filter(|(_, m)| m.matched)
        .collect();

    // Sort by score descending (best matches first)
    results.sort_by(|a, b| b.1.score.cmp(&a.1.score));

    results
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_query_matches_everything() {
        let result = fuzzy_match("", "anything");
        assert!(result.matched);
        assert_eq!(result.score, 0);
    }

    #[test]
    fn test_exact_match() {
        let result = fuzzy_match("save", "save");
        assert!(result.matched);
        assert!(result.score > 0);
    }

    #[test]
    fn test_case_insensitive() {
        let result = fuzzy_match("SAVE", "save file");
        assert!(result.matched);

        let result = fuzzy_match("save", "SAVE FILE");
        assert!(result.matched);
    }

    #[test]
    fn test_substring_match() {
        let result = fuzzy_match("file", "Save File");
        assert!(result.matched);
    }

    #[test]
    fn test_sparse_match() {
        let result = fuzzy_match("sf", "Save File");
        assert!(result.matched);
        assert_eq!(result.match_positions.len(), 2);
    }

    #[test]
    fn test_no_match() {
        let result = fuzzy_match("xyz", "Save File");
        assert!(!result.matched);
    }

    #[test]
    fn test_query_longer_than_target() {
        let result = fuzzy_match("very long query", "short");
        assert!(!result.matched);
    }

    #[test]
    fn test_consecutive_matches_score_higher() {
        // Use examples without word boundary interference
        let result_consecutive = fuzzy_match("ab", "xabc");
        let result_sparse = fuzzy_match("ab", "xaxb");
        assert!(result_consecutive.matched);
        assert!(result_sparse.matched);
        assert!(
            result_consecutive.score > result_sparse.score,
            "consecutive: {}, sparse: {}",
            result_consecutive.score,
            result_sparse.score
        );
    }

    #[test]
    fn test_word_boundary_scores_higher() {
        let result_boundary = fuzzy_match("sf", "Save File");
        let result_middle = fuzzy_match("af", "Save File");
        assert!(result_boundary.matched);
        assert!(result_middle.matched);
        assert!(
            result_boundary.score > result_middle.score,
            "boundary: {}, middle: {}",
            result_boundary.score,
            result_middle.score
        );
    }

    #[test]
    fn test_start_of_string_scores_higher() {
        let result_start = fuzzy_match("s", "Save File");
        let result_middle = fuzzy_match("a", "Save File");
        assert!(result_start.matched);
        assert!(result_middle.matched);
        assert!(
            result_start.score > result_middle.score,
            "start: {}, middle: {}",
            result_start.score,
            result_middle.score
        );
    }

    #[test]
    fn test_camel_case_boundary() {
        let result = fuzzy_match("sf", "saveFile");
        assert!(result.matched);
        // 'F' is at a camelCase boundary
        assert!(result.score > 0);
    }

    #[test]
    fn test_fuzzy_filter() {
        let items = vec!["Save File", "Open File", "Save As", "Quit"];
        let results = fuzzy_filter("sf", &items, |s| s);

        assert!(!results.is_empty());
        // "Save File" should match
        let matched_texts: Vec<&str> = results.iter().map(|(idx, _)| items[*idx]).collect();
        assert!(matched_texts.contains(&"Save File"));
    }

    #[test]
    fn test_match_positions_are_correct() {
        let result = fuzzy_match("sf", "Save File");
        assert!(result.matched);
        assert_eq!(result.match_positions.len(), 2);
        assert_eq!(result.match_positions[0], 0); // 'S' in "Save"
        assert_eq!(result.match_positions[1], 5); // 'F' in "File"
    }

    #[test]
    fn test_fuzzy_ordering() {
        // Better match should have higher score
        let match1 = FuzzyMatch {
            matched: true,
            score: 100,
            match_positions: vec![],
        };
        let match2 = FuzzyMatch {
            matched: true,
            score: 50,
            match_positions: vec![],
        };
        let no_match = FuzzyMatch::no_match();

        assert!(match1 > match2);
        assert!(match2 > no_match);
        assert!(match1 > no_match);
    }

    #[test]
    fn test_out_of_order_no_match() {
        // Characters must appear in order
        let result = fuzzy_match("fs", "Save File");
        assert!(!result.matched);
    }

    #[test]
    fn test_multi_term_query_with_spaces() {
        // Each term should be matched independently
        let result = fuzzy_match("features groups-view", "/features/groups/groups-view.tsx");
        assert!(result.matched);
    }

    #[test]
    fn test_multi_term_query_partial_match_fails() {
        // If any term doesn't match, the whole query fails
        let result = fuzzy_match("features nonexistent", "/features/groups/groups-view.tsx");
        assert!(!result.matched);
    }

    #[test]
    fn test_multi_term_query_all_must_match() {
        // All terms must match
        let result = fuzzy_match("src main rs", "src/main.rs");
        assert!(result.matched);

        let result = fuzzy_match("src xyz", "src/main.rs");
        assert!(!result.matched);
    }

    #[test]
    fn test_multi_term_combines_scores() {
        // Multi-term match should combine scores from each term
        let result = fuzzy_match("save file", "Save File");
        assert!(result.matched);
        assert!(result.score > 0);
    }

    #[test]
    fn test_leading_trailing_spaces_ignored() {
        // Leading/trailing whitespace should be ignored
        let result = fuzzy_match("  save  ", "Save File");
        assert!(result.matched);
    }

    #[test]
    fn test_multiple_spaces_between_terms() {
        // Multiple spaces between terms should be treated as single separator
        let result = fuzzy_match("save   file", "Save File");
        assert!(result.matched);
    }

    #[test]
    fn test_real_world_command_names() {
        // Test with real command palette patterns
        assert!(fuzzy_match("gtd", "Go to Definition").matched);
        assert!(fuzzy_match("ofl", "Open File").matched);
        assert!(fuzzy_match("sas", "Save As").matched);
        assert!(fuzzy_match("fr", "Find and Replace").matched);
    }

    #[test]
    fn test_tab_name_patterns() {
        // Test with typical tab/file names
        assert!(fuzzy_match("main", "src/main.rs").matched);
        assert!(fuzzy_match("mod", "src/input/mod.rs").matched);
        assert!(fuzzy_match("cmdreg", "command_registry.rs").matched);
    }

    #[test]
    fn test_exact_match_scores_highest() {
        // "fresh" should score higher against "fresh" than against "fresh-editor"
        let exact = fuzzy_match("fresh", "fresh");
        let longer = fuzzy_match("fresh", "fresh-editor");

        assert!(exact.matched);
        assert!(longer.matched);
        assert!(
            exact.score > longer.score,
            "exact: {}, longer: {}",
            exact.score,
            longer.score
        );
    }

    #[test]
    fn test_exact_basename_match_scores_high() {
        // "fresh" matching "fresh-editor" should score higher than "fresh" matching "freshness"
        let basename_match = fuzzy_match("fresh", "fresh-editor");
        let substring_match = fuzzy_match("fresh", "freshness");

        assert!(basename_match.matched);
        assert!(substring_match.matched);
        assert!(
            basename_match.score > substring_match.score,
            "basename: {}, substring: {}",
            basename_match.score,
            substring_match.score
        );
    }

    #[test]
    fn test_exact_match_with_extension() {
        // "config" should score higher against "config.rs" than "config_manager.rs"
        let exact_base = fuzzy_match("config", "config.rs");
        let longer_name = fuzzy_match("config", "config_manager.rs");

        assert!(exact_base.matched);
        assert!(longer_name.matched);
        assert!(
            exact_base.score > longer_name.score,
            "exact_base: {}, longer: {}",
            exact_base.score,
            longer_name.score
        );
    }

    #[test]
    fn test_multi_term_exact_target_scores_higher() {
        // "Package: Packages" should score higher against "Package: Packages"
        // than against "Package: Install from URL"
        let exact = fuzzy_match("Package: Packages", "Package: Packages");
        let partial = fuzzy_match("Package: Packages", "Package: Install from URL");

        assert!(exact.matched, "exact should match");
        assert!(partial.matched, "partial should match");
        assert!(
            exact.score > partial.score,
            "exact target should score higher: exact={}, partial={}",
            exact.score,
            partial.score
        );
    }

    #[test]
    fn test_contiguous_substring_beats_scattered() {
        // "results" as a contiguous substring in the path should rank above
        // scattered r-e-s-u-l-t-s across different path components
        let contiguous = fuzzy_match("results", "repos/editor-benchmark/results.json");
        let scattered = fuzzy_match("results", "repos/quicklsp/LSP_TEST_REPORT.md");

        assert!(contiguous.matched);
        assert!(scattered.matched);
        assert!(
            contiguous.score > scattered.score,
            "contiguous ({}) should beat scattered ({})",
            contiguous.score,
            scattered.score
        );
    }

    #[test]
    fn test_multi_term_joined_by_path_separator_ranks_above_scattered() {
        // When the user types "etc hosts" (two terms), a target that
        // reconstructs the query with a common path separator between
        // the terms (e.g. "/etc/hosts") must rank higher than a target
        // where each term matches individually but scattered across
        // unrelated path components.
        let joined = fuzzy_match("etc hosts", "/etc/hosts");
        let scattered = fuzzy_match("etc hosts", "some/etc/deeply/nested/host_tests/foo.rs");

        assert!(joined.matched);
        assert!(scattered.matched);
        assert!(
            joined.score > scattered.score,
            "joined /etc/hosts ({}) should outrank scattered ({})",
            joined.score,
            scattered.score
        );
    }

    #[test]
    fn test_multi_term_joined_by_underscore_ranks_above_scattered() {
        // Same idea with an underscore separator: "save file" → "save_file.rs".
        let joined = fuzzy_match("save file", "src/utils/save_file.rs");
        let scattered = fuzzy_match("save file", "src/storage/savepoint/filetree_handler.rs");

        assert!(joined.matched);
        assert!(scattered.matched);
        assert!(
            joined.score > scattered.score,
            "joined save_file.rs ({}) should outrank scattered ({})",
            joined.score,
            scattered.score
        );
    }

    #[test]
    fn test_multi_term_joined_by_arbitrary_chars_ranks_above_scattered() {
        // The tight-span bonus is not specific to path separators:
        // "etc hosts" should rank a target like "etcmohosts" (two chars
        // between the terms, no separator at all) above a target where
        // the individual characters e-t-c-h-o-s-t-s are scattered with
        // big gaps, even though both targets satisfy the per-term
        // subsequence check.
        let tight = fuzzy_match("etc hosts", "etcmohosts");
        let scattered = fuzzy_match("etc hosts", "eblatblacblahblaoblasblatblas");

        assert!(tight.matched);
        assert!(scattered.matched);
        assert!(
            tight.score > scattered.score,
            "etcmohosts ({}) should outrank scattered ({})",
            tight.score,
            scattered.score
        );
    }

    #[test]
    fn test_multi_term_camel_case_joined_ranks_above_scattered() {
        // "save file" → "saveFile" (zero characters between, just a
        // camelCase transition) should get the tight-span bonus too.
        let camel = fuzzy_match("save file", "saveFile.rs");
        let scattered = fuzzy_match("save file", "savepoint_filetree_handler.rs");

        assert!(camel.matched);
        assert!(scattered.matched);
        assert!(
            camel.score > scattered.score,
            "saveFile.rs ({}) should outrank scattered ({})",
            camel.score,
            scattered.score
        );
    }

    #[test]
    fn test_amortized_apis_equivalent_to_oneshot() {
        // Both amortized entry points (`fuzzy_match_prepared` borrowing a
        // pre-built `PreparedPattern`, and `FuzzyMatcher` reusing scratch
        // across calls) must produce identical results to the one-shot
        // `fuzzy_match` wrapper for every (query, target) pair.
        //
        // The target list is arranged to cover:
        //   - long target first, then shorter ones (scratch-growth /
        //     stale-tail check — if `FuzzyMatcher` didn't clear its
        //     scratch correctly, a subsequent shorter target would see
        //     leftover chars and diverge from the one-shot).
        //   - matches interleaved with rejections (rejection path must
        //     not corrupt scratch for the next accepting call).
        //   - an empty target (edge case).
        let queries = ["main", "config", "results", "sf", "save file"];
        let targets = [
            "a/very/long/path/to/some/nested/src/main.rs", // warm scratch large
            "src/main.rs",                                 // shorter, still matches "main"
            "src/app/config.rs",                           // rejects "main", matches "config"
            "repos/editor-benchmark/results.json",         // matches "results"
            "Save File",                                   // matches "sf" / "save file"
            "nomatchatall",                                // rejects most queries
            "README.md",
            "",
        ];

        for query in queries {
            let pattern = PreparedPattern::new(query);
            let mut matcher = FuzzyMatcher::new(query);
            for target in targets {
                let oneshot = fuzzy_match(query, target);
                let prepared = fuzzy_match_prepared(&pattern, target);
                let reused = matcher.match_target(target);
                assert_eq!(
                    oneshot, prepared,
                    "fuzzy_match_prepared mismatch for query={:?} target={:?}",
                    query, target
                );
                assert_eq!(
                    oneshot, reused,
                    "FuzzyMatcher reuse mismatch for query={:?} target={:?}",
                    query, target
                );
            }
        }
    }
}
