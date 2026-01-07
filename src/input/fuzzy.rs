//! Fuzzy matching algorithm inspired by fzf
//!
//! Provides substring-style fuzzy matching where query characters must appear
//! in order in the target string, but not necessarily consecutively.
//! Matching is case-insensitive.

/// Score bonus constants for match quality ranking
mod score {
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
/// ```
pub fn fuzzy_match(query: &str, target: &str) -> FuzzyMatch {
    if query.is_empty() {
        return FuzzyMatch {
            matched: true,
            score: 0,
            match_positions: Vec::new(),
        };
    }

    let query_lower: Vec<char> = query.to_lowercase().chars().collect();
    let target_chars: Vec<char> = target.chars().collect();
    let target_lower: Vec<char> = target.to_lowercase().chars().collect();

    // Try to find the best matching positions using a greedy approach
    // that considers bonuses at each step
    let result = find_best_match(&query_lower, &target_chars, &target_lower);

    if let Some((positions, mut final_score)) = result {
        // Apply exact match bonuses to prioritize exact matches
        let query_len = query_lower.len();
        let target_len = target_lower.len();

        // Exact match bonus: query matches entire target
        if query_len == target_len {
            final_score += score::EXACT_MATCH;
        } else if target_len > query_len {
            // Check if the query is a prefix match (all consecutive from start)
            let is_prefix_match = positions.len() == query_len
                && positions.iter().enumerate().all(|(i, &pos)| pos == i);

            if is_prefix_match {
                let next_char = target_chars[query_len];

                // Highest priority: exact basename match (before extension)
                // This handles "config" matching "config.rs" better than "config_manager.rs"
                if next_char == '.' {
                    final_score += score::EXACT_MATCH; // Full exact match bonus for extension case
                }
                // Second priority: match before word separator (hyphen, underscore, space)
                // This handles "fresh" matching "fresh-editor" better than "freshness"
                else if next_char == '-' || next_char == '_' || next_char == ' ' {
                    final_score += score::EXACT_BASENAME_MATCH;
                }
            }
        }

        FuzzyMatch {
            matched: true,
            score: final_score,
            match_positions: positions,
        }
    } else {
        FuzzyMatch::no_match()
    }
}

/// Find the best matching positions for query in target
fn find_best_match(
    query: &[char],
    target_chars: &[char],
    target_lower: &[char],
) -> Option<(Vec<usize>, i32)> {
    if query.is_empty() {
        return Some((Vec::new(), 0));
    }

    // Use dynamic programming to find the best match
    // For each query position and target position, track the best score achievable
    let n = target_lower.len();
    let m = query.len();

    if n < m {
        return None;
    }

    // First, check if a match is even possible (quick rejection)
    {
        let mut qi = 0;
        for &tc in target_lower {
            if qi < m && tc == query[qi] {
                qi += 1;
            }
        }
        if qi < m {
            return None; // Not all query chars matched
        }
    }

    // dp[qi] = (best_score, prev_match_pos) for matching query[0..qi]
    // We'll track match positions separately
    #[derive(Clone)]
    struct State {
        score: i32,
        positions: Vec<usize>,
        last_match_pos: Option<usize>,
    }

    let mut best_for_query_len: Vec<Option<State>> = vec![None; m + 1];
    best_for_query_len[0] = Some(State {
        score: 0,
        positions: Vec::new(),
        last_match_pos: None,
    });

    for ti in 0..n {
        // Process in reverse to avoid using updated values in same iteration
        for qi in (0..m).rev() {
            if target_lower[ti] != query[qi] {
                continue;
            }

            let prev_state = &best_for_query_len[qi];
            if prev_state.is_none() {
                continue;
            }
            let prev = prev_state.as_ref().unwrap();

            // Check if this position is valid (must be after last match)
            if let Some(last_pos) = prev.last_match_pos {
                if ti <= last_pos {
                    continue;
                }
            }

            // Calculate score for matching query[qi] at target[ti]
            let mut match_score = 0;

            // Start of string bonus
            if ti == 0 {
                match_score += score::START_OF_STRING;
            }

            // Word boundary bonus
            if ti > 0 {
                let prev_char = target_chars[ti - 1];
                if prev_char == ' '
                    || prev_char == '_'
                    || prev_char == '-'
                    || prev_char == '/'
                    || prev_char == '.'
                {
                    match_score += score::WORD_BOUNDARY;
                } else if prev_char.is_lowercase() && target_chars[ti].is_uppercase() {
                    match_score += score::CAMEL_CASE;
                }
            }

            // Consecutive match bonus
            if let Some(last_pos) = prev.last_match_pos {
                if ti == last_pos + 1 {
                    match_score += score::CONSECUTIVE;
                } else {
                    // Gap penalty
                    let gap_size = ti - last_pos - 1;
                    match_score += score::GAP_START_PENALTY;
                    match_score += score::GAP_PENALTY * (gap_size as i32 - 1).max(0);
                }
            }

            let new_score = prev.score + match_score;

            let current = &best_for_query_len[qi + 1];
            let should_update = match current {
                None => true,
                Some(curr) => new_score > curr.score,
            };

            if should_update {
                let mut new_positions = prev.positions.clone();
                new_positions.push(ti);
                best_for_query_len[qi + 1] = Some(State {
                    score: new_score,
                    positions: new_positions,
                    last_match_pos: Some(ti),
                });
            }
        }
    }

    best_for_query_len[m]
        .as_ref()
        .map(|s| (s.positions.clone(), s.score))
}

/// Filter a list of items using fuzzy matching, returning sorted results
///
/// Items are sorted by match quality (best matches first).
/// Non-matching items are excluded.
pub fn fuzzy_filter<T, F>(query: &str, items: &[T], get_text: F) -> Vec<(usize, FuzzyMatch)>
where
    F: Fn(&T) -> &str,
{
    let mut results: Vec<(usize, FuzzyMatch)> = items
        .iter()
        .enumerate()
        .map(|(idx, item)| (idx, fuzzy_match(query, get_text(item))))
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
}
