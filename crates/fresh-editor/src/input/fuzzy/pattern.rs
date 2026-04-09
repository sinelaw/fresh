//! Query preparation and fast subsequence rejection.
//!
//! A [`PreparedPattern`] owns the expensive bits of a query (lowercased
//! chars, ASCII fast-path bytes, split terms) so they are computed once
//! per keystroke instead of once per candidate target.  The subsequence
//! helpers then reject unmatched targets without allocating anything on
//! the target side, leaving the heavier DP in [`super::matcher`] to run
//! only on candidates that could actually match.

/// A query pre-processed for fuzzy matching.
///
/// Creating a `PreparedPattern` once per keystroke and reusing it across all
/// candidate targets avoids re-lowercasing and re-allocating the query for
/// every file in a large corpus.  This is the primary entry point for hot
/// paths that match one query against thousands of targets (Quick Open).
#[derive(Debug, Clone)]
pub struct PreparedPattern {
    /// Original query string (for substring lookups in multi-term mode).
    pub(super) original: String,
    /// Lowercased version of the original query (for substring find).
    pub(super) lower_str: String,
    /// Individual whitespace-separated terms, pre-lowercased and indexed.
    pub(super) terms: Vec<PreparedTerm>,
}

/// A single search term pre-processed for fast matching.
#[derive(Debug, Clone)]
pub(super) struct PreparedTerm {
    /// Lowercased query term as a Vec<char> (needed for DP indexing).
    pub(super) lower_chars: Vec<char>,
    /// Lowercased ASCII bytes — `Some` iff the term is all-ASCII, which
    /// unlocks byte-level fast paths for rejection.
    pub(super) ascii_lower: Option<Vec<u8>>,
}

impl PreparedTerm {
    fn new(term: &str) -> Self {
        let lower = term.to_lowercase();
        let lower_chars: Vec<char> = lower.chars().collect();
        let ascii_lower = if lower.is_ascii() {
            Some(lower.into_bytes())
        } else {
            None
        };
        Self {
            lower_chars,
            ascii_lower,
        }
    }
}

impl PreparedPattern {
    /// Prepare a query for repeated fuzzy matching.
    pub fn new(query: &str) -> Self {
        let original = query.to_string();
        let lower_str = query.to_lowercase();
        let terms: Vec<PreparedTerm> = query.split_whitespace().map(PreparedTerm::new).collect();
        Self {
            original,
            lower_str,
            terms,
        }
    }

    /// Returns `true` if the prepared query has no terms (empty or whitespace-only).
    pub fn is_empty(&self) -> bool {
        self.terms.is_empty()
    }
}

/// Non-allocating subsequence check: does every lowercased character of the
/// prepared term appear, in order, somewhere in `target` (case-insensitive)?
///
/// This runs on the hot path for every (query, target) pair.  It uses an
/// ASCII byte-level fast path when the term and target are both ASCII
/// (the common case for file paths), otherwise it falls back to iterating
/// `target.chars().flat_map(|c| c.to_lowercase())` without allocating.
pub(super) fn is_subsequence_prepared(term: &PreparedTerm, target: &str) -> bool {
    if let Some(ref ascii_q) = term.ascii_lower {
        if target.is_ascii() {
            return is_subsequence_ascii(ascii_q, target.as_bytes());
        }
    }
    is_subsequence_chars(&term.lower_chars, target)
}

fn is_subsequence_ascii(query_lower: &[u8], target: &[u8]) -> bool {
    if query_lower.is_empty() {
        return true;
    }
    let mut qi = 0;
    for &b in target {
        let lower = b.to_ascii_lowercase();
        if lower == query_lower[qi] {
            qi += 1;
            if qi == query_lower.len() {
                return true;
            }
        }
    }
    false
}

fn is_subsequence_chars(query_lower: &[char], target: &str) -> bool {
    if query_lower.is_empty() {
        return true;
    }
    let mut qi = 0;
    for lc in target.chars().flat_map(|c| c.to_lowercase()) {
        if lc == query_lower[qi] {
            qi += 1;
            if qi == query_lower.len() {
                return true;
            }
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn prepared_pattern_splits_terms() {
        let p = PreparedPattern::new("Save File");
        assert_eq!(p.terms.len(), 2);
        assert_eq!(p.terms[0].lower_chars, vec!['s', 'a', 'v', 'e']);
        assert_eq!(p.terms[1].lower_chars, vec!['f', 'i', 'l', 'e']);
    }

    #[test]
    fn prepared_pattern_ascii_detection() {
        let p = PreparedPattern::new("hello");
        assert!(p.terms[0].ascii_lower.is_some());

        let p = PreparedPattern::new("héllo");
        assert!(p.terms[0].ascii_lower.is_none());
    }

    #[test]
    fn prepared_pattern_empty_when_whitespace_only() {
        assert!(PreparedPattern::new("").is_empty());
        assert!(PreparedPattern::new("   ").is_empty());
        assert!(!PreparedPattern::new("x").is_empty());
    }

    #[test]
    fn subsequence_ascii_accepts_interleaved_matches() {
        let t = PreparedTerm::new("sf");
        assert!(is_subsequence_prepared(&t, "Save File"));
        assert!(is_subsequence_prepared(&t, "SAVE FILE"));
        assert!(!is_subsequence_prepared(&t, "only s"));
        assert!(!is_subsequence_prepared(&t, "fs"));
    }

    #[test]
    fn subsequence_non_ascii_target_falls_back() {
        let t = PreparedTerm::new("hl");
        assert!(is_subsequence_prepared(&t, "héllo"));
    }

    #[test]
    fn subsequence_non_ascii_query() {
        let t = PreparedTerm::new("é");
        assert!(is_subsequence_prepared(&t, "hÉllo"));
        assert!(!is_subsequence_prepared(&t, "hello"));
    }
}
