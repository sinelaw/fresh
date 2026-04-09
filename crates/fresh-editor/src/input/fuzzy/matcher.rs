//! The core fuzzy-matching algorithm.
//!
//! Given a [`PreparedPattern`] and a target string, this module scores the
//! match (or rejects it) using two complementary strategies:
//!
//! 1. A **DP pass** ([`find_best_match`]) that tracks the highest-scoring
//!    way to interleave query characters into the target, rewarding
//!    consecutive matches, word boundaries, camelCase transitions, etc.
//!    The DP uses an arena of backpointer nodes instead of cloning
//!    position vectors, keeping each state update O(1).
//!
//! 2. A **contiguous-substring pass** ([`find_contiguous_match`]) that
//!    finds literal-substring occurrences of the query and scores them.
//!    The DP sometimes prefers scattered matches with more word-boundary
//!    bonuses, so we always compare with the substring result and take
//!    whichever scores higher.
//!
//! Early rejection via [`super::pattern::is_subsequence_prepared`] happens
//! before any target-side allocation, so the heavy lifting here only runs
//! for candidates that could plausibly match.
//!
//! # Amortised hot path via [`FuzzyMatcher`]
//!
//! A [`FuzzyMatcher`] owns a [`PreparedPattern`] *and* two reusable
//! `Vec<char>` scratch buffers for the target.  Callers construct one per
//! keystroke and reuse it across all candidate targets:
//!
//! ```ignore
//! let mut matcher = FuzzyMatcher::new(search_query);
//! for file in files {
//!     let m = matcher.match_target(&file.relative_path);
//!     // ...
//! }
//! ```
//!
//! On the first call the scratch buffers grow to the size of the longest
//! target seen; every subsequent call truncates and refills them in place,
//! so the allocator is touched once per keystroke rather than once per
//! candidate.  This is the explicit alternative to stashing the scratch in
//! `thread_local!` state — lifetimes are visible in the signature and
//! future parallelisation can give each worker its own matcher.

use super::pattern::{is_subsequence_prepared, PreparedPattern, PreparedTerm};
use super::{score, FuzzyMatch};

/// A reusable fuzzy matcher that amortises both query preparation and
/// target-side scratch allocation across many calls.
///
/// See the module docs for usage.
#[derive(Debug, Clone)]
pub struct FuzzyMatcher {
    pattern: PreparedPattern,
    /// Scratch buffer: `target.chars().collect()`, cleared and refilled per call.
    target_chars: Vec<char>,
    /// Scratch buffer: lowercased target chars, cleared and refilled per call.
    target_lower: Vec<char>,
}

impl FuzzyMatcher {
    /// Create a new matcher for the given query, with empty scratch buffers.
    ///
    /// The scratch buffers start empty and grow lazily on the first
    /// [`match_target`](Self::match_target) call that gets past rejection.
    pub fn new(query: &str) -> Self {
        Self::from_pattern(PreparedPattern::new(query))
    }

    /// Construct a matcher from an already-built [`PreparedPattern`].
    pub fn from_pattern(pattern: PreparedPattern) -> Self {
        Self {
            pattern,
            target_chars: Vec::new(),
            target_lower: Vec::new(),
        }
    }

    /// Returns `true` if the prepared query has no terms (empty / whitespace).
    pub fn is_empty(&self) -> bool {
        self.pattern.is_empty()
    }

    /// Match a single target string against this matcher's query.
    ///
    /// Reuses the internal `Vec<char>` scratch buffers — no per-call
    /// allocations on the common "passed rejection → scoring" path after
    /// the first call has warmed them up.
    pub fn match_target(&mut self, target: &str) -> FuzzyMatch {
        if self.pattern.is_empty() {
            return FuzzyMatch {
                matched: true,
                score: 0,
                match_positions: Vec::new(),
            };
        }

        // Fast rejection gate.  Runs entirely on `&str`, no allocation.
        // In the multi-term case we must pass the gate for *every* term.
        for term in &self.pattern.terms {
            if !is_subsequence_prepared(term, target) {
                return FuzzyMatch::no_match();
            }
        }

        // Refill scratch buffers in place.  No `malloc`/`realloc` on
        // steady-state — the existing capacity is reused after the first
        // call that made them large enough for the corpus's worst case.
        refill_target(&mut self.target_chars, &mut self.target_lower, target);

        // Split the borrow so the scoring helpers can see both the pattern
        // and the scratch buffers simultaneously without fighting the
        // borrow checker.
        let pattern = &self.pattern;
        let target_chars: &[char] = &self.target_chars;
        let target_lower: &[char] = &self.target_lower;

        if pattern.terms.len() > 1 {
            score_multi_term(pattern, target_chars, target_lower)
        } else {
            score_single_term(&pattern.terms[0], target_chars, target_lower)
        }
    }
}

/// Entry point used by the backward-compatible free functions.
///
/// Creates a temporary `FuzzyMatcher` (which allocates empty scratch Vecs
/// but does not grow them until first use), so callers using the legacy
/// `fuzzy_match_prepared` API still work, just without cross-call
/// amortisation.  Hot-path callers should construct their own
/// `FuzzyMatcher` and reuse it.
pub(super) fn match_prepared(pattern: &PreparedPattern, target: &str) -> FuzzyMatch {
    let mut matcher = FuzzyMatcher::from_pattern(pattern.clone());
    matcher.match_target(target)
}

/// Refill the scratch buffers with the raw and lowercased characters of
/// `target`.  Grows the buffers if needed; otherwise reuses existing
/// capacity, so steady-state calls do zero allocations.
///
/// Note: for non-ASCII chars with expanding case mappings (e.g. Turkish
/// `İ` → `i` + combining dot), `target_lower` can be longer than
/// `target_chars`.  This matches the semantics of the previous
/// `target.to_lowercase().chars().collect()` implementation, including
/// its latent indexing inconsistency — preserving behaviour is out of
/// scope for this change.
fn refill_target(target_chars: &mut Vec<char>, target_lower: &mut Vec<char>, target: &str) {
    target_chars.clear();
    target_lower.clear();
    // `target.len()` is the byte length, which is an upper bound on the
    // character count (chars are 1-4 bytes in UTF-8).  Reserving once
    // avoids any growth checks during the push loop.
    target_chars.reserve(target.len());
    target_lower.reserve(target.len());
    for c in target.chars() {
        target_chars.push(c);
        for lc in c.to_lowercase() {
            target_lower.push(lc);
        }
    }
}

/// Score a single prepared term against a target whose char buffers have
/// already been refilled in the caller's scratch space.
fn score_single_term(
    term: &PreparedTerm,
    target_chars: &[char],
    target_lower: &[char],
) -> FuzzyMatch {
    let query_lower = &term.lower_chars;
    let query_len = query_lower.len();
    let target_len = target_lower.len();

    // Try to find the best matching positions using a DP approach.
    let dp_result = find_best_match(query_lower, target_chars, target_lower);

    // Also check for a contiguous substring match.  The DP may miss this
    // because it optimises per-character bonuses (word boundaries, etc.)
    // which can favour scattered matches over a tight substring.
    let substr_result = find_contiguous_match(query_lower, target_chars, target_lower);

    // Pick the better result.
    let (positions, mut final_score) = match (dp_result, substr_result) {
        (Some(dp), Some(sub)) => {
            if sub.1 >= dp.1 {
                sub
            } else {
                dp
            }
        }
        (Some(dp), None) => dp,
        (None, Some(sub)) => sub,
        (None, None) => return FuzzyMatch::no_match(),
    };

    // Check if all matched positions are consecutive (contiguous substring).
    let is_contiguous =
        positions.len() == query_len && positions.windows(2).all(|w| w[1] == w[0] + 1);

    if is_contiguous {
        final_score += score::CONTIGUOUS_SUBSTRING;
    }

    // Exact match bonus: query matches entire target.
    if query_len == target_len {
        final_score += score::EXACT_MATCH;
    } else if target_len > query_len && is_contiguous {
        // Check if the query is a prefix match (all consecutive from start).
        let is_prefix_match = positions.iter().enumerate().all(|(i, &pos)| pos == i);

        if is_prefix_match && query_len < target_chars.len() {
            let next_char = target_chars[query_len];
            if next_char == '.' {
                // Highest priority: exact basename match (before extension).
                final_score += score::EXACT_MATCH;
            } else if next_char == '-' || next_char == '_' || next_char == ' ' {
                // Second priority: match before word separator.
                final_score += score::EXACT_BASENAME_MATCH;
            }
        }
    }

    FuzzyMatch {
        matched: true,
        score: final_score,
        match_positions: positions,
    }
}

/// Score a multi-term pattern against a target whose char buffers have
/// already been refilled in the caller's scratch space.
fn score_multi_term(
    pattern: &PreparedPattern,
    target_chars: &[char],
    target_lower: &[char],
) -> FuzzyMatch {
    let mut total_score = 0;
    let mut all_positions = Vec::new();

    for term in &pattern.terms {
        let result = score_single_term(term, target_chars, target_lower);
        if !result.matched {
            return FuzzyMatch::no_match();
        }
        total_score += result.score;
        all_positions.extend(result.match_positions);
    }

    // Sort and deduplicate positions (terms may have overlapping matches).
    all_positions.sort_unstable();
    all_positions.dedup();

    // Tight-span bonus: reward targets where each term appears as a
    // contiguous substring and all the term matches are packed close
    // together (regardless of what sits between them).  This handles
    // every realistic "the query reconstructs a single name" case
    // uniformly — "/etc/hosts", "save_file.rs", "saveFile.rs",
    // "etcmohosts", even "foo.bar.baz" — without hard-coding a list
    // of separator characters that would miss camelCase, runs-together
    // words, or unusual punctuation.
    //
    // The bonus fires iff:
    //   1. Every term appears as a contiguous substring in `target_lower`,
    //   2. In the order given by the query, and
    //   3. The gap between consecutive term matches is small (we allow
    //      up to 4 chars per gap — generous enough for "etcmohosts"
    //      and "foo/bar/baz" but not for two terms at opposite ends
    //      of a long path).
    if let Some((span_start, span_end)) = find_sequential_term_span(target_lower, &pattern.terms) {
        let term_len_sum: usize = pattern.terms.iter().map(|t| t.lower_chars.len()).sum();
        let span_len = span_end - span_start;
        let gap_excess = span_len.saturating_sub(term_len_sum);
        let num_gaps = pattern.terms.len().saturating_sub(1);
        // Tolerance scales with the number of gaps: 4 chars per gap.
        let max_gap_excess = num_gaps.saturating_mul(4);

        if gap_excess <= max_gap_excess {
            total_score += score::EXACT_MATCH;

            let target_char_count = target_chars.len();
            all_positions = (span_start..span_end)
                .filter(|&i| i < target_char_count)
                .collect();
        }
    }

    FuzzyMatch {
        matched: true,
        score: total_score,
        match_positions: all_positions,
    }
}

/// Greedily find the first sequential occurrence of each term as a
/// contiguous substring in `target_lower`, advancing past each match
/// before searching for the next term.  Returns the `(start, end)`
/// char indices of the overall span covering the first term's start
/// through the last term's end, or `None` if any term can't be found
/// contiguously in the required order.
///
/// "Greedy" here means we take the *earliest* occurrence of each term,
/// not the tightest span — tighter spans would require backtracking
/// and are not worth the cost for a ranking signal.
fn find_sequential_term_span(
    target_lower: &[char],
    terms: &[PreparedTerm],
) -> Option<(usize, usize)> {
    let mut search_from = 0;
    let mut first_start: Option<usize> = None;
    let mut last_end = 0;
    for term in terms {
        let needle = &term.lower_chars;
        if needle.is_empty() {
            continue;
        }
        if search_from + needle.len() > target_lower.len() {
            return None;
        }
        let end_bound = target_lower.len() - needle.len();
        let found =
            (search_from..=end_bound).find(|&s| target_lower[s..s + needle.len()] == *needle)?;
        if first_start.is_none() {
            first_start = Some(found);
        }
        search_from = found + needle.len();
        last_end = search_from;
    }
    first_start.map(|start| (start, last_end))
}

/// Find the best contiguous substring match of `query` in `target`.
///
/// Scans for all occurrences of the query as a substring and picks the
/// one with the highest score (preferring word boundaries, basename, etc.).
fn find_contiguous_match(
    query: &[char],
    target_chars: &[char],
    target_lower: &[char],
) -> Option<(Vec<usize>, i32)> {
    let m = query.len();
    let n = target_lower.len();
    if m == 0 || m > n {
        return None;
    }

    let mut best: Option<(Vec<usize>, i32)> = None;

    for start in 0..=n - m {
        // Check if query matches at this position.
        if target_lower[start..start + m] != *query {
            continue;
        }

        // Score this contiguous match.
        let mut match_score = 0;

        if start == 0 {
            match_score += score::START_OF_STRING;
        }

        // Word boundary bonus for the first character.
        if start > 0 && start < target_chars.len() {
            let prev_char = target_chars[start - 1];
            if prev_char == ' '
                || prev_char == '_'
                || prev_char == '-'
                || prev_char == '/'
                || prev_char == '.'
            {
                match_score += score::WORD_BOUNDARY;
            } else if prev_char.is_lowercase() && target_chars[start].is_uppercase() {
                match_score += score::CAMEL_CASE;
            }
        }

        // Consecutive bonus for chars 1..m.
        match_score += score::CONSECUTIVE * (m as i32 - 1);

        let is_better = match &best {
            None => true,
            Some((_, s)) => match_score > *s,
        };
        if is_better {
            let positions: Vec<usize> = (start..start + m).collect();
            best = Some((positions, match_score));
        }
    }

    best
}

/// A single node in the backpointer arena used by [`find_best_match`].
///
/// Each node records the target index matched for one query character and a
/// link to the node that matched the previous query character (or `None` for
/// the first match).  Walking back from the final node reconstructs the full
/// list of match positions without ever cloning a `Vec<usize>`.
#[derive(Clone, Copy)]
struct ChainNode {
    ti: usize,
    prev: Option<u32>,
}

/// Find the best matching positions for query in target.
///
/// Same greedy DP as the original implementation, but replaces the
/// `Vec<usize>` position-cloning with an arena of linked backpointer nodes.
/// Per-state updates become O(1) (push one node) instead of O(m) (clone the
/// full positions vector), turning the worst-case cost from O(n·m²) into
/// O(n·m) with a single linear walk at the end to reconstruct positions.
///
/// Callers are expected to have already run subsequence rejection via
/// [`super::pattern::is_subsequence_prepared`] before allocating the
/// target buffers this function consumes — the duplicate guard that used
/// to live here was pure overhead on the hot path and has been removed.
fn find_best_match(
    query: &[char],
    target_chars: &[char],
    target_lower: &[char],
) -> Option<(Vec<usize>, i32)> {
    if query.is_empty() {
        return Some((Vec::new(), 0));
    }

    let n = target_lower.len();
    let m = query.len();

    if n < m {
        return None;
    }

    // Arena of backpointer nodes.  `best_node_for_qi[qi]` indexes into the
    // arena for the currently-best chain ending at query index `qi`, or
    // `None` if no match for `query[0..qi]` has been seen yet.  `qi == 0`
    // is always `None` (empty chain).
    let mut arena: Vec<ChainNode> = Vec::with_capacity(m.saturating_mul(4));
    let mut best_score: Vec<Option<i32>> = vec![None; m + 1];
    let mut best_node_for_qi: Vec<Option<u32>> = vec![None; m + 1];
    best_score[0] = Some(0); // empty query matches with score 0

    for ti in 0..n {
        // Process in reverse so we don't use values we just wrote this iteration.
        for qi in (0..m).rev() {
            if target_lower[ti] != query[qi] {
                continue;
            }

            // Can we extend the best chain for query[0..qi]?
            let prev_score = match best_score[qi] {
                Some(s) => s,
                None => continue,
            };
            // `last_match_pos` comes from the previous chain head, or None
            // if `qi == 0` (first character of the query).
            let prev_last_pos = best_node_for_qi[qi].map(|idx| arena[idx as usize].ti);

            // Match positions must be strictly increasing.
            if let Some(lp) = prev_last_pos {
                if ti <= lp {
                    continue;
                }
            }

            // Score the (ti, prev_last_pos) transition.
            let mut match_score = 0;

            // Start of string bonus
            if ti == 0 {
                match_score += score::START_OF_STRING;
            }

            // Word boundary bonus
            if ti > 0 && ti < target_chars.len() {
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

            // Consecutive / gap handling
            if let Some(lp) = prev_last_pos {
                if ti == lp + 1 {
                    match_score += score::CONSECUTIVE;
                } else {
                    let gap_size = ti - lp - 1;
                    match_score += score::GAP_START_PENALTY;
                    match_score += score::GAP_PENALTY * (gap_size as i32 - 1).max(0);
                }
            }

            let new_score = prev_score + match_score;

            let should_update = match best_score[qi + 1] {
                None => true,
                Some(curr) => new_score > curr,
            };

            if should_update {
                let new_idx = arena.len() as u32;
                arena.push(ChainNode {
                    ti,
                    prev: best_node_for_qi[qi],
                });
                best_score[qi + 1] = Some(new_score);
                best_node_for_qi[qi + 1] = Some(new_idx);
            }
        }
    }

    let final_score = best_score[m]?;
    let final_node = best_node_for_qi[m]?;

    // Walk backwards through the arena to recover positions.
    let mut positions = vec![0usize; m];
    let mut cursor = Some(final_node);
    let mut idx = m;
    while let Some(node_idx) = cursor {
        debug_assert!(idx > 0);
        idx -= 1;
        let node = arena[node_idx as usize];
        positions[idx] = node.ti;
        cursor = node.prev;
    }

    Some((positions, final_score))
}
