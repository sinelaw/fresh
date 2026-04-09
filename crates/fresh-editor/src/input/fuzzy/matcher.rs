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

use super::pattern::{is_subsequence_prepared, PreparedPattern, PreparedTerm};
use super::{score, FuzzyMatch};

/// Entry point for matching a prepared pattern against a target.
pub(super) fn match_prepared(pattern: &PreparedPattern, target: &str) -> FuzzyMatch {
    if pattern.is_empty() {
        return FuzzyMatch {
            matched: true,
            score: 0,
            match_positions: Vec::new(),
        };
    }

    if pattern.terms.len() > 1 {
        return match_multi_term(pattern, target);
    }

    match_single_term(&pattern.terms[0], target)
}

/// Match multiple space-separated terms against a target.
/// All terms must match for the overall match to succeed.
fn match_multi_term(pattern: &PreparedPattern, target: &str) -> FuzzyMatch {
    // Fast path: if any term can't be a subsequence of the target, reject
    // immediately without allocating per-term DP state.
    for term in &pattern.terms {
        if !is_subsequence_prepared(term, target) {
            return FuzzyMatch::no_match();
        }
    }

    let mut total_score = 0;
    let mut all_positions = Vec::new();

    for term in &pattern.terms {
        let result = match_single_term(term, target);
        if !result.matched {
            return FuzzyMatch::no_match();
        }
        total_score += result.score;
        all_positions.extend(result.match_positions);
    }

    // Sort and deduplicate positions (terms may have overlapping matches)
    all_positions.sort_unstable();
    all_positions.dedup();

    // Bonus: if the original query (with spaces) appears as a contiguous
    // substring in the target, give a significant bonus to prefer
    // exact/substring matches over scattered multi-term matches.
    let target_lower = target.to_lowercase();
    if let Some(start_pos) = target_lower.find(&pattern.lower_str) {
        total_score += score::EXACT_MATCH;

        // Rebuild positions to show the contiguous match instead of scattered matches
        let query_char_count = pattern.original.chars().count();
        let target_char_count = target.chars().count();

        let char_start = target
            .char_indices()
            .position(|(byte_idx, _)| byte_idx == start_pos)
            .unwrap_or(0);

        all_positions = (char_start..char_start + query_char_count)
            .filter(|&i| i < target_char_count)
            .collect();
    }

    FuzzyMatch {
        matched: true,
        score: total_score,
        match_positions: all_positions,
    }
}

/// Match a single prepared term against a target string.
fn match_single_term(term: &PreparedTerm, target: &str) -> FuzzyMatch {
    // Fast rejection: check subsequence WITHOUT allocating target `Vec<char>`.
    // Most files in a large corpus will fail this check for any given query,
    // so rejecting before the allocations saves the bulk of the work.
    if !is_subsequence_prepared(term, target) {
        return FuzzyMatch::no_match();
    }

    // Only now allocate the target char buffers needed for the scoring DP.
    let query_lower = &term.lower_chars;
    let target_chars: Vec<char> = target.chars().collect();
    let target_lower: Vec<char> = target.to_lowercase().chars().collect();
    let query_len = query_lower.len();
    let target_len = target_lower.len();

    // Try to find the best matching positions using a DP approach
    let dp_result = find_best_match(query_lower, &target_chars, &target_lower);

    // Also check for a contiguous substring match.  The DP may miss this
    // because it optimises per-character bonuses (word boundaries, etc.)
    // which can favour scattered matches over a tight substring.  We score
    // the contiguous match separately and take the better of the two.
    let substr_result = find_contiguous_match(query_lower, &target_chars, &target_lower);

    // Pick the better result
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

    // Check if all matched positions are consecutive (contiguous substring)
    let is_contiguous =
        positions.len() == query_len && positions.windows(2).all(|w| w[1] == w[0] + 1);

    if is_contiguous {
        // Contiguous substring bonus — the query appears as an
        // unbroken run in the target, which should always beat
        // scattered character matches.
        final_score += score::CONTIGUOUS_SUBSTRING;
    }

    // Exact match bonus: query matches entire target
    if query_len == target_len {
        final_score += score::EXACT_MATCH;
    } else if target_len > query_len && is_contiguous {
        // Check if the query is a prefix match (all consecutive from start)
        let is_prefix_match = positions.iter().enumerate().all(|(i, &pos)| pos == i);

        if is_prefix_match {
            let next_char = target_chars[query_len];

            // Highest priority: exact basename match (before extension)
            // This handles "config" matching "config.rs" better than "config_manager.rs"
            if next_char == '.' {
                final_score += score::EXACT_MATCH;
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
        // Check if query matches at this position
        if target_lower[start..start + m] != *query {
            continue;
        }

        // Score this contiguous match
        let mut match_score = 0;

        // Start of string bonus
        if start == 0 {
            match_score += score::START_OF_STRING;
        }

        // Word boundary bonus for the first character
        if start > 0 {
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

        // Consecutive bonus for chars 1..m
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

    // Quick rejection: callers that already rejected via
    // `is_subsequence_prepared` still pay for this check, but it's O(n) on
    // already-allocated slices so the cost is small and it guards
    // `find_best_match` when it's called in isolation (e.g. by tests or
    // future callers).
    {
        let mut qi = 0;
        for &tc in target_lower {
            if qi < m && tc == query[qi] {
                qi += 1;
            }
        }
        if qi < m {
            return None;
        }
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
