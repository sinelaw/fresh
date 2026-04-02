//! k-Deletion Neighborhood index for O(1) fuzzy symbol resolution.
//!
//! For every symbol, we precompute its k=1 and k=2 deletion variants
//! (e.g., `socket` -> `ocket`, `scket`, `soket`, `socet`, `sockt`, `socke`).
//!
//! To prevent combinatorial explosion for long strings, we use truncated
//! deletion neighborhoods — only indexing variants of a fixed-length prefix.

use std::collections::HashMap;

use ahash::RandomState;

/// Maximum prefix length for deletion neighborhood computation.
/// Captures the vast majority of human typing errors without unbounded memory.
const MAX_PREFIX_LEN: usize = 12;

/// A fuzzy symbol index using precomputed deletion neighborhoods.
pub struct DeletionIndex {
    /// Maps deletion-variant hashes -> canonical symbol names
    index: HashMap<u64, Vec<String>, RandomState>,
    /// All canonical symbols
    symbols: Vec<String>,
}

impl DeletionIndex {
    pub fn new() -> Self {
        Self {
            index: HashMap::with_hasher(RandomState::new()),
            symbols: Vec::new(),
        }
    }

    /// Add a symbol to the fuzzy index, precomputing its deletion neighborhoods.
    pub fn insert(&mut self, symbol: &str) {
        self.symbols.push(symbol.to_string());

        // Index the symbol itself
        let hasher = self.index.hasher().clone();
        let hash = hasher.hash_one(symbol);
        self.index.entry(hash).or_default().push(symbol.to_string());

        // Truncate to prefix for neighborhood generation
        let prefix: String = symbol.chars().take(MAX_PREFIX_LEN).collect();

        // k=1 deletion variants
        let k1_variants = deletion_variants(&prefix, 1);
        for variant in &k1_variants {
            let h = hasher.hash_one(variant.as_str());
            self.index.entry(h).or_default().push(symbol.to_string());
        }

        // k=2 deletion variants
        let k2_variants = deletion_variants(&prefix, 2);
        for variant in &k2_variants {
            let h = hasher.hash_one(variant.as_str());
            self.index.entry(h).or_default().push(symbol.to_string());
        }
    }

    /// Resolve a potentially misspelled query to canonical symbols.
    ///
    /// Computes the 1-deletion and 2-deletion neighborhoods of the query
    /// on the fly and checks each against the precomputed index.
    pub fn resolve(&self, query: &str) -> Vec<&str> {
        let mut results = Vec::new();
        let hasher = self.index.hasher().clone();
        let prefix: String = query.chars().take(MAX_PREFIX_LEN).collect();

        // Exact match
        let hash = hasher.hash_one(query);
        if let Some(matches) = self.index.get(&hash) {
            for m in matches {
                if !results.contains(&m.as_str()) {
                    results.push(m.as_str());
                }
            }
        }

        // k=1 deletion variants of the query
        for variant in deletion_variants(&prefix, 1) {
            let h = hasher.hash_one(variant.as_str());
            if let Some(matches) = self.index.get(&h) {
                for m in matches {
                    if !results.contains(&m.as_str()) {
                        results.push(m.as_str());
                    }
                }
            }
        }

        // k=2 for more aggressive fuzzy matching
        for variant in deletion_variants(&prefix, 2) {
            let h = hasher.hash_one(variant.as_str());
            if let Some(matches) = self.index.get(&h) {
                for m in matches {
                    if !results.contains(&m.as_str()) {
                        results.push(m.as_str());
                    }
                }
            }
        }

        results
    }

    /// Number of canonical symbols in the index.
    pub fn len(&self) -> usize {
        self.symbols.len()
    }

    pub fn is_empty(&self) -> bool {
        self.symbols.is_empty()
    }

    /// Clear the entire index.
    pub fn clear(&mut self) {
        self.index.clear();
        self.symbols.clear();
    }
}

impl Default for DeletionIndex {
    fn default() -> Self {
        Self::new()
    }
}

/// Generate all k-deletion variants of a string.
///
/// A 1-deletion variant is the string with exactly one character removed.
/// A 2-deletion variant has exactly two characters removed.
fn deletion_variants(s: &str, k: usize) -> Vec<String> {
    if k == 0 {
        return vec![s.to_string()];
    }

    let chars: Vec<char> = s.chars().collect();
    if chars.len() <= k {
        return vec![];
    }

    let mut results = Vec::new();

    if k == 1 {
        for i in 0..chars.len() {
            let mut variant: String = String::with_capacity(chars.len() - 1);
            for (j, &ch) in chars.iter().enumerate() {
                if j != i {
                    variant.push(ch);
                }
            }
            results.push(variant);
        }
    } else {
        // k >= 2: recursively generate by removing one char and then k-1 more
        for i in 0..chars.len() {
            let reduced: String = chars
                .iter()
                .enumerate()
                .filter(|&(j, _)| j != i)
                .map(|(_, &ch)| ch)
                .collect();
            for variant in deletion_variants(&reduced, k - 1) {
                if !results.contains(&variant) {
                    results.push(variant);
                }
            }
        }
    }

    results
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn exact_match() {
        let mut idx = DeletionIndex::new();
        idx.insert("socket");

        let results = idx.resolve("socket");
        assert!(results.contains(&"socket"));
    }

    #[test]
    fn typo_resolution_transposition() {
        let mut idx = DeletionIndex::new();
        idx.insert("socket");

        // "sokcet" is a transposition error. Its 1-deletion neighborhood
        // includes "soket", which is also in socket's 1-deletion neighborhood.
        let results = idx.resolve("sokcet");
        assert!(
            results.contains(&"socket"),
            "Should resolve transposition typo"
        );
    }

    #[test]
    fn typo_resolution_extra_char() {
        let mut idx = DeletionIndex::new();
        idx.insert("process");

        // "processs" has an extra 's' — its 1-deletion includes "process"
        let results = idx.resolve("processs");
        assert!(
            results.contains(&"process"),
            "Should resolve extra-char typo"
        );
    }

    #[test]
    fn deletion_variants_k1() {
        let variants = deletion_variants("abc", 1);
        assert_eq!(variants.len(), 3);
        assert!(variants.contains(&"bc".to_string()));
        assert!(variants.contains(&"ac".to_string()));
        assert!(variants.contains(&"ab".to_string()));
    }
}
