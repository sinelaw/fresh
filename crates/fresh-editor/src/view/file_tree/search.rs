//! File explorer search functionality
//!
//! Provides fuzzy search for quick navigation in the file explorer.
//! Users can type characters to filter files/directories, with matching
//! characters highlighted in the results.

use crate::input::fuzzy::{fuzzy_match, FuzzyMatch};

/// Search state for file explorer
#[derive(Debug, Default, Clone)]
pub struct FileExplorerSearch {
    /// Current search query
    query: String,
}

impl FileExplorerSearch {
    /// Create a new empty search state
    pub fn new() -> Self {
        Self {
            query: String::new(),
        }
    }

    /// Get the current search query
    pub fn query(&self) -> &str {
        &self.query
    }

    /// Check if search is active (has query text)
    pub fn is_active(&self) -> bool {
        !self.query.is_empty()
    }

    /// Add a character to the search query
    pub fn push_char(&mut self, c: char) {
        self.query.push(c);
    }

    /// Remove the last character from the search query
    pub fn pop_char(&mut self) {
        self.query.pop();
    }

    /// Clear the search query
    pub fn clear(&mut self) {
        self.query.clear();
    }

    /// Match a file/directory name against the current query
    ///
    /// Returns Some(FuzzyMatch) if the name matches, None if search is inactive.
    /// The FuzzyMatch contains match positions for highlighting.
    pub fn match_name(&self, name: &str) -> Option<FuzzyMatch> {
        if self.query.is_empty() {
            return None;
        }
        let result = fuzzy_match(&self.query, name);
        if result.matched {
            Some(result)
        } else {
            None
        }
    }

    /// Check if a name matches the current search query
    pub fn matches(&self, name: &str) -> bool {
        if self.query.is_empty() {
            true // Empty query matches everything
        } else {
            fuzzy_match(&self.query, name).matched
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_search_matches_all() {
        let search = FileExplorerSearch::new();
        assert!(!search.is_active());
        assert!(search.matches("anything"));
        assert!(search.matches("file.txt"));
    }

    #[test]
    fn test_search_query_operations() {
        let mut search = FileExplorerSearch::new();

        search.push_char('f');
        assert_eq!(search.query(), "f");
        assert!(search.is_active());

        search.push_char('o');
        search.push_char('o');
        assert_eq!(search.query(), "foo");

        search.pop_char();
        assert_eq!(search.query(), "fo");

        search.clear();
        assert_eq!(search.query(), "");
        assert!(!search.is_active());
    }

    #[test]
    fn test_fuzzy_matching() {
        let mut search = FileExplorerSearch::new();
        search.push_char('m');
        search.push_char('r');
        search.push_char('s');

        // "mrs" should match "main.rs" (m...r.s)
        assert!(search.matches("main.rs"));

        // Should not match something without these chars in order
        assert!(!search.matches("test.txt"));
    }

    #[test]
    fn test_match_positions() {
        let mut search = FileExplorerSearch::new();
        search.push_char('m');
        search.push_char('r');

        let result = search.match_name("main.rs");
        assert!(result.is_some());

        let m = result.unwrap();
        assert_eq!(m.match_positions.len(), 2);
        assert_eq!(m.match_positions[0], 0); // 'm' at position 0
        assert_eq!(m.match_positions[1], 5); // 'r' at position 5
    }
}
