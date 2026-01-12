//! Input history for prompt navigation
//!
//! This module provides a history mechanism for prompts, similar to bash/readline.
//! Users can navigate through previously entered values using up/down arrow keys.
//!
//! ## Design Goals
//!
//! 1. **Intuitive navigation**: Behaves like bash/readline history
//!    - Up arrow moves to previous (older) items
//!    - Down arrow moves to next (newer) items
//!    - Pressing down past the last item returns to current input
//!
//! 2. **Non-destructive editing**: Editing historical items doesn't modify stored history
//!    - History items are immutable once stored
//!    - Edits only affect the current prompt input
//!
//! 3. **Persistence-ready**: Designed for future file-based persistence
//!    - Simple structure that can be serialized (Vec<String>)
//!    - Placeholder methods for save/load operations
//!    - Separate histories for different prompt types (search vs replace)
//!
//! ## Usage Example
//!
//! ```
//! use fresh::input_history::InputHistory;
//!
//! let mut history = InputHistory::new();
//!
//! // Add items to history
//! history.push("first search".to_string());
//! history.push("second search".to_string());
//!
//! // Navigate backwards (up arrow)
//! let prev = history.navigate_prev("current input");
//! assert_eq!(prev, Some("second search".to_string()));
//!
//! // Navigate backwards again
//! let prev2 = history.navigate_prev("current input");
//! assert_eq!(prev2, Some("first search".to_string()));
//!
//! // Navigate forwards (down arrow)
//! let next = history.navigate_next();
//! assert_eq!(next, Some("second search".to_string()));
//!
//! // Navigate past the end returns to original input
//! let next2 = history.navigate_next();
//! assert_eq!(next2, Some("current input".to_string()));
//! ```

/// Input history for prompt navigation (like bash/readline)
///
/// This struct maintains a history of previously entered values
/// and allows navigating through them with up/down arrows.
///
/// ## Navigation Behavior
///
/// - History items are stored in a Vec (oldest to newest)
/// - `position = None` means "at current input" (not navigating)
/// - `position = Some(i)` means "viewing history item i"
/// - When you first press up, current input is saved to `temp_input`
/// - When you navigate past the end (down from last item), `temp_input` is restored
///
/// ## Future Persistence
///
/// To add persistence later:
/// - Implement `serde::Serialize` and `serde::Deserialize`
/// - Add methods: `save_to_file()`, `load_from_file()`
/// - Store in config directory, separate files per history type
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct InputHistory {
    /// History items (oldest to newest)
    items: Vec<String>,
    /// Maximum number of items to keep
    max_size: usize,
    /// Current navigation position
    /// - None = at current input (not navigating)
    /// - Some(index) = viewing history item at index
    position: Option<usize>,
    /// Temporary storage for current input when navigating away
    temp_input: Option<String>,
}

impl InputHistory {
    /// Default maximum history size
    pub const DEFAULT_MAX_SIZE: usize = 100;

    /// Create a new history with default capacity (100 items)
    pub fn new() -> Self {
        Self::with_capacity(Self::DEFAULT_MAX_SIZE)
    }

    /// Create a new history with specified capacity
    ///
    /// # Arguments
    /// * `max_size` - Maximum number of history items to keep (must be > 0)
    ///
    /// # Panics
    /// Panics if `max_size` is 0
    pub fn with_capacity(max_size: usize) -> Self {
        assert!(max_size > 0, "History max_size must be greater than 0");
        Self {
            items: Vec::new(),
            max_size,
            position: None,
            temp_input: None,
        }
    }

    /// Add an item to history (most recent)
    ///
    /// This method:
    /// - Skips empty strings
    /// - Skips exact duplicates of the most recent item
    /// - Enforces max_size by removing oldest items
    /// - Resets navigation state
    ///
    /// # Example
    /// ```
    /// # use fresh::input_history::InputHistory;
    /// let mut history = InputHistory::new();
    /// history.push("first".to_string());
    /// history.push("second".to_string());
    /// history.push("second".to_string()); // Skipped (duplicate)
    /// assert_eq!(history.len(), 2);
    /// ```
    pub fn push(&mut self, item: String) {
        // Skip empty strings
        if item.is_empty() {
            return;
        }

        // Skip duplicates of the most recent item
        if self.items.last().map(|s| s.as_str()) == Some(item.as_str()) {
            return;
        }

        // Add the item
        self.items.push(item);

        // Enforce max size by removing oldest items
        while self.items.len() > self.max_size {
            self.items.remove(0);
        }

        // Reset navigation state
        self.reset_navigation();
    }

    /// Navigate to previous item in history (up arrow)
    ///
    /// On first call, saves `current_input` to temporary storage and returns
    /// the most recent history item. On subsequent calls, moves backwards
    /// through history.
    ///
    /// # Arguments
    /// * `current_input` - The current prompt input (saved on first navigation)
    ///
    /// # Returns
    /// * `Some(String)` - The previous history item
    /// * `None` - No more items (already at oldest)
    ///
    /// # Example
    /// ```
    /// # use fresh::input_history::InputHistory;
    /// let mut history = InputHistory::new();
    /// history.push("first".to_string());
    /// history.push("second".to_string());
    ///
    /// let prev = history.navigate_prev("typing...");
    /// assert_eq!(prev, Some("second".to_string()));
    ///
    /// let prev2 = history.navigate_prev("typing...");
    /// assert_eq!(prev2, Some("first".to_string()));
    ///
    /// let prev3 = history.navigate_prev("typing...");
    /// assert_eq!(prev3, None); // Already at oldest
    /// ```
    pub fn navigate_prev(&mut self, current_input: &str) -> Option<String> {
        if self.items.is_empty() {
            return None;
        }

        match self.position {
            None => {
                // First navigation: save current input and go to last item
                self.temp_input = Some(current_input.to_string());
                self.position = Some(self.items.len() - 1);
                Some(self.items[self.items.len() - 1].clone())
            }
            Some(pos) if pos > 0 => {
                // Navigate to previous item
                self.position = Some(pos - 1);
                Some(self.items[pos - 1].clone())
            }
            Some(_) => {
                // Already at oldest item
                None
            }
        }
    }

    /// Navigate to next item in history (down arrow)
    ///
    /// Moves forward through history (towards more recent items).
    /// When navigating past the most recent item, returns the original
    /// input that was saved when navigation started.
    ///
    /// # Returns
    /// * `Some(String)` - The next history item, or original input if past end
    /// * `None` - Not currently navigating
    ///
    /// # Example
    /// ```
    /// # use fresh::input_history::InputHistory;
    /// let mut history = InputHistory::new();
    /// history.push("first".to_string());
    /// history.push("second".to_string());
    ///
    /// // Start navigating backwards
    /// history.navigate_prev("typing...");
    /// history.navigate_prev("typing...");
    ///
    /// // Navigate forwards
    /// let next = history.navigate_next();
    /// assert_eq!(next, Some("second".to_string()));
    ///
    /// // Navigate past the end returns to original input
    /// let next2 = history.navigate_next();
    /// assert_eq!(next2, Some("typing...".to_string()));
    /// ```
    pub fn navigate_next(&mut self) -> Option<String> {
        match self.position {
            None => {
                // Not navigating
                None
            }
            Some(pos) if pos < self.items.len() - 1 => {
                // Navigate to next item
                self.position = Some(pos + 1);
                Some(self.items[pos + 1].clone())
            }
            Some(_) => {
                // At most recent item, return to original input
                let original = self.temp_input.clone();
                self.reset_navigation();
                original
            }
        }
    }

    /// Reset navigation state
    ///
    /// Call this when:
    /// - User confirms the prompt (Enter)
    /// - User cancels the prompt (Escape)
    /// - User starts typing (optional, depends on desired behavior)
    ///
    /// This clears the temporary input storage and resets the position.
    pub fn reset_navigation(&mut self) {
        self.position = None;
        self.temp_input = None;
    }

    /// Get the most recent item without navigating
    ///
    /// Useful for pre-filling prompts with the last search term.
    ///
    /// # Example
    /// ```
    /// # use fresh::input_history::InputHistory;
    /// let mut history = InputHistory::new();
    /// history.push("last search".to_string());
    /// assert_eq!(history.last(), Some("last search"));
    /// ```
    pub fn last(&self) -> Option<&str> {
        self.items.last().map(|s| s.as_str())
    }

    /// Initialize navigation at the last history item
    ///
    /// Call this when pre-filling a prompt with the last history item.
    /// This sets up the navigation state so that pressing Up will go to
    /// the second-to-last item, not the last item again.
    ///
    /// # Example
    /// ```
    /// # use fresh::input_history::InputHistory;
    /// let mut history = InputHistory::new();
    /// history.push("first".to_string());
    /// history.push("second".to_string());
    /// history.push("third".to_string());
    ///
    /// // Pre-fill prompt with "third"
    /// history.init_at_last();
    ///
    /// // Now Up goes to "second", not "third"
    /// let prev = history.navigate_prev("third");
    /// assert_eq!(prev, Some("second".to_string()));
    /// ```
    pub fn init_at_last(&mut self) {
        if !self.items.is_empty() {
            self.position = Some(self.items.len() - 1);
            self.temp_input = None;
        }
    }

    /// Check if history is empty
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    /// Get number of items in history
    pub fn len(&self) -> usize {
        self.items.len()
    }

    /// Clear all history
    ///
    /// Removes all items and resets navigation state.
    pub fn clear(&mut self) {
        self.items.clear();
        self.reset_navigation();
    }

    /// Get a reference to the history items (oldest to newest)
    ///
    /// Useful for session persistence.
    pub fn items(&self) -> &[String] {
        &self.items
    }

    /// Create a history from existing items
    ///
    /// Useful for session restoration.
    pub fn from_items(items: Vec<String>) -> Self {
        let mut history = Self::new();
        // Add items respecting deduplication rules
        for item in items {
            history.push(item);
        }
        history
    }

    // ========================================================================
    // Persistence methods
    // ========================================================================

    /// Save history to a file
    pub fn save_to_file(&self, path: &std::path::Path) -> std::io::Result<()> {
        // Only save items, not navigation state
        let json = serde_json::to_string_pretty(&self.items)
            .map_err(std::io::Error::other)?;

        // Create parent directory if it doesn't exist
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }

        std::fs::write(path, json)?;
        Ok(())
    }

    /// Load history from a file
    pub fn load_from_file(path: &std::path::Path) -> std::io::Result<Self> {
        if !path.exists() {
            return Ok(Self::new());
        }

        let json = std::fs::read_to_string(path)?;
        let items: Vec<String> = serde_json::from_str(&json)
            .map_err(std::io::Error::other)?;

        let mut history = Self::new();
        history.items = items;

        // Trim to max_size if file had more items
        if history.items.len() > history.max_size {
            let excess = history.items.len() - history.max_size;
            history.items.drain(0..excess);
        }

        Ok(history)
    }
}

/// Get the data directory for Fresh editor state
/// Returns $XDG_DATA_HOME/fresh or ~/.local/share/fresh on Linux
/// Returns ~/Library/Application Support/fresh on macOS
pub fn get_data_dir() -> std::io::Result<std::path::PathBuf> {
    let data_dir = dirs::data_dir().ok_or_else(|| {
        std::io::Error::new(
            std::io::ErrorKind::NotFound,
            "Could not determine data directory",
        )
    })?;
    Ok(data_dir.join("fresh"))
}

/// Get the path for search history file
pub fn get_search_history_path() -> std::io::Result<std::path::PathBuf> {
    Ok(get_data_dir()?.join("search_history.json"))
}

/// Get the path for replace history file
pub fn get_replace_history_path() -> std::io::Result<std::path::PathBuf> {
    Ok(get_data_dir()?.join("replace_history.json"))
}

impl Default for InputHistory {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_history_is_empty() {
        let history = InputHistory::new();
        assert!(history.is_empty());
        assert_eq!(history.len(), 0);
        assert_eq!(history.last(), None);
    }

    #[test]
    fn test_push_adds_items() {
        let mut history = InputHistory::new();
        history.push("first".to_string());
        history.push("second".to_string());
        history.push("third".to_string());

        assert_eq!(history.len(), 3);
        assert_eq!(history.last(), Some("third"));
    }

    #[test]
    fn test_push_skips_empty_strings() {
        let mut history = InputHistory::new();
        history.push("first".to_string());
        history.push("".to_string());
        history.push("second".to_string());

        assert_eq!(history.len(), 2);
    }

    #[test]
    fn test_push_skips_consecutive_duplicates() {
        let mut history = InputHistory::new();
        history.push("first".to_string());
        history.push("second".to_string());
        history.push("second".to_string());
        history.push("second".to_string());
        history.push("third".to_string());

        assert_eq!(history.len(), 3);
        assert_eq!(history.items, vec!["first", "second", "third"]);
    }

    #[test]
    fn test_push_allows_non_consecutive_duplicates() {
        let mut history = InputHistory::new();
        history.push("search".to_string());
        history.push("other".to_string());
        history.push("search".to_string()); // Should be added

        assert_eq!(history.len(), 3);
        assert_eq!(history.items, vec!["search", "other", "search"]);
    }

    #[test]
    fn test_navigate_prev_empty_history() {
        let mut history = InputHistory::new();
        let result = history.navigate_prev("current");
        assert_eq!(result, None);
    }

    #[test]
    fn test_navigate_prev_basic() {
        let mut history = InputHistory::new();
        history.push("first".to_string());
        history.push("second".to_string());
        history.push("third".to_string());

        // First up: go to most recent
        let prev = history.navigate_prev("typing...");
        assert_eq!(prev, Some("third".to_string()));

        // Second up: go to previous
        let prev = history.navigate_prev("typing...");
        assert_eq!(prev, Some("second".to_string()));

        // Third up: go to oldest
        let prev = history.navigate_prev("typing...");
        assert_eq!(prev, Some("first".to_string()));

        // Fourth up: no more items
        let prev = history.navigate_prev("typing...");
        assert_eq!(prev, None);
    }

    #[test]
    fn test_navigate_next_without_prev() {
        let mut history = InputHistory::new();
        history.push("item".to_string());

        // navigate_next without navigate_prev should return None
        let result = history.navigate_next();
        assert_eq!(result, None);
    }

    #[test]
    fn test_navigate_next_returns_to_original() {
        let mut history = InputHistory::new();
        history.push("first".to_string());
        history.push("second".to_string());

        // Navigate backwards
        history.navigate_prev("typing...");
        history.navigate_prev("typing...");

        // Navigate forwards
        let next = history.navigate_next();
        assert_eq!(next, Some("second".to_string()));

        // Navigate past the end should return original input
        let next = history.navigate_next();
        assert_eq!(next, Some("typing...".to_string()));

        // After returning to original, we're no longer navigating
        let next = history.navigate_next();
        assert_eq!(next, None);
    }

    #[test]
    fn test_reset_navigation() {
        let mut history = InputHistory::new();
        history.push("item".to_string());

        // Start navigating
        history.navigate_prev("current");
        assert!(history.position.is_some());
        assert!(history.temp_input.is_some());

        // Reset
        history.reset_navigation();
        assert!(history.position.is_none());
        assert!(history.temp_input.is_none());
    }

    #[test]
    fn test_max_size_enforcement() {
        let mut history = InputHistory::with_capacity(3);

        history.push("first".to_string());
        history.push("second".to_string());
        history.push("third".to_string());
        assert_eq!(history.len(), 3);

        // Adding fourth item should remove first
        history.push("fourth".to_string());
        assert_eq!(history.len(), 3);
        assert_eq!(history.items, vec!["second", "third", "fourth"]);

        // Adding fifth item should remove second
        history.push("fifth".to_string());
        assert_eq!(history.len(), 3);
        assert_eq!(history.items, vec!["third", "fourth", "fifth"]);
    }

    #[test]
    fn test_clear() {
        let mut history = InputHistory::new();
        history.push("first".to_string());
        history.push("second".to_string());
        history.navigate_prev("current");

        history.clear();

        assert!(history.is_empty());
        assert_eq!(history.len(), 0);
        assert!(history.position.is_none());
        assert!(history.temp_input.is_none());
    }

    #[test]
    fn test_up_down_up_down_sequence() {
        let mut history = InputHistory::new();
        history.push("first".to_string());
        history.push("second".to_string());
        history.push("third".to_string());

        // Up, up, down, up sequence
        assert_eq!(history.navigate_prev("current"), Some("third".to_string()));
        assert_eq!(history.navigate_prev("current"), Some("second".to_string()));
        assert_eq!(history.navigate_next(), Some("third".to_string()));
        assert_eq!(history.navigate_prev("current"), Some("second".to_string()));
    }

    #[test]
    fn test_full_navigation_cycle() {
        let mut history = InputHistory::new();
        history.push("alpha".to_string());
        history.push("beta".to_string());
        history.push("gamma".to_string());

        let original = "my search query";

        // Go all the way back
        assert_eq!(history.navigate_prev(original), Some("gamma".to_string()));
        assert_eq!(history.navigate_prev(original), Some("beta".to_string()));
        assert_eq!(history.navigate_prev(original), Some("alpha".to_string()));
        assert_eq!(history.navigate_prev(original), None); // At oldest

        // Go all the way forward
        assert_eq!(history.navigate_next(), Some("beta".to_string()));
        assert_eq!(history.navigate_next(), Some("gamma".to_string()));
        assert_eq!(history.navigate_next(), Some(original.to_string())); // Back to original
        assert_eq!(history.navigate_next(), None); // Not navigating anymore
    }

    #[test]
    #[should_panic(expected = "History max_size must be greater than 0")]
    fn test_zero_capacity_panics() {
        InputHistory::with_capacity(0);
    }

    #[test]
    fn test_single_item_history() {
        let mut history = InputHistory::with_capacity(1);

        history.push("first".to_string());
        history.push("second".to_string());
        history.push("third".to_string());

        // Should only keep the most recent item
        assert_eq!(history.len(), 1);
        assert_eq!(history.last(), Some("third"));
    }
}
