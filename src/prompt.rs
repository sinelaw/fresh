//! Prompt/minibuffer system for user input

use crate::commands::Suggestion;
use crate::word_navigation::{find_word_end_bytes, find_word_start_bytes};

/// Type of prompt - determines what action to take when user confirms
#[derive(Debug, Clone, PartialEq)]
pub enum PromptType {
    /// Open a file
    OpenFile,
    /// Save current buffer to a new file
    SaveFileAs,
    /// Search for text in buffer
    Search,
    /// Replace text in buffer
    Replace { search: String },
    /// Execute a command by name (M-x)
    Command,
    /// Git grep - search through git-tracked files
    GitGrep,
    /// Git find file - find file by filtering git ls-files
    GitFindFile,
}

/// Prompt state for the minibuffer
#[derive(Debug, Clone)]
pub struct Prompt {
    /// The prompt message (e.g., "Find file: ")
    pub message: String,
    /// User's current input
    pub input: String,
    /// Cursor position in the input
    pub cursor_pos: usize,
    /// What to do when user confirms
    pub prompt_type: PromptType,
    /// Autocomplete suggestions
    pub suggestions: Vec<Suggestion>,
    /// Currently selected suggestion index
    pub selected_suggestion: Option<usize>,
}

impl Prompt {
    /// Create a new prompt
    pub fn new(message: String, prompt_type: PromptType) -> Self {
        Self {
            message,
            input: String::new(),
            cursor_pos: 0,
            prompt_type,
            suggestions: Vec::new(),
            selected_suggestion: None,
        }
    }

    /// Create a new prompt with suggestions
    pub fn with_suggestions(
        message: String,
        prompt_type: PromptType,
        suggestions: Vec<Suggestion>,
    ) -> Self {
        let selected_suggestion = if suggestions.is_empty() {
            None
        } else {
            Some(0)
        };
        Self {
            message,
            input: String::new(),
            cursor_pos: 0,
            prompt_type,
            suggestions,
            selected_suggestion,
        }
    }

    /// Move cursor left
    pub fn cursor_left(&mut self) {
        if self.cursor_pos > 0 {
            self.cursor_pos -= 1;
        }
    }

    /// Move cursor right
    pub fn cursor_right(&mut self) {
        if self.cursor_pos < self.input.len() {
            self.cursor_pos += 1;
        }
    }

    /// Insert a character at the cursor position
    pub fn insert_char(&mut self, ch: char) {
        self.input.insert(self.cursor_pos, ch);
        self.cursor_pos += ch.len_utf8();
    }

    /// Delete character before cursor (backspace)
    pub fn backspace(&mut self) {
        if self.cursor_pos > 0 {
            self.input.remove(self.cursor_pos - 1);
            self.cursor_pos -= 1;
        }
    }

    /// Delete character at cursor (delete key)
    pub fn delete(&mut self) {
        if self.cursor_pos < self.input.len() {
            self.input.remove(self.cursor_pos);
        }
    }

    /// Move to start of input
    pub fn move_to_start(&mut self) {
        self.cursor_pos = 0;
    }

    /// Move to end of input
    pub fn move_to_end(&mut self) {
        self.cursor_pos = self.input.len();
    }

    /// Select next suggestion
    pub fn select_next_suggestion(&mut self) {
        if !self.suggestions.is_empty() {
            self.selected_suggestion = Some(match self.selected_suggestion {
                Some(idx) if idx + 1 < self.suggestions.len() => idx + 1,
                Some(_) => 0, // Wrap to start
                None => 0,
            });
        }
    }

    /// Select previous suggestion
    pub fn select_prev_suggestion(&mut self) {
        if !self.suggestions.is_empty() {
            self.selected_suggestion = Some(match self.selected_suggestion {
                Some(0) => self.suggestions.len() - 1, // Wrap to end
                Some(idx) => idx - 1,
                None => 0,
            });
        }
    }

    /// Get the currently selected suggestion value
    pub fn selected_value(&self) -> Option<String> {
        self.selected_suggestion
            .and_then(|idx| self.suggestions.get(idx))
            .map(|s| s.get_value().to_string())
    }

    /// Get the final input (use selected suggestion if available, otherwise raw input)
    pub fn get_final_input(&self) -> String {
        self.selected_value().unwrap_or_else(|| self.input.clone())
    }

    // ========================================================================
    // Advanced editing operations (word-based, clipboard)
    // ========================================================================
    //
    // MOTIVATION:
    // These methods provide advanced editing capabilities in prompts that
    // users expect from normal text editing:
    // - Word-based deletion (Ctrl+Backspace/Delete)
    // - Copy/paste/cut operations
    //
    // This enables consistent editing experience across both buffer editing
    // and prompt input (command palette, file picker, search, etc.).

    /// Delete from cursor to end of word (Ctrl+Delete).
    ///
    /// Deletes from the current cursor position to the end of the current word.
    /// If the cursor is at a non-word character, skips to the next word and
    /// deletes to its end.
    ///
    /// # Example
    /// ```
    /// # use fresh::prompt::{Prompt, PromptType};
    /// let mut prompt = Prompt::new("Find: ".to_string(), PromptType::OpenFile);
    /// prompt.input = "hello world".to_string();
    /// prompt.cursor_pos = 0; // At start of "hello"
    /// prompt.delete_word_forward();
    /// assert_eq!(prompt.input, " world");
    /// assert_eq!(prompt.cursor_pos, 0);
    /// ```
    pub fn delete_word_forward(&mut self) {
        let word_end = find_word_end_bytes(self.input.as_bytes(), self.cursor_pos);
        if word_end > self.cursor_pos {
            self.input.drain(self.cursor_pos..word_end);
            // Cursor stays at same position
        }
    }

    /// Delete from start of word to cursor (Ctrl+Backspace).
    ///
    /// Deletes from the start of the current word to the cursor position.
    /// If the cursor is after a non-word character, deletes the previous word.
    ///
    /// # Example
    /// ```
    /// # use fresh::prompt::{Prompt, PromptType};
    /// let mut prompt = Prompt::new("Find: ".to_string(), PromptType::OpenFile);
    /// prompt.input = "hello world".to_string();
    /// prompt.cursor_pos = 5; // After "hello"
    /// prompt.delete_word_backward();
    /// assert_eq!(prompt.input, " world");
    /// assert_eq!(prompt.cursor_pos, 0);
    /// ```
    pub fn delete_word_backward(&mut self) {
        let word_start = find_word_start_bytes(self.input.as_bytes(), self.cursor_pos);
        if word_start < self.cursor_pos {
            self.input.drain(word_start..self.cursor_pos);
            self.cursor_pos = word_start;
        }
    }

    /// Get the current input text (for copy operation).
    ///
    /// Returns a copy of the entire input. In future, this could be extended
    /// to support selection ranges for copying only selected text.
    ///
    /// # Example
    /// ```
    /// # use fresh::prompt::{Prompt, PromptType};
    /// let mut prompt = Prompt::new("Search: ".to_string(), PromptType::Search);
    /// prompt.input = "test query".to_string();
    /// assert_eq!(prompt.get_text(), "test query");
    /// ```
    pub fn get_text(&self) -> String {
        self.input.clone()
    }

    /// Clear the input (used for cut operation).
    ///
    /// Removes all text from the input and resets cursor to start.
    ///
    /// # Example
    /// ```
    /// # use fresh::prompt::{Prompt, PromptType};
    /// let mut prompt = Prompt::new("Find: ".to_string(), PromptType::OpenFile);
    /// prompt.input = "some text".to_string();
    /// prompt.cursor_pos = 9;
    /// prompt.clear();
    /// assert_eq!(prompt.input, "");
    /// assert_eq!(prompt.cursor_pos, 0);
    /// ```
    pub fn clear(&mut self) {
        self.input.clear();
        self.cursor_pos = 0;
        // Also clear selection when clearing input
        self.selected_suggestion = None;
    }

    /// Insert text at cursor position (used for paste operation).
    ///
    /// Inserts the given text at the current cursor position and moves
    /// the cursor to the end of the inserted text.
    ///
    /// # Example
    /// ```
    /// # use fresh::prompt::{Prompt, PromptType};
    /// let mut prompt = Prompt::new("Command: ".to_string(), PromptType::Command);
    /// prompt.input = "save".to_string();
    /// prompt.cursor_pos = 4;
    /// prompt.insert_str(" file");
    /// assert_eq!(prompt.input, "save file");
    /// assert_eq!(prompt.cursor_pos, 9);
    /// ```
    pub fn insert_str(&mut self, text: &str) {
        self.input.insert_str(self.cursor_pos, text);
        self.cursor_pos += text.len();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_delete_word_forward_basic() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.input = "hello world test".to_string();
        prompt.cursor_pos = 0;

        prompt.delete_word_forward();
        assert_eq!(prompt.input, " world test");
        assert_eq!(prompt.cursor_pos, 0);
    }

    #[test]
    fn test_delete_word_forward_middle() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.input = "hello world test".to_string();
        prompt.cursor_pos = 3; // Middle of "hello"

        prompt.delete_word_forward();
        assert_eq!(prompt.input, "hel world test");
        assert_eq!(prompt.cursor_pos, 3);
    }

    #[test]
    fn test_delete_word_forward_at_space() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.input = "hello world".to_string();
        prompt.cursor_pos = 5; // At space after "hello"

        prompt.delete_word_forward();
        assert_eq!(prompt.input, "hello");
        assert_eq!(prompt.cursor_pos, 5);
    }

    #[test]
    fn test_delete_word_backward_basic() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.input = "hello world test".to_string();
        prompt.cursor_pos = 5; // After "hello"

        prompt.delete_word_backward();
        assert_eq!(prompt.input, " world test");
        assert_eq!(prompt.cursor_pos, 0);
    }

    #[test]
    fn test_delete_word_backward_middle() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.input = "hello world test".to_string();
        prompt.cursor_pos = 8; // Middle of "world"

        prompt.delete_word_backward();
        assert_eq!(prompt.input, "hello rld test");
        assert_eq!(prompt.cursor_pos, 6);
    }

    #[test]
    fn test_delete_word_backward_at_end() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.input = "hello world".to_string();
        prompt.cursor_pos = 11; // At end

        prompt.delete_word_backward();
        assert_eq!(prompt.input, "hello ");
        assert_eq!(prompt.cursor_pos, 6);
    }

    #[test]
    fn test_delete_word_with_special_chars() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.input = "save-file-as".to_string();
        prompt.cursor_pos = 12; // At end

        // Delete "as"
        prompt.delete_word_backward();
        assert_eq!(prompt.input, "save-file-");
        assert_eq!(prompt.cursor_pos, 10);

        // Delete "file"
        prompt.delete_word_backward();
        assert_eq!(prompt.input, "save-");
        assert_eq!(prompt.cursor_pos, 5);
    }

    #[test]
    fn test_get_text() {
        let mut prompt = Prompt::new("Find: ".to_string(), PromptType::OpenFile);
        prompt.input = "test content".to_string();

        assert_eq!(prompt.get_text(), "test content");
    }

    #[test]
    fn test_clear() {
        let mut prompt = Prompt::new("Find: ".to_string(), PromptType::OpenFile);
        prompt.input = "some text".to_string();
        prompt.cursor_pos = 5;
        prompt.selected_suggestion = Some(0);

        prompt.clear();

        assert_eq!(prompt.input, "");
        assert_eq!(prompt.cursor_pos, 0);
        assert_eq!(prompt.selected_suggestion, None);
    }

    #[test]
    fn test_insert_str_at_start() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.input = "world".to_string();
        prompt.cursor_pos = 0;

        prompt.insert_str("hello ");
        assert_eq!(prompt.input, "hello world");
        assert_eq!(prompt.cursor_pos, 6);
    }

    #[test]
    fn test_insert_str_at_middle() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.input = "helloworld".to_string();
        prompt.cursor_pos = 5;

        prompt.insert_str(" ");
        assert_eq!(prompt.input, "hello world");
        assert_eq!(prompt.cursor_pos, 6);
    }

    #[test]
    fn test_insert_str_at_end() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.input = "hello".to_string();
        prompt.cursor_pos = 5;

        prompt.insert_str(" world");
        assert_eq!(prompt.input, "hello world");
        assert_eq!(prompt.cursor_pos, 11);
    }

    #[test]
    fn test_delete_word_forward_empty() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.input = "".to_string();
        prompt.cursor_pos = 0;

        prompt.delete_word_forward();
        assert_eq!(prompt.input, "");
        assert_eq!(prompt.cursor_pos, 0);
    }

    #[test]
    fn test_delete_word_backward_empty() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.input = "".to_string();
        prompt.cursor_pos = 0;

        prompt.delete_word_backward();
        assert_eq!(prompt.input, "");
        assert_eq!(prompt.cursor_pos, 0);
    }

    #[test]
    fn test_delete_word_forward_only_spaces() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.input = "   ".to_string();
        prompt.cursor_pos = 0;

        prompt.delete_word_forward();
        assert_eq!(prompt.input, "");
        assert_eq!(prompt.cursor_pos, 0);
    }

    #[test]
    fn test_multiple_word_deletions() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.input = "one two three four".to_string();
        prompt.cursor_pos = 18;

        prompt.delete_word_backward();  // Delete "four"
        assert_eq!(prompt.input, "one two three ");

        prompt.delete_word_backward();  // Delete "three"
        assert_eq!(prompt.input, "one two ");

        prompt.delete_word_backward();  // Delete "two"
        assert_eq!(prompt.input, "one ");
    }

    // Property-based tests for Prompt operations
    #[cfg(test)]
    mod property_tests {
        use super::*;
        use proptest::prelude::*;

        proptest! {
            /// Property: delete_word_backward should never increase input length
            #[test]
            fn prop_delete_word_backward_shrinks(
                input in "[a-zA-Z0-9_ ]{0,50}",
                cursor_pos in 0usize..50
            ) {
                let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
                prompt.input = input.clone();
                prompt.cursor_pos = cursor_pos.min(input.len());

                let original_len = prompt.input.len();
                prompt.delete_word_backward();

                prop_assert!(prompt.input.len() <= original_len);
            }

            /// Property: delete_word_forward should never increase input length
            #[test]
            fn prop_delete_word_forward_shrinks(
                input in "[a-zA-Z0-9_ ]{0,50}",
                cursor_pos in 0usize..50
            ) {
                let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
                prompt.input = input.clone();
                prompt.cursor_pos = cursor_pos.min(input.len());

                let original_len = prompt.input.len();
                prompt.delete_word_forward();

                prop_assert!(prompt.input.len() <= original_len);
            }

            /// Property: delete_word_backward should not move cursor past input start
            #[test]
            fn prop_delete_word_backward_cursor_valid(
                input in "[a-zA-Z0-9_ ]{0,50}",
                cursor_pos in 0usize..50
            ) {
                let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
                prompt.input = input.clone();
                prompt.cursor_pos = cursor_pos.min(input.len());

                prompt.delete_word_backward();

                prop_assert!(prompt.cursor_pos <= prompt.input.len());
            }

            /// Property: delete_word_forward should keep cursor in valid range
            #[test]
            fn prop_delete_word_forward_cursor_valid(
                input in "[a-zA-Z0-9_ ]{0,50}",
                cursor_pos in 0usize..50
            ) {
                let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
                prompt.input = input.clone();
                prompt.cursor_pos = cursor_pos.min(input.len());

                prompt.delete_word_forward();

                prop_assert!(prompt.cursor_pos <= prompt.input.len());
            }

            /// Property: insert_str should increase length by inserted text length
            #[test]
            fn prop_insert_str_length(
                input in "[a-zA-Z0-9_ ]{0,30}",
                insert in "[a-zA-Z0-9_ ]{0,20}",
                cursor_pos in 0usize..30
            ) {
                let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
                prompt.input = input.clone();
                prompt.cursor_pos = cursor_pos.min(input.len());

                let original_len = prompt.input.len();
                prompt.insert_str(&insert);

                prop_assert_eq!(prompt.input.len(), original_len + insert.len());
            }

            /// Property: insert_str should move cursor by inserted text length
            #[test]
            fn prop_insert_str_cursor(
                input in "[a-zA-Z0-9_ ]{0,30}",
                insert in "[a-zA-Z0-9_ ]{0,20}",
                cursor_pos in 0usize..30
            ) {
                let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
                prompt.input = input.clone();
                let original_pos = cursor_pos.min(input.len());
                prompt.cursor_pos = original_pos;

                prompt.insert_str(&insert);

                prop_assert_eq!(prompt.cursor_pos, original_pos + insert.len());
            }

            /// Property: clear should always result in empty string and zero cursor
            #[test]
            fn prop_clear_resets(input in "[a-zA-Z0-9_ ]{0,50}") {
                let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
                prompt.input = input;
                prompt.cursor_pos = prompt.input.len();

                prompt.clear();

                prop_assert_eq!(prompt.input, "");
                prop_assert_eq!(prompt.cursor_pos, 0);
            }
        }
    }
}
