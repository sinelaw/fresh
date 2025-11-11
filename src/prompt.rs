//! Prompt/minibuffer system for user input

use crate::commands::Suggestion;
use crate::word_navigation::{find_word_end_bytes, find_word_start_bytes, is_word_char};

/// Type of prompt - determines what action to take when user confirms
#[derive(Debug, Clone, PartialEq)]
pub enum PromptType {
    /// Open a file
    OpenFile,
    /// Save current buffer to a new file
    SaveFileAs,
    /// Search for text in buffer
    Search,
    /// Search for text in buffer (for replace operation - will prompt for replacement after)
    ReplaceSearch,
    /// Replace text in buffer
    Replace { search: String },
    /// Search for text in buffer (for query-replace - will prompt for replacement after)
    QueryReplaceSearch,
    /// Query replace text in buffer interactively (y/n/!/q)
    QueryReplace { search: String },
    /// Execute a command by name (M-x)
    Command,
    /// Git grep - search through git-tracked files
    GitGrep,
    /// Git find file - find file by filtering git ls-files
    GitFindFile,
    /// Plugin-controlled prompt with custom type identifier
    /// The string identifier is used to filter hooks in plugin code
    Plugin { custom_type: String },
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
    /// Selection anchor position (for Shift+Arrow selection)
    /// When Some(pos), there's a selection from anchor to cursor_pos
    pub selection_anchor: Option<usize>,
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
            selection_anchor: None,
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
            selection_anchor: None,
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

    /// Set the input text and cursor position
    ///
    /// Used for history navigation - replaces the entire input with a new value
    /// and moves cursor to the end.
    ///
    /// # Example
    /// ```
    /// # use fresh::prompt::{Prompt, PromptType};
    /// let mut prompt = Prompt::new("Search: ".to_string(), PromptType::Search);
    /// prompt.input = "current".to_string();
    /// prompt.cursor_pos = 7;
    ///
    /// prompt.set_input("from history".to_string());
    /// assert_eq!(prompt.input, "from history");
    /// assert_eq!(prompt.cursor_pos, 12); // At end
    /// ```
    pub fn set_input(&mut self, text: String) {
        self.cursor_pos = text.len();
        self.input = text;
        self.clear_selection();
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
        // If there's a selection, delete it first
        if self.has_selection() {
            self.delete_selection();
        }
        self.input.insert_str(self.cursor_pos, text);
        self.cursor_pos += text.len();
    }

    // ========================================================================
    // Selection support
    // ========================================================================

    /// Check if there's an active selection
    pub fn has_selection(&self) -> bool {
        self.selection_anchor.is_some() && self.selection_anchor != Some(self.cursor_pos)
    }

    /// Get the selection range (start, end) where start <= end
    pub fn selection_range(&self) -> Option<(usize, usize)> {
        if let Some(anchor) = self.selection_anchor {
            if anchor != self.cursor_pos {
                let start = anchor.min(self.cursor_pos);
                let end = anchor.max(self.cursor_pos);
                return Some((start, end));
            }
        }
        None
    }

    /// Get the selected text
    pub fn selected_text(&self) -> Option<String> {
        self.selection_range()
            .map(|(start, end)| self.input[start..end].to_string())
    }

    /// Delete the current selection and return the deleted text
    pub fn delete_selection(&mut self) -> Option<String> {
        if let Some((start, end)) = self.selection_range() {
            let deleted = self.input[start..end].to_string();
            self.input.drain(start..end);
            self.cursor_pos = start;
            self.selection_anchor = None;
            Some(deleted)
        } else {
            None
        }
    }

    /// Clear selection without deleting text
    pub fn clear_selection(&mut self) {
        self.selection_anchor = None;
    }

    /// Move cursor left with selection
    pub fn move_left_selecting(&mut self) {
        // Set anchor if not already set
        if self.selection_anchor.is_none() {
            self.selection_anchor = Some(self.cursor_pos);
        }

        // Move cursor left
        if self.cursor_pos > 0 {
            let mut new_pos = self.cursor_pos - 1;
            while new_pos > 0 && !self.input.is_char_boundary(new_pos) {
                new_pos -= 1;
            }
            self.cursor_pos = new_pos;
        }
    }

    /// Move cursor right with selection
    pub fn move_right_selecting(&mut self) {
        // Set anchor if not already set
        if self.selection_anchor.is_none() {
            self.selection_anchor = Some(self.cursor_pos);
        }

        // Move cursor right
        if self.cursor_pos < self.input.len() {
            let mut new_pos = self.cursor_pos + 1;
            while new_pos < self.input.len() && !self.input.is_char_boundary(new_pos) {
                new_pos += 1;
            }
            self.cursor_pos = new_pos;
        }
    }

    /// Move to start of input with selection
    pub fn move_home_selecting(&mut self) {
        if self.selection_anchor.is_none() {
            self.selection_anchor = Some(self.cursor_pos);
        }
        self.cursor_pos = 0;
    }

    /// Move to end of input with selection
    pub fn move_end_selecting(&mut self) {
        if self.selection_anchor.is_none() {
            self.selection_anchor = Some(self.cursor_pos);
        }
        self.cursor_pos = self.input.len();
    }

    /// Move to start of previous word with selection
    /// Mimics Buffer's find_word_start_left behavior
    pub fn move_word_left_selecting(&mut self) {
        if self.selection_anchor.is_none() {
            self.selection_anchor = Some(self.cursor_pos);
        }

        let bytes = self.input.as_bytes();
        if self.cursor_pos == 0 {
            return;
        }

        let mut new_pos = self.cursor_pos.saturating_sub(1);

        // Skip non-word characters (spaces) backwards
        while new_pos > 0 && !is_word_char(bytes[new_pos]) {
            new_pos = new_pos.saturating_sub(1);
        }

        // Find start of word
        while new_pos > 0 && is_word_char(bytes[new_pos.saturating_sub(1)]) {
            new_pos = new_pos.saturating_sub(1);
        }

        self.cursor_pos = new_pos;
    }

    /// Move to end of next word with selection
    /// For selection, we want to select whole words, so move to word END, not word START
    pub fn move_word_right_selecting(&mut self) {
        if self.selection_anchor.is_none() {
            self.selection_anchor = Some(self.cursor_pos);
        }

        // Use find_word_end_bytes which moves to the END of words
        let bytes = self.input.as_bytes();
        let mut new_pos = find_word_end_bytes(bytes, self.cursor_pos);

        // If we didn't move (already at word end), move forward to next word end
        if new_pos == self.cursor_pos && new_pos < bytes.len() {
            new_pos = (new_pos + 1).min(bytes.len());
            new_pos = find_word_end_bytes(bytes, new_pos);
        }

        self.cursor_pos = new_pos;
    }

    /// Move to start of previous word (without selection)
    /// Mimics Buffer's find_word_start_left behavior
    pub fn move_word_left(&mut self) {
        self.clear_selection();

        let bytes = self.input.as_bytes();
        if self.cursor_pos == 0 {
            return;
        }

        let mut new_pos = self.cursor_pos.saturating_sub(1);

        // Skip non-word characters (spaces) backwards
        while new_pos > 0 && !is_word_char(bytes[new_pos]) {
            new_pos = new_pos.saturating_sub(1);
        }

        // Find start of word
        while new_pos > 0 && is_word_char(bytes[new_pos.saturating_sub(1)]) {
            new_pos = new_pos.saturating_sub(1);
        }

        self.cursor_pos = new_pos;
    }

    /// Move to start of next word (without selection)
    /// Mimics Buffer's find_word_start_right behavior
    pub fn move_word_right(&mut self) {
        self.clear_selection();

        let bytes = self.input.as_bytes();
        if self.cursor_pos >= bytes.len() {
            return;
        }

        let mut new_pos = self.cursor_pos;

        // Skip current word
        while new_pos < bytes.len() && is_word_char(bytes[new_pos]) {
            new_pos += 1;
        }

        // Skip non-word characters (spaces)
        while new_pos < bytes.len() && !is_word_char(bytes[new_pos]) {
            new_pos += 1;
        }

        self.cursor_pos = new_pos;
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
    fn test_delete_forward_basic() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.input = "hello".to_string();
        prompt.cursor_pos = 1; // After 'h'

        // Simulate delete key (remove 'e')
        prompt.input.drain(prompt.cursor_pos..prompt.cursor_pos + 1);

        assert_eq!(prompt.input, "hllo");
        assert_eq!(prompt.cursor_pos, 1);
    }

    #[test]
    fn test_delete_at_end() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.input = "hello".to_string();
        prompt.cursor_pos = 5; // At end

        // Delete at end should do nothing
        if prompt.cursor_pos < prompt.input.len() {
            prompt.input.drain(prompt.cursor_pos..prompt.cursor_pos + 1);
        }

        assert_eq!(prompt.input, "hello");
        assert_eq!(prompt.cursor_pos, 5);
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

        prompt.delete_word_backward(); // Delete "four"
        assert_eq!(prompt.input, "one two three ");

        prompt.delete_word_backward(); // Delete "three"
        assert_eq!(prompt.input, "one two ");

        prompt.delete_word_backward(); // Delete "two"
        assert_eq!(prompt.input, "one ");
    }

    // Tests for selection functionality
    #[test]
    fn test_selection_with_shift_arrows() {
        let mut prompt = Prompt::new("Command: ".to_string(), PromptType::Command);
        prompt.input = "hello world".to_string();
        prompt.cursor_pos = 5; // After "hello"

        // No selection initially
        assert!(!prompt.has_selection());
        assert_eq!(prompt.selected_text(), None);

        // Move right selecting - should select " "
        prompt.move_right_selecting();
        assert!(prompt.has_selection());
        assert_eq!(prompt.selection_range(), Some((5, 6)));
        assert_eq!(prompt.selected_text(), Some(" ".to_string()));

        // Move right selecting again - should select " w"
        prompt.move_right_selecting();
        assert_eq!(prompt.selection_range(), Some((5, 7)));
        assert_eq!(prompt.selected_text(), Some(" w".to_string()));

        // Move left selecting - should shrink to " "
        prompt.move_left_selecting();
        assert_eq!(prompt.selection_range(), Some((5, 6)));
        assert_eq!(prompt.selected_text(), Some(" ".to_string()));
    }

    #[test]
    fn test_selection_backward() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.input = "abcdef".to_string();
        prompt.cursor_pos = 4; // After "abcd"

        // Select backward
        prompt.move_left_selecting();
        prompt.move_left_selecting();
        assert!(prompt.has_selection());
        assert_eq!(prompt.selection_range(), Some((2, 4)));
        assert_eq!(prompt.selected_text(), Some("cd".to_string()));
    }

    #[test]
    fn test_selection_with_home_end() {
        let mut prompt = Prompt::new("Prompt: ".to_string(), PromptType::Command);
        prompt.input = "select this text".to_string();
        prompt.cursor_pos = 7; // After "select "

        // Select to end
        prompt.move_end_selecting();
        assert_eq!(prompt.selection_range(), Some((7, 16)));
        assert_eq!(prompt.selected_text(), Some("this text".to_string()));

        // Clear and select from current position to home
        prompt.clear_selection();
        prompt.move_home_selecting();
        assert_eq!(prompt.selection_range(), Some((0, 16)));
        assert_eq!(prompt.selected_text(), Some("select this text".to_string()));
    }

    #[test]
    fn test_word_selection() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.input = "one two three".to_string();
        prompt.cursor_pos = 4; // After "one "

        // Select word right
        prompt.move_word_right_selecting();
        assert_eq!(prompt.selection_range(), Some((4, 7)));
        assert_eq!(prompt.selected_text(), Some("two".to_string()));

        // Select another word
        prompt.move_word_right_selecting();
        assert_eq!(prompt.selection_range(), Some((4, 13)));
        assert_eq!(prompt.selected_text(), Some("two three".to_string()));
    }

    #[test]
    fn test_word_selection_backward() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.input = "one two three".to_string();
        prompt.cursor_pos = 13; // At end

        // Select word left - moves to start of "three"
        prompt.move_word_left_selecting();
        assert_eq!(prompt.selection_range(), Some((8, 13)));
        assert_eq!(prompt.selected_text(), Some("three".to_string()));

        // Note: Currently, calling move_word_left_selecting again when already
        // at a word boundary doesn't move further back. This matches the behavior
        // of find_word_start_bytes which finds the start of the current word.
        // For multi-word backward selection, move cursor backward first, then select.
    }

    #[test]
    fn test_delete_selection() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.input = "hello world".to_string();
        prompt.cursor_pos = 5;

        // Select " world"
        prompt.move_end_selecting();
        assert_eq!(prompt.selected_text(), Some(" world".to_string()));

        // Delete selection
        let deleted = prompt.delete_selection();
        assert_eq!(deleted, Some(" world".to_string()));
        assert_eq!(prompt.input, "hello");
        assert_eq!(prompt.cursor_pos, 5);
        assert!(!prompt.has_selection());
    }

    #[test]
    fn test_insert_deletes_selection() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.input = "hello world".to_string();
        prompt.cursor_pos = 0;

        // Select "hello"
        for _ in 0..5 {
            prompt.move_right_selecting();
        }
        assert_eq!(prompt.selected_text(), Some("hello".to_string()));

        // Insert text - should delete selection first
        prompt.insert_str("goodbye");
        assert_eq!(prompt.input, "goodbye world");
        assert_eq!(prompt.cursor_pos, 7);
        assert!(!prompt.has_selection());
    }

    #[test]
    fn test_clear_selection() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.input = "test".to_string();
        prompt.cursor_pos = 0;

        // Create selection
        prompt.move_end_selecting();
        assert!(prompt.has_selection());

        // Clear selection
        prompt.clear_selection();
        assert!(!prompt.has_selection());
        assert_eq!(prompt.cursor_pos, 4); // Cursor should remain at end
        assert_eq!(prompt.input, "test"); // Input unchanged
    }

    #[test]
    fn test_selection_edge_cases() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.input = "abc".to_string();
        prompt.cursor_pos = 3;

        // Select beyond end should stop at end (no movement, no selection)
        prompt.move_right_selecting();
        assert_eq!(prompt.cursor_pos, 3);
        // Since cursor didn't move, anchor equals cursor, so no selection
        assert_eq!(prompt.selection_range(), None);
        assert_eq!(prompt.selected_text(), None);

        // Delete non-existent selection should return None
        assert_eq!(prompt.delete_selection(), None);
        assert_eq!(prompt.input, "abc");
    }

    #[test]
    fn test_selection_with_unicode() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.input = "hello 世界 world".to_string();
        prompt.cursor_pos = 6; // After "hello "

        // Select the Chinese characters
        for _ in 0..2 {
            prompt.move_right_selecting();
        }

        let selected = prompt.selected_text().unwrap();
        assert_eq!(selected, "世界");

        // Delete should work correctly
        prompt.delete_selection();
        assert_eq!(prompt.input, "hello  world");
    }

    // BUG REPRODUCTION TESTS

    /// Test that Ctrl+Shift+Left continues past first word boundary (was bug #2)
    #[test]
    fn test_word_selection_continues_across_words() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.input = "one two three".to_string();
        prompt.cursor_pos = 13; // At end

        // First Ctrl+Shift+Left - selects "three"
        prompt.move_word_left_selecting();
        assert_eq!(prompt.selection_range(), Some((8, 13)));
        assert_eq!(prompt.selected_text(), Some("three".to_string()));

        // Second Ctrl+Shift+Left - should extend to "two three"
        // Now correctly moves back one more word when already at word boundary
        prompt.move_word_left_selecting();

        // Selection should extend to include "two three"
        assert_eq!(prompt.selection_range(), Some((4, 13)));
        assert_eq!(prompt.selected_text(), Some("two three".to_string()));
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
