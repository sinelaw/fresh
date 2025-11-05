//! Prompt/minibuffer system for user input

use crate::commands::Suggestion;

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
}
