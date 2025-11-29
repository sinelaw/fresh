//! Input handling for the file open dialog
//!
//! This module handles keyboard and mouse input specifically for the file
//! browser popup when the Open File prompt is active.

use super::file_open::{FileOpenSection, SortMode};
use super::Editor;
use crate::input::keybindings::Action;
use crate::view::prompt::PromptType;

impl Editor {
    /// Check if the file open dialog is active
    pub fn is_file_open_active(&self) -> bool {
        self.prompt
            .as_ref()
            .map(|p| p.prompt_type == PromptType::OpenFile)
            .unwrap_or(false)
            && self.file_open_state.is_some()
    }

    /// Handle action for file open dialog
    /// Returns true if the action was handled, false if it should be passed to normal prompt handling
    pub fn handle_file_open_action(&mut self, action: &Action) -> bool {
        if !self.is_file_open_active() {
            return false;
        }

        match action {
            // Navigation actions - Up/Down in file list
            Action::PromptSelectPrev => {
                if let Some(state) = &mut self.file_open_state {
                    state.select_prev();
                }
                true
            }
            Action::PromptSelectNext => {
                if let Some(state) = &mut self.file_open_state {
                    state.select_next();
                }
                true
            }
            Action::PromptPageUp => {
                if let Some(state) = &mut self.file_open_state {
                    state.page_up(10);
                }
                true
            }
            Action::PromptPageDown => {
                if let Some(state) = &mut self.file_open_state {
                    state.page_down(10);
                }
                true
            }
            Action::PromptMoveStart => {
                // Go to first entry (Ctrl+Home or similar)
                if let Some(state) = &mut self.file_open_state {
                    state.select_first();
                }
                true
            }
            Action::PromptMoveEnd => {
                // Go to last entry (Ctrl+End or similar)
                if let Some(state) = &mut self.file_open_state {
                    state.select_last();
                }
                true
            }

            // Enter to confirm selection
            Action::PromptConfirm => {
                self.file_open_confirm();
                true
            }

            // Tab to switch sections (navigation vs file list)
            Action::PromptAcceptSuggestion => {
                if let Some(state) = &mut self.file_open_state {
                    state.switch_section();
                }
                true
            }

            // Backspace when filter is empty goes to parent
            Action::PromptBackspace => {
                let filter_empty = self
                    .file_open_state
                    .as_ref()
                    .map(|s| s.filter.is_empty())
                    .unwrap_or(true);
                let prompt_empty = self
                    .prompt
                    .as_ref()
                    .map(|p| p.input.is_empty())
                    .unwrap_or(true);

                if filter_empty && prompt_empty {
                    self.file_open_go_parent();
                    true
                } else {
                    // Let normal prompt handling delete the character
                    false
                }
            }

            // Escape cancels
            Action::PromptCancel => {
                self.cancel_prompt();
                self.file_open_state = None;
                true
            }

            // Text input is handled by normal prompt, but we need to update filter
            _ => false,
        }
    }

    /// Confirm selection in file open dialog
    fn file_open_confirm(&mut self) {
        let (path, is_dir) = {
            let state = match &self.file_open_state {
                Some(s) => s,
                None => return,
            };

            let path = match state.get_selected_path() {
                Some(p) => p,
                None => return,
            };

            (path, state.selected_is_dir())
        };

        if is_dir {
            // Navigate into directory
            self.file_open_navigate_to(path);
        } else {
            // Open the file
            self.file_open_open_file(path);
        }
    }

    /// Navigate to a directory in the file browser
    fn file_open_navigate_to(&mut self, path: std::path::PathBuf) {
        // Clear prompt input
        if let Some(prompt) = self.prompt.as_mut() {
            prompt.input.clear();
            prompt.cursor_pos = 0;
        }

        // Load the new directory
        self.load_file_open_directory(path);
    }

    /// Open a file from the file browser
    fn file_open_open_file(&mut self, path: std::path::PathBuf) {
        // Close the file browser
        self.file_open_state = None;
        self.prompt = None;

        // Open the file
        if let Err(e) = self.open_file(&path) {
            self.set_status_message(format!("Error opening file: {}", e));
        } else {
            self.set_status_message(format!("Opened {}", path.display()));
        }
    }

    /// Navigate to parent directory
    fn file_open_go_parent(&mut self) {
        let parent = self
            .file_open_state
            .as_ref()
            .and_then(|s| s.current_dir.parent())
            .map(|p| p.to_path_buf());

        if let Some(parent_path) = parent {
            self.file_open_navigate_to(parent_path);
        }
    }

    /// Update filter when prompt text changes
    pub fn update_file_open_filter(&mut self) {
        if !self.is_file_open_active() {
            return;
        }

        let filter = self
            .prompt
            .as_ref()
            .map(|p| p.input.clone())
            .unwrap_or_default();

        if let Some(state) = &mut self.file_open_state {
            state.apply_filter(&filter);
        }
    }

    /// Handle sorting toggle (called from keybinding)
    pub fn file_open_toggle_sort(&mut self, mode: SortMode) {
        if let Some(state) = &mut self.file_open_state {
            state.set_sort_mode(mode);
        }
    }

    /// Handle hidden files toggle
    pub fn file_open_toggle_hidden(&mut self) {
        if let Some(state) = &mut self.file_open_state {
            let show_hidden = state.show_hidden;
            state.show_hidden = !show_hidden;

            // Reload directory to apply change
            let current_dir = state.current_dir.clone();
            self.load_file_open_directory(current_dir);
        }
    }

    /// Handle mouse click in file browser
    pub fn handle_file_open_click(&mut self, x: u16, y: u16) -> bool {
        if !self.is_file_open_active() {
            return false;
        }

        let layout = match &self.file_browser_layout {
            Some(l) => l.clone(),
            None => return false,
        };

        // Check if click is in the file list
        if layout.is_in_list(x, y) {
            let scroll_offset = self
                .file_open_state
                .as_ref()
                .map(|s| s.scroll_offset)
                .unwrap_or(0);

            if let Some(index) = layout.click_to_index(y, scroll_offset) {
                if let Some(state) = &mut self.file_open_state {
                    state.active_section = FileOpenSection::Files;
                    if index < state.entries.len() {
                        state.selected_index = index;
                    }
                }
            }
            return true;
        }

        // Check if click is in navigation area
        if layout.is_in_nav(x, y) {
            if let Some(state) = &mut self.file_open_state {
                state.active_section = FileOpenSection::Navigation;
                // TODO: Calculate which shortcut was clicked based on x position
            }
            return true;
        }

        // Check if click is in header (sorting)
        if layout.is_in_header(x, y) {
            if let Some(mode) = layout.header_column_at(x) {
                self.file_open_toggle_sort(mode);
            }
            return true;
        }

        // Check if click is in scrollbar
        if layout.is_in_scrollbar(x, y) {
            // TODO: Handle scrollbar click/drag
            return true;
        }

        false
    }

    /// Handle double-click in file browser
    pub fn handle_file_open_double_click(&mut self, x: u16, y: u16) -> bool {
        if !self.is_file_open_active() {
            return false;
        }

        let layout = match &self.file_browser_layout {
            Some(l) => l.clone(),
            None => return false,
        };

        // Double-click in file list opens/navigates
        if layout.is_in_list(x, y) {
            self.file_open_confirm();
            return true;
        }

        false
    }
}
