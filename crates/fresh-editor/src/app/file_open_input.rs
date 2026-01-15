//! Input handling for the file open dialog
//!
//! This module handles keyboard and mouse input specifically for the file
//! browser popup when the Open File or Switch Project prompt is active.

use super::file_open::{FileOpenSection, SortMode};
use super::Editor;
use crate::input::keybindings::Action;
use crate::primitives::path_utils::expand_tilde;
use crate::view::prompt::PromptType;
use rust_i18n::t;

impl Editor {
    /// Check if the file open dialog is active (for OpenFile, SwitchProject, or SaveFileAs)
    pub fn is_file_open_active(&self) -> bool {
        self.prompt
            .as_ref()
            .map(|p| {
                matches!(
                    p.prompt_type,
                    PromptType::OpenFile | PromptType::SwitchProject | PromptType::SaveFileAs
                )
            })
            .unwrap_or(false)
            && self.file_open_state.is_some()
    }

    /// Check if we're in folder-only selection mode (Switch Project)
    fn is_folder_open_mode(&self) -> bool {
        self.prompt
            .as_ref()
            .map(|p| p.prompt_type == PromptType::SwitchProject)
            .unwrap_or(false)
    }

    /// Check if we're in save mode (Save As)
    fn is_save_mode(&self) -> bool {
        self.prompt
            .as_ref()
            .map(|p| p.prompt_type == PromptType::SaveFileAs)
            .unwrap_or(false)
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
            // Let Home/End pass through to normal prompt cursor handling
            // PromptMoveStart and PromptMoveEnd are NOT intercepted here

            // Enter to confirm selection
            Action::PromptConfirm => {
                self.file_open_confirm();
                true
            }

            // Tab to autocomplete to selected item (and navigate into dir if it's a directory)
            Action::PromptAcceptSuggestion => {
                // Get the selected entry info
                let selected_info = self.file_open_state.as_ref().and_then(|s| {
                    s.selected_index
                        .and_then(|idx| s.entries.get(idx))
                        .map(|e| {
                            (
                                e.fs_entry.name.clone(),
                                e.fs_entry.is_dir(),
                                e.fs_entry.path.clone(),
                            )
                        })
                });

                if let Some((name, is_dir, path)) = selected_info {
                    if is_dir {
                        // Navigate into the directory
                        self.file_open_navigate_to(path);
                    } else {
                        // Just autocomplete the filename
                        if let Some(prompt) = &mut self.prompt {
                            prompt.input = name;
                            prompt.cursor_pos = prompt.input.len();
                        }
                        // Update the filter to match
                        self.update_file_open_filter();
                    }
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

            // Toggle hidden files visibility
            Action::FileBrowserToggleHidden => {
                self.file_open_toggle_hidden();
                true
            }

            // Text input is handled by normal prompt, but we need to update filter
            _ => false,
        }
    }

    /// Confirm selection in file open dialog
    fn file_open_confirm(&mut self) {
        let is_folder_mode = self.is_folder_open_mode();
        let is_save_mode = self.is_save_mode();
        let prompt_input = self
            .prompt
            .as_ref()
            .map(|p| p.input.clone())
            .unwrap_or_default();

        // Get the current directory from file open state
        let current_dir = self
            .file_open_state
            .as_ref()
            .map(|s| s.current_dir.clone())
            .unwrap_or_else(|| std::env::current_dir().unwrap_or_default());

        // If there's any prompt input, try to resolve it as a path
        if !prompt_input.is_empty() {
            // Expand tilde and resolve path
            let tilde_expanded = expand_tilde(&prompt_input);
            let expanded_path = if tilde_expanded.is_absolute() {
                tilde_expanded
            } else {
                // Relative path (including plain filename) - resolve against current directory
                current_dir.join(&prompt_input)
            };

            if expanded_path.is_dir() {
                if is_folder_mode {
                    // In folder mode, selecting a directory switches to it as the project root
                    self.file_open_select_folder(expanded_path);
                } else {
                    self.file_open_navigate_to(expanded_path);
                }
                return;
            } else if is_save_mode {
                // In save mode, save to the specified path
                self.file_open_save_file(expanded_path);
                return;
            } else if expanded_path.is_file() && !is_folder_mode {
                // File exists - open it directly (handles pasted paths before async load completes)
                // Only allowed in file mode, not folder mode
                self.file_open_open_file(expanded_path);
                return;
            } else if !is_folder_mode && Self::should_create_new_file(&prompt_input) {
                // File doesn't exist but input looks like a filename - create new file
                // This handles cases like "newfile.txt" or "/path/to/newfile.txt"
                self.file_open_create_new_file(expanded_path);
                return;
            }
            // File doesn't exist and doesn't look like a new filename -
            // fall through to use selected entry from file list
            // This allows partial filters like "bar" to match "bar.txt"
        }

        // Use the selected entry from the file list
        let (path, is_dir) = {
            let state = match &self.file_open_state {
                Some(s) => s,
                None => {
                    // If no file is selected but we're in folder mode, use the current directory
                    if is_folder_mode {
                        self.file_open_select_folder(current_dir);
                    }
                    return;
                }
            };

            let path = match state.get_selected_path() {
                Some(p) => p,
                None => {
                    // In save mode with no input, we can't save
                    if is_save_mode {
                        self.set_status_message(t!("file.save_as_no_filename").to_string());
                        return;
                    }
                    // If no file is selected but we're in folder mode, use the current directory
                    if is_folder_mode {
                        self.file_open_select_folder(current_dir);
                    }
                    return;
                }
            };

            (path, state.selected_is_dir())
        };

        if is_dir {
            if is_folder_mode {
                // In folder mode, selecting a directory switches to it as the project root
                self.file_open_select_folder(path);
            } else {
                // Navigate into directory
                self.file_open_navigate_to(path);
            }
        } else if is_save_mode {
            // In save mode, save to the selected file
            self.file_open_save_file(path);
        } else if !is_folder_mode {
            // Open the file (only in file mode)
            self.file_open_open_file(path);
        }
        // In folder mode, selecting a file does nothing
    }

    /// Select a folder as the new project root (for SwitchProject mode)
    fn file_open_select_folder(&mut self, path: std::path::PathBuf) {
        // Close the file browser
        self.file_open_state = None;
        self.prompt = None;

        // Change the working directory
        self.change_working_dir(path);
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

        // Reset key context to Normal so editor gets focus
        // This is important when the file explorer was focused before opening the file browser
        self.key_context = crate::input::keybindings::KeyContext::Normal;

        // Open the file
        if let Err(e) = self.open_file(&path) {
            self.set_status_message(t!("file.error_opening", error = e.to_string()).to_string());
        } else {
            self.set_status_message(
                t!("file.opened", path = path.display().to_string()).to_string(),
            );
        }
    }

    /// Create a new file (opens an unsaved buffer that will create the file on save)
    fn file_open_create_new_file(&mut self, path: std::path::PathBuf) {
        // Close the file browser
        self.file_open_state = None;
        self.prompt = None;

        // Reset key context to Normal so editor gets focus
        // This is important when the file explorer was focused before opening the file browser
        self.key_context = crate::input::keybindings::KeyContext::Normal;

        // Open the file - this will create an unsaved buffer with the path set
        if let Err(e) = self.open_file(&path) {
            self.set_status_message(t!("file.error_opening", error = e.to_string()).to_string());
        } else {
            self.set_status_message(
                t!("file.created_new", path = path.display().to_string()).to_string(),
            );
        }
    }

    /// Save the current buffer to a file (for SaveFileAs mode)
    fn file_open_save_file(&mut self, path: std::path::PathBuf) {
        use crate::view::prompt::PromptType as PT;

        // Close the file browser
        self.file_open_state = None;
        self.prompt = None;

        // Check if file exists and is different from current file
        let current_file_path = self
            .active_state()
            .buffer
            .file_path()
            .map(|p| p.to_path_buf());
        let is_different_file = current_file_path.as_ref() != Some(&path);

        if is_different_file && path.is_file() {
            // File exists and is different from current - ask for confirmation
            let filename = path
                .file_name()
                .map(|n| n.to_string_lossy().to_string())
                .unwrap_or_else(|| path.display().to_string());
            self.start_prompt(
                t!("buffer.overwrite_confirm", name = &filename).to_string(),
                PT::ConfirmOverwriteFile { path },
            );
            return;
        }

        // Proceed with save
        self.perform_save_file_as(path);
    }

    /// Check if the input looks like a filename that should be created
    /// (has an extension or contains a path separator)
    fn should_create_new_file(input: &str) -> bool {
        // If input contains a dot with something after it (extension), or
        // contains a path separator, treat it as a file to be created
        let has_extension = input.rfind('.').is_some_and(|pos| {
            // Check there's something after the dot that's not a path separator
            let after_dot = &input[pos + 1..];
            !after_dot.is_empty() && !after_dot.contains('/') && !after_dot.contains('\\')
        });

        let has_path_separator = input.contains('/') || input.contains('\\');

        has_extension || has_path_separator
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

        // Check if user typed/pasted a path containing directory separators
        // Navigate to the parent directory of the path (so the file appears in the list)
        if filter.contains('/') {
            let current_dir = self
                .file_open_state
                .as_ref()
                .map(|s| s.current_dir.clone())
                .unwrap_or_else(|| std::env::current_dir().unwrap_or_default());

            // Build the full path
            // Expand tilde and resolve path
            let tilde_expanded = expand_tilde(&filter);
            let full_path = if tilde_expanded.is_absolute() {
                tilde_expanded
            } else {
                current_dir.join(&filter)
            };

            // Get the parent directory and filename
            let (target_dir, filename) = if filter.ends_with('/') {
                // Path ends with /, treat the whole thing as a directory
                (full_path.clone(), String::new())
            } else {
                // Get parent directory so the file will be in the listing
                let parent = full_path
                    .parent()
                    .map(|p| p.to_path_buf())
                    .unwrap_or(full_path.clone());
                let name = full_path
                    .file_name()
                    .map(|n| n.to_string_lossy().to_string())
                    .unwrap_or_default();
                (parent, name)
            };

            // Navigate to target directory if it exists and is different from current
            if target_dir.is_dir() && target_dir != current_dir {
                // Update prompt to only show the filename (directory is shown separately)
                if let Some(prompt) = &mut self.prompt {
                    prompt.input = filename.clone();
                    prompt.cursor_pos = prompt.input.len();
                }
                self.load_file_open_directory(target_dir);

                // Apply filter with the filename only
                if let Some(state) = &mut self.file_open_state {
                    state.apply_filter(&filename);
                }
                return;
            }
        }

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
            let new_state = state.show_hidden;

            // Reload directory to apply change
            let current_dir = state.current_dir.clone();
            self.load_file_open_directory(current_dir);

            // Show status message
            let msg = if new_state {
                "Showing hidden files"
            } else {
                "Hiding hidden files"
            };
            self.set_status_message(msg.to_string());
        }
    }

    /// Handle mouse wheel scroll in file browser
    /// Returns true if the scroll was handled
    pub fn handle_file_open_scroll(&mut self, delta: i32) -> bool {
        if !self.is_file_open_active() {
            return false;
        }

        let visible_rows = self
            .file_browser_layout
            .as_ref()
            .map(|l| l.visible_rows)
            .unwrap_or(10);

        if let Some(state) = &mut self.file_open_state {
            let total_entries = state.entries.len();
            if total_entries <= visible_rows {
                // No scrolling needed if all entries fit
                return true;
            }

            let max_scroll = total_entries.saturating_sub(visible_rows);

            if delta < 0 {
                // Scroll up
                let scroll_amount = (-delta) as usize;
                state.scroll_offset = state.scroll_offset.saturating_sub(scroll_amount);
            } else {
                // Scroll down
                let scroll_amount = delta as usize;
                state.scroll_offset = (state.scroll_offset + scroll_amount).min(max_scroll);
            }
            return true;
        }

        false
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
                // Get the entry name before mutating state
                let entry_name = self
                    .file_open_state
                    .as_ref()
                    .and_then(|s| s.entries.get(index))
                    .map(|e| e.fs_entry.name.clone());

                if let Some(state) = &mut self.file_open_state {
                    state.active_section = FileOpenSection::Files;
                    if index < state.entries.len() {
                        state.selected_index = Some(index);
                    }
                }

                // Update prompt text to show the selected entry name
                if let Some(name) = entry_name {
                    if let Some(prompt) = &mut self.prompt {
                        prompt.input = name;
                        prompt.cursor_pos = prompt.input.len();
                    }
                }
            }
            return true;
        }

        // Check if click is on "Show Hidden" checkbox (in navigation area, right side)
        if layout.is_on_show_hidden_checkbox(x, y) {
            self.file_open_toggle_hidden();
            return true;
        }

        // Check if click is in navigation area
        if layout.is_in_nav(x, y) {
            // Get shortcut labels for hit testing
            let shortcut_labels: Vec<&str> = self
                .file_open_state
                .as_ref()
                .map(|s| s.shortcuts.iter().map(|sc| sc.label.as_str()).collect())
                .unwrap_or_default();

            if let Some(shortcut_idx) = layout.nav_shortcut_at(x, y, &shortcut_labels) {
                // Get the path from the shortcut and navigate there
                let target_path = self
                    .file_open_state
                    .as_ref()
                    .and_then(|s| s.shortcuts.get(shortcut_idx))
                    .map(|sc| sc.path.clone());

                if let Some(path) = target_path {
                    if let Some(state) = &mut self.file_open_state {
                        state.active_section = FileOpenSection::Navigation;
                        state.selected_shortcut = shortcut_idx;
                    }
                    self.file_open_navigate_to(path);
                }
            } else {
                // Clicked in nav area but not on a shortcut
                if let Some(state) = &mut self.file_open_state {
                    state.active_section = FileOpenSection::Navigation;
                }
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
            // Calculate scroll offset based on click position
            let rel_y = y.saturating_sub(layout.scrollbar_area.y) as usize;
            let track_height = layout.scrollbar_area.height as usize;

            if let Some(state) = &mut self.file_open_state {
                let total_items = state.entries.len();
                let visible_items = layout.visible_rows;

                if total_items > visible_items && track_height > 0 {
                    let max_scroll = total_items.saturating_sub(visible_items);
                    let click_ratio = rel_y as f64 / track_height as f64;
                    let new_offset = (click_ratio * max_scroll as f64) as usize;
                    state.scroll_offset = new_offset.min(max_scroll);
                }
            }
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

    /// Compute hover target for file browser
    pub fn compute_file_browser_hover(&self, x: u16, y: u16) -> Option<super::types::HoverTarget> {
        use super::types::HoverTarget;

        let layout = self.file_browser_layout.as_ref()?;

        // Check "Show Hidden" checkbox first (priority over navigation shortcuts)
        if layout.is_on_show_hidden_checkbox(x, y) {
            return Some(HoverTarget::FileBrowserShowHiddenCheckbox);
        }

        // Check navigation shortcuts
        if layout.is_in_nav(x, y) {
            let shortcut_labels: Vec<&str> = self
                .file_open_state
                .as_ref()
                .map(|s| s.shortcuts.iter().map(|sc| sc.label.as_str()).collect())
                .unwrap_or_default();

            if let Some(idx) = layout.nav_shortcut_at(x, y, &shortcut_labels) {
                return Some(HoverTarget::FileBrowserNavShortcut(idx));
            }
        }

        // Check column headers
        if layout.is_in_header(x, y) {
            if let Some(mode) = layout.header_column_at(x) {
                return Some(HoverTarget::FileBrowserHeader(mode));
            }
        }

        // Check file list entries
        if layout.is_in_list(x, y) {
            let scroll_offset = self
                .file_open_state
                .as_ref()
                .map(|s| s.scroll_offset)
                .unwrap_or(0);

            if let Some(idx) = layout.click_to_index(y, scroll_offset) {
                let total_entries = self
                    .file_open_state
                    .as_ref()
                    .map(|s| s.entries.len())
                    .unwrap_or(0);

                if idx < total_entries {
                    return Some(HoverTarget::FileBrowserEntry(idx));
                }
            }
        }

        // Check scrollbar
        if layout.is_in_scrollbar(x, y) {
            return Some(HoverTarget::FileBrowserScrollbar);
        }

        None
    }
}
