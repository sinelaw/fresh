//! File open dialog state and logic
//!
//! This module provides a plugin-free file browser for the Open File command.
//! It renders a structured popup above the prompt with sortable columns,
//! navigation shortcuts, and filtering.

use crate::services::fs::FsEntry;
use std::cmp::Ordering;
use std::path::PathBuf;
use std::time::SystemTime;

/// A file entry in the browser with filter match state
#[derive(Debug, Clone)]
pub struct FileOpenEntry {
    /// The filesystem entry
    pub fs_entry: FsEntry,
    /// Whether this entry matches the current filter
    pub matches_filter: bool,
}

/// Sort mode for file list
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum SortMode {
    #[default]
    Name,
    Size,
    Modified,
    Type,
}

/// Which section of the file browser is active
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum FileOpenSection {
    /// Navigation shortcuts (parent, root, home)
    Navigation,
    /// Main file list
    #[default]
    Files,
}

/// Navigation shortcut entry
#[derive(Debug, Clone)]
pub struct NavigationShortcut {
    /// Display label (e.g., "~", "..", "/")
    pub label: String,
    /// Full path to navigate to
    pub path: PathBuf,
    /// Description (e.g., "Home directory")
    pub description: String,
}

/// State for the file open dialog
#[derive(Debug, Clone)]
pub struct FileOpenState {
    /// Current directory being browsed
    pub current_dir: PathBuf,

    /// Directory entries with metadata
    pub entries: Vec<FileOpenEntry>,

    /// Whether directory is currently loading
    pub loading: bool,

    /// Error message if directory load failed
    pub error: Option<String>,

    /// Current sort mode
    pub sort_mode: SortMode,

    /// Sort direction (true = ascending)
    pub sort_ascending: bool,

    /// Selected index in the current section
    pub selected_index: usize,

    /// Scroll offset for file list
    pub scroll_offset: usize,

    /// Which section is currently active
    pub active_section: FileOpenSection,

    /// Filter text (from prompt input)
    pub filter: String,

    /// Navigation shortcuts
    pub shortcuts: Vec<NavigationShortcut>,

    /// Selected shortcut index (when in Navigation section)
    pub selected_shortcut: usize,

    /// Whether to show hidden files
    pub show_hidden: bool,
}

impl FileOpenState {
    /// Create a new file open state for the given directory
    pub fn new(dir: PathBuf) -> Self {
        let shortcuts = Self::build_shortcuts(&dir);
        Self {
            current_dir: dir,
            entries: Vec::new(),
            loading: true,
            error: None,
            sort_mode: SortMode::Name,
            sort_ascending: true,
            selected_index: 0,
            scroll_offset: 0,
            active_section: FileOpenSection::Files,
            filter: String::new(),
            shortcuts,
            selected_shortcut: 0,
            show_hidden: false,
        }
    }

    /// Build navigation shortcuts for the given directory
    fn build_shortcuts(current_dir: &PathBuf) -> Vec<NavigationShortcut> {
        let mut shortcuts = Vec::new();

        // Parent directory
        if let Some(parent) = current_dir.parent() {
            shortcuts.push(NavigationShortcut {
                label: "..".to_string(),
                path: parent.to_path_buf(),
                description: "Parent directory".to_string(),
            });
        }

        // Root directory
        #[cfg(unix)]
        {
            shortcuts.push(NavigationShortcut {
                label: "/".to_string(),
                path: PathBuf::from("/"),
                description: "Root directory".to_string(),
            });
        }

        // Home directory
        if let Some(home) = dirs::home_dir() {
            shortcuts.push(NavigationShortcut {
                label: "~".to_string(),
                path: home,
                description: "Home directory".to_string(),
            });
        }

        // Documents directory
        if let Some(docs) = dirs::document_dir() {
            shortcuts.push(NavigationShortcut {
                label: "Documents".to_string(),
                path: docs,
                description: "Documents folder".to_string(),
            });
        }

        // Downloads directory
        if let Some(downloads) = dirs::download_dir() {
            shortcuts.push(NavigationShortcut {
                label: "Downloads".to_string(),
                path: downloads,
                description: "Downloads folder".to_string(),
            });
        }

        // Windows: Add drive letters
        #[cfg(windows)]
        {
            for letter in b'A'..=b'Z' {
                let path = PathBuf::from(format!("{}:\\", letter as char));
                if path.exists() {
                    shortcuts.push(NavigationShortcut {
                        label: format!("{}:", letter as char),
                        path,
                        description: "Drive".to_string(),
                    });
                }
            }
        }

        shortcuts
    }

    /// Update shortcuts when directory changes
    pub fn update_shortcuts(&mut self) {
        self.shortcuts = Self::build_shortcuts(&self.current_dir);
        self.selected_shortcut = 0;
    }

    /// Set entries from filesystem and apply initial sort
    pub fn set_entries(&mut self, entries: Vec<FsEntry>) {
        self.entries = entries
            .into_iter()
            .filter(|e| self.show_hidden || !Self::is_hidden(&e.name))
            .map(|fs_entry| FileOpenEntry {
                fs_entry,
                matches_filter: true,
            })
            .collect();
        self.loading = false;
        self.error = None;
        self.apply_filter_internal();
        self.sort_entries();
        self.selected_index = 0;
        self.scroll_offset = 0;
    }

    /// Set error state
    pub fn set_error(&mut self, error: String) {
        self.loading = false;
        self.error = Some(error);
        self.entries.clear();
    }

    /// Check if a filename is hidden (starts with .)
    fn is_hidden(name: &str) -> bool {
        name.starts_with('.')
    }

    /// Apply filter text to entries
    pub fn apply_filter(&mut self, filter: &str) {
        self.filter = filter.to_string();
        self.apply_filter_internal();
        self.sort_entries();

        // Try to keep selection on a matching entry
        if !self.entries.is_empty() && !self.entries[self.selected_index].matches_filter {
            if let Some(first_match) = self.entries.iter().position(|e| e.matches_filter) {
                self.selected_index = first_match;
                self.ensure_selected_visible();
            }
        }
    }

    fn apply_filter_internal(&mut self) {
        let filter_lower = self.filter.to_lowercase();
        for entry in &mut self.entries {
            entry.matches_filter =
                self.filter.is_empty() || entry.fs_entry.name.to_lowercase().contains(&filter_lower);
        }
    }

    /// Sort entries according to current sort mode
    pub fn sort_entries(&mut self) {
        let sort_mode = self.sort_mode;
        let ascending = self.sort_ascending;

        self.entries.sort_by(|a, b| {
            // Matching entries first
            match (a.matches_filter, b.matches_filter) {
                (true, false) => return Ordering::Less,
                (false, true) => return Ordering::Greater,
                _ => {}
            }

            // Directories before files
            match (a.fs_entry.is_dir(), b.fs_entry.is_dir()) {
                (true, false) => return Ordering::Less,
                (false, true) => return Ordering::Greater,
                _ => {}
            }

            // Apply sort mode
            let ord = match sort_mode {
                SortMode::Name => a
                    .fs_entry
                    .name
                    .to_lowercase()
                    .cmp(&b.fs_entry.name.to_lowercase()),
                SortMode::Size => {
                    let a_size = a
                        .fs_entry
                        .metadata
                        .as_ref()
                        .and_then(|m| m.size)
                        .unwrap_or(0);
                    let b_size = b
                        .fs_entry
                        .metadata
                        .as_ref()
                        .and_then(|m| m.size)
                        .unwrap_or(0);
                    a_size.cmp(&b_size)
                }
                SortMode::Modified => {
                    let a_mod = a.fs_entry.metadata.as_ref().and_then(|m| m.modified);
                    let b_mod = b.fs_entry.metadata.as_ref().and_then(|m| m.modified);
                    match (a_mod, b_mod) {
                        (Some(a), Some(b)) => a.cmp(&b),
                        (Some(_), None) => Ordering::Less,
                        (None, Some(_)) => Ordering::Greater,
                        (None, None) => Ordering::Equal,
                    }
                }
                SortMode::Type => {
                    let a_ext = std::path::Path::new(&a.fs_entry.name)
                        .extension()
                        .and_then(|e| e.to_str())
                        .unwrap_or("");
                    let b_ext = std::path::Path::new(&b.fs_entry.name)
                        .extension()
                        .and_then(|e| e.to_str())
                        .unwrap_or("");
                    a_ext.to_lowercase().cmp(&b_ext.to_lowercase())
                }
            };

            if ascending {
                ord
            } else {
                ord.reverse()
            }
        });
    }

    /// Set sort mode and re-sort
    pub fn set_sort_mode(&mut self, mode: SortMode) {
        if self.sort_mode == mode {
            // Toggle direction if same mode
            self.sort_ascending = !self.sort_ascending;
        } else {
            self.sort_mode = mode;
            self.sort_ascending = true;
        }
        self.sort_entries();
    }

    /// Toggle hidden files visibility
    pub fn toggle_hidden(&mut self) {
        self.show_hidden = !self.show_hidden;
        // Need to reload directory to apply this change
    }

    /// Move selection up
    pub fn select_prev(&mut self) {
        match self.active_section {
            FileOpenSection::Navigation => {
                if self.selected_shortcut > 0 {
                    self.selected_shortcut -= 1;
                }
            }
            FileOpenSection::Files => {
                if self.selected_index > 0 {
                    self.selected_index -= 1;
                    self.ensure_selected_visible();
                }
            }
        }
    }

    /// Move selection down
    pub fn select_next(&mut self) {
        match self.active_section {
            FileOpenSection::Navigation => {
                if self.selected_shortcut + 1 < self.shortcuts.len() {
                    self.selected_shortcut += 1;
                }
            }
            FileOpenSection::Files => {
                if self.selected_index + 1 < self.entries.len() {
                    self.selected_index += 1;
                    self.ensure_selected_visible();
                }
            }
        }
    }

    /// Page up
    pub fn page_up(&mut self, page_size: usize) {
        if self.active_section == FileOpenSection::Files {
            self.selected_index = self.selected_index.saturating_sub(page_size);
            self.ensure_selected_visible();
        }
    }

    /// Page down
    pub fn page_down(&mut self, page_size: usize) {
        if self.active_section == FileOpenSection::Files {
            self.selected_index = (self.selected_index + page_size).min(self.entries.len().saturating_sub(1));
            self.ensure_selected_visible();
        }
    }

    /// Jump to first entry
    pub fn select_first(&mut self) {
        match self.active_section {
            FileOpenSection::Navigation => self.selected_shortcut = 0,
            FileOpenSection::Files => {
                self.selected_index = 0;
                self.scroll_offset = 0;
            }
        }
    }

    /// Jump to last entry
    pub fn select_last(&mut self) {
        match self.active_section {
            FileOpenSection::Navigation => {
                self.selected_shortcut = self.shortcuts.len().saturating_sub(1);
            }
            FileOpenSection::Files => {
                self.selected_index = self.entries.len().saturating_sub(1);
                self.ensure_selected_visible();
            }
        }
    }

    /// Ensure selected item is visible in viewport
    fn ensure_selected_visible(&mut self) {
        // This will be called with actual visible_rows from renderer
        // For now, use a reasonable default
        let visible_rows = 15;
        if self.selected_index < self.scroll_offset {
            self.scroll_offset = self.selected_index;
        } else if self.selected_index >= self.scroll_offset + visible_rows {
            self.scroll_offset = self.selected_index.saturating_sub(visible_rows - 1);
        }
    }

    /// Update scroll offset based on visible rows
    pub fn update_scroll_for_visible_rows(&mut self, visible_rows: usize) {
        if self.selected_index < self.scroll_offset {
            self.scroll_offset = self.selected_index;
        } else if self.selected_index >= self.scroll_offset + visible_rows {
            self.scroll_offset = self.selected_index.saturating_sub(visible_rows - 1);
        }
    }

    /// Switch between navigation and files sections
    pub fn switch_section(&mut self) {
        self.active_section = match self.active_section {
            FileOpenSection::Navigation => FileOpenSection::Files,
            FileOpenSection::Files => FileOpenSection::Navigation,
        };
    }

    /// Get the currently selected entry (file or directory)
    pub fn selected_entry(&self) -> Option<&FileOpenEntry> {
        if self.active_section == FileOpenSection::Files {
            self.entries.get(self.selected_index)
        } else {
            None
        }
    }

    /// Get the currently selected shortcut
    pub fn selected_shortcut_entry(&self) -> Option<&NavigationShortcut> {
        if self.active_section == FileOpenSection::Navigation {
            self.shortcuts.get(self.selected_shortcut)
        } else {
            None
        }
    }

    /// Get the path to open/navigate to based on current selection
    pub fn get_selected_path(&self) -> Option<PathBuf> {
        match self.active_section {
            FileOpenSection::Navigation => self
                .shortcuts
                .get(self.selected_shortcut)
                .map(|s| s.path.clone()),
            FileOpenSection::Files => self
                .entries
                .get(self.selected_index)
                .map(|e| e.fs_entry.path.clone()),
        }
    }

    /// Check if selected item is a directory
    pub fn selected_is_dir(&self) -> bool {
        match self.active_section {
            FileOpenSection::Navigation => true, // Shortcuts are always directories
            FileOpenSection::Files => self
                .entries
                .get(self.selected_index)
                .map(|e| e.fs_entry.is_dir())
                .unwrap_or(false),
        }
    }

    /// Count matching entries
    pub fn matching_count(&self) -> usize {
        self.entries.iter().filter(|e| e.matches_filter).count()
    }

    /// Get visible entries (for rendering)
    pub fn visible_entries(&self, max_rows: usize) -> &[FileOpenEntry] {
        let start = self.scroll_offset;
        let end = (start + max_rows).min(self.entries.len());
        &self.entries[start..end]
    }
}

/// Format file size in human-readable form
pub fn format_size(size: u64) -> String {
    const KB: u64 = 1024;
    const MB: u64 = KB * 1024;
    const GB: u64 = MB * 1024;

    if size >= GB {
        format!("{:.1} GB", size as f64 / GB as f64)
    } else if size >= MB {
        format!("{:.1} MB", size as f64 / MB as f64)
    } else if size >= KB {
        format!("{:.1} KB", size as f64 / KB as f64)
    } else {
        format!("{} B", size)
    }
}

/// Format timestamp in relative or absolute form
pub fn format_modified(time: SystemTime) -> String {
    let now = SystemTime::now();
    match now.duration_since(time) {
        Ok(duration) => {
            let secs = duration.as_secs();
            if secs < 60 {
                "just now".to_string()
            } else if secs < 3600 {
                format!("{} min ago", secs / 60)
            } else if secs < 86400 {
                format!("{} hr ago", secs / 3600)
            } else if secs < 86400 * 7 {
                format!("{} days ago", secs / 86400)
            } else {
                // Format as date
                let datetime: chrono::DateTime<chrono::Local> = time.into();
                datetime.format("%Y-%m-%d").to_string()
            }
        }
        Err(_) => {
            // Time is in the future
            let datetime: chrono::DateTime<chrono::Local> = time.into();
            datetime.format("%Y-%m-%d").to_string()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::services::fs::{FsEntryType, FsMetadata};

    fn make_entry(name: &str, is_dir: bool) -> FsEntry {
        FsEntry {
            path: PathBuf::from(format!("/test/{}", name)),
            name: name.to_string(),
            entry_type: if is_dir {
                FsEntryType::Directory
            } else {
                FsEntryType::File
            },
            metadata: None,
        }
    }

    fn make_entry_with_size(name: &str, size: u64) -> FsEntry {
        let mut entry = make_entry(name, false);
        entry.metadata = Some(FsMetadata {
            size: Some(size),
            modified: None,
            is_hidden: false,
            is_readonly: false,
        });
        entry
    }

    #[test]
    fn test_sort_by_name() {
        let mut state = FileOpenState::new(PathBuf::from("/test"));
        state.set_entries(vec![
            make_entry("zebra.txt", false),
            make_entry("alpha.txt", false),
            make_entry("beta", true),
        ]);

        assert_eq!(state.entries[0].fs_entry.name, "beta"); // Dir first
        assert_eq!(state.entries[1].fs_entry.name, "alpha.txt");
        assert_eq!(state.entries[2].fs_entry.name, "zebra.txt");
    }

    #[test]
    fn test_sort_by_size() {
        let mut state = FileOpenState::new(PathBuf::from("/test"));
        state.sort_mode = SortMode::Size;
        state.set_entries(vec![
            make_entry_with_size("big.txt", 1000),
            make_entry_with_size("small.txt", 100),
            make_entry_with_size("medium.txt", 500),
        ]);

        assert_eq!(state.entries[0].fs_entry.name, "small.txt");
        assert_eq!(state.entries[1].fs_entry.name, "medium.txt");
        assert_eq!(state.entries[2].fs_entry.name, "big.txt");
    }

    #[test]
    fn test_filter() {
        let mut state = FileOpenState::new(PathBuf::from("/test"));
        state.set_entries(vec![
            make_entry("foo.txt", false),
            make_entry("bar.txt", false),
            make_entry("foobar.txt", false),
        ]);

        state.apply_filter("foo");

        // Matching entries should come first
        assert!(state.entries[0].matches_filter);
        assert!(state.entries[1].matches_filter);
        assert!(!state.entries[2].matches_filter);

        assert_eq!(state.matching_count(), 2);
    }

    #[test]
    fn test_filter_case_insensitive() {
        let mut state = FileOpenState::new(PathBuf::from("/test"));
        state.set_entries(vec![
            make_entry("README.md", false),
            make_entry("readme.txt", false),
            make_entry("other.txt", false),
        ]);

        state.apply_filter("readme");

        assert!(state.entries[0].matches_filter);
        assert!(state.entries[1].matches_filter);
        assert!(!state.entries[2].matches_filter);
    }

    #[test]
    fn test_hidden_files() {
        let mut state = FileOpenState::new(PathBuf::from("/test"));
        state.show_hidden = false;
        state.set_entries(vec![
            make_entry(".hidden", false),
            make_entry("visible.txt", false),
        ]);

        // Hidden file should be filtered out
        assert_eq!(state.entries.len(), 1);
        assert_eq!(state.entries[0].fs_entry.name, "visible.txt");
    }

    #[test]
    fn test_format_size() {
        assert_eq!(format_size(500), "500 B");
        assert_eq!(format_size(1024), "1.0 KB");
        assert_eq!(format_size(1536), "1.5 KB");
        assert_eq!(format_size(1048576), "1.0 MB");
        assert_eq!(format_size(1073741824), "1.0 GB");
    }

    #[test]
    fn test_navigation() {
        let mut state = FileOpenState::new(PathBuf::from("/test"));
        state.set_entries(vec![
            make_entry("a.txt", false),
            make_entry("b.txt", false),
            make_entry("c.txt", false),
        ]);

        assert_eq!(state.selected_index, 0);

        state.select_next();
        assert_eq!(state.selected_index, 1);

        state.select_next();
        assert_eq!(state.selected_index, 2);

        state.select_next(); // Should stay at last
        assert_eq!(state.selected_index, 2);

        state.select_prev();
        assert_eq!(state.selected_index, 1);

        state.select_first();
        assert_eq!(state.selected_index, 0);

        state.select_last();
        assert_eq!(state.selected_index, 2);
    }
}
