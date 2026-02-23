use super::ignore::IgnorePatterns;
use super::node::NodeId;
use super::search::FileExplorerSearch;
use super::tree::FileTree;
use crate::input::fuzzy::FuzzyMatch;
use crate::model::filesystem::DirEntry;
use std::collections::HashMap;
use std::path::PathBuf;

/// View state for file tree navigation and filtering
#[derive(Debug)]
pub struct FileTreeView {
    /// The underlying tree model
    tree: FileTree,
    /// Currently selected node
    selected_node: Option<NodeId>,
    /// Scroll offset (index into visible nodes)
    scroll_offset: usize,
    /// Sort mode for entries
    sort_mode: SortMode,
    /// Ignore patterns for filtering
    ignore_patterns: IgnorePatterns,
    /// Last known viewport height (for scrolling calculations)
    pub(crate) viewport_height: usize,
    /// Search state for quick navigation
    search: FileExplorerSearch,
}

/// Sort mode for file tree entries
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SortMode {
    /// Sort by name alphabetically
    Name,
    /// Sort by type (directories first, then files)
    Type,
    /// Sort by modification time (newest first)
    Modified,
}

impl FileTreeView {
    /// Create a new file tree view
    pub fn new(tree: FileTree) -> Self {
        let root_id = tree.root_id();
        Self {
            tree,
            selected_node: Some(root_id),
            scroll_offset: 0,
            sort_mode: SortMode::Type,
            ignore_patterns: IgnorePatterns::new(),
            viewport_height: 10, // Default, will be updated during rendering
            search: FileExplorerSearch::new(),
        }
    }

    /// Get visible nodes filtered by ignore patterns (hidden files, gitignored, etc.)
    ///
    /// This wraps `tree.get_visible_nodes()` and removes nodes that should be
    /// hidden according to the current ignore settings. The root node is never
    /// filtered out.
    fn filtered_visible_nodes(&self) -> Vec<NodeId> {
        let root_id = self.tree.root_id();
        self.tree
            .get_visible_nodes()
            .into_iter()
            .filter(|&id| id == root_id || self.is_node_visible(id))
            .collect()
    }

    /// Set the viewport height (should be called during rendering)
    pub fn set_viewport_height(&mut self, height: usize) {
        self.viewport_height = height;
    }

    /// Get the underlying tree
    pub fn tree(&self) -> &FileTree {
        &self.tree
    }

    /// Get mutable reference to the underlying tree
    pub fn tree_mut(&mut self) -> &mut FileTree {
        &mut self.tree
    }

    /// Get currently visible nodes with their indent levels
    ///
    /// Returns a list of (NodeId, indent_level) tuples for rendering.
    pub fn get_display_nodes(&self) -> Vec<(NodeId, usize)> {
        let visible = self.filtered_visible_nodes();
        visible
            .into_iter()
            .map(|id| {
                let depth = self.tree.get_depth(id);
                (id, depth)
            })
            .collect()
    }

    /// Get the currently selected node ID
    pub fn get_selected(&self) -> Option<NodeId> {
        self.selected_node
    }

    /// Set the selected node
    pub fn set_selected(&mut self, node_id: Option<NodeId>) {
        self.selected_node = node_id;
    }

    /// Select the next visible node
    pub fn select_next(&mut self) {
        let visible = self.filtered_visible_nodes();
        if visible.is_empty() {
            return;
        }

        if let Some(current) = self.selected_node {
            if let Some(pos) = visible.iter().position(|&id| id == current) {
                if pos + 1 < visible.len() {
                    self.selected_node = Some(visible[pos + 1]);
                }
            }
        } else {
            self.selected_node = Some(visible[0]);
        }
    }

    /// Select the previous visible node
    pub fn select_prev(&mut self) {
        let visible = self.filtered_visible_nodes();
        if visible.is_empty() {
            return;
        }

        if let Some(current) = self.selected_node {
            if let Some(pos) = visible.iter().position(|&id| id == current) {
                if pos > 0 {
                    self.selected_node = Some(visible[pos - 1]);
                }
            }
        } else {
            self.selected_node = Some(visible[0]);
        }
    }

    /// Move selection up by a page (viewport height)
    pub fn select_page_up(&mut self) {
        if self.viewport_height == 0 {
            return;
        }

        let visible = self.filtered_visible_nodes();
        if visible.is_empty() {
            return;
        }

        if let Some(current) = self.selected_node {
            if let Some(pos) = visible.iter().position(|&id| id == current) {
                let new_pos = pos.saturating_sub(self.viewport_height);
                self.selected_node = Some(visible[new_pos]);
            }
        } else {
            self.selected_node = Some(visible[0]);
        }
    }

    /// Move selection down by a page (viewport height)
    pub fn select_page_down(&mut self) {
        if self.viewport_height == 0 {
            return;
        }

        let visible = self.filtered_visible_nodes();
        if visible.is_empty() {
            return;
        }

        if let Some(current) = self.selected_node {
            if let Some(pos) = visible.iter().position(|&id| id == current) {
                let new_pos = (pos + self.viewport_height).min(visible.len() - 1);
                self.selected_node = Some(visible[new_pos]);
            }
        } else {
            self.selected_node = Some(visible[0]);
        }
    }

    /// Update scroll offset to ensure symmetric scrolling behavior
    ///
    /// This should be called after navigation to implement symmetric scrolling:
    /// - When moving down, cursor moves to bottom of viewport before scrolling
    /// - When moving up, cursor moves to top of viewport before scrolling
    ///
    /// Uses the stored viewport_height which is updated during rendering.
    pub fn update_scroll_for_selection(&mut self) {
        if self.viewport_height == 0 {
            return;
        }

        if let Some(selected) = self.selected_node {
            let visible = self.filtered_visible_nodes();
            if let Some(pos) = visible.iter().position(|&id| id == selected) {
                // Only scroll if cursor goes PAST the viewport edges
                // This implements symmetric scrolling behavior

                // If selection is above the visible area, scroll up
                if pos < self.scroll_offset {
                    self.scroll_offset = pos;
                }
                // If selection is below the visible area, scroll down
                else if pos >= self.scroll_offset + self.viewport_height {
                    self.scroll_offset = pos - self.viewport_height + 1;
                }
                // Otherwise, cursor is within viewport - don't scroll
            }
        }
    }

    /// Select the first visible node
    pub fn select_first(&mut self) {
        let visible = self.filtered_visible_nodes();
        if !visible.is_empty() {
            self.selected_node = Some(visible[0]);
        }
    }

    /// Select the last visible node
    pub fn select_last(&mut self) {
        let visible = self.filtered_visible_nodes();
        if !visible.is_empty() {
            self.selected_node = Some(*visible.last().unwrap());
        }
    }

    /// Select the parent of the currently selected node
    pub fn select_parent(&mut self) {
        if let Some(current) = self.selected_node {
            if let Some(node) = self.tree.get_node(current) {
                if let Some(parent_id) = node.parent {
                    self.selected_node = Some(parent_id);
                }
            }
        }
    }

    /// Get the scroll offset
    pub fn get_scroll_offset(&self) -> usize {
        self.scroll_offset
    }

    /// Set the scroll offset
    pub fn set_scroll_offset(&mut self, offset: usize) {
        self.scroll_offset = offset;
    }

    /// Ensure the selected node is visible within the viewport
    ///
    /// Adjusts scroll offset if necessary to keep the selected node visible.
    ///
    /// # Arguments
    ///
    /// * `viewport_height` - Number of visible lines in the viewport
    pub fn ensure_visible(&mut self, viewport_height: usize) {
        if viewport_height == 0 {
            return;
        }

        if let Some(selected) = self.selected_node {
            let visible = self.filtered_visible_nodes();
            if let Some(pos) = visible.iter().position(|&id| id == selected) {
                // If selection is above viewport, scroll up
                if pos < self.scroll_offset {
                    self.scroll_offset = pos;
                }
                // If selection is below viewport, scroll down
                else if pos >= self.scroll_offset + viewport_height {
                    self.scroll_offset = pos - viewport_height + 1;
                }
            }
        }
    }

    /// Get the sort mode
    pub fn get_sort_mode(&self) -> SortMode {
        self.sort_mode
    }

    /// Set the sort mode
    pub fn set_sort_mode(&mut self, mode: SortMode) {
        self.sort_mode = mode;
        // TODO: Re-sort children when sort mode changes
    }

    /// Get selected node entry (convenience method)
    pub fn get_selected_entry(&self) -> Option<&DirEntry> {
        self.selected_node
            .and_then(|id| self.tree.get_node(id))
            .map(|node| &node.entry)
    }

    /// Navigate to a specific path if it exists in the tree
    pub fn navigate_to_path(&mut self, path: &std::path::Path) {
        if let Some(node) = self.tree.get_node_by_path(path) {
            self.selected_node = Some(node.id);
        }
    }

    /// Get the index of the selected node in the visible list
    pub fn get_selected_index(&self) -> Option<usize> {
        if let Some(selected) = self.selected_node {
            let visible = self.filtered_visible_nodes();
            visible.iter().position(|&id| id == selected)
        } else {
            None
        }
    }

    /// Get visible node at index (accounting for scroll offset)
    pub fn get_node_at_index(&self, index: usize) -> Option<NodeId> {
        let visible = self.filtered_visible_nodes();
        visible.get(index).copied()
    }

    /// Get the number of visible nodes
    pub fn visible_count(&self) -> usize {
        self.filtered_visible_nodes().len()
    }

    /// Get reference to ignore patterns
    pub fn ignore_patterns(&self) -> &IgnorePatterns {
        &self.ignore_patterns
    }

    /// Get mutable reference to ignore patterns
    pub fn ignore_patterns_mut(&mut self) -> &mut IgnorePatterns {
        &mut self.ignore_patterns
    }

    /// Toggle showing hidden files
    pub fn toggle_show_hidden(&mut self) {
        self.ignore_patterns.toggle_show_hidden();
    }

    /// Toggle showing gitignored files
    pub fn toggle_show_gitignored(&mut self) {
        self.ignore_patterns.toggle_show_gitignored();
    }

    /// Check if a node should be visible (not filtered by ignore patterns)
    pub fn is_node_visible(&self, node_id: NodeId) -> bool {
        if let Some(node) = self.tree.get_node(node_id) {
            !self
                .ignore_patterns
                .is_ignored(&node.entry.path, node.is_dir())
        } else {
            false
        }
    }

    /// Load .gitignore for a directory
    ///
    /// This should be called when expanding a directory to load its .gitignore
    pub fn load_gitignore_for_dir(&mut self, dir_path: &std::path::Path) -> std::io::Result<()> {
        self.ignore_patterns.load_gitignore(dir_path)
    }

    /// Expand all parent directories and select the given file path
    ///
    /// This is useful for revealing a specific file in the tree when switching
    /// focus to the file explorer. All parent directories will be expanded as needed,
    /// and the file will be selected.
    ///
    /// # Arguments
    ///
    /// * `path` - The full path to the file to reveal and select
    ///
    /// # Returns
    ///
    /// Returns true if the file was successfully expanded and selected, false otherwise.
    /// This will return false if:
    /// - The path is not under the root directory
    /// - The path doesn't exist
    /// - There was an error expanding intermediate directories
    pub async fn expand_and_select_file(&mut self, path: &std::path::Path) -> bool {
        if let Some(node_id) = self.tree.expand_to_path(path).await {
            self.selected_node = Some(node_id);
            true
        } else {
            false
        }
    }

    /// Collect symlink mappings from expanded symlink directories.
    ///
    /// Returns a HashMap where keys are symlink paths and values are their canonical targets.
    /// This is used to create decoration aliases so files under symlinked directories
    /// can show their git status correctly.
    pub fn collect_symlink_mappings(&self) -> HashMap<PathBuf, PathBuf> {
        let mut mappings = HashMap::new();

        for node_id in self.filtered_visible_nodes() {
            if let Some(node) = self.tree.get_node(node_id) {
                // Only process expanded symlink directories
                if node.entry.is_symlink() && node.is_dir() && node.is_expanded() {
                    // Canonicalize the symlink to get the target
                    if let Ok(canonical) = node.entry.path.canonicalize() {
                        if canonical != node.entry.path {
                            mappings.insert(node.entry.path.clone(), canonical);
                        }
                    }
                }
            }
        }

        mappings
    }

    // ==================== Search Methods ====================

    /// Get the current search query
    pub fn search_query(&self) -> &str {
        self.search.query()
    }

    /// Check if search is active
    pub fn is_search_active(&self) -> bool {
        self.search.is_active()
    }

    /// Add a character to the search query and jump to first match
    pub fn search_push_char(&mut self, c: char) {
        self.search.push_char(c);
        self.jump_to_first_match();
    }

    /// Remove the last character from the search query
    pub fn search_pop_char(&mut self) {
        self.search.pop_char();
        if self.search.is_active() {
            self.jump_to_first_match();
        }
    }

    /// Clear the search query
    pub fn search_clear(&mut self) {
        self.search.clear();
    }

    /// Get nodes that match the current search query
    fn get_matching_nodes(&self) -> Vec<NodeId> {
        if !self.search.is_active() {
            return self.filtered_visible_nodes();
        }

        self.filtered_visible_nodes()
            .into_iter()
            .filter(|&id| {
                if let Some(node) = self.tree.get_node(id) {
                    self.search.matches(&node.entry.name)
                } else {
                    false
                }
            })
            .collect()
    }

    /// Jump to the first matching node
    fn jump_to_first_match(&mut self) {
        let matching = self.get_matching_nodes();
        if let Some(&first) = matching.first() {
            self.selected_node = Some(first);
            self.update_scroll_for_selection();
        }
    }

    /// Select the next matching node (when search is active)
    pub fn select_next_match(&mut self) {
        if !self.search.is_active() {
            self.select_next();
            return;
        }

        let matching = self.get_matching_nodes();
        if matching.is_empty() {
            return;
        }

        if let Some(current) = self.selected_node {
            if let Some(pos) = matching.iter().position(|&id| id == current) {
                // Move to next match (wrap around)
                let next_pos = (pos + 1) % matching.len();
                self.selected_node = Some(matching[next_pos]);
            } else {
                // Current not in matches, select first match
                self.selected_node = Some(matching[0]);
            }
        } else {
            self.selected_node = Some(matching[0]);
        }
    }

    /// Select the previous matching node (when search is active)
    pub fn select_prev_match(&mut self) {
        if !self.search.is_active() {
            self.select_prev();
            return;
        }

        let matching = self.get_matching_nodes();
        if matching.is_empty() {
            return;
        }

        if let Some(current) = self.selected_node {
            if let Some(pos) = matching.iter().position(|&id| id == current) {
                // Move to previous match (wrap around)
                let prev_pos = if pos == 0 {
                    matching.len() - 1
                } else {
                    pos - 1
                };
                self.selected_node = Some(matching[prev_pos]);
            } else {
                // Current not in matches, select last match
                self.selected_node = Some(*matching.last().unwrap());
            }
        } else {
            self.selected_node = Some(*matching.last().unwrap());
        }
    }

    /// Get match result for a node's name (for highlighting)
    pub fn get_match_for_node(&self, node_id: NodeId) -> Option<FuzzyMatch> {
        if !self.search.is_active() {
            return None;
        }

        self.tree
            .get_node(node_id)
            .and_then(|node| self.search.match_name(&node.entry.name))
    }

    /// Check if a node matches the current search
    pub fn node_matches_search(&self, node_id: NodeId) -> bool {
        if !self.search.is_active() {
            return true;
        }

        self.tree
            .get_node(node_id)
            .map(|node| self.search.matches(&node.entry.name))
            .unwrap_or(false)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::filesystem::StdFileSystem;
    use crate::services::fs::FsManager;
    use std::fs as std_fs;
    use std::sync::Arc;
    use tempfile::TempDir;

    async fn create_test_view() -> (TempDir, FileTreeView) {
        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();

        // Create test structure
        std_fs::create_dir(temp_path.join("dir1")).unwrap();
        std_fs::write(temp_path.join("dir1/file1.txt"), "content1").unwrap();
        std_fs::write(temp_path.join("dir1/file2.txt"), "content2").unwrap();
        std_fs::create_dir(temp_path.join("dir2")).unwrap();
        std_fs::write(temp_path.join("file3.txt"), "content3").unwrap();

        let backend = Arc::new(StdFileSystem);
        let manager = Arc::new(FsManager::new(backend));
        let tree = FileTree::new(temp_path.to_path_buf(), manager)
            .await
            .unwrap();
        let view = FileTreeView::new(tree);

        (temp_dir, view)
    }

    #[tokio::test]
    async fn test_view_creation() {
        let (_temp_dir, view) = create_test_view().await;

        assert!(view.get_selected().is_some());
        assert_eq!(view.get_scroll_offset(), 0);
        assert_eq!(view.get_sort_mode(), SortMode::Type);
    }

    #[tokio::test]
    async fn test_get_display_nodes() {
        let (_temp_dir, mut view) = create_test_view().await;

        // Initially only root
        let display = view.get_display_nodes();
        assert_eq!(display.len(), 1);
        assert_eq!(display[0].1, 0); // Root has depth 0

        // Expand root
        let root_id = view.tree().root_id();
        view.tree_mut().expand_node(root_id).await.unwrap();

        let display = view.get_display_nodes();
        assert_eq!(display.len(), 4); // root + 3 children

        // Check depths
        assert_eq!(display[0].1, 0); // root
        assert_eq!(display[1].1, 1); // child
        assert_eq!(display[2].1, 1); // child
        assert_eq!(display[3].1, 1); // child
    }

    #[tokio::test]
    async fn test_navigation() {
        let (_temp_dir, mut view) = create_test_view().await;

        let root_id = view.tree().root_id();
        view.tree_mut().expand_node(root_id).await.unwrap();

        let root_id = view.tree().root_id();
        assert_eq!(view.get_selected(), Some(root_id));

        // Select next
        view.select_next();
        assert_ne!(view.get_selected(), Some(root_id));

        // Select prev
        view.select_prev();
        assert_eq!(view.get_selected(), Some(root_id));

        // Select last
        view.select_last();
        let visible = view.tree().get_visible_nodes();
        assert_eq!(view.get_selected(), Some(*visible.last().unwrap()));

        // Select first
        view.select_first();
        assert_eq!(view.get_selected(), Some(root_id));
    }

    #[tokio::test]
    async fn test_select_parent() {
        let (_temp_dir, mut view) = create_test_view().await;

        let root_id = view.tree().root_id();
        view.tree_mut().expand_node(root_id).await.unwrap();

        // Select first child
        view.select_next();
        let child_id = view.get_selected().unwrap();
        assert_ne!(child_id, root_id);

        // Select parent
        view.select_parent();
        assert_eq!(view.get_selected(), Some(root_id));
    }

    #[tokio::test]
    async fn test_ensure_visible() {
        let (_temp_dir, mut view) = create_test_view().await;

        let root_id = view.tree().root_id();
        view.tree_mut().expand_node(root_id).await.unwrap();

        let viewport_height = 2;

        // Select last item
        view.select_last();
        view.ensure_visible(viewport_height);

        // Scroll offset should be adjusted
        let selected_index = view.get_selected_index().unwrap();
        assert!(selected_index >= view.get_scroll_offset());
        assert!(selected_index < view.get_scroll_offset() + viewport_height);

        // Select first item
        view.select_first();
        view.ensure_visible(viewport_height);

        // Scroll offset should be 0
        assert_eq!(view.get_scroll_offset(), 0);
    }

    #[tokio::test]
    async fn test_get_selected_entry() {
        let (_temp_dir, view) = create_test_view().await;

        let entry = view.get_selected_entry();
        assert!(entry.is_some());
        assert!(entry.unwrap().is_dir());
    }

    #[tokio::test]
    async fn test_navigate_to_path() {
        let (_temp_dir, mut view) = create_test_view().await;

        let root_id = view.tree().root_id();
        view.tree_mut().expand_node(root_id).await.unwrap();

        let dir1_path = view.tree().root_path().join("dir1");
        view.navigate_to_path(&dir1_path);

        let selected_entry = view.get_selected_entry().unwrap();
        assert_eq!(selected_entry.name, "dir1");
    }

    #[tokio::test]
    async fn test_get_selected_index() {
        let (_temp_dir, mut view) = create_test_view().await;

        let root_id = view.tree().root_id();
        view.tree_mut().expand_node(root_id).await.unwrap();

        // Root is at index 0
        assert_eq!(view.get_selected_index(), Some(0));

        // Move to next
        view.select_next();
        assert_eq!(view.get_selected_index(), Some(1));

        // Move to last
        view.select_last();
        let visible_count = view.visible_count();
        assert_eq!(view.get_selected_index(), Some(visible_count - 1));
    }

    #[tokio::test]
    async fn test_visible_count() {
        let (_temp_dir, mut view) = create_test_view().await;

        // Initially only root
        assert_eq!(view.visible_count(), 1);

        // After expanding root
        let root_id = view.tree().root_id();
        view.tree_mut().expand_node(root_id).await.unwrap();
        assert_eq!(view.visible_count(), 4); // root + 3 children
    }

    #[tokio::test]
    async fn test_sort_mode() {
        let (_temp_dir, mut view) = create_test_view().await;

        assert_eq!(view.get_sort_mode(), SortMode::Type);

        view.set_sort_mode(SortMode::Name);
        assert_eq!(view.get_sort_mode(), SortMode::Name);

        view.set_sort_mode(SortMode::Modified);
        assert_eq!(view.get_sort_mode(), SortMode::Modified);
    }
}
