use super::node::{NodeId, NodeState, TreeNode};
use crate::fs::{FsEntry, FsManager};
use std::collections::HashMap;
use std::io;
use std::path::{Path, PathBuf};
use std::sync::Arc;

/// File tree with lazy loading support
///
/// The tree starts with just the root node. Directories are only read
/// when explicitly expanded via `expand_node()`. This makes the tree
/// efficient even for very large directory structures.
#[derive(Debug)]
pub struct FileTree {
    /// Root directory path
    root_path: PathBuf,
    /// All nodes indexed by ID
    nodes: HashMap<NodeId, TreeNode>,
    /// Path to node ID mapping for quick lookups
    path_to_node: HashMap<PathBuf, NodeId>,
    /// Root node ID
    root_id: NodeId,
    /// Next node ID to assign
    next_id: usize,
    /// Filesystem manager for async operations
    fs_manager: Arc<FsManager>,
}

impl FileTree {
    /// Create a new file tree rooted at the given path
    ///
    /// # Errors
    ///
    /// Returns an error if the root path doesn't exist or isn't a directory.
    pub async fn new(root_path: PathBuf, fs_manager: Arc<FsManager>) -> io::Result<Self> {
        // Verify root path exists and is a directory
        if !fs_manager.exists(&root_path).await {
            return Err(io::Error::new(
                io::ErrorKind::NotFound,
                format!("Path does not exist: {:?}", root_path),
            ));
        }

        if !fs_manager.is_dir(&root_path).await? {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                format!("Path is not a directory: {:?}", root_path),
            ));
        }

        // Get root entry
        let root_entry = fs_manager.get_entry(&root_path).await?;

        // Create root node
        let root_id = NodeId(0);
        let root_node = TreeNode::new(root_id, root_entry.clone(), None);

        let mut nodes = HashMap::new();
        nodes.insert(root_id, root_node);

        let mut path_to_node = HashMap::new();
        path_to_node.insert(root_path.clone(), root_id);

        Ok(Self {
            root_path,
            nodes,
            path_to_node,
            root_id,
            next_id: 1,
            fs_manager,
        })
    }

    /// Get the root node ID
    pub fn root_id(&self) -> NodeId {
        self.root_id
    }

    /// Get the root path
    pub fn root_path(&self) -> &Path {
        &self.root_path
    }

    /// Get a node by ID
    pub fn get_node(&self, id: NodeId) -> Option<&TreeNode> {
        self.nodes.get(&id)
    }

    /// Get a mutable reference to a node by ID
    fn get_node_mut(&mut self, id: NodeId) -> Option<&mut TreeNode> {
        self.nodes.get_mut(&id)
    }

    /// Get a node by path
    pub fn get_node_by_path(&self, path: &Path) -> Option<&TreeNode> {
        self.path_to_node.get(path).and_then(|id| self.get_node(*id))
    }

    /// Get all nodes
    pub fn all_nodes(&self) -> impl Iterator<Item = &TreeNode> {
        self.nodes.values()
    }

    /// Expand a directory node (load its children)
    ///
    /// This is an async operation that reads the directory contents and creates
    /// child nodes. If the directory is already expanded, this does nothing.
    ///
    /// # Errors
    ///
    /// Returns an error if the directory cannot be read.
    pub async fn expand_node(&mut self, id: NodeId) -> io::Result<()> {
        // Check if node exists and is a directory
        let node = self
            .get_node(id)
            .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "Node not found"))?;

        if !node.is_dir() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "Cannot expand a file node",
            ));
        }

        // If already expanded, do nothing
        if node.is_expanded() {
            return Ok(());
        }

        // Set state to loading
        if let Some(node) = self.get_node_mut(id) {
            node.state = NodeState::Loading;
        }

        // Read directory contents
        let path = self.get_node(id).unwrap().entry.path.clone();
        let result = self.fs_manager.list_dir(path.clone()).await;

        match result {
            Ok(entries) => {
                // Sort entries: directories first, then by name
                let mut sorted_entries = entries;
                sorted_entries.sort_by(|a, b| {
                    match (a.is_dir(), b.is_dir()) {
                        (true, false) => std::cmp::Ordering::Less,
                        (false, true) => std::cmp::Ordering::Greater,
                        _ => a.name.to_lowercase().cmp(&b.name.to_lowercase()),
                    }
                });

                // Create child nodes
                let mut child_ids = Vec::new();
                for entry in sorted_entries {
                    let child_id = self.add_node(entry, Some(id));
                    child_ids.push(child_id);
                }

                // Update parent node
                if let Some(node) = self.get_node_mut(id) {
                    node.children = child_ids;
                    node.state = NodeState::Expanded;
                }

                Ok(())
            }
            Err(e) => {
                // Set error state
                if let Some(node) = self.get_node_mut(id) {
                    node.state = NodeState::Error(e.to_string());
                }
                Err(e)
            }
        }
    }

    /// Collapse a directory node
    ///
    /// This removes all child nodes from memory to save space.
    /// They will be reloaded if the directory is expanded again.
    pub fn collapse_node(&mut self, id: NodeId) {
        if let Some(node) = self.get_node(id) {
            if !node.is_dir() {
                return;
            }

            // Collect child IDs to remove
            let children_to_remove: Vec<NodeId> = node.children.clone();

            // Remove all descendants recursively
            for child_id in children_to_remove {
                self.remove_node_recursive(child_id);
            }
        }

        // Update parent node state
        if let Some(node) = self.get_node_mut(id) {
            node.children.clear();
            node.state = NodeState::Collapsed;
        }
    }

    /// Toggle node expansion (expand if collapsed, collapse if expanded)
    pub async fn toggle_node(&mut self, id: NodeId) -> io::Result<()> {
        let node = self
            .get_node(id)
            .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "Node not found"))?;

        if !node.is_dir() {
            return Ok(());
        }

        if node.is_expanded() {
            self.collapse_node(id);
            Ok(())
        } else {
            self.expand_node(id).await
        }
    }

    /// Refresh a node (re-read directory contents)
    ///
    /// This is useful when filesystem contents have changed.
    pub async fn refresh_node(&mut self, id: NodeId) -> io::Result<()> {
        // Collapse and re-expand
        self.collapse_node(id);
        self.expand_node(id).await
    }

    /// Get all visible nodes in tree order
    ///
    /// Returns a flat list of nodes that should be visible, respecting
    /// the expansion state of parent directories.
    pub fn get_visible_nodes(&self) -> Vec<NodeId> {
        let mut visible = Vec::new();
        self.collect_visible_recursive(self.root_id, &mut visible);
        visible
    }

    /// Recursively collect visible nodes
    fn collect_visible_recursive(&self, id: NodeId, visible: &mut Vec<NodeId>) {
        visible.push(id);

        if let Some(node) = self.get_node(id) {
            if node.is_expanded() {
                for &child_id in &node.children {
                    self.collect_visible_recursive(child_id, visible);
                }
            }
        }
    }

    /// Get the parent chain for a node (from root to node)
    pub fn get_ancestors(&self, id: NodeId) -> Vec<NodeId> {
        let mut ancestors = Vec::new();
        let mut current = Some(id);

        while let Some(node_id) = current {
            ancestors.push(node_id);
            current = self.get_node(node_id).and_then(|n| n.parent);
        }

        ancestors.reverse();
        ancestors
    }

    /// Get the depth of a node (root is 0)
    pub fn get_depth(&self, id: NodeId) -> usize {
        self.get_ancestors(id).len() - 1
    }

    /// Find node by relative path from root
    pub fn find_by_relative_path(&self, relative_path: &Path) -> Option<NodeId> {
        let full_path = self.root_path.join(relative_path);
        self.path_to_node.get(&full_path).copied()
    }

    /// Add a new node to the tree
    fn add_node(&mut self, entry: FsEntry, parent: Option<NodeId>) -> NodeId {
        let id = NodeId(self.next_id);
        self.next_id += 1;

        let node = TreeNode::new(id, entry.clone(), parent);
        self.path_to_node.insert(entry.path.clone(), id);
        self.nodes.insert(id, node);

        id
    }

    /// Remove a node and all its descendants
    fn remove_node_recursive(&mut self, id: NodeId) {
        if let Some(node) = self.get_node(id) {
            let children = node.children.clone();
            let path = node.entry.path.clone();

            // Remove all children first
            for child_id in children {
                self.remove_node_recursive(child_id);
            }

            // Remove from path mapping
            self.path_to_node.remove(&path);

            // Remove node itself
            self.nodes.remove(&id);
        }
    }

    /// Get number of nodes currently in memory
    pub fn node_count(&self) -> usize {
        self.nodes.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fs::LocalFsBackend;
    use std::fs as std_fs;
    use tempfile::TempDir;

    async fn create_test_tree() -> (TempDir, FileTree) {
        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();

        // Create test structure:
        // /
        // ├── dir1/
        // │   ├── file1.txt
        // │   └── file2.txt
        // ├── dir2/
        // │   └── subdir/
        // │       └── file3.txt
        // └── file4.txt

        std_fs::create_dir(temp_path.join("dir1")).unwrap();
        std_fs::write(temp_path.join("dir1/file1.txt"), "content1").unwrap();
        std_fs::write(temp_path.join("dir1/file2.txt"), "content2").unwrap();

        std_fs::create_dir(temp_path.join("dir2")).unwrap();
        std_fs::create_dir(temp_path.join("dir2/subdir")).unwrap();
        std_fs::write(temp_path.join("dir2/subdir/file3.txt"), "content3").unwrap();

        std_fs::write(temp_path.join("file4.txt"), "content4").unwrap();

        let backend = Arc::new(LocalFsBackend::new());
        let manager = Arc::new(FsManager::new(backend));
        let tree = FileTree::new(temp_path.to_path_buf(), manager)
            .await
            .unwrap();

        (temp_dir, tree)
    }

    #[tokio::test]
    async fn test_tree_creation() {
        let (_temp_dir, tree) = create_test_tree().await;

        assert_eq!(tree.node_count(), 1); // Only root initially
        let root = tree.get_node(tree.root_id()).unwrap();
        assert!(root.is_collapsed());
        assert_eq!(root.children.len(), 0);
    }

    #[tokio::test]
    async fn test_expand_root() {
        let (_temp_dir, mut tree) = create_test_tree().await;

        tree.expand_node(tree.root_id()).await.unwrap();

        let root = tree.get_node(tree.root_id()).unwrap();
        assert!(root.is_expanded());
        assert_eq!(root.children.len(), 3); // dir1, dir2, file4.txt

        // Should be 4 nodes: root + 3 children
        assert_eq!(tree.node_count(), 4);
    }

    #[tokio::test]
    async fn test_expand_nested() {
        let (_temp_dir, mut tree) = create_test_tree().await;

        // Expand root
        tree.expand_node(tree.root_id()).await.unwrap();

        // Find dir1 and expand it
        let root = tree.get_node(tree.root_id()).unwrap();
        let dir1_id = root.children[0]; // dir1 (directories come first)

        tree.expand_node(dir1_id).await.unwrap();

        let dir1 = tree.get_node(dir1_id).unwrap();
        assert!(dir1.is_expanded());
        assert_eq!(dir1.children.len(), 2); // file1.txt, file2.txt

        // Total nodes: root + 3 children + 2 grandchildren = 6
        assert_eq!(tree.node_count(), 6);
    }

    #[tokio::test]
    async fn test_collapse_node() {
        let (_temp_dir, mut tree) = create_test_tree().await;

        // Expand root and dir1
        tree.expand_node(tree.root_id()).await.unwrap();
        let root = tree.get_node(tree.root_id()).unwrap();
        let dir1_id = root.children[0];
        tree.expand_node(dir1_id).await.unwrap();

        assert_eq!(tree.node_count(), 6);

        // Collapse dir1
        tree.collapse_node(dir1_id);

        let dir1 = tree.get_node(dir1_id).unwrap();
        assert!(dir1.is_collapsed());
        assert_eq!(dir1.children.len(), 0);

        // Should remove the 2 child nodes
        assert_eq!(tree.node_count(), 4);
    }

    #[tokio::test]
    async fn test_toggle_node() {
        let (_temp_dir, mut tree) = create_test_tree().await;

        tree.expand_node(tree.root_id()).await.unwrap();
        let root = tree.get_node(tree.root_id()).unwrap();
        let dir1_id = root.children[0];

        // Toggle to expand
        tree.toggle_node(dir1_id).await.unwrap();
        assert!(tree.get_node(dir1_id).unwrap().is_expanded());

        // Toggle to collapse
        tree.toggle_node(dir1_id).await.unwrap();
        assert!(tree.get_node(dir1_id).unwrap().is_collapsed());
    }

    #[tokio::test]
    async fn test_get_visible_nodes() {
        let (_temp_dir, mut tree) = create_test_tree().await;

        // Initially only root is visible
        let visible = tree.get_visible_nodes();
        assert_eq!(visible.len(), 1);

        // Expand root
        tree.expand_node(tree.root_id()).await.unwrap();
        let visible = tree.get_visible_nodes();
        assert_eq!(visible.len(), 4); // root + 3 children

        // Expand dir1
        let root = tree.get_node(tree.root_id()).unwrap();
        let dir1_id = root.children[0];
        tree.expand_node(dir1_id).await.unwrap();

        let visible = tree.get_visible_nodes();
        assert_eq!(visible.len(), 6); // root + 3 children + 2 grandchildren
    }

    #[tokio::test]
    async fn test_get_ancestors() {
        let (_temp_dir, mut tree) = create_test_tree().await;

        tree.expand_node(tree.root_id()).await.unwrap();
        let root = tree.get_node(tree.root_id()).unwrap();
        let dir1_id = root.children[0];
        tree.expand_node(dir1_id).await.unwrap();

        let dir1 = tree.get_node(dir1_id).unwrap();
        let file1_id = dir1.children[0];

        let ancestors = tree.get_ancestors(file1_id);
        assert_eq!(ancestors.len(), 3); // root -> dir1 -> file1
        assert_eq!(ancestors[0], tree.root_id());
        assert_eq!(ancestors[1], dir1_id);
        assert_eq!(ancestors[2], file1_id);
    }

    #[tokio::test]
    async fn test_get_depth() {
        let (_temp_dir, mut tree) = create_test_tree().await;

        tree.expand_node(tree.root_id()).await.unwrap();
        let root = tree.get_node(tree.root_id()).unwrap();
        let dir1_id = root.children[0];
        tree.expand_node(dir1_id).await.unwrap();

        assert_eq!(tree.get_depth(tree.root_id()), 0);
        assert_eq!(tree.get_depth(dir1_id), 1);

        let dir1 = tree.get_node(dir1_id).unwrap();
        let file1_id = dir1.children[0];
        assert_eq!(tree.get_depth(file1_id), 2);
    }

    #[tokio::test]
    async fn test_sorted_entries() {
        let (_temp_dir, mut tree) = create_test_tree().await;

        tree.expand_node(tree.root_id()).await.unwrap();

        let root = tree.get_node(tree.root_id()).unwrap();
        let children: Vec<_> = root
            .children
            .iter()
            .map(|&id| tree.get_node(id).unwrap())
            .collect();

        // Directories should come first
        assert!(children[0].is_dir());
        assert!(children[1].is_dir());
        assert!(children[2].is_file());

        // Directories should be sorted by name
        assert_eq!(children[0].entry.name, "dir1");
        assert_eq!(children[1].entry.name, "dir2");
    }

    #[tokio::test]
    async fn test_refresh_node() {
        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();

        std_fs::create_dir(temp_path.join("dir1")).unwrap();
        std_fs::write(temp_path.join("dir1/file1.txt"), "content").unwrap();

        let backend = Arc::new(LocalFsBackend::new());
        let manager = Arc::new(FsManager::new(backend));
        let mut tree = FileTree::new(temp_path.to_path_buf(), manager)
            .await
            .unwrap();

        // Expand root and dir1
        tree.expand_node(tree.root_id()).await.unwrap();
        let root = tree.get_node(tree.root_id()).unwrap();
        let dir1_id = root.children[0];
        tree.expand_node(dir1_id).await.unwrap();

        // Initially 1 file in dir1
        let dir1 = tree.get_node(dir1_id).unwrap();
        assert_eq!(dir1.children.len(), 1);

        // Add another file
        std_fs::write(temp_path.join("dir1/file2.txt"), "content2").unwrap();

        // Refresh dir1
        tree.refresh_node(dir1_id).await.unwrap();

        // Should now have 2 files
        let dir1 = tree.get_node(dir1_id).unwrap();
        assert_eq!(dir1.children.len(), 2);
    }

    #[tokio::test]
    async fn test_find_by_relative_path() {
        let (_temp_dir, mut tree) = create_test_tree().await;

        tree.expand_node(tree.root_id()).await.unwrap();
        let root = tree.get_node(tree.root_id()).unwrap();
        let dir1_id = root.children[0];

        let found_id = tree.find_by_relative_path(Path::new("dir1"));
        assert_eq!(found_id, Some(dir1_id));

        let not_found = tree.find_by_relative_path(Path::new("nonexistent"));
        assert_eq!(not_found, None);
    }
}
