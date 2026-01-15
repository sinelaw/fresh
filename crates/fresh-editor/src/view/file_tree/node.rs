use crate::services::fs::FsEntry;
use std::fmt;

/// Unique identifier for a tree node
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeId(pub usize);

impl fmt::Display for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Node({})", self.0)
    }
}

/// Represents a node in the file tree
#[derive(Debug, Clone)]
pub struct TreeNode {
    /// Unique identifier
    pub id: NodeId,
    /// Filesystem entry information
    pub entry: FsEntry,
    /// Parent node ID (None for root)
    pub parent: Option<NodeId>,
    /// Child node IDs (for directories)
    pub children: Vec<NodeId>,
    /// Current state of the node
    pub state: NodeState,
}

impl TreeNode {
    /// Create a new tree node
    pub fn new(id: NodeId, entry: FsEntry, parent: Option<NodeId>) -> Self {
        let state = if entry.is_dir() {
            NodeState::Collapsed
        } else {
            NodeState::Leaf
        };

        Self {
            id,
            entry,
            parent,
            children: Vec::new(),
            state,
        }
    }

    /// Check if this node is a directory
    pub fn is_dir(&self) -> bool {
        self.entry.is_dir()
    }

    /// Check if this node is a file
    pub fn is_file(&self) -> bool {
        self.entry.is_file()
    }

    /// Check if this node is expanded
    pub fn is_expanded(&self) -> bool {
        self.state == NodeState::Expanded
    }

    /// Check if this node is collapsed
    pub fn is_collapsed(&self) -> bool {
        self.state == NodeState::Collapsed
    }

    /// Check if this node is loading
    pub fn is_loading(&self) -> bool {
        self.state == NodeState::Loading
    }

    /// Check if this node has an error
    pub fn is_error(&self) -> bool {
        matches!(self.state, NodeState::Error(_))
    }

    /// Check if this node is a leaf (file, not a directory)
    pub fn is_leaf(&self) -> bool {
        self.state == NodeState::Leaf
    }

    /// Get the depth of this node in the tree
    pub fn depth(&self, get_parent: impl Fn(NodeId) -> Option<NodeId>) -> usize {
        let mut depth = 0;
        let mut current = self.parent;

        while let Some(parent_id) = current {
            depth += 1;
            current = get_parent(parent_id);
        }

        depth
    }
}

/// State of a tree node
#[derive(Debug, Clone, PartialEq)]
pub enum NodeState {
    /// Directory not yet expanded
    Collapsed,
    /// Directory expanded, loading children
    Loading,
    /// Directory expanded, children loaded
    Expanded,
    /// Failed to load (with error message)
    Error(String),
    /// File (leaf node, cannot be expanded)
    Leaf,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::services::fs::{FsEntry, FsEntryType};
    use std::path::PathBuf;

    #[test]
    fn test_node_creation() {
        let entry = FsEntry::new(
            PathBuf::from("/test/file.txt"),
            "file.txt".to_string(),
            FsEntryType::File,
        );

        let node = TreeNode::new(NodeId(0), entry, None);

        assert_eq!(node.id, NodeId(0));
        assert_eq!(node.parent, None);
        assert!(node.is_file());
        assert!(node.is_leaf());
        assert_eq!(node.children.len(), 0);
    }

    #[test]
    fn test_directory_node() {
        let entry = FsEntry::new(
            PathBuf::from("/test/dir"),
            "dir".to_string(),
            FsEntryType::Directory,
        );

        let node = TreeNode::new(NodeId(1), entry, Some(NodeId(0)));

        assert!(node.is_dir());
        assert!(node.is_collapsed());
        assert!(!node.is_expanded());
        assert_eq!(node.parent, Some(NodeId(0)));
    }

    #[test]
    fn test_node_states() {
        let entry = FsEntry::new(
            PathBuf::from("/test/dir"),
            "dir".to_string(),
            FsEntryType::Directory,
        );

        let mut node = TreeNode::new(NodeId(0), entry, None);

        assert!(node.is_collapsed());
        assert!(!node.is_loading());
        assert!(!node.is_error());

        node.state = NodeState::Loading;
        assert!(node.is_loading());
        assert!(!node.is_collapsed());

        node.state = NodeState::Expanded;
        assert!(node.is_expanded());
        assert!(!node.is_loading());

        node.state = NodeState::Error("Failed to read".to_string());
        assert!(node.is_error());
        assert!(!node.is_expanded());
    }

    #[test]
    fn test_node_depth() {
        // Create a simple tree structure
        let root = TreeNode::new(
            NodeId(0),
            FsEntry::new(PathBuf::from("/"), "/".to_string(), FsEntryType::Directory),
            None,
        );

        let child1 = TreeNode::new(
            NodeId(1),
            FsEntry::new(
                PathBuf::from("/dir1"),
                "dir1".to_string(),
                FsEntryType::Directory,
            ),
            Some(NodeId(0)),
        );

        let child2 = TreeNode::new(
            NodeId(2),
            FsEntry::new(
                PathBuf::from("/dir1/dir2"),
                "dir2".to_string(),
                FsEntryType::Directory,
            ),
            Some(NodeId(1)),
        );

        // Helper function to get parent
        let get_parent = |id: NodeId| match id.0 {
            0 => None,
            1 => Some(NodeId(0)),
            2 => Some(NodeId(1)),
            _ => None,
        };

        assert_eq!(root.depth(get_parent), 0);
        assert_eq!(child1.depth(get_parent), 1);
        assert_eq!(child2.depth(get_parent), 2);
    }
}
