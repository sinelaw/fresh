// File tree module for lazy-loaded directory hierarchy
//
// This module provides a tree structure for representing filesystem hierarchies
// with lazy loading (directories are only read when expanded) and efficient
// navigation.

pub mod decorations;
pub mod ignore;
pub mod node;
pub mod tree;
pub mod view;

pub use decorations::{FileExplorerDecoration, FileExplorerDecorationCache};
pub use ignore::{IgnorePatterns, IgnoreStatus};
pub use node::{NodeId, NodeState, TreeNode};
pub use tree::FileTree;
pub use view::{FileTreeView, SortMode};
