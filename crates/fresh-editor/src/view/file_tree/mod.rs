// File tree module for lazy-loaded directory hierarchy
//
// This module provides a tree structure for representing filesystem hierarchies
// with lazy loading (directories are only read when expanded) and efficient
// navigation.

mod cache;
pub mod decorations;
pub mod file_icons;
pub mod git_status;
pub mod ignore;
pub mod node;
pub mod search;
pub mod slots;
pub mod tree;
pub mod view;

pub use decorations::{
    decoration_symbol, resolve_explorer_status, ExplorerRowStatus, FileExplorerDecoration,
    FileExplorerDecorationCache, ResolvedExplorerStatus,
};
pub use file_icons::{
    resolve_file_icon, ExplorerIconColorRole, ResolvedFileIcon, DIRECTORY_ICON, GENERIC_FILE_ICON,
};
pub use git_status::{
    parse_porcelain_z, FileExplorerGitStatus, FileExplorerGitStatusCache, GitStatusKind,
};
pub use ignore::{IgnorePatterns, IgnoreStatus};
pub use node::{NodeId, NodeState, TreeNode};
pub use search::FileExplorerSearch;
pub use slots::{
    compatibility_slot_providers, default_slot_providers, ExplorerLeadingSlotPayload,
    ExplorerSlotContext, ExplorerSlotProviders, ExplorerSlotResolution, ExplorerSlotResolver,
    ExplorerTooltipSummary, ExplorerTrailingSlotPayload, ExplorerTrailingSlotResolution,
    FileExplorerSlotOverrideCache, COMPATIBILITY_TRAILING_SLOT_HIT_WIDTH,
    DEFAULT_LEADING_SLOT_MIN_WIDTH,
};
pub use tree::FileTree;
pub use view::{FileTreeView, SortMode};
