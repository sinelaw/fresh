use crate::model::event::{BufferId, LeafId, SplitDirection};
use std::collections::HashMap;

/// Unique identifier for a buffer group
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BufferGroupId(pub usize);

/// Layout node for a buffer group
#[derive(Debug, Clone)]
pub enum GroupLayoutNode {
    /// A scrollable panel backed by a real buffer
    Scrollable {
        /// Panel name (e.g., "tree", "picker")
        id: String,
        /// Buffer ID for this panel (set during creation)
        buffer_id: Option<BufferId>,
        /// Split leaf ID (set during creation)
        split_id: Option<LeafId>,
    },
    /// A fixed-height panel (header, footer, toolbar)
    Fixed {
        /// Panel name
        id: String,
        /// Height in rows
        height: u16,
        /// Buffer ID (set during creation)
        buffer_id: Option<BufferId>,
        /// Split leaf ID (set during creation)
        split_id: Option<LeafId>,
    },
    /// A horizontal or vertical split containing two children
    Split {
        direction: SplitDirection,
        /// Ratio for the first child (0.0 to 1.0)
        ratio: f32,
        first: Box<GroupLayoutNode>,
        second: Box<GroupLayoutNode>,
    },
}

/// A buffer group: multiple splits/buffers appearing as one tab.
///
/// Each panel is a real buffer with its own viewport, scrollbar,
/// and cursor. The group presents them as a single logical entity
/// in the tab bar and buffer list.
#[derive(Debug)]
pub struct BufferGroup {
    /// Unique ID
    pub id: BufferGroupId,
    /// Display name (shown in tab bar)
    pub name: String,
    /// Mode for keybindings
    pub mode: String,
    /// Layout tree
    pub layout: GroupLayoutNode,
    /// All buffer IDs in this group (panel name → buffer ID)
    pub panel_buffers: HashMap<String, BufferId>,
    /// All split leaf IDs in this group
    pub panel_splits: HashMap<String, LeafId>,
    /// The "representative" split that owns the tab entry.
    /// This is typically the first scrollable panel.
    pub representative_split: Option<LeafId>,
}
