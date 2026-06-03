use crate::model::event::{BufferId, LeafId};

/// Drop zone for tab drag-and-drop
/// Indicates where a dragged tab will be placed when released
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TabDropZone {
    /// Drop into an existing split's tab bar (before tab at index, or at end if None)
    /// (target_split_id, insert_index)
    TabBar(LeafId, Option<usize>),
    /// Create a new split on the left edge of the target split
    SplitLeft(LeafId),
    /// Create a new split on the right edge of the target split
    SplitRight(LeafId),
    /// Create a new split on the top edge of the target split
    SplitTop(LeafId),
    /// Create a new split on the bottom edge of the target split
    SplitBottom(LeafId),
    /// Drop into the center of a split (switch to that split's tab bar)
    SplitCenter(LeafId),
}

impl TabDropZone {
    /// Get the split ID this drop zone is associated with
    pub fn split_id(&self) -> LeafId {
        match self {
            Self::TabBar(id, _)
            | Self::SplitLeft(id)
            | Self::SplitRight(id)
            | Self::SplitTop(id)
            | Self::SplitBottom(id)
            | Self::SplitCenter(id) => *id,
        }
    }
}

/// State for a tab being dragged
#[derive(Debug, Clone)]
pub struct TabDragState {
    /// The buffer being dragged
    pub buffer_id: BufferId,
    /// The split the tab was dragged from
    pub source_split_id: LeafId,
    /// Starting mouse position when drag began
    pub start_position: (u16, u16),
    /// Current mouse position
    pub current_position: (u16, u16),
    /// Currently detected drop zone (if any)
    pub drop_zone: Option<TabDropZone>,
}

impl TabDragState {
    /// Create a new tab drag state
    pub fn new(buffer_id: BufferId, source_split_id: LeafId, start_position: (u16, u16)) -> Self {
        Self {
            buffer_id,
            source_split_id,
            start_position,
            current_position: start_position,
            drop_zone: None,
        }
    }

    /// Check if the drag has moved enough to be considered a real drag (not just a click)
    pub fn is_dragging(&self) -> bool {
        let dx = (self.current_position.0 as i32 - self.start_position.0 as i32).abs();
        let dy = (self.current_position.1 as i32 - self.start_position.1 as i32).abs();
        dx > 3 || dy > 3 // Threshold of 3 pixels before drag activates
    }
}
