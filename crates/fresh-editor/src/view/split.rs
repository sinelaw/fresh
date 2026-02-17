/// Split view system for displaying multiple buffers simultaneously
///
/// Design Philosophy (following Emacs model):
/// - A split is a tree structure: either a leaf (single buffer) or a node (horizontal/vertical split)
/// - Each split has a fixed size (in percentage or absolute lines/columns)
/// - Splits can be nested arbitrarily deep
/// - Only one split is "active" at a time (receives input)
/// - Splits can display the same buffer multiple times (useful for viewing different parts)
///
/// Example split layouts:
/// ```text
/// ┌────────────────────┐      ┌──────────┬─────────┐
/// │                    │      │          │         │
/// │   Single buffer    │      │  Buffer  │ Buffer  │
/// │                    │      │    A     │    B    │
/// └────────────────────┘      └──────────┴─────────┘
///   (no split)                  (vertical split)
///
/// ┌────────────────────┐      ┌──────────┬─────────┐
/// │     Buffer A       │      │          │ Buffer C│
/// ├────────────────────┤      │  Buffer  ├─────────┤
/// │     Buffer B       │      │    A     │ Buffer D│
/// └────────────────────┘      └──────────┴─────────┘
///  (horizontal split)          (mixed splits)
/// ```
use crate::model::cursor::Cursors;
use crate::model::event::{BufferId, SplitDirection, SplitId};
use crate::view::ui::view_pipeline::Layout;
use crate::view::viewport::Viewport;
use crate::{services::plugins::api::ViewTransformPayload, state::ViewMode};
use ratatui::layout::Rect;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// A node in the split tree
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum SplitNode {
    /// Leaf node: displays a single buffer
    Leaf {
        /// Which buffer to display
        buffer_id: BufferId,
        /// Unique ID for this split pane
        split_id: SplitId,
    },
    /// Internal node: contains two child splits
    Split {
        /// Direction of the split
        direction: SplitDirection,
        /// First child (top or left)
        first: Box<Self>,
        /// Second child (bottom or right)
        second: Box<Self>,
        /// Size ratio (0.0 to 1.0) - how much space the first child gets
        /// 0.5 = equal split, 0.3 = first gets 30%, etc.
        ratio: f32,
        /// Unique ID for this split container
        split_id: SplitId,
    },
}

/// Per-buffer view state within a split.
///
/// Each buffer opened in a split gets its own `BufferViewState` stored in the
/// split's `keyed_states` map. This ensures that switching buffers within a split
/// preserves cursor position, scroll state, view mode, and compose settings
/// independently for each buffer.
#[derive(Debug, Clone)]
pub struct BufferViewState {
    /// Independent cursor set (supports multi-cursor)
    pub cursors: Cursors,

    /// Independent scroll position
    pub viewport: Viewport,

    /// View mode (Source/Compose) for this buffer in this split
    pub view_mode: ViewMode,

    /// Optional compose width for centering/wrapping
    pub compose_width: Option<u16>,

    /// Column guides (e.g., tables)
    pub compose_column_guides: Option<Vec<u16>>,

    /// Vertical ruler positions (initialized from config, mutable per-buffer)
    pub rulers: Vec<usize>,

    /// Previously configured line number visibility (restored when leaving Compose)
    pub compose_prev_line_numbers: Option<bool>,

    /// Per-split line number visibility.
    /// This is the single source of truth for whether line numbers are shown
    /// in this split. Initialized from config when the split is created.
    pub show_line_numbers: bool,

    /// Optional view transform payload
    pub view_transform: Option<ViewTransformPayload>,

    /// True when the buffer was edited since the last view_transform_request hook fired.
    /// While true, incoming SubmitViewTransform commands are rejected as stale
    /// (their tokens have source_offsets from before the edit).
    pub view_transform_stale: bool,

    /// Plugin-managed state (arbitrary key-value pairs).
    /// Plugins can store per-buffer-per-split state here via the `setViewState`/`getViewState` API.
    /// Persisted across sessions via workspace serialization.
    pub plugin_state: std::collections::HashMap<String, serde_json::Value>,
}

impl BufferViewState {
    /// Create a new buffer view state with defaults
    pub fn new(width: u16, height: u16) -> Self {
        Self {
            cursors: Cursors::new(),
            viewport: Viewport::new(width, height),
            view_mode: ViewMode::Source,
            compose_width: None,
            compose_column_guides: None,
            rulers: Vec::new(),
            compose_prev_line_numbers: None,
            show_line_numbers: true,
            view_transform: None,
            view_transform_stale: false,
            plugin_state: std::collections::HashMap::new(),
        }
    }
}

/// Per-split view state (independent of buffer content)
///
/// Following the Emacs model where each window (split) has its own:
/// - Point (cursor position) - independent per split
/// - Window-start (scroll position) - independent per split
/// - Tabs (open buffers) - independent per split
///
/// Buffer-specific state (cursors, viewport, view_mode, compose settings) is stored
/// in the `keyed_states` map, keyed by `BufferId`. The active buffer's state is
/// accessible via `Deref`/`DerefMut` (so `vs.cursors` transparently accesses the
/// active buffer's cursors), or explicitly via `active_state()`/`active_state_mut()`.
#[derive(Debug, Clone)]
pub struct SplitViewState {
    /// Which buffer is currently active in this split
    pub active_buffer: BufferId,

    /// Per-buffer view state map. The active buffer always has an entry.
    pub keyed_states: HashMap<BufferId, BufferViewState>,

    /// List of buffer IDs open in this split's tab bar (in order)
    /// The currently displayed buffer is tracked in the SplitNode::Leaf
    pub open_buffers: Vec<BufferId>,

    /// Horizontal scroll offset for the tabs in this split
    pub tab_scroll_offset: usize,

    /// Computed layout for this view (from view_transform or base tokens)
    /// This is View state - each split has its own Layout
    pub layout: Option<Layout>,

    /// Whether the layout needs to be rebuilt (buffer changed, transform changed, etc.)
    pub layout_dirty: bool,

    /// Focus history stack for this split (most recent at end)
    /// Used for "Switch to Previous Tab" and for returning to previous buffer when closing
    pub focus_history: Vec<BufferId>,

    /// Sync group ID for synchronized scrolling
    /// Splits with the same sync_group will scroll together
    pub sync_group: Option<u32>,

    /// When set, this split renders a composite view (e.g., side-by-side diff).
    /// The split's buffer_id is the focused source buffer, but rendering uses
    /// the composite layout. This makes the source buffer the "active buffer"
    /// so normal keybindings work directly.
    pub composite_view: Option<BufferId>,
}

impl std::ops::Deref for SplitViewState {
    type Target = BufferViewState;

    fn deref(&self) -> &BufferViewState {
        self.active_state()
    }
}

impl std::ops::DerefMut for SplitViewState {
    fn deref_mut(&mut self) -> &mut BufferViewState {
        self.active_state_mut()
    }
}

impl SplitViewState {
    /// Create a new split view state with an initial buffer open
    pub fn with_buffer(width: u16, height: u16, buffer_id: BufferId) -> Self {
        let buf_state = BufferViewState::new(width, height);
        let mut keyed_states = HashMap::new();
        keyed_states.insert(buffer_id, buf_state);
        Self {
            active_buffer: buffer_id,
            keyed_states,
            open_buffers: vec![buffer_id],
            tab_scroll_offset: 0,
            layout: None,
            layout_dirty: true,
            focus_history: Vec::new(),
            sync_group: None,
            composite_view: None,
        }
    }

    /// Get the active buffer's view state
    pub fn active_state(&self) -> &BufferViewState {
        self.keyed_states
            .get(&self.active_buffer)
            .expect("active_buffer must always have an entry in keyed_states")
    }

    /// Get a mutable reference to the active buffer's view state
    pub fn active_state_mut(&mut self) -> &mut BufferViewState {
        self.keyed_states
            .get_mut(&self.active_buffer)
            .expect("active_buffer must always have an entry in keyed_states")
    }

    /// Switch the active buffer in this split.
    ///
    /// If the new buffer has a saved state in `keyed_states`, it is restored.
    /// Otherwise a default `BufferViewState` is created with the split's current
    /// viewport dimensions.
    pub fn switch_buffer(&mut self, new_buffer_id: BufferId) {
        if new_buffer_id == self.active_buffer {
            return;
        }
        // Ensure the new buffer has keyed state (create default if first time)
        if !self.keyed_states.contains_key(&new_buffer_id) {
            let active = self.active_state();
            let width = active.viewport.width;
            let height = active.viewport.height;
            self.keyed_states
                .insert(new_buffer_id, BufferViewState::new(width, height));
        }
        self.active_buffer = new_buffer_id;
        // Invalidate layout since we're now showing different buffer content
        self.layout_dirty = true;
    }

    /// Get the view state for a specific buffer (if it exists)
    pub fn buffer_state(&self, buffer_id: BufferId) -> Option<&BufferViewState> {
        self.keyed_states.get(&buffer_id)
    }

    /// Get a mutable reference to the view state for a specific buffer (if it exists)
    pub fn buffer_state_mut(&mut self, buffer_id: BufferId) -> Option<&mut BufferViewState> {
        self.keyed_states.get_mut(&buffer_id)
    }

    /// Ensure a buffer has keyed state, creating a default if needed.
    /// Returns a mutable reference to the buffer's view state.
    pub fn ensure_buffer_state(&mut self, buffer_id: BufferId) -> &mut BufferViewState {
        let (width, height) = {
            let active = self.active_state();
            (active.viewport.width, active.viewport.height)
        };
        self.keyed_states
            .entry(buffer_id)
            .or_insert_with(|| BufferViewState::new(width, height))
    }

    /// Remove keyed state for a buffer (when buffer is closed from this split)
    pub fn remove_buffer_state(&mut self, buffer_id: BufferId) {
        if buffer_id != self.active_buffer {
            self.keyed_states.remove(&buffer_id);
        }
    }

    /// Mark layout as needing rebuild (call after buffer changes)
    pub fn invalidate_layout(&mut self) {
        self.layout_dirty = true;
    }

    /// Ensure layout is valid, rebuilding if needed.
    /// Returns the Layout - never returns None. Following VSCode's ViewModel pattern.
    ///
    /// # Arguments
    /// * `tokens` - ViewTokenWire array (from view_transform or built from buffer)
    /// * `source_range` - The byte range this layout covers
    /// * `tab_size` - Tab width for rendering
    pub fn ensure_layout(
        &mut self,
        tokens: &[fresh_core::api::ViewTokenWire],
        source_range: std::ops::Range<usize>,
        tab_size: usize,
    ) -> &Layout {
        if self.layout.is_none() || self.layout_dirty {
            self.layout = Some(Layout::from_tokens(tokens, source_range, tab_size));
            self.layout_dirty = false;
        }
        self.layout.as_ref().unwrap()
    }

    /// Get the current layout if it exists and is valid
    pub fn get_layout(&self) -> Option<&Layout> {
        if self.layout_dirty {
            None
        } else {
            self.layout.as_ref()
        }
    }

    /// Add a buffer to this split's tabs (if not already present)
    pub fn add_buffer(&mut self, buffer_id: BufferId) {
        if !self.open_buffers.contains(&buffer_id) {
            self.open_buffers.push(buffer_id);
        }
    }

    /// Remove a buffer from this split's tabs and clean up its keyed state
    pub fn remove_buffer(&mut self, buffer_id: BufferId) {
        self.open_buffers.retain(|&id| id != buffer_id);
        // Clean up keyed state (but never remove the active buffer's state)
        if buffer_id != self.active_buffer {
            self.keyed_states.remove(&buffer_id);
        }
    }

    /// Check if a buffer is open in this split
    pub fn has_buffer(&self, buffer_id: BufferId) -> bool {
        self.open_buffers.contains(&buffer_id)
    }

    /// Push a buffer to the focus history (LRU-style)
    /// If the buffer is already in history, it's moved to the end
    pub fn push_focus(&mut self, buffer_id: BufferId) {
        // Remove if already in history (LRU-style)
        self.focus_history.retain(|&id| id != buffer_id);
        self.focus_history.push(buffer_id);
        // Limit to 50 entries
        if self.focus_history.len() > 50 {
            self.focus_history.remove(0);
        }
    }

    /// Get the most recently focused buffer (without removing it)
    pub fn previous_buffer(&self) -> Option<BufferId> {
        self.focus_history.last().copied()
    }

    /// Pop the most recent buffer from focus history
    pub fn pop_focus(&mut self) -> Option<BufferId> {
        self.focus_history.pop()
    }

    /// Remove a buffer from the focus history (called when buffer is closed)
    pub fn remove_from_history(&mut self, buffer_id: BufferId) {
        self.focus_history.retain(|&id| id != buffer_id);
    }
}

impl SplitNode {
    /// Create a new leaf node
    pub fn leaf(buffer_id: BufferId, split_id: SplitId) -> Self {
        Self::Leaf {
            buffer_id,
            split_id,
        }
    }

    /// Create a new split node with two children
    pub fn split(
        direction: SplitDirection,
        first: SplitNode,
        second: SplitNode,
        ratio: f32,
        split_id: SplitId,
    ) -> Self {
        SplitNode::Split {
            direction,
            first: Box::new(first),
            second: Box::new(second),
            ratio: ratio.clamp(0.1, 0.9), // Prevent extreme ratios
            split_id,
        }
    }

    /// Get the split ID for this node
    pub fn id(&self) -> SplitId {
        match self {
            Self::Leaf { split_id, .. } | Self::Split { split_id, .. } => *split_id,
        }
    }

    /// Get the buffer ID if this is a leaf node
    pub fn buffer_id(&self) -> Option<BufferId> {
        match self {
            Self::Leaf { buffer_id, .. } => Some(*buffer_id),
            Self::Split { .. } => None,
        }
    }

    /// Find a split by ID (returns mutable reference)
    pub fn find_mut(&mut self, target_id: SplitId) -> Option<&mut Self> {
        if self.id() == target_id {
            return Some(self);
        }

        match self {
            Self::Leaf { .. } => None,
            Self::Split { first, second, .. } => first
                .find_mut(target_id)
                .or_else(|| second.find_mut(target_id)),
        }
    }

    /// Find a split by ID (returns immutable reference)
    pub fn find(&self, target_id: SplitId) -> Option<&Self> {
        if self.id() == target_id {
            return Some(self);
        }

        match self {
            Self::Leaf { .. } => None,
            Self::Split { first, second, .. } => {
                first.find(target_id).or_else(|| second.find(target_id))
            }
        }
    }

    /// Get all leaf nodes (buffer views) with their rectangles
    pub fn get_leaves_with_rects(&self, rect: Rect) -> Vec<(SplitId, BufferId, Rect)> {
        match self {
            Self::Leaf {
                buffer_id,
                split_id,
            } => {
                vec![(*split_id, *buffer_id, rect)]
            }
            Self::Split {
                direction,
                first,
                second,
                ratio,
                ..
            } => {
                let (first_rect, second_rect) = split_rect(rect, *direction, *ratio);
                let mut leaves = first.get_leaves_with_rects(first_rect);
                leaves.extend(second.get_leaves_with_rects(second_rect));
                leaves
            }
        }
    }

    /// Get all split separator lines (for rendering borders)
    /// Returns (direction, x, y, length) tuples
    pub fn get_separators(&self, rect: Rect) -> Vec<(SplitDirection, u16, u16, u16)> {
        self.get_separators_with_ids(rect)
            .into_iter()
            .map(|(_, dir, x, y, len)| (dir, x, y, len))
            .collect()
    }

    /// Get all split separator lines with their split IDs (for mouse hit testing)
    /// Returns (split_id, direction, x, y, length) tuples
    pub fn get_separators_with_ids(
        &self,
        rect: Rect,
    ) -> Vec<(SplitId, SplitDirection, u16, u16, u16)> {
        match self {
            Self::Leaf { .. } => vec![],
            Self::Split {
                direction,
                first,
                second,
                ratio,
                split_id,
            } => {
                let (first_rect, second_rect) = split_rect(rect, *direction, *ratio);
                let mut separators = Vec::new();

                // Add separator for this split (in the 1-char gap between first and second)
                match direction {
                    SplitDirection::Horizontal => {
                        // Horizontal split: separator line is between first and second
                        // y position is at the end of first rect (the gap line)
                        separators.push((
                            *split_id,
                            SplitDirection::Horizontal,
                            rect.x,
                            first_rect.y + first_rect.height,
                            rect.width,
                        ));
                    }
                    SplitDirection::Vertical => {
                        // Vertical split: separator line is between first and second
                        // x position is at the end of first rect (the gap column)
                        separators.push((
                            *split_id,
                            SplitDirection::Vertical,
                            first_rect.x + first_rect.width,
                            rect.y,
                            rect.height,
                        ));
                    }
                }

                // Recursively get separators from children
                separators.extend(first.get_separators_with_ids(first_rect));
                separators.extend(second.get_separators_with_ids(second_rect));
                separators
            }
        }
    }

    /// Collect all split IDs in the tree
    pub fn all_split_ids(&self) -> Vec<SplitId> {
        let mut ids = vec![self.id()];
        match self {
            Self::Leaf { .. } => ids,
            Self::Split { first, second, .. } => {
                ids.extend(first.all_split_ids());
                ids.extend(second.all_split_ids());
                ids
            }
        }
    }

    /// Collect only leaf split IDs (visible buffer splits, not container nodes)
    pub fn leaf_split_ids(&self) -> Vec<SplitId> {
        match self {
            Self::Leaf { split_id, .. } => vec![*split_id],
            Self::Split { first, second, .. } => {
                let mut ids = first.leaf_split_ids();
                ids.extend(second.leaf_split_ids());
                ids
            }
        }
    }

    /// Count the number of leaf nodes (visible buffers)
    pub fn count_leaves(&self) -> usize {
        match self {
            Self::Leaf { .. } => 1,
            Self::Split { first, second, .. } => first.count_leaves() + second.count_leaves(),
        }
    }
}

/// Split a rectangle into two parts based on direction and ratio
/// Leaves 1 character space for the separator line between splits
fn split_rect(rect: Rect, direction: SplitDirection, ratio: f32) -> (Rect, Rect) {
    match direction {
        SplitDirection::Horizontal => {
            // Split into top and bottom, with 1 line for separator
            let total_height = rect.height.saturating_sub(1); // Reserve 1 line for separator
            let first_height = (total_height as f32 * ratio).round() as u16;
            let second_height = total_height.saturating_sub(first_height);

            let first = Rect {
                x: rect.x,
                y: rect.y,
                width: rect.width,
                height: first_height,
            };

            let second = Rect {
                x: rect.x,
                y: rect.y + first_height + 1, // +1 for separator
                width: rect.width,
                height: second_height,
            };

            (first, second)
        }
        SplitDirection::Vertical => {
            // Split into left and right, with 1 column for separator
            let total_width = rect.width.saturating_sub(1); // Reserve 1 column for separator
            let first_width = (total_width as f32 * ratio).round() as u16;
            let second_width = total_width.saturating_sub(first_width);

            let first = Rect {
                x: rect.x,
                y: rect.y,
                width: first_width,
                height: rect.height,
            };

            let second = Rect {
                x: rect.x + first_width + 1, // +1 for separator
                y: rect.y,
                width: second_width,
                height: rect.height,
            };

            (first, second)
        }
    }
}

/// Manager for the split view system
#[derive(Debug)]
pub struct SplitManager {
    /// Root of the split tree
    root: SplitNode,

    /// Currently active split (receives input)
    active_split: SplitId,

    /// Next split ID to assign
    next_split_id: usize,

    /// Currently maximized split (if any). When set, only this split is visible.
    maximized_split: Option<SplitId>,

    /// Labels for leaf splits (e.g., "sidebar" to mark managed splits)
    labels: HashMap<SplitId, String>,
}

impl SplitManager {
    /// Create a new split manager with a single buffer
    pub fn new(buffer_id: BufferId) -> Self {
        let split_id = SplitId(0);
        Self {
            root: SplitNode::leaf(buffer_id, split_id),
            active_split: split_id,
            next_split_id: 1,
            maximized_split: None,
            labels: HashMap::new(),
        }
    }

    /// Get the root split node
    pub fn root(&self) -> &SplitNode {
        &self.root
    }

    /// Get the currently active split ID
    pub fn active_split(&self) -> SplitId {
        self.active_split
    }

    /// Set the active split
    pub fn set_active_split(&mut self, split_id: SplitId) -> bool {
        // Verify the split exists
        if self.root.find(split_id).is_some() {
            self.active_split = split_id;
            true
        } else {
            false
        }
    }

    /// Get the buffer ID of the active split (if it's a leaf)
    pub fn active_buffer_id(&self) -> Option<BufferId> {
        self.root
            .find(self.active_split)
            .and_then(|node| node.buffer_id())
    }

    /// Get the buffer ID for a specific split (if it's a leaf)
    pub fn get_buffer_id(&self, split_id: SplitId) -> Option<BufferId> {
        self.root.find(split_id).and_then(|node| node.buffer_id())
    }

    /// Update the buffer ID of the active split
    /// Returns true if successful (active split is a leaf), false otherwise
    pub fn set_active_buffer_id(&mut self, new_buffer_id: BufferId) -> bool {
        if let Some(SplitNode::Leaf { buffer_id, .. }) = self.root.find_mut(self.active_split) {
            *buffer_id = new_buffer_id;
            return true;
        }
        false
    }

    /// Update the buffer ID of a specific split
    /// Returns Ok(()) if successful, Err with message if split not found or not a leaf
    pub fn set_split_buffer(
        &mut self,
        split_id: SplitId,
        new_buffer_id: BufferId,
    ) -> Result<(), String> {
        if let Some(node) = self.root.find_mut(split_id) {
            if let SplitNode::Leaf { buffer_id, .. } = node {
                *buffer_id = new_buffer_id;
                return Ok(());
            }
            return Err(format!("Split {:?} is not a leaf", split_id));
        }
        Err(format!("Split {:?} not found", split_id))
    }

    /// Allocate a new split ID
    fn allocate_split_id(&mut self) -> SplitId {
        let id = SplitId(self.next_split_id);
        self.next_split_id += 1;
        id
    }

    /// Split the currently active pane
    pub fn split_active(
        &mut self,
        direction: SplitDirection,
        new_buffer_id: BufferId,
        ratio: f32,
    ) -> Result<SplitId, String> {
        self.split_active_positioned(direction, new_buffer_id, ratio, false)
    }

    /// Split the active pane, placing the new buffer before (left/top) the existing content.
    /// `ratio` still controls the first child's proportion of space.
    pub fn split_active_before(
        &mut self,
        direction: SplitDirection,
        new_buffer_id: BufferId,
        ratio: f32,
    ) -> Result<SplitId, String> {
        self.split_active_positioned(direction, new_buffer_id, ratio, true)
    }

    pub fn split_active_positioned(
        &mut self,
        direction: SplitDirection,
        new_buffer_id: BufferId,
        ratio: f32,
        before: bool,
    ) -> Result<SplitId, String> {
        let active_id = self.active_split;

        // Find the parent of the active split
        let result =
            self.replace_split_with_split(active_id, direction, new_buffer_id, ratio, before);

        if let Ok(new_split_id) = result {
            // Set the new split as active
            self.active_split = new_split_id;
            Ok(new_split_id)
        } else {
            result
        }
    }

    /// Replace a split with a new split container.
    /// When `before` is true, the new buffer is placed as the first child (left/top).
    fn replace_split_with_split(
        &mut self,
        target_id: SplitId,
        direction: SplitDirection,
        new_buffer_id: BufferId,
        ratio: f32,
        before: bool,
    ) -> Result<SplitId, String> {
        // Pre-allocate all IDs before any borrowing
        let temp_id = self.allocate_split_id();
        let new_split_id = self.allocate_split_id();
        let new_leaf_id = self.allocate_split_id();

        // Special case: if target is root, replace root
        if self.root.id() == target_id {
            let old_root =
                std::mem::replace(&mut self.root, SplitNode::leaf(new_buffer_id, temp_id));
            let new_leaf = SplitNode::leaf(new_buffer_id, new_leaf_id);

            let (first, second) = if before {
                (new_leaf, old_root)
            } else {
                (old_root, new_leaf)
            };

            self.root = SplitNode::split(direction, first, second, ratio, new_split_id);

            return Ok(new_leaf_id);
        }

        // Find and replace the target node
        if let Some(node) = self.root.find_mut(target_id) {
            let old_node = std::mem::replace(node, SplitNode::leaf(new_buffer_id, temp_id));
            let new_leaf = SplitNode::leaf(new_buffer_id, new_leaf_id);

            let (first, second) = if before {
                (new_leaf, old_node)
            } else {
                (old_node, new_leaf)
            };

            *node = SplitNode::split(direction, first, second, ratio, new_split_id);

            Ok(new_leaf_id)
        } else {
            Err(format!("Split {:?} not found", target_id))
        }
    }

    /// Close a split pane (if not the last one)
    pub fn close_split(&mut self, split_id: SplitId) -> Result<(), String> {
        // Can't close if it's the only split
        if self.root.count_leaves() <= 1 {
            return Err("Cannot close the last split".to_string());
        }

        // Can't close if it's the root and root is a leaf
        if self.root.id() == split_id && self.root.buffer_id().is_some() {
            return Err("Cannot close the only split".to_string());
        }

        // If the split being closed is maximized, unmaximize first
        if self.maximized_split == Some(split_id) {
            self.maximized_split = None;
        }

        // Collect all split IDs that will be removed (the target and its children)
        let removed_ids: Vec<SplitId> = self
            .root
            .find(split_id)
            .map(|node| node.all_split_ids())
            .unwrap_or_default();

        // Find the parent of the split to close
        // This requires a parent-tracking traversal
        let result = self.remove_split_node(split_id);

        if result.is_ok() {
            // Clean up labels for all removed splits
            for id in &removed_ids {
                self.labels.remove(id);
            }

            // If we closed the active split, update active_split to another split
            if self.active_split == split_id {
                let leaf_ids = self.root.leaf_split_ids();
                if let Some(&first_leaf) = leaf_ids.first() {
                    self.active_split = first_leaf;
                }
            }
        }

        result
    }

    /// Remove a split node from the tree
    fn remove_split_node(&mut self, target_id: SplitId) -> Result<(), String> {
        // Special case: removing root
        if self.root.id() == target_id {
            if let SplitNode::Split { first, .. } = &self.root {
                // Replace root with the other child
                // Choose first child arbitrarily
                self.root = (**first).clone();
                return Ok(());
            }
        }

        // Recursively find and remove
        Self::remove_child_static(&mut self.root, target_id)
    }

    /// Helper to remove a child from a split node (static to avoid borrow issues)
    fn remove_child_static(node: &mut SplitNode, target_id: SplitId) -> Result<(), String> {
        match node {
            SplitNode::Leaf { .. } => Err("Target not found".to_string()),
            SplitNode::Split { first, second, .. } => {
                // Check if either child is the target
                if first.id() == target_id {
                    // Replace this node with the second child
                    *node = (**second).clone();
                    Ok(())
                } else if second.id() == target_id {
                    // Replace this node with the first child
                    *node = (**first).clone();
                    Ok(())
                } else {
                    // Recurse into children
                    Self::remove_child_static(first, target_id)
                        .or_else(|_| Self::remove_child_static(second, target_id))
                }
            }
        }
    }

    /// Adjust the split ratio of a container
    pub fn adjust_ratio(&mut self, split_id: SplitId, delta: f32) -> Result<(), String> {
        if let Some(node) = self.root.find_mut(split_id) {
            if let SplitNode::Split { ratio, .. } = node {
                *ratio = (*ratio + delta).clamp(0.1, 0.9);
                Ok(())
            } else {
                Err("Target is not a split container".to_string())
            }
        } else {
            Err("Split not found".to_string())
        }
    }

    /// Get all visible buffer views with their rectangles
    pub fn get_visible_buffers(&self, viewport_rect: Rect) -> Vec<(SplitId, BufferId, Rect)> {
        // If a split is maximized, only show that split taking up the full viewport
        if let Some(maximized_id) = self.maximized_split {
            if let Some(node) = self.root.find(maximized_id) {
                if let Some(buffer_id) = node.buffer_id() {
                    return vec![(maximized_id, buffer_id, viewport_rect)];
                }
            }
            // Maximized split no longer exists, clear it and fall through
        }
        self.root.get_leaves_with_rects(viewport_rect)
    }

    /// Get all split separator positions for rendering borders
    /// Returns (direction, x, y, length) tuples
    pub fn get_separators(&self, viewport_rect: Rect) -> Vec<(SplitDirection, u16, u16, u16)> {
        // No separators when a split is maximized
        if self.maximized_split.is_some() {
            return vec![];
        }
        self.root.get_separators(viewport_rect)
    }

    /// Get all split separator positions with their split IDs (for mouse hit testing)
    /// Returns (split_id, direction, x, y, length) tuples
    pub fn get_separators_with_ids(
        &self,
        viewport_rect: Rect,
    ) -> Vec<(SplitId, SplitDirection, u16, u16, u16)> {
        // No separators when a split is maximized
        if self.maximized_split.is_some() {
            return vec![];
        }
        self.root.get_separators_with_ids(viewport_rect)
    }

    /// Get the current ratio of a split container
    pub fn get_ratio(&self, split_id: SplitId) -> Option<f32> {
        if let Some(SplitNode::Split { ratio, .. }) = self.root.find(split_id) {
            Some(*ratio)
        } else {
            None
        }
    }

    /// Set the exact ratio of a split container
    pub fn set_ratio(&mut self, split_id: SplitId, new_ratio: f32) -> Result<(), String> {
        if let Some(node) = self.root.find_mut(split_id) {
            if let SplitNode::Split { ratio, .. } = node {
                *ratio = new_ratio.clamp(0.1, 0.9);
                Ok(())
            } else {
                Err("Target is not a split container".to_string())
            }
        } else {
            Err("Split not found".to_string())
        }
    }

    /// Distribute all visible splits evenly
    /// This sets the ratios of all container splits so that leaf splits get equal space
    pub fn distribute_splits_evenly(&mut self) {
        Self::distribute_node_evenly(&mut self.root);
    }

    /// Recursively distribute a node's splits evenly
    /// Returns the number of leaves in this subtree
    fn distribute_node_evenly(node: &mut SplitNode) -> usize {
        match node {
            SplitNode::Leaf { .. } => 1,
            SplitNode::Split {
                first,
                second,
                ratio,
                ..
            } => {
                let first_leaves = Self::distribute_node_evenly(first);
                let second_leaves = Self::distribute_node_evenly(second);
                let total_leaves = first_leaves + second_leaves;

                // Set ratio so each leaf gets equal space
                // ratio = proportion for first pane
                *ratio = (first_leaves as f32 / total_leaves as f32).clamp(0.1, 0.9);

                total_leaves
            }
        }
    }

    /// Navigate to the next split (circular)
    pub fn next_split(&mut self) {
        let leaf_ids = self.root.leaf_split_ids();
        if let Some(pos) = leaf_ids.iter().position(|id| *id == self.active_split) {
            let next_pos = (pos + 1) % leaf_ids.len();
            self.active_split = leaf_ids[next_pos];
        }
    }

    /// Navigate to the previous split (circular)
    pub fn prev_split(&mut self) {
        let leaf_ids = self.root.leaf_split_ids();
        if let Some(pos) = leaf_ids.iter().position(|id| *id == self.active_split) {
            let prev_pos = if pos == 0 { leaf_ids.len() } else { pos } - 1;
            self.active_split = leaf_ids[prev_pos];
        }
    }

    /// Get all split IDs that display a specific buffer
    pub fn splits_for_buffer(&self, target_buffer_id: BufferId) -> Vec<SplitId> {
        self.root
            .get_leaves_with_rects(Rect {
                x: 0,
                y: 0,
                width: 1,
                height: 1,
            })
            .into_iter()
            .filter(|(_, buffer_id, _)| *buffer_id == target_buffer_id)
            .map(|(split_id, _, _)| split_id)
            .collect()
    }

    /// Get the buffer ID for a specific split
    pub fn buffer_for_split(&self, target_split_id: SplitId) -> Option<BufferId> {
        self.root
            .get_leaves_with_rects(Rect {
                x: 0,
                y: 0,
                width: 1,
                height: 1,
            })
            .into_iter()
            .find(|(split_id, _, _)| *split_id == target_split_id)
            .map(|(_, buffer_id, _)| buffer_id)
    }

    /// Maximize the active split (hide all other splits temporarily)
    /// Returns Ok(()) if successful, Err if there's only one split
    pub fn maximize_split(&mut self) -> Result<(), String> {
        // Can't maximize if there's only one split
        if self.root.count_leaves() <= 1 {
            return Err("Cannot maximize: only one split exists".to_string());
        }

        // Can't maximize if already maximized
        if self.maximized_split.is_some() {
            return Err("A split is already maximized".to_string());
        }

        // Maximize the active split
        self.maximized_split = Some(self.active_split);
        Ok(())
    }

    /// Unmaximize the currently maximized split (restore all splits)
    /// Returns Ok(()) if successful, Err if no split is maximized
    pub fn unmaximize_split(&mut self) -> Result<(), String> {
        if self.maximized_split.is_none() {
            return Err("No split is maximized".to_string());
        }

        self.maximized_split = None;
        Ok(())
    }

    /// Check if a split is currently maximized
    pub fn is_maximized(&self) -> bool {
        self.maximized_split.is_some()
    }

    /// Get the currently maximized split ID (if any)
    pub fn maximized_split(&self) -> Option<SplitId> {
        self.maximized_split
    }

    /// Toggle maximize state for the active split
    /// If maximized, unmaximize. If not maximized, maximize.
    /// Returns true if maximized, false if ununmaximized.
    pub fn toggle_maximize(&mut self) -> Result<bool, String> {
        if self.is_maximized() {
            self.unmaximize_split()?;
            Ok(false)
        } else {
            self.maximize_split()?;
            Ok(true)
        }
    }

    /// Get all leaf split IDs that belong to a specific sync group
    pub fn get_splits_in_group(
        &self,
        group_id: u32,
        view_states: &std::collections::HashMap<SplitId, SplitViewState>,
    ) -> Vec<SplitId> {
        self.root
            .leaf_split_ids()
            .into_iter()
            .filter(|id| {
                view_states
                    .get(id)
                    .and_then(|vs| vs.sync_group)
                    .is_some_and(|g| g == group_id)
            })
            .collect()
    }

    // === Split labels ===

    /// Set a label on a leaf split (e.g., "sidebar")
    pub fn set_label(&mut self, split_id: SplitId, label: String) {
        self.labels.insert(split_id, label);
    }

    /// Remove a label from a split
    pub fn clear_label(&mut self, split_id: SplitId) {
        self.labels.remove(&split_id);
    }

    /// Get the label for a split (if any)
    pub fn get_label(&self, split_id: SplitId) -> Option<&str> {
        self.labels.get(&split_id).map(|s| s.as_str())
    }

    /// Get all split labels (for workspace serialization)
    pub fn labels(&self) -> &HashMap<SplitId, String> {
        &self.labels
    }

    /// Find the first leaf split with the given label
    pub fn find_split_by_label(&self, label: &str) -> Option<SplitId> {
        self.root
            .leaf_split_ids()
            .into_iter()
            .find(|id| self.labels.get(id).is_some_and(|l| l == label))
    }

    /// Find the first leaf split without a label
    pub fn find_unlabeled_leaf(&self) -> Option<SplitId> {
        self.root
            .leaf_split_ids()
            .into_iter()
            .find(|id| !self.labels.contains_key(id))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_split_manager() {
        let buffer_id = BufferId(0);
        let manager = SplitManager::new(buffer_id);

        assert_eq!(manager.active_buffer_id(), Some(buffer_id));
        assert_eq!(manager.root().count_leaves(), 1);
    }

    #[test]
    fn test_horizontal_split() {
        let buffer_a = BufferId(0);
        let buffer_b = BufferId(1);

        let mut manager = SplitManager::new(buffer_a);
        let result = manager.split_active(SplitDirection::Horizontal, buffer_b, 0.5);

        assert!(result.is_ok());
        assert_eq!(manager.root().count_leaves(), 2);
    }

    #[test]
    fn test_vertical_split() {
        let buffer_a = BufferId(0);
        let buffer_b = BufferId(1);

        let mut manager = SplitManager::new(buffer_a);
        let result = manager.split_active(SplitDirection::Vertical, buffer_b, 0.5);

        assert!(result.is_ok());
        assert_eq!(manager.root().count_leaves(), 2);
    }

    #[test]
    fn test_nested_splits() {
        let buffer_a = BufferId(0);
        let buffer_b = BufferId(1);
        let buffer_c = BufferId(2);

        let mut manager = SplitManager::new(buffer_a);

        // Split horizontally
        manager
            .split_active(SplitDirection::Horizontal, buffer_b, 0.5)
            .unwrap();

        // Split the second pane vertically
        manager
            .split_active(SplitDirection::Vertical, buffer_c, 0.5)
            .unwrap();

        assert_eq!(manager.root().count_leaves(), 3);
    }

    #[test]
    fn test_close_split() {
        let buffer_a = BufferId(0);
        let buffer_b = BufferId(1);

        let mut manager = SplitManager::new(buffer_a);
        let new_split = manager
            .split_active(SplitDirection::Horizontal, buffer_b, 0.5)
            .unwrap();

        assert_eq!(manager.root().count_leaves(), 2);

        // Close the new split
        let result = manager.close_split(new_split);
        assert!(result.is_ok());
        assert_eq!(manager.root().count_leaves(), 1);
    }

    #[test]
    fn test_cannot_close_last_split() {
        let buffer_a = BufferId(0);
        let mut manager = SplitManager::new(buffer_a);

        let result = manager.close_split(manager.active_split());
        assert!(result.is_err());
    }

    #[test]
    fn test_split_rect_horizontal() {
        let rect = Rect {
            x: 0,
            y: 0,
            width: 100,
            height: 100,
        };

        let (first, second) = split_rect(rect, SplitDirection::Horizontal, 0.5);

        // With 1 line reserved for separator: (100-1)/2 = 49.5 rounds to 50 and 49
        assert_eq!(first.height, 50);
        assert_eq!(second.height, 49);
        assert_eq!(first.width, 100);
        assert_eq!(second.width, 100);
        assert_eq!(first.y, 0);
        assert_eq!(second.y, 51); // first.y + first.height + 1 (separator)
    }

    #[test]
    fn test_split_rect_vertical() {
        let rect = Rect {
            x: 0,
            y: 0,
            width: 100,
            height: 100,
        };

        let (first, second) = split_rect(rect, SplitDirection::Vertical, 0.5);

        // With 1 column reserved for separator: (100-1)/2 = 49.5 rounds to 50 and 49
        assert_eq!(first.width, 50);
        assert_eq!(second.width, 49);
        assert_eq!(first.height, 100);
        assert_eq!(second.height, 100);
        assert_eq!(first.x, 0);
        assert_eq!(second.x, 51); // first.x + first.width + 1 (separator)
    }

    // === Split label tests ===

    #[test]
    fn test_set_and_get_label() {
        let mut manager = SplitManager::new(BufferId(0));
        let split = manager.active_split();

        assert_eq!(manager.get_label(split), None);

        manager.set_label(split, "sidebar".to_string());
        assert_eq!(manager.get_label(split), Some("sidebar"));
    }

    #[test]
    fn test_clear_label() {
        let mut manager = SplitManager::new(BufferId(0));
        let split = manager.active_split();

        manager.set_label(split, "sidebar".to_string());
        assert!(manager.get_label(split).is_some());

        manager.clear_label(split);
        assert_eq!(manager.get_label(split), None);
    }

    #[test]
    fn test_find_split_by_label() {
        let mut manager = SplitManager::new(BufferId(0));
        let first_split = manager.active_split();

        let second_split = manager
            .split_active(SplitDirection::Vertical, BufferId(1), 0.5)
            .unwrap();

        manager.set_label(first_split, "sidebar".to_string());

        assert_eq!(manager.find_split_by_label("sidebar"), Some(first_split));
        assert_eq!(manager.find_split_by_label("terminal"), None);

        // The second split has no label
        assert_ne!(manager.find_split_by_label("sidebar"), Some(second_split));
    }

    #[test]
    fn test_find_unlabeled_leaf() {
        let mut manager = SplitManager::new(BufferId(0));
        let first_split = manager.active_split();

        let second_split = manager
            .split_active(SplitDirection::Vertical, BufferId(1), 0.5)
            .unwrap();

        // No labels — first leaf returned
        assert!(manager.find_unlabeled_leaf().is_some());

        // Label the first split — unlabeled should return the second
        manager.set_label(first_split, "sidebar".to_string());
        assert_eq!(manager.find_unlabeled_leaf(), Some(second_split));

        // Label both — no unlabeled leaf
        manager.set_label(second_split, "terminal".to_string());
        assert_eq!(manager.find_unlabeled_leaf(), None);
    }

    #[test]
    fn test_close_split_cleans_up_label() {
        let mut manager = SplitManager::new(BufferId(0));
        let _first_split = manager.active_split();

        let second_split = manager
            .split_active(SplitDirection::Vertical, BufferId(1), 0.5)
            .unwrap();

        manager.set_label(second_split, "sidebar".to_string());
        assert_eq!(manager.find_split_by_label("sidebar"), Some(second_split));

        manager.close_split(second_split).unwrap();

        // Label should be cleaned up
        assert_eq!(manager.find_split_by_label("sidebar"), None);
        assert_eq!(manager.get_label(second_split), None);
    }

    #[test]
    fn test_label_overwrite() {
        let mut manager = SplitManager::new(BufferId(0));
        let split = manager.active_split();

        manager.set_label(split, "sidebar".to_string());
        assert_eq!(manager.get_label(split), Some("sidebar"));

        manager.set_label(split, "terminal".to_string());
        assert_eq!(manager.get_label(split), Some("terminal"));
        assert_eq!(manager.find_split_by_label("sidebar"), None);
        assert_eq!(manager.find_split_by_label("terminal"), Some(split));
    }

    #[test]
    fn test_find_unlabeled_leaf_single_split_no_label() {
        let manager = SplitManager::new(BufferId(0));
        // Single unlabeled split — should return it
        assert_eq!(manager.find_unlabeled_leaf(), Some(manager.active_split()));
    }

    #[test]
    fn test_find_unlabeled_leaf_single_split_labeled() {
        let mut manager = SplitManager::new(BufferId(0));
        let split = manager.active_split();
        manager.set_label(split, "only".to_string());
        // Only split is labeled — returns None
        assert_eq!(manager.find_unlabeled_leaf(), None);
    }
}
