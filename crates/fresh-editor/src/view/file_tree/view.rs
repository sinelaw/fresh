use super::ignore::IgnorePatterns;
use super::node::NodeId;
use super::node::NodeState;
use super::search::FileExplorerSearch;
use super::tree::FileTree;
use crate::input::fuzzy::FuzzyMatch;
use crate::model::filesystem::DirEntry;
use std::collections::{HashMap, HashSet};
use std::path::Path;
use std::path::PathBuf;
use std::sync::{Arc, OnceLock};

/// One visible row of the tree: the node, where it sits, and what a row
/// needs to say about it.
///
/// The facts are copied out of the tree when the projection is made, so a
/// row can be described from the projection alone — at layout, by the
/// window, for whichever rows it holds — without a borrow of the tree.
#[derive(Debug, Clone)]
pub struct VisibleRow {
    pub id: NodeId,
    /// Depth as drawn: absorbed compact-directory ancestors do not count.
    pub indent: usize,
    /// The nearest ancestor that has a row of its own, by row index.
    pub parent: Option<usize>,
    pub entry: DirEntry,
    pub state: NodeState,
    /// The ancestors compact mode folded into this row, outermost first.
    pub chain: Vec<ChainSegment>,
}

/// One ancestor compact mode folded into a row: the name the row draws for it,
/// and the directory it names.
#[derive(Debug, Clone)]
pub struct ChainSegment {
    pub name: String,
    pub path: PathBuf,
}

impl VisibleRow {
    pub fn is_dir(&self) -> bool {
        self.entry.is_dir()
    }

    pub fn is_expanded(&self) -> bool {
        self.state == NodeState::Expanded
    }
}

/// The tree as the explorer shows it: every visible row, in order.
///
/// **Made once per change to the tree, not once per frame.** Expansion, a
/// reload, the ignore rules and compact mode all go through `&mut
/// FileTreeView`, and each of those drops it; everything that reads the
/// visible order — navigation, the window, the web projection — shares the
/// one that was made.
#[derive(Debug, Default)]
pub struct Projection {
    pub rows: Vec<VisibleRow>,
    index_of: HashMap<NodeId, usize>,
}

impl Projection {
    pub fn len(&self) -> usize {
        self.rows.len()
    }

    pub fn is_empty(&self) -> bool {
        self.rows.is_empty()
    }

    /// Where `id` is in the visible order.
    pub fn index_of(&self, id: NodeId) -> Option<usize> {
        self.index_of.get(&id).copied()
    }
}

/// View state for file tree navigation and filtering
#[derive(Debug)]
pub struct FileTreeView {
    /// The underlying tree model
    tree: FileTree,
    /// Cursor / focus node (always a single item)
    selected_node: Option<NodeId>,
    /// Multi-selection set — empty means single-cursor mode
    multi_selection: HashSet<NodeId>,
    /// Anchor for Shift+range extension
    selection_anchor: Option<NodeId>,
    /// Where the explorer's window last reported it was, as the index of
    /// its first scrolled row. **A record, not the window:** the list that
    /// draws the tree owns its window, and this is only where it said it
    /// went — kept so a list mounted again (after a background expand
    /// hands the tree out, or a window switch) starts there, and so the
    /// workspace can save it.
    window_top: usize,
    /// Bumped by every command that should bring the selection into view —
    /// the keys, a toggle, a reveal of a path. The window follows the
    /// selection for it (and for a change of selected row) until the wheel
    /// takes it elsewhere; the model never says where the window goes.
    reveal: u64,
    /// The `reveal` the window at `window_top` already answers: the token
    /// as of the list's last report. A list mounted again follows only a
    /// request made since — one made while it was away (a background
    /// expand-to-path) — not one the reader has since wheeled away from.
    answered: u64,
    /// Sort mode for entries
    sort_mode: SortMode,
    /// Ignore patterns for filtering
    ignore_patterns: IgnorePatterns,
    /// Search state for quick navigation
    search: FileExplorerSearch,
    /// Render single-child directory chains as a single row
    /// (`foo/bar/baz`). Mirrors VSCode's `explorer.compactFolders`.
    compact_directories: bool,
    /// The visible rows, made on first read after a change. See
    /// [`Projection`].
    projection: OnceLock<Arc<Projection>>,
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
            multi_selection: HashSet::new(),
            selection_anchor: None,
            window_top: 0,
            reveal: 0,
            answered: 0,
            sort_mode: SortMode::Type,
            ignore_patterns: IgnorePatterns::new(),
            search: FileExplorerSearch::new(),
            compact_directories: true,
            projection: OnceLock::new(),
        }
    }

    /// The visible rows, made now if a change dropped them.
    pub fn projection(&self) -> Arc<Projection> {
        self.projection
            .get_or_init(|| Arc::new(self.project()))
            .clone()
    }

    /// The count the window follows the selection by. See `reveal`.
    pub fn reveal_token(&self) -> u64 {
        self.reveal
    }

    /// Ask for the selection to be shown, wherever it is — for a caller that
    /// set it with [`set_selected`](Self::set_selected) on a command the
    /// reader expects to see the result of.
    pub fn show_selection(&mut self) {
        self.reveal = self.reveal.wrapping_add(1);
    }

    /// Drop the projection: the tree, or what of it is visible, changed.
    fn changed(&mut self) {
        self.projection = OnceLock::new();
    }

    fn project(&self) -> Projection {
        let mut ids = Vec::new();
        self.collect_filtered_visible(self.tree.root_id(), &mut ids);
        // Only ids with a node get a row, and an index is a row's: a missing
        // node dropped below would otherwise shift every index after it.
        ids.retain(|&id| self.tree.get_node(id).is_some());
        let index_of: HashMap<NodeId, usize> =
            ids.iter().enumerate().map(|(i, &id)| (id, i)).collect();
        let rows = ids
            .iter()
            .filter_map(|&id| {
                let node = self.tree.get_node(id)?;
                // The nearest ancestor with a row: an absorbed one shares
                // this row and has none.
                let mut parent = None;
                let mut at = node.parent;
                while let Some(p) = at {
                    if let Some(&i) = index_of.get(&p) {
                        parent = Some(i);
                        break;
                    }
                    at = self.tree.get_node(p).and_then(|n| n.parent);
                }
                let depth = self.tree.get_depth(id);
                Some(VisibleRow {
                    id,
                    indent: depth.saturating_sub(self.count_absorbed_ancestors(id)),
                    parent,
                    entry: node.entry.clone(),
                    state: node.state.clone(),
                    chain: self
                        .compact_chain_for_anchor(id)
                        .into_iter()
                        .filter_map(|a| self.tree.get_node(a))
                        .map(|n| ChainSegment {
                            name: n.entry.name.clone(),
                            path: n.entry.path.clone(),
                        })
                        .collect(),
                })
            })
            .collect();
        Projection { rows, index_of }
    }

    /// Toggle/set the compact-directory rendering mode.
    pub fn set_compact_directories(&mut self, enabled: bool) {
        self.compact_directories = enabled;
        self.changed();
    }

    /// Whether compact-directory rendering is enabled.
    pub fn compact_directories(&self) -> bool {
        self.compact_directories
    }

    /// Returns true if `node_id` is a directory whose row gets folded into
    /// a deeper anchor's row under compact-directory rendering — i.e. it is
    /// expanded with exactly one visible child that is also a directory.
    /// Root is never absorbed.
    fn is_absorbed(&self, node_id: NodeId) -> bool {
        if !self.compact_directories {
            return false;
        }
        if node_id == self.tree.root_id() {
            return false;
        }
        let node = match self.tree.get_node(node_id) {
            Some(n) => n,
            None => return false,
        };
        if !node.is_dir() || !node.is_expanded() {
            return false;
        }
        let mut visible_iter = node
            .children
            .iter()
            .copied()
            .filter(|&c| self.is_node_visible(c));
        let only_child = match visible_iter.next() {
            Some(c) => c,
            None => return false,
        };
        if visible_iter.next().is_some() {
            return false;
        }
        match self.tree.get_node(only_child) {
            Some(child) => child.is_dir(),
            None => false,
        }
    }

    /// Expand `node_id` and then walk down any single-child-directory
    /// chain, expanding each step so the full chain reveals on a single
    /// rendered row. No-op when compact mode is off or when the node
    /// isn't a directory. Stops as soon as a step has zero, multiple, or
    /// non-directory visible children.
    pub async fn expand_with_chain(&mut self, node_id: NodeId) -> std::io::Result<()> {
        self.changed();
        // Always perform the first expansion so callers can use this in
        // place of `tree.expand_node` regardless of compact mode.
        let needs_initial_expand = self
            .tree
            .get_node(node_id)
            .map(|n| n.is_dir() && !n.is_expanded())
            .unwrap_or(false);
        if needs_initial_expand {
            self.tree.expand_node(node_id).await?;
            self.changed();
        }
        if !self.compact_directories {
            return Ok(());
        }
        let mut cur = node_id;
        loop {
            // Determine the unique visible directory child of `cur`, if
            // one exists. Limit the immutable borrow to this block so the
            // subsequent `expand_node` call can take a mutable borrow.
            let next = {
                let node = match self.tree.get_node(cur) {
                    Some(n) => n,
                    None => return Ok(()),
                };
                if !node.is_expanded() {
                    return Ok(());
                }
                let mut visible = node
                    .children
                    .iter()
                    .copied()
                    .filter(|&c| self.is_node_visible(c));
                let only = match visible.next() {
                    Some(c) => c,
                    None => return Ok(()),
                };
                if visible.next().is_some() {
                    return Ok(());
                }
                match self.tree.get_node(only) {
                    Some(child) if child.is_dir() => only,
                    _ => return Ok(()),
                }
            };
            let already_expanded = self
                .tree
                .get_node(next)
                .map(|n| n.is_expanded())
                .unwrap_or(false);
            if !already_expanded {
                self.tree.expand_node(next).await?;
                self.changed();
            }
            cur = next;
        }
    }

    /// Toggle expansion on `node_id`. When expanding, also reveals the
    /// rest of any single-child-directory chain (see `expand_with_chain`).
    pub async fn toggle_with_chain(&mut self, node_id: NodeId) -> std::io::Result<()> {
        self.show_selection();
        let was_expanded = self
            .tree
            .get_node(node_id)
            .map(|n| n.is_expanded())
            .unwrap_or(false);
        self.changed();
        self.tree.toggle_node(node_id).await?;
        self.changed();
        if !was_expanded {
            self.expand_with_chain(node_id).await?;
        }
        // The expansion may have folded the cursor's row into a deeper
        // anchor; re-promote so the cursor is a row the reader can see, since
        // Rename and Delete act on the cursor.
        if let Some(sel) = self.selected_node {
            self.selected_node = Some(self.promote_to_anchor(sel));
        }
        Ok(())
    }

    /// Build the compact-chain prefix for `anchor`: the chain of ancestor
    /// directories that share its row, ordered outermost-first. Empty when
    /// compact mode is off or the anchor isn't part of a chain.
    pub fn compact_chain_for_anchor(&self, anchor: NodeId) -> Vec<NodeId> {
        if !self.compact_directories {
            return Vec::new();
        }
        let mut prefix = Vec::new();
        let mut cur = anchor;
        while let Some(cur_node) = self.tree.get_node(cur) {
            let parent_id = match cur_node.parent {
                Some(p) => p,
                None => break,
            };
            if !self.is_absorbed(parent_id) {
                break;
            }
            prefix.push(parent_id);
            cur = parent_id;
        }
        prefix.reverse();
        prefix
    }

    /// The visible nodes in display order: expanded, not ignored, and not
    /// folded into a compact chain. Read off the projection; the walk is
    /// [`Self::collect_filtered_visible`], which only the projection runs.
    fn filtered_visible_nodes(&self) -> Vec<NodeId> {
        self.projection().rows.iter().map(|r| r.id).collect()
    }

    /// Recursively collect visible nodes, skipping ignored subtrees.
    /// When compact-directory mode is enabled, intermediate nodes that are
    /// folded into a deeper anchor's row are also skipped — only the
    /// anchor (the deepest non-absorbed node in the chain) appears in the
    /// list, so navigation, indexing, and scrolling all operate on
    /// rendered rows rather than raw tree nodes.
    fn collect_filtered_visible(&self, id: NodeId, result: &mut Vec<NodeId>) {
        let is_root = id == self.tree.root_id();
        if !is_root && !self.is_node_visible(id) {
            return;
        }

        if !self.is_absorbed(id) {
            result.push(id);
        }

        if let Some(node) = self.tree.get_node(id) {
            if node.is_expanded() {
                for &child_id in &node.children {
                    self.collect_filtered_visible(child_id, result);
                }
            }
        }
    }

    /// Get the underlying tree
    pub fn tree(&self) -> &FileTree {
        &self.tree
    }

    /// Get mutable reference to the underlying tree. Drops the projection:
    /// whatever the caller does with it is a change.
    pub fn tree_mut(&mut self) -> &mut FileTree {
        self.changed();
        &mut self.tree
    }

    /// Get currently visible nodes with their indent levels
    ///
    /// Returns a list of (NodeId, indent_level) tuples for rendering. In
    /// compact-directory mode the indent skips every absorbed ancestor
    /// in the path so every row sits beneath its visible parent — both
    /// chain anchors *and* their descendants render at the right level.
    pub fn get_display_nodes(&self) -> Vec<(NodeId, usize)> {
        self.projection()
            .rows
            .iter()
            .map(|r| (r.id, r.indent))
            .collect()
    }

    /// Count ancestors of `id` whose row is folded into a deeper anchor's
    /// row under compact mode. Returns 0 when compact mode is off.
    fn count_absorbed_ancestors(&self, id: NodeId) -> usize {
        let mut count = 0usize;
        let mut cur = id;
        while let Some(parent_id) = self.tree.get_node(cur).and_then(|n| n.parent) {
            if self.is_absorbed(parent_id) {
                count += 1;
            }
            cur = parent_id;
        }
        count
    }

    /// Get the currently selected node ID
    pub fn get_selected(&self) -> Option<NodeId> {
        self.selected_node
    }

    /// Set the selected node. The id is promoted to its chain anchor so
    /// the cursor always lands on a rendered row in compact mode.
    pub fn set_selected(&mut self, node_id: Option<NodeId>) {
        self.selected_node = node_id.map(|id| self.promote_to_anchor(id));
    }

    /// Set the selected node as given, with no promotion to a chain anchor: for
    /// a press on one segment of a `dir1/dir2/dir3` row, promoting would make
    /// the menu act on the deepest directory whichever name was clicked.
    pub fn set_selected_exact(&mut self, node_id: Option<NodeId>) {
        self.selected_node = node_id;
    }

    /// What a press on row `index` named: the folded directory `segment`, when
    /// the press landed on one of a compact row's names, else the row itself.
    ///
    /// By path, not by id: the label the press was resolved against was drawn
    /// a frame earlier, and a path still names the right directory if the
    /// chain has grown or shrunk since, or if a reload has recycled ids.
    pub fn press_target(&self, index: usize, segment: Option<&Path>) -> Option<NodeId> {
        let anchor = self.get_node_at_index(index)?;
        Some(
            segment
                .and_then(|path| self.tree.get_node_by_path(path))
                .map(|node| node.id)
                .unwrap_or(anchor),
        )
    }

    /// The row `id` is drawn on, by index in the visible order. A directory
    /// compact mode folded into a deeper row has no row of its own; the anchor
    /// whose label carries it is where it shows.
    fn row_of(&self, id: NodeId) -> Option<usize> {
        // An id with a row of its own is never absorbed, so promoting first
        // answers both cases.
        self.projection().index_of(self.promote_to_anchor(id))
    }

    /// The id of the row the cursor is drawn on.
    ///
    /// The multi-selection is a set of rows: it highlights them, and
    /// [`effective_selection`](Self::effective_selection) drops ids with no
    /// row. So the keys that seed it from the cursor use this, not the cursor.
    fn cursor_row_id(&self) -> Option<NodeId> {
        let id = self.promote_to_anchor(self.selected_node?);
        // The lookup only tests that it is a row.
        self.projection().index_of(id).map(|_| id)
    }

    /// Walk down a chain of absorbed directories until reaching the
    /// non-absorbed anchor. For non-absorbed nodes returns the input.
    fn promote_to_anchor(&self, node_id: NodeId) -> NodeId {
        let mut cur = node_id;
        while self.is_absorbed(cur) {
            let next = self.tree.get_node(cur).and_then(|node| {
                node.children
                    .iter()
                    .copied()
                    .find(|&c| self.is_node_visible(c))
            });
            match next {
                Some(c) => cur = c,
                None => break,
            }
        }
        cur
    }

    /// Select the next visible node (clears multi-selection)
    pub fn select_next(&mut self) {
        self.show_selection();
        self.clear_multi_selection();
        let visible = self.filtered_visible_nodes();
        if visible.is_empty() {
            return;
        }

        if let Some(current) = self.selected_node {
            if let Some(pos) = self.row_of(current) {
                if pos + 1 < visible.len() {
                    self.selected_node = Some(visible[pos + 1]);
                }
            }
        } else {
            self.selected_node = Some(visible[0]);
        }
    }

    /// Select the previous visible node (clears multi-selection)
    pub fn select_prev(&mut self) {
        self.show_selection();
        self.clear_multi_selection();
        let visible = self.filtered_visible_nodes();
        if visible.is_empty() {
            return;
        }

        if let Some(current) = self.selected_node {
            if let Some(pos) = self.row_of(current) {
                if pos > 0 {
                    self.selected_node = Some(visible[pos - 1]);
                }
            }
        } else {
            self.selected_node = Some(visible[0]);
        }
    }

    /// Put the cursor on the row at `index` in the visible order — where a
    /// page key lands, which the window that knows the page works out
    /// (clears multi-selection).
    pub fn select_index(&mut self, index: usize) {
        self.show_selection();
        self.clear_multi_selection();
        if let Some(r) = self.projection().rows.get(index) {
            self.selected_node = Some(r.id);
        }
    }

    /// Select the first visible node
    pub fn select_first(&mut self) {
        self.show_selection();
        let visible = self.filtered_visible_nodes();
        if !visible.is_empty() {
            self.selected_node = Some(visible[0]);
        }
    }

    /// Select the last visible node
    pub fn select_last(&mut self) {
        self.show_selection();
        let visible = self.filtered_visible_nodes();
        if !visible.is_empty() {
            self.selected_node = Some(*visible.last().unwrap());
        }
    }

    /// Toggle the cursor item in/out of the multi-selection and set the anchor.
    /// The cursor's *row*: see [`cursor_row_id`](Self::cursor_row_id).
    pub fn toggle_select(&mut self) {
        if let Some(cursor) = self.cursor_row_id() {
            if self.multi_selection.contains(&cursor) {
                self.multi_selection.remove(&cursor);
            } else {
                self.multi_selection.insert(cursor);
            }
            self.selection_anchor = Some(cursor);
        }
    }

    /// Extend the selection one step upward from the current cursor.
    pub fn extend_selection_up(&mut self) {
        self.show_selection();
        let visible = self.filtered_visible_nodes();
        if visible.is_empty() {
            return;
        }
        // The cursor's row, so the seed and the anchor are ids the set can
        // show: see `cursor_row_id`.
        let Some(pos) = self.selected_node.and_then(|id| self.row_of(id)) else {
            return;
        };
        let current = visible[pos];
        // Always seed the selection with the cursor row first — even at the
        // top boundary, so Escape / a subsequent Shift+Down sees a live
        // selection anchored on wherever the user started the range.
        if self.multi_selection.is_empty() {
            self.multi_selection.insert(current);
            self.selection_anchor = Some(current);
        }
        if pos == 0 {
            return;
        }
        let anchor = self.selection_anchor.unwrap_or(current);
        let new_pos = pos - 1;
        self.selected_node = Some(visible[new_pos]);
        let anchor_pos = visible
            .iter()
            .position(|&id| id == anchor)
            .unwrap_or(new_pos);
        let (lo, hi) = (new_pos.min(anchor_pos), new_pos.max(anchor_pos));
        self.multi_selection = visible[lo..=hi].iter().copied().collect();
    }

    /// Extend the selection one step downward from the current cursor.
    pub fn extend_selection_down(&mut self) {
        self.show_selection();
        let visible = self.filtered_visible_nodes();
        if visible.is_empty() {
            return;
        }
        // The cursor's row, so the seed and the anchor are ids the set can
        // show: see `cursor_row_id`.
        let Some(pos) = self.selected_node.and_then(|id| self.row_of(id)) else {
            return;
        };
        let current = visible[pos];
        // Always seed the selection with the cursor row first — even at the
        // bottom boundary, so Escape / a subsequent Shift+Up sees a live
        // selection anchored on wherever the user started the range.
        if self.multi_selection.is_empty() {
            self.multi_selection.insert(current);
            self.selection_anchor = Some(current);
        }
        if pos + 1 >= visible.len() {
            return;
        }
        let anchor = self.selection_anchor.unwrap_or(current);
        let new_pos = pos + 1;
        self.selected_node = Some(visible[new_pos]);
        let anchor_pos = visible
            .iter()
            .position(|&id| id == anchor)
            .unwrap_or(new_pos);
        let (lo, hi) = (new_pos.min(anchor_pos), new_pos.max(anchor_pos));
        self.multi_selection = visible[lo..=hi].iter().copied().collect();
    }

    /// Select all currently visible nodes.
    pub fn select_all(&mut self) {
        let visible = self.filtered_visible_nodes();
        self.multi_selection = visible.iter().copied().collect();
        // The cursor's row, so the range extension can find the anchor.
        self.selection_anchor = self.cursor_row_id();
    }

    /// Clear multi-selection (return to single-cursor mode).
    pub fn clear_multi_selection(&mut self) {
        self.multi_selection.clear();
        self.selection_anchor = None;
    }

    /// True when the explorer is in multi-selection mode — i.e. at least
    /// one item has been explicitly added to the selection via Shift+arrow,
    /// Space, or Ctrl+A. Distinguishes "user picked a specific set" from
    /// plain cursor navigation, even when that set holds just one item.
    pub fn has_multi_selection(&self) -> bool {
        !self.multi_selection.is_empty()
    }

    /// Returns the set of multi-selected nodes (empty in single-cursor mode).
    pub fn multi_selection(&self) -> &HashSet<NodeId> {
        &self.multi_selection
    }

    /// The nodes that operations (copy/cut/delete) should act on.
    /// Returns the multi-selection when non-empty, otherwise `[cursor]`.
    ///
    /// Multi-selected items are returned in visible tree order rather than
    /// `HashSet` iteration order, so callers (e.g. multi-paste) see a
    /// deterministic sequence matching what the user sees on screen.
    pub fn effective_selection(&self) -> Vec<NodeId> {
        if self.multi_selection.is_empty() {
            return self.selected_node.into_iter().collect();
        }
        // Walk visible nodes in order and keep those in the selection set.
        // This also filters out any stale NodeIds that may have lingered
        // from a prior tree mutation.
        self.filtered_visible_nodes()
            .into_iter()
            .filter(|id| self.multi_selection.contains(id))
            .collect()
    }

    /// Select the parent of the currently selected node
    pub fn select_parent(&mut self) {
        self.show_selection();
        if let Some(current) = self.selected_node {
            if let Some(node) = self.tree.get_node(current) {
                if let Some(mut parent_id) = node.parent {
                    // In compact mode, the immediate parent may be an
                    // absorbed directory folded into this same row. Walk
                    // up until we reach a non-absorbed ancestor so the
                    // cursor lands on a different visible row.
                    while self.is_absorbed(parent_id) {
                        let next = self.tree.get_node(parent_id).and_then(|n| n.parent);
                        match next {
                            Some(p) => parent_id = p,
                            None => break,
                        }
                    }
                    self.selected_node = Some(parent_id);
                }
            }
        }
    }

    /// Where the explorer's window last said it was. See `window_top`.
    pub fn window_top(&self) -> usize {
        self.window_top
    }

    /// Record where the explorer's window went — the list's report, not a
    /// request: the window is already there, and it answers every reveal
    /// asked so far.
    pub fn note_window(&mut self, top: usize) {
        self.window_top = top;
        self.answered = self.reveal;
    }

    /// The reveal token the window at [`window_top`](Self::window_top)
    /// already answers.
    pub fn answered_token(&self) -> u64 {
        self.answered
    }

    /// Get the sort mode
    #[cfg(test)]
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
        self.show_selection();
        if let Some(node) = self.tree.get_node_by_path(path) {
            let id = node.id;
            self.selected_node = Some(self.promote_to_anchor(id));
        }
    }

    /// Get the index of the selected node in the visible list. A cursor on a
    /// chain segment reports the row that draws it.
    pub fn get_selected_index(&self) -> Option<usize> {
        self.row_of(self.selected_node?)
    }

    /// The visible node at `index`, in the tree's display order.
    pub fn get_node_at_index(&self, index: usize) -> Option<NodeId> {
        self.projection().rows.get(index).map(|r| r.id)
    }

    /// Get the number of visible nodes
    pub fn visible_count(&self) -> usize {
        self.projection().len()
    }

    /// Get reference to ignore patterns
    pub fn ignore_patterns(&self) -> &IgnorePatterns {
        &self.ignore_patterns
    }

    /// Get mutable reference to ignore patterns
    pub fn ignore_patterns_mut(&mut self) -> &mut IgnorePatterns {
        self.changed();
        &mut self.ignore_patterns
    }

    /// Toggle showing hidden files
    pub fn toggle_show_hidden(&mut self) {
        self.ignore_patterns.toggle_show_hidden();
        self.changed();
    }

    /// Toggle showing gitignored files
    pub fn toggle_show_gitignored(&mut self) {
        self.ignore_patterns.toggle_show_gitignored();
        self.changed();
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

    /// Install a gitignore for `dir_path` from already-read bytes. Caller
    /// performs the I/O via the editor's filesystem authority.
    pub fn load_gitignore_from_bytes(
        &mut self,
        dir_path: &std::path::Path,
        contents: &[u8],
        mtime: Option<std::time::SystemTime>,
    ) {
        self.ignore_patterns
            .load_gitignore_from_bytes(dir_path, contents, mtime);
        self.changed();
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
        self.show_selection();
        self.changed();
        let found = self.tree.expand_to_path(path).await;
        self.changed();
        if let Some(node_id) = found {
            self.selected_node = Some(self.promote_to_anchor(node_id));
            true
        } else {
            false
        }
    }

    /// Collect symlink mappings from visible symlink directories.
    ///
    /// Returns a HashMap where keys are symlink paths and values are their canonical targets.
    /// This is used to create decoration aliases so files under symlinked directories
    /// can show their git status correctly.
    pub fn collect_symlink_mappings(&self) -> HashMap<PathBuf, PathBuf> {
        let mut mappings = HashMap::new();

        for node_id in self.filtered_visible_nodes() {
            if let Some(node) = self.tree.get_node(node_id) {
                // Visible symlink directories need aliases even while collapsed so
                // bubbled git/decorations can light the row before first expansion.
                if node.entry.is_symlink() && node.is_dir() {
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

    /// The search, while one is open: what a row's name is matched against.
    pub fn search(&self) -> Option<&FileExplorerSearch> {
        self.search.is_active().then_some(&self.search)
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
        self.show_selection();
        let matching = self.get_matching_nodes();
        if let Some(&first) = matching.first() {
            self.selected_node = Some(first);
        }
    }

    /// Select the next matching node (when search is active)
    pub fn select_next_match(&mut self) {
        self.show_selection();
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
        self.show_selection();
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

    async fn create_sticky_scroll_view() -> (TempDir, FileTreeView) {
        let temp_dir = TempDir::new().unwrap();
        let root = temp_dir.path();
        let leaf_dir = root.join("a/b/c");
        std_fs::create_dir_all(&leaf_dir).unwrap();
        for i in 0..6 {
            std_fs::write(leaf_dir.join(format!("file{i}.txt")), "content").unwrap();
        }

        let manager = Arc::new(FsManager::new(Arc::new(StdFileSystem)));
        let mut tree = FileTree::new(root.to_path_buf(), manager).await.unwrap();
        for path in [
            root.to_path_buf(),
            root.join("a"),
            root.join("a/b"),
            leaf_dir,
        ] {
            let id = tree.get_node_by_path(&path).unwrap().id;
            tree.expand_node(id).await.unwrap();
        }

        let mut view = FileTreeView::new(tree);
        // Keep every ancestor on its own row so this fixture exercises the
        // sticky stack rather than compact-directory folding.
        view.set_compact_directories(false);
        (temp_dir, view)
    }

    #[tokio::test]
    async fn test_view_creation() {
        let (_temp_dir, view) = create_test_view().await;

        assert!(view.get_selected().is_some());
        assert_eq!(view.window_top(), 0);
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

    /// **Each row knows its parent's row**, which is what the explorer's
    /// window pins above a scrolled run: the expanded ancestors of the run's
    /// first row. The window asks at layout; the answer is the projection's.
    #[tokio::test]
    async fn each_row_names_the_row_of_its_parent() {
        let (_temp_dir, view) = create_sticky_scroll_view().await;
        let p = view.projection();
        assert_eq!(p.len(), 10); // root + a/b/c + six files
        let parents: Vec<Option<usize>> = p.rows.iter().map(|r| r.parent).collect();
        assert_eq!(
            parents,
            [
                None,
                Some(0),
                Some(1),
                Some(2),
                Some(3),
                Some(3),
                Some(3),
                Some(3),
                Some(3),
                Some(3)
            ]
        );
    }

    /// A compact chain shares its anchor's row, so the anchor's parent is
    /// the row above the chain, not an absorbed directory without a row.
    #[tokio::test]
    async fn a_compact_chains_parent_is_the_row_above_the_chain() {
        let (_temp_dir, mut view) = create_sticky_scroll_view().await;
        view.set_compact_directories(true);
        let p = view.projection();
        let names: Vec<(String, Vec<String>, Option<usize>)> = p
            .rows
            .iter()
            .map(|r| {
                let chain = r.chain.iter().map(|seg| seg.name.clone()).collect();
                (r.entry.name.clone(), chain, r.parent)
            })
            .collect();
        assert_eq!(names[1].0, "c");
        assert_eq!(names[1].1, ["a", "b"], "a and b fold into c's row");
        assert_eq!(names[1].2, Some(0), "c's row sits under the root's");
        assert_eq!(names[2].2, Some(1), "the files under c's row");
    }

    /// **Made once per change.** Reading the projection twice with nothing
    /// in between hands out the same one; a change to what is visible makes
    /// a new one.
    #[tokio::test]
    async fn the_projection_is_made_again_only_after_a_change() {
        let (_temp_dir, mut view) = create_sticky_scroll_view().await;
        let first = view.projection();
        assert!(Arc::ptr_eq(&first, &view.projection()));
        view.select_last();
        assert!(
            Arc::ptr_eq(&first, &view.projection()),
            "a selection is not a change to the rows"
        );

        let c = first.rows[3].id;
        view.toggle_with_chain(c).await.unwrap();
        let folded = view.projection();
        assert_eq!(folded.len(), 4, "c folded: its six files are gone");

        view.set_compact_directories(true);
        assert_eq!(view.projection().len(), 2, "a/b/c is one row");
    }

    #[tokio::test]
    async fn collapsing_a_sticky_ancestor_keeps_it_selected() {
        let (_temp_dir, mut view) = create_sticky_scroll_view().await;
        let display = view.get_display_nodes();
        let sticky_parent = display[1].0;
        view.set_selected(Some(sticky_parent));

        view.toggle_with_chain(sticky_parent).await.unwrap();

        assert_eq!(view.get_selected(), Some(sticky_parent));
        assert_eq!(view.get_selected_index(), Some(1));
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

    /// Reproducer: expanding a directory whose only contents are gitignored
    /// (e.g. a build artifact dir whose own .gitignore is `*`) must not make
    /// the directory itself disappear from the tree.
    #[tokio::test]
    async fn test_expanded_dir_with_all_children_filtered_stays_visible() {
        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();

        let build_dir = temp_path.join("build");
        std_fs::create_dir(&build_dir).unwrap();
        std_fs::create_dir(build_dir.join("export")).unwrap();
        std_fs::write(build_dir.join("metadata"), b"").unwrap();
        std_fs::write(build_dir.join(".gitignore"), b"*\n").unwrap();

        let backend = Arc::new(StdFileSystem);
        let manager = Arc::new(FsManager::new(backend));
        let tree = FileTree::new(temp_path.to_path_buf(), manager)
            .await
            .unwrap();
        let mut view = FileTreeView::new(tree);

        let root_id = view.tree().root_id();
        view.tree_mut().expand_node(root_id).await.unwrap();

        let build_id = view
            .tree()
            .get_node(root_id)
            .unwrap()
            .children
            .iter()
            .copied()
            .find(|&id| {
                view.tree()
                    .get_node(id)
                    .map(|n| n.entry.name == "build")
                    .unwrap_or(false)
            })
            .expect("build/ child not found");

        view.tree_mut().expand_node(build_id).await.unwrap();
        view.load_gitignore_from_bytes(&build_dir, b"*\n", None);

        let visible: Vec<NodeId> = view
            .get_display_nodes()
            .into_iter()
            .map(|(id, _)| id)
            .collect();
        assert!(
            visible.contains(&build_id),
            "expanded build/ row vanished after its children were all filtered (visible={:?})",
            visible
                .iter()
                .filter_map(|&id| view.tree().get_node(id).map(|n| n.entry.name.clone()))
                .collect::<Vec<_>>()
        );
    }

    // ============================================================
    // Compact-directory tests
    //
    // Fixture layout (created by `create_chain_view`):
    //
    //     <root>/
    //       chain/
    //         a/
    //           b/
    //             c/
    //               leaf.txt
    //       sibling/
    //         other.txt
    //
    // The `chain → a → b → c` segment is a single-child directory chain.
    // `c` ends the chain because its only child is a file. `sibling` is
    // a separate dir at the root level so the root itself has multiple
    // visible children (and thus is never absorbed).
    // ============================================================

    async fn create_chain_view() -> (TempDir, FileTreeView) {
        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();

        std_fs::create_dir_all(temp_path.join("chain/a/b/c")).unwrap();
        std_fs::write(temp_path.join("chain/a/b/c/leaf.txt"), "leaf").unwrap();
        std_fs::create_dir(temp_path.join("sibling")).unwrap();
        std_fs::write(temp_path.join("sibling/other.txt"), "other").unwrap();

        let backend = Arc::new(StdFileSystem);
        let manager = Arc::new(FsManager::new(backend));
        let tree = FileTree::new(temp_path.to_path_buf(), manager)
            .await
            .unwrap();
        let view = FileTreeView::new(tree);

        (temp_dir, view)
    }

    /// Resolve a node id from a path relative to the tree root.
    fn id_for(view: &FileTreeView, rel: &str) -> NodeId {
        let path = view.tree().root_path().join(rel);
        view.tree()
            .get_node_by_path(&path)
            .unwrap_or_else(|| panic!("expected node at {:?}", path))
            .id
    }

    fn name_of(view: &FileTreeView, id: NodeId) -> String {
        view.tree().get_node(id).unwrap().entry.name.clone()
    }

    #[tokio::test]
    async fn test_compact_chain_collapses_single_child_dirs() {
        let (_t, mut view) = create_chain_view().await;
        let root_id = view.tree().root_id();
        view.tree_mut().expand_node(root_id).await.unwrap();

        // expand_with_chain should drill all the way down to `c` because
        // every step has exactly one directory child until `c`'s file
        // child breaks the chain.
        view.expand_with_chain(id_for(&view, "chain"))
            .await
            .unwrap();

        let c_id = id_for(&view, "chain/a/b/c");

        // The chain anchor for `c` is `[chain, a, b]`, outermost-first.
        let prefix = view.compact_chain_for_anchor(c_id);
        let prefix_names: Vec<String> = prefix.iter().map(|&id| name_of(&view, id)).collect();
        assert_eq!(prefix_names, vec!["chain", "a", "b"]);
    }

    #[tokio::test]
    async fn test_compact_display_skips_absorbed_nodes() {
        let (_t, mut view) = create_chain_view().await;
        let root_id = view.tree().root_id();
        view.tree_mut().expand_node(root_id).await.unwrap();
        view.expand_with_chain(id_for(&view, "chain"))
            .await
            .unwrap();

        let visible_names: Vec<String> = view
            .get_display_nodes()
            .into_iter()
            .map(|(id, _)| name_of(&view, id))
            .collect();

        // `chain`, `a`, `b` are folded into `c`'s row and must not appear
        // as separate rows. `c` is the chain anchor, `leaf.txt` sits
        // beneath it as its own row, and `sibling` is unaffected.
        assert!(!visible_names.contains(&"chain".to_string()));
        assert!(!visible_names.contains(&"a".to_string()));
        assert!(!visible_names.contains(&"b".to_string()));
        assert!(visible_names.contains(&"c".to_string()));
        assert!(visible_names.contains(&"leaf.txt".to_string()));
        assert!(visible_names.contains(&"sibling".to_string()));
    }

    #[tokio::test]
    async fn test_compact_indent_preserves_visual_hierarchy() {
        let (_t, mut view) = create_chain_view().await;
        let root_id = view.tree().root_id();
        view.tree_mut().expand_node(root_id).await.unwrap();
        view.expand_with_chain(id_for(&view, "chain"))
            .await
            .unwrap();

        let display = view.get_display_nodes();
        let indent_for = |target: NodeId| {
            display
                .iter()
                .find(|(id, _)| *id == target)
                .map(|(_, indent)| *indent)
                .unwrap_or_else(|| panic!("node {target:?} not in display"))
        };

        let c_id = id_for(&view, "chain/a/b/c");
        let leaf_id = id_for(&view, "chain/a/b/c/leaf.txt");
        let sibling_id = id_for(&view, "sibling");

        // `c` renders at indent 1 (the depth of its outermost folded
        // ancestor `chain`), even though its raw tree depth is 4.
        assert_eq!(indent_for(c_id), 1);
        // `leaf.txt` sits one level deeper than its visible parent `c`,
        // not at its raw depth of 5.
        assert_eq!(indent_for(leaf_id), 2);
        // `sibling` is unaffected by the chain on the other branch.
        assert_eq!(indent_for(sibling_id), 1);
    }

    #[tokio::test]
    async fn test_compact_chain_breaks_at_file_or_branch() {
        let (_t, mut view) = create_chain_view().await;
        let root_id = view.tree().root_id();
        view.tree_mut().expand_node(root_id).await.unwrap();
        view.expand_with_chain(id_for(&view, "chain"))
            .await
            .unwrap();

        // `c`'s only child is a file (`leaf.txt`), so `c` itself is not
        // absorbed and renders as a normal row with no further chain.
        let c_id = id_for(&view, "chain/a/b/c");
        let visible_names: Vec<String> = view
            .get_display_nodes()
            .into_iter()
            .map(|(id, _)| name_of(&view, id))
            .collect();
        assert!(visible_names.contains(&"c".to_string()));

        // `leaf.txt` (file) carries no chain prefix.
        let leaf_id = id_for(&view, "chain/a/b/c/leaf.txt");
        assert!(view.compact_chain_for_anchor(leaf_id).is_empty());

        // The root never participates in a chain — even when it has just
        // one visible child, it stays at the top of the tree on its own
        // row. Verify by collapsing siblings out of view.
        // (Here we just assert the principle directly: root is never the
        // start of a folded prefix for any other anchor.)
        let prefix_for_c = view.compact_chain_for_anchor(c_id);
        assert!(!prefix_for_c.contains(&root_id));
    }

    #[tokio::test]
    async fn test_compact_disabled_renders_each_node_as_row() {
        let (_t, mut view) = create_chain_view().await;
        let root_id = view.tree().root_id();
        view.tree_mut().expand_node(root_id).await.unwrap();

        // Expand the whole chain with compact mode on, then turn it off.
        // All four directories should now render as separate rows at
        // their raw depths.
        view.expand_with_chain(id_for(&view, "chain"))
            .await
            .unwrap();
        view.set_compact_directories(false);

        let display = view.get_display_nodes();
        let visible_names: Vec<String> =
            display.iter().map(|(id, _)| name_of(&view, *id)).collect();
        assert!(visible_names.contains(&"chain".to_string()));
        assert!(visible_names.contains(&"a".to_string()));
        assert!(visible_names.contains(&"b".to_string()));
        assert!(visible_names.contains(&"c".to_string()));

        // Indents should reflect raw depths since nothing is folded.
        let indent_for = |target: NodeId| {
            display
                .iter()
                .find(|(id, _)| *id == target)
                .map(|(_, indent)| *indent)
                .unwrap()
        };
        assert_eq!(indent_for(id_for(&view, "chain")), 1);
        assert_eq!(indent_for(id_for(&view, "chain/a")), 2);
        assert_eq!(indent_for(id_for(&view, "chain/a/b")), 3);
        assert_eq!(indent_for(id_for(&view, "chain/a/b/c")), 4);

        // No chain prefix is ever produced when compact mode is off.
        assert!(view
            .compact_chain_for_anchor(id_for(&view, "chain/a/b/c"))
            .is_empty());
    }

    #[tokio::test]
    async fn test_expand_with_chain_auto_expands_descendants() {
        let (_t, mut view) = create_chain_view().await;
        let root_id = view.tree().root_id();
        view.tree_mut().expand_node(root_id).await.unwrap();

        let chain_id = id_for(&view, "chain");
        // Pre-condition: `chain` is collapsed.
        assert!(view.tree().get_node(chain_id).unwrap().is_collapsed());

        view.expand_with_chain(chain_id).await.unwrap();

        // Every dir in the chain (including `c`) should now be expanded
        // so their tree nodes exist and are reachable. Without
        // auto-chain-expand the user would have to expand each level
        // manually before the chain could form visually.
        for rel in ["chain", "chain/a", "chain/a/b", "chain/a/b/c"] {
            let id = id_for(&view, rel);
            assert!(
                view.tree().get_node(id).unwrap().is_expanded(),
                "{rel} should be auto-expanded by expand_with_chain"
            );
        }
    }

    #[tokio::test]
    async fn test_toggle_with_chain_collapses_chain_root_only() {
        let (_t, mut view) = create_chain_view().await;
        let root_id = view.tree().root_id();
        view.tree_mut().expand_node(root_id).await.unwrap();

        let chain_id = id_for(&view, "chain");
        // First toggle: expand + auto-reveal chain.
        view.toggle_with_chain(chain_id).await.unwrap();
        assert!(view.tree().get_node(chain_id).unwrap().is_expanded());

        // Second toggle: collapse the chain root. `chain` is now
        // collapsed; its descendants get dropped from the tree (per
        // `collapse_node`'s own contract) so the chain is fully reset.
        view.toggle_with_chain(chain_id).await.unwrap();
        assert!(view.tree().get_node(chain_id).unwrap().is_collapsed());
    }

    #[tokio::test]
    async fn test_toggle_with_chain_keeps_cursor_on_visible_row() {
        // Regression: pressing Enter on a directory whose subtree folds
        // into a chain used to leave the cursor on the now-absorbed
        // directory, so its id was absent from `filtered_visible_nodes()`
        // and arrow-key navigation silently no-op'd.
        let (_t, mut view) = create_chain_view().await;
        let root_id = view.tree().root_id();
        view.tree_mut().expand_node(root_id).await.unwrap();

        let chain_id = id_for(&view, "chain");

        // Cursor sits on `chain` (collapsed → visible) before the user
        // hits Enter.
        view.set_selected(Some(chain_id));
        assert_eq!(view.get_selected(), Some(chain_id));

        // Enter expands the whole chain; `chain`, `a`, `b` become
        // absorbed and `c` is the chain anchor. The cursor must hop to
        // `c` so it lands on a rendered row. (The deeper nodes only
        // exist in the tree after expansion lazily loads them, so look
        // up `c`'s id afterwards.)
        view.toggle_with_chain(chain_id).await.unwrap();
        let c_id = id_for(&view, "chain/a/b/c");
        assert_eq!(view.get_selected(), Some(c_id));

        // Sanity: the new cursor id is actually present in the visible
        // rows, so `select_next`/`select_prev` can locate it.
        let visible: Vec<_> = view
            .get_display_nodes()
            .into_iter()
            .map(|(id, _)| id)
            .collect();
        assert!(visible.contains(&c_id));

        // And arrow-key navigation still works after the expansion.
        view.select_next();
        assert_ne!(view.get_selected(), Some(c_id));
    }

    #[tokio::test]
    async fn test_set_selected_promotes_absorbed_node_to_anchor() {
        let (_t, mut view) = create_chain_view().await;
        let root_id = view.tree().root_id();
        view.tree_mut().expand_node(root_id).await.unwrap();
        view.expand_with_chain(id_for(&view, "chain"))
            .await
            .unwrap();

        // `chain` is absorbed into `c`'s row. Selecting `chain` should
        // land the cursor on `c` so the cursor always sits on a
        // rendered row.
        let chain_id = id_for(&view, "chain");
        let c_id = id_for(&view, "chain/a/b/c");
        view.set_selected(Some(chain_id));
        assert_eq!(view.get_selected(), Some(c_id));
    }

    /// Each segment of a compact `chain/a/b/c` row carries the directory it
    /// names, so a press on one can be resolved by path.
    #[tokio::test]
    async fn each_segment_of_a_compact_row_carries_its_own_directory() {
        let (_t, mut view) = create_chain_view().await;
        let root_id = view.tree().root_id();
        view.tree_mut().expand_node(root_id).await.unwrap();
        view.expand_with_chain(id_for(&view, "chain"))
            .await
            .unwrap();

        let projection = view.projection();
        let c_id = id_for(&view, "chain/a/b/c");
        let row = &projection.rows[projection.index_of(c_id).expect("a row")];
        let root = view.tree().root_path();
        let got: Vec<(&str, &std::path::Path)> = row
            .chain
            .iter()
            .map(|seg| (seg.name.as_str(), seg.path.strip_prefix(root).unwrap()))
            .collect();
        assert_eq!(
            got,
            vec![
                ("chain", std::path::Path::new("chain")),
                ("a", std::path::Path::new("chain/a")),
                ("b", std::path::Path::new("chain/a/b")),
            ]
        );
    }

    /// A cursor on a segment joins the multi-selection as the row it is drawn
    /// on. An absorbed id in that set would highlight nothing and act on
    /// nothing: Space then Delete reported "cannot delete the project root".
    #[tokio::test]
    async fn a_segment_cursor_joins_the_multi_selection_as_its_row() {
        let (_t, mut view) = create_chain_view().await;
        let root_id = view.tree().root_id();
        view.tree_mut().expand_node(root_id).await.unwrap();
        view.expand_with_chain(id_for(&view, "chain"))
            .await
            .unwrap();

        let chain_id = id_for(&view, "chain");
        let c_id = id_for(&view, "chain/a/b/c");
        let leaf_id = id_for(&view, "chain/a/b/c/leaf.txt");

        view.set_selected_exact(Some(chain_id));
        view.toggle_select();
        assert!(view.has_multi_selection());
        assert_eq!(
            view.effective_selection(),
            vec![c_id],
            "the row the cursor is drawn on, not an id no row carries"
        );

        // And a range extended from such a cursor keeps the row it started on:
        // the anchor has to be findable in the visible order for the range to
        // span anything.
        view.clear_multi_selection();
        view.set_selected_exact(Some(chain_id));
        view.extend_selection_down();
        assert_eq!(view.effective_selection(), vec![c_id, leaf_id]);
    }

    /// A cursor on a segment stays on that segment, while the row drawn with
    /// the highlight is still the anchor's — the row the segment is on.
    #[tokio::test]
    async fn a_cursor_on_a_chain_segment_is_drawn_on_the_chains_row() {
        let (_t, mut view) = create_chain_view().await;
        let root_id = view.tree().root_id();
        view.tree_mut().expand_node(root_id).await.unwrap();
        view.expand_with_chain(id_for(&view, "chain"))
            .await
            .unwrap();

        let chain_id = id_for(&view, "chain");
        let c_id = id_for(&view, "chain/a/b/c");
        view.set_selected_exact(Some(chain_id));
        assert_eq!(view.get_selected(), Some(chain_id), "the segment itself");
        assert_eq!(
            view.get_selected_index(),
            view.projection().index_of(c_id),
            "drawn on the row that renders the chain"
        );
    }

    /// And the keys still step off that row, rather than failing to find a
    /// cursor with no row of its own and leaving it where it was.
    #[tokio::test]
    async fn the_keys_step_off_the_row_a_segment_cursor_is_drawn_on() {
        let (_t, mut view) = create_chain_view().await;
        let root_id = view.tree().root_id();
        view.tree_mut().expand_node(root_id).await.unwrap();
        view.expand_with_chain(id_for(&view, "chain"))
            .await
            .unwrap();

        let chain_id = id_for(&view, "chain");
        let leaf_id = id_for(&view, "chain/a/b/c/leaf.txt");
        view.set_selected_exact(Some(chain_id));
        view.select_next();
        assert_eq!(
            view.get_selected(),
            Some(leaf_id),
            "the row below the chain"
        );

        view.set_selected_exact(Some(chain_id));
        view.select_prev();
        assert_eq!(view.get_selected(), Some(root_id), "the row above it");
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn test_collect_symlink_mappings_includes_collapsed_visible_symlink_dirs() {
        use std::os::unix::fs::symlink;

        let temp_dir = TempDir::new().unwrap();
        let root = temp_dir.path();
        let real_dir = root.join("real");
        std_fs::create_dir_all(real_dir.join("nested")).unwrap();
        std_fs::write(real_dir.join("nested/file.txt"), "hello").unwrap();
        let link_dir = root.join("link");
        symlink(&real_dir, &link_dir).unwrap();

        let backend = Arc::new(StdFileSystem);
        let manager = Arc::new(FsManager::new(backend));
        let mut tree = FileTree::new(root.to_path_buf(), manager).await.unwrap();
        let root_id = tree.root_id();
        tree.expand_node(root_id).await.unwrap();
        let view = FileTreeView::new(tree);

        let mappings = view.collect_symlink_mappings();
        let canonical_real_dir = real_dir.canonicalize().unwrap();
        assert_eq!(mappings.get(&link_dir), Some(&canonical_real_dir));
    }

    #[tokio::test]
    async fn test_select_parent_skips_absorbed_ancestors() {
        let (_t, mut view) = create_chain_view().await;
        let root_id = view.tree().root_id();
        view.tree_mut().expand_node(root_id).await.unwrap();
        view.expand_with_chain(id_for(&view, "chain"))
            .await
            .unwrap();

        // Cursor on `leaf.txt` → its raw parent is `c` (visible), so
        // select_parent lands on `c`.
        let leaf_id = id_for(&view, "chain/a/b/c/leaf.txt");
        let c_id = id_for(&view, "chain/a/b/c");
        view.set_selected(Some(leaf_id));
        view.select_parent();
        assert_eq!(view.get_selected(), Some(c_id));

        // From `c`, the immediate parent `b` is absorbed (and so are
        // `a` and `chain`). select_parent must skip them all and land on
        // the root, the next non-absorbed ancestor.
        view.select_parent();
        assert_eq!(view.get_selected(), Some(root_id));
    }

    #[tokio::test]
    async fn test_compact_visible_count_matches_display_rows() {
        let (_t, mut view) = create_chain_view().await;
        let root_id = view.tree().root_id();
        view.tree_mut().expand_node(root_id).await.unwrap();
        view.expand_with_chain(id_for(&view, "chain"))
            .await
            .unwrap();

        // Rows: root, c (chain anchor), leaf.txt, sibling — 4 in total.
        // `chain`, `a`, `b` are folded into `c`'s row and don't count.
        // `sibling` stays collapsed so its file child is not visible.
        assert_eq!(view.visible_count(), 4);
        assert_eq!(view.get_display_nodes().len(), 4);
    }
}
