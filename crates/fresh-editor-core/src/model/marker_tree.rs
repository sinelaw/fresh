use std::cell::RefCell;
use std::cmp::{max, Ordering};
use std::collections::HashMap;
use std::mem;
use std::rc::{Rc, Weak};

/// Use a simple u64 for marker IDs
pub type MarkerId = u64;

// ---
// 1. Core Data Structures and Pointers
// ---

#[derive(Debug, Clone, PartialEq)]
pub struct Interval {
    pub start: u64,
    pub end: u64,
}

/// Type of marker - either a position marker or a line anchor
#[derive(Debug, Clone, PartialEq)]
pub enum MarkerType {
    /// Regular position marker (for overlays, cursors, etc.)
    Position,
    /// Line anchor with estimated/exact line number
    LineAnchor {
        estimated_line: usize,
        confidence: AnchorConfidence,
    },
}

/// Confidence level for line anchor estimates
#[derive(Debug, Clone, PartialEq)]
pub enum AnchorConfidence {
    /// Exact line number (scanned from known position)
    Exact,
    /// Estimated from average line length
    Estimated,
    /// Relative to another anchor
    Relative(MarkerId),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Marker {
    pub id: MarkerId,
    pub interval: Interval,
    pub marker_type: MarkerType,
    /// Insertion gravity at the marker's exact position.
    /// - `true` (right gravity, default): text inserted exactly at the marker
    ///   position pushes the marker forward (the marker moves after the text).
    /// - `false` (left gravity): text inserted exactly at the marker position
    ///   leaves it in place (the marker stays before the text).
    ///
    /// Only matters for insertions landing on the marker's own position;
    /// insertions strictly before always shift it, insertions strictly after
    /// never do. Used to keep search-match highlights from growing when text
    /// is typed immediately after a match (issue #2053).
    pub right_gravity: bool,
}

/// A Strong pointer to a tree node (child/sibling/map reference)
type NodePtr = Option<Rc<RefCell<Node>>>;
/// A Weak pointer to a tree node (parent reference, doesn't count for ownership)
type WeakNodePtr = Weak<RefCell<Node>>;

/// The internal tree node
#[derive(Debug)]
struct Node {
    pub marker: Marker,

    /// AVL: Height of this node's subtree
    pub height: i32,
    /// Augmentation: The max 'end' value in this node's subtree
    pub max_end: u64,
    /// VSCode-style: The delta to be applied to this node and its children
    pub lazy_delta: i64,

    pub parent: WeakNodePtr,
    pub left: NodePtr,
    pub right: NodePtr,
}

/// The main Interval Tree structure
#[derive(Debug, Default)]
pub struct IntervalTree {
    root: NodePtr,
    next_id: u64,
    /// ID-to-Node map for O(1) lookups
    marker_map: HashMap<MarkerId, Rc<RefCell<Node>>>,
}

// ---
// 2. Node Helpers (Pushing Deltas, Stats, Heights)
// ---

impl Node {
    fn new(marker: Marker, parent: WeakNodePtr) -> Rc<RefCell<Self>> {
        // Fix E0382: Calculate max_end before moving ownership of `marker` into the struct.
        let max_end_val = marker.interval.end;

        Rc::new(RefCell::new(Node {
            marker,
            height: 1,
            max_end: max_end_val,
            lazy_delta: 0,
            parent,
            left: None,
            right: None,
        }))
    }

    /// Gets the height of a node (0 for None).
    fn height(node: &NodePtr) -> i32 {
        node.as_ref().map_or(0, |n| n.borrow().height)
    }

    /// Calculates the balance factor of a node (height(left) - height(right)).
    fn balance_factor(node: &Rc<RefCell<Self>>) -> i32 {
        let n = node.borrow();
        Self::height(&n.left) - Self::height(&n.right)
    }

    /// Pushes this node's lazy_delta down to its immediate children.
    fn push_delta(node_rc: &Rc<RefCell<Self>>) {
        let mut node = node_rc.borrow_mut();
        if node.lazy_delta == 0 {
            return;
        }

        let delta = node.lazy_delta;

        // Apply delta to self (start and end)
        node.marker.interval.start = (node.marker.interval.start as i64 + delta) as u64;
        node.marker.interval.end = (node.marker.interval.end as i64 + delta) as u64;

        // Apply delta to children (only update their lazy_delta fields)
        if let Some(ref left) = node.left {
            left.borrow_mut().lazy_delta += delta;
        }
        if let Some(ref right) = node.right {
            right.borrow_mut().lazy_delta += delta;
        }

        node.lazy_delta = 0;

        // The max_end needs to be updated after the push
        let max_l = node.left.as_ref().map_or(0, |l| l.borrow().max_end);
        let max_r = node.right.as_ref().map_or(0, |r| r.borrow().max_end);
        node.max_end = max(node.marker.interval.end, max(max_l, max_r));
    }

    /// Updates a node's height and max_end based on its children.
    fn update_stats(node: &Rc<RefCell<Self>>) {
        let mut n = node.borrow_mut();
        let height_l = Self::height(&n.left);
        let height_r = Self::height(&n.right);

        n.height = 1 + max(height_l, height_r);

        let max_l = n.left.as_ref().map_or(0, |l| l.borrow().max_end);
        let max_r = n.right.as_ref().map_or(0, |r| r.borrow().max_end);
        n.max_end = max(n.marker.interval.end, max(max_l, max_r));
    }
}

// ---
// 3. Main Public API
// ---

impl IntervalTree {
    /// Below this many markers in one batch, the descent-per-insert path is
    /// cheaper than collecting and re-sorting the tree.
    const BULK_INSERT_MIN: usize = 64;
    /// ...and so it is whenever the tree already holds much more than the
    /// batch adds, since the bulk build's cost is in the total, not the batch.
    const BULK_INSERT_MAX_EXISTING_RATIO: usize = 4;

    pub fn new() -> Self {
        Self::default()
    }

    /// Inserts a new marker interval. Performance: O(log n)
    pub fn insert(&mut self, start: u64, end: u64) -> MarkerId {
        self.insert_with_type(start, end, MarkerType::Position)
    }

    /// Inserts a new left-gravity marker interval: text inserted exactly at the
    /// marker's position leaves it in place rather than pushing it forward.
    /// Performance: O(log n)
    pub fn insert_left_gravity(&mut self, start: u64, end: u64) -> MarkerId {
        self.insert_full(start, end, MarkerType::Position, false)
    }

    /// Insert a marker with a specific ID and type (for set_position).
    /// The caller must ensure the ID is not already in use.
    fn insert_with_id(
        &mut self,
        id: MarkerId,
        start: u64,
        end: u64,
        marker_type: MarkerType,
        right_gravity: bool,
    ) {
        debug_assert!(
            id < self.next_id,
            "insert_with_id: id {} must be < next_id {}",
            id,
            self.next_id
        );
        debug_assert!(
            !self.marker_map.contains_key(&id),
            "insert_with_id: id {} already in use",
            id
        );
        let marker = Marker {
            id,
            interval: Interval { start, end },
            marker_type,
            right_gravity,
        };

        let new_node = Node::new(marker.clone(), Weak::new());
        self.root = Self::insert_recursive(self.root.take(), new_node.clone());
        self.marker_map.insert(id, new_node);
    }

    /// Insert a marker with a specific type (right gravity).
    pub fn insert_with_type(&mut self, start: u64, end: u64, marker_type: MarkerType) -> MarkerId {
        self.insert_full(start, end, marker_type, true)
    }

    /// Insert a marker with a specific type and gravity.
    fn insert_full(
        &mut self,
        start: u64,
        end: u64,
        marker_type: MarkerType,
        right_gravity: bool,
    ) -> MarkerId {
        let id = self.next_id;
        self.next_id += 1;
        let marker = Marker {
            id,
            interval: Interval { start, end },
            marker_type,
            right_gravity,
        };

        let new_node = Node::new(marker.clone(), Weak::new());
        self.root = Self::insert_recursive(self.root.take(), new_node.clone());

        self.marker_map.insert(id, new_node);
        id
    }

    /// Insert many right-gravity position markers in one balanced build.
    ///
    /// Equivalent to calling [`insert`](Self::insert) on each interval in
    /// turn: ids are handed out in argument order, so the result is
    /// indistinguishable from the one-at-a-time path. What changes is how the
    /// tree gets there — bottom-up from a sorted slice, instead of N
    /// independent root-to-leaf descents each rebalancing on the way back up.
    ///
    /// Worth it when a single operation installs tens of thousands of markers
    /// at once, which is what replacing the contents of a decorated buffer
    /// does: one overlay per row, three markers per overlay.
    pub fn insert_many(&mut self, intervals: &[(u64, u64)]) -> Vec<MarkerId> {
        if intervals.is_empty() {
            return Vec::new();
        }
        // The bulk build has to collect and re-sort everything already in the
        // tree, so it only pays off on a batch that is both large in absolute
        // terms and large relative to what is already there.
        if intervals.len() < Self::BULK_INSERT_MIN
            || self.marker_map.len() > intervals.len() * Self::BULK_INSERT_MAX_EXISTING_RATIO
        {
            return intervals
                .iter()
                .map(|&(start, end)| self.insert(start, end))
                .collect();
        }

        // Existing nodes are reused rather than rebuilt, so every live
        // MarkerId — and the `marker_map` entry behind it — stays valid.
        let mut nodes: Vec<Rc<RefCell<Node>>> =
            Vec::with_capacity(self.marker_map.len() + intervals.len());
        Self::collect_nodes_in_order(&self.root, &mut nodes);
        for node in &nodes {
            let mut n = node.borrow_mut();
            // `collect_nodes_in_order` resolved every lazy delta on the way in,
            // so the intervals here are the true ones.
            debug_assert_eq!(n.lazy_delta, 0);
            n.left = None;
            n.right = None;
            n.parent = Weak::new();
        }

        let mut ids = Vec::with_capacity(intervals.len());
        for &(start, end) in intervals {
            let id = self.next_id;
            self.next_id += 1;
            let node = Node::new(
                Marker {
                    id,
                    interval: Interval { start, end },
                    marker_type: MarkerType::Position,
                    right_gravity: true,
                },
                Weak::new(),
            );
            self.marker_map.insert(id, Rc::clone(&node));
            nodes.push(node);
            ids.push(id);
        }

        nodes.sort_unstable_by_key(|n| {
            let n = n.borrow();
            (n.marker.interval.start, n.marker.id)
        });
        self.root = Self::build_from_sorted(&nodes);
        ids
    }

    /// Insert a line anchor at a specific position
    pub fn insert_line_anchor(
        &mut self,
        start: u64,
        end: u64,
        estimated_line: usize,
        confidence: AnchorConfidence,
    ) -> MarkerId {
        self.insert_with_type(
            start,
            end,
            MarkerType::LineAnchor {
                estimated_line,
                confidence,
            },
        )
    }

    /// Finds the current true position of a marker by its ID. Performance: O(log n)
    pub fn get_position(&self, id: MarkerId) -> Option<(u64, u64)> {
        let node_rc = self.marker_map.get(&id)?;
        let mut node_opt = Some(Rc::clone(node_rc));
        let mut current_delta: i64 = 0;

        // Walk up the tree, collecting all deltas that haven't been applied yet.
        while let Some(current_rc) = node_opt {
            let current = current_rc.borrow();

            // Add this node's delta (if any)
            current_delta += current.lazy_delta;

            // Move up to the parent
            node_opt = current.parent.upgrade();
        }

        let raw_marker = node_rc.borrow().marker.interval.clone();

        let start = (raw_marker.start as i64 + current_delta) as u64;
        let end = (raw_marker.end as i64 + current_delta) as u64;

        Some((start, end))
    }

    /// Deletes a marker by its ID. Performance: O(log n)
    ///
    /// Locates the node via `marker_map` and removes it using parent pointers
    /// rather than a `(start, id)` key search. Edits can transiently leave the
    /// tree position-ordered but *not* `(start, id)`-ordered — e.g. a deletion
    /// clamps two markers to the same position, and their ids contradict the
    /// order they reached that position. A key-routed delete would then take a
    /// wrong turn and silently fail to remove the node; identity-based removal
    /// is immune to that.
    pub fn delete(&mut self, id: MarkerId) -> bool {
        let node_rc = match self.marker_map.get(&id) {
            Some(n) => Rc::clone(n),
            None => return false,
        };

        // Flush pending lazy deltas from the root down to this node so the node
        // (and every ancestor) holds its true interval with no pending ancestor
        // delta, making structural surgery safe.
        Self::push_to_node(&node_rc);

        // Decide which physical node to splice out. With two children we swap
        // this node's marker with its in-order successor (which has no left
        // child) and remove the successor's node instead.
        let two_children = {
            let n = node_rc.borrow();
            n.left.is_some() && n.right.is_some()
        };

        let remove_rc = if two_children {
            let right = node_rc.borrow().right.as_ref().unwrap().clone();
            let succ = Self::min_node(&right);
            let succ_id = succ.borrow().marker.id;

            mem::swap(
                &mut node_rc.borrow_mut().marker,
                &mut succ.borrow_mut().marker,
            );
            // `node_rc` now carries the successor's marker; redirect the map.
            self.marker_map.insert(succ_id, Rc::clone(&node_rc));
            succ
        } else {
            Rc::clone(&node_rc)
        };

        // `remove_rc` now has at most one child. Splice that child (if any) into
        // remove_rc's slot under its parent.
        let child = {
            let mut rb = remove_rc.borrow_mut();
            rb.left.take().or_else(|| rb.right.take())
        };
        let parent = remove_rc.borrow().parent.upgrade();
        if let Some(ref ch) = child {
            ch.borrow_mut().parent = remove_rc.borrow().parent.clone();
        }
        match parent {
            None => self.root = child,
            Some(ref p) => {
                let is_left = p
                    .borrow()
                    .left
                    .as_ref()
                    .is_some_and(|l| Rc::ptr_eq(l, &remove_rc));
                if is_left {
                    p.borrow_mut().left = child;
                } else {
                    p.borrow_mut().right = child;
                }
            }
        }

        self.marker_map.remove(&id);

        // Rebalance from the splice point up to the root.
        self.rebalance_upward(parent);
        true
    }

    /// Pushes pending lazy deltas from the root down to (and including)
    /// `node_rc`, so the node holds its true interval and all ancestors on the
    /// path have a zero `lazy_delta`.
    fn push_to_node(node_rc: &Rc<RefCell<Node>>) {
        let mut path: Vec<Rc<RefCell<Node>>> = Vec::new();
        let mut cur = Some(Rc::clone(node_rc));
        while let Some(c) = cur {
            let parent = c.borrow().parent.upgrade();
            path.push(c);
            cur = parent;
        }
        for n in path.into_iter().rev() {
            Node::push_delta(&n);
        }
    }

    /// Walks from `start` up to the root, refreshing stats and AVL-balancing
    /// each node, fixing the parent links and `self.root` as subtrees rotate.
    fn rebalance_upward(&mut self, start: NodePtr) {
        let mut cur = start;
        while let Some(n) = cur {
            let parent = n.borrow().parent.upgrade();
            Node::update_stats(&n);
            let new_sub = Self::balance(Rc::clone(&n));
            match &parent {
                None => {
                    if let Some(ref ns) = new_sub {
                        ns.borrow_mut().parent = Weak::new();
                    }
                    self.root = new_sub;
                }
                Some(p) => {
                    let is_left = p.borrow().left.as_ref().is_some_and(|l| Rc::ptr_eq(l, &n));
                    if let Some(ref ns) = new_sub {
                        ns.borrow_mut().parent = Rc::downgrade(p);
                    }
                    if is_left {
                        p.borrow_mut().left = new_sub;
                    } else {
                        p.borrow_mut().right = new_sub;
                    }
                }
            }
            cur = parent;
        }
    }

    /// Move a marker to a new position, preserving its ID and type.
    /// Implemented as delete + reinsert with the same ID.
    /// Returns false if the marker doesn't exist.
    /// Performance: O(log n)
    pub fn set_position(&mut self, id: MarkerId, new_start: u64, new_end: u64) -> bool {
        // Get the marker's type and gravity before deleting
        let (marker_type, right_gravity) = match self.get_marker(id) {
            Some(m) => (m.marker_type, m.right_gravity),
            None => return false,
        };

        // Delete from tree
        if !self.delete(id) {
            return false;
        }

        // Reinsert with same ID
        self.insert_with_id(id, new_start, new_end, marker_type, right_gravity);
        true
    }

    /// Adjusts all markers for a batch of non-overlapping edits in one pass.
    ///
    /// `edits` are `(pos, delta)` pairs in the same net-delta form
    /// [`adjust_for_edit`](Self::adjust_for_edit) takes, in any order. The
    /// outcome matches applying them one at a time from the highest position
    /// down — the order a bulk edit uses — but costs O(n + m log m) for `n`
    /// markers and `m` edits instead of O(n·m).
    ///
    /// The per-edit path cannot do better: a deletion has to visit every marker
    /// at or after its position so each one can clamp into the deleted range,
    /// which made a replace-all (one edit per match, one marker per match)
    /// quadratic and effectively non-terminating (issue #2893).
    ///
    /// Edits must not overlap — deleted ranges are disjoint, which is what
    /// `apply_bulk_edits` requires of them anyway — and no two may share a
    /// position: each marker is resolved against the single edit governing it,
    /// which cannot express two edits applying in turn at the same spot. A
    /// bulk edit merges its same-position edits before this point.
    pub fn adjust_for_bulk_edits(&mut self, edits: &[(u64, i64)]) {
        if self.root.is_none() {
            return;
        }

        // Edits ascending by position, alongside the summed delta of every
        // edit strictly before each one. A marker is shifted by its own
        // governing edit (the last one at or before it) plus that prefix.
        let mut ordered: Vec<(u64, i64)> = edits.iter().copied().filter(|(_, d)| *d != 0).collect();
        if ordered.is_empty() {
            return;
        }
        ordered.sort_unstable_by_key(|(pos, _)| *pos);
        debug_assert!(
            ordered.windows(2).all(|w| w[0].0 != w[1].0),
            "adjust_for_bulk_edits needs one edit per position, got {ordered:?}"
        );
        let mut prefix: Vec<i64> = Vec::with_capacity(ordered.len());
        let mut running: i64 = 0;
        for (_, delta) in &ordered {
            prefix.push(running);
            running += delta;
        }

        // Collect every node with its true (delta-resolved) interval, then
        // rewrite the intervals in place. The nodes themselves are reused so
        // `marker_map` and every MarkerId stay valid.
        let mut nodes: Vec<Rc<RefCell<Node>>> = Vec::with_capacity(self.marker_map.len());
        Self::collect_nodes_in_order(&self.root, &mut nodes);

        for node_rc in &nodes {
            let mut node = node_rc.borrow_mut();
            let left_gravity = !node.marker.right_gravity;
            let (old_start, old_end) = (
                node.marker.interval.start,
                node.marker.interval.end.max(node.marker.interval.start),
            );

            // Edits at or before the start govern both coordinates; edits
            // strictly inside the interval move only the end, and each one
            // shrinks it against the (unmoved) start, so they are folded
            // individually. For a point marker that window is empty.
            let governing = ordered.partition_point(|(pos, _)| *pos <= old_start);
            let inside_end = ordered.partition_point(|(pos, _)| *pos <= old_end);

            let mut end = old_end as i64;
            for &(pos, delta) in ordered[governing..inside_end].iter().rev() {
                let shifts = if left_gravity && delta > 0 {
                    end > pos as i64
                } else {
                    end >= pos as i64
                };
                if shifts {
                    end = (end + delta).max(old_start as i64);
                }
            }

            let head = &ordered[..governing];
            let head_prefix = &prefix[..governing];
            let start = Self::map_coord(old_start, left_gravity, head, head_prefix);
            // Mirrors the per-edit path: an end never precedes its own start.
            let end =
                Self::map_coord(end.max(0) as u64, left_gravity, head, head_prefix).max(start);
            node.marker.interval.start = start;
            node.marker.interval.end = end;
            node.lazy_delta = 0;
            node.left = None;
            node.right = None;
            node.parent = Weak::new();
        }

        // Deletions can clamp several markers onto the same position, so the
        // pre-edit order is not necessarily the post-edit `(start, id)` order.
        // Re-sorting and rebuilding restores the BST invariant outright.
        nodes.sort_unstable_by_key(|n| {
            let n = n.borrow();
            (n.marker.interval.start, n.marker.id)
        });
        self.root = Self::build_from_sorted(&nodes);
    }

    /// Maps one coordinate through the batch, applying the same rules as
    /// [`adjust_recursive`](Self::adjust_recursive): a coordinate at or after
    /// an edit shifts by its delta (strictly after, for a left-gravity marker
    /// facing an insertion) and clamps to the edit position when the deletion
    /// swallows it.
    fn map_coord(coord: u64, left_gravity: bool, edits: &[(u64, i64)], prefix: &[i64]) -> u64 {
        let idx = edits.partition_point(|(pos, _)| *pos <= coord);
        if idx == 0 {
            return coord;
        }
        let (pos, delta) = edits[idx - 1];
        let base = if delta > 0 {
            if left_gravity && coord == pos {
                coord as i64
            } else {
                coord as i64 + delta
            }
        } else {
            (coord as i64 + delta).max(pos as i64)
        };
        (base + prefix[idx - 1]).max(0) as u64
    }

    /// Collects every node in position order, resolving lazy deltas on the way
    /// down so each node's stored interval is its true one.
    fn collect_nodes_in_order(node: &NodePtr, out: &mut Vec<Rc<RefCell<Node>>>) {
        let Some(n) = node else { return };
        Node::push_delta(n);
        let (left, right) = {
            let borrowed = n.borrow();
            (borrowed.left.clone(), borrowed.right.clone())
        };
        Self::collect_nodes_in_order(&left, out);
        out.push(Rc::clone(n));
        Self::collect_nodes_in_order(&right, out);
    }

    /// Rebuilds a balanced tree from nodes already sorted by `(start, id)`,
    /// fixing parent links, heights and `max_end` as it goes.
    fn build_from_sorted(nodes: &[Rc<RefCell<Node>>]) -> NodePtr {
        if nodes.is_empty() {
            return None;
        }
        let mid = nodes.len() / 2;
        let node = Rc::clone(&nodes[mid]);
        let left = Self::build_from_sorted(&nodes[..mid]);
        let right = Self::build_from_sorted(&nodes[mid + 1..]);
        if let Some(ref l) = left {
            l.borrow_mut().parent = Rc::downgrade(&node);
        }
        if let Some(ref r) = right {
            r.borrow_mut().parent = Rc::downgrade(&node);
        }
        {
            let mut n = node.borrow_mut();
            n.left = left;
            n.right = right;
        }
        Node::update_stats(&node);
        Some(node)
    }

    /// Adjusts all markers for a text edit (insertion or deletion).
    /// Performance: O(log n) due to lazy delta propagation.
    pub fn adjust_for_edit(&mut self, pos: u64, delta: i64) {
        // Special case: an insertion landing exactly on a position shared by a
        // left-gravity marker (which stays put) and a right-gravity marker
        // (which moves forward). The two markers' relative order is *reversed*
        // by the edit, but this tree is a positional BST keyed on `(start, id)`
        // and ordered when each marker was inserted — it cannot represent that
        // reversal in place. Leaving it would corrupt the BST invariant and
        // make later start-keyed traversals (adjust/delete/query) misroute,
        // silently dropping edits to the displaced markers.
        //
        // To keep the invariant intact: pull the left-gravity "stayers" out of
        // the tree first (while ordering is still valid so delete can find
        // them), shift everything else, then re-insert the stayers at their
        // correct post-edit interval so the BST is rebuilt in proper order.
        // Only needed when a co-located right-gravity marker actually moves;
        // otherwise the in-place adjust already keeps stayers correctly placed.
        if delta > 0 {
            // Collect every marker whose start is exactly `pos` by descending
            // the BST on `start` alone (no reliance on the `max_end`
            // augmentation, which can be transiently stale under lazy-delta
            // propagation). The tree is position-ordered here, so this is a
            // reliable O(log n + k) lookup.
            let mut at_pos: Vec<(MarkerId, u64, bool, MarkerType)> = Vec::new();
            Self::collect_starts_at(&self.root, 0, pos, &mut at_pos);

            let has_mover = at_pos.iter().any(|(_, _, rg, _)| *rg);
            let stayers: Vec<(MarkerId, u64, bool, MarkerType)> =
                at_pos.into_iter().filter(|(_, _, rg, _)| !*rg).collect();

            if has_mover && !stayers.is_empty() {
                for (id, _, _, _) in &stayers {
                    self.delete(*id);
                }
                Self::adjust_recursive(&mut self.root, pos, delta);
                for (id, end, _rg, mtype) in stayers {
                    // Left-gravity: the start stays at `pos`; the end shifts only
                    // if it is strictly after `pos`, mirroring the gravity logic
                    // in adjust_recursive.
                    let new_end = if end > pos {
                        (end as i64 + delta) as u64
                    } else {
                        end
                    };
                    self.insert_with_id(id, pos, new_end, mtype, false);
                }
                return;
            }
        }

        Self::adjust_recursive(&mut self.root, pos, delta);
    }

    /// Collects `(id, true_end, right_gravity, marker_type)` for every marker
    /// whose true start equals `pos`. Read-only descent that accumulates lazy
    /// deltas manually and routes purely on `start`; relies only on the BST's
    /// position ordering, not on `max_end`.
    fn collect_starts_at(
        node: &NodePtr,
        acc_delta: i64,
        pos: u64,
        out: &mut Vec<(MarkerId, u64, bool, MarkerType)>,
    ) {
        let Some(n) = node else { return };
        let nb = n.borrow();
        let d = acc_delta + nb.lazy_delta;
        let start = (nb.marker.interval.start as i64 + d) as u64;
        match pos.cmp(&start) {
            Ordering::Less => Self::collect_starts_at(&nb.left, d, pos, out),
            Ordering::Greater => Self::collect_starts_at(&nb.right, d, pos, out),
            Ordering::Equal => {
                let end = (nb.marker.interval.end as i64 + d) as u64;
                out.push((
                    nb.marker.id,
                    end,
                    nb.marker.right_gravity,
                    nb.marker.marker_type.clone(),
                ));
                // Equal-start markers can sit in either subtree (BST tie-break
                // by id), so search both.
                Self::collect_starts_at(&nb.left, d, pos, out);
                Self::collect_starts_at(&nb.right, d, pos, out);
            }
        }
    }

    /// Finds all markers that overlap a given query range.
    /// Performance: O(log n + k)
    pub fn query(&self, query_start: u64, query_end: u64) -> Vec<Marker> {
        let mut results = Vec::new();
        Self::query_recursive(&self.root, query_start, query_end, &mut results);
        results
    }

    /// Get the marker data for a given marker ID
    pub fn get_marker(&self, id: MarkerId) -> Option<Marker> {
        let node_rc = self.marker_map.get(&id)?;
        Some(node_rc.borrow().marker.clone())
    }

    /// Update a line anchor's estimated line number and confidence
    pub fn update_line_anchor(
        &mut self,
        id: MarkerId,
        estimated_line: usize,
        confidence: AnchorConfidence,
    ) -> bool {
        if let Some(node_rc) = self.marker_map.get(&id) {
            let mut node = node_rc.borrow_mut();
            node.marker.marker_type = MarkerType::LineAnchor {
                estimated_line,
                confidence,
            };
            true
        } else {
            false
        }
    }

    /// Query only line anchors in a range
    pub fn query_line_anchors(&self, query_start: u64, query_end: u64) -> Vec<Marker> {
        self.query(query_start, query_end)
            .into_iter()
            .filter(|m| matches!(m.marker_type, MarkerType::LineAnchor { .. }))
            .collect()
    }
}

// ---
// 4. Recursive Implementation Details (Insert, Delete, Adjust)
// ---

impl IntervalTree {
    /// Recursive helper for insert
    fn insert_recursive(root: NodePtr, new_node: Rc<RefCell<Node>>) -> NodePtr {
        // Remove unnecessary 'mut'
        let root = match root {
            Some(r) => r,
            None => return Some(new_node),
        };

        Node::push_delta(&root);

        let (start, id) = (
            new_node.borrow().marker.interval.start,
            new_node.borrow().marker.id,
        );

        let mut root_mut = root.borrow_mut();
        let (root_start, root_id) = (root_mut.marker.interval.start, root_mut.marker.id);

        if start < root_start || (start == root_start && id < root_id) {
            root_mut.left = Self::insert_recursive(root_mut.left.take(), Rc::clone(&new_node));
            root_mut.left.as_ref().unwrap().borrow_mut().parent = Rc::downgrade(&root);
        } else {
            root_mut.right = Self::insert_recursive(root_mut.right.take(), Rc::clone(&new_node));
            root_mut.right.as_ref().unwrap().borrow_mut().parent = Rc::downgrade(&root);
        }

        drop(root_mut);
        Node::update_stats(&root);
        Self::balance(root)
    }

    /// Finds the minimum node in a subtree (for deletion)
    fn min_node(node_rc: &Rc<RefCell<Node>>) -> Rc<RefCell<Node>> {
        let mut current = Rc::clone(node_rc);
        loop {
            Node::push_delta(&current);

            // Fix E0506: Clone the next node pointer before the borrow (Ref<Node>) on
            // `current` is dropped and potentially prevents reassignment.
            let next_left_opt = current.borrow().left.clone();

            if let Some(next) = next_left_opt {
                current = next;
            } else {
                break current;
            }
        }
    }

    /// CORRECTED Recursive helper for `adjust_for_edit` (O(log n) lazy update)
    fn adjust_recursive(node_opt: &mut NodePtr, pos: u64, delta: i64) {
        let node_rc = match node_opt {
            Some(n) => n,
            None => return,
        };

        Node::push_delta(node_rc);

        let mut node = node_rc.borrow_mut();
        let start = node.marker.interval.start;
        // Left-gravity markers stay put when text is inserted exactly at their
        // position (the insertion goes after them); right-gravity markers (the
        // default) are pushed forward. Gravity only changes behaviour for
        // insertions (delta > 0) landing exactly on the boundary.
        let left_gravity = !node.marker.right_gravity;

        if pos <= start {
            // CASE 1: Edit is at or before this node's start.
            // This node and everything to its right must be shifted.

            // Whether this node's own start should shift. A left-gravity marker
            // does NOT move when the insertion lands exactly on it.
            let stay_put = left_gravity && delta > 0 && pos == start;

            // 1. Shift the current node's start position directly, clamping at `pos` if needed.
            if !stay_put {
                if delta < 0 {
                    node.marker.interval.start = (start as i64 + delta).max(pos as i64) as u64;
                } else {
                    node.marker.interval.start = (start as i64 + delta) as u64;
                }
            }

            // 2. Handle the right subtree.
            // For insertions strictly before this node's start, every node to
            // the right has start > pos and shifts uniformly, so lazy
            // propagation is safe and efficient. When the insertion lands
            // exactly on this node's start (pos == start), the right subtree may
            // contain other markers also sitting at `pos` whose gravity must be
            // respected individually, so recurse instead of shifting them all.
            // Deletions always recurse so nodes can clamp to `pos`.
            if delta < 0 || pos == start {
                Self::adjust_recursive(&mut node.right, pos, delta);
            } else if let Some(ref right) = node.right {
                right.borrow_mut().lazy_delta += delta;
            }

            // 3. Recurse left, as it may contain markers spanning the edit pos.
            Self::adjust_recursive(&mut node.left, pos, delta);
        } else {
            // pos > start
            // CASE 2: This node's start is BEFORE the edit.
            // Its start is unaffected. We only need to check the right subtree
            // for nodes that might be affected.
            Self::adjust_recursive(&mut node.right, pos, delta);
        }

        // Handle the interval span case (where the edit falls inside [start, end]).
        // A left-gravity marker's end stays put for an insertion landing exactly
        // on it, matching its start behaviour above.
        let end = node.marker.interval.end;
        let shift_end = if left_gravity && delta > 0 {
            end > pos
        } else {
            end >= pos
        };
        if shift_end {
            node.marker.interval.end =
                (end as i64 + delta).max(node.marker.interval.start as i64) as u64;
        }

        drop(node);
        Node::update_stats(node_rc);
    }

    /// Recursive helper for query
    fn query_recursive(
        node_opt: &NodePtr,
        query_start: u64,
        query_end: u64,
        results: &mut Vec<Marker>,
    ) {
        let node_rc = match node_opt {
            Some(n) => n,
            None => return,
        };

        Node::push_delta(node_rc);
        let node = node_rc.borrow();

        let i = &node.marker.interval;
        if i.start <= query_end && i.end >= query_start {
            results.push(node.marker.clone());
        }

        if node.left.is_some() && node.left.as_ref().unwrap().borrow().max_end >= query_start {
            Self::query_recursive(&node.left, query_start, query_end, results);
        }

        if node.right.is_some() && node.marker.interval.start <= query_end {
            Self::query_recursive(&node.right, query_start, query_end, results);
        }
    }

    // --- AVL Balancing ---

    fn balance(node: Rc<RefCell<Node>>) -> NodePtr {
        let bf = Node::balance_factor(&node);

        if bf > 1 {
            let left_rc = node.borrow().left.as_ref().unwrap().clone();
            if Node::balance_factor(&left_rc) < 0 {
                // Fix RefCell borrow issue: extract left child before rotating
                let left_child = node.borrow_mut().left.take().unwrap();
                node.borrow_mut().left = Self::rotate_left(left_child);
            }
            Self::rotate_right(node)
        } else if bf < -1 {
            let right_rc = node.borrow().right.as_ref().unwrap().clone();
            if Node::balance_factor(&right_rc) > 0 {
                // Fix RefCell borrow issue: extract right child before rotating
                let right_child = node.borrow_mut().right.take().unwrap();
                node.borrow_mut().right = Self::rotate_right(right_child);
            }
            Self::rotate_left(node)
        } else {
            Some(node)
        }
    }

    fn rotate_left(node_rc: Rc<RefCell<Node>>) -> NodePtr {
        Node::push_delta(&node_rc);
        let x_rc = node_rc.borrow_mut().right.take().unwrap();
        Node::push_delta(&x_rc);

        let mut y = node_rc.borrow_mut();
        let mut x = x_rc.borrow_mut();

        y.right = x.left.take();
        if let Some(ref r) = y.right {
            r.borrow_mut().parent = Rc::downgrade(&node_rc);
        }
        x.left = Some(Rc::clone(&node_rc));
        x.parent = y.parent.clone();
        y.parent = Rc::downgrade(&x_rc);

        drop(x);
        drop(y);

        Node::update_stats(&node_rc);
        Node::update_stats(&x_rc);
        Some(x_rc)
    }

    fn rotate_right(node_rc: Rc<RefCell<Node>>) -> NodePtr {
        Node::push_delta(&node_rc);
        let x_rc = node_rc.borrow_mut().left.take().unwrap();
        Node::push_delta(&x_rc);

        let mut y = node_rc.borrow_mut();
        let mut x = x_rc.borrow_mut();

        y.left = x.right.take();
        if let Some(ref l) = y.left {
            l.borrow_mut().parent = Rc::downgrade(&node_rc);
        }
        x.right = Some(Rc::clone(&node_rc));
        x.parent = y.parent.clone();
        y.parent = Rc::downgrade(&x_rc);

        drop(x);
        drop(y);

        Node::update_stats(&node_rc);
        Node::update_stats(&x_rc);
        Some(x_rc)
    }
}

#[cfg(test)]
impl IntervalTree {
    fn height(&self) -> i32 {
        Node::height(&self.root)
    }
    fn debug_dump(&self) -> Vec<(MarkerId, u64, u64, bool)> {
        let mut out = Vec::new();
        Self::debug_collect(&self.root, 0, &mut out);
        out
    }
    fn debug_collect(node: &NodePtr, ad: i64, out: &mut Vec<(MarkerId, u64, u64, bool)>) {
        if let Some(n) = node {
            let nb = n.borrow();
            let d = ad + nb.lazy_delta;
            Self::debug_collect(&nb.left, d, out);
            out.push((
                nb.marker.id,
                (nb.marker.interval.start as i64 + d) as u64,
                (nb.marker.interval.end as i64 + d) as u64,
                nb.marker.right_gravity,
            ));
            Self::debug_collect(&nb.right, d, out);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Helper to insert and return the ID, making test setup cleaner.
    fn insert_marker(tree: &mut IntervalTree, start: u64, end: u64) -> MarkerId {
        tree.insert(start, end)
    }

    /// Helper to get position and unwrap, or panic with a clear message.
    fn get_pos(tree: &IntervalTree, id: MarkerId) -> (u64, u64) {
        tree.get_position(id)
            .unwrap_or_else(|| panic!("Marker ID {} not found.", id))
    }

    // --- Insertion gravity (issue #2053) ---

    #[test]
    fn test_left_gravity_marker_stays_on_insert_at_position() {
        // A left-gravity point marker models the end of a fixed-width
        // highlight (e.g. a search match). Inserting text exactly at its
        // position must NOT move it, so the highlight does not grow.
        let mut tree = IntervalTree::new();
        let m = tree.insert_left_gravity(3, 3);

        // Insert 4 bytes immediately at the marker.
        tree.adjust_for_edit(3, 4);

        assert_eq!(
            get_pos(&tree, m),
            (3, 3),
            "left-gravity marker must stay put when text is inserted at its position"
        );
    }

    #[test]
    fn test_right_gravity_marker_moves_on_insert_at_position() {
        // The default (right gravity) point marker moves forward when text is
        // inserted exactly at its position.
        let mut tree = IntervalTree::new();
        let m = tree.insert(3, 3);

        tree.adjust_for_edit(3, 4);

        assert_eq!(get_pos(&tree, m), (7, 7));
    }

    #[test]
    fn test_left_gravity_marker_still_shifts_on_insert_before() {
        // Left gravity only changes the exact-boundary case; insertions
        // strictly before the marker still shift it.
        let mut tree = IntervalTree::new();
        let m = tree.insert_left_gravity(5, 5);

        tree.adjust_for_edit(2, 3);

        assert_eq!(get_pos(&tree, m), (8, 8));
    }

    #[test]
    fn test_search_match_does_not_grow_on_adjacent_insert() {
        // Reproduces #2053 at the marker level: a match highlight spanning
        // [0, 3) is modelled by a right-gravity start marker at 0 and a
        // left-gravity end marker at 3. Typing immediately after the match
        // (at position 3) must leave both markers anchored to the match.
        let mut tree = IntervalTree::new();
        let start = tree.insert(0, 0);
        let end = tree.insert_left_gravity(3, 3);

        // User types "X" right after the match.
        tree.adjust_for_edit(3, 1);

        assert_eq!(get_pos(&tree, start), (0, 0));
        assert_eq!(
            get_pos(&tree, end),
            (3, 3),
            "highlight end must not extend over text typed after the match"
        );
    }

    #[test]
    fn test_adjacent_matches_keep_independent_gravity_at_shared_boundary() {
        // Two adjacent matches (e.g. searching "ab" in "abab") share a
        // boundary at position 2: the first match's left-gravity end and the
        // second match's right-gravity start both sit at 2. Inserting there
        // must keep the first match fixed while the second match shifts.
        let mut tree = IntervalTree::new();
        let m1_start = tree.insert(0, 0);
        let m1_end = tree.insert_left_gravity(2, 2);
        let m2_start = tree.insert(2, 2);
        let m2_end = tree.insert_left_gravity(4, 4);

        // Insert 3 bytes exactly at the shared boundary.
        tree.adjust_for_edit(2, 3);

        assert_eq!(get_pos(&tree, m1_start), (0, 0));
        assert_eq!(get_pos(&tree, m1_end), (2, 2), "first match must not grow");
        assert_eq!(get_pos(&tree, m2_start), (5, 5), "second match shifts");
        assert_eq!(get_pos(&tree, m2_end), (7, 7));
    }

    #[test]
    fn test_gravity_reversal_preserves_bst_for_later_edits() {
        // Regression for prop_shadow_model_matches_tree: a left-gravity marker
        // and a right-gravity marker created in id-order at distinct positions
        // can later collide on the same position. An insertion there makes the
        // right-gravity marker hop *past* the left-gravity one, reversing their
        // relative order. The positional BST must be rebuilt so that a
        // *subsequent* edit still routes to the displaced marker — the original
        // bug left the tree corrupt and silently dropped the later deletion.
        let mut tree = IntervalTree::new();

        // Created in this id order while positions are distinct.
        let right = tree.insert(10, 10); // right-gravity, lower id
        let left = tree.insert_left_gravity(11, 11); // left-gravity, higher id

        // Make them collide at position 10: pull `left` back by deleting the
        // single byte between them.
        tree.adjust_for_edit(10, -1);
        assert_eq!(get_pos(&tree, right), (10, 10));
        assert_eq!(get_pos(&tree, left), (10, 10));

        // Insert at 10. `right` (right-gravity) hops to 15; `left` stays at 10.
        // Their order is now reversed relative to the (start, id) BST key.
        tree.adjust_for_edit(10, 5);
        assert_eq!(get_pos(&tree, left), (10, 10), "left-gravity marker stays");
        assert_eq!(
            get_pos(&tree, right),
            (15, 15),
            "right-gravity marker moves"
        );

        // The real test: a later edit between the two positions must still
        // reach `right`. Before the fix this deletion never visited `right`'s
        // node because the BST was corrupt, leaving it stuck at 15.
        tree.adjust_for_edit(12, -1);
        assert_eq!(
            get_pos(&tree, right),
            (14, 14),
            "later deletion must still route to the displaced marker"
        );
        assert_eq!(get_pos(&tree, left), (10, 10));
    }

    #[test]
    fn test_gravity_reversal_after_clamp_with_unordered_ids() {
        // Production markers get ids in *creation* order, not position order, so
        // a higher-id marker can sit at a lower position. A deletion can then
        // clamp two markers to the same point, and a following insertion there
        // (left-gravity stayer + right-gravity mover) must still be handled
        // without losing or duplicating a marker.
        let mut tree = IntervalTree::new();
        let a = tree.insert(100, 100); // id 0, right-gravity, higher position
        let b = tree.insert_left_gravity(50, 50); // id 1, left-gravity, lower position

        // Clamp both to position 50.
        tree.adjust_for_edit(50, -60);
        assert_eq!(get_pos(&tree, a), (50, 50));
        assert_eq!(get_pos(&tree, b), (50, 50));

        // Insert at 50: `a` (right-gravity) moves to 55, `b` (left-gravity) stays.
        tree.adjust_for_edit(50, 5);
        assert_eq!(get_pos(&tree, b), (50, 50), "left-gravity stayer");
        assert_eq!(get_pos(&tree, a), (55, 55), "right-gravity mover");

        // Both markers must still be present and a later edit must route to both.
        tree.adjust_for_edit(52, -1);
        assert_eq!(get_pos(&tree, a), (54, 54));
        assert_eq!(get_pos(&tree, b), (50, 50));

        // The tree must hold exactly two physical nodes — no orphan/duplicate
        // left behind by a misrouted internal delete.
        assert_eq!(
            tree.debug_dump().len(),
            2,
            "tree leaked a duplicate node: {:?}",
            tree.debug_dump()
        );
    }

    #[test]
    fn test_initial_insert_and_delete() {
        let mut tree = IntervalTree::new();
        let id1 = insert_marker(&mut tree, 10, 20);
        let id2 = insert_marker(&mut tree, 30, 40);

        assert_eq!(get_pos(&tree, id1), (10, 20));
        assert_eq!(get_pos(&tree, id2), (30, 40));

        assert!(tree.delete(id1));
        assert_eq!(tree.get_position(id1), None);
        assert_eq!(get_pos(&tree, id2), (30, 40));
    }

    #[test]
    fn test_basic_edit_adjustment() {
        let mut tree = IntervalTree::new();
        let id1 = insert_marker(&mut tree, 10, 20); // Before edit
        let id2 = insert_marker(&mut tree, 30, 40); // At/After edit

        // Insert 5 characters at position 30
        tree.adjust_for_edit(30, 5);

        // id1 (10-20) should not move
        assert_eq!(
            get_pos(&tree, id1),
            (10, 20),
            "Marker before edit should not move."
        );

        // id2 (30-40) should move to (35-45)
        assert_eq!(
            get_pos(&tree, id2),
            (35, 45),
            "Marker at/after edit should move."
        );

        // Delete 10 characters at position 5
        tree.adjust_for_edit(5, -10); // All markers are after position 5

        // id1 (10-20) is inside the deletion [5, 15) and should be clamped and shrunk.
        assert_eq!(
            get_pos(&tree, id1),
            (5, 10),
            "Marker moved back by deletion."
        );

        // id2 (35-45) -> (25-35)
        assert_eq!(
            get_pos(&tree, id2),
            (25, 35),
            "Marker moved back by deletion."
        );
    }

    #[test]
    fn test_problematic_lazy_delta_scenario() {
        // This test replicates the tricky tree structure to ensure the O(log n) lazy
        // delta propagation works correctly and doesn't over-propagate to left children.

        let mut tree = IntervalTree::new();

        // Setup the tree with specific positions to force a parent/child relationship
        // that caused the previous bug:
        // L(100) -> P(200) <- R(300)
        let id_p = insert_marker(&mut tree, 200, 250); // Parent node (P)
        let id_r = insert_marker(&mut tree, 300, 350); // Right child (R)
        let id_l = insert_marker(&mut tree, 100, 150); // Left child (L)

        // --- Verify initial state ---
        assert_eq!(
            get_pos(&tree, id_l),
            (100, 150),
            "L initial position incorrect."
        );
        assert_eq!(
            get_pos(&tree, id_p),
            (200, 250),
            "P initial position incorrect."
        );
        assert_eq!(
            get_pos(&tree, id_r),
            (300, 350),
            "R initial position incorrect."
        );

        // --- Apply the problematic edit ---
        // Edit: Insert 50 characters at position 150 (P=150, delta=+50)
        // L(100) should NOT move (100 < 150).
        // P(200) and R(300) should move (+50).
        tree.adjust_for_edit(150, 50);

        // --- Verify corrected final state ---

        // L(100) should have its end expanded (100 < 150, but 150 >= 150).
        assert_eq!(
            get_pos(&tree, id_l),
            (100, 200),
            "L(100) should expand to (100, 200)."
        );

        // P(200) should be shifted (200 >= 150) -> 250
        assert_eq!(
            get_pos(&tree, id_p),
            (250, 300),
            "P(200) did not shift correctly. Should be 250."
        );

        // R(300) should be shifted (300 >= 150) -> 350
        assert_eq!(
            get_pos(&tree, id_r),
            (350, 400),
            "R(300) did not shift correctly. Should be 350."
        );
    }

    #[test]
    fn test_interval_spanning_edit() {
        let mut tree = IntervalTree::new();
        // Marker S starts before edit, but spans it.
        let id_s = insert_marker(&mut tree, 50, 200);

        // Edit: Insert 10 characters at position 100 (P=100, delta=+10)
        tree.adjust_for_edit(100, 10);

        // S(50, 200) starts before 100, so its start (50) is fixed.
        // Its end (200) is at/after 100, so its end should move to 210.
        assert_eq!(
            get_pos(&tree, id_s),
            (50, 210),
            "Spanning marker end did not move correctly."
        );
    }

    #[test]
    fn test_deletion_engulfing_marker_start() {
        let mut tree = IntervalTree::new();
        let id1 = insert_marker(&mut tree, 8, 20);

        // Delete 10 chars at pos 5. Deletion is on [5, 15).
        // Marker is on [8, 20). The part [8, 15) is deleted.
        // New start should be clamped at the deletion position: 5.
        // End is adjusted by delta: 20 - 10 = 10.
        // So new interval should be (5, 10).
        tree.adjust_for_edit(5, -10);

        assert_eq!(
            get_pos(&tree, id1),
            (5, 10),
            "Marker should be clamped and shrunk."
        );
    }

    #[test]
    fn test_zero_length_marker() {
        let mut tree = IntervalTree::new();
        let id1 = insert_marker(&mut tree, 10, 10);

        // Insertion at the marker's position should push it.
        tree.adjust_for_edit(10, 5);
        assert_eq!(
            get_pos(&tree, id1),
            (15, 15),
            "Insertion at zero-length marker."
        );

        // Insertion before should also push it.
        tree.adjust_for_edit(5, 5);
        assert_eq!(
            get_pos(&tree, id1),
            (20, 20),
            "Insertion before zero-length marker."
        );

        // Deletion after should not affect it.
        tree.adjust_for_edit(25, -5);
        assert_eq!(
            get_pos(&tree, id1),
            (20, 20),
            "Deletion after zero-length marker."
        );

        // Deletion that contains the marker.
        tree.adjust_for_edit(15, -10);
        // Marker at 20. Deletion on [15, 25).
        // Start becomes max(15, 20-10) = 15.
        // End becomes max(new_start, 20-10) = max(15, 10) = 15.
        assert_eq!(
            get_pos(&tree, id1),
            (15, 15),
            "Deletion containing zero-length marker."
        );
    }

    #[test]
    fn test_edit_at_pos_zero() {
        let mut tree = IntervalTree::new();
        let id1 = insert_marker(&mut tree, 10, 20);

        // Insertion at pos 0
        tree.adjust_for_edit(0, 5);
        assert_eq!(get_pos(&tree, id1), (15, 25), "Insertion at pos 0.");

        // Deletion at pos 0
        tree.adjust_for_edit(0, -5);
        assert_eq!(get_pos(&tree, id1), (10, 20), "Deletion at pos 0.");

        // Deletion at pos 0 that engulfs the start.
        tree.adjust_for_edit(0, -15);
        // Marker at (10, 20). Deletion on [0, 15).
        // New start becomes max(0, 10-15) = 0.
        // New end becomes max(new_start, 20-15) = max(0, 5) = 5.
        assert_eq!(get_pos(&tree, id1), (0, 5), "Engulfing deletion at pos 0.");
    }

    #[test]
    fn test_deletion_preserves_marker_ordering() {
        // This test reproduces the bug found in prop_marker_ordering_preserved
        // where lazy delta propagation causes ordering violations.
        let mut tree = IntervalTree::new();

        // Create markers in order: [0, 10, 20, 30, 40] (spacing=10)
        let id0 = insert_marker(&mut tree, 0, 0);
        let id1 = insert_marker(&mut tree, 10, 10);
        let id2 = insert_marker(&mut tree, 20, 20);
        let id3 = insert_marker(&mut tree, 30, 30);
        let id4 = insert_marker(&mut tree, 40, 40);

        // Verify initial state
        assert_eq!(get_pos(&tree, id0), (0, 0));
        assert_eq!(get_pos(&tree, id1), (10, 10));
        assert_eq!(get_pos(&tree, id2), (20, 20));
        assert_eq!(get_pos(&tree, id3), (30, 30));
        assert_eq!(get_pos(&tree, id4), (40, 40));

        // Delete 16 bytes starting at position 5
        // This deletes range [5, 21)
        // Expected positions after: [0, 5, 5, 14, 24]
        tree.adjust_for_edit(5, -16);

        // Get all positions
        let positions = vec![
            get_pos(&tree, id0).0,
            get_pos(&tree, id1).0,
            get_pos(&tree, id2).0,
            get_pos(&tree, id3).0,
            get_pos(&tree, id4).0,
        ];

        // Verify ordering is preserved (no inversions)
        for i in 0..positions.len() - 1 {
            assert!(
                positions[i] <= positions[i + 1],
                "Ordering violated at index {}: {:?}[{}]={} > {:?}[{}]={}",
                i,
                positions,
                i,
                positions[i],
                positions,
                i + 1,
                positions[i + 1]
            );
        }

        // Verify specific expected positions
        assert_eq!(get_pos(&tree, id0), (0, 0), "Marker at 0 should stay at 0");
        assert_eq!(
            get_pos(&tree, id1),
            (5, 5),
            "Marker at 10 should clamp to 5"
        );
        assert_eq!(
            get_pos(&tree, id2),
            (5, 5),
            "Marker at 20 should clamp to 5"
        );
        assert_eq!(
            get_pos(&tree, id3),
            (14, 14),
            "Marker at 30 should shift to 14"
        );
        assert_eq!(
            get_pos(&tree, id4),
            (24, 24),
            "Marker at 40 should shift to 24"
        );
    }

    // Property tests exercising the tree directly with creation-order ids
    // (decoupled from position), mixed gravity, clamping deletes, and explicit
    // marker deletes — the combination that exposed the BST-ordering and
    // delete-routing bugs.
    mod property_tests {
        use super::*;
        use proptest::prelude::*;

        #[derive(Debug, Clone)]
        enum Op {
            Insert { pos: u64, len: u64 },
            Delete { pos: u64, len: u64 },
            CreateMarker { pos: u64, right_gravity: bool },
            DeleteMarker { idx: usize },
        }

        fn arb_op(max: u64) -> impl Strategy<Value = Op> {
            prop_oneof![
                (0..=max, 1..=50u64).prop_map(|(pos, len)| Op::Insert { pos, len }),
                (0..=max, 1..=30u64).prop_map(|(pos, len)| Op::Delete { pos, len }),
                (0..=max, any::<bool>())
                    .prop_map(|(pos, right_gravity)| Op::CreateMarker { pos, right_gravity }),
                (0..200usize).prop_map(|idx| Op::DeleteMarker { idx }),
            ]
        }

        /// Fold one interval through the edits the way the per-edit path
        /// does: highest position first, shifting a coordinate at or after
        /// the edit (strictly after, for a left-gravity marker meeting an
        /// insertion) and never letting the end precede the start.
        fn shadow_adjust(
            mut start: i64,
            mut end: i64,
            right_gravity: bool,
            edits: &[(u64, i64)],
        ) -> (u64, u64) {
            for &(pos, delta) in edits {
                let pos = pos as i64;
                let left_gravity = !right_gravity;
                if pos <= start && !(left_gravity && delta > 0 && pos == start) {
                    start = if delta < 0 {
                        (start + delta).max(pos)
                    } else {
                        start + delta
                    };
                }
                let shift_end = if left_gravity && delta > 0 {
                    end > pos
                } else {
                    end >= pos
                };
                if shift_end {
                    end = (end + delta).max(start);
                }
            }
            (start.max(0) as u64, end.max(0) as u64)
        }

        proptest! {
            /// The tree's reported positions must always match a naive shadow
            /// model that slides/clamps point markers independently, regardless
            /// of marker creation order, gravity, clamping, or interleaved
            /// marker deletions.
            #[test]
            fn prop_tree_matches_shadow_with_unordered_ids(
                init in prop::collection::vec((0..1000u64, any::<bool>()), 0..15),
                ops in prop::collection::vec(arb_op(1000), 1..40),
            ) {
                let mut tree = IntervalTree::new();
                // shadow: (id, Option<pos>, right_gravity)
                let mut shadow: Vec<(MarkerId, Option<u64>, bool)> = Vec::new();

                for (pos, rg) in init {
                    let id = if rg {
                        tree.insert(pos, pos)
                    } else {
                        tree.insert_left_gravity(pos, pos)
                    };
                    shadow.push((id, Some(pos), rg));
                }

                for op in ops {
                    match op {
                        Op::Insert { pos, len } => {
                            tree.adjust_for_edit(pos, len as i64);
                            for (_id, p, rg) in shadow.iter_mut() {
                                if let Some(cur) = p {
                                    let shifts = if *rg { *cur >= pos } else { *cur > pos };
                                    if shifts {
                                        *cur += len;
                                    }
                                }
                            }
                        }
                        Op::Delete { pos, len } => {
                            tree.adjust_for_edit(pos, -(len as i64));
                            for (_id, p, _rg) in shadow.iter_mut() {
                                if let Some(cur) = p {
                                    if *cur >= pos + len {
                                        *cur -= len;
                                    } else if *cur > pos {
                                        *cur = pos;
                                    }
                                }
                            }
                        }
                        Op::CreateMarker { pos, right_gravity } => {
                            let id = if right_gravity {
                                tree.insert(pos, pos)
                            } else {
                                tree.insert_left_gravity(pos, pos)
                            };
                            shadow.push((id, Some(pos), right_gravity));
                        }
                        Op::DeleteMarker { idx } => {
                            if !shadow.is_empty() {
                                let i = idx % shadow.len();
                                if let (id, Some(_), _) = shadow[i] {
                                    tree.delete(id);
                                    shadow[i].1 = None;
                                }
                            }
                        }
                    }

                    // Every live marker must match its shadow position.
                    for (id, p, _rg) in &shadow {
                        if let Some(expected) = p {
                            let actual = tree.get_position(*id).map(|x| x.0);
                            prop_assert_eq!(
                                actual,
                                Some(*expected),
                                "marker {} expected at {} but tree says {:?}",
                                id,
                                expected,
                                actual
                            );
                        }
                    }

                    // The tree must remain position-ordered (in-order
                    // non-decreasing) and free of leaked/duplicate nodes.
                    let dump = tree.debug_dump();
                    for w in dump.windows(2) {
                        prop_assert!(
                            w[0].1 <= w[1].1,
                            "BST position order violated: id {}@{} before id {}@{}",
                            w[0].0, w[0].1, w[1].0, w[1].1
                        );
                    }
                    let live = shadow.iter().filter(|(_, p, _)| p.is_some()).count();
                    prop_assert_eq!(
                        dump.len(),
                        live,
                        "tree node count {} != live marker count {}",
                        dump.len(),
                        live
                    );
                }
            }

            /// `insert_many` must produce a tree that behaves exactly like
            /// one built by inserting the same intervals one at a time: same
            /// ids, same positions, same response to a later edit stream, and
            /// the BST/AVL invariants intact so those later edits stay cheap.
            ///
            /// The batch is padded past `BULK_INSERT_MIN` so the bulk path is
            /// actually taken rather than falling through to the per-insert
            /// one, and some markers are seeded first so the build has an
            /// existing tree to fold in rather than only the empty case.
            #[test]
            fn prop_bulk_insert_matches_sequential(
                seed in prop::collection::vec((0..1000u64, 0..40u64), 0..30),
                batch in prop::collection::vec((0..1000u64, 0..40u64), 70..140),
                raw_edits in prop::collection::vec((0..60u64, 0..20u64, 0..20u64), 0..15),
            ) {
                let mut sequential = IntervalTree::new();
                let mut bulk = IntervalTree::new();
                for (start, len) in &seed {
                    let (s, e) = (*start, start + len);
                    prop_assert_eq!(sequential.insert(s, e), bulk.insert(s, e));
                }

                let intervals: Vec<(u64, u64)> =
                    batch.iter().map(|(start, len)| (*start, start + len)).collect();
                let seq_ids: Vec<MarkerId> = intervals
                    .iter()
                    .map(|&(s, e)| sequential.insert(s, e))
                    .collect();
                let bulk_ids = bulk.insert_many(&intervals);
                prop_assert_eq!(&seq_ids, &bulk_ids, "bulk insert handed out different ids");

                // Both trees hold the same markers at the same positions.
                prop_assert_eq!(
                    sequential.debug_dump(),
                    bulk.debug_dump(),
                    "bulk-built tree holds different markers than the sequential one"
                );

                // Same edit stream through both. Point markers — which is what
                // an overlay's own extent is made of — must stay in lockstep.
                // Spanning ends are deliberately not compared: whether the
                // per-edit path shifts an end that straddles the edit depends
                // on where the node sits in the tree (`adjust_recursive` case
                // 2 never descends left), so any two differently shaped trees
                // disagree there, bulk build or not. See the note on
                // `prop_bulk_adjust_matches_sequential`.
                let mut edits: Vec<(u64, i64)> = Vec::new();
                let mut cursor = 0u64;
                for (gap, del_len, ins_len) in raw_edits {
                    cursor += gap;
                    let delta = ins_len as i64 - del_len as i64;
                    if delta != 0 {
                        edits.push((cursor, delta));
                    }
                    cursor += del_len.max(1);
                }
                for (pos, delta) in &edits {
                    sequential.adjust_for_edit(*pos, *delta);
                    bulk.adjust_for_edit(*pos, *delta);
                }

                for (id, (_, len)) in bulk_ids.iter().zip(&batch) {
                    if *len != 0 {
                        continue;
                    }
                    prop_assert_eq!(
                        bulk.get_position(*id),
                        sequential.get_position(*id),
                        "point marker {} diverged from the sequential tree after edits {:?}",
                        id,
                        edits
                    );
                }

                let bulk_dump = bulk.debug_dump();

                // Both dumps are in-order traversals, so this pins the BST
                // invariant on the bulk-built tree as well as the contents.
                for w in bulk_dump.windows(2) {
                    prop_assert!(
                        w[0].1 <= w[1].1,
                        "BST position order violated: id {}@{} before id {}@{}",
                        w[0].0, w[0].1, w[1].0, w[1].1
                    );
                }

                // A batch this size must not leave a degenerate tree: the
                // whole point is that later operations stay logarithmic.
                let n = (seed.len() + batch.len()) as u32;
                let max_avl_height = (2.0 * (n as f64 + 1.0).log2() + 1.0).ceil() as i32;
                prop_assert!(
                    bulk.height() <= max_avl_height,
                    "bulk-built tree height {} exceeds the AVL bound {} for {} markers",
                    bulk.height(), max_avl_height, n
                );

                // Every marker is still reachable by id.
                for (id, (start, len)) in bulk_ids.iter().zip(&batch) {
                    prop_assert!(
                        bulk.get_position(*id).is_some(),
                        "marker {} ({}..{}) vanished from the bulk-built tree",
                        id, start, start + len
                    );
                }
            }

            /// `adjust_for_bulk_edits` must land every marker exactly where
            /// applying the same edits one at a time (highest position first,
            /// as a bulk edit does) would have put it — intervals included.
            ///
            /// Point markers are additionally compared against a tree that
            /// really did apply the edits one at a time. Spanning intervals are
            /// not: whether the per-edit path shifts an end that straddles the
            /// edit depends on where the node sits in the tree, so only the
            /// shadow pins down the intended result there.
            #[test]
            fn prop_bulk_adjust_matches_sequential(
                markers in prop::collection::vec((0..1000u64, 0..40u64, any::<bool>()), 0..40),
                raw_edits in prop::collection::vec((0..60u64, 0..20u64, 0..20u64), 0..25),
            ) {
                // Lay the edits out left to right so their ranges are disjoint
                // and no position repeats, which is what a bulk edit
                // guarantees — it merges same-position edits before this point.
                let mut edits: Vec<(u64, i64)> = Vec::new();
                let mut cursor = 0u64;
                for (gap, del_len, ins_len) in raw_edits {
                    cursor += gap;
                    let delta = ins_len as i64 - del_len as i64;
                    if delta != 0 {
                        edits.push((cursor, delta));
                        cursor += del_len.max(1);
                    } else {
                        cursor += del_len;
                    }
                }
                // Bulk edits arrive highest position first.
                edits.reverse();

                let mut sequential = IntervalTree::new();
                let mut batched = IntervalTree::new();
                let mut expected: Vec<(MarkerId, (u64, u64), bool)> = Vec::new();
                for (start, len, right_gravity) in &markers {
                    let (s, e) = (*start, start + len);
                    let id = if *right_gravity {
                        sequential.insert(s, e);
                        batched.insert(s, e)
                    } else {
                        sequential.insert_left_gravity(s, e);
                        batched.insert_left_gravity(s, e)
                    };
                    let shadow = shadow_adjust(s as i64, e as i64, *right_gravity, &edits);
                    expected.push((id, shadow, *len == 0));
                }

                for (pos, delta) in &edits {
                    sequential.adjust_for_edit(*pos, *delta);
                }
                batched.adjust_for_bulk_edits(&edits);

                for (id, shadow, is_point) in expected {
                    prop_assert_eq!(
                        batched.get_position(id),
                        Some(shadow),
                        "marker {} diverged from the shadow for edits {:?}",
                        id,
                        edits
                    );
                    if is_point {
                        prop_assert_eq!(
                            batched.get_position(id),
                            sequential.get_position(id),
                            "point marker {} diverged from the per-edit path for edits {:?}",
                            id,
                            edits
                        );
                    }
                }

                // The rebuilt tree must still be position-ordered and hold
                // every marker exactly once.
                let dump = batched.debug_dump();
                for w in dump.windows(2) {
                    prop_assert!(
                        w[0].1 <= w[1].1,
                        "BST position order violated: id {}@{} before id {}@{}",
                        w[0].0, w[0].1, w[1].0, w[1].1
                    );
                }
                prop_assert_eq!(dump.len(), markers.len());
            }
        }
    }
}
