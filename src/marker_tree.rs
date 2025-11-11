use std::cell::{RefCell, RefMut};
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

#[derive(Debug, Clone, PartialEq)]
pub struct Marker {
    pub id: MarkerId,
    pub interval: Interval,
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
    pub fn new() -> Self {
        Self::default()
    }

    /// Inserts a new marker interval. Performance: O(log n)
    pub fn insert(&mut self, start: u64, end: u64) -> MarkerId {
        let id = self.next_id;
        self.next_id += 1;
        let marker = Marker {
            id,
            interval: Interval { start, end },
        };

        let new_node = Node::new(marker.clone(), Weak::new());
        self.root = Self::insert_recursive(self.root.take(), new_node.clone());

        self.marker_map.insert(id, new_node);
        id
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
    pub fn delete(&mut self, id: MarkerId) -> bool {
        let (start, _) = match self.get_position(id) {
            Some(pos) => pos,
            None => return false,
        };

        if !self.marker_map.contains_key(&id) {
            return false;
        }

        self.root = Self::delete_recursive(self.root.take(), start, id);

        self.marker_map.remove(&id).is_some()
    }

    /// Adjusts all markers for a text edit (insertion or deletion).
    /// Performance: O(log n) due to lazy delta propagation.
    pub fn adjust_for_edit(&mut self, pos: u64, delta: i64) {
        Self::adjust_recursive(&mut self.root, pos, delta);
    }

    /// Finds all markers that overlap a given query range.
    /// Performance: O(log n + k)
    pub fn query(&self, query_start: u64, query_end: u64) -> Vec<Marker> {
        let mut results = Vec::new();
        Self::query_recursive(&self.root, query_start, query_end, &mut results);
        results
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

    /// Recursive helper for delete
    fn delete_recursive(root: NodePtr, start: u64, id: MarkerId) -> NodePtr {
        // Remove unnecessary 'mut'
        let root = match root {
            Some(r) => r,
            None => return None,
        };

        Node::push_delta(&root);

        let mut root_mut = root.borrow_mut();
        let (root_start, root_id) = (root_mut.marker.interval.start, root_mut.marker.id);

        match start.cmp(&root_start) {
            Ordering::Less => {
                root_mut.left = Self::delete_recursive(root_mut.left.take(), start, id);
            }
            Ordering::Greater => {
                root_mut.right = Self::delete_recursive(root_mut.right.take(), start, id);
            }
            Ordering::Equal => match id.cmp(&root_id) {
                Ordering::Less => {
                    root_mut.left = Self::delete_recursive(root_mut.left.take(), start, id);
                }
                Ordering::Greater => {
                    root_mut.right = Self::delete_recursive(root_mut.right.take(), start, id);
                }
                Ordering::Equal => {
                    return Self::perform_node_deletion(root_mut, Rc::clone(&root));
                }
            },
        }

        drop(root_mut);
        Node::update_stats(&root);
        Self::balance(root)
    }

    /// Handles the actual structural changes for deletion.
    fn perform_node_deletion(mut node: RefMut<Node>, node_rc: Rc<RefCell<Node>>) -> NodePtr {
        if node.left.is_none() {
            let right = node.right.take();
            if let Some(ref r) = right {
                r.borrow_mut().parent = node.parent.clone();
            }
            return right;
        } else if node.right.is_none() {
            let left = node.left.take();
            if let Some(ref l) = left {
                l.borrow_mut().parent = node.parent.clone();
            }
            return left;
        } else {
            let successor_rc = Self::min_node(&node.right.as_ref().unwrap());

            let (successor_start, successor_id) = {
                let s = successor_rc.borrow();
                (s.marker.interval.start, s.marker.id)
            };

            mem::swap(&mut node.marker, &mut successor_rc.borrow_mut().marker);

            node.right = Self::delete_recursive(node.right.take(), successor_start, successor_id);

            drop(node);
            Node::update_stats(&node_rc);
            return Self::balance(node_rc);
        }
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

        if pos <= start {
            // CASE 1: Edit is at or before this node's start.
            // This node and everything to its right must be shifted.

            // 1. Shift the current node's start position directly, clamping at `pos` if needed.
            if delta < 0 {
                node.marker.interval.start = (start as i64 + delta).max(pos as i64) as u64;
            } else {
                node.marker.interval.start = (start as i64 + delta) as u64;
            }

            // 2. Handle the right subtree.
            // For insertions (delta > 0): can use lazy propagation since all nodes shift uniformly
            // For deletions (delta < 0): must recurse to provide position-aware clamping
            if delta < 0 {
                // Deletion: recurse immediately so nodes can clamp to `pos`
                Self::adjust_recursive(&mut node.right, pos, delta);
            } else {
                // Insertion: lazy propagation is safe and efficient
                if let Some(ref right) = node.right {
                    right.borrow_mut().lazy_delta += delta;
                }
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

        // Always handle the interval span case (where end >= pos)
        if node.marker.interval.end >= pos {
            node.marker.interval.end = (node.marker.interval.end as i64 + delta)
                .max(node.marker.interval.start as i64)
                as u64;
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
}
