//! The element tree: the persistent middle layer, and reconciliation.
//!
//! An element is the mounted instance of a description. It survives rebuilds,
//! it owns component state, and it is what a key identifies. The reconciler's
//! whole job is to decide, for each new description, which element it belongs
//! to — or that it belongs to a new one.

use std::any::Any;
use std::collections::HashMap;
use std::ops::Index;
use std::rc::Rc;

use crate::desc::{component_of, node_key, node_type, resolve, Desc, ElemType, Node};
use crate::key::Key;
use crate::schedule::{BuildCx, Ui};

/// A handle into the element arena. Stable while the element is mounted;
/// reused after it is disposed, so never store one across frames.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ElementId(pub(crate) u32);

impl std::fmt::Debug for ElementId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "E{}", self.0)
    }
}

pub(crate) struct Element<M> {
    pub key: Option<Key>,
    pub ty: ElemType,
    pub name: &'static str,
    pub desc: Node<M>,
    pub state: Option<Box<dyn Any>>,
    pub children: Vec<ElementId>,
    pub parent: Option<ElementId>,
    pub depth: u32,
    pub needs_build: bool,
    /// Set when the element is detached and awaiting deferred disposal. A
    /// detached element is still readable — that is what makes it safe for a
    /// handler to finish on it — but it never rebuilds.
    pub disposed: bool,
}

pub(crate) struct Arena<M> {
    slots: Vec<Option<Element<M>>>,
    free: Vec<u32>,
}

impl<M> Default for Arena<M> {
    fn default() -> Self {
        Arena {
            slots: Vec::new(),
            free: Vec::new(),
        }
    }
}

impl<M> Arena<M> {
    pub fn alloc(&mut self, el: Element<M>) -> ElementId {
        match self.free.pop() {
            Some(i) => {
                self.slots[i as usize] = Some(el);
                ElementId(i)
            }
            None => {
                self.slots.push(Some(el));
                ElementId(self.slots.len() as u32 - 1)
            }
        }
    }

    pub fn release(&mut self, id: ElementId) -> Option<Element<M>> {
        let el = self.slots.get_mut(id.0 as usize)?.take();
        if el.is_some() {
            self.free.push(id.0);
        }
        el
    }

    pub fn get(&self, id: ElementId) -> Option<&Element<M>> {
        self.slots.get(id.0 as usize).and_then(|s| s.as_ref())
    }

    pub fn get_mut(&mut self, id: ElementId) -> Option<&mut Element<M>> {
        self.slots.get_mut(id.0 as usize).and_then(|s| s.as_mut())
    }

    pub fn live(&self) -> usize {
        self.slots.iter().filter(|s| s.is_some()).count()
    }
}

impl<M> Index<ElementId> for Arena<M> {
    type Output = Element<M>;
    fn index(&self, id: ElementId) -> &Element<M> {
        self.get(id).expect("element id is not live")
    }
}

// ---------------------------------------------------------------------------
// Transaction journal
// ---------------------------------------------------------------------------

/// One reversible mutation. Reconcile buffers these so that a panic part-way
/// through leaves the last committed content intact; three separate rules
/// depend on it — the error policy, deferred disposal, and unwinding a failed
/// constructor.
pub(crate) enum Undo<M> {
    Created(ElementId),
    Root(Option<ElementId>),
    Children(ElementId, Vec<ElementId>),
    Desc(ElementId, Node<M>),
    NeedsBuild(ElementId, bool),
}

#[derive(Default)]
pub(crate) struct Txn<M> {
    pub undo: Vec<Undo<M>>,
    /// Elements removed from their parent during this transaction. They are
    /// only marked disposed at commit, so an abort can restore them.
    pub detached: Vec<ElementId>,
}

impl<M> Txn<M> {
    fn new() -> Self {
        Txn {
            undo: Vec::new(),
            detached: Vec::new(),
        }
    }
}

// ---------------------------------------------------------------------------
// Reconciliation
// ---------------------------------------------------------------------------

impl<M: 'static> Ui<M> {
    pub(crate) fn begin_txn(&mut self) {
        debug_assert!(self.txn.is_none(), "reconcile transactions do not nest");
        self.txn = Some(Txn::new());
    }

    pub(crate) fn commit_txn(&mut self) {
        let txn = self.txn.take().expect("commit without an open transaction");
        for c in txn.detached {
            self.mark_disposed(c);
            self.pending_dispose.push(c);
        }
    }

    pub(crate) fn abort_txn(&mut self) {
        let txn = self.txn.take().expect("abort without an open transaction");
        for u in txn.undo.into_iter().rev() {
            match u {
                Undo::Created(id) => {
                    if let Some(el) = self.arena.release(id) {
                        self.renderer.dispose(id, el.ty, el.name);
                    }
                }
                Undo::Root(r) => self.root = r,
                Undo::Children(id, kids) => {
                    if let Some(el) = self.arena.get_mut(id) {
                        el.children = kids;
                    }
                }
                Undo::Desc(id, d) => {
                    if let Some(el) = self.arena.get_mut(id) {
                        el.desc = d;
                    }
                }
                Undo::NeedsBuild(id, v) => {
                    if let Some(el) = self.arena.get_mut(id) {
                        el.needs_build = v;
                    }
                }
            }
        }
        // Detached elements were never marked, so they are already intact.
        self.sched.borrow_mut().clear_building();
    }

    fn journal(&mut self, u: Undo<M>) {
        if let Some(t) = self.txn.as_mut() {
            t.undo.push(u);
        }
    }

    fn set_children(&mut self, id: ElementId, kids: Vec<ElementId>) {
        let old = std::mem::replace(&mut self.arena.get_mut(id).expect("live").children, kids);
        self.journal(Undo::Children(id, old));
    }

    fn set_desc(&mut self, id: ElementId, d: Node<M>) {
        let old = std::mem::replace(&mut self.arena.get_mut(id).expect("live").desc, d);
        self.journal(Undo::Desc(id, old));
    }

    pub(crate) fn set_needs_build(&mut self, id: ElementId, v: bool) {
        let Some(el) = self.arena.get_mut(id) else {
            return;
        };
        let old = std::mem::replace(&mut el.needs_build, v);
        if old != v {
            self.journal(Undo::NeedsBuild(id, old));
        }
    }

    pub(crate) fn detach(&mut self, id: ElementId) {
        if let Some(t) = self.txn.as_mut() {
            t.detached.push(id);
        } else {
            // Outside a transaction there is nothing to roll back.
            self.mark_disposed(id);
            self.pending_dispose.push(id);
        }
    }

    fn mark_disposed(&mut self, id: ElementId) {
        let kids = match self.arena.get_mut(id) {
            Some(el) => {
                el.disposed = true;
                el.parent = None;
                el.children.clone()
            }
            None => return,
        };
        for k in kids {
            self.mark_disposed(k);
        }
    }

    /// Reconcile the root description.
    ///
    /// The root has no parent to match it against, so the `(type, key)` rule is
    /// applied here directly: same type and key updates the existing root in
    /// place, anything else replaces it.
    pub(crate) fn reconcile_root(&mut self, node: Node<M>) {
        let Some(r) = self.root else {
            let id = self.mount_node(node, None, 0);
            self.journal(Undo::Root(None));
            self.root = Some(id);
            return;
        };
        let (ty, _) = node_type(&node);
        let key = node_key(&node);
        let same = {
            let el = &self.arena[r];
            el.ty == ty && el.key == key
        };
        if same {
            self.update_node(r, node);
        } else {
            self.detach(r);
            let id = self.mount_node(node, None, 0);
            self.journal(Undo::Root(Some(r)));
            self.root = Some(id);
        }
    }

    /// Mount a description as a fresh element, recursively.
    pub(crate) fn mount_node(
        &mut self,
        node: Node<M>,
        parent: Option<ElementId>,
        depth: u32,
    ) -> ElementId {
        let key = node_key(&node);
        let (ty, name) = node_type(&node);
        let id = self.arena.alloc(Element {
            key,
            ty,
            name,
            desc: node,
            state: None,
            children: Vec::new(),
            parent,
            depth,
            needs_build: false,
            disposed: false,
        });
        self.journal(Undo::Created(id));
        self.renderer.create(id, ty, name);

        if let ElemType::Component(_) = ty {
            let comp = component_of(&self.arena[id].desc).expect("component description");
            let state = comp.new_state();
            self.arena.get_mut(id).expect("live").state = Some(state);
            self.rebuild(id);
        } else {
            let kids = resolve(&self.arena[id].desc).children.clone();
            let mut out = Vec::with_capacity(kids.len());
            for k in kids {
                out.push(self.mount_node(k, Some(id), depth + 1));
            }
            // The element itself is journalled, so its initial child list is
            // undone with it; no separate entry is needed.
            self.arena.get_mut(id).expect("live").children = out;
        }
        id
    }

    /// Update a mounted element in place against a new description.
    pub(crate) fn update_node(&mut self, id: ElementId, new: Node<M>) {
        // The identity short-circuit: the same instance means the same
        // subtree, so nothing below is touched. Reference identity is the only
        // skip rule — descriptions are reconstructed each frame, so structural
        // equality does not survive handlers.
        if let (Desc::Shared(a), Desc::Shared(b)) = (&self.arena[id].desc.desc, &new.desc) {
            if Rc::ptr_eq(a, b) {
                return;
            }
        }
        self.set_desc(id, new);
        self.renderer
            .update(id, self.arena[id].ty, self.arena[id].name);
        self.rebuild(id);
    }

    /// Re-run this element's build (component) or re-reconcile its children
    /// from the description it already holds (primitive).
    pub(crate) fn rebuild(&mut self, id: ElementId) {
        self.set_needs_build(id, false);
        let ty = self.arena[id].ty;

        if let ElemType::Component(_) = ty {
            if self.trace {
                self.build_log.push(id);
            }
            let (comp, props_children, name) = {
                let el = &self.arena[id];
                (
                    component_of(&el.desc).expect("component description"),
                    resolve(&el.desc).children.clone(),
                    el.name,
                )
            };
            let sched = self.sched.clone();
            sched.borrow_mut().enter_build(id, name);
            let built = {
                let el = &self.arena[id];
                let state: &dyn Any = &**el
                    .state
                    .as_ref()
                    .expect("component element mounted without state");
                let mut cx = BuildCx::new(id, sched.clone(), &props_children);
                comp.build_any(state, &mut cx)
            };
            sched.borrow_mut().clear_building();
            self.reconcile_children(id, vec![built]);
        } else {
            let kids = resolve(&self.arena[id].desc).children.clone();
            self.reconcile_children(id, kids);
        }
    }

    /// Match new descriptions against existing children.
    ///
    /// ```text
    /// same type AND same key  ->  same logical element; update in place
    /// otherwise               ->  unmount the old subtree, mount the new
    /// ```
    ///
    /// A keyed description looks for its key anywhere in the old child list, so
    /// a reorder moves elements instead of recreating them. An unkeyed
    /// description matches the old child at the same index, and only if that
    /// child is itself unkeyed: position is the implicit key.
    pub(crate) fn reconcile_children(&mut self, parent: ElementId, new_nodes: Vec<Node<M>>) {
        let old = self.arena[parent].children.clone();
        let depth = self.arena[parent].depth + 1;

        let mut by_key: HashMap<Key, Vec<usize>> = HashMap::new();
        for (i, &c) in old.iter().enumerate() {
            if let Some(k) = self.arena[c].key.clone() {
                by_key.entry(k).or_default().push(i);
            }
        }

        let mut used = vec![false; old.len()];
        let mut out = Vec::with_capacity(new_nodes.len());

        for (i, n) in new_nodes.into_iter().enumerate() {
            let key = node_key(&n);
            let (ty, _) = node_type(&n);
            let matched = match &key {
                Some(k) => by_key.get(k).and_then(|slots| {
                    slots
                        .iter()
                        .copied()
                        .find(|&j| !used[j] && self.arena[old[j]].ty == ty)
                }),
                None => (i < old.len()
                    && !used[i]
                    && self.arena[old[i]].key.is_none()
                    && self.arena[old[i]].ty == ty)
                    .then_some(i),
            };
            match matched {
                Some(j) => {
                    used[j] = true;
                    let c = old[j];
                    self.update_node(c, n);
                    out.push(c);
                }
                None => out.push(self.mount_node(n, Some(parent), depth)),
            }
        }

        for (j, &c) in old.iter().enumerate() {
            if !used[j] {
                self.detach(c);
            }
        }
        self.set_children(parent, out);
    }

    /// Deferred disposal. Teardown runs children before parents, so a child
    /// releasing a parent-owned handle finds the parent alive.
    pub(crate) fn process_disposals(&mut self) {
        while let Some(root) = self.pending_dispose.pop() {
            self.dispose_subtree(root);
        }
    }

    fn dispose_subtree(&mut self, id: ElementId) {
        let kids = match self.arena.get(id) {
            Some(el) => el.children.clone(),
            None => return,
        };
        for k in kids {
            self.dispose_subtree(k);
        }
        if let Some(el) = self.arena.release(id) {
            self.renderer.dispose(id, el.ty, el.name);
        }
        self.sched.borrow_mut().forget(id);
    }
}
