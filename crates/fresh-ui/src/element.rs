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

use crate::ambient::AmbientNode;
use crate::behavior::Behavior;
use crate::desc::{component_of, node_key, node_type, resolve, Desc, ElemType, Node};
use crate::key::Key;
use crate::render::object::{RenderData, RenderId, RenderNode};
use crate::schedule::{BuildCx, DirtyCause, InitCx, Ui};

/// A handle into the element arena. Stable while the element is mounted;
/// reused after it is disposed, so never store one across frames.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ElementId {
    pub(crate) idx: u32,
    /// The slot's generation when this id was minted. A slot's generation is
    /// bumped every time it is freed, so an id outlives its element by value
    /// but not by access: a lookup with a stale generation finds nothing rather
    /// than the element that later took the slot.
    pub(crate) gen: u32,
}

impl std::fmt::Debug for ElementId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "E{}", self.idx)
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

    /// Registered behaviors, in registration order; torn down in reverse.
    pub behaviors: Vec<Rc<dyn Behavior>>,

    /// The ambients visible to this element.
    pub scope: Option<Rc<AmbientNode>>,
    /// What this element exports to its children. `Provide` elements only.
    pub provides: Option<Rc<AmbientNode>>,
    /// `Provide` elements only: who read this ambient in `build`, and so must
    /// rebuild when the value changes.
    pub dependents: Vec<ElementId>,
    /// `Provide` elements only: who read this ambient in `init`, and so took a
    /// snapshot that will not track changes.
    pub init_dependents: Vec<ElementId>,
    /// The providers this element is registered with, so that disposal and
    /// re-reads can deregister.
    pub reads: Vec<ElementId>,
    pub init_reads: Vec<ElementId>,

    /// This element's render object, if it has geometry of its own.
    pub render: Option<RenderId>,
    /// This element's registration in the focus tree, if it is focusable or
    /// opens a scope.
    pub focus: Option<crate::focus::FocusId>,

    /// Whether this element's links into the render and focus trees have to be
    /// recomputed. Path-marked, so a relink walk skips any subtree carrying
    /// neither this bit nor a changed inheritance.
    pub link_dirty: bool,
    /// What was inherited the last time this element was relinked, and what it
    /// contributed. Equal inputs and a clean bit mean equal outputs.
    pub last_carry: Option<crate::render::layout::Carry>,
    pub last_out: Vec<RenderId>,
    pub last_fout: Vec<crate::focus::FocusId>,

    /// Diagnostics.
    pub builds: u32,
    pub last_dirty: Option<DirtyCause>,
    pub state_name: &'static str,
}

impl<M> Element<M> {
    fn blank(
        key: Option<Key>,
        ty: ElemType,
        name: &'static str,
        desc: Node<M>,
        parent: Option<ElementId>,
        depth: u32,
        scope: Option<Rc<AmbientNode>>,
    ) -> Element<M> {
        Element {
            key,
            ty,
            name,
            desc,
            state: None,
            children: Vec::new(),
            parent,
            depth,
            needs_build: false,
            disposed: false,
            behaviors: Vec::new(),
            scope,
            provides: None,
            dependents: Vec::new(),
            init_dependents: Vec::new(),
            reads: Vec::new(),
            init_reads: Vec::new(),
            render: None,
            focus: None,
            link_dirty: true,
            last_carry: None,
            last_out: Vec::new(),
            last_fout: Vec::new(),
            builds: 0,
            last_dirty: None,
            state_name: "",
        }
    }
}

/// One arena slot: its current generation, and its occupant if any.
struct Slot<M> {
    gen: u32,
    el: Option<Element<M>>,
}

pub(crate) struct Arena<M> {
    slots: Vec<Slot<M>>,
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
                let slot = &mut self.slots[i as usize];
                slot.el = Some(el);
                ElementId {
                    idx: i,
                    gen: slot.gen,
                }
            }
            None => {
                self.slots.push(Slot {
                    gen: 0,
                    el: Some(el),
                });
                ElementId {
                    idx: self.slots.len() as u32 - 1,
                    gen: 0,
                }
            }
        }
    }

    pub fn release(&mut self, id: ElementId) -> Option<Element<M>> {
        let slot = self.slots.get_mut(id.idx as usize)?;
        if slot.gen != id.gen {
            return None;
        }
        let el = slot.el.take();
        if el.is_some() {
            // The next occupant of this slot gets a fresh generation, so every
            // id minted for this element stops resolving here.
            slot.gen = slot.gen.wrapping_add(1);
            self.free.push(id.idx);
        }
        el
    }

    pub fn get(&self, id: ElementId) -> Option<&Element<M>> {
        let slot = self.slots.get(id.idx as usize)?;
        if slot.gen != id.gen {
            return None;
        }
        slot.el.as_ref()
    }

    pub fn get_mut(&mut self, id: ElementId) -> Option<&mut Element<M>> {
        let slot = self.slots.get_mut(id.idx as usize)?;
        if slot.gen != id.gen {
            return None;
        }
        slot.el.as_mut()
    }

    pub fn live(&self) -> usize {
        self.slots.iter().filter(|s| s.el.is_some()).count()
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
                        if let Some(r) = el.render {
                            self.render.release(r);
                        }
                        if let Some(f) = el.focus {
                            self.focus_tree.release(f);
                        }
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
        if old != self.arena[id].children {
            self.mark_link(id);
        }
        self.journal(Undo::Children(id, old));
    }

    fn set_desc(&mut self, id: ElementId, d: Node<M>) {
        let old = std::mem::replace(&mut self.arena.get_mut(id).expect("live").desc, d);
        self.mark_link(id);
        self.journal(Undo::Desc(id, old));
    }

    /// This element's links, and every ancestor's, have to be recomputed. Stops
    /// at the first ancestor already marked: the path above it is already set.
    pub(crate) fn mark_link(&mut self, id: ElementId) {
        let mut cur = Some(id);
        while let Some(c) = cur {
            let Some(el) = self.arena.get_mut(c) else {
                break;
            };
            if el.link_dirty && c != id {
                break;
            }
            el.link_dirty = true;
            cur = el.parent;
        }
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

    /// Mark an element for rebuild in this or the next flush, recording why.
    pub(crate) fn mark_dirty(&mut self, id: ElementId, cause: DirtyCause) {
        let Some(el) = self.arena.get(id) else { return };
        if el.disposed {
            return;
        }
        self.set_needs_build(id, true);
        if let Some(el) = self.arena.get_mut(id) {
            el.last_dirty = Some(cause);
        }
        self.sched.borrow_mut().mark(id);
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

    /// What an element's children see: what it provides, or what it sees.
    fn child_scope(&self, id: ElementId) -> Option<Rc<AmbientNode>> {
        let el = self.arena.get(id)?;
        el.provides.clone().or_else(|| el.scope.clone())
    }

    /// Reconcile the root description.
    ///
    /// The root has no parent to match it against, so the `(type, key)` rule is
    /// applied here directly: same type and key updates the existing root in
    /// place, anything else replaces it.
    pub(crate) fn reconcile_root(&mut self, node: Node<M>) {
        let Some(r) = self.root else {
            let id = self.mount_node(node, None, 0, None);
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
            let id = self.mount_node(node, None, 0, None);
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
        scope: Option<Rc<AmbientNode>>,
    ) -> ElementId {
        let key = node_key(&node);
        let (ty, name) = node_type(&node);
        let id = self
            .arena
            .alloc(Element::blank(key, ty, name, node, parent, depth, scope));
        self.journal(Undo::Created(id));
        self.renderer.create(id, ty, name);
        self.make_render(id);
        if let Some(a) = self.arena[id].desc.anchor.clone() {
            a.bind(id);
            self.anchored.push(id);
        }
        if let Some(p) = parent {
            self.mark_needs_layout(p);
        }
        if let Some(el) = self.arena.get_mut(id) {
            el.last_dirty = Some(DirtyCause::Mount);
        }

        // A `Provide` element exports a new ambient link to its children.
        if let Desc::Provide(p) = &resolve(&self.arena[id].desc).desc {
            let node = Rc::new(AmbientNode {
                parent: self.arena[id].scope.clone(),
                key: p.key,
                provider: id,
                value: std::cell::RefCell::new(p.value.clone()),
            });
            self.arena.get_mut(id).expect("live").provides = Some(node);
        }

        if let ElemType::Component(_) = ty {
            let comp = component_of(&self.arena[id].desc).expect("component description");
            let props_children = resolve(&self.arena[id].desc).children.clone();
            let scope = self.arena[id].scope.clone();
            let sched = self.sched.clone();
            let mut icx = InitCx::new(
                id,
                sched,
                scope,
                &props_children,
                self.services.clone(),
                self.geom_store.clone(),
            );
            let state = comp.init_any(&mut icx);
            let (behaviors, init_reads, focus_request) = icx.finish();
            if let Some(data) = focus_request {
                let f = self.focus_tree.alloc(data);
                self.arena.get_mut(id).expect("live").focus = Some(f);
            }
            for &p in &init_reads {
                if let Some(prov) = self.arena.get_mut(p) {
                    prov.init_dependents.push(id);
                }
            }
            let has_behaviors = !behaviors.is_empty();
            let el = self.arena.get_mut(id).expect("live");
            el.state = Some(state);
            el.behaviors = behaviors;
            el.init_reads = init_reads;
            el.state_name = comp.state_name();
            if has_behaviors {
                self.behaviour_hosts.push(id);
            }
            self.rebuild(id);
        } else {
            let kids = resolve(&self.arena[id].desc).children.clone();
            let child_scope = self.child_scope(id);
            let mut out = Vec::with_capacity(kids.len());
            for k in kids {
                out.push(self.mount_node(k, Some(id), depth + 1, child_scope.clone()));
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
        let moved = crate::desc::layout_relevant_changed(&self.arena[id].desc, &new);
        self.set_desc(id, new);
        if let Some(a) = self.arena[id].desc.anchor.clone() {
            a.bind(id);
            if !self.anchored.contains(&id) {
                self.anchored.push(id);
            }
        }
        // Props always reach the render object and the focus tree: a pointer
        // mode or a traversal position changes what a node does without moving
        // it, and a change that never arrives is a change that never happened.
        self.update_render(id);
        if moved {
            self.mark_needs_layout(id);
        }
        self.renderer
            .update(id, self.arena[id].ty, self.arena[id].name);
        self.refresh_provided(id);
        self.rebuild(id);
    }

    /// A `Provide` element whose value changed swaps it in place — descendants
    /// already point at this link — and marks the elements that read it.
    fn refresh_provided(&mut self, id: ElementId) {
        let Desc::Provide(p) = &resolve(&self.arena[id].desc).desc else {
            return;
        };
        let value = p.value.clone();
        let key = p.key;
        let Some(node) = self.arena[id].provides.clone() else {
            return;
        };
        if Rc::ptr_eq(&node.value.borrow(), &value) {
            return;
        }
        *node.value.borrow_mut() = value;

        let dependents = self.arena[id].dependents.clone();
        let init_only: Vec<ElementId> = self.arena[id]
            .init_dependents
            .iter()
            .copied()
            .filter(|d| !dependents.contains(d))
            .collect();
        for d in dependents {
            self.mark_dirty(d, DirtyCause::Ambient(key.name()));
        }
        for d in init_only {
            let name = self.arena.get(d).map(|e| e.name).unwrap_or("<gone>");
            debug_assert!(
                false,
                "ambient {} changed, but {name} ({d:?}) only read it in init(). \
                 A constructor read is a snapshot and does not re-run; read in build() instead.",
                key.name()
            );
        }
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
            let (comp, props_children, name, scope) = {
                let el = &self.arena[id];
                (
                    component_of(&el.desc).expect("component description"),
                    resolve(&el.desc).children.clone(),
                    el.name,
                    el.scope.clone(),
                )
            };
            let sched = self.sched.clone();
            sched.borrow_mut().enter_build(id, name);
            let (built, reads) = {
                let el = &self.arena[id];
                let state: &dyn Any = &**el
                    .state
                    .as_ref()
                    .expect("component element mounted without state");
                let mut cx = BuildCx::new(
                    id,
                    sched.clone(),
                    scope,
                    &props_children,
                    self.services.clone(),
                );
                let node = comp.build_any(state, &mut cx);
                (node, cx.finish())
            };
            sched.borrow_mut().clear_building();
            self.arena.get_mut(id).expect("live").builds += 1;
            self.register_reads(id, reads);
            self.reconcile_children(id, vec![built]);
        } else if ty == ElemType::LayoutReader {
            // A reader's children are produced by the layout pass, not by its
            // description — which carries none. Reconciling from the
            // description here would dispose the whole window every frame and
            // mount it again, losing element identity and everything that hangs
            // off it: state, focus, and an in-flight press. What this rebuild
            // does instead is tell the reader its builder is new.
            self.update_render(id);
            self.mark_needs_layout(id);
        } else {
            let kids = resolve(&self.arena[id].desc).children.clone();
            self.reconcile_children(id, kids);
        }
    }

    /// Replace this element's dependency registrations with the set the last
    /// build actually read.
    fn register_reads(&mut self, id: ElementId, reads: Vec<ElementId>) {
        if self.arena[id].reads == reads {
            return;
        }
        for p in self.arena[id].reads.clone() {
            if let Some(prov) = self.arena.get_mut(p) {
                prov.dependents.retain(|d| *d != id);
            }
        }
        for &p in &reads {
            if let Some(prov) = self.arena.get_mut(p) {
                prov.dependents.push(id);
            }
        }
        self.arena.get_mut(id).expect("live").reads = reads;
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
        let scope = self.child_scope(parent);

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
                    if let Some(el) = self.arena.get_mut(c) {
                        if el.last_dirty.is_none() || !el.needs_build {
                            el.last_dirty = Some(DirtyCause::Parent(parent));
                        }
                    }
                    self.update_node(c, n);
                    out.push(c);
                }
                None => out.push(self.mount_node(n, Some(parent), depth, scope.clone())),
            }
        }

        for (j, &c) in old.iter().enumerate() {
            if !used[j] {
                self.detach(c);
            }
        }
        if out != old {
            self.mark_needs_layout(parent);
        }
        self.set_children(parent, out);
    }

    /// Drop every reference the framework holds to an element that has gone
    /// away.
    ///
    /// Element ids are recycled, so a stale one is not merely inert: the next
    /// element to take that slot would inherit the pointer capture, the focus
    /// or the in-flight press of an element it has nothing to do with.
    fn forget_element(&mut self, id: ElementId) {
        if self.captured == Some(id) {
            self.captured = None;
        }
        if self.focus == Some(id) {
            // Nothing is told it lost focus: the element is already gone, and
            // `apply_autofocus` decides where focus goes next.
            self.focus = None;
        }
        if self.focus_restore == Some(id) {
            self.focus_restore = None;
        }
        self.hover.retain(|h| *h != id);
        if let Some((targets, _, _)) = &mut self.press {
            targets.retain(|t| *t != id);
        }
        if self.press.as_ref().is_some_and(|(t, _, _)| t.is_empty()) {
            self.press = None;
        }
        self.geom_store.borrow_mut().entries.remove(&id);
        self.behaviour_hosts.retain(|b| *b != id);
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
        // Deregister from every provider before the element goes away.
        for p in self.arena[id].reads.clone() {
            if let Some(prov) = self.arena.get_mut(p) {
                prov.dependents.retain(|d| *d != id);
            }
        }
        for p in self.arena[id].init_reads.clone() {
            if let Some(prov) = self.arena.get_mut(p) {
                prov.init_dependents.retain(|d| *d != id);
            }
        }
        if let Some(el) = self.arena.release(id) {
            // Within one state object, behaviors tear down in reverse
            // registration order.
            for b in el.behaviors.iter().rev() {
                b.teardown();
            }
            if let Some(r) = el.render {
                self.render.release(r);
                self.layout_dirty.retain(|d| *d != r);
                self.pending_layers.retain(|(l, p)| *l != r && *p != r);
            }
            if let Some(f) = el.focus {
                self.focus_tree.release(f);
                self.focus_roots.retain(|x| *x != f);
            }
            self.anchored.retain(|a| *a != id);
            self.renderer.dispose(id, el.ty, el.name);
            self.forget_element(id);
        }
        self.sched.borrow_mut().forget(id);
    }
}

// ---------------------------------------------------------------------------
// Render objects
// ---------------------------------------------------------------------------

impl<M: 'static> Ui<M> {
    /// Give an element a render object, if its description has geometry of its
    /// own. Descriptions that only carry identity or data — `Component`,
    /// `Provide`, `Shared` — get none, and the render tree skips them.
    fn make_render(&mut self, id: ElementId) {
        let Some(obj) = resolve(&self.arena[id].desc).desc.sync_render(None) else {
            return;
        };
        let clips = obj.clips();
        let clip_inset = obj.clip_inset();
        let out_of_flow = obj.out_of_flow();
        let reads_window = obj.reads_window();
        let raw_input = obj.takes_raw_input();
        let scrollbar = obj.shows_scrollbar();
        let reg = obj.focus_reg();
        let r = self.render.alloc(RenderNode {
            obj: Some(obj),
            element: id,
            parent: None,
            children: Vec::new(),
            w: crate::desc::Sizing::Auto,
            h: crate::desc::Sizing::Auto,
            min_w: 0,
            min_h: 0,
            pointer: None,
            clips,
            clip_inset,
            out_of_flow,
            reads_window,
            raw_input,
            scrollbar,
            theme: None,
            key: None,
            data: RenderData::fresh(),
        });
        self.arena.get_mut(id).expect("live").render = Some(r);
        // Focus registration is held alongside the render object and lives
        // exactly as long as it: that is why focus survives reconciliation.
        self.sync_focus_reg(id, reg);
    }

    /// Push changed props into an existing render object, so retained state —
    /// a viewport's scroll offset, a focus registration — survives.
    fn update_render(&mut self, id: ElementId) {
        let Some(r) = self.arena.get(id).and_then(|e| e.render) else {
            return;
        };
        let Some(mut obj) = self.render.get_mut(r).and_then(|n| n.obj.take()) else {
            return;
        };
        resolve(&self.arena[id].desc)
            .desc
            .sync_render(Some(obj.as_mut()));
        let clips = obj.clips();
        let clip_inset = obj.clip_inset();
        let out_of_flow = obj.out_of_flow();
        let reads_window = obj.reads_window();
        let raw_input = obj.takes_raw_input();
        let scrollbar = obj.shows_scrollbar();
        let reg = obj.focus_reg();
        if let Some(n) = self.render.get_mut(r) {
            n.obj = Some(obj);
            n.clips = clips;
            n.clip_inset = clip_inset;
            n.out_of_flow = out_of_flow;
            n.reads_window = reads_window;
            n.raw_input = raw_input;
            n.scrollbar = scrollbar;
        }
        self.sync_focus_reg(id, reg);
    }

    /// Make the focus tree agree with what the render object now declares:
    /// register, update, or deregister. A registration that came from a
    /// `Focusable` behavior is left alone — the behavior owns it.
    fn sync_focus_reg(&mut self, id: ElementId, reg: Option<crate::render::object::FocusReg>) {
        let existing = self.arena.get(id).and_then(|e| e.focus);
        match (existing, reg) {
            (Some(f), Some(reg)) => {
                if let Some(n) = self.focus_tree.get_mut(f) {
                    n.reg = reg;
                }
            }
            (None, Some(reg)) => {
                let f = self
                    .focus_tree
                    .alloc(crate::focus::tree::FocusNodeData::new(id, reg));
                self.arena.get_mut(id).expect("live").focus = Some(f);
            }
            (Some(f), None) => {
                // Only a registration this element's own render object made is
                // withdrawn here; a behavior's outlives its description.
                let from_behavior = self.arena.get(id).is_some_and(|el| {
                    el.behaviors
                        .iter()
                        .any(|b| b.behavior_name() == "Focusable")
                });
                if !from_behavior {
                    self.focus_tree.release(f);
                    self.arena.get_mut(id).expect("live").focus = None;
                    if self.focus == Some(id) {
                        let mut out = Vec::new();
                        self.fire_focus_change(id, false, &mut out);
                        self.focus = None;
                        self.pending_messages.extend(out);
                    }
                }
            }
            (None, None) => {}
        }
    }

    /// Run a constraint-dependent builder and reconcile what it produced. The
    /// one place a build happens inside layout: `set_state` is rejected for the
    /// duration, and the subtree is reconciled transactionally.
    pub(crate) fn run_reader(&mut self, r: RenderId, info: crate::render::object::LayoutInfo) {
        let e = self.render[r].element;
        let f = match &resolve(&self.arena[e].desc).desc {
            Desc::LayoutReader(p) => p.build.clone(),
            _ => return,
        };
        let name = self.arena[e].name;
        let sched = self.sched.clone();
        sched.borrow_mut().enter_build(e, name);
        let node = f(info);
        sched.borrow_mut().clear_building();

        self.begin_txn();
        self.reconcile_children(e, vec![node]);
        self.commit_txn();

        let kids = self.arena[e].children.clone();
        let theme = self.render.get(r).and_then(|n| n.theme.clone());
        let mut inner = Vec::new();
        for k in kids {
            // The subtree a reader produced is an ordinary part of the tree: it
            // inherits the reader's provenance the same way any other child
            // does. Relinking it from nothing would strip the theme off
            // everything the builder emitted.
            self.relink_from_pub(k, Some(r), theme.clone(), &mut inner);
        }
        if let Some(n) = self.render.get_mut(r) {
            n.children = inner;
        }
    }
}
