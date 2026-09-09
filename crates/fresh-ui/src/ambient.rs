//! Ambient values: data that reaches a subtree without passing through every
//! intermediate component's signature.
//!
//! Three pieces and nothing else — no tracking, no dependency graph:
//!
//! - [`Ambient<T>`], a typed token declared statically;
//! - [`provide`], an ordinary description whose element holds the value and the
//!   list of elements that read it;
//! - `cx.read`, which finds the nearest provider, registers this element as a
//!   dependent, and returns the value.
//!
//! When a provider's value is swapped for a non-identical one, it marks its
//! registered dependents. Every dependent rebuilds regardless of which part of
//! the value it read; that is what having no tracker costs.

use std::any::{Any, TypeId};
use std::cell::RefCell;
use std::fmt;
use std::marker::PhantomData;
use std::rc::Rc;

use crate::desc::{Desc, Node};
use crate::element::ElementId;

/// A typed token. Two tokens are the same ambient when their type and their
/// name agree, so the same `T` can back several distinct ambients.
pub struct Ambient<T: 'static> {
    name: &'static str,
    _p: PhantomData<fn() -> T>,
}

impl<T: 'static> Ambient<T> {
    pub const fn new(name: &'static str) -> Self {
        Ambient {
            name,
            _p: PhantomData,
        }
    }

    pub const fn name(&self) -> &'static str {
        self.name
    }

    pub fn key(&self) -> AmbientKey {
        AmbientKey {
            ty: TypeId::of::<T>(),
            name: self.name,
        }
    }
}

/// The runtime identity of an ambient. Part of the element type, so replacing
/// one ambient with another at the same position remounts rather than aliasing.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct AmbientKey {
    ty: TypeId,
    name: &'static str,
}

impl AmbientKey {
    pub fn name(&self) -> &'static str {
        self.name
    }
}

impl fmt::Debug for AmbientKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "@{}", self.name)
    }
}

/// One link in the chain of ambients visible at an element.
///
/// The value sits behind a `RefCell` so that a provider can swap it without
/// rewriting the chain every descendant already points at.
pub(crate) struct AmbientNode {
    pub parent: Option<Rc<AmbientNode>>,
    pub key: AmbientKey,
    pub provider: ElementId,
    pub value: RefCell<Rc<dyn Any>>,
}

impl AmbientNode {
    /// The nearest provider of `key`, and its current value.
    pub fn lookup(self: &Rc<Self>, key: AmbientKey) -> Option<(ElementId, Rc<dyn Any>)> {
        let mut cur = Some(self.clone());
        while let Some(n) = cur {
            if n.key == key {
                return Some((n.provider, n.value.borrow().clone()));
            }
            cur = n.parent.clone();
        }
        None
    }
}

/// What a `Provide` description carries.
#[derive(Clone)]
pub struct ProvideProps {
    pub key: AmbientKey,
    pub value: Rc<dyn Any>,
    /// Whether a newly-built value is the *same* value as the one already
    /// provided. `None` means pointer identity, which is the only test
    /// available for a `T` with no equality.
    ///
    /// **Why this exists.** A host that rebuilds its description every frame —
    /// which the cost model invites, since a rebuild costs one allocation per
    /// node — hands `provide` a *fresh* `Rc` each time. Under pointer identity
    /// alone that is a change: every dependent is marked dirty every frame,
    /// and any `Persisted` beneath it trips the "ambient read in `init()`
    /// changed" assertion on the second frame, having never actually changed.
    /// [`provide_eq`] supplies the comparison and both go away.
    #[allow(clippy::type_complexity)]
    pub same: Option<Rc<dyn Fn(&dyn Any, &dyn Any) -> bool>>,
}

impl fmt::Debug for ProvideProps {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Provide({:?})", self.key)
    }
}

/// Make `value` visible to `child` and everything below it.
pub fn provide<M, T: 'static>(ambient: &Ambient<T>, value: Rc<T>, child: Node<M>) -> Node<M> {
    let mut n = Node::new(Desc::Provide(ProvideProps {
        key: ambient.key(),
        value: value as Rc<dyn Any>,
        same: None,
    }));
    n.children.push(child);
    n
}

/// [`provide`], for a value that knows when it has actually changed.
///
/// Prefer this wherever `T: PartialEq`. Without it the provider compares by
/// pointer, so a host that rebuilds its description each frame re-provides an
/// equal-but-fresh `Rc` and every consumer below is marked dirty for a change
/// that did not happen.
pub fn provide_eq<M, T: PartialEq + 'static>(
    ambient: &Ambient<T>,
    value: Rc<T>,
    child: Node<M>,
) -> Node<M> {
    let mut n = Node::new(Desc::Provide(ProvideProps {
        key: ambient.key(),
        value: value as Rc<dyn Any>,
        same: Some(Rc::new(|a: &dyn Any, b: &dyn Any| {
            match (a.downcast_ref::<T>(), b.downcast_ref::<T>()) {
                (Some(a), Some(b)) => a == b,
                // A type mismatch is not "unchanged"; let the caller through
                // rather than silently swallowing it.
                _ => false,
            }
        })),
    }));
    n.children.push(child);
    n
}

/// A document boundary: one node that both **provides**
/// [`PERSISTENCE_SCOPE`](crate::behavior::PERSISTENCE_SCOPE) and **keys** the
/// subtree by the same id.
///
/// The two have to travel together, and not as a convention. A scope that
/// changes value under a *surviving* provider element is an ambient change,
/// and [`Persisted`](crate::behavior::Persisted) reads its scope in `init()`,
/// which the library forbids changing for a live element — so keying only
/// *inside* the provider trips that assertion rather than switching documents.
/// Keying the provider makes a switch a replacement, and the scope is never
/// observed to change.
///
/// Providing without keying is the other half of the same mistake: two
/// documents then share one element, and the value that was supposed to be
/// scoped is simply the same value.
///
/// ```ignore
/// scope("doc-2", col().children(document_body(doc)))
/// ```
pub fn scope<M: 'static>(id: impl Into<String>, child: Node<M>) -> Node<M> {
    let id = id.into();
    let key = crate::key::Key::Str(id.clone().into());
    provide_eq(&crate::behavior::PERSISTENCE_SCOPE, Rc::new(id), child).key(key)
}
