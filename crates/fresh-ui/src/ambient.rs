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
    }));
    n.children.push(child);
    n
}
