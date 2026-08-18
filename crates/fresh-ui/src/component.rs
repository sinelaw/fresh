//! Composition: a component turns props plus state into a description.

use std::any::{Any, TypeId};

use crate::desc::Node;
use crate::schedule::BuildCx;

/// A unit of composition.
///
/// `build` is pure with respect to everything outside the component: it reads
/// `self` (the props the parent supplied) and `state` (owned by the element),
/// and returns a description. It must not mutate state — see
/// [`BuildCx::updater`], whose mutations are deferred to the next flush.
pub trait Component<M>: 'static {
    type State: Default + 'static;

    fn build(&self, state: &Self::State, cx: &mut BuildCx<'_, M>) -> Node<M>;
}

/// The type-erased form stored in [`crate::desc::Desc::Component`].
pub trait AnyComponent<M> {
    fn build_any(&self, state: &dyn Any, cx: &mut BuildCx<'_, M>) -> Node<M>;
    fn new_state(&self) -> Box<dyn Any>;
    /// The concrete component type. Reconciliation matches on this, so two
    /// different components never update in place over one another.
    fn comp_type_id(&self) -> TypeId;
    fn comp_name(&self) -> &'static str;
}

impl<M, C: Component<M>> AnyComponent<M> for C {
    fn build_any(&self, state: &dyn Any, cx: &mut BuildCx<'_, M>) -> Node<M> {
        let s = state
            .downcast_ref::<C::State>()
            .expect("element state does not match its component type");
        self.build(s, cx)
    }

    fn new_state(&self) -> Box<dyn Any> {
        Box::new(C::State::default())
    }

    fn comp_type_id(&self) -> TypeId {
        TypeId::of::<C>()
    }

    fn comp_name(&self) -> &'static str {
        short_name(std::any::type_name::<C>())
    }
}

/// `foo::bar::Baz<x::Y>` -> `Baz<x::Y>`: enough to identify a component in a
/// tree dump without the module path noise.
fn short_name(full: &'static str) -> &'static str {
    match full[..full.find('<').unwrap_or(full.len())].rfind("::") {
        Some(i) => &full[i + 2..],
        None => full,
    }
}
