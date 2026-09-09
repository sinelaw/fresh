//! Composition: a component turns props plus state into a description.

use std::any::{Any, TypeId};

use crate::desc::Node;
use crate::schedule::{BuildCx, InitCx};

/// A unit of composition.
///
/// `build` is pure with respect to everything outside the component: it reads
/// `self` (the props the parent supplied) and `state` (owned by the element),
/// and returns a description. It must not mutate state — see
/// [`BuildCx::updater`], whose mutations are deferred to the next flush.
pub trait Component<M>: 'static {
    type State: Default + 'static;

    /// Construct the state. Runs once per mount, never again — a remount is
    /// indistinguishable from a first mount.
    ///
    /// This is where behaviors are registered ([`InitCx::register`]) and where
    /// an ambient may legally be read as a **snapshot**: the constructor does
    /// not re-run, so a value cached in a field from here does not track later
    /// changes. Values that must track an ambient are read in [`build`].
    ///
    /// [`build`]: Component::build
    fn init(&self, cx: &mut InitCx<'_, M>) -> Self::State {
        let _ = cx;
        Self::State::default()
    }

    fn build(&self, state: &Self::State, cx: &mut BuildCx<'_, M>) -> Node<M>;

    /// Whether this build can be skipped because the props are unchanged.
    ///
    /// **The reconciler's only other skip is reference identity**
    /// ([`Node::shared`]), which asks the caller to hold an `Rc<Node>` across
    /// frames. A host that derives its description from a store every frame
    /// structurally cannot do that — there is no `Rc` to keep, because the
    /// node is new each time — so for such a host the short-circuit never
    /// fires and the whole tree re-reconciles on every frame, however little
    /// changed. That is not a hypothetical: it is the state the editor
    /// integration is in.
    ///
    /// This is the other half: the component compares *what it was given*.
    /// Returning `true` skips the build and the child reconciliation below it;
    /// element state, focus and scroll are untouched, because nothing is
    /// unmounted. The default is `false` — always rebuild — so this costs
    /// nothing until a component opts in.
    ///
    /// **It must be a pure function of the props.** A component that reads an
    /// ambient in `build` and memoises on props alone will miss the ambient's
    /// change; either include what you read, or do not memoise.
    ///
    /// [`Node::shared`]: crate::desc::Node::shared
    fn memo(&self, prev: &Self) -> bool {
        let _ = prev;
        false
    }

    /// One line describing the live state, for the element dump. Opt-in,
    /// because the framework cannot format an arbitrary `State`.
    fn describe_state(&self, state: &Self::State) -> Option<String> {
        let _ = state;
        None
    }
}

/// The type-erased form stored in [`crate::desc::Desc::Component`].
pub trait AnyComponent<M> {
    fn build_any(&self, state: &dyn Any, cx: &mut BuildCx<'_, M>) -> Node<M>;
    fn init_any(&self, cx: &mut InitCx<'_, M>) -> Box<dyn Any>;
    fn describe_state_any(&self, state: &dyn Any) -> Option<String>;
    fn state_name(&self) -> &'static str;
    /// The concrete component type. Reconciliation matches on this, so two
    /// different components never update in place over one another.
    fn comp_type_id(&self) -> TypeId;
    fn comp_name(&self) -> &'static str;
    /// The erased [`Component::memo`]. `false` whenever the two are not the
    /// same concrete component, so a type change always rebuilds.
    fn props_eq(&self, prev: &dyn AnyComponent<M>) -> bool;
    /// For the downcast `props_eq` needs.
    fn as_any(&self) -> &dyn Any;
}

impl<M, C: Component<M>> AnyComponent<M> for C {
    fn build_any(&self, state: &dyn Any, cx: &mut BuildCx<'_, M>) -> Node<M> {
        let s = state
            .downcast_ref::<C::State>()
            .expect("element state does not match its component type");
        self.build(s, cx)
    }

    fn init_any(&self, cx: &mut InitCx<'_, M>) -> Box<dyn Any> {
        Box::new(self.init(cx))
    }

    fn describe_state_any(&self, state: &dyn Any) -> Option<String> {
        self.describe_state(state.downcast_ref::<C::State>()?)
    }

    fn state_name(&self) -> &'static str {
        short_name(std::any::type_name::<C::State>())
    }

    fn comp_type_id(&self) -> TypeId {
        TypeId::of::<C>()
    }

    fn comp_name(&self) -> &'static str {
        short_name(std::any::type_name::<C>())
    }

    fn props_eq(&self, prev: &dyn AnyComponent<M>) -> bool {
        match prev.as_any().downcast_ref::<C>() {
            Some(prev) => self.memo(prev),
            None => false,
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
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

/// A subtree rebuilt only when `props` changes.
///
/// The host-facing form of [`Component::memo`], for the common case where the
/// thing to compare is a value the caller already has and the build is a pure
/// function of it:
///
/// ```ignore
/// memo(status_bar_props, |p| status_bar(p))
/// ```
///
/// The builder is compared not at all — only `props` is. That is the contract:
/// a builder that closes over something outside `props` and expects to see it
/// change is a bug this cannot detect.
pub struct Memo<P, M> {
    props: P,
    build: std::rc::Rc<dyn Fn(&P) -> Node<M>>,
}

impl<P: PartialEq + 'static, M: 'static> Component<M> for Memo<P, M> {
    type State = ();

    fn build(&self, _state: &(), _cx: &mut BuildCx<'_, M>) -> Node<M> {
        (self.build)(&self.props)
    }

    fn memo(&self, prev: &Self) -> bool {
        self.props == prev.props
    }
}

/// Build `f(&props)`, and skip the rebuild on any frame where `props` is
/// unchanged. See [`Memo`].
pub fn memo<P: PartialEq + 'static, M: 'static>(
    props: P,
    f: impl Fn(&P) -> Node<M> + 'static,
) -> Node<M> {
    crate::desc::ComponentExt::node(Memo {
        props,
        build: std::rc::Rc::new(f),
    })
}
