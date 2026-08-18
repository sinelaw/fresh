//! A retained reconciling UI tree.
//!
//! Three trees, in order of cost:
//!
//! 1. **Descriptions** ([`Node`]) — immutable values, rebuilt freely, no
//!    identity and no side effects.
//! 2. **Elements** — the persistent instances descriptions are matched against
//!    by `(type, key)` at a position. They own component state, and they are
//!    what survives a rebuild.
//! 3. **Render objects** — geometry, hit-testing and focus registration. Added
//!    by the layout phase.
//!
//! This crate depends on no backend, no terminal library and no part of the
//! editor. It emits a display list; something else draws it.
//!
//! ## What is implemented
//!
//! Descriptions, the element tree and reconciliation, the scheduler, behavior
//! registration and teardown, ambient values, and tree diagnostics. Layout,
//! paint, pointer routing, focus and the widget set follow in later phases; the
//! primitives' props exist and are inert until then.

#![forbid(unsafe_code)]

pub mod ambient;
pub mod behavior;
pub mod component;
pub mod desc;
pub mod diagnose;
pub mod element;
pub mod event;
pub mod focus;
pub mod hit;
pub mod key;
pub mod render;
pub mod schedule;
pub mod test;

pub use ambient::{provide, Ambient, AmbientKey, ProvideProps};
pub use behavior::Behavior;
pub use component::{AnyComponent, Component};
pub use desc::{
    col, focusable, gesture, host, layer, layout_reader, node_key, node_type, resolve, row,
    shared_rc, stack, text, viewport, Align, Anchor, BoxProps, ComponentExt, Desc, Dir, Dismiss,
    ElemType, Fit, FocusProps, GestureProps, Handler, HostId, LayerProps, LayoutReaderProps,
    Listener, Modality, Node, Pad, Place, PointerMode, Scrim, Sizing, TextProps, ViewportProps,
};
pub use element::ElementId;
pub use event::{
    Button, Event, Flow, GestureKind, Input, KeyCode, KeyPress, Mods, Phase, SelectionOnFocus,
};
pub use focus::{
    default_shortcuts, Directional, FocusDir, FocusEntry, FocusScope, Intent, ReadingOrder,
    Shortcut, TraversalPolicy,
};
pub use key::{Key, KeyPath};
pub use render::geom::{distribute, Constraints, Point, Rect, Size};
pub use render::spec::{CursorSpec, Draw, Item, LayoutSpec, ThemeKey};
pub use schedule::{BuildCx, DirtyCause, InitCx, NullRenderer, Renderer, Sched, Ui, Updater};
