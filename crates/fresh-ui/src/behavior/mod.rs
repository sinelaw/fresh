//! Reusable stateful concerns, enrolled for ordered teardown.
//!
//! A behavior is an object held in a named field of a component's state and
//! registered with the element that owns that state. `Drop` handles teardown in
//! most cases; registration is what is needed where teardown must run in a
//! defined order, or before the tree is dismantled.
//!
//! Two orderings hold, and every later primitive depends on them:
//!
//! - across the tree, **children before parents**, so a child releasing a
//!   parent-owned handle finds the parent alive;
//! - within one state object, **reverse registration order**.
//!
//! The shipped set (`Tasks`, `Ticker`, `Cache`, `Controller`/`Anchor`,
//! `Focusable`, `Persisted`) arrives with the phases that need it. This module
//! is the substrate they register through.

/// Something with a teardown step.
///
/// Teardown takes `&self` because behaviors are shared between the element's
/// registry and the state field that holds them; a behavior with mutable
/// internals owns a `RefCell`, which it needs regardless — `build` only ever
/// sees `&State`.
pub mod tasks;

pub use tasks::{TaskHandle, Tasks};

pub trait Behavior {
    /// Handed the services when the behavior is registered — a spawner, the
    /// scheduler, the registries. Behaviors that need none ignore it.
    fn attach(&self, services: &crate::services::Services) {
        let _ = services;
    }

    /// Called once, when the owning element is disposed.
    fn teardown(&self) {}

    /// For the element dump.
    fn behavior_name(&self) -> &'static str {
        "Behavior"
    }

    /// Hand over anything that arrived from elsewhere. Called between frames,
    /// never during build, layout or paint. Behaviors with no inbox do nothing.
    fn pump(&self) -> usize {
        0
    }
}
