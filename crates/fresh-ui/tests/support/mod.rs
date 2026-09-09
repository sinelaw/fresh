//! Shared scaffolding for the integration tests and the example binaries.
//!
//! This is not part of the `fresh-ui` library: the library `src/` tree holds
//! only the library. What lives here is a reference character backend, a
//! recording renderer, and a complete demo application built on the public API
//! — the things the tests assert against and the example binaries drive. Each
//! test binary and each example includes this tree with `mod support;` (or a
//! `#[path]` include), so Cargo compiles a private copy per target; the
//! allow(dead_code) covers the items any one target does not touch.
#![allow(dead_code, unused_imports)]

pub mod demo;
pub mod fake;
pub mod screen;
