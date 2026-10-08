//! The JavaScript engine boundary for Fresh's plugin runtime.
//!
//! Everything in Fresh that touches the JS engine goes through this crate
//! instead of naming the engine directly. Today the engine is rquickjs (with
//! its bundled quickjs-ng), and every item below is a plain re-export of it, so
//! this layer costs nothing and changes no behaviour.
//!
//! The point is the *list*: it is the whole contract between Fresh and its JS
//! engine. A second backend — the system QuickJS that Debian ships, see
//! `docs/internal/debian-quickjs-spike.md` — only has to provide these names
//! with these signatures for the plugin runtime to build on it unchanged. So
//! the list is explicit rather than a glob re-export: reaching for something
//! new from rquickjs means adding it here, which makes the cost of the next
//! backend visible at the point it grows.
//!
//! rquickjs's proc macros (`#[class]`, `#[methods]`, `#[derive(Trace,
//! JsLifetime)]`) are re-exported here too, and callers spell them
//! `fresh_js::…`. They locate the `rquickjs` crate by reading the *calling*
//! crate's manifest, though, so a crate that uses them still lists rquickjs as
//! a direct dependency, for those macros alone.

pub use rquickjs::{
    Array, Class, Context, Ctx, Error, FromJs, Function, IntoJs, JsLifetime, Object, Persistent,
    Result, Runtime, String, Type, Value,
};

/// Function-argument adapters.
pub mod function {
    pub use rquickjs::function::{Opt, Rest};
}

/// Evaluation options.
pub mod context {
    pub use rquickjs::context::EvalOptions;
}

/// Class support: the `class` module (for `class::Trace`) and, in the macro
/// namespace, the `#[class]` attribute.
pub use rquickjs::class;

/// The `#[methods]` attribute that exposes an impl block's methods to JS.
pub use rquickjs::methods;

/// Conversion between JS values and serde types.
pub mod serde {
    pub use rquickjs_serde::{from_value, to_value};
}
