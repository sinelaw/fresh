//! The JavaScript engine boundary for Fresh's plugin runtime.
//!
//! Everything in Fresh that touches the JS engine goes through this crate
//! instead of naming the engine directly. The items below are the whole
//! contract between Fresh and its JS engine, and there are two backends that
//! provide them:
//!
//! - **rquickjs** (the default): rquickjs with its bundled quickjs-ng. Every
//!   item is a plain re-export, so this layer costs nothing.
//! - **system** (`RUSTFLAGS="--cfg fresh_js_system"`): Fresh's own backend over
//!   the system QuickJS that Debian ships (`libquickjs`, through
//!   `fresh-quickjs-sys`). It provides the same names with the same
//!   signatures and the same conversion and error behaviour, for the parts of
//!   rquickjs's API that Fresh uses. See `docs/internal/debian-quickjs-spike.md`.
//!
//! The backend is a cfg rather than a cargo feature so that `--all-features`
//! builds (which CI runs on every platform) never need libquickjs.
//!
//! The list is explicit rather than a glob re-export: reaching for something
//! new from rquickjs means adding it here, and to the system backend, which
//! keeps the cost of the second backend visible at the point the contract
//! grows.
//!
//! With the rquickjs backend, rquickjs's proc macros (`#[class]`, `#[methods]`,
//! `#[derive(Trace, JsLifetime)]`) are re-exported here too and callers spell
//! them `fresh_js::…`. They locate the `rquickjs` crate by reading the
//! *calling* crate's manifest, though, so a crate that uses them still lists
//! rquickjs as a direct dependency (for that backend only), for those macros
//! alone.

#[cfg(not(fresh_js_system))]
mod backend {
    pub use rquickjs::{
        Array, Class, Context, Ctx, Error, FromJs, Function, IntoJs, JsLifetime, Object,
        Persistent, Result, Runtime, String, Type, Value,
    };

    /// Function-argument adapters.
    pub mod function {
        pub use rquickjs::function::{Opt, Rest};
    }

    /// Evaluation options.
    pub mod context {
        pub use rquickjs::context::EvalOptions;
    }

    /// Class support: the `class` module (for `class::Trace`) and, in the
    /// macro namespace, the `#[class]` attribute.
    pub use rquickjs::class;

    /// The `#[methods]` attribute that exposes an impl block's methods to JS.
    pub use rquickjs::methods;

    /// Conversion between JS values and serde types.
    pub mod serde {
        pub use rquickjs_serde::{from_value, to_value};
    }
}

#[cfg(fresh_js_system)]
mod system;

#[cfg(fresh_js_system)]
mod backend {
    pub use crate::system::{
        Array, Class, Context, Ctx, Error, Exception, FromJs, Function, IntoJs, JsLifetime, Object,
        Persistent, Result, Runtime, String, Type, Value,
    };

    /// Function-argument adapters.
    pub mod function {
        pub use crate::system::convert::{Opt, Rest};
    }

    /// Evaluation options.
    pub mod context {
        pub use crate::system::runtime::EvalOptions;
    }

    /// Class support.
    pub mod class {
        pub use crate::system::class::{JsClass, JsMethods, Trace, Tracer};
        pub use fresh_js_macros::Trace;
    }

    pub use fresh_js_macros::{class, methods, JsLifetime};

    /// Conversion between JS values and serde types.
    pub mod serde {
        pub use crate::system::serde::{from_value, to_value, Error};
    }

    /// Internals the system backend's proc macros expand to. Not part of the
    /// contract.
    #[doc(hidden)]
    pub mod __private {
        pub use crate::system::class::{MethodDef, MethodFn};
        pub use crate::system::convert::{FromParam, ParamRequirement, Params, ParamsAccessor};
    }
}

pub use backend::*;
