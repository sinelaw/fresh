//! Fresh's backend over the system QuickJS (Debian's `libquickjs`).
//!
//! It mirrors the subset of rquickjs's API that Fresh uses — names,
//! signatures, conversion rules and error messages — so the plugin runtime
//! builds and behaves the same on either backend. Parts of it follow rquickjs
//! closely — its memory model (a `Ctx` holds one reference to its `JSContext`,
//! a `Value` one reference to its `JSValue` plus a `Ctx`, and the `'js`
//! lifetime keeps values from escaping the `Context::with` call that produced
//! them), its conversion rules and its error messages. rquickjs is
//! Copyright (c) 2020 Mees Delzenne, under the MIT license reproduced in
//! `LICENSE-rquickjs` next to this file.
//!
//! Every value-layout detail (tags, refcounts, the NaN-boxing on 32-bit
//! targets) goes through `fresh-quickjs-sys`'s C shim, compiled against the
//! same header as the library.

// bindgen picks the integer type of quickjs.h's constants (tags, flags) per
// header and platform, so comparisons cast them explicitly even where the cast
// is a no-op on the build machine.
#![allow(clippy::unnecessary_cast)]

pub(crate) use fresh_quickjs_sys as ffi;

pub mod class;
pub mod convert;
mod ctx;
mod error;
mod function;
mod object;
mod persistent;
pub mod runtime;
pub mod serde;
mod value;

pub use class::{Class, JsLifetime};
pub use convert::{FromJs, IntoJs};
pub use ctx::Ctx;
pub use error::{Error, Result};
pub use function::Function;
pub use object::{Array, Object};
pub use persistent::Persistent;
pub use runtime::{Context, Runtime};
pub use value::{Exception, String, Type, Value};

use std::marker::PhantomData;

/// Makes a type invariant over `'js` and neither `Send` nor `Sync`, like
/// rquickjs's handles.
pub(crate) type Invariant<'js> = PhantomData<*mut &'js ()>;
