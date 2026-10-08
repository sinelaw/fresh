//! Raw bindings to the system QuickJS library.
//!
//! Generated at build time by bindgen from the installed `quickjs.h` (see
//! `build.rs` for how it is found), plus the `fqjs_*` shim functions from
//! `shim.c` that stand in for the header's static-inline helpers and value
//! macros. Use [`fresh-quickjs`](../fresh_quickjs) for a safe API.
#![allow(non_upper_case_globals, non_camel_case_types, non_snake_case)]
#![allow(clippy::all)]

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));
