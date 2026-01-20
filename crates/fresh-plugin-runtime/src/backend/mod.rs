//! Plugin backend abstraction layer
//!
//! This module provides the JavaScript runtime backend for executing TypeScript plugins.
//! Currently implements QuickJS with oxc transpilation.

pub mod quickjs_backend;

pub use quickjs_backend::{
    has_fatal_js_error, set_panic_on_js_errors, take_fatal_js_error, PendingResponses,
    QuickJsBackend, TsPluginInfo,
};
