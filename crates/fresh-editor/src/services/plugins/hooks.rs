//! Hook System: Event subscription and notification for plugins
//!
//! Re-exports hook system types from fresh-core for backward compatibility.

pub use fresh_core::hooks::{
    hook_args_to_json, HookArgs, HookCallback, HookRegistry, LineInfo, LspLocation,
};
