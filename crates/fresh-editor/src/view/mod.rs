//! View and UI layer
//!
//! This module contains all presentation and rendering components.
//!
//! # WASM Compatibility
//!
//! Many view modules are WASM-compatible since they use ratatui (pure rendering)
//! and crossterm types (pure data structures). Modules that depend on runtime-only
//! code (app, state, config_io, input, tree-sitter) are gated behind runtime feature.

// Theme data + loader live in `fresh-editor-core` (config and the render
// primitives below it both need them); re-exported here so `view::theme`
// keeps resolving.
pub use fresh_editor_core::theme;

// WASM-compatible modules (pure rendering, no runtime deps)
#[cfg(any(feature = "runtime", feature = "wasm"))]
pub mod activation;
#[cfg(any(feature = "runtime", feature = "wasm"))]
pub mod animation;
#[cfg(any(feature = "runtime", feature = "wasm"))]
pub use fresh_editor_core::color_support;
#[cfg(any(feature = "runtime", feature = "wasm"))]
pub mod composite_view;
#[cfg(any(feature = "runtime", feature = "wasm"))]
pub mod conceal;
#[cfg(any(feature = "runtime", feature = "wasm"))]
pub mod controls;
#[cfg(any(feature = "runtime", feature = "wasm"))]
pub mod dimming;
#[cfg(any(feature = "runtime", feature = "wasm"))]
pub mod folding;
#[cfg(any(feature = "runtime", feature = "wasm"))]
pub mod line_wrap_cache;
#[cfg(any(feature = "runtime", feature = "wasm"))]
pub mod margin;
#[cfg(any(feature = "runtime", feature = "wasm"))]
pub mod overlay;
#[cfg(any(feature = "runtime", feature = "wasm"))]
pub mod scroll_sync;
#[cfg(any(feature = "runtime", feature = "wasm"))]
pub mod scrollbar_marker;
#[cfg(any(feature = "runtime", feature = "wasm"))]
pub mod soft_break;
#[cfg(any(feature = "runtime", feature = "wasm"))]
pub mod ui;
#[cfg(any(feature = "runtime", feature = "wasm"))]
pub mod viewport;
#[cfg(any(feature = "runtime", feature = "wasm"))]
pub mod virtual_text;

/// Byte <-> visual row, repaired rather than invalidated.
pub mod wrap_index;

/// The single wrap rule — see the module docs.
pub use fresh_editor_core::wrap_machine;

// Settings module has internal gating (schema is WASM-compatible)
#[cfg(any(feature = "runtime", feature = "wasm"))]
pub mod settings;
pub mod workspace_trust_dialog;

// Runtime-only modules (depend on app, state, config_io, input, or tree-sitter)
#[cfg(feature = "runtime")]
pub mod bracket_highlight_overlay;
#[cfg(feature = "runtime")]
#[cfg(feature = "runtime")]
pub mod cursor_line_overlay;
#[cfg(feature = "runtime")]
pub mod diff_gutter;
#[cfg(feature = "runtime")]
#[cfg(feature = "runtime")]
pub mod file_browser_input;
#[cfg(feature = "runtime")]
pub mod file_tree;
#[cfg(feature = "runtime")]
pub mod keybinding_editor;
#[cfg(feature = "runtime")]
pub use fresh_editor_core::markdown;
#[cfg(feature = "runtime")]
pub mod popup;
#[cfg(feature = "runtime")]
#[cfg(feature = "runtime")]
#[cfg(feature = "runtime")]
pub mod prompt;
#[cfg(feature = "runtime")]
pub mod prompt_input;
#[cfg(feature = "runtime")]
pub mod query_replace_input;
#[cfg(feature = "runtime")]
pub mod reference_highlight_overlay;
pub mod scene;
pub mod shell;
#[cfg(feature = "runtime")]
pub mod split;
#[cfg(feature = "runtime")]
pub mod stream;
