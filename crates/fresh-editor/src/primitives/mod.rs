//! Low-level primitives and utilities
//!
//! This module contains syntax highlighting, ANSI handling,
//! and text manipulation utilities.
//!
//! # WASM Compatibility
//!
//! Most primitives are WASM-compatible. Tree-sitter-based features have
//! pure-Rust fallbacks:
//!
//! | Feature | WASM Module | Runtime Module |
//! |---------|-------------|----------------|
//! | Syntax highlighting | `textmate_engine` | `highlight_engine` |
//! | Auto-indentation | `indent_pattern` | `indent` |
//! | Reference highlighting | `reference_highlight_text` | `reference_highlighter` |

// Pure modules - available for both runtime and WASM
pub mod display_width;
pub mod grapheme;
pub mod line_wrapping;
pub mod path_utils;
pub mod snippet;
pub mod text_property;

// Modules depending on model::buffer - available for both runtime and WASM
pub mod line_iterator;
pub mod word_navigation;

// Modules using ratatui types (Color, Style, etc.) - available for both runtime and WASM
// since ratatui core is WASM-compatible (only the crossterm backend is native-only)
#[cfg(any(feature = "runtime", feature = "wasm"))]
pub mod ansi;
#[cfg(any(feature = "runtime", feature = "wasm"))]
pub mod ansi_background;
#[cfg(any(feature = "runtime", feature = "wasm"))]
pub mod visual_layout;

// Grammar module - uses syntect which is WASM-compatible with fancy-regex feature
#[cfg(any(feature = "runtime", feature = "wasm"))]
pub mod grammar;
#[cfg(any(feature = "runtime", feature = "wasm"))]
pub use grammar::GrammarRegistry;

// Common highlight types - WASM-compatible
#[cfg(any(feature = "runtime", feature = "wasm"))]
pub mod highlight_types;

// WASM-compatible highlighting, indentation, and reference highlighting
// These provide pure-Rust implementations without tree-sitter
#[cfg(any(feature = "runtime", feature = "wasm"))]
pub mod indent_pattern;
#[cfg(any(feature = "runtime", feature = "wasm"))]
pub mod reference_highlight_text;
#[cfg(any(feature = "runtime", feature = "wasm"))]
pub mod textmate_engine;

// Runtime-only modules (depend on tree-sitter)
// These provide enhanced features using AST analysis
#[cfg(feature = "runtime")]
pub mod highlight_engine;
#[cfg(feature = "runtime")]
pub mod highlighter;
#[cfg(feature = "runtime")]
pub mod indent;
#[cfg(feature = "runtime")]
pub mod reference_highlighter;
