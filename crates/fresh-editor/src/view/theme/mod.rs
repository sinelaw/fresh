//! Theme module with separated pure types and I/O operations.
//!
//! This module is split into:
//! - `types`: Pure data types (WASM-compatible, no filesystem access)
//! - `loader`: I/O operations with `ThemeLoader` trait abstraction
//!
//! # Example
//!
//! ```ignore
//! use crate::view::theme::{Theme, ThemeLoader, LocalThemeLoader};
//!
//! // Load builtin theme (no I/O)
//! let dark = Theme::load_builtin("dark").unwrap();
//!
//! // Load theme with loader (for user themes)
//! let loader = LocalThemeLoader::new();
//! let theme = Theme::load("dark", &loader).unwrap();
//!
//! // List all available themes
//! let themes = Theme::all_available(&loader);
//! ```

mod loader;
mod types;

// Re-export all public items for backward compatibility
pub use loader::*;
pub use types::*;
