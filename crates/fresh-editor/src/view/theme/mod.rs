//! Theme module with separated pure types and I/O operations.
//!
//! This module is split into:
//! - `types`: Pure data types (WASM-compatible, no filesystem access)
//! - `loader`: ThemeLoader creates ThemeRegistry from embedded + user themes (runtime only)
//!
//! # Example
//!
//! ```ignore
//! use crate::view::theme::{Theme, ThemeLoader, ThemeRegistry};
//!
//! // Load all themes once at startup
//! let loader = ThemeLoader::new();
//! let registry = loader.load_all();
//!
//! // Get theme by name (no I/O, just lookup)
//! let dark = registry.get("dark").unwrap();
//!
//! // List all available themes
//! let themes = registry.list();
//! ```

// Loader requires filesystem access - runtime only
#[cfg(feature = "runtime")]
mod loader;
mod types;
#[cfg(feature = "runtime")]
mod validate;

// Re-export all public items for backward compatibility
#[cfg(feature = "runtime")]
pub use loader::*;
pub use types::*;
#[cfg(feature = "runtime")]
pub use validate::*;
