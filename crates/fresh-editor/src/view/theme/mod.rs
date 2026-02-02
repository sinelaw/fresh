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
//! use std::path::PathBuf;
//!
//! // Load all themes once at startup (requires themes directory path)
//! let themes_dir = PathBuf::from("/home/user/.config/fresh/themes");
//! let loader = ThemeLoader::new(themes_dir);
//! let registry = loader.load_all();
//!
//! // Or load embedded themes only (no user themes)
//! let loader = ThemeLoader::embedded_only();
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

// Re-export all public items for backward compatibility
#[cfg(feature = "runtime")]
pub use loader::*;
pub use types::*;
