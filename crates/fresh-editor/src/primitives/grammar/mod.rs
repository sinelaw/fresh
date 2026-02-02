//! Grammar registry module with separated pure types and I/O operations.
//!
//! This module is split into:
//! - `types`: Pure data types and lookup methods (WASM-compatible, no filesystem access)
//! - `loader`: I/O operations with `GrammarLoader` trait abstraction (runtime only)
//!
//! # Example
//!
//! ```ignore
//! use crate::primitives::grammar::{GrammarRegistry, GrammarLoader, LocalGrammarLoader};
//!
//! // Create empty registry (no I/O, for tests)
//! let registry = GrammarRegistry::empty();
//!
//! // Create default registry with embedded grammars only
//! let registry = GrammarRegistry::default();
//!
//! // Load registry with user grammars (runtime only)
//! #[cfg(feature = "runtime")]
//! {
//!     let config_dir = std::path::PathBuf::from("/home/user/.config/fresh");
//!     let registry = GrammarRegistry::for_editor(config_dir.clone());
//!     // Or manually:
//!     let loader = LocalGrammarLoader::new(config_dir);
//!     let registry = GrammarRegistry::load(&loader);
//! }
//! ```

// Loader requires filesystem access - runtime only
#[cfg(feature = "runtime")]
mod loader;
mod types;

// Re-export all public items for backward compatibility
#[cfg(feature = "runtime")]
pub use loader::*;
pub use types::*;
