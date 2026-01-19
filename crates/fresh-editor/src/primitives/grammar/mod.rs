//! Grammar registry module with separated pure types and I/O operations.
//!
//! This module is split into:
//! - `types`: Pure data types and lookup methods (WASM-compatible, no filesystem access)
//! - `loader`: I/O operations with `GrammarLoader` trait abstraction
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
//! // Load registry with user grammars using default loader
//! let registry = GrammarRegistry::for_editor();
//!
//! // Load registry with custom loader (for testing)
//! let loader = LocalGrammarLoader::new();
//! let registry = GrammarRegistry::load(&loader);
//! ```

mod loader;
mod types;

// Re-export all public items for backward compatibility
pub use loader::*;
pub use types::*;
