//! QuickLSP: High-Performance, Heuristic-Driven Universal Language Server
//!
//! Single unified `Workspace` engine that tokenizes all files and serves
//! every LSP operation from one data structure.

pub mod deps;
pub mod fuzzy;
pub mod lsp;
pub mod parsing;
pub mod workspace;

pub use deps::DependencyIndex;
pub use lsp::server::QuickLspServer;
pub use workspace::Workspace;
