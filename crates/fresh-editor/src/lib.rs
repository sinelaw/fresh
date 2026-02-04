// Editor library - exposes all core modules for testing

// Initialize i18n with translations from locales/ directory
rust_i18n::i18n!("locales", fallback = "en", minify_key = true);

pub mod i18n;

// Core types and config are always available (needed for schema generation)
pub mod config;
pub mod partial_config;
pub mod types;

// Runtime-only modules (require the "runtime" feature)
#[cfg(feature = "runtime")]
pub mod config_io;
#[cfg(feature = "runtime")]
pub mod state;
#[cfg(feature = "runtime")]
pub mod workspace;

// Core modules - always available (pure Rust, no platform dependencies)
// Submodules within primitives that need ratatui/syntect are internally gated
pub mod model;
pub mod primitives;

// Runtime-only modules (heavy dependencies, platform-specific)
#[cfg(feature = "runtime")]
pub mod app;
#[cfg(feature = "runtime")]
pub mod input;
#[cfg(feature = "runtime")]
pub mod services;

// Session persistence (client-server architecture)
#[cfg(feature = "runtime")]
pub mod client;
#[cfg(feature = "runtime")]
pub mod server;

// View module - available for runtime, WASM, and dev-bins (schema generation)
// Most submodules are runtime-only, but theme types are always available
#[cfg(any(feature = "runtime", feature = "wasm", feature = "dev-bins"))]
pub mod view;

// WASM-specific modules
#[cfg(feature = "wasm")]
pub mod wasm;
