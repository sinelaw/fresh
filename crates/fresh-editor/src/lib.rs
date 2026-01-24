// Editor library - exposes all core modules for testing

// Initialize i18n with translations from locales/ directory
rust_i18n::i18n!("locales", fallback = "en");

pub mod i18n;

// Core types and config are always available (needed for schema generation)
pub mod config;
pub mod partial_config;
pub mod types;

// Runtime-only modules (require the "runtime" feature)
#[cfg(feature = "runtime")]
pub mod config_io;
#[cfg(feature = "runtime")]
pub mod session;
#[cfg(feature = "runtime")]
pub mod state;

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

// View module - available for both runtime and WASM
// Most submodules are runtime-only, but theme types are WASM-compatible
#[cfg(any(feature = "runtime", feature = "wasm"))]
pub mod view;

// WASM-specific modules
#[cfg(feature = "wasm")]
pub mod wasm;
