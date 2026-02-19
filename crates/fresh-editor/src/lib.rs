#![deny(clippy::let_underscore_must_use)]

// Editor library - exposes all core modules for testing

pub mod i18n;

// Initialize i18n with empty directory (no compile-time code generation)
// All translations are provided by the runtime backend
rust_i18n::i18n!(
    "locales-empty",
    fallback = "en",
    backend = i18n::runtime_backend::RuntimeBackend::new()
);

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

// GUI mode - native window with wgpu rendering
#[cfg(feature = "gui")]
pub mod gui;

// WASM-specific modules
#[cfg(feature = "wasm")]
pub mod wasm;
