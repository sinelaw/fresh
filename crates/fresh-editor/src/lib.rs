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

// Organized modules (runtime-only)
#[cfg(feature = "runtime")]
pub mod app;
#[cfg(feature = "runtime")]
pub mod input;
#[cfg(feature = "runtime")]
pub mod model;
#[cfg(feature = "runtime")]
pub mod primitives;
#[cfg(feature = "runtime")]
pub mod services;
#[cfg(feature = "runtime")]
pub mod view;
