// Editor library - exposes all core modules for testing

#[cfg(feature = "plugins")]
pub mod v8_init;

// Core types and config are always available (needed for schema generation)
pub mod config;
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
