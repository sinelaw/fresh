// Editor library - exposes all core modules for testing

#[cfg(feature = "plugins")]
pub mod v8_init;

// Core modules at root level
pub mod config;
pub mod session;
pub mod state;

// Organized modules
pub mod app;
pub mod input;
pub mod model;
pub mod primitives;
pub mod services;
pub mod view;
