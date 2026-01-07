//! Input pipeline
//!
//! This module handles the input-to-action-to-event translation.

pub mod actions;
pub mod buffer_mode;
pub mod command_registry;
pub mod commands;
pub mod composite_router;
pub mod fuzzy;
pub mod handler;
pub mod input_history;
pub mod key_translator;
pub mod keybindings;
pub mod multi_cursor;
pub mod position_history;

#[cfg(test)]
pub mod tests_language_features;
