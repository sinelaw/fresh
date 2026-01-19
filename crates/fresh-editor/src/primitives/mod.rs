//! Low-level primitives and utilities
//!
//! This module contains syntax highlighting, ANSI handling,
//! and text manipulation utilities.

pub mod ansi;
pub mod ansi_background;
pub mod display_width;
pub mod grammar;
pub mod grapheme;

// Re-export for backward compatibility
pub use grammar::GrammarRegistry;
pub mod highlight_engine;
pub mod highlighter;
pub mod indent;
pub mod line_iterator;
pub mod line_wrapping;
pub mod path_utils;
pub mod reference_highlighter;
pub mod snippet;
pub mod text_property;
pub mod visual_layout;
pub mod word_navigation;
