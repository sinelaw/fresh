// Error types for the Fresh editor
//!
//! This module defines error types used throughout the editor.
//! We use thiserror for ergonomic error definitions.

use std::path::PathBuf;

/// Main error type for editor operations
#[derive(Debug, thiserror::Error)]
pub enum EditorError {
    /// I/O errors (file operations, etc.)
    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),

    /// Configuration parsing/loading errors
    #[error("Configuration error: {0}")]
    Config(#[from] ConfigError),

    /// Plugin system errors
    #[error("Plugin error: {0}")]
    Plugin(String),

    /// LSP related errors
    #[error("LSP error: {0}")]
    Lsp(String),

    /// Invalid input from user
    #[error("Invalid input: {0}")]
    InvalidInput(String),

    /// Lock poisoning error
    #[error("Internal lock poisoned - this indicates a bug")]
    LockPoisoned,

    /// Generic internal error
    #[error("Internal error: {0}")]
    Internal(String),
}

/// Configuration-specific errors
#[derive(Debug, thiserror::Error)]
pub enum ConfigError {
    /// JSON parsing error
    #[error("Failed to parse configuration: {0}")]
    ParseError(#[from] serde_json::Error),

    /// Invalid configuration value
    #[error("Invalid value for '{field}': {value}")]
    InvalidValue { field: String, value: String },

    /// Missing required field
    #[error("Missing required configuration field: {0}")]
    MissingRequired(String),

    /// File not found
    #[error("Configuration file not found: {0}")]
    FileNotFound(PathBuf),
}

/// Result type alias for editor operations
pub type Result<T> = std::result::Result<T, EditorError>;

/// Result type alias for configuration operations
pub type ConfigResult<T> = std::result::Result<T, ConfigError>;

/// Convert lock poisoning into an EditorError
impl<T> From<std::sync::PoisonError<T>> for EditorError {
    fn from(_: std::sync::PoisonError<T>) -> Self {
        EditorError::LockPoisoned
    }
}
