//! Terminal emulation service for Fresh
//!
//! This module provides built-in terminal support using:
//! - `alacritty_terminal` for terminal emulation (VT100/ANSI parsing, grid management)
//! - `portable-pty` for cross-platform PTY management
//!
//! Architecture follows the hybrid approach recommended in docs/TERMINAL.md:
//! - Core terminal infrastructure in Rust
//! - Terminal displayed as special buffer type (BufferKind::Virtual with mode "terminal")
//! - Input routed to PTY when terminal buffer is focused

mod manager;
pub mod pty;
pub mod term;

pub use manager::{detect_shell, TerminalId, TerminalManager};
pub use term::{TerminalCell, TerminalState};
