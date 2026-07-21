//! Server-side input parsing.
//!
//! The parser itself now lives in the standalone [`fresh_input_parser`] crate,
//! a DEC/ANSI state machine that converts the raw client byte stream into
//! crossterm events without ever leaking control-sequence bytes as literal
//! input (see sinelaw/fresh#2745). This module re-exports it so existing call
//! sites (`crate::server::input_parser::InputParser`) keep working.

pub use fresh_input_parser::InputParser;
