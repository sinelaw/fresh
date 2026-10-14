//! The Linux console mouse, through GPM (General Purpose Mouse).
//!
//! Not parsing — the console mouse never reaches the byte stream this crate
//! parses — but the other half of terminal input on a Linux console, so it
//! lives beside the parser.
//!
//! On a Linux virtual console (TTY) the mouse does not arrive on stdin as
//! terminal mouse reports: the GPM daemon owns it and hands each event to the
//! program over its own socket, through libgpm. This module loads libgpm at
//! runtime — so a system without it still runs, just without the console
//! mouse — and turns its events into what the rest of Fresh speaks:
//!
//! - a crossterm [`MouseEvent`](crossterm::event::MouseEvent), for an editor
//!   reading the console itself ([`gpm_to_crossterm`]);
//! - the SGR mouse report a terminal would have written to stdin, for a
//!   daemon client that forwards bytes to an editor running elsewhere
//!   ([`mouse_to_sgr`](crate::mouse_to_sgr), [`GpmClient::read_sgr_reports`]).
//!
//! Linux only.
//!
//! # Architecture
//!
//! - `ffi.rs` - libgpm, loaded with `dlopen`
//! - `types.rs` - GPM events, buttons, modifiers
//! - `client.rs` - the connection to the GPM daemon
//! - `convert.rs` - GPM events as crossterm events

mod client;
mod convert;
mod ffi;
mod types;

pub use client::GpmClient;
pub use convert::gpm_to_crossterm;
pub use types::{GpmButtons, GpmEvent, GpmEventType, GpmModifiers};
