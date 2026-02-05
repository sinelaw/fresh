//! Server-side implementation for session persistence
//!
//! The server runs as a daemon and holds all editor state. Clients connect
//! via IPC (Unix domain sockets or Windows named pipes) to send input and
//! receive rendered output.
//!
//! ## Architecture
//!
//! - **Data socket**: Pure byte stream for stdin/stdout relay (hot path)
//! - **Control socket**: JSON messages for resize, ping/pong, etc (cold path)
//!
//! See `docs/internal/session-persistence-design.md` for full design.

pub mod capture_backend;
pub mod daemon;
pub mod editor_server;
pub mod input_parser;
pub mod ipc;
pub mod protocol;

#[cfg(test)]
mod runner;
#[cfg(test)]
mod tests;

pub use capture_backend::{terminal_setup_sequences, terminal_teardown_sequences, CaptureBackend};
pub use daemon::{
    daemonize, is_process_running, read_pid_file, spawn_server_detached, write_pid_file,
};
pub use editor_server::{EditorServer, EditorServerConfig};
pub use input_parser::InputParser;
pub use ipc::{ServerListener, SocketPaths};
pub use protocol::{ClientHello, ControlMessage, ServerHello, PROTOCOL_VERSION};
#[cfg(test)]
pub use runner::{Server, ServerConfig};
