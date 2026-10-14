//! Server-side implementation for daemon persistence
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
pub mod command_access;
pub mod daemon;
pub mod editor_server;
pub mod input_parser;
pub mod ipc;
pub mod local_control;
pub mod protocol;

#[cfg(test)]
mod tests;

pub use capture_backend::{terminal_setup_sequences, terminal_teardown_sequences, CaptureBackend};
pub use daemon::{
    daemonize, is_process_running, read_pid_file, spawn_server_detached, write_pid_file,
    DaemonSpawn,
};

/// The daemon every bare `fresh` shares when Orchestrator mode is on.
///
/// One fixed name rather than the usual per-working-directory keying, because
/// the whole point of the mode is that `fresh` typed in *any* directory joins
/// the editor you were already in. It shows up under this name in
/// `fresh --cmd daemon list`, and `fresh -a orchestrator` attaches to it
/// explicitly.
pub const ORCHESTRATOR_DAEMON: &str = "orchestrator";
pub use editor_server::{EditorServer, EditorServerConfig};
pub use input_parser::InputParser;
pub use ipc::{ServerListener, ServerLiveness, SocketPaths};
pub use protocol::{ClientHello, ControlMessage, ServerHello, PROTOCOL_VERSION};

/// Helpers shared by the server's own test modules, which live in two files
/// (`editor_server.rs` and `tests.rs`).
#[cfg(test)]
pub(crate) mod test_support {
    /// Recovery chunk files anywhere under `dir`, which is how a test observes
    /// that a dirty buffer was actually written to recovery storage. The real
    /// layout is `<scope>/<slug>/{id}.chunk.N`, so this walks rather than
    /// reading one level.
    pub(crate) fn recovery_chunk_files(dir: &std::path::Path) -> Vec<std::path::PathBuf> {
        let mut found = Vec::new();
        let mut stack = vec![dir.to_path_buf()];
        while let Some(next) = stack.pop() {
            let Ok(entries) = std::fs::read_dir(&next) else {
                continue;
            };
            for entry in entries.flatten() {
                let path = entry.path();
                if path.is_dir() {
                    stack.push(path);
                } else if path
                    .file_name()
                    .and_then(|n| n.to_str())
                    .is_some_and(|n| n.contains(".chunk."))
                {
                    found.push(path);
                }
            }
        }
        found
    }
}
