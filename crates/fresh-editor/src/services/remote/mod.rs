//! SSH remote editing support
//!
//! This module provides remote file system access and process execution
//! via an SSH connection to a Python agent running on the remote host.

mod channel;
mod connection;
mod filesystem;
mod protocol;
mod spawner;

pub use channel::AgentChannel;
/// Test-only global: microseconds to sleep per chunk in the consumer loop.
/// Defaults to 0 (no delay). Set non-zero from tests to simulate slow consumers.
#[doc(hidden)]
pub use channel::TEST_RECV_DELAY_US;
/// Re-export for integration tests - spawns a local agent without SSH
#[doc(hidden)]
pub use connection::spawn_local_agent;
/// Spawns a local agent and returns raw reader/writer for reconnection testing.
#[doc(hidden)]
pub use connection::spawn_local_agent_transport;
/// Like `spawn_local_agent` but with a custom data channel capacity.
#[doc(hidden)]
pub use connection::spawn_local_agent_with_capacity;
pub use connection::{spawn_reconnect_task, spawn_reconnect_task_with, ReconnectConfig};
pub use connection::{ConnectionParams, SshConnection, SshError};
pub use filesystem::RemoteFileSystem;
pub use protocol::{
    decode_base64, encode_base64, ls_params, read_params, stat_params, sudo_write_params,
    write_params, AgentRequest, AgentResponse,
};
pub use spawner::{
    LocalProcessSpawner, ProcessSpawner, RemoteProcessSpawner, SpawnError, SpawnResult,
};

/// The Python agent source code, embedded at compile time.
pub const AGENT_SOURCE: &str = include_str!("agent.py");

#[cfg(test)]
mod tests;
