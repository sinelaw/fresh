//! Terminal Manager - manages multiple terminal sessions
//!
//! This module provides a manager for terminal sessions that:
//! - Spawns PTY processes with proper shell detection
//! - Manages multiple concurrent terminals
//! - Routes input/output between the editor and terminal processes
//! - Handles terminal resize events

use super::term::TerminalState;
use crate::services::async_bridge::AsyncBridge;
use portable_pty::{native_pty_system, CommandBuilder, PtySize};
use std::collections::HashMap;
use std::io::{Read, Write};
use std::sync::mpsc;
use std::sync::{Arc, Mutex};
use std::thread;

/// Unique identifier for a terminal session
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TerminalId(pub usize);

impl std::fmt::Display for TerminalId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Terminal-{}", self.0)
    }
}

/// Messages sent to terminal I/O thread
enum TerminalCommand {
    /// Write data to PTY
    Write(Vec<u8>),
    /// Resize the PTY
    Resize { cols: u16, rows: u16 },
    /// Shutdown the terminal
    Shutdown,
}

/// Handle to a running terminal session
pub struct TerminalHandle {
    /// Terminal state (grid, cursor, etc.)
    pub state: Arc<Mutex<TerminalState>>,
    /// Command sender to I/O thread
    command_tx: mpsc::Sender<TerminalCommand>,
    /// Whether the terminal is still alive
    alive: Arc<std::sync::atomic::AtomicBool>,
    /// Current dimensions
    cols: u16,
    rows: u16,
}

impl TerminalHandle {
    /// Write data to the terminal (sends to PTY)
    pub fn write(&self, data: &[u8]) {
        let _ = self.command_tx.send(TerminalCommand::Write(data.to_vec()));
    }

    /// Resize the terminal
    pub fn resize(&mut self, cols: u16, rows: u16) {
        if cols != self.cols || rows != self.rows {
            self.cols = cols;
            self.rows = rows;
            let _ = self.command_tx.send(TerminalCommand::Resize { cols, rows });
            // Also resize the terminal state
            if let Ok(mut state) = self.state.lock() {
                state.resize(cols, rows);
            }
        }
    }

    /// Check if the terminal is still running
    pub fn is_alive(&self) -> bool {
        self.alive.load(std::sync::atomic::Ordering::Relaxed)
    }

    /// Shutdown the terminal
    pub fn shutdown(&self) {
        let _ = self.command_tx.send(TerminalCommand::Shutdown);
    }

    /// Get current dimensions
    pub fn size(&self) -> (u16, u16) {
        (self.cols, self.rows)
    }
}

/// Manager for multiple terminal sessions
pub struct TerminalManager {
    /// Map from terminal ID to handle
    terminals: HashMap<TerminalId, TerminalHandle>,
    /// Next terminal ID
    next_id: usize,
    /// Async bridge for sending notifications to main loop
    async_bridge: Option<AsyncBridge>,
}

impl TerminalManager {
    /// Create a new terminal manager
    pub fn new() -> Self {
        Self {
            terminals: HashMap::new(),
            next_id: 0,
            async_bridge: None,
        }
    }

    /// Set the async bridge for communication with main loop
    pub fn set_async_bridge(&mut self, bridge: AsyncBridge) {
        self.async_bridge = Some(bridge);
    }

    /// Spawn a new terminal session
    ///
    /// # Arguments
    /// * `cols` - Initial terminal width in columns
    /// * `rows` - Initial terminal height in rows
    /// * `cwd` - Optional working directory (defaults to current directory)
    ///
    /// # Returns
    /// The terminal ID if successful
    pub fn spawn(
        &mut self,
        cols: u16,
        rows: u16,
        cwd: Option<std::path::PathBuf>,
    ) -> Result<TerminalId, String> {
        let id = TerminalId(self.next_id);
        self.next_id += 1;

        // Create PTY
        let pty_system = native_pty_system();
        let pty_pair = pty_system
            .openpty(PtySize {
                rows,
                cols,
                pixel_width: 0,
                pixel_height: 0,
            })
            .map_err(|e| format!("Failed to open PTY: {}", e))?;

        // Detect shell
        let shell = detect_shell();
        tracing::info!("Spawning terminal with shell: {}", shell);

        // Build command
        let mut cmd = CommandBuilder::new(&shell);
        if let Some(ref dir) = cwd {
            cmd.cwd(dir);
        }

        // Spawn the shell process
        let mut child = pty_pair
            .slave
            .spawn_command(cmd)
            .map_err(|e| format!("Failed to spawn shell: {}", e))?;

        // Create terminal state
        let state = Arc::new(Mutex::new(TerminalState::new(cols, rows)));

        // Create communication channel
        let (command_tx, command_rx) = mpsc::channel::<TerminalCommand>();

        // Alive flag
        let alive = Arc::new(std::sync::atomic::AtomicBool::new(true));
        let alive_clone = alive.clone();

        // Get master for I/O
        let mut master = pty_pair
            .master
            .take_writer()
            .map_err(|e| format!("Failed to get PTY writer: {}", e))?;

        let mut reader = pty_pair
            .master
            .try_clone_reader()
            .map_err(|e| format!("Failed to get PTY reader: {}", e))?;

        // Clone state for reader thread
        let state_clone = state.clone();
        let async_bridge = self.async_bridge.clone();

        // Spawn reader thread
        let terminal_id = id;
        thread::spawn(move || {
            let mut buf = [0u8; 4096];
            loop {
                match reader.read(&mut buf) {
                    Ok(0) => {
                        // EOF - process exited
                        tracing::info!("Terminal {:?} EOF", terminal_id);
                        break;
                    }
                    Ok(n) => {
                        // Process output through terminal emulator
                        if let Ok(mut state) = state_clone.lock() {
                            state.process_output(&buf[..n]);
                        }
                        // Notify main loop to redraw
                        if let Some(ref bridge) = async_bridge {
                            let _ = bridge.sender().send(
                                crate::services::async_bridge::AsyncMessage::TerminalOutput {
                                    terminal_id,
                                },
                            );
                        }
                    }
                    Err(e) => {
                        tracing::error!("Terminal read error: {}", e);
                        break;
                    }
                }
            }
            alive_clone.store(false, std::sync::atomic::Ordering::Relaxed);
            // Notify that terminal exited
            if let Some(ref bridge) = async_bridge {
                let _ = bridge
                    .sender()
                    .send(crate::services::async_bridge::AsyncMessage::TerminalExited {
                        terminal_id,
                    });
            }
        });

        // Spawn writer thread
        let pty_size_ref = pty_pair.master;
        thread::spawn(move || {
            loop {
                match command_rx.recv() {
                    Ok(TerminalCommand::Write(data)) => {
                        if let Err(e) = master.write_all(&data) {
                            tracing::error!("Terminal write error: {}", e);
                            break;
                        }
                        let _ = master.flush();
                    }
                    Ok(TerminalCommand::Resize { cols, rows }) => {
                        if let Err(e) = pty_size_ref.resize(PtySize {
                            rows,
                            cols,
                            pixel_width: 0,
                            pixel_height: 0,
                        }) {
                            tracing::warn!("Failed to resize PTY: {}", e);
                        }
                    }
                    Ok(TerminalCommand::Shutdown) | Err(_) => {
                        break;
                    }
                }
            }
            // Clean up child process
            let _ = child.kill();
            let _ = child.wait();
        });

        // Create handle
        let handle = TerminalHandle {
            state,
            command_tx,
            alive,
            cols,
            rows,
        };

        self.terminals.insert(id, handle);
        tracing::info!("Created terminal {:?} ({}x{})", id, cols, rows);

        Ok(id)
    }

    /// Get a terminal handle by ID
    pub fn get(&self, id: TerminalId) -> Option<&TerminalHandle> {
        self.terminals.get(&id)
    }

    /// Get a mutable terminal handle by ID
    pub fn get_mut(&mut self, id: TerminalId) -> Option<&mut TerminalHandle> {
        self.terminals.get_mut(&id)
    }

    /// Close a terminal
    pub fn close(&mut self, id: TerminalId) -> bool {
        if let Some(handle) = self.terminals.remove(&id) {
            handle.shutdown();
            true
        } else {
            false
        }
    }

    /// Get all terminal IDs
    pub fn terminal_ids(&self) -> Vec<TerminalId> {
        self.terminals.keys().copied().collect()
    }

    /// Get count of open terminals
    pub fn count(&self) -> usize {
        self.terminals.len()
    }

    /// Shutdown all terminals
    pub fn shutdown_all(&mut self) {
        for (_, handle) in self.terminals.drain() {
            handle.shutdown();
        }
    }

    /// Clean up dead terminals
    pub fn cleanup_dead(&mut self) -> Vec<TerminalId> {
        let dead: Vec<TerminalId> = self
            .terminals
            .iter()
            .filter(|(_, h)| !h.is_alive())
            .map(|(id, _)| *id)
            .collect();

        for id in &dead {
            self.terminals.remove(id);
        }

        dead
    }
}

impl Default for TerminalManager {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for TerminalManager {
    fn drop(&mut self) {
        self.shutdown_all();
    }
}

/// Detect the user's shell
fn detect_shell() -> String {
    // Try $SHELL environment variable first
    if let Ok(shell) = std::env::var("SHELL") {
        if !shell.is_empty() {
            return shell;
        }
    }

    // Fall back to platform defaults
    #[cfg(unix)]
    {
        "/bin/sh".to_string()
    }
    #[cfg(windows)]
    {
        std::env::var("COMSPEC").unwrap_or_else(|_| "cmd.exe".to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_terminal_id_display() {
        let id = TerminalId(42);
        assert_eq!(format!("{}", id), "Terminal-42");
    }

    #[test]
    fn test_detect_shell() {
        let shell = detect_shell();
        assert!(!shell.is_empty());
    }
}
