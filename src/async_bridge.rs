//! Async Bridge: Communication between async Tokio runtime and sync main loop
//!
//! This module implements the hybrid architecture described in TOKIO_ANALYSIS.md:
//! - Tokio runtime handles I/O tasks (LSP, file watching, git, etc.)
//! - Main UI loop stays synchronous (rendering, input, buffer manipulation)
//! - std::sync::mpsc channels bridge the two worlds
//!
//! Philosophy:
//! - I/O should be async (LSP, filesystem, network)
//! - Computation should be sync (editing, rendering)
//! - Main loop remains responsive and simple

use lsp_types::Diagnostic;
use std::sync::mpsc;

/// Messages sent from async tasks to the synchronous main loop
#[derive(Debug, Clone)]
pub enum AsyncMessage {
    /// LSP diagnostics received for a file
    LspDiagnostics {
        uri: String,
        diagnostics: Vec<Diagnostic>,
    },

    /// LSP server initialized successfully
    LspInitialized { language: String },

    /// LSP server crashed or failed
    LspError { language: String, error: String },

    /// File changed externally (future: file watching)
    FileChanged { path: String },

    /// Git status updated (future: git integration)
    GitStatusChanged { status: String },
}

/// Bridge between async Tokio runtime and sync main loop
///
/// Design:
/// - Lightweight, cloneable sender that can be passed to async tasks
/// - Non-blocking receiver checked each frame in main loop
/// - No locks needed in main loop (channel handles synchronization)
#[derive(Clone)]
pub struct AsyncBridge {
    sender: mpsc::Sender<AsyncMessage>,
    // Receiver wrapped in Arc<Mutex<>> to allow cloning
    receiver: std::sync::Arc<std::sync::Mutex<mpsc::Receiver<AsyncMessage>>>,
}

impl AsyncBridge {
    /// Create a new async bridge with an unbounded channel
    ///
    /// Unbounded is appropriate here because:
    /// 1. Main loop processes messages every 16ms (60fps)
    /// 2. LSP messages are infrequent (< 100/sec typically)
    /// 3. Memory usage is bounded by message rate × frame time
    pub fn new() -> Self {
        let (sender, receiver) = mpsc::channel();
        Self {
            sender,
            receiver: std::sync::Arc::new(std::sync::Mutex::new(receiver)),
        }
    }

    /// Get a cloneable sender for async tasks
    ///
    /// This sender can be:
    /// - Cloned freely (cheap Arc internally)
    /// - Sent to async tasks
    /// - Stored in LspClient instances
    pub fn sender(&self) -> mpsc::Sender<AsyncMessage> {
        self.sender.clone()
    }

    /// Try to receive pending messages (non-blocking)
    ///
    /// Called each frame in the main loop to process async messages.
    /// Returns all pending messages without blocking.
    pub fn try_recv_all(&self) -> Vec<AsyncMessage> {
        let mut messages = Vec::new();

        // Lock the receiver and drain all pending messages
        if let Ok(receiver) = self.receiver.lock() {
            while let Ok(msg) = receiver.try_recv() {
                messages.push(msg);
            }
        }

        messages
    }

    /// Check if there are pending messages (non-blocking)
    pub fn has_messages(&self) -> bool {
        // Note: This is racy but safe - only used for optimization
        if let Ok(receiver) = self.receiver.lock() {
            receiver.try_recv().is_ok()
        } else {
            false
        }
    }
}

impl Default for AsyncBridge {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_async_bridge_send_receive() {
        let bridge = AsyncBridge::new();
        let sender = bridge.sender();

        // Send a message
        let msg = AsyncMessage::LspInitialized {
            language: "rust".to_string(),
        };
        sender.send(msg.clone()).unwrap();

        // Receive it
        let messages = bridge.try_recv_all();
        assert_eq!(messages.len(), 1);

        match &messages[0] {
            AsyncMessage::LspInitialized { language } => {
                assert_eq!(language, "rust");
            }
            _ => panic!("Wrong message type"),
        }
    }

    #[test]
    fn test_async_bridge_multiple_messages() {
        let bridge = AsyncBridge::new();
        let sender = bridge.sender();

        // Send multiple messages
        sender
            .send(AsyncMessage::LspInitialized {
                language: "rust".to_string(),
            })
            .unwrap();
        sender
            .send(AsyncMessage::LspInitialized {
                language: "typescript".to_string(),
            })
            .unwrap();

        // Receive all at once
        let messages = bridge.try_recv_all();
        assert_eq!(messages.len(), 2);
    }

    #[test]
    fn test_async_bridge_no_messages() {
        let bridge = AsyncBridge::new();

        // Try to receive with no messages
        let messages = bridge.try_recv_all();
        assert_eq!(messages.len(), 0);
    }

    #[test]
    fn test_async_bridge_clone_sender() {
        let bridge = AsyncBridge::new();
        let sender1 = bridge.sender();
        let sender2 = sender1.clone();

        // Both senders work
        sender1
            .send(AsyncMessage::LspInitialized {
                language: "rust".to_string(),
            })
            .unwrap();
        sender2
            .send(AsyncMessage::LspInitialized {
                language: "typescript".to_string(),
            })
            .unwrap();

        let messages = bridge.try_recv_all();
        assert_eq!(messages.len(), 2);
    }
}
