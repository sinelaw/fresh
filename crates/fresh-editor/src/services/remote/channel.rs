//! Agent communication channel
//!
//! Handles request/response multiplexing over SSH stdin/stdout.

use crate::services::remote::protocol::{AgentRequest, AgentResponse};
use std::collections::HashMap;
use std::io;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Duration;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt};
use tokio::sync::{mpsc, oneshot};
use tracing::warn;

/// Default capacity for the per-request streaming data channel.
const DEFAULT_DATA_CHANNEL_CAPACITY: usize = 64;

/// Default timeout for remote requests. If a response is not received within
/// this duration, the request fails with `ChannelError::Timeout` and the
/// connection is marked as disconnected.
const DEFAULT_REQUEST_TIMEOUT: Duration = Duration::from_secs(10);

/// Test-only: microseconds to sleep in the consumer loop between chunks.
/// Set to a non-zero value from tests to simulate a slow consumer and
/// deterministically reproduce channel backpressure scenarios.
/// Always compiled (not cfg(test)) because integration tests need access.
pub static TEST_RECV_DELAY_US: AtomicU64 = AtomicU64::new(0);

/// Error type for channel operations
#[derive(Debug, thiserror::Error)]
pub enum ChannelError {
    #[error("IO error: {0}")]
    Io(#[from] io::Error),

    #[error("JSON error: {0}")]
    Json(#[from] serde_json::Error),

    #[error("Channel closed")]
    ChannelClosed,

    #[error("Request cancelled")]
    Cancelled,

    #[error("Request timed out")]
    Timeout,

    #[error("Remote error: {0}")]
    Remote(String),
}

/// Pending request state
struct PendingRequest {
    /// Channel for streaming data
    data_tx: mpsc::Sender<serde_json::Value>,
    /// Channel for final result
    result_tx: oneshot::Sender<Result<serde_json::Value, String>>,
}

/// Communication channel with the remote agent
pub struct AgentChannel {
    /// Sender to the write task
    write_tx: mpsc::Sender<String>,
    /// Pending requests awaiting responses
    pending: Arc<Mutex<HashMap<u64, PendingRequest>>>,
    /// Next request ID
    next_id: AtomicU64,
    /// Whether the channel is connected
    connected: Arc<std::sync::atomic::AtomicBool>,
    /// Runtime handle for blocking operations
    runtime_handle: tokio::runtime::Handle,
    /// Capacity for per-request streaming data channels
    data_channel_capacity: usize,
    /// Timeout for individual requests (stored as milliseconds for atomic access)
    request_timeout_ms: AtomicU64,
}

impl AgentChannel {
    /// Create a new channel from async read/write handles
    ///
    /// Must be called from within a Tokio runtime context.
    pub fn new(
        reader: tokio::io::BufReader<tokio::process::ChildStdout>,
        writer: tokio::process::ChildStdin,
    ) -> Self {
        Self::with_capacity(reader, writer, DEFAULT_DATA_CHANNEL_CAPACITY)
    }

    /// Create a new channel with a custom data channel capacity.
    ///
    /// Lower capacity makes channel overflow more likely if `try_send` is used,
    /// which is useful for stress-testing backpressure handling.
    pub fn with_capacity(
        mut reader: tokio::io::BufReader<tokio::process::ChildStdout>,
        mut writer: tokio::process::ChildStdin,
        data_channel_capacity: usize,
    ) -> Self {
        let pending: Arc<Mutex<HashMap<u64, PendingRequest>>> =
            Arc::new(Mutex::new(HashMap::new()));
        let connected = Arc::new(std::sync::atomic::AtomicBool::new(true));
        // Capture the runtime handle for later use in blocking operations
        let runtime_handle = tokio::runtime::Handle::current();

        // Channel for outgoing requests
        let (write_tx, mut write_rx) = mpsc::channel::<String>(64);

        // Spawn write task
        let connected_write = connected.clone();
        tokio::spawn(async move {
            while let Some(msg) = write_rx.recv().await {
                if writer.write_all(msg.as_bytes()).await.is_err() {
                    connected_write.store(false, Ordering::SeqCst);
                    break;
                }
                if writer.flush().await.is_err() {
                    connected_write.store(false, Ordering::SeqCst);
                    break;
                }
            }
        });

        // Spawn read task
        let pending_read = pending.clone();
        let connected_read = connected.clone();
        tokio::spawn(async move {
            let mut line = String::new();
            loop {
                line.clear();
                match reader.read_line(&mut line).await {
                    Ok(0) => {
                        // EOF
                        connected_read.store(false, Ordering::SeqCst);
                        break;
                    }
                    Ok(_) => {
                        if let Ok(resp) = serde_json::from_str::<AgentResponse>(&line) {
                            Self::handle_response(&pending_read, resp).await;
                        }
                    }
                    Err(_) => {
                        connected_read.store(false, Ordering::SeqCst);
                        break;
                    }
                }
            }

            // Clean up pending requests on disconnect.
            let mut pending = pending_read.lock().unwrap();
            for (id, req) in pending.drain() {
                match req.result_tx.send(Err("connection closed".to_string())) {
                    Ok(()) => {}
                    Err(_) => {
                        // Receiver was dropped before we could notify it.
                        // This is unexpected — callers should hold their
                        // receivers until the operation completes.
                        warn!("request {id}: receiver dropped during disconnect cleanup");
                    }
                }
            }
        });

        Self {
            write_tx,
            pending,
            next_id: AtomicU64::new(1),
            connected,
            runtime_handle,
            data_channel_capacity,
            request_timeout_ms: AtomicU64::new(DEFAULT_REQUEST_TIMEOUT.as_millis() as u64),
        }
    }

    /// Handle an incoming response.
    ///
    /// For streaming data, uses `send().await` to apply backpressure when the
    /// consumer is slower than the producer. This prevents silent data loss
    /// that occurred with `try_send` (#1059).
    async fn handle_response(
        pending: &Arc<Mutex<HashMap<u64, PendingRequest>>>,
        resp: AgentResponse,
    ) {
        // Send streaming data without holding the mutex (send().await may yield)
        if let Some(data) = resp.data {
            let data_tx = {
                let pending = pending.lock().unwrap();
                pending.get(&resp.id).map(|req| req.data_tx.clone())
            };
            if let Some(tx) = data_tx {
                // send().await blocks until the consumer drains a slot, providing
                // backpressure instead of silently dropping data.
                if tx.send(data).await.is_err() {
                    // Receiver was dropped — this is unexpected since callers
                    // should hold data_rx until the stream ends. Clean up the
                    // pending entry to avoid leaking the dead request.
                    warn!("request {}: data receiver dropped mid-stream", resp.id);
                    let mut pending = pending.lock().unwrap();
                    pending.remove(&resp.id);
                    return;
                }
            }
        }

        // Handle final result/error
        if resp.result.is_some() || resp.error.is_some() {
            let mut pending = pending.lock().unwrap();
            if let Some(req) = pending.remove(&resp.id) {
                let outcome = if let Some(result) = resp.result {
                    req.result_tx.send(Ok(result))
                } else if let Some(error) = resp.error {
                    req.result_tx.send(Err(error))
                } else {
                    // resp matched the outer condition (result or error is Some)
                    // but neither branch fired — unreachable by construction.
                    return;
                };
                match outcome {
                    Ok(()) => {}
                    Err(_) => {
                        // Receiver was dropped — this is unexpected since
                        // callers should hold result_rx until they get a result.
                        warn!("request {}: result receiver dropped", resp.id);
                    }
                }
            }
        }
    }

    /// Check if the channel is connected
    pub fn is_connected(&self) -> bool {
        self.connected.load(Ordering::SeqCst)
    }

    /// Set the request timeout duration.
    ///
    /// Requests that don't receive a response within this duration will fail
    /// with `ChannelError::Timeout` and the connection will be marked as
    /// disconnected.
    pub fn set_request_timeout(&self, timeout: Duration) {
        self.request_timeout_ms
            .store(timeout.as_millis() as u64, Ordering::SeqCst);
    }

    /// Get the current request timeout duration.
    fn request_timeout(&self) -> Duration {
        Duration::from_millis(self.request_timeout_ms.load(Ordering::SeqCst))
    }

    /// Send a request and wait for the final result (ignoring streaming data)
    pub async fn request(
        &self,
        method: &str,
        params: serde_json::Value,
    ) -> Result<serde_json::Value, ChannelError> {
        let (mut data_rx, result_rx) = self.request_streaming(method, params).await?;

        let timeout = self.request_timeout();

        // Drain streaming data and wait for final result, with timeout.
        let result = tokio::time::timeout(timeout, async {
            while data_rx.recv().await.is_some() {}
            result_rx
                .await
                .map_err(|_| ChannelError::ChannelClosed)?
                .map_err(ChannelError::Remote)
        })
        .await;

        match result {
            Ok(inner) => inner,
            Err(_elapsed) => {
                warn!("request '{}' timed out after {:?}", method, timeout);
                self.connected.store(false, Ordering::SeqCst);
                Err(ChannelError::Timeout)
            }
        }
    }

    /// Send a request that may stream data
    pub async fn request_streaming(
        &self,
        method: &str,
        params: serde_json::Value,
    ) -> Result<
        (
            mpsc::Receiver<serde_json::Value>,
            oneshot::Receiver<Result<serde_json::Value, String>>,
        ),
        ChannelError,
    > {
        if !self.is_connected() {
            return Err(ChannelError::ChannelClosed);
        }

        let id = self.next_id.fetch_add(1, Ordering::SeqCst);

        // Create channels for response
        let (data_tx, data_rx) = mpsc::channel(self.data_channel_capacity);
        let (result_tx, result_rx) = oneshot::channel();

        // Register pending request
        {
            let mut pending = self.pending.lock().unwrap();
            pending.insert(id, PendingRequest { data_tx, result_tx });
        }

        // Build and send request
        let req = AgentRequest::new(id, method, params);
        self.write_tx
            .send(req.to_json_line())
            .await
            .map_err(|_| ChannelError::ChannelClosed)?;

        Ok((data_rx, result_rx))
    }

    /// Send a request synchronously (blocking)
    ///
    /// This can be called from outside the Tokio runtime context.
    pub fn request_blocking(
        &self,
        method: &str,
        params: serde_json::Value,
    ) -> Result<serde_json::Value, ChannelError> {
        self.runtime_handle.block_on(self.request(method, params))
    }

    /// Send a request and collect all streaming data along with the final result
    pub async fn request_with_data(
        &self,
        method: &str,
        params: serde_json::Value,
    ) -> Result<(Vec<serde_json::Value>, serde_json::Value), ChannelError> {
        let (mut data_rx, result_rx) = self.request_streaming(method, params).await?;

        let timeout = self.request_timeout();

        let result = tokio::time::timeout(timeout, async {
            // Collect all streaming data
            let mut data = Vec::new();
            while let Some(chunk) = data_rx.recv().await {
                data.push(chunk);

                // Test hook: simulate slow consumer for backpressure testing.
                // Zero-cost in production (atomic load + branch-not-taken).
                let delay_us = TEST_RECV_DELAY_US.load(Ordering::Relaxed);
                if delay_us > 0 {
                    tokio::time::sleep(tokio::time::Duration::from_micros(delay_us)).await;
                }
            }

            // Wait for final result
            let result = result_rx
                .await
                .map_err(|_| ChannelError::ChannelClosed)?
                .map_err(ChannelError::Remote)?;

            Ok((data, result))
        })
        .await;

        match result {
            Ok(inner) => inner,
            Err(_elapsed) => {
                warn!("streaming request timed out after {:?}", timeout);
                self.connected.store(false, Ordering::SeqCst);
                Err(ChannelError::Timeout)
            }
        }
    }

    /// Send a request with streaming data, synchronously (blocking)
    ///
    /// This can be called from outside the Tokio runtime context.
    pub fn request_with_data_blocking(
        &self,
        method: &str,
        params: serde_json::Value,
    ) -> Result<(Vec<serde_json::Value>, serde_json::Value), ChannelError> {
        self.runtime_handle
            .block_on(self.request_with_data(method, params))
    }

    /// Cancel a request
    pub async fn cancel(&self, request_id: u64) -> Result<(), ChannelError> {
        use crate::services::remote::protocol::cancel_params;
        self.request("cancel", cancel_params(request_id)).await?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    // Tests are in the tests module to allow integration testing with mock agent
}
