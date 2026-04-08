//! Agent communication channel
//!
//! Handles request/response multiplexing over SSH stdin/stdout.
//! Supports transport hot-swapping for automatic reconnection:
//! the read/write tasks survive connection drops and resume when
//! a new transport is provided via `replace_transport()`.

use crate::services::remote::protocol::{AgentRequest, AgentResponse};
use std::collections::HashMap;
use std::io;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Duration;
use tokio::io::{AsyncBufRead, AsyncBufReadExt, AsyncWrite, AsyncWriteExt};
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

/// Boxed async reader type used by the read task.
type BoxedReader = Box<dyn AsyncBufRead + Unpin + Send>;
/// Boxed async writer type used by the write task.
type BoxedWriter = Box<dyn AsyncWrite + Unpin + Send>;

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
    /// Sender to deliver a new reader to the read task after reconnection
    new_reader_tx: mpsc::Sender<BoxedReader>,
    /// Sender to deliver a new writer to the write task after reconnection
    new_writer_tx: mpsc::Sender<BoxedWriter>,
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
        reader: tokio::io::BufReader<tokio::process::ChildStdout>,
        writer: tokio::process::ChildStdin,
        data_channel_capacity: usize,
    ) -> Self {
        Self::from_transport(reader, writer, data_channel_capacity)
    }

    /// Create a new channel from any async reader/writer pair.
    ///
    /// This is the generic constructor used by both production code (via
    /// `new`/`with_capacity`) and tests (via arbitrary `AsyncBufRead`/`AsyncWrite`
    /// implementations like `DuplexStream`).
    ///
    /// Must be called from within a Tokio runtime context.
    pub fn from_transport<R, W>(reader: R, writer: W, data_channel_capacity: usize) -> Self
    where
        R: AsyncBufRead + Unpin + Send + 'static,
        W: AsyncWrite + Unpin + Send + 'static,
    {
        let pending: Arc<Mutex<HashMap<u64, PendingRequest>>> =
            Arc::new(Mutex::new(HashMap::new()));
        let connected = Arc::new(std::sync::atomic::AtomicBool::new(true));
        let runtime_handle = tokio::runtime::Handle::current();

        // Channel for outgoing requests (lives for the lifetime of the AgentChannel)
        let (write_tx, write_rx) = mpsc::channel::<String>(64);

        // Channels for delivering replacement transports on reconnection.
        // Capacity 1: at most one pending reconnection at a time.
        let (new_reader_tx, new_reader_rx) = mpsc::channel::<BoxedReader>(1);
        let (new_writer_tx, new_writer_rx) = mpsc::channel::<BoxedWriter>(1);

        // Spawn write task (lives for the lifetime of the AgentChannel)
        let connected_write = connected.clone();
        tokio::spawn(Self::write_task(
            Box::new(writer),
            write_rx,
            new_writer_rx,
            connected_write,
        ));

        // Spawn read task (lives for the lifetime of the AgentChannel)
        let pending_read = pending.clone();
        let connected_read = connected.clone();
        tokio::spawn(Self::read_task(
            Box::new(reader),
            new_reader_rx,
            pending_read,
            connected_read,
        ));

        Self {
            write_tx,
            pending,
            next_id: AtomicU64::new(1),
            connected,
            runtime_handle,
            data_channel_capacity,
            request_timeout_ms: AtomicU64::new(DEFAULT_REQUEST_TIMEOUT.as_millis() as u64),
            new_reader_tx,
            new_writer_tx,
        }
    }

    /// Long-lived write task. Reads outgoing messages from `write_rx` and
    /// writes them to the current transport. On transport error or when a new
    /// transport arrives via `new_writer_rx`, switches to the new writer.
    async fn write_task(
        mut writer: BoxedWriter,
        mut write_rx: mpsc::Receiver<String>,
        mut new_writer_rx: mpsc::Receiver<BoxedWriter>,
        connected: Arc<std::sync::atomic::AtomicBool>,
    ) {
        loop {
            tokio::select! {
                // Normal path: send outgoing message
                msg = write_rx.recv() => {
                    let Some(msg) = msg else { break }; // AgentChannel dropped

                    let write_ok = writer.write_all(msg.as_bytes()).await.is_ok()
                        && writer.flush().await.is_ok();

                    if !write_ok {
                        connected.store(false, Ordering::SeqCst);
                        // Wait for replacement (can't select here, just block)
                        match new_writer_rx.recv().await {
                            Some(new_writer) => { writer = new_writer; continue; }
                            None => break,
                        }
                    }
                }
                // Reconnection: new transport arrived, switch immediately
                new_writer = new_writer_rx.recv() => {
                    match new_writer {
                        Some(w) => { writer = w; }
                        None => break, // AgentChannel dropped
                    }
                }
            }
        }
    }

    /// Long-lived read task. Reads responses from the current transport and
    /// dispatches them to pending requests. On transport error or when a new
    /// transport arrives, cleans up pending requests and switches readers.
    async fn read_task(
        mut reader: BoxedReader,
        mut new_reader_rx: mpsc::Receiver<BoxedReader>,
        pending: Arc<Mutex<HashMap<u64, PendingRequest>>>,
        connected: Arc<std::sync::atomic::AtomicBool>,
    ) {
        let mut line = String::new();

        loop {
            line.clear();

            tokio::select! {
                read_result = reader.read_line(&mut line) => {
                    match read_result {
                        Ok(0) | Err(_) => {
                            // EOF or error — transport is dead
                            connected.store(false, Ordering::SeqCst);
                            Self::drain_pending(&pending);

                            // Wait for replacement reader
                            match new_reader_rx.recv().await {
                                Some(new_reader) => { reader = new_reader; continue; }
                                None => break,
                            }
                        }
                        Ok(_) => {
                            if let Ok(resp) = serde_json::from_str::<AgentResponse>(&line) {
                                Self::handle_response(&pending, resp).await;
                            }
                        }
                    }
                }
                // Reconnection: new transport arrived, switch immediately.
                // Drain pending requests from the old connection first —
                // they were sent to the old agent and won't get responses
                // on the new one. Then mark connected so new requests can
                // be submitted.
                new_reader = new_reader_rx.recv() => {
                    match new_reader {
                        Some(r) => {
                            Self::drain_pending(&pending);
                            reader = r;
                            connected.store(true, Ordering::SeqCst);
                        }
                        None => break, // AgentChannel dropped
                    }
                }
            }
        }
    }

    /// Fail all pending requests with "connection closed" so callers don't hang.
    fn drain_pending(pending: &Arc<Mutex<HashMap<u64, PendingRequest>>>) {
        let mut pending = pending.lock().unwrap();
        for (id, req) in pending.drain() {
            match req.result_tx.send(Err("connection closed".to_string())) {
                Ok(()) => {}
                Err(_) => {
                    warn!("request {id}: receiver dropped during disconnect cleanup");
                }
            }
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

    /// Replace the underlying transport with a new reader/writer pair.
    ///
    /// This is used for reconnection: after establishing a new SSH connection,
    /// call this method to feed the new stdin/stdout to the existing read/write
    /// tasks. The tasks will resume processing and `is_connected()` will return
    /// `true` once the first successful read/write completes.
    ///
    /// The `connected` flag is set to `true` by the read task after it has
    /// received the new reader and drained stale pending requests. This
    /// ensures no race between draining and new request submission.
    pub async fn replace_transport<R, W>(&self, reader: R, writer: W)
    where
        R: AsyncBufRead + Unpin + Send + 'static,
        W: AsyncWrite + Unpin + Send + 'static,
    {
        // Send new transports to the tasks. Order matters: send writer first
        // so the write task is ready before the read task marks connected
        // (which allows new requests to flow).
        // Send can only fail if the task exited (AgentChannel dropped).
        if self.new_writer_tx.send(Box::new(writer)).await.is_err() {
            warn!("replace_transport: write task is gone, cannot reconnect");
            return;
        }
        if self.new_reader_tx.send(Box::new(reader)).await.is_err() {
            warn!("replace_transport: read task is gone, cannot reconnect");
        }
        // Note: connected is set to true by the read task after it drains
        // stale pending requests and switches to the new reader.
    }

    /// Replace the underlying transport (blocking version for non-async contexts).
    ///
    /// Sends the new transport to the tasks and waits until the channel is
    /// marked as connected (i.e., the read task has drained stale requests
    /// and is ready to receive responses on the new reader).
    pub fn replace_transport_blocking<R, W>(&self, reader: R, writer: W)
    where
        R: AsyncBufRead + Unpin + Send + 'static,
        W: AsyncWrite + Unpin + Send + 'static,
    {
        self.runtime_handle
            .block_on(self.replace_transport(reader, writer));

        // Yield until the read task has processed the new reader.
        // This is typically immediate since the channel send above wakes
        // the read task's select!, which drains pending and sets connected.
        while !self.is_connected() {
            std::thread::yield_now();
        }
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

    /// Send a streaming request synchronously, returning receivers for
    /// incremental processing.
    ///
    /// Unlike `request_with_data_blocking` which collects all data into
    /// memory, this returns the raw receivers so callers can process each
    /// chunk as it arrives (e.g., for `walk_files` where the server sends
    /// file paths in batches).
    ///
    /// Use `data_rx.blocking_recv()` to receive chunks from a sync context.
    #[allow(clippy::type_complexity)]
    pub fn request_streaming_blocking(
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
        self.runtime_handle
            .block_on(self.request_streaming(method, params))
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
