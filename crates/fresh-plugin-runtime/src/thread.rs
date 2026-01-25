//! Plugin Thread: Dedicated thread for TypeScript plugin execution
//!
//! This module implements a dedicated thread architecture for plugin execution,
//! using QuickJS as the JavaScript runtime with oxc for TypeScript transpilation.
//!
//! Architecture:
//! - Main thread (UI) sends requests to plugin thread via channel
//! - Plugin thread owns QuickJS runtime and persistent tokio runtime
//! - Results are sent back via the existing PluginCommand channel
//! - Async operations complete naturally without runtime destruction

use crate::backend::quickjs_backend::{PendingResponses, TsPluginInfo};
use crate::backend::QuickJsBackend;
use anyhow::{anyhow, Result};
use fresh_core::api::{EditorStateSnapshot, PluginCommand};
use fresh_core::hooks::HookArgs;
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::{Arc, RwLock};
use std::thread::{self, JoinHandle};
use std::time::Duration;

// Re-export PluginConfig from fresh-core
pub use fresh_core::config::PluginConfig;

/// Request messages sent to the plugin thread
#[derive(Debug)]
pub enum PluginRequest {
    /// Load a plugin from a file
    LoadPlugin {
        path: PathBuf,
        response: oneshot::Sender<Result<()>>,
    },

    /// Resolve an async callback with a result (for async operations like SpawnProcess, Delay)
    ResolveCallback {
        callback_id: fresh_core::api::JsCallbackId,
        result_json: String,
    },

    /// Reject an async callback with an error
    RejectCallback {
        callback_id: fresh_core::api::JsCallbackId,
        error: String,
    },

    /// Load all plugins from a directory
    LoadPluginsFromDir {
        dir: PathBuf,
        response: oneshot::Sender<Vec<String>>,
    },

    /// Load all plugins from a directory with config support
    /// Returns (errors, discovered_plugins) where discovered_plugins contains
    /// all found plugins with their paths and enabled status
    LoadPluginsFromDirWithConfig {
        dir: PathBuf,
        plugin_configs: HashMap<String, PluginConfig>,
        response: oneshot::Sender<(Vec<String>, HashMap<String, PluginConfig>)>,
    },

    /// Unload a plugin by name
    UnloadPlugin {
        name: String,
        response: oneshot::Sender<Result<()>>,
    },

    /// Reload a plugin by name
    ReloadPlugin {
        name: String,
        response: oneshot::Sender<Result<()>>,
    },

    /// Execute a plugin action
    ExecuteAction {
        action_name: String,
        response: oneshot::Sender<Result<()>>,
    },

    /// Run a hook (fire-and-forget, no response needed)
    RunHook { hook_name: String, args: HookArgs },

    /// Check if any handlers are registered for a hook
    HasHookHandlers {
        hook_name: String,
        response: oneshot::Sender<bool>,
    },

    /// List all loaded plugins
    ListPlugins {
        response: oneshot::Sender<Vec<TsPluginInfo>>,
    },

    /// Shutdown the plugin thread
    Shutdown,
}

/// Simple oneshot channel implementation
pub mod oneshot {
    use std::fmt;
    use std::sync::mpsc;

    pub struct Sender<T>(mpsc::SyncSender<T>);
    pub struct Receiver<T>(mpsc::Receiver<T>);

    use anyhow::Result;

    impl<T> fmt::Debug for Sender<T> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.debug_tuple("Sender").finish()
        }
    }

    impl<T> fmt::Debug for Receiver<T> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.debug_tuple("Receiver").finish()
        }
    }

    impl<T> Sender<T> {
        pub fn send(self, value: T) -> Result<(), T> {
            self.0.send(value).map_err(|e| e.0)
        }
    }

    impl<T> Receiver<T> {
        pub fn recv(self) -> Result<T, mpsc::RecvError> {
            self.0.recv()
        }

        pub fn recv_timeout(
            self,
            timeout: std::time::Duration,
        ) -> Result<T, mpsc::RecvTimeoutError> {
            self.0.recv_timeout(timeout)
        }

        pub fn try_recv(&self) -> Result<T, mpsc::TryRecvError> {
            self.0.try_recv()
        }
    }

    pub fn channel<T>() -> (Sender<T>, Receiver<T>) {
        let (tx, rx) = mpsc::sync_channel(1);
        (Sender(tx), Receiver(rx))
    }
}

/// Handle to the plugin thread for sending requests
pub struct PluginThreadHandle {
    /// Channel to send requests to the plugin thread
    /// Wrapped in Option so we can drop it to signal shutdown
    request_sender: Option<tokio::sync::mpsc::UnboundedSender<PluginRequest>>,

    /// Thread join handle
    thread_handle: Option<JoinHandle<()>>,

    /// State snapshot handle for editor to update
    state_snapshot: Arc<RwLock<EditorStateSnapshot>>,

    /// Pending response senders for async operations (shared with runtime)
    pending_responses: PendingResponses,

    /// Receiver for plugin commands (polled by editor directly)
    command_receiver: std::sync::mpsc::Receiver<PluginCommand>,
}

impl PluginThreadHandle {
    /// Create a new plugin thread and return its handle
    pub fn spawn(services: Arc<dyn fresh_core::services::PluginServiceBridge>) -> Result<Self> {
        tracing::debug!("PluginThreadHandle::spawn: starting plugin thread creation");

        // Create channel for plugin commands
        let (command_sender, command_receiver) = std::sync::mpsc::channel();

        // Create editor state snapshot for query API
        let state_snapshot = Arc::new(RwLock::new(EditorStateSnapshot::new()));

        // Create pending responses map (shared between handle and runtime)
        let pending_responses: PendingResponses =
            Arc::new(std::sync::Mutex::new(std::collections::HashMap::new()));
        let thread_pending_responses = Arc::clone(&pending_responses);

        // Create channel for requests (unbounded allows sync send, async recv)
        let (request_sender, request_receiver) = tokio::sync::mpsc::unbounded_channel();

        // Clone state snapshot for the thread
        let thread_state_snapshot = Arc::clone(&state_snapshot);

        // Spawn the plugin thread
        tracing::debug!("PluginThreadHandle::spawn: spawning OS thread for plugin runtime");
        let thread_handle = thread::spawn(move || {
            tracing::debug!("Plugin thread: OS thread started, creating tokio runtime");
            // Create tokio runtime for the plugin thread
            let rt = match tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
            {
                Ok(rt) => {
                    tracing::debug!("Plugin thread: tokio runtime created successfully");
                    rt
                }
                Err(e) => {
                    tracing::error!("Failed to create plugin thread runtime: {}", e);
                    return;
                }
            };

            // Create QuickJS runtime with state
            tracing::debug!("Plugin thread: creating QuickJS runtime");
            let runtime = match QuickJsBackend::with_state_and_responses(
                Arc::clone(&thread_state_snapshot),
                command_sender,
                thread_pending_responses,
                services.clone(),
            ) {
                Ok(rt) => {
                    tracing::debug!("Plugin thread: QuickJS runtime created successfully");
                    rt
                }
                Err(e) => {
                    tracing::error!("Failed to create QuickJS runtime: {}", e);
                    return;
                }
            };

            // Create internal manager state
            let mut plugins: HashMap<String, TsPluginInfo> = HashMap::new();

            // Run the event loop with a LocalSet to allow concurrent task execution
            tracing::debug!("Plugin thread: starting event loop with LocalSet");
            let local = tokio::task::LocalSet::new();
            local.block_on(&rt, async {
                // Wrap runtime in RefCell for interior mutability during concurrent operations
                let runtime = Rc::new(RefCell::new(runtime));
                tracing::debug!("Plugin thread: entering plugin_thread_loop");
                plugin_thread_loop(runtime, &mut plugins, request_receiver).await;
            });

            tracing::info!("Plugin thread shutting down");
        });

        tracing::debug!("PluginThreadHandle::spawn: OS thread spawned, returning handle");
        tracing::info!("Plugin thread spawned");

        Ok(Self {
            request_sender: Some(request_sender),
            thread_handle: Some(thread_handle),
            state_snapshot,
            pending_responses,
            command_receiver,
        })
    }

    /// Check if the plugin thread is still alive
    pub fn is_alive(&self) -> bool {
        self.thread_handle
            .as_ref()
            .map(|h| !h.is_finished())
            .unwrap_or(false)
    }

    /// Check thread health and panic if the plugin thread died due to a panic.
    /// This propagates plugin thread panics to the calling thread.
    /// Call this periodically to detect plugin thread failures.
    pub fn check_thread_health(&mut self) {
        if let Some(handle) = &self.thread_handle {
            if handle.is_finished() {
                tracing::error!(
                    "check_thread_health: plugin thread is finished, checking for panic"
                );
                // Thread finished - take ownership and check result
                if let Some(handle) = self.thread_handle.take() {
                    match handle.join() {
                        Ok(()) => {
                            tracing::warn!("Plugin thread exited normally (unexpected)");
                        }
                        Err(panic_payload) => {
                            // Re-panic with the original panic message to propagate it
                            std::panic::resume_unwind(panic_payload);
                        }
                    }
                }
            }
        }
    }

    /// Deliver a response to a pending async operation in the plugin
    ///
    /// This is called by the editor after processing a command that requires a response.
    pub fn deliver_response(&self, response: fresh_core::api::PluginResponse) {
        // First try to find a pending Rust request (oneshot channel)
        if respond_to_pending(&self.pending_responses, response.clone()) {
            return;
        }

        // If not found, it must be a JS callback
        use fresh_core::api::{JsCallbackId, PluginResponse};

        match response {
            PluginResponse::VirtualBufferCreated {
                request_id,
                buffer_id,
                split_id,
            } => {
                // Return an object with bufferId and splitId (camelCase for JS)
                let result = serde_json::json!({
                    "bufferId": buffer_id.0,
                    "splitId": split_id.map(|s| s.0)
                });
                self.resolve_callback(JsCallbackId(request_id), result.to_string());
            }
            PluginResponse::LspRequest { request_id, result } => match result {
                Ok(value) => {
                    self.resolve_callback(JsCallbackId(request_id), value.to_string());
                }
                Err(e) => {
                    self.reject_callback(JsCallbackId(request_id), e);
                }
            },
            PluginResponse::HighlightsComputed { request_id, spans } => {
                let result = serde_json::to_string(&spans).unwrap_or_else(|_| "[]".to_string());
                self.resolve_callback(JsCallbackId(request_id), result);
            }
            PluginResponse::BufferText { request_id, text } => match text {
                Ok(content) => {
                    // JSON stringify the content string
                    let result =
                        serde_json::to_string(&content).unwrap_or_else(|_| "\"\"".to_string());
                    self.resolve_callback(JsCallbackId(request_id), result);
                }
                Err(e) => {
                    self.reject_callback(JsCallbackId(request_id), e);
                }
            },
            PluginResponse::CompositeBufferCreated {
                request_id,
                buffer_id,
            } => {
                // Return just the buffer_id number, not an object
                self.resolve_callback(JsCallbackId(request_id), buffer_id.0.to_string());
            }
            PluginResponse::LineStartPosition {
                request_id,
                position,
            } => {
                // Return the position as a number or null
                let result =
                    serde_json::to_string(&position).unwrap_or_else(|_| "null".to_string());
                self.resolve_callback(JsCallbackId(request_id), result);
            }
        }
    }

    /// Load a plugin from a file (blocking)
    pub fn load_plugin(&self, path: &Path) -> Result<()> {
        let (tx, rx) = oneshot::channel();
        self.request_sender
            .as_ref()
            .ok_or_else(|| anyhow!("Plugin thread shut down"))?
            .send(PluginRequest::LoadPlugin {
                path: path.to_path_buf(),
                response: tx,
            })
            .map_err(|_| anyhow!("Plugin thread not responding"))?;

        rx.recv().map_err(|_| anyhow!("Plugin thread closed"))?
    }

    /// Load all plugins from a directory (blocking)
    pub fn load_plugins_from_dir(&self, dir: &Path) -> Vec<String> {
        let (tx, rx) = oneshot::channel();
        let Some(sender) = self.request_sender.as_ref() else {
            return vec!["Plugin thread shut down".to_string()];
        };
        if sender
            .send(PluginRequest::LoadPluginsFromDir {
                dir: dir.to_path_buf(),
                response: tx,
            })
            .is_err()
        {
            return vec!["Plugin thread not responding".to_string()];
        }

        rx.recv()
            .unwrap_or_else(|_| vec!["Plugin thread closed".to_string()])
    }

    /// Load all plugins from a directory with config support (blocking)
    /// Returns (errors, discovered_plugins) where discovered_plugins is a map of
    /// plugin name -> PluginConfig with paths populated.
    pub fn load_plugins_from_dir_with_config(
        &self,
        dir: &Path,
        plugin_configs: &HashMap<String, PluginConfig>,
    ) -> (Vec<String>, HashMap<String, PluginConfig>) {
        let (tx, rx) = oneshot::channel();
        let Some(sender) = self.request_sender.as_ref() else {
            return (vec!["Plugin thread shut down".to_string()], HashMap::new());
        };
        if sender
            .send(PluginRequest::LoadPluginsFromDirWithConfig {
                dir: dir.to_path_buf(),
                plugin_configs: plugin_configs.clone(),
                response: tx,
            })
            .is_err()
        {
            return (
                vec!["Plugin thread not responding".to_string()],
                HashMap::new(),
            );
        }

        rx.recv()
            .unwrap_or_else(|_| (vec!["Plugin thread closed".to_string()], HashMap::new()))
    }

    /// Unload a plugin (blocking)
    pub fn unload_plugin(&self, name: &str) -> Result<()> {
        let (tx, rx) = oneshot::channel();
        self.request_sender
            .as_ref()
            .ok_or_else(|| anyhow!("Plugin thread shut down"))?
            .send(PluginRequest::UnloadPlugin {
                name: name.to_string(),
                response: tx,
            })
            .map_err(|_| anyhow!("Plugin thread not responding"))?;

        rx.recv().map_err(|_| anyhow!("Plugin thread closed"))?
    }

    /// Reload a plugin (blocking)
    pub fn reload_plugin(&self, name: &str) -> Result<()> {
        let (tx, rx) = oneshot::channel();
        self.request_sender
            .as_ref()
            .ok_or_else(|| anyhow!("Plugin thread shut down"))?
            .send(PluginRequest::ReloadPlugin {
                name: name.to_string(),
                response: tx,
            })
            .map_err(|_| anyhow!("Plugin thread not responding"))?;

        rx.recv().map_err(|_| anyhow!("Plugin thread closed"))?
    }

    /// Execute a plugin action (non-blocking)
    ///
    /// Returns a receiver that will receive the result when the action completes.
    /// The caller should poll this while processing commands to avoid deadlock.
    pub fn execute_action_async(&self, action_name: &str) -> Result<oneshot::Receiver<Result<()>>> {
        tracing::trace!("execute_action_async: starting action '{}'", action_name);
        let (tx, rx) = oneshot::channel();
        self.request_sender
            .as_ref()
            .ok_or_else(|| anyhow!("Plugin thread shut down"))?
            .send(PluginRequest::ExecuteAction {
                action_name: action_name.to_string(),
                response: tx,
            })
            .map_err(|_| anyhow!("Plugin thread not responding"))?;

        tracing::trace!("execute_action_async: request sent for '{}'", action_name);
        Ok(rx)
    }

    /// Run a hook (non-blocking, fire-and-forget)
    ///
    /// This is the key improvement: hooks are now non-blocking.
    /// The plugin thread will execute them asynchronously and
    /// any results will come back via the PluginCommand channel.
    pub fn run_hook(&self, hook_name: &str, args: HookArgs) {
        if let Some(sender) = self.request_sender.as_ref() {
            let _ = sender.send(PluginRequest::RunHook {
                hook_name: hook_name.to_string(),
                args,
            });
        }
    }

    /// Check if any handlers are registered for a hook (blocking)
    pub fn has_hook_handlers(&self, hook_name: &str) -> bool {
        let (tx, rx) = oneshot::channel();
        let Some(sender) = self.request_sender.as_ref() else {
            return false;
        };
        if sender
            .send(PluginRequest::HasHookHandlers {
                hook_name: hook_name.to_string(),
                response: tx,
            })
            .is_err()
        {
            return false;
        }

        rx.recv().unwrap_or(false)
    }

    /// List all loaded plugins (blocking)
    pub fn list_plugins(&self) -> Vec<TsPluginInfo> {
        let (tx, rx) = oneshot::channel();
        let Some(sender) = self.request_sender.as_ref() else {
            return vec![];
        };
        if sender
            .send(PluginRequest::ListPlugins { response: tx })
            .is_err()
        {
            return vec![];
        }

        rx.recv().unwrap_or_default()
    }

    /// Process pending plugin commands (non-blocking)
    ///
    /// Returns immediately with any pending commands by polling the command queue directly.
    /// This does not require the plugin thread to respond, avoiding deadlocks.
    pub fn process_commands(&mut self) -> Vec<PluginCommand> {
        let mut commands = Vec::new();
        while let Ok(cmd) = self.command_receiver.try_recv() {
            commands.push(cmd);
        }
        commands
    }

    /// Get the state snapshot handle for editor to update
    pub fn state_snapshot_handle(&self) -> Arc<RwLock<EditorStateSnapshot>> {
        Arc::clone(&self.state_snapshot)
    }

    /// Shutdown the plugin thread
    pub fn shutdown(&mut self) {
        tracing::debug!("PluginThreadHandle::shutdown: starting shutdown");

        // Drop all pending response senders - this wakes up any plugin code waiting for responses
        // by causing their oneshot receivers to return an error
        if let Ok(mut pending) = self.pending_responses.lock() {
            if !pending.is_empty() {
                tracing::warn!(
                    "PluginThreadHandle::shutdown: dropping {} pending responses: {:?}",
                    pending.len(),
                    pending.keys().collect::<Vec<_>>()
                );
                pending.clear(); // Drop all senders, waking up waiting receivers
            }
        }

        // First send a Shutdown request to allow clean processing of pending work
        if let Some(sender) = self.request_sender.as_ref() {
            tracing::debug!("PluginThreadHandle::shutdown: sending Shutdown request");
            let _ = sender.send(PluginRequest::Shutdown);
        }

        // Then drop the sender to close the channel - this reliably wakes the receiver
        // even when it's parked in a tokio LocalSet (the Shutdown message above may not wake it)
        tracing::debug!("PluginThreadHandle::shutdown: dropping request_sender to close channel");
        self.request_sender.take();

        if let Some(handle) = self.thread_handle.take() {
            tracing::debug!("PluginThreadHandle::shutdown: joining plugin thread");
            let _ = handle.join();
            tracing::debug!("PluginThreadHandle::shutdown: plugin thread joined");
        }

        tracing::debug!("PluginThreadHandle::shutdown: shutdown complete");
    }

    /// Resolve an async callback in the plugin runtime
    /// Called by the app when async operations (SpawnProcess, Delay) complete
    pub fn resolve_callback(
        &self,
        callback_id: fresh_core::api::JsCallbackId,
        result_json: String,
    ) {
        if let Some(sender) = self.request_sender.as_ref() {
            let _ = sender.send(PluginRequest::ResolveCallback {
                callback_id,
                result_json,
            });
        }
    }

    /// Reject an async callback in the plugin runtime
    /// Called by the app when async operations fail
    pub fn reject_callback(&self, callback_id: fresh_core::api::JsCallbackId, error: String) {
        if let Some(sender) = self.request_sender.as_ref() {
            let _ = sender.send(PluginRequest::RejectCallback { callback_id, error });
        }
    }
}

impl Drop for PluginThreadHandle {
    fn drop(&mut self) {
        self.shutdown();
    }
}

fn respond_to_pending(
    pending_responses: &PendingResponses,
    response: fresh_core::api::PluginResponse,
) -> bool {
    let request_id = match &response {
        fresh_core::api::PluginResponse::VirtualBufferCreated { request_id, .. } => *request_id,
        fresh_core::api::PluginResponse::LspRequest { request_id, .. } => *request_id,
        fresh_core::api::PluginResponse::HighlightsComputed { request_id, .. } => *request_id,
        fresh_core::api::PluginResponse::BufferText { request_id, .. } => *request_id,
        fresh_core::api::PluginResponse::CompositeBufferCreated { request_id, .. } => *request_id,
        fresh_core::api::PluginResponse::LineStartPosition { request_id, .. } => *request_id,
    };

    let sender = {
        let mut pending = pending_responses.lock().unwrap();
        pending.remove(&request_id)
    };

    if let Some(tx) = sender {
        let _ = tx.send(response);
        true
    } else {
        false
    }
}

#[cfg(test)]
mod plugin_thread_tests {
    use super::*;
    use fresh_core::api::PluginResponse;
    use serde_json::json;
    use std::collections::HashMap;
    use std::sync::{Arc, Mutex};
    use tokio::sync::oneshot;

    #[test]
    fn respond_to_pending_sends_lsp_response() {
        let pending: PendingResponses = Arc::new(Mutex::new(HashMap::new()));
        let (tx, mut rx) = oneshot::channel();
        pending.lock().unwrap().insert(123, tx);

        respond_to_pending(
            &pending,
            PluginResponse::LspRequest {
                request_id: 123,
                result: Ok(json!({ "key": "value" })),
            },
        );

        let response = rx.try_recv().expect("expected response");
        match response {
            PluginResponse::LspRequest { result, .. } => {
                assert_eq!(result.unwrap(), json!({ "key": "value" }));
            }
            _ => panic!("unexpected variant"),
        }

        assert!(pending.lock().unwrap().is_empty());
    }

    #[test]
    fn respond_to_pending_handles_virtual_buffer_created() {
        let pending: PendingResponses = Arc::new(Mutex::new(HashMap::new()));
        let (tx, mut rx) = oneshot::channel();
        pending.lock().unwrap().insert(456, tx);

        respond_to_pending(
            &pending,
            PluginResponse::VirtualBufferCreated {
                request_id: 456,
                buffer_id: fresh_core::BufferId(7),
                split_id: Some(fresh_core::SplitId(1)),
            },
        );

        let response = rx.try_recv().expect("expected response");
        match response {
            PluginResponse::VirtualBufferCreated { buffer_id, .. } => {
                assert_eq!(buffer_id.0, 7);
            }
            _ => panic!("unexpected variant"),
        }

        assert!(pending.lock().unwrap().is_empty());
    }
}

/// Main loop for the plugin thread
///
/// Uses `tokio::select!` to interleave request handling with periodic event loop
/// polling. This allows long-running promises (like process spawns) to make progress
/// even when no requests are coming in, preventing the UI from getting stuck.
async fn plugin_thread_loop(
    runtime: Rc<RefCell<QuickJsBackend>>,
    plugins: &mut HashMap<String, TsPluginInfo>,
    mut request_receiver: tokio::sync::mpsc::UnboundedReceiver<PluginRequest>,
) {
    tracing::info!("Plugin thread event loop started");

    // Interval for polling the JS event loop when there's pending work
    let poll_interval = Duration::from_millis(1);
    let mut has_pending_work = false;

    loop {
        // Check for fatal JS errors (e.g., unhandled promise rejections in test mode)
        // These are set via set_fatal_js_error() because panicking inside FFI callbacks
        // is caught by rquickjs and doesn't terminate the thread.
        if crate::backend::has_fatal_js_error() {
            if let Some(error_msg) = crate::backend::take_fatal_js_error() {
                tracing::error!(
                    "Fatal JS error detected, terminating plugin thread: {}",
                    error_msg
                );
                panic!("Fatal plugin error: {}", error_msg);
            }
        }

        tokio::select! {
            biased; // Prefer handling requests over polling

            request = request_receiver.recv() => {
                match request {
                    Some(PluginRequest::ExecuteAction {
                        action_name,
                        response,
                    }) => {
                        // Start the action without blocking - this allows us to process
                        // ResolveCallback requests that the action may be waiting for.
                        let result = runtime.borrow_mut().start_action(&action_name);
                        let _ = response.send(result);
                        has_pending_work = true; // Action may have started async work
                    }
                    Some(request) => {
                        let should_shutdown =
                            handle_request(request, Rc::clone(&runtime), plugins).await;

                        if should_shutdown {
                            break;
                        }
                        has_pending_work = true; // Request may have started async work
                    }
                    None => {
                        // Channel closed
                        tracing::info!("Plugin thread request channel closed");
                        break;
                    }
                }
            }

            // Poll the JS event loop periodically to make progress on pending promises
            _ = tokio::time::sleep(poll_interval), if has_pending_work => {
                has_pending_work = runtime.borrow_mut().poll_event_loop_once();
            }
        }
    }
}

/// Execute an action while processing incoming hook requests concurrently.
///
/// This prevents deadlock when an action awaits a response from the main thread
/// while the main thread is waiting for a blocking hook to complete.
///
/// # Safety (clippy::await_holding_refcell_ref)
/// The RefCell borrow held across await is safe because:
/// - This runs on a single-threaded tokio runtime (no parallel task execution)
/// - No spawn_local calls exist that could create concurrent access to `runtime`
/// - The runtime Rc<RefCell<>> is never shared with other concurrent tasks

/// Run a hook with Rc<RefCell<QuickJsBackend>>
///
/// # Safety (clippy::await_holding_refcell_ref)
/// The RefCell borrow held across await is safe because:
/// - This runs on a single-threaded tokio runtime (no parallel task execution)
/// - No spawn_local calls exist that could create concurrent access to `runtime`
/// - The runtime Rc<RefCell<>> is never shared with other concurrent tasks
#[allow(clippy::await_holding_refcell_ref)]
async fn run_hook_internal_rc(
    runtime: Rc<RefCell<QuickJsBackend>>,
    hook_name: &str,
    args: &HookArgs,
) -> Result<()> {
    // Convert HookArgs to JSON using hook_args_to_json which produces flat JSON
    // (not enum-tagged JSON from serde's default Serialize)
    let json_start = std::time::Instant::now();
    let json_string = fresh_core::hooks::hook_args_to_json(args)?;
    let json_data: serde_json::Value = serde_json::from_str(&json_string)?;
    tracing::trace!(
        hook = hook_name,
        json_ms = json_start.elapsed().as_micros(),
        "hook args serialized"
    );

    // Emit to TypeScript handlers
    let emit_start = std::time::Instant::now();
    runtime.borrow_mut().emit(hook_name, &json_data).await?;
    tracing::trace!(
        hook = hook_name,
        emit_ms = emit_start.elapsed().as_millis(),
        "emit completed"
    );

    Ok(())
}

/// Handle a single request in the plugin thread
async fn handle_request(
    request: PluginRequest,
    runtime: Rc<RefCell<QuickJsBackend>>,
    plugins: &mut HashMap<String, TsPluginInfo>,
) -> bool {
    match request {
        PluginRequest::LoadPlugin { path, response } => {
            let result = load_plugin_internal(Rc::clone(&runtime), plugins, &path).await;
            let _ = response.send(result);
        }

        PluginRequest::LoadPluginsFromDir { dir, response } => {
            let errors = load_plugins_from_dir_internal(Rc::clone(&runtime), plugins, &dir).await;
            let _ = response.send(errors);
        }

        PluginRequest::LoadPluginsFromDirWithConfig {
            dir,
            plugin_configs,
            response,
        } => {
            let (errors, discovered) = load_plugins_from_dir_with_config_internal(
                Rc::clone(&runtime),
                plugins,
                &dir,
                &plugin_configs,
            )
            .await;
            let _ = response.send((errors, discovered));
        }

        PluginRequest::UnloadPlugin { name, response } => {
            let result = unload_plugin_internal(Rc::clone(&runtime), plugins, &name);
            let _ = response.send(result);
        }

        PluginRequest::ReloadPlugin { name, response } => {
            let result = reload_plugin_internal(Rc::clone(&runtime), plugins, &name).await;
            let _ = response.send(result);
        }

        PluginRequest::ExecuteAction {
            action_name,
            response,
        } => {
            // This is handled in plugin_thread_loop with select! for concurrent processing
            // If we get here, it's an unexpected state
            tracing::error!(
                "ExecuteAction should be handled in main loop, not here: {}",
                action_name
            );
            let _ = response.send(Err(anyhow::anyhow!(
                "Internal error: ExecuteAction in wrong handler"
            )));
        }

        PluginRequest::RunHook { hook_name, args } => {
            // Fire-and-forget hook execution
            let hook_start = std::time::Instant::now();
            // Use info level for prompt hooks to aid debugging
            if hook_name == "prompt_confirmed" || hook_name == "prompt_cancelled" {
                tracing::info!(hook = %hook_name, ?args, "RunHook request received (prompt hook)");
            } else {
                tracing::trace!(hook = %hook_name, "RunHook request received");
            }
            if let Err(e) = run_hook_internal_rc(Rc::clone(&runtime), &hook_name, &args).await {
                let error_msg = format!("Plugin error in '{}': {}", hook_name, e);
                tracing::error!("{}", error_msg);
                // Surface the error to the UI
                runtime.borrow_mut().send_status(error_msg);
            }
            if hook_name == "prompt_confirmed" || hook_name == "prompt_cancelled" {
                tracing::info!(
                    hook = %hook_name,
                    elapsed_ms = hook_start.elapsed().as_millis(),
                    "RunHook completed (prompt hook)"
                );
            } else {
                tracing::trace!(
                    hook = %hook_name,
                    elapsed_ms = hook_start.elapsed().as_millis(),
                    "RunHook completed"
                );
            }
        }

        PluginRequest::HasHookHandlers {
            hook_name,
            response,
        } => {
            let has_handlers = runtime.borrow().has_handlers(&hook_name);
            let _ = response.send(has_handlers);
        }

        PluginRequest::ListPlugins { response } => {
            let plugin_list: Vec<TsPluginInfo> = plugins.values().cloned().collect();
            let _ = response.send(plugin_list);
        }

        PluginRequest::ResolveCallback {
            callback_id,
            result_json,
        } => {
            tracing::info!(
                "ResolveCallback: resolving callback_id={} with result_json={}",
                callback_id,
                result_json
            );
            runtime
                .borrow_mut()
                .resolve_callback(callback_id, &result_json);
            // resolve_callback now runs execute_pending_job() internally
            tracing::info!(
                "ResolveCallback: done resolving callback_id={}",
                callback_id
            );
        }

        PluginRequest::RejectCallback { callback_id, error } => {
            runtime.borrow_mut().reject_callback(callback_id, &error);
            // reject_callback now runs execute_pending_job() internally
        }

        PluginRequest::Shutdown => {
            tracing::info!("Plugin thread received shutdown request");
            return true;
        }
    }

    false
}

/// Load a plugin from a file
///
/// # Safety (clippy::await_holding_refcell_ref)
/// The RefCell borrow held across await is safe because:
/// - This runs on a single-threaded tokio runtime (no parallel task execution)
/// - No spawn_local calls exist that could create concurrent access to `runtime`
/// - The runtime Rc<RefCell<>> is never shared with other concurrent tasks
#[allow(clippy::await_holding_refcell_ref)]
async fn load_plugin_internal(
    runtime: Rc<RefCell<QuickJsBackend>>,
    plugins: &mut HashMap<String, TsPluginInfo>,
    path: &Path,
) -> Result<()> {
    let plugin_name = path
        .file_stem()
        .and_then(|s| s.to_str())
        .ok_or_else(|| anyhow!("Invalid plugin filename"))?
        .to_string();

    tracing::info!("Loading TypeScript plugin: {} from {:?}", plugin_name, path);
    tracing::debug!(
        "load_plugin_internal: starting module load for plugin '{}'",
        plugin_name
    );

    // Load and execute the module, passing plugin name for command registration
    let path_str = path
        .to_str()
        .ok_or_else(|| anyhow!("Invalid path encoding"))?;

    // Try to load accompanying .i18n.json file
    let i18n_path = path.with_extension("i18n.json");
    if i18n_path.exists() {
        if let Ok(content) = std::fs::read_to_string(&i18n_path) {
            if let Ok(strings) = serde_json::from_str::<
                std::collections::HashMap<String, std::collections::HashMap<String, String>>,
            >(&content)
            {
                runtime
                    .borrow_mut()
                    .services
                    .register_plugin_strings(&plugin_name, strings);
                tracing::debug!("Loaded i18n strings for plugin '{}'", plugin_name);
            }
        }
    }

    let load_start = std::time::Instant::now();
    runtime
        .borrow_mut()
        .load_module_with_source(path_str, &plugin_name)
        .await?;
    let load_elapsed = load_start.elapsed();

    tracing::debug!(
        "load_plugin_internal: plugin '{}' loaded successfully in {:?}",
        plugin_name,
        load_elapsed
    );

    // Store plugin info
    plugins.insert(
        plugin_name.clone(),
        TsPluginInfo {
            name: plugin_name.clone(),
            path: path.to_path_buf(),
            enabled: true,
        },
    );

    tracing::debug!(
        "load_plugin_internal: plugin '{}' registered, total plugins loaded: {}",
        plugin_name,
        plugins.len()
    );

    Ok(())
}

/// Load all plugins from a directory
async fn load_plugins_from_dir_internal(
    runtime: Rc<RefCell<QuickJsBackend>>,
    plugins: &mut HashMap<String, TsPluginInfo>,
    dir: &Path,
) -> Vec<String> {
    tracing::debug!(
        "load_plugins_from_dir_internal: scanning directory {:?}",
        dir
    );
    let mut errors = Vec::new();

    if !dir.exists() {
        tracing::warn!("Plugin directory does not exist: {:?}", dir);
        return errors;
    }

    // Scan directory for .ts and .js files
    match std::fs::read_dir(dir) {
        Ok(entries) => {
            for entry in entries.flatten() {
                let path = entry.path();
                let ext = path.extension().and_then(|s| s.to_str());
                if ext == Some("ts") || ext == Some("js") {
                    tracing::debug!(
                        "load_plugins_from_dir_internal: attempting to load {:?}",
                        path
                    );
                    if let Err(e) = load_plugin_internal(Rc::clone(&runtime), plugins, &path).await
                    {
                        let err = format!("Failed to load {:?}: {}", path, e);
                        tracing::error!("{}", err);
                        errors.push(err);
                    }
                }
            }

            tracing::debug!(
                "load_plugins_from_dir_internal: finished loading from {:?}, {} errors",
                dir,
                errors.len()
            );
        }
        Err(e) => {
            let err = format!("Failed to read plugin directory: {}", e);
            tracing::error!("{}", err);
            errors.push(err);
        }
    }

    errors
}

/// Load all plugins from a directory with config support
/// Returns (errors, discovered_plugins) where discovered_plugins contains all
/// found plugin files with their configs (respecting enabled state from provided configs)
async fn load_plugins_from_dir_with_config_internal(
    runtime: Rc<RefCell<QuickJsBackend>>,
    plugins: &mut HashMap<String, TsPluginInfo>,
    dir: &Path,
    plugin_configs: &HashMap<String, PluginConfig>,
) -> (Vec<String>, HashMap<String, PluginConfig>) {
    tracing::debug!(
        "load_plugins_from_dir_with_config_internal: scanning directory {:?}",
        dir
    );
    let mut errors = Vec::new();
    let mut discovered_plugins: HashMap<String, PluginConfig> = HashMap::new();

    if !dir.exists() {
        tracing::warn!("Plugin directory does not exist: {:?}", dir);
        return (errors, discovered_plugins);
    }

    // First pass: scan directory and collect all plugin files
    let mut plugin_files: Vec<(String, std::path::PathBuf)> = Vec::new();
    match std::fs::read_dir(dir) {
        Ok(entries) => {
            for entry in entries.flatten() {
                let path = entry.path();
                let ext = path.extension().and_then(|s| s.to_str());
                if ext == Some("ts") || ext == Some("js") {
                    // Skip .i18n.json files (they're not plugins)
                    if path.to_string_lossy().contains(".i18n.") {
                        continue;
                    }
                    // Get plugin name from filename (without extension)
                    let plugin_name = path
                        .file_stem()
                        .and_then(|s| s.to_str())
                        .unwrap_or("unknown")
                        .to_string();
                    plugin_files.push((plugin_name, path));
                }
            }
        }
        Err(e) => {
            let err = format!("Failed to read plugin directory: {}", e);
            tracing::error!("{}", err);
            errors.push(err);
            return (errors, discovered_plugins);
        }
    }

    // Second pass: build discovered_plugins map and load enabled plugins
    for (plugin_name, path) in plugin_files {
        // Check if we have an existing config for this plugin
        let config = if let Some(existing_config) = plugin_configs.get(&plugin_name) {
            // Use existing config but ensure path is set
            PluginConfig {
                enabled: existing_config.enabled,
                path: Some(path.clone()),
            }
        } else {
            // Create new config with default enabled = true
            PluginConfig::new_with_path(path.clone())
        };

        // Add to discovered plugins
        discovered_plugins.insert(plugin_name.clone(), config.clone());

        // Only load if enabled
        if config.enabled {
            tracing::debug!(
                "load_plugins_from_dir_with_config_internal: loading enabled plugin '{}'",
                plugin_name
            );
            if let Err(e) = load_plugin_internal(Rc::clone(&runtime), plugins, &path).await {
                let err = format!("Failed to load {:?}: {}", path, e);
                tracing::error!("{}", err);
                errors.push(err);
            }
        } else {
            tracing::info!(
                "load_plugins_from_dir_with_config_internal: skipping disabled plugin '{}'",
                plugin_name
            );
        }
    }

    tracing::debug!(
        "load_plugins_from_dir_with_config_internal: finished. Discovered {} plugins, {} errors",
        discovered_plugins.len(),
        errors.len()
    );

    (errors, discovered_plugins)
}

/// Unload a plugin
fn unload_plugin_internal(
    runtime: Rc<RefCell<QuickJsBackend>>,
    plugins: &mut HashMap<String, TsPluginInfo>,
    name: &str,
) -> Result<()> {
    if plugins.remove(name).is_some() {
        tracing::info!("Unloading TypeScript plugin: {}", name);

        // Unregister i18n strings
        runtime
            .borrow_mut()
            .services
            .unregister_plugin_strings(name);

        // Remove all commands registered by this plugin
        runtime
            .borrow()
            .services
            .unregister_commands_by_plugin(name);

        Ok(())
    } else {
        Err(anyhow!("Plugin '{}' not found", name))
    }
}

/// Reload a plugin
async fn reload_plugin_internal(
    runtime: Rc<RefCell<QuickJsBackend>>,
    plugins: &mut HashMap<String, TsPluginInfo>,
    name: &str,
) -> Result<()> {
    let path = plugins
        .get(name)
        .ok_or_else(|| anyhow!("Plugin '{}' not found", name))?
        .path
        .clone();

    unload_plugin_internal(Rc::clone(&runtime), plugins, name)?;
    load_plugin_internal(runtime, plugins, &path).await?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use fresh_core::hooks::hook_args_to_json;

    #[test]
    fn test_oneshot_channel() {
        let (tx, rx) = oneshot::channel::<i32>();
        assert!(tx.send(42).is_ok());
        assert_eq!(rx.recv().unwrap(), 42);
    }

    #[test]
    fn test_hook_args_to_json_editor_initialized() {
        let args = HookArgs::EditorInitialized;
        let json = hook_args_to_json(&args).unwrap();
        assert_eq!(json, "{}");
    }

    #[test]
    fn test_hook_args_to_json_prompt_changed() {
        let args = HookArgs::PromptChanged {
            prompt_type: "search".to_string(),
            input: "test".to_string(),
        };
        let json = hook_args_to_json(&args).unwrap();
        let parsed: serde_json::Value = serde_json::from_str(&json).unwrap();
        assert_eq!(parsed["prompt_type"], "search");
        assert_eq!(parsed["input"], "test");
    }
}
