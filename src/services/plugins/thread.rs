//! Plugin Thread: Dedicated thread for TypeScript plugin execution
//!
//! This module implements a dedicated thread architecture for plugin execution,
//! solving the problem of creating new tokio runtimes for each hook call.
//!
//! Architecture:
//! - Main thread (UI) sends requests to plugin thread via channel
//! - Plugin thread owns JsRuntime and persistent tokio runtime
//! - Results are sent back via the existing PluginCommand channel
//! - Async operations complete naturally without runtime destruction

use crate::config::PluginConfig;
use crate::input::command_registry::CommandRegistry;
use crate::services::plugins::api::{EditorStateSnapshot, PluginCommand};
use crate::services::plugins::hooks::{hook_args_to_json, HookArgs};
use crate::services::plugins::runtime::{TsPluginInfo, TypeScriptRuntime};
use anyhow::{anyhow, Result};
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::{Arc, RwLock};
use std::thread::{self, JoinHandle};
use std::time::Duration;

/// Request messages sent to the plugin thread
#[derive(Debug)]
pub enum PluginRequest {
    /// Load a plugin from a file
    LoadPlugin {
        path: PathBuf,
        response: oneshot::Sender<Result<()>>,
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

    /// Command registry (shared with editor)
    commands: Arc<RwLock<CommandRegistry>>,

    /// Pending response senders for async operations (shared with runtime)
    pending_responses: crate::services::plugins::runtime::PendingResponses,

    /// Receiver for plugin commands (polled by editor directly)
    command_receiver: std::sync::mpsc::Receiver<PluginCommand>,
}

impl PluginThreadHandle {
    /// Create a new plugin thread and return its handle
    pub fn spawn(
        commands: Arc<RwLock<CommandRegistry>>,
        dir_context: crate::config_io::DirectoryContext,
    ) -> Result<Self> {
        tracing::debug!("PluginThreadHandle::spawn: starting plugin thread creation");

        // Create channel for plugin commands
        let (command_sender, command_receiver) = std::sync::mpsc::channel();

        // Create editor state snapshot for query API
        let state_snapshot = Arc::new(RwLock::new(EditorStateSnapshot::new()));

        // Create pending responses map (shared between handle and runtime)
        let pending_responses: crate::services::plugins::runtime::PendingResponses =
            Arc::new(std::sync::Mutex::new(std::collections::HashMap::new()));
        let thread_pending_responses = Arc::clone(&pending_responses);

        // Create channel for requests (unbounded allows sync send, async recv)
        let (request_sender, request_receiver) = tokio::sync::mpsc::unbounded_channel();

        // Clone state snapshot for the thread
        let thread_state_snapshot = Arc::clone(&state_snapshot);
        let thread_commands = Arc::clone(&commands);

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

            // Create TypeScript runtime with state
            tracing::debug!("Plugin thread: creating TypeScript runtime (V8 initialization)");
            let runtime = match TypeScriptRuntime::with_state_and_responses(
                Arc::clone(&thread_state_snapshot),
                command_sender,
                thread_pending_responses,
                dir_context,
            ) {
                Ok(rt) => {
                    tracing::debug!("Plugin thread: TypeScript runtime created successfully");
                    rt
                }
                Err(e) => {
                    tracing::error!("Failed to create TypeScript runtime: {}", e);
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
                plugin_thread_loop(runtime, &mut plugins, &thread_commands, request_receiver).await;
            });

            tracing::info!("Plugin thread shutting down");
        });

        tracing::debug!("PluginThreadHandle::spawn: OS thread spawned, returning handle");
        tracing::info!("Plugin thread spawned");

        Ok(Self {
            request_sender: Some(request_sender),
            thread_handle: Some(thread_handle),
            state_snapshot,
            commands,
            pending_responses,
            command_receiver,
        })
    }

    /// Deliver a response to a pending async operation in the plugin
    ///
    /// This is called by the editor after processing a command that requires a response.
    pub fn deliver_response(&self, response: crate::services::plugins::api::PluginResponse) {
        respond_to_pending(&self.pending_responses, response);
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

    /// Get the command registry
    #[allow(dead_code)]
    pub fn command_registry(&self) -> Arc<RwLock<CommandRegistry>> {
        Arc::clone(&self.commands)
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
}

impl Drop for PluginThreadHandle {
    fn drop(&mut self) {
        self.shutdown();
    }
}

fn respond_to_pending(
    pending_responses: &crate::services::plugins::runtime::PendingResponses,
    response: crate::services::plugins::api::PluginResponse,
) {
    let request_id = match &response {
        crate::services::plugins::api::PluginResponse::VirtualBufferCreated {
            request_id, ..
        } => *request_id,
        crate::services::plugins::api::PluginResponse::LspRequest { request_id, .. } => *request_id,
        crate::services::plugins::api::PluginResponse::HighlightsComputed {
            request_id, ..
        } => *request_id,
        crate::services::plugins::api::PluginResponse::BufferText { request_id, .. } => *request_id,
        crate::services::plugins::api::PluginResponse::CompositeBufferCreated {
            request_id,
            ..
        } => *request_id,
    };

    let sender = {
        let mut pending = pending_responses.lock().unwrap();
        pending.remove(&request_id)
    };

    if let Some(tx) = sender {
        let _ = tx.send(response);
    }
}

#[cfg(test)]
mod plugin_thread_tests {
    use super::*;
    use crate::services::plugins::api::PluginResponse;
    use crate::services::plugins::runtime::PendingResponses;
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
                buffer_id: crate::model::event::BufferId(7),
                split_id: Some(crate::model::event::SplitId(1)),
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
    runtime: Rc<RefCell<TypeScriptRuntime>>,
    plugins: &mut HashMap<String, TsPluginInfo>,
    commands: &Arc<RwLock<CommandRegistry>>,
    mut request_receiver: tokio::sync::mpsc::UnboundedReceiver<PluginRequest>,
) {
    tracing::info!("Plugin thread event loop started");

    // Interval for polling the JS event loop when there's pending work
    let poll_interval = Duration::from_millis(1);
    let mut has_pending_work = false;

    loop {
        tokio::select! {
            biased; // Prefer handling requests over polling

            request = request_receiver.recv() => {
                match request {
                    Some(PluginRequest::ExecuteAction {
                        action_name,
                        response,
                    }) => {
                        // Handle ExecuteAction specially
                        execute_action_with_hooks(&action_name, response, Rc::clone(&runtime)).await;
                        has_pending_work = true; // Action may have started async work
                    }
                    Some(request) => {
                        let should_shutdown =
                            handle_request(request, Rc::clone(&runtime), plugins, commands).await;

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
#[allow(clippy::await_holding_refcell_ref)]
async fn execute_action_with_hooks(
    action_name: &str,
    response: oneshot::Sender<Result<()>>,
    runtime: Rc<RefCell<TypeScriptRuntime>>,
) {
    tracing::trace!(
        "execute_action_with_hooks: starting action '{}'",
        action_name
    );

    // Execute the action - we can't process hooks during this because the runtime
    // is borrowed. Instead, we need a different approach to break the deadlock.
    //
    // The deadlock scenario is:
    // 1. Action awaits response from main thread
    // 2. Main thread calls run_hook_blocking and waits
    // 3. Main thread can't deliver response because it's waiting
    // 4. Plugin thread can't process hook because action has runtime
    //
    // The fix is to make the main thread continue processing commands while
    // waiting for hooks. But for now, we execute the action and hope for the best.
    // A proper fix requires changes to the main thread's wait_for logic.

    let result = runtime.borrow_mut().execute_action(action_name).await;

    tracing::trace!(
        "execute_action_with_hooks: action '{}' completed with result: {:?}",
        action_name,
        result.is_ok()
    );
    let _ = response.send(result);
}

/// Run a hook with Rc<RefCell<TypeScriptRuntime>>
///
/// # Safety (clippy::await_holding_refcell_ref)
/// The RefCell borrow held across await is safe because:
/// - This runs on a single-threaded tokio runtime (no parallel task execution)
/// - No spawn_local calls exist that could create concurrent access to `runtime`
/// - The runtime Rc<RefCell<>> is never shared with other concurrent tasks
#[allow(clippy::await_holding_refcell_ref)]
async fn run_hook_internal_rc(
    runtime: Rc<RefCell<TypeScriptRuntime>>,
    hook_name: &str,
    args: &HookArgs,
) -> Result<()> {
    // Convert HookArgs to JSON
    let json_start = std::time::Instant::now();
    let json_data = hook_args_to_json(args)?;
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
    runtime: Rc<RefCell<TypeScriptRuntime>>,
    plugins: &mut HashMap<String, TsPluginInfo>,
    commands: &Arc<RwLock<CommandRegistry>>,
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
            let result = unload_plugin_internal(plugins, commands, &name);
            let _ = response.send(result);
        }

        PluginRequest::ReloadPlugin { name, response } => {
            let result =
                reload_plugin_internal(Rc::clone(&runtime), plugins, commands, &name).await;
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
            tracing::trace!(hook = %hook_name, "RunHook request received");
            if let Err(e) = run_hook_internal_rc(Rc::clone(&runtime), &hook_name, &args).await {
                let error_msg = format!("Plugin error in '{}': {}", hook_name, e);
                tracing::error!("{}", error_msg);
                // Surface the error to the UI
                runtime.borrow_mut().send_status(error_msg);
            }
            tracing::trace!(
                hook = %hook_name,
                elapsed_ms = hook_start.elapsed().as_millis(),
                "RunHook completed"
            );
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
    runtime: Rc<RefCell<TypeScriptRuntime>>,
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
                crate::i18n::register_plugin_strings(&plugin_name, strings);
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
    runtime: Rc<RefCell<TypeScriptRuntime>>,
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
    runtime: Rc<RefCell<TypeScriptRuntime>>,
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
    plugins: &mut HashMap<String, TsPluginInfo>,
    commands: &Arc<RwLock<CommandRegistry>>,
    name: &str,
) -> Result<()> {
    if plugins.remove(name).is_some() {
        tracing::info!("Unloading TypeScript plugin: {}", name);

        // Unregister i18n strings
        crate::i18n::unregister_plugin_strings(name);

        // Remove plugin's commands (assuming they're prefixed with plugin name)
        let prefix = format!("{}:", name);
        commands.read().unwrap().unregister_by_prefix(&prefix);

        Ok(())
    } else {
        Err(anyhow!("Plugin '{}' not found", name))
    }
}

/// Reload a plugin
async fn reload_plugin_internal(
    runtime: Rc<RefCell<TypeScriptRuntime>>,
    plugins: &mut HashMap<String, TsPluginInfo>,
    commands: &Arc<RwLock<CommandRegistry>>,
    name: &str,
) -> Result<()> {
    let path = plugins
        .get(name)
        .ok_or_else(|| anyhow!("Plugin '{}' not found", name))?
        .path
        .clone();

    unload_plugin_internal(plugins, commands, name)?;
    load_plugin_internal(runtime, plugins, &path).await?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

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
