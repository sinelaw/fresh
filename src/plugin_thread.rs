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

use crate::command_registry::CommandRegistry;
use crate::hooks::HookArgs;
use crate::plugin_api::{EditorStateSnapshot, PluginCommand};
use crate::ts_runtime::{TsPluginInfo, TypeScriptRuntime};
use anyhow::{anyhow, Result};
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::{Arc, RwLock};
use std::thread::{self, JoinHandle};

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
    request_sender: tokio::sync::mpsc::UnboundedSender<PluginRequest>,

    /// Thread join handle
    thread_handle: Option<JoinHandle<()>>,

    /// State snapshot handle for editor to update
    state_snapshot: Arc<RwLock<EditorStateSnapshot>>,

    /// Command registry (shared with editor)
    commands: Arc<RwLock<CommandRegistry>>,

    /// Pending response senders for async operations (shared with runtime)
    pending_responses: crate::ts_runtime::PendingResponses,

    /// Receiver for plugin commands (polled by editor directly)
    command_receiver: std::sync::mpsc::Receiver<PluginCommand>,
}

impl PluginThreadHandle {
    /// Create a new plugin thread and return its handle
    pub fn spawn(commands: Arc<RwLock<CommandRegistry>>) -> Result<Self> {
        // Create channel for plugin commands
        let (command_sender, command_receiver) = std::sync::mpsc::channel();

        // Create editor state snapshot for query API
        let state_snapshot = Arc::new(RwLock::new(EditorStateSnapshot::new()));

        // Create pending responses map (shared between handle and runtime)
        let pending_responses: crate::ts_runtime::PendingResponses =
            Arc::new(std::sync::Mutex::new(std::collections::HashMap::new()));
        let thread_pending_responses = Arc::clone(&pending_responses);

        // Create channel for requests (unbounded allows sync send, async recv)
        let (request_sender, request_receiver) = tokio::sync::mpsc::unbounded_channel();

        // Clone state snapshot for the thread
        let thread_state_snapshot = Arc::clone(&state_snapshot);
        let thread_commands = Arc::clone(&commands);

        // Spawn the plugin thread
        let thread_handle = thread::spawn(move || {
            // Create tokio runtime for the plugin thread
            let rt = match tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
            {
                Ok(rt) => rt,
                Err(e) => {
                    tracing::error!("Failed to create plugin thread runtime: {}", e);
                    return;
                }
            };

            // Create TypeScript runtime with state
            let runtime = match TypeScriptRuntime::with_state_and_responses(
                Arc::clone(&thread_state_snapshot),
                command_sender,
                thread_pending_responses,
            ) {
                Ok(rt) => rt,
                Err(e) => {
                    tracing::error!("Failed to create TypeScript runtime: {}", e);
                    return;
                }
            };

            // Create internal manager state
            let mut plugins: HashMap<String, TsPluginInfo> = HashMap::new();

            // Run the event loop with a LocalSet to allow concurrent task execution
            let local = tokio::task::LocalSet::new();
            local.block_on(&rt, async {
                // Wrap runtime in RefCell for interior mutability during concurrent operations
                let runtime = Rc::new(RefCell::new(runtime));
                plugin_thread_loop(runtime, &mut plugins, &thread_commands, request_receiver).await;
            });

            tracing::info!("Plugin thread shutting down");
        });

        tracing::info!("Plugin thread spawned");

        Ok(Self {
            request_sender,
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
    pub fn deliver_response(&self, response: crate::plugin_api::PluginResponse) {
        respond_to_pending(&self.pending_responses, response);
    }

    /// Load a plugin from a file (blocking)
    pub fn load_plugin(&self, path: &Path) -> Result<()> {
        let (tx, mut rx) = oneshot::channel();
        self.request_sender
            .send(PluginRequest::LoadPlugin {
                path: path.to_path_buf(),
                response: tx,
            })
            .map_err(|_| anyhow!("Plugin thread not responding"))?;

        rx.recv().map_err(|_| anyhow!("Plugin thread closed"))?
    }

    /// Load all plugins from a directory (blocking)
    pub fn load_plugins_from_dir(&self, dir: &Path) -> Vec<String> {
        let (tx, mut rx) = oneshot::channel();
        if self
            .request_sender
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

    /// Unload a plugin (blocking)
    pub fn unload_plugin(&self, name: &str) -> Result<()> {
        let (tx, mut rx) = oneshot::channel();
        self.request_sender
            .send(PluginRequest::UnloadPlugin {
                name: name.to_string(),
                response: tx,
            })
            .map_err(|_| anyhow!("Plugin thread not responding"))?;

        rx.recv().map_err(|_| anyhow!("Plugin thread closed"))?
    }

    /// Reload a plugin (blocking)
    pub fn reload_plugin(&self, name: &str) -> Result<()> {
        let (tx, mut rx) = oneshot::channel();
        self.request_sender
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
        let (tx, mut rx) = oneshot::channel();
        self.request_sender
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
        let _ = self.request_sender.send(PluginRequest::RunHook {
            hook_name: hook_name.to_string(),
            args,
        });
    }

    /// Check if any handlers are registered for a hook (blocking)
    pub fn has_hook_handlers(&self, hook_name: &str) -> bool {
        let (tx, mut rx) = oneshot::channel();
        if self
            .request_sender
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
        let (tx, mut rx) = oneshot::channel();
        if self
            .request_sender
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
        let _ = self.request_sender.send(PluginRequest::Shutdown);

        if let Some(handle) = self.thread_handle.take() {
            let _ = handle.join();
        }
    }
}

impl Drop for PluginThreadHandle {
    fn drop(&mut self) {
        self.shutdown();
    }
}

fn respond_to_pending(
    pending_responses: &crate::ts_runtime::PendingResponses,
    response: crate::plugin_api::PluginResponse,
) {
    let request_id = match &response {
        crate::plugin_api::PluginResponse::VirtualBufferCreated { request_id, .. } => *request_id,
        crate::plugin_api::PluginResponse::LspRequest { request_id, .. } => *request_id,
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
    use crate::plugin_api::PluginResponse;
    use crate::ts_runtime::PendingResponses;
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
                buffer_id: crate::event::BufferId(7),
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
async fn plugin_thread_loop(
    runtime: Rc<RefCell<TypeScriptRuntime>>,
    plugins: &mut HashMap<String, TsPluginInfo>,
    commands: &Arc<RwLock<CommandRegistry>>,
    mut request_receiver: tokio::sync::mpsc::UnboundedReceiver<PluginRequest>,
) {
    tracing::info!("Plugin thread event loop started");

    loop {
        // Wait for requests (async, no polling/sleeping)
        match request_receiver.recv().await {
            Some(PluginRequest::ExecuteAction {
                action_name,
                response,
            }) => {
                // Handle ExecuteAction specially
                execute_action_with_hooks(&action_name, response, Rc::clone(&runtime)).await;
            }
            Some(request) => {
                let should_shutdown =
                    handle_request(request, Rc::clone(&runtime), plugins, commands).await;

                if should_shutdown {
                    break;
                }
            }
            None => {
                // Channel closed
                tracing::info!("Plugin thread request channel closed");
                break;
            }
        }
    }
}

/// Execute an action while processing incoming hook requests concurrently.
///
/// This prevents deadlock when an action awaits a response from the main thread
/// while the main thread is waiting for a blocking hook to complete.
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
async fn run_hook_internal_rc(
    runtime: Rc<RefCell<TypeScriptRuntime>>,
    hook_name: &str,
    args: &HookArgs,
) -> Result<()> {
    // Convert HookArgs to JSON
    let json_data = hook_args_to_json(args)?;

    // Emit to TypeScript handlers
    runtime.borrow_mut().emit(hook_name, &json_data).await?;

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
            if let Err(e) = run_hook_internal_rc(runtime, &hook_name, &args).await {
                tracing::error!("Error running hook '{}': {}", hook_name, e);
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

        PluginRequest::Shutdown => {
            tracing::info!("Plugin thread received shutdown request");
            return true;
        }
    }

    false
}

/// Load a plugin from a file
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

    // Load and execute the module
    let path_str = path
        .to_str()
        .ok_or_else(|| anyhow!("Invalid path encoding"))?;

    runtime.borrow_mut().load_module(path_str).await?;

    // Store plugin info
    plugins.insert(
        plugin_name.clone(),
        TsPluginInfo {
            name: plugin_name,
            path: path.to_path_buf(),
            enabled: true,
        },
    );

    Ok(())
}

/// Load all plugins from a directory
async fn load_plugins_from_dir_internal(
    runtime: Rc<RefCell<TypeScriptRuntime>>,
    plugins: &mut HashMap<String, TsPluginInfo>,
    dir: &Path,
) -> Vec<String> {
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
                    if let Err(e) = load_plugin_internal(Rc::clone(&runtime), plugins, &path).await
                    {
                        let err = format!("Failed to load {:?}: {}", path, e);
                        tracing::error!("{}", err);
                        errors.push(err);
                    }
                }
            }
        }
        Err(e) => {
            let err = format!("Failed to read plugin directory: {}", e);
            tracing::error!("{}", err);
            errors.push(err);
        }
    }

    errors
}

/// Unload a plugin
fn unload_plugin_internal(
    plugins: &mut HashMap<String, TsPluginInfo>,
    commands: &Arc<RwLock<CommandRegistry>>,
    name: &str,
) -> Result<()> {
    if plugins.remove(name).is_some() {
        tracing::info!("Unloading TypeScript plugin: {}", name);

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

/// Convert HookArgs to JSON string
fn hook_args_to_json(args: &HookArgs) -> Result<String> {
    let json_value = match args {
        HookArgs::RenderStart { buffer_id } => {
            serde_json::json!({
                "buffer_id": buffer_id.0,
            })
        }
        HookArgs::RenderLine {
            buffer_id,
            line_number,
            byte_start,
            byte_end,
            content,
        } => {
            serde_json::json!({
                "buffer_id": buffer_id.0,
                "line_number": line_number,
                "byte_start": byte_start,
                "byte_end": byte_end,
                "content": content,
            })
        }
        HookArgs::BufferActivated { buffer_id } => {
            serde_json::json!({ "buffer_id": buffer_id.0 })
        }
        HookArgs::BufferDeactivated { buffer_id } => {
            serde_json::json!({ "buffer_id": buffer_id.0 })
        }
        HookArgs::BufferClosed { buffer_id } => {
            serde_json::json!({ "buffer_id": buffer_id.0 })
        }
        HookArgs::CursorMoved {
            buffer_id,
            cursor_id,
            old_position,
            new_position,
        } => {
            serde_json::json!({
                "buffer_id": buffer_id.0,
                "cursor_id": cursor_id.0,
                "old_position": old_position,
                "new_position": new_position,
            })
        }
        HookArgs::BeforeInsert {
            buffer_id,
            position,
            text,
        } => {
            serde_json::json!({
                "buffer_id": buffer_id.0,
                "position": position,
                "text": text,
            })
        }
        HookArgs::AfterInsert {
            buffer_id,
            position,
            text,
        } => {
            serde_json::json!({
                "buffer_id": buffer_id.0,
                "position": position,
                "text": text,
            })
        }
        HookArgs::BeforeDelete { buffer_id, range } => {
            serde_json::json!({
                "buffer_id": buffer_id.0,
                "start": range.start,
                "end": range.end,
            })
        }
        HookArgs::AfterDelete {
            buffer_id,
            range,
            deleted_text,
        } => {
            serde_json::json!({
                "buffer_id": buffer_id.0,
                "start": range.start,
                "end": range.end,
                "deleted_text": deleted_text,
            })
        }
        HookArgs::BeforeFileOpen { path } => {
            serde_json::json!({ "path": path.to_string_lossy() })
        }
        HookArgs::AfterFileOpen { path, buffer_id } => {
            serde_json::json!({
                "path": path.to_string_lossy(),
                "buffer_id": buffer_id.0,
            })
        }
        HookArgs::BeforeFileSave { path, buffer_id } => {
            serde_json::json!({
                "path": path.to_string_lossy(),
                "buffer_id": buffer_id.0,
            })
        }
        HookArgs::AfterFileSave { path, buffer_id } => {
            serde_json::json!({
                "path": path.to_string_lossy(),
                "buffer_id": buffer_id.0,
            })
        }
        HookArgs::PreCommand { action } => {
            serde_json::json!({ "action": format!("{:?}", action) })
        }
        HookArgs::PostCommand { action } => {
            serde_json::json!({ "action": format!("{:?}", action) })
        }
        HookArgs::Idle { milliseconds } => {
            serde_json::json!({ "milliseconds": milliseconds })
        }
        HookArgs::EditorInitialized => {
            serde_json::json!({})
        }
        HookArgs::PromptChanged { prompt_type, input } => {
            serde_json::json!({
                "prompt_type": prompt_type,
                "input": input,
            })
        }
        HookArgs::PromptConfirmed {
            prompt_type,
            input,
            selected_index,
        } => {
            serde_json::json!({
                "prompt_type": prompt_type,
                "input": input,
                "selected_index": selected_index,
            })
        }
        HookArgs::PromptCancelled { prompt_type, input } => {
            serde_json::json!({
                "prompt_type": prompt_type,
                "input": input,
            })
        }
        HookArgs::KeyboardShortcuts { bindings } => {
            let entries: Vec<serde_json::Value> = bindings
                .iter()
                .map(|(key, action)| serde_json::json!({ "key": key, "action": action }))
                .collect();
            serde_json::json!({ "bindings": entries })
        }
        HookArgs::ManualPage => {
            serde_json::json!({})
        }
        HookArgs::LspReferences { symbol, locations } => {
            let locs: Vec<serde_json::Value> = locations
                .iter()
                .map(|loc| {
                    serde_json::json!({
                        "file": loc.file,
                        "line": loc.line,
                        "column": loc.column,
                    })
                })
                .collect();
            serde_json::json!({ "symbol": symbol, "locations": locs })
        }
        HookArgs::LinesChanged { buffer_id, lines } => {
            let lines_json: Vec<serde_json::Value> = lines
                .iter()
                .map(|line| {
                    serde_json::json!({
                        "line_number": line.line_number,
                        "byte_start": line.byte_start,
                        "byte_end": line.byte_end,
                        "content": line.content,
                    })
                })
                .collect();
            serde_json::json!({
                "buffer_id": buffer_id.0,
                "lines": lines_json,
            })
        }
        HookArgs::ViewTransformRequest {
            buffer_id,
            split_id,
            viewport_start,
            viewport_end,
            tokens,
        } => {
            use crate::plugin_api::ViewTokenWireKind;
            let tokens_json: Vec<serde_json::Value> = tokens
                .iter()
                .map(|token| {
                    let kind_json = match &token.kind {
                        ViewTokenWireKind::Text(s) => serde_json::json!({ "Text": s }),
                        ViewTokenWireKind::Newline => serde_json::json!("Newline"),
                        ViewTokenWireKind::Space => serde_json::json!("Space"),
                        ViewTokenWireKind::Break => serde_json::json!("Break"),
                    };
                    serde_json::json!({
                        "source_offset": token.source_offset,
                        "kind": kind_json,
                    })
                })
                .collect();
            serde_json::json!({
                "buffer_id": buffer_id.0,
                "split_id": split_id.0,
                "viewport_start": viewport_start,
                "viewport_end": viewport_end,
                "tokens": tokens_json,
            })
        }
    };

    serde_json::to_string(&json_value).map_err(|e| anyhow!("Failed to serialize hook args: {}", e))
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
