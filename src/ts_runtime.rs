//! TypeScript Plugin Runtime
//!
//! This module provides a TypeScript/JavaScript runtime for plugins using deno_core.
//! It enables native async/await support, solving the async command execution problem
//! that existed with the Lua plugin system.

use anyhow::{anyhow, Result};
use deno_core::{extension, op2, FastString, JsRuntime, RuntimeOptions};
use std::rc::Rc;

/// Custom ops for the Fresh editor API
#[op2(fast)]
fn op_fresh_set_status(#[string] message: String) {
    // For now, just log the message
    tracing::info!("TypeScript plugin set_status: {}", message);
}

#[op2(fast)]
fn op_fresh_debug(#[string] message: String) {
    tracing::debug!("TypeScript plugin: {}", message);
}

#[op2(fast)]
fn op_fresh_get_active_buffer_id() -> u32 {
    // Placeholder - will be connected to actual editor state
    0
}

// Define the extension with our ops
extension!(
    fresh_runtime,
    ops = [op_fresh_set_status, op_fresh_debug, op_fresh_get_active_buffer_id],
);

/// TypeScript plugin runtime
pub struct TypeScriptRuntime {
    js_runtime: JsRuntime,
}

impl TypeScriptRuntime {
    /// Create a new TypeScript runtime
    pub fn new() -> Result<Self> {
        let mut js_runtime = JsRuntime::new(RuntimeOptions {
            module_loader: Some(Rc::new(deno_core::FsModuleLoader)),
            extensions: vec![fresh_runtime::init_ops()],
            ..Default::default()
        });

        // Set up the global editor API
        js_runtime
            .execute_script(
                "<fresh_bootstrap>",
                r#"
                const core = Deno.core;

                // Create the editor API object
                const editor = {
                    setStatus(message) {
                        core.ops.op_fresh_set_status(message);
                    },
                    debug(message) {
                        core.ops.op_fresh_debug(message);
                    },
                    getActiveBufferId() {
                        return core.ops.op_fresh_get_active_buffer_id();
                    },
                };

                // Make editor globally available
                globalThis.editor = editor;
                "#
                .to_string(),
            )
            .map_err(|e| anyhow!("Failed to initialize editor API: {}", e))?;

        Ok(Self { js_runtime })
    }

    /// Execute JavaScript code directly
    pub async fn execute_script(&mut self, name: &'static str, code: &str) -> Result<()> {
        // Code needs to be FastString for the IntoModuleCodeString trait
        let code_static: FastString = code.to_string().into();
        self.js_runtime
            .execute_script(name, code_static)
            .map_err(|e| anyhow!("Failed to execute script '{}': {}", name, e))?;

        // Run the event loop to process any pending async operations
        self.js_runtime
            .run_event_loop(Default::default())
            .await
            .map_err(|e| anyhow!("Event loop error: {}", e))?;

        Ok(())
    }

    /// Load and execute a TypeScript/JavaScript module file
    pub async fn load_module(&mut self, path: &str) -> Result<()> {
        let main_module = deno_core::resolve_path(
            path,
            &std::env::current_dir().map_err(|e| anyhow!("Failed to get cwd: {}", e))?,
        )
        .map_err(|e| anyhow!("Failed to resolve module path '{}': {}", path, e))?;

        let mod_id = self
            .js_runtime
            .load_main_es_module(&main_module)
            .await
            .map_err(|e| anyhow!("Failed to load module '{}': {}", path, e))?;

        let result = self.js_runtime.mod_evaluate(mod_id);

        self.js_runtime
            .run_event_loop(Default::default())
            .await
            .map_err(|e| anyhow!("Event loop error while loading module: {}", e))?;

        result
            .await
            .map_err(|e| anyhow!("Module evaluation error: {}", e))?;

        Ok(())
    }

    /// Execute a global function by name (for plugin actions)
    pub async fn execute_action(&mut self, action_name: &str) -> Result<()> {
        let code = format!(
            r#"
            (async () => {{
                if (typeof globalThis.{} === 'function') {{
                    const result = globalThis.{}();
                    if (result instanceof Promise) {{
                        await result;
                    }}
                }} else {{
                    throw new Error('Action "{}" is not defined as a global function');
                }}
            }})();
            "#,
            action_name, action_name, action_name
        );

        self.execute_script("<action>", &code).await
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_typescript_runtime_creation() {
        let runtime = TypeScriptRuntime::new();
        assert!(runtime.is_ok(), "Failed to create TypeScript runtime");
    }

    #[tokio::test]
    async fn test_execute_simple_script() {
        let mut runtime = TypeScriptRuntime::new().unwrap();
        let result = runtime
            .execute_script("<test>", "const x = 1 + 1; console.log('Result:', x);")
            .await;
        assert!(result.is_ok(), "Failed to execute simple script: {:?}", result);
    }

    #[tokio::test]
    async fn test_call_fresh_ops() {
        let mut runtime = TypeScriptRuntime::new().unwrap();
        let result = runtime
            .execute_script(
                "<test_ops>",
                r#"
                Deno.core.ops.op_fresh_set_status("Hello from TypeScript!");
                Deno.core.ops.op_fresh_debug("Debug message");
                const bufferId = Deno.core.ops.op_fresh_get_active_buffer_id();
                console.log("Buffer ID:", bufferId);
                "#,
            )
            .await;
        assert!(result.is_ok(), "Failed to call Fresh ops: {:?}", result);
    }

    #[tokio::test]
    async fn test_async_await() {
        let mut runtime = TypeScriptRuntime::new().unwrap();
        let result = runtime
            .execute_script(
                "<test_async>",
                r#"
                async function testAsync() {
                    const result = await Promise.resolve(42);
                    console.log("Async result:", result);
                    return result;
                }
                testAsync();
                "#,
            )
            .await;
        assert!(result.is_ok(), "Failed to execute async code: {:?}", result);
    }

    #[tokio::test]
    async fn test_execute_action() {
        let mut runtime = TypeScriptRuntime::new().unwrap();

        // Define a global function
        runtime
            .execute_script(
                "<define_action>",
                r#"
                globalThis.my_test_action = function() {
                    Deno.core.ops.op_fresh_set_status("Action executed!");
                };
                "#,
            )
            .await
            .unwrap();

        // Execute the action
        let result = runtime.execute_action("my_test_action").await;
        assert!(result.is_ok(), "Failed to execute action: {:?}", result);
    }

    #[tokio::test]
    async fn test_execute_async_action() {
        let mut runtime = TypeScriptRuntime::new().unwrap();

        // Define an async global function (using Promise.resolve instead of setTimeout)
        runtime
            .execute_script(
                "<define_async_action>",
                r#"
                globalThis.my_async_action = async function() {
                    const result = await Promise.resolve("async data");
                    Deno.core.ops.op_fresh_set_status("Async action completed with: " + result);
                };
                "#,
            )
            .await
            .unwrap();

        // Execute the async action
        let result = runtime.execute_action("my_async_action").await;
        assert!(result.is_ok(), "Failed to execute async action: {:?}", result);
    }
}
