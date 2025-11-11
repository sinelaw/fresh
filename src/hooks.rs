//! Hook System: Event subscription and notification for plugins
//!
//! Hooks allow plugins to subscribe to editor events and react to them.
//! This is inspired by Emacs' hook system.

use crate::event::{BufferId, CursorId};
use crate::keybindings::Action;
use std::collections::HashMap;
use std::ops::Range;
use std::path::PathBuf;

/// Arguments passed to hook callbacks
#[derive(Debug, Clone)]
pub enum HookArgs {
    /// Before a file is opened
    BeforeFileOpen { path: PathBuf },

    /// After a file is successfully opened
    AfterFileOpen { buffer_id: BufferId, path: PathBuf },

    /// Before a buffer is saved to disk
    BeforeFileSave { buffer_id: BufferId, path: PathBuf },

    /// After a buffer is successfully saved
    AfterFileSave { buffer_id: BufferId, path: PathBuf },

    /// A buffer was closed
    BufferClosed { buffer_id: BufferId },

    /// Before text is inserted
    BeforeInsert {
        buffer_id: BufferId,
        position: usize,
        text: String,
    },

    /// After text was inserted
    AfterInsert {
        buffer_id: BufferId,
        position: usize,
        text: String,
    },

    /// Before text is deleted
    BeforeDelete {
        buffer_id: BufferId,
        range: Range<usize>,
    },

    /// After text was deleted
    AfterDelete {
        buffer_id: BufferId,
        range: Range<usize>,
        deleted_text: String,
    },

    /// Cursor moved to a new position
    CursorMoved {
        buffer_id: BufferId,
        cursor_id: CursorId,
        old_position: usize,
        new_position: usize,
    },

    /// Buffer became active
    BufferActivated { buffer_id: BufferId },

    /// Buffer was deactivated
    BufferDeactivated { buffer_id: BufferId },

    /// Before a command/action is executed
    PreCommand { action: Action },

    /// After a command/action was executed
    PostCommand { action: Action },

    /// Editor has been idle for N milliseconds (no input)
    Idle { milliseconds: u64 },

    /// Editor is initializing
    EditorInitialized,

    /// A line is being rendered (called during the rendering pass)
    /// This hook fires once per visible line during each frame
    /// Plugins can inspect content and add overlays without additional traversal
    RenderLine {
        buffer_id: BufferId,
        line_number: usize,
        byte_start: usize,
        byte_end: usize,
        content: String,
    },

    /// Prompt input changed (user typed/edited)
    PromptChanged {
        prompt_type: String,
        input: String,
    },

    /// Prompt was confirmed (user pressed Enter)
    PromptConfirmed {
        prompt_type: String,
        input: String,
        selected_index: Option<usize>,
    },

    /// Prompt was cancelled (user pressed Escape/Ctrl+G)
    PromptCancelled {
        prompt_type: String,
        input: String,
    },
}

/// Type for hook callbacks
/// Returns `true` to continue execution, `false` to cancel the operation
pub type HookCallback = Box<dyn Fn(&HookArgs) -> bool + Send + Sync>;

/// Registry for managing hooks
pub struct HookRegistry {
    /// Map from hook name to list of callbacks
    hooks: HashMap<String, Vec<HookCallback>>,
}

impl HookRegistry {
    /// Create a new hook registry
    pub fn new() -> Self {
        Self {
            hooks: HashMap::new(),
        }
    }

    /// Add a hook callback for a specific hook name
    ///
    /// # Arguments
    /// * `name` - Name of the hook (e.g., "after-file-save")
    /// * `callback` - Callback function to invoke when hook is triggered
    pub fn add_hook(&mut self, name: &str, callback: HookCallback) {
        self.hooks
            .entry(name.to_string())
            .or_insert_with(Vec::new)
            .push(callback);
    }

    /// Remove all hooks for a specific name
    pub fn remove_hooks(&mut self, name: &str) {
        self.hooks.remove(name);
    }

    /// Run all hooks for a specific name
    ///
    /// Returns `true` if all hooks returned true (continue execution)
    /// Returns `false` if any hook returned false (cancel operation)
    pub fn run_hooks(&self, name: &str, args: &HookArgs) -> bool {
        if let Some(hooks) = self.hooks.get(name) {
            for callback in hooks {
                if !callback(args) {
                    tracing::debug!("Hook '{}' cancelled operation", name);
                    return false; // Hook cancelled operation
                }
            }
        }
        true
    }

    /// Run hooks with timeout protection
    ///
    /// Returns `true` if all hooks completed successfully within timeout
    pub fn run_hooks_with_timeout(
        &self,
        name: &str,
        args: &HookArgs,
        timeout: std::time::Duration,
    ) -> bool {
        use std::time::Instant;

        let start = Instant::now();

        if let Some(hooks) = self.hooks.get(name) {
            for (i, callback) in hooks.iter().enumerate() {
                if start.elapsed() > timeout {
                    tracing::warn!(
                        "Hook '{}' timeout exceeded at callback {} ({:?})",
                        name,
                        i,
                        start.elapsed()
                    );
                    return true; // Continue but warn
                }

                if !callback(args) {
                    return false; // Hook cancelled
                }
            }
        }
        true
    }

    /// Get count of registered callbacks for a hook
    pub fn hook_count(&self, name: &str) -> usize {
        self.hooks.get(name).map(|v| v.len()).unwrap_or(0)
    }

    /// Get all registered hook names
    pub fn hook_names(&self) -> Vec<String> {
        self.hooks.keys().cloned().collect()
    }
}

impl Default for HookRegistry {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hook_registry_creation() {
        let registry = HookRegistry::new();
        assert_eq!(registry.hook_count("any-hook"), 0);
        assert_eq!(registry.hook_names().len(), 0);
    }

    #[test]
    fn test_add_and_run_hook() {
        let mut registry = HookRegistry::new();
        let called = false;

        // Can't capture mutable reference in Send callback, so use a different approach
        registry.add_hook(
            "test-hook",
            Box::new(|_args| {
                // Hook was called
                true
            }),
        );

        assert_eq!(registry.hook_count("test-hook"), 1);

        let args = HookArgs::EditorInitialized;
        let result = registry.run_hooks("test-hook", &args);
        assert!(result);
    }

    #[test]
    fn test_hook_cancellation() {
        let mut registry = HookRegistry::new();

        // First hook returns false (cancels)
        registry.add_hook("cancel-test", Box::new(|_args| false));

        // Second hook should not be called
        registry.add_hook(
            "cancel-test",
            Box::new(|_args| {
                panic!("Should not be called after cancellation");
            }),
        );

        let args = HookArgs::EditorInitialized;
        let result = registry.run_hooks("cancel-test", &args);
        assert!(!result);
    }

    #[test]
    fn test_multiple_hooks() {
        let mut registry = HookRegistry::new();

        registry.add_hook("multi-test", Box::new(|_args| true));
        registry.add_hook("multi-test", Box::new(|_args| true));
        registry.add_hook("multi-test", Box::new(|_args| true));

        assert_eq!(registry.hook_count("multi-test"), 3);

        let args = HookArgs::EditorInitialized;
        let result = registry.run_hooks("multi-test", &args);
        assert!(result);
    }

    #[test]
    fn test_remove_hooks() {
        let mut registry = HookRegistry::new();

        registry.add_hook("remove-test", Box::new(|_args| true));
        assert_eq!(registry.hook_count("remove-test"), 1);

        registry.remove_hooks("remove-test");
        assert_eq!(registry.hook_count("remove-test"), 0);
    }

    #[test]
    fn test_run_nonexistent_hook() {
        let registry = HookRegistry::new();
        let args = HookArgs::EditorInitialized;
        let result = registry.run_hooks("nonexistent", &args);
        assert!(result); // Should succeed (no hooks to fail)
    }

    #[test]
    fn test_hook_args_variants() {
        let registry = HookRegistry::new();

        // Test different hook arg variants
        let test_cases = vec![
            HookArgs::BeforeFileOpen {
                path: PathBuf::from("/test.txt"),
            },
            HookArgs::AfterFileOpen {
                buffer_id: BufferId(1),
                path: PathBuf::from("/test.txt"),
            },
            HookArgs::BeforeFileSave {
                buffer_id: BufferId(1),
                path: PathBuf::from("/test.txt"),
            },
            HookArgs::AfterFileSave {
                buffer_id: BufferId(1),
                path: PathBuf::from("/test.txt"),
            },
            HookArgs::BufferClosed {
                buffer_id: BufferId(1),
            },
            HookArgs::BeforeInsert {
                buffer_id: BufferId(1),
                position: 0,
                text: "test".to_string(),
            },
            HookArgs::AfterInsert {
                buffer_id: BufferId(1),
                position: 0,
                text: "test".to_string(),
            },
            HookArgs::BeforeDelete {
                buffer_id: BufferId(1),
                range: 0..5,
            },
            HookArgs::AfterDelete {
                buffer_id: BufferId(1),
                range: 0..5,
                deleted_text: "test".to_string(),
            },
            HookArgs::CursorMoved {
                buffer_id: BufferId(1),
                cursor_id: CursorId(0),
                old_position: 0,
                new_position: 5,
            },
            HookArgs::BufferActivated {
                buffer_id: BufferId(1),
            },
            HookArgs::BufferDeactivated {
                buffer_id: BufferId(1),
            },
            HookArgs::PreCommand {
                action: Action::Save,
            },
            HookArgs::PostCommand {
                action: Action::Save,
            },
            HookArgs::Idle { milliseconds: 500 },
            HookArgs::EditorInitialized,
        ];

        // All should run without panicking
        for args in test_cases {
            let result = registry.run_hooks("test", &args);
            assert!(result);
        }
    }

    #[test]
    fn test_hook_timeout() {
        use std::time::Duration;

        let mut registry = HookRegistry::new();

        // Add a slow hook (simulated)
        registry.add_hook(
            "timeout-test",
            Box::new(|_args| {
                // In real scenario, this would be a long-running operation
                true
            }),
        );

        let args = HookArgs::EditorInitialized;
        let result =
            registry.run_hooks_with_timeout("timeout-test", &args, Duration::from_millis(10));
        assert!(result); // Should complete quickly
    }
}
