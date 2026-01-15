use std::collections::HashMap;

/// Trait for the editor to provide services to the plugin runtime
/// without the runtime depending directly on UI or complex system logic.
pub trait PluginServiceBridge: Send + Sync + 'static {
    /// Support downcasting for tests
    fn as_any(&self) -> &dyn std::any::Any;

    /// Translate a string for a plugin
    fn translate(&self, plugin_name: &str, key: &str, args: &HashMap<String, String>) -> String;

    /// Get the current locale
    fn current_locale(&self) -> String;

    /// Update the current JavaScript execution state (for debugging/signal handlers)
    fn set_js_execution_state(&self, state: String);

    /// Clear the JavaScript execution state
    fn clear_js_execution_state(&self);

    /// Get the JSON schema for themes
    fn get_theme_schema(&self) -> serde_json::Value;

    /// Get a list of builtin theme names
    fn get_builtin_themes(&self) -> serde_json::Value;

    /// Register custom i18n strings for a plugin
    fn register_plugin_strings(
        &self,
        _plugin_name: &str,
        _strings: HashMap<String, HashMap<String, String>>,
    ) {
    }

    /// Unregister custom i18n strings for a plugin
    fn unregister_plugin_strings(&self, _plugin_name: &str) {}

    /// Register a plugin command
    fn register_command(&self, command: crate::command::Command);

    /// Unregister a command by name
    fn unregister_command(&self, name: &str);

    /// Unregister all commands with a given prefix
    fn unregister_commands_by_prefix(&self, prefix: &str);

    /// Get the plugins directory path
    fn plugins_dir(&self) -> std::path::PathBuf;

    /// Get the config directory path
    fn config_dir(&self) -> std::path::PathBuf;
}

/// A no-op implementation of the service bridge for testing
pub struct NoopServiceBridge;

impl PluginServiceBridge for NoopServiceBridge {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn translate(&self, _plugin_name: &str, key: &str, _args: &HashMap<String, String>) -> String {
        key.to_string()
    }
    fn current_locale(&self) -> String {
        "en".to_string()
    }
    fn set_js_execution_state(&self, _state: String) {}
    fn clear_js_execution_state(&self) {}
    fn get_theme_schema(&self) -> serde_json::Value {
        serde_json::Value::Null
    }
    fn get_builtin_themes(&self) -> serde_json::Value {
        serde_json::Value::Null
    }
    fn register_plugin_strings(
        &self,
        _plugin_name: &str,
        _strings: HashMap<String, HashMap<String, String>>,
    ) {
    }
    fn unregister_plugin_strings(&self, _plugin_name: &str) {}
    fn register_command(&self, _command: crate::command::Command) {}
    fn unregister_command(&self, _name: &str) {}
    fn unregister_commands_by_prefix(&self, _prefix: &str) {}
    fn plugins_dir(&self) -> std::path::PathBuf {
        std::path::PathBuf::from("/tmp/plugins")
    }
    fn config_dir(&self) -> std::path::PathBuf {
        std::path::PathBuf::from("/tmp/config")
    }
}
