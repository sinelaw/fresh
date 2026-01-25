use crate::config_io::DirectoryContext;
use crate::i18n;
use crate::input::command_registry::CommandRegistry;
use crate::services::signal_handler;
use crate::view::theme;
use fresh_core::services::PluginServiceBridge;
use std::any::Any;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::{Arc, RwLock};

pub struct EditorServiceBridge {
    pub command_registry: Arc<RwLock<CommandRegistry>>,
    pub dir_context: DirectoryContext,
}

impl PluginServiceBridge for EditorServiceBridge {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn translate(&self, plugin_name: &str, key: &str, args: &HashMap<String, String>) -> String {
        i18n::translate_plugin_string(plugin_name, key, args)
    }

    fn current_locale(&self) -> String {
        i18n::current_locale()
    }

    fn set_js_execution_state(&self, state: String) {
        signal_handler::set_js_execution_state(state);
    }

    fn clear_js_execution_state(&self) {
        signal_handler::clear_js_execution_state();
    }

    fn get_theme_schema(&self) -> serde_json::Value {
        theme::get_theme_schema()
    }

    fn get_builtin_themes(&self) -> serde_json::Value {
        theme::get_builtin_themes()
    }

    fn register_plugin_strings(
        &self,
        plugin_name: &str,
        strings: HashMap<String, HashMap<String, String>>,
    ) {
        i18n::register_plugin_strings(plugin_name, strings);
    }

    fn unregister_plugin_strings(&self, plugin_name: &str) {
        i18n::unregister_plugin_strings(plugin_name);
    }

    fn register_command(&self, command: fresh_core::command::Command) {
        // Convert fresh_core::command::Command to crate::input::commands::Command
        use crate::input::commands::{Command as EditorCommand, CommandSource};
        use crate::input::keybindings::{Action, KeyContext};

        let editor_command = EditorCommand {
            name: command.name,
            description: command.description,
            action: Action::PluginAction(command.action_name),
            contexts: vec![KeyContext::Global],
            custom_contexts: command.custom_contexts,
            source: CommandSource::Plugin(command.plugin_name),
        };
        self.command_registry
            .read()
            .unwrap()
            .register(editor_command);
    }

    fn unregister_command(&self, name: &str) {
        self.command_registry.read().unwrap().unregister(name);
    }

    fn unregister_commands_by_prefix(&self, prefix: &str) {
        self.command_registry
            .read()
            .unwrap()
            .unregister_by_prefix(prefix);
    }

    fn unregister_commands_by_plugin(&self, plugin_name: &str) {
        self.command_registry
            .read()
            .unwrap()
            .unregister_by_plugin(plugin_name);
    }

    fn plugins_dir(&self) -> PathBuf {
        self.dir_context.plugins_dir()
    }

    fn config_dir(&self) -> PathBuf {
        self.dir_context.config_dir.clone()
    }
}
