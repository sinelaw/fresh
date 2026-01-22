//! Partial configuration types for layered config merging.
//!
//! This module provides `Option`-wrapped versions of all config structs,
//! enabling a 4-level overlay architecture (System → User → Project → Session).

use crate::config::{
    AcceptSuggestionOnEnter, CursorStyle, FileBrowserConfig, FileExplorerConfig, FormatterConfig,
    HighlighterPreference, Keybinding, KeybindingMapName, KeymapConfig, LanguageConfig,
    LineEndingOption, OnSaveAction, PluginConfig, TerminalConfig, ThemeName, WarningsConfig,
};
use crate::types::LspServerConfig;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Trait for merging configuration layers.
/// Higher precedence values (self) override lower precedence (other).
pub trait Merge {
    /// Merge values from a lower-precedence layer into this layer.
    /// Values already set in self take precedence over values in other.
    fn merge_from(&mut self, other: &Self);
}

impl<T: Clone> Merge for Option<T> {
    fn merge_from(&mut self, other: &Self) {
        if self.is_none() {
            *self = other.clone();
        }
    }
}

/// Merge two HashMaps where self's entries take precedence.
/// Entries from other are added if not present in self.
fn merge_hashmap<K: Clone + Eq + std::hash::Hash, V: Clone>(
    target: &mut Option<HashMap<K, V>>,
    other: &Option<HashMap<K, V>>,
) {
    match (target, other) {
        (Some(t), Some(o)) => {
            for (key, value) in o {
                t.entry(key.clone()).or_insert_with(|| value.clone());
            }
        }
        (t @ None, Some(o)) => {
            *t = Some(o.clone());
        }
        _ => {}
    }
}

/// Merge two HashMaps where values implement Merge (for recursive merging).
fn merge_hashmap_recursive<K, V>(target: &mut Option<HashMap<K, V>>, other: &Option<HashMap<K, V>>)
where
    K: Clone + Eq + std::hash::Hash,
    V: Clone + Merge + Default,
{
    match (target, other) {
        (Some(t), Some(o)) => {
            for (key, value) in o {
                t.entry(key.clone())
                    .and_modify(|existing| existing.merge_from(value))
                    .or_insert_with(|| value.clone());
            }
        }
        (t @ None, Some(o)) => {
            *t = Some(o.clone());
        }
        _ => {}
    }
}

/// Partial configuration where all fields are optional.
/// Represents a single configuration layer (User, Project, or Session).
#[derive(Debug, Clone, Default, Deserialize, Serialize)]
#[serde(default)]
pub struct PartialConfig {
    pub version: Option<u32>,
    pub theme: Option<ThemeName>,
    pub locale: Option<String>,
    pub check_for_updates: Option<bool>,
    pub editor: Option<PartialEditorConfig>,
    pub file_explorer: Option<PartialFileExplorerConfig>,
    pub file_browser: Option<PartialFileBrowserConfig>,
    pub terminal: Option<PartialTerminalConfig>,
    pub keybindings: Option<Vec<Keybinding>>,
    pub keybinding_maps: Option<HashMap<String, KeymapConfig>>,
    pub active_keybinding_map: Option<KeybindingMapName>,
    pub languages: Option<HashMap<String, PartialLanguageConfig>>,
    pub lsp: Option<HashMap<String, LspServerConfig>>,
    pub warnings: Option<PartialWarningsConfig>,
    pub plugins: Option<HashMap<String, PartialPluginConfig>>,
}

impl Merge for PartialConfig {
    fn merge_from(&mut self, other: &Self) {
        self.version.merge_from(&other.version);
        self.theme.merge_from(&other.theme);
        self.locale.merge_from(&other.locale);
        self.check_for_updates.merge_from(&other.check_for_updates);

        // Nested structs: merge recursively
        merge_partial(&mut self.editor, &other.editor);
        merge_partial(&mut self.file_explorer, &other.file_explorer);
        merge_partial(&mut self.file_browser, &other.file_browser);
        merge_partial(&mut self.terminal, &other.terminal);
        merge_partial(&mut self.warnings, &other.warnings);

        // Lists: higher precedence replaces (per design doc)
        self.keybindings.merge_from(&other.keybindings);

        // HashMaps: merge entries, higher precedence wins on key collision
        merge_hashmap(&mut self.keybinding_maps, &other.keybinding_maps);
        merge_hashmap_recursive(&mut self.languages, &other.languages);
        merge_hashmap_recursive(&mut self.lsp, &other.lsp);
        merge_hashmap_recursive(&mut self.plugins, &other.plugins);

        self.active_keybinding_map
            .merge_from(&other.active_keybinding_map);
    }
}

/// Helper to merge nested partial structs.
fn merge_partial<T: Merge + Clone>(target: &mut Option<T>, other: &Option<T>) {
    match (target, other) {
        (Some(t), Some(o)) => t.merge_from(o),
        (t @ None, Some(o)) => *t = Some(o.clone()),
        _ => {}
    }
}

/// Partial editor configuration.
#[derive(Debug, Clone, Default, Deserialize, Serialize)]
#[serde(default)]
pub struct PartialEditorConfig {
    pub tab_size: Option<usize>,
    pub auto_indent: Option<bool>,
    pub line_numbers: Option<bool>,
    pub relative_line_numbers: Option<bool>,
    pub scroll_offset: Option<usize>,
    pub syntax_highlighting: Option<bool>,
    pub line_wrap: Option<bool>,
    pub highlight_timeout_ms: Option<u64>,
    pub snapshot_interval: Option<usize>,
    pub large_file_threshold_bytes: Option<u64>,
    pub estimated_line_length: Option<usize>,
    pub enable_inlay_hints: Option<bool>,
    pub enable_semantic_tokens_full: Option<bool>,
    pub recovery_enabled: Option<bool>,
    pub auto_save_interval_secs: Option<u32>,
    pub highlight_context_bytes: Option<usize>,
    pub mouse_hover_enabled: Option<bool>,
    pub mouse_hover_delay_ms: Option<u64>,
    pub double_click_time_ms: Option<u64>,
    pub auto_revert_poll_interval_ms: Option<u64>,
    pub file_tree_poll_interval_ms: Option<u64>,
    pub default_line_ending: Option<LineEndingOption>,
    pub cursor_style: Option<CursorStyle>,
    pub keyboard_disambiguate_escape_codes: Option<bool>,
    pub keyboard_report_event_types: Option<bool>,
    pub keyboard_report_alternate_keys: Option<bool>,
    pub keyboard_report_all_keys_as_escape_codes: Option<bool>,
    pub quick_suggestions: Option<bool>,
    pub quick_suggestions_delay_ms: Option<u64>,
    pub suggest_on_trigger_characters: Option<bool>,
    pub accept_suggestion_on_enter: Option<AcceptSuggestionOnEnter>,
    pub show_menu_bar: Option<bool>,
    pub show_tab_bar: Option<bool>,
    pub use_terminal_bg: Option<bool>,
}

impl Merge for PartialEditorConfig {
    fn merge_from(&mut self, other: &Self) {
        self.tab_size.merge_from(&other.tab_size);
        self.auto_indent.merge_from(&other.auto_indent);
        self.line_numbers.merge_from(&other.line_numbers);
        self.relative_line_numbers
            .merge_from(&other.relative_line_numbers);
        self.scroll_offset.merge_from(&other.scroll_offset);
        self.syntax_highlighting
            .merge_from(&other.syntax_highlighting);
        self.line_wrap.merge_from(&other.line_wrap);
        self.highlight_timeout_ms
            .merge_from(&other.highlight_timeout_ms);
        self.snapshot_interval.merge_from(&other.snapshot_interval);
        self.large_file_threshold_bytes
            .merge_from(&other.large_file_threshold_bytes);
        self.estimated_line_length
            .merge_from(&other.estimated_line_length);
        self.enable_inlay_hints
            .merge_from(&other.enable_inlay_hints);
        self.enable_semantic_tokens_full
            .merge_from(&other.enable_semantic_tokens_full);
        self.recovery_enabled.merge_from(&other.recovery_enabled);
        self.auto_save_interval_secs
            .merge_from(&other.auto_save_interval_secs);
        self.highlight_context_bytes
            .merge_from(&other.highlight_context_bytes);
        self.mouse_hover_enabled
            .merge_from(&other.mouse_hover_enabled);
        self.mouse_hover_delay_ms
            .merge_from(&other.mouse_hover_delay_ms);
        self.double_click_time_ms
            .merge_from(&other.double_click_time_ms);
        self.auto_revert_poll_interval_ms
            .merge_from(&other.auto_revert_poll_interval_ms);
        self.file_tree_poll_interval_ms
            .merge_from(&other.file_tree_poll_interval_ms);
        self.default_line_ending
            .merge_from(&other.default_line_ending);
        self.cursor_style.merge_from(&other.cursor_style);
        self.keyboard_disambiguate_escape_codes
            .merge_from(&other.keyboard_disambiguate_escape_codes);
        self.keyboard_report_event_types
            .merge_from(&other.keyboard_report_event_types);
        self.keyboard_report_alternate_keys
            .merge_from(&other.keyboard_report_alternate_keys);
        self.keyboard_report_all_keys_as_escape_codes
            .merge_from(&other.keyboard_report_all_keys_as_escape_codes);
        self.quick_suggestions.merge_from(&other.quick_suggestions);
        self.quick_suggestions_delay_ms
            .merge_from(&other.quick_suggestions_delay_ms);
        self.suggest_on_trigger_characters
            .merge_from(&other.suggest_on_trigger_characters);
        self.accept_suggestion_on_enter
            .merge_from(&other.accept_suggestion_on_enter);
        self.show_menu_bar.merge_from(&other.show_menu_bar);
        self.show_tab_bar.merge_from(&other.show_tab_bar);
        self.use_terminal_bg.merge_from(&other.use_terminal_bg);
    }
}

/// Partial file explorer configuration.
#[derive(Debug, Clone, Default, Deserialize, Serialize)]
#[serde(default)]
pub struct PartialFileExplorerConfig {
    pub respect_gitignore: Option<bool>,
    pub show_hidden: Option<bool>,
    pub show_gitignored: Option<bool>,
    pub custom_ignore_patterns: Option<Vec<String>>,
    pub width: Option<f32>,
}

impl Merge for PartialFileExplorerConfig {
    fn merge_from(&mut self, other: &Self) {
        self.respect_gitignore.merge_from(&other.respect_gitignore);
        self.show_hidden.merge_from(&other.show_hidden);
        self.show_gitignored.merge_from(&other.show_gitignored);
        self.custom_ignore_patterns
            .merge_from(&other.custom_ignore_patterns);
        self.width.merge_from(&other.width);
    }
}

/// Partial file browser configuration.
#[derive(Debug, Clone, Default, Deserialize, Serialize)]
#[serde(default)]
pub struct PartialFileBrowserConfig {
    pub show_hidden: Option<bool>,
}

impl Merge for PartialFileBrowserConfig {
    fn merge_from(&mut self, other: &Self) {
        self.show_hidden.merge_from(&other.show_hidden);
    }
}

/// Partial terminal configuration.
#[derive(Debug, Clone, Default, Deserialize, Serialize)]
#[serde(default)]
pub struct PartialTerminalConfig {
    pub jump_to_end_on_output: Option<bool>,
}

impl Merge for PartialTerminalConfig {
    fn merge_from(&mut self, other: &Self) {
        self.jump_to_end_on_output
            .merge_from(&other.jump_to_end_on_output);
    }
}

/// Partial warnings configuration.
#[derive(Debug, Clone, Default, Deserialize, Serialize)]
#[serde(default)]
pub struct PartialWarningsConfig {
    pub show_status_indicator: Option<bool>,
}

impl Merge for PartialWarningsConfig {
    fn merge_from(&mut self, other: &Self) {
        self.show_status_indicator
            .merge_from(&other.show_status_indicator);
    }
}

/// Partial plugin configuration.
#[derive(Debug, Clone, Default, Deserialize, Serialize)]
#[serde(default)]
pub struct PartialPluginConfig {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub enabled: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub path: Option<std::path::PathBuf>,
}

impl Merge for PartialPluginConfig {
    fn merge_from(&mut self, other: &Self) {
        self.enabled.merge_from(&other.enabled);
        self.path.merge_from(&other.path);
    }
}

/// Partial language configuration.
#[derive(Debug, Clone, Default, Deserialize, Serialize)]
#[serde(default)]
pub struct PartialLanguageConfig {
    pub extensions: Option<Vec<String>>,
    pub filenames: Option<Vec<String>>,
    pub grammar: Option<String>,
    pub comment_prefix: Option<String>,
    pub auto_indent: Option<bool>,
    pub highlighter: Option<HighlighterPreference>,
    pub textmate_grammar: Option<std::path::PathBuf>,
    pub show_whitespace_tabs: Option<bool>,
    pub use_tabs: Option<bool>,
    pub tab_size: Option<usize>,
    pub formatter: Option<FormatterConfig>,
    pub format_on_save: Option<bool>,
    pub on_save: Option<Vec<OnSaveAction>>,
}

impl Merge for PartialLanguageConfig {
    fn merge_from(&mut self, other: &Self) {
        self.extensions.merge_from(&other.extensions);
        self.filenames.merge_from(&other.filenames);
        self.grammar.merge_from(&other.grammar);
        self.comment_prefix.merge_from(&other.comment_prefix);
        self.auto_indent.merge_from(&other.auto_indent);
        self.highlighter.merge_from(&other.highlighter);
        self.textmate_grammar.merge_from(&other.textmate_grammar);
        self.show_whitespace_tabs
            .merge_from(&other.show_whitespace_tabs);
        self.use_tabs.merge_from(&other.use_tabs);
        self.tab_size.merge_from(&other.tab_size);
        self.formatter.merge_from(&other.formatter);
        self.format_on_save.merge_from(&other.format_on_save);
        self.on_save.merge_from(&other.on_save);
    }
}

impl Merge for LspServerConfig {
    fn merge_from(&mut self, other: &Self) {
        // If command is empty (serde default), use other's command
        if self.command.is_empty() {
            self.command = other.command.clone();
        }
        // If args is empty, use other's args
        if self.args.is_empty() {
            self.args = other.args.clone();
        }
        // For booleans, keep self's value (we can't tell if explicitly set)
        // For process_limits, keep self's value
        // For initialization_options, use self if Some, otherwise other
        if self.initialization_options.is_none() {
            self.initialization_options = other.initialization_options.clone();
        }
    }
}

// Conversion traits for resolving partial configs to concrete configs

impl From<&crate::config::EditorConfig> for PartialEditorConfig {
    fn from(cfg: &crate::config::EditorConfig) -> Self {
        Self {
            tab_size: Some(cfg.tab_size),
            auto_indent: Some(cfg.auto_indent),
            line_numbers: Some(cfg.line_numbers),
            relative_line_numbers: Some(cfg.relative_line_numbers),
            scroll_offset: Some(cfg.scroll_offset),
            syntax_highlighting: Some(cfg.syntax_highlighting),
            line_wrap: Some(cfg.line_wrap),
            highlight_timeout_ms: Some(cfg.highlight_timeout_ms),
            snapshot_interval: Some(cfg.snapshot_interval),
            large_file_threshold_bytes: Some(cfg.large_file_threshold_bytes),
            estimated_line_length: Some(cfg.estimated_line_length),
            enable_inlay_hints: Some(cfg.enable_inlay_hints),
            enable_semantic_tokens_full: Some(cfg.enable_semantic_tokens_full),
            recovery_enabled: Some(cfg.recovery_enabled),
            auto_save_interval_secs: Some(cfg.auto_save_interval_secs),
            highlight_context_bytes: Some(cfg.highlight_context_bytes),
            mouse_hover_enabled: Some(cfg.mouse_hover_enabled),
            mouse_hover_delay_ms: Some(cfg.mouse_hover_delay_ms),
            double_click_time_ms: Some(cfg.double_click_time_ms),
            auto_revert_poll_interval_ms: Some(cfg.auto_revert_poll_interval_ms),
            file_tree_poll_interval_ms: Some(cfg.file_tree_poll_interval_ms),
            default_line_ending: Some(cfg.default_line_ending.clone()),
            cursor_style: Some(cfg.cursor_style),
            keyboard_disambiguate_escape_codes: Some(cfg.keyboard_disambiguate_escape_codes),
            keyboard_report_event_types: Some(cfg.keyboard_report_event_types),
            keyboard_report_alternate_keys: Some(cfg.keyboard_report_alternate_keys),
            keyboard_report_all_keys_as_escape_codes: Some(
                cfg.keyboard_report_all_keys_as_escape_codes,
            ),
            quick_suggestions: Some(cfg.quick_suggestions),
            quick_suggestions_delay_ms: Some(cfg.quick_suggestions_delay_ms),
            suggest_on_trigger_characters: Some(cfg.suggest_on_trigger_characters),
            accept_suggestion_on_enter: Some(cfg.accept_suggestion_on_enter),
            show_menu_bar: Some(cfg.show_menu_bar),
            show_tab_bar: Some(cfg.show_tab_bar),
            use_terminal_bg: Some(cfg.use_terminal_bg),
        }
    }
}

impl PartialEditorConfig {
    /// Resolve this partial config to a concrete EditorConfig using defaults.
    pub fn resolve(self, defaults: &crate::config::EditorConfig) -> crate::config::EditorConfig {
        crate::config::EditorConfig {
            tab_size: self.tab_size.unwrap_or(defaults.tab_size),
            auto_indent: self.auto_indent.unwrap_or(defaults.auto_indent),
            line_numbers: self.line_numbers.unwrap_or(defaults.line_numbers),
            relative_line_numbers: self
                .relative_line_numbers
                .unwrap_or(defaults.relative_line_numbers),
            scroll_offset: self.scroll_offset.unwrap_or(defaults.scroll_offset),
            syntax_highlighting: self
                .syntax_highlighting
                .unwrap_or(defaults.syntax_highlighting),
            line_wrap: self.line_wrap.unwrap_or(defaults.line_wrap),
            highlight_timeout_ms: self
                .highlight_timeout_ms
                .unwrap_or(defaults.highlight_timeout_ms),
            snapshot_interval: self.snapshot_interval.unwrap_or(defaults.snapshot_interval),
            large_file_threshold_bytes: self
                .large_file_threshold_bytes
                .unwrap_or(defaults.large_file_threshold_bytes),
            estimated_line_length: self
                .estimated_line_length
                .unwrap_or(defaults.estimated_line_length),
            enable_inlay_hints: self
                .enable_inlay_hints
                .unwrap_or(defaults.enable_inlay_hints),
            enable_semantic_tokens_full: self
                .enable_semantic_tokens_full
                .unwrap_or(defaults.enable_semantic_tokens_full),
            recovery_enabled: self.recovery_enabled.unwrap_or(defaults.recovery_enabled),
            auto_save_interval_secs: self
                .auto_save_interval_secs
                .unwrap_or(defaults.auto_save_interval_secs),
            highlight_context_bytes: self
                .highlight_context_bytes
                .unwrap_or(defaults.highlight_context_bytes),
            mouse_hover_enabled: self
                .mouse_hover_enabled
                .unwrap_or(defaults.mouse_hover_enabled),
            mouse_hover_delay_ms: self
                .mouse_hover_delay_ms
                .unwrap_or(defaults.mouse_hover_delay_ms),
            double_click_time_ms: self
                .double_click_time_ms
                .unwrap_or(defaults.double_click_time_ms),
            auto_revert_poll_interval_ms: self
                .auto_revert_poll_interval_ms
                .unwrap_or(defaults.auto_revert_poll_interval_ms),
            file_tree_poll_interval_ms: self
                .file_tree_poll_interval_ms
                .unwrap_or(defaults.file_tree_poll_interval_ms),
            default_line_ending: self
                .default_line_ending
                .unwrap_or(defaults.default_line_ending.clone()),
            cursor_style: self.cursor_style.unwrap_or(defaults.cursor_style),
            keyboard_disambiguate_escape_codes: self
                .keyboard_disambiguate_escape_codes
                .unwrap_or(defaults.keyboard_disambiguate_escape_codes),
            keyboard_report_event_types: self
                .keyboard_report_event_types
                .unwrap_or(defaults.keyboard_report_event_types),
            keyboard_report_alternate_keys: self
                .keyboard_report_alternate_keys
                .unwrap_or(defaults.keyboard_report_alternate_keys),
            keyboard_report_all_keys_as_escape_codes: self
                .keyboard_report_all_keys_as_escape_codes
                .unwrap_or(defaults.keyboard_report_all_keys_as_escape_codes),
            quick_suggestions: self.quick_suggestions.unwrap_or(defaults.quick_suggestions),
            quick_suggestions_delay_ms: self
                .quick_suggestions_delay_ms
                .unwrap_or(defaults.quick_suggestions_delay_ms),
            suggest_on_trigger_characters: self
                .suggest_on_trigger_characters
                .unwrap_or(defaults.suggest_on_trigger_characters),
            accept_suggestion_on_enter: self
                .accept_suggestion_on_enter
                .unwrap_or(defaults.accept_suggestion_on_enter),
            show_menu_bar: self.show_menu_bar.unwrap_or(defaults.show_menu_bar),
            show_tab_bar: self.show_tab_bar.unwrap_or(defaults.show_tab_bar),
            use_terminal_bg: self.use_terminal_bg.unwrap_or(defaults.use_terminal_bg),
        }
    }
}

impl From<&FileExplorerConfig> for PartialFileExplorerConfig {
    fn from(cfg: &FileExplorerConfig) -> Self {
        Self {
            respect_gitignore: Some(cfg.respect_gitignore),
            show_hidden: Some(cfg.show_hidden),
            show_gitignored: Some(cfg.show_gitignored),
            custom_ignore_patterns: Some(cfg.custom_ignore_patterns.clone()),
            width: Some(cfg.width),
        }
    }
}

impl PartialFileExplorerConfig {
    pub fn resolve(self, defaults: &FileExplorerConfig) -> FileExplorerConfig {
        FileExplorerConfig {
            respect_gitignore: self.respect_gitignore.unwrap_or(defaults.respect_gitignore),
            show_hidden: self.show_hidden.unwrap_or(defaults.show_hidden),
            show_gitignored: self.show_gitignored.unwrap_or(defaults.show_gitignored),
            custom_ignore_patterns: self
                .custom_ignore_patterns
                .unwrap_or_else(|| defaults.custom_ignore_patterns.clone()),
            width: self.width.unwrap_or(defaults.width),
        }
    }
}

impl From<&FileBrowserConfig> for PartialFileBrowserConfig {
    fn from(cfg: &FileBrowserConfig) -> Self {
        Self {
            show_hidden: Some(cfg.show_hidden),
        }
    }
}

impl PartialFileBrowserConfig {
    pub fn resolve(self, defaults: &FileBrowserConfig) -> FileBrowserConfig {
        FileBrowserConfig {
            show_hidden: self.show_hidden.unwrap_or(defaults.show_hidden),
        }
    }
}

impl From<&TerminalConfig> for PartialTerminalConfig {
    fn from(cfg: &TerminalConfig) -> Self {
        Self {
            jump_to_end_on_output: Some(cfg.jump_to_end_on_output),
        }
    }
}

impl PartialTerminalConfig {
    pub fn resolve(self, defaults: &TerminalConfig) -> TerminalConfig {
        TerminalConfig {
            jump_to_end_on_output: self
                .jump_to_end_on_output
                .unwrap_or(defaults.jump_to_end_on_output),
        }
    }
}

impl From<&WarningsConfig> for PartialWarningsConfig {
    fn from(cfg: &WarningsConfig) -> Self {
        Self {
            show_status_indicator: Some(cfg.show_status_indicator),
        }
    }
}

impl PartialWarningsConfig {
    pub fn resolve(self, defaults: &WarningsConfig) -> WarningsConfig {
        WarningsConfig {
            show_status_indicator: self
                .show_status_indicator
                .unwrap_or(defaults.show_status_indicator),
        }
    }
}

impl From<&PluginConfig> for PartialPluginConfig {
    fn from(cfg: &PluginConfig) -> Self {
        Self {
            enabled: Some(cfg.enabled),
            path: cfg.path.clone(),
        }
    }
}

impl PartialPluginConfig {
    pub fn resolve(self, defaults: &PluginConfig) -> PluginConfig {
        PluginConfig {
            enabled: self.enabled.unwrap_or(defaults.enabled),
            path: self.path.or_else(|| defaults.path.clone()),
        }
    }
}

impl From<&LanguageConfig> for PartialLanguageConfig {
    fn from(cfg: &LanguageConfig) -> Self {
        Self {
            extensions: Some(cfg.extensions.clone()),
            filenames: Some(cfg.filenames.clone()),
            grammar: Some(cfg.grammar.clone()),
            comment_prefix: cfg.comment_prefix.clone(),
            auto_indent: Some(cfg.auto_indent),
            highlighter: Some(cfg.highlighter),
            textmate_grammar: cfg.textmate_grammar.clone(),
            show_whitespace_tabs: Some(cfg.show_whitespace_tabs),
            use_tabs: Some(cfg.use_tabs),
            tab_size: cfg.tab_size,
            formatter: cfg.formatter.clone(),
            format_on_save: Some(cfg.format_on_save),
            on_save: Some(cfg.on_save.clone()),
        }
    }
}

impl PartialLanguageConfig {
    pub fn resolve(self, defaults: &LanguageConfig) -> LanguageConfig {
        LanguageConfig {
            extensions: self
                .extensions
                .unwrap_or_else(|| defaults.extensions.clone()),
            filenames: self.filenames.unwrap_or_else(|| defaults.filenames.clone()),
            grammar: self.grammar.unwrap_or_else(|| defaults.grammar.clone()),
            comment_prefix: self
                .comment_prefix
                .or_else(|| defaults.comment_prefix.clone()),
            auto_indent: self.auto_indent.unwrap_or(defaults.auto_indent),
            highlighter: self.highlighter.unwrap_or(defaults.highlighter),
            textmate_grammar: self
                .textmate_grammar
                .or_else(|| defaults.textmate_grammar.clone()),
            show_whitespace_tabs: self
                .show_whitespace_tabs
                .unwrap_or(defaults.show_whitespace_tabs),
            use_tabs: self.use_tabs.unwrap_or(defaults.use_tabs),
            tab_size: self.tab_size.or(defaults.tab_size),
            formatter: self.formatter.or_else(|| defaults.formatter.clone()),
            format_on_save: self.format_on_save.unwrap_or(defaults.format_on_save),
            on_save: self.on_save.unwrap_or_else(|| defaults.on_save.clone()),
        }
    }
}

impl From<&crate::config::Config> for PartialConfig {
    fn from(cfg: &crate::config::Config) -> Self {
        Self {
            version: Some(cfg.version),
            theme: Some(cfg.theme.clone()),
            locale: cfg.locale.0.clone(),
            check_for_updates: Some(cfg.check_for_updates),
            editor: Some(PartialEditorConfig::from(&cfg.editor)),
            file_explorer: Some(PartialFileExplorerConfig::from(&cfg.file_explorer)),
            file_browser: Some(PartialFileBrowserConfig::from(&cfg.file_browser)),
            terminal: Some(PartialTerminalConfig::from(&cfg.terminal)),
            keybindings: Some(cfg.keybindings.clone()),
            keybinding_maps: Some(cfg.keybinding_maps.clone()),
            active_keybinding_map: Some(cfg.active_keybinding_map.clone()),
            languages: Some(
                cfg.languages
                    .iter()
                    .map(|(k, v)| (k.clone(), PartialLanguageConfig::from(v)))
                    .collect(),
            ),
            lsp: Some(cfg.lsp.clone()),
            warnings: Some(PartialWarningsConfig::from(&cfg.warnings)),
            // Only include plugins that differ from defaults
            // Path is auto-discovered at runtime and should never be saved
            plugins: {
                let default_plugin = crate::config::PluginConfig::default();
                let non_default_plugins: HashMap<String, PartialPluginConfig> = cfg
                    .plugins
                    .iter()
                    .filter(|(_, v)| v.enabled != default_plugin.enabled)
                    .map(|(k, v)| {
                        (
                            k.clone(),
                            PartialPluginConfig {
                                enabled: Some(v.enabled),
                                path: None, // Don't save path - it's auto-discovered
                            },
                        )
                    })
                    .collect();
                if non_default_plugins.is_empty() {
                    None
                } else {
                    Some(non_default_plugins)
                }
            },
        }
    }
}

impl PartialConfig {
    /// Resolve this partial config to a concrete Config using system defaults.
    pub fn resolve(self) -> crate::config::Config {
        let defaults = crate::config::Config::default();
        self.resolve_with_defaults(&defaults)
    }

    /// Resolve this partial config to a concrete Config using provided defaults.
    pub fn resolve_with_defaults(self, defaults: &crate::config::Config) -> crate::config::Config {
        // Resolve languages HashMap - merge with defaults
        let languages = {
            let mut result = defaults.languages.clone();
            if let Some(partial_langs) = self.languages {
                for (key, partial_lang) in partial_langs {
                    let default_lang = result.get(&key).cloned().unwrap_or_default();
                    result.insert(key, partial_lang.resolve(&default_lang));
                }
            }
            result
        };

        // Resolve lsp HashMap - merge with defaults
        let lsp = {
            let mut result = defaults.lsp.clone();
            if let Some(partial_lsp) = self.lsp {
                for (key, partial_config) in partial_lsp {
                    if let Some(default_config) = result.get(&key) {
                        result.insert(key, partial_config.merge_with_defaults(default_config));
                    } else {
                        // New language not in defaults - use as-is
                        result.insert(key, partial_config);
                    }
                }
            }
            result
        };

        // Resolve keybinding_maps HashMap - merge with defaults
        let keybinding_maps = {
            let mut result = defaults.keybinding_maps.clone();
            if let Some(partial_maps) = self.keybinding_maps {
                for (key, config) in partial_maps {
                    result.insert(key, config);
                }
            }
            result
        };

        // Resolve plugins HashMap - merge with defaults
        let plugins = {
            let mut result = defaults.plugins.clone();
            if let Some(partial_plugins) = self.plugins {
                for (key, partial_plugin) in partial_plugins {
                    let default_plugin = result.get(&key).cloned().unwrap_or_default();
                    result.insert(key, partial_plugin.resolve(&default_plugin));
                }
            }
            result
        };

        crate::config::Config {
            version: self.version.unwrap_or(defaults.version),
            theme: self.theme.unwrap_or_else(|| defaults.theme.clone()),
            locale: crate::config::LocaleName::from(
                self.locale.or_else(|| defaults.locale.0.clone()),
            ),
            check_for_updates: self.check_for_updates.unwrap_or(defaults.check_for_updates),
            editor: self
                .editor
                .map(|e| e.resolve(&defaults.editor))
                .unwrap_or_else(|| defaults.editor.clone()),
            file_explorer: self
                .file_explorer
                .map(|e| e.resolve(&defaults.file_explorer))
                .unwrap_or_else(|| defaults.file_explorer.clone()),
            file_browser: self
                .file_browser
                .map(|e| e.resolve(&defaults.file_browser))
                .unwrap_or_else(|| defaults.file_browser.clone()),
            terminal: self
                .terminal
                .map(|e| e.resolve(&defaults.terminal))
                .unwrap_or_else(|| defaults.terminal.clone()),
            keybindings: self
                .keybindings
                .unwrap_or_else(|| defaults.keybindings.clone()),
            keybinding_maps,
            active_keybinding_map: self
                .active_keybinding_map
                .unwrap_or_else(|| defaults.active_keybinding_map.clone()),
            languages,
            lsp,
            warnings: self
                .warnings
                .map(|e| e.resolve(&defaults.warnings))
                .unwrap_or_else(|| defaults.warnings.clone()),
            plugins,
        }
    }
}

// Default implementation for LanguageConfig to support merge_hashmap_recursive
impl Default for LanguageConfig {
    fn default() -> Self {
        Self {
            extensions: Vec::new(),
            filenames: Vec::new(),
            grammar: String::new(),
            comment_prefix: None,
            auto_indent: true,
            highlighter: HighlighterPreference::default(),
            textmate_grammar: None,
            show_whitespace_tabs: true,
            use_tabs: false,
            tab_size: None,
            formatter: None,
            format_on_save: false,
            on_save: Vec::new(),
        }
    }
}

/// Session-specific configuration for runtime/volatile overrides.
///
/// This struct represents the session layer of the config hierarchy - settings
/// that are temporary and may not persist across editor restarts.
///
/// Unlike PartialConfig, SessionConfig provides a focused API for common
/// runtime modifications like temporary theme switching.
#[derive(Debug, Clone, Default, Deserialize, Serialize)]
#[serde(default)]
pub struct SessionConfig {
    /// Temporarily override the theme (e.g., for preview)
    pub theme: Option<ThemeName>,

    /// Temporary editor overrides (e.g., changing tab_size for current session)
    pub editor: Option<PartialEditorConfig>,

    /// Buffer-specific overrides keyed by absolute file path.
    /// These allow per-file settings that persist only during the session.
    #[serde(default, skip_serializing_if = "HashMap::is_empty")]
    pub buffer_overrides: HashMap<std::path::PathBuf, PartialEditorConfig>,
}

impl SessionConfig {
    /// Create a new empty session config.
    pub fn new() -> Self {
        Self::default()
    }

    /// Set a temporary theme override.
    pub fn set_theme(&mut self, theme: ThemeName) {
        self.theme = Some(theme);
    }

    /// Clear the theme override, reverting to lower layers.
    pub fn clear_theme(&mut self) {
        self.theme = None;
    }

    /// Set an editor setting for the current session.
    pub fn set_editor_option<F>(&mut self, setter: F)
    where
        F: FnOnce(&mut PartialEditorConfig),
    {
        let editor = self.editor.get_or_insert_with(Default::default);
        setter(editor);
    }

    /// Set a buffer-specific editor override.
    pub fn set_buffer_override(&mut self, path: std::path::PathBuf, config: PartialEditorConfig) {
        self.buffer_overrides.insert(path, config);
    }

    /// Clear buffer-specific overrides for a path.
    pub fn clear_buffer_override(&mut self, path: &std::path::Path) {
        self.buffer_overrides.remove(path);
    }

    /// Get buffer-specific editor config if set.
    pub fn get_buffer_override(&self, path: &std::path::Path) -> Option<&PartialEditorConfig> {
        self.buffer_overrides.get(path)
    }

    /// Convert to a PartialConfig for merging with other layers.
    pub fn to_partial_config(&self) -> PartialConfig {
        PartialConfig {
            theme: self.theme.clone(),
            editor: self.editor.clone(),
            ..Default::default()
        }
    }

    /// Check if this session config has any values set.
    pub fn is_empty(&self) -> bool {
        self.theme.is_none() && self.editor.is_none() && self.buffer_overrides.is_empty()
    }
}

impl From<PartialConfig> for SessionConfig {
    fn from(partial: PartialConfig) -> Self {
        Self {
            theme: partial.theme,
            editor: partial.editor,
            buffer_overrides: HashMap::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn merge_option_higher_precedence_wins() {
        let mut higher: Option<i32> = Some(10);
        let lower: Option<i32> = Some(5);
        higher.merge_from(&lower);
        assert_eq!(higher, Some(10));
    }

    #[test]
    fn merge_option_fills_from_lower_when_none() {
        let mut higher: Option<i32> = None;
        let lower: Option<i32> = Some(5);
        higher.merge_from(&lower);
        assert_eq!(higher, Some(5));
    }

    #[test]
    fn merge_editor_config_recursive() {
        let mut higher = PartialEditorConfig {
            tab_size: Some(2),
            ..Default::default()
        };
        let lower = PartialEditorConfig {
            tab_size: Some(4),
            line_numbers: Some(true),
            ..Default::default()
        };

        higher.merge_from(&lower);

        assert_eq!(higher.tab_size, Some(2)); // Higher wins
        assert_eq!(higher.line_numbers, Some(true)); // Filled from lower
    }

    #[test]
    fn merge_partial_config_combines_languages() {
        let mut higher = PartialConfig {
            languages: Some(HashMap::from([(
                "rust".to_string(),
                PartialLanguageConfig {
                    tab_size: Some(4),
                    ..Default::default()
                },
            )])),
            ..Default::default()
        };
        let lower = PartialConfig {
            languages: Some(HashMap::from([(
                "python".to_string(),
                PartialLanguageConfig {
                    tab_size: Some(4),
                    ..Default::default()
                },
            )])),
            ..Default::default()
        };

        higher.merge_from(&lower);

        let langs = higher.languages.unwrap();
        assert!(langs.contains_key("rust"));
        assert!(langs.contains_key("python"));
    }

    #[test]
    fn merge_languages_same_key_higher_wins() {
        let mut higher = PartialConfig {
            languages: Some(HashMap::from([(
                "rust".to_string(),
                PartialLanguageConfig {
                    tab_size: Some(2),
                    use_tabs: Some(true),
                    ..Default::default()
                },
            )])),
            ..Default::default()
        };
        let lower = PartialConfig {
            languages: Some(HashMap::from([(
                "rust".to_string(),
                PartialLanguageConfig {
                    tab_size: Some(4),
                    auto_indent: Some(false),
                    ..Default::default()
                },
            )])),
            ..Default::default()
        };

        higher.merge_from(&lower);

        let langs = higher.languages.unwrap();
        let rust = langs.get("rust").unwrap();
        assert_eq!(rust.tab_size, Some(2)); // Higher wins
        assert_eq!(rust.use_tabs, Some(true)); // From higher
        assert_eq!(rust.auto_indent, Some(false)); // Filled from lower
    }

    #[test]
    fn resolve_fills_defaults() {
        let partial = PartialConfig {
            theme: Some(ThemeName::from("dark")),
            ..Default::default()
        };

        let resolved = partial.resolve();

        assert_eq!(resolved.theme.0, "dark");
        assert_eq!(resolved.editor.tab_size, 4); // Default
        assert!(resolved.editor.line_numbers); // Default true
    }

    #[test]
    fn resolve_preserves_set_values() {
        let partial = PartialConfig {
            editor: Some(PartialEditorConfig {
                tab_size: Some(2),
                line_numbers: Some(false),
                ..Default::default()
            }),
            ..Default::default()
        };

        let resolved = partial.resolve();

        assert_eq!(resolved.editor.tab_size, 2);
        assert!(!resolved.editor.line_numbers);
    }

    #[test]
    fn roundtrip_config_to_partial_and_back() {
        let original = crate::config::Config::default();
        let partial = PartialConfig::from(&original);
        let resolved = partial.resolve();

        assert_eq!(original.theme, resolved.theme);
        assert_eq!(original.editor.tab_size, resolved.editor.tab_size);
        assert_eq!(original.check_for_updates, resolved.check_for_updates);
    }

    #[test]
    fn session_config_new_is_empty() {
        let session = SessionConfig::new();
        assert!(session.is_empty());
    }

    #[test]
    fn session_config_set_theme() {
        let mut session = SessionConfig::new();
        session.set_theme(ThemeName::from("dark"));
        assert_eq!(session.theme, Some(ThemeName::from("dark")));
        assert!(!session.is_empty());
    }

    #[test]
    fn session_config_clear_theme() {
        let mut session = SessionConfig::new();
        session.set_theme(ThemeName::from("dark"));
        session.clear_theme();
        assert!(session.theme.is_none());
    }

    #[test]
    fn session_config_set_editor_option() {
        let mut session = SessionConfig::new();
        session.set_editor_option(|e| e.tab_size = Some(2));
        assert_eq!(session.editor.as_ref().unwrap().tab_size, Some(2));
    }

    #[test]
    fn session_config_buffer_overrides() {
        let mut session = SessionConfig::new();
        let path = std::path::PathBuf::from("/test/file.rs");
        let config = PartialEditorConfig {
            tab_size: Some(8),
            ..Default::default()
        };

        session.set_buffer_override(path.clone(), config);
        assert!(session.get_buffer_override(&path).is_some());
        assert_eq!(
            session.get_buffer_override(&path).unwrap().tab_size,
            Some(8)
        );

        session.clear_buffer_override(&path);
        assert!(session.get_buffer_override(&path).is_none());
    }

    #[test]
    fn session_config_to_partial_config() {
        let mut session = SessionConfig::new();
        session.set_theme(ThemeName::from("dark"));
        session.set_editor_option(|e| e.tab_size = Some(2));

        let partial = session.to_partial_config();
        assert_eq!(partial.theme, Some(ThemeName::from("dark")));
        assert_eq!(partial.editor.as_ref().unwrap().tab_size, Some(2));
    }

    // ============= Plugin Config Delta Saving Tests =============

    #[test]
    fn plugins_with_default_enabled_not_serialized() {
        // When all plugins have enabled=true (the default), plugins should be None
        let mut config = crate::config::Config::default();
        config.plugins.insert(
            "test_plugin".to_string(),
            PluginConfig {
                enabled: true, // Default value
                path: Some(std::path::PathBuf::from("/path/to/plugin.ts")),
            },
        );

        let partial = PartialConfig::from(&config);

        // plugins should be None since all have default values
        assert!(
            partial.plugins.is_none(),
            "Plugins with default enabled=true should not be serialized"
        );
    }

    #[test]
    fn plugins_with_disabled_are_serialized() {
        // When a plugin is disabled, it should be included in the partial config
        let mut config = crate::config::Config::default();
        config.plugins.insert(
            "enabled_plugin".to_string(),
            PluginConfig {
                enabled: true,
                path: Some(std::path::PathBuf::from("/path/to/enabled.ts")),
            },
        );
        config.plugins.insert(
            "disabled_plugin".to_string(),
            PluginConfig {
                enabled: false, // Not default!
                path: Some(std::path::PathBuf::from("/path/to/disabled.ts")),
            },
        );

        let partial = PartialConfig::from(&config);

        // plugins should contain only the disabled plugin
        assert!(partial.plugins.is_some());
        let plugins = partial.plugins.unwrap();
        assert_eq!(
            plugins.len(),
            1,
            "Only disabled plugins should be serialized"
        );
        assert!(plugins.contains_key("disabled_plugin"));
        assert!(!plugins.contains_key("enabled_plugin"));

        // Check the disabled plugin has correct values
        let disabled = plugins.get("disabled_plugin").unwrap();
        assert_eq!(disabled.enabled, Some(false));
        // Path should be None - it's auto-discovered and shouldn't be saved
        assert!(disabled.path.is_none(), "Path should not be serialized");
    }

    #[test]
    fn plugin_path_never_serialized() {
        // Even for disabled plugins, path should never be serialized
        let mut config = crate::config::Config::default();
        config.plugins.insert(
            "my_plugin".to_string(),
            PluginConfig {
                enabled: false,
                path: Some(std::path::PathBuf::from("/some/path/plugin.ts")),
            },
        );

        let partial = PartialConfig::from(&config);
        let plugins = partial.plugins.unwrap();
        let plugin = plugins.get("my_plugin").unwrap();

        assert!(
            plugin.path.is_none(),
            "Path is runtime-discovered and should never be serialized"
        );
    }

    #[test]
    fn resolving_partial_with_disabled_plugin_preserves_state() {
        // Loading a config with a disabled plugin should preserve disabled state
        let partial = PartialConfig {
            plugins: Some(HashMap::from([(
                "my_plugin".to_string(),
                PartialPluginConfig {
                    enabled: Some(false),
                    path: None,
                },
            )])),
            ..Default::default()
        };

        let resolved = partial.resolve();

        // Plugin should exist and be disabled
        let plugin = resolved.plugins.get("my_plugin");
        assert!(
            plugin.is_some(),
            "Disabled plugin should be in resolved config"
        );
        assert!(
            !plugin.unwrap().enabled,
            "Plugin should remain disabled after resolve"
        );
    }

    #[test]
    fn merge_plugins_preserves_higher_precedence_disabled_state() {
        // When merging, higher precedence disabled state should win
        let mut higher = PartialConfig {
            plugins: Some(HashMap::from([(
                "my_plugin".to_string(),
                PartialPluginConfig {
                    enabled: Some(false), // User disabled
                    path: None,
                },
            )])),
            ..Default::default()
        };

        let lower = PartialConfig {
            plugins: Some(HashMap::from([(
                "my_plugin".to_string(),
                PartialPluginConfig {
                    enabled: Some(true), // Lower layer has it enabled
                    path: None,
                },
            )])),
            ..Default::default()
        };

        higher.merge_from(&lower);

        let plugins = higher.plugins.unwrap();
        let plugin = plugins.get("my_plugin").unwrap();
        assert_eq!(
            plugin.enabled,
            Some(false),
            "Higher precedence disabled state should win"
        );
    }

    #[test]
    fn roundtrip_disabled_plugin_only_saves_delta() {
        // Roundtrip test: create config with mix of enabled/disabled plugins,
        // convert to partial, serialize to JSON, deserialize, and verify
        let mut config = crate::config::Config::default();
        config.plugins.insert(
            "plugin_a".to_string(),
            PluginConfig {
                enabled: true,
                path: Some(std::path::PathBuf::from("/a.ts")),
            },
        );
        config.plugins.insert(
            "plugin_b".to_string(),
            PluginConfig {
                enabled: false,
                path: Some(std::path::PathBuf::from("/b.ts")),
            },
        );
        config.plugins.insert(
            "plugin_c".to_string(),
            PluginConfig {
                enabled: true,
                path: Some(std::path::PathBuf::from("/c.ts")),
            },
        );

        // Convert to partial (delta)
        let partial = PartialConfig::from(&config);

        // Serialize to JSON
        let json = serde_json::to_string(&partial).unwrap();

        // Verify only plugin_b is in the JSON
        assert!(
            json.contains("plugin_b"),
            "Disabled plugin should be in serialized JSON"
        );
        assert!(
            !json.contains("plugin_a"),
            "Enabled plugin_a should not be in serialized JSON"
        );
        assert!(
            !json.contains("plugin_c"),
            "Enabled plugin_c should not be in serialized JSON"
        );

        // Deserialize back
        let deserialized: PartialConfig = serde_json::from_str(&json).unwrap();

        // Verify plugins section only contains the disabled one
        let plugins = deserialized.plugins.unwrap();
        assert_eq!(plugins.len(), 1);
        assert!(plugins.contains_key("plugin_b"));
        assert_eq!(plugins.get("plugin_b").unwrap().enabled, Some(false));
    }
}
