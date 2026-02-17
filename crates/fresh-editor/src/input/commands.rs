//! Command palette system for executing editor actions by name

use crate::input::keybindings::{Action, KeyContext};
use crate::types::context_keys;
use rust_i18n::t;

/// Source of a command (builtin or from a plugin)
#[derive(Debug, Clone, PartialEq)]
pub enum CommandSource {
    /// Built-in editor command
    Builtin,
    /// Command registered by a plugin (contains plugin filename without extension)
    Plugin(String),
}

/// A command that can be executed from the command palette
#[derive(Debug, Clone)]
pub struct Command {
    /// Command name (e.g., "Open File")
    pub name: String,
    /// Command description
    pub description: String,
    /// The action to trigger
    pub action: Action,
    /// Contexts where this command is available (empty = available in all contexts)
    pub contexts: Vec<KeyContext>,
    /// Custom contexts required for this command (plugin-defined contexts like "config-editor")
    /// If non-empty, all custom contexts must be active for the command to be available
    pub custom_contexts: Vec<String>,
    /// Source of the command (builtin or plugin)
    pub source: CommandSource,
}

impl Command {
    /// Get the localized name of the command
    pub fn get_localized_name(&self) -> String {
        if self.name.starts_with('%') {
            if let CommandSource::Plugin(ref plugin_name) = self.source {
                return crate::i18n::translate_plugin_string(
                    plugin_name,
                    &self.name[1..],
                    &std::collections::HashMap::new(),
                );
            }
        }
        self.name.clone()
    }

    /// Get the localized description of the command
    pub fn get_localized_description(&self) -> String {
        if self.description.starts_with('%') {
            if let CommandSource::Plugin(ref plugin_name) = self.source {
                return crate::i18n::translate_plugin_string(
                    plugin_name,
                    &self.description[1..],
                    &std::collections::HashMap::new(),
                );
            }
        }
        self.description.clone()
    }
}

/// A single suggestion item for autocomplete
#[derive(Debug, Clone, PartialEq)]
pub struct Suggestion {
    /// The text to display
    pub text: String,
    /// Optional description
    pub description: Option<String>,
    /// The value to use when selected (defaults to text if None)
    pub value: Option<String>,
    /// Whether this suggestion is disabled (greyed out)
    pub disabled: bool,
    /// Optional keyboard shortcut
    pub keybinding: Option<String>,
    /// Source of the command (for command palette)
    pub source: Option<CommandSource>,
}

impl Suggestion {
    pub fn new(text: String) -> Self {
        Self {
            text,
            description: None,
            value: None,
            disabled: false,
            keybinding: None,
            source: None,
        }
    }

    pub fn with_description(text: String, description: String) -> Self {
        Self {
            text,
            description: Some(description),
            value: None,
            disabled: false,
            keybinding: None,
            source: None,
        }
    }

    pub fn with_description_and_disabled(
        text: String,
        description: String,
        disabled: bool,
    ) -> Self {
        Self {
            text,
            description: Some(description),
            value: None,
            disabled,
            keybinding: None,
            source: None,
        }
    }

    pub fn with_all(
        text: String,
        description: Option<String>,
        disabled: bool,
        keybinding: Option<String>,
    ) -> Self {
        Self {
            text,
            description,
            value: None,
            disabled,
            keybinding,
            source: None,
        }
    }

    pub fn with_source(
        text: String,
        description: Option<String>,
        disabled: bool,
        keybinding: Option<String>,
        source: Option<CommandSource>,
    ) -> Self {
        Self {
            text,
            description,
            value: None,
            disabled,
            keybinding,
            source,
        }
    }

    pub fn get_value(&self) -> &str {
        self.value.as_ref().unwrap_or(&self.text)
    }
}

/// Static definition of a builtin command (all data except translated strings)
struct CommandDef {
    name_key: &'static str,
    desc_key: &'static str,
    action: fn() -> Action,
    contexts: &'static [KeyContext],
    custom_contexts: &'static [&'static str],
}

use KeyContext::{FileExplorer, Normal, Terminal};

/// All builtin command definitions as static data.
/// Translation happens at runtime via the loop in get_all_commands().
static COMMAND_DEFS: &[CommandDef] = &[
    // File operations
    CommandDef {
        name_key: "cmd.open_file",
        desc_key: "cmd.open_file_desc",
        action: || Action::Open,
        contexts: &[],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.switch_project",
        desc_key: "cmd.switch_project_desc",
        action: || Action::SwitchProject,
        contexts: &[],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.save_file",
        desc_key: "cmd.save_file_desc",
        action: || Action::Save,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.save_file_as",
        desc_key: "cmd.save_file_as_desc",
        action: || Action::SaveAs,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.new_file",
        desc_key: "cmd.new_file_desc",
        action: || Action::New,
        contexts: &[],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.close_buffer",
        desc_key: "cmd.close_buffer_desc",
        action: || Action::Close,
        contexts: &[Normal, Terminal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.close_tab",
        desc_key: "cmd.close_tab_desc",
        action: || Action::CloseTab,
        contexts: &[Normal, Terminal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.revert_file",
        desc_key: "cmd.revert_file_desc",
        action: || Action::Revert,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.toggle_auto_revert",
        desc_key: "cmd.toggle_auto_revert_desc",
        action: || Action::ToggleAutoRevert,
        contexts: &[],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.format_buffer",
        desc_key: "cmd.format_buffer_desc",
        action: || Action::FormatBuffer,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.trim_trailing_whitespace",
        desc_key: "cmd.trim_trailing_whitespace_desc",
        action: || Action::TrimTrailingWhitespace,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.ensure_final_newline",
        desc_key: "cmd.ensure_final_newline_desc",
        action: || Action::EnsureFinalNewline,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.quit",
        desc_key: "cmd.quit_desc",
        action: || Action::Quit,
        contexts: &[],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.detach",
        desc_key: "cmd.detach_desc",
        action: || Action::Detach,
        contexts: &[],
        custom_contexts: &[context_keys::SESSION_MODE],
    },
    // Edit operations
    CommandDef {
        name_key: "cmd.undo",
        desc_key: "cmd.undo_desc",
        action: || Action::Undo,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.redo",
        desc_key: "cmd.redo_desc",
        action: || Action::Redo,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.copy",
        desc_key: "cmd.copy_desc",
        action: || Action::Copy,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.copy_with_formatting",
        desc_key: "cmd.copy_with_formatting_desc",
        action: || Action::CopyWithTheme(String::new()),
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.cut",
        desc_key: "cmd.cut_desc",
        action: || Action::Cut,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.paste",
        desc_key: "cmd.paste_desc",
        action: || Action::Paste,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.delete_line",
        desc_key: "cmd.delete_line_desc",
        action: || Action::DeleteLine,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.delete_word_backward",
        desc_key: "cmd.delete_word_backward_desc",
        action: || Action::DeleteWordBackward,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.delete_word_forward",
        desc_key: "cmd.delete_word_forward_desc",
        action: || Action::DeleteWordForward,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.delete_to_end_of_line",
        desc_key: "cmd.delete_to_end_of_line_desc",
        action: || Action::DeleteToLineEnd,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.transpose_characters",
        desc_key: "cmd.transpose_characters_desc",
        action: || Action::TransposeChars,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.transform_uppercase",
        desc_key: "cmd.transform_uppercase_desc",
        action: || Action::ToUpperCase,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.transform_lowercase",
        desc_key: "cmd.transform_lowercase_desc",
        action: || Action::ToLowerCase,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.sort_lines",
        desc_key: "cmd.sort_lines_desc",
        action: || Action::SortLines,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.open_line",
        desc_key: "cmd.open_line_desc",
        action: || Action::OpenLine,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.duplicate_line",
        desc_key: "cmd.duplicate_line_desc",
        action: || Action::DuplicateLine,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.recenter",
        desc_key: "cmd.recenter_desc",
        action: || Action::Recenter,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.set_mark",
        desc_key: "cmd.set_mark_desc",
        action: || Action::SetMark,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    // Selection
    CommandDef {
        name_key: "cmd.select_all",
        desc_key: "cmd.select_all_desc",
        action: || Action::SelectAll,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.select_word",
        desc_key: "cmd.select_word_desc",
        action: || Action::SelectWord,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.select_line",
        desc_key: "cmd.select_line_desc",
        action: || Action::SelectLine,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.expand_selection",
        desc_key: "cmd.expand_selection_desc",
        action: || Action::ExpandSelection,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    // Multi-cursor
    CommandDef {
        name_key: "cmd.add_cursor_above",
        desc_key: "cmd.add_cursor_above_desc",
        action: || Action::AddCursorAbove,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.add_cursor_below",
        desc_key: "cmd.add_cursor_below_desc",
        action: || Action::AddCursorBelow,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.add_cursor_next_match",
        desc_key: "cmd.add_cursor_next_match_desc",
        action: || Action::AddCursorNextMatch,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.remove_secondary_cursors",
        desc_key: "cmd.remove_secondary_cursors_desc",
        action: || Action::RemoveSecondaryCursors,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    // Buffer navigation
    CommandDef {
        name_key: "cmd.next_buffer",
        desc_key: "cmd.next_buffer_desc",
        action: || Action::NextBuffer,
        contexts: &[Normal, Terminal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.previous_buffer",
        desc_key: "cmd.previous_buffer_desc",
        action: || Action::PrevBuffer,
        contexts: &[Normal, Terminal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.switch_to_previous_tab",
        desc_key: "cmd.switch_to_previous_tab_desc",
        action: || Action::SwitchToPreviousTab,
        contexts: &[Normal, Terminal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.switch_to_tab_by_name",
        desc_key: "cmd.switch_to_tab_by_name_desc",
        action: || Action::SwitchToTabByName,
        contexts: &[Normal, Terminal],
        custom_contexts: &[],
    },
    // Split operations
    CommandDef {
        name_key: "cmd.split_horizontal",
        desc_key: "cmd.split_horizontal_desc",
        action: || Action::SplitHorizontal,
        contexts: &[Normal, Terminal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.split_vertical",
        desc_key: "cmd.split_vertical_desc",
        action: || Action::SplitVertical,
        contexts: &[Normal, Terminal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.close_split",
        desc_key: "cmd.close_split_desc",
        action: || Action::CloseSplit,
        contexts: &[Normal, Terminal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.next_split",
        desc_key: "cmd.next_split_desc",
        action: || Action::NextSplit,
        contexts: &[Normal, Terminal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.previous_split",
        desc_key: "cmd.previous_split_desc",
        action: || Action::PrevSplit,
        contexts: &[Normal, Terminal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.increase_split_size",
        desc_key: "cmd.increase_split_size_desc",
        action: || Action::IncreaseSplitSize,
        contexts: &[Normal, Terminal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.decrease_split_size",
        desc_key: "cmd.decrease_split_size_desc",
        action: || Action::DecreaseSplitSize,
        contexts: &[Normal, Terminal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.toggle_maximize_split",
        desc_key: "cmd.toggle_maximize_split_desc",
        action: || Action::ToggleMaximizeSplit,
        contexts: &[Normal, Terminal],
        custom_contexts: &[],
    },
    // View toggles
    CommandDef {
        name_key: "cmd.toggle_line_numbers",
        desc_key: "cmd.toggle_line_numbers_desc",
        action: || Action::ToggleLineNumbers,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.toggle_scroll_sync",
        desc_key: "cmd.toggle_scroll_sync_desc",
        action: || Action::ToggleScrollSync,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.debug_toggle_highlight",
        desc_key: "cmd.debug_toggle_highlight_desc",
        action: || Action::ToggleDebugHighlights,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    // Rulers
    CommandDef {
        name_key: "cmd.add_ruler",
        desc_key: "cmd.add_ruler_desc",
        action: || Action::AddRuler,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.remove_ruler",
        desc_key: "cmd.remove_ruler_desc",
        action: || Action::RemoveRuler,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    // Buffer settings
    CommandDef {
        name_key: "cmd.set_tab_size",
        desc_key: "cmd.set_tab_size_desc",
        action: || Action::SetTabSize,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.set_line_ending",
        desc_key: "cmd.set_line_ending_desc",
        action: || Action::SetLineEnding,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.set_encoding",
        desc_key: "cmd.set_encoding_desc",
        action: || Action::SetEncoding,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.reload_with_encoding",
        desc_key: "cmd.reload_with_encoding_desc",
        action: || Action::ReloadWithEncoding,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.set_language",
        desc_key: "cmd.set_language_desc",
        action: || Action::SetLanguage,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.toggle_indentation",
        desc_key: "cmd.toggle_indentation_desc",
        action: || Action::ToggleIndentationStyle,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.toggle_tab_indicators",
        desc_key: "cmd.toggle_tab_indicators_desc",
        action: || Action::ToggleTabIndicators,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.reset_buffer_settings",
        desc_key: "cmd.reset_buffer_settings_desc",
        action: || Action::ResetBufferSettings,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.scroll_up",
        desc_key: "cmd.scroll_up_desc",
        action: || Action::ScrollUp,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.scroll_down",
        desc_key: "cmd.scroll_down_desc",
        action: || Action::ScrollDown,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.scroll_tabs_left",
        desc_key: "cmd.scroll_tabs_left_desc",
        action: || Action::ScrollTabsLeft,
        contexts: &[Normal, Terminal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.scroll_tabs_right",
        desc_key: "cmd.scroll_tabs_right_desc",
        action: || Action::ScrollTabsRight,
        contexts: &[Normal, Terminal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.toggle_mouse_support",
        desc_key: "cmd.toggle_mouse_support_desc",
        action: || Action::ToggleMouseCapture,
        contexts: &[Normal, Terminal],
        custom_contexts: &[],
    },
    // File explorer
    CommandDef {
        name_key: "cmd.toggle_file_explorer",
        desc_key: "cmd.toggle_file_explorer_desc",
        action: || Action::ToggleFileExplorer,
        contexts: &[Normal, FileExplorer, Terminal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.toggle_menu_bar",
        desc_key: "cmd.toggle_menu_bar_desc",
        action: || Action::ToggleMenuBar,
        contexts: &[Normal, FileExplorer, Terminal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.toggle_tab_bar",
        desc_key: "cmd.toggle_tab_bar_desc",
        action: || Action::ToggleTabBar,
        contexts: &[Normal, FileExplorer, Terminal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.toggle_vertical_scrollbar",
        desc_key: "cmd.toggle_vertical_scrollbar_desc",
        action: || Action::ToggleVerticalScrollbar,
        contexts: &[Normal, FileExplorer, Terminal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.toggle_horizontal_scrollbar",
        desc_key: "cmd.toggle_horizontal_scrollbar_desc",
        action: || Action::ToggleHorizontalScrollbar,
        contexts: &[Normal, FileExplorer, Terminal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.focus_file_explorer",
        desc_key: "cmd.focus_file_explorer_desc",
        action: || Action::FocusFileExplorer,
        contexts: &[Normal, Terminal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.focus_editor",
        desc_key: "cmd.focus_editor_desc",
        action: || Action::FocusEditor,
        contexts: &[FileExplorer],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.explorer_refresh",
        desc_key: "cmd.explorer_refresh_desc",
        action: || Action::FileExplorerRefresh,
        contexts: &[FileExplorer],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.explorer_new_file",
        desc_key: "cmd.explorer_new_file_desc",
        action: || Action::FileExplorerNewFile,
        contexts: &[FileExplorer],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.explorer_new_directory",
        desc_key: "cmd.explorer_new_directory_desc",
        action: || Action::FileExplorerNewDirectory,
        contexts: &[FileExplorer],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.explorer_delete",
        desc_key: "cmd.explorer_delete_desc",
        action: || Action::FileExplorerDelete,
        contexts: &[FileExplorer],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.explorer_rename",
        desc_key: "cmd.explorer_rename_desc",
        action: || Action::FileExplorerRename,
        contexts: &[FileExplorer],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.toggle_hidden_files",
        desc_key: "cmd.toggle_hidden_files_desc",
        action: || Action::FileExplorerToggleHidden,
        contexts: &[FileExplorer],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.toggle_gitignored_files",
        desc_key: "cmd.toggle_gitignored_files_desc",
        action: || Action::FileExplorerToggleGitignored,
        contexts: &[FileExplorer],
        custom_contexts: &[],
    },
    // View
    CommandDef {
        name_key: "cmd.toggle_line_wrap",
        desc_key: "cmd.toggle_line_wrap_desc",
        action: || Action::ToggleLineWrap,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.set_background",
        desc_key: "cmd.set_background_desc",
        action: || Action::SetBackground,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.set_background_blend",
        desc_key: "cmd.set_background_blend_desc",
        action: || Action::SetBackgroundBlend,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    // Search and replace
    CommandDef {
        name_key: "cmd.search",
        desc_key: "cmd.search_desc",
        action: || Action::Search,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.find_in_selection",
        desc_key: "cmd.find_in_selection_desc",
        action: || Action::FindInSelection,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.find_next",
        desc_key: "cmd.find_next_desc",
        action: || Action::FindNext,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.find_previous",
        desc_key: "cmd.find_previous_desc",
        action: || Action::FindPrevious,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.find_selection_next",
        desc_key: "cmd.find_selection_next_desc",
        action: || Action::FindSelectionNext,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.find_selection_previous",
        desc_key: "cmd.find_selection_previous_desc",
        action: || Action::FindSelectionPrevious,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.replace",
        desc_key: "cmd.replace_desc",
        action: || Action::Replace,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.query_replace",
        desc_key: "cmd.query_replace_desc",
        action: || Action::QueryReplace,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    // Navigation
    CommandDef {
        name_key: "cmd.goto_line",
        desc_key: "cmd.goto_line_desc",
        action: || Action::GotoLine,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.smart_home",
        desc_key: "cmd.smart_home_desc",
        action: || Action::SmartHome,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.show_completions",
        desc_key: "cmd.show_completions_desc",
        action: || Action::LspCompletion,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.goto_definition",
        desc_key: "cmd.goto_definition_desc",
        action: || Action::LspGotoDefinition,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.show_hover_info",
        desc_key: "cmd.show_hover_info_desc",
        action: || Action::LspHover,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.find_references",
        desc_key: "cmd.find_references_desc",
        action: || Action::LspReferences,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.show_signature_help",
        desc_key: "cmd.show_signature_help_desc",
        action: || Action::LspSignatureHelp,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.code_actions",
        desc_key: "cmd.code_actions_desc",
        action: || Action::LspCodeActions,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.start_restart_lsp",
        desc_key: "cmd.start_restart_lsp_desc",
        action: || Action::LspRestart,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.stop_lsp",
        desc_key: "cmd.stop_lsp_desc",
        action: || Action::LspStop,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.toggle_lsp_for_buffer",
        desc_key: "cmd.toggle_lsp_for_buffer_desc",
        action: || Action::LspToggleForBuffer,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.toggle_mouse_hover",
        desc_key: "cmd.toggle_mouse_hover_desc",
        action: || Action::ToggleMouseHover,
        contexts: &[],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.navigate_back",
        desc_key: "cmd.navigate_back_desc",
        action: || Action::NavigateBack,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.navigate_forward",
        desc_key: "cmd.navigate_forward_desc",
        action: || Action::NavigateForward,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    // Smart editing
    CommandDef {
        name_key: "cmd.toggle_comment",
        desc_key: "cmd.toggle_comment_desc",
        action: || Action::ToggleComment,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.dedent_selection",
        desc_key: "cmd.dedent_selection_desc",
        action: || Action::DedentSelection,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.goto_matching_bracket",
        desc_key: "cmd.goto_matching_bracket_desc",
        action: || Action::GoToMatchingBracket,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    // Error navigation
    CommandDef {
        name_key: "cmd.jump_to_next_error",
        desc_key: "cmd.jump_to_next_error_desc",
        action: || Action::JumpToNextError,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.jump_to_previous_error",
        desc_key: "cmd.jump_to_previous_error_desc",
        action: || Action::JumpToPreviousError,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    // LSP
    CommandDef {
        name_key: "cmd.rename_symbol",
        desc_key: "cmd.rename_symbol_desc",
        action: || Action::LspRename,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    // Bookmarks and Macros
    CommandDef {
        name_key: "cmd.list_bookmarks",
        desc_key: "cmd.list_bookmarks_desc",
        action: || Action::ListBookmarks,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.list_macros",
        desc_key: "cmd.list_macros_desc",
        action: || Action::ListMacros,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.record_macro",
        desc_key: "cmd.record_macro_desc",
        action: || Action::PromptRecordMacro,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.stop_recording_macro",
        desc_key: "cmd.stop_recording_macro_desc",
        action: || Action::StopMacroRecording,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.play_macro",
        desc_key: "cmd.play_macro_desc",
        action: || Action::PromptPlayMacro,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.play_last_macro",
        desc_key: "cmd.play_last_macro_desc",
        action: || Action::PlayLastMacro,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.set_bookmark",
        desc_key: "cmd.set_bookmark_desc",
        action: || Action::PromptSetBookmark,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.jump_to_bookmark",
        desc_key: "cmd.jump_to_bookmark_desc",
        action: || Action::PromptJumpToBookmark,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    // Help
    CommandDef {
        name_key: "cmd.show_manual",
        desc_key: "cmd.show_manual_desc",
        action: || Action::ShowHelp,
        contexts: &[],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.show_keyboard_shortcuts",
        desc_key: "cmd.show_keyboard_shortcuts_desc",
        action: || Action::ShowKeyboardShortcuts,
        contexts: &[],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.show_warnings",
        desc_key: "cmd.show_warnings_desc",
        action: || Action::ShowWarnings,
        contexts: &[],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.show_lsp_status",
        desc_key: "cmd.show_lsp_status_desc",
        action: || Action::ShowLspStatus,
        contexts: &[],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.clear_warnings",
        desc_key: "cmd.clear_warnings_desc",
        action: || Action::ClearWarnings,
        contexts: &[],
        custom_contexts: &[],
    },
    // Config
    CommandDef {
        name_key: "cmd.dump_config",
        desc_key: "cmd.dump_config_desc",
        action: || Action::DumpConfig,
        contexts: &[],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.toggle_inlay_hints",
        desc_key: "cmd.toggle_inlay_hints_desc",
        action: || Action::ToggleInlayHints,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    // Theme selection
    CommandDef {
        name_key: "cmd.select_theme",
        desc_key: "cmd.select_theme_desc",
        action: || Action::SelectTheme,
        contexts: &[],
        custom_contexts: &[],
    },
    // Keybinding map selection
    CommandDef {
        name_key: "cmd.select_keybinding_map",
        desc_key: "cmd.select_keybinding_map_desc",
        action: || Action::SelectKeybindingMap,
        contexts: &[],
        custom_contexts: &[],
    },
    // Cursor style selection
    CommandDef {
        name_key: "cmd.select_cursor_style",
        desc_key: "cmd.select_cursor_style_desc",
        action: || Action::SelectCursorStyle,
        contexts: &[],
        custom_contexts: &[],
    },
    // Locale selection
    CommandDef {
        name_key: "cmd.select_locale",
        desc_key: "cmd.select_locale_desc",
        action: || Action::SelectLocale,
        contexts: &[],
        custom_contexts: &[],
    },
    // Settings
    CommandDef {
        name_key: "cmd.open_settings",
        desc_key: "cmd.open_settings_desc",
        action: || Action::OpenSettings,
        contexts: &[],
        custom_contexts: &[],
    },
    // Keybinding editor
    CommandDef {
        name_key: "cmd.open_keybinding_editor",
        desc_key: "cmd.open_keybinding_editor_desc",
        action: || Action::OpenKeybindingEditor,
        contexts: &[],
        custom_contexts: &[],
    },
    // Input calibration
    CommandDef {
        name_key: "cmd.calibrate_input",
        desc_key: "cmd.calibrate_input_desc",
        action: || Action::CalibrateInput,
        contexts: &[],
        custom_contexts: &[],
    },
    // Terminal commands
    CommandDef {
        name_key: "cmd.open_terminal",
        desc_key: "cmd.open_terminal_desc",
        action: || Action::OpenTerminal,
        contexts: &[],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.focus_terminal",
        desc_key: "cmd.focus_terminal_desc",
        action: || Action::FocusTerminal,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.exit_terminal_mode",
        desc_key: "cmd.exit_terminal_mode_desc",
        action: || Action::TerminalEscape,
        contexts: &[Terminal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.toggle_keyboard_capture",
        desc_key: "cmd.toggle_keyboard_capture_desc",
        action: || Action::ToggleKeyboardCapture,
        contexts: &[Terminal],
        custom_contexts: &[],
    },
    // Shell command operations
    CommandDef {
        name_key: "cmd.shell_command",
        desc_key: "cmd.shell_command_desc",
        action: || Action::ShellCommand,
        contexts: &[Normal],
        custom_contexts: &[],
    },
    CommandDef {
        name_key: "cmd.shell_command_replace",
        desc_key: "cmd.shell_command_replace_desc",
        action: || Action::ShellCommandReplace,
        contexts: &[Normal],
        custom_contexts: &[],
    },
];

/// Get all available commands for the command palette
pub fn get_all_commands() -> Vec<Command> {
    COMMAND_DEFS
        .iter()
        .map(|def| Command {
            name: t!(def.name_key).to_string(),
            description: t!(def.desc_key).to_string(),
            action: (def.action)(),
            contexts: def.contexts.to_vec(),
            custom_contexts: def.custom_contexts.iter().map(|s| s.to_string()).collect(),
            source: CommandSource::Builtin,
        })
        .collect()
}

/// Filter commands by fuzzy matching the query, with context awareness
pub fn filter_commands(
    query: &str,
    current_context: KeyContext,
    keybinding_resolver: &crate::input::keybindings::KeybindingResolver,
) -> Vec<Suggestion> {
    let query_lower = query.to_lowercase();
    let commands = get_all_commands();

    // Helper function to check if command is available in current context
    let is_available = |cmd: &Command| -> bool {
        // Empty contexts means available in all contexts
        cmd.contexts.is_empty() || cmd.contexts.contains(&current_context)
    };

    // Helper function for fuzzy matching
    let matches_query = |cmd: &Command| -> bool {
        if query.is_empty() {
            return true;
        }

        let name_lower = cmd.name.to_lowercase();
        let mut query_chars = query_lower.chars();
        let mut current_char = query_chars.next();

        for name_char in name_lower.chars() {
            if let Some(qc) = current_char {
                if qc == name_char {
                    current_char = query_chars.next();
                }
            } else {
                break;
            }
        }

        current_char.is_none() // All query characters matched
    };

    // Filter and convert to suggestions
    let mut suggestions: Vec<Suggestion> = commands
        .into_iter()
        .filter(|cmd| matches_query(cmd))
        .map(|cmd| {
            let available = is_available(&cmd);
            let keybinding =
                keybinding_resolver.get_keybinding_for_action(&cmd.action, current_context);
            Suggestion::with_all(
                cmd.name.clone(),
                Some(cmd.description),
                !available,
                keybinding,
            )
        })
        .collect();

    // Sort: available commands first, then disabled ones
    suggestions.sort_by_key(|s| s.disabled);

    suggestions
}
