//! Command palette system for executing editor actions by name

use crate::input::keybindings::{Action, KeyContext};
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

/// Get all available commands for the command palette
pub fn get_all_commands() -> Vec<Command> {
    vec![
        // File operations
        Command {
            name: t!("cmd.open_file").to_string(),
            description: t!("cmd.open_file_desc").to_string(),
            action: Action::Open,
            contexts: vec![],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.switch_project").to_string(),
            description: t!("cmd.switch_project_desc").to_string(),
            action: Action::SwitchProject,
            contexts: vec![],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.save_file").to_string(),
            description: t!("cmd.save_file_desc").to_string(),
            action: Action::Save,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.save_file_as").to_string(),
            description: t!("cmd.save_file_as_desc").to_string(),
            action: Action::SaveAs,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.new_file").to_string(),
            description: t!("cmd.new_file_desc").to_string(),
            action: Action::New,
            contexts: vec![],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.close_buffer").to_string(),
            description: t!("cmd.close_buffer_desc").to_string(),
            action: Action::Close,
            contexts: vec![KeyContext::Normal, KeyContext::Terminal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.close_tab").to_string(),
            description: t!("cmd.close_tab_desc").to_string(),
            action: Action::CloseTab,
            contexts: vec![KeyContext::Normal, KeyContext::Terminal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.revert_file").to_string(),
            description: t!("cmd.revert_file_desc").to_string(),
            action: Action::Revert,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.toggle_auto_revert").to_string(),
            description: t!("cmd.toggle_auto_revert_desc").to_string(),
            action: Action::ToggleAutoRevert,
            contexts: vec![],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.format_buffer").to_string(),
            description: t!("cmd.format_buffer_desc").to_string(),
            action: Action::FormatBuffer,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.quit").to_string(),
            description: t!("cmd.quit_desc").to_string(),
            action: Action::Quit,
            contexts: vec![],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // Edit operations
        Command {
            name: t!("cmd.undo").to_string(),
            description: t!("cmd.undo_desc").to_string(),
            action: Action::Undo,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.redo").to_string(),
            description: t!("cmd.redo_desc").to_string(),
            action: Action::Redo,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.copy").to_string(),
            description: t!("cmd.copy_desc").to_string(),
            action: Action::Copy,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.copy_with_formatting").to_string(),
            description: t!("cmd.copy_with_formatting_desc").to_string(),
            action: Action::CopyWithTheme(String::new()),
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.cut").to_string(),
            description: t!("cmd.cut_desc").to_string(),
            action: Action::Cut,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.paste").to_string(),
            description: t!("cmd.paste_desc").to_string(),
            action: Action::Paste,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.delete_line").to_string(),
            description: t!("cmd.delete_line_desc").to_string(),
            action: Action::DeleteLine,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.delete_word_backward").to_string(),
            description: t!("cmd.delete_word_backward_desc").to_string(),
            action: Action::DeleteWordBackward,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.delete_word_forward").to_string(),
            description: t!("cmd.delete_word_forward_desc").to_string(),
            action: Action::DeleteWordForward,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.delete_to_end_of_line").to_string(),
            description: t!("cmd.delete_to_end_of_line_desc").to_string(),
            action: Action::DeleteToLineEnd,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.transpose_characters").to_string(),
            description: t!("cmd.transpose_characters_desc").to_string(),
            action: Action::TransposeChars,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.transform_uppercase").to_string(),
            description: t!("cmd.transform_uppercase_desc").to_string(),
            action: Action::ToUpperCase,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.transform_lowercase").to_string(),
            description: t!("cmd.transform_lowercase_desc").to_string(),
            action: Action::ToLowerCase,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.open_line").to_string(),
            description: t!("cmd.open_line_desc").to_string(),
            action: Action::OpenLine,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.recenter").to_string(),
            description: t!("cmd.recenter_desc").to_string(),
            action: Action::Recenter,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.set_mark").to_string(),
            description: t!("cmd.set_mark_desc").to_string(),
            action: Action::SetMark,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // Selection
        Command {
            name: t!("cmd.select_all").to_string(),
            description: t!("cmd.select_all_desc").to_string(),
            action: Action::SelectAll,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.select_word").to_string(),
            description: t!("cmd.select_word_desc").to_string(),
            action: Action::SelectWord,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.select_line").to_string(),
            description: t!("cmd.select_line_desc").to_string(),
            action: Action::SelectLine,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.expand_selection").to_string(),
            description: t!("cmd.expand_selection_desc").to_string(),
            action: Action::ExpandSelection,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // Multi-cursor
        Command {
            name: t!("cmd.add_cursor_above").to_string(),
            description: t!("cmd.add_cursor_above_desc").to_string(),
            action: Action::AddCursorAbove,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.add_cursor_below").to_string(),
            description: t!("cmd.add_cursor_below_desc").to_string(),
            action: Action::AddCursorBelow,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.add_cursor_next_match").to_string(),
            description: t!("cmd.add_cursor_next_match_desc").to_string(),
            action: Action::AddCursorNextMatch,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.remove_secondary_cursors").to_string(),
            description: t!("cmd.remove_secondary_cursors_desc").to_string(),
            action: Action::RemoveSecondaryCursors,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // Buffer navigation
        Command {
            name: t!("cmd.next_buffer").to_string(),
            description: t!("cmd.next_buffer_desc").to_string(),
            action: Action::NextBuffer,
            contexts: vec![KeyContext::Normal, KeyContext::Terminal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.previous_buffer").to_string(),
            description: t!("cmd.previous_buffer_desc").to_string(),
            action: Action::PrevBuffer,
            contexts: vec![KeyContext::Normal, KeyContext::Terminal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.switch_to_previous_tab").to_string(),
            description: t!("cmd.switch_to_previous_tab_desc").to_string(),
            action: Action::SwitchToPreviousTab,
            contexts: vec![KeyContext::Normal, KeyContext::Terminal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.switch_to_tab_by_name").to_string(),
            description: t!("cmd.switch_to_tab_by_name_desc").to_string(),
            action: Action::SwitchToTabByName,
            contexts: vec![KeyContext::Normal, KeyContext::Terminal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // Split operations
        Command {
            name: t!("cmd.split_horizontal").to_string(),
            description: t!("cmd.split_horizontal_desc").to_string(),
            action: Action::SplitHorizontal,
            contexts: vec![KeyContext::Normal, KeyContext::Terminal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.split_vertical").to_string(),
            description: t!("cmd.split_vertical_desc").to_string(),
            action: Action::SplitVertical,
            contexts: vec![KeyContext::Normal, KeyContext::Terminal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.close_split").to_string(),
            description: t!("cmd.close_split_desc").to_string(),
            action: Action::CloseSplit,
            contexts: vec![KeyContext::Normal, KeyContext::Terminal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.next_split").to_string(),
            description: t!("cmd.next_split_desc").to_string(),
            action: Action::NextSplit,
            contexts: vec![KeyContext::Normal, KeyContext::Terminal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.previous_split").to_string(),
            description: t!("cmd.previous_split_desc").to_string(),
            action: Action::PrevSplit,
            contexts: vec![KeyContext::Normal, KeyContext::Terminal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.increase_split_size").to_string(),
            description: t!("cmd.increase_split_size_desc").to_string(),
            action: Action::IncreaseSplitSize,
            contexts: vec![KeyContext::Normal, KeyContext::Terminal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.decrease_split_size").to_string(),
            description: t!("cmd.decrease_split_size_desc").to_string(),
            action: Action::DecreaseSplitSize,
            contexts: vec![KeyContext::Normal, KeyContext::Terminal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.toggle_maximize_split").to_string(),
            description: t!("cmd.toggle_maximize_split_desc").to_string(),
            action: Action::ToggleMaximizeSplit,
            contexts: vec![KeyContext::Normal, KeyContext::Terminal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // View toggles
        Command {
            name: t!("cmd.toggle_line_numbers").to_string(),
            description: t!("cmd.toggle_line_numbers_desc").to_string(),
            action: Action::ToggleLineNumbers,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.debug_toggle_highlight").to_string(),
            description: t!("cmd.debug_toggle_highlight_desc").to_string(),
            action: Action::ToggleDebugHighlights,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // Buffer settings commands
        Command {
            name: t!("cmd.set_tab_size").to_string(),
            description: t!("cmd.set_tab_size_desc").to_string(),
            action: Action::SetTabSize,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.set_line_ending").to_string(),
            description: t!("cmd.set_line_ending_desc").to_string(),
            action: Action::SetLineEnding,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.toggle_indentation").to_string(),
            description: t!("cmd.toggle_indentation_desc").to_string(),
            action: Action::ToggleIndentationStyle,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.toggle_tab_indicators").to_string(),
            description: t!("cmd.toggle_tab_indicators_desc").to_string(),
            action: Action::ToggleTabIndicators,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.reset_buffer_settings").to_string(),
            description: t!("cmd.reset_buffer_settings_desc").to_string(),
            action: Action::ResetBufferSettings,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.scroll_up").to_string(),
            description: t!("cmd.scroll_up_desc").to_string(),
            action: Action::ScrollUp,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.scroll_down").to_string(),
            description: t!("cmd.scroll_down_desc").to_string(),
            action: Action::ScrollDown,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.scroll_tabs_left").to_string(),
            description: t!("cmd.scroll_tabs_left_desc").to_string(),
            action: Action::ScrollTabsLeft,
            contexts: vec![KeyContext::Normal, KeyContext::Terminal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.scroll_tabs_right").to_string(),
            description: t!("cmd.scroll_tabs_right_desc").to_string(),
            action: Action::ScrollTabsRight,
            contexts: vec![KeyContext::Normal, KeyContext::Terminal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.toggle_mouse_support").to_string(),
            description: t!("cmd.toggle_mouse_support_desc").to_string(),
            action: Action::ToggleMouseCapture,
            contexts: vec![KeyContext::Normal, KeyContext::Terminal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // File explorer
        Command {
            name: t!("cmd.toggle_file_explorer").to_string(),
            description: t!("cmd.toggle_file_explorer_desc").to_string(),
            action: Action::ToggleFileExplorer,
            contexts: vec![
                KeyContext::Normal,
                KeyContext::FileExplorer,
                KeyContext::Terminal,
            ],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.toggle_menu_bar").to_string(),
            description: t!("cmd.toggle_menu_bar_desc").to_string(),
            action: Action::ToggleMenuBar,
            contexts: vec![
                KeyContext::Normal,
                KeyContext::FileExplorer,
                KeyContext::Terminal,
            ],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.toggle_tab_bar").to_string(),
            description: t!("cmd.toggle_tab_bar_desc").to_string(),
            action: Action::ToggleTabBar,
            contexts: vec![
                KeyContext::Normal,
                KeyContext::FileExplorer,
                KeyContext::Terminal,
            ],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.focus_file_explorer").to_string(),
            description: t!("cmd.focus_file_explorer_desc").to_string(),
            action: Action::FocusFileExplorer,
            contexts: vec![KeyContext::Normal, KeyContext::Terminal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.focus_editor").to_string(),
            description: t!("cmd.focus_editor_desc").to_string(),
            action: Action::FocusEditor,
            contexts: vec![KeyContext::FileExplorer],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.explorer_refresh").to_string(),
            description: t!("cmd.explorer_refresh_desc").to_string(),
            action: Action::FileExplorerRefresh,
            contexts: vec![KeyContext::FileExplorer],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.explorer_new_file").to_string(),
            description: t!("cmd.explorer_new_file_desc").to_string(),
            action: Action::FileExplorerNewFile,
            contexts: vec![KeyContext::FileExplorer],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.explorer_new_directory").to_string(),
            description: t!("cmd.explorer_new_directory_desc").to_string(),
            action: Action::FileExplorerNewDirectory,
            contexts: vec![KeyContext::FileExplorer],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.explorer_delete").to_string(),
            description: t!("cmd.explorer_delete_desc").to_string(),
            action: Action::FileExplorerDelete,
            contexts: vec![KeyContext::FileExplorer],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.explorer_rename").to_string(),
            description: t!("cmd.explorer_rename_desc").to_string(),
            action: Action::FileExplorerRename,
            contexts: vec![KeyContext::FileExplorer],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.toggle_hidden_files").to_string(),
            description: t!("cmd.toggle_hidden_files_desc").to_string(),
            action: Action::FileExplorerToggleHidden,
            contexts: vec![KeyContext::FileExplorer],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.toggle_gitignored_files").to_string(),
            description: t!("cmd.toggle_gitignored_files_desc").to_string(),
            action: Action::FileExplorerToggleGitignored,
            contexts: vec![KeyContext::FileExplorer],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // View
        Command {
            name: t!("cmd.toggle_line_wrap").to_string(),
            description: t!("cmd.toggle_line_wrap_desc").to_string(),
            action: Action::ToggleLineWrap,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // Note: Compose mode commands removed - markdown_compose plugin provides these
        Command {
            name: t!("cmd.set_background").to_string(),
            description: t!("cmd.set_background_desc").to_string(),
            action: Action::SetBackground,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.set_background_blend").to_string(),
            description: t!("cmd.set_background_blend_desc").to_string(),
            action: Action::SetBackgroundBlend,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // Note: Command Palette is intentionally not in the command list
        // to avoid confusion when it's already open (use Ctrl+P or Ctrl+/ to toggle)
        // Search and replace
        Command {
            name: t!("cmd.search").to_string(),
            description: t!("cmd.search_desc").to_string(),
            action: Action::Search,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.find_in_selection").to_string(),
            description: t!("cmd.find_in_selection_desc").to_string(),
            action: Action::FindInSelection,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.find_next").to_string(),
            description: t!("cmd.find_next_desc").to_string(),
            action: Action::FindNext,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.find_previous").to_string(),
            description: t!("cmd.find_previous_desc").to_string(),
            action: Action::FindPrevious,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.find_selection_next").to_string(),
            description: t!("cmd.find_selection_next_desc").to_string(),
            action: Action::FindSelectionNext,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.find_selection_previous").to_string(),
            description: t!("cmd.find_selection_previous_desc").to_string(),
            action: Action::FindSelectionPrevious,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.replace").to_string(),
            description: t!("cmd.replace_desc").to_string(),
            action: Action::Replace,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.query_replace").to_string(),
            description: t!("cmd.query_replace_desc").to_string(),
            action: Action::QueryReplace,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // Navigation
        Command {
            name: t!("cmd.goto_line").to_string(),
            description: t!("cmd.goto_line_desc").to_string(),
            action: Action::GotoLine,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.smart_home").to_string(),
            description: t!("cmd.smart_home_desc").to_string(),
            action: Action::SmartHome,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.show_completions").to_string(),
            description: t!("cmd.show_completions_desc").to_string(),
            action: Action::LspCompletion,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.goto_definition").to_string(),
            description: t!("cmd.goto_definition_desc").to_string(),
            action: Action::LspGotoDefinition,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.show_hover_info").to_string(),
            description: t!("cmd.show_hover_info_desc").to_string(),
            action: Action::LspHover,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.find_references").to_string(),
            description: t!("cmd.find_references_desc").to_string(),
            action: Action::LspReferences,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.show_signature_help").to_string(),
            description: t!("cmd.show_signature_help_desc").to_string(),
            action: Action::LspSignatureHelp,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.code_actions").to_string(),
            description: t!("cmd.code_actions_desc").to_string(),
            action: Action::LspCodeActions,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.start_restart_lsp").to_string(),
            description: t!("cmd.start_restart_lsp_desc").to_string(),
            action: Action::LspRestart,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.stop_lsp").to_string(),
            description: t!("cmd.stop_lsp_desc").to_string(),
            action: Action::LspStop,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.toggle_mouse_hover").to_string(),
            description: t!("cmd.toggle_mouse_hover_desc").to_string(),
            action: Action::ToggleMouseHover,
            contexts: vec![],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.navigate_back").to_string(),
            description: t!("cmd.navigate_back_desc").to_string(),
            action: Action::NavigateBack,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.navigate_forward").to_string(),
            description: t!("cmd.navigate_forward_desc").to_string(),
            action: Action::NavigateForward,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // Smart editing
        Command {
            name: t!("cmd.toggle_comment").to_string(),
            description: t!("cmd.toggle_comment_desc").to_string(),
            action: Action::ToggleComment,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.dedent_selection").to_string(),
            description: t!("cmd.dedent_selection_desc").to_string(),
            action: Action::DedentSelection,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.goto_matching_bracket").to_string(),
            description: t!("cmd.goto_matching_bracket_desc").to_string(),
            action: Action::GoToMatchingBracket,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // Error navigation
        Command {
            name: t!("cmd.jump_to_next_error").to_string(),
            description: t!("cmd.jump_to_next_error_desc").to_string(),
            action: Action::JumpToNextError,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.jump_to_previous_error").to_string(),
            description: t!("cmd.jump_to_previous_error_desc").to_string(),
            action: Action::JumpToPreviousError,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // LSP
        Command {
            name: t!("cmd.rename_symbol").to_string(),
            description: t!("cmd.rename_symbol_desc").to_string(),
            action: Action::LspRename,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // Bookmarks and Macros
        Command {
            name: t!("cmd.list_bookmarks").to_string(),
            description: t!("cmd.list_bookmarks_desc").to_string(),
            action: Action::ListBookmarks,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.list_macros").to_string(),
            description: t!("cmd.list_macros_desc").to_string(),
            action: Action::ListMacros,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.record_macro").to_string(),
            description: t!("cmd.record_macro_desc").to_string(),
            action: Action::PromptRecordMacro,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.stop_recording_macro").to_string(),
            description: t!("cmd.stop_recording_macro_desc").to_string(),
            action: Action::StopMacroRecording,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.play_macro").to_string(),
            description: t!("cmd.play_macro_desc").to_string(),
            action: Action::PromptPlayMacro,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.play_last_macro").to_string(),
            description: t!("cmd.play_last_macro_desc").to_string(),
            action: Action::PlayLastMacro,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.set_bookmark").to_string(),
            description: t!("cmd.set_bookmark_desc").to_string(),
            action: Action::PromptSetBookmark,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.jump_to_bookmark").to_string(),
            description: t!("cmd.jump_to_bookmark_desc").to_string(),
            action: Action::PromptJumpToBookmark,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // Help
        Command {
            name: t!("cmd.show_manual").to_string(),
            description: t!("cmd.show_manual_desc").to_string(),
            action: Action::ShowHelp,
            contexts: vec![],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.show_keyboard_shortcuts").to_string(),
            description: t!("cmd.show_keyboard_shortcuts_desc").to_string(),
            action: Action::ShowKeyboardShortcuts,
            contexts: vec![],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.show_warnings").to_string(),
            description: t!("cmd.show_warnings_desc").to_string(),
            action: Action::ShowWarnings,
            contexts: vec![],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.show_lsp_status").to_string(),
            description: t!("cmd.show_lsp_status_desc").to_string(),
            action: Action::ShowLspStatus,
            contexts: vec![],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.clear_warnings").to_string(),
            description: t!("cmd.clear_warnings_desc").to_string(),
            action: Action::ClearWarnings,
            contexts: vec![],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // Config
        Command {
            name: t!("cmd.dump_config").to_string(),
            description: t!("cmd.dump_config_desc").to_string(),
            action: Action::DumpConfig,
            contexts: vec![],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.toggle_inlay_hints").to_string(),
            description: t!("cmd.toggle_inlay_hints_desc").to_string(),
            action: Action::ToggleInlayHints,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // Theme selection
        Command {
            name: t!("cmd.select_theme").to_string(),
            description: t!("cmd.select_theme_desc").to_string(),
            action: Action::SelectTheme,
            contexts: vec![],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // Keybinding map selection
        Command {
            name: t!("cmd.select_keybinding_map").to_string(),
            description: t!("cmd.select_keybinding_map_desc").to_string(),
            action: Action::SelectKeybindingMap,
            contexts: vec![],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // Cursor style selection
        Command {
            name: t!("cmd.select_cursor_style").to_string(),
            description: t!("cmd.select_cursor_style_desc").to_string(),
            action: Action::SelectCursorStyle,
            contexts: vec![],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // Locale selection
        Command {
            name: t!("cmd.select_locale").to_string(),
            description: t!("cmd.select_locale_desc").to_string(),
            action: Action::SelectLocale,
            contexts: vec![],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // Settings
        Command {
            name: t!("cmd.open_settings").to_string(),
            description: t!("cmd.open_settings_desc").to_string(),
            action: Action::OpenSettings,
            contexts: vec![],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // Input calibration
        Command {
            name: t!("cmd.calibrate_input").to_string(),
            description: t!("cmd.calibrate_input_desc").to_string(),
            action: Action::CalibrateInput,
            contexts: vec![],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // Terminal commands
        Command {
            name: t!("cmd.open_terminal").to_string(),
            description: t!("cmd.open_terminal_desc").to_string(),
            action: Action::OpenTerminal,
            contexts: vec![], // Available in all contexts (file explorer, normal, terminal, etc.)
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.focus_terminal").to_string(),
            description: t!("cmd.focus_terminal_desc").to_string(),
            action: Action::FocusTerminal,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.exit_terminal_mode").to_string(),
            description: t!("cmd.exit_terminal_mode_desc").to_string(),
            action: Action::TerminalEscape,
            contexts: vec![KeyContext::Terminal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.toggle_keyboard_capture").to_string(),
            description: t!("cmd.toggle_keyboard_capture_desc").to_string(),
            action: Action::ToggleKeyboardCapture,
            contexts: vec![KeyContext::Terminal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // Shell command operations
        Command {
            name: t!("cmd.shell_command").to_string(),
            description: t!("cmd.shell_command_desc").to_string(),
            action: Action::ShellCommand,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: t!("cmd.shell_command_replace").to_string(),
            description: t!("cmd.shell_command_replace_desc").to_string(),
            action: Action::ShellCommandReplace,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
    ]
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
