//! Command palette system for executing editor actions by name

use crate::input::keybindings::{Action, KeyContext};

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
            name: "Open File".to_string(),
            description: "Open a file in a new or existing buffer".to_string(),
            action: Action::Open,
            contexts: vec![],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Save File".to_string(),
            description: "Save the current buffer to disk".to_string(),
            action: Action::Save,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Save File As".to_string(),
            description: "Save the current buffer to a new file".to_string(),
            action: Action::SaveAs,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "New File".to_string(),
            description: "Create a new empty buffer".to_string(),
            action: Action::New,
            contexts: vec![],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Close Buffer".to_string(),
            description: "Close the current buffer".to_string(),
            action: Action::Close,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Close Tab".to_string(),
            description: "Close the current tab in the current split".to_string(),
            action: Action::CloseTab,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Revert File".to_string(),
            description: "Discard changes and reload from disk".to_string(),
            action: Action::Revert,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Toggle Auto-Revert".to_string(),
            description: "Toggle automatic reloading when files change on disk".to_string(),
            action: Action::ToggleAutoRevert,
            contexts: vec![],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Quit".to_string(),
            description: "Exit the editor".to_string(),
            action: Action::Quit,
            contexts: vec![],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // Edit operations
        Command {
            name: "Undo".to_string(),
            description: "Undo the last edit".to_string(),
            action: Action::Undo,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Redo".to_string(),
            description: "Redo the last undone edit".to_string(),
            action: Action::Redo,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Copy".to_string(),
            description: "Copy selection to clipboard".to_string(),
            action: Action::Copy,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Cut".to_string(),
            description: "Cut selection to clipboard".to_string(),
            action: Action::Cut,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Paste".to_string(),
            description: "Paste from clipboard".to_string(),
            action: Action::Paste,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Delete Line".to_string(),
            description: "Delete the current line".to_string(),
            action: Action::DeleteLine,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Delete Word Backward".to_string(),
            description: "Delete the word before the cursor".to_string(),
            action: Action::DeleteWordBackward,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Delete Word Forward".to_string(),
            description: "Delete the word after the cursor".to_string(),
            action: Action::DeleteWordForward,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Delete to End of Line".to_string(),
            description: "Delete from cursor to the end of the line".to_string(),
            action: Action::DeleteToLineEnd,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Transpose Characters".to_string(),
            description: "Swap the character before cursor with the one at cursor".to_string(),
            action: Action::TransposeChars,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Open Line".to_string(),
            description: "Insert newline at cursor without moving cursor".to_string(),
            action: Action::OpenLine,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Recenter".to_string(),
            description: "Center the view on the cursor".to_string(),
            action: Action::Recenter,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Set Mark".to_string(),
            description: "Set selection anchor to start a selection".to_string(),
            action: Action::SetMark,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // Selection
        Command {
            name: "Select All".to_string(),
            description: "Select all text in the buffer".to_string(),
            action: Action::SelectAll,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Select Word".to_string(),
            description: "Select the word under the cursor".to_string(),
            action: Action::SelectWord,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Select Line".to_string(),
            description: "Select the current line".to_string(),
            action: Action::SelectLine,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Expand Selection".to_string(),
            description: "Expand the current selection by one word".to_string(),
            action: Action::ExpandSelection,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // Multi-cursor
        Command {
            name: "Add Cursor Above".to_string(),
            description: "Add a cursor on the line above".to_string(),
            action: Action::AddCursorAbove,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Add Cursor Below".to_string(),
            description: "Add a cursor on the line below".to_string(),
            action: Action::AddCursorBelow,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Add Cursor at Next Match".to_string(),
            description: "Add a cursor at the next occurrence of the selection".to_string(),
            action: Action::AddCursorNextMatch,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Remove Secondary Cursors".to_string(),
            description: "Remove all cursors except the primary".to_string(),
            action: Action::RemoveSecondaryCursors,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // Buffer navigation
        Command {
            name: "Next Buffer".to_string(),
            description: "Switch to the next buffer".to_string(),
            action: Action::NextBuffer,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Previous Buffer".to_string(),
            description: "Switch to the previous buffer".to_string(),
            action: Action::PrevBuffer,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Switch to Previous Tab".to_string(),
            description: "Switch to the most recently used tab".to_string(),
            action: Action::SwitchToPreviousTab,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Switch to Tab by Name".to_string(),
            description: "Switch to a tab by selecting from a list".to_string(),
            action: Action::SwitchToTabByName,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // Split operations
        Command {
            name: "Split Horizontal".to_string(),
            description: "Split the current view horizontally".to_string(),
            action: Action::SplitHorizontal,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Split Vertical".to_string(),
            description: "Split the current view vertically".to_string(),
            action: Action::SplitVertical,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Close Split".to_string(),
            description: "Close the current split pane".to_string(),
            action: Action::CloseSplit,
            contexts: vec![KeyContext::Normal, KeyContext::Terminal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Next Split".to_string(),
            description: "Move focus to the next split pane".to_string(),
            action: Action::NextSplit,
            contexts: vec![KeyContext::Normal, KeyContext::Terminal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Previous Split".to_string(),
            description: "Move focus to the previous split pane".to_string(),
            action: Action::PrevSplit,
            contexts: vec![KeyContext::Normal, KeyContext::Terminal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Increase Split Size".to_string(),
            description: "Increase the size of the current split".to_string(),
            action: Action::IncreaseSplitSize,
            contexts: vec![KeyContext::Normal, KeyContext::Terminal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Decrease Split Size".to_string(),
            description: "Decrease the size of the current split".to_string(),
            action: Action::DecreaseSplitSize,
            contexts: vec![KeyContext::Normal, KeyContext::Terminal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Toggle Maximize Split".to_string(),
            description: "Maximize or restore the current split".to_string(),
            action: Action::ToggleMaximizeSplit,
            contexts: vec![KeyContext::Normal, KeyContext::Terminal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // View toggles
        Command {
            name: "Toggle Line Numbers".to_string(),
            description: "Show or hide line numbers in the gutter".to_string(),
            action: Action::ToggleLineNumbers,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Scroll Up".to_string(),
            description: "Scroll the view up without moving cursor".to_string(),
            action: Action::ScrollUp,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Scroll Down".to_string(),
            description: "Scroll the view down without moving cursor".to_string(),
            action: Action::ScrollDown,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Scroll Tabs Left".to_string(),
            description: "Scroll the tab bar to show tabs on the left".to_string(),
            action: Action::ScrollTabsLeft,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Scroll Tabs Right".to_string(),
            description: "Scroll the tab bar to show tabs on the right".to_string(),
            action: Action::ScrollTabsRight,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Toggle Mouse Support".to_string(),
            description: "Enable or disable mouse capture".to_string(),
            action: Action::ToggleMouseCapture,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // File explorer
        Command {
            name: "Toggle File Explorer".to_string(),
            description: "Show or hide the file explorer".to_string(),
            action: Action::ToggleFileExplorer,
            contexts: vec![KeyContext::Normal, KeyContext::FileExplorer],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Focus File Explorer".to_string(),
            description: "Move focus to the file explorer".to_string(),
            action: Action::FocusFileExplorer,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Focus Editor".to_string(),
            description: "Move focus back to the editor".to_string(),
            action: Action::FocusEditor,
            contexts: vec![KeyContext::FileExplorer],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "File Explorer: Refresh".to_string(),
            description: "Refresh the file explorer".to_string(),
            action: Action::FileExplorerRefresh,
            contexts: vec![KeyContext::FileExplorer],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "File Explorer: New File".to_string(),
            description: "Create a new file in the current directory".to_string(),
            action: Action::FileExplorerNewFile,
            contexts: vec![KeyContext::FileExplorer],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "File Explorer: New Directory".to_string(),
            description: "Create a new directory".to_string(),
            action: Action::FileExplorerNewDirectory,
            contexts: vec![KeyContext::FileExplorer],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "File Explorer: Delete".to_string(),
            description: "Delete the selected file or directory".to_string(),
            action: Action::FileExplorerDelete,
            contexts: vec![KeyContext::FileExplorer],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "File Explorer: Rename".to_string(),
            description: "Rename the selected file or directory".to_string(),
            action: Action::FileExplorerRename,
            contexts: vec![KeyContext::FileExplorer],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Toggle Hidden Files".to_string(),
            description: "Show or hide hidden files in the file explorer".to_string(),
            action: Action::FileExplorerToggleHidden,
            contexts: vec![KeyContext::FileExplorer],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Toggle Gitignored Files".to_string(),
            description: "Show or hide gitignored files in the file explorer".to_string(),
            action: Action::FileExplorerToggleGitignored,
            contexts: vec![KeyContext::FileExplorer],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // View
        Command {
            name: "Toggle Line Wrap".to_string(),
            description: "Enable or disable line wrapping in the editor".to_string(),
            action: Action::ToggleLineWrap,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // Note: Compose mode commands removed - markdown_compose plugin provides these
        Command {
            name: "Set Background".to_string(),
            description: "Choose an ANSI art file to use as a faded background".to_string(),
            action: Action::SetBackground,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Set Background Blend".to_string(),
            description: "Adjust how strongly the background shows through (0-1)".to_string(),
            action: Action::SetBackgroundBlend,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // Note: Command Palette is intentionally not in the command list
        // to avoid confusion when it's already open (use Ctrl+P or Ctrl+/ to toggle)
        // Search and replace
        Command {
            name: "Search".to_string(),
            description: "Search for text in the current buffer".to_string(),
            action: Action::Search,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Find in Selection".to_string(),
            description: "Search only within the current selection".to_string(),
            action: Action::FindInSelection,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Find Next".to_string(),
            description: "Jump to the next search match".to_string(),
            action: Action::FindNext,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Find Previous".to_string(),
            description: "Jump to the previous search match".to_string(),
            action: Action::FindPrevious,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Replace".to_string(),
            description: "Replace text in the current buffer".to_string(),
            action: Action::Replace,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Query Replace".to_string(),
            description: "Interactive replace with y/n/!/q prompts for each match".to_string(),
            action: Action::QueryReplace,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // Navigation
        Command {
            name: "Go to Line".to_string(),
            description: "Jump to a specific line number".to_string(),
            action: Action::GotoLine,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Smart Home".to_string(),
            description: "Move to first non-whitespace character, or line start if already there"
                .to_string(),
            action: Action::SmartHome,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Show Completions".to_string(),
            description: "Trigger autocomplete suggestions at cursor".to_string(),
            action: Action::LspCompletion,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Go to Definition".to_string(),
            description: "Jump to the definition of the symbol under cursor".to_string(),
            action: Action::LspGotoDefinition,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Show Hover Info".to_string(),
            description: "Show documentation for the symbol under cursor".to_string(),
            action: Action::LspHover,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Find References".to_string(),
            description: "Find all references to the symbol under cursor".to_string(),
            action: Action::LspReferences,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Show Signature Help".to_string(),
            description: "Show function parameter hints".to_string(),
            action: Action::LspSignatureHelp,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Code Actions".to_string(),
            description: "Show available code actions (quick fixes, refactorings)".to_string(),
            action: Action::LspCodeActions,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Start/Restart LSP Server".to_string(),
            description: "Start or restart the LSP server for the current language".to_string(),
            action: Action::LspRestart,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Stop LSP Server".to_string(),
            description: "Stop a running LSP server (select from list)".to_string(),
            action: Action::LspStop,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Toggle Mouse Hover".to_string(),
            description: "Toggle LSP hover info on mouse hover".to_string(),
            action: Action::ToggleMouseHover,
            contexts: vec![],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Navigate Back".to_string(),
            description: "Go back in navigation history".to_string(),
            action: Action::NavigateBack,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Navigate Forward".to_string(),
            description: "Go forward in navigation history".to_string(),
            action: Action::NavigateForward,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // Smart editing
        Command {
            name: "Toggle Comment".to_string(),
            description: "Comment or uncomment the current line or selection".to_string(),
            action: Action::ToggleComment,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Indent Selection".to_string(),
            description: "Increase indentation of selected lines".to_string(),
            action: Action::IndentSelection,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Dedent Selection".to_string(),
            description: "Decrease indentation of selected lines".to_string(),
            action: Action::DedentSelection,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Go to Matching Bracket".to_string(),
            description: "Jump to the matching bracket, parenthesis, or brace".to_string(),
            action: Action::GoToMatchingBracket,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // Error navigation
        Command {
            name: "Jump to Next Error".to_string(),
            description: "Navigate to the next diagnostic error or warning".to_string(),
            action: Action::JumpToNextError,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Jump to Previous Error".to_string(),
            description: "Navigate to the previous diagnostic error or warning".to_string(),
            action: Action::JumpToPreviousError,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // LSP
        Command {
            name: "Rename Symbol".to_string(),
            description: "Rename the symbol under cursor across the project".to_string(),
            action: Action::LspRename,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // Bookmarks and Macros
        Command {
            name: "List Bookmarks".to_string(),
            description: "Show all defined bookmarks".to_string(),
            action: Action::ListBookmarks,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "List Macros".to_string(),
            description: "Show all recorded macros".to_string(),
            action: Action::ListMacros,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Record Macro".to_string(),
            description: "Toggle macro recording for a register (0-9)".to_string(),
            action: Action::PromptRecordMacro,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Stop Recording Macro".to_string(),
            description: "Stop the current macro recording".to_string(),
            action: Action::StopMacroRecording,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Play Macro".to_string(),
            description: "Play macro from a register (0-9)".to_string(),
            action: Action::PromptPlayMacro,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Play Last Macro".to_string(),
            description: "Play the last recorded macro (F12)".to_string(),
            action: Action::PlayLastMacro,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Set Bookmark".to_string(),
            description: "Set a bookmark at current position (0-9)".to_string(),
            action: Action::PromptSetBookmark,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Jump to Bookmark".to_string(),
            description: "Jump to a bookmark (0-9)".to_string(),
            action: Action::PromptJumpToBookmark,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // Help
        Command {
            name: "Show Manual".to_string(),
            description: "Open the help manual".to_string(),
            action: Action::ShowHelp,
            contexts: vec![],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Show Keyboard Shortcuts".to_string(),
            description: "Display all keyboard shortcuts".to_string(),
            action: Action::ShowKeyboardShortcuts,
            contexts: vec![],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // Config
        Command {
            name: "Dump Config".to_string(),
            description: "Save the current configuration to the user config file".to_string(),
            action: Action::DumpConfig,
            contexts: vec![],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Toggle Inlay Hints".to_string(),
            description: "Show or hide LSP inlay hints (type hints, parameter hints)".to_string(),
            action: Action::ToggleInlayHints,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // Theme selection
        Command {
            name: "Select Theme".to_string(),
            description: "Choose a color theme for the editor".to_string(),
            action: Action::SelectTheme,
            contexts: vec![],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // Keybinding map switching
        Command {
            name: "Switch to Default Keybindings".to_string(),
            description: "Switch to the default keybinding map".to_string(),
            action: Action::SwitchKeybindingMap("default".to_string()),
            contexts: vec![],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Switch to Emacs Keybindings".to_string(),
            description: "Switch to Emacs-style keybindings".to_string(),
            action: Action::SwitchKeybindingMap("emacs".to_string()),
            contexts: vec![],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Switch to VSCode Keybindings".to_string(),
            description: "Switch to VSCode-style keybindings".to_string(),
            action: Action::SwitchKeybindingMap("vscode".to_string()),
            contexts: vec![],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        // Terminal commands
        Command {
            name: "Open Terminal".to_string(),
            description: "Open a new terminal in the current split".to_string(),
            action: Action::OpenTerminal,
            contexts: vec![KeyContext::Normal, KeyContext::Terminal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Focus Terminal".to_string(),
            description: "Switch to terminal input mode".to_string(),
            action: Action::FocusTerminal,
            contexts: vec![KeyContext::Normal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Exit Terminal Mode".to_string(),
            description: "Exit terminal input mode and return to editor".to_string(),
            action: Action::TerminalEscape,
            contexts: vec![KeyContext::Terminal],
            custom_contexts: vec![],
            source: CommandSource::Builtin,
        },
        Command {
            name: "Toggle Keyboard Capture".to_string(),
            description: "Lock all keyboard input to terminal (bypass editor shortcuts)".to_string(),
            action: Action::ToggleKeyboardCapture,
            contexts: vec![KeyContext::Terminal],
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
