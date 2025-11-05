//! Command palette system for executing editor actions by name

use crate::keybindings::{Action, KeyContext};

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
}

impl Suggestion {
    pub fn new(text: String) -> Self {
        Self {
            text,
            description: None,
            value: None,
            disabled: false,
        }
    }

    pub fn with_description(text: String, description: String) -> Self {
        Self {
            text,
            description: Some(description),
            value: None,
            disabled: false,
        }
    }

    pub fn with_description_and_disabled(text: String, description: String, disabled: bool) -> Self {
        Self {
            text,
            description: Some(description),
            value: None,
            disabled,
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
        },
        Command {
            name: "Save File".to_string(),
            description: "Save the current buffer to disk".to_string(),
            action: Action::Save,
            contexts: vec![KeyContext::Normal],
        },
        Command {
            name: "Save File As".to_string(),
            description: "Save the current buffer to a new file".to_string(),
            action: Action::SaveAs,
            contexts: vec![KeyContext::Normal],
        },
        Command {
            name: "New File".to_string(),
            description: "Create a new empty buffer".to_string(),
            action: Action::New,
            contexts: vec![],
        },
        Command {
            name: "Close Buffer".to_string(),
            description: "Close the current buffer".to_string(),
            action: Action::Close,
            contexts: vec![KeyContext::Normal],
        },
        Command {
            name: "Quit".to_string(),
            description: "Exit the editor".to_string(),
            action: Action::Quit,
            contexts: vec![],
        },

        // Edit operations
        Command {
            name: "Undo".to_string(),
            description: "Undo the last edit".to_string(),
            action: Action::Undo,
            contexts: vec![KeyContext::Normal],
        },
        Command {
            name: "Redo".to_string(),
            description: "Redo the last undone edit".to_string(),
            action: Action::Redo,
            contexts: vec![KeyContext::Normal],
        },
        Command {
            name: "Copy".to_string(),
            description: "Copy selection to clipboard".to_string(),
            action: Action::Copy,
            contexts: vec![KeyContext::Normal],
        },
        Command {
            name: "Cut".to_string(),
            description: "Cut selection to clipboard".to_string(),
            action: Action::Cut,
            contexts: vec![KeyContext::Normal],
        },
        Command {
            name: "Paste".to_string(),
            description: "Paste from clipboard".to_string(),
            action: Action::Paste,
            contexts: vec![KeyContext::Normal],
        },
        Command {
            name: "Delete Line".to_string(),
            description: "Delete the current line".to_string(),
            action: Action::DeleteLine,
            contexts: vec![KeyContext::Normal],
        },

        // Selection
        Command {
            name: "Select All".to_string(),
            description: "Select all text in the buffer".to_string(),
            action: Action::SelectAll,
            contexts: vec![KeyContext::Normal],
        },
        Command {
            name: "Select Word".to_string(),
            description: "Select the word under the cursor".to_string(),
            action: Action::SelectWord,
            contexts: vec![KeyContext::Normal],
        },
        Command {
            name: "Select Line".to_string(),
            description: "Select the current line".to_string(),
            action: Action::SelectLine,
            contexts: vec![KeyContext::Normal],
        },
        Command {
            name: "Expand Selection".to_string(),
            description: "Expand the current selection by one word".to_string(),
            action: Action::ExpandSelection,
            contexts: vec![KeyContext::Normal],
        },

        // Multi-cursor
        Command {
            name: "Add Cursor Above".to_string(),
            description: "Add a cursor on the line above".to_string(),
            action: Action::AddCursorAbove,
            contexts: vec![KeyContext::Normal],
        },
        Command {
            name: "Add Cursor Below".to_string(),
            description: "Add a cursor on the line below".to_string(),
            action: Action::AddCursorBelow,
            contexts: vec![KeyContext::Normal],
        },
        Command {
            name: "Add Cursor at Next Match".to_string(),
            description: "Add a cursor at the next occurrence of the selection".to_string(),
            action: Action::AddCursorNextMatch,
            contexts: vec![KeyContext::Normal],
        },
        Command {
            name: "Remove Secondary Cursors".to_string(),
            description: "Remove all cursors except the primary".to_string(),
            action: Action::RemoveSecondaryCursors,
            contexts: vec![KeyContext::Normal],
        },

        // Buffer navigation
        Command {
            name: "Next Buffer".to_string(),
            description: "Switch to the next buffer".to_string(),
            action: Action::NextBuffer,
            contexts: vec![KeyContext::Normal],
        },
        Command {
            name: "Previous Buffer".to_string(),
            description: "Switch to the previous buffer".to_string(),
            action: Action::PrevBuffer,
            contexts: vec![KeyContext::Normal],
        },

        // Split operations
        Command {
            name: "Split Horizontal".to_string(),
            description: "Split the current view horizontally".to_string(),
            action: Action::SplitHorizontal,
            contexts: vec![KeyContext::Normal],
        },
        Command {
            name: "Split Vertical".to_string(),
            description: "Split the current view vertically".to_string(),
            action: Action::SplitVertical,
            contexts: vec![KeyContext::Normal],
        },
        Command {
            name: "Close Split".to_string(),
            description: "Close the current split pane".to_string(),
            action: Action::CloseSplit,
            contexts: vec![KeyContext::Normal],
        },
        Command {
            name: "Next Split".to_string(),
            description: "Move focus to the next split pane".to_string(),
            action: Action::NextSplit,
            contexts: vec![KeyContext::Normal],
        },
        Command {
            name: "Previous Split".to_string(),
            description: "Move focus to the previous split pane".to_string(),
            action: Action::PrevSplit,
            contexts: vec![KeyContext::Normal],
        },
        Command {
            name: "Increase Split Size".to_string(),
            description: "Increase the size of the current split".to_string(),
            action: Action::IncreaseSplitSize,
            contexts: vec![KeyContext::Normal],
        },
        Command {
            name: "Decrease Split Size".to_string(),
            description: "Decrease the size of the current split".to_string(),
            action: Action::DecreaseSplitSize,
            contexts: vec![KeyContext::Normal],
        },

        // File explorer
        Command {
            name: "Toggle File Explorer".to_string(),
            description: "Show or hide the file explorer".to_string(),
            action: Action::ToggleFileExplorer,
            contexts: vec![KeyContext::Normal, KeyContext::FileExplorer],
        },
        Command {
            name: "Focus File Explorer".to_string(),
            description: "Move focus to the file explorer".to_string(),
            action: Action::FocusFileExplorer,
            contexts: vec![KeyContext::Normal],
        },
        Command {
            name: "Focus Editor".to_string(),
            description: "Move focus back to the editor".to_string(),
            action: Action::FocusEditor,
            contexts: vec![KeyContext::FileExplorer],
        },
        Command {
            name: "File Explorer: Refresh".to_string(),
            description: "Refresh the file explorer".to_string(),
            action: Action::FileExplorerRefresh,
            contexts: vec![KeyContext::FileExplorer],
        },
        Command {
            name: "File Explorer: New File".to_string(),
            description: "Create a new file in the current directory".to_string(),
            action: Action::FileExplorerNewFile,
            contexts: vec![KeyContext::FileExplorer],
        },
        Command {
            name: "File Explorer: New Directory".to_string(),
            description: "Create a new directory".to_string(),
            action: Action::FileExplorerNewDirectory,
            contexts: vec![KeyContext::FileExplorer],
        },
        Command {
            name: "File Explorer: Delete".to_string(),
            description: "Delete the selected file or directory".to_string(),
            action: Action::FileExplorerDelete,
            contexts: vec![KeyContext::FileExplorer],
        },
        Command {
            name: "File Explorer: Rename".to_string(),
            description: "Rename the selected file or directory".to_string(),
            action: Action::FileExplorerRename,
            contexts: vec![KeyContext::FileExplorer],
        },

        // View
        Command {
            name: "Show Help".to_string(),
            description: "Display the help page with all keybindings".to_string(),
            action: Action::ShowHelp,
            contexts: vec![],
        },
        Command {
            name: "Command Palette".to_string(),
            description: "Open the command palette".to_string(),
            action: Action::CommandPalette,
            contexts: vec![KeyContext::Normal],
        },

        // Git operations
        Command {
            name: "Git: Grep".to_string(),
            description: "Search through git-tracked files with live results".to_string(),
            action: Action::GitGrep,
            contexts: vec![KeyContext::Normal],
        },
        Command {
            name: "Git: Find File".to_string(),
            description: "Find and open a file from git ls-files".to_string(),
            action: Action::GitFindFile,
            contexts: vec![KeyContext::Normal],
        },
    ]
}

/// Filter commands by fuzzy matching the query, with context awareness
pub fn filter_commands(query: &str, current_context: KeyContext) -> Vec<Suggestion> {
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
            Suggestion::with_description_and_disabled(
                cmd.name.clone(),
                cmd.description,
                !available,
            )
        })
        .collect();

    // Sort: available commands first, then disabled ones
    suggestions.sort_by_key(|s| s.disabled);

    suggestions
}
