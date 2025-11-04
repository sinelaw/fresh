//! Command palette system for executing editor actions by name

use crate::keybindings::Action;

/// A command that can be executed from the command palette
#[derive(Debug, Clone)]
pub struct Command {
    /// Command name (e.g., "Open File")
    pub name: String,
    /// Command description
    pub description: String,
    /// The action to trigger
    pub action: Action,
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
}

impl Suggestion {
    pub fn new(text: String) -> Self {
        Self {
            text,
            description: None,
            value: None,
        }
    }

    pub fn with_description(text: String, description: String) -> Self {
        Self {
            text,
            description: Some(description),
            value: None,
        }
    }

    pub fn get_value(&self) -> &str {
        self.value.as_ref().unwrap_or(&self.text)
    }
}

/// Get all available commands for the command palette
pub fn get_all_commands() -> Vec<Command> {
    vec![
        Command {
            name: "Open File".to_string(),
            description: "Open a file in a new or existing buffer".to_string(),
            action: Action::Open,
        },
        Command {
            name: "Save File".to_string(),
            description: "Save the current buffer to disk".to_string(),
            action: Action::Save,
        },
        Command {
            name: "Quit".to_string(),
            description: "Exit the editor".to_string(),
            action: Action::Quit,
        },
        Command {
            name: "Show Help".to_string(),
            description: "Display the help page with all keybindings".to_string(),
            action: Action::ShowHelp,
        },
        Command {
            name: "Undo".to_string(),
            description: "Undo the last edit".to_string(),
            action: Action::Undo,
        },
        Command {
            name: "Redo".to_string(),
            description: "Redo the last undone edit".to_string(),
            action: Action::Redo,
        },
        Command {
            name: "Copy".to_string(),
            description: "Copy selection to clipboard".to_string(),
            action: Action::Copy,
        },
        Command {
            name: "Cut".to_string(),
            description: "Cut selection to clipboard".to_string(),
            action: Action::Cut,
        },
        Command {
            name: "Paste".to_string(),
            description: "Paste from clipboard".to_string(),
            action: Action::Paste,
        },
        Command {
            name: "Select All".to_string(),
            description: "Select all text in the buffer".to_string(),
            action: Action::SelectAll,
        },
        Command {
            name: "Select Word".to_string(),
            description: "Select the word under the cursor".to_string(),
            action: Action::SelectWord,
        },
        Command {
            name: "Select Line".to_string(),
            description: "Select the current line".to_string(),
            action: Action::SelectLine,
        },
        Command {
            name: "Expand Selection".to_string(),
            description: "Expand the current selection by one word".to_string(),
            action: Action::ExpandSelection,
        },
        Command {
            name: "Add Cursor Above".to_string(),
            description: "Add a cursor on the line above".to_string(),
            action: Action::AddCursorAbove,
        },
        Command {
            name: "Add Cursor Below".to_string(),
            description: "Add a cursor on the line below".to_string(),
            action: Action::AddCursorBelow,
        },
        Command {
            name: "Add Cursor at Next Match".to_string(),
            description: "Add a cursor at the next occurrence of the selection".to_string(),
            action: Action::AddCursorNextMatch,
        },
        Command {
            name: "Remove Secondary Cursors".to_string(),
            description: "Remove all cursors except the primary".to_string(),
            action: Action::RemoveSecondaryCursors,
        },
    ]
}

/// Filter commands by fuzzy matching the query
pub fn filter_commands(query: &str) -> Vec<Suggestion> {
    let query_lower = query.to_lowercase();
    let commands = get_all_commands();

    if query.is_empty() {
        // Show all commands when no filter
        return commands
            .into_iter()
            .map(|cmd| Suggestion::with_description(cmd.name.clone(), cmd.description))
            .collect();
    }

    // Simple fuzzy matching: check if all characters appear in order
    commands
        .into_iter()
        .filter(|cmd| {
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
        })
        .map(|cmd| Suggestion::with_description(cmd.name.clone(), cmd.description))
        .collect()
}
