use crate::config::Config;
use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use std::collections::HashMap;

/// High-level actions that can be performed in the editor
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Action {
    // Character input
    InsertChar(char),
    InsertNewline,
    InsertTab,

    // Basic movement
    MoveLeft,
    MoveRight,
    MoveUp,
    MoveDown,
    MoveWordLeft,
    MoveWordRight,
    MoveLineStart,
    MoveLineEnd,
    MovePageUp,
    MovePageDown,
    MoveDocumentStart,
    MoveDocumentEnd,

    // Selection movement (extends selection while moving)
    SelectLeft,
    SelectRight,
    SelectUp,
    SelectDown,
    SelectWordLeft,
    SelectWordRight,
    SelectLineStart,
    SelectLineEnd,
    SelectAll,

    // Editing
    DeleteBackward,
    DeleteForward,
    DeleteWordBackward,
    DeleteWordForward,
    DeleteLine,

    // Clipboard
    Copy,
    Cut,
    Paste,

    // Multi-cursor
    AddCursorAbove,
    AddCursorBelow,
    AddCursorNextMatch,
    RemoveSecondaryCursors,

    // File operations
    Save,
    SaveAs,
    Open,
    New,
    Close,
    Quit,

    // Undo/redo
    Undo,
    Redo,

    // View
    ScrollUp,
    ScrollDown,

    // No-op
    None,
}

impl Action {
    /// Parse action from string (used when loading from config)
    pub fn from_str(s: &str, args: &HashMap<String, serde_json::Value>) -> Option<Action> {
        match s {
            "insert_char" => {
                if let Some(serde_json::Value::String(c)) = args.get("char") {
                    c.chars().next().map(Action::InsertChar)
                } else {
                    None
                }
            }
            "insert_newline" => Some(Action::InsertNewline),
            "insert_tab" => Some(Action::InsertTab),

            "move_left" => Some(Action::MoveLeft),
            "move_right" => Some(Action::MoveRight),
            "move_up" => Some(Action::MoveUp),
            "move_down" => Some(Action::MoveDown),
            "move_word_left" => Some(Action::MoveWordLeft),
            "move_word_right" => Some(Action::MoveWordRight),
            "move_line_start" => Some(Action::MoveLineStart),
            "move_line_end" => Some(Action::MoveLineEnd),
            "move_page_up" => Some(Action::MovePageUp),
            "move_page_down" => Some(Action::MovePageDown),
            "move_document_start" => Some(Action::MoveDocumentStart),
            "move_document_end" => Some(Action::MoveDocumentEnd),

            "select_left" => Some(Action::SelectLeft),
            "select_right" => Some(Action::SelectRight),
            "select_up" => Some(Action::SelectUp),
            "select_down" => Some(Action::SelectDown),
            "select_word_left" => Some(Action::SelectWordLeft),
            "select_word_right" => Some(Action::SelectWordRight),
            "select_line_start" => Some(Action::SelectLineStart),
            "select_line_end" => Some(Action::SelectLineEnd),
            "select_all" => Some(Action::SelectAll),

            "delete_backward" => Some(Action::DeleteBackward),
            "delete_forward" => Some(Action::DeleteForward),
            "delete_word_backward" => Some(Action::DeleteWordBackward),
            "delete_word_forward" => Some(Action::DeleteWordForward),
            "delete_line" => Some(Action::DeleteLine),

            "copy" => Some(Action::Copy),
            "cut" => Some(Action::Cut),
            "paste" => Some(Action::Paste),

            "add_cursor_above" => Some(Action::AddCursorAbove),
            "add_cursor_below" => Some(Action::AddCursorBelow),
            "add_cursor_next_match" => Some(Action::AddCursorNextMatch),
            "remove_secondary_cursors" => Some(Action::RemoveSecondaryCursors),

            "save" => Some(Action::Save),
            "save_as" => Some(Action::SaveAs),
            "open" => Some(Action::Open),
            "new" => Some(Action::New),
            "close" => Some(Action::Close),
            "quit" => Some(Action::Quit),

            "undo" => Some(Action::Undo),
            "redo" => Some(Action::Redo),

            "scroll_up" => Some(Action::ScrollUp),
            "scroll_down" => Some(Action::ScrollDown),

            _ => None,
        }
    }
}

/// Resolves key events to actions based on configuration
pub struct KeybindingResolver {
    /// Map from (key, modifiers) to action
    bindings: HashMap<(KeyCode, KeyModifiers), Action>,

    /// Default bindings (used as fallback)
    default_bindings: HashMap<(KeyCode, KeyModifiers), Action>,
}

impl KeybindingResolver {
    /// Create a new resolver from configuration
    pub fn new(config: &Config) -> Self {
        let mut resolver = Self {
            bindings: HashMap::new(),
            default_bindings: Self::create_default_bindings(),
        };

        // Load bindings from config
        for binding in &config.keybindings {
            if let Some(key_code) = Self::parse_key(&binding.key) {
                let modifiers = Self::parse_modifiers(&binding.modifiers);
                if let Some(action) = Action::from_str(&binding.action, &binding.args) {
                    resolver.bindings.insert((key_code, modifiers), action);
                }
            }
        }

        resolver
    }

    /// Resolve a key event to an action
    pub fn resolve(&self, event: &KeyEvent) -> Action {
        // Try custom bindings first
        if let Some(action) = self.bindings.get(&(event.code, event.modifiers)) {
            return action.clone();
        }

        // Fall back to default bindings
        if let Some(action) = self.default_bindings.get(&(event.code, event.modifiers)) {
            return action.clone();
        }

        // Handle regular character input
        if event.modifiers.is_empty() || event.modifiers == KeyModifiers::SHIFT {
            if let KeyCode::Char(c) = event.code {
                return Action::InsertChar(c);
            }
        }

        Action::None
    }

    /// Parse a key string to KeyCode
    fn parse_key(key: &str) -> Option<KeyCode> {
        match key.to_lowercase().as_str() {
            "enter" => Some(KeyCode::Enter),
            "backspace" => Some(KeyCode::Backspace),
            "delete" | "del" => Some(KeyCode::Delete),
            "tab" => Some(KeyCode::Tab),
            "esc" | "escape" => Some(KeyCode::Esc),
            "space" => Some(KeyCode::Char(' ')),

            "left" => Some(KeyCode::Left),
            "right" => Some(KeyCode::Right),
            "up" => Some(KeyCode::Up),
            "down" => Some(KeyCode::Down),
            "home" => Some(KeyCode::Home),
            "end" => Some(KeyCode::End),
            "pageup" => Some(KeyCode::PageUp),
            "pagedown" => Some(KeyCode::PageDown),

            s if s.len() == 1 => s.chars().next().map(KeyCode::Char),
            _ => None,
        }
    }

    /// Parse modifiers from strings
    fn parse_modifiers(modifiers: &[String]) -> KeyModifiers {
        let mut result = KeyModifiers::empty();
        for m in modifiers {
            match m.to_lowercase().as_str() {
                "ctrl" | "control" => result |= KeyModifiers::CONTROL,
                "shift" => result |= KeyModifiers::SHIFT,
                "alt" => result |= KeyModifiers::ALT,
                _ => {}
            }
        }
        result
    }

    /// Create default keybindings
    fn create_default_bindings() -> HashMap<(KeyCode, KeyModifiers), Action> {
        let mut bindings = HashMap::new();

        // Basic movement
        bindings.insert((KeyCode::Left, KeyModifiers::empty()), Action::MoveLeft);
        bindings.insert((KeyCode::Right, KeyModifiers::empty()), Action::MoveRight);
        bindings.insert((KeyCode::Up, KeyModifiers::empty()), Action::MoveUp);
        bindings.insert((KeyCode::Down, KeyModifiers::empty()), Action::MoveDown);

        bindings.insert((KeyCode::Home, KeyModifiers::empty()), Action::MoveLineStart);
        bindings.insert((KeyCode::End, KeyModifiers::empty()), Action::MoveLineEnd);
        bindings.insert((KeyCode::PageUp, KeyModifiers::empty()), Action::MovePageUp);
        bindings.insert((KeyCode::PageDown, KeyModifiers::empty()), Action::MovePageDown);

        // Word movement
        bindings.insert(
            (KeyCode::Left, KeyModifiers::CONTROL),
            Action::MoveWordLeft,
        );
        bindings.insert(
            (KeyCode::Right, KeyModifiers::CONTROL),
            Action::MoveWordRight,
        );

        // Selection
        bindings.insert((KeyCode::Left, KeyModifiers::SHIFT), Action::SelectLeft);
        bindings.insert((KeyCode::Right, KeyModifiers::SHIFT), Action::SelectRight);
        bindings.insert((KeyCode::Up, KeyModifiers::SHIFT), Action::SelectUp);
        bindings.insert((KeyCode::Down, KeyModifiers::SHIFT), Action::SelectDown);

        // Editing
        bindings.insert(
            (KeyCode::Backspace, KeyModifiers::empty()),
            Action::DeleteBackward,
        );
        bindings.insert(
            (KeyCode::Delete, KeyModifiers::empty()),
            Action::DeleteForward,
        );
        bindings.insert((KeyCode::Enter, KeyModifiers::empty()), Action::InsertNewline);
        bindings.insert((KeyCode::Tab, KeyModifiers::empty()), Action::InsertTab);

        // Delete word
        bindings.insert(
            (KeyCode::Backspace, KeyModifiers::CONTROL),
            Action::DeleteWordBackward,
        );
        bindings.insert(
            (KeyCode::Delete, KeyModifiers::CONTROL),
            Action::DeleteWordForward,
        );

        // File operations
        bindings.insert((KeyCode::Char('s'), KeyModifiers::CONTROL), Action::Save);
        bindings.insert((KeyCode::Char('q'), KeyModifiers::CONTROL), Action::Quit);
        bindings.insert((KeyCode::Char('o'), KeyModifiers::CONTROL), Action::Open);
        bindings.insert((KeyCode::Char('n'), KeyModifiers::CONTROL), Action::New);

        // Undo/redo
        bindings.insert((KeyCode::Char('z'), KeyModifiers::CONTROL), Action::Undo);
        bindings.insert((KeyCode::Char('y'), KeyModifiers::CONTROL), Action::Redo);

        // Clipboard
        bindings.insert((KeyCode::Char('c'), KeyModifiers::CONTROL), Action::Copy);
        bindings.insert((KeyCode::Char('x'), KeyModifiers::CONTROL), Action::Cut);
        bindings.insert((KeyCode::Char('v'), KeyModifiers::CONTROL), Action::Paste);

        // Selection
        bindings.insert((KeyCode::Char('a'), KeyModifiers::CONTROL), Action::SelectAll);

        // Multi-cursor
        bindings.insert(
            (KeyCode::Char('d'), KeyModifiers::CONTROL),
            Action::AddCursorNextMatch,
        );
        bindings.insert((KeyCode::Esc, KeyModifiers::empty()), Action::RemoveSecondaryCursors);

        bindings
    }

    /// Reload bindings from config (for hot reload)
    pub fn reload(&mut self, config: &Config) {
        self.bindings.clear();
        for binding in &config.keybindings {
            if let Some(key_code) = Self::parse_key(&binding.key) {
                let modifiers = Self::parse_modifiers(&binding.modifiers);
                if let Some(action) = Action::from_str(&binding.action, &binding.args) {
                    self.bindings.insert((key_code, modifiers), action);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_key() {
        assert_eq!(
            KeybindingResolver::parse_key("enter"),
            Some(KeyCode::Enter)
        );
        assert_eq!(
            KeybindingResolver::parse_key("backspace"),
            Some(KeyCode::Backspace)
        );
        assert_eq!(
            KeybindingResolver::parse_key("a"),
            Some(KeyCode::Char('a'))
        );
    }

    #[test]
    fn test_parse_modifiers() {
        let mods = vec!["ctrl".to_string()];
        assert_eq!(
            KeybindingResolver::parse_modifiers(&mods),
            KeyModifiers::CONTROL
        );

        let mods = vec!["ctrl".to_string(), "shift".to_string()];
        assert_eq!(
            KeybindingResolver::parse_modifiers(&mods),
            KeyModifiers::CONTROL | KeyModifiers::SHIFT
        );
    }

    #[test]
    fn test_resolve_basic() {
        let config = Config::default();
        let resolver = KeybindingResolver::new(&config);

        let event = KeyEvent::new(KeyCode::Left, KeyModifiers::empty());
        assert_eq!(resolver.resolve(&event), Action::MoveLeft);

        let event = KeyEvent::new(KeyCode::Char('a'), KeyModifiers::empty());
        assert_eq!(resolver.resolve(&event), Action::InsertChar('a'));
    }

    #[test]
    fn test_action_from_str() {
        let args = HashMap::new();
        assert_eq!(
            Action::from_str("move_left", &args),
            Some(Action::MoveLeft)
        );
        assert_eq!(
            Action::from_str("save", &args),
            Some(Action::Save)
        );
        assert_eq!(Action::from_str("unknown", &args), None);
    }
}
