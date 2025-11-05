use crate::config::Config;
use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use std::collections::HashMap;

/// Context in which a keybinding is active
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum KeyContext {
    /// Normal editing mode
    Normal,
    /// Help screen is visible
    Help,
    /// Prompt/minibuffer is active
    Prompt,
    /// Popup window is visible
    Popup,
}

impl KeyContext {
    /// Parse context from a "when" string
    pub fn from_when_clause(when: &str) -> Option<Self> {
        match when.trim() {
            "help" => Some(KeyContext::Help),
            "prompt" => Some(KeyContext::Prompt),
            "popup" => Some(KeyContext::Popup),
            "normal" => Some(KeyContext::Normal),
            _ => None,
        }
    }

    /// Convert context to "when" clause string
    pub fn to_when_clause(self) -> &'static str {
        match self {
            KeyContext::Normal => "normal",
            KeyContext::Help => "help",
            KeyContext::Prompt => "prompt",
            KeyContext::Popup => "popup",
        }
    }
}

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
    SelectDocumentStart,
    SelectDocumentEnd,
    SelectPageUp,
    SelectPageDown,
    SelectAll,
    SelectWord,
    SelectLine,
    ExpandSelection,

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
    ShowHelp,
    CommandPalette,

    // Buffer navigation
    NextBuffer,
    PrevBuffer,

    // Split view operations
    SplitHorizontal,
    SplitVertical,
    CloseSplit,
    NextSplit,
    PrevSplit,
    IncreaseSplitSize,
    DecreaseSplitSize,

    // Help mode actions
    HelpToggle,
    HelpScrollUp,
    HelpScrollDown,
    HelpPageUp,
    HelpPageDown,

    // Prompt mode actions
    PromptConfirm,
    PromptCancel,
    PromptBackspace,
    PromptMoveLeft,
    PromptMoveRight,
    PromptMoveStart,
    PromptMoveEnd,
    PromptSelectPrev,
    PromptSelectNext,
    PromptAcceptSuggestion,

    // Popup mode actions
    PopupSelectNext,
    PopupSelectPrev,
    PopupPageUp,
    PopupPageDown,
    PopupConfirm,
    PopupCancel,

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
            "select_document_start" => Some(Action::SelectDocumentStart),
            "select_document_end" => Some(Action::SelectDocumentEnd),
            "select_page_up" => Some(Action::SelectPageUp),
            "select_page_down" => Some(Action::SelectPageDown),
            "select_all" => Some(Action::SelectAll),
            "select_word" => Some(Action::SelectWord),
            "select_line" => Some(Action::SelectLine),
            "expand_selection" => Some(Action::ExpandSelection),

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
            "show_help" => Some(Action::ShowHelp),
            "command_palette" => Some(Action::CommandPalette),

            "next_buffer" => Some(Action::NextBuffer),
            "prev_buffer" => Some(Action::PrevBuffer),

            "split_horizontal" => Some(Action::SplitHorizontal),
            "split_vertical" => Some(Action::SplitVertical),
            "close_split" => Some(Action::CloseSplit),
            "next_split" => Some(Action::NextSplit),
            "prev_split" => Some(Action::PrevSplit),
            "increase_split_size" => Some(Action::IncreaseSplitSize),
            "decrease_split_size" => Some(Action::DecreaseSplitSize),

            "help_toggle" => Some(Action::HelpToggle),
            "help_scroll_up" => Some(Action::HelpScrollUp),
            "help_scroll_down" => Some(Action::HelpScrollDown),
            "help_page_up" => Some(Action::HelpPageUp),
            "help_page_down" => Some(Action::HelpPageDown),

            "prompt_confirm" => Some(Action::PromptConfirm),
            "prompt_cancel" => Some(Action::PromptCancel),
            "prompt_backspace" => Some(Action::PromptBackspace),
            "prompt_move_left" => Some(Action::PromptMoveLeft),
            "prompt_move_right" => Some(Action::PromptMoveRight),
            "prompt_move_start" => Some(Action::PromptMoveStart),
            "prompt_move_end" => Some(Action::PromptMoveEnd),
            "prompt_select_prev" => Some(Action::PromptSelectPrev),
            "prompt_select_next" => Some(Action::PromptSelectNext),
            "prompt_accept_suggestion" => Some(Action::PromptAcceptSuggestion),

            "popup_select_next" => Some(Action::PopupSelectNext),
            "popup_select_prev" => Some(Action::PopupSelectPrev),
            "popup_page_up" => Some(Action::PopupPageUp),
            "popup_page_down" => Some(Action::PopupPageDown),
            "popup_confirm" => Some(Action::PopupConfirm),
            "popup_cancel" => Some(Action::PopupCancel),

            _ => None,
        }
    }
}

/// Resolves key events to actions based on configuration
pub struct KeybindingResolver {
    /// Map from context to key bindings
    /// Context-specific bindings have priority over normal bindings
    bindings: HashMap<KeyContext, HashMap<(KeyCode, KeyModifiers), Action>>,

    /// Default bindings for each context
    default_bindings: HashMap<KeyContext, HashMap<(KeyCode, KeyModifiers), Action>>,
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
                    // Determine context from "when" clause
                    let context = if let Some(ref when) = binding.when {
                        KeyContext::from_when_clause(when).unwrap_or(KeyContext::Normal)
                    } else {
                        KeyContext::Normal
                    };

                    resolver.bindings
                        .entry(context)
                        .or_insert_with(HashMap::new)
                        .insert((key_code, modifiers), action);
                }
            }
        }

        resolver
    }

    /// Resolve a key event to an action in the given context
    pub fn resolve(&self, event: &KeyEvent, context: KeyContext) -> Action {
        tracing::debug!(
            "KeybindingResolver.resolve: code={:?}, modifiers={:?}, context={:?}",
            event.code,
            event.modifiers,
            context
        );

        // Try context-specific custom bindings first (highest priority)
        if let Some(context_bindings) = self.bindings.get(&context) {
            if let Some(action) = context_bindings.get(&(event.code, event.modifiers)) {
                tracing::debug!("  -> Found in custom {} bindings: {:?}", context.to_when_clause(), action);
                return action.clone();
            }
        }

        // Try context-specific default bindings
        if let Some(context_bindings) = self.default_bindings.get(&context) {
            if let Some(action) = context_bindings.get(&(event.code, event.modifiers)) {
                tracing::debug!("  -> Found in default {} bindings: {:?}", context.to_when_clause(), action);
                return action.clone();
            }
        }

        // Fall back to normal context if we're in a different context
        if context != KeyContext::Normal {
            if let Some(normal_bindings) = self.bindings.get(&KeyContext::Normal) {
                if let Some(action) = normal_bindings.get(&(event.code, event.modifiers)) {
                    tracing::debug!("  -> Found in custom normal bindings: {:?}", action);
                    return action.clone();
                }
            }

            if let Some(normal_bindings) = self.default_bindings.get(&KeyContext::Normal) {
                if let Some(action) = normal_bindings.get(&(event.code, event.modifiers)) {
                    tracing::debug!("  -> Found in default normal bindings: {:?}", action);
                    return action.clone();
                }
            }
        }

        // Handle regular character input (only in Normal and Prompt contexts)
        if matches!(context, KeyContext::Normal | KeyContext::Prompt) {
            if event.modifiers.is_empty() || event.modifiers == KeyModifiers::SHIFT {
                if let KeyCode::Char(c) = event.code {
                    tracing::debug!("  -> Character input: '{}'", c);
                    return Action::InsertChar(c);
                }
            }
        }

        tracing::debug!("  -> No binding found, returning Action::None");
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

    /// Create default keybindings organized by context
    fn create_default_bindings() -> HashMap<KeyContext, HashMap<(KeyCode, KeyModifiers), Action>> {
        let mut all_bindings = HashMap::new();

        // Normal context bindings
        let mut bindings = HashMap::new();

        // Basic movement
        bindings.insert((KeyCode::Left, KeyModifiers::empty()), Action::MoveLeft);
        bindings.insert((KeyCode::Right, KeyModifiers::empty()), Action::MoveRight);
        bindings.insert((KeyCode::Up, KeyModifiers::empty()), Action::MoveUp);
        bindings.insert((KeyCode::Down, KeyModifiers::empty()), Action::MoveDown);

        bindings.insert(
            (KeyCode::Home, KeyModifiers::empty()),
            Action::MoveLineStart,
        );
        bindings.insert((KeyCode::End, KeyModifiers::empty()), Action::MoveLineEnd);
        bindings.insert(
            (KeyCode::Home, KeyModifiers::CONTROL),
            Action::MoveDocumentStart,
        );
        bindings.insert(
            (KeyCode::End, KeyModifiers::CONTROL),
            Action::MoveDocumentEnd,
        );
        bindings.insert((KeyCode::PageUp, KeyModifiers::empty()), Action::MovePageUp);
        bindings.insert(
            (KeyCode::PageDown, KeyModifiers::empty()),
            Action::MovePageDown,
        );

        // Word movement
        bindings.insert((KeyCode::Left, KeyModifiers::CONTROL), Action::MoveWordLeft);
        bindings.insert(
            (KeyCode::Right, KeyModifiers::CONTROL),
            Action::MoveWordRight,
        );

        // Scrolling
        bindings.insert((KeyCode::Up, KeyModifiers::CONTROL), Action::ScrollUp);
        bindings.insert((KeyCode::Down, KeyModifiers::CONTROL), Action::ScrollDown);

        // Selection
        bindings.insert((KeyCode::Left, KeyModifiers::SHIFT), Action::SelectLeft);
        bindings.insert((KeyCode::Right, KeyModifiers::SHIFT), Action::SelectRight);
        bindings.insert((KeyCode::Up, KeyModifiers::SHIFT), Action::SelectUp);
        bindings.insert((KeyCode::Down, KeyModifiers::SHIFT), Action::SelectDown);
        bindings.insert(
            (KeyCode::Home, KeyModifiers::SHIFT),
            Action::SelectLineStart,
        );
        bindings.insert((KeyCode::End, KeyModifiers::SHIFT), Action::SelectLineEnd);
        bindings.insert((KeyCode::PageUp, KeyModifiers::SHIFT), Action::SelectPageUp);
        bindings.insert(
            (KeyCode::PageDown, KeyModifiers::SHIFT),
            Action::SelectPageDown,
        );
        bindings.insert(
            (KeyCode::Home, KeyModifiers::CONTROL | KeyModifiers::SHIFT),
            Action::SelectDocumentStart,
        );
        bindings.insert(
            (KeyCode::End, KeyModifiers::CONTROL | KeyModifiers::SHIFT),
            Action::SelectDocumentEnd,
        );

        // Editing
        bindings.insert(
            (KeyCode::Backspace, KeyModifiers::empty()),
            Action::DeleteBackward,
        );
        bindings.insert(
            (KeyCode::Delete, KeyModifiers::empty()),
            Action::DeleteForward,
        );
        bindings.insert(
            (KeyCode::Enter, KeyModifiers::empty()),
            Action::InsertNewline,
        );
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
        bindings.insert(
            (KeyCode::Char('a'), KeyModifiers::CONTROL),
            Action::SelectAll,
        );
        bindings.insert(
            (KeyCode::Char('w'), KeyModifiers::CONTROL),
            Action::SelectWord,
        );
        bindings.insert(
            (KeyCode::Char('l'), KeyModifiers::CONTROL),
            Action::SelectLine,
        );
        bindings.insert(
            (KeyCode::Right, KeyModifiers::CONTROL | KeyModifiers::SHIFT),
            Action::ExpandSelection,
        );
        bindings.insert(
            (KeyCode::Left, KeyModifiers::CONTROL | KeyModifiers::SHIFT),
            Action::SelectWordLeft,
        );

        // Multi-cursor
        bindings.insert(
            (KeyCode::Char('d'), KeyModifiers::CONTROL),
            Action::AddCursorNextMatch,
        );
        bindings.insert(
            (KeyCode::Up, KeyModifiers::CONTROL | KeyModifiers::ALT),
            Action::AddCursorAbove,
        );
        bindings.insert(
            (KeyCode::Down, KeyModifiers::CONTROL | KeyModifiers::ALT),
            Action::AddCursorBelow,
        );
        bindings.insert(
            (KeyCode::Esc, KeyModifiers::empty()),
            Action::RemoveSecondaryCursors,
        );

        // Help
        bindings.insert(
            (KeyCode::Char('h'), KeyModifiers::CONTROL),
            Action::ShowHelp,
        );

        // Command palette (Ctrl+P)
        bindings.insert(
            (KeyCode::Char('p'), KeyModifiers::CONTROL),
            Action::CommandPalette,
        );

        // Buffer navigation (Ctrl+PageUp/PageDown - standard in terminals and browsers)
        bindings.insert(
            (KeyCode::PageUp, KeyModifiers::CONTROL),
            Action::PrevBuffer,
        );
        bindings.insert(
            (KeyCode::PageDown, KeyModifiers::CONTROL),
            Action::NextBuffer,
        );

        all_bindings.insert(KeyContext::Normal, bindings);

        // Help context bindings
        let mut help_bindings = HashMap::new();
        help_bindings.insert((KeyCode::Esc, KeyModifiers::empty()), Action::HelpToggle);
        help_bindings.insert((KeyCode::Char('h'), KeyModifiers::CONTROL), Action::HelpToggle);
        help_bindings.insert((KeyCode::Up, KeyModifiers::empty()), Action::HelpScrollUp);
        help_bindings.insert((KeyCode::Down, KeyModifiers::empty()), Action::HelpScrollDown);
        help_bindings.insert((KeyCode::PageUp, KeyModifiers::empty()), Action::HelpPageUp);
        help_bindings.insert((KeyCode::PageDown, KeyModifiers::empty()), Action::HelpPageDown);
        all_bindings.insert(KeyContext::Help, help_bindings);

        // Prompt context bindings
        let mut prompt_bindings = HashMap::new();
        prompt_bindings.insert((KeyCode::Enter, KeyModifiers::empty()), Action::PromptConfirm);
        prompt_bindings.insert((KeyCode::Esc, KeyModifiers::empty()), Action::PromptCancel);
        prompt_bindings.insert((KeyCode::Backspace, KeyModifiers::empty()), Action::PromptBackspace);
        prompt_bindings.insert((KeyCode::Left, KeyModifiers::empty()), Action::PromptMoveLeft);
        prompt_bindings.insert((KeyCode::Right, KeyModifiers::empty()), Action::PromptMoveRight);
        prompt_bindings.insert((KeyCode::Home, KeyModifiers::empty()), Action::PromptMoveStart);
        prompt_bindings.insert((KeyCode::End, KeyModifiers::empty()), Action::PromptMoveEnd);
        prompt_bindings.insert((KeyCode::Up, KeyModifiers::empty()), Action::PromptSelectPrev);
        prompt_bindings.insert((KeyCode::Down, KeyModifiers::empty()), Action::PromptSelectNext);
        prompt_bindings.insert((KeyCode::Tab, KeyModifiers::empty()), Action::PromptAcceptSuggestion);
        all_bindings.insert(KeyContext::Prompt, prompt_bindings);

        // Popup context bindings
        let mut popup_bindings = HashMap::new();
        popup_bindings.insert((KeyCode::Up, KeyModifiers::empty()), Action::PopupSelectPrev);
        popup_bindings.insert((KeyCode::Down, KeyModifiers::empty()), Action::PopupSelectNext);
        popup_bindings.insert((KeyCode::PageUp, KeyModifiers::empty()), Action::PopupPageUp);
        popup_bindings.insert((KeyCode::PageDown, KeyModifiers::empty()), Action::PopupPageDown);
        popup_bindings.insert((KeyCode::Enter, KeyModifiers::empty()), Action::PopupConfirm);
        popup_bindings.insert((KeyCode::Esc, KeyModifiers::empty()), Action::PopupCancel);
        all_bindings.insert(KeyContext::Popup, popup_bindings);

        all_bindings
    }

    /// Get all keybindings (for help display)
    /// Returns a Vec of (key_description, action_description)
    pub fn get_all_bindings(&self) -> Vec<(String, String)> {
        let mut bindings = Vec::new();

        // Collect all bindings from all contexts
        for context in &[KeyContext::Normal, KeyContext::Help, KeyContext::Prompt, KeyContext::Popup] {
            let mut all_keys: HashMap<(KeyCode, KeyModifiers), Action> = HashMap::new();

            // Start with defaults for this context
            if let Some(context_defaults) = self.default_bindings.get(context) {
                for (key, action) in context_defaults {
                    all_keys.insert(*key, action.clone());
                }
            }

            // Override with custom bindings for this context
            if let Some(context_bindings) = self.bindings.get(context) {
                for (key, action) in context_bindings {
                    all_keys.insert(*key, action.clone());
                }
            }

            // Convert to readable format with context prefix
            let context_str = if *context != KeyContext::Normal {
                format!("[{}] ", context.to_when_clause())
            } else {
                String::new()
            };

            for ((key_code, modifiers), action) in all_keys {
                let key_str = Self::format_key(key_code, modifiers);
                let action_str = format!("{}{}", context_str, Self::format_action(&action));
                bindings.push((key_str, action_str));
            }
        }

        // Sort by action description for easier browsing
        bindings.sort_by(|a, b| a.1.cmp(&b.1));

        bindings
    }

    /// Format a key combination as a readable string
    fn format_key(key_code: KeyCode, modifiers: KeyModifiers) -> String {
        let mut parts = Vec::new();

        if modifiers.contains(KeyModifiers::CONTROL) {
            parts.push("Ctrl");
        }
        if modifiers.contains(KeyModifiers::ALT) {
            parts.push("Alt");
        }
        if modifiers.contains(KeyModifiers::SHIFT) {
            parts.push("Shift");
        }

        let key_name = match key_code {
            KeyCode::Char(c) => c.to_uppercase().to_string(),
            KeyCode::Enter => "Enter".to_string(),
            KeyCode::Tab => "Tab".to_string(),
            KeyCode::Backspace => "Backspace".to_string(),
            KeyCode::Delete => "Delete".to_string(),
            KeyCode::Left => "Left".to_string(),
            KeyCode::Right => "Right".to_string(),
            KeyCode::Up => "Up".to_string(),
            KeyCode::Down => "Down".to_string(),
            KeyCode::Home => "Home".to_string(),
            KeyCode::End => "End".to_string(),
            KeyCode::PageUp => "PageUp".to_string(),
            KeyCode::PageDown => "PageDown".to_string(),
            KeyCode::Esc => "Esc".to_string(),
            _ => format!("{key_code:?}"),
        };

        parts.push(&key_name);
        parts.join("+")
    }

    /// Format an action as a readable description
    fn format_action(action: &Action) -> String {
        match action {
            Action::InsertChar(c) => format!("Insert character '{c}'"),
            Action::InsertNewline => "Insert newline".to_string(),
            Action::InsertTab => "Insert tab".to_string(),
            Action::MoveLeft => "Move cursor left".to_string(),
            Action::MoveRight => "Move cursor right".to_string(),
            Action::MoveUp => "Move cursor up".to_string(),
            Action::MoveDown => "Move cursor down".to_string(),
            Action::MoveWordLeft => "Move word left".to_string(),
            Action::MoveWordRight => "Move word right".to_string(),
            Action::MoveLineStart => "Move to line start".to_string(),
            Action::MoveLineEnd => "Move to line end".to_string(),
            Action::MovePageUp => "Move page up".to_string(),
            Action::MovePageDown => "Move page down".to_string(),
            Action::MoveDocumentStart => "Move to document start".to_string(),
            Action::MoveDocumentEnd => "Move to document end".to_string(),
            Action::SelectLeft => "Select left".to_string(),
            Action::SelectRight => "Select right".to_string(),
            Action::SelectUp => "Select up".to_string(),
            Action::SelectDown => "Select down".to_string(),
            Action::SelectWordLeft => "Select word left".to_string(),
            Action::SelectWordRight => "Select word right".to_string(),
            Action::SelectLineStart => "Select to line start".to_string(),
            Action::SelectLineEnd => "Select to line end".to_string(),
            Action::SelectDocumentStart => "Select to document start".to_string(),
            Action::SelectDocumentEnd => "Select to document end".to_string(),
            Action::SelectPageUp => "Select page up".to_string(),
            Action::SelectPageDown => "Select page down".to_string(),
            Action::SelectAll => "Select all".to_string(),
            Action::SelectWord => "Select word under cursor".to_string(),
            Action::SelectLine => "Select current line".to_string(),
            Action::ExpandSelection => "Expand selection".to_string(),
            Action::DeleteBackward => "Delete backward".to_string(),
            Action::DeleteForward => "Delete forward".to_string(),
            Action::DeleteWordBackward => "Delete word backward".to_string(),
            Action::DeleteWordForward => "Delete word forward".to_string(),
            Action::DeleteLine => "Delete line".to_string(),
            Action::Copy => "Copy".to_string(),
            Action::Cut => "Cut".to_string(),
            Action::Paste => "Paste".to_string(),
            Action::AddCursorAbove => "Add cursor above".to_string(),
            Action::AddCursorBelow => "Add cursor below".to_string(),
            Action::AddCursorNextMatch => "Add cursor at next match".to_string(),
            Action::RemoveSecondaryCursors => "Remove secondary cursors".to_string(),
            Action::Save => "Save file".to_string(),
            Action::SaveAs => "Save file as...".to_string(),
            Action::Open => "Open file".to_string(),
            Action::New => "New file".to_string(),
            Action::Close => "Close file".to_string(),
            Action::Quit => "Quit editor".to_string(),
            Action::Undo => "Undo".to_string(),
            Action::Redo => "Redo".to_string(),
            Action::ScrollUp => "Scroll up".to_string(),
            Action::ScrollDown => "Scroll down".to_string(),
            Action::ShowHelp => "Show help".to_string(),
            Action::CommandPalette => "Command palette".to_string(),
            Action::NextBuffer => "Next buffer".to_string(),
            Action::PrevBuffer => "Previous buffer".to_string(),
            Action::SplitHorizontal => "Split horizontally".to_string(),
            Action::SplitVertical => "Split vertically".to_string(),
            Action::CloseSplit => "Close split".to_string(),
            Action::NextSplit => "Next split".to_string(),
            Action::PrevSplit => "Previous split".to_string(),
            Action::IncreaseSplitSize => "Increase split size".to_string(),
            Action::DecreaseSplitSize => "Decrease split size".to_string(),
            Action::HelpToggle => "Toggle help".to_string(),
            Action::HelpScrollUp => "Scroll help up".to_string(),
            Action::HelpScrollDown => "Scroll help down".to_string(),
            Action::HelpPageUp => "Help page up".to_string(),
            Action::HelpPageDown => "Help page down".to_string(),
            Action::PromptConfirm => "Confirm prompt".to_string(),
            Action::PromptCancel => "Cancel prompt".to_string(),
            Action::PromptBackspace => "Prompt backspace".to_string(),
            Action::PromptMoveLeft => "Prompt move left".to_string(),
            Action::PromptMoveRight => "Prompt move right".to_string(),
            Action::PromptMoveStart => "Prompt move to start".to_string(),
            Action::PromptMoveEnd => "Prompt move to end".to_string(),
            Action::PromptSelectPrev => "Prompt select previous".to_string(),
            Action::PromptSelectNext => "Prompt select next".to_string(),
            Action::PromptAcceptSuggestion => "Prompt accept suggestion".to_string(),
            Action::PopupSelectNext => "Popup select next".to_string(),
            Action::PopupSelectPrev => "Popup select previous".to_string(),
            Action::PopupPageUp => "Popup page up".to_string(),
            Action::PopupPageDown => "Popup page down".to_string(),
            Action::PopupConfirm => "Popup confirm".to_string(),
            Action::PopupCancel => "Popup cancel".to_string(),
            Action::None => "No action".to_string(),
        }
    }

    /// Reload bindings from config (for hot reload)
    pub fn reload(&mut self, config: &Config) {
        self.bindings.clear();
        for binding in &config.keybindings {
            if let Some(key_code) = Self::parse_key(&binding.key) {
                let modifiers = Self::parse_modifiers(&binding.modifiers);
                if let Some(action) = Action::from_str(&binding.action, &binding.args) {
                    // Determine context from "when" clause
                    let context = if let Some(ref when) = binding.when {
                        KeyContext::from_when_clause(when).unwrap_or(KeyContext::Normal)
                    } else {
                        KeyContext::Normal
                    };

                    self.bindings
                        .entry(context)
                        .or_insert_with(HashMap::new)
                        .insert((key_code, modifiers), action);
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
        assert_eq!(KeybindingResolver::parse_key("enter"), Some(KeyCode::Enter));
        assert_eq!(
            KeybindingResolver::parse_key("backspace"),
            Some(KeyCode::Backspace)
        );
        assert_eq!(KeybindingResolver::parse_key("a"), Some(KeyCode::Char('a')));
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
        assert_eq!(resolver.resolve(&event, KeyContext::Normal), Action::MoveLeft);

        let event = KeyEvent::new(KeyCode::Char('a'), KeyModifiers::empty());
        assert_eq!(resolver.resolve(&event, KeyContext::Normal), Action::InsertChar('a'));
    }

    #[test]
    fn test_action_from_str() {
        let args = HashMap::new();
        assert_eq!(Action::from_str("move_left", &args), Some(Action::MoveLeft));
        assert_eq!(Action::from_str("save", &args), Some(Action::Save));
        assert_eq!(Action::from_str("unknown", &args), None);
    }
}
