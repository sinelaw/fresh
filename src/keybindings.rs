use crate::config::Config;
use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use std::collections::HashMap;

/// Format a keybinding as a user-friendly string
/// On macOS, this will show ⌘ instead of Ctrl for better UX
pub fn format_keybinding(keycode: &KeyCode, modifiers: &KeyModifiers) -> String {
    let mut result = String::new();

    // On macOS, show ⌘ (Cmd) symbol instead of Ctrl for the Control modifier
    // This provides a more native experience for Mac users
    #[cfg(target_os = "macos")]
    let ctrl_label = "⌘";
    #[cfg(not(target_os = "macos"))]
    let ctrl_label = "Ctrl";

    if modifiers.contains(KeyModifiers::CONTROL) {
        result.push_str(ctrl_label);
        result.push('+');
    }
    if modifiers.contains(KeyModifiers::ALT) {
        #[cfg(target_os = "macos")]
        let alt_label = "⌥";
        #[cfg(not(target_os = "macos"))]
        let alt_label = "Alt";
        result.push_str(alt_label);
        result.push('+');
    }
    if modifiers.contains(KeyModifiers::SHIFT) {
        #[cfg(target_os = "macos")]
        let shift_label = "⇧";
        #[cfg(not(target_os = "macos"))]
        let shift_label = "Shift";
        result.push_str(shift_label);
        result.push('+');
    }

    match keycode {
        KeyCode::Enter => result.push_str("Enter"),
        KeyCode::Backspace => result.push_str("Backspace"),
        KeyCode::Delete => result.push_str("Del"),
        KeyCode::Tab => result.push_str("Tab"),
        KeyCode::Esc => result.push_str("Esc"),
        KeyCode::Left => result.push_str("←"),
        KeyCode::Right => result.push_str("→"),
        KeyCode::Up => result.push_str("↑"),
        KeyCode::Down => result.push_str("↓"),
        KeyCode::Home => result.push_str("Home"),
        KeyCode::End => result.push_str("End"),
        KeyCode::PageUp => result.push_str("PgUp"),
        KeyCode::PageDown => result.push_str("PgDn"),
        KeyCode::Char(' ') => result.push_str("Space"),
        KeyCode::Char(c) => result.push_str(&c.to_uppercase().to_string()),
        KeyCode::F(n) => result.push_str(&format!("F{}", n)),
        _ => return String::new(),
    }

    result
}

/// Context in which a keybinding is active
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum KeyContext {
    /// Global bindings that work in all contexts (checked first with highest priority)
    Global,
    /// Normal editing mode
    Normal,
    /// Help screen is visible
    Help,
    /// Prompt/minibuffer is active
    Prompt,
    /// Popup window is visible
    Popup,
    /// File explorer has focus
    FileExplorer,
    /// Rename mode is active
    Rename,
}

impl KeyContext {
    /// Check if a context should allow input
    pub fn allows_text_input(&self) -> bool {
        matches!(
            self,
            KeyContext::Normal | KeyContext::Prompt | KeyContext::Rename
        )
    }

    /// Parse context from a "when" string
    pub fn from_when_clause(when: &str) -> Option<Self> {
        match when.trim() {
            "global" => Some(KeyContext::Global),
            "help" => Some(KeyContext::Help),
            "prompt" => Some(KeyContext::Prompt),
            "popup" => Some(KeyContext::Popup),
            "fileExplorer" | "file_explorer" => Some(KeyContext::FileExplorer),
            "normal" => Some(KeyContext::Normal),
            _ => None,
        }
    }

    /// Convert context to "when" clause string
    pub fn to_when_clause(self) -> &'static str {
        match self {
            KeyContext::Global => "global",
            KeyContext::Normal => "normal",
            KeyContext::Help => "help",
            KeyContext::Prompt => "prompt",
            KeyContext::Popup => "popup",
            KeyContext::FileExplorer => "fileExplorer",
            KeyContext::Rename => "rename",
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
    ToggleLineWrap,

    // Buffer navigation
    NextBuffer,
    PrevBuffer,

    // Position history navigation
    NavigateBack,
    NavigateForward,

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
    PromptDelete,
    PromptMoveLeft,
    PromptMoveRight,
    PromptMoveStart,
    PromptMoveEnd,
    PromptSelectPrev,
    PromptSelectNext,
    PromptPageUp,
    PromptPageDown,
    PromptAcceptSuggestion,
    PromptMoveWordLeft,
    PromptMoveWordRight,
    // Advanced prompt editing (word operations, clipboard)
    PromptDeleteWordForward,
    PromptDeleteWordBackward,
    PromptCopy,
    PromptCut,
    PromptPaste,
    // Prompt selection actions
    PromptMoveLeftSelecting,
    PromptMoveRightSelecting,
    PromptMoveHomeSelecting,
    PromptMoveEndSelecting,
    PromptSelectWordLeft,
    PromptSelectWordRight,
    PromptSelectAll,

    // Popup mode actions
    PopupSelectNext,
    PopupSelectPrev,
    PopupPageUp,
    PopupPageDown,
    PopupConfirm,
    PopupCancel,

    // Rename mode actions
    RenameConfirm,
    RenameCancel,
    RenameMoveLeft,
    RenameMoveRight,
    RenameMoveHome,
    RenameMoveEnd,

    // File explorer operations
    ToggleFileExplorer,
    FocusFileExplorer,
    FocusEditor,
    FileExplorerUp,
    FileExplorerDown,
    FileExplorerExpand,
    FileExplorerCollapse,
    FileExplorerOpen,
    FileExplorerRefresh,
    FileExplorerNewFile,
    FileExplorerNewDirectory,
    FileExplorerDelete,
    FileExplorerRename,
    FileExplorerToggleHidden,
    FileExplorerToggleGitignored,

    // LSP operations
    LspCompletion,
    LspGotoDefinition,
    LspRename,

    // Git operations
    GitGrep,
    GitFindFile,

    // Search and replace
    Search,
    FindNext,
    FindPrevious,
    Replace,
    QueryReplace, // Interactive replace (y/n/!/q for each match)

    // Plugin custom actions
    PluginAction(String),

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
            "toggle_line_wrap" => Some(Action::ToggleLineWrap),

            "next_buffer" => Some(Action::NextBuffer),
            "prev_buffer" => Some(Action::PrevBuffer),

            "navigate_back" => Some(Action::NavigateBack),
            "navigate_forward" => Some(Action::NavigateForward),

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
            "prompt_page_up" => Some(Action::PromptPageUp),
            "prompt_page_down" => Some(Action::PromptPageDown),
            "prompt_accept_suggestion" => Some(Action::PromptAcceptSuggestion),
            "prompt_delete_word_forward" => Some(Action::PromptDeleteWordForward),
            "prompt_delete_word_backward" => Some(Action::PromptDeleteWordBackward),
            "prompt_copy" => Some(Action::PromptCopy),
            "prompt_cut" => Some(Action::PromptCut),
            "prompt_paste" => Some(Action::PromptPaste),

            "popup_select_next" => Some(Action::PopupSelectNext),
            "popup_select_prev" => Some(Action::PopupSelectPrev),
            "popup_page_up" => Some(Action::PopupPageUp),
            "popup_page_down" => Some(Action::PopupPageDown),
            "popup_confirm" => Some(Action::PopupConfirm),
            "popup_cancel" => Some(Action::PopupCancel),

            "rename_confirm" => Some(Action::RenameConfirm),
            "rename_cancel" => Some(Action::RenameCancel),
            "rename_move_left" => Some(Action::RenameMoveLeft),
            "rename_move_right" => Some(Action::RenameMoveRight),
            "rename_move_home" => Some(Action::RenameMoveHome),
            "rename_move_end" => Some(Action::RenameMoveEnd),

            "toggle_file_explorer" => Some(Action::ToggleFileExplorer),
            "focus_file_explorer" => Some(Action::FocusFileExplorer),
            "focus_editor" => Some(Action::FocusEditor),
            "file_explorer_up" => Some(Action::FileExplorerUp),
            "file_explorer_down" => Some(Action::FileExplorerDown),
            "file_explorer_expand" => Some(Action::FileExplorerExpand),
            "file_explorer_collapse" => Some(Action::FileExplorerCollapse),
            "file_explorer_open" => Some(Action::FileExplorerOpen),
            "file_explorer_refresh" => Some(Action::FileExplorerRefresh),
            "file_explorer_new_file" => Some(Action::FileExplorerNewFile),
            "file_explorer_new_directory" => Some(Action::FileExplorerNewDirectory),
            "file_explorer_delete" => Some(Action::FileExplorerDelete),
            "file_explorer_rename" => Some(Action::FileExplorerRename),
            "file_explorer_toggle_hidden" => Some(Action::FileExplorerToggleHidden),
            "file_explorer_toggle_gitignored" => Some(Action::FileExplorerToggleGitignored),

            "lsp_completion" => Some(Action::LspCompletion),
            "lsp_goto_definition" => Some(Action::LspGotoDefinition),
            "lsp_rename" => Some(Action::LspRename),

            "git_grep" => Some(Action::GitGrep),
            "git_find_file" => Some(Action::GitFindFile),

            "search" => Some(Action::Search),
            "find_next" => Some(Action::FindNext),
            "find_previous" => Some(Action::FindPrevious),
            "replace" => Some(Action::Replace),
            "query_replace" => Some(Action::QueryReplace),

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

                    resolver
                        .bindings
                        .entry(context)
                        .or_insert_with(HashMap::new)
                        .insert((key_code, modifiers), action);
                }
            }
        }

        resolver
    }

    /// Check if an action is application-wide (should be accessible in all contexts)
    fn is_application_wide_action(action: &Action) -> bool {
        matches!(
            action,
            Action::Quit
                | Action::Save
                | Action::SaveAs
                | Action::ShowHelp
                | Action::HelpToggle
                | Action::PromptCancel  // Esc should always cancel
                | Action::PopupCancel // Esc should always cancel
        )
    }

    /// Resolve a key event to an action in the given context
    pub fn resolve(&self, event: &KeyEvent, context: KeyContext) -> Action {
        tracing::debug!(
            "KeybindingResolver.resolve: code={:?}, modifiers={:?}, context={:?}",
            event.code,
            event.modifiers,
            context
        );

        // Check Global bindings first (highest priority - work in all contexts)
        if let Some(global_bindings) = self.bindings.get(&KeyContext::Global) {
            if let Some(action) = global_bindings.get(&(event.code, event.modifiers)) {
                tracing::debug!("  -> Found in custom global bindings: {:?}", action);
                return action.clone();
            }
        }

        if let Some(global_bindings) = self.default_bindings.get(&KeyContext::Global) {
            if let Some(action) = global_bindings.get(&(event.code, event.modifiers)) {
                tracing::debug!("  -> Found in default global bindings: {:?}", action);
                return action.clone();
            }
        }

        // Try context-specific custom bindings
        if let Some(context_bindings) = self.bindings.get(&context) {
            if let Some(action) = context_bindings.get(&(event.code, event.modifiers)) {
                tracing::debug!(
                    "  -> Found in custom {} bindings: {:?}",
                    context.to_when_clause(),
                    action
                );
                return action.clone();
            }
        }

        // Try context-specific default bindings
        if let Some(context_bindings) = self.default_bindings.get(&context) {
            if let Some(action) = context_bindings.get(&(event.code, event.modifiers)) {
                tracing::debug!(
                    "  -> Found in default {} bindings: {:?}",
                    context.to_when_clause(),
                    action
                );
                return action.clone();
            }
        }

        // Fall back to normal context ONLY for application-wide actions
        // This prevents keys from leaking through to the editor when in special contexts
        if context != KeyContext::Normal {
            if let Some(normal_bindings) = self.bindings.get(&KeyContext::Normal) {
                if let Some(action) = normal_bindings.get(&(event.code, event.modifiers)) {
                    if Self::is_application_wide_action(action) {
                        tracing::debug!(
                            "  -> Found application-wide action in custom normal bindings: {:?}",
                            action
                        );
                        return action.clone();
                    }
                }
            }

            if let Some(normal_bindings) = self.default_bindings.get(&KeyContext::Normal) {
                if let Some(action) = normal_bindings.get(&(event.code, event.modifiers)) {
                    if Self::is_application_wide_action(action) {
                        tracing::debug!(
                            "  -> Found application-wide action in default normal bindings: {:?}",
                            action
                        );
                        return action.clone();
                    }
                }
            }
        }

        // Handle regular character input in text input contexts
        if context.allows_text_input() {
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

        // Global context bindings (work in all contexts, checked first with highest priority)
        let mut global_bindings = HashMap::new();

        // Command palette (Ctrl+P, Ctrl+/)
        // These bindings work everywhere and provide a consistent way to access commands
        global_bindings.insert(
            (KeyCode::Char('p'), KeyModifiers::CONTROL),
            Action::CommandPalette,
        );
        global_bindings.insert(
            (KeyCode::Char('/'), KeyModifiers::CONTROL),
            Action::CommandPalette,
        );
        // Some terminals send Ctrl+7 for Ctrl+/ (since / is Shift+7 on many keyboards)
        global_bindings.insert(
            (KeyCode::Char('7'), KeyModifiers::CONTROL),
            Action::CommandPalette,
        );

        all_bindings.insert(KeyContext::Global, global_bindings);

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
        // Ctrl+H is what terminals actually send for Ctrl+Backspace
        bindings.insert(
            (KeyCode::Char('h'), KeyModifiers::CONTROL),
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

        // Search and replace (Ctrl+F for search, Ctrl+R for replace, Ctrl+Alt+R for query-replace, F3/Shift+F3 for next/prev)
        bindings.insert((KeyCode::Char('f'), KeyModifiers::CONTROL), Action::Search);
        bindings.insert((KeyCode::Char('r'), KeyModifiers::CONTROL), Action::Replace);
        bindings.insert(
            (
                KeyCode::Char('r'),
                KeyModifiers::CONTROL | KeyModifiers::ALT,
            ),
            Action::QueryReplace,
        );
        bindings.insert((KeyCode::F(3), KeyModifiers::empty()), Action::FindNext);
        bindings.insert((KeyCode::F(3), KeyModifiers::SHIFT), Action::FindPrevious);

        // Git operations (Ctrl+Shift+F for grep, Ctrl+Shift+P for find file)
        bindings.insert(
            (
                KeyCode::Char('F'),
                KeyModifiers::CONTROL | KeyModifiers::SHIFT,
            ),
            Action::GitGrep,
        );
        bindings.insert(
            (
                KeyCode::Char('P'),
                KeyModifiers::CONTROL | KeyModifiers::SHIFT,
            ),
            Action::GitFindFile,
        );

        // Buffer navigation (Ctrl+PageUp/PageDown - standard in terminals and browsers)
        bindings.insert((KeyCode::PageUp, KeyModifiers::CONTROL), Action::PrevBuffer);
        bindings.insert(
            (KeyCode::PageDown, KeyModifiers::CONTROL),
            Action::NextBuffer,
        );

        // Position history navigation (Alt+Left/Right - like VS Code)
        bindings.insert((KeyCode::Left, KeyModifiers::ALT), Action::NavigateBack);
        bindings.insert((KeyCode::Right, KeyModifiers::ALT), Action::NavigateForward);

        // File explorer focus (Ctrl+B to toggle focus to file explorer)
        bindings.insert(
            (KeyCode::Char('b'), KeyModifiers::CONTROL),
            Action::FocusFileExplorer,
        );

        // LSP operations (F2 for rename, like VS Code)
        bindings.insert((KeyCode::F(2), KeyModifiers::empty()), Action::LspRename);

        all_bindings.insert(KeyContext::Normal, bindings);

        // Help context bindings
        let mut help_bindings = HashMap::new();
        help_bindings.insert((KeyCode::Esc, KeyModifiers::empty()), Action::HelpToggle);
        help_bindings.insert(
            (KeyCode::Char('h'), KeyModifiers::CONTROL),
            Action::HelpToggle,
        );
        help_bindings.insert((KeyCode::Up, KeyModifiers::empty()), Action::HelpScrollUp);
        help_bindings.insert(
            (KeyCode::Down, KeyModifiers::empty()),
            Action::HelpScrollDown,
        );
        help_bindings.insert((KeyCode::PageUp, KeyModifiers::empty()), Action::HelpPageUp);
        help_bindings.insert(
            (KeyCode::PageDown, KeyModifiers::empty()),
            Action::HelpPageDown,
        );
        all_bindings.insert(KeyContext::Help, help_bindings);

        // Prompt context bindings
        let mut prompt_bindings = HashMap::new();
        prompt_bindings.insert(
            (KeyCode::Enter, KeyModifiers::empty()),
            Action::PromptConfirm,
        );
        prompt_bindings.insert((KeyCode::Esc, KeyModifiers::empty()), Action::PromptCancel);
        prompt_bindings.insert(
            (KeyCode::Backspace, KeyModifiers::empty()),
            Action::PromptBackspace,
        );
        prompt_bindings.insert(
            (KeyCode::Delete, KeyModifiers::empty()),
            Action::PromptDelete,
        );
        prompt_bindings.insert(
            (KeyCode::Left, KeyModifiers::empty()),
            Action::PromptMoveLeft,
        );
        prompt_bindings.insert(
            (KeyCode::Right, KeyModifiers::empty()),
            Action::PromptMoveRight,
        );
        prompt_bindings.insert(
            (KeyCode::Home, KeyModifiers::empty()),
            Action::PromptMoveStart,
        );
        prompt_bindings.insert((KeyCode::End, KeyModifiers::empty()), Action::PromptMoveEnd);
        prompt_bindings.insert(
            (KeyCode::Up, KeyModifiers::empty()),
            Action::PromptSelectPrev,
        );
        prompt_bindings.insert(
            (KeyCode::Down, KeyModifiers::empty()),
            Action::PromptSelectNext,
        );
        prompt_bindings.insert(
            (KeyCode::PageUp, KeyModifiers::empty()),
            Action::PromptPageUp,
        );
        prompt_bindings.insert(
            (KeyCode::PageDown, KeyModifiers::empty()),
            Action::PromptPageDown,
        );
        prompt_bindings.insert(
            (KeyCode::Tab, KeyModifiers::empty()),
            Action::PromptAcceptSuggestion,
        );
        // Word movement operations
        prompt_bindings.insert(
            (KeyCode::Left, KeyModifiers::CONTROL),
            Action::PromptMoveWordLeft,
        );
        prompt_bindings.insert(
            (KeyCode::Right, KeyModifiers::CONTROL),
            Action::PromptMoveWordRight,
        );
        // Word deletion operations
        prompt_bindings.insert(
            (KeyCode::Backspace, KeyModifiers::CONTROL),
            Action::PromptDeleteWordBackward,
        );
        // Ctrl+H is what terminals actually send for Ctrl+Backspace
        prompt_bindings.insert(
            (KeyCode::Char('h'), KeyModifiers::CONTROL),
            Action::PromptDeleteWordBackward,
        );
        prompt_bindings.insert(
            (KeyCode::Delete, KeyModifiers::CONTROL),
            Action::PromptDeleteWordForward,
        );
        // Clipboard operations
        prompt_bindings.insert(
            (KeyCode::Char('c'), KeyModifiers::CONTROL),
            Action::PromptCopy,
        );
        prompt_bindings.insert(
            (KeyCode::Char('x'), KeyModifiers::CONTROL),
            Action::PromptCut,
        );
        prompt_bindings.insert(
            (KeyCode::Char('v'), KeyModifiers::CONTROL),
            Action::PromptPaste,
        );
        // Selection operations
        prompt_bindings.insert(
            (KeyCode::Left, KeyModifiers::SHIFT),
            Action::PromptMoveLeftSelecting,
        );
        prompt_bindings.insert(
            (KeyCode::Right, KeyModifiers::SHIFT),
            Action::PromptMoveRightSelecting,
        );
        prompt_bindings.insert(
            (KeyCode::Home, KeyModifiers::SHIFT),
            Action::PromptMoveHomeSelecting,
        );
        prompt_bindings.insert(
            (KeyCode::End, KeyModifiers::SHIFT),
            Action::PromptMoveEndSelecting,
        );
        prompt_bindings.insert(
            (KeyCode::Left, KeyModifiers::SHIFT | KeyModifiers::CONTROL),
            Action::PromptSelectWordLeft,
        );
        prompt_bindings.insert(
            (KeyCode::Right, KeyModifiers::SHIFT | KeyModifiers::CONTROL),
            Action::PromptSelectWordRight,
        );
        prompt_bindings.insert(
            (KeyCode::Char('a'), KeyModifiers::CONTROL),
            Action::PromptSelectAll,
        );
        all_bindings.insert(KeyContext::Prompt, prompt_bindings);

        // Popup context bindings
        let mut popup_bindings = HashMap::new();
        popup_bindings.insert(
            (KeyCode::Up, KeyModifiers::empty()),
            Action::PopupSelectPrev,
        );
        popup_bindings.insert(
            (KeyCode::Down, KeyModifiers::empty()),
            Action::PopupSelectNext,
        );
        popup_bindings.insert(
            (KeyCode::PageUp, KeyModifiers::empty()),
            Action::PopupPageUp,
        );
        popup_bindings.insert(
            (KeyCode::PageDown, KeyModifiers::empty()),
            Action::PopupPageDown,
        );
        popup_bindings.insert(
            (KeyCode::Enter, KeyModifiers::empty()),
            Action::PopupConfirm,
        );
        popup_bindings.insert((KeyCode::Esc, KeyModifiers::empty()), Action::PopupCancel);
        all_bindings.insert(KeyContext::Popup, popup_bindings);

        // File Explorer context bindings
        let mut explorer_bindings = HashMap::new();
        explorer_bindings.insert((KeyCode::Up, KeyModifiers::empty()), Action::FileExplorerUp);
        explorer_bindings.insert(
            (KeyCode::Down, KeyModifiers::empty()),
            Action::FileExplorerDown,
        );
        explorer_bindings.insert(
            (KeyCode::Enter, KeyModifiers::empty()),
            Action::FileExplorerOpen,
        );
        explorer_bindings.insert(
            (KeyCode::Right, KeyModifiers::empty()),
            Action::FileExplorerExpand,
        );
        explorer_bindings.insert(
            (KeyCode::Left, KeyModifiers::empty()),
            Action::FileExplorerCollapse,
        );
        explorer_bindings.insert(
            (KeyCode::Char('r'), KeyModifiers::CONTROL),
            Action::FileExplorerRefresh,
        );
        explorer_bindings.insert((KeyCode::Esc, KeyModifiers::empty()), Action::FocusEditor);
        explorer_bindings.insert(
            (KeyCode::Char('b'), KeyModifiers::CONTROL),
            Action::FocusEditor,
        );
        all_bindings.insert(KeyContext::FileExplorer, explorer_bindings);

        // Rename context bindings
        // Note: Character input is handled by allows_text_input() in resolve()
        let mut rename_bindings = HashMap::new();
        rename_bindings.insert(
            (KeyCode::Enter, KeyModifiers::empty()),
            Action::RenameConfirm,
        );
        rename_bindings.insert((KeyCode::Esc, KeyModifiers::empty()), Action::RenameCancel);
        rename_bindings.insert(
            (KeyCode::Backspace, KeyModifiers::empty()),
            Action::DeleteBackward,
        );
        rename_bindings.insert(
            (KeyCode::Delete, KeyModifiers::empty()),
            Action::DeleteForward,
        );
        // Arrow keys for restricted movement within the renamed symbol
        rename_bindings.insert(
            (KeyCode::Left, KeyModifiers::empty()),
            Action::RenameMoveLeft,
        );
        rename_bindings.insert(
            (KeyCode::Right, KeyModifiers::empty()),
            Action::RenameMoveRight,
        );
        rename_bindings.insert(
            (KeyCode::Home, KeyModifiers::empty()),
            Action::RenameMoveHome,
        );
        rename_bindings.insert((KeyCode::End, KeyModifiers::empty()), Action::RenameMoveEnd);
        // Cancel rename on PageUp/PageDown (these would normally move viewport)
        rename_bindings.insert(
            (KeyCode::PageUp, KeyModifiers::empty()),
            Action::RenameCancel,
        );
        rename_bindings.insert(
            (KeyCode::PageDown, KeyModifiers::empty()),
            Action::RenameCancel,
        );
        // Also cancel on Up/Down arrow keys (navigating away from current line)
        rename_bindings.insert((KeyCode::Up, KeyModifiers::empty()), Action::RenameCancel);
        rename_bindings.insert((KeyCode::Down, KeyModifiers::empty()), Action::RenameCancel);
        all_bindings.insert(KeyContext::Rename, rename_bindings);

        all_bindings
    }

    /// Get all keybindings (for help display)
    /// Returns a Vec of (key_description, action_description)
    pub fn get_all_bindings(&self) -> Vec<(String, String)> {
        let mut bindings = Vec::new();

        // Collect all bindings from all contexts
        for context in &[
            KeyContext::Normal,
            KeyContext::Help,
            KeyContext::Prompt,
            KeyContext::Popup,
            KeyContext::FileExplorer,
            KeyContext::Rename,
        ] {
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
        format_keybinding(&key_code, &modifiers)
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
            Action::ToggleLineWrap => "Toggle line wrap".to_string(),
            Action::NextBuffer => "Next buffer".to_string(),
            Action::PrevBuffer => "Previous buffer".to_string(),
            Action::NavigateBack => "Navigate back in history".to_string(),
            Action::NavigateForward => "Navigate forward in history".to_string(),
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
            Action::PromptDelete => "Prompt delete".to_string(),
            Action::PromptMoveLeft => "Prompt move left".to_string(),
            Action::PromptMoveRight => "Prompt move right".to_string(),
            Action::PromptMoveStart => "Prompt move to start".to_string(),
            Action::PromptMoveEnd => "Prompt move to end".to_string(),
            Action::PromptSelectPrev => "Prompt select previous".to_string(),
            Action::PromptSelectNext => "Prompt select next".to_string(),
            Action::PromptPageUp => "Prompt page up".to_string(),
            Action::PromptPageDown => "Prompt page down".to_string(),
            Action::PromptAcceptSuggestion => "Prompt accept suggestion".to_string(),
            Action::PromptMoveWordLeft => "Prompt move word left".to_string(),
            Action::PromptMoveWordRight => "Prompt move word right".to_string(),
            Action::PromptDeleteWordForward => "Prompt delete word forward".to_string(),
            Action::PromptDeleteWordBackward => "Prompt delete word backward".to_string(),
            Action::PromptCopy => "Prompt copy".to_string(),
            Action::PromptCut => "Prompt cut".to_string(),
            Action::PromptPaste => "Prompt paste".to_string(),
            Action::PromptMoveLeftSelecting => "Prompt move left selecting".to_string(),
            Action::PromptMoveRightSelecting => "Prompt move right selecting".to_string(),
            Action::PromptMoveHomeSelecting => "Prompt move to start selecting".to_string(),
            Action::PromptMoveEndSelecting => "Prompt move to end selecting".to_string(),
            Action::PromptSelectWordLeft => "Prompt select word left".to_string(),
            Action::PromptSelectWordRight => "Prompt select word right".to_string(),
            Action::PromptSelectAll => "Prompt select all".to_string(),
            Action::PopupSelectNext => "Popup select next".to_string(),
            Action::PopupSelectPrev => "Popup select previous".to_string(),
            Action::PopupPageUp => "Popup page up".to_string(),
            Action::PopupPageDown => "Popup page down".to_string(),
            Action::PopupConfirm => "Popup confirm".to_string(),
            Action::PopupCancel => "Popup cancel".to_string(),
            Action::RenameConfirm => "Rename confirm".to_string(),
            Action::RenameCancel => "Rename cancel".to_string(),
            Action::RenameMoveLeft => "Rename: move cursor left".to_string(),
            Action::RenameMoveRight => "Rename: move cursor right".to_string(),
            Action::RenameMoveHome => "Rename: move to start".to_string(),
            Action::RenameMoveEnd => "Rename: move to end".to_string(),
            Action::ToggleFileExplorer => "Toggle file explorer".to_string(),
            Action::FocusFileExplorer => "Focus file explorer".to_string(),
            Action::FocusEditor => "Focus editor".to_string(),
            Action::FileExplorerUp => "File explorer: navigate up".to_string(),
            Action::FileExplorerDown => "File explorer: navigate down".to_string(),
            Action::FileExplorerExpand => "File explorer: expand directory".to_string(),
            Action::FileExplorerCollapse => "File explorer: collapse directory".to_string(),
            Action::FileExplorerOpen => "File explorer: open file".to_string(),
            Action::FileExplorerRefresh => "File explorer: refresh".to_string(),
            Action::FileExplorerNewFile => "File explorer: new file".to_string(),
            Action::FileExplorerNewDirectory => "File explorer: new directory".to_string(),
            Action::FileExplorerDelete => "File explorer: delete".to_string(),
            Action::FileExplorerRename => "File explorer: rename".to_string(),
            Action::FileExplorerToggleHidden => "File explorer: toggle hidden files".to_string(),
            Action::FileExplorerToggleGitignored => {
                "File explorer: toggle gitignored files".to_string()
            }
            Action::LspCompletion => "LSP: Show completion suggestions".to_string(),
            Action::LspGotoDefinition => "LSP: Go to definition".to_string(),
            Action::LspRename => "LSP: Rename symbol".to_string(),
            Action::GitGrep => "Git: Grep - search through git-tracked files".to_string(),
            Action::GitFindFile => {
                "Git: Find File - find file by filtering git ls-files".to_string()
            }
            Action::Search => "Search for text in buffer".to_string(),
            Action::FindNext => "Find next search match".to_string(),
            Action::FindPrevious => "Find previous search match".to_string(),
            Action::Replace => "Replace text in buffer".to_string(),
            Action::QueryReplace => "Interactive replace (y/n/!/q for each match)".to_string(),
            Action::PluginAction(name) => format!("Plugin action: {}", name),
            Action::None => "No action".to_string(),
        }
    }

    /// Get the keybinding string for an action in a specific context
    /// Returns the first keybinding found (prioritizing custom bindings over defaults)
    /// Returns None if no binding is found
    pub fn get_keybinding_for_action(
        &self,
        action: &Action,
        context: KeyContext,
    ) -> Option<String> {
        // Check custom bindings first (higher priority)
        if let Some(context_bindings) = self.bindings.get(&context) {
            for ((keycode, modifiers), bound_action) in context_bindings {
                if bound_action == action {
                    return Some(format_keybinding(keycode, modifiers));
                }
            }
        }

        // Check default bindings for this context
        if let Some(context_bindings) = self.default_bindings.get(&context) {
            for ((keycode, modifiers), bound_action) in context_bindings {
                if bound_action == action {
                    return Some(format_keybinding(keycode, modifiers));
                }
            }
        }

        // For certain contexts, also check Normal context for application-wide actions
        if context != KeyContext::Normal && Self::is_application_wide_action(action) {
            // Check custom normal bindings
            if let Some(normal_bindings) = self.bindings.get(&KeyContext::Normal) {
                for ((keycode, modifiers), bound_action) in normal_bindings {
                    if bound_action == action {
                        return Some(format_keybinding(keycode, modifiers));
                    }
                }
            }

            // Check default normal bindings
            if let Some(normal_bindings) = self.default_bindings.get(&KeyContext::Normal) {
                for ((keycode, modifiers), bound_action) in normal_bindings {
                    if bound_action == action {
                        return Some(format_keybinding(keycode, modifiers));
                    }
                }
            }
        }

        None
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
        assert_eq!(
            resolver.resolve(&event, KeyContext::Normal),
            Action::MoveLeft
        );

        let event = KeyEvent::new(KeyCode::Char('a'), KeyModifiers::empty());
        assert_eq!(
            resolver.resolve(&event, KeyContext::Normal),
            Action::InsertChar('a')
        );
    }

    #[test]
    fn test_action_from_str() {
        let args = HashMap::new();
        assert_eq!(Action::from_str("move_left", &args), Some(Action::MoveLeft));
        assert_eq!(Action::from_str("save", &args), Some(Action::Save));
        assert_eq!(Action::from_str("unknown", &args), None);

        // Test new context-specific actions
        assert_eq!(
            Action::from_str("help_toggle", &args),
            Some(Action::HelpToggle)
        );
        assert_eq!(
            Action::from_str("prompt_confirm", &args),
            Some(Action::PromptConfirm)
        );
        assert_eq!(
            Action::from_str("popup_cancel", &args),
            Some(Action::PopupCancel)
        );
    }

    #[test]
    fn test_key_context_from_when_clause() {
        assert_eq!(
            KeyContext::from_when_clause("normal"),
            Some(KeyContext::Normal)
        );
        assert_eq!(KeyContext::from_when_clause("help"), Some(KeyContext::Help));
        assert_eq!(
            KeyContext::from_when_clause("prompt"),
            Some(KeyContext::Prompt)
        );
        assert_eq!(
            KeyContext::from_when_clause("popup"),
            Some(KeyContext::Popup)
        );
        assert_eq!(
            KeyContext::from_when_clause("  help  "),
            Some(KeyContext::Help)
        ); // Test trimming
        assert_eq!(KeyContext::from_when_clause("unknown"), None);
        assert_eq!(KeyContext::from_when_clause(""), None);
    }

    #[test]
    fn test_key_context_to_when_clause() {
        assert_eq!(KeyContext::Normal.to_when_clause(), "normal");
        assert_eq!(KeyContext::Help.to_when_clause(), "help");
        assert_eq!(KeyContext::Prompt.to_when_clause(), "prompt");
        assert_eq!(KeyContext::Popup.to_when_clause(), "popup");
    }

    #[test]
    fn test_context_specific_bindings() {
        let config = Config::default();
        let resolver = KeybindingResolver::new(&config);

        // Test help context bindings
        let esc_event = KeyEvent::new(KeyCode::Esc, KeyModifiers::empty());
        assert_eq!(
            resolver.resolve(&esc_event, KeyContext::Help),
            Action::HelpToggle
        );
        assert_eq!(
            resolver.resolve(&esc_event, KeyContext::Normal),
            Action::RemoveSecondaryCursors
        );

        // Test prompt context bindings
        let enter_event = KeyEvent::new(KeyCode::Enter, KeyModifiers::empty());
        assert_eq!(
            resolver.resolve(&enter_event, KeyContext::Prompt),
            Action::PromptConfirm
        );
        assert_eq!(
            resolver.resolve(&enter_event, KeyContext::Normal),
            Action::InsertNewline
        );

        // Test popup context bindings
        let up_event = KeyEvent::new(KeyCode::Up, KeyModifiers::empty());
        assert_eq!(
            resolver.resolve(&up_event, KeyContext::Popup),
            Action::PopupSelectPrev
        );
        assert_eq!(
            resolver.resolve(&up_event, KeyContext::Normal),
            Action::MoveUp
        );
    }

    #[test]
    fn test_context_fallback_to_normal() {
        let config = Config::default();
        let resolver = KeybindingResolver::new(&config);

        // Ctrl+S should work in all contexts (falls back to normal)
        let save_event = KeyEvent::new(KeyCode::Char('s'), KeyModifiers::CONTROL);
        assert_eq!(
            resolver.resolve(&save_event, KeyContext::Normal),
            Action::Save
        );
        assert_eq!(
            resolver.resolve(&save_event, KeyContext::Help),
            Action::Save
        );
        assert_eq!(
            resolver.resolve(&save_event, KeyContext::Popup),
            Action::Save
        );
        // Note: Prompt context might handle this differently in practice
    }

    #[test]
    fn test_context_priority_resolution() {
        use crate::config::Keybinding;

        // Create a config with a custom binding that overrides default in help context
        let mut config = Config::default();
        config.keybindings.push(Keybinding {
            key: "esc".to_string(),
            modifiers: vec![],
            action: "quit".to_string(), // Override Esc in help context to quit
            args: HashMap::new(),
            when: Some("help".to_string()),
        });

        let resolver = KeybindingResolver::new(&config);
        let esc_event = KeyEvent::new(KeyCode::Esc, KeyModifiers::empty());

        // In help context, custom binding should override default HelpToggle
        assert_eq!(resolver.resolve(&esc_event, KeyContext::Help), Action::Quit);

        // In normal context, should still be RemoveSecondaryCursors
        assert_eq!(
            resolver.resolve(&esc_event, KeyContext::Normal),
            Action::RemoveSecondaryCursors
        );
    }

    #[test]
    fn test_character_input_in_contexts() {
        let config = Config::default();
        let resolver = KeybindingResolver::new(&config);

        let char_event = KeyEvent::new(KeyCode::Char('a'), KeyModifiers::empty());

        // Character input should work in Normal and Prompt contexts
        assert_eq!(
            resolver.resolve(&char_event, KeyContext::Normal),
            Action::InsertChar('a')
        );
        assert_eq!(
            resolver.resolve(&char_event, KeyContext::Prompt),
            Action::InsertChar('a')
        );

        // But not in Help or Popup contexts (returns None)
        assert_eq!(
            resolver.resolve(&char_event, KeyContext::Help),
            Action::None
        );
        assert_eq!(
            resolver.resolve(&char_event, KeyContext::Popup),
            Action::None
        );
    }

    #[test]
    fn test_custom_keybinding_loading() {
        use crate::config::Keybinding;

        let mut config = Config::default();

        // Add a custom keybinding for normal context
        config.keybindings.push(Keybinding {
            key: "f".to_string(),
            modifiers: vec!["ctrl".to_string()],
            action: "command_palette".to_string(),
            args: HashMap::new(),
            when: None, // Default to normal context
        });

        // Add a custom keybinding for prompt context
        config.keybindings.push(Keybinding {
            key: "k".to_string(),
            modifiers: vec!["ctrl".to_string()],
            action: "prompt_cancel".to_string(),
            args: HashMap::new(),
            when: Some("prompt".to_string()),
        });

        let resolver = KeybindingResolver::new(&config);

        // Test normal context custom binding
        let ctrl_f = KeyEvent::new(KeyCode::Char('f'), KeyModifiers::CONTROL);
        assert_eq!(
            resolver.resolve(&ctrl_f, KeyContext::Normal),
            Action::CommandPalette
        );

        // Test prompt context custom binding
        let ctrl_k = KeyEvent::new(KeyCode::Char('k'), KeyModifiers::CONTROL);
        assert_eq!(
            resolver.resolve(&ctrl_k, KeyContext::Prompt),
            Action::PromptCancel
        );
        assert_eq!(resolver.resolve(&ctrl_k, KeyContext::Normal), Action::None);
        // Not bound in normal
    }

    #[test]
    fn test_all_context_default_bindings_exist() {
        let config = Config::default();
        let resolver = KeybindingResolver::new(&config);

        // Verify that default bindings exist for all contexts
        assert!(resolver.default_bindings.contains_key(&KeyContext::Normal));
        assert!(resolver.default_bindings.contains_key(&KeyContext::Help));
        assert!(resolver.default_bindings.contains_key(&KeyContext::Prompt));
        assert!(resolver.default_bindings.contains_key(&KeyContext::Popup));

        // Verify each context has some bindings
        assert!(!resolver.default_bindings[&KeyContext::Normal].is_empty());
        assert!(!resolver.default_bindings[&KeyContext::Help].is_empty());
        assert!(!resolver.default_bindings[&KeyContext::Prompt].is_empty());
        assert!(!resolver.default_bindings[&KeyContext::Popup].is_empty());
    }

    #[test]
    fn test_resolve_determinism() {
        // Property: Resolving the same key in the same context should always return the same action
        let config = Config::default();
        let resolver = KeybindingResolver::new(&config);

        let test_cases = vec![
            (KeyCode::Left, KeyModifiers::empty(), KeyContext::Normal),
            (KeyCode::Esc, KeyModifiers::empty(), KeyContext::Help),
            (KeyCode::Enter, KeyModifiers::empty(), KeyContext::Prompt),
            (KeyCode::Down, KeyModifiers::empty(), KeyContext::Popup),
        ];

        for (key_code, modifiers, context) in test_cases {
            let event = KeyEvent::new(key_code, modifiers);
            let action1 = resolver.resolve(&event, context);
            let action2 = resolver.resolve(&event, context);
            let action3 = resolver.resolve(&event, context);

            assert_eq!(action1, action2, "Resolve should be deterministic");
            assert_eq!(action2, action3, "Resolve should be deterministic");
        }
    }

    #[test]
    fn test_modifier_combinations() {
        let config = Config::default();
        let resolver = KeybindingResolver::new(&config);

        // Test that modifier combinations are distinguished correctly
        let char_s = KeyCode::Char('s');

        let no_mod = KeyEvent::new(char_s, KeyModifiers::empty());
        let ctrl = KeyEvent::new(char_s, KeyModifiers::CONTROL);
        let shift = KeyEvent::new(char_s, KeyModifiers::SHIFT);
        let ctrl_shift = KeyEvent::new(char_s, KeyModifiers::CONTROL | KeyModifiers::SHIFT);

        let action_no_mod = resolver.resolve(&no_mod, KeyContext::Normal);
        let action_ctrl = resolver.resolve(&ctrl, KeyContext::Normal);
        let action_shift = resolver.resolve(&shift, KeyContext::Normal);
        let action_ctrl_shift = resolver.resolve(&ctrl_shift, KeyContext::Normal);

        // These should all be different actions (or at least distinguishable)
        assert_eq!(action_no_mod, Action::InsertChar('s'));
        assert_eq!(action_ctrl, Action::Save);
        assert_eq!(action_shift, Action::InsertChar('s')); // Shift alone is still character input
                                                           // Ctrl+Shift+S is not bound by default, should return None
        assert_eq!(action_ctrl_shift, Action::None);
    }
}
