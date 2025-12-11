use crate::config::Config;
use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, Ordering};

/// Global flag to force Linux-style keybinding display (Ctrl instead of ⌘)
/// This is primarily used in tests to ensure consistent output across platforms.
static FORCE_LINUX_KEYBINDINGS: AtomicBool = AtomicBool::new(false);

/// Force Linux-style keybinding display (Ctrl/Alt/Shift instead of ⌘/⌥/⇧)
/// Call this in tests to ensure consistent output regardless of platform.
pub fn set_force_linux_keybindings(force: bool) {
    FORCE_LINUX_KEYBINDINGS.store(force, Ordering::SeqCst);
}

/// Check if we should use macOS-style symbols for keybindings
fn use_macos_symbols() -> bool {
    if FORCE_LINUX_KEYBINDINGS.load(Ordering::SeqCst) {
        return false;
    }
    cfg!(target_os = "macos")
}

/// Format a keybinding as a user-friendly string
/// On macOS, this will show ⌘ instead of Ctrl for better UX
pub fn format_keybinding(keycode: &KeyCode, modifiers: &KeyModifiers) -> String {
    let mut result = String::new();

    // On macOS, show ⌘ (Cmd) symbol instead of Ctrl for the Control modifier
    // This provides a more native experience for Mac users
    let (ctrl_label, alt_label, shift_label) = if use_macos_symbols() {
        ("⌘", "⌥", "⇧")
    } else {
        ("Ctrl", "Alt", "Shift")
    };

    if modifiers.contains(KeyModifiers::CONTROL) {
        result.push_str(ctrl_label);
        result.push('+');
    }
    if modifiers.contains(KeyModifiers::ALT) {
        result.push_str(alt_label);
        result.push('+');
    }
    if modifiers.contains(KeyModifiers::SHIFT) {
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

/// Returns a priority score for a keybinding key.
/// Lower scores indicate canonical/preferred keys, higher scores indicate terminal equivalents.
/// This helps ensure deterministic selection when multiple keybindings exist for an action.
fn keybinding_priority_score(key: &KeyCode) -> u32 {
    match key {
        // Terminal equivalents get higher scores (deprioritized)
        KeyCode::Char('@') => 100, // Equivalent of Space
        KeyCode::Char('7') => 100, // Equivalent of /
        KeyCode::Char('_') => 100, // Equivalent of -
        // Ctrl+H as backspace equivalent is handled differently (only plain Ctrl+H)
        // All other keys get default priority
        _ => 0,
    }
}

/// Returns terminal key equivalents for a given key combination.
///
/// Some key combinations are sent differently by terminals:
/// - Ctrl+/ is often sent as Ctrl+7
/// - Ctrl+Backspace is often sent as Ctrl+H
/// - Ctrl+Space is often sent as Ctrl+@ (NUL)
/// - Ctrl+[ is often sent as Escape
///
/// This function returns any equivalent key combinations that should be
/// treated as aliases for the given key.
pub fn terminal_key_equivalents(
    key: KeyCode,
    modifiers: KeyModifiers,
) -> Vec<(KeyCode, KeyModifiers)> {
    let mut equivalents = Vec::new();

    // Only consider equivalents when Ctrl is pressed
    if modifiers.contains(KeyModifiers::CONTROL) {
        let base_modifiers = modifiers; // Keep all modifiers including Ctrl

        match key {
            // Ctrl+/ is often sent as Ctrl+7
            KeyCode::Char('/') => {
                equivalents.push((KeyCode::Char('7'), base_modifiers));
            }
            KeyCode::Char('7') => {
                equivalents.push((KeyCode::Char('/'), base_modifiers));
            }

            // Ctrl+Backspace is often sent as Ctrl+H
            KeyCode::Backspace => {
                equivalents.push((KeyCode::Char('h'), base_modifiers));
            }
            KeyCode::Char('h') if modifiers == KeyModifiers::CONTROL => {
                // Only add Backspace equivalent for plain Ctrl+H (not Ctrl+Shift+H etc.)
                equivalents.push((KeyCode::Backspace, base_modifiers));
            }

            // Ctrl+Space is often sent as Ctrl+@ (NUL character, code 0)
            KeyCode::Char(' ') => {
                equivalents.push((KeyCode::Char('@'), base_modifiers));
            }
            KeyCode::Char('@') => {
                equivalents.push((KeyCode::Char(' '), base_modifiers));
            }

            // Ctrl+- is often sent as Ctrl+_
            KeyCode::Char('-') => {
                equivalents.push((KeyCode::Char('_'), base_modifiers));
            }
            KeyCode::Char('_') => {
                equivalents.push((KeyCode::Char('-'), base_modifiers));
            }

            _ => {}
        }
    }

    equivalents
}

/// Context in which a keybinding is active
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum KeyContext {
    /// Global bindings that work in all contexts (checked first with highest priority)
    Global,
    /// Normal editing mode
    Normal,
    /// Prompt/minibuffer is active
    Prompt,
    /// Popup window is visible
    Popup,
    /// File explorer has focus
    FileExplorer,
    /// Menu bar is active
    Menu,
    /// Terminal has focus
    Terminal,
}

impl KeyContext {
    /// Check if a context should allow input
    pub fn allows_text_input(&self) -> bool {
        matches!(self, KeyContext::Normal | KeyContext::Prompt)
    }

    /// Parse context from a "when" string
    pub fn from_when_clause(when: &str) -> Option<Self> {
        match when.trim() {
            "global" => Some(KeyContext::Global),
            "prompt" => Some(KeyContext::Prompt),
            "popup" => Some(KeyContext::Popup),
            "fileExplorer" | "file_explorer" => Some(KeyContext::FileExplorer),
            "normal" => Some(KeyContext::Normal),
            "menu" => Some(KeyContext::Menu),
            "terminal" => Some(KeyContext::Terminal),
            _ => None,
        }
    }

    /// Convert context to "when" clause string
    pub fn to_when_clause(self) -> &'static str {
        match self {
            KeyContext::Global => "global",
            KeyContext::Normal => "normal",
            KeyContext::Prompt => "prompt",
            KeyContext::Popup => "popup",
            KeyContext::FileExplorer => "fileExplorer",
            KeyContext::Menu => "menu",
            KeyContext::Terminal => "terminal",
        }
    }
}

/// High-level actions that can be performed in the editor
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
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

    // Block/rectangular selection (column-wise)
    BlockSelectLeft,
    BlockSelectRight,
    BlockSelectUp,
    BlockSelectDown,

    // Editing
    DeleteBackward,
    DeleteForward,
    DeleteWordBackward,
    DeleteWordForward,
    DeleteLine,
    DeleteToLineEnd,
    TransposeChars,
    OpenLine,

    // View
    Recenter,

    // Selection
    SetMark,

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
    SwitchProject,
    New,
    Close,
    CloseTab,
    Quit,
    Revert,
    ToggleAutoRevert,

    // Navigation
    GotoLine,
    GoToMatchingBracket,
    JumpToNextError,
    JumpToPreviousError,

    // Smart editing
    SmartHome,
    IndentSelection,
    DedentSelection,
    ToggleComment,

    // Bookmarks
    SetBookmark(char),
    JumpToBookmark(char),
    ClearBookmark(char),
    ListBookmarks,

    // Search options
    ToggleSearchCaseSensitive,
    ToggleSearchWholeWord,
    ToggleSearchRegex,
    ToggleSearchConfirmEach,

    // Macros
    StartMacroRecording,
    StopMacroRecording,
    PlayMacro(char),
    ToggleMacroRecording(char),
    ShowMacro(char),
    ListMacros,
    PromptRecordMacro,
    PromptPlayMacro,
    PlayLastMacro,

    // Bookmarks (prompt-based)
    PromptSetBookmark,
    PromptJumpToBookmark,

    // Undo/redo
    Undo,
    Redo,

    // View
    ScrollUp,
    ScrollDown,
    ShowHelp,
    ShowKeyboardShortcuts,
    CommandPalette,
    ToggleLineWrap,
    ToggleComposeMode,
    SetComposeWidth,
    SelectTheme,
    SelectKeybindingMap,

    // Buffer/tab navigation
    NextBuffer,
    PrevBuffer,
    SwitchToPreviousTab,
    SwitchToTabByName,

    // Tab scrolling
    ScrollTabsLeft,
    ScrollTabsRight,

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
    ToggleMaximizeSplit,

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
    PromptDeleteToLineEnd,
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

    // File explorer operations
    ToggleFileExplorer,
    FocusFileExplorer,
    FocusEditor,
    FileExplorerUp,
    FileExplorerDown,
    FileExplorerPageUp,
    FileExplorerPageDown,
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
    LspReferences,
    LspRename,
    LspHover,
    LspSignatureHelp,
    LspCodeActions,
    LspRestart,
    LspStop,
    ToggleInlayHints,
    ToggleMouseHover,

    // View toggles
    ToggleLineNumbers,
    ToggleMouseCapture,
    SetBackground,
    SetBackgroundBlend,

    // Config operations
    DumpConfig,

    // Search and replace
    Search,
    FindInSelection,
    FindNext,
    FindPrevious,
    Replace,
    QueryReplace, // Interactive replace (y/n/!/q for each match)

    // Menu navigation
    MenuActivate,     // Open menu bar (Alt or F10)
    MenuClose,        // Close menu (Esc)
    MenuLeft,         // Navigate to previous menu
    MenuRight,        // Navigate to next menu
    MenuUp,           // Navigate to previous item in menu
    MenuDown,         // Navigate to next item in menu
    MenuExecute,      // Execute selected menu item (Enter)
    MenuOpen(String), // Open a specific menu by name (e.g., "File", "Edit")

    // Keybinding map switching
    SwitchKeybindingMap(String), // Switch to a named keybinding map (e.g., "default", "emacs", "vscode")

    // Plugin custom actions
    PluginAction(String),

    // Terminal operations
    OpenTerminal,          // Open a new terminal in the current split
    CloseTerminal,         // Close the current terminal
    FocusTerminal,         // Focus the terminal buffer (if viewing terminal, focus input)
    TerminalEscape,        // Escape from terminal mode back to editor
    ToggleKeyboardCapture, // Toggle keyboard capture mode (all keys go to terminal)
    TerminalPaste,         // Paste clipboard contents into terminal as a single batch

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

            // Block/rectangular selection
            "block_select_left" => Some(Action::BlockSelectLeft),
            "block_select_right" => Some(Action::BlockSelectRight),
            "block_select_up" => Some(Action::BlockSelectUp),
            "block_select_down" => Some(Action::BlockSelectDown),

            "delete_backward" => Some(Action::DeleteBackward),
            "delete_forward" => Some(Action::DeleteForward),
            "delete_word_backward" => Some(Action::DeleteWordBackward),
            "delete_word_forward" => Some(Action::DeleteWordForward),
            "delete_line" => Some(Action::DeleteLine),
            "delete_to_line_end" => Some(Action::DeleteToLineEnd),
            "transpose_chars" => Some(Action::TransposeChars),
            "open_line" => Some(Action::OpenLine),
            "recenter" => Some(Action::Recenter),
            "set_mark" => Some(Action::SetMark),

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
            "switch_project" => Some(Action::SwitchProject),
            "new" => Some(Action::New),
            "close" => Some(Action::Close),
            "close_tab" => Some(Action::CloseTab),
            "quit" => Some(Action::Quit),
            "revert" => Some(Action::Revert),
            "toggle_auto_revert" => Some(Action::ToggleAutoRevert),
            "goto_line" => Some(Action::GotoLine),
            "goto_matching_bracket" => Some(Action::GoToMatchingBracket),
            "jump_to_next_error" => Some(Action::JumpToNextError),
            "jump_to_previous_error" => Some(Action::JumpToPreviousError),

            "smart_home" => Some(Action::SmartHome),
            "indent_selection" => Some(Action::IndentSelection),
            "dedent_selection" => Some(Action::DedentSelection),
            "toggle_comment" => Some(Action::ToggleComment),

            "set_bookmark" => {
                if let Some(serde_json::Value::String(c)) = args.get("char") {
                    c.chars().next().map(Action::SetBookmark)
                } else {
                    None
                }
            }
            "jump_to_bookmark" => {
                if let Some(serde_json::Value::String(c)) = args.get("char") {
                    c.chars().next().map(Action::JumpToBookmark)
                } else {
                    None
                }
            }
            "clear_bookmark" => {
                if let Some(serde_json::Value::String(c)) = args.get("char") {
                    c.chars().next().map(Action::ClearBookmark)
                } else {
                    None
                }
            }
            "list_bookmarks" => Some(Action::ListBookmarks),

            "toggle_search_case_sensitive" => Some(Action::ToggleSearchCaseSensitive),
            "toggle_search_whole_word" => Some(Action::ToggleSearchWholeWord),
            "toggle_search_regex" => Some(Action::ToggleSearchRegex),
            "toggle_search_confirm_each" => Some(Action::ToggleSearchConfirmEach),

            "start_macro_recording" => Some(Action::StartMacroRecording),
            "stop_macro_recording" => Some(Action::StopMacroRecording),
            "play_macro" => {
                if let Some(serde_json::Value::String(c)) = args.get("char") {
                    c.chars().next().map(Action::PlayMacro)
                } else {
                    None
                }
            }
            "toggle_macro_recording" => {
                if let Some(serde_json::Value::String(c)) = args.get("char") {
                    c.chars().next().map(Action::ToggleMacroRecording)
                } else {
                    None
                }
            }
            "show_macro" => {
                if let Some(serde_json::Value::String(c)) = args.get("char") {
                    c.chars().next().map(Action::ShowMacro)
                } else {
                    None
                }
            }
            "list_macros" => Some(Action::ListMacros),
            "prompt_record_macro" => Some(Action::PromptRecordMacro),
            "prompt_play_macro" => Some(Action::PromptPlayMacro),
            "play_last_macro" => Some(Action::PlayLastMacro),
            "prompt_set_bookmark" => Some(Action::PromptSetBookmark),
            "prompt_jump_to_bookmark" => Some(Action::PromptJumpToBookmark),

            "undo" => Some(Action::Undo),
            "redo" => Some(Action::Redo),

            "scroll_up" => Some(Action::ScrollUp),
            "scroll_down" => Some(Action::ScrollDown),
            "show_help" => Some(Action::ShowHelp),
            "keyboard_shortcuts" => Some(Action::ShowKeyboardShortcuts),
            "command_palette" => Some(Action::CommandPalette),
            "toggle_line_wrap" => Some(Action::ToggleLineWrap),
            "toggle_compose_mode" => Some(Action::ToggleComposeMode),
            "set_compose_width" => Some(Action::SetComposeWidth),

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
            "toggle_maximize_split" => Some(Action::ToggleMaximizeSplit),

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
            "prompt_delete_to_line_end" => Some(Action::PromptDeleteToLineEnd),
            "prompt_copy" => Some(Action::PromptCopy),
            "prompt_cut" => Some(Action::PromptCut),
            "prompt_paste" => Some(Action::PromptPaste),
            "prompt_move_left_selecting" => Some(Action::PromptMoveLeftSelecting),
            "prompt_move_right_selecting" => Some(Action::PromptMoveRightSelecting),
            "prompt_move_home_selecting" => Some(Action::PromptMoveHomeSelecting),
            "prompt_move_end_selecting" => Some(Action::PromptMoveEndSelecting),
            "prompt_select_word_left" => Some(Action::PromptSelectWordLeft),
            "prompt_select_word_right" => Some(Action::PromptSelectWordRight),
            "prompt_select_all" => Some(Action::PromptSelectAll),
            "prompt_move_word_left" => Some(Action::PromptMoveWordLeft),
            "prompt_move_word_right" => Some(Action::PromptMoveWordRight),
            "prompt_delete" => Some(Action::PromptDelete),

            "popup_select_next" => Some(Action::PopupSelectNext),
            "popup_select_prev" => Some(Action::PopupSelectPrev),
            "popup_page_up" => Some(Action::PopupPageUp),
            "popup_page_down" => Some(Action::PopupPageDown),
            "popup_confirm" => Some(Action::PopupConfirm),
            "popup_cancel" => Some(Action::PopupCancel),

            "toggle_file_explorer" => Some(Action::ToggleFileExplorer),
            "focus_file_explorer" => Some(Action::FocusFileExplorer),
            "focus_editor" => Some(Action::FocusEditor),
            "file_explorer_up" => Some(Action::FileExplorerUp),
            "file_explorer_down" => Some(Action::FileExplorerDown),
            "file_explorer_page_up" => Some(Action::FileExplorerPageUp),
            "file_explorer_page_down" => Some(Action::FileExplorerPageDown),
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
            "lsp_references" => Some(Action::LspReferences),
            "lsp_rename" => Some(Action::LspRename),
            "lsp_hover" => Some(Action::LspHover),
            "lsp_signature_help" => Some(Action::LspSignatureHelp),
            "lsp_code_actions" => Some(Action::LspCodeActions),
            "lsp_restart" => Some(Action::LspRestart),
            "lsp_stop" => Some(Action::LspStop),
            "toggle_inlay_hints" => Some(Action::ToggleInlayHints),
            "toggle_mouse_hover" => Some(Action::ToggleMouseHover),

            "toggle_line_numbers" => Some(Action::ToggleLineNumbers),
            "toggle_mouse_capture" => Some(Action::ToggleMouseCapture),
            "set_background" => Some(Action::SetBackground),
            "set_background_blend" => Some(Action::SetBackgroundBlend),
            "select_theme" => Some(Action::SelectTheme),
            "select_keybinding_map" => Some(Action::SelectKeybindingMap),

            "dump_config" => Some(Action::DumpConfig),

            "search" => Some(Action::Search),
            "find_in_selection" => Some(Action::FindInSelection),
            "find_next" => Some(Action::FindNext),
            "find_previous" => Some(Action::FindPrevious),
            "replace" => Some(Action::Replace),
            "query_replace" => Some(Action::QueryReplace),

            "menu_activate" => Some(Action::MenuActivate),
            "menu_close" => Some(Action::MenuClose),
            "menu_left" => Some(Action::MenuLeft),
            "menu_right" => Some(Action::MenuRight),
            "menu_up" => Some(Action::MenuUp),
            "menu_down" => Some(Action::MenuDown),
            "menu_execute" => Some(Action::MenuExecute),
            "menu_open" => {
                let name = args.get("name")?.as_str()?;
                Some(Action::MenuOpen(name.to_string()))
            }

            "switch_keybinding_map" => {
                let map_name = args.get("map")?.as_str()?;
                Some(Action::SwitchKeybindingMap(map_name.to_string()))
            }

            // Terminal actions
            "open_terminal" => Some(Action::OpenTerminal),
            "close_terminal" => Some(Action::CloseTerminal),
            "focus_terminal" => Some(Action::FocusTerminal),
            "terminal_escape" => Some(Action::TerminalEscape),
            "toggle_keyboard_capture" => Some(Action::ToggleKeyboardCapture),
            "terminal_paste" => Some(Action::TerminalPaste),

            _ => None,
        }
    }
}

/// Result of chord resolution
#[derive(Debug, Clone, PartialEq)]
pub enum ChordResolution {
    /// Complete match: execute the action
    Complete(Action),
    /// Partial match: continue waiting for more keys in the sequence
    Partial,
    /// No match: the sequence doesn't match any binding
    NoMatch,
}

/// Resolves key events to actions based on configuration
#[derive(Clone)]
pub struct KeybindingResolver {
    /// Map from context to key bindings (single key bindings)
    /// Context-specific bindings have priority over normal bindings
    bindings: HashMap<KeyContext, HashMap<(KeyCode, KeyModifiers), Action>>,

    /// Default bindings for each context (single key bindings)
    default_bindings: HashMap<KeyContext, HashMap<(KeyCode, KeyModifiers), Action>>,

    /// Chord bindings (multi-key sequences)
    /// Maps context -> sequence -> action
    chord_bindings: HashMap<KeyContext, HashMap<Vec<(KeyCode, KeyModifiers)>, Action>>,

    /// Default chord bindings for each context
    default_chord_bindings: HashMap<KeyContext, HashMap<Vec<(KeyCode, KeyModifiers)>, Action>>,
}

impl KeybindingResolver {
    /// Create a new resolver from configuration
    pub fn new(config: &Config) -> Self {
        let mut resolver = Self {
            bindings: HashMap::new(),
            default_bindings: HashMap::new(),
            chord_bindings: HashMap::new(),
            default_chord_bindings: HashMap::new(),
        };

        // Load bindings from the active keymap (with inheritance resolution) into default_bindings
        let map_bindings = config.resolve_keymap(&config.active_keybinding_map);
        resolver.load_default_bindings_from_vec(&map_bindings);

        // Then, load custom keybindings (these override the default map bindings)
        resolver.load_bindings_from_vec(&config.keybindings);

        resolver
    }

    /// Load default bindings from a vector of keybinding definitions (into default_bindings/default_chord_bindings)
    fn load_default_bindings_from_vec(&mut self, bindings: &[crate::config::Keybinding]) {
        for binding in bindings {
            // Determine context from "when" clause
            let context = if let Some(ref when) = binding.when {
                KeyContext::from_when_clause(when).unwrap_or(KeyContext::Normal)
            } else {
                KeyContext::Normal
            };

            if let Some(action) = Action::from_str(&binding.action, &binding.args) {
                // Check if this is a chord binding (has keys field)
                if !binding.keys.is_empty() {
                    // Parse the chord sequence
                    let mut sequence = Vec::new();
                    for key_press in &binding.keys {
                        if let Some(key_code) = Self::parse_key(&key_press.key) {
                            let modifiers = Self::parse_modifiers(&key_press.modifiers);
                            sequence.push((key_code, modifiers));
                        } else {
                            // Invalid key in sequence, skip this binding
                            break;
                        }
                    }

                    // Only add if all keys in sequence were valid
                    if sequence.len() == binding.keys.len() && !sequence.is_empty() {
                        self.default_chord_bindings
                            .entry(context)
                            .or_insert_with(HashMap::new)
                            .insert(sequence, action);
                    }
                } else if let Some(key_code) = Self::parse_key(&binding.key) {
                    // Single key binding (legacy format)
                    let modifiers = Self::parse_modifiers(&binding.modifiers);

                    // Insert the primary binding
                    self.insert_binding_with_equivalents(
                        context,
                        key_code,
                        modifiers,
                        action,
                        &binding.key,
                    );
                }
            }
        }
    }

    /// Insert a binding and automatically add terminal key equivalents.
    /// Logs a warning if an equivalent key is already bound to a different action.
    fn insert_binding_with_equivalents(
        &mut self,
        context: KeyContext,
        key_code: KeyCode,
        modifiers: KeyModifiers,
        action: Action,
        key_name: &str,
    ) {
        let context_bindings = self
            .default_bindings
            .entry(context)
            .or_insert_with(HashMap::new);

        // Insert the primary binding
        context_bindings.insert((key_code, modifiers), action.clone());

        // Get terminal key equivalents and add them as aliases
        let equivalents = terminal_key_equivalents(key_code, modifiers);
        for (equiv_key, equiv_mods) in equivalents {
            // Check if this equivalent is already bound
            if let Some(existing_action) = context_bindings.get(&(equiv_key, equiv_mods)) {
                // Only warn if bound to a DIFFERENT action
                if existing_action != &action {
                    let equiv_name = format!("{:?}", equiv_key);
                    tracing::warn!(
                        "Terminal key equivalent conflict in {:?} context: {} (equivalent of {}) \
                         is bound to {:?}, but {} is bound to {:?}. \
                         The explicit binding takes precedence.",
                        context,
                        equiv_name,
                        key_name,
                        existing_action,
                        key_name,
                        action
                    );
                }
                // Don't override explicit bindings with auto-generated equivalents
            } else {
                // Add the equivalent binding
                context_bindings.insert((equiv_key, equiv_mods), action.clone());
            }
        }
    }

    /// Load custom bindings from a vector of keybinding definitions (into bindings/chord_bindings)
    fn load_bindings_from_vec(&mut self, bindings: &[crate::config::Keybinding]) {
        for binding in bindings {
            // Determine context from "when" clause
            let context = if let Some(ref when) = binding.when {
                KeyContext::from_when_clause(when).unwrap_or(KeyContext::Normal)
            } else {
                KeyContext::Normal
            };

            if let Some(action) = Action::from_str(&binding.action, &binding.args) {
                // Check if this is a chord binding (has keys field)
                if !binding.keys.is_empty() {
                    // Parse the chord sequence
                    let mut sequence = Vec::new();
                    for key_press in &binding.keys {
                        if let Some(key_code) = Self::parse_key(&key_press.key) {
                            let modifiers = Self::parse_modifiers(&key_press.modifiers);
                            sequence.push((key_code, modifiers));
                        } else {
                            // Invalid key in sequence, skip this binding
                            break;
                        }
                    }

                    // Only add if all keys in sequence were valid
                    if sequence.len() == binding.keys.len() && !sequence.is_empty() {
                        self.chord_bindings
                            .entry(context)
                            .or_insert_with(HashMap::new)
                            .insert(sequence, action);
                    }
                } else if let Some(key_code) = Self::parse_key(&binding.key) {
                    // Single key binding (legacy format)
                    let modifiers = Self::parse_modifiers(&binding.modifiers);
                    self.bindings
                        .entry(context)
                        .or_insert_with(HashMap::new)
                        .insert((key_code, modifiers), action);
                }
            }
        }
    }

    /// Check if an action is application-wide (should be accessible in all contexts)
    fn is_application_wide_action(action: &Action) -> bool {
        matches!(
            action,
            Action::Quit
                | Action::Save
                | Action::SaveAs
                | Action::ShowHelp
                | Action::ShowKeyboardShortcuts
                | Action::PromptCancel  // Esc should always cancel
                | Action::PopupCancel // Esc should always cancel
        )
    }

    /// Check if an action is a UI action that should work in terminal mode
    /// (without keyboard capture). These are general navigation and UI actions
    /// that don't involve text editing.
    pub fn is_terminal_ui_action(action: &Action) -> bool {
        matches!(
            action,
            // Global UI actions
            Action::CommandPalette
                | Action::MenuActivate
                | Action::MenuOpen(_)
                | Action::ShowHelp
                | Action::ShowKeyboardShortcuts
                | Action::Quit
                // Split navigation
                | Action::NextSplit
                | Action::PrevSplit
                | Action::SplitHorizontal
                | Action::SplitVertical
                | Action::CloseSplit
                | Action::ToggleMaximizeSplit
                // Tab/buffer navigation
                | Action::NextBuffer
                | Action::PrevBuffer
                | Action::Close
                | Action::ScrollTabsLeft
                | Action::ScrollTabsRight
                // Terminal control
                | Action::TerminalEscape
                | Action::ToggleKeyboardCapture
                | Action::OpenTerminal
                | Action::CloseTerminal
                | Action::TerminalPaste
                // File explorer
                | Action::ToggleFileExplorer
        )
    }

    /// Resolve a key event with chord state to check for multi-key sequences
    /// Returns:
    /// - Complete(action): The sequence is complete, execute the action
    /// - Partial: The sequence is partial (prefix of a chord), wait for more keys
    /// - NoMatch: The sequence doesn't match any chord binding
    pub fn resolve_chord(
        &self,
        chord_state: &[(KeyCode, KeyModifiers)],
        event: &KeyEvent,
        context: KeyContext,
    ) -> ChordResolution {
        // Build the full sequence: existing chord state + new key
        let mut full_sequence = chord_state.to_vec();
        full_sequence.push((event.code, event.modifiers));

        tracing::trace!(
            "KeybindingResolver.resolve_chord: sequence={:?}, context={:?}",
            full_sequence,
            context
        );

        // Check all chord binding sources in priority order
        let search_order = vec![
            (&self.chord_bindings, &KeyContext::Global, "custom global"),
            (
                &self.default_chord_bindings,
                &KeyContext::Global,
                "default global",
            ),
            (&self.chord_bindings, &context, "custom context"),
            (&self.default_chord_bindings, &context, "default context"),
        ];

        let mut has_partial_match = false;

        for (binding_map, bind_context, label) in search_order {
            if let Some(context_chords) = binding_map.get(bind_context) {
                // Check for exact match
                if let Some(action) = context_chords.get(&full_sequence) {
                    tracing::trace!("  -> Complete chord match in {}: {:?}", label, action);
                    return ChordResolution::Complete(action.clone());
                }

                // Check for partial match (our sequence is a prefix of any binding)
                for (chord_seq, _) in context_chords.iter() {
                    if chord_seq.len() > full_sequence.len()
                        && chord_seq[..full_sequence.len()] == full_sequence[..]
                    {
                        tracing::trace!("  -> Partial chord match in {}", label);
                        has_partial_match = true;
                        break;
                    }
                }
            }
        }

        if has_partial_match {
            ChordResolution::Partial
        } else {
            tracing::trace!("  -> No chord match");
            ChordResolution::NoMatch
        }
    }

    /// Resolve a key event to an action in the given context
    pub fn resolve(&self, event: &KeyEvent, context: KeyContext) -> Action {
        tracing::trace!(
            "KeybindingResolver.resolve: code={:?}, modifiers={:?}, context={:?}",
            event.code,
            event.modifiers,
            context
        );

        // Check Global bindings first (highest priority - work in all contexts)
        if let Some(global_bindings) = self.bindings.get(&KeyContext::Global) {
            if let Some(action) = global_bindings.get(&(event.code, event.modifiers)) {
                tracing::trace!("  -> Found in custom global bindings: {:?}", action);
                return action.clone();
            }
        }

        if let Some(global_bindings) = self.default_bindings.get(&KeyContext::Global) {
            if let Some(action) = global_bindings.get(&(event.code, event.modifiers)) {
                tracing::trace!("  -> Found in default global bindings: {:?}", action);
                return action.clone();
            }
        }

        // Try context-specific custom bindings
        if let Some(context_bindings) = self.bindings.get(&context) {
            if let Some(action) = context_bindings.get(&(event.code, event.modifiers)) {
                tracing::trace!(
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
                tracing::trace!(
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
                        tracing::trace!(
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
                        tracing::trace!(
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
                    tracing::trace!("  -> Character input: '{}'", c);
                    return Action::InsertChar(c);
                }
            }
        }

        tracing::trace!("  -> No binding found, returning Action::None");
        Action::None
    }

    /// Resolve a key event to a UI action for terminal mode.
    /// Only returns actions that are classified as UI actions (is_terminal_ui_action).
    /// Returns Action::None if the key doesn't map to a UI action.
    pub fn resolve_terminal_ui_action(&self, event: &KeyEvent) -> Action {
        tracing::trace!(
            "KeybindingResolver.resolve_terminal_ui_action: code={:?}, modifiers={:?}",
            event.code,
            event.modifiers
        );

        // Check Terminal context bindings first (highest priority for terminal mode)
        for bindings in [&self.bindings, &self.default_bindings] {
            if let Some(terminal_bindings) = bindings.get(&KeyContext::Terminal) {
                if let Some(action) = terminal_bindings.get(&(event.code, event.modifiers)) {
                    if Self::is_terminal_ui_action(action) {
                        tracing::trace!("  -> Found UI action in terminal bindings: {:?}", action);
                        return action.clone();
                    }
                }
            }
        }

        // Check Global bindings (work in all contexts)
        for bindings in [&self.bindings, &self.default_bindings] {
            if let Some(global_bindings) = bindings.get(&KeyContext::Global) {
                if let Some(action) = global_bindings.get(&(event.code, event.modifiers)) {
                    if Self::is_terminal_ui_action(action) {
                        tracing::trace!("  -> Found UI action in global bindings: {:?}", action);
                        return action.clone();
                    }
                }
            }
        }

        // Check Normal context bindings (for actions like next_split that are in Normal context)
        for bindings in [&self.bindings, &self.default_bindings] {
            if let Some(normal_bindings) = bindings.get(&KeyContext::Normal) {
                if let Some(action) = normal_bindings.get(&(event.code, event.modifiers)) {
                    if Self::is_terminal_ui_action(action) {
                        tracing::trace!("  -> Found UI action in normal bindings: {:?}", action);
                        return action.clone();
                    }
                }
            }
        }

        tracing::trace!("  -> No UI action found");
        Action::None
    }

    /// Find the primary keybinding for a given action (for display in menus)
    /// Returns a formatted string like "Ctrl+S" or "F12"
    pub fn find_keybinding_for_action(
        &self,
        action_name: &str,
        context: KeyContext,
    ) -> Option<String> {
        // Parse the action from the action name
        let target_action = Action::from_str(action_name, &HashMap::new())?;

        // Search in custom bindings first, then default bindings
        let search_maps = vec![
            self.bindings.get(&context),
            self.bindings.get(&KeyContext::Global),
            self.default_bindings.get(&context),
            self.default_bindings.get(&KeyContext::Global),
        ];

        for map in search_maps.into_iter().flatten() {
            // Collect all matching keybindings for deterministic selection
            let mut matches: Vec<(KeyCode, KeyModifiers)> = map
                .iter()
                .filter(|(_, action)| {
                    std::mem::discriminant(*action) == std::mem::discriminant(&target_action)
                })
                .map(|((key_code, modifiers), _)| (*key_code, *modifiers))
                .collect();

            if !matches.is_empty() {
                // Sort to get deterministic order: prefer fewer modifiers, then by key
                matches.sort_by(|(key_a, mod_a), (key_b, mod_b)| {
                    // Compare by number of modifiers first (prefer simpler bindings)
                    let mod_count_a = mod_a.bits().count_ones();
                    let mod_count_b = mod_b.bits().count_ones();
                    match mod_count_a.cmp(&mod_count_b) {
                        std::cmp::Ordering::Equal => {
                            // Then by modifier bits (for consistent ordering)
                            match mod_a.bits().cmp(&mod_b.bits()) {
                                std::cmp::Ordering::Equal => {
                                    // Finally by key code
                                    Self::key_code_sort_key(key_a)
                                        .cmp(&Self::key_code_sort_key(key_b))
                                }
                                other => other,
                            }
                        }
                        other => other,
                    }
                });

                let (key_code, modifiers) = matches[0];
                return Some(Self::format_keybinding(key_code, modifiers));
            }
        }

        None
    }

    /// Generate a sort key for KeyCode to ensure deterministic ordering
    fn key_code_sort_key(key_code: &KeyCode) -> (u8, u32) {
        match key_code {
            KeyCode::Char(c) => (0, *c as u32),
            KeyCode::F(n) => (1, *n as u32),
            KeyCode::Enter => (2, 0),
            KeyCode::Tab => (2, 1),
            KeyCode::Backspace => (2, 2),
            KeyCode::Delete => (2, 3),
            KeyCode::Esc => (2, 4),
            KeyCode::Left => (3, 0),
            KeyCode::Right => (3, 1),
            KeyCode::Up => (3, 2),
            KeyCode::Down => (3, 3),
            KeyCode::Home => (3, 4),
            KeyCode::End => (3, 5),
            KeyCode::PageUp => (3, 6),
            KeyCode::PageDown => (3, 7),
            _ => (255, 0),
        }
    }

    /// Find the mnemonic character for a menu (based on Alt+letter keybindings)
    /// Returns the character that should be underlined in the menu label
    pub fn find_menu_mnemonic(&self, menu_name: &str) -> Option<char> {
        // Search in custom bindings first, then default bindings
        let search_maps = vec![
            self.bindings.get(&KeyContext::Normal),
            self.bindings.get(&KeyContext::Global),
            self.default_bindings.get(&KeyContext::Normal),
            self.default_bindings.get(&KeyContext::Global),
        ];

        for map in search_maps.into_iter().flatten() {
            for ((key_code, modifiers), action) in map {
                // Check if this is an Alt+letter binding for MenuOpen with matching name
                if let Action::MenuOpen(name) = action {
                    if name.eq_ignore_ascii_case(menu_name) && *modifiers == KeyModifiers::ALT {
                        // Return the character for Alt+letter bindings
                        if let KeyCode::Char(c) = key_code {
                            return Some(c.to_ascii_lowercase());
                        }
                    }
                }
            }
        }

        None
    }

    /// Format a keybinding for display (e.g., "Ctrl+S", "Alt+Enter", "F12")
    fn format_keybinding(key_code: KeyCode, modifiers: KeyModifiers) -> String {
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

        // Format the key
        let key_str = match key_code {
            KeyCode::Char(c) if c == ' ' => "Space".to_string(),
            KeyCode::Char(c) => c.to_uppercase().to_string(),
            KeyCode::Enter => "Enter".to_string(),
            KeyCode::Backspace => "Backspace".to_string(),
            KeyCode::Delete => "Delete".to_string(),
            KeyCode::Tab => "Tab".to_string(),
            KeyCode::Esc => "Esc".to_string(),
            KeyCode::Left => "Left".to_string(),
            KeyCode::Right => "Right".to_string(),
            KeyCode::Up => "Up".to_string(),
            KeyCode::Down => "Down".to_string(),
            KeyCode::Home => "Home".to_string(),
            KeyCode::End => "End".to_string(),
            KeyCode::PageUp => "PageUp".to_string(),
            KeyCode::PageDown => "PageDown".to_string(),
            KeyCode::F(n) => format!("F{}", n),
            _ => return String::new(),
        };

        parts.push(&key_str);
        parts.join("+")
    }

    /// Parse a key string to KeyCode
    fn parse_key(key: &str) -> Option<KeyCode> {
        let lower = key.to_lowercase();
        match lower.as_str() {
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
            // Handle function keys like "f1", "f2", ..., "f12"
            s if s.starts_with('f') && s.len() >= 2 => s[1..].parse::<u8>().ok().map(KeyCode::F),
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

    /// Get all keybindings (for help display)
    /// Returns a Vec of (key_description, action_description)
    pub fn get_all_bindings(&self) -> Vec<(String, String)> {
        let mut bindings = Vec::new();

        // Collect all bindings from all contexts
        for context in &[
            KeyContext::Normal,
            KeyContext::Prompt,
            KeyContext::Popup,
            KeyContext::FileExplorer,
            KeyContext::Menu,
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
            Action::BlockSelectLeft => "Block select left".to_string(),
            Action::BlockSelectRight => "Block select right".to_string(),
            Action::BlockSelectUp => "Block select up".to_string(),
            Action::BlockSelectDown => "Block select down".to_string(),
            Action::DeleteBackward => "Delete backward".to_string(),
            Action::DeleteForward => "Delete forward".to_string(),
            Action::DeleteWordBackward => "Delete word backward".to_string(),
            Action::DeleteWordForward => "Delete word forward".to_string(),
            Action::DeleteLine => "Delete line".to_string(),
            Action::DeleteToLineEnd => "Delete to end of line".to_string(),
            Action::TransposeChars => "Transpose characters".to_string(),
            Action::OpenLine => "Open line below".to_string(),
            Action::Recenter => "Recenter view on cursor".to_string(),
            Action::SetMark => "Set mark (start selection)".to_string(),
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
            Action::SwitchProject => "Switch project".to_string(),
            Action::New => "New file".to_string(),
            Action::Close => "Close file".to_string(),
            Action::CloseTab => "Close tab".to_string(),
            Action::Quit => "Quit editor".to_string(),
            Action::Revert => "Revert to saved file".to_string(),
            Action::ToggleAutoRevert => "Toggle auto-revert mode".to_string(),
            Action::GotoLine => "Go to line number".to_string(),
            Action::GoToMatchingBracket => "Go to matching bracket".to_string(),
            Action::JumpToNextError => "Jump to next error/diagnostic".to_string(),
            Action::JumpToPreviousError => "Jump to previous error/diagnostic".to_string(),
            Action::SmartHome => {
                "Smart home (toggle line start / first non-whitespace)".to_string()
            }
            Action::IndentSelection => "Indent selection".to_string(),
            Action::DedentSelection => "Dedent selection".to_string(),
            Action::ToggleComment => "Toggle comment".to_string(),
            Action::SetBookmark(c) => format!("Set bookmark '{}'", c),
            Action::JumpToBookmark(c) => format!("Jump to bookmark '{}'", c),
            Action::ClearBookmark(c) => format!("Clear bookmark '{}'", c),
            Action::ListBookmarks => "List all bookmarks".to_string(),
            Action::ToggleSearchCaseSensitive => "Toggle search case sensitivity".to_string(),
            Action::ToggleSearchWholeWord => "Toggle search whole word matching".to_string(),
            Action::ToggleSearchRegex => "Toggle search regex mode".to_string(),
            Action::ToggleSearchConfirmEach => "Toggle confirm each replacement".to_string(),
            Action::StartMacroRecording => "Start macro recording".to_string(),
            Action::StopMacroRecording => "Stop macro recording".to_string(),
            Action::PlayMacro(c) => format!("Play macro '{}'", c),
            Action::ToggleMacroRecording(c) => format!("Toggle macro recording for '{}'", c),
            Action::ShowMacro(c) => format!("Show macro '{}' in buffer", c),
            Action::ListMacros => "List all recorded macros".to_string(),
            Action::PromptRecordMacro => "Record macro (prompts for register)".to_string(),
            Action::PromptPlayMacro => "Play macro (prompts for register)".to_string(),
            Action::PlayLastMacro => "Play last recorded macro".to_string(),
            Action::PromptSetBookmark => "Set bookmark (prompts for register)".to_string(),
            Action::PromptJumpToBookmark => "Jump to bookmark (prompts for register)".to_string(),
            Action::Undo => "Undo".to_string(),
            Action::Redo => "Redo".to_string(),
            Action::ScrollUp => "Scroll up".to_string(),
            Action::ScrollDown => "Scroll down".to_string(),
            Action::ShowHelp => "Show manual".to_string(),
            Action::ShowKeyboardShortcuts => "Show keyboard shortcuts".to_string(),
            Action::CommandPalette => "Command palette".to_string(),
            Action::ToggleLineWrap => "Toggle line wrap".to_string(),
            Action::ToggleComposeMode => "Toggle compose mode".to_string(),
            Action::SetComposeWidth => "Set compose width".to_string(),
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
            Action::ToggleMaximizeSplit => "Toggle maximize split".to_string(),
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
            Action::PromptDeleteToLineEnd => "Prompt delete to end of line".to_string(),
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
            Action::ToggleFileExplorer => "Toggle file explorer".to_string(),
            Action::FocusFileExplorer => "Focus file explorer".to_string(),
            Action::FocusEditor => "Focus editor".to_string(),
            Action::FileExplorerUp => "File explorer: navigate up".to_string(),
            Action::FileExplorerDown => "File explorer: navigate down".to_string(),
            Action::FileExplorerPageUp => "File explorer: page up".to_string(),
            Action::FileExplorerPageDown => "File explorer: page down".to_string(),
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
            Action::LspReferences => "LSP: Find references".to_string(),
            Action::LspRename => "LSP: Rename symbol".to_string(),
            Action::LspHover => "LSP: Show hover documentation".to_string(),
            Action::LspSignatureHelp => "LSP: Show signature help".to_string(),
            Action::LspCodeActions => "LSP: Show code actions".to_string(),
            Action::LspRestart => "LSP: Start/restart server for current language".to_string(),
            Action::LspStop => "LSP: Stop a running server".to_string(),
            Action::ToggleInlayHints => "Toggle inlay hints".to_string(),
            Action::ToggleMouseHover => "Toggle LSP hover on mouse".to_string(),
            Action::ToggleLineNumbers => "Toggle line numbers".to_string(),
            Action::ToggleMouseCapture => "Toggle mouse support".to_string(),
            Action::SetBackground => "Set ANSI background file".to_string(),
            Action::SetBackgroundBlend => "Set background blend ratio".to_string(),
            Action::DumpConfig => "Dump config to file".to_string(),
            Action::Search => "Search for text in buffer".to_string(),
            Action::FindInSelection => "Search within selection".to_string(),
            Action::FindNext => "Find next search match".to_string(),
            Action::FindPrevious => "Find previous search match".to_string(),
            Action::Replace => "Replace text in buffer".to_string(),
            Action::QueryReplace => "Interactive replace (y/n/!/q for each match)".to_string(),
            Action::MenuActivate => "Activate menu bar".to_string(),
            Action::MenuClose => "Close menu".to_string(),
            Action::MenuLeft => "Navigate to previous menu".to_string(),
            Action::MenuRight => "Navigate to next menu".to_string(),
            Action::MenuUp => "Navigate to previous menu item".to_string(),
            Action::MenuDown => "Navigate to next menu item".to_string(),
            Action::MenuExecute => "Execute selected menu item".to_string(),
            Action::MenuOpen(name) => format!("Open {} menu", name),
            Action::SwitchKeybindingMap(map) => format!("Switch to '{}' keybindings", map),
            Action::PluginAction(name) => format!("Plugin action: {}", name),
            Action::ScrollTabsLeft => "Scroll tabs left".to_string(),
            Action::ScrollTabsRight => "Scroll tabs right".to_string(),
            Action::SelectTheme => "Select theme".to_string(),
            Action::SelectKeybindingMap => "Select keybinding map".to_string(),
            Action::SwitchToPreviousTab => "Switch to previous tab".to_string(),
            Action::SwitchToTabByName => "Switch to tab by name".to_string(),
            Action::OpenTerminal => "Open terminal".to_string(),
            Action::CloseTerminal => "Close terminal".to_string(),
            Action::FocusTerminal => "Focus terminal".to_string(),
            Action::TerminalEscape => "Exit terminal mode".to_string(),
            Action::ToggleKeyboardCapture => "Toggle keyboard capture (terminal)".to_string(),
            Action::TerminalPaste => "Paste into terminal".to_string(),
            Action::None => "No action".to_string(),
        }
    }

    /// Get the keybinding string for an action in a specific context
    /// Returns the first keybinding found (prioritizing custom bindings over defaults)
    /// When multiple keybindings exist for the same action, prefers canonical keys over
    /// terminal equivalents (e.g., "Space" over "@")
    /// Returns None if no binding is found
    pub fn get_keybinding_for_action(
        &self,
        action: &Action,
        context: KeyContext,
    ) -> Option<String> {
        // Helper to collect all matching keybindings from a map and pick the best one
        fn find_best_keybinding(
            bindings: &HashMap<(KeyCode, KeyModifiers), Action>,
            action: &Action,
        ) -> Option<(KeyCode, KeyModifiers)> {
            let matches: Vec<_> = bindings
                .iter()
                .filter(|(_, a)| *a == action)
                .map(|((k, m), _)| (*k, *m))
                .collect();

            if matches.is_empty() {
                return None;
            }

            // Sort to prefer canonical keys over terminal equivalents
            // Terminal equivalents like '@' (for space), '7' (for '/'), etc. should be deprioritized
            let mut sorted = matches;
            sorted.sort_by(|(k1, m1), (k2, m2)| {
                let score1 = keybinding_priority_score(k1);
                let score2 = keybinding_priority_score(k2);
                // Lower score = higher priority
                match score1.cmp(&score2) {
                    std::cmp::Ordering::Equal => {
                        // Tie-break by formatted string for full determinism
                        let s1 = format_keybinding(k1, m1);
                        let s2 = format_keybinding(k2, m2);
                        s1.cmp(&s2)
                    }
                    other => other,
                }
            });

            sorted.into_iter().next()
        }

        // Check custom bindings first (higher priority)
        if let Some(context_bindings) = self.bindings.get(&context) {
            if let Some((keycode, modifiers)) = find_best_keybinding(context_bindings, action) {
                return Some(format_keybinding(&keycode, &modifiers));
            }
        }

        // Check default bindings for this context
        if let Some(context_bindings) = self.default_bindings.get(&context) {
            if let Some((keycode, modifiers)) = find_best_keybinding(context_bindings, action) {
                return Some(format_keybinding(&keycode, &modifiers));
            }
        }

        // For certain contexts, also check Normal context for application-wide actions
        if context != KeyContext::Normal && Self::is_application_wide_action(action) {
            // Check custom normal bindings
            if let Some(normal_bindings) = self.bindings.get(&KeyContext::Normal) {
                if let Some((keycode, modifiers)) = find_best_keybinding(normal_bindings, action) {
                    return Some(format_keybinding(&keycode, &modifiers));
                }
            }

            // Check default normal bindings
            if let Some(normal_bindings) = self.default_bindings.get(&KeyContext::Normal) {
                if let Some((keycode, modifiers)) = find_best_keybinding(normal_bindings, action) {
                    return Some(format_keybinding(&keycode, &modifiers));
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
            Action::from_str("keyboard_shortcuts", &args),
            Some(Action::ShowKeyboardShortcuts)
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
        assert_eq!(
            KeyContext::from_when_clause("prompt"),
            Some(KeyContext::Prompt)
        );
        assert_eq!(
            KeyContext::from_when_clause("popup"),
            Some(KeyContext::Popup)
        );
        assert_eq!(KeyContext::from_when_clause("help"), None);
        assert_eq!(KeyContext::from_when_clause("  help  "), None); // Test trimming
        assert_eq!(KeyContext::from_when_clause("unknown"), None);
        assert_eq!(KeyContext::from_when_clause(""), None);
    }

    #[test]
    fn test_key_context_to_when_clause() {
        assert_eq!(KeyContext::Normal.to_when_clause(), "normal");
        assert_eq!(KeyContext::Prompt.to_when_clause(), "prompt");
        assert_eq!(KeyContext::Popup.to_when_clause(), "popup");
    }

    #[test]
    fn test_context_specific_bindings() {
        let config = Config::default();
        let resolver = KeybindingResolver::new(&config);

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
            keys: vec![],
            action: "quit".to_string(), // Override Esc in popup context to quit
            args: HashMap::new(),
            when: Some("popup".to_string()),
        });

        let resolver = KeybindingResolver::new(&config);
        let esc_event = KeyEvent::new(KeyCode::Esc, KeyModifiers::empty());

        // In popup context, custom binding should override default PopupCancel
        assert_eq!(
            resolver.resolve(&esc_event, KeyContext::Popup),
            Action::Quit
        );

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

        // But not in Popup contexts (returns None)
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
            keys: vec![],
            action: "command_palette".to_string(),
            args: HashMap::new(),
            when: None, // Default to normal context
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
            Action::PromptDeleteToLineEnd
        );
        assert_eq!(
            resolver.resolve(&ctrl_k, KeyContext::Normal),
            Action::DeleteToLineEnd
        );
    }

    #[test]
    fn test_all_context_default_bindings_exist() {
        let config = Config::default();
        let resolver = KeybindingResolver::new(&config);

        // Verify that default bindings exist for all contexts
        assert!(resolver.default_bindings.contains_key(&KeyContext::Normal));
        assert!(resolver.default_bindings.contains_key(&KeyContext::Prompt));
        assert!(resolver.default_bindings.contains_key(&KeyContext::Popup));
        assert!(resolver
            .default_bindings
            .contains_key(&KeyContext::FileExplorer));
        assert!(resolver.default_bindings.contains_key(&KeyContext::Menu));

        // Verify each context has some bindings
        assert!(!resolver.default_bindings[&KeyContext::Normal].is_empty());
        assert!(!resolver.default_bindings[&KeyContext::Prompt].is_empty());
        assert!(!resolver.default_bindings[&KeyContext::Popup].is_empty());
        assert!(!resolver.default_bindings[&KeyContext::FileExplorer].is_empty());
        assert!(!resolver.default_bindings[&KeyContext::Menu].is_empty());
    }

    #[test]
    fn test_resolve_determinism() {
        // Property: Resolving the same key in the same context should always return the same action
        let config = Config::default();
        let resolver = KeybindingResolver::new(&config);

        let test_cases = vec![
            (KeyCode::Left, KeyModifiers::empty(), KeyContext::Normal),
            (
                KeyCode::Esc,
                KeyModifiers::empty(),
                KeyContext::FileExplorer,
            ),
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

    #[test]
    fn test_scroll_keybindings() {
        let config = Config::default();
        let resolver = KeybindingResolver::new(&config);

        // Test Ctrl+Up -> ScrollUp
        let ctrl_up = KeyEvent::new(KeyCode::Up, KeyModifiers::CONTROL);
        assert_eq!(
            resolver.resolve(&ctrl_up, KeyContext::Normal),
            Action::ScrollUp,
            "Ctrl+Up should resolve to ScrollUp"
        );

        // Test Ctrl+Down -> ScrollDown
        let ctrl_down = KeyEvent::new(KeyCode::Down, KeyModifiers::CONTROL);
        assert_eq!(
            resolver.resolve(&ctrl_down, KeyContext::Normal),
            Action::ScrollDown,
            "Ctrl+Down should resolve to ScrollDown"
        );
    }

    #[test]
    fn test_lsp_completion_keybinding() {
        let config = Config::default();
        let resolver = KeybindingResolver::new(&config);

        // Test Ctrl+Space -> LspCompletion
        let ctrl_space = KeyEvent::new(KeyCode::Char(' '), KeyModifiers::CONTROL);
        assert_eq!(
            resolver.resolve(&ctrl_space, KeyContext::Normal),
            Action::LspCompletion,
            "Ctrl+Space should resolve to LspCompletion"
        );
    }

    #[test]
    fn test_terminal_key_equivalents() {
        // Test that terminal_key_equivalents returns correct mappings
        let ctrl = KeyModifiers::CONTROL;

        // Ctrl+/ <-> Ctrl+7
        let slash_equivs = terminal_key_equivalents(KeyCode::Char('/'), ctrl);
        assert_eq!(slash_equivs, vec![(KeyCode::Char('7'), ctrl)]);

        let seven_equivs = terminal_key_equivalents(KeyCode::Char('7'), ctrl);
        assert_eq!(seven_equivs, vec![(KeyCode::Char('/'), ctrl)]);

        // Ctrl+Backspace <-> Ctrl+H
        let backspace_equivs = terminal_key_equivalents(KeyCode::Backspace, ctrl);
        assert_eq!(backspace_equivs, vec![(KeyCode::Char('h'), ctrl)]);

        let h_equivs = terminal_key_equivalents(KeyCode::Char('h'), ctrl);
        assert_eq!(h_equivs, vec![(KeyCode::Backspace, ctrl)]);

        // No equivalents for regular keys
        let a_equivs = terminal_key_equivalents(KeyCode::Char('a'), ctrl);
        assert!(a_equivs.is_empty());

        // No equivalents without Ctrl
        let slash_no_ctrl = terminal_key_equivalents(KeyCode::Char('/'), KeyModifiers::empty());
        assert!(slash_no_ctrl.is_empty());
    }

    #[test]
    fn test_terminal_key_equivalents_auto_binding() {
        let config = Config::default();
        let resolver = KeybindingResolver::new(&config);

        // Ctrl+/ should be bound to toggle_comment
        let ctrl_slash = KeyEvent::new(KeyCode::Char('/'), KeyModifiers::CONTROL);
        let action_slash = resolver.resolve(&ctrl_slash, KeyContext::Normal);
        assert_eq!(
            action_slash,
            Action::ToggleComment,
            "Ctrl+/ should resolve to ToggleComment"
        );

        // Ctrl+7 should also be bound to toggle_comment (auto-generated equivalent)
        let ctrl_7 = KeyEvent::new(KeyCode::Char('7'), KeyModifiers::CONTROL);
        let action_7 = resolver.resolve(&ctrl_7, KeyContext::Normal);
        assert_eq!(
            action_7,
            Action::ToggleComment,
            "Ctrl+7 should resolve to ToggleComment (terminal equivalent of Ctrl+/)"
        );
    }

    #[test]
    fn test_terminal_key_equivalents_normalization() {
        // This test verifies that all terminal key equivalents are correctly mapped
        // These mappings exist because terminals send different key codes for certain
        // key combinations due to historical terminal emulation reasons.

        let ctrl = KeyModifiers::CONTROL;

        // === Ctrl+/ <-> Ctrl+7 ===
        // Most terminals send Ctrl+7 (0x1F) when user presses Ctrl+/
        let slash_equivs = terminal_key_equivalents(KeyCode::Char('/'), ctrl);
        assert_eq!(
            slash_equivs,
            vec![(KeyCode::Char('7'), ctrl)],
            "Ctrl+/ should map to Ctrl+7"
        );
        let seven_equivs = terminal_key_equivalents(KeyCode::Char('7'), ctrl);
        assert_eq!(
            seven_equivs,
            vec![(KeyCode::Char('/'), ctrl)],
            "Ctrl+7 should map back to Ctrl+/"
        );

        // === Ctrl+Backspace <-> Ctrl+H ===
        // Many terminals send Ctrl+H (0x08, ASCII backspace) for Ctrl+Backspace
        let backspace_equivs = terminal_key_equivalents(KeyCode::Backspace, ctrl);
        assert_eq!(
            backspace_equivs,
            vec![(KeyCode::Char('h'), ctrl)],
            "Ctrl+Backspace should map to Ctrl+H"
        );
        let h_equivs = terminal_key_equivalents(KeyCode::Char('h'), ctrl);
        assert_eq!(
            h_equivs,
            vec![(KeyCode::Backspace, ctrl)],
            "Ctrl+H should map back to Ctrl+Backspace"
        );

        // === Ctrl+Space <-> Ctrl+@ ===
        // Ctrl+Space sends NUL (0x00), same as Ctrl+@
        let space_equivs = terminal_key_equivalents(KeyCode::Char(' '), ctrl);
        assert_eq!(
            space_equivs,
            vec![(KeyCode::Char('@'), ctrl)],
            "Ctrl+Space should map to Ctrl+@"
        );
        let at_equivs = terminal_key_equivalents(KeyCode::Char('@'), ctrl);
        assert_eq!(
            at_equivs,
            vec![(KeyCode::Char(' '), ctrl)],
            "Ctrl+@ should map back to Ctrl+Space"
        );

        // === Ctrl+- <-> Ctrl+_ ===
        // Ctrl+- and Ctrl+_ both send 0x1F in some terminals
        let minus_equivs = terminal_key_equivalents(KeyCode::Char('-'), ctrl);
        assert_eq!(
            minus_equivs,
            vec![(KeyCode::Char('_'), ctrl)],
            "Ctrl+- should map to Ctrl+_"
        );
        let underscore_equivs = terminal_key_equivalents(KeyCode::Char('_'), ctrl);
        assert_eq!(
            underscore_equivs,
            vec![(KeyCode::Char('-'), ctrl)],
            "Ctrl+_ should map back to Ctrl+-"
        );

        // === No equivalents for regular keys ===
        assert!(
            terminal_key_equivalents(KeyCode::Char('a'), ctrl).is_empty(),
            "Ctrl+A should have no terminal equivalents"
        );
        assert!(
            terminal_key_equivalents(KeyCode::Char('z'), ctrl).is_empty(),
            "Ctrl+Z should have no terminal equivalents"
        );
        assert!(
            terminal_key_equivalents(KeyCode::Enter, ctrl).is_empty(),
            "Ctrl+Enter should have no terminal equivalents"
        );

        // === No equivalents without Ctrl modifier ===
        assert!(
            terminal_key_equivalents(KeyCode::Char('/'), KeyModifiers::empty()).is_empty(),
            "/ without Ctrl should have no equivalents"
        );
        assert!(
            terminal_key_equivalents(KeyCode::Char('7'), KeyModifiers::SHIFT).is_empty(),
            "Shift+7 should have no equivalents"
        );
        assert!(
            terminal_key_equivalents(KeyCode::Char('h'), KeyModifiers::ALT).is_empty(),
            "Alt+H should have no equivalents"
        );

        // === Ctrl+H only maps to Backspace when ONLY Ctrl is pressed ===
        // Ctrl+Shift+H or Ctrl+Alt+H should NOT map to Backspace
        let ctrl_shift = KeyModifiers::CONTROL | KeyModifiers::SHIFT;
        let ctrl_shift_h_equivs = terminal_key_equivalents(KeyCode::Char('h'), ctrl_shift);
        assert!(
            ctrl_shift_h_equivs.is_empty(),
            "Ctrl+Shift+H should NOT map to Ctrl+Shift+Backspace"
        );
    }
}
