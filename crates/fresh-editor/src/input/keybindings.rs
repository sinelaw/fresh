use crate::config::Config;
use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use rust_i18n::t;
use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, Ordering};

/// Global flag to force Linux-style keybinding display (Alt/Shift instead of ⌥/⇧)
/// This is primarily used in tests to ensure consistent output across platforms.
static FORCE_LINUX_KEYBINDINGS: AtomicBool = AtomicBool::new(false);

/// Force Linux-style keybinding display (Alt/Shift instead of ⌥/⇧)
/// Call this in tests to ensure consistent output regardless of platform.
pub fn set_force_linux_keybindings(force: bool) {
    FORCE_LINUX_KEYBINDINGS.store(force, Ordering::SeqCst);
}

/// Check if we should use macOS-style symbols for Alt and Shift keybindings
fn use_macos_symbols() -> bool {
    if FORCE_LINUX_KEYBINDINGS.load(Ordering::SeqCst) {
        return false;
    }
    cfg!(target_os = "macos")
}

/// Check if the given modifiers allow text input (character insertion).
///
/// Returns true for:
/// - No modifiers
/// - Shift only (for uppercase letters, symbols)
/// - Ctrl+Alt on Windows (AltGr key, used for special characters on international keyboards)
///
/// On Windows, the AltGr key is reported as Ctrl+Alt by crossterm, which is needed for
/// typing characters like @, [, ], {, }, etc. on German, French, and other keyboard layouts.
/// See: https://github.com/crossterm-rs/crossterm/issues/820
fn is_text_input_modifier(modifiers: KeyModifiers) -> bool {
    if modifiers.is_empty() || modifiers == KeyModifiers::SHIFT {
        return true;
    }

    // Windows: AltGr is reported as Ctrl+Alt by crossterm
    #[cfg(windows)]
    if modifiers == (KeyModifiers::CONTROL | KeyModifiers::ALT) {
        return true;
    }

    false
}

/// Format a keybinding as a user-friendly string
/// On macOS, uses native symbols: ⌃ (Control), ⌥ (Option), ⇧ (Shift) without separators
/// On other platforms, uses "Ctrl+Alt+Shift+" format
pub fn format_keybinding(keycode: &KeyCode, modifiers: &KeyModifiers) -> String {
    let mut result = String::new();

    // On macOS, use native symbols: ⌃ (Control), ⌥ (Option/Alt), ⇧ (Shift)
    let (ctrl_label, alt_label, shift_label) = if use_macos_symbols() {
        ("⌃", "⌥", "⇧")
    } else {
        ("Ctrl", "Alt", "Shift")
    };

    let use_plus = !use_macos_symbols();

    if modifiers.contains(KeyModifiers::CONTROL) {
        result.push_str(ctrl_label);
        if use_plus {
            result.push('+');
        }
    }
    if modifiers.contains(KeyModifiers::ALT) {
        result.push_str(alt_label);
        if use_plus {
            result.push('+');
        }
    }
    if modifiers.contains(KeyModifiers::SHIFT) {
        result.push_str(shift_label);
        if use_plus {
            result.push('+');
        }
    }

    match keycode {
        KeyCode::Enter => result.push_str("Enter"),
        KeyCode::Backspace => result.push_str("Backspace"),
        KeyCode::Delete => result.push_str("Del"),
        KeyCode::Tab => result.push_str("Tab"),
        KeyCode::Esc => result.push_str("Esc"),
        KeyCode::Left => result.push('←'),
        KeyCode::Right => result.push('→'),
        KeyCode::Up => result.push('↑'),
        KeyCode::Down => result.push('↓'),
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
    /// Settings modal is active
    Settings,
}

impl KeyContext {
    /// Check if a context should allow input
    pub fn allows_text_input(&self) -> bool {
        matches!(self, Self::Normal | Self::Prompt | Self::FileExplorer)
    }

    /// Parse context from a "when" string
    pub fn from_when_clause(when: &str) -> Option<Self> {
        Some(match when.trim() {
            "global" => Self::Global,
            "prompt" => Self::Prompt,
            "popup" => Self::Popup,
            "fileExplorer" | "file_explorer" => Self::FileExplorer,
            "normal" => Self::Normal,
            "menu" => Self::Menu,
            "terminal" => Self::Terminal,
            "settings" => Self::Settings,
            _ => return None,
        })
    }

    /// Convert context to "when" clause string
    pub fn to_when_clause(self) -> &'static str {
        match self {
            Self::Global => "global",
            Self::Normal => "normal",
            Self::Prompt => "prompt",
            Self::Popup => "popup",
            Self::FileExplorer => "fileExplorer",
            Self::Menu => "menu",
            Self::Terminal => "terminal",
            Self::Settings => "settings",
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
    MoveWordEnd, // Move to end of current word
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
    SelectToParagraphUp,   // Jump to previous empty line with selection
    SelectToParagraphDown, // Jump to next empty line with selection
    SelectWordLeft,
    SelectWordRight,
    SelectWordEnd, // Select to end of current word
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
    DeleteToLineStart,
    TransposeChars,
    OpenLine,

    // View
    Recenter,

    // Selection
    SetMark,

    // Clipboard
    Copy,
    CopyWithTheme(String),
    Cut,
    Paste,

    // Vi-style yank (copy without selection, then restore cursor)
    YankWordForward,
    YankWordBackward,
    YankToLineEnd,
    YankToLineStart,

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
    ForceQuit,
    Revert,
    ToggleAutoRevert,
    FormatBuffer,
    TrimTrailingWhitespace,
    EnsureFinalNewline,

    // Navigation
    GotoLine,
    GoToMatchingBracket,
    JumpToNextError,
    JumpToPreviousError,

    // Smart editing
    SmartHome,
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
    ShowWarnings,
    ShowStatusLog,
    ShowLspStatus,
    ClearWarnings,
    CommandPalette, // TODO: Consider dropping this now that we have QuickOpen
    /// Quick Open - unified prompt with prefix-based provider routing
    QuickOpen,
    ToggleLineWrap,
    ToggleComposeMode,
    SetComposeWidth,
    SelectTheme,
    SelectKeybindingMap,
    SelectCursorStyle,
    SelectLocale,

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
    /// PromptConfirm with recorded text for macro playback
    PromptConfirmWithText(String),
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

    // File browser actions
    FileBrowserToggleHidden,
    FileBrowserToggleDetectEncoding,

    // Popup mode actions
    PopupSelectNext,
    PopupSelectPrev,
    PopupPageUp,
    PopupPageDown,
    PopupConfirm,
    PopupCancel,

    // File explorer operations
    ToggleFileExplorer,
    // Menu bar visibility
    ToggleMenuBar,
    // Tab bar visibility
    ToggleTabBar,
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
    FileExplorerSearchClear,
    FileExplorerSearchBackspace,

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
    ToggleDebugHighlights, // Debug mode: show highlight/overlay byte ranges
    SetBackground,
    SetBackgroundBlend,

    // Buffer settings (per-buffer overrides)
    SetTabSize,
    SetLineEnding,
    SetEncoding,
    ReloadWithEncoding,
    SetLanguage,
    ToggleIndentationStyle,
    ToggleTabIndicators,
    ResetBufferSettings,

    // Config operations
    DumpConfig,

    // Search and replace
    Search,
    FindInSelection,
    FindNext,
    FindPrevious,
    FindSelectionNext,     // Quick find next occurrence of selection (Ctrl+F3)
    FindSelectionPrevious, // Quick find previous occurrence of selection (Ctrl+Shift+F3)
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

    // Settings operations
    OpenSettings,        // Open the settings modal
    CloseSettings,       // Close the settings modal
    SettingsSave,        // Save settings changes
    SettingsReset,       // Reset current setting to default
    SettingsToggleFocus, // Toggle focus between category and settings panels
    SettingsActivate,    // Activate/toggle the current setting
    SettingsSearch,      // Start search in settings
    SettingsHelp,        // Show settings help overlay
    SettingsIncrement,   // Increment number value or next dropdown option
    SettingsDecrement,   // Decrement number value or previous dropdown option

    // Terminal operations
    OpenTerminal,          // Open a new terminal in the current split
    CloseTerminal,         // Close the current terminal
    FocusTerminal,         // Focus the terminal buffer (if viewing terminal, focus input)
    TerminalEscape,        // Escape from terminal mode back to editor
    ToggleKeyboardCapture, // Toggle keyboard capture mode (all keys go to terminal)
    TerminalPaste,         // Paste clipboard contents into terminal as a single batch

    // Shell command operations
    ShellCommand,        // Run shell command on buffer/selection, output to new buffer
    ShellCommandReplace, // Run shell command on buffer/selection, replace content

    // Case conversion
    ToUpperCase, // Convert selection to uppercase
    ToLowerCase, // Convert selection to lowercase
    SortLines,   // Sort selected lines alphabetically

    // Input calibration
    CalibrateInput, // Open the input calibration wizard

    // Event debug
    EventDebug, // Open the event debug dialog

    // No-op
    None,
}

impl Action {
    fn with_char(
        args: &HashMap<String, serde_json::Value>,
        make_action: impl FnOnce(char) -> Self,
    ) -> Option<Self> {
        if let Some(serde_json::Value::String(value)) = args.get("char") {
            value.chars().next().map(make_action)
        } else {
            None
        }
    }

    /// Parse action from string (used when loading from config)
    pub fn from_str(s: &str, args: &HashMap<String, serde_json::Value>) -> Option<Self> {
        Some(match s {
            "insert_char" => return Self::with_char(args, Self::InsertChar),
            "insert_newline" => Self::InsertNewline,
            "insert_tab" => Self::InsertTab,

            "move_left" => Self::MoveLeft,
            "move_right" => Self::MoveRight,
            "move_up" => Self::MoveUp,
            "move_down" => Self::MoveDown,
            "move_word_left" => Self::MoveWordLeft,
            "move_word_right" => Self::MoveWordRight,
            "move_word_end" => Self::MoveWordEnd,
            "move_line_start" => Self::MoveLineStart,
            "move_line_end" => Self::MoveLineEnd,
            "move_page_up" => Self::MovePageUp,
            "move_page_down" => Self::MovePageDown,
            "move_document_start" => Self::MoveDocumentStart,
            "move_document_end" => Self::MoveDocumentEnd,

            "select_left" => Self::SelectLeft,
            "select_right" => Self::SelectRight,
            "select_up" => Self::SelectUp,
            "select_down" => Self::SelectDown,
            "select_to_paragraph_up" => Self::SelectToParagraphUp,
            "select_to_paragraph_down" => Self::SelectToParagraphDown,
            "select_word_left" => Self::SelectWordLeft,
            "select_word_right" => Self::SelectWordRight,
            "select_word_end" => Self::SelectWordEnd,
            "select_line_start" => Self::SelectLineStart,
            "select_line_end" => Self::SelectLineEnd,
            "select_document_start" => Self::SelectDocumentStart,
            "select_document_end" => Self::SelectDocumentEnd,
            "select_page_up" => Self::SelectPageUp,
            "select_page_down" => Self::SelectPageDown,
            "select_all" => Self::SelectAll,
            "select_word" => Self::SelectWord,
            "select_line" => Self::SelectLine,
            "expand_selection" => Self::ExpandSelection,

            // Block/rectangular selection
            "block_select_left" => Self::BlockSelectLeft,
            "block_select_right" => Self::BlockSelectRight,
            "block_select_up" => Self::BlockSelectUp,
            "block_select_down" => Self::BlockSelectDown,

            "delete_backward" => Self::DeleteBackward,
            "delete_forward" => Self::DeleteForward,
            "delete_word_backward" => Self::DeleteWordBackward,
            "delete_word_forward" => Self::DeleteWordForward,
            "delete_line" => Self::DeleteLine,
            "delete_to_line_end" => Self::DeleteToLineEnd,
            "delete_to_line_start" => Self::DeleteToLineStart,
            "transpose_chars" => Self::TransposeChars,
            "open_line" => Self::OpenLine,
            "recenter" => Self::Recenter,
            "set_mark" => Self::SetMark,

            "copy" => Self::Copy,
            "copy_with_theme" => {
                // Empty theme = open theme picker prompt
                let theme = args.get("theme").and_then(|v| v.as_str()).unwrap_or("");
                Self::CopyWithTheme(theme.to_string())
            }
            "cut" => Self::Cut,
            "paste" => Self::Paste,

            // Vi-style yank actions
            "yank_word_forward" => Self::YankWordForward,
            "yank_word_backward" => Self::YankWordBackward,
            "yank_to_line_end" => Self::YankToLineEnd,
            "yank_to_line_start" => Self::YankToLineStart,

            "add_cursor_above" => Self::AddCursorAbove,
            "add_cursor_below" => Self::AddCursorBelow,
            "add_cursor_next_match" => Self::AddCursorNextMatch,
            "remove_secondary_cursors" => Self::RemoveSecondaryCursors,

            "save" => Self::Save,
            "save_as" => Self::SaveAs,
            "open" => Self::Open,
            "switch_project" => Self::SwitchProject,
            "new" => Self::New,
            "close" => Self::Close,
            "close_tab" => Self::CloseTab,
            "quit" => Self::Quit,
            "force_quit" => Self::ForceQuit,
            "revert" => Self::Revert,
            "toggle_auto_revert" => Self::ToggleAutoRevert,
            "format_buffer" => Self::FormatBuffer,
            "goto_line" => Self::GotoLine,
            "goto_matching_bracket" => Self::GoToMatchingBracket,
            "jump_to_next_error" => Self::JumpToNextError,
            "jump_to_previous_error" => Self::JumpToPreviousError,

            "smart_home" => Self::SmartHome,
            "dedent_selection" => Self::DedentSelection,
            "toggle_comment" => Self::ToggleComment,

            "set_bookmark" => return Self::with_char(args, Self::SetBookmark),
            "jump_to_bookmark" => return Self::with_char(args, Self::JumpToBookmark),
            "clear_bookmark" => return Self::with_char(args, Self::ClearBookmark),

            "list_bookmarks" => Self::ListBookmarks,

            "toggle_search_case_sensitive" => Self::ToggleSearchCaseSensitive,
            "toggle_search_whole_word" => Self::ToggleSearchWholeWord,
            "toggle_search_regex" => Self::ToggleSearchRegex,
            "toggle_search_confirm_each" => Self::ToggleSearchConfirmEach,

            "start_macro_recording" => Self::StartMacroRecording,
            "stop_macro_recording" => Self::StopMacroRecording,
            "play_macro" => return Self::with_char(args, Self::PlayMacro),
            "toggle_macro_recording" => return Self::with_char(args, Self::ToggleMacroRecording),

            "show_macro" => return Self::with_char(args, Self::ShowMacro),

            "list_macros" => Self::ListMacros,
            "prompt_record_macro" => Self::PromptRecordMacro,
            "prompt_play_macro" => Self::PromptPlayMacro,
            "play_last_macro" => Self::PlayLastMacro,
            "prompt_set_bookmark" => Self::PromptSetBookmark,
            "prompt_jump_to_bookmark" => Self::PromptJumpToBookmark,

            "undo" => Self::Undo,
            "redo" => Self::Redo,

            "scroll_up" => Self::ScrollUp,
            "scroll_down" => Self::ScrollDown,
            "show_help" => Self::ShowHelp,
            "keyboard_shortcuts" => Self::ShowKeyboardShortcuts,
            "show_warnings" => Self::ShowWarnings,
            "show_status_log" => Self::ShowStatusLog,
            "show_lsp_status" => Self::ShowLspStatus,
            "clear_warnings" => Self::ClearWarnings,
            "command_palette" => Self::CommandPalette,
            "quick_open" => Self::QuickOpen,
            "toggle_line_wrap" => Self::ToggleLineWrap,
            "toggle_compose_mode" => Self::ToggleComposeMode,
            "set_compose_width" => Self::SetComposeWidth,

            "next_buffer" => Self::NextBuffer,
            "prev_buffer" => Self::PrevBuffer,

            "navigate_back" => Self::NavigateBack,
            "navigate_forward" => Self::NavigateForward,

            "split_horizontal" => Self::SplitHorizontal,
            "split_vertical" => Self::SplitVertical,
            "close_split" => Self::CloseSplit,
            "next_split" => Self::NextSplit,
            "prev_split" => Self::PrevSplit,
            "increase_split_size" => Self::IncreaseSplitSize,
            "decrease_split_size" => Self::DecreaseSplitSize,
            "toggle_maximize_split" => Self::ToggleMaximizeSplit,

            "prompt_confirm" => Self::PromptConfirm,
            "prompt_cancel" => Self::PromptCancel,
            "prompt_backspace" => Self::PromptBackspace,
            "prompt_move_left" => Self::PromptMoveLeft,
            "prompt_move_right" => Self::PromptMoveRight,
            "prompt_move_start" => Self::PromptMoveStart,
            "prompt_move_end" => Self::PromptMoveEnd,
            "prompt_select_prev" => Self::PromptSelectPrev,
            "prompt_select_next" => Self::PromptSelectNext,
            "prompt_page_up" => Self::PromptPageUp,
            "prompt_page_down" => Self::PromptPageDown,
            "prompt_accept_suggestion" => Self::PromptAcceptSuggestion,
            "prompt_delete_word_forward" => Self::PromptDeleteWordForward,
            "prompt_delete_word_backward" => Self::PromptDeleteWordBackward,
            "prompt_delete_to_line_end" => Self::PromptDeleteToLineEnd,
            "prompt_copy" => Self::PromptCopy,
            "prompt_cut" => Self::PromptCut,
            "prompt_paste" => Self::PromptPaste,
            "prompt_move_left_selecting" => Self::PromptMoveLeftSelecting,
            "prompt_move_right_selecting" => Self::PromptMoveRightSelecting,
            "prompt_move_home_selecting" => Self::PromptMoveHomeSelecting,
            "prompt_move_end_selecting" => Self::PromptMoveEndSelecting,
            "prompt_select_word_left" => Self::PromptSelectWordLeft,
            "prompt_select_word_right" => Self::PromptSelectWordRight,
            "prompt_select_all" => Self::PromptSelectAll,
            "file_browser_toggle_hidden" => Self::FileBrowserToggleHidden,
            "file_browser_toggle_detect_encoding" => Self::FileBrowserToggleDetectEncoding,
            "prompt_move_word_left" => Self::PromptMoveWordLeft,
            "prompt_move_word_right" => Self::PromptMoveWordRight,
            "prompt_delete" => Self::PromptDelete,

            "popup_select_next" => Self::PopupSelectNext,
            "popup_select_prev" => Self::PopupSelectPrev,
            "popup_page_up" => Self::PopupPageUp,
            "popup_page_down" => Self::PopupPageDown,
            "popup_confirm" => Self::PopupConfirm,
            "popup_cancel" => Self::PopupCancel,

            "toggle_file_explorer" => Self::ToggleFileExplorer,
            "toggle_menu_bar" => Self::ToggleMenuBar,
            "toggle_tab_bar" => Self::ToggleTabBar,
            "focus_file_explorer" => Self::FocusFileExplorer,
            "focus_editor" => Self::FocusEditor,
            "file_explorer_up" => Self::FileExplorerUp,
            "file_explorer_down" => Self::FileExplorerDown,
            "file_explorer_page_up" => Self::FileExplorerPageUp,
            "file_explorer_page_down" => Self::FileExplorerPageDown,
            "file_explorer_expand" => Self::FileExplorerExpand,
            "file_explorer_collapse" => Self::FileExplorerCollapse,
            "file_explorer_open" => Self::FileExplorerOpen,
            "file_explorer_refresh" => Self::FileExplorerRefresh,
            "file_explorer_new_file" => Self::FileExplorerNewFile,
            "file_explorer_new_directory" => Self::FileExplorerNewDirectory,
            "file_explorer_delete" => Self::FileExplorerDelete,
            "file_explorer_rename" => Self::FileExplorerRename,
            "file_explorer_toggle_hidden" => Self::FileExplorerToggleHidden,
            "file_explorer_toggle_gitignored" => Self::FileExplorerToggleGitignored,
            "file_explorer_search_clear" => Self::FileExplorerSearchClear,
            "file_explorer_search_backspace" => Self::FileExplorerSearchBackspace,

            "lsp_completion" => Self::LspCompletion,
            "lsp_goto_definition" => Self::LspGotoDefinition,
            "lsp_references" => Self::LspReferences,
            "lsp_rename" => Self::LspRename,
            "lsp_hover" => Self::LspHover,
            "lsp_signature_help" => Self::LspSignatureHelp,
            "lsp_code_actions" => Self::LspCodeActions,
            "lsp_restart" => Self::LspRestart,
            "lsp_stop" => Self::LspStop,
            "toggle_inlay_hints" => Self::ToggleInlayHints,
            "toggle_mouse_hover" => Self::ToggleMouseHover,

            "toggle_line_numbers" => Self::ToggleLineNumbers,
            "toggle_mouse_capture" => Self::ToggleMouseCapture,
            "toggle_debug_highlights" => Self::ToggleDebugHighlights,
            "set_background" => Self::SetBackground,
            "set_background_blend" => Self::SetBackgroundBlend,
            "select_theme" => Self::SelectTheme,
            "select_keybinding_map" => Self::SelectKeybindingMap,
            "select_locale" => Self::SelectLocale,

            // Buffer settings
            "set_tab_size" => Self::SetTabSize,
            "set_line_ending" => Self::SetLineEnding,
            "set_encoding" => Self::SetEncoding,
            "reload_with_encoding" => Self::ReloadWithEncoding,
            "toggle_indentation_style" => Self::ToggleIndentationStyle,
            "toggle_tab_indicators" => Self::ToggleTabIndicators,
            "reset_buffer_settings" => Self::ResetBufferSettings,

            "dump_config" => Self::DumpConfig,

            "search" => Self::Search,
            "find_in_selection" => Self::FindInSelection,
            "find_next" => Self::FindNext,
            "find_previous" => Self::FindPrevious,
            "find_selection_next" => Self::FindSelectionNext,
            "find_selection_previous" => Self::FindSelectionPrevious,
            "replace" => Self::Replace,
            "query_replace" => Self::QueryReplace,

            "menu_activate" => Self::MenuActivate,
            "menu_close" => Self::MenuClose,
            "menu_left" => Self::MenuLeft,
            "menu_right" => Self::MenuRight,
            "menu_up" => Self::MenuUp,
            "menu_down" => Self::MenuDown,
            "menu_execute" => Self::MenuExecute,
            "menu_open" => {
                let name = args.get("name")?.as_str()?;
                Self::MenuOpen(name.to_string())
            }

            "switch_keybinding_map" => {
                let map_name = args.get("map")?.as_str()?;
                Self::SwitchKeybindingMap(map_name.to_string())
            }

            // Terminal actions
            "open_terminal" => Self::OpenTerminal,
            "close_terminal" => Self::CloseTerminal,
            "focus_terminal" => Self::FocusTerminal,
            "terminal_escape" => Self::TerminalEscape,
            "toggle_keyboard_capture" => Self::ToggleKeyboardCapture,
            "terminal_paste" => Self::TerminalPaste,

            // Shell command actions
            "shell_command" => Self::ShellCommand,
            "shell_command_replace" => Self::ShellCommandReplace,

            // Case conversion
            "to_upper_case" => Self::ToUpperCase,
            "to_lower_case" => Self::ToLowerCase,
            "sort_lines" => Self::SortLines,

            // Input calibration
            "calibrate_input" => Self::CalibrateInput,

            // Event debug
            "event_debug" => Self::EventDebug,

            // Settings actions
            "open_settings" => Self::OpenSettings,
            "close_settings" => Self::CloseSettings,
            "settings_save" => Self::SettingsSave,
            "settings_reset" => Self::SettingsReset,
            "settings_toggle_focus" => Self::SettingsToggleFocus,
            "settings_activate" => Self::SettingsActivate,
            "settings_search" => Self::SettingsSearch,
            "settings_help" => Self::SettingsHelp,
            "settings_increment" => Self::SettingsIncrement,
            "settings_decrement" => Self::SettingsDecrement,

            _ => return None,
        })
    }

    /// Check if this action is a movement or editing action that should be
    /// ignored in virtual buffers with hidden cursors.
    pub fn is_movement_or_editing(&self) -> bool {
        matches!(
            self,
            // Movement actions
            Action::MoveLeft
                | Action::MoveRight
                | Action::MoveUp
                | Action::MoveDown
                | Action::MoveWordLeft
                | Action::MoveWordRight
                | Action::MoveWordEnd
                | Action::MoveLineStart
                | Action::MoveLineEnd
                | Action::MovePageUp
                | Action::MovePageDown
                | Action::MoveDocumentStart
                | Action::MoveDocumentEnd
                // Selection actions
                | Action::SelectLeft
                | Action::SelectRight
                | Action::SelectUp
                | Action::SelectDown
                | Action::SelectToParagraphUp
                | Action::SelectToParagraphDown
                | Action::SelectWordLeft
                | Action::SelectWordRight
                | Action::SelectWordEnd
                | Action::SelectLineStart
                | Action::SelectLineEnd
                | Action::SelectDocumentStart
                | Action::SelectDocumentEnd
                | Action::SelectPageUp
                | Action::SelectPageDown
                | Action::SelectAll
                | Action::SelectWord
                | Action::SelectLine
                | Action::ExpandSelection
                // Block selection
                | Action::BlockSelectLeft
                | Action::BlockSelectRight
                | Action::BlockSelectUp
                | Action::BlockSelectDown
                // Editing actions
                | Action::InsertChar(_)
                | Action::InsertNewline
                | Action::InsertTab
                | Action::DeleteBackward
                | Action::DeleteForward
                | Action::DeleteWordBackward
                | Action::DeleteWordForward
                | Action::DeleteLine
                | Action::DeleteToLineEnd
                | Action::DeleteToLineStart
                | Action::TransposeChars
                | Action::OpenLine
                // Clipboard editing (but not Copy)
                | Action::Cut
                | Action::Paste
                // Undo/Redo
                | Action::Undo
                | Action::Redo
        )
    }

    /// Check if this action modifies buffer content (for block selection conversion).
    /// Block selections should be converted to multi-cursor before these actions.
    pub fn is_editing(&self) -> bool {
        matches!(
            self,
            Action::InsertChar(_)
                | Action::InsertNewline
                | Action::InsertTab
                | Action::DeleteBackward
                | Action::DeleteForward
                | Action::DeleteWordBackward
                | Action::DeleteWordForward
                | Action::DeleteLine
                | Action::DeleteToLineEnd
                | Action::DeleteToLineStart
                | Action::TransposeChars
                | Action::OpenLine
                | Action::Cut
                | Action::Paste
        )
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
                            .or_default()
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
        let context_bindings = self.default_bindings.entry(context).or_default();

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
                            .or_default()
                            .insert(sequence, action);
                    }
                } else if let Some(key_code) = Self::parse_key(&binding.key) {
                    // Single key binding (legacy format)
                    let modifiers = Self::parse_modifiers(&binding.modifiers);
                    self.bindings
                        .entry(context)
                        .or_default()
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
                | Action::ForceQuit
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
                | Action::QuickOpen
                | Action::OpenSettings
                | Action::MenuActivate
                | Action::MenuOpen(_)
                | Action::ShowHelp
                | Action::ShowKeyboardShortcuts
                | Action::Quit
                | Action::ForceQuit
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
                // Menu bar
                | Action::ToggleMenuBar
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
        if context.allows_text_input() && is_text_input_modifier(event.modifiers) {
            if let KeyCode::Char(c) = event.code {
                tracing::trace!("  -> Character input: '{}'", c);
                return Action::InsertChar(c);
            }
        }

        tracing::trace!("  -> No binding found, returning Action::None");
        Action::None
    }

    /// Resolve a key event looking only in the specified context (no Global fallback).
    /// This is used when a modal context (like Prompt) needs to check if it has
    /// a specific binding without being overridden by Global bindings.
    /// Returns None if no binding found in the specified context.
    pub fn resolve_in_context_only(&self, event: &KeyEvent, context: KeyContext) -> Option<Action> {
        // Try custom bindings for this context
        if let Some(context_bindings) = self.bindings.get(&context) {
            if let Some(action) = context_bindings.get(&(event.code, event.modifiers)) {
                return Some(action.clone());
            }
        }

        // Try default bindings for this context
        if let Some(context_bindings) = self.default_bindings.get(&context) {
            if let Some(action) = context_bindings.get(&(event.code, event.modifiers)) {
                return Some(action.clone());
            }
        }

        None
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
                return Some(format_keybinding(&key_code, &modifiers));
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

    /// Parse a key string to KeyCode
    fn parse_key(key: &str) -> Option<KeyCode> {
        let lower = key.to_lowercase();
        match lower.as_str() {
            "enter" => Some(KeyCode::Enter),
            "backspace" => Some(KeyCode::Backspace),
            "delete" | "del" => Some(KeyCode::Delete),
            "tab" => Some(KeyCode::Tab),
            "backtab" => Some(KeyCode::BackTab),
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
            Action::InsertChar(c) => t!("action.insert_char", char = c),
            Action::InsertNewline => t!("action.insert_newline"),
            Action::InsertTab => t!("action.insert_tab"),
            Action::MoveLeft => t!("action.move_left"),
            Action::MoveRight => t!("action.move_right"),
            Action::MoveUp => t!("action.move_up"),
            Action::MoveDown => t!("action.move_down"),
            Action::MoveWordLeft => t!("action.move_word_left"),
            Action::MoveWordRight => t!("action.move_word_right"),
            Action::MoveWordEnd => t!("action.move_word_end"),
            Action::MoveLineStart => t!("action.move_line_start"),
            Action::MoveLineEnd => t!("action.move_line_end"),
            Action::MovePageUp => t!("action.move_page_up"),
            Action::MovePageDown => t!("action.move_page_down"),
            Action::MoveDocumentStart => t!("action.move_document_start"),
            Action::MoveDocumentEnd => t!("action.move_document_end"),
            Action::SelectLeft => t!("action.select_left"),
            Action::SelectRight => t!("action.select_right"),
            Action::SelectUp => t!("action.select_up"),
            Action::SelectDown => t!("action.select_down"),
            Action::SelectToParagraphUp => t!("action.select_to_paragraph_up"),
            Action::SelectToParagraphDown => t!("action.select_to_paragraph_down"),
            Action::SelectWordLeft => t!("action.select_word_left"),
            Action::SelectWordRight => t!("action.select_word_right"),
            Action::SelectWordEnd => t!("action.select_word_end"),
            Action::SelectLineStart => t!("action.select_line_start"),
            Action::SelectLineEnd => t!("action.select_line_end"),
            Action::SelectDocumentStart => t!("action.select_document_start"),
            Action::SelectDocumentEnd => t!("action.select_document_end"),
            Action::SelectPageUp => t!("action.select_page_up"),
            Action::SelectPageDown => t!("action.select_page_down"),
            Action::SelectAll => t!("action.select_all"),
            Action::SelectWord => t!("action.select_word"),
            Action::SelectLine => t!("action.select_line"),
            Action::ExpandSelection => t!("action.expand_selection"),
            Action::BlockSelectLeft => t!("action.block_select_left"),
            Action::BlockSelectRight => t!("action.block_select_right"),
            Action::BlockSelectUp => t!("action.block_select_up"),
            Action::BlockSelectDown => t!("action.block_select_down"),
            Action::DeleteBackward => t!("action.delete_backward"),
            Action::DeleteForward => t!("action.delete_forward"),
            Action::DeleteWordBackward => t!("action.delete_word_backward"),
            Action::DeleteWordForward => t!("action.delete_word_forward"),
            Action::DeleteLine => t!("action.delete_line"),
            Action::DeleteToLineEnd => t!("action.delete_to_line_end"),
            Action::DeleteToLineStart => t!("action.delete_to_line_start"),
            Action::TransposeChars => t!("action.transpose_chars"),
            Action::OpenLine => t!("action.open_line"),
            Action::Recenter => t!("action.recenter"),
            Action::SetMark => t!("action.set_mark"),
            Action::Copy => t!("action.copy"),
            Action::CopyWithTheme(theme) if theme.is_empty() => t!("action.copy_with_formatting"),
            Action::CopyWithTheme(theme) => t!("action.copy_with_theme", theme = theme),
            Action::Cut => t!("action.cut"),
            Action::Paste => t!("action.paste"),
            Action::YankWordForward => t!("action.yank_word_forward"),
            Action::YankWordBackward => t!("action.yank_word_backward"),
            Action::YankToLineEnd => t!("action.yank_to_line_end"),
            Action::YankToLineStart => t!("action.yank_to_line_start"),
            Action::AddCursorAbove => t!("action.add_cursor_above"),
            Action::AddCursorBelow => t!("action.add_cursor_below"),
            Action::AddCursorNextMatch => t!("action.add_cursor_next_match"),
            Action::RemoveSecondaryCursors => t!("action.remove_secondary_cursors"),
            Action::Save => t!("action.save"),
            Action::SaveAs => t!("action.save_as"),
            Action::Open => t!("action.open"),
            Action::SwitchProject => t!("action.switch_project"),
            Action::New => t!("action.new"),
            Action::Close => t!("action.close"),
            Action::CloseTab => t!("action.close_tab"),
            Action::Quit => t!("action.quit"),
            Action::ForceQuit => t!("action.force_quit"),
            Action::Revert => t!("action.revert"),
            Action::ToggleAutoRevert => t!("action.toggle_auto_revert"),
            Action::FormatBuffer => t!("action.format_buffer"),
            Action::TrimTrailingWhitespace => t!("action.trim_trailing_whitespace"),
            Action::EnsureFinalNewline => t!("action.ensure_final_newline"),
            Action::GotoLine => t!("action.goto_line"),
            Action::GoToMatchingBracket => t!("action.goto_matching_bracket"),
            Action::JumpToNextError => t!("action.jump_to_next_error"),
            Action::JumpToPreviousError => t!("action.jump_to_previous_error"),
            Action::SmartHome => t!("action.smart_home"),
            Action::DedentSelection => t!("action.dedent_selection"),
            Action::ToggleComment => t!("action.toggle_comment"),
            Action::SetBookmark(c) => t!("action.set_bookmark", key = c),
            Action::JumpToBookmark(c) => t!("action.jump_to_bookmark", key = c),
            Action::ClearBookmark(c) => t!("action.clear_bookmark", key = c),
            Action::ListBookmarks => t!("action.list_bookmarks"),
            Action::ToggleSearchCaseSensitive => t!("action.toggle_search_case_sensitive"),
            Action::ToggleSearchWholeWord => t!("action.toggle_search_whole_word"),
            Action::ToggleSearchRegex => t!("action.toggle_search_regex"),
            Action::ToggleSearchConfirmEach => t!("action.toggle_search_confirm_each"),
            Action::StartMacroRecording => t!("action.start_macro_recording"),
            Action::StopMacroRecording => t!("action.stop_macro_recording"),
            Action::PlayMacro(c) => t!("action.play_macro", key = c),
            Action::ToggleMacroRecording(c) => t!("action.toggle_macro_recording", key = c),
            Action::ShowMacro(c) => t!("action.show_macro", key = c),
            Action::ListMacros => t!("action.list_macros"),
            Action::PromptRecordMacro => t!("action.prompt_record_macro"),
            Action::PromptPlayMacro => t!("action.prompt_play_macro"),
            Action::PlayLastMacro => t!("action.play_last_macro"),
            Action::PromptSetBookmark => t!("action.prompt_set_bookmark"),
            Action::PromptJumpToBookmark => t!("action.prompt_jump_to_bookmark"),
            Action::Undo => t!("action.undo"),
            Action::Redo => t!("action.redo"),
            Action::ScrollUp => t!("action.scroll_up"),
            Action::ScrollDown => t!("action.scroll_down"),
            Action::ShowHelp => t!("action.show_help"),
            Action::ShowKeyboardShortcuts => t!("action.show_keyboard_shortcuts"),
            Action::ShowWarnings => t!("action.show_warnings"),
            Action::ShowStatusLog => t!("action.show_status_log"),
            Action::ShowLspStatus => t!("action.show_lsp_status"),
            Action::ClearWarnings => t!("action.clear_warnings"),
            Action::CommandPalette => t!("action.command_palette"),
            Action::QuickOpen => t!("action.quick_open"),
            Action::ToggleLineWrap => t!("action.toggle_line_wrap"),
            Action::ToggleComposeMode => t!("action.toggle_compose_mode"),
            Action::SetComposeWidth => t!("action.set_compose_width"),
            Action::NextBuffer => t!("action.next_buffer"),
            Action::PrevBuffer => t!("action.prev_buffer"),
            Action::NavigateBack => t!("action.navigate_back"),
            Action::NavigateForward => t!("action.navigate_forward"),
            Action::SplitHorizontal => t!("action.split_horizontal"),
            Action::SplitVertical => t!("action.split_vertical"),
            Action::CloseSplit => t!("action.close_split"),
            Action::NextSplit => t!("action.next_split"),
            Action::PrevSplit => t!("action.prev_split"),
            Action::IncreaseSplitSize => t!("action.increase_split_size"),
            Action::DecreaseSplitSize => t!("action.decrease_split_size"),
            Action::ToggleMaximizeSplit => t!("action.toggle_maximize_split"),
            Action::PromptConfirm => t!("action.prompt_confirm"),
            Action::PromptConfirmWithText(ref text) => {
                format!("{} ({})", t!("action.prompt_confirm"), text).into()
            }
            Action::PromptCancel => t!("action.prompt_cancel"),
            Action::PromptBackspace => t!("action.prompt_backspace"),
            Action::PromptDelete => t!("action.prompt_delete"),
            Action::PromptMoveLeft => t!("action.prompt_move_left"),
            Action::PromptMoveRight => t!("action.prompt_move_right"),
            Action::PromptMoveStart => t!("action.prompt_move_start"),
            Action::PromptMoveEnd => t!("action.prompt_move_end"),
            Action::PromptSelectPrev => t!("action.prompt_select_prev"),
            Action::PromptSelectNext => t!("action.prompt_select_next"),
            Action::PromptPageUp => t!("action.prompt_page_up"),
            Action::PromptPageDown => t!("action.prompt_page_down"),
            Action::PromptAcceptSuggestion => t!("action.prompt_accept_suggestion"),
            Action::PromptMoveWordLeft => t!("action.prompt_move_word_left"),
            Action::PromptMoveWordRight => t!("action.prompt_move_word_right"),
            Action::PromptDeleteWordForward => t!("action.prompt_delete_word_forward"),
            Action::PromptDeleteWordBackward => t!("action.prompt_delete_word_backward"),
            Action::PromptDeleteToLineEnd => t!("action.prompt_delete_to_line_end"),
            Action::PromptCopy => t!("action.prompt_copy"),
            Action::PromptCut => t!("action.prompt_cut"),
            Action::PromptPaste => t!("action.prompt_paste"),
            Action::PromptMoveLeftSelecting => t!("action.prompt_move_left_selecting"),
            Action::PromptMoveRightSelecting => t!("action.prompt_move_right_selecting"),
            Action::PromptMoveHomeSelecting => t!("action.prompt_move_home_selecting"),
            Action::PromptMoveEndSelecting => t!("action.prompt_move_end_selecting"),
            Action::PromptSelectWordLeft => t!("action.prompt_select_word_left"),
            Action::PromptSelectWordRight => t!("action.prompt_select_word_right"),
            Action::PromptSelectAll => t!("action.prompt_select_all"),
            Action::FileBrowserToggleHidden => t!("action.file_browser_toggle_hidden"),
            Action::FileBrowserToggleDetectEncoding => {
                t!("action.file_browser_toggle_detect_encoding")
            }
            Action::PopupSelectNext => t!("action.popup_select_next"),
            Action::PopupSelectPrev => t!("action.popup_select_prev"),
            Action::PopupPageUp => t!("action.popup_page_up"),
            Action::PopupPageDown => t!("action.popup_page_down"),
            Action::PopupConfirm => t!("action.popup_confirm"),
            Action::PopupCancel => t!("action.popup_cancel"),
            Action::ToggleFileExplorer => t!("action.toggle_file_explorer"),
            Action::ToggleMenuBar => t!("action.toggle_menu_bar"),
            Action::ToggleTabBar => t!("action.toggle_tab_bar"),
            Action::FocusFileExplorer => t!("action.focus_file_explorer"),
            Action::FocusEditor => t!("action.focus_editor"),
            Action::FileExplorerUp => t!("action.file_explorer_up"),
            Action::FileExplorerDown => t!("action.file_explorer_down"),
            Action::FileExplorerPageUp => t!("action.file_explorer_page_up"),
            Action::FileExplorerPageDown => t!("action.file_explorer_page_down"),
            Action::FileExplorerExpand => t!("action.file_explorer_expand"),
            Action::FileExplorerCollapse => t!("action.file_explorer_collapse"),
            Action::FileExplorerOpen => t!("action.file_explorer_open"),
            Action::FileExplorerRefresh => t!("action.file_explorer_refresh"),
            Action::FileExplorerNewFile => t!("action.file_explorer_new_file"),
            Action::FileExplorerNewDirectory => t!("action.file_explorer_new_directory"),
            Action::FileExplorerDelete => t!("action.file_explorer_delete"),
            Action::FileExplorerRename => t!("action.file_explorer_rename"),
            Action::FileExplorerToggleHidden => t!("action.file_explorer_toggle_hidden"),
            Action::FileExplorerToggleGitignored => t!("action.file_explorer_toggle_gitignored"),
            Action::FileExplorerSearchClear => t!("action.file_explorer_search_clear"),
            Action::FileExplorerSearchBackspace => t!("action.file_explorer_search_backspace"),
            Action::LspCompletion => t!("action.lsp_completion"),
            Action::LspGotoDefinition => t!("action.lsp_goto_definition"),
            Action::LspReferences => t!("action.lsp_references"),
            Action::LspRename => t!("action.lsp_rename"),
            Action::LspHover => t!("action.lsp_hover"),
            Action::LspSignatureHelp => t!("action.lsp_signature_help"),
            Action::LspCodeActions => t!("action.lsp_code_actions"),
            Action::LspRestart => t!("action.lsp_restart"),
            Action::LspStop => t!("action.lsp_stop"),
            Action::ToggleInlayHints => t!("action.toggle_inlay_hints"),
            Action::ToggleMouseHover => t!("action.toggle_mouse_hover"),
            Action::ToggleLineNumbers => t!("action.toggle_line_numbers"),
            Action::ToggleMouseCapture => t!("action.toggle_mouse_capture"),
            Action::ToggleDebugHighlights => t!("action.toggle_debug_highlights"),
            Action::SetBackground => t!("action.set_background"),
            Action::SetBackgroundBlend => t!("action.set_background_blend"),
            Action::SetTabSize => t!("action.set_tab_size"),
            Action::SetLineEnding => t!("action.set_line_ending"),
            Action::SetEncoding => t!("action.set_encoding"),
            Action::ReloadWithEncoding => t!("action.reload_with_encoding"),
            Action::SetLanguage => t!("action.set_language"),
            Action::ToggleIndentationStyle => t!("action.toggle_indentation_style"),
            Action::ToggleTabIndicators => t!("action.toggle_tab_indicators"),
            Action::ResetBufferSettings => t!("action.reset_buffer_settings"),
            Action::DumpConfig => t!("action.dump_config"),
            Action::Search => t!("action.search"),
            Action::FindInSelection => t!("action.find_in_selection"),
            Action::FindNext => t!("action.find_next"),
            Action::FindPrevious => t!("action.find_previous"),
            Action::FindSelectionNext => t!("action.find_selection_next"),
            Action::FindSelectionPrevious => t!("action.find_selection_previous"),
            Action::Replace => t!("action.replace"),
            Action::QueryReplace => t!("action.query_replace"),
            Action::MenuActivate => t!("action.menu_activate"),
            Action::MenuClose => t!("action.menu_close"),
            Action::MenuLeft => t!("action.menu_left"),
            Action::MenuRight => t!("action.menu_right"),
            Action::MenuUp => t!("action.menu_up"),
            Action::MenuDown => t!("action.menu_down"),
            Action::MenuExecute => t!("action.menu_execute"),
            Action::MenuOpen(name) => t!("action.menu_open", name = name),
            Action::SwitchKeybindingMap(map) => t!("action.switch_keybinding_map", map = map),
            Action::PluginAction(name) => t!("action.plugin_action", name = name),
            Action::ScrollTabsLeft => t!("action.scroll_tabs_left"),
            Action::ScrollTabsRight => t!("action.scroll_tabs_right"),
            Action::SelectTheme => t!("action.select_theme"),
            Action::SelectKeybindingMap => t!("action.select_keybinding_map"),
            Action::SelectCursorStyle => t!("action.select_cursor_style"),
            Action::SelectLocale => t!("action.select_locale"),
            Action::SwitchToPreviousTab => t!("action.switch_to_previous_tab"),
            Action::SwitchToTabByName => t!("action.switch_to_tab_by_name"),
            Action::OpenTerminal => t!("action.open_terminal"),
            Action::CloseTerminal => t!("action.close_terminal"),
            Action::FocusTerminal => t!("action.focus_terminal"),
            Action::TerminalEscape => t!("action.terminal_escape"),
            Action::ToggleKeyboardCapture => t!("action.toggle_keyboard_capture"),
            Action::TerminalPaste => t!("action.terminal_paste"),
            Action::OpenSettings => t!("action.open_settings"),
            Action::CloseSettings => t!("action.close_settings"),
            Action::SettingsSave => t!("action.settings_save"),
            Action::SettingsReset => t!("action.settings_reset"),
            Action::SettingsToggleFocus => t!("action.settings_toggle_focus"),
            Action::SettingsActivate => t!("action.settings_activate"),
            Action::SettingsSearch => t!("action.settings_search"),
            Action::SettingsHelp => t!("action.settings_help"),
            Action::SettingsIncrement => t!("action.settings_increment"),
            Action::SettingsDecrement => t!("action.settings_decrement"),
            Action::ShellCommand => t!("action.shell_command"),
            Action::ShellCommandReplace => t!("action.shell_command_replace"),
            Action::ToUpperCase => t!("action.to_uppercase"),
            Action::ToLowerCase => t!("action.to_lowercase"),
            Action::SortLines => t!("action.sort_lines"),
            Action::CalibrateInput => t!("action.calibrate_input"),
            Action::EventDebug => t!("action.event_debug"),
            Action::None => t!("action.none"),
        }
        .to_string()
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
                        .or_default()
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
        assert_eq!(KeybindingResolver::parse_key("tab"), Some(KeyCode::Tab));
        assert_eq!(
            KeybindingResolver::parse_key("backtab"),
            Some(KeyCode::BackTab)
        );
        assert_eq!(
            KeybindingResolver::parse_key("BackTab"),
            Some(KeyCode::BackTab)
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

        // Test calibrate_input action
        assert_eq!(
            Action::from_str("calibrate_input", &args),
            Some(Action::CalibrateInput)
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

    #[test]
    fn test_no_duplicate_keybindings_in_keymaps() {
        // Load all keymaps and check for duplicate bindings within the same context
        // A duplicate is when the same key+modifiers+context is defined more than once
        use std::collections::HashMap;

        let keymaps: &[(&str, &str)] = &[
            ("default", include_str!("../../keymaps/default.json")),
            ("macos", include_str!("../../keymaps/macos.json")),
        ];

        for (keymap_name, json_content) in keymaps {
            let keymap: crate::config::KeymapConfig = serde_json::from_str(json_content)
                .unwrap_or_else(|e| panic!("Failed to parse keymap '{}': {}", keymap_name, e));

            // Track seen bindings per context: (key, modifiers, context) -> action
            let mut seen: HashMap<(String, Vec<String>, String), String> = HashMap::new();
            let mut duplicates: Vec<String> = Vec::new();

            for binding in &keymap.bindings {
                let when = binding.when.clone().unwrap_or_default();
                let key_id = (binding.key.clone(), binding.modifiers.clone(), when.clone());

                if let Some(existing_action) = seen.get(&key_id) {
                    duplicates.push(format!(
                        "Duplicate in '{}': key='{}', modifiers={:?}, when='{}' -> '{}' vs '{}'",
                        keymap_name,
                        binding.key,
                        binding.modifiers,
                        when,
                        existing_action,
                        binding.action
                    ));
                } else {
                    seen.insert(key_id, binding.action.clone());
                }
            }

            assert!(
                duplicates.is_empty(),
                "Found duplicate keybindings:\n{}",
                duplicates.join("\n")
            );
        }
    }
}
