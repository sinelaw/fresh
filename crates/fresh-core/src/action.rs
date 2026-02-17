use serde::{Deserialize, Serialize};

/// Context in which a keybinding is active
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, ts_rs::TS)]
#[ts(export)]
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
        matches!(self, Self::Normal | Self::Prompt)
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
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, ts_rs::TS)]
#[ts(export)]
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
    Revert,
    ToggleAutoRevert,
    FormatBuffer,

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
    ShowLspStatus,
    ClearWarnings,
    CommandPalette,
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
    ToggleScrollSync,
    ToggleMouseCapture,
    ToggleDebugHighlights, // Debug mode: show highlight/overlay byte ranges
    SetBackground,
    SetBackgroundBlend,

    // Buffer settings (per-buffer overrides)
    SetTabSize,
    SetLineEnding,
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

    // Input calibration
    CalibrateInput, // Open the input calibration wizard

    // No-op
    None,
}
