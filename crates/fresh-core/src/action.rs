use serde::{Deserialize, Serialize};

/// Context in which a keybinding is active
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, ts_rs::TS)]
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
    /// Buffer-local mode context (e.g. "search-replace-list")
    Mode(String),
}

impl KeyContext {
    /// Check if a context should allow input
    pub fn allows_text_input(&self) -> bool {
        matches!(self, Self::Normal | Self::Prompt)
    }

    /// Parse context from a "when" string
    pub fn from_when_clause(when: &str) -> Option<Self> {
        let trimmed = when.trim();
        if let Some(mode_name) = trimmed.strip_prefix("mode:") {
            return Some(Self::Mode(mode_name.to_string()));
        }
        Some(match trimmed {
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
    pub fn to_when_clause(&self) -> String {
        match self {
            Self::Global => "global".to_string(),
            Self::Normal => "normal".to_string(),
            Self::Prompt => "prompt".to_string(),
            Self::Popup => "popup".to_string(),
            Self::FileExplorer => "fileExplorer".to_string(),
            Self::Menu => "menu".to_string(),
            Self::Terminal => "terminal".to_string(),
            Self::Settings => "settings".to_string(),
            Self::Mode(name) => format!("mode:{}", name),
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
    ScanLineIndex,
    GoToMatchingBracket,
    JumpToNextError,
    JumpToPreviousError,

    // Smart editing
    SmartHome,
    DedentSelection,
    ToggleComment,
    /// Cycle through dabbrev completions (Emacs Alt+/ style).
    /// Unlike popup-based completion, this inserts the best match directly
    /// and cycles through alternatives on repeated invocations.
    DabbrevExpand,

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
    ToggleReadOnly,
    TogglePageView,
    SetPageWidth,
    InspectThemeAtCursor,
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

    // Load the current buffer's contents as a plugin
    LoadPluginFromBuffer,

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

#[cfg(test)]
mod tests {
    use super::*;

    /// Every variant must round-trip through `to_when_clause` → `from_when_clause`
    /// as the identity. This is the core contract these two functions implement.
    #[test]
    fn when_clause_is_a_roundtrip_for_every_variant() {
        let variants = [
            KeyContext::Global,
            KeyContext::Normal,
            KeyContext::Prompt,
            KeyContext::Popup,
            KeyContext::FileExplorer,
            KeyContext::Menu,
            KeyContext::Terminal,
            KeyContext::Settings,
            KeyContext::Mode("search-replace-list".into()),
            KeyContext::Mode(String::new()),
        ];
        for ctx in &variants {
            let clause = ctx.to_when_clause();
            assert_eq!(
                KeyContext::from_when_clause(&clause).as_ref(),
                Some(ctx),
                "roundtrip failed: {:?} → {:?}",
                ctx,
                clause
            );
        }
    }

    /// Non-canonical inputs the parser must also accept, plus invalid inputs
    /// it must reject. Not covered by the roundtrip, since `to_when_clause`
    /// only emits canonical forms.
    #[test]
    fn from_when_clause_handles_aliases_whitespace_and_rejects_unknown() {
        // snake_case alias for fileExplorer
        assert_eq!(
            KeyContext::from_when_clause("file_explorer"),
            Some(KeyContext::FileExplorer)
        );
        // Surrounding whitespace is trimmed
        assert_eq!(
            KeyContext::from_when_clause("  prompt  "),
            Some(KeyContext::Prompt)
        );
        // Unknown / case-mismatched / empty → None
        assert_eq!(KeyContext::from_when_clause("nonsense"), None);
        assert_eq!(KeyContext::from_when_clause("GLOBAL"), None);
        assert_eq!(KeyContext::from_when_clause(""), None);
    }

    /// `allows_text_input` is true iff the context is `Normal` or `Prompt`.
    #[test]
    fn allows_text_input_iff_normal_or_prompt() {
        for ctx in [
            KeyContext::Global,
            KeyContext::Normal,
            KeyContext::Prompt,
            KeyContext::Popup,
            KeyContext::FileExplorer,
            KeyContext::Menu,
            KeyContext::Terminal,
            KeyContext::Settings,
            KeyContext::Mode("foo".into()),
        ] {
            let expected = matches!(ctx, KeyContext::Normal | KeyContext::Prompt);
            assert_eq!(
                ctx.allows_text_input(),
                expected,
                "{:?} text-input expectation violated",
                ctx
            );
        }
    }
}
