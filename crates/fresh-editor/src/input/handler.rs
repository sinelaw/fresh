//! Flat per-surface input handlers for the bespoke modal interiors.
//!
//! Each [`InputHandler`] is one surface's keyboard interior — Settings,
//! the menu, the prompt, the popup stacks, the file browser, the
//! query-replace confirm, terminal mode — invoked FROM its owning
//! chrome component's `on_layer_key` (or, for terminal mode, the
//! documented pre-band stage). Between-surface routing does NOT happen
//! here: precedence lives in the derived overlay-layer walk
//! (`Editor::dispatch_layer_keyboard`), and a handler's `Ignored`
//! becomes the walk's fall-through. Principles:
//!
//! 1. **Explicit consumption**: return `InputResult::Consumed` to stop
//!    the walk or `InputResult::Ignored` to fall through.
//! 2. **Modals consume by default**: modal interiors return `Consumed`
//!    for unhandled keys to prevent input leakage, opting out per key
//!    (e.g. Ctrl+P toggling Quick Open closed while it's open).
//!
//! (This module once described a tree-based leaf-first/bubble-up
//! hierarchy; no handler ever had a child, and keyboard bubbling is
//! the layer walk's job — the hierarchy half is deleted.)

use crossterm::event::KeyEvent;

/// Mouse event kinds for terminal forwarding.
/// Simplified from crossterm's MouseEventKind to capture what we need.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TerminalMouseEventKind {
    /// Button press
    Down(TerminalMouseButton),
    /// Button release
    Up(TerminalMouseButton),
    /// Mouse drag with button held
    Drag(TerminalMouseButton),
    /// Mouse movement (no button)
    Moved,
    /// Scroll up
    ScrollUp,
    /// Scroll down
    ScrollDown,
    /// Horizontal scroll left (xterm button 6)
    ScrollLeft,
    /// Horizontal scroll right (xterm button 7)
    ScrollRight,
}

/// Mouse buttons for terminal forwarding.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TerminalMouseButton {
    Left,
    Right,
    Middle,
}

/// Result of handling an input event.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InputResult {
    /// The input was handled - stop propagation.
    Consumed,
    /// The input was not handled - try parent.
    Ignored,
}

impl InputResult {
    /// Returns true if the input was consumed.
    pub fn is_consumed(self) -> bool {
        self == InputResult::Consumed
    }
}

/// Context passed to input handlers, providing access to shared state.
#[derive(Default)]
pub struct InputContext {
    /// Status message to display (set by handlers).
    pub status_message: Option<String>,
    /// Actions to execute after input handling (for deferred operations).
    pub deferred_actions: Vec<DeferredAction>,
}

impl InputContext {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn set_status(&mut self, msg: impl Into<String>) {
        self.status_message = Some(msg.into());
    }

    pub fn defer(&mut self, action: DeferredAction) {
        self.deferred_actions.push(action);
    }
}

/// Actions that need to be executed after input handling completes.
/// These are operations that require mutable access to Editor.
#[derive(Debug, Clone)]
pub enum DeferredAction {
    // Settings actions
    CloseSettings {
        save: bool,
    },
    /// Paste text from clipboard into the active settings input
    PasteToSettings,
    /// Open the config file for the specified layer in the editor
    OpenConfigFile {
        layer: crate::config_io::ConfigLayer,
    },

    // Menu actions
    CloseMenu,
    ExecuteMenuAction {
        action: String,
        args: std::collections::HashMap<String, serde_json::Value>,
    },

    // Prompt actions
    ClosePrompt,
    ConfirmPrompt,
    UpdatePromptSuggestions,
    PromptHistoryPrev,
    PromptHistoryNext,
    /// Preview theme from the current prompt input (for SelectTheme)
    PreviewThemeFromPrompt,
    /// Notify plugin that prompt selection changed (for live preview in Live Grep, etc.)
    PromptSelectionChanged {
        selected_index: usize,
    },

    // Popup actions
    ClosePopup,
    ConfirmPopup,
    /// Type a character while completion popup is open (for type-to-filter)
    PopupTypeChar(char),
    /// Backspace while completion popup is open (for type-to-filter)
    PopupBackspace,
    /// Copy text to clipboard (from popup text selection)
    CopyToClipboard(String),

    // File browser actions
    FileBrowserSelectPrev,
    FileBrowserSelectNext,
    FileBrowserPageUp,
    FileBrowserPageDown,
    FileBrowserConfirm,
    FileBrowserAcceptSuggestion,
    FileBrowserGoParent,
    FileBrowserUpdateFilter,
    FileBrowserToggleHidden,

    // Interactive replace actions
    InteractiveReplaceKey(char),
    CancelInteractiveReplace,

    // Terminal mode actions
    ToggleKeyboardCapture,
    SendTerminalKey(crossterm::event::KeyCode, crossterm::event::KeyModifiers),
    /// Send a mouse event to the terminal PTY.
    /// Fields: (col, row, event_kind, button, modifiers)
    /// Coordinates are terminal-relative (0-based from terminal content area).
    SendTerminalMouse {
        col: u16,
        row: u16,
        kind: TerminalMouseEventKind,
        modifiers: crossterm::event::KeyModifiers,
    },
    ExitTerminalMode {
        explicit: bool,
    },
    EnterScrollbackMode,
    EnterTerminalMode,

    // Generic action execution
    ExecuteAction(crate::input::keybindings::Action),

    // Insert character (for prompts that need to update suggestions)
    InsertCharAndUpdate(char),
}

/// Trait for one surface's keyboard interior (see module doc).
///
/// Implementors should:
/// 1. Handle keys relevant to this surface
/// 2. Return `Consumed` or `Ignored` appropriately
/// 3. Modal surfaces should return `Consumed` for unhandled keys
pub trait InputHandler {
    /// Handle a key event. Returns whether the event was consumed.
    fn handle_key_event(&mut self, event: &KeyEvent, ctx: &mut InputContext) -> InputResult;

    /// Whether this handler is modal (consumes all unhandled input).
    fn is_modal(&self) -> bool {
        false
    }

    /// Dispatch input through this handler — the entry point the
    /// owning component calls.
    fn dispatch_input(&mut self, event: &KeyEvent, ctx: &mut InputContext) -> InputResult {
        let result = self.handle_key_event(event, ctx);
        if result == InputResult::Consumed {
            return InputResult::Consumed;
        }

        // If explicitly ignored, pass through (even for modal handlers)
        // This allows modal handlers to opt-out of consuming specific keys
        // (e.g., Ctrl+P to toggle Quick Open while it's open)
        if result == InputResult::Ignored {
            return InputResult::Ignored;
        }

        // If modal and result is not explicitly Ignored, consume to prevent leaking
        if self.is_modal() {
            return InputResult::Consumed;
        }

        InputResult::Ignored
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crossterm::event::{KeyCode, KeyModifiers};

    #[test]
    fn test_is_consumed() {
        assert!(InputResult::Consumed.is_consumed());
        assert!(!InputResult::Ignored.is_consumed());
    }

    /// Test handler that tracks what it returns
    struct TestModalHandler {
        returns_ignored: bool,
    }

    impl InputHandler for TestModalHandler {
        fn handle_key_event(&mut self, _event: &KeyEvent, _ctx: &mut InputContext) -> InputResult {
            if self.returns_ignored {
                InputResult::Ignored
            } else {
                InputResult::Consumed
            }
        }

        fn is_modal(&self) -> bool {
            true
        }
    }

    #[test]
    fn test_modal_handler_respects_ignored() {
        // When modal handler returns Ignored, dispatch_input should also return Ignored
        let mut handler = TestModalHandler {
            returns_ignored: true,
        };
        let mut ctx = InputContext::new();
        let event = KeyEvent::new(KeyCode::Char('p'), KeyModifiers::CONTROL);

        let result = handler.dispatch_input(&event, &mut ctx);
        assert_eq!(
            result,
            InputResult::Ignored,
            "Modal handler should respect Ignored result"
        );
    }

    #[test]
    fn test_modal_handler_consumes_unknown_keys() {
        // When modal handler returns Consumed, dispatch_input should also return Consumed
        let mut handler = TestModalHandler {
            returns_ignored: false,
        };
        let mut ctx = InputContext::new();
        let event = KeyEvent::new(KeyCode::Char('x'), KeyModifiers::NONE);

        let result = handler.dispatch_input(&event, &mut ctx);
        assert_eq!(
            result,
            InputResult::Consumed,
            "Modal handler should consume handled keys"
        );
    }
}
