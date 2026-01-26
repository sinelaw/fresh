//! Hierarchical Input Handling System
//!
//! This module provides a tree-based input dispatch system where input events
//! flow through a hierarchy of handlers. The design follows these principles:
//!
//! 1. **Leaf-first, bubble up**: Input is dispatched to the deepest focused
//!    element first. If not consumed, it bubbles up to parents.
//!
//! 2. **Explicit consumption**: Handlers return `InputResult::Consumed` to stop
//!    propagation or `InputResult::Ignored` to let parents try.
//!
//! 3. **Modals consume by default**: Modal dialogs (Settings, Prompts) should
//!    return `Consumed` for unhandled keys to prevent input leakage.
//!
//! 4. **No capture phase**: Unlike DOM events, there's no capture phase.
//!    This keeps the model simple and predictable.
//!
//! ## Example
//!
//! ```ignore
//! impl InputHandler for MyPanel {
//!     fn handle_input(&mut self, event: &KeyEvent, ctx: &mut InputContext) -> InputResult {
//!         // Let focused child try first
//!         if let Some(child) = self.focused_child_mut() {
//!             if child.handle_input(event, ctx) == InputResult::Consumed {
//!                 return InputResult::Consumed;
//!             }
//!         }
//!
//!         // Handle at this level
//!         match event.code {
//!             KeyCode::Up => { self.move_up(); InputResult::Consumed }
//!             KeyCode::Down => { self.move_down(); InputResult::Consumed }
//!             _ => InputResult::Ignored // Let parent handle
//!         }
//!     }
//! }
//! ```

use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

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

    /// Combines two results - consumed if either is consumed.
    pub fn or(self, other: InputResult) -> InputResult {
        if self == InputResult::Consumed || other == InputResult::Consumed {
            InputResult::Consumed
        } else {
            InputResult::Ignored
        }
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
    /// Enter key in completion popup - may confirm or insert newline based on config
    CompletionEnterKey,
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

/// Trait for elements that can handle input events.
///
/// Implementors should:
/// 1. First delegate to `focused_child_mut()` if it exists
/// 2. Handle keys relevant to this element
/// 3. Return `Consumed` or `Ignored` appropriately
/// 4. Modal elements should return `Consumed` for unhandled keys
pub trait InputHandler {
    /// Handle a key event. Returns whether the event was consumed.
    fn handle_key_event(&mut self, event: &KeyEvent, ctx: &mut InputContext) -> InputResult;

    /// Get the currently focused child handler, if any.
    fn focused_child(&self) -> Option<&dyn InputHandler> {
        None
    }

    /// Get the currently focused child handler mutably, if any.
    fn focused_child_mut(&mut self) -> Option<&mut dyn InputHandler> {
        None
    }

    /// Whether this handler is modal (consumes all unhandled input).
    fn is_modal(&self) -> bool {
        false
    }

    /// Dispatch input through this handler and its children.
    /// This is the main entry point - it handles the bubble-up logic.
    fn dispatch_input(&mut self, event: &KeyEvent, ctx: &mut InputContext) -> InputResult {
        // First, let the deepest focused child try
        if let Some(child) = self.focused_child_mut() {
            let result = child.dispatch_input(event, ctx);
            if result == InputResult::Consumed {
                return InputResult::Consumed;
            }
        }

        // Child didn't consume, try this handler
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

/// Helper to check for common key combinations.
pub fn is_key(event: &KeyEvent, code: KeyCode) -> bool {
    event.code == code && event.modifiers.is_empty()
}

pub fn is_key_with_ctrl(event: &KeyEvent, c: char) -> bool {
    event.code == KeyCode::Char(c) && event.modifiers == KeyModifiers::CONTROL
}

pub fn is_key_with_shift(event: &KeyEvent, code: KeyCode) -> bool {
    event.code == code && event.modifiers == KeyModifiers::SHIFT
}

pub fn is_key_with_alt(event: &KeyEvent, code: KeyCode) -> bool {
    event.code == code && event.modifiers == KeyModifiers::ALT
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_input_result_or() {
        assert_eq!(
            InputResult::Consumed.or(InputResult::Consumed),
            InputResult::Consumed
        );
        assert_eq!(
            InputResult::Consumed.or(InputResult::Ignored),
            InputResult::Consumed
        );
        assert_eq!(
            InputResult::Ignored.or(InputResult::Consumed),
            InputResult::Consumed
        );
        assert_eq!(
            InputResult::Ignored.or(InputResult::Ignored),
            InputResult::Ignored
        );
    }

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
