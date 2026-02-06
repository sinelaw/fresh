//! Input handler for terminal mode.
//!
//! This handler processes input when the editor is in terminal mode,
//! routing keys to the terminal PTY or handling UI keybindings.

use crate::input::handler::{DeferredAction, InputContext, InputHandler, InputResult};
use crate::input::keybindings::{Action, KeybindingResolver};
use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

/// Input handler for terminal mode.
///
/// When active, this handler:
/// - F9 always toggles keyboard capture mode
/// - With keyboard capture ON: all keys go to terminal
/// - With keyboard capture OFF: UI keybindings are checked first
/// - Handles terminal escape, split navigation, scrollback mode
pub struct TerminalModeInputHandler<'a> {
    keyboard_capture: bool,
    keybindings: &'a KeybindingResolver,
}

impl<'a> TerminalModeInputHandler<'a> {
    pub fn new(keyboard_capture: bool, keybindings: &'a KeybindingResolver) -> Self {
        Self {
            keyboard_capture,
            keybindings,
        }
    }
}

impl InputHandler for TerminalModeInputHandler<'_> {
    fn handle_key_event(&mut self, event: &KeyEvent, ctx: &mut InputContext) -> InputResult {
        let code = event.code;
        let modifiers = event.modifiers;

        // F9 always toggles keyboard capture mode (works even when capture is ON)
        if code == KeyCode::F(9) {
            ctx.defer(DeferredAction::ToggleKeyboardCapture);
            return InputResult::Consumed;
        }

        // When keyboard capture is ON, forward ALL keys to terminal
        if self.keyboard_capture {
            ctx.defer(DeferredAction::SendTerminalKey(code, modifiers));
            return InputResult::Consumed;
        }

        // When keyboard capture is OFF, check for UI keybindings first
        let ui_action = self.keybindings.resolve_terminal_ui_action(event);

        if !matches!(ui_action, Action::None) {
            // Handle terminal escape specially - exits terminal mode
            if matches!(ui_action, Action::TerminalEscape) {
                ctx.defer(DeferredAction::ExitTerminalMode { explicit: true });
                return InputResult::Consumed;
            }

            // For split navigation, exit terminal mode first (non-explicit)
            if matches!(
                ui_action,
                Action::NextSplit | Action::PrevSplit | Action::CloseSplit
            ) {
                ctx.defer(DeferredAction::ExitTerminalMode { explicit: false });
            }

            ctx.defer(DeferredAction::ExecuteAction(ui_action));
            return InputResult::Consumed;
        }

        // Handle scrollback: Shift+PageUp exits terminal mode and enters scrollback
        if modifiers.contains(KeyModifiers::SHIFT) && code == KeyCode::PageUp {
            ctx.defer(DeferredAction::EnterScrollbackMode);
            return InputResult::Consumed;
        }

        // Forward all other keys to the terminal
        ctx.defer(DeferredAction::SendTerminalKey(code, modifiers));
        InputResult::Consumed
    }

    fn is_modal(&self) -> bool {
        true
    }
}

/// Check if a key combination should re-enter terminal mode from read-only view.
/// Returns true if the key should trigger terminal mode entry.
///
/// In scrollback mode, any plain character key (without Ctrl/Alt modifiers)
/// exits scrollback and returns to terminal mode. This matches the behavior
/// of most terminal emulators where typing resumes the live terminal.
pub fn should_enter_terminal_mode(event: &KeyEvent) -> bool {
    let code = event.code;
    let modifiers = event.modifiers;

    // Ctrl+Space, Ctrl+], or Ctrl+` to enter terminal mode
    if modifiers.contains(KeyModifiers::CONTROL) {
        return matches!(
            code,
            KeyCode::Char(' ') | KeyCode::Char(']') | KeyCode::Char('`')
        );
    }

    // Any plain character key (no Ctrl/Alt) exits scrollback mode
    // This includes Enter, Tab, Backspace, and regular characters
    if !modifiers.contains(KeyModifiers::ALT) {
        return matches!(
            code,
            KeyCode::Char(_) | KeyCode::Enter | KeyCode::Tab | KeyCode::Backspace
        );
    }

    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::Config;

    fn key(code: KeyCode) -> KeyEvent {
        KeyEvent::new(code, KeyModifiers::NONE)
    }

    fn key_with_mods(code: KeyCode, modifiers: KeyModifiers) -> KeyEvent {
        KeyEvent::new(code, modifiers)
    }

    fn make_resolver() -> KeybindingResolver {
        let config = Config::default();
        KeybindingResolver::new(&config)
    }

    #[test]
    fn test_f9_toggles_capture() {
        let resolver = make_resolver();
        let mut handler = TerminalModeInputHandler::new(false, &resolver);
        let mut ctx = InputContext::new();

        let result = handler.handle_key_event(&key(KeyCode::F(9)), &mut ctx);
        assert!(matches!(result, InputResult::Consumed));
        assert_eq!(ctx.deferred_actions.len(), 1);
        assert!(matches!(
            ctx.deferred_actions[0],
            DeferredAction::ToggleKeyboardCapture
        ));
    }

    #[test]
    fn test_keyboard_capture_forwards_all_keys() {
        let resolver = make_resolver();
        let mut handler = TerminalModeInputHandler::new(true, &resolver);
        let mut ctx = InputContext::new();

        let result = handler.handle_key_event(&key(KeyCode::Char('a')), &mut ctx);
        assert!(matches!(result, InputResult::Consumed));
        assert_eq!(ctx.deferred_actions.len(), 1);
        assert!(matches!(
            ctx.deferred_actions[0],
            DeferredAction::SendTerminalKey(KeyCode::Char('a'), KeyModifiers::NONE)
        ));
    }

    #[test]
    fn test_shift_pageup_enters_scrollback() {
        let resolver = make_resolver();
        let mut handler = TerminalModeInputHandler::new(false, &resolver);
        let mut ctx = InputContext::new();

        let result = handler.handle_key_event(
            &key_with_mods(KeyCode::PageUp, KeyModifiers::SHIFT),
            &mut ctx,
        );
        assert!(matches!(result, InputResult::Consumed));
        assert_eq!(ctx.deferred_actions.len(), 1);
        assert!(matches!(
            ctx.deferred_actions[0],
            DeferredAction::EnterScrollbackMode
        ));
    }

    #[test]
    fn test_regular_keys_forwarded() {
        let resolver = make_resolver();
        let mut handler = TerminalModeInputHandler::new(false, &resolver);
        let mut ctx = InputContext::new();

        let result = handler.handle_key_event(&key(KeyCode::Char('x')), &mut ctx);
        assert!(matches!(result, InputResult::Consumed));
        assert_eq!(ctx.deferred_actions.len(), 1);
        assert!(matches!(
            ctx.deferred_actions[0],
            DeferredAction::SendTerminalKey(KeyCode::Char('x'), KeyModifiers::NONE)
        ));
    }

    #[test]
    fn test_should_enter_terminal_mode() {
        // Ctrl+Space
        assert!(should_enter_terminal_mode(&key_with_mods(
            KeyCode::Char(' '),
            KeyModifiers::CONTROL
        )));
        // Ctrl+]
        assert!(should_enter_terminal_mode(&key_with_mods(
            KeyCode::Char(']'),
            KeyModifiers::CONTROL
        )));
        // Ctrl+`
        assert!(should_enter_terminal_mode(&key_with_mods(
            KeyCode::Char('`'),
            KeyModifiers::CONTROL
        )));
        // 'q' alone
        assert!(should_enter_terminal_mode(&key(KeyCode::Char('q'))));
        // Any plain character key should exit scrollback (issue #863)
        assert!(should_enter_terminal_mode(&key(KeyCode::Char('a'))));
        assert!(should_enter_terminal_mode(&key(KeyCode::Enter)));
        assert!(should_enter_terminal_mode(&key(KeyCode::Tab)));
        assert!(should_enter_terminal_mode(&key(KeyCode::Backspace)));
        // Navigation keys should NOT exit scrollback (used for scrolling)
        assert!(!should_enter_terminal_mode(&key(KeyCode::PageUp)));
        assert!(!should_enter_terminal_mode(&key(KeyCode::PageDown)));
        assert!(!should_enter_terminal_mode(&key(KeyCode::Up)));
        assert!(!should_enter_terminal_mode(&key(KeyCode::Down)));
        // Alt+key should NOT exit scrollback (used for UI keybindings)
        assert!(!should_enter_terminal_mode(&key_with_mods(
            KeyCode::Char('a'),
            KeyModifiers::ALT
        )));
    }

    #[test]
    fn test_is_modal() {
        let resolver = make_resolver();
        let handler = TerminalModeInputHandler::new(false, &resolver);
        assert!(handler.is_modal());
    }
}
