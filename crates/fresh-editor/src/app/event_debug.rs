//! Event Debug Dialog
//!
//! A dialog for debugging terminal key events. Shows raw key codes and modifiers
//! as they are received from the terminal, helping diagnose keybinding issues.

use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

/// Maximum number of events to display in the history
const MAX_HISTORY: usize = 10;

/// A recorded key event with display information
#[derive(Debug, Clone)]
pub struct RecordedEvent {
    /// The raw key event
    pub event: KeyEvent,
    /// Human-readable description
    pub description: String,
}

impl RecordedEvent {
    fn new(event: KeyEvent) -> Self {
        let description = format_key_event(&event);
        Self { event, description }
    }
}

/// Format a key event for display
fn format_key_event(event: &KeyEvent) -> String {
    let mut parts = Vec::new();

    // Build modifier string
    if event.modifiers.contains(KeyModifiers::CONTROL) {
        parts.push("Ctrl");
    }
    if event.modifiers.contains(KeyModifiers::ALT) {
        parts.push("Alt");
    }
    if event.modifiers.contains(KeyModifiers::SHIFT) {
        parts.push("Shift");
    }
    if event.modifiers.contains(KeyModifiers::SUPER) {
        parts.push("Super");
    }
    if event.modifiers.contains(KeyModifiers::HYPER) {
        parts.push("Hyper");
    }
    if event.modifiers.contains(KeyModifiers::META) {
        parts.push("Meta");
    }

    // Format key code
    let key_str = match event.code {
        KeyCode::Backspace => "Backspace".to_string(),
        KeyCode::Enter => "Enter".to_string(),
        KeyCode::Left => "Left".to_string(),
        KeyCode::Right => "Right".to_string(),
        KeyCode::Up => "Up".to_string(),
        KeyCode::Down => "Down".to_string(),
        KeyCode::Home => "Home".to_string(),
        KeyCode::End => "End".to_string(),
        KeyCode::PageUp => "PageUp".to_string(),
        KeyCode::PageDown => "PageDown".to_string(),
        KeyCode::Tab => "Tab".to_string(),
        KeyCode::BackTab => "BackTab".to_string(),
        KeyCode::Delete => "Delete".to_string(),
        KeyCode::Insert => "Insert".to_string(),
        KeyCode::F(n) => format!("F{}", n),
        KeyCode::Char(c) => {
            if c == ' ' {
                "Space".to_string()
            } else if c.is_control() {
                format!("0x{:02x}", c as u8)
            } else {
                format!("'{}'", c)
            }
        }
        KeyCode::Null => "Null".to_string(),
        KeyCode::Esc => "Esc".to_string(),
        KeyCode::CapsLock => "CapsLock".to_string(),
        KeyCode::ScrollLock => "ScrollLock".to_string(),
        KeyCode::NumLock => "NumLock".to_string(),
        KeyCode::PrintScreen => "PrintScreen".to_string(),
        KeyCode::Pause => "Pause".to_string(),
        KeyCode::Menu => "Menu".to_string(),
        KeyCode::KeypadBegin => "KeypadBegin".to_string(),
        KeyCode::Modifier(m) => format!("Modifier({:?})", m),
        KeyCode::Media(m) => format!("Media({:?})", m),
    };

    parts.push(&key_str);

    // Join with + separator, or just key if no modifiers
    if parts.len() > 1 {
        parts.join("+")
    } else {
        key_str
    }
}

/// The event debug dialog state
#[derive(Debug)]
pub struct EventDebug {
    /// History of recorded events (newest first)
    pub history: Vec<RecordedEvent>,
    /// Whether the dialog is active
    pub active: bool,
}

impl EventDebug {
    /// Create a new event debug dialog
    pub fn new() -> Self {
        Self {
            history: Vec::new(),
            active: true,
        }
    }

    /// Record a new key event
    pub fn record_event(&mut self, event: KeyEvent) {
        // Check for close keys first
        if event.modifiers == KeyModifiers::NONE {
            match event.code {
                KeyCode::Char('q') | KeyCode::Esc => {
                    self.active = false;
                    return;
                }
                KeyCode::Char('c') => {
                    // Clear history
                    self.history.clear();
                    return;
                }
                _ => {}
            }
        }

        // Record the event
        let recorded = RecordedEvent::new(event);
        self.history.insert(0, recorded);

        // Trim history to max size
        if self.history.len() > MAX_HISTORY {
            self.history.truncate(MAX_HISTORY);
        }
    }

    /// Check if the dialog should be closed
    pub fn should_close(&self) -> bool {
        !self.active
    }

    /// Get the raw details for the most recent event
    pub fn last_event_details(&self) -> Option<String> {
        self.history.first().map(|e| {
            format!(
                "code={:?}, modifiers={:?} (bits=0x{:02x}), kind={:?}, state={:?}",
                e.event.code,
                e.event.modifiers,
                e.event.modifiers.bits(),
                e.event.kind,
                e.event.state
            )
        })
    }
}

impl Default for EventDebug {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crossterm::event::KeyEventKind;

    #[test]
    fn test_event_debug_creation() {
        let debug = EventDebug::new();
        assert!(debug.active);
        assert!(debug.history.is_empty());
    }

    #[test]
    fn test_record_event() {
        let mut debug = EventDebug::new();
        let event = KeyEvent::new(KeyCode::Char('a'), KeyModifiers::CONTROL);
        debug.record_event(event);

        assert_eq!(debug.history.len(), 1);
        assert_eq!(debug.history[0].description, "Ctrl+'a'");
    }

    #[test]
    fn test_close_with_q() {
        let mut debug = EventDebug::new();
        let event = KeyEvent::new(KeyCode::Char('q'), KeyModifiers::NONE);
        debug.record_event(event);

        assert!(debug.should_close());
    }

    #[test]
    fn test_close_with_esc() {
        let mut debug = EventDebug::new();
        let event = KeyEvent::new(KeyCode::Esc, KeyModifiers::NONE);
        debug.record_event(event);

        assert!(debug.should_close());
    }

    #[test]
    fn test_clear_with_c() {
        let mut debug = EventDebug::new();

        // Add some events
        debug.record_event(KeyEvent::new(KeyCode::Char('a'), KeyModifiers::NONE));
        debug.record_event(KeyEvent::new(KeyCode::Char('b'), KeyModifiers::NONE));
        assert_eq!(debug.history.len(), 2);

        // Clear with 'c'
        debug.record_event(KeyEvent::new(KeyCode::Char('c'), KeyModifiers::NONE));
        assert!(debug.history.is_empty());
        assert!(debug.active); // Should not close
    }

    #[test]
    fn test_max_history() {
        let mut debug = EventDebug::new();

        // Add more than MAX_HISTORY events
        for i in 0..15 {
            debug.record_event(KeyEvent::new(
                KeyCode::Char((b'a' + i) as char),
                KeyModifiers::NONE,
            ));
        }

        assert_eq!(debug.history.len(), MAX_HISTORY);
    }

    #[test]
    fn test_format_modifiers() {
        let mut debug = EventDebug::new();

        // Ctrl+Shift+Home
        let event = KeyEvent::new(KeyCode::Home, KeyModifiers::CONTROL | KeyModifiers::SHIFT);
        debug.record_event(event);

        assert_eq!(debug.history[0].description, "Ctrl+Shift+Home");
    }
}
