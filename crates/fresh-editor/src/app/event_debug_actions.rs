//! Event debug dialog action handling
//!
//! This module provides the action handlers for the event debug dialog.

use super::event_debug::EventDebug;
use super::Editor;
use crate::input::handler::InputResult;
use crossterm::event::KeyEvent;
use rust_i18n::t;

impl Editor {
    /// Open the event debug dialog
    pub fn open_event_debug(&mut self) {
        self.event_debug = Some(EventDebug::new());
        self.set_status_message(t!("event_debug.started").to_string());
    }

    /// Handle input when event debug dialog is active
    pub fn handle_event_debug_input(&mut self, event: &KeyEvent) -> InputResult {
        // Take the dialog temporarily to avoid borrowing issues
        let mut debug = match self.event_debug.take() {
            Some(d) => d,
            None => return InputResult::Ignored,
        };

        // Record the event
        debug.record_event(*event);

        // Check if we should close
        if debug.should_close() {
            self.set_status_message(t!("event_debug.closed").to_string());
            // Don't put it back - it's closed
        } else {
            // Put it back
            self.event_debug = Some(debug);
        }

        InputResult::Consumed
    }

    /// Check if event debug dialog is active
    pub fn is_event_debug_active(&self) -> bool {
        self.event_debug.is_some()
    }
}
