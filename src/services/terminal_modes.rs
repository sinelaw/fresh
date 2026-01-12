//! Terminal mode management
//!
//! This module handles enabling and disabling various terminal modes:
//! - Raw mode
//! - Alternate screen
//! - Mouse capture
//! - Keyboard enhancement flags
//! - Bracketed paste
//!
//! It provides a `TerminalModes` struct that tracks which modes were enabled
//! and can restore the terminal to its original state via the `undo()` method.

use anyhow::Result;
use crossterm::{
    cursor::SetCursorStyle,
    event::{
        DisableBracketedPaste, DisableMouseCapture, EnableBracketedPaste, EnableMouseCapture,
        KeyboardEnhancementFlags, PopKeyboardEnhancementFlags, PushKeyboardEnhancementFlags,
    },
    terminal::{
        disable_raw_mode, enable_raw_mode, supports_keyboard_enhancement, EnterAlternateScreen,
        LeaveAlternateScreen,
    },
    ExecutableCommand,
};
use std::io::{stdout, Write};

/// Tracks which terminal modes have been enabled and provides cleanup.
///
/// Use `TerminalModes::enable()` to set up the terminal, then call `undo()`
/// to restore the original state (e.g., on exit or panic).
#[derive(Debug, Default)]
pub struct TerminalModes {
    raw_mode: bool,
    alternate_screen: bool,
    mouse_capture: bool,
    keyboard_enhancement: bool,
    bracketed_paste: bool,
}

impl TerminalModes {
    /// Create a new TerminalModes with nothing enabled.
    pub fn new() -> Self {
        Self::default()
    }

    /// Enable all terminal modes, checking support for each.
    ///
    /// Returns Ok(Self) with tracked state of what was enabled.
    /// On error, automatically undoes any partially enabled modes.
    pub fn enable() -> Result<Self> {
        let mut modes = Self::new();

        // Enable raw mode
        if let Err(e) = enable_raw_mode() {
            tracing::error!("Failed to enable raw mode: {}", e);
            return Err(e.into());
        }
        modes.raw_mode = true;
        tracing::debug!("Enabled raw mode");

        // Check and enable keyboard enhancement flags
        match supports_keyboard_enhancement() {
            Ok(true) => {
                let flags = KeyboardEnhancementFlags::DISAMBIGUATE_ESCAPE_CODES
                    | KeyboardEnhancementFlags::REPORT_ALTERNATE_KEYS;
                if let Err(e) = stdout().execute(PushKeyboardEnhancementFlags(flags)) {
                    tracing::warn!("Failed to enable keyboard enhancement: {}", e);
                    // Non-fatal, continue without it
                } else {
                    modes.keyboard_enhancement = true;
                    tracing::debug!("Enabled keyboard enhancement flags");
                }
            }
            Ok(false) => {
                tracing::info!("Keyboard enhancement not supported by terminal");
            }
            Err(e) => {
                tracing::warn!("Failed to query keyboard enhancement support: {}", e);
            }
        }

        // Enable alternate screen
        if let Err(e) = stdout().execute(EnterAlternateScreen) {
            tracing::error!("Failed to enter alternate screen: {}", e);
            modes.undo();
            return Err(e.into());
        }
        modes.alternate_screen = true;
        tracing::debug!("Entered alternate screen");

        // Enable mouse capture
        if let Err(e) = stdout().execute(EnableMouseCapture) {
            tracing::warn!("Failed to enable mouse capture: {}", e);
            // Non-fatal, continue without it
        } else {
            modes.mouse_capture = true;
            tracing::debug!("Enabled mouse capture");
        }

        // Enable bracketed paste
        if let Err(e) = stdout().execute(EnableBracketedPaste) {
            tracing::warn!("Failed to enable bracketed paste: {}", e);
            // Non-fatal, continue without it
        } else {
            modes.bracketed_paste = true;
            tracing::debug!("Enabled bracketed paste mode");
        }

        Ok(modes)
    }

    /// Restore terminal to original state by disabling all enabled modes.
    ///
    /// This is safe to call multiple times - it tracks what was enabled
    /// and only disables those modes.
    pub fn undo(&mut self) {
        // Disable mouse capture
        if self.mouse_capture {
            let _ = stdout().execute(DisableMouseCapture);
            self.mouse_capture = false;
            tracing::debug!("Disabled mouse capture");
        }

        // Disable bracketed paste
        if self.bracketed_paste {
            let _ = stdout().execute(DisableBracketedPaste);
            self.bracketed_paste = false;
            tracing::debug!("Disabled bracketed paste");
        }

        // Reset cursor style to default
        let _ = stdout().execute(SetCursorStyle::DefaultUserShape);

        // Reset terminal cursor color
        crate::view::theme::Theme::reset_terminal_cursor_color();

        // Pop keyboard enhancement flags
        if self.keyboard_enhancement {
            let _ = stdout().execute(PopKeyboardEnhancementFlags);
            self.keyboard_enhancement = false;
            tracing::debug!("Popped keyboard enhancement flags");
        }

        // Disable raw mode (before leaving alternate screen for cleaner output)
        if self.raw_mode {
            let _ = disable_raw_mode();
            self.raw_mode = false;
            tracing::debug!("Disabled raw mode");
        }

        // Leave alternate screen last
        if self.alternate_screen {
            let _ = stdout().execute(LeaveAlternateScreen);
            self.alternate_screen = false;
            tracing::debug!("Left alternate screen");
        }

        // Flush stdout to ensure all escape sequences are sent
        let _ = stdout().flush();
    }

    /// Returns true if raw mode is enabled.
    pub fn raw_mode_enabled(&self) -> bool {
        self.raw_mode
    }

    /// Returns true if keyboard enhancement is enabled.
    pub fn keyboard_enhancement_enabled(&self) -> bool {
        self.keyboard_enhancement
    }

    /// Returns true if mouse capture is enabled.
    pub fn mouse_capture_enabled(&self) -> bool {
        self.mouse_capture
    }

    /// Returns true if bracketed paste is enabled.
    pub fn bracketed_paste_enabled(&self) -> bool {
        self.bracketed_paste
    }

    /// Returns true if alternate screen is enabled.
    pub fn alternate_screen_enabled(&self) -> bool {
        self.alternate_screen
    }
}

impl Drop for TerminalModes {
    fn drop(&mut self) {
        self.undo();
    }
}

/// Unconditionally restore terminal state without tracking.
///
/// This is intended for use in panic hooks where we don't have access
/// to the TerminalModes instance. It attempts to disable all modes
/// regardless of whether they were actually enabled.
pub fn emergency_cleanup() {
    // Disable mouse capture
    let _ = stdout().execute(DisableMouseCapture);

    // Disable bracketed paste
    let _ = stdout().execute(DisableBracketedPaste);

    // Reset cursor style to default
    let _ = stdout().execute(SetCursorStyle::DefaultUserShape);

    // Reset terminal cursor color
    crate::view::theme::Theme::reset_terminal_cursor_color();

    // Pop keyboard enhancement flags
    let _ = stdout().execute(PopKeyboardEnhancementFlags);

    // Disable raw mode
    let _ = disable_raw_mode();

    // Leave alternate screen
    let _ = stdout().execute(LeaveAlternateScreen);

    // Flush stdout
    let _ = stdout().flush();
}
