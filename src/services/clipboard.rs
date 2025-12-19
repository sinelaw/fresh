//! Clipboard module: handles both internal and system clipboard operations
//!
//! This module provides a unified clipboard interface that:
//! - Maintains an internal clipboard for in-editor copy/paste
//! - Uses crossterm's OSC 52 escape sequences for copying to system clipboard
//! - Uses arboard crate for reading from system clipboard
//! - Gracefully falls back to internal clipboard if system clipboard is unavailable

use crossterm::clipboard::CopyToClipboard;
use crossterm::execute;
use std::io::{stdout, Write};
use std::sync::Mutex;

/// Global clipboard holder to maintain X11 clipboard ownership for the application lifetime.
/// On X11, the clipboard owner must stay alive to respond to paste requests from other apps.
static SYSTEM_CLIPBOARD: Mutex<Option<arboard::Clipboard>> = Mutex::new(None);

/// Clipboard manager that handles both internal and system clipboard
#[derive(Debug, Clone, Default)]
pub struct Clipboard {
    /// Internal clipboard content (always available)
    internal: String,
}

impl Clipboard {
    /// Create a new empty clipboard
    pub fn new() -> Self {
        Self {
            internal: String::new(),
        }
    }

    /// Copy text to both internal and system clipboard
    ///
    /// Tries multiple methods to maximize compatibility:
    /// 1. OSC 52 escape sequence (works in Konsole, Kitty, Alacritty, Wezterm, xterm, iTerm2)
    /// 2. arboard crate (works via X11/Wayland APIs in Gnome Console, XFCE Terminal, etc.)
    pub fn copy(&mut self, text: String) {
        self.internal = text.clone();

        // Try OSC 52 first (works in modern terminals)
        // Note: This doesn't "fail" in a detectable way - it just sends escape sequences
        // that the terminal may or may not handle
        let osc52_result = execute!(stdout(), CopyToClipboard::to_clipboard_from(&text));
        if let Err(e) = &osc52_result {
            tracing::debug!("Crossterm OSC 52 clipboard copy failed: {}", e);
        }
        // Ensure the escape sequence is flushed to the terminal
        let _ = stdout().flush();

        // Also try arboard (works via X11/Wayland in terminals without OSC 52 support)
        // This provides coverage for Gnome Console, XFCE Terminal, and similar
        //
        // Important: On X11, the clipboard owner must stay alive to respond to paste requests.
        // We store the clipboard in a static so it lives for the application lifetime.
        if let Ok(mut guard) = SYSTEM_CLIPBOARD.lock() {
            // Create clipboard if it doesn't exist yet
            if guard.is_none() {
                match arboard::Clipboard::new() {
                    Ok(cb) => *guard = Some(cb),
                    Err(e) => {
                        tracing::debug!("arboard clipboard init failed: {}", e);
                    }
                }
            }

            // Try to set text on the clipboard
            if let Some(clipboard) = guard.as_mut() {
                if let Err(e) = clipboard.set_text(&text) {
                    tracing::debug!("arboard copy failed: {}, recreating clipboard", e);
                    // If set_text fails, try recreating the clipboard
                    drop(guard);
                    if let Ok(mut guard) = SYSTEM_CLIPBOARD.lock() {
                        if let Ok(new_clipboard) = arboard::Clipboard::new() {
                            *guard = Some(new_clipboard);
                            if let Some(cb) = guard.as_mut() {
                                let _ = cb.set_text(&text);
                            }
                        }
                    }
                }
            }
        }
    }

    /// Get text from clipboard, preferring system clipboard
    ///
    /// Tries system clipboard first, falls back to internal clipboard
    pub fn paste(&mut self) -> Option<String> {
        // Try arboard crate via the static clipboard (reads from system clipboard)
        if let Ok(mut guard) = SYSTEM_CLIPBOARD.lock() {
            // Create clipboard if it doesn't exist yet
            if guard.is_none() {
                if let Ok(cb) = arboard::Clipboard::new() {
                    *guard = Some(cb);
                }
            }

            if let Some(clipboard) = guard.as_mut() {
                if let Ok(text) = clipboard.get_text() {
                    if !text.is_empty() {
                        self.internal = text.clone();
                        return Some(text);
                    }
                }
            }
        }

        // Fall back to internal clipboard
        if self.internal.is_empty() {
            None
        } else {
            Some(self.internal.clone())
        }
    }

    /// Get the internal clipboard content without checking system clipboard
    pub fn get_internal(&self) -> &str {
        &self.internal
    }

    /// Set the internal clipboard content without updating system clipboard
    pub fn set_internal(&mut self, text: String) {
        self.internal = text;
    }

    /// Get text from internal clipboard only (ignores system clipboard)
    /// This is useful for testing where we don't want system clipboard interference
    pub fn paste_internal(&self) -> Option<String> {
        if self.internal.is_empty() {
            None
        } else {
            Some(self.internal.clone())
        }
    }

    /// Check if clipboard is empty (checks both internal and system)
    pub fn is_empty(&self) -> bool {
        if !self.internal.is_empty() {
            return false;
        }

        // Check system clipboard via the static clipboard
        if let Ok(mut guard) = SYSTEM_CLIPBOARD.lock() {
            if guard.is_none() {
                if let Ok(cb) = arboard::Clipboard::new() {
                    *guard = Some(cb);
                }
            }

            if let Some(clipboard) = guard.as_mut() {
                if let Ok(text) = clipboard.get_text() {
                    return text.is_empty();
                }
            }
        }

        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_clipboard_internal() {
        let mut clipboard = Clipboard::new();
        assert!(clipboard.get_internal().is_empty());

        clipboard.set_internal("test".to_string());
        assert_eq!(clipboard.get_internal(), "test");
    }

    #[test]
    fn test_clipboard_copy_updates_internal() {
        let mut clipboard = Clipboard::new();
        clipboard.copy("hello".to_string());
        assert_eq!(clipboard.get_internal(), "hello");
    }
}
