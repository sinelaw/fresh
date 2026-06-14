//! Clipboard module: handles both internal and system clipboard operations
//!
//! This module provides a unified clipboard interface that:
//! - Maintains an internal clipboard for in-editor copy/paste
//! - Uses crossterm's OSC 52 escape sequences for copying to system clipboard
//! - Uses arboard crate for reading from system clipboard
//! - Supports copying HTML-formatted text for rich text editors
//! - Gracefully falls back to internal clipboard if system clipboard is unavailable
//! - Respects clipboard configuration to disable problematic methods

use crossterm::clipboard::CopyToClipboard;
use crossterm::execute;
use std::io::{stdout, Write};
use std::sync::Mutex;

/// True when running inside Termux on Android.
///
/// Termux exports `TERMUX_VERSION` and sets `$PREFIX` to a path under
/// `com.termux`. We use this to gate the `termux-clipboard-*` helpers so we
/// never spawn those processes on a normal Linux/macOS/Windows host.
fn is_termux() -> bool {
    if std::env::var_os("TERMUX_VERSION").is_some() {
        return true;
    }
    std::env::var_os("PREFIX")
        .map(|p| p.to_string_lossy().contains("com.termux"))
        .unwrap_or(false)
}

/// Read the Android clipboard via the `termux-clipboard-get` helper.
///
/// Returns `None` when not on Termux, when the `termux-api` package isn't
/// installed (helper missing / non-zero exit), or when the clipboard is empty.
/// `arboard` has no Android backend, so this is the only way to read the
/// system clipboard inside Termux.
///
/// Deliberately uses `std::process::Command` rather than the editor's
/// `ProcessSpawner`: the clipboard is a property of the *local* Android
/// device, so it must never be routed to a remote SSH/container host, and
/// this runs on the detached background paste thread which has no spawner.
fn termux_clipboard_get() -> Option<String> {
    if !is_termux() {
        return None;
    }
    let output = std::process::Command::new("termux-clipboard-get")
        .output()
        .ok()?;
    if !output.status.success() {
        return None;
    }
    let text = String::from_utf8_lossy(&output.stdout).into_owned();
    if text.is_empty() {
        None
    } else {
        Some(text)
    }
}

/// Write the Android clipboard via the `termux-clipboard-set` helper.
///
/// No-op (returns `false`) when not on Termux or the helper is unavailable.
fn termux_clipboard_set(text: &str) -> bool {
    if !is_termux() {
        return false;
    }
    use std::process::{Command, Stdio};
    let mut child = match Command::new("termux-clipboard-set")
        .stdin(Stdio::piped())
        .spawn()
    {
        Ok(child) => child,
        Err(e) => {
            tracing::debug!("termux-clipboard-set spawn failed: {}", e);
            return false;
        }
    };
    if let Some(mut stdin) = child.stdin.take() {
        if let Err(e) = stdin.write_all(text.as_bytes()) {
            tracing::debug!("termux-clipboard-set write failed: {}", e);
        }
    }
    child.wait().map(|s| s.success()).unwrap_or(false)
}

/// Read text from the system clipboard, trying platform backends in order.
///
/// On Linux/macOS/Windows this is `arboard` (X11/Wayland/native). On Termux,
/// where `arboard` has no backend and always errors, it falls back to the
/// `termux-clipboard-get` helper from the `termux-api` package. Returns `None`
/// when no backend yields non-empty text.
///
/// A fresh `arboard::Clipboard` is created per call (rather than reusing the
/// persistent `SYSTEM_CLIPBOARD` handle) so this is safe to call from the
/// background paste thread without contending on the copy-side mutex.
pub fn read_system_clipboard() -> Option<String> {
    if let Some(text) = arboard::Clipboard::new()
        .and_then(|mut cb| cb.get_text())
        .ok()
        .filter(|s| !s.is_empty())
    {
        return Some(text);
    }
    termux_clipboard_get()
}

/// Global clipboard holder to maintain X11/Wayland clipboard ownership.
///
/// On X11, the clipboard owner must stay alive to respond to paste requests.
/// On Wayland, the data-source is destroyed when dropped.
/// This static keeps the handle alive for the process lifetime.
static SYSTEM_CLIPBOARD: Mutex<Option<arboard::Clipboard>> = Mutex::new(None);

/// Copy text to the system clipboard using OSC 52 and/or arboard.
///
/// This is the shared implementation used by both direct-mode clipboard
/// operations and the client relay in session mode. It:
/// - Sends OSC 52 escape sequences to stdout (if `use_osc52`)
/// - Sets arboard clipboard via a persistent static handle (if `use_system_clipboard`)
///
/// The persistent handle is critical: a temporary arboard::Clipboard would take
/// selection ownership from the terminal (clobbering OSC 52) then destroy the
/// selection/data-source on drop, leaving the clipboard empty.
pub fn copy_to_system_clipboard(text: &str, use_osc52: bool, use_system_clipboard: bool) {
    if use_osc52 {
        if let Err(e) = execute!(stdout(), CopyToClipboard::to_clipboard_from(text)) {
            tracing::debug!("OSC 52 clipboard copy failed: {}", e);
        }
        #[allow(clippy::let_underscore_must_use)]
        let _ = stdout().flush();
    }

    if use_system_clipboard {
        set_system_clipboard_text(text);
        // On Termux arboard is a no-op (no Android backend), so also push to
        // the Android clipboard via the termux-api helper. The call is gated
        // on `is_termux()` inside, so it costs nothing on other platforms.
        termux_clipboard_set(text);
    }
}

/// Set text on the arboard system clipboard, creating it if needed.
fn set_system_clipboard_text(text: &str) {
    if let Ok(mut guard) = SYSTEM_CLIPBOARD.lock() {
        if guard.is_none() {
            match arboard::Clipboard::new() {
                Ok(cb) => *guard = Some(cb),
                Err(e) => {
                    tracing::debug!("arboard clipboard init failed: {}", e);
                    return;
                }
            }
        }
        if let Some(clipboard) = guard.as_mut() {
            if let Err(e) = clipboard.set_text(text) {
                tracing::debug!("arboard copy failed: {}, recreating clipboard", e);
                // If set_text fails, try recreating the clipboard
                drop(guard);
                if let Ok(mut guard) = SYSTEM_CLIPBOARD.lock() {
                    if let Ok(new_clipboard) = arboard::Clipboard::new() {
                        *guard = Some(new_clipboard);
                        if let Some(cb) = guard.as_mut() {
                            #[allow(clippy::let_underscore_must_use)]
                            let _ = cb.set_text(text);
                        }
                    }
                }
            }
        }
    }
}

/// Pending clipboard data to deliver to clients in session mode
#[derive(Debug, Clone)]
pub struct PendingClipboard {
    pub text: String,
    pub use_osc52: bool,
    pub use_system_clipboard: bool,
}

/// Clipboard manager that handles both internal and system clipboard
#[derive(Debug, Clone, Default)]
pub struct Clipboard {
    /// Internal clipboard content (always available)
    internal: String,
    /// When true, paste() uses internal clipboard only (for testing)
    internal_only: bool,
    /// When true, OSC 52 escape sequences are used for clipboard copy
    use_osc52: bool,
    /// When true, system clipboard (arboard/X11/Wayland) is used for copy/paste
    use_system_clipboard: bool,
    /// When true, skip direct stdout writes (OSC 52 / arboard) and queue text
    /// for the server to send to clients via control messages instead
    session_mode: bool,
    /// Clipboard data pending delivery to clients (session mode only)
    pending_clipboard: Option<PendingClipboard>,
}

impl Clipboard {
    /// Create a new empty clipboard with all methods enabled
    pub fn new() -> Self {
        Self {
            internal: String::new(),
            internal_only: false,
            use_osc52: true,
            use_system_clipboard: true,
            session_mode: false,
            pending_clipboard: None,
        }
    }

    /// Update clipboard configuration from editor config.
    /// Called on initialization and when config is reloaded.
    pub fn apply_config(&mut self, config: &crate::config::ClipboardConfig) {
        self.use_osc52 = config.use_osc52;
        self.use_system_clipboard = config.use_system_clipboard;
    }

    /// Enable internal-only mode (for testing)
    /// When enabled, paste() uses internal clipboard only, ignoring system clipboard
    pub fn set_internal_only(&mut self, enabled: bool) {
        self.internal_only = enabled;
    }

    /// True when paste() should bypass the system clipboard entirely
    /// (test mode). Lets the async paste path short-circuit straight to
    /// the internal clipboard without spawning a background read.
    pub fn is_internal_only(&self) -> bool {
        self.internal_only
    }

    /// True when the system clipboard is enabled for reads/writes.
    /// When false, the async paste path skips the background arboard
    /// thread and uses only the internal clipboard.
    pub fn uses_system_clipboard(&self) -> bool {
        self.use_system_clipboard
    }

    /// Enable session mode (server/daemon operation)
    /// When enabled, copy() skips stdout/arboard and queues text for the server
    /// to deliver to clients via control messages
    pub fn set_session_mode(&mut self, enabled: bool) {
        self.session_mode = enabled;
    }

    /// Take pending clipboard data queued in session mode, clearing the request
    pub fn take_pending_clipboard(&mut self) -> Option<PendingClipboard> {
        self.pending_clipboard.take()
    }

    /// Copy HTML-formatted text to the system clipboard
    ///
    /// Uses arboard to copy HTML with a plain text fallback.
    /// This allows pasting styled/colored text into applications that support rich text.
    /// Returns true if successful, false otherwise.
    pub fn copy_html(&mut self, html: &str, plain_text: &str) -> bool {
        self.internal = plain_text.to_string();

        if !self.use_system_clipboard {
            return false;
        }

        if let Ok(mut guard) = SYSTEM_CLIPBOARD.lock() {
            // Create clipboard if it doesn't exist yet
            if guard.is_none() {
                match arboard::Clipboard::new() {
                    Ok(cb) => *guard = Some(cb),
                    Err(e) => {
                        tracing::debug!("arboard clipboard init failed for HTML: {}", e);
                        return false;
                    }
                }
            }

            // Try to set HTML on the clipboard
            if let Some(clipboard) = guard.as_mut() {
                match clipboard.set_html(html, Some(plain_text)) {
                    Ok(()) => {
                        tracing::debug!("HTML copied to clipboard ({} bytes)", html.len());
                        return true;
                    }
                    Err(e) => {
                        tracing::debug!("arboard HTML copy failed: {}", e);
                    }
                }
            }
        }

        false
    }

    /// Copy text to both internal and system clipboard
    ///
    /// Tries multiple methods to maximize compatibility:
    /// 1. OSC 52 escape sequence (works in Konsole, Kitty, Alacritty, Wezterm, xterm, iTerm2)
    /// 2. arboard crate (works via X11/Wayland APIs in Gnome Console, XFCE Terminal, etc.)
    ///
    /// Methods can be disabled via clipboard configuration.
    pub fn copy(&mut self, text: String) {
        self.internal = text.clone();

        // In session mode, the server process has no terminal or display server.
        // Queue the text for delivery to clients via a control message instead.
        if self.session_mode {
            self.pending_clipboard = Some(PendingClipboard {
                text,
                use_osc52: self.use_osc52,
                use_system_clipboard: self.use_system_clipboard,
            });
            return;
        }

        copy_to_system_clipboard(&text, self.use_osc52, self.use_system_clipboard);
    }

    /// Get text from clipboard, preferring system clipboard
    ///
    /// Tries system clipboard first, falls back to internal clipboard.
    /// If internal_only mode is enabled (for testing), skips system clipboard.
    pub fn paste(&mut self) -> Option<String> {
        // In internal-only mode, skip system clipboard entirely
        if self.internal_only {
            return self.paste_internal();
        }

        // Read from the system clipboard (arboard on desktop, the
        // termux-clipboard-get helper on Termux where arboard has no backend).
        if self.use_system_clipboard {
            if let Some(text) = read_system_clipboard() {
                self.internal = text.clone();
                return Some(text);
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

        // Check the system clipboard (arboard / termux-clipboard-get).
        if self.use_system_clipboard && read_system_clipboard().is_some() {
            return false;
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

    #[test]
    fn test_clipboard_config_disables_osc52() {
        let mut clipboard = Clipboard::new();
        let config = crate::config::ClipboardConfig {
            use_osc52: false,
            use_system_clipboard: true,
        };
        clipboard.apply_config(&config);
        assert!(!clipboard.use_osc52);
        assert!(clipboard.use_system_clipboard);
    }

    #[test]
    fn test_clipboard_config_disables_system() {
        let mut clipboard = Clipboard::new();
        let config = crate::config::ClipboardConfig {
            use_osc52: true,
            use_system_clipboard: false,
        };
        clipboard.apply_config(&config);
        assert!(clipboard.use_osc52);
        assert!(!clipboard.use_system_clipboard);
    }

    #[test]
    fn test_clipboard_internal_only_mode() {
        let mut clipboard = Clipboard::new();
        let config = crate::config::ClipboardConfig {
            use_osc52: false,
            use_system_clipboard: false,
        };
        clipboard.apply_config(&config);

        clipboard.copy("internal only".to_string());
        assert_eq!(clipboard.get_internal(), "internal only");
    }
}
