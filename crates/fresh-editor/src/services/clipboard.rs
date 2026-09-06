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
use std::time::Duration;

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
///
/// NOTE: Only the COPY path touches this static. Synchronous PASTE reads use
/// a fresh `arboard::Clipboard` in a timeout-bounded thread (see
/// `read_system_clipboard_with_timeout`) so that a hung X11 selection owner
/// can never wedge this mutex and freeze every subsequent clipboard call
/// (issue #2155).
static SYSTEM_CLIPBOARD: Mutex<Option<arboard::Clipboard>> = Mutex::new(None);

/// Maximum time a synchronous `Clipboard::paste()` will block waiting for the
/// system clipboard. A normal arboard read completes in single-digit
/// milliseconds; anything past this generally means the X11 CLIPBOARD owner is
/// unresponsive (e.g. a recently-closed window whose process is gone, or the
/// case in issue #2155 where another window is ignoring `SelectionRequest`s).
/// Without this cap the read would hang the UI thread indefinitely.
///
/// The async buffer-paste path (`Editor::paste`) keeps its own deadline; this
/// constant only governs the synchronous fallbacks (prompt paste, terminal
/// paste, focused-widget paste, settings-dialog paste, and the no-bridge
/// bootstrap path).
pub(crate) const PASTE_SYNC_TIMEOUT: Duration = Duration::from_millis(500);

/// Run `reader` on a background thread and wait up to `timeout` for its
/// result. Returns `None` if the reader doesn't finish in time.
///
/// Existence of this helper — rather than calling `arboard` directly — is
/// what bounds the worst-case stall on a hung X11 clipboard owner. The
/// reader thread is allowed to leak: it will complete eventually (when the
/// owner finally responds or the process exits) and its result is dropped.
/// This is acceptable because the timeout fires before pending threads
/// pile up faster than they drain in any realistic usage pattern.
///
/// The closure is generic so tests can substitute a deterministic blocker
/// (e.g. `std::thread::park`) without depending on a real X11 server.
fn read_with_timeout<F>(reader: F, timeout: Duration) -> Option<String>
where
    F: FnOnce() -> Option<String> + Send + 'static,
{
    let (tx, rx) = std::sync::mpsc::sync_channel::<Option<String>>(1);
    let spawned = std::thread::Builder::new()
        .name("clipboard-paste-sync".into())
        .spawn(move || {
            // Receiver may be dropped if the caller already timed out.
            #[allow(clippy::let_underscore_must_use)]
            let _ = tx.send(reader());
        });
    if spawned.is_err() {
        // OS refused a new thread; degrade to no-op rather than blocking.
        return None;
    }
    rx.recv_timeout(timeout).ok().flatten()
}

/// Read text from the system clipboard with a timeout, using a fresh
/// `arboard::Clipboard` to avoid contending on the `SYSTEM_CLIPBOARD` mutex.
///
/// Delegates to `read_system_clipboard` so the timeout-bounded path keeps the
/// same backend coverage (arboard, then the Termux helper) as the direct read.
fn read_system_clipboard_with_timeout(timeout: Duration) -> Option<String> {
    read_with_timeout(read_system_clipboard, timeout)
}

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
    ///
    /// The system-clipboard read is bounded by `PASTE_SYNC_TIMEOUT`: when an
    /// X11 selection owner is unresponsive (see issue #2155), the read would
    /// otherwise block the UI thread indefinitely. On timeout we fall back to
    /// the internal clipboard so the user still gets *something* meaningful
    /// (and Ctrl+V doesn't appear to do nothing).
    pub fn paste(&mut self) -> Option<String> {
        // In internal-only mode, skip system clipboard entirely
        if self.internal_only {
            return self.paste_internal();
        }

        // Read from the system clipboard (arboard on desktop, the
        // termux-clipboard-get helper on Termux where arboard has no backend),
        // bounded by PASTE_SYNC_TIMEOUT so a hung selection owner can't wedge
        // the UI thread (issue #2155).
        if self.use_system_clipboard {
            if let Some(text) = read_system_clipboard_with_timeout(PASTE_SYNC_TIMEOUT) {
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
    ///
    /// Uses the same timeout-bounded read as `paste()` so a hung clipboard
    /// owner can't wedge this call either (issue #2155).
    pub fn is_empty(&self) -> bool {
        if !self.internal.is_empty() {
            return false;
        }

        // Check the system clipboard (arboard / termux-clipboard-get),
        // timeout-bounded so a hung owner can't wedge this call (issue #2155).
        if self.use_system_clipboard {
            if let Some(text) = read_system_clipboard_with_timeout(PASTE_SYNC_TIMEOUT) {
                return text.is_empty();
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

    /// Issue #2155: a reader that never returns (modelling a hung X11
    /// CLIPBOARD owner) must NOT block the caller — the timeout fires and
    /// `None` is returned. Without the timeout this test would hang
    /// forever (and `cargo nextest` would kill it externally).
    #[test]
    fn read_with_timeout_returns_none_when_reader_blocks() {
        let result = read_with_timeout(
            || {
                std::thread::park();
                unreachable!("parked thread should never proceed");
            },
            Duration::from_millis(50),
        );
        assert!(result.is_none(), "hung reader must yield None");
    }

    /// The happy-path timeout helper still delivers a fast reader's value.
    #[test]
    fn read_with_timeout_returns_fast_reader_value() {
        let result = read_with_timeout(|| Some("hello".to_string()), Duration::from_millis(500));
        assert_eq!(result.as_deref(), Some("hello"));
    }

    /// An empty / "no clipboard text" reader maps to `None`, distinguishing
    /// "nothing on the clipboard" from "the read timed out" at the caller's
    /// granularity (both fall back to the internal clipboard).
    #[test]
    fn read_with_timeout_returns_none_for_reader_returning_none() {
        let result = read_with_timeout(|| None, Duration::from_millis(500));
        assert!(result.is_none());
    }
}
