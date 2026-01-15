//! High-level GPM client for connecting to the daemon and reading events

use super::ffi::{self, GpmConnect, GpmEventRaw, GpmLib};
use super::types::{GpmButtons, GpmEvent, GpmModifiers};
use std::io;
use std::os::unix::io::RawFd;

/// GPM client connection
pub struct GpmClient {
    fd: RawFd,
    lib: &'static GpmLib,
}

impl GpmClient {
    /// Try to connect to GPM daemon
    ///
    /// Returns `Ok(Some(client))` if connected successfully,
    /// `Ok(None)` if GPM is not available (e.g., running in xterm, no GPM daemon, or libgpm not installed),
    /// `Err` on unexpected error.
    pub fn connect() -> io::Result<Option<Self>> {
        tracing::debug!("GPM: Attempting to connect...");

        // First check if libgpm is available
        let Some(lib) = ffi::get_gpm_lib() else {
            tracing::debug!("GPM: libgpm not available on this system");
            return Ok(None);
        };
        tracing::debug!("GPM: libgpm loaded successfully");

        // Check if we're on a Linux virtual console
        let is_console = Self::is_linux_console();
        tracing::debug!("GPM: is_linux_console() = {}", is_console);
        if !is_console {
            tracing::debug!("GPM: Not a Linux console, skipping GPM");
            return Ok(None);
        }

        // Try to connect to the GPM daemon
        let mut conn = GpmConnect {
            // Request all mouse events
            event_mask: ffi::GPM_MOVE
                | ffi::GPM_DRAG
                | ffi::GPM_DOWN
                | ffi::GPM_UP
                | ffi::GPM_SINGLE
                | ffi::GPM_DOUBLE
                | ffi::GPM_TRIPLE,
            // Let GPM handle events we don't want (none in our case)
            default_mask: 0,
            // Accept events with any modifier combination
            min_mod: 0,
            max_mod: !0,
            pid: 0, // Let GPM fill this
            vc: 0,  // Current virtual console
        };

        tracing::debug!("GPM: Calling Gpm_Open...");
        let result = lib.open(&mut conn);
        tracing::debug!("GPM: Gpm_Open returned {}", result);

        match result {
            -2 => {
                // Running in xterm or similar - use xterm mouse protocol instead
                tracing::debug!("GPM: Reports xterm mode (-2), using standard mouse protocol");
                Ok(None)
            }
            -1 => {
                // Error connecting to GPM (daemon not running, permission denied, etc.)
                let err = io::Error::last_os_error();
                tracing::debug!("GPM: Connection failed (-1): {}", err);
                Ok(None) // Don't treat as error, just fall back to no GPM
            }
            fd if fd >= 0 => {
                tracing::info!("GPM: Connected successfully, fd={}", fd);
                // Enable GPM's built-in pointer drawing
                lib.set_visible_pointer(true);
                Ok(Some(Self { fd, lib }))
            }
            _ => {
                // Unexpected return value
                tracing::warn!("GPM: Unexpected Gpm_Open return value: {}", result);
                Ok(None)
            }
        }
    }

    /// Get the file descriptor for use with poll/select
    pub fn fd(&self) -> RawFd {
        self.fd
    }

    /// Read a GPM event (call only when poll indicates data is ready)
    pub fn read_event(&self) -> io::Result<Option<GpmEvent>> {
        let mut raw = GpmEventRaw {
            buttons: 0,
            modifiers: 0,
            vc: 0,
            dx: 0,
            dy: 0,
            x: 0,
            y: 0,
            event_type: 0,
            clicks: 0,
            margin: 0,
            wdx: 0,
            wdy: 0,
        };

        let result = self.lib.get_event(&mut raw);

        match result {
            1 => {
                // Event received
                let event = GpmEvent {
                    buttons: GpmButtons(raw.buttons),
                    modifiers: GpmModifiers(raw.modifiers),
                    // GPM uses 1-based coordinates, convert to 0-based
                    x: raw.x.saturating_sub(1),
                    y: raw.y.saturating_sub(1),
                    dx: raw.dx,
                    dy: raw.dy,
                    event_type: raw.event_type as u32,
                    clicks: raw.clicks,
                    wdx: raw.wdx,
                    wdy: raw.wdy,
                };
                tracing::trace!(
                    "GPM event: x={}, y={}, buttons={:?}, type=0x{:x}, wdy={}",
                    event.x,
                    event.y,
                    event.buttons.0,
                    event.event_type,
                    event.wdy
                );
                Ok(Some(event))
            }
            0 => {
                // No event available
                Ok(None)
            }
            _ => {
                // Error
                Err(io::Error::last_os_error())
            }
        }
    }

    /// Check if we're running on a Linux virtual console (TTY)
    fn is_linux_console() -> bool {
        use std::fs;
        use std::io;

        // Check if stdin is a TTY
        let is_tty = nix::unistd::isatty(io::stdin()).unwrap_or(false);
        tracing::debug!("GPM: stdin isatty = {}", is_tty);
        if !is_tty {
            return false;
        }

        // Check if we're on a Linux virtual console (/dev/tty[0-9]+)
        // by checking the TTY name
        match fs::read_link("/proc/self/fd/0") {
            Ok(tty_path) => {
                let tty_str = tty_path.to_string_lossy();
                tracing::debug!("GPM: stdin tty path = {}", tty_str);

                // Linux virtual consoles are /dev/tty1, /dev/tty2, etc.
                // Pseudo-terminals are /dev/pts/0, /dev/pts/1, etc.
                if tty_str.starts_with("/dev/tty") && !tty_str.starts_with("/dev/ttyS") {
                    // Check if it's a numbered tty (not just /dev/tty which is the controlling terminal)
                    let suffix = &tty_str[8..];
                    tracing::debug!("GPM: tty suffix = '{}'", suffix);
                    if suffix.chars().all(|c| c.is_ascii_digit()) && !suffix.is_empty() {
                        tracing::debug!("GPM: Detected Linux console: {}", tty_str);
                        return true;
                    }
                }
                tracing::debug!(
                    "GPM: Not a Linux virtual console (tty path doesn't match pattern)"
                );
                false
            }
            Err(e) => {
                tracing::debug!("GPM: Failed to read /proc/self/fd/0 link: {}", e);
                false
            }
        }
    }
}

impl Drop for GpmClient {
    fn drop(&mut self) {
        self.lib.close();
        tracing::debug!("GPM connection closed");
    }
}
