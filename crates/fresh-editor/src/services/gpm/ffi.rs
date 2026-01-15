//! Dynamic FFI bindings to libgpm using dlopen
//!
//! This module loads libgpm.so at runtime if available, allowing the application
//! to run on systems without GPM installed while still providing GPM support
//! when the library is present.

use libloading::{Library, Symbol};
use std::os::raw::{c_int, c_short, c_uchar, c_ushort};
use std::sync::atomic::{AtomicPtr, Ordering};
use std::sync::OnceLock;

/// Gpm_Event structure from gpm.h
///
/// This structure contains all information about a mouse event from GPM.
#[repr(C)]
pub struct GpmEventRaw {
    pub buttons: c_uchar,
    pub modifiers: c_uchar,
    pub vc: c_ushort,
    pub dx: c_short,
    pub dy: c_short,
    pub x: c_short,
    pub y: c_short,
    pub event_type: c_int, // enum Gpm_Etype
    pub clicks: c_int,
    pub margin: c_int, // enum Gpm_Margin
    pub wdx: c_short,
    pub wdy: c_short,
}

/// Gpm_Connect structure from gpm.h
///
/// Used to specify which events the client wants to receive.
#[repr(C)]
pub struct GpmConnect {
    pub event_mask: c_ushort,
    pub default_mask: c_ushort,
    pub min_mod: c_ushort,
    pub max_mod: c_ushort,
    pub pid: c_int,
    pub vc: c_int,
}

// Event type constants from gpm.h
pub const GPM_MOVE: c_ushort = 1;
pub const GPM_DRAG: c_ushort = 2;
pub const GPM_DOWN: c_ushort = 4;
pub const GPM_UP: c_ushort = 8;
pub const GPM_SINGLE: c_ushort = 16;
pub const GPM_DOUBLE: c_ushort = 32;
pub const GPM_TRIPLE: c_ushort = 64;

// Function pointer types
type GpmOpenFn = unsafe extern "C" fn(*mut GpmConnect, c_int) -> c_int;
type GpmCloseFn = unsafe extern "C" fn() -> c_int;
type GpmGetEventFn = unsafe extern "C" fn(*mut GpmEventRaw) -> c_int;

/// Dynamically loaded GPM library and its functions
pub struct GpmLib {
    _library: Library,
    gpm_open: GpmOpenFn,
    gpm_close: GpmCloseFn,
    gpm_get_event: GpmGetEventFn,
    /// Pointer to gpm_visiblepointer global variable (controls cursor visibility)
    gpm_visiblepointer: AtomicPtr<c_int>,
}

// SAFETY: GpmLib is safe to share between threads because:
// - The function pointers are immutable after initialization
// - The AtomicPtr is thread-safe
// - The Library is only dropped when GpmLib is dropped (which won't happen due to OnceLock)
unsafe impl Send for GpmLib {}
unsafe impl Sync for GpmLib {}

// Global singleton for the loaded library
static GPM_LIB: OnceLock<Option<GpmLib>> = OnceLock::new();

/// Common paths where libgpm.so might be found
const LIBGPM_PATHS: &[&str] = &[
    "libgpm.so.2",
    "libgpm.so",
    "/usr/lib/libgpm.so.2",
    "/usr/lib/libgpm.so",
    "/usr/lib64/libgpm.so.2",
    "/usr/lib64/libgpm.so",
    "/usr/lib/x86_64-linux-gnu/libgpm.so.2",
    "/usr/lib/x86_64-linux-gnu/libgpm.so",
    "/lib/libgpm.so.2",
    "/lib/libgpm.so",
    "/lib64/libgpm.so.2",
    "/lib64/libgpm.so",
];

impl GpmLib {
    /// Try to load libgpm from common system paths
    fn try_load() -> Option<Self> {
        tracing::debug!("GPM FFI: Attempting to load libgpm...");
        for path in LIBGPM_PATHS {
            tracing::trace!("GPM FFI: Trying path: {}", path);
            match Self::load_from_path(path) {
                Ok(lib) => {
                    tracing::debug!("GPM FFI: Loaded libgpm from: {}", path);
                    return Some(lib);
                }
                Err(e) => {
                    tracing::trace!("GPM FFI: Failed to load {}: {}", path, e);
                }
            }
        }
        tracing::debug!("GPM FFI: libgpm not found in any standard location");
        None
    }

    /// Load libgpm from a specific path
    fn load_from_path(path: &str) -> Result<Self, libloading::Error> {
        // SAFETY: We're loading a well-known system library with a stable ABI
        unsafe {
            let library = Library::new(path)?;

            let gpm_open: Symbol<GpmOpenFn> = library.get(b"Gpm_Open")?;
            let gpm_close: Symbol<GpmCloseFn> = library.get(b"Gpm_Close")?;
            let gpm_get_event: Symbol<GpmGetEventFn> = library.get(b"Gpm_GetEvent")?;

            // Try to get the gpm_visiblepointer global variable (optional)
            let gpm_visiblepointer: *mut c_int = library
                .get::<*mut c_int>(b"gpm_visiblepointer")
                .ok()
                .map(|s| *s)
                .unwrap_or(std::ptr::null_mut());

            Ok(Self {
                gpm_open: *gpm_open,
                gpm_close: *gpm_close,
                gpm_get_event: *gpm_get_event,
                gpm_visiblepointer: AtomicPtr::new(gpm_visiblepointer),
                _library: library,
            })
        }
    }

    /// Open a connection to GPM
    ///
    /// # Returns
    /// * `-1` on error
    /// * `-2` if running in xterm (use xterm mouse protocol instead)
    /// * `>= 0` file descriptor on success
    pub fn open(&self, conn: &mut GpmConnect) -> c_int {
        // SAFETY: We're calling a C function with the correct signature
        unsafe { (self.gpm_open)(conn, 0) }
    }

    /// Close the GPM connection
    pub fn close(&self) -> c_int {
        // SAFETY: We're calling a C function with the correct signature
        unsafe { (self.gpm_close)() }
    }

    /// Get a mouse event
    ///
    /// # Returns
    /// * `1` if an event was read
    /// * `0` if no event available
    /// * `-1` on error
    pub fn get_event(&self, event: &mut GpmEventRaw) -> c_int {
        // SAFETY: We're calling a C function with the correct signature
        unsafe { (self.gpm_get_event)(event) }
    }

    /// Set whether GPM should draw the mouse pointer
    ///
    /// When set to 1, GPM draws the cursor on the console.
    /// When set to 0, the application is responsible for cursor display.
    pub fn set_visible_pointer(&self, visible: bool) {
        let ptr = self.gpm_visiblepointer.load(Ordering::Relaxed);
        if !ptr.is_null() {
            // SAFETY: We're writing to a global variable in libgpm
            unsafe {
                *ptr = if visible { 1 } else { 0 };
            }
            tracing::debug!(
                "GPM: set gpm_visiblepointer = {}",
                if visible { 1 } else { 0 }
            );
        } else {
            tracing::debug!("GPM: gpm_visiblepointer symbol not found, cannot set visibility");
        }
    }
}

/// Get the loaded GPM library, or None if libgpm is not available
pub fn get_gpm_lib() -> Option<&'static GpmLib> {
    GPM_LIB.get_or_init(GpmLib::try_load).as_ref()
}
