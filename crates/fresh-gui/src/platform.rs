//! Per-platform process-level setup that has to run before any window is
//! created.  Currently only Windows has work here; the macOS bundle already
//! provides identity via `Info.plist` and Linux has no equivalent concept.

/// AppUserModelID for the Fresh GUI process.
///
/// Must match the ID used by the MSI for its Start-Menu / Desktop shortcuts
/// (see PRODUCTIZATION_PLAN.md §3.1).  If they disagree, Windows treats the
/// running window and the pinned shortcut as different applications and
/// they appear as two separate entries on the taskbar.
#[cfg(target_os = "windows")]
pub const APP_USER_MODEL_ID: &str = "dev.getfresh.Fresh";

/// Run process-level platform setup.  Call once, before constructing the
/// winit `EventLoop`.  Failures are logged but not propagated — the app
/// can still run with a degraded taskbar identity.
pub fn init() {
    #[cfg(target_os = "windows")]
    init_windows();
}

#[cfg(target_os = "windows")]
fn init_windows() {
    use windows_sys::Win32::UI::Shell::SetCurrentProcessExplicitAppUserModelID;

    // Encode as UTF-16 with a trailing NUL, as the Win32 API expects.
    let wide: Vec<u16> = APP_USER_MODEL_ID
        .encode_utf16()
        .chain(std::iter::once(0))
        .collect();

    // SAFETY: `wide` is a valid NUL-terminated UTF-16 string for the
    // duration of the call; the API copies it internally.
    let hr = unsafe { SetCurrentProcessExplicitAppUserModelID(wide.as_ptr()) };
    if hr < 0 {
        tracing::warn!(
            "SetCurrentProcessExplicitAppUserModelID failed: HRESULT 0x{:08X}",
            hr as u32
        );
    } else {
        tracing::debug!("AppUserModelID set to {}", APP_USER_MODEL_ID);
    }
}
