//! Per-platform process-level setup that has to run before any window is
//! created, plus per-platform window decoration helpers.  Currently only
//! Windows has work here; the macOS bundle already provides identity via
//! `Info.plist` and Linux has no equivalent concept.

use winit::window::Window;

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

/// Apply a high-quality, size-correct icon to the running window's small and
/// large slots.
///
/// On Windows this loads the multi-size HICON from the .exe's embedded
/// `RT_GROUP_ICON` resource (the same one Explorer / Properties / Alt-Tab
/// see) and sends `WM_SETICON` for both `ICON_SMALL` and `ICON_BIG`, so the
/// taskbar gets a crisp icon at every DPI without upscaling a single PNG.
///
/// On other platforms this is a no-op — the cross-platform `winit` icon set
/// at window construction time is enough.  Call after `create_window`.
pub fn set_window_icon(window: &Window) {
    #[cfg(target_os = "windows")]
    set_window_icon_windows(window);
    #[cfg(not(target_os = "windows"))]
    let _ = window;
}

#[cfg(target_os = "windows")]
fn set_window_icon_windows(window: &Window) {
    use winit::raw_window_handle::{HasWindowHandle, RawWindowHandle};
    use windows_sys::Win32::Foundation::HWND;
    use windows_sys::Win32::System::LibraryLoader::GetModuleHandleW;
    use windows_sys::Win32::UI::WindowsAndMessaging::{
        GetSystemMetrics, LoadImageW, SendMessageW, ICON_BIG, ICON_SMALL, IMAGE_ICON,
        LR_DEFAULTCOLOR, LR_SHARED, SM_CXICON, SM_CXSMICON, SM_CYICON, SM_CYSMICON, WM_SETICON,
    };

    // Resource ordinal "1" — winresource's `set_icon` writes the icon as
    // `1 ICON "path"` in the generated .rc.  Wrapping a small integer as a
    // pointer is the standard MAKEINTRESOURCE pattern.
    const ICON_RESOURCE_ID: usize = 1;

    let hwnd: HWND = match window.window_handle() {
        Ok(handle) => match handle.as_raw() {
            RawWindowHandle::Win32(h) => h.hwnd.get() as *mut _,
            other => {
                tracing::debug!(?other, "window handle is not Win32; skipping icon override");
                return;
            }
        },
        Err(e) => {
            tracing::debug!("no window handle: {e}");
            return;
        }
    };

    // SAFETY: GetModuleHandleW(NULL) returns the running .exe's HMODULE,
    // valid for the lifetime of the process.  LoadImageW with LR_SHARED
    // returns an OS-managed HICON — we must NOT call DestroyIcon on it.
    // SendMessageW(WM_SETICON) just stores the HICON pointer on the HWND.
    unsafe {
        let hinstance = GetModuleHandleW(std::ptr::null());
        if hinstance.is_null() {
            tracing::warn!("GetModuleHandleW returned null; cannot load embedded icon");
            return;
        }

        let load_at = |w: i32, h: i32| -> *mut std::ffi::c_void {
            LoadImageW(
                hinstance as *mut _,
                ICON_RESOURCE_ID as *const u16,
                IMAGE_ICON,
                w,
                h,
                LR_DEFAULTCOLOR | LR_SHARED,
            )
        };

        let big = load_at(GetSystemMetrics(SM_CXICON), GetSystemMetrics(SM_CYICON));
        let small = load_at(GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CYSMICON));

        if big.is_null() && small.is_null() {
            tracing::warn!(
                "LoadImageW returned NULL for both icon sizes; \
                 the embedded RT_GROUP_ICON resource may be missing"
            );
            return;
        }

        if !big.is_null() {
            SendMessageW(hwnd, WM_SETICON, ICON_BIG as _, big as _);
        }
        if !small.is_null() {
            SendMessageW(hwnd, WM_SETICON, ICON_SMALL as _, small as _);
        }
        tracing::debug!("applied embedded RT_GROUP_ICON to running window");
    }
}
