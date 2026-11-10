//! Windows terminal I/O helpers for the Fresh editor.
//!
//! This crate encapsulates all Windows-specific console hacks:
//! - VT input mode (`ENABLE_VIRTUAL_TERMINAL_INPUT` + `ReadConsoleInputW`)
//! - Dedicated reader thread for draining the console buffer
//! - Client/server relay loop (trait-based, decoupled from IPC details)
//! - Terminal size queries via console screen buffer
//! - Detection of MSYS2/Cygwin ptys (mintty), which have no console
//! - The win32-input-mode key report format ([`win32_input`])
//!
//! On non-Windows platforms, only the platform-independent byte matching is
//! built: [`win32_input`], which the input parser uses on every platform, and
//! `corrupt_input`, kept there so its tests run everywhere.

// Plain byte matching used by `vt_input`; built everywhere so its tests run
// on every platform.
#[cfg_attr(not(windows), allow(dead_code))]
mod corrupt_input;
#[cfg(windows)]
mod msys_pty;
#[cfg(windows)]
mod relay;
#[cfg(windows)]
mod terminal_size;
#[cfg(windows)]
mod vt_input;
pub mod win32_input;

#[cfg(windows)]
pub use vt_input::{
    disable_mouse_tracking, disable_win32_input_mode, enable_mouse_tracking, enable_vt_input,
    enable_win32_input_mode, restore_console_mode, save_console_mode, set_mouse_mode, MouseMode,
    VtInputEvent, VtInputReader,
};

#[cfg(windows)]
pub use msys_pty::msys_pty_stream;

#[cfg(windows)]
pub use relay::{relay_loop, RelayConnection, RelayExitReason};

#[cfg(windows)]
pub use terminal_size::{get_terminal_size, TerminalSize};
