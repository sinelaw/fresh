//! Windows terminal I/O helpers for the Fresh editor.
//!
//! This crate encapsulates all Windows-specific console hacks:
//! - VT input mode (`ENABLE_VIRTUAL_TERMINAL_INPUT` + `ReadConsoleInputW`)
//! - Dedicated reader thread for draining the console buffer
//! - Client/server relay loop (trait-based, decoupled from IPC details)
//! - Terminal size queries via console screen buffer
//! - Detection of MSYS2/Cygwin ptys (mintty), which have no console
//!
//! On non-Windows platforms, this crate compiles as empty.

#[cfg(windows)]
mod msys_pty;
#[cfg(windows)]
mod relay;
#[cfg(windows)]
mod terminal_size;
#[cfg(windows)]
mod vt_input;

#[cfg(windows)]
pub use vt_input::{
    disable_mouse_tracking, enable_mouse_tracking, enable_vt_input, restore_console_mode,
    save_console_mode, set_mouse_mode, MouseMode, VtInputEvent, VtInputReader,
};

#[cfg(windows)]
pub use msys_pty::msys_pty_stream;

#[cfg(windows)]
pub use relay::{relay_loop, RelayConnection, RelayExitReason};

#[cfg(windows)]
pub use terminal_size::{get_terminal_size, TerminalSize};
