//! Detection of MSYS2 / Cygwin pseudo-terminals (mintty, Git Bash).
//!
//! A native Windows program started from mintty gets named pipes such as
//! `\\.\pipe\msys-<hash>-pty0-from-master` for its standard streams, not a
//! console. Rust's `IsTerminal` deliberately reports those pipes as terminals
//! (it falls back to matching the pipe name when `GetConsoleMode` fails), so
//! code that trusts it goes on to call console APIs on a pipe and fails with
//! `ERROR_INVALID_HANDLE` (os error 6). Keystrokes typed into mintty only ever
//! reach the pipe, never a console, so there is nothing to fall back to.

use std::io::{self, IsTerminal};
use std::os::windows::io::{AsHandle, AsRawHandle};

use windows_sys::Win32::System::Console::GetConsoleMode;

/// Which standard stream, if any, is an MSYS2/Cygwin pty instead of a console.
///
/// Checks stdin, then stdout. A stream that `IsTerminal` accepts but that has
/// no console mode can only be such a pty: `IsTerminal` returns `true` without
/// a console only for pipes named `msys-*-pty*` / `cygwin-*-pty*`.
pub fn msys_pty_stream() -> Option<&'static str> {
    if is_pty_without_console(&io::stdin()) {
        Some("stdin")
    } else if is_pty_without_console(&io::stdout()) {
        Some("stdout")
    } else {
        None
    }
}

fn is_pty_without_console(stream: &(impl IsTerminal + AsHandle)) -> bool {
    if !stream.is_terminal() {
        return false;
    }
    let mut mode = 0u32;
    // SAFETY: GetConsoleMode only writes to `mode`; an invalid or non-console
    // handle makes it return 0, which is the case being detected.
    unsafe { GetConsoleMode(stream.as_handle().as_raw_handle(), &mut mode) == 0 }
}
