//! Console front door for the Windows GUI build of Fresh (#2965).
//!
//! `fresh-gui.exe` is built with the "windows" subsystem so a launch from
//! Explorer or the Start menu does not flash a console. When it is started
//! from a terminal it attaches to the parent console and runs the TUI — but
//! interactive cmd.exe and PowerShell never wait for a "windows" subsystem
//! program, so the shell goes back to reading the same console input and the
//! two fight over keystrokes and console modes.
//!
//! This binary is shipped next to it as `fresh-gui.com`. Both shells try
//! `.COM` before `.EXE` (PATHEXT order), so typing `fresh-gui` lands here: a
//! "console" subsystem program the shell does wait for. It starts the sibling
//! `.exe` with the console's handles inherited, waits for it, and exits with
//! its exit code. The `.exe` attaches to this process's console exactly as it
//! would to the shell's, so nothing in Fresh itself has to change.

use std::ffi::OsStr;

/// True when the arguments ask Fresh for its window. Such a launch is handed
/// off without waiting, so the prompt comes back as it does for any GUI app.
/// Only arguments before a bare `--` or `--cmd` count: after `--` it is a
/// file name, and `--cmd` takes every remaining argument as its own value.
#[cfg_attr(not(windows), allow(dead_code))]
fn launches_gui<I, S>(args: I) -> bool
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    args.into_iter()
        .map(|a| a.as_ref().to_owned())
        .take_while(|a| a != "--" && a != "--cmd")
        .any(|a| a == "--gui")
}

#[cfg(windows)]
mod shim {
    use std::path::{Path, PathBuf};
    use std::process::Command;
    use windows_sys::core::BOOL;
    use windows_sys::Win32::System::Console::{
        SetConsoleCtrlHandler, CTRL_BREAK_EVENT, CTRL_C_EVENT,
    };

    /// Swallows Ctrl+C and Ctrl+Break; close, logoff and shutdown fall
    /// through to the default handler. A real handler, not the null one: the
    /// "ignore Ctrl+C" flag a null handler sets is inherited by the editor
    /// and everything it starts, and it does not cover Ctrl+Break.
    unsafe extern "system" fn ignore_interrupts(ctrl_type: u32) -> BOOL {
        BOOL::from(ctrl_type == CTRL_C_EVENT || ctrl_type == CTRL_BREAK_EVENT)
    }

    pub fn run() -> i32 {
        let target = match gui_exe() {
            Ok(path) => path,
            Err(msg) => {
                eprintln!("fresh: {msg}");
                return 1;
            }
        };
        let args: Vec<_> = std::env::args_os().skip(1).collect();
        let mut command = Command::new(&target);
        command.args(&args);

        if super::launches_gui(&args) {
            return match command.spawn() {
                Ok(_) => 0,
                Err(e) => {
                    eprintln!("fresh: cannot start {}: {e}", target.display());
                    1
                }
            };
        }

        // Ctrl+C and Ctrl+Break go to every process on the console. Fresh
        // handles them itself (in raw mode they arrive as keys); this process
        // has to outlive them so the shell keeps waiting and gets the real
        // exit code.
        // SAFETY: registers a handler that only compares its argument.
        unsafe { SetConsoleCtrlHandler(Some(ignore_interrupts), 1) };

        match command.status() {
            // `code()` is always `Some` on Windows; an NTSTATUS such as
            // 0xC0000005 comes back negative and round-trips through `exit`.
            Ok(status) => status.code().unwrap_or(1),
            Err(e) => {
                eprintln!("fresh: cannot start {}: {e}", target.display());
                1
            }
        }
    }

    /// The `.exe` next to this `.com`. A package manager may put a link to
    /// the `.com` on PATH, so the link's own directory is tried first and
    /// then the directory of the file it resolves to.
    fn gui_exe() -> Result<PathBuf, String> {
        let me =
            std::env::current_exe().map_err(|e| format!("cannot locate this executable: {e}"))?;
        let resolved = std::fs::canonicalize(&me).unwrap_or_else(|_| me.clone());
        for base in [&me, &resolved] {
            let exe = base.with_extension("exe");
            if exe.is_file() {
                if exe == me || is_same_file(&exe, &resolved) {
                    // Renamed to `.exe`: launching "the sibling" would start
                    // this shim again, forever.
                    return Err(format!(
                        "{} is the console launcher for fresh-gui.exe and must be named with a .com extension",
                        me.display()
                    ));
                }
                return Ok(exe);
            }
        }
        Err(format!(
            "cannot find {} next to {}",
            me.with_extension("exe")
                .file_name()
                .map(|n| n.to_string_lossy().into_owned())
                .unwrap_or_default(),
            me.display()
        ))
    }

    fn is_same_file(a: &Path, b: &Path) -> bool {
        match (std::fs::canonicalize(a), std::fs::canonicalize(b)) {
            (Ok(a), Ok(b)) => a == b,
            _ => false,
        }
    }
}

#[cfg(windows)]
fn main() {
    std::process::exit(shim::run());
}

#[cfg(not(windows))]
fn main() {
    eprintln!("fresh-console-shim is the Windows console launcher for fresh-gui.exe; it does nothing on this platform");
    std::process::exit(1);
}

#[cfg(test)]
mod tests {
    use super::launches_gui;

    #[test]
    fn gui_flag_hands_off() {
        assert!(launches_gui(["--gui"]));
        assert!(launches_gui(["file.rs", "--gui"]));
    }

    #[test]
    fn tui_launches_wait() {
        assert!(!launches_gui::<[&str; 0], &str>([]));
        assert!(!launches_gui(["file.rs"]));
        assert!(!launches_gui(["--guide"]));
        assert!(!launches_gui(["--", "--gui"]));
        assert!(!launches_gui(["--cmd", "agent", "--gui"]));
    }
}
