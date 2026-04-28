//! Suppress the transient console window that Windows opens for
//! console-subsystem children spawned from a GUI parent (git, ssh,
//! docker, formatters, plugin processes, …). Setting `CREATE_NO_WINDOW`
//! at spawn time prevents the brief black flash.
//!
//! No-op on non-Windows targets.

#[cfg(windows)]
const CREATE_NO_WINDOW: u32 = 0x0800_0000;

pub trait HideWindow {
    /// Mark the command so its child does not get a visible console
    /// window on Windows. No-op elsewhere.
    fn hide_window(&mut self) -> &mut Self;
}

impl HideWindow for std::process::Command {
    #[cfg(windows)]
    fn hide_window(&mut self) -> &mut Self {
        use std::os::windows::process::CommandExt;
        self.creation_flags(CREATE_NO_WINDOW)
    }
    #[cfg(not(windows))]
    fn hide_window(&mut self) -> &mut Self {
        self
    }
}

impl HideWindow for tokio::process::Command {
    #[cfg(windows)]
    fn hide_window(&mut self) -> &mut Self {
        self.creation_flags(CREATE_NO_WINDOW)
    }
    #[cfg(not(windows))]
    fn hide_window(&mut self) -> &mut Self {
        self
    }
}
