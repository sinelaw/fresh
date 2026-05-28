//! Windows shell-detection helpers for the integrated terminal.
//!
//! Picks the executable handed to ConPTY when the user hasn't overridden
//! `terminal.shell` in their config. The main wrinkle this module exists to
//! handle is the Microsoft Store build of `pwsh.exe`, which lives on PATH
//! as a zero-byte `APPEXECLINK` reparse-point stub (under
//! `%LOCALAPPDATA%\Microsoft\WindowsApps`). Spawning that stub through
//! ConPTY has been reported to crash with `0xc0000142`
//! (STATUS_DLL_INIT_FAILED) on Windows 11 23H2; see issue #2077.
//!
//! The workaround is opt-out via `terminal.skip_app_execution_alias`. The
//! flag is mirrored into a process-wide atomic so [`detect_shell`] stays a
//! parameter-less function (its many callers don't all have a
//! `TerminalConfig` handy); [`set_skip_app_execution_alias`] is called
//! once at editor startup from `Config::apply_runtime_flags`.
//!
//! [`detect_shell`]: super::detect_shell

use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};

static SKIP_APP_EXECUTION_ALIAS: AtomicBool = AtomicBool::new(true);

/// Set the workaround flag at editor startup.
///
/// `true` (the default) makes [`select_windows_shell`] skip Microsoft
/// Store App Execution Alias stubs. Set to `false` to disable the
/// workaround — useful for users who want to debug or who only ever have
/// a real `pwsh.exe` on PATH.
pub fn set_skip_app_execution_alias(skip: bool) {
    SKIP_APP_EXECUTION_ALIAS.store(skip, Ordering::Relaxed);
}

fn skip_app_execution_alias() -> bool {
    SKIP_APP_EXECUTION_ALIAS.load(Ordering::Relaxed)
}

/// Pick a Windows shell, preferring PowerShell for its ConPTY/ANSI support.
///
/// Candidates are resolved to a concrete on-disk path. When the workaround
/// is enabled (see [`set_skip_app_execution_alias`]), Microsoft Store
/// App Execution Alias stubs are skipped — see [`is_app_execution_alias`].
/// Returning the fully-resolved path keeps spawn-time PATH resolution from
/// re-finding the alias.
pub fn select_windows_shell() -> String {
    let skip_alias = skip_app_execution_alias();

    // Prefer a real PowerShell 7 install over a Store alias, then bare
    // `pwsh.exe`/`powershell.exe` from PATH, then Windows PowerShell 5.
    let mut candidates: Vec<String> = Vec::new();
    for var in ["ProgramFiles", "ProgramW6432", "ProgramFiles(x86)"] {
        if let Ok(base) = std::env::var(var) {
            if !base.is_empty() {
                candidates.push(format!(r"{base}\PowerShell\7\pwsh.exe"));
            }
        }
    }
    candidates.push("pwsh.exe".to_string());
    candidates.push("powershell.exe".to_string());
    candidates.push(r"C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe".to_string());

    for candidate in &candidates {
        if let Some(resolved) = resolve_shell_candidate(candidate) {
            if skip_alias && is_app_execution_alias(&resolved) {
                // Zero-byte WindowsApps stub: crashes under ConPTY with
                // 0xc0000142 (STATUS_DLL_INIT_FAILED). (issue #2077)
                continue;
            }
            return resolved.to_string_lossy().into_owned();
        }
    }

    std::env::var("COMSPEC").unwrap_or_else(|_| "cmd.exe".to_string())
}

/// Resolve a shell candidate to a concrete file path.
///
/// Candidates containing a path separator are checked directly; bare names
/// are looked up on `PATH`. Returns `None` when no matching file exists.
pub(crate) fn resolve_shell_candidate(cmd: &str) -> Option<PathBuf> {
    if cmd.contains('\\') || cmd.contains('/') {
        let p = PathBuf::from(cmd);
        return p.is_file().then_some(p);
    }
    let path_var = std::env::var("PATH").ok()?;
    path_var
        .split(';')
        .filter(|d| !d.is_empty())
        .find_map(|dir| {
            let full = Path::new(dir).join(cmd);
            full.is_file().then_some(full)
        })
}

/// Detect a Microsoft Store **App Execution Alias** stub.
///
/// Aliases such as the Store build of `pwsh.exe` under
/// `%LOCALAPPDATA%\Microsoft\WindowsApps` are zero-byte `APPEXECLINK`
/// reparse points, not real executables. A genuine shell binary is always
/// non-empty. (issue #2077)
pub(crate) fn is_app_execution_alias(path: &Path) -> bool {
    std::fs::metadata(path)
        .map(|m| m.len() == 0)
        .unwrap_or(false)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;

    // A Microsoft Store App Execution Alias is a zero-byte stub; the
    // workaround must detect and skip it. (issue #2077)
    #[test]
    fn detects_zero_byte_stub() {
        let dir = tempfile::tempdir().unwrap();
        let alias = dir.path().join("pwsh.exe");
        std::fs::File::create(&alias).unwrap();
        assert!(is_app_execution_alias(&alias));

        let real = dir.path().join("powershell.exe");
        let mut f = std::fs::File::create(&real).unwrap();
        f.write_all(b"MZ\x90\x00").unwrap();
        drop(f);
        assert!(!is_app_execution_alias(&real));
    }

    #[test]
    fn resolve_candidate_honors_explicit_path_and_misses() {
        let dir = tempfile::tempdir().unwrap();
        let real = dir.path().join("powershell.exe");
        std::fs::write(&real, b"MZ").unwrap();

        let resolved = resolve_shell_candidate(&real.to_string_lossy()).unwrap();
        assert_eq!(resolved, real);
        assert!(
            resolve_shell_candidate(&dir.path().join("missing.exe").to_string_lossy()).is_none()
        );
    }

    #[test]
    fn workaround_flag_toggle_round_trips() {
        // Restore whatever the test harness left in place.
        let original = skip_app_execution_alias();
        set_skip_app_execution_alias(false);
        assert!(!skip_app_execution_alias());
        set_skip_app_execution_alias(true);
        assert!(skip_app_execution_alias());
        set_skip_app_execution_alias(original);
    }
}
