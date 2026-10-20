//! Path utilities for path expansion and normalization.

use std::borrow::Cow;
use std::path::{Path, PathBuf};

/// Expand tilde (~) in a path to the user's home directory.
///
/// # Examples
/// - `~/Documents` -> `/home/user/Documents`
/// - `~` -> `/home/user`
/// - `/absolute/path` -> `/absolute/path` (unchanged)
/// - `relative/path` -> `relative/path` (unchanged)
///
/// If the home directory cannot be determined, the path is returned unchanged.
pub fn expand_tilde(path: &str) -> PathBuf {
    if let Some(suffix) = path.strip_prefix('~') {
        if let Some(home) = dirs::home_dir() {
            home.join(suffix.trim_start_matches('/'))
        } else {
            PathBuf::from(path)
        }
    } else {
        PathBuf::from(path)
    }
}

/// Convert a Windows verbatim path (`\\?\C:\…` or `\\?\UNC\server\share\…`)
/// into its non-verbatim equivalent (`C:\…` or `\\server\share\…`).
///
/// Returns the input unchanged on non-Windows platforms or for paths that
/// have no verbatim prefix.
///
/// The prefix is also kept when the plain spelling would name another file or
/// would not open at all, because Win32 rewrites a non-verbatim path while
/// parsing it: a path at or past `MAX_PATH`, or one with a component ending in
/// a dot or space, a DOS device name (`NUL.txt`), `.`/`..`, or a character a
/// plain path cannot hold. See [`component_keeps_meaning_unprefixed`].
pub fn strip_verbatim_prefix(path: &Path) -> Cow<'_, Path> {
    #[cfg(windows)]
    {
        use std::os::windows::ffi::OsStrExt;
        use std::path::{Component, Prefix};

        /// Longest path most programs can open without the verbatim prefix,
        /// in UTF-16 units including the terminating NUL.
        const MAX_PATH: usize = 260;

        let mut components = path.components();
        let prefix = match components.next() {
            Some(Component::Prefix(p)) => p,
            _ => return Cow::Borrowed(path),
        };

        let mut rebuilt = PathBuf::new();
        match prefix.kind() {
            Prefix::VerbatimDisk(drive) => {
                rebuilt.push(format!("{}:\\", drive as char));
            }
            Prefix::VerbatimUNC(server, share) => {
                rebuilt.push(format!(
                    r"\\{}\{}\",
                    server.to_string_lossy(),
                    share.to_string_lossy()
                ));
            }
            _ => return Cow::Borrowed(path),
        }
        // Skip the original RootDir (which the rebuilt prefix already includes)
        // and append the rest of the components.
        for component in components {
            match component {
                Component::RootDir => continue,
                Component::Normal(name)
                    if component_keeps_meaning_unprefixed(&name.to_string_lossy()) => {}
                // `.` and `..` stay literal only in a verbatim path.
                _ => return Cow::Borrowed(path),
            }
            rebuilt.push(component.as_os_str());
        }
        if rebuilt.as_os_str().encode_wide().count() >= MAX_PATH {
            return Cow::Borrowed(path);
        }
        Cow::Owned(rebuilt)
    }
    #[cfg(not(windows))]
    {
        Cow::Borrowed(path)
    }
}

/// Whether Win32 reads one path component the same without the verbatim
/// prefix as with it.
///
/// A verbatim path skips Win32's normalization, so a component that the
/// normalization would change names another file once the prefix is gone.
/// Kept platform-independent so the rule is tested on every platform.
#[cfg_attr(not(windows), allow(dead_code))]
fn component_keeps_meaning_unprefixed(component: &str) -> bool {
    // Trailing dots and spaces are dropped: `notes.` opens `notes`.
    if component.ends_with('.') || component.ends_with(' ') {
        return false;
    }
    // Characters a plain path cannot hold.
    if component.chars().any(|c| {
        matches!(c, '<' | '>' | ':' | '"' | '|' | '?' | '*' | '/' | '\\') || (c as u32) < 0x20
    }) {
        return false;
    }
    // Device names are reserved with or without an extension (`NUL.txt`).
    let stem = component.split('.').next().unwrap_or(component);
    let stem = stem.to_ascii_uppercase();
    if matches!(
        stem.as_str(),
        "CON" | "PRN" | "AUX" | "NUL" | "CONIN$" | "CONOUT$"
    ) {
        return false;
    }
    if let Some(unit) = stem
        .strip_prefix("COM")
        .or_else(|| stem.strip_prefix("LPT"))
    {
        // The ISO-8859-1 superscript digits count as device numbers too.
        if matches!(unit, "¹" | "²" | "³")
            || (!unit.is_empty() && unit.chars().all(|c| c.is_ascii_digit()))
        {
            return false;
        }
    }
    true
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_expand_tilde_with_path() {
        let result = expand_tilde("~/Documents/test.txt");
        if let Some(home) = dirs::home_dir() {
            assert_eq!(result, home.join("Documents/test.txt"));
        }
    }

    #[test]
    fn test_expand_tilde_just_tilde() {
        let result = expand_tilde("~");
        if let Some(home) = dirs::home_dir() {
            assert_eq!(result, home);
        }
    }

    #[test]
    fn test_expand_tilde_absolute_path() {
        let result = expand_tilde("/absolute/path");
        assert_eq!(result, PathBuf::from("/absolute/path"));
    }

    #[test]
    fn test_expand_tilde_relative_path() {
        let result = expand_tilde("relative/path");
        assert_eq!(result, PathBuf::from("relative/path"));
    }

    #[test]
    fn test_expand_tilde_with_slash() {
        let result = expand_tilde("~/");
        if let Some(home) = dirs::home_dir() {
            assert_eq!(result, home);
        }
    }

    #[cfg(not(windows))]
    #[test]
    fn strip_verbatim_prefix_is_noop_on_unix() {
        let p = Path::new("/home/user/project");
        assert_eq!(strip_verbatim_prefix(p).as_ref(), p);
    }

    #[cfg(windows)]
    #[test]
    fn strip_verbatim_prefix_removes_verbatim_disk() {
        let verbatim = PathBuf::from(r"\\?\C:\Users\HP\OneDrive\Desktop\PY'PGMS");
        let stripped = strip_verbatim_prefix(&verbatim);
        assert_eq!(
            stripped.as_ref(),
            Path::new(r"C:\Users\HP\OneDrive\Desktop\PY'PGMS"),
            "verbatim disk prefix should be replaced with plain drive form"
        );
    }

    #[cfg(windows)]
    #[test]
    fn strip_verbatim_prefix_removes_verbatim_unc() {
        let verbatim = PathBuf::from(r"\\?\UNC\server\share\dir\file");
        let stripped = strip_verbatim_prefix(&verbatim);
        assert_eq!(
            stripped.as_ref(),
            Path::new(r"\\server\share\dir\file"),
            "verbatim UNC prefix should be replaced with plain UNC form"
        );
    }

    #[cfg(windows)]
    #[test]
    fn strip_verbatim_prefix_passes_plain_paths_through() {
        let plain = PathBuf::from(r"C:\Users\HP\project");
        let result = strip_verbatim_prefix(&plain);
        assert_eq!(result.as_ref(), Path::new(r"C:\Users\HP\project"));
    }

    #[test]
    fn components_win32_would_rewrite_need_the_prefix() {
        for component in [
            "notes.",  // Win32 drops the trailing dot
            "notes ",  // ...and the trailing space
            "NUL.txt", // DOS device name, extension or not
            "con",
            "com1.log",
            "COM¹.txt",
            "com².log",
            "LPT³",
            "a?b.txt", // plain paths cannot hold these
            "a*b.txt",
            "a\u{1}b",
        ] {
            assert!(
                !component_keeps_meaning_unprefixed(component),
                "{component:?}"
            );
        }
    }

    #[test]
    fn ordinary_components_do_not_need_the_prefix() {
        // A dot inside a name, and a name that merely starts like a device
        // name, are ordinary to Win32.
        for component in [
            "notes.txt",
            "a.b",
            ".gitignore",
            "CONSOLE",
            "nulify",
            "COM",
            "LPTx",
        ] {
            assert!(
                component_keeps_meaning_unprefixed(component),
                "{component:?}"
            );
        }
    }

    #[cfg(windows)]
    #[test]
    fn strip_verbatim_prefix_keeps_paths_the_plain_form_would_change() {
        let long = format!(r"\\?\C:\{}", "a".repeat(300));
        for path in [
            r"\\?\C:\dir\notes.",
            r"\\?\C:\dir.\f.txt",
            r"\\?\C:\dir\NUL.txt",
            r"\\?\C:\dir\..\f.txt",
            r"\\?\C:\dir\.\f.txt",
            r"\\?\UNC\server\share\notes ",
            // No drive or UNC spelling.
            r"\\?\Volume{01234567-89ab-cdef-0123-456789abcdef}\f.txt",
            // Only usable in its verbatim form.
            long.as_str(),
        ] {
            assert_eq!(
                strip_verbatim_prefix(Path::new(path)).as_ref(),
                Path::new(path),
                "{path}"
            );
        }
    }
}
