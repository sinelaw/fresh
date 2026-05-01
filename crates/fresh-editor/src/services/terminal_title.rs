//! Set the host terminal's window/tab title.
//!
//! Emits an OSC 2 ("Set Window Title") escape sequence on stdout. This is a
//! terminal-global property that lives outside the alternate screen buffer,
//! so writing it during rendering is fine and takes effect immediately in
//! terminals that support it. Terminals that don't understand OSC 2
//! silently ignore the sequence.

use std::io::{stdout, IsTerminal, Write};

/// Maximum number of bytes to send as a terminal title. Titles longer than
/// this are truncated. Most terminals cap internally; this guards against
/// pathological buffer names bloating the output.
const MAX_TITLE_BYTES: usize = 256;

/// Return a title string safe to embed in an OSC 2 sequence.
///
/// Strips ASCII control characters (including ESC and BEL, which would
/// terminate or break out of the sequence) and truncates to
/// [`MAX_TITLE_BYTES`] on a UTF-8 character boundary.
pub fn sanitize_title(title: &str) -> String {
    let filtered: String = title.chars().filter(|c| !c.is_control()).collect();

    if filtered.len() <= MAX_TITLE_BYTES {
        return filtered;
    }

    let mut end = MAX_TITLE_BYTES;
    while end > 0 && !filtered.is_char_boundary(end) {
        end -= 1;
    }
    filtered[..end].to_string()
}

/// Build a terminal window title from the active buffer's display name and
/// the optional project name (typically the working directory's last
/// component). Including the project name disambiguates files when several
/// Fresh sessions are open across different projects.
///
/// Format:
/// - With project: `<display_name> — <project_name> — Fresh`
/// - Without project: `<display_name> — Fresh`
///
/// An empty `project_name` is treated the same as `None`, so callers can pass
/// the result of `Path::file_name().and_then(OsStr::to_str)` directly.
pub fn build_window_title(display_name: &str, project_name: Option<&str>) -> String {
    match project_name {
        Some(p) if !p.is_empty() => {
            format!("{} \u{2014} {} \u{2014} Fresh", display_name, p)
        }
        _ => format!("{} \u{2014} Fresh", display_name),
    }
}

/// Write an OSC 2 escape sequence to stdout setting the terminal title.
///
/// The title is sanitized before writing. No-op when stdout is not a
/// terminal (e.g. under cargo test, when piped, when redirected to a
/// file) — OSC 2 only has meaning for an interactive terminal, and
/// writing it elsewhere would just leak escape bytes into captured
/// output.
///
/// Best-effort: if stdout is unavailable the error is discarded (the
/// title is a purely decorative feature and should never disrupt editing).
pub fn write_terminal_title(title: &str) {
    if !stdout().is_terminal() {
        return;
    }
    let safe = sanitize_title(title);
    // OSC 2 ; <title> BEL
    #[allow(clippy::let_underscore_must_use)]
    let _ = write!(stdout(), "\x1b]2;{}\x07", safe);
    #[allow(clippy::let_underscore_must_use)]
    let _ = stdout().flush();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn strips_control_characters() {
        assert_eq!(sanitize_title("hello\x07world"), "helloworld");
        assert_eq!(sanitize_title("hi\x1b[0m"), "hi[0m");
        assert_eq!(sanitize_title("line1\nline2"), "line1line2");
    }

    #[test]
    fn preserves_ordinary_text() {
        assert_eq!(sanitize_title("foo/bar.rs — Fresh"), "foo/bar.rs — Fresh");
    }

    #[test]
    fn truncates_long_titles_on_char_boundary() {
        let long = "á".repeat(MAX_TITLE_BYTES); // 2 bytes per char
        let out = sanitize_title(&long);
        assert!(out.len() <= MAX_TITLE_BYTES);
        // Must remain valid UTF-8 — String construction asserts this; also
        // make sure we didn't slice through a 2-byte sequence.
        assert!(out.chars().all(|c| c == 'á'));
    }

    #[test]
    fn empty_input_yields_empty_output() {
        assert_eq!(sanitize_title(""), "");
    }

    #[test]
    fn title_includes_project_when_provided() {
        assert_eq!(
            build_window_title("foo/bar.rs", Some("my-project")),
            "foo/bar.rs \u{2014} my-project \u{2014} Fresh"
        );
    }

    #[test]
    fn title_omits_project_when_none() {
        assert_eq!(
            build_window_title("foo/bar.rs", None),
            "foo/bar.rs \u{2014} Fresh"
        );
    }

    #[test]
    fn title_omits_project_when_empty_string() {
        assert_eq!(
            build_window_title("foo/bar.rs", Some("")),
            "foo/bar.rs \u{2014} Fresh"
        );
    }

    #[test]
    fn title_handles_virtual_buffer_names() {
        assert_eq!(
            build_window_title("[No Name]", Some("my-project")),
            "[No Name] \u{2014} my-project \u{2014} Fresh"
        );
    }
}
