//! Encoding a workspace path into a filesystem-safe filename.
//!
//! Used by `config_io` to name per-workspace config files and by
//! `fresh-editor`'s workspace store to name session files, so it lives here
//! rather than in either caller.

use std::path::Path;

/// Encode a path into a filesystem-safe filename using percent encoding
///
/// Keeps alphanumeric chars, `-`, `.`, `_` as-is.
/// Replaces `/` with `_` for readability.
/// Percent-encodes other special characters as %XX.
///
/// Example: `/home/user/my project` -> `home_user_my%20project`
pub fn encode_path_for_filename(path: &Path) -> String {
    let path_str = path.to_string_lossy();
    let mut result = String::with_capacity(path_str.len() * 2);

    for c in path_str.chars() {
        match c {
            // Path separators become underscores for readability
            '/' | '\\' => result.push('_'),
            // Safe chars pass through
            c if c.is_ascii_alphanumeric() => result.push(c),
            '-' | '.' => result.push(c),
            // Underscore needs special handling to avoid collision with /
            '_' => result.push_str("%5F"),
            // Everything else gets percent-encoded
            c => {
                for byte in c.to_string().as_bytes() {
                    result.push_str(&format!("%{:02X}", byte));
                }
            }
        }
    }

    // Remove leading underscores (from leading /)
    let result = result.trim_start_matches('_').to_string();

    // Collapse multiple underscores
    let mut final_result = String::with_capacity(result.len());
    let mut last_was_underscore = false;
    for c in result.chars() {
        if c == '_' {
            if !last_was_underscore {
                final_result.push(c);
            }
            last_was_underscore = true;
        } else {
            final_result.push(c);
            last_was_underscore = false;
        }
    }

    if final_result.is_empty() {
        final_result = "root".to_string();
    }

    final_result
}
