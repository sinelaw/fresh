///! LSP diagnostics display
///!
///! This module handles converting LSP diagnostics to visual overlays in the editor.
///! Diagnostics are displayed as colored underlines (red for errors, yellow for warnings, etc.)
use crate::model::buffer::Buffer;
use crate::state::EditorState;
use crate::view::overlay::{Overlay, OverlayFace, OverlayNamespace};
use lsp_types::{Diagnostic, DiagnosticSeverity};
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::ops::Range;
use std::sync::{LazyLock, Mutex};

/// Namespace for all LSP diagnostic overlays
pub fn lsp_diagnostic_namespace() -> OverlayNamespace {
    OverlayNamespace::from_string("lsp-diagnostic".to_string())
}

/// Cache for diagnostic hash to avoid redundant updates, keyed by file path.
/// This prevents diagnostics from one buffer from invalidating another buffer's cache.
static DIAGNOSTIC_CACHE: LazyLock<Mutex<HashMap<String, u64>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

/// Compute a hash for a slice of diagnostics
/// This hash is used to quickly detect if diagnostics have changed
fn compute_diagnostic_hash(diagnostics: &[Diagnostic]) -> u64 {
    let mut hasher = DefaultHasher::new();

    // Hash the count first
    diagnostics.len().hash(&mut hasher);

    // Hash each diagnostic's key properties
    for diag in diagnostics {
        // Hash the range (start/end line and character)
        diag.range.start.line.hash(&mut hasher);
        diag.range.start.character.hash(&mut hasher);
        diag.range.end.line.hash(&mut hasher);
        diag.range.end.character.hash(&mut hasher);

        // Hash severity - match on all variants to get a hashable value
        let severity_value: i32 = match diag.severity {
            Some(DiagnosticSeverity::ERROR) => 1,
            Some(DiagnosticSeverity::WARNING) => 2,
            Some(DiagnosticSeverity::INFORMATION) => 3,
            Some(DiagnosticSeverity::HINT) => 4,
            None => 0,
            _ => -1,
        };
        severity_value.hash(&mut hasher);

        // Hash the message (most important part)
        diag.message.hash(&mut hasher);

        // Hash the source if present
        if let Some(source) = &diag.source {
            source.hash(&mut hasher);
        }
    }

    hasher.finish()
}

/// Apply LSP diagnostics to editor state with hash-based caching
///
/// This is the recommended entry point that skips redundant work when diagnostics haven't changed.
/// On a typical keystroke, diagnostics don't change, so this returns immediately.
pub fn apply_diagnostics_to_state_cached(
    state: &mut EditorState,
    diagnostics: &[Diagnostic],
    theme: &crate::view::theme::Theme,
) {
    // Get cache key from buffer's file path
    let cache_key = match state.buffer.file_path() {
        Some(path) => path.to_string_lossy().to_string(),
        None => return apply_diagnostics_to_state(state, diagnostics, theme),
    };

    // Compute hash of incoming diagnostics
    let new_hash = compute_diagnostic_hash(diagnostics);

    // Check if this is the same as last time for this specific buffer
    if let Ok(cache) = DIAGNOSTIC_CACHE.lock() {
        if let Some(&cached_hash) = cache.get(&cache_key) {
            if cached_hash == new_hash {
                // Diagnostics haven't changed for this buffer, skip all work
                return;
            }
        }
    }

    // Diagnostics have changed, do the expensive update
    apply_diagnostics_to_state(state, diagnostics, theme);

    // Update cache for this buffer
    if let Ok(mut cache) = DIAGNOSTIC_CACHE.lock() {
        cache.insert(cache_key, new_hash);
    }
}

/// Convert an LSP diagnostic to an overlay (range, face, priority)
/// Returns None if the diagnostic cannot be converted (invalid range, etc.)
pub fn diagnostic_to_overlay(
    diagnostic: &Diagnostic,
    buffer: &Buffer,
    theme: &crate::view::theme::Theme,
) -> Option<(Range<usize>, OverlayFace, i32)> {
    // Convert LSP positions (line/character) to byte offsets
    // LSP uses 0-indexed lines and characters (UTF-16 code units)
    let start_line = diagnostic.range.start.line as usize;
    let start_char = diagnostic.range.start.character as usize;
    let end_line = diagnostic.range.end.line as usize;
    let end_char = diagnostic.range.end.character as usize;

    // Convert LSP positions (line/UTF-16 character) to byte positions
    // LSP uses UTF-16 code units for character offsets
    let start_byte = buffer.lsp_position_to_byte(start_line, start_char);
    let end_byte = buffer.lsp_position_to_byte(end_line, end_char);

    // Determine overlay face based on diagnostic severity using theme colors
    let (face, priority) = match diagnostic.severity {
        Some(DiagnosticSeverity::ERROR) => (
            OverlayFace::Background {
                color: theme.diagnostic_error_bg,
            },
            100, // Highest priority
        ),
        Some(DiagnosticSeverity::WARNING) => (
            OverlayFace::Background {
                color: theme.diagnostic_warning_bg,
            },
            50, // Medium priority
        ),
        Some(DiagnosticSeverity::INFORMATION) => (
            OverlayFace::Background {
                color: theme.diagnostic_info_bg,
            },
            30, // Lower priority
        ),
        Some(DiagnosticSeverity::HINT) | None => (
            OverlayFace::Background {
                color: theme.diagnostic_hint_bg,
            },
            10, // Lowest priority
        ),
        _ => return None, // Unknown severity
    };

    Some((start_byte..end_byte, face, priority))
}

/// Apply LSP diagnostics to editor state as overlays
///
/// This function:
/// 1. Clears all existing LSP diagnostic overlays (using namespace)
/// 2. Adds overlays for all current diagnostics
pub fn apply_diagnostics_to_state(
    state: &mut EditorState,
    diagnostics: &[Diagnostic],
    theme: &crate::view::theme::Theme,
) {
    let ns = lsp_diagnostic_namespace();

    // Clear all existing LSP diagnostic overlays using namespace
    state.overlays.clear_namespace(&ns, &mut state.marker_list);

    // Add overlays for all current diagnostics
    let mut added_count = 0;
    for diagnostic in diagnostics {
        if let Some((range, face, priority)) =
            diagnostic_to_overlay(diagnostic, &state.buffer, theme)
        {
            let message = diagnostic.message.clone();

            let overlay = Overlay::with_namespace(&mut state.marker_list, range, face, ns.clone())
                .with_priority_value(priority)
                .with_message(message);

            state.overlays.add(overlay);
            added_count += 1;
        }
    }

    if added_count > 0 {
        tracing::debug!("Applied {} diagnostic overlays", added_count);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::buffer::Buffer;
    use crate::view::theme;
    use lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};

    #[test]
    fn test_lsp_position_to_byte() {
        let buffer = Buffer::from_str_test("hello\nworld\ntest");

        // Line 0, character 0
        assert_eq!(buffer.lsp_position_to_byte(0, 0), 0);

        // Line 0, character 5 (end of "hello")
        assert_eq!(buffer.lsp_position_to_byte(0, 5), 5);

        // Line 1, character 0 (start of "world")
        assert_eq!(buffer.lsp_position_to_byte(1, 0), 6);

        // Line 1, character 5 (end of "world")
        assert_eq!(buffer.lsp_position_to_byte(1, 5), 11);

        // Line 2, character 0 (start of "test")
        assert_eq!(buffer.lsp_position_to_byte(2, 0), 12);

        // Out of bounds line - should clamp to end of buffer
        assert_eq!(buffer.lsp_position_to_byte(10, 0), buffer.len());
    }

    #[test]
    fn test_diagnostic_to_overlay_error() {
        let buffer = Buffer::from_str_test("hello world");

        let diagnostic = Diagnostic {
            range: Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: Position {
                    line: 0,
                    character: 5,
                },
            },
            severity: Some(DiagnosticSeverity::ERROR),
            code: None,
            code_description: None,
            source: None,
            message: "Test error".to_string(),
            related_information: None,
            tags: None,
            data: None,
        };

        let theme = crate::view::theme::Theme::from_name(theme::THEME_DARK).unwrap();
        let result = diagnostic_to_overlay(&diagnostic, &buffer, &theme);
        assert!(result.is_some());

        let (range, face, priority) = result.unwrap();
        assert_eq!(range, 0..5);
        assert_eq!(priority, 100); // Error has highest priority

        match face {
            OverlayFace::Background { color } => {
                assert_eq!(color, theme.diagnostic_error_bg);
            }
            _ => panic!("Expected Background face"),
        }
    }

    #[test]
    fn test_diagnostic_to_overlay_warning() {
        let buffer = Buffer::from_str_test("hello world");

        let diagnostic = Diagnostic {
            range: Range {
                start: Position {
                    line: 0,
                    character: 6,
                },
                end: Position {
                    line: 0,
                    character: 11,
                },
            },
            severity: Some(DiagnosticSeverity::WARNING),
            code: None,
            code_description: None,
            source: None,
            message: "Test warning".to_string(),
            related_information: None,
            tags: None,
            data: None,
        };

        let theme = crate::view::theme::Theme::from_name(theme::THEME_DARK).unwrap();
        let result = diagnostic_to_overlay(&diagnostic, &buffer, &theme);
        assert!(result.is_some());

        let (range, face, priority) = result.unwrap();
        assert_eq!(range, 6..11);
        assert_eq!(priority, 50); // Warning has medium priority

        match face {
            OverlayFace::Background { color } => {
                assert_eq!(color, theme.diagnostic_warning_bg);
            }
            _ => panic!("Expected Background face"),
        }
    }

    #[test]
    fn test_diagnostic_to_overlay_multiline() {
        let buffer = Buffer::from_str_test("line1\nline2\nline3");

        let diagnostic = Diagnostic {
            range: Range {
                start: Position {
                    line: 0,
                    character: 3,
                },
                end: Position {
                    line: 1,
                    character: 2,
                },
            },
            severity: Some(DiagnosticSeverity::ERROR),
            code: None,
            code_description: None,
            source: None,
            message: "Multi-line error".to_string(),
            related_information: None,
            tags: None,
            data: None,
        };

        let theme = crate::view::theme::Theme::from_name(theme::THEME_DARK).unwrap();
        let result = diagnostic_to_overlay(&diagnostic, &buffer, &theme);
        assert!(result.is_some());

        let (range, _, _) = result.unwrap();
        // "line1\n" is 6 bytes, "li" is 2 bytes
        // start: line 0, char 3 = byte 3 ("e1")
        // end: line 1, char 2 = byte 8 ("ne")
        assert_eq!(range.start, 3);
        assert_eq!(range.end, 8);
    }
}
