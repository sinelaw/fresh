///! LSP diagnostics display
///!
///! This module handles converting LSP diagnostics to visual overlays in the editor.
///! Diagnostics are displayed as colored underlines (red for errors, yellow for warnings, etc.)
use crate::text_buffer::Buffer;
use crate::overlay::OverlayFace;
use crate::state::EditorState;
use lsp_types::{Diagnostic, DiagnosticSeverity};
use ratatui::style::Color;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::ops::Range;
use std::sync::Mutex;

/// Cache for diagnostic hash to avoid redundant updates
/// Using a global static with Mutex for simplicity - could be moved to EditorState later
static DIAGNOSTIC_CACHE: Mutex<Option<u64>> = Mutex::new(None);

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
    theme: &crate::theme::Theme,
) {
    // Compute hash of incoming diagnostics
    let new_hash = compute_diagnostic_hash(diagnostics);

    // Check if this is the same as last time
    if let Ok(cache) = DIAGNOSTIC_CACHE.lock() {
        if let Some(cached_hash) = *cache {
            if cached_hash == new_hash {
                // Diagnostics haven't changed, skip all work
                return;
            }
        }
    }

    // Diagnostics have changed, do the expensive update
    apply_diagnostics_to_state(state, diagnostics, theme);

    // Update cache
    if let Ok(mut cache) = DIAGNOSTIC_CACHE.lock() {
        *cache = Some(new_hash);
    }
}

/// Convert an LSP diagnostic to an overlay (range, face, priority)
/// Returns None if the diagnostic cannot be converted (invalid range, etc.)
pub fn diagnostic_to_overlay(
    diagnostic: &Diagnostic,
    buffer: &Buffer,
    theme: &crate::theme::Theme,
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

/// Create a stable ID for a diagnostic based on its content
/// This allows us to track which diagnostics have changed
fn diagnostic_id(diagnostic: &Diagnostic) -> String {
    // Create an ID based on position and message
    // Format: "lsp-diagnostic-L{line}C{col}-{hash}"
    let line = diagnostic.range.start.line;
    let col = diagnostic.range.start.character;

    // Simple hash of the message (first 8 chars is enough for uniqueness in most cases)
    let msg_hash: String = diagnostic.message.chars().take(8).collect();

    format!("lsp-diagnostic-L{}C{}-{}", line, col, msg_hash)
}

/// Apply LSP diagnostics to editor state as overlays
///
/// This function:
/// 1. Compares incoming diagnostics with existing overlays
/// 2. Removes overlays for diagnostics that no longer exist
/// 3. Adds overlays for new diagnostics
/// 4. Keeps overlays for unchanged diagnostics (incremental update)
/// 5. Updates margin indicators
pub fn apply_diagnostics_to_state(
    state: &mut EditorState,
    diagnostics: &[Diagnostic],
    theme: &crate::theme::Theme,
) {
    use crate::overlay::Overlay;
    use std::collections::HashSet;

    // Build set of incoming diagnostic IDs
    let incoming_ids: HashSet<String> = diagnostics.iter().map(|d| diagnostic_id(d)).collect();

    // Find existing diagnostic overlay IDs
    let existing_ids: Vec<String> = state
        .overlays
        .all()
        .iter()
        .filter_map(|o| {
            o.id.as_ref().and_then(|id| {
                if id.starts_with("lsp-diagnostic-") {
                    Some(id.clone())
                } else {
                    None
                }
            })
        })
        .collect();

    // Remove overlays for diagnostics that are no longer present
    let mut removed_count = 0;
    for id in &existing_ids {
        if !incoming_ids.contains(id) {
            state.overlays.remove_by_id(id, &mut state.marker_list);
            removed_count += 1;
        }
    }

    // Convert existing IDs to a set for fast lookup
    let existing_id_set: HashSet<String> = existing_ids.into_iter().collect();

    // Add new diagnostics (only those that don't already exist)
    let mut added_count = 0;

    // Track unique lines with diagnostics to avoid duplicate margin markers
    let mut diagnostic_lines = std::collections::HashSet::new();

    // Add new diagnostic overlays (skip if already exists)
    // The line anchor system creates anchors on-demand, so no pre-population needed
    for diagnostic in diagnostics {
        let overlay_id = diagnostic_id(diagnostic);

        // Skip if this diagnostic already has an overlay
        if existing_id_set.contains(&overlay_id) {
            // Still track the line for margin indicators
            let line = diagnostic.range.start.line as usize;
            diagnostic_lines.insert(line);
            continue;
        }

        // This is a new diagnostic, create an overlay for it
        if let Some((range, face, priority)) =
            diagnostic_to_overlay(diagnostic, &state.buffer, theme)
        {
            let message = diagnostic.message.clone();

            let overlay = Overlay::with_id(&mut state.marker_list, range, face, overlay_id)
                .with_priority_value(priority)
                .with_message(message);

            state.overlays.add(overlay);
            added_count += 1;

            // Track the line number for diagnostic indicator
            let line = diagnostic.range.start.line as usize;
            diagnostic_lines.insert(line);
        }
    }

    // Clear and rebuild diagnostic indicators (this is fast)
    state.margins.clear_diagnostic_indicators();
    for line in diagnostic_lines {
        state
            .margins
            .set_diagnostic_indicator(line, "●".to_string(), Color::Red);
    }

    // Log incremental update stats
    if added_count > 0 || removed_count > 0 {
        tracing::debug!(
            "Incremental diagnostic update: added={}, removed={}, kept={}",
            added_count,
            removed_count,
            diagnostics.len() - added_count
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::text_buffer::Buffer;
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

        let theme = crate::theme::Theme::dark();
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

        let theme = crate::theme::Theme::dark();
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

        let theme = crate::theme::Theme::dark();
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
