///! LSP diagnostics display
///!
///! This module handles converting LSP diagnostics to visual overlays in the editor.
///! Diagnostics are displayed as colored underlines (red for errors, yellow for warnings, etc.)
use crate::buffer::Buffer;
use crate::overlay::OverlayFace;
use crate::state::EditorState;
use lsp_types::{Diagnostic, DiagnosticSeverity};
use ratatui::style::Color;
use std::ops::Range;

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

/// Apply LSP diagnostics to editor state as overlays
///
/// This function:
/// 1. Clears existing diagnostic overlays (IDs starting with "lsp-diagnostic-")
/// 2. Converts diagnostics to overlays
/// 3. Adds overlays to the editor state
/// 4. Adds red bullet point indicators in the margin for lines with diagnostics
pub fn apply_diagnostics_to_state(
    state: &mut EditorState,
    diagnostics: &[Diagnostic],
    theme: &crate::theme::Theme,
) {
    use crate::overlay::Overlay;

    // Clear existing diagnostic overlays
    // We'll use a special prefix for diagnostic overlay IDs
    let overlay_ids: Vec<String> = state
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

    for id in overlay_ids {
        state.overlays.remove_by_id(&id, &mut state.marker_list);
    }

    // Clear existing diagnostic indicators
    state.margins.clear_diagnostic_indicators();

    // Track unique lines with diagnostics to avoid duplicate margin markers
    let mut diagnostic_lines = std::collections::HashSet::new();

    // Sort diagnostics by start line to process them in order
    // This allows us to build up the line cache incrementally, avoiding O(N²) behavior
    let mut diagnostics_with_idx: Vec<_> = diagnostics.iter().enumerate().collect();
    diagnostics_with_idx.sort_by_key(|(_, diag)| diag.range.start.line);

    // Pre-populate the line cache for the maximum line we'll need
    // This transforms the O(N²) iteration problem into O(N)
    if let Some((_, last_diag)) = diagnostics_with_idx.last() {
        let max_line = last_diag.range.end.line as usize;
        // Populate cache from start to max_line + 1
        state.buffer.populate_line_cache(0, max_line + 1);
    }

    // Add new diagnostic overlays and collect diagnostic lines
    for (idx, diagnostic) in diagnostics_with_idx {
        if let Some((range, face, priority)) =
            diagnostic_to_overlay(diagnostic, &state.buffer, theme)
        {
            let overlay_id = format!("lsp-diagnostic-{}", idx);
            let message = diagnostic.message.clone();

            let overlay = Overlay::with_id(&mut state.marker_list, range, face, overlay_id)
                .with_priority_value(priority)
                .with_message(message);

            state.overlays.add(overlay);

            // Track the line number for diagnostic indicator
            let line = diagnostic.range.start.line as usize;
            diagnostic_lines.insert(line);
        }
    }

    // Add red bullet point indicators for each unique diagnostic line
    for line in diagnostic_lines {
        state
            .margins
            .set_diagnostic_indicator(line, "●".to_string(), Color::Red);
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::buffer::Buffer;
    use lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};

    #[test]
    fn test_lsp_position_to_byte() {
        let buffer = Buffer::from_str("hello\nworld\ntest");

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
        let buffer = Buffer::from_str("hello world");

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
        let buffer = Buffer::from_str("hello world");

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
        let buffer = Buffer::from_str("line1\nline2\nline3");

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
