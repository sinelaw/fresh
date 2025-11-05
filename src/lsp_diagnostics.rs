///! LSP diagnostics display
///!
///! This module handles converting LSP diagnostics to visual overlays in the editor.
///! Diagnostics are displayed as colored underlines (red for errors, yellow for warnings, etc.)

use crate::buffer::Buffer;
use crate::margin::{MarginAnnotation, MarginContent, MarginPosition};
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

    // Convert line numbers to byte positions
    // For simplicity, we'll use character offsets as byte offsets for now
    // TODO: Properly handle UTF-16 to UTF-8 conversion as per LSP spec
    let start_byte = line_char_to_byte(buffer, start_line, start_char)?;
    let end_byte = line_char_to_byte(buffer, end_line, end_char)?;

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
/// 4. Adds red bullet point margin annotations for lines with diagnostics
pub fn apply_diagnostics_to_state(state: &mut EditorState, diagnostics: &[Diagnostic], theme: &crate::theme::Theme) {
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
        state.overlays.remove_by_id(&id);
    }

    // Clear existing diagnostic margin annotations
    // Use a separate prefix for margin annotations
    state.margins.remove_by_id("lsp-diagnostic-margin");

    // Track unique lines with diagnostics to avoid duplicate margin markers
    let mut diagnostic_lines = std::collections::HashSet::new();

    // Add new diagnostic overlays and collect diagnostic lines
    for (idx, diagnostic) in diagnostics.iter().enumerate() {
        if let Some((range, face, priority)) = diagnostic_to_overlay(diagnostic, &state.buffer, theme) {
            let overlay_id = format!("lsp-diagnostic-{}", idx);
            let message = diagnostic.message.clone();

            let overlay = Overlay::with_id(range, face, overlay_id)
                .with_priority_value(priority)
                .with_message(message);

            state.overlays.add(overlay);

            // Track the line number for margin annotation
            let line = diagnostic.range.start.line as usize;
            diagnostic_lines.insert(line);
        }
    }

    // Add red bullet point margin annotations for each unique diagnostic line
    for line in diagnostic_lines {
        let annotation = MarginAnnotation::with_id(
            line,
            MarginPosition::Left,
            MarginContent::colored_symbol("●", Color::Red),
            "lsp-diagnostic-margin".to_string(),
        );
        state.margins.add_annotation(annotation);
    }
}

/// Convert line/character position to byte offset
/// Returns None if the position is out of bounds
fn line_char_to_byte(buffer: &Buffer, line: usize, character: usize) -> Option<usize> {
    // Iterate to the target line
    let mut iter = buffer.line_iterator(0);
    let mut current_line = 0;

    while current_line < line {
        if iter.next().is_none() {
            return None; // Line doesn't exist
        }
        current_line += 1;
    }

    // Get the start of the target line
    let (line_start, line_content) = iter.next()?;

    // Character offset within the line
    // For now, treat character as byte offset (UTF-8)
    // TODO: Handle UTF-16 properly for full LSP compliance
    let byte_offset = character.min(line_content.len());

    Some(line_start + byte_offset)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::buffer::Buffer;
    use lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};

    #[test]
    fn test_line_char_to_byte() {
        let buffer = Buffer::from_str("hello\nworld\ntest");

        // Line 0, character 0
        assert_eq!(line_char_to_byte(&buffer, 0, 0), Some(0));

        // Line 0, character 5 (end of "hello")
        assert_eq!(line_char_to_byte(&buffer, 0, 5), Some(5));

        // Line 1, character 0 (start of "world")
        assert_eq!(line_char_to_byte(&buffer, 1, 0), Some(6));

        // Line 1, character 5 (end of "world")
        assert_eq!(line_char_to_byte(&buffer, 1, 5), Some(11));

        // Line 2, character 0 (start of "test")
        assert_eq!(line_char_to_byte(&buffer, 2, 0), Some(12));

        // Out of bounds line
        assert_eq!(line_char_to_byte(&buffer, 10, 0), None);
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
