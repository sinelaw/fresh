//! Small self-contained style / color helpers used across the split renderer.
//!
//! This module has no dependency on any shared render-time "mega struct".

use crate::primitives::display_width::char_width;
use crate::view::theme::{color_to_rgb, Theme};
use crate::view::ui::view_pipeline::{LineStart, ViewLine};
use fresh_core::api::ViewTokenStyle;
use ratatui::style::{Color, Modifier, Style};
use std::collections::HashSet;

/// Style for inline diagnostic text, selected from overlay priority (severity).
/// Priority values: 100=error, 50=warning, 30=info, 10=hint.
pub(super) fn inline_diagnostic_style(priority: i32, theme: &Theme) -> Style {
    match priority {
        100 => Style::default().fg(theme.diagnostic_error_fg),
        50 => Style::default().fg(theme.diagnostic_warning_fg),
        30 => Style::default().fg(theme.diagnostic_info_fg),
        _ => Style::default().fg(theme.diagnostic_hint_fg),
    }
}

/// Style for fold placeholder text (italic, dimmed).
pub(super) fn fold_placeholder_style(theme: &Theme) -> ViewTokenStyle {
    let fg = color_to_rgb(theme.line_number_fg).or_else(|| color_to_rgb(theme.editor_fg));
    ViewTokenStyle {
        fg,
        bg: None,
        bold: false,
        italic: true,
    }
}

/// Compute a dimmed version of a color for EOF tilde lines.
/// This replaces using `Modifier::DIM` which can bleed through to overlays.
pub(super) fn dim_color_for_tilde(color: Color) -> Color {
    match color {
        Color::Rgb(r, g, b) => {
            // Reduce brightness by ~50% (similar to DIM modifier effect)
            Color::Rgb(r / 2, g / 2, b / 2)
        }
        Color::Indexed(idx) => {
            // For indexed colors, map to a reasonable dim equivalent.
            if idx < 16 {
                Color::Rgb(50, 50, 50)
            } else {
                Color::Rgb(40, 40, 40)
            }
        }
        // Map named colors to dimmed RGB equivalents
        Color::Black => Color::Rgb(15, 15, 15),
        Color::White => Color::Rgb(128, 128, 128),
        Color::Red => Color::Rgb(100, 30, 30),
        Color::Green => Color::Rgb(30, 100, 30),
        Color::Yellow => Color::Rgb(100, 100, 30),
        Color::Blue => Color::Rgb(30, 30, 100),
        Color::Magenta => Color::Rgb(100, 30, 100),
        Color::Cyan => Color::Rgb(30, 100, 100),
        Color::Gray => Color::Rgb(64, 64, 64),
        Color::DarkGray => Color::Rgb(40, 40, 40),
        Color::LightRed => Color::Rgb(128, 50, 50),
        Color::LightGreen => Color::Rgb(50, 128, 50),
        Color::LightYellow => Color::Rgb(128, 128, 50),
        Color::LightBlue => Color::Rgb(50, 50, 128),
        Color::LightMagenta => Color::Rgb(128, 50, 128),
        Color::LightCyan => Color::Rgb(50, 128, 128),
        Color::Reset => Color::Rgb(50, 50, 50),
    }
}

/// Append a fold placeholder string to the given view line, keeping any
/// trailing newline at the end.
pub(super) fn append_fold_placeholder(line: &mut ViewLine, text: &str, style: &ViewTokenStyle) {
    if text.is_empty() {
        return;
    }

    // If this line ends with a newline, temporarily remove it so we can insert
    // the placeholder before the newline.
    let mut removed_newline: Option<(char, Option<usize>, Option<ViewTokenStyle>)> = None;
    if line.ends_with_newline {
        if let Some(last_char) = line.text.chars().last() {
            if last_char == '\n' {
                let removed = line.text.pop();
                if removed.is_some() {
                    let removed_source = line.char_source_bytes.pop().unwrap_or(None);
                    let removed_style = line.char_styles.pop().unwrap_or(None);
                    line.char_visual_cols.pop();
                    let width = char_width(last_char);
                    for _ in 0..width {
                        line.visual_to_char.pop();
                    }
                    removed_newline = Some((last_char, removed_source, removed_style));
                }
            }
        }
    }

    let mut col = line.visual_to_char.len();
    for ch in text.chars() {
        let char_idx = line.char_source_bytes.len();
        let width = char_width(ch);
        line.text.push(ch);
        line.char_source_bytes.push(None);
        line.char_styles.push(Some(style.clone()));
        line.char_visual_cols.push(col);
        for _ in 0..width {
            line.visual_to_char.push(char_idx);
        }
        col += width;
    }

    if let Some((ch, source, style)) = removed_newline {
        let char_idx = line.char_source_bytes.len();
        let width = char_width(ch);
        line.text.push(ch);
        line.char_source_bytes.push(source);
        line.char_styles.push(style);
        line.char_visual_cols.push(col);
        for _ in 0..width {
            line.visual_to_char.push(char_idx);
        }
    }
}

/// Create one or more ViewLines from virtual text content, soft-wrapping
/// the text into segments no wider than `wrap_width` visual columns when
/// that bound is supplied.
///
/// Each resulting line is a self-contained virtual line marked
/// `LineStart::AfterInjectedNewline`, so the renderer's bg-fill path for
/// virtual lines (which is gated on that variant) extends the style's bg
/// to the viewport edge of every wrapped row.
pub(super) fn create_wrapped_virtual_lines(
    text: &str,
    style: Style,
    wrap_width: Option<usize>,
) -> Vec<ViewLine> {
    let token_style = ViewTokenStyle {
        fg: style.fg.and_then(|c| match c {
            Color::Rgb(r, g, b) => Some((r, g, b)),
            _ => None,
        }),
        bg: style.bg.and_then(|c| match c {
            Color::Rgb(r, g, b) => Some((r, g, b)),
            _ => None,
        }),
        bold: style.add_modifier.contains(Modifier::BOLD),
        italic: style.add_modifier.contains(Modifier::ITALIC),
    };

    // Group chars into segments whose visual width stays at or under
    // `wrap_width`. With no wrap width (or width 0, which would be a
    // pathological viewport) we degenerate to a single segment.
    let segments: Vec<Vec<char>> = match wrap_width {
        Some(w) if w > 0 => split_by_visual_width(text, w),
        _ => vec![text.chars().collect()],
    };

    if segments.is_empty() {
        // Empty input still produces one empty virtual line so it
        // contributes a row to the screen, matching prior behaviour.
        return vec![build_virtual_view_line(String::new(), 0, &token_style)];
    }

    segments
        .into_iter()
        .map(|chars| {
            let len = chars.len();
            let segment_text: String = chars.into_iter().collect();
            build_virtual_view_line(segment_text, len, &token_style)
        })
        .collect()
}

fn build_virtual_view_line(text: String, len: usize, token_style: &ViewTokenStyle) -> ViewLine {
    ViewLine {
        text,
        source_start_byte: None,
        char_source_bytes: vec![None; len],
        char_styles: vec![Some(token_style.clone()); len],
        char_visual_cols: (0..len).collect(),
        visual_to_char: (0..len).collect(),
        tab_starts: HashSet::new(),
        line_start: LineStart::AfterInjectedNewline,
        ends_with_newline: true,
    }
}

/// Greedy grapheme-by-grapheme split of `text` into segments whose
/// `unicode_width` does not exceed `wrap_width`. A char wider than the
/// limit (e.g. a double-width CJK glyph in a 1-column viewport) is
/// emitted on its own row to guarantee forward progress.
fn split_by_visual_width(text: &str, wrap_width: usize) -> Vec<Vec<char>> {
    let mut segments: Vec<Vec<char>> = Vec::new();
    let mut current: Vec<char> = Vec::new();
    let mut current_width: usize = 0;

    for ch in text.chars() {
        let w = char_width(ch);
        if !current.is_empty() && current_width + w > wrap_width {
            segments.push(std::mem::take(&mut current));
            current_width = 0;
        }
        current.push(ch);
        current_width += w;
    }

    if !current.is_empty() {
        segments.push(current);
    }

    segments
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn split_by_visual_width_splits_long_text() {
        let text: String = std::iter::repeat('A')
            .take(32)
            .chain(std::iter::repeat('B').take(32))
            .collect();
        let segs = split_by_visual_width(&text, 33);
        assert_eq!(segs.len(), 2);
        assert_eq!(segs[0].len(), 33);
        assert_eq!(segs[1].len(), 31);
    }

    #[test]
    fn split_by_visual_width_keeps_short_text_in_one_segment() {
        let segs = split_by_visual_width("hello", 80);
        assert_eq!(segs.len(), 1);
        assert_eq!(segs[0].len(), 5);
    }

    #[test]
    fn split_by_visual_width_breaks_at_exact_boundary() {
        let segs = split_by_visual_width("AAAAAA", 3);
        assert_eq!(segs.len(), 2);
        assert_eq!(segs[0].len(), 3);
        assert_eq!(segs[1].len(), 3);
    }

    #[test]
    fn split_by_visual_width_handles_double_width_chars() {
        // CJK characters take 2 columns; with wrap_width=4, each segment
        // should fit at most 2 chars.
        let segs = split_by_visual_width("世界你好", 4);
        // 4 chars × width 2 = 8 cols total. With wrap=4, we get 2 segments
        // of 2 chars each.
        assert_eq!(segs.len(), 2);
        assert_eq!(segs[0].len(), 2);
        assert_eq!(segs[1].len(), 2);
    }

    #[test]
    fn split_by_visual_width_makes_progress_even_for_oversized_char() {
        // A double-width char in a 1-col viewport: emit it on its own row
        // so we don't loop forever.
        let segs = split_by_visual_width("世", 1);
        assert_eq!(segs.len(), 1);
        assert_eq!(segs[0].len(), 1);
    }

    #[test]
    fn create_wrapped_virtual_lines_no_wrap_returns_one_line() {
        let lines = create_wrapped_virtual_lines("hello world", Style::default(), None);
        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0].text, "hello world");
        assert_eq!(lines[0].line_start, LineStart::AfterInjectedNewline);
    }

    #[test]
    fn create_wrapped_virtual_lines_splits_under_wrap_width() {
        let text: String = std::iter::repeat('X').take(50).collect();
        let lines = create_wrapped_virtual_lines(&text, Style::default(), Some(20));
        assert_eq!(lines.len(), 3);
        assert_eq!(lines[0].text.chars().count(), 20);
        assert_eq!(lines[1].text.chars().count(), 20);
        assert_eq!(lines[2].text.chars().count(), 10);
        // All segments must be virtual rows so the bg-fill path triggers
        // for each one.
        for line in &lines {
            assert_eq!(line.line_start, LineStart::AfterInjectedNewline);
        }
    }
}
