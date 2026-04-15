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

/// Create a ViewLine from virtual text content (for LineAbove/LineBelow).
pub(super) fn create_virtual_line(text: &str, style: Style) -> ViewLine {
    let text = text.to_string();
    let len = text.chars().count();

    // Convert ratatui Style to ViewTokenStyle
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

    ViewLine {
        text,
        source_start_byte: None,
        char_source_bytes: vec![None; len],
        char_styles: vec![Some(token_style); len],
        char_visual_cols: (0..len).collect(),
        visual_to_char: (0..len).collect(),
        tab_starts: HashSet::new(),
        line_start: LineStart::AfterInjectedNewline,
        ends_with_newline: true,
    }
}
