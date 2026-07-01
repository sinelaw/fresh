//! Small self-contained style / color helpers used across the split renderer.
//!
//! This module has no dependency on any shared render-time "mega struct".

use crate::primitives::display_width::char_width;
use crate::primitives::visual_layout::wrap_str_to_width;
use crate::view::theme::{color_to_rgb, Theme, TokenColorExt};
use crate::view::ui::view_pipeline::{LineStart, ViewLine};
use fresh_core::api::{TokenColor, ViewTokenStyle};
use ratatui::style::{Color, Modifier, Style};
use std::collections::HashSet;

/// Lossy-but-faithful conversion of a resolved ratatui [`Style`] into a
/// wire [`ViewTokenStyle`], so styled content can flow through the token
/// stream (which only carries `ViewTokenStyle`).
///
/// `TokenColor::from_ratatui` captures every `ratatui::Color` variant (RGB,
/// named ANSI, indexed, `Reset`); the bold/italic/underline modifiers are
/// the subset `ViewTokenStyle` can represent. Used for plugin-injected
/// virtual lines and for inline inlay-hint cells spliced before wrapping.
pub(super) fn token_style_from_ratatui(style: Style) -> ViewTokenStyle {
    ViewTokenStyle {
        fg: style.fg.and_then(TokenColor::from_ratatui),
        bg: style.bg.and_then(TokenColor::from_ratatui),
        bold: style.add_modifier.contains(Modifier::BOLD),
        italic: style.add_modifier.contains(Modifier::ITALIC),
        underline: style.add_modifier.contains(Modifier::UNDERLINED),
    }
}

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
    let fg = color_to_rgb(theme.line_number_fg)
        .or_else(|| color_to_rgb(theme.editor_fg))
        .map(|(r, g, b)| TokenColor::Rgb(r, g, b));
    ViewTokenStyle {
        fg,
        bg: None,
        bold: false,
        italic: true,
        underline: false,
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
/// Wrapping uses the shared [`wrap_str_to_width`] helper, so virtual
/// lines break at UAX #29 word boundaries within `WRAP_MAX_LOOKBACK`
/// columns of the hard cap — the same algorithm `apply_wrapping_transform`
/// uses for source lines.  Each resulting line is a self-contained
/// virtual line marked `LineStart::AfterInjectedNewline`, so the
/// renderer's bg-fill path for virtual lines (gated on that variant)
/// extends the style's bg to the viewport edge of every wrapped row.
pub(super) fn create_wrapped_virtual_lines(
    text: &str,
    style: Style,
    wrap_width: Option<usize>,
    gutter_glyph: Option<(String, Color)>,
    text_overlays: &[fresh_core::api::VirtualLineTextOverlay],
) -> Vec<ViewLine> {
    // `TokenColor` faithfully captures every `ratatui::Color` variant
    // (RGB, named ANSI, indexed, `Reset`) so themes like `terminal` —
    // which use named ANSI colors for the diff backgrounds — survive
    // round-tripping through `ViewTokenStyle` and reach the renderer
    // intact. Previously only `Color::Rgb` survived, so virtual lines
    // dropped the bg on ANSI-only themes.
    let token_style = token_style_from_ratatui(style);

    let chunk_ranges = match wrap_width {
        Some(w) if w > 0 => wrap_str_to_width(text, w),
        _ => {
            if text.is_empty() {
                Vec::new()
            } else {
                std::iter::once(0..text.len()).collect()
            }
        }
    };

    if chunk_ranges.is_empty() {
        // Empty input still produces one empty virtual line so it
        // contributes a row to the screen, matching prior behaviour.
        return vec![build_virtual_view_line(
            "",
            &token_style,
            gutter_glyph,
            0,
            text_overlays,
        )];
    }

    // The gutter glyph belongs to the *virtual line as a whole*, not
    // its wrapped sub-rows, so it only goes on the first ViewLine
    // produced; continuation rows get `None`.
    let mut rows: Vec<ViewLine> = Vec::with_capacity(chunk_ranges.len());
    let mut glyph = gutter_glyph;
    for r in chunk_ranges {
        let chunk_byte_offset = r.start;
        rows.push(build_virtual_view_line(
            &text[r],
            &token_style,
            glyph.take(),
            chunk_byte_offset,
            text_overlays,
        ));
    }
    rows
}

fn build_virtual_view_line(
    text: &str,
    token_style: &ViewTokenStyle,
    gutter_glyph: Option<(String, Color)>,
    // Byte offset of `text` within the original (pre-wrap) virtual line
    // text. Used to resolve `text_overlays`, whose `start`/`end` are in
    // the original text's byte coordinates.
    chunk_byte_offset: usize,
    text_overlays: &[fresh_core::api::VirtualLineTextOverlay],
) -> ViewLine {
    let len = text.chars().count();
    let char_styles = if text_overlays.is_empty() {
        vec![Some(token_style.clone()); len]
    } else {
        // Walk char-by-char, OR-ing in modifier flags from any overlay
        // that covers this char's byte. Linear scan over `text_overlays`
        // is fine — typical use case (live-diff word ranges) yields a
        // handful of overlays per line.
        text.char_indices()
            .map(|(b, ch)| {
                let abs_start = chunk_byte_offset + b;
                let abs_end = abs_start + ch.len_utf8();
                let mut s = token_style.clone();
                for o in text_overlays {
                    if (o.end as usize) <= abs_start || (o.start as usize) >= abs_end {
                        continue;
                    }
                    if o.bold {
                        s.bold = true;
                    }
                    if o.underline {
                        s.underline = true;
                    }
                }
                Some(s)
            })
            .collect()
    };
    ViewLine {
        text: text.to_string(),
        source_start_byte: None,
        char_source_bytes: vec![None; len],
        char_styles,
        char_visual_cols: (0..len).collect(),
        visual_to_char: (0..len).collect(),
        tab_starts: HashSet::new(),
        line_start: LineStart::AfterInjectedNewline,
        ends_with_newline: true,
        virtual_gutter_glyph: gutter_glyph,
        virtual_line_style: Some(token_style.clone()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn create_wrapped_virtual_lines_no_wrap_returns_one_line() {
        let lines = create_wrapped_virtual_lines("hello world", Style::default(), None, None, &[]);
        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0].text, "hello world");
        assert_eq!(lines[0].line_start, LineStart::AfterInjectedNewline);
    }

    #[test]
    fn create_wrapped_virtual_lines_empty_input_yields_single_empty_row() {
        let lines = create_wrapped_virtual_lines("", Style::default(), Some(20), None, &[]);
        assert_eq!(lines.len(), 1);
        assert!(lines[0].text.is_empty());
        assert_eq!(lines[0].line_start, LineStart::AfterInjectedNewline);
    }

    #[test]
    fn create_wrapped_virtual_lines_splits_no_boundary_at_hard_cap() {
        // No word boundary anywhere — must hard-cap at width.
        let text: String = std::iter::repeat('X').take(50).collect();
        let lines = create_wrapped_virtual_lines(&text, Style::default(), Some(20), None, &[]);
        assert_eq!(lines.len(), 3);
        assert_eq!(lines[0].text.chars().count(), 20);
        assert_eq!(lines[1].text.chars().count(), 20);
        assert_eq!(lines[2].text.chars().count(), 10);
        // Every segment must be a virtual row so the bg-fill path triggers
        // for each one.
        for line in &lines {
            assert_eq!(line.line_start, LineStart::AfterInjectedNewline);
        }
    }

    #[test]
    fn create_wrapped_virtual_lines_prefers_word_boundary() {
        // With a sentence and width 18, we should break at a space — not
        // mid-word — proving virtual lines now share the source-line
        // wrap algorithm.
        let lines = create_wrapped_virtual_lines(
            "the quick brown fox jumps over the lazy dog",
            Style::default(),
            Some(18),
            None,
            &[],
        );
        assert!(lines.len() >= 2);
        // Concatenating the segment texts must round-trip the input.
        let joined: String = lines.iter().map(|l| l.text.as_str()).collect();
        assert_eq!(joined, "the quick brown fox jumps over the lazy dog");
        // First row must end at a space, not split a word.
        let first = &lines[0].text;
        assert!(
            first.ends_with(' '),
            "expected first row to end at a word boundary; got {first:?}",
        );
    }
}
