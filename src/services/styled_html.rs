//! Styled text rendering for clipboard copy feature
//!
//! This module renders styled text with syntax highlighting as HTML
//! for pasting into rich text editors (Google Docs, Word, etc.)

use crate::primitives::highlighter::HighlightSpan;
use crate::view::theme::Theme;
use ratatui::style::Color;

/// Convert a ratatui Color to a CSS hex color string
fn color_to_css(color: Color, default: &str) -> String {
    match color {
        Color::Rgb(r, g, b) => format!("#{:02x}{:02x}{:02x}", r, g, b),
        Color::Black => "#000000".to_string(),
        Color::Red => "#cd3131".to_string(),
        Color::Green => "#0dbc79".to_string(),
        Color::Yellow => "#e5e510".to_string(),
        Color::Blue => "#2472c8".to_string(),
        Color::Magenta => "#bc3fbc".to_string(),
        Color::Cyan => "#11a8cd".to_string(),
        Color::Gray => "#808080".to_string(),
        Color::DarkGray => "#505050".to_string(),
        Color::LightRed => "#f14c4c".to_string(),
        Color::LightGreen => "#23d18b".to_string(),
        Color::LightYellow => "#f5f543".to_string(),
        Color::LightBlue => "#3b8eea".to_string(),
        Color::LightMagenta => "#d670d6".to_string(),
        Color::LightCyan => "#29b8db".to_string(),
        Color::White => "#e5e5e5".to_string(),
        Color::Reset | Color::Indexed(_) => default.to_string(),
    }
}

/// Render styled text with syntax highlighting to HTML with inline CSS
///
/// The generated HTML uses a `<pre>` block with inline styles for each
/// syntax-highlighted span. This allows pasting into rich text editors
/// like Google Docs, Word, etc.
///
/// # Arguments
/// * `text` - The text to render
/// * `highlight_spans` - Syntax highlighting spans with byte ranges and colors
/// * `theme` - The theme to use for background and default foreground colors
///
/// # Returns
/// HTML string with inline styles
pub fn render_styled_html(text: &str, highlight_spans: &[HighlightSpan], theme: &Theme) -> String {
    let bg_color = color_to_css(theme.editor_bg, "#1e1e1e");
    let fg_color = color_to_css(theme.editor_fg, "#d4d4d4");

    // Build a map of byte offset to color for quick lookup
    let mut color_map: Vec<Option<Color>> = vec![None; text.len()];
    for span in highlight_spans {
        let start = span.range.start.min(text.len());
        let end = span.range.end.min(text.len());
        for slot in &mut color_map[start..end] {
            *slot = Some(span.color);
        }
    }

    // Build HTML with spans for colored regions
    let mut html = String::new();
    html.push_str(&format!(
        "<pre style=\"background-color:{};color:{};font-family:'Fira Mono','Fira Code',Consolas,'Courier New',monospace;font-size:14px;padding:12px 16px;border-radius:6px;margin:0;white-space:pre;overflow-x:auto;\">",
        bg_color, fg_color
    ));

    let mut current_color: Option<Color> = None;
    let mut span_open = false;
    let mut byte_offset = 0;

    for ch in text.chars() {
        let char_byte_len = ch.len_utf8();

        // Get color for this character
        let char_color = if byte_offset < color_map.len() {
            color_map[byte_offset]
        } else {
            None
        };

        // Check if we need to change the color span
        if char_color != current_color {
            // Close previous span if open
            if span_open {
                html.push_str("</span>");
                span_open = false;
            }

            // Open new span if we have a color
            if let Some(color) = char_color {
                let css_color = color_to_css(color, &fg_color);
                html.push_str(&format!("<span style=\"color:{};\">", css_color));
                span_open = true;
            }

            current_color = char_color;
        }

        // Escape HTML special characters and add the character
        match ch {
            '<' => html.push_str("&lt;"),
            '>' => html.push_str("&gt;"),
            '&' => html.push_str("&amp;"),
            '"' => html.push_str("&quot;"),
            '\'' => html.push_str("&#39;"),
            _ => html.push(ch),
        }

        byte_offset += char_byte_len;
    }

    // Close any remaining span
    if span_open {
        html.push_str("</span>");
    }

    html.push_str("</pre>");
    html
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::view::theme;

    #[test]
    fn test_render_html_simple() {
        let text = "Hello, World!";
        let spans = vec![];
        let theme = Theme::from_name(theme::THEME_DARK).unwrap();

        let html = render_styled_html(text, &spans, &theme);

        assert!(html.starts_with("<pre style=\""));
        assert!(html.ends_with("</pre>"));
        assert!(html.contains("Hello, World!"));
    }

    #[test]
    fn test_render_html_escapes_special_chars() {
        let text = "<script>&test</script>";
        let spans = vec![];
        let theme = Theme::from_name(theme::THEME_DARK).unwrap();

        let html = render_styled_html(text, &spans, &theme);

        assert!(html.contains("&lt;script&gt;"));
        assert!(html.contains("&amp;test"));
        assert!(!html.contains("<script>"));
    }

    #[test]
    fn test_render_html_with_highlights() {
        use std::ops::Range;

        let text = "fn main()";
        let spans = vec![HighlightSpan {
            range: Range { start: 0, end: 2 },
            color: Color::Blue,
        }];
        let theme = Theme::from_name(theme::THEME_DARK).unwrap();

        let html = render_styled_html(text, &spans, &theme);

        // Should contain a span with blue color for "fn"
        assert!(html.contains("<span style=\"color:#2472c8;\">fn</span>"));
        assert!(html.contains("main()"));
    }

    #[test]
    fn test_color_to_css() {
        assert_eq!(color_to_css(Color::Black, "#fff"), "#000000");
        assert_eq!(color_to_css(Color::Rgb(255, 128, 0), "#fff"), "#ff8000");
        assert_eq!(color_to_css(Color::Reset, "#default"), "#default");
    }
}
