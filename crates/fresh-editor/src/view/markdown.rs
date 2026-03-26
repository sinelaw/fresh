//! Markdown parsing and rendering for terminal display
//!
//! This module provides markdown-to-styled-text conversion for popups,
//! hover documentation, and other UI elements. It also provides word
//! wrapping utilities for styled text.

use crate::primitives::grammar::GrammarRegistry;
use crate::primitives::highlight_engine::highlight_string;
use crate::primitives::highlighter::HighlightSpan;
use pulldown_cmark::{Event, Options, Parser, Tag, TagEnd};
use ratatui::style::{Color, Modifier, Style};

/// Whether a character is a space-like separator (regular space or NBSP).
fn is_space(ch: char) -> bool {
    ch == ' ' || ch == '\u{00A0}'
}

/// Calculate hanging indent width from leading spaces, clamped so that
/// at least 10 characters of content remain.
fn hanging_indent_width(leading_spaces: usize, max_width: usize) -> usize {
    if leading_spaces + 10 > max_width {
        0
    } else {
        leading_spaces
    }
}

/// Count the number of leading space-like characters (space or NBSP) in a string.
fn count_leading_spaces(text: &str) -> usize {
    text.chars().take_while(|&ch| is_space(ch)).count()
}

/// Word-wrap a single line of text to fit within a given width.
/// Breaks at word boundaries (spaces) when possible.
/// Falls back to character-based breaking for words longer than max_width.
/// Continuation lines are indented to match the original line's leading whitespace.
pub fn wrap_text_line(text: &str, max_width: usize) -> Vec<String> {
    if max_width == 0 {
        return vec![text.to_string()];
    }

    let indent_width = hanging_indent_width(count_leading_spaces(text), max_width);
    let indent = " ".repeat(indent_width);

    let mut result = Vec::new();
    let mut current_line = String::new();
    let mut current_width = 0;

    for (word, word_width) in WordSplitter::new(text) {
        // Word fits on current line
        if current_width + word_width <= max_width {
            current_line.push_str(&word);
            current_width += word_width;
            continue;
        }

        // First word on line but too long — break mid-word
        if current_line.is_empty() {
            for ch in word.chars() {
                let ch_width = unicode_width::UnicodeWidthChar::width(ch).unwrap_or(1);
                if current_width + ch_width > max_width && !current_line.is_empty() {
                    result.push(current_line);
                    current_line = indent.clone();
                    current_width = indent_width;
                }
                current_line.push(ch);
                current_width += ch_width;
            }
            continue;
        }

        // Start a new line with hanging indent
        result.push(current_line);
        let trimmed = word.trim_start_matches(is_space);
        current_line = format!("{}{}", indent, trimmed);
        current_width = indent_width + unicode_width::UnicodeWidthStr::width(trimmed);
    }

    if !current_line.is_empty() || result.is_empty() {
        result.push(current_line);
    }

    result
}

/// Word-wrap a vector of text lines to fit within a given width.
pub fn wrap_text_lines(lines: &[String], max_width: usize) -> Vec<String> {
    let mut result = Vec::new();
    for line in lines {
        if line.is_empty() {
            result.push(String::new());
        } else {
            result.extend(wrap_text_line(line, max_width));
        }
    }
    result
}

/// Word-wrap styled lines to fit within a given width.
/// Breaks at word boundaries (spaces) when possible, preserving styling.
/// Continuation lines are indented to match the original line's leading whitespace.
pub fn wrap_styled_lines(lines: &[StyledLine], max_width: usize) -> Vec<StyledLine> {
    if max_width == 0 {
        return lines.to_vec();
    }

    let mut result = Vec::new();

    for line in lines {
        let total_width: usize = line
            .spans
            .iter()
            .map(|s| unicode_width::UnicodeWidthStr::width(s.text.as_str()))
            .sum();

        if total_width <= max_width {
            result.push(line.clone());
            continue;
        }

        // Calculate leading indent across spans (space or NBSP)
        let leading_spaces = {
            let mut count = 0usize;
            'outer: for span in &line.spans {
                for ch in span.text.chars() {
                    if is_space(ch) {
                        count += 1;
                    } else {
                        break 'outer;
                    }
                }
            }
            count
        };
        let indent_width = hanging_indent_width(leading_spaces, max_width);

        // Flatten spans into (text, style, link_url) segments split at word boundaries
        let segments = flatten_styled_segments(&line.spans);

        let mut current_line = StyledLine::new();
        let mut current_width = 0;

        for (segment, style, link_url) in segments {
            let seg_width = unicode_width::UnicodeWidthStr::width(segment.as_str());

            // Segment fits on current line
            if current_width + seg_width <= max_width {
                current_line.push_with_link(segment, style, link_url);
                current_width += seg_width;
                continue;
            }

            // First segment on line but too long — break mid-word
            if current_width == 0 {
                let mut remaining = segment.as_str();
                while !remaining.is_empty() {
                    let available = max_width.saturating_sub(current_width);
                    if available == 0 {
                        result.push(current_line);
                        current_line = new_continuation_line(indent_width);
                        current_width = indent_width;
                        continue;
                    }

                    let (take, rest) = split_at_width(remaining, available);
                    current_line.push_with_link(take.to_string(), style, link_url.clone());
                    current_width += unicode_width::UnicodeWidthStr::width(take);
                    remaining = rest;
                }
                continue;
            }

            // Start new continuation line with hanging indent
            result.push(current_line);
            current_line = new_continuation_line(indent_width);
            // Trim leading space/NBSP — either replaced by hanging indent or
            // just a word separator that shouldn't start a new line.
            let trimmed = segment.trim_start_matches(is_space);
            let trimmed_width = unicode_width::UnicodeWidthStr::width(trimmed);
            current_line.push_with_link(trimmed.to_string(), style, link_url);
            current_width = indent_width + trimmed_width;
        }

        if !current_line.spans.is_empty() {
            result.push(current_line);
        }
    }

    result
}

/// Create a new `StyledLine` pre-filled with hanging indent spaces.
fn new_continuation_line(indent_width: usize) -> StyledLine {
    let mut line = StyledLine::new();
    if indent_width > 0 {
        line.push(" ".repeat(indent_width), Style::default());
    }
    line
}

/// Split `text` so the first part fits within `available` display columns.
/// Returns (taken, remaining).
fn split_at_width(text: &str, available: usize) -> (&str, &str) {
    let mut take_chars = 0;
    let mut take_width = 0;
    for ch in text.chars() {
        let w = unicode_width::UnicodeWidthChar::width(ch).unwrap_or(1);
        if take_width + w > available && take_chars > 0 {
            break;
        }
        take_width += w;
        take_chars += 1;
    }
    let byte_idx = text
        .char_indices()
        .nth(take_chars)
        .map(|(i, _)| i)
        .unwrap_or(text.len());
    text.split_at(byte_idx)
}

/// Flatten styled spans into word-boundary segments, preserving style and link info.
fn flatten_styled_segments(spans: &[StyledSpan]) -> Vec<(String, Style, Option<String>)> {
    let mut segments = Vec::new();
    for span in spans {
        for (word, _width) in WordSplitter::new(&span.text) {
            segments.push((word, span.style, span.link_url.clone()));
        }
    }
    segments
}

/// Iterator that splits text into word segments (spaces + non-spaces),
/// yielding `(segment_text, display_width)` pairs.
struct WordSplitter<'a> {
    chars: std::iter::Peekable<std::str::Chars<'a>>,
}

impl<'a> WordSplitter<'a> {
    fn new(text: &'a str) -> Self {
        Self {
            chars: text.chars().peekable(),
        }
    }
}

impl<'a> Iterator for WordSplitter<'a> {
    type Item = (String, usize);

    fn next(&mut self) -> Option<Self::Item> {
        self.chars.peek()?;

        let mut word = String::new();
        let mut width = 0;

        // Collect leading spaces (regular or NBSP)
        while let Some(&ch) = self.chars.peek() {
            if !is_space(ch) {
                break;
            }
            let w = unicode_width::UnicodeWidthChar::width(ch).unwrap_or(1);
            word.push(ch);
            width += w;
            self.chars.next();
        }

        // Collect non-space characters
        while let Some(&ch) = self.chars.peek() {
            if is_space(ch) {
                break;
            }
            let w = unicode_width::UnicodeWidthChar::width(ch).unwrap_or(1);
            word.push(ch);
            width += w;
            self.chars.next();
        }

        if word.is_empty() {
            None
        } else {
            Some((word, width))
        }
    }
}

/// A styled span for markdown rendering
#[derive(Debug, Clone, PartialEq)]
pub struct StyledSpan {
    pub text: String,
    pub style: Style,
    /// Optional URL if this span is part of a link
    pub link_url: Option<String>,
}

/// A line of styled spans for markdown rendering
#[derive(Debug, Clone, PartialEq)]
pub struct StyledLine {
    pub spans: Vec<StyledSpan>,
}

impl StyledLine {
    pub fn new() -> Self {
        Self { spans: Vec::new() }
    }

    pub fn push(&mut self, text: String, style: Style) {
        self.spans.push(StyledSpan {
            text,
            style,
            link_url: None,
        });
    }

    /// Push a span with an optional link URL
    pub fn push_with_link(&mut self, text: String, style: Style, link_url: Option<String>) {
        self.spans.push(StyledSpan {
            text,
            style,
            link_url,
        });
    }

    /// Find the link URL at the given column position (0-indexed)
    /// Returns None if there's no link at that position
    pub fn link_at_column(&self, column: usize) -> Option<&str> {
        let mut current_col = 0;
        for span in &self.spans {
            let span_width = unicode_width::UnicodeWidthStr::width(span.text.as_str());
            if column >= current_col && column < current_col + span_width {
                // Found the span at this column
                return span.link_url.as_deref();
            }
            current_col += span_width;
        }
        None
    }

    /// Get the plain text content (without styling)
    pub fn plain_text(&self) -> String {
        self.spans.iter().map(|s| s.text.as_str()).collect()
    }
}

impl Default for StyledLine {
    fn default() -> Self {
        Self::new()
    }
}

/// Convert highlight spans to styled lines for code blocks
fn highlight_code_to_styled_lines(
    code: &str,
    spans: &[HighlightSpan],
    theme: &crate::view::theme::Theme,
) -> Vec<StyledLine> {
    let mut result = vec![StyledLine::new()];
    let code_bg = theme.inline_code_bg;
    let default_fg = theme.help_key_fg;

    let bytes = code.as_bytes();
    let mut pos = 0;

    for span in spans {
        // Add unhighlighted text before this span
        if span.range.start > pos {
            let text = String::from_utf8_lossy(&bytes[pos..span.range.start]);
            add_text_to_lines(
                &mut result,
                &text,
                Style::default().fg(default_fg).bg(code_bg),
                None,
            );
        }

        // Add highlighted text
        let text = String::from_utf8_lossy(&bytes[span.range.start..span.range.end]);
        add_text_to_lines(
            &mut result,
            &text,
            Style::default().fg(span.color).bg(code_bg),
            None,
        );

        pos = span.range.end;
    }

    // Add remaining unhighlighted text
    if pos < bytes.len() {
        let text = String::from_utf8_lossy(&bytes[pos..]);
        add_text_to_lines(
            &mut result,
            &text,
            Style::default().fg(default_fg).bg(code_bg),
            None,
        );
    }

    result
}

/// Add text to styled lines, splitting on newlines.
/// Each `\n` starts a new `StyledLine`. Non-empty segments are pushed with
/// the given style and optional link URL.
fn add_text_to_lines(
    lines: &mut Vec<StyledLine>,
    text: &str,
    style: Style,
    link_url: Option<String>,
) {
    for (i, part) in text.split('\n').enumerate() {
        if i > 0 {
            lines.push(StyledLine::new());
        }
        if !part.is_empty() {
            if let Some(line) = lines.last_mut() {
                line.push_with_link(part.to_string(), style, link_url.clone());
            }
        }
    }
}

/// Preserve leading whitespace in text by replacing leading regular spaces
/// with non-breaking spaces. Markdown parsers strip leading spaces from
/// paragraphs, but LSP documentation (e.g. Python docstrings) uses indentation
/// for structure. Non-breaking spaces survive markdown parsing.
fn preserve_leading_whitespace(text: &str) -> String {
    text.lines()
        .map(|line| {
            let indent = line.len() - line.trim_start_matches(' ').len();
            if indent > 0 {
                format!("{}{}", "\u{00A0}".repeat(indent), &line[indent..])
            } else {
                line.to_string()
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// Parse markdown text into styled lines for terminal rendering
///
/// If `registry` is provided, uses syntect for syntax highlighting in code blocks,
/// which supports ~150+ languages. If None, falls back to uniform code styling.
pub fn parse_markdown(
    text: &str,
    theme: &crate::view::theme::Theme,
    registry: Option<&GrammarRegistry>,
) -> Vec<StyledLine> {
    // Preserve leading whitespace (as NBSP) before markdown parsing,
    // since pulldown_cmark strips leading spaces from paragraph text.
    let preserved = preserve_leading_whitespace(text);

    let mut options = Options::empty();
    options.insert(Options::ENABLE_STRIKETHROUGH);

    let parser = Parser::new_ext(&preserved, options);
    let mut lines: Vec<StyledLine> = vec![StyledLine::new()];

    // Style stack for nested formatting
    let mut style_stack: Vec<Style> = vec![Style::default()];
    let mut in_code_block = false;
    let mut code_block_lang = String::new();
    // Track current link URL (if inside a link)
    let mut current_link_url: Option<String> = None;

    for event in parser {
        match event {
            Event::Start(tag) => {
                match tag {
                    Tag::Strong => {
                        let current = *style_stack.last().unwrap_or(&Style::default());
                        style_stack.push(current.add_modifier(Modifier::BOLD));
                    }
                    Tag::Emphasis => {
                        let current = *style_stack.last().unwrap_or(&Style::default());
                        style_stack.push(current.add_modifier(Modifier::ITALIC));
                    }
                    Tag::Strikethrough => {
                        let current = *style_stack.last().unwrap_or(&Style::default());
                        style_stack.push(current.add_modifier(Modifier::CROSSED_OUT));
                    }
                    Tag::CodeBlock(kind) => {
                        in_code_block = true;
                        code_block_lang = match kind {
                            pulldown_cmark::CodeBlockKind::Fenced(lang) => lang.to_string(),
                            pulldown_cmark::CodeBlockKind::Indented => String::new(),
                        };
                        // Start new line for code block
                        if !lines.last().map(|l| l.spans.is_empty()).unwrap_or(true) {
                            lines.push(StyledLine::new());
                        }
                    }
                    Tag::Heading { .. } => {
                        let current = *style_stack.last().unwrap_or(&Style::default());
                        style_stack
                            .push(current.add_modifier(Modifier::BOLD).fg(theme.help_key_fg));
                    }
                    Tag::Link { dest_url, .. } => {
                        let current = *style_stack.last().unwrap_or(&Style::default());
                        style_stack
                            .push(current.add_modifier(Modifier::UNDERLINED).fg(Color::Cyan));
                        // Store the link URL for text spans inside this link
                        current_link_url = Some(dest_url.to_string());
                    }
                    Tag::Image { .. } => {
                        let current = *style_stack.last().unwrap_or(&Style::default());
                        style_stack
                            .push(current.add_modifier(Modifier::UNDERLINED).fg(Color::Cyan));
                    }
                    Tag::List(_) | Tag::Item => {
                        // Start list items on new line
                        if !lines.last().map(|l| l.spans.is_empty()).unwrap_or(true) {
                            lines.push(StyledLine::new());
                        }
                    }
                    Tag::Paragraph => {
                        // Start paragraphs on new line if we have any prior content.
                        // This preserves blank lines from previous paragraph ends.
                        let has_prior_content = lines.iter().any(|l| !l.spans.is_empty());
                        if has_prior_content {
                            lines.push(StyledLine::new());
                        }
                    }
                    _ => {}
                }
            }
            Event::End(tag_end) => {
                match tag_end {
                    TagEnd::Strong
                    | TagEnd::Emphasis
                    | TagEnd::Strikethrough
                    | TagEnd::Heading(_)
                    | TagEnd::Image => {
                        style_stack.pop();
                    }
                    TagEnd::Link => {
                        style_stack.pop();
                        // Clear link URL when exiting the link
                        current_link_url = None;
                    }
                    TagEnd::CodeBlock => {
                        in_code_block = false;
                        code_block_lang.clear();
                        // End code block with new line
                        lines.push(StyledLine::new());
                    }
                    TagEnd::Paragraph => {
                        // Add blank line after paragraph
                        lines.push(StyledLine::new());
                    }
                    TagEnd::Item => {
                        // Items end naturally
                    }
                    _ => {}
                }
            }
            Event::Text(text) => {
                if in_code_block {
                    // Try syntax highlighting for code blocks using syntect
                    let spans = if let Some(reg) = registry {
                        if !code_block_lang.is_empty() {
                            let s = highlight_string(&text, &code_block_lang, reg, theme);
                            // Check coverage - if < 20% highlighted, content may not be valid code
                            let highlighted_bytes: usize =
                                s.iter().map(|span| span.range.end - span.range.start).sum();
                            let non_ws_bytes =
                                text.bytes().filter(|b| !b.is_ascii_whitespace()).count();
                            let good_coverage =
                                non_ws_bytes == 0 || highlighted_bytes * 5 >= non_ws_bytes;
                            if good_coverage {
                                s
                            } else {
                                Vec::new()
                            }
                        } else {
                            Vec::new()
                        }
                    } else {
                        Vec::new()
                    };

                    if !spans.is_empty() {
                        let highlighted_lines =
                            highlight_code_to_styled_lines(&text, &spans, theme);
                        for (i, styled_line) in highlighted_lines.into_iter().enumerate() {
                            if i > 0 {
                                lines.push(StyledLine::new());
                            }
                            // Merge spans into the current line
                            if let Some(current_line) = lines.last_mut() {
                                for span in styled_line.spans {
                                    current_line.push(span.text, span.style);
                                }
                            }
                        }
                    } else {
                        // Fallback: uniform code style for unknown languages
                        let code_style = Style::default()
                            .fg(theme.help_key_fg)
                            .bg(theme.inline_code_bg);
                        add_text_to_lines(&mut lines, &text, code_style, None);
                    }
                } else {
                    let current_style = *style_stack.last().unwrap_or(&Style::default());
                    add_text_to_lines(&mut lines, &text, current_style, current_link_url.clone());
                }
            }
            Event::Code(code) => {
                // Inline code - render with background styling (no backticks needed)
                let style = Style::default()
                    .fg(theme.help_key_fg)
                    .bg(theme.inline_code_bg);
                if let Some(line) = lines.last_mut() {
                    line.push(code.to_string(), style);
                }
            }
            Event::SoftBreak => {
                // Soft break - preserve as newline for better docstring/hover formatting
                // (Standard markdown renders soft breaks as spaces, but for LSP hover
                // content which often contains formatted docstrings, newlines are better)
                lines.push(StyledLine::new());
            }
            Event::HardBreak => {
                // Hard break - new line
                lines.push(StyledLine::new());
            }
            Event::Rule => {
                // Horizontal rule
                lines.push(StyledLine::new());
                if let Some(line) = lines.last_mut() {
                    line.push("─".repeat(40), Style::default().fg(Color::DarkGray));
                }
                lines.push(StyledLine::new());
            }
            _ => {}
        }
    }

    // Remove trailing empty lines
    while lines.last().map(|l| l.spans.is_empty()).unwrap_or(false) {
        lines.pop();
    }

    lines
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::view::theme;
    use crate::view::theme::Theme;

    fn has_modifier(line: &StyledLine, modifier: Modifier) -> bool {
        line.spans
            .iter()
            .any(|s| s.style.add_modifier.contains(modifier))
    }

    #[test]
    fn test_plain_text() {
        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();
        let lines = parse_markdown("Hello world", &theme, None);

        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0].plain_text(), "Hello world");
    }

    #[test]
    fn test_bold_text() {
        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();
        let lines = parse_markdown("This is **bold** text", &theme, None);

        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0].plain_text(), "This is bold text");

        // Check that "bold" span has BOLD modifier
        let bold_span = lines[0].spans.iter().find(|s| s.text == "bold");
        assert!(bold_span.is_some(), "Should have a 'bold' span");
        assert!(
            bold_span
                .unwrap()
                .style
                .add_modifier
                .contains(Modifier::BOLD),
            "Bold span should have BOLD modifier"
        );
    }

    #[test]
    fn test_italic_text() {
        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();
        let lines = parse_markdown("This is *italic* text", &theme, None);

        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0].plain_text(), "This is italic text");

        let italic_span = lines[0].spans.iter().find(|s| s.text == "italic");
        assert!(italic_span.is_some(), "Should have an 'italic' span");
        assert!(
            italic_span
                .unwrap()
                .style
                .add_modifier
                .contains(Modifier::ITALIC),
            "Italic span should have ITALIC modifier"
        );
    }

    #[test]
    fn test_strikethrough_text() {
        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();
        let lines = parse_markdown("This is ~~deleted~~ text", &theme, None);

        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0].plain_text(), "This is deleted text");

        let strike_span = lines[0].spans.iter().find(|s| s.text == "deleted");
        assert!(strike_span.is_some(), "Should have a 'deleted' span");
        assert!(
            strike_span
                .unwrap()
                .style
                .add_modifier
                .contains(Modifier::CROSSED_OUT),
            "Strikethrough span should have CROSSED_OUT modifier"
        );
    }

    #[test]
    fn test_inline_code() {
        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();
        let lines = parse_markdown("Use `println!` to print", &theme, None);

        assert_eq!(lines.len(), 1);
        // Inline code is rendered without backticks (styling indicates it's code)
        assert_eq!(lines[0].plain_text(), "Use println! to print");

        // Inline code should have background color
        let code_span = lines[0].spans.iter().find(|s| s.text.contains("println"));
        assert!(code_span.is_some(), "Should have a code span");
        assert!(
            code_span.unwrap().style.bg.is_some(),
            "Inline code should have background color"
        );
    }

    #[test]
    fn test_code_block() {
        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();
        let lines = parse_markdown("```rust\nfn main() {}\n```", &theme, None);

        // Code block should have content with background
        let code_line = lines.iter().find(|l| l.plain_text().contains("fn"));
        assert!(code_line.is_some(), "Should have code block content");

        // With syntax highlighting, "fn" may be in its own span
        // Check that at least one span has background color
        let has_bg = code_line
            .unwrap()
            .spans
            .iter()
            .any(|s| s.style.bg.is_some());
        assert!(has_bg, "Code block should have background color");
    }

    #[test]
    fn test_code_block_syntax_highlighting() {
        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        // Rust code with keywords and strings that should get different colors
        let markdown = "```rust\nfn main() {\n    println!(\"Hello\");\n}\n```";
        let lines = parse_markdown(markdown, &theme, Some(&registry));

        // Should have parsed lines with content
        assert!(!lines.is_empty(), "Should have parsed lines");

        // Collect all colors used in the code block
        let mut colors_used = std::collections::HashSet::new();
        for line in &lines {
            for span in &line.spans {
                if let Some(fg) = span.style.fg {
                    colors_used.insert(format!("{:?}", fg));
                }
            }
        }

        // Should have multiple different colors (syntax highlighting)
        // Not just a single uniform color
        assert!(
            colors_used.len() > 1,
            "Code block should have multiple colors for syntax highlighting, got: {:?}",
            colors_used
        );

        // Verify the code content is preserved
        let all_text: String = lines
            .iter()
            .map(|l| l.plain_text())
            .collect::<Vec<_>>()
            .join("");
        assert!(all_text.contains("fn"), "Should contain 'fn' keyword");
        assert!(all_text.contains("main"), "Should contain 'main'");
        assert!(all_text.contains("println"), "Should contain 'println'");
    }

    #[test]
    fn test_code_block_unknown_language_fallback() {
        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();
        // Unknown language should fallback to uniform styling
        let markdown = "```unknownlang\nsome code here\n```";
        let lines = parse_markdown(markdown, &theme, None);

        // Should have parsed lines
        assert!(!lines.is_empty(), "Should have parsed lines");

        // Content should be preserved
        let all_text: String = lines
            .iter()
            .map(|l| l.plain_text())
            .collect::<Vec<_>>()
            .join("");
        assert!(
            all_text.contains("some code here"),
            "Should contain the code"
        );

        // All spans should have the fallback code style (uniform color)
        let code_line = lines.iter().find(|l| l.plain_text().contains("some code"));
        if let Some(line) = code_line {
            for span in &line.spans {
                assert!(span.style.bg.is_some(), "Code should have background color");
            }
        }
    }

    #[test]
    fn test_heading() {
        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();
        let lines = parse_markdown("# Heading\n\nContent", &theme, None);

        // Heading should be bold
        let heading_line = &lines[0];
        assert!(
            has_modifier(heading_line, Modifier::BOLD),
            "Heading should be bold"
        );
        assert_eq!(heading_line.plain_text(), "Heading");
    }

    #[test]
    fn test_link() {
        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();
        let lines = parse_markdown("Click [here](https://example.com) for more", &theme, None);

        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0].plain_text(), "Click here for more");

        // Link text should be underlined and cyan
        let link_span = lines[0].spans.iter().find(|s| s.text == "here");
        assert!(link_span.is_some(), "Should have 'here' span");
        let style = link_span.unwrap().style;
        assert!(
            style.add_modifier.contains(Modifier::UNDERLINED),
            "Link should be underlined"
        );
        assert_eq!(style.fg, Some(Color::Cyan), "Link should be cyan");
    }

    #[test]
    fn test_link_url_stored() {
        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();
        let lines = parse_markdown("Click [here](https://example.com) for more", &theme, None);

        assert_eq!(lines.len(), 1);

        // The "here" span should have the link URL stored
        let link_span = lines[0].spans.iter().find(|s| s.text == "here");
        assert!(link_span.is_some(), "Should have 'here' span");
        assert_eq!(
            link_span.unwrap().link_url,
            Some("https://example.com".to_string()),
            "Link span should store the URL"
        );

        // Non-link spans should not have a URL
        let click_span = lines[0].spans.iter().find(|s| s.text == "Click ");
        assert!(click_span.is_some(), "Should have 'Click ' span");
        assert_eq!(
            click_span.unwrap().link_url,
            None,
            "Non-link span should not have URL"
        );
    }

    #[test]
    fn test_link_at_column() {
        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();
        let lines = parse_markdown("Click [here](https://example.com) for more", &theme, None);

        assert_eq!(lines.len(), 1);
        let line = &lines[0];

        // "Click " is 6 chars (0-5), "here" is 4 chars (6-9), " for more" is after
        // Column 0-5: "Click " - no link
        assert_eq!(
            line.link_at_column(0),
            None,
            "Column 0 should not be a link"
        );
        assert_eq!(
            line.link_at_column(5),
            None,
            "Column 5 should not be a link"
        );

        // Column 6-9: "here" - link
        assert_eq!(
            line.link_at_column(6),
            Some("https://example.com"),
            "Column 6 should be the link"
        );
        assert_eq!(
            line.link_at_column(9),
            Some("https://example.com"),
            "Column 9 should be the link"
        );

        // Column 10+: " for more" - no link
        assert_eq!(
            line.link_at_column(10),
            None,
            "Column 10 should not be a link"
        );
    }

    #[test]
    fn test_unordered_list() {
        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();
        let lines = parse_markdown("- Item 1\n- Item 2\n- Item 3", &theme, None);

        // Each item should be on its own line
        assert!(lines.len() >= 3, "Should have at least 3 lines for 3 items");

        let all_text: String = lines.iter().map(|l| l.plain_text()).collect();
        assert!(all_text.contains("Item 1"), "Should contain Item 1");
        assert!(all_text.contains("Item 2"), "Should contain Item 2");
        assert!(all_text.contains("Item 3"), "Should contain Item 3");
    }

    #[test]
    fn test_paragraph_separation() {
        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();
        let lines = parse_markdown("First paragraph.\n\nSecond paragraph.", &theme, None);

        // Should have 3 lines: first para, blank line, second para
        assert_eq!(
            lines.len(),
            3,
            "Should have 3 lines (para, blank, para), got: {:?}",
            lines.iter().map(|l| l.plain_text()).collect::<Vec<_>>()
        );

        assert_eq!(lines[0].plain_text(), "First paragraph.");
        assert!(
            lines[1].spans.is_empty(),
            "Second line should be empty (paragraph break)"
        );
        assert_eq!(lines[2].plain_text(), "Second paragraph.");
    }

    #[test]
    fn test_soft_break_becomes_newline() {
        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();
        // Single newline in markdown is a soft break
        let lines = parse_markdown("Line one\nLine two", &theme, None);

        // Soft break should become a newline for better docstring/hover formatting
        assert!(
            lines.len() >= 2,
            "Soft break should create separate lines, got {} lines",
            lines.len()
        );
        let all_text: String = lines.iter().map(|l| l.plain_text()).collect();
        assert!(
            all_text.contains("one") && all_text.contains("two"),
            "Should contain both lines"
        );
    }

    #[test]
    fn test_hard_break() {
        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();
        // Two spaces before newline creates a hard break
        let lines = parse_markdown("Line one  \nLine two", &theme, None);

        // Hard break creates a new line within the same paragraph
        assert!(lines.len() >= 2, "Hard break should create multiple lines");
    }

    #[test]
    fn test_horizontal_rule() {
        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();
        let lines = parse_markdown("Above\n\n---\n\nBelow", &theme, None);

        // Should have a line with horizontal rule characters
        let has_rule = lines.iter().any(|l| l.plain_text().contains("─"));
        assert!(has_rule, "Should contain horizontal rule character");
    }

    #[test]
    fn test_nested_formatting() {
        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();
        let lines = parse_markdown("This is ***bold and italic*** text", &theme, None);

        assert_eq!(lines.len(), 1);

        // Find the nested formatted span
        let nested_span = lines[0].spans.iter().find(|s| s.text == "bold and italic");
        assert!(nested_span.is_some(), "Should have nested formatted span");

        let style = nested_span.unwrap().style;
        assert!(
            style.add_modifier.contains(Modifier::BOLD),
            "Should be bold"
        );
        assert!(
            style.add_modifier.contains(Modifier::ITALIC),
            "Should be italic"
        );
    }

    #[test]
    fn test_lsp_hover_docstring() {
        // Real-world example from Python LSP hover
        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();
        let markdown = "```python\n(class) Path\n```\n\nPurePath subclass that can make system calls.\n\nPath represents a filesystem path.";

        let lines = parse_markdown(markdown, &theme, None);

        // Should have code block, blank line, first paragraph, blank line, second paragraph
        assert!(lines.len() >= 3, "Should have multiple sections");

        // Code block should have background
        let code_line = lines.iter().find(|l| l.plain_text().contains("Path"));
        assert!(code_line.is_some(), "Should have code block with Path");

        // Documentation text should be present
        let all_text: String = lines.iter().map(|l| l.plain_text()).collect();
        assert!(
            all_text.contains("PurePath subclass"),
            "Should contain docstring"
        );
    }

    #[test]
    fn test_python_docstring_formatting() {
        // Test Python-style docstring with keyword arguments list
        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();
        let markdown = "Keyword Arguments:\n    - prog -- The name\n    - usage -- A usage message";
        let lines = parse_markdown(markdown, &theme, None);

        // Should preserve line breaks for proper list formatting
        assert!(
            lines.len() >= 3,
            "Should have multiple lines for keyword args list, got {} lines: {:?}",
            lines.len(),
            lines.iter().map(|l| l.plain_text()).collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_empty_input() {
        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();
        let lines = parse_markdown("", &theme, None);

        // Empty input should produce empty or minimal output
        assert!(
            lines.is_empty() || (lines.len() == 1 && lines[0].spans.is_empty()),
            "Empty input should produce empty output"
        );
    }

    #[test]
    fn test_only_whitespace() {
        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();
        let lines = parse_markdown("   \n\n   ", &theme, None);

        // Whitespace-only should produce empty or minimal output
        for line in &lines {
            let text = line.plain_text();
            assert!(
                text.trim().is_empty(),
                "Whitespace-only input should not produce content"
            );
        }
    }

    // ==================== Word Wrapping Tests ====================

    #[test]
    fn test_wrap_text_line_at_word_boundaries() {
        // Test that wrapping happens at word boundaries, not mid-word
        let text = "Path represents a filesystem path but unlike PurePath also offers methods";
        let wrapped = wrap_text_line(text, 30);

        // Should wrap at word boundaries
        for (i, line) in wrapped.iter().enumerate() {
            // Lines should not start with a space (spaces are trimmed when wrapping)
            if !line.is_empty() {
                assert!(
                    !line.starts_with(' '),
                    "Line {} should not start with space: {:?}",
                    i,
                    line
                );
            }

            // Each line should fit within max_width
            let line_width = unicode_width::UnicodeWidthStr::width(line.as_str());
            assert!(
                line_width <= 30,
                "Line {} exceeds max width: {} > 30, content: {:?}",
                i,
                line_width,
                line
            );
        }

        // Check that we didn't break any words mid-character
        // All words in wrapped output should be complete words from original
        let original_words: Vec<&str> = text.split_whitespace().collect();
        let wrapped_words: Vec<&str> = wrapped
            .iter()
            .flat_map(|line| line.split_whitespace())
            .collect();
        assert_eq!(
            original_words, wrapped_words,
            "Words should be preserved without breaking mid-word"
        );

        // Verify specific expected wrapping (28 chars fits: "Path represents a filesystem")
        assert_eq!(
            wrapped[0], "Path represents a filesystem",
            "First line should break at word boundary"
        );
        assert_eq!(
            wrapped[1], "path but unlike PurePath also",
            "Second line should contain next words (30 chars fits)"
        );
        assert_eq!(
            wrapped[2], "offers methods",
            "Third line should contain remaining words"
        );
    }

    #[test]
    fn test_wrap_text_line_long_word() {
        // Test that words longer than max_width are broken mid-word
        let text = "supercalifragilisticexpialidocious";
        let wrapped = wrap_text_line(text, 10);

        assert!(
            wrapped.len() > 1,
            "Long word should be split into multiple lines"
        );

        // Each line should be at most max_width
        for line in &wrapped {
            let width = unicode_width::UnicodeWidthStr::width(line.as_str());
            assert!(width <= 10, "Line should not exceed max width: {}", line);
        }

        // Content should be preserved
        let rejoined: String = wrapped.join("");
        assert_eq!(rejoined, text, "Content should be preserved");
    }

    #[test]
    fn test_wrap_text_line_empty() {
        let wrapped = wrap_text_line("", 30);
        assert_eq!(wrapped.len(), 1);
        assert_eq!(wrapped[0], "");
    }

    #[test]
    fn test_wrap_text_line_fits() {
        let text = "Short text";
        let wrapped = wrap_text_line(text, 30);
        assert_eq!(wrapped.len(), 1);
        assert_eq!(wrapped[0], text);
    }

    #[test]
    fn test_wrap_styled_lines_long_hover_content() {
        // Test that long hover lines get wrapped correctly
        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();

        // Simulate a long LSP hover response (e.g., a function signature that's too long)
        let long_text = "def very_long_function_name(param1: str, param2: int, param3: float, param4: list, param5: dict) -> tuple[str, int, float]";
        let markdown = format!("```python\n{}\n```", long_text);

        let lines = parse_markdown(&markdown, &theme, None);

        // The code block should produce styled lines
        assert!(!lines.is_empty(), "Should have parsed lines");

        // Now wrap to a narrow width (40 chars)
        let wrapped = wrap_styled_lines(&lines, 40);

        // The long line should be wrapped into multiple lines
        assert!(
            wrapped.len() > lines.len(),
            "Long line should wrap into multiple lines. Original: {}, Wrapped: {}",
            lines.len(),
            wrapped.len()
        );

        // Each wrapped line should not exceed max width
        for (i, line) in wrapped.iter().enumerate() {
            let line_width: usize = line
                .spans
                .iter()
                .map(|s| unicode_width::UnicodeWidthStr::width(s.text.as_str()))
                .sum();
            assert!(
                line_width <= 40,
                "Wrapped line {} exceeds max width: {} > 40, content: {:?}",
                i,
                line_width,
                line.spans
                    .iter()
                    .map(|s| s.text.as_str())
                    .collect::<Vec<_>>()
            );
        }

        // Verify no content is lost (spaces at wrap points are trimmed, which is expected)
        let original_text: String = lines
            .iter()
            .flat_map(|l| l.spans.iter().map(|s| s.text.as_str()))
            .collect();
        let wrapped_text: String = wrapped
            .iter()
            .map(|l| l.spans.iter().map(|s| s.text.as_str()).collect::<String>())
            .collect::<Vec<_>>()
            .join(" ");
        assert_eq!(
            original_text, wrapped_text,
            "Content should be preserved after wrapping (with spaces at line joins)"
        );
    }

    #[test]
    fn test_wrap_styled_lines_preserves_style() {
        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();
        let lines = parse_markdown("**bold text that is quite long**", &theme, None);

        let wrapped = wrap_styled_lines(&lines, 15);

        // All wrapped segments should preserve the bold style
        for line in &wrapped {
            for span in &line.spans {
                if !span.text.trim().is_empty() {
                    assert!(
                        span.style.add_modifier.contains(Modifier::BOLD),
                        "Style should be preserved after wrapping: {:?}",
                        span.text
                    );
                }
            }
        }
    }

    #[test]
    fn test_wrap_text_lines_multiple() {
        let lines = vec![
            "Short".to_string(),
            "This is a longer line that needs wrapping".to_string(),
            "".to_string(),
            "Another line".to_string(),
        ];

        let wrapped = wrap_text_lines(&lines, 20);

        // Should preserve empty lines
        assert!(
            wrapped.iter().any(|l| l.is_empty()),
            "Should preserve empty lines"
        );

        // All lines should fit within max_width
        for line in &wrapped {
            let width = unicode_width::UnicodeWidthStr::width(line.as_str());
            assert!(width <= 20, "Line exceeds max width: {}", line);
        }
    }

    #[test]
    fn test_signature_help_doc_indent_preserved() {
        // Simulate the markdown content produced by signature help for print()
        // The doc text from pyright uses blank lines between param name and description
        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();
        let content = "(*values: object, sep: str) -> None\n\n> *values\n\n---\n\nPrints the values to a stream.\n\nsep\n\n  string inserted between values, default a space.\n\nend\n\n  string appended after the last value, default a newline.";

        let lines = parse_markdown(content, &theme, None);
        let texts: Vec<String> = lines.iter().map(|l| l.plain_text()).collect();
        eprintln!("[TEST] Parsed markdown lines:");
        for (i, t) in texts.iter().enumerate() {
            eprintln!("  [{}] {:?}", i, t);
        }

        // Find the line with "string appended" - it should have leading spaces
        let desc_line = texts
            .iter()
            .find(|t| t.contains("string appended"))
            .expect("Should find 'string appended' line");
        eprintln!("[TEST] desc_line: {:?}", desc_line);

        // Now test wrapping at width 40 (narrow popup to force wrapping)
        let wrapped = wrap_styled_lines(&lines, 40);
        let wrapped_texts: Vec<String> = wrapped.iter().map(|l| l.plain_text()).collect();
        eprintln!("[TEST] Wrapped lines:");
        for (i, t) in wrapped_texts.iter().enumerate() {
            eprintln!("  [{}] {:?}", i, t);
        }

        // Find continuation of "string appended" line
        let desc_idx = wrapped_texts
            .iter()
            .position(|t| t.contains("string appended"))
            .expect("Should find 'string appended' line in wrapped output");
        assert!(
            desc_idx + 1 < wrapped_texts.len(),
            "Line should have wrapped, but didn't. Lines: {:?}",
            wrapped_texts
        );
        let continuation = &wrapped_texts[desc_idx + 1];
        eprintln!("[TEST] continuation: {:?}", continuation);

        // Continuation should have indent (spaces matching the NBSP indent)
        let orig_indent = count_leading_spaces(desc_line);
        let cont_indent = count_leading_spaces(continuation);
        eprintln!(
            "[TEST] orig_indent={}, cont_indent={}",
            orig_indent, cont_indent
        );
        assert_eq!(
            cont_indent, orig_indent,
            "Continuation line should have same indent as original"
        );
    }
}
