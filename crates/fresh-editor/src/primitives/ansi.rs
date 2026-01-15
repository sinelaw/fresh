//! ANSI escape code parser for rendering text with embedded control codes
//!
//! This module parses ANSI escape sequences from text content and converts them
//! into ratatui styles for proper rendering in the editor.

use crate::primitives::display_width::{char_width, str_width};
use ratatui::style::{Color, Modifier, Style};

/// Standard ANSI colors (codes 30-37 for fg, 40-47 for bg)
const STANDARD_COLORS: [Color; 8] = [
    Color::Black,   // 0
    Color::Red,     // 1
    Color::Green,   // 2
    Color::Yellow,  // 3
    Color::Blue,    // 4
    Color::Magenta, // 5
    Color::Cyan,    // 6
    Color::Gray,    // 7
];

/// Bright ANSI colors (codes 90-97 for fg, 100-107 for bg)
const BRIGHT_COLORS: [Color; 8] = [
    Color::DarkGray,     // 0
    Color::LightRed,     // 1
    Color::LightGreen,   // 2
    Color::LightYellow,  // 3
    Color::LightBlue,    // 4
    Color::LightMagenta, // 5
    Color::LightCyan,    // 6
    Color::White,        // 7
];

/// Result of parsing a single character or escape sequence
#[derive(Debug, Clone)]
pub enum AnsiToken {
    /// A regular character to display
    Char(char),
    /// A style change (from an escape sequence)
    StyleChange(Style),
}

/// Parser state for ANSI escape sequences
#[derive(Debug, Clone)]
pub struct AnsiParser {
    /// Current accumulated style from escape codes
    current_style: Style,
    /// Buffer for incomplete escape sequences
    escape_buffer: String,
    /// Whether we're currently inside an escape sequence
    in_escape: bool,
}

impl Default for AnsiParser {
    fn default() -> Self {
        Self::new()
    }
}

impl AnsiParser {
    pub fn new() -> Self {
        Self {
            current_style: Style::default(),
            escape_buffer: String::new(),
            in_escape: false,
        }
    }

    /// Get the current style accumulated from parsed escape sequences
    pub fn current_style(&self) -> Style {
        self.current_style
    }

    /// Reset the parser state
    pub fn reset(&mut self) {
        self.current_style = Style::default();
        self.escape_buffer.clear();
        self.in_escape = false;
    }

    /// Parse a single character, returning what to do with it
    ///
    /// Returns None if the character is part of an escape sequence and should not be displayed,
    /// or Some(style) if the character should be displayed with the given style.
    pub fn parse_char(&mut self, ch: char) -> Option<Style> {
        if self.in_escape {
            self.escape_buffer.push(ch);

            // Check if escape sequence is complete
            if self.is_escape_complete() {
                self.process_escape_sequence();
                self.escape_buffer.clear();
                self.in_escape = false;
            }
            // Don't display escape sequence characters
            None
        } else if ch == '\x1b' {
            // Start of escape sequence
            self.in_escape = true;
            self.escape_buffer.clear();
            self.escape_buffer.push(ch);
            None
        } else {
            // Regular character - return current style
            Some(self.current_style)
        }
    }

    /// Check if the escape sequence in the buffer is complete
    fn is_escape_complete(&self) -> bool {
        if self.escape_buffer.len() < 2 {
            return false;
        }

        // Check for CSI sequences (ESC [)
        if self.escape_buffer.starts_with("\x1b[") {
            // CSI sequences end with a letter (0x40-0x7E)
            if let Some(last) = self.escape_buffer.chars().last() {
                return last.is_ascii_alphabetic();
            }
        }

        // Check for OSC sequences (ESC ])
        if self.escape_buffer.starts_with("\x1b]") {
            // OSC sequences end with BEL (\x07) or ST (ESC \)
            return self.escape_buffer.ends_with('\x07') || self.escape_buffer.ends_with("\x1b\\");
        }

        // Simple two-character sequences (ESC followed by single char)
        if self.escape_buffer.len() == 2 {
            let second = self.escape_buffer.chars().nth(1).unwrap();
            // Not a CSI or OSC start
            return second != '[' && second != ']';
        }

        // For safety, limit buffer size
        self.escape_buffer.len() > 32
    }

    /// Process the completed escape sequence and update current_style
    fn process_escape_sequence(&mut self) {
        // Only handle CSI SGR (Select Graphic Rendition) sequences
        if !self.escape_buffer.starts_with("\x1b[") {
            return;
        }

        // Check if this is an SGR sequence (ends with 'm')
        if !self.escape_buffer.ends_with('m') {
            return;
        }

        // Extract the parameters (between [ and the final letter)
        let params_end = self.escape_buffer.len() - 1;
        let params_str = self.escape_buffer[2..params_end].to_string();

        // Parse SGR parameters
        self.parse_sgr_params(&params_str);
    }

    /// Parse SGR (Select Graphic Rendition) parameters
    fn parse_sgr_params(&mut self, params_str: &str) {
        if params_str.is_empty() {
            // ESC[m is equivalent to ESC[0m (reset)
            self.current_style = Style::default();
            return;
        }

        let params: Vec<u8> = params_str
            .split(';')
            .filter_map(|s| s.parse().ok())
            .collect();

        let mut i = 0;
        while i < params.len() {
            let code = params[i];
            match code {
                // Reset
                0 => self.current_style = Style::default(),

                // Text attributes (add)
                1 => self.current_style = self.current_style.add_modifier(Modifier::BOLD),
                2 => self.current_style = self.current_style.add_modifier(Modifier::DIM),
                3 => self.current_style = self.current_style.add_modifier(Modifier::ITALIC),
                4 => self.current_style = self.current_style.add_modifier(Modifier::UNDERLINED),
                5 => self.current_style = self.current_style.add_modifier(Modifier::SLOW_BLINK),
                7 => self.current_style = self.current_style.add_modifier(Modifier::REVERSED),
                8 => self.current_style = self.current_style.add_modifier(Modifier::HIDDEN),
                9 => self.current_style = self.current_style.add_modifier(Modifier::CROSSED_OUT),

                // Text attributes (remove)
                21 => self.current_style = self.current_style.remove_modifier(Modifier::BOLD),
                22 => {
                    self.current_style = self
                        .current_style
                        .remove_modifier(Modifier::BOLD)
                        .remove_modifier(Modifier::DIM)
                }
                23 => self.current_style = self.current_style.remove_modifier(Modifier::ITALIC),
                24 => self.current_style = self.current_style.remove_modifier(Modifier::UNDERLINED),
                25 => self.current_style = self.current_style.remove_modifier(Modifier::SLOW_BLINK),
                27 => self.current_style = self.current_style.remove_modifier(Modifier::REVERSED),
                28 => self.current_style = self.current_style.remove_modifier(Modifier::HIDDEN),
                29 => {
                    self.current_style = self.current_style.remove_modifier(Modifier::CROSSED_OUT)
                }

                // Standard foreground colors (30-37)
                30..=37 => {
                    self.current_style =
                        self.current_style.fg(STANDARD_COLORS[(code - 30) as usize])
                }

                // Extended foreground color (38;5;n or 38;2;r;g;b)
                38 => i += Self::parse_extended_color(&params[i..], &mut self.current_style, true),

                // Default foreground
                39 => self.current_style = self.current_style.fg(Color::Reset),

                // Standard background colors (40-47)
                40..=47 => {
                    self.current_style =
                        self.current_style.bg(STANDARD_COLORS[(code - 40) as usize])
                }

                // Extended background color (48;5;n or 48;2;r;g;b)
                48 => i += Self::parse_extended_color(&params[i..], &mut self.current_style, false),

                // Default background
                49 => self.current_style = self.current_style.bg(Color::Reset),

                // Bright foreground colors (90-97)
                90..=97 => {
                    self.current_style = self.current_style.fg(BRIGHT_COLORS[(code - 90) as usize])
                }

                // Bright background colors (100-107)
                100..=107 => {
                    self.current_style = self.current_style.bg(BRIGHT_COLORS[(code - 100) as usize])
                }

                _ => {} // Ignore unknown codes
            }
            i += 1;
        }
    }

    /// Parse extended color sequences (256-color or RGB)
    /// Returns the number of additional parameters consumed
    fn parse_extended_color(params: &[u8], style: &mut Style, is_foreground: bool) -> usize {
        if params.len() < 2 {
            return 0;
        }

        match params[1] {
            // 256-color mode: code;5;n
            5 if params.len() >= 3 => {
                let color = Color::Indexed(params[2]);
                *style = if is_foreground {
                    style.fg(color)
                } else {
                    style.bg(color)
                };
                2
            }
            // RGB mode: code;2;r;g;b
            2 if params.len() >= 5 => {
                let color = Color::Rgb(params[2], params[3], params[4]);
                *style = if is_foreground {
                    style.fg(color)
                } else {
                    style.bg(color)
                };
                4
            }
            _ => 0,
        }
    }
}

/// Check if a line contains ANSI escape sequences
pub fn contains_ansi_codes(text: &str) -> bool {
    text.contains('\x1b')
}

/// Strip all ANSI escape codes from a string, returning just the text
pub fn strip_ansi_codes(text: &str) -> String {
    if !contains_ansi_codes(text) {
        return text.to_string();
    }

    let mut result = String::with_capacity(text.len());
    let mut parser = AnsiParser::new();

    for ch in text.chars() {
        if parser.parse_char(ch).is_some() {
            result.push(ch);
        }
    }

    result
}

/// Count the visual width of visible characters in a string, excluding ANSI escape sequences
/// This is useful for calculating visual width for line wrapping
/// Returns the total display width (e.g., CJK characters count as 2, ASCII as 1)
pub fn visible_char_count(text: &str) -> usize {
    if !contains_ansi_codes(text) {
        return str_width(text);
    }

    let mut width = 0;
    let mut parser = AnsiParser::new();

    for ch in text.chars() {
        if parser.parse_char(ch).is_some() {
            width += char_width(ch);
        }
    }

    width
}

/// Parse a string with ANSI codes and return segments with their styles
///
/// Returns a vector of (text, style) pairs representing the parsed content.
pub fn parse_ansi_string(text: &str) -> Vec<(String, Style)> {
    if !contains_ansi_codes(text) {
        return vec![(text.to_string(), Style::default())];
    }

    let mut result = Vec::new();
    let mut parser = AnsiParser::new();
    let mut current_text = String::new();
    let mut current_style = Style::default();

    for ch in text.chars() {
        match parser.parse_char(ch) {
            Some(style) => {
                // Check if style changed
                if style != current_style && !current_text.is_empty() {
                    result.push((current_text, current_style));
                    current_text = String::new();
                }
                current_style = style;
                current_text.push(ch);
            }
            None => {
                // Part of escape sequence, skip
            }
        }
    }

    // Push remaining text
    if !current_text.is_empty() {
        result.push((current_text, current_style));
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_no_ansi() {
        let text = "Hello, world!";
        assert!(!contains_ansi_codes(text));
        assert_eq!(strip_ansi_codes(text), text);
    }

    #[test]
    fn test_strip_simple_color() {
        let text = "\x1b[31mRed\x1b[0m";
        assert!(contains_ansi_codes(text));
        assert_eq!(strip_ansi_codes(text), "Red");
    }

    #[test]
    fn test_strip_multiple_colors() {
        let text = "\x1b[31mRed\x1b[32mGreen\x1b[0m";
        assert_eq!(strip_ansi_codes(text), "RedGreen");
    }

    #[test]
    fn test_parse_red_text() {
        let text = "\x1b[31mRed\x1b[0m";
        let segments = parse_ansi_string(text);

        assert_eq!(segments.len(), 1);
        assert_eq!(segments[0].0, "Red");
        assert_eq!(segments[0].1.fg, Some(Color::Red));
    }

    #[test]
    fn test_parse_multiple_colors() {
        let text = "\x1b[31mRed\x1b[32mGreen\x1b[0mNormal";
        let segments = parse_ansi_string(text);

        assert_eq!(segments.len(), 3);
        assert_eq!(segments[0].0, "Red");
        assert_eq!(segments[0].1.fg, Some(Color::Red));
        assert_eq!(segments[1].0, "Green");
        assert_eq!(segments[1].1.fg, Some(Color::Green));
        assert_eq!(segments[2].0, "Normal");
        assert_eq!(segments[2].1.fg, None);
    }

    #[test]
    fn test_parse_bold() {
        let text = "\x1b[1mBold\x1b[0m";
        let segments = parse_ansi_string(text);

        assert_eq!(segments.len(), 1);
        assert_eq!(segments[0].0, "Bold");
        assert!(segments[0].1.add_modifier.contains(Modifier::BOLD));
    }

    #[test]
    fn test_parse_256_color() {
        let text = "\x1b[38;5;196mRed256\x1b[0m";
        let segments = parse_ansi_string(text);

        assert_eq!(segments.len(), 1);
        assert_eq!(segments[0].0, "Red256");
        assert_eq!(segments[0].1.fg, Some(Color::Indexed(196)));
    }

    #[test]
    fn test_parse_rgb_color() {
        let text = "\x1b[38;2;255;128;0mOrange\x1b[0m";
        let segments = parse_ansi_string(text);

        assert_eq!(segments.len(), 1);
        assert_eq!(segments[0].0, "Orange");
        assert_eq!(segments[0].1.fg, Some(Color::Rgb(255, 128, 0)));
    }

    #[test]
    fn test_bright_colors() {
        let text = "\x1b[91mBrightRed\x1b[0m";
        let segments = parse_ansi_string(text);

        assert_eq!(segments.len(), 1);
        assert_eq!(segments[0].0, "BrightRed");
        assert_eq!(segments[0].1.fg, Some(Color::LightRed));
    }

    #[test]
    fn test_combined_attributes() {
        let text = "\x1b[1;31;4mBoldRedUnderline\x1b[0m";
        let segments = parse_ansi_string(text);

        assert_eq!(segments.len(), 1);
        assert_eq!(segments[0].0, "BoldRedUnderline");
        assert_eq!(segments[0].1.fg, Some(Color::Red));
        assert!(segments[0].1.add_modifier.contains(Modifier::BOLD));
        assert!(segments[0].1.add_modifier.contains(Modifier::UNDERLINED));
    }

    #[test]
    fn test_background_color() {
        let text = "\x1b[44mBlueBackground\x1b[0m";
        let segments = parse_ansi_string(text);

        assert_eq!(segments.len(), 1);
        assert_eq!(segments[0].0, "BlueBackground");
        assert_eq!(segments[0].1.bg, Some(Color::Blue));
    }

    #[test]
    fn test_mixed_content() {
        let text = "Normal \x1b[31mRed\x1b[0m Normal";
        let segments = parse_ansi_string(text);

        assert_eq!(segments.len(), 3);
        assert_eq!(segments[0].0, "Normal ");
        assert_eq!(segments[1].0, "Red");
        assert_eq!(segments[2].0, " Normal");
    }
}
