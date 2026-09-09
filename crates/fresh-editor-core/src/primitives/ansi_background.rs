use crate::primitives::ansi::AnsiParser;
use ratatui::style::Color;
use std::path::Path;

/// Default blend factor used to fade the background under text
pub const DEFAULT_BACKGROUND_FADE: f32 = 0.22;

/// Parsed ANSI art that can be sampled as a background
pub struct AnsiBackground {
    width: usize,
    height: usize,
    /// Row-major map of colors (None = transparent)
    colors: Vec<Option<Color>>,
}

impl AnsiBackground {
    /// Load ANSI art from a file on disk
    pub fn from_file(path: &Path) -> std::io::Result<Self> {
        let contents = std::fs::read_to_string(path)?;
        Ok(Self::from_text(&contents))
    }

    /// Parse ANSI art from a string
    pub fn from_text(text: &str) -> Self {
        let mut parser = AnsiParser::new();
        let mut rows: Vec<Vec<Option<Color>>> = Vec::new();
        let mut max_width = 0usize;

        for line in text.lines() {
            let mut row: Vec<Option<Color>> = Vec::new();

            for ch in line.chars() {
                if let Some(style) = parser.parse_char(ch) {
                    // Prefer explicit foreground color, fall back to background
                    let color = style
                        .fg
                        .or(style.bg)
                        .and_then(color_to_rgb)
                        .map(|(r, g, b)| Color::Rgb(r, g, b));
                    row.push(color);
                }
            }

            max_width = max_width.max(row.len());
            rows.push(row);
        }

        let width = max_width;
        let height = rows.len();

        // Normalize rows to consistent width so we can index quickly
        let mut colors = Vec::with_capacity(width * height);
        for row in rows {
            let mut padded = row;
            padded.resize(width, None);
            colors.extend(padded);
        }

        Self {
            width,
            height,
            colors,
        }
    }

    /// Get a faded background color for the given coordinate, wrapping if necessary
    pub fn faded_color(&self, x: usize, y: usize, base_bg: Color, opacity: f32) -> Option<Color> {
        if self.width == 0 || self.height == 0 {
            return None;
        }

        let wrapped_x = x % self.width;
        let wrapped_y = y % self.height;
        let idx = wrapped_y * self.width + wrapped_x;

        let fg_color = self.colors.get(idx).cloned().flatten()?;
        let fg_rgb = color_to_rgb(fg_color)?;
        let bg_rgb = color_to_rgb(base_bg)?;

        Some(Color::Rgb(
            blend_channel(fg_rgb.0, bg_rgb.0, opacity),
            blend_channel(fg_rgb.1, bg_rgb.1, opacity),
            blend_channel(fg_rgb.2, bg_rgb.2, opacity),
        ))
    }
}

fn blend_channel(fg: u8, bg: u8, opacity: f32) -> u8 {
    let fg_f = fg as f32;
    let bg_f = bg as f32;
    ((fg_f * opacity) + (bg_f * (1.0 - opacity)))
        .round()
        .clamp(0.0, 255.0) as u8
}

fn color_to_rgb(color: Color) -> Option<(u8, u8, u8)> {
    match color {
        Color::Rgb(r, g, b) => Some((r, g, b)),
        Color::Black => Some((0, 0, 0)),
        Color::Red => Some((205, 0, 0)),
        Color::Green => Some((0, 205, 0)),
        Color::Yellow => Some((205, 205, 0)),
        Color::Blue => Some((0, 0, 238)),
        Color::Magenta => Some((205, 0, 205)),
        Color::Cyan => Some((0, 205, 205)),
        Color::Gray => Some((229, 229, 229)),
        Color::DarkGray => Some((127, 127, 127)),
        Color::LightRed => Some((255, 0, 0)),
        Color::LightGreen => Some((0, 255, 0)),
        Color::LightYellow => Some((255, 255, 0)),
        Color::LightBlue => Some((92, 92, 255)),
        Color::LightMagenta => Some((255, 0, 255)),
        Color::LightCyan => Some((0, 255, 255)),
        Color::White => Some((255, 255, 255)),
        Color::Indexed(idx) => indexed_to_rgb(idx),
        Color::Reset => None,
    }
}

fn indexed_to_rgb(idx: u8) -> Option<(u8, u8, u8)> {
    // 0-15 = ANSI 16-color palette, 16-231 = 6x6x6 cube, 232-255 = grayscale
    match idx {
        0 => Some((0, 0, 0)),
        1 => Some((205, 0, 0)),
        2 => Some((0, 205, 0)),
        3 => Some((205, 205, 0)),
        4 => Some((0, 0, 238)),
        5 => Some((205, 0, 205)),
        6 => Some((0, 205, 205)),
        7 => Some((229, 229, 229)),
        8 => Some((127, 127, 127)),
        9 => Some((255, 0, 0)),
        10 => Some((0, 255, 0)),
        11 => Some((255, 255, 0)),
        12 => Some((92, 92, 255)),
        13 => Some((255, 0, 255)),
        14 => Some((0, 255, 255)),
        15 => Some((255, 255, 255)),
        16..=231 => {
            let i = idx - 16;
            let r = (i / 36) % 6;
            let g = (i / 6) % 6;
            let b = i % 6;
            Some((to_6cube(r), to_6cube(g), to_6cube(b)))
        }
        232..=255 => {
            let shade = (idx - 232) * 10 + 8;
            Some((shade, shade, shade))
        }
    }
}

fn to_6cube(idx: u8) -> u8 {
    [0, 95, 135, 175, 215, 255][idx as usize]
}
