//! Terminal color capability detection and color conversion
//!
//! This module handles detecting the terminal's color support level and
//! converting RGB colors to the nearest approximation for limited color terminals.
//!
//! # Usage
//!
//! Detect capability at startup and pass it to the Editor:
//! ```ignore
//! let capability = ColorCapability::detect();
//! let editor = Editor::new(config, width, height, dir_context, capability)?;
//! ```
//!
//! The Editor will automatically convert colors during rendering based on the capability.

use ratatui::style::Color;

/// Terminal color capability levels
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ColorCapability {
    /// Full 24-bit RGB color support (16 million colors)
    TrueColor,
    /// 256 color palette (xterm-256color)
    Color256,
    /// Basic 16 color palette (standard ANSI colors)
    Color16,
}

impl ColorCapability {
    /// Detect the terminal's color capability
    /// Can be overridden with FRESH_COLOR_MODE env var: "truecolor", "256", or "16"
    pub fn detect() -> Self {
        // Check for manual override first
        if let Ok(mode) = std::env::var("FRESH_COLOR_MODE") {
            match mode.to_lowercase().as_str() {
                "truecolor" | "24bit" | "true" => return ColorCapability::TrueColor,
                "256" | "256color" => return ColorCapability::Color256,
                "16" | "basic" | "ansi" => return ColorCapability::Color16,
                _ => {} // Fall through to auto-detection
            }
        }

        // Check TERM first for multiplexers that don't support truecolor
        // (they may pass through COLORTERM from the outer terminal)
        if let Ok(term) = std::env::var("TERM") {
            let t = term.to_lowercase();

            // GNU Screen doesn't support truecolor - cap at 256
            if t.starts_with("screen") {
                return ColorCapability::Color256;
            }

            // tmux usually supports 256, some configs support truecolor
            if t.starts_with("tmux") {
                if t.contains("direct") {
                    return ColorCapability::TrueColor;
                }
                // Check COLORTERM - tmux can pass through truecolor if configured
                if let Ok(colorterm) = std::env::var("COLORTERM") {
                    let ct = colorterm.to_lowercase();
                    if ct == "truecolor" || ct == "24bit" {
                        return ColorCapability::TrueColor;
                    }
                }
                return ColorCapability::Color256;
            }
        }

        // Check COLORTERM - reliable for truecolor (but not inside Screen/tmux)
        if let Ok(colorterm) = std::env::var("COLORTERM") {
            let ct = colorterm.to_lowercase();
            if ct == "truecolor" || ct == "24bit" {
                return ColorCapability::TrueColor;
            }
        }

        // Check TERM for other indicators
        if let Ok(term) = std::env::var("TERM") {
            let t = term.to_lowercase();

            // Check for truecolor indicators
            if t.contains("truecolor") || t.contains("24bit") || t.contains("direct") {
                return ColorCapability::TrueColor;
            }

            // Check for 256color
            if t.contains("256color") || t.contains("256-color") {
                return ColorCapability::Color256;
            }

            // Modern terminals that support truecolor
            if t.contains("kitty")
                || t.contains("alacritty")
                || t.contains("iterm")
                || t.contains("vte")
                || t.contains("konsole")
                || t.contains("gnome")
                || t.contains("wezterm")
            {
                return ColorCapability::TrueColor;
            }

            // xterm usually supports 256
            if t.starts_with("xterm") {
                return ColorCapability::Color256;
            }

            // Linux console, dumb terminal - basic colors only
            if t == "linux" || t == "cons25" || t == "dumb" {
                return ColorCapability::Color16;
            }
        }

        // Default to 256 as safe middle ground
        ColorCapability::Color256
    }
}

/// Convert an RGB color to the nearest 256-color palette index
///
/// The 256-color palette consists of:
/// - 0-15: Standard ANSI colors (basic 16 colors)
/// - 16-231: 6x6x6 color cube (216 colors)
/// - 232-255: Grayscale ramp (24 shades)
fn rgb_to_256(r: u8, g: u8, b: u8) -> u8 {
    // Check if it's close to grayscale
    let gray_threshold = 8;
    if r.abs_diff(g) < gray_threshold && g.abs_diff(b) < gray_threshold {
        // Use grayscale ramp (232-255)
        let gray = (r as u16 + g as u16 + b as u16) / 3;
        if gray < 8 {
            return 16; // Use black from color cube
        }
        if gray > 248 {
            return 231; // Use white from color cube
        }
        // Map to grayscale ramp (232-255, 24 shades)
        // Each step is ~10.625 units
        return 232 + ((gray - 8) * 24 / 240) as u8;
    }

    // Map to 6x6x6 color cube (indices 16-231)
    // Each RGB component maps to 0-5
    let r_idx = if r < 48 {
        0
    } else {
        ((r as u16 - 35) * 5 / 220).min(5) as u8
    };
    let g_idx = if g < 48 {
        0
    } else {
        ((g as u16 - 35) * 5 / 220).min(5) as u8
    };
    let b_idx = if b < 48 {
        0
    } else {
        ((b as u16 - 35) * 5 / 220).min(5) as u8
    };

    16 + 36 * r_idx + 6 * g_idx + b_idx
}

/// Convert an RGB color to the nearest basic 16 ANSI color
fn rgb_to_16(r: u8, g: u8, b: u8) -> Color {
    // Calculate luminance and saturation to help with color matching
    let max = r.max(g).max(b);
    let min = r.min(g).min(b);
    let lum = (max as u16 + min as u16) / 2;
    let is_bright = lum > 127;

    // Check for grayscale
    let gray_threshold = 30;
    if max - min < gray_threshold {
        return if lum < 64 {
            Color::Black
        } else if lum < 128 {
            Color::DarkGray
        } else if lum < 192 {
            Color::Gray
        } else {
            Color::White
        };
    }

    // Determine dominant color(s)
    let r_dom = r >= g && r >= b;
    let g_dom = g >= r && g >= b;
    let b_dom = b >= r && b >= g;

    // Determine if secondary colors are significant
    let threshold = max / 2;
    let r_sig = r > threshold;
    let g_sig = g > threshold;
    let b_sig = b > threshold;

    // Map to ANSI colors
    match (r_dom, g_dom, b_dom, r_sig, g_sig, b_sig) {
        // Pure colors
        (true, false, false, true, false, false) => {
            if is_bright {
                Color::LightRed
            } else {
                Color::Red
            }
        }
        (false, true, false, false, true, false) => {
            if is_bright {
                Color::LightGreen
            } else {
                Color::Green
            }
        }
        (false, false, true, false, false, true) => {
            if is_bright {
                Color::LightBlue
            } else {
                Color::Blue
            }
        }

        // Yellow (red + green)
        (_, _, false, true, true, false) => {
            if is_bright {
                Color::LightYellow
            } else {
                Color::Yellow
            }
        }

        // Cyan (green + blue)
        (false, _, _, false, true, true) => {
            if is_bright {
                Color::LightCyan
            } else {
                Color::Cyan
            }
        }

        // Magenta (red + blue)
        (_, false, _, true, false, true) => {
            if is_bright {
                Color::LightMagenta
            } else {
                Color::Magenta
            }
        }

        // White-ish (all colors significant)
        (_, _, _, true, true, true) => {
            if is_bright {
                Color::White
            } else {
                Color::Gray
            }
        }

        // Fallback
        _ => {
            if is_bright {
                Color::White
            } else {
                Color::DarkGray
            }
        }
    }
}

/// Convert a Color to the appropriate format for the terminal's capability
pub fn convert_color(color: Color, capability: ColorCapability) -> Color {
    match capability {
        ColorCapability::TrueColor => color, // No conversion needed
        ColorCapability::Color256 => match color {
            Color::Rgb(r, g, b) => Color::Indexed(rgb_to_256(r, g, b)),
            _ => color, // Named colors work in 256-color mode
        },
        ColorCapability::Color16 => match color {
            Color::Rgb(r, g, b) => rgb_to_16(r, g, b),
            Color::Indexed(idx) => indexed_to_16(idx),
            _ => color, // Named colors are already 16-color compatible
        },
    }
}

/// Convert a 256-color index to the nearest 16 color
fn indexed_to_16(idx: u8) -> Color {
    match idx {
        // Standard colors (0-7)
        0 => Color::Black,
        1 => Color::Red,
        2 => Color::Green,
        3 => Color::Yellow,
        4 => Color::Blue,
        5 => Color::Magenta,
        6 => Color::Cyan,
        7 => Color::Gray,
        // Bright colors (8-15)
        8 => Color::DarkGray,
        9 => Color::LightRed,
        10 => Color::LightGreen,
        11 => Color::LightYellow,
        12 => Color::LightBlue,
        13 => Color::LightMagenta,
        14 => Color::LightCyan,
        15 => Color::White,
        // Color cube (16-231) - convert back to RGB then to 16
        16..=231 => {
            let idx = idx - 16;
            let r = (idx / 36) * 51;
            let g = ((idx % 36) / 6) * 51;
            let b = (idx % 6) * 51;
            rgb_to_16(r, g, b)
        }
        // Grayscale (232-255)
        232..=255 => {
            let gray = (idx - 232) * 10 + 8;
            if gray < 64 {
                Color::Black
            } else if gray < 128 {
                Color::DarkGray
            } else if gray < 192 {
                Color::Gray
            } else {
                Color::White
            }
        }
    }
}

/// Convert all colors in a ratatui Buffer for the given color capability
/// This is the main entry point - call once after all widgets have rendered
pub fn convert_buffer_colors(buffer: &mut ratatui::buffer::Buffer, capability: ColorCapability) {
    // For true color terminals, no conversion needed
    if capability == ColorCapability::TrueColor {
        return;
    }

    // Iterate through all cells and convert colors
    for cell in buffer.content.iter_mut() {
        cell.fg = convert_color(cell.fg, capability);
        cell.bg = convert_color(cell.bg, capability);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rgb_to_256_black() {
        assert_eq!(rgb_to_256(0, 0, 0), 16); // Should map to black in color cube
    }

    #[test]
    fn test_rgb_to_256_white() {
        assert_eq!(rgb_to_256(255, 255, 255), 231); // Should map to white in color cube
    }

    #[test]
    fn test_rgb_to_256_red() {
        let idx = rgb_to_256(255, 0, 0);
        assert!(idx >= 16 && idx <= 231); // Should be in color cube
                                          // Pure red should be index 196 (5*36 + 0*6 + 0 + 16)
        assert_eq!(idx, 196);
    }

    #[test]
    fn test_rgb_to_256_grayscale() {
        let idx = rgb_to_256(128, 128, 128);
        assert!(idx >= 232); // Should be in grayscale range (232-255, u8 max is 255)
    }

    #[test]
    fn test_rgb_to_16_basic_colors() {
        // Pure red
        assert!(matches!(rgb_to_16(255, 0, 0), Color::LightRed | Color::Red));
        // Pure green
        assert!(matches!(
            rgb_to_16(0, 255, 0),
            Color::LightGreen | Color::Green
        ));
        // Pure blue
        assert!(matches!(
            rgb_to_16(0, 0, 255),
            Color::LightBlue | Color::Blue
        ));
        // Black
        assert_eq!(rgb_to_16(0, 0, 0), Color::Black);
        // White
        assert_eq!(rgb_to_16(255, 255, 255), Color::White);
    }

    #[test]
    fn test_convert_color_truecolor() {
        let color = Color::Rgb(100, 150, 200);
        let converted = convert_color(color, ColorCapability::TrueColor);
        assert_eq!(converted, color);
    }

    #[test]
    fn test_convert_color_256() {
        let color = Color::Rgb(100, 150, 200);
        let converted = convert_color(color, ColorCapability::Color256);
        assert!(matches!(converted, Color::Indexed(_)));
    }

    #[test]
    fn test_convert_color_16() {
        let color = Color::Rgb(100, 150, 200);
        let converted = convert_color(color, ColorCapability::Color16);
        // Should be a named color, not RGB or Indexed
        assert!(!matches!(converted, Color::Rgb(_, _, _)));
        assert!(!matches!(converted, Color::Indexed(_)));
    }
}
