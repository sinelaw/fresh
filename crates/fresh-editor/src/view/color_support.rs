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

        // Windows Terminal sets WT_SESSION and supports truecolor
        if std::env::var("WT_SESSION").is_ok() {
            return ColorCapability::TrueColor;
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
        // Map to grayscale ramp (232-255, 24 shades = indices 0-23)
        // Formula maps gray 8-248 to offset 0-23, avoiding u8 overflow
        return 232 + ((gray - 8) * 23 / 240) as u8;
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
        assert!((16..=231).contains(&idx)); // Should be in color cube
                                            // Pure red should be index 196 (5*36 + 0*6 + 0 + 16)
        assert_eq!(idx, 196);
    }

    #[test]
    fn test_rgb_to_256_grayscale() {
        let idx = rgb_to_256(128, 128, 128);
        assert!(idx >= 232); // Should be in grayscale range (232-255, u8 max is 255)
    }

    #[test]
    fn test_rgb_to_256_light_gray_no_overflow() {
        // Regression test: light grays near 248 used to overflow u8 and become 0 (black)
        // These are colors used in the light theme for menus
        let idx_245 = rgb_to_256(245, 245, 245); // menu_bg
        let idx_248 = rgb_to_256(248, 248, 248); // menu_dropdown_bg

        // Light grays should map to grayscale ramp (232-255) or white (231), not black
        assert!(
            idx_245 >= 231,
            "RGB(245,245,245) should be light, got index {}",
            idx_245
        );
        assert!(
            idx_248 >= 231,
            "RGB(248,248,248) should be light, got index {}",
            idx_248
        );

        // Critical: ensure no overflow to 0 (which would display as black)
        assert_ne!(idx_245, 0, "RGB(245,245,245) overflowed to 0");
        assert_ne!(idx_248, 0, "RGB(248,248,248) overflowed to 0");
    }

    #[test]
    fn test_rgb_to_256_grayscale_never_overflows() {
        // Property test: no grayscale value should ever produce index 0-15 (ANSI colors)
        // or cause overflow. All grays should map to either:
        // - 16 (black from color cube) for very dark
        // - 231 (white from color cube) for very bright
        // - 232-255 (grayscale ramp) for mid-range
        for gray in 0..=255u8 {
            let idx = rgb_to_256(gray, gray, gray);
            assert!(
                idx == 16 || idx == 231 || (232..=255).contains(&idx),
                "Gray {} mapped to invalid index {}, expected 16, 231, or 232-255",
                gray,
                idx
            );
        }
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

    // =========================================================================
    // Issue #1239: 256-color theme reproduction tests
    // These tests document the contrast problems when themes are rendered
    // in 256-color mode. They currently demonstrate the broken behavior.
    // =========================================================================

    /// Helper: compute WCAG-like contrast ratio between two 256-color indices
    fn contrast_ratio_256(idx1: u8, idx2: u8) -> f64 {
        fn idx_to_rgb(idx: u8) -> (u8, u8, u8) {
            match idx {
                0..=15 => {
                    let ansi: [(u8, u8, u8); 16] = [
                        (0, 0, 0), (128, 0, 0), (0, 128, 0), (128, 128, 0),
                        (0, 0, 128), (128, 0, 128), (0, 128, 128), (192, 192, 192),
                        (128, 128, 128), (255, 0, 0), (0, 255, 0), (255, 255, 0),
                        (0, 0, 255), (255, 0, 255), (0, 255, 255), (255, 255, 255),
                    ];
                    ansi[idx as usize]
                }
                16..=231 => {
                    let i = idx - 16;
                    ((i / 36) * 51, ((i % 36) / 6) * 51, (i % 6) * 51)
                }
                232..=255 => {
                    let g = (idx - 232) * 10 + 8;
                    (g, g, g)
                }
            }
        }
        fn relative_luminance(r: u8, g: u8, b: u8) -> f64 {
            let r = {
                let s = r as f64 / 255.0;
                if s <= 0.03928 { s / 12.92 } else { ((s + 0.055) / 1.055).powf(2.4) }
            };
            let g = {
                let s = g as f64 / 255.0;
                if s <= 0.03928 { s / 12.92 } else { ((s + 0.055) / 1.055).powf(2.4) }
            };
            let b = {
                let s = b as f64 / 255.0;
                if s <= 0.03928 { s / 12.92 } else { ((s + 0.055) / 1.055).powf(2.4) }
            };
            0.2126 * r + 0.7152 * g + 0.0722 * b
        }
        let (r1, g1, b1) = idx_to_rgb(idx1);
        let (r2, g2, b2) = idx_to_rgb(idx2);
        let l1 = relative_luminance(r1, g1, b1);
        let l2 = relative_luminance(r2, g2, b2);
        let (lighter, darker) = if l1 > l2 { (l1, l2) } else { (l2, l1) };
        (lighter + 0.05) / (darker + 0.05)
    }

    /// Solarized-dark: editor bg and selection_bg both map to the SAME index (black).
    /// Selection is completely invisible.
    #[test]
    fn test_issue_1239_solarized_bg_and_selection_collapse() {
        let bg_idx = rgb_to_256(0, 43, 54);        // solarized bg
        let sel_idx = rgb_to_256(7, 54, 66);        // solarized selection_bg

        // CURRENT BEHAVIOR: both map to 16 (black) — selection is invisible
        // This test documents the bug. When fixed, these should map to
        // different indices.
        assert_eq!(bg_idx, sel_idx,
            "BUG (issue #1239): solarized bg and selection_bg both map to idx {}, \
             making selection invisible. Fix should make them different.", bg_idx);
    }

    /// Solarized-dark: popup text on selection bg is unreadable (contrast ~1.09)
    #[test]
    fn test_issue_1239_solarized_popup_selection_unreadable() {
        let text_idx = rgb_to_256(131, 148, 150);   // popup_text_fg
        let sel_idx = rgb_to_256(38, 139, 210);      // popup_selection_bg
        let cr = contrast_ratio_256(text_idx, sel_idx);

        // CURRENT BEHAVIOR: contrast ratio ~1.09 — practically identical
        assert!(cr < 1.5,
            "BUG (issue #1239): solarized popup selected text has contrast {:.2}, \
             expected < 1.5 (currently broken). Minimum readable is ~3.0.", cr);
    }

    /// Nord: line numbers are nearly invisible (contrast ~1.52)
    #[test]
    fn test_issue_1239_nord_line_numbers_invisible() {
        let ln_fg_idx = rgb_to_256(76, 86, 106);    // line_number_fg
        let bg_idx = rgb_to_256(46, 52, 64);         // editor bg / line_number_bg
        let cr = contrast_ratio_256(ln_fg_idx, bg_idx);

        // CURRENT BEHAVIOR: both map to very dark colors, contrast ~1.52
        assert!(cr < 2.0,
            "BUG (issue #1239): nord line numbers have contrast {:.2}, \
             expected < 2.0 (currently broken). Should be at least 3.0.", cr);
    }

    /// Nord: popup border is unreadable (contrast ~1.45)
    #[test]
    fn test_issue_1239_nord_popup_border_unreadable() {
        let border_idx = rgb_to_256(76, 86, 106);   // popup_border_fg
        let popup_bg_idx = rgb_to_256(59, 66, 82);   // popup_bg
        let cr = contrast_ratio_256(border_idx, popup_bg_idx);

        assert!(cr < 2.0,
            "BUG (issue #1239): nord popup border has contrast {:.2}, \
             expected < 2.0 (currently broken).", cr);
    }

    /// Dracula: line numbers are hard to read (contrast ~1.81)
    #[test]
    fn test_issue_1239_dracula_line_numbers_low_contrast() {
        let ln_fg_idx = rgb_to_256(98, 114, 164);   // line_number_fg
        let bg_idx = rgb_to_256(40, 42, 54);         // editor bg
        let cr = contrast_ratio_256(ln_fg_idx, bg_idx);

        assert!(cr < 2.0,
            "BUG (issue #1239): dracula line numbers have contrast {:.2}, \
             expected < 2.0 (currently broken).", cr);
    }

    /// Light theme: Ctrl+P helper text is completely unreadable (contrast ~1.03)
    #[test]
    fn test_issue_1239_light_palette_helper_unreadable() {
        // The "file | >command | :line | #buffer" text at the bottom of Ctrl+P
        // Light theme uses default fg (idx 7 = gray 192,192,192) on
        // popup highlight bg mapped to idx 152 (153,204,204)
        let cr = contrast_ratio_256(7, 152);

        assert!(cr < 1.5,
            "BUG (issue #1239): light theme palette helper text has contrast {:.2}, \
             virtually unreadable.", cr);
    }

    /// Multiple dark theme backgrounds collapse to the same black (idx 16)
    #[test]
    fn test_issue_1239_dark_backgrounds_collapse_to_black() {
        // These are all conceptually different backgrounds but map to the same index
        let solarized_bg = rgb_to_256(0, 43, 54);
        let nord_bg = rgb_to_256(46, 52, 64);
        let dracula_bg = rgb_to_256(40, 42, 54);
        let nord_sel = rgb_to_256(67, 76, 94);
        let solarized_sel = rgb_to_256(7, 54, 66);

        // All map to idx 16 or 17 (pure black or near-black)
        assert!(solarized_bg <= 17, "solarized bg={}", solarized_bg);
        assert!(nord_bg <= 17, "nord bg={}", nord_bg);
        assert!(dracula_bg <= 17, "dracula bg={}", dracula_bg);
        assert!(nord_sel <= 17, "nord sel={}", nord_sel);
        assert!(solarized_sel <= 17, "solarized sel={}", solarized_sel);
    }
}
