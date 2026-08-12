//! Dimming effects for modal dialogs
//!
//! Provides utilities to dim areas of the frame buffer to indicate
//! that focus is on a modal dialog layer above the dimmed content.

use ratatui::layout::Rect;
use ratatui::style::{Color, Modifier};
use ratatui::Frame;

/// Dims a color by reducing its brightness by ~60%
///
/// Returns `None` for `Color::Reset`, whose on-screen appearance belongs to the
/// terminal: the user's default background is as likely to be light as dark, so
/// there is no honest RGB value to substitute. Replacing it with a fixed dark
/// gray is what made modals look like they had their own theme regardless of
/// the configured one, and it also destroyed the `Reset` that the `terminal`
/// theme relies on (issue #2982). Those cells are dimmed with `Modifier::DIM`
/// instead — see `apply_dimming_excluding`.
fn dim_color(color: Color) -> Option<Color> {
    Some(match color {
        Color::Reset => return None,
        Color::Rgb(r, g, b) => Color::Rgb(r / 3, g / 3, b / 3),
        Color::Indexed(idx) => {
            if idx == 0 {
                Color::Rgb(10, 10, 10)
            } else {
                Color::Rgb(40, 40, 40)
            }
        }
        Color::Black => Color::Rgb(10, 10, 10),
        Color::White => Color::Rgb(85, 85, 85),
        Color::Red => Color::Rgb(60, 20, 20),
        Color::Green => Color::Rgb(20, 60, 20),
        Color::Yellow => Color::Rgb(60, 60, 20),
        Color::Blue => Color::Rgb(20, 20, 60),
        Color::Magenta => Color::Rgb(60, 20, 60),
        Color::Cyan => Color::Rgb(20, 60, 60),
        Color::Gray => Color::Rgb(40, 40, 40),
        Color::DarkGray => Color::Rgb(20, 20, 20),
        Color::LightRed => Color::Rgb(80, 30, 30),
        Color::LightGreen => Color::Rgb(30, 80, 30),
        Color::LightYellow => Color::Rgb(80, 80, 30),
        Color::LightBlue => Color::Rgb(30, 30, 80),
        Color::LightMagenta => Color::Rgb(80, 30, 80),
        Color::LightCyan => Color::Rgb(30, 80, 80),
    })
}

/// Apply dimming effect to all cells in an area
pub fn apply_dimming(frame: &mut Frame, area: Rect) {
    apply_dimming_excluding(frame, area, None);
}

/// Apply dimming effect to an area, optionally excluding a sub-area
pub fn apply_dimming_excluding(frame: &mut Frame, area: Rect, exclude: Option<Rect>) {
    dim_buffer(frame.buffer_mut(), area, exclude);
}

/// Buffer-level dimming, split out from the `Frame` wrapper so it can be
/// exercised directly by tests.
fn dim_buffer(buf: &mut ratatui::buffer::Buffer, area: Rect, exclude: Option<Rect>) {
    for y in area.y..area.y.saturating_add(area.height) {
        for x in area.x..area.x.saturating_add(area.width) {
            // Skip cells inside the excluded area (if any)
            if let Some(ex) = exclude {
                if x >= ex.x && x < ex.x + ex.width && y >= ex.y && y < ex.y + ex.height {
                    continue;
                }
            }

            if let Some(cell) = buf.cell_mut((x, y)) {
                if let Some(bg) = dim_color(cell.bg) {
                    cell.bg = bg;
                }
                match dim_color(cell.fg) {
                    Some(fg) => cell.fg = fg,
                    // Terminal default foreground: let the terminal dim its own
                    // color rather than inventing one that could come out
                    // brighter than what it replaced.
                    None => cell.modifier |= Modifier::DIM,
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::view::color_support::{convert_buffer_colors, ColorCapability};
    use ratatui::buffer::Buffer;

    /// Build a 4x2 buffer with every cell set to `fg` on `bg`.
    fn buffer_of(fg: Color, bg: Color) -> Buffer {
        let mut buffer = Buffer::empty(Rect::new(0, 0, 4, 2));
        for cell in buffer.content.iter_mut() {
            cell.fg = fg;
            cell.bg = bg;
        }
        buffer
    }

    /// Issue #2982: under the `terminal` theme every color is `Color::Reset`,
    /// so a modal backdrop must stay on the terminal's own colors. Substituting
    /// a fixed dark gray gave modals a theme of their own, and inverted on a
    /// light terminal background.
    #[test]
    fn terminal_default_colors_survive_dimming() {
        let mut buffer = buffer_of(Color::Reset, Color::Reset);
        let area = buffer.area;

        dim_buffer(&mut buffer, area, None);

        for cell in buffer.content.iter() {
            assert_eq!(
                cell.bg,
                Color::Reset,
                "terminal default background must be left to the terminal"
            );
            assert_eq!(
                cell.fg,
                Color::Reset,
                "terminal default foreground must be left to the terminal"
            );
            assert!(
                cell.modifier.contains(Modifier::DIM),
                "a cell we cannot dim numerically must be dimmed by the terminal"
            );
        }
    }

    /// The other half of issue #2982: because dimming used to replace `Reset`
    /// with RGB, the 256-color contrast pass could no longer skip those cells
    /// and ran its palette search over the whole screen every frame. Preserving
    /// `Reset` restores the early-out.
    #[test]
    fn dimmed_terminal_theme_cells_stay_transparent_to_contrast_enforcement() {
        let mut buffer = buffer_of(Color::Reset, Color::Reset);
        let area = buffer.area;

        dim_buffer(&mut buffer, area, None);
        convert_buffer_colors(&mut buffer, ColorCapability::Color256);

        for cell in buffer.content.iter() {
            assert_eq!(cell.fg, Color::Reset);
            assert_eq!(cell.bg, Color::Reset);
        }
    }

    /// Themes that define real colors must dim exactly as before — this fix is
    /// only about colors we cannot resolve.
    #[test]
    fn rgb_colors_still_dim_numerically() {
        let mut buffer = buffer_of(Color::Rgb(212, 212, 212), Color::Rgb(30, 30, 30));
        let area = buffer.area;

        dim_buffer(&mut buffer, area, None);

        for cell in buffer.content.iter() {
            assert_eq!(cell.fg, Color::Rgb(70, 70, 70));
            assert_eq!(cell.bg, Color::Rgb(10, 10, 10));
            assert!(
                !cell.modifier.contains(Modifier::DIM),
                "colors we dimmed ourselves need no help from the terminal"
            );
        }
    }

    /// A `Reset` foreground over a themed background keeps its own handling:
    /// the background still dims, the foreground defers to the terminal.
    #[test]
    fn mixed_default_and_themed_colors_are_handled_independently() {
        let mut buffer = buffer_of(Color::Reset, Color::Rgb(30, 30, 30));
        let area = buffer.area;

        dim_buffer(&mut buffer, area, None);

        for cell in buffer.content.iter() {
            assert_eq!(cell.fg, Color::Reset);
            assert_eq!(cell.bg, Color::Rgb(10, 10, 10));
            assert!(cell.modifier.contains(Modifier::DIM));
        }
    }

    #[test]
    fn excluded_area_is_left_untouched() {
        let mut buffer = buffer_of(Color::Rgb(212, 212, 212), Color::Rgb(30, 30, 30));
        let area = buffer.area;

        dim_buffer(&mut buffer, area, Some(Rect::new(0, 0, 2, 1)));

        assert_eq!(buffer[(0, 0)].bg, Color::Rgb(30, 30, 30), "excluded");
        assert_eq!(buffer[(2, 0)].bg, Color::Rgb(10, 10, 10), "dimmed");
    }
}
