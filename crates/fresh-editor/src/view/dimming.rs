//! Dimming effects for modal dialogs
//!
//! Provides utilities to dim areas of the frame buffer to indicate
//! that focus is on a modal dialog layer above the dimmed content.

use ratatui::layout::Rect;
use ratatui::style::Color;
use ratatui::Frame;

/// Dims a color by reducing its brightness by ~60%
fn dim_color(color: Color) -> Color {
    match color {
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
        Color::Reset => Color::Rgb(30, 30, 30),
    }
}

/// Apply dimming effect to all cells in an area
pub fn apply_dimming(frame: &mut Frame, area: Rect) {
    apply_dimming_excluding(frame, area, None);
}

/// Apply dimming effect to an area, optionally excluding a sub-area
pub fn apply_dimming_excluding(frame: &mut Frame, area: Rect, exclude: Option<Rect>) {
    let buf = frame.buffer_mut();

    for y in area.y..area.y.saturating_add(area.height) {
        for x in area.x..area.x.saturating_add(area.width) {
            // Skip cells inside the excluded area (if any)
            if let Some(ex) = exclude {
                if x >= ex.x && x < ex.x + ex.width && y >= ex.y && y < ex.y + ex.height {
                    continue;
                }
            }

            if let Some(cell) = buf.cell_mut((x, y)) {
                let style = cell.style();
                let new_fg = style.fg.map(dim_color).unwrap_or(Color::Rgb(40, 40, 40));
                let new_bg = style.bg.map(dim_color).unwrap_or(Color::Rgb(15, 15, 15));
                cell.set_fg(new_fg);
                cell.set_bg(new_bg);
            }
        }
    }
}
