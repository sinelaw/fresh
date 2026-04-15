//! Post-pass decorators applied to a ratatui buffer after the main render.
//!
//! Each helper takes a typed input (frame/buffer + typed parameters) and has
//! no dependency on any shared render-time "mega struct".

use super::spans::compress_chars;
use crate::app::types::ViewLineMapping;
use crate::primitives::ansi_background::AnsiBackground;
use crate::view::overlay::Overlay;
use ratatui::layout::Rect;
use ratatui::style::{Color, Style};
use ratatui::text::Line;
use ratatui::Frame;
use std::ops::Range;

/// Render vertical column guide lines in the editor content area.
/// Used for both config-based vertical rulers and compose-mode column guides.
pub(super) fn render_column_guides(
    frame: &mut Frame,
    columns: &[u16],
    style: Style,
    render_area: Rect,
    gutter_width: usize,
    content_height: usize,
    left_column: usize,
) {
    let guide_height = content_height.min(render_area.height as usize);
    for &col in columns {
        // Account for horizontal scroll
        let Some(scrolled_col) = (col as usize).checked_sub(left_column) else {
            continue;
        };
        let guide_x = render_area.x + gutter_width as u16 + scrolled_col as u16;
        if guide_x < render_area.x + render_area.width {
            for row in 0..guide_height {
                let cell = &mut frame.buffer_mut()[(guide_x, render_area.y + row as u16)];
                cell.set_symbol("│");
                if let Some(fg) = style.fg {
                    cell.set_fg(fg);
                }
                if !style.add_modifier.is_empty() {
                    cell.set_style(Style::default().add_modifier(style.add_modifier));
                }
            }
        }
    }
}

/// Render vertical rulers as a subtle background color tint.
/// Unlike `render_column_guides` which draws │ characters (for compose guides),
/// this preserves the existing text content and only adjusts the background color.
pub(super) fn render_ruler_bg(
    frame: &mut Frame,
    columns: &[u16],
    color: Color,
    render_area: Rect,
    gutter_width: usize,
    content_height: usize,
    left_column: usize,
) {
    let guide_height = content_height.min(render_area.height as usize);
    for &col in columns {
        let Some(scrolled_col) = (col as usize).checked_sub(left_column) else {
            continue;
        };
        let guide_x = render_area.x + gutter_width as u16 + scrolled_col as u16;
        if guide_x < render_area.x + render_area.width {
            for row in 0..guide_height {
                let cell = &mut frame.buffer_mut()[(guide_x, render_area.y + row as u16)];
                cell.set_bg(color);
            }
        }
    }
}

/// Post-process the rendered frame to apply OSC 8 hyperlink escape sequences
/// for any overlays that have a URL set.
///
/// Uses `view_line_mappings` to translate overlay byte ranges into screen
/// positions, then wraps the corresponding cells with OSC 8 sequences so they
/// become clickable in terminals that support the protocol.
#[allow(dead_code)]
pub(super) fn apply_hyperlink_overlays(
    frame: &mut Frame,
    viewport_overlays: &[(Overlay, Range<usize>)],
    view_line_mappings: &[ViewLineMapping],
    render_area: Rect,
    gutter_width: usize,
    cursor_screen_pos: Option<(u16, u16)>,
) {
    let hyperlink_overlays: Vec<_> = viewport_overlays
        .iter()
        .filter(|(overlay, _)| overlay.url.is_some())
        .collect();

    if hyperlink_overlays.is_empty() {
        return;
    }

    let buf = frame.buffer_mut();
    for (screen_row, mapping) in view_line_mappings.iter().enumerate() {
        let y = render_area.y + screen_row as u16;
        if y >= render_area.y + render_area.height {
            break;
        }
        for (overlay, range) in &hyperlink_overlays {
            let url = overlay.url.as_ref().unwrap();
            // Find screen columns in this row whose source byte falls in range
            let mut run_start: Option<u16> = None;
            let content_x_offset = render_area.x + gutter_width as u16;
            for (char_idx, maybe_byte) in mapping.char_source_bytes.iter().enumerate() {
                let in_range = maybe_byte
                    .map(|b| b >= range.start && b < range.end)
                    .unwrap_or(false);
                let screen_x = content_x_offset + char_idx as u16;
                if in_range && screen_x < render_area.x + render_area.width {
                    if run_start.is_none() {
                        run_start = Some(screen_x);
                    }
                } else if let Some(start_x) = run_start.take() {
                    apply_osc8_to_cells(buf, start_x, screen_x, y, url, cursor_screen_pos);
                }
            }
            // Flush trailing run
            if let Some(start_x) = run_start {
                let end_x = content_x_offset + mapping.char_source_bytes.len() as u16;
                let end_x = end_x.min(render_area.x + render_area.width);
                apply_osc8_to_cells(buf, start_x, end_x, y, url, cursor_screen_pos);
            }
        }
    }
}

/// Apply OSC 8 hyperlink escape sequences to a run of buffer cells.
///
/// Uses 2-character chunking to work around Crossterm width accounting issues
/// with OSC sequences. When the cursor falls on the second character of a
/// 2-char chunk, the chunk is split into two 1-char chunks so the terminal
/// cursor remains visible on the correct cell.
#[allow(dead_code)]
pub(super) fn apply_osc8_to_cells(
    buf: &mut ratatui::buffer::Buffer,
    start_x: u16,
    end_x: u16,
    y: u16,
    url: &str,
    cursor_pos: Option<(u16, u16)>,
) {
    let area = *buf.area();
    if y < area.y || y >= area.y + area.height {
        return;
    }
    let max_x = area.x + area.width;
    let cursor_x = cursor_pos.and_then(|(cx, cy)| if cy == y { Some(cx) } else { None });
    let mut x = start_x;
    while x < end_x {
        if x >= max_x {
            break;
        }
        let chunk_size = if cursor_x == Some(x + 1) { 1 } else { 2 };

        let mut chunk = String::new();
        let chunk_start = x;
        for _ in 0..chunk_size {
            if x >= end_x || x >= max_x {
                break;
            }
            let sym = buf[(x, y)].symbol().to_string();
            chunk.push_str(&sym);
            x += 1;
        }
        if !chunk.is_empty() {
            let actual_chunk_len = x - chunk_start;
            let hyperlink = format!("\x1B]8;;{}\x07{}\x1B]8;;\x07", url, chunk);
            buf[(chunk_start, y)].set_symbol(&hyperlink);
            for cx in (chunk_start + 1)..chunk_start + actual_chunk_len {
                buf[(cx, y)].set_symbol("");
            }
        }
    }
}

/// Apply an `AnsiBackground` fade to a rendered line-of-spans buffer.
///
/// Each cell's background is only replaced when the existing background is
/// unset or `Color::Reset`, so syntax/selection backgrounds win.
#[allow(clippy::too_many_arguments)]
pub(super) fn apply_background_to_lines(
    lines: &mut Vec<Line<'static>>,
    area_width: u16,
    background: &AnsiBackground,
    theme_bg: Color,
    default_fg: Color,
    fade: f32,
    x_offset: usize,
    y_offset: usize,
) {
    if area_width == 0 {
        return;
    }

    let width = area_width as usize;

    for (y, line) in lines.iter_mut().enumerate() {
        // Flatten existing spans into per-character styles
        let mut existing: Vec<(char, Style)> = Vec::new();
        let spans = std::mem::take(&mut line.spans);
        for span in spans {
            let style = span.style;
            for ch in span.content.chars() {
                existing.push((ch, style));
            }
        }

        let mut chars_with_style = Vec::with_capacity(width);
        for x in 0..width {
            let sample_x = x_offset + x;
            let sample_y = y_offset + y;

            let (ch, mut style) = if x < existing.len() {
                existing[x]
            } else {
                (' ', Style::default().fg(default_fg))
            };

            if let Some(bg_color) = background.faded_color(sample_x, sample_y, theme_bg, fade) {
                if style.bg.is_none() || matches!(style.bg, Some(Color::Reset)) {
                    style = style.bg(bg_color);
                }
            }

            chars_with_style.push((ch, style));
        }

        line.spans = compress_chars(chars_with_style);
    }
}
