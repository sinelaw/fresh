//! Theme inspector: Ctrl+Right-Click shows which theme key(s) style a screen position.

use super::types::{ThemeInfoPopup, ThemeKeyInfo};
use super::Editor;
use crate::services::plugins::hooks::HookArgs;
use crate::view::theme::color_to_rgb;
use anyhow::Result as AnyhowResult;
use ratatui::layout::Rect;
use ratatui::style::{Color, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Clear, Paragraph};
use ratatui::Frame;

impl Editor {
    /// Show the theme info popup at the given screen position (Ctrl+Right-Click).
    pub(super) fn show_theme_info_popup(&mut self, col: u16, row: u16) -> AnyhowResult<()> {
        if let Some(info) = self.resolve_theme_key_at(col, row) {
            // Position the popup near the click, offset down-right by 1
            let popup_x = col.saturating_add(1);
            let popup_y = row.saturating_add(1);
            self.theme_info_popup = Some(ThemeInfoPopup {
                position: (popup_x, popup_y),
                info,
                button_highlighted: false,
            });
        }
        Ok(())
    }

    /// Fire the `theme_inspect_key` hook for the given key.
    pub(super) fn fire_theme_inspect_hook(&mut self, key: String) {
        let theme_name = self.config.theme.0.clone();
        self.plugin_manager.run_hook(
            "theme_inspect_key",
            HookArgs::ThemeInspectKey { theme_name, key },
        );
    }

    /// Inspect the theme key at the current cursor's screen position and open the theme editor.
    pub(super) fn inspect_theme_at_cursor(&mut self) {
        let active_split = self.split_manager.active_split();
        let active_buffer = self.active_buffer();

        // Gather layout info and cursor from split_view_states (immutable borrows)
        let (content_rect, gutter_width, compose_width, primary_cursor) = match self
            .cached_layout
            .split_areas
            .iter()
            .find(|(sid, bid, ..)| *sid == active_split && *bid == active_buffer)
        {
            Some((split_id, buffer_id, rect, ..)) => {
                let gw = self
                    .buffers
                    .get(buffer_id)
                    .map(|s| s.margins.left_total_width() as u16)
                    .unwrap_or(0);
                let vs = match self.split_view_states.get(split_id) {
                    Some(vs) => vs,
                    None => return,
                };
                (*rect, gw, vs.compose_width, *vs.cursors.primary())
            }
            None => return,
        };

        // Compute cursor screen position (needs &mut buffer for line_iterator)
        let state = match self.buffers.get_mut(&active_buffer) {
            Some(s) => s,
            None => return,
        };
        let viewport = &self.split_view_states[&active_split].viewport;
        let cursor_rel = viewport.cursor_screen_position(&mut state.buffer, &primary_cursor);

        let adjusted_rect = Self::adjust_content_rect_for_compose(content_rect, compose_width);
        let screen_col = cursor_rel.0 + adjusted_rect.x + gutter_width;
        let screen_row = cursor_rel.1 + content_rect.y;

        if let Some(info) = self.resolve_theme_key_at(screen_col, screen_row) {
            if let Some(key) = info.fg_key {
                self.fire_theme_inspect_hook(key);
            }
        }
    }

    /// Resolve which theme key(s) style the character at screen position (col, row).
    /// Looks up the per-cell theme key map populated during rendering.
    fn resolve_theme_key_at(&self, col: u16, row: u16) -> Option<ThemeKeyInfo> {
        let cell = self.cached_layout.cell_theme_at(col, row)?;
        let theme = &self.theme;

        // Resolve actual colors from theme keys
        let fg_color = cell.fg_key.and_then(|k| theme.resolve_theme_key(k));
        let bg_color = cell.bg_key.and_then(|k| theme.resolve_theme_key(k));

        // Build region string, incorporating syntax category if present
        let region = if let Some(cat) = cell.syntax_category {
            format!("Syntax: {}", cat)
        } else {
            cell.region.to_string()
        };

        Some(ThemeKeyInfo {
            fg_key: cell.fg_key.map(String::from),
            bg_key: cell.bg_key.map(String::from),
            region,
            fg_color,
            bg_color,
            syntax_category: cell.syntax_category.map(String::from),
        })
    }

    /// Record theme key info for non-editor UI regions (status bar, tabs, menu, file explorer, scrollbar).
    /// Called after all rendering is complete, using cached layout areas.
    pub(super) fn record_non_editor_theme_regions(&mut self) {
        use super::types::CellThemeInfo;

        let sw = self.cached_layout.last_frame_width as usize;

        // Status bar
        if let Some((row, x, width)) = self.cached_layout.status_bar_area {
            let info = CellThemeInfo {
                fg_key: Some("ui.status_bar_fg"),
                bg_key: Some("ui.status_bar_bg"),
                region: "Status Bar",
                syntax_category: None,
            };
            for col in x..x + width {
                let idx = row as usize * sw + col as usize;
                if let Some(cell) = self.cached_layout.cell_theme_map.get_mut(idx) {
                    *cell = info.clone();
                }
            }
        }

        // Menu bar
        if let Some(bar_area) = self.cached_layout.menu_layout.as_ref().map(|m| m.bar_area) {
            let info = CellThemeInfo {
                fg_key: Some("ui.menu_fg"),
                bg_key: Some("ui.menu_bg"),
                region: "Menu Bar",
                syntax_category: None,
            };
            for row in bar_area.y..bar_area.y + bar_area.height {
                for col in bar_area.x..bar_area.x + bar_area.width {
                    let idx = row as usize * sw + col as usize;
                    if let Some(cell) = self.cached_layout.cell_theme_map.get_mut(idx) {
                        *cell = info.clone();
                    }
                }
            }
        }

        // File explorer
        if let Some(area) = self.cached_layout.file_explorer_area {
            let info = CellThemeInfo {
                fg_key: Some("editor.fg"),
                bg_key: Some("editor.bg"),
                region: "File Explorer",
                syntax_category: None,
            };
            for row in area.y..area.y + area.height {
                for col in area.x..area.x + area.width {
                    let idx = row as usize * sw + col as usize;
                    if let Some(cell) = self.cached_layout.cell_theme_map.get_mut(idx) {
                        *cell = info.clone();
                    }
                }
            }
        }

        // Scrollbars
        let split_areas = self.cached_layout.split_areas.clone();
        for (_, _, _, scrollbar_rect, thumb_start, thumb_end) in &split_areas {
            for row in scrollbar_rect.y..scrollbar_rect.y + scrollbar_rect.height {
                let rel_row = (row - scrollbar_rect.y) as usize;
                let is_thumb = rel_row >= *thumb_start && rel_row < *thumb_end;
                let info = CellThemeInfo {
                    fg_key: Some(if is_thumb {
                        "ui.scrollbar_thumb_fg"
                    } else {
                        "ui.scrollbar_track_fg"
                    }),
                    bg_key: Some("editor.bg"),
                    region: if is_thumb {
                        "Scrollbar Thumb"
                    } else {
                        "Scrollbar Track"
                    },
                    syntax_category: None,
                };
                for col in scrollbar_rect.x..scrollbar_rect.x + scrollbar_rect.width {
                    let idx = row as usize * sw + col as usize;
                    if let Some(cell) = self.cached_layout.cell_theme_map.get_mut(idx) {
                        *cell = info.clone();
                    }
                }
            }
        }

        // Tab bars — record from tab layouts
        let tab_layouts = self.cached_layout.tab_layouts.clone();
        for (_, tab_layout) in &tab_layouts {
            {
                let area = tab_layout.bar_area;
                let info = CellThemeInfo {
                    fg_key: Some("ui.tab_inactive_fg"),
                    bg_key: Some("ui.tab_separator_bg"),
                    region: "Tab Bar",
                    syntax_category: None,
                };
                for row in area.y..area.y + area.height {
                    for col in area.x..area.x + area.width {
                        let idx = row as usize * sw + col as usize;
                        if let Some(cell) = self.cached_layout.cell_theme_map.get_mut(idx) {
                            *cell = info.clone();
                        }
                    }
                }
            }
        }
    }

    /// Render the theme info popup.
    pub(super) fn render_theme_info_popup(&self, frame: &mut Frame) {
        let popup = match &self.theme_info_popup {
            Some(p) => p,
            None => return,
        };
        let theme = &self.theme;
        let info = &popup.info;

        let mut lines = vec![];
        lines.push(Line::from(format!(" Region: {}", info.region)));
        lines.push(Line::from(""));

        if let Some(ref fg_key) = info.fg_key {
            lines.push(Line::from(vec![
                Span::styled(" Foreground: ", Style::default().fg(theme.popup_text_fg)),
                Span::styled(fg_key.clone(), Style::default().fg(theme.menu_highlight_fg)),
            ]));
            if let Some(color) = info.fg_color {
                let rgb_str = format_color_rgb(color);
                lines.push(Line::from(vec![
                    Span::raw("   "),
                    Span::styled("\u{2589} ", Style::default().fg(color)),
                    Span::raw(rgb_str),
                ]));
            }
            if let Some(ref cat) = info.syntax_category {
                lines.push(Line::from(format!("   Category: {}", cat)));
            }
        }

        lines.push(Line::from(""));
        if let Some(ref bg_key) = info.bg_key {
            lines.push(Line::from(vec![
                Span::styled(" Background: ", Style::default().fg(theme.popup_text_fg)),
                Span::styled(bg_key.clone(), Style::default().fg(theme.menu_highlight_fg)),
            ]));
            if let Some(color) = info.bg_color {
                let rgb_str = format_color_rgb(color);
                lines.push(Line::from(vec![
                    Span::raw("   "),
                    Span::styled("\u{2589} ", Style::default().fg(color)),
                    Span::raw(rgb_str),
                ]));
            }
        }

        lines.push(Line::from(""));
        let button_style = if popup.button_highlighted {
            Style::default()
                .fg(theme.popup_selection_fg)
                .bg(theme.popup_selection_bg)
        } else {
            Style::default().fg(theme.popup_text_fg)
        };
        lines.push(Line::from(Span::styled(
            " \u{25b6} Open in Theme Editor ",
            button_style,
        )));

        let width = POPUP_WIDTH;
        let height = lines.len() as u16 + 2; // +2 for borders

        let screen = frame.area();
        let rect = compute_popup_rect(popup.position, width, height, screen.width, screen.height);

        frame.render_widget(Clear, rect);
        let block = Block::default()
            .borders(Borders::ALL)
            .border_style(Style::default().fg(theme.popup_border_fg))
            .title(" Theme Info ")
            .style(Style::default().bg(theme.popup_bg).fg(theme.popup_text_fg));
        let paragraph = Paragraph::new(lines).block(block);
        frame.render_widget(paragraph, rect);
    }

    /// Compute the bounding rect of the theme info popup (for hit-testing).
    pub(super) fn theme_info_popup_rect(&self) -> Option<(Rect, u16)> {
        let popup = self.theme_info_popup.as_ref()?;
        let info = &popup.info;

        // Count lines (must match render_theme_info_popup logic)
        let mut line_count: u16 = 2; // region + blank
        if info.fg_key.is_some() {
            line_count += 1; // foreground key
            if info.fg_color.is_some() {
                line_count += 1; // color swatch
            }
            if info.syntax_category.is_some() {
                line_count += 1; // category
            }
        }
        line_count += 1; // blank
        if info.bg_key.is_some() {
            line_count += 1; // background key
            if info.bg_color.is_some() {
                line_count += 1; // color swatch
            }
        }
        line_count += 2; // blank + button

        let width = POPUP_WIDTH;
        let height = line_count + 2; // +2 for borders
                                     // The button is on the last content row (before bottom border)
        let button_row_offset = line_count; // 0-indexed from popup y + 1 (top border)

        // Use the same screen-aware positioning as render to match the actual drawn rect
        let screen_w = self.cached_layout.last_frame_width;
        let screen_h = self.cached_layout.last_frame_height;
        let rect = compute_popup_rect(popup.position, width, height, screen_w, screen_h);

        Some((rect, button_row_offset))
    }
}

/// Width of the theme info popup (wide enough for keys like "editor.line_number_bg").
const POPUP_WIDTH: u16 = 40;

/// Compute the final popup rect, flipping near screen edges.
fn compute_popup_rect(
    position: (u16, u16),
    width: u16,
    height: u16,
    screen_w: u16,
    screen_h: u16,
) -> Rect {
    let x = if position.0 + width > screen_w {
        screen_w.saturating_sub(width)
    } else {
        position.0
    };
    let y = if position.1 + height > screen_h {
        position.1.saturating_sub(height + 1)
    } else {
        position.1
    };
    Rect::new(x, y, width.min(screen_w), height.min(screen_h))
}

fn format_color_rgb(color: Color) -> String {
    if let Some((r, g, b)) = color_to_rgb(color) {
        format!("RGB({}, {}, {})", r, g, b)
    } else {
        format!("{:?}", color)
    }
}
