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
            // Dismiss any existing LSP hover popup to avoid overlapping popups
            self.active_window_mut().mouse_state.lsp_hover_state = None;
            self.active_window_mut().mouse_state.lsp_hover_request_sent = false;
            self.dismiss_transient_popups();

            // Position the popup near the click, offset down-right by 1
            let popup_x = col.saturating_add(1);
            let popup_y = row.saturating_add(1);
            self.active_window_mut().theme_info_popup = Some(ThemeInfoPopup {
                position: (popup_x, popup_y),
                info,
                button_highlighted: false,
            });
        }
        Ok(())
    }

    /// Fire the `theme_inspect_key` hook for the given key.
    pub(super) fn fire_theme_inspect_hook(&mut self, key: String) {
        // Resolve the config value (which may be a portable form like
        // `s-dark.json` or `builtin://dark`) to the canonical registry key
        // the plugin's theme registry uses internally. Falls back to the
        // raw config value if resolution fails.
        let theme_name = self
            .theme_registry
            .resolve_key(&self.config.theme.0)
            .unwrap_or_else(|| self.config.theme.0.clone());
        self.plugin_manager.read().unwrap().run_hook(
            "theme_inspect_key",
            HookArgs::ThemeInspectKey { theme_name, key },
        );
    }

    /// Inspect the theme key at the current cursor's screen position and open the theme editor.
    pub(super) fn inspect_theme_at_cursor(&mut self) {
        let active_split = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
            .active_split();
        let active_buffer = self.active_buffer();

        // Gather layout info and cursor from split_view_states (immutable borrows)
        let (content_rect, gutter_width, compose_width, primary_cursor) = match self
            .active_layout()
            .split_areas
            .iter()
            .find(|(sid, bid, ..)| *sid == active_split && *bid == active_buffer)
        {
            Some((split_id, buffer_id, rect, ..)) => {
                let gw = self
                    .buffers()
                    .get(buffer_id)
                    .map(|s| s.margins.left_total_width() as u16)
                    .unwrap_or(0);
                let vs = match self
                    .windows
                    .get(&self.active_window)
                    .and_then(|w| w.buffers.splits())
                    .map(|(_, vs)| vs)
                    .expect("active window must have a populated split layout")
                    .get(split_id)
                {
                    Some(vs) => vs,
                    None => return,
                };
                (*rect, gw, vs.compose_width, *vs.cursors.primary())
            }
            None => return,
        };

        // Compute cursor screen position (needs &mut buffer for line_iterator).
        // Clone the viewport via the Window accessor so we can later
        // pass `&mut buffer` to cursor_screen_position without
        // overlapping with the splits read.
        let viewport = self
            .active_window()
            .buffers
            .splits()
            .expect("active window must have a populated split layout")
            .1[&active_split]
            .viewport
            .clone();
        let state = match self.active_window_mut().buffers.get_mut(&active_buffer) {
            Some(s) => s,
            None => return,
        };
        let cursor_rel = viewport.cursor_screen_position(&mut state.buffer, &primary_cursor);

        let adjusted_rect =
            super::click_geometry::adjust_content_rect_for_compose(content_rect, compose_width);
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
        let cell = self.active_chrome().cell_theme_at(col, row)?;
        let theme = &*self.theme.read().unwrap();

        // Resolve actual colors from theme keys
        let fg_color = cell
            .fg_key
            .as_ref()
            .and_then(|k| theme.resolve_theme_key(k));
        let bg_color = cell
            .bg_key
            .as_ref()
            .and_then(|k| theme.resolve_theme_key(k));

        // Build region string, incorporating syntax category if present
        let region = if let Some(cat) = cell.syntax_category.as_ref() {
            format!("Syntax: {}", cat)
        } else {
            cell.region.to_string()
        };

        Some(ThemeKeyInfo {
            fg_key: cell.fg_key.as_ref().map(|k| k.to_string()),
            bg_key: cell.bg_key.as_ref().map(|k| k.to_string()),
            region,
            fg_color,
            bg_color,
            syntax_category: cell.syntax_category.as_ref().map(|c| c.to_string()),
        })
    }

    /// Render the theme info popup.
    pub(super) fn render_theme_info_popup(&self, frame: &mut Frame) {
        let popup = match &self.active_window().theme_info_popup {
            Some(p) => p,
            None => return,
        };
        let theme = &*self.theme.read().unwrap();
        let info = &popup.info;

        // Key names render in the popup's own text colour (always legible on
        // popup_bg) with bold to set them apart from the "Foreground:" label.
        // `menu_highlight_fg` was wrong here: it's the fg for `menu_highlight_bg`
        // and on some themes (e.g. dracula) equals popup_bg, so the key vanished.
        let key_style = Style::default()
            .fg(theme.popup_text_fg)
            .add_modifier(ratatui::style::Modifier::BOLD);

        // When no theme key was recorded for this cell there is nothing the
        // theme editor could open, so we show an explanatory message instead
        // of a "▶ Open in Theme Editor" button that would silently do nothing.
        let has_keys = info.fg_key.is_some() || info.bg_key.is_some();

        let mut lines = vec![];
        if !info.region.is_empty() {
            lines.push(Line::from(format!(" Region: {}", info.region)));
            lines.push(Line::from(""));
        }

        if !has_keys {
            lines.push(Line::from(Span::styled(
                " No theme key recorded here. ",
                Style::default().fg(theme.popup_text_fg),
            )));
            lines.push(Line::from(Span::styled(
                " This element isn't inspectable yet. ",
                Style::default().fg(theme.menu_disabled_fg),
            )));

            let width = POPUP_WIDTH;
            let height = lines.len() as u16 + 2; // +2 for borders

            let screen = frame.area();
            let rect =
                compute_popup_rect(popup.position, width, height, screen.width, screen.height);

            frame.render_widget(Clear, rect);
            let block = Block::default()
                .borders(Borders::ALL)
                .border_style(Style::default().fg(theme.popup_border_fg))
                .title(" Theme Info ")
                .style(Style::default().bg(theme.popup_bg).fg(theme.popup_text_fg));
            let paragraph = Paragraph::new(lines).block(block);
            frame.render_widget(paragraph, rect);
            return;
        }

        if let Some(ref fg_key) = info.fg_key {
            lines.push(Line::from(vec![
                Span::styled(" Foreground: ", Style::default().fg(theme.popup_text_fg)),
                Span::styled(fg_key.clone(), key_style),
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
                Span::styled(bg_key.clone(), key_style),
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

    /// Compute the bounding rect of the theme info popup (for hit-testing),
    /// plus the row offset of the "Open in Theme Editor" button relative to
    /// `rect.y` (i.e. the button's screen row is `rect.y + offset`). The
    /// offset is `None` when the popup carries no theme keys (the "no theme
    /// key recorded" message variant has no button).
    pub(super) fn theme_info_popup_rect(&self) -> Option<(Rect, Option<u16>)> {
        let popup = self.active_window().theme_info_popup.as_ref()?;
        let info = &popup.info;
        let has_keys = info.fg_key.is_some() || info.bg_key.is_some();

        // Count lines (must match render_theme_info_popup logic)
        let mut line_count: u16 = 0;
        if !info.region.is_empty() {
            line_count += 2; // region + blank
        }

        let button_row_offset = if has_keys {
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
            line_count += 1; // blank before button
            line_count += 1; // button (the last content row)
                             // The button is the final content line, so its
                             // screen row is `popup_rect.y + total_line_count`
                             // (matches the click/hover hit-testing math).
            Some(line_count)
        } else {
            line_count += 2; // two-line "no theme key recorded" message
            None
        };

        let width = POPUP_WIDTH;
        let height = line_count + 2; // +2 for borders

        // Use the same screen-aware positioning as render to match the actual drawn rect
        let screen_w = self.active_chrome().last_frame_width;
        let screen_h = self.active_chrome().last_frame_height;
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
