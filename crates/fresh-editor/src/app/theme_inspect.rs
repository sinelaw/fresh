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
    fn resolve_theme_key_at(&self, col: u16, row: u16) -> Option<ThemeKeyInfo> {
        let theme = &self.theme;

        // 1. Check status bar area
        if let Some((bar_row, bar_x, bar_width)) = self.cached_layout.status_bar_area {
            if row == bar_row && col >= bar_x && col < bar_x + bar_width {
                return Some(ThemeKeyInfo {
                    fg_key: Some("ui.status_bar_fg".into()),
                    bg_key: Some("ui.status_bar_bg".into()),
                    region: "Status Bar".into(),
                    fg_color: Some(theme.status_bar_fg),
                    bg_color: Some(theme.status_bar_bg),
                    syntax_category: None,
                });
            }
        }

        // 2. Check menu bar
        if let Some(ref menu_layout) = self.cached_layout.menu_layout {
            if point_in_rect(col, row, menu_layout.bar_area) {
                return Some(ThemeKeyInfo {
                    fg_key: Some("ui.menu_fg".into()),
                    bg_key: Some("ui.menu_bg".into()),
                    region: "Menu Bar".into(),
                    fg_color: Some(theme.menu_fg),
                    bg_color: Some(theme.menu_bg),
                    syntax_category: None,
                });
            }
        }

        // 3. Check tab bars
        for tab_layout in self.cached_layout.tab_layouts.values() {
            use crate::view::ui::tabs::TabHit;
            if let Some(hit) = tab_layout.hit_test(col, row) {
                let is_active = match &hit {
                    TabHit::TabName(buf_id) | TabHit::CloseButton(buf_id) => {
                        *buf_id == self.active_buffer()
                    }
                    _ => false,
                };
                let (fg_key, bg_key, fg_color, bg_color) = if is_active {
                    (
                        "ui.tab_active_fg",
                        "ui.tab_active_bg",
                        theme.tab_active_fg,
                        theme.tab_active_bg,
                    )
                } else {
                    (
                        "ui.tab_inactive_fg",
                        "ui.tab_inactive_bg",
                        theme.tab_inactive_fg,
                        theme.tab_inactive_bg,
                    )
                };
                return Some(ThemeKeyInfo {
                    fg_key: Some(fg_key.into()),
                    bg_key: Some(bg_key.into()),
                    region: if is_active {
                        "Active Tab".into()
                    } else {
                        "Inactive Tab".into()
                    },
                    fg_color: Some(fg_color),
                    bg_color: Some(bg_color),
                    syntax_category: None,
                });
            }
        }

        // 4. Check split separators
        for &(_, _, sep_x, sep_y, sep_len) in &self.cached_layout.separator_areas {
            if col == sep_x && row >= sep_y && row < sep_y + sep_len {
                return Some(ThemeKeyInfo {
                    fg_key: Some("ui.split_separator_fg".into()),
                    bg_key: None,
                    region: "Split Separator".into(),
                    fg_color: Some(theme.split_separator_fg),
                    bg_color: None,
                    syntax_category: None,
                });
            }
        }

        // 5. Check file explorer area
        if let Some(fe_area) = self.cached_layout.file_explorer_area {
            if point_in_rect(col, row, fe_area) {
                return Some(ThemeKeyInfo {
                    fg_key: Some("editor.fg".into()),
                    bg_key: Some("editor.bg".into()),
                    region: "File Explorer".into(),
                    fg_color: Some(theme.editor_fg),
                    bg_color: Some(theme.editor_bg),
                    syntax_category: None,
                });
            }
        }

        // 6. Check editor content areas (main case)
        for (split_id, buffer_id, content_rect, ..) in &self.cached_layout.split_areas {
            if !point_in_rect(col, row, *content_rect) {
                continue;
            }

            // Determine gutter width from buffer state
            let gutter_width = self
                .buffers
                .get(buffer_id)
                .map(|s| s.margins.left_total_width() as u16)
                .unwrap_or(0);

            let compose_width = self
                .split_view_states
                .get(split_id)
                .and_then(|vs| vs.compose_width);

            let adjusted_rect = Self::adjust_content_rect_for_compose(*content_rect, compose_width);
            let content_col = col.saturating_sub(adjusted_rect.x);

            // Line number gutter
            if content_col < gutter_width {
                return Some(ThemeKeyInfo {
                    fg_key: Some("editor.line_number_fg".into()),
                    bg_key: Some("editor.line_number_bg".into()),
                    region: "Line Numbers".into(),
                    fg_color: Some(theme.line_number_fg),
                    bg_color: Some(theme.line_number_bg),
                    syntax_category: None,
                });
            }

            // Content area: resolve byte position
            let cached_mappings = self.cached_layout.view_line_mappings.get(split_id).cloned();
            let fallback = self
                .split_view_states
                .get(split_id)
                .map(|vs| vs.viewport.top_byte)
                .unwrap_or(0);

            if let Some(byte_pos) = Self::screen_to_buffer_position(
                col,
                row,
                *content_rect,
                gutter_width,
                &cached_mappings,
                fallback,
                false,
                compose_width,
            ) {
                // Look up highlight category at this byte position
                if let Some(state) = self.buffers.get(buffer_id) {
                    let category = state.highlighter.category_at_position(byte_pos);
                    if let Some(cat) = category {
                        let key = cat.theme_key();
                        let color = crate::primitives::highlighter::highlight_color(cat, theme);
                        return Some(ThemeKeyInfo {
                            fg_key: Some(key.into()),
                            bg_key: Some("editor.bg".into()),
                            region: format!("Syntax: {}", cat.display_name()),
                            fg_color: Some(color),
                            bg_color: Some(theme.editor_bg),
                            syntax_category: Some(cat.display_name().into()),
                        });
                    }
                }

                // No highlight span → plain editor text
                return Some(ThemeKeyInfo {
                    fg_key: Some("editor.fg".into()),
                    bg_key: Some("editor.bg".into()),
                    region: "Editor Content".into(),
                    fg_color: Some(theme.editor_fg),
                    bg_color: Some(theme.editor_bg),
                    syntax_category: None,
                });
            }

            // Past end of line / empty area
            return Some(ThemeKeyInfo {
                fg_key: None,
                bg_key: Some("editor.bg".into()),
                region: "Editor Background".into(),
                fg_color: None,
                bg_color: Some(theme.editor_bg),
                syntax_category: None,
            });
        }

        // 7. Check scrollbar areas
        for (_, _, _, scrollbar_rect, thumb_start, thumb_end) in &self.cached_layout.split_areas {
            if point_in_rect(col, row, *scrollbar_rect) {
                let rel_row = (row - scrollbar_rect.y) as usize;
                let is_thumb = rel_row >= *thumb_start && rel_row < *thumb_end;
                return Some(ThemeKeyInfo {
                    fg_key: Some(if is_thumb {
                        "ui.scrollbar_thumb_fg".into()
                    } else {
                        "ui.scrollbar_track_fg".into()
                    }),
                    bg_key: None,
                    region: if is_thumb {
                        "Scrollbar Thumb".into()
                    } else {
                        "Scrollbar Track".into()
                    },
                    fg_color: Some(if is_thumb {
                        theme.scrollbar_thumb_fg
                    } else {
                        theme.scrollbar_track_fg
                    }),
                    bg_color: None,
                    syntax_category: None,
                });
            }
        }

        None
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

fn point_in_rect(col: u16, row: u16, rect: Rect) -> bool {
    col >= rect.x && col < rect.x + rect.width && row >= rect.y && row < rect.y + rect.height
}

fn format_color_rgb(color: Color) -> String {
    if let Some((r, g, b)) = color_to_rgb(color) {
        format!("RGB({}, {}, {})", r, g, b)
    } else {
        format!("{:?}", color)
    }
}
