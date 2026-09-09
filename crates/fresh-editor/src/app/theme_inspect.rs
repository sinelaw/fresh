//! Theme inspector: Ctrl+Right-Click shows which theme key(s) style a screen position.

use super::types::{ThemeInfoPopup, ThemeKeyInfo};
use super::Editor;
use crate::services::plugins::hooks::HookArgs;
use crate::view::theme::color_to_rgb;
use anyhow::Result as AnyhowResult;
use ratatui::style::{Color, Style};

/// Where the last frame's cells came from, by their theme provenance.
///
/// **The provenance gate** (design §3.7.9): every cell a frame shows was
/// written either by the fold, from a display-list item — recorded with the
/// region `Chrome`, or relabelled by the described surface it belongs to,
/// as the status bar does — or by the one painter left, the pane's text
/// pipeline, which records its own two regions ([`PAINTER_REGIONS`]). A
/// painter-written cell outside every `Draw::Host` item's rectangle would
/// be a painter that is not a leaf's, which is the migration's defect;
/// there are none, and this is what says so.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct CellsProvenance {
    /// Cells the fold wrote from a described item.
    pub fold: usize,
    /// Cells the pane's painter wrote inside a host leaf's rectangle.
    pub painter_in_hosts: usize,
    /// Cells the pane's painter wrote outside every host leaf's rectangle,
    /// as `(col, row, region)`. Empty on every frame.
    pub painter_outside_hosts: Vec<(u16, u16, String)>,
    /// The host leaves the frame declared, for a report that names where the
    /// painter was allowed to write as well as where it did.
    pub hosts: Vec<ratatui::layout::Rect>,
}

/// The regions the pane's text pipeline files its cells under — the one
/// painter's labels. Every other region is a described surface's.
pub const PAINTER_REGIONS: [&str; 2] = ["Editor Content", "Line Numbers"];

impl Editor {
    /// Classify the last frame's cells by who wrote them — see
    /// [`CellsProvenance`]. Read after `render`, over the frame it drew.
    pub fn cells_provenance(&self) -> CellsProvenance {
        let chrome = self.active_chrome();
        let (width, height) = (chrome.last_frame.width, chrome.last_frame.height);
        let hosts: Vec<ratatui::layout::Rect> = self
            .shell_ui
            .as_ref()
            .map(|ui| {
                ui.spec()
                    .items
                    .iter()
                    .filter(|i| matches!(i.draw, fresh_ui::Draw::Host(_)))
                    .map(|i| {
                        let r = i.visible_rect();
                        ratatui::layout::Rect::new(r.x.max(0) as u16, r.y.max(0) as u16, r.w, r.h)
                    })
                    .collect()
            })
            .unwrap_or_default();
        let mut out = CellsProvenance::default();
        for row in 0..height {
            for col in 0..width {
                let Some(cell) = chrome.cell_theme_at(col, row) else {
                    continue;
                };
                let region = cell.region.as_ref();
                if region.is_empty() {
                    continue;
                }
                if !PAINTER_REGIONS.contains(&region) {
                    out.fold += 1;
                    continue;
                }
                let inside = hosts.iter().any(|h| {
                    col >= h.x && col < h.x + h.width && row >= h.y && row < h.y + h.height
                });
                match inside {
                    true => out.painter_in_hosts += 1,
                    false => out
                        .painter_outside_hosts
                        .push((col, row, region.to_string())),
                }
            }
        }
        out.hosts = hosts;
        out
    }

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

        // Where the active pane's content is, from the tree. The scan this
        // replaces looked the same pane up in the painter's record and used
        // the entry to confirm the pane still shows the active buffer —
        // which the split model answers, and answers about *now* rather than
        // about the last frame.
        let (content_rect, gutter_width, compose_width, primary_cursor) = {
            let shows_active = self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.pane_buffer(active_split))
                == Some(active_buffer);
            let Some(rect) = self
                .pane_content_rect(active_split)
                .filter(|_| shows_active)
            else {
                return;
            };
            let gw = self
                .buffers()
                .get(&active_buffer)
                .map(|s| s.margins.left_total_width() as u16)
                .unwrap_or(0);
            let vs = match self
                .windows
                .get(&self.active_window)
                .and_then(|w| w.buffers.splits())
                .map(|(_, vs)| vs)
                .expect("active window must have a populated split layout")
                .get(&active_split)
            {
                Some(vs) => vs,
                None => return,
            };
            (rect, gw, vs.compose_width, *vs.cursors.primary())
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

    /// The inspector's description: its lines, its button, and where it goes.
    ///
    /// **One derivation.** `render_theme_info_popup` built these lines and
    /// drew them; `theme_info_popup_rect` walked the same conditions again
    /// with a `line_count` to say how tall the box was and which row the
    /// button sat on, under a comment saying it "must match
    /// render_theme_info_popup logic". The tree measures the lines it is
    /// given, and the button is a node rather than a row offset, so both
    /// second derivations are gone with the painter.
    pub(crate) fn theme_info_description(
        &self,
    ) -> Option<crate::view::shell::theme_info::ThemeInfo> {
        use crate::view::markdown::StyledLine;
        use crate::view::shell::theme_info::{Button, ThemeInfo};

        let popup = self.active_window().theme_info_popup.as_ref()?;
        let theme = &*self.theme.read().unwrap();
        let info = &popup.info;

        // Key names render in the popup's own text colour (always legible on
        // popup_bg) with bold to set them apart from the "Foreground:" label.
        // `menu_highlight_fg` was wrong here: it's the fg for
        // `menu_highlight_bg` and on some themes (e.g. dracula) equals
        // popup_bg, so the key vanished.
        let key_style = Style::default()
            .fg(theme.popup_text_fg)
            .add_modifier(ratatui::style::Modifier::BOLD);
        let plain = Style::default().fg(theme.popup_text_fg);
        let line = |spans: Vec<(String, Style)>| StyledLine {
            spans: spans
                .into_iter()
                .map(|(text, style)| crate::view::markdown::StyledSpan {
                    text,
                    style,
                    link_url: None,
                })
                .collect(),
        };
        let one = |text: String, style: Style| line(vec![(text, style)]);

        let mut lines: Vec<StyledLine> = Vec::new();
        if !info.region.is_empty() {
            lines.push(one(format!(" Region: {}", info.region), plain));
            lines.push(StyledLine::new());
        }

        // Nothing the theme editor could open, so an explanatory message
        // instead of a button that would silently do nothing.
        if info.fg_key.is_none() && info.bg_key.is_none() {
            lines.push(one(" No theme key recorded here. ".into(), plain));
            lines.push(one(
                " This element isn't inspectable yet. ".into(),
                Style::default().fg(theme.menu_disabled_fg),
            ));
            return Some(ThemeInfo {
                at: popup.position,
                lines,
                button: None,
            });
        }

        // One half of the pair: its key, its swatch in its own colour, and —
        // for the foreground — the syntax category behind it.
        let half = |out: &mut Vec<StyledLine>,
                    label: &str,
                    key: &Option<String>,
                    color: Option<Color>,
                    cat: bool| {
            let Some(k) = key else { return };
            out.push(line(vec![
                (format!(" {label}: "), plain),
                (k.clone(), key_style),
            ]));
            if let Some(color) = color {
                out.push(line(vec![
                    ("   ".to_string(), Style::default()),
                    ("\u{2589} ".to_string(), Style::default().fg(color)),
                    (format_color_rgb(color), Style::default()),
                ]));
            }
            if cat {
                if let Some(cat) = info.syntax_category.as_ref() {
                    out.push(one(format!("   Category: {}", cat), Style::default()));
                }
            }
        };
        half(&mut lines, "Foreground", &info.fg_key, info.fg_color, true);
        lines.push(StyledLine::new());
        half(&mut lines, "Background", &info.bg_key, info.bg_color, false);
        lines.push(StyledLine::new());

        Some(ThemeInfo {
            at: popup.position,
            lines,
            button: Some(Button {
                label: " \u{25b6} Open in Theme Editor ".to_string(),
                hovered: matches!(
                    self.hovered(),
                    Some(crate::app::types::HoverTarget::ThemeInfoButton)
                ),
            }),
        })
    }
}

fn format_color_rgb(color: Color) -> String {
    if let Some((r, g, b)) = color_to_rgb(color) {
        format!("RGB({}, {}, {})", r, g, b)
    } else {
        format!("{:?}", color)
    }
}
