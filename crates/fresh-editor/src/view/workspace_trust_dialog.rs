//! Workspace-trust prompt rendering.
//!
//! A bespoke security modal (radio group + descriptions + an OK button and a
//! secondary button), rendered on a dimmed backdrop in the modal z-band. As
//! the mandatory open-time gate the secondary button is "Quit" (exit the
//! editor) and there is no undecided outcome; when opened voluntarily from the
//! command palette the secondary button is "Cancel" (close without changing
//! the current level).

use crate::primitives::display_width::{str_width, DisplayWidth};
use crate::view::theme::Theme;
use ratatui::{
    layout::Rect,
    style::{Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, Clear, Paragraph},
    Frame,
};
use rust_i18n::t;

/// One selectable trust option: its radio label and the one-line description
/// shown beneath it. The mnemonic letter is appended in parentheses (e.g.
/// "(T)") so the keypress works the same in every locale while the surrounding
/// text is translatable.
pub struct TrustOption {
    pub label: String,
    pub description: String,
}

pub fn options() -> [TrustOption; 3] {
    [
        TrustOption {
            label: t!("trust.dialog.opt_trust_label").into_owned(),
            description: t!("trust.dialog.opt_trust_desc").into_owned(),
        },
        TrustOption {
            label: t!("trust.dialog.opt_restrict_label").into_owned(),
            description: t!("trust.dialog.opt_restrict_desc").into_owned(),
        },
        TrustOption {
            label: t!("trust.dialog.opt_block_label").into_owned(),
            description: t!("trust.dialog.opt_block_desc").into_owned(),
        },
    ]
}

const DIALOG_WIDTH: u16 = 68;

/// Click-target rects produced by a render pass, consumed by mouse hit-testing.
#[derive(Debug, Clone, Default)]
pub struct TrustDialogLayout {
    /// Outer dialog rect (borders included) — absorbs stray clicks.
    pub dialog: Rect,
    /// The radio line for each option (index matches the selection index).
    /// A zero-area rect means that row is currently scrolled out of view.
    pub radios: [Rect; 3],
    pub ok: Rect,
    pub quit: Rect,
    /// Maximum scroll offset (rows) — 0 when the dialog fits. Mouse-wheel
    /// handling clamps the scroll offset to this.
    pub max_scroll: u16,
}

/// Render the workspace-trust prompt centered in `area`, with `selected`
/// (0=Trust, 1=Restricted, 2=Block) marked. `triggers` is a comma-separated
/// list of the marker files/dirs that caused the prompt (shown as a "Detected:"
/// line; pass "" to omit). `secondary_label` is the right-hand button text
/// (e.g. "Quit (Ctrl+Q)" at startup, "Cancel (Esc)" from the palette). Returns
/// the click layout.
pub fn render_workspace_trust_dialog(
    frame: &mut Frame,
    area: Rect,
    selected: usize,
    path: &str,
    triggers: &str,
    secondary_label: &str,
    scroll: u16,
    theme: &Theme,
) -> TrustDialogLayout {
    let width = DIALOG_WIDTH.min(area.width.saturating_sub(4));
    let inner_w = width.saturating_sub(2);
    let bg = theme.popup_bg;
    let fg = theme.popup_text_fg;

    // --- Build the row plan, wrapping descriptions, so we can size the dialog
    //     to its content (no fixed height to drift out of sync). ---
    enum Seg {
        Header,
        Sep,
        Plain(String),
        Dim(String),
        Path(String),
        Radio(usize),
        Desc(String),
        Blank,
        Buttons,
    }
    let desc_w = inner_w.saturating_sub(6).max(8) as usize;
    let shown_path = truncate_middle(path, inner_w.saturating_sub(8).max(8) as usize);
    let opts = options();

    let mut segs: Vec<Seg> = vec![
        Seg::Header,
        Seg::Sep,
        Seg::Plain(format!(" {}", t!("trust.dialog.can_execute"))),
        Seg::Path(shown_path),
    ];
    // Why the prompt fired: the marker files/dirs we detected.
    if !triggers.is_empty() {
        let lines = wrap_text(
            &t!("trust.dialog.detected", triggers = triggers),
            inner_w.saturating_sub(2).max(8) as usize,
        );
        for line in lines {
            segs.push(Seg::Dim(format!(" {line}")));
        }
    }
    segs.push(Seg::Blank);
    segs.push(Seg::Plain(format!(" {}", t!("trust.dialog.how_proceed"))));
    segs.push(Seg::Blank);
    for (i, opt) in opts.iter().enumerate() {
        segs.push(Seg::Radio(i));
        for line in wrap_text(&opt.description, desc_w) {
            segs.push(Seg::Desc(line));
        }
        segs.push(Seg::Blank);
    }
    segs.push(Seg::Sep);
    segs.push(Seg::Buttons);

    let content_rows = segs.len() as u16;
    let height = (content_rows + 2).min(area.height.saturating_sub(2));
    let x = area.x + (area.width.saturating_sub(width)) / 2;
    let y = area.y + (area.height.saturating_sub(height)) / 2;
    let dialog = Rect {
        x,
        y,
        width,
        height,
    };

    frame.render_widget(Clear, dialog);
    let block = Block::default()
        .borders(Borders::ALL)
        .border_style(Style::default().fg(theme.popup_border_fg).bg(bg))
        .style(Style::default().bg(bg).fg(fg));
    let inner = block.inner(dialog);
    frame.render_widget(block, dialog);

    let mut layout = TrustDialogLayout {
        dialog,
        ..Default::default()
    };
    if inner.width == 0 || inner.height == 0 {
        return layout;
    }
    let iw = inner.width;
    let row_rect = |r: u16| Rect {
        x: inner.x,
        y: inner.y + r,
        width: iw,
        height: 1,
    };
    let put = |frame: &mut Frame, r: u16, line: Line| {
        if r < inner.height {
            frame.render_widget(
                Paragraph::new(line).style(Style::default().bg(bg)),
                row_rect(r),
            );
        }
    };

    // Scroll window: when the content is taller than the inner area (small
    // terminal), render the slice [scroll, scroll+visible) and show a scrollbar.
    let visible = inner.height;
    let content_rows = segs.len() as u16;
    let max_scroll = content_rows.saturating_sub(visible);
    let scroll = scroll.min(max_scroll);
    layout.max_scroll = max_scroll;

    for (lr, seg) in segs.into_iter().enumerate() {
        let lr = lr as u16;
        if lr < scroll {
            continue;
        }
        let r = lr - scroll;
        if r >= visible {
            break;
        }
        match seg {
            Seg::Header => put(
                frame,
                r,
                Line::from(Span::styled(
                    format!(" ⚠  {}", t!("trust.dialog.security_warning")),
                    // Title shares the dialog's own border/separator accent
                    // (`popup_border_fg`) so the whole frame reads as one
                    // coherent palette, rather than borrowing the status-bar
                    // warning colour from an unrelated component.
                    Style::default()
                        .fg(theme.popup_border_fg)
                        .bg(bg)
                        .add_modifier(Modifier::BOLD),
                )),
            ),
            Seg::Sep => put(
                frame,
                r,
                Line::from(Span::styled(
                    "─".repeat(iw as usize),
                    Style::default().fg(theme.popup_border_fg).bg(bg),
                )),
            ),
            Seg::Plain(text) => put(
                frame,
                r,
                Line::from(Span::styled(text, Style::default().fg(fg).bg(bg))),
            ),
            Seg::Dim(text) => put(
                frame,
                r,
                Line::from(Span::styled(
                    text,
                    Style::default().fg(fg).bg(bg).add_modifier(Modifier::DIM),
                )),
            ),
            Seg::Path(p) => put(
                frame,
                r,
                Line::from(vec![
                    Span::styled(
                        format!(" {} ", t!("trust.dialog.path_label")),
                        Style::default().fg(fg).bg(bg),
                    ),
                    Span::styled(
                        p,
                        Style::default().fg(fg).bg(bg).add_modifier(Modifier::BOLD),
                    ),
                ]),
            ),
            Seg::Radio(i) => {
                let is_sel = i == selected;
                let marker = if is_sel { "(*)" } else { "( )" };
                let style = if is_sel {
                    Style::default()
                        .fg(theme.popup_selection_fg)
                        .bg(theme.popup_selection_bg)
                        .add_modifier(Modifier::BOLD)
                } else {
                    Style::default().fg(fg).bg(bg)
                };
                let text = pad_to(&format!(" {marker} {}", opts[i].label), iw as usize);
                put(frame, r, Line::from(Span::styled(text, style)));
                layout.radios[i] = row_rect(r);
            }
            Seg::Desc(line) => put(
                frame,
                r,
                Line::from(Span::styled(
                    format!("      {line}"),
                    Style::default().fg(fg).bg(bg).add_modifier(Modifier::DIM),
                )),
            ),
            Seg::Blank => {}
            Seg::Buttons => {
                let (ok_rect, sec_rect) =
                    render_buttons(frame, row_rect(r), secondary_label, bg, fg);
                layout.ok = ok_rect;
                layout.quit = sec_rect;
            }
        }
    }

    // Scrollbar in the right-most inner column when the dialog overflows.
    if max_scroll > 0 {
        let track = Rect {
            x: inner.x + iw.saturating_sub(1),
            y: inner.y,
            width: 1,
            height: visible,
        };
        let state = crate::view::ui::scrollbar::ScrollbarState::new(
            content_rows as usize,
            visible as usize,
            scroll as usize,
        );
        let colors = crate::view::ui::scrollbar::ScrollbarColors::from_theme(theme);
        crate::view::ui::scrollbar::render_scrollbar(frame, track, &state, &colors);
    }

    layout
}

fn render_buttons(
    frame: &mut Frame,
    row: Rect,
    secondary_label: &str,
    bg: ratatui::style::Color,
    fg: ratatui::style::Color,
) -> (Rect, Rect) {
    let ok_label = format!("[ {} ]", t!("trust.dialog.btn_ok"));
    let quit_label = format!("[ {secondary_label} ]");
    let ok_w = ok_label.display_width() as u16;
    let quit_w = quit_label.display_width() as u16;
    // OK at ~1/4, Quit at ~3/4 of the row.
    let ok_x = row.x + row.width / 4 - ok_w / 2;
    let quit_x = row.x + (row.width * 3) / 4 - quit_w / 2;
    let ok_rect = Rect {
        x: ok_x,
        y: row.y,
        width: ok_w,
        height: 1,
    };
    let quit_rect = Rect {
        x: quit_x,
        y: row.y,
        width: quit_w,
        height: 1,
    };
    frame.render_widget(
        Paragraph::new(Line::from(Span::styled(
            ok_label,
            Style::default().fg(fg).bg(bg).add_modifier(Modifier::BOLD),
        ))),
        ok_rect,
    );
    frame.render_widget(
        Paragraph::new(Line::from(Span::styled(
            quit_label,
            Style::default().fg(fg).bg(bg).add_modifier(Modifier::BOLD),
        ))),
        quit_rect,
    );
    (ok_rect, quit_rect)
}

/// Greedy word-wrap `s` to lines of at most `width` display columns. Uses
/// Unicode display width so CJK double-wide characters count as two columns.
fn wrap_text(s: &str, width: usize) -> Vec<String> {
    let width = width.max(1);
    let mut lines = Vec::new();
    let mut cur = String::new();
    for word in s.split_whitespace() {
        if cur.is_empty() {
            cur.push_str(word);
        } else if str_width(&cur) + 1 + str_width(word) <= width {
            cur.push(' ');
            cur.push_str(word);
        } else {
            lines.push(std::mem::take(&mut cur));
            cur.push_str(word);
        }
    }
    if !cur.is_empty() {
        lines.push(cur);
    }
    if lines.is_empty() {
        lines.push(String::new());
    }
    lines
}

/// Right-pad `s` with spaces to `width` display columns (no truncation here;
/// callers pass text known to fit). Uses Unicode display width, so CJK
/// double-wide characters count as two columns.
fn pad_to(s: &str, width: usize) -> String {
    let len = str_width(s);
    if len >= width {
        s.to_string()
    } else {
        format!("{s}{}", " ".repeat(width - len))
    }
}

/// Shorten `s` to at most `max` columns, keeping the head and tail with an
/// ellipsis in the middle (paths are most meaningful at both ends).
fn truncate_middle(s: &str, max: usize) -> String {
    let chars: Vec<char> = s.chars().collect();
    if chars.len() <= max {
        return s.to_string();
    }
    if max <= 1 {
        return "…".to_string();
    }
    let keep = max - 1;
    let head = keep.div_ceil(2);
    let tail = keep - head;
    let head_s: String = chars[..head].iter().collect();
    let tail_s: String = chars[chars.len() - tail..].iter().collect();
    format!("{head_s}…{tail_s}")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn truncate_middle_keeps_ends() {
        assert_eq!(
            truncate_middle("/home/noam/chunky/fresh", 100),
            "/home/noam/chunky/fresh"
        );
        let t = truncate_middle("/home/noam/chunky/fresh", 11);
        assert_eq!(t.chars().count(), 11);
        assert!(t.contains('…'));
        assert!(t.starts_with('/'));
        assert!(t.ends_with('h'));
    }

    #[test]
    fn pad_to_fills_width() {
        assert_eq!(pad_to("ab", 5), "ab   ");
        assert_eq!(pad_to("abcde", 3), "abcde");
    }
}
