//! Workspace-trust prompt rendering.
//!
//! A bespoke security modal (radio group + descriptions + OK/Quit buttons),
//! rendered on a dimmed backdrop in the modal z-band. The choice is forced:
//! the user either picks a trust level and confirms, or quits the editor —
//! there is no "cancel"/undecided outcome.

use crate::view::theme::Theme;
use ratatui::{
    layout::Rect,
    style::{Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, Clear, Paragraph},
    Frame,
};

/// One selectable trust option: its radio label and the one-line description
/// shown beneath it. The mnemonic is the bracketed capital in `label`.
struct TrustOption {
    label: &'static str,
    description: &'static str,
}

const OPTIONS: [TrustOption; 3] = [
    TrustOption {
        label: "[T]rust folder & Allow Tooling",
        description: "Enables full LSP, scripts, and env manager.",
    },
    TrustOption {
        label: "[K]eep Restricted (Default)",
        description: "Open as plain text; blocks repo-controlled code.",
    },
    TrustOption {
        label: "[B]lock All Execution",
        description: "Hard sandbox; no background processes run at all.",
    },
];

const DIALOG_WIDTH: u16 = 60;
const INNER_ROWS: u16 = 18;

/// Click-target rects produced by a render pass, consumed by mouse hit-testing.
#[derive(Debug, Clone, Default)]
pub struct TrustDialogLayout {
    /// Outer dialog rect (borders included) — absorbs stray clicks.
    pub dialog: Rect,
    /// The radio line for each option (index matches the selection index).
    pub radios: [Rect; 3],
    pub ok: Rect,
    pub quit: Rect,
}

/// Render the workspace-trust prompt centered in `area`, with `selected`
/// (0=Trust, 1=Restricted, 2=Block) marked. `quit_hint` is the user's bound
/// quit key (e.g. "Ctrl+Q"), shown on the Quit button. Returns the click layout.
pub fn render_workspace_trust_dialog(
    frame: &mut Frame,
    area: Rect,
    selected: usize,
    path: &str,
    quit_hint: Option<&str>,
    theme: &Theme,
) -> TrustDialogLayout {
    let width = DIALOG_WIDTH.min(area.width.saturating_sub(4));
    let height = (INNER_ROWS + 2).min(area.height.saturating_sub(2));
    let x = area.x + (area.width.saturating_sub(width)) / 2;
    let y = area.y + (area.height.saturating_sub(height)) / 2;
    let dialog = Rect {
        x,
        y,
        width,
        height,
    };

    frame.render_widget(Clear, dialog);

    let bg = theme.popup_bg;
    let fg = theme.popup_text_fg;
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

    // Helper: render one full-width line of text at inner row `r`.
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
    let separator = |frame: &mut Frame, r: u16| {
        put(
            frame,
            r,
            Line::from(Span::styled(
                "─".repeat(iw as usize),
                Style::default().fg(theme.popup_border_fg).bg(bg),
            )),
        );
    };

    // Header.
    put(
        frame,
        0,
        Line::from(vec![Span::styled(
            " ⚠  SECURITY WARNING",
            Style::default()
                .fg(theme.status_warning_indicator_fg)
                .bg(bg)
                .add_modifier(Modifier::BOLD),
        )]),
    );
    separator(frame, 1);

    // Body.
    put(
        frame,
        2,
        Line::from(Span::styled(
            " This project folder can execute arbitrary code:",
            Style::default().fg(fg).bg(bg),
        )),
    );
    let avail = iw.saturating_sub(8) as usize; // " Path: " + margin
    let shown_path = truncate_middle(path, avail.max(8));
    put(
        frame,
        3,
        Line::from(vec![
            Span::styled(" Path: ", Style::default().fg(fg).bg(bg)),
            Span::styled(
                shown_path,
                Style::default().fg(fg).bg(bg).add_modifier(Modifier::BOLD),
            ),
        ]),
    );
    put(
        frame,
        5,
        Line::from(Span::styled(
            " How would you like to proceed?",
            Style::default().fg(fg).bg(bg),
        )),
    );

    // Radio options: each occupies a marker line (clickable) + a description.
    let option_rows = [7u16, 10, 13];
    for (i, opt) in OPTIONS.iter().enumerate() {
        let r = option_rows[i];
        let is_sel = i == selected;
        let marker = if is_sel { "(*)" } else { "( )" };
        let line_style = if is_sel {
            Style::default()
                .fg(theme.popup_selection_fg)
                .bg(theme.popup_selection_bg)
                .add_modifier(Modifier::BOLD)
        } else {
            Style::default().fg(fg).bg(bg)
        };
        // Pad the radio line to full inner width so the selection highlight
        // spans the whole row.
        let text = format!(" {marker} {}", opt.label);
        let padded = pad_to(&text, iw as usize);
        put(frame, r, Line::from(Span::styled(padded, line_style)));
        put(
            frame,
            r + 1,
            Line::from(Span::styled(
                format!("      {}", opt.description),
                Style::default().fg(fg).bg(bg).add_modifier(Modifier::DIM),
            )),
        );
        layout.radios[i] = row_rect(r);
    }

    // Footer: separator + buttons.
    separator(frame, 16);
    let (ok_rect, quit_rect) = render_buttons(frame, row_rect(17), quit_hint, bg, fg);
    layout.ok = ok_rect;
    layout.quit = quit_rect;

    layout
}

fn render_buttons(
    frame: &mut Frame,
    row: Rect,
    quit_hint: Option<&str>,
    bg: ratatui::style::Color,
    fg: ratatui::style::Color,
) -> (Rect, Rect) {
    let ok_label = "[ OK ]".to_string();
    let quit_label = match quit_hint {
        Some(k) => format!("[ Quit ({k}) ]"),
        None => "[ Quit ]".to_string(),
    };
    let ok_w = ok_label.chars().count() as u16;
    let quit_w = quit_label.chars().count() as u16;
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

/// Right-pad `s` with spaces to `width` display columns (no truncation here;
/// callers pass text known to fit).
fn pad_to(s: &str, width: usize) -> String {
    let len = s.chars().count();
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
