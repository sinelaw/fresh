//! Event debug dialog rendering
//!
//! Renders the event debug dialog modal overlay.

use crate::app::event_debug::EventDebug;
use crate::view::theme::Theme;
use ratatui::{
    layout::{Constraint, Layout, Rect},
    style::{Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, Clear, Paragraph, Wrap},
    Frame,
};
use rust_i18n::t;

/// Width of the dialog in characters
const DIALOG_WIDTH: u16 = 70;
/// Height of the dialog
const DIALOG_HEIGHT: u16 = 18;

/// Render the event debug dialog overlay
pub fn render_event_debug(frame: &mut Frame, area: Rect, debug: &EventDebug, theme: &Theme) {
    // Calculate dialog dimensions
    let dialog_height = DIALOG_HEIGHT.min(area.height.saturating_sub(4));
    let dialog_width = DIALOG_WIDTH.min(area.width.saturating_sub(4));

    // Center the dialog
    let dialog_x = (area.width.saturating_sub(dialog_width)) / 2;
    let dialog_y = (area.height.saturating_sub(dialog_height)) / 2;

    let dialog_area = Rect {
        x: dialog_x,
        y: dialog_y,
        width: dialog_width,
        height: dialog_height,
    };

    // Clear the area behind the dialog
    frame.render_widget(Clear, dialog_area);

    // Create the outer block
    let block = Block::default()
        .title(t!("event_debug.title").to_string())
        .borders(Borders::ALL)
        .border_style(Style::default().fg(theme.editor_fg))
        .style(Style::default().bg(theme.editor_bg).fg(theme.editor_fg));

    let inner_area = block.inner(dialog_area);
    frame.render_widget(block, dialog_area);

    // Layout: instructions at top, event history in middle, controls at bottom
    let chunks = Layout::vertical([
        Constraint::Length(3), // Instructions
        Constraint::Min(8),    // Event history
        Constraint::Length(4), // Controls/details
    ])
    .split(inner_area);

    // Instructions
    let instructions = vec![
        Line::from(vec![Span::styled(
            t!("event_debug.instructions").to_string(),
            Style::default().add_modifier(Modifier::BOLD),
        )]),
        Line::from(t!("event_debug.help_text").to_string()),
    ];

    let instructions_para = Paragraph::new(instructions)
        .style(Style::default().fg(theme.editor_fg))
        .wrap(Wrap { trim: true });
    frame.render_widget(instructions_para, chunks[0]);

    // Event history
    let mut history_lines: Vec<Line> = Vec::new();

    if debug.history.is_empty() {
        history_lines.push(Line::from(vec![Span::styled(
            t!("event_debug.no_events").to_string(),
            Style::default().fg(theme.line_number_fg),
        )]));
    } else {
        history_lines.push(Line::from(vec![
            Span::styled(
                t!("event_debug.recent_events").to_string(),
                Style::default()
                    .fg(theme.help_key_fg)
                    .add_modifier(Modifier::BOLD),
            ),
            Span::raw(format!(" ({})", debug.history.len())),
        ]));
        history_lines.push(Line::from(""));

        for (i, recorded) in debug.history.iter().enumerate() {
            let style = if i == 0 {
                Style::default()
                    .fg(theme.diagnostic_info_fg)
                    .add_modifier(Modifier::BOLD)
            } else {
                Style::default().fg(theme.editor_fg)
            };

            let prefix = if i == 0 { "> " } else { "  " };
            history_lines.push(Line::from(vec![
                Span::styled(prefix, style),
                Span::styled(&recorded.description, style),
            ]));
        }
    }

    let history_para = Paragraph::new(history_lines).style(Style::default().fg(theme.editor_fg));
    frame.render_widget(history_para, chunks[1]);

    // Controls and last event details
    let mut control_lines = vec![Line::from(vec![
        Span::styled("[q]", Style::default().fg(theme.help_key_fg)),
        Span::raw(" "),
        Span::raw(t!("event_debug.close").to_string()),
        Span::raw("  "),
        Span::styled("[Esc]", Style::default().fg(theme.help_key_fg)),
        Span::raw(" "),
        Span::raw(t!("event_debug.close").to_string()),
        Span::raw("  "),
        Span::styled("[c]", Style::default().fg(theme.help_key_fg)),
        Span::raw(" "),
        Span::raw(t!("event_debug.clear").to_string()),
    ])];

    // Show raw details of last event
    if let Some(details) = debug.last_event_details() {
        control_lines.push(Line::from(""));
        control_lines.push(Line::from(vec![Span::styled(
            details,
            Style::default().fg(theme.line_number_fg),
        )]));
    }

    let controls_para = Paragraph::new(control_lines)
        .style(Style::default().fg(theme.editor_fg))
        .wrap(Wrap { trim: true });
    frame.render_widget(controls_para, chunks[2]);
}
