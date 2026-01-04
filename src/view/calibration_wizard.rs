//! Calibration wizard rendering
//!
//! Renders the input calibration wizard modal overlay.

use crate::app::calibration_wizard::{CalibrationStep, CalibrationWizard, KeyStatus};
use crate::view::theme::Theme;
use ratatui::{
    layout::{Constraint, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, Clear, Paragraph, Wrap},
    Frame,
};
use rust_i18n::t;

/// Width of the wizard dialog in characters
const DIALOG_WIDTH: u16 = 60;
/// Minimum height of the wizard dialog
const MIN_DIALOG_HEIGHT: u16 = 20;

/// Render the calibration wizard overlay
pub fn render_calibration_wizard(
    frame: &mut Frame,
    area: Rect,
    wizard: &CalibrationWizard,
    theme: &Theme,
) {
    // Calculate dialog dimensions
    let dialog_height = MIN_DIALOG_HEIGHT.min(area.height.saturating_sub(4));
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
    let title = match &wizard.step {
        CalibrationStep::Capture { .. } => t!("calibration.title_capture").to_string(),
        CalibrationStep::Verify => t!("calibration.title_verify").to_string(),
    };

    let block = Block::default()
        .title(title)
        .borders(Borders::ALL)
        .border_style(Style::default().fg(theme.editor_fg))
        .style(Style::default().bg(theme.editor_bg).fg(theme.editor_fg));

    let inner_area = block.inner(dialog_area);
    frame.render_widget(block, dialog_area);

    // Layout: instructions at top, progress in middle, controls at bottom
    let chunks = Layout::vertical([
        Constraint::Length(5), // Instructions
        Constraint::Min(8),    // Progress/key list
        Constraint::Length(4), // Controls/status
    ])
    .split(inner_area);

    // Render based on phase
    match &wizard.step {
        CalibrationStep::Capture { group_idx, key_idx } => {
            render_capture_phase(frame, &chunks, wizard, *group_idx, *key_idx, theme);
        }
        CalibrationStep::Verify => {
            render_verify_phase(frame, &chunks, wizard, theme);
        }
    }
}

/// Render the capture phase UI
fn render_capture_phase(
    frame: &mut Frame,
    chunks: &[Rect],
    wizard: &CalibrationWizard,
    group_idx: usize,
    key_idx: usize,
    theme: &Theme,
) {
    let groups = wizard.groups();
    let group = &groups[group_idx];
    let target = &group.targets[key_idx];
    let (step, total) = wizard.current_step_info();

    // Instructions
    let instructions = vec![
        Line::from(vec![Span::styled(
            t!("calibration.press_key").to_string(),
            Style::default().add_modifier(Modifier::BOLD),
        )]),
        Line::from(""),
        Line::from(vec![
            Span::raw(format!("{}: ", t!("calibration.group"))),
            Span::styled(group.name, Style::default().fg(Color::Cyan)),
        ]),
        Line::from(vec![
            Span::raw(format!("{}: ", t!("calibration.key"))),
            Span::styled(
                target.name,
                Style::default()
                    .fg(Color::Yellow)
                    .add_modifier(Modifier::BOLD),
            ),
        ]),
    ];

    let instructions_para = Paragraph::new(instructions)
        .style(Style::default().fg(theme.editor_fg))
        .wrap(Wrap { trim: true });
    frame.render_widget(instructions_para, chunks[0]);

    // Progress - show current group's keys
    let mut progress_lines: Vec<Line> = Vec::new();
    progress_lines.push(Line::from(vec![Span::raw(format!(
        "{} {}/{}",
        t!("calibration.step"),
        step,
        total
    ))]));
    progress_lines.push(Line::from(""));

    // Show keys in current group with their status
    let flat_base = groups[..group_idx]
        .iter()
        .map(|g| g.targets.len())
        .sum::<usize>();

    for (idx, t) in group.targets.iter().enumerate() {
        let flat_idx = flat_base + idx;
        let status = wizard.key_status(flat_idx);
        let (status_char, style) = match status {
            KeyStatus::Pending => {
                if idx == key_idx {
                    (
                        '>',
                        Style::default()
                            .fg(Color::Yellow)
                            .add_modifier(Modifier::BOLD),
                    )
                } else {
                    (' ', Style::default().fg(theme.line_number_fg))
                }
            }
            KeyStatus::Captured => ('*', Style::default().fg(Color::Green)),
            KeyStatus::Skipped => ('-', Style::default().fg(theme.line_number_fg)),
            KeyStatus::Verified => ('v', Style::default().fg(Color::Cyan)),
        };

        progress_lines.push(Line::from(vec![
            Span::styled(format!(" {} ", status_char), style),
            Span::styled(t.name, style),
        ]));
    }

    let progress_para = Paragraph::new(progress_lines).style(Style::default().fg(theme.editor_fg));
    frame.render_widget(progress_para, chunks[1]);

    // Controls
    let controls = vec![
        Line::from(vec![
            Span::styled("[s]", Style::default().fg(Color::Cyan)),
            Span::raw(format!(" {} ", t!("calibration.skip"))),
            Span::styled("[g]", Style::default().fg(Color::Cyan)),
            Span::raw(format!(" {} ", t!("calibration.skip_group"))),
            Span::styled("[a]", Style::default().fg(Color::Red)),
            Span::raw(format!(" {}", t!("calibration.abort"))),
        ]),
        Line::from(""),
        Line::from(wizard.status_message.as_deref().unwrap_or("")),
    ];

    let controls_para = Paragraph::new(controls).style(Style::default().fg(theme.editor_fg));
    frame.render_widget(controls_para, chunks[2]);
}

/// Render the verification phase UI
fn render_verify_phase(
    frame: &mut Frame,
    chunks: &[Rect],
    wizard: &CalibrationWizard,
    theme: &Theme,
) {
    let (verified, total) = wizard.verification_progress();
    let translation_count = wizard.translation_count();

    // Instructions
    let instructions = vec![
        Line::from(vec![Span::styled(
            t!("calibration.verify_title").to_string(),
            Style::default().add_modifier(Modifier::BOLD),
        )]),
        Line::from(""),
        Line::from(t!("calibration.verify_instructions").to_string()),
        Line::from(""),
        Line::from(vec![
            Span::raw(format!("{}: ", t!("calibration.translations"))),
            Span::styled(
                translation_count.to_string(),
                Style::default().fg(Color::Green),
            ),
        ]),
    ];

    let instructions_para = Paragraph::new(instructions)
        .style(Style::default().fg(theme.editor_fg))
        .wrap(Wrap { trim: true });
    frame.render_widget(instructions_para, chunks[0]);

    // Show verification status of captured keys
    let mut status_lines: Vec<Line> = Vec::new();
    status_lines.push(Line::from(vec![Span::raw(format!(
        "{}: {}/{}",
        t!("calibration.verified"),
        verified,
        total
    ))]));
    status_lines.push(Line::from(""));

    // List captured keys with verification status
    for (_group_idx, _, target, status) in wizard.all_key_info() {
        if matches!(status, KeyStatus::Captured | KeyStatus::Verified) {
            let (status_char, style) = match status {
                KeyStatus::Verified => ('v', Style::default().fg(Color::Green)),
                KeyStatus::Captured => (' ', Style::default().fg(Color::Yellow)),
                _ => continue,
            };
            status_lines.push(Line::from(vec![
                Span::styled(format!("[{}] ", status_char), style),
                Span::styled(target.name, style),
            ]));
        }
    }

    let status_para = Paragraph::new(status_lines).style(Style::default().fg(theme.editor_fg));
    frame.render_widget(status_para, chunks[1]);

    // Controls
    let controls = vec![
        Line::from(vec![
            Span::styled("[y]", Style::default().fg(Color::Green)),
            Span::raw(format!(" {} ", t!("calibration.save"))),
            Span::styled("[r]", Style::default().fg(Color::Yellow)),
            Span::raw(format!(" {} ", t!("calibration.restart"))),
            Span::styled("[a]", Style::default().fg(Color::Red)),
            Span::raw(format!(" {}", t!("calibration.abort"))),
        ]),
        Line::from(""),
        Line::from(wizard.status_message.as_deref().unwrap_or("")),
    ];

    let controls_para = Paragraph::new(controls).style(Style::default().fg(theme.editor_fg));
    frame.render_widget(controls_para, chunks[2]);
}
