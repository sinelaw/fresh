//! Calibration wizard rendering
//!
//! Renders the input calibration wizard modal overlay.

use crate::app::calibration_wizard::{
    CalibrationStep, CalibrationWizard, KeyStatus, PendingConfirmation,
};
use crate::view::theme::Theme;
use ratatui::{
    layout::{Constraint, Layout, Rect},
    style::{Modifier, Style},
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

    // Check if we need to show a confirmation dialog
    if wizard.has_pending_confirmation() {
        render_confirmation_dialog(frame, dialog_area, wizard, theme);
        return;
    }

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

/// Render confirmation dialog for destructive actions
fn render_confirmation_dialog(
    frame: &mut Frame,
    area: Rect,
    wizard: &CalibrationWizard,
    theme: &Theme,
) {
    let (title, message, confirm_key, confirm_action, cancel_action) =
        match wizard.pending_confirmation {
            PendingConfirmation::Abort => (
                t!("calibration.confirm_abort_title").to_string(),
                t!("calibration.confirm_abort_message").to_string(),
                "d",
                t!("calibration.action_discard").to_string(),
                t!("calibration.action_cancel").to_string(),
            ),
            PendingConfirmation::Restart => (
                t!("calibration.confirm_restart_title").to_string(),
                t!("calibration.confirm_restart_message").to_string(),
                "r",
                t!("calibration.action_restart").to_string(),
                t!("calibration.action_cancel").to_string(),
            ),
            PendingConfirmation::None => return,
        };

    let block = Block::default()
        .title(title)
        .borders(Borders::ALL)
        .border_style(Style::default().fg(theme.diagnostic_warning_fg))
        .style(Style::default().bg(theme.editor_bg).fg(theme.editor_fg));

    let inner_area = block.inner(area);
    frame.render_widget(block, area);

    let content = vec![
        Line::from(""),
        Line::from(vec![Span::styled(
            message,
            Style::default().add_modifier(Modifier::BOLD),
        )]),
        Line::from(""),
        Line::from(""),
        Line::from(vec![
            Span::styled(
                format!("[{}]", confirm_key),
                Style::default().fg(theme.diagnostic_error_fg),
            ),
            Span::raw(format!(" {} ", confirm_action)),
            Span::styled("[c]", Style::default().fg(theme.help_key_fg)),
            Span::raw(format!(" {}", cancel_action)),
        ]),
    ];

    let para = Paragraph::new(content)
        .style(Style::default().fg(theme.editor_fg))
        .wrap(Wrap { trim: true });
    frame.render_widget(para, inner_area);
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

    // Instructions - group info and prominent "press the key" instruction
    let instructions = vec![
        Line::from(vec![
            Span::raw(format!("{}: ", t!("calibration.group"))),
            Span::styled(group.name, Style::default().fg(theme.help_key_fg)),
        ]),
        Line::from(""),
        Line::from(vec![Span::styled(
            t!("calibration.press_key").to_string(),
            Style::default().add_modifier(Modifier::BOLD),
        )]),
        Line::from(vec![Span::styled(
            format!("  {}", target.name),
            Style::default()
                .fg(theme.diagnostic_warning_fg)
                .add_modifier(Modifier::BOLD),
        )]),
    ];

    let instructions_para = Paragraph::new(instructions)
        .style(Style::default().fg(theme.editor_fg))
        .wrap(Wrap { trim: true });
    frame.render_widget(instructions_para, chunks[0]);

    // Progress - show current group's keys with scrolling
    let mut progress_lines: Vec<Line> = Vec::new();

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
                            .fg(theme.diagnostic_warning_fg)
                            .add_modifier(Modifier::BOLD),
                    )
                } else {
                    (' ', Style::default().fg(theme.line_number_fg))
                }
            }
            KeyStatus::Captured => ('*', Style::default().fg(theme.diagnostic_info_fg)),
            KeyStatus::Skipped => ('-', Style::default().fg(theme.line_number_fg)),
            KeyStatus::Verified => ('v', Style::default().fg(theme.help_key_fg)),
        };

        progress_lines.push(Line::from(vec![
            Span::styled(format!(" {} ", status_char), style),
            Span::styled(t.name, style),
        ]));
    }

    // Add step info at the bottom of progress, in gray
    progress_lines.push(Line::from(""));
    progress_lines.push(Line::from(vec![Span::styled(
        format!("{} {}/{}", t!("calibration.step"), step, total),
        Style::default().fg(theme.line_number_fg),
    )]));

    // Calculate scroll offset to keep current key visible
    // Available height minus footer lines (blank + step info)
    let available_height = chunks[1].height.saturating_sub(2) as usize;
    let scroll_offset = if available_height > 0 && key_idx >= available_height {
        (key_idx - available_height + 1) as u16
    } else {
        0
    };

    let progress_para = Paragraph::new(progress_lines)
        .style(Style::default().fg(theme.editor_fg))
        .scroll((scroll_offset, 0));
    frame.render_widget(progress_para, chunks[1]);

    // Controls - add [b] for back
    let controls = vec![
        Line::from(vec![
            Span::styled("[s]", Style::default().fg(theme.help_key_fg)),
            Span::raw(format!(" {} ", t!("calibration.skip"))),
            Span::styled("[b]", Style::default().fg(theme.help_key_fg)),
            Span::raw(format!(" {} ", t!("calibration.back"))),
            Span::styled("[g]", Style::default().fg(theme.help_key_fg)),
            Span::raw(format!(" {} ", t!("calibration.skip_group"))),
            Span::styled("[a]", Style::default().fg(theme.diagnostic_error_fg)),
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
    let translation_count = wizard.translation_count();

    // If no translations needed, show success message
    if translation_count == 0 {
        render_all_keys_ok(frame, chunks, wizard, theme);
        return;
    }

    let (verified, total) = wizard.verification_progress();

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
                Style::default().fg(theme.diagnostic_info_fg),
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
                KeyStatus::Verified => ('v', Style::default().fg(theme.diagnostic_info_fg)),
                KeyStatus::Captured => (' ', Style::default().fg(theme.diagnostic_warning_fg)),
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
            Span::styled("[y]", Style::default().fg(theme.diagnostic_info_fg)),
            Span::raw(format!(" {} ", t!("calibration.save"))),
            Span::styled("[b]", Style::default().fg(theme.help_key_fg)),
            Span::raw(format!(" {} ", t!("calibration.back"))),
            Span::styled("[r]", Style::default().fg(theme.diagnostic_warning_fg)),
            Span::raw(format!(" {} ", t!("calibration.restart"))),
            Span::styled("[a]", Style::default().fg(theme.diagnostic_error_fg)),
            Span::raw(format!(" {}", t!("calibration.abort"))),
        ]),
        Line::from(""),
        Line::from(wizard.status_message.as_deref().unwrap_or("")),
    ];

    let controls_para = Paragraph::new(controls).style(Style::default().fg(theme.editor_fg));
    frame.render_widget(controls_para, chunks[2]);
}

/// Render success message when all keys work correctly (no translations needed)
fn render_all_keys_ok(
    frame: &mut Frame,
    chunks: &[Rect],
    wizard: &CalibrationWizard,
    theme: &Theme,
) {
    // Success message
    let content = vec![
        Line::from(""),
        Line::from(vec![Span::styled(
            t!("calibration.all_keys_ok_title").to_string(),
            Style::default()
                .fg(theme.diagnostic_info_fg)
                .add_modifier(Modifier::BOLD),
        )]),
        Line::from(""),
        Line::from(t!("calibration.all_keys_ok_message").to_string()),
    ];

    let para = Paragraph::new(content)
        .style(Style::default().fg(theme.editor_fg))
        .wrap(Wrap { trim: true });
    frame.render_widget(para, chunks[0]);

    // Empty middle section
    frame.render_widget(
        Paragraph::new("").style(Style::default().fg(theme.editor_fg)),
        chunks[1],
    );

    // Controls - offer save to clear any previous stale translations
    let controls = vec![
        Line::from(vec![
            Span::styled("[y]", Style::default().fg(theme.diagnostic_info_fg)),
            Span::raw(format!(" {} ", t!("calibration.save"))),
            Span::styled("[a]", Style::default().fg(theme.diagnostic_error_fg)),
            Span::raw(format!(" {}", t!("calibration.abort"))),
        ]),
        Line::from(""),
        Line::from(wizard.status_message.as_deref().unwrap_or("")),
    ];

    let controls_para = Paragraph::new(controls).style(Style::default().fg(theme.editor_fg));
    frame.render_widget(controls_para, chunks[2]);
}
