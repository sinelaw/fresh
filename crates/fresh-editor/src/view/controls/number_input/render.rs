//! Number input rendering functions

use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::Paragraph;
use ratatui::Frame;

use super::{FocusState, NumberInputColors, NumberInputLayout, NumberInputState};

/// Render a number input control
///
/// # Arguments
/// * `frame` - The ratatui frame to render to
/// * `area` - Rectangle where the control should be rendered
/// * `state` - The number input state
/// * `colors` - Colors for rendering
///
/// # Returns
/// Layout information for hit testing
pub fn render_number_input(
    frame: &mut Frame,
    area: Rect,
    state: &NumberInputState,
    colors: &NumberInputColors,
) -> NumberInputLayout {
    render_number_input_aligned(frame, area, state, colors, None)
}

/// Render a number input control with optional label width alignment
///
/// # Arguments
/// * `frame` - The ratatui frame to render to
/// * `area` - Rectangle where the control should be rendered
/// * `state` - The number input state
/// * `colors` - Colors for rendering
/// * `label_width` - Optional minimum label width for alignment
///
/// # Returns
/// Layout information for hit testing
pub fn render_number_input_aligned(
    frame: &mut Frame,
    area: Rect,
    state: &NumberInputState,
    colors: &NumberInputColors,
    label_width: Option<u16>,
) -> NumberInputLayout {
    if area.height == 0 || area.width < 10 {
        return NumberInputLayout::default();
    }

    let (label_color, value_color, border_color, button_color) = match state.focus {
        FocusState::Normal => (colors.label, colors.value, colors.border, colors.button),
        FocusState::Focused => (
            colors.focused_fg,
            colors.focused_fg,
            colors.focused_fg,
            colors.focused_fg,
        ),
        FocusState::Hovered => (
            colors.focused_fg,
            colors.focused_fg,
            colors.focused_fg,
            colors.focused_fg,
        ),
        FocusState::Disabled => (
            colors.disabled,
            colors.disabled,
            colors.disabled,
            colors.disabled,
        ),
    };

    // Format: "Label: [ value ] [-] [+]"
    let value_str = state.display_text();

    let actual_label_width = label_width.unwrap_or(state.label.len() as u16);
    let padded_label = format!(
        "{:width$}",
        state.label,
        width = actual_label_width as usize
    );

    // Build value spans with cursor and selection support
    let value_spans = if state.editing() {
        build_editing_spans(&value_str, state, value_color, colors)
    } else {
        // Right-align the digits to MIN_WIDTH and append the trailing
        // reserved cell so the visible layout matches editing mode.
        vec![Span::styled(
            format!("{:>width$} ", value_str, width = VALUE_CELL_MIN_WIDTH),
            Style::default().fg(value_color),
        )]
    };

    let mut spans = vec![
        Span::styled(padded_label, Style::default().fg(label_color)),
        Span::styled(": ", Style::default().fg(label_color)),
        Span::styled("[", Style::default().fg(border_color)),
    ];
    spans.extend(value_spans);
    spans.extend(vec![
        Span::styled("]", Style::default().fg(border_color)),
        Span::raw(" "),
        Span::styled("[-]", Style::default().fg(button_color)),
        Span::raw(" "),
        Span::styled("[+]", Style::default().fg(button_color)),
    ]);

    let line = Line::from(spans);

    let paragraph = Paragraph::new(line);
    frame.render_widget(paragraph, area);

    let final_label_width = actual_label_width + 2;
    let value_start = area.x + final_label_width;
    // 2 brackets + MIN_WIDTH digit cells + 1 trailing cursor cell.
    let value_width = (VALUE_CELL_MIN_WIDTH as u16) + 1 + 2;

    let dec_start = value_start + value_width + 1;
    let dec_width = 3;

    let inc_start = dec_start + dec_width + 1;
    let inc_width = 3;

    NumberInputLayout {
        value_area: Rect::new(value_start, area.y, value_width, 1),
        decrement_area: Rect::new(dec_start, area.y, dec_width, 1),
        increment_area: Rect::new(inc_start, area.y, inc_width, 1),
        full_area: Rect::new(area.x, area.y, inc_start - area.x + inc_width, 1),
    }
}

/// Minimum visible width of the digit area (right-aligned). The total
/// inner cell is one cell wider — the trailing reserved cell holds the
/// block cursor when it's at end-of-text, so typing doesn't shove the
/// digits leftward as the cursor advances. Values longer than this
/// width still render in full and grow the cell to the right.
pub(super) const VALUE_CELL_MIN_WIDTH: usize = 3;

/// Build spans for the editing value with cursor and selection highlighting
pub(super) fn build_editing_spans(
    value: &str,
    state: &NumberInputState,
    value_color: ratatui::style::Color,
    colors: &NumberInputColors,
) -> Vec<Span<'static>> {
    let cursor_pos = state.cursor_col();
    let selection_range = state.selection_range();

    let normal_style = Style::default().fg(value_color);
    let cursor_style = Style::default()
        .fg(value_color)
        .add_modifier(Modifier::REVERSED);
    // Use a colour distinct from the row's focus highlight (`colors.focused`),
    // otherwise selecting the value while the row is focused renders as
    // bg-on-bg and the user can't tell the value is selected.
    let selection_style = Style::default().fg(colors.value).bg(colors.selection_bg);

    let chars: Vec<char> = value.chars().collect();
    let cursor_at_end = selection_range.is_none() && cursor_pos >= chars.len();

    // Layout: [leading padding][digits][trailing reserved cell]
    // The trailing cell is always 1 char wide so the digits stay
    // right-aligned at the same column whether the cursor is on a digit
    // or past the last one. It holds the cursor block at end-of-text,
    // and a plain space otherwise.
    let inner_width = (chars.len() + 1).max(VALUE_CELL_MIN_WIDTH + 1);
    let leading = inner_width - chars.len() - 1;

    let mut spans = Vec::new();

    if leading > 0 {
        spans.push(Span::raw(" ".repeat(leading)));
    }

    if let Some((sel_start, sel_end)) = selection_range {
        // Render with selection highlighting
        // Text before selection
        if sel_start > 0 {
            let before: String = chars[..sel_start.min(chars.len())].iter().collect();
            spans.push(Span::styled(before, normal_style));
        }

        // Selected text
        let sel_end_clamped = sel_end.min(chars.len());
        if sel_start < sel_end_clamped {
            let selected: String = chars[sel_start..sel_end_clamped].iter().collect();
            spans.push(Span::styled(selected, selection_style));
        }

        // Text after selection
        if sel_end_clamped < chars.len() {
            let after: String = chars[sel_end_clamped..].iter().collect();
            spans.push(Span::styled(after, normal_style));
        }

        // Trailing reserved cell (no cursor visible during selection)
        spans.push(Span::raw(" "));
    } else if cursor_at_end {
        // Cursor sits in the trailing reserved cell — render every digit
        // normally and place the block cursor in that last cell.
        if !chars.is_empty() {
            spans.push(Span::styled(value.to_string(), normal_style));
        }
        spans.push(Span::styled(" ", cursor_style));
    } else {
        // Cursor on a digit: render before/cursor/after, then the
        // trailing reserved cell as a plain space.
        if cursor_pos > 0 {
            let before: String = chars[..cursor_pos].iter().collect();
            spans.push(Span::styled(before, normal_style));
        }
        let cursor_char = chars[cursor_pos].to_string();
        spans.push(Span::styled(cursor_char, cursor_style));
        if cursor_pos + 1 < chars.len() {
            let after: String = chars[cursor_pos + 1..].iter().collect();
            spans.push(Span::styled(after, normal_style));
        }
        spans.push(Span::raw(" "));
    }

    spans
}
