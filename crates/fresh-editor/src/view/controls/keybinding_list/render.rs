//! Keybinding list rendering functions

use ratatui::layout::Rect;
use ratatui::style::Style;
use ratatui::text::{Line, Span};
use ratatui::widgets::Paragraph;
use ratatui::Frame;

use super::{FocusState, KeybindingListColors, KeybindingListLayout, KeybindingListState};
use serde_json::Value;

/// Render a keybinding list control
pub fn render_keybinding_list(
    frame: &mut Frame,
    area: Rect,
    state: &KeybindingListState,
    colors: &KeybindingListColors,
) -> KeybindingListLayout {
    let mut layout = KeybindingListLayout {
        entry_rects: Vec::new(),
        add_rect: None,
    };

    let is_focused = state.focus == FocusState::Focused;

    // Render label
    let label_line = Line::from(vec![Span::styled(
        format!("{}:", state.label),
        Style::default().fg(colors.label_fg),
    )]);
    frame.render_widget(Paragraph::new(label_line), area);

    // Render entries
    for (idx, binding) in state.bindings.iter().enumerate() {
        let y = area.y + 1 + idx as u16;
        if y >= area.y + area.height {
            break;
        }

        let entry_area = Rect::new(area.x + 2, y, area.width.saturating_sub(2), 1);
        layout.entry_rects.push((idx, entry_area));

        let is_entry_focused = is_focused && state.focused_index == Some(idx);
        let bg = if is_entry_focused {
            colors.focused_bg
        } else {
            colors.row_bg
        };

        let key_combo = format_key_combo(binding);
        // Use display_field from state if available, otherwise default to "action"
        let field_name = state
            .display_field
            .as_ref()
            .and_then(|p| p.strip_prefix('/'))
            .unwrap_or("action");
        let action = binding
            .get(field_name)
            .and_then(|a| a.as_str())
            .unwrap_or("(no action)");

        let indicator = if is_entry_focused { "> " } else { "  " };
        // Use focused_fg for all text when entry is focused for good contrast
        let (indicator_fg, key_fg, arrow_fg, action_fg) = if is_entry_focused {
            (
                colors.focused_fg,
                colors.focused_fg,
                colors.focused_fg,
                colors.focused_fg,
            )
        } else {
            (
                colors.label_fg,
                colors.key_fg,
                colors.label_fg,
                colors.action_fg,
            )
        };
        let line = Line::from(vec![
            Span::styled(indicator, Style::default().fg(indicator_fg).bg(bg)),
            Span::styled(
                format!("{:<20}", key_combo),
                Style::default().fg(key_fg).bg(bg),
            ),
            Span::styled(" → ", Style::default().fg(arrow_fg).bg(bg)),
            Span::styled(action, Style::default().fg(action_fg).bg(bg)),
        ]);
        frame.render_widget(Paragraph::new(line), entry_area);
    }

    // Render add-new row
    let add_y = area.y + 1 + state.bindings.len() as u16;
    if add_y < area.y + area.height {
        let add_area = Rect::new(area.x + 2, add_y, area.width.saturating_sub(2), 1);
        layout.add_rect = Some(add_area);

        let is_add_focused = is_focused && state.focused_index.is_none();
        let bg = if is_add_focused {
            colors.focused_bg
        } else {
            colors.row_bg
        };

        let indicator = if is_add_focused { "> " } else { "  " };
        // Use focused_fg for text when add row is focused
        let (indicator_fg, add_fg) = if is_add_focused {
            (colors.focused_fg, colors.focused_fg)
        } else {
            (colors.label_fg, colors.add_fg)
        };
        let line = Line::from(vec![
            Span::styled(indicator, Style::default().fg(indicator_fg).bg(bg)),
            Span::styled("[+] Add new", Style::default().fg(add_fg).bg(bg)),
        ]);
        frame.render_widget(Paragraph::new(line), add_area);
    }

    layout
}

/// Format a keybinding's key combination for display
pub fn format_key_combo(binding: &Value) -> String {
    // Check for keys array (chord binding) first
    if let Some(keys) = binding.get("keys").and_then(|k| k.as_array()) {
        let parts: Vec<String> = keys
            .iter()
            .map(|k| {
                let mut key_str = String::new();
                if let Some(mods) = k.get("modifiers").and_then(|m| m.as_array()) {
                    for m in mods {
                        if let Some(s) = m.as_str() {
                            key_str.push_str(&capitalize_mod(s));
                            key_str.push('+');
                        }
                    }
                }
                if let Some(key) = k.get("key").and_then(|k| k.as_str()) {
                    key_str.push_str(&capitalize_key(key));
                }
                key_str
            })
            .collect();
        return parts.join(" ");
    }

    // Single key binding
    let mut result = String::new();
    if let Some(mods) = binding.get("modifiers").and_then(|m| m.as_array()) {
        for m in mods {
            if let Some(s) = m.as_str() {
                result.push_str(&capitalize_mod(s));
                result.push('+');
            }
        }
    }
    if let Some(key) = binding.get("key").and_then(|k| k.as_str()) {
        result.push_str(&capitalize_key(key));
    }
    result
}

fn capitalize_mod(s: &str) -> String {
    match s.to_lowercase().as_str() {
        "ctrl" | "control" => "Ctrl".to_string(),
        "alt" => "Alt".to_string(),
        "shift" => "Shift".to_string(),
        "super" | "meta" | "cmd" => "Super".to_string(),
        _ => s.to_string(),
    }
}

fn capitalize_key(s: &str) -> String {
    if s.len() == 1 {
        s.to_uppercase()
    } else {
        let mut chars = s.chars();
        match chars.next() {
            None => String::new(),
            Some(c) => c.to_uppercase().chain(chars).collect(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::view::controls::KeybindingListState;
    use ratatui::backend::TestBackend;
    use ratatui::style::Color;
    use ratatui::Terminal;

    /// Regression test for issue #2033: unfocused keybinding rows used to
    /// paint with `Color::Reset`, which falls back to the host terminal's
    /// default bg — visible as a black band over the cream Settings panel
    /// when the light theme runs on a dark-terminal host. Now the caller
    /// passes a `row_bg` and the cells must adopt it.
    #[test]
    fn unfocused_row_paints_with_caller_supplied_row_bg() {
        let backend = TestBackend::new(50, 5);
        let mut terminal = Terminal::new(backend).unwrap();

        let mut state = KeybindingListState::new("Keys");
        state.bindings = vec![serde_json::json!({"key": "a", "action": "act"})];

        let row_bg = Color::Rgb(232, 238, 245); // light theme popup_bg
        let colors = KeybindingListColors {
            row_bg,
            ..Default::default()
        };

        terminal
            .draw(|frame| {
                let area = Rect::new(0, 0, 50, 5);
                render_keybinding_list(frame, area, &state, &colors);
            })
            .unwrap();

        // Row 1 holds the (only) unfocused entry; column 3 sits inside the
        // entry's rendered span — well past the 2-cell indent and indicator.
        let cell = terminal.backend().buffer().cell((3, 1)).unwrap();
        assert_eq!(
            cell.bg,
            row_bg,
            "unfocused row cell must adopt caller-supplied row_bg, not fall through to Color::Reset"
        );
    }
}
