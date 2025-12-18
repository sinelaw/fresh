//! Keybinding list rendering functions

use ratatui::layout::Rect;
use ratatui::style::{Color, Style};
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
        delete_rects: Vec::new(),
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
        layout.entry_rects.push(entry_area);

        let is_entry_focused = is_focused && state.focused_index == Some(idx);
        let bg = if is_entry_focused {
            colors.focused_bg
        } else {
            Color::Reset
        };

        let key_combo = format_key_combo(binding);
        let action = binding
            .get("action")
            .and_then(|a| a.as_str())
            .unwrap_or("(no action)");

        let indicator = if is_entry_focused { "> " } else { "  " };
        let line = Line::from(vec![
            Span::styled(indicator, Style::default().fg(colors.label_fg).bg(bg)),
            Span::styled(
                format!("{:<20}", key_combo),
                Style::default().fg(colors.key_fg).bg(bg),
            ),
            Span::styled(" → ", Style::default().fg(colors.label_fg).bg(bg)),
            Span::styled(action, Style::default().fg(colors.action_fg).bg(bg)),
            Span::styled(" [x]", Style::default().fg(colors.delete_fg).bg(bg)),
        ]);
        frame.render_widget(Paragraph::new(line), entry_area);

        // Track delete button area
        let delete_x = entry_area.x + entry_area.width.saturating_sub(4);
        layout.delete_rects.push(Rect::new(delete_x, y, 3, 1));
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
            Color::Reset
        };

        let indicator = if is_add_focused { "> " } else { "  " };
        let line = Line::from(vec![
            Span::styled(indicator, Style::default().fg(colors.label_fg).bg(bg)),
            Span::styled("[+] Add new", Style::default().fg(colors.add_fg).bg(bg)),
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
