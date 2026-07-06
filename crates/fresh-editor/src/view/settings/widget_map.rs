//! Settings control → `WidgetSpec` mapping (Phase 3 of the
//! Settings↔widget unification).
//!
//! This is the load-bearing step of the unification: it turns a
//! `SettingControl` (the Settings UI's own control model, backed by the
//! `view/controls/` ratatui library) into a `WidgetSpec` node from the
//! plugin-facing widget framework. Once Settings renders the resulting
//! tree through `widgets::render_spec`, the two frameworks are one.
//!
//! The scalar controls map onto the widget kinds directly:
//!
//! | `SettingControl` | `WidgetSpec` |
//! |---|---|
//! | `Toggle`   | `Toggle` |
//! | `Number`   | `Number` (integer, or `percent` for float-as-percent) |
//! | `Dropdown` | `Dropdown` |
//! | `Text`     | `Text` (single-line) |
//! | `DualList` | `DualList` |
//!
//! The composite controls compose generic widget primitives fed by
//! **domain-formatted** content — the robust shape: domain code (here,
//! the `view/controls` formatters) produces the row text, and a generic
//! widget renders + navigates it, so nothing re-implements rendering.
//!
//! * `Map` / `ObjectArray` → a label (+ `Name │ <col>` header for maps)
//!   and a generic `List`; rows come from `MapState::get_display_value`
//!   / `format_key_combo`, and selection is seeded from the control's
//!   focused entry so the List paints the highlight, `[Enter to edit]`,
//!   and Up/Down navigation. The `List` is the same primitive plugins
//!   use — one renderer, no duplication.
//! * `TextList` → a `Col` of a label, item rows, and an add row.
//! * `Json` → a multi-line `Text`.
//! * `Complex` → a labelled `Raw` (uneditable).
//!
//! Editing still runs through the settings input path — this migrates
//! the *view*; the nested entry-editing surfaces move onto floating
//! widget panels in a later step.

use super::items::{SettingControl, SettingItem};
use crate::view::controls::keybinding_list::format_key_combo;
use crate::view::controls::FocusState;
use fresh_core::api::{DualListOption, OverlayColorSpec, OverlayOptions, WidgetSpec};
use fresh_core::text_property::{InlineOverlay, OffsetUnit, StyledSegment, TextPropertyEntry};

/// Accent color for the "key" column (key combo / map key). Matches the
/// widget framework's help-key accent.
const ACCENT_KEY: &str = "ui.help_key_fg";
/// Color for the "value" column (action / display value).
const ACCENT_VALUE: &str = "ui.tab_active_fg";

/// Map one Settings control to a `WidgetSpec` node, keyed by the
/// setting's stable identifier (its JSON-pointer path) so the widget
/// runtime preserves instance state across re-renders.
pub fn setting_control_to_widget(field_key: &str, control: &SettingControl) -> WidgetSpec {
    setting_control_to_widget_aligned(field_key, control, None)
}

/// Like [`setting_control_to_widget`], with an optional label column
/// width so a page of scalar controls aligns their value cells (the
/// Settings dialog computes the max label width per page).
pub fn setting_control_to_widget_aligned(
    field_key: &str,
    control: &SettingControl,
    label_width: Option<u16>,
) -> WidgetSpec {
    let key = Some(field_key.to_string());
    let lw = label_width.unwrap_or(0) as u32;
    match control {
        // Form layout (`label: [v]`, chip-only hit) to match the
        // Settings dialog's historical toggle; an inherited/unset value
        // renders the neutral `[-]` chip (issue #2345).
        SettingControl::Toggle(s) => WidgetSpec::Toggle {
            checked: s.checked,
            label: s.label.clone(),
            focused: false,
            indeterminate: s.inherited,
            label_first: true,
            label_width: lw,
            key,
        },
        SettingControl::Number(s) => {
            // `NumberInputState` stores a percentage as the already
            // ×100 integer (0.25 → 25) and divides by 100 when writing
            // JSON. The `Number` widget stores the raw value and
            // multiplies by 100 only for display, so undo the ×100 here.
            let scale = if s.is_percentage { 100.0 } else { 1.0 };
            // In-place edit state: while the control's `editor` exists,
            // the cell renders the typed buffer with caret + selection
            // instead of the formatted value.
            let (edit_text, edit_cursor, edit_sel) = match &s.editor {
                Some(ed) => {
                    let sel = ed
                        .selection_flat_range()
                        .map(|(a, b)| (a as i32, b as i32))
                        .unwrap_or((-1, -1));
                    (Some(ed.value()), ed.flat_cursor_byte() as i32, sel)
                }
                None => (None, -1, (-1, -1)),
            };
            WidgetSpec::Number {
                value: s.value as f64 / scale,
                min: s.min.map(|m| m as f64 / scale),
                max: s.max.map(|m| m as f64 / scale),
                step: (s.step as f64 / scale).max(f64::MIN_POSITIVE),
                integer: !s.is_percentage,
                percent: s.is_percentage,
                label: s.label.clone(),
                focused: false,
                label_width: lw,
                edit_text,
                edit_cursor,
                edit_sel_start: edit_sel.0,
                edit_sel_end: edit_sel.1,
                key,
            }
        }
        SettingControl::Dropdown(s) => WidgetSpec::Dropdown {
            options: s.options.clone(),
            selected_index: s.selected as i32,
            label: s.label.clone(),
            focused: false,
            label_width: lw,
            open: s.open,
            scroll_offset: s.scroll_offset as u32,
            key,
        },
        SettingControl::Text(s) => WidgetSpec::Text {
            value: s.value.clone(),
            // While editing, carry the caret (`cursor` is a byte
            // offset) and mark the field focused so the renderer
            // paints the block caret (`block_caret`) where typing
            // lands.
            cursor_byte: if s.editing {
                s.cursor.min(s.value.len()) as i32
            } else {
                -1
            },
            focused: s.editing,
            label: s.label.clone(),
            placeholder: if s.placeholder.is_empty() {
                None
            } else {
                Some(s.placeholder.clone())
            },
            rows: 1,
            field_width: 0,
            max_visible_chars: 0,
            full_width: true,
            completions: Vec::new(),
            completions_visible_rows: 0,
            block_caret: true,
            key,
        },
        SettingControl::DualList(s) => WidgetSpec::DualList {
            options: s
                .all_options
                .iter()
                .map(|(value, label)| DualListOption {
                    value: value.clone(),
                    label: label.clone(),
                })
                .collect(),
            included: s.included.clone(),
            excluded: s.excluded.clone(),
            label: s.label.clone(),
            focused: false,
            visible_rows: 6,
            key,
        },
        // String-list editor: label, one bracketed cell + `[x]` delete
        // button per committed item, and a trailing add row that flips
        // between `[+] Add new` and a live input box (placeholder +
        // block caret + inline hints) — matching the historical
        // Settings TextList renderer row for row. Editing (add /
        // remove / reorder) still runs through the settings input
        // path; the rows faithfully project its state.
        SettingControl::TextList(s) => {
            let focused = s.focus == FocusState::Focused;
            let mut children = Vec::with_capacity(s.items.len() + 2);
            children.push(raw_row(format!("{}:", s.label)));
            for (idx, it) in s.items.iter().enumerate() {
                let row_focused = focused && s.focused_item == Some(idx);
                let mut segs = vec![
                    seg("  [", None),
                    seg(&pad(it, TEXTLIST_CELL_WIDTH), None),
                    seg("] ", None),
                    seg("[x]", Some(ACCENT_VALUE)),
                ];
                if row_focused {
                    segs.push(seg("  Del:remove  Enter:edit", Some(DIM_HINT)));
                }
                children.push(raw_entry_row(segments_row(segs)));
            }
            children.push(text_list_add_row(s, focused));
            WidgetSpec::Col { children, key }
        }
        // Key→value map (e.g. Languages, LSP servers). Label, a dimmed
        // `Name  <Col>` header when the control names a display field,
        // and a generic `List` whose rows are formatted by the domain
        // helper `get_display_value` (padded key column + truncated
        // preview), with selection seeded from the control's focused
        // entry so the List paints the highlight, `[Enter to edit]`,
        // and Up/Down navigation. Domain code formats rows; the
        // generic List renders and navigates them.
        SettingControl::Map(s) => {
            let display_title = s.display_field.as_deref().map(column_title);
            let key_width = 20usize;
            let rows: Vec<TextPropertyEntry> = s
                .entries
                .iter()
                .enumerate()
                .map(|(idx, (k, v))| {
                    let focused = s.focus == FocusState::Focused && s.focused_entry == Some(idx);
                    let preview = truncate_chars(&s.get_display_value(v), 20);
                    let mut segs = vec![
                        seg("  ", None),
                        seg(&pad(k, key_width), Some(ACCENT_KEY)),
                        seg(" ", None),
                        seg(&preview, Some(ACCENT_VALUE)),
                    ];
                    if focused {
                        segs.push(seg("  [Enter to edit]", Some(DIM_HINT)));
                    }
                    segments_row(segs)
                })
                .collect();
            let selected = list_selection(s.focus, s.focused_entry);
            let mut children = vec![raw_row(format!("{}:", s.label))];
            if let Some(title) = display_title {
                children.push(header_row(&pad("Name", key_width), &title));
            }
            // An EMPTY List still pads one blank row (its virtual
            // viewport is min 1 tall), which `control_height` doesn't
            // count — the extra row pushes the add row below the
            // control's clipped band and it never renders. Skip the
            // List entirely when there are no entries.
            if !rows.is_empty() {
                children.push(list_of(field_key, rows, selected));
            }
            if !s.no_add {
                let add_focused = s.focus == FocusState::Focused && s.focused_entry.is_none();
                children.push(add_new_row(add_focused, "  [Enter to add]"));
            }
            WidgetSpec::Col { children, key }
        }
        // Object array (keybindings, LSP server lists). Rows are
        // formatted by the domain helpers: a `combo → action` pair for
        // keybinding-shaped entries, collapsing to just the display
        // value when the combo column is empty (LSP servers and other
        // non-keybinding arrays). The `> ` indicator marks the focused
        // row, mirroring the historical renderer; the generic `List`
        // supplies the highlight + navigation.
        SettingControl::ObjectArray(s) => {
            // `display_field` is a JSON pointer (`/command`); the
            // lookup key is the bare field name.
            let field = s
                .display_field
                .as_deref()
                .and_then(|p| p.strip_prefix('/'))
                .or(s.display_field.as_deref())
                .unwrap_or("action");
            let combo_width = 20usize;
            let focused_ctl = s.focus == FocusState::Focused;
            let rows: Vec<TextPropertyEntry> = s
                .bindings
                .iter()
                .enumerate()
                .map(|(idx, b)| {
                    let combo = format_key_combo(b);
                    let action = b
                        .get(field)
                        .and_then(|v| v.as_str())
                        .unwrap_or("(no action)");
                    let row_focused = focused_ctl && s.focused_index == Some(idx);
                    let indicator = if row_focused { "> " } else { "  " };
                    let segs = if combo.trim().is_empty() {
                        vec![seg(indicator, None), seg(action, Some(ACCENT_VALUE))]
                    } else {
                        vec![
                            seg(indicator, None),
                            seg(&pad(&combo, combo_width), Some(ACCENT_KEY)),
                            seg(" → ", None),
                            seg(action, Some(ACCENT_VALUE)),
                        ]
                    };
                    segments_row(segs)
                })
                .collect();
            let selected = list_selection(s.focus, s.focused_index);
            let add_focused = focused_ctl && s.focused_index.is_none();
            let mut children = vec![raw_row(format!("{}:", s.label))];
            // Skip the empty List — its 1-row minimum viewport would
            // push the add row out of the control band (see Map arm).
            if !rows.is_empty() {
                children.push(list_of(field_key, rows, selected));
            }
            children.push(add_new_row(add_focused, ""));
            WidgetSpec::Col { children, key }
        }
        // Multiline JSON editor: label, a `│`-bordered line box showing
        // the editor's text with selection highlight + block caret, an
        // `⚠ Invalid JSON` row when the buffer doesn't parse, and a
        // muted placeholder when the value is unset. Editing (and JSON
        // validation) runs through the settings input path against the
        // control's `TextEdit`; the rows faithfully project its state.
        SettingControl::Json(s) => {
            let mut children = vec![raw_row(format!("{}:", s.label))];
            if s.is_unset() {
                children.push(raw_entry_row(segments_row(vec![
                    seg("  ", None),
                    seg("(not set — press Enter to add)", Some(DIM_HINT)),
                ])));
                return WidgetSpec::Col { children, key };
            }
            let focused = s.focus == FocusState::Focused;
            let border_key = if !s.is_valid() {
                "diagnostic.error_fg"
            } else if focused {
                "ui.menu_highlight_fg"
            } else {
                "ui.split_separator_fg"
            };
            let lines = s.lines();
            let inner_width = lines
                .iter()
                .map(|l| l.chars().count())
                .max()
                .unwrap_or(0)
                .max(20)
                + 2;
            let selection = if focused { s.selection_range() } else { None };
            let (cursor_row, cursor_col) = s.cursor_pos();
            for (line_idx, line) in lines.iter().enumerate() {
                children.push(json_line_row(
                    line,
                    line_idx,
                    inner_width,
                    border_key,
                    selection,
                    focused.then_some((cursor_row, cursor_col)),
                ));
            }
            if !s.is_valid() {
                children.push(raw_entry_row(segments_row(vec![seg(
                    "  ⚠ Invalid JSON",
                    Some("diagnostic.warning_fg"),
                )])));
            }
            WidgetSpec::Col { children, key }
        }
        SettingControl::Complex { type_name } => WidgetSpec::Raw {
            entries: vec![TextPropertyEntry::text(format!(
                "{field_key}: <{type_name} - edit in config.toml>"
            ))],
            key: Some(field_key.to_string()),
        },
    }
}

/// Inner width of a TextList item/input cell (between its brackets).
/// Matches the historical `field_width = 30` minus the two brackets.
const TEXTLIST_CELL_WIDTH: usize = 28;

/// Dim hint / disabled-text color.
const DIM_HINT: &str = "ui.menu_disabled_fg";

/// The TextList's trailing add row, in its three historical states:
/// a live input box (with placeholder, block caret and `Enter:add
/// Esc:cancel` hints) once the user starts adding; a focused
/// `[+] Add new` with a "press Enter" hint; or the plain label.
fn text_list_add_row(s: &crate::view::controls::TextListState, focused: bool) -> WidgetSpec {
    let add_focused = focused && s.focused_item.is_none();
    let show_input = add_focused && (s.pending_active || !s.new_item_text.is_empty());
    if show_input {
        let mut text = String::from("  [");
        let cell_start = text.len();
        let caret_byte = if s.new_item_text.is_empty() {
            None
        } else {
            // `cursor` is a char index into the buffer; the caret
            // overlay wants bytes.
            Some(
                cell_start
                    + s.new_item_text
                        .char_indices()
                        .nth(s.cursor)
                        .map(|(b, _)| b)
                        .unwrap_or(s.new_item_text.len()),
            )
        };
        let (cell_text, dim) = if s.new_item_text.is_empty() {
            (pad("type new item", TEXTLIST_CELL_WIDTH), true)
        } else {
            (pad(&s.new_item_text, TEXTLIST_CELL_WIDTH), false)
        };
        text.push_str(&cell_text);
        text.push_str("] ");
        text.push_str("[+]");
        text.push_str("  Enter:add  Esc:cancel");
        let mut entry = TextPropertyEntry::text(&text);
        if dim {
            entry.inline_overlays.push(InlineOverlay {
                start: cell_start,
                end: cell_start + cell_text.len(),
                style: OverlayOptions {
                    fg: Some(OverlayColorSpec::theme_key(DIM_HINT)),
                    italic: true,
                    ..Default::default()
                },
                properties: Default::default(),
                unit: OffsetUnit::Byte,
            });
        }
        if let Some(b) = caret_byte {
            let ch_len = entry.text[b..]
                .chars()
                .next()
                .map(|c| c.len_utf8())
                .unwrap_or(1);
            entry.inline_overlays.push(InlineOverlay {
                start: b,
                end: b + ch_len,
                style: OverlayOptions {
                    reversed: true,
                    ..Default::default()
                },
                properties: Default::default(),
                unit: OffsetUnit::Byte,
            });
        }
        WidgetSpec::Raw {
            entries: vec![entry],
            key: None,
        }
    } else if add_focused {
        add_new_row(true, "  press Enter (or type) to add a new item")
    } else {
        add_new_row(false, "")
    }
}

/// An `  [+] Add new` row, with an optional dim hint when focused.
fn add_new_row(focused: bool, hint: &str) -> WidgetSpec {
    let mut segs = vec![seg("  ", None), seg("[+] Add new", Some(ACCENT_VALUE))];
    if focused && !hint.is_empty() {
        segs.push(seg(hint, Some(DIM_HINT)));
    }
    raw_entry_row(segments_row(segs))
}

/// One bordered JSON-editor line: `  │{padded line}│` with an optional
/// selection highlight and block caret (both computed from the
/// editor's (row, char-col) coordinates).
fn json_line_row(
    line: &str,
    line_idx: usize,
    inner_width: usize,
    border_key: &str,
    selection: Option<((usize, usize), (usize, usize))>,
    cursor: Option<(usize, usize)>,
) -> WidgetSpec {
    let mut text = String::from("  ");
    let border_a = text.len();
    text.push_str("│");
    let content_start = text.len();
    let padded = pad(line, inner_width);
    text.push_str(&padded);
    let border_b = text.len();
    text.push_str("│");
    let mut entry = TextPropertyEntry::text(&text);
    for (s, e) in [(border_a, content_start), (border_b, border_b + "│".len())] {
        entry.inline_overlays.push(InlineOverlay {
            start: s,
            end: e,
            style: OverlayOptions {
                fg: Some(OverlayColorSpec::theme_key(border_key)),
                ..Default::default()
            },
            properties: Default::default(),
            unit: OffsetUnit::Byte,
        });
    }
    // Char-col → byte-offset within the padded content.
    let col_to_byte = |col: usize| -> usize {
        padded
            .char_indices()
            .nth(col)
            .map(|(b, _)| b)
            .unwrap_or(padded.len())
    };
    if let Some(((sr, sc), (er, ec))) = selection {
        // The selected span of THIS line: full line when strictly
        // inside the range, partial on the boundary rows.
        if line_idx >= sr && line_idx <= er {
            let from = if line_idx == sr { sc } else { 0 };
            let to = if line_idx == er {
                ec
            } else {
                line.chars().count()
            };
            if to > from {
                entry.inline_overlays.push(InlineOverlay {
                    start: content_start + col_to_byte(from),
                    end: content_start + col_to_byte(to),
                    style: OverlayOptions {
                        bg: Some(OverlayColorSpec::theme_key("ui.selection_bg")),
                        ..Default::default()
                    },
                    properties: Default::default(),
                    unit: OffsetUnit::Byte,
                });
            }
        }
    }
    if let Some((cr, cc)) = cursor {
        if cr == line_idx {
            let b = content_start + col_to_byte(cc.min(padded.chars().count().saturating_sub(1)));
            let ch_len = text[b..].chars().next().map(|c| c.len_utf8()).unwrap_or(1);
            entry.inline_overlays.push(InlineOverlay {
                start: b,
                end: b + ch_len,
                style: OverlayOptions {
                    reversed: true,
                    ..Default::default()
                },
                properties: Default::default(),
                unit: OffsetUnit::Byte,
            });
        }
    }
    WidgetSpec::Raw {
        entries: vec![entry],
        key: None,
    }
}

/// Truncate to `max` chars, appending `…` when shortened (settings map
/// previews have always clipped at 20 columns).
fn truncate_chars(s: &str, max: usize) -> String {
    let n = s.chars().count();
    if n <= max {
        s.to_string()
    } else {
        let mut out: String = s.chars().take(max.saturating_sub(1)).collect();
        out.push('…');
        out
    }
}

/// A single-row `Raw` widget from a pre-built entry.
fn raw_entry_row(entry: TextPropertyEntry) -> WidgetSpec {
    WidgetSpec::Raw {
        entries: vec![entry],
        key: None,
    }
}

/// A single-row `Raw` widget from a plain string.
fn raw_row(text: String) -> WidgetSpec {
    WidgetSpec::Raw {
        entries: vec![TextPropertyEntry::text(text)],
        key: None,
    }
}

/// A styled segment with an optional theme-key foreground.
fn seg(text: &str, fg_key: Option<&str>) -> StyledSegment {
    StyledSegment {
        text: text.to_string(),
        style: fg_key.map(|k| OverlayOptions {
            fg: Some(OverlayColorSpec::theme_key(k)),
            ..Default::default()
        }),
        overlays: Vec::new(),
    }
}

/// A `TextPropertyEntry` row built from styled segments (the host
/// concatenates them into text + overlays at render time).
fn segments_row(segments: Vec<StyledSegment>) -> TextPropertyEntry {
    TextPropertyEntry {
        segments,
        ..TextPropertyEntry::text("")
    }
}

/// Left-pad `s` to `width` display columns (char-approximate).
fn pad(s: &str, width: usize) -> String {
    let n = s.chars().count();
    if n >= width {
        s.to_string()
    } else {
        let mut out = s.to_string();
        out.extend(std::iter::repeat(' ').take(width - n));
        out
    }
}

/// A dimmed two-column header row (`Name │ <title>`).
fn header_row(left: &str, right: &str) -> WidgetSpec {
    WidgetSpec::Raw {
        entries: vec![segments_row(vec![
            seg("  ", None),
            seg(left, Some("ui.menu_disabled_fg")),
            seg(right, Some("ui.menu_disabled_fg")),
        ])],
        key: None,
    }
}

/// Wrap pre-formatted styled rows in a generic virtual-scrolled `List`
/// (host-owned selection + navigation). `selected` is the absolute
/// index to highlight (`-1` for none). `visible_rows` covers the whole
/// set — the settings viewport does the outer scroll/clipping.
fn list_of(field_key: &str, rows: Vec<TextPropertyEntry>, selected: i32) -> WidgetSpec {
    let visible = rows.len().max(1) as u32;
    WidgetSpec::List {
        items: rows,
        item_specs: Vec::new(),
        item_keys: Vec::new(),
        selected_index: selected,
        visible_rows: visible,
        focusable: true,
        key: Some(format!("{field_key}::list")),
    }
}

/// The List's selected index: the control's focused entry when the
/// control is focused, else `-1` (no highlight).
fn list_selection(focus: FocusState, focused: Option<usize>) -> i32 {
    if focus == FocusState::Focused {
        focused.map(|i| i as i32).unwrap_or(-1)
    } else {
        -1
    }
}

/// Human column title from a `display_field` pointer (`/grammar` →
/// `Grammar`).
fn column_title(display_field: &str) -> String {
    let last = display_field.rsplit('/').next().unwrap_or(display_field);
    let mut chars = last.chars();
    match chars.next() {
        Some(c) => c.to_uppercase().collect::<String>() + chars.as_str(),
        None => String::new(),
    }
}

/// Map a whole settings page — an ordered list of `SettingItem`s — into
/// a single `Col` of control widgets, inserting a section header (and a
/// divider between sections) at each `is_section_start` boundary. This
/// is the tree Settings hands to `widgets::render_spec` once it renders
/// through the widget framework.
pub fn settings_items_to_widget(items: &[SettingItem]) -> WidgetSpec {
    let mut children: Vec<WidgetSpec> = Vec::with_capacity(items.len());
    for item in items {
        if item.is_section_start {
            if let Some(section) = item.section.as_deref() {
                if !children.is_empty() {
                    children.push(WidgetSpec::Divider {
                        ch: "─".to_string(),
                        style: None,
                        key: None,
                    });
                }
                children.push(section_header(section));
            }
        }
        children.push(setting_control_to_widget(&item.path, &item.control));
    }
    WidgetSpec::Col {
        children,
        key: Some("settings-page".to_string()),
    }
}

/// A styled section-header row (`Raw`, accent fg + bold).
fn section_header(section: &str) -> WidgetSpec {
    let mut entry = TextPropertyEntry::text(section);
    entry.inline_overlays.push(InlineOverlay {
        start: 0,
        end: section.len(),
        style: OverlayOptions {
            fg: Some(OverlayColorSpec::theme_key("ui.help_key_fg")),
            bold: true,
            ..Default::default()
        },
        properties: Default::default(),
        unit: OffsetUnit::Byte,
    });
    WidgetSpec::Raw {
        entries: vec![entry],
        key: None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::view::controls::{
        DropdownState, DualListState, NumberInputState, TextInputState, ToggleState,
    };

    #[test]
    fn toggle_maps_to_toggle_widget() {
        let s = ToggleState::new(true, "Word wrap");
        match setting_control_to_widget("/editor/word_wrap", &SettingControl::Toggle(s)) {
            WidgetSpec::Toggle {
                checked,
                label,
                key,
                ..
            } => {
                assert!(checked);
                assert_eq!(label, "Word wrap");
                assert_eq!(key.as_deref(), Some("/editor/word_wrap"));
            }
            other => panic!("expected Toggle, got {other:?}"),
        }
    }

    #[test]
    fn integer_number_maps_directly() {
        let s = NumberInputState {
            value: 4,
            min: Some(1),
            max: Some(16),
            step: 1,
            label: "Tab size".into(),
            focus: Default::default(),
            editor: None,
            is_percentage: false,
        };
        match setting_control_to_widget("/editor/tab_size", &SettingControl::Number(s)) {
            WidgetSpec::Number {
                value,
                min,
                max,
                integer,
                percent,
                ..
            } => {
                assert_eq!(value, 4.0);
                assert_eq!(min, Some(1.0));
                assert_eq!(max, Some(16.0));
                assert!(integer);
                assert!(!percent);
            }
            other => panic!("expected Number, got {other:?}"),
        }
    }

    #[test]
    fn percentage_number_unscales_by_100() {
        let s = NumberInputState {
            value: 25, // stored ×100
            min: Some(0),
            max: Some(100),
            step: 5,
            label: "Opacity".into(),
            focus: Default::default(),
            editor: None,
            is_percentage: true,
        };
        match setting_control_to_widget("/ui/opacity", &SettingControl::Number(s)) {
            WidgetSpec::Number {
                value,
                percent,
                max,
                ..
            } => {
                assert_eq!(value, 0.25);
                assert_eq!(max, Some(1.0));
                assert!(percent);
            }
            other => panic!("expected Number, got {other:?}"),
        }
    }

    #[test]
    fn dropdown_maps_options_and_selection() {
        let s = DropdownState::new(vec!["Light".into(), "Dark".into()], "Theme").with_selected(1);
        match setting_control_to_widget("/ui/theme", &SettingControl::Dropdown(s)) {
            WidgetSpec::Dropdown {
                options,
                selected_index,
                ..
            } => {
                assert_eq!(options, vec!["Light".to_string(), "Dark".to_string()]);
                assert_eq!(selected_index, 1);
            }
            other => panic!("expected Dropdown, got {other:?}"),
        }
    }

    #[test]
    fn text_maps_value_and_placeholder() {
        let s = TextInputState::new("Formatter")
            .with_value("rustfmt")
            .with_placeholder("command");
        match setting_control_to_widget("/fmt/cmd", &SettingControl::Text(s)) {
            WidgetSpec::Text {
                value,
                placeholder,
                rows,
                ..
            } => {
                assert_eq!(value, "rustfmt");
                assert_eq!(placeholder.as_deref(), Some("command"));
                assert_eq!(rows, 1);
            }
            other => panic!("expected Text, got {other:?}"),
        }
    }

    #[test]
    fn dual_list_maps_options_included_excluded() {
        let s = DualListState::new(
            "Status bar",
            vec![
                ("mode".into(), "Mode".into()),
                ("git".into(), "Git".into()),
                ("pos".into(), "Position".into()),
            ],
        )
        .with_included(vec!["mode".into()])
        .with_excluded(vec!["git".into()]);
        match setting_control_to_widget("/statusbar/elements", &SettingControl::DualList(s)) {
            WidgetSpec::DualList {
                options,
                included,
                excluded,
                ..
            } => {
                assert_eq!(options.len(), 3);
                assert_eq!(options[0].value, "mode");
                assert_eq!(options[0].label, "Mode");
                assert_eq!(included, vec!["mode".to_string()]);
                assert_eq!(excluded, vec!["git".to_string()]);
            }
            other => panic!("expected DualList, got {other:?}"),
        }
    }

    fn item(path: &str, control: SettingControl, section: Option<&str>) -> SettingItem {
        SettingItem {
            path: path.into(),
            name: path.into(),
            description: None,
            control,
            default: None,
            modified: false,
            layer_source: crate::config_io::ConfigLayer::System,
            read_only: false,
            is_auto_managed: false,
            nullable: false,
            is_null: false,
            section: section.map(|s| s.to_string()),
            is_section_start: section.is_some(),
            style: Default::default(),
            dual_list_sibling: None,
        }
    }

    #[test]
    fn page_builds_col_with_section_headers_and_dividers() {
        let items = vec![
            item(
                "/editor/word_wrap",
                SettingControl::Toggle(ToggleState::new(true, "Word wrap")),
                Some("Editor"),
            ),
            item(
                "/editor/tab_size",
                SettingControl::Number(NumberInputState {
                    value: 4,
                    min: Some(1),
                    max: Some(8),
                    step: 1,
                    label: "Tab size".into(),
                    focus: Default::default(),
                    editor: None,
                    is_percentage: false,
                }),
                None,
            ),
            item(
                "/ui/theme",
                SettingControl::Dropdown(DropdownState::new(
                    vec!["Light".into(), "Dark".into()],
                    "Theme",
                )),
                Some("UI"),
            ),
        ];
        match settings_items_to_widget(&items) {
            WidgetSpec::Col { children, .. } => {
                // [Editor header][word_wrap][tab_size][divider][UI header][theme]
                assert_eq!(children.len(), 6);
                assert!(matches!(children[1], WidgetSpec::Toggle { .. }));
                assert!(matches!(children[2], WidgetSpec::Number { .. }));
                assert!(matches!(children[3], WidgetSpec::Divider { .. }));
                assert!(matches!(children[5], WidgetSpec::Dropdown { .. }));
            }
            other => panic!("expected Col, got {other:?}"),
        }
    }

    #[test]
    fn mapped_page_renders_coherently_through_widget_runtime() {
        // End-to-end: a settings page maps to a WidgetSpec tree and
        // renders through the *same* `render_spec` the plugin widget
        // framework uses — the render path the Settings swap will adopt.
        use std::collections::HashMap;
        let items = vec![
            item(
                "/editor/word_wrap",
                SettingControl::Toggle(ToggleState::new(true, "Word wrap")),
                Some("Editor"),
            ),
            item(
                "/editor/tab_size",
                SettingControl::Number(NumberInputState {
                    value: 4,
                    min: Some(1),
                    max: Some(8),
                    step: 1,
                    label: "Tab size".into(),
                    focus: Default::default(),
                    editor: None,
                    is_percentage: false,
                }),
                None,
            ),
            item(
                "/ui/theme",
                SettingControl::Dropdown(
                    DropdownState::new(vec!["Light".into(), "Dark".into()], "Theme")
                        .with_selected(1),
                ),
                None,
            ),
        ];
        let tree = settings_items_to_widget(&items);
        let out = crate::widgets::render_spec(&tree, &HashMap::new(), "", u32::MAX);
        let screen: String = out.entries.iter().map(|e| e.text.clone()).collect();
        // Section header, form-layout toggle, number value cell, and
        // dropdown button all present in the rendered text.
        assert!(screen.contains("Editor"), "section header: {screen:?}");
        assert!(screen.contains("Word wrap: [v]"), "toggle: {screen:?}");
        assert!(screen.contains("Tab size: [  4 ]"), "number: {screen:?}");
        assert!(screen.contains("Theme: [Dark  ▼]"), "dropdown: {screen:?}");
    }

    #[test]
    fn complex_control_maps_to_placeholder() {
        let c = SettingControl::Complex {
            type_name: "opaque".into(),
        };
        match setting_control_to_widget("/x", &c) {
            WidgetSpec::Raw { entries, .. } => {
                assert!(entries[0].text.contains("opaque"));
            }
            other => panic!("expected Raw placeholder, got {other:?}"),
        }
    }
}
