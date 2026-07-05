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
    let key = Some(field_key.to_string());
    match control {
        SettingControl::Toggle(s) => WidgetSpec::Toggle {
            checked: s.checked,
            label: s.label.clone(),
            focused: false,
            key,
        },
        SettingControl::Number(s) => {
            // `NumberInputState` stores a percentage as the already
            // ×100 integer (0.25 → 25) and divides by 100 when writing
            // JSON. The `Number` widget stores the raw value and
            // multiplies by 100 only for display, so undo the ×100 here.
            let scale = if s.is_percentage { 100.0 } else { 1.0 };
            WidgetSpec::Number {
                value: s.value as f64 / scale,
                min: s.min.map(|m| m as f64 / scale),
                max: s.max.map(|m| m as f64 / scale),
                step: (s.step as f64 / scale).max(f64::MIN_POSITIVE),
                integer: !s.is_percentage,
                percent: s.is_percentage,
                label: s.label.clone(),
                focused: false,
                key,
            }
        }
        SettingControl::Dropdown(s) => WidgetSpec::Dropdown {
            options: s.options.clone(),
            selected_index: s.selected as i32,
            label: s.label.clone(),
            focused: false,
            key,
        },
        SettingControl::Text(s) => WidgetSpec::Text {
            value: s.value.clone(),
            // While editing, carry the caret (char index → byte) and
            // mark the field focused so the widget renderer highlights
            // it and the host can place the hardware cursor.
            cursor_byte: if s.editing {
                s.value
                    .char_indices()
                    .nth(s.cursor)
                    .map(|(b, _)| b as i32)
                    .unwrap_or(s.value.len() as i32)
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
        // String-list editor: a label header, one row per item, and an
        // "add new" row — matching `SettingControl::control_height`
        // (label + items + add). Editing (add/remove/reorder) still runs
        // through the settings input path; this migrates the *view*.
        SettingControl::TextList(s) => {
            let mut children = Vec::with_capacity(s.items.len() + 2);
            children.push(raw_row(format!("{}:", s.label)));
            for it in &s.items {
                children.push(raw_row(format!("  {it}")));
            }
            children.push(raw_row("  [+] Add new".to_string()));
            WidgetSpec::Col { children, key }
        }
        // Composite controls with nested values / their own editors keep
        // a labelled placeholder for now (Map/ObjectArray carry
        // `serde_json::Value` entries + expansion, Json a `TextEdit`);
        // their faithful migration rides the entry-editor work.
        // Key→value map (e.g. Languages, LSP servers). Rendered as a
        // label + a `Name │ <display>` header + a generic `List` whose
        // rows are formatted by the domain helper `get_display_value`,
        // with selection seeded from the control's focused entry so the
        // List paints the highlight + navigation. This is the robust
        // shape: domain code formats rows, the generic List renders and
        // navigates them (the same primitive plugins use).
        SettingControl::Map(s) => {
            let display_title = s.display_field.as_deref().map(column_title);
            let key_width = 20usize;
            let rows: Vec<TextPropertyEntry> = s
                .entries
                .iter()
                .enumerate()
                .map(|(idx, (k, v))| {
                    let arrow = if s.expanded.contains(&idx) {
                        "▼ "
                    } else {
                        "> "
                    };
                    let focused = s.focus == FocusState::Focused && s.focused_entry == Some(idx);
                    let mut segs = vec![
                        seg(arrow, None),
                        seg(&pad(k, key_width), Some(ACCENT_KEY)),
                        seg("  ", None),
                        seg(&s.get_display_value(v), Some(ACCENT_VALUE)),
                    ];
                    if focused {
                        segs.push(seg("  [Enter to edit]", Some(ACCENT_KEY)));
                    }
                    segments_row(segs)
                })
                .collect();
            let selected = list_selection(s.focus, s.focused_entry);
            let mut children = vec![raw_row(format!("{}:", s.label))];
            if let Some(title) = display_title {
                children.push(header_row(&pad("Name", key_width + 2), &title));
            }
            children.push(list_of(field_key, rows, selected));
            if !s.no_add {
                children.push(raw_row("  [+] Add new".to_string()));
            }
            WidgetSpec::Col { children, key }
        }
        // Object array / keybinding list. Rows are formatted by the
        // domain helper `format_key_combo` (`Alt+= → next_window`); the
        // generic `List` renders + navigates them.
        SettingControl::ObjectArray(s) => {
            let field = s.display_field.as_deref().unwrap_or("action");
            let combo_width = 20usize;
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
                    let focused = s.focus == FocusState::Focused && s.focused_index == Some(idx);
                    let mut segs = vec![
                        seg(&pad(&combo, combo_width), Some(ACCENT_KEY)),
                        seg(" → ", None),
                        seg(action, Some(ACCENT_VALUE)),
                    ];
                    if focused {
                        segs.push(seg("  [Enter to edit]", Some(ACCENT_KEY)));
                    }
                    segments_row(segs)
                })
                .collect();
            let selected = list_selection(s.focus, s.focused_index);
            WidgetSpec::Col {
                children: vec![
                    raw_row(format!("{}:", s.label)),
                    list_of(field_key, rows, selected),
                    raw_row("  [+] Add new".to_string()),
                ],
                key,
            }
        }
        // Multiline JSON editor → a multi-line `Text` showing the
        // editor's current text. Editing still runs through the settings
        // input path against the control's `TextEdit`.
        SettingControl::Json(s) => {
            let value = s.editor.value();
            let rows = (value.lines().count().max(1)).min(20) as u32;
            WidgetSpec::Text {
                value,
                cursor_byte: -1,
                focused: false,
                label: s.label.clone(),
                placeholder: None,
                rows,
                field_width: 0,
                max_visible_chars: 0,
                full_width: true,
                completions: Vec::new(),
                completions_visible_rows: 0,
                key,
            }
        }
        SettingControl::Complex { type_name } => WidgetSpec::Raw {
            entries: vec![TextPropertyEntry::text(format!(
                "{field_key}: <{type_name} - edit in config.toml>"
            ))],
            key: Some(field_key.to_string()),
        },
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
        // Section header, toggle, number stepper, and dropdown cycler
        // all present in the rendered text.
        assert!(screen.contains("Editor"), "section header: {screen:?}");
        assert!(screen.contains("[v] Word wrap"), "toggle: {screen:?}");
        assert!(screen.contains("Tab size ◂ 4 ▸"), "number: {screen:?}");
        assert!(screen.contains("Theme ◂ Dark ▸"), "dropdown: {screen:?}");
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
