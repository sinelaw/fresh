//! Settings control → `WidgetSpec` mapping.
//!
//! A `SettingControl` — the settings dialog's model of one setting — is
//! described as a `WidgetSpec` node from the plugin-facing widget
//! framework: the same kinds a plugin's panel is made of paint it and edit
//! it (`view::settings::live`), so the two are one framework.
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
//! The composite controls compose the same widget primitives a plugin's
//! form is made of, fed by **domain-formatted** content — the row text is
//! the model's (`items::map_display_value`, `items::object_array_row`)
//! and a generic widget renders and navigates it, so nothing re-implements
//! rendering.
//!
//! * `Map` / `ObjectArray` → a label (+ `Name │ <col>` header for maps)
//!   and one `List` keyed by the path: an entry per row and the `[+] Add
//!   new` row last. The selection is the surface's cursor, kept in its
//!   store by the `List` kind; the description is told which row it is on
//!   so the row can say `[Enter to edit]`.
//! * `TextList` → a `Col` of a label and one row per item: a `Text` field
//!   keyed `{path}::row::{i}` with a `[x]` `Button` beside it, and the add
//!   row's field keyed `{path}::add` last — all edited by the `Text` kind.
//! * `Json` → a multi-line `Text`.
//! * `Complex` → a labelled `Raw` (uneditable).
//!
//! Editing is the kinds' (`view::settings::live`): a key or a press on a
//! live control goes to its kind against the surface's store, and the
//! events the kind reports are written to the model.

use super::items::{
    json_is_unset, json_is_valid, map_display_value, object_array_row, SettingControl, SettingItem,
};
use fresh_core::api::{ButtonKind, DualListOption, OverlayColorSpec, OverlayOptions, WidgetSpec};
use fresh_core::text_property::{InlineOverlay, OffsetUnit, StyledSegment, TextPropertyEntry};

/// Accent color for the "key" column (key combo / map key). Matches the
/// widget framework's help-key accent and the historical `MapColors::key`.
const ACCENT_KEY: &str = "ui.help_key_fg";
/// Map value-preview column. The historical `MapColors::value_preview`
/// was `line_number_fg` — a mid-gray readable on the popup surface in
/// every bundled theme. (`ui.tab_active_fg` is NOT usable here: themes
/// pair it with the active tab's bright background — high-contrast sets
/// it to pure black — so on the dialog surface it disappears.)
const VALUE_PREVIEW_FG: &str = "editor.line_number_fg";
/// ObjectArray action/display column (historical
/// `KeybindingListColors::action_fg` = `syntax_function`).
const ACTION_FG: &str = "syntax.function";
/// `[x]` remove button (historical `remove_button` =
/// `diagnostic_error_fg`).
const REMOVE_FG: &str = "diagnostic.error_fg";
/// `[+] Add new` rows (historical `add_button` = `diagnostic_info_fg`).
const ADD_FG: &str = "diagnostic.info_fg";
/// Map one Settings control to a `WidgetSpec` node, keyed by the
/// setting's stable identifier (its JSON-pointer path) so the widget
/// runtime preserves instance state across re-renders.
pub fn setting_control_to_widget(field_key: &str, control: &SettingControl) -> WidgetSpec {
    setting_control_to_widget_aligned(field_key, control, None, None)
}

/// The node of a control's description that its kind is handed: the one
/// carrying `key` — the control's own, or one of a text list's rows. A
/// scalar's description *is* that node; a JSON editor's is the text area
/// under its label row; a map's is its `List`. The key names the node, on
/// every surface, so the search is by key rather than by shape. The
/// description itself when nothing in it carries the key.
pub fn live_widget(field_key: &str, control: &SettingControl, key: &str) -> WidgetSpec {
    fn keyed(spec: &WidgetSpec, key: &str) -> Option<WidgetSpec> {
        if spec.key() == Some(key) {
            return Some(spec.clone());
        }
        match spec {
            WidgetSpec::Col { children, .. } | WidgetSpec::Row { children, .. } => {
                children.iter().find_map(|c| keyed(c, key))
            }
            _ => None,
        }
    }
    let spec = setting_control_to_widget(field_key, control);
    keyed(&spec, key).unwrap_or(spec)
}

/// Like [`setting_control_to_widget`], with an optional label column
/// width so a page of scalar controls aligns their value cells (the
/// Settings dialog computes the max label width per page), and the row a
/// map's or an object array's cursor is on — the surface's word, from its
/// store — so the row can say `[Enter to edit]` and the add row can be
/// the one the `List` selects.
pub fn setting_control_to_widget_aligned(
    field_key: &str,
    control: &SettingControl,
    label_width: Option<u16>,
    cursor: Option<usize>,
) -> WidgetSpec {
    let key = Some(field_key.to_string());
    let lw = label_width.unwrap_or(0) as u32;
    match control {
        // Form layout (`label: [v]`, chip-only hit) to match the
        // Settings dialog's historical toggle; an inherited/unset value
        // renders the neutral `[-]` chip (issue #2345).
        SettingControl::Toggle {
            label,
            checked,
            inherited,
        } => WidgetSpec::Toggle {
            checked: *checked,
            label: label.clone(),
            focused: false,
            indeterminate: *inherited,
            label_first: true,
            label_width: lw,
            key,
        },
        // The value as the JSON carries it; the kind formats a percent as
        // `value × 100` and edits it in those units. The draft, when one is
        // open, is the kind's instance state, not the spec's.
        SettingControl::Number {
            label,
            value,
            min,
            max,
            integer,
            percent,
        } => WidgetSpec::Number {
            value: *value,
            min: *min,
            max: *max,
            step: match percent {
                true => 0.01,
                false => 1.0,
            },
            integer: *integer,
            percent: *percent,
            label: label.clone(),
            focused: false,
            label_width: lw,
            key,
        },
        // The selection is the model's; whether the list is up is the
        // kind's instance state (`dropdown::resolve`), so the spec never
        // says `open`.
        SettingControl::Dropdown {
            label,
            options,
            selected,
            ..
        } => WidgetSpec::Dropdown {
            options: options.clone(),
            selected_index: *selected as i32,
            label: label.clone(),
            focused: false,
            label_width: lw,
            open: false,
            scroll_offset: 0,
            key,
        },
        // The value is the model's; the caret and the selection, while the
        // field is being edited, are its editor's in the store
        // (`text::resolve`), and the field paints them only while it is the
        // surface's focused widget.
        SettingControl::Text {
            label,
            value,
            placeholder,
        } => WidgetSpec::Text {
            value: value.clone(),
            cursor_byte: -1,
            focused: false,
            label: label.clone(),
            placeholder: match placeholder.is_empty() {
                true => None,
                false => Some(placeholder.clone()),
            },
            rows: 1,
            field_width: 0,
            max_visible_chars: 0,
            full_width: true,
            completions: Vec::new(),
            completions_visible_rows: 0,
            block_caret: true,
            sel_start: -1,
            sel_end: -1,
            // Align the value cell with the sibling toggles/numbers
            // in the same form column (issue: Text `[` started right
            // after the label instead of at the shared column).
            label_width: lw,
            read_only: false,
            markdown: false,
            key,
        },
        // The included set is the model's; which column the keyboard
        // drives and where its cursors sit are the kind's instance state
        // (`dual_list::resolve`), painted only while the control is the
        // surface's focused widget — outside edit mode the arrows walk
        // the settings list, and a cursor would promise a movement they
        // do not make. The key hint is the surface's word
        // (`dual_list_hint`), filled in by the shell that knows whether
        // the control is selected or live.
        SettingControl::DualList {
            label,
            options,
            included,
            excluded,
        } => WidgetSpec::DualList {
            options: options
                .iter()
                .map(|(value, label)| DualListOption {
                    value: value.clone(),
                    label: label.clone(),
                })
                .collect(),
            included: included.clone(),
            excluded: excluded.clone(),
            label: label.clone(),
            focused: false,
            active_included: false,
            available_cursor: 0,
            included_cursor: 0,
            hint: String::new(),
            // One body row per option this control can show, so its height
            // holds still as items move between its columns.
            visible_rows: options
                .iter()
                .filter(|(v, _)| !excluded.contains(v))
                .count() as u32,
            key,
        },
        // A list of strings is its rows as fields: one text field per item,
        // keyed `{path}::row::{i}` with its `[x]` beside it, and an add row
        // keyed `{path}::add` — all edited by the `Text` kind, the live one
        // painting its own caret. Which row is live is the store's word, so
        // the description is the same whichever it is.
        SettingControl::TextList { label, items, .. } => {
            let mut children = vec![raw_row(format!("{label}:"))];
            children.extend(
                items
                    .iter()
                    .enumerate()
                    .map(|(i, item)| text_list_row(field_key, Some(i), item)),
            );
            children.push(text_list_row(field_key, None, ""));
            WidgetSpec::Col {
                children,
                key: None,
            }
        }
        // Key→value map (e.g. Languages, LSP servers). Label, a dimmed
        // `Name  <Col>` header when the control names a display field,
        // and one `List` keyed by the path: an entry per row (padded key
        // column + truncated preview, the cursor's row saying what Enter
        // does) and the `[+] Add new` row last. The list's selection is
        // the surface's cursor.
        SettingControl::Map {
            label,
            entries,
            display_field,
            no_add,
            ..
        } => {
            let key_width = 20usize;
            let mut rows: Vec<TextPropertyEntry> = entries
                .iter()
                .enumerate()
                .map(|(idx, (k, v))| {
                    let preview =
                        truncate_chars(&map_display_value(display_field.as_deref(), v), 20);
                    let mut segs = vec![
                        seg("  ", None),
                        seg(&pad(k, key_width), Some(ACCENT_KEY)),
                        seg(" ", None),
                        seg(&preview, Some(VALUE_PREVIEW_FG)),
                    ];
                    if cursor == Some(idx) {
                        segs.push(seg("  [Enter to edit]", Some(DIM_HINT)));
                    }
                    segments_row(segs)
                })
                .collect();
            if !no_add {
                rows.push(add_row(cursor == Some(entries.len()), "  [Enter to add]"));
            }
            let mut children = vec![raw_row(format!("{label}:"))];
            if let Some(title) = display_field.as_deref().map(column_title) {
                children.push(header_row(&pad("Name", key_width), &title));
            }
            // An EMPTY List still pads one blank row (its virtual viewport
            // is min 1 tall); an auto-managed map with nothing in it has no
            // rows at all.
            if !rows.is_empty() {
                children.push(rows_list(field_key, rows, control.add_row(), cursor));
            }
            WidgetSpec::Col {
                children,
                key: None,
            }
        }
        // Object array (keybindings, LSP server lists). Rows are formatted
        // by the domain helper: a `combo → action` pair for
        // keybinding-shaped entries, collapsing to just the display value
        // when the combo column is empty (LSP servers and other
        // non-keybinding arrays). The `> ` indicator marks the cursor's
        // row, mirroring the historical renderer; the `List` supplies the
        // highlight and the navigation, its add row last.
        SettingControl::ObjectArray {
            label,
            items,
            display_field,
            ..
        } => {
            let combo_width = 20usize;
            let mut rows: Vec<TextPropertyEntry> = items
                .iter()
                .enumerate()
                .map(|(idx, b)| {
                    let (combo, action) = object_array_row(display_field.as_deref(), b);
                    let indicator = match cursor == Some(idx) {
                        true => "> ",
                        false => "  ",
                    };
                    let segs = if combo.trim().is_empty() {
                        vec![seg(indicator, None), seg(&action, Some(ACTION_FG))]
                    } else {
                        vec![
                            seg(indicator, None),
                            seg(&pad(&combo, combo_width), Some(ACCENT_KEY)),
                            seg(" → ", None),
                            seg(&action, Some(ACTION_FG)),
                        ]
                    };
                    segments_row(segs)
                })
                .collect();
            rows.push(add_row(cursor == Some(items.len()), ""));
            WidgetSpec::Col {
                children: vec![
                    raw_row(format!("{label}:")),
                    rows_list(field_key, rows, control.add_row(), cursor),
                ],
                key: None,
            }
        }
        // Multiline JSON editor: label, a `│`-bordered line box showing
        // the editor's text with selection highlight + block caret, an
        // A JSON value is a multi-line text field under its label row: the
        // text is the model's, applied as it is typed; the caret and the
        // selection are the field's editor in the store (`text::resolve`),
        // painted only while the field is the surface's focused widget. An
        // unset value shows the field empty with a hint in it, and a text
        // that does not parse says so under the field.
        SettingControl::Json { label, text } => {
            let value = match json_is_unset(text) {
                true => String::new(),
                false => text.clone(),
            };
            let mut children = vec![
                raw_row(format!("{label}:")),
                WidgetSpec::Text {
                    rows: value.split('\n').count() as u32,
                    value,
                    cursor_byte: -1,
                    focused: false,
                    label: String::new(),
                    placeholder: Some("(not set — press Enter to add)".to_string()),
                    field_width: 0,
                    max_visible_chars: 0,
                    full_width: true,
                    completions: Vec::new(),
                    completions_visible_rows: 0,
                    block_caret: true,
                    sel_start: -1,
                    sel_end: -1,
                    label_width: 0,
                    read_only: false,
                    markdown: false,
                    key,
                },
            ];
            if !json_is_valid(text) {
                children.push(raw_entry_row(segments_row(vec![seg(
                    "  ⚠ Invalid JSON",
                    Some("diagnostic.warning_fg"),
                )])));
            }
            WidgetSpec::Col {
                children,
                key: None,
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

/// Inner width of a text list row's field (between its brackets).
/// Matches the historical `field_width = 30` minus the two brackets.
const TEXTLIST_CELL_WIDTH: u32 = 28;

/// Dim hint / disabled-text color.
const DIM_HINT: &str = "ui.menu_disabled_fg";

/// One-line key hint carried under a `DualList`'s columns.
///
/// Which keys move an item between the columns, and which reorder the
/// Included side, cannot be inferred from the control's appearance, so
/// the hint is always rendered once the row is reachable: "press Enter
/// to start" while merely selected, the full key list while live.
pub fn dual_list_hint(live: bool, selected: bool) -> String {
    use fresh_i18n::t;
    if live {
        t!("settings.dual_list_keys_hint").to_string()
    } else if selected {
        t!("settings.dual_list_enter_hint").to_string()
    } else {
        String::new()
    }
}

/// One row of a text list: an item's field with its `[x]` beside it, or —
/// for `row: None` — the add row, a field labelled `[+] Add new` whose
/// draft becomes an item on Enter. Both are the `Text` kind's, keyed so the
/// store can hold the live one's editor.
fn text_list_row(field_key: &str, row: Option<usize>, value: &str) -> WidgetSpec {
    let mut children = vec![
        raw_row("  ".to_string()),
        WidgetSpec::Text {
            value: value.to_string(),
            cursor_byte: -1,
            focused: false,
            label: match row {
                Some(_) => String::new(),
                None => "[+] Add new".to_string(),
            },
            placeholder: row.is_none().then(|| "type new item".to_string()),
            rows: 1,
            field_width: TEXTLIST_CELL_WIDTH,
            max_visible_chars: 0,
            full_width: false,
            completions: Vec::new(),
            completions_visible_rows: 0,
            block_caret: true,
            sel_start: -1,
            sel_end: -1,
            label_width: 0,
            read_only: false,
            markdown: false,
            key: Some(SettingControl::text_list_row_key(field_key, row)),
        },
    ];
    if let Some(i) = row {
        children.push(raw_row(" ".to_string()));
        children.push(WidgetSpec::Button {
            label: "[x]".to_string(),
            focused: false,
            intent: ButtonKind::Normal,
            key: Some(format!("{field_key}::remove::{i}")),
            disabled: false,
            focusable: false,
            bare: true,
            full_width: false,
            hover_style: None,
            style: Some(OverlayOptions {
                fg: Some(OverlayColorSpec::theme_key(REMOVE_FG)),
                ..Default::default()
            }),
        });
    }
    WidgetSpec::Row {
        children,
        key: None,
        wrap: false,
    }
}

/// The `  [+] Add new` row of a map's or an object array's list, with a dim
/// hint when the cursor is on it.
fn add_row(on_cursor: bool, hint: &str) -> TextPropertyEntry {
    let mut segs = vec![seg("  ", None), seg("[+] Add new", Some(ADD_FG))];
    if on_cursor && !hint.is_empty() {
        segs.push(seg(hint, Some(DIM_HINT)));
    }
    segments_row(segs)
}

/// A map's or an object array's rows as one `List` keyed by the control's
/// path: the entries, then the add row. Each row is keyed —
/// `{path}::row::{i}`, the add row `{path}::add` — so the surface can name
/// it (the row the window is moved to) and a press on it says which it
/// was. The selection is the surface's cursor; `visible_rows` covers the
/// whole set, the settings viewport doing the outer scroll.
///
/// A `List` — rather than a `Raw` — is what makes the rows *clickable at
/// all*: the runtime emits a `select` hit per list row carrying
/// `payload.index`, and nothing whatsoever for a `Raw`.
fn rows_list(
    field_key: &str,
    rows: Vec<TextPropertyEntry>,
    add_row: Option<usize>,
    cursor: Option<usize>,
) -> WidgetSpec {
    let visible = rows.len().max(1) as u32;
    let item_keys = (0..rows.len())
        .map(|i| match add_row == Some(i) {
            true => SettingControl::text_list_row_key(field_key, None),
            false => SettingControl::text_list_row_key(field_key, Some(i)),
        })
        .collect();
    WidgetSpec::List {
        items: rows,
        item_specs: Vec::new(),
        item_keys,
        selected_index: cursor.map(|c| c as i32).unwrap_or(-1),
        visible_rows: Some(visible),
        focusable: true,
        key: Some(field_key.to_string()),
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
        out.extend(std::iter::repeat_n(' ', width - n));
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

/// Human column title from a `display_field` pointer (`/grammar` →
/// `Grammar`).
pub(crate) fn column_title(display_field: &str) -> String {
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
    use std::collections::HashMap;

    #[test]
    fn toggle_maps_to_toggle_widget() {
        let s = SettingControl::Toggle {
            label: "Word wrap".into(),
            checked: true,
            inherited: false,
        };
        match setting_control_to_widget("/editor/word_wrap", &s) {
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
        let s = SettingControl::Number {
            label: "Tab size".into(),
            value: 4.0,
            min: Some(1.0),
            max: Some(16.0),
            integer: true,
            percent: false,
        };
        match setting_control_to_widget("/editor/tab_size", &s) {
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
    fn a_percent_is_the_fraction_the_json_carries() {
        let s = SettingControl::Number {
            label: "Opacity".into(),
            value: 0.25,
            min: Some(0.0),
            max: Some(1.0),
            integer: false,
            percent: true,
        };
        match setting_control_to_widget("/ui/opacity", &s) {
            WidgetSpec::Number {
                value,
                percent,
                max,
                step,
                ..
            } => {
                assert_eq!(value, 0.25);
                assert_eq!(max, Some(1.0));
                assert!(percent);
                assert_eq!(step, 0.01);
            }
            other => panic!("expected Number, got {other:?}"),
        }
    }

    #[test]
    fn dropdown_maps_options_and_selection() {
        let s = SettingControl::dropdown("Theme", vec!["Light".into(), "Dark".into()], vec![], 1);
        match setting_control_to_widget("/ui/theme", &s) {
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
        let s = SettingControl::Text {
            label: "Formatter".into(),
            value: "rustfmt".into(),
            placeholder: "command".into(),
        };
        match setting_control_to_widget("/fmt/cmd", &s) {
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
        let s = SettingControl::DualList {
            label: "Status bar".into(),
            options: vec![
                ("mode".into(), "Mode".into()),
                ("git".into(), "Git".into()),
                ("pos".into(), "Position".into()),
            ],
            included: vec!["mode".into()],
            excluded: vec!["git".into()],
        };
        match setting_control_to_widget("/statusbar/elements", &s) {
            WidgetSpec::DualList {
                options,
                included,
                excluded,
                visible_rows,
                ..
            } => {
                assert_eq!(options.len(), 3);
                assert_eq!(options[0].value, "mode");
                assert_eq!(options[0].label, "Mode");
                assert_eq!(included, vec!["mode".to_string()]);
                assert_eq!(excluded, vec!["git".to_string()]);
                // The rows both columns could ever need between them.
                assert_eq!(visible_rows, 2);
            }
            other => panic!("expected DualList, got {other:?}"),
        }
    }

    /// A JSON value is a text area under its label; the node its kind is
    /// handed is that area, found by the control's key. An unset value is
    /// an empty field with a hint, and a text that does not parse grows a
    /// warning row.
    #[test]
    fn json_is_a_text_area_its_key_names() {
        let s = SettingControl::Json {
            label: "Formatter".into(),
            text: "{\n  \"a\": 1\n}".into(),
        };
        let spec = setting_control_to_widget("/formatter", &s);
        let WidgetSpec::Col { children, key } = &spec else {
            panic!("expected Col, got {spec:?}");
        };
        assert_eq!(key, &None);
        assert_eq!(children.len(), 2);
        match live_widget("/formatter", &s, "/formatter") {
            WidgetSpec::Text {
                rows, value, key, ..
            } => {
                assert_eq!(rows, 3);
                assert_eq!(value, "{\n  \"a\": 1\n}");
                assert_eq!(key.as_deref(), Some("/formatter"));
            }
            other => panic!("expected Text, got {other:?}"),
        }

        let unset = SettingControl::Json {
            label: "Formatter".into(),
            text: "null".into(),
        };
        match live_widget("/formatter", &unset, "/formatter") {
            WidgetSpec::Text {
                rows,
                value,
                placeholder,
                ..
            } => {
                assert_eq!(rows, 1);
                assert_eq!(value, "");
                assert!(placeholder.is_some());
            }
            other => panic!("expected Text, got {other:?}"),
        }

        let broken = SettingControl::Json {
            label: "Formatter".into(),
            text: "{".into(),
        };
        let WidgetSpec::Col { children, .. } = setting_control_to_widget("/formatter", &broken)
        else {
            panic!("expected Col");
        };
        assert_eq!(children.len(), 3);
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
                SettingControl::Toggle {
                    label: "Word wrap".into(),
                    checked: true,
                    inherited: false,
                },
                Some("Editor"),
            ),
            item(
                "/editor/tab_size",
                SettingControl::Number {
                    label: "Tab size".into(),
                    value: 4.0,
                    min: Some(1.0),
                    max: Some(8.0),
                    integer: true,
                    percent: false,
                },
                None,
            ),
            item(
                "/ui/theme",
                SettingControl::dropdown("Theme", vec!["Light".into(), "Dark".into()], vec![], 0),
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
                SettingControl::Toggle {
                    label: "Word wrap".into(),
                    checked: true,
                    inherited: false,
                },
                Some("Editor"),
            ),
            item(
                "/editor/tab_size",
                SettingControl::Number {
                    label: "Tab size".into(),
                    value: 4.0,
                    min: Some(1.0),
                    max: Some(8.0),
                    integer: true,
                    percent: false,
                },
                None,
            ),
            item(
                "/ui/theme",
                SettingControl::dropdown("Theme", vec!["Light".into(), "Dark".into()], vec![], 1),
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

    /// Collect every foreground theme key referenced by the rendered
    /// entries' inline overlays (row-level styles ride the entries'
    /// `style`, segment styles become inline overlays at render time).
    fn rendered_fg_keys(out: &crate::widgets::RenderOutput) -> Vec<String> {
        let mut keys = Vec::new();
        for e in &out.entries {
            let styles = e
                .inline_overlays
                .iter()
                .map(|o| &o.style)
                .chain(e.style.as_ref());
            for s in styles {
                if let Some(OverlayColorSpec::ThemeKey(k)) = &s.fg {
                    keys.push(k.clone());
                }
            }
        }
        keys
    }

    /// A text list is its rows as fields: one keyed text field per item
    /// with its `[x]` beside it, and the add row's field last. The node a
    /// row's kind is handed is found by the row's key.
    #[test]
    fn a_text_list_s_rows_are_fields_its_kind_edits() {
        let s = SettingControl::TextList {
            label: "Extensions".into(),
            items: vec!["cpp".into(), "cc".into()],
            integer: false,
        };
        let field = "/languages/cpp/extensions";
        let WidgetSpec::Col { children, .. } = setting_control_to_widget(field, &s) else {
            panic!("expected Col");
        };
        // The label, two item rows, the add row.
        assert_eq!(children.len(), 4);
        match live_widget(field, &s, &format!("{field}::row::1")) {
            WidgetSpec::Text { value, key, .. } => {
                assert_eq!(value, "cc");
                assert_eq!(key.as_deref(), Some("/languages/cpp/extensions::row::1"));
            }
            other => panic!("expected the row's field, got {other:?}"),
        }
        match live_widget(field, &s, &format!("{field}::add")) {
            WidgetSpec::Text {
                value,
                label,
                placeholder,
                ..
            } => {
                assert_eq!(value, "");
                assert_eq!(label, "[+] Add new");
                assert!(placeholder.is_some());
            }
            other => panic!("expected the add row's field, got {other:?}"),
        }
    }

    /// A map's rows are one list keyed by the path, the add row last; the
    /// cursor's row says what Enter does, and the list selects it.
    #[test]
    fn a_map_s_rows_are_one_list_with_the_add_row_last() {
        let s = SettingControl::Map {
            label: "Languages".into(),
            entries: vec![("rust".into(), serde_json::json!({}))],
            value_schema: None,
            display_field: None,
            no_add: false,
        };
        match live_widget("/languages", &s, "/languages") {
            WidgetSpec::List {
                items,
                item_keys,
                selected_index,
                key,
                ..
            } => {
                assert_eq!(items.len(), 2);
                assert_eq!(item_keys, vec!["/languages::row::0", "/languages::add"]);
                assert_eq!(selected_index, -1);
                assert_eq!(key.as_deref(), Some("/languages"));
            }
            other => panic!("expected the map's list, got {other:?}"),
        }
        let with_cursor = setting_control_to_widget_aligned("/languages", &s, None, Some(0));
        let out = crate::widgets::render_spec(&with_cursor, &HashMap::new(), "", u32::MAX);
        assert!(
            out.entries
                .iter()
                .any(|e| e.text.contains("[Enter to edit]")),
            "the cursor's row says what Enter does"
        );
        let with_cursor = setting_control_to_widget_aligned("/languages", &s, None, Some(1));
        match with_cursor {
            WidgetSpec::Col { children, .. } => match &children[1] {
                WidgetSpec::List { selected_index, .. } => assert_eq!(*selected_index, 1),
                other => panic!("expected the list, got {other:?}"),
            },
            other => panic!("expected Col, got {other:?}"),
        }
    }

    #[test]
    fn composite_rows_use_surface_readable_theme_keys() {
        // The value/action columns must use foregrounds that read on
        // the dialog surface in every bundled theme. Regression:
        // `ui.tab_active_fg` was used — themes pair it with the bright
        // active-tab background (high-contrast sets it to pure black),
        // so Languages previews and Env detector rows vanished.
        use serde_json::json;

        for control in [
            SettingControl::Map {
                label: "Languages".into(),
                entries: vec![("cpp".to_string(), json!({"grammar": "C++"}))],
                value_schema: None,
                display_field: None,
                no_add: false,
            },
            SettingControl::ObjectArray {
                label: "Detectors".into(),
                items: vec![json!({"name": ".venv"})],
                item_schema: None,
                display_field: Some("/name".to_string()),
            },
            SettingControl::TextList {
                label: "Extensions".into(),
                items: vec!["cpp".into()],
                integer: false,
            },
        ] {
            let spec = setting_control_to_widget("/k", &control);
            let out = crate::widgets::render_spec(&spec, &HashMap::new(), "", u32::MAX);
            let keys = rendered_fg_keys(&out);
            assert!(
                keys.iter().all(|k| k != "ui.tab_active_fg"),
                "tab_active_fg is a tab-surface color, unreadable on the \
                 dialog surface; got {keys:?}"
            );
        }
    }

    #[test]
    fn object_array_add_new_row_highlights_when_focused() {
        // The `[+] Add new` row must read as selected (list-row selection
        // bg, extended to line end) when it is the cursor's row —
        // otherwise the user can't tell it's selected. Regression: the
        // add row rendered with no highlight at all.
        use serde_json::json;

        let arr = SettingControl::ObjectArray {
            label: "Detectors".into(),
            items: vec![json!({"name": ".venv"})],
            item_schema: None,
            display_field: Some("/name".to_string()),
        };

        // The cursor on an entry: the add row carries no selection bg.
        let spec = setting_control_to_widget_aligned("/env/detectors", &arr, None, Some(0));
        let out = crate::widgets::render_spec(&spec, &HashMap::new(), "", u32::MAX);
        let add_unfocused = out
            .entries
            .iter()
            .find(|e| e.text.contains("[+] Add new"))
            .expect("add-new row");
        assert!(
            add_unfocused
                .style
                .as_ref()
                .and_then(|s| s.bg.as_ref())
                .is_none(),
            "add row must not highlight while an entry is focused: {add_unfocused:?}"
        );

        // The cursor on the add row: it gets the selection bg.
        let spec = setting_control_to_widget_aligned("/env/detectors", &arr, None, Some(1));
        let out = crate::widgets::render_spec(&spec, &HashMap::new(), "", u32::MAX);
        let add_focused = out
            .entries
            .iter()
            .find(|e| e.text.contains("[+] Add new"))
            .expect("add-new row");
        let bg = add_focused
            .style
            .as_ref()
            .and_then(|s| s.bg.as_ref())
            .expect("focused add row must carry a selection background");
        assert!(
            matches!(bg, OverlayColorSpec::ThemeKey(k) if k == "ui.popup_selection_bg"),
            "add row highlight uses the list selection bg: {bg:?}"
        );
        assert!(
            add_focused
                .style
                .as_ref()
                .is_some_and(|s| s.extend_to_line_end),
            "selection bg extends to the line end like a list row"
        );
    }

    /// Every row of a composite control that the user can press must
    /// carry a hit, and the hit must say *which* row — the settings
    /// side has no other way to tell "the second extension" from "the
    /// add row" once the body is a description rather than a set of
    /// stashed rects.
    #[test]
    fn every_clickable_row_of_a_composite_control_names_itself() {
        /// `(widget_key, event)` of every hit, in row order.
        fn hits(out: &crate::widgets::RenderOutput) -> Vec<(String, &'static str)> {
            let mut hits: Vec<_> = out.hits.iter().collect();
            hits.sort_by_key(|h| (h.buffer_row, h.byte_start));
            hits.iter()
                .map(|h| (h.event.widget_key.clone(), h.event.event_type))
                .collect()
        }

        let field = "/languages/cpp/extensions";
        let list = SettingControl::TextList {
            label: "Extensions".into(),
            items: vec!["cpp".into(), "cc".into()],
            integer: false,
        };
        let out = crate::widgets::render_spec(
            &setting_control_to_widget(field, &list),
            &HashMap::new(),
            "",
            u32::MAX,
        );
        assert_eq!(
            hits(&out),
            vec![
                (format!("{field}::row::0"), "focus"),
                (format!("{field}::remove::0"), "activate"),
                (format!("{field}::row::1"), "focus"),
                (format!("{field}::remove::1"), "activate"),
                (format!("{field}::add"), "focus"),
            ],
            "each item's field and its [x], then the add row's field"
        );

        // A map: one list whose rows carry `select`, the add row last.
        let map = SettingControl::Map {
            label: "Languages".into(),
            entries: vec![("rust".into(), serde_json::json!({}))],
            value_schema: None,
            display_field: None,
            no_add: false,
        };
        let out = crate::widgets::render_spec(
            &setting_control_to_widget("/languages", &map),
            &HashMap::new(),
            "",
            u32::MAX,
        );
        // A row's hit names the row; the list it belongs to is the owner.
        let selects: Vec<(String, String, i64)> = out
            .hits
            .iter()
            .filter(|h| h.event.event_type == "select")
            .map(|h| {
                (
                    h.event.owner_key.clone().unwrap_or_default(),
                    h.event.widget_key.clone(),
                    h.event.payload["index"].as_i64().unwrap_or(-1),
                )
            })
            .collect();
        assert_eq!(
            selects,
            vec![
                (
                    "/languages".to_string(),
                    "/languages::row::0".to_string(),
                    0
                ),
                ("/languages".to_string(), "/languages::add".to_string(), 1),
            ],
            "the map's one entry and its add row are both rows of its list"
        );
    }
}
