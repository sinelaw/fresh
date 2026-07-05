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
//! The composite controls (`TextList`, `Map`, `ObjectArray`, `Json`,
//! `Complex`) don't yet have first-class widget kinds — the plan builds
//! their editing on floating widget panels rather than bespoke inline
//! controls — so they map to a labelled `Raw` placeholder for now. The
//! placeholder keeps the function total and the tree renderable; the
//! placeholder row is replaced when those editors land.

use super::items::{SettingControl, SettingItem};
use fresh_core::api::{DualListOption, OverlayColorSpec, OverlayOptions, WidgetSpec};
use fresh_core::text_property::{InlineOverlay, OffsetUnit, TextPropertyEntry};

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
            cursor_byte: -1,
            focused: false,
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
            label: String::new(),
            focused: false,
            visible_rows: 6,
            key,
        },
        // Composite controls: labelled placeholder until their
        // floating-panel editors land (see the plan's Phase 4/§5.4).
        SettingControl::TextList(_) => placeholder(field_key, "string list"),
        SettingControl::Map(_) => placeholder(field_key, "map"),
        SettingControl::ObjectArray(_) => placeholder(field_key, "keybinding list"),
        SettingControl::Json(_) => placeholder(field_key, "JSON"),
        SettingControl::Complex { type_name } => placeholder(field_key, type_name),
    }
}

/// A single-row `Raw` placeholder for a control that doesn't yet have a
/// first-class widget kind.
fn placeholder(field_key: &str, kind: &str) -> WidgetSpec {
    WidgetSpec::Raw {
        entries: vec![TextPropertyEntry::text(format!("{field_key}: <{kind}>"))],
        key: Some(field_key.to_string()),
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
