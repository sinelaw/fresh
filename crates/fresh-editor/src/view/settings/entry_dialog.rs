//! Entry detail dialog for editing complex map entries
//!
//! Provides a modal dialog for editing complex map entries using the same
//! SettingItem/SettingControl infrastructure as the main settings UI.

use super::items::{
    build_item_from_value, control_to_value, ItemBoxStyle, SettingControl, SettingItem,
};
use super::live;
use super::schema::{SettingSchema, SettingType};
use fresh_i18n::t;
use serde_json::Value;
use std::collections::{HashMap, HashSet};

/// A per-field action affordance rendered at the right edge of a field's row.
///
/// These target different values:
/// * `Reset` sets the field to its *built-in default* (the value the bundled
///   config ships for this entry).
/// * `Inherit` sets the field to `null`. It renders as `[Inherit]` when null
///   falls back to a parent-scope value (e.g. a per-language `line_wrap`
///   inheriting `editor.line_wrap`), or `[Clear]` when there's no such fallback
///   and null just unsets the field (e.g. a `formatter`). See
///   [`EntryDialogState::field_action_buttons`].
///
/// A field only offers the action(s) that lead to a *different* result, so a
/// nullable field whose built-in default is itself `null` shows only the
/// Inherit/Clear button (Reset would be identical), while a plain field with a
/// built-in default and no inheritance chain shows only `Reset`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FieldAction {
    /// Set the field to its built-in default value.
    Reset,
    /// Set the field to `null` — inherit a parent value, or clear it when there
    /// is no parent to inherit from.
    Inherit,
}

/// Simple scalar controls whose per-field action buttons join the Tab order.
/// Composite controls (lists, maps, JSON) keep their own internal navigation,
/// so their inherit affordance stays mouse-only.
fn is_simple_field_control(control: &SettingControl) -> bool {
    live::kind_edited(control)
}

/// Lay out right-aligned per-field action buttons against `right_edge`
/// (exclusive). Returns `(action, x, width)` left to right, with a one-column
/// gap between buttons and a one-column margin at the right edge. Shared by the
/// renderer and the click hit-tester so their geometry can't drift.
pub fn layout_field_action_buttons(
    buttons: &[(FieldAction, String)],
    right_edge: u16,
) -> Vec<(FieldAction, u16, u16)> {
    if buttons.is_empty() {
        return Vec::new();
    }
    let widths: Vec<u16> = buttons
        .iter()
        .map(|(_, label)| label.chars().count() as u16)
        .collect();
    let gaps = buttons.len().saturating_sub(1) as u16;
    let total: u16 = widths.iter().sum::<u16>() + gaps + 1;
    let mut x = right_edge.saturating_sub(total);
    let mut out = Vec::with_capacity(buttons.len());
    for ((action, _), w) in buttons.iter().zip(widths) {
        out.push((*action, x, w));
        x = x.saturating_add(w + 1);
    }
    out
}

/// State for the entry detail dialog
#[derive(Debug, Clone)]
pub struct EntryDialogState {
    /// The entry key (e.g., "rust" for language)
    pub entry_key: String,
    /// The map path this entry belongs to (e.g., "/languages", "/lsp")
    pub map_path: String,
    /// Human-readable title for the dialog
    pub title: String,
    /// Whether this is a new entry (vs editing existing)
    pub is_new: bool,
    /// Items in the dialog (using same SettingItem structure as main settings)
    pub items: Vec<SettingItem>,
    /// Currently selected item index
    pub selected_item: usize,
    /// Currently focused button (0=Save, 1=Delete, 2=Cancel for existing; 0=Save, 1=Cancel for new)
    pub focused_button: usize,
    /// Whether focus is on buttons (true) or items (false)
    pub focus_on_buttons: bool,
    /// Whether deletion was requested
    pub delete_requested: bool,
    /// Scroll offset for the items area
    pub scroll_offset: usize,
    /// Last known viewport height (updated during render)
    pub viewport_height: usize,
    /// The field window's handle. The twin of `SettingsState::body_anchor`,
    /// one surface in: the fields are a `col` in a `viewport`, so how far the
    /// window has moved is layout's answer and moving it is a message.
    pub body_anchor: std::rc::Rc<fresh_ui::behavior::Anchor>,
    /// Hovered item index (for mouse hover feedback)
    pub hover_item: Option<usize>,
    /// Hovered button index (for mouse hover feedback)
    pub hover_button: Option<usize>,
    /// Original value when dialog was opened (for Cancel to restore)
    pub original_value: Value,
    /// Index of first editable item (items before this are read-only)
    /// Used for rendering separator and focus navigation
    pub first_editable_index: usize,
    /// Whether deletion is disabled (for auto-managed entries like plugins)
    pub no_delete: bool,
    /// When true, the dialog wraps a single non-Object value (e.g., an ObjectArray).
    /// `to_value()` returns the raw control value instead of wrapping in an Object.
    pub is_single_value: bool,
    /// True when the dialog edits an item in an array (constructed via
    /// `for_array_item`); false for map entries (`from_schema`). Drives
    /// the Delete button's label/confirmation copy so the prompt doesn't
    /// show a numeric index as if it were a meaningful name.
    pub is_array_item: bool,
    /// Set to true on the first user-driven mutation (typed char,
    /// toggled bool, list add/remove, etc.). Drives the dirty
    /// indicator + the Esc discard prompt without relying on a
    /// JSON-equality check that's too noisy at the schema layer.
    pub user_edited: bool,
    /// When `Some(i)`, keyboard focus is on the i-th per-field action button
    /// (`[Reset]`/`[Inherit]`/`[Clear]`) of the currently selected field rather
    /// than on the field's control — `i` indexes [`field_action_buttons`]. Tab
    /// moves onto these buttons and Enter/Space activates them; it is the only
    /// keyboard path to the per-field actions.
    pub field_button_focus: Option<usize>,
    /// Field names (item path without the leading `/`) that genuinely *inherit*
    /// from a parent scope when unset — e.g. a per-language `line_wrap` falls
    /// back to the global `editor.line_wrap`. For these the per-field "set to
    /// null" button is labelled `[Inherit]`; for everything else (a `formatter`
    /// with no global fallback) it's labelled `[Clear]`, since null just unsets
    /// the value rather than inheriting one. Empty means "nothing inherits".
    pub inheritable_fields: HashSet<String>,
    /// Snapshot of the field being text-edited, captured the moment editing
    /// begins (before `start_editing` mutates the control). Esc restores it so
    /// an in-progress edit is discarded — the platform convention where Enter
    /// and Tab commit a field while Esc cancels it. `None` whenever no edit is
    /// in flight.
    edit_snapshot: Option<FieldEditSnapshot>,
    /// The dialog's fields as the widget kinds see them — the store the
    /// kinds read and write, and the path of the field that is *live*. Its
    /// own, not the page's: the same field path can be open at two levels
    /// of the stack. See `view::settings::live`.
    pub controls: crate::widgets::WidgetPanelState,
}

/// Pre-edit state of a single dialog field, used to revert an abandoned edit.
/// Cloning the whole control covers every editable kind uniformly (Text,
/// Number, TextList, Json) — each restores its exact text/value, cursor, and
/// buffers. `is_null` and the dialog-level `user_edited` flag are captured too
/// so reverting also undoes the "field is now set / dialog is dirty" side
/// effects that typing triggered via `mark_field_edited`.
#[derive(Debug, Clone)]
struct FieldEditSnapshot {
    item_index: usize,
    control: SettingControl,
    is_null: bool,
    user_edited: bool,
}

/// The path of the one field of a single-value dialog — a map entry whose
/// value is not an object (a language's server list): the entry's root,
/// with nothing to join onto the entry path.
pub const SINGLE_VALUE_PATH: &str = "/";

impl EntryDialogState {
    /// Create a dialog from a schema definition
    ///
    /// This is the primary, schema-driven constructor. It builds items
    /// dynamically from the SettingSchema's properties using the same
    /// build logic as the main settings UI.
    pub fn from_schema(
        key: String,
        value: &Value,
        schema: &SettingSchema,
        map_path: &str,
        is_new: bool,
        no_delete: bool,
        available_status_bar_tokens: &HashMap<String, String>,
    ) -> Self {
        let mut items = Vec::new();

        // Add key field as first item (read-only for existing entries)
        let key_item = SettingItem {
            path: "__key__".to_string(),
            name: "Key".to_string(),
            description: Some("unique identifier for this entry".to_string()),
            control: SettingControl::Text {
                label: "Key".to_string(),
                value: key.clone(),
                placeholder: String::new(),
            },
            default: None,
            modified: false,
            layer_source: crate::config_io::ConfigLayer::System,
            read_only: !is_new, // Key is editable only for new entries
            is_auto_managed: false,
            nullable: false,
            is_null: false,
            section: None,
            is_section_start: false,
            style: ItemBoxStyle::default(),
            dual_list_sibling: None,
        };
        items.push(key_item);

        // Add schema-driven items from object properties
        let is_single_value = !matches!(&schema.setting_type, SettingType::Object { .. });
        if let SettingType::Object { properties } = &schema.setting_type {
            for prop in properties {
                let field_name = prop.path.trim_start_matches('/');
                let field_value = value.get(field_name);
                let item = build_item_from_value(prop, field_value, available_status_bar_tokens);
                items.push(item);
            }
        } else {
            // For non-object types (e.g., ObjectArray, Map), build a single item
            // from the entire value so the dialog can render it. Its path is
            // the entry's root, `/`: the item *is* the entry's value
            // (`entry_path` joins nothing onto it), and the kinds' store needs
            // a key that is not empty — an empty key is no key to them.
            let mut item = build_item_from_value(schema, Some(value), available_status_bar_tokens);
            item.path = SINGLE_VALUE_PATH.to_string();
            items.push(item);
        }

        // Sort items: read-only first, then editable (stable sort preserves x-order)
        items.sort_by_key(|item| !item.read_only);

        // Compute is_section_start for section headers in entry dialogs
        Self::compute_section_starts(&mut items);

        // Find the first editable item index
        let first_editable_index = items
            .iter()
            .position(|item| !item.read_only)
            .unwrap_or(items.len());

        // If all items are read-only, start with focus on buttons
        let focus_on_buttons = first_editable_index >= items.len();
        let selected_item = if focus_on_buttons {
            0
        } else {
            first_editable_index
        };

        let title = if is_new {
            format!("Add {}", schema.name)
        } else {
            format!("Edit {}", schema.name)
        };

        let mut result = Self {
            entry_key: key,
            map_path: map_path.to_string(),
            title,
            is_new,
            items,
            selected_item,
            focused_button: 0,
            focus_on_buttons,
            delete_requested: false,
            scroll_offset: 0,
            viewport_height: 20, // Default, updated during render
            body_anchor: fresh_ui::behavior::Anchor::new(),
            hover_item: None,
            hover_button: None,
            original_value: value.clone(),
            first_editable_index,
            no_delete,
            is_single_value,
            is_array_item: false,
            user_edited: false,
            field_button_focus: None,
            inheritable_fields: HashSet::new(),
            edit_snapshot: None,
            controls: crate::widgets::WidgetPanelState::surface(fresh_core::api::WidgetSpec::Col {
                children: Vec::new(),
                key: None,
            }),
        };
        // Pre-focus the first item in any ObjectArray controls so pressing
        // Enter opens the item editor instead of "Add new".
        result.init_object_array_focus();
        result
    }

    /// Create a dialog for an array item (no key field)
    ///
    /// Used for ObjectArray controls where items are identified by index, not key.
    pub fn for_array_item(
        index: Option<usize>,
        value: &Value,
        schema: &SettingSchema,
        array_path: &str,
        is_new: bool,
        available_status_bar_tokens: &HashMap<String, String>,
    ) -> Self {
        let mut items = Vec::new();

        // Add schema-driven items from object properties (no key field for arrays)
        if let SettingType::Object { properties } = &schema.setting_type {
            for prop in properties {
                let field_name = prop.path.trim_start_matches('/');
                let field_value = value.get(field_name);
                let item = build_item_from_value(prop, field_value, available_status_bar_tokens);
                items.push(item);
            }
        }

        // Sort items: read-only first, then editable
        items.sort_by_key(|item| !item.read_only);

        // Compute is_section_start for section headers
        Self::compute_section_starts(&mut items);

        // Find the first editable item index
        let first_editable_index = items
            .iter()
            .position(|item| !item.read_only)
            .unwrap_or(items.len());

        // If all items are read-only, start with focus on buttons
        let focus_on_buttons = first_editable_index >= items.len();
        let selected_item = if focus_on_buttons {
            0
        } else {
            first_editable_index
        };

        let title = if is_new {
            format!("Add {}", schema.name)
        } else {
            format!("Edit {}", schema.name)
        };

        Self {
            entry_key: index.map_or(String::new(), |i| i.to_string()),
            map_path: array_path.to_string(),
            title,
            is_new,
            items,
            selected_item,
            focused_button: 0,
            focus_on_buttons,
            delete_requested: false,
            scroll_offset: 0,
            viewport_height: 20,
            body_anchor: fresh_ui::behavior::Anchor::new(),
            hover_item: None,
            hover_button: None,
            original_value: value.clone(),
            first_editable_index,
            no_delete: false, // Arrays typically allow deletion
            is_single_value: false,
            is_array_item: true,
            user_edited: false,
            field_button_focus: None,
            inheritable_fields: HashSet::new(),
            edit_snapshot: None,
            controls: crate::widgets::WidgetPanelState::surface(fresh_core::api::WidgetSpec::Col {
                children: Vec::new(),
                key: None,
            }),
        }
    }

    /// Compute is_section_start flags for section headers.
    /// Marks the first item in each new section so the renderer can draw headers.
    fn compute_section_starts(items: &mut [SettingItem]) {
        let mut last_section: Option<&str> = None;
        for item in items.iter_mut() {
            let current = item.section.as_deref();
            if current.is_some() && current != last_section {
                item.is_section_start = true;
            }
            if current.is_some() {
                last_section = current;
            }
        }
    }

    /// Get the current key value from the key item
    pub fn get_key(&self) -> String {
        // Find the key item by path (may not be first after sorting)
        for item in &self.items {
            if item.path == "__key__" {
                if let SettingControl::Text { value, .. } = &item.control {
                    return value.clone();
                }
            }
        }
        self.entry_key.clone()
    }

    /// Full JSON pointer path to the entry this dialog edits.
    ///
    /// For an existing map entry under `/universal_lsp` with key `quicklsp`,
    /// this returns `/universal_lsp/quicklsp`. For array items, `entry_key`
    /// is the stringified index. For brand-new map entries whose key has
    /// not been chosen yet, this falls back to `map_path` (the parent
    /// container) — callers are expected to avoid writing at that path.
    ///
    /// Nested dialogs and any pending-change paths derived from this dialog
    /// must be rooted here — not at `map_path` — otherwise the entry key
    /// segment is dropped and changes land under `""` in the saved config.
    pub fn entry_path(&self) -> String {
        // Use the live key field so new entries pick up whatever the user
        // has typed before opening a nested dialog. For existing entries
        // the key field is read-only and equals `entry_key`, so this is
        // consistent with the on-disk path.
        let key = self.get_key();
        if key.is_empty() {
            self.map_path.clone()
        } else {
            format!("{}/{}", self.map_path, key)
        }
    }

    /// Get button count (3 for existing entries with Delete, 2 for new/no_delete entries)
    pub fn button_count(&self) -> usize {
        if self.is_new || self.no_delete {
            2 // Save, Cancel (no Delete for new entries or when no_delete is set)
        } else {
            3
        }
    }

    /// True when the user has made *any* change to the dialog since
    /// it was opened. Tracked as an explicit flag (`user_edited`)
    /// rather than comparing `to_value() != original_value`, because
    /// the rebuilt JSON shape can differ from the input shape by
    /// schema-default normalization (e.g. an absent optional field
    /// rebuilds as an explicit empty string) — which would make the
    /// dialog read as dirty at open, with no user input.
    ///
    /// Used to gate the Esc 'Discard changes?' prompt and to drive
    /// the title-bar modified indicator.
    /// The window's handle, so the keyboard can move it to a field. The
    /// twin of `SettingsState::body_anchor`, one surface in.
    pub fn anchor(&self) -> std::rc::Rc<fresh_ui::behavior::Anchor> {
        self.body_anchor.clone()
    }

    /// The label column its scalar fields align their value cells against.
    ///
    /// **Content, not geometry.** The painter capped this at half the box's
    /// inner width and *excluded* any label wider than the cap rather than
    /// clamping it, so one long name could not push every value across. The
    /// cap is a constant here: the width it was half of is the tree's now,
    /// and a form whose labels approach forty columns has a naming problem
    /// rather than a layout one.
    pub fn label_column(&self) -> Option<u16> {
        const CAP: u16 = 40;
        self.items
            .iter()
            .map(|item| item.name.len() as u16 + 2)
            .filter(|&w| w <= CAP)
            .max()
    }

    /// The one line of contextual help above the buttons: what the focused
    /// field is for, or what Enter does on a list's pending row.
    pub fn helper_line(&self) -> Option<String> {
        if self.focus_on_buttons {
            return None;
        }
        // A text list says what Enter and Esc do on it rather than absorbing
        // them silently: how to open its add row, and what the field that is
        // open takes.
        let pending = self.current_item().and_then(|it| match &it.control {
            SettingControl::TextList { .. } => {
                Some(match live::text_list::live_row(&self.controls, &it.path) {
                    None => "Press Enter (or type) to add a new item; ↓/Tab to leave",
                    Some(None) => match live::text_list::draft(&self.controls, &it.path)
                        .is_some_and(|d| !d.is_empty())
                    {
                        true => "Editing new item — Enter to add, Esc to cancel",
                        false => "Type the new item — Enter to add, Esc to cancel",
                    },
                    Some(Some(_)) => "Editing item — ↑↓ other rows, Del removes it, Tab/Esc done",
                })
            }
            _ => None,
        });
        pending.map(String::from).or_else(|| {
            self.current_item()
                .and_then(|it| it.description.as_deref())
                .filter(|d| !d.is_empty())
                .map(String::from)
        })
    }

    /// The key legend under the buttons, or the warning that replaces it when
    /// a field will not parse.
    pub fn legend_line(&self) -> (String, bool) {
        let editing_json = self.is_editing_json();
        let (invalid, is_json) = self
            .current_item()
            .map(|item| match &item.control {
                SettingControl::Json { text, .. } => {
                    (!super::items::json_is_valid(text), editing_json)
                }
                _ => (false, false),
            })
            .unwrap_or((false, false));
        let text = if invalid && !is_json {
            return ("⚠ Invalid JSON - fix before leaving field".into(), true);
        } else if invalid {
            return ("⚠ Invalid JSON".into(), true);
        } else if is_json {
            "↑↓←→:Move  Enter:Newline  Tab/Esc:Exit"
        } else if self.is_editing() {
            "Enter/Tab:Commit field  Esc:Cancel"
        } else {
            // The `●:modified` legend is the only place that explains the
            // row indicator.
            "↑↓:Navigate  Tab:Fields/Buttons  Enter:Edit/Apply  Ctrl+S:Save  Esc:Cancel  ●:modified"
        };
        (text.into(), false)
    }

    pub fn is_dirty(&self) -> bool {
        self.user_edited
    }

    /// Mark the dialog as edited. Called from every mutator path
    /// (insert_char, toggle_bool, list add/remove, etc.) — anywhere
    /// the user can produce a change the dialog should remember.
    pub fn mark_edited(&mut self) {
        self.user_edited = true;
    }

    /// Mark the *focused field* as explicitly edited: flags the dialog dirty
    /// and clears the field's inherited state. Once the user gives a field a
    /// value of their own it is no longer inherited, so `to_value` persists it
    /// and the row shows a definite value rather than the neutral inherited
    /// chip. Call this from value-changing mutators (not cursor moves).
    fn mark_field_edited(&mut self) {
        self.user_edited = true;
        if let Some(item) = self.current_item_mut() {
            item.is_null = false;
            if let SettingControl::Toggle { inherited, .. } = &mut item.control {
                *inherited = false;
            }
        }
    }

    /// Reset the field at `idx` to *inherited* (unset). The value falls back to
    /// the global/default layer again: the row renders the neutral inherited
    /// chip / `(Inherited)` badge and `to_value` omits the field. Returns true
    /// if anything changed. No-op for read-only, non-nullable, or
    /// already-inherited fields.
    pub fn inherit_field(&mut self, idx: usize) -> bool {
        let Some(item) = self.items.get_mut(idx) else {
            return false;
        };
        if item.read_only || !item.nullable || item.is_null {
            return false;
        }
        item.is_null = true;
        item.modified = false;
        if let SettingControl::Toggle { inherited, .. } = &mut item.control {
            *inherited = true;
        }
        self.user_edited = true;
        true
    }

    /// Reset the field at `idx` to its built-in default value. Returns true if
    /// anything changed. No-op unless `reset_distinct_default` reports a
    /// distinct, non-inherited default to reset to.
    pub fn reset_field(&mut self, idx: usize) -> bool {
        let Some(default) = self.reset_distinct_default(idx) else {
            return false;
        };
        let Some(item) = self.items.get_mut(idx) else {
            return false;
        };
        super::state::update_control_from_value(&mut item.control, &default);
        // An explicit default value is a real (non-inherited) value.
        item.is_null = false;
        item.modified = false;
        if let SettingControl::Toggle { inherited, .. } = &mut item.control {
            *inherited = false;
        }
        self.user_edited = true;
        true
    }

    /// The value `[Reset]` would set, when reset is a *distinct, meaningful*
    /// action for the field at `idx` — i.e. a simple, editable field that
    /// currently overrides its built-in default, where that default isn't just
    /// `null` (which `[Inherit]` already covers). Returns `None` otherwise.
    fn reset_distinct_default(&self, idx: usize) -> Option<Value> {
        let item = self.items.get(idx)?;
        // Reset is offered for simple scalar controls and for object/JSON
        // controls (e.g. a language's `formatter`), whose only other per-field
        // action — Inherit → null — would *clear* a non-null built-in default
        // rather than restore it. Composite list/map controls and opaque
        // Complex controls are excluded.
        let resettable = is_simple_field_control(&item.control)
            || matches!(item.control, SettingControl::Json { .. });
        if item.read_only || item.is_null || !resettable {
            return None;
        }
        let default = item.default.as_ref()?;
        // A nullable field whose default is `null` resets to the same place as
        // Inherit, so don't offer a redundant Reset.
        if item.nullable && default.is_null() {
            return None;
        }
        // Only when the current value actually differs from the default.
        if control_to_value(&item.control) == *default {
            return None;
        }
        Some(default.clone())
    }

    /// True when unsetting the field at `idx` makes it inherit a parent-scope
    /// value (e.g. `editor.line_wrap`) rather than simply clearing it.
    fn field_inherits(&self, idx: usize) -> bool {
        self.items
            .get(idx)
            .map(|item| {
                self.inheritable_fields
                    .contains(item.path.trim_start_matches('/'))
            })
            .unwrap_or(false)
    }

    /// The per-field action buttons to render at the right edge of the field at
    /// `idx`, left to right, with their labels. Empty when the field offers
    /// none (e.g. inherited/unset, at its default, or read-only). The
    /// `(Inherited)` badge is rendered separately.
    pub fn field_action_buttons(&self, idx: usize) -> Vec<(FieldAction, String)> {
        let Some(item) = self.items.get(idx) else {
            return Vec::new();
        };
        if item.read_only {
            return Vec::new();
        }
        let mut buttons = Vec::new();
        if self.reset_distinct_default(idx).is_some() {
            buttons.push((
                FieldAction::Reset,
                format!("[{}]", t!("settings.btn_reset")),
            ));
        }
        // "Set to null" is offered for any overriding nullable field (composite
        // ones too — they're click-only, see `field_focusable_count`). The label
        // is [Inherit] when null falls back to a parent value, else [Clear].
        if item.nullable && !item.is_null {
            let label = if self.field_inherits(idx) {
                t!("settings.btn_inherit")
            } else {
                t!("settings.btn_clear")
            };
            buttons.push((FieldAction::Inherit, format!("[{}]", label)));
        }
        buttons
    }

    /// Perform the action button at focusable index `i` for the field at `idx`.
    fn perform_field_action(&mut self, idx: usize, action: FieldAction) -> bool {
        match action {
            FieldAction::Reset => self.reset_field(idx),
            FieldAction::Inherit => self.inherit_field(idx),
        }
    }

    /// Activate the currently keyboard-focused field action button, if any.
    /// Returns true if a button was focused (so the key is consumed).
    pub fn activate_focused_field_button(&mut self) -> bool {
        let Some(i) = self.field_button_focus else {
            return false;
        };
        if self.focus_on_buttons {
            return false;
        }
        let idx = self.selected_item;
        if let Some(action) = self.field_action_buttons(idx).get(i).map(|(a, _)| *a) {
            self.perform_field_action(idx, action);
        }
        self.field_button_focus = None;
        true
    }

    /// Convert dialog state back to JSON value (excludes the __key__ item)
    /// Auto-commit any draft text sitting in a TextList's trailing
    /// `[+] Add new` slot. Without this, saving a dialog while the user
    /// has typed (but not pressed Enter or ↓) into the new-item row
    /// silently drops that text — the diverging commit semantics
    /// between text fields ("typed value is just there") and list rows
    /// ("typing isn't enough — you must commit") was the F21 surprise.
    /// Run this from every save path so the saved value matches what
    /// the user sees on screen.
    pub fn commit_pending_list_drafts(&mut self) {
        for idx in 0..self.items.len() {
            if !matches!(self.items[idx].control, SettingControl::TextList { .. }) {
                continue;
            }
            let path = self.items[idx].path.clone();
            if let Some(text) = live::text_list::take_draft(&mut self.controls, &path) {
                if let SettingControl::TextList { items, .. } = &mut self.items[idx].control {
                    items.push(text);
                }
                self.user_edited = true;
            }
        }
    }

    pub fn to_value(&self) -> Value {
        // For single-value dialogs (non-Object schemas like ObjectArray),
        // return the control's value directly instead of wrapping in an Object.
        if self.is_single_value {
            for item in &self.items {
                if item.path != "__key__" {
                    return control_to_value(&item.control);
                }
            }
        }

        let mut obj = serde_json::Map::new();

        for item in &self.items {
            // Skip the special key item - it's stored separately
            if item.path == "__key__" {
                continue;
            }

            let field_name = item.path.trim_start_matches('/');

            // Preserve inheritance: a nullable field whose value is inherited
            // (`is_null`) must NOT be written back as a concrete value, or it
            // stops inheriting from the global/default layer. `is_null` starts
            // true for inherited fields, is cleared the moment the user edits
            // the field (`mark_field_edited`), and is set again by the per-field
            // Inherit action — so it precisely tracks "did the user give this
            // field a value of its own?". Without this, opening a language entry
            // and toggling one field would freeze every *other* inherited field
            // (e.g. writing `line_wrap: false`), which then overrides the global
            // Toggle Line Wrap command forever (issue #2345).
            if item.nullable && item.is_null {
                continue;
            }

            let value = control_to_value(&item.control);
            obj.insert(field_name.to_string(), value);
        }

        Value::Object(obj)
    }

    /// Get currently selected item
    pub fn current_item(&self) -> Option<&SettingItem> {
        if self.focus_on_buttons {
            None
        } else {
            self.items.get(self.selected_item)
        }
    }

    /// Get currently selected item mutably
    pub fn current_item_mut(&mut self) -> Option<&mut SettingItem> {
        if self.focus_on_buttons {
            None
        } else {
            self.items.get_mut(self.selected_item)
        }
    }

    /// Move focus to next editable item, navigating within composite controls first.
    ///
    /// For composite controls (Map, ObjectArray, TextList), Down first navigates
    /// through their internal entries and [+] Add new row before moving to the
    /// next dialog item. When at the last editable item, wraps to buttons.
    /// When on the last button, wraps back to the first editable item.
    /// Number of focusable per-field action buttons (`[Reset]`/`[Inherit]`/
    /// `[Clear]`) for the field at `idx`. Every field's buttons join the Tab
    /// order — for composite controls they come *after* the control's own
    /// internal sub-navigation (handled by `try_composite_focus_*`), so Tab is
    /// the sole keyboard path to these actions.
    fn field_focusable_count(&self, idx: usize) -> usize {
        self.field_action_buttons(idx).len()
    }

    /// Advance focus to the next *field* (control), skipping any per-field
    /// action buttons. Used by the form-style "commit and move on" flow
    /// (Enter/Tab/arrows while editing), where stopping on the field's own
    /// `[Reset]`/`[Inherit]` button would be surprising. Those buttons remain
    /// reachable via Tab in navigation mode.
    pub fn focus_next_field(&mut self) {
        if self.is_editing() {
            return;
        }
        self.field_button_focus = None;
        if self.selected_item + 1 < self.items.len() {
            self.select_field(self.selected_item + 1, true);
        } else {
            self.focus_on_buttons = true;
            self.focused_button = 0;
        }
        self.ensure_selected_visible(self.viewport_height);
    }

    /// Retreat focus to the previous *field* (control), skipping per-field
    /// action buttons. The arrow-key counterpart to [`focus_next_field`].
    pub fn focus_prev_field(&mut self) {
        if self.is_editing() {
            return;
        }
        self.field_button_focus = None;
        if self.selected_item > self.first_editable_index {
            self.select_field(self.selected_item - 1, false);
        } else {
            self.focus_on_buttons = true;
            self.focused_button = self.button_count().saturating_sub(1);
        }
        self.ensure_selected_visible(self.viewport_height);
    }

    pub fn focus_next(&mut self) {
        if self.is_editing() {
            return;
        }

        if self.focus_on_buttons {
            if self.focused_button + 1 < self.button_count() {
                self.focused_button += 1;
            } else {
                // Wrap to first editable item
                if self.first_editable_index < self.items.len() {
                    self.focus_on_buttons = false;
                    self.field_button_focus = None;
                    self.select_field(self.first_editable_index, true);
                }
            }
        } else if let Some(i) = self.field_button_focus {
            // Advance through this field's action buttons, then to the next field.
            if i + 1 < self.field_focusable_count(self.selected_item) {
                self.field_button_focus = Some(i + 1);
            } else {
                self.field_button_focus = None;
                if self.selected_item + 1 < self.items.len() {
                    self.select_field(self.selected_item + 1, true);
                } else {
                    self.focus_on_buttons = true;
                    self.focused_button = 0;
                }
            }
        } else {
            // Try navigating within a composite control first
            let handled = self.try_composite_focus_next();
            if !handled {
                // Composite at its exit boundary (or not a composite). Stop on
                // this field's action buttons before advancing, if it has any.
                if self.field_focusable_count(self.selected_item) > 0 {
                    self.field_button_focus = Some(0);
                } else if self.selected_item + 1 < self.items.len() {
                    self.select_field(self.selected_item + 1, true);
                } else {
                    // Past last item, go to buttons
                    self.focus_on_buttons = true;
                    self.focused_button = 0;
                }
            }
        }

        self.ensure_selected_visible(self.viewport_height);
    }

    /// Move focus to previous editable item, navigating within composite controls first.
    ///
    /// For composite controls, Up first navigates backwards through their internal
    /// entries before moving to the previous dialog item. When at the first editable
    /// item, wraps to buttons. When on the first button, wraps back to the last item.
    pub fn focus_prev(&mut self) {
        if self.is_editing() {
            return;
        }

        if self.focus_on_buttons {
            if self.focused_button > 0 {
                self.focused_button -= 1;
            } else {
                // Wrap to last editable item
                if self.first_editable_index < self.items.len() {
                    self.focus_on_buttons = false;
                    self.select_field(self.items.len().saturating_sub(1), false);
                    // Land on the field's last action button, if it has any.
                    self.field_button_focus = self
                        .field_focusable_count(self.selected_item)
                        .checked_sub(1);
                }
            }
        } else if let Some(i) = self.field_button_focus {
            // Step back through the action buttons, then to the control.
            self.field_button_focus = i.checked_sub(1);
        } else {
            // Try navigating within a composite control first
            let handled = self.try_composite_focus_prev();
            if !handled {
                // Composite is at its entry boundary (or not a composite) — go to previous item
                if self.selected_item > self.first_editable_index {
                    self.select_field(self.selected_item - 1, false);
                    // Going backwards lands on the previous field's last
                    // element, which is its last action button when present.
                    self.field_button_focus = self
                        .field_focusable_count(self.selected_item)
                        .checked_sub(1);
                } else {
                    // Before first editable item, go to buttons
                    self.focus_on_buttons = true;
                    self.focused_button = self.button_count().saturating_sub(1);
                }
            }
        }

        self.ensure_selected_visible(self.viewport_height);
    }

    /// Step a map's or an object array's list cursor down (`true`) — the
    /// kind's own Down — and say whether it moved: at the last row it does
    /// not, and the dialog moves on to the next field.
    fn try_composite_focus_next(&mut self) -> bool {
        self.list_cursor_step(1)
    }

    /// The same, up: at the first row the dialog moves to the previous
    /// field.
    fn try_composite_focus_prev(&mut self) -> bool {
        self.list_cursor_step(-1)
    }

    fn list_cursor_step(&mut self, delta: i32) -> bool {
        if !self
            .current_item()
            .is_some_and(|i| i.control.has_list_rows())
        {
            return false;
        }
        let before = self.composite_cursor();
        let code = match delta < 0 {
            true => crossterm::event::KeyCode::Up,
            false => crossterm::event::KeyCode::Down,
        };
        self.live_dispatch(&crossterm::event::KeyEvent::new(
            code,
            crossterm::event::KeyModifiers::NONE,
        ));
        let after = self.composite_cursor();
        if let (Some(row), Some(item)) = (after, self.current_item()) {
            // The field's window is asked to hold the cursor's row.
            self.body_anchor
                .reveal_key(item.control.row_tree_key(&item.path, row));
        }
        after != before
    }

    /// Move the selection to field `idx`, leaving whatever was live on the
    /// field it leaves, and entering the new one: a map's or an object
    /// array's list takes the keyboard with its cursor on the first row
    /// when entered from above, the add row from below.
    fn select_field(&mut self, idx: usize, from_above: bool) {
        self.stop_editing();
        self.selected_item = idx;
        self.init_composite_focus(from_above);
    }

    /// Initialize a composite control's focus when entering it.
    /// `from_above`: true = entering from the item above (start at first entry),
    ///               false = entering from below (start at the add row).
    ///
    /// A text list's rows are fields, and the form enters them in order:
    /// the first item's field from above, the add row's from below.
    fn init_composite_focus(&mut self, from_above: bool) {
        let Some(item) = self.items.get(self.selected_item) else {
            return;
        };
        if let SettingControl::TextList { items, .. } = &item.control {
            let row = (from_above && !items.is_empty()).then_some(0);
            self.edit_list_row(row);
            return;
        }
        if !item.control.has_list_rows() {
            return;
        }
        let n = item.control.list_row_count();
        if n == 0 {
            return;
        }
        let path = item.path.clone();
        live::seed_list(
            &mut self.controls,
            &path,
            if from_above { 0 } else { n - 1 },
        );
    }

    /// The row the selected field's list cursor is on, while the list has
    /// the keyboard: an entry of the map or the object array, or its add
    /// row (`SettingControl::add_row`).
    pub fn composite_cursor(&self) -> Option<usize> {
        let item = self.current_item()?;
        self.composite_cursor_of(item)
    }

    /// [`composite_cursor`](Self::composite_cursor) for any field.
    pub fn composite_cursor_of(&self, item: &SettingItem) -> Option<usize> {
        if !item.control.has_list_rows() || self.controls.focus_key != item.path {
            return None;
        }
        let spec = super::widget_map::live_widget(&item.path, &item.control, &item.path);
        live::list_row(&self.controls, &spec, &item.path)
    }

    /// The store's focus key when it names `item`'s control or one of its
    /// rows — what the field paints as focused.
    pub fn focus_key_of(&self, item: &SettingItem) -> Option<&str> {
        let key = self.controls.focus_key.as_str();
        (key == item.path
            || key
                .strip_prefix(&item.path)
                .is_some_and(|r| r.starts_with("::")))
        .then_some(key)
    }

    /// Which row of field `idx` the cursor sits on, for the `>` beside it:
    /// zero, the label row, for a scalar; a map's or an object array's
    /// cursor row (after the header, when there is one); a text list's live
    /// field.
    pub fn cursor_row(&self, idx: usize) -> u16 {
        let Some(item) = self.items.get(idx) else {
            return 0;
        };
        match &item.control {
            SettingControl::Map { display_field, .. } => self
                .composite_cursor_of(item)
                .map(|r| 1 + u16::from(display_field.is_some()) + r as u16)
                .unwrap_or(0),
            SettingControl::ObjectArray { .. } => self
                .composite_cursor_of(item)
                .map(|r| 1 + r as u16)
                .unwrap_or(0),
            SettingControl::TextList { items, .. } => {
                match live::text_list::live_row(&self.controls, &item.path) {
                    Some(row) => 1 + row.unwrap_or(items.len()) as u16,
                    None => 0,
                }
            }
            _ => 0,
        }
    }

    /// Toggle focus between items region and buttons region.
    /// Used by Tab key to provide region-level navigation.
    pub fn toggle_focus_region(&mut self) {
        self.toggle_focus_region_direction(true);
    }

    /// Toggle between items and buttons regions.
    /// When in buttons region, Tab cycles through buttons before returning to items.
    /// `forward` controls direction: true = Tab, false = Shift+Tab.
    pub fn toggle_focus_region_direction(&mut self, forward: bool) {
        if self.is_editing() {
            return;
        }

        if self.focus_on_buttons {
            if forward {
                // Tab forward through buttons, then back to items
                if self.focused_button + 1 < self.button_count() {
                    self.focused_button += 1;
                } else {
                    // Past last button — return to items
                    if self.first_editable_index < self.items.len() {
                        self.focus_on_buttons = false;
                        if self.selected_item < self.first_editable_index {
                            self.selected_item = self.first_editable_index;
                        }
                    } else {
                        // All items read-only, wrap to first button
                        self.focused_button = 0;
                    }
                }
            } else {
                // Shift+Tab backward through buttons, then back to items
                if self.focused_button > 0 {
                    self.focused_button -= 1;
                } else {
                    // Before first button — return to items
                    if self.first_editable_index < self.items.len() {
                        self.focus_on_buttons = false;
                        if self.selected_item < self.first_editable_index {
                            self.selected_item = self.first_editable_index;
                        }
                    } else {
                        // All items read-only, wrap to last button
                        self.focused_button = self.button_count().saturating_sub(1);
                    }
                }
            }
        } else {
            // Move to buttons
            self.focus_on_buttons = true;
            self.focused_button = if forward {
                0
            } else {
                self.button_count().saturating_sub(1)
            };
        }

        self.ensure_selected_visible(self.viewport_height);
    }

    /// Initialize composite control focus for the selected item (when dialog opens)
    fn init_object_array_focus(&mut self) {
        self.init_composite_focus(true);
    }

    /// Ensure the selected item is visible within the viewport
    /// Move the field window so the cursor's field is in it.
    ///
    /// **One call, where three walks of every field's height used to be.**
    /// `selected_item_offset` summed the rows above the cursor and
    /// `selected_item_height` measured the cursor's own, both from
    /// `control_height` — the same numbers the painter drew each field with,
    /// kept in step by hand. The field is a band the layout measured; the
    /// window is asked to hold it, by name.
    ///
    /// The buttons are not in the window, so putting the keyboard on them
    /// asks for the last field instead — which is what "scroll to bottom"
    /// meant.
    pub fn ensure_selected_visible(&mut self, _viewport_height: usize) {
        let target = match self.focus_on_buttons {
            true => self.items.len().saturating_sub(1),
            false => self.selected_item,
        };
        self.body_anchor
            .reveal_key(crate::view::shell::entry::item_key(target));
    }

    /// Ensure the cursor within a JSON editor is visible
    ///
    /// When editing a multiline JSON control, this moves the field window
    /// to keep the caret's row in it.
    pub fn ensure_cursor_visible(&mut self) {
        if self.focus_on_buttons || !self.is_editing_json() {
            return;
        }
        let Some(path) = self.live_control() else {
            return;
        };
        let cursor_row = match self.controls.instance_states.get(&path) {
            Some(crate::widgets::WidgetInstanceState::Text { editor, .. }) => editor.cursor_row,
            _ => return,
        };

        // The caret's row *within the field* — its label row, then the line
        // the cursor is on. Where that field starts in the column is the
        // window's business, not this one's: it used to be
        // `selected_item_offset()`, a sum of every field above it.
        self.body_anchor.reveal_key_at(
            crate::view::shell::entry::item_key(self.selected_item),
            1 + cursor_row as u32,
        );
    }

    /// Scroll up by one line
    pub fn scroll_up(&mut self) {
        self.scroll_by(-1);
    }

    /// Scroll down by one line
    pub fn scroll_down(&mut self, _viewport_height: usize) {
        self.scroll_by(1);
    }

    /// Move the field window by `delta` rows.
    ///
    /// The window clamps itself against the column it holds, so there is no
    /// content height to compute here — which is what `total_content_height`
    /// was for, and it was the fourth walk of every field's rows.
    fn scroll_by(&mut self, delta: i32) {
        let y = (self.scroll_offset as i32 + delta).max(0);
        self.body_anchor.scroll_to(fresh_ui::Point::new(0, y));
        self.scroll_offset = y as usize;
    }

    /// Enter on the selected field, or a press that means the same: the
    /// field is edited from here on. A scalar's kind acts
    /// (`activate_control`); a composite flips its own edit flag, with a
    /// snapshot for Escape.
    pub fn start_editing(&mut self) {
        if self.focus_on_buttons {
            return;
        }
        if let Some(item) = self.items.get(self.selected_item) {
            if item.read_only {
                return;
            }
            // A text field, a number, a JSON editor, a dual list and a text
            // list open their edit; a toggle and a dropdown are *activated*
            // (Enter, Space) rather than edited, and a map's or an object
            // array's rows open the nested dialog, so selecting those is all
            // that happens here.
            if matches!(
                item.control,
                SettingControl::Text { .. }
                    | SettingControl::Number { .. }
                    | SettingControl::Json { .. }
                    | SettingControl::DualList { .. }
                    | SettingControl::TextList { .. }
            ) {
                self.activate_control();
            }
        }
    }

    /// The selected field's control as its kind sees it, keyed by its path.
    fn current_spec(&self) -> Option<(String, fresh_core::api::WidgetSpec)> {
        let item = self.current_item()?;
        Some((item.path.clone(), self.spec_for(&item.path)?))
    }

    /// The node of the selected field's description that carries `key`:
    /// the control's own, or one of a text list's rows.
    fn spec_for(&self, key: &str) -> Option<fresh_core::api::WidgetSpec> {
        let item = self.current_item()?;
        Some(super::widget_map::live_widget(
            &item.path,
            &item.control,
            key,
        ))
    }

    /// The key of the live control: the selected field's, or one of its
    /// rows', when the store's focus names it.
    pub fn live_control(&self) -> Option<String> {
        let item = self.current_item()?;
        (!self.focus_on_buttons
            && live::kind_edited(&item.control)
            && self.focus_key_of(item).is_some())
        .then(|| self.controls.focus_key.clone())
    }

    /// Whether the selected field's JSON editor is being edited.
    pub fn is_editing_json(&self) -> bool {
        self.live_control().is_some()
            && matches!(
                self.current_item().map(|i| &i.control),
                Some(SettingControl::Json { .. })
            )
    }

    /// Whether the selected field's JSON text will save.
    pub fn json_field_valid(&self) -> bool {
        match self.current_item().map(|i| &i.control) {
            Some(SettingControl::Json { text, .. }) => super::items::json_is_valid(text),
            _ => true,
        }
    }

    /// Whether the selected field's dropdown has its list up.
    pub fn is_dropdown_open(&self) -> bool {
        self.current_item().is_some_and(|item| {
            matches!(item.control, SettingControl::Dropdown { .. })
                && crate::widgets::kinds::dropdown::is_open(&item.path, &self.controls)
        })
    }

    /// Whether the selected field's text is being edited.
    pub fn is_editing_text_field(&self) -> bool {
        self.live_control().is_some()
            && matches!(
                self.current_item().map(|i| &i.control),
                Some(SettingControl::Text { .. })
            )
    }

    /// The kinds' events onto the field: a change marks the field edited
    /// (and the dialog dirty). Then: the field stays live while its kind
    /// holds it.
    fn absorb(&mut self, key: &str, events: &[(String, Value)]) {
        let changed = match self.current_item_mut() {
            Some(item) => live::apply(&mut item.control, key, events),
            None => false,
        };
        if changed {
            self.mark_field_edited();
        }
        let Some(path) = self.live_control() else {
            return;
        };
        let held = self
            .current_item()
            .is_some_and(|i| live::kind_holds(&i.control, &self.controls, &path));
        if !held {
            live::drop_state(&mut self.controls, &path);
        }
    }

    /// Enter (or its press) on the selected kind-edited field: a toggle
    /// flips, a number opens its draft, a dropdown its list, a text field
    /// or a JSON editor its editor with the caret at the end, a dual list
    /// takes the keyboard.
    pub fn activate_control(&mut self) {
        if self.focus_on_buttons || self.current_item().is_none_or(|i| i.read_only) {
            return;
        }
        let Some((path, spec)) = self.current_spec() else {
            return;
        };
        match self.current_item().map(|i| &i.control) {
            Some(SettingControl::Toggle { .. }) => {
                let o = live::named(&mut self.controls, &spec, &path, "Enter");
                self.absorb(&path, &o.fx.events);
            }
            Some(SettingControl::Number { .. } | SettingControl::Dropdown { .. }) => {
                self.controls.focus_key = path.clone();
                let o = live::named(&mut self.controls, &spec, &path, "Enter");
                self.absorb(&path, &o.fx.events);
            }
            Some(SettingControl::Text { .. } | SettingControl::Json { .. }) => {
                self.begin_text_edit()
            }
            Some(SettingControl::DualList { .. }) => self.controls.focus_key = path,
            // A field of the text list already live keeps the keyboard;
            // otherwise its add row opens. A map's or an object array's rows
            // open the nested dialog, which is the settings state's to do.
            Some(SettingControl::TextList { .. })
                if live::text_list::live_row(&self.controls, &path).is_none() =>
            {
                self.edit_list_row(None)
            }
            _ => {}
        }
    }

    // =========== Text lists: rows as fields ===========

    /// The row of the selected text list whose field is live: `Some(i)`
    /// an item's, `None` the add row's.
    pub fn live_list_row(&self) -> Option<Option<usize>> {
        let item = self.current_item()?;
        live::text_list::live_row(&self.controls, &item.path)
    }

    /// Open a row of the selected text list for editing — an item's field,
    /// or the add row's for `None` — the caret at the end, with a snapshot
    /// for Escape. A draft in the add row becomes an item first.
    pub fn edit_list_row(&mut self, row: Option<usize>) {
        if row.is_some() {
            self.commit_list_draft();
        }
        let Some(item) = self.current_item() else {
            return;
        };
        let SettingControl::TextList { items, .. } = &item.control else {
            return;
        };
        let (path, items) = (item.path.clone(), items.clone());
        if self.edit_snapshot.is_none() {
            self.edit_snapshot = Some(FieldEditSnapshot {
                item_index: self.selected_item,
                control: item.control.clone(),
                is_null: item.is_null,
                user_edited: self.user_edited,
            });
        }
        live::text_list::edit_row(&mut self.controls, &path, &items, row);
        // The field's window is asked to hold the row that opened.
        if let Some(item) = self.current_item() {
            let n = items.len();
            self.body_anchor
                .reveal_key(item.control.row_tree_key(&item.path, row.unwrap_or(n)));
        }
    }

    /// The add row's draft becomes an item. Returns whether one did.
    fn commit_list_draft(&mut self) -> bool {
        let Some(item) = self.current_item() else {
            return false;
        };
        let path = item.path.clone();
        let Some(text) = live::text_list::take_draft(&mut self.controls, &path) else {
            return false;
        };
        self.mark_field_edited();
        if let Some(SettingControl::TextList { items, .. }) =
            self.current_item_mut().map(|i| &mut i.control)
        {
            items.push(text);
        }
        true
    }

    /// Up or Down in a live text list field: the adjacent row's field
    /// opens — the add row's after the last item. Returns whether the
    /// keyboard moved; at either end it did not, and the dialog moves on.
    pub fn list_row_step(&mut self, delta: i32) -> bool {
        let Some(live) = self.live_list_row() else {
            return false;
        };
        // A draft in the add row becomes an item first, so the row above
        // the add row is the one just typed.
        if live.is_none() {
            self.commit_list_draft();
        }
        let Some(SettingControl::TextList { items, .. }) = self.current_item().map(|i| &i.control)
        else {
            return false;
        };
        let n = items.len();
        let cur = live.unwrap_or(n) as i32;
        let target = cur + delta;
        if target < 0 || target > n as i32 {
            return false;
        }
        let target = target as usize;
        self.edit_list_row((target < n).then_some(target));
        true
    }

    /// Enter in a live text list field: the add row's draft becomes an
    /// item and the add row stays open for the next; an item's field keeps
    /// the keyboard.
    pub fn list_row_enter(&mut self) {
        if self.live_list_row() == Some(None) && self.commit_list_draft() {
            self.edit_list_row(None);
        }
    }

    /// Remove item `i` of the selected text list. A field live on it moves
    /// to the row that takes its place.
    pub fn remove_list_row(&mut self, i: usize) {
        let live = self.live_list_row();
        let Some(SettingControl::TextList { items, .. }) =
            self.current_item_mut().map(|it| &mut it.control)
        else {
            return;
        };
        if i >= items.len() {
            return;
        }
        items.remove(i);
        let n = items.len();
        self.mark_field_edited();
        if let Some(row) = live {
            if let Some(item) = self.current_item() {
                let path = item.path.clone();
                live::text_list::leave(&mut self.controls, &path);
            }
            let row = match row {
                Some(r) if r > i => Some(r - 1),
                Some(r) if r == i => (r < n).then_some(r),
                other => other,
            };
            self.edit_list_row(row);
        }
    }

    /// A press on a row of the selected field's list: the list takes the
    /// keyboard with its cursor on the row.
    pub fn select_list_row(&mut self, row: usize) {
        let Some((path, spec)) = self.current_spec() else {
            return;
        };
        if !self
            .current_item()
            .is_some_and(|i| i.control.has_list_rows())
        {
            return;
        }
        self.controls.focus_key = path.clone();
        let o = live::pointer(
            &mut self.controls,
            &spec,
            &path,
            "select",
            &serde_json::json!({ "index": row }),
        );
        self.absorb(&path, &o.fx.events);
    }

    /// Open the selected text field or JSON editor, the caret at the end.
    /// An unset JSON value opens empty, so what is typed is the value
    /// rather than an edit of the `null` literal.
    fn begin_text_edit(&mut self) {
        let Some(item) = self.current_item() else {
            return;
        };
        let (seed, multiline) = match &item.control {
            SettingControl::Text { value, .. } => (value.clone(), false),
            SettingControl::Json { text, .. } => match super::items::json_is_unset(text) {
                true => (String::new(), true),
                false => (text.clone(), true),
            },
            _ => return,
        };
        let path = item.path.clone();
        self.edit_snapshot = Some(FieldEditSnapshot {
            item_index: self.selected_item,
            control: item.control.clone(),
            is_null: item.is_null,
            user_edited: self.user_edited,
        });
        live::seed_text(&mut self.controls, &path, &seed, false, multiline);
        self.controls.focus_key = path;
    }

    /// A keystroke while a field is live: the kind's, then the field's.
    /// `None` when nothing is live.
    pub fn live_dispatch(&mut self, ev: &crossterm::event::KeyEvent) -> Option<live::Outcome> {
        let key = self.live_control()?;
        let spec = self.spec_for(&key)?;
        let outcome = live::key(&mut self.controls, &spec, &key, ev);
        self.absorb(&key, &outcome.fx.events);
        self.ensure_cursor_visible();
        Some(outcome)
    }

    /// Type into the live field: a paste, or the character that began the
    /// edit.
    fn live_text(&mut self, text: &str) -> bool {
        let Some(key) = self.live_control() else {
            return false;
        };
        let Some(spec) = self.spec_for(&key) else {
            return false;
        };
        let outcome = live::text(&mut self.controls, &spec, &key, text);
        self.absorb(&key, &outcome.fx.events);
        true
    }

    /// Typing on a field that is not yet live: the field becomes live and
    /// the text is its kind's — a text field or a JSON editor types it at
    /// the end, a number opens its draft with it.
    pub fn type_into_control(&mut self, text: &str) {
        if self.live_control().is_none() {
            match self.current_item().map(|i| (&i.control, i.read_only)) {
                Some((SettingControl::Text { .. } | SettingControl::Json { .. }, false)) => {
                    self.begin_text_edit()
                }
                Some((SettingControl::TextList { .. }, false)) => self.edit_list_row(None),
                Some((SettingControl::Number { .. }, false)) => {
                    if let Some((path, _)) = self.current_spec() {
                        self.controls.focus_key = path;
                    }
                }
                _ => return,
            }
        }
        self.live_text(text);
    }

    /// Move the live text field's caret to a byte of its value — a press.
    pub fn position_text_cursor(&mut self, byte: usize) {
        let Some(path) = self.live_control() else {
            return;
        };
        if let Some(editor) = live::text_editor(&mut self.controls, &path) {
            editor.clear_selection();
            editor.set_cursor_from_flat(byte);
        }
    }

    /// Paste into whatever is being edited. Returns whether the text
    /// landed anywhere.
    pub fn paste(&mut self, text: &str) -> bool {
        self.live_text(text)
    }

    /// Commit text editing mode. This is the *accept* path — Enter, Tab, and
    /// clicking away (blur) all land here, following the platform convention
    /// that those gestures keep the typed value. Esc takes `revert_editing`
    /// instead. A live scalar is left the same way: a text field keeps what
    /// was typed, a number's draft is committed, a dropdown's list closed.
    pub fn stop_editing(&mut self) {
        if let Some(path) = self.live_control() {
            if let Some((_, spec)) = self.current_spec() {
                let name = match self.current_item().map(|i| &i.control) {
                    Some(SettingControl::TextList { .. }) => {
                        // The add row's draft is kept: leaving a field
                        // commits it (F21).
                        self.commit_list_draft();
                        None
                    }
                    Some(
                        SettingControl::Text { .. }
                        | SettingControl::Json { .. }
                        | SettingControl::DualList { .. }
                        | SettingControl::Map { .. }
                        | SettingControl::ObjectArray { .. },
                    ) => None,
                    Some(SettingControl::Dropdown { .. }) => Some("Escape"),
                    _ => Some("Enter"),
                };
                if let Some(name) = name {
                    let o = live::named(&mut self.controls, &spec, &path, name);
                    self.absorb(&path, &o.fx.events);
                }
            }
            live::drop_state(&mut self.controls, &path);
        }
        // The edit was accepted — there is nothing left to revert.
        self.edit_snapshot = None;
    }

    /// Cancel editing, discarding the in-progress edit and restoring the
    /// field to its pre-edit state. This is the Esc path — matching the
    /// Windows/macOS/web convention where Esc reverts an edit while Enter and
    /// Tab commit it. A number's draft is the kind's and is abandoned there;
    /// a text field's value comes back from the snapshot.
    pub fn revert_editing(&mut self) {
        if let Some(path) = self.live_control() {
            if let Some((_, spec)) = self.current_spec() {
                if matches!(
                    self.current_item().map(|i| &i.control),
                    Some(SettingControl::Number { .. } | SettingControl::Dropdown { .. })
                ) {
                    let o = live::named(&mut self.controls, &spec, &path, "Escape");
                    self.absorb(&path, &o.fx.events);
                }
            }
            live::drop_state(&mut self.controls, &path);
        }
        if let Some(snap) = self.edit_snapshot.take() {
            if let Some(item) = self.items.get_mut(snap.item_index) {
                item.control = snap.control;
                item.is_null = snap.is_null;
            }
            // Restore the dialog's dirty flag to what it was before this edit,
            // so reverting the *only* change also drops the "unsaved" state
            // while preserving edits made to other fields earlier.
            self.user_edited = snap.user_edited;
        }
    }

    /// Check if any field is currently in edit mode
    pub fn is_editing(&self) -> bool {
        self.live_control().is_some()
            && !self
                .current_item()
                .is_some_and(|i| i.control.has_list_rows())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_schema() -> SettingSchema {
        SettingSchema {
            path: "/test".to_string(),
            name: "Test".to_string(),
            description: Some("Test schema".to_string()),
            setting_type: SettingType::Object {
                properties: vec![
                    SettingSchema {
                        path: "/enabled".to_string(),
                        name: "Enabled".to_string(),
                        description: Some("Enable this".to_string()),
                        setting_type: SettingType::Boolean,
                        default: Some(serde_json::json!(true)),
                        read_only: false,
                        section: None,
                        order: None,
                        nullable: false,
                        enum_from: None,
                        dual_list_sibling: None,
                        dynamically_extendable_status_bar_elements: false,
                    },
                    SettingSchema {
                        path: "/command".to_string(),
                        name: "Command".to_string(),
                        description: Some("Command to run".to_string()),
                        setting_type: SettingType::String,
                        default: Some(serde_json::json!("")),
                        read_only: false,
                        section: None,
                        order: None,
                        nullable: false,
                        enum_from: None,
                        dual_list_sibling: None,
                        dynamically_extendable_status_bar_elements: false,
                    },
                ],
            },
            default: None,
            read_only: false,
            section: None,
            order: None,
            nullable: false,
            enum_from: None,
            dual_list_sibling: None,
            dynamically_extendable_status_bar_elements: false,
        }
    }

    #[test]
    fn from_schema_creates_key_item_first() {
        let schema = create_test_schema();
        let dialog = EntryDialogState::from_schema(
            "test".to_string(),
            &serde_json::json!({}),
            &schema,
            "/test",
            false,
            false,
            &HashMap::new(),
        );

        assert!(!dialog.items.is_empty());
        assert_eq!(dialog.items[0].path, "__key__");
        assert_eq!(dialog.items[0].name, "Key");
    }

    #[test]
    fn from_schema_creates_items_from_properties() {
        let schema = create_test_schema();
        let dialog = EntryDialogState::from_schema(
            "test".to_string(),
            &serde_json::json!({"enabled": true, "command": "test-cmd"}),
            &schema,
            "/test",
            false,
            false,
            &HashMap::new(),
        );

        // Key + 2 properties = 3 items
        assert_eq!(dialog.items.len(), 3);
        assert_eq!(dialog.items[1].name, "Enabled");
        assert_eq!(dialog.items[2].name, "Command");
    }

    #[test]
    fn get_key_returns_key_value() {
        let schema = create_test_schema();
        let dialog = EntryDialogState::from_schema(
            "mykey".to_string(),
            &serde_json::json!({}),
            &schema,
            "/test",
            false,
            false,
            &HashMap::new(),
        );

        assert_eq!(dialog.get_key(), "mykey");
    }

    #[test]
    fn to_value_excludes_key() {
        let schema = create_test_schema();
        let dialog = EntryDialogState::from_schema(
            "test".to_string(),
            &serde_json::json!({"enabled": true, "command": "cmd"}),
            &schema,
            "/test",
            false,
            false,
            &HashMap::new(),
        );

        let value = dialog.to_value();
        assert!(value.get("__key__").is_none());
        assert!(value.get("enabled").is_some());
    }

    #[test]
    fn focus_navigation_works() {
        let schema = create_test_schema();
        let mut dialog = EntryDialogState::from_schema(
            "test".to_string(),
            &serde_json::json!({}),
            &schema,
            "/test",
            false, // existing entry - Key is read-only
            false, // allow delete
            &HashMap::new(),
        );

        // With is_new=false, Key is read-only and sorted first
        // Items: [Key (read-only), Enabled, Command]
        // Focus starts at first editable item (index 1)
        assert_eq!(dialog.first_editable_index, 1);
        assert_eq!(dialog.selected_item, 1); // First editable (Enabled)
        assert!(!dialog.focus_on_buttons);

        dialog.focus_next();
        assert_eq!(dialog.selected_item, 2); // Command

        dialog.focus_next();
        assert!(dialog.focus_on_buttons); // No more editable items
        assert_eq!(dialog.focused_button, 0);

        // Going back should skip read-only Key
        dialog.focus_prev();
        assert!(!dialog.focus_on_buttons);
        assert_eq!(dialog.selected_item, 2); // Last editable (Command)

        dialog.focus_prev();
        assert_eq!(dialog.selected_item, 1); // First editable (Enabled)

        dialog.focus_prev();
        assert!(dialog.focus_on_buttons); // Wraps to buttons, not to read-only Key
    }

    /// A nullable field whose built-in default is itself non-null *and* is
    /// currently overridden to a third value offers both `[Reset]` and
    /// `[Inherit]`. Tab must step control → Reset → Inherit → (footer), and
    /// Shift+Tab must reverse exactly: (footer) → Inherit → Reset → control.
    #[test]
    fn focus_cycles_through_both_field_action_buttons() {
        let schema = SettingSchema {
            path: "/test".to_string(),
            name: "Test".to_string(),
            description: None,
            setting_type: SettingType::Object {
                properties: vec![SettingSchema {
                    path: "/wrap".to_string(),
                    name: "Wrap".to_string(),
                    description: None,
                    setting_type: SettingType::Boolean,
                    // Non-null built-in default, so Reset (→true) differs from
                    // Inherit (→null).
                    default: Some(serde_json::json!(true)),
                    read_only: false,
                    section: None,
                    order: None,
                    nullable: true,
                    enum_from: None,
                    dual_list_sibling: None,
                    dynamically_extendable_status_bar_elements: false,
                }],
            },
            default: None,
            read_only: false,
            section: None,
            order: None,
            nullable: false,
            enum_from: None,
            dual_list_sibling: None,
            dynamically_extendable_status_bar_elements: false,
        };
        // Overridden to `false` — distinct from both the default (true) and
        // inherit (null).
        let mut dialog = EntryDialogState::from_schema(
            "k".to_string(),
            &serde_json::json!({ "wrap": false }),
            &schema,
            "/test",
            false,
            false,
            &HashMap::new(),
        );

        // Field is index 1 (after read-only Key) and offers both buttons.
        assert_eq!(dialog.selected_item, 1);
        let buttons = dialog.field_action_buttons(1);
        assert_eq!(
            buttons.iter().map(|(a, _)| *a).collect::<Vec<_>>(),
            vec![FieldAction::Reset, FieldAction::Inherit]
        );
        assert_eq!(dialog.field_button_focus, None);

        // Forward: control → Reset → Inherit → footer.
        dialog.focus_next();
        assert_eq!(dialog.field_button_focus, Some(0)); // Reset
        dialog.focus_next();
        assert_eq!(dialog.field_button_focus, Some(1)); // Inherit
        dialog.focus_next();
        assert!(dialog.focus_on_buttons);

        // Backward: footer → Inherit → Reset → control.
        dialog.focus_prev();
        assert!(!dialog.focus_on_buttons);
        assert_eq!(dialog.field_button_focus, Some(1)); // Inherit
        dialog.focus_prev();
        assert_eq!(dialog.field_button_focus, Some(0)); // Reset
        dialog.focus_prev();
        assert_eq!(dialog.field_button_focus, None); // back on the control
        dialog.focus_prev();
        assert!(dialog.focus_on_buttons); // before first field → footer
    }

    /// A JSON/object field (like a language `formatter`) is not a "simple"
    /// control, but its per-field action buttons must still be reachable by Tab
    /// — that's the only keyboard path now that Ctrl+R is gone.
    #[test]
    fn focus_reaches_action_buttons_on_json_field() {
        let schema = SettingSchema {
            path: "/test".to_string(),
            name: "Test".to_string(),
            description: None,
            setting_type: SettingType::Object {
                properties: vec![SettingSchema {
                    path: "/formatter".to_string(),
                    name: "Formatter".to_string(),
                    description: None,
                    // Object => rendered as a JSON control.
                    setting_type: SettingType::Object { properties: vec![] },
                    default: Some(serde_json::json!({ "command": "clang-format" })),
                    read_only: false,
                    section: None,
                    order: None,
                    nullable: true,
                    enum_from: None,
                    dual_list_sibling: None,
                    dynamically_extendable_status_bar_elements: false,
                }],
            },
            default: None,
            read_only: false,
            section: None,
            order: None,
            nullable: false,
            enum_from: None,
            dual_list_sibling: None,
            dynamically_extendable_status_bar_elements: false,
        };
        // Overridden to a different command, so it differs from the default.
        let mut dialog = EntryDialogState::from_schema(
            "c".to_string(),
            &serde_json::json!({ "formatter": { "command": "my-fmt" } }),
            &schema,
            "/languages",
            false,
            false,
            &HashMap::new(),
        );

        // The JSON field offers buttons (at least [Reset]); Tab steps onto them.
        assert_eq!(dialog.selected_item, 1);
        assert!(
            !dialog.field_action_buttons(1).is_empty(),
            "overridden JSON field should offer action buttons"
        );
        assert_eq!(dialog.field_button_focus, None);
        dialog.focus_next();
        assert_eq!(
            dialog.field_button_focus,
            Some(0),
            "Tab should land on the JSON field's first action button"
        );
    }

    #[test]
    fn entry_path_joins_map_path_and_entry_key() {
        let schema = create_test_schema();

        // Existing entry: full path is map_path + "/" + entry_key
        let existing = EntryDialogState::from_schema(
            "rust".to_string(),
            &serde_json::json!({}),
            &schema,
            "/lsp",
            false,
            false,
            &HashMap::new(),
        );
        assert_eq!(existing.entry_path(), "/lsp/rust");

        // New entry with no key typed yet falls back to the parent map path.
        // Nested dialogs keyed off this are outside the scope of this test.
        let new_entry = EntryDialogState::from_schema(
            String::new(),
            &serde_json::json!({}),
            &schema,
            "/lsp",
            true,
            false,
            &HashMap::new(),
        );
        assert_eq!(new_entry.entry_path(), "/lsp");
    }

    #[test]
    fn entry_path_tracks_live_key_edits_for_new_entries() {
        let schema = create_test_schema();
        let mut dialog = EntryDialogState::from_schema(
            String::new(),
            &serde_json::json!({}),
            &schema,
            "/universal_lsp",
            true,
            false,
            &HashMap::new(),
        );

        // User types a key into the editable key field.
        for item in dialog.items.iter_mut() {
            if item.path == "__key__" {
                if let SettingControl::Text { value, .. } = &mut item.control {
                    *value = "myserver".to_string();
                }
            }
        }

        assert_eq!(dialog.entry_path(), "/universal_lsp/myserver");
    }

    #[test]
    fn button_count_differs_for_new_vs_existing() {
        let schema = create_test_schema();

        let new_dialog = EntryDialogState::from_schema(
            "test".to_string(),
            &serde_json::json!({}),
            &schema,
            "/test",
            true,
            false,
            &HashMap::new(),
        );
        assert_eq!(new_dialog.button_count(), 2); // Save, Cancel

        let existing_dialog = EntryDialogState::from_schema(
            "test".to_string(),
            &serde_json::json!({}),
            &schema,
            "/test",
            false,
            false, // allow delete
            &HashMap::new(),
        );
        assert_eq!(existing_dialog.button_count(), 3); // Save, Delete, Cancel

        // no_delete hides the Delete button even for existing entries
        let no_delete_dialog = EntryDialogState::from_schema(
            "test".to_string(),
            &serde_json::json!({}),
            &schema,
            "/test",
            false,
            true, // no delete (auto-managed entries like plugins)
            &HashMap::new(),
        );
        assert_eq!(no_delete_dialog.button_count(), 2); // Save, Cancel (no Delete)
    }

    // ---- Esc-reverts / Enter-commits field editing --------------------------

    /// Build a single-property schema whose one field has the given type, so a
    /// dialog can be driven straight to that control.
    fn prop_schema(path: &str, name: &str, ty: SettingType) -> SettingSchema {
        SettingSchema {
            path: "/test".to_string(),
            name: "Test".to_string(),
            description: None,
            setting_type: SettingType::Object {
                properties: vec![SettingSchema {
                    path: path.to_string(),
                    name: name.to_string(),
                    description: None,
                    setting_type: ty,
                    default: None,
                    read_only: false,
                    section: None,
                    order: None,
                    nullable: false,
                    enum_from: None,
                    dual_list_sibling: None,
                    dynamically_extendable_status_bar_elements: false,
                }],
            },
            default: None,
            read_only: false,
            section: None,
            order: None,
            nullable: false,
            enum_from: None,
            dual_list_sibling: None,
            dynamically_extendable_status_bar_elements: false,
        }
    }

    /// Focus the field at `path` (leaving button focus), returning its index.
    fn select_field(dialog: &mut EntryDialogState, path: &str) -> usize {
        let idx = dialog
            .items
            .iter()
            .position(|it| it.path == path)
            .expect("field should exist");
        dialog.selected_item = idx;
        dialog.focus_on_buttons = false;
        idx
    }

    /// Esc reverts a Text field to its original string; the edit is discarded.
    #[test]
    fn esc_reverts_text_field() {
        let schema = prop_schema("/grammar", "Grammar", SettingType::String);
        let mut dialog = EntryDialogState::from_schema(
            "k".to_string(),
            &serde_json::json!({ "grammar": "typescript" }),
            &schema,
            "/test",
            false,
            false,
            &HashMap::new(),
        );
        let idx = select_field(&mut dialog, "/grammar");

        dialog.start_editing();
        dialog.type_into_control("X");
        dialog.type_into_control("Y");
        assert_eq!(
            control_to_value(&dialog.items[idx].control),
            serde_json::json!("typescriptXY"),
            "sanity: the edit is applied live before reverting"
        );

        dialog.revert_editing();
        assert!(!dialog.is_editing(), "revert exits edit mode");
        assert_eq!(
            control_to_value(&dialog.items[idx].control),
            serde_json::json!("typescript"),
            "Esc must restore the original text"
        );
    }

    /// Esc reverts a Number field to its original value (typed digits dropped).
    #[test]
    fn esc_reverts_number_field() {
        let schema = prop_schema(
            "/tab_size",
            "Tab Size",
            SettingType::Integer {
                minimum: None,
                maximum: None,
            },
        );
        let mut dialog = EntryDialogState::from_schema(
            "k".to_string(),
            &serde_json::json!({ "tab_size": 4 }),
            &schema,
            "/test",
            false,
            false,
            &HashMap::new(),
        );
        let idx = select_field(&mut dialog, "/tab_size");

        dialog.start_editing();
        dialog.type_into_control("9"); // replaces the selected "4" in the draft
        dialog.revert_editing();

        assert_eq!(
            control_to_value(&dialog.items[idx].control),
            serde_json::json!(4),
            "Esc must restore the original number"
        );
    }

    /// Esc reverts an in-place edit of an existing TextList item — the case the
    /// old accept-on-Esc path silently kept.
    #[test]
    fn esc_reverts_textlist_item_edit() {
        let schema = prop_schema("/extensions", "Extensions", SettingType::StringArray);
        let mut dialog = EntryDialogState::from_schema(
            "k".to_string(),
            &serde_json::json!({ "extensions": ["ts", "tsx"] }),
            &schema,
            "/test",
            false,
            false,
            &HashMap::new(),
        );
        let idx = select_field(&mut dialog, "/extensions");

        // Open the first item's field and type into it.
        dialog.edit_list_row(Some(0));
        dialog.type_into_control("Z"); // "ts" -> "tsZ"
        assert_eq!(
            control_to_value(&dialog.items[idx].control),
            serde_json::json!(["tsZ", "tsx"]),
            "sanity: the item is mutated live before reverting"
        );

        dialog.revert_editing();
        assert_eq!(
            control_to_value(&dialog.items[idx].control),
            serde_json::json!(["ts", "tsx"]),
            "Esc must restore the original list items"
        );
    }

    /// Esc reverts a JSON/object field (a language `formatter`) to its original.
    #[test]
    fn esc_reverts_json_field() {
        let schema = prop_schema("/formatter", "Formatter", SettingType::Complex);
        let mut dialog = EntryDialogState::from_schema(
            "k".to_string(),
            &serde_json::json!({ "formatter": { "command": "prettier" } }),
            &schema,
            "/test",
            false,
            false,
            &HashMap::new(),
        );
        let idx = select_field(&mut dialog, "/formatter");

        dialog.start_editing();
        dialog.type_into_control("X"); // corrupt the JSON text in the editor
        assert!(dialog.is_editing_json());
        dialog.revert_editing();

        assert_eq!(
            control_to_value(&dialog.items[idx].control),
            serde_json::json!({ "command": "prettier" }),
            "Esc must restore the original JSON value"
        );
    }

    /// The commit path is unchanged: Enter/Tab (via `stop_editing`) keep the
    /// typed value, so the two keys are genuinely distinct.
    #[test]
    fn stop_editing_commits_text_field() {
        let schema = prop_schema("/grammar", "Grammar", SettingType::String);
        let mut dialog = EntryDialogState::from_schema(
            "k".to_string(),
            &serde_json::json!({ "grammar": "typescript" }),
            &schema,
            "/test",
            false,
            false,
            &HashMap::new(),
        );
        let idx = select_field(&mut dialog, "/grammar");

        dialog.start_editing();
        dialog.type_into_control("X");
        dialog.stop_editing();

        assert_eq!(
            control_to_value(&dialog.items[idx].control),
            serde_json::json!("typescriptX"),
            "Enter/Tab must keep the typed value"
        );
        assert!(!dialog.is_editing());
    }
}
