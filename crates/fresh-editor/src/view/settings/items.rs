//! Setting items for the UI
//!
//! Converts schema information into renderable setting items.

use super::schema::{SettingCategory, SettingSchema, SettingType};
use crate::config_io::ConfigLayer;
use std::collections::{HashMap, HashSet};

/// The text a JSON control shows for a value: pretty-printed, or the
/// `null` literal when there is none.
pub fn json_text(value: Option<&serde_json::Value>) -> String {
    value
        .map(|v| serde_json::to_string_pretty(v).unwrap_or_else(|_| "null".to_string()))
        .unwrap_or_else(|| "null".to_string())
}

/// Whether a JSON control's text means "not set": the `null` literal, or
/// nothing at all — an edit that was opened and left empty.
pub fn json_is_unset(text: &str) -> bool {
    let t = text.trim();
    t.is_empty() || t == "null"
}

/// Whether a JSON control's text will save: unset counts as valid — it
/// round-trips as `null` — and anything else has to parse.
pub fn json_is_valid(text: &str) -> bool {
    json_is_unset(text) || serde_json::from_str::<serde_json::Value>(text).is_ok()
}

/// Create a JSON control for editing arbitrary JSON values (multiline)
fn json_control(
    name: &str,
    current_value: Option<&serde_json::Value>,
    default: Option<&serde_json::Value>,
) -> SettingControl {
    SettingControl::Json {
        label: name.to_string(),
        text: json_text(current_value.or(default)),
    }
}

/// Extract a JSON array of strings from a value (or fall back to a default).
fn value_as_string_array(
    current: Option<&serde_json::Value>,
    default: Option<&serde_json::Value>,
) -> Vec<String> {
    let from = |v: &serde_json::Value| -> Option<Vec<String>> {
        v.as_array().map(|arr| {
            arr.iter()
                .filter_map(|v| v.as_str().map(String::from))
                .collect()
        })
    };
    current
        .and_then(from)
        .or_else(|| default.and_then(from))
        .unwrap_or_default()
}

/// Build a dual list from schema options, current value, and optional sibling excluded set.
fn build_dual_list(
    schema: &SettingSchema,
    options: &[crate::view::settings::schema::EnumOption],
    current_value: Option<&serde_json::Value>,
    excluded: Vec<String>,
    available_status_bar_tokens: &HashMap<String, String>,
) -> SettingControl {
    // Start with static schema options (built-in tokens)
    let mut all_options: Vec<(String, String)> = options
        .iter()
        .map(|o| (o.value.clone(), o.name.clone()))
        .collect();

    // Append plugin-registered tokens when this control opts in.
    if schema.dynamically_extendable_status_bar_elements {
        for (key, display) in available_status_bar_tokens {
            let token = format!("{{{}}}", key);
            if !all_options.iter().any(|(v, _)| v == &token) {
                all_options.push((token, display.clone()));
            }
        }
    }

    let included = value_as_string_array(current_value, schema.default.as_ref());
    SettingControl::DualList {
        label: schema.name.clone(),
        options: all_options,
        included,
        excluded,
    }
}

/// A renderable setting item
#[derive(Debug, Clone)]
pub struct SettingItem {
    /// JSON pointer path
    pub path: String,
    /// Display name
    pub name: String,
    /// Description
    pub description: Option<String>,
    /// The control for this setting
    pub control: SettingControl,
    /// Default value (for reset)
    pub default: Option<serde_json::Value>,
    /// Whether this setting is defined in the current target layer.
    /// This is the new semantic: modified means "explicitly set in target layer",
    /// not "differs from schema default".
    pub modified: bool,
    /// Which layer this setting's current value comes from.
    /// System means it's using the schema default.
    pub layer_source: ConfigLayer,
    /// Whether this field is read-only (cannot be edited)
    pub read_only: bool,
    /// Whether this is an auto-managed map (no_add) that should never show as modified
    pub is_auto_managed: bool,
    /// Whether this setting accepts null (can be "unset" to inherit)
    pub nullable: bool,
    /// Whether this setting's current value is null (inherited/unset)
    pub is_null: bool,
    /// Section/group within the category (from x-section)
    pub section: Option<String>,
    /// Whether this item is the first in its section (for rendering section headers)
    pub is_section_start: bool,
    /// Visual style (card border thickness, padding, etc.) for this item.
    /// Cached on the item so the `ScrollItem::height(width)` trait impl can
    /// compute the correct height without taking a style parameter; flipped
    /// in bulk by `SettingsState::set_item_style` when the user toggles UI mode.
    pub style: ItemBoxStyle,
    /// Path to sibling dual-list setting (for cross-exclusion refresh)
    pub dual_list_sibling: Option<String>,
}

/// The type of control to render for a setting
#[derive(Debug, Clone)]
pub enum SettingControl {
    /// A boolean. `inherited` says the setting is unset and shows what it
    /// falls back to, as a neutral `[-]` chip (#2345); an explicit toggle
    /// clears it.
    Toggle {
        label: String,
        checked: bool,
        inherited: bool,
    },
    /// A number, as the JSON carries it: an `integer` is whole, a `percent`
    /// is the fraction its cell displays ×100.
    Number {
        label: String,
        value: f64,
        min: Option<f64>,
        max: Option<f64>,
        integer: bool,
        percent: bool,
    },
    /// One of a set. `options` are shown; `values` are stored, and when
    /// empty the options are the values.
    Dropdown {
        label: String,
        options: Vec<String>,
        values: Vec<String>,
        selected: usize,
    },
    /// A string.
    Text {
        label: String,
        value: String,
        placeholder: String,
    },
    /// A list of strings (or, when `integer`, of whole numbers kept as
    /// their text). Its rows are text fields keyed `{path}::row::{i}` and
    /// its add row a field keyed `{path}::add`, edited by the `Text` kind:
    /// a row's value is the model's as it is typed; the add row's draft is
    /// the field's until Enter makes it a row.
    TextList {
        label: String,
        items: Vec<String>,
        integer: bool,
    },
    /// An ordered subset of a fixed set — the status bar's elements. The
    /// `options` are `(value, display name)`; `included` is the subset in
    /// its order; `excluded` are the options a sibling list has claimed,
    /// which this one's Available column must not offer. Which column the
    /// keyboard drives and where its cursors sit are the kind's instance
    /// state while the control is live.
    DualList {
        label: String,
        options: Vec<(String, String)>,
        included: Vec<String>,
        excluded: Vec<String>,
    },
    /// A key → value map, its entries in key order. The rows are a `List`
    /// keyed by the path with the add row last — unless `no_add`, an
    /// auto-managed map that takes no entries of the user's own. An entry
    /// is edited through the entry dialog `value_schema` describes;
    /// `display_field` names the value's preview column.
    Map {
        label: String,
        entries: Vec<(String, serde_json::Value)>,
        value_schema: Option<Box<SettingSchema>>,
        display_field: Option<String>,
        no_add: bool,
    },
    /// An array of objects — keybindings, a language's servers. The rows
    /// are a `List` keyed by the path with the add row last; an item is
    /// edited through the entry dialog `item_schema` describes, and
    /// `display_field` names the field a row shows.
    ObjectArray {
        label: String,
        items: Vec<serde_json::Value>,
        item_schema: Option<Box<SettingSchema>>,
        display_field: Option<String>,
    },
    /// A JSON value edited as text, in a multi-line field. `text` is the
    /// value as it reads — pretty-printed, `null` or empty when unset
    /// ([`json_is_unset`]) — and it is applied as it is typed; the surface
    /// records it when the edit ends and it parses ([`json_is_valid`]).
    Json { label: String, text: String },
    /// Complex settings that can't be edited inline
    Complex { type_name: String },
}

/// The label column a page aligns its scalar controls' value cells against:
/// the widest label among them, and `None` when the page has none.
///
/// Only single-row controls take part — a multi-row control puts its label on
/// a line of its own, so padding it would move nothing.
pub fn page_label_width(items: &[SettingItem]) -> Option<u16> {
    use crate::primitives::display_width::str_width;
    items
        .iter()
        .filter_map(|item| match &item.control {
            SettingControl::Toggle { label, .. }
            | SettingControl::Number { label, .. }
            | SettingControl::Dropdown { label, .. }
            | SettingControl::Text { label, .. } => Some(str_width(label) as u16),
            _ => None,
        })
        .max()
}

impl SettingControl {
    /// A dropdown, with its selection clamped into its options.
    pub fn dropdown(
        label: impl Into<String>,
        options: Vec<String>,
        values: Vec<String>,
        selected: usize,
    ) -> Self {
        debug_assert!(values.is_empty() || values.len() == options.len());
        let selected = match options.is_empty() {
            true => 0,
            false => selected.min(options.len() - 1),
        };
        Self::Dropdown {
            label: label.into(),
            options,
            values,
            selected,
        }
    }

    /// The label a scalar control carries; `None` for a composite, which
    /// puts its label on a row of its own.
    pub fn label(&self) -> Option<&str> {
        match self {
            Self::Toggle { label, .. }
            | Self::Number { label, .. }
            | Self::Dropdown { label, .. }
            | Self::Text { label, .. } => Some(label),
            _ => None,
        }
    }

    /// The value a dropdown stores: the selected entry of `values`, or of
    /// `options` when there are no separate values.
    pub fn dropdown_selected_value(&self) -> Option<&str> {
        let Self::Dropdown {
            options,
            values,
            selected,
            ..
        } = self
        else {
            return None;
        };
        match values.is_empty() {
            true => options.get(*selected).map(String::as_str),
            false => values.get(*selected).map(String::as_str),
        }
    }

    /// Calculate the height needed for this control (in lines)
    pub fn control_height(&self) -> u16 {
        match self {
            // TextList: 1 label line + items + 1 add row
            Self::TextList { items, .. } => (items.len() + 2) as u16,
            // DualList: 1 label + 1 header + one body row per option it can
            // show, plus the key-hint row it grows once it is reachable.
            Self::DualList {
                options, excluded, ..
            } => {
                3 + options
                    .iter()
                    .filter(|(v, _)| !excluded.contains(v))
                    .count() as u16
            }
            // Map: 1 label + 1 header (if display_field) + entries + 1 add row (if allowed)
            Self::Map {
                entries,
                display_field,
                no_add,
                ..
            } => {
                (1 + usize::from(display_field.is_some()) + entries.len() + usize::from(!no_add))
                    as u16
            }
            // ObjectArray: 1 label + items + 1 add row
            Self::ObjectArray { items, .. } => (items.len() + 2) as u16,
            // Json: 1 label + its lines
            Self::Json { text, .. } => 1 + text.lines().count().max(1) as u16,
            // All other controls fit in 1 line
            _ => 1,
        }
    }

    /// Whether the control's rows are a `List` the surface's cursor walks —
    /// a map or an object array. (A text list's rows are fields.)
    pub fn has_list_rows(&self) -> bool {
        matches!(self, Self::Map { .. } | Self::ObjectArray { .. })
    }

    /// How many rows the control's `List` has, its add row included; the
    /// add row's index is one past the last entry.
    pub fn list_row_count(&self) -> usize {
        match self {
            Self::Map {
                entries, no_add, ..
            } => entries.len() + usize::from(!no_add),
            Self::ObjectArray { items, .. } => items.len() + 1,
            _ => 0,
        }
    }

    /// The row index of the control's add row, when it has one.
    pub fn add_row(&self) -> Option<usize> {
        match self {
            Self::Map {
                entries, no_add, ..
            } => (!no_add).then_some(entries.len()),
            Self::ObjectArray { items, .. } => Some(items.len()),
            Self::TextList { items, .. } => Some(items.len()),
            _ => None,
        }
    }

    /// The widget key of a text list's row: an item's field, or the add
    /// row's when `row` is `None`.
    pub fn text_list_row_key(path: &str, row: Option<usize>) -> String {
        match row {
            Some(i) => format!("{path}::row::{i}"),
            None => format!("{path}::add"),
        }
    }

    /// The key of a composite's row in the tree — the row of its `List`, or
    /// a text list's field — what the surface asks the window to reveal.
    /// `row` counts the add row after the items (`add_row`).
    pub fn row_tree_key(&self, path: &str, row: usize) -> fresh_ui::Key {
        let row = (Some(row) != self.add_row()).then_some(row);
        match self {
            Self::TextList { .. } => {
                crate::view::shell::widgets::widget_node_key(&Self::text_list_row_key(path, row))
            }
            _ => fresh_ui::Key::Str(Self::text_list_row_key(path, row).into()),
        }
    }
}

/// A map's entries from its JSON object, in key order.
pub fn map_entries(value: &serde_json::Value) -> Vec<(String, serde_json::Value)> {
    let mut entries: Vec<(String, serde_json::Value)> = value
        .as_object()
        .map(|obj| obj.iter().map(|(k, v)| (k.clone(), v.clone())).collect())
        .unwrap_or_default();
    entries.sort_by(|a, b| a.0.cmp(&b.0));
    entries
}

/// Set `key` to `value` in a map's entries: the entry it has, or a new one
/// in key order.
pub fn map_set(
    entries: &mut Vec<(String, serde_json::Value)>,
    key: String,
    value: serde_json::Value,
) {
    if let Some(entry) = entries.iter_mut().find(|(k, _)| *k == key) {
        entry.1 = value;
    } else {
        entries.push((key, value));
        entries.sort_by(|a, b| a.0.cmp(&b.0));
    }
}

/// The preview a map row shows for its value: the `display_field` of the
/// value (each element's, for an array value), or a count of what it holds.
pub fn map_display_value(display_field: Option<&str>, value: &serde_json::Value) -> String {
    if let Some(field) = display_field {
        // For array values (e.g. multi-server LSP entries), show each
        // element's `display_field` so the collapsed row reflects the
        // full set. Otherwise a row that maps `python` to two servers
        // still rendered as just `pylsp`, making the user think the
        // second server hadn't saved.
        if let serde_json::Value::Array(arr) = value {
            let parts: Vec<String> = arr
                .iter()
                .filter_map(|el| el.pointer(field))
                .filter_map(|v| match v {
                    serde_json::Value::String(s) => Some(s.clone()),
                    serde_json::Value::Bool(b) => Some(b.to_string()),
                    serde_json::Value::Number(n) => Some(n.to_string()),
                    _ => None,
                })
                .collect();
            if !parts.is_empty() {
                // The map row's value column truncates at ~20 chars. When
                // the joined list would overflow, fall back to "first +N
                // more" so the user can still tell the entry has more than
                // one item.
                let joined = parts.join(", ");
                if joined.chars().count() <= 20 || parts.len() == 1 {
                    return joined;
                }
                return format!("{}, +{} more", parts[0], parts.len() - 1);
            }
        } else if let Some(v) = value.pointer(field) {
            return match v {
                serde_json::Value::String(s) => s.clone(),
                serde_json::Value::Bool(b) => b.to_string(),
                serde_json::Value::Number(n) => n.to_string(),
                serde_json::Value::Null => "null".to_string(),
                serde_json::Value::Array(arr) => format!("[{} items]", arr.len()),
                serde_json::Value::Object(obj) => format!("{{{} fields}}", obj.len()),
            };
        }
    }
    // Fallback to showing field count with correct pluralization
    match value {
        serde_json::Value::Object(obj) => match obj.len() {
            1 => "1 field".to_string(),
            n => format!("{n} fields"),
        },
        serde_json::Value::Array(arr) => match arr.len() {
            1 => "1 item".to_string(),
            n => format!("{n} items"),
        },
        other => other.to_string(),
    }
}

/// What an object array's row says for an item: the key combination and
/// the action, for a keybinding-shaped item, or just the display field's
/// value — a server's command — when there is no key combination.
pub fn object_array_row(display_field: Option<&str>, item: &serde_json::Value) -> (String, String) {
    // `display_field` is a JSON pointer (`/command`); the lookup key is the
    // bare field name.
    let field = display_field
        .and_then(|p| p.strip_prefix('/'))
        .or(display_field)
        .unwrap_or("action");
    let combo = format_key_combo(item);
    let action = item
        .get(field)
        .and_then(|v| v.as_str())
        .unwrap_or("(no action)")
        .to_string();
    (combo, action)
}

/// Format a keybinding's key combination for display
pub fn format_key_combo(binding: &serde_json::Value) -> String {
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

// === Layout primitives ===
//
// Every magic number that used to be sprinkled through the render path lives
// inside `ItemBoxStyle`. The struct is `Copy`, has a `Default` impl, and is
// stored on each `SettingItem` — so toggling cards on/off, removing the
// indicator gutter, or tightening the padding is a single state mutation
// rather than a code change.

/// Visual style for a setting item: tunes every dimension of the layout so
/// chrome (card border, padding, section header, indicator gutter) can be
/// toggled or tweaked from one place.
///
/// All values are in terminal cells (rows or columns). Setting a row/col count
/// to `0` disables that piece of chrome; the rest of the layout still works.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ItemBoxStyle {
    /// Rows occupied by a section header: title row + blank gap below it.
    /// Set to `0` to suppress section headings entirely.
    pub section_header_rows: u16,
    /// Top/bottom border thickness of the per-item card (rows).
    pub card_border_rows: u16,
    /// Left/right border thickness of the per-item card (columns).
    pub card_border_cols: u16,
    /// Columns reserved on the left of the card's interior for the focus
    /// indicator (`>`), the modified marker (`●`), and a single-space gutter.
    pub focus_indicator_cols: u16,
    /// Right-side padding inside the card so wrapped description text doesn't
    /// butt up against the right border.
    pub description_right_padding_cols: u16,
}

impl ItemBoxStyle {
    /// The default look used by the settings panel: 1-row top/bottom card
    /// borders, 1-col side borders, 2-row section headers.
    pub const fn cards() -> Self {
        Self {
            section_header_rows: 2,
            card_border_rows: 1,
            card_border_cols: 1,
            focus_indicator_cols: 3,
            description_right_padding_cols: 2,
        }
    }

    /// A flat look with no card border. Items still get 1-row gap chrome
    /// (carried by the section header) and the indicator gutter.
    pub const fn flat() -> Self {
        Self {
            section_header_rows: 2,
            card_border_rows: 0,
            card_border_cols: 0,
            focus_indicator_cols: 3,
            description_right_padding_cols: 2,
        }
    }

    /// Width available for wrapped description text inside a card of the
    /// given outer width (subtracting both borders, the focus gutter, and
    /// the right padding).
    pub fn inner_text_width(&self, card_outer_width: u16) -> u16 {
        card_outer_width
            .saturating_sub(2 * self.card_border_cols)
            .saturating_sub(self.focus_indicator_cols)
            .saturating_sub(self.description_right_padding_cols)
    }
}

impl Default for ItemBoxStyle {
    fn default() -> Self {
        Self::cards()
    }
}

/// Vertical layout descriptor for a single setting item.
///
/// Fields are named bands of rows; together they describe both the total
/// height of the item and where each band lives along the y-axis. The render
/// path uses these offsets directly instead of recomputing them inline.
///
/// All offsets are relative to the top of the area allocated to the item.
#[derive(Debug, Clone, Copy, Default)]
pub struct ItemBox {
    /// Section header band above the card (0 if not a section start).
    pub section_header_rows: u16,
    /// Top edge of the card.
    pub top_border_rows: u16,
    /// The control widget (toggle, dropdown, multi-row list, …).
    pub control_rows: u16,
    /// The wrapped description text below the control.
    pub description_rows: u16,
    /// Bottom edge of the card.
    pub bottom_border_rows: u16,
}

impl ItemBox {
    pub fn total_rows(&self) -> u16 {
        self.section_header_rows
            + self.top_border_rows
            + self.control_rows
            + self.description_rows
            + self.bottom_border_rows
    }

    /// Y of the card's top border.
    pub fn card_top_y(&self) -> u16 {
        self.section_header_rows
    }

    /// Y of the first content row (the control).
    pub fn control_y(&self) -> u16 {
        self.card_top_y() + self.top_border_rows
    }

    /// Y of the first description row.
    pub fn description_y(&self) -> u16 {
        self.control_y() + self.control_rows
    }

    /// Y of the bottom border.
    pub fn bottom_border_y(&self) -> u16 {
        self.description_y() + self.description_rows
    }

    /// Total card height (top border + content + bottom border).
    pub fn card_height(&self) -> u16 {
        self.top_border_rows + self.control_rows + self.description_rows + self.bottom_border_rows
    }

    /// Card content rows (control + description, no borders).
    pub fn content_rows(&self) -> u16 {
        self.control_rows + self.description_rows
    }
}

impl SettingItem {
    /// Compute the visual layout of this item for a given outer width and
    /// style. `width` is the full width allocated to the item (including the
    /// card borders and the focus-indicator columns).
    pub fn layout_box(&self, width: u16, style: &ItemBoxStyle) -> ItemBox {
        ItemBox {
            section_header_rows: if self.is_section_start {
                style.section_header_rows
            } else {
                0
            },
            top_border_rows: style.card_border_rows,
            control_rows: self.control.control_height(),
            description_rows: self.description_rows_for(style.inner_text_width(width)),
            bottom_border_rows: style.card_border_rows,
        }
    }

    /// Rows needed for the description when wrapped to `inner_width` columns.
    ///
    /// The wrapping here is a byte-based approximation that overestimates
    /// slightly compared to the word-wrap used at render time; that's fine —
    /// the renderer clips to the available rows, never to fewer than the
    /// number of wrapped lines it produces.
    pub fn description_rows_for(&self, inner_width: u16) -> u16 {
        let Some(desc) = self.description.as_deref() else {
            return 0;
        };
        if desc.is_empty() {
            return 0;
        }
        if inner_width == 0 {
            return 1;
        }
        desc.len().div_ceil(inner_width as usize) as u16
    }
}

/// Clean a description to remove redundancy with the name.
/// Returns None if the description is empty or essentially just repeats the name.
pub fn clean_description(name: &str, description: Option<&str>) -> Option<String> {
    let desc = description?;
    if desc.is_empty() {
        return None;
    }

    // Build a set of significant words from the name (lowercase for comparison)
    let name_words: HashSet<String> = name
        .to_lowercase()
        .split(|c: char| !c.is_alphanumeric())
        .filter(|w| !w.is_empty() && w.len() > 2)
        .map(String::from)
        .collect();

    // Common filler words to ignore when checking for new info
    let filler_words: HashSet<&str> = [
        "the", "a", "an", "to", "for", "of", "in", "on", "is", "are", "be", "and", "or", "when",
        "whether", "if", "this", "that", "with", "from", "by", "as", "at", "show", "enable",
        "disable", "set", "use", "allow", "default", "true", "false",
    ]
    .into_iter()
    .collect();

    // Split description into words
    let desc_words: Vec<&str> = desc
        .split(|c: char| !c.is_alphanumeric())
        .filter(|w| !w.is_empty())
        .collect();

    // Check if description has any meaningful new information
    let has_new_info = desc_words.iter().any(|word| {
        let lower = word.to_lowercase();
        lower.len() > 2 && !name_words.contains(&lower) && !filler_words.contains(lower.as_str())
    });

    if !has_new_info {
        return None;
    }

    Some(desc.to_string())
}

// **`ScrollItem for SettingItem` is gone, and `ItemBox` with it.** Its
// `height` re-derived what the painter drew each card with so
// `ScrollablePanel` could bound the scroll, and its `focus_regions` walked
// the same rows again so a sub-focus could be scrolled to. The cards are a
// `col` in a `viewport` now: the column measures them, the window is asked
// to hold a card by key (`Anchor::reveal_key`), and a sub-row names itself
// through `SettingControl::sub_row_key`.

/// A page of settings (corresponds to a category)
#[derive(Debug, Clone)]
pub struct SettingsPage {
    /// Page name
    pub name: String,
    /// JSON path prefix
    pub path: String,
    /// Description
    pub description: Option<String>,
    /// Whether this page represents a nullable category that can be cleared as a whole
    pub nullable: bool,
    /// Settings on this page
    pub items: Vec<SettingItem>,
    /// Subpages
    pub subpages: Vec<SettingsPage>,
    /// Cached section list for the tree view in the left panel.
    /// Computed once after sorting items in `build_page`.
    pub sections: Vec<SectionInfo>,
}

/// One section within a page — name plus the index of its first item, used by
/// the left-panel tree view to jump straight to that section when clicked.
#[derive(Debug, Clone)]
pub struct SectionInfo {
    pub name: String,
    pub first_item_index: usize,
}

/// One entry for the Settings theme dropdown: a human-readable display name
/// (the theme's name, as shown by "Select Theme") paired with the config value
/// that persists when it is selected (the theme's portable form). Sourced from
/// the live [`ThemeRegistry`](crate::view::theme::ThemeRegistry) so the
/// dropdown never drifts from Select Theme (#2738).
#[derive(Debug, Clone)]
pub struct ThemeOption {
    /// Display name shown in the dropdown (the theme name).
    pub display: String,
    /// Value stored in config when this option is chosen (portable form).
    pub value: String,
}

/// Context for building setting items with layer awareness
pub struct BuildContext<'a> {
    /// The merged config value (effective values)
    pub config_value: &'a serde_json::Value,
    /// Map of paths to their source layer
    pub layer_sources: &'a HashMap<String, ConfigLayer>,
    /// The layer currently being edited
    pub target_layer: ConfigLayer,
    /// Plugin-registered status-bar tokens (key → display title). Always
    /// present; pass an empty map in tests.
    pub available_status_bar_tokens: &'a HashMap<String, String>,
    /// Live theme options `(display, value)` for the theme dropdown, in
    /// Select-Theme order. Resolves the reserved `x-enum-from: "$themes"`
    /// source. Empty when the registry isn't available (e.g. in tests) — the
    /// control then simply renders no options.
    pub theme_options: &'a [ThemeOption],
}

/// Convert a category tree into pages with control states
pub fn build_pages(
    categories: &[SettingCategory],
    config_value: &serde_json::Value,
    layer_sources: &HashMap<String, ConfigLayer>,
    target_layer: ConfigLayer,
    available_status_bar_tokens: &HashMap<String, String>,
    theme_options: &[ThemeOption],
) -> Vec<SettingsPage> {
    let ctx = BuildContext {
        config_value,
        layer_sources,
        target_layer,
        available_status_bar_tokens,
        theme_options,
    };
    categories.iter().map(|cat| build_page(cat, &ctx)).collect()
}

/// Build a single page from a category
fn build_page(category: &SettingCategory, ctx: &BuildContext) -> SettingsPage {
    let mut items: Vec<SettingItem> = category
        .settings
        .iter()
        .flat_map(|s| expand_or_build(s, ctx))
        .collect();

    // Sort items: by section first (None comes last), then alphabetically by name
    items.sort_by(|a, b| match (&a.section, &b.section) {
        (Some(sec_a), Some(sec_b)) => sec_a.cmp(sec_b).then_with(|| a.name.cmp(&b.name)),
        (Some(_), None) => std::cmp::Ordering::Less,
        (None, Some(_)) => std::cmp::Ordering::Greater,
        (None, None) => a.name.cmp(&b.name),
    });

    // Mark items that start a new section, and capture the section list
    // for the left-panel tree view in one pass.
    let mut sections: Vec<SectionInfo> = Vec::new();
    let mut prev_section: Option<&String> = None;
    for (idx, item) in items.iter_mut().enumerate() {
        let is_new_section = match (&item.section, prev_section) {
            (Some(sec), Some(prev)) => sec != prev,
            (Some(_), None) => true,
            (None, Some(_)) => false, // Unsectioned items after sectioned ones don't start a section
            (None, None) => false,
        };
        item.is_section_start = is_new_section;
        if is_new_section {
            if let Some(name) = item.section.clone() {
                sections.push(SectionInfo {
                    name,
                    first_item_index: idx,
                });
            }
        }
        prev_section = item.section.as_ref();
    }

    let subpages = category
        .subcategories
        .iter()
        .map(|sub| build_page(sub, ctx))
        .collect();

    SettingsPage {
        name: category.name.clone(),
        path: category.path.clone(),
        description: category.description.clone(),
        nullable: category.nullable,
        items,
        subpages,
        sections,
    }
}

/// Expand an Object schema into its children when every child has a native
/// (non-JSON) control, otherwise build it as a single item. This lets compound
/// config structs like `StatusBarConfig` surface their children as individual
/// settings with proper DualList / toggle / etc. controls, while objects whose
/// children would all fall through to JSON editors stay collapsed.
fn expand_or_build(schema: &SettingSchema, ctx: &BuildContext) -> Vec<SettingItem> {
    if let SettingType::Object { properties } = &schema.setting_type {
        let all_native = !properties.is_empty()
            && properties.iter().all(|child| {
                !matches!(
                    child.setting_type,
                    SettingType::Object { .. } | SettingType::Complex
                )
            });
        if all_native {
            // Children parsed inside determine_type have paths relative to ""
            // (e.g. "/left"); prefix with the parent's path to get absolute
            // paths (e.g. "/editor/status_bar/left").
            return properties
                .iter()
                .map(|child| {
                    let mut child = child.clone();
                    if !child.path.starts_with(&schema.path) {
                        child.path = format!("{}{}", schema.path, child.path);
                    }
                    if let Some(ref mut sib) = child.dual_list_sibling {
                        if !sib.starts_with(&schema.path) {
                            *sib = format!("{}{}", schema.path, sib);
                        }
                    }
                    build_item(&child, ctx)
                })
                .collect();
        }
    }
    vec![build_item(schema, ctx)]
}

/// Build a setting item with its control state initialized from current config
pub fn build_item(schema: &SettingSchema, ctx: &BuildContext) -> SettingItem {
    // Get current value from config
    let current_value = ctx.config_value.pointer(&schema.path);

    // Detect if the current value is null (inherited/unset) for nullable fields
    let is_null = schema.nullable
        && current_value
            .map(|v| v.is_null())
            .unwrap_or(schema.default.as_ref().map(|d| d.is_null()).unwrap_or(true));

    // Check if this is an auto-managed map (no_add)
    let is_auto_managed = matches!(&schema.setting_type, SettingType::Map { no_add: true, .. });

    // Create control based on type
    let control = match &schema.setting_type {
        SettingType::Boolean => {
            let checked = current_value
                .and_then(|v| v.as_bool())
                .or_else(|| schema.default.as_ref().and_then(|d| d.as_bool()))
                .unwrap_or(false);
            SettingControl::Toggle {
                label: schema.name.clone(),
                checked,
                inherited: false,
            }
        }

        SettingType::Integer { minimum, maximum } => {
            let value = current_value
                .and_then(|v| v.as_i64())
                .or_else(|| schema.default.as_ref().and_then(|d| d.as_i64()))
                .unwrap_or(0);

            SettingControl::Number {
                label: schema.name.clone(),
                value: value as f64,
                min: minimum.map(|m| m as f64),
                max: maximum.map(|m| m as f64),
                integer: true,
                percent: false,
            }
        }

        SettingType::Number { minimum, maximum } => {
            // A float is a fraction its cell shows as a percentage.
            let value = current_value
                .and_then(|v| v.as_f64())
                .or_else(|| schema.default.as_ref().and_then(|d| d.as_f64()))
                .unwrap_or(0.0);
            SettingControl::Number {
                label: schema.name.clone(),
                value,
                min: *minimum,
                max: *maximum,
                integer: false,
                percent: true,
            }
        }

        SettingType::String => {
            let value = current_value
                .and_then(|v| v.as_str())
                .or_else(|| schema.default.as_ref().and_then(|d| d.as_str()))
                .unwrap_or("");

            // Check for dynamic enum: derive dropdown options from another config field's keys
            if let Some(ref source_path) = schema.enum_from {
                // Reserved `$themes` source: the theme dropdown can't use the
                // generic object-keys path, because a user theme's DISPLAY (its
                // name) differs from the stored config VALUE (its portable
                // form). Populate it from the live registry as (display, value)
                // pairs, in Select-Theme order (no re-sorting), so the two lists
                // are one source of truth (#2738).
                if source_path == "$themes" {
                    let display_names: Vec<String> = ctx
                        .theme_options
                        .iter()
                        .map(|o| o.display.clone())
                        .collect();
                    let values: Vec<String> =
                        ctx.theme_options.iter().map(|o| o.value.clone()).collect();

                    let current = if is_null { "" } else { value };
                    let selected = values
                        .iter()
                        .position(|v| v == current)
                        // Legacy configs may store a bare built-in name (e.g.
                        // `"dark"`) rather than the portable `builtin://dark`;
                        // still pre-select the matching option.
                        .or_else(|| {
                            values.iter().position(|v| {
                                v.strip_prefix("builtin://")
                                    .is_some_and(|name| name == current)
                            })
                        })
                        .unwrap_or(0);
                    SettingControl::dropdown(&schema.name, display_names, values, selected)
                } else {
                    let mut options: Vec<String> = ctx
                        .config_value
                        .pointer(source_path)
                        .and_then(|v| v.as_object())
                        .map(|obj| obj.keys().cloned().collect())
                        .unwrap_or_default();
                    options.sort();

                    // Add empty option for nullable fields (unset/inherit)
                    let mut display_names = Vec::new();
                    let mut values = Vec::new();
                    if schema.nullable {
                        display_names.push("(none)".to_string());
                        values.push(String::new());
                    }
                    for key in &options {
                        display_names.push(key.clone());
                        values.push(key.clone());
                    }

                    let current = if is_null { "" } else { value };
                    let selected = values.iter().position(|v| v == current).unwrap_or(0);
                    SettingControl::dropdown(&schema.name, display_names, values, selected)
                }
            } else {
                SettingControl::Text {
                    label: schema.name.clone(),
                    value: value.to_string(),
                    placeholder: String::new(),
                }
            }
        }

        SettingType::Enum { options } => {
            // Handle null values in enums (represented as empty string in dropdown values)
            let current = if current_value.map(|v| v.is_null()).unwrap_or(false) {
                "" // null maps to empty string (Auto-detect option)
            } else {
                current_value
                    .and_then(|v| v.as_str())
                    .or_else(|| {
                        let default = schema.default.as_ref()?;
                        if default.is_null() {
                            Some("")
                        } else {
                            default.as_str()
                        }
                    })
                    .unwrap_or("")
            };

            let display_names: Vec<String> = options.iter().map(|o| o.name.clone()).collect();
            let values: Vec<String> = options.iter().map(|o| o.value.clone()).collect();
            let selected = values.iter().position(|v| v == current).unwrap_or(0);
            SettingControl::dropdown(&schema.name, display_names, values, selected)
        }

        SettingType::DualList {
            options,
            sibling_path,
        } => {
            let excluded = sibling_path
                .as_ref()
                .and_then(|path| ctx.config_value.pointer(path))
                .map(|v| value_as_string_array(Some(v), None))
                .unwrap_or_default();
            build_dual_list(
                schema,
                options,
                current_value,
                excluded,
                ctx.available_status_bar_tokens,
            )
        }

        SettingType::StringArray => {
            let items = value_as_string_array(current_value, schema.default.as_ref());
            SettingControl::TextList {
                label: schema.name.clone(),
                items,
                integer: false,
            }
        }

        SettingType::IntegerArray => {
            let items: Vec<String> = current_value
                .and_then(|v| v.as_array())
                .map(|arr| {
                    arr.iter()
                        .filter_map(|v| {
                            v.as_i64()
                                .map(|n| n.to_string())
                                .or_else(|| v.as_u64().map(|n| n.to_string()))
                                .or_else(|| v.as_f64().map(|n| n.to_string()))
                        })
                        .collect()
                })
                .or_else(|| {
                    schema.default.as_ref().and_then(|d| {
                        d.as_array().map(|arr| {
                            arr.iter()
                                .filter_map(|v| {
                                    v.as_i64()
                                        .map(|n| n.to_string())
                                        .or_else(|| v.as_u64().map(|n| n.to_string()))
                                        .or_else(|| v.as_f64().map(|n| n.to_string()))
                                })
                                .collect()
                        })
                    })
                })
                .unwrap_or_default();

            SettingControl::TextList {
                label: schema.name.clone(),
                items,
                integer: true,
            }
        }

        SettingType::Object { .. } => {
            json_control(&schema.name, current_value, schema.default.as_ref())
        }

        SettingType::Map {
            value_schema,
            display_field,
            no_add,
        } => {
            // Get current map value or default
            let map_value = current_value
                .cloned()
                .or_else(|| schema.default.clone())
                .unwrap_or_else(|| serde_json::json!({}));

            SettingControl::Map {
                label: schema.name.clone(),
                entries: map_entries(&map_value),
                value_schema: Some(Box::new((**value_schema).clone())),
                display_field: display_field.clone(),
                no_add: *no_add,
            }
        }

        SettingType::ObjectArray {
            item_schema,
            display_field,
        } => {
            // Get current array or default
            let array_value = current_value
                .cloned()
                .or_else(|| schema.default.clone())
                .unwrap_or_else(|| serde_json::json!([]));

            SettingControl::ObjectArray {
                label: schema.name.clone(),
                items: array_value.as_array().cloned().unwrap_or_default(),
                item_schema: Some(Box::new((**item_schema).clone())),
                display_field: display_field.clone(),
            }
        }

        SettingType::Complex => json_control(&schema.name, current_value, schema.default.as_ref()),
    };

    // Determine layer source for this setting
    let layer_source = ctx
        .layer_sources
        .get(&schema.path)
        .copied()
        .unwrap_or(ConfigLayer::System);

    // NEW SEMANTICS: "modified" means the value is defined in the target layer being edited.
    // Auto-managed maps (no_add like plugins/languages) are never "modified" at the container level.
    let modified = if is_auto_managed {
        false // Auto-managed content never shows as modified
    } else {
        layer_source == ctx.target_layer
    };

    // Clean description to remove redundancy with name
    let cleaned_description = clean_description(&schema.name, schema.description.as_deref());

    SettingItem {
        path: schema.path.clone(),
        name: schema.name.clone(),
        description: cleaned_description,
        control,
        default: schema.default.clone(),
        modified,
        layer_source,
        read_only: schema.read_only,
        is_auto_managed,
        nullable: schema.nullable,
        is_null,
        section: schema.section.clone(),
        is_section_start: false, // Set later in build_page after sorting
        style: ItemBoxStyle::default(),
        dual_list_sibling: schema.dual_list_sibling.clone(),
    }
}

/// Build a setting item with a value provided directly (for dialogs)
pub fn build_item_from_value(
    schema: &SettingSchema,
    current_value: Option<&serde_json::Value>,
    available_status_bar_tokens: &HashMap<String, String>,
) -> SettingItem {
    // Create control based on type
    let control = match &schema.setting_type {
        SettingType::Boolean => {
            let checked = current_value
                .and_then(|v| v.as_bool())
                .or_else(|| schema.default.as_ref().and_then(|d| d.as_bool()))
                .unwrap_or(false);
            // A nullable boolean with no explicit value is *inherited*: render
            // it as a neutral chip rather than a definite off-state so it isn't
            // misread as disabled (issue #2345).
            let inherited = schema.nullable
                && current_value
                    .map(|v| v.is_null())
                    .unwrap_or(schema.default.as_ref().map(|d| d.is_null()).unwrap_or(true));
            SettingControl::Toggle {
                label: schema.name.clone(),
                checked,
                inherited,
            }
        }

        SettingType::Integer { minimum, maximum } => {
            let value = current_value
                .and_then(|v| v.as_i64())
                .or_else(|| schema.default.as_ref().and_then(|d| d.as_i64()))
                .unwrap_or(0);

            SettingControl::Number {
                label: schema.name.clone(),
                value: value as f64,
                min: minimum.map(|m| m as f64),
                max: maximum.map(|m| m as f64),
                integer: true,
                percent: false,
            }
        }

        SettingType::Number { minimum, maximum } => {
            // A float is a fraction its cell shows as a percentage.
            let value = current_value
                .and_then(|v| v.as_f64())
                .or_else(|| schema.default.as_ref().and_then(|d| d.as_f64()))
                .unwrap_or(0.0);
            SettingControl::Number {
                label: schema.name.clone(),
                value,
                min: *minimum,
                max: *maximum,
                integer: false,
                percent: true,
            }
        }

        SettingType::String => {
            let value = current_value
                .and_then(|v| v.as_str())
                .or_else(|| schema.default.as_ref().and_then(|d| d.as_str()))
                .unwrap_or("");

            SettingControl::Text {
                label: schema.name.clone(),
                value: value.to_string(),
                placeholder: String::new(),
            }
        }

        SettingType::Enum { options } => {
            // Handle null values in enums (represented as empty string in dropdown values)
            let current = if current_value.map(|v| v.is_null()).unwrap_or(false) {
                "" // null maps to empty string (Auto-detect option)
            } else {
                current_value
                    .and_then(|v| v.as_str())
                    .or_else(|| {
                        let default = schema.default.as_ref()?;
                        if default.is_null() {
                            Some("")
                        } else {
                            default.as_str()
                        }
                    })
                    .unwrap_or("")
            };

            let display_names: Vec<String> = options.iter().map(|o| o.name.clone()).collect();
            let values: Vec<String> = options.iter().map(|o| o.value.clone()).collect();
            let selected = values.iter().position(|v| v == current).unwrap_or(0);
            SettingControl::dropdown(&schema.name, display_names, values, selected)
        }

        SettingType::DualList { options, .. } => {
            // Dialog context has no sibling to cross-exclude against
            build_dual_list(
                schema,
                options,
                current_value,
                vec![],
                available_status_bar_tokens,
            )
        }

        SettingType::StringArray => {
            let items: Vec<String> = current_value
                .and_then(|v| v.as_array())
                .map(|arr| {
                    arr.iter()
                        .filter_map(|v| v.as_str().map(String::from))
                        .collect()
                })
                .or_else(|| {
                    schema.default.as_ref().and_then(|d| {
                        d.as_array().map(|arr| {
                            arr.iter()
                                .filter_map(|v| v.as_str().map(String::from))
                                .collect()
                        })
                    })
                })
                .unwrap_or_default();

            SettingControl::TextList {
                label: schema.name.clone(),
                items,
                integer: false,
            }
        }

        SettingType::IntegerArray => {
            let items: Vec<String> = current_value
                .and_then(|v| v.as_array())
                .map(|arr| {
                    arr.iter()
                        .filter_map(|v| {
                            v.as_i64()
                                .map(|n| n.to_string())
                                .or_else(|| v.as_u64().map(|n| n.to_string()))
                                .or_else(|| v.as_f64().map(|n| n.to_string()))
                        })
                        .collect()
                })
                .or_else(|| {
                    schema.default.as_ref().and_then(|d| {
                        d.as_array().map(|arr| {
                            arr.iter()
                                .filter_map(|v| {
                                    v.as_i64()
                                        .map(|n| n.to_string())
                                        .or_else(|| v.as_u64().map(|n| n.to_string()))
                                        .or_else(|| v.as_f64().map(|n| n.to_string()))
                                })
                                .collect()
                        })
                    })
                })
                .unwrap_or_default();

            SettingControl::TextList {
                label: schema.name.clone(),
                items,
                integer: true,
            }
        }

        SettingType::Object { .. } => {
            json_control(&schema.name, current_value, schema.default.as_ref())
        }

        SettingType::Map {
            value_schema,
            display_field,
            no_add,
        } => {
            let map_value = current_value
                .cloned()
                .or_else(|| schema.default.clone())
                .unwrap_or_else(|| serde_json::json!({}));

            SettingControl::Map {
                label: schema.name.clone(),
                entries: map_entries(&map_value),
                value_schema: Some(Box::new((**value_schema).clone())),
                display_field: display_field.clone(),
                no_add: *no_add,
            }
        }

        SettingType::ObjectArray {
            item_schema,
            display_field,
        } => {
            let array_value = current_value
                .cloned()
                .or_else(|| schema.default.clone())
                .unwrap_or_else(|| serde_json::json!([]));

            SettingControl::ObjectArray {
                label: schema.name.clone(),
                items: array_value.as_array().cloned().unwrap_or_default(),
                item_schema: Some(Box::new((**item_schema).clone())),
                display_field: display_field.clone(),
            }
        }

        SettingType::Complex => json_control(&schema.name, current_value, schema.default.as_ref()),
    };

    // For dialog items, we use the traditional definition of "modified":
    // differs from schema default (since we don't have layer context in dialogs)
    let modified = match (&current_value, &schema.default) {
        (Some(current), Some(default)) => *current != default,
        (Some(_), None) => true,
        _ => false,
    };

    // Check if this is an auto-managed map (no_add)
    let is_auto_managed = matches!(&schema.setting_type, SettingType::Map { no_add: true, .. });

    let is_null = schema.nullable
        && current_value
            .map(|v| v.is_null())
            .unwrap_or(schema.default.as_ref().map(|d| d.is_null()).unwrap_or(true));

    SettingItem {
        path: schema.path.clone(),
        name: schema.name.clone(),
        description: schema.description.clone(),
        control,
        default: schema.default.clone(),
        modified,
        // For dialogs, we don't track layer source - default to System
        layer_source: ConfigLayer::System,
        read_only: schema.read_only,
        is_auto_managed,
        nullable: schema.nullable,
        is_null,
        section: schema.section.clone(),
        is_section_start: false, // Not used in dialogs
        style: ItemBoxStyle::default(),
        dual_list_sibling: schema.dual_list_sibling.clone(),
    }
}

/// Extract the current value from a control
pub fn control_to_value(control: &SettingControl) -> serde_json::Value {
    match control {
        SettingControl::Toggle { checked, .. } => serde_json::Value::Bool(*checked),

        SettingControl::Number { value, integer, .. } => match integer {
            true => serde_json::Value::Number((value.round() as i64).into()),
            false => serde_json::Number::from_f64(*value)
                .map(serde_json::Value::Number)
                .unwrap_or(serde_json::Value::Null),
        },

        SettingControl::Dropdown { .. } => control
            .dropdown_selected_value()
            .map(|s| {
                if s.is_empty() {
                    // Empty string represents null in nullable enums
                    serde_json::Value::Null
                } else {
                    serde_json::Value::String(s.to_string())
                }
            })
            .unwrap_or(serde_json::Value::Null),

        SettingControl::Text { value, .. } => serde_json::Value::String(value.clone()),

        SettingControl::TextList { items, integer, .. } => serde_json::Value::Array(
            items
                .iter()
                .filter_map(|s| match integer {
                    true => s
                        .parse::<i64>()
                        .ok()
                        .map(|n| serde_json::Value::Number(n.into())),
                    false => Some(serde_json::Value::String(s.clone())),
                })
                .collect(),
        ),

        SettingControl::DualList { included, .. } => serde_json::Value::Array(
            included
                .iter()
                .map(|s| serde_json::Value::String(s.clone()))
                .collect(),
        ),

        SettingControl::Map { entries, .. } => serde_json::Value::Object(
            entries
                .iter()
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect(),
        ),

        SettingControl::ObjectArray { items, .. } => serde_json::Value::Array(items.clone()),

        SettingControl::Json { text, .. } => {
            serde_json::from_str(text).unwrap_or(serde_json::Value::Null)
        }

        SettingControl::Complex { .. } => serde_json::Value::Null,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_config() -> serde_json::Value {
        serde_json::json!({
            "theme": "monokai",
            "check_for_updates": false,
            "editor": {
                "tab_size": 2,
                "line_numbers": true
            }
        })
    }

    /// Helper to create a BuildContext for testing
    fn test_context(config: &serde_json::Value) -> BuildContext<'_> {
        // Create static empty HashMap for layer_sources
        static EMPTY_SOURCES: std::sync::LazyLock<HashMap<String, ConfigLayer>> =
            std::sync::LazyLock::new(HashMap::new);
        static EMPTY_TOKENS: std::sync::LazyLock<HashMap<String, String>> =
            std::sync::LazyLock::new(HashMap::new);
        BuildContext {
            config_value: config,
            layer_sources: &EMPTY_SOURCES,
            target_layer: ConfigLayer::User,
            available_status_bar_tokens: &EMPTY_TOKENS,
            theme_options: &[],
        }
    }

    /// Helper to create a BuildContext with layer sources for testing "modified" behavior
    fn test_context_with_sources<'a>(
        config: &'a serde_json::Value,
        layer_sources: &'a HashMap<String, ConfigLayer>,
        target_layer: ConfigLayer,
    ) -> BuildContext<'a> {
        static EMPTY_TOKENS: std::sync::LazyLock<HashMap<String, String>> =
            std::sync::LazyLock::new(HashMap::new);
        BuildContext {
            config_value: config,
            layer_sources,
            target_layer,
            available_status_bar_tokens: &EMPTY_TOKENS,
            theme_options: &[],
        }
    }

    /// Regression test for #2738: a `theme` string setting carrying the
    /// reserved `x-enum-from: "$themes"` source renders as a dropdown populated
    /// from the live registry — built-ins AND a user theme, in Select-Theme
    /// order (display = name, value = portable form) — not from a static enum.
    #[test]
    fn test_theme_dropdown_populated_from_registry_with_user_theme() {
        // A user theme present in the registry, discovered exactly as Select
        // Theme would.
        let temp_dir = tempfile::tempdir().expect("temp dir");
        let themes_dir = temp_dir.path().to_path_buf();
        std::fs::write(
            themes_dir.join("my-user-theme.json"),
            r#"{"name":"my-user-theme","editor":{},"ui":{},"search":{},"diagnostic":{},"syntax":{}}"#,
        )
        .expect("write user theme");
        let registry = crate::view::theme::ThemeLoader::new(themes_dir).load_all(&[]);
        let theme_options: Vec<ThemeOption> = registry
            .settings_theme_options()
            .into_iter()
            .map(|(display, value)| ThemeOption { display, value })
            .collect();

        let schema = SettingSchema {
            path: "/theme".to_string(),
            name: "Theme".to_string(),
            description: Some("Color theme".to_string()),
            setting_type: SettingType::String,
            default: Some(serde_json::json!("high-contrast")),
            read_only: false,
            section: None,
            order: None,
            nullable: false,
            enum_from: Some("$themes".to_string()),
            dual_list_sibling: None,
            dynamically_extendable_status_bar_elements: false,
        };

        // Config stores the legacy bare built-in name "dark".
        let config = serde_json::json!({ "theme": "dark" });
        static EMPTY_SOURCES: std::sync::LazyLock<HashMap<String, ConfigLayer>> =
            std::sync::LazyLock::new(HashMap::new);
        static EMPTY_TOKENS: std::sync::LazyLock<HashMap<String, String>> =
            std::sync::LazyLock::new(HashMap::new);
        let ctx = BuildContext {
            config_value: &config,
            layer_sources: &EMPTY_SOURCES,
            target_layer: ConfigLayer::User,
            available_status_bar_tokens: &EMPTY_TOKENS,
            theme_options: &theme_options,
        };

        let item = build_item(&schema, &ctx);
        let SettingControl::Dropdown {
            options, values, ..
        } = &item.control
        else {
            panic!("theme should render as a dropdown, not free text");
        };

        // The dropdown lists exactly the registry themes, in list (picker)
        // order: display = name, value = portable form.
        assert_eq!(options.len(), registry.list().len());
        for (opt_display, info) in options.iter().zip(registry.list().iter()) {
            assert_eq!(opt_display, &info.name);
        }

        // The user theme is present and valued by its portable form.
        let user_idx = options
            .iter()
            .position(|d| d == "my-user-theme")
            .expect("user theme should be an option");
        assert_eq!(values[user_idx], "my-user-theme.json");

        // Legacy bare "dark" pre-selects the built-in whose portable form is
        // "builtin://dark".
        assert_eq!(
            item.control.dropdown_selected_value(),
            Some("builtin://dark")
        );
    }

    #[test]
    fn test_build_toggle_item() {
        let schema = SettingSchema {
            path: "/check_for_updates".to_string(),
            name: "Check For Updates".to_string(),
            description: Some("Check for updates".to_string()),
            setting_type: SettingType::Boolean,
            default: Some(serde_json::Value::Bool(true)),
            read_only: false,
            section: None,
            order: None,
            nullable: false,
            enum_from: None,
            dual_list_sibling: None,
            dynamically_extendable_status_bar_elements: false,
        };

        let config = sample_config();
        let ctx = test_context(&config);
        let item = build_item(&schema, &ctx);

        assert_eq!(item.path, "/check_for_updates");
        // With new semantics, modified = false when layer_sources is empty
        // (value is not defined in target layer)
        assert!(!item.modified);
        assert_eq!(item.layer_source, ConfigLayer::System);

        if let SettingControl::Toggle { checked, .. } = &item.control {
            assert!(!checked); // Current value is false
        } else {
            panic!("Expected toggle control");
        }
    }

    #[test]
    fn test_build_toggle_item_modified_in_user_layer() {
        let schema = SettingSchema {
            path: "/check_for_updates".to_string(),
            name: "Check For Updates".to_string(),
            description: Some("Check for updates".to_string()),
            setting_type: SettingType::Boolean,
            default: Some(serde_json::Value::Bool(true)),
            read_only: false,
            section: None,
            order: None,
            nullable: false,
            enum_from: None,
            dual_list_sibling: None,
            dynamically_extendable_status_bar_elements: false,
        };

        let config = sample_config();
        let mut layer_sources = HashMap::new();
        layer_sources.insert("/check_for_updates".to_string(), ConfigLayer::User);
        let ctx = test_context_with_sources(&config, &layer_sources, ConfigLayer::User);
        let item = build_item(&schema, &ctx);

        // With new semantics: modified = true because value is defined in User layer
        // and target_layer is User
        assert!(item.modified);
        assert_eq!(item.layer_source, ConfigLayer::User);
    }

    #[test]
    fn test_build_number_item() {
        let schema = SettingSchema {
            path: "/editor/tab_size".to_string(),
            name: "Tab Size".to_string(),
            description: None,
            setting_type: SettingType::Integer {
                minimum: Some(1),
                maximum: Some(16),
            },
            default: Some(serde_json::Value::Number(4.into())),
            read_only: false,
            section: None,
            order: None,
            nullable: false,
            enum_from: None,
            dual_list_sibling: None,
            dynamically_extendable_status_bar_elements: false,
        };

        let config = sample_config();
        let ctx = test_context(&config);
        let item = build_item(&schema, &ctx);

        // With new semantics, modified = false when layer_sources is empty
        assert!(!item.modified);

        if let SettingControl::Number {
            value, min, max, ..
        } = &item.control
        {
            assert_eq!(*value, 2.0);
            assert_eq!(*min, Some(1.0));
            assert_eq!(*max, Some(16.0));
        } else {
            panic!("Expected number control");
        }
    }

    #[test]
    fn test_build_text_item() {
        let schema = SettingSchema {
            path: "/theme".to_string(),
            name: "Theme".to_string(),
            description: None,
            setting_type: SettingType::String,
            default: Some(serde_json::Value::String("high-contrast".to_string())),
            read_only: false,
            section: None,
            order: None,
            nullable: false,
            enum_from: None,
            dual_list_sibling: None,
            dynamically_extendable_status_bar_elements: false,
        };

        let config = sample_config();
        let ctx = test_context(&config);
        let item = build_item(&schema, &ctx);

        // With new semantics, modified = false when layer_sources is empty
        assert!(!item.modified);

        if let SettingControl::Text { value, .. } = &item.control {
            assert_eq!(value, "monokai");
        } else {
            panic!("Expected text control");
        }
    }

    #[test]
    fn test_clean_description_keeps_full_desc_with_new_info() {
        // "Tab Size" + "Number of spaces per tab character" -> keeps full desc (has "spaces", "character")
        let result = clean_description("Tab Size", Some("Number of spaces per tab character"));
        assert!(result.is_some());
        let cleaned = result.unwrap();
        // Should preserve original casing and contain the full info
        assert!(cleaned.starts_with('N')); // uppercase 'N' from "Number"
        assert!(cleaned.contains("spaces"));
        assert!(cleaned.contains("character"));
    }

    #[test]
    fn test_clean_description_keeps_extra_info() {
        // "Line Numbers" + "Show line numbers in the gutter" -> should keep full desc with "gutter"
        let result = clean_description("Line Numbers", Some("Show line numbers in the gutter"));
        assert!(result.is_some());
        let cleaned = result.unwrap();
        assert!(cleaned.contains("gutter"));
    }

    #[test]
    fn test_clean_description_returns_none_for_pure_redundancy() {
        // If description is just the name repeated, return None
        let result = clean_description("Theme", Some("Theme"));
        assert!(result.is_none());

        // Or only filler words around the name
        let result = clean_description("Theme", Some("The theme to use"));
        assert!(result.is_none());
    }

    #[test]
    fn test_clean_description_returns_none_for_empty() {
        let result = clean_description("Theme", Some(""));
        assert!(result.is_none());

        let result = clean_description("Theme", None);
        assert!(result.is_none());
    }

    #[test]
    fn test_control_to_value() {
        let toggle = SettingControl::Toggle {
            label: "Test".into(),
            checked: true,
            inherited: false,
        };
        assert_eq!(control_to_value(&toggle), serde_json::Value::Bool(true));

        let number = SettingControl::Number {
            label: "Test".into(),
            value: 42.0,
            min: None,
            max: None,
            integer: true,
            percent: false,
        };
        assert_eq!(control_to_value(&number), serde_json::json!(42));

        let text = SettingControl::Text {
            label: "Test".into(),
            value: "hello".into(),
            placeholder: String::new(),
        };
        assert_eq!(
            control_to_value(&text),
            serde_json::Value::String("hello".to_string())
        );
    }
}
