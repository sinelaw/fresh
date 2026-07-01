//! Setting items for the UI
//!
//! Converts schema information into renderable setting items.

use super::schema::{SettingCategory, SettingSchema, SettingType};
use crate::config_io::ConfigLayer;
use crate::view::controls::{
    DropdownState, DualListState, FocusState, KeybindingListState, MapState, NumberInputState,
    TextInputState, TextListState, ToggleState,
};
use crate::view::ui::{FocusRegion, ScrollItem, TextEdit};
use std::collections::{HashMap, HashSet};

/// State for multiline JSON editing
#[derive(Debug, Clone)]
pub struct JsonEditState {
    /// The text editor state
    pub editor: TextEdit,
    /// Original text (for revert on Escape)
    pub original_text: String,
    /// Label for the control
    pub label: String,
    /// Focus state
    pub focus: FocusState,
    /// Scroll offset for viewing (used by entry dialog)
    pub scroll_offset: usize,
    /// Maximum visible lines (for main settings panel)
    pub max_visible_lines: usize,
}

impl JsonEditState {
    /// Create a new JSON edit state with pretty-printed JSON
    pub fn new(label: impl Into<String>, value: Option<&serde_json::Value>) -> Self {
        let json_str = value
            .map(|v| serde_json::to_string_pretty(v).unwrap_or_else(|_| "null".to_string()))
            .unwrap_or_else(|| "null".to_string());

        Self {
            original_text: json_str.clone(),
            editor: TextEdit::with_text(&json_str),
            label: label.into(),
            focus: FocusState::Normal,
            scroll_offset: 0,
            max_visible_lines: 6,
        }
    }

    /// True if the editor is showing the JSON `null` sentinel — i.e. the
    /// value is "not set". Used by the renderer to swap the literal
    /// `null` for a helpful placeholder line, and by `start_editing` to
    /// wipe the buffer so the user can type fresh JSON without manually
    /// deleting the placeholder first.
    pub fn is_unset(&self) -> bool {
        self.editor.value().trim() == "null"
    }

    /// Clear the `null` placeholder so subsequent keystrokes type into
    /// an empty editor. Called at the start of an edit session — paired
    /// with `restore_unset_if_empty` on stop so abandoning the edit
    /// brings the placeholder back.
    pub fn clear_placeholder_for_edit(&mut self) {
        if self.is_unset() {
            self.editor.set_value("");
        }
    }

    /// If the user left the editor empty, restore the `null` sentinel
    /// so the saved value round-trips as JSON null rather than as the
    /// empty string (which doesn't parse).
    pub fn restore_unset_if_empty(&mut self) {
        if self.editor.value().trim().is_empty() {
            self.editor.set_value("null");
        }
    }

    /// Revert to original value (for Escape key)
    pub fn revert(&mut self) {
        self.editor.set_value(&self.original_text);
        self.scroll_offset = 0;
    }

    /// Commit current value as the new original (after saving)
    pub fn commit(&mut self) {
        self.original_text = self.editor.value();
    }

    /// Get the full text value
    pub fn value(&self) -> String {
        self.editor.value()
    }

    /// Check if the JSON is valid. An empty editor counts as valid:
    /// it's the "the user just opened a not-yet-set field" state, which
    /// `restore_unset_if_empty` will round-trip back to JSON `null` on
    /// close. Treating empty as invalid otherwise flashes a spurious
    /// "Invalid JSON" footer the moment the user presses Enter on a
    /// null field.
    pub fn is_valid(&self) -> bool {
        let text = self.value();
        text.trim().is_empty() || serde_json::from_str::<serde_json::Value>(&text).is_ok()
    }

    /// Get number of lines to display (all lines)
    pub fn display_height(&self) -> usize {
        self.editor.line_count()
    }

    /// Get number of lines for constrained view (e.g., main settings panel)
    pub fn display_height_capped(&self) -> usize {
        self.editor.line_count().min(self.max_visible_lines)
    }

    /// Get lines for rendering
    pub fn lines(&self) -> &[String] {
        &self.editor.lines
    }

    /// Get cursor position (row, col)
    pub fn cursor_pos(&self) -> (usize, usize) {
        (self.editor.cursor_row, self.editor.cursor_col)
    }

    // Delegate editing methods to TextEdit
    pub fn insert(&mut self, c: char) {
        self.editor.insert_char(c);
    }

    pub fn insert_str(&mut self, s: &str) {
        self.editor.insert_str(s);
    }

    pub fn backspace(&mut self) {
        self.editor.backspace();
    }

    pub fn delete(&mut self) {
        self.editor.delete();
    }

    pub fn move_left(&mut self) {
        self.editor.move_left();
    }

    pub fn move_right(&mut self) {
        self.editor.move_right();
    }

    pub fn move_up(&mut self) {
        self.editor.move_up();
    }

    pub fn move_down(&mut self) {
        self.editor.move_down();
    }

    pub fn move_home(&mut self) {
        self.editor.move_home();
    }

    pub fn move_end(&mut self) {
        self.editor.move_end();
    }

    pub fn move_word_left(&mut self) {
        self.editor.move_word_left();
    }

    pub fn move_word_right(&mut self) {
        self.editor.move_word_right();
    }

    // Selection methods
    pub fn has_selection(&self) -> bool {
        self.editor.has_selection()
    }

    pub fn selection_range(&self) -> Option<((usize, usize), (usize, usize))> {
        self.editor.selection_range()
    }

    pub fn selected_text(&self) -> Option<String> {
        self.editor.selected_text()
    }

    pub fn delete_selection(&mut self) -> Option<String> {
        self.editor.delete_selection()
    }

    pub fn clear_selection(&mut self) {
        self.editor.clear_selection();
    }

    pub fn move_left_selecting(&mut self) {
        self.editor.move_left_selecting();
    }

    pub fn move_right_selecting(&mut self) {
        self.editor.move_right_selecting();
    }

    pub fn move_up_selecting(&mut self) {
        self.editor.move_up_selecting();
    }

    pub fn move_down_selecting(&mut self) {
        self.editor.move_down_selecting();
    }

    pub fn move_home_selecting(&mut self) {
        self.editor.move_home_selecting();
    }

    pub fn move_end_selecting(&mut self) {
        self.editor.move_end_selecting();
    }

    pub fn move_word_left_selecting(&mut self) {
        self.editor.move_word_left_selecting();
    }

    pub fn move_word_right_selecting(&mut self) {
        self.editor.move_word_right_selecting();
    }

    pub fn select_all(&mut self) {
        self.editor.select_all();
    }

    pub fn delete_word_forward(&mut self) {
        self.editor.delete_word_forward();
    }

    pub fn delete_word_backward(&mut self) {
        self.editor.delete_word_backward();
    }

    pub fn delete_to_end(&mut self) {
        self.editor.delete_to_end();
    }
}

/// Create a JSON control for editing arbitrary JSON values (multiline)
fn json_control(
    name: &str,
    current_value: Option<&serde_json::Value>,
    default: Option<&serde_json::Value>,
) -> SettingControl {
    let value = current_value.or(default);
    SettingControl::Json(JsonEditState::new(name, value))
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

/// Build a DualListState from schema options, current value, and optional sibling excluded set.
fn build_dual_list_state(
    schema: &SettingSchema,
    options: &[crate::view::settings::schema::EnumOption],
    current_value: Option<&serde_json::Value>,
    excluded: Vec<String>,
    available_status_bar_tokens: &HashMap<String, String>,
) -> DualListState {
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
    DualListState::new(&schema.name, all_options)
        .with_included(included)
        .with_excluded(excluded)
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
    Toggle(ToggleState),
    Number(NumberInputState),
    Dropdown(DropdownState),
    Text(TextInputState),
    TextList(TextListState),
    /// Dual-list picker for ordered subset selection (e.g., status bar elements)
    DualList(DualListState),
    /// Map/dictionary control for key-value pairs
    Map(MapState),
    /// Array of objects control (for keybindings, etc.)
    ObjectArray(KeybindingListState),
    /// Multiline JSON editor
    Json(JsonEditState),
    /// Complex settings that can't be edited inline
    Complex {
        type_name: String,
    },
}

impl SettingControl {
    /// Calculate the height needed for this control (in lines)
    pub fn control_height(&self) -> u16 {
        match self {
            // TextList needs: 1 label line + items + 1 "add new" row
            Self::TextList(state) => {
                // 1 for label + items count + 1 for add-new row
                (state.items.len() + 2) as u16
            }
            // DualList needs: 1 label + 1 header + body rows
            Self::DualList(state) => 2 + state.body_rows() as u16,
            // Map needs: 1 label + 1 header (if display_field) + entries + expanded content + 1 add-new row (if allowed)
            Self::Map(state) => {
                let header_row = if state.display_field.is_some() { 1 } else { 0 };
                let add_new_row = if state.no_add { 0 } else { 1 };
                let base = 1 + header_row + state.entries.len() + add_new_row; // label + header? + entries + add-new?
                                                                               // Add extra height for expanded entries (up to 6 lines each)
                let expanded_height: usize = state
                    .expanded
                    .iter()
                    .filter_map(|&idx| state.entries.get(idx))
                    .map(|(_, v)| {
                        if let Some(obj) = v.as_object() {
                            obj.len().min(5) + if obj.len() > 5 { 1 } else { 0 }
                        } else {
                            0
                        }
                    })
                    .sum();
                (base + expanded_height) as u16
            }
            // Dropdown needs extra height when open to show options
            Self::Dropdown(state) if state.open => {
                // 1 for label/button + number of options (max 8 visible)
                1 + state.options.len().min(8) as u16
            }
            // KeybindingList needs: 1 label + bindings + 1 add-new row
            Self::ObjectArray(state) => {
                // 1 for label + bindings count + 1 for add-new row
                (state.bindings.len() + 2) as u16
            }
            // Json needs: 1 label + visible lines
            Self::Json(state) => {
                // 1 for label + displayed lines
                1 + state.display_height() as u16
            }
            // All other controls fit in 1 line
            _ => 1,
        }
    }

    /// Whether this is a composite control (TextList, Map, ObjectArray) that has
    /// internal sub-items. For composite controls, highlighting should be per-row,
    /// not across the entire control area.
    pub fn is_composite(&self) -> bool {
        matches!(
            self,
            Self::TextList(_) | Self::DualList(_) | Self::Map(_) | Self::ObjectArray(_)
        )
    }

    /// Get the row offset of the focused sub-item within a composite control.
    /// Returns 0 for non-composite controls or if no sub-item is focused.
    /// The offset is relative to the start of the control's render area.
    pub fn focused_sub_row(&self) -> u16 {
        match self {
            Self::TextList(state) => {
                // Row 0 = label, rows 1..N = items, row N+1 = add-new
                match state.focused_item {
                    Some(idx) => 1 + idx as u16,          // item rows start at offset 1
                    None => 1 + state.items.len() as u16, // add-new row
                }
            }
            Self::DualList(state) => {
                // Row 0 = label, Row 1 = headers, Rows 2+ = body
                use crate::view::controls::DualListColumn;
                let row = match state.active_column {
                    DualListColumn::Available => state.available_cursor,
                    DualListColumn::Included => state.included_cursor,
                };
                2 + row as u16
            }
            Self::ObjectArray(state) => {
                // Row 0 = label, rows 1..N = bindings, row N+1 = add-new
                match state.focused_index {
                    Some(idx) => 1 + idx as u16,
                    None => 1 + state.bindings.len() as u16,
                }
            }
            Self::Map(state) => {
                // Row 0 = label, row 1 = header (if display_field), then entries, then add-new
                let header_offset = if state.display_field.is_some() { 1 } else { 0 };
                match state.focused_entry {
                    Some(idx) => 1 + header_offset + idx as u16,
                    None => 1 + header_offset + state.entries.len() as u16,
                }
            }
            _ => 0,
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

impl ScrollItem for SettingItem {
    fn height(&self, width: u16) -> u16 {
        self.layout_box(width, &self.style).total_rows()
    }

    fn focus_regions(&self, width: u16) -> Vec<FocusRegion> {
        // y_offset is ABSOLUTE within the item — `ScrollablePanel` adds it
        // to the cumulative item-y to compute a screen y for
        // `ensure_visible`. Since the item now starts with a section header
        // and/or a card top border above the control row, y=0 of the
        // control is `plan.control_y()` rows down from the item top. Using
        // 0 here scrolls the viewport to the chrome, not to the actual
        // entry, which is exactly the bug that hid the focused map entry
        // off-screen on search-jump.
        let plan = self.layout_box(width, &self.style);
        let label_y = plan.control_y();

        match &self.control {
            // TextList: each row is a focus region
            SettingControl::TextList(state) => {
                let mut regions = Vec::new();
                // Label row
                regions.push(FocusRegion {
                    id: 0,
                    y_offset: label_y,
                    height: 1,
                });
                // Each item row (id = 1 + row_index)
                for i in 0..state.items.len() {
                    regions.push(FocusRegion {
                        id: 1 + i,
                        y_offset: label_y + 1 + i as u16,
                        height: 1,
                    });
                }
                // Add-new row
                regions.push(FocusRegion {
                    id: 1 + state.items.len(),
                    y_offset: label_y + 1 + state.items.len() as u16,
                    height: 1,
                });
                regions
            }
            // DualList: label + header + body rows
            SettingControl::DualList(state) => {
                let mut regions = Vec::new();
                // Label row
                regions.push(FocusRegion {
                    id: 0,
                    y_offset: label_y,
                    height: 1,
                });
                // Header row (not selectable, but takes space)
                // Body rows (id = 1 + row_index)
                let body = state.body_rows();
                for i in 0..body {
                    regions.push(FocusRegion {
                        id: 1 + i,
                        y_offset: label_y + 2 + i as u16, // after label + header
                        height: 1,
                    });
                }
                regions
            }
            // Map: each entry row is a focus region
            SettingControl::Map(state) => {
                let mut regions = Vec::new();
                let mut y = label_y;

                // Label row
                regions.push(FocusRegion {
                    id: 0,
                    y_offset: y,
                    height: 1,
                });
                y += 1;

                // Column header row (if display_field is set)
                if state.display_field.is_some() {
                    y += 1;
                }

                // Each entry (id = 1 + entry_index)
                for (i, (_, v)) in state.entries.iter().enumerate() {
                    let mut entry_height = 1u16;
                    // Add expanded content height if expanded
                    if state.expanded.contains(&i) {
                        if let Some(obj) = v.as_object() {
                            entry_height += obj.len().min(5) as u16;
                            if obj.len() > 5 {
                                entry_height += 1;
                            }
                        }
                    }
                    regions.push(FocusRegion {
                        id: 1 + i,
                        y_offset: y,
                        height: entry_height,
                    });
                    y += entry_height;
                }

                // Add-new row
                regions.push(FocusRegion {
                    id: 1 + state.entries.len(),
                    y_offset: y,
                    height: 1,
                });
                regions
            }
            // KeybindingList: each entry row is a focus region
            SettingControl::ObjectArray(state) => {
                let mut regions = Vec::new();
                // Label row
                regions.push(FocusRegion {
                    id: 0,
                    y_offset: label_y,
                    height: 1,
                });
                // Each binding (id = 1 + index)
                for i in 0..state.bindings.len() {
                    regions.push(FocusRegion {
                        id: 1 + i,
                        y_offset: label_y + 1 + i as u16,
                        height: 1,
                    });
                }
                // Add-new row
                regions.push(FocusRegion {
                    id: 1 + state.bindings.len(),
                    y_offset: label_y + 1 + state.bindings.len() as u16,
                    height: 1,
                });
                regions
            }
            // Other controls: single region covering the card content.
            _ => {
                vec![FocusRegion {
                    id: 0,
                    y_offset: label_y,
                    height: plan.content_rows(),
                }]
            }
        }
    }
}

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
}

/// Convert a category tree into pages with control states
pub fn build_pages(
    categories: &[SettingCategory],
    config_value: &serde_json::Value,
    layer_sources: &HashMap<String, ConfigLayer>,
    target_layer: ConfigLayer,
    available_status_bar_tokens: &HashMap<String, String>,
) -> Vec<SettingsPage> {
    let ctx = BuildContext {
        config_value,
        layer_sources,
        target_layer,
        available_status_bar_tokens,
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
            SettingControl::Toggle(ToggleState::new(checked, &schema.name))
        }

        SettingType::Integer { minimum, maximum } => {
            let value = current_value
                .and_then(|v| v.as_i64())
                .or_else(|| schema.default.as_ref().and_then(|d| d.as_i64()))
                .unwrap_or(0);

            let mut state = NumberInputState::new(value, &schema.name);
            if let Some(min) = minimum {
                state = state.with_min(*min);
            }
            if let Some(max) = maximum {
                state = state.with_max(*max);
            }
            SettingControl::Number(state)
        }

        SettingType::Number { minimum, maximum } => {
            // For floats, we display as integers (multiply by 100 for percentages)
            let value = current_value
                .and_then(|v| v.as_f64())
                .or_else(|| schema.default.as_ref().and_then(|d| d.as_f64()))
                .unwrap_or(0.0);

            // Convert to integer representation
            let int_value = (value * 100.0).round() as i64;
            let mut state = NumberInputState::new(int_value, &schema.name).with_percentage();
            if let Some(min) = minimum {
                state = state.with_min((*min * 100.0) as i64);
            }
            if let Some(max) = maximum {
                state = state.with_max((*max * 100.0) as i64);
            }
            SettingControl::Number(state)
        }

        SettingType::String => {
            let value = current_value
                .and_then(|v| v.as_str())
                .or_else(|| schema.default.as_ref().and_then(|d| d.as_str()))
                .unwrap_or("");

            // Check for dynamic enum: derive dropdown options from another config field's keys
            if let Some(ref source_path) = schema.enum_from {
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
                let state = DropdownState::with_values(display_names, values, &schema.name)
                    .with_selected(selected);
                SettingControl::Dropdown(state)
            } else {
                let state = TextInputState::new(&schema.name).with_value(value);
                SettingControl::Text(state)
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
            let state = DropdownState::with_values(display_names, values, &schema.name)
                .with_selected(selected);
            SettingControl::Dropdown(state)
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
            SettingControl::DualList(build_dual_list_state(
                schema,
                options,
                current_value,
                excluded,
                ctx.available_status_bar_tokens,
            ))
        }

        SettingType::StringArray => {
            let items = value_as_string_array(current_value, schema.default.as_ref());
            let state = TextListState::new(&schema.name).with_items(items);
            SettingControl::TextList(state)
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

            let state = TextListState::new(&schema.name)
                .with_items(items)
                .with_integer_mode();
            SettingControl::TextList(state)
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

            let mut state = MapState::new(&schema.name).with_entries(&map_value);
            state = state.with_value_schema((**value_schema).clone());
            if let Some(field) = display_field {
                state = state.with_display_field(field.clone());
            }
            if *no_add {
                state = state.with_no_add(true);
            }
            SettingControl::Map(state)
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

            let mut state = KeybindingListState::new(&schema.name).with_bindings(&array_value);
            state = state.with_item_schema((**item_schema).clone());
            if let Some(field) = display_field {
                state = state.with_display_field(field.clone());
            }
            SettingControl::ObjectArray(state)
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
            SettingControl::Toggle(
                ToggleState::new(checked, &schema.name).with_inherited(inherited),
            )
        }

        SettingType::Integer { minimum, maximum } => {
            let value = current_value
                .and_then(|v| v.as_i64())
                .or_else(|| schema.default.as_ref().and_then(|d| d.as_i64()))
                .unwrap_or(0);

            let mut state = NumberInputState::new(value, &schema.name);
            if let Some(min) = minimum {
                state = state.with_min(*min);
            }
            if let Some(max) = maximum {
                state = state.with_max(*max);
            }
            SettingControl::Number(state)
        }

        SettingType::Number { minimum, maximum } => {
            let value = current_value
                .and_then(|v| v.as_f64())
                .or_else(|| schema.default.as_ref().and_then(|d| d.as_f64()))
                .unwrap_or(0.0);

            let int_value = (value * 100.0).round() as i64;
            let mut state = NumberInputState::new(int_value, &schema.name).with_percentage();
            if let Some(min) = minimum {
                state = state.with_min((*min * 100.0) as i64);
            }
            if let Some(max) = maximum {
                state = state.with_max((*max * 100.0) as i64);
            }
            SettingControl::Number(state)
        }

        SettingType::String => {
            let value = current_value
                .and_then(|v| v.as_str())
                .or_else(|| schema.default.as_ref().and_then(|d| d.as_str()))
                .unwrap_or("");

            let state = TextInputState::new(&schema.name).with_value(value);
            SettingControl::Text(state)
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
            let state = DropdownState::with_values(display_names, values, &schema.name)
                .with_selected(selected);
            SettingControl::Dropdown(state)
        }

        SettingType::DualList { options, .. } => {
            // Dialog context has no sibling to cross-exclude against
            SettingControl::DualList(build_dual_list_state(
                schema,
                options,
                current_value,
                vec![],
                available_status_bar_tokens,
            ))
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

            let state = TextListState::new(&schema.name).with_items(items);
            SettingControl::TextList(state)
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

            let state = TextListState::new(&schema.name)
                .with_items(items)
                .with_integer_mode();
            SettingControl::TextList(state)
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

            let mut state = MapState::new(&schema.name).with_entries(&map_value);
            state = state.with_value_schema((**value_schema).clone());
            if let Some(field) = display_field {
                state = state.with_display_field(field.clone());
            }
            if *no_add {
                state = state.with_no_add(true);
            }
            SettingControl::Map(state)
        }

        SettingType::ObjectArray {
            item_schema,
            display_field,
        } => {
            let array_value = current_value
                .cloned()
                .or_else(|| schema.default.clone())
                .unwrap_or_else(|| serde_json::json!([]));

            let mut state = KeybindingListState::new(&schema.name).with_bindings(&array_value);
            state = state.with_item_schema((**item_schema).clone());
            if let Some(field) = display_field {
                state = state.with_display_field(field.clone());
            }
            SettingControl::ObjectArray(state)
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
        SettingControl::Toggle(state) => serde_json::Value::Bool(state.checked),

        SettingControl::Number(state) => {
            if state.is_percentage {
                // Convert back to float (divide by 100)
                let float_value = state.value as f64 / 100.0;
                serde_json::Number::from_f64(float_value)
                    .map(serde_json::Value::Number)
                    .unwrap_or(serde_json::Value::Number(state.value.into()))
            } else {
                serde_json::Value::Number(state.value.into())
            }
        }

        SettingControl::Dropdown(state) => state
            .selected_value()
            .map(|s| {
                if s.is_empty() {
                    // Empty string represents null in nullable enums
                    serde_json::Value::Null
                } else {
                    serde_json::Value::String(s.to_string())
                }
            })
            .unwrap_or(serde_json::Value::Null),

        SettingControl::Text(state) => serde_json::Value::String(state.value.clone()),

        SettingControl::TextList(state) => {
            let arr: Vec<serde_json::Value> = state
                .items
                .iter()
                .filter_map(|s| {
                    if state.is_integer {
                        s.parse::<i64>()
                            .ok()
                            .map(|n| serde_json::Value::Number(n.into()))
                    } else {
                        Some(serde_json::Value::String(s.clone()))
                    }
                })
                .collect();
            serde_json::Value::Array(arr)
        }

        SettingControl::DualList(state) => {
            let arr: Vec<serde_json::Value> = state
                .included
                .iter()
                .map(|s| serde_json::Value::String(s.clone()))
                .collect();
            serde_json::Value::Array(arr)
        }

        SettingControl::Map(state) => state.to_value(),

        SettingControl::ObjectArray(state) => state.to_value(),

        SettingControl::Json(state) => {
            // Parse the JSON string back to a value
            serde_json::from_str(&state.value()).unwrap_or(serde_json::Value::Null)
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
        }
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

        if let SettingControl::Toggle(state) = &item.control {
            assert!(!state.checked); // Current value is false
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

        if let SettingControl::Number(state) = &item.control {
            assert_eq!(state.value, 2);
            assert_eq!(state.min, Some(1));
            assert_eq!(state.max, Some(16));
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

        if let SettingControl::Text(state) = &item.control {
            assert_eq!(state.value, "monokai");
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
        let toggle = SettingControl::Toggle(ToggleState::new(true, "Test"));
        assert_eq!(control_to_value(&toggle), serde_json::Value::Bool(true));

        let number = SettingControl::Number(NumberInputState::new(42, "Test"));
        assert_eq!(control_to_value(&number), serde_json::json!(42));

        let text = SettingControl::Text(TextInputState::new("Test").with_value("hello"));
        assert_eq!(
            control_to_value(&text),
            serde_json::Value::String("hello".to_string())
        );
    }
}
