//! Dual-list control for selecting and ordering items from a fixed set
//!
//! Renders as two side-by-side columns with transfer/reorder buttons:
//! ```text
//! Label:
//!   Available        Included
//!   cursor:compact [>] filename
//!   chord          [<] cursor
//!   clock          [▲] diagnostics
//!                  [▼] cursor_count
//! ```

mod render;

use ratatui::layout::Rect;
use ratatui::style::Color;

pub use render::render_dual_list_partial;

use super::FocusState;

/// Which column is currently active
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum DualListColumn {
    #[default]
    Available,
    Included,
}

/// State for a dual-list control
#[derive(Debug, Clone)]
pub struct DualListState {
    /// Full universe of options: (value, display_name)
    pub all_options: Vec<(String, String)>,
    /// Ordered values currently in this list
    pub included: Vec<String>,
    /// Values in the sibling list (not available for selection)
    pub excluded: Vec<String>,
    /// Which column is active
    pub active_column: DualListColumn,
    /// Cursor position in the Available column
    pub available_cursor: usize,
    /// Cursor position in the Included column
    pub included_cursor: usize,
    /// Label displayed above the control
    pub label: String,
    /// Focus state
    pub focus: FocusState,
    /// Whether the control is in active editing mode (Enter was pressed)
    pub editing: bool,
}

impl DualListState {
    /// Create a new dual-list state
    pub fn new(label: impl Into<String>, all_options: Vec<(String, String)>) -> Self {
        Self {
            all_options,
            included: Vec::new(),
            excluded: Vec::new(),
            active_column: DualListColumn::Available,
            available_cursor: 0,
            included_cursor: 0,
            label: label.into(),
            focus: FocusState::Normal,
            editing: false,
        }
    }

    /// Set included items
    pub fn with_included(mut self, included: Vec<String>) -> Self {
        self.included = included;
        self
    }

    /// Set excluded items (from sibling)
    pub fn with_excluded(mut self, excluded: Vec<String>) -> Self {
        self.excluded = excluded;
        self
    }

    /// Get available items (all_options minus included minus excluded)
    pub fn available_items(&self) -> Vec<&(String, String)> {
        self.all_options
            .iter()
            .filter(|(value, _)| !self.included.contains(value) && !self.excluded.contains(value))
            .collect()
    }

    /// Get included items with display names
    pub fn included_items(&self) -> Vec<(&String, &String)> {
        self.included
            .iter()
            .filter_map(|value| {
                self.all_options
                    .iter()
                    .find(|(v, _)| v == value)
                    .map(|(v, name)| (v, name))
            })
            .collect()
    }

    /// Move the selected available item to included
    pub fn add_selected(&mut self) {
        let available = self.available_items();
        if self.available_cursor < available.len() {
            let value = available[self.available_cursor].0.clone();
            self.included.push(value);
            let new_len = self.available_items().len();
            if self.available_cursor >= new_len && new_len > 0 {
                self.available_cursor = new_len - 1;
            }
        }
    }

    /// Remove the selected included item back to available
    pub fn remove_selected(&mut self) {
        if self.included_cursor < self.included.len() {
            self.included.remove(self.included_cursor);
            if self.included_cursor >= self.included.len() && !self.included.is_empty() {
                self.included_cursor = self.included.len() - 1;
            }
        }
    }

    /// Move the selected included item up (only when Included column is active)
    pub fn move_up(&mut self) {
        if self.active_column != DualListColumn::Included {
            return;
        }
        if self.included_cursor > 0 && self.included_cursor < self.included.len() {
            self.included
                .swap(self.included_cursor, self.included_cursor - 1);
            self.included_cursor -= 1;
        }
    }

    /// Move the selected included item down (only when Included column is active)
    pub fn move_down(&mut self) {
        if self.active_column != DualListColumn::Included {
            return;
        }
        if self.included_cursor + 1 < self.included.len() {
            self.included
                .swap(self.included_cursor, self.included_cursor + 1);
            self.included_cursor += 1;
        }
    }

    /// Move cursor up in the active column
    pub fn cursor_up(&mut self) {
        match self.active_column {
            DualListColumn::Available => {
                if self.available_cursor > 0 {
                    self.available_cursor -= 1;
                }
            }
            DualListColumn::Included => {
                if self.included_cursor > 0 {
                    self.included_cursor -= 1;
                }
            }
        }
    }

    /// Move cursor down in the active column
    pub fn cursor_down(&mut self) {
        match self.active_column {
            DualListColumn::Available => {
                let len = self.available_items().len();
                if len > 0 && self.available_cursor + 1 < len {
                    self.available_cursor += 1;
                }
            }
            DualListColumn::Included => {
                if !self.included.is_empty() && self.included_cursor + 1 < self.included.len() {
                    self.included_cursor += 1;
                }
            }
        }
    }

    /// Switch between Available and Included columns
    pub fn switch_column(&mut self) {
        self.active_column = match self.active_column {
            DualListColumn::Available => DualListColumn::Included,
            DualListColumn::Included => DualListColumn::Available,
        };
    }

    /// Number of visible body rows (max of available, included, and button
    /// count so all action buttons are always reachable)
    pub fn body_rows(&self) -> usize {
        let avail = self.available_items().len();
        let incl = self.included.len();
        // 5 = add + remove + separator + up + down
        avail.max(incl).max(5)
    }
}

/// Colors for the dual-list control
#[derive(Debug, Clone, Copy)]
pub struct DualListColors {
    /// Label color
    pub label: Color,
    /// Item text color
    pub text: Color,
    /// Column header color
    pub header: Color,
    /// Button color
    pub button: Color,
    /// Focused item highlight background
    pub focused_bg: Color,
    /// Focused item foreground
    pub focused_fg: Color,
    /// Disabled/inactive color
    pub disabled: Color,
}

impl Default for DualListColors {
    fn default() -> Self {
        Self {
            label: Color::White,
            text: Color::White,
            header: Color::Gray,
            button: Color::Cyan,
            focused_bg: Color::Cyan,
            focused_fg: Color::Black,
            disabled: Color::DarkGray,
        }
    }
}

impl DualListColors {
    /// Create colors from theme
    pub fn from_theme(theme: &crate::view::theme::Theme) -> Self {
        Self {
            label: theme.editor_fg,
            text: theme.editor_fg,
            header: theme.line_number_fg,
            button: theme.help_key_fg,
            focused_bg: theme.settings_selected_bg,
            focused_fg: theme.settings_selected_fg,
            disabled: theme.line_number_fg,
        }
    }
}

/// Hit area for a single row in one column
#[derive(Debug, Clone, Copy)]
pub struct DualListRowArea {
    pub area: Rect,
    pub index: usize,
}

/// Layout information returned after rendering for hit testing
#[derive(Debug, Clone, Default)]
pub struct DualListLayout {
    /// Available column row areas
    pub available_rows: Vec<DualListRowArea>,
    /// Included column row areas
    pub included_rows: Vec<DualListRowArea>,
    /// Add button area [>]
    pub add_button: Option<Rect>,
    /// Remove button area [<]
    pub remove_button: Option<Rect>,
    /// Move up button area
    pub move_up_button: Option<Rect>,
    /// Move down button area
    pub move_down_button: Option<Rect>,
    /// Full control area
    pub full_area: Rect,
}

/// Result of hit testing on a dual list
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DualListHit {
    AvailableRow(usize),
    IncludedRow(usize),
    AddButton,
    RemoveButton,
    MoveUpButton,
    MoveDownButton,
}

impl DualListLayout {
    /// Hit test a position
    pub fn hit_test(&self, x: u16, y: u16) -> Option<DualListHit> {
        // Check buttons first
        if let Some(ref area) = self.add_button {
            if point_in_rect(*area, x, y) {
                return Some(DualListHit::AddButton);
            }
        }
        if let Some(ref area) = self.remove_button {
            if point_in_rect(*area, x, y) {
                return Some(DualListHit::RemoveButton);
            }
        }
        if let Some(ref area) = self.move_up_button {
            if point_in_rect(*area, x, y) {
                return Some(DualListHit::MoveUpButton);
            }
        }
        if let Some(ref area) = self.move_down_button {
            if point_in_rect(*area, x, y) {
                return Some(DualListHit::MoveDownButton);
            }
        }

        // Check available rows
        for row in &self.available_rows {
            if point_in_rect(row.area, x, y) {
                return Some(DualListHit::AvailableRow(row.index));
            }
        }

        // Check included rows
        for row in &self.included_rows {
            if point_in_rect(row.area, x, y) {
                return Some(DualListHit::IncludedRow(row.index));
            }
        }

        None
    }
}

fn point_in_rect(rect: Rect, x: u16, y: u16) -> bool {
    x >= rect.x && x < rect.x + rect.width && y >= rect.y && y < rect.y + rect.height
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_options() -> Vec<(String, String)> {
        vec![
            ("filename".into(), "Filename".into()),
            ("cursor".into(), "Cursor".into()),
            ("cursor:compact".into(), "Cursor (compact)".into()),
            ("diagnostics".into(), "Diagnostics".into()),
            ("cursor_count".into(), "Cursor Count".into()),
            ("messages".into(), "Messages".into()),
            ("chord".into(), "Chord".into()),
        ]
    }

    #[test]
    fn test_available_items_excludes_included_and_excluded() {
        let state = DualListState::new("Test", test_options())
            .with_included(vec!["filename".into(), "cursor".into()])
            .with_excluded(vec!["chord".into()]);

        let available = state.available_items();
        assert_eq!(available.len(), 4); // 7 - 2 included - 1 excluded
        assert!(available
            .iter()
            .all(|(v, _)| v != "filename" && v != "cursor" && v != "chord"));
    }

    #[test]
    fn test_add_selected() {
        let mut state = DualListState::new("Test", test_options());
        state.available_cursor = 1; // "cursor"
        state.add_selected();

        assert_eq!(state.included, vec!["cursor"]);
        assert_eq!(state.available_items().len(), 6);
    }

    #[test]
    fn test_remove_selected() {
        let mut state = DualListState::new("Test", test_options())
            .with_included(vec!["filename".into(), "cursor".into()]);
        state.included_cursor = 0;
        state.remove_selected();

        assert_eq!(state.included, vec!["cursor"]);
    }

    #[test]
    fn test_move_up_down() {
        let mut state = DualListState::new("Test", test_options()).with_included(vec![
            "filename".into(),
            "cursor".into(),
            "diagnostics".into(),
        ]);

        state.active_column = DualListColumn::Included;
        state.included_cursor = 2;
        state.move_up();
        assert_eq!(state.included, vec!["filename", "diagnostics", "cursor"]);
        assert_eq!(state.included_cursor, 1);

        state.move_down();
        assert_eq!(state.included, vec!["filename", "cursor", "diagnostics"]);
        assert_eq!(state.included_cursor, 2);
    }

    #[test]
    fn test_switch_column() {
        let mut state = DualListState::new("Test", test_options());
        assert_eq!(state.active_column, DualListColumn::Available);

        state.switch_column();
        assert_eq!(state.active_column, DualListColumn::Included);

        state.switch_column();
        assert_eq!(state.active_column, DualListColumn::Available);
    }

    /// Helper: simulate Shift+Right (add item, focus follows to Included)
    fn shift_right(state: &mut DualListState) {
        assert_eq!(state.active_column, DualListColumn::Available);
        state.add_selected();
        state.active_column = DualListColumn::Included;
        state.included_cursor = state.included.len().saturating_sub(1);
    }

    /// Helper: simulate Shift+Left (remove item, focus follows to Available)
    fn shift_left(state: &mut DualListState) {
        assert_eq!(state.active_column, DualListColumn::Included);
        let value = state.included[state.included_cursor].clone();
        state.remove_selected();
        state.active_column = DualListColumn::Available;
        let avail = state.available_items();
        if let Some(pos) = avail.iter().position(|(v, _)| *v == value) {
            state.available_cursor = pos;
        }
    }

    /// Comprehensive test: Shift+arrows in both columns, back and forth
    #[test]
    fn test_shift_arrows_focus_follows_item() {
        let mut state = DualListState::new("Test", test_options());
        // Available: [filename, cursor, cursor:compact, diagnostics, cursor_count, messages, chord]
        // Included: []

        // --- Shift+Right from Available: add "cursor" (index 1) ---
        state.active_column = DualListColumn::Available;
        state.available_cursor = 1; // "cursor"
        shift_right(&mut state);
        assert_eq!(state.included, vec!["cursor"]);
        assert_eq!(state.active_column, DualListColumn::Included);
        assert_eq!(state.included_cursor, 0);

        // --- Shift+Right: add "filename" (now index 0 in Available) ---
        state.active_column = DualListColumn::Available;
        state.available_cursor = 0; // "filename"
        shift_right(&mut state);
        assert_eq!(state.included, vec!["cursor", "filename"]);
        assert_eq!(state.active_column, DualListColumn::Included);
        assert_eq!(state.included_cursor, 1); // appended at end

        // --- Shift+Up in Included: move "filename" up ---
        state.move_up();
        assert_eq!(state.included, vec!["filename", "cursor"]);
        assert_eq!(state.included_cursor, 0);

        // --- Shift+Up at top: no-op ---
        state.move_up();
        assert_eq!(state.included, vec!["filename", "cursor"]);
        assert_eq!(state.included_cursor, 0);

        // --- Shift+Down: move "filename" back down ---
        state.move_down();
        assert_eq!(state.included, vec!["cursor", "filename"]);
        assert_eq!(state.included_cursor, 1);

        // --- Shift+Down at bottom: no-op ---
        state.move_down();
        assert_eq!(state.included, vec!["cursor", "filename"]);
        assert_eq!(state.included_cursor, 1);

        // --- Shift+Left from Included: remove "filename", focus follows to Available ---
        shift_left(&mut state);
        assert_eq!(state.included, vec!["cursor"]);
        assert_eq!(state.active_column, DualListColumn::Available);
        // "filename" should be back in Available at its natural position (index 0)
        let avail = state.available_items();
        assert_eq!(avail[state.available_cursor].0, "filename");

        // --- Shift+Right again: add "filename" back ---
        shift_right(&mut state);
        assert_eq!(state.included, vec!["cursor", "filename"]);
        assert_eq!(state.active_column, DualListColumn::Included);
        assert_eq!(state.included_cursor, 1);

        // --- Shift+Left on first item: remove "cursor" ---
        state.included_cursor = 0;
        shift_left(&mut state);
        assert_eq!(state.included, vec!["filename"]);
        assert_eq!(state.active_column, DualListColumn::Available);
        let avail = state.available_items();
        assert_eq!(avail[state.available_cursor].0, "cursor");

        // --- Add more items to test reorder after round-trip ---
        // Currently: Included=[filename], Available has cursor back
        // Add "diagnostics" and "cursor" to Included
        state.available_cursor = 0; // "cursor" (back in Available)
        shift_right(&mut state);
        state.active_column = DualListColumn::Available;
        state.available_cursor = 1; // "diagnostics" (shifted after cursor was removed)
        shift_right(&mut state);
        assert_eq!(state.included, vec!["filename", "cursor", "diagnostics"]);
        assert_eq!(state.included_cursor, 2); // on "diagnostics"

        // --- Shift+Up twice: move "diagnostics" to top ---
        state.move_up();
        assert_eq!(state.included, vec!["filename", "diagnostics", "cursor"]);
        assert_eq!(state.included_cursor, 1);
        state.move_up();
        assert_eq!(state.included, vec!["diagnostics", "filename", "cursor"]);
        assert_eq!(state.included_cursor, 0);

        // --- Shift+Down to middle ---
        state.move_down();
        assert_eq!(state.included, vec!["filename", "diagnostics", "cursor"]);
        assert_eq!(state.included_cursor, 1);

        // --- Shift+Left from middle of Included: remove "diagnostics" ---
        shift_left(&mut state);
        assert_eq!(state.included, vec!["filename", "cursor"]);
        assert_eq!(state.active_column, DualListColumn::Available);
        let avail = state.available_items();
        assert_eq!(avail[state.available_cursor].0, "diagnostics");

        // --- Shift+Up/Down in Available column: no-op on both columns ---
        let avail_before: Vec<String> = state
            .available_items()
            .iter()
            .map(|(v, _)| v.clone())
            .collect();
        let included_before = state.included.clone();
        state.move_up();
        // Check after each operation, not just the pair (catches round-trip coincidence)
        assert_eq!(
            included_before, state.included,
            "Included should not change after move_up in Available column"
        );
        state.move_down();
        assert_eq!(
            included_before, state.included,
            "Included should not change after move_down in Available column"
        );
        let avail_after: Vec<String> = state
            .available_items()
            .iter()
            .map(|(v, _)| v.clone())
            .collect();
        assert_eq!(
            avail_before, avail_after,
            "Available order should not change"
        );
    }

    #[test]
    fn test_cursor_bounds() {
        let mut state = DualListState::new("Test", test_options());

        // Can't go below 0
        state.cursor_up();
        assert_eq!(state.available_cursor, 0);

        // Can go down to last item
        for _ in 0..10 {
            state.cursor_down();
        }
        assert_eq!(state.available_cursor, 6); // 7 items, max index 6

        // Cursor adjusts after add
        state.available_cursor = 6;
        state.add_selected();
        // After adding, there are 6 available, cursor should be clamped
        assert!(state.available_cursor < state.available_items().len());
    }
}
