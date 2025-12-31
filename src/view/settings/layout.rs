//! Settings layout for hit testing
//!
//! Tracks the layout of rendered settings UI elements for mouse interaction.

use super::render::ControlLayoutInfo;
use ratatui::layout::Rect;

/// Layout information for the entire settings UI
#[derive(Debug, Clone, Default)]
pub struct SettingsLayout {
    /// The modal area
    pub modal_area: Rect,
    /// Category list items (index, area)
    pub categories: Vec<(usize, Rect)>,
    /// Setting items (index, path, area, control_layout)
    pub items: Vec<ItemLayout>,
    /// Search result items (page_index, item_index, area)
    pub search_results: Vec<SearchResultLayout>,
    /// Layer button area
    pub layer_button: Option<Rect>,
    /// Save button area
    pub save_button: Option<Rect>,
    /// Cancel button area
    pub cancel_button: Option<Rect>,
    /// Reset button area
    pub reset_button: Option<Rect>,
    /// Settings panel area (for scroll hit testing)
    pub settings_panel_area: Option<Rect>,
    /// Scrollbar area (for drag detection)
    pub scrollbar_area: Option<Rect>,
}

/// Layout info for a search result
#[derive(Debug, Clone)]
pub struct SearchResultLayout {
    /// Page index (category)
    pub page_index: usize,
    /// Item index within the page
    pub item_index: usize,
    /// Full area for this result
    pub area: Rect,
}

/// Layout info for a setting item
#[derive(Debug, Clone)]
pub struct ItemLayout {
    /// Item index within current page
    pub index: usize,
    /// JSON path for this setting
    pub path: String,
    /// Full item area (for selection)
    pub area: Rect,
    /// Control-specific layout info
    pub control: ControlLayoutInfo,
}

impl SettingsLayout {
    /// Create a new layout for the given modal area
    pub fn new(modal_area: Rect) -> Self {
        Self {
            modal_area,
            categories: Vec::new(),
            items: Vec::new(),
            search_results: Vec::new(),
            layer_button: None,
            save_button: None,
            cancel_button: None,
            reset_button: None,
            settings_panel_area: None,
            scrollbar_area: None,
        }
    }

    /// Add a category to the layout
    pub fn add_category(&mut self, index: usize, area: Rect) {
        self.categories.push((index, area));
    }

    /// Add a setting item to the layout
    pub fn add_item(&mut self, index: usize, path: String, area: Rect, control: ControlLayoutInfo) {
        self.items.push(ItemLayout {
            index,
            path,
            area,
            control,
        });
    }

    /// Add a search result to the layout
    pub fn add_search_result(&mut self, page_index: usize, item_index: usize, area: Rect) {
        self.search_results.push(SearchResultLayout {
            page_index,
            item_index,
            area,
        });
    }

    /// Hit test a position and return what was clicked
    pub fn hit_test(&self, x: u16, y: u16) -> Option<SettingsHit> {
        // Check if outside modal
        if !self.contains(self.modal_area, x, y) {
            return Some(SettingsHit::Outside);
        }

        // Check footer buttons
        if let Some(ref layer) = self.layer_button {
            if self.contains(*layer, x, y) {
                return Some(SettingsHit::LayerButton);
            }
        }
        if let Some(ref save) = self.save_button {
            if self.contains(*save, x, y) {
                return Some(SettingsHit::SaveButton);
            }
        }
        if let Some(ref cancel) = self.cancel_button {
            if self.contains(*cancel, x, y) {
                return Some(SettingsHit::CancelButton);
            }
        }
        if let Some(ref reset) = self.reset_button {
            if self.contains(*reset, x, y) {
                return Some(SettingsHit::ResetButton);
            }
        }

        // Check categories
        for (index, area) in &self.categories {
            if self.contains(*area, x, y) {
                return Some(SettingsHit::Category(*index));
            }
        }

        // Check setting items
        for item in &self.items {
            if self.contains(item.area, x, y) {
                // Check specific control areas
                match &item.control {
                    ControlLayoutInfo::Toggle(toggle_area) => {
                        if self.contains(*toggle_area, x, y) {
                            return Some(SettingsHit::ControlToggle(item.index));
                        }
                    }
                    ControlLayoutInfo::Number {
                        decrement,
                        increment,
                        value,
                    } => {
                        if self.contains(*decrement, x, y) {
                            return Some(SettingsHit::ControlDecrement(item.index));
                        }
                        if self.contains(*increment, x, y) {
                            return Some(SettingsHit::ControlIncrement(item.index));
                        }
                        if self.contains(*value, x, y) {
                            return Some(SettingsHit::Item(item.index));
                        }
                    }
                    ControlLayoutInfo::Dropdown(area) => {
                        if self.contains(*area, x, y) {
                            return Some(SettingsHit::ControlDropdown(item.index));
                        }
                    }
                    ControlLayoutInfo::Text(area) => {
                        if self.contains(*area, x, y) {
                            return Some(SettingsHit::ControlText(item.index));
                        }
                    }
                    ControlLayoutInfo::TextList { rows } => {
                        for (row_idx, row_area) in rows.iter().enumerate() {
                            if self.contains(*row_area, x, y) {
                                return Some(SettingsHit::ControlTextListRow(item.index, row_idx));
                            }
                        }
                    }
                    ControlLayoutInfo::Map { entry_rows } => {
                        for (row_idx, row_area) in entry_rows.iter().enumerate() {
                            if self.contains(*row_area, x, y) {
                                return Some(SettingsHit::ControlMapRow(item.index, row_idx));
                            }
                        }
                    }
                    ControlLayoutInfo::ObjectArray { entry_rows } => {
                        for (row_idx, row_area) in entry_rows.iter().enumerate() {
                            if self.contains(*row_area, x, y) {
                                return Some(SettingsHit::ControlMapRow(item.index, row_idx));
                            }
                        }
                    }
                    ControlLayoutInfo::Json { edit_area } => {
                        if self.contains(*edit_area, x, y) {
                            return Some(SettingsHit::ControlText(item.index));
                        }
                    }
                    ControlLayoutInfo::Complex => {}
                }

                return Some(SettingsHit::Item(item.index));
            }
        }

        // Check scrollbar area (for drag detection)
        if let Some(ref scrollbar) = self.scrollbar_area {
            if self.contains(*scrollbar, x, y) {
                return Some(SettingsHit::Scrollbar);
            }
        }

        // Check settings panel area (for scroll wheel)
        if let Some(ref panel) = self.settings_panel_area {
            if self.contains(*panel, x, y) {
                return Some(SettingsHit::SettingsPanel);
            }
        }

        Some(SettingsHit::Background)
    }

    /// Check if a point is within a rectangle
    fn contains(&self, rect: Rect, x: u16, y: u16) -> bool {
        x >= rect.x && x < rect.x + rect.width && y >= rect.y && y < rect.y + rect.height
    }
}

/// Result of a hit test on the settings UI
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SettingsHit {
    /// Click outside the modal
    Outside,
    /// Click on modal background
    Background,
    /// Click on a category (index)
    Category(usize),
    /// Click on a setting item (index)
    Item(usize),
    /// Click on toggle control
    ControlToggle(usize),
    /// Click on number decrement button
    ControlDecrement(usize),
    /// Click on number increment button
    ControlIncrement(usize),
    /// Click on dropdown button
    ControlDropdown(usize),
    /// Click on text input
    ControlText(usize),
    /// Click on text list row (item_idx, row_idx)
    ControlTextListRow(usize, usize),
    /// Click on map row (item_idx, row_idx)
    ControlMapRow(usize, usize),
    /// Click on layer button
    LayerButton,
    /// Click on save button
    SaveButton,
    /// Click on cancel button
    CancelButton,
    /// Click on reset button
    ResetButton,
    /// Click on settings panel scrollbar
    Scrollbar,
    /// Click on settings panel (scrollable area)
    SettingsPanel,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_layout_creation() {
        let modal = Rect::new(10, 5, 80, 30);
        let mut layout = SettingsLayout::new(modal);

        layout.add_category(0, Rect::new(11, 6, 20, 1));
        layout.add_category(1, Rect::new(11, 7, 20, 1));

        assert_eq!(layout.categories.len(), 2);
    }

    #[test]
    fn test_hit_test_outside() {
        let modal = Rect::new(10, 5, 80, 30);
        let layout = SettingsLayout::new(modal);

        assert_eq!(layout.hit_test(0, 0), Some(SettingsHit::Outside));
        assert_eq!(layout.hit_test(5, 5), Some(SettingsHit::Outside));
    }

    #[test]
    fn test_hit_test_category() {
        let modal = Rect::new(10, 5, 80, 30);
        let mut layout = SettingsLayout::new(modal);

        layout.add_category(0, Rect::new(11, 6, 20, 1));
        layout.add_category(1, Rect::new(11, 7, 20, 1));

        assert_eq!(layout.hit_test(15, 6), Some(SettingsHit::Category(0)));
        assert_eq!(layout.hit_test(15, 7), Some(SettingsHit::Category(1)));
    }

    #[test]
    fn test_hit_test_buttons() {
        let modal = Rect::new(10, 5, 80, 30);
        let mut layout = SettingsLayout::new(modal);

        layout.save_button = Some(Rect::new(60, 32, 8, 1));
        layout.cancel_button = Some(Rect::new(70, 32, 10, 1));

        assert_eq!(layout.hit_test(62, 32), Some(SettingsHit::SaveButton));
        assert_eq!(layout.hit_test(75, 32), Some(SettingsHit::CancelButton));
    }

    #[test]
    fn test_hit_test_item_with_toggle() {
        let modal = Rect::new(10, 5, 80, 30);
        let mut layout = SettingsLayout::new(modal);

        layout.add_item(
            0,
            "/test".to_string(),
            Rect::new(35, 10, 50, 2),
            ControlLayoutInfo::Toggle(Rect::new(37, 11, 15, 1)),
        );

        // Click on toggle control
        assert_eq!(layout.hit_test(40, 11), Some(SettingsHit::ControlToggle(0)));

        // Click on item but not on toggle
        assert_eq!(layout.hit_test(35, 10), Some(SettingsHit::Item(0)));
    }
}
