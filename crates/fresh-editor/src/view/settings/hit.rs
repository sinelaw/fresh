//! What a press in the settings dialog resolved to.
//!
//! This module used to hold `SettingsLayout` as well: a record every painter
//! filed as it drew — the modal's rectangle, each control's chip, each visible
//! search row, two scrollbar tracks — that `hit_test` then compared a cell
//! against, in the order the painter happened to register them. Both halves
//! could be right on their own and still disagree, and the second one only
//! ever knew about what the first had drawn on the *previous* frame.
//!
//! Every surface in the dialog is a node now and answers its own press, so
//! nothing files rectangles. What survives is the vocabulary: `SettingsHit`
//! names an interaction rather than a cell, and both frontends speak it — the
//! TUI's nodes resolve to one through `Editor::settings_widget_hit`, the web's
//! `/settings` route sends the one it rendered, and `dispatch_settings_hit` is
//! the single body both run.

/// Result of a hit test on the settings UI
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SettingsHit {
    /// Click on a category (index)
    Category(usize),
    /// Click on a setting item (index)
    Item(usize),
    /// Click on a search result (absolute index into the state's
    /// `search_results`, not the on-screen slot)
    SearchResult(usize),
    /// Click on toggle control
    ControlToggle(usize),
    /// Click on number decrement button
    ControlDecrement(usize),
    /// Click on number increment button
    ControlIncrement(usize),
    /// Click on the value area between the brackets of a number control —
    /// should focus the item and enter inline editing mode.
    ControlNumberValue(usize),
    /// Click on dropdown button
    ControlDropdown(usize),
    /// Click on dropdown option (item_idx, option_idx)
    ControlDropdownOption(usize, usize),
    /// Click on text input
    ControlText(usize),
    /// Click on a text list's field (item_idx, row_idx); a row past the
    /// items is the add row's
    ControlTextListRow(usize, usize),
    /// Click on a text list item's `[x]` (item_idx, row_idx)
    ControlTextListRemove(usize, usize),
    /// Click on map row (item_idx, row_idx)
    ControlMapRow(usize, usize),
    /// Click on map add-new row (item_idx)
    ControlMapAddNew(usize),
    /// Click on inherit button (item_idx) - unset a nullable value
    ControlInherit(usize),
    /// Click on dual-list available row (item_idx, row_idx)
    ControlDualListAvailable(usize, usize),
    /// Click on dual-list included row (item_idx, row_idx)
    ControlDualListIncluded(usize, usize),
    /// Click on dual-list add button (item_idx)
    ControlDualListAdd(usize),
    /// Click on dual-list remove button (item_idx)
    ControlDualListRemove(usize),
    /// Click on dual-list move-up button (item_idx)
    ControlDualListMoveUp(usize),
    /// Click on dual-list move-down button (item_idx)
    ControlDualListMoveDown(usize),
    /// Click on layer button
    LayerButton,
    /// Click on edit config file button
    EditButton,
    /// Click on save button
    SaveButton,
    /// Click on cancel button
    CancelButton,
    /// Click on reset button
    ResetButton,
    /// Click on clear category button (for nullable categories)
    ClearCategoryButton,
}
