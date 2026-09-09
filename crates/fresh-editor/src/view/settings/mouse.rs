//! What a press in the settings dialog does, once a node has said where it
//! landed.
//!
//! Every surface of the dialog answers its own press as a `SettingsHit`
//! (the TUI's nodes through `Editor::settings_widget_hit`, the web's
//! `/settings` route directly), and `dispatch_settings_hit` is the one body
//! both run. The raw mouse handler that used to sit in front of it —
//! comparing a cell against rectangles the painter recorded, scrolling the
//! body by hand, swallowing what nothing matched — is gone: the box, its
//! windows and its scrim are the tree's.

use crate::app::Editor;

use super::state::FocusTarget;
use super::SettingsHit;
use crate::widgets::kinds::dual_list::DualOp;

impl Editor {
    /// Perform the action for a resolved `SettingsHit` — the one body both
    /// frontends run. The TUI's nodes resolve their own presses to a hit
    /// through `Editor::settings_widget_hit`; the web's `/settings` route
    /// sends the hit it rendered. So a click does the same thing in both.
    pub(crate) fn dispatch_settings_hit(&mut self, hit: SettingsHit, is_double_click: bool) {
        // If a dropdown is open and the click is outside it, cancel and stop.
        if let Some(ref mut state) = self.settings_state {
            if state.is_dropdown_open() {
                let is_click_on_open_dropdown = matches!(
                    hit,
                    SettingsHit::ControlDropdown(idx) | SettingsHit::ControlDropdownOption(idx, _)
                        if idx == state.selected_item
                );
                if !is_click_on_open_dropdown {
                    state.dropdown_cancel();
                    return;
                }
            }
        }

        match hit {
            // The wide layout's tree answers for itself now: its rows carry
            // `UiFact::SettingsCategory`, `SettingsCategorySection` and
            // `SettingsCategoryDisclosure` — the identity the row has, rather
            // than a rectangle a cell is compared against. This arm is the
            // **narrow** strip's, which is still painted.
            SettingsHit::Category(idx) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus_on(FocusTarget::Categories);
                    state.selected_category = idx;
                    state.selected_item = 0;
                    state.body_anchor.scroll_to(fresh_ui::Point::ZERO);
                    state.tree_cursor_section = None;
                    state.auto_expand_current_category();
                }
            }
            SettingsHit::SearchResult(idx) => {
                // Click on search result - select it and jump to it (same as Enter)
                if let Some(ref mut state) = self.settings_state {
                    state.selected_search_result = idx;
                    state.jump_to_search_result();
                }
            }
            SettingsHit::Item(idx) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus_on(FocusTarget::Card(idx));
                }
            }
            SettingsHit::ControlToggle(idx) | SettingsHit::ControlDropdown(idx) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus_on(FocusTarget::Card(idx));
                }
                self.settings_activate_current();
            }
            SettingsHit::ControlDropdownOption(idx, option_idx) => {
                // Click on a dropdown option - select it and close dropdown
                if let Some(ref mut state) = self.settings_state {
                    state.focus_on(FocusTarget::Card(idx));
                    state.select_dropdown_option(option_idx);
                }
            }
            SettingsHit::ControlDecrement(idx) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus_on(FocusTarget::Card(idx));
                }
                self.settings_decrement_current();
            }
            SettingsHit::ControlIncrement(idx) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus_on(FocusTarget::Card(idx));
                }
                self.settings_increment_current();
            }
            SettingsHit::ControlNumberValue(idx) => {
                // Click on the value between the brackets — focus the item
                // and open its draft (the kind's own answer to the press).
                if let Some(ref mut state) = self.settings_state {
                    state.focus_on(FocusTarget::Card(idx));
                    state.press_number_value();
                }
            }
            SettingsHit::ControlText(idx) => {
                // The caret lands where the press said, when it said
                // (`settings_widget_hit` follows with `position_text_cursor`).
                if let Some(ref mut state) = self.settings_state {
                    state.focus_on(FocusTarget::Card(idx));
                    state.press_text(None);
                }
            }
            // A press on a text list's field opens it: an item's, or the add
            // row's for a row past the items. (The caret lands where the
            // press said: `settings_widget_hit` follows with
            // `position_text_cursor`.)
            SettingsHit::ControlTextListRow(idx, row) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus_on(FocusTarget::Card(idx));
                    let n = state
                        .current_item()
                        .and_then(|i| i.control.add_row())
                        .unwrap_or(0);
                    state.edit_list_row((row < n).then_some(row));
                }
            }
            // A press on an item's `[x]` removes it.
            SettingsHit::ControlTextListRemove(idx, row) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus_on(FocusTarget::Card(idx));
                    state.remove_list_row(row);
                }
            }
            // A press on a row of a map's or an object array's list puts the
            // cursor there. An entry opens on a double press, the add row on
            // a single one (#604).
            SettingsHit::ControlMapRow(idx, row) => {
                let on_add = match self.settings_state {
                    Some(ref mut state) => {
                        state.focus_on(FocusTarget::Card(idx));
                        state.select_list_row(row);
                        state.current_item().and_then(|i| i.control.add_row()) == Some(row)
                    }
                    None => false,
                };
                if on_add || is_double_click {
                    self.settings_activate_current();
                }
            }
            SettingsHit::ControlMapAddNew(idx) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus_on(FocusTarget::Card(idx));
                    if let Some(add) = state.current_item().and_then(|i| i.control.add_row()) {
                        state.select_list_row(add);
                    }
                }
                self.settings_activate_current();
            }
            // A press on a dual list's cell selects that row and makes the
            // control live; the web's buttons beside its columns are the
            // kind's moves, on the row its cursor is on.
            SettingsHit::ControlDualListAvailable(idx, row) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus_on(FocusTarget::Card(idx));
                    state.press_dual_list(false, row);
                }
            }
            SettingsHit::ControlDualListIncluded(idx, row) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus_on(FocusTarget::Card(idx));
                    state.press_dual_list(true, row);
                }
            }
            SettingsHit::ControlDualListAdd(idx) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus_on(FocusTarget::Card(idx));
                    state.dual_list_op(DualOp::Carry(true));
                }
            }
            SettingsHit::ControlDualListRemove(idx) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus_on(FocusTarget::Card(idx));
                    state.dual_list_op(DualOp::Carry(false));
                }
            }
            SettingsHit::ControlDualListMoveUp(idx) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus_on(FocusTarget::Card(idx));
                    state.dual_list_op(DualOp::SwitchColumn(true));
                    state.dual_list_op(DualOp::Reorder(-1));
                }
            }
            SettingsHit::ControlDualListMoveDown(idx) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus_on(FocusTarget::Card(idx));
                    state.dual_list_op(DualOp::SwitchColumn(true));
                    state.dual_list_op(DualOp::Reorder(1));
                }
            }
            SettingsHit::ControlInherit(idx) => {
                // Click on [Inherit] button - set value to null (inherited)
                if let Some(ref mut state) = self.settings_state {
                    state.focus_on(FocusTarget::Card(idx));
                    state.set_current_to_null();
                }
            }
            SettingsHit::LayerButton => {
                if let Some(ref mut state) = self.settings_state {
                    state.cycle_target_layer();
                }
            }
            SettingsHit::SaveButton => self.close_settings(true),
            SettingsHit::CancelButton => {
                if let Some(ref mut state) = self.settings_state {
                    if state.has_changes() {
                        state.showing_confirm_dialog = true;
                        state.confirm_dialog_selection = 0;
                    } else {
                        state.visible = false;
                    }
                }
            }
            SettingsHit::ResetButton => {
                if let Some(ref mut state) = self.settings_state {
                    state.reset_current_to_default();
                }
            }
            SettingsHit::ClearCategoryButton => {
                if let Some(ref mut state) = self.settings_state {
                    state.clear_current_category();
                }
            }
            SettingsHit::EditButton => {
                // Open config file for the selected layer
                if let Some(ref state) = self.settings_state {
                    let layer = state.target_layer;
                    // Best-effort: open may fail if file doesn't exist yet
                    #[allow(clippy::let_underscore_must_use)]
                    let _ = self.open_config_file(layer);
                }
            }
        }
    }

    /// Which of the dialog's buttons index `i` is.
    ///
    /// The row is `[Save] [Cancel]` plus `[Delete …]` when the entry can be
    /// removed — the same rule `entry::Dialog` builds the labels from, read
    /// back here rather than a fourth copy of the row's arithmetic.
    pub(crate) fn entry_button_kind(
        d: &super::entry_dialog::EntryDialogState,
        i: usize,
    ) -> &'static str {
        match i {
            0 => "save",
            1 => "cancel",
            _ if !d.is_new && !d.no_delete => "delete",
            _ => "cancel",
        }
    }

    /// A press on a field's `[Reset]` / `[Inherit]`.
    pub(crate) fn entry_dialog_field_action(&mut self, idx: usize, action: usize) {
        use super::entry_dialog::FieldAction;
        let Some(dialog) = self
            .settings_state
            .as_mut()
            .and_then(|s| s.entry_dialog_mut())
        else {
            return;
        };
        let Some((which, _)) = dialog.field_action_buttons(idx).into_iter().nth(action) else {
            return;
        };
        let _ = match which {
            FieldAction::Reset => dialog.reset_field(idx),
            FieldAction::Inherit => dialog.inherit_field(idx),
        };
        dialog.focus_on_buttons = false;
        dialog.field_button_focus = None;
        dialog.selected_item = idx;
    }

    /// A press on a described entry-dialog field.
    ///
    /// **The hit says which field, which of its rows, and where in that row.**
    /// All three used to be recovered from the pointer's cell: the field by
    /// walking every item's height, the row by subtracting the item's start,
    /// and the column by comparing against a rectangle the renderer had drawn
    /// — with the trailing `[x]` guessed at, because the field width that
    /// would have placed it exactly was not carried this far. It is carried
    /// now, by the row that has it.
    pub(crate) fn settings_entry_widget_hit(
        &mut self,
        hit: &crate::widgets::WidgetEvent,
        byte: Option<usize>,
        clicks: u8,
    ) {
        let key = hit.owner_key.as_deref().unwrap_or(hit.widget_key.as_str());
        let (path, part) = match key.split_once("::") {
            Some((p, s)) => (p, s),
            None => (key, ""),
        };
        let Some(dialog) = self.settings_state.as_ref().and_then(|s| s.entry_dialog()) else {
            return;
        };
        let Some(idx) = dialog.items.iter().position(|i| i.path == path) else {
            return;
        };
        if dialog.items[idx].read_only {
            return;
        }
        let row = || {
            hit.payload
                .get("index")
                .and_then(|v| v.as_u64())
                .unwrap_or(0) as usize
        };
        let row_count = dialog.items[idx].control.list_row_count();
        let add = dialog.items[idx].control.add_row();
        match hit.widget_kind {
            // A row of a map's or an object array's list.
            "list" if row_count > 0 => {
                let r = row();
                let entry = (Some(r) != add).then_some(r);
                self.entry_composite_press(idx, entry, clicks >= 2);
                return;
            }
            // A field of a text list, by its row key.
            "text" => {
                if let Some(row) = super::live::text_list::row_of(key) {
                    let caret = byte.and_then(|b| crate::widgets::value_byte_from_hit(hit, b));
                    self.entry_text_list_press(idx, row, caret);
                    return;
                }
            }
            // An item's `[x]`.
            "button" => {
                if let Some(i) = part.strip_prefix("remove::").and_then(|i| i.parse().ok()) {
                    self.entry_text_list_remove(idx, i);
                }
                return;
            }
            _ => {}
        }
        // A press on a text field also says where in the value the caret goes.
        //
        // **The press reports its byte and the hit carries the layout.** This
        // used to rebuild the control's spec, render it at a width read back
        // off the tree, and measure the row that came out — with a comment
        // saying both the label column and the width had to match what the
        // tree drew, "a byte resolved at any other pair is not the byte under
        // the pointer". That was true of a *column*: turning one into a byte
        // means knowing where every grapheme landed, which means laying the
        // text out. The library already did, and now says so.
        let caret = match hit.widget_kind == "text" {
            false => None,
            true => byte.and_then(|b| crate::widgets::value_byte_from_hit(hit, b)),
        };
        self.entry_dialog_select_item(idx);
        if let Some(byte) = caret {
            if let Some(dialog) = self
                .settings_state
                .as_mut()
                .and_then(|s| s.entry_dialog_mut())
            {
                dialog.position_text_cursor(byte);
            }
        }
    }

    /// Select an entry-dialog field by index and begin editing it.
    ///
    /// **Two callers now.** It was the web `/settings` route's alone — the
    /// TUI reached the same behaviour through
    /// `handle_entry_dialog_item_click`, which spent most of its length
    /// working out *which* field the pointer was on. The field says so
    /// itself now, so both frontends end here.
    pub(crate) fn entry_dialog_select_item(&mut self, idx: usize) {
        if let Some(state) = self.settings_state.as_mut() {
            if let Some(dialog) = state.entry_dialog_mut() {
                if idx >= dialog.items.len() || dialog.items[idx].read_only {
                    return;
                }
                // **Clicking away from a field commits it.** The live field
                // is the one the store's focus key names, and `selected_item`
                // the one the dialog's keys apply to; moving the selection
                // with a field still live would point the next Esc at the
                // field the user had *left*, reverting the value they had
                // just typed. Enter and Tab already commit; a press on
                // another field is the third way to leave one.
                if dialog.is_editing() && dialog.selected_item != idx {
                    dialog.stop_editing();
                }
                dialog.focus_on_buttons = false;
                dialog.selected_item = idx;
                if !dialog.is_editing() {
                    dialog.start_editing();
                }
            }
        }
    }

    /// Activate an entry-dialog button by semantic name ("save" | "cancel" |
    /// "delete"). Routing by name rather than index keeps the web and TUI in
    /// agreement even though they lay the buttons out in a different order —
    /// which is now what the TUI's own button nodes do too, through
    /// [`Self::entry_button_kind`].
    pub(crate) fn entry_dialog_activate_button(&mut self, kind: &str) {
        let Some(state) = self.settings_state.as_mut() else {
            return;
        };
        let has_delete = state
            .entry_dialog()
            .map(|d| !d.is_new && !d.no_delete)
            .unwrap_or(false);
        match kind {
            "save" => state.save_entry_dialog(),
            "delete" if has_delete => state.request_entry_delete_confirm(),
            _ => state.close_entry_dialog(),
        }
    }

    // **The body's scrollbar is the window's own.** Its track was a
    // rectangle the painter filed and two handlers compared a cell against;
    // the `viewport` the cards live in draws its bar in its own gutter and
    // the framework maps a press or a drag on it to an offset.

    // **The results' scrollbar is its window's own too.** Its track was a
    // second filed rectangle, and a press and a drag each converted a row
    // inside it to a ratio; the results are a `List` in a `viewport` now, and
    // the framework maps both to an offset.

    // **Everything the entry-dialog stack hit-tested by geometry is gone.**
    // `EntryDialogLayout` recomputed the box, its inner band, its button row
    // and its scrollbar column from the modal area on every event;
    // `entry_dialog_update_hover` and `handle_entry_dialog_click` then walked
    // the fields a second and third time to find which one a cell was on —
    // and the hover walk omitted the section headers the renderer drew, so it
    // had been two rows out per section. The stack is `view::shell::entry`
    // now: a layer per level, its fields in a `viewport`, and each field,
    // button and per-field action answering its own press.

    /// A press on a row of an entry-dialog map or object array: the field
    /// is selected, its list's cursor put on the row, and the nested dialog
    /// opened when the press says to — the add row on a single press, an
    /// entry on a double one (#604).
    ///
    /// `entry` is the committed row the pointer was on, or `None` for the
    /// trailing `[+] Add new` row. `double` is the press's own doubleness,
    /// carried on the fact by the row that saw it
    /// (`UiFact::WidgetHit::clicks`).
    pub(crate) fn entry_composite_press(
        &mut self,
        item_idx: usize,
        entry: Option<usize>,
        double: bool,
    ) {
        let Some(state) = self.settings_state.as_mut() else {
            return;
        };
        let Some(dialog) = state.entry_dialog_mut() else {
            return;
        };
        // The same rule the text path follows: leaving a field commits it.
        if dialog.is_editing() && dialog.selected_item != item_idx {
            dialog.stop_editing();
        }
        let Some(item) = dialog.items.get(item_idx) else {
            return;
        };
        let Some(add) = item.control.add_row() else {
            return;
        };
        let row = entry.filter(|r| *r < add).unwrap_or(add);
        dialog.focus_on_buttons = false;
        dialog.field_button_focus = None;
        dialog.selected_item = item_idx;
        dialog.select_list_row(row);
        if row == add || double {
            state.open_nested_entry_dialog();
        }
    }

    /// A press on a field of an entry-dialog text list: the field opens —
    /// an item's, or the add row's — with the caret where the press said.
    pub(crate) fn entry_text_list_press(
        &mut self,
        item_idx: usize,
        row: Option<usize>,
        byte: Option<usize>,
    ) {
        let Some(state) = self.settings_state.as_mut() else {
            return;
        };
        let Some(dialog) = state.entry_dialog_mut() else {
            return;
        };
        if dialog.is_editing() && dialog.selected_item != item_idx {
            dialog.stop_editing();
        }
        dialog.focus_on_buttons = false;
        dialog.field_button_focus = None;
        dialog.selected_item = item_idx;
        dialog.edit_list_row(row);
        if let Some(byte) = byte {
            dialog.position_text_cursor(byte);
        }
    }

    /// A press on an item's `[x]` in an entry-dialog text list removes it.
    pub(crate) fn entry_text_list_remove(&mut self, item_idx: usize, row: usize) {
        let Some(state) = self.settings_state.as_mut() else {
            return;
        };
        let Some(dialog) = state.entry_dialog_mut() else {
            return;
        };
        if dialog.is_editing() && dialog.selected_item != item_idx {
            dialog.stop_editing();
        }
        dialog.focus_on_buttons = false;
        dialog.field_button_focus = None;
        dialog.selected_item = item_idx;
        dialog.remove_list_row(row);
    }

    pub(crate) fn save_settings_and_close(&mut self) {
        self.save_settings();
        if let Some(ref mut state) = self.settings_state {
            state.visible = false;
            state.showing_confirm_dialog = false;
        }
    }

    pub(crate) fn discard_settings_and_close(&mut self) {
        if let Some(ref mut state) = self.settings_state {
            state.visible = false;
            state.showing_confirm_dialog = false;
        }
    }
}
