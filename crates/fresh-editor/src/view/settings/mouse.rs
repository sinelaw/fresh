//! Mouse input handling for the Settings dialog.
//!
//! This module contains all mouse event handling for the settings modal,
//! including clicks, scrolling, and drag operations.

use crate::app::Editor;
use anyhow::Result as AnyhowResult;

use super::items::SettingControl;
use super::{FocusPanel, SettingsHit};
use crate::view::controls::DualListColumn;

impl Editor {
    /// Handle mouse events when settings modal is open.
    pub(crate) fn handle_settings_mouse(
        &mut self,
        mouse_event: crossterm::event::MouseEvent,
    ) -> AnyhowResult<bool> {
        use crossterm::event::{MouseButton, MouseEventKind};

        let col = mouse_event.column;
        let row = mouse_event.row;

        // When help overlay is open, consume all mouse events
        if let Some(ref state) = self.settings_state {
            if state.showing_help {
                return Ok(false);
            }
        }

        // **The confirm prompt answers for itself.** Its buttons are nodes and
        // arrive as `UiFact::SettingsDialog`; the hover likewise. What was
        // here re-derived the painter's layout to find which button a cell was
        // on — `get_confirm_dialog_button_at` carried "same as in
        // `render_confirm_dialog`" beside the copy — and the two could drift
        // without either one being wrong on its own. A press that reaches here
        // while the prompt is up landed on its scrim.
        if self
            .settings_state
            .as_ref()
            .is_some_and(|s| s.showing_confirm_dialog || s.showing_reset_dialog)
        {
            return Ok(false);
        }

        // **The entry-dialog stack answers for itself.** Its fields, buttons
        // and per-field actions are nodes, and its window is a `viewport` —
        // so the wheel, the scrollbar drag and the hover that were handled
        // here are the framework's. A press that reaches this far while the
        // stack is up landed on its scrim.
        if self
            .settings_state
            .as_ref()
            .is_some_and(|s| s.showing_entry_dialog())
        {
            return Ok(false);
        }

        match mouse_event.kind {
            // **Every surface reports its own hover.** A card, a category
            // row, a footer button and a search result each say when the
            // pointer enters and leaves them; this arm compared the cell
            // against every recorded rectangle in the dialog on every move.
            MouseEventKind::Moved => {
                if let Some(ref mut state) = self.settings_state {
                    state.hover_position = Some((col, row));
                }
                return Ok(false);
            }
            MouseEventKind::ScrollUp => {
                // If a dropdown is open, forward scroll to it
                if let Some(ref mut state) = self.settings_state {
                    if state.is_dropdown_open() {
                        state.dropdown_scroll(-3);
                        return Ok(true);
                    }
                }
                // A wheel over the category tree is the tree's: it is a
                // `viewport` now, so its window moves without the selection
                // moving with it — which is what this arm and
                // `over_categories_panel`'s recorded rectangle did between
                // them. The tree's layer answers first, so nothing reaches
                // here from over it.
                return Ok(self.settings_scroll_up(3));
            }
            MouseEventKind::ScrollDown => {
                // If a dropdown is open, forward scroll to it
                if let Some(ref mut state) = self.settings_state {
                    if state.is_dropdown_open() {
                        state.dropdown_scroll(3);
                        return Ok(true);
                    }
                }
                return Ok(self.settings_scroll_down(3));
            }
            // Both bars in the dialog — the body's and the results' — are
            // their windows' own, and the framework maps a press or a drag on
            // one to an offset.
            MouseEventKind::Drag(MouseButton::Left) => return Ok(false),
            // **Every surface in the dialog answers its own press**, and the
            // tree's layers answer before this handler runs. What reached
            // here was `SettingsLayout::hit_test`: the modal's rectangle, each
            // control's chip, each visible search row and two scrollbar
            // tracks, compared against the cell in the order the painter had
            // registered them. A press that gets this far landed on the box
            // or its scrim, which is `Background` — and `Background` did
            // nothing. It is still swallowed, because the dialog is modal and
            // the editor behind it must not see the click.
            MouseEventKind::Down(MouseButton::Left) => Ok(true),
            _ => Ok(false),
        }
    }

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
                    state.focus.set(FocusPanel::Categories);
                    state.selected_category = idx;
                    state.selected_item = 0;
                    state.body_anchor.scroll_to(fresh_ui::Point::ZERO);
                    state.sub_focus = None;
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
                    state.focus.set(FocusPanel::Settings);
                    state.selected_item = idx;
                }
            }
            SettingsHit::ControlToggle(idx) | SettingsHit::ControlDropdown(idx) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus.set(FocusPanel::Settings);
                    state.selected_item = idx;
                }
                self.settings_activate_current();
            }
            SettingsHit::ControlDropdownOption(idx, option_idx) => {
                // Click on a dropdown option - select it and close dropdown
                if let Some(ref mut state) = self.settings_state {
                    state.focus.set(FocusPanel::Settings);
                    state.selected_item = idx;
                    state.dropdown_select(option_idx);
                }
            }
            SettingsHit::ControlDecrement(idx) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus.set(FocusPanel::Settings);
                    state.selected_item = idx;
                }
                self.settings_decrement_current();
            }
            SettingsHit::ControlIncrement(idx) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus.set(FocusPanel::Settings);
                    state.selected_item = idx;
                }
                self.settings_increment_current();
            }
            SettingsHit::ControlNumberValue(idx) => {
                // Click on the value between the brackets — focus the item
                // and enter inline editing mode (matches the Enter-key flow).
                if let Some(ref mut state) = self.settings_state {
                    state.focus.set(FocusPanel::Settings);
                    state.selected_item = idx;
                    state.start_number_editing();
                }
            }
            SettingsHit::ControlText(idx) | SettingsHit::ControlTextListRow(idx, _) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus.set(FocusPanel::Settings);
                    state.selected_item = idx;
                    state.start_editing();
                }
            }
            SettingsHit::ControlMapRow(idx, row_idx) => {
                let is_add_new_row = if let Some(ref mut state) = self.settings_state {
                    state.focus.set(FocusPanel::Settings);
                    state.selected_item = idx;

                    let mut is_add_new = false;
                    if let Some(page) = state.pages.get_mut(state.selected_category) {
                        if let Some(item) = page.items.get_mut(idx) {
                            if let SettingControl::Map(map_state) = &mut item.control {
                                is_add_new = row_idx >= map_state.entries.len();
                                map_state.focused_entry = if row_idx < map_state.entries.len() {
                                    Some(row_idx)
                                } else {
                                    None
                                };
                            }
                        }
                    }
                    is_add_new
                } else {
                    false
                };
                // "Add new" row activates on single click (#604), entries require double-click
                if is_add_new_row || is_double_click {
                    self.settings_activate_current();
                }
            }
            SettingsHit::ControlMapAddNew(idx) => {
                // Click on map add-new row - focus it and activate immediately
                if let Some(ref mut state) = self.settings_state {
                    state.focus.set(FocusPanel::Settings);
                    state.selected_item = idx;

                    if let Some(page) = state.pages.get_mut(state.selected_category) {
                        if let Some(item) = page.items.get_mut(idx) {
                            if let SettingControl::Map(map_state) = &mut item.control {
                                map_state.focused_entry = None; // Focus add-new row
                            }
                        }
                    }
                }
                // Single click on add-new activates immediately
                self.settings_activate_current();
            }
            SettingsHit::ControlDualListAvailable(idx, row) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus.set(FocusPanel::Settings);
                    state.selected_item = idx;
                    state.with_dual_list_mut(idx, |dl| {
                        dl.active_column = DualListColumn::Available;
                        dl.available_cursor = row;
                    });
                    state.start_editing();
                }
            }
            SettingsHit::ControlDualListIncluded(idx, row) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus.set(FocusPanel::Settings);
                    state.selected_item = idx;
                    state.with_dual_list_mut(idx, |dl| {
                        dl.active_column = DualListColumn::Included;
                        dl.included_cursor = row;
                    });
                    state.start_editing();
                }
            }
            SettingsHit::ControlDualListAdd(idx) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus.set(FocusPanel::Settings);
                    state.selected_item = idx;
                    state.with_dual_list_mut(idx, |dl| dl.add_selected());
                    state.on_value_changed();
                    state.refresh_dual_list_sibling();
                }
            }
            SettingsHit::ControlDualListRemove(idx) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus.set(FocusPanel::Settings);
                    state.selected_item = idx;
                    state.with_dual_list_mut(idx, |dl| dl.remove_selected());
                    state.on_value_changed();
                    state.refresh_dual_list_sibling();
                }
            }
            SettingsHit::ControlDualListMoveUp(idx) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus.set(FocusPanel::Settings);
                    state.selected_item = idx;
                    state.with_dual_list_mut(idx, |dl| dl.move_up());
                    state.on_value_changed();
                }
            }
            SettingsHit::ControlDualListMoveDown(idx) => {
                if let Some(ref mut state) = self.settings_state {
                    state.focus.set(FocusPanel::Settings);
                    state.selected_item = idx;
                    state.with_dual_list_mut(idx, |dl| dl.move_down());
                    state.on_value_changed();
                }
            }
            SettingsHit::ControlInherit(idx) => {
                // Click on [Inherit] button - set value to null (inherited)
                if let Some(ref mut state) = self.settings_state {
                    state.focus.set(FocusPanel::Settings);
                    state.selected_item = idx;
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
        dialog.update_focus_states();
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
        hit: &crate::widgets::HitArea,
        at: Option<u16>,
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
        // A control's rows are numbered from its label, which is row zero —
        // the same numbering `ScrollItem::focus_regions` handed out.
        let sub_row = match (hit.widget_kind, part) {
            // The sentinel is one past the last committed row.
            ("list", "add") => match &dialog.items[idx].control {
                SettingControl::TextList(t) => t.items.len() + 1,
                SettingControl::Map(m) => m.entries.len() + 1,
                SettingControl::ObjectArray(a) => a.bindings.len() + 1,
                _ => 1,
            },
            ("list", _) => row() + 1,
            _ => 0,
        };
        if matches!(dialog.items[idx].control, SettingControl::TextList(_)) && sub_row > 0 {
            if let Err(e) = self.entry_text_list_press(idx, sub_row, at.unwrap_or(0)) {
                tracing::warn!("settings entry text-list press failed: {e}");
            }
            return;
        }
        // A press on a text field also says where in the value the caret goes.
        let caret = match hit.widget_kind == "text" {
            false => None,
            true => at.and_then(|col| {
                let item = &dialog.items[idx];
                let spec = super::widget_map::setting_control_to_widget_aligned(
                    &item.path,
                    &item.control,
                    None,
                );
                let out = crate::widgets::render_spec_no_autofocus(
                    &spec,
                    crate::view::shell::widgets::no_state(),
                    "",
                    u32::MAX,
                );
                crate::widgets::WidgetTextClickGeometry::from_render_output(&out, 0)
                    .map(|g| g.value_byte_in_cell(hit.byte_start, col))
            }),
        };
        self.entry_dialog_select_item(idx);
        if let Some(byte) = caret {
            if let Some(dialog) = self
                .settings_state
                .as_mut()
                .and_then(|s| s.entry_dialog_mut())
            {
                if let Some(SettingControl::Text(ts)) =
                    dialog.items.get_mut(idx).map(|it| &mut it.control)
                {
                    ts.set_cursor_from_flat(byte);
                }
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
                dialog.focus_on_buttons = false;
                dialog.selected_item = idx;
                dialog.update_focus_states();
                if !dialog.editing_text {
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

    fn settings_scroll_up(&mut self, delta: usize) -> bool {
        self.settings_state
            .as_mut()
            .map(|state| state.scroll_up(delta))
            .unwrap_or(false)
    }

    fn settings_scroll_down(&mut self, delta: usize) -> bool {
        self.settings_state
            .as_mut()
            .map(|state| state.scroll_down(delta))
            .unwrap_or(false)
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

    /// A press on one row of a `TextList` inside an entry dialog.
    ///
    /// `col` is the column *within that row*, which the row's own hit
    /// reports. The trailing `[x]` / `[+]` is then a question for the module
    /// that wrote the row — [`super::widget_map::text_list_target`] — rather
    /// than for a guess made from the dialog's outer width.
    pub(crate) fn entry_text_list_press(
        &mut self,
        item_idx: usize,
        sub_row: usize,
        col: u16,
    ) -> AnyhowResult<bool> {
        let Some(ref mut state) = self.settings_state else {
            return Ok(false);
        };
        let Some(dialog) = state.entry_dialog_mut() else {
            return Ok(false);
        };
        let item = match dialog.items.get_mut(item_idx) {
            Some(it) => it,
            None => return Ok(false),
        };
        let tl = match &mut item.control {
            SettingControl::TextList(s) => s,
            _ => return Ok(false),
        };

        // sub_row 0 is the label row.
        if sub_row == 0 {
            // Focus the control on a generic label click; user
            // can keyboard from there.
            dialog.focus_on_buttons = false;
            dialog.selected_item = item_idx;
            dialog.update_focus_states();
            return Ok(true);
        }

        let n_items = tl.items.len();
        // Compute which TextList row was clicked: existing item rows
        // are sub_row 1..=n_items, the add-new row is sub_row n_items+1.
        let on_add_row = sub_row == n_items + 1;
        let item_row_idx = if !on_add_row { Some(sub_row - 1) } else { None };

        // The trailing `[x]` / `[+]`, at the columns the row was built with.
        let in_trailing_button = matches!(
            super::widget_map::text_list_target(col),
            super::widget_map::TextListTarget::Button
        );

        match (on_add_row, item_row_idx, in_trailing_button) {
            // Click on `[+]` of an active input: commit pending.
            (true, _, true) if tl.pending_active || !tl.new_item_text.is_empty() => {
                tl.add_item();
                dialog.user_edited = true;
                dialog.focus_on_buttons = false;
                dialog.selected_item = item_idx;
                tl.focused_item = None;
                tl.pending_active = false;
                dialog.update_focus_states();
                Ok(true)
            }
            // Click anywhere on the (collapsed) `[+] Add new` row, or
            // on the input text area: focus the trailing slot and
            // activate input mode so the user can start typing.
            (true, _, _) => {
                dialog.focus_on_buttons = false;
                dialog.selected_item = item_idx;
                tl.activate_pending();
                dialog.editing_text = true;
                dialog.update_focus_states();
                Ok(true)
            }
            // Click on `[x]` of a committed row: remove it.
            (false, Some(row_idx), true) if row_idx < tl.items.len() => {
                tl.remove_item(row_idx);
                dialog.user_edited = true;
                dialog.focus_on_buttons = false;
                dialog.selected_item = item_idx;
                dialog.update_focus_states();
                Ok(true)
            }
            // Click on a committed row's text area: focus that item.
            (false, Some(row_idx), false) if row_idx < tl.items.len() => {
                dialog.focus_on_buttons = false;
                dialog.selected_item = item_idx;
                tl.focused_item = Some(row_idx);
                tl.pending_active = false;
                dialog.update_focus_states();
                Ok(true)
            }
            _ => Ok(false),
        }
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
