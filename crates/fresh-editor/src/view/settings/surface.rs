//! What the settings page and the entry dialog share.
//!
//! [`SettingsState`](super::state::SettingsState) and
//! [`EntryDialogState`](super::entry_dialog::EntryDialogState) are the same
//! shape: a column of [`SettingItem`]s, one of them selected, over one
//! widget store (`controls`) that the selected item's control is edited in.
//! Which control is live, what it paints as focused, a list's cursor row, a
//! press on a list row, typing into a live field and a text list's rows are
//! the same act on either. Each type used to carry its own copy of all of
//! them, and the copies had started to drift (the dialog marked the field
//! edited before pushing a draft, the page after). The bodies live here once.
//! What differs is what each surface does when a value changes, how it opens
//! a text list's row, and what else can hold its keyboard.

use super::items::SettingControl;
use super::items::SettingItem;
use super::live;
use crate::widgets::WidgetPanelState;
use serde_json::Value;

pub(crate) trait SettingsSurface {
    /// The store the selected item's control is edited in.
    fn controls(&self) -> &WidgetPanelState;
    fn controls_mut(&mut self) -> &mut WidgetPanelState;
    fn current_item(&self) -> Option<&SettingItem>;
    fn current_item_mut(&mut self) -> Option<&mut SettingItem>;
    /// Whether the column of items holds the keyboard, rather than
    /// something beside it (the dialog's buttons, the page's categories).
    fn items_have_keyboard(&self) -> bool;
    /// Fold a kind's events for `key` back into the item's model.
    fn absorb(&mut self, key: &str, events: &[(String, Value)]);
    /// The selected item's value was changed by the user.
    fn value_changed(&mut self);
    /// Open a row of the selected text list for editing — an item's field,
    /// or the add row's for `None`.
    fn edit_list_row(&mut self, row: Option<usize>);

    /// The key of the live control: the selected item's, or one of its
    /// rows', when the store's focus names it.
    fn live_control(&self) -> Option<String> {
        let item = self.current_item()?;
        (self.items_have_keyboard()
            && live::kind_edited(&item.control)
            && self.focus_key_of(item).is_some())
        .then(|| self.controls().focus_key.clone())
    }

    /// The store's focus key when it names `item`'s control or one of its
    /// rows — what the item paints as focused.
    fn focus_key_of(&self, item: &SettingItem) -> Option<&str> {
        let key = self.controls().focus_key.as_str();
        (key == item.path
            || key
                .strip_prefix(&item.path)
                .is_some_and(|r| r.starts_with("::")))
        .then_some(key)
    }

    /// The row the selected item's list cursor is on, while the list has
    /// the keyboard: a map's or an object array's entry, or its add row
    /// (`SettingControl::add_row`).
    fn composite_cursor(&self) -> Option<usize> {
        let item = self.current_item()?;
        self.composite_cursor_of(item)
    }

    /// [`composite_cursor`](Self::composite_cursor) for any item.
    fn composite_cursor_of(&self, item: &SettingItem) -> Option<usize> {
        if !item.control.has_list_rows() || self.controls().focus_key != item.path {
            return None;
        }
        let spec = super::widget_map::live_widget(&item.path, &item.control, &item.path);
        live::list_row(self.controls(), &spec, &item.path)
    }

    /// Whether the selected item's dropdown has its list up.
    fn is_dropdown_open(&self) -> bool {
        self.current_item().is_some_and(|item| {
            matches!(item.control, SettingControl::Dropdown { .. })
                && crate::widgets::kinds::dropdown::is_open(&item.path, self.controls())
        })
    }

    /// Whether the selected item's JSON editor is being edited.
    fn is_editing_json(&self) -> bool {
        self.live_control().is_some()
            && matches!(
                self.current_item().map(|i| &i.control),
                Some(SettingControl::Json { .. })
            )
    }

    /// Move the live text field's caret to a byte of its value — a press
    /// (#2573). No-op unless a text edit is open.
    fn position_text_cursor(&mut self, byte: usize) {
        let Some(path) = self.live_control() else {
            return;
        };
        if let Some(editor) = live::text_editor(self.controls_mut(), &path) {
            editor.clear_selection();
            editor.set_cursor_from_flat(byte);
        }
    }

    /// Up or Down in a live text list field: the adjacent row's field
    /// opens — the add row's after the last item. Returns whether the
    /// keyboard moved; at either end it did not, and the caller moves on.
    fn list_row_step(&mut self, delta: i32) -> bool {
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
        let target = live.unwrap_or(n) as i32 + delta;
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
    fn list_row_enter(&mut self) {
        if self.live_list_row() == Some(None) && self.commit_list_draft() {
            self.edit_list_row(None);
        }
    }

    /// The node of the selected item's description that carries `key`: the
    /// control's own, or one of a text list's rows.
    fn spec_for(&self, key: &str) -> Option<fresh_core::api::WidgetSpec> {
        let item = self.current_item()?;
        Some(super::widget_map::live_widget(
            &item.path,
            &item.control,
            key,
        ))
    }

    /// The selected item's control as its kind sees it, keyed by its path.
    fn current_spec(&self) -> Option<(String, fresh_core::api::WidgetSpec)> {
        let item = self.current_item()?;
        Some((item.path.clone(), self.spec_for(&item.path)?))
    }

    /// The row of the selected text list whose field is live: `Some(i)`
    /// an item's, `None` the add row's.
    fn live_list_row(&self) -> Option<Option<usize>> {
        let item = self.current_item()?;
        live::text_list::live_row(self.controls(), &item.path)
    }

    /// A press on a row of the selected item's list: the list takes the
    /// keyboard with its cursor on the row.
    fn select_list_row(&mut self, row: usize) {
        let Some((path, spec)) = self.current_spec() else {
            return;
        };
        if !self
            .current_item()
            .is_some_and(|i| i.control.has_list_rows())
        {
            return;
        }
        self.controls_mut().focus_key = path.clone();
        let o = live::pointer(
            self.controls_mut(),
            &spec,
            &path,
            "select",
            &serde_json::json!({ "index": row }),
        );
        self.absorb(&path, &o.fx.events);
    }

    /// The add row's draft becomes an item. Returns whether one did.
    fn commit_list_draft(&mut self) -> bool {
        let Some(item) = self.current_item() else {
            return false;
        };
        let path = item.path.clone();
        let Some(text) = live::text_list::take_draft(self.controls_mut(), &path) else {
            return false;
        };
        if let Some(SettingControl::TextList { items, .. }) =
            self.current_item_mut().map(|i| &mut i.control)
        {
            items.push(text);
        }
        self.value_changed();
        true
    }

    /// Remove item `i` of the selected text list. A field live on it moves
    /// to the row that takes its place.
    fn remove_list_row(&mut self, i: usize) {
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
        self.value_changed();
        if let Some(row) = live {
            if let Some(item) = self.current_item() {
                let path = item.path.clone();
                live::text_list::leave(self.controls_mut(), &path);
            }
            let row = match row {
                Some(r) if r > i => Some(r - 1),
                Some(r) if r == i => (r < n).then_some(r),
                other => other,
            };
            self.edit_list_row(row);
        }
    }

    /// Type into the live control: a paste, or the character that began
    /// the edit.
    fn live_text(&mut self, text: &str) -> bool {
        let Some(key) = self.live_control() else {
            return false;
        };
        let Some(spec) = self.spec_for(&key) else {
            return false;
        };
        let outcome = live::text(self.controls_mut(), &spec, &key, text);
        self.absorb(&key, &outcome.fx.events);
        true
    }
}
