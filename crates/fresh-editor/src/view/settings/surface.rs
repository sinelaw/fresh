//! What the settings page and the entry dialog share.
//!
//! [`SettingsState`](super::state::SettingsState) and
//! [`EntryDialogState`](super::entry_dialog::EntryDialogState) are the same
//! shape: a column of [`SettingItem`]s, one of them selected, over one
//! widget store (`controls`) that the selected item's control is edited in.
//! A press on a list row, typing into a live field and a text list's rows
//! are the same act on either, and each used to carry its own copy of them —
//! which is how the two drifted (the dialog marked the field edited before
//! pushing a draft, the page after). The bodies live here once; what differs
//! is what each surface does when a value changes and how it opens a row.

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
    /// The key of the live control, when the store's focus names one of the
    /// selected item's.
    fn live_control(&self) -> Option<String>;
    /// Fold a kind's events for `key` back into the item's model.
    fn absorb(&mut self, key: &str, events: &[(String, Value)]);
    /// The selected item's value was changed by the user.
    fn value_changed(&mut self);
    /// Open a row of the selected text list for editing — an item's field,
    /// or the add row's for `None`.
    fn edit_list_row(&mut self, row: Option<usize>);

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
