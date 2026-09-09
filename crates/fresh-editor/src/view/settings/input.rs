//! Input handling for the Settings dialog.
//!
//! Implements the InputHandler trait for SettingsState, routing input
//! through the focus hierarchy: Dialog -> Panel -> Control.

use super::items::SettingControl;
use super::state::{FocusPanel, FocusTarget, SettingsState};
use crate::input::handler::{DeferredAction, InputContext, InputHandler, InputResult};
use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

/// Button action in entry dialog
enum ButtonAction {
    Save,
    Delete,
    Cancel,
}

/// Control activation action in entry dialog
enum ControlAction {
    /// A scalar: its kind acts (`EntryDialogState::activate_control`).
    Activate,
    OpenNestedDialog,
}

impl InputHandler for SettingsState {
    fn handle_key_event(&mut self, event: &KeyEvent, ctx: &mut InputContext) -> InputResult {
        // Entry-dialog "Delete X?" prompt takes priority over both
        // the discard prompt and the entry dialog — it's the topmost
        // overlay.
        if self.showing_entry_delete_confirm {
            return self.handle_entry_delete_confirm_input(event);
        }

        // Entry-dialog "Discard changes?" prompt takes priority over
        // the entry dialog itself — it's stacked on top.
        if self.showing_entry_discard_confirm {
            return self.handle_entry_discard_confirm_input(event);
        }

        // Entry dialog takes priority when open
        if self.has_entry_dialog() {
            return self.handle_entry_dialog_input(event, ctx);
        }

        // Confirmation dialog takes priority
        if self.showing_confirm_dialog {
            return self.handle_confirm_dialog_input(event, ctx);
        }

        // Reset confirmation dialog takes priority
        if self.showing_reset_dialog {
            return self.handle_reset_dialog_input(event);
        }

        // Help overlay takes priority
        if self.showing_help {
            return self.handle_help_input(event, ctx);
        }

        // Search mode takes priority
        if self.search_active {
            return self.handle_search_input(event, ctx);
        }

        // Global shortcut: Ctrl+S to save
        if event.modifiers.contains(KeyModifiers::CONTROL)
            && matches!(event.code, KeyCode::Char('s') | KeyCode::Char('S'))
        {
            ctx.defer(DeferredAction::CloseSettings { save: true });
            return InputResult::Consumed;
        }

        // Route to focused panel
        match self.focus_panel() {
            FocusPanel::Categories => self.handle_categories_input(event, ctx),
            FocusPanel::Settings => self.handle_settings_input(event, ctx),
            FocusPanel::Footer => self.handle_footer_input(event, ctx),
        }
    }

    fn is_modal(&self) -> bool {
        true // Settings dialog consumes all unhandled input
    }
}

impl SettingsState {
    /// Handle input when entry dialog is open
    ///
    /// Uses the same input flow as the main settings UI:
    /// 1. A live scalar's keys are its kind's (`handle_entry_live_control_key`)
    /// 2. A composite being edited answers its own keys
    /// 3. Otherwise -> navigation and control activation
    fn handle_entry_dialog_input(
        &mut self,
        event: &KeyEvent,
        ctx: &mut InputContext,
    ) -> InputResult {
        // Ctrl+S saves entry dialog from any mode
        if event.modifiers.contains(KeyModifiers::CONTROL)
            && matches!(event.code, KeyCode::Char('s') | KeyCode::Char('S'))
        {
            self.save_entry_dialog();
            return InputResult::Consumed;
        }

        let editing = match self.entry_dialog() {
            Some(dialog) => dialog.is_editing(),
            None => return InputResult::Consumed,
        };

        // A map's or an object array's list is live while its field is
        // selected, but the arrows on it are the dialog's navigation
        // (`focus_next` steps its rows through the kind); only a field being
        // *edited* takes every key.
        if editing {
            self.handle_entry_live_control_key(event, ctx)
        } else {
            self.handle_entry_dialog_navigation(event)
        }
    }

    /// A key while one of the dialog's scalar fields is live — the page's
    /// `handle_live_control_key`, with the dialog's form conventions: Enter
    /// on a field commits it and moves to the next, Tab commits it, Escape
    /// reverts it.
    fn handle_entry_live_control_key(
        &mut self,
        event: &KeyEvent,
        ctx: &mut InputContext,
    ) -> InputResult {
        use crate::widgets::kinds::KeyDisposition;
        if event.modifiers.contains(KeyModifiers::CONTROL)
            && matches!(event.code, KeyCode::Char('v') | KeyCode::Char('V'))
        {
            ctx.defer(DeferredAction::PasteToSettings);
            return InputResult::Consumed;
        }
        let Some(dialog) = self.entry_dialog_mut() else {
            return InputResult::Consumed;
        };
        let is_number = matches!(
            dialog.current_item().map(|i| &i.control),
            Some(SettingControl::Number { .. })
        );
        // Delete in a text list item's field removes the item — the row's
        // key, ahead of the field's forward-delete.
        if event.code == KeyCode::Delete {
            if let Some(Some(i)) = dialog.live_list_row() {
                dialog.remove_list_row(i);
                return InputResult::Consumed;
            }
        }
        let Some(outcome) = dialog.live_dispatch(event) else {
            return InputResult::Consumed;
        };
        if let Some(text) = outcome.fx.clipboard_copy {
            ctx.defer(DeferredAction::CopyToClipboard(text));
        }
        if outcome.disposition == KeyDisposition::Consumed {
            // A number's Enter committed its draft (the kind's own
            // convention); the form's is to move on to the next field.
            if is_number && event.code == KeyCode::Enter && dialog.live_control().is_none() {
                dialog.focus_next_field();
            }
            // A dual list's Enter asks for the next field, form-like; the
            // dialog obliges, and the field is no longer live.
            if let Some(delta) = outcome.fx.focus_advance {
                dialog.stop_editing();
                match delta < 0 {
                    true => dialog.focus_prev_field(),
                    false => dialog.focus_next_field(),
                }
            }
            return InputResult::Consumed;
        }
        if dialog.is_editing_text_field() {
            match event.code {
                KeyCode::Enter => {
                    dialog.stop_editing();
                    dialog.focus_next_field();
                }
                KeyCode::Tab | KeyCode::BackTab => dialog.stop_editing(),
                KeyCode::Esc => dialog.revert_editing(),
                _ => {}
            }
        } else if dialog.is_editing_json() {
            // Enter is the editor's (a newline). Tab leaves it once the
            // text parses — until then it stays, the legend saying why —
            // and Escape puts the text back.
            match event.code {
                KeyCode::Tab | KeyCode::BackTab if dialog.json_field_valid() => {
                    dialog.stop_editing()
                }
                KeyCode::Esc => dialog.revert_editing(),
                _ => {}
            }
        } else if let Some(row) = dialog.live_list_row() {
            // A text list's field. Up and Down walk the rows — Down past the
            // add row leaves for the next field, Up on an empty list's add
            // row for the previous one, and the first item is where Up
            // stops; Enter makes the add row's draft an item; Tab commits
            // the draft and moves on; Escape puts the list back; Delete
            // removes the item the field is in.
            match event.code {
                KeyCode::Up | KeyCode::Down => {
                    let up = event.code == KeyCode::Up;
                    let moved = dialog.list_row_step(if up { -1 } else { 1 });
                    if !moved && (!up || row.is_none()) {
                        dialog.stop_editing();
                        match up {
                            true => dialog.focus_prev_field(),
                            false => dialog.focus_next_field(),
                        }
                    }
                }
                KeyCode::Enter => dialog.list_row_enter(),
                KeyCode::Tab | KeyCode::BackTab => {
                    dialog.stop_editing();
                    dialog.focus_next_field();
                }
                KeyCode::Esc => dialog.revert_editing(),
                _ => {}
            }
        } else if matches!(event.code, KeyCode::Esc | KeyCode::Tab | KeyCode::BackTab) {
            // A dual list hands the keyboard back; its value is already the
            // field's.
            dialog.stop_editing();
        }
        InputResult::Consumed
    }

    /// Handle navigation and activation in entry dialog (same pattern as handle_settings_input)
    fn handle_entry_dialog_navigation(&mut self, event: &KeyEvent) -> InputResult {
        match event.code {
            KeyCode::Esc => {
                // Esc on a dialog with uncommitted edits prompts for
                // confirmation; a clean dialog closes immediately.
                // Without the dirty check, an accidental Esc silently
                // destroys every field the user just typed in.
                let dirty = self.entry_dialog().map(|d| d.is_dirty()).unwrap_or(false);
                if dirty {
                    self.showing_entry_discard_confirm = true;
                    self.entry_discard_confirm_selection = 0;
                } else {
                    self.close_entry_dialog();
                }
            }
            KeyCode::Up => {
                if let Some(dialog) = self.entry_dialog_mut() {
                    dialog.focus_prev();
                }
            }
            KeyCode::Down => {
                if let Some(dialog) = self.entry_dialog_mut() {
                    dialog.focus_next();
                }
            }
            KeyCode::Tab => {
                // Tab cycles sequentially through all fields, sub-fields, and buttons
                if let Some(dialog) = self.entry_dialog_mut() {
                    dialog.focus_next();
                }
            }
            KeyCode::BackTab => {
                // Shift+Tab cycles in reverse
                if let Some(dialog) = self.entry_dialog_mut() {
                    dialog.focus_prev();
                }
            }
            KeyCode::Left => {
                if let Some(dialog) = self.entry_dialog_mut() {
                    if dialog.focus_on_buttons && dialog.focused_button > 0 {
                        dialog.focused_button -= 1;
                    }
                }
            }
            KeyCode::Right => {
                if let Some(dialog) = self.entry_dialog_mut() {
                    if dialog.focus_on_buttons && dialog.focused_button + 1 < dialog.button_count()
                    {
                        dialog.focused_button += 1;
                    }
                }
            }
            KeyCode::Enter => {
                // A focused per-field action button ([Reset]/[Inherit]) handles activation.
                if self.entry_dialog_activate_focused_field_button() {
                    return InputResult::Consumed;
                }

                // Check button state first with immutable borrow
                // Button layout: [Save, Cancel] or [Save, Cancel, Delete].
                // Save = 0, Cancel = 1, Delete = 2 (when present).
                let button_action = self.entry_dialog().and_then(|dialog| {
                    if dialog.focus_on_buttons {
                        let has_delete = !dialog.is_new && !dialog.no_delete;
                        match dialog.focused_button {
                            0 => Some(ButtonAction::Save),
                            1 => Some(ButtonAction::Cancel),
                            2 if has_delete => Some(ButtonAction::Delete),
                            _ => None,
                        }
                    } else {
                        None
                    }
                });

                if let Some(action) = button_action {
                    match action {
                        ButtonAction::Save => self.save_entry_dialog(),
                        ButtonAction::Delete => self.request_entry_delete_confirm(),
                        ButtonAction::Cancel => self.close_entry_dialog(),
                    }
                } else if event.modifiers.contains(KeyModifiers::CONTROL) {
                    // Ctrl+Enter always saves
                    self.save_entry_dialog();
                } else {
                    // Activate current control
                    let control_action = self
                        .entry_dialog()
                        .and_then(|dialog| {
                            dialog.current_item().map(|item| match &item.control {
                                SettingControl::Toggle { .. }
                                | SettingControl::Dropdown { .. }
                                | SettingControl::Text { .. }
                                | SettingControl::Number { .. }
                                | SettingControl::DualList { .. }
                                | SettingControl::Json { .. }
                                | SettingControl::TextList { .. } => Some(ControlAction::Activate),
                                SettingControl::Map { .. } | SettingControl::ObjectArray { .. } => {
                                    Some(ControlAction::OpenNestedDialog)
                                }
                                _ => None,
                            })
                        })
                        .flatten();

                    if let Some(action) = control_action {
                        match action {
                            ControlAction::Activate => {
                                if let Some(dialog) = self.entry_dialog_mut() {
                                    dialog.activate_control();
                                }
                            }
                            ControlAction::OpenNestedDialog => {
                                self.open_nested_entry_dialog();
                            }
                        }
                    }
                }
            }
            KeyCode::Char(' ') => {
                // A focused per-field action button ([Reset]/[Inherit]) handles activation.
                if self.entry_dialog_activate_focused_field_button() {
                    return InputResult::Consumed;
                }

                // Space toggles booleans, activates dropdowns (but doesn't submit form)
                let activates = self.entry_dialog().is_some_and(|dialog| {
                    !dialog.focus_on_buttons // Space on buttons does nothing (Enter activates)
                        && matches!(
                            dialog.current_item().map(|i| &i.control),
                            Some(SettingControl::Toggle { .. } | SettingControl::Dropdown { .. })
                        )
                });
                if activates {
                    if let Some(dialog) = self.entry_dialog_mut() {
                        dialog.activate_control();
                    }
                }
            }
            KeyCode::Char(c) => {
                // Auto-enter edit mode when typing on a text or number field
                let scalar = self
                    .entry_dialog()
                    .and_then(|dialog| {
                        if dialog.focus_on_buttons {
                            return None;
                        }
                        dialog.current_item().map(|item| match &item.control {
                            SettingControl::Text { .. }
                            | SettingControl::Json { .. }
                            | SettingControl::TextList { .. } => true,
                            SettingControl::Number { .. } => {
                                c.is_ascii_digit() || c == '-' || c == '.'
                            }
                            _ => false,
                        })
                    })
                    .unwrap_or(false);

                if scalar {
                    // The field becomes live and the character is its kind's:
                    // a text field types it at the end, a number opens its
                    // draft with it, a text list's add row opens with it.
                    if let Some(dialog) = self.entry_dialog_mut() {
                        dialog.type_into_control(&c.to_string());
                    }
                    return InputResult::Consumed;
                }
            }
            _ => {}
        }
        InputResult::Consumed
    }

    /// Handle input when confirmation dialog is showing
    fn handle_confirm_dialog_input(
        &mut self,
        event: &KeyEvent,
        ctx: &mut InputContext,
    ) -> InputResult {
        match event.code {
            KeyCode::Left | KeyCode::BackTab => {
                if self.confirm_dialog_selection > 0 {
                    self.confirm_dialog_selection -= 1;
                }
                InputResult::Consumed
            }
            KeyCode::Right | KeyCode::Tab => {
                if self.confirm_dialog_selection < 2 {
                    self.confirm_dialog_selection += 1;
                }
                InputResult::Consumed
            }
            KeyCode::Enter => {
                match self.confirm_dialog_selection {
                    0 => ctx.defer(DeferredAction::CloseSettings { save: true }), // Save
                    1 => ctx.defer(DeferredAction::CloseSettings { save: false }), // Discard
                    2 => self.showing_confirm_dialog = false, // Cancel - back to settings
                    _ => {}
                }
                InputResult::Consumed
            }
            KeyCode::Esc => {
                self.showing_confirm_dialog = false;
                InputResult::Consumed
            }
            KeyCode::Char('s') | KeyCode::Char('S') => {
                ctx.defer(DeferredAction::CloseSettings { save: true });
                InputResult::Consumed
            }
            KeyCode::Char('d') | KeyCode::Char('D') => {
                ctx.defer(DeferredAction::CloseSettings { save: false });
                InputResult::Consumed
            }
            _ => InputResult::Consumed, // Modal: consume all
        }
    }

    /// Handle input when reset confirmation dialog is showing
    fn handle_reset_dialog_input(&mut self, event: &KeyEvent) -> InputResult {
        match event.code {
            KeyCode::Left | KeyCode::BackTab => {
                if self.reset_dialog_selection > 0 {
                    self.reset_dialog_selection -= 1;
                }
                InputResult::Consumed
            }
            KeyCode::Right | KeyCode::Tab => {
                if self.reset_dialog_selection < 1 {
                    self.reset_dialog_selection += 1;
                }
                InputResult::Consumed
            }
            KeyCode::Enter => {
                match self.reset_dialog_selection {
                    0 => {
                        // Reset all changes
                        self.discard_changes();
                        self.showing_reset_dialog = false;
                    }
                    1 => {
                        // Cancel - back to settings
                        self.showing_reset_dialog = false;
                    }
                    _ => {}
                }
                InputResult::Consumed
            }
            KeyCode::Esc => {
                self.showing_reset_dialog = false;
                InputResult::Consumed
            }
            KeyCode::Char('r') | KeyCode::Char('R') => {
                self.discard_changes();
                self.showing_reset_dialog = false;
                InputResult::Consumed
            }
            _ => InputResult::Consumed, // Modal: consume all
        }
    }

    /// Handle input when the entry-dialog discard-confirm prompt is up.
    /// Buttons: 0 = Keep editing (default), 1 = Discard.
    fn handle_entry_discard_confirm_input(&mut self, event: &KeyEvent) -> InputResult {
        match event.code {
            KeyCode::Left | KeyCode::BackTab if self.entry_discard_confirm_selection > 0 => {
                self.entry_discard_confirm_selection -= 1;
            }
            KeyCode::Right | KeyCode::Tab if self.entry_discard_confirm_selection < 1 => {
                self.entry_discard_confirm_selection += 1;
            }
            KeyCode::Enter => {
                match self.entry_discard_confirm_selection {
                    0 => {
                        // Keep editing — just dismiss the prompt.
                        self.showing_entry_discard_confirm = false;
                    }
                    1 => {
                        // Discard — close the entry dialog without saving.
                        self.showing_entry_discard_confirm = false;
                        self.close_entry_dialog();
                    }
                    _ => {}
                }
            }
            KeyCode::Esc => {
                // Esc on the prompt means "keep editing".
                self.showing_entry_discard_confirm = false;
            }
            KeyCode::Char('d') | KeyCode::Char('D') => {
                self.showing_entry_discard_confirm = false;
                self.close_entry_dialog();
            }
            _ => {}
        }
        InputResult::Consumed
    }

    /// Handle input when the entry-dialog delete-confirm prompt is up.
    /// Buttons: 0 = Cancel (default), 1 = Delete.
    fn handle_entry_delete_confirm_input(&mut self, event: &KeyEvent) -> InputResult {
        match event.code {
            KeyCode::Left | KeyCode::BackTab if self.entry_delete_confirm_selection > 0 => {
                self.entry_delete_confirm_selection -= 1;
            }
            KeyCode::Right | KeyCode::Tab if self.entry_delete_confirm_selection < 1 => {
                self.entry_delete_confirm_selection += 1;
            }
            KeyCode::Enter => match self.entry_delete_confirm_selection {
                0 => {
                    self.showing_entry_delete_confirm = false;
                }
                1 => {
                    self.showing_entry_delete_confirm = false;
                    self.delete_entry_dialog();
                }
                _ => {}
            },
            KeyCode::Esc => {
                self.showing_entry_delete_confirm = false;
            }
            _ => {}
        }
        InputResult::Consumed
    }

    /// Handle input when help overlay is showing
    fn handle_help_input(&mut self, _event: &KeyEvent, _ctx: &mut InputContext) -> InputResult {
        // Any key dismisses help
        self.showing_help = false;
        InputResult::Consumed
    }

    /// Handle input when search is active
    fn handle_search_input(&mut self, event: &KeyEvent, _ctx: &mut InputContext) -> InputResult {
        match event.code {
            KeyCode::Esc => {
                self.cancel_search();
                InputResult::Consumed
            }
            KeyCode::Enter => {
                self.jump_to_search_result();
                InputResult::Consumed
            }
            KeyCode::Up => {
                self.search_prev();
                InputResult::Consumed
            }
            KeyCode::Down => {
                self.search_next();
                InputResult::Consumed
            }
            // Cursor movement within the query text. Up/Down are reserved
            // for navigating the result list (above), so Left/Right/Home/End
            // edit the query — matching the Command Palette, where the same
            // split makes the filter feel like a real text input.
            KeyCode::Left => {
                self.search_cursor_left();
                InputResult::Consumed
            }
            KeyCode::Right => {
                self.search_cursor_right();
                InputResult::Consumed
            }
            KeyCode::Home => {
                self.search_cursor_home();
                InputResult::Consumed
            }
            KeyCode::End => {
                self.search_cursor_end();
                InputResult::Consumed
            }
            KeyCode::Delete => {
                self.search_delete();
                InputResult::Consumed
            }
            // Only plain (or Shift-modified) chars type into the filter.
            // Ctrl/Alt chords — Ctrl+A/C/V/X etc. — must NOT insert their
            // letter; they fall through to the modal consume below so they
            // no-op instead of corrupting the query. (Selection and
            // clipboard aren't wired for this field.)
            KeyCode::Char(c)
                if !event.modifiers.contains(KeyModifiers::CONTROL)
                    && !event.modifiers.contains(KeyModifiers::ALT) =>
            {
                self.search_insert_char(c);
                InputResult::Consumed
            }
            KeyCode::Backspace => {
                self.search_backspace();
                InputResult::Consumed
            }
            _ => InputResult::Consumed, // Modal: consume all
        }
    }

    /// The category tree's own keys, wherever they came in.
    ///
    /// **One implementation, two entry points.** The tree's node claims these
    /// when it holds focus and sends `UiFact::SettingsTree`; the arms below
    /// call this when it does not. They must not drift, which is why they are
    /// not two `match`es: what moved onto the node in Phase 3.2 is the
    /// *interpretation* of the key, not the behaviour behind it.
    pub fn tree_key(&mut self, k: crate::view::shell::settings::TreeKey) {
        use crate::view::shell::settings::TreeKey as T;
        match k {
            T::Prev => self.select_prev(),
            T::Next => self.select_next(),
            // Page up and down in the tree scroll by viewport height.
            T::PageUp => {
                let viewport = self.categories_scroll.scroll.viewport.max(1) as i32;
                self.tree_step(-viewport);
            }
            T::PageDown => {
                let viewport = self.categories_scroll.scroll.viewport.max(1) as i32;
                self.tree_step(viewport);
            }
            T::First => {
                let rows = self.visible_tree();
                let cur = self.tree_cursor_index(&rows) as i32;
                if cur > 0 {
                    self.tree_step(-cur);
                }
            }
            T::Last => {
                let rows = self.visible_tree();
                let cur = self.tree_cursor_index(&rows) as i32;
                let last = rows.len() as i32 - 1;
                if last > cur {
                    self.tree_step(last - cur);
                }
            }
            T::Expand => {
                // Right ONLY expands an expandable category. Does not move
                // focus into the body panel — that's Tab's job.
                let cat_idx = self.selected_category;
                if self.is_category_expandable(cat_idx)
                    && !self.expanded_categories.contains(&cat_idx)
                {
                    self.expanded_categories.insert(cat_idx);
                }
            }
            T::Collapse => {
                // Left ONLY collapses an expanded category. No-op otherwise.
                let cat_idx = self.selected_category;
                if self.expanded_categories.contains(&cat_idx) {
                    self.expanded_categories.remove(&cat_idx);
                    // Sections aren't visible anymore — pull the cursor
                    // back to the category row so the next Down step
                    // walks to the *next* category, not into the
                    // (now-hidden) sections.
                    self.tree_cursor_section = None;
                }
            }
        }
    }

    /// Handle input when Categories panel is focused
    fn handle_categories_input(&mut self, event: &KeyEvent, ctx: &mut InputContext) -> InputResult {
        use crate::view::shell::settings::TreeKey as T;
        // The eight the tree's node claims for itself when it has focus
        // (`view::shell::settings::categories_keys`), for a key that arrives
        // without the tree in front of it.
        let tree = match event.code {
            KeyCode::Up => Some(T::Prev),
            KeyCode::Down => Some(T::Next),
            KeyCode::PageUp => Some(T::PageUp),
            KeyCode::PageDown => Some(T::PageDown),
            KeyCode::Home => Some(T::First),
            KeyCode::End => Some(T::Last),
            KeyCode::Right => Some(T::Expand),
            KeyCode::Left => Some(T::Collapse),
            _ => None,
        };
        if let Some(k) = tree {
            self.tree_key(k);
            return InputResult::Consumed;
        }
        match event.code {
            KeyCode::Char('/') => {
                self.start_search();
                InputResult::Consumed
            }
            KeyCode::Char('?') => {
                self.toggle_help();
                InputResult::Consumed
            }
            KeyCode::Esc => {
                self.request_close(ctx);
                InputResult::Consumed
            }
            _ => InputResult::Ignored, // Let modal catch it
        }
    }

    /// Handle input when Settings panel is focused
    fn handle_settings_input(&mut self, event: &KeyEvent, ctx: &mut InputContext) -> InputResult {
        // A live control's keys are its kind's; what the kind declines is
        // the surface's edit convention, and what a list at the end of its
        // rows declines is the page's. See `handle_live_control_key`.
        if self.live_control().is_some() {
            if let Some(result) = self.handle_live_control_key(event, ctx) {
                return result;
            }
        }

        match event.code {
            KeyCode::Up => {
                self.select_prev();
                InputResult::Consumed
            }
            KeyCode::Down => {
                self.select_next();
                InputResult::Consumed
            }
            KeyCode::Left => {
                // Left always navigates back to categories — numbers no
                // longer use Left/Right for inc/dec (direct typing only).
                self.focus_on(FocusTarget::Categories);
                InputResult::Consumed
            }
            KeyCode::Enter | KeyCode::Char(' ') => {
                self.handle_control_activate(ctx);
                InputResult::Consumed
            }
            // Type-to-edit: a digit, `-` or `.` on a number card opens its
            // draft with the typed character replacing the value — the
            // kind's own answer to text typed at a displayed cell, once the
            // card's control is live.
            KeyCode::Char(c)
                if self.is_number_control() && (c.is_ascii_digit() || c == '-' || c == '.') =>
            {
                if let Some(path) = self.current_item().map(|i| i.path.clone()) {
                    self.controls.focus_key = path;
                }
                self.handle_live_control_key(event, ctx)
                    .unwrap_or(InputResult::Consumed)
            }
            KeyCode::PageDown => {
                self.select_next_page();
                InputResult::Consumed
            }
            KeyCode::PageUp => {
                self.select_prev_page();
                InputResult::Consumed
            }
            KeyCode::Char('/') => {
                self.start_search();
                InputResult::Consumed
            }
            KeyCode::Char('?') => {
                self.toggle_help();
                InputResult::Consumed
            }
            KeyCode::Delete => {
                // Delete key: set nullable setting to null (inherit)
                self.set_current_to_null();
                InputResult::Consumed
            }
            KeyCode::Esc => {
                self.request_close(ctx);
                InputResult::Consumed
            }
            _ => InputResult::Ignored, // Let modal catch it
        }
    }

    /// Handle input when Footer is focused
    /// Footer buttons: [Layer] [Reset] [Save] [Cancel] + [Edit] on left for advanced users
    /// Left/Right step between buttons; Left off the first returns to the body.
    /// Tab is the tree's ring (`view::shell::settings::keys`).
    fn handle_footer_input(&mut self, event: &KeyEvent, ctx: &mut InputContext) -> InputResult {
        const FOOTER_BUTTON_COUNT: usize = 5;

        match event.code {
            KeyCode::Left => {
                // Move to previous button, or back to the body
                match self.footer_button_index {
                    0 => self.focus_on(FocusTarget::Card(self.selected_item)),
                    i => self.focus_on(FocusTarget::Footer(i - 1)),
                }
                InputResult::Consumed
            }
            KeyCode::Right => {
                // Move to next button
                if self.footer_button_index < FOOTER_BUTTON_COUNT - 1 {
                    self.focus_on(FocusTarget::Footer(self.footer_button_index + 1));
                }
                InputResult::Consumed
            }
            KeyCode::Enter => {
                match self.footer_button_index {
                    0 => self.cycle_target_layer(), // Layer button
                    1 => {
                        // Reset/Inherit button — for nullable items, set to null (inherit);
                        // otherwise show reset-all dialog
                        let is_nullable_set = self
                            .current_item()
                            .map(|item| item.nullable && !item.is_null)
                            .unwrap_or(false);
                        if is_nullable_set {
                            self.set_current_to_null();
                        } else {
                            self.request_reset();
                        }
                    }
                    2 => ctx.defer(DeferredAction::CloseSettings { save: true }),
                    3 => self.request_close(ctx),
                    4 => ctx.defer(DeferredAction::OpenConfigFile {
                        layer: self.target_layer,
                    }), // Edit config file
                    _ => {}
                }
                InputResult::Consumed
            }
            KeyCode::Esc => {
                self.request_close(ctx);
                InputResult::Consumed
            }
            KeyCode::Char('/') => {
                self.start_search();
                InputResult::Consumed
            }
            KeyCode::Char('?') => {
                self.toggle_help();
                InputResult::Consumed
            }
            _ => InputResult::Ignored, // Let modal catch it
        }
    }

    /// A key while the selected card's control is live.
    ///
    /// The kind answers first — caret and selection keys, a draft's digits,
    /// a list's arrows, the clipboard chords it owns — and reports what the
    /// model should learn. What it declines is the surface's: paste, which
    /// reads a clipboard the kind cannot see, and the text field's edit
    /// convention, where Enter and Tab record the value and Escape puts the
    /// old one back. A number's and a dropdown's conventions are the kind's
    /// own (`Number::on_key`, `Dropdown::on_key`), so nothing is left here
    /// for them; the control stops being live when its kind lets go.
    ///
    /// `None` is the page's key: what a map's or an object array's list
    /// declines (Left, Tab, Escape, the search and help keys), and an arrow
    /// or a page key at the end of its rows, which moves on to the next
    /// card.
    fn handle_live_control_key(
        &mut self,
        event: &KeyEvent,
        ctx: &mut InputContext,
    ) -> Option<InputResult> {
        use crate::widgets::kinds::KeyDisposition;
        if event.modifiers.contains(KeyModifiers::CONTROL)
            && matches!(event.code, KeyCode::Char('v') | KeyCode::Char('V'))
        {
            ctx.defer(DeferredAction::PasteToSettings);
            return Some(InputResult::Consumed);
        }
        let has_rows = self
            .current_item()
            .is_some_and(|i| i.control.has_list_rows());
        // Delete in a text list item's field removes the item — the row's
        // key, ahead of the field's forward-delete; the `[x]` beside the
        // row is the same act by mouse.
        if event.code == KeyCode::Delete {
            if let Some(Some(i)) = self.live_list_row() {
                self.remove_list_row(i);
                return Some(InputResult::Consumed);
            }
        }
        let Some(outcome) = self.live_dispatch(event) else {
            return Some(InputResult::Consumed);
        };
        if let Some(text) = outcome.fx.clipboard_copy {
            ctx.defer(DeferredAction::CopyToClipboard(text));
        }
        if outcome.disposition == KeyDisposition::Consumed {
            let moved = outcome.fx.events.iter().any(|(e, _)| e == "select");
            let stepping = matches!(
                event.code,
                KeyCode::Up | KeyCode::Down | KeyCode::PageUp | KeyCode::PageDown
            );
            if has_rows && stepping && !moved {
                return None;
            }
            return Some(InputResult::Consumed);
        }
        if has_rows {
            return None;
        }
        if self.is_editing_text_control() {
            match event.code {
                KeyCode::Enter | KeyCode::Tab | KeyCode::BackTab => {
                    self.commit_text_edit();
                }
                KeyCode::Esc => self.revert_text_edit(),
                _ => {}
            }
        } else if self.is_editing_json() {
            // Enter is the editor's (a newline); Tab and Escape leave it,
            // keeping a text that parses and putting back one that does
            // not.
            if matches!(event.code, KeyCode::Tab | KeyCode::BackTab | KeyCode::Esc) {
                self.json_exit_editing();
            }
        } else if self.is_editing_dual_list() {
            // Escape hands the keyboard back to the page; Tab does the
            // same and then moves on, as it does from any card.
            match event.code {
                KeyCode::Esc => self.leave_live_control(),
                KeyCode::Tab | KeyCode::BackTab => {
                    self.leave_live_control();
                    return Some(InputResult::Ignored);
                }
                _ => {}
            }
        } else if self.live_list_row().is_some() {
            // A text list's field. Up and Down walk the rows (and stay at
            // either end); Enter makes the add row's draft an item; Tab
            // commits the draft and leaves; Escape drops it and leaves;
            // Delete removes the item the field is in.
            match event.code {
                KeyCode::Up => {
                    self.list_row_step(-1);
                }
                KeyCode::Down => {
                    self.list_row_step(1);
                }
                KeyCode::Enter => self.list_row_enter(),
                KeyCode::Tab | KeyCode::BackTab => self.leave_list_row(true),
                KeyCode::Esc => self.leave_list_row(false),
                _ => {}
            }
        }
        Some(InputResult::Consumed)
    }

    /// Request to reset all changes (shows confirm dialog if there are changes)
    fn request_reset(&mut self) {
        if self.has_changes() {
            self.showing_reset_dialog = true;
            self.reset_dialog_selection = 0;
        }
    }

    /// Request to close settings (shows confirm dialog if there are changes)
    fn request_close(&mut self, ctx: &mut InputContext) {
        if self.has_changes() {
            self.showing_confirm_dialog = true;
            self.confirm_dialog_selection = 0;
        } else {
            ctx.defer(DeferredAction::CloseSettings { save: false });
        }
    }

    /// Handle control activation (Enter/Space on a setting): the control's
    /// kind acts (`activate_control`), whatever it is.
    fn handle_control_activate(&mut self, _ctx: &mut InputContext) {
        self.activate_control();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

    fn key(code: KeyCode) -> KeyEvent {
        KeyEvent::new(code, KeyModifiers::NONE)
    }

    /// The value shown in the currently-focused plain `Text` control.
    fn text_value(state: &SettingsState) -> String {
        match state.current_item().map(|i| &i.control) {
            Some(SettingControl::Text { value, .. }) => value.clone(),
            _ => panic!("current item is not a Text control"),
        }
    }

    /// Focus the first editable plain `Text` control in the body. The
    /// Terminal shell `command` field is one such scalar; any Text control
    /// exercises the same shared body text handler.
    fn select_first_text_control(state: &mut SettingsState) -> bool {
        for pi in 0..state.pages.len() {
            for ii in 0..state.pages[pi].items.len() {
                if matches!(
                    state.pages[pi].items[ii].control,
                    SettingControl::Text { .. }
                ) {
                    state.selected_category = pi;
                    state.selected_item = ii;
                    return true;
                }
            }
        }
        false
    }

    /// A map's rows are walked by the arrows once its card is selected —
    /// the list is live, its cursor the kind's — and at the last row Down
    /// moves on to the next card.
    #[test]
    fn a_map_s_rows_are_walked_by_the_arrows_and_left_at_the_end() {
        let schema = include_str!("../../../plugins/config-schema.json");
        let config = crate::config::Config::default();
        let mut state = SettingsState::new(schema, &config).unwrap();
        state.visible = true;
        let (page, idx) = state
            .pages
            .iter()
            .enumerate()
            .find_map(|(p, page)| {
                page.items
                    .iter()
                    .position(|i| i.path == "/lsp")
                    .map(|i| (p, i))
            })
            .expect("the LSP map");
        state.selected_category = page;
        state.focus_on(FocusTarget::Card(idx));
        assert!(matches!(
            state.current_item().map(|i| &i.control),
            Some(SettingControl::Map { .. })
        ));
        assert_eq!(state.composite_cursor(), Some(0), "entered from above");
        assert!(state.live_control().is_some(), "the list has the keyboard");

        let mut ctx = InputContext::new();
        state.handle_key_event(&key(KeyCode::Down), &mut ctx);
        assert_eq!(state.composite_cursor(), Some(1), "Down walks the rows");
        state.handle_key_event(&key(KeyCode::Up), &mut ctx);
        assert_eq!(state.composite_cursor(), Some(0));

        // At the last row Down leaves the card.
        let last = state.current_item().unwrap().control.list_row_count() - 1;
        for _ in 0..last {
            state.handle_key_event(&key(KeyCode::Down), &mut ctx);
        }
        assert_eq!(state.composite_cursor(), Some(last), "on the add row");
        state.handle_key_event(&key(KeyCode::Down), &mut ctx);
        assert_eq!(state.selected_item, idx + 1, "Down at the add row moves on");
    }

    /// The search jump lands on the LSP map with its list live, and the
    /// arrows walk its rows from there.
    #[test]
    fn a_search_jump_to_a_map_lands_with_its_list_live() {
        let schema = include_str!("../../../plugins/config-schema.json");
        let config = crate::config::Config::default();
        let mut state = SettingsState::new(schema, &config).unwrap();
        state.visible = true;
        let mut ctx = InputContext::new();
        state.handle_key_event(&key(KeyCode::Char('/')), &mut ctx);
        for c in "lsp".chars() {
            state.handle_key_event(&key(KeyCode::Char(c)), &mut ctx);
        }
        state.handle_key_event(&key(KeyCode::Enter), &mut ctx);
        assert_eq!(
            state.current_item().map(|i| i.path.as_str()),
            Some("/lsp"),
            "the jump lands on the LSP map"
        );
        assert_eq!(state.composite_cursor(), Some(0));
        state.handle_key_event(&key(KeyCode::Down), &mut ctx);
        assert_eq!(state.composite_cursor(), Some(1), "Down walks the rows");
    }

    #[test]
    fn test_settings_is_modal() {
        // SettingsState should be modal - consume all unhandled input
        let schema = include_str!("../../../plugins/config-schema.json");
        let config = crate::config::Config::default();
        let state = SettingsState::new(schema, &config).unwrap();
        assert!(state.is_modal());
    }

    #[test]
    fn test_categories_panel_does_not_leak_to_settings() {
        let schema = include_str!("../../../plugins/config-schema.json");
        let config = crate::config::Config::default();
        let mut state = SettingsState::new(schema, &config).unwrap();
        state.visible = true;
        state.focus_on(FocusTarget::Categories);

        let mut ctx = InputContext::new();

        // Per the tree-view spec: only Tab switches panels. Enter,
        // Left, and Right are *no longer* shortcuts to move focus
        // out of the categories panel.
        // * Enter falls through (Ignored) — let the modal handle it.
        // * Right expands the focused category (no-op for non-
        //   expandable ones); does NOT move focus to Settings.
        // * Left collapses; same — does not switch panels.
        // * Tab is not the dispatcher's at all: it is the tree's ring
        //   (`view::shell::settings::keys`), and reaches here only when the
        //   ring could not serve it, where it is nothing.
        let result = state.handle_key_event(&key(KeyCode::Enter), &mut ctx);
        assert_eq!(result, InputResult::Ignored);
        assert_eq!(state.focus_panel(), FocusPanel::Categories);

        let result = state.handle_key_event(&key(KeyCode::Right), &mut ctx);
        assert_eq!(result, InputResult::Consumed);
        assert_eq!(state.focus_panel(), FocusPanel::Categories);

        let result = state.handle_key_event(&key(KeyCode::Left), &mut ctx);
        assert_eq!(result, InputResult::Consumed);
        assert_eq!(state.focus_panel(), FocusPanel::Categories);

        let result = state.handle_key_event(&key(KeyCode::Tab), &mut ctx);
        assert_eq!(result, InputResult::Ignored);
        assert_eq!(state.focus_panel(), FocusPanel::Categories);
    }

    #[test]
    fn test_escape_shows_confirm_dialog_with_changes() {
        let schema = include_str!("../../../plugins/config-schema.json");
        let config = crate::config::Config::default();
        let mut state = SettingsState::new(schema, &config).unwrap();
        state.visible = true;

        // Simulate a change
        state
            .pending_changes
            .insert("/test".to_string(), serde_json::json!(true));

        let mut ctx = InputContext::new();

        // Escape should show confirm dialog, not close directly
        state.handle_key_event(&key(KeyCode::Esc), &mut ctx);
        assert!(state.showing_confirm_dialog);
        assert!(ctx.deferred_actions.is_empty()); // No close action yet
    }

    #[test]
    fn test_escape_closes_directly_without_changes() {
        let schema = include_str!("../../../plugins/config-schema.json");
        let config = crate::config::Config::default();
        let mut state = SettingsState::new(schema, &config).unwrap();
        state.visible = true;

        let mut ctx = InputContext::new();

        // Escape without changes should defer close action
        state.handle_key_event(&key(KeyCode::Esc), &mut ctx);
        assert!(!state.showing_confirm_dialog);
        assert_eq!(ctx.deferred_actions.len(), 1);
        assert!(matches!(
            ctx.deferred_actions[0],
            DeferredAction::CloseSettings { save: false }
        ));
    }

    #[test]
    fn test_confirm_dialog_navigation() {
        let schema = include_str!("../../../plugins/config-schema.json");
        let config = crate::config::Config::default();
        let mut state = SettingsState::new(schema, &config).unwrap();
        state.visible = true;
        state.showing_confirm_dialog = true;
        state.confirm_dialog_selection = 0; // Save

        let mut ctx = InputContext::new();

        // Right -> Discard
        state.handle_key_event(&key(KeyCode::Right), &mut ctx);
        assert_eq!(state.confirm_dialog_selection, 1);

        // Right -> Cancel
        state.handle_key_event(&key(KeyCode::Right), &mut ctx);
        assert_eq!(state.confirm_dialog_selection, 2);

        // Right again -> stays at Cancel (no wrap)
        state.handle_key_event(&key(KeyCode::Right), &mut ctx);
        assert_eq!(state.confirm_dialog_selection, 2);

        // Left -> Discard
        state.handle_key_event(&key(KeyCode::Left), &mut ctx);
        assert_eq!(state.confirm_dialog_selection, 1);
    }

    #[test]
    fn test_search_mode_captures_typing() {
        let schema = include_str!("../../../plugins/config-schema.json");
        let config = crate::config::Config::default();
        let mut state = SettingsState::new(schema, &config).unwrap();
        state.visible = true;

        let mut ctx = InputContext::new();

        // Start search
        state.handle_key_event(&key(KeyCode::Char('/')), &mut ctx);
        assert!(state.search_active);

        // Type search query
        state.handle_key_event(&key(KeyCode::Char('t')), &mut ctx);
        state.handle_key_event(&key(KeyCode::Char('a')), &mut ctx);
        state.handle_key_event(&key(KeyCode::Char('b')), &mut ctx);
        assert_eq!(state.search_query(), "tab");

        // Escape cancels search
        state.handle_key_event(&key(KeyCode::Esc), &mut ctx);
        assert!(!state.search_active);
        assert!(state.search_query().is_empty());
    }

    /// The settings filter used to only append/backspace at the end: arrow
    /// keys did nothing within the text (unlike the Command Palette). It now
    /// tracks a cursor, so Left/Right/Home/End move the caret and edits land
    /// at the cursor — matching the palette.
    #[test]
    fn test_search_arrow_keys_edit_within_query() {
        let schema = include_str!("../../../plugins/config-schema.json");
        let config = crate::config::Config::default();
        let mut state = SettingsState::new(schema, &config).unwrap();
        state.visible = true;

        let mut ctx = InputContext::new();

        // Start search and type "theme"
        state.handle_key_event(&key(KeyCode::Char('/')), &mut ctx);
        for c in "theme".chars() {
            state.handle_key_event(&key(KeyCode::Char(c)), &mut ctx);
        }
        assert_eq!(state.search_query(), "theme");
        assert_eq!(state.search_cursor(), 5);

        // Left twice, then insert 'X' -> lands mid-string, not at the end
        state.handle_key_event(&key(KeyCode::Left), &mut ctx);
        state.handle_key_event(&key(KeyCode::Left), &mut ctx);
        assert_eq!(state.search_cursor(), 3);
        state.handle_key_event(&key(KeyCode::Char('X')), &mut ctx);
        assert_eq!(state.search_query(), "theXme");
        assert_eq!(state.search_cursor(), 4);

        // Home jumps to start; Backspace at start is a no-op
        state.handle_key_event(&key(KeyCode::Home), &mut ctx);
        assert_eq!(state.search_cursor(), 0);
        state.handle_key_event(&key(KeyCode::Backspace), &mut ctx);
        assert_eq!(state.search_query(), "theXme");

        // Delete removes the char at the cursor (the leading 't')
        state.handle_key_event(&key(KeyCode::Delete), &mut ctx);
        assert_eq!(state.search_query(), "heXme");
        assert_eq!(state.search_cursor(), 0);

        // End jumps to the end; Backspace removes the trailing char
        state.handle_key_event(&key(KeyCode::End), &mut ctx);
        assert_eq!(state.search_cursor(), state.search_query().len());
        state.handle_key_event(&key(KeyCode::Backspace), &mut ctx);
        assert_eq!(state.search_query(), "heXm");
    }

    /// Ctrl/Alt chords must not type their letter into the filter. The
    /// `Char` arm used to match regardless of modifiers, so Ctrl+A/C/V
    /// inserted a literal `a`/`c`/`v` instead of no-op'ing.
    #[test]
    fn test_search_ignores_ctrl_and_alt_chords() {
        let schema = include_str!("../../../plugins/config-schema.json");
        let config = crate::config::Config::default();
        let mut state = SettingsState::new(schema, &config).unwrap();
        state.visible = true;

        let mut ctx = InputContext::new();

        state.handle_key_event(&key(KeyCode::Char('/')), &mut ctx);
        for c in "hello".chars() {
            state.handle_key_event(&key(KeyCode::Char(c)), &mut ctx);
        }
        assert_eq!(state.search_query(), "hello");

        // Ctrl+A / Ctrl+C / Ctrl+V / Ctrl+X leave the query untouched
        for c in ['a', 'c', 'v', 'x'] {
            state.handle_key_event(
                &KeyEvent::new(KeyCode::Char(c), KeyModifiers::CONTROL),
                &mut ctx,
            );
        }
        // Alt chord too
        state.handle_key_event(
            &KeyEvent::new(KeyCode::Char('a'), KeyModifiers::ALT),
            &mut ctx,
        );
        assert_eq!(state.search_query(), "hello");

        // A plain char still types; Shift+char types uppercase
        state.handle_key_event(&key(KeyCode::Char('!')), &mut ctx);
        state.handle_key_event(
            &KeyEvent::new(KeyCode::Char('Z'), KeyModifiers::SHIFT),
            &mut ctx,
        );
        assert_eq!(state.search_query(), "hello!Z");
    }

    #[test]
    fn test_footer_button_activation() {
        let schema = include_str!("../../../plugins/config-schema.json");
        let config = crate::config::Config::default();
        let mut state = SettingsState::new(schema, &config).unwrap();
        state.visible = true;
        state.focus_on(FocusTarget::Footer(0));
        state.footer_button_index = 2; // Save button (0=Layer, 1=Reset, 2=Save, 3=Cancel)

        let mut ctx = InputContext::new();

        // Enter on Save button should defer save action
        state.handle_key_event(&key(KeyCode::Enter), &mut ctx);
        assert_eq!(ctx.deferred_actions.len(), 1);
        assert!(matches!(
            ctx.deferred_actions[0],
            DeferredAction::CloseSettings { save: true }
        ));
    }

    /// Reproducer for issue #1825: Tab while editing a Number control was a
    /// no-op, leaving the user "stuck" in the input. Tab should commit the
    /// pending edit and exit number-editing mode (matching the Text-control
    /// behavior).
    #[test]
    fn test_tab_exits_number_editing() {
        use crate::view::settings::items::SettingControl;

        let schema = include_str!("../../../plugins/config-schema.json");
        let config = crate::config::Config::default();
        let mut state = SettingsState::new(schema, &config).unwrap();
        state.visible = true;
        state.focus_on(FocusTarget::Card(0));

        // Find a number setting (any will do)
        let number_idx = state
            .pages
            .get(state.selected_category)
            .and_then(|page| {
                page.items
                    .iter()
                    .position(|item| matches!(item.control, SettingControl::Number { .. }))
            })
            .expect("expected at least one Number control on the default page");
        state.selected_item = number_idx;

        // Enter number editing mode and type a digit so we have a pending edit
        state.activate_control();
        assert!(
            state.is_number_editing(),
            "precondition: should be in number-editing mode"
        );
        let mut ctx = InputContext::new();
        state.handle_key_event(&key(KeyCode::Char('7')), &mut ctx);

        // Tab should exit editing mode (currently fails: Tab is unhandled)
        state.handle_key_event(&key(KeyCode::Tab), &mut ctx);
        assert!(
            !state.is_number_editing(),
            "Tab while editing a Number control must exit editing mode"
        );
    }

    /// A plain `Text` field in the main settings body (e.g. Terminal ▸
    /// Command) must honor Home, End, and forward-Delete. The body text
    /// handler previously had no `Home`/`End` arms — the caret never moved —
    /// and routed `Delete` to list-item removal, a no-op on a scalar field.
    /// Regression guard for both.
    #[test]
    fn test_body_text_field_home_end_and_forward_delete() {
        let schema = include_str!("../../../plugins/config-schema.json");
        let config = crate::config::Config::default();
        let mut state = SettingsState::new(schema, &config).unwrap();
        state.visible = true;
        state.focus_on(FocusTarget::Card(0));

        assert!(
            select_first_text_control(&mut state),
            "expected at least one editable Text control in the schema"
        );

        let mut ctx = InputContext::new();

        // Enter edit mode and type a known value. The first keystroke clears
        // any armed replace-on-type, so the field ends up holding "abcdef".
        state.start_editing();
        for c in "abcdef".chars() {
            state.handle_key_event(&key(KeyCode::Char(c)), &mut ctx);
        }
        assert_eq!(text_value(&state), "abcdef");

        // Home moves the caret to the start: the next char lands there.
        state.handle_key_event(&key(KeyCode::Home), &mut ctx);
        state.handle_key_event(&key(KeyCode::Char('X')), &mut ctx);
        assert_eq!(
            text_value(&state),
            "Xabcdef",
            "Home must move the caret to the start of the field"
        );

        // End moves the caret to the end: the next char appends.
        state.handle_key_event(&key(KeyCode::End), &mut ctx);
        state.handle_key_event(&key(KeyCode::Char('Z')), &mut ctx);
        assert_eq!(
            text_value(&state),
            "XabcdefZ",
            "End must move the caret to the end of the field"
        );

        // Delete forward-deletes: Left puts the caret before 'Z', Delete
        // removes the 'Z' at the caret (rather than no-op'ing).
        state.handle_key_event(&key(KeyCode::Left), &mut ctx);
        state.handle_key_event(&key(KeyCode::Delete), &mut ctx);
        assert_eq!(
            text_value(&state),
            "Xabcdef",
            "Delete must forward-delete the character at the caret"
        );
    }

    /// Build a `SettingsState` with an entry ("Edit Item") dialog open on a
    /// one-field object entry, that field focused and already in text-edit
    /// mode. The shape mirrors the dialog from issue #2875 — an LSP server's
    /// `Command` sitting beside its `Enabled` toggle.
    fn entry_dialog_editing_field(
        field_key: &str,
        label: &str,
        setting_type: super::super::schema::SettingType,
        default: serde_json::Value,
        entry_value: serde_json::Value,
        nullable: bool,
    ) -> SettingsState {
        use super::super::entry_dialog::EntryDialogState;
        use super::super::schema::{SettingSchema, SettingType};
        use std::collections::HashMap;

        let prop = SettingSchema {
            path: format!("/{field_key}"),
            name: label.to_string(),
            description: None,
            setting_type,
            default: Some(default),
            read_only: false,
            section: None,
            order: None,
            nullable,
            enum_from: None,
            dual_list_sibling: None,
            dynamically_extendable_status_bar_elements: false,
        };
        let schema = SettingSchema {
            path: "/test".to_string(),
            name: "Test".to_string(),
            description: None,
            setting_type: SettingType::Object {
                properties: vec![prop],
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

        let config_schema = include_str!("../../../plugins/config-schema.json");
        let config = crate::config::Config::default();
        let mut state = SettingsState::new(config_schema, &config).unwrap();

        let dialog = EntryDialogState::from_schema(
            "entry".to_string(),
            &entry_value,
            &schema,
            "/test",
            true, // new entry
            false,
            &HashMap::new(),
        );
        state.entry_dialog_stack.push(dialog);

        let dialog = state.entry_dialog_mut().expect("dialog present");
        let idx = dialog
            .items
            .iter()
            .position(|i| i.name == label)
            .expect("field present in dialog");
        dialog.selected_item = idx;
        dialog.start_editing();
        assert!(dialog.is_editing(), "precondition: editing {label}");
        state
    }

    /// The value of the entry dialog's focused plain `Text` control.
    fn dialog_text_value(state: &SettingsState) -> String {
        match state
            .entry_dialog()
            .and_then(|d| d.current_item())
            .map(|i| &i.control)
        {
            Some(SettingControl::Text { value, .. }) => value.clone(),
            _ => panic!("current dialog item is not a Text control"),
        }
    }

    /// The text sitting in the entry dialog's focused `Number` control's
    /// draft — the digits the user sees while the field is being typed into.
    fn dialog_number_edit_buffer(state: &SettingsState) -> String {
        let dialog = state.entry_dialog().expect("dialog present");
        let item = dialog.current_item().expect("a field is selected");
        assert!(
            matches!(item.control, SettingControl::Number { .. }),
            "current dialog item is not a Number control"
        );
        crate::widgets::kinds::number::resolve(
            0.0,
            None,
            None,
            Some(&item.path),
            &dialog.controls.instance_states,
        )
        .draft
        .expect("number control should be in edit mode")
        .text
    }

    /// The issue's own case: a dialog holding one editable `Command` string.
    fn string_field_dialog() -> SettingsState {
        use super::super::schema::SettingType;
        entry_dialog_editing_field(
            "command",
            "Command",
            SettingType::String,
            serde_json::json!(""),
            serde_json::json!({ "command": "" }),
            false,
        )
    }

    /// A plain `Text` field inside the entry ("Edit Item") dialog must honor
    /// forward-Delete. The dialog's text-editing handler previously routed
    /// Delete to list-item removal for every non-JSON control, so a scalar
    /// Text field's Delete key was dead even though Backspace worked
    /// (issue #2875). Regression guard.
    #[test]
    fn test_entry_dialog_text_field_forward_delete() {
        let mut state = string_field_dialog();
        let mut ctx = InputContext::new();

        for c in "abc".chars() {
            state.handle_key_event(&key(KeyCode::Char(c)), &mut ctx);
        }
        assert_eq!(dialog_text_value(&state), "abc");

        // Home moves the caret to the start; Delete removes the char at the
        // caret. Before the fix this routed to list-item removal — a no-op on
        // a scalar Text field — leaving the value untouched.
        state.handle_key_event(&key(KeyCode::Home), &mut ctx);
        state.handle_key_event(&key(KeyCode::Delete), &mut ctx);
        assert_eq!(
            dialog_text_value(&state),
            "bc",
            "Delete must forward-delete the character at the caret in the entry dialog"
        );
    }

    /// Forward-Delete removes one *grapheme cluster*, not one byte and not
    /// one `char`. The field holds `a`, a decomposed `e` + combining acute,
    /// and a three-byte `漢`, so each of the three properties is separable:
    ///
    /// * a byte-indexed delete would split the combining mark or `漢` and
    ///   panic on the resulting non-boundary — ad-hoc slicing at sites like
    ///   this is a known recurring source of multibyte crashes;
    /// * a char-indexed delete would survive but leave the orphaned
    ///   combining mark behind, so the caret would need two presses to clear
    ///   one visible character;
    /// * only a cluster-indexed delete clears `e` + mark in a single press.
    ///
    /// A precomposed `é` cannot tell these apart — it is one byte sequence,
    /// one `char`, and one cluster at once — so the decomposed form is what
    /// makes this test load-bearing.
    #[test]
    fn test_entry_dialog_text_field_forward_delete_multibyte() {
        let mut state = string_field_dialog();
        let mut ctx = InputContext::new();

        // "ae\u{301}漢": 'a', then 'e' + COMBINING ACUTE ACCENT (one cluster,
        // two chars, three bytes), then a three-byte ideograph.
        for c in "ae\u{301}漢".chars() {
            state.handle_key_event(&key(KeyCode::Char(c)), &mut ctx);
        }
        assert_eq!(dialog_text_value(&state), "ae\u{301}漢");

        state.handle_key_event(&key(KeyCode::Home), &mut ctx);
        state.handle_key_event(&key(KeyCode::Delete), &mut ctx);
        assert_eq!(
            dialog_text_value(&state),
            "e\u{301}漢",
            "Delete must remove exactly the one-byte 'a' at the caret"
        );

        state.handle_key_event(&key(KeyCode::Delete), &mut ctx);
        assert_eq!(
            dialog_text_value(&state),
            "漢",
            "one Delete must clear the whole 'e' + combining-mark cluster, \
             not just its base character"
        );

        state.handle_key_event(&key(KeyCode::Delete), &mut ctx);
        assert_eq!(
            dialog_text_value(&state),
            "",
            "Delete must remove all three bytes of the ideograph at the caret"
        );
    }

    /// A `Command` field pre-filled with `text`, caret at the end. Each chord
    /// below gets its own dialog so the cases stay independent and readable.
    fn typed_string_field_dialog(text: &str, ctx: &mut InputContext) -> SettingsState {
        let mut state = string_field_dialog();
        for c in text.chars() {
            state.handle_key_event(&key(KeyCode::Char(c)), ctx);
        }
        assert_eq!(dialog_text_value(&state), text, "precondition: field typed");
        state
    }

    /// Routing the dialog's plain Text fields through the shared text-key
    /// table hands them the chords the dialog used to swallow. The table's own
    /// semantics are covered in `primitives::text_key` against a raw editor;
    /// what is pinned here is that the *entry dialog* reaches it, which is the
    /// seam issue #2875 was about. Every chord the change advertises is
    /// exercised, because a table that works and a surface that never calls it
    /// look identical from the outside.
    #[test]
    fn test_entry_dialog_text_field_word_and_selection_keys() {
        let mut ctx = InputContext::new();

        // Ctrl+Backspace deletes the word before the caret.
        let mut state = typed_string_field_dialog("foo bar", &mut ctx);
        state.handle_key_event(
            &KeyEvent::new(KeyCode::Backspace, KeyModifiers::CONTROL),
            &mut ctx,
        );
        assert_eq!(
            dialog_text_value(&state),
            "foo ",
            "Ctrl+Backspace must delete the word before the caret"
        );

        // Ctrl+Delete deletes the word at the caret.
        let mut state = typed_string_field_dialog("foo bar", &mut ctx);
        state.handle_key_event(&key(KeyCode::Home), &mut ctx);
        state.handle_key_event(
            &KeyEvent::new(KeyCode::Delete, KeyModifiers::CONTROL),
            &mut ctx,
        );
        assert_eq!(
            dialog_text_value(&state),
            " bar",
            "Ctrl+Delete must delete the word at the caret"
        );

        // Ctrl+Left parks the caret at the start of the preceding word, so
        // the next character lands there.
        let mut state = typed_string_field_dialog("foo bar baz", &mut ctx);
        state.handle_key_event(
            &KeyEvent::new(KeyCode::Left, KeyModifiers::CONTROL),
            &mut ctx,
        );
        state.handle_key_event(&key(KeyCode::Char('|')), &mut ctx);
        assert_eq!(
            dialog_text_value(&state),
            "foo bar |baz",
            "Ctrl+Left must move the caret one word left"
        );

        // Ctrl+Right parks it at the end of the following word.
        let mut state = typed_string_field_dialog("alpha beta", &mut ctx);
        state.handle_key_event(&key(KeyCode::Home), &mut ctx);
        state.handle_key_event(
            &KeyEvent::new(KeyCode::Right, KeyModifiers::CONTROL),
            &mut ctx,
        );
        state.handle_key_event(&key(KeyCode::Char('|')), &mut ctx);
        assert_eq!(
            dialog_text_value(&state),
            "alpha| beta",
            "Ctrl+Right must move the caret one word right"
        );

        // Shift+Home selects back to the start; the next character replaces
        // the selection rather than being appended to it.
        let mut state = typed_string_field_dialog("foo bar", &mut ctx);
        state.handle_key_event(&KeyEvent::new(KeyCode::Home, KeyModifiers::SHIFT), &mut ctx);
        state.handle_key_event(&key(KeyCode::Char('x')), &mut ctx);
        assert_eq!(
            dialog_text_value(&state),
            "x",
            "Shift+Home must select to the start of the field"
        );

        // Shift+End selects forward to the end, and is likewise replaced.
        let mut state = typed_string_field_dialog("foo bar", &mut ctx);
        state.handle_key_event(&key(KeyCode::Home), &mut ctx);
        state.handle_key_event(&KeyEvent::new(KeyCode::End, KeyModifiers::SHIFT), &mut ctx);
        state.handle_key_event(&key(KeyCode::Char('x')), &mut ctx);
        assert_eq!(
            dialog_text_value(&state),
            "x",
            "Shift+End must select to the end of the field"
        );
    }

    /// The same dead-key bug outlived the Text fix in the dialog's `Number`
    /// fields: `start_editing` puts a Number control into text-edit mode, but
    /// `delete`/`cursor_home`/`cursor_end`/`cursor_left`/`cursor_right` had no
    /// `Number` arm, so every one of those keys was a silent no-op while the
    /// field was being typed into.
    #[test]
    fn test_entry_dialog_number_field_caret_keys() {
        use super::super::schema::SettingType;

        let mut state = entry_dialog_editing_field(
            "tab_size",
            "Tab Size",
            SettingType::Integer {
                minimum: None,
                maximum: None,
            },
            serde_json::json!(4),
            serde_json::json!({ "tab_size": 42 }),
            false,
        );
        let mut ctx = InputContext::new();
        assert_eq!(dialog_number_edit_buffer(&state), "42");

        // Home clears the select-all that start_editing arms and parks the
        // caret before the '4'; Delete removes it.
        state.handle_key_event(&key(KeyCode::Home), &mut ctx);
        state.handle_key_event(&key(KeyCode::Delete), &mut ctx);
        assert_eq!(
            dialog_number_edit_buffer(&state),
            "2",
            "Delete must forward-delete the digit at the caret in a Number field"
        );

        // The caret stayed at the start, so the next digit lands there.
        state.handle_key_event(&key(KeyCode::Char('7')), &mut ctx);
        assert_eq!(dialog_number_edit_buffer(&state), "72");

        // End parks the caret after the '2'; Left steps back over it so the
        // following Delete has something to remove.
        state.handle_key_event(&key(KeyCode::End), &mut ctx);
        state.handle_key_event(&key(KeyCode::Left), &mut ctx);
        state.handle_key_event(&key(KeyCode::Delete), &mut ctx);
        assert_eq!(
            dialog_number_edit_buffer(&state),
            "7",
            "End/Left must move the caret in a Number field so Delete lands on '2'"
        );

        // Home parks the caret before the '7'; Right steps over it, so the
        // next digit appends instead of prepending.
        state.handle_key_event(&key(KeyCode::Home), &mut ctx);
        state.handle_key_event(&key(KeyCode::Right), &mut ctx);
        state.handle_key_event(&key(KeyCode::Char('9')), &mut ctx);
        assert_eq!(
            dialog_number_edit_buffer(&state),
            "79",
            "Right must move the caret in a Number field"
        );
    }

    /// Marking a field edited does more than flip the dialog's dirty bit: it
    /// also clears the field's inherited/null state, so the field stops
    /// falling back to the parent value and starts being persisted with one
    /// of its own. That is why the shared-table seam keys off the value
    /// changing rather than off the key being handled — a user who tabs into
    /// an inherited field and only moves the caret must leave it inherited,
    /// and must not be asked to discard changes on the way out.
    ///
    /// The dirty bit and the inherited flag are model state with no direct
    /// rendering of their own, so this is a unit test on the component.
    #[test]
    fn test_entry_dialog_caret_motion_leaves_an_inherited_field_alone() {
        use super::super::schema::SettingType;

        let mut state = entry_dialog_editing_field(
            "command",
            "Command",
            SettingType::String,
            serde_json::json!(null),
            serde_json::json!({ "command": null }),
            true, // nullable: the field opens inherited (unset)
        );
        let mut ctx = InputContext::new();

        let inherited = |state: &SettingsState| -> bool {
            let dialog = state.entry_dialog().expect("dialog present");
            dialog.items[dialog.selected_item].is_null
        };
        let dirty = |state: &SettingsState| -> bool {
            state.entry_dialog().expect("dialog present").is_dirty()
        };
        assert!(inherited(&state), "precondition: field opens inherited");
        assert!(!dirty(&state), "precondition: dialog opens clean");

        // Every caret and selection key the shared table handles. None of
        // them changes the value, so none may touch either flag.
        for event in [
            key(KeyCode::Home),
            key(KeyCode::End),
            key(KeyCode::Left),
            key(KeyCode::Right),
            KeyEvent::new(KeyCode::Left, KeyModifiers::CONTROL),
            KeyEvent::new(KeyCode::Right, KeyModifiers::CONTROL),
            KeyEvent::new(KeyCode::Home, KeyModifiers::SHIFT),
            KeyEvent::new(KeyCode::End, KeyModifiers::SHIFT),
        ] {
            state.handle_key_event(&event, &mut ctx);
        }
        assert!(
            inherited(&state),
            "caret motion must not clear the field's inherited state"
        );
        assert!(
            !dirty(&state),
            "caret motion must not mark the dialog modified"
        );

        // A keystroke that does change the value still does both.
        state.handle_key_event(&key(KeyCode::Char('p')), &mut ctx);
        assert_eq!(dialog_text_value(&state), "p");
        assert!(
            !inherited(&state),
            "typing must give the field a value of its own"
        );
        assert!(dirty(&state), "typing must mark the dialog modified");
    }
}
