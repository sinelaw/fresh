//! Keybinding Editor
//!
//! A modal dialog for browsing, searching, and editing keybindings.
//! Provides a table view of all resolved bindings with search, filter,
//! key recording, conflict detection, and keymap management.

use crate::config::{Config, KeyPress, Keybinding};
use crate::input::keybindings::{format_keybinding, Action, KeybindingResolver};
use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use ratatui::layout::Rect;
use rust_i18n::t;
use std::collections::HashMap;

/// Where a binding comes from
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BindingSource {
    /// From the active keymap (built-in or named map)
    Keymap,
    /// User custom override (in config keybindings array)
    Custom,
}

/// A single resolved keybinding entry for display
#[derive(Debug, Clone)]
pub struct ResolvedBinding {
    /// Formatted key combination for display (e.g., "Ctrl+S")
    pub key_display: String,
    /// Action name (machine-readable, e.g., "save")
    pub action: String,
    /// Human-readable action description (e.g., "Save")
    pub action_display: String,
    /// Context / when clause (e.g., "normal", "global")
    pub context: String,
    /// Where this binding comes from
    pub source: BindingSource,
    /// The raw key code
    pub key_code: KeyCode,
    /// The raw modifiers
    pub modifiers: KeyModifiers,
    /// Whether this is a chord (multi-key) binding
    pub is_chord: bool,
}

/// Mode for the edit/add dialog
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EditMode {
    /// Recording the key combination
    RecordingKey,
    /// Editing the action name
    EditingAction,
    /// Selecting the context
    EditingContext,
}

/// State for the add/edit binding dialog
#[derive(Debug, Clone)]
pub struct EditBindingState {
    /// The mode of the edit dialog
    pub mode: EditMode,
    /// The recorded key code (if any)
    pub key_code: Option<KeyCode>,
    /// The recorded modifiers
    pub modifiers: KeyModifiers,
    /// The formatted key display
    pub key_display: String,
    /// The action name being edited
    pub action_text: String,
    /// Cursor position within action_text
    pub action_cursor: usize,
    /// The selected context
    pub context: String,
    /// Index of binding being edited (None = adding new)
    pub editing_index: Option<usize>,
    /// Detected conflicts
    pub conflicts: Vec<String>,
    /// Available context options
    pub context_options: Vec<String>,
    /// Selected context option index
    pub context_option_index: usize,
    /// Whether the context dropdown is open
    pub context_dropdown_open: bool,
    /// Selected button (0=Save, 1=Cancel)
    pub selected_button: usize,
    /// Focus area (0=key, 1=action, 2=context, 3=buttons)
    pub focus_area: usize,
    /// Filtered autocomplete suggestions for action name
    pub autocomplete_suggestions: Vec<String>,
    /// Selected index in autocomplete suggestions (-1 = none)
    pub autocomplete_selected: Option<usize>,
    /// Whether the autocomplete popup is visible
    pub autocomplete_visible: bool,
    /// Error message for invalid action name (shown when trying to save)
    pub action_error: Option<String>,
}

impl EditBindingState {
    pub fn new_add() -> Self {
        Self {
            mode: EditMode::RecordingKey,
            key_code: None,
            modifiers: KeyModifiers::NONE,
            key_display: String::new(),
            action_text: String::new(),
            action_cursor: 0,
            context: "normal".to_string(),
            editing_index: None,
            conflicts: Vec::new(),
            context_options: vec![
                "global".to_string(),
                "normal".to_string(),
                "prompt".to_string(),
                "popup".to_string(),
                "file_explorer".to_string(),
                "menu".to_string(),
                "terminal".to_string(),
            ],
            context_option_index: 1, // default to "normal"
            context_dropdown_open: false,
            selected_button: 0,
            focus_area: 0,
            autocomplete_suggestions: Vec::new(),
            autocomplete_selected: None,
            autocomplete_visible: false,
            action_error: None,
        }
    }

    pub fn new_edit(index: usize, binding: &ResolvedBinding) -> Self {
        let context_options = vec![
            "global".to_string(),
            "normal".to_string(),
            "prompt".to_string(),
            "popup".to_string(),
            "file_explorer".to_string(),
            "menu".to_string(),
            "terminal".to_string(),
        ];
        let context_option_index = context_options
            .iter()
            .position(|c| c == &binding.context)
            .unwrap_or(1);

        Self {
            mode: EditMode::RecordingKey,
            key_code: Some(binding.key_code),
            modifiers: binding.modifiers,
            key_display: binding.key_display.clone(),
            action_text: binding.action.clone(),
            action_cursor: binding.action.len(),
            context: binding.context.clone(),
            editing_index: Some(index),
            conflicts: Vec::new(),
            context_options,
            context_option_index,
            context_dropdown_open: false,
            selected_button: 0,
            focus_area: 0,
            autocomplete_suggestions: Vec::new(),
            autocomplete_selected: None,
            autocomplete_visible: false,
            action_error: None,
        }
    }
}

/// Search mode for the keybinding editor
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SearchMode {
    /// Search by typing action/key name
    Text,
    /// Search by recording a key combination
    RecordKey,
}

/// Context filter options
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ContextFilter {
    All,
    Specific(String),
}

/// Source filter options
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SourceFilter {
    All,
    KeymapOnly,
    CustomOnly,
}

/// Layout information for mouse hit testing
#[derive(Debug, Clone, Default)]
pub struct KeybindingEditorLayout {
    /// The full modal area (all mouse events inside are captured)
    pub modal_area: Rect,
    /// The table area (for scroll and click)
    pub table_area: Rect,
    /// The y-offset of the first visible row in the table
    pub table_first_row_y: u16,
    /// Edit dialog button areas: (save_rect, cancel_rect)
    pub dialog_buttons: Option<(Rect, Rect)>,
    /// Edit dialog key field area
    pub dialog_key_field: Option<Rect>,
    /// Edit dialog action field area
    pub dialog_action_field: Option<Rect>,
    /// Edit dialog context field area
    pub dialog_context_field: Option<Rect>,
    /// Confirm dialog button areas: (save, discard, cancel)
    pub confirm_buttons: Option<(Rect, Rect, Rect)>,
    /// Search bar area (for clicking to focus)
    pub search_bar: Option<Rect>,
}

/// The main keybinding editor state
#[derive(Debug)]
pub struct KeybindingEditor {
    /// All resolved bindings
    pub bindings: Vec<ResolvedBinding>,
    /// Indices into `bindings` after filtering/searching
    pub filtered_indices: Vec<usize>,
    /// Currently selected index (within filtered list)
    pub selected: usize,
    /// Scroll state (offset, viewport, content_height) — shared with render
    pub scroll: crate::view::ui::ScrollState,

    /// Whether search is active (search bar visible)
    pub search_active: bool,
    /// Whether search input is focused (accepting keystrokes)
    pub search_focused: bool,
    /// Search query string
    pub search_query: String,
    /// Search mode (text or record key)
    pub search_mode: SearchMode,
    /// Recorded search key display (when in RecordKey mode)
    pub search_key_display: String,
    /// Recorded search key code (when in RecordKey mode)
    pub search_key_code: Option<KeyCode>,
    /// Recorded search modifiers (when in RecordKey mode)
    pub search_modifiers: KeyModifiers,

    /// Context filter
    pub context_filter: ContextFilter,
    /// Source filter
    pub source_filter: SourceFilter,

    /// Edit/add binding dialog state (None = not open)
    pub edit_dialog: Option<EditBindingState>,

    /// Whether help overlay is showing
    pub showing_help: bool,

    /// Active keymap name
    pub active_keymap: String,
    /// Config file path for display
    pub config_file_path: String,

    /// Custom bindings that have been added (pending save)
    pub pending_adds: Vec<Keybinding>,
    /// Indices of custom bindings to remove (pending save)
    pub pending_removes: Vec<usize>,
    /// Whether there are unsaved changes
    pub has_changes: bool,

    /// Showing unsaved changes confirmation dialog
    pub showing_confirm_dialog: bool,
    /// Selected button in confirm dialog (0=Save, 1=Discard, 2=Cancel)
    pub confirm_selection: usize,

    /// Named keymaps info for display
    pub keymap_names: Vec<String>,

    /// Available action names (for autocomplete)
    pub available_actions: Vec<String>,

    /// Layout info for mouse hit testing (updated during render)
    pub layout: KeybindingEditorLayout,
}

impl KeybindingEditor {
    /// Create a new keybinding editor from config and resolver
    pub fn new(config: &Config, resolver: &KeybindingResolver, config_file_path: String) -> Self {
        let bindings = Self::resolve_all_bindings(config, resolver);
        let filtered_indices: Vec<usize> = (0..bindings.len()).collect();

        // Collect available action names
        let available_actions = Self::collect_action_names();

        // Collect keymap names
        let mut keymap_names: Vec<String> = config.keybinding_maps.keys().cloned().collect();
        keymap_names.sort();

        let mut editor = Self {
            bindings,
            filtered_indices,
            selected: 0,
            scroll: crate::view::ui::ScrollState::default(),
            search_active: false,
            search_focused: false,
            search_query: String::new(),
            search_mode: SearchMode::Text,
            search_key_display: String::new(),
            search_key_code: None,
            search_modifiers: KeyModifiers::NONE,
            context_filter: ContextFilter::All,
            source_filter: SourceFilter::All,
            edit_dialog: None,
            showing_help: false,
            active_keymap: config.active_keybinding_map.to_string(),
            config_file_path,
            pending_adds: Vec::new(),
            pending_removes: Vec::new(),
            has_changes: false,
            showing_confirm_dialog: false,
            confirm_selection: 0,
            keymap_names,
            available_actions,
            layout: KeybindingEditorLayout::default(),
        };

        editor.apply_filters();
        editor
    }

    /// Resolve all bindings from the active keymap + custom overrides
    fn resolve_all_bindings(
        config: &Config,
        resolver: &KeybindingResolver,
    ) -> Vec<ResolvedBinding> {
        let mut bindings = Vec::new();
        let mut seen: HashMap<(String, String), usize> = HashMap::new(); // (key_display, context) -> index

        // First, load bindings from the active keymap
        let map_bindings = config.resolve_keymap(&config.active_keybinding_map);
        for kb in &map_bindings {
            if let Some(entry) = Self::keybinding_to_resolved(kb, BindingSource::Keymap, resolver) {
                let key = (entry.key_display.clone(), entry.context.clone());
                let idx = bindings.len();
                seen.insert(key, idx);
                bindings.push(entry);
            }
        }

        // Then, load custom bindings (these override keymap bindings)
        for kb in &config.keybindings {
            if let Some(entry) = Self::keybinding_to_resolved(kb, BindingSource::Custom, resolver) {
                let key = (entry.key_display.clone(), entry.context.clone());
                if let Some(&existing_idx) = seen.get(&key) {
                    // Override the existing binding
                    bindings[existing_idx] = entry;
                } else {
                    let idx = bindings.len();
                    seen.insert(key, idx);
                    bindings.push(entry);
                }
            }
        }

        // Sort by context, then by action name
        bindings.sort_by(|a, b| {
            a.context
                .cmp(&b.context)
                .then(a.action_display.cmp(&b.action_display))
        });

        bindings
    }

    /// Convert a Keybinding config entry to a ResolvedBinding
    fn keybinding_to_resolved(
        kb: &Keybinding,
        source: BindingSource,
        resolver: &KeybindingResolver,
    ) -> Option<ResolvedBinding> {
        let context = kb.when.as_deref().unwrap_or("normal").to_string();

        if !kb.keys.is_empty() {
            // Chord binding
            let key_display = format_chord_keys(&kb.keys);
            let action_display = KeybindingResolver::format_action_from_str(&kb.action);
            Some(ResolvedBinding {
                key_display,
                action: kb.action.clone(),
                action_display,
                context,
                source,
                key_code: KeyCode::Null,
                modifiers: KeyModifiers::NONE,
                is_chord: true,
            })
        } else if !kb.key.is_empty() {
            // Single key binding
            let key_code = KeybindingResolver::parse_key_public(&kb.key)?;
            let modifiers = KeybindingResolver::parse_modifiers_public(&kb.modifiers);
            let key_display = format_keybinding(&key_code, &modifiers);
            let action_display = KeybindingResolver::format_action_from_str(&kb.action);
            Some(ResolvedBinding {
                key_display,
                action: kb.action.clone(),
                action_display,
                context,
                source,
                key_code,
                modifiers,
                is_chord: false,
            })
        } else {
            None
        }
    }

    /// Collect all available action names (delegates to the macro-generated source of truth)
    fn collect_action_names() -> Vec<String> {
        Action::all_action_names()
    }

    /// Update autocomplete suggestions based on current action text
    pub fn update_autocomplete(&mut self) {
        if let Some(ref mut dialog) = self.edit_dialog {
            let query = dialog.action_text.to_lowercase();
            if query.is_empty() {
                dialog.autocomplete_suggestions.clear();
                dialog.autocomplete_visible = false;
                dialog.autocomplete_selected = None;
                return;
            }

            dialog.autocomplete_suggestions = self
                .available_actions
                .iter()
                .filter(|a| a.to_lowercase().contains(&query))
                .cloned()
                .collect();

            // Sort: exact prefix matches first, then contains matches
            let q = query.clone();
            dialog.autocomplete_suggestions.sort_by(|a, b| {
                let a_prefix = a.to_lowercase().starts_with(&q);
                let b_prefix = b.to_lowercase().starts_with(&q);
                match (a_prefix, b_prefix) {
                    (true, false) => std::cmp::Ordering::Less,
                    (false, true) => std::cmp::Ordering::Greater,
                    _ => a.cmp(b),
                }
            });

            dialog.autocomplete_visible = !dialog.autocomplete_suggestions.is_empty();
            // Reset selection when text changes
            dialog.autocomplete_selected = if dialog.autocomplete_visible {
                Some(0)
            } else {
                None
            };
            // Clear any previous error
            dialog.action_error = None;
        }
    }

    /// Check if the given action name is valid
    pub fn is_valid_action(&self, action_name: &str) -> bool {
        self.available_actions.iter().any(|a| a == action_name)
    }

    /// Apply current search and filter criteria
    pub fn apply_filters(&mut self) {
        self.filtered_indices.clear();

        for (i, binding) in self.bindings.iter().enumerate() {
            // Apply context filter
            if let ContextFilter::Specific(ref ctx) = self.context_filter {
                if &binding.context != ctx {
                    continue;
                }
            }

            // Apply source filter
            match self.source_filter {
                SourceFilter::KeymapOnly if binding.source != BindingSource::Keymap => continue,
                SourceFilter::CustomOnly if binding.source != BindingSource::Custom => continue,
                _ => {}
            }

            // Apply search
            if self.search_active {
                match self.search_mode {
                    SearchMode::Text => {
                        if !self.search_query.is_empty() {
                            let query = self.search_query.to_lowercase();
                            let matches = binding.action.to_lowercase().contains(&query)
                                || binding.action_display.to_lowercase().contains(&query)
                                || binding.key_display.to_lowercase().contains(&query)
                                || binding.context.to_lowercase().contains(&query);
                            if !matches {
                                continue;
                            }
                        }
                    }
                    SearchMode::RecordKey => {
                        if let Some(search_key) = self.search_key_code {
                            if !binding.is_chord {
                                let key_matches = binding.key_code == search_key
                                    && binding.modifiers == self.search_modifiers;
                                if !key_matches {
                                    continue;
                                }
                            } else {
                                continue; // Skip chords in key search mode
                            }
                        }
                    }
                }
            }

            self.filtered_indices.push(i);
        }

        // Reset selection if it's out of bounds
        if self.selected >= self.filtered_indices.len() {
            self.selected = self.filtered_indices.len().saturating_sub(1);
        }
        self.ensure_visible();
    }

    /// Get the currently selected binding
    pub fn selected_binding(&self) -> Option<&ResolvedBinding> {
        self.filtered_indices
            .get(self.selected)
            .and_then(|&i| self.bindings.get(i))
    }

    /// Move selection up
    pub fn select_prev(&mut self) {
        if self.selected > 0 {
            self.selected -= 1;
            self.ensure_visible();
        }
    }

    /// Move selection down
    pub fn select_next(&mut self) {
        if self.selected + 1 < self.filtered_indices.len() {
            self.selected += 1;
            self.ensure_visible();
        }
    }

    /// Page up
    pub fn page_up(&mut self) {
        let page = self.scroll.viewport as usize;
        if self.selected > page {
            self.selected -= page;
        } else {
            self.selected = 0;
        }
        self.ensure_visible();
    }

    /// Page down
    pub fn page_down(&mut self) {
        let page = self.scroll.viewport as usize;
        self.selected = (self.selected + page).min(self.filtered_indices.len().saturating_sub(1));
        self.ensure_visible();
    }

    /// Ensure the selected item is visible (public version)
    pub fn ensure_visible_public(&mut self) {
        self.ensure_visible();
    }

    /// Ensure the selected item is visible
    fn ensure_visible(&mut self) {
        self.scroll.ensure_visible(self.selected as u16, 1);
    }

    /// Start text search (preserves existing query when re-focusing)
    pub fn start_search(&mut self) {
        if !self.search_active || self.search_mode != SearchMode::Text {
            // Starting fresh or switching from record mode
            self.search_mode = SearchMode::Text;
            if !self.search_active {
                self.search_query.clear();
            }
        }
        self.search_active = true;
        self.search_focused = true;
    }

    /// Start record-key search
    pub fn start_record_key_search(&mut self) {
        self.search_active = true;
        self.search_focused = true;
        self.search_mode = SearchMode::RecordKey;
        self.search_key_display.clear();
        self.search_key_code = None;
        self.search_modifiers = KeyModifiers::NONE;
    }

    /// Cancel search (clear everything)
    pub fn cancel_search(&mut self) {
        self.search_active = false;
        self.search_focused = false;
        self.search_query.clear();
        self.search_key_code = None;
        self.search_key_display.clear();
        self.apply_filters();
    }

    /// Record a search key
    pub fn record_search_key(&mut self, event: &KeyEvent) {
        self.search_key_code = Some(event.code);
        self.search_modifiers = event.modifiers;
        self.search_key_display = format_keybinding(&event.code, &event.modifiers);
        self.apply_filters();
    }

    /// Cycle context filter
    pub fn cycle_context_filter(&mut self) {
        let contexts = vec![
            ContextFilter::All,
            ContextFilter::Specific("global".to_string()),
            ContextFilter::Specific("normal".to_string()),
            ContextFilter::Specific("prompt".to_string()),
            ContextFilter::Specific("popup".to_string()),
            ContextFilter::Specific("file_explorer".to_string()),
            ContextFilter::Specific("menu".to_string()),
            ContextFilter::Specific("terminal".to_string()),
        ];

        let current_idx = contexts
            .iter()
            .position(|c| c == &self.context_filter)
            .unwrap_or(0);
        let next_idx = (current_idx + 1) % contexts.len();
        self.context_filter = contexts.into_iter().nth(next_idx).unwrap();
        self.apply_filters();
    }

    /// Cycle source filter
    pub fn cycle_source_filter(&mut self) {
        self.source_filter = match self.source_filter {
            SourceFilter::All => SourceFilter::CustomOnly,
            SourceFilter::CustomOnly => SourceFilter::KeymapOnly,
            SourceFilter::KeymapOnly => SourceFilter::All,
        };
        self.apply_filters();
    }

    /// Open the add binding dialog
    pub fn open_add_dialog(&mut self) {
        self.edit_dialog = Some(EditBindingState::new_add());
    }

    /// Open the edit binding dialog for the selected binding
    pub fn open_edit_dialog(&mut self) {
        if let Some(binding) = self.selected_binding().cloned() {
            let idx = self.filtered_indices[self.selected];
            self.edit_dialog = Some(EditBindingState::new_edit(idx, &binding));
        }
    }

    /// Close the edit dialog
    pub fn close_edit_dialog(&mut self) {
        self.edit_dialog = None;
    }

    /// Delete the selected custom binding
    pub fn delete_selected(&mut self) -> bool {
        if let Some(&idx) = self.filtered_indices.get(self.selected) {
            if self.bindings[idx].source == BindingSource::Custom {
                self.pending_removes.push(idx);
                self.bindings.remove(idx);
                self.has_changes = true;
                self.apply_filters();
                return true;
            }
        }
        false
    }

    /// Apply the edit dialog to create/update a binding.
    /// Returns an error message if validation fails.
    pub fn apply_edit_dialog(&mut self) -> Option<String> {
        let dialog = match self.edit_dialog.take() {
            Some(d) => d,
            None => return None,
        };

        if dialog.key_code.is_none() || dialog.action_text.is_empty() {
            self.edit_dialog = Some(dialog);
            return Some(t!("keybinding_editor.error_key_action_required").to_string());
        }

        // Validate the action name
        if !self.is_valid_action(&dialog.action_text) {
            let err_msg = t!(
                "keybinding_editor.error_unknown_action",
                action = &dialog.action_text
            )
            .to_string();
            let mut dialog = dialog;
            dialog.action_error = Some(
                t!(
                    "keybinding_editor.error_unknown_action_short",
                    action = &dialog.action_text
                )
                .to_string(),
            );
            self.edit_dialog = Some(dialog);
            return Some(err_msg);
        }

        let key_code = dialog.key_code.unwrap();
        let modifiers = dialog.modifiers;
        let key_name = key_code_to_config_name(key_code);
        let modifier_names = modifiers_to_config_names(modifiers);

        let new_binding = Keybinding {
            key: key_name,
            modifiers: modifier_names,
            keys: Vec::new(),
            action: dialog.action_text.clone(),
            args: HashMap::new(),
            when: Some(dialog.context.clone()),
        };

        // Add as custom binding
        self.pending_adds.push(new_binding.clone());
        self.has_changes = true;

        // Update display
        let key_display = format_keybinding(&key_code, &modifiers);
        let action_display = KeybindingResolver::format_action_from_str(&dialog.action_text);

        let resolved = ResolvedBinding {
            key_display,
            action: dialog.action_text,
            action_display,
            context: dialog.context,
            source: BindingSource::Custom,
            key_code,
            modifiers,
            is_chord: false,
        };

        if let Some(edit_idx) = dialog.editing_index {
            // Editing existing - replace it
            if edit_idx < self.bindings.len() {
                self.bindings[edit_idx] = resolved;
            }
        } else {
            // Adding new
            self.bindings.push(resolved);
        }

        self.apply_filters();
        None
    }

    /// Check for conflicts with the given key combination
    pub fn find_conflicts(
        &self,
        key_code: KeyCode,
        modifiers: KeyModifiers,
        context: &str,
    ) -> Vec<String> {
        let mut conflicts = Vec::new();
        let key_display = format_keybinding(&key_code, &modifiers);

        for binding in &self.bindings {
            if !binding.is_chord
                && binding.key_code == key_code
                && binding.modifiers == modifiers
                && (binding.context == context
                    || binding.context == "global"
                    || context == "global")
            {
                conflicts.push(format!(
                    "{} ({}, {})",
                    binding.action_display,
                    binding.context,
                    if binding.source == BindingSource::Custom {
                        "custom"
                    } else {
                        "keymap"
                    }
                ));
            }
        }

        conflicts
    }

    /// Get the custom bindings to save to config
    pub fn get_custom_bindings(&self) -> Vec<Keybinding> {
        self.pending_adds.clone()
    }

    /// Get the context filter display string
    pub fn context_filter_display(&self) -> &str {
        match &self.context_filter {
            ContextFilter::All => "All",
            ContextFilter::Specific(ctx) => ctx.as_str(),
        }
    }

    /// Get the source filter display string
    pub fn source_filter_display(&self) -> &str {
        match &self.source_filter {
            SourceFilter::All => "All",
            SourceFilter::KeymapOnly => "Keymap",
            SourceFilter::CustomOnly => "Custom",
        }
    }
}

/// Format chord keys for display
fn format_chord_keys(keys: &[KeyPress]) -> String {
    keys.iter()
        .filter_map(|kp| {
            let key_code = KeybindingResolver::parse_key_public(&kp.key)?;
            let modifiers = KeybindingResolver::parse_modifiers_public(&kp.modifiers);
            Some(format_keybinding(&key_code, &modifiers))
        })
        .collect::<Vec<_>>()
        .join(" ")
}

/// Convert a KeyCode back to a config-friendly name
fn key_code_to_config_name(key_code: KeyCode) -> String {
    match key_code {
        KeyCode::Char(c) => c.to_string(),
        KeyCode::Enter => "Enter".to_string(),
        KeyCode::Tab => "Tab".to_string(),
        KeyCode::Backspace => "Backspace".to_string(),
        KeyCode::Delete => "Delete".to_string(),
        KeyCode::Esc => "Escape".to_string(),
        KeyCode::Up => "Up".to_string(),
        KeyCode::Down => "Down".to_string(),
        KeyCode::Left => "Left".to_string(),
        KeyCode::Right => "Right".to_string(),
        KeyCode::Home => "Home".to_string(),
        KeyCode::End => "End".to_string(),
        KeyCode::PageUp => "PageUp".to_string(),
        KeyCode::PageDown => "PageDown".to_string(),
        KeyCode::Insert => "Insert".to_string(),
        KeyCode::F(n) => format!("F{}", n),
        _ => format!("{:?}", key_code),
    }
}

/// Convert KeyModifiers back to config-friendly modifier names
fn modifiers_to_config_names(modifiers: KeyModifiers) -> Vec<String> {
    let mut names = Vec::new();
    if modifiers.contains(KeyModifiers::CONTROL) {
        names.push("ctrl".to_string());
    }
    if modifiers.contains(KeyModifiers::ALT) {
        names.push("alt".to_string());
    }
    if modifiers.contains(KeyModifiers::SHIFT) {
        names.push("shift".to_string());
    }
    if modifiers.contains(KeyModifiers::SUPER) {
        names.push("super".to_string());
    }
    names
}
