//! KeybindingEditor - the main editor state and logic.

use super::helpers::{format_chord_keys, key_code_to_config_name, modifiers_to_config_names};
use super::types::*;
use crate::config::{Config, Keybinding};
use crate::input::keybindings::{format_keybinding, Action, KeybindingResolver};
use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use rust_i18n::t;
use std::collections::HashMap;

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
    /// Custom bindings to remove from config (pending save)
    pub pending_removes: Vec<Keybinding>,
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

        // Add entries for actions that have no keybinding
        let bound_actions: std::collections::HashSet<String> =
            bindings.iter().map(|b| b.action.clone()).collect();
        for action_name in Action::all_action_names() {
            if !bound_actions.contains(&action_name) {
                let action_display = KeybindingResolver::format_action_from_str(&action_name);
                bindings.push(ResolvedBinding {
                    key_display: String::new(),
                    action: action_name,
                    action_display,
                    context: String::new(),
                    source: BindingSource::Unbound,
                    key_code: KeyCode::Null,
                    modifiers: KeyModifiers::NONE,
                    is_chord: false,
                });
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
        _resolver: &KeybindingResolver,
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

    /// Delete the selected binding.
    ///
    /// * **Custom** bindings are removed outright (tracked in `pending_removes`
    ///   or dropped from `pending_adds` when added in the same session).
    /// * **Keymap** bindings cannot be removed from the built-in map, so a
    ///   custom `noop` override is created for the same key, which shadows the
    ///   default binding in the resolver.
    ///
    /// Returns `DeleteResult` indicating what happened.
    pub fn delete_selected(&mut self) -> DeleteResult {
        let Some(&idx) = self.filtered_indices.get(self.selected) else {
            return DeleteResult::NothingSelected;
        };

        match self.bindings[idx].source {
            BindingSource::Custom => {
                let binding = &self.bindings[idx];
                let action_name = binding.action.clone();

                // Build config-level Keybinding for matching during save
                let config_kb = self.resolved_to_config_keybinding(binding);

                // If this binding was added in the current session, just
                // remove it from pending_adds. Otherwise track for removal
                // from the persisted config.
                let found_in_adds = self.pending_adds.iter().position(|kb| {
                    kb.action == config_kb.action
                        && kb.key == config_kb.key
                        && kb.modifiers == config_kb.modifiers
                        && kb.when == config_kb.when
                });
                if let Some(pos) = found_in_adds {
                    self.pending_adds.remove(pos);
                } else {
                    self.pending_removes.push(config_kb);
                }

                self.bindings.remove(idx);
                self.has_changes = true;

                // If no other binding exists for this action, re-add as unbound
                let still_bound = self.bindings.iter().any(|b| b.action == action_name);
                if !still_bound {
                    let action_display = KeybindingResolver::format_action_from_str(&action_name);
                    self.bindings.push(ResolvedBinding {
                        key_display: String::new(),
                        action: action_name,
                        action_display,
                        context: String::new(),
                        source: BindingSource::Unbound,
                        key_code: KeyCode::Null,
                        modifiers: KeyModifiers::NONE,
                        is_chord: false,
                    });
                }

                self.apply_filters();
                DeleteResult::CustomRemoved
            }
            BindingSource::Keymap => {
                let binding = &self.bindings[idx];
                let action_name = binding.action.clone();

                // Build a noop custom override for the same key+context
                let noop_kb = Keybinding {
                    key: if binding.is_chord {
                        String::new()
                    } else {
                        key_code_to_config_name(binding.key_code)
                    },
                    modifiers: if binding.is_chord {
                        Vec::new()
                    } else {
                        modifiers_to_config_names(binding.modifiers)
                    },
                    keys: Vec::new(),
                    action: "noop".to_string(),
                    args: HashMap::new(),
                    when: if binding.context.is_empty() {
                        None
                    } else {
                        Some(binding.context.clone())
                    },
                };
                self.pending_adds.push(noop_kb);

                // Replace the keymap entry with a noop custom entry in the display
                let noop_display = KeybindingResolver::format_action_from_str("noop");
                self.bindings[idx] = ResolvedBinding {
                    key_display: self.bindings[idx].key_display.clone(),
                    action: "noop".to_string(),
                    action_display: noop_display,
                    context: self.bindings[idx].context.clone(),
                    source: BindingSource::Custom,
                    key_code: self.bindings[idx].key_code,
                    modifiers: self.bindings[idx].modifiers,
                    is_chord: self.bindings[idx].is_chord,
                };
                self.has_changes = true;

                // The original action may now be unbound
                let still_bound = self.bindings.iter().any(|b| b.action == action_name);
                if !still_bound {
                    let action_display = KeybindingResolver::format_action_from_str(&action_name);
                    self.bindings.push(ResolvedBinding {
                        key_display: String::new(),
                        action: action_name,
                        action_display,
                        context: String::new(),
                        source: BindingSource::Unbound,
                        key_code: KeyCode::Null,
                        modifiers: KeyModifiers::NONE,
                        is_chord: false,
                    });
                }

                self.apply_filters();
                DeleteResult::KeymapOverridden
            }
            BindingSource::Unbound => DeleteResult::CannotDelete,
        }
    }

    /// Convert a ResolvedBinding to a config-level Keybinding (for matching).
    fn resolved_to_config_keybinding(&self, binding: &ResolvedBinding) -> Keybinding {
        Keybinding {
            key: if binding.is_chord {
                String::new()
            } else {
                key_code_to_config_name(binding.key_code)
            },
            modifiers: if binding.is_chord {
                Vec::new()
            } else {
                modifiers_to_config_names(binding.modifiers)
            },
            keys: Vec::new(),
            action: binding.action.clone(),
            args: HashMap::new(),
            when: if binding.context.is_empty() {
                None
            } else {
                Some(binding.context.clone())
            },
        }
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

    /// Get the custom bindings to remove from config
    pub fn get_pending_removes(&self) -> &[Keybinding] {
        &self.pending_removes
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
