//! KeybindingEditor - the main editor state and logic.

use super::helpers::{format_chord_keys, key_code_to_config_name, modifiers_to_config_names};
use super::types::*;
use crate::config::{Config, Keybinding};
use crate::input::command_registry::CommandRegistry;
use crate::input::keybindings::{format_keybinding, Action, KeyContext, KeybindingResolver};
use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use rust_i18n::t;
use std::collections::{HashMap, HashSet};

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

    /// Mode context names (from plugins) for context filter cycling
    pub mode_contexts: Vec<String>,

    /// Display rows (section headers + binding rows) after filtering
    pub display_rows: Vec<DisplayRow>,
    /// Sections that are manually collapsed (by plugin name, None = builtin)
    pub collapsed_sections: HashSet<Option<String>>,

    /// Layout info for mouse hit testing (updated during render)
    pub layout: KeybindingEditorLayout,

    /// True while the user is dragging the table scrollbar thumb.
    /// Set on press inside the scrollbar rect, cleared on release.
    pub dragging_table_scrollbar: bool,
}

impl KeybindingEditor {
    /// Create a new keybinding editor from config and resolver.
    ///
    /// `menu_names` are the stable English identifiers of top-level menus
    /// (File, Edit, …, plus any plugin menus). They're used to enumerate
    /// concrete `menu_open:<name>` entries in the action dropdown, so each
    /// menu gets its own selectable row instead of one generic `menu_open`.
    pub fn new(
        config: &Config,
        resolver: &KeybindingResolver,
        mode_registry: &crate::input::buffer_mode::ModeRegistry,
        command_registry: &CommandRegistry,
        config_file_path: String,
        menu_names: &[String],
    ) -> Self {
        let bindings =
            Self::resolve_all_bindings(config, resolver, mode_registry, command_registry);
        let filtered_indices: Vec<usize> = (0..bindings.len()).collect();

        // Collect available action names (include plugin action names from plugin defaults)
        let mut available_actions = Self::collect_action_names();
        for mode_bindings in resolver.get_plugin_defaults().values() {
            for action in mode_bindings.values() {
                let action_name = format!("{:?}", action);
                let action_str = match action {
                    Action::PluginAction(name) => name.clone(),
                    other => format!("{:?}", other),
                };
                if !available_actions.contains(&action_str) {
                    available_actions.push(action_str);
                }
                let _ = action_name;
            }
        }
        // Include action names from plugin-registered commands
        for cmd in command_registry.get_all() {
            if let Action::PluginAction(ref name) = cmd.action {
                if !available_actions.contains(name) {
                    available_actions.push(name.clone());
                }
            }
        }

        // Expand parameterised actions (menu_open, switch_keybinding_map) from a
        // single bare entry — which is unparseable without args and silently
        // becomes a no-op PluginAction — into one entry per concrete variant.
        Self::expand_variant_actions(&mut available_actions, menu_names, config);

        available_actions.sort();
        available_actions.dedup();

        // Collect keymap names
        let mut keymap_names: Vec<String> = config.keybinding_maps.keys().cloned().collect();
        keymap_names.sort();

        // Collect mode context names from plugin defaults
        let mut mode_contexts: Vec<String> = resolver
            .get_plugin_defaults()
            .keys()
            .filter_map(|ctx| {
                if let KeyContext::Mode(name) = ctx {
                    Some(format!("mode:{}", name))
                } else {
                    None
                }
            })
            .collect();
        mode_contexts.sort();

        // Collapse plugin sections by default
        let mut collapsed_sections: HashSet<Option<String>> = HashSet::new();
        for b in &bindings {
            if b.plugin_name.is_some() {
                collapsed_sections.insert(b.plugin_name.clone());
            }
        }

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
            mode_contexts,
            display_rows: Vec::new(),
            collapsed_sections,
            layout: KeybindingEditorLayout::default(),
            dragging_table_scrollbar: false,
        };

        editor.apply_filters();
        editor
    }

    /// Resolve all bindings from the active keymap + custom overrides + plugin modes
    fn resolve_all_bindings(
        config: &Config,
        resolver: &KeybindingResolver,
        mode_registry: &crate::input::buffer_mode::ModeRegistry,
        command_registry: &CommandRegistry,
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

        // Load plugin mode bindings from KeybindingResolver plugin_defaults
        for (context, context_bindings) in resolver.get_plugin_defaults() {
            if let KeyContext::Mode(mode_name) = context {
                let context_str = format!("mode:{}", mode_name);
                // Use plugin_name from mode registry for section grouping
                let section = mode_registry
                    .get(mode_name)
                    .and_then(|m| m.plugin_name.clone())
                    .unwrap_or_else(|| mode_name.clone());
                for ((key_code, modifiers), action) in context_bindings {
                    let key_display = format_keybinding(key_code, modifiers);
                    let seen_key = (key_display.clone(), context_str.clone());
                    // Skip if already overridden by a user custom binding
                    if seen.contains_key(&seen_key) {
                        continue;
                    }
                    let command = action.to_qualified_action_str();
                    let action_display = KeybindingResolver::format_action(action);
                    let idx = bindings.len();
                    seen.insert(seen_key, idx);
                    bindings.push(ResolvedBinding {
                        key_display,
                        action: command,
                        action_display,
                        context: context_str.clone(),
                        source: BindingSource::Plugin,
                        key_code: *key_code,
                        modifiers: *modifiers,
                        is_chord: false,
                        plugin_name: Some(section.clone()),
                        command_name: None,
                        original_config: None,
                    });
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
                    plugin_name: None,
                    command_name: None,
                    original_config: None,
                });
            }
        }

        // Add unbound entries for plugin-registered command actions
        for cmd in command_registry.get_all() {
            if let Action::PluginAction(ref action_name) = cmd.action {
                if !bound_actions.contains(action_name) {
                    let plugin_name = match &cmd.source {
                        crate::input::commands::CommandSource::Plugin(name) => Some(name.clone()),
                        _ => None,
                    };
                    bindings.push(ResolvedBinding {
                        key_display: String::new(),
                        action: action_name.clone(),
                        action_display: cmd.get_localized_name(),
                        context: String::new(),
                        source: BindingSource::Unbound,
                        key_code: KeyCode::Null,
                        modifiers: KeyModifiers::NONE,
                        is_chord: false,
                        plugin_name,
                        command_name: Some(cmd.get_localized_name()),
                        original_config: None,
                    });
                }
            }
        }

        // Populate command_name for bound plugin actions from the registry
        {
            let commands = command_registry.get_all();
            let cmd_by_action: std::collections::HashMap<&str, &crate::input::commands::Command> =
                commands
                    .iter()
                    .filter_map(|c| {
                        if let Action::PluginAction(ref name) = c.action {
                            Some((name.as_str(), c))
                        } else {
                            None
                        }
                    })
                    .collect();
            for binding in &mut bindings {
                if binding.command_name.is_none() {
                    if let Some(cmd) = cmd_by_action.get(binding.action.as_str()) {
                        let name = cmd.get_localized_name();
                        // Use the command name as the display description so it
                        // matches what users see in the command palette.
                        binding.action_display = name.clone();
                        binding.command_name = Some(name);
                    }
                }
            }
        }

        // Sort by plugin_name (None/builtin first), then context, then action name
        bindings.sort_by(|a, b| {
            a.plugin_name
                .cmp(&b.plugin_name)
                .then(a.context.cmp(&b.context))
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

        // Store the qualified form (e.g. `menu_open:File`) on ResolvedBinding
        // so the dropdown round-trips faithfully and "still-bound" checks
        // distinguish variants of the same bare action.
        let qualified_action = Action::qualify_action(&kb.action, &kb.args);

        if !kb.keys.is_empty() {
            // Chord binding
            let key_display = format_chord_keys(&kb.keys);
            let action_display =
                KeybindingResolver::format_action_from_str_with_args(&kb.action, &kb.args);
            let original_config = if source == BindingSource::Custom {
                Some(kb.clone())
            } else {
                None
            };
            Some(ResolvedBinding {
                key_display,
                action: qualified_action,
                action_display,
                context,
                source,
                key_code: KeyCode::Null,
                modifiers: KeyModifiers::NONE,
                is_chord: true,
                plugin_name: None,
                command_name: None,
                original_config,
            })
        } else if !kb.key.is_empty() {
            // Single key binding
            let key_code = KeybindingResolver::parse_key_public(&kb.key)?;
            let modifiers = KeybindingResolver::parse_modifiers_public(&kb.modifiers);
            let key_display = format_keybinding(&key_code, &modifiers);
            let action_display =
                KeybindingResolver::format_action_from_str_with_args(&kb.action, &kb.args);
            let original_config = if source == BindingSource::Custom {
                Some(kb.clone())
            } else {
                None
            };
            Some(ResolvedBinding {
                key_display,
                action: qualified_action,
                action_display,
                context,
                source,
                key_code,
                modifiers,
                is_chord: false,
                plugin_name: None,
                command_name: None,
                original_config,
            })
        } else {
            None
        }
    }

    /// Collect all available action names (delegates to the macro-generated source of truth)
    fn collect_action_names() -> Vec<String> {
        Action::all_action_names()
    }

    /// Replace bare entries for parameterised actions (`menu_open`,
    /// `switch_keybinding_map`) with one qualified entry per variant — e.g.
    /// `menu_open:File`, `menu_open:Edit`. Without this, picking the bare
    /// `menu_open` from the dropdown would produce an un-parseable binding
    /// because `Action::from_str` requires the args map to carry the menu
    /// name.
    fn expand_variant_actions(actions: &mut Vec<String>, menu_names: &[String], config: &Config) {
        // Menu names: built-in + plugin, deduplicated case-insensitively.
        let mut menus: Vec<String> = menu_names.to_vec();
        menus.sort();
        menus.dedup();
        actions.retain(|a| a != "menu_open");
        for name in &menus {
            actions.push(format!("menu_open:{}", name));
        }

        // Keybinding maps: the four built-ins plus user-defined.
        let mut keymaps: Vec<String> = ["default", "emacs", "vscode", "macos"]
            .map(String::from)
            .to_vec();
        keymaps.extend(config.keybinding_maps.keys().cloned());
        keymaps.sort();
        keymaps.dedup();
        actions.retain(|a| a != "switch_keybinding_map");
        for map in &keymaps {
            actions.push(format!("switch_keybinding_map:{}", map));
        }
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
                SourceFilter::PluginOnly if binding.source != BindingSource::Plugin => continue,
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
                                || binding.context.to_lowercase().contains(&query)
                                || binding
                                    .command_name
                                    .as_ref()
                                    .is_some_and(|n| n.to_lowercase().contains(&query));
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

        // Build display rows with section headers
        self.build_display_rows();

        // Reset selection if it's out of bounds
        if self.selected >= self.display_rows.len() {
            self.selected = self.display_rows.len().saturating_sub(1);
        }
        self.ensure_visible();
    }

    /// Build display rows from filtered indices, inserting section headers
    fn build_display_rows(&mut self) {
        self.display_rows.clear();

        let has_active_filter = (self.search_active
            && match self.search_mode {
                SearchMode::Text => !self.search_query.is_empty(),
                SearchMode::RecordKey => self.search_key_code.is_some(),
            })
            || !matches!(self.context_filter, ContextFilter::All)
            || !matches!(self.source_filter, SourceFilter::All);

        // Group filtered indices by section (plugin_name)
        let mut sections: Vec<(Option<String>, Vec<usize>)> = Vec::new();
        let mut current_section: Option<&Option<String>> = None;

        for &idx in &self.filtered_indices {
            let binding = &self.bindings[idx];
            if current_section != Some(&binding.plugin_name) {
                sections.push((binding.plugin_name.clone(), Vec::new()));
                current_section = Some(&binding.plugin_name);
            }
            sections.last_mut().unwrap().1.push(idx);
        }

        for (plugin_name, indices) in sections {
            // When filtering, hide sections with zero matches (already filtered out)
            // When searching, auto-expand all sections that have matches
            let collapsed = if has_active_filter {
                false
            } else {
                self.collapsed_sections.contains(&plugin_name)
            };

            self.display_rows.push(DisplayRow::SectionHeader {
                plugin_name: plugin_name.clone(),
                collapsed,
                binding_count: indices.len(),
            });

            if !collapsed {
                for idx in indices {
                    self.display_rows.push(DisplayRow::Binding(idx));
                }
            }
        }
    }

    /// Toggle the collapsed state of the section at the current selection
    pub fn toggle_section_at_selected(&mut self) {
        if let Some(DisplayRow::SectionHeader { plugin_name, .. }) =
            self.display_rows.get(self.selected)
        {
            let key = plugin_name.clone();
            if self.collapsed_sections.contains(&key) {
                self.collapsed_sections.remove(&key);
            } else {
                self.collapsed_sections.insert(key);
            }
            self.build_display_rows();
            // Keep selected in bounds
            if self.selected >= self.display_rows.len() {
                self.selected = self.display_rows.len().saturating_sub(1);
            }
            self.ensure_visible();
        }
    }

    /// Check if the currently selected display row is a section header
    pub fn selected_is_section_header(&self) -> bool {
        matches!(
            self.display_rows.get(self.selected),
            Some(DisplayRow::SectionHeader { .. })
        )
    }

    /// Get the currently selected binding (None if a section header is selected)
    pub fn selected_binding(&self) -> Option<&ResolvedBinding> {
        match self.display_rows.get(self.selected) {
            Some(DisplayRow::Binding(idx)) => self.bindings.get(*idx),
            _ => None,
        }
    }

    /// Get the binding index in `self.bindings` for the current selection
    fn selected_binding_index(&self) -> Option<usize> {
        match self.display_rows.get(self.selected) {
            Some(DisplayRow::Binding(idx)) => Some(*idx),
            _ => None,
        }
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
        if self.selected + 1 < self.display_rows.len() {
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
        self.selected = (self.selected + page).min(self.display_rows.len().saturating_sub(1));
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
        let mut contexts = vec![
            ContextFilter::All,
            ContextFilter::Specific("global".to_string()),
            ContextFilter::Specific("normal".to_string()),
            ContextFilter::Specific("prompt".to_string()),
            ContextFilter::Specific("popup".to_string()),
            ContextFilter::Specific("completion".to_string()),
            ContextFilter::Specific("file_explorer".to_string()),
            ContextFilter::Specific("menu".to_string()),
            ContextFilter::Specific("terminal".to_string()),
        ];
        // Add mode contexts dynamically
        for mode_ctx in &self.mode_contexts {
            contexts.push(ContextFilter::Specific(mode_ctx.clone()));
        }

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
            SourceFilter::KeymapOnly => SourceFilter::PluginOnly,
            SourceFilter::PluginOnly => SourceFilter::All,
        };
        self.apply_filters();
    }

    /// Open the add binding dialog
    pub fn open_add_dialog(&mut self) {
        self.edit_dialog = Some(EditBindingState::new_add_with_modes(&self.mode_contexts));
    }

    /// Open the edit binding dialog for the selected binding
    pub fn open_edit_dialog(&mut self) {
        if let Some(idx) = self.selected_binding_index() {
            let binding = self.bindings[idx].clone();
            self.edit_dialog = Some(EditBindingState::new_edit_with_modes(
                idx,
                &binding,
                &self.mode_contexts,
            ));
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
        let Some(idx) = self.selected_binding_index() else {
            return DeleteResult::NothingSelected;
        };

        match self.bindings[idx].source {
            BindingSource::Custom => {
                let binding = &self.bindings[idx];
                let action_name = binding.action.clone();

                // Use the original config-level Keybinding if available (for
                // bindings loaded from config), otherwise reconstruct it.
                // This avoids lossy round-trips through parse_key which
                // lowercases key names (e.g. "N" → "n").
                let config_kb = binding
                    .original_config
                    .clone()
                    .unwrap_or_else(|| self.resolved_to_config_keybinding(binding));

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
                        plugin_name: None,
                        command_name: None,
                        original_config: None,
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
                    plugin_name: self.bindings[idx].plugin_name.clone(),
                    command_name: None,
                    original_config: None,
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
                        plugin_name: None,
                        command_name: None,
                        original_config: None,
                    });
                }

                self.apply_filters();
                DeleteResult::KeymapOverridden
            }
            BindingSource::Plugin => {
                // Plugin bindings behave like keymap bindings - create a noop override
                let binding = &self.bindings[idx];
                let action_name = binding.action.clone();

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
                    plugin_name: self.bindings[idx].plugin_name.clone(),
                    command_name: None,
                    original_config: None,
                };
                self.has_changes = true;

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
                        plugin_name: None,
                        command_name: None,
                        original_config: None,
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
        let (action, args) = Action::unqualify_action(&binding.action);
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
            action,
            args,
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
        let dialog = self.edit_dialog.take()?;

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

        // Split the qualified form (e.g. `menu_open:File`) into bare action +
        // args so the written Keybinding actually parses back to the right
        // variant at runtime.
        let (bare_action, args) = Action::unqualify_action(&dialog.action_text);

        let new_binding = Keybinding {
            key: key_name,
            modifiers: modifier_names,
            keys: Vec::new(),
            action: bare_action.clone(),
            args: args.clone(),
            when: Some(dialog.context.clone()),
        };

        // Add as custom binding
        self.pending_adds.push(new_binding.clone());
        self.has_changes = true;

        // Update display
        let key_display = format_keybinding(&key_code, &modifiers);
        let action_display =
            KeybindingResolver::format_action_from_str_with_args(&bare_action, &args);

        // When editing an existing binding, preserve its plugin_name so it stays
        // in the same section. New bindings go to Builtin (plugin_name: None).
        let preserved_plugin_name = dialog
            .editing_index
            .and_then(|idx| self.bindings.get(idx))
            .and_then(|b| b.plugin_name.clone());

        let resolved = ResolvedBinding {
            key_display,
            action: dialog.action_text,
            action_display,
            context: dialog.context,
            source: BindingSource::Custom,
            key_code,
            modifiers,
            is_chord: false,
            plugin_name: preserved_plugin_name,
            command_name: None,
            original_config: None,
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
                    match binding.source {
                        BindingSource::Custom => "custom",
                        BindingSource::Plugin => "plugin",
                        _ => "keymap",
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
            SourceFilter::PluginOnly => "Plugin",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::input::buffer_mode::ModeRegistry;

    fn make_editor(extra_menus: &[&str]) -> KeybindingEditor {
        let config = Config::default();
        let resolver = KeybindingResolver::new(&config);
        let mode_registry = ModeRegistry::new();
        let cmd_registry = CommandRegistry::new();
        let mut menu_names: Vec<String> = ["File", "Edit", "View"]
            .iter()
            .map(|s| s.to_string())
            .collect();
        menu_names.extend(extra_menus.iter().map(|s| s.to_string()));
        KeybindingEditor::new(
            &config,
            &resolver,
            &mode_registry,
            &cmd_registry,
            String::from("/tmp/fresh-config.toml"),
            &menu_names,
        )
    }

    #[test]
    fn dropdown_lists_menu_open_variants_not_bare_entry() {
        // Regression for #1407 follow-up: picking `menu_open` from the
        // dropdown used to produce a no-op binding because no args were
        // attached. The dropdown should instead offer one entry per menu.
        let editor = make_editor(&[]);
        assert!(
            !editor.available_actions.iter().any(|a| a == "menu_open"),
            "bare `menu_open` must not appear — it is un-parseable without args"
        );
        assert!(
            editor
                .available_actions
                .contains(&"menu_open:File".to_string()),
            "expected dropdown to list `menu_open:File`, got {:?}",
            editor.available_actions
        );
        assert!(
            editor
                .available_actions
                .contains(&"menu_open:Edit".to_string()),
            "expected dropdown to list `menu_open:Edit`"
        );
    }

    #[test]
    fn dropdown_includes_plugin_menus_passed_in() {
        let editor = make_editor(&["MyPluginMenu"]);
        assert!(
            editor
                .available_actions
                .contains(&"menu_open:MyPluginMenu".to_string()),
            "plugin menus should surface as dropdown entries"
        );
    }

    #[test]
    fn dropdown_lists_builtin_keybinding_maps() {
        let editor = make_editor(&[]);
        for map in ["default", "emacs", "vscode", "macos"] {
            let qualified = format!("switch_keybinding_map:{}", map);
            assert!(
                editor.available_actions.contains(&qualified),
                "expected `{}` in dropdown",
                qualified
            );
        }
        assert!(
            !editor
                .available_actions
                .iter()
                .any(|a| a == "switch_keybinding_map"),
            "bare `switch_keybinding_map` must not appear"
        );
    }

    #[test]
    fn qualified_action_roundtrips_through_resolved_to_config() {
        // A binding selected from the dropdown as `menu_open:File` must be
        // written to config as `{action: "menu_open", args: {name: "File"}}`.
        let editor = make_editor(&[]);
        let rb = ResolvedBinding {
            key_display: "Alt+F".to_string(),
            action: "menu_open:File".to_string(),
            action_display: String::new(),
            context: "global".to_string(),
            source: BindingSource::Custom,
            key_code: KeyCode::Char('f'),
            modifiers: KeyModifiers::ALT,
            is_chord: false,
            plugin_name: None,
            command_name: None,
            original_config: None,
        };
        let kb = editor.resolved_to_config_keybinding(&rb);
        assert_eq!(kb.action, "menu_open");
        assert_eq!(
            kb.args.get("name").and_then(|v| v.as_str()),
            Some("File"),
            "the variant name must land in args.name, got {:?}",
            kb.args
        );
    }
}
