//! Data types for the keybinding editor.

use crate::config::{KeyPress, Keybinding};
use crossterm::event::{KeyCode, KeyModifiers};
use ratatui::layout::Rect;

/// Where a binding comes from
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BindingSource {
    /// From the active keymap (built-in or named map)
    Keymap,
    /// User custom override (in config keybindings array)
    Custom,
    /// From a plugin mode (registered via defineMode)
    Plugin,
    /// Action exists but has no keybinding
    Unbound,
}

/// Result of a delete operation in the keybinding editor
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DeleteResult {
    /// A custom binding was removed
    CustomRemoved,
    /// A keymap or plugin binding was removed with an `unbind` entry
    KeymapRemoved,
    /// The binding was disabled with a `noop` override (a real binding that
    /// makes the key do nothing in that context)
    Disabled,
    /// Cannot delete (e.g. unbound entry)
    CannotDelete,
    /// Nothing was selected
    NothingSelected,
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
    /// The key sequence, for chord bindings (empty otherwise). Kept so the
    /// editor can write a chord back to config: a chord has no single
    /// `key_code`/`modifiers` to reconstruct it from, and dropping this is
    /// what made "delete" and "edit" silently destroy chord bindings.
    pub chord_keys: Vec<KeyPress>,
    /// Plugin name this binding belongs to (None = builtin)
    pub plugin_name: Option<String>,
    /// Human-friendly command name from the CommandRegistry (e.g., "Titlecase").
    /// Present for plugin commands so the keybinding editor can display and search
    /// by the same name shown in the command palette.
    pub command_name: Option<String>,
    /// Original config-level Keybinding (preserved for Custom bindings loaded from config)
    pub original_config: Option<Keybinding>,
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
    /// When true, the next keypress in the key field is captured raw
    /// (including Esc, Tab, Enter). Resets to false after one capture.
    pub capturing_special: bool,
    /// Key sequence of the chord being edited, empty when the dialog holds a
    /// single-key binding. Cleared as soon as the user records a new key, so
    /// saving without re-recording keeps the chord intact instead of writing
    /// a keyless binding.
    pub chord_keys: Vec<KeyPress>,
}

impl EditBindingState {
    pub(crate) fn base_context_options() -> Vec<String> {
        vec![
            "global".to_string(),
            "normal".to_string(),
            "prompt".to_string(),
            "searchPrompt".to_string(),
            "popup".to_string(),
            "completion".to_string(),
            "fileExplorer".to_string(),
            "dock".to_string(),
            "menu".to_string(),
            "terminal".to_string(),
            "settings".to_string(),
            "compositeBuffer".to_string(),
        ]
    }

    pub fn new_add() -> Self {
        Self::new_add_with_modes(&[])
    }

    pub fn new_add_with_modes(mode_contexts: &[String]) -> Self {
        let mut context_options = Self::base_context_options();
        context_options.extend(mode_contexts.iter().cloned());
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
            context_options,
            context_option_index: 1, // default to "normal"
            context_dropdown_open: false,
            selected_button: 0,
            focus_area: 0,
            autocomplete_suggestions: Vec::new(),
            autocomplete_selected: None,
            autocomplete_visible: false,
            action_error: None,
            capturing_special: false,
            chord_keys: Vec::new(),
        }
    }

    pub fn new_edit(index: usize, binding: &ResolvedBinding) -> Self {
        Self::new_edit_with_modes(index, binding, &[])
    }

    pub fn new_edit_with_modes(
        index: usize,
        binding: &ResolvedBinding,
        mode_contexts: &[String],
    ) -> Self {
        let mut context_options = Self::base_context_options();
        context_options.extend(mode_contexts.iter().cloned());
        let context_option_index = context_options
            .iter()
            .position(|c| c == &binding.context)
            .unwrap_or(1);

        Self {
            mode: EditMode::RecordingKey,
            // A chord has no single key code to seed the field with; keep the
            // sequence in `chord_keys` instead and leave `key_code` empty so
            // saving without re-recording round-trips the chord unchanged.
            key_code: if binding.is_chord {
                None
            } else {
                Some(binding.key_code)
            },
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
            capturing_special: false,
            chord_keys: binding.chord_keys.clone(),
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
    PluginOnly,
}

/// A row in the keybinding editor display list — either a section header or a binding
#[derive(Debug, Clone)]
pub enum DisplayRow {
    /// A collapsible section header
    SectionHeader {
        /// Plugin name (None = builtin section)
        plugin_name: Option<String>,
        /// Whether this section is collapsed
        collapsed: bool,
        /// Number of bindings in this section (after filtering)
        binding_count: usize,
    },
    /// A binding row (index into `bindings`)
    Binding(usize),
}

/// Layout information for mouse hit testing
#[derive(Debug, Clone, Default)]
pub struct KeybindingEditorLayout {
    /// The full modal area (all mouse events inside are captured)
    pub modal_area: Rect,
    // **The five the dialogs owned are gone.** `dialog_buttons`,
    // `dialog_key_field`, `dialog_action_field`, `dialog_context_field` and
    // `confirm_buttons` were rectangles the painter filed for a chain of
    // `point_in_rect` in the mouse arm. The dialogs are descriptions now and
    // their fields and buttons answer their own presses.
    /// Search bar area (for clicking to focus)
    pub search_bar: Option<Rect>,
    // `table_area`, `table_first_row_y` and `table_scrollbar` are gone with the
    // table. The first two existed so a mouse arm could turn a cell back into
    // a row index — the row knows its own — and the third so a second arm
    // could drag a thumb the library's viewport already drags.
}
